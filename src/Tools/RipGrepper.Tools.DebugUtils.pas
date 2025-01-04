unit RipGrepper.Tools.DebugUtils;

interface

type

	ETraceFilterType = (tftError, tftWarning, tftInfo, tftBegin, tftEnd, tftRegex, tftNone);
	TTraceFilterTypes = set of ETraceFilterType;

	TTraceFilterTypeRec = record
		Name : string;
		Value : ETraceFilterType;
	end;

const
	TRACE_TYPES : array [0 .. 6] of TTraceFilterTypeRec = (
		{ } (name : 'tftError'; Value : tftError),
		{ } (name : 'tftWarning'; Value : tftWarning),
		{ } (name : 'tftInfo'; Value : tftInfo),
		{ } (name : 'tftBegin'; Value : tftBegin),
		{ } (name : 'tftEnd'; Value : tftEnd),
		{ } (name : 'tftRegex'; Value : tftRegex),
		{ } (name : 'tftNone'; Value : tftNone)
		{ } );

type
	TDebugUtils = class(TObject)
		private
		class var
			FTraceFilerTypes : TTraceFilterTypes;
			FDebugTraceInactiveMsgShown : Boolean;
			FTraceFilterRegex : string;

			class constructor Create;
			class procedure InnerOutputDebugString(const _s : string; const _type : ETraceFilterType);

		public
			class procedure DebugMessage(const _s : string; const _type : ETraceFilterType = tftInfo);
			class procedure Msg(const _s : string; const _type : ETraceFilterType = tftInfo);
			class procedure DebugMessageFormat(const _s : string; const _args : array of const; const _type : ETraceFilterType = tftInfo);
			class procedure MsgFmt(const _s : string; const _args : array of const; const _type : ETraceFilterType = tftInfo);

			class function StrToTraceTypes(const _s : string) : TTraceFilterTypes; static;
			class function TraceTypesToStr(const _tt : TTraceFilterTypes) : string; static;

			class procedure UpdateTraceActive;
			class property TraceFilterRegex : string read FTraceFilterRegex write FTraceFilterRegex;
	end;

	TDebugMsgBeginEnd = record

		private
			FProcName : string;
			FSilentBeginEnd : Boolean;

		public
			procedure Msg(const _sMsg : string);
			procedure ErrorMsg(const _sMsg : string);
			procedure ErrorMsgFmt(const _sMsg : string; const _args : array of const);
			procedure MsgIf(const _bCondition : Boolean; const _sMsg : string);
			procedure MsgFmt(const _s : string; const _args : array of const);
			procedure MsgFmtIf(const _bCondition : Boolean; const _s : string; const _args : array of const);
			class function New(const _sProcName : string; const _bSilent : Boolean = False) : TDebugMsgBeginEnd; static;
			class operator Finalize(var Dest : TDebugMsgBeginEnd);
	end;

implementation

uses
	Winapi.Windows,
	System.SysUtils,
	RipGrepper.Settings.RipGrepperSettings,
	System.RegularExpressions,
	RipGrepper.Common.Constants;

class constructor TDebugUtils.Create;
begin
	{$IFDEF DEBUG}
	FTraceFilerTypes := [tftBegin, tftEnd, tftError, tftWarning, tftInfo];
	{$ENDIF}
	{$IFDEF TESTINSIGHT}
	FTraceFilerTypes := [tftError];
	{$ENDIF}
	{$IFDEF CONSOLE}
	FTraceFilerTypes := [tftError];
	{$ENDIF}
	UpdateTraceActive;
end;

class procedure TDebugUtils.DebugMessage(const _s : string; const _type : ETraceFilterType = tftInfo);
begin
	InnerOutputDebugString(_s, _type);
end;

class procedure TDebugUtils.Msg(const _s : string; const _type : ETraceFilterType = tftInfo);
begin
	DebugMessage(_s, _type);
end;

class procedure TDebugUtils.DebugMessageFormat(const _s : string; const _args : array of const; const _type : ETraceFilterType = tftInfo);
begin
	InnerOutputDebugString(Format(_s, _args), _type);
end;

class function TDebugUtils.StrToTraceTypes(const _s : string) : TTraceFilterTypes;
begin
	Result := [];

	for var i := 0 to high(TRACE_TYPES) do begin
		var
		tt := TRACE_TYPES[i];
		if _s.contains(tt.Name) then begin
			Include(Result, tt.Value);
		end;
	end;

	if Result <> [tftNone] then begin
		Exclude(Result, tftNone);
	end;
end;

class procedure TDebugUtils.InnerOutputDebugString(const _s : string; const _type : ETraceFilterType);
begin
	if (tftRegex in FTraceFilerTypes) and (not FTraceFilterRegex.IsEmpty) and
	{ } TRegEx.IsMatch(_s, FTraceFilterRegex) then begin
		OutputDebugString(PChar(_s));
		Exit;
	end;

	if _type in FTraceFilerTypes then begin
		OutputDebugString(PChar(_s));
	end else begin
		if not FDebugTraceInactiveMsgShown and (FTraceFilerTypes = []) then begin
			FDebugTraceInactiveMsgShown := True;
			OutputDebugString(PChar(APPNAME + ' DebugTrace off'));
		end;
	end;
end;

class procedure TDebugUtils.MsgFmt(const _s : string; const _args : array of const; const _type : ETraceFilterType = tftInfo);
begin
	DebugMessageFormat(_s, _args, _type);
end;

class function TDebugUtils.TraceTypesToStr(const _tt : TTraceFilterTypes) : string;
var
	arr : TArray<string>;
begin
	Result := '';

	for var i := 0 to high(TRACE_TYPES) do begin
		var
		tt := TRACE_TYPES[i];
		if tt.Value in _tt then begin
			arr := arr + [tt.Name];
		end;
	end;

	if Length(arr) > 0 then begin
		Result := string.Join(ARRAY_SEPARATOR, arr);
	end;
end;

class procedure TDebugUtils.UpdateTraceActive;
begin
	FTraceFilerTypes := [];

	if (Assigned(GSettings) and Assigned(GSettings.AppSettings)) then begin
		FTraceFilerTypes := StrToTraceTypes(GSettings.AppSettings.DebugTrace);
		FTraceFilterRegex := GSettings.AppSettings.DebugTraceRegexFilter;
	end;
	OutputDebugString(PChar(APPNAME + ' DebugTraceActive [' +
		{ } TraceTypesToStr(FTraceFilerTypes) + '] RegEx: "' + FTraceFilterRegex + '"'));
end;

procedure TDebugMsgBeginEnd.Msg(const _sMsg : string);
begin
	TDebugUtils.Msg(FProcName + ' - ' + _sMsg, tftInfo);
end;

procedure TDebugMsgBeginEnd.ErrorMsg(const _sMsg : string);
begin
	TDebugUtils.Msg(FProcName + ' - ERROR -' + _sMsg, tftError);
end;

procedure TDebugMsgBeginEnd.ErrorMsgFmt(const _sMsg : string; const _args : array of const);
begin
	TDebugUtils.MsgFmt(FProcName + ' - ERROR -' + _sMsg, _args, tftError);
end;

procedure TDebugMsgBeginEnd.MsgIf(const _bCondition : Boolean; const _sMsg : string);
begin
	if _bCondition then
		TDebugUtils.Msg(FProcName + ' - ' + _sMsg);
end;

procedure TDebugMsgBeginEnd.MsgFmt(const _s : string; const _args : array of const);
begin
	TDebugUtils.MsgFmt(FProcName + ' - ' + _s, _args);
end;

procedure TDebugMsgBeginEnd.MsgFmtIf(const _bCondition : Boolean; const _s : string; const _args : array of const);
begin
	if _bCondition then
		TDebugUtils.MsgFmt(FProcName + ' - ' + _s, _args);
end;

class function TDebugMsgBeginEnd.New(const _sProcName : string; const _bSilent : Boolean = False) : TDebugMsgBeginEnd;
begin
	Result.FProcName := _sProcName;
	Result.FSilentBeginEnd := _bSilent;
	if not Result.FSilentBeginEnd then begin
		TDebugUtils.DebugMessage(_sProcName + ' - begin', tftBegin);
	end;
end;

class operator TDebugMsgBeginEnd.Finalize(var Dest : TDebugMsgBeginEnd);
begin
	if not Dest.FSilentBeginEnd then begin
		TDebugUtils.DebugMessage(Dest.FProcName + ' - end', tftEnd);
	end;
end;

initialization
    OutputDebugString(PChar('DebugTrace initialized.'));
finalization
    OutputDebugString(PChar('DebugTrace finalized.'));
end.
