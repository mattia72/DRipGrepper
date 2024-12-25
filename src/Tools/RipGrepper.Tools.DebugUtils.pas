unit RipGrepper.Tools.DebugUtils;

interface

type

	TDebugUtils = class(TObject)
		strict private
		class var
			FDebugTraceActive : Boolean;
			FDebugTraceInactiveMsgShown : Boolean;

			class constructor Create;
			class procedure InnerOutputDebugString(const _s : string);

		public
			class procedure DebugMessage(const _s : string);
			class procedure Msg(const _s : string);
			class procedure DebugMessageFormat(const _s : string; const _args : array of const);
			class procedure MsgFmt(const _s : string; const _args : array of const);
			class procedure UpdateTraceActive;
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
	FDebugTraceActive := True;
	{$ENDIF}
	{$IFDEF TESTINSIGHT}
	FDebugTraceActive := FALSE;
	{$ENDIF}
	{$IFDEF CONSOLE}
	FDebugTraceActive := FALSE;
	{$ENDIF}
	UpdateTraceActive;
end;

class procedure TDebugUtils.DebugMessage(const _s : string);
begin
	InnerOutputDebugString(_s);
end;

class procedure TDebugUtils.Msg(const _s : string);
begin
	DebugMessage(_s);
end;

class procedure TDebugUtils.DebugMessageFormat(const _s : string; const _args : array of const);
begin
	InnerOutputDebugString(Format(_s, _args));
end;

class procedure TDebugUtils.InnerOutputDebugString(const _s : string);
begin
	if FDebugTraceActive then begin
		// if TRegEx.IsMatch(_s, '') then
		OutputDebugString(PChar(_s));
	end else begin
		if not FDebugTraceInactiveMsgShown then begin
			FDebugTraceInactiveMsgShown := True;
			OutputDebugString(PChar(APPNAME + 'DebugTrace off'));
		end;
	end;
end;

class procedure TDebugUtils.MsgFmt(const _s : string; const _args : array of const);
begin
	DebugMessageFormat(_s, _args);
end;

class procedure TDebugUtils.UpdateTraceActive;
begin
	FDebugTraceActive := { } FDebugTraceActive or
	{ } (Assigned(GSettings) and
		{ } Assigned(GSettings.AppSettings) and
		{ } GSettings.AppSettings.DebugTrace);
	OutputDebugString(PChar(APPNAME + 'DebugTraceActive ' + BoolToStr(FDebugTraceActive, True)));
end;

procedure TDebugMsgBeginEnd.Msg(const _sMsg : string);
begin
	TDebugUtils.Msg(FProcName + ' - ' + _sMsg);
end;

procedure TDebugMsgBeginEnd.ErrorMsg(const _sMsg : string);
begin
	TDebugUtils.Msg(FProcName + ' - ERROR -' + _sMsg);
end;

procedure TDebugMsgBeginEnd.ErrorMsgFmt(const _sMsg : string; const _args : array of const);
begin
	TDebugUtils.MsgFmt(FProcName + ' - ERROR -' + _sMsg, _args);
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
		TDebugUtils.DebugMessage(_sProcName + ' - begin');
	end;
end;

class operator TDebugMsgBeginEnd.Finalize(var Dest : TDebugMsgBeginEnd);
begin
	if not Dest.FSilentBeginEnd then begin
		TDebugUtils.DebugMessage(Dest.FProcName + ' - end');
	end;
end;

end.
