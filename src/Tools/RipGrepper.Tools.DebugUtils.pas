unit RipGrepper.Tools.DebugUtils;

interface

uses
	System.Classes,
	System.SyncObjs,
	RipGrepper.Common.SimpleTypes;

type

	ETraceFilterType = (tftError, tftWarning, tftInfo, tftBegin, tftEnd, tftVerbose, tftRegex, tftNone);
	TTraceFilterTypes = set of ETraceFilterType;

	TTraceFilterTypeRec = record
		Name : string;
		Value : ETraceFilterType;
	end;

const
	TRACE_TYPES : array [0 .. 7] of TTraceFilterTypeRec = (
		{ } (name : 'tftError'; Value : tftError),
		{ } (name : 'tftWarning'; Value : tftWarning),
		{ } (name : 'tftInfo'; Value : tftInfo),
		{ } (name : 'tftBegin'; Value : tftBegin),
		{ } (name : 'tftEnd'; Value : tftEnd),
		{ } (name : 'tftVerbose'; Value : tftVerbose),
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
			FLogDestinations : TLogDestinations;
			FLogFilePath : string;
			FLogFileWriter : TStreamWriter;
			FLogLock : TCriticalSection;
			FLogFileCreationMode : ELogFileCreationMode;
			FLogFileSettingsApplied : Boolean;
			FIsFinalized : Boolean;

			class constructor Create;
			class destructor Destroy;
			class procedure InnerOutputDebugString(const _s : string; const _type : ETraceFilterType);
			class function FormatLogLine(const _s : string) : string;
			class procedure writeToLogFile(const _s : string);
			class procedure openLogFile();
			class procedure closeLogFile();
			class function GetTimestampedLogFilePath(const _basePath : string) : string;
			class procedure ApplyLogFileSettings();

		public
			class procedure DebugMessage(const _s : string; const _type : ETraceFilterType = tftInfo);
			class procedure Msg(const _s : string; const _type : ETraceFilterType = tftInfo);
			class procedure DebugMessageFormat(const _s : string; const _args : array of const; const _type : ETraceFilterType = tftInfo);
			class procedure MsgFmt(const _s : string; const _args : array of const; const _type : ETraceFilterType = tftInfo);

			class function StrToTraceTypes(const _s : string) : TTraceFilterTypes; static;
			class function TraceTypesToStr(const _tt : TTraceFilterTypes) : string; static;

			class procedure UpdateTraceActive;
			class property TraceFilterRegex : string read FTraceFilterRegex write FTraceFilterRegex;
			class property LogDestinations : TLogDestinations read FLogDestinations write FLogDestinations;
			class property LogFilePath : string read FLogFilePath write FLogFilePath;
			class property LogFileCreationMode : ELogFileCreationMode read FLogFileCreationMode write FLogFileCreationMode;
	end;

	TDebugMsgBeginEnd = record

		private
			FProcName : string;
			FSilentBeginEnd : Boolean;

		public
			procedure Msg(const _sMsg : string; const _type : ETraceFilterType = tftInfo);
			procedure ErrorMsg(const _sMsg : string);
			procedure WarningMsg(const _sMsg : string);
			procedure ErrorMsgFmt(const _sMsg : string; const _args : array of const);
			procedure WarningMsgFmt(const _sMsg : string; const _args : array of const);
			procedure MsgIf(const _bCondition : Boolean; const _sMsg : string; const _type : ETraceFilterType = tftInfo);
			procedure MsgFmt(const _s : string; const _args : array of const; const _type : ETraceFilterType = tftInfo);
			procedure MsgFmtIf(const _bCondition : Boolean; const _s : string; const _args : array of const;
				const _type : ETraceFilterType = tftInfo);
			class function New(const _sProcName : string; const _bSilent : Boolean = False) : TDebugMsgBeginEnd; static;
			class operator Finalize(var Dest : TDebugMsgBeginEnd);
	end;

implementation

uses
	Winapi.Windows,
	System.IOUtils,
	System.SysUtils,
	RipGrepper.Settings.RipGrepperSettings,
	RipGrepper.Settings.AppSettings,
	System.RegularExpressions,
	RipGrepper.Common.Constants,
	Spring.DesignPatterns;

class constructor TDebugUtils.Create;
begin
	FLogLock := TCriticalSection.Create;
	FLogDestinations := [ldFile];
	FLogFilePath := TPath.Combine(TPath.GetDirectoryName(ParamStr(0)), APPNAME + '.log');
	FLogFileWriter := nil;
	FLogFileCreationMode := lfcmRecreateOnStart;
	FLogFileSettingsApplied := False;
	{$IFDEF DEBUG}
	FTraceFilerTypes := [tftBegin, tftEnd, tftError, tftWarning, tftInfo, tftVerbose];
	{$ENDIF}
	{$IFDEF TESTINSIGHT}
	FTraceFilerTypes := [tftError];
	{$ENDIF}
	{$IFDEF CONSOLE}
	FTraceFilerTypes := [tftError];
	{$ENDIF}
	UpdateTraceActive;
end;

class destructor TDebugUtils.Destroy;
var
	tmp : TCriticalSection;
begin
	FIsFinalized := True;
	closeLogFile();
	tmp := FLogLock;
	FLogLock := nil;
	tmp.Free;
end;

class function TDebugUtils.FormatLogLine(const _s : string) : string;
begin
	Result := Format('%s [%5d] %s', [FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now), GetCurrentThreadId, _s]);
end;

class procedure TDebugUtils.openLogFile();
var
	fs : TFileStream;
begin
	if Assigned(FLogFileWriter) then begin
		Exit;
	end;
	if FileExists(FLogFilePath) then begin
		fs := TFileStream.Create(FLogFilePath, fmOpenWrite or fmShareDenyNone);
		fs.Seek(0, soEnd);
	end else begin
		fs := TFileStream.Create(FLogFilePath, fmCreate or fmShareDenyNone);
	end;
	FLogFileWriter := TStreamWriter.Create(fs, TEncoding.UTF8);
	FLogFileWriter.OwnStream;
	FLogFileWriter.AutoFlush := True;
end;

class procedure TDebugUtils.closeLogFile();
begin
	FreeAndNil(FLogFileWriter);
end;

class procedure TDebugUtils.writeToLogFile(const _s : string);
begin
	if not Assigned(FLogLock) then
		Exit;
	FLogLock.Enter;
	try
		try
			openLogFile();
			FLogFileWriter.WriteLine(_s);
		except
			on E : Exception do begin
				OutputDebugString(PChar('Log file write error: ' + E.Message));
			end;
		end;
	finally
		FLogLock.Leave;
	end;
end;

class procedure TDebugUtils.DebugMessage(const _s : string; const _type : ETraceFilterType = tftInfo);
begin
	if FIsFinalized then
		Exit;
	InnerOutputDebugString(_s, _type);
end;

class procedure TDebugUtils.Msg(const _s : string; const _type : ETraceFilterType = tftInfo);
begin
	DebugMessage(_s, _type);
end;

class procedure TDebugUtils.DebugMessageFormat(const _s : string; const _args : array of const; const _type : ETraceFilterType = tftInfo);
begin
	if FIsFinalized then
		Exit;
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
var
	shallLog : Boolean;
	logLine : string;
begin
	shallLog := False;

	if (tftRegex in FTraceFilerTypes) and (not FTraceFilterRegex.IsEmpty) and
	{ } TRegEx.IsMatch(_s, FTraceFilterRegex) then begin
		shallLog := True;
	end else if _type in FTraceFilerTypes then begin
		shallLog := True;
	end else begin
		if not FDebugTraceInactiveMsgShown and (FTraceFilerTypes = []) then begin
			FDebugTraceInactiveMsgShown := True;
			OutputDebugString(PChar(APPNAME + ' DebugTrace off'));
		end;
	end;

	if shallLog then begin
		logLine := FormatLogLine(_s);
		if ldOutputDebugString in FLogDestinations then begin
			OutputDebugString(PChar(logLine));
		end;
		if ldFile in FLogDestinations then begin
			writeToLogFile(logLine);
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

	var
	appSettings := TSingleton.GetInstance<TRipGrepperSettings>().AppSettings;

	if ( { Assigned(GSettings) and } Assigned(appSettings)) then begin
		FTraceFilerTypes := StrToTraceTypes(appSettings.DebugTrace);
		FTraceFilterRegex := appSettings.DebugTraceRegexFilter;
		ApplyLogFileSettings();
	end;
	OutputDebugString(PChar(APPNAME + ' DebugTraceActive [' +
		{ } TraceTypesToStr(FTraceFilerTypes) + '] RegEx: "' + FTraceFilterRegex + '"'));
end;

class function TDebugUtils.GetTimestampedLogFilePath(const _basePath : string) : string;
var
	dir, baseName, ext : string;
begin
	dir := TPath.GetDirectoryName(_basePath);
	baseName := TPath.GetFileNameWithoutExtension(_basePath);
	ext := TPath.GetExtension(_basePath);
	Result := TPath.Combine(dir, baseName + '_' + FormatDateTime('yyyymmdd_HHnnss', Now) + ext);
end;

class procedure TDebugUtils.ApplyLogFileSettings();
var
	appSettings : TAppSettings;
	configuredPath : string;
begin
	appSettings := TSingleton.GetInstance<TRipGrepperSettings>().AppSettings;
	if not Assigned(appSettings) then begin
		Exit;
	end;

	configuredPath := appSettings.LogFilePath;
	if not configuredPath.IsEmpty then begin
		FLogFilePath := configuredPath;
	end;

	FLogFileCreationMode := appSettings.LogFileCreationMode;
	FLogDestinations := appSettings.LogDestinations;

	if FLogFileSettingsApplied then begin
		Exit;
	end;
	FLogFileSettingsApplied := True;

	case FLogFileCreationMode of
		lfcmRecreateOnStart : begin
			closeLogFile();
			if FileExists(FLogFilePath) then begin
				DeleteFile(FLogFilePath);
			end;
		end;
		lfcmTimestamped : begin
			closeLogFile();
			FLogFilePath := GetTimestampedLogFilePath(FLogFilePath);
		end;
	end;
end;

procedure TDebugMsgBeginEnd.Msg(const _sMsg : string; const _type : ETraceFilterType = tftInfo);
begin
	TDebugUtils.Msg(FProcName + ' - ' + _sMsg, _type);
end;

procedure TDebugMsgBeginEnd.ErrorMsg(const _sMsg : string);
begin
	TDebugUtils.Msg(FProcName + ' - ERROR -' + _sMsg, tftError);
end;

procedure TDebugMsgBeginEnd.WarningMsg(const _sMsg : string);
begin
	TDebugUtils.Msg(FProcName + ' - WARNING -' + _sMsg, tftError);
end;

procedure TDebugMsgBeginEnd.ErrorMsgFmt(const _sMsg : string; const _args : array of const);
begin
	TDebugUtils.MsgFmt(FProcName + ' - ERROR -' + _sMsg, _args, tftError);
end;

procedure TDebugMsgBeginEnd.WarningMsgFmt(const _sMsg : string; const _args : array of const);
begin
	TDebugUtils.MsgFmt(FProcName + ' - WARNING -' + _sMsg, _args, tftError);
end;

procedure TDebugMsgBeginEnd.MsgIf(const _bCondition : Boolean; const _sMsg : string; const _type : ETraceFilterType = tftInfo);
begin
	if _bCondition then
		TDebugUtils.Msg(FProcName + ' - ' + _sMsg, _type);
end;

procedure TDebugMsgBeginEnd.MsgFmt(const _s : string; const _args : array of const; const _type : ETraceFilterType = tftInfo);
begin
	TDebugUtils.MsgFmt(FProcName + ' - ' + _s, _args, _type);
end;

procedure TDebugMsgBeginEnd.MsgFmtIf(const _bCondition : Boolean; const _s : string; const _args : array of const;
	const _type : ETraceFilterType = tftInfo);
begin
	if _bCondition then
		TDebugUtils.MsgFmt(FProcName + ' - ' + _s, _args, _type);
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

TDebugUtils.closeLogFile();
OutputDebugString(PChar('DebugTrace finalized.'));

end.
