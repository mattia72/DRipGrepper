unit RipGrepper.Settings.RipGrepParameterSettings;

interface

uses
	System.Classes,
	System.IniFiles,
	ArrayEx,
	RipGrepper.Common.Constants,
	RipGrepper.Settings.Persistable,
	RipGrepper.Common.GuiSearchParams,
	RipGrepper.Helper.Types,
	System.Generics.Collections,
	RipGrepper.CommandLine.OptionStrings,
	RipGrepper.Common.SimpleTypes,
	Spring,
	RipGrepper.Settings.SettingVariant;

type
	ERipGrepPathInitResult = (rgpiNotSet, rgpiFound, rgpiNotFound);

	TRipGrepParameterSettings = class(TPersistableSettings)
		const
			INI_SECTION = 'RipGrepSettings';

		private
			FRipGrepArguments : IShared<TRipGrepArguments>;
			FRgExeOptions : TOptionStrings;
			FSearchPath : IStringSetting;
			FSearchText : string;
			FReplaceText : string;
			FFileMasks : IStringSetting;
			FGuiSearchTextParams : IShared<TGuiSearchTextParams>;
			FRipGrepPathInitResult : ERipGrepPathInitResult;
			FRipGrepPath : IStringSetting;
			procedure AddQuotedRgArgs(var _cmdLine : IShared<TStringList>; const _quoteChar : Char; const _shell : TShellType);
			function GetFileMasks() : string;
			function GetIsRgPathInitOk() : Boolean;
			function GetRipGrepPath : string;
			function GetSearchPath() : string;
			procedure SetFileMasks(const Value : string);
			procedure SetGuiSearchTextParams(const Value : IShared<TGuiSearchTextParams>);
			procedure SetRgExeOptions(const Value : TOptionStrings);
			procedure SetSearchPath(const Value : string);
			procedure SetReplaceText(const Value : string);
			procedure SetRipGrepPath(const Value : string);
			procedure SetSearchText(const Value : string);
			function TryFindRipGrepExePath : string;

		protected
			procedure Init; override;

		public
			constructor Create(const _Owner : TPersistableSettings);
			destructor Destroy; override;
			procedure Copy(const _other : TPersistableSettings); override;
			function GetCommandLine(const _shell : TShellType) : string;
			procedure ReadFile(); override;
			procedure LoadFromDict; override;
			function TryGetRipGrepPath(out _rgPath : string) : ERipGrepPathInitResult;
			property FileMasks : string read GetFileMasks write SetFileMasks;
			property GuiSearchTextParams : IShared<TGuiSearchTextParams> read FGuiSearchTextParams write SetGuiSearchTextParams;
			property IsRgPathInitOk : Boolean read GetIsRgPathInitOk;
			property RgExeOptions : TOptionStrings read FRgExeOptions write SetRgExeOptions;
			property SearchPath : string read GetSearchPath write SetSearchPath;
			property SearchText : string read FSearchText write SetSearchText;
			property RipGrepArguments : IShared<TRipGrepArguments> read FRipGrepArguments write FRipGrepArguments;
			property RipGrepPath : string read GetRipGrepPath write SetRipGrepPath;
			property ReplaceText : string read FReplaceText write SetReplaceText;
			property RipGrepPathInitResult : ERipGrepPathInitResult read FRipGrepPathInitResult write FRipGrepPathInitResult;
	end;

implementation

uses
	System.SysUtils,
	Vcl.Dialogs,
	RipGrepper.Tools.FileUtils,
	System.IOUtils,
	Vcl.Forms,
	RipGrepper.Tools.ProcessUtils,
	System.RegularExpressions,
	RipGrepper.Helper.UI,
	RipGrepper.Tools.DebugUtils,
	System.StrUtils,
	Spring.Collections;

const
	FILEMASKS_KEY = 'FileMasks';
	SEARCHPATH_KEY = 'SearchPath';

constructor TRipGrepParameterSettings.Create(const _Owner : TPersistableSettings);
begin
	IniSectionName := INI_SECTION;
	inherited Create(_Owner);
	FGuiSearchTextParams := Shared.Make<TGuiSearchTextParams>(TGuiSearchTextParams.Create(_Owner, INI_SECTION));
	AddChildSettings(FGuiSearchTextParams);
	FRipGrepPathInitResult := rgpiNotSet;
	RipGrepPath := '';
	FRipGrepArguments := Shared.Make<TStringList>();
end;

destructor TRipGrepParameterSettings.Destroy;
begin
	RemoveChildSettings(FGuiSearchTextParams);
	inherited Destroy() // ok;
end;

procedure TRipGrepParameterSettings.AddQuotedRgArgs(var _cmdLine : IShared<TStringList>; const _quoteChar : Char;
	const _shell : TShellType);
var
	arrParamValue : TArrayEx<string>;
	key : string;
	sParam : string;
	argName : string;
	argValue : string;
begin
	for var argKeyValue in RipGrepArguments do begin
		arrParamValue := argKeyValue.Split(['=']);
		key := arrParamValue[0];
		if key = RG_ARG_REPLACE_TEXT then begin
			continue;
		end;

		argName := arrParamValue.SafeItem[1];
		argVAlue := arrParamValue.SafeItem[2];

		sParam := IfThen(argVAlue.IsEmpty, argName, argName + '=' + argValue);
		if (key = RG_ARG_OPTIONS) then begin
			if (_shell = TShellType.stPowershell) then begin
				if TRegEx.IsMatch(argName, RG_PARAM_REGEX_GLOB) then begin
					sParam := argName + '=' + argValue.QuotedString(_quoteChar);
				end;
			end;
		end else if MatchStr(key, [RG_ARG_SEARCH_PATH, RG_ARG_SEARCH_TEXT]) then begin
			sParam := argName.QuotedString(_quoteChar);
		end;
		_cmdLine.Add(sParam);
	end;
end;

procedure TRipGrepParameterSettings.Copy(const _other : TPersistableSettings);
begin
	FGuiSearchTextParams.Copy((_other as TRipGrepParameterSettings).GuiSearchTextParams());
	StoreToPersister;
	inherited Copy(_other);
end;

function TRipGrepParameterSettings.GetCommandLine(const _shell : TShellType) : string;
var
	cmdLine : IShared<TStringList>;
	quote : Nullable<char>;
begin
	cmdLine := Shared.Make<TStringList>();

	case _shell of
		TShellType.stPowershell : begin
			quote := '''';
			cmdLine.Add('&' + RipGrepPath.QuotedString(quote));
		end;
		TShellType.stCmd : begin
			quote := '"';
			cmdLine.Add(RipGrepPath.QuotedString(quote));
		end;
		TShellType.stNone : begin
			cmdLine.Add(RipGrepPath);
		end;
	end;

	if quote.HasValue then begin
		AddQuotedRgArgs(cmdLine, quote, _shell);
	end else begin
		cmdLine.AddStrings(RipGrepArguments.GetValues(RG_ARG_OPTIONS));
		cmdLine.AddStrings(RipGrepArguments.GetValues(RG_ARG_SEARCH_TEXT));
		cmdLine.AddStrings(RipGrepArguments.GetValues(RG_ARG_SEARCH_PATH));
	end;

	// DelimitedText puts unnecessary quotes so we build it
	for var s in cmdLine do begin
		Result := Result.Trim() + ' ' + s;
	end;
end;

function TRipGrepParameterSettings.GetFileMasks() : string;
begin
	Result := FFileMasks.Value;
end;

function TRipGrepParameterSettings.GetIsRgPathInitOk() : Boolean;
begin
	if rgpiFound <> FRipGrepPathInitResult then begin
		var
			tmp : string;
		FRipGrepPathInitResult := TryGetRipGrepPath(tmp);
		RipGrepPath := tmp; // Settings store should be called
	end;
	Result := FRipGrepPathInitResult = rgpiFound;
end;

function TRipGrepParameterSettings.GetRipGrepPath : string;
var
	path : string;
begin
	if (rgpiNotSet = FRipGrepPathInitResult) then begin
		FRipGrepPathInitResult := TryGetRipGrepPath(path);
		FRipGrepPath.Value := path;
	end;
	Result := FRipGrepPath.Value;
end;

function TRipGrepParameterSettings.GetSearchPath() : string;
begin
	Result := FSearchPath.Value;
end;

procedure TRipGrepParameterSettings.Init;
begin
	FSearchPath := TStringSetting.Create('');
	FRipGrepPath := TStringSetting.Create('');
	FFileMasks := TStringSetting.Create('');
	CreateSetting(RG_INI_KEY_RGPATH, FSearchPath);
	CreateSetting(SEARCHPATH_KEY, FSearchPath);
	CreateSetting(FILEMASKS_KEY, FFileMasks);
	// inherited Init(); abstract
end;

procedure TRipGrepParameterSettings.ReadFile();
begin
	inherited ReadFile();
end;

function TRipGrepParameterSettings.TryFindRipGrepExePath : string;
var
	rgExists : Boolean;
	rgPath : string;
	scoopRgPath : string;
	vscodeRgPath : string;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepParameterSettings.TryFindRipGrepExePath');
	rgPath := FRipGrepPath.Value;
	if rgPath.IsEmpty or (not FileExists(rgPath)) then begin
		rgExists := TFileUtils.FindExecutable(RG_EXE, rgPath);
		dbgMsg.MsgFmt('rgExists=%s, rgPath=%s', [BoolToStr(rgExists), rgPath]);
		if not rgExists then begin
			scoopRgPath := TPath.Combine(GetEnvironmentVariable('SCOOP'), 'apps\ripgrep\current\' + RG_EXE);
			if FileExists(scoopRgPath) then begin
				rgPath := scoopRgPath;
				dbgMsg.MsgFmt('%s found in scoopRgPath=%s', [RG_EXE, scoopRgPath]);
			end else begin
				var
					sVsDir : string := TFileUtils.GetVsCodeDir;
				if not sVsDir.IsEmpty then begin
					sVsDir := TFileUtils.ShortToLongPath(sVsDir.Remove(sVsDir.Length - '\bin'.Length));
					vscodeRgPath := TFileUtils.FindFileInSubDirs(TPath.Combine(sVsDir, VSCODE_RG_EXE_FIND_PATH), RG_EXE);
					if not vscodeRgPath.IsEmpty then begin
						rgPath := vscodeRgPath;
						dbgMsg.MsgFmt('%s found in vscodeRgPath=%s', [RG_EXE, vscodeRgPath]);
					end;
				end;
			end;
			if not FileExists(rgPath) then begin
				dbgMsg.MsgFmt('%s not found in rgPath=%s', [RG_EXE, rgPath]);
				rgPath := '';
			end;
			// raise Exception.Create('RipGrep(rg.exe) not found');
		end;
	end;
	Result := rgPath.Trim();
end;

procedure TRipGrepParameterSettings.LoadFromDict();
begin
	FGuiSearchTextParams.LoadFromDict();
end;

procedure TRipGrepParameterSettings.SetFileMasks(const Value : string);
begin
	FFileMasks.Value := Value;
end;

procedure TRipGrepParameterSettings.SetGuiSearchTextParams(const Value : IShared<TGuiSearchTextParams>);
begin
	RemoveChildSettings(FGuiSearchTextParams);
	FGuiSearchTextParams.Clear;
	FGuiSearchTextParams := Value;
	AddChildSettings(FGuiSearchTextParams);
	FIsModified := True;
end;

procedure TRipGrepParameterSettings.SetRgExeOptions(const Value : TOptionStrings);
begin
	if FRgExeOptions.AsString <> Value.AsString then begin
		FRgExeOptions := Value;
		FIsModified := True;
	end;
end;

procedure TRipGrepParameterSettings.SetSearchPath(const Value : string);
begin
	if FSearchPath.Value <> Value then begin
		TDebugUtils.Msg('SetSearchPath=' + Value);
		FSearchPath.Value := Value;
		FIsModified := True;
	end;
end;

procedure TRipGrepParameterSettings.SetReplaceText(const Value : string);
begin
	if FReplaceText <> Value then begin
		TDebugUtils.Msg('SetReplaceText=' + Value);
		FReplaceText := Value;
	end;
end;

procedure TRipGrepParameterSettings.SetRipGrepPath(const Value : string);
begin
	if FRipGrepPath.Value <> Value then begin
		FRipGrepPath.Value := Value;
		if FileExists(FRipGrepPath.Value) then begin
			FRipGrepPathInitResult := rgpiFound;
			StoreToPersister;
			UpdateFile();
		end;
	end;
end;

procedure TRipGrepParameterSettings.SetSearchText(const Value : string);
begin
	if FSearchText <> Value then begin
		FSearchText := Value;
		FIsModified := True;
	end;
end;

function TRipGrepParameterSettings.TryGetRipGrepPath(out _rgPath : string) : ERipGrepPathInitResult;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepParameterSettings.TryGetRipGrepPath');
	Result := rgpiNotFound;
	var
	iniVal := SettingsDict[IniSectionName][RG_INI_KEY_RGPATH].AsString;
	if iniVal.IsEmpty or not FileExists(iniVal) then begin
		_rgPath := TryFindRipGrepExePath();
		dbgMsg.Msg('Found:' + FRipGrepPath.Value);
	end else begin
		_rgPath := iniVal;
	end;
	if FileExists(_rgPath) then begin
		Result := rgpiFound;
	end;
end;

end.
