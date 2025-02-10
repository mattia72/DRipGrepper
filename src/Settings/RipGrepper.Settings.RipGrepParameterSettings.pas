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
	RipGrepper.Common.SimpleTypes;

type
	ERipGrepPathInitResult = (rgpiNotSet, rgpiFound, rgpiNotFound);

	TRipGrepParameterSettings = class(TPersistableSettings)
		const
			INI_SECTION = 'RipGrepSettings';

		private
			FRipGrepArguments : TRipGrepArguments;
			FRgExeOptions : TOptionStrings;
			FSearchPath : string;
			FSearchText : string;
			FReplaceText : string;
			FFileMasks : string;
			FGuiSearchTextParams : TGuiSearchTextParams;
			FRipGrepPathInitResult : ERipGrepPathInitResult;
			FRipGrepPath : string;
			function GetIsRgPathInitOk() : Boolean;
			function GetRipGrepPath : string;
			procedure SetFileMasks(const Value : string);
			procedure SetGuiSearchTextParams(const Value : TGuiSearchTextParams);
			procedure SetRgExeOptions(const Value : TOptionStrings);
			procedure SetSearchPath(const Value : string);
			procedure SetReplaceText(const Value : string);
			procedure SetRipGrepPath(const Value : string);
			procedure SetSearchText(const Value : string);
			function TryFindRipGrepExePath: string;
		protected
			procedure Init; override;

		public
			constructor Create(const _Owner : TPersistableSettings);
			destructor Destroy; override;
			procedure Copy(const _other : TPersistableSettings); override;
			procedure CopyDefaultsToValues; override;
			function GetCommandLine : string;
			procedure ReadIni; override;
			procedure LoadDefaultsFromDict; override;
			procedure LoadFromDict; override;
			procedure StoreToDict; override;
			procedure StoreAsDefaultsToDict; override;
			function TryGetRipGrepPath(out _rgPath : string): ERipGrepPathInitResult;
			property FileMasks : string read FFileMasks write SetFileMasks;
			property GuiSearchTextParams : TGuiSearchTextParams read FGuiSearchTextParams write SetGuiSearchTextParams;
			property IsRgPathInitOk : Boolean read GetIsRgPathInitOk;
			property RgExeOptions : TOptionStrings read FRgExeOptions write SetRgExeOptions;
			property SearchPath : string read FSearchPath write SetSearchPath;
			property SearchText : string read FSearchText write SetSearchText;
			property RipGrepArguments : TRipGrepArguments read FRipGrepArguments write FRipGrepArguments;
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
	RipGrepper.Tools.DebugUtils;

const
	FILEMASKS_KEY = 'FileMasks';
	SEARCHPATH_KEY = 'SearchPath';

constructor TRipGrepParameterSettings.Create(const _Owner : TPersistableSettings);
begin
	IniSectionName := INI_SECTION;
	inherited Create(_Owner);
	FGuiSearchTextParams := TGuiSearchTextParams.Create(_Owner, INI_SECTION);
	AddChildSettings(FGuiSearchTextParams);
	FRipGrepPathInitResult := rgpiNotSet;
	RipGrepPath := '';
	FRipGrepArguments := TStringList.Create;
end;

destructor TRipGrepParameterSettings.Destroy;
begin
	FRipGrepArguments.Free;
	inherited Destroy() // ok;
end;

procedure TRipGrepParameterSettings.Copy(const _other : TPersistableSettings);
begin
	var
	gstp := (_other as TRipGrepParameterSettings).GuiSearchTextParams;
	FGuiSearchTextParams.Copy(gstp);
	StoreToDict;
	inherited Copy(_other);
end;

procedure TRipGrepParameterSettings.CopyDefaultsToValues;
begin
	FGuiSearchTextParams.CopyDefaultsToValues;
	inherited CopyDefaultsToValues; // child not supported yet
end;

function TRipGrepParameterSettings.GetCommandLine : string;
var
	cmdLine : TStringList;
begin
	cmdLine := TStringList.Create();
	try
		cmdLine.Add(RipGrepPath);
		cmdLine.AddStrings(RipGrepArguments.GetValues());
		// DelimitedText puts unnecessary quotes so we build it
		for var s in cmdLine do begin
			Result := Result + ' ' + s;
		end;
	finally
		cmdLine.Free;
	end;
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
begin
	if (rgpiNotSet = FRipGrepPathInitResult) then begin
		FRipGrepPathInitResult := TryGetRipGrepPath(FRipGrepPath);
	end;
	Result := FRipGrepPath;
end;

procedure TRipGrepParameterSettings.Init;
begin
	SettingsDict.CreateSetting(RG_INI_KEY_RGPATH, varString, '');
	SettingsDict.CreateDefaultRelevantSetting(SEARCHPATH_KEY, varString, '');
	SettingsDict.CreateDefaultRelevantSetting(FILEMASKS_KEY, varString, '');
	// inherited Init(); abstract
end;

procedure TRipGrepParameterSettings.ReadIni;
begin
	inherited ReadIni();
end;

procedure TRipGrepParameterSettings.LoadDefaultsFromDict;
begin
	FSearchPath := SettingsDict.GetSetting(SEARCHPATH_KEY, True);
	FFileMasks := SettingsDict.GetSetting(FILEMASKS_KEY, True);
	FGuiSearchTextParams.LoadDefaultsFromDict;
	// inherited LoadDefaultsFromDict;  abstract
end;

function TRipGrepParameterSettings.TryFindRipGrepExePath: string;
var
	rgExists : Boolean;
	rgPath : string;
	scoopRgPath : string;
	vscodeRgPath : string;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepParameterSettings.TryFindRipGrepExePath');
	rgPath := FRipGrepPath;
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
	RipGrepPath := SettingsDict.GetSetting(RG_INI_KEY_RGPATH);
	FSearchPath := SettingsDict.GetSetting('SearchPath');
	FFileMasks := SettingsDict.GetSetting('FileMasks');
	FGuiSearchTextParams.LoadFromDict();
	// inherited abstract
end;

procedure TRipGrepParameterSettings.SetFileMasks(const Value : string);
begin
	if FFileMasks <> Value then begin
		FFileMasks := Value;
		FIsModified := True;
	end;
end;

procedure TRipGrepParameterSettings.SetGuiSearchTextParams(const Value : TGuiSearchTextParams);
begin
	RemoveChildSettings(FGuiSearchTextParams);
	FGuiSearchTextParams.Free;
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
	if FSearchPath <> Value then begin
		TDebugUtils.Msg('SetSearchPath=' + Value);
		FSearchPath := Value;
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
	if FRipGrepPath <> Value then begin
		FRipGrepPath := Value;
		if FileExists(FRipGrepPath) then begin
			FRipGrepPathInitResult := rgpiFound;
			StoreToDict;
			UpdateIniFile();
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

procedure TRipGrepParameterSettings.StoreToDict;
begin
	SettingsDict.StoreSetting(RG_INI_KEY_RGPATH, RipGrepPath);
	SettingsDict.StoreSetting('SearchPath', FSearchPath);
	SettingsDict.StoreSetting('FileMasks', FFileMasks);
	inherited StoreToDict; // Children will be stored and it writes into mem ini. after UpdateIniFile will be saved
end;

procedure TRipGrepParameterSettings.StoreAsDefaultsToDict;
begin
	SettingsDict.StoreDefaultSetting('SearchPath', FSearchPath);
	SettingsDict.StoreDefaultSetting('FileMasks', FFileMasks);
	GuiSearchTextParams.StoreAsDefaultsToDict();
	CopySettingsDictSection(GuiSearchTextParams);
	inherited StoreAsDefaultsToDict;
end;

function TRipGrepParameterSettings.TryGetRipGrepPath(out _rgPath : string): ERipGrepPathInitResult;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepParameterSettings.TryGetRipGrepPath');
	Result := rgpiNotFound;
	var
	iniVal := IniFile.ReadString(IniSectionName, RG_INI_KEY_RGPATH, '');
	if iniVal.IsEmpty or not FileExists(iniVal) then begin
		_rgPath := TryFindRipGrepExePath();
		dbgMsg.Msg('Found:' + FRipGrepPath);
	end else begin
		_rgPath := iniVal;
	end;
	if FileExists(_rgPath) then begin
		Result := rgpiFound;
	end;
end;

end.
