unit RipGrepper.Common.Settings.RipGrepParameterSettings;

interface

uses
	System.Classes,
	System.IniFiles,
	ArrayEx,
	RipGrepper.Common.Constants,
	RipGrepper.Common.Settings.Persistable,
	RipGrepper.Common.GuiSearchParams,
	RipGrepper.Helper.Types,
	System.Generics.Collections,
	RipGrepper.CommandLine.OptionStrings,
	RipGrepper.Common.SimpleTypes;

type

	TRipGrepParameterSettings = class(TPersistableSettings)
		const
			INI_SECTION = 'RipGrepSettings';

		private
			FbRgPathInitOk : Boolean;
			FRipGrepArguments : TRipGrepArguments;
			FRgExeOptions : TOptionStrings;
			FRipGrepPath : string;
			FSearchPath : string;
			FSearchText : string;
			FReplaceText : string;
			FFileMasks : string;
			FGuiSearchTextParams : TGuiSearchTextParams;
			function GetRipGrepPath : string;
			procedure SetFileMasks(const Value : string);
			procedure SetGuiSearchTextParams(const Value : TGuiSearchTextParams);
			procedure SetRgExeOptions(const Value : TOptionStrings);
			procedure SetSearchPath(const Value : string);
			procedure SetReplaceText(const Value : string);
			procedure SetSearchText(const Value : string);

		protected
			procedure Init; override;

		public
			constructor Create(const _ini : TMemIniFile);
			destructor Destroy; override;
			procedure CopyDefaultsToValues; override;
			function GetCommandLine : string;
			procedure InitRipGrepExePath;
			procedure ReadIni; override;
			procedure LoadDefaultsFromDict; override;
			procedure LoadFromDict; override;
			procedure StoreToDict; override;
			procedure StoreAsDefaultsToDict; override;
			property FileMasks : string read FFileMasks write SetFileMasks;
			property GuiSearchTextParams : TGuiSearchTextParams read FGuiSearchTextParams write SetGuiSearchTextParams;
			property RgExeOptions : TOptionStrings read FRgExeOptions write SetRgExeOptions;
			property SearchPath : string read FSearchPath write SetSearchPath;
			property SearchText : string read FSearchText write SetSearchText;
			property RipGrepArguments : TRipGrepArguments read FRipGrepArguments write FRipGrepArguments;
			property RipGrepPath : string read GetRipGrepPath write FRipGrepPath;
			property ReplaceText : string read FReplaceText write SetReplaceText;
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

constructor TRipGrepParameterSettings.Create(const _ini : TMemIniFile);
begin
	IniSectionName := INI_SECTION;
	inherited Create(_ini);
	FGuiSearchTextParams := TGuiSearchTextParams.Create(_ini, INI_SECTION);
	FbRgPathInitOk := False;
	RipGrepPath := '';
	FRipGrepArguments := TStringList.Create;
end;

destructor TRipGrepParameterSettings.Destroy;
begin
	FGuiSearchTextParams.Free;
	FRipGrepArguments.Free;
	inherited Destroy() // ok;
end;

procedure TRipGrepParameterSettings.CopyDefaultsToValues;
begin
	FGuiSearchTextParams.CopyDefaultsToValues;
	inherited CopyDefaultsToValues;
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

function TRipGrepParameterSettings.GetRipGrepPath : string;
begin
	if not FbRgPathInitOk then begin
		InitRipGrepExePath();
	end;
	Result := FRipGrepPath;
end;

procedure TRipGrepParameterSettings.Init;
begin
	SettingsDict.CreateSetting(RG_INI_KEY_RGPATH, varString, '');
	SettingsDict.CreateDefaultRelevantSetting('SearchPath', varString, '');
	SettingsDict.CreateDefaultRelevantSetting('FileMasks', varString, '');
end;

procedure TRipGrepParameterSettings.ReadIni;
begin
	FGuiSearchTextParams.ReadIni;
	inherited ReadIni();
end;

procedure TRipGrepParameterSettings.LoadDefaultsFromDict;
begin
	FGuiSearchTextParams.LoadDefaultsFromDict;
end;

procedure TRipGrepParameterSettings.InitRipGrepExePath;
var
	rgExists : Boolean;
	rgPath : string;
	scoopRgPath : string;
	vscodeRgPath : string;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New(' TRipGrepParameterSettings.InitRipGrepExePath');
	if FRipGrepPath.IsEmpty or (not FileExists(FRipGrepPath)) then begin
		rgExists := TFileUtils.FindExecutable('rg.exe', rgPath);
		dbgMsg.MsgFmt('rgExists=%s, rgPath=%s', [BoolToStr(rgExists), rgPath]);
		if not rgExists then begin
			scoopRgPath := TPath.Combine(GetEnvironmentVariable('SCOOP'), 'apps\ripgrep\current\rg.exe');
			if FileExists(scoopRgPath) then begin
				rgPath := scoopRgPath;
				dbgMsg.MsgFmt('RG.EXE Found in scoopRgPath=%s', [scoopRgPath]);
			end else begin
				var
					sVsDir : string := TFileUtils.GetVsCodeDir;
				if not sVsDir.IsEmpty then begin
					sVsDir := TFileUtils.ShortToLongPath(sVsDir.Remove(sVsDir.Length - '\bin'.Length));
					vscodeRgPath := TFileUtils.FindFileInSubDirs(TPath.Combine(sVsDir, VSCODE_RG_EXE_FIND_PATH), 'rg.exe');
					if not vscodeRgPath.IsEmpty then begin
						rgPath := vscodeRgPath;
						dbgMsg.MsgFmt('RG.EXE Found in vscodeRgPath=%s', [vscodeRgPath]);
					end;
				end;
			end;
			if not FileExists(rgPath) then begin
				dbgMsg.MsgFmt('RG.EXE not found in rgPath=%s', [rgPath]);
				TMsgBox.ShowError(Format(FORMAT_RIPGREP_EXE_NOT_FOUND, [FIniFile.FileName]));
				rgPath := '';
			end;
			// raise Exception.Create('RipGrep(rg.exe) not found');
		end;
		FRipGrepPath := rgPath.Trim();
	end;
	FbRgPathInitOk := FileExists(FRipGrepPath);
end;

procedure TRipGrepParameterSettings.LoadFromDict();
begin
	FSearchPath := SettingsDict.GetSetting('SearchPath');
	FFileMasks := SettingsDict.GetSetting('FileMasks');
	FGuiSearchTextParams.LoadFromDict();
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
	FGuiSearchTextParams.Free;
	FGuiSearchTextParams := Value;
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
		// FIsModified := True;
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
	GuiSearchTextParams.StoreToDict;
	inherited StoreToDict; // Write to mem ini, after UpdateIniFile will be saved
end;

procedure TRipGrepParameterSettings.StoreAsDefaultsToDict;
begin
	SettingsDict.StoreDefaultSetting('SearchPath', FSearchPath);
	SettingsDict.StoreDefaultSetting('FileMasks', FFileMasks);
	GuiSearchTextParams.StoreAsDefaultsToDict();
	inherited StoreAsDefaultsToDict;
end;

end.
