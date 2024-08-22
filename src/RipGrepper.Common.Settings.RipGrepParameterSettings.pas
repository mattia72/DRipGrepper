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
	System.Generics.Collections;

type

	TRipGrepParameterSettings = class(TPersistableSettings)
		const
			INI_SECTION = 'RipGrepSettings';

		private
			FbRgPathInitOk : Boolean;
			FRipGrepArguments : TRipGrepArguments;
			FRgExeOptions : string;
			FRipGrepPath : string;
			FSearchPath : string;
			FSearchText : string;
			FFileMasks : string;
			FGuiSearchTextParams : TGuiSearchTextParams;
			function GetRipGrepPath : string;
			procedure SetFileMasks(const Value : string);
			procedure SeTGuiSearchTextParams(const Value : TGuiSearchTextParams);
			procedure SetRgExeOptions(const Value : string);
			procedure SetSearchPath(const Value : string);
			procedure SetSearchText(const Value : string);

		protected
			procedure Init; override;

		public
			constructor Create(const _ini : TMemIniFile);
			destructor Destroy; override;
			function GetCommandLine : string;
			procedure InitRipGrepExePath;
			procedure Load; override;
			procedure LoadDefault; override;
			procedure Store; override;
			procedure StoreAsDefault; override;
			property FileMasks : string read FFileMasks write SetFileMasks;
			property GuiSearchTextParams : TGuiSearchTextParams read FGuiSearchTextParams write SeTGuiSearchTextParams;
			property RgExeOptions : string read FRgExeOptions write SetRgExeOptions;
			property SearchPath : string read FSearchPath write SetSearchPath;
			property SearchText : string read FSearchText write SetSearchText;
			property RipGrepArguments : TRipGrepArguments read FRipGrepArguments write FRipGrepArguments;
			property RipGrepPath : string read GetRipGrepPath write FRipGrepPath;
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
	RipGrepper.Helper.UI;

constructor TRipGrepParameterSettings.Create(const _ini : TMemIniFile);
begin
	inherited Create(_ini);
	IniSectionName := INI_SECTION;
	FGuiSearchTextParams := TGuiSearchTextParams.Create(_ini);
    FGuiSearchTextParams.IniSectionName := INI_SECTION;
	FbRgPathInitOk := False;
	RipGrepPath := '';
	FRipGrepArguments := TStringList.Create;
end;

destructor TRipGrepParameterSettings.Destroy;
begin
	FGuiSearchTextParams.Free;
	FRipGrepArguments.Free;
	inherited;
end;

function TRipGrepParameterSettings.GetCommandLine : string;
var
	cmdLine : TStringList;
begin
	cmdLine := TStringList.Create();
	try
		cmdLine.Add(RipGrepPath);
		cmdLine.AddStrings(RipGrepArguments.GetValues());
		cmdLine.Delimiter := ' ';
		Result := cmdLine.DelimitedText;
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
	inherited Init();
	CreateSetting(RG_INI_KEY_RGPATH, TRipGrepperSetting.New(varString, ''));
	CreateDefaultSetting('SearchPath', TRipGrepperSetting.New(varString, ''));
	CreateDefaultSetting('FileMasks', TRipGrepperSetting.New(varString, ''));
end;

procedure TRipGrepParameterSettings.Load;
begin
	inherited Load();
	FRipGrepPath := LoadSetting(RG_INI_KEY_RGPATH);
	FRipGrepPath := FRipGrepPath.Trim(['"', '''']);

	LoadDefault;
end;

procedure TRipGrepParameterSettings.InitRipGrepExePath;
var
	rgExists : Boolean;
	rgPath : string;
	scoopRgPath : string;
	vscodeRgPath : string;
begin

	if FRipGrepPath.IsEmpty or (not FileExists(FRipGrepPath)) then begin
		rgExists := TFileUtils.FindExecutable('rg.exe', rgPath);
		if not rgExists then begin
			scoopRgPath := TPath.Combine(GetEnvironmentVariable('SCOOP'), 'apps\ripgrep\current\rg.exe');
			if FileExists(scoopRgPath) then begin
				rgPath := scoopRgPath;
			end else begin
				var
					sVsDir : string := TFileUtils.GetVsCodeDir;
				if not sVsDir.IsEmpty then begin
					sVsDir := TFileUtils.ShortToLongPath(sVsDir.Remove(sVsDir.Length - '\bin'.Length));
					vscodeRgPath := TPath.Combine(sVsdir, VSCODE_RG_EXE_PATH);
					if FileExists(vscodeRgPath) then begin
						rgPath := vscodeRgPath;
					end;
				end;
			end;
			if not FileExists(rgPath) then begin
				TMsgBox.ShowError(Format(FORMAT_RIPGREP_EXE_NOT_FOUND, [FIniFile.FileName]));
			end;
			// raise Exception.Create('RipGrep(rg.exe) not found');
		end;

		FRipGrepPath := rgPath.Trim();
	end;
	FbRgPathInitOk := True;
end;

procedure TRipGrepParameterSettings.LoadDefault;
begin
	FSearchPath := LoadDefaultSetting('SearchPath');
	FFileMasks := LoadDefaultSetting('FileMasks');
end;

procedure TRipGrepParameterSettings.SetFileMasks(const Value : string);
begin
	if FFileMasks <> Value then begin
		FFileMasks := Value;
		FIsModified := True;
	end;
end;

procedure TRipGrepParameterSettings.SeTGuiSearchTextParams(const Value : TGuiSearchTextParams);
begin
	FGuiSearchTextParams := Value;
	FIsModified := True;
end;

procedure TRipGrepParameterSettings.SetRgExeOptions(const Value : string);
begin
	if FRgExeOptions <> Value then begin
		FRgExeOptions := Value;
		FIsModified := True;
	end;
end;

procedure TRipGrepParameterSettings.SetSearchPath(const Value : string);
begin
	if FSearchPath <> Value then begin
		FSearchPath := Value;
		FIsModified := True;
	end;
end;

procedure TRipGrepParameterSettings.SetSearchText(const Value : string);
begin
	if FSearchText <> Value then begin
		FSearchText := Value;
		FIsModified := True;
	end;
end;

procedure TRipGrepParameterSettings.Store;
begin
	StoreSetting(RG_INI_KEY_RGPATH, RipGrepPath);
	inherited Store; // Write to mem ini, after UpdateIniFile will be saved
end;

procedure TRipGrepParameterSettings.StoreAsDefault;
begin
	StoreDefaultSetting('SearchPath', FSearchPath);
	StoreDefaultSetting('FileMasks', FFileMasks);

	GuiSearchTextParams.StoreAsDefault();

	inherited StoreAsDefault; // Write to mem ini, after UpdateIniFile will be saved
end;

end.
