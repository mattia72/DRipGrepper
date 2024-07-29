unit RipGrepper.Common.Settings.RipGrepParameterSettings;

interface

uses
	System.Classes,
	System.IniFiles,
	ArrayEx,
	RipGrepper.Common.Constants,
	RipGrepper.Common.Settings.Base,
	RipGrepper.Common.GuiSearchParams,
	RipGrepper.Helper.Types,
	System.Generics.Collections;

type

	TRipGrepParameterSettings = class(TRipGrepperSettingsBase)
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
			FGuiSetSearchParams : TGuiSearchTextParams;
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
			function GetIniSectionName : string; override;
			procedure InitRipGrepExePath;
			procedure Load; override;
			procedure Store; override;
			property FileMasks : string read FFileMasks write SetFileMasks;
			property GuiSetSearchParams : TGuiSearchTextParams read FGuiSetSearchParams write SeTGuiSearchTextParams;
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
	FbRgPathInitOk := False;
	RipGrepPath := '';
	FRipGrepArguments := TStringList.Create;
end;

destructor TRipGrepParameterSettings.Destroy;
begin
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

function TRipGrepParameterSettings.GetIniSectionName : string;
begin
	Result := INI_SECTION;
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
	inherited;
	CreateSetting(RG_INI_KEY_RGPATH, TRipGrepperSetting.New(vtString, ''));
end;

procedure TRipGrepParameterSettings.InitRipGrepExePath;
var
	rgExists : Boolean;
	rgPath : string;
	scoopInstall : string;
begin

	if FRipGrepPath.IsEmpty or (not FileExists(FRipGrepPath)) then begin
		rgExists := TFileUtils.FindExecutable('rg.exe', rgPath);
		if not rgExists then begin
			TMsgBox.ShowError(Format(FORMAT_RIPGREP_EXE_NOT_FOUND, [FIniFile.FileName]));
			// raise Exception.Create('RipGrep(rg.exe) not found');
		end;
		scoopInstall := TPath.Combine(GetEnvironmentVariable('SCOOP'), 'apps\ripgrep\current\rg.exe');
		if FileExists(scoopInstall) then begin
			rgPath := scoopInstall;
		end;

		FRipGrepPath := rgPath.Trim();
	end;
	FbRgPathInitOk := True;
end;

procedure TRipGrepParameterSettings.Load;
begin
	inherited Load();
	FRipGrepPath := LoadSetting(RG_INI_KEY_RGPATH);
	FRipGrepPath := FRipGrepPath.Trim(['"', '''']);
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
	FGuiSetSearchParams := Value;
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
	inherited Store; // Write to ini
end;

end.
