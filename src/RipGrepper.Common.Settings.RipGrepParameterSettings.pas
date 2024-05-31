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
			FRipGrepArguments : TRipGrepArguments;
			FRgExeOptions : string;
			FRipGrepPath : string;
			FSearchPath : string;
			FSearchText : string;
			FFileMasks : string;
			FGuiSetSearchParams : TGuiSetSearchParams;
			procedure SetFileMasks(const Value : string);
			procedure SetGuiSetSearchParams(const Value : TGuiSetSearchParams);
			procedure SetRgExeOptions(const Value : string);
			procedure SetSearchPath(const Value : string);
			procedure SetSearchText(const Value : string);

		protected
			procedure Init; override;

		public
			constructor Create(const _ini : TIniFile);
			destructor Destroy; override;
			function GetCommandLine : string;
			function GetIniSectionName : string; override;
			procedure InitRipGrepExePath;
			procedure Load; override;
			procedure Store; override;
			property FileMasks : string read FFileMasks write SetFileMasks;
			property GuiSetSearchParams : TGuiSetSearchParams read FGuiSetSearchParams write SetGuiSetSearchParams;
			property RgExeOptions : string read FRgExeOptions write SetRgExeOptions;
			property SearchPath : string read FSearchPath write SetSearchPath;
			property SearchText : string read FSearchText write SetSearchText;
			property RipGrepArguments : TRipGrepArguments read FRipGrepArguments write FRipGrepArguments;
			property RipGrepPath : string read FRipGrepPath write FRipGrepPath;
	end;

implementation

uses
	System.SysUtils,
	Vcl.Dialogs,
	RipGrepper.Tools.FileUtils,
	System.IOUtils,
	Vcl.Forms,
	RipGrepper.Tools.ProcessUtils,
	System.RegularExpressions;

constructor TRipGrepParameterSettings.Create(const _ini : TIniFile);
begin
	inherited Create(_ini);
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
	if RipGrepPath.IsEmpty or (not FileExists(RipGrepPath)) then begin
		rgExists := TFileUtils.FindExecutable('rg.exe', rgPath);
		if not rgExists then begin
			MessageDlg('rg.exe not found', mtError, [mbOk], 0);
			Application.Terminate();
		end;
		scoopInstall := TPath.Combine(GetEnvironmentVariable('SCOOP'), 'apps\ripgrep\current\rg.exe');
		if FileExists(scoopInstall) then begin
			rgPath := scoopInstall;
		end;

		RipGrepPath := rgPath.Trim();
	end;
end;

procedure TRipGrepParameterSettings.Load;
begin
	inherited Load();
	RipGrepPath := LoadSetting(RG_INI_KEY_RGPATH);
end;

procedure TRipGrepParameterSettings.SetFileMasks(const Value : string);
begin
	if FFileMasks <> Value then begin
		FFileMasks := Value;
		FIsModified := True;
	end;
end;

procedure TRipGrepParameterSettings.SetGuiSetSearchParams(const Value : TGuiSetSearchParams);
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
    inherited Store; //Write to ini
end;

end.
