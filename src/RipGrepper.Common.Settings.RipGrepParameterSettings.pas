unit RipGrepper.Common.Settings.RipGrepParameterSettings;

interface

uses
	RipGrepper.Common.Settings.Base,
	System.Classes,
	System.IniFiles,
	RipGrepper.Common.Constants,
	ArrayEx;

type
	TRipGrepParameterSettings = class(TRipGrepperSettingsBase)
		const
			INI_SECTION = 'RipGrepSettings';

		private
			FRipGrepArguments : TRipGrepArguments;
			FOptions : string;
			FRipGrepPath : string;
			FSearchPath : string;
			FSearchText : string;
			FFileMasks: string;
			FMatchWholeWord : Boolean;
			procedure SetFileMasks(const Value: string);
			procedure SetOptions(const Value : string);
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
			property FileMasks: string read FFileMasks write SetFileMasks;
			property MatchWholeWord : Boolean read FMatchWholeWord write FMatchWholeWord;
			property Options : string read FOptions write SetOptions;
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
	System.RegularExpressions,
	RipGrepper.Helper.Types;

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

procedure TRipGrepParameterSettings.SetFileMasks(const Value: string);
begin
	if FFileMasks <> Value then begin
		FFileMasks := Value;
		FIsModified := True;
	end;
end;

procedure TRipGrepParameterSettings.SetOptions(const Value : string);
begin
	if FOptions <> Value then begin
		FOptions := Value;
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
	// quote string if it contains whitespace hapens automatically in TProcess :)
	if FSearchText <> Value then begin
		FSearchText := Value;
		FIsModified := True;
	end;
end;

procedure TRipGrepParameterSettings.Store;
begin
	StoreSetting(RG_INI_KEY_RGPATH, RipGrepPath);
end;

end.
