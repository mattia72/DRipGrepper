unit RipGrepper.Common.Settings.RipGrepParameterSettings;

interface

uses
	RipGrepper.Common.Settings.Base,
	System.Classes,
	System.IniFiles,
	RipGrepper.Common.Constants,
	ArrayEx,
	RipGrepper.Helper.Types, System.Generics.Collections;

type

	TGuiSetSearchParams = record
		public
			SearchText : string;
			EscapedSearchText : string;
			SearchOptions : set of EGuiOption;
			function IsSet(_options : TArray<EGuiOption>) : Boolean;
			class function New(const _sText : string; const _bIC, _bMW, _bUR : Boolean) : TGuiSetSearchParams; static;
			function SearchOptionsAsBitField : TBitField;
			procedure SetOption(const _searchOption : EGuiOption);
			procedure ResetOption(const _searchOption : EGuiOption);
			function ToString : string;
	end;

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
end;

function TGuiSetSearchParams.IsSet(_options : TArray<EGuiOption>) : Boolean;
begin
	Result := True;
	for var o in _options do begin
		if not(o in SearchOptions) then begin
			Result := False;
			break
		end;
	end;
end;

class function TGuiSetSearchParams.New(const _sText : string; const _bIC, _bMW, _bUR : Boolean) : TGuiSetSearchParams;
begin
	Result.SearchText := _sText;
	if _bIC then
		Include(Result.SearchOptions, EGuiOption.soMatchCase);
	if _bMW then
		Include(Result.SearchOptions, EGuiOption.soMatchWord);
	if _bUR then
		Include(Result.SearchOptions, EGuiOption.soUseRegex);
end;

function TGuiSetSearchParams.SearchOptionsAsBitField : TBitField;
begin
	for var i in GUI_SEARCH_PARAMS do begin
		if i in SearchOptions then begin
			Result.SetBit(Integer(i));
		end;
	end;
end;

procedure TGuiSetSearchParams.SetOption(const _searchOption : EGuiOption);
begin
	Include(SearchOptions, _searchOption);
end;

procedure TGuiSetSearchParams.ResetOption(const _searchOption : EGuiOption);
begin
	Exclude(SearchOptions, _searchOption);
end;

function TGuiSetSearchParams.ToString : string;
var
	arr : TArrayEx<string>;
begin
	Result := '';
	for var i in GUI_SEARCH_PARAMS do begin
		if i in SearchOptions then begin
			case i of
				EGuiOption.soMatchCase :
				arr.Add('MatchCase');
				EGuiOption.soMatchWord :
				arr.Add('soMatchWord');
				EGuiOption.soUseRegex :
				arr.Add('soUseRegex');
			end;
		end;
	end;

	Result := string.Join(',', arr.Items);
end;

end.
