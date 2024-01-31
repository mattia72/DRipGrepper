unit RipGrepper.Common.Settings;

interface

uses
	System.Classes,
	System.IniFiles,
	System.Generics.Collections,
	System.Generics.Defaults;

type
	ISettingsPersister = interface
		['{A841C46D-56AF-4391-AB88-4C9496589FF4}']
		procedure Load;
		procedure Store;
	end;

	TRipGrepperSettingsBase = class(TSingletonImplementation, ISettingsPersister)
		private
			function GetIniFile : TIniFile;
			procedure SetIniFile(const Value : TIniFile);
		protected
			FIniFile : TIniFile;
			FIsLoaded : Boolean;
		public
			constructor Create(const _ini : TIniFile);
			procedure Load; virtual; abstract;
			procedure Store; virtual; abstract;
			property IniFile : TIniFile read GetIniFile write SetIniFile;
			property IsLoaded : Boolean read FIsLoaded;
	end;

	TRipGrepParameterSettings = class(TRipGrepperSettingsBase)
		private
			FRipGrepArguments : TStrings;
			FOptions : string;
			FRipGrepPath : string;
			FSearchPath : string;
			FSearchText : string;
			procedure AddArgs(const _args : TArray<string>; const _bQuote : Boolean = False);
		public
			constructor Create(const _ini : TIniFile);
			destructor Destroy; override;
			function BuildCmdLine : string;
			procedure InitRipGrepExePath;
			procedure Load; override;
			function ReBuildArguments : TStrings;
			procedure Store; override;
			property Options : string read FOptions write FOptions;
			property SearchPath : string read FSearchPath write FSearchPath;
			property SearchText : string read FSearchText write FSearchText;
			property RipGrepArguments : TStrings read FRipGrepArguments write FRipGrepArguments;
			property RipGrepPath : string read FRipGrepPath write FRipGrepPath;
	end;

	TRipGrepperViewSettings = class(TRipGrepperSettingsBase)
		const
			VIEW_SETTINGS : array [0 .. 3] of string = ('ShowRelativePath', 'ShowFileIcon', 'AlternateRowColors', 'IndentLines');

		var
		private
			FAlternateRowColors : Boolean;
		public
			IndentLines : Boolean;
			ShowFileIcon : Boolean;
			ShowRelativePath : Boolean;
			procedure StoreViewSettings(_ini : TIniFile; const _s : string = '');
			property AlternateRowColors : Boolean read FAlternateRowColors write FAlternateRowColors;
			constructor Create(const _ini : TIniFile);
			procedure Init;
			procedure Load; override;
			procedure Store; override;
	end;

	TRipGrepperOpenWithSettings = class(TRipGrepperSettingsBase)
		private
			FCommandList : TStringList;
			function GetCommand(Index : Integer) : string;
			procedure SetCommand(Index : Integer; const Value : string);
		public
			constructor Create(const _ini : TIniFile);
			destructor Destroy; override;
			procedure Load; override;
			procedure Store; override;
			property Command[index : Integer] : string read GetCommand write SetCommand;
	end;

	TRipGrepperSettings = class(TRipGrepperSettingsBase)
		const
			MAX_HISTORY_COUNT = 20;
		private
		var
			FRipGrepParameters : TRipGrepParameterSettings;
			FRipGrepperViewSettings : TRipGrepperViewSettings;
			FRipGrepperOpenWithSettings : TRipGrepperOpenWithSettings;
			FRipGrepParamsHistory : TSTrings;
			FSearchPathsHistory : TStrings;
			FSearchTextsHistory : TStrings;
			FRipGrepArguments : TStrings;
		class var
			function GetActualRipGrepParam : string;
			function GetActualSearchPath : string;
			function GetActualSearchText : string;
			function GetIsEmpty : Boolean;
			procedure InitSettings;
			procedure LoadHistoryEntries(var _list : TStrings; const _section : string);
			procedure StoreHistoryEntries(const _list : TStrings; const _section : string);
		public
			procedure Load; override;
			procedure Store; override;
			procedure StoreViewSettings(const _s : string = '');
			constructor Create;
			destructor Destroy; override;
			function GetRipGrepArguments : TStrings;
			function ReBuildArguments : TStrings;
			property ActualRipGrepParam : string read GetActualRipGrepParam;
			property ActualSearchPath : string read GetActualSearchPath;
			property ActualSearchText : string read GetActualSearchText;
			property IsEmpty : Boolean read GetIsEmpty;

			property RipGrepParameters : TRipGrepParameterSettings read FRipGrepParameters write FRipGrepParameters;
			property SearchPathsHistory : TStrings read FSearchPathsHistory;
			property RipGrepParamsHistory : TSTrings read FRipGrepParamsHistory;
			property RipGrepperOpenWithSettings : TRipGrepperOpenWithSettings read FRipGrepperOpenWithSettings;
			property RipGrepperViewSettings : TRipGrepperViewSettings read FRipGrepperViewSettings write FRipGrepperViewSettings;
			property SearchTextsHistory : TStrings read FSearchTextsHistory;
	end;

	TRipGrepperSettingsInstance = class
		private
			class var FInstance : TRipGrepperSettings;
			class function GetInstance : TRipGrepperSettings; static;
		public
			constructor Create;
			class destructor Destroy;
			procedure FreeInstance; reintroduce;
			class property Instance : TRipGrepperSettings read GetInstance;
	end;

implementation

uses
	System.SysUtils,
	Vcl.Forms,
	System.StrUtils,
	RipGrepper.Common.Types,
	RipGrepper.Helper.Types,
	RipGrepper.Tools.DebugTools,
	RipGrepper.Tools.FileUtils,
	Vcl.Dialogs,
	System.IOUtils,
	Winapi.Windows,
	System.UITypes,
	RipGrepper.Tools.ProcessUtils,
	RipGrepper.OpenWith.Constants;

function TRipGrepperSettings.GetActualRipGrepParam : string;
begin
	RipGrepParamsHistory.TryGetDef(0, Result);
end;

function TRipGrepperSettings.GetActualSearchPath : string;
begin
	SearchPathsHistory.TryGetDef(0, Result);
end;

function TRipGrepperSettings.GetActualSearchText : string;
begin
	SearchTextsHistory.TryGetDef(0, Result);
end;

function TRipGrepperSettings.GetIsEmpty : Boolean;
begin
	Result := FRipGrepParameters.RipGrepPath.IsEmpty;
end;

function TRipGrepperSettings.GetRipGrepArguments : TStrings;
begin
	Result := FRipGrepParameters.RipGrepArguments;
end;

procedure TRipGrepperSettings.InitSettings;
begin
	FRipGrepParameters.InitRipGrepExePath();

	if SearchPathsHistory.Count = 0 then begin
		SearchPathsHistory.Add(TDirectory.GetCurrentDirectory());
	end;

	if SearchTextsHistory.Count = 0 then begin
		SearchTextsHistory.Add('search text');
	end;

	if RipGrepParamsHistory.Count = 0 then begin
		RipGrepParamsHistory.Add('');
	end;
end;

procedure TRipGrepperSettings.LoadHistoryEntries(var _list : TStrings; const _section : string);
begin
	for var i := 0 to MAX_HISTORY_COUNT do begin
		var
		s := FIniFile.ReadString(_section, 'Item_' + i.ToString, '');
		if s <> '' then begin
			_list.Add(s);
		end
		else
			break;
	end;
end;

destructor TRipGrepperSettings.Destroy;
begin
	FRipGrepArguments.Free;
	FRipGrepParamsHistory.Free;
	FSearchTextsHistory.Free;
	FSearchPathsHistory.Free;
	FRipGrepperViewSettings.Free;
	FRipGrepperOpenWithSettings.Free;
	FRipGrepParameters.Free;
	FIniFile.Free;
	inherited;
end;

constructor TRipGrepperSettings.Create;
begin
	FIniFile := TIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'));
	FRipGrepParameters := TRipGrepParameterSettings.Create(FIniFile);
	FRipGrepperViewSettings := TRipGrepperViewSettings.Create(FIniFile);
	FRipGrepperOpenWithSettings := TRipGrepperOpenWithSettings.Create(FIniFile);
	FRipGrepperViewSettings.Init();
	FSearchPathsHistory := TStringList.Create;
	FSearchTextsHistory := TStringList.Create;
	FRipGrepParamsHistory := TStringList.Create;
	FRipGrepArguments := TStringList.Create();
	FIsLoaded := False;
end;

procedure TRipGrepperSettings.Load;
begin
	FRipGrepParameters.Load;
	FRipGrepperViewSettings.Load;
	FRipGrepperOpenWithSettings.Load;

	LoadHistoryEntries(FSearchPathsHistory, 'SearchPathsHistory');
	LoadHistoryEntries(FSearchTextsHistory, 'SearchTextsHistory');
	LoadHistoryEntries(FRipGrepParamsHistory, 'RipGrepParamsHistory');

	InitSettings;
	FIsLoaded := True;
end;

function TRipGrepperSettings.ReBuildArguments : TStrings;
begin
	Result := FRipGrepParameters.ReBuildArguments;
end;

procedure TRipGrepperSettings.Store;
begin
	if IsLoaded then begin
		FRipGrepParameters.Store;
		FRipGrepperViewSettings.Store;
		FRipGrepperOpenWithSettings.Store;

		StoreHistoryEntries(SearchPathsHistory, 'SearchPathsHistory');
		StoreHistoryEntries(SearchTextsHistory, 'SearchTextsHistory');
		StoreHistoryEntries(RipGrepParamsHistory, 'RipGrepParamsHistory');

	end;
end;

procedure TRipGrepperSettings.StoreViewSettings(const _s : string = '');
begin
	FRipGrepperViewSettings.StoreViewSettings(FIniFile, _s);
end;

procedure TRipGrepperSettings.StoreHistoryEntries(const _list : TStrings; const _section : string);
begin
	for var i := _list.Count - 1 downto 0 do begin
		FIniFile.WriteString(_section, 'Item_' + i.ToString, _list[i]);
	end;
end;

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

procedure TRipGrepParameterSettings.AddArgs(const _args : TArray<string>; const _bQuote : Boolean = False);
begin
	for var s : string in _args do begin
		if not s.IsEmpty then begin
			if _bQuote then begin
				FRipGrepArguments.Add(TProcessUtils.MaybeQuoteIfNotQuoted(s));
			end else begin
				FRipGrepArguments.Add(s);
			end;
		end;
	end;
end;

function TRipGrepParameterSettings.BuildCmdLine : string;
var
	cmdLine : TStringList;
begin
	cmdLine := TStringList.Create();
	try
		cmdLine.Add(RipGrepPath);
		cmdLine.AddStrings(RipGrepArguments);
		cmdLine.Delimiter := ' ';
		Result := cmdLine.DelimitedText;
	finally
		cmdLine.Free;
	end;
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
	RipGrepPath := FIniFile.ReadString('RipGrepSettings', 'Path', '');
	FIsLoaded := True;
end;

function TRipGrepParameterSettings.ReBuildArguments : TStrings;
var
	params : string;
begin
	FRipGrepArguments.Clear();
	params := Options;
	for var s in RG_NECESSARY_PARAMS do begin
		if not params.Contains(s) then begin
			params := s + ' ' + params;
		end;
	end;

	AddArgs(params.Split([' ']));
	FRipGrepArguments.Add(SearchText);
	AddArgs(searchPath.Split([',', ';']), True);
	FRipGrepArguments.Delimiter := ' '; // sArgs.QuoteChar := '"';
	Result := FRipGrepArguments;
end;

procedure TRipGrepParameterSettings.Store;
begin
	if IsLoaded then begin
		FIniFile.WriteString('RipGrepSettings', 'Path', RipGrepPath);
	end;
end;

constructor TRipGrepperViewSettings.Create(const _ini : TIniFile);
begin
	inherited;

end;

procedure TRipGrepperViewSettings.Init;
begin
	ShowRelativePath := False;
	ShowFileIcon := False;
	AlternateRowColors := False;
	IndentLines := False;
end;

procedure TRipGrepperViewSettings.Load;
begin

	ShowRelativePath := FIniFile.ReadBool('RipGrepperSettings', 'ShowRelativePath', False);
	ShowFileIcon := FIniFile.ReadBool('RipGrepperSettings', 'ShowFileIcon', False);
	AlternateRowColors := FIniFile.ReadBool('RipGrepperSettings', 'AlternateRowColors', False);
	IndentLines := FIniFile.ReadBool('RipGrepperSettings', 'IndentLines', False);

	FIsLoaded := True;
end;

procedure TRipGrepperViewSettings.Store;
begin
	if IsLoaded then begin
		StoreViewSettings(FIniFile, '');
	end;
end;

procedure TRipGrepperViewSettings.StoreViewSettings(_ini : TIniFile; const _s : string = '');
var
	i : integer;
begin
	i := 0;
	if _s.IsEmpty then begin
		// store all
		for i := 0 to high(VIEW_SETTINGS) do begin
			StoreViewSettings(_ini, VIEW_SETTINGS[i]);
		end;
	end else if MatchStr(_s, VIEW_SETTINGS[i]) then begin
		_ini.WriteBool('RipGrepperSettings', VIEW_SETTINGS[i], ShowRelativePath);
		TDebugUtils.DebugMessage(VIEW_SETTINGS[i] + ' stored');
	end else if MatchStr(_s, VIEW_SETTINGS[PreInc(i)]) then begin
		_ini.WriteBool('RipGrepperSettings', VIEW_SETTINGS[i], ShowFileIcon);
		TDebugUtils.DebugMessage(VIEW_SETTINGS[i] + ' stored');
	end else if MatchStr(_s, VIEW_SETTINGS[PreInc(i)]) then begin
		_ini.WriteBool('RipGrepperSettings', VIEW_SETTINGS[i], AlternateRowColors);
		TDebugUtils.DebugMessage(VIEW_SETTINGS[i] + ' stored');
	end else if MatchStr(_s, VIEW_SETTINGS[PreInc(i)]) then begin
		_ini.WriteBool('RipGrepperSettings', VIEW_SETTINGS[i], IndentLines);
		TDebugUtils.DebugMessage(VIEW_SETTINGS[i] + ' stored');
	end else begin
		raise Exception.Create('Settings: ' + _s + ' not stored!');
	end;
end;

constructor TRipGrepperSettingsInstance.Create();
begin
	inherited;
end;

class destructor TRipGrepperSettingsInstance.Destroy;
begin
	//
end;

procedure TRipGrepperSettingsInstance.FreeInstance;
begin
	FInstance.Free;
end;

class function TRipGrepperSettingsInstance.GetInstance : TRipGrepperSettings;
begin
	if FInstance = nil then begin
		FInstance := TRipGrepperSettings.Create;
	end;
	Result := FInstance;
end;

constructor TRipGrepperOpenWithSettings.Create(const _ini : TIniFile);
begin
	inherited;
	FCommandList := TStringList.Create;
end;

destructor TRipGrepperOpenWithSettings.Destroy;
begin
	FCommandList.Free;
	inherited;
end;

function TRipGrepperOpenWithSettings.GetCommand(Index : Integer) : string;
begin
	Result := '';
	if FCommandList.Count > 0 then
		Result := FCommandList[index];
end;

procedure TRipGrepperOpenWithSettings.Load;
var
	s : string;
begin
	for var i : integer := 0 to MAX_COMMAND_NUM do begin
		s := FIniFile.ReadString(OPEN_WITH_SETTINGS, OPENWITH_COMMAND_KEY + i.ToString, '');
		if (not s.IsEmpty) then begin
			Command[i] := s;
		end else begin
			break
		end;
	end;

	FIsLoaded := True;
end;

procedure TRipGrepperOpenWithSettings.SetCommand(Index : Integer; const Value : string);
begin
	if FCommandList.Count > Index then begin
		FCommandList[index] := Value;
	end else begin
		FCommandList.Add(Value);
	end;
end;

procedure TRipGrepperOpenWithSettings.Store;
var
	s : string;
begin
	if IsLoaded then begin
		if not FCommandList.Count > 0 then begin
			for var i : integer := 0 to MAX_COMMAND_NUM do begin
				s := Command[i];
				FIniFile.WriteString(OPEN_WITH_SETTINGS, OPENWITH_COMMAND_KEY + i.ToString, s);
			end;
		end;
	end;
end;

constructor TRipGrepperSettingsBase.Create(const _ini : TIniFile);
begin
	inherited Create();
	FIniFile := _ini;
end;

function TRipGrepperSettingsBase.GetIniFile : TIniFile;
begin
	Result := FIniFile;
end;

procedure TRipGrepperSettingsBase.SetIniFile(const Value : TIniFile);
begin
	FIniFile := Value;
end;

end.
