unit RipGrepper.Common.Settings;

interface

uses
	System.Classes,
	System.IniFiles,
	System.Generics.Collections,
	System.Generics.Defaults,
	RipGrepper.OpenWith.SimpleTypes,
	RipGrepper.Common.Types;

type
	ISettingsPersister = interface
		['{A841C46D-56AF-4391-AB88-4C9496589FF4}']
		procedure Load;
		procedure Store;
	end;

	TRipGrepperSettingsBase = class(TSingletonImplementation, ISettingsPersister)
		private
			FIsModified : Boolean;
			function GetIniFile : TIniFile;
			procedure SetIniFile(const Value : TIniFile);
			procedure SetIsModified(const Value : Boolean);

		protected
			FIniFile : TIniFile;
			FIsLoaded : Boolean;
			function GetIsLoaded : Boolean; virtual;
			function GetIsModified : Boolean; virtual;

		public
			constructor Create(const _ini : TIniFile);
			procedure Load; virtual; abstract;
			procedure Store; virtual; abstract;
			property IniFile : TIniFile read GetIniFile write SetIniFile;
			property IsLoaded : Boolean read GetIsLoaded;
			property IsModified : Boolean read GetIsModified write SetIsModified;
	end;

	TRipGrepParameterSettings = class(TRipGrepperSettingsBase)
		private
			FRipGrepArguments : TRipGrepArguments;
			FOptions : string;
			FRipGrepPath : string;
			FSearchPath : string;
			FSearchText : string;
			procedure AddArgs(const _sName : string; const _args : TArray<string>; const _bQuote : Boolean = False);
			procedure SetOptions(const Value : string);
			procedure SetSearchPath(const Value : string);
			procedure SetSearchText(const Value : string);

		public
			constructor Create(const _ini : TIniFile);
			destructor Destroy; override;
			function BuildCmdLine : string;
			procedure InitRipGrepExePath;
			procedure Load; override;
			function ReBuildArguments : TStrings;
			procedure Store; override;
			property Options : string read FOptions write SetOptions;
			property SearchPath : string read FSearchPath write SetSearchPath;
			property SearchText : string read FSearchText write SetSearchText;
			property RipGrepArguments : TRipGrepArguments read FRipGrepArguments write FRipGrepArguments;
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
			FTestFile : TOpenWithParams;
			function GetCommand(Index : Integer) : string;
			procedure SetCommand(Index : Integer; const Value : string);

		public
			constructor Create(const _ini : TIniFile);
			destructor Destroy; override;
			procedure Load; override;
			procedure Store; override;
			property Command[index : Integer] : string read GetCommand write SetCommand;
			property TestFile : TOpenWithParams read FTestFile write FTestFile;
	end;

	TRipGrepperExtensionSettings = class(TRipGrepperSettingsBase)
		const
			INI_SECTION = 'DelphiExtensionSettings';

		private
			FDripGrepperShortCut : string;

		public
			procedure Load; override;
			procedure Store; override;
			property DripGrepperShortCut : string read FDripGrepperShortCut write FDripGrepperShortCut;
	end;

	TRipGrepperSettings = class(TRipGrepperSettingsBase)
		private
		var
			FActualSearchPath : string;
			FRipGrepParameters : TRipGrepParameterSettings;
			FRipGrepperViewSettings : TRipGrepperViewSettings;
			FRipGrepperOpenWithSettings : TRipGrepperOpenWithSettings;
			FRipGrepParamsHistory : TSTrings;
			FSearchPathsHistory : TStrings;
			FSearchTextsHistory : TStrings;
			FRipGrepArguments : TRipGrepArguments;
			FSearchPathIsDir : Boolean;
			FExtensionSettings : TRipGrepperExtensionSettings;
		class var
			function GetActualRipGrepParam : string;
			function GetActualSearchPath : string;
			function GetActualSearchText : string;
			function GetIsEmpty : Boolean;
			function GetSearchPathIsDir : Boolean;
			procedure InitSettings;
			procedure LoadHistoryEntries(var _list : TStrings; const _section : string);
			procedure SetRipGrepParamsHistory(const Value : TSTrings);
			procedure SetSearchPathsHistory(const Value : TStrings);
			procedure SetSearchTextsHistory(const Value : TStrings);
			procedure StoreHistoryEntries(const _list : TStrings; const _section : string);

		public
			procedure Load; override;
			procedure Store; override;
			procedure StoreViewSettings(const _s : string = '');
			constructor Create;
			destructor Destroy; override;
			procedure AddIfNotContains(_to, _from : TStrings);
			function GetIsModified : Boolean; override;
			function GetRipGrepArguments : TRipGrepArguments;
			function ReBuildArguments : TStrings;
			property ActualRipGrepParam : string read GetActualRipGrepParam;
			property ActualSearchPath : string read GetActualSearchPath;
			property ActualSearchText : string read GetActualSearchText;
			property ExtensionSettings : TRipGrepperExtensionSettings read FExtensionSettings write FExtensionSettings;
			property IsEmpty : Boolean read GetIsEmpty;

			property RipGrepParameters : TRipGrepParameterSettings read FRipGrepParameters write FRipGrepParameters;
			property SearchPathsHistory : TStrings read FSearchPathsHistory write SetSearchPathsHistory;
			property RipGrepParamsHistory : TSTrings read FRipGrepParamsHistory write SetRipGrepParamsHistory;
			property RipGrepperOpenWithSettings : TRipGrepperOpenWithSettings read FRipGrepperOpenWithSettings;
			property RipGrepperViewSettings : TRipGrepperViewSettings read FRipGrepperViewSettings write FRipGrepperViewSettings;
			property SearchPathIsDir : Boolean read GetSearchPathIsDir;
			property SearchTextsHistory : TStrings read FSearchTextsHistory write SetSearchTextsHistory;
	end;

	TRipGrepperSettingsInstance = class
		private
			class var FInstance : TRipGrepperSettings;
			class function GetInstance : TRipGrepperSettings; static;

		public
			constructor Create;
			class destructor Destroy;
			class procedure FreeInstance; reintroduce;
			class property Instance : TRipGrepperSettings read GetInstance;
	end;

var
	GSettings : TRipGrepperSettings;

implementation

uses
	System.SysUtils,
	Vcl.Forms,
	System.StrUtils,
	RipGrepper.Helper.Types,
	RipGrepper.Tools.DebugTools,
	RipGrepper.Tools.FileUtils,
	Vcl.Dialogs,
	System.IOUtils,
	Winapi.Windows,
	System.UITypes,
	{$IFNDEF STANDALONE}
	DripExtension.IOTA.Utils,
	{$ENDIF}
	RipGrepper.Tools.ProcessUtils,
	RipGrepper.Helper.UI,
	Vcl.Menus;

function TRipGrepperSettings.GetActualRipGrepParam : string;
begin
	RipGrepParamsHistory.TryGetDef(0, Result);
end;

function TRipGrepperSettings.GetActualSearchPath : string;
var
	s : string;
begin
	if SearchPathsHistory.TryGetDef(0, s) and (s <> FActualSearchPath) then begin
		FActualSearchPath := s;
		FSearchPathIsDir := TDirectory.Exists(FActualSearchPath);
	end;
	Result := FActualSearchPath;
end;

function TRipGrepperSettings.GetActualSearchText : string;
begin
	SearchTextsHistory.TryGetDef(0, Result);
end;

function TRipGrepperSettings.GetIsEmpty : Boolean;
begin
	Result := FRipGrepParameters.RipGrepPath.IsEmpty;
end;

function TRipGrepperSettings.GetRipGrepArguments : TRipGrepArguments;
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
		if -1 = _list.IndexOf(s) then begin
			_list.Add(s);
		end;
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
	FExtensionSettings.Free;
	FIniFile.Free;
	inherited;
end;

constructor TRipGrepperSettings.Create;
begin
	{$IFDEF STANDALONE}
	FIniFile := TIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'));
	{$ELSE}
	FIniFile := TIniFile.Create(TPath.Combine(IOTAUTils.GetSettingFilePath, EXTENSION_NAME + '.ini'));
	{$ENDIF}
	FRipGrepParameters := TRipGrepParameterSettings.Create(FIniFile);
	FRipGrepperViewSettings := TRipGrepperViewSettings.Create(FIniFile);
	FRipGrepperOpenWithSettings := TRipGrepperOpenWithSettings.Create(FIniFile);
	FExtensionSettings := TRipGrepperExtensionSettings.Create(FIniFile);
	FRipGrepperViewSettings.Init();
	FSearchPathsHistory := TStringList.Create(dupIgnore, False, True);
	FSearchTextsHistory := TStringList.Create(dupIgnore, False, True);
	FRipGrepParamsHistory := TStringList.Create(dupIgnore, False, True);
	FRipGrepArguments := TStringList.Create();
	FIsLoaded := False;
end;

procedure TRipGrepperSettings.AddIfNotContains(_to, _from : TStrings);
begin
	FIsModified := TItemInserter.AddToSringListIfNotContains(_to, _from);
end;

function TRipGrepperSettings.GetIsModified : Boolean;
begin
	Result := FIsModified or FRipGrepParameters.IsModified or FRipGrepperViewSettings.IsModified or FRipGrepperOpenWithSettings.IsModified;
end;

function TRipGrepperSettings.GetSearchPathIsDir : Boolean;
begin
	Result := FSearchPathIsDir;
end;

procedure TRipGrepperSettings.Load;
begin
	FRipGrepParameters.Load;
	FRipGrepperViewSettings.Load;
	FRipGrepperOpenWithSettings.Load;
	FExtensionSettings.Load;

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

procedure TRipGrepperSettings.SetRipGrepParamsHistory(const Value : TSTrings);
begin
	AddIfNotContains(FRipGrepParamsHistory, Value);
end;

procedure TRipGrepperSettings.SetSearchPathsHistory(const Value : TStrings);
begin
	AddIfNotContains(FSearchPathsHistory, Value);
end;

procedure TRipGrepperSettings.SetSearchTextsHistory(const Value : TStrings);
begin
	AddIfNotContains(FSearchTextsHistory, Value);
end;

procedure TRipGrepperSettings.Store;
begin
	if IsLoaded and IsModified then begin
		FRipGrepperViewSettings.Store;
		FRipGrepperOpenWithSettings.Store;
		FExtensionSettings.Store;

		if (FRipGrepParameters.IsModified) then begin
			FRipGrepParameters.Store;
			StoreHistoryEntries(SearchPathsHistory, 'SearchPathsHistory');
			StoreHistoryEntries(SearchTextsHistory, 'SearchTextsHistory');
			StoreHistoryEntries(RipGrepParamsHistory, 'RipGrepParamsHistory');
		end;
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

procedure TRipGrepParameterSettings.AddArgs(const _sName : string; const _args : TArray<string>; const _bQuote : Boolean = False);
begin
	for var s : string in _args do begin
		if not s.IsEmpty then begin
			if _bQuote then begin
				FRipGrepArguments.AddPair(_sName, TProcessUtils.MaybeQuoteIfNotQuoted(s));
			end else begin
				FRipGrepArguments.AddPair(_sName, s);
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
	if Assigned(FIniFile) then begin
		RipGrepPath := FIniFile.ReadString('RipGrepSettings', 'Path', '');
		FIsLoaded := True;
	end else begin
		raise Exception.Create('Settings file is nil!')
	end;
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

	AddArgs(RG_ARG_OPTIONS, params.Split([' ']));
	FRipGrepArguments.AddPair(RG_ARG_SEARCH_TEXT, SearchText);
	AddArgs(RG_ARG_SEARCH_PATH, searchPath.Split([',', ';']), True);
	FRipGrepArguments.Delimiter := ' '; // sArgs.QuoteChar := '"';
	Result := FRipGrepArguments;
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
	if FSearchText <> Value then begin
		FSearchText := Value;
		FIsModified := True;
	end;
end;

procedure TRipGrepParameterSettings.Store;
begin
	if IsLoaded and IsModified then begin
		FIniFile.WriteString('RipGrepSettings', 'Path', RipGrepPath);
		FIsModified := False;
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
	if IsLoaded and IsModified then begin
		StoreViewSettings(FIniFile, '');
		FIsModified := False;
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
	IsModified := True;
end;

constructor TRipGrepperSettingsInstance.Create();
begin
	inherited;
	FInstance := nil;
end;

class destructor TRipGrepperSettingsInstance.Destroy;
begin
	//
end;

class procedure TRipGrepperSettingsInstance.FreeInstance;
begin
	FInstance.Free;
end;

class function TRipGrepperSettingsInstance.GetInstance : TRipGrepperSettings;
begin
	if Assigned(FInstance) then begin
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
	if FCommandList.Count > index then begin
		Result := FCommandList[index];
	end;
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
	if Value.IsEmpty then
		Exit;

	if FCommandList.Count > index then begin
		if (FCommandList[index] <> Value) then begin
			FCommandList[index] := Value;
			IsModified := True;
		end;
	end else begin
		FCommandList.Add(Value);
		IsModified := True;
	end;
end;

procedure TRipGrepperOpenWithSettings.Store;
var
	s : string;
begin
	if IsLoaded and IsModified then begin
		if FCommandList.Count > 0 then begin
			for var i : integer := 0 to MAX_COMMAND_NUM do begin
				s := Command[i];
				if s.IsEmpty then
					break;
				FIniFile.WriteString(OPEN_WITH_SETTINGS, OPENWITH_COMMAND_KEY + i.ToString, s);
			end;
		end;
		FIsModified := False;
	end;
end;

constructor TRipGrepperSettingsBase.Create(const _ini : TIniFile);
begin
	inherited Create();
	FIniFile := _ini;
	FIsModified := False;
	FIsLoaded := False;
end;

function TRipGrepperSettingsBase.GetIniFile : TIniFile;
begin
	Result := FIniFile;
end;

function TRipGrepperSettingsBase.GetIsLoaded : Boolean;
begin
	Result := FIsLoaded;
end;

function TRipGrepperSettingsBase.GetIsModified : Boolean;
begin
	Result := FIsModified;
end;

procedure TRipGrepperSettingsBase.SetIniFile(const Value : TIniFile);
begin
	if Assigned(FIniFile) then
		FIniFile.Free;
	FIniFile := Value;
end;

procedure TRipGrepperSettingsBase.SetIsModified(const Value : Boolean);
begin
	FIsModified := Value;
end;

procedure TRipGrepperExtensionSettings.Load;

begin
	{$IFNDEF STANDALONE}
	if Assigned(FIniFile) then begin
		DripGrepperShortCut := FIniFile.ReadString(INI_SECTION, 'DripGrepperShortCut', '');
		if DripGrepperShortCut = '' then begin
			DripGrepperShortCut := ShortCutToText(ShortCut(Word('R'), [ssShift, ssAlt]));
            FIsModified := True;
		end;
		FIsLoaded := True;
	end else begin
		raise Exception.Create('Settings file is nil!')
	end;
	{$ENDIF}
end;

procedure TRipGrepperExtensionSettings.Store;
begin
	{$IFNDEF STANDALONE}
	if IsLoaded and IsModified then begin
		FIniFile.WriteString(INI_SECTION, 'DripGrepperShortCut', DripGrepperShortCut);
		FIsModified := False;
	end;
	{$ENDIF}
end;

initialization

GSettings := TRipGrepperSettings.Create;
GSettings.Load;

finalization

GSettings.Store;
GSettings.Free;

end.
