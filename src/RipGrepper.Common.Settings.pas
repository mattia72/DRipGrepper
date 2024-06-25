unit RipGrepper.Common.Settings;

interface

uses
	System.Classes,
	System.IniFiles,
	System.Generics.Collections,
	System.Generics.Defaults,
	RipGrepper.OpenWith.SimpleTypes,
	RipGrepper.Common.Constants,
	RipGrepper.Common.Settings.Base,
	ArrayEx,
	RipGrepper.Common.Settings.RipGrepParameterSettings;

type

	TRipGrepperViewSettings = class(TRipGrepperSettingsBase)
		const
			VIEW_SETTINGS : array [0 .. 4] of string = (
				{ } 'ShowRelativePath',
				{ } 'ShowFileIcon',
				{ } 'AlternateRowColors',
				{ } 'IndentLines',
				{ } 'ExpandNodes');

		const
			INI_SECTION = 'RipGrepperViewSettings';

		public
			AlternateRowColors : Boolean;
			IndentLines : Boolean;
			ShowFileIcon : Boolean;
			ShowRelativePath : Boolean;
			ExpandNodes : Boolean;
			procedure StoreViewSettings(const _s : string = '');
			constructor Create(const _ini : TIniFile);
			function GetIniSectionName : string; override;
			procedure Init; override;
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
			function GetIniSectionName : string; override;
			procedure Load; override;
			procedure Store; override;
			property Command[index : Integer] : string read GetCommand write SetCommand;
			property TestFile : TOpenWithParams read FTestFile write FTestFile;
	end;

	ERipGrepperExtensionContext = (
		{ } rgecActiveFile = EXT_SEARCH_ACTIVE_FILE,
		{ } rgecProjectFiles = EXT_SEARCH_PROJECT_FILES,
		{ } rgecPath = EXT_SEARCH_GIVEN_PATH,
		{ } rgecOpenedProjectFiles);

	TRipGrepperExtensionContext = record
		Context : ERipGrepperExtensionContext;
		ActiveFile : string;
		OpenedProjectFiles : TArray<string>;
		ProjectFiles : TArray<string>;
		ActiveProject : string;

		public
			function ToString : string;
	end;

	TRipGrepperExtensionSettings = class(TRipGrepperSettingsBase)
		const
			INI_SECTION = 'DelphiExtensionSettings';

		private
			FDripGrepperShortCut : string;
			FCurrentSearchSettings : TRipGrepperExtensionContext;

		public
			constructor Create(const _ini : TIniFile);
			function GetIniSectionName : string; override;
			procedure Load; override;
			procedure Store; override;
			function ToString : string; override;
			property DripGrepperShortCut : string read FDripGrepperShortCut write FDripGrepperShortCut;
			property CurrentSearchSettings : TRipGrepperExtensionContext read FCurrentSearchSettings write FCurrentSearchSettings;
	end;

	TRipGrepperAppSettings = class(TRipGrepperSettingsBase)
		const
			INI_SECTION = 'RipGrepperSettings';

		private
			FDebugTrace : Boolean;
			FExpertMode : Boolean;

		protected
			procedure Init; override;

		public
			constructor Create(const _ini : TIniFile);
			function GetIniSectionName : string; override;
			procedure Load; override;
			procedure Store; override;
			property DebugTrace : Boolean read FDebugTrace write FDebugTrace;
			property ExpertMode : Boolean read FExpertMode write FExpertMode;
	end;

	TRipGrepperSearchFormSettings = class(TRipGrepperSettingsBase)
		const
			SEARCH_SETTINGS : array [0 .. 3] of string = ('Pretty', 'Hidden', 'NoIgnore', 'Context');

		const
			INI_SECTION = 'RipGrepperSearchSettings';

		public
			Hidden : Boolean;
			NoIgnore : Boolean;
			Pretty : Boolean;
			Context : integer;
			procedure StoreSearchSettings(const _s : string = '');
			constructor Create(const _ini : TIniFile);
			function GetIniSectionName : string; override;
			procedure Init; override;
			procedure Load; override;
			procedure Store; override;
	end;

	TRipGrepperSettings = class(TRipGrepperSettingsBase)
		private
		var
			FActualSearchPath : string;
			FRipGrepParameters : TRipGrepParameterSettings;
			FRipGrepperViewSettings : TRipGrepperViewSettings;
			FRipGrepperOpenWithSettings : TRipGrepperOpenWithSettings;
			FRipGrepOptionsHistory : TSTrings;
			FSearchPathsHistory : TStrings;
			FSearchTextsHistory : TStrings;
			FRipGrepArguments : TRipGrepArguments;
			FSearchPathIsDir : Boolean;
			FExtensionSettings : TRipGrepperExtensionSettings;
			FFileMasksHistory : TStrings;
			FRipGrepperSettings : TRipGrepperAppSettings;
			FSearchFormSettings : TRipGrepperSearchFormSettings;

		class var
			function GetActualRipGrepParams : string;
			function GetActualSearchPath : string;
			function GetActualSearchText : string;
			function GetIsEmpty : Boolean;
			function GetSearchPathIsDir : Boolean;
			procedure InitSettings;
			procedure LoadHistoryEntries(var _list : TStrings; const _section : string);
			procedure SetFileMasksHistory(const Value : TStrings);
			procedure SetRipGrepOptionsHistory(const Value : TSTrings);
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
			function GetIniSectionName : string; override;
			function GetIsModified : Boolean; override;
			function GetRipGrepArguments : TRipGrepArguments;
			procedure RebuildArguments;
			property ActualRipGrepParams : string read GetActualRipGrepParams;
			property ActualSearchPath : string read GetActualSearchPath;
			property ActualSearchText : string read GetActualSearchText;
			property ExtensionSettings : TRipGrepperExtensionSettings read FExtensionSettings write FExtensionSettings;
			property FileMasksHistory : TStrings read FFileMasksHistory write SetFileMasksHistory;
			property IsEmpty : Boolean read GetIsEmpty;

			property RipGrepParameters : TRipGrepParameterSettings read FRipGrepParameters write FRipGrepParameters;
			property SearchPathsHistory : TStrings read FSearchPathsHistory write SetSearchPathsHistory;
			property RipGrepOptionsHistory : TSTrings read FRipGrepOptionsHistory write SetRipGrepOptionsHistory;
			property RipGrepperOpenWithSettings : TRipGrepperOpenWithSettings read FRipGrepperOpenWithSettings;
			property RipGrepperSettings : TRipGrepperAppSettings read FRipGrepperSettings write FRipGrepperSettings;
			property RipGrepperViewSettings : TRipGrepperViewSettings read FRipGrepperViewSettings write FRipGrepperViewSettings;
			property SearchPathIsDir : Boolean read GetSearchPathIsDir;
			property SearchTextsHistory : TStrings read FSearchTextsHistory write SetSearchTextsHistory;
			property SearchFormSettings : TRipGrepperSearchFormSettings read FSearchFormSettings write FSearchFormSettings;

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
	RipGrepper.Tools.DebugUtils,
	RipGrepper.Tools.FileUtils,
	Vcl.Dialogs,
	System.IOUtils,
	Winapi.Windows,
	System.UITypes,
	RipGrepper.Tools.ProcessUtils,
	RipGrepper.Helper.UI,
	Vcl.Menus,
	System.RegularExpressions,
	RipGrepper.CommandLine.Builder,
	RipGrepper.Common.IOTAUtils;

function TRipGrepperSettings.GetActualRipGrepParams : string;
begin
	RipGrepOptionsHistory.TryGetDef(0, Result);
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

	if RipGrepOptionsHistory.Count = 0 then begin
		RipGrepOptionsHistory.Add('');
	end;

	if FileMasksHistory.Count = 0 then begin
		FileMasksHistory.Add('');
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
	FRipGrepOptionsHistory.Free;
	FSearchTextsHistory.Free;
	FSearchPathsHistory.Free;
	FRipGrepperViewSettings.Free;
	FRipGrepperOpenWithSettings.Free;
	FRipGrepParameters.Free;
	FExtensionSettings.Free;
	FRipGrepperSettings.Free;
	FFileMasksHistory.Free;
	FSearchFormSettings.Free;
	FIniFile.Free;
	inherited;
end;

constructor TRipGrepperSettings.Create;
begin
	if IOTAUTils.IsStandAlone then begin
		FIniFile := TIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'));
	end else begin
		FIniFile := TIniFile.Create(TPath.Combine(IOTAUTils.GetSettingFilePath, EXTENSION_NAME + '.ini'));
	end;
	FRipGrepParameters := TRipGrepParameterSettings.Create(FIniFile);
	FRipGrepperViewSettings := TRipGrepperViewSettings.Create(FIniFile);
	FRipGrepperOpenWithSettings := TRipGrepperOpenWithSettings.Create(FIniFile);
	FExtensionSettings := TRipGrepperExtensionSettings.Create(FIniFile);
	FSearchPathsHistory := TStringList.Create(dupIgnore, False, True);
	FSearchTextsHistory := TStringList.Create(dupIgnore, False, True);
	FRipGrepOptionsHistory := TStringList.Create(dupIgnore, False, True);
	FRipGrepArguments := TStringList.Create();
	FRipGrepArguments.Delimiter := ' ';
	FRipGrepperSettings := TRipGrepperAppSettings.Create(FIniFile);
	FFileMasksHistory := TStringList.Create(dupIgnore, False, True);
	FSearchFormSettings := TRipGrepperSearchFormSettings.Create(FIniFile);
	FIsLoaded := False;
end;

procedure TRipGrepperSettings.AddIfNotContains(_to, _from : TStrings);
begin
	FIsModified := TItemInserter.AddToSringListIfNotContains(_to, _from);
end;

function TRipGrepperSettings.GetIniSectionName : string;
begin
	Result := 'DummySection';
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
	TDebugUtils.DebugMessage('TRipGrepperSettings.Load: start');

	try
		FRipGrepParameters.Load;
		FRipGrepperViewSettings.Load;
		FRipGrepperOpenWithSettings.Load;
		FExtensionSettings.Load;
		FRipGrepperSettings.Load;
		FSearchFormSettings.Load;

		LoadHistoryEntries(FSearchPathsHistory, 'SearchPathsHistory');
		LoadHistoryEntries(FSearchTextsHistory, 'SearchTextsHistory');
		LoadHistoryEntries(FRipGrepOptionsHistory, 'RipGrepOptionsHistory');
		LoadHistoryEntries(FFileMasksHistory, 'FileMasksHistory');
	except
		on E : Exception do begin
			TDebugUtils.DebugMessage(Format('TRipGrepperSettings.Load: Exception %s ', [E.Message]));
			MessageDlg(E.Message + CRLF + 'Settings load from ' + FIniFile.FileName + ' went wrong.', TMsgDlgType.mtError, [mbOk], 0);
		end;
	end;
	InitSettings;
	FIsLoaded := True;
end;

procedure TRipGrepperSettings.RebuildArguments;
begin
	TDebugUtils.DebugMessage('TRipGrepperSettings.RebuildArguments: GuiSetSearchParams start ' +
		FRipGrepParameters.GuiSetSearchParams.ToString);

	TCommandLineBuilder.RebuildArguments(FRipGrepParameters);

	TDebugUtils.DebugMessage('TRipGrepperSettings.RebuildArguments: GuiSetSearchParams end ' +
		FRipGrepParameters.GuiSetSearchParams.ToString);
end;

procedure TRipGrepperSettings.SetFileMasksHistory(const Value : TStrings);
begin
	AddIfNotContains(FFileMasksHistory, Value);
end;

procedure TRipGrepperSettings.SetRipGrepOptionsHistory(const Value : TSTrings);
begin
	AddIfNotContains(FRipGrepOptionsHistory, Value);
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
		FRipGrepperSettings.Store;

		if (FRipGrepParameters.IsModified) then begin
			FRipGrepParameters.Store;
			FSearchFormSettings.Store;
			StoreHistoryEntries(SearchPathsHistory, 'SearchPathsHistory');
			StoreHistoryEntries(SearchTextsHistory, 'SearchTextsHistory');
			StoreHistoryEntries(RipGrepOptionsHistory, 'RipGrepOptionsHistory');
			StoreHistoryEntries(FileMasksHistory, 'FileMasksHistory');
		end;
	end;
end;

procedure TRipGrepperSettings.StoreViewSettings(const _s : string = '');
begin
	FRipGrepperViewSettings.StoreViewSettings(_s);
	FRipGrepperViewSettings.Store;
end;

procedure TRipGrepperSettings.StoreHistoryEntries(const _list : TStrings; const _section : string);
var
	multiLineVal : TMultiLineString;
begin
	for var i := _list.Count - 1 downto 0 do begin
		if not _list[i].IsEmpty then begin
			multiLineVal := _list[i];
			FIniFile.WriteString(_section, 'Item_' + i.ToString, multiLineVal.GetLine(0));
		end;
	end;
end;

constructor TRipGrepperViewSettings.Create(const _ini : TIniFile);
begin
	inherited;
	TDebugUtils.DebugMessage('TRipGrepperViewSettings.Create: ' + FIniFile.FileName + '[' + GetIniSectionName + ']');
end;

function TRipGrepperViewSettings.GetIniSectionName : string;
begin
	Result := INI_SECTION;
end;

procedure TRipGrepperViewSettings.Init;
begin
	inherited;
	CreateSetting('ShowRelativePath', TRipGrepperSetting.New(vtBoolean, False));
	CreateSetting('ShowFileIcon', TRipGrepperSetting.New(vtBoolean, False));
	CreateSetting('AlternateRowColors', TRipGrepperSetting.New(vtBoolean, False));
	CreateSetting('IndentLines', TRipGrepperSetting.New(vtBoolean, False));
	CreateSetting('ExpandNodes', TRipGrepperSetting.New(vtBoolean, True));
end;

procedure TRipGrepperViewSettings.Load;
begin
	inherited Load();
	ShowRelativePath := LoadSetting('ShowRelativePath');
	ShowFileIcon := LoadSetting('ShowFileIcon');
	AlternateRowColors := LoadSetting('AlternateRowColors');
	IndentLines := LoadSetting('IndentLines');
	ExpandNodes := LoadSetting('ExpandNodes');

	FIsLoaded := True;
end;

procedure TRipGrepperViewSettings.Store;
begin
	if IsLoaded and IsModified then begin
		StoreViewSettings('');
		inherited Store();
		FIsModified := False;
	end;

end;

procedure TRipGrepperViewSettings.StoreViewSettings(const _s : string = '');
var
	i : integer;
begin
	TDebugUtils.DebugMessage('TRipGrepperSearchDialogForm.StoreViewSettings: ' + _s);

	i := 0;
	if _s.IsEmpty then begin
		// store all
		for i := 0 to high(VIEW_SETTINGS) do begin
			StoreViewSettings(VIEW_SETTINGS[i]);
		end;
	end else if MatchStr(_s, VIEW_SETTINGS[i]) then begin
		StoreSetting(VIEW_SETTINGS[i], ShowRelativePath);
	end else if MatchStr(_s, VIEW_SETTINGS[PreInc(i)]) then begin
		StoreSetting(VIEW_SETTINGS[i], ShowFileIcon);
	end else if MatchStr(_s, VIEW_SETTINGS[PreInc(i)]) then begin
		StoreSetting(VIEW_SETTINGS[i], AlternateRowColors);
	end else if MatchStr(_s, VIEW_SETTINGS[PreInc(i)]) then begin
		StoreSetting(VIEW_SETTINGS[i], IndentLines);
	end else if MatchStr(_s, VIEW_SETTINGS[PreInc(i)]) then begin
		StoreSetting(VIEW_SETTINGS[i], ExpandNodes);
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
	TDebugUtils.DebugMessage('TRipGrepperOpenWithSettings.Create: ' + FIniFile.FileName + '[' + GetIniSectionName + ']');
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

function TRipGrepperOpenWithSettings.GetIniSectionName : string;
begin
	Result := OPEN_WITH_SETTINGS;
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

constructor TRipGrepperExtensionSettings.Create(const _ini : TIniFile);
begin
	inherited;
	TDebugUtils.DebugMessage('TRipGrepperExtensionSettings.Create: ' + FIniFile.FileName + '[' + GetIniSectionName + ']');
end;

function TRipGrepperExtensionSettings.GetIniSectionName : string;
begin
	Result := INI_SECTION;
end;

procedure TRipGrepperExtensionSettings.Load;
begin
	if IOTAUTils.IsStandAlone then begin
		Exit;
	end;
	if Assigned(FIniFile) then begin
		var
		css := CurrentSearchSettings;
		css.Context := ERipGrepperExtensionContext(FIniFile.ReadInteger(INI_SECTION, 'DripGrepperContext', EXT_SEARCH_GIVEN_PATH));
		CurrentSearchSettings := css;

		DripGrepperShortCut := FIniFile.ReadString(INI_SECTION, 'DripGrepperShortCut', '');
		if DripGrepperShortCut = '' then begin
			DripGrepperShortCut := ShortCutToText(ShortCut(Word('R'), [ssShift, ssAlt]));
		end;
		FIsModified := True;
		FIsLoaded := True;
		TDebugUtils.DebugMessage('TRipGrepperExtensionSettings.Load ' + ToString());

	end else begin
		raise Exception.Create('Settings ini file is nil!')
	end;
end;

procedure TRipGrepperExtensionSettings.Store;
begin
	if IOTAUTils.IsStandAlone then begin
		Exit;
	end;
	var
	bStore := IsLoaded and IsModified;
	TDebugUtils.DebugMessage('TRipGrepperExtensionSettings.Store ' + BoolToStr(bStore) + ' ' + ToString());
	if bStore then begin
		FIniFile.WriteString(INI_SECTION, 'DripGrepperShortCut', DripGrepperShortCut);
		FIniFile.WriteInteger(INI_SECTION, 'DripGrepperContext', Integer(CurrentSearchSettings.Context));
		FIsModified := False;
	end;

end;

function TRipGrepperExtensionSettings.ToString : string;
begin
	Result := Format('ShortCut: %s, CurrentSearchSettings: %s', [DripGrepperShortCut, CurrentSearchSettings.ToString]);
end;

constructor TRipGrepperAppSettings.Create(const _ini : TIniFile);
begin
	inherited;
	TDebugUtils.DebugMessage('TRipGrepperAppSettings.Create: ' + FIniFile.FileName + '[' + GetIniSectionName + ']');
end;

function TRipGrepperAppSettings.GetIniSectionName : string;
begin
	Result := INI_SECTION;
end;

procedure TRipGrepperAppSettings.Init;
begin
	inherited;
	CreateSetting('DebugTrace', TRipGrepperSetting.New(vtBoolean, False));
	CreateSetting('ExpertMode', TRipGrepperSetting.New(vtBoolean, False));
end;

procedure TRipGrepperAppSettings.Load;
begin
	inherited Load();
	TDebugUtils.DebugMessage('TRipGrepperAppSettings.Load start');

	FExpertMode := LoadSetting('ExpertMode');
	FDebugTrace := LoadSetting('DebugTrace');
end;

procedure TRipGrepperAppSettings.Store;
begin
	StoreSetting('ExpertMode', FExpertMode);
	StoreSetting('DebugTrace', FDebugTrace);
	inherited Store();
end;

constructor TRipGrepperSearchFormSettings.Create(const _ini : TIniFile);
begin
	inherited;
	TDebugUtils.DebugMessage('TRipGrepperSearchFormSettings.Create: ' + FIniFile.FileName + '[' + GetIniSectionName + ']');
end;

function TRipGrepperSearchFormSettings.GetIniSectionName : string;
begin
	Result := INI_SECTION;
end;

procedure TRipGrepperSearchFormSettings.Init;
begin
	inherited;
	CreateSetting('Pretty', TRipGrepperSetting.New(vtBoolean, True));
	CreateSetting('Hidden', TRipGrepperSetting.New(vtBoolean, False));
	CreateSetting('NoIgnore', TRipGrepperSetting.New(vtBoolean, False));
	CreateSetting('Context', TRipGrepperSetting.New(vtInteger, 0));
end;

procedure TRipGrepperSearchFormSettings.Load;
begin
	inherited Load();
	Pretty := LoadSetting('Pretty');
	Hidden := LoadSetting('Hidden');
	NoIgnore := LoadSetting('NoIgnore');
	Context := LoadSetting('Context');
	FIsLoaded := True;
end;

procedure TRipGrepperSearchFormSettings.Store;
begin
	if IsLoaded and IsModified then begin
		StoreSearchSettings('');
		inherited Store();
		FIsModified := False;
	end;

end;

procedure TRipGrepperSearchFormSettings.StoreSearchSettings(const _s : string = '');
var
	i : integer;
begin
	TDebugUtils.DebugMessage('TRipGrepperSearchDialogForm.StoreSearchSettings: ' + _s);

	i := 0;
	if _s.IsEmpty then begin
		// store all
		for i := 0 to high(SEARCH_SETTINGS) do begin
			StoreSearchSettings(SEARCH_SETTINGS[i]);
		end;
	end else if MatchStr(_s, SEARCH_SETTINGS[PreInc(i)]) then begin
		StoreSetting(SEARCH_SETTINGS[i], Pretty);
	end else if MatchStr(_s, SEARCH_SETTINGS[PreInc(i)]) then begin
		StoreSetting(SEARCH_SETTINGS[i], Hidden);
	end else if MatchStr(_s, SEARCH_SETTINGS[PreInc(i)]) then begin
		StoreSetting(SEARCH_SETTINGS[i], NoIgnore);
	end else if MatchStr(_s, SEARCH_SETTINGS[PreInc(i)]) then begin
		StoreSetting(SEARCH_SETTINGS[i], Context);
	end else begin
		raise Exception.Create('Settings: ' + _s + ' not stored!');
	end;
	IsModified := True;
end;

function TRipGrepperExtensionContext.ToString : string;
begin
	Result := Format('Context: %d, ActiveProject: %s, ActiveFile: %s', [Integer(Context), ActiveProject, ActiveFile]);
end;

initialization

GSettings := TRipGrepperSettings.Create;
GSettings.Load;

finalization

GSettings.Store;
GSettings.Free;

end.
