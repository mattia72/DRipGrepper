unit RipGrepper.Settings.RipGrepperSettings;

interface

uses
	System.Classes,
	System.IniFiles,
	RipGrepper.Common.Constants,
	RipGrepper.Common.SimpleTypes,
	RipGrepper.Settings.AppSettings,
	RipGrepper.Settings.Persistable,
	RipGrepper.Settings.RipGrepParameterSettings,
	RipGrepper.Settings.SearchFormSettings,
	RipGrepper.Settings.NodeLookSettings,
	RipGrepper.Settings.OpenWithSettings,
	RipGrepper.Settings.FontColors;

type
	TRipGrepperSettings = class(TPersistableSettings)

		private
			FSearchFormSettings : TSearchFormSettings;

			FRipGrepParameters : TRipGrepParameterSettings;
			FNodeLookSettings : TNodeLookSettings;
			FOpenWithSettings : TOpenWithSettings;
			FFontColorSettings : TColorSettings;

			FExpertOptionHistory : TSTrings;
			FSearchPathsHistory : TStrings;
			FSearchTextsHistory : TStrings;
			FFileMasksHistory : TStrings;

			FRipGrepArguments : TRipGrepArguments;
			FSearchPathIsDir : Boolean;
			FAppSettings : TAppSettings;

			FActualSearchPath : string;

			FLastSearchText : string;
			FLastReplaceText : string;
			FReplaceTextsHistory : TStrings;

			function GetIsEmpty : Boolean;
			function GetSearchPathIsDir : Boolean;
			procedure LoadHistoryEntries(var _list : TStrings; const _section : string);
			procedure SetFileMasksHistory(const Value : TStrings);
			procedure SetExpertOptionHistory(const Value : TSTrings);
			procedure SetSearchPathsHistory(const Value : TStrings);
			procedure SetSearchTextsHistory(const Value : TStrings);
			procedure StoreHistoryEntries(const _list : TStrings; const _section : string);
			function GetActualSearchPath : string;
			function GetIsReplaceMode : Boolean;
			function GetSearchFormSettings : TSearchFormSettings;
			procedure LoadFirstNecessarySettings;
			procedure SetReplaceTextsHistory(const Value : TStrings);

		protected
			function GetIsAlreadyRead : Boolean; override;

		public
			procedure ReadIni; override;
			procedure StoreToDict; override;
			procedure StoreViewSettings(const _s : string = '');
			constructor Create;
			destructor Destroy; override;
			procedure AddIfNotContains(_to, _from : TStrings);
			procedure Copy(const _other : TPersistableSettings); override;
			procedure CopyDefaultsToValues; override;
			function GetIsModified : Boolean; override;
			function GetLastHistorySearchText : string;
			function GetRipGrepArguments : TRipGrepArguments;
			procedure Init; override;
			procedure LoadDefaultsFromDict; override;
			procedure RebuildArguments;
			procedure LoadFromDict(); override;
			procedure LoadInitialSettings;
			procedure ReCreateMemIni; override;
			procedure ReLoad; override;
			procedure StoreAsDefaultsToDict; override;
			procedure StoreHistories;
			property LastSearchText : string read FLastSearchText write FLastSearchText;
			property FileMasksHistory : TStrings read FFileMasksHistory write SetFileMasksHistory;
			property IsEmpty : Boolean read GetIsEmpty;

			property ActualSearchPath : string read GetActualSearchPath;
			property SearchPathsHistory : TStrings read FSearchPathsHistory write SetSearchPathsHistory;
			property ExpertOptionHistory : TSTrings read FExpertOptionHistory write SetExpertOptionHistory;
			property RipGrepParameters : TRipGrepParameterSettings read FRipGrepParameters write FRipGrepParameters;
			property OpenWithSettings : TOpenWithSettings read FOpenWithSettings;
			property SearchFormSettings : TSearchFormSettings read GetSearchFormSettings write FSearchFormSettings;
			property AppSettings : TAppSettings read FAppSettings write FAppSettings;
			property FontColorSettings : TColorSettings read FFontColorSettings write FFontColorSettings;
			property IsReplaceMode : Boolean read GetIsReplaceMode;
			property LastReplaceText : string read FLastReplaceText write FLastReplaceText;
			property NodeLookSettings : TNodeLookSettings read FNodeLookSettings write FNodeLookSettings;
			property SearchPathIsDir : Boolean read GetSearchPathIsDir;
			property SearchTextsHistory : TStrings read FSearchTextsHistory write SetSearchTextsHistory;
			property ReplaceTextsHistory : TStrings read FReplaceTextsHistory write SetReplaceTextsHistory;
	end;

var
	GSettings : TRipGrepperSettings;

implementation

uses
	RipGrepper.Helper.Types,
	System.IOUtils,
	System.SysUtils,
	System.AnsiStrings,
	Vcl.Forms,
	RipGrepper.Helper.UI,
	RipGrepper.Tools.DebugUtils,
	RipGrepper.CommandLine.Builder,
	Winapi.Windows;

function TRipGrepperSettings.GetLastHistorySearchText : string;
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

procedure TRipGrepperSettings.LoadInitialSettings;
begin
	LoadFirstNecessarySettings;

	if SearchPathsHistory.Count = 0 then begin
		SearchPathsHistory.Add(TDirectory.GetCurrentDirectory());
	end;

	if SearchTextsHistory.Count = 0 then begin
		SearchTextsHistory.Add('search text');
	end;

	if ExpertOptionHistory.Count = 0 then begin
		ExpertOptionHistory.Add('');
	end;

	if FileMasksHistory.Count = 0 then begin
		FileMasksHistory.Add('');
	end;
end;

procedure TRipGrepperSettings.LoadHistoryEntries(var _list : TStrings; const _section : string);
begin
	for var i := 0 to MAX_HISTORY_COUNT do begin
		var
		s := IniFile.ReadString(_section, 'Item_' + i.ToString, '');
		if -1 = _list.IndexOf(s) then begin
			_list.Add(s);
		end;
	end;
end;

destructor TRipGrepperSettings.Destroy;
begin
	// var
	// dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperSettings.Destroy');
	// trace causes exception on closing delphi ide
	FRipGrepArguments.Free;
	FExpertOptionHistory.Free;
	FSearchTextsHistory.Free;
	FReplaceTextsHistory.Free;
	FSearchPathsHistory.Free;

	FFileMasksHistory.Free;
	inherited Destroy(); // ok;
end;

constructor TRipGrepperSettings.Create;
begin
	IniSectionName := ROOT_DUMMY_INI_SECTION;

	inherited Create;

	FAppSettings := TAppSettings.Create(self);
	FFontColorSettings := TColorSettings.Create(self);
	FRipGrepParameters := TRipGrepParameterSettings.Create(self);
	FNodeLookSettings := TNodeLookSettings.Create(self);
	FOpenWithSettings := TOpenWithSettings.Create(self);
	FSearchFormSettings := TSearchFormSettings.Create(self);

	AddChildSettings(FAppSettings);
	AddChildSettings(FFontColorSettings);
	AddChildSettings(FRipGrepParameters);
	AddChildSettings(FNodeLookSettings);
	AddChildSettings(FOpenWithSettings);
	AddChildSettings(FSearchFormSettings);

	FSearchPathsHistory := TStringList.Create(dupIgnore, False, True);
	FSearchTextsHistory := TStringList.Create(dupIgnore, False, True);
	FReplaceTextsHistory := TStringList.Create(dupIgnore, False, True);
	FExpertOptionHistory := TStringList.Create(dupIgnore, False, True);
	FRipGrepArguments := TStringList.Create();
	FRipGrepArguments.Delimiter := ' ';
	FFileMasksHistory := TStringList.Create(dupIgnore, False, True);
end;

procedure TRipGrepperSettings.AddIfNotContains(_to, _from : TStrings);
begin
	FIsModified := TItemInserter.AddToSringListIfNotContains(_to, _from);
end;

procedure TRipGrepperSettings.Copy(const _other : TPersistableSettings);
begin
	if Assigned(_other) then begin
		var
		s := _other as TRipGrepperSettings;

		FAppSettings.Copy(s.AppSettings);
		FFontColorSettings.Copy(s.FontColorSettings);
		FNodeLookSettings.Copy(s.NodeLookSettings);
		FOpenWithSettings.Copy(s.OpenWithSettings);
		FRipGrepParameters.Copy(s.RipGrepParameters);
		FSearchFormSettings.Copy(s.SearchFormSettings);

		FSearchPathsHistory.Assign(s.SearchPathsHistory);
		FSearchTextsHistory.Assign(s.SearchTextsHistory);
		FReplaceTextsHistory.Assign(s.ReplaceTextsHistory);
		FExpertOptionHistory.Assign(s.ExpertOptionHistory);
		FRipGrepArguments.Assign(s.FRipGrepArguments);
		inherited Copy(_other as TPersistableSettings);
	end;
end;

procedure TRipGrepperSettings.CopyDefaultsToValues;
begin
	FSearchFormSettings.CopyDefaultsToValues;
	FRipGrepParameters.CopyDefaultsToValues;
	inherited CopyDefaultsToValues;
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

function TRipGrepperSettings.GetIsAlreadyRead : Boolean;
begin
	Result := inherited;
end;

function TRipGrepperSettings.GetIsModified : Boolean;
begin
	Result := FIsModified or FRipGrepParameters.IsModified or
	{ } FNodeLookSettings.IsModified or
	{ } FOpenWithSettings.IsModified or
	{ } FFontColorSettings.IsModified;
end;

function TRipGrepperSettings.GetIsReplaceMode : Boolean;
begin
	Result := RipGrepParameters.GuiSearchTextParams.IsReplaceMode;
end;

function TRipGrepperSettings.GetSearchFormSettings : TSearchFormSettings;
begin
	if not FSearchFormSettings.IsAlreadyRead then begin
		FSearchFormSettings.ReadIni;
	end;
	Result := FSearchFormSettings;
end;

function TRipGrepperSettings.GetSearchPathIsDir : Boolean;
begin
	Result := FSearchPathIsDir;
end;

procedure TRipGrepperSettings.Init;
begin
	// nothing todo
end;

procedure TRipGrepperSettings.ReadIni; // Composit
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperSettings.ReadIni');
	try
		inherited ReadIni();
	except
		on E : Exception do begin
			TDebugUtils.DebugMessage(Format('TRipGrepperSettings.ReadIni: Exception %s ', [E.Message]));
			TMsgBox.ShowError(E.Message + CRLF + 'Settings Read from ' + IniFile.FileName + ' went wrong.');
		end;
	end;
end;

procedure TRipGrepperSettings.LoadDefaultsFromDict;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperSettings.LoadDefaultsFromDict');

	FRipGrepParameters.LoadDefaultsFromDict;
	FSearchFormSettings.LoadDefaultsFromDict;
end;

procedure TRipGrepperSettings.LoadFirstNecessarySettings;
begin
	FNodeLookSettings.LoadFromDict();

	LoadHistoryEntries(FSearchPathsHistory, 'SearchPathsHistory');
	LoadHistoryEntries(FSearchTextsHistory, 'SearchTextsHistory');
	LoadHistoryEntries(FReplaceTextsHistory, 'ReplaceTextsHistory');
	LoadHistoryEntries(FExpertOptionHistory, 'ExpertOptionHistory');
	LoadHistoryEntries(FFileMasksHistory, 'FileMasksHistory');
end;

procedure TRipGrepperSettings.RebuildArguments;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperSettings.RebuildArguments');
	dbgMsg.Msg(FRipGrepParameters.GuiSearchTextParams.ToString);
	dbgMsg.Msg(FRipGrepParameters.ToString);
	TCommandLineBuilder.RebuildArguments(FRipGrepParameters);
	dbgMsg.Msg(FRipGrepParameters.GuiSearchTextParams.ToString);
end;

procedure TRipGrepperSettings.LoadFromDict;
begin
	FRipGrepParameters.LoadFromDict;
	FSearchFormSettings.LoadFromDict;
end;

procedure TRipGrepperSettings.ReCreateMemIni; // Composit
begin
	inherited ReCreateMemIni;
	FSearchFormSettings.ReCreateMemIni();
end;

procedure TRipGrepperSettings.ReLoad; // Composit
begin
	inherited ReLoad;
end;

procedure TRipGrepperSettings.SetFileMasksHistory(const Value : TStrings);
begin
	AddIfNotContains(FFileMasksHistory, Value);
end;

procedure TRipGrepperSettings.SetReplaceTextsHistory(const Value : TStrings);
begin
	AddIfNotContains(FReplaceTextsHistory, Value);
end;

procedure TRipGrepperSettings.SetExpertOptionHistory(const Value : TSTrings);
begin
	AddIfNotContains(FExpertOptionHistory, Value);
end;

procedure TRipGrepperSettings.SetSearchPathsHistory(const Value : TStrings);
begin
	AddIfNotContains(FSearchPathsHistory, Value);
end;

procedure TRipGrepperSettings.SetSearchTextsHistory(const Value : TStrings);
begin
	AddIfNotContains(FSearchTextsHistory, Value);
end;

procedure TRipGrepperSettings.StoreToDict;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperSettings.StoreToDict');
	inherited StoreToDict();
	if IsModified then begin
		dbgMsg.Msg('IsModified');

		if (FRipGrepParameters.IsModified) then begin
			StoreHistories();
		end;
	end;
end;

procedure TRipGrepperSettings.StoreAsDefaultsToDict;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperSettings.StoreAsDefaultsToDict');
	FSearchFormSettings.StoreAsDefaultsToDict;
	FRipGrepParameters.StoreAsDefaultsToDict;
	inherited StoreAsDefaultsToDict;
end;

procedure TRipGrepperSettings.StoreHistories;
begin
	StoreHistoryEntries(SearchPathsHistory, 'SearchPathsHistory');
	StoreHistoryEntries(SearchTextsHistory, 'SearchTextsHistory');
	StoreHistoryEntries(ReplaceTextsHistory, 'ReplaceTextsHistory');
	StoreHistoryEntries(ExpertOptionHistory, 'ExpertOptionHistory');
	StoreHistoryEntries(FileMasksHistory, 'FileMasksHistory');
end;

procedure TRipGrepperSettings.StoreViewSettings(const _s : string = '');
begin
	FNodeLookSettings.SetViewSettingValues(_s);
	FNodeLookSettings.StoreToDict;
end;

procedure TRipGrepperSettings.StoreHistoryEntries(const _list : TStrings; const _section : string);
var multiLineVal : TMultiLineString;
begin
	for var i := _list.Count - 1 downto 0 do begin
		if not _list[i].IsEmpty then begin
			multiLineVal := _list[i];
			IniFile.WriteString(_section, 'Item_' + i.ToString, multiLineVal.GetLine(0));
		end;
	end;
end;

end.
