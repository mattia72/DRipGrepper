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
	RipGrepper.Settings.FontColors,
	RipGrepper.Helper.MemIniFile,
	Spring,
	RipGrepper.Settings.SettingVariant,
    RipGrepper.Settings.RipGrepArguments;

type
	TRipGrepperSettings = class(TPersistableSettings)

		private
			FSearchFormSettings : TSearchFormSettings;

			FRipGrepParameters : TRipGrepParameterSettings;
			FNodeLookSettings : TNodeLookSettings;
			FOpenWithSettings : TOpenWithSettings;
			FFontColorSettings : TColorSettings;
			FAppSettings : TAppSettings;

			FExpertOptionHistory : IArraySetting;
			FSearchPathsHistory : IArraySetting;
			FSearchTextsHistory : IArraySetting;
			FFileMasksHistory : IArraySetting;

			FRipGrepArguments : IShared<TRipGrepArguments>;
			FSearchPathIsDir : Boolean;

			FActualSearchPath : string;
			FLastSearchText : string;
			FLastReplaceText : string;
			FReplaceTextsHistory : IArraySetting;

			function GetIsEmpty : Boolean;
			function GetSearchPathIsDir : Boolean;
			procedure SetFileMasksHistory(const Value : IArraySetting);
			procedure SetExpertOptionHistory(const Value : IArraySetting);
			procedure SetSearchPathsHistory(const Value : IArraySetting);
			procedure SetSearchTextsHistory(const Value : IArraySetting);
			function GetActualSearchPath : string;
			function GetIsReplaceMode : Boolean;
			function GetSearchFormSettings : TSearchFormSettings;
			procedure LoadFirstNecessarySettings;
			procedure SetReplaceTextsHistory(const Value : IArraySetting);

		protected
			function GetIsAlreadyRead : Boolean; override;

		public
			procedure ReadFile(); override;
			procedure StoreToPersister; override;
			procedure StoreViewSettings(const _s : string = '');
			constructor Create;
			destructor Destroy; override;
			procedure AddIfNotContains(_to, _from : IArraySetting);
			procedure Copy(const _other : TPersistableSettings); override;
			function GetIsModified : Boolean; override;
			function GetLastHistorySearchText : string;
			function GetRipGrepArguments : IShared<TRipGrepArguments>;
			procedure Init; override;
			procedure RebuildArguments;
			procedure LoadFromDict(); override;
			procedure LoadInitialSettings;
			procedure ReLoad; override;
			procedure StoreHistories;
			property LastSearchText : string read FLastSearchText write FLastSearchText;
			property FileMasksHistory : IArraySetting read FFileMasksHistory write SetFileMasksHistory;
			property IsEmpty : Boolean read GetIsEmpty;

			property ActualSearchPath : string read GetActualSearchPath;
			property SearchPathsHistory : IArraySetting read FSearchPathsHistory write SetSearchPathsHistory;
			property ExpertOptionHistory : IArraySetting read FExpertOptionHistory write SetExpertOptionHistory;
			property RipGrepParameters : TRipGrepParameterSettings read FRipGrepParameters write FRipGrepParameters;
			property OpenWithSettings : TOpenWithSettings read FOpenWithSettings;
			property SearchFormSettings : TSearchFormSettings read GetSearchFormSettings write FSearchFormSettings;
			property AppSettings : TAppSettings read FAppSettings write FAppSettings;
			property FontColorSettings : TColorSettings read FFontColorSettings write FFontColorSettings;
			property IsReplaceMode : Boolean read GetIsReplaceMode;
			property LastReplaceText : string read FLastReplaceText write FLastReplaceText;
			property NodeLookSettings : TNodeLookSettings read FNodeLookSettings write FNodeLookSettings;
			property SearchPathIsDir : Boolean read GetSearchPathIsDir;
			property SearchTextsHistory : IArraySetting read FSearchTextsHistory write SetSearchTextsHistory;
			property ReplaceTextsHistory : IArraySetting read FReplaceTextsHistory write SetReplaceTextsHistory;
	end;

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
	Winapi.Windows,
	RipGrepper.Tools.LockGuard,
	RipGrepper.Settings.FilePersister,
	RipGrepper.Settings.SettingsDictionary,
	RipGrepper.Settings.Persister.Interfaces,
	Spring.DesignPatterns;

function TRipGrepperSettings.GetLastHistorySearchText : string;
begin
	Result := SearchTextsHistory.Value.SafeItem[0];
end;

function TRipGrepperSettings.GetIsEmpty : Boolean;
begin
	Result := FRipGrepParameters.RipGrepPath.IsEmpty;
end;

function TRipGrepperSettings.GetRipGrepArguments : IShared<TRipGrepArguments>;
begin
	Result := FRipGrepParameters.RipGrepArguments;
end;

procedure TRipGrepperSettings.LoadInitialSettings;
begin
	var
	dbgArr := TSettingsDictionary.DictToStringArray(SettingsDict());

	LoadFirstNecessarySettings;

	if TArraySetting(SearchPathsHistory).Count = 0 then begin
		SearchPathsHistory.Value.Add(TDirectory.GetCurrentDirectory());
	end;

	if TArraySetting(SearchTextsHistory).Count = 0 then begin
		SearchTextsHistory.Value.Add('search text');
	end;

	if TArraySetting(ExpertOptionHistory).Count = 0 then begin
		ExpertOptionHistory.Value.Add('');
	end;

	if TArraySetting(FileMasksHistory).Count = 0 then begin
		FileMasksHistory.Value.Add('');
	end;
end;

destructor TRipGrepperSettings.Destroy;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperSettings.Destroy');
	UpdateFile(True);
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

	FRipGrepArguments := Shared.Make<TRipGrepArguments>();
	FRipGrepArguments.Delimiter := ' ';
end;

procedure TRipGrepperSettings.AddIfNotContains(_to, _from : IArraySetting);
begin
	var
	origCount := _to.Value.Count;
	for var s in _from.Value do begin
		_to.Value.InsertIfNotContains(0, s);
		FIsModified := (origCount <> _to.Value.Count) or FIsModified;
	end;
end;

procedure TRipGrepperSettings.Copy(const _other : TPersistableSettings);
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperSettings.Copy');

	if Assigned(_other) then begin
		inherited Copy(_other as TPersistableSettings);

		var
		s := _other as TRipGrepperSettings;

		FAppSettings.Copy(s.AppSettings);
		FFontColorSettings.Copy(s.FontColorSettings);
		FNodeLookSettings.Copy(s.NodeLookSettings);
		FOpenWithSettings.Copy(s.OpenWithSettings);
		FRipGrepParameters.Copy(s.RipGrepParameters);
		FSearchFormSettings.Copy(s.SearchFormSettings);

		FSearchPathsHistory.Value.SetItems(s.SearchPathsHistory.Value.Items);
		FSearchTextsHistory.Value.SetItems(s.SearchTextsHistory.Value.Items);
		FReplaceTextsHistory.Value.SetItems(s.ReplaceTextsHistory.Value.Items);
		FExpertOptionHistory.Value.SetItems(s.ExpertOptionHistory.Value.Items);
		FRipGrepArguments.Assign(s.FRipGrepArguments());
		// inherited Copy(_other as TPersistableSettings);
	end;
end;

function TRipGrepperSettings.GetActualSearchPath : string;
var
	s : string;
begin
	s := SearchPathsHistory.Value.SafeItem[0];
	if not SearchPathsHistory.Value.IsEmpty and (s <> FActualSearchPath) then begin
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
	// if not FSearchFormSettings.IsAlreadyRead then begin
	// FSearchFormSettings.ReadFile;
	// end;
	Result := FSearchFormSettings;
end;

function TRipGrepperSettings.GetSearchPathIsDir : Boolean;
begin
	Result := FSearchPathIsDir;
end;

procedure TRipGrepperSettings.Init;
begin
	FSearchPathsHistory := TArraySetting.Create();
	FSearchTextsHistory := TArraySetting.Create();
	FReplaceTextsHistory := TArraySetting.Create();
	FExpertOptionHistory := TArraySetting.Create();
	FFileMasksHistory := TArraySetting.Create();

	CreateSetting('SearchPathsHistory', ITEM_KEY_PREFIX, FSearchPathsHistory);
	CreateSetting('SearchTextsHistory', ITEM_KEY_PREFIX, FSearchTextsHistory);
	CreateSetting('ReplaceTextsHistory', ITEM_KEY_PREFIX, FReplaceTextsHistory);
	CreateSetting('ExpertOptionHistory', ITEM_KEY_PREFIX, FExpertOptionHistory);
	CreateSetting('FileMasksHistory', ITEM_KEY_PREFIX, FFileMasksHistory);

end;

procedure TRipGrepperSettings.ReadFile();
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperSettings.ReadFile');
	try
		inherited ReadFile();

		FSearchPathsHistory.LoadFromPersister;
		FSearchTextsHistory.LoadFromPersister;
		FReplaceTextsHistory.LoadFromPersister;
		FExpertOptionHistory.LoadFromPersister;
		FFileMasksHistory.LoadFromPersister;

	except
		on E : Exception do begin
			TDebugUtils.DebugMessage(Format('TRipGrepperSettings.ReadFile: Exception %s ', [E.Message]));
			TMsgBox.ShowError(E.Message + CRLF + 'Settings Read from ' + ' went wrong.');
		end;
	end;
end;

procedure TRipGrepperSettings.LoadFirstNecessarySettings;
begin
	FNodeLookSettings.LoadFromDict();

	FSearchPathsHistory.LoadFromPersister;
	FSearchTextsHistory.LoadFromPersister;
	FReplaceTextsHistory.LoadFromPersister;
	FExpertOptionHistory.LoadFromPersister;
	FFileMasksHistory.LoadFromPersister;

	FFontColorSettings.ReadFile;
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

procedure TRipGrepperSettings.ReLoad; // Composit
begin
	inherited ReLoad;
end;

procedure TRipGrepperSettings.SetFileMasksHistory(const Value : IArraySetting);
begin
	AddIfNotContains(FFileMasksHistory, Value);
end;

procedure TRipGrepperSettings.SetReplaceTextsHistory(const Value : IArraySetting);
begin
	AddIfNotContains(FReplaceTextsHistory, Value);
end;

procedure TRipGrepperSettings.SetExpertOptionHistory(const Value : IArraySetting);
begin
	AddIfNotContains(FExpertOptionHistory, Value);
end;

procedure TRipGrepperSettings.SetSearchPathsHistory(const Value : IArraySetting);
begin
	AddIfNotContains(FSearchPathsHistory, Value);
end;

procedure TRipGrepperSettings.SetSearchTextsHistory(const Value : IArraySetting);
begin
	AddIfNotContains(FSearchTextsHistory, Value);
end;

procedure TRipGrepperSettings.StoreToPersister; // histories save to file
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperSettings.StoreToPersister');
	inherited StoreToPersister();
	StoreHistories();
end;

procedure TRipGrepperSettings.StoreHistories;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperSettings.StoreHistories');

	SearchTextsHistory.StoreToPersister;
	ReplaceTextsHistory.StoreToPersister;
	SearchPathsHistory.StoreToPersister;
	FileMasksHistory.StoreToPersister;
	ExpertOptionHistory.StoreToPersister;
end;

procedure TRipGrepperSettings.StoreViewSettings(const _s : string = '');
var
	fh : IFileHandler;
begin
	NodeLookSettings.UpdateFile(True);
	if Supports(PersisterFactory, IFileHandler, fh) then begin
		fh.UpdateFile();
	end;
end;

initialization

OutputDebugString(PChar('RipGrepperSettings initialization.'));

var
mainSettingInstance := TSingleton.GetInstance<TRipGrepperSettings>();
mainSettingInstance.AppSettings.ReadFile;
mainSettingInstance.AppSettings.LoadFromDict();
TDebugUtils.UpdateTraceActive;

finalization

OutputDebugString(PChar('RipGrepperSettings finalized.'));

end.
