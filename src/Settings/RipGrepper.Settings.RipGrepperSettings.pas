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
	RipGrepper.Settings.SettingVariant;

type
	TRipGrepperSettings = class(TPersistableSettings)

		private
			FSearchFormSettings : TSearchFormSettings;

			FRipGrepParameters : TRipGrepParameterSettings;
			FNodeLookSettings : TNodeLookSettings;
			FOpenWithSettings : TOpenWithSettings;
			FFontColorSettings : TColorSettings;
			FAppSettings : TAppSettings;

			FExpertOptionHistory : TArraySetting;
			FSearchPathsHistory : TArraySetting;
			FSearchTextsHistory : TArraySetting;
			FFileMasksHistory : TArraySetting;

			FRipGrepArguments : IShared<TRipGrepArguments>;
			FSearchPathIsDir : Boolean;

			FActualSearchPath : string;
			FLastSearchText : string;
			FLastReplaceText : string;
			FReplaceTextsHistory : TArraySetting;

			function GetIsEmpty : Boolean;
			function GetSearchPathIsDir : Boolean;
			procedure SetFileMasksHistory(const Value : TArraySetting);
			procedure SetExpertOptionHistory(const Value : TArraySetting);
			procedure SetSearchPathsHistory(const Value : TArraySetting);
			procedure SetSearchTextsHistory(const Value : TArraySetting);
			function GetActualSearchPath : string;
			function GetIsReplaceMode : Boolean;
			function GetSearchFormSettings : TSearchFormSettings;
			procedure LoadFirstNecessarySettings;
			procedure SetReplaceTextsHistory(const Value : TArraySetting);

		protected
			function GetIsAlreadyRead : Boolean; override;

		public
			procedure ReadIni; override;
			procedure StoreToDict; override;
			procedure StoreViewSettings(const _s : string = '');
			constructor Create;
			destructor Destroy; override;
			procedure AddIfNotContains(_to, _from : TArraySetting);
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
			property FileMasksHistory : TArraySetting read FFileMasksHistory write SetFileMasksHistory;
			property IsEmpty : Boolean read GetIsEmpty;

			property ActualSearchPath : string read GetActualSearchPath;
			property SearchPathsHistory : TArraySetting read FSearchPathsHistory write SetSearchPathsHistory;
			property ExpertOptionHistory : TArraySetting read FExpertOptionHistory write SetExpertOptionHistory;
			property RipGrepParameters : TRipGrepParameterSettings read FRipGrepParameters write FRipGrepParameters;
			property OpenWithSettings : TOpenWithSettings read FOpenWithSettings;
			property SearchFormSettings : TSearchFormSettings read GetSearchFormSettings write FSearchFormSettings;
			property AppSettings : TAppSettings read FAppSettings write FAppSettings;
			property FontColorSettings : TColorSettings read FFontColorSettings write FFontColorSettings;
			property IsReplaceMode : Boolean read GetIsReplaceMode;
			property LastReplaceText : string read FLastReplaceText write FLastReplaceText;
			property NodeLookSettings : TNodeLookSettings read FNodeLookSettings write FNodeLookSettings;
			property SearchPathIsDir : Boolean read GetSearchPathIsDir;
			property SearchTextsHistory : TArraySetting read FSearchTextsHistory write SetSearchTextsHistory;
			property ReplaceTextsHistory : TArraySetting read FReplaceTextsHistory write SetReplaceTextsHistory;
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
	Winapi.Windows,
	RipGrepper.Tools.LockGuard,
	RipGrepper.Settings.FilePersister;

function TRipGrepperSettings.GetLastHistorySearchText : string;
begin
	Result := SearchTextsHistory.Value.SafeItemAt[0];
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
	LoadFirstNecessarySettings;

	if SearchPathsHistory.Count = 0 then begin
		SearchPathsHistory.Value.Add(TDirectory.GetCurrentDirectory());
	end;

	if SearchTextsHistory.Count = 0 then begin
		SearchTextsHistory.Value.Add('search text');
	end;

	if ExpertOptionHistory.Count = 0 then begin
		ExpertOptionHistory.Value.Add('');
	end;

	if FileMasksHistory.Count = 0 then begin
		FileMasksHistory.Value.Add('');
	end;
end;

destructor TRipGrepperSettings.Destroy;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperSettings.Destroy');
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

	FSearchPathsHistory := TArraySetting.Create();
	FSearchTextsHistory := TArraySetting.Create();
	FReplaceTextsHistory := TArraySetting.Create();
	FExpertOptionHistory := TArraySetting.Create();
	FFileMasksHistory := TArraySetting.Create();

	FRipGrepArguments := Shared.Make<TStringList>();
	FRipGrepArguments.Delimiter := ' ';
end;

procedure TRipGrepperSettings.AddIfNotContains(_to, _from : TArraySetting);
begin
	for var s in _from.Value do begin
		FIsModified := (_to.Value.AddIfNotContains(s) <> -1) or FIsModified;
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
	s := SearchPathsHistory.Value.SafeItemAt[0];
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
			TMsgBox.ShowError(E.Message + CRLF + 'Settings Read from ' + ' went wrong.');
		end;
	end;
end;

procedure TRipGrepperSettings.LoadFirstNecessarySettings;
begin
	FNodeLookSettings.LoadFromDict();

	FSearchPathsHistory.LoadFromFile;
	FSearchTextsHistory.LoadFromFile;
	FReplaceTextsHistory.LoadFromFile;
	FExpertOptionHistory.LoadFromFile;
	FFileMasksHistory.LoadFromFile;
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

procedure TRipGrepperSettings.SetFileMasksHistory(const Value : TArraySetting);
begin
	AddIfNotContains(FFileMasksHistory, Value);
end;

procedure TRipGrepperSettings.SetReplaceTextsHistory(const Value : TArraySetting);
begin
	AddIfNotContains(FReplaceTextsHistory, Value);
end;

procedure TRipGrepperSettings.SetExpertOptionHistory(const Value : TArraySetting);
begin
	AddIfNotContains(FExpertOptionHistory, Value);
end;

procedure TRipGrepperSettings.SetSearchPathsHistory(const Value : TArraySetting);
begin
	AddIfNotContains(FSearchPathsHistory, Value);
end;

procedure TRipGrepperSettings.SetSearchTextsHistory(const Value : TArraySetting);
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

procedure TRipGrepperSettings.StoreHistories;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperSettings.StoreHistories');

	SearchPathsHistory.SaveToFile;
	SearchTextsHistory.SaveToFile;
	ReplaceTextsHistory.SaveToFile;
	ExpertOptionHistory.SaveToFile;
	FileMasksHistory.SaveToFile;
end;

procedure TRipGrepperSettings.StoreViewSettings(const _s : string = '');
var
	fh : IFileHandler;
begin
	NodeLookSettings.SettingsDict.SaveToFile();
	NodeLookSettings.UpdateIniFile(NodeLookSettings.IniSectionName);
	NodeLookSettings.WriteSettingsDictToIni(NodeLookSettings.IniSectionName);
	if Supports(PersisterFactory, IFileHandler, fh) then begin
		fh.ReLoadFile();
		fh.WriteFile();
	end;
end;

end.
