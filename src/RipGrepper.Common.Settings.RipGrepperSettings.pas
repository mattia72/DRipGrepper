unit RipGrepper.Common.Settings.RipGrepperSettings;

interface

uses
	RipGrepper.Common.Settings,
	RipGrepper.Common.Settings.Persistable,
	RipGrepper.Common.Settings.RipGrepParameterSettings,
	RipGrepper.Common.Settings.RipGrepperSearchFormSettings,
	System.Classes,
	RipGrepper.Common.Constants,
	System.IniFiles,
	RipGrepper.Common.Settings.RipGrepperViewSettings,
	RipGrepper.Common.Settings.RipGrepperSettingsDefaults,
	RipGrepper.Common.Settings.RipGrepperOpenWithSettings;

type
	TRipGrepperSettings = class(TRipGrepperSettingsDefaults)
		private
			FRipGrepperViewSettings : TRipGrepperViewSettings;
			FRipGrepperOpenWithSettings : TRipGrepperOpenWithSettings;

			FRipGrepOptionsHistory : TSTrings;
			FSearchPathsHistory : TStrings;
			FSearchTextsHistory : TStrings;
			FFileMasksHistory : TStrings;

			FRipGrepArguments : TRipGrepArguments;
			FSearchPathIsDir : Boolean;
			FRipGrepperSettings : TRipGrepperAppSettings;

			FActualSearchPath : string;

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
			function GetActualSearchPath : string;

		public
			procedure Load; override;
			procedure Store; override;
			procedure StoreViewSettings(const _s : string = '');
			constructor Create;
			destructor Destroy; override;
			procedure AddIfNotContains(_to, _from : TStrings);
			function GetIsModified : Boolean; override;
			function GetRipGrepArguments : TRipGrepArguments;
			procedure RebuildArguments;
			property ActualSearchText : string read GetActualSearchText;
			property FileMasksHistory : TStrings read FFileMasksHistory write SetFileMasksHistory;
			property IsEmpty : Boolean read GetIsEmpty;

			property ActualSearchPath : string read GetActualSearchPath;
			property SearchPathsHistory : TStrings read FSearchPathsHistory write SetSearchPathsHistory;
			property RipGrepOptionsHistory : TSTrings read FRipGrepOptionsHistory write SetRipGrepOptionsHistory;
			property RipGrepperOpenWithSettings : TRipGrepperOpenWithSettings read FRipGrepperOpenWithSettings;
			property RipGrepperSettings : TRipGrepperAppSettings read FRipGrepperSettings write FRipGrepperSettings;
			property RipGrepperViewSettings : TRipGrepperViewSettings read FRipGrepperViewSettings write FRipGrepperViewSettings;
			property SearchPathIsDir : Boolean read GetSearchPathIsDir;
			property SearchTextsHistory : TStrings read FSearchTextsHistory write SetSearchTextsHistory;
	end;

type
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
	RipGrepper.Helper.Types,
	System.IOUtils,
	System.SysUtils,
	RipGrepper.Common.IOTAUtils,
	System.AnsiStrings,
	Vcl.Forms,
	RipGrepper.Helper.UI,
	RipGrepper.Tools.DebugUtils,
	RipGrepper.CommandLine.Builder;

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
	FRipGrepperSettings.Free;
	FFileMasksHistory.Free;
	UpdateIniFile;
	FIniFile.Free;
	inherited;
end;

constructor TRipGrepperSettings.Create;
begin
	inherited;
	IniSectionName := ROOT_DUMMY_INI_SECTION;
	FRipGrepperSettings := TRipGrepperAppSettings.Create(FIniFile);
	FRipGrepParameters := TRipGrepParameterSettings.Create(FIniFile);
	FRipGrepperViewSettings := TRipGrepperViewSettings.Create(FIniFile);
	FRipGrepperOpenWithSettings := TRipGrepperOpenWithSettings.Create(FIniFile);
	FSearchPathsHistory := TStringList.Create(dupIgnore, False, True);
	FSearchTextsHistory := TStringList.Create(dupIgnore, False, True);
	FRipGrepOptionsHistory := TStringList.Create(dupIgnore, False, True);
	FRipGrepArguments := TStringList.Create();
	FRipGrepArguments.Delimiter := ' ';
	FFileMasksHistory := TStringList.Create(dupIgnore, False, True);

	FIsLoaded := False;
end;

procedure TRipGrepperSettings.AddIfNotContains(_to, _from : TStrings);
begin
	FIsModified := TItemInserter.AddToSringListIfNotContains(_to, _from);
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

function TRipGrepperSettings.GetIsModified : Boolean;
begin
	Result := FIsModified or FRipGrepParameters.IsModified or
	{ } FRipGrepperViewSettings.IsModified or
	{ } FRipGrepperOpenWithSettings.IsModified;
end;

function TRipGrepperSettings.GetSearchPathIsDir : Boolean;
begin
	Result := FSearchPathIsDir;
end;

procedure TRipGrepperSettings.Load;
begin
	inherited Load;
	TDebugUtils.DebugMessage('TRipGrepperSettings.Load: start');

	try
		FRipGrepperViewSettings.Load;
		FRipGrepperOpenWithSettings.Load;
		FRipGrepperSettings.Load;

		LoadHistoryEntries(FSearchPathsHistory, 'SearchPathsHistory');
		LoadHistoryEntries(FSearchTextsHistory, 'SearchTextsHistory');
		LoadHistoryEntries(FRipGrepOptionsHistory, 'RipGrepOptionsHistory');
		LoadHistoryEntries(FFileMasksHistory, 'FileMasksHistory');
	except
		on E : Exception do begin
			TDebugUtils.DebugMessage(Format('TRipGrepperSettings.Load: Exception %s ', [E.Message]));
			TMsgBox.ShowError(E.Message + CRLF + 'Settings load from ' + FIniFile.FileName + ' went wrong.');
		end;
	end;
	InitSettings;
	FIsLoaded := True;
end;

procedure TRipGrepperSettings.RebuildArguments;
begin
	TDebugUtils.DebugMessage('TRipGrepperSettings.RebuildArguments: GuiSearchTextParams start ' +
		FRipGrepParameters.GuiSearchTextParams.ToString);

	TCommandLineBuilder.RebuildArguments(FRipGrepParameters);

	TDebugUtils.DebugMessage('TRipGrepperSettings.RebuildArguments: GuiSearchTextParams end ' +
		FRipGrepParameters.GuiSearchTextParams.ToString);
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
	inherited;
	if IsLoaded and IsModified then begin
		FRipGrepperViewSettings.Store;
		FRipGrepperOpenWithSettings.Store;
		FRipGrepperSettings.Store;

		if (FRipGrepParameters.IsModified) then begin
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

initialization

GSettings := TRipGrepperSettings.Create;

finalization

GSettings.Store;
GSettings.Free;

end.
