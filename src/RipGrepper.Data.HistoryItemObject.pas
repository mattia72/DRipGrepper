unit RipGrepper.Data.HistoryItemObject;

interface

uses
	Vcl.ComCtrls,
	System.Generics.Defaults,
	System.Classes,
	ArrayEx,
	RipGrepper.Common.Interfaces,
	RipGrepper.Common.ParsedObject,
	RipGrepper.Settings.AppSettings,
	RipGrepper.Common.Constants,
	RipGrepper.Common.SimpleTypes,
	RipGrepper.Common.GuiSearchParams,
	RipGrepper.Settings.RipGrepperSettings,
	RipGrepper.Settings.SearchFormSettings;

type
	// THistoryItemObject = class(TNoRefCountObject, IHistoryItemObject)
	THistoryItemObject = class(TInterfacedObject, IHistoryItemObject)

		private
			FElapsedTimeText : string;
			FErrorCounters : TErrorCounters;
			FFileCount : integer;
			FGuiSearchTextParams : TGuiSearchTextParams;
			FHasResult : Boolean;
			FMatches : TParsedObjectRowCollection;
			FNoMatchFound : Boolean;
			FParserType : TParserType;
			FRipGrepArguments : TRipGrepArguments;
			FSearchFormSettings : TSearchFormSettings;
			FRipGrepResult : Integer;
			FTotalMatchCount : integer;
			function GetElapsedTimeText : string;
			function GetErrorCounters : TErrorCounters;
			function GetFileCount : integer;
			function GetGuiSearchTextParams : TGuiSearchTextParams;
			function GetIsReplaceMode : Boolean;
			function GetMatches : TParsedObjectRowCollection;
			function GetNoMatchFound : Boolean;
			function GetRipGrepArguments : TRipGrepArguments;
			function GetTotalMatchCount : integer;
			procedure SetFileCount(const Value : integer);
			procedure SetMatches(const Value : TParsedObjectRowCollection);
			procedure SetRipGrepArguments(const Value : TRipGrepArguments);
			function GetParserType : TParserType;
			function GetSearchFormSettings : TSearchFormSettings;
			function GetRipGrepResult : Integer;
			procedure SetParserType(const Value : TParserType);
			function GetSearchText : string;
			procedure SetElapsedTimeText(const Value : string);
			procedure SetGuiSearchTextParams(const Value : TGuiSearchTextParams);
			procedure SetNoMatchFound(const Value : Boolean);
			procedure SetSearchFormSettings(const Value : TSearchFormSettings);
			procedure SetRipGrepResult(const Value : Integer);

		public
			constructor Create;
			destructor Destroy; override;
			procedure SetErrorCounters(const Value : TErrorCounters);
			procedure ClearMatches;
			procedure CopyToSettings(const _settings : TRipGrepperSettings);
			function GetReplaceText : string;
			function HasResult : Boolean;
			procedure LoadFromSettings(const _settings : TRipGrepperSettings);
			function UpdateParserType : TParserType;

			property FileCount : integer read GetFileCount write SetFileCount;
			property Matches : TParsedObjectRowCollection read GetMatches write SetMatches;
			property RipGrepArguments : TRipGrepArguments read GetRipGrepArguments write SetRipGrepArguments;
			property TotalMatchCount : integer read GetTotalMatchCount;
			property ElapsedTimeText : string read GetElapsedTimeText write SetElapsedTimeText;
			property GuiSearchTextParams : TGuiSearchTextParams read GetGuiSearchTextParams write SetGuiSearchTextParams;
			property IsReplaceMode : Boolean read GetIsReplaceMode;
			property NoMatchFound : Boolean read GetNoMatchFound write SetNoMatchFound;
			property RipGrepResult : Integer read GetRipGrepResult write SetRipGrepResult;
			property ParserType : TParserType read GetParserType write SetParserType;
			property SearchFormSettings : TSearchFormSettings read GetSearchFormSettings write SetSearchFormSettings;
			property SearchText : string read GetSearchText;
			property ReplaceText : string read GetReplaceText;

	end;

	TVSHistoryReplaceNodeData = record
		IsReplaceMode : Boolean;
		ReplaceText : string;
	end;

	TVSHistoryNodeData = record
		SearchText : string;
		ReplaceData : TVSHistoryReplaceNodeData;
	end;

	PVSHistoryNodeData = ^TVSHistoryNodeData;

implementation

uses

	System.SysUtils,
	RipGrepper.Parsers.Factory,
	RipGrepper.Helper.Types,
	RipGrepper.Settings.RipGrepParameterSettings;

procedure THistoryItemObject.LoadFromSettings(const _settings : TRipGrepperSettings);
begin
	RipGrepArguments.Assign(_settings.GetRipGrepArguments);
	GuiSearchTextParams.Copy(_settings.RipGrepParameters.GuiSearchTextParams);
	SearchFormSettings.Copy(_settings.SearchFormSettings);
	_settings.LastReplaceText := ReplaceText;
end;

function THistoryItemObject.GetFileCount : integer;
begin
	Result := FFileCount;
end;

function THistoryItemObject.GetMatches : TParsedObjectRowCollection;
begin
	Result := FMatches;
end;

function THistoryItemObject.GetRipGrepArguments : TRipGrepArguments;
begin
	Result := FRipGrepArguments;
end;

function THistoryItemObject.GetTotalMatchCount : integer;
begin
	Result := FMatches.Items.Count - FErrorCounters.FSumOfErrors - FErrorCounters.FStatLineCount;
	{$IFDEF THREADSAFE_LIST}
	FMatches.Unlock;
	{$ENDIF}
end;

procedure THistoryItemObject.SetFileCount(const Value : integer);
begin
	FFileCount := Value;
end;

procedure THistoryItemObject.SetMatches(const Value : TParsedObjectRowCollection);
begin
	FMatches := Value;
end;

procedure THistoryItemObject.SetRipGrepArguments(const Value : TStringList);
begin
	FRipGrepArguments := Value;
end;

destructor THistoryItemObject.Destroy;
begin
	(FMatches as TParsedObjectRowCollection).Free;
	FGuiSearchTextParams.Free;
	FSearchFormSettings.Free;
	FRipGrepArguments.Free;
	inherited;
end;

constructor THistoryItemObject.Create;
begin
	inherited;
	FMatches := TParsedObjectRowCollection.Create();
	FSearchFormSettings := TSearchFormSettings.Create();
	FGuiSearchTextParams := TGuiSearchTextParams.Create(TRipGrepParameterSettings.INI_SECTION);
	FRipGrepArguments := TStringList.Create;
	FParserType := ptEmpty;
	ClearMatches;
	FHasResult := False;
end;

procedure THistoryItemObject.ClearMatches;
begin
	FFileCount := 0;
	FTotalMatchCount := 0;
	FErrorCounters.Reset;
	FNoMatchFound := False;
	FMatches.Items.Clear;
	{$IFDEF THREADSAFE_LIST}
	FMatches.Unlock;
	{$ENDIF}
end;

procedure THistoryItemObject.CopyToSettings(const _settings : TRipGrepperSettings);
begin
	_settings.RipGrepParameters.RipGrepArguments.Assign(RipGrepArguments);
	_settings.RipGrepParameters.GuiSearchTextParams.Copy(GuiSearchTextParams);
	_settings.SearchFormSettings.Copy(SearchFormSettings);
end;

function THistoryItemObject.GetElapsedTimeText : string;
begin
	Result := FElapsedTimeText;
end;

function THistoryItemObject.GetErrorCounters : TErrorCounters;
begin
	Result := FErrorCounters;
end;

function THistoryItemObject.GetGuiSearchTextParams : TGuiSearchTextParams;
begin
	Result := FGuiSearchTextParams;
end;

function THistoryItemObject.GetIsReplaceMode : Boolean;
begin
	Result := GuiSearchTextParams.IsReplaceMode;
end;

function THistoryItemObject.GetNoMatchFound : Boolean;
begin
	Result := FNoMatchFound;
end;

function THistoryItemObject.GetParserType : TParserType;
begin
	if FParserType = ptEmpty then begin
		UpdateParserType();
	end;
	Result := FParserType;
end;

function THistoryItemObject.GetReplaceText : string;
begin
	Result := GuiSearchTextParams.ReplaceText;
end;

function THistoryItemObject.GetSearchFormSettings : TSearchFormSettings;
begin
	Result := FSearchFormSettings;
end;

function THistoryItemObject.GetRipGrepResult : Integer;
begin
	Result := FRipGrepResult;
end;

function THistoryItemObject.GetSearchText : string;
begin
	Result := GuiSearchTextParams.SearchText;
end;

function THistoryItemObject.HasResult : Boolean;
begin
	Result := FHasResult;
end;

procedure THistoryItemObject.SetElapsedTimeText(const Value : string);
begin
	FElapsedTimeText := Value;
end;

procedure THistoryItemObject.SetErrorCounters(const Value : TErrorCounters);
begin
	FErrorCounters := Value;
end;

procedure THistoryItemObject.SetGuiSearchTextParams(const Value : TGuiSearchTextParams);
begin
	FGuiSearchTextParams := Value;
end;

procedure THistoryItemObject.SetNoMatchFound(const Value : Boolean);
begin
	FNoMatchFound := Value;
end;

procedure THistoryItemObject.SetParserType(const Value : TParserType);
begin
	FParserType := Value;
end;

procedure THistoryItemObject.SetSearchFormSettings(const Value : TSearchFormSettings);
begin
	FSearchFormSettings := Value;
end;

procedure THistoryItemObject.SetRipGrepResult(const Value : Integer);
begin
	FRipGrepResult := Value;
	FHasResult := True;
end;

function THistoryItemObject.UpdateParserType : TParserType;
begin
	FParserType := TRipGrepperParsersFactory.TryGetParserType(TArrayEx<string>.Create(RipGrepArguments.GetValues()));
	Result := FParserType;
end;

end.
