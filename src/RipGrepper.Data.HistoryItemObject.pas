unit RipGrepper.Data.HistoryItemObject;

interface

uses
	RipGrepper.Common.Interfaces,
	RipGrepper.Common.ParsedObject,
	ArrayEx,
	RipGrepper.Common.Settings.Misc,
	Vcl.ComCtrls,
	System.Generics.Defaults,
	System.Classes,
	RipGrepper.Common.Constants,
	RipGrepper.Common.GuiSearchParams,
	RipGrepper.Common.Settings.RipGrepperSettings,
	RipGrepper.Common.Settings.RipGrepperSearchFormSettings;

type
//	THistoryItemObject = class(TNoRefCountObject, IHistoryItemObject)
	THistoryItemObject = class(TInterfacedObject, IHistoryItemObject)
		private
			FElapsedTimeText : string;
			FErrorCount: Integer;
			FFileCount : integer;
			FGuiSearchTextParams : TGuiSearchTextParams;
			FMatches : TParsedObjectRowCollection;
			FNoMatchFound : Boolean;
			FParserType : TParserType;
			FRipGrepArguments : TRipGrepArguments;
			FRipGrepperSearchFormSettings : TRipGrepperSearchFormSettings;
			FRipGrepResult: Integer;
			FTotalMatchCount : integer;
			function GetElapsedTimeText : string;
			function GetErrorCount: Integer; export;
			function GetFileCount : integer;
			function GetGuiSearchTextParams : TGuiSearchTextParams;
			function GetMatches : TParsedObjectRowCollection;
			function GetNoMatchFound : Boolean;
			function GetRipGrepArguments : TRipGrepArguments;
			function GetTotalMatchCount : integer;
			procedure SetFileCount(const Value : integer);
			procedure SetMatches(const Value : TParsedObjectRowCollection);
			procedure SetRipGrepArguments(const Value : TRipGrepArguments);
			function GetParserType : TParserType;
			function GetRipGrepperSearchFormSettings : TRipGrepperSearchFormSettings;
			function GetRipGrepResult: Integer;
			procedure SetParserType(const Value : TParserType);
			function GetSearchText : string;
			procedure SetElapsedTimeText(const Value : string);
			procedure SetErrorCount(const Value: Integer);
			procedure SetGuiSearchTextParams(const Value : TGuiSearchTextParams);
			procedure SetNoMatchFound(const Value : Boolean);
			procedure SetRipGrepperSearchFormSettings(const Value : TRipGrepperSearchFormSettings);
			procedure SetRipGrepResult(const Value: Integer);

		public
			procedure LoadFromSettings(const _settings : TRipGrepperSettings);
			destructor Destroy; override;
			constructor Create;
			procedure ClearMatches;
			procedure CopyToSettings(const _settings : TRipGrepperSettings);
			function UpdateParserType : TParserType;
			property FileCount : integer read GetFileCount write SetFileCount;
			property Matches : TParsedObjectRowCollection read GetMatches write SetMatches;
			property RipGrepArguments : TRipGrepArguments read GetRipGrepArguments write SetRipGrepArguments;
			property TotalMatchCount : integer read GetTotalMatchCount;
			property ErrorCount: Integer read GetErrorCount write SetErrorCount;
			property ElapsedTimeText : string read GetElapsedTimeText write SetElapsedTimeText;
			property GuiSearchTextParams : TGuiSearchTextParams read GetGuiSearchTextParams write SetGuiSearchTextParams;
			property NoMatchFound : Boolean read GetNoMatchFound write SetNoMatchFound;
			property RipGrepResult: Integer read GetRipGrepResult write SetRipGrepResult;
			property ParserType : TParserType read GetParserType write SetParserType;
			property RipGrepperSearchFormSettings : TRipGrepperSearchFormSettings read GetRipGrepperSearchFormSettings
				write SetRipGrepperSearchFormSettings;
			property SearchText : string read GetSearchText;

	end;

	TVSHistoryNodeData = record
		SearchText : string;
	end;

	PVSHistoryNodeData = ^TVSHistoryNodeData;

implementation

uses

	System.SysUtils,
	RipGrepper.Parsers.Factory,
	RipGrepper.Helper.Types;

procedure THistoryItemObject.LoadFromSettings(const _settings : TRipGrepperSettings);
begin
	RipGrepArguments.Assign(_settings.GetRipGrepArguments);
	GuiSearchTextParams.Copy(_settings.RipGrepParameters.GuiSearchTextParams);
	RipGrepperSearchFormSettings.Copy(_settings.RipGrepperSearchFormSettings);
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
	Result := FMatches.Items.Count - ErrorCount;
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
	FRipGrepperSearchFormSettings.Free;
	FRipGrepArguments.Free;
	inherited;
end;

constructor THistoryItemObject.Create;
begin
	inherited;
	FMatches := TParsedObjectRowCollection.Create();
	FRipGrepperSearchFormSettings := TRipGrepperSearchFormSettings.Create();
	FGuiSearchTextParams := TGuiSearchTextParams.Create;
	FRipGrepArguments := TStringList.Create;
	FParserType := ptEmpty;
	ClearMatches;
end;

procedure THistoryItemObject.ClearMatches;
begin
	FFileCount := 0;
	FTotalMatchCount := 0;
	FErrorCount := 0;
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
	_settings.RipGrepperSearchFormSettings.Copy(RipGrepperSearchFormSettings);
end;

function THistoryItemObject.GetElapsedTimeText : string;
begin
	Result := FElapsedTimeText;
end;

function THistoryItemObject.GetErrorCount: Integer;
begin
	Result := FErrorCount;
end;

function THistoryItemObject.GetGuiSearchTextParams : TGuiSearchTextParams;
begin
	Result := FGuiSearchTextParams;
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

function THistoryItemObject.GetRipGrepperSearchFormSettings : TRipGrepperSearchFormSettings;
begin
	Result := FRipGrepperSearchFormSettings;
end;

function THistoryItemObject.GetRipGrepResult: Integer;
begin
	Result := FRipGrepResult;
end;

function THistoryItemObject.GetSearchText : string;
begin
	Result := GuiSearchTextParams.SearchText;
end;

procedure THistoryItemObject.SetElapsedTimeText(const Value : string);
begin
	FElapsedTimeText := Value;
end;

procedure THistoryItemObject.SetErrorCount(const Value: Integer);
begin
	FErrorCount := Value;
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

procedure THistoryItemObject.SetRipGrepperSearchFormSettings(const Value : TRipGrepperSearchFormSettings);
begin
	FRipGrepperSearchFormSettings := Value;
end;

procedure THistoryItemObject.SetRipGrepResult(const Value: Integer);
begin
	FRipGrepResult := Value;
end;

function THistoryItemObject.UpdateParserType : TParserType;
begin
	FParserType := TRipGrepperParsersFactory.TryGetParserType(TArrayEx<string>.Create(RipGrepArguments.GetValues()));
	Result := FParserType;
end;

end.
