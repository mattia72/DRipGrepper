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
	THistoryItemObject = class(TNoRefCountObject, IHistoryItem)
		private
			FElapsedTimeText : string;
			FErrorCount : Integer;
			FFileCount : integer;
			FGuiSearchTextParams: TGuiSearchTextParams;
			FMatches : TParsedObjectRowCollection;
			FNoMatchFound : Boolean;
			FParserType : TParserType;
			FRipGrepArguments : TRipGrepArguments;
			FRipGrepperSearchFormSettings : TRipGrepperSearchFormSettings;
			FRipGrepResult : Integer;
			FTotalMatchCount : integer;
			function GetErrorCount : Integer; export;
			function GetFileCount : integer;
			function GetMatches : TParsedObjectRowCollection;
			function GetRipGrepArguments : TRipGrepArguments;
			function GetTotalMatchCount : integer;
			procedure SetFileCount(const Value : integer);
			procedure SetMatches(const Value : TParsedObjectRowCollection);
			procedure SetRipGrepArguments(const Value : TRipGrepArguments);
			function GetParserType : TParserType;
			procedure SetParserType(const Value : TParserType);

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
			property ErrorCount : Integer read GetErrorCount write FErrorCount;
			property ElapsedTimeText : string read FElapsedTimeText write FElapsedTimeText;
			property GuiSearchTextParams: TGuiSearchTextParams read FGuiSearchTextParams write FGuiSearchTextParams;
			property NoMatchFound : Boolean read FNoMatchFound write FNoMatchFound;
			property RipGrepResult : Integer read FRipGrepResult write FRipGrepResult;
			property ParserType : TParserType read GetParserType write SetParserType;
			property RipGrepperSearchFormSettings : TRipGrepperSearchFormSettings read FRipGrepperSearchFormSettings write FRipGrepperSearchFormSettings;

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
	GuiSearchTextParams := _settings.RipGrepParameters.GuiSearchTextParams;
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
	FRipGrepArguments.Free;
	FRipGrepperSearchFormSettings.Free;
	inherited;
end;

constructor THistoryItemObject.Create;
begin
	inherited;
	FMatches := TParsedObjectRowCollection.Create();
	FRipGrepperSearchFormSettings := TRipGrepperSearchFormSettings.Create();
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
	_settings.RipGrepParameters.GuiSearchTextParams := GuiSearchTextParams;
	_settings.RipGrepperSearchFormSettings.Copy(RipGrepperSearchFormSettings);
end;

function THistoryItemObject.GetErrorCount : Integer;
begin
	Result := FErrorCount;
end;

function THistoryItemObject.GetParserType : TParserType;
begin
	if FParserType = ptEmpty then begin
		UpdateParserType();
	end;
	Result := FParserType;
end;

procedure THistoryItemObject.SetParserType(const Value : TParserType);
begin
	FParserType := Value;
end;

function THistoryItemObject.UpdateParserType : TParserType;
begin
	FParserType := TRipGrepperParsersFactory.TryGetParserType(TArrayEx<string>.Create(RipGrepArguments.GetValues()));
	Result := FParserType;
end;

end.
