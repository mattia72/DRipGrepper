unit RipGrepper.Data.HistoryItemObject;

interface

uses
	RipGrepper.Common.Interfaces,
	RipGrepper.Common.ParsedObject,
	ArrayEx,
	RipGrepper.Common.Settings,
	Vcl.ComCtrls,
	System.Generics.Defaults,
	System.Classes,
	RipGrepper.Common.Constants;

type
	THistoryItemObject = class(TNoRefCountObject, IHistoryItem)
		private
			FElapsedTimeText : string;
			FErrorCount : Integer;
			FFileCount : integer;
			FMatches : TParsedObjectRowCollection;
			FParserType : TParserType;
			FRipGrepArguments : TRipGrepArguments;
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
			procedure CopyRipGrepArgsFromSettings(const _settings : TRipGrepperSettings);
			procedure DataToGrid(_lv : TListView; _item : TListItem; const _index : Integer);
			destructor Destroy; override;
			constructor Create;
			procedure ClearMatches;
			procedure CopyToSettings(const _settings : TRipGrepperSettings);
			function UpdateParserType: TParserType;
			property FileCount : integer read GetFileCount write SetFileCount;
			property Matches : TParsedObjectRowCollection read GetMatches write SetMatches;
			property RipGrepArguments : TRipGrepArguments read GetRipGrepArguments write SetRipGrepArguments;
			property TotalMatchCount : integer read GetTotalMatchCount;
			property ErrorCount : Integer read GetErrorCount write FErrorCount;
			property ElapsedTimeText : string read FElapsedTimeText write FElapsedTimeText;
			property RipGrepResult : Integer read FRipGrepResult write FRipGrepResult;
			property ParserType : TParserType read GetParserType write SetParserType;

	end;

implementation

uses

	System.SysUtils, RipGrepper.Parsers.Factory, RipGrepper.Helper.Types;

procedure THistoryItemObject.CopyRipGrepArgsFromSettings(const _settings : TRipGrepperSettings);
begin
	RipGrepArguments.Assign(_settings.GetRipGrepArguments);
end;

procedure THistoryItemObject.DataToGrid(_lv : TListView; _item : TListItem; const _index : Integer);
var
	fn : string;
begin
	// (_index, _lv, _item);
	var
	matchItems := Matches.Items;
	try
		fn := matchItems[_index].Columns.Items[0].Text;
		if matchItems[_index].IsError then begin
			_item.Caption := ' ' + fn;
			_item.ImageIndex := LV_IMG_IDX_ERROR;
		end else begin
			_item.Caption := fn;
			_item.ImageIndex := LV_IMG_IDX_OK;
		end;
		_item.SubItems.Add(matchItems[_index].Columns.Items[1].Text);
		_item.SubItems.Add(matchItems[_index].Columns.Items[2].Text);
		_item.SubItems.Add(matchItems[_index].Columns.Items[3].Text);
	finally
		{$IFDEF THREADSAFE_LIST}
		Matches.Unlock;
		{$ENDIF}
	end;
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
	inherited;
end;

constructor THistoryItemObject.Create;
begin
	inherited;
	FMatches := TParsedObjectRowCollection.Create();
	FRipGrepArguments := TStringList.Create;
    FParserType := ptEmpty;
	ClearMatches;
end;

procedure THistoryItemObject.ClearMatches;
begin
	FFileCount := 0;
	FTotalMatchCount := 0;
	FErrorCount := 0;
	FMatches.Items.Clear;
	{$IFDEF THREADSAFE_LIST}
	FMatches.Unlock;
	{$ENDIF}
end;

procedure THistoryItemObject.CopyToSettings(const _settings : TRipGrepperSettings);
begin
	_settings.RipGrepParameters.RipGrepArguments.Assign(RipGrepArguments);
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

function THistoryItemObject.UpdateParserType: TParserType;
begin
	FParserType := TRipGrepperParsersFactory.TryGetParserType(
		TArrayEx<string>.Create(RipGrepArguments.GetValues()));
	Result := FParserType;
end;

end.
