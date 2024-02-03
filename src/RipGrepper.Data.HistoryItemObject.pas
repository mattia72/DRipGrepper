unit RipGrepper.Data.HistoryItemObject;

interface

uses
	RipGrepper.Common.Interfaces,
	RipGrepper.Common.ParsedObject,
	ArrayHelper,
	RipGrepper.Common.Settings,
	Vcl.ComCtrls,
	System.Generics.Defaults,
	System.Classes;

type
	THistoryItemObject = class(TSingletonImplementation, IHistoryItem)
		private
			FErrorCount: Integer;
			FFileCount : integer;
			FMatches : IParsedObjectRowCollection;
			FRipGrepArguments : TStringList;
			FTotalMatchCount: integer;
			function GetErrorCount: Integer; export;
			function GetFileCount : integer;
			function GetMatches: IParsedObjectRowCollection;
			function GetRipGrepArguments : TStringList;
			function GetTotalMatchCount: integer;
			procedure SetFileCount(const Value : integer);
			procedure SetMatches(const Value: IParsedObjectRowCollection);
			procedure SetRipGrepArguments(const Value : TStringList);
		public
			procedure CopyFromSettings(const _settings : TRipGrepperSettings);
			procedure DataToGrid(_lv : TListView; _item : TListItem; const _index : Integer);
			destructor Destroy; override;
			constructor Create;
			procedure ClearMatches;
			procedure CopyToSettings(const _settings : TRipGrepperSettings);
			property FileCount : integer read GetFileCount write SetFileCount;
			property Matches: IParsedObjectRowCollection read GetMatches write SetMatches;
			property RipGrepArguments : TStringList read GetRipGrepArguments write SetRipGrepArguments;
			property TotalMatchCount: integer read GetTotalMatchCount;
			property ErrorCount: Integer read GetErrorCount write FErrorCount;
	end;

	PHistoryItemObject = ^THistoryItemObject;

implementation

uses
	RipGrepper.Common.Types,
	System.SysUtils;

procedure THistoryItemObject.CopyFromSettings(const _settings : TRipGrepperSettings);
begin
	RipGrepArguments.Assign(_settings.GetRipGrepArguments);
end;

procedure THistoryItemObject.DataToGrid(_lv : TListView; _item : TListItem; const _index : Integer);
var
	fn : string;
begin
	// (_index, _lv, _item);
	fn := Matches.Items[_index].Columns.Items[0].Text;
	if Matches.Items[_index].IsError then begin
		_item.Caption := ' ' + fn;
		_item.ImageIndex := LV_IMAGE_IDX_ERROR;
	end else begin
		_item.Caption := fn;
		_item.ImageIndex := LV_IMAGE_IDX_OK;
	end;
	_item.SubItems.Add(Matches.Items[_index].Columns.Items[1].Text);
	_item.SubItems.Add(Matches.Items[_index].Columns.Items[2].Text);
	_item.SubItems.Add(Matches.Items[_index].Columns.Items[3].Text);
end;

function THistoryItemObject.GetFileCount : integer;
begin
	Result := FFileCount;
end;

function THistoryItemObject.GetMatches: IParsedObjectRowCollection;
begin
	Result := FMatches;
end;

function THistoryItemObject.GetRipGrepArguments : TStringList;
begin
	Result := FRipGrepArguments;
end;

function THistoryItemObject.GetTotalMatchCount: integer;
begin
	Result := FMatches.Items.Count - ErrorCount ;
end;

procedure THistoryItemObject.SetFileCount(const Value : integer);
begin
	FFileCount := Value;
end;

procedure THistoryItemObject.SetMatches(const Value: IParsedObjectRowCollection);
begin
	FMatches := Value;
end;

procedure THistoryItemObject.SetRipGrepArguments(const Value : TStringList);
begin
	FRipGrepArguments := Value;
end;

destructor THistoryItemObject.Destroy;
begin
	//FMatches.Free;
    FRipGrepArguments.Free;
	inherited;
end;

constructor THistoryItemObject.Create;
begin
	inherited;
	FMatches := TParsedObjectRowCollection.Create();
	FRipGrepArguments := TStringList.Create;
	ClearMatches;
end;

procedure THistoryItemObject.ClearMatches;
begin
	FFileCount := 0;
	FMatches.Items.Clear;
	FTotalMatchCount := 0;
    FErrorCount := 0;
end;

procedure THistoryItemObject.CopyToSettings(const _settings : TRipGrepperSettings);
begin
	_settings.RipGrepParameters.RipGrepArguments.Assign(RipGrepArguments);
end;

function THistoryItemObject.GetErrorCount: Integer;
begin
    Result := FErrorCount;
end;

end.
