unit RipGrepper.Data.HistoryItemObject;

interface

uses
	RipGrepper.Common.Interfaces,
	ArrayHelper,
	RipGrepper.Common.Settings,
	Vcl.ComCtrls,
	System.Generics.Defaults;

type
	THistoryItemObject = class(TSingletonImplementation, IHistoryItem)
		private
			FFileCount : integer;
			FMatches : TRipGrepMatchLineCollection;
			FRipGrepArguments : TArrayRecord<string>;
			FTotalMatchCount : integer;
			function GetFileCount : integer;
			function GetMatches : TRipGrepMatchLineCollection;
			function GetRipGrepArguments : TArrayRecord<string>;
			function GetTotalMatchCount : integer;
			procedure SetFileCount(const Value : integer);
			procedure SetMatches(const Value : TRipGrepMatchLineCollection);
			procedure SetRipGrepArguments(const Value : TArrayRecord<string>);
			procedure SetTotalMatchCount(const Value : integer);

		public
			procedure CopySettings(const _settings : TRipGrepperSettings);
			procedure DataToGrid(_lv : TListView; _item : TListItem; const _index : Integer);
			destructor Destroy; override;
			constructor Create;
			procedure ClearMatches;
			property FileCount : integer read GetFileCount write SetFileCount;
			property Matches : TRipGrepMatchLineCollection read GetMatches write SetMatches;
			property RipGrepArguments : TArrayRecord<string> read GetRipGrepArguments write SetRipGrepArguments;
			property TotalMatchCount : integer read GetTotalMatchCount write SetTotalMatchCount;
	end;

	PHistoryItemObject = ^THistoryItemObject;

implementation

uses
	RipGrepper.Common.Types,
	System.SysUtils;

procedure THistoryItemObject.CopySettings(const _settings : TRipGrepperSettings);
begin
	RipGrepArguments.AddRange(_settings.RipGrepArguments.ToStringArray());
end;

procedure THistoryItemObject.DataToGrid(_lv : TListView; _item : TListItem; const _index : Integer);
var
	fn : string;
begin
	// (_index, _lv, _item);
	fn := Matches[_index].FileName;
	if Matches[_index].IsError then begin
		_item.Caption := ' ' + fn;
		_item.ImageIndex := LV_IMAGE_IDX_ERROR;
	end else begin
		_item.Caption := fn;
		_item.ImageIndex := LV_IMAGE_IDX_OK;
	end;
	_item.SubItems.Add(Matches[_index].Row.ToString);
	_item.SubItems.Add(Matches[_index].Col.ToString);
	_item.SubItems.Add(Matches[_index].Text);
end;

function THistoryItemObject.GetFileCount : integer;
begin
	Result := FFileCount;
end;

function THistoryItemObject.GetMatches : TRipGrepMatchLineCollection;
begin
	Result := FMatches;
end;

function THistoryItemObject.GetRipGrepArguments : TArrayRecord<string>;
begin
	Result := FRipGrepArguments;
end;

function THistoryItemObject.GetTotalMatchCount : integer;
begin
	Result := FMatches.Count;
end;

procedure THistoryItemObject.SetFileCount(const Value : integer);
begin
	FFileCount := Value;
end;

procedure THistoryItemObject.SetMatches(const Value : TRipGrepMatchLineCollection);
begin
	FMatches := Value;
end;

procedure THistoryItemObject.SetRipGrepArguments(const Value : TArrayRecord<string>);
begin
	FRipGrepArguments := Value;
end;

procedure THistoryItemObject.SetTotalMatchCount(const Value : integer);
begin
	FTotalMatchCount := Value;
end;

destructor THistoryItemObject.Destroy;
begin
	FMatches.Free;
	inherited;
end;

constructor THistoryItemObject.Create;
begin
	inherited;
	FMatches := TRipGrepMatchLineCollection.Create();
	FRipGrepArguments.Clear;  //Creates an Array :/
	ClearMatches;
end;

procedure THistoryItemObject.ClearMatches;
begin
	FFileCount := 0;
	FMatches.Clear;
	FTotalMatchCount := 0;
end;

end.
