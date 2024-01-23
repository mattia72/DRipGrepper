unit RipGrepper.Data.HistoryItemObject;

interface

uses
	RipGrepper.Common.Interfaces,
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
			FMatches : TRipGrepMatchLineCollection;
			FRipGrepArguments : TStringList;
			FTotalMatchCount: integer;
			function GetErrorCount: Integer; export;
			function GetFileCount : integer;
			function GetMatches : TRipGrepMatchLineCollection;
			function GetRipGrepArguments : TStringList;
			function GetTotalMatchCount: integer;
			procedure SetFileCount(const Value : integer);
			procedure SetMatches(const Value : TRipGrepMatchLineCollection);
			procedure SetRipGrepArguments(const Value : TStringList);
		public
			procedure CopyFromSettings(const _settings : TRipGrepperSettingsHistory);
			procedure DataToGrid(_lv : TListView; _item : TListItem; const _index : Integer);
			destructor Destroy; override;
			constructor Create;
			procedure ClearMatches;
			procedure CopyToSettings(const _settings : TRipGrepperSettingsHistory);
			property FileCount : integer read GetFileCount write SetFileCount;
			property Matches : TRipGrepMatchLineCollection read GetMatches write SetMatches;
			property RipGrepArguments : TStringList read GetRipGrepArguments write SetRipGrepArguments;
			property TotalMatchCount: integer read GetTotalMatchCount;
			property ErrorCount: Integer read GetErrorCount write FErrorCount;
	end;

	PHistoryItemObject = ^THistoryItemObject;

implementation

uses
	RipGrepper.Common.Types,
	System.SysUtils;

procedure THistoryItemObject.CopyFromSettings(const _settings : TRipGrepperSettingsHistory);
begin
	RipGrepArguments.Assign(_settings.GetRipGrepArguments);
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

function THistoryItemObject.GetRipGrepArguments : TStringList;
begin
	Result := FRipGrepArguments;
end;

function THistoryItemObject.GetTotalMatchCount: integer;
begin
	Result := FMatches.Count - ErrorCount ;
end;

procedure THistoryItemObject.SetFileCount(const Value : integer);
begin
	FFileCount := Value;
end;

procedure THistoryItemObject.SetMatches(const Value : TRipGrepMatchLineCollection);
begin
	FMatches := Value;
end;

procedure THistoryItemObject.SetRipGrepArguments(const Value : TStringList);
begin
	FRipGrepArguments := Value;
end;

destructor THistoryItemObject.Destroy;
begin
	FMatches.Free;
    FRipGrepArguments.Free;
	inherited;
end;

constructor THistoryItemObject.Create;
begin
	inherited;
	FMatches := TRipGrepMatchLineCollection.Create();
	FRipGrepArguments := TStringList.Create;
	ClearMatches;
end;

procedure THistoryItemObject.ClearMatches;
begin
	FFileCount := 0;
	FMatches.Clear;
	FTotalMatchCount := 0;
    FErrorCount := 0;
end;

procedure THistoryItemObject.CopyToSettings(const _settings : TRipGrepperSettingsHistory);
begin
	_settings.RipGrepParameters.RipGrepArguments.Assign(RipGrepArguments);
end;

function THistoryItemObject.GetErrorCount: Integer;
begin
    Result := FErrorCount;
end;

end.
