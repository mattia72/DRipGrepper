unit RipGrepper.Data.HistoryItemObject;

interface

uses
	RipGrepper.Common.Interfaces,
	ArrayHelper,
	RipGrepper.Common.Settings,
	Vcl.ComCtrls;

type
	THistoryItemObject = record // class(TInterfacedObject, IHistoryItem)
		private
			FFileCount : integer;
			FPMatches : PRipGrepMatchLineCollection;
			FRipGrepArguments : TArrayRecord<string>;
			FTotalMatchCount : integer;
			function GetFileCount : integer;
			function GetPMatches : PRipGrepMatchLineCollection;
			function GetRipGrepArguments : TArrayRecord<string>;
			function GetTotalMatchCount : integer;
			procedure SetFileCount(const Value : integer);
			procedure SetPMatches(const Value : PRipGrepMatchLineCollection);
			procedure SetRipGrepArguments(const Value : TArrayRecord<string>);
			procedure SetTotalMatchCount(const Value : integer);

		public
			procedure CopySettings(const _settings : TRipGrepperSettings);
			procedure DataToGrid(_lv : TListView; _item : TListItem; const _index : Integer);
			class operator Finalize(var Dest : THistoryItemObject);
			class operator Initialize(out Dest : THistoryItemObject);
			property FileCount : integer read GetFileCount write SetFileCount;
			property PMatches : PRipGrepMatchLineCollection read GetPMatches write SetPMatches;
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
	fn := PMatches^[_index].FileName;
	if PMatches^[_index].IsError then begin
		_item.Caption := ' ' + fn;
		_item.ImageIndex := LV_IMAGE_IDX_ERROR;
	end else begin
		_item.Caption := fn;
		_item.ImageIndex := LV_IMAGE_IDX_OK;
	end;
	_item.SubItems.Add(PMatches^[_index].Row.ToString);
	_item.SubItems.Add(PMatches^[_index].Col.ToString);
	_item.SubItems.Add(PMatches^[_index].Text);
end;

function THistoryItemObject.GetFileCount : integer;
begin
	Result := FFileCount;
end;

function THistoryItemObject.GetPMatches : PRipGrepMatchLineCollection;
begin
	Result := FPMatches;
end;

function THistoryItemObject.GetRipGrepArguments : TArrayRecord<string>;
begin
	Result := FRipGrepArguments;
end;

function THistoryItemObject.GetTotalMatchCount : integer;
begin
	Result := FPMatches^.Count;
end;

procedure THistoryItemObject.SetFileCount(const Value : integer);
begin
	FFileCount := Value;
end;

procedure THistoryItemObject.SetPMatches(const Value : PRipGrepMatchLineCollection);
begin
	FPMatches := Value;
end;

procedure THistoryItemObject.SetRipGrepArguments(const Value : TArrayRecord<string>);
begin
	FRipGrepArguments := Value;
end;

procedure THistoryItemObject.SetTotalMatchCount(const Value : integer);
begin
	FTotalMatchCount := Value;
end;

class operator THistoryItemObject.Finalize(var Dest : THistoryItemObject);
begin
	Dest.FPMatches^.Clear;
	Dispose(Dest.FPMatches);
end;

class operator THistoryItemObject.Initialize(out Dest : THistoryItemObject);
begin
	Dest.FFileCount := 0;
	new(Dest.FPMatches);
	Dest.FPMatches^.Clear;
	Dest.FTotalMatchCount := 0;
	Dest.FRipGrepArguments.Clear;
end;

end.
