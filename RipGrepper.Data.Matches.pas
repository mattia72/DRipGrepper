unit RipGrepper.Data.Matches;

interface

uses
	System.Generics.Collections,
	System.Classes,
	RipGrepper.Common.Types,
	RipGrepper.Common.Interfaces,
	System.RegularExpressions,
	Vcl.ComCtrls,
	ArrayHelper;

type

	TRipGrepperMatchCollection = TList<IRipGrepMatchLineGroup>;

	TRipGrepperData = class
		Matches : TRipGrepperMatchCollection;
		MatchFiles : TStringList;
		ItemGroups : TStringList;
		SortedBy : TSortTypeDirectionList;

		private
			FGrouping : Boolean;
			function GetTotalMatchCount : Integer;
			function GetFileCount : Integer;
			procedure PutIntoGroup(const _idx : Integer; _lv : TListView; _item : TListItem);
			procedure SortByType(const _sbt : TSortByType; const _st : TSortDirectionType);

		public
			constructor Create;
			destructor Destroy; override;
			procedure Add(const _item : IRipGrepMatchLineGroup);
			procedure Clear;
			procedure DataToGrid(const _index : Integer; _lv : TListView; _item : TListItem);
			procedure SortBy(const _sbt : TSortByType; const _st : TSortDirectionType);
			procedure SortByFileName(_bDescending : Boolean = False);
			procedure SortByRow(_bDescending : Boolean = False);
			procedure SortByLineNr(_bDescending : Boolean = False);
			property TotalMatchCount : Integer read GetTotalMatchCount;
			property FileCount : Integer read GetFileCount;
			property Grouping : Boolean read FGrouping write FGrouping;
	end;

implementation

uses
	System.SysUtils,
	System.Generics.Defaults,
	System.IOUtils,
	Vcl.Dialogs,
	RipGrepper.Tools.DebugTools,
	RipGrepper.Helper.Types;

constructor TRipGrepperData.Create;
begin
	inherited;
	ItemGroups := TStringList.Create(TDuplicates.dupIgnore, True, True);
	MatchFiles := TStringList.Create(TDuplicates.dupIgnore, True, True);
	Matches := TRipGrepperMatchCollection.Create;
end;

destructor TRipGrepperData.Destroy;
begin
	inherited;
	ItemGroups.Free;
	MatchFiles.Free;
	Matches.Free;
end;

procedure TRipGrepperData.Add(const _item : IRipGrepMatchLineGroup);
begin
	Matches.Add(_item);
	MatchFiles.Add(_item.FileName);
end;

procedure TRipGrepperData.Clear;
begin
	Matches.Clear;
	ItemGroups.Clear;
	MatchFiles.Clear;
end;

procedure TRipGrepperData.DataToGrid(const _index : Integer; _lv : TListView; _item : TListItem);
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

function TRipGrepperData.GetTotalMatchCount : Integer;
begin
	Result := Matches.Count;
end;

function TRipGrepperData.GetFileCount : Integer;
begin
	Result := MatchFiles.Count;
end;

procedure TRipGrepperData.PutIntoGroup(const _idx : Integer; _lv : TListView; _item : TListItem);
begin
	if not Grouping then
		Exit;

	if ItemGroups.Contains(Matches[_idx].FileName) then begin
		_item.GroupID := Matches[_idx].GroupID;
	end else begin
		var
		Group := _lv.Groups.Add;
		Group.State := [lgsNormal, lgsCollapsible];
		Group.Header := Matches[_idx].FileName;
		var
		match := Matches[_idx];
		match.GroupID := Group.GroupID;
		Matches[_idx] := match;
		ItemGroups.Add(Matches[_idx].FileName);
	end;
end;

procedure TRipGrepperData.SortBy(const _sbt : TSortByType; const _st : TSortDirectionType);
begin
	if _st <> stUnsorted then begin
		for var sbt in SortedBy.Items do begin
			if (sbt.SortType <> _sbt) then
				SortByType(sbt.SortType, sbt.Direction);
		end;
		SortByType(_sbt, _st);
	end else begin
		SortByLineNr(_st = stDescending);
		SortedBy.Delete(_sbt);
	end;
end;

procedure TRipGrepperData.SortByFileName(_bDescending : Boolean = False);
begin
	Matches.Sort(TComparer<IRipGrepMatchLineGroup>.Construct(
		function(const Left, Right : IRipGrepMatchLineGroup) : Integer
		begin
			if _bDescending then begin
				Result := -TComparer<string>.Default.Compare(Left.FileName, Right.FileName);
			end else begin
				Result := TComparer<string>.Default.Compare(Left.FileName, Right.FileName);
			end;
		end));
	SortedBy.Delete(sbtFile);
	SortedBy.Items.Add(TSortTypeDirection.New(sbtFile, _bDescending));
end;

procedure TRipGrepperData.SortByRow(_bDescending : Boolean = False);
begin
	Matches.Sort(TComparer<IRipGrepMatchLineGroup>.Construct(
		function(const Left, Right : IRipGrepMatchLineGroup) : Integer
		begin
			if _bDescending then begin
				Result := -TComparer<integer>.Default.Compare(Left.Row, Right.Row);
			end else begin
				Result := TComparer<integer>.Default.Compare(Left.Row, Right.Row);
			end;

		end));
	SortedBy.Delete(sbtRow);
	SortedBy.Items.Add(TSortTypeDirection.New(sbtRow, _bDescending));
end;

procedure TRipGrepperData.SortByLineNr(_bDescending : Boolean = False);
begin
	Matches.Sort(TComparer<IRipGrepMatchLineGroup>.Construct(
		function(const Left, Right : IRipGrepMatchLineGroup) : Integer
		begin
			if _bDescending then begin
				Result := -TComparer<integer>.Default.Compare(Left.LineNr, Right.LineNr);
			end else begin
				Result := TComparer<integer>.Default.Compare(Left.LineNr, Right.LineNr);
			end;
		end));
	SortedBy.Items.Clear;
end;

procedure TRipGrepperData.SortByType(const _sbt : TSortByType; const _st : TSortDirectionType);
begin
	case _sbt of
		sbtFile : begin
			SortByFileName(_st = stDescending);
		end;
		sbtRow : begin
			SortByRow(_st = stDescending);
		end;
	end;
end;

end.
