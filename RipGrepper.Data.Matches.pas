unit RipGrepper.Data.Matches;

interface

uses
	System.Generics.Collections,
	System.Classes,
	RipGrepper.Common.Types,
	RipGrepper.Common.Interfaces,
	System.RegularExpressions,
	Vcl.ComCtrls,
	ArrayHelper,
	System.Generics.Defaults,
	RipGrepper.Common.Sorter;

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
			function GetComparer(const _sbt : TSortByType) : IComparer<IRipGrepMatchLineGroup>;
			procedure SortMultiColumns(const _st : TSortDirectionType);

		public
			constructor Create;
			destructor Destroy; override;
			procedure Add(const _item : IRipGrepMatchLineGroup);
			procedure Clear;
			procedure DataToGrid(const _index : Integer; _lv : TListView; _item : TListItem);
			procedure SortBy(const _sbt : TSortByType; const _st : TSortDirectionType);
			property TotalMatchCount : Integer read GetTotalMatchCount;
			property FileCount : Integer read GetFileCount;
			property Grouping : Boolean read FGrouping write FGrouping;
	end;

implementation

uses
	System.SysUtils,

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
		SortedBy.MoveToStart(_sbt, _st);
		SortMultiColumns(_st);
	end else begin
		SortedBy.Delete(_sbt);
		SortMultiColumns(_st);
	end;
end;

function TRipGrepperData.GetComparer(const _sbt : TSortByType) : IComparer<IRipGrepMatchLineGroup>;
begin
	case _sbt of
		sbtText : begin
			Result := TComparer<IRipGrepMatchLineGroup>.Construct(
				function(const Left, Right : IRipGrepMatchLineGroup) : Integer
				begin
					Result := TComparer<string>.Default.Compare(Left.Text, Right.Text);
				end);
		end;
		sbtFile : begin
			Result := TComparer<IRipGrepMatchLineGroup>.Construct(
				function(const Left, Right : IRipGrepMatchLineGroup) : Integer
				begin
					Result := TComparer<string>.Default.Compare(Left.FileName, Right.FileName);
				end);
		end;
		sbtRow : begin
			Result := TComparer<IRipGrepMatchLineGroup>.Construct(
				function(const Left, Right : IRipGrepMatchLineGroup) : Integer
				begin
					Result := TComparer<integer>.Default.Compare(Left.Row, Right.Row);
				end);
		end;
		sbtCol : begin
			Result := TComparer<IRipGrepMatchLineGroup>.Construct(
				function(const Left, Right : IRipGrepMatchLineGroup) : Integer
				begin
					Result := TComparer<integer>.Default.Compare(Left.Col, Right.Col);
				end);
		end;
		sbtLineNr : begin
			Result := TComparer<IRipGrepMatchLineGroup>.Construct(
				function(const Left, Right : IRipGrepMatchLineGroup) : Integer
				begin
					Result := TComparer<integer>.Default.Compare(Left.LineNr, Right.LineNr);
				end);
		end;
	end;
end;

procedure TRipGrepperData.SortMultiColumns(const _st : TSortDirectionType);
var
	criterion : TSortCriterion<IRipGrepMatchLineGroup>;
	lineComparer : TSortCriteriaComparer<IRipGrepMatchLineGroup>;
begin
	lineComparer := TSortCriteriaComparer<IRipGrepMatchLineGroup>.Create;
	try
		if (SortedBy.Items.Count > 0) then begin
			for var i := 0 to SortedBy.Items.Count - 1 do begin
				criterion := TSortCriterion<IRipGrepMatchLineGroup>.Create;
				criterion.Ascending := _st = stAscending;
				criterion.Comparer := GetComparer(SortedBy.Items[i].Column);
				lineComparer.AddCriterion(criterion);
			end;
		end else begin
			criterion := TSortCriterion<IRipGrepMatchLineGroup>.Create;
			criterion.Ascending := _st = stAscending;
			criterion.Comparer := GetComparer(sbtLineNr);
			lineComparer.AddCriterion(criterion);
		end;
		Matches.Sort(lineComparer);
	finally
		lineComparer.Free;
	end;
end;

end.
