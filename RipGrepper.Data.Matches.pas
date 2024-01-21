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
	RipGrepper.Common.Sorter,
	RipGrepper.Data.HistoryItemObject;

type

	TRipGrepperData = class
		PHistObject : PHistoryItemObject;
		MatchFiles : TStringList;
		SortedBy : TSortTypeDirectionList;

		private
			function GetTotalMatchCount : Integer;
			function GetFileCount : Integer;
			function GetComparer(const _sbt : TSortByType) : IComparer<IRipGrepMatchLine>;
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
	MatchFiles := TStringList.Create(TDuplicates.dupIgnore, True, True);
end;

destructor TRipGrepperData.Destroy;
begin
	inherited;
	MatchFiles.Free;
end;

procedure TRipGrepperData.Add(const _item : IRipGrepMatchLineGroup);
begin
	PHistObject^.PMatches.Add(_item);
	MatchFiles.Add(_item.FileName);
end;

procedure TRipGrepperData.Clear;
begin
	if PHistObject <> nil then begin
		PHistObject^.PMatches.Clear; // Create Array :/
	end;
	MatchFiles.Clear;
end;

procedure TRipGrepperData.DataToGrid(const _index : Integer; _lv : TListView; _item : TListItem);
var
	fn : string;
begin
	// (_index, _lv, _item);
	fn := PHistObject.PMatches^[_index].FileName;
	if PHistObject.PMatches^[_index].IsError then begin
		_item.Caption := ' ' + fn;
		_item.ImageIndex := LV_IMAGE_IDX_ERROR;
	end else begin
		_item.Caption := fn;
		_item.ImageIndex := LV_IMAGE_IDX_OK;
	end;
	_item.SubItems.Add(PHistObject.PMatches^[_index].Row.ToString);
	_item.SubItems.Add(PHistObject.PMatches^[_index].Col.ToString);
	_item.SubItems.Add(PHistObject.PMatches^[_index].Text);
end;

function TRipGrepperData.GetTotalMatchCount : Integer;
begin
	Result := PHistObject^.PMatches^.Count;
end;

function TRipGrepperData.GetFileCount : Integer;
begin
	Result := MatchFiles.Count;
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

function TRipGrepperData.GetComparer(const _sbt : TSortByType) : IComparer<IRipGrepMatchLine>;
begin
	case _sbt of
		sbtText : begin
			Result := TComparer<IRipGrepMatchLine>.Construct(
				function(const Left, Right : IRipGrepMatchLine) : Integer
				begin
					Result := TComparer<string>.Default.Compare(Left.Text, Right.Text);
				end);
		end;
		sbtFile : begin
			Result := TComparer<IRipGrepMatchLine>.Construct(
				function(const Left, Right : IRipGrepMatchLine) : Integer
				begin
					Result := TComparer<string>.Default.Compare(Left.FileName, Right.FileName);
				end);
		end;
		sbtRow : begin
			Result := TComparer<IRipGrepMatchLine>.Construct(
				function(const Left, Right : IRipGrepMatchLine) : Integer
				begin
					Result := TComparer<integer>.Default.Compare(Left.Row, Right.Row);
				end);
		end;
		sbtCol : begin
			Result := TComparer<IRipGrepMatchLine>.Construct(
				function(const Left, Right : IRipGrepMatchLine) : Integer
				begin
					Result := TComparer<integer>.Default.Compare(Left.Col, Right.Col);
				end);
		end;
		sbtLineNr : begin
			Result := TComparer<IRipGrepMatchLine>.Construct(
				function(const Left, Right : IRipGrepMatchLine) : Integer
				begin
					Result := TComparer<integer>.Default.Compare(Left.LineNr, Right.LineNr);
				end);
		end;
	end;
end;

procedure TRipGrepperData.SortMultiColumns(const _st : TSortDirectionType);
var
	criterion : TSortCriterion<IRipGrepMatchLine>;
	lineComparer : TSortCriteriaComparer<IRipGrepMatchLine>;
begin
	lineComparer := TSortCriteriaComparer<IRipGrepMatchLine>.Create;
	try
		if (SortedBy.Items.Count > 0) then begin
			for var i := 0 to SortedBy.Items.Count - 1 do begin
				criterion := TSortCriterion<IRipGrepMatchLine>.Create;
				criterion.Ascending := _st = stAscending;
				criterion.Comparer := GetComparer(SortedBy.Items[i].Column);
				lineComparer.AddCriterion(criterion);
			end;
		end else begin
			criterion := TSortCriterion<IRipGrepMatchLine>.Create;
			criterion.Ascending := _st = stAscending;
			criterion.Comparer := GetComparer(sbtLineNr);
			lineComparer.AddCriterion(criterion);
		end;
		PHistObject^.PMatches^.Sort(lineComparer);
	finally
		lineComparer.Free;
	end;
end;

end.
