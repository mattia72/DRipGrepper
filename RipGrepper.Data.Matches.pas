unit RipGrepper.Data.Matches;

interface

uses
	System.Generics.Collections,
	System.Classes,
	RipGrepper.Common.Types,
	RipGrepper.Common.Interfaces,
	System.RegularExpressions,
	Vcl.ComCtrls;

type

	TRipGrepperMatchCollection = TList<IRipGrepMatchLineGroup>;

	TSortByType = (sbtFile, sbtRow, sbtCol, sbtText);

	TRipGrepOutput = class
		Matches : TRipGrepperMatchCollection;
		MatchFiles : TStringList;
		ItemGroups : TStringList;
		SortedBy : TList<TSortByType>;

		private
			FGrouping : Boolean;
			function GetTotalMatchCount : Integer;
			function GetFileCount : Integer;
			procedure PutIntoGroup(const _idx : Integer; _lv : TListView; _item : TListItem);

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

constructor TRipGrepOutput.Create;
begin
	inherited;
	ItemGroups := TStringList.Create(TDuplicates.dupIgnore, True, True);
	MatchFiles := TStringList.Create(TDuplicates.dupIgnore, True, True);
	Matches := TRipGrepperMatchCollection.Create;
	SortedBy := TList<TSortByType>.Create;
end;

destructor TRipGrepOutput.Destroy;
begin
	inherited;
	ItemGroups.Free;
	MatchFiles.Free;
	Matches.Free;
	SortedBy.Free;
end;

procedure TRipGrepOutput.Add(const _item : IRipGrepMatchLineGroup);
begin
	Matches.Add(_item);
	MatchFiles.Add(_item.FileName);
end;

procedure TRipGrepOutput.Clear;
begin
	Matches.Clear;
	ItemGroups.Clear;
	MatchFiles.Clear;
end;

procedure TRipGrepOutput.DataToGrid(const _index : Integer; _lv : TListView; _item : TListItem);
var
	fn : string;
begin
	PutIntoGroup(_index, _lv, _item);
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

function TRipGrepOutput.GetTotalMatchCount : Integer;
begin
	Result := Matches.Count;
end;

function TRipGrepOutput.GetFileCount : Integer;
begin
	Result := MatchFiles.Count;
end;

procedure TRipGrepOutput.PutIntoGroup(const _idx : Integer; _lv : TListView; _item : TListItem);
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

procedure TRipGrepOutput.SortBy(const _sbt : TSortByType; const _st : TSortDirectionType);
begin
	if _st <> stUnsorted then begin
		case _sbt of
			sbtFile : begin
				SortByFileName(_st = stDescending);
			end;
			sbtRow : begin
				SortByRow(_st = stDescending);
			end;
		end;
	end else begin
		SortByLineNr(_st = stDescending);
		SortedBy.Remove(_sbt);
	end;
end;

procedure TRipGrepOutput.SortByFileName(_bDescending : Boolean = False);
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
	SortedBy.Add(sbtFile);
end;

procedure TRipGrepOutput.SortByRow(_bDescending : Boolean = False);
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
	SortedBy.Add(sbtRow);
end;

procedure TRipGrepOutput.SortByLineNr(_bDescending : Boolean = False);
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
	SortedBy.Clear;
end;

end.
