unit RipGrepper.Data.Matches;

interface

uses
	System.Generics.Collections,
	System.Classes,
	RipGrepper.Common.Types,
	RipGrepper.Common.Interfaces,
	System.RegularExpressions,
	Vcl.ComCtrls,
	ArrayEx,
	System.Generics.Defaults,
	RipGrepper.Common.Sorter,
	RipGrepper.Data.HistoryItemObject,
	RipGrepper.Common.ParsedObject,
	VirtualTrees;

type

	TRipGrepperData = class
		MatchFiles : TStringList;
		SortedBy : TSortTypeDirectionList;

		private
			FErrorCount : Integer;
			FHistObject : THistoryItemObject;
			FVst : TCustomVirtualStringTree;
			function AddVSTStructure(_node : PVirtualNode; _rec : TVSFileNodeData) : PVirtualNode;
			function GetTotalMatchCount : Integer;
			function GetFileCount : Integer;
			function GetComparer(const _sbt : TSortByType) : IComparer<IParsedObjectRow>;
			function GetErrorCount : Integer;
			function GetHistObject : THistoryItemObject;
			function GetListItemCount : Integer;
			procedure SetHistObject(const Value : THistoryItemObject);
			procedure SortMultiColumns(const _st : TSortDirectionType);

		public
			constructor Create(_vst : TCustomVirtualStringTree);
			destructor Destroy; override;
			procedure Add(_item : IParsedObjectRow);
			procedure ClearMatchFiles;
			procedure DataToGrid(const _index : Integer; _lv : TListView; _item : TListItem);
			procedure SortBy(const _sbt : TSortByType; const _st : TSortDirectionType);
			property ErrorCount : Integer read GetErrorCount;
			property TotalMatchCount : Integer read GetTotalMatchCount;
			property FileCount : Integer read GetFileCount;
			property HistObject : THistoryItemObject read GetHistObject write SetHistObject;
			property ListItemCount : Integer read GetListItemCount;
	end;

implementation

uses
	System.SysUtils,
	System.IOUtils,
	Vcl.Dialogs,
	RipGrepper.Tools.DebugTools,
	RipGrepper.Helper.Types;

constructor TRipGrepperData.Create(_vst : TCustomVirtualStringTree);
begin
	inherited Create();
	MatchFiles := TStringList.Create(TDuplicates.dupIgnore, True, True);
	FErrorCount := 0;
	FVst := _vst;
end;

destructor TRipGrepperData.Destroy;
begin
	MatchFiles.Free;
	inherited;
end;

procedure TRipGrepperData.Add(_item : IParsedObjectRow);
var
	vstNode : TVSFileNodeData;
	node : PVirtualNode;
	sFile : string;
begin
	FVst.BeginUpdate;
	try
		HistObject.Matches.Items.Add(_item);
		if (_item.IsError) then begin
			Inc(FErrorCount);
			Exit
		end else begin
			sFile := _item.Columns[0].Text;
			var
			idx := MatchFiles.IndexOf(sFile);
			if idx < 0 then begin
				vstNode := TVSFileNodeData.New(_item.Columns[0].Text);
				node := AddVSTStructure(nil, vstNode);
				MatchFiles.AddObject(sFile, TObject(node));
			end else begin
				node := PVirtualNode(MatchFiles.Objects[idx]);
			end
		end;

		vstNode := TVSFileNodeData.New('',
			{ } StrToIntDef(_item.Columns[1].Text, -1),
			{ } StrToIntDef(_item.Columns[2].Text, -1),
			{ } _item.Columns[3].Text);
		AddVSTStructure(node, vstNode);
	finally
		FVst.EndUpdate;
	end;

	{$IFDEF THREADSAFE_LIST}
	HistObject.Matches.Unlock;
	{$ENDIF}
end;

function TRipGrepperData.AddVSTStructure(_node : PVirtualNode; _rec : TVSFileNodeData) : PVirtualNode;
var
	Data : PVSFileNodeData;
begin
	Result := FVst.AddChild(_node);
	Data := FVst.GetNodeData(Result);
	// FVst.ValidateNode(Result, False);
	Data^.FilePath := _rec.FilePath;
	Data^.MatchData := _rec.MatchData;
end;

procedure TRipGrepperData.ClearMatchFiles;
begin
	MatchFiles.Clear;
	FErrorCount := 0;
end;

procedure TRipGrepperData.DataToGrid(const _index : Integer; _lv : TListView; _item : TListItem);
var
	fn : string;
begin
	// (_index, _lv, _item);
	var
	matchItems := HistObject.Matches.Items;
	try
		fn := matchItems[_index].Columns[0].Text;
		if matchItems[_index].IsError then begin
			_item.Caption := ' ' + fn;
			_item.ImageIndex := LV_IMAGE_IDX_ERROR;
		end else begin
			_item.Caption := fn;
			_item.ImageIndex := LV_IMAGE_IDX_OK;
		end;
		_item.SubItems.Add(matchItems[_index].Columns[1].Text);
		_item.SubItems.Add(matchItems[_index].Columns[2].Text);
		_item.SubItems.Add(matchItems[_index].Columns[3].Text);
	finally
		{$IFDEF THREADSAFE_LIST}
		HistObject.Matches.Unlock;
		{$ENDIF}
	end;
end;

function TRipGrepperData.GetTotalMatchCount : Integer;
begin
	Result := HistObject.Matches.Items.Count;
	{$IFDEF THREADSAFE_LIST}
	HistObject.Matches.Unlock;
	{$ENDIF}
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

function TRipGrepperData.GetComparer(const _sbt : TSortByType) : IComparer<IParsedObjectRow>;
begin
	case _sbt of
		sbtFile, sbtRow, sbtCol, sbtText : begin
			Result := TComparer<IParsedObjectRow>.Construct(
				function(const Left, Right : IParsedObjectRow) : Integer
				begin
					var
					idx := Integer(_sbt);
					Result := TComparer<string>.Default.Compare(Left.Columns[idx].Text, Right.Columns[idx].Text);
				end);
		end;
		sbtLineNr : begin
			Result := TComparer<IParsedObjectRow>.Construct(
				function(const Left, Right : IParsedObjectRow) : Integer
				begin
					Result := TComparer<integer>.Default.Compare(Left.RowNr, Right.RowNr);
				end);
		end;
	end;
end;

function TRipGrepperData.GetErrorCount : Integer;
begin
	Result := FErrorCount;
end;

function TRipGrepperData.GetHistObject : THistoryItemObject;
begin
	Result := FHistObject;
end;

function TRipGrepperData.GetListItemCount : Integer;
begin
	Result := TotalMatchCount + ErrorCount;
end;

procedure TRipGrepperData.SetHistObject(const Value : THistoryItemObject);
begin
	FHistObject := Value;
end;

procedure TRipGrepperData.SortMultiColumns(const _st : TSortDirectionType);
var
	criterion : TSortCriterion<IParsedObjectRow>;
	lineComparer : TSortCriteriaComparer<IParsedObjectRow>;
begin
	lineComparer := TSortCriteriaComparer<IParsedObjectRow>.Create;
	try
		if (SortedBy.Items.Count > 0) then begin
			for var i := 0 to SortedBy.Items.Count - 1 do begin
				criterion := TSortCriterion<IParsedObjectRow>.Create;
				criterion.Ascending := _st = stAscending;
				criterion.Comparer := GetComparer(SortedBy.Items[i].Column);
				lineComparer.AddCriterion(criterion);
			end;
		end else begin
			criterion := TSortCriterion<IParsedObjectRow>.Create;
			criterion.Ascending := _st = stAscending;
			criterion.Comparer := GetComparer(sbtLineNr);
			lineComparer.AddCriterion(criterion);
		end;

		if Assigned(HistObject) then begin
			var
			matchItems := HistObject.Matches.Items;
			try
				matchItems.Sort(lineComparer);
			finally
				{$IFDEF THREADSAFE_LIST}
				HistObject.Matches.Unlock;
				{$ENDIF}
			end;
		end;
	finally
		lineComparer.Free;
	end;
end;

end.
