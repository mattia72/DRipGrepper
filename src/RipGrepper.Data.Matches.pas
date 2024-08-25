unit RipGrepper.Data.Matches;

interface

uses
	System.Generics.Collections,
	System.Classes,
	// RipGrepper.Common.Constants,
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
			FHistObject: IHistoryItemObject;
			FVst : TCustomVirtualStringTree;
			function AddVSTStructure(_node : PVirtualNode; _rec : TVSFileNodeData; _asFirst : Boolean) : PVirtualNode;
			function GetTotalMatchCount : Integer;
			function GetFileCount : Integer;
			function GetComparer(const _sbt : TSortByType) : IComparer<IParsedObjectRow>;
			function GetErrorCount : Integer;
			function GetHistObject: IHistoryItemObject;
			function GetListItemCount : Integer;
			function GetParentNode(const _sNodeText : string; _asFirst : Boolean = False) : PVirtualNode;
			procedure AddChildNode(const _parentNode : PVirtualNode; _item : IParsedObjectRow);
			function ErrorHandling(const _sFileColumnText : string; _item : IParsedObjectRow) : PVirtualNode;
			function GetNoMatchFound : Boolean;
			procedure SetHistObject(const Value: IHistoryItemObject);
			procedure SetNoMatchFound(const Value : Boolean);
			procedure SortMultiColumns(const _st : TSortDirectionType);

		public
			constructor Create(_vst : TCustomVirtualStringTree);
			destructor Destroy; override;
			procedure Add(_item : IParsedObjectRow);
			procedure ClearMatchFiles;
			procedure DataToGrid(const _index : Integer; _lv : TListView; _item : TListItem); overload;
			procedure DataToGrid; overload;
			procedure SortBy(const _sbt : TSortByType; const _st : TSortDirectionType);
			property ErrorCount : Integer read GetErrorCount;
			property TotalMatchCount : Integer read GetTotalMatchCount;
			property FileCount : Integer read GetFileCount;
			property HistObject: IHistoryItemObject read GetHistObject write SetHistObject;
			property ListItemCount : Integer read GetListItemCount;
			property NoMatchFound : Boolean read GetNoMatchFound write SetNoMatchFound;
	end;

implementation

uses
	System.SysUtils,
	System.IOUtils,
	Vcl.Dialogs,
	RipGrepper.Tools.DebugUtils,
	RipGrepper.Helper.Types,
	RipGrepper.Common.Constants;

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
	node : PVirtualNode;
	sFileColumnText : string;
begin
	if (_item.Columns.IsEmpty) then
		Exit;

	FVst.BeginUpdate;
	try
		HistObject.Matches.Items.Add(_item);
		sFileColumnText := _item.Columns[Integer(ciFile)].Text;
		if (_item.IsError) then begin
			node := ErrorHandling(sFileColumnText, _item);
		end else begin
			node := GetParentNode(sFileColumnText);
		end;
		AddChildNode(node, _item);
	finally
		FVst.EndUpdate;
		{$IFDEF THREADSAFE_LIST}
		HistObject.Matches.Unlock;
		{$ENDIF}
	end;
end;

function TRipGrepperData.AddVSTStructure(_node : PVirtualNode; _rec : TVSFileNodeData; _asFirst : Boolean) : PVirtualNode;
var
	Data : PVSFileNodeData;
begin
	if _asFirst then begin
		Result := FVst.InsertNode(_node, TVTNodeAttachMode.amAddChildFirst);
	end else begin
		Result := FVst.AddChild(_node);
	end;
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
		fn := matchItems[_index].Columns[Integer(ciFile)].Text;
		if matchItems[_index].IsError then begin
			_item.Caption := ' ' + fn;
			_item.ImageIndex := LV_IMG_IDX_ERROR;
		end else begin
			_item.Caption := fn;
			_item.ImageIndex := LV_IMG_IDX_OK;
		end;
		_item.SubItems.Add(matchItems[_index].Columns[Integer(ciRow)].Text);
		_item.SubItems.Add(matchItems[_index].Columns[Integer(ciCol)].Text);
		_item.SubItems.Add(matchItems[_index].Columns[Integer(ciText)].Text);
	finally
		{$IFDEF THREADSAFE_LIST}
		HistObject.Matches.Unlock;
		{$ENDIF}
	end;
end;

procedure TRipGrepperData.DataToGrid;
var
	node : PVirtualNode;
	sFile : string;
	matchItems : TListType;
begin
	MatchFiles.Clear;
	matchItems := HistObject.Matches.Items;
	try
		for var item in matchItems do begin
			sFile := item.Columns[Integer(ciFile)].Text;
			if item.IsError then begin
				node := ErrorHandling(sFile, item);
			end else begin
				node := GetParentNode(sFile);
			end;
			AddChildNode(node, item);
		end;
	finally
		{$IFDEF THREADSAFE_LIST}
		HistObject.Matches.Unlock;
		{$ENDIF}
	end;
end;

function TRipGrepperData.GetTotalMatchCount : Integer;
begin
	Result := HistObject.Matches.Items.Count; // Todo: filter context lines
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

function TRipGrepperData.GetHistObject: IHistoryItemObject;
begin
	Result := FHistObject;
end;

function TRipGrepperData.GetListItemCount : Integer;
begin
	Result := 0;
	if not NoMatchFound then begin
		Result := TotalMatchCount + ErrorCount;
	end;
end;

function TRipGrepperData.GetParentNode(const _sNodeText : string; _asFirst : Boolean = False) : PVirtualNode;
var
	nodeData : TVSFileNodeData;
begin
	var
	idx := MatchFiles.IndexOf(_sNodeText);
	if idx < 0 then begin
		nodeData := TVSFileNodeData.New(_sNodeText);
		Result := AddVSTStructure(nil, nodeData, _asFirst);
		MatchFiles.AddObject(_sNodeText, TObject(Result));
	end else begin
		Result := PVirtualNode(MatchFiles.Objects[idx]);
	end;
end;

procedure TRipGrepperData.AddChildNode(const _parentNode : PVirtualNode; _item : IParsedObjectRow);
var
	bAsFirst : Boolean;
	nodeData : TVSFileNodeData;
begin
	if not Assigned(_parentNode) then begin
		Exit;
	end;
	bAsFirst := False;
	if _item.IsError then begin
		_item.ParserType := ptRipGrepError;
		bAsFirst := True;
	end;
	case _item.ParserType of
		ptRipGrepSearch, ptRipGrepPrettySearch : begin
			nodeData := TVSFileNodeData.New('', // File
			{ } StrToIntDef(_item.Columns[Integer(ciRow)].Text, -1), // Row
			{ } StrToIntDef(_item.Columns[Integer(ciCol)].Text, -1), // Col
			{ } _item.GetColumnText(Integer(ciText)), // TextBefore
			{ } _item.GetColumnText(Integer(ciMatchText)), // MatchText
			{ } _item.GetColumnText(Integer(ciTextAfterMatch))// TextAfter
				);
		end;
		ptRipGrepError :
		nodeData := TVSFileNodeData.New(_item.Columns[Integer(ciFile)].Text, // File
		{ } -1, // Row
		{ } -1, // Col
		{ } ''); // LineText
	end;

	AddVSTStructure(_parentNode, nodeData, bAsFirst);
end;

function TRipGrepperData.ErrorHandling(const _sFileColumnText : string; _item : IParsedObjectRow) : PVirtualNode;
var
	node : PVirtualNode;
	nodeData : TVSFileNodeData;
begin
	Result := nil;
	if _sFileColumnText.EndsWith(RG_HAS_NO_OUTUT) then begin
		NoMatchFound := True;
		Exit;
	end else if TRegEx.IsMatch(_sFileColumnText, RG_ENDED_ERROR) then begin
		NoMatchFound := True;
		Exit;
	end else if _item.ErrorText = RG_PARSE_ERROR then begin
		if TRegEx.IsMatch(_sFileColumnText, '^' + RG_ERROR_MSG_PREFIX) then begin
			NoMatchFound := True;
			node := GetParentNode(RG_ERROR_MSG_PREFIX, True);
			nodeData := TVSFileNodeData.New(_sFileColumnText.Remove(0, RG_ERROR_MSG_PREFIX.Length));
			AddVSTStructure(node, nodeData, true);
			Exit;
		end;
	end;

	Inc(FErrorCount);
	Result := GetParentNode(_item.ErrorText, True);
end;

function TRipGrepperData.GetNoMatchFound : Boolean;
begin

	Result := HistObject.NoMatchFound;
end;

procedure TRipGrepperData.SetHistObject(const Value: IHistoryItemObject);
begin
	FHistObject := Value;
end;

procedure TRipGrepperData.SetNoMatchFound(const Value : Boolean);
begin
	HistObject.NoMatchFound := Value;
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
