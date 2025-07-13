unit RipGrepper.Data.Matches;

interface

uses
	System.Generics.Collections,
	System.Classes,
	RipGrepper.Common.Interfaces,
	System.RegularExpressions,
	Vcl.ComCtrls,
	ArrayEx,
	System.Generics.Defaults,
	RipGrepper.Common.Sorter,
	RipGrepper.Data.HistoryItemObject,
	RipGrepper.Common.ParsedObject,
	VirtualTrees,
	RipGrepper.Common.NodeData,
	RipGrepper.Common.SimpleTypes;

type

	TRipGrepperData = class
		MatchFiles : TStringList;
		SortedBy : TSortTypeDirectionList;

		private
			FHistObject : IHistoryItemObject;
			FVst : TCustomVirtualStringTree;
			function AddVSTStructure(_node : PVirtualNode; _rec : TVSFileNodeData; _asFirst : Boolean) : PVirtualNode;
			function GetTotalMatchCount : Integer;
			function GetFileCount : Integer;
			function GetComparer(const _sbt : TSortByType) : IComparer<IParsedObjectRow>;
			function GetErrorCount : Integer;
			function GetHistObject : IHistoryItemObject;
			function GetListItemCount : Integer;
			function GetParentNode(const _sNodeText : string; _asFirst : Boolean = False) : PVirtualNode;
			procedure AddChildNode(const _parentNode : PVirtualNode; _item : IParsedObjectRow);
			procedure AddNode(_item : IParsedObjectRow);
			function ErrorOrStatsLinesHandling(const _sFileColumnText : string; _item : IParsedObjectRow) : PVirtualNode;
			function GetNoMatchFound : Boolean;
			function GetParentNodeByItemType(const sFileColumnText : string; _item : IParsedObjectRow) : PVirtualNode;
			procedure SetHistObject(const Value : IHistoryItemObject);
			procedure SetNoMatchFound(const Value : Boolean);
			procedure SortMultiColumns(const _st : TSortDirectionType);

		public
			FErrorCounters : TErrorCounters;

			constructor Create(_vst : TCustomVirtualStringTree);
			destructor Destroy; override;
			procedure Add(_item : IParsedObjectRow);
			procedure ClearMatchFiles;
			procedure DataToGrid; overload;
			procedure SortBy(const _sbt : TSortByType; const _st : TSortDirectionType);
			property ErrorCount : Integer read GetErrorCount;
			property TotalMatchCount : Integer read GetTotalMatchCount;
			property FileCount : Integer read GetFileCount;
			property HistObject : IHistoryItemObject read GetHistObject write SetHistObject;
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
	RipGrepper.Common.Constants,
	System.Math;

constructor TRipGrepperData.Create(_vst : TCustomVirtualStringTree);
begin
	inherited Create();
	MatchFiles := TStringList.Create(TDuplicates.dupIgnore, True, True);
	FErrorCounters.Reset();
	FVst := _vst;
end;

destructor TRipGrepperData.Destroy;
begin
	MatchFiles.Free;
	inherited;
end;

procedure TRipGrepperData.Add(_item : IParsedObjectRow);
begin
	if (_item.Columns.IsEmpty) then
		Exit;

	FVst.BeginUpdate;
	try
		HistObject.Matches.Items.Add(_item);
		AddNode(_item);
	finally
		FVst.EndUpdate;
	end;
end;

function TRipGrepperData.AddVSTStructure(_node : PVirtualNode; _rec : TVSFileNodeData; _asFirst : Boolean) : PVirtualNode;
var
	nodeData : PVSFileNodeData;
begin
	if _asFirst then begin
		Result := FVst.InsertNode(_node, TVTNodeAttachMode.amAddChildFirst);
	end else begin
		Result := FVst.AddChild(_node);
	end;
	Result.CheckType := ctCheckBox;
	nodeData := FVst.GetNodeData(Result);
	// FVst.ValidateNode(Result, False);
	nodeData^.FilePath := _rec.FilePath;
	nodeData^.MatchData := _rec.MatchData;
end;

procedure TRipGrepperData.ClearMatchFiles;
begin
	MatchFiles.Clear;
	FErrorCounters.Reset();
end;

procedure TRipGrepperData.DataToGrid;
var
	node : PVirtualNode;
	sFile : string;
	matchItems : TParsedRowList;
begin
	MatchFiles.Clear;
	matchItems := HistObject.Matches.Items;

	for var item in matchItems do begin
		sFile := item.Columns[Integer(ciFile)].Text;
		node := GetParentNodeByItemType(sFile, item);
		AddChildNode(node, item);
	end;
end;

function TRipGrepperData.GetTotalMatchCount : Integer;
begin
	Result := HistObject.Matches.Items.Count; // TODO: filter context lines (they have no column nummber)
end;

function TRipGrepperData.GetFileCount : Integer;
begin
	Result := MatchFiles.Count - IfThen(FErrorCounters.FStatLineCount > 0, 1) - IfThen(FErrorCounters.FSumOfErrors > 0, 1);
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
					Result := TComparer<integer>.Default.Compare(Left.ParsedRowNr, Right.ParsedRowNr);
				end);
		end;
	end;
end;

function TRipGrepperData.GetErrorCount : Integer;
begin
	Result := FErrorCounters.FSumOfErrors;
end;

function TRipGrepperData.GetHistObject : IHistoryItemObject;
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
	if _item.IsStatsLine then begin
		_item.ParserType := ptRipGrepStats;
		bAsFirst := True;
	end;
	case _item.ParserType of
		ptRipGrepSearch, ptRipGrepPrettySearch : begin
			nodeData := TVSFileNodeData.New('', // File
			{ } _item.Row, // Row
			{ } _item.Col, // Col
			{ } _item.GetColumnText(ciText), // TextBefore
			{ } _item.GetColumnText(ciMatchText), // MatchText
			{ } _item.GetColumnText(ciTextAfterMatch) // TextAfter
				);
		end;
		ptRipGrepError : begin
			nodeData := TVSFileNodeData.New(_item.Columns[Integer(ciFile)].Text, // File
			{ } -1, // Row
			{ } -1, // Col
			{ } ''); // LineText
		end;
		ptRipGrepStats : begin
			nodeData := TVSFileNodeData.New('', // File
			{ } -1, // Row
			{ } -1, // Col
			{ } _item.GetColumnText(ciText)); // LineText
		end;
	end;

	AddVSTStructure(_parentNode, nodeData, bAsFirst);
	_parentNode.CheckType := ctCheckBox;
end;

procedure TRipGrepperData.AddNode(_item : IParsedObjectRow);
var
	node : PVirtualNode;
	sFileColumnText : string;
begin
	sFileColumnText := _item.Columns[Integer(ciFile)].Text;
	node := GetParentNodeByItemType(sFileColumnText, _item);
	AddChildNode(node, _item);
end;

function TRipGrepperData.ErrorOrStatsLinesHandling(const _sFileColumnText : string; _item : IParsedObjectRow) : PVirtualNode;
var
	node : PVirtualNode;
	nodeData : TVSFileNodeData;
begin
	Result := nil;
	if _item.IsError then begin
		if _item.ErrorText = RG_PARSE_ERROR then begin
			if TRegEx.IsMatch(_sFileColumnText, '(^' + RG_ERROR_MSG_PREFIX + '|' + RG_ENDED_ERROR + ')') then begin
				NoMatchFound := True;
				FErrorCounters.FIsRGReportedError := TRegEx.IsMatch(_sFileColumnText, RG_ENDED_ERROR);
				node := GetParentNode(RG_ERROR_MSG_PREFIX, True);
				nodeData := TVSFileNodeData.New(_sFileColumnText.Remove(0, RG_ERROR_MSG_PREFIX.Length));
				AddVSTStructure(node, nodeData, true);
				Exit;
			end else if _sFileColumnText.EndsWith(RG_HAS_NO_OUTUT) then begin
				FErrorCounters.FIsNoOutputError := True;
				NoMatchFound := True;
				Exit;
			end;
			Inc(FErrorCounters.FParserErrors);
		end;
		Inc(FErrorCounters.FSumOfErrors);
		Result := GetParentNode(_item.ErrorText, True);
	end else if _item.IsStatsLine then begin
		Inc(FErrorCounters.FStatLineCount);
		Result := GetParentNode(_sFileColumnText, True);
	end;

end;

function TRipGrepperData.GetNoMatchFound : Boolean;
begin

	Result := HistObject.NoMatchFound;
end;

function TRipGrepperData.GetParentNodeByItemType(const sFileColumnText : string; _item : IParsedObjectRow) : PVirtualNode;
begin
	if (_item.IsError) then begin
		Result := ErrorOrStatsLinesHandling(sFileColumnText, _item);
	end else if (_item.IsStatsLine) then begin
		Result := ErrorOrStatsLinesHandling(sFileColumnText, _item);
	end else begin
		Result := GetParentNode(sFileColumnText);
	end;
end;

procedure TRipGrepperData.SetHistObject(const Value : IHistoryItemObject);
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
			matchItems.Sort(lineComparer);
		end;
	finally
		lineComparer.Free;
	end;
end;

end.
