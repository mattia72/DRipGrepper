unit RipGrepper.UI.MiddleLeftFrame;

interface

uses
	Winapi.Windows,
	Winapi.Messages,
	// System.SysUtils,
	System.Variants,
	System.Classes,
	Vcl.Graphics,
	Vcl.Controls,
	Vcl.Forms,
	Vcl.Dialogs,
	VirtualTrees,
	System.Actions,
	Vcl.ActnList,
	RipGrepper.Settings.RipGrepperSettings,
	ArrayEx,
	RipGrepper.Common.Interfaces,
	RipGrepper.Data.Matches,
	RipGrepper.Data.HistoryItemObject,
	Vcl.Menus,
	System.ImageList,
	Vcl.ImgList;

type
	THistoryObjectArray = TArrayEx<IHistoryItemObject>;

	TMiddleLeftFrame = class(TFrame)
		VstHistory : TVirtualStringTree;
		ActionList : TActionList;
		ActionHistoryDelete : TAction;
		ActionHistoryDeleteAll : TAction;
		ActionCopyCmdLineToClipboard : TAction;
		ActionOpenSearchForm : TAction;
		PopupMenuHistory : TPopupMenu;
		pmOpenSearchForm : TMenuItem;
		N3 : TMenuItem;
		pmCopyCommandLine : TMenuItem;
		N2 : TMenuItem;
		pmHistoryDelete : TMenuItem;
		pmHistoryDeleteAll : TMenuItem;
		ImageList1 : TImageList;
		procedure ActionCopyCmdLineToClipboardExecute(Sender : TObject);
		procedure ActionHistoryDeleteAllExecute(Sender : TObject);
		procedure ActionHistoryDeleteAllUpdate(Sender : TObject);
		procedure ActionHistoryDeleteExecute(Sender : TObject);
		procedure ActionHistoryDeleteUpdate(Sender : TObject);
		procedure ActionOpenSearchFormExecute(Sender : TObject);
		procedure PopupMenuHistoryPopup(Sender : TObject);
		procedure VstHistoryBeforeCellPaint(Sender : TBaseVirtualTree; TargetCanvas : TCanvas; Node : PVirtualNode; Column : TColumnIndex;
			CellPaintMode : TVTCellPaintMode; CellRect : TRect; var ContentRect : TRect);
		procedure VstHistoryFreeNode(Sender : TBaseVirtualTree; Node : PVirtualNode);
		procedure VstHistoryGetHint(Sender : TBaseVirtualTree; Node : PVirtualNode; Column : TColumnIndex;
			var LineBreakStyle : TVTTooltipLineBreakStyle; var HintText : string);
		procedure VstHistoryGetHintKind(Sender : TBaseVirtualTree; Node : PVirtualNode; Column : TColumnIndex; var Kind : TVTHintKind);
		procedure VstHistoryGetText(Sender : TBaseVirtualTree; Node : PVirtualNode; Column : TColumnIndex; TextType : TVSTTextType;
			var CellText : string);
		procedure VstHistoryNodeClick(Sender : TBaseVirtualTree; const HitInfo : THitInfo);
		procedure VstHistoryNodeDblClick(Sender : TBaseVirtualTree; const HitInfo : THitInfo);
		procedure VstHistoryPaintText(Sender : TBaseVirtualTree; const TargetCanvas : TCanvas; Node : PVirtualNode; Column : TColumnIndex;
			TextType : TVSTTextType);

		private const
			COL_SEARCH_TEXT = 0;
			COL_REPLACE_TEXT = 1;

		var
			FCurrentHistoryItemIndex : Integer;
			FData : TRipGrepperData;
			FHistoryObjectList : THistoryObjectArray;
			FSettings : TRipGrepperSettings;
			procedure AddVstHistItem;
			procedure AddVstReplaceNode(Node : PVirtualNode);
			procedure ChangeVstReplaceNode(Node : PVirtualNode; const _Data : PVSHistoryNodeData = nil);
			function GetData : TRipGrepperData;
			function GetHistNodeIndex(Node : PVirtualNode) : integer;
			function GetHistoryObject(const _index : Integer) : THistoryItemObject;
			function GetNodeByIndex(Tree : TVirtualStringTree; Index : Integer) : PVirtualNode;
			function GetSettings : TRipGrepperSettings;
			procedure ShowReplaceColumn(const _bShow : Boolean);
			procedure UpdateReplaceColumnVisible;
			property Settings : TRipGrepperSettings read GetSettings write FSettings;

			{ Private-Deklarationen }
		public
			constructor Create(AOwner : TComponent); override;
			procedure AddHistoryObject(_ho : IHistoryItemObject);
			procedure AddOrUpdateHistoryItem;
			procedure ChangeDataHistItemObject(_ho : IHistoryItemObject);
			procedure ChangeHistoryNodeText;
			procedure ClearMatchesInHistoryObject;
			procedure ClearHistoryObjectList;
			procedure DeleteCurrentHistoryItemFromList;
			function GetCurrentHistoryObject : IHistoryItemObject;
			procedure Init;
			procedure PrepareAndDoSearch;
			procedure SetReplaceMode(_hio : IHistoryItemObject = nil);
			procedure SetSelectedHistoryItem(const _idx : Integer);
			property CurrentHistoryItemIndex : Integer read FCurrentHistoryItemIndex write FCurrentHistoryItemIndex;
			property Data : TRipGrepperData read GetData write FData;
			{ Public-Deklarationen }
	end;

var
	MiddleLeftFrame : TMiddleLeftFrame;

implementation

uses
	RipGrepper.Common.Constants,
	RipGrepper.Helper.UI,
	RipGrepper.Tools.DebugUtils,
	RipGrepper.UI.SearchForm,
	RipGrepper.UI.ParentFrame,
	RipGrepper.UI.MiddleFrame,
	System.Math,
	System.RegularExpressions,
	VirtualTrees.Types,
	System.SysUtils,
	System.StrUtils,
	RipGrepper.Helper.Types,
	RipGrepper.Common.SimpleTypes,
	RipGrepper.Settings.FontColors;

{$R *.dfm}

constructor TMiddleLeftFrame.Create(AOwner : TComponent);
begin
	inherited;
	TDebugUtils.DebugMessage('TMiddleLeftFrame.Create ' + AOwner.Name);
	MiddleLeftFrame := self;
end;

procedure TMiddleLeftFrame.ActionCopyCmdLineToClipboardExecute(Sender : TObject);
begin
	ParentFrame.MainFrame.ActionCopyCmdLineToClipboardExecute(Sender);
end;

procedure TMiddleLeftFrame.ActionHistoryDeleteAllExecute(Sender : TObject);
begin
	MainFrame.VstResult.Clear;
	VstHistory.Clear;
	ClearHistoryObjectList;
	MainFrame.HistItemObject := nil;
end;

procedure TMiddleLeftFrame.ActionHistoryDeleteAllUpdate(Sender : TObject);
begin
	ActionHistoryDeleteAll.Enabled := VstHistory.RootNodeCount <> 0;
end;

procedure TMiddleLeftFrame.ActionHistoryDeleteExecute(Sender : TObject);
var
	ho : IHistoryItemObject;
	Node : PVirtualNode;
	Data : PVSHistoryNodeData;
begin
	ho := GetCurrentHistoryObject;

	Node := GetNodeByIndex(VstHistory, CurrentHistoryItemIndex);
	Data := VstHistory.GetNodeData(Node);
	TDebugUtils.DebugMessageFormat('TMiddleLeftFrame.ActionHistoryDeleteExecute: idx:%d Node:%s, ho:%s',
		[CurrentHistoryItemIndex, Data.SearchText, ho.GuiSearchTextParams.SearchText]);

	Assert((Data.SearchText = ho.GuiSearchTextParams.SearchText) or
		(WB + Data.SearchText + WB = ho.GuiSearchTextParams.WordBoundedSearchText) or
		(TRegEx.Escape(Data.SearchText) = ho.GuiSearchTextParams.EscapedSearchText),
		Data.SearchText + ' != ' + ho.GuiSearchTextParams.SearchText);

	VstHistory.DeleteNode(Node);
	VstHistory.Refresh;
	DeleteCurrentHistoryItemFromList;
	// FreeAndNil(ho);
	ho := nil;
end;

procedure TMiddleLeftFrame.ActionHistoryDeleteUpdate(Sender : TObject);
begin
	ActionHistoryDelete.Enabled := CurrentHistoryItemIndex <> -1;
end;

procedure TMiddleLeftFrame.ActionOpenSearchFormExecute(Sender : TObject);
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TMiddleLeftFrame.ActionOpenSearchFormExecute');
	var
	formResult := TRipGrepperSearchDialogForm.ShowSearchForm(self, Settings, MainFrame.HistItemObj);
	if mrOK = formResult then begin
		dbgMsg.Msg('after ShowSearchForm cmdline: ' + Settings.RipGrepParameters.GetCommandLine);
		MainFrame.PrepareAndDoSearch();
	end else begin
		dbgMsg.Msg('ShowSearchForm cancel');
	end;

	// ChangeHistoryNodeText;
	// MainFrame.BeforeSearch;
end;

procedure TMiddleLeftFrame.AddHistoryObject(_ho : IHistoryItemObject);
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TMiddleLeftFrame.AddHistoryObject');

	dbgMsg.Msg('Add HistoryObject ' + Settings.LastSearchText);
	CurrentHistoryItemIndex := FHistoryObjectList.Add(_ho);
	dbgMsg.Msg('CurrentHistoryItemIndex=' + CurrentHistoryItemIndex.ToString);
end;

procedure TMiddleLeftFrame.AddOrUpdateHistoryItem;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TMiddleLeftFrame.AddOrUpdateHistoryItem');
	dbgMsg.Msg('CurrentHistoryItemIndex ' + CurrentHistoryItemIndex.ToString);
	if not MainFrame.HistItemObj.HasResult then begin
		AddVstHistItem;
	end else begin
		ChangeHistoryNodeText;
	end;

	UpdateReplaceColumnVisible;

	if MainFrame.HistItemObj.HasResult then begin
		MainFrame.UpdateHistObjectAndCopyToSettings;
	end;
	MainFrame.UpdateRipGrepArgumentsInHistObj;

	ClearMatchesInHistoryObject();
	dbgMsg.Msg('Update HistoryObject LastSearchText=' + Settings.LastSearchText);
end;

procedure TMiddleLeftFrame.AddVstHistItem;
var
	Node : PVirtualNode;
	Data : PVSHistoryNodeData;
begin
	Node := VstHistory.AddChild(nil);
	Data := VstHistory.GetNodeData(Node);
	Data^.SearchText := Settings.LastSearchText;
	Data^.ReplaceData.IsReplaceMode := Settings.IsReplaceMode;
	Data^.ReplaceData.ReplaceText := '';

	if Settings.IsReplaceMode then begin
		AddVstReplaceNode(Node);
	end;
	// VstHistory.MultiLine[Node] := True;
end;

procedure TMiddleLeftFrame.AddVstReplaceNode(Node : PVirtualNode);
var
	childNode : PVirtualNode;
	data : PVSHistoryNodeData;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TMiddleLeftFrame.AddVstReplaceNode');
	childNode := VstHistory.AddChild(Node);
	data := VstHistory.GetNodeData(childNode);
	data^.SearchText := '';
	data^.ReplaceData.IsReplaceMode := True;
	data^.ReplaceData.ReplaceText := Settings.LastReplaceText;
end;

procedure TMiddleLeftFrame.ChangeDataHistItemObject(_ho : IHistoryItemObject);
begin
	// var beu := TBeginEndUpdater.New(VstHistory);
	// First access to data will create MainFrame.Data
	MainFrame.Data.HistObject := _ho;

	SetReplaceMode(_ho);
end;

procedure TMiddleLeftFrame.ChangeHistoryNodeText;
var
	Node : PVirtualNode;
	Data : PVSHistoryNodeData;
begin
	TDebugUtils.DebugMessage('TMiddleLeftFrame.ChangeHistoryNodeText: idx = ' + CurrentHistoryItemIndex.ToString);

	Node := GetNodeByIndex(VstHistory, CurrentHistoryItemIndex);
	Data := VstHistory.GetNodeData(Node);
	TDebugUtils.DebugMessageFormat('TMiddleLeftFrame.ChangeHistoryNodeText: SearchText orig=''%s'' new=''%''',
		[Data^.SearchText, Settings.LastSearchText]);
	if not Settings.LastSearchText.IsEmpty then begin
		Data^.SearchText := Settings.LastSearchText;
		var
		bChangedReplaceMode := Data^.ReplaceData.IsReplaceMode <> Settings.IsReplaceMode;
		if bChangedReplaceMode then begin
			ChangeVstReplaceNode(Node, Data);
		end;
	end;
	VstHistory.Repaint;
end;

procedure TMiddleLeftFrame.ChangeVstReplaceNode(Node : PVirtualNode; const _Data : PVSHistoryNodeData = nil);
var
	Data : PVSHistoryNodeData;
begin
	TDebugUtils.DebugMessage('TMiddleLeftFrame.ChangeVstReplaceNode');
	Data := _Data;
	if not Assigned(Data) then begin
		Data := VstHistory.GetNodeData(Node);
	end;

	Data^.ReplaceData.IsReplaceMode := Settings.IsReplaceMode;
	if Data^.ReplaceData.IsReplaceMode then begin
		if Node.ChildCount = 0 then begin
			AddVstReplaceNode(Node);
			Data^.ReplaceData.ReplaceText := '';
		end;
	end else begin
		if Node.ChildCount > 0 then begin
			VstHistory.DeleteNode(Node.FirstChild);
		end;
	end;
end;

procedure TMiddleLeftFrame.ClearMatchesInHistoryObject;
begin
	var
	beu := TBeginEndUpdater.New(VstHistory);
	MainFrame.HistItemObject.ClearMatches;
end;

procedure TMiddleLeftFrame.ClearHistoryObjectList;
begin
	for var i := 0 to FHistoryObjectList.Count - 1 do begin
		if (FHistoryObjectList.Count > i)
		{ } and (FHistoryObjectList[i] is THistoryItemObject) then begin
			// (FHistoryObjectList[i] as THistoryItemObject).Free; // NoRefCountObj!
			FHistoryObjectList[i] := nil;
		end;
	end;
	FHistoryObjectList.Clear;
end;

procedure TMiddleLeftFrame.DeleteCurrentHistoryItemFromList;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TMiddleLeftFrame.DeleteCurrentHistoryItemFromList');
	dbgMsg.Msg('Deleting history item at index ' + CurrentHistoryItemIndex.ToString);
	FHistoryObjectList.Delete(CurrentHistoryItemIndex);

	CurrentHistoryItemIndex := IfThen(VstHistory.RootNodeCount = 0, -1, IfThen(CurrentHistoryItemIndex = 0, 0,
		CurrentHistoryItemIndex - 1));

	dbgMsg.Msg('CurrentHistoryItemIndex=' + CurrentHistoryItemIndex.ToString);

	if CurrentHistoryItemIndex <> -1 then begin
		MainFrame.UpdateHistObjectAndGui;
		VstHistory.Selected[GetNodeByIndex(VstHistory, CurrentHistoryItemIndex)] := True;
	end else begin
		MainFrame.VstResult.Clear;
		VstHistory.Clear;
		MainFrame.HistItemObject := nil;
	end;
end;

function TMiddleLeftFrame.GetCurrentHistoryObject : IHistoryItemObject;
begin
	Result := GetHistoryObject(CurrentHistoryItemIndex);
end;

function TMiddleLeftFrame.GetData : TRipGrepperData;
begin
	Result := FData;
end;

function TMiddleLeftFrame.GetHistNodeIndex(Node : PVirtualNode) : integer;
begin
	if (Node.Parent = VstHistory.RootNode) then begin
		Result := Node.Index;
	end else begin
		Result := Node.Parent.Index;
	end;
end;

function TMiddleLeftFrame.GetHistoryObject(const _index : Integer) : THistoryItemObject;
begin
	Result := nil;
	if (_index > -1) and (_index < FHistoryObjectList.Count { _lb.Items.Count } ) then begin
		Result := THistoryItemObject(FHistoryObjectList[_index]);
		// Result := THistoryItemObject(_lb.Items.Objects[_index]);
	end;
end;

function TMiddleLeftFrame.GetNodeByIndex(Tree : TVirtualStringTree; Index : Integer) : PVirtualNode;
var
	node : PVirtualNode;
begin
	Result := nil;
	node := Tree.GetFirstChildNoInit(nil);
	while Assigned(node) do begin
		if Integer(node.Index) = index then begin
			Result := node;
			Exit;
		end;
		node := Tree.GetNextNoInit(node);
	end;
end;

function TMiddleLeftFrame.GetSettings : TRipGrepperSettings;
begin
	if not Assigned(FSettings) then begin
		FSettings := ParentFrame.Settings;
	end;
	Result := FSettings;
end;

procedure TMiddleLeftFrame.Init;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TMiddleLeftFrame.Init');
	FCurrentHistoryItemIndex := -1;
	FHistoryObjectList.Clear();

	VstHistory.TreeOptions.AutoOptions := VstHistory.TreeOptions.AutoOptions + [toAutoExpand, toAutoSpanColumns]; //
	VstHistory.TreeOptions.StringOptions := VstHistory.TreeOptions.StringOptions + [toShowStaticText];
	VstHistory.TreeOptions.PaintOptions := VstHistory.TreeOptions.PaintOptions + [toUseExplorerTheme];
	VstHistory.TreeOptions.MiscOptions := VstHistory.TreeOptions.MiscOptions + [TVTMiscOption.toVariablenodeHeight];

	VstHistory.NodeDataSize := SizeOf(TVSHistoryNodeData);

	VstHistory.Header.Columns[COL_SEARCH_TEXT].MinWidth := 50;

	SetReplaceMode();
	ShowReplaceColumn(False);

end;

procedure TMiddleLeftFrame.PopupMenuHistoryPopup(Sender : TObject);
begin
	SetSelectedHistoryItem(CurrentHistoryItemIndex);
end;

procedure TMiddleLeftFrame.PrepareAndDoSearch;
begin
	AddOrUpdateHistoryItem;
	SetSelectedHistoryItem(CurrentHistoryItemIndex);
end;

procedure TMiddleLeftFrame.SetReplaceMode(_hio : IHistoryItemObject = nil);
var
	hio : IHistoryItemObject;
	mode : TGuiReplaceModes;
	repText : string;
begin
	if not Assigned(_hio) then begin
		hio := GetCurrentHistoryObject();
	end else begin
		hio := _hio;
	end;

	mode := [];
	if Assigned(hio) then begin
		if hio.IsReplaceMode then begin
			Include(mode, EGuiReplaceMode.grmActive);
			Include(mode, EGuiReplaceMode.grmSaveEnabled);
			Include(mode, EGuiReplaceMode.grmRGReplace);
		end else begin
			Include(mode, EGuiReplaceMode.grmEditEnabled);
		end;
	end;

	repText := '';
	if Assigned(hio) then begin
		repText := hio.ReplaceText;
	end;
	ParentFrame.TopFrame.SetGuiReplaceMode(mode, repText);
end;

procedure TMiddleLeftFrame.SetSelectedHistoryItem(const _idx : Integer);
var
	Node : PVirtualNode;
begin
	Node := GetNodeByIndex(VstHistory, _idx);
	if Assigned(Node) then begin
		VstHistory.Selected[Node] := true;
	end;
end;

procedure TMiddleLeftFrame.ShowReplaceColumn(const _bShow : Boolean);
begin
	if _bShow then begin
		VstHistory.Header.Columns[COL_REPLACE_TEXT].Options := VstHistory.Header.Columns[COL_REPLACE_TEXT].Options + [coVisible];
	end else begin
		VstHistory.Header.Columns[COL_REPLACE_TEXT].Options := VstHistory.Header.Columns[COL_REPLACE_TEXT].Options - [coVisible];
	end;
	VstHistory.Header.AutoSizeIndex := 0; // VstHistory.Header.Columns.GetLastVisibleColumn();
end;

procedure TMiddleLeftFrame.UpdateReplaceColumnVisible;
var
	bFoundReplace : Boolean;
begin
	bFoundReplace := False;
	for var node in VstHistory.Nodes do begin
		if node.ChildCount > 0 then begin
			bFoundReplace := True;
			break;
		end;
	end;

	ShowReplaceColumn(bFoundReplace);
end;

procedure TMiddleLeftFrame.VstHistoryBeforeCellPaint(Sender : TBaseVirtualTree; TargetCanvas : TCanvas; Node : PVirtualNode;
	Column : TColumnIndex; CellPaintMode : TVTCellPaintMode; CellRect : TRect; var ContentRect : TRect);
var
	R : TRect;
begin
	if CellPaintMode = cpmPaint then begin
		R := Sender.GetDisplayRect(Node, Column, True, False, True);
		R.Offset(0, -R.Top);
		case Sender.GetNodeLevel(Node) of
			0 : begin
				if (Node.ChildCount = 0) then begin
					TargetCanvas.Brush.Color := TDefaultFontColors.HIST_TREEVIEW_SEARCH_TEXT.BgColor;
				end else begin
					TargetCanvas.Brush.Color := TDefaultFontColors.HIST_TREEVIEW_REPLACED_TEXT.BgColor;
				end;
			end;
			1 :
			TargetCanvas.Brush.Color := TDefaultFontColors.HIST_TREEVIEW_REPLACE_TEXT.BgColor;
		end;
		TargetCanvas.FillRect(R);
	end;
end;

procedure TMiddleLeftFrame.VstHistoryFreeNode(Sender : TBaseVirtualTree; Node : PVirtualNode);
var
	Data : PVSHistoryNodeData;
begin
	Data := VstHistory.GetNodeData(Node);
	Data.SearchText := '';
	Data.ReplaceData.ReplaceText := '';
	// Data.hio.Free;
end;

procedure TMiddleLeftFrame.VstHistoryGetHint(Sender : TBaseVirtualTree; Node : PVirtualNode; Column : TColumnIndex;
	var LineBreakStyle : TVTTooltipLineBreakStyle; var HintText : string);
var
	ho : IHistoryItemObject;
begin
	ho := GetHistoryObject(Node.Index);
	LineBreakStyle := TVTTooltipLineBreakStyle.hlbForceMultiLine;
	var
	lineBreakTab := CRLF + '  ';
	HintText := 'rg.exe' + lineBreakTab + string.Join(lineBreakTab, ho.RipGrepArguments.GetValues);
end;

procedure TMiddleLeftFrame.VstHistoryGetHintKind(Sender : TBaseVirtualTree; Node : PVirtualNode; Column : TColumnIndex;
	var Kind : TVTHintKind);
begin
	Kind := TVTHintKind.vhkText;
end;

procedure TMiddleLeftFrame.VstHistoryGetText(Sender : TBaseVirtualTree; Node : PVirtualNode; Column : TColumnIndex; TextType : TVSTTextType;
	var CellText : string);
var
	Data : PVSHistoryNodeData;
begin
	Data := VstHistory.GetNodeData(Node);
	case Column of
		COL_SEARCH_TEXT : begin
			if TextType = ttNormal then begin
				CellText := Data.SearchText; // + CRLF + MainFrame.GetCounterText(GetHistoryObject(Node.Index));
			end else begin
				// only for root
				if Node.Parent = VstHistory.RootNode then begin
					// ttStatic not shown in Multiline cell
					CellText := MainFrame.GetCounterText(GetHistoryObject(Node.Index));
				end;
			end;
		end;
		COL_REPLACE_TEXT : begin
			if Data.ReplaceData.IsReplaceMode then begin
				if TextType = ttNormal then begin
					CellText := Data.ReplaceData.ReplaceText;
				end;
			end;
		end;
	end;

end;

procedure TMiddleLeftFrame.VstHistoryNodeClick(Sender : TBaseVirtualTree; const HitInfo : THitInfo);
var
	idx : integer;
begin
	idx := GetHistNodeIndex(HitInfo.HitNode);

	if (CurrentHistoryItemIndex <> idx) then begin
		CurrentHistoryItemIndex := idx;
		ParentFrame.AfterHistObjChange();

		// MainFrame.UpdateHistObjectAndGui;
	end;

	SetReplaceMode();
end;

procedure TMiddleLeftFrame.VstHistoryNodeDblClick(Sender : TBaseVirtualTree; const HitInfo : THitInfo);
begin
	TDebugUtils.DebugMessage('TMiddleLeftFrame.VstHistoryNodeDblClick: idx = ' + HitInfo.HitNode.Index.ToString);
	VstHistoryNodeClick(Sender, HitInfo);
	ActionOpenSearchFormExecute(Sender);
end;

procedure TMiddleLeftFrame.VstHistoryPaintText(Sender : TBaseVirtualTree; const TargetCanvas : TCanvas; Node : PVirtualNode;
	Column : TColumnIndex; TextType : TVSTTextType);
var
	hio : IHistoryItemObject;
	idx : integer;
begin
	idx := GetHistNodeIndex(Node);
	hio := GetHistoryObject(idx);

	if TextType = ttNormal then begin
		case Column of
			COL_SEARCH_TEXT : begin
				if hio.IsReplaceMode then begin
					TItemDrawer.SetTextColorHistoryReplacedText(TargetCanvas, TDefaultFontColors.HIST_TREEVIEW_REPLACED_TEXT);
				end else begin
					TItemDrawer.SetTextColorHistorySearchText(TargetCanvas, TDefaultFontColors.HIST_TREEVIEW_SEARCH_TEXT);
				end;
			end;
			COL_REPLACE_TEXT : begin
				TItemDrawer.SetTextColorHistoryReplaceText(TargetCanvas, TDefaultFontColors.HIST_TREEVIEW_REPLACE_TEXT);
			end;
		end
	end else begin // ttStatic
		TItemDrawer.SetTextColorErrorStaticText(TargetCanvas,
        	TDefaultFontColors.TREEVIEW_STAT_TEXT,
        	TDefaultFontColors.TREEVIEW_ERROR_TEXT,
            hio.GetErrorCounters().FSumOfErrors > 0);
	end;
end;

end.
