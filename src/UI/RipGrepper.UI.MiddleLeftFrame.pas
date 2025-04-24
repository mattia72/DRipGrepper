unit RipGrepper.UI.MiddleLeftFrame;

interface

uses
	Winapi.Windows,
	Winapi.Messages,
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
	Vcl.ImgList,
	RipGrepper.Settings.FontColors,
	Vcl.ExtCtrls,
	SVGIconImageListBase,
	SVGIconImageList;

type

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
		Panel1 : TPanel;
		SVGIconImageList1 : TSVGIconImageList;
		pmSave : TMenuItem;
		N1 : TMenuItem;
		ActionSave : TAction;
		pmLoad : TMenuItem;
		ActionLoad : TAction;
		SaveDialog1 : TSaveDialog;
		OpenDialog1 : TOpenDialog;
		procedure ActionCopyCmdLineToClipboardExecute(Sender : TObject);
		procedure ActionHistoryDeleteAllExecute(Sender : TObject);
		procedure ActionHistoryDeleteAllUpdate(Sender : TObject);
		procedure ActionHistoryDeleteExecute(Sender : TObject);
		procedure ActionHistoryDeleteUpdate(Sender : TObject);
		procedure ActionLoadExecute(Sender : TObject);
		procedure ActionOpenSearchFormExecute(Sender : TObject);
		procedure ActionSaveExecute(Sender : TObject);
		procedure PopupMenuHistoryPopup(Sender : TObject);
		procedure VstHistoryBeforeCellPaint(Sender : TBaseVirtualTree; TargetCanvas : TCanvas; Node : PVirtualNode; Column : TColumnIndex;
			CellPaintMode : TVTCellPaintMode; CellRect : TRect; var ContentRect : TRect);
		procedure VstHistoryFreeNode(Sender : TBaseVirtualTree; Node : PVirtualNode);
		procedure VstHistoryGetHint(Sender : TBaseVirtualTree; Node : PVirtualNode; Column : TColumnIndex;
			var LineBreakStyle : TVTTooltipLineBreakStyle; var HintText : string);
		procedure VstHistoryGetHintKind(Sender : TBaseVirtualTree; Node : PVirtualNode; Column : TColumnIndex; var Kind : TVTHintKind);
		procedure VstHistoryGetText(Sender : TBaseVirtualTree; Node : PVirtualNode; Column : TColumnIndex; TextType : TVSTTextType;
			var CellText : string);
		procedure VstHistoryLoadTree(Sender : TBaseVirtualTree; Stream : TStream);
		procedure VstHistoryNodeClick(Sender : TBaseVirtualTree; const HitInfo : THitInfo);
		procedure VstHistoryNodeDblClick(Sender : TBaseVirtualTree; const HitInfo : THitInfo);
		procedure VstHistoryPaintText(Sender : TBaseVirtualTree; const TargetCanvas : TCanvas; Node : PVirtualNode; Column : TColumnIndex;
			TextType : TVSTTextType);
		procedure VstHistorySaveTree(Sender : TBaseVirtualTree; Stream : TStream);

		private const
			COL_SEARCH_TEXT = 0;
			COL_REPLACE_TEXT = 1;

		var
			FColorSettings : TFontColors;
			FCurrentHistoryItemIndex : Integer;
			FData : TRipGrepperData;
			FHistoryObjectList : THistoryObjectArray;
			FSettings : TRipGrepperSettings;
			function AddVstHistItem(_nodeData : PVSHistoryNodeData) : PVirtualNode;
			procedure AddVstReplaceNode(Node : PVirtualNode; NodeData : PVSHistoryNodeData);
			procedure ChangeVstReplaceNode(Node : PVirtualNode; const _Data : PVSHistoryNodeData = nil);
			procedure DeleteAllHistoryItems();
			procedure ExpandIfHasChild(const Node : PVirtualNode);
			function GetData : TRipGrepperData;
			function GetHistNodeIndex(Node : PVirtualNode) : integer;
			function GetHistoryObject(const _index : Integer) : THistoryItemObject;
			function GetNodeByIndex(Tree : TVirtualStringTree; Index : Integer) : PVirtualNode;
			function GetSettings : TRipGrepperSettings;
			procedure ShowReplaceColumn(const _bShow : Boolean);
			procedure UpdateReplaceColumnVisible;
			property Settings : TRipGrepperSettings read GetSettings write FSettings;

		protected
			procedure ChangeScale(M, D : Integer; isDpiChange : Boolean); override;

			{ Private-Deklarationen }
		public
			constructor Create(AOwner : TComponent); override;
			procedure AddHistoryObject(_ho : IHistoryItemObject);
			procedure AddOrUpdateHistoryItem;
			procedure ChangeDataHistItemObject(_ho : IHistoryItemObject);
			function ChangeHistoryNodeText() : PVirtualNode;
			procedure ClearMatchesInHistoryObject;
			procedure ClearHistoryObjectList;
			procedure DeleteCurrentHistoryItemFromList;
			function GetCurrentHistoryObject : IHistoryItemObject;
			procedure Init;
			procedure PrepareAndDoSearch;
			procedure RefreshSearch;
			procedure ReloadColorSettings;
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
	Spring;

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
	DeleteAllHistoryItems();
end;

procedure TMiddleLeftFrame.ActionHistoryDeleteAllUpdate(Sender : TObject);
begin
	ActionHistoryDeleteAll.Enabled := VstHistory.Focused and (VstHistory.RootNodeCount <> 0);
end;

procedure TMiddleLeftFrame.ActionHistoryDeleteExecute(Sender : TObject);
var
	ho : IHistoryItemObject;
	Node : PVirtualNode;
	Data : PVSHistoryNodeData;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TMiddleLeftFrame.ActionHistoryDeleteExecute');

	if not VstHistory.Focused then begin
		var
		origFocus := Screen.ActiveControl;
		var
		origFocusedName := IfThen(Assigned(origFocus), origFocus.Name);
		dbgMsg.MsgFmt('Del key pressed somewhere on %s', [origFocusedName]);
		Exit;
	end;

	ho := GetCurrentHistoryObject;

	Node := GetNodeByIndex(VstHistory, CurrentHistoryItemIndex);
	Data := VstHistory.GetNodeData(Node);
	dbgMsg.MsgFmt('idx:%d Node:%s, ho:%s', [CurrentHistoryItemIndex, Data.SearchText, ho.GuiSearchTextParams.GetSearchText]);

	// Assert((Data.SearchText = ho.GuiSearchTextParams.GetSearchText) or
	// (WB + Data.SearchText + WB = ho.GuiSearchTextParams.WordBoundedSearchText) or
	// (TRegEx.Escape(Data.SearchText) = ho.GuiSearchTextParams.EscapedSearchText),
	// Data.SearchText + ' != ' + ho.GuiSearchTextParams.SearchText);

	VstHistory.DeleteNode(Node);
	VstHistory.Refresh;
	DeleteCurrentHistoryItemFromList;
	// FreeAndNil(ho);
	ho := nil;
end;

procedure TMiddleLeftFrame.ActionHistoryDeleteUpdate(Sender : TObject);
begin
	ActionHistoryDelete.Enabled := VstHistory.Focused and (CurrentHistoryItemIndex <> -1);
end;

procedure TMiddleLeftFrame.ActionLoadExecute(Sender : TObject);
begin
	OpenDialog1.InitialDir := ExtractFilePath(Application.ExeName);
	OpenDialog1.Filter := 'DRipGrepper History File (*.drh)|*.drh|All Files (*.*)|*.*';
	OpenDialog1.DefaultExt := 'rgh';
	if OpenDialog1.Execute then begin
		if VstHistory.RootNodeCount > 0 then begin
			if mrYes = TMsgBox.ShowWarning('Existing history will be deleted! Do you want to continue?') then begin
				DeleteAllHistoryItems();
			end else begin
				Exit;
			end;
			DeleteAllHistoryItems();
		end;
		VstHistory.LoadFromFile(OpenDialog1.FileName);
	end;
end;

procedure TMiddleLeftFrame.ActionOpenSearchFormExecute(Sender : TObject);
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TMiddleLeftFrame.ActionOpenSearchFormExecute');
	var
	formResult := TRipGrepperSearchDialogForm.ShowSearchForm(self, Settings, MainFrame.HistItemObject);
	if mrOK = formResult then begin
		dbgMsg.Msg('after ShowSearchForm cmdline: ' + Settings.RipGrepParameters.GetCommandLine(Settings.AppSettings.CopyToClipBoardShell));
		MainFrame.PrepareAndDoSearch();
	end else begin
		dbgMsg.Msg('ShowSearchForm cancel');
	end;

end;

procedure TMiddleLeftFrame.ActionSaveExecute(Sender : TObject);
begin
	SaveDialog1.InitialDir := ExtractFilePath(Application.ExeName);
	SaveDialog1.Filter := 'DRipGrepper History File (*.drh)|*.drh|All Files (*.*)|*.*';
	SaveDialog1.DefaultExt := 'rgh';
	if SaveDialog1.Execute then begin
		VstHistory.SaveToFile(SaveDialog1.FileName);
	end;

end;

procedure TMiddleLeftFrame.AddHistoryObject(_ho : IHistoryItemObject);
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TMiddleLeftFrame.AddHistoryObject');

	dbgMsg.Msg('Add HistoryObject: ' + _ho.SearchText + ' Setting:' + Settings.LastSearchText);
	CurrentHistoryItemIndex := FHistoryObjectList.Add(_ho);
	dbgMsg.Msg('CurrentHistoryItemIndex=' + CurrentHistoryItemIndex.ToString);
end;

procedure TMiddleLeftFrame.AddOrUpdateHistoryItem;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TMiddleLeftFrame.AddOrUpdateHistoryItem');
	dbgMsg.Msg('CurrentHistoryItemIndex ' + CurrentHistoryItemIndex.ToString);

	if not(MainFrame.HistItemObject.HasResult or MainFrame.HistItemObject.IsLoadedFromStream) then begin
		var
			nodeData : TVSHistoryNodeData;
		nodeData.SearchText := Settings.LastSearchText;
		nodeData.ReplaceData.IsReplaceMode := Settings.IsReplaceMode;
		nodeData.ReplaceData.ReplaceText := Settings.LastReplaceText;

		AddVstHistItem(@nodeData);
	end else begin
		ChangeHistoryNodeText;
	end;

	UpdateReplaceColumnVisible;

	if MainFrame.HistItemObject.HasResult then begin
		MainFrame.UpdateHistObjectAndCopyToSettings;
	end;
	MainFrame.UpdateRipGrepArgumentsInHistObj;

	ClearMatchesInHistoryObject();
	dbgMsg.Msg('Update HistoryObject LastSearchText=' + Settings.LastSearchText);
end;

function TMiddleLeftFrame.AddVstHistItem(_nodeData : PVSHistoryNodeData) : PVirtualNode;
var
	Node : PVirtualNode;
	Data : PVSHistoryNodeData;
begin
	Node := VstHistory.AddChild(nil);
	Data := VstHistory.GetNodeData(Node);

	Data^.SearchText := _nodeData.SearchText;
	Data^.ReplaceData.IsReplaceMode := _nodeData^.ReplaceData.IsReplaceMode;
	// only child should be filled, if replace node added, it will be deleted
	Data^.ReplaceData.ReplaceText := _nodeData^.ReplaceData.ReplaceText;

	if _nodeData.ReplaceData.IsReplaceMode then begin
		// VstHistory.MultiLine[Node] := True;
		AddVstReplaceNode(Node, Data);
	end;
	Result := Node;
end;

procedure TMiddleLeftFrame.AddVstReplaceNode(Node : PVirtualNode; NodeData : PVSHistoryNodeData);
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
	dbgMsg.MsgFmt('ReplaceText: %s', [data^.ReplaceData.ReplaceText]);
	NodeData^.ReplaceData.ReplaceText := ''; // only child should be filled
end;

procedure TMiddleLeftFrame.ChangeDataHistItemObject(_ho : IHistoryItemObject);
begin
	// var beu := TBeginEndUpdater.New(VstHistory);
	// First access to data will create MainFrame.Data
	MainFrame.Data.HistObject := _ho;

	SetReplaceMode(_ho);
end;

function TMiddleLeftFrame.ChangeHistoryNodeText() : PVirtualNode;
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
		if Data^.ReplaceData.IsReplaceMode then begin
			ChangeVstReplaceNode(Node, Data);
			ExpandIfHasChild(Node);
		end;
	end;
	VstHistory.Repaint;
	Result := Node;
end;

procedure TMiddleLeftFrame.ChangeVstReplaceNode(Node : PVirtualNode; const _Data : PVSHistoryNodeData = nil);
var
	Data : PVSHistoryNodeData;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TMiddleLeftFrame.ChangeVstReplaceNode');

	Data := _Data;
	if not Assigned(Data) then begin
		Data := VstHistory.GetNodeData(Node);
	end;

	Data^.ReplaceData.IsReplaceMode := Settings.IsReplaceMode;
	dbgMsg.MsgFmt('IsReplaceMode: %s, ChildCount: %d', [BoolToStr(Settings.IsReplaceMode), Node.ChildCount]);

	if Data^.ReplaceData.IsReplaceMode then begin
		if Node.ChildCount = 0 then begin
			AddVstReplaceNode(Node, Data);
		end else begin
			VstHistory.DeleteNode(Node.FirstChild);
			AddVstReplaceNode(Node, Data);
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
	ReloadColorSettings;
	AddOrUpdateHistoryItem;
	SetSelectedHistoryItem(CurrentHistoryItemIndex);
end;

procedure TMiddleLeftFrame.RefreshSearch;
begin
	ClearMatchesInHistoryObject();
	ReloadColorSettings;
end;

procedure TMiddleLeftFrame.ReloadColorSettings;
begin
	// load color settings
	Settings.FontColorSettings.ReloadColors;
	FColorSettings := Settings.FontColorSettings.FontColors;
	VstHistory.Repaint;
end;

procedure TMiddleLeftFrame.SetReplaceMode(_hio : IHistoryItemObject = nil);
var
	hio : IHistoryItemObject;
	mode : TGuiReplaceModes;
	replaceText : string;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TMiddleLeftFrame.SetReplaceMode');

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

	replaceText := '';
	if Assigned(hio) then begin
		replaceText := hio.ReplaceText;
		dbgMsg.MsgFmt('hio.ReplaceText: %s', [replaceText]);
	end;
	ParentFrame.TopFrame.SetGuiReplaceMode(mode, replaceText);
	dbgMsg.MsgFmt('replaceText: %s', [replaceText]);
end;

procedure TMiddleLeftFrame.SetSelectedHistoryItem(const _idx : Integer);
var
	Node : PVirtualNode;
begin
	Node := GetNodeByIndex(VstHistory, _idx);
	if Assigned(Node) then begin
		VstHistory.Selected[Node] := true;
		// ExpandIfHasChild(Node); //automatic through toAutoExpand
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
					TargetCanvas.SetBgColorIfNotTransparent(FColorSettings.SearchTextInHistory.BgColor);
				end else begin
					TargetCanvas.SetBgColorIfNotTransparent(FColorSettings.ReplacedTextInHistory.BgColor);
				end;
			end;
			1 :
			TargetCanvas.SetBgColorIfNotTransparent(FColorSettings.ReplaceTextInHistory.BgColor);
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
	if not Assigned(ho) then begin
		Exit;
	end;
	LineBreakStyle := TVTTooltipLineBreakStyle.hlbForceMultiLine;
	var
	lineBreakTab := CRLF + '  ';
	HintText := RG_EXE + lineBreakTab + string.Join(lineBreakTab, ho.RipGrepArguments.GetValues);
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
	var
	dbgMsg := TDebugMsgBeginEnd.New('TMiddleLeftFrame.VstHistoryNodeClick');
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

	if not Assigned(hio) then
		Exit;

	if TextType = ttNormal then begin
		case Column of
			COL_SEARCH_TEXT : begin
				if hio.IsReplaceMode then begin
					TItemDrawer.SetTextColor(TargetCanvas, FColorSettings.ReplacedTextInHistory);
				end else begin
					TItemDrawer.SetTextColor(TargetCanvas, FColorSettings.SearchTextInHistory);
				end;
			end;
			COL_REPLACE_TEXT : begin
				TItemDrawer.SetTextColor(TargetCanvas, FColorSettings.ReplaceTextInHistory);
			end;
		end
	end else begin // ttStatic
		TItemDrawer.SetTextColorErrorStaticText(TargetCanvas, FColorSettings.CounterText, FColorSettings.ErrorText,
			hio.GetErrorCounters().FSumOfErrors > 0);
	end;
end;

procedure TMiddleLeftFrame.ChangeScale(M, D : Integer; isDpiChange : Boolean);
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TMiddleLeftFrame.ChangeScale');

	inherited ChangeScale(M, D, isDpiChange);
	if isDpiChange then begin
		dbgMsg.MsgFmt('M(%d) / D(%d) = %d%%', [M, D, MulDiv(100, M, D)]);
		dbgMsg.MsgFmt('Orig VstHistory Fonts: %d', [VstHistory.Font.Height]);

		VstHistory.Header.Font.Height := MulDiv(VstHistory.Header.Font.Height, M, D);
		// VstHistory.Font.Height := MulDiv(VstHistory.Font.Height, M, D); it's too much!
		dbgMsg.MsgFmt('New VstHistory Fonts: %d', [VstHistory.Font.Height]);
	end;
end;

procedure TMiddleLeftFrame.DeleteAllHistoryItems();
begin
	MainFrame.VstResult.Clear;
	VstHistory.Clear;
	ClearHistoryObjectList;
	MainFrame.HistItemObject := nil;
end;

procedure TMiddleLeftFrame.ExpandIfHasChild(const Node : PVirtualNode);
begin
	if Node.ChildCount > 0 then begin // if replace node is the first it is closed
		VstHistory.Expanded[Node] := True;
	end;
end;

procedure TMiddleLeftFrame.VstHistoryLoadTree(Sender : TBaseVirtualTree; Stream : TStream);
var
	hio : IHistoryItemObject;
	sr : IShared<TStreamReader>;
	count : integer;
	nodeData : TVSHistoryNodeData;
begin
	sr := Shared.Make<TStreamReader>(TStreamReader.Create(Stream));
	count := StrToInt(sr.ReadLine());

	for var i := 0 to count - 1 do begin
		hio := THistoryItemObject.Create;
		hio.LoadFromStreamReader(sr);
		AddHistoryObject(hio);

		nodeData.SearchText := hio.GetSearchTextWithOptions().SearchTextOfUser;
		nodeData.ReplaceData.IsReplaceMode := False;
		nodeData.ReplaceData.ReplaceText := '';

		AddVstHistItem(@nodeData);
	end;
end;

procedure TMiddleLeftFrame.VstHistorySaveTree(Sender : TBaseVirtualTree; Stream : TStream);
var
	hio : IHistoryItemObject;
	idx : integer;
	sw : IShared<TStreamWriter>;
begin
	sw := Shared.Make<TStreamWriter>(TStreamWriter.Create(Stream));
	sw.WriteLine(VstHistory.RootNodeCount);
	for var node : PVirtualNode in VstHistory.Nodes() do begin
		idx := GetHistNodeIndex(node);
		hio := GetHistoryObject(idx);
		hio.SaveToStreamWriter(sw);
	end;
end;

end.
