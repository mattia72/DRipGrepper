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
	SVGIconImageList,
	RipGrepper.UI.IFrameEvents,
	System.UITypes;

type

	TMiddleLeftFrame = class(TFrame, IFrameEvents)
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
		procedure VstHistoryGetImageIndex(Sender : TBaseVirtualTree; Node : PVirtualNode; Kind : TVTImageKind; Column : TColumnIndex;
			var Ghosted : Boolean; var ImageIndex : TImageIndex);
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
			IMAGE_IDX_X = 4;

		var
			FColorSettings : TFontColors;
			FCurrentHistoryItemIndex : Integer;
			FData : TRipGrepperData;
			FHistoryObjectList : THistoryObjectArray;
			FIsInitialized : Boolean;
			FSettings : TRipGrepperSettings;
			function AddVstHistItem(_nodeData : PVSHistoryNodeData) : PVirtualNode;
			procedure AddVstReplaceNode(Node : PVirtualNode; NodeData : PVSHistoryNodeData);
			procedure ChangeVstReplaceNode(Node : PVirtualNode; const _Data : PVSHistoryNodeData = nil);
			function CountSaveNodes() : integer;
			procedure DeleteAllHistoryItems();
			procedure DeleteCurrentNode;
			procedure DeleteHistItemNode(const node : PVirtualNode);
			procedure ExpandIfHasChild(const Node : PVirtualNode);
			function GetCurrentValidHistoryObject() : IHistoryItemObject;
			function GetData : TRipGrepperData;
			function GetHistoryItemNodeIndex(Node : PVirtualNode) : integer;
			function GetHistoryObject(const _index : Integer) : THistoryItemObject;
			function GetIsInitialized() : Boolean;
			function GetNodeByIndex(Tree : TVirtualStringTree; Index : Integer) : PVirtualNode;
			function GetSettings : TRipGrepperSettings;
			function IsIndexIsInLastHistoryCount(const _idx : Integer): Boolean;
			function NodeDataFromStream(const sr : TStreamReader) : TVSHistoryNodeData;
			procedure ShowReplaceColumn(const _bShow : Boolean);
			procedure UpdateReplaceColumnVisible;
			property Settings : TRipGrepperSettings read GetSettings write FSettings;

		protected
			procedure ChangeScale(M, D : Integer; isDpiChange : Boolean); override;

			{ Private-Deklarationen }
		public
			constructor Create(AOwner : TComponent); override;
			procedure AddHistoryObject(_ho : IHistoryItemObject);
			function AddOrUpdateHistoryItem() : IHistoryItemObject;
			procedure AfterHistObjChange();
			procedure AfterSearch();
			procedure BeforeSearch(var _bAbort : Boolean);
			procedure ChangeDataHistItemObject(_ho : IHistoryItemObject);
			function ChangeHistoryNodeText() : PVirtualNode;
			procedure ClearMatchesInHistoryObject;
			procedure ClearHistoryObjectList;
			procedure DeleteCurrentHistoryItemFromList;
			function GetCurrentHistoryObject : IHistoryItemObject;
			procedure Initialize();
			procedure PrepareAndDoSearch;
			procedure RefreshSearch;
			procedure ReloadColorSettings;
			procedure SetReplaceMode(_hio : IHistoryItemObject = nil);
			procedure SetSelectedHistoryItem(const _idx : Integer);
			procedure UpdateUIStyle(_sNewStyle : string = '');
			property CurrentHistoryItemIndex : Integer read FCurrentHistoryItemIndex write FCurrentHistoryItemIndex;
			property Data : TRipGrepperData read GetData write FData;
			property IsInitialized : Boolean read GetIsInitialized;
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
	Spring,
	RipGrepper.Settings.AppSettings;

{$R *.dfm}

constructor TMiddleLeftFrame.Create(AOwner : TComponent);
begin
	inherited;
	TDebugUtils.DebugMessage('TMiddleLeftFrame.Create ' + AOwner.Name);
	MiddleLeftFrame := self;
	FIsInitialized := False;
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

	DeleteCurrentNode;
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
			if mrYes = TMsgBox.ShowQuestion('Existing history will be deleted! Do you want to continue?') then begin
				DeleteAllHistoryItems();
			end else begin
				Exit;
			end;
		end;
		try
			VstHistory.LoadFromFile(OpenDialog1.FileName);
		except
			on E : Exception do
				TMsgBox.ShowError('Error occurred while loading saved searches.');
		end;
	end;
end;

procedure TMiddleLeftFrame.ActionOpenSearchFormExecute(Sender : TObject);
var
	formResult : TModalResult;
	hio : IHistoryItemObject;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TMiddleLeftFrame.ActionOpenSearchFormExecute');

	hio := GetCurrentValidHistoryObject();
	formResult := TRipGrepperSearchDialogForm.ShowSearchForm(self, Settings, hio);
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

function TMiddleLeftFrame.AddOrUpdateHistoryItem() : IHistoryItemObject;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TMiddleLeftFrame.AddOrUpdateHistoryItem');
	dbgMsg.Msg('CurrentHistoryItemIndex ' + CurrentHistoryItemIndex.ToString);

	Result := GetCurrentValidHistoryObject();

	if not(Result.HasResult or Result.IsLoadedFromStream) then begin
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

	if Result.HasResult then begin
		MainFrame.UpdateHistObjectAndCopyToSettings;
	end;
	if not Result.IsLoadedFromStream then begin
		MainFrame.UpdateRipGrepArgumentsInHistObj;
		ClearMatchesInHistoryObject();
	end;

	dbgMsg.Msg('Update HistoryObject LastSearchText=' + Settings.LastSearchText);
end;

function TMiddleLeftFrame.AddVstHistItem(_nodeData : PVSHistoryNodeData) : PVirtualNode;
var
	node : PVirtualNode;
	nodeData : PVSHistoryNodeData;
begin
	node := VstHistory.AddChild(nil);
	nodeData := VstHistory.GetNodeData(node);

	nodeData^.IsFromStream := _nodeData.IsFromStream;
	nodeData^.SearchText := _nodeData.SearchText;
	nodeData^.ReplaceData.IsReplaceMode := _nodeData^.ReplaceData.IsReplaceMode;
	// only child should be filled, if replace node added, it will be deleted
	nodeData^.ReplaceData.ReplaceText := _nodeData^.ReplaceData.ReplaceText;

	if _nodeData.ReplaceData.IsReplaceMode then begin
		// VstHistory.MultiLine[node] := True;
		AddVstReplaceNode(node, nodeData);
	end;
	Result := node;
end;

procedure TMiddleLeftFrame.AddVstReplaceNode(Node : PVirtualNode; NodeData : PVSHistoryNodeData);
var
	childNode : PVirtualNode;
	childData : PVSHistoryNodeData;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TMiddleLeftFrame.AddVstReplaceNode');
	childNode := VstHistory.AddChild(Node);
	childData := VstHistory.GetNodeData(childNode);
	childData^.SearchText := '';
	childData^.ReplaceData.IsReplaceMode := True;
	childData^.ReplaceData.ReplaceText := Settings.LastReplaceText;
	dbgMsg.MsgFmt('ReplaceText: %s', [childData^.ReplaceData.ReplaceText]);
	NodeData^.ReplaceData.ReplaceText := ''; // only child should be filled
end;

procedure TMiddleLeftFrame.AfterHistObjChange();
begin
	// TODO -cMM: TMiddleLeftFrame.AfterHistObjChange default body inserted
end;

procedure TMiddleLeftFrame.AfterSearch();
begin
	ChangeHistoryNodeText;
end;

procedure TMiddleLeftFrame.BeforeSearch(var _bAbort : Boolean);
begin
	// TODO -cMM: TMiddleLeftFrame.BeforeSearch default body inserted
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

function TMiddleLeftFrame.GetHistoryItemNodeIndex(Node : PVirtualNode) : integer;
begin
	if (Node.Parent = VstHistory.RootNode) then begin
		Result := Node.Index;
	end else begin
		Result := Node.Parent.Index;
	end;
end;

function TMiddleLeftFrame.GetHistoryObject(const _index : Integer) : THistoryItemObject;
begin
	// VsHistroyGetHint produces a lot of calls to this function
	// var
	// dbgMsg := TDebugMsgBeginEnd.New('TMiddleLeftFrame.GetHistoryObject',True);
	// dbgMsg.MsgFmt('at index %d', [_index]);
	Result := nil;
	if (_index > -1) and (_index < FHistoryObjectList.Count) then begin
		Result := THistoryItemObject(FHistoryObjectList[_index]);
		// dbgMsg.MsgFmt('Result hio: %s', [Result.SearchText]);
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

procedure TMiddleLeftFrame.Initialize();
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TMiddleLeftFrame.Initialize');

	if IsInitialized then begin
		dbgMsg.Msg('Already initialized');
		Exit;
	end;

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
	FIsInitialized := True;
end;

procedure TMiddleLeftFrame.PopupMenuHistoryPopup(Sender : TObject);
begin
	SetSelectedHistoryItem(CurrentHistoryItemIndex);
end;

procedure TMiddleLeftFrame.PrepareAndDoSearch;
var
	hio : IHistoryItemObject;
begin
	ReloadColorSettings;
	hio := AddOrUpdateHistoryItem;
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
	node : PVirtualNode;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TMiddleLeftFrame.VstHistoryNodeClick');
	node := HitInfo.HitNode;
	idx := GetHistoryItemNodeIndex(node);

	if (node.Parent = Sender.RootNode) and (hiOnNormalIcon in HitInfo.HitPositions) then begin
		DeleteHistItemNode(node);
		Exit;
	end;

	if (CurrentHistoryItemIndex <> idx) then begin
		CurrentHistoryItemIndex := idx;
		ParentFrame.AfterHistObjChange();
		// MainFrame.UpdateHistObjectAndGui;
	end;

	{$IFDEF debug}
	var
		data : PVSHistoryNodeData := VstHistory.GetNodeData(HitInfo.HitNode);
	if data.ReplaceData.IsReplaceMode then begin
		VstHistory.Expanded[node] := node.ChildCount > 0;
		var
			childData : PVSHistoryNodeData := VstHistory.GetNodeData(node.FirstChild);
		dbgMsg.MsgFmt('%s -> %s', [data.SearchText, childData.ReplaceData.ReplaceText]);
	end;
	{$ENDIF}
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
	idx := GetHistoryItemNodeIndex(Node);
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

function TMiddleLeftFrame.CountSaveNodes() : integer;
var
	hio : THistoryItemObject;
	appSettings : TAppSettings;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TMiddleLeftFrame.CountSaveNodes');

	appSettings := Settings.AppSettings;
	if llsmAll = appSettings.LoadLastSearchHistoryMode then begin
		Result := VstHistory.RootNodeCount;
		exit
	end;
	Result := 0;
	for var node : PVirtualNode in VstHistory.Nodes() do begin
		if node.Parent <> VstHistory.RootNode then begin
			dbgMsg.Msg('Child node skipped');
			continue;
		end;
		var
		idx := GetHistoryItemNodeIndex(node);
		hio := GetHistoryObject(idx);

		dbgMsg.MsgFmt('check : %s', [hio.SearchText]);

		case appSettings.LoadLastSearchHistoryMode of
			llsmHasResultOnly : begin
				if hio.HasResult then begin
					Inc(Result);
					dbgMsg.MsgFmt('has result : %d', [Result]);
				end
			end;
			llsmMaxCount : begin
				if IsIndexIsInLastHistoryCount(idx) then begin
					Inc(Result);
					dbgMsg.MsgFmt('idx %d >= %d', [idx, Result]);
				end
			end;
			else begin
				dbgMsg.Msg('skip');
			end;
		end;
	end;
end;

procedure TMiddleLeftFrame.DeleteAllHistoryItems();
begin
	MainFrame.VstResult.Clear;
	VstHistory.Clear;
	ClearHistoryObjectList;
	MainFrame.HistItemObject := nil;
end;

procedure TMiddleLeftFrame.DeleteCurrentNode;
var
	ho : IHistoryItemObject;
	node : PVirtualNode;
	{$IFDEF DEBUG}
	data : PVSHistoryNodeData;
	{$ENDIF}
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TMiddleLeftFrame.DeleteCurrentNode');

	ho := GetCurrentHistoryObject;

	node := GetNodeByIndex(VstHistory, CurrentHistoryItemIndex);
	{$IFDEF DEBUG}
	data := VstHistory.GetNodeData(node);
	dbgMsg.MsgFmt('idx:%d node:%s, ho:%s', [CurrentHistoryItemIndex, data.SearchText, ho.GuiSearchTextParams.GetSearchText]);
	{$ENDIF}
	VstHistory.DeleteNode(node);
	VstHistory.Refresh;
	DeleteCurrentHistoryItemFromList;

	ho := nil;
end;

procedure TMiddleLeftFrame.DeleteHistItemNode(const node : PVirtualNode);
var
	idx : integer;
begin
	idx := GetHistoryItemNodeIndex(node);

	VstHistory.DeleteNode(node);
	VstHistory.Refresh;
	if idx = CurrentHistoryItemIndex then begin
		DeleteCurrentHistoryItemFromList;
	end else begin
		FHistoryObjectList.Delete(idx);
	end;
end;

procedure TMiddleLeftFrame.ExpandIfHasChild(const Node : PVirtualNode);
begin
	if Node.ChildCount > 0 then begin // if replace node is the first it is closed
		VstHistory.Expanded[Node] := True;
	end;
end;

function TMiddleLeftFrame.GetCurrentValidHistoryObject() : IHistoryItemObject;
begin
	Result := MainFrame.HistItemObject;
	if not Assigned(Result) then begin
		Result := GetCurrentHistoryObject();
	end;
end;

function TMiddleLeftFrame.GetIsInitialized() : Boolean;
begin
	Result := FIsInitialized;
end;

function TMiddleLeftFrame.IsIndexIsInLastHistoryCount(const _idx : Integer): Boolean;
begin
	Result := _idx >= (Integer(VstHistory.RootNodeCount) - Settings.AppSettings.SearchHistoryCount);
end;

function TMiddleLeftFrame.NodeDataFromStream(const sr : TStreamReader) : TVSHistoryNodeData;
var
	hio : IHistoryItemObject;
begin
	hio := THistoryItemObject.Create;
	hio.LoadFromStreamReader(sr);
	AddHistoryObject(hio);

	Result.SearchText := hio.GetSearchTextWithOptions().SearchTextOfUser;
	Result.ReplaceData.IsReplaceMode := hio.IsReplaceMode;
	Result.ReplaceData.ReplaceText := hio.ReplaceText;

	Settings.LastReplaceText := hio.ReplaceText;
	Settings.LastSearchText := Result.SearchText;
end;

procedure TMiddleLeftFrame.UpdateUIStyle(_sNewStyle : string = '');
begin
	// TODO -cMM: TMiddleLeftFrame.UpdateUIStyle default body inserted
end;

procedure TMiddleLeftFrame.VstHistoryGetImageIndex(Sender : TBaseVirtualTree; Node : PVirtualNode; Kind : TVTImageKind;
	Column : TColumnIndex; var Ghosted : Boolean; var ImageIndex : TImageIndex);
begin
	case Kind of
		ikNormal, ikSelected :
		case Column of
			COL_SEARCH_TEXT : begin
				var
				showNodeIcon := (Node.Parent = Sender.RootNode);
				ImageIndex := -1;
				Ghosted := False;
				if (Sender.HotNode = Node) and showNodeIcon then begin
					ImageIndex := IMAGE_IDX_X;
					Ghosted := True;
				end;
			end;
		end;
	end;
end;

procedure TMiddleLeftFrame.VstHistoryLoadTree(Sender : TBaseVirtualTree; Stream : TStream);
var
	sr : IShared<TStreamReader>;
	count : integer;
	nodeData : TVSHistoryNodeData;
begin
	sr := Shared.Make<TStreamReader>(TStreamReader.Create(Stream));
	count := StrToIntDef(sr.ReadLine(), 0);

	VstHistory.Clear; // LoadFile creates the nodes, we should clear it
	for var i := 0 to count - 1 do begin
		nodeData := NodeDataFromStream(sr);
		nodeData.IsFromStream := True;
		AddVstHistItem(@nodeData);
	end;
	UpdateReplaceColumnVisible;
end;

procedure TMiddleLeftFrame.VstHistorySaveTree(Sender : TBaseVirtualTree; Stream : TStream);
var
	hio : IHistoryItemObject;
	idx : integer;
	sw : IShared<TStreamWriter>;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TMiddleLeftFrame.VstHistorySaveTree');
	sw := Shared.Make<TStreamWriter>(TStreamWriter.Create(Stream));
	sw.WriteLine(CountSaveNodes());

	for var node : PVirtualNode in VstHistory.Nodes() do begin
		if node.Parent <> VstHistory.RootNode then begin
			dbgMsg.Msg('Child node skipped');
			continue;
		end;

		idx := GetHistoryItemNodeIndex(node);
		hio := GetHistoryObject(idx);
		var
			appSettings : TAppSettings;
		appSettings := Settings.AppSettings;
		case appSettings.LoadLastSearchHistoryMode of
			llsmAll : begin
				dbgMsg.MsgFmt('Save idx: %d - %s', [idx, hio.SearchText]);
				hio.SaveToStreamWriter(sw);
			end;
			llsmHasResultOnly : begin
				if hio.HasResult then begin
					dbgMsg.MsgFmt('Save idx: %d - %s', [idx, hio.SearchText]);
					hio.SaveToStreamWriter(sw);
				end else begin
					dbgMsg.MsgFmt('Has no result, skip idx: %d - %s', [idx, hio.SearchText], ETraceFilterType.tftVerbose);
				end;
			end;
			llsmMaxCount : begin
				if IsIndexIsInLastHistoryCount(idx) then begin
					dbgMsg.MsgFmt('Save idx: %d - %s', [idx, hio.SearchText]);
					hio.SaveToStreamWriter(sw);
				end else begin
					dbgMsg.MsgFmt('Idx is not in last max count, skip idx: %d - %s', [idx, hio.SearchText], ETraceFilterType.tftVerbose);
				end;
			end;
		end;
	end;
end;

end.
