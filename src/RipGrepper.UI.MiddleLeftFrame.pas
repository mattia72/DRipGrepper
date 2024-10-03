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
	RipGrepper.Common.Settings.RipGrepperSettings,
	ArrayEx,
	RipGrepper.Common.Interfaces,
	RipGrepper.Data.Matches,
	RipGrepper.Data.HistoryItemObject;

type
	THistoryObjectArray = TArrayEx<IHistoryItemObject>;

	TMiddleLeftFrame = class(TFrame)
		VstHistory : TVirtualStringTree;
		ActionList : TActionList;
		ActionHistoryDelete : TAction;
		ActionHistoryDeleteAll : TAction;
		ActionCopyCmdLineToClipboard : TAction;
		ActionOpenSearchForm : TAction;
		procedure ActionHistoryDeleteAllExecute(Sender : TObject);
		procedure ActionHistoryDeleteAllUpdate(Sender : TObject);
		procedure ActionHistoryDeleteExecute(Sender : TObject);
		procedure ActionHistoryDeleteUpdate(Sender : TObject);
		procedure ActionOpenSearchFormExecute(Sender : TObject);
		procedure PopupMenuHistoryPopup(Sender : TObject);
		procedure VstHistoryDrawText(Sender : TBaseVirtualTree; TargetCanvas : TCanvas; Node : PVirtualNode; Column : TColumnIndex;
			const Text : string; const CellRect : TRect; var DefaultDraw : Boolean);
		procedure VstHistoryFreeNode(Sender : TBaseVirtualTree; Node : PVirtualNode);
		procedure VstHistoryGetHint(Sender : TBaseVirtualTree; Node : PVirtualNode; Column : TColumnIndex;
			var LineBreakStyle : TVTTooltipLineBreakStyle; var HintText : string);
		procedure VstHistoryGetHintKind(Sender : TBaseVirtualTree; Node : PVirtualNode; Column : TColumnIndex; var Kind : TVTHintKind);
		procedure VstHistoryGetText(Sender : TBaseVirtualTree; Node : PVirtualNode; Column : TColumnIndex; TextType : TVSTTextType;
			var CellText : string);
		procedure VstHistoryMeasureItem(Sender : TBaseVirtualTree; TargetCanvas : TCanvas; Node : PVirtualNode; var NodeHeight : Integer);
		procedure VstHistoryNodeClick(Sender : TBaseVirtualTree; const HitInfo : THitInfo);
		procedure VstHistoryNodeDblClick(Sender : TBaseVirtualTree; const HitInfo : THitInfo);

		private
			FCurrentHistoryItemIndex : Integer;
			FData : TRipGrepperData;
			FHistoryObjectList : THistoryObjectArray;
			FSettings : TRipGrepperSettings;
			procedure AddVstHistItem;
			function GetData : TRipGrepperData;
			function GetHistoryObject(const _index : Integer) : THistoryItemObject;
			function GetNodeByIndex(Tree : TVirtualStringTree; Index : Integer) : PVirtualNode;
			function GetSettings : TRipGrepperSettings;
			property Settings : TRipGrepperSettings read GetSettings write FSettings;

			{ Private-Deklarationen }
		public
			constructor Create(AOwner : TComponent); override;
			procedure AddHistoryObject(_ho : IHistoryItemObject);
			procedure AddOrUpdateHistoryItem;
			procedure ChangeDataHistItemObject(_ho : IHistoryItemObject);
			procedure ChangeHistoryNodeText;
			procedure ClearHistoryObject;
			procedure ClearHistoryObjectList;
			procedure DeleteCurrentHistoryItemFromList;
			function GetCurrentHistoryObject : IHistoryItemObject;
			procedure Init;
			procedure PrepareAndDoSearch;
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
	RipGrepper.Helper.Types;

{$R *.dfm}

constructor TMiddleLeftFrame.Create(AOwner : TComponent);
begin
	inherited;
	TDebugUtils.DebugMessage('TMiddleLeftFrame.Create ' + AOwner.Name);
	MiddleLeftFrame := self;
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
	ho := (* TODO: extracted code // GetHistoryObject(CurrentHistoryItemIndex) *) GetCurrentHistoryObject;

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
	formResult := TRipGrepperSearchDialogForm.ShowSearchForm(self, Settings, MainFrame.HistItemObj);
	if mrOK = formResult then begin
		TDebugUtils.DebugMessage('TRipGrepperTopFrame.VstHistoryNodeDblClick: after ShowSearchForm cmdline: ' +
			Settings.RipGrepParameters.GetCommandLine);
		PrepareAndDoSearch();
	end else begin
		TDebugUtils.DebugMessage('TRipGrepperTopFrame.VstHistoryNodeDblClick: ShowSearchForm cancel');
	end;

	ChangeHistoryNodeText;
	MainFrame.UpdateHistObjectAndGui;
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
	end;
	ChangeHistoryNodeText;
	MainFrame.UpdateRipGrepArgumentsInHistObj;
	MainFrame.UpdateHistObject;
	ClearHistoryObject();
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
	Data^.IsReplaceMode := Settings.IsReplaceMode;
	Data^.ReplaceText := Settings.LastReplaceText;
	VstHistory.MultiLine[Node] := True;
end;

procedure TMiddleLeftFrame.ChangeDataHistItemObject(_ho : IHistoryItemObject);
begin
	var
	beu := TBeginEndUpdater.New(VstHistory);
	// First access to data will create MainFrame.Data
	MainFrame.Data.HistObject := _ho;
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
		Data^.IsReplaceMode := Settings.IsReplaceMode;
		Data^.ReplaceText := Settings.LastReplaceText;
	end;
end;

procedure TMiddleLeftFrame.ClearHistoryObject;
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

	VstHistory.TreeOptions.StringOptions := VstHistory.TreeOptions.StringOptions + [toShowStaticText];
	VstHistory.TreeOptions.PaintOptions := VstHistory.TreeOptions.PaintOptions + [toUseExplorerTheme];
	VstHistory.TreeOptions.MiscOptions := VstHistory.TreeOptions.MiscOptions + [TVTMiscOption.toVariablenodeHeight];
	VstHistory.NodeDataSize := SizeOf(TVSHistoryNodeData);
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

procedure TMiddleLeftFrame.SetSelectedHistoryItem(const _idx : Integer);
var
	Node : PVirtualNode;
begin
	Node := GetNodeByIndex(VstHistory, _idx);
	if Assigned(Node) then begin
		VstHistory.Selected[Node] := true;
	end;
end;

procedure TMiddleLeftFrame.VstHistoryDrawText(Sender : TBaseVirtualTree; TargetCanvas : TCanvas; Node : PVirtualNode; Column : TColumnIndex;
	const Text : string; const CellRect : TRect; var DefaultDraw : Boolean);
var
	sSearchText, sStatistic : string;
	lineBegin : Integer;
	size : Winapi.Windows.TSize;
	rectTemp : TRect;
begin
	case Column of
		0 : begin
			// First, store the default font size and color number
			var
			backup := TDrawParams.Save(TargetCanvas);

			DefaultDraw := False;
			lineBegin := Text.LastIndexOf(CRLF);
			sSearchText := Text.Substring(0, lineBegin);

			TargetCanvas.Font.Color := HIST_TREEVIEW_SEARCH_TEXT_COLOR;
			TargetCanvas.Brush.Color := HIST_TREEVIEW_SEARCH_TEXT_BGCOLOR;
			TargetCanvas.Font.style := HIST_TREEVIEW_SEARCH_TEXT_STYLE;

			// TargetCanvas.TextOut(CellRect.Left, TREEVIEW_FONTSPACE, sSearchText);
			rectTemp := CellRect;
			Winapi.Windows.DrawText(TargetCanvas.Handle, pwidechar(sSearchText), length(sSearchText), rectTemp,
				DT_NOPREFIX or DT_WORDBREAK);

			sStatistic := Text.Substring(lineBegin + 2);
			if -1 <> sStatistic.IndexOf(TREEVIEW_HISTORY_COUNTER_ERROR_PREFIX) then begin
				TargetCanvas.Font.Color := TREEVIEW_ERROR_COLOR;
				TargetCanvas.Font.style := [fsBold];
			end else begin
				TargetCanvas.Font.Color := TREEVIEW_STAT_COLOR;
				TargetCanvas.Font.style := [];
			end;

			size := TFontSizeHelper.TrueFontSize(TargetCanvas.Font, sStatistic);
			TargetCanvas.TextOut(CellRect.Left + TREEVIEW_FONTSPACE * 4, CellRect.BottomRight.Y - size.cy, sStatistic);

			backup.Load(TargetCanvas);
		end;
	end;
end;

procedure TMiddleLeftFrame.VstHistoryFreeNode(Sender : TBaseVirtualTree; Node : PVirtualNode);
var
	Data : PVSHistoryNodeData;
begin
	Data := VstHistory.GetNodeData(Node);
	Data.SearchText := '';
	Data.ReplaceText := '';
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

	if TextType = ttNormal then begin
		// if Data.IsReplaceMode then begin
		// case Column of
		// 0 :
		CellText := Data.SearchText + CRLF + MainFrame.GetCounterText(GetHistoryObject(Node.Index));
		// 1 :
		// CellText := Data.ReplaceText
		// end;
		//
		// end else begin
		// case Column of
		// 0 :
		// CellText := Data.SearchText + CRLF + GetCounterText(GetHistoryObject(Node.Index));
		// end;
		// end;
	end else begin // ttStatic not shown in Multiline cell
		CellText := MainFrame.GetCounterText(GetHistoryObject(Node.Index));
	end;
end;

procedure TMiddleLeftFrame.VstHistoryMeasureItem(Sender : TBaseVirtualTree; TargetCanvas : TCanvas; Node : PVirtualNode;
	var NodeHeight : Integer);
begin
	if Sender.MultiLine[Node] then begin
		TargetCanvas.Font := Sender.Font;
		NodeHeight := VstHistory.ComputeNodeHeight(TargetCanvas, Node, -1);
	end;
end;

procedure TMiddleLeftFrame.VstHistoryNodeClick(Sender : TBaseVirtualTree; const HitInfo : THitInfo);
begin
	if (CurrentHistoryItemIndex = Integer(HitInfo.HitNode.Index)) then
		Exit;

	CurrentHistoryItemIndex := HitInfo.HitNode.Index;
	MainFrame.UpdateHistObjectAndGui;
end;

procedure TMiddleLeftFrame.VstHistoryNodeDblClick(Sender : TBaseVirtualTree; const HitInfo : THitInfo);
begin
	TDebugUtils.DebugMessage('TMiddleLeftFrame.VstHistoryNodeDblClick: idx = ' + HitInfo.HitNode.Index.ToString);
	VstHistoryNodeClick(Sender, HitInfo);
	ActionOpenSearchFormExecute(Sender);
end;

end.
