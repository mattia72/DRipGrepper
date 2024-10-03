unit RipGrepper.UI.MiddleFrame;

interface

uses
	Winapi.Windows,
	Winapi.Messages,
	System.SysUtils,
	System.Variants,
	System.Classes,
	Vcl.Graphics,
	Vcl.Controls,
	Vcl.Forms,
	Vcl.Dialogs,
	Vcl.Menus,
	System.Actions,
	Vcl.ActnList,
	System.ImageList,
	Vcl.ImgList,
	Vcl.WinXCtrls,
	Vcl.ComCtrls,
	Vcl.StdCtrls,
	Vcl.ExtCtrls,
	Vcl.ToolWin,
	RipGrepper.OpenWith.Constants,
	RipGrepper.OpenWith.Params,
	RipGrepper.Common.Settings.AppSettings,
	RipGrepper.Data.HistoryItemObject,
	RipGrepper.Data.Matches,
	RipGrepper.Common.Sorter,
	RipGrepper.Common.Interfaces,
	System.Diagnostics,
	RipGrepper.Common.Constants,
	System.Threading,
	VirtualTrees, // GetIt TurboPack VirtualTree
	RipGrepper.Helper.UI,
	RipGrepper.Parsers.ParallelParser,
	ArrayEx,
	RipGrepper.Tools.ProcessUtils,
	RipGrepper.Helper.Types,
	RipGrepper.Common.Settings.RipGrepParameterSettings,
	RipGrepper.Common.ParsedObject,
	RipGrepper.Common.Settings.RipGrepperSettings;

type
	TDrawParams = record
		FgColor : TColor;
		BgColor : TColor;
		FontSize : TColor;
		FontStyle : TFontStyles;
		class function Save(const _canvas : TCanvas) : TDrawParams; static;
		procedure Load(const _canvas : TCanvas);
	end;

	THistoryObjectList = TArrayEx<IHistoryItemObject>;

	TRipGrepperMiddleFrame = class(TFrame, INewLineEventHandler, ITerminateEventProducer, IEOFProcessEventHandler)
		ActionList : TActionList;
		ActionCopyFileName : TAction;
		ActionCopyPathToClipboard : TAction;
		ActionOpenWith : TAction;
		ActionOpenInIde : TAction;
		PopupMenuResult : TPopupMenu;
		miOpenwith1 : TMenuItem;
		miCopyFileNameToClipboard : TMenuItem;
		miCopyPathToClipboard : TMenuItem;
		ImageListListView : TImageList;
		panelMain : TPanel;
		SplitView1 : TSplitView;
		Splitter1 : TSplitter;
		PanelHistory : TPanel;
		PanelResult : TPanel;
		VstResult : TVirtualStringTree;
		ImageList1 : TImageList;
		VstHistory : TVirtualStringTree;
		PopupMenuHistory : TPopupMenu;
		ActionHistoryDelete : TAction;
		ActionHistoryDeleteAll : TAction;
		miResultAddAsUsingInterface : TMenuItem;
		miAddAsUsingImplementation : TMenuItem;
		ActionAddUsingImplementation : TAction;
		ActionAddUsingInterface : TAction;
		ActionCopyLineToClipboard : TAction;
		ActionCopyMatchToClipboard : TAction;
		miCopyLine : TMenuItem;
		miCopyMatchingText : TMenuItem;
		miAddToUSESList : TMenuItem;
		miCopytoClipboard : TMenuItem;
		N1 : TMenuItem;
		miOpenInIde : TMenuItem;
		ActionCopyCmdLineToClipboard : TAction;
		N2 : TMenuItem;
		pmCopyCommandLine : TMenuItem;
		N3 : TMenuItem;
		pmOpenSearchForm : TMenuItem;
		procedure ActionAddUsingImplementationExecute(Sender : TObject);
		procedure ActionAddUsingImplementationUpdate(Sender : TObject);
		procedure ActionAddUsingInterfaceExecute(Sender : TObject);
		procedure ActionAddUsingInterfaceUpdate(Sender : TObject);
		procedure ActionCopyCmdLineToClipboardExecute(Sender : TObject);
		procedure ActionOpenSearchFormExecute(Sender : TObject);
		procedure ActionCopyFileNameExecute(Sender : TObject);
		procedure ActionCopyFileNameUpdate(Sender : TObject);
		procedure ActionCopyLineToClipboardExecute(Sender : TObject);
		procedure ActionCopyLineToClipboardUpdate(Sender : TObject);
		procedure ActionCopyMatchToClipboardExecute(Sender : TObject);
		procedure ActionCopyMatchToClipboardUpdate(Sender : TObject);
		procedure ActionCopyPathToClipboardExecute(Sender : TObject);
		procedure ActionCopyPathToClipboardUpdate(Sender : TObject);
		procedure ActionHistoryDeleteAllExecute(Sender : TObject);
		procedure ActionHistoryDeleteAllUpdate(Sender : TObject);
		procedure ActionHistoryDeleteExecute(Sender : TObject);
		procedure ActionHistoryDeleteUpdate(Sender : TObject);
		procedure ActionOpenWithExecute(Sender : TObject);
		procedure ActionOpenWithUpdate(Sender : TObject);
		procedure ActionOpenInIdeExecute(Sender : TObject);
		procedure ActionOpenInIdeUpdate(Sender : TObject);
		procedure FrameResize(Sender : TObject);
		procedure PopupMenuHistoryPopup(Sender : TObject);
		procedure Splitter1Moved(Sender : TObject);
		procedure SplitView1Resize(Sender : TObject);
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
		procedure VstResultBeforeCellPaint(Sender : TBaseVirtualTree; TargetCanvas : TCanvas; Node : PVirtualNode; Column : TColumnIndex;
			CellPaintMode : TVTCellPaintMode; CellRect : TRect; var ContentRect : TRect);
		procedure VstResultCompareNodes(Sender : TBaseVirtualTree; Node1, Node2 : PVirtualNode; Column : TColumnIndex;
			var Result : Integer);
		procedure VstResultDblClick(Sender : TObject);
		procedure VstResultDrawText(Sender : TBaseVirtualTree; TargetCanvas : TCanvas; Node : PVirtualNode; Column : TColumnIndex;
			const Text : string; const CellRect : TRect; var DefaultDraw : Boolean);
		procedure VstResultFreeNode(Sender : TBaseVirtualTree; Node : PVirtualNode);
		procedure VstResultGetImageIndex(Sender : TBaseVirtualTree; Node : PVirtualNode; Kind : TVTImageKind; Column : TColumnIndex;
			var Ghosted : Boolean; var ImageIndex : TImageIndex);
		procedure VstResultGetText(Sender : TBaseVirtualTree; Node : PVirtualNode; Column : TColumnIndex; TextType : TVSTTextType;
			var CellText : string);
		procedure VstResultHeaderClick(Sender : TVTHeader; HitInfo : TVTHeaderHitInfo);
		procedure VstResultPaintText(Sender : TBaseVirtualTree; const TargetCanvas : TCanvas; Node : PVirtualNode; Column : TColumnIndex;
			TextType : TVSTTextType);

		private
			FAbortSearch : Boolean;
			FCurrentHistoryItemIndex : Integer;
			FData : TRipGrepperData;
			FExeVersion : string;
			FFileNameType : TFileNameType;
			FHistItemObj : IHistoryItemObject;
			FHistoryObjectList : THistoryObjectList;
			FIsParsingRunning : Boolean;
			FMaxWidths : TArray<Integer>;
			FMeassureFirstDrawEvent : Boolean;
			FRipGrepTask : ITask;
			FSettings : TRipGrepperSettings;
			FswSearchStart : TStopwatch;
			FIconImgList : TIconImageList;
			FIsReplaceMode : Boolean;
			FParsingThreads : TArrayEx<TParallelParser>;
			procedure AddAsUsing(_bToImpl : Boolean);
			procedure AddVstHistItem;
			procedure ChangeHistoryNodeText;
			procedure ClearHistoryObjectList;
			procedure DoSearch;
			procedure EnableActionIfResultSelected(_act : TAction);
			procedure ExpandNodes;
			function GetAbsOrRelativePath(const _sFullPath : string) : string;
			function GetCounterText(Data : IHistoryItemObject) : string;
			function GetData : TRipGrepperData;
			function GetHistItemObject : IHistoryItemObject;
			function GetHistoryObject(const _index : Integer) : THistoryItemObject;
			function GetIsReplaceMode : Boolean;
			function GetNewParallelParser : TParallelParser;
			function GetNodeByIndex(Tree : TVirtualStringTree; Index : Integer) : PVirtualNode;
			function GetResultSelectedFilePath : string;
			function GetSelectedResultFileNodeData : PVSFileNodeData;
			function GetSettings : TRipGrepperSettings;
			procedure InitSearch;
			procedure LoadBeforeSearchSettings;
			procedure OnLastLine(const _iLineNr : Integer);
			procedure OnParsingProgress;
			procedure OpenSelectedInIde;
			procedure RefreshCounters;
			procedure RefreshCountersInGUI;
			procedure RunRipGrep;
			procedure SetColumnWidths;
			procedure SetHistItemObject(const Value : IHistoryItemObject);
			procedure SetTextColorMatch(TargetCanvas : TCanvas);
			procedure SetTextColorReplacedText(TargetCanvas : TCanvas);
			procedure SetTextColorReplaceText(var pos : Integer; var ss1_repl : string; TargetCanvas : TCanvas; const CellRect : TRect);
			function SliceArgs(const _rgp : TRipGrepParameterSettings) : TStringsArrayEx;
			procedure UpdateArgumentsAndSettings;
			procedure UpdateHistObject;
			procedure UpdateHistObjectAndGui;
			procedure UpdateRipGrepArgumentsInHistObj;
			property IsReplaceMode : Boolean read GetIsReplaceMode write FIsReplaceMode;
			property Settings : TRipGrepperSettings read GetSettings write FSettings;

			{ Private-Deklarationen }
		public
			constructor Create(AOwner : TComponent); override;
			destructor Destroy; override;
			procedure AddOrUpdateHistoryItem;
			procedure AlignToolBars;
			procedure ChangeDataHistItemObject(_ho : IHistoryItemObject);
			procedure ClearFilter(const _bForce : Boolean = False);
			procedure ClearHistoryObject;
			procedure DeleteCurrentHistoryItemFromList;
			procedure CopyToClipboardFileOfSelected;
			procedure CopyToClipboardPathOfSelected;
			function CreateNewHistObject : IHistoryItemObject;
			procedure FilterNodes(const _sFilterPattern : string);
			function GetFilePathFromNode(_node : PVirtualNode) : string;
			function GetOpenWithParamsFromSelected : TOpenWithParams;
			function GetRowColText(_i : Integer; _type : TVSTTextType) : string;
			procedure Init;
			function IsNodeFiltered(const Data : PVSFileNodeData; const _sFilterText : string) : Boolean;
			function IsSearchRunning : Boolean;
			// IEOFProcessEventHandler
			procedure OnEOFProcess;
			// INewLineEventHandler
			procedure OnNewOutputLine(const _iLineNr : Integer; const _sLine : string; _bIsLast : Boolean = False);
			procedure PrepareAndDoSearch;
			// ITerminateEventProducer
			function ProcessShouldTerminate : Boolean;
			procedure RefreshSearch;
			procedure SetResultListViewDataToHistoryObj;
			procedure SetSelectedHistoryItem(const _idx : Integer);
			property AbortSearch : Boolean read FAbortSearch write FAbortSearch;
			property CurrentHistoryItemIndex : Integer read FCurrentHistoryItemIndex write FCurrentHistoryItemIndex;
			property Data : TRipGrepperData read GetData write FData;
			property ExeVersion : string read FExeVersion write FExeVersion;
			property FileNameType : TFileNameType read FFileNameType write FFileNameType;
			property HistItemObject : IHistoryItemObject read GetHistItemObject write SetHistItemObject;
			property MaxWidths : TArray<Integer> read FMaxWidths write FMaxWidths;
			property RipGrepTask : ITask read FRipGrepTask write FRipGrepTask;
			{ Public-Deklarationen }
	end;

var
	MainFrame : TRipGrepperMiddleFrame;

implementation

uses
	RipGrepper.UI.ParentFrame,
	RipGrepper.OpenWith,
	System.StrUtils,
	RipGrepper.Tools.DebugUtils,
	System.IOUtils,
	Vcl.Clipbrd,
	Winapi.CommCtrl,
	RipGrepper.Helper.ListBox,
	RipGrepper.Tools.FileUtils,
	RipGrepper.Parsers.VimGrepMatchLine,
	System.Math,
	RipGrepper.UI.MainForm,
	RipGrepper.UI.BottomFrame,
	VirtualTrees.Types,
	RipGrepper.Parsers.Factory,
	RipGrepper.UI.TopFrame,
	RipGrepper.Common.IOTAUtils,
	GX_UsesManager,
	System.Generics.Defaults,
	RipGrepper.UI.SearchForm,
	System.RegularExpressions;

{$R *.dfm}

constructor TRipGrepperMiddleFrame.Create(AOwner : TComponent);
begin
	inherited;
	TDebugUtils.DebugMessage('TRipGrepperMiddleFrame.Create ' + AOwner.Name);
	FIconImgList := TIconImageList.Create(Handle, ImageListListView);
	MainFrame := self;
end;

destructor TRipGrepperMiddleFrame.Destroy;
begin
	TDebugUtils.DebugMessage('TRipGrepperMiddleFrame.Destroy');
	ClearHistoryObjectList;
	FData.Free;
	FIconImgList.Free;
	for var t : TParallelParser in FParsingThreads do begin
		t.Free;
	end;

	inherited;
end;

procedure TRipGrepperMiddleFrame.ActionAddUsingImplementationExecute(Sender : TObject);
begin
	AddAsUsing(True);
end;

procedure TRipGrepperMiddleFrame.ActionAddUsingImplementationUpdate(Sender : TObject);
begin
	miAddToUSESList.Visible := not IOTAUTils.IsStandAlone();
	ActionAddUsingImplementation.Visible := not IOTAUTils.IsStandAlone();
	EnableActionIfResultSelected(ActionAddUsingImplementation);
end;

procedure TRipGrepperMiddleFrame.ActionAddUsingInterfaceExecute(Sender : TObject);
begin
	AddAsUsing(False);
end;

procedure TRipGrepperMiddleFrame.ActionAddUsingInterfaceUpdate(Sender : TObject);
begin
	ActionAddUsingInterface.Visible := not IOTAUTils.IsStandAlone();
	EnableActionIfResultSelected(ActionAddUsingInterface);
end;

procedure TRipGrepperMiddleFrame.ActionCopyCmdLineToClipboardExecute(Sender : TObject);
begin
	TopFrame.ActionCmdLineCopyExecute(Sender);
end;

procedure TRipGrepperMiddleFrame.ActionOpenSearchFormExecute(Sender : TObject);
begin
	var
	formResult := TRipGrepperSearchDialogForm.ShowSearchForm(self, Settings, FHistItemObj);
	if mrOK = formResult then begin
		TDebugUtils.DebugMessage('TRipGrepperTopFrame.VstHistoryNodeDblClick: after ShowSearchForm cmdline: ' +
			Settings.RipGrepParameters.GetCommandLine);
		PrepareAndDoSearch();
	end else begin
		TDebugUtils.DebugMessage('TRipGrepperTopFrame.VstHistoryNodeDblClick: ShowSearchForm cancel');
	end;

	ChangeHistoryNodeText;
	UpdateHistObjectAndGui;
end;

procedure TRipGrepperMiddleFrame.ActionCopyFileNameExecute(Sender : TObject);
begin
	CopyToClipboardFileOfSelected();
end;

procedure TRipGrepperMiddleFrame.ActionCopyFileNameUpdate(Sender : TObject);
begin
	EnableActionIfResultSelected(ActionCopyFileName);
end;

procedure TRipGrepperMiddleFrame.ActionCopyLineToClipboardExecute(Sender : TObject);
var
	Data : PVSFileNodeData;
begin
	Data := GetSelectedResultFileNodeData();
	if Assigned(Data) then
		Clipboard.AsText := Data.MatchData.LineText;
end;

procedure TRipGrepperMiddleFrame.ActionCopyLineToClipboardUpdate(Sender : TObject);
begin
	EnableActionIfResultSelected(ActionCopyLineToClipboard);
end;

procedure TRipGrepperMiddleFrame.ActionCopyMatchToClipboardExecute(Sender : TObject);
var
	Data : PVSFileNodeData;
begin
	Data := GetSelectedResultFileNodeData();
	if Assigned(Data) then
		Clipboard.AsText := Data.MatchData.LineText.Substring(Data.MatchData.Col - 1, Data.MatchData.MatchLength);
end;

procedure TRipGrepperMiddleFrame.ActionCopyMatchToClipboardUpdate(Sender : TObject);
begin
	EnableActionIfResultSelected(ActionCopyMatchToClipboard);
end;

procedure TRipGrepperMiddleFrame.ActionCopyPathToClipboardExecute(Sender : TObject);
begin
	CopyToClipboardPathOfSelected();
end;

procedure TRipGrepperMiddleFrame.ActionCopyPathToClipboardUpdate(Sender : TObject);
begin
	EnableActionIfResultSelected(ActionCopyPathToClipboard);
end;

procedure TRipGrepperMiddleFrame.ActionHistoryDeleteAllExecute(Sender : TObject);
begin
	VstResult.Clear;
	VstHistory.Clear;
	ClearHistoryObjectList;
	HistItemObject := nil;
end;

procedure TRipGrepperMiddleFrame.ActionHistoryDeleteAllUpdate(Sender : TObject);
begin
	ActionHistoryDeleteAll.Enabled := VstHistory.RootNodeCount <> 0;
end;

procedure TRipGrepperMiddleFrame.ActionHistoryDeleteExecute(Sender : TObject);
var
	ho : IHistoryItemObject;
	Node : PVirtualNode;
	Data : PVSHistoryNodeData;
begin
	ho := GetHistoryObject(CurrentHistoryItemIndex);

	Node := GetNodeByIndex(VstHistory, CurrentHistoryItemIndex);
	Data := VstHistory.GetNodeData(Node);
	TDebugUtils.DebugMessageFormat('TRipGrepperMiddleFrame.ActionHistoryDeleteExecute: idx:%d Node:%s, ho:%s',
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

procedure TRipGrepperMiddleFrame.DeleteCurrentHistoryItemFromList;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperMiddleFrame.DeleteCurrentHistoryItemFromList');
	dbgMsg.Msg('Deleting history item at index ' + CurrentHistoryItemIndex.ToString);
	FHistoryObjectList.Delete(CurrentHistoryItemIndex);

	CurrentHistoryItemIndex := IfThen(VstHistory.RootNodeCount = 0, -1, IfThen(CurrentHistoryItemIndex = 0, 0,
		CurrentHistoryItemIndex - 1));

	dbgMsg.Msg('CurrentHistoryItemIndex=' + CurrentHistoryItemIndex.ToString);

	if CurrentHistoryItemIndex <> -1 then begin
		UpdateHistObjectAndGui;
		VstHistory.Selected[GetNodeByIndex(VstHistory, CurrentHistoryItemIndex)] := True;
	end else begin
		VstResult.Clear;
		VstHistory.Clear;
		HistItemObject := nil;
	end;
end;

procedure TRipGrepperMiddleFrame.ActionHistoryDeleteUpdate(Sender : TObject);
begin
	ActionHistoryDelete.Enabled := CurrentHistoryItemIndex <> -1;
end;

procedure TRipGrepperMiddleFrame.ActionOpenWithExecute(Sender : TObject);
var
	owp : TOpenWithParams;
begin
	owp := GetOpenWithParamsFromSelected();
	if owp.IsEmpty and (not IOTAUTils.IsStandAlone) then begin
		owp := TOpenWithParams.GetParamsOfActiveFileInDelphiIde();
	end;
	if not owp.IsEmpty then begin
		TOpenWith.Execute(owp);
	end;
end;

procedure TRipGrepperMiddleFrame.ActionOpenWithUpdate(Sender : TObject);
begin
	EnableActionIfResultSelected(ActionOpenWith);
end;

procedure TRipGrepperMiddleFrame.ActionOpenInIdeExecute(Sender : TObject);
begin
	OpenSelectedInIde;
end;

procedure TRipGrepperMiddleFrame.ActionOpenInIdeUpdate(Sender : TObject);
begin
	EnableActionIfResultSelected(ActionOpenInIde);
	ActionOpenInIde.Enabled := ActionOpenInIde.Enabled and (not IOTAUTils.IsStandAlone);
	ActionOpenInIde.Visible := not IOTAUTils.IsStandAlone;
end;

procedure TRipGrepperMiddleFrame.AddAsUsing(_bToImpl : Boolean);
var
	fn : string;
	usesman : TUsesManager;
	st : TUsesStatus;
begin
	usesman := TUsesManager.Create(IOTAUtils.GxOtaGetCurrentSourceEditor);
	try
		fn := TPath.GetFileNameWithoutExtension(GetResultSelectedFilePath);

		st := usesman.GetUsesStatus(fn);
		if (usNonExisting = st) then begin
			if _bToImpl then begin
				usesman.AddToImpSection(fn);
			end else begin
				usesman.AddToIntSection(fn);
			end;
		end else begin
			TMsgBox.ShowInfo(Format('Unit %s is already in %s section.', [fn, IfThen(st = usInterface, 'interface', 'implementation')]));
		end;

	finally
		usesman.Free;
	end;
end;

procedure TRipGrepperMiddleFrame.AddOrUpdateHistoryItem;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperMiddleFrame.AddOrUpdateHistoryItem');
	dbgMsg.Msg('CurrentHistoryItemIndex ' + CurrentHistoryItemIndex.ToString);
	if not FHistItemObj.HasResult then begin
		AddVstHistItem;
	end;
	ChangeHistoryNodeText;
	UpdateRipGrepArgumentsInHistObj;
	UpdateHistObject;
	ClearHistoryObject();
	dbgMsg.Msg('Update HistoryObject LastSearchText=' + Settings.LastSearchText);
end;

procedure TRipGrepperMiddleFrame.AddVstHistItem;
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

procedure TRipGrepperMiddleFrame.AlignToolBars;
begin
	if Assigned(TopFrame) then begin
		TopFrame.AlignToolBars(PanelResult.Left, PanelHistory.Width, PanelResult.Width);
	end;
end;

procedure TRipGrepperMiddleFrame.ChangeDataHistItemObject(_ho : IHistoryItemObject);
begin
	var
	beu := TBeginEndUpdater.New(VstHistory);
	Data.HistObject := _ho;
end;

procedure TRipGrepperMiddleFrame.ChangeHistoryNodeText;
var
	Node : PVirtualNode;
	Data : PVSHistoryNodeData;
begin
	TDebugUtils.DebugMessage('TRipGrepperMiddleFrame.ChangeHistoryNodeText: idx = ' + CurrentHistoryItemIndex.ToString);

	Node := GetNodeByIndex(VstHistory, CurrentHistoryItemIndex);
	Data := VstHistory.GetNodeData(Node);
	TDebugUtils.DebugMessageFormat('TRipGrepperMiddleFrame.ChangeHistoryNodeText: SearchText orig=''%s'' new=''%''',
		[Data^.SearchText, Settings.LastSearchText]);
	if not Settings.LastSearchText.IsEmpty then begin
		Data^.SearchText := Settings.LastSearchText;
		Data^.IsReplaceMode := Settings.IsReplaceMode;
		Data^.ReplaceText := Settings.LastReplaceText;
	end;
end;

procedure TRipGrepperMiddleFrame.ClearFilter(const _bForce : Boolean = False);
begin
	if _bForce then begin
		VstResult.BeginUpdate;
		try
			for var Node in VstResult.InitializedNodes(True) do begin
				VstResult.IsFiltered[Node] := False;
			end;
		finally
			VstResult.EndUpdate;
		end;
	end;
end;

procedure TRipGrepperMiddleFrame.ClearHistoryObject;
begin
	var
	beu := TBeginEndUpdater.New(VstHistory);
	HistItemObject.ClearMatches;
end;

procedure TRipGrepperMiddleFrame.ClearHistoryObjectList;
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

procedure TRipGrepperMiddleFrame.CopyToClipboardFileOfSelected;
begin
	Clipboard.AsText := TPath.GetFileName(GetResultSelectedFilePath);
end;

procedure TRipGrepperMiddleFrame.CopyToClipboardPathOfSelected;
begin
	Clipboard.AsText := TPath.GetFullPath(GetResultSelectedFilePath);
end;

function TRipGrepperMiddleFrame.CreateNewHistObject : IHistoryItemObject;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperMiddleFrame.CreateNewHistObject');
	Result := THistoryItemObject.Create();
	HistItemObject := Result;
	ChangeDataHistItemObject(Result);
	dbgMsg.Msg('Add HistoryObject ' + Settings.LastSearchText);
	CurrentHistoryItemIndex := FHistoryObjectList.Add(Result);
	dbgMsg.Msg('CurrentHistoryItemIndex=' + CurrentHistoryItemIndex.ToString);
end;

procedure TRipGrepperMiddleFrame.DoSearch;
begin
	ParentFrame.SetStatusBarStatistic('Searching...');
	FAbortSearch := False;
	UpdateArgumentsAndSettings;
	RunRipGrep();
end;

procedure TRipGrepperMiddleFrame.EnableActionIfResultSelected(_act : TAction);
begin
	_act.Enabled := VstResult.SelectedCount = 1;
end;

procedure TRipGrepperMiddleFrame.ExpandNodes;
begin
	if Settings.NodeLookSettings.ExpandNodes then begin
		VstResult.FullExpand();
	end;
end;

procedure TRipGrepperMiddleFrame.FilterNodes(const _sFilterPattern : string);
var
	Node : PVirtualNode;
	Data : PVSFileNodeData;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperMiddleFrame.FilterNodes');
	VstResult.BeginUpdate;
	try
		for Node in VstResult.InitializedNodes(True) do begin
			Data := VstResult.GetNodeData(Node);
			var
			bIsFiltered := IsNodeFiltered(Data, _sFilterPattern);
			if (Node.Parent = VstResult.RootNode) then begin
				// TODO:  filter for filenames should include childs
			end else begin
				VstResult.IsFiltered[Node] := bIsFiltered;
				if not bIsFiltered and Assigned(Node.Parent) and (Node.Parent <> VstResult.RootNode) then begin
					VstResult.IsFiltered[Node.Parent] := False;
				end;
			end;
			dbgMsg.MsgFmtIf(not bIsFiltered, 'not IsFiltered: %s', [Data.MatchData.LineText]);
		end;
	finally
		VstResult.EndUpdate;
	end;
end;

procedure TRipGrepperMiddleFrame.FrameResize(Sender : TObject);
begin
	// TDebugUtils.DebugMessage('TRipGrepperMiddleFrame.FrameResize');

	SplitView1.Width := panelMain.Width;
	if Assigned(ParentFrame) then begin
		BottomFrame.StatusBar1.Panels[0].Width := PanelHistory.Width;
	end;
	AlignToolBars;

	SetColumnWidths;
end;

function TRipGrepperMiddleFrame.GetAbsOrRelativePath(const _sFullPath : string) : string;
var
	actPath : string;
begin
	Result := _sFullPath;
	if Settings.NodeLookSettings.ShowRelativePath then begin
		if IOTAUTils.IsStandAlone then begin
			actPath := Settings.ActualSearchPath;
			if (actPath.IsEmpty or (not Settings.SearchPathIsDir)) then begin
				Exit;
			end;
		end else begin
			actPath := TPath.GetDirectoryName(Settings.SearchFormSettings.ExtensionSettings.CurrentIDEContext.ActiveProject);
		end;
		Result := ExtractRelativePath(actPath + '\', _sFullPath);
	end;
end;

function TRipGrepperMiddleFrame.GetCounterText(Data : IHistoryItemObject) : string;
begin
	Result := '';
	if not Assigned(Data) then begin
		Exit;
	end;
	if Data.ErrorCount > 0 then begin
		Result := Format('%s %d in %d(%d!)', [TREEVIEW_HISTORY_COUNTER_ERROR_PREFIX, Data.TotalMatchCount, Data.FileCount,
			Data.ErrorCount]);
	end else begin
		if Data.NoMatchFound then begin
			Result := TREEVIEW_HISTORY_COUNTER_NOTHING_FOUND_PREFIX + ' 0 in 0';
		end else begin
			Result := Format('%s %d in %d', [TREEVIEW_HISTORY_COUNTER_OK_PREFIX, Data.TotalMatchCount, Data.FileCount]);
		end;
	end;
end;

function TRipGrepperMiddleFrame.GetData : TRipGrepperData;
begin
	if not Assigned(FData) then begin
		FData := TRipGrepperData.Create(VstResult);
	end;
	Result := FData;
end;

function TRipGrepperMiddleFrame.GetFilePathFromNode(_node : PVirtualNode) : string;
var
	Data : PVSFileNodeData;
begin
	if _node.ChildCount = 0 then begin
		Data := VstResult.GetNodeData(_node.Parent);
	end else begin
		Data := VstResult.GetNodeData(_node);
	end;
	Result := Data.FilePath;
end;

function TRipGrepperMiddleFrame.GetHistItemObject : IHistoryItemObject;
begin
	Result := FHistItemObj;
end;

function TRipGrepperMiddleFrame.GetHistoryObject(const _index : Integer) : THistoryItemObject;
begin
	Result := nil;
	if (_index > -1) and (_index < FHistoryObjectList.Count { _lb.Items.Count } ) then begin
		Result := THistoryItemObject(FHistoryObjectList[_index]);
		// Result := THistoryItemObject(_lb.Items.Objects[_index]);
	end;
end;

function TRipGrepperMiddleFrame.GetIsReplaceMode : Boolean;
begin
	Result := not Settings.RipGrepParameters.ReplaceText.IsEmpty;
end;

function TRipGrepperMiddleFrame.GetOpenWithParamsFromSelected : TOpenWithParams;
var
	Node : PVirtualNode;
	Data : PVSFileNodeData;
	dataParent : PVSFileNodeData;
begin
	Node := VstResult.GetFirstSelected();
	if not Assigned(Node) then
		Exit;

	Data := VstResult.GetNodeData(Node);
	if Node.ChildCount > 0 then begin
		Result.DirPath := IfThen(Settings.SearchPathIsDir, Settings.ActualSearchPath, ExtractFileDir(Data.FilePath));
		Result.FileName := Data.FilePath;
		Result.Row := 0;
		Result.Column := 0;
		Result.IsEmpty := False;
	end else begin
		dataParent := VstResult.GetNodeData(Node.Parent);
		Result.DirPath := IfThen(Settings.SearchPathIsDir, Settings.ActualSearchPath, ExtractFileDir(dataParent.FilePath));
		Result.FileName := dataParent.FilePath;
		Result.Row := Data.MatchData.Row;
		Result.Column := Data.MatchData.Col;
		Result.IsEmpty := False;
	end;

	TDebugUtils.DebugMessage('TRipGrepperMiddleFrame.GetOpenWithParamsFromSelected ' + Result.ToString);

end;

function TRipGrepperMiddleFrame.GetNewParallelParser : TParallelParser;
begin
	Result := TParallelParser.Create(FData, FHistItemObj);
	Result.OnLastLine := OnLastLine;
	Result.OnProgress := OnParsingProgress;
	FParsingThreads.Add(Result);
end;

function TRipGrepperMiddleFrame.GetRowColText(_i : Integer; _type : TVSTTextType) : string;
begin
	Result := '';
	if (_type = ttNormal) and (_i > 0) then begin
		Result := _i.ToString;
	end;
end;

function TRipGrepperMiddleFrame.GetSettings : TRipGrepperSettings;
begin
	if not Assigned(FSettings) then begin
		FSettings := ParentFrame.Settings;
	end;
	Result := FSettings;
end;

procedure TRipGrepperMiddleFrame.Init;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperMiddleFrame.Init');
	FCurrentHistoryItemIndex := -1;
	FHistoryObjectList.Clear();
	if IOTAUTils.IsStandAlone then begin
		FExeVersion := TFileUtils.GetAppNameAndVersion(Application.ExeName);
	end else begin
		FExeVersion := TFileUtils.GetPackageNameAndVersion(HInstance);
		PanelHistory.BevelOuter := bvNone;
		PanelResult.BevelOuter := bvNone;
	end;
	Align := alClient;
	dbgMsg.Msg('TRipGrepperMiddleFrame.Init ' + FExeVersion);
	FFileNameType := ftAbsolute;
	VstResult.TreeOptions.StringOptions := VstResult.TreeOptions.StringOptions + [toShowStaticText];
	VstResult.TreeOptions.PaintOptions := VstResult.TreeOptions.PaintOptions + [toUseExplorerTheme];
	VstResult.NodeDataSize := SizeOf(TVSFileNodeData);

	VstHistory.TreeOptions.StringOptions := VstHistory.TreeOptions.StringOptions + [toShowStaticText];
	VstHistory.TreeOptions.PaintOptions := VstHistory.TreeOptions.PaintOptions + [toUseExplorerTheme];
	VstHistory.TreeOptions.MiscOptions := VstHistory.TreeOptions.MiscOptions + [TVTMiscOption.toVariablenodeHeight];
	VstHistory.NodeDataSize := SizeOf(TVSHistoryNodeData);

	miOpenwith1.Default := IOTAUTils.IsStandAlone;
	miOpenInIde.Default := not IOTAUTils.IsStandAlone;
	if not miOpenInIde.Default then begin
		PopupMenuResult.Items[0].Visible := False;
	end;
end;

procedure TRipGrepperMiddleFrame.InitSearch;
begin
	VstResult.Clear;
	Data.ClearMatchFiles;
	// ClearData;
	FswSearchStart := TStopwatch.Create();
	FMeassureFirstDrawEvent := True;
	ParentFrame.InitStatusBar;
	LoadBeforeSearchSettings();
end;

function TRipGrepperMiddleFrame.IsSearchRunning : Boolean;
begin
	Result := FIsParsingRunning or (Assigned(FRipGrepTask) and (FRipGrepTask.Status = TTaskStatus.Running));
end;

procedure TRipGrepperMiddleFrame.LoadBeforeSearchSettings;
begin
	//
end;

procedure TRipGrepperMiddleFrame.OnEOFProcess;
begin
	TDebugUtils.DebugMessage(Format('TRipGrepperMiddleFrame.OnEOFProcess: End of processing rg.exe output in %s sec.',
		[GetElapsedTime(FswSearchStart)]));
end;

procedure TRipGrepperMiddleFrame.OnLastLine(const _iLineNr : Integer);
begin
	// ListViewResult.AdjustColumnWidths(MaxWidths);
	TDebugUtils.DebugMessage(Format('TRipGrepperMiddleFrame.OnLastLine: Last line (%d.) received in %s sec.',
		[_iLineNr, GetElapsedTime(FswSearchStart)]));

	TThread.Synchronize(nil,
		procedure
		begin
			SetColumnWidths;
			BottomFrame.ActivityIndicator1.Animate := False;
			FIsParsingRunning := False;
			ExpandNodes;
		end);
end;

procedure TRipGrepperMiddleFrame.OnNewOutputLine(const _iLineNr : Integer; const _sLine : string; _bIsLast : Boolean = False);
var
	parser : TParallelParser;
begin
	if (FAbortSearch) then begin
		OnLastLine(_iLineNr);
		Exit;
	end;

	if (_iLineNr >= RG_PROCESSING_LINE_COUNT_LIMIT) then begin
		OnLastLine(_iLineNr);
		if _iLineNr = RG_PROCESSING_LINE_COUNT_LIMIT then begin
			TMsgBox.ShowWarning(Format('Too many results.' + CRLF + 'The first %d will be shown. Try to be more specific.',
				[_iLineNr, RG_PROCESSING_LINE_COUNT_LIMIT]));
		end else begin
			Exit;
		end;
	end;

	parser := GetNewParallelParser();
	parser.SetNewLine(_iLineNr, _sLine, _bIsLast);

	try
		parser.Parse();
	except
		on e : EOutOfMemory do begin
			TDebugUtils.DebugMessage(Format('Exception %s ' + CRLF + 'on line %d', [e.Message, _iLineNr]));
			TMsgBox.ShowError(e.Message + CRLF + 'Too many results. Try to be more specific.');
		end;
	end;

end;

procedure TRipGrepperMiddleFrame.OnParsingProgress;
begin
	RefreshCounters;
	VstResult.Repaint;
end;

function TRipGrepperMiddleFrame.ProcessShouldTerminate : Boolean;
begin
	Result := Assigned(FRipGrepTask) and (FRipGrepTask.Status = TTaskStatus.Canceled);
end;

procedure TRipGrepperMiddleFrame.RefreshCounters;
begin
	if Assigned(FHistItemObj) then begin
		var
		beu := TBeginEndUpdater.New(VstHistory);
		FHistItemObj.FileCount := VstResult.RootNodeCount; // synced
		FHistItemObj.ErrorCount := Data.ErrorCount; // synced
	end;
	RefreshCountersInGUI;
end;

procedure TRipGrepperMiddleFrame.RefreshCountersInGUI;
begin
	TThread.Synchronize(nil,
		procedure
		begin
			VstHistory.Refresh;
			ParentFrame.SetStatusBarStatistic(GetCounterText(HistItemObject));
		end);
end;

procedure TRipGrepperMiddleFrame.RunRipGrep;
var
	workDir : string;
	args : TStrings;
	argsArrs : TStringsArrayEx;
begin
	FRipGrepTask := TTask.Create(
		procedure()
		begin
			if not FileExists(Settings.RipGrepParameters.RipGrepPath) then begin
				TMsgBox.ShowError(Format(FORMAT_RIPGREP_EXE_NOT_FOUND, [Settings.IniFile.FileName]));
			end;
			workDir := TDirectory.GetCurrentDirectory();
			TDebugUtils.DebugMessage('TRipGrepperMiddleFrame.RunRipGrep: run: ' + Settings.RipGrepParameters.RipGrepPath + ' '
				{ } + Settings.RipGrepParameters.RipGrepArguments.DelimitedText);
			FswSearchStart := TStopwatch.StartNew;
			args := TStringList.Create;
			try
				argsArrs := SliceArgs(Settings.RipGrepParameters);
				for var i := 0 to argsArrs.MaxIndex do begin
					args.Clear;
					args.AddStrings(argsArrs[i]);
					if i < argsArrs.MaxIndex then begin
						FHistItemObj.RipGrepResult := TProcessUtils.RunProcess(Settings.RipGrepParameters.RipGrepPath, args,
							{ } workDir,
							{ } self as INewLineEventHandler,
							{ } self as ITerminateEventProducer,
							{ } nil);
					end else begin
						FHistItemObj.RipGrepResult := TProcessUtils.RunProcess(Settings.RipGrepParameters.RipGrepPath, args,
							{ } workDir,
							{ } self as INewLineEventHandler,
							{ } self as ITerminateEventProducer,
							{ } self as IEOFProcessEventHandler);
					end;
				end;

			finally
				args.Free;
			end;
			FHistItemObj.ElapsedTimeText := GetElapsedTime(FswSearchStart);
			ParentFrame.SetStatusBarMessage(True);
			FswSearchStart.Stop;
			TDebugUtils.DebugMessage(Format('TRipGrepperMiddleFrame.RunRipGrep: rg.exe ended in %s sec.', [FHistItemObj.ElapsedTimeText]));
		end);
	FRipGrepTask.Start;
	BottomFrame.SetRunningStatus();
end;

procedure TRipGrepperMiddleFrame.SetColumnWidths;
begin
	// TListView_Resize(ListViewResult);
	// ListView_SetColumnWidth(ListViewResult.Handle, 0, ColumnTextWidth);
	// ListView_SetColumnWidth(ListViewResult.Handle, 1, ColumnHeaderWidth);
	// ListView_SetColumnWidth(ListViewResult.Handle, 2, ColumnHeaderWidth);
	// ListView_SetColumnWidth(ListViewResult.Handle, 3, ColumnTextWidth);
end;

procedure TRipGrepperMiddleFrame.SetHistItemObject(const Value : IHistoryItemObject);
begin
	FHistItemObj := Value;
end;

procedure TRipGrepperMiddleFrame.SetResultListViewDataToHistoryObj;
begin
	TThread.Synchronize(nil, // Refresh listview on history click
		procedure
		begin
			VstResult.Clear;
			ChangeDataHistItemObject(FHistItemObj);
			Data.DataToGrid;
			// ListViewResult.Items.Count := matchItems.Count + FHistItemObj.ErrorCount;
			{$IFDEF THREADSAFE_LIST}
			FHistItemObj.Matches.Unlock;
			{$ENDIF}
		end);
end;

function TRipGrepperMiddleFrame.GetNodeByIndex(Tree : TVirtualStringTree; Index : Integer) : PVirtualNode;
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

function TRipGrepperMiddleFrame.GetResultSelectedFilePath : string;
var
	Node : PVirtualNode;
begin
	Node := VstResult.GetFirstSelected();
	if not Assigned(Node) then
		Exit;

	Result := GetFilePathFromNode(Node);
end;

function TRipGrepperMiddleFrame.GetSelectedResultFileNodeData : PVSFileNodeData;
var
	Node : PVirtualNode;
begin
	Result := nil;
	Node := VstResult.GetFirstSelected();
	if not Assigned(Node) then
		Exit;
	Result := VstResult.GetNodeData(Node);
end;

function TRipGrepperMiddleFrame.IsNodeFiltered(const Data : PVSFileNodeData; const _sFilterText : string) : Boolean;
begin
	if _sFilterText.IsEmpty then begin
		Result := False;
	end else begin
		Result := not TRegEx.IsMatch(Data.MatchData.LineText, _sFilterText);
	end;
end;

procedure TRipGrepperMiddleFrame.OpenSelectedInIde;
var
	owp : TOpenWithParams;
begin
	owp := GetOpenWithParamsFromSelected();
	TDebugUtils.DebugMessage(Format('TRipGrepperMiddleFrame.OpenSelectedInIde: %s(%d:%d)', [owp.FileName, owp.Row, owp.Column]));
	IOTAUtils.GxOtaGoToFileLineColumn(owp.FileName, owp.Row, owp.Column, owp.Column - 1);
end;

procedure TRipGrepperMiddleFrame.PopupMenuHistoryPopup(Sender : TObject);
begin
	SetSelectedHistoryItem(CurrentHistoryItemIndex);
end;

procedure TRipGrepperMiddleFrame.PrepareAndDoSearch;
begin
	AddOrUpdateHistoryItem;
	SetSelectedHistoryItem(MainFrame.CurrentHistoryItemIndex);
	Data.ClearMatchFiles;
	InitSearch();
	DoSearch();
end;

procedure TRipGrepperMiddleFrame.RefreshSearch;
begin
	UpdateHistObject();
	ClearHistoryObject();
	InitSearch();
	DoSearch();
end;

procedure TRipGrepperMiddleFrame.SetSelectedHistoryItem(const _idx : Integer);
var
	Node : PVirtualNode;
begin
	Node := GetNodeByIndex(VstHistory, _idx);
	if Assigned(Node) then begin
		VstHistory.Selected[Node] := true;
	end;
end;

procedure TRipGrepperMiddleFrame.SetTextColorMatch(TargetCanvas : TCanvas);
begin
	TargetCanvas.Font.Color := TREEVIEW_MATCH_TEXT_COLOR;
	TargetCanvas.Brush.Color := TREEVIEW_MATCH_TEXT_BGCOLOR;
	TargetCanvas.Font.style := TREEVIEW_MATCH_TEXT_STYLE;
end;

procedure TRipGrepperMiddleFrame.SetTextColorReplacedText(TargetCanvas : TCanvas);
begin
	TargetCanvas.Font.Color := TREEVIEW_REPLACED_TEXT_COLOR;
	TargetCanvas.Brush.Color := TREEVIEW_REPLACED_TEXT_BGCOLOR;
	TargetCanvas.Font.style := TREEVIEW_REPLACED_TEXT_STYLE;
end;

procedure TRipGrepperMiddleFrame.SetTextColorReplaceText(var pos : Integer; var ss1_repl : string; TargetCanvas : TCanvas;
const CellRect : TRect);
begin
	TargetCanvas.Font.Color := TREEVIEW_REPLACE_TEXT_COLOR;
	TargetCanvas.Font.style := TREEVIEW_REPLACE_TEXT_STYLE;
	TargetCanvas.Brush.Color := TREEVIEW_REPLACE_TEXT_BGCOLOR;
	TargetCanvas.TextOut(CellRect.Left + pos, TREEVIEW_FONTSPACE, ss1_repl);
end;

function TRipGrepperMiddleFrame.SliceArgs(const _rgp : TRipGrepParameterSettings) : TStringsArrayEx;
var
	args : TStrings;
	op_args : TArray<string>;
	path_args : TArray<string>;
	exe : string;
	options : string;
	strsArr : TStringsArrayEx;
	fullCmdLen : integer;
begin
	options := _rgp.RgExeOptions.AsString;
	args := TStringList.Create;
	try
		args.Delimiter := ' ';
		args.AddStrings(_rgp.RipGrepArguments.GetValues());
		exe := _rgp.RipGrepPath;
		fullCmdLen := TProcessUtils.GetCommandLineLength(exe, args);
		if (MAX_COMMAND_LINE_LENGTH > fullCmdLen) then begin
			Result.Add(args.ToStringArray);
		end else begin
			args.Clear;
			path_args := _rgp.SearchPath.Split([SEARCH_PATH_SEPARATOR]);
			args.AddStrings(path_args);
			strsArr := args.SliceMaxLength(MAX_COMMAND_LINE_LENGTH - (fullCmdLen - args.Text.Length));
			op_args := _rgp.RipGrepArguments.GetValues(RG_ARG_OPTIONS);
			for var arrPath in strsArr do begin
				Result.Add(op_args + [_rgp.GuiSearchTextParams.SearchText] + arrPath);
			end;
		end;
	finally
		args.Free;
	end;
end;

procedure TRipGrepperMiddleFrame.Splitter1Moved(Sender : TObject);
begin
	AlignToolBars();
end;

procedure TRipGrepperMiddleFrame.SplitView1Resize(Sender : TObject);
begin
	// AlignToolBars();
end;

procedure TRipGrepperMiddleFrame.UpdateArgumentsAndSettings;
begin
	if FHistItemObj.RipGrepArguments.Count = 0 then begin
		FHistItemObj.LoadFromSettings(Settings);
	end;
end;

procedure TRipGrepperMiddleFrame.UpdateHistObject;
begin
	FHistItemObj := GetHistoryObject(CurrentHistoryItemIndex);
	if Assigned(FHistItemObj) then begin
		FHistItemObj.UpdateParserType();
		FHistItemObj.CopyToSettings(Settings);
	end;
end;

procedure TRipGrepperMiddleFrame.UpdateHistObjectAndGui;
begin
	UpdateHistObject;
	TDebugUtils.DebugMessage('TRipGrepperMiddleFrame.UpdateHistObjectAndGui History selected: ' + CurrentHistoryItemIndex.ToString);
	TDebugUtils.DebugMessage('TRipGrepperMiddleFrame.UpdateHistObjectAndGui History Object: ' +
		HistItemObject.RipGrepArguments.DelimitedText);
	TDebugUtils.DebugMessage('TRipGrepperMiddleFrame.UpdateHistObjectAndGui History Matches: ' + HistItemObject.TotalMatchCount.ToString);
	TDebugUtils.DebugMessage('TRipGrepperMiddleFrame.UpdateHistObjectAndGui History Files: ' + HistItemObject.FileCount.ToString);
	TDebugUtils.DebugMessage('TRipGrepperMiddleFrame.UpdateHistObjectAndGui History Errors: ' + HistItemObject.ErrorCount.ToString);
	TDebugUtils.DebugMessage('TRipGrepperMiddleFrame.UpdateHistObjectAndGui History Gui: ' + HistItemObject.GuiSearchTextParams.SearchText +
		' ' + HistItemObject.GuiSearchTextParams.ToString);
	SetResultListViewDataToHistoryObj();
	ExpandNodes();
	TopFrame.SetFilter(False);
	RefreshCountersInGUI;
	ParentFrame.SetStatusBarMessage(True);
end;

procedure TRipGrepperMiddleFrame.UpdateRipGrepArgumentsInHistObj;
begin
	FHistItemObj.RipGrepArguments.Clear;
	Settings.RebuildArguments();
	FHistItemObj.LoadFromSettings(Settings);
end;

procedure TRipGrepperMiddleFrame.VstHistoryDrawText(Sender : TBaseVirtualTree; TargetCanvas : TCanvas; Node : PVirtualNode;
Column : TColumnIndex; const Text : string; const CellRect : TRect; var DefaultDraw : Boolean);
var
	sSearchText, sStatistic : string;
	lineBegin : Integer;
	size : Winapi.Windows.TSize;
	rectTemp : TRect;
begin
	case Column of
		0 : begin
			// First, store the default font size and color number
			var backup := TDrawParams.Save(TargetCanvas);

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

			backup.Load(TargetCanvas);

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
		end;
	end;
end;

procedure TRipGrepperMiddleFrame.VstHistoryFreeNode(Sender : TBaseVirtualTree; Node : PVirtualNode);
var
	Data : PVSHistoryNodeData;
begin
	Data := VstHistory.GetNodeData(Node);
	Data.SearchText := '';
	Data.ReplaceText := '';
	// Data.hio.Free;
end;

procedure TRipGrepperMiddleFrame.VstHistoryGetHint(Sender : TBaseVirtualTree; Node : PVirtualNode; Column : TColumnIndex;
var LineBreakStyle : TVTTooltipLineBreakStyle; var HintText : string);
var
	ho : IHistoryItemObject;
begin
	ho := GetHistoryObject(Node.Index);
	LineBreakStyle := TVTTooltipLineBreakStyle.hlbForceMultiLine;
	var
	lineBreakTab := CRLF + '  ';
	HintText := 'rg.exe' + lineBreakTab + string.Join(lineBreakTab, ho.RipGrepArguments.GetValues());
end;

procedure TRipGrepperMiddleFrame.VstHistoryGetHintKind(Sender : TBaseVirtualTree; Node : PVirtualNode; Column : TColumnIndex;
var Kind : TVTHintKind);
begin
	Kind := TVTHintKind.vhkText;
end;

procedure TRipGrepperMiddleFrame.VstHistoryGetText(Sender : TBaseVirtualTree; Node : PVirtualNode; Column : TColumnIndex;
TextType : TVSTTextType; var CellText : string);
var
	Data : PVSHistoryNodeData;
begin
	Data := VstHistory.GetNodeData(Node);

	if TextType = ttNormal then begin
		// if Data.IsReplaceMode then begin
		// case Column of
		// 0 :
		CellText := Data.SearchText + CRLF + GetCounterText(GetHistoryObject(Node.Index));
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
		CellText := GetCounterText(GetHistoryObject(Node.Index));
	end;
end;

procedure TRipGrepperMiddleFrame.VstHistoryMeasureItem(Sender : TBaseVirtualTree; TargetCanvas : TCanvas; Node : PVirtualNode;
var NodeHeight : Integer);
begin
	if Sender.MultiLine[Node] then begin
		TargetCanvas.Font := Sender.Font;
		NodeHeight := VstHistory.ComputeNodeHeight(TargetCanvas, Node, -1);
	end;
end;

procedure TRipGrepperMiddleFrame.VstHistoryNodeClick(Sender : TBaseVirtualTree; const HitInfo : THitInfo);
begin
	if (CurrentHistoryItemIndex = Integer(HitInfo.HitNode.Index)) then
		Exit;

	CurrentHistoryItemIndex := HitInfo.HitNode.Index;
	UpdateHistObjectAndGui;
end;

procedure TRipGrepperMiddleFrame.VstHistoryNodeDblClick(Sender : TBaseVirtualTree; const HitInfo : THitInfo);
begin
	TDebugUtils.DebugMessage('TRipGrepperMiddleFrame.VstHistoryNodeDblClick: idx = ' + HitInfo.HitNode.Index.ToString);
	VstHistoryNodeClick(Sender, HitInfo);
	ActionOpenSearchFormExecute(Sender);
end;

procedure TRipGrepperMiddleFrame.VstResultBeforeCellPaint(Sender : TBaseVirtualTree; TargetCanvas : TCanvas; Node : PVirtualNode;
Column : TColumnIndex; CellPaintMode : TVTCellPaintMode; CellRect : TRect; var ContentRect : TRect);
begin
	if Settings.NodeLookSettings.AlternateRowColors and (Node.ChildCount = 0) then begin
		TargetCanvas.SetAlteringColors(Node.Index);
	end;

	TargetCanvas.FillRect(CellRect);
end;

procedure TRipGrepperMiddleFrame.VstResultCompareNodes(Sender : TBaseVirtualTree; Node1, Node2 : PVirtualNode; Column : TColumnIndex;
var Result : Integer);
var
	Data1 : PVSFileNodeData;
	Data2 : PVSFileNodeData;
	Data1Parent : PVSFileNodeData; // TODO: compare file (parent) with childs ?
	Data2Parent : PVSFileNodeData;
	s1, s2 : string;
begin
	Data1 := VstResult.GetNodeData(Node1);
	Data2 := VstResult.GetNodeData(Node2);
	s1 := '';
	s2 := '';
	if Assigned(Node1.Parent) and Assigned(Node2.Parent) then begin
		Data1Parent := VstResult.GetNodeData(Node1.Parent);
		Data2Parent := VstResult.GetNodeData(Node2.Parent);

		if Assigned(Data1Parent) and Assigned(Data2Parent) then begin
			s1 := Data1Parent.FilePath;
			s2 := Data2Parent.FilePath;
		end;
	end;

	if (not Assigned(Data1)) or (not Assigned(Data2)) then
		Result := 0
	else
		case Column of
			0 :
			Result := CompareText(Data1.FilePath, Data2.FilePath);
			1 :
			Result := { CompareText(s1, s2) + } CompareValue(Data1.MatchData.Row, Data2.MatchData.Row);
			2 :
			Result := { CompareText(s1, s2) + } CompareValue(Data1.MatchData.Col, Data2.MatchData.Col);
			3 :
			Result := CompareText(Data1.MatchData.LineText, Data2.MatchData.LineText);
		end;
end;

procedure TRipGrepperMiddleFrame.VstResultDblClick(Sender : TObject);
begin
	if IOTAUTils.IsStandAlone then begin
		ActionOpenWithExecute(Sender);
	end else begin
		OpenSelectedInIde;
	end;
end;

procedure TRipGrepperMiddleFrame.VstResultDrawText(Sender : TBaseVirtualTree; TargetCanvas : TCanvas; Node : PVirtualNode;
Column : TColumnIndex; const Text : string; const CellRect : TRect; var DefaultDraw : Boolean);
var
	pos : Integer;
	Data : PVSFileNodeData;
	s, ss0, ss1, ss1_repl, ss2 : string;
	iSpaces, iTabs, matchBegin : Integer;
begin
	case Column of
		0 : begin
			if MatchStr(Text, [RG_ERROR_MSG_PREFIX, RG_PARSE_ERROR]) then begin
				DefaultDraw := False;
				TargetCanvas.Font.Color := TREEVIEW_ERROR_COLOR;
				TargetCanvas.TextOut(CellRect.Left, TREEVIEW_FONTSPACE, Text);
			end;
		end;
		3 : begin
			case FHistItemObj.ParserType of
				ptRipGrepSearch, ptRipGrepPrettySearch : begin
					DefaultDraw := False;
					var
						// First, store the default font size and color number
					backup := TDrawParams.Save(TargetCanvas);

					Data := VstResult.GetNodeData(Node);
					s := Data.GetLineText(not Settings.NodeLookSettings.IndentLines, iSpaces, iTabs);

					matchBegin := Data.MatchData.Col - 1 - (iSpaces + iTabs);

					ss0 := s.Substring(0, matchBegin).Replace(#9, TREEVIEW_INDENT_TAB_AS_SPACES, [rfReplaceAll]);
					pos := TargetCanvas.TextWidth(ss0);

					TargetCanvas.TextOut(CellRect.Left, TREEVIEW_FONTSPACE, ss0);

					ss1 := s.Substring(matchBegin, Data.MatchData.MatchLength);
					if IsReplaceMode and (not Settings.LastSearchText.IsEmpty) then begin
						ss1_repl := TRegEx.Replace(ss1, Settings.LastSearchText, Settings.RipGrepParameters.ReplaceText, [roIgnoreCase]);
					end;
					ss2 := s.Substring(matchBegin + Data.MatchData.MatchLength);

					if IsReplaceMode then begin
						SetTextColorReplacedText(TargetCanvas);
					end else begin
						SetTextColorMatch(TargetCanvas);
					end;;
					TargetCanvas.TextOut(CellRect.Left + pos, TREEVIEW_FONTSPACE, ss1);
					pos := pos + TargetCanvas.TextWidth(ss1);
					if IsReplaceMode then begin
						SetTextColorReplaceText(pos, ss1_repl, TargetCanvas, CellRect);
						pos := pos + TargetCanvas.TextWidth(ss1_repl);
					end;

					backup.Load(TargetCanvas);
					TargetCanvas.TextOut(CellRect.Left + pos, TREEVIEW_FONTSPACE, ss2);
				end;
			end;
		end;
	end;
end;

procedure TRipGrepperMiddleFrame.VstResultFreeNode(Sender : TBaseVirtualTree; Node : PVirtualNode);
var
	NodeData : PVSFileNodeData;
begin
	NodeData := Sender.GetNodeData(Node);
	NodeData^.MatchData := TVSMatchData.New(0, 0, 0, '');
	NodeData^.FilePath := ''; // so we don't have mem leaks
end;

procedure TRipGrepperMiddleFrame.VstResultGetImageIndex(Sender : TBaseVirtualTree; Node : PVirtualNode; Kind : TVTImageKind;
Column : TColumnIndex; var Ghosted : Boolean; var ImageIndex : TImageIndex);
var
	NodeData : PVSFileNodeData;
begin
	if not Settings.NodeLookSettings.ShowFileIcon then
		Exit;

	if Node.ChildCount > 0 then begin
		NodeData := Sender.GetNodeData(Node);
		case Kind of
			ikNormal, ikSelected :
			case Column of
				0 :
				ImageIndex := FIconImgList.GetImgIndex(NodeData^.FilePath);
			end;
		end;
	end;
end;

procedure TRipGrepperMiddleFrame.VstResultGetText(Sender : TBaseVirtualTree; Node : PVirtualNode; Column : TColumnIndex;
TextType : TVSTTextType; var CellText : string);
var
	NodeData : PVSFileNodeData;
begin
	NodeData := Sender.GetNodeData(Node);

	case Column of
		- 1, 0 : begin // main column, -1 if columns are hidden, 0 if they are shown
			if TextType = ttNormal then begin
				CellText := GetAbsOrRelativePath(NodeData^.FilePath);
			end else begin // ttStatic
				CellText := '';
				if Node.ChildCount > 0 then begin
					CellText := Format('[%d]', [Node.ChildCount]);
				end;
			end;
		end;
		1 : begin
			CellText := GetRowColText(NodeData.MatchData.Row, TextType);
		end;
		2 : begin
			CellText := GetRowColText(NodeData.MatchData.Col, TextType);
		end;
		3 : begin
			if (TextType = ttNormal) then begin
				var
					dummy1, dummy2 : Integer;
				CellText := NodeData.GetLineText(Settings.NodeLookSettings.IndentLines, dummy1, dummy2);
			end;

		end;
	end;

end;

procedure TRipGrepperMiddleFrame.VstResultHeaderClick(Sender : TVTHeader; HitInfo : TVTHeaderHitInfo);
begin
	// Don't call sorting procedure on right click
	// Some list-headers have a contextmenu which should popup then.
	if HitInfo.Button = mbRight then
		Exit;

	VstResult.SortTree(HitInfo.Column, Sender.SortDirection);

	if Sender.SortColumn <> HitInfo.Column then
		Sender.SortColumn := HitInfo.Column
	else if Sender.SortDirection = sdAscending then
		Sender.SortDirection := sdDescending
	else
		Sender.SortDirection := sdAscending;
end;

procedure TRipGrepperMiddleFrame.VstResultPaintText(Sender : TBaseVirtualTree; const TargetCanvas : TCanvas; Node : PVirtualNode;
Column : TColumnIndex; TextType : TVSTTextType);
begin
	if TextType = ttNormal then begin
		case Column of
			0 : begin
				TargetCanvas.Font.style := [fsBold];
				TargetCanvas.Font.Color := TREEVIEW_NORMAL_TEXT_COLOR;
			end;
		end;
	end else begin // ttStatic
		TargetCanvas.Font.Color := TREEVIEW_STAT_COLOR; // Not shown on MultiLine
	end;
end;

class function TDrawParams.Save(const _canvas : TCanvas) : TDrawParams;
begin
	Result.FgColor := _canvas.Font.Color;
	Result.BgColor := _canvas.Brush.Color;
	Result.FontSize := _canvas.Font.Size;
	Result.FontStyle := _canvas.Font.style;
end;

procedure TDrawParams.Load(const _canvas : TCanvas);
begin
	_canvas.Font.Color := FgColor;
	_canvas.Brush.Color := BgColor;
	_canvas.Font.Size := FontSize;
	_canvas.Font.style := FontStyle;
end;

end.
