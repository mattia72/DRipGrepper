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
	RipGrepper.Settings.AppSettings,
	RipGrepper.Data.HistoryItemObject,
	RipGrepper.Data.Matches,
	RipGrepper.Common.Sorter,
	RipGrepper.Common.Interfaces,
	System.Diagnostics,
	RipGrepper.Common.Constants,
	RipGrepper.Common.SimpleTypes,
	System.Threading,
	VirtualTrees, // GetIt TurboPack VirtualTree
	RipGrepper.Helper.UI,
	RipGrepper.Parsers.ParallelParser,
	ArrayEx,
	RipGrepper.Tools.ProcessUtils,
	RipGrepper.Helper.Types,
	RipGrepper.Settings.RipGrepParameterSettings,
	RipGrepper.Common.ParsedObject,
	RipGrepper.Settings.RipGrepperSettings,
	RipGrepper.UI.MiddleLeftFrame,
	RipGrepper.Common.NodeData,
	RipGrepper.UI.IFrameEvents,
	RipGrepper.Settings.FontColors,
	SVGIconImageListBase,
	SVGIconImageList;

type
	TRipGrepperMiddleFrame = class(TFrame, IFrameEvents, INewLineEventHandler, ITerminateEventProducer, IEOFProcessEventHandler)
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
		MiddleLeftFrame1 : TMiddleLeftFrame;
		SVGIconImageList1 : TSVGIconImageList;
		ActionDeleteResultNode : TAction;
		N4 : TMenuItem;
		miDeleteResultNode : TMenuItem;
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
		procedure ActionDeleteResultNodeExecute(Sender : TObject);
		procedure ActionDeleteResultNodeUpdate(Sender : TObject);
		procedure ActionOpenWithExecute(Sender : TObject);
		procedure ActionOpenWithUpdate(Sender : TObject);
		procedure ActionOpenInIdeExecute(Sender : TObject);
		procedure ActionOpenInIdeUpdate(Sender : TObject);
		procedure FrameResize(Sender : TObject);
		procedure Splitter1Moved(Sender : TObject);
		procedure SplitView1Resize(Sender : TObject);
		procedure VstResultBeforeCellPaint(Sender : TBaseVirtualTree; TargetCanvas : TCanvas; Node : PVirtualNode; Column : TColumnIndex;
			CellPaintMode : TVTCellPaintMode; CellRect : TRect; var ContentRect : TRect);
		procedure VstResultChecked(Sender : TBaseVirtualTree; Node : PVirtualNode);
		procedure VstResultColumnResize(Sender : TVTHeader; Column : TColumnIndex);
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
		procedure VstResultNodeClick(Sender : TBaseVirtualTree; const HitInfo : THitInfo);
		procedure VstResultPaintText(Sender : TBaseVirtualTree; const TargetCanvas : TCanvas; Node : PVirtualNode; Column : TColumnIndex;
			TextType : TVSTTextType);

		private const
			COL_HIDDEN = -1;
			COL_FILE = 0;
			COL_ROW_NUM = 1;
			COL_COL_NUM = 2;
			COL_MATCH_TEXT = 3;

		var
			FAbortSearch : Boolean;
			FColorSettings : TFontColors;
			FData : TRipGrepperData;
			FModuleNameAndVersion : string;
			FFileNameType : TFileNameType;
			FHeaderColRect : TRect;
			FHeaderRowRect : TRect;
			FHistItemObj : IHistoryItemObject;
			FIsParsingRunning : Boolean;
			FMeassureFirstDrawEvent : Boolean;
			FRipGrepTask : ITask;
			FSettings : TRipGrepperSettings;
			FswSearchStart : TStopwatch;
			FIconImgList : TIconImageList;
			FIsInitialized : Boolean;
			FParsingThreads : TArrayEx<TParallelParser>;
			procedure AddAsUsing(_bToImpl : Boolean);
			procedure DoSearch;
			procedure EnableActionIfResultSelected(_act : TAction);
			procedure ExpandNodes;
			procedure FilterAllFileNode();
			procedure FilterTextMode(const Node : PVirtualNode; const _sFilterPattern : string; const _filterModes : TFilterModes);
			procedure FilterFileMode(const Node : PVirtualNode; const _sFilterPattern : string; const _filterModes : TFilterModes);
			function GetAbsOrRelativePath(const _sFullPath : string) : string;
			function GetData : TRipGrepperData;
			function GetHistItemObject : IHistoryItemObject;
			function GetIsGuiReplaceMode : Boolean;
			function GetIsRgReplaceMode : Boolean;
			function AddParallelParser(const _iLineNr : Integer; const _sLine : string; const _bIsLast : Boolean) : TParallelParser;
			procedure DeleteResultNode(const node : PVirtualNode);
			function GetActiveProject() : string;
			function GetIsInitialized() : Boolean;
			function GetOpenWithRelativeBaseDirPath(const _nodeData : PVSFileNodeData) : string;
			function GetResultSelectedFilePath : string;
			function GetSelectedResultFileNodeData : PVSFileNodeData;
			function GetSettings : TRipGrepperSettings;
			procedure InitSearch;
			function IsTextMatch(_input, _filter : string; const _filterModes : TFilterModes) : Boolean;
			procedure LoadBeforeSearchSettings;
			procedure DeleteNodeAndData(const _sFilePath : string; const _node : PVirtualNode; const _bAllMatchingFile : Boolean = False);
			procedure OnLastLine(const _iLineNr : Integer);
			procedure OnParsingProgress(const _bLastLine : Boolean);
			procedure OpenSelectedInIde;
			procedure RefreshCounters;
			procedure RefreshCountersInGUI;
			procedure RunRipGrep;
			procedure SetAbortSearch(const Value : Boolean);
			procedure SetCheckedAllSubNode(ANode : PVirtualNode);
			procedure SetColumnWidths;
			procedure SetDeleteIconOnHotNodeForColumn(const _colNum : Integer; Sender : TBaseVirtualTree; Node : PVirtualNode;
				Column : TColumnIndex; var Ghosted : Boolean; var ImageIndex : TImageIndex);
			procedure SetFileIconImgIdx(Sender : TBaseVirtualTree; Node : PVirtualNode; Kind : TVTImageKind; Column : TColumnIndex;
				var Ghosted : Boolean; var ImageIndex : TImageIndex);
			procedure SetHistItemObject(const Value : IHistoryItemObject);
			function SliceArgs(const _rgp : TRipGrepParameterSettings) : TStringsArrayEx;
			procedure UpdateArgumentsAndSettings;
			procedure UpdateGui;
			property IsGuiReplaceMode : Boolean read GetIsGuiReplaceMode;
			property IsRgReplaceMode : Boolean read GetIsRgReplaceMode;
			property Settings : TRipGrepperSettings read GetSettings write FSettings;

		protected
			procedure ChangeScale(M, D : Integer; isDpiChange : Boolean); override;

			{ Private-Deklarationen }
		public
			constructor Create(AOwner : TComponent); override;
			destructor Destroy; override;
			procedure AfterHistObjChange;
			procedure AfterSearch;
			procedure AlignToolBars;
			procedure BeforeSearch(var _bAbort : Boolean);
			procedure ClearFilter;
			procedure CopyToClipboardFileOfSelected;
			procedure CopyToClipboardPathOfSelected;
			function CreateNewHistObject : IHistoryItemObject;
			procedure FilterNodes(const _sFilterPattern : string; const _filterModes : TFilterModes);
			procedure FreeAndCleanParserList;
			function GetCounterText(_ho : IHistoryItemObject) : string;
			function GetFilePathFromNode(_node : PVirtualNode) : string;
			function GetOpenWithParamsFromSelected : TOpenWithParams;
			function GetRowColText(_i : Integer; _type : TVSTTextType) : string;
			procedure Initialize();
			function IsNodeFiltered(const _nodeData : PVSFileNodeData; const _sFilterText : string; const _filterModes : TFilterModes)
				: Boolean;
			function IsSearchRunning : Boolean;
			// IEOFProcessEventHandler
			procedure OnEOFProcess;
			// INewLineEventHandler
			procedure OnNewOutputLine(const _iLineNr : Integer; const _sLine : string; _bIsLast : Boolean = False);
			procedure PrepareAndDoSearch;
			// ITerminateEventProducer
			function ProcessShouldTerminate : Boolean;
			procedure RefreshSearch;
			procedure ReloadColorSettings;
			procedure SetReplaceModeOnGrid(const _bOn : Boolean);
			procedure SetResultListViewDataToHistoryObj;
			procedure UpdateHistObjectAndCopyToSettings;
			procedure UpdateHistObjectAndGui;
			procedure UpdateRipGrepArgumentsInHistObj;
			procedure UpdateUIStyle(_sNewStyle : string = '');
			property AbortSearch : Boolean read FAbortSearch write SetAbortSearch;
			property Data : TRipGrepperData read GetData write FData;
			property ModuleNameAndVersion : string read FModuleNameAndVersion write FModuleNameAndVersion;
			property FileNameType : TFileNameType read FFileNameType write FFileNameType;
			property HistItemObject : IHistoryItemObject read GetHistItemObject write SetHistItemObject;
			property IsInitialized : Boolean read GetIsInitialized;
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
	RipGrepper.Tools.FileUtils,
	RipGrepper.Parsers.VimGrepMatchLine,
	System.Math,
	RipGrepper.UI.MainForm,
	RipGrepper.UI.BottomFrame,
	VirtualTrees.Types,
	RipGrepper.Parsers.Factory,
	RipGrepper.UI.TopFrame,
	{$IFNDEF STANDALONE} RipGrepper.Common.IOTAUtils,
	GX_UsesManager,
	{$ENDIF}
	System.Generics.Defaults,
	RipGrepper.UI.SearchForm,
	System.RegularExpressions,
	RipGrepper.Tools.Replacer,
	RipGrepper.Tools.ReleaseUtils;

{$R *.dfm}

constructor TRipGrepperMiddleFrame.Create(AOwner : TComponent);
begin
	inherited;
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperMiddleFrame.Create');
	FIconImgList := TIconImageList.Create(Handle, ImageListListView);
	MainFrame := self;
	{$IFDEF STANDALONE}
	var
	bStandalone := True;
	{$ELSE}
	var
	bStandalone := False;
	{$ENDIF}
	miAddToUSESList.Visible := not bStandalone;
	ActionAddUsingImplementation.Visible := not bStandalone;
	ActionAddUsingInterface.Visible := not bStandalone;

	FIsInitialized := False;
	TDebugUtils.DebugMessageFormat('TRipGrepperMiddleFrame.Create: AddToUsesList.Visible=%s', [BoolToStr(not bStandalone)]);
end;

destructor TRipGrepperMiddleFrame.Destroy;
begin
	TDebugUtils.DebugMessage('TRipGrepperMiddleFrame.Destroy');
	MiddleLeftFrame1.ClearHistoryObjectList;
	FData.Free;
	FIconImgList.Free;
	FreeAndCleanParserList;
	inherited;
end;

procedure TRipGrepperMiddleFrame.ActionAddUsingImplementationExecute(Sender : TObject);
begin
	AddAsUsing(True);
end;

procedure TRipGrepperMiddleFrame.ActionAddUsingImplementationUpdate(Sender : TObject);
begin
	EnableActionIfResultSelected(ActionAddUsingImplementation);
end;

procedure TRipGrepperMiddleFrame.ActionAddUsingInterfaceExecute(Sender : TObject);
begin
	AddAsUsing(False);
end;

procedure TRipGrepperMiddleFrame.ActionAddUsingInterfaceUpdate(Sender : TObject);
begin
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
			Settings.RipGrepParameters.GetCommandLine(Settings.AppSettings.CopyToClipBoardShell));
		PrepareAndDoSearch();
	end else begin
		TDebugUtils.DebugMessage('TRipGrepperTopFrame.VstHistoryNodeDblClick: ShowSearchForm cancel');
	end;

	MiddleLeftFrame1.ChangeHistoryNodeText();
	UpdateGui();
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
	nodeData : PVSFileNodeData;
begin
	nodeData := GetSelectedResultFileNodeData();
	if Assigned(nodeData) then
		Clipboard.AsText := nodeData.MatchData.LineText;
end;

procedure TRipGrepperMiddleFrame.ActionCopyLineToClipboardUpdate(Sender : TObject);
begin
	EnableActionIfResultSelected(ActionCopyLineToClipboard);
end;

procedure TRipGrepperMiddleFrame.ActionCopyMatchToClipboardExecute(Sender : TObject);
var
	nodeData : PVSFileNodeData;
begin
	nodeData := GetSelectedResultFileNodeData();
	if Assigned(nodeData) then
		Clipboard.AsText := nodeData.MatchData.LineText.Substring(nodeData.MatchData.Col - 1, nodeData.MatchData.MatchLength);
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

procedure TRipGrepperMiddleFrame.ActionDeleteResultNodeExecute(Sender : TObject);
var
	node : PVirtualNode;
begin
	for node in VstResult.SelectedNodes(True) do begin
		DeleteResultNode(node);
	end;
end;

procedure TRipGrepperMiddleFrame.ActionDeleteResultNodeUpdate(Sender : TObject);
begin
	EnableActionIfResultSelected(ActionDeleteResultNode);
end;

procedure TRipGrepperMiddleFrame.ActionOpenWithExecute(Sender : TObject);
var
	owp : TOpenWithParams;
begin
	{$IFDEF STANDALONE}
	var
	bStandalone := True;
	{$ELSE}
	var
	bStandalone := False;
	{$ENDIF}
	owp := GetOpenWithParamsFromSelected();
	if owp.IsEmpty and (not bStandalone) then begin
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
	{$IFDEF STANDALONE}
	var
	bStandalone := True;
	{$ELSE}
	var
	bStandalone := False;
	{$ENDIF}
	EnableActionIfResultSelected(ActionOpenInIde);
	ActionOpenInIde.Enabled := ActionOpenInIde.Enabled and (not bStandalone);
	ActionOpenInIde.Visible := (not bStandalone)
end;

procedure TRipGrepperMiddleFrame.AddAsUsing(_bToImpl : Boolean);
begin
	{$IFNDEF STANDALONE}
	var
		usesman : TUsesManager := TUsesManager.Create(IOTAUtils.GxOtaGetCurrentSourceEditor);
		try var fn : string := TPath.GetFileNameWithoutExtension(GetResultSelectedFilePath);

	var
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
{$ENDIF}
end;

procedure TRipGrepperMiddleFrame.AfterHistObjChange;
begin
	UpdateHistObjectAndGui
end;

procedure TRipGrepperMiddleFrame.AfterSearch;
var
	ec : TErrorCounters;
begin
	ec := HistItemObject.GetErrorCounters();
	if ec.FParserErrors > 0 then begin
		TAsyncMsgBox.ShowWarning(RG_PARSE_ERROR_MSG); // , false, self);
	end;
	if ec.FIsNoOutputError then begin
		TAsyncMsgBox.ShowWarning(RG_PRODUCED_NO_OUTPUT_MSG);
	end;
	if ec.FIsRGReportedError then begin
		TAsyncMsgBox.ShowWarning(RG_REPORTED_ERROR_MSG);
	end;
	FreeAndCleanParserList();
end;

procedure TRipGrepperMiddleFrame.AlignToolBars;
begin
	if Assigned(TopFrame) then begin
		TopFrame.AlignToolBars(PanelResult.Left, PanelHistory.Width, PanelResult.Width);
	end;
end;

procedure TRipGrepperMiddleFrame.BeforeSearch(var _bAbort : Boolean);
begin
	if _bAbort then begin
		Exit;
	end;

	InitSearch();
	FAbortSearch := False;
	UpdateArgumentsAndSettings;
	// hist object parser type should set before painting begins...
	UpdateHistObjectAndGui;
end;

procedure TRipGrepperMiddleFrame.ClearFilter;
begin
	VstResult.BeginUpdate;
	try
		for var Node in VstResult.Nodes(True) do begin
			VstResult.IsFiltered[Node] := False;
		end;
	finally
		VstResult.EndUpdate;
	end;
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

	MiddleLeftFrame1.ChangeDataHistItemObject(Result);
	MiddleLeftFrame1.AddHistoryObject(Result);
end;

procedure TRipGrepperMiddleFrame.DoSearch;
begin
	var
	bAbort := not Settings.RipGrepParameters.IsRgPathInitOk;
	ParentFrame.BeforeSearch(bAbort);
	if not bAbort then begin
		RunRipGrep();
	end;
end;

procedure TRipGrepperMiddleFrame.EnableActionIfResultSelected(_act : TAction);
begin
	_act.Enabled := VstResult.SelectedCount > 0;
end;

procedure TRipGrepperMiddleFrame.ExpandNodes;
begin
	if Settings.NodeLookSettings.ExpandNodes then begin
		VstResult.FullExpand();
	end;
end;

procedure TRipGrepperMiddleFrame.FilterAllFileNode;
begin
	for var Node : PVirtualNode in VstResult.InitializedNodes(True) do begin
		if (Node.Parent = VstResult.RootNode) then begin
			VstResult.IsFiltered[Node] := True;
		end;
	end;
end;

procedure TRipGrepperMiddleFrame.FilterNodes(const _sFilterPattern : string; const _filterModes : TFilterModes);
var
	Node : PVirtualNode;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperMiddleFrame.FilterNodes');
	VstResult.BeginUpdate;
	try
		if EFilterMode.fmFilterText in _filterModes then begin
			FilterAllFileNode();
		end;
		for Node in VstResult.InitializedNodes(True) do begin
			if EFilterMode.fmFilterFile in _filterModes then begin
				FilterFileMode(Node, _sFilterPattern, _filterModes);
			end else if EFilterMode.fmFilterText in _filterModes then begin
				FilterTextMode(Node, _sFilterPattern, _filterModes);
			end;
		end;
	finally
		VstResult.EndUpdate;
	end;
end;

procedure TRipGrepperMiddleFrame.FilterTextMode(const Node : PVirtualNode; const _sFilterPattern : string;
	const _filterModes : TFilterModes);
var
	bIsFiltered : Boolean;
	nodeData : PVSFileNodeData;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperMiddleFrame.FilterTextMode');
	bIsFiltered := False;
	var
	line := '';
	if (Node.Parent <> VstResult.RootNode) then begin
		nodeData := VstResult.GetNodeData(Node);
		bIsFiltered := IsNodeFiltered(nodeData, _sFilterPattern, _filterModes);

		VstResult.IsFiltered[Node] := bIsFiltered;
		if not bIsFiltered and Assigned(Node.Parent) and (Node.Parent <> VstResult.RootNode) then begin
			VstResult.IsFiltered[Node.Parent] := False;
		end;
		line := nodeData.MatchData.LineText;
	end;
	dbgMsg.MsgFmt('IsFiltered: %s %s', [BoolToStr(bIsFiltered, TRUE), line]);
end;

procedure TRipGrepperMiddleFrame.FilterFileMode(const Node : PVirtualNode; const _sFilterPattern : string;
	const _filterModes : TFilterModes);
var
	bIsFiltered : Boolean;
	nodeData : PVSFileNodeData;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperMiddleFrame.FilterFileMode');
	nodeData := VstResult.GetNodeData(Node);
	bIsFiltered := False;
	if (Node.Parent = VstResult.RootNode) then begin
		bIsFiltered := IsNodeFiltered(nodeData, _sFilterPattern, _filterModes);
		VstResult.IsFiltered[Node] := bIsFiltered;
	end else begin
		VstResult.IsFiltered[Node] := VstResult.IsFiltered[Node.Parent];
	end;
	dbgMsg.MsgFmt('IsFiltered: %s %s', [BoolToStr(bIsFiltered, TRUE), nodeData.FilePath]);
end;

procedure TRipGrepperMiddleFrame.FrameResize(Sender : TObject);
begin
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
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperMiddleFrame.GetAbsOrRelativePath');

	Result := _sFullPath;
	if Settings.NodeLookSettings.ShowRelativePath then begin
		{$IFDEF STANDALONE}
		var
		bStandalone := True;
		{$ELSE}
		var
		bStandalone := False;
		{$ENDIF}
		if bStandalone then begin
			actPath := Settings.ActualSearchPath;
			if (actPath.IsEmpty or (not Settings.SearchPathIsDir)) then begin
				Exit;
			end;
		end else begin
			var
			activeProject := GetActiveProject();
			actPath := TPath.GetDirectoryName(activeProject);
		end;
		dbgMsg.MsgFmt('search path: %s', [actPath]);
		Result := ExtractRelativePath(actPath + '\', _sFullPath);
		dbgMsg.MsgFmt('relative path: %s', [Result]);
	end;
end;

function TRipGrepperMiddleFrame.GetCounterText(_ho : IHistoryItemObject) : string;
begin
	Result := '';
	if not Assigned(_ho) then begin
		Exit;
	end;

	if _ho.IsLoadedFromStream then begin
		Result := Format('%s %d in %d', [TREEVIEW_HISTORY_LOADED_PREFIX, _ho.TotalMatchCount, _ho.FileCount]);
	end else begin
		var
		ec := _ho.GetErrorCounters;
		if ec.FSumOfErrors > 0 then begin
			Result := Format('%s %d in %d(%d!)', [TREEVIEW_HISTORY_COUNTER_ERROR_PREFIX, _ho.TotalMatchCount, _ho.FileCount,
				ec.FSumOfErrors]);
		end else if _ho.IsReplaceMode then begin
			Result := Format('%s %d in %d', [TREEVIEW_HISTORY_REPLACE_PREFIX, _ho.TotalMatchCount, _ho.FileCount]);
		end else begin
			if _ho.NoMatchFound then begin
				Result := TREEVIEW_HISTORY_COUNTER_NOTHING_FOUND_PREFIX + ' 0 in 0';
			end else begin
				Result := Format('%s %d in %d', [TREEVIEW_HISTORY_COUNTER_OK_PREFIX, _ho.TotalMatchCount, _ho.FileCount]);
			end;
		end;
	end;

end;

function TRipGrepperMiddleFrame.GetData : TRipGrepperData;
begin
	if not Assigned(FData) then begin
		FData := TRipGrepperData.Create(VstResult);
		MiddleLeftFrame1.Data := FData;
	end;
	Result := FData;
end;

function TRipGrepperMiddleFrame.GetFilePathFromNode(_node : PVirtualNode) : string;
var
	nodeData : PVSFileNodeData;
begin
	if _node.ChildCount = 0 then begin
		nodeData := VstResult.GetNodeData(_node.Parent);
	end else begin
		nodeData := VstResult.GetNodeData(_node);
	end;
	Result := nodeData.FilePath;
end;

function TRipGrepperMiddleFrame.GetHistItemObject : IHistoryItemObject;
begin
	if not Assigned(FHistItemObj) then begin
		FHistItemObj := MiddleLeftFrame1.GetCurrentHistoryObject();
	end;

	Result := FHistItemObj;
end;

function TRipGrepperMiddleFrame.GetIsGuiReplaceMode : Boolean;
begin
	Result := ParentFrame.TopFrame.IsGuiReplaceMode;
end;

function TRipGrepperMiddleFrame.GetIsRgReplaceMode : Boolean;
begin
	Result := Settings.IsReplaceMode;
end;

function TRipGrepperMiddleFrame.GetOpenWithParamsFromSelected : TOpenWithParams;
var
	node : PVirtualNode;
	nodeData : PVSFileNodeData;
	dataParent : PVSFileNodeData;
begin
	node := VstResult.GetFirstSelected();
	if not Assigned(node) then
		Exit;

	nodeData := VstResult.GetNodeData(node);
	if node.ChildCount > 0 then begin
		Result.RelativeBaseDirPath := GetOpenWithRelativeBaseDirPath(nodeData);
		Result.FilePath := nodeData.FilePath;
		Result.Row := 0;
		Result.Column := 0;
		Result.IsEmpty := False;
	end else begin
		dataParent := VstResult.GetNodeData(node.Parent);
		Result.RelativeBaseDirPath := GetOpenWithRelativeBaseDirPath(dataParent);
		Result.FilePath := dataParent.FilePath;
		Result.Row := nodeData.MatchData.Row;
		Result.Column := nodeData.MatchData.Col;
		Result.IsEmpty := False;
	end;

	TDebugUtils.DebugMessage('TRipGrepperMiddleFrame.GetOpenWithParamsFromSelected ' + Result.ToString);

end;

function TRipGrepperMiddleFrame.AddParallelParser(const _iLineNr : Integer; const _sLine : string; const _bIsLast : Boolean)
	: TParallelParser;
begin
	Result := TParallelParser.Create(FData, FHistItemObj);
	Result.OnLastLine := OnLastLine;
	Result.OnProgress := OnParsingProgress;
	// if _bIsLast then begin we need it on MAX_LINE
	Result.OnAfterAllFinished := ParentFrame.AfterSearch;
	// end;
	Result.SetNewLine(_iLineNr, _sLine, _bIsLast);
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

procedure TRipGrepperMiddleFrame.Initialize();
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperMiddleFrame.Initialize');

	if IsInitialized then begin
		dbgMsg.Msg('Already initialized');
		Exit;
	end;

	{$IFDEF STANDALONE}
	var
	bStandalone := True;
	{$ELSE}
	var
	bStandalone := False;
	{$ENDIF}
	if bStandalone then begin
		FModuleNameAndVersion := TReleaseUtils.GetAppNameAndVersion(Application.ExeName);
	end else begin
		FModuleNameAndVersion := TReleaseUtils.GetModuleNameAndVersion();
		PanelHistory.BevelOuter := bvNone;
		PanelResult.BevelOuter := bvNone;
	end;
	Align := alClient;
	dbgMsg.Msg('TRipGrepperMiddleFrame.Initialize ' + FModuleNameAndVersion);
	FFileNameType := ftAbsolute;
	VstResult.TreeOptions.AutoOptions := VstResult.TreeOptions.AutoOptions + [toAutoSpanColumns]; // merges empty cells
	VstResult.TreeOptions.StringOptions := VstResult.TreeOptions.StringOptions + [toShowStaticText];
	VstResult.TreeOptions.PaintOptions := VstResult.TreeOptions.PaintOptions + [toUseExplorerTheme];
	VstResult.NodeDataSize := SizeOf(TVSFileNodeData);

	ReloadColorSettings;
	dbgMsg.Msg('MiddleLeftFrame1.Initialize');
	MiddleLeftFrame1.Initialize;

	miOpenwith1.Default := bStandalone;
	miOpenInIde.Default := not bStandalone;
	if not miOpenInIde.Default then begin
		PopupMenuResult.Items[0].Visible := False;
	end;
	FIsInitialized := True;
end;

procedure TRipGrepperMiddleFrame.InitSearch;
begin
	VstResult.Clear;
	Data.ClearMatchFiles;
	// ClearData;
	FswSearchStart := TStopwatch.Create();
	FMeassureFirstDrawEvent := True;
	LoadBeforeSearchSettings();
end;

function TRipGrepperMiddleFrame.IsSearchRunning : Boolean;
begin
	Result := FIsParsingRunning or (Assigned(FRipGrepTask) and (FRipGrepTask.Status = TTaskStatus.Running));
end;

procedure TRipGrepperMiddleFrame.LoadBeforeSearchSettings;
begin
	ReloadColorSettings;
end;

procedure TRipGrepperMiddleFrame.OnEOFProcess;
begin
	TDebugUtils.DebugMessage(Format('TRipGrepperMiddleFrame.OnEOFProcess: End of processing rg.exe output in %s sec.',
		[GetElapsedTime(FswSearchStart)]));
end;

procedure TRipGrepperMiddleFrame.OnLastLine(const _iLineNr : Integer);
begin
	TDebugUtils.DebugMessage(Format('TRipGrepperMiddleFrame.OnLastLine: Last line (%d) received in %s sec.',
		[_iLineNr, GetElapsedTime(FswSearchStart)]));

	TThread.Synchronize(nil,
		procedure
		begin
			BottomFrame.ActivityIndicator1.Animate := False;
			FIsParsingRunning := False;
			ExpandNodes;
			RefreshCounters;
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
			TAsyncMsgBox.Show(Format(MSG_FORMAT_TOO_MANY_RESULTS, [_iLineNr, RG_PROCESSING_LINE_COUNT_LIMIT]), TMsgDlgType.mtWarning);
		end else begin
			Exit;
		end;
	end;

	parser := AddParallelParser(_iLineNr, _sLine, _bIsLast);

	try
		parser.Parse();
	except
		on e : EOutOfMemory do begin
			TAsyncMsgBox.Show(Format('Exception %s ' + CRLF + 'on line %d', [e.Message, _iLineNr]), TMsgDlgType.mtError);
			TMsgBox.ShowError(e.Message + CRLF + 'Too many results. Try to be more specific.');
		end;
	end;
end;

procedure TRipGrepperMiddleFrame.OnParsingProgress(const _bLastLine : Boolean);
begin
	RefreshCounters;
	VstResult.Repaint();
end;

function TRipGrepperMiddleFrame.ProcessShouldTerminate : Boolean;
begin
	Result := Assigned(FRipGrepTask) and (FRipGrepTask.Status = TTaskStatus.Canceled);
end;

procedure TRipGrepperMiddleFrame.RefreshCounters;
begin
	if Assigned(FHistItemObj) then begin
		var
		beu := TBeginEndUpdater.New(MiddleLeftFrame1.VstHistory);
		FHistItemObj.RefreshCounters(Data.FErrorCounters, Data.FileCount); // synced
	end;
	RefreshCountersInGUI;
end;

procedure TRipGrepperMiddleFrame.RefreshCountersInGUI;
begin
	TThread.Synchronize(nil,
		procedure
		begin
			MiddleLeftFrame1.VstHistory.Refresh;
			BottomFrame.StatusBarStatistic := GetCounterText(HistItemObject);
		end);
end;

procedure TRipGrepperMiddleFrame.RunRipGrep;
var
	workDir : string;
	args : TStrings;
	argsArrs : TStringsArrayEx;
	rgPath : string;
begin
	rgPath := Settings.RipGrepParameters.RipGrepPath;
	if not FileExists(rgPath) then begin
		TAsyncMsgBox.ShowError(Format(FORMAT_RIPGREP_EXE_NOT_FOUND, [Settings.PersisterFactory.FilePath]));
		Exit;
	end;

	FRipGrepTask := TTask.Create(
		procedure()
		begin
			workDir := TDirectory.GetCurrentDirectory();
			TDebugUtils.DebugMessage('TRipGrepperMiddleFrame.RunRipGrep: run: ' + rgPath + ' '
				{ } + Settings.RipGrepParameters.RipGrepArguments.DelimitedText);
			FswSearchStart := TStopwatch.StartNew;
			args := TStringList.Create;
			try
				argsArrs := SliceArgs(Settings.RipGrepParameters);
				for var i := 0 to argsArrs.MaxIndex do begin
					args.Clear;
					args.AddStrings(argsArrs[i]);
					if i < argsArrs.MaxIndex then begin
						// if cmd line is too long, we slice it and run in separate processes...
						FHistItemObj.RipGrepResult := TProcessUtils.RunProcess(
							{ } rgPath,
							{ } args,
							{ } workDir,
							{ } self as INewLineEventHandler,
							{ } self as ITerminateEventProducer,
							{ } nil);
					end else begin
						FHistItemObj.RipGrepResult := TProcessUtils.RunProcess(
							{ } rgPath,
							{ } args,
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
			FswSearchStart.Stop;

			// MainFrame not needed here...
			TopFrame.AfterHistObjChange;
			BottomFrame.AfterHistObjChange;

			TDebugUtils.DebugMessage(Format('TRipGrepperMiddleFrame.RunRipGrep: rg.exe ended in %s sec.', [FHistItemObj.ElapsedTimeText]));
		end);
	FRipGrepTask.Start;
end;

procedure TRipGrepperMiddleFrame.SetColumnWidths;
begin
	VstResult.Header.Columns[COL_FILE].Width := IfThen(toCheckSupport in VstResult.TreeOptions.MiscOptions, 70, 50);
	FHeaderRowRect := VstResult.Header.Columns[COL_ROW_NUM].GetRect();
	FHeaderColRect := VstResult.Header.Columns[COL_COL_NUM].GetRect();
end;

procedure TRipGrepperMiddleFrame.SetHistItemObject(const Value : IHistoryItemObject);
begin
	FHistItemObj := Value;
end;

procedure TRipGrepperMiddleFrame.SetResultListViewDataToHistoryObj;
begin
	TThread.Synchronize(nil,
	// Refresh listview on history click
		procedure
		begin
			VstResult.Clear;
			MiddleLeftFrame1.ChangeDataHistItemObject(FHistItemObj);
			Data.DataToGrid;
			// ListViewResult.Items.Count := matchItems.Count + FHistItemObj.ErrorCount;
		end);
end;

function TRipGrepperMiddleFrame.GetResultSelectedFilePath : string;
var
	node : PVirtualNode;
begin
	node := VstResult.GetFirstSelected();
	if not Assigned(node) then
		Exit;

	Result := GetFilePathFromNode(node);
end;

function TRipGrepperMiddleFrame.GetSelectedResultFileNodeData : PVSFileNodeData;
var
	node : PVirtualNode;
begin
	Result := nil;
	node := VstResult.GetFirstSelected();
	if not Assigned(node) then
		Exit;
	Result := VstResult.GetNodeData(node);
end;

function TRipGrepperMiddleFrame.IsNodeFiltered(const _nodeData : PVSFileNodeData; const _sFilterText : string;
const _filterModes : TFilterModes) : Boolean;
begin
	if _sFilterText.IsEmpty then begin
		Result := False;
	end else begin
		if EFilterMode.fmFilterFile in _filterModes then begin
			Result := not IsTextMatch(GetAbsOrRelativePath(_nodeData.FilePath), _sFilterText, _filterModes);
		end else begin
			Result := not IsTextMatch(_nodeData.MatchData.LineText, _sFilterText, _filterModes);
		end;
	end;
end;

function TRipGrepperMiddleFrame.IsTextMatch(_input, _filter : string; const _filterModes : TFilterModes) : Boolean;
var
	options : TRegexOptions;
begin
	if EFilterMode.fmUseRegex in _filterModes then begin
		options := [roIgnoreCase];
		if EFilterMode.fmCaseSensitive in _filterModes then begin
			options := [];
		end;
		Result := TRegEx.IsMatch(_input, _filter, options);
	end else begin
		if EFilterMode.fmCaseSensitive in _filterModes then begin
			Result := _input.Contains(_filter);
		end else begin
			Result := _input.ToUpper.Contains(_filter.ToUpper);
		end;
	end;
end;

procedure TRipGrepperMiddleFrame.OpenSelectedInIde;
var
	owp : TOpenWithParams;
begin
	{$IFNDEF STANDALONE}
	owp := GetOpenWithParamsFromSelected();
	TDebugUtils.DebugMessage(Format('TRipGrepperMiddleFrame.OpenSelectedInIde: %s', [owp.ToString]));
	IOTAUtils.GxOtaGoToFileLineColumn(owp.FilePath, owp.Row, owp.Column, owp.Column - 1);
	{$ENDIF}
end;

procedure TRipGrepperMiddleFrame.PrepareAndDoSearch;
begin
	MiddleLeftFrame1.PrepareAndDoSearch();
	DoSearch();
end;

procedure TRipGrepperMiddleFrame.RefreshSearch;
begin
	MiddleLeftFrame1.RefreshSearch();
	DoSearch();
end;

procedure TRipGrepperMiddleFrame.SetCheckedAllSubNode(ANode : PVirtualNode);
begin
	var
	beu := TBeginEndUpdater.New(VstResult);
	var
	subNode := VstResult.GetFirstChild(ANode);
	var
		bChecked : Boolean := ANode.CheckState.IsChecked;
	while Assigned(subNode) do begin
		VstResult.Selected[subNode] := bChecked;
		subNode := VstResult.GetNextSibling(subNode);
	end;
end;

procedure TRipGrepperMiddleFrame.SetReplaceModeOnGrid(const _bOn : Boolean);
begin
	if _bOn then begin
		VstResult.TreeOptions.MiscOptions := VstResult.TreeOptions.MiscOptions + [toCheckSupport];
		VstResult.TreeOptions.SelectionOptions := VstResult.TreeOptions.SelectionOptions + [toMultiSelect, toSyncCheckboxesWithSelection];
	end else begin
		VstResult.TreeOptions.MiscOptions := VstResult.TreeOptions.MiscOptions - [toCheckSupport];
		VstResult.TreeOptions.SelectionOptions := VstResult.TreeOptions.SelectionOptions - [toMultiSelect, toSyncCheckboxesWithSelection];
	end;
	// VstResult.Repaint;
	VstResult.Realign;
	SetColumnWidths;
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
			op_args := _rgp.RipGrepArguments.GetOptions();
			for var arrPath in strsArr do begin
				Result.Add(op_args + [_rgp.GuiSearchTextParams.GetSearchText] + arrPath);
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

procedure TRipGrepperMiddleFrame.UpdateGui;
begin
	SetResultListViewDataToHistoryObj();
	ExpandNodes();
	TopFrame.SetFilterBtnImage(False);
	RefreshCountersInGUI;
end;

procedure TRipGrepperMiddleFrame.UpdateHistObjectAndCopyToSettings;
begin
	HistItemObject := MiddleLeftFrame1.GetCurrentHistoryObject();
	if Assigned(HistItemObject) then begin
		HistItemObject.UpdateParserType();
		HistItemObject.CopyToSettings(Settings);
	end;
end;

procedure TRipGrepperMiddleFrame.UpdateHistObjectAndGui;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperMiddleFrame.UpdateHistObjectAndGui');

	UpdateHistObjectAndCopyToSettings;
	dbgMsg.Msg('History Object: ' + HistItemObject.RipGrepArguments.DelimitedText);
	dbgMsg.Msg('History Matches: ' + HistItemObject.TotalMatchCount.ToString);
	dbgMsg.Msg('History Files: ' + HistItemObject.FileCount.ToString);
	dbgMsg.Msg('History Errors: ' + HistItemObject.GetErrorCounters().FSumOfErrors.ToString);
	dbgMsg.Msg('History Gui: ' + HistItemObject.GuiSearchTextParams.GetSearchText + ' ' + HistItemObject.GuiSearchTextParams.ToString);
	UpdateGui();
end;

procedure TRipGrepperMiddleFrame.UpdateRipGrepArgumentsInHistObj;
begin
	HistItemObject.RipGrepArguments.Clear;
	Settings.RebuildArguments();
	HistItemObject.LoadFromSettings(Settings);
end;

procedure TRipGrepperMiddleFrame.VstResultBeforeCellPaint(Sender : TBaseVirtualTree; TargetCanvas : TCanvas; Node : PVirtualNode;
Column : TColumnIndex; CellPaintMode : TVTCellPaintMode; CellRect : TRect; var ContentRect : TRect);
begin
	if Settings.NodeLookSettings.AlternateRowColors and (Node.ChildCount = 0) then begin
		TargetCanvas.SetAlteringColors(Node.Index, FColorSettings.AlternateRow.BgColor);
	end;

	TargetCanvas.FillRect(CellRect);
end;

procedure TRipGrepperMiddleFrame.VstResultChecked(Sender : TBaseVirtualTree; Node : PVirtualNode);
begin
	if Node.Parent = VstResult.RootNode then begin
		SetCheckedAllSubNode(Node);
	end;
end;

procedure TRipGrepperMiddleFrame.VstResultColumnResize(Sender : TVTHeader; Column : TColumnIndex);
begin
	FHeaderRowRect := VstResult.Header.Columns[COL_ROW_NUM].GetRect();
	FHeaderColRect := VstResult.Header.Columns[COL_COL_NUM].GetRect();
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
			COL_FILE :
			Result := CompareText(Data1.FilePath, Data2.FilePath);
			COL_ROW_NUM :
			Result := { CompareText(s1, s2) + } CompareValue(Data1.MatchData.Row, Data2.MatchData.Row);
			COL_COL_NUM :
			Result := { CompareText(s1, s2) + } CompareValue(Data1.MatchData.Col, Data2.MatchData.Col);
			COL_MATCH_TEXT :
			Result := CompareText(Data1.MatchData.LineText, Data2.MatchData.LineText);
		end;
end;

procedure TRipGrepperMiddleFrame.VstResultDblClick(Sender : TObject);
begin
	{$IFDEF STANDALONE}
	var
	bStandalone := True;
	{$ELSE}
	var
	bStandalone := False;
	{$ENDIF}
	if bStandalone then begin
		ActionOpenWithExecute(Sender);
	end else begin
		OpenSelectedInIde;
	end;
end;

procedure TRipGrepperMiddleFrame.VstResultDrawText(Sender : TBaseVirtualTree; TargetCanvas : TCanvas; Node : PVirtualNode;
Column : TColumnIndex; const Text : string; const CellRect : TRect; var DefaultDraw : Boolean);
var
	pos : Integer;
	nodeData : PVSFileNodeData;
	s, ss0, ss1, ss1_repl, ss2 : string;
	iSpaces, iTabs, matchBegin : Integer;
begin
	case Column of
		COL_FILE : begin
			if MatchStr(Text, [RG_ERROR_MSG_PREFIX, RG_PARSE_ERROR]) then begin
				DefaultDraw := False;
				TItemDrawer.ColoredTextOut(TargetCanvas, CellRect, Text, FColorSettings.ErrorText);
			end else if MatchStr(Text, [RG_STATS_LINE]) then begin
				DefaultDraw := False;
				TItemDrawer.ColoredTextOut(TargetCanvas, CellRect, Text, FColorSettings.StatisticsText);
			end;
		end;
		COL_ROW_NUM : begin
			DefaultDraw := False;
			TItemDrawer.AlignedTextOut(TargetCanvas, CellRect, FHeaderRowRect, Text, FColorSettings.LineNumText);
		end;
		COL_COL_NUM : begin
			DefaultDraw := False;
			TItemDrawer.AlignedTextOut(TargetCanvas, CellRect, FHeaderColRect, Text, FColorSettings.ColNumText);
		end;
		COL_MATCH_TEXT : begin
			case FHistItemObj.ParserType of
				ptRipGrepSearch, ptRipGrepPrettySearch : begin
					DefaultDraw := False;
					// First, store the default font size and color number
					var
					backup := TDrawParams.Save(TargetCanvas);

					nodeData := VstResult.GetNodeData(Node);
					s := nodeData.GetLineText(not Settings.NodeLookSettings.IndentLines, iSpaces, iTabs);

					matchBegin := nodeData.MatchData.Col - 1 - (iSpaces + iTabs);

					ss0 := s.Substring(0, matchBegin).Replace(#9, TREEVIEW_INDENT_TAB_AS_SPACES, [rfReplaceAll]);
					pos := TargetCanvas.TextWidth(ss0);

					TItemDrawer.ColoredTextOut(TargetCanvas, CellRect, ss0, FColorSettings.NormalText);

					ss1 := s.Substring(matchBegin, nodeData.MatchData.MatchLength);
					if IsGuiReplaceMode and (not Settings.LastSearchText.IsEmpty) then begin
						ss1_repl := TReplaceHelper.ReplaceString(ss1, Settings.LastSearchText,
						{ } Settings.RipGrepParameters.ReplaceText, 1, TopFrame.GetReplaceMode());
					end;
					ss2 := s.Substring(matchBegin + nodeData.MatchData.MatchLength);

					if IsGuiReplaceMode or IsRgReplaceMode then begin
						TItemDrawer.SetTextColor(TargetCanvas, FColorSettings.ReplacedText);
					end else begin
						TItemDrawer.SetTextColor(TargetCanvas, FColorSettings.MatchText);
					end;
					if not IsRgReplaceMode then begin
						TargetCanvas.TextOut(CellRect.Left + pos, TREEVIEW_FONTSPACE, ss1);
						pos := pos + TargetCanvas.TextWidth(ss1);
					end;
					if IsGuiReplaceMode or IsRgReplaceMode then begin
						TItemDrawer.SetTextColor(TargetCanvas, FColorSettings.ReplaceText);
						TargetCanvas.TextOut(CellRect.Left + pos, TREEVIEW_FONTSPACE, ss1_repl);
						pos := pos + TargetCanvas.TextWidth(ss1_repl);
					end;

					backup.Load(TargetCanvas);
					TItemDrawer.ColoredTextOut(TargetCanvas, CellRect, ss2, FColorSettings.NormalText, pos + 1);
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
begin
	if not(Kind in [ikNormal, ikSelected]) then begin
		Exit;
	end;

	SetDeleteIconOnHotNodeForColumn(COL_ROW_NUM, Sender, Node, Column, Ghosted, ImageIndex);

	// Otherwise, show file icons as before
	if (ImageIndex = -1) and (Node.ChildCount > 0) then begin
		SetDeleteIconOnHotNodeForColumn(COL_FILE, Sender, Node, Column, Ghosted, ImageIndex);

		if (ImageIndex = -1) and Settings.NodeLookSettings.ShowFileIcon then begin
			SetFileIconImgIdx(Sender, Node, Kind, Column, Ghosted, ImageIndex);
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
		COL_HIDDEN, COL_FILE : begin // main column, -1 if columns are hidden, 0 if they are shown
			if TextType = ttNormal then begin
				if Node.Parent = VstResult.RootNode then begin
					CellText := GetAbsOrRelativePath(NodeData^.FilePath);
				end else begin
					CellText := NodeData^.FilePath;
				end;
			end else begin
				// ttStatic
				CellText := '';
				if Node.Parent = VstResult.RootNode then begin
					CellText := Format('[%d]', [Node.ChildCount]);
				end;
			end;
		end;
		COL_ROW_NUM : begin
			CellText := GetRowColText(NodeData.MatchData.Row, TextType);
		end;
		COL_COL_NUM : begin
			CellText := GetRowColText(NodeData.MatchData.Col, TextType);
		end;
		COL_MATCH_TEXT : begin
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
			COL_FILE : begin
				TItemDrawer.SetTextColor(TargetCanvas, FColorSettings.FileText, false);
			end;
		end;
	end else begin // ttStatic
		TItemDrawer.SetTextColor(TargetCanvas, FColorSettings.CounterText, false); // Not shown on MultiLine
	end;
end;

procedure TRipGrepperMiddleFrame.ChangeScale(M, D : Integer; isDpiChange : Boolean);
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperMiddleFrame.ChangeScale');

	inherited ChangeScale(M, D, isDpiChange);
	if isDpiChange then begin
		dbgMsg.MsgFmt('M(%d) / D(%d) = %d%%', [M, D, MulDiv(100, M, D)]);
		dbgMsg.MsgFmt('Orig VstResult Fonts: %d', [VstResult.Font.Height]);
		VstResult.Header.Font.Height := MulDiv(VstResult.Header.Font.Height, M, D);
		// VstResult.Font.Height := MulDiv(VstResult.Font.Height, M, D); too much
		dbgMsg.MsgFmt('New VstResult Fonts: %d', [VstResult.Font.Height]);

	end;
end;

procedure TRipGrepperMiddleFrame.DeleteResultNode(const node : PVirtualNode);
var
	bDeleteParent : Boolean;
	nodeData : PVSFileNodeData;
	parentNode : PVirtualNode;
	parentNodeData : PVSFileNodeData;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperMiddleFrame.DeleteResultNode');
	if not Assigned(node) then begin
		Exit;
	end;

	nodeData := VstResult.GetNodeData(node);
	if (node.Parent <> VstResult.RootNode) then begin
		parentNode := node.Parent;
		parentNodeData := VstResult.GetNodeData(parentNode);
		var
		sFilePath := parentNodeData.FilePath;
		dbgMsg.MsgFmt('sFilePath = %s', [sFilePath]);
		bDeleteParent := parentNode.ChildCount = 1;

		DeleteNodeAndData(sFilePath, node);
		if bDeleteParent then begin
			DeleteNodeAndData(sFilePath, parentNode, true);
		end;
	end else begin
		DeleteNodeAndData(nodeData.FilePath, node, true);
	end;
	VstResult.Refresh;
	RefreshCounters;
end;

procedure TRipGrepperMiddleFrame.FreeAndCleanParserList;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperMiddleFrame.FreeAndCleanParserList');

	for var i := FParsingThreads.MaxIndex downto 0 do begin
		var
		pt := FParsingThreads[i];
		if pt.IsParsingDone then begin
			// dbgMsg.MsgFmt('Free parser of line %d', [pt.LineNr]);
			// FParsingThreads.Delete(i);
			pt.Free;
		end else begin
			dbgMsg.MsgFmt('Parser of line %d is busy.', [pt.LineNr]);
			raise Exception.Create('Parser of line busy, can''t free.');
		end;
	end;
	FParsingThreads.Clear;
	// dbgMsg.MsgFmt('FParsingThreads.Count %d.', [FParsingThreads.Count])
end;

function TRipGrepperMiddleFrame.GetActiveProject() : string;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperMiddleFrame.GetActiveProject');
	var
	extSettings := Settings.SearchFormSettings.ExtensionSettings;
	Result := extSettings.CurrentIDEContext.ActiveProject;
end;

function TRipGrepperMiddleFrame.GetIsInitialized() : Boolean;
begin
	Result := FIsInitialized;
end;

function TRipGrepperMiddleFrame.GetOpenWithRelativeBaseDirPath(const _nodeData : PVSFileNodeData) : string;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperMiddleFrame.GetOpenWithRelativeBaseDirPath');
	Result := '';
	{$IFDEF STANDALONE}
	Result := IfThen(Settings.SearchPathIsDir, Settings.ActualSearchPath, ExtractFileDir(_nodeData.FilePath));
	{$ELSE}
	var
	activeProject := GetActiveProject();
	Result := TPath.GetDirectoryName(activeProject);
	{$ENDIF}
	dbgMsg.MsgFmt('OpenWith <DIR>=%s', [Result]);
end;

procedure TRipGrepperMiddleFrame.DeleteNodeAndData(const _sFilePath : string; const _node : PVirtualNode;
const _bAllMatchingFile : Boolean = False);
var
	nodeData : PVSFileNodeData;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperMiddleFrame.DeleteNodeAndData');

	nodeData := VstResult.GetNodeData(_node);
	var
	removed := HistItemObject.Matches.DeleteItemByNodeData(_sFilePath, _node, nodeData, _bAllMatchingFile);
	dbgMsg.MsgFmt('%d HistItemObject.Matches removed', [removed]);

	if (_node.Parent = VstResult.RootNode) and _bAllMatchingFile then begin
		dbgMsg.MsgFmt('DeleteChildren of idx = %d', [_node.Index]);
		VstResult.DeleteChildren(_node);
	end;
	dbgMsg.MsgFmt('DeleteNode of idx = %d', [_node.Index]);
	VstResult.DeleteNode(_node);
end;

procedure TRipGrepperMiddleFrame.ReloadColorSettings;
begin
	// load color settings
	Settings.FontColorSettings.ReloadColors;
	FColorSettings := Settings.FontColorSettings.FontColors;
	VstResult.Repaint;
	MiddleLeftFrame1.ReloadColorSettings;
end;

procedure TRipGrepperMiddleFrame.SetAbortSearch(const Value : Boolean);
begin
	FAbortSearch := Value;
end;

procedure TRipGrepperMiddleFrame.SetDeleteIconOnHotNodeForColumn(const _colNum : Integer; Sender : TBaseVirtualTree; Node : PVirtualNode;
Column : TColumnIndex; var Ghosted : Boolean; var ImageIndex : TImageIndex);
begin
	ImageIndex := -1;

	// Always show the delete icon ('x') on every hot node in the file column
	if (Column = _colNum) and (Sender.HotNode = Node) then begin
		ImageIndex := LV_IMG_IDX_X;
	end;
end;

procedure TRipGrepperMiddleFrame.SetFileIconImgIdx(Sender : TBaseVirtualTree; Node : PVirtualNode; Kind : TVTImageKind;
Column : TColumnIndex; var Ghosted : Boolean; var ImageIndex : TImageIndex);
var
	NodeData : PVSFileNodeData;
begin
	NodeData := Sender.GetNodeData(Node);
	case Kind of
		ikNormal, ikSelected :
		case Column of
			COL_FILE : begin
				var
				text := NodeData^.FilePath;
				if TRegEx.IsMatch(text, '(^' + RG_ERROR_MSG_PREFIX + '|' + RG_ENDED_ERROR + ')') then begin
					ImageIndex := LV_IMG_IDX_ERROR;
					// ImageIndex := FIconImgList.GetImgIndexFromResourceDll('ERROR', ICON_IDX_ERROR);
				end else if text = RG_STATS_LINE then begin
					ImageIndex := LV_IMG_IDX_INFO;
					// ImageIndex := FIconImgList.GetImgIndexFromResourceDll('STATISTIC', ICON_IDX_STATISTIC);
				end else begin;
					ImageIndex := FIconImgList.GetIconImgIndex(NodeData^.FilePath);
				end;
			end;
		end;
	end;
end;

procedure TRipGrepperMiddleFrame.UpdateUIStyle(_sNewStyle : string = '');
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperMiddleFrame.UpdateUIStyle');

	if not _sNewStyle.IsEmpty then begin
		StyleName := _sNewStyle;
		BottomFrame.UpdateUIStyle(styleName);
	end;

	dbgMsg.MsgFmt('MainFrame.StyleName = %s', [StyleName])
end;

procedure TRipGrepperMiddleFrame.VstResultNodeClick(Sender : TBaseVirtualTree; const HitInfo : THitInfo);
var
	node : PVirtualNode;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperMiddleFrame.VstResultNodeClick');
	node := HitInfo.HitNode;
	dbgMsg.MsgFmt('Node index = %d', [node.Index]);

	if (hiOnNormalIcon in HitInfo.HitPositions) then begin
		DeleteResultNode(node);
	end;
end;

end.
