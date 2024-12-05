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
	RipGrepper.Settings.FontColors;

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
		ImageList1 : TImageList;
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
			FExeVersion : string;
			FFileNameType : TFileNameType;
			FHistItemObj : IHistoryItemObject;
			FIsParsingRunning : Boolean;
			FMeassureFirstDrawEvent : Boolean;
			FRipGrepTask : ITask;
			FSettings : TRipGrepperSettings;
			FswSearchStart : TStopwatch;
			FIconImgList : TIconImageList;
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
			function GetNewParallelParser : TParallelParser;
			function GetResultSelectedFilePath : string;
			function GetSelectedResultFileNodeData : PVSFileNodeData;
			function GetSettings : TRipGrepperSettings;
			procedure InitSearch;
			function IsTextMatch(_input, _filter : string; const _filterModes : TFilterModes) : Boolean;
			procedure LoadBeforeSearchSettings;
			procedure OnLastLine(const _iLineNr : Integer);
			procedure OnParsingProgress;
			procedure OpenSelectedInIde;
			procedure RefreshCounters;
			procedure RefreshCountersInGUI;
			procedure RunRipGrep;
			procedure SelectAllSubNode(ANode : PVirtualNode);
			procedure SetColumnWidths;
			procedure SetHistItemObject(const Value : IHistoryItemObject);
			function SliceArgs(const _rgp : TRipGrepParameterSettings) : TStringsArrayEx;
			procedure UpdateArgumentsAndSettings;
			procedure UpdateGui;
			property IsGuiReplaceMode : Boolean read GetIsGuiReplaceMode;
			property IsRgReplaceMode : Boolean read GetIsRgReplaceMode;
			property Settings : TRipGrepperSettings read GetSettings write FSettings;

			{ Private-Deklarationen }
		public
			constructor Create(AOwner : TComponent); override;
			destructor Destroy; override;
			procedure AfterHistObjChange;
			procedure AfterSearch;
			procedure AlignToolBars;
			procedure BeforeSearch;
			procedure ClearFilter;
			procedure CopyToClipboardFileOfSelected;
			procedure CopyToClipboardPathOfSelected;
			function CreateNewHistObject : IHistoryItemObject;
			procedure FilterNodes(const _sFilterPattern : string; const _filterModes : TFilterModes);
			function GetCounterText(Data : IHistoryItemObject) : string;
			function GetFilePathFromNode(_node : PVirtualNode) : string;
			function GetOpenWithParamsFromSelected : TOpenWithParams;
			function GetRowColText(_i : Integer; _type : TVSTTextType) : string;
			procedure Init;
			function IsNodeFiltered(const Data : PVSFileNodeData; const _sFilterText : string; const _filterModes : TFilterModes) : Boolean;
			function IsSearchRunning : Boolean;
			// IEOFProcessEventHandler
			procedure OnEOFProcess;
			// INewLineEventHandler
			procedure OnNewOutputLine(const _iLineNr : Integer; const _sLine : string; _bIsLast : Boolean = False);
			procedure PrepareAndDoSearch;
			// ITerminateEventProducer
			function ProcessShouldTerminate : Boolean;
			procedure RefreshSearch;
			procedure SetReplaceModeOnGrid(const _bOn : Boolean);
			procedure SetResultListViewDataToHistoryObj;
			procedure UpdateHistObjectAndCopyToSettings;
			procedure UpdateHistObjectAndGui;
			procedure UpdateRipGrepArgumentsInHistObj;
			property AbortSearch : Boolean read FAbortSearch write FAbortSearch;
			property Data : TRipGrepperData read GetData write FData;
			property ExeVersion : string read FExeVersion write FExeVersion;
			property FileNameType : TFileNameType read FFileNameType write FFileNameType;
			property HistItemObj : IHistoryItemObject read FHistItemObj write FHistItemObj;
			property HistItemObject : IHistoryItemObject read GetHistItemObject write SetHistItemObject;
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
	System.RegularExpressions;

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

	TDebugUtils.DebugMessageFormat('TRipGrepperMiddleFrame.Create: AddToUsesList.Visible=%s', [BoolToStr(not bStandalone)]);
end;

destructor TRipGrepperMiddleFrame.Destroy;
begin
	TDebugUtils.DebugMessage('TRipGrepperMiddleFrame.Destroy');
	MiddleLeftFrame1.ClearHistoryObjectList;
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
			Settings.RipGrepParameters.GetCommandLine);
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
	ec := HistItemObj.GetErrorCounters();
	if ec.FParserErrors > 0 then begin
		TMsgBox.ShowWarning(RG_PARSE_ERROR_MSG); // , false, self);
	end;
	if ec.FIsNoOutputError then begin
		TMsgBox.ShowWarning(RG_PRODUCED_NO_OUTPUT_MSG);
	end;
	if ec.FIsRGReportedError then begin
		TMsgBox.ShowWarning(RG_REPORTED_ERROR_MSG);
	end;

end;

procedure TRipGrepperMiddleFrame.AlignToolBars;
begin
	if Assigned(TopFrame) then begin
		TopFrame.AlignToolBars(PanelResult.Left, PanelHistory.Width, PanelResult.Width);
	end;
end;

procedure TRipGrepperMiddleFrame.BeforeSearch;
begin
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
	ParentFrame.BeforeSearch();
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
	Data : PVSFileNodeData;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperMiddleFrame.FilterTextMode');
	bIsFiltered := False;
	var
	line := '';
	if (Node.Parent <> VstResult.RootNode) then begin
		Data := VstResult.GetNodeData(Node);
		bIsFiltered := IsNodeFiltered(Data, _sFilterPattern, _filterModes);

		VstResult.IsFiltered[Node] := bIsFiltered;
		if not bIsFiltered and Assigned(Node.Parent) and (Node.Parent <> VstResult.RootNode) then begin
			VstResult.IsFiltered[Node.Parent] := False;
		end;
		line := Data.MatchData.LineText;
	end;
	dbgMsg.MsgFmt('IsFiltered: %s %s', [BoolToStr(bIsFiltered, TRUE), line]);
end;

procedure TRipGrepperMiddleFrame.FilterFileMode(const Node : PVirtualNode; const _sFilterPattern : string;
	const _filterModes : TFilterModes);
var
	bIsFiltered : Boolean;
	Data : PVSFileNodeData;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TRipGrepperMiddleFrame.FilterFileMode');
	Data := VstResult.GetNodeData(Node);
	bIsFiltered := False;
	if (Node.Parent = VstResult.RootNode) then begin
		bIsFiltered := IsNodeFiltered(Data, _sFilterPattern, _filterModes);
		VstResult.IsFiltered[Node] := bIsFiltered;
	end else begin
		VstResult.IsFiltered[Node] := VstResult.IsFiltered[Node.Parent];
	end;
	dbgMsg.MsgFmt('IsFiltered: %s %s', [BoolToStr(bIsFiltered, TRUE), Data.FilePath]);
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
	var
	ec := Data.GetErrorCounters;
	if ec.FSumOfErrors > 0 then begin
		Result := Format('%s %d in %d(%d!)', [TREEVIEW_HISTORY_COUNTER_ERROR_PREFIX, Data.TotalMatchCount, Data.FileCount,
			ec.FSumOfErrors]);
	end else if Data.IsReplaceMode then begin
		Result := Format('%s %d in %d', [TREEVIEW_HISTORY_REPLACE_PREFIX, Data.TotalMatchCount, Data.FileCount]);
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
		MiddleLeftFrame1.Data := FData;
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
	{$IFDEF STANDALONE}
	var
	bStandalone := True;
	{$ELSE}
	var
	bStandalone := False;
	{$ENDIF}
	if bStandalone then begin
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

	MiddleLeftFrame1.Init;

	miOpenwith1.Default := bStandalone;
	miOpenInIde.Default := not bStandalone;
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
	LoadBeforeSearchSettings();
end;

function TRipGrepperMiddleFrame.IsSearchRunning : Boolean;
begin
	Result := FIsParsingRunning or (Assigned(FRipGrepTask) and (FRipGrepTask.Status = TTaskStatus.Running));
end;

procedure TRipGrepperMiddleFrame.LoadBeforeSearchSettings;
begin
	Settings.FontColorSettings.ReloadColors;
	FColorSettings := Settings.FontColorSettings.FontColors;
end;

procedure TRipGrepperMiddleFrame.OnEOFProcess;
begin
	TDebugUtils.DebugMessage(Format('TRipGrepperMiddleFrame.OnEOFProcess: End of processing rg.exe output in %s sec.',
		[GetElapsedTime(FswSearchStart)]));
end;

procedure TRipGrepperMiddleFrame.OnLastLine(const _iLineNr : Integer);
begin
	TDebugUtils.DebugMessage(Format('TRipGrepperMiddleFrame.OnLastLine: Last line (%d.) received in %s sec.',
		[_iLineNr, GetElapsedTime(FswSearchStart)]));

	TThread.Synchronize(nil,
		procedure
		begin
			BottomFrame.ActivityIndicator1.Animate := False;
			FIsParsingRunning := False;
			ExpandNodes;
			RefreshCounters;
			ParentFrame.AfterSearch();
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
			TMsgBox.ShowWarning(Format(MSG_FORMAT_TOO_MANY_RESULTS, [_iLineNr, RG_PROCESSING_LINE_COUNT_LIMIT]));
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
		beu := TBeginEndUpdater.New(MiddleLeftFrame1.VstHistory);
		FHistItemObj.SetErrorCounters(Data.FErrorCounters); // synced
		FHistItemObj.FileCount := Data.FileCount; // synced
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
			FswSearchStart.Stop;
			TDebugUtils.DebugMessage(Format('TRipGrepperMiddleFrame.RunRipGrep: rg.exe ended in %s sec.', [FHistItemObj.ElapsedTimeText]));
		end);
	FRipGrepTask.Start;
end;

procedure TRipGrepperMiddleFrame.SetColumnWidths;
begin
	VstResult.Header.Columns[COL_FILE].Width := IfThen(toCheckSupport in VstResult.TreeOptions.MiscOptions, 70, 50);
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
			MiddleLeftFrame1.ChangeDataHistItemObject(FHistItemObj);
			Data.DataToGrid;
			// ListViewResult.Items.Count := matchItems.Count + FHistItemObj.ErrorCount;
			{$IFDEF THREADSAFE_LIST}
			FHistItemObj.Matches.Unlock;
			{$ENDIF}
		end);
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

function TRipGrepperMiddleFrame.IsNodeFiltered(const Data : PVSFileNodeData; const _sFilterText : string; const _filterModes : TFilterModes)
	: Boolean;
begin
	if _sFilterText.IsEmpty then begin
		Result := False;
	end else begin
		if EFilterMode.fmFilterFile in _filterModes then begin
			Result := not IsTextMatch(GetAbsOrRelativePath(Data.FilePath), _sFilterText, _filterModes);
		end else begin
			Result := not IsTextMatch(Data.MatchData.LineText, _sFilterText, _filterModes);
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
	TDebugUtils.DebugMessage(Format('TRipGrepperMiddleFrame.OpenSelectedInIde: %s(%d:%d)', [owp.FileName, owp.Row, owp.Column]));
	IOTAUtils.GxOtaGoToFileLineColumn(owp.FileName, owp.Row, owp.Column, owp.Column - 1);
	{$ENDIF}
end;

procedure TRipGrepperMiddleFrame.PrepareAndDoSearch;
begin
	MiddleLeftFrame1.PrepareAndDoSearch();
	DoSearch();
	MiddleLeftFrame1.ChangeHistoryNodeText;
end;

procedure TRipGrepperMiddleFrame.RefreshSearch;
begin
	MiddleLeftFrame1.RefreshSearch();
	DoSearch();
end;

procedure TRipGrepperMiddleFrame.SelectAllSubNode(ANode : PVirtualNode);
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

procedure TRipGrepperMiddleFrame.UpdateGui;
begin
	SetResultListViewDataToHistoryObj();
	ExpandNodes();
	TopFrame.SetFilterBtnImage(False);
	RefreshCountersInGUI;
end;

procedure TRipGrepperMiddleFrame.UpdateHistObjectAndCopyToSettings;
begin
	FHistItemObj := MiddleLeftFrame1.GetCurrentHistoryObject();
	if Assigned(FHistItemObj) then begin
		FHistItemObj.UpdateParserType();
		FHistItemObj.CopyToSettings(Settings);
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
	dbgMsg.Msg('History Gui: ' + HistItemObject.GuiSearchTextParams.SearchText + ' ' + HistItemObject.GuiSearchTextParams.ToString);
	UpdateGui();
end;

procedure TRipGrepperMiddleFrame.UpdateRipGrepArgumentsInHistObj;
begin
	FHistItemObj.RipGrepArguments.Clear;
	Settings.RebuildArguments();
	FHistItemObj.LoadFromSettings(Settings);
end;

procedure TRipGrepperMiddleFrame.VstResultBeforeCellPaint(Sender : TBaseVirtualTree; TargetCanvas : TCanvas; Node : PVirtualNode;
Column : TColumnIndex; CellPaintMode : TVTCellPaintMode; CellRect : TRect; var ContentRect : TRect);
begin
	if Settings.NodeLookSettings.AlternateRowColors and (Node.ChildCount = 0) then begin
		TargetCanvas.SetAlteringColors(Node.Index);
	end;

	TargetCanvas.FillRect(CellRect);
end;

procedure TRipGrepperMiddleFrame.VstResultChecked(Sender : TBaseVirtualTree; Node : PVirtualNode);
begin
	if Node.Parent = VstResult.RootNode then begin
		SelectAllSubNode(Node);
	end;
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
	Data : PVSFileNodeData;
	s, ss0, ss1, ss1_repl, ss2 : string;
	iSpaces, iTabs, matchBegin : Integer;
begin
	case Column of
		COL_FILE : begin
			if MatchStr(Text, [RG_ERROR_MSG_PREFIX, RG_PARSE_ERROR]) then begin
				DefaultDraw := False;
				TItemDrawer.SetTextColor(TargetCanvas, FColorSettings.ErrorText, false);
				TargetCanvas.TextOut(CellRect.Left, TREEVIEW_FONTSPACE, Text);
			end else if MatchStr(Text, [RG_STATS_LINE]) then begin
				DefaultDraw := False;
				TItemDrawer.SetTextColor(TargetCanvas, FColorSettings.StatisticsText, false);
				TargetCanvas.TextOut(CellRect.Left, TREEVIEW_FONTSPACE, Text);
			end;
		end;
		COL_MATCH_TEXT : begin
			case FHistItemObj.ParserType of
				ptRipGrepSearch, ptRipGrepPrettySearch : begin
					DefaultDraw := False;
					// First, store the default font size and color number
					var
					backup := TDrawParams.Save(TargetCanvas);

					Data := VstResult.GetNodeData(Node);
					s := Data.GetLineText(not Settings.NodeLookSettings.IndentLines, iSpaces, iTabs);

					matchBegin := Data.MatchData.Col - 1 - (iSpaces + iTabs);

					ss0 := s.Substring(0, matchBegin).Replace(#9, TREEVIEW_INDENT_TAB_AS_SPACES, [rfReplaceAll]);
					pos := TargetCanvas.TextWidth(ss0);

					TItemDrawer.SetTextColor(TargetCanvas, FColorSettings.NormalText, false);
					TargetCanvas.TextOut(CellRect.Left, TREEVIEW_FONTSPACE, ss0);

					ss1 := s.Substring(matchBegin, Data.MatchData.MatchLength);
					if IsGuiReplaceMode and (not Settings.LastSearchText.IsEmpty) then begin
						ss1_repl := TRegEx.Replace(ss1, Settings.LastSearchText, Settings.RipGrepParameters.ReplaceText, [roIgnoreCase]);
					end;
					ss2 := s.Substring(matchBegin + Data.MatchData.MatchLength);

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
					TItemDrawer.SetTextColor(TargetCanvas, FColorSettings.NormalText, false);
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
			end else begin // ttStatic
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

end.
