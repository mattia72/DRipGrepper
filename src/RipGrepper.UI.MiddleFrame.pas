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
	RipGrepper.Common.Settings,
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
	TRipGrepperMiddleFrame = class(TFrame, INewLineEventHandler, ITerminateEventProducer, IEOFProcessEventHandler)
		ActionList : TActionList;
		ActionCopyFileName : TAction;
		ActionCopyPathToClipboard : TAction;
		ActionOpenWith : TAction;
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
		procedure ActionAddUsingImplementationExecute(Sender : TObject);
		procedure ActionAddUsingImplementationUpdate(Sender : TObject);
		procedure ActionAddUsingInterfaceExecute(Sender : TObject);
		procedure ActionAddUsingInterfaceUpdate(Sender : TObject);
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
		procedure FrameResize(Sender : TObject);
		procedure PopupMenuHistoryPopup(Sender : TObject);
		procedure Splitter1Moved(Sender : TObject);
		procedure SplitView1Resize(Sender : TObject);
		procedure VstHistoryDrawText(Sender : TBaseVirtualTree; TargetCanvas : TCanvas; Node : PVirtualNode; Column : TColumnIndex; const Text : string;
			const CellRect : TRect; var DefaultDraw : Boolean);
		procedure VstHistoryFreeNode(Sender : TBaseVirtualTree; Node : PVirtualNode);
		procedure VstHistoryGetText(Sender : TBaseVirtualTree; Node : PVirtualNode; Column : TColumnIndex; TextType : TVSTTextType; var CellText : string);
		procedure VstHistoryMeasureItem(Sender : TBaseVirtualTree; TargetCanvas : TCanvas; Node : PVirtualNode; var NodeHeight : Integer);
		procedure VstHistoryNodeClick(Sender : TBaseVirtualTree; const HitInfo : THitInfo);
		procedure VstHistoryNodeDblClick(Sender : TBaseVirtualTree; const HitInfo : THitInfo);
		procedure VstResultBeforeCellPaint(Sender : TBaseVirtualTree; TargetCanvas : TCanvas; Node : PVirtualNode; Column : TColumnIndex;
			CellPaintMode : TVTCellPaintMode; CellRect : TRect; var ContentRect : TRect);
		procedure VstResultCompareNodes(Sender : TBaseVirtualTree; Node1, Node2 : PVirtualNode; Column : TColumnIndex; var Result : Integer);
		procedure VstResultDblClick(Sender : TObject);
		procedure VstResultDrawText(Sender : TBaseVirtualTree; TargetCanvas : TCanvas; Node : PVirtualNode; Column : TColumnIndex; const Text : string;
			const CellRect : TRect; var DefaultDraw : Boolean);
		procedure VstResultFreeNode(Sender : TBaseVirtualTree; Node : PVirtualNode);
		procedure VstResultGetImageIndex(Sender : TBaseVirtualTree; Node : PVirtualNode; Kind : TVTImageKind; Column : TColumnIndex; var Ghosted : Boolean;
			var ImageIndex : TImageIndex);
		procedure VstResultGetText(Sender : TBaseVirtualTree; Node : PVirtualNode; Column : TColumnIndex; TextType : TVSTTextType; var CellText : string);
		procedure VstResultHeaderClick(Sender : TVTHeader; HitInfo : TVTHeaderHitInfo);
		procedure VstResultPaintText(Sender : TBaseVirtualTree; const TargetCanvas : TCanvas; Node : PVirtualNode; Column : TColumnIndex;
			TextType : TVSTTextType);

		private
			FAbortSearch : Boolean;
			FCurrentHistoryItemIndex : Integer;
			FData : TRipGrepperData;
			FExeVersion : string;
			FFileNameType : TFileNameType;
			FHistItemObject : THistoryItemObject;
			FHistoryObjectList : TStringList;
			FIsParsingRunning : Boolean;
			FMaxWidths : TArray<Integer>;
			FMeassureFirstDrawEvent : Boolean;
			FRipGrepTask : ITask;
			FSettings : TRipGrepperSettings;
			FswSearchStart : TStopwatch;
			FIconImgList : TIconImageList;
			FParsingThreads : TArrayEx<TParallelParser>;
			procedure AddAsUsing(_bToImpl : Boolean);
			procedure ClearHistoryObjectList;
			procedure CreateNewHistObject;
			procedure EnableActionIfResultSelected(_act : TAction);
			procedure ExpandNodes;
			function GetAbsOrRelativePath(const _sFullPath : string) : string;
			function GetCounterText(Data : THistoryItemObject) : string;
			function GetData : TRipGrepperData;
			function GetHistItemObject : THistoryItemObject;
			function GetHistoryObject(const _index : Integer) : THistoryItemObject;
			function GetHistoryObjectList : TStringList;
			function GetNewParallelParser : TParallelParser;
			function GetNodeByIndex(Tree : TVirtualStringTree; Index : Integer) : PVirtualNode;
			function GetResultSelectedFilePath : string;
			function GetSelectedResultFileNodeData : PVSFileNodeData;
			function GetSettings : TRipGrepperSettings;
			procedure LoadBeforeSearchSettings;
			procedure OnLastLine(const _iLineNr : Integer);
			procedure OnParsingProgress;
			procedure RefreshCounters;
			procedure RefreshCountersInGUI;
			procedure RunRipGrep;
			procedure SetColumnWidths;
			procedure SetHistItemObject(const Value : THistoryItemObject);
			function SliceArgs(const _rgp : TRipGrepParameterSettings) : TStringsArrayEx;
			procedure UpdateArgumentsAndSettings;
			procedure UpdateHistObjectAndGui;
			procedure UpdateRipGrepArgumentsInHistObj;
			property Settings : TRipGrepperSettings read GetSettings write FSettings;

			{ Private-Deklarationen }
		public
			constructor Create(AOwner : TComponent); override;
			destructor Destroy; override;
			procedure AddOrUpdateHistoryItem;
			procedure AlignToolBars;
			procedure ChangeDataHistItemObject(_ho : THistoryItemObject);
			procedure ClearHistoryObject;
			procedure CopyToClipboardFileOfSelected;
			procedure CopyToClipboardPathOfSelected;
			procedure DoSearch;
			function GetFilePathFromNode(_node : PVirtualNode) : string;
			function GetOpenWithParamsFromSelected : TOpenWithParams;
			function GetRowColText(_i : Integer; _type : TVSTTextType) : string;
			procedure Init;
			procedure InitSearch;
			function IsSearchRunning : Boolean;
			// IEOFProcessEventHandler
			procedure OnEOFProcess;
			// INewLineEventHandler
			procedure OnNewOutputLine(const _iLineNr : Integer; const _sLine : string; _bIsLast : Boolean = False);
			// ITerminateEventProducer
			function ProcessShouldTerminate : Boolean;
			procedure SetResultListViewDataToHistoryObj;
			procedure SetSelectedHistoryItem(const _idx : Integer);
			procedure UpdateHistObject;
			property AbortSearch : Boolean read FAbortSearch write FAbortSearch;
			property CurrentHistoryItemIndex : Integer read FCurrentHistoryItemIndex write FCurrentHistoryItemIndex;
			property Data : TRipGrepperData read GetData write FData;
			property ExeVersion : string read FExeVersion write FExeVersion;
			property FileNameType : TFileNameType read FFileNameType write FFileNameType;
			property HistItemObject : THistoryItemObject read GetHistItemObject write SetHistItemObject;
			property HistoryObjectList : TStringList read GetHistoryObjectList write FHistoryObjectList;
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
	GX_UsesManager;

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
	FHistoryObjectList.Free;
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
	ho : THistoryItemObject;
	Node : PVirtualNode;
	Data : PVSHistoryNodeData;
begin
	ho := GetHistoryObject(CurrentHistoryItemIndex);

	Node := GetNodeByIndex(VstHistory, CurrentHistoryItemIndex);
	Data := VstHistory.GetNodeData(Node);

	Assert(Data.SearchText = ho.GuiSearchTextParams.SearchText);

	VstHistory.DeleteNode(Node);
	VstHistory.Refresh;

	HistoryObjectList.Delete(CurrentHistoryItemIndex);
	FreeAndNil(ho);

	FCurrentHistoryItemIndex := IfThen(VstHistory.RootNodeCount = 0, -1, IfThen(FCurrentHistoryItemIndex = 0, 0, FCurrentHistoryItemIndex - 1));

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
	if not owp.IsEmpty then begin
		TOpenWith.Execute(owp);
	end;
end;

procedure TRipGrepperMiddleFrame.ActionOpenWithUpdate(Sender : TObject);
begin
	EnableActionIfResultSelected(ActionOpenWith);
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
	TDebugUtils.DebugMessage('TRipGrepperMiddleFrame.AddOrUpdateHistoryItem ActualSearchText: ' + Settings.ActualSearchText);
	CurrentHistoryItemIndex := HistoryObjectList.IndexOf(Settings.ActualSearchText);
	TDebugUtils.DebugMessage('TRipGrepperMiddleFrame.AddOrUpdateHistoryItem CurrentHistoryItemIndex ' + CurrentHistoryItemIndex.ToString);
	if CurrentHistoryItemIndex = -1 then begin
		CreateNewHistObject;
	end else begin
		UpdateRipGrepArgumentsInHistObj;
		UpdateHistObject;
		ClearHistoryObject();
		TDebugUtils.DebugMessage('TRipGrepperMiddleFrame.AddOrUpdateHistoryItem Update HistoryObject ' + Settings.ActualSearchText);
	end;
end;

procedure TRipGrepperMiddleFrame.AlignToolBars;
begin
	if Assigned(TopFrame) then begin
		TopFrame.AlignToolBars(PanelResult.Left, PanelHistory.Width, PanelResult.Width);
	end;
end;

procedure TRipGrepperMiddleFrame.ChangeDataHistItemObject(_ho : THistoryItemObject);
begin
	var
	beu := TBeginEndUpdater.New(VstHistory);
	Data.HistObject := _ho;
end;

procedure TRipGrepperMiddleFrame.ClearHistoryObject;
begin
	var
	beu := TBeginEndUpdater.New(VstHistory);
	HistItemObject.ClearMatches;
end;

procedure TRipGrepperMiddleFrame.ClearHistoryObjectList;
begin
	for var i := 0 to HistoryObjectList.Count - 1 do begin
		if Assigned(HistoryObjectList.Objects[i])
		{ } and (HistoryObjectList.Objects[i] is THistoryItemObject) then begin
			(HistoryObjectList.Objects[i] as THistoryItemObject).Free;
		end;
	end;
	HistoryObjectList.Clear;
end;

procedure TRipGrepperMiddleFrame.CopyToClipboardFileOfSelected;
begin
	Clipboard.AsText := TPath.GetFileName(GetResultSelectedFilePath);
end;

procedure TRipGrepperMiddleFrame.CopyToClipboardPathOfSelected;
begin
	Clipboard.AsText := TPath.GetFullPath(GetResultSelectedFilePath);
end;

procedure TRipGrepperMiddleFrame.CreateNewHistObject;
var
	Node : PVirtualNode;
	Data : PVSHistoryNodeData;
	hi : THistoryItemObject;
begin
	hi := THistoryItemObject.Create();
	HistItemObject := hi;
	ChangeDataHistItemObject(hi);
	TDebugUtils.DebugMessage('TRipGrepperMiddleFrame.AddOrUpdateHistoryItem Add HistoryObject ' + Settings.ActualSearchText);
	CurrentHistoryItemIndex := HistoryObjectList.AddObject(Settings.ActualSearchText, hi);
	Node := VstHistory.AddChild(nil);
	Data := VstHistory.GetNodeData(Node);
	Data^.SearchText := Settings.ActualSearchText;
	VstHistory.MultiLine[Node] := True;
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
	if Settings.RipGrepperViewSettings.ExpandNodes then begin
		VstResult.FullExpand();
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
	if Settings.RipGrepperViewSettings.ShowRelativePath then begin
		if IOTAUTils.IsStandAlone then begin
			actPath := Settings.ActualSearchPath;
			if (actPath.IsEmpty or (not Settings.SearchPathIsDir)) then begin
				Exit;
			end;
		end else begin
			actPath := TPath.GetDirectoryName(Settings.ExtensionSettings.CurrentSearchSettings.ActiveProject);
		end;
		Result := ExtractRelativePath(actPath + '\', _sFullPath);
	end;
end;

function TRipGrepperMiddleFrame.GetCounterText(Data : THistoryItemObject) : string;
begin
	Result := '';
	if not Assigned(Data) then begin
		Exit;
	end;
	if Data.ErrorCount > 0 then begin
		Result := Format('%s %d in %d(%d!)', [TREEVIEW_HISTORY_COUNTER_ERROR_PREFIX, Data.TotalMatchCount, Data.FileCount, Data.ErrorCount]);
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

function TRipGrepperMiddleFrame.GetHistItemObject : THistoryItemObject;
begin
	Result := FHistItemObject;
end;

function TRipGrepperMiddleFrame.GetHistoryObject(const _index : Integer) : THistoryItemObject;
begin
	Result := nil;
	if (_index > -1) and (_index < HistoryObjectList.Count { _lb.Items.Count } ) then begin
		Result := THistoryItemObject(HistoryObjectList.Objects[_index]);
		// Result := THistoryItemObject(_lb.Items.Objects[_index]);
	end;
end;

function TRipGrepperMiddleFrame.GetHistoryObjectList : TStringList;
begin
	if not Assigned(FHistoryObjectList) then begin
		FHistoryObjectList := TStringList.Create(TDuplicates.dupIgnore, False, False);
	end;
	Result := FHistoryObjectList;
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
	Result := TParallelParser.Create(FData, FHistItemObject);
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
	TDebugUtils.DebugMessage('TRipGrepperMiddleFrame.Init Begin');
	FCurrentHistoryItemIndex := -1;
	HistoryObjectList.Clear();
	if IOTAUTils.IsStandAlone then begin
		FExeVersion := TFileUtils.GetAppNameAndVersion(Application.ExeName);
	end else begin
		FExeVersion := TFileUtils.GetPackageNameAndVersion(HInstance);
		PanelHistory.BevelOuter := bvNone;
		PanelResult.BevelOuter := bvNone;
	end;
	Align := alClient;
	TDebugUtils.DebugMessage('TRipGrepperMiddleFrame.Init ' + FExeVersion);
	FFileNameType := ftAbsolute;
	VstResult.TreeOptions.StringOptions := VstResult.TreeOptions.StringOptions + [toShowStaticText];
	VstResult.TreeOptions.PaintOptions := VstResult.TreeOptions.PaintOptions + [toUseExplorerTheme];
	VstResult.NodeDataSize := SizeOf(TVSFileNodeData);

	VstHistory.TreeOptions.StringOptions := VstHistory.TreeOptions.StringOptions + [toShowStaticText];
	VstHistory.TreeOptions.PaintOptions := VstHistory.TreeOptions.PaintOptions + [toUseExplorerTheme];
	VstHistory.TreeOptions.MiscOptions := VstHistory.TreeOptions.MiscOptions + [TVTMiscOption.toVariablenodeHeight];
	VstHistory.NodeDataSize := SizeOf(TVSHistoryNodeData);
	TDebugUtils.DebugMessage('TRipGrepperMiddleFrame.InitForm Ended');
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
	TDebugUtils.DebugMessage(Format('TRipGrepperMiddleFrame.OnEOFProcess: End of processing rg.exe output in %s sec.', [GetElapsedTime(FswSearchStart)]));
end;

procedure TRipGrepperMiddleFrame.OnLastLine(const _iLineNr : Integer);
begin
	// ListViewResult.AdjustColumnWidths(MaxWidths);
	TDebugUtils.DebugMessage(Format('TRipGrepperMiddleFrame.OnLastLine: Last line (%d.) received in %s sec.', [_iLineNr, GetElapsedTime(FswSearchStart)]));

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
	if Assigned(FHistItemObject) then begin
		var
		beu := TBeginEndUpdater.New(VstHistory);
		FHistItemObject.FileCount := VstResult.RootNodeCount; // synced
		FHistItemObject.ErrorCount := Data.ErrorCount; // synced
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
						FHistItemObject.RipGrepResult := TProcessUtils.RunProcess(Settings.RipGrepParameters.RipGrepPath, args,
							{ } workDir,
							{ } self as INewLineEventHandler,
							{ } self as ITerminateEventProducer,
							{ } nil);
					end else begin
						FHistItemObject.RipGrepResult := TProcessUtils.RunProcess(Settings.RipGrepParameters.RipGrepPath, args,
							{ } workDir,
							{ } self as INewLineEventHandler,
							{ } self as ITerminateEventProducer,
							{ } self as IEOFProcessEventHandler);
					end;
				end;

			finally
				args.Free;
			end;
			FHistItemObject.ElapsedTimeText := GetElapsedTime(FswSearchStart);
			ParentFrame.SetStatusBarMessage(True);
			FswSearchStart.Stop;
			TDebugUtils.DebugMessage(Format('TRipGrepperMiddleFrame.RunRipGrep: rg.exe ended in %s sec.', [FHistItemObject.ElapsedTimeText]));
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

procedure TRipGrepperMiddleFrame.SetHistItemObject(const Value : THistoryItemObject);
begin
	FHistItemObject := Value;
end;

procedure TRipGrepperMiddleFrame.SetResultListViewDataToHistoryObj;
begin
	TThread.Synchronize(nil, // Refresh listview on history click
		procedure
		begin
			VstResult.Clear;
			ChangeDataHistItemObject(FHistItemObject);
			Data.DataToGrid;
			// ListViewResult.Items.Count := matchItems.Count + FHistItemObject.ErrorCount;
			{$IFDEF THREADSAFE_LIST}
			FHistItemObject.Matches.Unlock;
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

procedure TRipGrepperMiddleFrame.PopupMenuHistoryPopup(Sender : TObject);
begin
	SetSelectedHistoryItem(CurrentHistoryItemIndex);
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
	options := _rgp.RgExeOptions;
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
	if FHistItemObject.RipGrepArguments.Count = 0 then begin
		FHistItemObject.LoadFromSettings(Settings);
	end;
end;

procedure TRipGrepperMiddleFrame.UpdateHistObject;
begin
	FHistItemObject := GetHistoryObject(CurrentHistoryItemIndex);
	if Assigned(FHistItemObject) then begin
		FHistItemObject.UpdateParserType();
		FHistItemObject.CopyToSettings(Settings);
	end;
end;

procedure TRipGrepperMiddleFrame.UpdateHistObjectAndGui;
begin
	UpdateHistObject;
	TDebugUtils.DebugMessage('TRipGrepperMiddleFrame.UpdateHistObjectAndGui History selected: ' + CurrentHistoryItemIndex.ToString);
	TDebugUtils.DebugMessage('TRipGrepperMiddleFrame.UpdateHistObjectAndGui History Object: ' + HistItemObject.RipGrepArguments.DelimitedText);
	TDebugUtils.DebugMessage('TRipGrepperMiddleFrame.UpdateHistObjectAndGui History Matches: ' + HistItemObject.TotalMatchCount.ToString);
	TDebugUtils.DebugMessage('TRipGrepperMiddleFrame.UpdateHistObjectAndGui History Files: ' + HistItemObject.FileCount.ToString);
	TDebugUtils.DebugMessage('TRipGrepperMiddleFrame.UpdateHistObjectAndGui History Errors: ' + HistItemObject.ErrorCount.ToString);
	TDebugUtils.DebugMessage('TRipGrepperMiddleFrame.UpdateHistObjectAndGui History Gui: ' + HistItemObject.GuiSearchTextParams.SearchText + ' ' +
		HistItemObject.GuiSearchTextParams.ToString);
	SetResultListViewDataToHistoryObj();
	ExpandNodes;
	RefreshCountersInGUI;
	ParentFrame.SetStatusBarMessage(True);
end;

procedure TRipGrepperMiddleFrame.UpdateRipGrepArgumentsInHistObj;
begin
	FHistItemObject.RipGrepArguments.Clear;
	Settings.RebuildArguments();
	FHistItemObject.LoadFromSettings(Settings);
end;

procedure TRipGrepperMiddleFrame.VstHistoryDrawText(Sender : TBaseVirtualTree; TargetCanvas : TCanvas; Node : PVirtualNode; Column : TColumnIndex;
const Text : string; const CellRect : TRect; var DefaultDraw : Boolean);
var
	sSearchText, sStatistic : string;
	lineBegin : Integer;
	size : Winapi.Windows.TSize;
	rectTemp : TRect;
begin
	case Column of
		0 : begin
			DefaultDraw := False;
			lineBegin := Text.LastIndexOf(CRLF);
			sSearchText := Text.Substring(0, lineBegin);
			TargetCanvas.Font.Color := TREEVIEW_MATCH_ITEM_COLOR;
			TargetCanvas.Font.style := [fsBold];
			// TargetCanvas.TextOut(CellRect.Left, TREEVIEW_FONTSPACE, sSearchText);
			rectTemp := CellRect;
			Winapi.Windows.DrawText(TargetCanvas.Handle, pwidechar(sSearchText), length(sSearchText), rectTemp, DT_NOPREFIX or DT_WORDBREAK);

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
	// Data.hio.Free;
end;

procedure TRipGrepperMiddleFrame.VstHistoryGetText(Sender : TBaseVirtualTree; Node : PVirtualNode; Column : TColumnIndex; TextType : TVSTTextType;
var CellText : string);
var
	Data : PVSHistoryNodeData;
begin
	Data := VstHistory.GetNodeData(Node);

	if TextType = ttNormal then begin
		CellText := Data.SearchText + CRLF + GetCounterText(GetHistoryObject(Node.Index));
	end else begin // ttStatic not shown in Multiline cell
		CellText := GetCounterText(GetHistoryObject(Node.Index));
	end;
end;

procedure TRipGrepperMiddleFrame.VstHistoryMeasureItem(Sender : TBaseVirtualTree; TargetCanvas : TCanvas; Node : PVirtualNode; var NodeHeight : Integer);
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
	TDebugUtils.DebugMessage('History dbl clicked:' + HitInfo.HitNode.Index.ToString);
	VstHistoryNodeClick(Sender, HitInfo);
	ParentFrame.TopFrame.ActionShowSearchFormExecute(Sender);
end;

procedure TRipGrepperMiddleFrame.VstResultBeforeCellPaint(Sender : TBaseVirtualTree; TargetCanvas : TCanvas; Node : PVirtualNode; Column : TColumnIndex;
CellPaintMode : TVTCellPaintMode; CellRect : TRect; var ContentRect : TRect);
begin
	if Settings.RipGrepperViewSettings.AlternateRowColors and (Node.ChildCount = 0) then begin
		TargetCanvas.SetAlteringColors(Node.Index);
	end;

	TargetCanvas.FillRect(CellRect);
end;

procedure TRipGrepperMiddleFrame.VstResultCompareNodes(Sender : TBaseVirtualTree; Node1, Node2 : PVirtualNode; Column : TColumnIndex; var Result : Integer);
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
var
	owp : TOpenWithParams;
begin
	if IOTAUTils.IsStandAlone then begin
		ActionOpenWithExecute(Sender);
	end else begin
		owp := GetOpenWithParamsFromSelected();
		TDebugUtils.DebugMessage(Format('TRipGrepperMiddleFrame.VstResultDblClick: %s(%d, %d)', [owp.FileName, owp.Row, owp.Column]));
		IOTAUtils.GxOtaGoToFileLineColumn(owp.FileName, owp.Row, owp.Column, owp.Column - 1);
	end;
end;

procedure TRipGrepperMiddleFrame.VstResultDrawText(Sender : TBaseVirtualTree; TargetCanvas : TCanvas; Node : PVirtualNode; Column : TColumnIndex;
const Text : string; const CellRect : TRect; var DefaultDraw : Boolean);
var
	fc : TColor;
	fs, pos : Integer;
	style : TFontStyles;
	Data : PVSFileNodeData;
	s, ss0, ss1, ss2 : string;
	spaces, tabs, matchBegin : Integer;
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
			case FHistItemObject.ParserType of
				ptRipGrepSearch, ptRipGrepPrettySearch : begin
					DefaultDraw := False;
					// First, store the default font size and color number
					fc := TargetCanvas.Font.Color;
					fs := TargetCanvas.Font.Size;
					style := TargetCanvas.Font.style;

					Data := VstResult.GetNodeData(Node);
					s := Data.GetText(not Settings.RipGrepperViewSettings.IndentLines, spaces, tabs);

					matchBegin := Data.MatchData.Col - 1 - (spaces + tabs);

					ss0 := s.Substring(0, matchBegin).Replace(#9, TREEVIEW_INDENT_TAB_AS_SPACES, [rfReplaceAll]);
					pos := TargetCanvas.TextWidth(ss0);

					TargetCanvas.TextOut(CellRect.Left, TREEVIEW_FONTSPACE, ss0);

					ss1 := s.Substring(matchBegin, Data.MatchData.MatchLength);
					ss2 := s.Substring(matchBegin + Data.MatchData.MatchLength);

					TargetCanvas.Font.Color := TREEVIEW_MATCH_TEXT_COLOR;
					TargetCanvas.Font.style := [fsBold, fsUnderline];
					TargetCanvas.TextOut(CellRect.Left + pos, TREEVIEW_FONTSPACE, ss1);

					pos := pos + TargetCanvas.TextWidth(ss1);
					TargetCanvas.Font.Color := fc;
					TargetCanvas.Font.Size := fs;
					TargetCanvas.Font.style := style;
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

procedure TRipGrepperMiddleFrame.VstResultGetImageIndex(Sender : TBaseVirtualTree; Node : PVirtualNode; Kind : TVTImageKind; Column : TColumnIndex;
var Ghosted : Boolean; var ImageIndex : TImageIndex);
var
	NodeData : PVSFileNodeData;
begin
	if not Settings.RipGrepperViewSettings.ShowFileIcon then
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

procedure TRipGrepperMiddleFrame.VstResultGetText(Sender : TBaseVirtualTree; Node : PVirtualNode; Column : TColumnIndex; TextType : TVSTTextType;
var CellText : string);
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
				CellText := NodeData.GetText(Settings.RipGrepperViewSettings.IndentLines, dummy1, dummy2);
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

procedure TRipGrepperMiddleFrame.VstResultPaintText(Sender : TBaseVirtualTree; const TargetCanvas : TCanvas; Node : PVirtualNode; Column : TColumnIndex;
TextType : TVSTTextType);
begin
	if TextType = ttNormal then begin
		case Column of
			0 : begin
				TargetCanvas.Font.style := [fsBold];
				TargetCanvas.Font.Color := TREEVIEW_MATCH_ITEM_COLOR;
			end;
		end;
	end else begin // ttStatic
		TargetCanvas.Font.Color := TREEVIEW_STAT_COLOR; // Not shown on MultiLine
	end;
end;

end.
