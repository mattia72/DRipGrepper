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
	RipGrepper.Common.NodeData;

type
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
			procedure AlignToolBars;
			procedure ClearFilter(const _bForce : Boolean = False);
			procedure CopyToClipboardFileOfSelected;
			procedure CopyToClipboardPathOfSelected;
			function CreateNewHistObject : IHistoryItemObject;
			procedure FilterNodes(const _sFilterPattern : string);
			function GetCounterText(Data : IHistoryItemObject) : string;
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
	TDebugUtils.DebugMessage('TRipGrepperMiddleFrame.Create ' + AOwner.Name);
	FIconImgList := TIconImageList.Create(Handle, ImageListListView);
	MainFrame := self;
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
{$IFDEF STANDALONE}
var bStandalone := True;
{$ELSE}
var bStandalone := False;
{$ENDIF}
	miAddToUSESList.Visible := bStandalone;
	ActionAddUsingImplementation.Visible := bStandalone;
	EnableActionIfResultSelected(ActionAddUsingImplementation);
end;

procedure TRipGrepperMiddleFrame.ActionAddUsingInterfaceExecute(Sender : TObject);
begin
	AddAsUsing(False);
end;

procedure TRipGrepperMiddleFrame.ActionAddUsingInterfaceUpdate(Sender : TObject);
begin
	{$IFNDEF STANDALONE}
	ActionAddUsingInterface.Visible := True;
	{$ELSE}
	ActionAddUsingInterface.Visible := False;
	{$ENDIF}
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
	try
		var fn :string := TPath.GetFileNameWithoutExtension(GetResultSelectedFilePath);

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

procedure TRipGrepperMiddleFrame.AlignToolBars;
begin
	if Assigned(TopFrame) then begin
		TopFrame.AlignToolBars(PanelResult.Left, PanelHistory.Width, PanelResult.Width);
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
	ParentFrame.SetStatusBarStatistic('Searching...');
	FAbortSearch := False;
	UpdateArgumentsAndSettings;
	// hist object parser type should set before painting begins...
	UpdateHistObjectAndCopyToSettings;
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
	if Data.ErrorCount > 0 then begin
		Result := Format('%s %d in %d(%d!)', [TREEVIEW_HISTORY_COUNTER_ERROR_PREFIX, Data.TotalMatchCount, Data.FileCount,
			Data.ErrorCount]);
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
	TDebugUtils.DebugMessage(Format('TRipGrepperMiddleFrame.OnLastLine: Last line (%d.) received in %s sec.',
		[_iLineNr, GetElapsedTime(FswSearchStart)]));

	TThread.Synchronize(nil,
		procedure
		begin
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
		beu := TBeginEndUpdater.New(MiddleLeftFrame1.VstHistory);
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
			MiddleLeftFrame1.VstHistory.Refresh;
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
    {$IFNDEF STANDALONE}
	owp := GetOpenWithParamsFromSelected();
	TDebugUtils.DebugMessage(Format('TRipGrepperMiddleFrame.OpenSelectedInIde: %s(%d:%d)', [owp.FileName, owp.Row, owp.Column]));
	IOTAUtils.GxOtaGoToFileLineColumn(owp.FileName, owp.Row, owp.Column, owp.Column - 1);
    {$ENDIF}
end;

procedure TRipGrepperMiddleFrame.PrepareAndDoSearch;
begin
	MiddleLeftFrame1.PrepareAndDoSearch();
	Data.ClearMatchFiles();
	InitSearch();
	DoSearch();
	MiddleLeftFrame1.ChangeHistoryNodeText;
end;

procedure TRipGrepperMiddleFrame.RefreshSearch;
begin
	UpdateHistObjectAndCopyToSettings();
	MiddleLeftFrame1.ClearMatchesInHistoryObject();
	InitSearch();
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
	TopFrame.SetFilter(False);
	RefreshCountersInGUI;
	ParentFrame.SetStatusBarMessage(True);
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
	UpdateHistObjectAndCopyToSettings;
	TDebugUtils.DebugMessage('TRipGrepperMiddleFrame.UpdateHistObjectAndGui History Object: ' +
		HistItemObject.RipGrepArguments.DelimitedText);
	TDebugUtils.DebugMessage('TRipGrepperMiddleFrame.UpdateHistObjectAndGui History Matches: ' + HistItemObject.TotalMatchCount.ToString);
	TDebugUtils.DebugMessage('TRipGrepperMiddleFrame.UpdateHistObjectAndGui History Files: ' + HistItemObject.FileCount.ToString);
	TDebugUtils.DebugMessage('TRipGrepperMiddleFrame.UpdateHistObjectAndGui History Errors: ' + HistItemObject.ErrorCount.ToString);
	TDebugUtils.DebugMessage('TRipGrepperMiddleFrame.UpdateHistObjectAndGui History Gui: ' + HistItemObject.GuiSearchTextParams.SearchText +
		' ' + HistItemObject.GuiSearchTextParams.ToString);
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
				TargetCanvas.Font.Color := TREEVIEW_ERROR_COLOR;
				TargetCanvas.TextOut(CellRect.Left, TREEVIEW_FONTSPACE, Text);
			end;
		end;
		COL_MATCH_TEXT : begin
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
					if IsGuiReplaceMode and (not Settings.LastSearchText.IsEmpty) then begin
						ss1_repl := TRegEx.Replace(ss1, Settings.LastSearchText, Settings.RipGrepParameters.ReplaceText, [roIgnoreCase]);
					end;
					ss2 := s.Substring(matchBegin + Data.MatchData.MatchLength);

					if IsGuiReplaceMode or IsRgReplaceMode then begin
						TItemDrawer.SetTextColorReplacedText(TargetCanvas);
					end else begin
						TItemDrawer.SetTextColorMatch(TargetCanvas);
					end;
					if not IsRgReplaceMode then begin
						TargetCanvas.TextOut(CellRect.Left + pos, TREEVIEW_FONTSPACE, ss1);
						pos := pos + TargetCanvas.TextWidth(ss1);
					end;
					if IsGuiReplaceMode or IsRgReplaceMode then begin
						TItemDrawer.SetTextColorReplaceText(TargetCanvas);
						TargetCanvas.TextOut(CellRect.Left + pos, TREEVIEW_FONTSPACE, ss1_repl);

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
				COL_FILE :
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
				TargetCanvas.Font.style := [fsBold];
				TargetCanvas.Font.Color := TREEVIEW_NORMAL_TEXT_COLOR;
			end;
		end;
	end else begin // ttStatic
		TargetCanvas.Font.Color := TREEVIEW_STAT_COLOR; // Not shown on MultiLine
	end;
end;

end.
