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
	RipGrepper.OpenWith.SimpleTypes,
	RipGrepper.Common.Settings,
	RipGrepper.Data.HistoryItemObject,
	RipGrepper.Data.Matches,
	RipGrepper.Common.Sorter,
	RipGrepper.Common.Interfaces,
	System.Diagnostics,
	RipGrepper.Common.Types,
	System.Threading,
	VirtualTrees,
	RipGrepper.Helper.UI;

type
	TRipGrepperMainFrame = class(TFrame, INewLineEventHandler, ITerminateEventProducer, IEOFProcessEventHandler)
		ActionList : TActionList;
		ActionSearch : TAction;
		ActionCancel : TAction;
		ActionConfig : TAction;
		ActionSwitchView : TAction;
		ActionSortByFile : TAction;
		ActionShowRelativePath : TAction;
		ActionCmdLineCopy : TAction;
		ActionSortByRow : TAction;
		ActionCopyFileName : TAction;
		ActionCopyPathToClipboard : TAction;
		ActionShowSearchForm : TAction;
		ActionShowFileIcons : TAction;
		ActionAlternateRowColors : TAction;
		ActionAbortSearch : TAction;
		ActionRefreshSearch : TAction;
		ActionIndentLine : TAction;
		ActionStatusBar : TAction;
		ActionOpenWith : TAction;
		PopupMenu1 : TPopupMenu;
		Openwith1 : TMenuItem;
		N1 : TMenuItem;
		CopyFileNameToClipboard : TMenuItem;
		CopyPathToClipboard : TMenuItem;
		ImageListListView : TImageList;
		panelMain : TPanel;
		SplitView1 : TSplitView;
		Splitter1 : TSplitter;
		PanelHistory : TPanel;
		ListBoxSearchHistory : TListBox;
		PanelResult : TPanel;
		VstResult : TVirtualStringTree;
		procedure ActionCopyFileNameExecute(Sender : TObject);
		procedure ActionCopyPathToClipboardExecute(Sender : TObject);
		procedure ActionOpenWithExecute(Sender : TObject);
		procedure ActionOpenWithUpdate(Sender : TObject);
		procedure FrameResize(Sender : TObject);
		procedure ListBoxSearchHistoryClick(Sender : TObject);
		procedure ListBoxSearchHistoryData(Control : TWinControl; Index : Integer; var Data : string);
		procedure ListBoxSearchHistoryDblClick(Sender : TObject);
		procedure ListBoxSearchHistoryDrawItem(Control : TWinControl; Index : Integer; Rect : TRect; State : TOwnerDrawState);
		procedure ListViewResultDblClick(Sender : TObject);
		procedure VstResultBeforeCellPaint(Sender : TBaseVirtualTree; TargetCanvas : TCanvas; Node : PVirtualNode; Column : TColumnIndex;
			CellPaintMode : TVTCellPaintMode; CellRect : TRect; var ContentRect : TRect);
		procedure VstResultCompareNodes(Sender : TBaseVirtualTree; Node1, Node2 : PVirtualNode; Column : TColumnIndex;
			var Result : Integer);
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
			FHistObject : THistoryItemObject;
			FHistoryObjectList : TStringList;
			FIsParsingRunning : Boolean;
			FMaxWidths : TArray<integer>;
			FMeassureFirstDrawEvent : Boolean;
			FRipGrepTask : ITask;
			FSettings : TRipGrepperSettings;
			FswSearchStart : TStopwatch;
			FIconImgList : TIconImageList;
			function GetAbsOrRelativePath(const _sFullPath : string) : string;
			function GetCounterText(data : THistoryItemObject) : string;
			function GetData : TRipGrepperData;
			function GetHistObject : THistoryItemObject;
			function GetHistoryObject(const _index : Integer) : THistoryItemObject;
			function GetHistoryObjectList : TStringList;
			function GetSettings : TRipGrepperSettings;
			procedure LoadBeforeSearchSettings;
			procedure OnLastLine(const _iLineNr : integer);
			procedure RefreshCounters;
			procedure RefreshCountersInGUI;
			procedure RunRipGrep;
			procedure SetColumnWidths;
			procedure SetHistObject(const Value : THistoryItemObject);
			procedure UpdateArgumentsAndSettings;
			procedure UpdateRipGrepArgumentsInHistObj;
			property Settings : TRipGrepperSettings read GetSettings write FSettings;

			{ Private-Deklarationen }
		public
			constructor Create(AOwner : TComponent); override;
			destructor Destroy; override;
			procedure AddOrUpdateHistoryItem;
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
			procedure OnNewOutputLine(const _iLineNr : integer; const _sLine : string; _bIsLast : Boolean = False);
			// ITerminateEventProducer
			function ProcessShouldTerminate : boolean;
			procedure SetResultListViewDataToHistoryObj;
			procedure UpdateHistObject;
			property AbortSearch : Boolean read FAbortSearch write FAbortSearch;
			property CurrentHistoryItemIndex : Integer read FCurrentHistoryItemIndex write FCurrentHistoryItemIndex;
			property Data : TRipGrepperData read GetData write FData;
			property ExeVersion : string read FExeVersion write FExeVersion;
			property FileNameType : TFileNameType read FFileNameType write FFileNameType;
			property HistObject : THistoryItemObject read GetHistObject write SetHistObject;
			property HistoryObjectList : TStringList read GetHistoryObjectList write FHistoryObjectList;
			property MaxWidths : TArray<integer> read FMaxWidths write FMaxWidths;
			property RipGrepTask : ITask read FRipGrepTask write FRipGrepTask;
			{ Public-Deklarationen }
	end;

var
	MainFrame : TRipGrepperMainFrame;

implementation

uses
	RipGrepper.UI.ParentFrame,
	RipGrepper.OpenWith,
	System.StrUtils,
	RipGrepper.Tools.DebugTools,

	System.IOUtils,
	Vcl.Clipbrd,
	Winapi.CommCtrl,
	RipGrepper.Helper.ListBox,
	RipGrepper.Tools.FileUtils,
	RipGrepper.Helper.Types,
	RipGrepper.Parsers.VimGrepMatchLine,
	RipGrepper.Common.ParsedObject,
	RipGrepper.Tools.ProcessUtils,
	System.Math,
	RipGrepper.UI.MainForm,
	GX_OtaUtils,
	RipGrepper.UI.BottomFrame,
	VirtualTrees.Types;

{$R *.dfm}

constructor TRipGrepperMainFrame.Create(AOwner : TComponent);
begin
	inherited;
	TDebugUtils.DebugMessage('TRipGrepperMainFrame.Create ' + AOwner.Name);
	FIconImgList := TIconImageList.Create(ImageListListView);
	MainFrame := self;
end;

destructor TRipGrepperMainFrame.Destroy;
begin
	TDebugUtils.DebugMessage('TRipGrepperMainFrame.Destroy');

	TListBoxHelper.FreeItemObjects(ListBoxSearchHistory);

	// if Assigned(ListViewResult.Items) then begin
	// ListViewResult.Items.Count := 0;
	// end;

	for var i := 0 to HistoryObjectList.Count - 1 do begin
		if Assigned(HistoryObjectList.Objects[i])
		{ } and (HistoryObjectList.Objects[i] is THistoryItemObject) then begin
			(HistoryObjectList.Objects[i] as THistoryItemObject).Free;
		end;
	end;
	FHistoryObjectList.Free;
	FData.Free;
	FIconImgList.Free;

	inherited;
end;

procedure TRipGrepperMainFrame.ActionCopyFileNameExecute(Sender : TObject);
begin
	CopyToClipboardFileOfSelected();
end;

procedure TRipGrepperMainFrame.ActionCopyPathToClipboardExecute(Sender : TObject);
begin
	CopyToClipboardPathOfSelected();
end;

procedure TRipGrepperMainFrame.ActionOpenWithExecute(Sender : TObject);
var
	owp : TOpenWithParams;
begin
	owp := GetOpenWithParamsFromSelected();
	if not owp.IsEmpty then begin
		TOpenWith.Execute(owp);
	end;

end;

procedure TRipGrepperMainFrame.ActionOpenWithUpdate(Sender : TObject);
begin
	ActionOpenWith.Enabled := VstResult.SelectedCount = 1;
end;

procedure TRipGrepperMainFrame.AddOrUpdateHistoryItem;
var
	hi : THistoryItemObject;
begin
	TDebugUtils.DebugMessage('ActualSearchText: ' + Settings.ActualSearchText);
	CurrentHistoryItemIndex := HistoryObjectList.IndexOf(Settings.ActualSearchText);
	TDebugUtils.DebugMessage('CurrentHistoryItemIndex ' + CurrentHistoryItemIndex.ToString);
	if CurrentHistoryItemIndex = -1 then begin
		hi := THistoryItemObject.Create();
		HistObject := hi;
		ChangeDataHistItemObject(hi);
		TDebugUtils.DebugMessage('Add HistoryObject ' + Settings.ActualSearchText);
		CurrentHistoryItemIndex := HistoryObjectList.AddObject(Settings.ActualSearchText, hi);
	end else begin
		UpdateRipGrepArgumentsInHistObj;
		UpdateHistObject;
		ClearHistoryObject();
		TDebugUtils.DebugMessage('Update HistoryObject ' + Settings.ActualSearchText);
	end;
	ListBoxSearchHistory.Count := HistoryObjectList.Count;
end;

procedure TRipGrepperMainFrame.ChangeDataHistItemObject(_ho : THistoryItemObject);
begin
	var
	beu := TBeginEndUpdater.New(ListBoxSearchHistory);
	Data.HistObject := _ho;
end;

procedure TRipGrepperMainFrame.ClearHistoryObject;
begin
	var
	beu := TBeginEndUpdater.New(ListBoxSearchHistory);
	HistObject.ClearMatches;
end;

procedure TRipGrepperMainFrame.CopyToClipboardFileOfSelected;
var
	node : PVirtualNode;
begin
	node := VstResult.GetFirstSelected();
	if not Assigned(node) then
		Exit;

	Clipboard.AsText := TPath.GetFileName(GetFilePathFromNode(node));
end;

procedure TRipGrepperMainFrame.CopyToClipboardPathOfSelected;
var
	node : PVirtualNode;
begin
	node := VstResult.GetFirstSelected();
	if not Assigned(node) then
		Exit;

	Clipboard.AsText := TPath.GetFullPath(GetFilePathFromNode(node));
end;

procedure TRipGrepperMainFrame.DoSearch;
begin
	ParentFrame.SetStatusBarStatistic('Searching...');
	FAbortSearch := False;
	UpdateArgumentsAndSettings;
	RunRipGrep();
end;

procedure TRipGrepperMainFrame.FrameResize(Sender : TObject);
begin
	// TDebugUtils.DebugMessage('TRipGrepperMainFrame.FrameResize');

	SplitView1.Width := panelMain.Width;
	if Assigned(ParentFrame) then begin
		BottomFrame.StatusBar1.Panels[0].Width := PanelHistory.Width;
	end;
	SetColumnWidths;
end;

function TRipGrepperMainFrame.GetAbsOrRelativePath(const _sFullPath : string) : string;
begin
	Result := _sFullPath;
	var
	actPath := Settings.ActualSearchPath;
	if Settings.RipGrepperViewSettings.ShowRelativePath and ((not actPath.IsEmpty) and Settings.SearchPathIsDir) then begin
		Result := Result.Replace(actPath, '.', [rfIgnoreCase]);
	end;
end;

function TRipGrepperMainFrame.GetCounterText(data : THistoryItemObject) : string;
begin
	if data.ErrorCount > 0 then begin
		Result := Format('%d(%d!) in %d', [data.TotalMatchCount, data.ErrorCount, data.FileCount]);
	end else begin
		Result := Format('%d in %d', [data.TotalMatchCount, data.FileCount]);
	end;
end;

function TRipGrepperMainFrame.GetData : TRipGrepperData;
begin
	if not Assigned(FData) then begin
		FData := TRipGrepperData.Create(VstResult);
	end;
	Result := FData;
end;

function TRipGrepperMainFrame.GetFilePathFromNode(_node : PVirtualNode) : string;
var
	data : PVSFileNodeData;
begin
	if _node.ChildCount = 0 then begin
		data := VstResult.GetNodeData(_node.Parent);
	end else begin
		data := VstResult.GetNodeData(_node);
	end;
	Result := data.FilePath;
end;

function TRipGrepperMainFrame.GetHistObject : THistoryItemObject;
begin
	Result := FHistObject;
end;

function TRipGrepperMainFrame.GetHistoryObject(const _index : Integer) : THistoryItemObject;
begin
	Result := nil;
	if (_index > -1) and (_index < HistoryObjectList.Count { _lb.Items.Count } ) then begin
		Result := THistoryItemObject(HistoryObjectList.Objects[_index]);
		// Result := THistoryItemObject(_lb.Items.Objects[_index]);
	end;
end;

function TRipGrepperMainFrame.GetHistoryObjectList : TStringList;
begin
	if not Assigned(FHistoryObjectList) then begin
		FHistoryObjectList := TStringList.Create(TDuplicates.dupIgnore, False, False);
	end;
	Result := FHistoryObjectList;
end;

function TRipGrepperMainFrame.GetOpenWithParamsFromSelected : TOpenWithParams;
var
	node : PVirtualNode;
	data : PVSFileNodeData;
	dataParent : PVSFileNodeData;
begin
	node := VstResult.GetFirstSelected();
	if not Assigned(node) then
		Exit;

	data := VstResult.GetNodeData(node);
	if node.ChildCount > 0 then begin
		Result.DirPath := IfThen(Settings.SearchPathIsDir, Settings.ActualSearchPath, ExtractFileDir(data.FilePath));
		Result.FileName := data.FilePath;
		Result.Row := 0;
		Result.Column := 0;
		Result.IsEmpty := False;
	end else begin
		dataParent := VstResult.GetNodeData(node.Parent);
		Result.DirPath := IfThen(Settings.SearchPathIsDir, Settings.ActualSearchPath, ExtractFileDir(dataParent.FilePath));
		Result.FileName := dataParent.FilePath;
		Result.Row := data.MatchData.Row;
		Result.Column := data.MatchData.Col;
		Result.IsEmpty := False;
	end;

end;

function TRipGrepperMainFrame.GetRowColText(_i : Integer; _type : TVSTTextType) : string;
begin
	Result := '';
	if (_type = ttNormal) and (_i > 0) then begin
		Result := _i.ToString;
	end;
end;

function TRipGrepperMainFrame.GetSettings : TRipGrepperSettings;
begin
	if not Assigned(FSettings) then begin
		FSettings := GSettings;
	end;
	Result := FSettings;
end;

procedure TRipGrepperMainFrame.Init;
begin
	TDebugUtils.DebugMessage('TRipGrepperMainFrame.Init Begin');

	HistoryObjectList.Clear();
	if IsStandAlone then begin
		FExeVersion := TFileUtils.GetAppNameAndVersion(Application.ExeName);
	end else begin
		FExeVersion := TFileUtils.GetPackageNameAndVersion(HInstance);
	end;
	TDebugUtils.DebugMessage('TRipGrepperMainFrame.Init ' + FExeVersion);

	FFileNameType := ftAbsolute;

	// ListViewResult.InitMaxWidths(FMaxWidths);

	VstResult.TreeOptions.StringOptions := VstResult.TreeOptions.StringOptions + [toShowStaticText];
	VstResult.TreeOptions.PaintOptions := VstResult.TreeOptions.PaintOptions + [toUseExplorerTheme];

	VstResult.NodeDataSize := SizeOf(TVSFileNodeData);

	TDebugUtils.DebugMessage('TRipGrepperMainFrame.InitForm Ended');
end;

procedure TRipGrepperMainFrame.InitSearch;
begin
	VstResult.Clear;
	Data.ClearMatchFiles;
	// ClearData;
	FswSearchStart := TStopwatch.Create();
	FMeassureFirstDrawEvent := True;
	ParentFrame.InitStatusBar;
	// ListViewResult.Repaint();
	LoadBeforeSearchSettings();
end;

function TRipGrepperMainFrame.IsSearchRunning : Boolean;
begin
	Result := FIsParsingRunning or (Assigned(FRipGrepTask) and (FRipGrepTask.Status = TTaskStatus.Running));
end;

procedure TRipGrepperMainFrame.ListBoxSearchHistoryClick(Sender : TObject);
begin
	CurrentHistoryItemIndex := ListBoxSearchHistory.ItemIndex;
	UpdateHistObject;
	TDebugUtils.DebugMessage('History clicked: ' + CurrentHistoryItemIndex.ToString);
	TDebugUtils.DebugMessage('History Object: ' + HistObject.RipGrepArguments.DelimitedText);
	TDebugUtils.DebugMessage('History Matches: ' + HistObject.TotalMatchCount.ToString);
	TDebugUtils.DebugMessage('History Files: ' + HistObject.FileCount.ToString);
	TDebugUtils.DebugMessage('History Errors: ' + HistObject.ErrorCount.ToString);
	SetResultListViewDataToHistoryObj();
	RefreshCountersInGUI;
	ParentFrame.SetStatusBarMessage(True);
end;

procedure TRipGrepperMainFrame.ListBoxSearchHistoryData(Control : TWinControl; Index : Integer; var Data : string);
begin
	Data := HistoryObjectList[index];
end;

procedure TRipGrepperMainFrame.ListBoxSearchHistoryDblClick(Sender : TObject);
begin
	CurrentHistoryItemIndex := ListBoxSearchHistory.ItemIndex;
	UpdateHistObject;
	TDebugUtils.DebugMessage('History dbl clicked:' + CurrentHistoryItemIndex.ToString);
	SetResultListViewDataToHistoryObj();
end;

procedure TRipGrepperMainFrame.ListBoxSearchHistoryDrawItem(Control : TWinControl; index : Integer; Rect : TRect; State : TOwnerDrawState);
var
	c2ndRowTop : Integer;
	cMatchesLeft : Integer;
	cnv : TCanvas;
	data : THistoryItemObject;
	lb : TListBox;
	r2ndRow : TRect;
begin
	lb := (Control as TListBox);
	data := GetHistoryObject(index);
	if not Assigned(data) then
		Exit;

	cnv := lb.Canvas;
	cnv.SetSelectedColors(State);

	c2ndRowTop := lb.ItemHeight div 2; // cnv.TextHeight(ALL_ALPHANUMERIC_CHARS);
	cMatchesLeft := 20;
	r2ndRow := Rect;
	r2ndRow.Offset(0, c2ndRowTop);

	cnv.FillRect(Rect);
	cnv.TextOut(Rect.Left + 1, Rect.Top + 1, HistoryObjectList[index]);
	cnv.TextOut(Rect.Left + 1, Rect.Top + c2ndRowTop, '*');

	if (not lb.Items.Updating) then begin
		cnv.TextOut(Rect.Left + cMatchesLeft, Rect.Top + c2ndRowTop, GetCounterText(data));
	end;
end;

procedure TRipGrepperMainFrame.ListViewResultDblClick(Sender : TObject);
var
	owp : TOpenWithParams;
begin
	if IsStandalone then begin
		ActionOpenWithExecute(Sender);
	end else begin
		owp := GetOpenWithParamsFromSelected();
		TDebugUtils.DebugMessage(Format('%s(%d, %d)', [owp.FileName, owp.Row, owp.Column]));
		GxOtaGoToFileLineColumn(owp.FileName, owp.Row, owp.Column, owp.Column - 1);
	end;
end;

procedure TRipGrepperMainFrame.LoadBeforeSearchSettings;
begin
	//
end;

procedure TRipGrepperMainFrame.OnEOFProcess;
begin
	TDebugUtils.DebugMessage(Format('End of processing rg.exe output in %s sec.', [GetElapsedTime(FswSearchStart)]));
end;

procedure TRipGrepperMainFrame.OnLastLine(const _iLineNr : integer);
begin
	// ListViewResult.AdjustColumnWidths(MaxWidths);
	TThread.Synchronize(nil,
		procedure
		begin
			SetColumnWidths;
			BottomFrame.ActivityIndicator1.Animate := False;
			FIsParsingRunning := False;
		end);
end;

procedure TRipGrepperMainFrame.OnNewOutputLine(const _iLineNr : integer; const _sLine : string; _bIsLast : Boolean = False);
begin
	if (FAbortSearch) then begin
		OnLastLine(_iLineNr);
		Exit;
	end;

	if (_iLineNr >= RG_PROCESSING_LINE_COUNT_LIMIT) then begin
		OnLastLine(_iLineNr);
		if _iLineNr = RG_PROCESSING_LINE_COUNT_LIMIT then begin
			MessageDlg(Format('Too many results.' + CRLF + 'The first %d will be shown. Try to be more specific.',
				[_iLineNr, RG_PROCESSING_LINE_COUNT_LIMIT]), TMsgDlgType.mtWarning, [mbOk], 0);
		end else begin
			Exit;
		end;
	end;

	try
		TThread.Queue(nil, // faster
		// TThread.Synchronize(nil, // slower
			procedure
			begin
				if _bIsLast then begin
					OnLastLine(_iLineNr);
					TDebugUtils.DebugMessage(Format('Last line (%d.) received in %s sec.', [_iLineNr, GetElapsedTime(FswSearchStart)]));
					TDebugUtils.DebugMessage(Format('Before parse last line: %d in %d err: %d', [FHistObject.TotalMatchCount,
						FHistObject.FileCount, FHistObject.ErrorCount]));
				end;

				if (not _sLine.IsEmpty) then begin
					var
					parser := TVimGrepMatchLineParser.Create();
					try
						var
						parsedObj := parser.ParseLine(_iLineNr, _sLine, _bIsLast);
						var
						o := TParsedObjectRow.Create(parsedObj);
						Data.Add(o);
					finally
						parser.Free;
					end;
				end;
				// First 100 than every 100 and the last
				if (_iLineNr < DRAW_RESULT_UNTIL_FIRST_LINE_COUNT) or ((_iLineNr mod DRAW_RESULT_ON_EVERY_LINE_COUNT) = 0) or _bIsLast then
				begin
					RefreshCounters;
				end;
			end);
	except
		on e : EOutOfMemory do begin
			TDebugUtils.DebugMessage(Format('Exception %s ' + CRLF + 'on line %d', [E.Message, _iLineNr]));
			MessageDlg(Format(E.Message + CRLF + 'Too many results. Try to be more specific', [_iLineNr]), TMsgDlgType.mtError, [mbOk], 0);
		end;
	end;

end;

function TRipGrepperMainFrame.ProcessShouldTerminate : boolean;
begin
	Result := Assigned(FRipGrepTask) and (FRipGrepTask.Status = TTaskStatus.Canceled);
end;

procedure TRipGrepperMainFrame.RefreshCounters;
begin
	if Assigned(FHistObject) then begin
		var
		beu := TBeginEndUpdater.New(ListBoxSearchHistory);
		FHistObject.FileCount := VstResult.TotalCount; // synced
		FHistObject.ErrorCount := Data.ErrorCount; // synced
	end;
	RefreshCountersInGUI;
end;

procedure TRipGrepperMainFrame.RefreshCountersInGUI;
begin
	TThread.Synchronize(nil,
		procedure
		begin
			ListBoxSearchHistory.Refresh;
			ParentFrame.SetStatusBarStatistic(GetCounterText(HistObject));
		end);
end;

procedure TRipGrepperMainFrame.RunRipGrep;
var
	workDir : string;
	args : TStrings;
begin
	FRipGrepTask := TTask.Create(
		procedure()
		begin
			workDir := TDirectory.GetCurrentDirectory();
			TDebugUtils.DebugMessage('run: ' + Settings.RipGrepParameters.RipGrepPath + ' '
				{ } + Settings.RipGrepParameters.RipGrepArguments.DelimitedText);
			FswSearchStart := TStopwatch.StartNew;
			args := TStringList.Create;
			try
				args.AddStrings(Settings.RipGrepParameters.RipGrepArguments.GetValues);
				FHistObject.RipGrepResult := TProcessUtils.RunProcess(Settings.RipGrepParameters.RipGrepPath, args,
					{ } workDir,
					{ } self as INewLineEventHandler,
					{ } self as ITerminateEventProducer,
					{ } self as IEOFProcessEventHandler);

			finally
				args.Free;
			end;
			FHistObject.ElapsedTimeText := GetElapsedTime(FswSearchStart);
			ParentFrame.SetStatusBarMessage(True);
			FswSearchStart.Stop;
			TDebugUtils.DebugMessage(Format('rg.exe ended in %s sec.', [FHistObject.ElapsedTimeText]));
		end);
	FRipGrepTask.Start;
	BottomFrame.SetRunningStatus();
end;

procedure TRipGrepperMainFrame.SetColumnWidths;
begin
	// TListView_Resize(ListViewResult);
	// ListView_SetColumnWidth(ListViewResult.Handle, 0, ColumnTextWidth);
	// ListView_SetColumnWidth(ListViewResult.Handle, 1, ColumnHeaderWidth);
	// ListView_SetColumnWidth(ListViewResult.Handle, 2, ColumnHeaderWidth);
	// ListView_SetColumnWidth(ListViewResult.Handle, 3, ColumnTextWidth);
end;

procedure TRipGrepperMainFrame.SetHistObject(const Value : THistoryItemObject);
begin
	FHistObject := Value;
end;

procedure TRipGrepperMainFrame.SetResultListViewDataToHistoryObj;
begin
	TThread.Synchronize(nil, // Refresh listview on history click
		procedure
		begin
			VstResult.Clear;
			ChangeDataHistItemObject(FHistObject);
            Data.DataToGrid;
			// ListViewResult.Items.Count := matchItems.Count + FHistObject.ErrorCount;
			{$IFDEF THREADSAFE_LIST}
			FHistObject.Matches.Unlock;
			{$ENDIF}
		end);
end;

procedure TRipGrepperMainFrame.UpdateArgumentsAndSettings;
begin
	if FHistObject.RipGrepArguments.Count = 0 then begin
		FHistObject.CopyRipGrepArgsFromSettings(Settings);
	end;
end;

procedure TRipGrepperMainFrame.UpdateHistObject;
begin
	FHistObject := GetHistoryObject(CurrentHistoryItemIndex);
	FHistObject.CopyToSettings(Settings);
end;

procedure TRipGrepperMainFrame.UpdateRipGrepArgumentsInHistObj;
begin
	FHistObject.RipGrepArguments.Clear;
	Settings.ReBuildArguments();
	FHistObject.CopyRipGrepArgsFromSettings(Settings);
end;

procedure TRipGrepperMainFrame.VstResultBeforeCellPaint(Sender : TBaseVirtualTree; TargetCanvas : TCanvas; Node : PVirtualNode;
Column : TColumnIndex; CellPaintMode : TVTCellPaintMode; CellRect : TRect; var ContentRect : TRect);
begin
	if Settings.RipGrepperViewSettings.AlternateRowColors and (Node.ChildCount = 0) then begin
		TargetCanvas.SetAlteringColors(Node.Index);
	end;

	TargetCanvas.FillRect(CellRect);
end;

procedure TRipGrepperMainFrame.VstResultCompareNodes(Sender : TBaseVirtualTree; Node1, Node2 : PVirtualNode; Column : TColumnIndex;
var Result : Integer);
var
	Data1 : PVSFileNodeData;
	Data2 : PVSFileNodeData;
begin
	Data1 := VstResult.GetNodeData(Node1);
	Data2 := VstResult.GetNodeData(Node2);
	if (not Assigned(Data1)) or (not Assigned(Data2)) then
		Result := 0
	else
		case Column of
			0 :
			Result := CompareText(Data1.FilePath, Data2.FilePath);
			1 :
			Result := CompareValue(Data1.MatchData.Row, Data2.MatchData.Row);
			2 :
			Result := CompareValue(Data1.MatchData.Col, Data2.MatchData.Col);
			3 :
			Result := CompareText(Data1.MatchData.MatchText, Data2.MatchData.MatchText);

		end;
end;

procedure TRipGrepperMainFrame.VstResultDrawText(Sender : TBaseVirtualTree; TargetCanvas : TCanvas; Node : PVirtualNode;
Column : TColumnIndex; const Text : string; const CellRect : TRect; var DefaultDraw : Boolean);
begin
	case Column of
		3 : begin
			// First, store the default font size and color number
			var
			fc := TargetCanvas.Font.Color;
			var
			fs := TargetCanvas.Font.Size;
			var
			style := TargetCanvas.Font.Style;
			DefaultDraw := false;
			var
			ss0 := Text.Substring(0, 1);
			var
			ss1 := Text.Substring(1, 2);
			var
			ss2 := Text.Substring(3);

			TargetCanvas.TextOut(CellRect.left, 4, ss0);
			var
			pos := TargetCanvas.TextWidth(ss0);

			TargetCanvas.Font.Color := clMaroon;
			TargetCanvas.Font.Style := [fsBold];
			TargetCanvas.TextOut(CellRect.left + pos, 4, ss1);

			pos := pos + TargetCanvas.TextWidth(ss1);
			TargetCanvas.Font.Color := fc;
			TargetCanvas.Font.size := fs;
			TargetCanvas.Font.Style := style;
			TargetCanvas.TextOut(CellRect.left + pos, 4, ss2);
		end;
	end;

end;

procedure TRipGrepperMainFrame.VstResultFreeNode(Sender : TBaseVirtualTree; Node : PVirtualNode);
var
	NodeData : PVSFileNodeData;
begin
	NodeData := Sender.GetNodeData(Node);
	NodeData^.MatchData.Free;
end;

procedure TRipGrepperMainFrame.VstResultGetImageIndex(Sender : TBaseVirtualTree; Node : PVirtualNode; Kind : TVTImageKind;
Column : TColumnIndex; var Ghosted : Boolean; var ImageIndex : TImageIndex);
var
	nodeData : PVSFileNodeData;
begin
	if not Settings.RipGrepperViewSettings.ShowFileIcon then
		Exit;

	if Node.ChildCount > 0 then begin
		nodeData := Sender.GetNodeData(Node);
		case Kind of
			ikNormal, ikSelected :
			case Column of
				0 :
				ImageIndex := FIconImgList.GetImgIndex(nodeData^.FilePath);
			end;
		end;
	end;
end;

procedure TRipGrepperMainFrame.VstResultGetText(Sender : TBaseVirtualTree; Node : PVirtualNode; Column : TColumnIndex;
TextType : TVSTTextType; var CellText : string);
var
	nodeData : PVSFileNodeData;
begin
	nodeData := Sender.GetNodeData(Node);
	// return identifier of the node
	CellText := nodeData^.FilePath;

	// return the the identifier of the node
	if nodeData^.MatchData = nil then
		CellText := ''
	else begin

		case Column of
			- 1, 0 : begin // main column, -1 if columns are hidden, 0 if they are shown
				if TextType = ttNormal then begin
					CellText := GetAbsOrRelativePath(nodeData^.FilePath);
				end else begin // ttStatic
					CellText := '';
					if Node.ChildCount > 0 then begin
						CellText := Format('[%d]', [Node.ChildCount]);
					end;
				end;
			end;
			1 : begin
				CellText := GetRowColText(nodeData.MatchData.Row, TextType);
			end;
			2 : begin
				CellText := GetRowColText(nodeData.MatchData.Col, TextType);
			end;
			3 : begin
				CellText := IfThen(TextType = ttNormal, nodeData.MatchData.MatchText);
				if not Settings.RipGrepperViewSettings.IndentLines then begin
					CellText := CellText.TrimLeft;
				end;
			end;

		end;
	end;
end;

procedure TRipGrepperMainFrame.VstResultHeaderClick(Sender : TVTHeader; HitInfo : TVTHeaderHitInfo);
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

procedure TRipGrepperMainFrame.VstResultPaintText(Sender : TBaseVirtualTree; const TargetCanvas : TCanvas; Node : PVirtualNode;
Column : TColumnIndex; TextType : TVSTTextType);
begin
	if TextType = ttNormal then begin
		case Column of
			0 : begin
				TargetCanvas.Font.Style := TargetCanvas.Font.Style + [fsBold];
				TargetCanvas.Font.Color := clGrayText;
			end;
		end;
	end else begin // ttStatic
		TargetCanvas.Font.Color := clPurple;
	end;
end;

end.
