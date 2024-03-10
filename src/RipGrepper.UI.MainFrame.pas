unit RipGrepper.UI.MainFrame;

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
	System.Threading;

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
		ImageFileIcon : TImage;
		SplitView1 : TSplitView;
		Splitter1 : TSplitter;
		PanelHistory : TPanel;
		ListBoxSearchHistory : TListBox;
		PanelResult : TPanel;
		ListViewResult : TListView;
		procedure ActionCopyFileNameExecute(Sender : TObject);
		procedure ActionCopyPathToClipboardExecute(Sender : TObject);
		procedure ActionOpenWithExecute(Sender : TObject);
		procedure ActionOpenWithUpdate(Sender : TObject);
		procedure ActionSortByFileExecute(Sender : TObject);
		procedure ActionSortByFileUpdate(Sender : TObject);
		procedure ActionSortByRowExecute(Sender : TObject);
		procedure ActionSortByRowUpdate(Sender : TObject);
		procedure FrameResize(Sender : TObject);
		procedure ListBoxSearchHistoryClick(Sender : TObject);
		procedure ListBoxSearchHistoryData(Control : TWinControl; Index : Integer; var Data : string);
		procedure ListBoxSearchHistoryDblClick(Sender : TObject);
		procedure ListBoxSearchHistoryDrawItem(Control : TWinControl; Index : Integer; Rect : TRect; State : TOwnerDrawState);
		procedure ListViewResultColumnClick(Sender : TObject; Column : TListColumn);
		procedure ListViewResultData(Sender : TObject; Item : TListItem);
		procedure ListViewResultDblClick(Sender : TObject);
		procedure ListViewResultDrawItem(Sender : TCustomListView; Item : TListItem; Rect : TRect; State : TOwnerDrawState);

		private
			FAbortSearch : Boolean;
			FColumnSortTypes : TArray<TSortDirectionType>;
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
			procedure DoSortOnColumn(const _sbt : TSortByType);
			procedure DrawItemOnCanvas(_Canvas : TCanvas; _Rect : TRect; _Item : TListItem; _State : TOwnerDrawState);
			function GetAbsOrRelativePath(const _sFullPath : string) : string;
			function GetCounterText(data : THistoryItemObject) : string;
			function GetHistObject : THistoryItemObject;
			function GetHistoryObject(const _index : Integer) : THistoryItemObject;
			function GetSettings : TRipGrepperSettings;
			function GetSortingImageIndex(const _idx : Integer) : Integer;
			procedure InitColumnSortTypes;
			procedure LoadBeforeSearchSettings;
			procedure OnLastLine(const _iLineNr : integer);
			procedure RefreshCounters;
			procedure RefreshCountersInGUI;
			procedure RunRipGrep;
			procedure SetColumnWidths;
			procedure SetHistObject(const Value : THistoryItemObject);
			procedure UpdateArgumentsAndSettings;
			procedure UpdateRipGrepArgumentsInHistObj;
			procedure UpdateSortingImages(const _sbtArr : TArray<TSortByType>);
			property Settings : TRipGrepperSettings read GetSettings write FSettings;

			{ Private-Deklarationen }
		public
			destructor Destroy; override;
			procedure AddOrUpdateHistoryItem;
			procedure ChangeDataHistItemObject(_ho : THistoryItemObject);
			procedure ClearHistoryObject;
			procedure CopyToClipboardFileOfSelected;
			procedure CopyToClipboardPathOfSelected;
			procedure DoSearch;
			function GetOpenWithParamsFromSelected : TOpenWithParams;
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
			property Data : TRipGrepperData read FData write FData;
			property ExeVersion : string read FExeVersion write FExeVersion;
			property FileNameType : TFileNameType read FFileNameType write FFileNameType;
			property HistObject : THistoryItemObject read GetHistObject write SetHistObject;
			property HistoryObjectList : TStringList read FHistoryObjectList write FHistoryObjectList;
			property MaxWidths : TArray<integer> read FMaxWidths write FMaxWidths;
			property RipGrepTask : ITask read FRipGrepTask write FRipGrepTask;
			{ Public-Deklarationen }
	end;

implementation

uses
	RipGrepper.UI.AllFrames,
	RipGrepper.OpenWith,
	System.StrUtils,
	RipGrepper.Tools.DebugTools,
	RipGrepper.Helper.UI,
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
	RipGrepper.UI.MainForm;

{$R *.dfm}

destructor TRipGrepperMainFrame.Destroy;
begin
	TListBoxHelper.FreeItemObjects(ListBoxSearchHistory);
	ListViewResult.Items.Count := 0;
	for var i := 0 to FHistoryObjectList.Count - 1 do begin
		if Assigned(FHistoryObjectList.Objects[i])
		{ } and (FHistoryObjectList.Objects[i] is THistoryItemObject) then begin
			(FHistoryObjectList.Objects[i] as THistoryItemObject).Free;
		end;
	end;
	FHistoryObjectList.Free;
	FData.Free;

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
	ActionOpenWith.Enabled := ListViewResult.SelCount = 1;
end;

procedure TRipGrepperMainFrame.ActionSortByFileExecute(Sender : TObject);
begin
	DoSortOnColumn(sbtFile);
end;

procedure TRipGrepperMainFrame.ActionSortByFileUpdate(Sender : TObject);
begin
	UpdateSortingImages([sbtFile]);
end;

procedure TRipGrepperMainFrame.ActionSortByRowExecute(Sender : TObject);
begin
	DoSortOnColumn(sbtRow);
end;

procedure TRipGrepperMainFrame.ActionSortByRowUpdate(Sender : TObject);
begin
	UpdateSortingImages([sbtRow]);
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
	FData.HistObject := _ho;
end;

procedure TRipGrepperMainFrame.ClearHistoryObject;
begin
	var
	beu := TBeginEndUpdater.New(ListBoxSearchHistory);
	HistObject.ClearMatches;
end;

procedure TRipGrepperMainFrame.CopyToClipboardFileOfSelected;
var
	idx : Integer;
begin
	if not ListViewResult.TryGetSelected(idx) then
		Exit;
	Clipboard.AsText := TPath.GetFileName(ListViewResult.Items[idx].Caption);
end;

procedure TRipGrepperMainFrame.CopyToClipboardPathOfSelected;
var
	idx : Integer;
begin
	if not ListViewResult.TryGetSelected(idx) then
		Exit;
	Clipboard.AsText := TPath.GetFullPath(ListViewResult.Items[idx].Caption);
end;

procedure TRipGrepperMainFrame.DoSearch;
begin
	AllFrames.SetStatusBarStatistic('Searching...');
	FAbortSearch := False;
	UpdateArgumentsAndSettings;
	RunRipGrep();
end;

procedure TRipGrepperMainFrame.DoSortOnColumn(const _sbt : TSortByType);
var
	cursor : TCursorSaver;
	st : TSortDirectionType;
begin
	if not Assigned(HistObject) then
		Exit;

	cursor.SetHourGlassCursor();
	try
		st := FColumnSortTypes[integer(_sbt)];
		st := TSortDirectionType((Integer(st) + 1) mod 3);
		FColumnSortTypes[integer(_sbt)] := st;
		FData.SortBy(_sbt, st);
		UpdateSortingImages([_sbt]);
	finally
		ListViewResult.Repaint();
	end;
end;

procedure TRipGrepperMainFrame.DrawItemOnCanvas(_Canvas : TCanvas; _Rect : TRect; _Item : TListItem; _State : TOwnerDrawState);
var
	bm : Vcl.Graphics.TBitmap;
	i : Integer;
	x1 : integer;
	x2 : integer;
	r : TRect;
	s : string;
begin
	if _item.SubItems.Count = 0 then
		Exit;

	if (Settings.RipGrepperViewSettings.AlternateRowColors) then begin
		_Canvas.SetAlteringColors(_Item);
	end;
	_Canvas.SetSelectedColors(_State);

	_Canvas.Brush.Style := bsSolid;
	_Canvas.FillRect(_Rect);
	_Canvas.Brush.Style := bsClear;

	x1 := _Rect.Left;
	x2 := _Rect.Left;
	r := _Rect;
	bm := nil;
	if Settings.RipGrepperViewSettings.ShowFileIcon then begin
		bm := TItemDrawer.DrawFileIcon(_Canvas, r, _Item, ImageFileIcon);
	end;

	for i := 0 to ListViewResult.Columns.Count - 1 do begin
		inc(x2, ListView_GetColumnWidth(ListViewResult.Handle, ListViewResult.Columns[i].Index));
		r.Left := x1;
		r.Right := x2;
		case i of
			0 : begin
				s := GetAbsOrRelativePath(_Item.Caption);
				// File
				r.Left := r.Left + 6;
				if Assigned(bm) then begin
					r.Left := r.Left + bm.Width;
				end;
			end;
			1, 2 : begin
				s := _Item.SubItems[i - 1];
				// Row, Col
				if (s = '-1') then
					s := '';
			end;
			3 : begin
				s := _Item.SubItems[i - 1]; // Match
				if not Settings.RipGrepperViewSettings.IndentLines then begin
					s := s.TrimLeft;
				end;
			end;
			else
			TDebugUtils.DebugMessage('Invalid index: ' + i.ToString);
		end;
		_Canvas.TextRect(r, s, [tfSingleLine, DT_ALIGN[ListViewResult.Columns[i].Alignment], tfVerticalCenter, tfEndEllipsis]);
		x1 := x2;
	end;
end;

procedure TRipGrepperMainFrame.FrameResize(Sender : TObject);
begin
	SplitView1.Width := panelMain.Width;
	AllFrames.BottomFrame.StatusBar1.Panels[0].Width := PanelHistory.Width;
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

function TRipGrepperMainFrame.GetOpenWithParamsFromSelected : TOpenWithParams;
var
	selected : TListItem;
begin
	selected := ListViewResult.Selected;
	if Assigned(selected) then begin
		Result.DirPath := IfThen(Settings.SearchPathIsDir, Settings.ActualSearchPath, ExtractFileDir(selected.Caption));
		Result.FileName := selected.Caption;
		Result.Row := StrToIntDef(selected.SubItems[0], -1);
		Result.Column := StrToIntDef(selected.SubItems[1], -1);
		Result.IsEmpty := False;
	end;
end;

function TRipGrepperMainFrame.GetSettings : TRipGrepperSettings;
begin
	if not Assigned(FSettings) then begin
		FSettings := GSettings;
	end;
	Result := FSettings;
end;

function TRipGrepperMainFrame.GetSortingImageIndex(const _idx : Integer) : Integer;
begin
	case FColumnSortTypes[_idx] of
		stUnsorted :
		Result := IMAGE_IDX_UNSORTED;
		stAscending :
		Result := IMAGE_IDX_ASCENDING_SORTED;
		stDescending :
		Result := IMAGE_IDX_DESCENDING_SORTED;
		else
		Result := -1;
	end;
end;

procedure TRipGrepperMainFrame.InitColumnSortTypes;
begin
	FColumnSortTypes := [stUnsorted, stUnsorted, stUnsorted, stUnsorted];
end;

procedure TRipGrepperMainFrame.Init;
begin
	FData := TRipGrepperData.Create();
	FHistoryObjectList := TStringList.Create(TDuplicates.dupIgnore, False, False);
	{$IFDEF STANDALONE}
	FExeVersion := TFileUtils.GetAppNameAndVersion(Application.ExeName);
	{$ELSE}
	FExeVersion := TFileUtils.GetPackageNameAndVersion(HInstance);
	{$ENDIF}
	FFileNameType := ftAbsolute;

	InitColumnSortTypes;
	UpdateSortingImages([sbtFile, sbtRow]);
	ListViewResult.InitMaxWidths(FMaxWidths);

	TDebugUtils.DebugMessage('TRipGrepperMainFrame.InitForm Ended');
end;

procedure TRipGrepperMainFrame.InitSearch;
begin
	ListViewResult.Items.Count := 0;
	FData.ClearMatchFiles;
	// ClearData;
	FswSearchStart := TStopwatch.Create();
	FMeassureFirstDrawEvent := True;
	AllFrames.InitStatusBar;
	InitColumnSortTypes;
	UpdateSortingImages([sbtFile, sbtRow]);
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
	AllFrames.SetStatusBarMessage(True);
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

procedure TRipGrepperMainFrame.ListViewResultColumnClick(Sender : TObject; Column : TListColumn);
begin
	case Column.Index of
		0 :
		ActionSortByFileExecute(self);
		1 :
		ActionSortByRowExecute(self);
	end;
end;

procedure TRipGrepperMainFrame.ListViewResultData(Sender : TObject; Item : TListItem);
var
	idx : Integer;
begin
	idx := Item.Index;
	if idx < FData.TotalMatchCount then begin
		FData.DataToGrid(idx, ListViewResult, Item);
	end;
end;

procedure TRipGrepperMainFrame.ListViewResultDblClick(Sender : TObject);
begin
	ActionOpenWithExecute(Sender);
end;

procedure TRipGrepperMainFrame.ListViewResultDrawItem(Sender : TCustomListView; Item : TListItem; Rect : TRect; State : TOwnerDrawState);
begin
	if FMeassureFirstDrawEvent then begin
		TDebugUtils.DebugMessage(Format('Firs draw event in %s sec.', [GetElapsedTime(FswSearchStart)]));
		FMeassureFirstDrawEvent := False;
	end;

	DrawItemOnCanvas(Sender.Canvas, Rect, Item, State);
	// DrawItemOnBitmap(Sender, Item, Rect, State);
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
			AllFrames.BottomFrame.ActivityIndicator1.Animate := False;
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
						FData.Add(o);
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
	// virtual listview! Items count should be updated to refresh ui
	ListViewResult.Items.Count := FData.ListItemCount;
	if Assigned(FHistObject) then begin
		var
		beu := TBeginEndUpdater.New(ListBoxSearchHistory);
		FHistObject.FileCount := FData.FileCount; // synced
		FHistObject.ErrorCount := FData.ErrorCount; // synced
	end;
	RefreshCountersInGUI;
end;

procedure TRipGrepperMainFrame.RefreshCountersInGUI;
begin
	TThread.Synchronize(nil,
		procedure
		begin
			ListBoxSearchHistory.Refresh;
			AllFrames.SetStatusBarStatistic(GetCounterText(HistObject));
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
			AllFrames.SetStatusBarMessage(True);
			FswSearchStart.Stop;
			TDebugUtils.DebugMessage(Format('rg.exe ended in %s sec.', [FHistObject.ElapsedTimeText]));
		end);
	FRipGrepTask.Start;
	AllFrames.BottomFrame.ActivityIndicator1.Animate := True;
end;

procedure TRipGrepperMainFrame.SetColumnWidths;
begin
	// TListView_Resize(ListViewResult);
	ListView_SetColumnWidth(ListViewResult.Handle, 0, ColumnTextWidth);
	ListView_SetColumnWidth(ListViewResult.Handle, 1, ColumnHeaderWidth);
	ListView_SetColumnWidth(ListViewResult.Handle, 2, ColumnHeaderWidth);
	ListView_SetColumnWidth(ListViewResult.Handle, 3, ColumnTextWidth);
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
			ListViewResult.Items.Count := 0;
			ChangeDataHistItemObject(FHistObject);
			var
			matchItems := FHistObject.Matches.Items;
			ListViewResult.Items.Count := matchItems.Count + FHistObject.ErrorCount;
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

procedure TRipGrepperMainFrame.UpdateSortingImages(const _sbtArr : TArray<TSortByType>);
var
	imgIdx : integer;
begin

	for var sbt in _sbtArr do begin
		var
		i := integer(sbt);
		imgIdx := GetSortingImageIndex(i);
		ListViewResult.Columns[i].ImageIndex := imgIdx;
		case i of
			0 :
			ActionSortByFile.ImageIndex := imgIdx;
			1 :
			ActionSortByRow.ImageIndex := imgIdx;
		end;
	end;
end;

end.
