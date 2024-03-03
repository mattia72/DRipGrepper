unit RipGrepper.UI.MainForm;

interface

uses

	// Winapi.Messages,
	System.Variants,
	System.Classes,
	Vcl.Graphics,
	Vcl.Controls,
	Vcl.Forms,
	Vcl.StdCtrls,
	Vcl.ComCtrls,
	Vcl.ExtCtrls,
	// System.ImageList,
	Vcl.ImgList,
	// System.Actions,
	Vcl.ActnList,
	Vcl.ToolWin,
	Vcl.Menus,
	RipGrepper.Tools.ProcessUtils,
	RipGrepper.Common.Settings,
	RipGrepper.Data.Matches,
	RipGrepper.Common.Types,
	RipGrepper.Common.Interfaces,
	Winapi.Windows,
	System.ImageList,
	System.Actions,
	System.Threading,
	Vcl.WinXCtrls,
	System.Diagnostics,
	RipGrepper.Common.Sorter,
	RipGrepper.Data.HistoryItemObject,
	RipGrepper.UI.DockableForm,
	u_dzDpiScaleUtils,
	RipGrepper.OpenWith.SimpleTypes,
	System.IniFiles,
	ToolsAPI,
	GX_BaseForm,
	RipGrepper.UI.MainFrame,
	RipGrepper.UI.BottomFrame,
	RipGrepper.UI.TopFrame;

type
	TRipGrepperForm = class(TfmBaseForm, { TfmIdeDockForm, } INewLineEventHandler, ITerminateEventProducer, IEOFProcessEventHandler)
		var
			panelMain : TPanel;
			ListViewResult : TListView;
			ImageListButtons : TImageList;
			ActionList : TActionList;
			ActionCancel : TAction;
			ActionSortByFile : TAction;
			ActionSortByRow : TAction;
			PopupMenu1 : TPopupMenu;
			ActionCopyFileName : TAction;
			CopyFileNameToClipboard : TMenuItem;
			ActionCopyPathToClipboard : TAction;
			CopyPathToClipboard : TMenuItem;
			ImageListListView : TImageList;
			ImageFileIcon : TImage;
			SplitView1 : TSplitView;
			Splitter1 : TSplitter;
			ListBoxSearchHistory : TListBox;
			PanelHistory : TPanel;
			PanelResult : TPanel;
			ActionOpenWith : TAction;
			Openwith1 : TMenuItem;
			N1 : TMenuItem;
			BottomFrame : TRipGrepperBottomFrame;
			TopFrame : TRipGrepperTopFrame;
			procedure ActionCancelExecute(Sender : TObject);
			procedure ActionCopyFileNameExecute(Sender : TObject);
			procedure ActionCopyPathToClipboardExecute(Sender : TObject);
			procedure ActionOpenWithExecute(Sender : TObject);
			procedure ActionOpenWithUpdate(Sender : TObject);
			procedure ActionSortByFileExecute(Sender : TObject);
			procedure ActionSortByFileUpdate(Sender : TObject);
			procedure ActionSortByRowExecute(Sender : TObject);
			procedure ActionSortByRowUpdate(Sender : TObject);
			procedure FormCreate(Sender : TObject);
			procedure FormClose(Sender : TObject; var Action : TCloseAction);
			procedure FormResize(Sender : TObject);
			procedure FormShow(Sender : TObject);
			/// <summary>
			/// Returns the class of the frame that you want embedded in the dockable form
			/// </summary>
			function GetFrameClass : TCustomFrameClass; // override;
			procedure ListBoxSearchHistoryClick(Sender : TObject);
			procedure ListBoxSearchHistoryData(Control : TWinControl; Index : Integer; var Data : string);
			procedure ListBoxSearchHistoryDblClick(Sender : TObject);
			procedure ListBoxSearchHistoryDrawItem(Control : TWinControl; Index : Integer; Rect : TRect; State : TOwnerDrawState);
			procedure ListViewResultColumnClick(Sender : TObject; Column : TListColumn);
			procedure ListViewResultData(Sender : TObject; Item : TListItem);
			procedure ListViewResultDblClick(Sender : TObject);
			procedure ListViewResultDrawItem(Sender : TCustomListView; Item : TListItem; Rect : TRect; State : TOwnerDrawState);

		private const
			RIPGREPPER_FORM = 'RipGrepperForm';

		var
			FAbortSearch : Boolean;
			FswSearchStart : TStopwatch;
			FData : TRipGrepperData;
			FExeVersion : string;
			FMaxWidths : TArray<integer>;
			FFileNameType : TFileNameType;
			FRgExeVersion : string;
			FSearchPathIsDir : Boolean;
			FSettings : TRipGrepperSettings;
			FColumnSortTypes : TArray<TSortDirectionType>;
			FCurrentHistoryItemIndex : Integer;
			FHistObject : THistoryItemObject;
			FHistoryObjectList : TStringList;
			FImageScaler : TImageListScaler;
			FIsParsingRunning : Boolean;
			FMeassureFirstDrawEvent : Boolean;
			FRipGrepTask : ITask;
			FViewStyleIndex : Integer;
			function GetSortingImageIndex(const _idx : Integer) : Integer;
			function GetViewStyleIndex : Integer;
			procedure InitStatusBar;
			procedure LoadSettings;
			procedure DoSortOnColumn(const _sbt : TSortByType);
			procedure DrawItemOnCanvas(_Canvas : TCanvas; _Rect : TRect; _Item : TListItem; _State : TOwnerDrawState);
			function GetAbsOrRelativePath(const _sFullPath : string) : string;
			function GetCounterText(data : THistoryItemObject) : string;
			function GetHistObject : THistoryItemObject;
			function GetHistoryObject(const _index : Integer) : THistoryItemObject;
			function GetSettings : TRipGrepperSettings;
			procedure InitColumnSortTypes;
			procedure InitForm;
			procedure LoadBeforeSearchSettings;
			procedure OnLastLine(const _iLineNr : integer);
			procedure RefreshCounters;
			procedure RefreshCountersInGUI;
			procedure RunRipGrep;
			procedure SetColumnWidths;
			procedure SetHistObject(const Value : THistoryItemObject);
			procedure SetStatusBarMessage(const _bWithElapsedTime : Boolean = False);
			procedure SetStatusBarStatistic(const _s : string);
			procedure UpdateArgumentsAndSettings;
			procedure UpdateRipGrepArgumentsInHistObj;
			procedure UpdateSortingImages(const _sbtArr : TArray<TSortByType>);
			property Settings : TRipGrepperSettings read GetSettings write FSettings;

		protected
			procedure ApplyDpi(_NewDpi : Integer; _NewBounds : PRect); override;
			procedure ArrangeControls; override;
			procedure CreateParams(var Params : TCreateParams); override;

		public
			constructor Create(_settings : TRipGrepperSettings); reintroduce; overload;
			constructor Create(AOwner : TComponent); overload; override;
			destructor Destroy; override;
			procedure AddOrUpdateHistoryItem;
			procedure CopyToClipboardPathOfSelected;
			class function CreateAndShow(const _settings : TRipGrepperSettings) : TRipGrepperForm;
			function IsSearchRunning : Boolean;
			// INewLineEventHandler
			procedure OnNewOutputLine(const _iLineNr : integer; const _sLine : string; _bIsLast : Boolean = False);
			// ITerminateEventProducer
			function ProcessShouldTerminate : boolean;
			// IEOFProcessEventHandler
			procedure OnEOFProcess();
			procedure SetResultListViewDataToHistoryObj;
			procedure ChangeDataHistItemObject(_ho : THistoryItemObject);
			procedure ClearHistoryObject;
			procedure CopyToClipboardFileOfSelected;
			procedure DoSearch;
			function GetNextViewStyleIdx : Integer;
			function GetOpenWithParamsFromSelected : TOpenWithParams;
			procedure InitSearch;
			procedure Loaded; override;
			procedure UpdateHistObject;
			property AbortSearch : Boolean read FAbortSearch write FAbortSearch;
			property CurrentHistoryItemIndex : Integer read FCurrentHistoryItemIndex write FCurrentHistoryItemIndex;
			property Data : TRipGrepperData read FData write FData;
			property FileNameType : TFileNameType read FFileNameType write FFileNameType;
			property HistObject : THistoryItemObject read GetHistObject write SetHistObject;
			property HistoryObjectList : TStringList read FHistoryObjectList write FHistoryObjectList;
			property MaxWidths : TArray<integer> read FMaxWidths write FMaxWidths;
			property RipGrepTask : ITask read FRipGrepTask write FRipGrepTask;
			property ViewStyleIndex : Integer read GetViewStyleIndex write FViewStyleIndex;
	end;

var
	RipGrepperForm : TRipGrepperForm;

implementation

uses
	System.Math,
	System.UITypes,
	System.IOUtils,
	System.SysUtils,
	System.Types,
	RipGrepper.Tools.DebugTools,
	RipGrepper.Helper.UI,
	RipGrepper.Tools.FileUtils,
	RipGrepper.Helper.Types,
	System.Generics.Defaults,
	Vcl.Dialogs,
	Vcl.Clipbrd,
	Winapi.ShellAPI,
	Winapi.CommCtrl,
	System.StrUtils,
	RipGrepper.UI.SearchForm,
	RipGrepper.Data.Parsers,
	RipGrepper.Helper.ListBox,
	u_dzVclUtils,
	RipGrepper.Parsers.VimGrepMatchLine,
	RipGrepper.Common.ParsedObject,
	RipGrepper.OpenWith,
	RipGrepper.OpenWith.ConfigForm,
	GX_OtaUtils,
	System.TypInfo;

{$R *.dfm}

constructor TRipGrepperForm.Create(_settings : TRipGrepperSettings);
begin
	inherited Create(nil);
	InitForm;

	if Assigned(_settings) then begin
		FSettings := _settings;
	end;
	TDebugUtils.DebugMessage('TRipGrepperForm.Create: ' + FSettings.IniFile.FileName);
end;

constructor TRipGrepperForm.Create(AOwner : TComponent);
begin
	inherited Create(AOwner);

	InitForm;
	LoadSettings;
end;

procedure TRipGrepperForm.FormCreate(Sender : TObject);
begin
	// LoadSettings;
	FRgExeVersion := TFileUtils.GetAppNameAndVersion(Settings.RipGrepParameters.RipGrepPath);
end;

destructor TRipGrepperForm.Destroy;
begin
	TDebugUtils.DebugMessage('TRipGrepperForm.Destroy');
	// if not IsStandAlone then begin
	// TDebugUtils.DebugMessage('UnRegisterDockableForm - ' + RIPGREPPER_FORM);
	// IdeDockManager.UnRegisterDockableForm(self, RIPGREPPER_FORM);
	// end;

	for var i := 0 to FHistoryObjectList.Count - 1 do begin
		if Assigned(FHistoryObjectList.Objects[i])
		{ } and (FHistoryObjectList.Objects[i] is THistoryItemObject) then begin
			(FHistoryObjectList.Objects[i] as THistoryItemObject).Free;
		end;
	end;
	FHistoryObjectList.Free;
	FData.Free;
	// TRipGrepperSettingsInstance.FreeInstance;
	// Settings.Free;
	FImageScaler.Free;
	inherited;
end;

procedure TRipGrepperForm.ActionCancelExecute(Sender : TObject);
begin
	ModalResult := mrCancel;
	Close();
end;

procedure TRipGrepperForm.ActionCopyFileNameExecute(Sender : TObject);
begin
	CopyToClipboardFileOfSelected();
end;

procedure TRipGrepperForm.ActionCopyPathToClipboardExecute(Sender : TObject);
begin
	CopyToClipboardPathOfSelected();
end;

procedure TRipGrepperForm.ActionOpenWithExecute(Sender : TObject);
var
	owp : TOpenWithParams;
begin
	owp := GetOpenWithParamsFromSelected();
	if not owp.IsEmpty then begin
		TOpenWith.Execute(owp);
	end;
end;

procedure TRipGrepperForm.ActionOpenWithUpdate(Sender : TObject);
begin
	ActionOpenWith.Enabled := ListViewResult.SelCount = 1;
end;

procedure TRipGrepperForm.ActionSortByFileExecute(Sender : TObject);
begin
	DoSortOnColumn(sbtFile);
end;

procedure TRipGrepperForm.ActionSortByFileUpdate(Sender : TObject);
begin
	UpdateSortingImages([sbtFile]);
end;

procedure TRipGrepperForm.ActionSortByRowExecute(Sender : TObject);
begin
	DoSortOnColumn(sbtRow);
end;

procedure TRipGrepperForm.ActionSortByRowUpdate(Sender : TObject);
begin
	UpdateSortingImages([sbtRow]);
end;

procedure TRipGrepperForm.AddOrUpdateHistoryItem;
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

procedure TRipGrepperForm.ApplyDpi(_NewDpi : Integer; _NewBounds : PRect);
var
	li : TImageList;
begin
	inherited;
	if not Assigned(FImageScaler) then
		FImageScaler := TImageListScaler.Create(Self, ImageListButtons);
	li := FImageScaler.GetScaledList(_NewDpi);
	TopFrame.Toolbar1.Images := li;
end;

procedure TRipGrepperForm.ArrangeControls;
begin
	inherited;
	SetColumnWidths;
end;

procedure TRipGrepperForm.ClearHistoryObject;
begin
	var
	beu := TBeginEndUpdater.New(ListBoxSearchHistory);
	HistObject.ClearMatches;
end;

class function TRipGrepperForm.CreateAndShow(const _settings : TRipGrepperSettings) : TRipGrepperForm;
begin
	TDebugUtils.DebugMessage('TRipGrepperForm.CreateAndShow: ' + _settings.IniFile.FileName);
	Result := TRipGrepperForm.Create(_settings);

	try
		Result.Show;
		// IdeDockManager.ShowForm(Result);
		// if (mrOk = form.ShowModal()) then begin
		// Result := form.ListViewResult.Items[form.ListViewResult.ItemIndex].SubItems[0];
		// end;
	finally
		TDebugUtils.DebugMessage('TRipGrepperForm.CreateAndShow: finally');
	end;
end;

procedure TRipGrepperForm.DoSearch;
begin
	SetStatusBarStatistic('Searching...');
	FAbortSearch := False;
	UpdateArgumentsAndSettings;
	RunRipGrep();
end;

procedure TRipGrepperForm.FormClose(Sender : TObject; var Action : TCloseAction);
begin
	TDebugUtils.DebugMessage('TRipGrepperForm.FormClose - begin action:' + Integer(Action).ToString);
	Settings.Store;
	TListBoxHelper.FreeItemObjects(ListBoxSearchHistory);
	ListViewResult.Items.Count := 0;
end;

procedure TRipGrepperForm.FormShow(Sender : TObject);
begin
	TDebugUtils.DebugMessage('TRipGrepperForm.FormShow - begin');
	inherited;
	SetStatusBarMessage();
	TDebugUtils.DebugMessage('TRipGrepperForm.FormShow - end');
end;

function TRipGrepperForm.GetSortingImageIndex(const _idx : Integer) : Integer;
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

function TRipGrepperForm.GetViewStyleIndex : Integer;
begin
	Result := FViewStyleIndex;
end;

procedure TRipGrepperForm.InitStatusBar;
begin
	SetStatusBarMessage();
	SetStatusBarStatistic('Ready.');
end;

procedure TRipGrepperForm.LoadSettings;
begin
	Settings.Load;
	LoadBeforeSearchSettings();
end;

procedure TRipGrepperForm.ListViewResultColumnClick(Sender : TObject; Column : TListColumn);
begin
	case Column.Index of
		0 :
		ActionSortByFileExecute(self);
		1 :
		ActionSortByRowExecute(self);
	end;
end;

procedure TRipGrepperForm.ListViewResultData(Sender : TObject; Item : TListItem);
var
	idx : Integer;
begin
	idx := Item.Index;
	if idx < FData.TotalMatchCount then begin
		FData.DataToGrid(idx, ListViewResult, Item);
	end;
end;

procedure TRipGrepperForm.OnEOFProcess;
begin
	TDebugUtils.DebugMessage(Format('End of processing rg.exe output in %s sec.', [GetElapsedTime(FswSearchStart)]));
end;

procedure TRipGrepperForm.OnNewOutputLine(const _iLineNr : integer; const _sLine : string; _bIsLast : Boolean = False);
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

procedure TRipGrepperForm.DoSortOnColumn(const _sbt : TSortByType);
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

procedure TRipGrepperForm.InitColumnSortTypes;
begin
	FColumnSortTypes := [stUnsorted, stUnsorted, stUnsorted, stUnsorted];
end;

procedure TRipGrepperForm.CopyToClipboardFileOfSelected;
var
	idx : Integer;
begin
	if not ListViewResult.TryGetSelected(idx) then
		Exit;
	Clipboard.AsText := TPath.GetFileName(ListViewResult.Items[idx].Caption);
end;

procedure TRipGrepperForm.CopyToClipboardPathOfSelected;
var
	idx : Integer;
begin
	if not ListViewResult.TryGetSelected(idx) then
		Exit;
	Clipboard.AsText := TPath.GetFullPath(ListViewResult.Items[idx].Caption);
end;

procedure TRipGrepperForm.DrawItemOnCanvas(_Canvas : TCanvas; _Rect : TRect; _Item : TListItem; _State : TOwnerDrawState);
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

procedure TRipGrepperForm.FormResize(Sender : TObject);
begin
	SplitView1.Width := panelMain.Width;
	BottomFrame.StatusBar1.Panels[0].Width := PanelHistory.Width;
	SetColumnWidths;
end;

function TRipGrepperForm.GetAbsOrRelativePath(const _sFullPath : string) : string;
begin
	Result := _sFullPath;
	if Settings.RipGrepperViewSettings.ShowRelativePath and FSearchPathIsDir then begin
		Result := Result.Replace(Settings.ActualSearchPath, '.', [rfIgnoreCase]);
	end;
end;

function TRipGrepperForm.GetHistoryObject(const _index : Integer) : THistoryItemObject;
begin
	Result := nil;
	if (_index > -1) and (_index < HistoryObjectList.Count { _lb.Items.Count } ) then begin
		Result := THistoryItemObject(HistoryObjectList.Objects[_index]);
		// Result := THistoryItemObject(_lb.Items.Objects[_index]);
	end;
end;

procedure TRipGrepperForm.InitSearch;
begin
	ListViewResult.Items.Count := 0;
	FData.ClearMatchFiles;
	// ClearData;
	FswSearchStart := TStopwatch.Create();
	FMeassureFirstDrawEvent := True;
	InitStatusBar;
	InitColumnSortTypes;
	UpdateSortingImages([sbtFile, sbtRow]);
	// ListViewResult.Repaint();
	LoadBeforeSearchSettings();
end;

function TRipGrepperForm.IsSearchRunning : Boolean;
begin
	Result := FIsParsingRunning or (Assigned(FRipGrepTask) and (FRipGrepTask.Status = TTaskStatus.Running));
end;

procedure TRipGrepperForm.ListBoxSearchHistoryClick(Sender : TObject);
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
	SetStatusBarMessage(True);
end;

procedure TRipGrepperForm.ListBoxSearchHistoryDblClick(Sender : TObject);
begin
	CurrentHistoryItemIndex := ListBoxSearchHistory.ItemIndex;
	UpdateHistObject;
	TDebugUtils.DebugMessage('History dbl clicked:' + CurrentHistoryItemIndex.ToString);
	SetResultListViewDataToHistoryObj();
end;

procedure TRipGrepperForm.ListBoxSearchHistoryDrawItem(Control : TWinControl; index : Integer; Rect : TRect; State : TOwnerDrawState);
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

procedure TRipGrepperForm.ListViewResultDrawItem(Sender : TCustomListView; Item : TListItem; Rect : TRect; State : TOwnerDrawState);
begin
	if FMeassureFirstDrawEvent then begin
		TDebugUtils.DebugMessage(Format('Firs draw event in %s sec.', [GetElapsedTime(FswSearchStart)]));
		FMeassureFirstDrawEvent := False;
	end;

	DrawItemOnCanvas(Sender.Canvas, Rect, Item, State);
	// DrawItemOnBitmap(Sender, Item, Rect, State);
end;

procedure TRipGrepperForm.LoadBeforeSearchSettings;
begin
	FSearchPathIsDir := TDirectory.Exists(Settings.ActualSearchPath);
end;

function TRipGrepperForm.ProcessShouldTerminate : boolean;
begin
	Result := Assigned(FRipGrepTask) and (FRipGrepTask.Status = TTaskStatus.Canceled);
end;

procedure TRipGrepperForm.RefreshCounters;
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

procedure TRipGrepperForm.SetResultListViewDataToHistoryObj;
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

procedure TRipGrepperForm.RunRipGrep;
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
			SetStatusBarMessage(True);
			FswSearchStart.Stop;
			TDebugUtils.DebugMessage(Format('rg.exe ended in %s sec.', [FHistObject.ElapsedTimeText]));
		end);
	FRipGrepTask.Start;
	BottomFrame.ActivityIndicator1.Animate := True;
end;

procedure TRipGrepperForm.ChangeDataHistItemObject(_ho : THistoryItemObject);
begin
	var
	beu := TBeginEndUpdater.New(ListBoxSearchHistory);
	FData.HistObject := _ho;
end;

procedure TRipGrepperForm.CreateParams(var Params : TCreateParams);
begin
	TDebugUtils.DebugMessage('TRipGrepperForm.CreateParams');
	inherited CreateParams(Params);

	if IsStandAlone then begin
		Params.ExStyle := Params.ExStyle or WS_EX_APPWINDOW;
		Params.WndParent := GetDesktopwindow;
	end;
end;

function TRipGrepperForm.GetCounterText(data : THistoryItemObject) : string;
begin
	if data.ErrorCount > 0 then begin
		Result := Format('%d(%d!) in %d', [data.TotalMatchCount, data.ErrorCount, data.FileCount]);
	end else begin
		Result := Format('%d in %d', [data.TotalMatchCount, data.FileCount]);
	end;
end;

function TRipGrepperForm.GetFrameClass : TCustomFrameClass;
begin
	Result := nil; // TRipGrepperForm;
end;

function TRipGrepperForm.GetHistObject : THistoryItemObject;
begin
	Result := FHistObject;
end;

function TRipGrepperForm.GetNextViewStyleIdx : Integer;
begin
	Result := IfThen(FViewStyleIndex < Length(LISTVIEW_TYPES) - 1, FViewStyleIndex + 1);
	Result := (Result mod Length(LISTVIEW_TYPES));
end;

function TRipGrepperForm.GetOpenWithParamsFromSelected : TOpenWithParams;
var
	selected : TListItem;
begin
	selected := ListViewResult.Selected;
	if Assigned(selected) then begin
		Result.DirPath := ifthen(FSearchPathIsDir, Settings.ActualSearchPath, ExtractFileDir(selected.Caption));
		Result.FileName := selected.Caption;
		Result.Row := StrToIntDef(selected.SubItems[0], -1);
		Result.Column := StrToIntDef(selected.SubItems[1], -1);
		Result.IsEmpty := False;
	end;
end;

function TRipGrepperForm.GetSettings : TRipGrepperSettings;
begin
	if not Assigned(FSettings) then begin
		FSettings := GSettings;
	end;
	Result := FSettings;
end;

procedure TRipGrepperForm.InitForm;
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

	InitDpiScaler;

	// if not IsStandAlone then begin
	// TDebugUtils.DebugMessage('RegisterDockableForm - ' + RIPGREPPER_FORM);
	// IdeDockManager.RegisterDockableForm(TRipGrepperForm, self, RIPGREPPER_FORM);
	// end;

	TDebugUtils.DebugMessage('TRipGrepperForm.InitForm Ended');
end;

procedure TRipGrepperForm.ListBoxSearchHistoryData(Control : TWinControl; Index : Integer; var Data : string);
begin
	Data := HistoryObjectList[index];
end;

procedure TRipGrepperForm.ListViewResultDblClick(Sender : TObject);
begin
	ActionOpenWithExecute(Sender);
end;

procedure TRipGrepperForm.Loaded;
var
	PropInfo : PPropInfo;
	i : Integer;
	cmp : TComponent;
begin
	TDebugUtils.DebugMessage('TRipGrepperForm.Loaded');
	inherited Loaded;
	PropInfo := GetPropInfo(Self, 'StyleElements');
	if Assigned(PropInfo) then
		SetOrdProp(Self, PropInfo, 0);
	for I := 0 to ComponentCount - 1 do begin
		cmp := Components[I];
		PropInfo := GetPropInfo(cmp, 'StyleElements');
		if Assigned(PropInfo) then
			SetOrdProp(cmp, PropInfo, 0);
	end;
end;

procedure TRipGrepperForm.OnLastLine(const _iLineNr : integer);
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

procedure TRipGrepperForm.RefreshCountersInGUI;
begin
	TThread.Synchronize(nil,
		procedure
		begin
			ListBoxSearchHistory.Refresh;
			SetStatusBarStatistic(GetCounterText(HistObject));
		end);
end;

procedure TRipGrepperForm.SetColumnWidths;
begin
	// TListView_Resize(ListViewResult);
	ListView_SetColumnWidth(ListViewResult.Handle, 0, ColumnTextWidth);
	ListView_SetColumnWidth(ListViewResult.Handle, 1, ColumnHeaderWidth);
	ListView_SetColumnWidth(ListViewResult.Handle, 2, ColumnHeaderWidth);
	ListView_SetColumnWidth(ListViewResult.Handle, 3, ColumnTextWidth);
end;

procedure TRipGrepperForm.SetHistObject(const Value : THistoryItemObject);
begin
	FHistObject := Value;
end;

procedure TRipGrepperForm.SetStatusBarMessage(const _bWithElapsedTime : Boolean = False);
var
	msg : string;
begin
	if _bWithElapsedTime then begin
		msg := Format('Search took %s seconds with ' + EXE_AND_VERSION_FORMAT, [FHistObject.ElapsedTimeText, FExeVersion]);
		BottomFrame.FStatusBarStatus := IfThen(FHistObject.RipGrepResult = RIPGREP_ERROR, 'ERROR', 'SUCCES');
	end else begin
		msg := Format(EXE_AND_VERSION_FORMAT, [FExeVersion]);
		BottomFrame.FStatusBarStatus := 'READY';
	end;
	BottomFrame.FStatusBarMessage := msg;
end;

procedure TRipGrepperForm.SetStatusBarStatistic(const _s : string);
begin
	BottomFrame.FStatusBarStatistic := _s;
end;

procedure TRipGrepperForm.UpdateArgumentsAndSettings;
begin
	if FHistObject.RipGrepArguments.Count = 0 then begin
		FHistObject.CopyRipGrepArgsFromSettings(Settings);
	end;
end;

procedure TRipGrepperForm.UpdateHistObject;
begin
	FHistObject := GetHistoryObject(CurrentHistoryItemIndex);
	FHistObject.CopyToSettings(Settings);
end;

procedure TRipGrepperForm.UpdateRipGrepArgumentsInHistObj;
begin
	FHistObject.RipGrepArguments.Clear;
	Settings.ReBuildArguments();
	FHistObject.CopyRipGrepArgsFromSettings(Settings);
end;

procedure TRipGrepperForm.UpdateSortingImages(const _sbtArr : TArray<TSortByType>);
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
