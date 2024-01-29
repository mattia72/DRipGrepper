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
	GX_IdeDock,
	u_dzDpiScaleUtils;

type
	TRipGrepperForm = class(TfmIdeDockForm, INewLineEventHandler, ITerminateEventProducer, IEOFProcessEventHandler)
		panelMain : TPanel;
		ListViewResult : TListView;
		pnlBottom : TPanel;
		ImageListButtons : TImageList;
		ActionList : TActionList;
		ActionSearch : TAction;
		ActionCancel : TAction;
		ActionConfig : TAction;
		StatusBar1 : TStatusBar;
		ActionSwitchView : TAction;
		ActionSortByFile : TAction;
		ToolBar1 : TToolBar;
		tbCopyCmdLine : TToolButton;
		tbView : TToolButton;
		tbShowRelativePath : TToolButton;
		ActionShowRelativePath : TAction;
		ToolButton1 : TToolButton;
		ToolButton2 : TToolButton;
		ActionCmdLineCopy : TAction;
		ActionSortByRow : TAction;
		PopupMenu1 : TPopupMenu;
		ActionCopyFileName : TAction;
		Action11 : TMenuItem;
		ActionCopyPathToClipboard : TAction;
		Action12 : TMenuItem;
		ImageListListView : TImageList;
		ImageFileIcon : TImage;
		tbDoSearchCancel : TToolButton;
		ActionShowSearchForm : TAction;
		tbShowFileIcon : TToolButton;
		ActionShowFileIcons : TAction;
		ToolButton3 : TToolButton;
		tbAlternateRowColors : TToolButton;
		ActionAlternateRowColors : TAction;
		ActionAbortSearch : TAction;
		tbAbortSearch : TToolButton;
		tbRefreshSearch : TToolButton;
		ActionRefreshSearch : TAction;
		tbIndentLines : TToolButton;
		ActionIndentLine : TAction;
		SplitView1 : TSplitView;
		Splitter1 : TSplitter;
		ListBoxSearchHistory : TListBox;
		PanelHistory : TPanel;
		PanelResult : TPanel;
		ActionStatusBar : TAction;
		procedure ActionStatusBarUpdate(Sender : TObject);
		procedure ActionAbortSearchExecute(Sender : TObject);
		procedure ActionAbortSearchUpdate(Sender : TObject);
		procedure ActionAlternateRowColorsExecute(Sender : TObject);
		procedure ActionAlternateRowColorsUpdate(Sender : TObject);
		procedure ActionCancelExecute(Sender : TObject);
		procedure ActionCmdLineCopyExecute(Sender : TObject);
		procedure ActionCmdLineCopyUpdate(Sender : TObject);
		procedure ActionConfigExecute(Sender : TObject);
		procedure ActionCopyFileNameExecute(Sender : TObject);
		procedure ActionCopyPathToClipboardExecute(Sender : TObject);
		procedure ActionShowSearchFormExecute(Sender : TObject);
		procedure ActionShowSearchFormUpdate(Sender : TObject);
		procedure ActionIndentLineExecute(Sender : TObject);
		procedure ActionIndentLineUpdate(Sender : TObject);
		procedure ActionRefreshSearchExecute(Sender : TObject);
		procedure ActionRefreshSearchUpdate(Sender : TObject);
		procedure ActionShowRelativePathExecute(Sender : TObject);
		procedure ActionSearchExecute(Sender : TObject);
		procedure ActionShowFileIconsExecute(Sender : TObject);
		procedure ActionShowFileIconsUpdate(Sender : TObject);
		procedure ActionShowRelativePathUpdate(Sender : TObject);
		procedure ActionSortByFileExecute(Sender : TObject);
		procedure ActionSortByFileUpdate(Sender : TObject);
		procedure ActionSortByRowExecute(Sender : TObject);
		procedure ActionSortByRowUpdate(Sender : TObject);
		procedure ActionSwitchViewExecute(Sender : TObject);
		procedure ActionSwitchViewUpdate(Sender : TObject);
		procedure FormClose(Sender : TObject; var Action : TCloseAction);
		procedure FormResize(Sender : TObject);
		procedure FormShow(Sender : TObject);
		procedure ListBoxSearchHistoryClick(Sender : TObject);
		procedure ListBoxSearchHistoryData(Control : TWinControl; Index : Integer; var Data : string);
		procedure ListBoxSearchHistoryDblClick(Sender : TObject);
		procedure ListBoxSearchHistoryDrawItem(Control : TWinControl; Index : Integer; Rect : TRect; State : TOwnerDrawState);
		procedure ListViewResultColumnClick(Sender : TObject; Column : TListColumn);
		procedure ListViewResultData(Sender : TObject; Item : TListItem);
		procedure ListViewResultDrawItem(Sender : TCustomListView; Item : TListItem; Rect : TRect; State : TOwnerDrawState);

		private
			FswSearchStart : TStopwatch;
			FData : TRipGrepperData;
			FExeVersion : string;
			FMaxWidths : TArray<integer>;
			FFileNameType : TFileNameType;
			FRgExeVersion : string;
			FSearchPathIsDir : Boolean;
			FSettings : TRipGrepperSettingsHistory;
			FColumnSortTypes : TArray<TSortDirectionType>;
			FCurrentHistoryItemIndex : Integer;
			FHistObject : THistoryItemObject;
			FHistoryObjectList : TStringList;
			FImageScaler : TImageListScaler;
			FMeassureFirstDrawEvent : Boolean;
			FRipGrepTask : ITask;
			FStatusBarMessage : string;
			FStatusBarStatus : string;
			FStatusBarStatistic : string;
			FViewStyleIndex : Integer;
			procedure AddNewHistoryItem;
			procedure ClearHistoryObject;
			procedure DoSearch;
			function GetSortingImageIndex(const _idx : Integer) : Integer;
			function GetViewStyleIndex : Integer;
			procedure InitStatusBar;
			procedure LoadSettings;
			procedure CopyToClipboardFileOfSelected;
			procedure DoSortOnColumn(const _sbt : TSortByType);
			procedure DrawItemOnCanvas(_Canvas : TCanvas; _Rect : TRect; _Item : TListItem; _State : TOwnerDrawState);
			function GetAbsOrRelativePath(const _sFullPath : string) : string;
			function GetHistoryObject(const _index : Integer) : THistoryItemObject;
			procedure InitColumnSortTypes;
			procedure InitSearch;
			procedure LoadBeforeSearchSettings;
			procedure RefreshCounters;
			procedure RunRipGrep;
			procedure SetColumnWidths;
			procedure SetStatusBarMessage(const _iRipGrepResultOk : Integer = 0);
			procedure SetStatusBarStatistic(const _s : string);
			procedure UpdateArgumentsAndSettings;
			procedure UpdateHistObject;
			procedure UpdateSortingImages(const _sbtArr : TArray<TSortByType>);
			property CurrentHistoryItemIndex : Integer read FCurrentHistoryItemIndex write FCurrentHistoryItemIndex;
			property HistoryObjectList : TStringList read FHistoryObjectList write FHistoryObjectList;
			property ViewStyleIndex : Integer read GetViewStyleIndex;

		protected
			procedure ApplyDpi(_NewDpi : Integer; _NewBounds : PRect); override;
			procedure ArrangeControls; override;

		public
			constructor Create(_settings : TRipGrepperSettingsHistory); reintroduce; overload;
			constructor Create(AOwner : TComponent); overload; override;
			destructor Destroy; override;
			procedure CopyToClipboardPathOfSelected;
			class function CreateAndShow(const _settings : TRipGrepperSettingsHistory) : string;
			function IsSearchRunning : Boolean;
			// INewLineEventHandler
			procedure OnNewOutputLine(const _iLineNr : integer; const _sLine : string; _bIsLast : Boolean = False);
			// ITerminateEventProducer
			function ProcessShouldTerminate : boolean;
			// IEOFProcessEventHandler
			procedure OnEOFProcess();
			procedure RefreshResultByHistoryItem;
			procedure ChangeDataHistItemObject(_ho : THistoryItemObject);

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
	RipGrepper.Common.ParsedObject;

{$R *.dfm}

constructor TRipGrepperForm.Create(_settings : TRipGrepperSettingsHistory);
begin
	inherited Create(nil);
	FSettings.Free;
	FSettings := _settings;
end;

constructor TRipGrepperForm.Create(AOwner : TComponent);
begin
	inherited Create(AOwner);
	FSettings := TRipGrepperSettingsHistory.Create();
	FData := TRipGrepperData.Create();
	FHistoryObjectList := TStringList.Create(TDuplicates.dupIgnore, False, False);

	FExeVersion := TFileUtils.GetAppNameAndVersion(Application.ExeName);
	FFileNameType := ftAbsolute;

	InitColumnSortTypes;
	UpdateSortingImages([sbtFile, sbtRow]);
	ListViewResult.InitMaxWidths(FMaxWidths);

	InitDpiScaler;
end;

destructor TRipGrepperForm.Destroy;
begin
	for var i := 0 to FHistoryObjectList.Count - 1 do begin
		FHistoryObjectList.Objects[i].Free;
	end;
	FHistoryObjectList.Free;
	FData.Free;
	FSettings.Free;
	inherited;
end;

procedure TRipGrepperForm.ActionStatusBarUpdate(Sender : TObject);
begin
	StatusBar1.Panels[PNL_MESSAGE_IDX].Text := FStatusBarMessage;
	StatusBar1.Panels[PNL_STATUS_IDX].Text := FStatusBarStatus;
	StatusBar1.Panels[PNL_STATTS_IDX].Text := FStatusBarStatistic;
end;

procedure TRipGrepperForm.ActionAbortSearchExecute(Sender : TObject);
begin
	FRipGrepTask.Cancel;
end;

procedure TRipGrepperForm.ActionAbortSearchUpdate(Sender : TObject);
begin
	ActionAbortSearch.Enabled := IsSearchRunning;
end;

procedure TRipGrepperForm.ActionAlternateRowColorsExecute(Sender : TObject);
begin
	FSettings.RipGrepperViewSettings.AlternateRowColors := (not FSettings.RipGrepperViewSettings.AlternateRowColors);
	FSettings.StoreViewSettings('AlternateRowColors');
	ListViewResult.Repaint();
end;

procedure TRipGrepperForm.ActionAlternateRowColorsUpdate(Sender : TObject);
begin
	tbAlternateRowColors.Down := FSettings.RipGrepperViewSettings.AlternateRowColors;
end;

procedure TRipGrepperForm.ActionCancelExecute(Sender : TObject);
begin
	ModalResult := mrCancel;
	Close;
end;

procedure TRipGrepperForm.ActionCmdLineCopyExecute(Sender : TObject);
begin
	ClipBoard.AsText := FSettings.RipGrepParameters.BuildCmdLine;
end;

procedure TRipGrepperForm.ActionCmdLineCopyUpdate(Sender : TObject);
begin
	ActionCmdLineCopy.Hint := 'Copy command line:' + CRLF + FSettings.RipGrepParameters.BuildCmdLine;
end;

procedure TRipGrepperForm.ActionConfigExecute(Sender : TObject);
begin
	//
end;

procedure TRipGrepperForm.ActionCopyFileNameExecute(Sender : TObject);
begin
	CopyToClipboardFileOfSelected();
end;

procedure TRipGrepperForm.ActionCopyPathToClipboardExecute(Sender : TObject);
begin
	CopyToClipboardPathOfSelected();
end;

procedure TRipGrepperForm.ActionShowSearchFormExecute(Sender : TObject);
begin
	var
	frm := TRipGrepperSearchDialogForm.Create(self, FSettings);
	try
		if (mrOk = frm.ShowModal) then begin
			ActionSearchExecute(self);
		end;

	finally
		frm.Free;
	end;
end;

procedure TRipGrepperForm.ActionShowSearchFormUpdate(Sender : TObject);
begin
	ActionShowSearchForm.Enabled := FSettings.IsEmpty or (not IsSearchRunning);
end;

procedure TRipGrepperForm.ActionIndentLineExecute(Sender : TObject);
begin
	FSettings.RipGrepperViewSettings.IndentLines := not FSettings.RipGrepperViewSettings.IndentLines;
	FSettings.StoreViewSettings('IndentLines');
	ListViewResult.Repaint();
end;

procedure TRipGrepperForm.ActionIndentLineUpdate(Sender : TObject);
begin
	tbIndentLines.Down := FSettings.RipGrepperViewSettings.IndentLines;
end;

procedure TRipGrepperForm.ActionRefreshSearchExecute(Sender : TObject);
var
	cursor : TCursorSaver;
begin
	cursor.SetHourGlassCursor;
	ClearHistoryObject();
	InitSearch();
	DoSearch();
end;

procedure TRipGrepperForm.ActionRefreshSearchUpdate(Sender : TObject);
begin
	ActionRefreshSearch.Enabled := FSettings.IsLoaded and (not IsSearchRunning);
end;

procedure TRipGrepperForm.ActionShowRelativePathExecute(Sender : TObject);
const
	PARSER_TYPES : TArray<TFileNameType> = [ftAbsolute, ftRelative];
begin
	FSettings.RipGrepperViewSettings.ShowRelativePath := not FSettings.RipGrepperViewSettings.ShowRelativePath;
	var
	idx := Integer(FSettings.RipGrepperViewSettings.ShowRelativePath);
	FFileNameType := PARSER_TYPES[idx mod Length(PARSER_TYPES)];
	ListViewResult.InitMaxWidths(FMaxWidths);
	FSettings.StoreViewSettings('ShowRelativePath');
	ListViewResult.Repaint;
end;

procedure TRipGrepperForm.ActionSearchExecute(Sender : TObject);
var
	cursor : TCursorSaver;
begin
	cursor.SetHourGlassCursor;
	AddNewHistoryItem;
	ListBoxSearchHistory.ItemIndex := 0;
	FData.ClearMatchFiles;
	InitSearch();
	DoSearch();
end;

procedure TRipGrepperForm.ActionShowFileIconsExecute(Sender : TObject);
begin
	FSettings.RipGrepperViewSettings.ShowFileIcon := not FSettings.RipGrepperViewSettings.ShowFileIcon;
	FSettings.StoreViewSettings('ShowFileIcon');
	ListViewResult.Repaint();
end;

procedure TRipGrepperForm.ActionShowFileIconsUpdate(Sender : TObject);
begin
	tbShowFileIcon.Down := FSettings.RipGrepperViewSettings.ShowFileIcon;
	// ActionShowFileIcons.ImageIndex := Ifthen(FSettings.ShowFileIcon, IMG_IDX_SHOW_FILE_ICON_TRUE, IMG_IDX_SHOW_FILE_ICON_FALSE);
end;

procedure TRipGrepperForm.ActionShowRelativePathUpdate(Sender : TObject);
begin
	tbShowRelativePath.Down := FSettings.RipGrepperViewSettings.ShowRelativePath;
	// ActionShowRelativePath.ImageIndex := Ifthen(FSettings.ShowRelativePath, IMG_IDX_SHOW_RELATIVE_PATH, IMG_IDX_SHOW_ABS_PATH);
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

procedure TRipGrepperForm.ActionSwitchViewExecute(Sender : TObject);
begin
	ListViewResult.ViewStyle := LISTVIEW_TYPES[ViewStyleIndex];
end;

procedure TRipGrepperForm.ActionSwitchViewUpdate(Sender : TObject);
begin
	var
	idx := IfThen((FViewStyleIndex + 1) <= (Length(LISTVIEW_TYPES) - 1), FViewStyleIndex + 1, 0);
	// ActionSwitchView.ImageIndex := idx + 2;
	ActionSwitchView.Hint := 'Change View ' + LISTVIEW_TYPE_TEXTS[idx];
end;

procedure TRipGrepperForm.AddNewHistoryItem;
var
	hi : THistoryItemObject;
begin
	hi := THistoryItemObject.Create();
	FHistObject := hi;
	ChangeDataHistItemObject(hi);
	CurrentHistoryItemIndex := HistoryObjectList.IndexOf(FSettings.ActualSearchText);
	if CurrentHistoryItemIndex = -1 then begin
		CurrentHistoryItemIndex := HistoryObjectList.AddObject(FSettings.ActualSearchText, hi);
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
	Toolbar1.Images := li;
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
	FHistObject.ClearMatches;
end;

class function TRipGrepperForm.CreateAndShow(const _settings : TRipGrepperSettingsHistory) : string;
begin
	var
	form := TRipGrepperForm.Create(_settings);
	try
		if (mrOk = form.ShowModal()) then begin
			Result := form.ListViewResult.Items[form.ListViewResult.ItemIndex].SubItems[0];
		end;
	finally
		form.Free;
	end;

end;

procedure TRipGrepperForm.DoSearch;
begin
	SetStatusBarStatistic('Searching...');
	UpdateArgumentsAndSettings;
	RunRipGrep();
end;

procedure TRipGrepperForm.FormClose(Sender : TObject; var Action : TCloseAction);
begin
	TListBoxHelper.FreeItemObjects(ListBoxSearchHistory);
end;

procedure TRipGrepperForm.FormShow(Sender : TObject);
begin
	LoadSettings;
	SetStatusBarMessage();
	FRgExeVersion := TFileUtils.GetAppNameAndVersion(FSettings.RipGrepParameters.RipGrepPath);
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
	FViewStyleIndex := IfThen(FViewStyleIndex < Length(LISTVIEW_TYPES) - 1, FViewStyleIndex + 1);
	Result := (FViewStyleIndex mod Length(LISTVIEW_TYPES));
end;

procedure TRipGrepperForm.InitStatusBar;
begin
	SetStatusBarMessage();
	SetStatusBarStatistic('Ready.');
end;

procedure TRipGrepperForm.LoadSettings;
begin
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
	if _bIsLast then begin
		// ListViewResult.AdjustColumnWidths(FMaxWidths);
		SetColumnWidths;
		TDebugUtils.DebugMessage(Format('Last line (%d.) received in %s sec.', [_iLineNr, GetElapsedTime(FswSearchStart)]));
	end;

	if (_iLineNr >= RG_PROCESSING_LINE_COUNT_LIMIT) then begin
		if _iLineNr = RG_PROCESSING_LINE_COUNT_LIMIT then begin
			MessageDlg(Format('Too many results.' + CRLF + 'The first %d will be shown. Try to be more specific.',
				[_iLineNr, RG_PROCESSING_LINE_COUNT_LIMIT]), TMsgDlgType.mtWarning, [mbOk], 0);
		end else begin
			Exit;
		end;
	end;

	TTask.Run(
		procedure
		begin
			try
				TThread.Queue(nil,
					procedure
					var
						parsedObj : IParsedObjectRow;
						parser : ILineParser;
					begin
						if (not _sLine.IsEmpty) then begin
							parser := TVimGrepMatchLineParser.Create();
							case FFileNameType of
								ftAbsolute, ftRelative : begin
									parsedObj := parser.ParseLine(_iLineNr, _sLine, _bIsLast);
								end;
							end;
							FData.Add(parsedObj);
						end;
						// First 100 than every 100
						if (_iLineNr < 100) or ((_iLineNr mod DRAW_RESULT_ON_EVERY_LINE_COUNT) = 0) or _bIsLast then begin
							RefreshCounters;
						end;
					end);
			except
				on e : EOutOfMemory do begin
					TDebugUtils.DebugMessage(Format('Exception %s ' + CRLF + 'on line %d', [E.Message, _iLineNr]));
					MessageDlg(Format(E.Message + CRLF + 'Too many results. Try to be more specific', [_iLineNr]), TMsgDlgType.mtError,
						[mbOk], 0);
				end;
			end;
		end);

end;

procedure TRipGrepperForm.DoSortOnColumn(const _sbt : TSortByType);
var
	cursor : TCursorSaver;
	st : TSortDirectionType;
begin
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

	if (FSettings.RipGrepperViewSettings.AlternateRowColors) then begin
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
	if FSettings.RipGrepperViewSettings.ShowFileIcon then begin
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
				if not FSettings.RipGrepperViewSettings.IndentLines then begin
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
	StatusBar1.Panels[0].Width := PanelHistory.Width;
	SetColumnWidths;
end;

function TRipGrepperForm.GetAbsOrRelativePath(const _sFullPath : string) : string;
begin
	Result := _sFullPath;
	if FSettings.RipGrepperViewSettings.ShowRelativePath and FSearchPathIsDir then begin
		Result := Result.Replace(FSettings.ActualSearchPath, '.', [rfIgnoreCase]);
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
	Result := Assigned(FRipGrepTask) and (FRipGrepTask.Status = TTaskStatus.Running);
end;

procedure TRipGrepperForm.ListBoxSearchHistoryClick(Sender : TObject);
begin
	CurrentHistoryItemIndex := ListBoxSearchHistory.ItemIndex;
	UpdateHistObject;
	TDebugUtils.DebugMessage('History clicked: ' + CurrentHistoryItemIndex.ToString);
	TDebugUtils.DebugMessage('History Object: ' + FHistObject.RipGrepArguments.DelimitedText);
	TDebugUtils.DebugMessage('History Matches: ' + FHistObject.Matches.Items.Count.ToString);
	TDebugUtils.DebugMessage('History Files: ' + FHistObject.FileCount.ToString);
	RefreshResultByHistoryItem();
end;

procedure TRipGrepperForm.ListBoxSearchHistoryDblClick(Sender : TObject);
begin
	CurrentHistoryItemIndex := ListBoxSearchHistory.ItemIndex;
	UpdateHistObject;
	TDebugUtils.DebugMessage('History dbl clicked:' + CurrentHistoryItemIndex.ToString);
	RefreshResultByHistoryItem();
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
		cnv.TextOut(Rect.Left + cMatchesLeft, Rect.Top + c2ndRowTop,
		{ } Format('%d in %d', [data.TotalMatchCount, data.FileCount]));
	end;
end;

procedure TRipGrepperForm.ListViewResultDrawItem(Sender : TCustomListView; Item : TListItem; Rect : TRect; State : TOwnerDrawState);
begin
	if FMeassureFirstDrawEvent then begin
		TDebugUtils.DebugMessage(Format('Firs drw event in %s sec.', [GetElapsedTime(FswSearchStart)]));
		FMeassureFirstDrawEvent := False;
	end;

	DrawItemOnCanvas(Sender.Canvas, Rect, Item, State);
	// DrawItemOnBitmap(Sender, Item, Rect, State);
end;

procedure TRipGrepperForm.LoadBeforeSearchSettings;
begin
	FSearchPathIsDir := TDirectory.Exists(FSettings.ActualSearchPath);
end;

function TRipGrepperForm.ProcessShouldTerminate : boolean;
begin
	Result := Assigned(FRipGrepTask) and (FRipGrepTask.Status = TTaskStatus.Canceled);
end;

procedure TRipGrepperForm.RefreshCounters;
begin
	// virtual listview! Items count should be updated to refresh ui
	ListViewResult.Items.Count := FData.ListItemCount;
	SetStatusBarStatistic(Format('%d matches in %d files', [FData.TotalMatchCount, FData.FileCount]));
	if Assigned(FHistObject) then begin
		var
		beu := TBeginEndUpdater.New(ListBoxSearchHistory);
		FHistObject.FileCount := FData.FileCount;
		FHistObject.ErrorCount := FData.ErrorCount;
	end;
	ListBoxSearchHistory.Refresh;
end;

procedure TRipGrepperForm.RefreshResultByHistoryItem;
begin
	TThread.Synchronize(nil, // Refresh result by history
		procedure
		begin
			ListViewResult.Items.Count := 0;
			ChangeDataHistItemObject(FHistObject);
			ListViewResult.Items.Count := FHistObject.Matches.Items.Count + FHistObject.ErrorCount;
		end);
end;

procedure TRipGrepperForm.RunRipGrep;
var
	iRipGrepResult : Integer;
	workDir : string;
begin
	FRipGrepTask := TTask.Create(
		procedure()
		begin
			workDir := TDirectory.GetCurrentDirectory();
			TDebugUtils.DebugMessage('run: ' + FSettings.RipGrepParameters.RipGrepPath + ' '
				{ } + FSettings.RipGrepParameters.RipGrepArguments.DelimitedText);
			FswSearchStart := TStopwatch.StartNew;
			iRipGrepResult := TProcessUtils.RunProcess(FSettings.RipGrepParameters.RipGrepPath,
				FSettings.RipGrepParameters.RipGrepArguments,
				{ } workDir,
				{ } self as INewLineEventHandler,
				{ } self as ITerminateEventProducer,
				{ } self as IEOFProcessEventHandler);
			SetStatusBarMessage(iRipGrepResult);
			TDebugUtils.DebugMessage(Format('rg.exe ended in %s sec.', [GetElapsedTime(FswSearchStart)]));
			FswSearchStart.Stop;
		end);
	FRipGrepTask.Start;
end;

procedure TRipGrepperForm.ChangeDataHistItemObject(_ho : THistoryItemObject);
begin
	var
	beu := TBeginEndUpdater.New(ListBoxSearchHistory);
	FData.HistObject := _ho;
end;

procedure TRipGrepperForm.ListBoxSearchHistoryData(Control : TWinControl; Index : Integer; var Data : string);
begin
	Data := HistoryObjectList[index];
end;

procedure TRipGrepperForm.SetColumnWidths;
begin
	// TListView_Resize(ListViewResult);
	ListView_SetColumnWidth(ListViewResult.Handle, 0, ColumnTextWidth);
	ListView_SetColumnWidth(ListViewResult.Handle, 1, ColumnHeaderWidth);
	ListView_SetColumnWidth(ListViewResult.Handle, 2, ColumnHeaderWidth);
	ListView_SetColumnWidth(ListViewResult.Handle, 3, ColumnTextWidth);
end;

procedure TRipGrepperForm.SetStatusBarMessage(const _iRipGrepResultOk : Integer = 0);
var
	msg : string;
begin
	if FswSearchStart.IsRunning then begin
		msg := Format('Search took %s seconds with ' + EXE_AND_VERSION_FORMAT, [GetElapsedTime(FswSearchStart), FExeVersion]);
		FStatusBarStatus := IfThen(_iRipGrepResultOk = RIPGREP_ERROR, 'ERROR', 'SUCCES');
	end else begin
		msg := Format(EXE_AND_VERSION_FORMAT, [FExeVersion]);
		FStatusBarStatus := 'READY';
	end;
	FStatusBarMessage := msg;
end;

procedure TRipGrepperForm.SetStatusBarStatistic(const _s : string);
begin
	FStatusBarStatistic := _s;
end;

procedure TRipGrepperForm.UpdateArgumentsAndSettings;
begin
	if FHistObject.RipGrepArguments.Count = 0 then begin
		FHistObject.CopyFromSettings(FSettings);
	end;
end;

procedure TRipGrepperForm.UpdateHistObject;
begin
	FHistObject := GetHistoryObject(CurrentHistoryItemIndex);
	FHistObject.CopyToSettings(FSettings);
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
