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
	System.Diagnostics;

type
	TRipGrepperForm = class(TForm, INewLineEventHandler, ITerminateEventProducer, IEOFProcessEventHandler)
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
		ActionDoSearch : TAction;
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
		procedure ActionConfigExecute(Sender : TObject);
		procedure ActionCopyFileNameExecute(Sender : TObject);
		procedure ActionCopyPathToClipboardExecute(Sender : TObject);
		procedure ActionDoSearchExecute(Sender : TObject);
		procedure ActionDoSearchUpdate(Sender : TObject);
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
		procedure ListBoxSearchHistoryDrawItem(Control : TWinControl; Index : Integer; Rect : TRect; State : TOwnerDrawState);
		procedure ListViewResultColumnClick(Sender : TObject; Column : TListColumn);
		procedure ListViewResultData(Sender : TObject; Item : TListItem);
		procedure ListViewResultDrawItem(Sender : TCustomListView; Item : TListItem; Rect : TRect; State : TOwnerDrawState);

		private
			FswSearchStart : TStopwatch;
			FData : TRipGrepOutput;
			FExeVersion : string;
			FMaxWidths : TArray<integer>;
			FFileNameType : TFileNameType;
			FLineNr : Integer;
			FRgExeVersion : string;
			FSearchPathIsDir : Boolean;
			FSettings : TRipGrepperSettings;
			FColumnSortTypes : TArray<TSortDirectionType>;
			FMeassureFirstDrawEvent : Boolean;
			FRipGrepTask : ITask;
			FStatusBarMessage : string;
			FStatusBarStatus : string;
			FStatusBarStatistic : string;
			FViewStyleIndex : Integer;
			procedure ClearData;
			procedure DoSearch;
			function GetAppNameAndVersion(const _exePath : string) : string;
			function GetSortingImageIndex(const _idx : Integer) : Integer;
			function GetViewStyleIndex : Integer;
			procedure InitStatusBar;
			procedure LoadSettings;
			function BuildCmdLine : string;
			procedure CopyToClipboardFileOfSelected;
			procedure DoSortOnColumn(const _sbt : TSortByType);
			function DrawFileIcon(Canvas : TCanvas; Rect : TRect; Item : TListItem) : Vcl.Graphics.TBitmap;
			procedure DrawItemOnCanvas(_Canvas : TCanvas; _Rect : TRect; _Item : TListItem; _State : TOwnerDrawState);
			function GetAbsOrRelativePath(const _sFullPath : string) : string;
			function GetIconBitmap(const sFileName : string) : Vcl.Graphics.TBitmap;
			procedure InitColumnSortTypes;
			procedure LoadBeforeSearchSettings;
			procedure RunRipGrep;
			procedure SetStatusBarMessage(const _iRipGrepResultOk : Integer = 0);
			procedure SetStatusBarStatistic(const _s : string);
			procedure UpdateSortingImages(const _sbtArr : TArray<TSortByType>);
			property ViewStyleIndex : Integer read GetViewStyleIndex;

		protected
		public
			constructor Create(_settings : TRipGrepperSettings); reintroduce; overload;
			constructor Create(AOwner : TComponent); overload; override;
			destructor Destroy; override;
			procedure CopyToClipboardPathOfSelected;
			class function CreateAndShow(const _settings : TRipGrepperSettings) : string;
			// INewLineEventHandler
			procedure OnNewOutputLine(const _sLine : string; _bIsLast : Boolean = False);
			// ITerminateEventProducer
			function ProcessShouldTerminate : boolean;
			// IEOFProcessEventHandler
			procedure OnEOFProcess();

		const
			EXE_AND_VERSION_FORMAT = '%s   ';
			PNL_STATTS_IDX = 0;
			PNL_STATUS_IDX = 1;
			PNL_MESSAGE_IDX = 2;
	end;

const
	LISTVIEW_TYPES : TArray<TViewStyle> = [vsList, vsIcon, vsReport, vsSmallIcon];
	LISTVIEW_TYPE_TEXTS : TArray<string> = ['List', 'Icon', 'Report', 'SmallIcon'];

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
	RipGrepper.UI.SearchForm, RipGrepper.Data.Parsers;

{$R *.dfm}

constructor TRipGrepperForm.Create(_settings : TRipGrepperSettings);
begin
	inherited Create(nil);
	FSettings := _settings;
end;

constructor TRipGrepperForm.Create(AOwner : TComponent);
begin
	inherited Create(AOwner);
	FData := TRipGrepOutput.Create();
	FExeVersion := GetAppNameAndVersion(Application.ExeName);
	InitColumnSortTypes;
	FFileNameType := ftAbsolute;
	FLineNr := 0;
	UpdateSortingImages([sbtFile, sbtRow]);
	ListViewResult.InitMaxWidths(FMaxWidths);
end;

destructor TRipGrepperForm.Destroy;
begin
	FData.Free;
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
	ActionAbortSearch.Enabled := Assigned(FRipGrepTask) and (FRipGrepTask.Status = TTaskStatus.Running);
end;

procedure TRipGrepperForm.ActionAlternateRowColorsExecute(Sender : TObject);
begin
	FSettings.AlternateRowColors := not FSettings.AlternateRowColors;
	FSettings.StoreViewSettings('AlternateRowColors');
	ListViewResult.Repaint();
end;

procedure TRipGrepperForm.ActionAlternateRowColorsUpdate(Sender : TObject);
begin
	tbAlternateRowColors.Down := FSettings.AlternateRowColors;
end;

procedure TRipGrepperForm.ActionCancelExecute(Sender : TObject);
begin
	ModalResult := mrCancel;
	Close;
end;

procedure TRipGrepperForm.ActionCmdLineCopyExecute(Sender : TObject);
begin
	ClipBoard.AsText := BuildCmdLine;
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

procedure TRipGrepperForm.ActionDoSearchExecute(Sender : TObject);
begin
	var
	frm := TRipGrepperSearchDialogForm.Create(self, @FSettings);
	try
		if (mrOk = frm.ShowModal) then begin
			ActionSearchExecute(self);
		end;

	finally
		frm.Free;
	end;
end;

procedure TRipGrepperForm.ActionDoSearchUpdate(Sender : TObject);
begin
	ActionDoSearch.Enabled := FSettings.IsEmpty or (Assigned(FRipGrepTask) and (FRipGrepTask.Status <> TTaskStatus.Running));
end;

procedure TRipGrepperForm.ActionIndentLineExecute(Sender : TObject);
begin
	FSettings.IndentLines := not FSettings.IndentLines;
	FSettings.StoreViewSettings('IndentLines');
	ListViewResult.Repaint();
end;

procedure TRipGrepperForm.ActionIndentLineUpdate(Sender : TObject);
begin
	tbIndentLines.Down := FSettings.IndentLines;
end;

procedure TRipGrepperForm.ActionRefreshSearchExecute(Sender : TObject);
begin
	ActionSearchExecute(self);
end;

procedure TRipGrepperForm.ActionRefreshSearchUpdate(Sender : TObject);
begin
	ActionRefreshSearch.Enabled := FSettings.IsLoaded;
end;

procedure TRipGrepperForm.ActionShowRelativePathExecute(Sender : TObject);
const
	PARSER_TYPES : TArray<TFileNameType> = [ftAbsolute, ftRelative];
begin
	FSettings.ShowRelativePath := not FSettings.ShowRelativePath;
	var
	idx := Integer(FSettings.ShowRelativePath);
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
	ClearData;
	FswSearchStart := TStopwatch.Create();
	FMeassureFirstDrawEvent := True;
	InitStatusBar;
	InitColumnSortTypes;
	UpdateSortingImages([sbtFile, sbtRow]);
	ListViewResult.Repaint();
	LoadBeforeSearchSettings();
	TItemInserter.AddToListBoxIfNotContains(ListBoxSearchHistory, FSettings.ActualSearchText, FData);
	DoSearch();
end;

procedure TRipGrepperForm.ActionShowFileIconsExecute(Sender : TObject);
begin
	FSettings.ShowFileIcon := not FSettings.ShowFileIcon;
	FSettings.StoreViewSettings('ShowFileIcon');
	ListViewResult.Repaint();
end;

procedure TRipGrepperForm.ActionShowFileIconsUpdate(Sender : TObject);
begin
	tbShowFileIcon.Down := FSettings.ShowFileIcon;
	// ActionShowFileIcons.ImageIndex := Ifthen(FSettings.ShowFileIcon, IMG_IDX_SHOW_FILE_ICON_TRUE, IMG_IDX_SHOW_FILE_ICON_FALSE);
end;

procedure TRipGrepperForm.ActionShowRelativePathUpdate(Sender : TObject);
begin
	tbShowRelativePath.Down := FSettings.ShowRelativePath;
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

procedure TRipGrepperForm.ClearData;
begin
	ListViewResult.Items.Count := 0;
	FData.Clear;
	ListViewResult.Items.Clear;
end;

class function TRipGrepperForm.CreateAndShow(const _settings : TRipGrepperSettings) : string;
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
	FSettings.ReBuildArguments;
	RunRipGrep();
end;

procedure TRipGrepperForm.FormClose(Sender : TObject; var Action : TCloseAction);
begin
	// StoreSearchSettings;
end;

procedure TRipGrepperForm.FormShow(Sender : TObject);
begin
	LoadSettings;
	SetStatusBarMessage();
	FRgExeVersion := GetAppNameAndVersion(FSettings.RipGrepPath);
end;

function TRipGrepperForm.GetAppNameAndVersion(const _exePath : string) : string;
var
	major : Cardinal;
	minor : Cardinal;
	build : Cardinal;
	name : string;
begin
	GetProductVersion(_exePath, major, minor, build);
	name := TPath.GetFileNameWithoutExtension(_exePath);
	Result := Format('%s v%d.%d.%d', [name, major, minor, build]);
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

procedure TRipGrepperForm.OnNewOutputLine(const _sLine : string; _bIsLast : Boolean = False);
var
	newItem : IRipGrepMatchLineGroup;
begin
	TTask.Run(
		procedure
		begin
			if (not _sLine.IsEmpty) then begin
				newItem := TRipGrepMatchLine.Create();
				case FFileNameType of
					ftAbsolute, ftRelative : begin
						newItem.ParseLine(PostInc(FLineNr), _sLine, _bIsLast);
					end;
				end;
			end;

			TThread.Synchronize(nil,
				procedure
				begin
					if (not _sLine.IsEmpty) then
						FData.Add(newItem);
					if ((FLineNr mod DRAW_RESULT_ON_EVERY_LINE_COUNT) = 0) or _bIsLast then begin
						// virtual listview! Items count should be updated
						ListViewResult.Items.Count := FData.TotalMatchCount;
						SetStatusBarStatistic(Format('%d matches in %d files', [FData.TotalMatchCount, FData.FileCount]));
						ListBoxSearchHistory.Refresh;
						ListViewResult.AdjustColumnWidths(FMaxWidths);
					end;
				end);
		end);

	if _bIsLast then begin
		TDebugUtils.DebugMessage(Format('Last line received in %s sec.', [GetElapsedTime(FswSearchStart)]));
	end;
end;

function TRipGrepperForm.BuildCmdLine : string;
var
	cmdLine : TStringList;
begin
	cmdLine := TStringList.Create();
	try
		cmdLine.Add(FSettings.RipGrepPath);
		cmdLine.AddStrings(FSettings.ReBuildArguments);
		cmdLine.Delimiter := ' ';
		Result := cmdLine.DelimitedText;
	finally
		cmdLine.Free;
	end;
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

function TRipGrepperForm.DrawFileIcon(Canvas : TCanvas; Rect : TRect; Item : TListItem) : Vcl.Graphics.TBitmap;
var
	bm : Vcl.Graphics.TBitmap; // ImageFileIcon
	sFileName : string;
begin
	sFileName := item.Caption;
	// SHGetFileInfo(PChar(item.Caption), 0, sfi, SizeOf(sfi), SHGFI_DISPLAYNAME);
	// item.Caption := sfi.szDisplayName;
	bm := GetIconBitmap(sFileName);
	Canvas.Draw(Rect.Left + 3, Rect.Top + (Rect.Bottom - Rect.Top - bm.Height) div 2, bm);
	Result := bm;
end;

procedure TRipGrepperForm.DrawItemOnCanvas(_Canvas : TCanvas; _Rect : TRect; _Item : TListItem; _State : TOwnerDrawState);
var
	bm : Vcl.Graphics.TBitmap;
	i : Integer;
	x1 : integer;
	x2 : integer;
	r : TRect;
	s : string;
const
	DT_ALIGN : array [TAlignment] of TTextFormats = (
	{ } tfLeft,
	{ } tfRight,
	{ } tfCenter);
begin
	if (FSettings.AlternateRowColors) then begin
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
	if FSettings.ShowFileIcon then begin
		bm := DrawFileIcon(_Canvas, r, _Item);
	end;

	for i := 0 to ListViewResult.Columns.Count - 1 do begin
		inc(x2, ListView_GetColumnWidth(ListViewResult.Handle, ListViewResult.Columns[i].Index));
		r.Left := x1;
		r.Right := x2;
		if i = 0 then begin
			s := GetAbsOrRelativePath(_Item.Caption);
			r.Left := r.Left + 6;
			if Assigned(bm) then begin
				r.Left := r.Left + bm.Width;
			end;
		end else begin
			if i = 3 then begin
				s := _Item.SubItems[i - 1];
				if not FSettings.IndentLines then begin
					s := s.TrimLeft;
				end;
			end else begin
				s := _Item.SubItems[i - 1]; //Row, Col
				if (s = '-1') then
					s := '';
			end;
		end;
		_Canvas.TextRect(r, s, [tfSingleLine, DT_ALIGN[ListViewResult.Columns[i].Alignment], tfVerticalCenter, tfEndEllipsis]);
		x1 := x2;
	end;
end;

procedure TRipGrepperForm.FormResize(Sender : TObject);
begin
	SplitView1.Width := panelMain.Width;
	StatusBar1.Panels[0].Width := PanelHistory.Width;
end;

function TRipGrepperForm.GetAbsOrRelativePath(const _sFullPath : string) : string;
begin
	Result := _sFullPath;
	if FSettings.ShowRelativePath and FSearchPathIsDir then begin
		Result := Result.Replace(FSettings.ActualSearchPath, '.', [rfIgnoreCase]);
	end;
end;

function TRipGrepperForm.GetIconBitmap(const sFileName : string) : Vcl.Graphics.TBitmap;
var
	sfi : TSHFileInfo;
	icon : TIcon;
begin
	icon := TIcon.Create;
	try
		SHGetFileInfo(PChar(sFileName), 0, sfi, SizeOf(TSHFileInfo), SHGFI_SMALLICON or SHGFI_ICON);
		icon.Handle := sfi.hIcon;
		ImageFileIcon.Picture.Bitmap.Assign(Icon);
		Result := ImageFileIcon.Picture.Bitmap;
	finally
		icon.Free;
	end;
end;

procedure TRipGrepperForm.ListBoxSearchHistoryDrawItem(Control : TWinControl; index : Integer; Rect : TRect; State : TOwnerDrawState);
var
	c2ndRowTop : Integer;
	cMatchesLeft : Integer;
	cnv : TCanvas;
	data : TRipGrepOutput;
	lb : TListBox;
	r2ndRow : TRect;
begin
	lb := (Control as TListBox);
	cnv := lb.Canvas;
	cnv.SetSelectedColors(State);

	c2ndRowTop := cnv.TextHeight(SAllAlphaNumericChars);
	cMatchesLeft := 20;
	r2ndRow := Rect;
	r2ndRow.Offset(0, c2ndRowTop);

	cnv.FillRect(TRect.Union(Rect, r2ndRow));
	cnv.TextOut(Rect.Left + 1, Rect.Top + 1, lb.Items[index]);
	cnv.TextOut(Rect.Left + 1, Rect.Top + c2ndRowTop, '�');

	data := lb.Items.Objects[index] as TRipGrepOutput;
	cnv.TextOut(Rect.Left + cMatchesLeft, Rect.Top + c2ndRowTop, Format('(%d in %d)', [data.TotalMatchCount, data.FileCount]))
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

procedure TRipGrepperForm.RunRipGrep;
var
	iRipGrepResult : Integer;
	workDir : string;
begin
	FRipGrepTask := TTask.Create(
		procedure()
		begin
			workDir := TDirectory.GetCurrentDirectory();
			TDebugUtils.DebugMessage('run: ' + FSettings.RipGrepPath + ' ' + FSettings.RipGrepArguments.DelimitedText);
			FswSearchStart := TStopwatch.StartNew;
			iRipGrepResult := TProcessUtils.RunProcess(FSettings.RipGrepPath, FSettings.RipGrepArguments,
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

procedure TRipGrepperForm.SetStatusBarMessage(const _iRipGrepResultOk : Integer = 0);
var
	msg : string;
begin
	TThread.Synchronize(nil,
		procedure
		begin
			if FswSearchStart.IsRunning then begin
				msg := Format('Search took %s seconds with ' + EXE_AND_VERSION_FORMAT, [GetElapsedTime(FswSearchStart), FExeVersion]);
				FStatusBarStatus := IfThen(_iRipGrepResultOk = RIPGREP_ERROR, 'ERROR', 'SUCCES');
			end else begin
				msg := Format(EXE_AND_VERSION_FORMAT, [FExeVersion]);
				FStatusBarStatus := 'READY';
			end;
			FStatusBarMessage := msg;
		end);
end;

procedure TRipGrepperForm.SetStatusBarStatistic(const _s : string);
begin
	TThread.Synchronize(nil,
		procedure
		begin
			FStatusBarStatistic := _s;
		end);
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
