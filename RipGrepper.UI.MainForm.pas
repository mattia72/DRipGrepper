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
	Winapi.Windows,
	System.ImageList,
	System.Actions,
	System.Threading,
	Vcl.WinXCtrls;

type
	TRipGrepperForm = class(TForm, INewLineEventHandler, ITerminateEventProducer, IEOFProcessEventHandler)
		panelMain : TPanel;
		lblPaths : TLabel;
		lblParams : TLabel;
		lblText : TLabel;
		btnConfig : TButton;
		ListViewResult : TListView;
		pnlBottom : TPanel;
		btnSearch : TButton;
		btnCancel : TButton;
		ImageListButtons : TImageList;
		ActionList : TActionList;
		ActionSearch : TAction;
		ActionCancel : TAction;
		ActionConfig : TAction;
		cmbSearchDir : TComboBox;
		cmbSearchText : TComboBox;
		cmbParameters : TComboBox;
		StatusBar1 : TStatusBar;
		ActionSwitchView : TAction;
		ActionSortByFile : TAction;
		pnlSearch : TPanel;
		ToolBar1 : TToolBar;
		tbCopyCmdLine : TToolButton;
		tbView : TToolButton;
		gbSearch : TGroupBox;
		tbShowRelativePath : TToolButton;
		ActionShowRelativePath : TAction;
		ToolButton1 : TToolButton;
		ToolButton2 : TToolButton;
		btnCmdLineCopy : TButton;
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
		Panel1 : TPanel;
		Panel2 : TPanel;
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
		procedure ListViewResultColumnClick(Sender : TObject; Column : TListColumn);
		procedure ListViewResultData(Sender : TObject; Item : TListItem);
		procedure ListViewResultDrawItem(Sender : TCustomListView; Item : TListItem; Rect : TRect; State : TOwnerDrawState);

		private
			FdtStartSearch : TDateTime;
			FArguments : TStringList;
			FData : TRipGrepperMatches;
			FExeVersion : string;
			FMaxWidths : TArray<integer>;
			FParserType : TParserType;
			FRecId : Integer;
			FRgExeVersion : string;
			FSearchPathIsDir : Boolean;
			FSettings : TRipGrepperSettings;
			FColumnSortTypes : TArray<TSortType>;
			FRipGrepTask : ITask;
			FStatusBarMessage : string;
			FStatusBarStatus : string;
			FStatusBarStatistic : string;
			FViewStyleIndex : Integer;
			procedure AddToCmbIfNotContains(_cmb : TComboBox);
			procedure AddToListBoxIfNotContains(_lb : TListBox; const _s : string);
			procedure ReBuildArguments;
			procedure ClearData;
			procedure DoSearch;
			function GetAppNameAndVersion(const _exePath : string) : string;
			function GetSortingImageIndex(const _idx : Integer) : Integer;
			function GetViewStyleIndex : Integer;
			procedure InitSettings;
			procedure InitStatusBar;
			procedure LoadSettings;
			procedure PutIntoGroup(const idx : Integer; Item : TListItem);
			function BuildCmdLine : string;
			procedure CopyToClipboardFileOfSelected;
			procedure DataToGrid(const _index : Integer; _item : TListItem);
			procedure DoSortOnColumn(const _sbt : TSortByType);
			function DrawFileIcon(Canvas : TCanvas; Rect : TRect; Item : TListItem) : Vcl.Graphics.TBitmap;
			procedure DrawItemOnBitmap(Sender : TCustomListView; Item : TListItem; Rect : TRect; State : TOwnerDrawState);
			procedure DrawItemOnCanvas(_Canvas : TCanvas; _Rect : TRect; _Item : TListItem; _State : TOwnerDrawState);
			function GetAbsOrRelativePath(const _sFullPath : string) : string;
			function GetIconBitmap(const sFileName : string) : Vcl.Graphics.TBitmap;
			procedure InitColumnSortTypes;
			procedure InitMaxWidths;
			procedure LoadBeforeSearchSettings;
			procedure RunRipGrep;
			procedure SetStatusBarInfo(const _iRipGrepResultOk : Integer = 0);
			procedure SetStatusBarResultText(const _s : string);
			procedure StoreHistories;
			procedure StoreSearchSettings;
			procedure UpdateSortingImages(const _sbtArr : TArray<TSortByType>);
			property ViewStyleIndex : Integer read GetViewStyleIndex;

		protected
			procedure SetResultInHistoryList;

		public
			constructor Create(_settings : TRipGrepperSettings); reintroduce; overload;
			constructor Create(AOwner : TComponent); overload; override;
			destructor Destroy; override;
			procedure CopyToClipboardPathOfSelected;
			class function CreateAndShow(const _settings : TRipGrepperSettings) : string;
			// INewLineEventHandler
			procedure OnNewOutputLine(const _sLine : string);
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

procedure OnNewLine(_handler : INewLineEventHandler; const _sLine : string);
function TerminateProcess(_obj : ITerminateEventProducer) : boolean;

const
	LISTVIEW_TYPES : TArray<TViewStyle> = [vsList, vsIcon, vsReport, vsSmallIcon];
	LISTVIEW_TYPE_TEXTS : TArray<string> = ['List', 'Icon', 'Report', 'SmallIcon'];

var
	Form1 : TRipGrepperForm;

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
	System.Generics.Defaults,
	Vcl.Dialogs,
	Vcl.Clipbrd,
	Winapi.ShellAPI,
	Winapi.CommCtrl,
	System.StrUtils;

const
	IMG_IDX_SHOW_ABS_PATH = 11;
	IMG_IDX_SHOW_RELATIVE_PATH = 12;
	IMG_IDX_SHOW_FILE_ICON_TRUE = 5;
	IMG_IDX_SHOW_FILE_ICON_FALSE = 2;

	IMAGE_IDX_UNSORTED = 3;
	IMAGE_IDX_DESCENDING_SORTED = 4;
	IMAGE_IDX_ASCENDING_SORTED = 5;

	LV_IMAGE_IDX_OK = 0;
	LV_IMAGE_IDX_ERROR = 1;
	LV_IMAGE_IDX_INFO = 2;

	{$R *.dfm}

procedure OnNewLine(_handler : INewLineEventHandler; const _sLine : string);

begin
	_handler.OnNewOutputLine(_sLine);
	TDebugUtils.DebugMessage(string(_sLine));
end;

function TerminateProcess(_obj : ITerminateEventProducer) : boolean;
begin
	Result := _obj.ProcessShouldTerminate();
end;

constructor TRipGrepperForm.Create(_settings : TRipGrepperSettings);
begin
	inherited Create(nil);
	FSettings := _settings;
end;

constructor TRipGrepperForm.Create(AOwner : TComponent);
begin
	inherited Create(AOwner);
	FData := TRipGrepperMatches.Create();
	FExeVersion := GetAppNameAndVersion(Application.ExeName);
	InitColumnSortTypes;
	FParserType := ptRipGrepSearchCutParent;
	FArguments := TStringList.Create();
	FRecId := 0;
	UpdateSortingImages([sbtFile, sbtRow]);
	InitMaxWidths;
end;

destructor TRipGrepperForm.Destroy;
begin
	FData.Free;
	FArguments.Free;
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
	ActionSearchExecute(self);
end;

procedure TRipGrepperForm.ActionDoSearchUpdate(Sender : TObject);
begin
	ActionDoSearch.Enabled := Assigned(FRipGrepTask) and (FRipGrepTask.Status <> TTaskStatus.Running);
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

procedure TRipGrepperForm.ActionShowRelativePathExecute(Sender : TObject);
const
	PARSER_TYPES : TArray<TParserType> = [ptRipGrepSearch, ptRipGrepSearchCutParent];
begin
	FSettings.ShowRelativePath := not FSettings.ShowRelativePath;
	var
	idx := Integer(FSettings.ShowRelativePath);
	FParserType := PARSER_TYPES[idx mod Length(PARSER_TYPES)];
	InitMaxWidths();
	FSettings.StoreViewSettings('ShowRelativePath');
	ListViewResult.Repaint;
end;

procedure TRipGrepperForm.ActionSearchExecute(Sender : TObject);
var
	cursor : TCursorSaver;
begin
	cursor.SetHourGlassCursor;
	ClearData;
	FdtStartSearch := 0;
	InitStatusBar;
	InitColumnSortTypes;
	UpdateSortingImages([sbtFile, sbtRow]);
	ListViewResult.Repaint();
	LoadBeforeSearchSettings();
	StoreHistories();
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

procedure TRipGrepperForm.AddToCmbIfNotContains(_cmb : TComboBox);
var
	idxval : Integer;
	val : string;
begin
	val := _cmb.Text;
	if not _cmb.Items.Contains(val) then begin
		_cmb.Items.Insert(0, val);
	end else begin
		idxval := _cmb.Items.IndexOf(val);
		_cmb.Items.Delete(idxval);
		_cmb.Items.Insert(0, val);
		_cmb.ItemIndex := 0;
	end;
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

procedure TRipGrepperForm.AddToListBoxIfNotContains(_lb : TListBox; const _s : string);
var
	idxval : Integer;
	val : string;
begin
	val := _s;
	if not _lb.Items.Contains(val) then begin
		_lb.Items.InsertObject(0, val, FData);
	end else begin
		idxval := _lb.Items.IndexOf(val);
		_lb.Items.Delete(idxval);
		_lb.Items.InsertObject(0, val, FData);
		_lb.ItemIndex := 0;
	end;

end;

procedure TRipGrepperForm.ReBuildArguments;
const
	NECESSARY_PARAMS : TArray<string> = ['--vimgrep', '--line-buffered' // ,// some big search couldn't be catched without this
	// '--pretty' // TODO: parse color escape
		];
var
	paramsArr : TArray<string>;
	params : string;
begin
	params := cmbParameters.Text;
	FArguments.Clear();

	for var s in NECESSARY_PARAMS do begin
		if not params.Contains(s) then begin
			params := s + ' ' + params;
		end;
	end;

	paramsArr := params.Split([' ']);
	for var s : string in paramsArr do begin
		if not s.IsEmpty then begin
			FArguments.Add(s);
		end;
	end;

	FArguments.Add(cmbSearchText.Text);
	var
	searchPath := TProcessUtils.MaybeQuoteIfNotQuoted(cmbSearchDir.Text);
	FArguments.Add(searchPath);
	FArguments.Delimiter := ' '; // sArgs.QuoteChar := '"';
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
	SetStatusBarResultText('Searching...');
	ReBuildArguments();
	RunRipGrep();
end;

procedure TRipGrepperForm.FormClose(Sender : TObject; var Action : TCloseAction);
begin
	StoreSearchSettings;
end;

procedure TRipGrepperForm.FormShow(Sender : TObject);
begin
	LoadSettings;
	SetStatusBarInfo();
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

procedure TRipGrepperForm.InitSettings;
var
	rgExists : Boolean;
	rgPath : string;
	scoopInstall : string;
begin
	if FSettings.RipGrepPath.IsEmpty or (not FileExists(FSettings.RipGrepPath)) then begin
		rgExists := TFileUtils.FindExecutable('rg.exe', rgPath);
		if not rgExists then begin
			MessageDlg('rg.exe not found', mtError, [mbOk], 0);
			Application.Terminate();
		end;
		scoopInstall := TPath.Combine(GetEnvironmentVariable('SCOOP'), 'apps\ripgrep\current\rg.exe');
		if FileExists(scoopInstall) then begin
			rgPath := scoopInstall;
		end;

		FSettings.RipGrepPath := rgPath.Trim();
	end;

	if FSettings.SearchPaths.Count = 0 then begin
		FSettings.SearchPaths.Add(TDirectory.GetCurrentDirectory());
	end;

	if FSettings.SearchTexts.Count = 0 then begin
		FSettings.SearchTexts.Add('search text');
	end;

	if FSettings.RipGrepParams.Count = 0 then begin
		FSettings.RipGrepParams.Add('');
	end;
end;

procedure TRipGrepperForm.InitStatusBar;
begin
	SetStatusBarInfo();
	SetStatusBarResultText('Ready.');
end;

procedure TRipGrepperForm.LoadSettings;
begin
	FSettings.Load;
	InitSettings;
	cmbSearchDir.Items.Assign(FSettings.SearchPaths);
	LoadBeforeSearchSettings();
	cmbSearchDir.ItemIndex := 0;
	cmbSearchText.Items.Assign(FSettings.SearchTexts);
	cmbSearchText.ItemIndex := 0;
	cmbParameters.Items.Assign(FSettings.RipGrepParams);
	cmbParameters.ItemIndex := 0;
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
	if idx < FData.Count then begin
		DataToGrid(idx, Item);
		SetStatusBarResultText(Format('%d matches in %d files', [FData.Matches.Count, FData.MatchFiles.Count]));
	end;
end;

procedure TRipGrepperForm.OnEOFProcess;
begin
	TDebugUtils.DebugMessage(Format('End of processing rg.exe output in %s sec.', [GetElapsedTime(FdtStartSearch)]));
end;

procedure TRipGrepperForm.OnNewOutputLine(const _sLine : string);
var
	newItem : TRipGrepMatch;
begin
	if _sLine = '' then begin
		exit;
	end;

	if _sLine.StartsWith(':') then begin
		TDebugUtils.DebugMessage('line begins with :');
	end;

	TTask.Run(
		procedure
		begin
			case FParserType of
				ptRipGrepSearch, ptRipGrepSearchCutParent : begin
					newItem.ParseLine(_sLine);
				end;
			end;

			TThread.Synchronize(nil,
				procedure
				begin
					newItem.RecId := PostInc(FRecId);
					FData.Matches.Add(newItem);
					// virtual listview! Items count should be updated
					ListViewResult.Items.Count := FData.Count;
				end);
		end);
end;

procedure TRipGrepperForm.PutIntoGroup(const idx : Integer; Item : TListItem);
var
	m : TRipGrepperMatchCollection;
begin
	m := FData.Matches;
	if FData.MatchFiles.Contains(m[idx].FileName) then begin
		Item.GroupID := m[idx].GroupID;
	end else begin
		var
		Group := ListViewResult.Groups.Add;
		Group.State := [lgsNormal, lgsCollapsible];
		Group.Header := m[idx].FileName;
		var
		match := FData.Matches[idx];
		match.GroupID := Group.GroupID;
		FData.Matches[idx] := match;
		FData.MatchFiles.Add(m[idx].FileName);
	end;
end;

function TRipGrepperForm.BuildCmdLine : string;
var
	cmdLine : TStringList;
begin
	cmdLine := TStringList.Create();
	try
		cmdLine.Add(FSettings.RipGrepPath);
		ReBuildArguments();
		cmdLine.AddStrings(FArguments);
		cmdLine.Delimiter := ' ';
		Result := cmdLine.DelimitedText;
	finally
		cmdLine.Free;
	end;
end;

procedure TRipGrepperForm.DataToGrid(const _index : Integer; _item : TListItem);
var
	m : TRipGrepperMatchCollection;
	fn : string;
begin
	PutIntoGroup(_index, _item);
	m := FData.Matches;
	fn := m[_index].FileName;
	if m[_index].IsError then begin
		_item.Caption := ' ' + fn;
		_item.ImageIndex := LV_IMAGE_IDX_ERROR;
	end else begin
		_item.Caption := fn;
		_item.ImageIndex := LV_IMAGE_IDX_OK;
	end;
	_item.SubItems.Add(m[_index].Row.ToString);
	_item.SubItems.Add(m[_index].Col.ToString);
	_item.SubItems.Add(m[_index].Text);
end;

procedure TRipGrepperForm.DoSortOnColumn(const _sbt : TSortByType);
var
	cursor : TCursorSaver;
	st : TSortType;
begin
	cursor.SetHourGlassCursor();
	try
		st := FColumnSortTypes[integer(_sbt)];
		st := TSortType((Integer(st) + 1) mod 3);
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

procedure TRipGrepperForm.InitMaxWidths;
begin
	if Length(FMaxWidths) = 0 then begin
		for var i := 0 to ListViewResult.Columns.Count - 1 do begin
			FMaxWidths := FMaxWidths + [0];
		end;
	end else begin
		for var i := 0 to ListViewResult.Columns.Count - 1 do begin
			FMaxWidths[i] := 0;
		end;
	end;
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

procedure TRipGrepperForm.DrawItemOnBitmap(Sender : TCustomListView; Item : TListItem; Rect : TRect; State : TOwnerDrawState);
var
	noFlickerBm : Vcl.Graphics.TBitmap;
begin
	noFlickerBm := Vcl.Graphics.TBitmap.Create();
	try
		noFlickerBm.Width := Rect.Right - Rect.Left;
		noFlickerBm.Height := Rect.Bottom - Rect.Top;
		DrawItemOnCanvas(noFlickerBm.Canvas, Rect, Item, State);
		Sender.Canvas.Draw(Rect.Left, Rect.Top, noFlickerBm);
	finally
		noFlickerBm.Free;
	end;
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
				if FSettings.IndentLines then begin
					s := s.TrimLeft;
				end;
			end else begin
				s := _Item.SubItems[i - 1];
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
	SplitView1.Width := ClientWidth;
	StatusBar1.Panels[0].Width := Panel1.Width;
end;

function TRipGrepperForm.GetAbsOrRelativePath(const _sFullPath : string) : string;
begin
	Result := _sFullPath;
	if FSettings.ShowRelativePath and FSearchPathIsDir then begin
		Result := Result.Replace(cmbSearchDir.Text, '.', [rfIgnoreCase]);
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

procedure TRipGrepperForm.ListViewResultDrawItem(Sender : TCustomListView; Item : TListItem; Rect : TRect; State : TOwnerDrawState);
begin
	DrawItemOnCanvas(Sender.Canvas, Rect, Item, State);
	// DrawItemOnBitmap(Sender, Item, Rect, State);
end;

procedure TRipGrepperForm.LoadBeforeSearchSettings;
begin
	FSearchPathIsDir := TDirectory.Exists(FSettings.SearchPaths[0]);
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
			TDebugUtils.DebugMessage('run: ' + FSettings.RipGrepPath + ' ' + FArguments.DelimitedText);
			FdtStartSearch := Now;
			iRipGrepResult := TProcessUtils.RunProcess(FSettings.RipGrepPath, FArguments,
				{ } workDir,
				{ } self as INewLineEventHandler,
				{ } self as ITerminateEventProducer,
				{ } self as IEOFProcessEventHandler);
			SetStatusBarInfo(iRipGrepResult);
			TDebugUtils.DebugMessage(Format('rg.exe ended in %s sec.', [GetElapsedTime(FdtStartSearch)]));
			// SetResultInHistoryList();
		end);
	FRipGrepTask.Start;
end;

procedure TRipGrepperForm.SetResultInHistoryList;
var
	data : TRipGrepperMatches;
	idx : Integer;
	val : string;
begin
	TThread.Synchronize(nil,
		procedure()
		begin
			idx := ListBoxSearchHistory.Itemindex;
			val := ListBoxSearchHistory.Items[idx];
			data := (ListBoxSearchHistory.Items.Objects[idx] as TRipGrepperMatches);
			ListBoxSearchHistory.Items[idx] := Format('%s' + CRLF + '* %d in %d', [val, data.MatchFiles.Count, data.Matches.Count]);
			ListBoxSearchHistory.Items.EndUpdate;
		end);

end;

procedure TRipGrepperForm.SetStatusBarInfo(const _iRipGrepResultOk : Integer = 0);
var
	msg : string;
begin
	if FdtStartSearch <> 0 then begin
		msg := Format('Search took %s seconds with ' + EXE_AND_VERSION_FORMAT, [GetElapsedTime(FdtStartSearch), FExeVersion]);
		FStatusBarStatus := IfThen(_iRipGrepResultOk = TProcessUtils.RIPGREP_ERROR, 'ERROR', 'SUCCES');
	end else begin
		msg := Format(EXE_AND_VERSION_FORMAT, [FExeVersion]);
	end;
	FStatusBarMessage := msg;
end;

procedure TRipGrepperForm.SetStatusBarResultText(const _s : string);
begin
	FStatusBarStatistic := _s;
end;

procedure TRipGrepperForm.StoreHistories;
begin
	AddToCmbIfNotContains(cmbParameters);
	AddToCmbIfNotContains(cmbSearchDir);
	AddToCmbIfNotContains(cmbSearchText);
	AddToListBoxIfNotContains(ListBoxSearchHistory, cmbSearchText.Text);
end;

procedure TRipGrepperForm.StoreSearchSettings;
begin
	FSettings.SearchPaths.Assign(cmbSearchDir.Items);
	FSettings.SearchTexts.Assign(cmbSearchText.Items);
	FSettings.RipGrepParams.Assign(cmbParameters.Items);
	FSettings.Store
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
