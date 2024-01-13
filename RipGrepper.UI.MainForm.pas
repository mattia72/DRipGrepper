unit RipGrepper.UI.MainForm;

interface

uses

	Winapi.Messages,
	System.Variants,
	System.Classes,
	Vcl.Graphics,
	Vcl.Controls,
	Vcl.Forms,
	Vcl.StdCtrls,
	Vcl.ComCtrls,
	Vcl.ExtCtrls,
	System.ImageList,
	Vcl.ImgList,
	System.Actions,
	Vcl.ActnList,
	RipGrepper.Tools.ProcessUtils,
	RipGrepper.Common.Settings,
	RipGrepper.Data.Matches,
	RipGrepper.Common.Types,
	Vcl.ToolWin,
	Vcl.Menus,
	Winapi.Windows;

type
	TRipGrepperForm = class(TForm, INewLineEventHandler)
		panelMain : TPanel;
		lblPaths : TLabel;
		lblParams : TLabel;
		lblText : TLabel;
		btnConfig : TButton;
		lvResult : TListView;
		pnlBottom : TPanel;
		btnSearch : TButton;
		btnCancel : TButton;
		ImageListButtons : TImageList;
		alActions : TActionList;
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
		tbSort : TToolButton;
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
    tbDoSearchCancel: TToolButton;
    ActionDoSearch: TAction;
		procedure ActionCancelExecute(Sender : TObject);
		procedure ActionCmdLineCopyExecute(Sender : TObject);
		procedure ActionConfigExecute(Sender : TObject);
		procedure ActionCopyFileNameExecute(Sender : TObject);
		procedure ActionCopyPathToClipboardExecute(Sender : TObject);
		procedure ActionShowRelativePathExecute(Sender : TObject);
		procedure ActionSearchExecute(Sender : TObject);
		procedure ActionShowRelativePathUpdate(Sender : TObject);
		procedure ActionSortByFileExecute(Sender : TObject);
		procedure ActionSortByFileUpdate(Sender : TObject);
		procedure ActionSortByRowExecute(Sender : TObject);
		procedure ActionSortByRowUpdate(Sender : TObject);
		procedure ActionSwitchViewExecute(Sender : TObject);
		procedure ActionSwitchViewUpdate(Sender : TObject);
		procedure FormClose(Sender : TObject; var Action : TCloseAction);
		procedure FormShow(Sender : TObject);
		procedure lvResultColumnClick(Sender : TObject; Column : TListColumn);
		procedure lvResultData(Sender : TObject; Item : TListItem);
		procedure lvResultDrawItem(Sender : TCustomListView; Item : TListItem; Rect : TRect; State : TOwnerDrawState);

		private
			FArguments : TStringList;
			FData : TRipGrepperMatches;
			FExeVersion : string;
			FMaxWidths : TArray<integer>;
			FParserType : TParserType;
			FRecId : Integer;
			FRgExeVersion : string;
			FSearchPathIsDir : Boolean;
			FSettings : TRipGrepperSettings;
			FShowRelativePath : Boolean;
			FColumnSortTypes : TArray<TSortType>;
			FViewStyleIndex : Integer;
			procedure AddIfNotContains(_cmb : TComboBox);
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
			function GetIconBitmap(const sFileName : string) : Vcl.Graphics.TBitmap;
			procedure InitColumnSortTypes;
			procedure InitMaxWidths;
			procedure RunRipGrep;
			procedure SetStatusBarInfo(const _dtStart : TDateTime = 0);
			procedure SetStatusBarResultText(const _s : string);
			procedure StoreHistories;
			procedure StoreSettings;
			procedure UpdateSortingImages(const _sbtArr : TArray<TSortByType>);
			property ViewStyleIndex : Integer read GetViewStyleIndex;

		public
			constructor Create(_settings : TRipGrepperSettings); reintroduce; overload;
			constructor Create(AOwner : TComponent); overload; override;
			destructor Destroy; override;
			procedure CopyToClipboardPathOfSelected;
			class function CreateAndShow(const _settings : TRipGrepperSettings) : string;
			// INewLineEventHandler
			procedure OnNewOutputLine(const _sLine : string);
	end;

procedure OnNewLine(_handler : INewLineEventHandler; const _sLine : string);

const
	LISTVIEW_TYPES : TArray<TViewStyle> = [vsList, vsIcon, vsReport, vsSmallIcon];
	LISTVIEW_TYPE_TEXTS : TArray<string> = ['List', 'Icon', 'Report', 'SmallIcon'];

var
	Form1 : TRipGrepperForm;

implementation

uses
	RipGrepper.Tools.DebugTools,
	Vcl.Dialogs,
	System.UITypes,
	System.IOUtils,
	System.SysUtils,
	System.Threading,
	Winapi.CommCtrl,
	RipGrepper.Helper.UI,
	RipGrepper.Tools.FileUtils,
	System.Math,
	System.Generics.Defaults,
	Vcl.Clipbrd,
	System.Types,
	Winapi.ShellAPI;

const
	IMG_IDX_SHOW_ABS_PATH = 11;
	IMG_IDX_SHOW_RELATIVE_PATH = 12;
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

procedure TRipGrepperForm.ActionShowRelativePathExecute(Sender : TObject);
const
	PARSER_TYPES : TArray<TParserType> = [ptRipGrepSearch, ptRipGrepSearchCutParent];
begin
	FShowRelativePath := not FShowRelativePath;
	var
	idx := Integer(FShowRelativePath);
	FParserType := PARSER_TYPES[idx mod Length(PARSER_TYPES)];
	InitMaxWidths();
	lvResult.Repaint;
end;

procedure TRipGrepperForm.ActionSearchExecute(Sender : TObject);
var
	cursor : TCursorSaver;
begin
	cursor.SetHourGlassCursor;
	ClearData;
	InitStatusBar;
	InitColumnSortTypes;
	UpdateSortingImages([sbtFile, sbtRow]);
	lvResult.Repaint();
	// btnSort.Repaint();
	DoSearch;
end;

procedure TRipGrepperForm.ActionShowRelativePathUpdate(Sender : TObject);
begin
	tbShowRelativePath.Down := FShowRelativePath;
	ActionShowRelativePath.ImageIndex := Ifthen(FShowRelativePath, IMG_IDX_SHOW_RELATIVE_PATH, IMG_IDX_SHOW_ABS_PATH);
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

procedure TRipGrepperForm.AddIfNotContains(_cmb : TComboBox);
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
	lvResult.ViewStyle := LISTVIEW_TYPES[ViewStyleIndex];
end;

procedure TRipGrepperForm.ActionSwitchViewUpdate(Sender : TObject);
begin
	var
	idx := IfThen((FViewStyleIndex + 1) <= (Length(LISTVIEW_TYPES) - 1), FViewStyleIndex + 1, 0);
	// ActionSwitchView.ImageIndex := idx + 2;
	ActionSwitchView.Hint := 'Change View ' + LISTVIEW_TYPE_TEXTS[idx];
end;

procedure TRipGrepperForm.ReBuildArguments;
const
	NECESSARY_PARAMS : TArray<string> = ['--vimgrep' { , '--line-buffered' } ];
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
	lvResult.Items.Count := 0;
	FData.Clear;
	lvResult.Items.Clear;
end;

class function TRipGrepperForm.CreateAndShow(const _settings : TRipGrepperSettings) : string;
begin
	var
	form := TRipGrepperForm.Create(_settings);
	try
		if (mrOk = form.ShowModal()) then begin
			Result := form.lvResult.Items[form.lvResult.ItemIndex].SubItems[0];
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
	StoreSettings;
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
	FSearchPathIsDir := TDirectory.Exists(FSettings.SearchPaths[0]);
	cmbSearchDir.ItemIndex := 0;
	cmbSearchText.Items.Assign(FSettings.SearchTexts);
	cmbSearchText.ItemIndex := 0;
	cmbParameters.Items.Assign(FSettings.RipGrepParams);
	cmbParameters.ItemIndex := 0;
	FShowRelativePath := FSettings.ShowRelativePath;
end;

procedure TRipGrepperForm.lvResultColumnClick(Sender : TObject; Column : TListColumn);
begin
	case Column.Index of
		0 :
		ActionSortByFileExecute(self);
		1 :
		ActionSortByRowExecute(self);
	end;
end;

procedure TRipGrepperForm.lvResultData(Sender : TObject; Item : TListItem);
var
	idx : Integer;
begin
	idx := Item.Index;
	if FData.Count > idx then begin
		DataToGrid(idx, Item);
		SetStatusBarResultText(Format('%d matches in %d files', [FData.Matches.Count, FData.MatchFiles.Count]));
		TListViewColumnAdjuster.AdjustColumnWidths(lvResult, Item, FMaxWidths);
	end;
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
					lvResult.Items.Count := FData.Count;
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
		Group := lvResult.Groups.Add;
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
	if FShowRelativePath then begin
		fn := fn.Replace(cmbSearchDir.Text, '.', [rfIgnoreCase]);
	end;
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
		lvResult.Repaint();
	end;
end;

procedure TRipGrepperForm.InitColumnSortTypes;
begin
	FColumnSortTypes := [stUnsorted, stUnsorted, stUnsorted, stUnsorted];
end;

procedure TRipGrepperForm.InitMaxWidths;
begin
	if Length(FMaxWidths) = 0 then begin
		for var i := 0 to lvResult.Columns.Count - 1 do begin
			FMaxWidths := FMaxWidths + [0];
		end;
	end else begin
		for var i := 0 to lvResult.Columns.Count - 1 do begin
			FMaxWidths[i] := 0;
		end;
	end;
end;

procedure TRipGrepperForm.CopyToClipboardFileOfSelected;
var
	idx : Integer;
begin
	if not lvResult.TryGetSelected(idx) then
		Exit;
	Clipboard.AsText := TPath.GetFileName(lvResult.Items[idx].Caption);
end;

procedure TRipGrepperForm.CopyToClipboardPathOfSelected;
var
	idx : Integer;
begin
	if not lvResult.TryGetSelected(idx) then
		Exit;
	Clipboard.AsText := TPath.GetFullPath(lvResult.Items[idx].Caption);
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

procedure TRipGrepperForm.lvResultDrawItem(Sender : TCustomListView; Item : TListItem; Rect : TRect; State : TOwnerDrawState);
var
	bm: Vcl.Graphics.TBitmap;
	i : Integer;
	x1, x2 : integer;
	r : TRect;
	s : string;
	sFileName : string;
//    ts: TTextStyle;
const
	DT_ALIGN : array [TAlignment] of integer = (DT_LEFT, DT_RIGHT, DT_CENTER);
begin
	Sender.SetAlteringColors(Item);
	Sender.Canvas.Brush.Style := bsSolid;
	Sender.Canvas.FillRect(Rect);
	x1 := 0;
	x2 := 0;
	r := Rect;
	Sender.Canvas.Brush.Style := bsClear;

	sFileName := item.Caption;
	// SHGetFileInfo(PChar(item.Caption), 0, sfi, SizeOf(sfi), SHGFI_DISPLAYNAME);
	// item.Caption := sfi.szDisplayName;

	bm := GetIconBitmap(sFileName);
	Sender.Canvas.Draw(3, r.Top + (r.Bottom - r.Top - bm.Height) div 2, bm);

	for i := 0 to lvResult.Columns.Count - 1 do begin
		inc(x2, lvResult.Columns[i].Width);
		r.Left := x1;
		r.Right := x2;
		if i = 0 then begin
			s := Item.Caption;
			r.Left := bm.Width + 6;
		end else begin
			s := Item.SubItems[i - 1];
		end;
//        ts.Alignment :=
//        Sender.Canvas.TextRect(
		DrawText(Sender.Canvas.Handle, S, length(s), r,
		{ } DT_SINGLELINE or DT_ALIGN[lvResult.Columns[i].Alignment] or DT_VCENTER or DT_END_ELLIPSIS);
		x1 := x2;
	end;
end;

procedure TRipGrepperForm.RunRipGrep;
var
	dtStart : TDateTime;
	rgResultOk : Boolean;
	workDir : string;
begin
	TTask.Run(
		procedure()
		begin
			workDir := TDirectory.GetCurrentDirectory();
			TDebugUtils.DebugMessage('run: ' + FSettings.RipGrepPath + ' ' + FArguments.DelimitedText);
			dtStart := Now;
			rgResultOk := TProcessUtils.RunProcess(FSettings.RipGrepPath, FArguments, workDir, self as INewLineEventHandler);
			if rgResultOk then begin
				StoreHistories();
				SetStatusBarInfo(dtStart);
			end;
		end);
end;

procedure TRipGrepperForm.SetStatusBarInfo(const _dtStart : TDateTime = 0);
const
	EXE_AND_VERSION_FORMAT = '%s   ';
var
	msg : string;
begin
	if _dtStart <> 0 then begin
		msg := Format('Search took %.2f seconds with ' + EXE_AND_VERSION_FORMAT, [(Now - _dtStart) * 24 * 60 * 60, FExeVersion]);
	end else begin
		msg := Format(EXE_AND_VERSION_FORMAT, [FExeVersion]);
	end;
	StatusBar1.Panels[2].Text := msg;
end;

procedure TRipGrepperForm.SetStatusBarResultText(const _s : string);
begin
	StatusBar1.BeginInvoke(
		procedure()
		begin
			StatusBar1.Panels[0].Text := _s; //
			TStatusBarAdjuster.AutoSizeStatusbarPanel(StatusBar1, 0);
		end);
end;

procedure TRipGrepperForm.StoreHistories;
begin
	AddIfNotContains(cmbParameters);
	AddIfNotContains(cmbSearchDir);
	AddIfNotContains(cmbSearchText);
end;

procedure TRipGrepperForm.StoreSettings;
begin
	FSettings.SearchPaths.Assign(cmbSearchDir.Items);
	FSettings.SearchTexts.Assign(cmbSearchText.Items);
	FSettings.RipGrepParams.Assign(cmbParameters.Items);
	FSettings.ShowRelativePath := FShowRelativePath;
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
		lvResult.Columns[i].ImageIndex := imgIdx;
		case i of
			0 :
			ActionSortByFile.ImageIndex := imgIdx;
			1 :
			ActionSortByRow.ImageIndex := imgIdx;
		end;
	end;
end;

end.
