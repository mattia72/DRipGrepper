unit RipGrepper.UI.MainForm;

interface

uses
	Winapi.Windows,
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
	RipGrepperSettings,
	RipGrepperMatches;

type

	TStringsHelper = class helper for TStrings
		function Contains(const s: string): Boolean;
	end;

	TRipGrepperForm = class(TForm, INewLineEventHandler)
		panelMain: TPanel;
		Label1: TLabel;
		LabelParams: TLabel;
		Label3: TLabel;
		btnConfig: TButton;
		lvResult: TListView;
		pnl_Bottom: TPanel;
		btn_Save: TButton;
		btn_Cancel: TButton;
		ImageListButtons: TImageList;
		alActions: TActionList;
		ActionSearch: TAction;
		ActionCancel: TAction;
		ActionConfig: TAction;
		cmbSearchDir: TComboBox;
		cmbSearchText: TComboBox;
		cmbParameters: TComboBox;
		StatusBar1: TStatusBar;
		btnView: TButton;
		ActionSwitchView: TAction;
    btnSort: TButton;
    ActionSort: TAction;
		procedure ActionCancelExecute(Sender: TObject);
		procedure ActionConfigExecute(Sender: TObject);
		procedure ActionSearchExecute(Sender: TObject);
		procedure ActionSortExecute(Sender: TObject);
		procedure ActionSwitchViewExecute(Sender: TObject);
		procedure ActionSwitchViewUpdate(Sender: TObject);
		procedure FormClose(Sender: TObject; var Action: TCloseAction);
		procedure FormShow(Sender: TObject);
		procedure lvResultData(Sender: TObject; Item: TListItem);

		private
			FData : TRipGrepperMatches;
			FExeVersion : string;
		FRgExeVersion: string;
			FSettings : TRipGrepperSettings;
		FViewStyleIndex: Integer;
			procedure AddIfNotContains(_cmb : TComboBox);
			procedure BuildArgs(var sArgs : TStringList);
			procedure ClearData;
		procedure DoSearch;
		function GetAppNameAndVersion(const _exePath: string): string;
		function GetViewStyleIndex: Integer;
			procedure InitSettings;
			procedure InitStatusBar;
			procedure LoadSettings;
			procedure PutIntoGroup(const idx : integer; Item : TListItem);
			procedure SetColumnWidths;
			procedure SetStatusBarInfo(const _dtStart : TDateTime = 0);
			procedure SetStatusBarResultTexts;
			procedure StoreHistories;
			procedure StoreSettings;
		property ViewStyleIndex: Integer read GetViewStyleIndex;

	public
		constructor Create(_settings: TRipGrepperSettings); reintroduce; overload;
		constructor Create(AOwner: TComponent); overload; override;
		destructor Destroy; override;
		class function CreateAndShow(const _settings: TRipGrepperSettings): string;
		// INewLineEventHandler
		procedure OnNewResultLine(const _sLine: string);
	end;

procedure OnNewLine(_handler: INewLineEventHandler; const _sLine: string);

const
	LISTVIEW_TYPES: TArray<TViewStyle> = [vsList, vsIcon, vsReport, vsSmallIcon];
	LISTVIEW_TYPE_TEXTS: TArray<string> = ['List', 'Icon', 'Report', 'SmallIcon'];

var
	Form1: TRipGrepperForm;

implementation

uses
	RipGrepper.Tools.DebugTools,
	Vcl.Dialogs,
	System.UITypes,
	System.IOUtils,
	System.SysUtils,
	System.Threading,
	Winapi.CommCtrl,
	RipGrepper.Helper.CursorSaver,
	RipGrepper.Tools.FileUtils,
	System.Math;

{$R *.dfm}

procedure OnNewLine(_handler: INewLineEventHandler; const _sLine: string);
begin
	_handler.OnNewResultLine(_sLine);
	TDebugUtils.DebugMessage(string(_sLine));
end;

constructor TRipGrepperForm.Create(_settings: TRipGrepperSettings);
begin
	inherited Create(nil);
	FSettings := _settings;
end;

constructor TRipGrepperForm.Create(AOwner : TComponent);
begin
	inherited Create(AOwner);
	FData := TRipGrepperMatches.Create();
	FExeVersion :=GetAppNameAndVersion(Application.ExeName);
end;

destructor TRipGrepperForm.Destroy;
begin
	inherited;
	FData.Free;
end;

procedure TRipGrepperForm.ActionCancelExecute(Sender: TObject);
begin
	ModalResult := mrCancel;
	Close;
end;

procedure TRipGrepperForm.ActionConfigExecute(Sender: TObject);
begin
//
end;

procedure TRipGrepperForm.ActionSearchExecute(Sender: TObject);
var
	cursor: TCursorSaver;
begin
	cursor.SetHourGlassCursor;
	ClearData;
	InitStatusBar;
	DoSearch;
end;

procedure TRipGrepperForm.ActionSortExecute(Sender: TObject);
begin
//
end;

procedure TRipGrepperForm.AddIfNotContains(_cmb: TComboBox);
var
	idxval: Integer;
	val: string;
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

procedure TRipGrepperForm.ActionSwitchViewExecute(Sender: TObject);
begin
	lvResult.ViewStyle := LISTVIEW_TYPES[ViewStyleIndex];
end;

procedure TRipGrepperForm.ActionSwitchViewUpdate(Sender: TObject);
begin
	var
	idx := IfThen((FViewStyleIndex + 1) <= (Length(LISTVIEW_TYPES) - 1), FViewStyleIndex + 1, 0);
	// ActionSwitchView.ImageIndex := idx + 2;
	ActionSwitchView.Hint := 'Change View ' + LISTVIEW_TYPE_TEXTS[idx];
end;

procedure TRipGrepperForm.BuildArgs(var sArgs: TStringList);
const
	NECESSARY_PARAMS: TArray<string> = ['--vimgrep', '--line-buffered'];
var
	paramsArr: TArray<string>;
	params: string;
begin
	params := cmbParameters.Text;

	for var s in NECESSARY_PARAMS do begin
		if not params.Contains(s) then begin
			params := s + ' ' + params;
		end;
	end;

	paramsArr := params.Split([' ']);
	for var s: string in paramsArr do begin
		if not s.IsEmpty then begin
			sArgs.Add(s);
		end;
	end;

	sArgs.Add(cmbSearchText.Text);
	var
	workDir := TProcessUtils.MaybeQuoteIfNotQuoted(cmbSearchDir.Text);
	sArgs.Add(workDir);
	sArgs.Delimiter := ' '; // sArgs.QuoteChar := '"';
end;

procedure TRipGrepperForm.ClearData;
begin
	lvResult.Items.Count := 0;
	FData.Clear;
	lvResult.Items.Clear;
end;

class function TRipGrepperForm.CreateAndShow(const _settings: TRipGrepperSettings): string;
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
var
	cmd: string;
	dtStart: TDateTime;
	sArgs: TStringList;
	rgResultOk: Boolean;
	workDir: string;
begin
	sArgs := TStringList.Create();
	try
		BuildArgs(sArgs);
		TDebugUtils.DebugMessage('run: ' + FSettings.RipGrepPath + ' ' + sArgs.DelimitedText);
		cmd := TProcessUtils.MaybeQuoteIfNotQuoted(FSettings.RipGrepPath) + ' ' + sArgs.DelimitedText;
		workDir := TDirectory.GetCurrentDirectory();
		dtStart := Now;
		rgResultOk := TProcessUtils.RunProcess(FSettings.RipGrepPath, sArgs, workDir, self as INewLineEventHandler);
		if rgResultOk then begin
			StoreHistories();
			SetStatusBarInfo(dtStart);
		end;
	finally
		sArgs.Free;
	end;
end;

procedure TRipGrepperForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
	StoreSettings;
end;

procedure TRipGrepperForm.FormShow(Sender: TObject);
begin
	LoadSettings;
	SetStatusBarInfo();
	FRgExeVersion := GetAppNameAndVersion(FSettings.RipGrepPath);
end;

function TRipGrepperForm.GetAppNameAndVersion(const _exePath: string): string;
var
	major: Cardinal;
	minor: Cardinal;
	build: Cardinal;
	name: string;
begin
	GetProductVersion(_exePath, major, minor, build);
	name := TPath.GetFileNameWithoutExtension(_exePath);
	Result := Format('%s v%d.%d.%d', [name, major, minor, build]);
end;

function TRipGrepperForm.GetViewStyleIndex: Integer;
begin
	FViewStyleIndex := IfThen(FViewStyleIndex < Length(LISTVIEW_TYPES) - 1, FViewStyleIndex + 1);
	Result := (FViewStyleIndex mod Length(LISTVIEW_TYPES));
end;

procedure TRipGrepperForm.InitSettings;
var
	rgExists: Boolean;
	rgPath: string;
	scoopInstall: string;
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

	if FSettings.SearchDirs.Count = 0 then begin
		FSettings.SearchDirs.Add(TDirectory.GetCurrentDirectory());
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
	StatusBar1.Panels[0].Text := '';
end;

procedure TRipGrepperForm.LoadSettings;
begin
	FSettings.Load;
	InitSettings;
	cmbSearchDir.Items.Assign(FSettings.SearchDirs);
	cmbSearchDir.ItemIndex := 0;
	cmbSearchText.Items.Assign(FSettings.SearchTexts);
	cmbSearchText.ItemIndex := 0;
	cmbParameters.Items.Assign(FSettings.RipGrepParams);
	cmbParameters.ItemIndex := 0;
end;

procedure TRipGrepperForm.lvResultData(Sender: TObject; Item: TListItem);
var
	idx: Integer;
	m: TRipGrepperMatchCollection;
begin
	idx := Item.Index;
	if FData.Count > idx then begin
		PutIntoGroup(idx, Item);
		m := FData.Matches;
		Item.Caption := m[idx].FileName;
		Item.SubItems.Add(m[idx].Row.ToString);
		Item.SubItems.Add(m[idx].Col.ToString);
		Item.SubItems.Add(m[idx].Text);
		SetStatusBarResultTexts();
		SetColumnWidths;
	end;
end;

procedure TRipGrepperForm.OnNewResultLine(const _sLine: string);
var
	newItem: TRipGrepMatch;
begin

	TTask.Run(
		procedure
		begin
			newItem.ParseLine(_sLine);
			TThread.Synchronize(nil,
				procedure
				begin
					FData.Matches.Add(newItem);
					// virtual listview! Items count should be updated
					lvResult.Items.Count := FData.Count;
				end);

		end);
end;

procedure TRipGrepperForm.PutIntoGroup(const idx: Integer; Item: TListItem);
var
	m: TRipGrepperMatchCollection;
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
	// lvResult.GroupView := FData.Groups.Count > 0;
end;

procedure TRipGrepperForm.SetColumnWidths;
begin
	for var i := 0 to lvResult.Columns.Count - 1 do begin
		lvResult.Columns[i].Width := LVSCW_AUTOSIZE or LVSCW_AUTOSIZE_USEHEADER;
	end;
end;

procedure TRipGrepperForm.SetStatusBarInfo(const _dtStart: TDateTime = 0);
const
	EXE_AND_VERSION_FORMAT = '%s   ';
var
	msg: string;
begin
	if _dtStart <> 0 then begin
		msg := Format('Search took %.2f seconds with ' + EXE_AND_VERSION_FORMAT, [(Now - _dtStart) * 24 * 60 * 60, FExeVersion]);
	end else begin
		msg := Format(EXE_AND_VERSION_FORMAT, [FExeVersion]);
	end;
	StatusBar1.Panels[2].Text := msg;
end;

procedure TRipGrepperForm.SetStatusBarResultTexts;
begin
	StatusBar1.BeginInvoke(
		procedure()
		begin
			StatusBar1.Panels[0].Text := Format('%d matches in %d files', [FData.Matches.Count, FData.MatchFiles.Count])
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
	FSettings.SearchDirs.Assign(cmbSearchDir.Items);
	FSettings.SearchTexts.Assign(cmbSearchText.Items);
	FSettings.RipGrepParams.Assign(cmbParameters.Items);
	FSettings.Store
end;

function TStringsHelper.Contains(const s: string): Boolean;
begin
	Result := self.IndexOf(s) <> -1;
end;

end.
