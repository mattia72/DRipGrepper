unit RipGrepperForm;

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
	ProcessTools,
	RipGrepperSettings,
	System.Generics.Collections;

type

	TStringsHelper = class helper for TStrings
		function Contains(const s: string): Boolean;
	end;

	TRipGrepMatch = record
		FileName: string;
		Row: integer;
		Col: integer;
		Text: string;
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
		procedure ActionCancelExecute(Sender: TObject);
		procedure ActionSearchExecute(Sender: TObject);
		procedure FormClose(Sender: TObject; var Action: TCloseAction);
		procedure FormShow(Sender: TObject);
		procedure lvResultData(Sender: TObject; Item: TListItem);

	private
		FData: TList<TRipGrepMatch>;
		FSettings: TRipGrepperSettings;
		procedure AddIfNotContains(_cmb: TComboBox);
		procedure BuildArgs(var sArgs: TStringList);
		procedure ClearData;
		procedure InitSettings;

	protected
		procedure StoreHistories;

	public
		constructor Create(_settings: TRipGrepperSettings); reintroduce; overload;
		constructor Create(AOwner: TComponent); overload; override;
		destructor Destroy; override;
		class function CreateAndShow(const _settings: TRipGrepperSettings): string;
		// INewLineEventHandler
		procedure OnNewResultLine(const _sLine: string);
	end;

procedure OnNewLine(_handler: INewLineEventHandler; const _sLine: string);

var
	Form1: TRipGrepperForm;

implementation

uses
	DebugTools,
	Vcl.Dialogs,
	System.UITypes,
	System.IOUtils,
	System.SysUtils,
	System.Threading;

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

constructor TRipGrepperForm.Create(AOwner: TComponent);
begin
	inherited Create(AOwner);
	FData := TList<TRipGrepMatch>.Create();
end;

destructor TRipGrepperForm.Destroy;
begin
	inherited;
	FData.Free;
end;

procedure TRipGrepperForm.ActionCancelExecute(Sender: TObject);
begin
	ModalResult := mrCancel;
end;

procedure TRipGrepperForm.ActionSearchExecute(Sender: TObject);
var
	cmd: string;
	sArgs: TStringList;
	rgResultOk: Boolean;
	workDir: string;
begin
	ClearData;
	sArgs := TStringList.Create();
	try

		BuildArgs(sArgs);

		TDebugUtils.DebugMessage('run: ' + FSettings.RipGrepPath + ' ' + sArgs.DelimitedText);

		cmd := TProcessUtils.MaybeQuoteIfNotQuoted(FSettings.RipGrepPath) + ' ' + sArgs.DelimitedText;
		workDir := TDirectory.GetCurrentDirectory();
		rgResultOk := TProcessUtils.RunProcess(FSettings.RipGrepPath, sArgs, workDir, self as INewLineEventHandler);
		if rgResultOk then begin
			StoreHistories();
		end;

	finally
		sArgs.Free;
	end;
end;

procedure TRipGrepperForm.AddIfNotContains(_cmb: TComboBox);
var
	idxval: integer;
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

procedure TRipGrepperForm.BuildArgs(var sArgs: TStringList);
const
	NECESSARY_PARAMS: TArray<string> = ['--vimgrep', '--trim', '--line-buffered'];
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
	FData.Clear;
	lvResult.Items.Clear;
	lvResult.Items.Count := 0;
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

procedure TRipGrepperForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
	FSettings.SearchDirs.Assign(cmbSearchDir.Items);
	FSettings.SearchTexts.Assign(cmbSearchText.Items);
	FSettings.RipGrepParams.Assign(cmbParameters.Items);
	FSettings.Store;
end;

procedure TRipGrepperForm.FormShow(Sender: TObject);
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

procedure TRipGrepperForm.lvResultData(Sender: TObject; Item: TListItem);
begin
	if FData.Count > Item.Index then begin
		Item.Caption := FData[Item.Index].FileName;
		Item.SubItems.Add(FData[Item.Index].Row.ToString);
		Item.SubItems.Add(FData[Item.Index].Col.ToString);
		Item.SubItems.Add(FData[Item.Index].Text);
	end;
end;

procedure TRipGrepperForm.OnNewResultLine(const _sLine: string);
var
	iPosMatch: integer;
	iPosRow: integer;
	iPosCol: integer;
	sFileName: string;
	sRow: string;
	sCol: string;
	sMatch: string;
begin

	TTask.Run(
		procedure
		begin
			iPosRow := Pos(':', _sLine, 3);
			if iPosRow <> 0 then begin
				// TDebugUtils.DebugMessage(_sLine);
				sFileName := _sLine.Substring(0, iPosRow - 1);
				iPosCol := Pos(':', _sLine, iPosRow + 1);
				sRow := _sLine.Substring(iPosRow, iPosCol - iPosRow - 1);
				iPosMatch := Pos(':', _sLine, iPosCol + 1);
				sCol := _sLine.Substring(iPosCol, iPosMatch - iPosCol - 1);
				sMatch := _sLine.Substring(iPosMatch + 1);
			end else begin
				sFileName := _sLine;
			end;
			TThread.Synchronize(nil,
				procedure
				begin
					var
						newItem: TRipGrepMatch;
					newItem.FileName := sFileName;
					newItem.Row := sRow.ToInteger;
					newItem.Col := sCol.ToInteger;
					newItem.Text := sMatch;
					FData.Add(newItem);
					// virtual listview! Items count should be updated
					lvResult.Items.Count := FData.Count;
				end);

		end);
end;

procedure TRipGrepperForm.StoreHistories;
begin
	AddIfNotContains(cmbParameters);
	AddIfNotContains(cmbSearchDir);
	AddIfNotContains(cmbSearchText);
end;

function TStringsHelper.Contains(const s: string): Boolean;
begin
	Result := self.IndexOf(s) <> -1;
end;

end.
