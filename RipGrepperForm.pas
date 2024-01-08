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
	RipGrepperSettings;

type
	TRipGrepperForm = class(TForm, INewLineEventHandler)
		panelMain : TPanel;
		Label1 : TLabel;
		LabelParams : TLabel;
		Label3 : TLabel;
		btnConfig : TButton;
		lvResult : TListView;
		pnl_Bottom : TPanel;
		btn_Save : TButton;
		btn_Cancel : TButton;
		ImageListButtons : TImageList;
		alActions : TActionList;
		ActionSearch : TAction;
		ActionCancel : TAction;
		ActionConfig : TAction;
		cmbSearchDir : TComboBox;
		cmbSearchText : TComboBox;
		cmbParameters : TComboBox;
    StatusBar1: TStatusBar;
		procedure ActionCancelExecute(Sender : TObject);
		procedure ActionSearchExecute(Sender : TObject);
		procedure FormShow(Sender : TObject);

		private
			FSettings : TRipGrepperSettings;
			procedure InitSettings;

			{ Private-Deklarationen }
		public
			constructor Create(_settings : TRipGrepperSettings); reintroduce; overload;
			class function CreateAndShow(const _settings : TRipGrepperSettings) : string;
			// INewLineEventHandler
			procedure OnNewResultLine(const _sLine : string);
			{ Public-Deklarationen }
	end;

procedure OnNewLine(_handler : INewLineEventHandler; const _sLine : string);

var
	Form1 : TRipGrepperForm;

implementation

uses
	DebugTools,
	Vcl.Dialogs,
	System.UITypes,
	System.IOUtils,
	System.SysUtils,
	System.Threading;

{$R *.dfm}

procedure OnNewLine(_handler : INewLineEventHandler; const _sLine : string);
begin
	_handler.OnNewResultLine(_sLine);
	TDebugUtils.DebugMessage(string(_sLine));
end;

constructor TRipGrepperForm.Create(_settings : TRipGrepperSettings);
begin
	inherited Create(nil);
	FSettings := _settings
end;

procedure TRipGrepperForm.ActionCancelExecute(Sender : TObject);
begin
	ModalResult := mrCancel;
end;

procedure TRipGrepperForm.ActionSearchExecute(Sender : TObject);
const
	NECESSARY_PARAMS : TArray<string> = ['--vimgrep', '--trim', '--line-buffered'];
var
	sArgs : TStringList;
	paramsArr : TArray<string>;
	params : string;
begin
	sArgs := TStringList.Create();
	try
		params := cmbParameters.Text;

		for var s in NECESSARY_PARAMS do begin
		   if not params.Contains(s) then begin
			   params := s + ' ' + params;
		   end;
		end;

		paramsArr := params.Split([' ']);
		for var s : string in paramsArr do begin
			sArgs.Add(s);
		end;

//		sArgs.Add(TProcessUtils.MaybeQuoteIfNotQuoted(cmbSearchText.Text, ''''));
		sArgs.Add(cmbSearchText.Text);
		var
		workDir := TProcessUtils.MaybeQuoteIfNotQuoted(cmbSearchDir.Text);
		sArgs.Add(workDir);

		sArgs.Delimiter := ' ';
		// sArgs.QuoteChar := '"';

		TDebugUtils.DebugMessage('run: ' + FSettings.RipGrepPath + ' ' + sArgs.DelimitedText);
		lvResult.items.Clear;
		var
		cmd := TProcessUtils.MaybeQuoteIfNotQuoted(FSettings.RipGrepPath) + ' ' + sArgs.DelimitedText;
		// TProcessUtils.GetDosOutput(cmd, workDir, self as INewLineEventHandler);
		TProcessUtils.RunProcess(FSettings.RipGrepPath, sArgs, workDir, self as INewLineEventHandler);
	finally
		sArgs.Free;
	end;
end;

class function TRipGrepperForm.CreateAndShow(const _settings : TRipGrepperSettings) : string;
begin
	var
	form := TRipGrepperForm.Create(_settings);
	try
		if (mrOk = form.ShowModal()) then begin
			Result := form.lvResult.items[form.lvResult.ItemIndex].SubItems[0];
		end;

	finally
		form.Free;
	end;

end;

procedure TRipGrepperForm.FormShow(Sender : TObject);
begin
	InitSettings;

	cmbSearchDir.Text := FSettings.SearchDirs[0];
	cmbSearchText.Text := FSettings.SearchTexts[0];
	cmbParameters.Text := FSettings.RipGrepParams[0];;
end;

procedure TRipGrepperForm.InitSettings;

begin
	if FSettings.RipGrepPath.IsEmpty or (not FileExists(FSettings.RipGrepPath)) then begin
		var
			rgPath : string;
		var
		rgExists := TFileUtils.FindExecutable('rg.exe', rgPath);
		if not rgExists then begin
			MessageDlg('rg.exe not found', mtError, [mbOk], 0);
			Application.Terminate();
		end;

		var
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

procedure TRipGrepperForm.OnNewResultLine(const _sLine : string);
var
	item : TListItem;
	iPosMatch, iPosRow : integer;
	iPosCol : integer;
	sFileName : string;
	sRow : string;
	sCol : string;
	sMatch : string;
begin

	TTask.Run(
		procedure
		begin
			iPosRow := Pos(':', _sLine, 3);
			if iPosRow <> 0 then begin
				// TDebugUtils.DebugMessage(_sLine);
				sFileName := _sLine.Substring(0, iPosRow - 1);
				iPosCol := Pos(':', _sLine, iPosRow + 1);
				sRow := _sLine.Substring(iPosRow,iPosCol - iPosRow - 1);
				iPosMatch := Pos(':', _sLine, iPosCol + 1);
				sCol := _sLine.Substring(iPosCol, iPosMatch - iPosCol - 1);
				sMatch := _sLine.Substring(iPosMatch + 1);
			end else begin
				sFileName := _sLine;
			end;
			TThread.Synchronize(nil,
				procedure
				begin
					lvREsult.BeginInvoke(
						procedure()
						begin
							item := lvResult.items.Add();
							item.Caption := sFileName;
							item.SubItems.Add(sRow);
							item.SubItems.Add(sCol);
							item.SubItems.Add(sMatch);
						end);

				end);

		end);
end;

end.
