unit RipGrepper.UI.Settings.AdvancedForm;

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
	Vcl.ExtCtrls,
	Vcl.StdCtrls,
	RipGrepper.Settings.Persistable,
	RipGrepper.Settings.AppSettings,
	RipGrepper.UI.SettingsFormBase,
	RipGrepper.UI.ColorSelectorFrame,
	RipGrepper.Settings.FontColors,
	RipGrepper.Settings.RipGrepperSettings,
	RTTI,
	Vcl.Mask,
	RipGrepper.Settings.RipGrepParameterSettings,
	System.ImageList,
	Vcl.ImgList,
	System.Actions,
	Vcl.ActnList,
	RipGrepper.Tools.DebugUtils,
	RipGrepper.Common.Constants,
	SVGIconImageListBase,
	SVGIconImageList,
	Spring,
	Vcl.Samples.Spin,
	Vcl.ControlList,
	RipGrepper.Common.SimpleTypes;

type
	EValidateCtrls = (vcRgExePath, vcIniFilePath);

	TAdvancedForm = class(TSettingsBaseForm)
		chBegin : TCheckBox;
		chExpertMode : TCheckBox;
		grpAdvanced : TGroupBox;
		OpenDialog1 : TOpenDialog;
		ActionList1 : TActionList;
		ActionOpenFileDialog : TAction;
		btnedtIniFilePath : TButtonedEdit;
		Label1 : TLabel;
		chEnd : TCheckBox;
		gbTrace : TGroupBox;
		chError : TCheckBox;
		chWarning : TCheckBox;
		chInfo : TCheckBox;
		chRegex : TCheckBox;
		edtRegex : TEdit;
		SVGIconImageList1 : TSVGIconImageList;
		chVerbose : TCheckBox;
		ScrollBox1 : TScrollBox;
		lblTraceOutput : TLabel;
		chTraceToDebugView : TCheckBox;
		chTraceToFile : TCheckBox;
		lblLogFilePath : TLabel;
		lblLogCreation : TLabel;
		edtLogFilePath : TButtonedEdit;
		cmbLogCreation : TComboBox;
		procedure btnedtIniFilePathEnter(Sender : TObject);
		procedure btnedtIniFilePathExit(Sender : TObject);
		procedure btnedtIniFilePathLeftButtonClick(Sender : TObject);
		procedure btnedtIniFilePathRightButtonClick(Sender : TObject);
		procedure chRegexClick(Sender : TObject);
		procedure chTraceToFileClick(Sender : TObject);
		procedure edtLogFilePathRightButtonClick(Sender : TObject);
		procedure FormShow(Sender : TObject);

		private

			FRefocusing : TObject;
			FAppSettings : TAppSettings;
			FRipGrepSettings : TRipGrepParameterSettings;
			function GetTraceTypeFilters : TTraceFilterTypes;
			procedure UpdateLogFileControlsState();
			{ User-defined message handler }
			procedure ValidateInput(var M : TMessage); message USERMESSAGE_VALIDATE_INPUT;

		protected
			procedure OnSettingsUpdated(); override;
			procedure ReadSettings; override;
			procedure WriteSettings; override;

		public
			constructor Create(_Owner : TComponent; _settings : TRipGrepperSettings);

		published
	end;

var
	AdvancedForm : TAdvancedForm;

implementation

uses

	RipGrepper.Tools.FileUtils,
	RipGrepper.Helper.UI,
	System.IOUtils,
	RipGrepper.Tools.ProcessUtils,
	RipGrepper.OpenWith.Params,
	RipGrepper.OpenWith,
	RipGrepper.Helper.UI.DarkMode,
	System.StrUtils,
	RipGrepper.Helper.Types,
	RipGrepper.Common.LoadHistoryMode;

{$R *.dfm}

constructor TAdvancedForm.Create(_Owner : TComponent; _settings : TRipGrepperSettings);
begin
	inherited Create(_Owner, _settings);
	Caption := 'General';
	FAppSettings := (FSettings as TRipGrepperSettings).AppSettings;
	FRipGrepSettings := (FSettings as TRipGrepperSettings).RipGrepParameters;
end;

procedure TAdvancedForm.btnedtIniFilePathEnter(Sender : TObject);
begin
	if FRefocusing = btnedtIniFilePath then
		FRefocusing := nil;
end;

procedure TAdvancedForm.btnedtIniFilePathExit(Sender : TObject);
begin
	if FRefocusing = nil then
		PostMessage(Handle, USERMESSAGE_VALIDATE_INPUT, 0, LParam(vcIniFilePath));
end;

procedure TAdvancedForm.btnedtIniFilePathLeftButtonClick(Sender : TObject);
begin
	FSettings.ReLoadFromFile();
	ReadSettings;
	TMsgBox.ShowInfo('Settings reloaded from disk.');
end;

procedure TAdvancedForm.btnedtIniFilePathRightButtonClick(Sender : TObject);
var
	owp : TOpenWithParams;
begin
	owp.FilePath := btnedtIniFilePath.Text;
	owp.Row := 0;
	owp.Column := 0;
	TOpenWith.Execute(owp);
end;

procedure TAdvancedForm.chRegexClick(Sender : TObject);
begin
	edtRegex.Enabled := chRegex.Checked;
	for var ch in [chError, chWarning, chInfo, chVerbose, chBegin, chEnd] do begin
		ch.Enabled := not chRegex.Checked;
		if chRegex.Checked then begin
			ch.Checked := False;
		end;
	end;
end;

procedure TAdvancedForm.FormShow(Sender : TObject);
begin
	btnedtIniFilePath.LeftButton.Hint := 'Reload settings from disk';
	btnedtIniFilePath.RightButton.Hint := 'Open with...';

	cmbLogCreation.Items.Clear;
	for var mode := Low(ELogFileCreationMode) to High(ELogFileCreationMode) do begin
		cmbLogCreation.Items.Add(LOG_FILE_CREATION_MODE_NAMES[mode]);
	end;

	ReadSettings;
end;

procedure TAdvancedForm.chTraceToFileClick(Sender : TObject);
begin
	UpdateLogFileControlsState();
end;

procedure TAdvancedForm.UpdateLogFileControlsState();
begin
	edtLogFilePath.Enabled := chTraceToFile.Checked;
	cmbLogCreation.Enabled := chTraceToFile.Checked;
	lblLogFilePath.Enabled := chTraceToFile.Checked;
	lblLogCreation.Enabled := chTraceToFile.Checked;
end;

procedure TAdvancedForm.edtLogFilePathRightButtonClick(Sender : TObject);
var
	dlg : TSaveDialog;
begin
	dlg := TSaveDialog.Create(nil);
	try
		dlg.Title := 'Select log file path';
		dlg.Filter := 'Log files (*.log)|*.log|All files (*.*)|*.*';
		dlg.DefaultExt := 'log';
		if edtLogFilePath.Text <> '' then begin
			dlg.FileName := edtLogFilePath.Text;
		end;
		dlg.Options := dlg.Options + [ofOverwritePrompt];
		if dlg.Execute then begin
			edtLogFilePath.Text := dlg.FileName;
		end;
	finally
		dlg.Free;
	end;
end;

function TAdvancedForm.GetTraceTypeFilters : TTraceFilterTypes;
begin
	Result := [];
	if (chInfo.Checked) then begin
		Result := Result + [tftInfo];
	end;
	if (chVerbose.Checked) then begin
		Result := Result + [tftVerbose];
	end;
	if (chWarning.Checked) then begin
		Result := Result + [tftWarning];
	end;
	if (chError.Checked) then begin
		Result := Result + [tftError];
	end;
	if (chBegin.Checked) then begin
		Result := Result + [tftBegin];
	end;
	if (chEnd.Checked) then begin
		Result := Result + [tftEnd];
	end;
	if (chRegex.Checked) then begin
		Result := Result + [tftRegex];
		FAppSettings.DebugTraceRegexFilter := edtRegex.Text;
	end else begin
	end;
end;

procedure TAdvancedForm.OnSettingsUpdated();
begin
	// here you can update things depending on changed settings
	TDebugUtils.UpdateTraceActive;
end;

procedure TAdvancedForm.ReadSettings;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TAdvancedForm.ReadSettings');
	FReadSettingsCalled := True;

	FAppSettings.LoadFromDict;
	FRipGrepSettings.LoadFromDict;

	var
	tts := TDebugUtils.StrToTraceTypes(FAppSettings.DebugTrace);
	chError.Checked := tftError in tts;
	chWarning.Checked := tftWarning in tts;
	chInfo.Checked := tftInfo in tts;
	chVerbose.Checked := tftVerbose in tts;
	chBegin.Checked := tftBegin in tts;
	chEnd.Checked := tftEnd in tts;
	chRegex.Checked := tftRegex in tts;
	edtRegex.Text := FAppSettings.DebugTraceRegexFilter;
	edtRegex.Enabled := chRegex.Checked;

	chExpertMode.Checked := FAppSettings.IsExpertMode;
	btnedtIniFilePath.Text := FAppSettings.PersisterFactory.FilePath;

	var
	logDestinations := FAppSettings.LogDestinations;
	chTraceToDebugView.Checked := ldOutputDebugString in logDestinations;
	chTraceToFile.Checked := ldFile in logDestinations;

	edtLogFilePath.Text := FAppSettings.LogFilePath;
	if edtLogFilePath.Text = '' then begin
		edtLogFilePath.Text := TDebugUtils.LogFilePath;
	end;
	cmbLogCreation.ItemIndex := Ord(FAppSettings.LogFileCreationMode);

	UpdateLogFileControlsState();

	FAppSettings.UpdateInternalsFromSettings();
end;

procedure TAdvancedForm.ValidateInput(var M : TMessage);
begin
	case EValidateCtrls(M.LParam) of
		vcIniFilePath : begin // not used: ini file location can not be changed
			if not FileExists(btnedtIniFilePath.Text) then begin
				FRefocusing := btnedtIniFilePath;
				TMsgBox.ShowError('Ini file path not valid!');
				btnedtIniFilePath.SetFocus;
			end;
		end;
	end;
end;

procedure TAdvancedForm.WriteSettings;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TAdvancedForm.WriteSettings');
	FAppSettings.DebugTrace := TDebugUtils.TraceTypesToStr(GetTraceTypeFilters());
	FAppSettings.ExpertMode.Value := chExpertMode.Checked;
	FAppSettings.LogFilePath := edtLogFilePath.Text;
	FAppSettings.LogFileCreationMode := ELogFileCreationMode(cmbLogCreation.ItemIndex);

	var
	logDestinations : TLogDestinations := [];
	if chTraceToDebugView.Checked then begin
		logDestinations := logDestinations + [ldOutputDebugString];
	end;
	if chTraceToFile.Checked then begin
		logDestinations := logDestinations + [ldFile];
	end;
	FAppSettings.LogDestinations := logDestinations;

	FAppSettings.UpdateSettingsFromInternals;
end;

end.
