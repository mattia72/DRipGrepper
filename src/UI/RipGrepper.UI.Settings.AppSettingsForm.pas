unit RipGrepper.UI.Settings.AppSettingsForm;

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
	Vcl.ControlList;

type
	EValidateCtrls = (vcRgExePath, vcIniFilePath);

	TAppSettingsForm = class(TSettingsBaseForm)
		chBegin : TCheckBox;
		chExpertMode : TCheckBox;
		grpAdvanced : TGroupBox;
		btnedtRgExePath : TButtonedEdit;
		lblRgExePath : TLabel;
		OpenDialog1 : TOpenDialog;
		ActionList1 : TActionList;
		ActionOpenFileDialog : TAction;
		lblVersion : TLabel;
		Memo1 : TMemo;
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
		grpSettings : TGroupBox;
		Label2 : TLabel;
		cmbCopyCmdShell : TComboBox;
		Label3 : TLabel;
		seCmbHistoryCount : TSpinEdit;
		cbLoadLastSearchHistories : TCheckBox;
		rgModeLoadSeraches : TRadioGroup;
		seSearchHistoryCount : TSpinEdit;
		lblSearches : TLabel;
		cbSaveResults : TCheckBox;
		grpSaveLoad : TGroupBox;
		procedure btnedtIniFilePathLeftButtonClick(Sender : TObject);
		procedure btnedtIniFilePathRightButtonClick(Sender : TObject);
		procedure btnedtRgExePathEnter(Sender : TObject);
		procedure btnedtRgExePathExit(Sender : TObject);
		procedure btnedtRgExePathLeftButtonClick(Sender : TObject);
		procedure btnedtRgExePathRightButtonClick(Sender : TObject);
		procedure chRegexClick(Sender : TObject);
		procedure FormShow(Sender : TObject);

		private

			FRefocusing : TObject;
			FAppSettings : TAppSettings;
			FRipGrepSettings : TRipGrepParameterSettings;
			function GetTraceTypeFilters : TTraceFilterTypes;
			function IsRgExeValid(const filePath : string) : Boolean;
			{ User-defined message handler }
			procedure ValidateInput(var M : TMessage); message USERMESSAGE_VALIDATE_INPUT;

		protected
			procedure OnSettingsUpdated(); override;
			procedure ReadSettings; override;
			procedure WriteSettings; override;

		public
			constructor Create(_Owner : TComponent; _settings : TRipGrepperSettings);
			function GetRgVersion(const _rgPath : string) : string;

		published
	end;

var
	AppSettingsForm : TAppSettingsForm;

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
	RipGrepper.Common.SimpleTypes,
	RipGrepper.Helper.Types,
	RipGrepper.Common.LoadHistoryMode;

{$R *.dfm}

constructor TAppSettingsForm.Create(_Owner : TComponent; _settings : TRipGrepperSettings);
begin
	inherited Create(_Owner, _settings);
	Caption := 'General';
	FAppSettings := (FSettings as TRipGrepperSettings).AppSettings;
	FRipGrepSettings := (FSettings as TRipGrepperSettings).RipGrepParameters;
end;

procedure TAppSettingsForm.btnedtIniFilePathLeftButtonClick(Sender : TObject);
begin
	FSettings.ReLoadFromDisk;
	ReadSettings;
end;

procedure TAppSettingsForm.btnedtIniFilePathRightButtonClick(Sender : TObject);
var
	owp : TOpenWithParams;
begin
	owp.FilePath := btnedtIniFilePath.Text;
	owp.Row := 0;
	owp.Column := 0;
	TOpenWith.Execute(owp);
end;

procedure TAppSettingsForm.btnedtRgExePathEnter(Sender : TObject);
begin
	if FRefocusing = btnedtRgExePath then
		FRefocusing := nil;
end;

procedure TAppSettingsForm.btnedtRgExePathExit(Sender : TObject);
begin
	if FRefocusing = nil then
		PostMessage(Handle, USERMESSAGE_VALIDATE_INPUT, 0, LParam(vcRgExePath));
end;

procedure TAppSettingsForm.btnedtRgExePathLeftButtonClick(Sender : TObject);
begin
	Memo1.Text := 'rg.exe --version';
	Memo1.Text := GetRgVersion(btnedtRgExePath.Text);
end;

procedure TAppSettingsForm.btnedtRgExePathRightButtonClick(Sender : TObject);
var
	origFilePath, filePath : string;
begin
	OpenDialog1.Filter := 'Executable files (*.exe)|*.exe';
	origFilePath := btnedtRgExePath.Text;
	OpenDialog1.FileName := origFilePath;
	if OpenDialog1.Execute(self.Handle) then begin
		filePath := OpenDialog1.FileName;
		btnedtRgExePath.Text := filePath;
	end;
	PostMessage(Handle, USERMESSAGE_VALIDATE_INPUT, 0, LParam(vcRgExePath));
end;

procedure TAppSettingsForm.chRegexClick(Sender : TObject);
begin
	edtRegex.Enabled := chRegex.Checked;
	for var ch in [chError, chWarning, chInfo, chVerbose, chBegin, chEnd] do begin
		ch.Enabled := not chRegex.Checked;
		if chRegex.Checked then begin
			ch.Checked := False;
		end;
	end;
end;

procedure TAppSettingsForm.FormShow(Sender : TObject);
begin
	ReadSettings;
end;

function TAppSettingsForm.GetRgVersion(const _rgPath : string) : string;
var
	sl : TStrings;
begin
	if _rgPath.IsEmpty then begin
		Result := RG_EXE + ' not found.';
		Exit;
	end;

	sl := TStringList.Create();
	sl.Add('--version');
	try
		TSimpleProcessOutputStringReader.RunProcess(_rgPath, sl, '.', sl);
		Result := sl.Text;
	finally
		sl.Free;
	end;
end;

function TAppSettingsForm.GetTraceTypeFilters : TTraceFilterTypes;
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

function TAppSettingsForm.IsRgExeValid(const filePath : string) : Boolean;
var
	name : string;
begin
	name := TPath.GetFileName(filePath);
	Result := LowerCase(name) = LowerCase(RG_EXE);
end;

procedure TAppSettingsForm.OnSettingsUpdated();
begin
	// here you can update things depending on changed settings
	TDebugUtils.UpdateTraceActive;
end;

procedure TAppSettingsForm.ReadSettings;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TAppSettingsForm.ReadSettings');

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

	chExpertMode.Checked := FAppSettings.ExpertMode;
	btnedtIniFilePath.Text := FAppSettings.PersisterFactory.FilePath;
	cmbCopyCmdShell.ItemIndex := Integer(FAppSettings.CopyToClipBoardShell);
	seCmbHistoryCount.Value := FAppSettings.ComboHistoryCount;
	seSearchHistoryCount.Value := FAppSettings.SearchHistoryCount;

	FAppSettings.UpdateInternalsFromSettings();
	cbLoadLastSearchHistories.Checked := FAppSettings.LoadHistoryMode.IsSaveHistoryActive;
	rgModeLoadSeraches.ItemIndex := FAppSettings.LoadHistoryMode.ToInt;
	cbSaveResults.Checked := FAppSettings.LoadHistoryMode.IsSet(lhmSaveResults);
	var
	path := FRipGrepSettings.RipGrepPath;
	if path.IsEmpty then begin
		FRipGrepSettings.TryGetRipGrepPath(path);
	end;
	btnedtRgExePath.Text := path;
	Memo1.Text := GetRgVersion(path);
end;

procedure TAppSettingsForm.ValidateInput(var M : TMessage);
begin
	case EValidateCtrls(M.LParam) of
		vcRgExePath : begin
			if IsRgExeValid(btnedtRgExePath.Text) then begin
				Memo1.Text := GetRgVersion(btnedtRgExePath.Text);
			end else begin
				FRefocusing := btnedtRgExePath;
				TMsgBox.ShowError('Rg.exe path is not valid!');
				btnedtRgExePath.SetFocus;
			end;

		end;
		vcIniFilePath : begin // not used: ini file location can not be changed
			if not IsRgExeValid(btnedtIniFilePath.Text) then begin
				FRefocusing := btnedtIniFilePath;
				TMsgBox.ShowError('Ini file path not valid!');
				btnedtIniFilePath.SetFocus;
			end;

		end;
	end;
end;

procedure TAppSettingsForm.WriteSettings;
var
	lhm : IShared<TLoadHistoryModes>;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TAppSettingsForm.WriteSettings');
	FAppSettings.DebugTrace := TDebugUtils.TraceTypesToStr(GetTraceTypeFilters());
	FAppSettings.ExpertMode := chExpertMode.Checked;
	FAppSettings.CopyToClipBoardShell := TShellType(cmbCopyCmdShell.ItemIndex);
	FAppSettings.ComboHistoryCount := seCmbHistoryCount.Value;
	FAppSettings.SearchHistoryCount := seSearchHistoryCount.Value;
	lhm := FAppSettings.LoadHistoryMode;
	if (cbLoadLastSearchHistories.Checked) then begin
		lhm.AddModeFromInt(rgModeLoadSeraches.ItemIndex);
	end else begin
		lhm.CleanModes(False);
	end;
	if cbSaveResults.Checked then begin
		lhm.AddMode(lhmSaveResults);
	end else begin
		lhm.RemoveMode(lhmSaveResults);
	end;
	FAppSettings.UpdateSettingsFromInternals;

	var
	rgPath := btnedtRgExePath.Text;
	if IsRgExeValid(rgPath) then begin
		FRipGrepSettings.RipGrepPath := rgPath;
	end;
end;

end.
