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
	Vcl.ControlList;

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
		procedure btnedtIniFilePathLeftButtonClick(Sender : TObject);
		procedure btnedtIniFilePathRightButtonClick(Sender : TObject);
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
	RipGrepper.Common.SimpleTypes,
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

procedure TAdvancedForm.btnedtIniFilePathLeftButtonClick(Sender : TObject);
begin
	FSettings.ReLoadFromDisk;
	ReadSettings;
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
	ReadSettings;
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

function TAdvancedForm.IsRgExeValid(const filePath : string) : Boolean;
var
	name : string;
begin
	name := TPath.GetFileName(filePath);
	Result := LowerCase(name) = LowerCase(RG_EXE);
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

	FAppSettings.UpdateInternalsFromSettings();
end;

procedure TAdvancedForm.ValidateInput(var M : TMessage);
begin
	case EValidateCtrls(M.LParam) of
		vcIniFilePath : begin // not used: ini file location can not be changed
			if not IsRgExeValid(btnedtIniFilePath.Text) then begin
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
	FAppSettings.ExpertMode := chExpertMode.Checked;

	FAppSettings.UpdateSettingsFromInternals;
end;

end.
