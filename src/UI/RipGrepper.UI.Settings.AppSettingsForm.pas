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
	Vcl.ActnList;

const
	{ User-defined message }
	UserMessageValidateInput = wm_User + 100;

type
	EValidateCtrls = (vcRgExePath, vcIniFilePath);

	TAppSettingsForm = class(TSettingsBaseForm)
		Panel1 : TPanel;
		chDebugTrace : TCheckBox;
		chExpertMode : TCheckBox;
		grpDeveloper : TGroupBox;
		lbledtIniFilePath : TLabeledEdit;
		btnedtRgExePath : TButtonedEdit;
		ImageListButtons : TImageList;
		lblRgExePath : TLabel;
		OpenDialog1 : TOpenDialog;
		ActionList1 : TActionList;
		ActionOpenFileDialog : TAction;
		lblVersion : TLabel;
		Memo1 : TMemo;
		procedure btnedtRgExePathEnter(Sender : TObject);
		procedure btnedtRgExePathExit(Sender : TObject);
		procedure btnedtRgExePathRightButtonClick(Sender : TObject);
		procedure FormShow(Sender : TObject);

		private

			FRefocusing : TObject;
			FAppSettings : TAppSettings;
			FRipGrepSettings : TRipGrepParameterSettings;
			function IsRgExeValid(const filePath : string) : Boolean;
			{ User-defined message handler }
			procedure ValidateInput(var M : TMessage); message UserMessageValidateInput;

		protected
			procedure ReadSettings; override;
			procedure WriteSettings; override;

		public
			constructor Create(_Owner : TComponent; _settings : TRipGrepperSettings);
			function GetRgVersion(const _rgPath: string): string;
	end;

var
	AppSettingsForm : TAppSettingsForm;

implementation

uses
	RipGrepper.Tools.DebugUtils,
	RipGrepper.Common.Constants,
	RipGrepper.Tools.FileUtils,
	RipGrepper.Helper.UI,
	System.IOUtils,
	RipGrepper.Tools.ProcessUtils;

{$R *.dfm}

constructor TAppSettingsForm.Create(_Owner : TComponent; _settings : TRipGrepperSettings);
begin
	inherited Create(_Owner, _settings);
	Caption := 'General';
	FAppSettings := (FSettings as TRipGrepperSettings).AppSettings;
	FRipGrepSettings := (FSettings as TRipGrepperSettings).RipGrepParameters;
end;

procedure TAppSettingsForm.btnedtRgExePathEnter(Sender : TObject);
begin
	if FRefocusing = btnedtRgExePath then
		FRefocusing := nil;
end;

procedure TAppSettingsForm.btnedtRgExePathExit(Sender : TObject);
begin
	if FRefocusing = nil then
		PostMessage(Handle, UserMessageValidateInput, 0, LParam(vcRgExePath));
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
	PostMessage(Handle, UserMessageValidateInput, 0, LParam(vcRgExePath));
end;

procedure TAppSettingsForm.FormShow(Sender : TObject);
begin
	ReadSettings;
end;

function TAppSettingsForm.GetRgVersion(const _rgPath: string): string;
var
	sl : TStrings;
begin
	sl := TStringList.Create();
	sl.Add('--version');
	try
		TSimpleProcessOutputStringReader.RunProcess(_rgPath, sl, '.', sl);
		Result := sl.Text;
	finally
		sl.Free;
	end;
end;

function TAppSettingsForm.IsRgExeValid(const filePath : string) : Boolean;
var
	name : string;
begin
	name := TPath.GetFileName(filePath);
	Result := LowerCase(name) = 'rg.exe';
end;

procedure TAppSettingsForm.ReadSettings;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TAppSettingsForm.ReadSettings');

	FAppSettings.LoadFromDict;
	FRipGrepSettings.LoadFromDict;

	chDebugTrace.Checked := FAppSettings.DebugTrace;
	chExpertMode.Checked := FAppSettings.ExpertMode;
	lbledtIniFilePath.Text := FAppSettings.IniFile.FileName;
	btnedtRgExePath.Text := FRipGrepSettings.RipGrepPath;
	Memo1.Text := GetRgVersion(FRipGrepSettings.RipGrepPath);
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
		vcIniFilePath : begin
			if not IsRgExeValid(lbledtIniFilePath.Text) then begin
				FRefocusing := lbledtIniFilePath;
				TMsgBox.ShowError('Ini file path not valid!');
				lbledtIniFilePath.SetFocus;
			end;

		end;
	end;
end;

procedure TAppSettingsForm.WriteSettings;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TAppSettingsForm.WriteSettings');
	FAppSettings.DebugTrace := chDebugTrace.Checked;
	FAppSettings.ExpertMode := chExpertMode.Checked;
	var
	rgPath := btnedtRgExePath.Text;
	if IsRgExeValid(rgPath) then begin
		FRipGrepSettings.RipGrepPath := rgPath;
	end;
	inherited WriteSettings;
end;

end.
