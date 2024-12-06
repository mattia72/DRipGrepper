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
	Vcl.Mask, RipGrepper.Settings.RipGrepParameterSettings;

type

	TAppSettingsForm = class(TSettingsBaseForm)
		Panel1 : TPanel;
		chDebugTrace : TCheckBox;
		chExpertMode : TCheckBox;
		grpDeveloper : TGroupBox;
		lbledtIniFilePath : TLabeledEdit;
    lbledtRgExePath: TLabeledEdit;
		procedure FormShow(Sender : TObject);

		private
			FAppSettings : TAppSettings;
		FRipGrepSettings: TRipGrepParameterSettings;
		protected
			procedure ReadSettings; override;
			procedure WriteSettings; override;

		public
			constructor Create(_Owner : TComponent; _settings : TRipGrepperSettings);
	end;

var
	AppSettingsForm : TAppSettingsForm;

implementation

uses
	RipGrepper.Tools.DebugUtils,
	RipGrepper.Common.Constants;

{$R *.dfm}

constructor TAppSettingsForm.Create(_Owner : TComponent; _settings : TRipGrepperSettings);
begin
	inherited Create(_Owner, _settings);
	Caption := 'Expert';
	FAppSettings := (FSettings as TRipGrepperSettings).AppSettings;
    FRipGrepSettings := (FSettings as TRipGrepperSettings).RipGrepParameters;
end;

procedure TAppSettingsForm.FormShow(Sender : TObject);
begin
	ReadSettings;
end;

procedure TAppSettingsForm.ReadSettings;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TAppSettingsForm.ReadSettings');
	FAppSettings.LoadFromDict;
	chDebugTrace.Checked := FAppSettings.DebugTrace;
	chExpertMode.Checked := FAppSettings.ExpertMode;
	lbledtIniFilePath.Text := FAppSettings.IniFile.FileName;
    lbledtRgExePath.Text := FRipGrepSettings.RipGrepPath;
end;

procedure TAppSettingsForm.WriteSettings;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TAppSettingsForm.WriteSettings');
	FAppSettings.DebugTrace := chDebugTrace.Checked;
	FAppSettings.ExpertMode := chExpertMode.Checked;
	inherited WriteSettings;
end;

end.
