unit RipGrepper.UI.Settings.ExtensionSettingsForm;

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
	RipGrepper.UI.SettingsFormBase,
	RipGrepper.Settings.RipGrepperSettings,
	RipGrepper.Settings.ExtensionSettings, Vcl.StdCtrls, Vcl.ComCtrls, Vcl.ExtCtrls;

type
	TExtensionSettingsForm = class(TSettingsBaseForm)
    pnlMiddle: TPanel;
    hkedtOpenWidth: THotKey;
    lblOpenWith: TLabel;
    grpShortcuts: TGroupBox;
    HotKey1: THotKey;
    lblSearch: TLabel;
		private
			FExtensionSettings : TRipGrepperExtensionSettings;
		protected
			procedure ReadSettings; override;
			procedure WriteSettings; override;

		public
			constructor Create(_Owner : TComponent; _settings : TRipGrepperSettings);
	end;

var
	ExtensionSettingsForm : TExtensionSettingsForm;

implementation

uses
	RipGrepper.Tools.DebugUtils;

{$R *.dfm}

constructor TExtensionSettingsForm.Create(_Owner : TComponent; _settings : TRipGrepperSettings);
begin
	inherited Create(_Owner, _settings);
	Caption := 'Extension';
//  FExtensionSettings := (FSettings as TRipGrepperExtensionSettings).;
end;

procedure TExtensionSettingsForm.ReadSettings;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TForm1.ReadSettings');

	FExtensionSettings.LoadFromDict;

	// chDebugTrace.Checked := FAppSettings.DebugTrace;
	// chExpertMode.Checked := FAppSettings.ExpertMode;
	// lbledtIniFilePath.Text := FAppSettings.IniFile.FileName;
	// btnedtRgExePath.Text := FRipGrepSettings.RipGrepPath;
end;

procedure TExtensionSettingsForm.WriteSettings;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TForm1.WriteSettings');
//  FExtensionSettings.
	inherited WriteSettings;
end;

end.
