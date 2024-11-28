unit RipGrepper.UI.AppSettingsForm;

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
	RipGrepper.UI.SettingsFormBase;

type

	TAppSettingsForm = class(TSettingsBaseForm)
		Panel1 : TPanel;
		ColorBox1 : TColorBox;
		Label1 : TLabel;
		chDebugTrace : TCheckBox;
		chExpertMode : TCheckBox;
		Label2 : TLabel;
		ColorBox2 : TColorBox;

		private
			FAppSettings : TAppSettings;

			{ Private declarations }
		protected
			procedure OnCancel; override;
			procedure OnOk; override;
			procedure ReadSettings; override;
			procedure WriteSettings; override;

		public
			constructor Create(_Owner : TComponent; _settings : TAppSettings);
			{ Public declarations }
	end;

var
	AppSettingsForm : TAppSettingsForm;

implementation

uses
	RipGrepper.Tools.DebugUtils;

{$R *.dfm}

constructor TAppSettingsForm.Create(_Owner : TComponent; _settings : TAppSettings);
begin
	inherited Create(_Owner, _settings);
    Caption := 'General';
	FAppSettings := FSettings as TAppSettings;
	ReadSettings;
end;

procedure TAppSettingsForm.OnCancel;
begin
	inherited;
end;

procedure TAppSettingsForm.OnOk;
begin
	inherited;
end;

procedure TAppSettingsForm.ReadSettings;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TAppSettingsForm.ReadSettings');
	chDebugTrace.Checked := FAppSettings.DebugTrace;
	chExpertMode.Checked := FAppSettings.ExpertMode;
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
