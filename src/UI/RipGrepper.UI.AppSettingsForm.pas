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
	RipGrepper.UI.SettingsFormBase,
	RipGrepper.UI.ColorSelectorFrame,
	RipGrepper.Settings.FontColors,
	RipGrepper.Settings.RipGrepperSettings;

type

	TAppSettingsForm = class(TSettingsBaseForm)
		Panel1 : TPanel;
		chDebugTrace : TCheckBox;
		chExpertMode : TCheckBox;
		ColorSelectorFrame1 : TColorSelectorFrame;
		procedure FormShow(Sender : TObject);

		private
			FAppSettings : TAppSettings;
			FFontColorSettings : TColorSettings;
			FInternFont : TFont;

		protected
			procedure OnCancel; override;
			procedure OnOk; override;
			procedure ReadSettings; override;
			procedure WriteSettings; override;

		public
			constructor Create(_Owner : TComponent; _settings : TRipGrepperSettings);
	end;

var
	AppSettingsForm : TAppSettingsForm;

implementation

uses
	RipGrepper.Tools.DebugUtils;

{$R *.dfm}

constructor TAppSettingsForm.Create(_Owner : TComponent; _settings : TRipGrepperSettings);
begin
	inherited Create(_Owner, _settings);
	Caption := 'General';
	FFontColorSettings := (FSettings as TRipGrepperSettings).FontColorSettings;
	FAppSettings := (FSettings as TRipGrepperSettings).AppSettings;
	FInternFont := TFont.Create();

end;

procedure TAppSettingsForm.FormShow(Sender : TObject);
begin
	ReadSettings;
	ColorSelectorFrame1.LabelText.Caption := 'Match Text:';

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
	ColorSelectorFrame1.AssignFontAttributes(FFontColorSettings.FontColors.TreeViewMatchText);
	FInternFont.Assign(ColorSelectorFrame1.SelectedFont);
end;

procedure TAppSettingsForm.WriteSettings;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TAppSettingsForm.WriteSettings');
	FAppSettings.DebugTrace := chDebugTrace.Checked;
	FAppSettings.ExpertMode := chExpertMode.Checked;
	var
	fc := FFontColorSettings.FontColors;
	fc.TreeViewMatchText := ColorSelectorFrame1.SelectedFontAttributes;
	FFontColorSettings.FontColors := fc;
	inherited WriteSettings;
end;

end.
