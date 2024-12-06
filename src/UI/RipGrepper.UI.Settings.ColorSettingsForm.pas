unit RipGrepper.UI.Settings.ColorSettingsForm;

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
	Vcl.StdCtrls,
	RipGrepper.Settings.FontColors,
	RipGrepper.UI.SettingsFormBase;

type
	TColorSettingsForm = class(TSettingsBaseForm)
		grpFontColors : TGroupBox;
		procedure FormShow(Sender : TObject);

		private
			FFontColorSettings : TColorSettings;

		protected
			procedure ReadSettings; override;
			procedure WriteSettings; override;

		public
			constructor Create(_Owner : TComponent; _settings : TColorSettings);
	end;

var
	ColorSettingsForm : TColorSettingsForm;

implementation

uses
	RipGrepper.UI.ColorSelectorFrame,
	RipGrepper.Tools.DebugUtils;

{$R *.dfm}

constructor TColorSettingsForm.Create(_Owner : TComponent; _settings : TColorSettings);
begin
	inherited Create(_Owner, _settings);;
	FFontColorSettings := _settings;
end;

procedure TColorSettingsForm.FormShow(Sender : TObject);
begin
	ReadSettings;
	var
	allHeight := TColorSelectorFrame.AddSelectionFrames(FFontColorSettings.FontColors, self, grpFontColors);
	grpFontColors.Height := allHeight + grpFontColors.Margins.Bottom;
	self.Height := grpFontColors.Height;
end;

procedure TColorSettingsForm.ReadSettings;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TColorSettingsForm.ReadSettings');
	FFontColorSettings.LoadFromDict;
end;

procedure TColorSettingsForm.WriteSettings;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TColorSettingsForm.WriteSettings');
	var
	fc := FFontColorSettings.FontColors;
	TColorSelectorFrame.WriteColorSettings(fc, self);
	FFontColorSettings.FontColors := fc;
	inherited WriteSettings;
end;

end.
