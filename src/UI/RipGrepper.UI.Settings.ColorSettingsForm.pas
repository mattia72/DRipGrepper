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
	RipGrepper.UI.SettingsFormBase,
	Vcl.ExtCtrls;

type
	TColorSettingsForm = class(TSettingsBaseForm)
		grpFontColors : TGroupBox;
		Button1 : TButton;
		pnlBottom : TPanel;
		pnlTop : TPanel;
		ScrollBox1: TScrollBox;
		procedure Button1Click(Sender : TObject);
		procedure FormShow(Sender : TObject);

		private
			FAllHeight : Integer;
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
	RipGrepper.Tools.DebugUtils,
	RipGrepper.Common.Constants,
	System.RegularExpressions;

{$R *.dfm}

constructor TColorSettingsForm.Create(_Owner : TComponent; _settings : TColorSettings);
begin
	inherited Create(_Owner, _settings);
	Caption := FONTS_AND_COLORS_CAPTION;
	FFontColorSettings := _settings;
	ReadSettings;
	FAllHeight := TColorSelectorFrame.AddSelectionFrames(FFontColorSettings.FontColors, self, ScrollBox1);
	FAllHeight := FAllHeight + pnlBottom.Height;
end;

procedure TColorSettingsForm.Button1Click(Sender : TObject);
var
	cf : TColorSelectorFrame;
	sFontAttribs : string;
	sSettingsName : string;
begin
	FFontColorSettings.LoadDefaultColors(TDarkModeHelper.GetActualThemeMode);
	FFontColorSettings.StoreToPersister;
	for var i := 0 to ComponentCount - 1 do begin
		if Components[i] is TColorSelectorFrame then begin
			cf := TColorSelectorFrame(Components[i]);
			sSettingsName := TRegex.Replace(cf.LabelText.Caption, '[ ,:]', '');
			sFontAttribs := FFontColorSettings.SettingsDict.GetSetting(sSettingsName).AsString;
			cf.SelectedFontAttributes.FromString(sFontAttribs);
			cf.Refresh;
		end;
	end;
end;

procedure TColorSettingsForm.FormShow(Sender : TObject);

begin
	inherited;
	self.Height := FAllHeight + self.Height;
end;

procedure TColorSettingsForm.ReadSettings;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TColorSettingsForm.ReadSettings');
	if FFontColorSettings.FontColors.IsEmpty then begin
		FFontColorSettings.LoadDefaultColors(TDarkModeHelper.GetActualThemeMode);
		FFontColorSettings.StoreToPersister();
	end;
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
