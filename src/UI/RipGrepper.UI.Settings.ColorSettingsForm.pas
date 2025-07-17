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
	Vcl.ExtCtrls,
	Spring,
	RipGrepper.Settings.RipGrepperSettings,
	RipGrepper.Settings.AppSettings;

type
	TColorSettingsForm = class(TSettingsBaseForm)
		grpFontColors : TGroupBox;
		btnLoadDefaults : TButton;
		pnlBottom : TPanel;
		pnlTop : TPanel;
		ScrollBox1 : TScrollBox;
		rgTheme : TRadioGroup;
		procedure btnLoadDefaultsClick(Sender : TObject);
		procedure FormShow(Sender : TObject);
		procedure rgThemeClick(Sender : TObject);

		private
			FAllHeight : Integer;
			FAppSettings : TAppSettings;
			FbSkipClickEvent : Boolean;
			FFontColorSettings : TColorSettings;
			FOnThemeChanged : Event<TNotifyEvent>;
			function GetOnThemeChanged() : IInvokableEvent<TNotifyEvent>;
			procedure SetFontAttribsForFrames();
			procedure RefreshColorSelectorFrames();

		protected
			procedure ReadSettings; override;
			procedure ThemeChanged();
			procedure WriteSettings; override;

		public
			constructor Create(_Owner : TComponent; _settings : TRipGrepperSettings);
			procedure LoadDefaultColorsForTheme(Sender : TObject);

		published
			property OnThemeChanged : IInvokableEvent<TNotifyEvent> read GetOnThemeChanged;
	end;

var
	ColorSettingsForm : TColorSettingsForm;

implementation

uses
	RipGrepper.UI.ColorSelectorFrame,
	RipGrepper.Tools.DebugUtils,
	RipGrepper.Common.Constants,
	System.RegularExpressions,
	RipGrepper.Helper.UI.DarkMode,
	System.StrUtils,
	Vcl.Themes,
	RipGrepper.Helper.UI,
	RipGrepper.UI.MainForm;

{$R *.dfm}

constructor TColorSettingsForm.Create(_Owner : TComponent; _settings : TRipGrepperSettings);
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TColorSettingsForm.Create');

	inherited Create(_Owner, _settings);
	Caption := FONTS_AND_COLORS_CAPTION;
	FFontColorSettings := _settings.FontColorSettings;
	FAppSettings := _settings.AppSettings;
	ReadSettings;

	// setting theme is necessary here, if you don't want to get exception on close
	{$IFDEF STANDALONE}
	var
	colorTheme := FAppSettings.ColorTheme;
	dbgMsg.Msg('colorTheme = ' + colorTheme);
	TDarkModeHelper.SetThemeMode(colorTheme);
	{$ENDIF}
	FAllHeight := TColorSelectorFrame.AddSelectionFrames(FFontColorSettings.FontColors, self, ScrollBox1);
	FAllHeight := FAllHeight + pnlBottom.Height;

	// FOnThemeChanged.Add(LoadDefaultColorsForTheme); doesn't work :/ exception from rgTheme.SetChecked ???
end;

procedure TColorSettingsForm.btnLoadDefaultsClick(Sender : TObject);
begin
	FFontColorSettings.LoadDefaultColors(TDarkModeHelper.GetActualThemeMode, True);
	FFontColorSettings.StoreToPersister;

	SetFontAttribsForFrames;
end;

procedure TColorSettingsForm.FormShow(Sender : TObject);

begin
	inherited;
	self.Height := FAllHeight + self.Height;
	FbSkipClickEvent := True;
	try
		rgTheme.ItemIndex := Integer(TDarkModeHelper.GetActualThemeMode);

		{$IFNDEF STANDALONE}
		rgTheme.Enabled := False;
		rgTheme.ItemIndex := Integer(tmSystem);
		rgTheme.Hint := 'Theme can be changed only in IDE';
		{$ENDIF}
	finally
		FbSkipClickEvent := False;
	end;

end;

function TColorSettingsForm.GetOnThemeChanged() : IInvokableEvent<TNotifyEvent>;
begin
	Result := FOnThemeChanged;
end;

procedure TColorSettingsForm.LoadDefaultColorsForTheme(Sender : TObject);
var
	themeName : string;
	tm : EThemeMode;
begin
	tm := EThemeMode(rgTheme.ItemIndex);
	themeName := TDarkModeHelper.GetThemeNameByMode(tm);
	TAsyncMsgBox.ShowQuestion(
		{ } Format('Would you like to load default fonts and color settings for ''%s'' theme?', [themeName]),
		procedure()
		begin
			btnLoadDefaultsClick(Sender);
		end);
end;

procedure TColorSettingsForm.ReadSettings;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TColorSettingsForm.ReadSettings');
	FAppSettings.LoadFromDict;

	if FFontColorSettings.FontColors.IsEmpty then begin
		FFontColorSettings.LoadDefaultColors(TDarkModeHelper.GetActualThemeMode);
		FFontColorSettings.StoreToPersister();
	end;
	FFontColorSettings.LoadFromDict;
end;

procedure TColorSettingsForm.SetFontAttribsForFrames();
var
	cf : TColorSelectorFrame;
	sFontAttribs : string;
	sSettingsName : string;
begin
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

procedure TColorSettingsForm.rgThemeClick(Sender : TObject);
var
	tm : EThemeMode;
begin
	if FbSkipClickEvent then begin
		Exit;
	end;
	tm := EThemeMode(rgTheme.ItemIndex);
	TDarkModeHelper.SetThemeMode(tm);
	ThemeChanged();
end;

procedure TColorSettingsForm.RefreshColorSelectorFrames();
var
	cf : TColorSelectorFrame;
begin
	for var i := 0 to ComponentCount - 1 do begin
		if Components[i] is TColorSelectorFrame then begin
			cf := TColorSelectorFrame(Components[i]);
			cf.Refresh;
		end;
	end;
end;

procedure TColorSettingsForm.ThemeChanged();
begin
	if FOnThemeChanged.CanInvoke then begin
		FOnThemeChanged.Invoke(Self);
	end;
	RipGrepperForm.ThemeChengedEventSubscriber.HandleThemeChangedEvent(self);
	RefreshColorSelectorFrames();
end;

procedure TColorSettingsForm.WriteSettings;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TColorSettingsForm.WriteSettings');
	var
	fc := FFontColorSettings.FontColors;
	TColorSelectorFrame.WriteColorSettings(fc, self);
	FFontColorSettings.FontColors := fc;
	// it is needed to store FontColorToSettings
	FFontColorSettings.StoreToPersister;

	var
	tm := EThemeMode(rgTheme.ItemIndex);
	var
	theme := TDarkModeHelper.GetThemeNameByMode(tm);
	dbgMsg.Msg('GetThemeNameByMode: ' + theme);

	FAppSettings.ColorTheme := theme;
	dbgMsg.Msg('set FAppSettings.ColorTheme: ' + FAppSettings.ColorTheme);
end;

end.
