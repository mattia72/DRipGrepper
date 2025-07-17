unit RipGrepper.UI.Settings.ConfigForm;

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
	Vcl.ComCtrls,
	RipGrepper.Settings.RipGrepperSettings,
	RipGrepper.OpenWith.ConfigForm,
	System.Actions,
	Vcl.ActnList,
	Vcl.StdCtrls,
	Vcl.ExtCtrls,
	RipGrepper.UI.Settings.AppSettingsForm,
	RipGrepper.UI.Settings.AdvancedForm,
	RipGrepper.UI.SettingsFormBase,
	ArrayEx,
	System.Generics.Collections,
	RipGrepper.Settings.ExtensionSettings,
	RipGrepper.UI.Settings.ExtensionSettingsForm,
	RipGrepper.UI.Settings.ColorSettingsForm,
	RipGrepper.UI.Settings.AboutForm,
	RipGrepper.Helper.UI.DarkMode,
	Spring.Collections;

type
	TConfigForm = class(TForm)
		PageControl1 : TPageControl;
		pnlBottom : TPanel;
		btn_Save : TButton;
		btn_Cancel : TButton;
		ActionList1 : TActionList;
		ActionOk : TAction;
		ActionCancel : TAction;
		pnlTop : TPanel;
		procedure FormCreate(Sender : TObject);
		procedure ActionCancelExecute(Sender : TObject);
		procedure ActionOkExecute(Sender : TObject);
		procedure FormShow(Sender : TObject);

		private
			FAboutForm : TAboutForm;
			FAppSettingsForm : TAppSettingsForm;
			FAdvancedForm : TAdvancedForm;
			FColorSettingsForm : TColorSettingsForm;
			FExtensionSettings : TRipGrepperExtensionSettings;
			FExtensionSettingsForm : TExtensionSettingsForm;
			FOpenWithConfigForm : TOpenWithConfigForm;
			FSettings : TRipGrepperSettings;
			FSettingsForms : IList<TForm>;
			FThemeHandler : TThemeHandler;
			FThemeName : string;
			procedure AddSettingTabs;
			procedure CallFormsOnOk();
			procedure CallFormsOnSettingsUpdated();
			function GetThemeHandler : TThemeHandler;
			property ThemeHandler : TThemeHandler read GetThemeHandler;

		public
			constructor Create(_settings : TRipGrepperSettings); reintroduce;
			destructor Destroy; override;
			property Settings : TRipGrepperSettings read FSettings write FSettings;
	end;

var
	ConfigForm : TConfigForm;

implementation

uses
	System.Math,
	RipGrepper.Common.Constants,
	RipGrepper.Tools.DebugUtils,
	Vcl.Themes;

{$R *.dfm}

constructor TConfigForm.Create(_settings : TRipGrepperSettings);
begin
	inherited Create(nil);
	Settings := _settings;
	Screen.Cursor := crHourGlass;
	try
		FOpenWithConfigForm := TOpenWithConfigForm.Create(nil, Settings.OpenWithSettings, Settings.AppSettings.ColorTheme);
		FOpenWithConfigForm.Caption := 'Open With...';
		FAppSettingsForm := TAppSettingsForm.Create(nil, Settings);
		FAdvancedForm := TAdvancedForm.Create(nil, Settings);
		FAdvancedForm.Caption := 'Advanced';
		FColorSettingsForm := TColorSettingsForm.Create(nil, Settings);

		FExtensionSettings := Settings.SearchFormSettings.ExtensionSettings;
		FExtensionSettings.ReadFile;
		FExtensionSettings.LoadFromDict;
		FExtensionSettingsForm := TExtensionSettingsForm.Create(nil, Settings);

		FAboutForm := TAboutForm.Create(nil, Settings);

		FSettingsForms := TCollections.CreateList<TForm>();
		FSettingsForms.AddRange([
			{ } FAppSettingsForm,
			{ } FColorSettingsForm,
			{ } FOpenWithConfigForm,
			{ } FExtensionSettingsForm,
			{ } FAdvancedForm,
			{ } FAboutForm]);
	finally
		Screen.Cursor := crDefault;
	end;

	ThemeHandler.Init(Settings.AppSettings.ColorTheme);
end;

procedure TConfigForm.FormCreate(Sender : TObject);
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TConfigForm.FormCreate');
	AddSettingTabs;
	var
	theme := Settings.AppSettings.ColorTheme;
	dbgMsg.Msg('Applying theme from appsettings: ' + theme);
	TDarkModeHelper.SetThemeMode(theme);
end;

destructor TConfigForm.Destroy;
begin
	Settings.ReLoadFromDisk;
	for var i := 0 to PageControl1.PageCount - 1 do
		PageControl1.Pages[0].Free;
	inherited;
end;

procedure TConfigForm.ActionCancelExecute(Sender : TObject);
var
	iif : ISettingsForm;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TConfigForm.ActionCancelExecute');

	for var f in FSettingsForms do begin
		if Supports(f, ISettingsForm, iif) then begin
			iif.OnCancel();
		end else begin
			dbgMsg.ErrorMsg(f.Caption + ' not a SettingsForm');
			raise Exception.Create(f.Caption + ' not a SettingsForm');
		end;
	end;
	ModalResult := mrCancel;
end;

procedure TConfigForm.ActionOkExecute(Sender : TObject);
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TConfigForm.ActionOkExecute');

	CallFormsOnOk;

	FSettings.StoreToPersister();
	FSettings.UpdateFile();

	CallFormsOnSettingsUpdated();

	ModalResult := mrOk;
end;

procedure TConfigForm.AddSettingTabs;
var
	iMaxHeight : integer;
	iMaxWidth : integer;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TConfigForm.AddSettingTabs');
	Screen.Cursor := crHourGlass;
	try
		iMaxHeight := 0;
		iMaxWidth := 0;
		for var form : TForm in FSettingsForms do begin
			iMaxHeight := System.Math.Max(iMaxHeight, form.Height);
			iMaxWidth := System.Math.Max(iMaxWidth, form.Width);
			form.ManualDock(PageControl1);
		end;
		FOpenWithConfigForm.pnlBottom.Visible := False;
		self.Height := iMaxHeight + PageControl1.TabHeight + pnlBottom.Height;
		self.Width := iMaxWidth;
		// Autoscroll := true;
		// VertScrollBar.Range := iMaxHeight;
		PageControl1.TabIndex := 0;
	finally
		Screen.Cursor := crDefault;
	end;
end;

procedure TConfigForm.CallFormsOnOk();
var
	iif : ISettingsForm;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TConfigForm.CallFormsOnOk');
	for var f in FSettingsForms do begin
		if Supports(f, ISettingsForm, iif) then begin
			iif.OnOk();
		end else begin
			dbgMsg.ErrorMsg(f.Caption + ' not a SettingsForm');
			raise Exception.Create(f.Caption + ' not a SettingsForm');
		end;
	end;
end;

procedure TConfigForm.CallFormsOnSettingsUpdated();
var
	iif : ISettingsForm;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TConfigForm.CallFormsOnSettingsUpdated');
	for var f in FSettingsForms do begin
		if Supports(f, ISettingsForm, iif) then begin
			iif.OnSettingsUpdated();
		end else begin
			dbgMsg.ErrorMsg(f.Caption + ' not a SettingsForm');
			raise Exception.Create(f.Caption + ' not a SettingsForm');
		end;
	end;
end;

procedure TConfigForm.FormShow(Sender : TObject);
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TConfigForm.FormShow');

	for var form : TForm in FSettingsForms do begin
		try
			dbgMsg.Msg('Showing form: ' + form.Caption);
			form.Show();
		except
			on E : Exception do
				dbgMsg.Msg('Error showing form: ' + form.Name + ' - ' + E.Message);
		end;
	end;
end;

function TConfigForm.GetThemeHandler : TThemeHandler;
begin
	if not Assigned(FThemeHandler) then begin
		FThemeHandler := TThemeHandler.Create(self, FThemeName);
	end;
	Result := FThemeHandler;
end;

end.
