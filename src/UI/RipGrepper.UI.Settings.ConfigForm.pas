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
	RipGrepper.UI.SettingsFormBase,
	ArrayEx,
	System.Generics.Collections,
	RipGrepper.Settings.ExtensionSettings,
	RipGrepper.UI.Settings.ExtensionSettingsForm,
	RipGrepper.UI.Settings.ColorSettingsForm,
	RipGrepper.Helper.UI.DarkMode;

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
			FAppSettingsForm : TAppSettingsForm;
			FColorSettingsForm : TColorSettingsForm;
			FExtensionSettings : TRipGrepperExtensionSettings;
			FExtensionSettingsForm : TExtensionSettingsForm;
			FOpenWithConfigForm : TOpenWithConfigForm;
			FSettings : TRipGrepperSettings;
			FSettingsForms : TObjectList<TForm>;
			FThemeHandler : TThemeHandler;
			procedure AddSettingTabs;
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
	RipGrepper.Tools.DebugUtils;

{$R *.dfm}

constructor TConfigForm.Create(_settings : TRipGrepperSettings);
begin
	inherited Create(nil);
	Settings := _settings;
	Screen.Cursor := crHourGlass;
	try
		FOpenWithConfigForm := TOpenWithConfigForm.Create(nil, Settings.OpenWithSettings);
		FOpenWithConfigForm.Caption := 'Open With...';
		FAppSettingsForm := TAppSettingsForm.Create(nil, Settings);
		FColorSettingsForm := TColorSettingsForm.Create(nil, Settings.FontColorSettings);

		FExtensionSettings := TRipGrepperExtensionSettings.Create(Settings);
		FExtensionSettings.ReadIni;
		FExtensionSettings.LoadFromDict;
		FExtensionSettingsForm := TExtensionSettingsForm.Create(nil, FExtensionSettings);

		FSettingsForms := TObjectList<TForm>.Create();
		FSettingsForms.AddRange([
			{ } FAppSettingsForm,
			{ } FColorSettingsForm,
			{ } FOpenWithConfigForm,
			{ } FExtensionSettingsForm]);
	finally
		Screen.Cursor := crDefault;
	end;
end;

procedure TConfigForm.FormCreate(Sender : TObject);
begin
	{$IFNDEF STANDALONE}
	TIDEThemeHelper.AllowThemes(TConfigForm);
	{$ELSE}
	TDarkModeHelper.AllowThemes();
	{$ENDIF}
	ThemeHandler.HandleThemes(GSettings.AppSettings.ColorTheme);
	AddSettingTabs;
end;

destructor TConfigForm.Destroy;
begin
	FExtensionSettings.Free;
	FSettingsForms.Free;
	FThemeHandler.Free;
	Settings.ReCreateMemIni;
	Settings.ReLoad;
	inherited;
end;

procedure TConfigForm.ActionCancelExecute(Sender : TObject);
begin
	for var f in FSettingsForms do begin
		(f as ISettingsForm).OnCancel();
	end;

	FOpenWithConfigForm.ActionCancelExecute(Sender);
	ModalResult := mrCancel;
end;

procedure TConfigForm.ActionOkExecute(Sender : TObject);
begin
	for var f in FSettingsForms do begin
		(f as ISettingsForm).OnOk();
	end;

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
			// var tab := PageControl1.Pages. (TTabSheet.Create(PageControl1));
			// form.Parent := tab;
			// form.Align := alClient;
			// form.BorderStyle := bsNone;
			// form.ParentBackground := True;
			// form.Show();
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
		FThemeHandler := TThemeHandler.Create(self);
	end;
	Result := FThemeHandler;
end;

end.
