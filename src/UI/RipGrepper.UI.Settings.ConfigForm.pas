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
	RipGrepper.UI.Settings.ColorSettingsForm,
	RipGrepper.UI.Settings.ExtensionSettingsForm,
	RipGrepper.Settings.ExtensionSettings;

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
	RipGrepper.Common.Constants;

{$R *.dfm}

constructor TConfigForm.Create(_settings : TRipGrepperSettings);
begin
	inherited Create(nil);
	Settings := _settings;
	Screen.Cursor := crHourGlass;
	try
		// write ini file content
		Settings.UpdateIniFile;

		FOpenWithConfigForm := TOpenWithConfigForm.Create(nil, Settings.OpenWithSettings);
		FOpenWithConfigForm.Caption := 'Open With...';
		FAppSettingsForm := TAppSettingsForm.Create(nil, Settings);
		FColorSettingsForm := TColorSettingsForm.Create(nil, Settings.FontColorSettings);

		FExtensionSettings := TRipGrepperExtensionSettings.Create(Settings.IniFile);
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

destructor TConfigForm.Destroy;
begin
	FExtensionSettings.Free;
	FSettingsForms.Free;
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

procedure TConfigForm.FormShow(Sender : TObject);
var
	iMaxHeight : integer;
	iMaxWidth : integer;
begin
	Screen.Cursor := crHourGlass;
	try
		iMaxHeight := 0;
		iMaxWidth := 0;
		for var form : TForm in FSettingsForms do begin
			iMaxHeight := System.Math.Max(iMaxHeight, form.Height);
			iMaxWidth := System.Math.Max(iMaxWidth, form.Width);
			form.ManualDock(PageControl1);
			form.Show();
		end;
		FOpenWithConfigForm.pnlBottom.Visible := False;
		self.Height := iMaxHeight + PageControl1.TabHeight + pnlBottom.Height;
		self.Width := iMaxWidth; // TODO
//		Autoscroll := true;
//		VertScrollBar.Range := iMaxHeight;
		PageControl1.TabIndex := 0;
	finally
		Screen.Cursor := crDefault;
	end;
end;

end.
