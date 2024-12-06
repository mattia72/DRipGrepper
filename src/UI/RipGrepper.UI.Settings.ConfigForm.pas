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
	RipGrepper.UI.Settings.ColorSettingsForm;

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
	System.Math;

{$R *.dfm}

constructor TConfigForm.Create(_settings : TRipGrepperSettings);
begin
	inherited Create(nil);
	Settings := _settings;
	// write ini file content
	Settings.UpdateIniFile;

	FOpenWithConfigForm := TOpenWithConfigForm.Create(nil, Settings.OpenWithSettings);
	FOpenWithConfigForm.Caption := 'Open With...';
	FAppSettingsForm := TAppSettingsForm.Create(nil, Settings);
	FColorSettingsForm := TColorSettingsForm.Create(nil, Settings.FontColorSettings);

	FSettingsForms := TObjectList<TForm>.Create();
	FSettingsForms.AddRange([FColorSettingsForm, FOpenWithConfigForm, FAppSettingsForm]);
end;

destructor TConfigForm.Destroy;
begin
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
	iMaxHeight := 0;
	iMaxWidth := 0;
	for var form : TForm in FSettingsForms do begin
		iMaxHeight := System.Math.Max(iMaxHeight, form.Height);
		iMaxWidth := System.Math.Max(iMaxWidth, form.Width);
		form.ManualDock(PageControl1);
		form.Show();
	end;
	FOpenWithConfigForm.pnlBottom.Visible := False;
	self.Height := iMaxHeight;
	self.Width := iMaxWidth; // TODO
//	Autoscroll := true;
//	VertScrollBar.Range := iMaxHeight;
	PageControl1.TabIndex := 0;
end;

end.
