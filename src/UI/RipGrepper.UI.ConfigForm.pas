unit RipGrepper.UI.ConfigForm;

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
	RipGrepper.UI.AppSettingsForm,
	RipGrepper.UI.SettingsFormBase,
	ArrayEx,
	System.Generics.Collections;

type
	TConfigForm = class(TForm)
		PageControl1 : TPageControl;
		pnlBottom : TPanel;
		btn_Save : TButton;
		btn_Cancel : TButton;
		ActionList1 : TActionList;
		ActionOk : TAction;
		ActionCancel : TAction;
		procedure ActionCancelExecute(Sender : TObject);
		procedure ActionOkExecute(Sender : TObject);
		procedure FormShow(Sender : TObject);

		private
			FAppSettingsForm : TAppSettingsForm;
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

{$R *.dfm}

constructor TConfigForm.Create(_settings : TRipGrepperSettings);
begin
	inherited Create(nil);
	Settings := _settings;
	// write ini file content
	Settings.UpdateIniFile;

	FOpenWithConfigForm := TOpenWithConfigForm.Create(nil, Settings.OpenWithSettings);
    FOpenWithConfigForm.Caption := 'Open With...'   ;
	FAppSettingsForm := TAppSettingsForm.Create(nil, Settings);

	FSettingsForms := TObjectList<TForm>.Create();
	FSettingsForms.AddRange([FAppSettingsForm, FOpenWithConfigForm]);

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
begin
	for var form : TForm in FSettingsForms do begin
		form.ManualDock(PageControl1);
		form.Show();
	end;

	FOpenWithConfigForm.pnlBottom.Visible := False;

	PageControl1.TabIndex := 0;
end;

end.
