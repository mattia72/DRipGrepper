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
	Vcl.ExtCtrls;

type
	TConfigForm = class(TForm)
		PageControl1 : TPageControl;
		pnlBottom : TPanel;
		btn_Save : TButton;
		btn_Cancel : TButton;
		ActionList1 : TActionList;
		ActionOk : TAction;
		ActionCancel : TAction;
		procedure ActionCancelExecute(Sender: TObject);
		procedure ActionOkExecute(Sender : TObject);
		procedure FormShow(Sender : TObject);

		private
			FOpenWithConfigForm : TOpenWithConfigForm;
			FSettings : TRipGrepperSettings;

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
end;

destructor TConfigForm.Destroy;
begin
	Settings.ReCreateMemIni;
	Settings.ReLoad;
	inherited;
end;

procedure TConfigForm.ActionCancelExecute(Sender: TObject);
begin
	FOpenWithConfigForm.ActionCancelExecute(Sender);
	ModalResult := mrCancel;
end;

procedure TConfigForm.ActionOkExecute(Sender : TObject);
begin
	FOpenWithConfigForm.ActionOkExecute(sender);
	ModalResult := mrOk;
end;

procedure TConfigForm.FormShow(Sender : TObject);
begin
	FOpenWithConfigForm.ManualDock(PageControl1);
	FOpenWithConfigForm.pnlBottom.Visible := False;
	FOpenWithConfigForm.Show();
	PageControl1.TabIndex := 0;
end;

end.
