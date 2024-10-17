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
	RipGrepper.Common.Settings.RipGrepperSettings;

type
	TConfigForm = class(TForm)
		PageControl1 : TPageControl;
		procedure FormShow(Sender : TObject);

		private
			FSettings : TRipGrepperSettings;

		public
			constructor Create(_settings : TRipGrepperSettings); reintroduce;
			property Settings : TRipGrepperSettings read FSettings write FSettings;
	end;

var
	ConfigForm : TConfigForm;

implementation

uses
	RipGrepper.OpenWith.ConfigForm;

{$R *.dfm}

constructor TConfigForm.Create(_settings : TRipGrepperSettings);
begin
	inherited Create(nil);
	Settings := _settings;
end;

procedure TConfigForm.FormShow(Sender : TObject);
begin
	var
	owForm := TOpenWithConfigForm.Create(nil, Settings.OpenWithSettings);
	owForm.ManualDock(PageControl1);
	owForm.Show();
	PageControl1.TabIndex := 1;
	Show();
end;

end.
