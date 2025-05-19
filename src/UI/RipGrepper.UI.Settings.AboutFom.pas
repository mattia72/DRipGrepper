unit RipGrepper.UI.Settings.AboutFom;

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
	RipGrepper.Settings.AppSettings,
	System.Actions,
	Vcl.ActnList,
	REST.Types,
	REST.Client,
	Data.Bind.Components,
	Data.Bind.ObjectScope;

type
	TAboutForm = class(TSettingsBaseForm)
		grpFontColors : TGroupBox;
		btnCheckUpdate : TButton;
		pnlBottom : TPanel;
		pnlTop : TPanel;
		ScrollBox1 : TScrollBox;
		ActionList1 : TActionList;
		ActionCheckUpdate : TAction;
		RESTClient1 : TRESTClient;
		RESTRequest1 : TRESTRequest;
		RESTResponse1 : TRESTResponse;
		Memo1 : TMemo;
		procedure ActionCheckUpdateExecute(Sender : TObject);
		procedure FormShow(Sender : TObject);

		private
			FAppSettings : TAppSettings;
			FbSkipClickEvent : Boolean;

		protected
			procedure ReadSettings; override;
			procedure WriteSettings; override;

		public
			constructor Create(_Owner : TComponent; _settings : TRipGrepperSettings);

		published
	end;

var
	AboutForm : TAboutForm;

implementation

uses
	RipGrepper.UI.ColorSelectorFrame,
	RipGrepper.Tools.DebugUtils,
	RipGrepper.Common.Constants,
	System.RegularExpressions,
	RipGrepper.Helper.UI.DarkMode,
	System.StrUtils,
	RipGrepper.Helper.UI,
	System.JSON;

{$R *.dfm}

constructor TAboutForm.Create(_Owner : TComponent; _settings : TRipGrepperSettings);
begin
	inherited Create(_Owner, _settings);
	Caption := ABOUT_CAPTION;

	FAppSettings := _settings.AppSettings;
	ReadSettings;
end;

procedure TAboutForm.ActionCheckUpdateExecute(Sender : TObject);
var
	jValue : TJSONValue;
begin
	RESTRequest1.Execute;
	jValue := RESTResponse1.JSONValue;
	Memo1.Text := jValue.ToString;
end;

procedure TAboutForm.FormShow(Sender : TObject);

begin
	inherited;
	FbSkipClickEvent := True;
	try

	finally
		FbSkipClickEvent := False;
	end;

end;

procedure TAboutForm.ReadSettings;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TAboutForm.ReadSettings');
	FAppSettings.LoadFromDict;

end;

procedure TAboutForm.WriteSettings;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TAboutForm.WriteSettings');
end;

end.
