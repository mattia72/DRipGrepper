unit RipGrepper.UI.Settings.ExtensionSettingsForm;

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
	RipGrepper.UI.SettingsFormBase,
	RipGrepper.Settings.RipGrepperSettings,
	RipGrepper.Settings.ExtensionSettings,
	Vcl.StdCtrls,
	Vcl.ComCtrls,
	Vcl.ExtCtrls;

type
	TExtensionSettingsForm = class(TSettingsBaseForm)
		pnlMiddle : TPanel;
		hkedtOpenWidth : THotKey;
		lblOpenWith : TLabel;
		grpShortcuts : TGroupBox;
		hkedtSearchSelected : THotKey;
		lblSearch : TLabel;

		private
			FExtensionSettings : TRipGrepperExtensionSettings;

		protected
			procedure ReadSettings; override;
			procedure WriteSettings; override;

		public
			constructor Create(_Owner : TComponent; _settings : TRipGrepperExtensionSettings);
	end;

var
	ExtensionSettingsForm : TExtensionSettingsForm;

implementation

uses
	RipGrepper.Tools.DebugUtils,
	Vcl.Menus;

{$R *.dfm}

constructor TExtensionSettingsForm.Create(_Owner : TComponent; _settings : TRipGrepperExtensionSettings);
begin
	inherited Create(_Owner, _settings);
	Caption := 'Extension';
	FExtensionSettings := _settings;
	ReadSettings;
end;

procedure TExtensionSettingsForm.ReadSettings;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TForm1.ReadSettings');
	FExtensionSettings.ReadIni;
	FExtensionSettings.LoadFromDict;
	hkedtOpenWidth.HotKey := TextToShortCut(FExtensionSettings.OpenWithShortCut);
	hkedtSearchSelected.HotKey := TextToShortCut(FExtensionSettings.SearchSelectedShortcut);
end;

procedure TExtensionSettingsForm.WriteSettings;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TForm1.WriteSettings');

	FExtensionSettings.OpenWithShortCut := ShortCutToText(hkedtOpenWidth.HotKey);
	FExtensionSettings.SearchSelectedShortcut := ShortCutToText(hkedtSearchSelected.HotKey);

	// FExtensionSettings.
	inherited WriteSettings;
end;

end.
