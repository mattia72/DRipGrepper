unit RipGrepper.UI.SettingsFormBase;

interface

uses
	RipGrepper.Settings.Persistable,
	System.Classes,
	Vcl.Forms,
	RipGrepper.UI.BaseForm;

type
	ISettingsForm = interface
		['{27B41AE6-5394-4748-BBB7-5B09CBB58925}']
		procedure OnCancel;
		procedure OnOk;
		procedure ReadSettings;
		procedure WriteSettings;
		procedure OnSettingsUpdated;

	end;

	TSettingsBaseForm = class(TBaseForm, ISettingsForm)
		protected
			FSettings : IPersistable;
			FReadSettingsCalled : Boolean;
			procedure OnCancel; virtual;
			procedure OnOk; virtual;
			/// ReadSettings: here you can transform FSettings to your needs
			procedure ReadSettings(); virtual; abstract;
			/// WriteSettings: here you can transform controls to FSettings
			procedure WriteSettings(); virtual; abstract;
			procedure OnSettingsUpdated(); virtual;

		public
			constructor Create(_Owner : TComponent; _settings : IPersistable; _themeName : string = ''); reintroduce;
	end;

{$R *.dfm}

implementation

uses
	Vcl.Controls,
	RipGrepper.Tools.DebugUtils,
	RipGrepper.Settings.RipGrepperSettings;

constructor TSettingsBaseForm.Create(_Owner : TComponent; _settings : IPersistable; _themeName : string = '');
begin
	inherited Create(_Owner, _themeName);
	PanelBottom.Visible := False;
	FSettings := _settings;
	FReadSettingsCalled := False;
end;

procedure TSettingsBaseForm.OnCancel;
begin
	ModalResult := mrCancel;
end;

procedure TSettingsBaseForm.OnOk;
begin
	// Skip WriteSettings if ReadSettings was never called (tab was never visited)
	if FReadSettingsCalled then
		WriteSettings;
	ModalResult := mrOk;
end;

procedure TSettingsBaseForm.OnSettingsUpdated();
begin
	// here you can update things depending on changed settings
end;

end.
