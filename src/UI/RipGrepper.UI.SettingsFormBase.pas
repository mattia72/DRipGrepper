unit RipGrepper.UI.SettingsFormBase;

interface

uses
	RipGrepper.Settings.Persistable,
	System.Classes,
	Vcl.Forms,
	RipGrepper.UI.DpiScaler;

type
	ISettingsForm = interface
		['{27B41AE6-5394-4748-BBB7-5B09CBB58925}']
		procedure OnCancel;
		procedure OnOk;
		procedure ReadSettings;
		procedure WriteSettings;

	end;

	TSettingsBaseForm = class(TForm, ISettingsForm)
		private
			FDpiScaler : TRipGrepperDpiScaler;

		protected
			FSettings : TPersistableSettings;
			procedure OnCancel; virtual;
			procedure OnOk; virtual;
			procedure ReadSettings; virtual;
			procedure WriteSettings; virtual;

		public
			constructor Create(_Owner : TComponent; _settings : TPersistableSettings); reintroduce;
			destructor Destroy; override;
	end;

implementation

uses
	Vcl.Controls,
	RipGrepper.Tools.DebugUtils;

constructor TSettingsBaseForm.Create(_Owner : TComponent; _settings : TPersistableSettings);
begin
	inherited Create(_Owner);
	FSettings := _settings;
	FDpiScaler := TRipGrepperDpiScaler.Create(self);
end;

destructor TSettingsBaseForm.Destroy;
begin
	FDpiScaler.Free;
	inherited;
end;

procedure TSettingsBaseForm.OnCancel;
begin
	ModalResult := mrCancel;
end;

procedure TSettingsBaseForm.OnOk;
begin
	WriteSettings;
	ModalResult := mrOk;
end;

procedure TSettingsBaseForm.ReadSettings;
begin
	//
end;

procedure TSettingsBaseForm.WriteSettings;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TSettingsBaseForm.WriteSettings');
	FSettings.StoreToDict;
	FSettings.UpdateIniFile; // save
end;

end.
