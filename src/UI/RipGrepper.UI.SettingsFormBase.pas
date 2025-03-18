unit RipGrepper.UI.SettingsFormBase;

interface

uses
	RipGrepper.Settings.Persistable,
	System.Classes,
	Vcl.Forms,
	RipGrepper.UI.DpiScaler,
	RipGrepper.Helper.UI.DarkMode;

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
			FThemeHandler : TThemeHandler;
			function GetThemeHandler : TThemeHandler;
			property ThemeHandler : TThemeHandler read GetThemeHandler;

		protected
			FSettings : TPersistableSettings;
			procedure OnCancel; virtual;
			procedure OnOk; virtual;
			procedure ReadSettings; virtual;
			procedure WriteSettings; virtual;

		public
			constructor Create(_Owner : TComponent; _settings : TPersistableSettings;
				_themeName : string = ''); reintroduce;
			destructor Destroy; override;
	end;

implementation

uses
	Vcl.Controls,
	RipGrepper.Tools.DebugUtils, RipGrepper.Settings.RipGrepperSettings;

constructor TSettingsBaseForm.Create(_Owner : TComponent; _settings : TPersistableSettings; _themeName : string = '');
begin
	inherited Create(_Owner);
	FSettings := _settings;
	FDpiScaler := TRipGrepperDpiScaler.Create(self);
    ThemeHandler.Init(_themeName);
end;

destructor TSettingsBaseForm.Destroy;
begin
	FDpiScaler.Free;
	inherited;
end;

function TSettingsBaseForm.GetThemeHandler : TThemeHandler;
begin
	if not Assigned(FThemeHandler) then begin
		FThemeHandler := TThemeHandler.Create(self);
	end;
	Result := FThemeHandler;
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
end;

end.
