unit RipGrepper.Common.Settings.RipGrepperSettingsDefaults;

interface

uses
	RipGrepper.Common.Settings.Misc,
	RipGrepper.Common.Settings.Persistable,
	RipGrepper.Common.Settings.RipGrepperSearchFormSettings,
	RipGrepper.Common.Settings.RipGrepParameterSettings,
	System.IniFiles;

type
	TRipGrepperSettingsDefaults = class(TPersistableSettings)
		private
		var
			FRipGrepperSearchFormSettings : TRipGrepperSearchFormSettings;
			FExtensionSettings : TRipGrepperExtensionSettings;

			function GetRipGrepParameters : TRipGrepParameterSettings;

		protected
			FRipGrepParameters : TRipGrepParameterSettings;
		public
			constructor Create(_iniFile : TMemIniFile);
			destructor Destroy; override;
			procedure Init; override;
			procedure Load; override;
			procedure LoadDefault; override;
			procedure Store; override;
			procedure StoreAsDefault; override;

			property ExtensionSettings : TRipGrepperExtensionSettings read FExtensionSettings write FExtensionSettings;
			property RipGrepParameters : TRipGrepParameterSettings read GetRipGrepParameters write FRipGrepParameters;
			property RipGrepperSearchFormSettings : TRipGrepperSearchFormSettings read FRipGrepperSearchFormSettings write FRipGrepperSearchFormSettings;
	end;

implementation

uses
	RipGrepper.Common.IOTAUtils,
	System.SysUtils,
	System.IOUtils,
	Vcl.Forms,
	RipGrepper.Common.Constants,
	RipGrepper.Tools.DebugUtils;

constructor TRipGrepperSettingsDefaults.Create(_iniFile : TMemIniFile);
begin
	inherited Create(_iniFile);

	FRipGrepParameters := TRipGrepParameterSettings.Create(_iniFile);
	FRipGrepperSearchFormSettings := TRipGrepperSearchFormSettings.Create(_iniFile);
	FExtensionSettings := TRipGrepperExtensionSettings.Create(_iniFile);
	FIsLoaded := False;
end;

destructor TRipGrepperSettingsDefaults.Destroy;
begin
	FRipGrepParameters.Free;
	FRipGrepperSearchFormSettings.Free;
	FExtensionSettings.Free;
	inherited;
end;

function TRipGrepperSettingsDefaults.GetRipGrepParameters : TRipGrepParameterSettings;
begin
	Result := FRipGrepParameters;
end;

procedure TRipGrepperSettingsDefaults.Init;
begin
	inherited;
end;

procedure TRipGrepperSettingsDefaults.Load;
begin
	inherited Load();
	TDebugUtils.DebugMessage('TRipGrepperSettingsDefaults.Load: start');

	FRipGrepParameters.Load;
	FRipGrepperSearchFormSettings.Load;
	FExtensionSettings.Load;
	FIsLoaded := True;
end;

procedure TRipGrepperSettingsDefaults.LoadDefault;
begin
	inherited;
	FRipGrepParameters.Load;
	FExtensionSettings.Load;
	FRipGrepperSearchFormSettings.Load;
	FIsLoaded := True;
end;

procedure TRipGrepperSettingsDefaults.Store;
begin
	//
end;

procedure TRipGrepperSettingsDefaults.StoreAsDefault;
begin
	// if IsLoaded and IsModified then begin
	FRipGrepParameters.StoreAsDefault;
	FExtensionSettings.StoreAsDefault;
	FRipGrepperSearchFormSettings.StoreAsDefault;
	inherited;
	// end;
end;

end.
