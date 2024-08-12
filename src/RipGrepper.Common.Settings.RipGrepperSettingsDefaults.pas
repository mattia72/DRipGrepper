unit RipGrepper.Common.Settings.RipGrepperSettingsDefaults;

interface

uses
	RipGrepper.Common.Settings,
	RipGrepper.Common.Settings.Persistable,
	RipGrepper.Common.Settings.RipGrepperSearchFormSettings,
	RipGrepper.Common.Settings.RipGrepParameterSettings;

type
	TRipGrepperSettingsDefaults = class(TPersistableSettings)
		var
			FRipGrepperSearchFormSettings : TRipGrepperSearchFormSettings;
			FExtensionSettings : TRipGrepperExtensionSettings;
		private
			function GetRipGrepParameters : TRipGrepParameterSettings;

		protected
			FRipGrepParameters : TRipGrepParameterSettings;
			function GetIniSectionName : string; override;

		public
			constructor Create;
			destructor Destroy; override;
			procedure Init; override;
			procedure Load; override;
			procedure Store; override;
			procedure StoreAsDefault; override;

			property ExtensionSettings : TRipGrepperExtensionSettings read FExtensionSettings write FExtensionSettings;
			property RipGrepParameters : TRipGrepParameterSettings read GetRipGrepParameters write FRipGrepParameters;
			property RipGrepperSearchFormSettings : TRipGrepperSearchFormSettings read FRipGrepperSearchFormSettings
				write FRipGrepperSearchFormSettings;
	end;

implementation

uses
	RipGrepper.Common.IOTAUtils,
	System.IniFiles,
	System.SysUtils,
	System.IOUtils,
	Vcl.Forms,
	RipGrepper.Common.Constants,
	RipGrepper.Tools.DebugUtils;

constructor TRipGrepperSettingsDefaults.Create;
begin
	inherited;
	if IOTAUTils.IsStandAlone then begin
		FIniFile := TMemIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'), TEncoding.UTF8);
	end else begin
		FIniFile := TMemIniFile.Create(TPath.Combine(IOTAUTils.GetSettingFilePath, EXTENSION_NAME + '.ini'), TEncoding.UTF8);
	end;
	FRipGrepperSearchFormSettings := TRipGrepperSearchFormSettings.Create(FIniFile);
	FExtensionSettings := TRipGrepperExtensionSettings.Create(FIniFile);
	FIsLoaded := False;
end;

destructor TRipGrepperSettingsDefaults.Destroy;
begin
	FRipGrepperSearchFormSettings.Free;
	FExtensionSettings.Free;
	inherited;
end;

function TRipGrepperSettingsDefaults.GetIniSectionName : string;
begin
	Result := DEFAULTS_INI_SECTION;
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

procedure TRipGrepperSettingsDefaults.Store;
begin
	inherited;
	if IsLoaded and IsModified then begin
        FRipGrepParameters.Store;
		FExtensionSettings.Store;
		FRipGrepperSearchFormSettings.Store;
	end;
end;

procedure TRipGrepperSettingsDefaults.StoreAsDefault;
begin
	inherited;
	if IsLoaded and IsModified then begin
		FRipGrepParameters.StoreAsDefault;
		FExtensionSettings.StoreAsDefault;
		FRipGrepperSearchFormSettings.StoreAsDefault;
	end;
end;

end.
