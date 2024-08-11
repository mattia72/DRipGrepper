unit RipGrepper.Common.Settings.RipGrepperSettingsDefaults;

interface

uses
	RipGrepper.Common.Settings,
	RipGrepper.Common.Settings.Base,
	RipGrepper.Common.Settings.RipGrepperSearchFormSettings;

type
	TRipGrepperSettingsDefaults = class(TRipGrepperSettingsBase)
		strict private
		const
			INI_SECTION = 'RipGrepperSettingsDefaults';

		var
			FRipGrepperSearchFormSettings : TRipGrepperSearchFormSettings;
			FExtensionSettings : TRipGrepperExtensionSettings;
			FSearchPath : string;
			procedure SetSearchPath(const Value : string);

		public
			constructor Create;
			destructor Destroy; override;
			procedure Init; override;
			procedure Load; override;
			procedure Store; override;

			property ExtensionSettings : TRipGrepperExtensionSettings read FExtensionSettings write FExtensionSettings;
			property RipGrepperSearchFormSettings : TRipGrepperSearchFormSettings read FRipGrepperSearchFormSettings
				write FRipGrepperSearchFormSettings;
			property SearchPath : string read FSearchPath write SetSearchPath;
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

procedure TRipGrepperSettingsDefaults.Init;
begin
	inherited;
	CreateSetting('SearchPath', TRipGrepperSetting.New(vtBoolean, False));
end;

procedure TRipGrepperSettingsDefaults.Load;
begin
	inherited;
	TDebugUtils.DebugMessage('TRipGrepperSettingsDefaults.Load: start');
	FSearchPath := LoadSetting('SearchPath');
	FRipGrepperSearchFormSettings.Load;
	FExtensionSettings.Load;
	FIsLoaded := True;
end;

procedure TRipGrepperSettingsDefaults.SetSearchPath(const Value : string);
begin
	if FSearchPath <> Value then begin
		FSearchPath := Value;
		FIsModified := True;
	end;
end;

procedure TRipGrepperSettingsDefaults.Store;
begin
	inherited;
	if IsLoaded and IsModified then begin
		FIniFile.WriteString(INI_SECTION, 'SearchPath', SearchPath);

		FExtensionSettings.Store;
		FRipGrepperSearchFormSettings.Store;
	end;
end;

end.
