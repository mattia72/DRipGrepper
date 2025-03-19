unit RipGrepper.Settings.TestOwnerSettings;

interface

uses
	RipGrepper.Settings.Persistable,
	RipGrepper.Settings.SettingsDictionary,
	RipGrepper.Settings.SettingVariant;

const

	INIFILE = 'PersistableSettingsTest.ini';
	INI_SECTION = 'TestSettings';
	OWNER_INI_SECTION = 'TestOwnerSettings';

	INITIAL_STR_VALUE = 'str_initial_value';
	DEFAULT_STR_VAL = 'default_str_val';

type
	TTestOwnerSettings = class(TPersistableSettings)
		private
			FStrSetting : string;

		public
			constructor Create(const _Owner : TPersistableSettings); overload;
			constructor Create(const _iniSection : string); overload;
			function GetDict : TSettingsDictionary;
			procedure Init; override;
			procedure ReadIni; override;
			property StrSetting : string read FStrSetting write FStrSetting;
	end;

type
	TTestSettings = class(TPersistableSettings)
		private
			FStrSetting1 : IStringSetting;
			function GetStrSetting() : string;
			procedure SetStrSetting(const Value : string);
			// FStrSetting2 : string;

		public
			constructor Create(const _Owner : TPersistableSettings); overload;
			constructor Create(const _iniSection : string); overload;
			function GetDict : TSettingsDictionary;
			procedure Init; override;
			procedure ReadIni; override;
			property StrSetting : string read GetStrSetting write SetStrSetting;
	end;

implementation

constructor TTestOwnerSettings.Create(const _Owner : TPersistableSettings);
begin
	IniSectionName := 'dummy owner'; // should be set before create
	inherited Create(_Owner);
end;

constructor TTestOwnerSettings.Create(const _iniSection : string);
begin
	IniSectionName := _iniSection; // as in  GUISearchTextParams
	inherited Create();
end;

function TTestOwnerSettings.GetDict : TSettingsDictionary;
begin
	Result := FSettingsDict;
end;

procedure TTestOwnerSettings.Init;
begin
	//
end;

procedure TTestOwnerSettings.ReadIni;
begin
	inherited ReadIni;
end;

constructor TTestSettings.Create(const _Owner : TPersistableSettings);
begin
	IniSectionName := INI_SECTION; // should be set before create
	inherited Create(_Owner);
end;

constructor TTestSettings.Create(const _iniSection : string);
begin
	IniSectionName := _iniSection; // as in  GUISearchTextParams
	inherited Create();
end;

function TTestSettings.GetDict : TSettingsDictionary;
begin
	Result := FSettingsDict;
end;

function TTestSettings.GetStrSetting() : string;
begin
	Result := FStrSetting1.Value;
end;

procedure TTestSettings.Init;
begin
	FStrSetting1 := TStringSetting.Create(INITIAL_STR_VALUE);
	CreateSetting('StrSetting', FStrSetting1);
end;

procedure TTestSettings.ReadIni;
begin
	inherited ReadIni;
end;

procedure TTestSettings.SetStrSetting(const Value : string);
begin
	FStrSetting1.Value := Value;
end;

end.
