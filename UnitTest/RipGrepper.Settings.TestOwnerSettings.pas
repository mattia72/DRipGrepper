unit RipGrepper.Settings.TestOwnerSettings;

interface

uses
	RipGrepper.Settings.Persistable,
	RipGrepper.Settings.SettingsDictionary;

const

	INIFILE = 'PersistableSettingsTest.ini';
	INI_SECTION = 'TestSettings';

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
			procedure LoadFromDict(); override;
			procedure LoadDefaultsFromDict; override;
			procedure StoreToDict; override;
			procedure StoreAsDefaultsToDict; override;

			property StrSetting : string read FStrSetting write FStrSetting;
	end;

type
	TTestSettings = class(TPersistableSettings)
		private
			FStrSetting1 : string;
			FStrSetting2 : string;

		public
			constructor Create(const _Owner : TPersistableSettings); overload;
			constructor Create(const _iniSection : string); overload;
			function GetDict : TSettingsDictionary;
			procedure Init; override;
			procedure ReadIni; override;
			procedure LoadFromDict(); override;
			procedure LoadDefaultsFromDict; override;
			procedure StoreToDict; override;
			procedure StoreAsDefaultsToDict; override;

			property StrSetting : string read FStrSetting1 write FStrSetting1;
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

procedure TTestOwnerSettings.LoadFromDict;
begin
	//
end;

procedure TTestOwnerSettings.LoadDefaultsFromDict;
begin
	//
end;

procedure TTestOwnerSettings.StoreToDict;
begin
	//
end;

procedure TTestOwnerSettings.StoreAsDefaultsToDict;
begin
	//
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

procedure TTestSettings.Init;
begin
	SettingsDict.CreateDefaultRelevantSetting('StrSetting', vtString, INITIAL_STR_VALUE);
end;

procedure TTestSettings.ReadIni;
begin
	inherited ReadIni;
end;

procedure TTestSettings.LoadFromDict;
begin
	StrSetting := SettingsDict.GetSetting('StrSetting');
end;

procedure TTestSettings.LoadDefaultsFromDict;
begin
	StrSetting := SettingsDict.GetSetting('StrSetting', True);
end;

procedure TTestSettings.StoreToDict;
begin
	SettingsDict.StoreSetting('StrSetting', StrSetting);
end;

procedure TTestSettings.StoreAsDefaultsToDict;
begin
	SettingsDict.StoreDefaultSetting('StrSetting', StrSetting);
end;

end.
