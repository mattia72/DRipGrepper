unit RipGrepper.Common.Settings.PersistableSettingsTest;

interface

uses
	RipGrepper.Common.Settings.SearchFormSettings,
	System.IniFiles,
	RipGrepper.Common.Settings.Persistable,
	DUnitX.TestFramework,
	RipGrepper.Common.Settings.SettingsDictionary;

const
	INITIAL_STR_VALUE = 'str_initial_value';
	DEFAULT_STR_VAL = 'default_str_val';

type

	TTestSettings = class(TPersistableSettings)
		private
			FStrSetting : string;

		public
			constructor Create(const _ini : TMemIniFile); overload;
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

	[TestFixture]
	TPersistableSettingsTest = class
		const
			INIFILE = 'PersistableSettingsTest.ini';
			INI_SECTION = 'TestSettings';

		private
			FIniFile : TMemIniFile;
			FSettings : TTestSettings;
			procedure CreateDefaultsInIni;

		public
			constructor Create;
			destructor Destroy; override;
			[Setup]
			procedure Setup;
			[TearDown]
			procedure TearDown;

			[Test]
			procedure FirstRefreshMembersShouldLoadInitialValue;
			[Test]
			procedure LoadDefaultsShouldReadDefaultFromIni;
			[Test]
			procedure AfterCopyValuesValuesShouldBeEqual;
			[Test]
			procedure AfterCopyDefaultsToValuesSetsDefaultValueAndCreatesNewKey;
			[Test]
			procedure AfterCopyDefaultValuesShouldBeEqual;
			[Test]
			procedure LoadDefaultsReadsIni;
			[Test]
			procedure InitCreatesKeysInDict;

	end;

implementation

uses
	System.SysUtils,
	RipGrepper.Common.Constants,
	System.Variants;

constructor TPersistableSettingsTest.Create;
begin
	inherited;
	// FIniFile := TMemIniFile.Create(INIFILE, TEncoding.UTF8);
end;

destructor TPersistableSettingsTest.Destroy;
begin
	// FIniFile.Free;
	inherited;
end;

procedure TPersistableSettingsTest.FirstRefreshMembersShouldLoadInitialValue;
begin
	FSettings.LoadDefaultsFromDict;
	Assert.AreEqual(INITIAL_STR_VALUE, FSettings.StrSetting);
end;

procedure TPersistableSettingsTest.LoadDefaultsShouldReadDefaultFromIni;
begin
	CreateDefaultsInIni;
	FSettings.ReadIni;
	Assert.AreEqual('', FSettings.StrSetting, 'StrSetting should be empty');
	FSettings.LoadFromDict();
	Assert.AreEqual(DEFAULT_STR_VAL, FSettings.StrSetting, 'If only default value is in the ini file, then StrSetting should be ' +
		DEFAULT_STR_VAL);

	FSettings.LoadDefaultsFromDict;
	Assert.AreEqual(DEFAULT_STR_VAL, FSettings.StrSetting, 'StrSetting should be default ' + DEFAULT_STR_VAL);
end;

procedure TPersistableSettingsTest.AfterCopyValuesValuesShouldBeEqual;
begin
	CreateDefaultsInIni;
	FSettings.LoadDefaultsFromDict;
	var
	s := TTestSettings.Create(TPersistableSettingsTest.INI_SECTION);
	try
		s.Copy(FSettings);
		Assert.AreEqual(s.StrSetting, FSettings.StrSetting, 'StrSetting should be equal');
	finally
		s.Free;
	end;
end;

procedure TPersistableSettingsTest.AfterCopyDefaultsToValuesSetsDefaultValueAndCreatesNewKey;
begin
	CreateDefaultsInIni;
	FSettings.ReadIni;
	FSettings.LoadDefaultsFromDict;

	Assert.AreEqual(DEFAULT_STR_VAL, VarToStr(FSettings.SettingsDict.GetSetting('StrSetting' + DEFAULT_KEY)),
		'StrSetting_DEFAULT.Value should be equal');
	// Assert.AreEqual(DEFAULT_STR_VAL, VarToStr(FSettings.GetSetting('StrSetting' + DEFAULT_KEY, True)),
	// 'StrSetting_DEFAULT.DefaultValue should be equal');

	FSettings.CopyDefaultsToValues;
	Assert.AreEqual(DEFAULT_STR_VAL, VarToStr(FSettings.SettingsDict.GetSetting('StrSetting')), 'StrSetting.Value should be equal');
	Assert.AreEqual(DEFAULT_STR_VAL, VarToStr(FSettings.SettingsDict.GetSetting('StrSetting', True)), 'StrSetting.Default should be equal');
	// it is in fact StrSetting_DEFAULT.Value;
	Assert.AreEqual(VarToStr(FSettings.SettingsDict.GetSetting('StrSetting' + DEFAULT_KEY)),
		VarToStr(FSettings.SettingsDict.GetSetting('StrSetting', True)), 'StrSetting.Default should be equal StrSetting_DEFAULT.Value');

end;

procedure TPersistableSettingsTest.AfterCopyDefaultValuesShouldBeEqual;
begin
	var
	s := TTestSettings.Create(TPersistableSettingsTest.INI_SECTION);
	try
		s.Copy(FSettings);
		Assert.AreEqual(s.StrSetting, FSettings.StrSetting, 'StrSetting should be equal');
	finally
		s.Free;
	end;
end;

procedure TPersistableSettingsTest.LoadDefaultsReadsIni;
begin
	CreateDefaultsInIni;
	Assert.IsFalse(FSettings.IsAlreadyRead);
	FSettings.ReadIni;
	FSettings.LoadDefaultsFromDict;
	Assert.IsTrue(FSettings.IsAlreadyRead);
end;

procedure TPersistableSettingsTest.InitCreatesKeysInDict;
begin
	Assert.AreEqual( { default_members*2+normal_members } 2, FSettings.GetDict.Count);
end;

procedure TPersistableSettingsTest.CreateDefaultsInIni;
begin
	var
	sec := FSettings.IniSectionName;
	FIniFile.WriteString(sec, 'StrSetting' + DEFAULT_KEY, DEFAULT_STR_VAL);
	Assert.AreEqual('empty', FIniFile.ReadString(sec, 'StrSetting', 'empty'), 'StrSetting should not exist in the ini file');
end;

procedure TPersistableSettingsTest.Setup;
begin
	FIniFile := TMemIniFile.Create(INIFILE, TEncoding.UTF8);
	FSettings := TTestSettings.Create(FIniFile);
end;

procedure TPersistableSettingsTest.TearDown;
begin
	FSettings.Free;
	FIniFile.Free;
end;

constructor TTestSettings.Create(const _ini : TMemIniFile);
begin
	IniSectionName := TPersistableSettingsTest.INI_SECTION; // should be set before create
	inherited Create(_ini);
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

initialization

TDUnitX.RegisterTestFixture(TPersistableSettingsTest);

end.
