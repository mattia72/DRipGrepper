unit RipGrepper.Settings.PersistableSettingsTest;

interface

uses
	RipGrepper.Settings.SearchFormSettings,
	System.IniFiles,
	RipGrepper.Settings.Persistable,
	DUnitX.TestFramework,
	RipGrepper.Settings.SettingsDictionary,
	RipGrepper.Settings.TestOwnerSettings;


type

	[TestFixture]
	TPersistableSettingsTest = class

		private
			FIniFile : TMemIniFile;
			FOwner : TPersistableSettings;
			FSettings1: TTestSettings;
			FSettings2 : TTestSettings;
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
	FSettings1.LoadDefaultsFromDict;
	Assert.AreEqual(INITIAL_STR_VALUE, FSettings1.StrSetting);
end;

procedure TPersistableSettingsTest.LoadDefaultsShouldReadDefaultFromIni;
begin
	CreateDefaultsInIni;
	FSettings1.ReadIni;
	Assert.AreEqual('', FSettings1.StrSetting, 'StrSetting should be empty');
	FSettings1.LoadFromDict();
	Assert.AreEqual(DEFAULT_STR_VAL, FSettings1.StrSetting, 'If only default value is in the ini file, then StrSetting should be ' +
		DEFAULT_STR_VAL);

	FSettings1.LoadDefaultsFromDict;
	Assert.AreEqual(DEFAULT_STR_VAL, FSettings1.StrSetting, 'StrSetting should be default ' + DEFAULT_STR_VAL);
end;

procedure TPersistableSettingsTest.AfterCopyValuesValuesShouldBeEqual;
begin
	CreateDefaultsInIni;
	FSettings1.LoadDefaultsFromDict;
	var
	s := TTestSettings.Create(INI_SECTION);
	try
		s.Copy(FSettings1);
		Assert.AreEqual(s.StrSetting, FSettings1.StrSetting, 'StrSetting should be equal');
	finally
		s.Free;
	end;
end;

procedure TPersistableSettingsTest.AfterCopyDefaultsToValuesSetsDefaultValueAndCreatesNewKey;
begin
	CreateDefaultsInIni;
	FSettings1.ReadIni;
	FSettings1.LoadDefaultsFromDict;

	Assert.AreEqual(DEFAULT_STR_VAL, VarToStr(FSettings1.SettingsDict.GetSetting('StrSetting' + DEFAULT_KEY)),
		'StrSetting_DEFAULT.Value should be equal');
	// Assert.AreEqual(DEFAULT_STR_VAL, VarToStr(FSettings1.GetSetting('StrSetting' + DEFAULT_KEY, True)),
	// 'StrSetting_DEFAULT.DefaultValue should be equal');

	FSettings1.CopyDefaultsToValues;
	Assert.AreEqual(DEFAULT_STR_VAL, VarToStr(FSettings1.SettingsDict.GetSetting('StrSetting')), 'StrSetting.Value should be equal');
	Assert.AreEqual(DEFAULT_STR_VAL, VarToStr(FSettings1.SettingsDict.GetSetting('StrSetting', True)), 'StrSetting.Default should be equal');
	// it is in fact StrSetting_DEFAULT.Value;
	Assert.AreEqual(VarToStr(FSettings1.SettingsDict.GetSetting('StrSetting' + DEFAULT_KEY)),
		VarToStr(FSettings1.SettingsDict.GetSetting('StrSetting', True)), 'StrSetting.Default should be equal StrSetting_DEFAULT.Value');

end;

procedure TPersistableSettingsTest.AfterCopyDefaultValuesShouldBeEqual;
begin
	var
	s := TTestSettings.Create(INI_SECTION);
	try
		s.Copy(FSettings1);
		Assert.AreEqual(s.StrSetting, FSettings1.StrSetting, 'StrSetting should be equal');
	finally
		s.Free;
	end;
end;

procedure TPersistableSettingsTest.LoadDefaultsReadsIni;
begin
	CreateDefaultsInIni;
	Assert.IsFalse(FSettings1.IsAlreadyRead);
	FSettings1.ReadIni;
	FSettings1.LoadDefaultsFromDict;
	Assert.IsTrue(FSettings1.IsAlreadyRead);
end;

procedure TPersistableSettingsTest.InitCreatesKeysInDict;
begin
	Assert.AreEqual( { default_members*2+normal_members } 2, FSettings1.GetDict.Count);
end;

procedure TPersistableSettingsTest.CreateDefaultsInIni;
begin
	var
	sec := FSettings1.IniSectionName;
	FIniFile.WriteString(sec, 'StrSetting' + DEFAULT_KEY, DEFAULT_STR_VAL);
	Assert.AreEqual('empty', FIniFile.ReadString(sec, 'StrSetting', 'empty'), 'StrSetting should not exist in the ini file');
end;

procedure TPersistableSettingsTest.Setup;
begin
	FOwner := TTestOwnerSettings.Create();
	FIniFile := FOwner.IniFile;
	FSettings1 := TTestSettings.Create(FOwner);
	FSettings2 := TTestSettings.Create(FOwner);
end;

procedure TPersistableSettingsTest.TearDown;
begin
	FSettings1.Free;
    FSettings2.Free;
	FOwner.Free;
end;

initialization

TDUnitX.RegisterTestFixture(TPersistableSettingsTest);

end.
