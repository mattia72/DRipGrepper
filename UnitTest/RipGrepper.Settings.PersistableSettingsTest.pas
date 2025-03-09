unit RipGrepper.Settings.PersistableSettingsTest;

interface

uses
	RipGrepper.Settings.SearchFormSettings,
	System.IniFiles,
	RipGrepper.Settings.Persistable,
	DUnitX.TestFramework,
	RipGrepper.Settings.SettingsDictionary,
	RipGrepper.Settings.TestOwnerSettings,
	Spring;

type

	[TestFixture]
	TPersistableSettingsTest = class

		private
			FIniFile : IShared<TMemIniFile>;
			FOwner : TPersistableSettings;
			FSettings1 : TTestSettings;
			FSettings2 : TTestSettings;
			procedure CreateDefaultsInIni;

		public
			[Setup]
			procedure Setup;
			[TearDown]
			procedure TearDown;

			[Test]
			procedure LoadFromDictShouldLoadInitialValue();
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
			[Test]
			procedure CopySettingsDictSectionShouldCopyCorrectly;

	end;

implementation

uses
	System.SysUtils,
	RipGrepper.Common.Constants,
	System.Variants,
	RipGrepper.Settings.FilePersister;

const
	STRSETTING = 'StrSetting';
	STRSETTING2 = 'StrSetting2';

procedure TPersistableSettingsTest.LoadFromDictShouldLoadInitialValue();
begin
	FSettings1.LoadFromDict;
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

	// FSettings1.LoadDefaultsFromDict;
	// Assert.AreEqual(DEFAULT_STR_VAL, FSettings1.StrSetting, 'StrSetting should be default ' + DEFAULT_STR_VAL);
end;

procedure TPersistableSettingsTest.AfterCopyValuesValuesShouldBeEqual;
begin
	CreateDefaultsInIni;
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
	FSettings1.LoadFromDict; // FSettings1.LoadDefaultsFromDict;

	Assert.AreEqual(DEFAULT_STR_VAL, VarToStr(FSettings1.SettingsDict.GetSetting('StrSetting' { +DEFAULT_KEY } )),
		'StrSetting_DEFAULT.Value should be equal');
	// Assert.AreEqual(DEFAULT_STR_VAL, VarToStr(FSettings1.GetSetting('StrSetting' {+DEFAULT_KEY}, True)),
	// 'StrSetting_DEFAULT.DefaultValue should be equal');

	// FSettings1.CopyDefaultsToValues;
	Assert.AreEqual(DEFAULT_STR_VAL, VarToStr(FSettings1.SettingsDict.GetSetting('StrSetting')), 'StrSetting.Value should be equal');
	Assert.AreEqual(DEFAULT_STR_VAL, VarToStr(FSettings1.SettingsDict.GetSetting('StrSetting')), 'StrSetting.Default should be equal');
	// it is in fact StrSetting_DEFAULT.Value;
	Assert.AreEqual(VarToStr(FSettings1.SettingsDict.GetSetting('StrSetting' { +DEFAULT_KEY } )),
		VarToStr(FSettings1.SettingsDict.GetSetting('StrSetting')), 'StrSetting.Default should be equal StrSetting_DEFAULT.Value');

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
	Assert.IsTrue(FSettings1.IsAlreadyRead);

	Assert.IsFalse(FSettings2.IsAlreadyRead);
	FSettings2.ReadIni;
	Assert.IsTrue(FSettings2.IsAlreadyRead);
	FSettings2.StrSetting := STRSETTING2;
end;

procedure TPersistableSettingsTest.InitCreatesKeysInDict;
begin
	Assert.AreEqual( { default_members*2+normal_members =2 } 1, FSettings1.GetDict.Count);
end;

procedure TPersistableSettingsTest.CreateDefaultsInIni;
begin
	var
	sec := FSettings1.IniSectionName;
	FIniFile.WriteString(sec, 'StrSetting' { +DEFAULT_KEY } , DEFAULT_STR_VAL);
	Assert.AreEqual('empty', FIniFile.ReadString(sec, STRSETTING, 'empty'), 'StrSetting should not exist in the ini file');
end;

procedure TPersistableSettingsTest.Setup;
begin
	FIniFile := Shared.Make<TMemIniFile>(
    	TMemIniFile.Create(INIFILE, TEncoding.UTF8));

	FOwner := TTestOwnerSettings.Create();
	FOwner.PersisterFactory := TIniPersister.Create(FIniFile);

	FSettings1 := TTestSettings.Create(FOwner);
	FSettings2 := TTestSettings.Create(FSettings1);
end;

procedure TPersistableSettingsTest.TearDown;
begin
	FSettings1.Free;
	FSettings2.Free;
	FOwner.Free;
end;

procedure TPersistableSettingsTest.CopySettingsDictSectionShouldCopyCorrectly;
begin
	LoadDefaultsReadsIni;
	FSettings1.StrSetting := STRSETTING;
	FSettings1.StoreToDict;

	Assert.AreEqual(STRSETTING,
		{ } FSettings1.StrSetting, 'StrSetting should be equal to initial value');

	FSettings1.CopySettingsDictSection(FSettings2);

	FSettings1.LoadFromDict();
	Assert.AreEqual(STRSETTING2,
		{ } FSettings1.StrSetting, 'StrSetting should be copied correctly');
end;

initialization

TDUnitX.RegisterTestFixture(TPersistableSettingsTest);

end.
