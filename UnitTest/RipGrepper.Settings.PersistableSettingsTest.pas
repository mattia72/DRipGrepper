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
			procedure AfterCopyValuesValuesShouldBeEqual;
			[Test]
			procedure CopyCreatesNewKey();
			[Test]
			procedure AfterCopyValuesShouldBeEqual();
			[Test]
			procedure DontCopyValueIfSectionNotExists();
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

procedure TPersistableSettingsTest.CopyCreatesNewKey();
begin
	CreateDefaultsInIni;
	FSettings1.ReadIni;

	Assert.AreEqual(DEFAULT_STR_VAL, FSettings1.SettingsDict.GetSetting(STRSETTING).AsString,
		'StrSetting should be equal to value written in the ini file');

	Assert.AreEqual(INITIAL_STR_VALUE, FSettings2.SettingsDict.GetSetting(STRSETTING).AsString,
		'StrSetting should be equal to value written FSetting2');

	FSettings1.Copy(FSettings2);

	Assert.AreEqual(INITIAL_STR_VALUE, FSettings1.SettingsDict.GetSetting(STRSETTING).AsString,
		'StrSetting should be equal to value in FSetting2');
end;

procedure TPersistableSettingsTest.AfterCopyValuesShouldBeEqual();
var
	settings : IShared<TTestSettings>;
begin
	settings := Shared.Make<TTestSettings>(TTestSettings.Create(INI_SECTION));
	settings.Copy(FSettings1);
	Assert.AreEqual(settings.StrSetting, FSettings1.StrSetting, 'StrSetting should be equal');
end;

procedure TPersistableSettingsTest.DontCopyValueIfSectionNotExists();
var
	settings : IShared<TTestSettings>;
begin
	settings := Shared.Make<TTestSettings>(TTestSettings.Create('SomeOTherSection'));
    settings.StrSetting := 'some other value';
	settings.Copy(FSettings1);
	Assert.AreEqual('some other value', settings.StrSetting, 'StrSetting should be equal');
	Assert.AreNotEqual(settings.StrSetting, FSettings1.StrSetting, 'StrSetting should be equal');
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
	FIniFile.WriteString(FSettings1.IniSectionName, STRSETTING, DEFAULT_STR_VAL);
end;

procedure TPersistableSettingsTest.Setup;
begin
	FIniFile := Shared.Make<TMemIniFile>(TMemIniFile.Create(INIFILE, TEncoding.UTF8));

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
