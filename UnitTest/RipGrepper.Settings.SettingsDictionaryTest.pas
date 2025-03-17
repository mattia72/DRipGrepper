unit RipGrepper.Settings.SettingsDictionaryTest;

interface

uses
	DUnitX.TestFramework,
	System.Classes,
	RipGrepper.Settings.SettingsDictionary,
	RipGrepper.Settings.SettingVariant,
	Spring,
	System.IniFiles,
	RipGrepper.Settings.FilePersister;

type

	[TestFixture]
	TSettingsDictionaryTest = class
		FDictTo : IShared<TSettingsDictionary>;
		FDictFrom : IShared<TSettingsDictionary>;
		FIniFile : IShared<TMemIniFile>;
		FPersisterFactory : IPersisterFactory;

		private
		public
			[Test]
			procedure CopySectionShouldCopySettingValues();
			[Test]
			procedure SectionShouldContainAddedSettings();
			[Test]
			procedure SectionShouldNotContainModifiedSettings();
			[Test]
			procedure SectionSholudContainModifiedSettings();
			[Test]
			procedure SectionSholudContainStoredSetting();
			[Test]
			procedure SectionSholudContainSavedSetting();
			[Setup]
			procedure Setup();
			[TearDown]
			procedure TearDown();
	end;

implementation

uses
	System.SysUtils,

	System.TypInfo,
	Spring.Collections,

	RipGrepper.Helper.MemIniFile,
	RipGrepper.Tools.FileUtils,
	Vcl.Forms,
	RipGrepper.Settings.Persistable;

const
	TESTVALUE = 'TestValue';
	TESTKEY = 'TestKey';
	TESTSECTION = 'TestSection';
	MAX_COUNT = 5;

procedure TSettingsDictionaryTest.CopySectionShouldCopySettingValues();
var
	key, expected, actual : string;
begin
	FDictTo.CopySection(TESTSECTION, FDictFrom);

	for var i := 0 to MAX_COUNT do begin
		key := Format('%s_%d', [TESTKEY, i]);
		expected := Format('%s_%d', [TESTVALUE, i]);

		actual := ISettingVariant<string>(FDictTo[TESTSECTION][key]).Value;
		Assert.AreEqual(expected, actual, 'The value should match the copied value.');
	end;
end;

procedure TSettingsDictionaryTest.SectionShouldContainAddedSettings();
var
	key, value, expected, actual : string;
begin
	for var i := 0 to MAX_COUNT do begin
		key := Format('%s_%d', [TESTKEY, i]);
		value := Format('%s_%d', [TESTVALUE, i]);
		expected := value;
		actual := ISettingVariant<string>(FDictFrom[TESTSECTION][key]).Value;
		Assert.AreEqual(expected, actual, 'The value should match the added value in source dictionary.');
	end;
end;

procedure TSettingsDictionaryTest.SectionShouldNotContainModifiedSettings();
var
	actual : Boolean;
begin
	actual := FDictFrom.HasState(ssInitialized);
	Assert.IsTrue(actual, 'There should be initialised settings');
	actual := FDictFrom.HasState(ssModified);
	Assert.IsFalse(actual, 'There shouldn''t be modified settings');
end;

procedure TSettingsDictionaryTest.SectionSholudContainModifiedSettings();
var
	actual : Boolean;
	setting : IStringSetting;
begin
	setting := FDictFrom[TESTSECTION][TESTKEY + '_2'].AsStringSetting;
	setting.Value := 'modified value';
	actual := FDictFrom.HasState(ssInitialized);
	Assert.IsTrue(actual, 'There should be initialised settings');
	actual := FDictFrom.HasState(ssModified);
	Assert.IsTrue(actual, 'There shouldn''t be modified settings');
end;

procedure TSettingsDictionaryTest.SectionSholudContainStoredSetting();
var
	actual : Boolean;
	setting : IStringSetting;
begin
	setting := FDictFrom[TESTSECTION][TESTKEY + '_2'].AsStringSetting;
	setting.Value := 'modified value';
	FDictFrom.StoreToPersister();
	actual := FDictFrom.HasState(ssStored);
	Assert.IsTrue(actual, 'There should be stored settings');
end;

procedure TSettingsDictionaryTest.SectionSholudContainSavedSetting();
var
	actual : Boolean;
	setting : IStringSetting;
begin
	setting := FDictFrom[TESTSECTION][TESTKEY + '_2'].AsStringSetting;
	setting.Value := 'modified value';
	FDictFrom.StoreToPersister();

	TPersistableSettings.CallUpdateFileOnFactory(FPersisterFactory, FDictFrom);

	actual := FDictFrom.HasState(ssSaved);
	Assert.IsTrue(actual, 'There should be saved settings');
end;

procedure TSettingsDictionaryTest.Setup();
var
	keyDict : ISettingKeys;
	setting : IStringSetting;
	key : string;
	value : string;
begin
	FPersisterFactory := TIniPersister.Create();

	FDictTo := Shared.Make<TSettingsDictionary>(TSettingsDictionary.Create(TESTSECTION));
	FDictFrom := Shared.Make<TSettingsDictionary>(TSettingsDictionary.Create(TESTSECTION));
	var
	tmpFile := ChangeFileExt(Application.ExeName, '.ini');
	FIniFile := Shared.Make<TMemIniFile>(
		{ } TMemIniFile.Create(tmpFile, TEncoding.UTF8));

	keyDict := TCollections.CreateSortedDictionary<string, ISetting>;
	for var i := 0 to MAX_COUNT do begin
		key := Format('%s_%d', [TESTKEY, i]);
		value := Format('%s_%d', [TESTVALUE, i]);
		setting := TStringSetting.Create(value);
		setting.Persister := TMemIniStringPersister.Create(FIniFile, 'TestSection', key);
		keyDict.Add(key, setting);
	end;
	FDictFrom.InnerDictionary.Add(TESTSECTION, keyDict);
end;

procedure TSettingsDictionaryTest.TearDown();
begin
	TFileUtils.EmptyFile(ChangeFileExt(Application.ExeName, '.ini'));
end;

initialization

TDUnitX.RegisterTestFixture(TSettingsDictionaryTest);

end.
