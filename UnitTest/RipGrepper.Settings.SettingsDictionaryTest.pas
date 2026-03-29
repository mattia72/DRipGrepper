unit RipGrepper.Settings.SettingsDictionaryTest;

interface

uses
	DUnitX.TestFramework,
	System.Classes,
	RipGrepper.Settings.SettingsDictionary,
	RipGrepper.Settings.SettingVariant,
	Spring,
	System.IniFiles,
	RipGrepper.Settings.FilePersister,
	RipGrepper.Settings.Persister.Interfaces,
	RipGrepper.Common.Constants;

type

	[TestFixture]
	TSettingsDictionaryTest = class
		private
			FDictTo : IShared<TSettingsDictionary>;
			FDictFrom : IShared<TSettingsDictionary>;
			FIniFile : IShared<TMemIniFile>;
			FPersisterFactory : IPersisterFactory;
			FSettingsDict : TSettingsDictionary;
			FStream : TStringStream;
			FStreamReader : TStreamReader;
			FStreamWriter : TStreamWriter;

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
			[Test]
			procedure TestSaveToStreamWriter();
			[Test]
			procedure TestLoadFromStreamReader();
			[Test]
			procedure AddOrChangeWithExplicitSectionShouldUseThatSection();
			[Test]
			procedure AddOrChangeWithEmptySectionShouldUseSectionName();
			[Test]
			procedure NewSectionShouldDefaultToSstOwn();
			[Test]
			procedure CreateSettingWithStrArrayShouldMarkSectionAsChildArray();
			[Test]
			procedure StoreToPersisterShouldSkipChildArraySection();
			[Test]
			procedure StoreToPersisterShouldStoreOwnSection();
			[Test]
			procedure CopySectionShouldPropagateSectionType();
			[Test]
			procedure SetSectionTypeShouldOverrideDefault();
			[Test]
			procedure GetSectionTypeForUnknownSectionShouldReturnSstOwn();
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
	RipGrepper.Settings.Persistable,
	RipGrepper.Helper.SettingStoreBehaviours;

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

	for var i : Integer := 0 to MAX_COUNT do begin
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
	for var i : Integer := 0 to MAX_COUNT do begin
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
	FDictFrom.StoreToPersister(TESTSECTION);
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
	FDictFrom.StoreToPersister(TESTSECTION);

	TPersistableSettings.CallUpdateFileOnFactory(FPersisterFactory, FDictFrom);

	actual := FDictFrom.HasState(ssSaved);
	Assert.IsTrue(actual, 'There should be saved settings');
end;

procedure TSettingsDictionaryTest.TestSaveToStreamWriter();
var
	setting : ISetting;
begin
	// Arrange
	setting := TStringSetting.Create('TestKey', 'TestValue');
	FSettingsDict.AddOrChange('TestSection', 'TestKey', setting);

	// Act
	FSettingsDict.SaveToStreamWriter(FStreamWriter);
	FStream.Position := 0;

	// Assert
	Assert.AreEqual('1', FStreamReader.ReadLine, 'Section count mismatch');
	Assert.AreEqual('TestSection', FStreamReader.ReadLine, 'Section name mismatch');
	Assert.AreEqual('1', FStreamReader.ReadLine, 'Key count mismatch');
	Assert.AreEqual('TestKey', FStreamReader.ReadLine, 'Key name mismatch');
	Assert.AreEqual(IntToStr(Integer(stString)), FStreamReader.ReadLine, 'Setting type mismatch');
	Assert.AreEqual(TSettingStoreBehavioursHelper.ToString(setting.SaveBehaviour), FStreamReader.ReadLine, 'Setting type mismatch');
	Assert.AreEqual(Ord(setting.State).ToString, FStreamReader.ReadLine, 'Setting type mismatch');
	Assert.AreEqual('TestValue', FStreamReader.ReadLine, 'Setting value mismatch');
end;

procedure TSettingsDictionaryTest.TestLoadFromStreamReader();
var
	setting : ISetting;
begin
	// Arrange
	TestSaveToStreamWriter();
	FStream.Position := 0;

	// Act
	FSettingsDict.LoadFromStreamReader(FStreamReader);

	// Assert
	Assert.AreEqual(1, FSettingsDict.Count, 'Section count mismatch');
	Assert.IsTrue(FSettingsDict.ContainsSection('TestSection'), 'Section not found');
	setting := FSettingsDict['TestSection']['TestKey'];
	Assert.IsNotNull(setting, 'Setting not found');

	Assert.AreEqual(Ord(stString), Ord(setting.SettingType), 'SettingType mismatch');
	Assert.AreEqual(TSettingStoreBehavioursHelper.ToString([TSettingStoreBehaviour.ssbStoreIfModified]), TSettingStoreBehavioursHelper.ToString(setting.SaveBehaviour),
		'Setting SaveBehaviour mismatch');
	Assert.AreEqual(Ord(TSettingState.ssModified), Ord(setting.State), 'Setting State mismatch');

	Assert.AreEqual('TestValue', setting.AsString, 'Setting value mismatch');
end;

procedure TSettingsDictionaryTest.AddOrChangeWithExplicitSectionShouldUseThatSection();
var
	dict : TSettingsDictionary;
	setting : ISetting;
begin
	dict := TSettingsDictionary.Create('DefaultSection', FPersisterFactory);
	try
		setting := TStringSetting.Create('Key1', 'Value1');
		// Pass explicit section different from the dictionary SectionName
		dict.AddOrChange('ExplicitSection', 'Key1', setting);

		Assert.IsTrue(dict.ContainsSection('ExplicitSection'),
			{ } 'Should contain the explicit section');
		Assert.IsFalse(dict.ContainsSection('DefaultSection'),
			{ } 'Should not fall back to default SectionName');
		Assert.AreEqual('Value1', dict['ExplicitSection']['Key1'].AsString,
			{ } 'Value should match');
	finally
		dict.Free;
	end;
end;

procedure TSettingsDictionaryTest.AddOrChangeWithEmptySectionShouldUseSectionName();
var
	dict : TSettingsDictionary;
	setting : ISetting;
begin
	dict := TSettingsDictionary.Create('DefaultSection', FPersisterFactory);
	try
		setting := TStringSetting.Create('Key1', 'Value1');
		// Pass empty section - should fall back to SectionName
		dict.AddOrChange('', 'Key1', setting);

		Assert.IsTrue(dict.ContainsSection('DefaultSection'),
			{ } 'Should use default SectionName when section is empty');
		Assert.AreEqual('Value1', dict['DefaultSection']['Key1'].AsString,
			{ } 'Value should match');
	finally
		dict.Free;
	end;
end;

procedure TSettingsDictionaryTest.NewSectionShouldDefaultToSstOwn();
var
	dict : TSettingsDictionary;
	setting : ISetting;
begin
	dict := TSettingsDictionary.Create('MySection', FPersisterFactory);
	try
		setting := TStringSetting.Create('Key1', 'Value1');
		dict.AddOrChange('MySection', 'Key1', setting);

		Assert.AreEqual(Ord(sstOwn), Ord(dict.GetSectionType('MySection')),
			{ } 'New section should default to sstOwn');
	finally
		dict.Free;
	end;
end;

procedure TSettingsDictionaryTest.CreateSettingWithStrArrayShouldMarkSectionAsChildArray();
var
	dict : TSettingsDictionary;
	arrSetting : IArraySetting;
begin
	dict := TSettingsDictionary.Create('ParentSection', FPersisterFactory);
	try
		arrSetting := TArraySetting.Create('ArraySection');
		arrSetting.Add('item1');
		arrSetting.Add('item2');

		dict.CreateSetting('ArraySection', 'Item_', arrSetting, FPersisterFactory);

		Assert.AreEqual(Ord(sstChildArray), Ord(dict.GetSectionType('ArraySection')),
			{ } 'Section created via str array should be marked as sstChildArray');
	finally
		dict.Free;
	end;
end;

procedure TSettingsDictionaryTest.StoreToPersisterShouldSkipChildArraySection();
var
	dict : TSettingsDictionary;
	arrSetting : IArraySetting;
	iniVal : string;
begin
	// Use ROOT_DUMMY_INI_SECTION as SectionName to trigger the root iteration path
	dict := TSettingsDictionary.Create(ROOT_DUMMY_INI_SECTION, FPersisterFactory);
	try
		// Add an array section that should be skipped during root iteration
		arrSetting := TArraySetting.Create('ChildArraySection');
		arrSetting.Add('item1');
		arrSetting.State := ssModified;
		dict.CreateSetting('ChildArraySection', 'Item_', arrSetting, FPersisterFactory);

		// Verify section type is sstChildArray
		Assert.AreEqual(Ord(sstChildArray), Ord(dict.GetSectionType('ChildArraySection')),
			{ } 'Array section should be sstChildArray');

		// StoreToPersister with ROOT_DUMMY should skip child array sections
		dict.StoreToPersister(ROOT_DUMMY_INI_SECTION);

		// Verify that item was NOT stored to persister
		iniVal := FPersisterFactory.GetStringPersister().LoadValue('ChildArraySection', 'Item_0');
		Assert.AreEqual('', iniVal,
			{ } 'Child array items should not be stored by root dict during ROOT_DUMMY iteration');
	finally
		dict.Free;
	end;
end;

procedure TSettingsDictionaryTest.StoreToPersisterShouldStoreOwnSection();
var
	dict : TSettingsDictionary;
	iniVal : string;
begin
	dict := TSettingsDictionary.Create('OwnSection', FPersisterFactory);
	try
		var
		setting : ISetting := TStringSetting.Create('Key1', 'StoredValue');
		setting.State := ssModified;
		dict.CreateSetting('OwnSection', 'Key1', setting, FPersisterFactory);

		Assert.AreEqual(Ord(sstOwn), Ord(dict.GetSectionType('OwnSection')),
			{ } 'Regular section should be sstOwn');

		dict.StoreToPersister('OwnSection');

		iniVal := FPersisterFactory.GetStringPersister().LoadValue('OwnSection', 'Key1');
		Assert.AreEqual('StoredValue', iniVal,
			{ } 'Own section settings should be stored to persister');
	finally
		dict.Free;
	end;
end;

procedure TSettingsDictionaryTest.CopySectionShouldPropagateSectionType();
var
	dictFrom : TSettingsDictionary;
	dictTo : TSettingsDictionary;
	arrSetting : IArraySetting;
begin
	dictFrom := TSettingsDictionary.Create('FromSection', FPersisterFactory);
	dictTo := TSettingsDictionary.Create('ToSection', FPersisterFactory);
	try
		// Create an array section in source
		arrSetting := TArraySetting.Create('SharedArray');
		arrSetting.Add('item1');
		dictFrom.CreateSetting('SharedArray', 'Item_', arrSetting, FPersisterFactory);

		Assert.AreEqual(Ord(sstChildArray), Ord(dictFrom.GetSectionType('SharedArray')),
			{ } 'Source should have sstChildArray');

		// Copy section to target
		dictTo.CopySection('SharedArray', dictFrom);

		Assert.AreEqual(Ord(sstChildArray), Ord(dictTo.GetSectionType('SharedArray')),
			{ } 'Section type should be propagated to target after CopySection');
	finally
		dictFrom.Free;
		dictTo.Free;
	end;
end;

procedure TSettingsDictionaryTest.SetSectionTypeShouldOverrideDefault();
var
	dict : TSettingsDictionary;
	setting : ISetting;
begin
	dict := TSettingsDictionary.Create('TestSection', FPersisterFactory);
	try
		setting := TStringSetting.Create('Key1', 'Value1');
		dict.AddOrChange('TestSection', 'Key1', setting);

		Assert.AreEqual(Ord(sstOwn), Ord(dict.GetSectionType('TestSection')),
			{ } 'Should default to sstOwn');

		dict.SetSectionType('TestSection', sstChildArray);

		Assert.AreEqual(Ord(sstChildArray), Ord(dict.GetSectionType('TestSection')),
			{ } 'SetSectionType should override the default');
	finally
		dict.Free;
	end;
end;

procedure TSettingsDictionaryTest.GetSectionTypeForUnknownSectionShouldReturnSstOwn();
var
	dict : TSettingsDictionary;
begin
	dict := TSettingsDictionary.Create('MySection', FPersisterFactory);
	try
		Assert.AreEqual(Ord(sstOwn), Ord(dict.GetSectionType('NonExistentSection')),
			{ } 'Unknown section should default to sstOwn');
	finally
		dict.Free;
	end;
end;

procedure TSettingsDictionaryTest.Setup();
var
	keyDict : ISettingKeys;
	setting : IStringSetting;
	key, value : string;
begin
	FPersisterFactory := TIniPersister.Create();

	FDictTo := Shared.Make<TSettingsDictionary>(TSettingsDictionary.Create(TESTSECTION, FPersisterFactory));
	FDictFrom := Shared.Make<TSettingsDictionary>(TSettingsDictionary.Create(TESTSECTION, FPersisterFactory));
	var
	tmpFile := ChangeFileExt(Application.ExeName, '.ini');
	FIniFile := Shared.Make<TMemIniFile>(
		{ } TMemIniFile.Create(tmpFile, TEncoding.UTF8));

	keyDict := TCollections.CreateSortedDictionary<string, ISetting>;
	for var i : Integer := 0 to MAX_COUNT do begin
		key := Format('%s_%d', [TESTKEY, i]);
		value := Format('%s_%d', [TESTVALUE, i]);
		setting := TStringSetting.Create(key, value);
		setting.Persister := TMemIniStringPersister.Create(FIniFile, 'TestSection', key);
		keyDict.Add(key, setting);
	end;
	FDictFrom.InnerDictionary.Add(TESTSECTION, keyDict);

	FSettingsDict := TSettingsDictionary.Create('TestSection', FPersisterFactory);
	FStream := TStringStream.Create('', TEncoding.UTF8);
	FStreamReader := TStreamReader.Create(FStream);
	FStreamWriter := TStreamWriter.Create(FStream);
end;

procedure TSettingsDictionaryTest.TearDown();
begin
	TFileUtils.EmptyFile(ChangeFileExt(Application.ExeName, '.ini'));
	FStream.Free;
	FSettingsDict.Free;
	FStreamReader.Free;
	FStreamWriter.Free;
end;

initialization

TDUnitX.RegisterTestFixture(TSettingsDictionaryTest);

end.
