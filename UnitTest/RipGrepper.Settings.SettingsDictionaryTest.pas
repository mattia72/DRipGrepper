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
	RipGrepper.Settings.Persister.Interfaces;

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
