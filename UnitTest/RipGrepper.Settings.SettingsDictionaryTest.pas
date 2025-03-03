unit RipGrepper.Settings.SettingsDictionaryTest;

interface

uses
	DUnitX.TestFramework,
	System.Classes,
	RipGrepper.Settings.SettingsDictionary,
	RipGrepper.Settings.SettingVariant;

type

	[TestFixture]
	TSettingsDictionaryTest = class
		public
			[Test]
			procedure CopySectionTest();
	end;

implementation

uses
	System.SysUtils,
	System.IniFiles,
	Spring,
	System.TypInfo,
	Spring.Collections;

const
	TESTSECTION = 'TestSection';

procedure TSettingsDictionaryTest.CopySectionTest();
var
	dict : IShared<TSettingsDictionary>;
	sourceDict : IShared<TSettingsDictionary>;
	keyDict : ISettingsKeyCollection;
	key, expected, actual : string;
	setting : IShared<TStringSetting>;
begin
	dict := Shared.Make<TSettingsDictionary>(TSettingsDictionary.Create(TESTSECTION));
	sourceDict := Shared.Make<TSettingsDictionary>(TSettingsDictionary.Create('SourceSection'));

	key := 'TestKey';
	setting := Shared.Make<TStringSetting>(TStringSetting.Create('TestValue'));

	keyDict := TCollections.CreateSortedDictionary<string, ISetting>;
	keyDict.Add(key, setting);
	sourceDict.InnerDictionary.Add('SourceSection', keyDict);

	expected := setting.Value;
    actual := ISettingVariant<string>(sourceDict['SourceSection'][key]).Value;

	Assert.AreEqual(expected, actual, 'The value should match the added value in source dictionary.');

	dict.CopySection(TESTSECTION, sourceDict);

	actual := ISettingVariant<string>(dict[TESTSECTION ][key]).Value;
	Assert.AreEqual(expected, actual, 'The value should match the copied value.');
end;

initialization

TDUnitX.RegisterTestFixture(TSettingsDictionaryTest);

end.
