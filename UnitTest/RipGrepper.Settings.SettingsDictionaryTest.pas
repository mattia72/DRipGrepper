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
	dictTo : IShared<TSettingsDictionary>;
	dictFrom : IShared<TSettingsDictionary>;
	keyDict : ISettingKeys;
	key, expected, actual : string;
	setting : IStringSetting;
begin
	dictTo := Shared.Make<TSettingsDictionary>(TSettingsDictionary.Create(TESTSECTION));
	dictFrom := Shared.Make<TSettingsDictionary>(TSettingsDictionary.Create(TESTSECTION));

	key := 'TestKey';
	setting := TStringSetting.Create('TestValue');

	keyDict := TCollections.CreateSortedDictionary<string, ISetting>;
	keyDict.Add(key, setting);
	dictFrom.InnerDictionary.Add(TESTSECTION, keyDict);

	expected := setting.Value;
    actual := ISettingVariant<string>(dictFrom[TESTSECTION][key]).Value;

	Assert.AreEqual(expected, actual, 'The value should match the added value in source dictionary.');

	dictTo.CopySection(TESTSECTION, dictFrom);

	actual := ISettingVariant<string>(dictTo[TESTSECTION ][key]).Value;
	Assert.AreEqual(expected, actual, 'The value should match the copied value.');
end;

initialization

TDUnitX.RegisterTestFixture(TSettingsDictionaryTest);

end.
