unit RipGrepper.Settings.SettingsDictionaryTest;

interface

uses
	DUnitX.TestFramework,
	System.Classes,
	RipGrepper.Settings.SettingsDictionary;

type

	[TestFixture]
	TSettingsDictionaryTest = class
		public
			[Test]
			procedure AddOrSetTest();

			[Test]
			procedure AddOrChangeTest();
	end;

implementation

uses
	System.SysUtils,
	System.IniFiles,
	RipGrepper.Settings.SettingVariant;

const
	TESTSECTION = 'TestSection';

procedure TSettingsDictionaryTest.AddOrSetTest();
var
	dict : TSettingsDictionary;
	setting : ISettingVariant;
begin
	dict := TSettingsDictionary.Create(TESTSECTION);
	try
		var key := 'TestKey';
		setting := TSettingVariant.Create('TestValue');
		dict.AddOrSet(key, setting);
		var expected := setting.Value;
		var actual := dict[TESTSECTION + '|' + key].Value;

		Assert.AreEqual(expected, actual, '');
	finally
		dict.Free();
	end;
end;

procedure TSettingsDictionaryTest.AddOrChangeTest();
var
	dict : TSettingsDictionary;
	key, expected, actual : string;
	setting : ISettingVariant;
begin
	dict := TSettingsDictionary.Create(TESTSECTION);
	try
		key := 'TestKey';
		setting := TSettingVariant.Create('TestValue');

		dict.CreateSetting(key, setting);

		expected := setting.Value;
		actual := dict[TESTSECTION + '|' + key].Value;
		Assert.AreEqual(expected, actual, 'The value should match the added value.');

		setting.Value := 'NewValue';
		dict.AddOrChange(key, setting);
		expected := setting.Value;
		actual := dict[TESTSECTION + '|' + key].Value;
		Assert.AreEqual(expected, actual, 'The value should match the changed value.');
		Assert.AreEqual(1, dict.Count, 'Dict should have only one item.');
	finally
		dict.Free();
	end;
end;

initialization

TDUnitX.RegisterTestFixture(TSettingsDictionaryTest);

end.
