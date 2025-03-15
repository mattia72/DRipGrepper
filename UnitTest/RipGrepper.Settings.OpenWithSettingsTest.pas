unit RipGrepper.Settings.OpenWithSettingsTest;

interface

uses
	DUnitX.TestFramework,
	ArrayEx,
	RipGrepper.Settings.RipGrepperSettingsTest;

type

	[TestFixture]
	TInnerOpenWithSettingsTest = class(TRipGrepperSettingsTestBase)
		public
			[Test]
			procedure CommandListShouldBeFilledWithDefaults();
			[Test]
			procedure CommandListShouldBeFilledInDict();
	end;

implementation

uses
	RipGrepper.Settings.SettingsDictionary,
    RipGrepper.OpenWith.Constants;

procedure TInnerOpenWithSettingsTest.CommandListShouldBeFilledWithDefaults();
begin
	var
	dbgArr := TSettingsDictionary.DictToStringArray(FSettings.SettingsDict());

    var arr := FSettings.OpenWithSettings.GetCommands;
	Assert.AreEqual(Length(arr), Length(DEFAULT_EDITORS), 'Default Editors should be equal');
end;

procedure TInnerOpenWithSettingsTest.CommandListShouldBeFilledInDict();
var
	dictVal: string;
	settingVal: string;
begin
	var
	dbgArr := TSettingsDictionary.DictToStringArray(FSettings.SettingsDict());

	settingVal := FSettings.OpenWithSettings.Command[0];
	dictVal := FSettings.SettingsDict()[FSettings.OpenWithSettings.IniSectionName]['Command_0'].AsString;

	Assert.AreEqual(settingVal, dictVal, 'Command 0 should be equal');
end;


// iniVal := FFactory.GetStringPersister(FSettings.SearchFormSettings.IniSectionName, 'Encoding').LoadFromFile;
// settingVal := FSettings.SearchFormSettings.Encoding;

initialization

TDUnitX.RegisterTestFixture(TInnerOpenWithSettingsTest);

end.
