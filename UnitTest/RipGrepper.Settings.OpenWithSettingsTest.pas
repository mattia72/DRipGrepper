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
			[Test]
			procedure CommandListShouldBePersisted();
	end;

implementation

uses
	RipGrepper.Settings.SettingsDictionary,
	RipGrepper.OpenWith.Constants,
	System.SysUtils;

procedure TInnerOpenWithSettingsTest.CommandListShouldBeFilledWithDefaults();
begin
	var
	dbgArr := TSettingsDictionary.DictToStringArray(FSettings.SettingsDict());

	var
	arr := FSettings.OpenWithSettings.GetCommands;
	Assert.AreEqual(Length(arr), Length(DEFAULT_EDITORS), 'Default Editors should be equal');
end;

procedure TInnerOpenWithSettingsTest.CommandListShouldBeFilledInDict();
var
	dictVal : string;
	settingVal : string;
begin
	for var i := 0 to FSettings.OpenWithSettings.Count do begin
		var
		key := Format('%s%d', [OPENWITH_COMMAND_KEY, i]);

		settingVal := FSettings.OpenWithSettings.Command[i];
		dictVal := FSettings.SettingsDict()[FSettings.OpenWithSettings.IniSectionName][key].AsString;

		Assert.AreEqual(settingVal, dictVal, 'Command 0 should be equal');
	end;
end;

procedure TInnerOpenWithSettingsTest.CommandListShouldBePersisted();
var
	iniVal : string;
	settingVal : string;
	i : integer;
begin
	FSettings.UpdateIniFile();
	i := 0;
	for settingVal in FSettings.OpenWithSettings.GetCommands do begin
		var
		key := Format('%s%d', [OPENWITH_COMMAND_KEY, i]);
		iniVal := FFactory.GetStringPersister(FSettings.OpenWithSettings.IniSectionName, key).LoadFromFile;
		Assert.AreEqual(settingVal, iniVal, key + ' should be persisted.');
		Inc(i);
	end;
end;

initialization

TDUnitX.RegisterTestFixture(TInnerOpenWithSettingsTest);

end.
