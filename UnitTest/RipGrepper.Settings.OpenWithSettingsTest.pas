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
	count: Integer;
	dictVal : string;
	section : string;
	settingVal : string;
begin
	var
	dbgArr := TSettingsDictionary.DictToStringArray(FSettings.SettingsDict());
	section := FSettings.OpenWithSettings.IniSectionName;
	count := FSettings.OpenWithSettings.Count;
	for var i := 0 to count - 1 do begin
		var
		key := Format('%s%d', [OPENWITH_COMMAND_KEY, i]);

		settingVal := FSettings.OpenWithSettings.Command[i];
		dictVal := FSettings.SettingsDict()[section][key].AsString;

		Assert.AreEqual(settingVal, dictVal, 'Command 0 should be equal');
	end;
end;

procedure TInnerOpenWithSettingsTest.CommandListShouldBePersisted();
var
	iniVal : string;
	settingVal : string;
	i : integer;
begin
	FSettings.OpenWithSettings.ForceUpdateFile;
	i := 0;
	var
	arr := FSettings.OpenWithSettings.GetCommands;
	for settingVal in arr do begin
		var
		key := Format('%s%d', [OPENWITH_COMMAND_KEY, i]);
		iniVal := FFactory.GetStringPersister().LoadValue(FSettings.OpenWithSettings.IniSectionName, key);
		Assert.AreEqual(settingVal, iniVal, key + ' should be persisted.');
		Inc(i);
	end;
end;

initialization

TDUnitX.RegisterTestFixture(TInnerOpenWithSettingsTest);

end.
