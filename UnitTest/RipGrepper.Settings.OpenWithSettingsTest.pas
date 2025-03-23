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
			[Test]
			procedure ChangedCommandShouldBePersisted();
	end;

implementation

uses
	RipGrepper.Settings.SettingsDictionary,
	RipGrepper.OpenWith.Constants,
	System.SysUtils,
	RipGrepper.Common.Constants;

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
	count : Integer;
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
		key := Format('%s%d', [ITEM_KEY_PREFIX, i]);

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
	var
	dbgArr := TSettingsDictionary.DictToStringArray(FSettings.SettingsDict());

	FSettings.OpenWithSettings.ForceUpdateFile;
	FSettings.StoreToPersister;

	dbgArr := TSettingsDictionary.DictToStringArray(FSettings.SettingsDict());

	var
	section := FSettings.OpenWithSettings.IniSectionName;
	i := 0;
	var
	arr := FSettings.OpenWithSettings.GetCommands;
	for settingVal in arr do begin
		var
		key := Format('%s%d', [ITEM_KEY_PREFIX, i]);
		iniVal := FFactory.GetStringPersister().LoadValue(section, key);
		Assert.AreEqual(settingVal, iniVal, key + ' should be persisted.');
		Inc(i);
	end;
end;

procedure TInnerOpenWithSettingsTest.ChangedCommandShouldBePersisted();
const
	NEW_COMMAND = 'new command';
var
	iniVal : string;
	settingVal : string;
	i : integer;
begin
	var
	dbgArr := TSettingsDictionary.DictToStringArray(FSettings.SettingsDict());
	var
	section := FSettings.OpenWithSettings.IniSectionName;

	var
	ows := FSettings.OpenWithSettings;
	ows.Command[0] := NEW_COMMAND;
	var
	arr := ows.GetCommands;
 	ows.RecreateCommandList(arr);

	FSettings.StoreToPersister;   // TOpenWithConfigForm.WriteSettings() tested here
	ows.ForceUpdateFile;

	dbgArr := TSettingsDictionary.DictToStringArray(FSettings.SettingsDict());

	iniVal := FFactory.GetStringPersister().LoadValue(section, 'Item_0');
	Assert.AreEqual(NEW_COMMAND, iniVal, NEW_COMMAND + ' should be persisted.');

	i := 0;
	for settingVal in arr do begin
		var
		key := Format('%s%d', [ITEM_KEY_PREFIX, i]);
		iniVal := FFactory.GetStringPersister().LoadValue(section, key);
		Assert.AreEqual(settingVal, iniVal, key + ' should be persisted.');
		Inc(i);
	end;
end;

initialization

TDUnitX.RegisterTestFixture(TInnerOpenWithSettingsTest);

end.
