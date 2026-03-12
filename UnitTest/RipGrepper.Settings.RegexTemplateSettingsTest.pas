unit RipGrepper.Settings.RegexTemplateSettingsTest;

interface

uses
	DUnitX.TestFramework,
	System.IniFiles,
	RipGrepper.Settings.Persistable,
	RipGrepper.Settings.RegexTemplateSettings,
	RipGrepper.Settings.TestOwnerSettings,
	Spring;

type

	[TestFixture]
	TRegexTemplateSettingsTest = class
		const
			INIFILE = 'RegexTemplateSettingsTest.ini';

		private
			FIniFile : IShared<TMemIniFile>;
			FOwner : TPersistableSettings;
			FSettings : TRegexTemplateSettings;

		public
			[Setup]
			procedure Setup;
			[TearDown]
			procedure TearDown;

			[Test]
			procedure InitShouldCreateDefaultTemplates;
			[Test]
			procedure PersistableArrayShouldBeAssigned;
			[Test]
			procedure DefaultTemplatesShouldContainThreeItems;
			[Test]
			procedure CopyShouldDuplicateTemplates;
			[Test]
			procedure IniSectionNameShouldBeRegexTemplates;
			[Test]
			procedure CopyWithNilShouldNotRaise;
	end;

implementation

uses
	System.SysUtils,
	RipGrepper.Common.Constants,
	RipGrepper.Settings.FilePersister,
	RipGrepper.Settings.SettingVariant;

procedure TRegexTemplateSettingsTest.Setup;
begin
	FIniFile := Shared.Make<TMemIniFile>(
		{ } TMemIniFile.Create(INIFILE, TEncoding.UTF8));

	FOwner := TTestOwnerSettings.Create();
	FOwner.PersisterFactory := TIniPersister.Create(FIniFile);

	FSettings := TRegexTemplateSettings.Create(FOwner);
end;

procedure TRegexTemplateSettingsTest.TearDown;
begin
	FSettings.Free;
	FOwner.Free;
end;

procedure TRegexTemplateSettingsTest.InitShouldCreateDefaultTemplates;
begin
	// Init is called during Create; verify the array setting has defaults
	Assert.IsNotNull(FSettings.PersistableArray, 'PersistableArray should be assigned after Init');
	var
		arr := FSettings.PersistableArray.ArraySetting;
	Assert.IsTrue(arr.Count > 0, 'Default templates should be populated');
end;

procedure TRegexTemplateSettingsTest.PersistableArrayShouldBeAssigned;
begin
	Assert.IsNotNull(FSettings.PersistableArray, 'PersistableArray should not be nil');
end;

procedure TRegexTemplateSettingsTest.DefaultTemplatesShouldContainThreeItems;
begin
	var
		arr := FSettings.PersistableArray.ArraySetting;
	Assert.AreEqual(3, arr.Count, 'Should contain exactly 3 default regex templates');
end;

procedure TRegexTemplateSettingsTest.CopyShouldDuplicateTemplates;
var
	otherSettings : TRegexTemplateSettings;
begin
	otherSettings := TRegexTemplateSettings.Create(FOwner);
	try
		otherSettings.Copy(FSettings);

		var
			arrSrc := FSettings.PersistableArray.ArraySetting;
		var
			arrDst := otherSettings.PersistableArray.ArraySetting;

		Assert.AreEqual(arrSrc.Count, arrDst.Count, 'Copied settings should have same template count');
		for var i := 0 to arrSrc.Count - 1 do begin
			Assert.AreEqual(arrSrc.Value[i], arrDst.Value[i],
				{ } Format('Template at index %d should match', [i]));
		end;
	finally
		otherSettings.Free;
	end;
end;

procedure TRegexTemplateSettingsTest.IniSectionNameShouldBeRegexTemplates;
begin
	Assert.AreEqual(TRegexTemplateSettings.INI_SECTION, FSettings.IniSectionName,
		{ } 'IniSectionName should be "RegexTemplates"');
end;

procedure TRegexTemplateSettingsTest.CopyWithNilShouldNotRaise;
begin
	// Copy with nil should silently do nothing
	Assert.WillNotRaise(
		procedure
		begin
			FSettings.Copy(nil);
		end, nil,
		'Copy with nil should not raise');
end;

initialization

TDUnitX.RegisterTestFixture(TRegexTemplateSettingsTest);

end.
