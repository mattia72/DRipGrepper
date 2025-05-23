unit RipGrepper.Settings.NoodeLookSettingsTest;

interface

uses
	RipGrepper.Settings.SearchFormSettings,
	System.IniFiles,
	RipGrepper.Settings.NodeLookSettings,
	DUnitX.TestFramework,
	System.Classes,
	RipGrepper.Settings.TestOwnerSettings,
	RipGrepper.Helper.MemIniFile,
	Spring;

const
	NOTEXISTS = '<not exists>';
	SC_OPEN_WITH = 'SHIFT + F2';

type

	[TestFixture]
	TNodeLookSettingsTest = class
		const
			INIFILE = 'DripGrepperUnittest.ini';

		private
			FOwner : TTestOwnerSettings;
			FIniFile : IShared<TMemIniFile>;
			FSettings : TNodeLookSettings;
			procedure CheckNodeSettingsDict(const _id : string);
			procedure SetTestDefaultAndActualValues;

		public
			[Setup]
			procedure Setup;
			[TearDown]
			procedure TearDown;

			// [Test]
			procedure UpdateIniTest;
			[Test]
			procedure StoreViewSettingsTest;
			[Test]
			procedure SetViewSettingsToDictTest;
	end;

implementation

uses
	RipGrepper.Common.Constants,
	System.SysUtils,
	Vcl.Forms,
	RipGrepper.Common.GuiSearchParams,
	RipGrepper.Common.SimpleTypes,
	RipGrepper.Settings.ExtensionSettings,
	RipGrepper.Tools.FileUtils,
	RipGrepper.Settings.Persistable,
	RipGrepper.Settings.FilePersister,
	RipGrepper.Settings.SettingVariant, 
	RipGrepper.Settings.SettingsDictionary;

procedure TNodeLookSettingsTest.CheckNodeSettingsDict(const _id : string);
begin
	for var s in VIEW_SETTINGS_TYPES do begin
		var
			b : Boolean := FSettings.SettingsDict()['NodeLookSettings'][s].AsBool;
		Assert.IsTrue(b, _id + ' NodeLookSettings|' + s + ' should be true');
	end;
end;

procedure TNodeLookSettingsTest.SetTestDefaultAndActualValues;
begin
	FSettings.AlternateRowColors := True;
	FSettings.ShowFileIcon := True;
	FSettings.IndentLines := True;
	FSettings.ShowRelativePath := True;
	FSettings.ExpandNodes := True;
end;

procedure TNodeLookSettingsTest.Setup;
begin
	FIniFile := Shared.Make<TMemIniFile>(
		{ } TMemIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'), TEncoding.UTF8));

	FOwner := TTestOwnerSettings.Create();
	FOwner.PersisterFactory := TIniPersister.Create(FIniFile);

	FSettings := TNodeLookSettings.Create(FOwner);
end;

procedure TNodeLookSettingsTest.TearDown;
begin
	FSettings.Free; // instance will be free
	FOwner.Free;
	TFileUtils.EmptyFile(ChangeFileExt(Application.ExeName, '.ini'));
end;

procedure TNodeLookSettingsTest.StoreViewSettingsTest;
var
	iniVal : string;
	settingVal : Boolean;
begin
	SetTestDefaultAndActualValues;

	// TRipGrepperSettings.StoreViewSettings tested here

	CheckNodeSettingsDict('after storeview');
	{ 2 } FSettings.UpdateFile(True); // create temp ini
	CheckNodeSettingsDict('after updateini');

//  { 3 } FSettings.StoreDictToPersister();

	// Assert.IsTrue(not DirectoryExists(FSettings.IniFile.GetDripGrepperIniTempDir), ' temp dir should not exists');

	var
	fact := FSettings.PersisterFactory;

	fact.GetStringPersister(FSettings.IniSectionName, 'AlternateRowColors').TryLoadValue(iniVal);
	settingVal := FSettings.AlternateRowColors;
	Assert.AreEqual(settingVal, iniVal = '1', 'AlternateRowColors should be equal');

	fact.GetStringPersister(FSettings.IniSectionName, 'IndentLines').TryLoadValue(iniVal);
	settingVal := FSettings.IndentLines;
	Assert.AreEqual(settingVal, iniVal = '1', 'IndentLines should be equal');

	fact.GetStringPersister(FSettings.IniSectionName, 'ShowRelativePath').TryLoadValue(iniVal);
	settingVal := FSettings.ShowRelativePath;
	Assert.AreEqual(settingVal, iniVal = '1', 'ShowRelativePath should be equal');

	fact.GetStringPersister(FSettings.IniSectionName, 'ExpandNodes').TryLoadValue(iniVal);
	settingVal := FSettings.ExpandNodes;
	Assert.AreEqual(settingVal, iniVal = '1', 'ExpandNodes should be equal');

	CheckNodeSettingsDict('after updateini');

end;

procedure TNodeLookSettingsTest.UpdateIniTest;
begin
	SetTestDefaultAndActualValues;
	FSettings.UpdateFile();
	FSettings.ReadFile;
	for var s in VIEW_SETTINGS_TYPES do begin
		var
		b := FSettings.SettingsDict()['NodeLookSettings'][s].AsBool;
		Assert.IsTrue(b, 'NodeLookSettings|' + s + ' should be true');
	end;
end;

procedure TNodeLookSettingsTest.SetViewSettingsToDictTest;
// var
// arr : TArray<TArray<string>>;
begin
	SetTestDefaultAndActualValues;
	var sectionDict := FSettings.SettingsDict()['NodeLookSettings'];
    //arr := TSettingsDictionary.DictToStringArray(FSettings.SettingsDict());
	for var s in VIEW_SETTINGS_TYPES do begin
		var
		b := sectionDict[s].AsBool;
		Assert.IsTrue(b, 'NodeLookSettings|' + s + ' should be true');
	end;
end;

initialization

TDUnitX.RegisterTestFixture(TNodeLookSettingsTest);

end.
