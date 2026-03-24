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
			[Test]
			procedure TestDateFormatDefault;
			[Test]
			procedure TestDateFormatCustomValue;
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
			setting := FSettings.SettingsDict()['NodeLookSettings'][s];
		if setting.SettingType = stBool then begin
			Assert.IsTrue(setting.AsBool, _id + ' NodeLookSettings|' + s + ' should be true');
		end;
	end;
end;

procedure TNodeLookSettingsTest.SetTestDefaultAndActualValues;
begin
	FSettings.AlternateRowColors := True;
	FSettings.ShowFileIcon := True;
	FSettings.IndentLines := True;
	FSettings.ShowRelativePath := True;
	FSettings.ExpandNodes := True;
	FSettings.ShowLastModifiedDateColumn := True;
	FSettings.ShowCreationDateColumn := True;
	FSettings.ShowLastAccessDateColumn := True;
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

	fact.GetStringPersister(FSettings.IniSectionName, 'ShowLastModifiedDateColumn').TryLoadValue(iniVal);
	settingVal := FSettings.ShowLastModifiedDateColumn;
	Assert.AreEqual(settingVal, iniVal = '1', 'ShowLastModifiedDateColumn should be equal');

	CheckNodeSettingsDict('after updateini');

end;

procedure TNodeLookSettingsTest.UpdateIniTest;
begin
	SetTestDefaultAndActualValues;
	FSettings.UpdateFile();
	FSettings.ReadFile;
	for var s in VIEW_SETTINGS_TYPES do begin
		var
		setting := FSettings.SettingsDict()['NodeLookSettings'][s];
		if setting.SettingType = stBool then begin
			Assert.IsTrue(setting.AsBool, 'NodeLookSettings|' + s + ' should be true');
		end;
	end;
end;

procedure TNodeLookSettingsTest.SetViewSettingsToDictTest;
begin
	SetTestDefaultAndActualValues;
	var sectionDict := FSettings.SettingsDict()['NodeLookSettings'];
	for var s in VIEW_SETTINGS_TYPES do begin
		var
		setting := sectionDict[s];
		if setting.SettingType = stBool then begin
			Assert.IsTrue(setting.AsBool, 'NodeLookSettings|' + s + ' should be true');
		end;
	end;
end;

procedure TNodeLookSettingsTest.TestDateFormatDefault;
begin
	Assert.AreEqual('yyyy-mm-dd hh:nn:ss', FSettings.DateFormat);
end;

procedure TNodeLookSettingsTest.TestDateFormatCustomValue;
begin
	FSettings.DateFormat := 'dd.mm.yyyy';
	FSettings.UpdateFile(True);
	FSettings.ReadFile();
	Assert.AreEqual('dd.mm.yyyy', FSettings.DateFormat);
end;

initialization

TDUnitX.RegisterTestFixture(TNodeLookSettingsTest);

end.
