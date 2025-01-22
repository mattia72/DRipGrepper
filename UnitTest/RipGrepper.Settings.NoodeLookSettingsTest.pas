unit RipGrepper.Settings.NoodeLookSettingsTest;

interface

uses
	RipGrepper.Settings.SearchFormSettings,
	System.IniFiles,
	RipGrepper.Settings.NodeLookSettings,
	DUnitX.TestFramework,
	System.Classes,
	RipGrepper.Settings.TestOwnerSettings, RipGrepper.Helper.MemIniFile;

const
	NOTEXISTS = '<not exists>';
	SC_OPEN_WITH = 'SHIFT + F2';
	ACT_HIST_VAL = 'hist 2';

type

	[TestFixture]
	TNodeLookSettingsTest = class
		const
			INIFILE = 'DripGrepperUnittest.ini';

		private
			FIniFile : TMemIniFile;
			FOwner : TTestOwnerSettings;
			// FIniFile : TMemIniFile;
			FSettings : TNodeLookSettings;
			procedure SetTestDefaultAndActualValues;

		public
			constructor Create;
			destructor Destroy; override;
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
	RipGrepper.Tools.FileUtils;

constructor TNodeLookSettingsTest.Create;
begin
	inherited;
	// FIniFile := TMemIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'), TEncoding.UTF8);
end;

destructor TNodeLookSettingsTest.Destroy;
begin
	inherited;
	// FIniFile.Free;
end;

procedure TNodeLookSettingsTest.SetTestDefaultAndActualValues;
begin
	FSettings.AlternateRowColors := True;
	FSettings.ShowFileIcon := True;
	FSettings.IndentLines := True;
	FSettings.ShowRelativePath := True;
	FSettings.ExpandNodes := True;

	FSettings.StoreToDict;
end;

procedure TNodeLookSettingsTest.Setup;
begin
	FOwner := TTestOwnerSettings.Create();
	FIniFile := FOwner.IniFile;
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

	FSettings.StoreViewSettingToDict();
	var
		b : Boolean;
	for var s in VIEW_SETTINGS_TYPES do begin
		b := FSettings.SettingsDict['NodeLookSettings|' + s].Value;
		Assert.IsTrue(b, 'NodeLookSettings|' + s + ' should be true');
	end;
	FSettings.UpdateIniFile(FSettings.IniSectionName); // create temp ini
	FSettings.IniFile.ReadTempSectionFiles();

	iniVal := FSettings.IniFile.ReadString(FSettings.IniSectionName, 'AlternateRowColors', 'False');
	settingVal := FSettings.AlternateRowColors;
	Assert.AreEqual(settingVal, iniVal = '1', 'AlternateRowColors should be equal');

	iniVal := FSettings.IniFile.ReadString(FSettings.IniSectionName, 'IndentLines', 'False');
	settingVal := FSettings.IndentLines;
	Assert.AreEqual(settingVal, iniVal = '1', 'IndentLines should be equal');

	iniVal := FSettings.IniFile.ReadString(FSettings.IniSectionName, 'ShowRelativePath', 'False');
	settingVal := FSettings.ShowRelativePath;
	Assert.AreEqual(settingVal, iniVal = '1', 'ShowRelativePath should be equal');

	iniVal := FSettings.IniFile.ReadString(FSettings.IniSectionName, 'ExpandNodes', 'False');
	settingVal := FSettings.ExpandNodes;
	Assert.AreEqual(settingVal, iniVal = '1', 'ExpandNodes should be equal');
end;

procedure TNodeLookSettingsTest.UpdateIniTest;
begin
	SetTestDefaultAndActualValues;
	FSettings.UpdateIniFile();
	FSettings.ReadIni;
	for var s in VIEW_SETTINGS_TYPES do begin
		var
		b := FSettings.SettingsDict['NodeLookSettings|' + s].Value;
		Assert.IsTrue(b, 'NodeLookSettings|' + s + ' should be true');
	end;
end;

procedure TNodeLookSettingsTest.SetViewSettingsToDictTest;
begin
	SetTestDefaultAndActualValues;
	for var s in VIEW_SETTINGS_TYPES do begin
		FSettings.StoreViewSettingToDict(s);
		var
		b := FSettings.SettingsDict['NodeLookSettings|' + s].Value;
		Assert.IsTrue(b, 'NodeLookSettings|' + s + ' should be true');
	end;
end;

initialization

TDUnitX.RegisterTestFixture(TNodeLookSettingsTest);

end.
