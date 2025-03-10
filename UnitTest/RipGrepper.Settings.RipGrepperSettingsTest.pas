unit RipGrepper.Settings.RipGrepperSettingsTest;

interface

uses
	RipGrepper.Settings.SearchFormSettings,
	System.IniFiles,
	RipGrepper.Settings.RipGrepperSettings,
	DUnitX.TestFramework,
	System.Classes,
	RipGrepper.Helper.MemIniFile;

const
	NOTEXISTS = '<not exists>';
	SC_OPEN_WITH = 'SHIFT + F2';
	SC_SEARCH = 'CTRL + F2';
	ACT_HIST_VAL = 'hist 2';

type

	[TestFixture]
	TRipGrepperSettingsTest = class
		const
			INIFILE = 'DripGrepperUnittest.ini';

		private
			// FIniFile : TMemIniFile;
			FSettings : TRipGrepperSettings;
			FTextHist : TStringList;
			function ReadBoolIniAsString(const _section, _key : string) : string;
			procedure SetTestDefaultAndActualValues;

		public
			constructor Create;
			destructor Destroy; override;
			[Setup]
			procedure Setup;
			[TearDown]
			procedure TearDown;

			[Test]
			procedure RefreshMembersShouldLoadDefaultsTest;
			[Test]
			procedure LoadDefaultsShouldReadDefaultFromIni;
			[Test]
			procedure AfterCopyValuesValuesShouldBeEqual;
			[Test]
			procedure ActionSetAsDefaultExecuteShouldSaveInIni;
			[Test]
			procedure AfterUpdateIniValuesShouldBeProperlySaved;
			[Test]
			procedure AfterUpdateIniDefaultsShouldBeProperlySaved;
			[Test]
			procedure UpdateIniFileTest();
			[Test]
			procedure NodeLookSettingsTest;
			[Test]
			procedure ExtensionSettingsTest;
			[Test]
			procedure UpdateHistInIniTest;
			[Test]
			procedure UpdateShortCutsIniTest;
	end;

implementation

uses
	RipGrepper.Common.Constants,
	System.SysUtils,
	Vcl.Forms,
	RipGrepper.Common.GuiSearchParams,
	RipGrepper.Common.SimpleTypes,
	RipGrepper.Settings.ExtensionSettings,
	RipGrepper.Settings.NodeLookSettings,
	RipGrepper.Tools.FileUtils,
	System.StrUtils;

constructor TRipGrepperSettingsTest.Create;
begin
	inherited;
	// FIniFile := TMemIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'), TEncoding.UTF8);
end;

destructor TRipGrepperSettingsTest.Destroy;
begin
	inherited;
	// FIniFile.Free;
end;

procedure TRipGrepperSettingsTest.RefreshMembersShouldLoadDefaultsTest;
begin
	FSettings.LoadDefaultsFromDict;
	Assert.AreEqual(True, FSettings.SearchFormSettings.Pretty, 'Pretty should be true');
	Assert.AreEqual(False, FSettings.SearchFormSettings.Hidden, 'Hidden should be true');
	Assert.AreEqual(False, FSettings.SearchFormSettings.NoIgnore, 'NoIgnore should be true');
	Assert.AreEqual(0, FSettings.SearchFormSettings.Context, 'Context should be true');
	Assert.AreEqual('', FSettings.SearchFormSettings.Encoding, 'Encoding should be ''''');
end;

procedure TRipGrepperSettingsTest.LoadDefaultsShouldReadDefaultFromIni;
begin
	SetTestDefaultAndActualValues;
	FSettings.LoadDefaultsFromDict;
	// Assert.IsTrue(FSettings.IsAlreadyRead, 'IsAlreadyRead should read');
	Assert.AreEqual('utf8', FSettings.SearchFormSettings.Encoding, 'Encoding should be utf8');
	Assert.AreEqual(5, FSettings.SearchFormSettings.Context, 'Context should be 5');
	Assert.AreEqual(False, FSettings.SearchFormSettings.Pretty, 'Pretty should be false');
	Assert.AreEqual(True, FSettings.SearchFormSettings.Hidden, 'Hidden should be true');
	Assert.AreEqual(True, FSettings.SearchFormSettings.NoIgnore, 'NoIgnore should be true');
end;

procedure TRipGrepperSettingsTest.AfterCopyValuesValuesShouldBeEqual;
begin
	SetTestDefaultAndActualValues;
	FSettings.LoadDefaultsFromDict;
	var
	s := TRipGrepperSettings.Create();
	try
		s.Copy(FSettings);

		Assert.AreEqual(s.SearchFormSettings.Encoding,
			{ } FSettings.SearchFormSettings.Encoding, 'Encoding should be utf8');
		Assert.AreEqual(s.SearchFormSettings.Context,
			{ } FSettings.SearchFormSettings.Context, 'Context should be 5');
		Assert.AreEqual(s.SearchFormSettings.Pretty,
			{ } FSettings.SearchFormSettings.Pretty, 'Pretty should be false');
		Assert.AreEqual(s.SearchFormSettings.Hidden,
			{ } FSettings.SearchFormSettings.Hidden, 'Hidden should be true');
		Assert.AreEqual(s.SearchFormSettings.NoIgnore,
			{ } FSettings.SearchFormSettings.NoIgnore, 'NoIgnore should be true');

		Assert.AreEqual(s.RipGrepParameters.SearchPath,
			{ } FSettings.RipGrepParameters.SearchPath, 'SearchPath should be equal');
		Assert.AreEqual(s.RipGrepParameters.FileMasks,
			{ } FSettings.RipGrepParameters.FileMasks, 'FileMasks should be equal');

		Assert.AreEqual(
			{ } s.RipGrepParameters.GuiSearchTextParams.GetAsString(True),
			{ } FSettings.RipGrepParameters.GuiSearchTextParams.GetAsString(True),
			{ } 'SearchParameter should be equal');

	finally
		s.Free;
	end;
end;

procedure TRipGrepperSettingsTest.ActionSetAsDefaultExecuteShouldSaveInIni;
var
	s1, s2 : string;
	FHistItemGuiSearchParams : TGuiSearchTextParams;
begin
	SetTestDefaultAndActualValues;
	FSettings.LoadDefaultsFromDict;

	FHistItemGuiSearchParams :=
	{ } TGuiSearchTextParams.Create(FSettings.RipGrepParameters.IniSectionName);
	try
		FHistItemGuiSearchParams.SetSearchText('search this');
		FHistItemGuiSearchParams.SetOption(EGuiOption.soMatchWord);
		FHistItemGuiSearchParams.SetOption(EGuiOption.soMatchCase);

		// -- Act as TRipGrepperSearchDialogForm.ActionSetAsDefaultExecute
		FSettings.RipGrepParameters.GuiSearchTextParams.Copy(FHistItemGuiSearchParams);

		s1 := FHistItemGuiSearchParams.GetAsString();
		s2 := FSettings.RipGrepParameters.GuiSearchTextParams.GetAsString();
		Assert.AreEqual(s1, s2, '1. -ok GuiSearchTextParams should be equal');

		// -- Act as TRipGrepperSearchDialogForm.ActionSetAsDefaultExecute
		FSettings.StoreAsDefaultsToDict();

		s1 := FHistItemGuiSearchParams.GetAsString(True);
		s2 := FSettings.RipGrepParameters.SettingsDict['RipGrepSettings|SearchParams_DEFAULT'].Value;
		Assert.AreEqual(s1, s2, '2. - ok GuiSearchTextParams should be equal');

		// -- Act as TRipGrepperSearchDialogForm.ActionSetAsDefaultExecute
		FSettings.UpdateIniFile;

		s1 := FHistItemGuiSearchParams.GetAsString(True);
		s2 := FSettings.IniFile.ReadString(FSettings.RipGrepParameters.IniSectionName, 'SearchParams_DEFAULT', '');
		Assert.AreEqual(s1, s2, 'last. GuiSearchTextParams should be equal');

	finally
		FHistItemGuiSearchParams.Free;
	end;
end;

procedure TRipGrepperSettingsTest.AfterUpdateIniValuesShouldBeProperlySaved;
var
	iniVal, settingVal : string;
begin
	SetTestDefaultAndActualValues;

	FSettings.UpdateIniFile;
	FSettings.LoadFromDict;

	iniVal := FSettings.IniFile.ReadString(FSettings.RipGrepParameters.IniSectionName, 'SearchParams', '');
	settingVal := FSettings.RipGrepParameters.GuiSearchTextParams.GetAsString(True);
	Assert.AreEqual(settingVal.Trim(['[', ']']), iniVal.Trim(['[', ']']), 'SearchParams should be equal');

	iniVal := FSettings.IniFile.ReadString(FSettings.SearchFormSettings.IniSectionName, 'Encoding', 'none');
	settingVal := FSettings.SearchFormSettings.Encoding;
	Assert.AreEqual(settingVal, iniVal, 'Encoding should be equal');

	iniVal := FSettings.IniFile.ReadString(FSettings.SearchFormSettings.IniSectionName, 'Context', '');
	settingVal := FSettings.SearchFormSettings.Context.ToString;
	Assert.AreEqual(settingVal, iniVal, 'Context should be equal');

	iniVal := ReadBoolIniAsString(FSettings.SearchFormSettings.IniSectionName, 'Pretty');
	settingVal := BoolToStr(FSettings.SearchFormSettings.Pretty, True);
	Assert.AreEqual(settingVal, iniVal, 'Pretty should be equal');

	iniVal := ReadBoolIniAsString(FSettings.SearchFormSettings.IniSectionName, 'Hidden');
	settingVal := BoolToStr(FSettings.SearchFormSettings.Hidden, True);
	Assert.AreEqual(settingVal, iniVal, 'Hidden should be equal');

	iniVal := ReadBoolIniAsString(FSettings.SearchFormSettings.IniSectionName, 'NoIgnore');
	settingVal := BoolToStr(FSettings.SearchFormSettings.NoIgnore, True);
	Assert.AreEqual(settingVal, iniVal, 'NoIgnore should be equal');

	// iniVal := FSettings.IniFile.ReadString(FSettings.SearchFormSettings.ExtensionSettings.INI_SECTION, 'CurrentIDEContext', '');
	// settingVal := Integer(FSettings.SearchFormSettings.ExtensionSettings.CurrentIDEContext).ToString;
	// Assert.AreEqual(settingVal, iniVal, 'CurrentIDEContext should be equal');

end;

procedure TRipGrepperSettingsTest.AfterUpdateIniDefaultsShouldBeProperlySaved;
var
	iniVal, settingVal : string;
begin
	SetTestDefaultAndActualValues;

	FSettings.UpdateIniFile;
	FSettings.LoadDefaultsFromDict;

	iniVal := FSettings.IniFile.ReadString(FSettings.RipGrepParameters.IniSectionName, 'SearchParams_DEFAULT', '');
	settingVal := FSettings.RipGrepParameters.GuiSearchTextParams.GetAsString(True);
	Assert.AreEqual(settingVal.Trim(['[', ']']), iniVal.Trim(['[', ']']), 'SearchParams should be equal');

	iniVal := FSettings.IniFile.ReadString(FSettings.SearchFormSettings.IniSectionName, 'Encoding_DEFAULT', 'none');
	settingVal := FSettings.SearchFormSettings.Encoding;
	Assert.AreEqual(settingVal, iniVal, 'Encoding should be equal');

	iniVal := FSettings.IniFile.ReadString(FSettings.SearchFormSettings.IniSectionName, 'Context_DEFAULT', '');
	settingVal := FSettings.SearchFormSettings.Context.ToString;
	Assert.AreEqual(settingVal, iniVal, 'Context should be equal');

	iniVal := ReadBoolIniAsString(FSettings.SearchFormSettings.IniSectionName, 'Pretty_DEFAULT');
	settingVal := BoolToStr(FSettings.SearchFormSettings.Pretty, True);
	Assert.AreEqual(settingVal, iniVal, 'Pretty should be equal');

	iniVal := ReadBoolIniAsString(FSettings.SearchFormSettings.IniSectionName, 'Hidden_DEFAULT');
	settingVal := BoolToStr(FSettings.SearchFormSettings.Hidden, True);
	Assert.AreEqual(settingVal, iniVal, 'Hidden should be equal');

	iniVal := ReadBoolIniAsString(FSettings.SearchFormSettings.IniSectionName, 'NoIgnore_DEFAULT');
	settingVal := BoolToStr(FSettings.SearchFormSettings.NoIgnore, True);
	Assert.AreEqual(settingVal, iniVal, 'NoIgnore should be equal');
end;

procedure TRipGrepperSettingsTest.SetTestDefaultAndActualValues;
begin
	FSettings.SearchFormSettings.Encoding := 'utf8';
	FSettings.SearchFormSettings.Context := 5;
	FSettings.SearchFormSettings.Pretty := False;
	FSettings.SearchFormSettings.Hidden := True;
	FSettings.SearchFormSettings.NoIgnore := True;
	FSettings.RipGrepParameters.SearchPath := 'def\search\path';
	FSettings.RipGrepParameters.FileMasks := '*.def';
	FSettings.RipGrepParameters.GuiSearchTextParams.SetSearchOptions([EGuiOption.soUseRegex]);
	FSettings.StoreAsDefaultsToDict;
	FSettings.SearchFormSettings.Encoding := 'none';
	FSettings.SearchFormSettings.Context := 1;
	FSettings.SearchFormSettings.Pretty := True;
	FSettings.SearchFormSettings.Hidden := True;
	FSettings.SearchFormSettings.NoIgnore := True;
	FSettings.RipGrepParameters.SearchPath := 'act\search\path';
	FSettings.RipGrepParameters.FileMasks := '*.act';
	FSettings.RipGrepParameters.GuiSearchTextParams.SetSearchOptions([EGuiOption.soNotSet]);

	FSettings.NodeLookSettings.AlternateRowColors := True;
	FSettings.NodeLookSettings.ShowFileIcon := True;
	FSettings.NodeLookSettings.IndentLines := True;
	FSettings.NodeLookSettings.ShowRelativePath := True;
	FSettings.NodeLookSettings.ExpandNodes := True;

	FSettings.SearchFormSettings.ExtensionSettings.OpenWithShortcut := SC_OPEN_WITH;
	FSettings.SearchFormSettings.ExtensionSettings.SearchSelectedShortcut := SC_SEARCH;
	FSettings.SearchFormSettings.ExtensionSettings.CurrentIDEContext :=
	{ } TRipGrepperExtensionContext.FromString('2', 'active project', 'active file');

	for var s in ['hist 0', 'hist 1', ACT_HIST_VAL] do
		FTextHist.Add(s);

	FSettings.SearchTextsHistory := FTextHist;
	FSettings.StoreToDict;
end;

procedure TRipGrepperSettingsTest.Setup;
begin
	FSettings := TRipGrepperSettings.Create();
	FTextHist := TStringList.Create();
end;

procedure TRipGrepperSettingsTest.TearDown;
begin
	FSettings.Free; // instance will be free
	FTextHist.Free;
	TFileUtils.EmptyFile(ChangeFileExt(Application.ExeName, '.ini'));
end;

procedure TRipGrepperSettingsTest.NodeLookSettingsTest;
var
	iniVal : string;
	settingVal : Boolean;
begin
	SetTestDefaultAndActualValues;

	// TRipGrepperSettings.StoreViewSettings tested here
	{ 1 } FSettings.NodeLookSettings.StoreViewSettingToDict();
	var
		b : Boolean;
	for var s in VIEW_SETTINGS_TYPES do begin
		b := FSettings.NodeLookSettings.SettingsDict['NodeLookSettings|' + s].Value;
		Assert.IsTrue(b, 'NodeLookSettings|' + s + ' should be true');
	end;

	{ 2 } FSettings.NodeLookSettings.UpdateIniFile(FSettings.NodeLookSettings.IniSectionName); // create temp ini

	// Assert.IsTrue(FileExists(FSettings.IniFile.GetTempSectionFileName('NodeLookSettings')), 'temp ini should exist.');
	{ 3 } FSettings.StoreToDict;

	Assert.IsTrue(not DirectoryExists(FSettings.IniFile.GetDripGrepperIniTempDir), ' temp dir should not exists');

	iniVal := FSettings.IniFile.ReadString(FSettings.NodeLookSettings.IniSectionName, 'AlternateRowColors', 'False');
	settingVal := FSettings.NodeLookSettings.AlternateRowColors;
	Assert.AreEqual(settingVal, iniVal = '1', 'AlternateRowColors should be equal');

	iniVal := FSettings.IniFile.ReadString(FSettings.NodeLookSettings.IniSectionName, 'IndentLines', 'False');
	settingVal := FSettings.NodeLookSettings.IndentLines;
	Assert.AreEqual(settingVal, iniVal = '1', 'IndentLines should be equal');

	iniVal := FSettings.IniFile.ReadString(FSettings.NodeLookSettings.IniSectionName, 'ShowRelativePath', 'False');
	settingVal := FSettings.NodeLookSettings.ShowRelativePath;
	Assert.AreEqual(settingVal, iniVal = '1', 'ShowRelativePath should be equal');

	iniVal := FSettings.IniFile.ReadString(FSettings.NodeLookSettings.IniSectionName, 'ExpandNodes', 'False');
	settingVal := FSettings.NodeLookSettings.ExpandNodes;
	Assert.AreEqual(settingVal, iniVal = '1', 'ExpandNodes should be equal');
end;

procedure TRipGrepperSettingsTest.ExtensionSettingsTest;
var
	extSetting : TRipGrepperExtensionSettings;
	iniVal : string;
	settingVal : string;
begin
	SetTestDefaultAndActualValues;
	extSetting := FSettings.SearchFormSettings.ExtensionSettings;

	{ 1 } // FSettings.SearchFormSettings.ExtensionSettings;
	for var s in [extSetting.KEY_IDE_CONTEXT, extSetting.KEY_SHORTCUT_SEARCH_SELECTED, extSetting.KEY_SHORTCUT_OPENWITH] do begin
		settingVal := extSetting.SettingsDict['DelphiExtensionSettings|' + s].Value;
		Assert.IsTrue(MatchStr(settingVal, [SC_OPEN_WITH, SC_SEARCH, '2']), 'DelphiExtensionSettings|' + s +
			' should match the initial setting');
	end;

	// TRipGrepperSearchDialogForm.FormClose tested here
	FSettings.SearchFormSettings.UpdateIniFile(FSettings.SearchFormSettings.IniSectionName); // create temp section
	// Fsettings.IniFile.ReadTempSectionFiles(); // read temp section
	Fsettings.IniFile.UpdateFile;

	iniVal := FSettings.IniFile.ReadString(extSetting.INI_SECTION, extSetting.KEY_IDE_CONTEXT, '');
	settingVal := IntToStr(Integer(FSettings.SearchFormSettings.ExtensionSettings.CurrentIDEContext.IDEContext));
	Assert.AreEqual(settingVal.Trim(['[', ']']), iniVal.Trim(['[', ']']), extSetting.KEY_IDE_CONTEXT + ' should be equal');

	iniVal := FSettings.IniFile.ReadString(extSetting.INI_SECTION, extSetting.KEY_SHORTCUT_SEARCH_SELECTED, '');
	settingVal := FSettings.SearchFormSettings.ExtensionSettings.SearchSelectedShortcut;
	Assert.AreEqual(settingVal.Trim(['[', ']']), iniVal.Trim(['[', ']']), extSetting.KEY_SHORTCUT_SEARCH_SELECTED + ' should be equal');

	iniVal := FSettings.IniFile.ReadString(extSetting.INI_SECTION, extSetting.KEY_SHORTCUT_OPENWITH, '');
	settingVal := FSettings.SearchFormSettings.ExtensionSettings.OpenWithShortcut;
	Assert.AreEqual(settingVal.Trim(['[', ']']), iniVal.Trim(['[', ']']), extSetting.KEY_SHORTCUT_OPENWITH + ' should be equal');
end;

procedure TRipGrepperSettingsTest.UpdateIniFileTest();
begin
	SetTestDefaultAndActualValues;
	FSettings.UpdateIniFile(); // config form close is tested here?
	FSettings.ReadIni;
	var
	iniSection := FSettings.SearchFormSettings.IniSectionName;
	Assert.AreEqual('none', FSettings.IniFile.ReadString(iniSection, 'Encoding', NOTEXISTS));
	Assert.AreEqual(1, FSettings.IniFile.ReadInteger(iniSection, 'Context', -99999));
	Assert.AreEqual(True, FSettings.IniFile.ReadBool(iniSection, 'Pretty', FALSE));
	Assert.AreEqual(True, FSettings.IniFile.ReadBool(iniSection, 'Hidden', FALSE));
	Assert.AreEqual(True, FSettings.IniFile.ReadBool(iniSection, 'NoIgnore', FALSE));

	var
	extSection := FSettings.SearchFormSettings.ExtensionSettings.IniSectionName;
	var
	scIniVal := FSettings.IniFile.ReadString(extSection, 'OpenWithShortcut', NOTEXISTS);
	Assert.AreEqual(SC_OPEN_WITH, scIniVal);
	scIniVal := FSettings.IniFile.ReadString(extSection, 'SearchSelectedShortcut', NOTEXISTS);
	Assert.AreEqual(SC_SEARCH, scIniVal);

end;

function TRipGrepperSettingsTest.ReadBoolIniAsString(const _section, _key : string) : string;
begin
	Result := BoolToStr(FSettings.IniFile.ReadBool(_section, _key, False), True);
end;

procedure TRipGrepperSettingsTest.UpdateHistInIniTest;
begin
	SetTestDefaultAndActualValues;
	// see SearchForm.OnClose.
	FSettings.StoreHistories();
	FSettings.UpdateIniFile();
	FSettings.ReadIni;

	Assert.AreEqual(ACT_HIST_VAL, FSettings.SearchTextsHistory[0], 'SearchTextsHistory[0] should be hist 0');
	Assert.AreEqual(ACT_HIST_VAL,
		{ } FSettings.IniFile.ReadString('SearchTextsHistory', 'Item_0', NOTEXISTS));
end;

procedure TRipGrepperSettingsTest.UpdateShortCutsIniTest;
begin
	SetTestDefaultAndActualValues; // after config form close is tested here
	FSettings.SearchFormSettings.ExtensionSettings.UpdateIniFile(
		{ } FSettings.SearchFormSettings.ExtensionSettings.INI_SECTION);

	FSettings.UpdateIniFile();

	Assert.AreEqual(SC_OPEN_WITH,
		{ } FSettings.IniFile.ReadString(FSettings.SearchFormSettings.ExtensionSettings.INI_SECTION,
		{ } FSettings.SearchFormSettings.ExtensionSettings.KEY_SHORTCUT_OPENWITH, 'not exists'));

	FSettings.ReadIni;
	Assert.AreEqual(SC_OPEN_WITH, FSettings.SearchFormSettings.ExtensionSettings.OpenWithShortcut, 'OpenWithShortcut should be ok');
end;

initialization

TDUnitX.RegisterTestFixture(TRipGrepperSettingsTest);

end.
