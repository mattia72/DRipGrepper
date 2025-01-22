unit RipGrepper.Settings.RipGrepperSettingsTest;

interface

uses
	RipGrepper.Settings.SearchFormSettings,
	System.IniFiles,
	RipGrepper.Settings.RipGrepperSettings,
	DUnitX.TestFramework,
	System.Classes;

const
	NOTEXISTS = '<not exists>';
	SC_OPEN_WITH = 'SHIFT + F2';
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
			procedure EmptyFile(const _filePath : string);
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
			procedure UpdateIniTest;
			[Test]
			procedure NodeLookSettingsTest;
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
	RipGrepper.Settings.NodeLookSettings;

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
		FHistItemGuiSearchParams.SearchText := 'search this';
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
	FSettings.RipGrepParameters.GuiSearchTextParams.SearchOptions := [EGuiOption.soUseRegex];
	FSettings.StoreAsDefaultsToDict;
	FSettings.SearchFormSettings.Encoding := 'none';
	FSettings.SearchFormSettings.Context := 1;
	FSettings.SearchFormSettings.Pretty := True;
	FSettings.SearchFormSettings.Hidden := True;
	FSettings.SearchFormSettings.NoIgnore := True;
	FSettings.RipGrepParameters.SearchPath := 'act\search\path';
	FSettings.RipGrepParameters.FileMasks := '*.act';
	FSettings.RipGrepParameters.GuiSearchTextParams.SearchOptions := [EGuiOption.soNotSet];

	FSettings.NodeLookSettings.AlternateRowColors := True;
	FSettings.NodeLookSettings.IndentLines := True;
	FSettings.NodeLookSettings.ShowRelativePath := True;
	FSettings.NodeLookSettings.ExpandNodes := True;

	FSettings.SearchFormSettings.ExtensionSettings.CurrentIDEContext :=
	{ } TRipGrepperExtensionContext.FromString('2', 'active project', 'active file');

	for var s in ['hist 0', 'hist 1', ACT_HIST_VAL] do
		FTextHist.Add(s);

	FSettings.SearchFormSettings.ExtensionSettings.OpenWithShortCut := SC_OPEN_WITH;
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
	EmptyFile(ChangeFileExt(Application.ExeName, '.ini'));
end;

procedure TRipGrepperSettingsTest.EmptyFile(const _filePath : string);
var
	txtFile : TextFile;
begin
	AssignFile(txtFile, _filePath);
	Rewrite(txtFile);
	CloseFile(txtFile);
end;

procedure TRipGrepperSettingsTest.NodeLookSettingsTest;
var
	iniVal : string;
	settingVal : Boolean;
begin
	SetTestDefaultAndActualValues;
	FSettings.NodeLookSettings.StoreViewSettingToDict();
 	var b : Boolean;
	for var s in TNodeLookSettings.VIEW_SETTINGS do begin
		b := FSettings.NodeLookSettings.SettingsDict['NodeLookSettings|' + s].Value;
		Assert.IsTrue(b, 'NodeLookSettings|' + s + ' should be true');
	end;

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

procedure TRipGrepperSettingsTest.UpdateIniTest;
begin
	SetTestDefaultAndActualValues;
	FSettings.UpdateIniFile();
	FSettings.ReadIni;
	Assert.AreEqual('none', FSettings.IniFile.ReadString(FSettings.SearchFormSettings.IniSectionName, 'Encoding', NOTEXISTS));
	Assert.AreEqual(1, FSettings.IniFile.ReadInteger(FSettings.SearchFormSettings.IniSectionName, 'Context', -99999));
	Assert.AreEqual(True, FSettings.IniFile.ReadBool(FSettings.SearchFormSettings.IniSectionName, 'Pretty', FALSE));
	Assert.AreEqual(True, FSettings.IniFile.ReadBool(FSettings.SearchFormSettings.IniSectionName, 'Hidden', FALSE));
	Assert.AreEqual(True, FSettings.IniFile.ReadBool(FSettings.SearchFormSettings.IniSectionName, 'NoIgnore', FALSE));
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
	SetTestDefaultAndActualValues;
	FSettings.SearchFormSettings.ExtensionSettings.UpdateIniFile(
		{ } FSettings.SearchFormSettings.ExtensionSettings.INI_SECTION);

	FSettings.UpdateIniFile();

	Assert.AreEqual(SC_OPEN_WITH,
		{ } FSettings.IniFile.ReadString(FSettings.SearchFormSettings.ExtensionSettings.INI_SECTION,
		{ } FSettings.SearchFormSettings.ExtensionSettings.KEY_SHORTCUT_OPENWITH, 'not exists'));

	FSettings.ReadIni;
	Assert.AreEqual(SC_OPEN_WITH, FSettings.SearchFormSettings.ExtensionSettings.OpenWithShortCut, 'OpenWithShortCut should be ok');
end;

initialization

TDUnitX.RegisterTestFixture(TRipGrepperSettingsTest);

end.
