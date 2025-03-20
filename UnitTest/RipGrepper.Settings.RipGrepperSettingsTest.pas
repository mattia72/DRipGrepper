unit RipGrepper.Settings.RipGrepperSettingsTest;

interface

uses
	RipGrepper.Settings.SearchFormSettings,
	System.IniFiles,
	RipGrepper.Settings.RipGrepperSettings,
	DUnitX.TestFramework,
	System.Classes,
	RipGrepper.Helper.MemIniFile,
	RipGrepper.Settings.FilePersister,
	ArrayEx,
	Spring;

const
	NOTEXISTS = '<not exists>';
	SC_OPEN_WITH = 'SHIFT + F2';
	SC_SEARCH = 'CTRL + F2';
	ACT_HIST_VAL = 'hist 2';

type

	TRipGrepperSettingsTestBase = class
		const
			INIFILE = 'DripGrepperUnittest.ini';

		private
			procedure SetSettingValues();

		protected
			FboolPers : IFilePersister<Boolean>;
			FintPers : IFilePersister<integer>;
			FstrPers : IFilePersister<string>;

			FFactory : IPersisterFactory;
			FSettings : IShared<TRipGrepperSettings>;
			FTextHist : TArrayEx<string>;

		public
			[Setup]
			procedure Setup();
			[TearDown]
			procedure TearDown();
	end;

	[TestFixture]
	TInnerSearchFormSettingsTest = class(TRipGrepperSettingsTestBase)
		public
			[Test]
			procedure EncodingChangeAndUpdateFile();
			[Test]
			procedure EncodingChangeChangesDictionary();
			[Test]
			procedure ContextChangeChangesDictionary();
			[Test]
			procedure UpdateShortCutsIniTest();
	end;

	[TestFixture]
	TRipGrepperSettingsTest = class(TRipGrepperSettingsTestBase)
		private

			function ReadBoolIniAsString(const _section, _key : string) : string;

		public
			[Test]
			procedure InitialSettingsShouldLoadSearchFormSettingsDefaults;
			[Test]
			procedure AfterCopyValuesValuesShouldBeEqual;
			[Test]
			procedure AfterUpdateIniValuesShouldBeProperlySaved;
			[Test]
			procedure UpdateFileTest();
			[Test]
			procedure NodeLookSettingsTest;
			[Test]
			procedure ExtensionSettingsTest;
			[Test]
			procedure InitialSettingsShouldLoadDefaultNoodeLooks;
			[Test]
			procedure InitialSettingsShouldLoadDefaultFontColors();
			[Test]
			procedure UpdateFileAfterConfigTest;
			[Test]
			procedure UpdateHistInIniTest;
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
	System.StrUtils,
	RipGrepper.Settings.SettingsDictionary,
	RipGrepper.Settings.FontColors,
	RipGrepper.Helper.UI.DarkMode;

procedure TRipGrepperSettingsTest.InitialSettingsShouldLoadSearchFormSettingsDefaults;
var
	s : IShared<TRipGrepperSettings>;
	sfs : TSearchFormSettings;
begin
	s := Shared.Make<TRipGrepperSettings>(TRipGrepperSettings.Create());
	sfs := s().SearchFormSettings;
	// sfs.Init;
	Assert.AreEqual(False, sfs.Pretty, 'Pretty should be False');
	Assert.AreEqual(False, sfs.Hidden, 'Hidden should be False');
	Assert.AreEqual(False, sfs.NoIgnore, 'NoIgnore should be False');
	Assert.AreEqual(0, sfs.Context, 'Context should be 0');
	Assert.AreEqual('', sfs.Encoding, 'Encoding should be ''''');
end;

procedure TRipGrepperSettingsTest.AfterCopyValuesValuesShouldBeEqual;
begin
	SetSettingValues;
	FSettings.LoadFromDict; // FSettings.LoadDefaultsFromDict;
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

procedure TRipGrepperSettingsTest.AfterUpdateIniValuesShouldBeProperlySaved;
var
	iniVal, settingVal : string;
begin
	SetSettingValues;

	var
	dbgArr := TSettingsDictionary.DictToStringArray(FSettings.SettingsDict());

	FSettings.UpdateFile;
	// FSettings.LoadFromDict;

	FFactory.GetStringPersister(FSettings.RipGrepParameters.IniSectionName, 'SearchParams').TryLoadValue(iniVal);
	settingVal := FSettings.RipGrepParameters.GuiSearchTextParams.GetAsString(True);
	Assert.AreEqual(settingVal.Trim(['[', ']']), iniVal.Trim(['[', ']']), 'SearchParams should be equal');

	FFactory.GetStringPersister(FSettings.SearchFormSettings.IniSectionName, 'Encoding').TryLoadValue(iniVal);
	settingVal := FSettings.SearchFormSettings.Encoding;
	Assert.AreEqual(settingVal, iniVal, 'Encoding should be equal');

	FFactory.GetStringPersister(FSettings.SearchFormSettings.IniSectionName, 'Context').TryLoadValue(iniVal);
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

procedure TRipGrepperSettingsTest.NodeLookSettingsTest;
var
	iniVal : string;
	settingVal : Boolean;
begin
	SetSettingValues;

	// TRipGrepperSettings.StoreViewSettings tested here
	var
		b : Boolean;
	for var s in VIEW_SETTINGS_TYPES do begin
		b := FSettings.NodeLookSettings.SettingsDict.InnerDictionary['NodeLookSettings'][s].AsBoolSetting.Value;
		Assert.IsTrue(b, 'NodeLookSettings|' + s + ' should be true');
	end;

	{ 2 } FSettings.NodeLookSettings.UpdateFile();

	// Assert.IsTrue(FileExists(FSettings.IniFile.GetTempSectionFileName('NodeLookSettings')), 'temp ini should exist.');
	{ 3 } FSettings.StoreToPersister;

	FFactory.GetStringPersister(FSettings.NodeLookSettings.IniSectionName, 'AlternateRowColors').TryLoadValue(iniVal);
	settingVal := FSettings.NodeLookSettings.AlternateRowColors;
	Assert.AreEqual(settingVal, iniVal = '1', 'AlternateRowColors should be equal');

	FFactory.GetStringPersister(FSettings.NodeLookSettings.IniSectionName, 'IndentLines').TryLoadValue(iniVal);
	settingVal := FSettings.NodeLookSettings.IndentLines;
	Assert.AreEqual(settingVal, iniVal = '1', 'IndentLines should be equal');

	FFactory.GetStringPersister(FSettings.NodeLookSettings.IniSectionName, 'ShowRelativePath').TryLoadValue(iniVal);
	settingVal := FSettings.NodeLookSettings.ShowRelativePath;
	Assert.AreEqual(settingVal, iniVal = '1', 'ShowRelativePath should be equal');

	FFactory.GetStringPersister(FSettings.NodeLookSettings.IniSectionName, 'ExpandNodes').TryLoadValue(iniVal);
	settingVal := FSettings.NodeLookSettings.ExpandNodes;
	Assert.AreEqual(settingVal, iniVal = '1', 'ExpandNodes should be equal');
end;

procedure TRipGrepperSettingsTest.ExtensionSettingsTest;
var
	extSetting : TRipGrepperExtensionSettings;
	fh : IFileHandler;
	iniVal : string;
	settingVal : string;
begin
	SetSettingValues;
	extSetting := FSettings.SearchFormSettings.ExtensionSettings;

	{ 1 } // FSettings.SearchFormSettings.ExtensionSettings;
	for var s in [extSetting.KEY_IDE_CONTEXT, extSetting.KEY_SHORTCUT_SEARCH_SELECTED, extSetting.KEY_SHORTCUT_OPENWITH] do begin
		var
		d := extSetting.SettingsDict()['DelphiExtensionSettings'];
		var
		dbgArr := TSettingsDictionary.DictToStringArray(extSetting.SettingsDict());
		settingVal := d[s].AsString;
		Assert.IsTrue(MatchStr(settingVal, [SC_OPEN_WITH, SC_SEARCH, '2']), 'DelphiExtensionSettings|' + s +
			' should match the initial setting');
	end;

	// TRipGrepperSearchDialogForm.FormClose tested here
	FSettings.SearchFormSettings.UpdateFile(); // create temp section
	// Fsettings.IniFile.ReadTempSectionFiles(); // read temp section

	if Supports(FFactory, IFileHandler, fh) then begin
		fh.UpdateFile();
	end;

	FFactory.GetStringPersister(extSetting.INI_SECTION, extSetting.KEY_IDE_CONTEXT).TryLoadValue(iniVal);
	settingVal := IntToStr(Integer(FSettings.SearchFormSettings.ExtensionSettings.CurrentIDEContext.IDEContext));
	Assert.AreEqual(settingVal.Trim(['[', ']']), iniVal.Trim(['[', ']']), extSetting.KEY_IDE_CONTEXT + ' should be equal');

	FFactory.GetStringPersister(extSetting.INI_SECTION, extSetting.KEY_SHORTCUT_SEARCH_SELECTED).TryLoadValue(iniVal);
	settingVal := FSettings.SearchFormSettings.ExtensionSettings.SearchSelectedShortcut;
	Assert.AreEqual(settingVal.Trim(['[', ']']), iniVal.Trim(['[', ']']), extSetting.KEY_SHORTCUT_SEARCH_SELECTED + ' should be equal');

	FFactory.GetStringPersister(extSetting.INI_SECTION, extSetting.KEY_SHORTCUT_OPENWITH).TryLoadValue(iniVal);
	settingVal := FSettings.SearchFormSettings.ExtensionSettings.OpenWithShortcut;
	Assert.AreEqual(settingVal.Trim(['[', ']']), iniVal.Trim(['[', ']']), extSetting.KEY_SHORTCUT_OPENWITH + ' should be equal');
end;

procedure TRipGrepperSettingsTest.InitialSettingsShouldLoadDefaultNoodeLooks;
var
	s : IShared<TRipGrepperSettings>;
	nls : TNodeLookSettings;
begin
	s := Shared.Make<TRipGrepperSettings>(TRipGrepperSettings.Create());
	nls := s().NodeLookSettings;
	Assert.AreEqual(nls.AlternateRowColors, False, 'AlternateRowColors should be False');
	Assert.AreEqual(nls.ShowFileIcon, False, 'ShowFileIcon should be False');
	Assert.AreEqual(nls.IndentLines, False, 'NoIgnore should be False');
	Assert.AreEqual(nls.ShowRelativePath, False, 'IndentLines should be False');
	Assert.AreEqual(nls.ExpandNodes, False, 'ExpandNodes should be False');
end;

procedure TRipGrepperSettingsTest.InitialSettingsShouldLoadDefaultFontColors();
var
	s : IShared<TRipGrepperSettings>;
	cs : TColorSettings;
	df : IShared<TDefaultFontColors>;
begin
	s := Shared.Make<TRipGrepperSettings>(TRipGrepperSettings.Create());
	cs := s.FontColorSettings;

	df := Shared.Make<TDefaultFontColors>(TDefaultFontColors.Create(
		{ } TDarkModeHelper.GetActualThemeMode));

	Assert.AreEqual(cs.FontColors.AlternateRow.ToString, df.TREEVIEW_ALTERNATE_ROW.ToString, 'AlternateRowColors should be equal');
	Assert.AreEqual(cs.FontColors.ColNumText.ToString, df.TREEVIEW_COL_NUM_TEXT.ToString, 'ColNumTextColor should be equal');
	Assert.AreEqual(cs.FontColors.CounterText.ToString, df.TREEVIEW_STAT_TEXT.ToString, 'CounterTextColor should be equal');
	Assert.AreEqual(cs.FontColors.ErrorText.ToString, df.TREEVIEW_ERROR_TEXT.ToString, 'ErrorTextColor should be equal');
	Assert.AreEqual(cs.FontColors.FileText.ToString, df.TREEVIEW_FILE_TEXT.ToString, 'FileTextColor should be equal');
	Assert.AreEqual(cs.FontColors.LineNumText.ToString, df.TREEVIEW_LINE_NUM_TEXT.ToString, 'LineNumTextColor should be equal');
	Assert.AreEqual(cs.FontColors.MatchText.ToString, df.TREEVIEW_MATCH_TEXT.ToString, 'MatchTextColor should be equal');
	Assert.AreEqual(cs.FontColors.NormalText.ToString, df.TREEVIEW_NORMAL_TEXT.ToString, 'NormalTextColor should be equal');
	Assert.AreEqual(cs.FontColors.ReplaceText.ToString, df.TREEVIEW_REPLACE_TEXT.ToString, 'ReplaceTextColor should be equal');
	Assert.AreEqual(cs.FontColors.ReplaceTextInHistory.ToString, df.HIST_TREEVIEW_REPLACE_TEXT.ToString,
		'ReplaceTextInHistoryColor should be equal');
	Assert.AreEqual(cs.FontColors.ReplacedText.ToString, df.TREEVIEW_REPLACED_TEXT.ToString, 'ReplacedTextColor should be equal');
	Assert.AreEqual(cs.FontColors.SearchTextInHistory.ToString, df.HIST_TREEVIEW_SEARCH_TEXT.ToString, 'SearchTextColor should be equal');
	Assert.AreEqual(cs.FontColors.StatisticsText.ToString, df.TREEVIEW_STATS_TEXT.ToString, 'StatisticsTextColor should be equal');
end;

procedure TRipGrepperSettingsTest.UpdateFileTest();
begin
	SetSettingValues;
	FSettings.UpdateFile(); // config form close is tested here?
	FSettings.ReadIni;
	var
	iniSection := FSettings.SearchFormSettings.IniSectionName;
	Assert.AreEqual('none', FFactory.GetStringPersister().LoadSectionKey(iniSection, 'Encoding'));
	Assert.AreEqual(1, FFactory.GetIntegerPersister.LoadSectionKey(iniSection, 'Context'));
	Assert.AreEqual(True, FFactory.GetBoolPersister.LoadSectionKey(iniSection, 'Pretty'));
	Assert.AreEqual(True, FFactory.GetBoolPersister.LoadSectionKey(iniSection, 'Hidden'));
	Assert.AreEqual(True, FFactory.GetBoolPersister.LoadSectionKey(iniSection, 'NoIgnore'));

	var
	extSection := FSettings.SearchFormSettings.ExtensionSettings.IniSectionName;
	var
	scIniVal := FFactory.GetStringPersister.LoadSectionKey(extSection, 'OpenWithShortcut');
	Assert.AreEqual(SC_OPEN_WITH, scIniVal);
	scIniVal := FFactory.GetStringPersister.LoadSectionKey(extSection, 'SearchSelectedShortcut');
	Assert.AreEqual(SC_SEARCH, scIniVal);

end;

function TRipGrepperSettingsTest.ReadBoolIniAsString(const _section, _key : string) : string;
begin
	Result := BoolToStr(FFactory.GetBoolPersister.LoadSectionKey(_section, _key), True);
end;

procedure TRipGrepperSettingsTest.UpdateFileAfterConfigTest;
begin
	SetSettingValues;

	var
	dbgArr := TSettingsDictionary.DictToStringArray(FSettings.SettingsDict());
	var
	appSection := FSettings.AppSettings.IniSectionName;

	FSettings.UpdateFile(); // config form close is tested here?
	dbgArr := TSettingsDictionary.DictToStringArray(FSettings.SettingsDict());

	FSettings.ReadIni;
	dbgArr := TSettingsDictionary.DictToStringArray(FSettings.SettingsDict());
	var
	cbsh := FSettings.SettingsDict()[appSection]['CopyToClipBoardShell'].AsString;

	Assert.AreEqual(True, FBoolPers.LoadSectionKey(appSection, 'ExpertMode'), 'ExpertMode should be True');
	cbsh := FSettings.SettingsDict()[appSection]['CopyToClipBoardShell'].AsString;
	Assert.AreEqual('Carbon', FStrPers.LoadSectionKey(appSection, 'ColorTheme'), 'ColorTheme should be Carbon');
    var iniVal := Integer(FSettings.AppSettings.CopyToClipBoardShell);
    FFactory.GetIntegerPersister(appSection, 'CopyToClipBoardShell').TryLoadValue(iniVal);
	Assert.AreEqual(0, iniVal, 'CopyToClipBoardShell should be 1');

	var
	extSection := FSettings.SearchFormSettings.ExtensionSettings.IniSectionName;
	Assert.AreEqual(SC_OPEN_WITH, FStrPers.LoadSectionKey(extSection, 'OpenWithShortcut'), 'OpenWithShortcut should be SC_OPEN_WITH');
	Assert.AreEqual(SC_SEARCH, FStrPers.LoadSectionKey(extSection, 'SearchSelectedShortcut'), 'SearchSelectedShortcut should be SC_SEARCH');
end;

procedure TRipGrepperSettingsTest.UpdateHistInIniTest;
begin
	SetSettingValues;
	// see SearchForm.OnClose.
	FSettings.StoreHistories();
	FSettings.UpdateFile();
	FSettings.ReadIni;

	var
	dbgArr := TSettingsDictionary.DictToStringArray(FSettings.SettingsDict());

	Assert.AreEqual(ACT_HIST_VAL, FSettings.SearchTextsHistory.AsArray[0], 'SearchTextsHistory[0] should be hist 0');
	Assert.AreEqual(ACT_HIST_VAL,
		{ } FFactory.GetStringPersister.LoadSectionKey('SearchTextsHistory', 'Item_0'));
end;

procedure TRipGrepperSettingsTestBase.SetSettingValues();
begin

	FSettings.AppSettings.ExpertMode := True;
	FSettings.AppSettings.CopyToClipBoardShell := TShellType.stPowershell;
	FSettings.AppSettings.ColorTheme := 'Carbon';

	FSettings.FontColorSettings.LoadDefaultColors(EThemeMode.tmLight);

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
		FTextHist.Insert(0, s);

	FSettings.SearchTextsHistory.Value := FTextHist.Items;
	FSettings.StoreToPersister;

	FstrPers := FFactory.GetStringPersister();
	FintPers := FFactory.GetIntegerPersister();
	FboolPers := FFactory.GetBoolPersister();
end;

procedure TRipGrepperSettingsTestBase.Setup();
begin
	FSettings := Shared.Make<TRipGrepperSettings>(TRipGrepperSettings.Create());
	FFactory := FSettings.PersisterFactory;
end;

procedure TRipGrepperSettingsTestBase.TearDown();
begin
	FSettings := nil;
	TFileUtils.EmptyFile(ChangeFileExt(Application.ExeName, '.ini'));
end;

procedure TInnerSearchFormSettingsTest.EncodingChangeAndUpdateFile();
var
	settingVal, iniVal : string;
begin
	FSettings.SearchFormSettings.Encoding := 'utf8';
	FSettings.StoreToPersister();
	FSettings.UpdateFile();

	iniVal := FFactory.GetStringPersister().LoadSectionKey(FSettings.SearchFormSettings.IniSectionName, 'Encoding');
	settingVal := FSettings.SearchFormSettings.Encoding;

	Assert.AreEqual(settingVal, iniVal, 'Encoding should be equal');
end;

procedure TInnerSearchFormSettingsTest.EncodingChangeChangesDictionary();
var
	settingVal, dictVal : string;
begin
	FSettings.SearchFormSettings.Encoding := 'utf8';

	var
	dbgArr := TSettingsDictionary.DictToStringArray(FSettings.SettingsDict());

	settingVal := FSettings.SearchFormSettings.Encoding;
	dictVal := FSettings.SettingsDict()['RipGrepperSearchSettings']['Encoding'].AsString;

	Assert.AreEqual(settingVal, dictVal, 'Encoding should be equal');
end;

procedure TInnerSearchFormSettingsTest.ContextChangeChangesDictionary();
var
	settingVal, dictVal : string;
begin
	FSettings.SearchFormSettings.Context := 1;

	var
	dbgArr := TSettingsDictionary.DictToStringArray(FSettings.SettingsDict());

	settingVal := FSettings.SearchFormSettings.Context.ToString;
	dictVal := FSettings.SettingsDict()['RipGrepperSearchSettings']['Context'].AsString;

	Assert.AreEqual(settingVal, dictVal, 'Encoding should be equal');
end;

procedure TInnerSearchFormSettingsTest.UpdateShortCutsIniTest();
begin
	SetSettingValues; // after config form close is tested here
	// FSettings.SearchFormSettings.ExtensionSettings.UpdateFile(
	// { } FSettings.SearchFormSettings.ExtensionSettings.INI_SECTION);

	FSettings.UpdateFile();

	Assert.AreEqual(SC_OPEN_WITH,
		{ } FFactory.GetStringPersister().LoadSectionKey(
		{ } FSettings.SearchFormSettings.ExtensionSettings.INI_SECTION,
		{ } FSettings.SearchFormSettings.ExtensionSettings.KEY_SHORTCUT_OPENWITH));

	FSettings.ReadIni;
	Assert.AreEqual(SC_OPEN_WITH, FSettings.SearchFormSettings.ExtensionSettings.OpenWithShortcut, 'OpenWithShortcut should be ok');
end;

initialization

TDUnitX.RegisterTestFixture(TInnerSearchFormSettingsTest);
TDUnitX.RegisterTestFixture(TRipGrepperSettingsTest);

end.
