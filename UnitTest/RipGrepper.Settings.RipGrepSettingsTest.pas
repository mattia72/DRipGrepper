unit RipGrepper.Settings.RipGrepSettingsTest;

interface

uses
	RipGrepper.Settings.SearchFormSettings,
	System.IniFiles,
	RipGrepper.Settings.RipGrepParameterSettings,
	DUnitX.TestFramework,
	Spring,
	RipGrepper.Settings.FilePersister;

type

	[TestFixture]
	TRipGrepSettingsTest = class
		const
			C_PATH_TO_DIR = 'c:\path\to\dir';
			PAS_DFM = '*.pas;*.dfm';
			EGUIOPTION_MATCHCASE = '[MatchCase]';
			MATCHWORD_USEREGEX = '[MatchWord,UseRegex]';

		private
			FIniFile : IShared<TMemIniFile>;
			FPersisterFactory : IPersisterFactory;
			FSettings : TRipGrepParameterSettings;
			procedure SetSettingValues();
			procedure SetRipGrepArguments(const Settings : TRipGrepParameterSettings);

		public
			[Setup]
			procedure Setup;
			[TearDown]
			procedure TearDown;

			[Test]
			procedure AfterCopyValuesValuesShouldBeEqual;
			[Test]
			procedure LoadActualTest;
			[Test]
			procedure DictActualTest;
			[Test]
			procedure UpdateIniReloadTest;
			[Test]
			procedure TestGetCommandLineCmd();
			[Test]
			procedure TestGetCommandLineNone();
			[Test]
			procedure TestGetCommandLinePowershell();
	end;

implementation

uses
	RipGrepper.Common.Constants,
	System.SysUtils,
	Vcl.Forms,
	RipGrepper.Common.GuiSearchParams,
	System.Classes,
	RipGrepper.Common.SearchTextWithOptions,
	RipGrepper.Common.SimpleTypes,
	RipGrepper.Tools.FileUtils,
	System.Variants,
	RipGrepper.Settings.SettingsDictionary;

procedure TRipGrepSettingsTest.AfterCopyValuesValuesShouldBeEqual;
var
	s1, s2 : string;
begin
	SetSettingValues;
	// FSettings.LoadFromDict; // FSettings.LoadDefaultsFromDict;
	var
	s := TRipGrepParameterSettings.Create(nil);
	try
		s.Copy(FSettings);

		s1 := FSettings.GuiSearchTextParams.GetAsString();
		s2 := s.GuiSearchTextParams.GetAsString();
		Assert.AreEqual(s1, s2, 'GuiSearchTextParams should be equal');

		Assert.AreEqual(FSettings.RipGrepPath,
			{ } s.RipGrepPath, 'RipGrepPath should be equal');
		Assert.AreEqual(FSettings.FileMasks,
			{ } s.FileMasks, 'FileMasks should be eq');
		Assert.AreEqual(FSettings.SearchPath,
			{ } s.SearchPath, 'SearchPath should be eq');

		s1 := FSettings.GuiSearchTextParams.GetAsString();
		s2 := s.GuiSearchTextParams.GetAsString();

		Assert.AreEqual(s1, s2, 'still GuiSearchTextParams should be equal');
	finally
		s.Free;
	end;
end;

procedure TRipGrepSettingsTest.SetSettingValues();
begin
	FSettings.RipGrepPath := RG_EXE;
	FSettings.FileMasks := PAS_DFM;
	FSettings.SearchPath := C_PATH_TO_DIR;
	FSettings.GuiSearchTextParams.SetSearchOptions(
		{ } TSearchTextWithOptions.StringToSearchOptionSet(MATCHWORD_USEREGEX));
end;

procedure TRipGrepSettingsTest.Setup;
begin
	FSettings := TRipGrepParameterSettings.Create(nil);
	FSettings.IsOwnerOfPersisterFactory := False;
	FSettings.GuiSearchTextParams.IsOwnerOfPersisterFactory := False;

	var
	iniName := ChangeFileExt(Application.ExeName, '.ini');

	if FileExists(iniName) then begin
		TFileUtils.EmptyFile(iniName);
	end;

	FIniFile := Shared.Make<TMemIniFile>(TMemIniFile.Create(iniName, TEncoding.UTF8));
	FPersisterFactory := TIniPersister.Create(FIniFile);

	Assert.IsTrue(FSettings.PersisterFactory.FilePath = FSettings.GuiSearchTextParams.PersisterFactory.FilePath,
		'File Path should be equal.');
end;

procedure TRipGrepSettingsTest.TearDown;
begin
	FSettings.Free; // instance will be free

	TFileUtils.EmptyFile(ChangeFileExt(Application.ExeName, '.ini'));
end;

procedure TRipGrepSettingsTest.LoadActualTest;
begin
	SetSettingValues;

	// FSettings.LoadFromDict;
	Assert.IsTrue(FSettings.RipGrepPath.Contains(RG_EXE), 'RipGrepPath should be set');
	Assert.AreEqual(PAS_DFM, FSettings.FileMasks, '2. FileMasks shouldn''t be the default');
	Assert.AreEqual(C_PATH_TO_DIR, FSettings.SearchPath, 'SearchPath should be set');
	Assert.AreEqual(MATCHWORD_USEREGEX, FSettings.GuiSearchTextParams.GetAsString(true), 'GuiSearchTextParams should be set');
end;

procedure TRipGrepSettingsTest.DictActualTest;
begin
	SetSettingValues;

	var
	dict := FSettings.SettingsDict;
	var
	inisec := FSettings.IniSectionName;

	var
	dbgArr := TSettingsDictionary.DictToStringArray(dict());

	Assert.AreEqual(PAS_DFM,
		{ } dict()[inisec]['FileMasks'].AsString, 'FileMasks are not equal');
	Assert.AreEqual(C_PATH_TO_DIR,
		{ } dict()[inisec]['SearchPath'].AsString, 'SearchPath are not equal');
	Assert.AreEqual(MATCHWORD_USEREGEX,
		{ } dict()[inisec]['SearchParams'].AsString, 'SearchParams are not equal');
end;

procedure TRipGrepSettingsTest.SetRipGrepArguments(const Settings : TRipGrepParameterSettings);
begin
	Settings.RipGrepPath := 'C:\Path\To\rg.exe';
	Settings.RipGrepPathInitResult := rgpiFound;
	Settings.RipGrepArguments := Shared.Make<TStringList>();
	Settings.RipGrepArguments.AddPair(RG_ARG_OPTIONS, '--vimgrep');
	Settings.RipGrepArguments.AddPair(RG_ARG_OPTIONS, '-g=*.txt');
	// Settings.RipGrepArguments.AddPair(RG_ARG_OPTIONS, '--replace=replace text');
	Settings.RipGrepArguments.AddPair(RG_ARG_OPTIONS, RG_PARAM_END);
	Settings.RipGrepArguments.AddPair(RG_ARG_SEARCH_TEXT, 'search text');
//  Settings.RipGrepArguments.AddPair(RG_ARG_REPLACE_TEXT, 'replace text');
	Settings.RipGrepArguments.AddPair(RG_ARG_SEARCH_PATH, 'C:\Path\Search\Files');
end;

procedure TRipGrepSettingsTest.UpdateIniReloadTest;
 begin
	SetSettingValues;
	FSettings.UpdateFile(True{_bForceWritePersister});
	FSettings.FileMasks := '';
	FSettings.SearchPath := '';
	FSettings.ReLoad; // fills only settings dict
	// FSettings.LoadFromDict; // why loads defaults ?????
	Assert.IsTrue(FSettings.RipGrepPath.Contains(RG_EXE), 'RipGrepPath should be set');
	Assert.AreEqual(PAS_DFM, FSettings.FileMasks, 'FileMasks should be set');
	Assert.AreEqual(C_PATH_TO_DIR, FSettings.SearchPath, 'SearchPath should be set');
	Assert.AreEqual(MATCHWORD_USEREGEX,
		{ } FSettings.GuiSearchTextParams.GetAsString(true), 'GuiSearchTextParams should be set');
end;

procedure TRipGrepSettingsTest.TestGetCommandLineCmd();
var
	cmdLine : string;
begin
	SetRipGrepArguments(FSettings);
	cmdLine := FSettings.GetCommandLine(TShellType.stCmd);
	Assert.AreEqual(
		{ } '"C:\Path\To\rg.exe" --vimgrep -g=*.txt -- "search text" "C:\Path\Search\Files"', cmdLine);
end;

procedure TRipGrepSettingsTest.TestGetCommandLineNone();
var
	cmdLine : string;
begin
	SetRipGrepArguments(FSettings);
	cmdLine := FSettings.GetCommandLine(TShellType.stNone);
	Assert.AreEqual(
		{ } 'C:\Path\To\rg.exe --vimgrep -g=*.txt -- search text C:\Path\Search\Files', cmdLine);
end;

procedure TRipGrepSettingsTest.TestGetCommandLinePowershell();
var
	cmdLine : string;
begin
	SetRipGrepArguments(FSettings);
	cmdLine := FSettings.GetCommandLine(TShellType.stPowershell);
	Assert.AreEqual(
		{ } '&''C:\Path\To\rg.exe'' --vimgrep -g=''*.txt'' -- ''search text'' ''C:\Path\Search\Files''', cmdLine);
end;

initialization

TDUnitX.RegisterTestFixture(TRipGrepSettingsTest);

end.
