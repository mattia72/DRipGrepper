unit RipGrepper.Settings.RipGrepSettingsTest;

interface

uses
	RipGrepper.Settings.SearchFormSettings,
	System.IniFiles,
	RipGrepper.Settings.RipGrepParameterSettings,
	DUnitX.TestFramework;

type

	[TestFixture]
	TRipGrepSettingsTest = class
		const
			INIFILE = 'DripGrepperUnittest.ini';

		private
			FIniFile : TMemIniFile;
			FSettings : TRipGrepParameterSettings;
			procedure SetDefaultsAndCurrentValues;

		public
			constructor Create;
			destructor Destroy; override;

			[Setup]
			procedure Setup;
			[TearDown]
			procedure TearDown;

			[Test]
			procedure WithoutIniReadShouldLoadDefaultsTest;
			[Test]
			procedure LoadDefaultsShouldReadDefaultFromIni;
			[Test]
			procedure AfterCopyValuesValuesShouldBeEqual;
			[Test]
			procedure LoadDefaultsTest;
			[Test]
			procedure LoadActualTest;
			[Test]
			procedure StoreAsDefaultsToDictTest;
			[Test]
			procedure DictActualTest;
			[Test]
			procedure UpdateIniReloadTest;
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
	System.Variants;

const
	MATCHWORD_USEREGEX = '[MatchWord,UseRegex]';
	C_DEF_PATH_TO_DIR = 'c:\def\path\to\dir';
	DEFPAS_DEFDFM = '*.defpas;*.defdfm';
	C_PATH_TO_DIR = 'c:\path\to\dir';
	PAS_DFM = '*.pas;*.dfm';
	RG_EXE_PATH = 'rg.exe';
	EGUIOPTION_MATCHCASE = '[MatchCase]';

constructor TRipGrepSettingsTest.Create;
begin
	inherited;
end;

destructor TRipGrepSettingsTest.Destroy;
begin
	// FIniFile.Free;
	inherited;
end;

procedure TRipGrepSettingsTest.WithoutIniReadShouldLoadDefaultsTest;
begin
	FSettings.LoadDefaultsFromDict;
	Assert.IsTrue(FileExists(FSettings.RipGrepPath), 'RipGrepPath should be an existing path');
	Assert.AreEqual('', FSettings.FileMasks, 'FileMasks should be empty');
	Assert.AreEqual('', FSettings.SearchPath, 'SearchPath should be empty');
	Assert.AreEqual('[]', FSettings.GuiSearchTextParams.GetAsString(True), 'GuiSearchTextParams should be empty');
end;

procedure TRipGrepSettingsTest.LoadDefaultsShouldReadDefaultFromIni;
begin
	SetDefaultsAndCurrentValues;
	FSettings.UpdateIniFile;
	var
	strs := TStringList.Create();
	try
		FIniFile.ReadSection(FSettings.IniSectionName, strs);
		var
		keyCount := 2 * 3 + 1;
		Assert.AreEqual(keyCount, strs.Count, Format('%s section should countain %d keys.', [FSettings.IniSectionName, keyCount]));
	finally
		strs.Free;
	end;
	FSettings.LoadDefaultsFromDict;
	Assert.IsTrue(FileExists(FSettings.RipGrepPath), 'RipGrepPath should be set');
	Assert.IsTrue(FSettings.FileMasks.Contains('def'), 'FileMasks should be set');
	Assert.IsTrue(FSettings.SearchPath.Contains('def'), 'SearchPath should be set');
	Assert.AreEqual(EGUIOPTION_MATCHCASE,
		{ } FSettings.GuiSearchTextParams.GetAsString(True), 'GuiSearchTextParams should be set');
end;

procedure TRipGrepSettingsTest.AfterCopyValuesValuesShouldBeEqual;
var
	s1, s2 : string;
begin
	SetDefaultsAndCurrentValues;
	FSettings.LoadDefaultsFromDict;
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

procedure TRipGrepSettingsTest.SetDefaultsAndCurrentValues;
begin
	FSettings.RipGrepPath := 'not default relevant\rg.exe';
	FSettings.FileMasks := DEFPAS_DEFDFM;
	FSettings.SearchPath := C_DEF_PATH_TO_DIR;
	FSettings.GuiSearchTextParams.SetSearchOptions(
		{ } TSearchTextWithOptions.StringToSearchOptionSet(EGUIOPTION_MATCHCASE));
	// Assert.AreEqual(0, FSettings.SettingsDict.Keys.Count, 'SettingsDict should be empty');
	FSettings.StoreAsDefaultsToDict;

	FSettings.RipGrepPath := RG_EXE_PATH;
	FSettings.FileMasks := PAS_DFM;
	FSettings.SearchPath := C_PATH_TO_DIR;
	FSettings.GuiSearchTextParams.SetSearchOptions(
		{ } TSearchTextWithOptions.StringToSearchOptionSet(MATCHWORD_USEREGEX));
	FSettings.StoreToDict;
end;

procedure TRipGrepSettingsTest.Setup;
begin
	FSettings := TRipGrepParameterSettings.Create(nil);
	FSettings.OwnIniFile := False;
	FSettings.GuiSearchTextParams.OwnIniFile := False;

	var
	iniName := ChangeFileExt(Application.ExeName, '.ini');

	if FileExists(iniName) then begin
		TFileUtils.EmptyFile(iniName);
	end;

	FIniFile := TMemIniFile.Create(iniName, TEncoding.UTF8);

	FSettings.IniFile := FIniFile;
	Assert.IsTrue(FSettings.IniFile = FSettings.GuiSearchTextParams.IniFile);
end;

procedure TRipGrepSettingsTest.TearDown;
begin
	FSettings.Free; // instance will be free
	FIniFile.Free;
	TFileUtils.EmptyFile(ChangeFileExt(Application.ExeName, '.ini'));
end;

procedure TRipGrepSettingsTest.LoadDefaultsTest;
begin
	SetDefaultsAndCurrentValues;
	FSettings.LoadDefaultsFromDict;
	Assert.IsTrue(FSettings.RipGrepPath.Contains(RG_EXE_PATH), 'RipGrepPath should be set');

	Assert.AreEqual(DEFPAS_DEFDFM, FSettings.FileMasks, 'FileMasks should be set');
	Assert.AreEqual(C_DEF_PATH_TO_DIR, FSettings.SearchPath, 'SearchPath should be set');
	Assert.AreEqual(EGUIOPTION_MATCHCASE,
		{ } FSettings.GuiSearchTextParams.GetAsString(true), 'GuiSearchTextParams should be set');
end;

procedure TRipGrepSettingsTest.LoadActualTest;
begin
	SetDefaultsAndCurrentValues;

	for var s in ['FileMasks', 'SearchPath'] do begin
		Assert.AreNotEqual(VarToStrDef(FSettings.SettingsDict[FSettings.IniSectionName + '|' + s].Value, ''),
			{ } VarToStrDef(FSettings.SettingsDict[FSettings.IniSectionName + '|' + s + DEFAULT_KEY].Value, ''),
			{ } 'Default and actual are not equal');
	end;

	FSettings.LoadFromDict;
	Assert.IsTrue(FSettings.RipGrepPath.Contains(RG_EXE_PATH), 'RipGrepPath should be set');
	Assert.AreEqual(PAS_DFM, FSettings.FileMasks, '2. FileMasks shouldn''t be the default');
	Assert.AreEqual(C_PATH_TO_DIR, FSettings.SearchPath, 'SearchPath should be set');
	Assert.AreEqual(MATCHWORD_USEREGEX, FSettings.GuiSearchTextParams.GetAsString(true), 'GuiSearchTextParams should be set');
end;

procedure TRipGrepSettingsTest.StoreAsDefaultsToDictTest;
begin
	FSettings.RipGrepPath := 'not default relevant\rg.exe';
	FSettings.FileMasks := DEFPAS_DEFDFM;
	FSettings.SearchPath := C_DEF_PATH_TO_DIR;
	FSettings.GuiSearchTextParams.SetSearchOptions(TSearchTextWithOptions.StringToSearchOptionSet(EGUIOPTION_MATCHCASE));

	FSettings.StoreAsDefaultsToDict;

	var
	dict := FSettings.SettingsDict;
	var
	inisec := FSettings.IniSectionName;

	Assert.AreEqual(DEFPAS_DEFDFM,
		{ } VarToStrDef(dict[inisec + '|FileMasks' + DEFAULT_KEY].Value, ''),
		{ } 'Default and actual are not equal');
	Assert.AreEqual(C_DEF_PATH_TO_DIR,
		{ } VarToStrDef(dict[inisec + '|SearchPath' + DEFAULT_KEY].Value, ''),
		{ } 'Default and actual are not equal');
	Assert.AreEqual(EGUIOPTION_MATCHCASE,
		{ } VarToStrDef(dict[inisec + '|SearchParams' + DEFAULT_KEY].Value, ''),
		{ } 'Default and actual are not equal');
end;

procedure TRipGrepSettingsTest.DictActualTest;
begin
	SetDefaultsAndCurrentValues;

	var
	dict := FSettings.SettingsDict;
	var
	inisec := FSettings.IniSectionName;

	Assert.AreEqual(PAS_DFM,
		{ } VarToStrDef(dict[inisec + '|FileMasks'].Value, ''),
		{ } 'FileMasks are not equal');
	Assert.AreEqual(C_PATH_TO_DIR,
		{ } VarToStrDef(dict[inisec + '|SearchPath'].Value, ''),
		{ } 'SearchPath are not equal');
	Assert.AreEqual(MATCHWORD_USEREGEX,
		{ } VarToStrDef(dict[inisec + '|SearchParams'].Value, ''),
		{ } 'SearchParams are not equal');
end;

procedure TRipGrepSettingsTest.UpdateIniReloadTest;
begin
	SetDefaultsAndCurrentValues;
	FSettings.UpdateIniFile();
	FSettings.FileMasks := '';
	FSettings.SearchPath := '';
	FSettings.ReLoad; // fills only settings dict
	FSettings.LoadFromDict; // why loads defaults ?????
	Assert.IsTrue(FSettings.RipGrepPath.Contains(RG_EXE_PATH), 'RipGrepPath should be set');
	Assert.AreEqual(PAS_DFM, FSettings.FileMasks, 'FileMasks should be set');
	Assert.AreEqual(C_PATH_TO_DIR, FSettings.SearchPath, 'SearchPath should be set');
	Assert.AreEqual(MATCHWORD_USEREGEX,
		{ } FSettings.GuiSearchTextParams.GetAsString(true), 'GuiSearchTextParams should be set');
end;

initialization

TDUnitX.RegisterTestFixture(TRipGrepSettingsTest);

end.
