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
			procedure EmptyFile(const _filePath : string);
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
			procedure UpdateIniTest;
	end;

implementation

uses
	RipGrepper.Common.Constants,
	System.SysUtils,
	Vcl.Forms,
	RipGrepper.Common.GuiSearchParams,
	System.Classes,
	RipGrepper.Common.SearchParams,
	RipGrepper.Common.SimpleTypes;

const
	EGUIOPTION_MATCHCASE = '[MatchCase]';

constructor TRipGrepSettingsTest.Create;
begin
	inherited;
end;

destructor TRipGrepSettingsTest.Destroy;
begin
//  FIniFile.Free;
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
	Assert.AreEqual(EGUIOPTION_MATCHCASE, FSettings.GuiSearchTextParams.GetAsString(True), 'GuiSearchTextParams should be set');
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
	FSettings.FileMasks := '*.defpas;*.defdfm';
	FSettings.SearchPath := 'c:\def\path\to\dir';
	FSettings.GuiSearchTextParams.SearchOptions := TSearchParams.StringToSearchParams(EGUIOPTION_MATCHCASE);
	FSettings.StoreAsDefaultsToDict;

	FSettings.RipGrepPath := 'rg.exe';
	FSettings.FileMasks := '*.pas;*.dfm';
	FSettings.SearchPath := 'c:\path\to\dir';
	FSettings.GuiSearchTextParams.SearchOptions := [EGuiOption.soNotSet];
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
		EmptyFile(iniName);
	end;

	FIniFile := TMemIniFile.Create(iniName, TEncoding.UTF8);

	FSettings.IniFile := FIniFile;
	Assert.IsTrue(FSettings.IniFile = FSettings.GuiSearchTextParams.IniFile);
end;

procedure TRipGrepSettingsTest.TearDown;
begin
	FSettings.Free; // instance will be free
	FIniFile.Free;
	EmptyFile(ChangeFileExt(Application.ExeName, '.ini'));
end;

procedure TRipGrepSettingsTest.EmptyFile(const _filePath : string);
var txtFile : TextFile;
begin
	AssignFile(txtFile, _filePath);
	Rewrite(txtFile);
	CloseFile(txtFile);
end;

procedure TRipGrepSettingsTest.LoadDefaultsTest;
begin
	SetDefaultsAndCurrentValues;
	FSettings.LoadDefaultsFromDict;
	Assert.IsTrue(FSettings.RipGrepPath <> '', 'RipGrepPath should be set');
	Assert.IsTrue(FSettings.FileMasks <> '', 'FileMasks should be set');
	Assert.IsTrue(FSettings.SearchPath <> '', 'SearchPath should be set');
	Assert.IsTrue(FSettings.GuiSearchTextParams.GetAsString(True) <> '', 'GuiSearchTextParams should be set');
end;

procedure TRipGrepSettingsTest.UpdateIniTest;
begin
	SetDefaultsAndCurrentValues;
	FSettings.UpdateIniFile();
	FSettings.ReLoad;
	Assert.IsTrue(FSettings.RipGrepPath <> '', 'RipGrepPath should be set');
	Assert.IsTrue(FSettings.FileMasks <> '', 'FileMasks should be set');
	Assert.IsTrue(FSettings.SearchPath <> '', 'SearchPath should be set');
	Assert.IsTrue(FSettings.GuiSearchTextParams.GetAsString(True) <> '', 'GuiSearchTextParams should be set');
end;

initialization

TDUnitX.RegisterTestFixture(TRipGrepSettingsTest);

end.
