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
	end;

implementation

uses
	RipGrepper.Common.Constants,
	System.SysUtils,
	Vcl.Forms,
	RipGrepper.Common.GuiSearchParams,
	System.Classes;

constructor TRipGrepSettingsTest.Create;
begin
	inherited;
	FIniFile := TMemIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'), TEncoding.UTF8);
end;

destructor TRipGrepSettingsTest.Destroy;
begin
	FIniFile.Free;
	inherited;
end;

procedure TRipGrepSettingsTest.WithoutIniReadShouldLoadDefaultsTest;
begin
	FSettings.LoadDefaultsFromDict;
	Assert.IsTrue(FSettings.RipGrepPath <> '', 'RipGrepPath should be set');
	Assert.IsTrue(FSettings.FileMasks = '', 'FileMasks should be empty');
	Assert.IsTrue(FSettings.SearchPath = '', 'SearchPath should be empty');
	Assert.IsTrue(FSettings.GuiSearchTextParams.GetAsString(True) = '', 'GuiSearchTextParams should be empty');
end;

procedure TRipGrepSettingsTest.LoadDefaultsShouldReadDefaultFromIni;
begin
	SetDefaultsAndCurrentValues;
	FSettings.UpdateIniFile;
	var
	strs := TStringList.Create();
	try
		FIniFile.ReadSection(FSettings.IniSectionName, strs);
		Assert.AreEqual(6, strs.Count, FSettings.IniSectionName + ' section should countain 6 key.')
	finally

		strs.Free;
	end;
	FSettings.LoadDefaultsFromDict;
	Assert.IsTrue(FSettings.RipGrepPath <> '', 'RipGrepPath should be set');
	Assert.IsTrue(FSettings.FileMasks <> '', 'FileMasks should be set');
	Assert.IsTrue(FSettings.SearchPath <> '', 'SearchPath should be set');
	Assert.IsTrue(FSettings.GuiSearchTextParams.GetAsString(True) <> '', 'GuiSearchTextParams should be set');
end;

procedure TRipGrepSettingsTest.AfterCopyValuesValuesShouldBeEqual;
begin
	SetDefaultsAndCurrentValues;
	FSettings.LoadDefaultsFromDict;
	var
	s := TRipGrepParameterSettings.Create(nil);
	try
		s.Copy(FSettings);

		Assert.AreEqual(s.RipGrepPath,
			{ } FSettings.RipGrepPath, 'RipGrepPath should be equal');
		Assert.AreEqual(s.FileMasks,
			{ } FSettings.FileMasks, 'FileMasks should be eq');
		Assert.AreEqual(s.SearchPath,
			{ } FSettings.SearchPath, 'SearchPath should be eq');
		Assert.AreEqual(s.GuiSearchTextParams.GetAsString(),
			{ } FSettings.GuiSearchTextParams.GetAsString(), 'GuiSearchTextParams should be true');
	finally
		s.Free;
	end;
end;

procedure TRipGrepSettingsTest.SetDefaultsAndCurrentValues;
begin
	FSettings.RipGrepPath := 'def\rg.exe';
	FSettings.FileMasks := '*.defpas;*.defdfm';
	FSettings.SearchPath := 'c:\def\path\to\dir';
	FSettings.GuiSearchTextParams.SearchOptions := [EGuiOption.soMatchCase, EGuiOption.soMatchCase];
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

	FSettings.IniFile := FIniFile;
	Assert.IsTrue(FSettings.IniFile = FSettings.GuiSearchTextParams.IniFile);
end;

procedure TRipGrepSettingsTest.TearDown;
begin
	FSettings.Free; // instance will be free
	EmptyFile(ChangeFileExt(Application.ExeName, '.ini'));
end;

procedure TRipGrepSettingsTest.EmptyFile(const _filePath : string);
var
	txtFile : TextFile;
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

initialization

TDUnitX.RegisterTestFixture(TRipGrepSettingsTest);

end.
