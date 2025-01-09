unit RipGrepper.Settings.RipGrepperSettingsTest;

interface

uses
	RipGrepper.Settings.SearchFormSettings,
	System.IniFiles,
	RipGrepper.Settings.RipGrepperSettings,
	DUnitX.TestFramework;

type

	[TestFixture]
	TRipGrepperSettingsTest = class
		const
			INIFILE = 'DripGrepperUnittest.ini';

		private
			// FIniFile : TMemIniFile;
			FSettings : TRipGrepperSettings;
			procedure EmptyFile(const _filePath : string);
			procedure SetDefaults;

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
	end;

implementation

uses
	RipGrepper.Common.Constants,
	System.SysUtils,
	Vcl.Forms,
	RipGrepper.Common.GuiSearchParams,
	RipGrepper.Common.SimpleTypes;

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
	SetDefaults;
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
	SetDefaults;
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
	SetDefaults;
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
	SetDefaults;

	FSettings.UpdateIniFile;

	iniVal := FSettings.IniFile.ReadString(FSettings.RipGrepParameters.IniSectionName, 'SearchParams', '');
	settingVal := FSettings.RipGrepParameters.GuiSearchTextParams.GetAsString(True);
	Assert.AreEqual(settingVal.Trim(['[', ']']), iniVal.Trim(['[', ']']), 'SearchParams should be equal');

	FSettings.LoadDefaultsFromDict;
	iniVal := FSettings.IniFile.ReadString(FSettings.RipGrepParameters.IniSectionName, 'SearchParams_DEFAULT', '');
	settingVal := FSettings.RipGrepParameters.GuiSearchTextParams.GetAsString(True);
	Assert.AreEqual(settingVal, iniVal, 'SearchParams_DEFAULT should be equal');

//	iniVal := FSettings.IniFile.ReadString(FSettings.SearchFormSettings.IniSectionName, 'Encoding', '');
//	Assert.AreEqual('', iniVal, 'Encoding should be empty');
//
//	iniVal := FSettings.IniFile.ReadString(FSettings.SearchFormSettings.IniSectionName, 'Context', '');
//	Assert.AreEqual('', iniVal, 'Context should be empty');
//
//	iniVal := FSettings.IniFile.ReadString(FSettings.SearchFormSettings.IniSectionName, 'Pretty', '');
//	Assert.AreEqual('', iniVal, 'Pretty should be empty');
//
//	iniVal := FSettings.IniFile.ReadString(FSettings.SearchFormSettings.IniSectionName, 'Hidden', '');
//	Assert.AreEqual('', iniVal, 'Hidden should be empty');
//
//	iniVal := FSettings.IniFile.ReadString(FSettings.SearchFormSettings.IniSectionName, 'NoIgnore', '');
//	Assert.AreEqual('', iniVal, 'NoIgnore should be empty');
//
//	iniVal := FSettings.IniFile.ReadString(FSettings.RipGrepParameters.IniSectionName, 'SearchPath', '');
//	Assert.AreEqual('', iniVal, 'SearchPath should be empty');
//
//	iniVal := FSettings.IniFile.ReadString(FSettings.RipGrepParameters.IniSectionName, 'FileMasks', '');
//	Assert.AreEqual('', iniVal, 'FileMasks should be empty');
//
//	iniVal := FSettings.IniFile.ReadString(FSettings.RipGrepParameters.IniSectionName, 'SearchOptions', '');
//	Assert.AreEqual('', iniVal, 'SearchOptions should be empty');
//
//	iniVal := FSettings.IniFile.ReadString(FSettings.RipGrepParameters.IniSectionName, 'SearchOptions', '');
//	Assert.AreEqual('', iniVal, 'SearchOptions should be empty');
//
//	iniVal := FSettings.IniFile.ReadString(FSettings.RipGrepParameters.IniSectionName, 'SearchOptions', '');
//	Assert.AreEqual('', iniVal, 'SearchOptions should be empty');
//
//	iniVal := FSettings.IniFile.ReadString(FSettings.RipGrepParameters.IniSectionName, 'SearchOptions', '');
//	Assert.AreEqual('', iniVal, 'SearchOptions should be empty');
//
//	iniVal := FSettings.IniFile.ReadString(FSettings.RipGrepParameters.IniSectionName, 'SearchOptions', '');
//	Assert.AreEqual('', iniVal, 'SearchOptions should be empty');

end;

procedure TRipGrepperSettingsTest.SetDefaults;
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

end;

procedure TRipGrepperSettingsTest.Setup;
begin
	FSettings := TRipGrepperSettings.Create();
end;

procedure TRipGrepperSettingsTest.TearDown;
begin
	FSettings.Free; // instance will be free
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

initialization

TDUnitX.RegisterTestFixture(TRipGrepperSettingsTest);

end.
