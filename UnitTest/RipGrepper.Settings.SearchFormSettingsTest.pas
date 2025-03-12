unit RipGrepper.Settings.SearchFormSettingsTest;

interface

uses
	DUnitX.TestFramework,
	System.IniFiles,
	RipGrepper.Common.GuiSearchParams,
	RipGrepper.Settings.RipGrepParameterSettings,
	RipGrepper.Settings.SearchFormSettings,
	RipGrepper.Settings.Persistable,
	RipGrepper.Settings.TestOwnerSettings, Spring;

type

	[TestFixture]
	TSearchFormSettingsTest = class
		const
			INIFILE = 'DripGrepperUnittest.ini';

		private
			FIniFile : IShared<TMemIniFile>;
			FOwner : TPersistableSettings;
			FSettings : TSearchFormSettings;
			procedure WriteDefaultsToIni();

		public
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
			procedure LoadDefaultsReadsIni;

	end;

implementation

uses
	System.SysUtils,
	System.Classes,
	System.Variants,
	RipGrepper.Common.Constants, RipGrepper.Settings.FilePersister;

procedure TSearchFormSettingsTest.RefreshMembersShouldLoadDefaultsTest;
begin
//  FSettings.LoadDefaultsFromDict;
	Assert.AreEqual(False, FSettings.Pretty, 'Pretty should be true');
	Assert.AreEqual(False, FSettings.Hidden, 'Hidden should be true');
	Assert.AreEqual(False, FSettings.NoIgnore, 'NoIgnore should be true');
	Assert.AreEqual(0, FSettings.Context, 'Context should be true');
	Assert.AreEqual('', FSettings.Encoding, 'Encoding should be ''''');
end;

procedure TSearchFormSettingsTest.LoadDefaultsShouldReadDefaultFromIni;
begin
	WriteDefaultsToIni;
	FSettings.ReadIni;

	Assert.IsTrue(FSettings.IsAlreadyRead, 'IsAlreadyRead should be true');
	Assert.AreEqual('utf8', FSettings.Encoding, 'Encoding should be utf8');
	Assert.AreEqual(5, FSettings.Context, 'Context should be 5');
	Assert.AreEqual(False, FSettings.Pretty, 'Pretty should be false');
	Assert.AreEqual(True, FSettings.Hidden, 'Hidden should be true');
	Assert.AreEqual(True, FSettings.NoIgnore, 'NoIgnore should be true');
end;

procedure TSearchFormSettingsTest.AfterCopyValuesValuesShouldBeEqual;
begin
	WriteDefaultsToIni;
//  FSettings.LoadDefaultsFromDict;
	FSettings.LoadFromDict();

	FSettings.ExtensionSettings.SearchSelectedShortcut := 'CTRL-X';
	FSettings.StoreToDict;

	var
	s := TSearchFormSettings.Create();
	try
		s.Copy(FSettings);
		s.LoadFromDict();
		Assert.AreEqual(s.Encoding, FSettings.Encoding, 'Encoding should be equal');
		Assert.AreEqual(s.Context, FSettings.Context, 'Context should be equal');
		Assert.AreEqual(s.Pretty, FSettings.Pretty, 'Pretty should be equal');
		Assert.AreEqual(s.Hidden, FSettings.Hidden, 'Hidden should be equal');
		Assert.AreEqual(s.NoIgnore, FSettings.NoIgnore, 'NoIgnore should be equal');

		Assert.AreEqual(s.ExtensionSettings.SearchSelectedShortcut, FSettings.ExtensionSettings.SearchSelectedShortcut,
			'SearchSelectedSC should be equal')

	finally
		s.Free;
	end;
end;

procedure TSearchFormSettingsTest.LoadDefaultsReadsIni;
begin
	WriteDefaultsToIni;
	Assert.IsFalse(FSettings.IsAlreadyRead);
	FSettings.ReadIni;
//  FSettings.LoadDefaultsFromDict;
	Assert.IsTrue(FSettings.IsAlreadyRead);
end;

procedure TSearchFormSettingsTest.WriteDefaultsToIni();
begin
	var
	sec := FSettings.IniSectionName;
	FIniFile.WriteString(sec, 'Encoding' {+DEFAULT_KEY}, 'utf8');
	FIniFile.WriteInteger(sec, 'Context' {+DEFAULT_KEY}, 5);
	FIniFile.WriteBool(sec, 'Pretty' {+DEFAULT_KEY}, False);
	FIniFile.WriteBool(sec, 'Hidden' {+DEFAULT_KEY}, True);
	FIniFile.WriteBool(sec, 'NoIgnore' {+DEFAULT_KEY}, True);
end;

procedure TSearchFormSettingsTest.Setup;
begin
	FIniFile := Shared.Make<TMemIniFile>(
		{ } TMemIniFile.Create(INIFILE, TEncoding.UTF8));

	FOwner := TTestOwnerSettings.Create();
	FOwner.PersisterFactory := TIniPersister.Create(FIniFile);

//  FIniFile := FOwner.IniFile();
	FSettings := TSearchFormSettings.Create(FOwner);
end;

procedure TSearchFormSettingsTest.TearDown;
begin
	FSettings.Free;
	FOwner.Free;
end;

initialization

TDUnitX.RegisterTestFixture(TSearchFormSettingsTest);

end.
