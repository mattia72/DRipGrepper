unit RipGrepper.Settings.SearchFormSettingsTest;

interface

uses
	DUnitX.TestFramework,
	System.IniFiles,
	RipGrepper.Common.GuiSearchParams,
	RipGrepper.Settings.RipGrepParameterSettings,
	RipGrepper.Settings.SearchFormSettings;

type

	[TestFixture]
	TSearchFormSettingsTest = class
		const
			INIFILE = 'DripGrepperUnittest.ini';

		private
			FIniFile : TMemIniFile;
			FSettings : TSearchFormSettings;
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
			procedure LoadDefaultsReadsIni;

	end;

implementation

uses
	System.SysUtils,
	System.Classes,
	System.Variants,
	RipGrepper.Common.Constants;

constructor TSearchFormSettingsTest.Create;
begin
	inherited;
	// FIniFile := TMemIniFile.Create(INIFILE, TEncoding.UTF8);
end;

destructor TSearchFormSettingsTest.Destroy;
begin
	// FIniFile.Free;
	inherited;
end;

procedure TSearchFormSettingsTest.RefreshMembersShouldLoadDefaultsTest;
begin
	FSettings.LoadDefaultsFromDict;
	Assert.AreEqual(True, FSettings.Pretty, 'Pretty should be true');
	Assert.AreEqual(False, FSettings.Hidden, 'Hidden should be true');
	Assert.AreEqual(False, FSettings.NoIgnore, 'NoIgnore should be true');
	Assert.AreEqual(0, FSettings.Context, 'Context should be true');
	Assert.AreEqual('', FSettings.Encoding, 'Encoding should be ''''');
end;

procedure TSearchFormSettingsTest.LoadDefaultsShouldReadDefaultFromIni;
begin
	SetDefaults;
	FSettings.ReadIni;
	FSettings.LoadDefaultsFromDict;
	Assert.IsTrue(FSettings.IsAlreadyRead, 'IsAlreadyRead should be true');
	Assert.AreEqual('utf8', FSettings.Encoding, 'Encoding should be utf8');
	Assert.AreEqual(5, FSettings.Context, 'Context should be 5');
	Assert.AreEqual(False, FSettings.Pretty, 'Pretty should be false');
	Assert.AreEqual(True, FSettings.Hidden, 'Hidden should be true');
	Assert.AreEqual(True, FSettings.NoIgnore, 'NoIgnore should be true');
end;

procedure TSearchFormSettingsTest.AfterCopyValuesValuesShouldBeEqual;
begin
	SetDefaults;
	FSettings.LoadDefaultsFromDict;
	FSettings.LoadFromDict();
	var
	s := TSearchFormSettings.Create();
	try
		s.Copy(FSettings);
		s.LoadFromDict();
		Assert.AreEqual(s.Encoding, FSettings.Encoding, 'Encoding should be utf8');
		Assert.AreEqual(s.Context, FSettings.Context, 'Context should be 5');
		Assert.AreEqual(s.Pretty, FSettings.Pretty, 'Pretty should be false');
		Assert.AreEqual(s.Hidden, FSettings.Hidden, 'Hidden should be true');
		Assert.AreEqual(s.NoIgnore, FSettings.NoIgnore, 'NoIgnore should be true');
	finally
		s.Free;
	end;
end;

procedure TSearchFormSettingsTest.LoadDefaultsReadsIni;
begin
	SetDefaults;
	Assert.IsFalse(FSettings.IsAlreadyRead);
    FSettings.ReadIni;
	FSettings.LoadDefaultsFromDict;
	Assert.IsTrue(FSettings.IsAlreadyRead);
end;

procedure TSearchFormSettingsTest.SetDefaults;
begin
	var
	sec := FSettings.IniSectionName;
	FIniFile.WriteString(sec, 'Encoding' + DEFAULT_KEY, 'utf8');
	FIniFile.WriteInteger(sec, 'Context' + DEFAULT_KEY, 5);
	FIniFile.WriteBool(sec, 'Pretty' + DEFAULT_KEY, False);
	FIniFile.WriteBool(sec, 'Hidden' + DEFAULT_KEY, True);
	FIniFile.WriteBool(sec, 'NoIgnore' + DEFAULT_KEY, True);
end;

procedure TSearchFormSettingsTest.Setup;
begin
	FIniFile := TMemIniFile.Create(INIFILE, TEncoding.UTF8);
	FSettings := TSearchFormSettings.Create(FIniFile);
end;

procedure TSearchFormSettingsTest.TearDown;
begin
	FSettings.Free;
	FIniFile.Free;
end;

initialization

TDUnitX.RegisterTestFixture(TSearchFormSettingsTest);

end.
