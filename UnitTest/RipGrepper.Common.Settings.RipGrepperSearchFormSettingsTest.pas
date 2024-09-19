unit RipGrepper.Common.Settings.RipGrepperSearchFormSettingsTest;

interface

uses
	DUnitX.TestFramework,
	System.IniFiles,
	RipGrepper.Common.GuiSearchParams,
	RipGrepper.Common.Settings.RipGrepParameterSettings,
	RipGrepper.Common.Settings.RipGrepperSearchFormSettings;

type

	[TestFixture]
	TRipGrepperSearchFormSettingsTest = class
		const
			INIFILE = 'DripGrepperUnittest.ini';

		private
			FIniFile : TMemIniFile;
			FSettings : TRipGrepperSearchFormSettings;
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

constructor TRipGrepperSearchFormSettingsTest.Create;
begin
	inherited;
	// FIniFile := TMemIniFile.Create(INIFILE, TEncoding.UTF8);
end;

destructor TRipGrepperSearchFormSettingsTest.Destroy;
begin
	// FIniFile.Free;
	inherited;
end;

procedure TRipGrepperSearchFormSettingsTest.RefreshMembersShouldLoadDefaultsTest;
begin
	FSettings.RefreshMembers(True);
	Assert.AreEqual(True, FSettings.Pretty, 'Pretty should be true');
	Assert.AreEqual(False, FSettings.Hidden, 'Hidden should be true');
	Assert.AreEqual(False, FSettings.NoIgnore, 'NoIgnore should be true');
	Assert.AreEqual(0, FSettings.Context, 'Context should be true');
	Assert.AreEqual('', FSettings.Encoding, 'Encoding should be ''''');
end;

procedure TRipGrepperSearchFormSettingsTest.LoadDefaultsShouldReadDefaultFromIni;
begin
	SetDefaults;
	FSettings.LoadDefault;
	Assert.IsTrue(FSettings.IsAlreadyRead);
	Assert.AreEqual('utf8', FSettings.Encoding, 'Encoding should be utf8');
	Assert.AreEqual(5, FSettings.Context, 'Context should be 5');
	Assert.AreEqual(False, FSettings.Pretty, 'Pretty should be false');
	Assert.AreEqual(True, FSettings.Hidden, 'Hidden should be true');
	Assert.AreEqual(True, FSettings.NoIgnore, 'NoIgnore should be true');
end;

procedure TRipGrepperSearchFormSettingsTest.AfterCopyValuesValuesShouldBeEqual;
begin
	SetDefaults;
	FSettings.LoadDefault;
	FSettings.RefreshMembers(false);
	var
	s := TRipGrepperSearchFormSettings.Create();
	try
		s.Copy(FSettings);
		s.RefreshMembers(false);
		Assert.AreEqual(s.Encoding, FSettings.Encoding, 'Encoding should be utf8');
		Assert.AreEqual(s.Context, FSettings.Context, 'Context should be 5');
		Assert.AreEqual(s.Pretty, FSettings.Pretty, 'Pretty should be false');
		Assert.AreEqual(s.Hidden, FSettings.Hidden, 'Hidden should be true');
		Assert.AreEqual(s.NoIgnore, FSettings.NoIgnore, 'NoIgnore should be true');
	finally
		s.Free;
	end;
end;

procedure TRipGrepperSearchFormSettingsTest.LoadDefaultsReadsIni;
begin
	SetDefaults;
	Assert.IsFalse(FSettings.IsAlreadyRead);
	FSettings.LoadDefault;
	Assert.IsTrue(FSettings.IsAlreadyRead);
end;

procedure TRipGrepperSearchFormSettingsTest.SetDefaults;
begin
	var
	sec := FSettings.IniSectionName;
	FIniFile.WriteString(sec, 'Encoding' + DEFAULT_KEY, 'utf8');
	FIniFile.WriteInteger(sec, 'Context' + DEFAULT_KEY, 5);
	FIniFile.WriteBool(sec, 'Pretty' + DEFAULT_KEY, False);
	FIniFile.WriteBool(sec, 'Hidden' + DEFAULT_KEY, True);
	FIniFile.WriteBool(sec, 'NoIgnore' + DEFAULT_KEY, True);
end;

procedure TRipGrepperSearchFormSettingsTest.Setup;
begin
	FIniFile := TMemIniFile.Create(INIFILE, TEncoding.UTF8);
	FSettings := TRipGrepperSearchFormSettings.Create(FIniFile);
end;

procedure TRipGrepperSearchFormSettingsTest.TearDown;
begin
	FSettings.Free;
	FIniFile.Free;
end;

initialization

TDUnitX.RegisterTestFixture(TRipGrepperSearchFormSettingsTest);

end.
