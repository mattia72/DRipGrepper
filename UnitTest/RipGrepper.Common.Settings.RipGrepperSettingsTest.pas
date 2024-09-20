unit RipGrepper.Common.Settings.RipGrepperSettingsTest;

interface

uses
	RipGrepper.Common.Settings.RipGrepperSearchFormSettings,
	System.IniFiles,
	RipGrepper.Common.Settings.RipGrepperSettings,
	DUnitX.TestFramework;

type

	[TestFixture]
	TRipGrepperSettingsTest = class
		const
			INIFILE = 'DripGrepperUnittest.ini';

		private
			FIniFile : TMemIniFile;
			FSettings : TRipGrepperSettings;
			procedure EmptyFile(const _filePath: string);
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
	end;

implementation

uses
	RipGrepper.Common.Constants,
	System.SysUtils,
	Vcl.Forms;

constructor TRipGrepperSettingsTest.Create;
begin
	inherited;
	// FIniFile := TMemIniFile.Create(INIFILE, TEncoding.UTF8);
end;

destructor TRipGrepperSettingsTest.Destroy;
begin
	// FIniFile.Free;
	inherited;
end;

procedure TRipGrepperSettingsTest.RefreshMembersShouldLoadDefaultsTest;
begin
	FSettings.RefreshMembers(True);
	Assert.AreEqual(True, FSettings.RipGrepperSearchFormSettings.Pretty, 'Pretty should be true');
	Assert.AreEqual(False, FSettings.RipGrepperSearchFormSettings.Hidden, 'Hidden should be true');
	Assert.AreEqual(False, FSettings.RipGrepperSearchFormSettings.NoIgnore, 'NoIgnore should be true');
	Assert.AreEqual(0, FSettings.RipGrepperSearchFormSettings.Context, 'Context should be true');
	Assert.AreEqual('', FSettings.RipGrepperSearchFormSettings.Encoding, 'Encoding should be ''''');
end;

procedure TRipGrepperSettingsTest.LoadDefaultsShouldReadDefaultFromIni;
begin
	SetDefaults;
	FSettings.LoadDefault;
	// Assert.IsTrue(FSettings.IsAlreadyRead, 'IsAlreadyRead should read');
	Assert.AreEqual('utf8', FSettings.RipGrepperSearchFormSettings.Encoding, 'Encoding should be utf8');
	Assert.AreEqual(5, FSettings.RipGrepperSearchFormSettings.Context, 'Context should be 5');
	Assert.AreEqual(False, FSettings.RipGrepperSearchFormSettings.Pretty, 'Pretty should be false');
	Assert.AreEqual(True, FSettings.RipGrepperSearchFormSettings.Hidden, 'Hidden should be true');
	Assert.AreEqual(True, FSettings.RipGrepperSearchFormSettings.NoIgnore, 'NoIgnore should be true');
end;

procedure TRipGrepperSettingsTest.AfterCopyValuesValuesShouldBeEqual;
begin
	SetDefaults;
	FSettings.LoadDefault;
	var
	s := TRipGrepperSettings.Create();
	try
		s.Copy(FSettings);

		Assert.AreEqual(s.RipGrepperSearchFormSettings.Encoding,
			{ } FSettings.RipGrepperSearchFormSettings.Encoding, 'Encoding should be utf8');
		Assert.AreEqual(s.RipGrepperSearchFormSettings.Context,
			{ } FSettings.RipGrepperSearchFormSettings.Context, 'Context should be 5');
		Assert.AreEqual(s.RipGrepperSearchFormSettings.Pretty,
			{ } FSettings.RipGrepperSearchFormSettings.Pretty, 'Pretty should be false');
		Assert.AreEqual(s.RipGrepperSearchFormSettings.Hidden,
			{ } FSettings.RipGrepperSearchFormSettings.Hidden, 'Hidden should be true');
		Assert.AreEqual(s.RipGrepperSearchFormSettings.NoIgnore,
			{ } FSettings.RipGrepperSearchFormSettings.NoIgnore, 'NoIgnore should be true');
	finally
		s.Free;
	end;
end;

procedure TRipGrepperSettingsTest.SetDefaults;
begin
	FSettings.RipGrepperSearchFormSettings.Encoding := 'utf8';
	FSettings.RipGrepperSearchFormSettings.Context := 5;
	FSettings.RipGrepperSearchFormSettings.Pretty := False;
	FSettings.RipGrepperSearchFormSettings.Hidden := True;
	FSettings.RipGrepperSearchFormSettings.NoIgnore := True;
	FSettings.StoreAsDefault;
	FSettings.RipGrepperSearchFormSettings.Encoding := 'none';
	FSettings.RipGrepperSearchFormSettings.Context := 1;
	FSettings.RipGrepperSearchFormSettings.Pretty := True;
	FSettings.RipGrepperSearchFormSettings.Hidden := True;
	FSettings.RipGrepperSearchFormSettings.NoIgnore := True;

end;

procedure TRipGrepperSettingsTest.Setup;
begin
	// FIniFile := TMemIniFile.Create(INIFILE, TEncoding.UTF8);
	FSettings := TRipGrepperSettings.Create();
end;

procedure TRipGrepperSettingsTest.TearDown;
begin
	FSettings.Free;
	FIniFile.Free;
	EmptyFile(Application.ExeName + '.ini');
end;

procedure TRipGrepperSettingsTest.EmptyFile(const _filePath: string);
var
	txtFile : TextFile;
begin
	AssignFile(txtFile, _filePath);
	Rewrite(txtFile);
	CloseFile(txtFile);
end;

end.
