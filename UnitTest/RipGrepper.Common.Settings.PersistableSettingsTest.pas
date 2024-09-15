unit RipGrepper.Common.Settings.PersistableSettingsTest;

interface

uses
	RipGrepper.Common.Settings.RipGrepperSearchFormSettings,
	System.IniFiles,
	RipGrepper.Common.Settings.Persistable,
	DUnitX.TestFramework, RipGrepper.Common.Settings.SettingsDictionary;

const
	STR_INITIAL_VALUE = 'str_initial_value';
	DEFAULT_STR_VAL = 'default_str_val';

type

	TTestSettings = class(TPersistableSettings)
		private
			FStrSetting : string;

		public
			constructor Create(const _ini : TMemIniFile); overload;
			constructor Create(const _iniSection: string); overload;
			function GetDict: TSettingsDictionary;
			procedure Init; override;
			procedure ReadIni; override;
			procedure RefreshMembers(const _bWithDefault : Boolean); override;
			procedure Store; override;
			procedure StoreAsDefault; override;

			property StrSetting : string read FStrSetting write FStrSetting;
	end;

//	[TestFixture]
	TPersistableSettingsTest = class
		const
			INIFILE = 'PersistableSettingsTest.ini';
			INI_SECTION = 'TestSettings';

		private
			FIniFile : TMemIniFile;
			FSettings : TTestSettings;
			procedure SetDefaults;

		public
			constructor Create;
			destructor Destroy; override;
			[Setup]
			procedure Setup;
			[TearDown]
			procedure TearDown;

			[Test]
			procedure FirstRefreshMembersShouldLoadInitialValue;
			[Test]
			procedure LoadDefaultsShouldReadDefaultFromIni;
			[Test]
			procedure AfterCopyValuesValuesShouldBeEqual;
			[Test]
			procedure LoadDefaultsReadsIni;
			[Test]
			procedure InitCreatesKeysInDict;

	end;

implementation

uses
	System.SysUtils,
	RipGrepper.Common.Constants;

constructor TPersistableSettingsTest.Create;
begin
	inherited;
//	FIniFile := TMemIniFile.Create(INIFILE, TEncoding.UTF8);
end;

destructor TPersistableSettingsTest.Destroy;
begin
//	FIniFile.Free;
	inherited;
end;

procedure TPersistableSettingsTest.FirstRefreshMembersShouldLoadInitialValue;
begin
	FSettings.RefreshMembers(True);
	Assert.AreEqual(STR_INITIAL_VALUE, FSettings.StrSetting);
end;

procedure TPersistableSettingsTest.LoadDefaultsShouldReadDefaultFromIni;
begin
	SetDefaults;
	FSettings.LoadDefault;
	Assert.AreEqual(DEFAULT_STR_VAL, FSettings.StrSetting, 'StrSetting should be default');
end;

procedure TPersistableSettingsTest.AfterCopyValuesValuesShouldBeEqual;
begin
	SetDefaults;
	FSettings.LoadDefault;
	var
	s := TTestSettings.Create(TPersistableSettingsTest.INI_SECTION);
	try
		s.Copy(FSettings);
		s.LoadDefault;
		Assert.AreEqual(s.StrSetting, FSettings.StrSetting, 'StrSetting should be equal');
	finally
		s.Free;
	end;
end;

procedure TPersistableSettingsTest.LoadDefaultsReadsIni;
begin
	SetDefaults;
	Assert.IsFalse(FSettings.IsAlreadyRead);
	FSettings.LoadDefault;
	Assert.IsTrue(FSettings.IsAlreadyRead);
end;

procedure TPersistableSettingsTest.InitCreatesKeysInDict;
begin
	Assert.AreEqual({default_members*2+normal_members}2, FSettings.GetDict.Count);
end;

procedure TPersistableSettingsTest.SetDefaults;
begin
	var
	sec := FSettings.IniSectionName;
	FIniFile.WriteString(sec, 'StrSetting' + DEFAULT_KEY, DEFAULT_STR_VAL);
end;

procedure TPersistableSettingsTest.Setup;
begin
	FIniFile := TMemIniFile.Create(INIFILE, TEncoding.UTF8);
	FSettings := TTestSettings.Create(FIniFile);
end;

procedure TPersistableSettingsTest.TearDown;
begin
	FSettings.Free;
    FIniFile.Free;
end;

constructor TTestSettings.Create(const _ini : TMemIniFile);
begin
    IniSectionName := TPersistableSettingsTest.INI_SECTION; // should be set before create
	inherited Create(_ini);
end;

constructor TTestSettings.Create(const _iniSection: string);
begin
	IniSectionName := _iniSection;  //as in  GUISearchTextParams
	inherited Create();
end;

function TTestSettings.GetDict: TSettingsDictionary;
begin
	Result := FSettingsDict;
end;

procedure TTestSettings.Init;
begin
	CreateDefaultSetting('StrSetting', vtString, STR_INITIAL_VALUE);
end;

procedure TTestSettings.ReadIni;
begin
	inherited
end;

procedure TTestSettings.RefreshMembers(const _bWithDefault : Boolean);
begin
	StrSetting := GetSetting('StrSetting', _bWithDefault);
end;

procedure TTestSettings.Store;
begin
	StoreSetting('StrSetting', StrSetting);
end;

procedure TTestSettings.StoreAsDefault;
begin
	StoreDefaultSetting('StrSetting', StrSetting);
end;

initialization

TDUnitX.RegisterTestFixture(TPersistableSettingsTest);

end.
