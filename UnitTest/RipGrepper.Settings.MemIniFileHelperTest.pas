unit RipGrepper.Settings.MemIniFileHelperTest;

interface

uses
	System.IniFiles,
	DUnitX.TestFramework,
	RipGrepper.Helper.MemIniFile,
	System.Classes;

const
	MEMINIFILEHELPERTEST_INI = 'MemIniFileHelperTest.ini';

type

	TMemIniHack = class(TMemIniFile)
		function GetPrivateFunction : string;
	end;

	[TestFixture]
	TMemIniFileHelperTest = class

		private
			FIniFile : TMemIniFile;
			FSectionValues : TStringList;
			procedure AsserIniReads(_ini : TMemIniFile);
			procedure CreateIniFileWithValues(const sFileName : string);

		public
			constructor Create;
			destructor Destroy; override;
			[Test]
			procedure ReloadIniTest;
			[Test]
			procedure WriteTempSectionIniTest;
			[Setup]
			procedure Setup;
			[TearDown]
			procedure TearDown;
			[Test]
			procedure ReadTempSectionIniTest;

	end;

implementation

uses
	System.SysUtils,
	System.IOUtils,
	System.Rtti;

constructor TMemIniFileHelperTest.Create;
begin
	inherited;
end;

destructor TMemIniFileHelperTest.Destroy;
begin
	inherited;
end;

procedure TMemIniFileHelperTest.AsserIniReads(_ini : TMemIniFile);
begin
	for var i := 1 to 3 do begin
		var
		si := i.ToString;
		Assert.AreEqual('val' + si, FIniFile.ReadString('section2', 'key' + si, ''),
			{ } Format('Inifile should have [section2] key%s=val%s', [si, si]));
	end;
end;

procedure TMemIniFileHelperTest.CreateIniFileWithValues(const sFileName : string);
begin
	var
	ini := TMemIniFile.Create(sFileName, TEncoding.UTF8);
	try
		for var i := 1 to 3 do begin
			for var j := 1 to 3 do begin
				ini.WriteString('section' + i.ToString, 'key' + j.ToString, 'val' + j.ToString);
			end;
		end;
		ini.UpdateFile;
	finally
		ini.Free;
	end;
end;

procedure TMemIniFileHelperTest.ReloadIniTest;
begin
	FIniFile.EraseSection('section2');
	FIniFile.ReloadIniFile;
	AsserIniReads(FIniFile);
end;

procedure TMemIniFileHelperTest.WriteTempSectionIniTest;
var
	ini : TMemIniFile;
begin

	var
	section := 'section2';
	FIniFile.ReadSectionValues(section, FSectionValues);
	FIniFile.WriteTempSectionIni(section, FSectionValues);
	var
	newFile := FIniFile.GetTempSectionFileName(section);
	Assert.IsTrue(TFile.Exists(newFile));

	ini := TMemIniFile.Create(newFile, TEncoding.UTF8);
	try
		for var j := 1 to 3 do begin
			var
			sj := j.ToString;
			Assert.AreEqual('val' + sj, ini.ReadString(section, 'key' + sj, ''));
		end;
		Assert.IsEmpty(ini.ReadString('section1', 'key1', ''), 'section1 should be empty');
		Assert.IsEmpty(ini.ReadString('section3', 'key3', ''), 'section3 should be empty');
	finally
		ini.Free;
		DeleteFile(newFile);
	end;
end;

procedure TMemIniFileHelperTest.Setup;
begin
	CreateIniFileWithValues(MEMINIFILEHELPERTEST_INI);
	FIniFile := TMemIniFile.Create(MEMINIFILEHELPERTEST_INI, TEncoding.UTF8);
	FSectionValues := TStringList.Create();

end;

procedure TMemIniFileHelperTest.TearDown;
begin
	var
	sFileName := FIniFile.FileName;
	FIniFile.Free;
	TFile.Delete(sFileName);
	FSectionValues.Free;
end;

procedure TMemIniFileHelperTest.ReadTempSectionIniTest;
var
	ini : TMemIniFile;
begin

	var
	section := 'section2';
	FIniFile.ReadSectionValues(section, FSectionValues);
	FIniFile.WriteTempSectionIni(section, FSectionValues);
	var
	newFile := FIniFile.GetTempSectionFileName(section);

	ini := TMemIniFile.Create(newFile, TEncoding.UTF8);
	try
		for var j := 0 to 5 do begin
			var
			sj := j.ToString;
			ini.WriteString(section, 'key' + sj, 'new_value' + sj);
		end;
		ini.UpdateFile;

		FIniFile.ReadTempSectionFiles;
//		Assert.IsTrue(not DirectoryExists(FIniFile.GetDripGrepperIniTempDir), ' temp dir should not exists');

		for var j := 0 to 5 do begin
			var
			sj := j.ToString;
			Assert.AreEqual('new_value' + sj, FIniFile.ReadString(section, 'key' + sj, ''));
		end;
	finally
		ini.Free;
		DeleteFile(newFile);
	end;
end;

function TMemIniHack.GetPrivateFunction : string;
begin
	var
		Method : TRttiMethod := TRttiContext.Create.GetType(TMemIniFile).GetMethod('GetTempFileName');
	Result := Method.Invoke(self, []).AsString;
end;

initialization

TDUnitX.RegisterTestFixture(TMemIniFileHelperTest);

end.
