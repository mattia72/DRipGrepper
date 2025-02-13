unit RipGrepper.Settings.SettingVariantTest;

interface

uses
	DUnitX.TestFramework,
	System.Classes,
	RipGrepper.Settings.SettingVariant;

type

	[TestFixture]
	TSettingVariantTest = class
		public
			[Test]
			procedure TestEquals();
			[Test]
			procedure TestEqualsStr();
			[Test]
			procedure TestWriteToMemIniStr();
			[Test]
			procedure TestWriteToMemIniInt();
			[Test]
			procedure TestWriteToMemIniBool();
			[Test]
			procedure TestWriteToMemIniArray();
	end;

implementation

uses
	System.SysUtils,
	System.IniFiles,
	System.Variants;

procedure TSettingVariantTest.TestEquals();
var
	v, v2 : ISettingVariant;
begin
	v := TSettingVariant.Create(varInteger, 42);
	v2 := TSettingVariant.Create(varInteger, 42);

	Assert.IsTrue(v.Equals(v2), Format('Expected %s should be equal to %s', [v.Value, v2.Value]));

	v2.Value := '42';
	Assert.IsTrue(not v.Equals(v2), Format('Expected %s not to be equal to %s', [v.Value, v2.Value]));
end;

procedure TSettingVariantTest.TestEqualsStr();
var
	v, v2 : ISettingVariant;
begin
	v := TSettingVariant.Create(varString, 'SHIFT + F2');
	v2 := TSettingVariant.Create(varString, 'SHIFT + F2');

	Assert.IsTrue(v.Equals(v2), Format('Expected %s should be equal to %s', [v.Value, v2.Value]));

	v2.Value := 'SHIFT + R';
	Assert.IsTrue(not v.Equals(v2), Format('Expected %s not to be equal to %s', [v.Value, v2.Value]));
end;

procedure TSettingVariantTest.TestWriteToMemIniStr();
var
	v : ISettingVariant;
	IniFile : TMemIniFile;
	Section, Ident, ExpectedValue, ActualValue : string;
begin
	v := TSettingVariant.Create(varString, 'TestValue');
	IniFile := TMemIniFile.Create('');
	try
		Section := 'TestSection';
		Ident := 'TestIdent';
		ExpectedValue := v.Value;

		v.WriteToMemIni(IniFile, Section, Ident);
		ActualValue := IniFile.ReadString(Section, Ident, '');

		Assert.AreEqual(ExpectedValue, ActualValue, Format('Expected %s should be equal to %s', [ExpectedValue, ActualValue]));
	finally
		IniFile.Free;
	end;
end;

procedure TSettingVariantTest.TestWriteToMemIniInt();
var
	v : ISettingVariant;
	IniFile : TMemIniFile;
	Section, Ident, ExpectedValue, ActualValue : string;
begin
	v := TSettingVariant.Create(varInteger, 42);
	IniFile := TMemIniFile.Create('');
	try
		Section := 'TestSection';
		Ident := 'TestIdent';
		ExpectedValue := v.Value;

		v.WriteToMemIni(IniFile, Section, Ident);
		ActualValue := IniFile.ReadString(Section, Ident, '');

		Assert.AreEqual(ExpectedValue, ActualValue, Format('Expected %s should be equal to %s', [ExpectedValue, ActualValue]));
	finally
		IniFile.Free;
	end;
end;

procedure TSettingVariantTest.TestWriteToMemIniBool();
var
	v : ISettingVariant;
	IniFile : TMemIniFile;
	Section, Ident, ExpectedValue, ActualValue : string;
begin
	v := TSettingVariant.Create(varBoolean, True);
	IniFile := TMemIniFile.Create('');
	try
		Section := 'TestSection';
		Ident := 'TestIdent';
		ExpectedValue := v.Value;

		v.WriteToMemIni(IniFile, Section, Ident);
		// ini file stores 0 or 1
		ActualValue := BoolToStr('1' = IniFile.ReadString(Section, Ident, ''), True);

		Assert.AreEqual(ExpectedValue, ActualValue, Format('Expected %s should be equal to %s', [ExpectedValue, ActualValue]));
	finally
		IniFile.Free;
	end;
end;

procedure TSettingVariantTest.TestWriteToMemIniArray();
var
	v : ISettingVariant;
	IniFile : TMemIniFile;
	Section, Ident, ExpectedValue, ActualValue : string;
begin
	var
	varr := VarArrayOf(['item1', 'item2', 'item3']);
	v := TSettingVariant.Create(varr);
	IniFile := TMemIniFile.Create('');
	try
		Section := 'TestSection';
		Ident := 'TestIdent';
		v.WriteToMemIni(IniFile, Section, Ident);

		for var i := 0 to 2 do begin
			ActualValue := IniFile.ReadString(Section, Format('%s_Item%d', [Ident, i]), '');
			ExpectedValue := varr[i];
			Assert.AreEqual(ExpectedValue, ActualValue, Format('Expected %s should be equal to %s', [ExpectedValue, ActualValue]));
		end;

	finally
		IniFile.Free;
	end;
end;

initialization

TDUnitX.RegisterTestFixture(TSettingVariantTest);

end.
