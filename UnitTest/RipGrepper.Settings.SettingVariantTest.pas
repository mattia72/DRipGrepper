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
	System.Variants,
	RipGrepper.Settings.FilePersister,
	ArrayEx;

procedure TSettingVariantTest.TestEquals();
var
	v, v2 : ISettingVariant<integer>;
begin
	v := TSettingVariant<integer>.Create(42);
	v2 := TSettingVariant<integer>.Create(42);

	Assert.IsTrue(v.Equals(v2), Format('Expected %d should be equal to %d', [v.Value, v2.Value]));

	v2.Value := 41;
	Assert.IsTrue(not v.Equals(v2), Format('Expected %d not to be equal to %d', [v.Value, v2.Value]));
	v2.Value := 42;
	Assert.IsTrue(not v.Equals(v2), Format('Expected %d not to be equal to %d', [v.Value, v2.Value]));
end;

procedure TSettingVariantTest.TestEqualsStr();
var
	v, v2 : ISettingVariant<string>;
begin
	v := TSettingVariant<string>.Create('SHIFT + F2');
	v2 := TSettingVariant<string>.Create('SHIFT + F2');

	Assert.IsTrue(v.Equals(v2), Format('Expected %s should be equal to %s', [v.Value, v2.Value]));

	v2.Value := 'SHIFT + R';
	Assert.IsTrue(not v.Equals(v2), Format('Expected %s not to be equal to %s', [v.Value, v2.Value]));
	v2.Value := 'SHIFT + F2';
	Assert.IsTrue(not v.Equals(v2), Format('Expected %s not to be equal to %s', [v.Value, v2.Value]));
end;

procedure TSettingVariantTest.TestWriteToMemIniStr();
var
	v : ISettingVariant<string>;
	IniFile : TMemIniFile;
	Section, Ident, ExpectedValue, ActualValue : string;
begin
	v := TSettingVariant<string>.Create('TestValue');
	IniFile := TMemIniFile.Create('');
	try
		Section := 'TestSection';
		Ident := 'TestIdent';
		ExpectedValue := v.Value;
		v.Persister:= TMemIniStringPersister.Create(IniFile, Section, Ident);

		v.SaveToFile();
		ActualValue := IniFile.ReadString(Section, Ident, '');

		Assert.AreEqual(ExpectedValue, ActualValue, Format('Expected %s should be equal to %s', [ExpectedValue, ActualValue]));
	finally
		IniFile.Free;
	end;
end;

procedure TSettingVariantTest.TestWriteToMemIniInt();
var
	v : ISettingVariant<integer>;
	IniFile : TMemIniFile;
	Section, Ident : string;
	ExpectedValue, ActualValue : integer;
begin
	v := TIntegerSetting.Create(42);
	IniFile := TMemIniFile.Create('');
	try
		Section := 'TestSection';
		Ident := 'TestIdent';
		ExpectedValue := v.Value;
		v.Persister := TMemIniIntegerPersister.Create(IniFile, Section, Ident);

		v.SaveToFile();
		ActualValue := IniFile.ReadInteger(Section, Ident, -1);

		Assert.AreEqual(ExpectedValue, ActualValue, Format('Expected %d should be equal to %d', [ExpectedValue, ActualValue]));
	finally
		IniFile.Free;
	end;
end;

procedure TSettingVariantTest.TestWriteToMemIniBool();
var
	v : ISettingVariant<boolean>;
	IniFile : TMemIniFile;
	Section, Ident : string;
	ExpectedValue, ActualValue : Boolean;
begin
	v := TBoolSetting.Create(True);
	IniFile := TMemIniFile.Create('');
	try
		Section := 'TestSection';
		Ident := 'TestIdent';
		ExpectedValue := v.Value;
		v.Persister := TMemIniBoolPersister.Create(IniFile, Section, Ident);

		v.SaveToFile();
		// ini file stores 0 or 1
		ActualValue := IniFile.ReadBool(Section, Ident, False);

		Assert.AreEqual(ExpectedValue, ActualValue,
        Format('Expected %s should be equal to %s', [BoolToStr(ExpectedValue), BoolToStr(ActualValue)]));
	finally
		IniFile.Free;
	end;
end;

procedure TSettingVariantTest.TestWriteToMemIniArray();
var
	v : ISettingVariant<TArrayEx<string>>;
	IniFile : TMemIniFile;
	Section, Ident : string;
	ExpectedValue, ActualValue : string;
begin
	var
	arr := ['item1', 'item2', 'item3'];
	var
	varr := TArrayEx<string>.Create(arr);
	v := TArraySetting.Create(varr);
	IniFile := TMemIniFile.Create('');
	try
		Section := 'TestSection';
		Ident := 'TestIdent';
		v.Persister := TMemIniStrArrayPersister.Create(IniFile, Section, Ident);

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
