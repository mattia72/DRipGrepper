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
			[Test]
			procedure TestCopy();
			[Test]
			procedure TestCompareTo();
			[Test]
			procedure TestCopySettingValue;
			[Test]
			procedure TestCopyIntegerSetting;
			[Test]
			procedure TestCopyBoolSetting;
			[Test]
			procedure TestCopyStringSetting;
			[Test]
			procedure TestCopyStrArraySetting;
			[Test]
			procedure TestIsEmpty();
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
		v.Persister := TMemIniStringPersister.Create(IniFile, Section, Ident);

		v.StoreToPersister();
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

		v.StoreToPersister();
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

		v.StoreToPersister();
		// ini file stores 0 or 1
		ActualValue := IniFile.ReadBool(Section, Ident, False);

		Assert.AreEqual(ExpectedValue, ActualValue, Format('Expected %s should be equal to %s',
			[BoolToStr(ExpectedValue), BoolToStr(ActualValue)]));
	finally
		IniFile.Free;
	end;
end;

procedure TSettingVariantTest.TestWriteToMemIniArray();
var
	v : ISettingVariant<TArrayEx<string>>;
	IniFile : TMemIniFile;
	Section : string;
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

		v.Persister := TMemIniStrArrayPersister.Create(IniFile, Section);
		v.StoreToPersister;

		for var i := 0 to 2 do begin
			ActualValue := IniFile.ReadString(Section, Format('Item_%d', [i]), '');
			ExpectedValue := varr[i];
			Assert.AreEqual(ExpectedValue, ActualValue, Format('Expected %s should be equal to %s', [ExpectedValue, ActualValue]));
		end;

	finally
		IniFile.Free;
	end;
end;

procedure TSettingVariantTest.TestCopy();
var
	v, v2 : ISettingVariant<integer>;
begin
	v := TSettingVariant<integer>.Create(42);
	v2 := TSettingVariant<integer>.Create(0);

	v2.Copy(v);
	Assert.AreEqual(v.Value, v2.Value, 'Values should be equal after copy');
end;

procedure TSettingVariantTest.TestCompareTo();
var
	v, v2 : ISettingVariant<integer>;
begin
	v := TSettingVariant<integer>.Create(42);
	v2 := TSettingVariant<integer>.Create(42);

	Assert.AreEqual(0, v.CompareTo(v2), 'Expected values to be equal');

	v2.Value := 41;
	Assert.AreNotEqual(0, v.CompareTo(v2), 'Expected values to be different');
end;

procedure TSettingVariantTest.TestCopySettingValue;
var
	v, v2 : ISetting;
begin
	v := TIntegerSetting.Create(42);
	v2 := TIntegerSetting.Create(0);

	v2.Copy(v);
	Assert.AreNotEqual(v.AsString, v2.AsString, 'is not equal after copy :( ');

	TSetting.CopySettingValues(v, v2);
	Assert.AreEqual(v.AsString, v2.AsString, 'Values should be equal after copy :)');
end;

procedure TSettingVariantTest.TestCopyIntegerSetting;
var
	v, v2 : IIntegerSetting;
begin
	v := TIntegerSetting.Create(42);
	v2 := TIntegerSetting.Create(0);

	v2.Copy(v);
	Assert.AreEqual(v.Value, v2.Value, 'Values should be equal after copy');
	Assert.IsTrue(v.Equals(v2), 'Settings should be equal after copy');
end;

procedure TSettingVariantTest.TestCopyBoolSetting;
var
	v, v2 : IBoolSetting;
begin
	v := TBoolSetting.Create(True);
	v2 := TBoolSetting.Create(False);

	v2.Copy(v);
	Assert.AreEqual(True, v2.Value, 'Value should be True');
	Assert.AreEqual(v.Value, v2.Value, 'Values should be equal after copy');
	Assert.IsTrue(v.Equals(v2), 'Settings should be equal after copy');
end;

procedure TSettingVariantTest.TestCopyStringSetting;
var
	v, v2 : IStringSetting;
begin
	v := TStringSetting.Create('string value');
	v2 := TStringSetting.Create('other string value');

	v2.Copy(v);
	Assert.AreEqual(v.Value, v2.Value, 'Values should be equal after copy');
	Assert.IsTrue(v.Equals(v2), 'Settings should be equal after copy');
end;

procedure TSettingVariantTest.TestCopyStrArraySetting;
var
	v, v2 : IArraySetting;
begin
	var a := ['one', 'two', 'three'];
	v := TArraySetting.Create(a);
	v2 := TArraySetting.Create();

	v2.Copy(v);
	Assert.AreEqual(v.Value, v2.Value, 'Values should be equal after copy');
	Assert.IsTrue(v.Equals(v2), 'Settings should be equal after copy');
end;

procedure TSettingVariantTest.TestIsEmpty();
var
	v : ISettingVariant<integer>;
begin
	v := TSettingVariant<integer>.Create(0);
	Assert.IsTrue(v.IsEmpty, 'Expected setting to be empty');

	v.Value := 42;
	Assert.IsFalse(v.IsEmpty, 'Expected setting to be not empty');
end;

initialization

TDUnitX.RegisterTestFixture(TSettingVariantTest);

end.
