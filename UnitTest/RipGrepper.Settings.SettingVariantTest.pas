unit RipGrepper.Settings.SettingVariantTest;

interface

uses
	DUnitX.TestFramework,
	System.Classes,
	RipGrepper.Settings.SettingVariant,
	System.IniFiles,
	Spring;

type

	[TestFixture]
	TSettingVariantTest = class
		private
			IniFile : IShared<TMemIniFile>;

		public
			[Setup]
			procedure Setup();
			[TearDown]
			procedure TearDown();
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
			procedure TestStatus();
			[Test]
			procedure TestWriteToMemIniArrayReversed();
	end;

implementation

uses
	System.SysUtils,
	System.Variants,
	RipGrepper.Settings.FilePersister,
	ArrayEx;

{$WARN CONSTRUCTING_ABSTRACT OFF}

procedure TSettingVariantTest.Setup();
begin
	IniFile := Shared.Make<TMemIniFile>(TMemIniFile.Create(''));
end;

procedure TSettingVariantTest.TearDown();
begin
	IniFile := nil;
end;

procedure TSettingVariantTest.TestEquals();
var
	v, v2 : ISettingVariant<integer>;
begin
	v := TSettingVariant<integer>.Create('v', 42);
	v2 := TSettingVariant<integer>.Create('v2', 42);

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
	v := TSettingVariant<string>.Create('v', 'SHIFT + F2');
	v2 := TSettingVariant<string>.Create('v2', 'SHIFT + F2');

	Assert.IsTrue(v.Equals(v2), Format('Expected %s should be equal to %s', [v.Value, v2.Value]));

	v2.Value := 'SHIFT + R';
	Assert.IsTrue(not v.Equals(v2), Format('Expected %s not to be equal to %s', [v.Value, v2.Value]));
	v2.Value := 'SHIFT + F2';
	Assert.IsTrue(not v.Equals(v2), Format('Expected %s not to be equal to %s', [v.Value, v2.Value]));
end;

procedure TSettingVariantTest.TestWriteToMemIniStr();
var
	v : ISettingVariant<string>;
	Section, Ident, ExpectedValue, ActualValue : string;
begin
	v := TSettingVariant<string>.Create('TestValue');

	Section := 'TestSection';
	Ident := 'TestIdent';
	v.Persister := TMemIniStringPersister.Create(IniFile, Section, Ident);

	v.StoreToPersister();
	ActualValue := IniFile.ReadString(Section, Ident, '');
	ExpectedValue := '';

	Assert.AreEqual(ExpectedValue, ActualValue, Format('Initialized is not stored. Expected %s should be equal to %s',
		[ExpectedValue, ActualValue]));

	v.Value := 'ChangedValue';
	v.StoreToPersister();
	ActualValue := IniFile.ReadString(Section, Ident, '');
	ExpectedValue := v.Value;

	Assert.AreEqual(ExpectedValue, ActualValue, Format('Modified should stored. Expected %s should be equal to %s',
		[ExpectedValue, ActualValue]));

end;

procedure TSettingVariantTest.TestWriteToMemIniInt();
var
	v : ISettingVariant<integer>;

	Section, Ident : string;
	ExpectedValue, ActualValue : integer;
begin
	v := TIntegerSetting.Create('v', 42);

	Section := 'TestSection';
	Ident := 'TestIdent';
	v.Persister := TMemIniIntegerPersister.Create(IniFile, Section, Ident);

	v.StoreToPersister();
	ActualValue := IniFile.ReadInteger(Section, Ident, -1);
	ExpectedValue := -1;

	Assert.AreEqual(ExpectedValue, ActualValue, Format('Initialized is not stored. Expected %d should be equal to %d',
		[ExpectedValue, ActualValue]));

	v.Value := 43;
	v.StoreToPersister();
	ActualValue := IniFile.ReadInteger(Section, Ident, -1);
	ExpectedValue := 43;

	Assert.AreEqual(ExpectedValue, ActualValue, Format('Modified should be stored. Expected %d should be equal to %d',
		[ExpectedValue, ActualValue]));

end;

procedure TSettingVariantTest.TestWriteToMemIniBool();
var
	v : ISettingVariant<boolean>;
	Section, Ident : string;
	ExpectedValue, ActualValue : Boolean;
begin
	v := TBoolSetting.Create('v', True);
	Section := 'TestSection';
	Ident := 'TestIdent';
	v.Persister := TMemIniBoolPersister.Create(IniFile, Section, Ident);

	v.StoreToPersister();
	// ini file stores 0 or 1
	ActualValue := IniFile.ReadBool(Section, Ident, False);
	ExpectedValue := v.Value;

	Assert.AreNotEqual(ExpectedValue, ActualValue, Format('Initialized is not stored. Expected %s should be equal to %s',
		[BoolToStr(ExpectedValue), BoolToStr(ActualValue)]));

	v.Value := False;
	v.Value := True;
	v.StoreToPersister();
	// ini file stores 0 or 1
	ActualValue := IniFile.ReadBool(Section, Ident, False);
	ExpectedValue := v.Value;

	Assert.AreEqual(ExpectedValue, ActualValue, Format('Expected %s should be equal to %s',
		[BoolToStr(ExpectedValue), BoolToStr(ActualValue)]));

end;

procedure TSettingVariantTest.TestWriteToMemIniArray();
var
	v : ISettingVariant<TArrayEx<string>>;
	Section : string;
	ExpectedValue, ActualValue : string;
begin
	var
	arr := ['item1', 'item2', 'item3'];
	var
	varr := TArrayEx<string>.Create(arr);
	v := TArraySetting.Create('varr', varr);
	Section := 'TestSection';

	v.Persister := TMemIniStrArrayPersister.Create(IniFile, Section);
	v.StoreToPersister;

	for var i := 0 to 2 do begin
		ActualValue := IniFile.ReadString(Section, Format('Item_%d', [i]), '');
		ExpectedValue := varr[i];
		Assert.AreNotEqual(ExpectedValue, ActualValue, Format('Initialized not saved. Expected %s should be equal to %s',
			[ExpectedValue, ActualValue]));
	end;

	varr := ['1', '2', '3'];
	v.Value := varr;
	v.StoreToPersister;

	for var i := 0 to 2 do begin
		ActualValue := IniFile.ReadString(Section, Format('Item_%d', [i]), '');
		ExpectedValue := varr[i];
		Assert.AreEqual(ExpectedValue, ActualValue, Format('Modified should stored. Expected %s should be equal to %s',
			[ExpectedValue, ActualValue]));
	end;

end;

procedure TSettingVariantTest.TestCopy();
var
	v, v2 : ISettingVariant<integer>;
begin
	v := TSettingVariant<integer>.Create('v', 42);
	v2 := TSettingVariant<integer>.Create('v2', 0);

	v2.Copy(v);
	Assert.AreEqual(v.Value, v2.Value, 'Values should be equal after copy');
end;

procedure TSettingVariantTest.TestCompareTo();
var
	v, v2 : ISettingVariant<integer>;
begin
	v := TSettingVariant<integer>.Create('v', 42);
	v2 := TSettingVariant<integer>.Create('v2', 42);

	Assert.AreEqual(0, v.CompareTo(v2), 'Expected values to be equal');

	v2.Value := 41;
	Assert.AreNotEqual(0, v.CompareTo(v2), 'Expected values to be different');
end;

procedure TSettingVariantTest.TestCopySettingValue;
var
	v, v2 : ISetting;
begin
	v := TIntegerSetting.Create('v', 42);
	v2 := TIntegerSetting.Create('v2', 0);

	v2.Copy(v);
	Assert.AreNotEqual(v.AsString, v2.AsString, 'is not equal after copy :( ');

	TSetting.CopySettingFields(v, v2);
	Assert.AreEqual(v.AsString, v2.AsString, 'Values should be equal after copy :)');
end;

procedure TSettingVariantTest.TestCopyIntegerSetting;
var
	v, v2 : IIntegerSetting;
begin
	v := TIntegerSetting.Create('v', 42);
	v2 := TIntegerSetting.Create('v2', 0);

	v2.Copy(v);
	Assert.AreEqual(v.Value, v2.Value, 'Values should be equal after copy');
	Assert.IsTrue(v.Equals(v2), 'Settings should be equal after copy');
end;

procedure TSettingVariantTest.TestCopyBoolSetting;
var
	v, v2 : IBoolSetting;
begin
	v := TBoolSetting.Create('v', True);
	v2 := TBoolSetting.Create('v2', False);

	v2.Copy(v);
	Assert.AreEqual(True, v2.Value, 'Value should be True');
	Assert.AreEqual(v.Value, v2.Value, 'Values should be equal after copy');
	Assert.IsTrue(v.Equals(v2), 'Settings should be equal after copy');
end;

procedure TSettingVariantTest.TestCopyStringSetting;
var
	v, v2 : IStringSetting;
begin
	v := TStringSetting.Create('v', 'string value');
	v2 := TStringSetting.Create('v2', 'other string value');

	v2.Copy(v);
	Assert.AreEqual(v.Value, v2.Value, 'Values should be equal after copy');
	Assert.IsTrue(v.Equals(v2), 'Settings should be equal after copy');
end;

procedure TSettingVariantTest.TestCopyStrArraySetting;
var
	v, v2 : IArraySetting;
begin
	var
	a := ['one', 'two', 'three'];
	v := TArraySetting.Create('v', a);
	v2 := TArraySetting.Create('v2');

	v2.Copy(v);
	Assert.AreEqual(v.Value, v2.Value, 'Values should be equal after copy');
	Assert.IsTrue(v.Equals(v2), 'Settings should be equal after copy');
end;

procedure TSettingVariantTest.TestStatus();
var
	v : ISettingVariant<integer>;
begin
	v := TSettingVariant<integer>.Create('v', 0);
	Assert.IsTrue(v.State = ssInitialized, 'Expected setting state ssInitialized');

	v.Value := 42;
	Assert.IsTrue(v.State = ssModified, 'Expected setting state ssModified');

	v.Persister := TMemIniIntegerPersister.Create(IniFile, 'TestSection', 'TestKey');
	v.StoreToPersister();
	Assert.IsTrue(v.State = ssStored, 'Expected setting state ssStored');
end;

procedure TSettingVariantTest.TestWriteToMemIniArrayReversed();
var
	v : ISettingVariant<TArrayEx<string>>;
	Section : string;
	ExpectedValue, ActualValue : string;
begin
	var
	arr := ['item1', 'item2', 'item3'];
	var
	varr := TArrayEx<string>.Create(arr);
	v := TArraySetting.Create('varr', varr);
	Section := 'TestSection';

	v.Persister := TMemIniStrArrayPersister.Create(IniFile, Section);
	v.StoreToPersister;

	for var i := 0 to 2 do begin
		ActualValue := IniFile.ReadString(Section, Format('Item_%d', [i]), '');
		ExpectedValue := varr[i];
		Assert.AreNotEqual(ExpectedValue, ActualValue, Format('Initialized not saved. Expected %s should be equal to %s',
			[ExpectedValue, ActualValue]));
	end;

	varr := ['1', '2', '3'];
	v.Value := varr;
	v.StoreToPersister;

	for var i := 0 to 2 do begin
		ActualValue := IniFile.ReadString(Section, Format('Item_%d', [i]), '');
		ExpectedValue := varr[i];
		Assert.AreEqual(ExpectedValue, ActualValue, Format('Modified should stored. Expected %s should be equal to %s',
			[ExpectedValue, ActualValue]));
	end;

end;
{$WARN CONSTRUCTING_ABSTRACT ON}

initialization

TDUnitX.RegisterTestFixture(TSettingVariantTest);

end.
