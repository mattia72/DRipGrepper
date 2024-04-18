unit RipGrepper.Helper.Types.Test;

interface

uses
	DUnitX.TestFramework,
	System.Classes;

type

	[TestFixture]
	TStringsHelperTest = class
		private
			FStrings : TStrings;
			FStrings2 : TStrings;
			FStrings3 : TStrings;

		public
			[Setup]
			procedure Setup;
			[TearDown]
			procedure TearDown;
			// Sample Methods
			// Simple single Test
			[Test]
			procedure TestGetValues;
			// Test with TestCase Attribute to supply parameters.
			[Test]
			procedure TestDeleteAll;
			// Sample Methods
			// Simple single Test
			[Test]
			procedure TestGetValues1;
	end;

	[TestFixture]
	TBitFieldTest = class

		public
			[Test]
			procedure TestBitSetValue;
			[Test]
			procedure TestBitSetValue1;
			[Test]
			procedure TestBitSetValue2;
	end;

implementation

uses
	RipGrepper.Helper.Types,
	System.StrUtils,
	System.SysUtils,
	System.Math, ArrayEx;

procedure TStringsHelperTest.Setup;
begin
	FStrings := TStringList.Create;
	FStrings2 := TStringList.Create;
	FStrings3 := TStringList.Create;
end;

procedure TStringsHelperTest.TearDown;
begin
	FStrings.Free;
	FStrings2.Free;
	FStrings3.Free;
end;

procedure TStringsHelperTest.TestGetValues;
begin
	var
	values := ['key1_val1', 'key1_val2', 'key2_val3', 'key2_val4', 'key3_val5', 'key3_val6'];

	for var s : string in values do begin
		FStrings.AddPair(s.Remove(s.IndexOf('_')), s.Remove(0, 1 + s.IndexOf('_')));
	end;

	var
	act_vals := FStrings.GetValues();

	for var i := 0 to high(values) do begin
		Assert.IsTrue(values[i].EndsWith(act_vals[i]), Format('%s should end with %s', [values[i], act_vals[i]]));
	end;
end;

procedure TStringsHelperTest.TestDeleteAll;
var
	arr : TArrayEx<string>;
begin
	for var i : integer := 0 to 10 do begin
		FStrings.Add(i.ToString);
	end;

	for var i : integer := 0 to 10 do begin
		 if 0= i mod 2 then begin
			 arr.Add(i.ToString);
         end;
	end;

	FStrings.DeleteAll(arr);

	Assert.AreEqual(-1 , FStrings.IndexOfAny(arr), 'arr shouldn''t contained by the list');

end;

procedure TStringsHelperTest.TestGetValues1;
begin
	var
	values := ['key1_val1', 'key1_val2', 'key2_val3', 'key2_val4', 'key3_val5', 'key3_val6'];

	for var s : string in values do begin
		FStrings.AddPair(s.Remove(s.IndexOf('_')), s.Remove(0, 1 + s.IndexOf('_')));
	end;

	for var j in [1, 2, 3] do begin
		var
		act_vals := FStrings.GetValues('key' + j.ToString);

		for var i := 0 to high(act_vals) do begin
			var
			valIdx := (2 * j) - 2 + i;
			Assert.IsTrue(values[valIdx].EndsWith(act_vals[i]), Format('%s should end with %s', [values[valIdx], act_vals[i]]));
		end;
	end;
end;

procedure TBitFieldTest.TestBitSetValue;
var
	bf : TBitField;
	i : integer;
begin
	Assert.AreEqual(0, bf.Value, '');
	for i := 0 to sizeof(integer) - 1 do begin
		bf.Value := 0;
		bf.SetBit(i);
		Assert.AreEqual(Power(2, i), Single(bf.Value), '');
	end;

end;

procedure TBitFieldTest.TestBitSetValue1;
var
	bf : TBitField;
	i : integer;
begin
	Assert.AreEqual(0, bf.Value, '');
	for i := 0 to sizeof(integer) - 1 do begin
		bf.SetBit(i);
		if i > 0 then
			bf.ResetBit(i - 1);
		Assert.AreEqual(Power(2, i), Single(bf.Value), '');
	end;

end;

procedure TBitFieldTest.TestBitSetValue2;
var
	bf : TBitField;
begin
	Assert.IsTrue(bf.IsEqual([0, 0, 0, 0]), '');
	bf.SetBit(0);
	bf.SetBit(2);
	Assert.IsTrue(bf.IsEqual([1, 0, 1, 0]), '');
	Assert.IsTrue(bf.IsEqual([1, 8, 1, 8]), '');
end;

initialization

TDUnitX.RegisterTestFixture(TStringsHelperTest);

end.
