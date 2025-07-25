unit RipGrepper.Helper.Types.Test;

interface

uses
	DUnitX.TestFramework,
	System.Classes,
	RipGrepper.Common.Constants;

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
			// Test with TestCase Attribute to supply parameters.
			[Test]
			procedure TestSliceMaxLength;
			// Sample Methods
			// Simple single Test
			[Test]
			procedure TestGetValues1;
			// Sample Methods
			// Simple single Test
			[Test]
			procedure TestIndexOfValue;
			// Test with TestCase Attribute to supply parameters.
			[Test]
			procedure TestSlice;
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

	[TestFixture]
	TFileUtilsTest = class

		public
			[Test]
			procedure DeleteDirTest;
	end;

	[TestFixture]
	TMultiLineStringTest = class

		public
			[Test]
			procedure TestIsMultiLine;
			[Test]
			procedure TestGetLine;
			[Test]
			procedure TestGetLineWithEqualsChar;
			[Test]
			procedure TestGetLineWithSpecialChars;
			[Test]
			procedure TestIsMultiLineWithDifferentLineEndings;
			[Test]
			[TestCase('empty', ',0')]
			[TestCase('CRLF only', CRLF + ',2')]
			[TestCase('one line', 'one line,1')]
			[TestCase('two line', 'one line' + CRLF + 'two line,2')]
			[TestCase('line with equals', 'key=value,1')]
			[TestCase('multiline with equals', 'key=value' + CRLF + 'another=line,2')]
			procedure TestGetLineCount(const _str : string; const _count : Integer);
	end;

type

	[TestFixture]
	TStringsHelperMoreTest = class
		private
			FStrings : TStrings;

		public
			[Setup]
			procedure Setup;
			[TearDown]
			procedure TearDown;

			[Test]
			procedure TestContains;
			[Test]
			procedure TestAddIfNotContains;
			[Test]
			procedure TestHasMatch;
			[Test]
			procedure TestContainsAny;
			[Test]
			procedure TestDeleteAllMatched;
			[Test]
			procedure TestIndexOfAllMatch;
			[Test]
			procedure TestIndexOfFirstMatch;
			[Test]
			procedure TestTryGetDef;
	end;

type

	[TestFixture]
	TLoadHistoryModesTest = class
		public
			[Test]
			procedure TestModeToIntAndIntToMode;
			[Test]
			procedure TestIsModeActive();

			[Test]
			procedure TestModeToStringAndStringToMode;
	end;

type

	[TestFixture]
	TConversionsTest = class
		type
			TTestEnum = (teOne, teTwo, teThree);

		public
			[Test]
			procedure TestStringToEnumeration;
			[Test]
			procedure TestEnumerationToString;
	end;

type

	[TestFixture]
	TAutoDeleteTempFileTest = class
		public
			[Test]
			procedure TestFilePathAndFinalize;
	end;

implementation

uses
	RipGrepper.Helper.Types,
	System.StrUtils,
	System.SysUtils,
	System.Math,
	ArrayEx,
	System.IOUtils,
	RipGrepper.Tools.FileUtils,
	RipGrepper.Common.SimpleTypes,
	RipGrepper.Common.LoadHistoryMode,
	Spring;

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
		if 0 = i mod 2 then begin
			arr.Add(i.ToString);
		end;
	end;

	FStrings.DeleteAll(arr);
	Assert.AreEqual(-1, FStrings.IndexOfAny(arr), 'arr shouldn''t contained by the list');
end;

procedure TStringsHelperTest.TestSliceMaxLength;
const
	MAXLEN = 10;
var
	arr : TStringsArrayEx;
	sum : TArrayEx<string>;
begin
	for var i : integer := 0 to 100 do begin
		FStrings.Add(i.ToString);
	end;

	arr := FStrings.SliceMaxLength(MAXLEN);

	for var a in arr do begin
		Assert.IsTrue(MAXLEN >= string.Join(' ', a).Length, 'arr join length should be less then MAXLEN');
		sum.AddRange(a);
	end;

	Assert.IsTrue(sum.Compare(FStrings.ToStringArray), 'sum of slices should be equal to origin');
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

procedure TStringsHelperTest.TestIndexOfValue;
begin
	var
	values := ['key1_val1', 'key1_val2', 'key2_val3', 'key2_val4', 'key3_val5', 'key3_val6'];

	for var s : string in values do begin
		FStrings.AddPair(s.Remove(s.IndexOf('_')), s.Remove(0, 1 + s.IndexOf('_')));
	end;

	var
	act_vals := FStrings.GetValues();

	for var i : integer := 0  to high(values) do begin
		Assert.AreEqual(i, FStrings.IndexOfValue(act_vals[i]), Format('index of %s should be %d', [act_vals[i], i]));
	end;
end;

procedure TStringsHelperTest.TestSlice;
const
	SLICE_IDX = 10;
var
	arr : TStringsArrayEx;
	sum : TArrayEx<string>;
begin
	for var i : integer := 0 to 100 do begin
		FStrings.Add(i.ToString);
	end;

	arr := FStrings.Slice(SLICE_IDX);

	var
		ae : TArrayEx<string> := arr[0];
	Assert.AreEqual(SLICE_IDX, ae.MaxIndex, 'arr MaxIndex should be SLICE_IDX');

	for var a in arr do begin
		sum.AddRange(a);
	end;

	Assert.IsTrue(sum.Compare(FStrings.ToStringArray), 'sum of slices should be equal to origin');
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

procedure TMultiLineStringTest.TestIsMultiLine;
begin
	var
		str : TMultiLineString := 'one liner';
	Assert.IsFalse(str.IsMultiLine(), '"' + str + '" should not be multiline');

	str := 'one line' + CRLF + 'two line';
	Assert.IsTrue(str.IsMultiLine(), '"' + str + '" should be multiline');
end;

procedure TMultiLineStringTest.TestGetLine;
var
	str : TMultiLineString;
begin
	str := '';
	Assert.AreEqual(str.GetLine(0), '', '"' + str + '" first line should equal empty string');

	str := CRLF;
	Assert.AreEqual(str.GetLine(0), '', '"' + str + '" first line should equal empty string');
	Assert.AreEqual(str.GetLine(1), '', '"' + str + '" second line should equal empty string');

	str := 'one liner';
	Assert.AreEqual(str.GetLine(0), string(str), '"' + str + '" first line should equal itself');

	str := 'one line' + CRLF + 'two line';
	Assert.AreEqual(str.GetLine(0), 'one line', '"' + str + '" first line should equal first line');
	Assert.AreEqual(str.GetLine(1), 'two line', '"' + str + '" second line should equal second line');
	Assert.AreEqual(str.GetLine(2), '', '"' + str + '" second line should equal empty string');

end;

procedure TMultiLineStringTest.TestGetLineCount(const _str : string; const _count : Integer);
var
	str : TMultiLineString;
begin
	str := _str;
	Assert.AreEqual(str.GetLineCount(), _count, '"' + str + '" line count should be ' + _count.ToString);
end;

procedure TMultiLineStringTest.TestGetLineWithEqualsChar;
var
	str : TMultiLineString;
begin
	// Test single line with equals - this is the main bug from GitHub issue #15
	str := 'key=value';
	Assert.AreEqual('key=value', str.GetLine(0), 'Single line with equals should return the full string');
	Assert.IsFalse(str.IsMultiLine(), 'Single line with equals should not be detected as multiline');
	Assert.AreEqual(1, str.GetLineCount(), 'Single line with equals should have count 1');
	
	// Test multiline with equals
	str := 'key=value' + CRLF + 'another=line';
	Assert.AreEqual('key=value', str.GetLine(0), 'First line with equals should be returned correctly');
	Assert.AreEqual('another=line', str.GetLine(1), 'Second line with equals should be returned correctly');
	Assert.IsTrue(str.IsMultiLine(), 'Multiline with equals should be detected as multiline');
	Assert.AreEqual(2, str.GetLineCount(), 'Multiline with equals should have count 2');
	
	// Test with multiple equals on same line
	str := 'key1=value1=extra' + CRLF + 'key2=value2';
	Assert.AreEqual('key1=value1=extra', str.GetLine(0), 'Line with multiple equals should be preserved');
	Assert.AreEqual('key2=value2', str.GetLine(1), 'Second line should be correct');
	Assert.AreEqual(2, str.GetLineCount(), 'Lines with multiple equals should have correct count');
	
	// Test complex case with empty lines and equals
	str := 'first=line' + CRLF + '' + CRLF + 'third=line';
	Assert.AreEqual('first=line', str.GetLine(0), 'First line with equals should be correct');
	Assert.AreEqual('', str.GetLine(1), 'Empty middle line should be empty string');
	Assert.AreEqual('third=line', str.GetLine(2), 'Third line with equals should be correct');
	Assert.AreEqual(3, str.GetLineCount(), 'Complex case should have correct line count');
	
	// Test edge case: just equals sign
	str := '=' + CRLF + 'value=';
	Assert.AreEqual('=', str.GetLine(0), 'Line with just equals should be preserved');
	Assert.AreEqual('value=', str.GetLine(1), 'Line ending with equals should be preserved');
	Assert.AreEqual(2, str.GetLineCount(), 'Edge case should have correct count');
end;

procedure TMultiLineStringTest.TestGetLineWithSpecialChars;
var
	str : TMultiLineString;
begin
	// Test with various special characters that could cause parsing issues
	str := 'line with spaces and symbols: @#$%^&*()' + CRLF + 'second=line';
	Assert.AreEqual('line with spaces and symbols: @#$%^&*()', str.GetLine(0), 'Line with special chars should be preserved');
	Assert.AreEqual('second=line', str.GetLine(1), 'Second line should be correct');
	
	// Test with quotes and equals
	str := 'key="value with spaces"' + CRLF + 'path=C:\folder\file.txt';
	Assert.AreEqual('key="value with spaces"', str.GetLine(0), 'Line with quotes should be preserved');
	Assert.AreEqual('path=C:\folder\file.txt', str.GetLine(1), 'Line with path should be preserved');
end;

procedure TMultiLineStringTest.TestIsMultiLineWithDifferentLineEndings;
var
	str : TMultiLineString;
begin
	// Test with CRLF (Windows)
	str := 'line1' + CRLF + 'line2';
	Assert.IsTrue(str.IsMultiLine(), 'CRLF should be detected as multiline');
	
	// Test with LF only (Unix)
	str := 'line1' + LF + 'line2';
	Assert.IsTrue(str.IsMultiLine(), 'LF should be detected as multiline');
	
	// Test with CR only (Mac)
	str := 'line1' + CR + 'line2';
	Assert.IsTrue(str.IsMultiLine(), 'CR should be detected as multiline');
	
	// Test single line with no line endings
	str := 'single line with equals=sign';
	Assert.IsFalse(str.IsMultiLine(), 'Single line should not be detected as multiline');
	
	// Test mixed line endings
	str := 'line1' + CRLF + 'line2' + LF + 'line3' + CR + 'line4';
	Assert.IsTrue(str.IsMultiLine(), 'Mixed line endings should be detected as multiline');
	Assert.AreEqual('line1', str.GetLine(0), 'First line should be correct');
	Assert.AreEqual('line2', str.GetLine(1), 'Second line should be correct');
	Assert.AreEqual('line3', str.GetLine(2), 'Third line should be correct');
	Assert.AreEqual('line4', str.GetLine(3), 'Fourth line should be correct');
end;

procedure TFileUtilsTest.DeleteDirTest;
begin
	var
	tmpDir := TPath.GetTempPath;
	var
	tmpDir1 := tmpDir + '\unittest_tmpdir_01';
	var
	tmpDir2 := tmpDir + '\unittest_tmpdir_02';
	var
	tmpDir3 := tmpDir + '\unittest_tmpdir_03';
	TDirectory.CreateDirectory(tmpDir1);
	TDirectory.CreateDirectory(tmpDir2);
	TDirectory.CreateDirectory(tmpDir3);

	TFileUtils.DeleteTempDirectory('unittest_tmpdir*');

	Assert.IsFalse(TDirectory.Exists(tmpDir1), 'Directory shouldn''t exists.');
	Assert.IsFalse(TDirectory.Exists(tmpDir2), 'Directory shouldn''t exists.');
	Assert.IsFalse(TDirectory.Exists(tmpDir3), 'Directory shouldn''t exists.');
end;

{ TStringsHelperMoreTest }

procedure TStringsHelperMoreTest.Setup;
begin
	FStrings := TStringList.Create;
	FStrings.Add('foo');
	FStrings.Add('bar');
	FStrings.Add('baz=42');
	FStrings.Add('qux=99');
end;

procedure TStringsHelperMoreTest.TearDown;
begin
	FStrings.Free;
end;

procedure TStringsHelperMoreTest.TestContains;
begin
	Assert.IsTrue(FStrings.Contains('foo'));
	Assert.IsFalse(FStrings.Contains('notfound'));
end;

procedure TStringsHelperMoreTest.TestAddIfNotContains;
begin
	Assert.IsTrue(FStrings.AddIfNotContains('newitem'));
	Assert.IsFalse(FStrings.AddIfNotContains('foo'));
end;

procedure TStringsHelperMoreTest.TestHasMatch;
begin
	Assert.IsTrue(FStrings.HasMatch('^ba'));
	Assert.IsFalse(FStrings.HasMatch('^zzz'));
end;

procedure TStringsHelperMoreTest.TestContainsAny;
var
	arr : TArray<string>;
begin
	arr := ['bar', 'notfound'];
	Assert.IsTrue(FStrings.ContainsAny(arr));
	arr := ['notfound'];
	Assert.IsFalse(FStrings.ContainsAny(arr));
end;

procedure TStringsHelperMoreTest.TestDeleteAllMatched;
begin
	FStrings.Add('test123');
	Assert.AreNotEqual(0, FStrings.DeleteAllMatched('\d+'));
	Assert.IsFalse(FStrings.HasMatch('\d+'));
end;

procedure TStringsHelperMoreTest.TestIndexOfAllMatch;
var
	idxs : TArray<integer>;
begin
	FStrings.Add('abc123');
	idxs := FStrings.IndexOfAllMatch('\d+');
	Assert.IsTrue(Length(idxs) > 0);
end;

procedure TStringsHelperMoreTest.TestIndexOfFirstMatch;
begin
	FStrings.Add('abc~123');
	Assert.AreEqual(FStrings.Count - 1, FStrings.IndexOfFirstMatch('a.*~\d+'));
	Assert.AreEqual(-1, FStrings.IndexOfFirstMatch('notfound'));
end;

procedure TStringsHelperMoreTest.TestTryGetDef;
var
	val : string;
begin
	Assert.IsTrue(FStrings.TryGetDef(0, val));
	Assert.AreEqual('foo', val);
	Assert.IsFalse(FStrings.TryGetDef(100, val, 'default'));
	Assert.AreEqual('default', val);
end;

{ TLoadHistoryModesTest }

procedure TLoadHistoryModesTest.TestModeToIntAndIntToMode;
var
	m : IShared<TLoadHistoryModes>;
	i : integer;
begin
	for var e := low(ELoadHistoryMode) to high(ELoadHistoryMode) do begin
		m := Shared.Make<TLoadHistoryModes>();
		m.AddMode(e);
		i := m.ToInt();
		if Ord(e) < Ord(lhmSaveResults) then begin
			Assert.IsTrue(i >= 0, 'ToInt should return a non-negative integer');
		end else begin
			Assert.AreEqual(-1, i, 'ToInt should return a negative integer');
		end;
		m.AddModeFromInt(i);
		Assert.IsTrue(m.IsSet(e), Format('Mode %s should be in the converted mode',
			{ } [TConversions<ELoadHistoryMode>.EnumerationToString(e)]));
	end;
end;

{ TLoadHistoryModesTest }

procedure TLoadHistoryModesTest.TestIsModeActive();
var
	eAsString : string;
	m : IShared<TLoadHistoryModes>;
begin
	for var e := low(ELoadHistoryMode) to high(ELoadHistoryMode) do begin
		m := Shared.Make<TLoadHistoryModes>(TLoadHistoryModes.Create(e));
		eAsString := TConversions<ELoadHistoryMode>.EnumerationToString(e);

		if Ord(e) < Ord(lhmSaveResults) then begin
			Assert.IsTrue(m.IsSaveHistoryActive,
				{ } Format('IsSaveHistoryActive should be set at %s', [eAsString]));
		end else begin
			Assert.IsTrue(not m.IsSaveHistoryActive,
				{ } Format('IsSaveHistoryActive shouldn''t be set at %s', [eAsString]));
		end;
	end;
end;

procedure TLoadHistoryModesTest.TestModeToStringAndStringToMode;
var
	eAsString : string;
	m : IShared<TLoadHistoryModes>;
	s : string;
begin
	for var e := low(ELoadHistoryMode) to high(ELoadHistoryMode) do begin
		m := Shared.Make<TLoadHistoryModes>(TLoadHistoryModes.Create(e));
		s := m.ToString;
		eAsString := TConversions<ELoadHistoryMode>.EnumerationToString(e);
		Assert.IsTrue(s.Contains(eAsString), 'ToString should contain ' + eAsString);
		m.FromString(s);
		Assert.IsTrue(m.IsSet(e), Format('Mode %s should be in the converted mode', [eAsString]));
	end;
end;

{ TConversionsTest }

procedure TConversionsTest.TestStringToEnumeration;
var
	e : TTestEnum;
begin
	e := TConversions<TTestEnum>.StringToEnumeration('teTwo');
	Assert.AreEqual(Ord(teTwo), Ord(e));
end;

procedure TConversionsTest.TestEnumerationToString;
var
	e : TTestEnum;
	s : string;
begin
	e := teThree;
	s := TConversions<TTestEnum>.EnumerationToString(e);
	Assert.AreEqual('teThree', s);
end;

{ TAutoDeleteTempFileTest }

procedure TAutoDeleteTempFileTest.TestFilePathAndFinalize;
var
	path : string;
begin
	path := '';
	for var i := 0 to 3 do begin
		if True then begin
			var temp : TAutoDeleteTempFile;
			Assert.AreNotEqual(path, temp.FilePath);
			path := temp.FilePath;
			Assert.IsTrue(TFile.Exists(path));
		end;
		Assert.IsFalse(TFile.Exists(path));
	end;
end;

initialization

TDUnitX.RegisterTestFixture(TStringsHelperTest);
TDUnitX.RegisterTestFixture(TBitFieldTest);
TDUnitX.RegisterTestFixture(TFileUtilsTest);
TDUnitX.RegisterTestFixture(TMultiLineStringTest);

end.
