unit RipGrepper.Common.ArrayExTest;

interface

uses
	DUnitX.TestFramework,
	System.Classes,
	System.Generics.Defaults,
	RipGrepper.Common.Constants,
	ArrayEx;

type

	TTestRecord = record
		Name : string;
		Age : integer;
		constructor Create(AName : string; AAge : integer);
		class function NameComparer : IComparer<TTestRecord>; static;
		class function AgeComparer : IComparer<TTestRecord>; static;
		class function ConvertToNames(const Value : TTestRecord) : string; static;
		class function ConvertToAges(const Value : TTestRecord) : integer; static;
		class function NameAgeComparer() : IComparer<TTestRecord>; static;
	end;

	[TestFixture]
	TArrayExTest = class

		private
			function CompareJokerFunction(const Value : string) : boolean;
			procedure TestArrayContainer();
			procedure TestArrayHelper();
			procedure Test_TestRecord();

		public
			[Test]
			procedure GetReversedRangeTest();
			// old tests:...
			[Test]
			procedure RecordTest();
			[Test]
			procedure ArrayHelperTest();
			[Test]
			procedure ArrayContainerTest();
			// new tests:
			[Test]
			procedure GetReversedRangeCountTest();
			[Test]
			procedure GetReversedRangeMaxCountTest();
			[Test]
			procedure GetReversedRangeMaxCountTest1();
			[Test]
			procedure GetRangeAndReversedRange();
			[Test]
			procedure MultiDimContainTest();
			[Test]
			procedure MultiDimStrContainTest();
			[Test]
			[TestCase('Empty array', '', '')]
			[TestCase('No matches', '10,20,30', '1,2,3')]
			[TestCase('First element match', '1,2,3', '1,4,5')]
			[TestCase('Second element match', '1,2,3', '4,2,5')]
			[TestCase('Last element match', '1,2,3', '4,5,3')]
			[TestCase('Multiple matches', '1,2,3', '2,3,4')]
			[TestCase('Single element match', '5', '5')]
			[TestCase('Single element no match', '5', '7')]
			procedure GetFirstMatchIndexTest(const sourceArray : string; const testArray : string);
			[Test]
			procedure GetFirstMatchIndexStringTest();
			[Test]
			procedure ContainsTest();
			[Test]
			procedure InsertIfNotContainsTest();
			[Test]
			procedure InsertIfNotContainsEmptyArrayTest();
			[Test]
			procedure InsertIfNotContainsAlreadyExistsTest();
			[Test]
			procedure InsertIfNotContainsAtBeginningTest();
			[Test]
			procedure InsertIfNotContainsAtEndTest();
			[Test]
			procedure InsertIfNotContainsAtMiddleTest();
	end;

implementation

uses
	System.SysUtils,
	System.Generics.Collections;

function TArrayExTest.CompareJokerFunction(const Value : string) : boolean;
begin
	Result := LowerCase(Value) = 'joker';
end;

procedure TArrayExTest.GetReversedRangeTest();
var
	ai : TArrayEx<integer>;
begin
	ai := [0, 1, 2, 3, 4, 5, 6, 7, 8, 9];
	var
	revArr := ai.GetReversedRange();
	Assert.AreEqual < TArray < integer >> ([9, 8, 7, 6, 5, 4, 3, 2, 1, 0], revArr, 'reversed array should be equal');
end;

procedure TArrayExTest.RecordTest();
begin
	Test_TestRecord;
end;

procedure TArrayExTest.ArrayHelperTest();
begin
	TestArrayHelper;
end;

procedure TArrayExTest.ArrayContainerTest();
begin
	TestArrayContainer;
end;

procedure TArrayExTest.GetReversedRangeCountTest();
var
	ai : TArrayEx<integer>;
begin
	ai := [0, 1, 2, 3, 4, 5, 6, 7, 8, 9];

	var
	revArr := ai.GetReversedRange(7, 4);

	Assert.AreEqual < TArray < integer >> ([7, 6, 5, 4], revArr, 'reversed array count should be equal')
end;

procedure TArrayExTest.GetReversedRangeMaxCountTest();
var
	ai : TArrayEx<integer>;
begin
	ai := [0, 1, 2, 3, 4];

	var
	revArr := ai.GetReversedRange(-1, 10);

	Assert.AreEqual < TArray < integer >> ([4, 3, 2, 1, 0], revArr, 'reversed array count should be equal')
end;

procedure TArrayExTest.GetReversedRangeMaxCountTest1();
var
	ai : TArrayEx<integer>;
begin
	ai := [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10];

	var
	revArr := ai.GetReversedRange(-1, 5);

	Assert.AreEqual < TArray < integer >> ([10, 9, 8, 7, 6], revArr, 'reversed array count should be equal')
end;

procedure TArrayExTest.GetRangeAndReversedRange();
var
	ai : TArrayEx<integer>;
begin
	ai := [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10];

	var
	revArr := ai.GetRange(0, 5).GetReversedRange();

	Assert.AreEqual < TArray < integer >> ([4, 3, 2, 1, 0], revArr, 'reversed array count should be equal')
end;

procedure TArrayExTest.MultiDimContainTest();
begin
	var
	a := TArrayEx < TArray < integer >>.Create([[0, 1], [1, 2], [3, 4]]);

	Assert.IsTrue(a.Contains([1, 2]), 'array should contain [1,2]');
	Assert.IsFalse(a.Contains([4, 2]), 'array shouldn''t contain [4,2]');
	Assert.IsFalse(a.Contains([1, 4]), 'array shouldn''t contain [1,4]');
	Assert.IsTrue(a.Contains([3, 4]), 'array should contain [3,4]');
end;

procedure TArrayExTest.MultiDimStrContainTest();
begin
	var
	a := TArrayEx < TArray < string >>.Create([['0', '1'], ['1', '2'], ['3', '4']]);
	var
	comp := TComparer < TArray < string >>.Construct(
		function(const Left, Right : TArray<string>) : Integer
		begin
			Result := TComparer<string>.Default.Compare(string.Join('', Left), string.Join('', Right));
		end);
	Assert.IsTrue(a.Contains(['1', '2'], comp), 'array should contain [1,2]');
	Assert.IsFalse(a.Contains(['4', '2'], comp), 'array shouldn''t contain [4,2]');
	Assert.IsFalse(a.Contains(['1', '4'], comp), 'array shouldn''t contain [1,4]');
	Assert.IsTrue(a.Contains(['3', '4'], comp), 'array should contain [3,4]');
end;

procedure TArrayExTest.GetFirstMatchIndexTest(const sourceArray : string; const testArray : string);
var
	ai : TArrayEx<integer>;
	testValues : TArray<integer>;
	result : integer;
	expectedResult : integer;
begin
	expectedResult := -1;
	// Parse source array
	if sourceArray = '' then begin
		ai.Clear;
	end else begin
		var
		sourceElements := sourceArray.Split([',']);
		var
			sourceInts : TArray<integer>;
		SetLength(sourceInts, Length(sourceElements));
		for var i := 0 to high(sourceElements) do
			sourceInts[i] := StrToInt(sourceElements[i]);
		ai := sourceInts;
	end;

	// Parse test array
	if testArray = '' then begin
		SetLength(testValues, 0);
	end else begin
		var
		testElements := testArray.Split([',']);
		SetLength(testValues, Length(testElements));
		for var i := 0 to high(testElements) do
			testValues[i] := StrToInt(testElements[i]);
	end;

	// Calculate expected result if not already set
	if (sourceArray <> '') and (testArray <> '') then begin
		for var i := 0 to ai.Count - 1 do begin
			for var j := 0 to high(testValues) do begin
				if ai[i] = testValues[j] then begin
					expectedResult := i;
					Break;
				end;
			end;
			if expectedResult >= 0 then
				Break;
		end;
	end;

	// Execute test
	result := ai.GetFirstMatchIndex(testValues);

	// Assert result
	Assert.IsTrue(result = expectedResult, Format('Expected %d but got %d for source=%s, test=%s', [expectedResult, result, sourceArray,
		testArray]));
end;

procedure TArrayExTest.GetFirstMatchIndexStringTest();
var
	aStr : TArrayEx<string>;
	testStrings : TArray<string>;
	result : integer;
begin
	// Test with string array
	aStr := ['apple', 'banana', 'cherry'];
	testStrings := ['orange', 'banana', 'grape'];
	result := aStr.GetFirstMatchIndex(testStrings);
	Assert.IsTrue(result = 1, 'String array match should return correct index');

	// Test with no string matches
	aStr := ['apple', 'banana', 'cherry'];
	testStrings := ['orange', 'kiwi', 'grape'];
	result := aStr.GetFirstMatchIndex(testStrings);
	Assert.IsTrue(result = -1, 'String array with no matches should return -1');
end;

procedure TArrayExTest.ContainsTest();
var
	aiInt : TArrayEx<integer>;
	asStr : TArrayEx<string>;
	arRec : TArrayEx<TTestRecord>;
	customComparer : IComparer<integer>;
begin
	// Test with integer array - basic contains
	aiInt := [1, 2, 3, 4, 5];
	Assert.IsTrue(aiInt.Contains(3), 'Integer array should contain 3');
	Assert.IsFalse(aiInt.Contains(10), 'Integer array should not contain 10');
	Assert.IsTrue(aiInt.Contains(1), 'Integer array should contain first element');
	Assert.IsTrue(aiInt.Contains(5), 'Integer array should contain last element');

	// Test with empty integer array
	aiInt.Clear;
	Assert.IsFalse(aiInt.Contains(1), 'Empty array should not contain any element');

	// Test with single element
	aiInt := [42];
	Assert.IsTrue(aiInt.Contains(42), 'Single element array should contain the element');
	Assert.IsFalse(aiInt.Contains(41), 'Single element array should not contain different element');

	// Test with string array
	asStr := ['apple', 'banana', 'cherry', 'date'];
	Assert.IsTrue(asStr.Contains('banana'), 'String array should contain banana');
	Assert.IsFalse(asStr.Contains('orange'), 'String array should not contain orange');
	Assert.IsTrue(asStr.Contains('apple'), 'String array should contain first element');
	Assert.IsTrue(asStr.Contains('date'), 'String array should contain last element');

	// Test with custom comparer for integers
	aiInt := [1, 2, 3, 4, 5];
	customComparer := TComparer<integer>.Construct(
		function(const Left, Right : integer) : Integer
		begin
			// Custom comparison: consider values equal if their difference is <= 1
			if Abs(Left - Right) <= 1 then
				Result := 0
			else
				Result := Left - Right;
		end);

	Assert.IsTrue(aiInt.Contains(3, customComparer), 'Should contain 3 with custom comparer');
	Assert.IsTrue(aiInt.Contains(2, customComparer), 'Should contain 2 with custom comparer (matches 1,2,3)');
	Assert.IsFalse(aiInt.Contains(10, customComparer), 'Should not contain 10 with custom comparer');

	// Test with record array using default equality
	arRec := [TTestRecord.Create('Alice', 25), TTestRecord.Create('Bob', 30), TTestRecord.Create('Charlie', 35)];
	Assert.IsTrue(arRec.Contains(TTestRecord.Create('Bob', 30), TTestRecord.NameComparer), 'Record array should contain Bob record');
	Assert.IsFalse(arRec.Contains(TTestRecord.Create('Dave', 40), TTestRecord.NameComparer), 'Record array should not contain Dave record');
	Assert.IsFalse(arRec.Contains(TTestRecord.Create('Bob', 31), TTestRecord.NameAgeComparer),
		'Record array should not contain Bob with different age');

	// Test with record array using custom name comparer
	Assert.IsTrue(arRec.Contains(TTestRecord.Create('Alice', 99), TTestRecord.NameComparer), 'Should find Alice by name regardless of age');
	Assert.IsFalse(arRec.Contains(TTestRecord.Create('Eve', 25), TTestRecord.NameComparer), 'Should not find Eve by name');
end;

procedure TArrayExTest.TestArrayContainer();
const
	CWeek : array [1 .. 8] of string = ('Mon', 'Tues', 'Wednes', 'Bug', 'Thurs', 'Fri', 'Satur', 'Sun');
var
	AStr : TArrayEx<string>;
	AI, AI2 : TArrayEx<integer>;
	I : Integer;
	S : string;
begin
	AI := TArrayEx<integer>.Create(0);
	Assert.IsTrue(AI.Count = 0);
	AStr := TArrayEx<string>.Create(10);
	Assert.IsTrue((AStr.Count = 10) and (AStr[1] = ''), 'first');

	// Create
	AI.Create([1, 2, 3]);
	Assert.IsTrue(AI.Compare([1, 2, 3]), 'Create');

	// Add
	AI.Clear;
	Assert.IsTrue(AI.Add(1) = 0, 'Add0');
	Assert.IsTrue(AI.Add(2) = 1, 'Add1');
	Assert.IsTrue(AI.Add(3) = 2, 'Add2');

	// IndexOf
	Assert.IsTrue(AI.IndexOf(1) = 0, 'IndexOf0');
	Assert.IsTrue(AI.IndexOf(2) = 1, 'IndexOf1');
	Assert.IsTrue(AI.IndexOf(5) = -1, 'IndexOf-1');

	// Contains
	Assert.IsTrue(AI.Contains(2) = TRUE);
	Assert.IsTrue(AI.Contains(5) = FALSE);
	Assert.IsTrue(AI.Contains(5, TComparer<integer>.Construct(
		function(const Left, Right : integer) : Integer
		begin
			Result := (Left + 4) - Right;
		end)) = TRUE);

	// Delete
	AI.Delete(1);
	Assert.IsTrue(AI.Contains(2) = FALSE);
	Assert.IsTrue(AI.Count = 2);
	// try
	// AI.Delete(2);
	// except
	// end; // exception expected
	AI.Delete(0);
	Assert.IsTrue(AI.Count = 1);
	AI.Delete(0);
	Assert.IsTrue(AI.Count = 0);
	// try
	// AI.Delete(0);
	// Assert.IsTrue(TRUE);
	// except
	// end; // exception expected

	// Insert
	AStr.Clear;
	AStr.Insert(0, 'one');
	AStr.Insert(0, 'two');
	Assert.IsTrue(AStr.Count = 2);
	Assert.IsTrue(AStr[0] = 'two');
	Assert.IsTrue(AStr[1] = 'one');

	AStr.Insert(2, 'three');
	Assert.IsTrue((AStr.Count = 3) and (AStr[2] = 'three'));

	// AddRange
	AI.Clear;
	AI.AddRange(TArray<integer>.Create(4, 5, 6));
	Assert.IsTrue((AI.Count = 3) and (AI[2] = 6));
	AI.AddRange(TArray<integer>.Create(10, 11, 12));
	Assert.IsTrue((AI.Count = 6) and (AI[5] = 12) and (AI[0] = 4));

	// Compare
	AI.Create([1, 2, 3]);
	AI2 := AI;
	Assert.IsTrue(AI.Compare([1, 2, 3]));
	Assert.IsTrue(AI.Compare(AI.Items));
	Assert.IsTrue(AI.Compare(AI2));
	AI2.Add(4);
	Assert.IsTrue(not AI.Compare(AI2));

	// Equal
	AI.Create([1, 2, 3, 4, 5, 6]);
	AI2 := AI;
	Assert.IsTrue(AI = AI2);
	AI.AddRange(AI2);
	Assert.IsTrue((AI.Count = 12) and (AI <> AI2));
	// TODO AI2.InsertRange(AI2.Count, AI2);
	// Assert.IsTrue((AI.Count = AI2.Count) and (AI = AI2));

	// InsertRange
	AI.Clear;
	AI.InsertRange(0, TArray<integer>.Create(4, 5, 6));
	Assert.IsTrue((AI.Count = 3) and (AI[2] = 6));
	AI.InsertRange(0, [10, 11, 12]);
	Assert.IsTrue((AI.Count = 6) and (AI[5] = 6) and (AI[0] = 10));
	AI.InsertRange(3, [21, 22]);
	Assert.IsTrue((AI.Count = 8) and (AI[7] = 6) and (AI[0] = 10) and (AI[3] = 21));

	// ForEach
	AI.Items := TArray<integer>.Create(5, 4, 3, 2, 1);
	AStr.Clear;
	AI.ForEach(
		procedure(var Value : integer; Index : integer)
		begin
			Value := Value * 10;
			AStr.Add(IntToStr(Value));
		end);
	// sort
	AI.Sort;
	AStr.Sort;
	Assert.IsTrue(AI.Compare([10, 20, 30, 40, 50]));
	Assert.IsTrue(AStr.Compare(['10', '20', '30', '40', '50']));

	// Find
	AI.Clear;
	AStr.SetItems(['4', 'king', 'joker', '7', 'JOKER', 'joker', 'ace', 'joker']);
	I := -1;
	repeat
		I := AStr.Find(CompareJokerFunction, I + 1);
		if I >= 0 then
			AI.Add(I);
	until I < 0;
	Assert.IsTrue(AI.Compare([2, 4, 5, 7]));

	// Map
	AI.Clear;
	for I := 1 to 50 do
		AI.Add(I);
	AI := AI.Map(
		function(var Value : integer; Index : integer) : boolean
		begin
			Result := (Value >= 10) and (Value < 20);
			if Result then
				Value := Value + 100;
		end);
	Assert.IsTrue(AI.Count = 10);
	Assert.IsTrue(AI[1] = 111);

	// Map <string>
	AStr.SetItems(CWeek);
	AStr := AStr.Map(
		function(var Value : string; Index : integer) : boolean
		begin
			Result := Value <> 'Bug';
			Value := Value + 'day';
		end);
	Assert.IsTrue(AStr.Contains('Monday'));
	Assert.IsTrue(AStr.Contains('Sunday'));
	Assert.IsTrue(not AStr.Contains('Bugday'));

	// enumerate
	AI.Clear;
	AStr.SetItems(CWeek);
	for S in AStr do
		AI.Add(length(S));
	Assert.IsTrue(AI.Count = AStr.Count);
	Assert.IsTrue(AI.Compare([3, 4, 6, 3, 5, 3, 5, 3]));
	// check empty enumeration
	AStr.Clear;
	for S in AStr do
		AI.Add(length(S));
	Assert.IsTrue(AI.Compare([3, 4, 6, 3, 5, 3, 5, 3]));

	// Unique
	AI.Unique;
	AI.Sort;
	Assert.IsTrue(AI.Compare([3, 4, 5, 6]));

	// GetRange
	// TODO Assert.IsTrue(AI.GetRange(2).Compare([5, 6]));
	Assert.IsTrue(AI.GetRange(0, 2).Compare([3, 4]));
	// TODO Assert.IsTrue(AI.GetRange(1, 2).Compare([4, 5]));

end;

procedure TArrayExTest.TestArrayHelper();
var
	AI : TArray<integer>;
	AStr : TArray<string>;
	I : Integer;
begin
	// Add
	AI := nil;
	Assert.IsTrue(TArray.Add<integer>(AI, 1) = 0);
	Assert.IsTrue(TArray.Add<integer>(AI, 2) = 1);
	Assert.IsTrue(TArray.Add<integer>(AI, 3) = 2);

	// IndexOf
	Assert.IsTrue(TArray.IndexOf<integer>(AI, 1) = 0);
	Assert.IsTrue(TArray.IndexOf<integer>(AI, 2) = 1);
	Assert.IsTrue(TArray.IndexOf<integer>(AI, 5) = -1);

	// Contains
	Assert.IsTrue(TArray.Contains<integer>(AI, 2) = TRUE);
	Assert.IsTrue(TArray.Contains<integer>(AI, 5) = FALSE);
	Assert.IsTrue(TArray.Contains<integer>(AI, 5, TComparer<integer>.Construct(
		function(const Left, Right : integer) : Integer
		begin
			Result := Left - (Right + 4);
		end)) = FALSE);

	// Delete
	TArray.Delete<integer>(AI, 1);
	Assert.IsTrue(TArray.Contains<integer>(AI, 2) = FALSE);
	Assert.IsTrue(length(AI) = 2);
	// try
	// TArray.Delete<integer>(AI, 2);
	// Assert.IsTrue(TRUE);
	// except
	// end; // exception expected
	TArray.Delete<integer>(AI, 0);
	Assert.IsTrue(length(AI) = 1);
	TArray.Delete<integer>(AI, 0);
	Assert.IsTrue(length(AI) = 0);
	// try
	// TArray.Delete<integer>(AI, 0);
	// Assert.IsTrue(TRUE);
	// except
	// end; // exception expected

	// Insert
	AStr := nil;
	TArray.Insert<string>(AStr, 0, 'one');
	TArray.Insert<string>(AStr, 0, 'two');
	Assert.IsTrue(length(AStr) = 2);
	Assert.IsTrue(AStr[0] = 'two');
	Assert.IsTrue(AStr[1] = 'one');

	TArray.Insert<string>(AStr, 2, 'three');
	Assert.IsTrue((length(AStr) = 3) and (AStr[2] = 'three'));

	// AddRange
	AI := nil;
	TArray.AddRange<integer>(AI, TArray<integer>.Create(4, 5, 6));
	Assert.IsTrue((length(AI) = 3) and (AI[2] = 6));
	TArray.AddRange<integer>(AI, TArray<integer>.Create(10, 11, 12));
	Assert.IsTrue((length(AI) = 6) and (AI[5] = 12) and (AI[0] = 4));

	// InsertRange
	AI := nil;
	TArray.InsertRange<integer>(AI, 0, TArray<integer>.Create(4, 5, 6));
	Assert.IsTrue((length(AI) = 3) and (AI[2] = 6));
	TArray.InsertRange<integer>(AI, 0, TArray<integer>.Create(10, 11, 12));
	Assert.IsTrue((length(AI) = 6) and (AI[5] = 6) and (AI[0] = 10));
	TArray.InsertRange<integer>(AI, 3, TArray<integer>.Create(21, 22));
	Assert.IsTrue((length(AI) = 8) and (AI[7] = 6) and (AI[0] = 10) and (AI[3] = 21));

	// ForEach
	AI := TArray<integer>.Create(5, 4, 3, 2, 1);
	AStr := nil;
	TArray.ForEach<integer>(AI,
		procedure(var Value : integer; Index : integer)
		begin
			Value := Value * 10;
			TArray.Add<string>(AStr, IntToStr(Value));
		end);
	TArray.Sort<integer>(AI);
	TArray.Sort<string>(AStr);
	Assert.IsTrue(TArray.Compare<integer>(AI, TArray<integer>.Create(10, 20, 30, 40, 50)));
	Assert.IsTrue(TArray.Compare<string>(AStr, TArray<string>.Create('10', '20', '30', '40', '50')));

	// Find
	AI := nil;
	AStr := TArray<string>.Create('4', 'king', 'joker', '7', 'JOKER', 'joker', 'ace', 'joker');
	I := -1;
	repeat
		I := TArray.Find<string>(AStr, CompareJokerFunction, I + 1);
		if I >= 0 then
			TArray.Add<integer>(AI, I);
	until I < 0;
	Assert.IsTrue(TArray.Compare<integer>(AI, TArray<integer>.Create(2, 4, 5, 7)));

	// Map
	AI := nil;
	for I := 1 to 50 do
		TArray.Add<integer>(AI, I);
	AI := TArray.Map<integer>(AI,
		function(var Value : integer; Index : integer) : boolean
		begin
			Result := (Value >= 10) and (Value < 20);
			if Result then
				Value := Value + 100;
		end);
	Assert.IsTrue(length(AI) = 10);
	Assert.IsTrue(AI[1] = 111);

	// Map <string>
	AStr := TArray<string>.Create('Mon', 'Tues', 'Wednes', 'Thurs', 'Fri', 'Satur', 'Sun');
	AStr := TArray.Map<string>(AStr,
		function(var Value : string; Index : integer) : boolean
		begin
			Result := TRUE;
			Value := Value + 'day';
		end);
	Assert.IsTrue(TArray.Contains<string>(AStr, 'Monday'));
	Assert.IsTrue(TArray.Contains<string>(AStr, 'Sunday'));

end;

procedure TArrayExTest.InsertIfNotContainsTest();
var
	ai : TArrayEx<integer>;
	result : boolean;
begin
	// Test inserting a new item
	ai := [1, 2, 3, 4, 5];
	result := ai.InsertIfNotContains(2, 10);

	Assert.IsTrue(result, 'Should return true when item is inserted');
	Assert.AreEqual(6, Length(ai.Items), 'Array length should increase by 1');
	Assert.AreEqual(10, ai.Items[2], 'Item should be at specified index');
	Assert.IsTrue(ai.Compare([1, 2, 10, 3, 4, 5]), 'Array should contain item at correct position');
end;

procedure TArrayExTest.InsertIfNotContainsEmptyArrayTest();
var
	ai : TArrayEx<integer>;
	result : boolean;
begin
	// Test inserting into empty array
	ai := [];
	result := ai.InsertIfNotContains(0, 42);

	Assert.IsTrue(result, 'Should return true when inserting into empty array');
	Assert.AreEqual(1, Length(ai.Items), 'Array length should be 1');
	Assert.AreEqual(42, ai.Items[0], 'Item should be at index 0');
end;

procedure TArrayExTest.InsertIfNotContainsAlreadyExistsTest();
var
	ai : TArrayEx<integer>;
	result : boolean;
	originalLength : integer;
begin
	// Test inserting an item that already exists
	ai := [1, 2, 3, 4, 5];
	originalLength := Length(ai.Items);
	result := ai.InsertIfNotContains(2, 3);

	Assert.IsFalse(result, 'Should return false when item already exists');
	Assert.AreEqual(originalLength, Length(ai.Items), 'Array length should not change');
	Assert.IsTrue(ai.Compare([1, 2, 3, 4, 5]), 'Array should remain unchanged');
end;

procedure TArrayExTest.InsertIfNotContainsAtBeginningTest();
var
	ai : TArrayEx<integer>;
	result : boolean;
begin
	// Test inserting at the beginning
	ai := [10, 20, 30];
	result := ai.InsertIfNotContains(0, 5);

	Assert.IsTrue(result, 'Should return true when inserting at beginning');
	Assert.AreEqual(4, Length(ai.Items), 'Array length should increase by 1');
	Assert.AreEqual(5, ai.Items[0], 'Item should be at index 0');
	Assert.IsTrue(ai.Compare([5, 10, 20, 30]), 'Array should have item at beginning');
end;

procedure TArrayExTest.InsertIfNotContainsAtEndTest();
var
	ai : TArrayEx<integer>;
	result : boolean;
begin
	// Test inserting at the end
	ai := [10, 20, 30];
	result := ai.InsertIfNotContains(3, 40);

	Assert.IsTrue(result, 'Should return true when inserting at end');
	Assert.AreEqual(4, Length(ai.Items), 'Array length should increase by 1');
	Assert.AreEqual(40, ai.Items[3], 'Item should be at last index');
	Assert.IsTrue(ai.Compare([10, 20, 30, 40]), 'Array should have item at end');
end;

procedure TArrayExTest.InsertIfNotContainsAtMiddleTest();
var
	as_ : TArrayEx<string>;
	result : boolean;
begin
	// Test with strings at middle position
	as_ := ['alpha', 'beta', 'gamma', 'delta'];
	result := as_.InsertIfNotContains(2, 'epsilon');

	Assert.IsTrue(result, 'Should return true when inserting string');
	Assert.AreEqual(5, Length(as_.Items), 'Array length should increase by 1');
	Assert.AreEqual('epsilon', as_.Items[2], 'String should be at index 2');
	Assert.IsTrue(as_.Compare(['alpha', 'beta', 'epsilon', 'gamma', 'delta']), 'Array should have string at correct position');
end;

procedure TArrayExTest.Test_TestRecord();
var
	List : TArrayEx<TTestRecord>;
	StrList : TArrayEx<string>;
	I : integer;
begin
	// create list
	List.Clear;
	List.Add(TTestRecord.Create('Jack', 26));
	List.Add(TTestRecord.Create('Anton', 28));
	List.Add(TTestRecord.Create('Barbie', 50));
	List.Add(TTestRecord.Create('Mickey Mouse', 90));

	// sort by name
	List.Sort(TTestRecord.NameComparer);
	// convert to string array

	StrList := List.Convert<string>(TTestRecord.ConvertToNames);
	Assert.IsTrue(StrList.Compare(['Anton', 'Barbie', 'Jack', 'Mickey Mouse']));

	// convert to integer array
	Assert.IsTrue(List.Convert<integer>(TTestRecord.ConvertToAges).Compare([28, 50, 26, 90]));

	// sort by age
	List.Sort(TTestRecord.AgeComparer);
	Assert.IsTrue(List[0].Name = 'Jack');

	// IndexOf Min / Max
	Assert.IsTrue(List.IndexOfMax(TTestRecord.AgeComparer) = 3);
	Assert.IsTrue(List.IndexOfMin(TTestRecord.AgeComparer) = 0);

	I := List.IndexOfMax(TTestRecord.NameComparer);
	Assert.IsTrue(List[I].Name = 'Mickey Mouse');

	I := List.IndexOfMin(TTestRecord.NameComparer);
	Assert.IsTrue(List[I].Name = 'Anton');

	// Unique
	List.Add(List[0]);
	List.Insert(2, List[1]);
	List.Insert(4, List[1]);
	List.Unique;
	List.Sort(TTestRecord.NameComparer);
	StrList := List.Convert<string>(TTestRecord.ConvertToNames);
	Assert.IsTrue(StrList.Compare(['Anton', 'Barbie', 'Jack', 'Mickey Mouse']));

end;

constructor TTestRecord.Create(AName : string; AAge : integer);
begin
	name := AName;
	Age := AAge;
end;

class function TTestRecord.ConvertToNames(const Value : TTestRecord) : string;
begin
	Result := Value.Name;
end;

class function TTestRecord.ConvertToAges(const Value : TTestRecord) : integer;
begin
	Result := Value.Age;
end;

class function TTestRecord.AgeComparer : IComparer<TTestRecord>;
begin
	Result := TComparer<TTestRecord>.Construct(
		function(const Left, Right : TTestRecord) : Integer
		begin
			Result := TComparer<integer>.Default.Compare(Left.Age, Right.Age);
		end);
end;

class function TTestRecord.NameComparer : IComparer<TTestRecord>;
begin
	Result := TComparer<TTestRecord>.Construct(
		function(const Left, Right : TTestRecord) : Integer
		begin
			Result := TComparer<string>.Default.Compare(Left.Name, Right.Name);
		end);
end;

class function TTestRecord.NameAgeComparer() : IComparer<TTestRecord>;
begin
	Result := TComparer<TTestRecord>.Construct(
		function(const Left, Right : TTestRecord) : Integer
		begin
			Result := Abs(TComparer<string>.Default.Compare(Left.Name, Right.Name)) + Abs(TComparer<integer>.Default.Compare(Left.Age,
				Right.Age));
		end);
end;

initialization

TDUnitX.RegisterTestFixture(TArrayExTest);

end.
