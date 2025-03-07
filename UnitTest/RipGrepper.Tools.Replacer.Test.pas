unit RipGrepper.Tools.Replacer.Test;

interface

uses
	DUnitX.TestFramework,
	RipGrepper.Tools.Replacer,
	System.SysUtils;

type

	[TestFixture]
	TTestReplaceHelper = class
		public
			[Test]
			procedure TestReplaceString_UseRegex;
			[Test]
			procedure TestReplaceString_IgnoreCase;
			[Test]
			procedure TestReplaceStringFromCol_IgnoreCase();
			[Test]
			procedure TestReplaceString_SimpleReplace;
			[Test]
			procedure TestSort;
			[Test]
			procedure TestReplaceInFile();
	end;

implementation

uses
	ArrayEx,
	System.Classes,
	System.IOUtils,
	Spring;

procedure TTestReplaceHelper.TestReplaceString_UseRegex;
var
	input, pattern, replacement, result : string;
	mode : TReplaceModes;
begin
	input := 'Hello Delphi Delphi';
	pattern := '\w+';
	replacement := 'World';
	mode := [rmUseRegex];
	result := TReplaceHelper.ReplaceString(input, pattern, replacement, 14, mode);
	Assert.AreEqual('Hello Delphi World', result);
end;

procedure TTestReplaceHelper.TestReplaceString_IgnoreCase;
var
	input, pattern, replacement, result : string;
	mode : TReplaceModes;
begin
	input := 'Hello World';
	pattern := 'world';
	replacement := 'Delphi';
	mode := [rmIgnoreCase];
	result := TReplaceHelper.ReplaceString(input, pattern, replacement, 0, mode);
	Assert.AreEqual('Hello Delphi', result);
end;

procedure TTestReplaceHelper.TestReplaceStringFromCol_IgnoreCase();
var
	input, pattern, replacement, result : string;
	mode : TReplaceModes;
begin
	input := 'Hello Delphi Delphi';
	pattern := 'delphi';
	replacement := 'World';
	mode := [rmIgnoreCase];
	result := TReplaceHelper.ReplaceString(input, pattern, replacement, 14, mode);
	Assert.AreEqual('Hello Delphi World', result);
end;

procedure TTestReplaceHelper.TestReplaceString_SimpleReplace;
var
	input, pattern, replacement, result : string;
	mode : TReplaceModes;
begin
	input := 'Hello World';
	pattern := 'World';
	replacement := 'Delphi';
	mode := [];
	result := TReplaceHelper.ReplaceString(input, pattern, replacement, 0, mode);
	Assert.AreEqual('Hello Delphi', result);
end;

procedure TTestReplaceHelper.TestSort;
var
	rl : TReplaceList;
	rd : TReplaceData;
	SortedList : TArrayEx<TReplaceData>;
begin
	rl := TReplaceList.Create;
	try
		var
		arr := [TReplaceData.New(3, 1, 'Line 3'),
		{ } TReplaceData.New(1, 1, 'Line 1'),
		{ } TReplaceData.New(2, 1, 'Line 2')
		{ } ];

		for rd in arr do begin
			rl.AddUnique('testfile', rd.Row, rd.Col, rd.ReplacedLine);
		end;

		rl.Sort;

		SortedList := rl.Items['testfile'];

		Assert.AreEqual(1, SortedList[0].Row);
		Assert.AreEqual(2, SortedList[1].Row);
		Assert.AreEqual(3, SortedList[2].Row);
	finally
		rl.Free;
	end;
end;

procedure TTestReplaceHelper.TestReplaceInFile();
var
	rl : IShared<TReplaceList>;
	rd : TReplaceData;
	arr : TArrayEx<TReplaceData>;
	sl : IShared<TStringList>;
	sTempFile : string;
begin

	rl := Shared.Make<TReplaceList>();
	sl := Shared.Make<TStringList>();
	for var j : integer := 1 to 7 do begin
		sl.Add(Format('Line %d with word1 word2 word3', [j]));
	end;

	var
	idxWord_1 := sl[0].IndexOf('word1') + 1;
	var
	idxWord_2 := sl[0].IndexOf('word2') + 1;
	var
	idxWord_3 := sl[0].IndexOf('word3') + 1;

	arr := [ // TReplaceData.New( row, col, line )
	{ } TReplaceData.New(1, 1, sl[0].Replace('Line 1', 'First')),

	{ } TReplaceData.New(3, idxWord_2, sl[2].Replace('word2', 'bbbb2')),
	{ } TReplaceData.New(3, idxWord_3, sl[2].Replace('word3', 'cccc3')),
	{ } TReplaceData.New(3, idxWord_1, sl[2].Replace('word1', 'aaaa1')),

	{ } TReplaceData.New(4, idxWord_1, sl[3].Replace('word1', 'aaaa1')),
	{ } TReplaceData.New(4, idxWord_3, sl[3].Replace('word3', 'cccc3')),

	{ } TReplaceData.New(2, 1, sl[1]), // unchanged

	// shorter than orig
	{ } TReplaceData.New(5, idxWord_2, sl[4].Replace('word2', 'cc2')),
	{ } TReplaceData.New(5, idxWord_3, sl[4].Replace('word3', 'bb3')),
	{ } TReplaceData.New(5, idxWord_1, sl[4].Replace('word1', 'bb1')),
	// longer than orig
	{ } TReplaceData.New(6, idxWord_1, sl[5].Replace('word1', 'dddddddd1')),
	{ } TReplaceData.New(6, idxWord_2, sl[5].Replace('word2', 'dddddddd2')),
	{ } TReplaceData.New(6, idxWord_3, sl[5].Replace('word3', 'dddddddd3')),

	{ } TReplaceData.New(7, 1        , sl[6].Replace('Line 7', 'Last')),
	{ } TReplaceData.New(7, idxWord_3, sl[6].Replace('word3', 'and last word'))
	{ } ];

	sTempFile := TPath.GetTempFileName();

	for rd in arr do begin
		rl.AddUnique(sTempFile, rd.Row, rd.Col, rd.ReplacedLine);
	end;

	rl.Sort;
	sl.SaveToFile(sTempFile);
	try
		TReplaceHelper.ReplaceLineInFiles(rl, False);
		sl.Clear;
		sl.LoadFromFile(sTempFile);
	finally
		TFile.Delete(sTempFile);
	end;

	Assert.AreEqual('First with word1 word2 word3', sl[0]);
	Assert.AreEqual('Line 2 with word1 word2 word3', sl[1]);
	Assert.AreEqual('Line 3 with aaaa1 bbbb2 cccc3', sl[2]);
	Assert.AreEqual('Line 4 with aaaa1 word2 cccc3', sl[3]);
	Assert.AreEqual('Line 5 with bb1 cc2 bb3', sl[4]);
	Assert.AreEqual('Line 6 with dddddddd1 dddddddd2 dddddddd3', sl[5]);
	Assert.AreEqual('Last with word1 word2 and last word', sl[6]);
end;

initialization

TDUnitX.RegisterTestFixture(TTestReplaceHelper);

end.
