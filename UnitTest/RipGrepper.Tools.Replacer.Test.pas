unit RipGrepper.Tools.Replacer.Test;

interface

uses
	DUnitX.TestFramework,
	RipGrepper.Tools.Replacer,
	System.SysUtils,
	System.Classes,
	Spring;

type

	[TestFixture]
	TTestReplaceHelper = class
		private
			procedure PreapreFileContent(_replaceList : IShared<TReplaceList>; _fileContent : IShared<TStringList>;
				var _idxWord_1, _idxWord_2, _idxWord_3 : Integer);

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

	System.IOUtils,

	RipGrepper.Common.SimpleTypes;

procedure TTestReplaceHelper.PreapreFileContent(_replaceList : IShared<TReplaceList>; _fileContent : IShared<TStringList>;
	var _idxWord_1, _idxWord_2, _idxWord_3 : Integer);
begin
	for var j : integer := 1 to 7 do begin
		_fileContent.Add(Format('Line %d with word1 word2 word3', [j]));
	end;

	_idxWord_1 := _fileContent[0].IndexOf('word1') + 1;
	_idxWord_2 := _fileContent[0].IndexOf('word2') + 1;
	_idxWord_3 := _fileContent[0].IndexOf('word3') + 1;
end;

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
		arr := [TReplaceData.New(3, 1, 'orig line 3', 'Line 3'),
		{ } TReplaceData.New(1, 1, 'orig line 1', 'Line 1'),
		{ } TReplaceData.New(2, 1, 'orig line 2', 'Line 2')
		{ } ];

		for rd in arr do begin
			rl.AddUnique('testfile', rd.Row, rd.Col, rd.OrigLine, rd.ReplacedLine);
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
	idxWord_1, idxWord_2, idxWord_3 : Integer;
	slFileContent : IShared<TStringList>;
	sOrigFileName : string;
begin
	rl := Shared.Make<TReplaceList>();
	slFileContent := Shared.Make<TStringList>();

	PreapreFileContent(rl, slFileContent, idxWord_1, idxWord_2, idxWord_3);
 	sOrigFileName := TPath.GetTempFileName();
	slFileContent.SaveToFile(sOrigFileName);

	arr := [ // TReplaceData.New( row, col, line )
	{ } TReplaceData.New(1, 1, slFileContent[0], slFileContent[0].Replace('Line 1', 'First')),

	{ } TReplaceData.New(3, idxWord_2, slFileContent[2], slFileContent[2].Replace('word2', 'bbbb2')),
	{ } TReplaceData.New(3, idxWord_3, slFileContent[2], slFileContent[2].Replace('word3', 'cccc3')),
	{ } TReplaceData.New(3, idxWord_1, slFileContent[2], slFileContent[2].Replace('word1', 'aaaa1')),

	{ } TReplaceData.New(4, idxWord_1, slFileContent[3], slFileContent[3].Replace('word1', 'aaaa1')),
	{ } TReplaceData.New(4, idxWord_3, slFileContent[3], slFileContent[3].Replace('word3', 'cccc3')),

	{ } TReplaceData.New(2, 1, slFileContent[1], slFileContent[1]), // unchanged

	// shorter than orig
	{ } TReplaceData.New(5, idxWord_2, slFileContent[4], slFileContent[4].Replace('word2', 'cc2')),
	{ } TReplaceData.New(5, idxWord_3, slFileContent[4], slFileContent[4].Replace('word3', 'bb3')),
	{ } TReplaceData.New(5, idxWord_1, slFileContent[4], slFileContent[4].Replace('word1', 'bb1')),
	// longer than orig
	{ } TReplaceData.New(6, idxWord_1, slFileContent[5], slFileContent[5].Replace('word1', 'dddddddd1')),
	{ } TReplaceData.New(6, idxWord_2, slFileContent[5], slFileContent[5].Replace('word2', 'dddddddd2')),
	{ } TReplaceData.New(6, idxWord_3, slFileContent[5], slFileContent[5].Replace('word3', 'dddddddd3')),

	{ } TReplaceData.New(7, 1, slFileContent[6], slFileContent[6].Replace('Line 7', 'Last')),
	{ } TReplaceData.New(7, idxWord_3, slFileContent[6], slFileContent[6].Replace('word3', 'and last word'))
	{ } ];

	for rd in arr do begin
		rl.AddUnique(sOrigFileName, rd.Row, rd.Col, rd.OrigLine, rd.ReplacedLine);
	end;

	rl.Sort;
	try
		TReplaceHelper.ReplaceLineInFiles(rl, False);
		slFileContent.Clear;
		slFileContent.LoadFromFile(sOrigFileName);
	finally
		TFile.Delete(sOrigFileName);
	end;

	Assert.AreEqual('First with word1 word2 word3', slFileContent[0]);
	Assert.AreEqual('Line 2 with word1 word2 word3', slFileContent[1]);
	Assert.AreEqual('Line 3 with aaaa1 bbbb2 cccc3', slFileContent[2]);
	Assert.AreEqual('Line 4 with aaaa1 word2 cccc3', slFileContent[3]);
	Assert.AreEqual('Line 5 with bb1 cc2 bb3', slFileContent[4]);
	Assert.AreEqual('Line 6 with dddddddd1 dddddddd2 dddddddd3', slFileContent[5]);
	Assert.AreEqual('Last with word1 word2 and last word', slFileContent[6]);
end;

initialization

TDUnitX.RegisterTestFixture(TTestReplaceHelper);

end.
