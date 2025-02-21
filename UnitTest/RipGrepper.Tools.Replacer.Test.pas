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
	System.IOUtils;

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
			rl.AddUnique('testfile', rd.Row, rd.Col, rd.Line);
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
	rl : TReplaceList;
	rd : TReplaceData;
	SortedList : TArrayEx<TReplaceData>;
	arr : TArrayEx<TReplaceData>;
	sl : TStringList;
	sTempFile : string;
begin
	for var i : integer := 1 to 5 do begin
		sl.Add(Format('Line %d with word1_%d word2_%d word3_%d', [i, i, i, i]));
	end;
	sTempFile := TPath.GetTempFileName();
	sl.SaveToFile(sTempFile);
	var
	i := 2;
	arr := [
	{ } TReplaceData.New(5, 13, sl[4]),
	{ } TReplaceData.New(1, 13, sl[0]),

	{ } TReplaceData.New(3, 21, Format('Line %d with cccc1_%d word2_%d word3_%d', [i, i, i, i])),
	{ } TReplaceData.New(3, 13, Format('Line %d with word1_%d aaaa2_%d word3_%d', [i, i, i, i])),
	{ } TReplaceData.New(3, 29, Format('Line %d with word1_%d word2_%d bbbb3_%d', [i, i, i, i])),

	{ } TReplaceData.New(4, 13, sl[3]),
	{ } TReplaceData.New(2, 13, sl[1])
	{ } ];

	rl := TReplaceList.Create;
	try
		for rd in arr do begin
			rl.AddUnique(sTempFile, rd.Row, rd.Col, rd.Line);
		end;

		rl.Sort;
		TReplaceHelper.ReplaceLineInFiles(rl, False);
		sl.Clear;
		sl.LoadFromFile(sTempFile);

		Assert.AreEqual(Format('Line %d with cccc1_%d aaaa2_%d bbbb3_%d', [i, i, i, i]), sl[3]);

	finally
		rl.Free;
		TFile.Delete(sTempFile);
	end;
end;

initialization

TDUnitX.RegisterTestFixture(TTestReplaceHelper);

end.
