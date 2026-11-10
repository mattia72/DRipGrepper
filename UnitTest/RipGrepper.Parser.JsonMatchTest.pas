unit RipGrepper.Parser.JsonMatchTest;

interface

uses
	DUnitX.TestFramework,
	RipGrepper.Common.Interfaces,
	RipGrepper.Common.SearchTextWithOptions,
	Spring;

type

	TSearchParamMock = class(TInterfacedObject, ISearchParams)
		private
			FGuiSearchParams : IShared<TSearchTextWithOptions>;

		public
			constructor Create(const _guiParams : IShared<TSearchTextWithOptions>);
			function GetGuiSearchParams() : IShared<TSearchTextWithOptions>;
	end;

	[TestFixture]
	TRipGrepJsonMatchTest = class
		private
			FSearchParamMock : ISearchParams;
			FguiParams : IShared<TSearchTextWithOptions>;

		public
			[Setup]
			procedure Setup;
			[TearDown]
			procedure TearDown;

			// Test JSON begin line
			[Test]
			procedure ParseJsonBeginTest;

			// Test JSON match lines - basic test
			[Test]
			procedure ParseJsonMatchTestBasic;

			[Test]
			[TestCase('Separated matches', 'before one gap two end three,one,two,three,8,16,24')]
			[TestCase('Adjacent matches', 'onetwothree,one,two,three,1,4,7')]
			[TestCase('Repeated matches', 'test test test,test,test,test,1,6,11')]
			[TestCase('Unicode before matches', 'ä one 中文 two Ü three,one,two,three,3,10,16')]
			[TestCase('Emoji before matches', '🚀 one 🚀 two 🚀 three,one,two,three,4,11,18')]
			[TestCase('Unicode matches', 'ä 中文 🚀,ä,中文,🚀,1,3,6')]
			procedure ParseJsonMultipleSubmatchesTest(const _lineText, _firstMatch, _secondMatch, _thirdMatch : string;
				_firstColumn, _secondColumn, _thirdColumn : Integer);

			[Test]
			[TestCase('Four adjacent submatches', '4,False')]
			[TestCase('Ten separated submatches', '10,True')]
			[TestCase('One hundred separated submatches', '100,True')]
			procedure ParseJsonManySubmatchesTest(_matchCount : Integer; _separated : Boolean);

			[Test]
			procedure ParseJsonMultipleSubmatchesWithBytesTest;

			[Test]
			procedure ParseJsonSubmatchesResetTest;

			[Test]
			procedure ParseJsonZeroLengthSubmatchesTest;

			// Test JSON match lines with parameters and Unicode
			[Test]
			[TestCase('Basic ASCII Match', 'test.pas,test line,test,1,0,4')]
			[TestCase('German Umlauts', 'äöü.pas,Zeile mit Ümlauten,Ümlauten,2,10,19')]
			[TestCase('French Accents', 'café.pas,ligne avec accénts,accénts,3,11,19')]
			[TestCase('Cyrillic Text', 'файл.pas,строка с текстом,текстом,4,16,32')]
			[TestCase('Chinese Characters', '文件.pas,包含中文的行,中文,5,6,12')]
			[TestCase('Mixed Unicode', 'test_файл_中文.pas,Line with مختلف languages,مختلف,6,10,20')]
			[TestCase('Emoji in Path', '🔍search.pas,line with 🚀 emoji,🚀,7,10,14')]
			[TestCase('Special Characters', 'test@#$.pas,Line with @#$ symbols,@#$,8,10,13')]
			procedure ParseJsonMatchTest(const _fileName, _lineText, _matchText : string; _lineNumber, _start, _end : Integer);

			// Test JSON end line
			[Test]
			procedure ParseJsonEndTest;

			// Test JSON summary line
			[Test]
			procedure ParseJsonSummaryTest;

			// Test invalid JSON
			[Test]
			[TestCase('Invalid JSON 1', 'invalid json string')]
			[TestCase('Empty String', '')]
			[TestCase('Malformed JSON', '{"type":"match","data"]')]
			procedure ParseJsonErrorTest(const _s : string);

			// Test complete JSON sequence
			[Test]
			procedure ParseJsonSequenceTest;

			// Test bytes field handling for invalid UTF-8
			[Test]
			procedure ParseJsonMatchWithTextFieldTest;

			[Test]
			procedure ParseJsonMatchWithValidBytesTest;

			[Test]
			procedure ParseJsonMatchWithInvalidBytesTest;

			// Test no output scenario
			[Test]
			procedure ParseNoOutputTest;

			// Test rg.exe error scenario
			[Test]
			procedure ParseRgErrorTest;

			// Test rg: prefix error from ripgrep
			[Test]
			procedure ParseRgPrefixErrorTest;
	end;

implementation

uses
	RipGrepper.Common.Constants,
	System.Classes,
	System.JSON,
	System.SysUtils,
	RipGrepper.Parsers.JsonMatchLine,
	RipGrepper.Common.ParsedObject,
	DUnitX.Utils;

const
	COLUMN_NUM = 7;

	{ TSearchParamMock }

constructor TSearchParamMock.Create(const _guiParams : IShared<TSearchTextWithOptions>);
begin
	inherited Create();
	FGuiSearchParams := _guiParams;
end;

function TSearchParamMock.GetGuiSearchParams() : IShared<TSearchTextWithOptions>;
begin
	Result := FGuiSearchParams;
end;

procedure TRipGrepJsonMatchTest.Setup;
begin
	FguiParams := Shared.Make<TSearchTextWithOptions>(TSearchTextWithOptions.Create('search_text', []));
	FSearchParamMock := TSearchParamMock.Create(FguiParams);
end;

procedure TRipGrepJsonMatchTest.TearDown;
begin
	FSearchParamMock := nil;
	FguiParams := nil;
end;

procedure TRipGrepJsonMatchTest.ParseJsonBeginTest;
var
	parser : TJsonMatchLineParser;
	pr : IParsedObjectRow;
	testLine : string;
begin
	testLine := '{"type":"begin","data":{"path":{"text":"test.pas"}}}';

	parser := TJsonMatchLineParser.Create();
	try
		parser.SearchParams := FSearchParamMock;
		parser.ParseLine(0, testLine);
		pr := parser.ParseResult;

		Assert.IsFalse(pr.IsError, 'JSON begin line should not have errors: ' + pr.ErrorText);
		Assert.AreEqual(COLUMN_NUM, pr.Columns.Count, 'Expected columns for begin line');
		Assert.AreEqual('test.pas', pr.Columns[Integer(ciFile)].Text, 'File path not correctly extracted');
		Assert.IsTrue(pr.Columns[Integer(ciText)].Text.Contains('Begin file'), 'Begin line text should contain "Begin file"');
	finally
		parser.Free;
	end;
end;

procedure TRipGrepJsonMatchTest.ParseJsonMatchTest(const _fileName, _lineText, _matchText : string; _lineNumber, _start, _end : Integer);
var
	parser : TJsonMatchLineParser;
	pr : IParsedObjectRow;
	testLine : string;
begin
	// Build JSON string with provided parameters
	testLine := Format('{"type":"match","data":{"path":{"text":"%s"},"lines":{"text":"%s"},' +
		'"line_number":%d,"submatches":[{"match":{"text":"%s"},"start":%d,"end":%d}]}}', [_fileName, _lineText, _lineNumber, _matchText,
		_start, _end]);

	parser := TJsonMatchLineParser.Create();
	try
		parser.SearchParams := FSearchParamMock;
		parser.ParseLine(0, testLine);
		pr := parser.ParseResult;

		Assert.IsFalse(pr.IsError, 'JSON match line should not have errors: ' + pr.ErrorText);
		Assert.AreEqual(COLUMN_NUM, pr.Columns.Count, 'Expected columns for match line');

		// Check if file path is extracted
		Assert.AreEqual(_fileName, pr.Columns[Integer(ciFile)].Text, 'File path should be ' + _fileName);

		// Check if line number is extracted
		Assert.AreEqual(IntToStr(_lineNumber), pr.Columns[Integer(ciRow)].Text, 'Line number should be ' + IntToStr(_lineNumber));

		// Parser converts byte positions to character positions, so we need to calculate expected character position
		// The _start and _end parameters are now byte positions
		var
		expectedCharPos := 0;
		var
		byteCount := 0;
		for var i := 1 to Length(_lineText) do begin
			if byteCount >= _start then begin
				expectedCharPos := i - 1; // Convert to 0-based
				break;
			end;
			Inc(byteCount, TEncoding.UTF8.GetByteCount(_lineText[i]));
		end;
		
		Assert.AreEqual(IntToStr(expectedCharPos + 1), pr.Columns[Integer(ciColBegin)].Text, 'Column should be ' + IntToStr(expectedCharPos + 1));

		// Check if match text is extracted
		Assert.AreEqual(_matchText, pr.Columns[Integer(ciMatchText)].Text, 'Match text should be "' + _matchText + '"');

		// Additional check: verify text contains the text before match text (using character position)
		var
		beforeMatch := Copy(_lineText, 1, expectedCharPos);
		Assert.AreEqual(beforeMatch, pr.Columns[Integer(ciText)].Text, 'Before match text should be "' + beforeMatch + '"');
	finally
		parser.Free;
	end;
end;

procedure TRipGrepJsonMatchTest.ParseJsonMatchTestBasic;
var
	parser : TJsonMatchLineParser;
	pr : IParsedObjectRow;
	testLine : string;
begin
	// Use a shorter JSON string that fits in 255 characters
	testLine := '{"type":"match","data":{"path":{"text":"test.pas"},"lines":{"text":"test line"},' +
		'"line_number":1,"submatches":[{"match":{"text":"test"},"start":0,"end":4}]}}';

	parser := TJsonMatchLineParser.Create();
	try
		parser.SearchParams := FSearchParamMock;
		parser.ParseLine(0, testLine);
		pr := parser.ParseResult;

		Assert.IsFalse(pr.IsError, 'JSON match line should not have errors: ' + pr.ErrorText);
		Assert.AreEqual(COLUMN_NUM, pr.Columns.Count, 'Expected columns for match line');

		// Check if file path is extracted
		Assert.AreEqual('test.pas', pr.Columns[Integer(ciFile)].Text, 'File path should be test.pas');

		// Check if line number is extracted
		Assert.AreEqual('1', pr.Columns[Integer(ciRow)].Text, 'Line number should be 1');
		// Check if column is extracted (should be start position + 1)
		Assert.AreEqual('1', pr.Columns[Integer(ciColBegin)].Text, 'Column should be 1');
		// Check if match text is extracted
		Assert.AreEqual('test', pr.Columns[Integer(ciMatchText)].Text, 'Match text should be "test"');
	finally
		parser.Free;
	end;
end;

procedure TRipGrepJsonMatchTest.ParseJsonMultipleSubmatchesTest(const _lineText, _firstMatch, _secondMatch,
	_thirdMatch : string; _firstColumn, _secondColumn, _thirdColumn : Integer);
var
	parser : ISearchResultLineParser;
	jsonObj, dataObj : TJSONObject;
	submatches : TJSONArray;
	matchTexts : TArray<string>;
	columns : TArray<Integer>;
begin
	matchTexts := [_firstMatch, _secondMatch, _thirdMatch];
	columns := [_firstColumn, _secondColumn, _thirdColumn];
	jsonObj := TJSONObject.Create;
	try
		jsonObj.AddPair('type', 'match');
		dataObj := TJSONObject.Create;
		jsonObj.AddPair('data', dataObj);
		dataObj.AddPair('path', TJSONObject.Create.AddPair('text', 'test.pas'));
		dataObj.AddPair('lines', TJSONObject.Create.AddPair('text', _lineText));
		dataObj.AddPair('line_number', TJSONNumber.Create(42));
		submatches := TJSONArray.Create;
		dataObj.AddPair('submatches', submatches);
		for var i := 0 to High(matchTexts) do begin
			var startByte := TEncoding.UTF8.GetByteCount(Copy(_lineText, 1, columns[i] - 1));
			submatches.AddElement(TJSONObject.Create
				.AddPair('match', TJSONObject.Create.AddPair('text', matchTexts[i]))
				.AddPair('start', TJSONNumber.Create(startByte))
				.AddPair('end', TJSONNumber.Create(startByte + TEncoding.UTF8.GetByteCount(matchTexts[i]))));
		end;

		parser := TJsonMatchLineParser.Create();
		parser.SearchParams := FSearchParamMock;
		parser.ParseLine(7, jsonObj.ToJSON);
		var results := parser.ParseResults;
		Assert.AreEqual(3, Length(results), 'Every submatch should produce a result row');
		Assert.IsTrue(parser.ParseResult = results[0], 'ParseResult should remain the first submatch');
		for var i := 0 to High(results) do begin
			var row := results[i];
			Assert.IsFalse(row.IsError, row.ErrorText);
			Assert.AreEqual(COLUMN_NUM, row.Columns.Count);
			Assert.AreEqual('test.pas', row.FilePath);
			Assert.AreEqual(42, row.Row);
			Assert.AreEqual(7, row.ParsedRowNr);
			Assert.AreEqual(columns[i], row.ColBegin, 'Each match should have its own start column');
			Assert.AreEqual(columns[i] + Length(matchTexts[i]), row.ColEnd);
			Assert.AreEqual(matchTexts[i], row.GetColumnText(ciMatchText));
			Assert.AreEqual(Copy(_lineText, 1, columns[i] - 1), row.GetColumnText(ciText));
			Assert.AreEqual(Copy(_lineText, columns[i] + Length(matchTexts[i]), MaxInt), row.GetColumnText(ciTextAfterMatch));
			Assert.AreEqual(_lineText, row.GetColumnText(ciText) + row.GetColumnText(ciMatchText)
				+ row.GetColumnText(ciTextAfterMatch), 'Each result should preserve the full source line');
		end;
	finally
		jsonObj.Free;
	end;
end;

procedure TRipGrepJsonMatchTest.ParseJsonManySubmatchesTest(_matchCount : Integer; _separated : Boolean);
var
	parser : ISearchResultLineParser;
	jsonObj, dataObj : TJSONObject;
	submatches : TJSONArray;
	matchTexts : TArray<string>;
	columns : TArray<Integer>;
	lineText : string;
begin
	SetLength(matchTexts, _matchCount);
	SetLength(columns, _matchCount);
	lineText := 'ä🚀 before ';
	jsonObj := TJSONObject.Create;
	try
		jsonObj.AddPair('type', 'match');
		dataObj := TJSONObject.Create;
		jsonObj.AddPair('data', dataObj);
		dataObj.AddPair('path', TJSONObject.Create.AddPair('text', 'many.pas'));
		dataObj.AddPair('line_number', TJSONNumber.Create(23));
		submatches := TJSONArray.Create;
		dataObj.AddPair('submatches', submatches);
		for var i := 0 to _matchCount - 1 do begin
			if _separated and (i > 0) then begin
				lineText := lineText + ' | ';
			end;
			case i mod 3 of
				0 : matchTexts[i] := 'match' + IntToStr(i + 1);
				1 : matchTexts[i] := '中文' + IntToStr(i + 1);
				2 : matchTexts[i] := '🚀' + IntToStr(i + 1);
			end;
			columns[i] := Length(lineText) + 1;
			var startByte := TEncoding.UTF8.GetByteCount(lineText);
			submatches.AddElement(TJSONObject.Create
				.AddPair('match', TJSONObject.Create.AddPair('text', matchTexts[i]))
				.AddPair('start', TJSONNumber.Create(startByte))
				.AddPair('end', TJSONNumber.Create(startByte + TEncoding.UTF8.GetByteCount(matchTexts[i]))));
			lineText := lineText + matchTexts[i];
		end;
		lineText := lineText + ' after  ' + CRLF;
		dataObj.AddPair('lines', TJSONObject.Create.AddPair('text', lineText));

		parser := TJsonMatchLineParser.Create();
		parser.SearchParams := FSearchParamMock;
		parser.ParseLine(11, jsonObj.ToJSON);
		var results := parser.ParseResults;
		Assert.AreEqual(_matchCount, Length(results), 'All submatches should be returned');
		Assert.IsTrue(parser.ParseResult = results[0], 'ParseResult should remain the first submatch');
		for var i := 0 to High(results) do begin
			var row := results[i];
			var message := Format('Submatch %d of %d', [i + 1, _matchCount]);
			Assert.IsFalse(row.IsError, message + ': ' + row.ErrorText);
			Assert.AreEqual(COLUMN_NUM, row.Columns.Count, message);
			Assert.AreEqual('many.pas', row.FilePath, message);
			Assert.AreEqual(23, row.Row, message);
			Assert.AreEqual(11, row.ParsedRowNr, message);
			Assert.AreEqual(columns[i], row.ColBegin, message);
			Assert.AreEqual(columns[i] + Length(matchTexts[i]), row.ColEnd, message);
			Assert.AreEqual(matchTexts[i], row.GetColumnText(ciMatchText), message);
			Assert.AreEqual(Copy(lineText, 1, columns[i] - 1), row.GetColumnText(ciText), message);
			Assert.AreEqual(Copy(lineText, columns[i] + Length(matchTexts[i]), MaxInt),
				row.GetColumnText(ciTextAfterMatch), message);
		end;
	finally
		jsonObj.Free;
	end;
end;

procedure TRipGrepJsonMatchTest.ParseJsonMultipleSubmatchesWithBytesTest;
var
	parser : ISearchResultLineParser;
begin
	parser := TJsonMatchLineParser.Create();
	parser.ParseLine(0, '{"type":"match","data":{"path":{"text":"binary.bin"},' +
		'"lines":{"bytes":"YmluYXJ5IGRhdGE="},"line_number":2,"submatches":[' +
		'{"match":{"text":"binary"},"start":0,"end":6},{"match":{"text":"data"},"start":7,"end":11}]}}');
	var results := parser.ParseResults;
	Assert.AreEqual(2, Length(results));
	Assert.IsFalse(results[0].IsError, results[0].ErrorText);
	Assert.IsFalse(results[1].IsError, results[1].ErrorText);
	Assert.AreEqual('binary', results[0].GetColumnText(ciMatchText));
	Assert.AreEqual('data', results[1].GetColumnText(ciMatchText));
	Assert.AreEqual(1, results[0].ColBegin);
	Assert.AreEqual(7, results[0].ColEnd);
	Assert.AreEqual(8, results[1].ColBegin);
	Assert.AreEqual(12, results[1].ColEnd);
	for var row in results do begin
		Assert.AreEqual('binary data', row.GetColumnText(ciText) + row.GetColumnText(ciMatchText)
			+ row.GetColumnText(ciTextAfterMatch));
	end;
end;

procedure TRipGrepJsonMatchTest.ParseJsonSubmatchesResetTest;
var
	parser : ISearchResultLineParser;
begin
	parser := TJsonMatchLineParser.Create();
	parser.ParseLine(0, '{"type":"match","data":{"path":{"text":"test.pas"},"lines":{"text":"test test"},' +
		'"line_number":1,"submatches":[{"start":0,"end":4},{"start":5,"end":9}]}}');
	var previousResults := parser.ParseResults;
	Assert.AreEqual(2, Length(previousResults));

	parser.ParseLine(1, '{"type":"context","data":{"path":{"text":"test.pas"},"lines":{"text":"context"},' +
		'"line_number":2,"submatches":[]}}');
	Assert.AreEqual(1, Length(parser.ParseResults), 'Context should not retain earlier matches');
	Assert.AreEqual('context', parser.ParseResult.GetColumnText(ciText));
	Assert.AreEqual('test', previousResults[0].GetColumnText(ciMatchText), 'Previous results should remain unchanged');

	parser.ParseLine(2, 'invalid JSON');
	Assert.AreEqual(1, Length(parser.ParseResults), 'Errors should not retain earlier matches');
	Assert.IsTrue(parser.ParseResults[0].IsError);

	parser.ParseLine(3, '{"type":"match","data":{"path":{"text":"test.pas"},"lines":{"text":"no match"},' +
		'"line_number":3,"submatches":[]}}');
	Assert.AreEqual(1, Length(parser.ParseResults));
	Assert.IsFalse(parser.ParseResult.IsError, 'A valid line should clear the previous error');
	Assert.AreEqual('no match', parser.ParseResult.GetColumnText(ciText));
	Assert.AreEqual('', parser.ParseResult.GetColumnText(ciMatchText));
end;

procedure TRipGrepJsonMatchTest.ParseJsonZeroLengthSubmatchesTest;
var
	parser : ISearchResultLineParser;
begin
	parser := TJsonMatchLineParser.Create();
	parser.ParseLine(0, '{"type":"match","data":{"path":{"text":"test.pas"},"lines":{"text":"abc"},' +
		'"line_number":1,"submatches":[{"start":0,"end":0},{"start":3,"end":3}]}}');
	var results := parser.ParseResults;
	Assert.AreEqual(2, Length(results));
	Assert.AreEqual(1, results[0].ColBegin);
	Assert.AreEqual(4, results[1].ColBegin);
	for var row in results do begin
		Assert.IsFalse(row.IsError, row.ErrorText);
		Assert.AreEqual(row.ColBegin, row.ColEnd);
		Assert.AreEqual('', row.GetColumnText(ciMatchText));
		Assert.AreEqual('abc', row.GetColumnText(ciText) + row.GetColumnText(ciTextAfterMatch));
	end;
end;

procedure TRipGrepJsonMatchTest.ParseJsonEndTest;
var
	parser : TJsonMatchLineParser;
	pr : IParsedObjectRow;
	testLine : string;
begin
	testLine := '{"type":"end","data":{"path":{"text":"test.pas"},' + '"stats":{"matches":4,"matched_lines":3}}}';

	parser := TJsonMatchLineParser.Create();
	try
		parser.SearchParams := FSearchParamMock;
		parser.ParseLine(0, testLine);
		pr := parser.ParseResult;

		Assert.IsFalse(pr.IsError, 'JSON end line should not have errors: ' + pr.ErrorText);
		Assert.IsTrue(pr.IsStatsLine, 'End line should be marked as stats line');
		Assert.AreEqual(COLUMN_NUM, pr.Columns.Count, 'Expected columns for end line');
		Assert.AreEqual(RG_STATS_LINE, pr.Columns[Integer(ciFile)].Text, 'End line should have RG_STATS_LINE as file');
		Assert.IsTrue(pr.Columns[Integer(ciText)].Text.Contains('End file'), 'End line text should contain "End file"');
		Assert.IsTrue(pr.Columns[Integer(ciText)].Text.Contains('Matches:'), 'End line text should contain match statistics');
	finally
		parser.Free;
	end;
end;

procedure TRipGrepJsonMatchTest.ParseJsonSummaryTest;
var
	parser : TJsonMatchLineParser;
	pr : IParsedObjectRow;
	testLine : string;
begin
	testLine := '{"type":"summary","data":{"stats":{"matches":4,"matched_lines":3,"searches":1}}}';

	parser := TJsonMatchLineParser.Create();
	try
		parser.SearchParams := FSearchParamMock;
		parser.ParseLine(0, testLine);
		pr := parser.ParseResult;

		Assert.IsFalse(pr.IsError, 'JSON summary line should not have errors: ' + pr.ErrorText);
		Assert.IsTrue(pr.IsStatsLine, 'Summary line should be marked as stats line');
		Assert.AreEqual(COLUMN_NUM, pr.Columns.Count, 'Expected columns for summary line');
		Assert.AreEqual(RG_STATS_LINE, pr.Columns[Integer(ciFile)].Text, 'Summary line should have RG_STATS_LINE as file');
		Assert.IsTrue(pr.Columns[Integer(ciText)].Text.Contains('Summary:'), 'Summary line text should contain "Summary:"');
		Assert.IsTrue(pr.Columns[Integer(ciText)].Text.Contains('searches'), 'Summary line text should contain search statistics');
	finally
		parser.Free;
	end;
end;

procedure TRipGrepJsonMatchTest.ParseJsonErrorTest(const _s : string);
var
	parser : TJsonMatchLineParser;
	pr : IParsedObjectRow;
begin
	parser := TJsonMatchLineParser.Create();
	try
		parser.SearchParams := FSearchParamMock;
		parser.ParseLine(0, _s);
		pr := parser.ParseResult;

		Assert.IsTrue(pr.IsError, 'Invalid JSON should result in an error: ' + _s);
		Assert.IsFalse(pr.ErrorText.IsEmpty, 'Error text should not be empty for invalid JSON');
	finally
		parser.Free;
	end;
end;

procedure TRipGrepJsonMatchTest.ParseJsonSequenceTest;
var
	parser : TJsonMatchLineParser;
	pr : IParsedObjectRow;
	testLines : TArray<string>;
	i : Integer;
begin
	// Test a complete sequence with shorter test data
	testLines := ['{"type":"begin","data":{"path":{"text":"Test.pas"}}}',
		'{"type":"match","data":{"path":{"text":"Test.pas"},"lines":{"text":"test line"},' +
		'"line_number":1,"submatches":[{"match":{"text":"test"},"start":0,"end":4}]}}',
		'{"type":"match","data":{"path":{"text":"Test.pas"},"lines":{"text":"another test"},' +
		'"line_number":2,"submatches":[{"match":{"text":"test"},"start":8,"end":12}]}}', '{"type":"end","data":{"path":{"text":"Test.pas"},'
		+ '"stats":{"matches":2,"matched_lines":2}}}', '{"type":"summary","data":{"stats":{"matches":2,"matched_lines":2,"searches":1}}}'];

	parser := TJsonMatchLineParser.Create();
	try
		parser.SearchParams := FSearchParamMock;

		for i := 0 to high(testLines) do begin
			parser.ParseLine(i, testLines[i]);
			pr := parser.ParseResult;

			Assert.IsFalse(pr.IsError, Format('Line %d should not have errors: %s' + CRLF + 'Line: %s', [i, pr.ErrorText, testLines[i]]));

			Assert.AreEqual(COLUMN_NUM, pr.Columns.Count, Format('Line %d should have %d columns, got %d',
				[i, COLUMN_NUM, pr.Columns.Count]));
		end;
	finally
		parser.Free;
	end;
end;

procedure TRipGrepJsonMatchTest.ParseJsonMatchWithTextFieldTest;
var
	parser : TJsonMatchLineParser;
	pr : IParsedObjectRow;
	testLineWithText : string;
begin
	// Test normal text field
	testLineWithText := '{"type":"match","data":{"path":{"text":"test.pas"},"lines":{"text":"normal text"},' +
		'"line_number":1,"submatches":[{"match":{"text":"text"},"start":7,"end":11}]}}';

	parser := TJsonMatchLineParser.Create();
	try
		parser.SearchParams := FSearchParamMock;
		parser.ParseLine(0, testLineWithText);
		pr := parser.ParseResult;

		Assert.IsFalse(pr.IsError, 'JSON match line with text should not have errors: ' + pr.ErrorText);
		Assert.AreEqual(COLUMN_NUM, pr.Columns.Count, 'Expected columns for match line');
		Assert.AreEqual('normal text', pr.Columns[Integer(ciText)].Text + pr.Columns[Integer(ciMatchText)].Text + 
			pr.Columns[Integer(ciTextAfterMatch)].Text, 'Full line text should be preserved');
	finally
		parser.Free;
	end;
end;

procedure TRipGrepJsonMatchTest.ParseJsonMatchWithValidBytesTest;
var
	parser : TJsonMatchLineParser;
	pr : IParsedObjectRow;
	testLineWithBytes : string;
	fullLineText : string;
begin
	// Test bytes field (base64 encoded "binary data")
	// "YmluYXJ5IGRhdGE=" is base64 for "binary data"
	testLineWithBytes := '{"type":"match","data":{"path":{"text":"binary.bin"},"lines":{"bytes":"YmluYXJ5IGRhdGE="},' +
		'"line_number":2,"submatches":[{"match":{"text":"data"},"start":7,"end":11}]}}';

	parser := TJsonMatchLineParser.Create();
	try
		parser.SearchParams := FSearchParamMock;
		parser.ParseLine(0, testLineWithBytes);
		pr := parser.ParseResult;

		Assert.IsFalse(pr.IsError, 'JSON match line with bytes should not have errors: ' + pr.ErrorText);
		Assert.AreEqual(COLUMN_NUM, pr.Columns.Count, 'Expected columns for match line with bytes');
		
		// The full line text should be the decoded base64 content
		fullLineText := pr.Columns[Integer(ciText)].Text + pr.Columns[Integer(ciMatchText)].Text + 
			pr.Columns[Integer(ciTextAfterMatch)].Text;
		Assert.AreEqual('binary data', fullLineText, 'Bytes field should be decoded from base64');
	finally
		parser.Free;
	end;
end;

procedure TRipGrepJsonMatchTest.ParseJsonMatchWithInvalidBytesTest;
var
	parser : TJsonMatchLineParser;
	pr : IParsedObjectRow;
	testLineWithInvalidBytes : string;
	fullLineText : string;
begin
	// Test invalid base64 - should use fallback format when empty bytes or exception
	testLineWithInvalidBytes := '{"type":"match","data":{"path":{"text":"invalid.bin"},"lines":{"bytes":"invalid-base64!"},' +
		'"line_number":3,"submatches":[{"match":{"text":"data"},"start":0,"end":4}]}}';

	parser := TJsonMatchLineParser.Create();
	try
		parser.SearchParams := FSearchParamMock;
		parser.ParseLine(0, testLineWithInvalidBytes);
		pr := parser.ParseResult;

		Assert.IsFalse(pr.IsError, 'JSON match line with invalid bytes should not have errors: ' + pr.ErrorText);
		fullLineText := pr.Columns[Integer(ciText)].Text + pr.Columns[Integer(ciMatchText)].Text + 
			pr.Columns[Integer(ciTextAfterMatch)].Text;
		
		// Invalid base64 should either decode to something or use fallback format
		Assert.IsTrue(Length(fullLineText) > 0, 'Invalid base64 should produce some result');
		// If it uses fallback format, it should contain the base64 string
		if fullLineText.StartsWith('<base64:') then begin
			Assert.IsTrue(fullLineText.EndsWith('>'), 'Fallback format should end with >');
			Assert.IsTrue(fullLineText.Contains('invalid-base64!'), 'Fallback should contain original base64 string');
		end;
	finally
		parser.Free;
	end;
end;

procedure TRipGrepJsonMatchTest.ParseNoOutputTest;
var
	parser : TJsonMatchLineParser;
	pr : IParsedObjectRow;
	testLine : string;
begin
	// Test the special "no output" line that rg.exe sends when there are no matches
	testLine := 'rg.exe' + RG_HAS_NO_OUTPUT;

	parser := TJsonMatchLineParser.Create();
	try
		parser.SearchParams := FSearchParamMock;
		parser.ParseLine(0, testLine);
		pr := parser.ParseResult;

		Assert.IsTrue(pr.IsError, 'No output line is treated as an error by the parser: ' + pr.ErrorText);
		Assert.AreEqual(COLUMN_NUM, pr.Columns.Count, 'Expected columns for no output line');
		Assert.AreEqual(testLine, pr.Columns[Integer(ciFile)].Text, 'File column should contain the no output message');
	finally
		parser.Free;
	end;
end;

procedure TRipGrepJsonMatchTest.ParseRgErrorTest;
var
	parser : TJsonMatchLineParser;
	pr : IParsedObjectRow;
	testLine : string;
begin
	// Test the special error line that rg.exe sends when it fails
	testLine := 'rg.exe' + RG_ENDED_ERROR + '2';

	parser := TJsonMatchLineParser.Create();
	try
		parser.SearchParams := FSearchParamMock;
		parser.ParseLine(0, testLine);
		pr := parser.ParseResult;

		Assert.IsTrue(pr.IsError, 'RG error line is treated as a parse error by the parser: ' + pr.ErrorText);
		Assert.AreEqual(COLUMN_NUM, pr.Columns.Count, 'Expected columns for rg error line');
		Assert.AreEqual(testLine, pr.Columns[Integer(ciFile)].Text, 'File column should contain the error message');
	finally
		parser.Free;
	end;
end;

procedure TRipGrepJsonMatchTest.ParseRgPrefixErrorTest;
var
	parser : TJsonMatchLineParser;
	pr : IParsedObjectRow;
	testLine : string;
begin
	// Test error messages from ripgrep that start with "rg:" prefix
	testLine := RG_ERROR_MSG_PREFIX + ' some error message from ripgrep';

	parser := TJsonMatchLineParser.Create();
	try
		parser.SearchParams := FSearchParamMock;
		parser.ParseLine(0, testLine);
		pr := parser.ParseResult;

		Assert.IsTrue(pr.IsError, 'RG prefix error is treated as a parse error by the parser: ' + pr.ErrorText);
		Assert.AreEqual(COLUMN_NUM, pr.Columns.Count, 'Expected columns for rg prefix error line');
		Assert.AreEqual(testLine, pr.Columns[Integer(ciFile)].Text, 'File column should contain the error message');
	finally
		parser.Free;
	end;
end;

initialization

TDUnitX.RegisterTestFixture(TRipGrepJsonMatchTest);

end.
