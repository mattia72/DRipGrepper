unit RipGrepper.Helper.StreamReaderWriter.Test;

interface

uses
	DUnitX.TestFramework,
	System.Classes,
	System.SysUtils,
	RipGrepper.Common.Constants;

type

	[TestFixture]
	TStreamReaderWriterHelperTest = class
		private
			FMemoryStream : TMemoryStream;
			FStreamWriter : TStreamWriter;
			FStreamReader : TStreamReader;

		public
			[Setup]
			procedure Setup;
			[TearDown]
			procedure TearDown;

			[Test]
			procedure TestWriteReadBool;
			[Test]
			procedure TestWriteReadInteger;
			[Test]
			procedure TestWriteReadString;
			[Test]
			procedure TestWriteReadStringWithCRLF;
			[Test]
			procedure TestWriteReadStringWithBackslashes;
			[Test]
			procedure TestWriteReadStringWithMixedLineEndings;
			[Test]
			procedure TestWriteReadEmptyString;
			[Test]
			procedure TestWriteReadStringAllowEmpty;
			[Test]
			procedure TestReadEmptyStringThrowsException;
			[Test]
			procedure TestWriteEmptyStringThrowsException;
			[Test]
			procedure TestComplexStringWithAllSpecialChars;
			[Test]
			procedure TestMultipleItemsRoundTrip;
		private
			procedure ResetStreamForReading;
			function ReadAllStreamContent: string;
	end;

implementation

uses
	RipGrepper.Helper.StreamReaderWriter,
	RipGrepper.Tools.DebugUtils;

{ TStreamReaderWriterHelperTest }

procedure TStreamReaderWriterHelperTest.Setup;
begin
	FMemoryStream := TMemoryStream.Create;
	FStreamWriter := TStreamWriter.Create(FMemoryStream);
	// Note: StreamReader will be created in ResetStreamForReading when needed
	FStreamReader := nil;
end;

procedure TStreamReaderWriterHelperTest.TearDown;
begin
	FStreamReader.Free;
	FStreamWriter.Free;
	FMemoryStream.Free;
end;

procedure TStreamReaderWriterHelperTest.ResetStreamForReading;
begin
	// Flush writer and reset stream position for reading
	FStreamWriter.Flush;
	FMemoryStream.Position := 0;
	
	// Free old reader and create new one
	FStreamReader.Free;
	FStreamReader := TStreamReader.Create(FMemoryStream);
end;

function TStreamReaderWriterHelperTest.ReadAllStreamContent: string;
begin
	FStreamWriter.Flush;
	FMemoryStream.Position := 0;
	
	var tempReader := TStreamReader.Create(FMemoryStream);
	try
		Result := tempReader.ReadToEnd;
	finally
		tempReader.Free;
	end;
	
	// Reset position for normal reading
	FMemoryStream.Position := 0;
end;

procedure TStreamReaderWriterHelperTest.TestWriteReadBool;
begin
	// Test True
	FStreamWriter.WriteLineAsBool(True, 'test bool true');
	ResetStreamForReading;
	Assert.IsTrue(FStreamReader.ReadLineAsBool('test bool true'), 'Should read True correctly');
	
	// Reset and test False
	FMemoryStream.SetSize(0);
	FMemoryStream.Position := 0;
	FStreamWriter.WriteLineAsBool(False, 'test bool false');
	ResetStreamForReading;
	Assert.IsFalse(FStreamReader.ReadLineAsBool('test bool false'), 'Should read False correctly');
end;

procedure TStreamReaderWriterHelperTest.TestWriteReadInteger;
var
	testValues: array[0..4] of Integer;
	i: Integer;
begin
	testValues[0] := 0;
	testValues[1] := 42;
	testValues[2] := -123;
	testValues[3] := MaxInt;
	testValues[4] := Low(Integer);
	
	for i := Low(testValues) to High(testValues) do begin
		FMemoryStream.SetSize(0);
		FMemoryStream.Position := 0;
		
		FStreamWriter.WriteLineAsInteger(testValues[i], 'test integer');
		ResetStreamForReading;
		
		Assert.AreEqual(testValues[i], FStreamReader.ReadLineAsInteger('test integer'),
			Format('Should read integer %d correctly', [testValues[i]]));
	end;
end;

procedure TStreamReaderWriterHelperTest.TestWriteReadString;
const
	TEST_STRING = 'Hello, World!';
begin
	FStreamWriter.WriteLineAsString(TEST_STRING, False, 'test string');
	ResetStreamForReading;
	
	var result := FStreamReader.ReadLineAsString(False, 'test string');
	Assert.AreEqual(TEST_STRING, result, 'Should read string correctly');
end;

procedure TStreamReaderWriterHelperTest.TestWriteReadStringWithCRLF;
const
	TEST_STRING = 'Line 1' + CRLF + 'Line 2' + CRLF + 'Line 3';
begin
	FStreamWriter.WriteLineAsString(TEST_STRING, False, 'test string with CRLF');
	ResetStreamForReading;
	
	var result := FStreamReader.ReadLineAsString(False, 'test string with CRLF');
	Assert.AreEqual(TEST_STRING, result, 'Should preserve CRLF in string content');
end;

procedure TStreamReaderWriterHelperTest.TestWriteReadStringWithBackslashes;
const
	TEST_STRING = 'Path: C:\folder\file.txt and regex: \d+\.\w*';
begin
	FStreamWriter.WriteLineAsString(TEST_STRING, False, 'test string with backslashes');
	ResetStreamForReading;
	
	var result := FStreamReader.ReadLineAsString(False, 'test string with backslashes');
	Assert.AreEqual(TEST_STRING, result, 'Should preserve backslashes in string content');
end;

procedure TStreamReaderWriterHelperTest.TestWriteReadStringWithMixedLineEndings;
const
	TEST_STRING = 'Line 1' + CR + 'Line 2' + LF + 'Line 3' + CRLF + 'Line 4';
begin
	FStreamWriter.WriteLineAsString(TEST_STRING, False, 'test mixed line endings');
	ResetStreamForReading;
	
	var result := FStreamReader.ReadLineAsString(False, 'test mixed line endings');
	Assert.AreEqual(TEST_STRING, result, 'Should preserve mixed line endings');
end;

procedure TStreamReaderWriterHelperTest.TestWriteReadEmptyString;
const
	EMPTY_STRING = '';
begin
	FStreamWriter.WriteLineAsString(EMPTY_STRING, True, 'test empty string allowed');
	ResetStreamForReading;
	
	var result := FStreamReader.ReadLineAsString(True, 'test empty string allowed');
	Assert.AreEqual(EMPTY_STRING, result, 'Should handle empty string when allowed');
end;

procedure TStreamReaderWriterHelperTest.TestWriteReadStringAllowEmpty;
const
	EMPTY_STRING = '';
begin
	FStreamWriter.WriteLineAsString(EMPTY_STRING, True, 'test empty string');
	ResetStreamForReading;
	
	var result := FStreamReader.ReadLineAsString(True, 'test empty string');
	Assert.AreEqual(EMPTY_STRING, result, 'Should read empty string when allowed');
end;

procedure TStreamReaderWriterHelperTest.TestReadEmptyStringThrowsException;
begin
	// Write empty line directly (simulating the issue scenario)
	FStreamWriter.WriteLine('');
	ResetStreamForReading;
	
	Assert.WillRaise(
		procedure
		begin
			FStreamReader.ReadLineAsString(False, 'test empty not allowed');
		end,
		Exception
	);
end;

procedure TStreamReaderWriterHelperTest.TestWriteEmptyStringThrowsException;
begin
	Assert.WillRaise(
		procedure
		begin
			FStreamWriter.WriteLineAsString('', False, 'test empty not allowed');
		end,
		Exception
	);
end;

procedure TStreamReaderWriterHelperTest.TestComplexStringWithAllSpecialChars;
const
	COMPLEX_STRING = 'Complex: \backslashes\ and' + CRLF + 'CRLF and' + CR + 'CR and' + LF + 'LF and \\escaped\\';
begin
	FStreamWriter.WriteLineAsString(COMPLEX_STRING, False, 'complex string');
	ResetStreamForReading;
	
	var result := FStreamReader.ReadLineAsString(False, 'complex string');
	Assert.AreEqual(COMPLEX_STRING, result, 'Should handle complex string with all special characters');
end;

procedure TStreamReaderWriterHelperTest.TestMultipleItemsRoundTrip;
var
	testStrings: array[0..4] of string;
	i: Integer;
	results: array[0..4] of string;
begin
	// Prepare test data
	testStrings[0] := 'Simple string';
	testStrings[1] := 'String with' + CRLF + 'CRLF';
	testStrings[2] := 'String with \ backslashes \d+';
	testStrings[3] := 'Mixed' + CR + 'line' + LF + 'endings' + CRLF + 'here';
	testStrings[4] := '';
	
	// Write all items
	FStreamWriter.WriteLineAsInteger(Length(testStrings), 'item count');
	for i := Low(testStrings) to High(testStrings) do begin
		FStreamWriter.WriteLineAsString(testStrings[i], True, Format('item %d', [i]));
	end;
	
	// Read all items back
	ResetStreamForReading;
	var itemCount := FStreamReader.ReadLineAsInteger('item count');
	Assert.AreEqual(Length(testStrings), itemCount, 'Should read correct item count');
	
	for i := 0 to itemCount - 1 do begin
		results[i] := FStreamReader.ReadLineAsString(True, Format('item %d', [i]));
	end;
	
	// Verify all items
	for i := Low(testStrings) to High(testStrings) do begin
		Assert.AreEqual(testStrings[i], results[i], 
			Format('Item %d should match: expected "%s", got "%s"', [i, testStrings[i], results[i]]));
	end;
end;

initialization
	TDUnitX.RegisterTestFixture(TStreamReaderWriterHelperTest);

end.