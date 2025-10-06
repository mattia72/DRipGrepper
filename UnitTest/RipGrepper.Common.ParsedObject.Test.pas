unit RipGrepper.Common.ParsedObject.Test;

interface

uses
	DUnitX.TestFramework,
	RipGrepper.Common.ParsedObject,
	RipGrepper.Common.SimpleTypes,
	RipGrepper.Common.Constants,
	Spring,
	System.Classes,
	System.SysUtils,
	ArrayEx;

type

	[TestFixture]
	TParsedObjectRowCollectionTest = class

		private

		public
			[Setup]
			procedure Setup;
			[TearDown]
			procedure TearDown;

			[Test]
			procedure TestSaveToStreamWriter_EmptyCollection();

			[Test]
			procedure TestSaveToStreamWriter_SingleItem();

			[Test]
			procedure TestSaveToStreamWriter_SingleItemNoOutput();

			[Test]
			procedure TestSaveToStreamWriter_MultipleItems();

			[Test]
			procedure TestSaveToStreamWriter_ItemsWithEmptyColumns();

			[Test]
			procedure TestLoadFromStreamReader_EmptyCollection();

			[Test]
			procedure TestLoadFromStreamReader_SingleItem();

			[Test]
			procedure TestLoadFromStreamReader_SingleItemNoOutput();

			[Test]
			procedure TestLoadFromStreamReader_MultipleItems();

			[Test]
			procedure TestLoadFromStreamReader_ItemsWithEmptyColumns();

			[Test]
			procedure TestRoundtripPersistence_EmptyCollection();

			[Test]
			procedure TestRoundtripPersistence_SingleItem();

			[Test]
			procedure TestRoundtripPersistence_MultipleItems();

			[Test]
			procedure TestRoundtripPersistence_ItemsWithEmptyColumns();

			[Test]
			procedure TestRoundtripPersistence_MixedContent();

			[Test]
			procedure TestStreamContent_DebugOutput();

		private
			function CreateSampleRow(const fileName : string; row : Integer; const matchText : string;
				parserType : TParserType = ptRipGrepSearch) : IParsedObjectRow;
			function CreateNoOutputRow() : IParsedObjectRow;
			function CreateRowWithEmptyColumns() : IParsedObjectRow;
			procedure DebugStreamContent(stream : TMemoryStream; const testName : string);
	end;

implementation

uses
	RipGrepper.Helper.StreamReaderWriter,
	RipGrepper.Tools.DebugUtils;

procedure TParsedObjectRowCollectionTest.Setup;
begin
	// Setup method - called before each test
end;

procedure TParsedObjectRowCollectionTest.TearDown;
begin
	// TearDown method - called after each test
end;

function TParsedObjectRowCollectionTest.CreateSampleRow(const fileName : string; row : Integer; const matchText : string;
	parserType : TParserType = ptRipGrepSearch) : IParsedObjectRow;
var
	columns : TArrayEx<TColumnData>;
begin
	Result := TParsedObjectRow.Create();
	Result.ParserType := parserType;

	columns.Clear();
	columns.Add(TColumnData.New(ciFile, fileName));
	columns.Add(TColumnData.New(ciRow, row.ToString));
	columns.Add(TColumnData.New(ciColBegin, '5'));
	columns.Add(TColumnData.New(ciColEnd, '10'));
	columns.Add(TColumnData.New(ciText, 'This is ' + matchText + ' in the text'));
	columns.Add(TColumnData.New(ciMatchText, matchText));
	columns.Add(TColumnData.New(ciTextAfterMatch, ' in the text'));

	Result.Columns := columns;
end;

function TParsedObjectRowCollectionTest.CreateNoOutputRow() : IParsedObjectRow;
var
	columns : TArrayEx<TColumnData>;
begin
	Result := TParsedObjectRow.Create();
	Result.ParserType := ptRipGrepSearch;

	columns.Clear();
	columns.Add(TColumnData.New(ciFile, 'rg.exe' + RG_HAS_NO_OUTPUT));
	columns.Add(TColumnData.New(ciRow, ''));
	columns.Add(TColumnData.New(ciColBegin, ''));
	columns.Add(TColumnData.New(ciColEnd, ''));
	columns.Add(TColumnData.New(ciText, ''));
	columns.Add(TColumnData.New(ciMatchText, ''));
	columns.Add(TColumnData.New(ciTextAfterMatch, ''));

	Result.Columns := columns;
end;

function TParsedObjectRowCollectionTest.CreateRowWithEmptyColumns() : IParsedObjectRow;
var
	columns : TArrayEx<TColumnData>;
begin
	Result := TParsedObjectRow.Create();
	Result.ParserType := ptRipGrepSearch;

	columns.Clear();
	columns.Add(TColumnData.New(ciFile, 'C:\Test\File.pas'));
	columns.Add(TColumnData.New(ciRow, '15'));
	columns.Add(TColumnData.New(ciColBegin, '')); // Empty column
	columns.Add(TColumnData.New(ciColEnd, '')); // Empty column
	columns.Add(TColumnData.New(ciText, 'Some text'));
	columns.Add(TColumnData.New(ciMatchText, '')); // Empty match text
	columns.Add(TColumnData.New(ciTextAfterMatch, '')); // Empty text after match

	Result.Columns := columns;
end;

procedure TParsedObjectRowCollectionTest.DebugStreamContent(stream : TMemoryStream; const testName : string);
var
	content : string;
	lines : TArray<string>;
	i : Integer;
begin
	stream.Position := 0;
	SetLength(content, stream.Size);
	if stream.Size > 0 then begin
		stream.ReadBuffer(content[1], stream.Size);
	end;

	lines := content.Split([#13#10, #10], TStringSplitOptions.None);

	// Debug output using TDebugMsgBeginEnd
	var dbgMsg := TDebugMsgBeginEnd.New('DebugStreamContent: ' + testName);
	dbgMsg.Msg('Stream size: ' + stream.Size.ToString + ' bytes');
	dbgMsg.Msg('Number of lines: ' + Length(lines).ToString);
	for i := 0 to High(lines) do begin
		dbgMsg.Msg(Format('Line %d: "%s"', [i, lines[i]]));
	end;
end;

procedure TParsedObjectRowCollectionTest.TestSaveToStreamWriter_EmptyCollection();
var
	collection : TParsedObjectRowCollection;
	stream : TMemoryStream;
	writer : TStreamWriter;
begin
	// Arrange
	collection := TParsedObjectRowCollection.Create();
	stream := TMemoryStream.Create();
	try
		writer := TStreamWriter.Create(stream, TEncoding.UTF8);
		try
			// Act
			collection.SaveToStreamWriter(writer);
			writer.Flush();

			// Debug
			DebugStreamContent(stream, 'EmptyCollection');

			// Assert
			Assert.IsTrue(stream.Size > 0, 'Stream should contain data even for empty collection');
		finally
			writer.Free();
		end;
	finally
		stream.Free();
		collection.Free();
	end;
end;

procedure TParsedObjectRowCollectionTest.TestSaveToStreamWriter_SingleItem();
var
	collection : TParsedObjectRowCollection;
	stream : TMemoryStream;
	writer : TStreamWriter;
	row : IParsedObjectRow;
begin
	// Arrange
	collection := TParsedObjectRowCollection.Create();
	row := CreateSampleRow('C:\Test\File1.pas', 10, 'pattern');
	collection.Items.Add(row);

	stream := TMemoryStream.Create();
	try
		writer := TStreamWriter.Create(stream, TEncoding.UTF8);
		try
			// Act
			collection.SaveToStreamWriter(writer);
			writer.Flush();

			// Debug
			DebugStreamContent(stream, 'SingleItem');

			// Assert
			Assert.IsTrue(stream.Size > 0, 'Stream should contain data for single item');
		finally
			writer.Free();
		end;
	finally
		stream.Free();
		collection.Free();
	end;
end;

procedure TParsedObjectRowCollectionTest.TestSaveToStreamWriter_SingleItemNoOutput();
var
	collection : TParsedObjectRowCollection;
	stream : TMemoryStream;
	writer : TStreamWriter;
	row : IParsedObjectRow;
begin
	// Arrange
	collection := TParsedObjectRowCollection.Create();
	row := CreateNoOutputRow();
	collection.Items.Add(row);

	stream := TMemoryStream.Create();
	try
		writer := TStreamWriter.Create(stream, TEncoding.UTF8);
		try
			// Act
			collection.SaveToStreamWriter(writer);
			writer.Flush();

			// Debug
			DebugStreamContent(stream, 'SingleItemNoOutput');

			// Assert
			Assert.IsTrue(stream.Size > 0, 'Stream should contain data for no output item');
		finally
			writer.Free();
		end;
	finally
		stream.Free();
		collection.Free();
	end;
end;

procedure TParsedObjectRowCollectionTest.TestSaveToStreamWriter_MultipleItems();
var
	collection : TParsedObjectRowCollection;
	stream : TMemoryStream;
	writer : TStreamWriter;
begin
	// Arrange
	collection := TParsedObjectRowCollection.Create();
	collection.Items.Add(CreateSampleRow('C:\Test\File1.pas', 10, 'pattern1'));
	collection.Items.Add(CreateSampleRow('C:\Test\File2.pas', 20, 'pattern2'));
	collection.Items.Add(CreateSampleRow('C:\Test\File3.pas', 30, 'pattern3'));

	stream := TMemoryStream.Create();
	try
		writer := TStreamWriter.Create(stream, TEncoding.UTF8);
		try
			// Act
			collection.SaveToStreamWriter(writer);
			writer.Flush();

			// Debug
			DebugStreamContent(stream, 'MultipleItems');

			// Assert
			Assert.IsTrue(stream.Size > 0, 'Stream should contain data for multiple items');
		finally
			writer.Free();
		end;
	finally
		stream.Free();
		collection.Free();
	end;
end;

procedure TParsedObjectRowCollectionTest.TestSaveToStreamWriter_ItemsWithEmptyColumns();
var
	collection : TParsedObjectRowCollection;
	stream : TMemoryStream;
	writer : TStreamWriter;
begin
	// Arrange
	collection := TParsedObjectRowCollection.Create();
	collection.Items.Add(CreateSampleRow('C:\Test\File1.pas', 10, 'pattern1'));
	collection.Items.Add(CreateRowWithEmptyColumns()); // This has empty columns
	collection.Items.Add(CreateSampleRow('C:\Test\File3.pas', 30, 'pattern3'));

	stream := TMemoryStream.Create();
	try
		writer := TStreamWriter.Create(stream, TEncoding.UTF8);
		try
			// Act
			collection.SaveToStreamWriter(writer);
			writer.Flush();

			// Debug
			DebugStreamContent(stream, 'ItemsWithEmptyColumns');

			// Assert
			Assert.IsTrue(stream.Size > 0, 'Stream should contain data for items with empty columns');
		finally
			writer.Free();
		end;
	finally
		stream.Free();
		collection.Free();
	end;
end;

procedure TParsedObjectRowCollectionTest.TestLoadFromStreamReader_EmptyCollection();
var
	originalCollection, loadedCollection : TParsedObjectRowCollection;
	stream : TMemoryStream;
	writer : TStreamWriter;
	reader : TStreamReader;
begin
	// Arrange
	originalCollection := TParsedObjectRowCollection.Create();
	stream := TMemoryStream.Create();
	try
		writer := TStreamWriter.Create(stream, TEncoding.UTF8);
		try
			originalCollection.SaveToStreamWriter(writer);
			writer.Flush();
		finally
			writer.Free();
		end;

		// Act
		stream.Position := 0;
		reader := TStreamReader.Create(stream, TEncoding.UTF8);
		loadedCollection := TParsedObjectRowCollection.Create();
		try
			loadedCollection.LoadFromStreamReader(reader);

			// Assert
			Assert.AreEqual(0, loadedCollection.Items.Count, 'Loaded collection should be empty');
		finally
			reader.Free();
			loadedCollection.Free();
		end;
	finally
		stream.Free();
		originalCollection.Free();
	end;
end;

procedure TParsedObjectRowCollectionTest.TestLoadFromStreamReader_SingleItem();
var
	originalCollection, loadedCollection : TParsedObjectRowCollection;
	stream : TMemoryStream;
	writer : TStreamWriter;
	reader : TStreamReader;
	row : IParsedObjectRow;
begin
	// Arrange
	originalCollection := TParsedObjectRowCollection.Create();
	row := CreateSampleRow('C:\Test\File1.pas', 10, 'pattern');
	originalCollection.Items.Add(row);

	stream := TMemoryStream.Create();
	try
		writer := TStreamWriter.Create(stream, TEncoding.UTF8);
		try
			originalCollection.SaveToStreamWriter(writer);
			writer.Flush();
		finally
			writer.Free();
		end;

		// Debug
		DebugStreamContent(stream, 'LoadSingleItem_BeforeLoad');

		// Act
		stream.Position := 0;
		reader := TStreamReader.Create(stream, TEncoding.UTF8);
		loadedCollection := TParsedObjectRowCollection.Create();
		try
			loadedCollection.LoadFromStreamReader(reader);

			// Assert
			Assert.AreEqual(1, loadedCollection.Items.Count, 'Loaded collection should have 1 item');
			Assert.AreEqual('C:\Test\File1.pas', loadedCollection.Items[0].FilePath, 'File path should match');
			Assert.AreEqual(10, loadedCollection.Items[0].Row, 'Row should match');
			Assert.AreEqual('pattern', loadedCollection.Items[0].GetColumnText(ciMatchText), 'Match text should match');
		finally
			reader.Free();
			loadedCollection.Free();
		end;
	finally
		stream.Free();
		originalCollection.Free();
	end;
end;

procedure TParsedObjectRowCollectionTest.TestLoadFromStreamReader_SingleItemNoOutput();
var
	originalCollection, loadedCollection : TParsedObjectRowCollection;
	stream : TMemoryStream;
	writer : TStreamWriter;
	reader : TStreamReader;
	row : IParsedObjectRow;
begin
	// Arrange
	originalCollection := TParsedObjectRowCollection.Create();
	row := CreateNoOutputRow();
	originalCollection.Items.Add(row);

	stream := TMemoryStream.Create();
	try
		writer := TStreamWriter.Create(stream, TEncoding.UTF8);
		try
			originalCollection.SaveToStreamWriter(writer);
			writer.Flush();
		finally
			writer.Free();
		end;

		// Debug
		DebugStreamContent(stream, 'LoadSingleItemNoOutput_BeforeLoad');

		// Act
		stream.Position := 0;
		reader := TStreamReader.Create(stream, TEncoding.UTF8);
		loadedCollection := TParsedObjectRowCollection.Create();
		try
			loadedCollection.LoadFromStreamReader(reader);

			// Assert - No output items should result in 0 items loaded
			Assert.AreEqual(0, loadedCollection.Items.Count, 'No output items should result in 0 loaded items');
		finally
			reader.Free();
			loadedCollection.Free();
		end;
	finally
		stream.Free();
		originalCollection.Free();
	end;
end;

procedure TParsedObjectRowCollectionTest.TestLoadFromStreamReader_MultipleItems();
var
	originalCollection, loadedCollection : TParsedObjectRowCollection;
	stream : TMemoryStream;
	writer : TStreamWriter;
	reader : TStreamReader;
begin
	// Arrange
	originalCollection := TParsedObjectRowCollection.Create();
	originalCollection.Items.Add(CreateSampleRow('C:\Test\File1.pas', 10, 'pattern1'));
	originalCollection.Items.Add(CreateSampleRow('C:\Test\File2.pas', 20, 'pattern2'));
	originalCollection.Items.Add(CreateSampleRow('C:\Test\File3.pas', 30, 'pattern3'));

	stream := TMemoryStream.Create();
	try
		writer := TStreamWriter.Create(stream, TEncoding.UTF8);
		try
			originalCollection.SaveToStreamWriter(writer);
			writer.Flush();
		finally
			writer.Free();
		end;

		// Debug
		DebugStreamContent(stream, 'LoadMultipleItems_BeforeLoad');

		// Act
		stream.Position := 0;
		reader := TStreamReader.Create(stream, TEncoding.UTF8);
		loadedCollection := TParsedObjectRowCollection.Create();
		try
			loadedCollection.LoadFromStreamReader(reader);

			// Assert
			Assert.AreEqual(3, loadedCollection.Items.Count, 'Loaded collection should have 3 items');
			Assert.AreEqual('C:\Test\File1.pas', loadedCollection.Items[0].FilePath, 'First item file path should match');
			Assert.AreEqual('pattern1', loadedCollection.Items[0].GetColumnText(ciMatchText), 'First item match text should match');
			Assert.AreEqual('C:\Test\File2.pas', loadedCollection.Items[1].FilePath, 'Second item file path should match');
			Assert.AreEqual('pattern2', loadedCollection.Items[1].GetColumnText(ciMatchText), 'Second item match text should match');
			Assert.AreEqual('C:\Test\File3.pas', loadedCollection.Items[2].FilePath, 'Third item file path should match');
			Assert.AreEqual('pattern3', loadedCollection.Items[2].GetColumnText(ciMatchText), 'Third item match text should match');
		finally
			reader.Free();
			loadedCollection.Free();
		end;
	finally
		stream.Free();
		originalCollection.Free();
	end;
end;

procedure TParsedObjectRowCollectionTest.TestLoadFromStreamReader_ItemsWithEmptyColumns();
var
	originalCollection, loadedCollection : TParsedObjectRowCollection;
	stream : TMemoryStream;
	writer : TStreamWriter;
	reader : TStreamReader;
begin
	// Arrange
	originalCollection := TParsedObjectRowCollection.Create();
	originalCollection.Items.Add(CreateSampleRow('C:\Test\File1.pas', 10, 'pattern1'));
	originalCollection.Items.Add(CreateRowWithEmptyColumns()); // This has empty columns
	originalCollection.Items.Add(CreateSampleRow('C:\Test\File3.pas', 30, 'pattern3'));

	stream := TMemoryStream.Create();
	try
		writer := TStreamWriter.Create(stream, TEncoding.UTF8);
		try
			originalCollection.SaveToStreamWriter(writer);
			writer.Flush();
		finally
			writer.Free();
		end;

		// Debug
		DebugStreamContent(stream, 'LoadItemsWithEmptyColumns_BeforeLoad');

		// Act
		stream.Position := 0;
		reader := TStreamReader.Create(stream, TEncoding.UTF8);
		loadedCollection := TParsedObjectRowCollection.Create();
		try
			loadedCollection.LoadFromStreamReader(reader);

			// Assert
			Assert.AreEqual(3, loadedCollection.Items.Count, 'Loaded collection should have 3 items');
			Assert.AreEqual('C:\Test\File1.pas', loadedCollection.Items[0].FilePath, 'First item should match');
			Assert.AreEqual('C:\Test\File.pas', loadedCollection.Items[1].FilePath, 'Second item (with empty cols) should match');
			Assert.AreEqual('', loadedCollection.Items[1].GetColumnText(ciMatchText), 'Second item should have empty match text');
			Assert.AreEqual('C:\Test\File3.pas', loadedCollection.Items[2].FilePath, 'Third item should match');
		finally
			reader.Free();
			loadedCollection.Free();
		end;
	finally
		stream.Free();
		originalCollection.Free();
	end;
end;

procedure TParsedObjectRowCollectionTest.TestRoundtripPersistence_EmptyCollection();
var
	originalCollection, loadedCollection : TParsedObjectRowCollection;
	stream : TMemoryStream;
	writer : TStreamWriter;
	reader : TStreamReader;
begin
	// Arrange
	originalCollection := TParsedObjectRowCollection.Create();

	stream := TMemoryStream.Create();
	try
		// Act - Save
		writer := TStreamWriter.Create(stream, TEncoding.UTF8);
		try
			originalCollection.SaveToStreamWriter(writer);
			writer.Flush();
		finally
			writer.Free();
		end;

		// Act - Load
		stream.Position := 0;
		reader := TStreamReader.Create(stream, TEncoding.UTF8);
		loadedCollection := TParsedObjectRowCollection.Create();
		try
			loadedCollection.LoadFromStreamReader(reader);

			// Assert
			Assert.AreEqual(originalCollection.Items.Count, loadedCollection.Items.Count, 'Item counts should match');
		finally
			reader.Free();
			loadedCollection.Free();
		end;
	finally
		stream.Free();
		originalCollection.Free();
	end;
end;

procedure TParsedObjectRowCollectionTest.TestRoundtripPersistence_SingleItem();
var
	originalCollection, loadedCollection : TParsedObjectRowCollection;
	stream : TMemoryStream;
	writer : TStreamWriter;
	reader : TStreamReader;
	originalRow, loadedRow : IParsedObjectRow;
begin
	// Arrange
	originalCollection := TParsedObjectRowCollection.Create();
	originalRow := CreateSampleRow('C:\Test\File1.pas', 10, 'pattern');
	originalCollection.Items.Add(originalRow);

	stream := TMemoryStream.Create();
	try
		// Act - Save
		writer := TStreamWriter.Create(stream, TEncoding.UTF8);
		try
			originalCollection.SaveToStreamWriter(writer);
			writer.Flush();
		finally
			writer.Free();
		end;

		// Act - Load
		stream.Position := 0;
		reader := TStreamReader.Create(stream, TEncoding.UTF8);
		loadedCollection := TParsedObjectRowCollection.Create();
		try
			loadedCollection.LoadFromStreamReader(reader);

			// Assert
			Assert.AreEqual(1, loadedCollection.Items.Count, 'Should have 1 item after roundtrip');
			loadedRow := loadedCollection.Items[0];

			Assert.AreEqual(originalRow.FilePath, loadedRow.FilePath, 'File path should be preserved');
			Assert.AreEqual(originalRow.Row, loadedRow.Row, 'Row should be preserved');
			Assert.AreEqual(originalRow.ColBegin, loadedRow.ColBegin, 'ColBegin should be preserved');
			Assert.AreEqual(originalRow.ColEnd, loadedRow.ColEnd, 'ColEnd should be preserved');
			Assert.AreEqual(originalRow.GetColumnText(ciMatchText), loadedRow.GetColumnText(ciMatchText), 'Match text should be preserved');
			Assert.AreEqual(originalRow.ParserType, loadedRow.ParserType, 'Parser type should be preserved');
		finally
			reader.Free();
			loadedCollection.Free();
		end;
	finally
		stream.Free();
		originalCollection.Free();
	end;
end;

procedure TParsedObjectRowCollectionTest.TestRoundtripPersistence_MultipleItems();
var
	originalCollection, loadedCollection : TParsedObjectRowCollection;
	stream : TMemoryStream;
	writer : TStreamWriter;
	reader : TStreamReader;
	i : Integer;
begin
	// Arrange
	originalCollection := TParsedObjectRowCollection.Create();
	originalCollection.Items.Add(CreateSampleRow('C:\Test\File1.pas', 10, 'pattern1'));
	originalCollection.Items.Add(CreateSampleRow('C:\Test\File2.pas', 20, 'pattern2'));
	originalCollection.Items.Add(CreateSampleRow('C:\Test\File3.pas', 30, 'pattern3'));

	stream := TMemoryStream.Create();
	try
		// Act - Save
		writer := TStreamWriter.Create(stream, TEncoding.UTF8);
		try
			originalCollection.SaveToStreamWriter(writer);
			writer.Flush();
		finally
			writer.Free();
		end;

		// Act - Load
		stream.Position := 0;
		reader := TStreamReader.Create(stream, TEncoding.UTF8);
		loadedCollection := TParsedObjectRowCollection.Create();
		try
			loadedCollection.LoadFromStreamReader(reader);

			// Assert
			Assert.AreEqual(originalCollection.Items.Count, loadedCollection.Items.Count, 'Item counts should match');

			for i := 0 to originalCollection.Items.Count - 1 do begin
				Assert.AreEqual(originalCollection.Items[i].FilePath, loadedCollection.Items[i].FilePath,
					Format('Item %d file path should match', [i]));
				Assert.AreEqual(originalCollection.Items[i].Row, loadedCollection.Items[i].Row, Format('Item %d row should match', [i]));
				Assert.AreEqual(originalCollection.Items[i].GetColumnText(ciMatchText),
					loadedCollection.Items[i].GetColumnText(ciMatchText), Format('Item %d match text should match', [i]));
				Assert.AreEqual(originalCollection.Items[i].ParserType, loadedCollection.Items[i].ParserType,
					Format('Item %d parser type should match', [i]));
			end;
		finally
			reader.Free();
			loadedCollection.Free();
		end;
	finally
		stream.Free();
		originalCollection.Free();
	end;
end;

procedure TParsedObjectRowCollectionTest.TestRoundtripPersistence_ItemsWithEmptyColumns();
var
	originalCollection, loadedCollection : TParsedObjectRowCollection;
	stream : TMemoryStream;
	writer : TStreamWriter;
	reader : TStreamReader;
	i : Integer;
begin
	// Arrange
	originalCollection := TParsedObjectRowCollection.Create();
	originalCollection.Items.Add(CreateSampleRow('C:\Test\File1.pas', 10, 'pattern1'));
	originalCollection.Items.Add(CreateRowWithEmptyColumns());
	originalCollection.Items.Add(CreateSampleRow('C:\Test\File3.pas', 30, 'pattern3'));

	stream := TMemoryStream.Create();
	try
		// Act - Save
		writer := TStreamWriter.Create(stream, TEncoding.UTF8);
		try
			originalCollection.SaveToStreamWriter(writer);
			writer.Flush();
		finally
			writer.Free();
		end;

		// Act - Load
		stream.Position := 0;
		reader := TStreamReader.Create(stream, TEncoding.UTF8);
		loadedCollection := TParsedObjectRowCollection.Create();
		try
			loadedCollection.LoadFromStreamReader(reader);

			// Assert
			Assert.AreEqual(originalCollection.Items.Count, loadedCollection.Items.Count, 'Item counts should match');

			for i := 0 to originalCollection.Items.Count - 1 do begin
				Assert.AreEqual(originalCollection.Items[i].FilePath, loadedCollection.Items[i].FilePath,
					Format('Item %d file path should match', [i]));
				Assert.AreEqual(originalCollection.Items[i].Row, loadedCollection.Items[i].Row, Format('Item %d row should match', [i]));
				Assert.AreEqual(originalCollection.Items[i].GetColumnText(ciMatchText),
					loadedCollection.Items[i].GetColumnText(ciMatchText), Format('Item %d match text should match (even if empty)', [i]));
				Assert.AreEqual(originalCollection.Items[i].ParserType, loadedCollection.Items[i].ParserType,
					Format('Item %d parser type should match', [i]));
			end;
		finally
			reader.Free();
			loadedCollection.Free();
		end;
	finally
		stream.Free();
		originalCollection.Free();
	end;
end;

procedure TParsedObjectRowCollectionTest.TestRoundtripPersistence_MixedContent();
var
	originalCollection, loadedCollection : TParsedObjectRowCollection;
	stream : TMemoryStream;
	writer : TStreamWriter;
	reader : TStreamReader;
begin
	// Arrange - Mix of regular items, empty columns, and different parser types
	originalCollection := TParsedObjectRowCollection.Create();
	originalCollection.Items.Add(CreateSampleRow('C:\Test\File1.pas', 10, 'pattern1', ptRipGrepSearch));
	originalCollection.Items.Add(CreateRowWithEmptyColumns());
	originalCollection.Items.Add(CreateSampleRow('C:\Test\File2.pas', 20, 'pattern2', ptRipGrepPrettySearch));
	originalCollection.Items.Add(CreateSampleRow('C:\Test\File3.pas', 30, '', ptRipGrepJson)); // Empty match text

	stream := TMemoryStream.Create();
	try
		// Act - Save
		writer := TStreamWriter.Create(stream, TEncoding.UTF8);
		try
			originalCollection.SaveToStreamWriter(writer);
			writer.Flush();
		finally
			writer.Free();
		end;

		// Debug
		DebugStreamContent(stream, 'MixedContent_AfterSave');

		// Act - Load
		stream.Position := 0;
		reader := TStreamReader.Create(stream, TEncoding.UTF8);
		loadedCollection := TParsedObjectRowCollection.Create();
		try
			loadedCollection.LoadFromStreamReader(reader);

			// Assert
			Assert.AreEqual(4, loadedCollection.Items.Count, 'Should have 4 items');
			Assert.AreEqual(ptRipGrepSearch, loadedCollection.Items[0].ParserType, 'First item parser type should match');
			Assert.AreEqual(ptRipGrepSearch, loadedCollection.Items[1].ParserType, 'Second item parser type should match');
			Assert.AreEqual(ptRipGrepPrettySearch, loadedCollection.Items[2].ParserType, 'Third item parser type should match');
			Assert.AreEqual(ptRipGrepJson, loadedCollection.Items[3].ParserType, 'Fourth item parser type should match');
		finally
			reader.Free();
			loadedCollection.Free();
		end;
	finally
		stream.Free();
		originalCollection.Free();
	end;
end;

procedure TParsedObjectRowCollectionTest.TestStreamContent_DebugOutput();
var
	collection : TParsedObjectRowCollection;
	stream : TMemoryStream;
	writer : TStreamWriter;
	reader : TStreamReader;
	line : string;
	lineCount : Integer;
begin
	// Arrange - Create a simple 2-item collection to debug the exact stream format
	collection := TParsedObjectRowCollection.Create();
	collection.Items.Add(CreateSampleRow('C:\Test\File1.pas', 10, 'pattern1'));
	collection.Items.Add(CreateSampleRow('C:\Test\File2.pas', 20, 'pattern2'));

	stream := TMemoryStream.Create();
	try
		// Act - Save and examine exact stream content
		writer := TStreamWriter.Create(stream, TEncoding.UTF8);
		try
			collection.SaveToStreamWriter(writer);
			writer.Flush();
		finally
			writer.Free();
		end;

		// Debug exact stream content line by line
		stream.Position := 0;
		reader := TStreamReader.Create(stream, TEncoding.UTF8);
		try
			lineCount := 0;
			var dbgMsg := TDebugMsgBeginEnd.New('ExactStreamContentDebug');
			while not reader.EndOfStream do begin
				line := reader.ReadLine();
				dbgMsg.Msg(Format('Line %d: "%s"', [lineCount, line]));
				Inc(lineCount);
			end;
			dbgMsg.Msg(Format('Total lines: %d', [lineCount]));
			dbgMsg.Msg('Expected: 1 (count) + 2 * (1 parsertype + 7 columns) = 17 lines');

			// Assert
			Assert.AreEqual(17, lineCount, 'Should have exactly 17 lines for 2 items');
		finally
			reader.Free();
		end;
	finally
		stream.Free();
		collection.Free();
	end;
end;

initialization

TDUnitX.RegisterTestFixture(TParsedObjectRowCollectionTest);

end.
