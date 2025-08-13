unit RipGrepper.Data.HistoryItemObjectTest;

interface

uses
	DUnitX.TestFramework,
	RipGrepper.Data.HistoryItemObject,
	Spring,
	RipGrepper.Common.SearchTextWithOptions,
	RipGrepper.Common.GuiSearchParams,
	System.Classes,
	RipGrepper.Settings.SearchFormSettings,
	RipGrepper.Settings.SettingVariant,
	System.Generics.Defaults,
	RipGrepper.Settings.RipGrepArguments,
	RipGrepper.Common.Interfaces;

type

	[TestFixture]
	THistoryItemObjectTest = class(TObject)
		const
			TEST_SEARCH_TEXT = 'TestText';
			HIST_OBJ_COUNT = 10;
			ENCODING_VALUE = 'EncodingValue';
			INT_SETTING_KEY = 'IntSettingKey';
			INT_SETTING_VAL = 11;
			STR_SETTING_KEY = 'SettingKey';
			STR_SETTING_VAL = 'str value';
			REPLACE_TEXT = 'replace text';

		private
		var
			FArrayComparer : IComparer<TArray<string>>;
			FGuiParams : IShared<TSearchTextWithOptions>;
			FGuiSearchTextParams : IShared<TGuiSearchTextParams>;
			FHistoryObjectList : THistoryObjectArray;
			FRipGrepArguments : IShared<TRipGrepArguments>;
			FIntSetting : ISetting;
			FStrSetting : ISetting;

			procedure AssertContainsIntAndStrSetting(hio : IHistoryItemObject);
			function GetSettingsDictAsArray(const hio : IHistoryItemObject) : TArray<TArray<string>>;
			procedure WriteHistObjsToStream(const ms : IShared<TMemoryStream>);
			function CreateSampleHistoryItemWithSearchAndMatches() : IHistoryItemObject;
			function CreateSampleHistoryItemWithSearchAndNoMatches() : IHistoryItemObject;
			function CreateSampleHistoryItemWithReplaceAndMatches() : IHistoryItemObject;

		public
			constructor Create();
			[Setup]
			procedure Setup();
			[TearDown]
			procedure TearDown();
			[Test]
			[Ignore('todo')]
			procedure TestSaveLoadFromStream();
			[Test]
			[Ignore('todo')]
			procedure TestSaveLoadListFromStream();
			[Test]
//          [Ignore('todo')]
			procedure LoadFromStreamShouldRestoreSearchWithMatches();
			[Test]
			procedure LoadFromStreamShouldRestoreSearchWithoutMatches();
			[Test]
			// [Ignore('todo')]
			procedure LoadFromStreamShouldRestoreReplaceWithMatches();
			[Test]
			procedure SaveToStreamShouldPersistSearchWithMatches();
			[Test]
			procedure SaveToStreamShouldPersistSearchWithoutMatches();
			[Test]
			// [Ignore('todo')]
			procedure SaveToStreamShouldPersistReplaceWithMatches();
			[Test]
			procedure SearchItemShouldNotBecomeReplaceAfterSerialization();
			[Test]
			procedure ReplaceItemShouldNotBecomeSearchAfterSerialization();
			[Test]
			procedure MultipleSearchItemsSerializationShouldPreserveMode();
			[Test]
			procedure MultipleReplaceItemsSerializationShouldPreserveMode();
			[Test]
			procedure MixedSearchAndReplaceItemsSerializationShouldPreserveMode();
			[Test]
			procedure EmptySearchTextSerializationShouldPreserveMode();
			[Test]
			procedure EmptyReplaceTextSerializationShouldPreserveMode();
			[Test]
			procedure SpecialCharactersInTextShouldPreserveMode();
			[Test]
			procedure LargeDataSerializationShouldPreserveMode();
	end;

implementation

uses

	RipGrepper.Common.SimpleTypes,
	System.SysUtils,
	RipGrepper.Common.Constants,
	RipGrepper.Settings.SettingsDictionary,
	ArrayEx,
	RipGrepper.Helper.StreamReaderWriter,
	RipGrepper.Common.ParsedObject;

constructor THistoryItemObjectTest.Create();
begin
	inherited;
	FArrayComparer := TComparer < TArray < string >>.Construct(
		function(const Left, Right : TArray<string>) : Integer
		begin
			Result := TComparer<string>.Default.Compare(string.Join('', Left), string.Join('', Right));
		end);

end;

procedure THistoryItemObjectTest.AssertContainsIntAndStrSetting(hio : IHistoryItemObject);
var
	arr : TArray<TArray<string>>;
	arrEx : TArrayEx<TArray<string>>;
	cont : Boolean;
begin
	arr := GetSettingsDictAsArray(hio);
	arrEx := arr;
	var inta : TArray<string> := [INT_SETTING_KEY, INT_SETTING_VAL.ToString];
	cont := arrEx.Contains(inta, FArrayComparer);
	Assert.IsTrue(cont,
	{ } Format(hio.SearchText + ' [%s]%s Dict should contain Int Setting', [INT_SETTING_KEY, INT_SETTING_VAL.ToString]));

	inta := [STR_SETTING_KEY, STR_SETTING_VAL];
	cont := arrEx.Contains(inta, FArrayComparer);
	Assert.IsTrue(cont, hio.SearchText + ' Dict should contain Str Setting');
end;

function THistoryItemObjectTest.GetSettingsDictAsArray(const hio : IHistoryItemObject) : TArray<TArray<string>>;
var
	dict : TSettingsDictionary;
begin
	dict := hio.SearchFormSettings.SettingsDict();
	Result := TSettingsDictionary.DictToStringArray(dict);
end;

procedure THistoryItemObjectTest.Setup();
begin
	FGuiParams := Shared.Make<TSearchTextWithOptions>(TSearchTextWithOptions.Create);
	FGuiParams.SearchTextOfUser := TEST_SEARCH_TEXT;
	FGuiParams.SetOption(EGuiOption.soUseRegex);
	FGuiParams.SetOption(EGuiOption.soMatchWord);
	FGuiParams.SetOption(EGuiOption.soMatchCase);

	FGuiSearchTextParams := Shared.Make<TGuiSearchTextParams>();
	FGuiSearchTextParams.SetSearchOptions(FGuiParams.SearchOptions);
	FGuiSearchTextParams.SetSearchText(FGuiParams.SearchTextOfUser);
	FGuiSearchTextParams.IsReplaceMode := True;
	FGuiSearchTextParams.ReplaceText := REPLACE_TEXT;

	FRipGrepArguments := Shared.Make<TRipGrepArguments>();

	FRipGrepArguments.AddPair(RG_ARG_OPTIONS, '--vimgrep');
	FRipGrepArguments.AddPair(RG_ARG_OPTIONS, '-g=*.txt');
	FRipGrepArguments.AddPair(RG_ARG_OPTIONS, RG_PARAM_END);
	FRipGrepArguments.AddPair(RG_ARG_SEARCH_TEXT, 'search text');
	FRipGrepArguments.AddPair(RG_ARG_SEARCH_PATH, 'C:\Path\Search\Files');

	FStrSetting := TStringSetting.Create(STR_SETTING_VAL);
	FIntSetting := TIntegerSetting.Create(INT_SETTING_VAL);
end;

procedure THistoryItemObjectTest.TearDown();
begin
	//
end;

procedure THistoryItemObjectTest.TestSaveLoadFromStream();
var
	hio : IShared<THistoryItemObject>;
	other : IShared<THistoryItemObject>;
	ms : IShared<TMemoryStream>;
begin
	hio := Shared.Make<THistoryItemObject>(THistoryItemObject.Create);
	hio.GuiSearchTextParams := FGuiSearchTextParams;
	hio.RipGrepArguments := FRipGrepArguments;

	ms := Shared.Make<TMemoryStream>();
	hio.SaveToStream(ms);
	ms.Position := 0;

	other := Shared.Make<THistoryItemObject>(THistoryItemObject.Create());
	ms.Position := 0;
	other.LoadFromStream(ms);

	Assert.AreEqual(TEST_SEARCH_TEXT,
	{ } other.GuiSearchTextParams.SearchTextWithOptions.SearchTextOfUser,
	{ } 'SearchText content should match the expected serialized data');
	Assert.AreEqual(REPLACE_TEXT,
	{ } other.GuiSearchTextParams.ReplaceText,
	{ } 'ReplaceText content should match the expected serialized data');
	Assert.IsTrue(
	{ } other.GuiSearchTextParams.IsReplaceMode,
	{ } 'IsReplaceMode content should match the expected serialized data');

	Assert.AreEqual(hio.RipGrepArguments.ToStringArray,
	{ } other.RipGrepArguments.ToStringArray,
	{ } 'RipGrepArguments content should match the expected serialized data');
end;

procedure THistoryItemObjectTest.TestSaveLoadListFromStream();
var
	arr : TArray<TArray<string>>;
	hio : IHistoryItemObject;
	ms : IShared<TMemoryStream>;
	sr : IShared<TStreamReader>;
	hioList : THistoryObjectArray;
begin
	ms := Shared.Make<TMemoryStream>();
	sr := Shared.Make<TStreamReader>(TStreamReader.Create(ms));

	WriteHistObjsToStream(ms);
	ms.Position := 0;
	var
	count := StrToInt(sr.ReadLine());

	for var i := 0 to count - 1 do begin
		hio := THistoryItemObject.Create();
		hio.LoadFromStreamReader(sr);
		hioList.Add(hio);
	end;

	for var i := 0 to count - 1 do begin
		hio := hioList[i];
		Assert.AreEqual(TEST_SEARCH_TEXT + i.ToString,
		{ } hio.GuiSearchTextParams.SearchTextWithOptions.SearchTextOfUser,
		{ } 'SearchText content should match the expected serialized data');
		Assert.AreEqual(FHistoryObjectList[i].RipGrepArguments.ToStringArray,
		{ } hio.RipGrepArguments.ToStringArray,
		{ } 'RipGrepArguments content should match the expected serialized data');
		AssertContainsIntAndStrSetting(hio);
	end;
end;

procedure THistoryItemObjectTest.WriteHistObjsToStream(const ms : IShared<TMemoryStream>);
var
	hio : IHistoryItemObject;
	sw : IShared<TStreamWriter>;
begin
	sw := Shared.Make<TStreamWriter>(TStreamWriter.Create(ms));
	sw.WriteLineAsInteger(HIST_OBJ_COUNT);
	for var i := 0 to (HIST_OBJ_COUNT - 1) do begin
		hio := THistoryItemObject.Create();
		hio.GuiSearchTextParams := FGuiSearchTextParams;
		var
		searchText := TEST_SEARCH_TEXT + i.ToString;

		hio.GuiSearchTextParams.SetSearchText(searchText);
		hio.RipGrepArguments := FRipGrepArguments;
		hio.RipGrepArguments.AddPair(RG_ARG_SEARCH_TEXT, searchText);

		var
		dict := hio.SearchFormSettings.SettingsDict;
		dict.AddOrChange(STR_SETTING_KEY, FStrSetting);
		dict.AddOrChange(INT_SETTING_KEY, FIntSetting);
		var
		sec := hio.SearchFormSettings.IniSectionName;
		Assert.AreEqual(dict()[sec][STR_SETTING_KEY], FStrSetting, Format('AddOrChange %d failed', [i]));
		Assert.AreEqual(dict()[sec][INT_SETTING_KEY], FIntSetting, Format('AddOrChange %d failed', [i]));

		hio.SaveToStreamWriter(sw);
		FHistoryObjectList.Add(hio);
	end;
	AssertContainsIntAndStrSetting(hio);
end;

function THistoryItemObjectTest.CreateSampleHistoryItemWithSearchAndMatches() : IHistoryItemObject;
var
	guiParams : IShared<TGuiSearchTextParams>;
	searchText : IShared<TSearchTextWithOptions>;
	row : IParsedObjectRow;
	colData : TArrayEx<TColumnData>;
begin
	Result := THistoryItemObject.Create();

	// Setup search parameters
	guiParams := Shared.Make<TGuiSearchTextParams>(TGuiSearchTextParams.Create('TestSection'));
	searchText := guiParams.SearchTextWithOptions;
	searchText.SearchTextOfUser := 'test pattern';
	searchText.SetOption(EGuiOption.soMatchCase);
	guiParams.IsReplaceMode := False;

	Result.GuiSearchTextParams := guiParams;

	// Add some RipGrep arguments
	Result.RipGrepArguments.AddPair(RG_ARG_OPTIONS, '--case-sensitive');
	Result.RipGrepArguments.AddPair(RG_ARG_SEARCH_TEXT, 'test pattern');
	Result.RipGrepArguments.AddPair(RG_ARG_SEARCH_PATH, 'C:\Test\Path');
	Result.RipGrepArguments.AddPair(RG_ARG_OPTIONS, '-g *.pas');

	// Create some sample matches
	Result.ShouldSaveResult := True;

	// Create first match
	row := TParsedObjectRow.Create({nil, ptRipGrepSearch});
	colData.Clear();
	colData.Add(TColumnData.New(ciFile, 'C:\Test\File1.pas'));
	colData.Add(TColumnData.New(ciRow, '10'));
	colData.Add(TColumnData.New(ciCol, '5'));
	colData.Add(TColumnData.New(ciText, 'This is a test pattern match'));
	colData.Add(TColumnData.New(ciMatchText, 'test pattern'));
	colData.Add(TColumnData.New(ciTextAfterMatch, ' match'));
	row.Columns := colData;
	Result.Matches.Items.Add(row);

	// Create second match
	row := TParsedObjectRow.Create({nil, ptRipGrepSearch});
	colData.Clear();
	colData.Add(TColumnData.New(ciFile, 'C:\Test\File2.pas'));
	colData.Add(TColumnData.New(ciRow, '15'));
	colData.Add(TColumnData.New(ciCol, '8'));
	colData.Add(TColumnData.New(ciText, 'Another test pattern here'));
	colData.Add(TColumnData.New(ciMatchText, 'test pattern'));
	colData.Add(TColumnData.New(ciTextAfterMatch, ' here'));
	row.Columns := colData;
	Result.Matches.Items.Add(row);

	Result.FileCount := 2;
end;

function THistoryItemObjectTest.CreateSampleHistoryItemWithSearchAndNoMatches() : IHistoryItemObject;
var
	guiParams : IShared<TGuiSearchTextParams>;
	searchText : IShared<TSearchTextWithOptions>;
	row : IParsedObjectRow;
	colData : TArrayEx<TColumnData>;
begin
	Result := THistoryItemObject.Create();

	// Setup search parameters
	guiParams := Shared.Make<TGuiSearchTextParams>(TGuiSearchTextParams.Create('TestSection'));
	searchText := guiParams.SearchTextWithOptions;
	searchText.SearchTextOfUser := 'nonexistent pattern';
	searchText.SetOption(EGuiOption.soUseRegex);
	guiParams.IsReplaceMode := False;

	Result.GuiSearchTextParams := guiParams;

	// Add some RipGrep arguments
	Result.RipGrepArguments.AddPair(RG_ARG_OPTIONS, '--regex');
	Result.RipGrepArguments.AddPair(RG_ARG_SEARCH_TEXT, 'nonexistent pattern');
	Result.RipGrepArguments.AddPair(RG_ARG_SEARCH_PATH, 'C:\Empty\Path');
	Result.RipGrepArguments.AddPair(RG_ARG_OPTIONS, '-g *.text');

	// Create "no output" result
	Result.ShouldSaveResult := True;
	row := TParsedObjectRow.Create({nil, ptRipGrepSearch});
	colData.Clear();
	colData.Add(TColumnData.New(ciFile, 'rg.exe' + RG_HAS_NO_OUTPUT));
	colData.Add(TColumnData.New(ciRow, ''));
	colData.Add(TColumnData.New(ciCol, ''));
	colData.Add(TColumnData.New(ciText, ''));
	colData.Add(TColumnData.New(ciMatchText, ''));
	colData.Add(TColumnData.New(ciTextAfterMatch, ''));
	row.Columns := colData;
	Result.Matches.Items.Add(row);

	Result.FileCount := 0;
end;

function THistoryItemObjectTest.CreateSampleHistoryItemWithReplaceAndMatches() : IHistoryItemObject;
var
	guiParams : IShared<TGuiSearchTextParams>;
	searchText : IShared<TSearchTextWithOptions>;
	row : IParsedObjectRow;
	colData : TArrayEx<TColumnData>;
begin
	Result := THistoryItemObject.Create();

	// Setup replace parameters
	guiParams := Shared.Make<TGuiSearchTextParams>(TGuiSearchTextParams.Create('TestSection'));
	searchText := guiParams.SearchTextWithOptions;
	searchText.SearchTextOfUser := 'old_value';
	searchText.SetOption(EGuiOption.soMatchWord);
	guiParams.IsReplaceMode := True;
	guiParams.ReplaceText := 'new_value';

	Result.GuiSearchTextParams := guiParams;

	// Add some RipGrep arguments for replace
	Result.RipGrepArguments.AddPair(RG_ARG_OPTIONS, '--replace');
	Result.RipGrepArguments.AddPair(RG_ARG_SEARCH_TEXT, 'new_value');
	Result.RipGrepArguments.AddPair(RG_ARG_OPTIONS, '--word-regexp');
	Result.RipGrepArguments.AddPair(RG_ARG_SEARCH_TEXT, 'old_value');
	Result.RipGrepArguments.AddPair(RG_ARG_SEARCH_PATH, 'C:\Replace\Path');
	Result.RipGrepArguments.AddPair(RG_ARG_OPTIONS, '-g *.pas');
	Result.RipGrepArguments.AddPair(RG_ARG_OPTIONS, '-g *.inc');

	// Create some sample replace matches
	Result.ShouldSaveResult := True;

	// Create first replace match
	row := TParsedObjectRow.Create({nil, ptRipGrepSearch});
	colData.Clear();
	colData.Add(TColumnData.New(ciFile, 'C:\Replace\File1.pas'));
	colData.Add(TColumnData.New(ciRow, '20'));
	colData.Add(TColumnData.New(ciCol, '12'));
	colData.Add(TColumnData.New(ciText, 'var old_value: string;'));
	colData.Add(TColumnData.New(ciMatchText, 'old_value'));
	colData.Add(TColumnData.New(ciTextAfterMatch, ': string;'));
	row.Columns := colData;
	Result.Matches.Items.Add(row);

	Result.FileCount := 1;
end;

procedure THistoryItemObjectTest.SaveToStreamShouldPersistSearchWithMatches();
var
	historyItem : IHistoryItemObject;
	stream : TMemoryStream;
begin
	// Arrange
	historyItem := CreateSampleHistoryItemWithSearchAndMatches();
	stream := TMemoryStream.Create();
	try
		// Act
		historyItem.SaveToStream(stream);

		// Assert
		Assert.IsTrue(stream.Size > 0, 'Stream should contain data after saving');
		Assert.AreEqual('test pattern', historyItem.GuiSearchTextParams.SearchTextWithOptions.SearchTextOfUser,
			'Search text should be preserved');
		Assert.AreEqual(2, historyItem.Matches.Items.Count, 'Should have 2 matches');
		Assert.AreEqual(4, historyItem.RipGrepArguments.Count, 'Should have 4 RipGrep arguments');
	finally
		stream.Free();
	end;
end;

procedure THistoryItemObjectTest.SaveToStreamShouldPersistSearchWithoutMatches();
var
	historyItem : IHistoryItemObject;
	stream : TMemoryStream;
begin
	// Arrange
	historyItem := CreateSampleHistoryItemWithSearchAndNoMatches();
	stream := TMemoryStream.Create();
	try
		// Act
		historyItem.SaveToStream(stream);

		// Assert
		Assert.IsTrue(stream.Size > 0, 'Stream should contain data after saving');
		Assert.AreEqual('nonexistent pattern', historyItem.GuiSearchTextParams.SearchTextWithOptions.SearchTextOfUser,
			'Search text should be preserved');
		Assert.AreEqual(1, historyItem.Matches.Items.Count, 'Should have 1 "no output" item');
		Assert.AreEqual(4, historyItem.RipGrepArguments.Count, 'Should have 4 RipGrep arguments');
	finally
		stream.Free();
	end;
end;

procedure THistoryItemObjectTest.SaveToStreamShouldPersistReplaceWithMatches();
var
	historyItem : IHistoryItemObject;
	stream : TMemoryStream;
begin
	// Arrange
	historyItem := CreateSampleHistoryItemWithReplaceAndMatches();
	stream := TMemoryStream.Create();
	try
		// Act
		historyItem.SaveToStream(stream);

		// Assert
		Assert.IsTrue(stream.Size > 0, 'Stream should contain data after saving');
		Assert.AreEqual('old_value', historyItem.GuiSearchTextParams.SearchTextWithOptions.SearchTextOfUser,
			'Search text should be preserved');
		Assert.IsTrue(historyItem.GuiSearchTextParams.IsReplaceMode, 'Should be in replace mode');
		Assert.AreEqual('new_value', historyItem.GuiSearchTextParams.ReplaceText, 'Replace text should be preserved');
		Assert.AreEqual(1, historyItem.Matches.Items.Count, 'Should have 1 replace match');
		Assert.AreEqual(7, historyItem.RipGrepArguments.Count, 'Should have 7 RipGrep arguments');
	finally
		stream.Free();
	end;
end;

procedure THistoryItemObjectTest.LoadFromStreamShouldRestoreSearchWithMatches();
var
	originalItem : IHistoryItemObject;
	loadedItem : IHistoryItemObject;
	stream : TMemoryStream;
begin
	// Arrange
	originalItem := CreateSampleHistoryItemWithSearchAndMatches();
	stream := TMemoryStream.Create();
	try
		originalItem.SaveToStream(stream);
		stream.Position := 0;

		// Act
		loadedItem := THistoryItemObject.Create();
		loadedItem.LoadFromStream(stream);

		// Assert
		Assert.IsTrue(loadedItem.IsLoadedFromStream, 'Should be marked as loaded from stream');
		Assert.AreEqual('test pattern', loadedItem.GuiSearchTextParams.SearchTextWithOptions.SearchTextOfUser,
			'Search text should be restored');
		Assert.IsFalse(loadedItem.GuiSearchTextParams.IsReplaceMode, 'Should not be in replace mode');
		Assert.IsTrue(EGuiOption.soMatchCase in loadedItem.GuiSearchTextParams.SearchTextWithOptions.SearchOptions,
			'Search options should be restored');
		Assert.AreEqual(4, loadedItem.RipGrepArguments.Count, 'RipGrep arguments should be restored');
		Assert.IsTrue(loadedItem.RipGrepArguments.Text.Contains('--case-sensitive'), 'First argument should be restored');
		Assert.AreEqual('test pattern', loadedItem.SearchText, 'Search path should be restored');
		// Assert.AreEqual('*.pas', loadedItem.FileMasks, 'File masks should be restored');
		Assert.AreEqual(2, loadedItem.Matches.Items.Count, 'Matches should be restored');
		Assert.AreEqual('C:\Test\File1.pas', loadedItem.Matches.Items[0].GetColumnText(ciFile), 'First match file should be restored');
		Assert.AreEqual('test pattern', loadedItem.Matches.Items[0].GetColumnText(ciMatchText), 'First match text should be restored');
		Assert.AreEqual(2, loadedItem.FileCount, 'File count should be restored');
	finally
		stream.Free();
	end;
end;

procedure THistoryItemObjectTest.LoadFromStreamShouldRestoreSearchWithoutMatches();
var
	originalItem : IHistoryItemObject;
	loadedItem : IHistoryItemObject;
	stream : TMemoryStream;
begin
	// Arrange
	originalItem := CreateSampleHistoryItemWithSearchAndNoMatches();
	stream := TMemoryStream.Create();
	try
		originalItem.SaveToStream(stream);
		stream.Position := 0;

		// Act
		loadedItem := THistoryItemObject.Create();
		loadedItem.LoadFromStream(stream);

		// Assert
		Assert.IsTrue(loadedItem.IsLoadedFromStream, 'Should be marked as loaded from stream');
		Assert.AreEqual('nonexistent pattern', loadedItem.GuiSearchTextParams.SearchTextWithOptions.SearchTextOfUser,
			'Search text should be restored');
		Assert.IsFalse(loadedItem.GuiSearchTextParams.IsReplaceMode, 'Should not be in replace mode');
		Assert.IsTrue(EGuiOption.soUseRegex in loadedItem.GuiSearchTextParams.SearchTextWithOptions.SearchOptions,
			'Search options should be restored');
		Assert.AreEqual(4, loadedItem.RipGrepArguments.Count, 'RipGrep arguments should be restored');
		Assert.IsTrue(loadedItem.RipGrepArguments.Text.Contains('--regex'), 'First argument should be restored');
		// Assert.AreEqual('C:\Empty\Path', loadedItem.SearchPath, 'Search path should be restored');
		// Assert.AreEqual('*.text', loadedItem.SearchFormSettings.FileMasks, 'File masks should be restored');
		// Note: "No output" items result in 0 items being stored to stream
		Assert.AreEqual(0, loadedItem.Matches.Items.Count, 'No matches should be restored for "no output" case');
		Assert.AreEqual(0, loadedItem.FileCount, 'File count should be 0 for no matches');
	finally
		stream.Free();
	end;
end;

procedure THistoryItemObjectTest.LoadFromStreamShouldRestoreReplaceWithMatches();
var
	originalItem : IHistoryItemObject;
	loadedItem : IHistoryItemObject;
	stream : TMemoryStream;
begin
	// Arrange
	originalItem := CreateSampleHistoryItemWithReplaceAndMatches();
	stream := TMemoryStream.Create();
	try
		originalItem.SaveToStream(stream);
		stream.Position := 0;

		// Act
		loadedItem := THistoryItemObject.Create();
		loadedItem.LoadFromStream(stream);

		// Assert
		Assert.IsTrue(loadedItem.IsLoadedFromStream, 'Should be marked as loaded from stream');
		Assert.AreEqual('old_value', loadedItem.GuiSearchTextParams.SearchTextWithOptions.SearchTextOfUser,
			'Search text should be restored');
		Assert.IsTrue(loadedItem.GuiSearchTextParams.IsReplaceMode, 'Should be in replace mode');
		Assert.AreEqual('new_value', loadedItem.GuiSearchTextParams.ReplaceText, 'Replace text should be restored');
		Assert.IsTrue(EGuiOption.soMatchWord in loadedItem.GuiSearchTextParams.SearchTextWithOptions.SearchOptions,
			'Search options should be restored');
		Assert.AreEqual(7, loadedItem.RipGrepArguments.Count, 'RipGrep arguments should be restored');
		Assert.IsTrue(loadedItem.RipGrepArguments.Text.Contains('--replace'), 'First argument should be restored');
		// Assert.AreEqual('C:\Replace\Path', loadedItem.SearchFormSettings.SearchPath, 'Search path should be restored');
		// Assert.AreEqual('*.pas;*.inc', loadedItem.SearchFormSettings.FileMasks, 'File masks should be restored');
		Assert.AreEqual(1, loadedItem.Matches.Items.Count, 'Matches should be restored');
		Assert.AreEqual('C:\Replace\File1.pas', loadedItem.Matches.Items[0].GetColumnText(ciFile), 'Match file should be restored');
		Assert.AreEqual('old_value', loadedItem.Matches.Items[0].GetColumnText(ciMatchText), 'Match text should be restored');
		Assert.AreEqual(1, loadedItem.FileCount, 'File count should be restored');
	finally
		stream.Free();
	end;
end;

// New tests to detect the serialization fault where search items become replace items
procedure THistoryItemObjectTest.SearchItemShouldNotBecomeReplaceAfterSerialization();
var
	originalItem : IHistoryItemObject;
	loadedItem : IHistoryItemObject;
	stream : TMemoryStream;
begin
	// Arrange: Create a search item (NOT replace mode)
	originalItem := CreateSampleHistoryItemWithSearchAndMatches();
	stream := TMemoryStream.Create();
	try
		// Ensure it's NOT in replace mode
		Assert.IsFalse(originalItem.IsReplaceMode, 'Original item should NOT be in replace mode');
		Assert.AreEqual('', originalItem.ReplaceText, 'Original item should have empty replace text');

		// Act: Save and load
		originalItem.SaveToStream(stream);
		stream.Position := 0;
		loadedItem := THistoryItemObject.Create();
		loadedItem.LoadFromStream(stream);

		// Assert: The loaded item should still be a search item, NOT a replace item
		Assert.IsFalse(loadedItem.IsReplaceMode, 
			'CRITICAL FAULT: Search item became replace item after serialization!');
		Assert.AreEqual('', loadedItem.ReplaceText, 
			'CRITICAL FAULT: Search item should have empty replace text after deserialization!');
		Assert.AreEqual('test pattern', loadedItem.SearchText, 'Search text should be preserved');
	finally
		stream.Free();
	end;
end;

procedure THistoryItemObjectTest.ReplaceItemShouldNotBecomeSearchAfterSerialization();
var
	originalItem : IHistoryItemObject;
	loadedItem : IHistoryItemObject;
	stream : TMemoryStream;
begin
	// Arrange: Create a replace item
	originalItem := CreateSampleHistoryItemWithReplaceAndMatches();
	stream := TMemoryStream.Create();
	try
		// Ensure it's in replace mode
		Assert.IsTrue(originalItem.IsReplaceMode, 'Original item should be in replace mode');
		Assert.AreEqual('new_value', originalItem.ReplaceText, 'Original item should have replace text');

		// Act: Save and load
		originalItem.SaveToStream(stream);
		stream.Position := 0;
		loadedItem := THistoryItemObject.Create();
		loadedItem.LoadFromStream(stream);

		// Assert: The loaded item should still be a replace item
		Assert.IsTrue(loadedItem.IsReplaceMode, 
			'CRITICAL FAULT: Replace item became search item after serialization!');
		Assert.AreEqual('new_value', loadedItem.ReplaceText, 
			'CRITICAL FAULT: Replace item should preserve replace text after deserialization!');
		Assert.AreEqual('old_value', loadedItem.SearchText, 'Search text should be preserved');
	finally
		stream.Free();
	end;
end;

procedure THistoryItemObjectTest.MultipleSearchItemsSerializationShouldPreserveMode();
var
	searchItems : TArray<IHistoryItemObject>;
	loadedItems : TArray<IHistoryItemObject>;
	stream : TMemoryStream;
	sw : TStreamWriter;
	sr : TStreamReader;
begin
	// Arrange: Create multiple search items
	SetLength(searchItems, 3);
	searchItems[0] := CreateSampleHistoryItemWithSearchAndMatches();
	searchItems[1] := CreateSampleHistoryItemWithSearchAndNoMatches();
	searchItems[2] := CreateSampleHistoryItemWithSearchAndMatches();
	
	stream := TMemoryStream.Create();
	try
		sw := TStreamWriter.Create(stream);
		try
			// Save multiple search items
			sw.WriteLine(IntToStr(Length(searchItems)));
			for var item in searchItems do begin
				Assert.IsFalse(item.IsReplaceMode, 'All original items should be search items');
				item.SaveToStreamWriter(sw);
			end;
		finally
			sw.Free();
		end;

		// Act: Load items back
		stream.Position := 0;
		sr := TStreamReader.Create(stream, TEncoding.UTF8);
		try
			var count := StrToInt(sr.ReadLine());
			SetLength(loadedItems, count);
			for var i := 0 to count - 1 do begin
				loadedItems[i] := THistoryItemObject.Create();
				loadedItems[i].LoadFromStreamReader(sr);
			end;
		finally
			sr.Free();
		end;

		// Assert: All loaded items should still be search items
		for var i := 0 to Length(loadedItems) - 1 do begin
			Assert.IsFalse(loadedItems[i].IsReplaceMode, 
				Format('CRITICAL FAULT: Search item %d became replace item after serialization!', [i]));
			Assert.AreEqual('', loadedItems[i].ReplaceText, 
				Format('CRITICAL FAULT: Search item %d should have empty replace text!', [i]));
		end;
	finally
		stream.Free();
	end;
end;

procedure THistoryItemObjectTest.MultipleReplaceItemsSerializationShouldPreserveMode();
var
	replaceItems : TArray<IHistoryItemObject>;
	loadedItems : TArray<IHistoryItemObject>;
	stream : TMemoryStream;
	sw : TStreamWriter;
	sr : TStreamReader;
begin
	// Arrange: Create multiple replace items
	SetLength(replaceItems, 3);
	replaceItems[0] := CreateSampleHistoryItemWithReplaceAndMatches();
	replaceItems[1] := CreateSampleHistoryItemWithReplaceAndMatches();
	replaceItems[2] := CreateSampleHistoryItemWithReplaceAndMatches();
	
	stream := TMemoryStream.Create();
	try
		sw := TStreamWriter.Create(stream);
		try
			// Save multiple replace items
			sw.WriteLine(IntToStr(Length(replaceItems)));
			for var item in replaceItems do begin
				Assert.IsTrue(item.IsReplaceMode, 'All original items should be replace items');
				item.SaveToStreamWriter(sw);
			end;
		finally
			sw.Free();
		end;

		// Act: Load items back
		stream.Position := 0;
		sr := TStreamReader.Create(stream, TEncoding.UTF8);
		try
			var count := StrToInt(sr.ReadLine());
			SetLength(loadedItems, count);
			for var i := 0 to count - 1 do begin
				loadedItems[i] := THistoryItemObject.Create();
				loadedItems[i].LoadFromStreamReader(sr);
			end;
		finally
			sr.Free();
		end;

		// Assert: All loaded items should still be replace items
		for var i := 0 to Length(loadedItems) - 1 do begin
			Assert.IsTrue(loadedItems[i].IsReplaceMode, 
				Format('CRITICAL FAULT: Replace item %d became search item after serialization!', [i]));
			Assert.AreNotEqual('', loadedItems[i].ReplaceText, 
				Format('CRITICAL FAULT: Replace item %d should have non-empty replace text!', [i]));
		end;
	finally
		stream.Free();
	end;
end;

procedure THistoryItemObjectTest.MixedSearchAndReplaceItemsSerializationShouldPreserveMode();
var
	items : TArray<IHistoryItemObject>;
	loadedItems : TArray<IHistoryItemObject>;
	expectedModes : TArray<Boolean>; // True = replace, False = search
	stream : TMemoryStream;
	sw : TStreamWriter;
	sr : TStreamReader;
begin
	// Arrange: Create mixed search and replace items
	SetLength(items, 4);
	SetLength(expectedModes, 4);
	
	items[0] := CreateSampleHistoryItemWithSearchAndMatches();
	expectedModes[0] := False; // search
	
	items[1] := CreateSampleHistoryItemWithReplaceAndMatches();
	expectedModes[1] := True; // replace
	
	items[2] := CreateSampleHistoryItemWithSearchAndNoMatches();
	expectedModes[2] := False; // search
	
	items[3] := CreateSampleHistoryItemWithReplaceAndMatches();
	expectedModes[3] := True; // replace
	
	stream := TMemoryStream.Create();
	try
		sw := TStreamWriter.Create(stream);
		try
			sw.WriteLine(IntToStr(Length(items)));
			for var i := 0 to Length(items) - 1 do begin
				Assert.AreEqual(expectedModes[i], items[i].IsReplaceMode, 
					Format('Original item %d should have correct mode', [i]));
				items[i].SaveToStreamWriter(sw);
			end;
		finally
			sw.Free();
		end;

		// Act: Load items back
		stream.Position := 0;
		sr := TStreamReader.Create(stream, TEncoding.UTF8);
		try
			var count := StrToInt(sr.ReadLine());
			SetLength(loadedItems, count);
			for var i := 0 to count - 1 do begin
				loadedItems[i] := THistoryItemObject.Create();
				loadedItems[i].LoadFromStreamReader(sr);
			end;
		finally
			sr.Free();
		end;

		// Assert: All loaded items should preserve their original modes
		for var i := 0 to Length(loadedItems) - 1 do begin
			Assert.AreEqual(expectedModes[i], loadedItems[i].IsReplaceMode, 
				Format('CRITICAL FAULT: Item %d mode changed from %s to %s after serialization!', 
					[i, BoolToStr(expectedModes[i], True), BoolToStr(loadedItems[i].IsReplaceMode, True)]));
				
			if expectedModes[i] then begin
				Assert.AreNotEqual('', loadedItems[i].ReplaceText, 
					Format('Replace item %d should have non-empty replace text', [i]));
			end else begin
				Assert.AreEqual('', loadedItems[i].ReplaceText, 
					Format('Search item %d should have empty replace text', [i]));
			end;
		end;
	finally
		stream.Free();
	end;
end;

procedure THistoryItemObjectTest.EmptySearchTextSerializationShouldPreserveMode();
var
	searchItem : IHistoryItemObject;
	loadedItem : IHistoryItemObject;
	stream : TMemoryStream;
	guiParams : IShared<TGuiSearchTextParams>;
begin
	// Arrange: Create search item with empty search text
	searchItem := THistoryItemObject.Create();
	guiParams := Shared.Make<TGuiSearchTextParams>(TGuiSearchTextParams.Create('TestSection'));
	guiParams.SearchTextWithOptions.SearchTextOfUser := '';
	guiParams.IsReplaceMode := False; // Explicitly set to search mode
	guiParams.ReplaceText := '';
	searchItem.GuiSearchTextParams := guiParams;
	
	stream := TMemoryStream.Create();
	try
		// Act: Save and load
		searchItem.SaveToStream(stream);
		stream.Position := 0;
		loadedItem := THistoryItemObject.Create();
		loadedItem.LoadFromStream(stream);

		// Assert: Should still be search item even with empty text
		Assert.IsFalse(loadedItem.IsReplaceMode, 
			'CRITICAL FAULT: Empty search item became replace item after serialization!');
		Assert.AreEqual('', loadedItem.ReplaceText, 'Empty search item should have empty replace text');
		Assert.AreEqual('', loadedItem.SearchText, 'Empty search text should be preserved');
	finally
		stream.Free();
	end;
end;

procedure THistoryItemObjectTest.EmptyReplaceTextSerializationShouldPreserveMode();
var
	replaceItem : IHistoryItemObject;
	loadedItem : IHistoryItemObject;
	stream : TMemoryStream;
	guiParams : IShared<TGuiSearchTextParams>;
begin
	// Arrange: Create replace item with empty replace text
	replaceItem := THistoryItemObject.Create();
	guiParams := Shared.Make<TGuiSearchTextParams>(TGuiSearchTextParams.Create('TestSection'));
	guiParams.SearchTextWithOptions.SearchTextOfUser := 'search pattern';
	guiParams.IsReplaceMode := True; // Explicitly set to replace mode
	guiParams.ReplaceText := ''; // Empty replace text
	replaceItem.GuiSearchTextParams := guiParams;
	
	stream := TMemoryStream.Create();
	try
		// Act: Save and load
		replaceItem.SaveToStream(stream);
		stream.Position := 0;
		loadedItem := THistoryItemObject.Create();
		loadedItem.LoadFromStream(stream);

		// Assert: Should still be replace item even with empty replace text
		Assert.IsTrue(loadedItem.IsReplaceMode, 
			'CRITICAL FAULT: Replace item with empty replace text became search item after serialization!');
		Assert.AreEqual('', loadedItem.ReplaceText, 'Empty replace text should be preserved');
		Assert.AreEqual('search pattern', loadedItem.SearchText, 'Search text should be preserved');
	finally
		stream.Free();
	end;
end;

procedure THistoryItemObjectTest.SpecialCharactersInTextShouldPreserveMode();
var
	searchItem, replaceItem : IHistoryItemObject;
	loadedSearchItem, loadedReplaceItem : IHistoryItemObject;
	stream : TMemoryStream;
	guiParams : IShared<TGuiSearchTextParams>;
	specialText : string;
begin
	// Arrange: Create items with special characters
	specialText := 'pattern with "quotes" & <brackets> and\nnewlines\t\ttabs';
	
	// Create search item
	searchItem := THistoryItemObject.Create();
	guiParams := Shared.Make<TGuiSearchTextParams>(TGuiSearchTextParams.Create('TestSection'));
	guiParams.SearchTextWithOptions.SearchTextOfUser := specialText;
	guiParams.IsReplaceMode := False;
	guiParams.ReplaceText := '';
	searchItem.GuiSearchTextParams := guiParams;
	
	// Create replace item
	replaceItem := THistoryItemObject.Create();
	guiParams := Shared.Make<TGuiSearchTextParams>(TGuiSearchTextParams.Create('TestSection'));
	guiParams.SearchTextWithOptions.SearchTextOfUser := specialText;
	guiParams.IsReplaceMode := True;
	guiParams.ReplaceText := 'replace with "special" & <chars>';
	replaceItem.GuiSearchTextParams := guiParams;
	
	stream := TMemoryStream.Create();
	try
		// Test search item
		searchItem.SaveToStream(stream);
		stream.Position := 0;
		loadedSearchItem := THistoryItemObject.Create();
		loadedSearchItem.LoadFromStream(stream);

		Assert.IsFalse(loadedSearchItem.IsReplaceMode, 
			'CRITICAL FAULT: Search item with special chars became replace item!');
		Assert.AreEqual(specialText, loadedSearchItem.SearchText, 'Special search text should be preserved');

		// Test replace item
		stream.Clear();
		replaceItem.SaveToStream(stream);
		stream.Position := 0;
		loadedReplaceItem := THistoryItemObject.Create();
		loadedReplaceItem.LoadFromStream(stream);

		Assert.IsTrue(loadedReplaceItem.IsReplaceMode, 
			'CRITICAL FAULT: Replace item with special chars became search item!');
		Assert.AreEqual(specialText, loadedReplaceItem.SearchText, 'Special search text should be preserved');
		Assert.AreEqual('replace with "special" & <chars>', loadedReplaceItem.ReplaceText, 
			'Special replace text should be preserved');
	finally
		stream.Free();
	end;
end;

procedure THistoryItemObjectTest.LargeDataSerializationShouldPreserveMode();
var
	searchItem, replaceItem : IHistoryItemObject;
	loadedSearchItem, loadedReplaceItem : IHistoryItemObject;
	stream : TMemoryStream;
	guiParams : IShared<TGuiSearchTextParams>;
	largeSearchText, largeReplaceText : string;
	row : IParsedObjectRow;
	colData : TArrayEx<TColumnData>;
begin
	// Arrange: Create items with large amounts of data
	largeSearchText := StringOfChar('A', 1000) + 'pattern' + StringOfChar('B', 1000);
	largeReplaceText := StringOfChar('X', 500) + 'replacement' + StringOfChar('Y', 500);
	
	// Create search item with many matches
	searchItem := THistoryItemObject.Create();
	guiParams := Shared.Make<TGuiSearchTextParams>(TGuiSearchTextParams.Create('TestSection'));
	guiParams.SearchTextWithOptions.SearchTextOfUser := largeSearchText;
	guiParams.IsReplaceMode := False;
	searchItem.GuiSearchTextParams := guiParams;
	
	// Add many matches to make serialization larger
	for var i := 1 to 100 do begin
		row := TParsedObjectRow.Create();
		colData.Clear();
		colData.Add(TColumnData.New(ciFile, Format('C:\Test\LargeFile%d.pas', [i])));
		colData.Add(TColumnData.New(ciRow, IntToStr(i * 10)));
		colData.Add(TColumnData.New(ciCol, '5'));
		colData.Add(TColumnData.New(ciText, largeSearchText + ' match ' + IntToStr(i)));
		colData.Add(TColumnData.New(ciMatchText, largeSearchText));
		row.Columns := colData;
		searchItem.Matches.Items.Add(row);
	end;
	
	// Create replace item
	replaceItem := THistoryItemObject.Create();
	guiParams := Shared.Make<TGuiSearchTextParams>(TGuiSearchTextParams.Create('TestSection'));
	guiParams.SearchTextWithOptions.SearchTextOfUser := largeSearchText;
	guiParams.IsReplaceMode := True;
	guiParams.ReplaceText := largeReplaceText;
	replaceItem.GuiSearchTextParams := guiParams;
	
	stream := TMemoryStream.Create();
	try
		// Test search item with large data
		searchItem.SaveToStream(stream);
		stream.Position := 0;
		loadedSearchItem := THistoryItemObject.Create();
		loadedSearchItem.LoadFromStream(stream);

		Assert.IsFalse(loadedSearchItem.IsReplaceMode, 
			'CRITICAL FAULT: Large search item became replace item after serialization!');
		Assert.AreEqual(largeSearchText, loadedSearchItem.SearchText, 'Large search text should be preserved');
		Assert.AreEqual('', loadedSearchItem.ReplaceText, 'Search item should have empty replace text');

		// Test replace item with large data
		stream.Clear();
		replaceItem.SaveToStream(stream);
		stream.Position := 0;
		loadedReplaceItem := THistoryItemObject.Create();
		loadedReplaceItem.LoadFromStream(stream);

		Assert.IsTrue(loadedReplaceItem.IsReplaceMode, 
			'CRITICAL FAULT: Large replace item became search item after serialization!');
		Assert.AreEqual(largeSearchText, loadedReplaceItem.SearchText, 'Large search text should be preserved');
		Assert.AreEqual(largeReplaceText, loadedReplaceItem.ReplaceText, 'Large replace text should be preserved');
	finally
		stream.Free();
	end;
end;

end.
