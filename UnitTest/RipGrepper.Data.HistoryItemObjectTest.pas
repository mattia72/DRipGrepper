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
			procedure AddUniqueSearchText(const _ripGrepArgs : IShared<TRipGrepArguments>; const _searchText : string);

		public
			constructor Create();
			[Setup]
			procedure Setup();
			[TearDown]
			procedure TearDown();
			[Test]
			procedure TestSaveLoadFromStream();
			[Test]
			procedure TestSaveLoadListFromStream();
			[Test]
			// [Ignore('todo')]
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
			procedure EmptyReplaceTextSerializationShouldPreserveMode();
			[Test]
			procedure SpecialCharactersInTextShouldPreserveMode();
			[Test]
			procedure LargeDataSerializationShouldPreserveMode();
			[Test]
			procedure EmptyTest();

			// New comprehensive StreamWriter/StreamReader tests
			[Test]
			procedure TestSaveToStreamWriter_SearchWithMatches();
			[Test]
			procedure TestSaveToStreamWriter_ReplaceWithMatches();
			[Test]
			procedure TestSaveToStreamWriter_EmptyMatches();
			[Test]
			procedure TestSaveToStreamWriter_WithSpecialCharacters();
			[Test]
			procedure TestLoadFromStreamReader_SearchWithMatches();
			[Test]
			procedure TestLoadFromStreamReader_ReplaceWithMatches();
			[Test]
			procedure TestLoadFromStreamReader_EmptyMatches();
			[Test]
			procedure TestLoadFromStreamReader_WithSpecialCharacters();
			[Test]
			procedure TestRoundtripStreamPersistence_AllCombinations();
			[Test]
			procedure TestRoundtripStreamPersistence_EdgeCases();
			[Test]
			procedure TestStreamPersistence_LargeData();
			[Test]
			procedure TestStreamPersistence_UTF8Encoding();
			[Test]
			procedure TestLoadFromStreamReader_InvalidFormat();

			// New TestCase-based tests for SearchPath and FileMasks persistence
			[Test]
			[TestCase('Empty path', '')]
			[TestCase('Single path', 'C:\TestPath')]
			[TestCase('Multiple paths with separator', 'C:\Path1;D:\Path2;E:\Path3')]
			[TestCase('Path with spaces', 'C:\Path With Spaces')]
			[TestCase('Network path', '\\server\share\folder')]
			[TestCase('Relative path', '..\relative\path')]
			procedure TestSearchPathPersistence(const searchPath : string);

			[Test]
			[TestCase('Empty masks', '')]
			[TestCase('Single mask', '*.pas')]
			[TestCase('Multiple masks', '*.pas;*.dfm;*.inc')]
			[TestCase('Complex masks', '*.{pas,dfm,inc}')]
			[TestCase('Exclusion masks', '!*.bak;*.pas')]
			[TestCase('Pattern with wildcards', '*test*.pas')]
			procedure TestFileMasksPersistence(const fileMasks : string);

			[Test]
			[TestCase('All false', 'False,False,False,0,')]
			[TestCase('All true with context', 'True,True,True,5,utf-8')]
			[TestCase('Mixed settings', 'True,False,True,3,latin-1')]
			[TestCase('With encoding only', 'False,False,False,0,utf-16')]
			procedure TestSearchFormSettingsPersistence(const hidden : Boolean; const noIgnore : Boolean; const pretty : Boolean;
				const context : Integer; const encoding : string);

			[Test]
			[TestCase('Valid search path and masks', 'C:\TestPath,*.pas;*.dfm')]
			[TestCase('Empty path with masks', ',*.txt')]
			[TestCase('Path with empty masks', 'D:\SomePath,')]
			[TestCase('Both empty', ',')]
			procedure TestRoundtripPersistence_PathAndMasks(const searchPath : string; const fileMasks : string);
	end;

implementation

uses

	RipGrepper.Common.SimpleTypes,
	System.SysUtils,
	RipGrepper.Common.Constants,
	RipGrepper.Settings.SettingsDictionary,
	ArrayEx,
	RipGrepper.Helper.StreamReaderWriter,
	RipGrepper.Common.ParsedObject,
	RipGrepper.Helper.Types;

constructor THistoryItemObjectTest.Create();
begin
	inherited;
	FArrayComparer := TComparer < TArray < string >>.Construct(
		function(const Left, Right : TArray<string>) : Integer
		begin
			Result := TComparer<string>.Default.Compare(string.Join('', Left), string.Join('', Right));
		end);

end;

procedure THistoryItemObjectTest.AddUniqueSearchText(const _ripGrepArgs : IShared<TRipGrepArguments>; const _searchText : string);
begin
	// Add the new unique search text
	_ripGrepArgs.AddPairUnique(RG_ARG_SEARCH_TEXT, _searchText);
end;

procedure THistoryItemObjectTest.AssertContainsIntAndStrSetting(hio : IHistoryItemObject);
var
	arr : TArray<TArray<string>>;
	arrEx : TArrayEx<TArray<string>>;
	cont : Boolean;
	dictContent : string;
begin
	arr := GetSettingsDictAsArray(hio);
	arrEx := arr;
	var
		inta : TArray<string> := [INT_SETTING_KEY, INT_SETTING_VAL.ToString];
	dictContent := 'SettingsDict count:' + arrEx.Count.ToString + CRLF + 'SettingsDict content:' + CRLF;
	for var a : TArray<string> in arr do begin
		dictContent := dictContent + string.Join('CRLF', a);
	end;
	cont := arrEx.Contains(inta, FArrayComparer);
	Assert.IsTrue(cont,
	{ } Format(dictContent + CRLF + ' SettingsDict[%s] = %s should contain Int Setting', [INT_SETTING_KEY, INT_SETTING_VAL.ToString]));

	inta := [STR_SETTING_KEY, STR_SETTING_VAL];
	cont := arrEx.Contains(inta, FArrayComparer);
	Assert.IsTrue(cont, Format(dictContent + CRLF + ' SettingsDict[%s] = %s should contain Str Setting',
		[STR_SETTING_KEY, STR_SETTING_VAL]));
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
	AddUniqueSearchText(FRipGrepArguments, 'search text');
	FRipGrepArguments.AddPair(RG_ARG_SEARCH_PATH, 'C:\Path\Search\Files');

	FStrSetting := TStringSetting.Create(STR_SETTING_KEY, STR_SETTING_VAL);
	FIntSetting := TIntegerSetting.Create(INT_SETTING_KEY, INT_SETTING_VAL);
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
	arrHio : TArray<string>;
	arrOther : TArray<string>;
begin
	hio := Shared.Make<THistoryItemObject>(THistoryItemObject.Create);
	hio.GuiSearchTextParams := FGuiSearchTextParams;
	hio.RipGrepArguments := FRipGrepArguments;
	hio.SearchFormSettings.Init;

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
	arrHio := hio.RipGrepArguments.ToStringArray;
	arrOther := other.RipGrepArguments.ToStringArray;
	Assert.AreEqual(string.Join(';', arrHio), string.Join(';', arrOther),
	{ } 'RipGrepArguments content should match the expected serialized data');
end;

procedure THistoryItemObjectTest.TestSaveLoadListFromStream();
var
	hioLoaded : IHistoryItemObject;
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
		hioLoaded := THistoryItemObject.Create();
		hioLoaded.LoadFromStreamReader(sr);
		hioList.Add(hioLoaded);
	end;

	for var i := 0 to count - 1 do begin
		hioLoaded := hioList[i];
		Assert.AreEqual(TEST_SEARCH_TEXT + i.ToString,
		{ } hioLoaded.GuiSearchTextParams.SearchTextWithOptions.SearchTextOfUser,
		{ } 'SearchText content should match the expected serialized data');
		var
		arr := hioLoaded.RipGrepArguments.ToStringArray;
		var
		arr2 := FHistoryObjectList[i].RipGrepArguments.ToStringArray;
		var
		hioArgs := string.Join(';', arr);
		var
		hioArgs2 := string.Join(';', arr2);

		Assert.AreEqual(hioArgs2, hioArgs,
		{ } 'RipGrepArguments content should match the expected serialized data');

		AssertContainsIntAndStrSetting(hioLoaded);
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
		hio.RipGrepArguments.Assign(FRipGrepArguments);

		AddUniqueSearchText(hio.RipGrepArguments, searchText);

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
	AddUniqueSearchText(Result.RipGrepArguments, 'test pattern');
	Result.RipGrepArguments.AddPair(RG_ARG_SEARCH_PATH, 'C:\Test\Path');
	Result.RipGrepArguments.AddPair(RG_ARG_OPTIONS, '-g *.pas');

	// Create some sample matches
	Result.ShouldSaveResult := True;

	// Create first match
	row := TParsedObjectRow.Create( { nil, ptRipGrepSearch } );
	colData.Clear();
	colData.Add(TColumnData.New(ciFile, 'C:\Test\File1.pas'));
	colData.Add(TColumnData.New(ciRow, '10'));
	colData.Add(TColumnData.New(ciColBegin, '5'));
	colData.Add(TColumnData.New(ciColEnd, '6'));
	colData.Add(TColumnData.New(ciText, 'This is a test pattern match'));
	colData.Add(TColumnData.New(ciMatchText, 'test pattern'));
	colData.Add(TColumnData.New(ciTextAfterMatch, ' match'));
	row.Columns := colData;
	Result.Matches.Items.Add(row);

	// Create second match
	row := TParsedObjectRow.Create( { nil, ptRipGrepSearch } );
	colData.Clear();
	colData.Add(TColumnData.New(ciFile, 'C:\Test\File2.pas'));
	colData.Add(TColumnData.New(ciRow, '15'));
	colData.Add(TColumnData.New(ciColBegin, '8'));
	colData.Add(TColumnData.New(ciColEnd, '9'));
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
	AddUniqueSearchText(Result.RipGrepArguments, 'nonexistent pattern');
	Result.RipGrepArguments.AddPair(RG_ARG_SEARCH_PATH, 'C:\Empty\Path');
	Result.RipGrepArguments.AddPair(RG_ARG_OPTIONS, '-g *.text');

	// Create "no output" result
	Result.ShouldSaveResult := True;
	row := TParsedObjectRow.Create( { nil, ptRipGrepSearch } );
	colData.Clear();
	colData.Add(TColumnData.New(ciFile, 'rg.exe' + RG_HAS_NO_OUTPUT));
	colData.Add(TColumnData.New(ciRow, ''));
	colData.Add(TColumnData.New(ciColBegin, ''));
	colData.Add(TColumnData.New(ciColEnd, ''));
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
	AddUniqueSearchText(Result.RipGrepArguments, 'new_value');
	Result.RipGrepArguments.AddPair(RG_ARG_OPTIONS, '--word-regexp');
	// Note: In replace mode, we typically only have one search text, but keeping the pattern
	// The second AddUniqueSearchText will remove the first and add this one
	AddUniqueSearchText(Result.RipGrepArguments, 'old_value');
	Result.RipGrepArguments.AddPair(RG_ARG_SEARCH_PATH, 'C:\Replace\Path');
	Result.RipGrepArguments.AddPair(RG_ARG_OPTIONS, '-g *.pas');
	Result.RipGrepArguments.AddPair(RG_ARG_OPTIONS, '-g *.inc');

	// Create some sample replace matches
	Result.ShouldSaveResult := True;

	// Create first replace match
	row := TParsedObjectRow.Create( { nil, ptRipGrepSearch } );
	colData.Clear();
	colData.Add(TColumnData.New(ciFile, 'C:\Replace\File1.pas'));
	colData.Add(TColumnData.New(ciRow, '20'));
	colData.Add(TColumnData.New(ciColBegin, '5'));
	colData.Add(TColumnData.New(ciColEnd, '13'));
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
		Assert.AreEqual(6, historyItem.RipGrepArguments.Count, 'Should have 6 RipGrep arguments');
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
		Assert.AreEqual(6, loadedItem.RipGrepArguments.Count, 'RipGrep arguments should be restored');
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
		Assert.IsFalse(loadedItem.IsReplaceMode, 'CRITICAL FAULT: Search item became replace item after serialization!');
		Assert.AreEqual('', loadedItem.ReplaceText, 'CRITICAL FAULT: Search item should have empty replace text after deserialization!');
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
		Assert.IsTrue(loadedItem.IsReplaceMode, 'CRITICAL FAULT: Replace item became search item after serialization!');
		Assert.AreEqual('new_value', loadedItem.ReplaceText,
			'CRITICAL FAULT: Replace item should preserve replace text after deserialization!');
		Assert.AreEqual(originalItem.GuiSearchTextParams.ToLogString, loadedItem.GuiSearchTextParams.ToLogString,
			'CRITICAL FAULT: Replace item should preserve GUI search text parameters after deserialization!');
		Assert.AreEqual(originalItem.SearchText, loadedItem.SearchText, 'Search text should be preserved');
	finally
		stream.Free();
	end;
end;

procedure THistoryItemObjectTest.MultipleSearchItemsSerializationShouldPreserveMode();
var
	searchItems : TArrayEx<IHistoryItemObject>;
	loadedItems : TArrayEx<IHistoryItemObject>;
	stream : TMemoryStream;
	sw : TStreamWriter;
	sr : TStreamReader;
begin
	// Arrange: Create multiple search items
	searchItems.Add(CreateSampleHistoryItemWithSearchAndMatches());
	searchItems.Add(CreateSampleHistoryItemWithSearchAndNoMatches());
	searchItems.Add(CreateSampleHistoryItemWithSearchAndMatches());

	stream := TMemoryStream.Create();
	try
		sw := TStreamWriter.Create(stream);
		try
			// Save multiple search items
			sw.WriteLine(searchItems.Count.ToString);
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
			var
			count := StrToInt(sr.ReadLine());
			for var i := 0 to count - 1 do begin
				loadedItems.Add(THistoryItemObject.Create());
				loadedItems[i].LoadFromStreamReader(sr);
			end;
		finally
			sr.Free();
		end;

		// Assert: All loaded items should still be search items
		for var i := 0 to loadedItems.Count - 1 do begin
			Assert.IsFalse(loadedItems[i].IsReplaceMode,
				Format('CRITICAL FAULT: Search item %d became replace item after serialization!', [i]));
			Assert.AreEqual('', loadedItems[i].ReplaceText, Format('CRITICAL FAULT: Search item %d should have empty replace text!', [i]));
		end;
	finally
		stream.Free();
	end;
end;

procedure THistoryItemObjectTest.MultipleReplaceItemsSerializationShouldPreserveMode();
var
	replaceItems : TArrayEx<IHistoryItemObject>;
	loadedItems : TArrayEx<IHistoryItemObject>;
	stream : TMemoryStream;
	sw : TStreamWriter;
	sr : TStreamReader;
begin
	// Arrange: Create multiple replace items
	replaceItems.Add(CreateSampleHistoryItemWithReplaceAndMatches());
	replaceItems.Add(CreateSampleHistoryItemWithReplaceAndMatches());
	replaceItems.Add(CreateSampleHistoryItemWithReplaceAndMatches());

	stream := TMemoryStream.Create();
	try
		sw := TStreamWriter.Create(stream);
		try
			// Save multiple replace items
			sw.WriteLine(replaceItems.Count.ToString);
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
			var
			count := StrToInt(sr.ReadLine());
			for var i := 0 to count - 1 do begin
				loadedItems.Add(THistoryItemObject.Create());
				loadedItems[i].LoadFromStreamReader(sr);
			end;
		finally
			sr.Free();
		end;

		// Assert: All loaded items should still be replace items
		for var i := 0 to loadedItems.Count - 1 do begin
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
				Assert.AreEqual(expectedModes[i], items[i].IsReplaceMode, Format('Original item %d should have correct mode', [i]));
				items[i].SaveToStreamWriter(sw);
			end;
		finally
			sw.Free();
		end;

		// Act: Load items back
		stream.Position := 0;
		sr := TStreamReader.Create(stream, TEncoding.UTF8);
		try
			var
			count := StrToInt(sr.ReadLine());
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
				Format('CRITICAL FAULT: Item %d mode changed from %s to %s after serialization!', [i, BoolToStr(expectedModes[i], True),
				BoolToStr(loadedItems[i].IsReplaceMode, True)]));

			if expectedModes[i] then begin
				Assert.AreNotEqual('', loadedItems[i].ReplaceText, Format('Replace item %d should have non-empty replace text', [i]));
			end else begin
				Assert.AreEqual('', loadedItems[i].ReplaceText, Format('Search item %d should have empty replace text', [i]));
			end;
		end;
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

		Assert.IsFalse(loadedSearchItem.IsReplaceMode, 'CRITICAL FAULT: Search item with special chars became replace item!');
		Assert.AreEqual(specialText, loadedSearchItem.SearchText, 'Special search text should be preserved');

		// Test replace item
		stream.Clear();
		replaceItem.SaveToStream(stream);
		stream.Position := 0;
		loadedReplaceItem := THistoryItemObject.Create();
		loadedReplaceItem.LoadFromStream(stream);

		Assert.IsTrue(loadedReplaceItem.IsReplaceMode, 'CRITICAL FAULT: Replace item with special chars became search item!');
		Assert.AreEqual(specialText, loadedReplaceItem.SearchText, 'Special search text should be preserved');
		Assert.AreEqual('replace with "special" & <chars>', loadedReplaceItem.ReplaceText, 'Special replace text should be preserved');
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
		colData.Add(TColumnData.New(ciColBegin, '5'));
		colData.Add(TColumnData.New(ciColEnd, '6'));
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

		Assert.IsFalse(loadedSearchItem.IsReplaceMode, 'CRITICAL FAULT: Large search item became replace item after serialization!');
		Assert.AreEqual(largeSearchText, loadedSearchItem.SearchText, 'Large search text should be preserved');
		Assert.AreEqual('', loadedSearchItem.ReplaceText, 'Search item should have empty replace text');

		// Test replace item with large data
		stream.Clear();
		replaceItem.SaveToStream(stream);
		stream.Position := 0;
		loadedReplaceItem := THistoryItemObject.Create();
		loadedReplaceItem.LoadFromStream(stream);

		Assert.IsTrue(loadedReplaceItem.IsReplaceMode, 'CRITICAL FAULT: Large replace item became search item after serialization!');
		Assert.AreEqual(largeSearchText, loadedReplaceItem.SearchText, 'Large search text should be preserved');
		Assert.AreEqual(largeReplaceText, loadedReplaceItem.ReplaceText, 'Large replace text should be preserved');
	finally
		stream.Free();
	end;
end;

procedure THistoryItemObjectTest.EmptyTest;
begin
	Assert.IsFalse(THistoryItemObject.Create().IsReplaceMode, 'CRITICAL FAULT: Empty item became replace item!');
end;

// New comprehensive StreamWriter/StreamReader test implementations

procedure THistoryItemObjectTest.TestSaveToStreamWriter_SearchWithMatches();
var
	historyItem : IHistoryItemObject;
	stream : IShared<TMemoryStream>;
	writer : IShared<TStreamWriter>;
	reader : IShared<TStreamReader>;
	lines : TArray<string>;
	i : Integer;
begin
	// Arrange
	historyItem := CreateSampleHistoryItemWithSearchAndMatches();
	stream := Shared.Make<TMemoryStream>(TMemoryStream.Create());
	writer := Shared.Make<TStreamWriter>(TStreamWriter.Create(stream, TEncoding.UTF8));

	// Act
	historyItem.SaveToStreamWriter(writer);
	writer.Flush();

	// Assert - Read the stream content line by line
	stream.Position := 0;
	reader := Shared.Make<TStreamReader>(TStreamReader.Create(stream, TEncoding.UTF8));
	SetLength(lines, 0);
	while not reader.EndOfStream do begin
		SetLength(lines, Length(lines) + 1);
		lines[high(lines)] := reader.ReadLine();
	end;

	Assert.IsTrue(Length(lines) > 0, 'Stream should contain multiple lines of data');
	// The first part should contain GuiSearchTextParams data
	var
	foundSearchText := False;
	for i := 0 to high(lines) do begin
		if lines[i] = 'test pattern' then begin
			foundSearchText := True;
			break;
		end;
	end;
	Assert.IsTrue(foundSearchText, 'Stream should contain the search text');
end;

procedure THistoryItemObjectTest.TestSaveToStreamWriter_ReplaceWithMatches();
var
	historyItem : IHistoryItemObject;
	stream : IShared<TMemoryStream>;
	writer : IShared<TStreamWriter>;
	reader : IShared<TStreamReader>;
	lines : TArray<string>;
	i : Integer;
begin
	// Arrange
	historyItem := CreateSampleHistoryItemWithReplaceAndMatches();
	stream := Shared.Make<TMemoryStream>(TMemoryStream.Create());
	writer := Shared.Make<TStreamWriter>(TStreamWriter.Create(stream, TEncoding.UTF8));

	// Act
	historyItem.SaveToStreamWriter(writer);
	writer.Flush();

	// Assert - Read the stream content
	stream.Position := 0;
	reader := Shared.Make<TStreamReader>(TStreamReader.Create(stream, TEncoding.UTF8));
	SetLength(lines, 0);
	while not reader.EndOfStream do begin
		SetLength(lines, Length(lines) + 1);
		lines[high(lines)] := reader.ReadLine();
	end;

	Assert.IsTrue(Length(lines) > 0, 'Stream should contain multiple lines of data');
	// Should contain both search text and replace text
	var
	foundSearchText := False;
	var
	foundReplaceText := False;
	for i := 0 to high(lines) do begin
		if lines[i] = 'old_value' then
			foundSearchText := True;
		if lines[i] = 'new_value' then
			foundReplaceText := True;
	end;
	Assert.IsTrue(foundSearchText, 'Stream should contain the search text');
	Assert.IsTrue(foundReplaceText, 'Stream should contain the replace text');
end;

procedure THistoryItemObjectTest.TestSaveToStreamWriter_EmptyMatches();
var
	historyItem : IHistoryItemObject;
	stream : IShared<TMemoryStream>;
	writer : IShared<TStreamWriter>;
	reader : IShared<TStreamReader>;
	lines : TArray<string>;
begin
	// Arrange
	historyItem := CreateSampleHistoryItemWithSearchAndNoMatches();
	stream := Shared.Make<TMemoryStream>(TMemoryStream.Create());
	writer := Shared.Make<TStreamWriter>(TStreamWriter.Create(stream, TEncoding.UTF8));

	// Act
	historyItem.SaveToStreamWriter(writer);
	writer.Flush();

	// Assert
	stream.Position := 0;
	reader := Shared.Make<TStreamReader>(TStreamReader.Create(stream, TEncoding.UTF8));
	SetLength(lines, 0);
	while not reader.EndOfStream do begin
		SetLength(lines, Length(lines) + 1);
		lines[high(lines)] := reader.ReadLine();
	end;

	Assert.IsTrue(Length(lines) > 0, 'Stream should contain data even with no matches');
	Assert.IsTrue(stream.Size > 0, 'Stream should have content');
end;

procedure THistoryItemObjectTest.TestSaveToStreamWriter_WithSpecialCharacters();
var
	historyItem : IHistoryItemObject;
	guiParams : IShared<TGuiSearchTextParams>;
	stream : IShared<TMemoryStream>;
	writer : IShared<TStreamWriter>;
	reader : IShared<TStreamReader>;
	specialText : string;
	lines : TArray<string>;
	i : Integer;
begin
	// Arrange
	specialText := 'test with "quotes" & <brackets> \backslash/ forward αβγ unicode';
	historyItem := THistoryItemObject.Create();
	guiParams := Shared.Make<TGuiSearchTextParams>(TGuiSearchTextParams.Create('TestSection'));
	guiParams.SearchTextWithOptions.SearchTextOfUser := specialText;
	guiParams.IsReplaceMode := False;
	historyItem.GuiSearchTextParams := guiParams;
	AddUniqueSearchText(historyItem.RipGrepArguments, specialText);

	stream := Shared.Make<TMemoryStream>(TMemoryStream.Create());
	writer := Shared.Make<TStreamWriter>(TStreamWriter.Create(stream, TEncoding.UTF8));

	// Act
	historyItem.SaveToStreamWriter(writer);
	writer.Flush();

	// Assert
	stream.Position := 0;
	reader := Shared.Make<TStreamReader>(TStreamReader.Create(stream, TEncoding.UTF8));
	SetLength(lines, 0);
	while not reader.EndOfStream do begin
		SetLength(lines, Length(lines) + 1);
		lines[high(lines)] := reader.ReadLine();
	end;

	var
	foundSpecialText := False;
	for i := 0 to high(lines) do begin
		if lines[i] = specialText then begin
			foundSpecialText := True;
			break;
		end;
	end;
	Assert.IsTrue(foundSpecialText, 'Stream should properly encode special characters');
end;

procedure THistoryItemObjectTest.TestLoadFromStreamReader_SearchWithMatches();
var
	originalItem : IHistoryItemObject;
	loadedItem : IHistoryItemObject;
	stream : IShared<TMemoryStream>;
	writer : IShared<TStreamWriter>;
	reader : IShared<TStreamReader>;
begin
	// Arrange
	originalItem := CreateSampleHistoryItemWithSearchAndMatches();
	stream := Shared.Make<TMemoryStream>(TMemoryStream.Create());
	writer := Shared.Make<TStreamWriter>(TStreamWriter.Create(stream, TEncoding.UTF8));

	// Save using SaveToStreamWriter
	originalItem.SaveToStreamWriter(writer);
	writer.Flush();

	// Act - Load using LoadFromStreamReader
	stream.Position := 0;
	reader := Shared.Make<TStreamReader>(TStreamReader.Create(stream, TEncoding.UTF8));
	loadedItem := THistoryItemObject.Create();
	loadedItem.LoadFromStreamReader(reader);

	// Assert
	Assert.IsTrue(loadedItem.IsLoadedFromStream, 'Should be marked as loaded from stream');
	Assert.AreEqual(originalItem.SearchText, loadedItem.SearchText, 'Search text should match');
	Assert.AreEqual(originalItem.IsReplaceMode, loadedItem.IsReplaceMode, 'Replace mode should match');
	Assert.AreEqual(originalItem.RipGrepArguments.Count, loadedItem.RipGrepArguments.Count, 'RipGrep arguments count should match');
end;

procedure THistoryItemObjectTest.TestLoadFromStreamReader_ReplaceWithMatches();
var
	originalItem : IHistoryItemObject;
	loadedItem : IHistoryItemObject;
	stream : IShared<TMemoryStream>;
	writer : IShared<TStreamWriter>;
	reader : IShared<TStreamReader>;
begin
	// Arrange
	originalItem := CreateSampleHistoryItemWithReplaceAndMatches();
	stream := Shared.Make<TMemoryStream>(TMemoryStream.Create());
	writer := Shared.Make<TStreamWriter>(TStreamWriter.Create(stream, TEncoding.UTF8));

	// Save using SaveToStreamWriter
	originalItem.SaveToStreamWriter(writer);
	writer.Flush();

	// Act - Load using LoadFromStreamReader
	stream.Position := 0;
	reader := Shared.Make<TStreamReader>(TStreamReader.Create(stream, TEncoding.UTF8));
	loadedItem := THistoryItemObject.Create();
	loadedItem.LoadFromStreamReader(reader);

	// Assert
	Assert.IsTrue(loadedItem.IsLoadedFromStream, 'Should be marked as loaded from stream');
	Assert.AreEqual(originalItem.SearchText, loadedItem.SearchText, 'Search text should match');
	Assert.IsTrue(loadedItem.IsReplaceMode, 'Should be in replace mode');
	Assert.AreEqual(originalItem.ReplaceText, loadedItem.ReplaceText, 'Replace text should match');
	Assert.AreEqual(originalItem.RipGrepArguments.Count, loadedItem.RipGrepArguments.Count, 'RipGrep arguments count should match');
end;

procedure THistoryItemObjectTest.TestLoadFromStreamReader_EmptyMatches();
var
	originalItem : IHistoryItemObject;
	loadedItem : IHistoryItemObject;
	stream : IShared<TMemoryStream>;
	writer : IShared<TStreamWriter>;
	reader : IShared<TStreamReader>;
begin
	// Arrange
	originalItem := CreateSampleHistoryItemWithSearchAndNoMatches();
	stream := Shared.Make<TMemoryStream>(TMemoryStream.Create());
	writer := Shared.Make<TStreamWriter>(TStreamWriter.Create(stream, TEncoding.UTF8));

	// Save using SaveToStreamWriter
	originalItem.SaveToStreamWriter(writer);
	writer.Flush();

	// Act - Load using LoadFromStreamReader
	stream.Position := 0;
	reader := Shared.Make<TStreamReader>(TStreamReader.Create(stream, TEncoding.UTF8));
	loadedItem := THistoryItemObject.Create();
	loadedItem.LoadFromStreamReader(reader);

	// Assert
	Assert.IsTrue(loadedItem.IsLoadedFromStream, 'Should be marked as loaded from stream');
	Assert.AreEqual(originalItem.SearchText, loadedItem.SearchText, 'Search text should match');
	Assert.IsFalse(loadedItem.IsReplaceMode, 'Should not be in replace mode');
	Assert.AreEqual('', loadedItem.ReplaceText, 'Replace text should be empty');
	Assert.AreEqual(originalItem.RipGrepArguments.Count, loadedItem.RipGrepArguments.Count, 'RipGrep arguments count should match');
end;

procedure THistoryItemObjectTest.TestLoadFromStreamReader_WithSpecialCharacters();
var
	originalItem : IHistoryItemObject;
	loadedItem : IHistoryItemObject;
	guiParams : IShared<TGuiSearchTextParams>;
	stream : IShared<TMemoryStream>;
	writer : IShared<TStreamWriter>;
	reader : IShared<TStreamReader>;
	specialText : string;
begin
	// Arrange
	specialText := 'pattern with "quotes" & <brackets> \backslash/ αβγδε unicode \n\t';
	originalItem := THistoryItemObject.Create();
	guiParams := Shared.Make<TGuiSearchTextParams>(TGuiSearchTextParams.Create('TestSection'));
	guiParams.SearchTextWithOptions.SearchTextOfUser := specialText;
	guiParams.IsReplaceMode := True;
	guiParams.ReplaceText := 'replace with special chars αβγ';
	originalItem.GuiSearchTextParams := guiParams;
	AddUniqueSearchText(originalItem.RipGrepArguments, specialText);

	stream := Shared.Make<TMemoryStream>(TMemoryStream.Create());
	writer := Shared.Make<TStreamWriter>(TStreamWriter.Create(stream, TEncoding.UTF8));

	// Save using SaveToStreamWriter
	originalItem.SaveToStreamWriter(writer);
	writer.Flush();

	// Act - Load using LoadFromStreamReader
	stream.Position := 0;
	reader := Shared.Make<TStreamReader>(TStreamReader.Create(stream, TEncoding.UTF8));
	loadedItem := THistoryItemObject.Create();
	loadedItem.LoadFromStreamReader(reader);

	// Assert
	Assert.AreEqual(specialText, loadedItem.SearchText, 'Special characters in search text should be preserved');
	Assert.AreEqual('replace with special chars αβγ', loadedItem.ReplaceText, 'Special characters in replace text should be preserved');
	Assert.IsTrue(loadedItem.IsReplaceMode, 'Replace mode should be preserved');
end;

procedure THistoryItemObjectTest.TestRoundtripStreamPersistence_AllCombinations();
var
	testItems : TArray<IHistoryItemObject>;
	loadedItems : TArray<IHistoryItemObject>;
	stream : IShared<TMemoryStream>;
	writer : IShared<TStreamWriter>;
	reader : IShared<TStreamReader>;
	i : Integer;
begin
	// Arrange - Create test items with different combinations
	SetLength(testItems, 3);
	testItems[0] := CreateSampleHistoryItemWithSearchAndMatches();
	testItems[1] := CreateSampleHistoryItemWithReplaceAndMatches();
	testItems[2] := CreateSampleHistoryItemWithSearchAndNoMatches();

	for i := 0 to high(testItems) do begin
		// Act - Save and load each item
		stream := Shared.Make<TMemoryStream>(TMemoryStream.Create());
		writer := Shared.Make<TStreamWriter>(TStreamWriter.Create(stream, TEncoding.UTF8));

		testItems[i].SaveToStreamWriter(writer);
		writer.Flush();

		stream.Position := 0;
		reader := Shared.Make<TStreamReader>(TStreamReader.Create(stream, TEncoding.UTF8));
		SetLength(loadedItems, i + 1);
		loadedItems[i] := THistoryItemObject.Create();
		loadedItems[i].LoadFromStreamReader(reader);

		// Assert - All properties should be preserved
		Assert.AreEqual(testItems[i].SearchText, loadedItems[i].SearchText, Format('Search text should be preserved for item %d', [i]));
		Assert.AreEqual(testItems[i].IsReplaceMode, loadedItems[i].IsReplaceMode,
			Format('Replace mode should be preserved for item %d', [i]));
		Assert.AreEqual(testItems[i].ReplaceText, loadedItems[i].ReplaceText, Format('Replace text should be preserved for item %d', [i]));
		Assert.AreEqual(testItems[i].RipGrepArguments.Count, loadedItems[i].RipGrepArguments.Count,
			Format('RipGrep arguments count should be preserved for item %d', [i]));
	end;
end;

procedure THistoryItemObjectTest.TestRoundtripStreamPersistence_EdgeCases();
var
	edgeCaseItems : TArray<IHistoryItemObject>;
	loadedItem : IHistoryItemObject;
	stream : IShared<TMemoryStream>;
	writer : IShared<TStreamWriter>;
	reader : IShared<TStreamReader>;
	guiParams : IShared<TGuiSearchTextParams>;
	i : Integer;
	edgeCaseTexts : TArray<string>;
begin
	// Arrange - Create items with edge case texts
	edgeCaseTexts := [
	// '', // Empty text - but this would need non-empty placeholder first
		' ', // Single space
	#9, // Tab character
	'text with spaces and \ttabs', 'text with "double quotes" and ''single quotes''', 'text with [brackets] and (parentheses) and {braces}',
		'unicode: αβγδεζ θικλμ', 'regex pattern: \d+\.\w+\s*', 'very long text: ' + StringOfChar('A', 500)];

	SetLength(edgeCaseItems, Length(edgeCaseTexts));

	for i := 0 to high(edgeCaseTexts) do begin
		// Create item with edge case text
		edgeCaseItems[i] := THistoryItemObject.Create();
		guiParams := Shared.Make<TGuiSearchTextParams>(TGuiSearchTextParams.Create('TestSection'));

		// Handle empty string case
		if edgeCaseTexts[i] = '' then begin
			guiParams.SearchTextWithOptions.SearchTextOfUser := 'placeholder';
			guiParams.SearchTextWithOptions.SearchTextOfUser := ''; // Set empty after creation
		end else begin
			guiParams.SearchTextWithOptions.SearchTextOfUser := edgeCaseTexts[i];
		end;

		guiParams.IsReplaceMode := (i mod 2) = 1; // Alternate replace mode
		if guiParams.IsReplaceMode then begin
			guiParams.ReplaceText := 'replaced_' + IntToStr(i);
		end;
		edgeCaseItems[i].GuiSearchTextParams := guiParams;
		AddUniqueSearchText(edgeCaseItems[i].RipGrepArguments, edgeCaseTexts[i]);

		// Act - Save and load
		stream := Shared.Make<TMemoryStream>(TMemoryStream.Create());
		writer := Shared.Make<TStreamWriter>(TStreamWriter.Create(stream, TEncoding.UTF8));

		edgeCaseItems[i].SaveToStreamWriter(writer);
		writer.Flush();

		stream.Position := 0;
		reader := Shared.Make<TStreamReader>(TStreamReader.Create(stream, TEncoding.UTF8));
		loadedItem := THistoryItemObject.Create();
		loadedItem.LoadFromStreamReader(reader);

		// Assert
		Assert.AreEqual(edgeCaseTexts[i], loadedItem.SearchText, Format('Edge case text %d should be preserved: "%s"',
			[i, edgeCaseTexts[i]]));
		Assert.AreEqual(edgeCaseItems[i].IsReplaceMode, loadedItem.IsReplaceMode,
			Format('Replace mode should be preserved for edge case %d', [i]));
		if edgeCaseItems[i].IsReplaceMode then begin
			Assert.AreEqual(edgeCaseItems[i].ReplaceText, loadedItem.ReplaceText,
				Format('Replace text should be preserved for edge case %d', [i]));
		end;
	end;
end;

procedure THistoryItemObjectTest.TestStreamPersistence_LargeData();
var
	historyItem : IHistoryItemObject;
	loadedItem : IHistoryItemObject;
	guiParams : IShared<TGuiSearchTextParams>;
	stream : IShared<TMemoryStream>;
	writer : IShared<TStreamWriter>;
	reader : IShared<TStreamReader>;
	largeText : string;
	row : IParsedObjectRow;
	colData : TArrayEx<TColumnData>;
begin
	// Arrange - Create item with large amounts of data
	largeText := 'Large search pattern: ' + StringOfChar('X', 2000);
	historyItem := THistoryItemObject.Create();
	guiParams := Shared.Make<TGuiSearchTextParams>(TGuiSearchTextParams.Create('TestSection'));
	guiParams.SearchTextWithOptions.SearchTextOfUser := largeText;
	guiParams.IsReplaceMode := True;
	guiParams.ReplaceText := 'Large replace text: ' + StringOfChar('Y', 1000);
	historyItem.GuiSearchTextParams := guiParams;

	// Add many RipGrep arguments
	for var i := 1 to 50 do begin
		historyItem.RipGrepArguments.AddPair(RG_ARG_OPTIONS, '--option' + IntToStr(i));
	end;

	// Add many matches
	historyItem.ShouldSaveResult := True;
	for var i := 1 to 100 do begin
		row := TParsedObjectRow.Create();
		colData.Clear();
		colData.Add(TColumnData.New(ciFile, Format('C:\Large\File%d.pas', [i])));
		colData.Add(TColumnData.New(ciRow, IntToStr(i * 10)));
		colData.Add(TColumnData.New(ciText, largeText + ' match ' + IntToStr(i)));
		colData.Add(TColumnData.New(ciColBegin, IntToStr(i * 10)));
		colData.Add(TColumnData.New(ciColEnd, IntToStr(i * 10)));
		colData.Add(TColumnData.New(ciMatchText, 'match'));
		colData.Add(TColumnData.New(ciTextAfterMatch, ' ' + IntToStr(i)));
		row.Columns := colData;
		historyItem.Matches.Items.Add(row);
	end;

	stream := Shared.Make<TMemoryStream>(TMemoryStream.Create());
	writer := Shared.Make<TStreamWriter>(TStreamWriter.Create(stream, TEncoding.UTF8));

	// Act - Save and load large data
	historyItem.SaveToStreamWriter(writer);
	writer.Flush();

	stream.Position := 0;
	reader := Shared.Make<TStreamReader>(TStreamReader.Create(stream, TEncoding.UTF8));
	loadedItem := THistoryItemObject.Create();
	loadedItem.LoadFromStreamReader(reader);

	// Assert
	Assert.AreEqual(largeText, loadedItem.SearchText, 'Large search text should be preserved');
	Assert.IsTrue(loadedItem.IsReplaceMode, 'Replace mode should be preserved for large data');
	Assert.AreEqual(guiParams.ReplaceText, loadedItem.ReplaceText, 'Large replace text should be preserved');
	Assert.AreEqual(historyItem.RipGrepArguments.Count, loadedItem.RipGrepArguments.Count, 'Large number of arguments should be preserved');
	Assert.AreEqual(100, loadedItem.Matches.Items.Count, 'Large number of matches should be preserved');
end;

procedure THistoryItemObjectTest.TestStreamPersistence_UTF8Encoding();
var
	historyItem : IHistoryItemObject;
	loadedItem : IHistoryItemObject;
	guiParams : IShared<TGuiSearchTextParams>;
	stream : IShared<TMemoryStream>;
	writer : IShared<TStreamWriter>;
	reader : IShared<TStreamReader>;
	unicodeText, replaceText : string;
begin
	// Arrange - Create item with various Unicode characters
	unicodeText := 'Search: αβγδε ñáéíóú çñü 中文字符 עברית العربية русский 🚀🎯💻';
	replaceText := 'Replace: ΑΒΓΔΕفعلمكنت ÑÁÉÍÓÚ ЦНЮ 漢字 🔥⭐🌟';

	historyItem := THistoryItemObject.Create();
	guiParams := Shared.Make<TGuiSearchTextParams>(TGuiSearchTextParams.Create('TestSection'));
	guiParams.SearchTextWithOptions.SearchTextOfUser := unicodeText;
	guiParams.IsReplaceMode := True;
	guiParams.ReplaceText := replaceText;
	historyItem.GuiSearchTextParams := guiParams;
	AddUniqueSearchText(historyItem.RipGrepArguments, unicodeText);

	stream := Shared.Make<TMemoryStream>(TMemoryStream.Create());
	writer := Shared.Make<TStreamWriter>(TStreamWriter.Create(stream, TEncoding.UTF8));

	// Act - Save and load Unicode data
	historyItem.SaveToStreamWriter(writer);
	writer.Flush();

	stream.Position := 0;
	reader := Shared.Make<TStreamReader>(TStreamReader.Create(stream, TEncoding.UTF8));
	loadedItem := THistoryItemObject.Create();
	loadedItem.LoadFromStreamReader(reader);

	// Assert - Unicode characters should be perfectly preserved
	Assert.AreEqual(unicodeText, loadedItem.SearchText, 'Unicode search text should be perfectly preserved');
	Assert.AreEqual(replaceText, loadedItem.ReplaceText, 'Unicode replace text should be perfectly preserved');
	Assert.IsTrue(loadedItem.IsReplaceMode, 'Replace mode should be preserved with Unicode text');
end;

procedure THistoryItemObjectTest.TestLoadFromStreamReader_InvalidFormat();
var
	historyItem : IHistoryItemObject;
	stream : IShared<TMemoryStream>;
	writer : IShared<TStreamWriter>;
	reader : IShared<TStreamReader>;
	invalidData : string;
	exceptionRaised : Boolean;
begin
	// Arrange - Create stream with invalid format
	invalidData := 'This is not a valid history item format' + #13#10 + 'More invalid data';
	stream := Shared.Make<TMemoryStream>(TMemoryStream.Create());
	writer := Shared.Make<TStreamWriter>(TStreamWriter.Create(stream, TEncoding.UTF8));
	writer.Write(invalidData);
	writer.Flush();

	stream.Position := 0;
	reader := Shared.Make<TStreamReader>(TStreamReader.Create(stream, TEncoding.UTF8));
	historyItem := THistoryItemObject.Create();

	// Act & Assert - Should raise exception for invalid format
	exceptionRaised := False;
	try
		historyItem.LoadFromStreamReader(reader);
	except
		on E : Exception do begin
			exceptionRaised := True;
		end;
	end;

	Assert.IsTrue(exceptionRaised, 'LoadFromStreamReader should raise exception for invalid format');
end;

// New TestCase-based test implementations

procedure THistoryItemObjectTest.TestSearchPathPersistence(const searchPath : string);
var
	originalItem : IHistoryItemObject;
	loadedItem : IHistoryItemObject;
	stream : IShared<TMemoryStream>;
	writer : IShared<TStreamWriter>;
	reader : IShared<TStreamReader>;
	guiParams : IShared<TGuiSearchTextParams>;
begin
	// Arrange - Create item with specified search path
	originalItem := THistoryItemObject.Create();
	guiParams := Shared.Make<TGuiSearchTextParams>(TGuiSearchTextParams.Create('TestSection'));
	guiParams.SearchTextWithOptions.SearchTextOfUser := 'test search';
	guiParams.IsReplaceMode := False;
	originalItem.GuiSearchTextParams := guiParams;

	// Add search path to RipGrep arguments
	if not searchPath.IsEmpty then begin
		originalItem.RipGrepArguments.AddPair(RG_ARG_SEARCH_PATH, searchPath);
	end;

	stream := Shared.Make<TMemoryStream>(TMemoryStream.Create());
	writer := Shared.Make<TStreamWriter>(TStreamWriter.Create(stream, TEncoding.UTF8));

	// Act - Save and load
	originalItem.SaveToStreamWriter(writer);
	writer.Flush();

	stream.Position := 0;
	reader := Shared.Make<TStreamReader>(TStreamReader.Create(stream, TEncoding.UTF8));
	loadedItem := THistoryItemObject.Create();
	loadedItem.LoadFromStreamReader(reader);

	// Assert - SearchPath should be preserved
	var
	expectedSearchPath := searchPath;
	var
	actualSearchPaths := loadedItem.RipGrepArguments().GetSearchPath();
	var
	actualSearchPath := string.Join(';', actualSearchPaths);
	Assert.AreEqual(expectedSearchPath, actualSearchPath, Format('SearchPath should be preserved. Expected: "%s", Got: "%s"',
		[expectedSearchPath, actualSearchPath]));
end;

procedure THistoryItemObjectTest.TestFileMasksPersistence(const fileMasks : string);
var
	originalItem : IHistoryItemObject;
	loadedItem : IHistoryItemObject;
	stream : IShared<TMemoryStream>;
	writer : IShared<TStreamWriter>;
	reader : IShared<TStreamReader>;
	guiParams : IShared<TGuiSearchTextParams>;
begin
	// Arrange - Create item with specified file masks
	originalItem := THistoryItemObject.Create();
	guiParams := Shared.Make<TGuiSearchTextParams>(TGuiSearchTextParams.Create('TestSection'));
	guiParams.SearchTextWithOptions.SearchTextOfUser := 'test search';
	guiParams.IsReplaceMode := False;
	originalItem.GuiSearchTextParams := guiParams;

	// Add file masks to RipGrep arguments
	if not fileMasks.IsEmpty then begin
		var
		masks := fileMasks.Split([';']);
		for var mask in masks do begin
			originalItem.RipGrepArguments.AddPair(RG_ARG_OPTIONS, '-g=' + mask);
		end;
	end;

	stream := Shared.Make<TMemoryStream>(TMemoryStream.Create());
	writer := Shared.Make<TStreamWriter>(TStreamWriter.Create(stream, TEncoding.UTF8));

	// Act - Save and load
	originalItem.SaveToStreamWriter(writer);
	writer.Flush();

	stream.Position := 0;
	reader := Shared.Make<TStreamReader>(TStreamReader.Create(stream, TEncoding.UTF8));
	loadedItem := THistoryItemObject.Create();
	loadedItem.LoadFromStreamReader(reader);

	// Assert - FileMasks should be preserved
	var
	originalMaskCount := 0;
	var
	loadedMaskCount := 0;

	if not fileMasks.IsEmpty then begin
		originalMaskCount := Length(fileMasks.Split([';']));
	end;

	// Count -g= options in loaded arguments
	for var arg in loadedItem.RipGrepArguments.ToStringArray do begin
		if arg.Contains('-g=') then begin
			Inc(loadedMaskCount);
		end;
	end;

	Assert.AreEqual(originalMaskCount, loadedMaskCount,
	{ } Format('FileMasks count should be preserved. Expected: %d, Got: %d for masks: "%s"',
		{ } [originalMaskCount, loadedMaskCount, fileMasks]));
end;

procedure THistoryItemObjectTest.TestSearchFormSettingsPersistence(const hidden : Boolean; const noIgnore : Boolean; const pretty : Boolean;
const context : Integer; const encoding : string);
var
	originalItem : IHistoryItemObject;
	loadedItem : IHistoryItemObject;
	stream : IShared<TMemoryStream>;
	writer : IShared<TStreamWriter>;
	reader : IShared<TStreamReader>;
	guiParams : IShared<TGuiSearchTextParams>;
begin
	// Arrange - Create item with specified SearchFormSettings
	originalItem := THistoryItemObject.Create();
	guiParams := Shared.Make<TGuiSearchTextParams>(TGuiSearchTextParams.Create('TestSection'));
	guiParams.SearchTextWithOptions.SearchTextOfUser := 'test search';
	guiParams.IsReplaceMode := False;
	originalItem.GuiSearchTextParams := guiParams;

	// Set SearchFormSettings properties
	originalItem.SearchFormSettings.Hidden := hidden;
	originalItem.SearchFormSettings.NoIgnore := noIgnore;
	originalItem.SearchFormSettings.Pretty := pretty;
	originalItem.SearchFormSettings.Context := context;
	originalItem.SearchFormSettings.Encoding := encoding;

	stream := Shared.Make<TMemoryStream>(TMemoryStream.Create());
	writer := Shared.Make<TStreamWriter>(TStreamWriter.Create(stream, TEncoding.UTF8));

	// Act - Save and load
	originalItem.SaveToStreamWriter(writer);
	writer.Flush();

	stream.Position := 0;
	reader := Shared.Make<TStreamReader>(TStreamReader.Create(stream, TEncoding.UTF8));
	loadedItem := THistoryItemObject.Create();
	loadedItem.LoadFromStreamReader(reader);

	// Assert - All SearchFormSettings properties should be preserved
	Assert.AreEqual(hidden, loadedItem.SearchFormSettings.Hidden, Format('Hidden should be preserved: expected %s',
		[BoolToStr(hidden, True)]));
	Assert.AreEqual(noIgnore, loadedItem.SearchFormSettings.NoIgnore, Format('NoIgnore should be preserved: expected %s',
		[BoolToStr(noIgnore, True)]));
	Assert.AreEqual(pretty, loadedItem.SearchFormSettings.Pretty, Format('Pretty should be preserved: expected %s',
		[BoolToStr(pretty, True)]));
	Assert.AreEqual(context, loadedItem.SearchFormSettings.Context, Format('Context should be preserved: expected %d', [context]));
	Assert.AreEqual(encoding, loadedItem.SearchFormSettings.Encoding, Format('Encoding should be preserved: expected "%s"', [encoding]));
end;

procedure THistoryItemObjectTest.TestRoundtripPersistence_PathAndMasks(const searchPath : string; const fileMasks : string);
var
	originalItem : IHistoryItemObject;
	loadedItem : IHistoryItemObject;
	stream : IShared<TMemoryStream>;
	guiParams : IShared<TGuiSearchTextParams>;
begin
	// Arrange - Create item with both search path and file masks
	originalItem := THistoryItemObject.Create();
	guiParams := Shared.Make<TGuiSearchTextParams>(TGuiSearchTextParams.Create('TestSection'));
	guiParams.SearchTextWithOptions.SearchTextOfUser := 'roundtrip test';
	guiParams.IsReplaceMode := False;
	originalItem.GuiSearchTextParams := guiParams;

	// Add search path if not empty
	if not searchPath.IsEmpty then begin
		originalItem.RipGrepArguments.AddPair(RG_ARG_SEARCH_PATH, searchPath);
	end;

	// Add file masks if not empty
	if not fileMasks.IsEmpty then begin
		var
		masks := fileMasks.Split([';']);
		for var mask in masks do begin
			originalItem.RipGrepArguments.AddPair(RG_ARG_OPTIONS, '-g=' + mask);
		end;
	end;

	stream := Shared.Make<TMemoryStream>(TMemoryStream.Create());

	// Act - Save and load using SaveToStream/LoadFromStream (different from StreamWriter/Reader)
	originalItem.SaveToStream(stream);
	stream.Position := 0;
	loadedItem := THistoryItemObject.Create();
	loadedItem.LoadFromStream(stream);

	// Assert - Both search path and file masks should be preserved
	var
	actualSearchPath := string.Join(';', loadedItem.RipGrepArguments().GetSearchPath());
	Assert.AreEqual(searchPath, actualSearchPath, Format('Roundtrip SearchPath persistence failed. Expected: "%s", Got: "%s"',
		[searchPath, actualSearchPath]));

	// Count file mask options
	var
	originalMaskCount := 0;
	var
	loadedMaskCount := 0;

	if not fileMasks.IsEmpty then begin
		originalMaskCount := Length(fileMasks.Split([';']));
	end;

	for var arg in loadedItem.RipGrepArguments() do begin
		if arg.Contains('-g=') then begin
			Inc(loadedMaskCount);
		end;
	end;

	Assert.AreEqual(originalMaskCount, loadedMaskCount,
		Format('Roundtrip FileMasks persistence failed. Expected: %d masks, Got: %d masks for: "%s"', [originalMaskCount, loadedMaskCount,
		fileMasks]));

	// Verify the item is properly loaded and functional
	Assert.IsTrue(loadedItem.IsLoadedFromStream, 'Item should be marked as loaded from stream');
	Assert.AreEqual('roundtrip test', loadedItem.SearchText, 'Search text should be preserved in roundtrip');
end;

initialization

// Register the test class
TDUnitX.RegisterTestFixture(THistoryItemObjectTest);

end.
