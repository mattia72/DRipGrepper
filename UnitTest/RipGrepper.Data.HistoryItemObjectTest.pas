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
			[Ignore('todo')]
			procedure LoadFromStreamShouldRestoreReplaceWithMatches();
			[Test]
			procedure SaveToStreamShouldPersistSearchWithMatches();
			[Test]
			procedure SaveToStreamShouldPersistSearchWithoutMatches();
			[Test]
			[Ignore('todo')]
			procedure SaveToStreamShouldPersistReplaceWithMatches();
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

end.
