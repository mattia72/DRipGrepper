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
	end;

implementation

uses

	RipGrepper.Common.SimpleTypes,
	System.SysUtils,

	RipGrepper.Common.Constants,
	RipGrepper.Settings.SettingsDictionary,
	ArrayEx;

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
	arr := GetSettingsDictAsArray(hio);

	for var i := 0 to count - 1 do begin
		hio := hioList[i];
		Assert.AreEqual(TEST_SEARCH_TEXT + i.ToString,
		{ } hio.GuiSearchTextParams.SearchTextWithOptions.SearchTextOfUser,
		{ } 'SearchText content should match the expected serialized data');
		Assert.AreEqual(hio.RipGrepArguments.ToStringArray,
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

end.
