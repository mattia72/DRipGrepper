unit RipGrepper.Data.HistoryItemObjectTest;

interface

uses
	DUnitX.TestFramework,
	RipGrepper.Data.HistoryItemObject,
	Spring,
	RipGrepper.Common.SearchTextWithOptions,
	RipGrepper.Common.GuiSearchParams,
	System.Classes;

type

	[TestFixture]
	THistoryItemObjectTest = class(TObject)
		const
			TEST_SEARCH_TEXT = 'TEST_SEARCH_TEXT';
			HIST_OBJ_COUNT = 10;

		private
		var
			FGuiParams : IShared<TSearchTextWithOptions>;
			FGuiSearchTextParams : IShared<TGuiSearchTextParams>;
			FHistoryObjectList : THistoryObjectArray;
			FRipGrepArguments : IShared<TStringList>;
			procedure WriteHistObjsToStream(const ms : IShared<TMemoryStream>);

		public
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
	RipGrepper.Common.Interfaces, RipGrepper.Common.Constants;

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

	FRipGrepArguments := Shared.Make<TStringList>();

	FRipGrepArguments.AddPair(RG_ARG_OPTIONS, '--vimgrep');
	FRipGrepArguments.AddPair(RG_ARG_OPTIONS, '-g=*.txt');
	FRipGrepArguments.AddPair(RG_ARG_OPTIONS, RG_PARAM_END);
	FRipGrepArguments.AddPair(RG_ARG_SEARCH_TEXT, 'search text');
	FRipGrepArguments.AddPair(RG_ARG_SEARCH_PATH, 'C:\Path\Search\Files');

end;

procedure THistoryItemObjectTest.TearDown();
begin

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
	Assert.AreEqual(hio.RipGrepArguments.ToStringArray,
		{ } other.RipGrepArguments.ToStringArray,
		{ } 'RipGrepArguments content should match the expected serialized data');
end;

procedure THistoryItemObjectTest.TestSaveLoadListFromStream();
var
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

	for var i := 0 to count do begin
		hio := THistoryItemObject.Create();
		hio.LoadFromStreamReader(sr);
		hioList.Add(hio);
	end;

	for var i := 0 to count do begin
		hio := hioList[i];
		Assert.AreEqual(TEST_SEARCH_TEXT + i.ToString,
			{ } hio.GuiSearchTextParams.SearchTextWithOptions.SearchTextOfUser,
			{ } 'SearchText content should match the expected serialized data');
		Assert.AreEqual(hio.RipGrepArguments.ToStringArray,
			{ } hio.RipGrepArguments.ToStringArray,
			{ } 'RipGrepArguments content should match the expected serialized data');
	end;

end;

procedure THistoryItemObjectTest.WriteHistObjsToStream(const ms : IShared<TMemoryStream>);
var
	hio : IHistoryItemObject;
	sw : IShared<TStreamWriter>;
begin
	sw := Shared.Make<TStreamWriter>(TStreamWriter.Create(ms));
	sw.WriteLine(HIST_OBJ_COUNT.ToString);
	for var i := 0 to HIST_OBJ_COUNT do begin
		hio := THistoryItemObject.Create();
		hio.GuiSearchTextParams := FGuiSearchTextParams;
        var searchText :=  TEST_SEARCH_TEXT + i.ToString;
		hio.GuiSearchTextParams.SetSearchText(searchTExt);
        hio.RipGrepArguments := FRipGrepArguments;
        hio.RipGrepArguments.AddPair(RG_ARG_SEARCH_TEXT, searchTExt);

		hio.SaveToStreamWriter(sw);
		FHistoryObjectList.Add(hio);
	end;
end;

end.
