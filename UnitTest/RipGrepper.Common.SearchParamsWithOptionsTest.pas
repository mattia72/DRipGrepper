unit RipGrepper.Common.SearchParamsWithOptionsTest;

interface

uses
	DUnitX.TestFramework,
	RipGrepper.Common.SearchTextWithOptions,
	Spring;

type

	[TestFixture]
	TSearchParamsWithOptionsTest = class

		private

		public
			[Setup]
			procedure Setup;
			[TearDown]
			procedure TearDown;

			[Test]
			[Testcase('Single word ur           ', 'aaa      |0|1|' + 'aaa      ', '|')]
			[Testcase('Single word mc           ', 'aaa      |1|0|' + '\baaa      \b', '|')]
			[Testcase('Single word mc,ur        ', 'aaa      |1|1|' + '\baaa      \b', '|')]

			[Testcase('Double word ur           ', 'aaa bbb  |0|1|' + 'aaa bbb  ', '|')]
			[Testcase('Double word mc           ', 'aaa bbb  |1|0|' + '\baaa bbb  \b', '|')]
			[Testcase('Double word mc,ur        ', 'aaa bbb  |1|1|' + '\baaa bbb  \b', '|')]

			[Testcase('Start Bounded word ur    ', '\baaa|0|1|' + '\baaa', '|')]
			[Testcase('Start Bounded word mc    ', '\baaa|1|0|' + '\b\\baaa\b', '|')]
			[Testcase('Start Bounded word mc,ur ', '\baaa|1|1|' + '\baaa', '|')]

			[Testcase('End Bounded word ur      ', 'aaa\b|0|1|' + 'aaa\b', '|')]
			[Testcase('End Bounded word mc      ', 'aaa\b|1|0|' + '\baaa\\b\b', '|')]
			[Testcase('End Bounded word mc,ur   ', 'aaa\b|1|1|' + 'aaa\b', '|')]

			[Testcase('Invalid PCRE in word ur   ', 'aaa\u bbb|0|1|' + 'aaa\u bbb', '|')]
			[Testcase('Invalid PCRE in word mc   ', 'aaa\u bbb|1|0|' + '\baaa\\u bbb\b', '|')]
			[Testcase('Invalid PCRE in word mc,ur', 'aaa\u bbb|1|1|' + '\baaa\u bbb\b', '|')]
			procedure TestSearchText(const _sSearchText : string; const _bMatchWord, _bUseRegex : Integer; _expected : string);
			[Test]
			[Testcase('Single word ur           ', 'aaa      |0|1|' + 'aaa      ', '|')]
			[Testcase('Single word mc           ', 'aaa      |1|0|' + '\baaa      \b', '|')]
			[Testcase('Single word mc,ur        ', 'aaa      |1|1|' + '\baaa      \b', '|')]

			[Testcase('Double word ur           ', 'aaa bbb  |0|1|' + 'aaa bbb  ', '|')]
			[Testcase('Double word mc           ', 'aaa bbb  |1|0|' + '\baaa bbb  \b', '|')]
			[Testcase('Double word mc,ur        ', 'aaa bbb  |1|1|' + '\baaa bbb  \b', '|')]

			[Testcase('Start Bounded word ur    ', '\baaa|0|1|' + '\baaa', '|')]
			[Testcase('Start Bounded word mc    ', '\baaa|1|0|' + '\b\\baaa\b', '|')]
			[Testcase('Start Bounded word mc,ur ', '\baaa|1|1|' + '\baaa', '|')]

			[Testcase('End Bounded word ur      ', 'aaa\b|0|1|' + 'aaa\b', '|')]
			[Testcase('End Bounded word mc      ', 'aaa\b|1|0|' + '\baaa\\b\b', '|')]
			[Testcase('End Bounded word mc,ur   ', 'aaa\b|1|1|' + 'aaa\b', '|')]

			[Testcase('Invalid PCRE in word ur   ', 'aaa\u bbb|0|1|' + 'aaa\u bbb', '|')]
			[Testcase('Invalid PCRE in word mc   ', 'aaa\u bbb|1|0|' + '\baaa\\u bbb\b', '|')]
			[Testcase('Invalid PCRE in word mc,ur', 'aaa\u bbb|1|1|' + '\baaa\u bbb\b', '|')]
			procedure TestSetOption(const _sSearchText : string; const _bMatchWord, _bUseRegex : Integer; _expected : string);
			[Test]
			[Testcase('Single word ur           ', 'aaa      |0|1|' + 'aaa      ', '|')]
			[Testcase('Single word mc           ', 'aaa      |1|0|' + '\baaa      \b', '|')]
			[Testcase('Single word mc,ur        ', 'aaa      |1|1|' + '\baaa      \b', '|')]

			[Testcase('Double word ur           ', 'aaa bbb  |0|1|' + 'aaa bbb  ', '|')]
			[Testcase('Double word mc           ', 'aaa bbb  |1|0|' + '\baaa bbb  \b', '|')]
			[Testcase('Double word mc,ur        ', 'aaa bbb  |1|1|' + '\baaa bbb  \b', '|')]

			[Testcase('Start Bounded word ur    ', '\baaa|0|1|' + '\baaa', '|')]
			[Testcase('Start Bounded word mc    ', '\baaa|1|0|' + '\b\\baaa\b', '|')]
			[Testcase('Start Bounded word mc,ur ', '\baaa|1|1|' + '\baaa', '|')]

			[Testcase('End Bounded word ur      ', 'aaa\b|0|1|' + 'aaa\b', '|')]
			[Testcase('End Bounded word mc      ', 'aaa\b|1|0|' + '\baaa\\b\b', '|')]
			[Testcase('End Bounded word mc,ur   ', 'aaa\b|1|1|' + 'aaa\b', '|')]

			[Testcase('Invalid PCRE in word ur   ', 'aaa\u bbb|0|1|' + 'aaa\u bbb', '|')]
			[Testcase('Invalid PCRE in word mc   ', 'aaa\u bbb|1|0|' + '\baaa\\u bbb\b', '|')]
			[Testcase('Invalid PCRE in word mc,ur', 'aaa\u bbb|1|1|' + '\baaa\u bbb\b', '|')]
			procedure TestResetOption(const _sSearchText : string; const _bMatchWord, _bUseRegex : Integer; _expected : string);

			[Test]
			procedure TestSaveToStream();
			[Test]
			procedure TestLoadFromStream();

			[Test]
			[Testcase('Single word ur           ', 'aaa      |0|1|' + 'aaa      ', '|')]
			[Testcase('Single word mc           ', 'aaa      |1|0|' + '\baaa      \b', '|')]
			[Testcase('Single word mc,ur        ', 'aaa      |1|1|' + '\baaa      \b', '|')]

			[Testcase('Double word ur           ', 'aaa bbb  |0|1|' + 'aaa bbb  ', '|')]
			[Testcase('Double word mc           ', 'aaa bbb  |1|0|' + '\baaa bbb  \b', '|')]
			[Testcase('Double word mc,ur        ', 'aaa bbb  |1|1|' + '\baaa bbb  \b', '|')]

			[Testcase('Start Bounded word ur    ', '\baaa|0|1|' + '\baaa', '|')]
			[Testcase('Start Bounded word mc    ', '\baaa|1|0|' + '\b\\baaa\b', '|')]
			[Testcase('Start Bounded word mc,ur ', '\baaa|1|1|' + '\baaa', '|')]

			[Testcase('End Bounded word ur      ', 'aaa\b|0|1|' + 'aaa\b', '|')]
			[Testcase('End Bounded word mc      ', 'aaa\b|1|0|' + '\baaa\\b\b', '|')]
			[Testcase('End Bounded word mc      ', 'bbb.aaa\b|1|0|' + '\bbbb\.aaa\\b\b', '|')]
			[Testcase('End Bounded word mc,ur   ', 'aaa\b|1|1|' + 'aaa\b', '|')]

			[Testcase('Invalid PCRE in word ur   ', 'aaa\u bbb|0|1|' + 'aaa\u bbb', '|')]
			[Testcase('Invalid PCRE in word mc   ', 'aaa\u bbb|1|0|' + '\baaa\\u bbb\b', '|')]
			[Testcase('Invalid PCRE in word mc,ur', 'aaa\u bbb|1|1|' + '\baaa\u bbb\b', '|')]
			procedure TestSwitchOptionDef(const _sSearchText : string; const _bMatchWord, _bUseRegex : Integer; _expected : string);
			[Test]
			[Testcase('Single word ur           ', 'aaa      |0|1|' + 'aaa      ', '|')]
			[Testcase('Single word mc           ', 'aaa      |1|0|' + '\baaa      \b', '|')]
			[Testcase('Single word mc,ur        ', 'aaa      |1|1|' + '\baaa      \b', '|')]

			[Testcase('Double word ur           ', 'aaa bbb  |0|1|' + 'aaa bbb  ', '|')]
			[Testcase('Double word mc           ', 'aaa bbb  |1|0|' + '\baaa bbb  \b', '|')]
			[Testcase('Double word mc,ur        ', 'aaa bbb  |1|1|' + '\baaa bbb  \b', '|')]

			[Testcase('Start Bounded word ur    ', '\baaa|0|1|' + '\baaa', '|')]
			[Testcase('Start Bounded word mc    ', '\baaa|1|0|' + '\b\\baaa\b', '|')]
			[Testcase('Start Bounded word mc,ur ', '\baaa|1|1|' + '\baaa', '|')]

			[Testcase('End Bounded word ur      ', 'aaa\b|0|1|' + 'aaa\b', '|')]
			[Testcase('End Bounded word mc      ', 'aaa\b|1|0|' + '\baaa\\b\b', '|')]
			[Testcase('End Bounded word mc      ', 'bbb.aaa\b|1|0|' + '\bbbb\.aaa\\b\b', '|')]
			[Testcase('End Bounded word mc,ur   ', 'aaa\b|1|1|' + 'aaa\b', '|')]

			[Testcase('Invalid PCRE in word ur   ', 'aaa\u bbb|0|1|' + 'aaa\u bbb', '|')]
			[Testcase('Invalid PCRE in word mc   ', 'aaa\u bbb|1|0|' + '\baaa\\u bbb\b', '|')]
			[Testcase('Invalid PCRE in word mc,ur', 'aaa\u bbb|1|1|' + '\baaa\u bbb\b', '|')]
			procedure TestSwitchOptionOrig(const _sSearchText : string; const _bMatchWord, _bUseRegex : Integer; _expected : string);
	end;

implementation

uses
	RipGrepper.Common.SimpleTypes,
	System.Classes,
	System.SysUtils;

const
	TEST_SEARCH_TEXT = 'test search text';

procedure TSearchParamsWithOptionsTest.Setup;
begin

end;

procedure TSearchParamsWithOptionsTest.TearDown;
begin

end;

procedure TSearchParamsWithOptionsTest.TestSearchText(const _sSearchText : string; const _bMatchWord, _bUseRegex : Integer;
	_expected : string);
var
	FGuiParams : IShared<TSearchTextWithOptions>;
begin
	FGuiParams := Shared.Make<TSearchTextWithOptions>(TSearchTextWithOptions.Create);
	FGuiParams.SearchTextOfUser := _sSearchText;
	var
	os := TSearchTextWithOptions.GetAsSearchOptionSet(False, _bMatchWord = 1, _bUseRegex = 1);
	FGuiParams.SearchOptions := os;

	Assert.AreEqual(_expected, FGuiParams.SearchTextAsRgParam, 'search text should equal' + _expected);
end;

procedure TSearchParamsWithOptionsTest.TestSetOption(const _sSearchText : string; const _bMatchWord, _bUseRegex : Integer;
	_expected : string);
var
	FGuiParams : IShared<TSearchTextWithOptions>;
begin
	FGuiParams := Shared.Make<TSearchTextWithOptions>(TSearchTextWithOptions.Create);
	FGuiParams.SearchTextOfUser := _sSearchText;

	if (_bUseRegex = 1) then begin
		FGuiParams.SetOption(EGuiOption.soUseRegex);
	end;
	if (_bMatchWord = 1) then begin
		FGuiParams.SetOption(EGuiOption.soMatchWord);
	end;

	Assert.AreEqual(_expected, FGuiParams.SearchTextAsRgParam, 'search text should equal' + _expected);
end;

procedure TSearchParamsWithOptionsTest.TestResetOption(const _sSearchText : string; const _bMatchWord, _bUseRegex : Integer;
	_expected : string);
var
	FGuiParams : IShared<TSearchTextWithOptions>;
begin
	FGuiParams := Shared.Make<TSearchTextWithOptions>(TSearchTextWithOptions.Create);
	FGuiParams.SearchTextOfUser := _sSearchText;
	var
	os := TSearchTextWithOptions.GetAsSearchOptionSet(True, True, True);
	FGuiParams.SearchOptions := os;

	if (_bUseRegex = 0) then begin
		FGuiParams.ReSetOption(EGuiOption.soUseRegex);
	end;
	if (_bMatchWord = 0) then begin
		FGuiParams.ReSetOption(EGuiOption.soMatchWord);
	end;

	Assert.AreEqual(_expected, FGuiParams.SearchTextAsRgParam, 'search text should equal' + _expected);
end;

procedure TSearchParamsWithOptionsTest.TestSaveToStream();
var
	actualOptions : TSearchOptionSet;
	FGuiParams : IShared<TSearchTextWithOptions>;
	ms : IShared<TMemoryStream>;
	sr : IShared<TStreamReader>;
	actualSearchText : string;
begin
	FGuiParams := Shared.Make<TSearchTextWithOptions>(TSearchTextWithOptions.Create);
	FGuiParams.SearchTextOfUser := TEST_SEARCH_TEXT;
	FGuiParams.SetOption(EGuiOption.soUseRegex);
	FGuiParams.SetOption(EGuiOption.soMatchWord);

	ms := Shared.Make<TMemoryStream>();
	FGuiParams.SaveToStream(ms);
	ms.Position := 0;

	sr := Shared.Make<TStreamReader>(TStreamReader.Create(ms, TEncoding.UTF8));
	actualSearchText := sr.ReadLine;
	actualOptions := TSearchTextWithOptions.StringToSearchOptionSet(sr.ReadLine);

	var other : IShared<TSearchTextWithOptions>;
	other := Shared.Make<TSearchTextWithOptions>(TSearchTextWithOptions.Create(actualSearchText, actualOptions));

	Assert.AreEqual(TEST_SEARCH_TEXT, actualSearchText, 'Stream content should match the expected serialized data');
	Assert.AreEqual(FGuiParams.GetAsString(), other.GetAsString(), 'Stream content should match the expected serialized data');
end;

procedure TSearchParamsWithOptionsTest.TestLoadFromStream();
var
	actualOptions : TSearchOptionSet;
	FGuiParams : IShared<TSearchTextWithOptions>;
	ms : IShared<TMemoryStream>;
	sr : IShared<TStreamReader>;
	actualSearchText : string;
begin
	FGuiParams := Shared.Make<TSearchTextWithOptions>(TSearchTextWithOptions.Create);
	FGuiParams.SearchTextOfUser := TEST_SEARCH_TEXT;
	FGuiParams.SetOption(EGuiOption.soMatchCase);
	FGuiParams.SetOption(EGuiOption.soUseRegex);
	FGuiParams.SetOption(EGuiOption.soMatchWord);

	ms := Shared.Make<TMemoryStream>();
	FGuiParams.SaveToStream(ms);
	ms.Position := 0;

	sr := Shared.Make<TStreamReader>(TStreamReader.Create(ms, TEncoding.UTF8));
	actualSearchText := sr.ReadLine;
	actualOptions := TSearchTextWithOptions.StringToSearchOptionSet(sr.ReadLine);

	var other : IShared<TSearchTextWithOptions>;
	other := Shared.Make<TSearchTextWithOptions>(TSearchTextWithOptions.Create());
	ms.Position := 0;
	other.LoadFromStream(ms);

	Assert.AreEqual(TEST_SEARCH_TEXT, other.SearchTextOfUser, 'SearchText content should match the expected serialized data');
	Assert.AreEqual(FGuiParams.GetAsString(), other.GetAsString(), 'Stream content should match the expected serialized data');
end;

procedure TSearchParamsWithOptionsTest.TestSwitchOptionDef(const _sSearchText : string; const _bMatchWord, _bUseRegex : Integer;
	_expected : string);
var
	FGuiParams : IShared<TSearchTextWithOptions>;
begin
	FGuiParams := Shared.Make<TSearchTextWithOptions>(TSearchTextWithOptions.Create);
	FGuiParams.SearchTextOfUser := _sSearchText;

	if (_bUseRegex = 1) then begin
		FGuiParams.SwitchOption(EGuiOption.soUseRegex);
	end;
	if (_bMatchWord = 1) then begin
		FGuiParams.SetOption(EGuiOption.soMatchWord);
	end;

	Assert.AreEqual(_expected, FGuiParams.SearchTextAsRgParam, 'search text should equal' + _expected);
end;

procedure TSearchParamsWithOptionsTest.TestSwitchOptionOrig(const _sSearchText : string; const _bMatchWord, _bUseRegex : Integer;
	_expected : string);
var
	FGuiParams : IShared<TSearchTextWithOptions>;
begin
	FGuiParams := Shared.Make<TSearchTextWithOptions>(TSearchTextWithOptions.Create);
	FGuiParams.SearchTextOfUser := _sSearchText;
	FGuiParams.SearchOptions := FGuiParams.GetAsSearchOptionSet((_bMatchWord = 1), (_bMatchWord = 1), False);

	if (_bUseRegex = 1) then begin
		FGuiParams.SwitchOption(EGuiOption.soUseRegex);
	end;
	if (_bMatchWord = 1) then begin
		FGuiParams.SetOption(EGuiOption.soMatchWord);
	end;

	Assert.AreEqual(_expected, FGuiParams.SearchTextAsRgParam, 'search text should equal' + _expected);
end;

initialization

TDUnitX.RegisterTestFixture(TSearchParamsWithOptionsTest);

end.
