unit RipGrepper.Common.SearchParamsWithOptionsTest;

interface

uses
	DUnitX.TestFramework,
	RipGrepper.Common.SearchTextWithOptions;

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
	end;

implementation

procedure TSearchParamsWithOptionsTest.Setup;
begin

end;

procedure TSearchParamsWithOptionsTest.TearDown;
begin

end;

procedure TSearchParamsWithOptionsTest.TestSearchText(const _sSearchText : string; const _bMatchWord, _bUseRegex : Integer;
	_expected : string);
var
	FGuiParams : TSearchTextWithOptions;
begin
	FGuiParams.SearchText := _sSearchText;
	var
	os := TSearchTextWithOptions.GetAsSearchOptionSet(False, _bMatchWord = 1, _bUseRegex = 1);
	FGuiParams.SearchOptions := os;

	Assert.AreEqual(_expected, FGuiParams.SearchText, 'search text should equal' + _expected);
end;

initialization

TDUnitX.RegisterTestFixture(TSearchParamsWithOptionsTest);

end.
