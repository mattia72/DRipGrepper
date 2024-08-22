unit RipGrepper.Parser.MatchTest;

interface

uses
	DUnitX.TestFramework,
	RipGrepper.Common.Interfaces,
	Delphi.Mocks;

type

	[TestFixture]
	TRipGrepMatchTest = class
		private
			FifSearchParam : TMock<ISearchParams>;

		public
			[Setup]
			procedure Setup;
			[TearDown]
			procedure TearDown;
			// Test with TestCase Attribute to supply parameters.
			[Test]
			[TestCase('Test Full Path', 'c:\test\path\file.ext:1:1:test match')]
			[TestCase('Test Relative Path', '.\test\path\file.ext:1:1:test match')]
			[TestCase('Test File', 'file.ext:1:1:test match')]
			procedure ParseLineTest(const _s : string);
			// Test with TestCase Attribute to supply parameters.
			[Test]
			[TestCase('Test Error', 'rg: unrecognized flag --status')]
			[TestCase('Test Error2', 'similar flags that are available: --stats')]
			procedure ParseErrorTest(const _s : string);
			// Test with TestCase Attribute to supply parameters.
			[Test]
			[TestCase('Test Full Path',
				#$1B'[0m'#$1B'[36mc:\test\path\file.ext'#$1B'[0m:'#$1B'[0m'#$1B'[32m24'#$1B'[0m:'#$1B'[0m19'#$1B'[0m: text_before_match'#$1B'[0m'#$1B'[1m'#$1B'[31mmatch'#$1B'[0mtext_after_match')
				]
			[TestCase('Test Relative Path, with match begin',

				#$1B'[0m'#$1B'[36m.\test\path\file.ext'#$1B'[0m:'#$1B'[0m'#$1B'[32m24'#$1B'[0m:'#$1B'[0m19'#$1B'[0m:'#$1B'[0m'#$1B'[1m'#$1B'[31mmatch'#$1B'[0mtext_after_match')
				]
			[TestCase('Test File, with match end',
				#$1B'[0m'#$1B'[36mfile.ext'#$1B'[0m:'#$1B'[0m'#$1B'[32m24'#$1B'[0m:'#$1B'[0m19'#$1B'[0m:'#$1B'[0m'#$1B'[1m'#$1B'[31mmatch'#$1B'[0m')
				]
			procedure ParsePrettyLineTest(const _s : string);
			// Test with TestCase Attribute to supply parameters.
			[Test]
			[TestCase('Test Full Path', 'c:\test\path\file.ext:1:1:test match')]
			[TestCase('Test Relative Path', '.\test\path\file.ext:1:1:test match')]
			[TestCase('Test File', 'file.ext:1:1:test match')]
			procedure ValidatePathTest(const _s : string);
	end;

implementation

uses
	RipGrepper.Data.Matches,
	RipGrepper.Common.Constants,
	System.Classes,
	System.SysUtils,
	RipGrepper.Parsers.VimGrepMatchLine,

	RipGrepper.Common.ParsedObject,

	DUnitX.Utils,
	RipGrepper.Common.GuiSearchParams;

procedure TRipGrepMatchTest.Setup;
var
	guiParams : TGuiSearchTextParams;
begin
	guiParams := TGuiSearchTextParams.Create('search_text');

	FifSearchParam := TMock<ISearchParams>.Create();
	FifSearchParam.Setup.WillReturn(TValue.From(guiParams)).When.GetGuiSearchParams;
end;

procedure TRipGrepMatchTest.TearDown;
begin
end;

procedure TRipGrepMatchTest.ParseLineTest(const _s : string);
var
	parser : TVimGrepMatchLineParser;
begin
	parser := TVimGrepMatchLineParser.Create();

	parser.SearchParams := FifSearchParam; // TODO Mock!
	parser.ParseLine(0, _s);
	var
	pr := parser.ParseResult;
	Assert.IsTrue(not pr.IsError,
		{ } 'Line:' + CRLF +
		{ } _s + CRLF +
		{ } pr.Columns[Integer(ciFile)].Text + CRLF +
		{ } pr.Columns[Integer(ciRow)].Text + CRLF +
		{ } pr.Columns[Integer(ciCol)].Text + CRLF +
		{ } pr.Columns[Integer(ciText)].Text + CRLF +
		{ } pr.Columns[Integer(ciMatchText)].Text + CRLF +
		{ } pr.Columns[Integer(ciTextAfterMatch)].Text);
	parser.Free;
end;

procedure TRipGrepMatchTest.ParseErrorTest(const _s : string);
var
	m : ISearchResultLineParser;
	parseRes : IParsedObjectRow;
begin
	m := TVimGrepMatchLineParser.Create();
	m.ParseLine(0, _s);
	parseRes := (m as TVimGrepMatchLineParser).ParseResult;
	Assert.IsTrue(parseRes.IsError and
		{ } parseRes.Columns[Integer(ciRow)].Text.IsEmpty and
		{ } parseRes.Columns[Integer(ciCol)].Text.IsEmpty and
		{ } parseRes.Columns[Integer(ciText)].Text.IsEmpty,
		{ } 'Line:' + CRLF +
		{ } _s + CRLF +
		{ } parseRes.Columns[Integer(ciFile)].Text + CRLF +
		{ } parseRes.Columns[Integer(ciRow)].Text + CRLF +
		{ } parseRes.Columns[Integer(ciCol)].Text + CRLF +
		{ } parseRes.Columns[Integer(ciText)].Text + CRLF);

end;

procedure TRipGrepMatchTest.ParsePrettyLineTest(const _s : string);
var
	m : TVimGrepMatchLineParser;
	parseRes : IParsedObjectRow;
begin
	m := TVimGrepPrettyMatchLineParser.Create();
	m.SearchParams := FifSearchParam; // TODO Mock!

	m.ParseLine(0, _s);
	parseRes := (m as TVimGrepMatchLineParser).ParseResult;

	Assert.IsTrue(not parseRes.IsError, 'Line parsed with error: ' + _s);

	Assert.IsTrue(parseRes.Columns.Count = 6,
		{ } 'Line:' + CRLF +
		{ } _s + CRLF +
		{ } parseRes.Columns[Integer(ciFile)].Text + CRLF +
		{ } parseRes.Columns[Integer(ciRow)].Text + CRLF +
		{ } parseRes.Columns[Integer(ciCol)].Text + CRLF +
		{ } parseRes.Columns[Integer(ciText)].Text + CRLF +
		{ } parseRes.Columns[Integer(ciMatchText)].Text + CRLF +
		{ } parseRes.Columns[Integer(ciTextAfterMatch)].Text + CRLF

		);
	m.Free;
end;

procedure TRipGrepMatchTest.ValidatePathTest(const _s : string);
var
	m : ISearchResultLineParser;
begin
	m := TVimGrepMatchLineParser.Create();
	m.SearchParams := FifSearchParam; // TODO Mock!
	m.ParseLine(0, _s);
	Assert.IsTrue(not(m as TVimGrepMatchLineParser).ParseResult.IsError,
		{ } 'Line:' + CRLF +
		{ } _s + CRLF +
		{ } (m as TVimGrepMatchLineParser).ParseResult.Columns[Integer(ciFile)].Text);
end;

initialization

TDUnitX.RegisterTestFixture(TRipGrepMatchTest);

end.
