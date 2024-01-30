unit RipGrepMatchTest;

interface

uses
	DUnitX.TestFramework;

type

	[TestFixture]
	TRipGrepMatchTest = class
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
			[TestCase('Test Full Path', 'c:\test\path\file.ext:1:1:test match')]
			[TestCase('Test Relative Path', '.\test\path\file.ext:1:1:test match')]
			[TestCase('Test File', 'file.ext:1:1:test match')]
			procedure ValidatePathTest(const _s : string);
	end;

implementation

uses
	RipGrepper.Data.Matches,
	RipGrepper.Common.Types,
	System.Classes,
	System.SysUtils,
	RipGrepper.Parsers.VimGrepMatchLine;

procedure TRipGrepMatchTest.Setup;
begin
end;

procedure TRipGrepMatchTest.TearDown;
begin
end;

procedure TRipGrepMatchTest.ParseLineTest(const _s : string);
var
	m : TVimGrepMatchLineParser;
begin
	m := TVimGrepMatchLineParser.Create();
	m.ParseLine(0, _s);
	Assert.IsTrue(not m.ParseResult.IsError,
		{ } 'Line:' + CRLF +
		{ } _s + CRLF +
		{ } m.ParseResult.Columns[Integer(ciFile)].Text + CRLF +
		{ } m.ParseResult.Columns[Integer(ciRow)].Text + CRLF +
		{ } m.ParseResult.Columns[Integer(ciCol)].Text + CRLF +
		{ } m.ParseResult.Columns[Integer(ciText)].Text + CRLF);
end;

procedure TRipGrepMatchTest.ValidatePathTest(const _s : string);
var
	m : TVimGrepMatchLineParser;
begin
	m := TVimGrepMatchLineParser.Create();
	m.ParseLine(0, _s);
	Assert.IsTrue(not m.ParseResult.IsError,
		{ } 'Line:' + CRLF +
		{ } _s + CRLF +
		{ } m.ParseResult.Columns[Integer(ciFile)].Text);
end;

initialization

TDUnitX.RegisterTestFixture(TRipGrepMatchTest);

end.
