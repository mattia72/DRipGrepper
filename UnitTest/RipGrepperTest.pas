unit RipGrepperTest;

interface

uses
	DUnitX.TestFramework;

type

	[TestFixture]
	TRipGrepperTest = class
		public
			[Setup]
			procedure Setup;
			[TearDown]
			procedure TearDown;
			// Sample Methods
			// Simple single Test
			[Test]
			procedure Test1;
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
	System.SysUtils;

procedure TRipGrepperTest.Setup;
begin
end;

procedure TRipGrepperTest.TearDown;
begin
end;

procedure TRipGrepperTest.Test1;
begin

end;

procedure TRipGrepperTest.ParseLineTest(const _s : string);
var
	m : TRipGrepMatch;
begin
	m.ParseLine(_s);
	Assert.IsTrue(m.Validate,
		{ } 'Line:' + CRLF +
		{ } _s + CRLF +
		{ } m.FileName + CRLF +
		{ } m.Row.ToString + CRLF +
		{ } m.Col.ToString + CRLF +
		{ } m.Text + CRLF);
end;

procedure TRipGrepperTest.ValidatePathTest(const _s : string);
var
	m : TRipGrepMatch;
begin
	m.ParseLine(_s);
	Assert.IsTrue(m.ValidatePath(),
		{ } 'Line:' + CRLF +
		{ } _s + CRLF +
		{ } m.FileName + CRLF

		);
end;

initialization

TDUnitX.RegisterTestFixture(TRipGrepperTest);

end.
