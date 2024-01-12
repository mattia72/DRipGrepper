unit StringPadLeftTest;

interface

uses
	DUnitX.TestFramework;

type

	[TestFixture]
	TMyTestObj = class
		public
			// Sample Methods
			// Simple single Test
			// [Test]
			function OldSrLeftPad(const _s : string; const _length : Integer) : string;
			// Test with TestCase Attribute to supply parameters.
			[Test]
			[TestCase('TestA', 'a,2', ',')]
			[TestCase('TestB', 'aa,4', ',')]
			[TestCase('TestB', 'aaa,5', ',')]
			procedure StrLeftPad(const _s, _length : string);
	end;

implementation

uses
	System.SysUtils;

function TMyTestObj.OldSrLeftPad(const _s : string; const _length : Integer) : string;
begin
	Result := Copy(_s, _length);
	while length(Result) < _length do
		Result := ' ' + Result;
end;

procedure TMyTestObj.StrLeftPad(const _s, _length : string);
begin
	var
	s := OldSrLeftPad(_s, StrToInt(_length));
	var
	s1 := _s.PadLeft(StrToInt(_length));
	Assert.AreNOTEqual(s, s1); //Padleft is better :)
end;

initialization

TDUnitX.RegisterTestFixture(TMyTestObj);

end.
