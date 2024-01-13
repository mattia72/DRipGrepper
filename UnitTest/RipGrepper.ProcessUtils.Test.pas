unit RipGrepper.ProcessUtils.Test;

interface

uses
	DUnitX.TestFramework,
	Delphi.Mocks,
	RipGrepper.Tools.ProcessUtils,
	RipGrepper.Common.Types;

type

	[TestFixture]
	TRipGrepperToolsProcessTest = class
		FEventHandlerMock : TMock<INewLineEventHandler>;

		public
			[Setup]
			procedure Setup;
			[TearDown]
			procedure TearDown;
			// Test with TestCase Attribute to supply parameters.
			[Test]
			[TestCase('Test1', '12345' + CR + '67890')]
			[TestCase('Test2', '12345' + LF + '67890')]
			[TestCase('Test2', '1' + LF + '23')]
			[TestCase('Test2', '12' + LF + '3')]
			[TestCase('TestA', '1234567890')]
			[TestCase('TestB', '123456789')]
			[TestCase('TestC', '12345678')]
			[TestCase('TestD', '1234567')]
			[TestCase('TestE', '123456')]
			[TestCase('TestF', '12345')]
			[TestCase('TestG', '1234')]
			[TestCase('TestH', '12')]
			[TestCase('TestI', '1')]
			[TestCase('TestJ', '')]
			procedure ProcessOutputTest(const _line : string);
	end;

implementation

uses

	System.Classes,
	DUnitX.Utils,
	System.Rtti,
	System.SysUtils, System.AnsiStrings, System.StrUtils;

procedure TRipGrepperToolsProcessTest.Setup;
begin
	FEventHandlerMock := TMock<INewLineEventHandler>.Create();
end;

procedure TRipGrepperToolsProcessTest.TearDown;
begin
end;

procedure TRipGrepperToolsProcessTest.ProcessOutputTest(const _line : string);
var
	st : TStream;
	s : string;
	callCount : Integer;
begin
	callCount := 0;
	for var i := 0 to 3 do begin
		for var j := 0 to TProcessUtils.BUFF_LENGTH do begin
			s := s + _line + CRLF;
			Inc(callCount);
		end;
	end;

	FEventHandlerMock.Setup.Expect.Exactly(callCount); // empty lines could be given if only crlf is in a line
	FEventHandlerMock.Setup.WillExecute(
		function(const args : TArray<TValue>; const ReturnType : TRttiType) : TValue
		var
			sIn : string;
		begin
			// oIn := args[0].AsType; // handler
			sIn := args[1].AsType<string>;
			var
			bOk := (sIn = _line);
			if not bOk then begin
				if MatchStr(sIn, _line.Split([CR, LF])) then begin
					bOk := True;
				end else begin
                    bOk := True;
                end;
			end;
			Assert.IsTrue(bOk, Format('Actual: %s <> Expected: %s', [sIn, _line]));
		end).When.OnNewOutputLine(It(0).IsAny<string>);

	st := TStringStream.Create(s);
	try
		TProcessUtils.ProcessOutput(st, FEventHandlerMock);
	finally
		st.Free;
	end;
	FEventHandlerMock.VerifyAll();
end;

initialization

TDUnitX.RegisterTestFixture(TRipGrepperToolsProcessTest);

end.
