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
		FTerminateEventProducer : TMock<ITerminateEventProducer>;

		private
			function BuildLines(const _line, _crlf : string; out _s : string) : Integer;
			function CheckLine(const _sIn, _line : string) : Boolean;
			function CheckStartOfLine(const _sIn, _line : string) : Boolean;
			procedure SetupMocks(const _line : string; const _callCount, _checker : Integer);

		public
			[Setup]
			procedure Setup;
			[TearDown]
			procedure TearDown;
			[Test]
			[TestCase('Test1', '12345' + CR + '67890')]
			[TestCase('Test2', '12345' + LF + '67890')]
			[TestCase('Test3', '1' + LF + '234')]
			[TestCase('Test4', '12' + LF + '34')]
			[TestCase('Test5', '123' + LF + '4')]
			[TestCase('Test6', '1234' + LF + '')]
			[TestCase('Test7', '1' + CR + '234')]
			[TestCase('Test8', '12' + CR + '34')]
			[TestCase('Test9', '123' + CR + '4')]
			[TestCase('Test0', '1234' + CR + '')]
			[TestCase('Test3', '1' + LF + LF + '234')]
			[TestCase('Test4', '12' + LF + LF + '34')]
			[TestCase('Test5', '123' + LF + LF + '4')]
			[TestCase('Test6', '1234' + LF + LF + '')]
			[TestCase('Test7', '1' + CR + CR + '234')]
			[TestCase('Test8', '12' + CR + CR + '34')]
			[TestCase('Test9', '123' + CR + CR + '4')]
			[TestCase('Test0', '1234' + CR + CR + '')]
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
			procedure ProcessOutputTestCRLF(const _line : string);
			// Test with TestCase Attribute to supply parameters.
			[Test]
			[TestCase('Test1', '12345' + CR + '67890')]
			[TestCase('Test2', '12345' + LF + '67890')]
			[TestCase('Test3', '1' + LF + '234')]
			[TestCase('Test4', '12' + LF + '34')]
			[TestCase('Test5', '123' + LF + '4')]
			[TestCase('Test6', '1234' + LF + '')]
			[TestCase('Test7', '1' + CR + '234')]
			[TestCase('Test8', '12' + CR + '34')]
			[TestCase('Test9', '123' + CR + '4')]
			[TestCase('Test0', '1234' + CR + '')]
			[TestCase('Test3', '1' + LF + LF + '234')]
			[TestCase('Test4', '12' + LF + LF + '34')]
			[TestCase('Test5', '123' + LF + LF + '4')]
			[TestCase('Test6', '1234' + LF + LF + '')]
			[TestCase('Test7', '1' + CR + CR + '234')]
			[TestCase('Test8', '12' + CR + CR + '34')]
			[TestCase('Test9', '123' + CR + CR + '4')]
			[TestCase('Test0', '1234' + CR + CR + '')]
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
			procedure ProcessOutputTestLF(const _line : string);
			// Test with TestCase Attribute to supply parameters.
			[Test]

			procedure ProcessRunTest;
	end;

implementation

uses

	System.Classes,
	DUnitX.Utils,
	System.Rtti,
	System.SysUtils,
	System.AnsiStrings,
	System.StrUtils;

function TRipGrepperToolsProcessTest.BuildLines(const _line, _crlf : string; out _s : string) : Integer;
var
	callCount : Integer;
begin
	callCount := 0;
	for var i := 0 to 2 do begin
		for var j := 0 to TProcessUtils.BUFF_LENGTH do begin
			_s := _s + _line + CRLF;
			Inc(callCount);
		end;
	end;
	Result := callCount;
end;

procedure TRipGrepperToolsProcessTest.Setup;
begin
	FEventHandlerMock := TMock<INewLineEventHandler>.Create();
	FTerminateEventProducer := TMock<ITerminateEventProducer>.Create();
end;

procedure TRipGrepperToolsProcessTest.TearDown;
begin
end;

procedure TRipGrepperToolsProcessTest.ProcessOutputTestCRLF(const _line : string);
var
	st : TStream;
	s : string;
	callCount : Integer;
begin
	callCount := BuildLines(_line, CRLF, s);

	SetupMocks(_line, callCount, 0);

	st := TStringStream.Create(s);
	try
		TProcessUtils.ProcessOutput(st, FEventHandlerMock, FTerminateEventProducer);
	finally
		st.Free;
	end;
	FEventHandlerMock.VerifyAll();
end;

procedure TRipGrepperToolsProcessTest.ProcessRunTest;
var
	st : TStream;
	s : string;
	callCount : Integer;
begin
	callCount := BuildLines('', LF, s);

	SetupMocks('', callCount, 1);

	st := TStringStream.Create(s);
	try
		TProcessUtils.ProcessOutput(st, FEventHandlerMock, FTerminateEventProducer);
	finally
		st.Free;
	end;
	FEventHandlerMock.VerifyAll();
end;

function TRipGrepperToolsProcessTest.CheckLine(const _sIn, _line : string) : Boolean;
begin
	var
	bOk := (_sIn = _line);
	if not bOk then begin
		if MatchStr(_sIn, _line.Split([CR, LF])) then begin
			bOk := True;
		end else begin
			bOk := False; // Break Point
		end;
	end;
	Result := bOk;
end;

function TRipGrepperToolsProcessTest.CheckStartOfLine(const _sIn, _line : string) : Boolean;
begin
	Result := True;

	if not _sIn.IsEmpty and _sIn.StartsWith('C:\') then begin
		Result := False
	end;
end;

procedure TRipGrepperToolsProcessTest.ProcessOutputTestLF(const _line : string);
var
	st : TStream;
	s : string;
	callCount : Integer;
begin
	callCount := 0;
	for var i := 0 to 3 do begin
		for var j := 0 to TProcessUtils.BUFF_LENGTH do begin
			s := s + _line + LF;
			Inc(callCount);
		end;
	end;

	SetupMocks(_line, callCount, 0);

	st := TStringStream.Create(s);
	try
		TProcessUtils.ProcessOutput(st, FEventHandlerMock, FTerminateEventProducer);
	finally
		st.Free;
	end;
	FEventHandlerMock.VerifyAll();
end;

procedure TRipGrepperToolsProcessTest.SetupMocks(const _line : string; const _callCount, _checker : Integer);
var
	callCount : integer;
begin
	// FTerminateEventProducer.Setup.ClearExpectations;
	FTerminateEventProducer.Setup.WillReturn(TValue.From<Boolean>(False)).When.ProcessShouldTerminate;
	FTerminateEventProducer.Setup.Expect.Never.When.ProcessShouldTerminate;

	// FEventHandlerMock.Setup.ClearExpectations;
	FEventHandlerMock.Setup.Expect.AtLeast(_callCount); // empty lines could be given if only crlf is in a line
	callCount := 0;
	FEventHandlerMock.Setup.WillExecute(
		function(const args : TArray<TValue>; const ReturnType : TRttiType) : TValue
		var
			sIn : string;
			bOk : Boolean;
			// sl : TStrings;
			sExp : string;
		begin
			bOk := False;
			// oIn := args[0].AsType; // handler
			sIn := args[1].AsType<string>;
			var
			arr := _line.Split([CR, LF]);

			if callCount >= Length(arr) then
				callCount := 0;

			if Length(arr) = 0 then begin
				sExp := '';
			end else begin
				sExp := arr[callCount].Trim([CR, LF]);
			end;
			case _checker of
				0 :
				bOk := CheckLine(sIn, sExp) or _line.StartsWith(sIn) or _line.EndsWith(sIn); // Todo : a better check
				1 :
				bOk := CheckStartOfLine(sIn, sExp);
			end;
			Inc(callCount);
			if not bOk then begin
				Assert.IsTrue(bOk, Format(CRLF + 'Act:|%s|' + CRLF + 'Exp:|%s| ' + CRLF + 'Org:|%s|', [sIn, sExp, _line]));
			end;
		end).When.OnNewOutputLine(It(0).IsAny<string>);
end;

initialization

TDUnitX.RegisterTestFixture(TRipGrepperToolsProcessTest);

end.
