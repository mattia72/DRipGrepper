unit RipGrepper.Tools.ProcessUtils;

interface

uses
	System.Classes,
	dprocess,
	RipGrepper.Common.Constants,
	RipGrepper.Common.Interfaces,
	ArrayEx,
	System.SysUtils;

type
	TSimpleProcessOutputStringReader = class(TInterfacedObject, INewLineEventHandler)
		procedure OnNewOutputLine(const _iLineNr : integer; const _sLine : string; _bIsLast : Boolean = False);

		private
			FOutputText : TStrings;
			function GetOutputText : TStrings;

		public
			constructor Create;
			destructor Destroy; override;
			class procedure RunProcess(const _exe : string; _args : TStrings; _workDir : string; out _stdOut : TStrings); overload;
			property OutputText : TStrings read GetOutputText;
	end;

	TProcessUtils = class(TObject)
		private
			class var FProcessedLineCount : Integer;

			class procedure BuffToLine(const _sBuf : string; const _iCnt : integer; var sLineOut : string;
				_newLineHandler : INewLineEventHandler);
			class function EncodeBuffer(const byteBuff : TBytes; var sBuff : string; var iBuffLength : integer; var enc : TEncoding)
				: Boolean;
			class procedure GoToNextCRLF(var P : PChar; const PEndVal : PChar);
			class function GoTillCRLF(var P : PChar; const PEndVal : PChar) : Integer;

			class procedure NewLineEventHandler(_obj : INewLineEventHandler; const _s : string; const _bIsLast : Boolean = False);
			class procedure EOFProcessingEventHandler(_obj : IEOFProcessEventHandler);
			class function GetDefaultEncodingName : string;
			class function GetDefaultEncodingCodePage : Integer;
			class function GetEncodingName(Encoding : TEncoding) : string;

		protected
			class procedure ProcessLastLine(const sLineOut : string; _newLineHandler : INewLineEventHandler;
				_eofProcHandler : IEOFProcessEventHandler);

		public
			class function MaybeQuoteIfNotQuoted(const _s : string; const _delimiter : string = '"') : string;
			class function ProcessOutput(const _s : TStream;
				{ } _newLineHandler : INewLineEventHandler;
				{ } _terminateEventProducer : ITerminateEventProducer;
				{ } _eofProcHandler : IEOFProcessEventHandler) : integer;
			class function RunProcess(const _exe : string; _args : TStrings; _workDir : string;
				{ } _newLIneHandler : INewLineEventHandler;
				{ } _terminateEventProducer : ITerminateEventProducer;
				{ } _eofProcHandler : IEOFProcessEventHandler) : Integer; overload;
			class procedure RunProcess(const _exe : string; _args : TStrings; _workDir : string; out _stdOut : TStrings); overload;
			class function GetCommandLineLength(const _exe : string; const _args : TStrings) : integer; overload;
			class function RunProcessAsync(const _exe : string; const _args : TStrings; const _workDir : string;
				{ } _newLineHandler : INewLineEventHandler;
				{ } _terminateEventProducer : ITerminateEventProducer;
				{ } _eofProcHandler : IEOFProcessEventHandler) : Boolean;
	end;

implementation

uses

	RipGrepper.Tools.DebugUtils,
	Winapi.Windows,
	Winapi.ShellAPI,
	System.Threading,
	System.AnsiStrings,
	System.Math;

class function TProcessUtils.MaybeQuoteIfNotQuoted(const _s : string; const _delimiter : string = '"') : string;
begin
	if (Pos(' ', _s) <> 0) and (Pos(_delimiter, _s) = 0) then
		Result := _delimiter + _s + _delimiter
	else
		Result := _s;
end;

class procedure TProcessUtils.NewLineEventHandler(_obj : INewLineEventHandler; const _s : string; const _bIsLast : Boolean = False);
begin
	if (AtomicIncrement(FProcessedLineCount) <= RG_PROCESSING_LINE_COUNT_LIMIT) then begin
		if Assigned(_obj) then begin
			_obj.OnNewOutputLine(FProcessedLineCount, _s, _bIsLast);
		end;
	end else begin
		if FProcessedLineCount = RG_PROCESSING_LINE_COUNT_LIMIT + 1 then
			TDebugUtils.DebugMessage(Format('Too many results: %d', [FProcessedLineCount]));
	end;
end;

class procedure TProcessUtils.BuffToLine(const _sBuf : string; const _iCnt : integer; var sLineOut : string;
	_newLineHandler : INewLineEventHandler);
var
	P, PStartVal, PEndVal : PChar;
	iLineEndFound : integer;
	S : string;
begin
	P := Pointer(_sBuf);
	if (P = nil) or (_iCnt = 0) then
		Exit;
	PEndVal := P + _iCnt;
	while P < PEndVal do begin
		PStartVal := P;
		GoToNextCRLF(P, PEndVal);
		SetString(S, PStartVal, P - PStartVal);
		sLineOut := sLineOut + S; // TODO: FastMM reports here Memory leak

		iLineEndFound := GoTillCRLF(P, PEndVal);

		// Empty lines will be skipped
		if (iLineEndFound > 0) and (not sLineOut.IsEmpty) then begin
			NewLineEventHandler(_newLineHandler, sLineOut);
			sLineOut := '';
		end;
	end;
end;

class function TProcessUtils.EncodeBuffer(const byteBuff : TBytes; var sBuff : string; var iBuffLength : integer; var enc : TEncoding)
	: Boolean;
begin
	Result := True;
	try
		if Assigned(enc) then begin
			if enc = TEncoding.ANSI then begin
				sBuff := TEncoding.ANSI.GetString(byteBuff, 0, iBuffLength);
			end else if enc = TEncoding.Default then begin
				sBuff := TEncoding.GetEncoding(GetDefaultEncodingCodePage()).GetString(byteBuff, 0, iBuffLength);
			end else begin
				sBuff := enc.GetString(byteBuff, 0, iBuffLength);
			end;
		end else begin
			sBuff := TEncoding.UTF8.GetString(byteBuff, 0, iBuffLength); // unicode conversion
		end;
	except
		on E : EEncodingError do begin
			enc := nil;
			TEncoding.GetBufferEncoding(byteBuff, enc);
			sBuff := enc.GetString(byteBuff);
			Result := False;
		end;
	end;
end;

class procedure TProcessUtils.GoToNextCRLF(var P : PChar; const PEndVal : PChar);
begin
	while (P < PEndVal) and not(CharInSet(P^, [CR, LF])) do
		Inc(P);
end;

class procedure TProcessUtils.EOFProcessingEventHandler(_obj : IEOFProcessEventHandler);
begin
	if Assigned(_obj) then begin
		_obj.OnEOFProcess();
	end;
end;

class function TProcessUtils.GoTillCRLF(var P : PChar; const PEndVal : PChar) : Integer;
begin
	Result := 0;
	while (P < PEndVal) and CharInSet(P^, [CR, LF]) do begin
		Inc(Result);
		Inc(P);
	end;
end;

class procedure TProcessUtils.ProcessLastLine(const sLineOut : string; _newLineHandler : INewLineEventHandler;
	_eofProcHandler : IEOFProcessEventHandler);
begin
	// if sLineOut <> '' then begin
	NewLineEventHandler(_newLineHandler, sLineOut, True);
	// end;
	EOFProcessingEventHandler(_eofProcHandler);
end;

class function TProcessUtils.ProcessOutput(const _s : TStream;
	{ } _newLineHandler : INewLineEventHandler;
	{ } _terminateEventProducer : ITerminateEventProducer;
	{ } _eofProcHandler : IEOFProcessEventHandler) : integer;
var
	byteBuff : TBytes;
	sBuff : string;
	iBuffLength : integer;
	sLineOut : string;
	enc : TEncoding;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TTProcessUtils.ProcessOutput');

	FProcessedLineCount := 0;
	{ Now process the output }
	SetLength(byteBuff, BUFF_LENGTH);
	try
		enc := nil;
		repeat
			if (Assigned(_terminateEventProducer) and _terminateEventProducer.ProcessShouldTerminate()) then begin
				dbgMsg.Msg('Process should terminate');
				break
			end;
			iBuffLength := 0;
			if (_s <> nil) then begin
				iBuffLength := _s.Read(Pointer(byteBuff)^, Length(byteBuff));
			end;

			while not EncodeBuffer(byteBuff, sBuff, iBuffLength, enc) do begin
				dbgMsg.Msg('Encoding not succeded, so we try this ' + GetEncodingName(enc));
			end;
			BuffToLine(sBuff, sBuff.Length, sLineOut, _newLineHandler);
		until iBuffLength = 0;

		ProcessLastLine(sLineOut, _newLineHandler, _eofProcHandler);
	except
		on E : Exception do begin
			dbgMsg.Msg('Exception: ' + E.Message + CRLF + 'line:' + sLineOut);
			raise;
		end;
	end;
	Result := FProcessedLineCount;
end;

class function TProcessUtils.GetEncodingName(Encoding : TEncoding) : string;
begin
	if Encoding = TEncoding.UTF7 then
		Result := 'UTF-7'
	else if Encoding = TEncoding.UTF8 then
		Result := 'UTF-8'
	else if Encoding = TEncoding.Unicode then
		Result := 'UTF-16 LE'
	else if Encoding = TEncoding.BigEndianUnicode then
		Result := 'UTF-16 BE'
	else if Encoding = TEncoding.ANSI then
		Result := 'ANSI'
	else if Encoding = TEncoding.ASCII then
		Result := 'ASCII'
	else if Encoding = TEncoding.Default then begin
		Result := 'System Default = ' + GetDefaultEncodingName();
	end else begin
		Result := 'Unknown Encoding';
	end;
end;

class function TProcessUtils.GetDefaultEncodingName : string;
var CPInfoEx : TCPInfoEx;
begin
	if GetCPInfoEx(CP_ACP, 0, CPInfoEx) then
		Result := CPInfoEx.CodePageName
	else
		Result := 'Unknown Encoding';
end;

class function TProcessUtils.RunProcess(const _exe : string; _args : TStrings;
	{ } _workDir : string;
	{ } _newLIneHandler : INewLineEventHandler;
	{ } _terminateEventProducer : ITerminateEventProducer;
	{ } _eofProcHandler : IEOFProcessEventHandler) : Integer;
var p : TProcess; cmdLineLength : integer;
begin
	p := TProcess.Create(nil);
	try
		p.Executable := MaybeQuoteIfNotQuoted(_exe);
		p.Parameters.Assign(_args);
		// p.ShowWindow := swoShowNormal; // Is this needed?
		p.ShowWindow := swoHIDE;
		p.Options := p.Options + [poNewConsole, poUsePipes, poStdErrToOutPut];
		p.CurrentDirectory := _workDir;

		cmdLineLength := GetCommandLineLength(_exe, _args);
		if cmdLineLength >= MAX_COMMAND_LINE_LENGTH then begin
			ProcessLastLine('The command line is too long', _newLineHandler, _eofProcHandler);
			Result := RG_ERROR;
			Exit;
		end;
		TDebugUtils.DebugMessage('Running command ' + p.Executable);
		TDebugUtils.DebugMessage('arguments: ' + p.Parameters.Text);
		try
			p.Execute;

			ProcessOutput(p.Output, _newLIneHandler, _terminateEventProducer, _eofProcHandler);

			if (Assigned(_terminateEventProducer) and _terminateEventProducer.ProcessShouldTerminate()) then begin
				Result := IfThen(p.Terminate(PROCESS_TERMINATE), ERROR_CANCELLED, 1);
				TDebugUtils.DebugMessage(Format('Process should terminate returned: %s', [Result.ToString, True]));
			end else begin
				p.WaitOnExit;
			end;
			Result := p.ExitStatus;
			case Result of
				RG_SUCCESS :
				;
				RG_NO_MATCH :
				NewLineEventHandler(_newLIneHandler, p.Executable + RG_HAS_NO_OUTUT);
				RG_ERROR :
				NewLineEventHandler(_newLIneHandler, p.Executable + RG_ENDED_ERROR + p.ExitStatus.ToString);
			end;
		except
			on E : Exception do begin
				ProcessLastLine(E.Message, _newLineHandler, _eofProcHandler);
				Result := RG_ERROR;
			end;
		end;

	finally
		FreeAndNil(p);
	end;
end;

class procedure TProcessUtils.RunProcess(const _exe : string; _args : TStrings; _workDir : string; out _stdOut : TStrings);
begin
	var
	sor := TSimpleProcessOutputStringReader.Create;
	try

	finally
		sor.Free;
	end;
end;

class function TProcessUtils.GetCommandLineLength(const _exe : string; const _args : TStrings) : integer;
begin
	Result := MaybeQuoteIfNotQuoted(_exe).Length + 1 + _args.Text.Length;
end;

class function TProcessUtils.GetDefaultEncodingCodePage : Integer;
var CPInfoEx : TCPInfoEx;
begin
	if GetCPInfoEx(CP_ACP, 0, CPInfoEx) then begin
		Result := CPInfoEx.CodePage;
	end else begin
		Result := -1;
	end;
end;

class function TProcessUtils.RunProcessAsync(const _exe : string; const _args : TStrings; const _workDir : string;
	_newLineHandler : INewLineEventHandler; _terminateEventProducer : ITerminateEventProducer; _eofProcHandler : IEOFProcessEventHandler)
	: Boolean;
var task : ITask;
begin
	task := TTask.Run(
		procedure
		begin
			TThread.Synchronize(nil,
				procedure
				begin
					RunProcess(_exe, _args, _workdir, _newLineHandler, _terminateEventProducer, _eofProcHandler);
				end);
		end);
	Result := task.Status = TTAskStatus.Running;
end;

constructor TSimpleProcessOutputStringReader.Create;
begin
	inherited;
	FOutputText := TStringList.Create;
end;

destructor TSimpleProcessOutputStringReader.Destroy;
begin
	FOutputText.Free;
	inherited;
end;

function TSimpleProcessOutputStringReader.GetOutputText : TStrings;
begin
	Result := FOutputText;
end;

procedure TSimpleProcessOutputStringReader.OnNewOutputLine(const _iLineNr : integer; const _sLine : string; _bIsLast : Boolean = False);
begin
	FOutputText.Add(_sLine);
end;

class procedure TSimpleProcessOutputStringReader.RunProcess(const _exe : string; _args : TStrings; _workDir : string;
out _stdOut : TStrings);
var spor : TSimpleProcessOutputStringReader;
begin
	spor := TSimpleProcessOutputStringReader.Create; // It will be freed... How?
	TProcessUtils.RunProcess(_exe, _args, _workDir, spor as INewLineEventHandler, nil, nil);
	_stdOut.Clear;
	_stdOut.Assign(spor.OutputText);
end;

end.
