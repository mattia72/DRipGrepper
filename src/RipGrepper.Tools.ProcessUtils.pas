unit RipGrepper.Tools.ProcessUtils;

interface

uses
	System.Classes,
	dprocess,
	RipGrepper.Common.Types,
	RipGrepper.Common.Interfaces;

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

			class procedure BuffToLine(const _sBuf : ansistring; const _iCnt : integer; var sLineOut : string;
				_newLineHandler : INewLineEventHandler);
			class procedure GoToNextCRLF(var P : PAnsiChar; const PEndVal : PAnsiChar);
			class function GoTillCRLF(var P : PAnsiChar; const PEndVal : PAnsiChar) : Integer;

			class procedure NewLineEventHandler(_obj : INewLineEventHandler; const _s : string; const _bIsLast : Boolean = False);
			class procedure EOFProcessingEventHandler(_obj : IEOFProcessEventHandler);

		public
			class function MaybeQuoteIfNotQuoted(const _s : string; const _delimiter : string = '"') : string;
			class procedure ProcessOutput(const _s : TStream;
				{ } _newLineHandler : INewLineEventHandler;
				{ } _terminateEventProducer : ITerminateEventProducer;
				{ } _eofProcHandler : IEOFProcessEventHandler);
			class function RunProcess(const _exe : string; _args : TStrings; _workDir : string;
				{ } _newLIneHandler : INewLineEventHandler;
				{ } _terminateEventProducer : ITerminateEventProducer;
				{ } _eofProcHandler : IEOFProcessEventHandler) : Integer; overload;
			class procedure RunProcess(const _exe : string; _args : TStrings; _workDir : string; out _stdOut : TStrings); overload;
			class function RunProcessAsync(const _exe : string; const _args : TStrings; const _workDir : string;
				{ } _newLineHandler : INewLineEventHandler;
				{ } _terminateEventProducer : ITerminateEventProducer;
				{ } _eofProcHandler : IEOFProcessEventHandler) : Boolean;
	end;

implementation

uses

	System.SysUtils,
	RipGrepper.Tools.DebugTools,
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

class procedure TProcessUtils.BuffToLine(const _sBuf : ansistring; const _iCnt : integer; var sLineOut : string;
	_newLineHandler : INewLineEventHandler);
var
	P, PStartVal, PEndVal : PAnsiChar;
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
		sLineOut := sLineOut + S;  // TODO: FastMM reports here Memory leak

		iLineEndFound := GoTillCRLF(P, PEndVal);

		// Empty lines will be skipped
		if (iLineEndFound > 0) and (not sLineOut.IsEmpty) then begin
			NewLineEventHandler(_newLineHandler, sLineOut);
			sLineOut := '';
		end;
	end;
end;

class procedure TProcessUtils.GoToNextCRLF(var P : PAnsiChar; const PEndVal : PAnsiChar);
begin
	while (P < PEndVal) and not(P^ in [CR, LF]) do
		Inc(P);
end;

class procedure TProcessUtils.EOFProcessingEventHandler(_obj : IEOFProcessEventHandler);
begin
	if Assigned(_obj) then begin
		_obj.OnEOFProcess();
	end;
end;

class function TProcessUtils.GoTillCRLF(var P : PAnsiChar; const PEndVal : PAnsiChar) : Integer;
begin
	Result := 0;
	while (P < PEndVal) and (P^ in [CR, LF]) do begin
		Inc(Result);
		Inc(P);
	end;
end;

class procedure TProcessUtils.ProcessOutput(const _s : TStream;
	{ } _newLineHandler : INewLineEventHandler;
	{ } _terminateEventProducer : ITerminateEventProducer;
	{ } _eofProcHandler : IEOFProcessEventHandler);
var
	sBuff : ansistring;
	iBuffLength : integer;
	sLineOut : string;
begin
	FProcessedLineCount := 0;
	{ Now process the output }
	SetLength(sBuff, BUFF_LENGTH);

	repeat
		if (Assigned(_terminateEventProducer) and _terminateEventProducer.ProcessShouldTerminate()) then begin
			TDebugUtils.DebugMessage('Process should terminate');
			break
		end;
		iBuffLength := 0;
		if (_s <> nil) then begin
			// L505 todo: try this when using unicodestring buffer
			// Count := _s.Output.Read(pchar(Buf)^, BUFF_LENGTH);
			iBuffLength := _s.Read(sBuff[1], Length(sBuff));
		end;

		BuffToLine(sBuff, iBuffLength, sLineOut, _newLineHandler);
	until iBuffLength = 0;

	// if sLineOut <> '' then begin
	NewLineEventHandler(_newLineHandler, sLineOut, True);
	// end;
	EOFProcessingEventHandler(_eofProcHandler);
end;

class function TProcessUtils.RunProcess(const _exe : string; _args : TStrings;
	{ } _workDir : string;
	{ } _newLIneHandler : INewLineEventHandler;
	{ } _terminateEventProducer : ITerminateEventProducer;
	{ } _eofProcHandler : IEOFProcessEventHandler) : Integer;
var
	p : TProcess;
begin
	p := TProcess.Create(nil);
	try
		p.Executable := MaybeQuoteIfNotQuoted(_exe);
		p.Parameters.Assign(_args);
		// p.ShowWindow := swoShowNormal; // Is this needed?
		p.ShowWindow := swoHIDE;
		p.Options := p.Options + [poNewConsole, poUsePipes, poStdErrToOutPut];
		p.CurrentDirectory := _workDir;

		TDebugUtils.DebugMessage('Running command ' + p.Executable);
		TDebugUtils.DebugMessage('arguments: ' + p.Parameters.Text);
		p.Execute;

		ProcessOutput(p.Output, _newLIneHandler, _terminateEventProducer, _eofProcHandler);

		if (Assigned(_terminateEventProducer) and _terminateEventProducer.ProcessShouldTerminate()) then begin
			Result := IfThen(p.Terminate(PROCESS_TERMINATE), ERROR_CANCELLED, 1);
			TDebugUtils.DebugMessage(Format('Process should terminate returned: %s', [Result.ToString, True]));
		end else begin
			p.WaitOnExit;
		end;
		Result := p.ExitStatus;
		if Result = RIPGREP_ERROR then begin
			NewLineEventHandler(_newLIneHandler, p.Executable + ' failed with exit code: ' + p.ExitStatus.ToString);
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

class function TProcessUtils.RunProcessAsync(const _exe : string; const _args : TStrings; const _workDir : string;
	_newLineHandler : INewLineEventHandler; _terminateEventProducer : ITerminateEventProducer; _eofProcHandler : IEOFProcessEventHandler)
	: Boolean;
var
	task : ITask;
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
var
	spor : TSimpleProcessOutputStringReader;
begin
	spor := TSimpleProcessOutputStringReader.Create; // It will be freed... How?
	TProcessUtils.RunProcess(_exe, _args, _workDir, spor as INewLineEventHandler, nil, nil);
	_stdOut.Clear;
	_stdOut.Assign(spor.OutputText);
end;

end.
