unit RipGrepper.Tools.ProcessUtils;

interface

uses
	System.Classes,
	dprocess,
	RipGrepper.Common.Types,
	RipGrepper.Common.Interfaces;

type
	TProcessUtils = class(TObject)
		private
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
				{ } _eofProcHandler : IEOFProcessEventHandler) : Integer;
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
	if Assigned(_obj) then begin
		_obj.OnNewOutputLine(_s, _bIsLast);
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
		sLineOut := sLineOut + S;

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
	sBuf : ansistring;
	iCnt : integer;
	sLineOut : string;
begin
	{ Now process the output }
	SetLength(sBuf, BUFF_LENGTH);

	repeat
		if (_terminateEventProducer.ProcessShouldTerminate()) then begin
			TDebugUtils.DebugMessage('Process should terminate');
			break
		end;
		iCnt := 0;
		if (_s <> nil) then begin
			// L505 todo: try this when using unicodestring buffer
			// Count := _s.Output.Read(pchar(Buf)^, BUFF_LENGTH);
			iCnt := _s.Read(sBuf[1], Length(sBuf));
		end;

		BuffToLine(sBuf, iCnt, sLineOut, _newLineHandler);
	until iCnt = 0;

	// if sLineOut <> '' then begin
	NewLineEventHandler(_newLineHandler, sLineOut, True);
	// end;
	_eofProcHandler.OnEOFProcess();
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

		if (_terminateEventProducer.ProcessShouldTerminate()) then begin
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

end.
