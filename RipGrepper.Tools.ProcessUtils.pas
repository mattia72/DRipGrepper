unit RipGrepper.Tools.ProcessUtils;

interface

uses
	System.Classes,
	dprocess,
	RipGrepper.Common.Types;

type
	TProcessUtils = class(TObject)
		const
			BUFF_LENGTH = 1024;

		private
			class procedure BuffToLine(const sBuf : ansistring; const iCnt : integer; var bPrevWasCrLf : Boolean; var sLineOut : string;
				_newLineHandler : INewLineEventHandler);
			class procedure BuffToLine1(const sBuf : ansistring; const iCnt : integer; var bPrevWasCrLf : Boolean; var sLineOut : string;
				_newLineHandler : INewLineEventHandler);
			class procedure GoToNextCRLF(var P : PAnsiChar; const PEndVal : PAnsiChar);
			class function IncTillCRLF(var P : PAnsiChar; const PEndVal : PAnsiChar) : Boolean;

		protected
			class procedure NewLineEventHandler(_obj : INewLineEventHandler; const _s : string);

		public
			class function MaybeQuoteIfNotQuoted(const _s : string; const _delimiter : string = '"') : string;
			class procedure ProcessOutput(const _s : TStream; { } _newLineHandler : INewLineEventHandler;
				_terminateEventProducer : ITerminateEventProducer);
			class function RunProcess(const _exe : string; _args : TStrings; _workDir : string; _newLIneHandler : INewLineEventHandler;
				_terminateEventProducer : ITerminateEventProducer) : Boolean;
			class function RunProcessAsync(const _exe : string; const _args : TStrings; const _workDir : string;
				_newLineHandler : INewLineEventHandler; _terminateEventProducer : ITerminateEventProducer) : Boolean;
	end;

implementation

uses

	System.SysUtils,
	RipGrepper.Tools.DebugTools,
	Winapi.Windows,
	Winapi.ShellAPI,
	System.Threading,
	System.AnsiStrings;

class function TProcessUtils.MaybeQuoteIfNotQuoted(const _s : string; const _delimiter : string = '"') : string;
begin
	if (Pos(' ', _s) <> 0) and (Pos(_delimiter, _s) = 0) then
		Result := _delimiter + _s + _delimiter
	else
		Result := _s;
end;

class procedure TProcessUtils.NewLineEventHandler(_obj : INewLineEventHandler; const _s : string);
begin
	if Assigned(_obj) then begin
		_obj.OnNewOutputLine(_s);
	end;
end;

class procedure TProcessUtils.BuffToLine(const sBuf : ansistring; const iCnt : integer; var bPrevWasCrLf : Boolean; var sLineOut : string;
	_newLineHandler : INewLineEventHandler);
var
	bCurrentIsCrLf : Boolean;
	i : integer;
	iLineStartIndex : integer;
begin
	iLineStartIndex := 1;
	i := 1;
	while (i <= iCnt) do begin
		bCurrentIsCrLf := CharInSet(sBuf[i], [CR, LF]);
		if bCurrentIsCrLf then begin
			if (i <> 1) then begin
				// line shouldn't begin with cr or lf
				sLineOut := sLineOut + Copy(string(sBuf), iLineStartIndex, i - iLineStartIndex);
				if (not bPrevWasCrLf) then begin
					// if prev was crlf and next won't be crlf
					NewLineEventHandler(_newLineHandler, sLineOut);
				end;
			end;

			sLineOut := '';
			if (i <> 1) and (i < iCnt) and
			{ } bCurrentIsCrLf and // (sBuf[i] <> sBuf[i + 1]) and
			{ } not bPrevWasCrLf and
			{ } CharInSet(sBuf[i + 1], [CR, LF]) then begin
				Inc(i);
			end;
			iLineStartIndex := i + 1;
			bPrevWasCrLf := True;
		end else begin
			bPrevWasCrLf := False;
		end;
		Inc(i);
	end;
	sLineOut := Copy(string(sBuf), iLineStartIndex, iCnt - iLineStartIndex + 1);
end;

class procedure TProcessUtils.BuffToLine1(const sBuf : ansistring; const iCnt : integer; var bPrevWasCrLf : Boolean; var sLineOut : string;
	_newLineHandler : INewLineEventHandler);
var
	bCurrentIsCrLf : Boolean;
	P, PStartVal, PEndVal : PAnsiChar;
	bLineEndFound : Boolean;
	S : string;
begin
	P := Pointer(sBuf);
	if (P = nil) or (iCnt = 0) then
		Exit;
	PEndVal := P + iCnt;
	while P < PEndVal do begin
		PStartVal := P;
		GoToNextCRLF(P, PEndVal);
		SetString(S, PStartVal, P - PStartVal);
		sLineOut := sLineOut + S;

		bLineEndFound := IncTillCRLF(P, PEndVal);

		if (bLineEndFound) then begin
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

class function TProcessUtils.IncTillCRLF(var P : PAnsiChar; const PEndVal : PAnsiChar) : Boolean;
begin
	Result := False;
	while (P < PEndVal) and (P^ in [CR, LF]) do begin
		Result := True;
		Inc(P);
	end;
end;

class procedure TProcessUtils.ProcessOutput(const _s : TStream;
	{ } _newLineHandler : INewLineEventHandler;
	{ } _terminateEventProducer : ITerminateEventProducer);
var
	bCurrentIsCrLf : Boolean;
	bPrevWasCrLf : Boolean;
	sBuf : ansistring;
	iCnt : integer;
	i : integer;
	iLineStartIndex : integer;
	sLineOut : string;
begin
	{ Now process the output }
	SetLength(sBuf, BUFF_LENGTH);
	bPrevWasCrLf := False; // warning
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

		BuffToLine1(sBuf, iCnt, bPrevWasCrLf, sLineOut, _newLineHandler);
	until iCnt = 0;

	if sLineOut <> '' then begin
		NewLineEventHandler(_newLineHandler, sLineOut);
	end;
end;

class function TProcessUtils.RunProcess(const _exe : string; _args : TStrings;
	{ } _workDir : string;
	{ } _newLIneHandler : INewLineEventHandler;
	{ } _terminateEventProducer : ITerminateEventProducer) : Boolean;
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

		ProcessOutput(p.Output, _newLIneHandler, _terminateEventProducer);
		if (_terminateEventProducer.ProcessShouldTerminate()) then begin
			Result := p.Terminate(PROCESS_TERMINATE);
			TDebugUtils.DebugMessage(Format('Process should terminate returned: %s', [BoolToStr(Result, True)]))
		end else begin
			p.WaitOnExit;
		end;
		Result := p.ExitStatus = ERROR_SUCCESS;
		if not Result then begin
			NewLineEventHandler(_newLIneHandler, p.Executable + ' failed with exit code: ' + p.ExitStatus.ToString);
		end;
	finally
		FreeAndNil(p);
	end;
end;

class function TProcessUtils.RunProcessAsync(const _exe : string; const _args : TStrings;
	{ } const _workDir : string;
	{ } _newLineHandler : INewLineEventHandler;
	{ } _terminateEventProducer : ITerminateEventProducer) : Boolean;
var
	task : ITask;
begin
	task := TTask.Run(
		procedure
		begin
			TThread.Synchronize(nil,
				procedure
				begin
					RunProcess(_exe, _args, _workdir, _newLineHandler, _terminateEventProducer);
				end);
		end);
	Result := task.Status = TTAskStatus.Running;
end;

end.
