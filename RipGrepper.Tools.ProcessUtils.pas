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

		protected
			class procedure NewLineEventHandler(_obj : INewLineEventHandler; const _s : string);

		public
			class function MaybeQuoteIfNotQuoted(const _s : string; const _delimiter : string = '"') : string;
			class procedure ProcessOutput(const _s : TStream; _handler : INewLineEventHandler);
			class function RunProcess(const _exe : string; _args : TStrings; _workDir : string; _handler : INewLineEventHandler) : Boolean;
			class function RunProcessAsync(const _exe : string; _args : TStrings; _workDir : string; _handler : INewLineEventHandler)
				: Boolean;
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

class procedure TProcessUtils.ProcessOutput(const _s : TStream; _handler : INewLineEventHandler);
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
	repeat
		if (_s <> nil) then begin
			iCnt := _s.Read(sBuf[1], Length(sBuf));
			// L505 todo: try this when using unicodestring buffer
			// Count := _s.Output.Read(pchar(Buf)^, BUFF_LENGTH);
		end else begin
			iCnt := 0;
		end;
		iLineStartIndex := 1;
		i := 1;
		while (i <= iCnt) do begin
			bCurrentIsCrLf := CharInSet(sBuf[i], [CR, LF]);
			if bCurrentIsCrLf then begin
				if (i <> 1) then begin
					// line shouldn't begin with cr or lf
					sLineOut := sLineOut + Copy(string(sBuf), iLineStartIndex, i - iLineStartIndex);
					if not bPrevWasCrLf then begin
						NewLineEventHandler(_handler, sLineOut);
					end;
				end;

				sLineOut := '';
				if (i <> 1) and (i < iCnt) and
				{ } bCurrentIsCrLf and (sBuf[i] <> sBuf[i + 1]) and
				{ } not bPrevWasCrLf then begin
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
	until iCnt = 0;

	if sLineOut <> '' then begin
		NewLineEventHandler(_handler, sLineOut);
	end;
end;

class function TProcessUtils.RunProcess(const _exe : string; _args : TStrings; _workDir : string; _handler : INewLineEventHandler)
	: Boolean;
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

		ProcessOutput(p.Output, _handler);

		p.WaitOnExit;
		Result := p.ExitStatus = 0;
		if not Result then begin
			NewLineEventHandler(_handler, p.Executable + ' failed with exit code: ' + p.ExitStatus.ToString);
		end;
	finally
		FreeAndNil(p);
	end;
end;

class function TProcessUtils.RunProcessAsync(const _exe : string; _args : TStrings; _workDir : string; _handler : INewLineEventHandler)
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
					RunProcess(_exe, _args, _workdir, _handler);
				end);
		end);
	Result := task.Status = TTAskStatus.Running;
end;

end.
