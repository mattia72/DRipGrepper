unit RipGrepper.Tools.ProcessUtils;

interface

uses
	System.Classes;

type
	INewLineEventHandler = interface
		procedure OnNewOutputLine(const _sLine : string);
	end;

	TNewLineEventHandler = procedure(_obj : INewLineEventHandler; const _s : string);

type
	TProcessUtils = class(TObject)
		const
			BUFF_LENGTH = 1024;

		protected
			class procedure NewLineEventHandler(_obj : INewLineEventHandler; const _s : string);

		public
			class function MaybeQuoteIfNotQuoted(const _s : string; const _delimiter : string = '"') : string;
			class function RunProcess(const _exe : string; _args : TStrings; _workDir : string; _handler : INewLineEventHandler) : Boolean;
			class function RunProcessAsync(const _exe : string; _args : TStrings; _workDir : string; _handler : INewLineEventHandler)
				: Boolean;
	end;

implementation

uses
	dprocess,
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
	_obj.OnNewOutputLine(_s);
end;

class function TProcessUtils.RunProcess(const _exe : string; _args : TStrings; _workDir : string; _handler : INewLineEventHandler)
	: Boolean;
const
	BUFF_LENGTH = 1024;
var
	p : TProcess;
	Buf : ansistring;
	// Buf: string;
	Count : integer;
	i : integer;
	LineStart : integer;
	OutputLine : string;
	// OutputLine: ansistring;
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

		{ Now process the output }
		OutputLine := '';
		SetLength(Buf, BUFF_LENGTH);
		repeat
			if (p.Output <> nil) then begin
				Count := p.Output.Read(Buf[1], Length(Buf));
				// L505 todo: try this when using unicodestring buffer
				// Count := p.Output.Read(pchar(Buf)^, BUFF_LENGTH);
			end else begin
				Count := 0;
			end;
			LineStart := 1;
			i := 1;
			while (i <= Count) do begin
				// L505
				// if Buf[i] in [#10,#13] then
				if CharInSet(Buf[i], [#10, #13]) then begin
					if (i <> 1) then begin
						// line shouldn't begin with crlf
						OutputLine := OutputLine + Copy(string(Buf), LineStart, i - LineStart);
						if (Length(OutputLine) > 0) and (OutputLine[1] = ':') then begin
							TDebugUtils.DebugMessage(Format('Buffer begins with crlf (i:%d ls:%d)|%s|', [i, LineStart, OutputLine]));
						end;
						NewLineEventHandler(_handler, OutputLine);
						// end else begin
						// TDebugUtils.DebugMessage(Format('Buffer begins with crlf (i:%d ls:%d)|%s|',
						// [i, LineStart, Copy(string(Buf), LineStart, i - LineStart)]));
					end;

					OutputLine := '';
					// L505
					// if (i<Count) and (Buf[i+1] in [#10,#13]) and (Buf[i]<>Buf[i+1]) then
					if (i <> 1) and (i < Count) and (CharInSet(Buf[i], [#10, #13])) and (Buf[i] <> Buf[i + 1]) then
						Inc(i);
					LineStart := i + 1;
				end;
				Inc(i);
			end;
			OutputLine := Copy(string(Buf), LineStart, Count - LineStart + 1);
		until Count = 0;

		if OutputLine <> '' then begin
			if OutputLine[1] = ':' then begin
				TDebugUtils.DebugMessage(Format('Buffer begins with crlf (i:%d ls:%d)|%s|', [i, LineStart, OutputLine]));
			end;
			NewLineEventHandler(_handler, OutputLine);
		end;
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
