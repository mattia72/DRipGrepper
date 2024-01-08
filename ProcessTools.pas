unit ProcessTools;

interface

uses
	System.Classes;

type
	INewLineEventHandler = interface
		procedure OnNewResultLine(const _sLine : string);
	end;

	TNewLineEventHandler = procedure(_obj : INewLineEventHandler; const _s : string);

type
	TProcessUtils = class(TObject)
		private
		protected
			class procedure NewLineEventHandler(_obj : INewLineEventHandler; const _s : string);

		public
			class function GetDosOutput(const _cmdLine, _workDir : string; _handler : INewLineEventHandler) : string;
			class function MaybeQuoteIfNotQuoted(const _s: string; const _delimiter: string = '"'): string;
			class function RunProcess(const _exe : string; _args : TStrings; _workDir : string; _handler : INewLineEventHandler) : Boolean;
	end;

type
	TFileUtils = class(TObject)
		private
		public
			class function FindExecutable(sFileName : string; out sOutpuPath : string) : Boolean;
			class function GetValidPath(const aPath : string) : string;
	end;

implementation

uses
	dprocess,
	System.SysUtils,
	DebugTools,
	Winapi.Windows,
	Winapi.ShellAPI;

class function TProcessUtils.GetDosOutput(const _cmdLine, _workDir : string; _handler : INewLineEventHandler) : string;
const
	BUFF_LENGTH = 1024;
var
	SA : TSecurityAttributes;
	SI : TStartupInfo;
	PI : TProcessInformation;
	StdOutPipeRead, StdOutPipeWrite : THandle;
	WasOK : Boolean;
	Buffer : PAnsiChar;
	BytesRead : Cardinal;
	Handle : Boolean;
begin
	Result := '';
	SA.nLength := SizeOf(SA);
	SA.bInheritHandle := True;
	SA.lpSecurityDescriptor := nil;
	CreatePipe(StdOutPipeRead, StdOutPipeWrite, @SA, 0);
	try

		FillChar(SI, SizeOf(SI), #0);
		SI.cb := SizeOf(SI);
		SI.dwFlags := STARTF_USESHOWWINDOW or STARTF_USESTDHANDLES;
		SI.wShowWindow := SW_HIDE;
		SI.hStdInput := GetStdHandle(STD_INPUT_HANDLE); // don't redirect stdin
		SI.hStdOutput := StdOutPipeWrite;
		SI.hStdError := StdOutPipeWrite;

		var
		cmd := 'cmd.exe /C ' + _cmdLine; // .Replace(string('"'), string('\"'), [rfReplaceAll]);
		// cmd := _cmdLine;
		UniqueString(cmd);
		Handle := CreateProcess(nil, PChar(cmd), nil, nil, True, 0, nil, PChar(_workDir), SI, PI);
		CloseHandle(StdOutPipeWrite);
		if Handle then begin
			Buffer := AllocMem(BUFF_LENGTH + 1);
			try
				repeat
					WasOK := ReadFile(StdOutPipeRead, Buffer[0], BUFF_LENGTH, BytesRead, nil);
					if BytesRead > 0 then begin
						Buffer[BytesRead] := #0;
						var
							s : string := Buffer;
						NewLineEventHandler(_handler, s);
						Result := Result + s;
					end;
				until not WasOK or (BytesRead = 0);
				WaitForSingleObject(PI.hProcess, INFINITE);
			finally
				CloseHandle(PI.hThread);
				CloseHandle(PI.hProcess);
			end;
		end;
	finally
		CloseHandle(StdOutPipeRead);
	end;
end;

class function TProcessUtils.MaybeQuoteIfNotQuoted(const _s: string; const _delimiter: string = '"'): string;
begin
	if (Pos(' ', _s) <> 0) and (Pos(_delimiter, _s) = 0) then
		Result := _delimiter + _s + _delimiter
	else
		Result := _s;
end;

class procedure TProcessUtils.NewLineEventHandler(_obj : INewLineEventHandler; const _s : string);
begin
	_obj.OnNewResultLine(_s);
end;

class function TProcessUtils.RunProcess(const _exe : string; _args : TStrings; _workDir : string; _handler : INewLineEventHandler)
	: Boolean;
const
	BufSize = 1024;
var
	p : TProcess;
	Buf : string;
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
		SetLength(Buf, BufSize);
		repeat
			if (p.Output <> nil) then begin
				Count := p.Output.Read(Buf[1], Length(Buf));
				// Count := p.Output.Read(pchar(Buf)^, BufSize);
				// L505 todo: try this when using unicodestring buffer
				// writeln('DEBUG: len buf: ', length(buf));
			end else begin
				Count := 0;
			end;
			LineStart := 1;
			i := 1;
			while (i <= Count) do begin
				// L505
				// if Buf[i] in [#10,#13] then
				if CharInSet(Buf[i], [#10, #13]) then begin
					OutputLine := OutputLine + Copy(Buf, LineStart, i - LineStart);

					NewLineEventHandler(_handler, OutputLine);
					OutputLine := '';
					// L505
					// if (i<Count) and (Buf[i+1] in [#10,#13]) and (Buf[i]<>Buf[i+1]) then
					if (i < Count) and (CharInSet(Buf[i], [#10, #13])) and (Buf[i] <> Buf[i + 1]) then
						Inc(i);
					LineStart := i + 1;
				end;
				Inc(i);
			end;
			OutputLine := Copy(Buf, LineStart, Count - LineStart + 1);
		until Count = 0;
		if OutputLine <> '' then
			NewLineEventHandler(_handler, OutputLine);
		p.WaitOnExit;
		Result := p.ExitStatus = 0;
		if not Result then begin
			NewLineEventHandler(_handler, 'Command ' + p.Executable + ' failed with exit code: ' + p.ExitStatus.ToString);
		end;
	finally
		FreeAndNil(p);
	end;
end;

class function TFileUtils.FindExecutable(sFileName : string; out sOutpuPath : string) : Boolean;
var
	Buffer : array [0 .. MAX_PATH] of Char;
	res : integer;
begin
	Result := True;
	res := Winapi.ShellAPI.FindExecutable(PChar(sFileName), PChar(nil), &Buffer);
	case res of
		0, ERROR_FILE_NOT_FOUND, ERROR_PATH_NOT_FOUND :
		Result := False;
		else begin
			SetString(sOutpuPath, PChar(@Buffer[0]), Length(Buffer));
			SetLength(sOutpuPath, Pos(#0, sOutpuPath) - 1);
		end;
	end;

	TDebugUtils.DebugMessage(sFileName + ' path:' + sOutpuPath);
	// WriteDebugMessage(sOutpuPath);
end;

class function TFileUtils.GetValidPath(const aPath : string) : string;
begin
	Result := aPath;
	if Length(Result) > 0 then
		Result := IncludeTrailingPathDelimiter(Result);
end;

end.
