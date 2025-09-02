unit DRipExtension.VsCodeBridge;

interface

uses
	System.Classes;

type
	TVsCodeBridge = class
		const
			PIPENAME = '\\.\pipe\vscode_delphi_bridge';

		private
			class var FServerThread : TThread;
			class var FStopRequested : Boolean;
			class procedure parseJsonAndDoTheJob(const s : string);

		public
			class procedure StartPipeServer();
			class procedure StopPipeServer();
	end;

implementation

uses
	Windows,
	SysUtils,
	Dialogs,
	ShellApi,
	ActiveX,
	ComObj,
	JSON,
	RipGrepper.Tools.DebugUtils,
	{$IFNDEF STANDALONE}
	RipGrepper.Common.IOTAUtils,
	{$ENDIF}
	Spring;

class procedure TVsCodeBridge.parseJsonAndDoTheJob(const s : string);
var
	jsonObj : IShared<TJSONObject>;
	filePath : string;
	line : Integer;
	column : Integer;
	command : string;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TVsCodeBridge.ListenToPipe');

	jsonObj := Shared.Make<TJsonObject>(TJSONObject.ParseJSONValue(s) as TJSONObject);
	if not Assigned(jsonObj) then begin
		dbgMsg.Msg('JSON parse failed');
		Exit;
	end;

	dbgMsg.Msg('JSON parsed: ' + s);
 	command := jsonObj.GetValue('command').Value;
	if command = 'gotoFileLocation' then begin
		filePath := jsonObj.GetValue('filePath').Value;
		line := StrToIntDef(jsonObj.GetValue('line').Value, 1);
		column := StrToIntDef(jsonObj.GetValue('column').Value, 1);
		TThread.Synchronize(nil,
			procedure
			begin
				var
				dbgMsg := TDebugMsgBeginEnd.New('TThread.Synchronize');
				dbgMsg.Msg('Opening file: ' + filePath + ' at line ' + IntToStr(line) + ', column ' + IntToStr(column));
				{$IFNDEF STANDALONE}
				IOTAUtils.GxOtaGoToFileLineColumn(filePath, line, column, column - 1);
				{$ENDIF}
			end);
	end else if command = 'buildActiveProject' then begin
		TThread.Synchronize(nil,
			procedure
			begin
				var
				dbgMsg := TDebugMsgBeginEnd.New('TThread.Synchronize buildActiveProject');
				dbgMsg.Msg('Building active project');
				{$IFNDEF STANDALONE}
				IOTAUtils.ReloadModifiedFiles();
				IOTAUtils.BuildActiveProject();
				{$ENDIF}
			end);
	end else if command = 'compileActiveProject' then begin
		TThread.Synchronize(nil,
			procedure
			begin
				var
				dbgMsg := TDebugMsgBeginEnd.New('TThread.Synchronize compileActiveProject');
				dbgMsg.Msg('Compiling active project');
				{$IFNDEF STANDALONE}
				IOTAUtils.ReloadModifiedFiles();
				IOTAUtils.CompileActiveProject();
				{$ENDIF}
			end);
	end else if command = 'stop' then begin
		dbgMsg.Msg('Stop command received');
		// No action needed, just unblocks the pipe
	end;
end;

class procedure TVsCodeBridge.StartPipeServer();
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TVsCodeBridge.StartPipeServer');

	if Assigned(FServerThread) and not FServerThread.Finished then begin
		dbgMsg.Msg('Pipe server already running');
		Exit;
	end else begin
		FServerThread.Free;
		dbgMsg.Msg('Starting pipe server');
	end;

	FStopRequested := False;
	FServerThread := TThread.CreateAnonymousThread(
		procedure
		var
			hPipe : THandle;
			buffer : array [0 .. 4095] of Byte;
			bytesRead : DWORD;
			s : string;
			dbgMsg : TDebugMsgBeginEnd;
		begin
			dbgMsg := TDebugMsgBeginEnd.New('TVsCodeBridge.StartPipeServer Thread');

			while not FStopRequested do begin
				hPipe := CreateNamedPipe(PChar(PIPENAME), PIPE_ACCESS_INBOUND, PIPE_TYPE_BYTE or PIPE_READMODE_BYTE or PIPE_WAIT, 1, 4096,
					4096, 1000, nil); // 1 second timeout
				if hPipe = INVALID_HANDLE_VALUE then begin
					dbgMsg.Msg('Pipe could not be created!');
					Exit;
				end;
				dbgMsg.Msg('Pipe created, waiting for client...');

				// Wait for client with timeout
				if ConnectNamedPipe(hPipe, nil) or (GetLastError = ERROR_PIPE_CONNECTED) then begin
					if not FStopRequested then begin
						dbgMsg.Msg('Client connected');
						if ReadFile(hPipe, buffer, SizeOf(buffer), bytesRead, nil) and (bytesRead > 0) then begin
							SetString(s, PAnsiChar(@buffer), bytesRead);
							dbgMsg.Msg('Received: ' + s);
							// Process JSON data here
							parseJsonAndDoTheJob(s);
						end else begin
							dbgMsg.Msg('No data received from client');
						end;
					end;
				end else begin
					dbgMsg.Msg('Client connection failed or timeout');
				end;
				DisconnectNamedPipe(hPipe);
				CloseHandle(hPipe);
				dbgMsg.Msg('Pipe closed');
			end;
			dbgMsg.Msg('Pipe server stopped');
		end);
	FServerThread.Start;
end;

class procedure TVsCodeBridge.StopPipeServer();
var
	hClientPipe : THandle;
	bytesWritten : DWORD;
	stopCommand : AnsiString;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TVsCodeBridge.StopPipeServer');

	if not Assigned(FServerThread) or FServerThread.Finished then begin
		dbgMsg.Msg('Pipe server not running');
		Exit;
	end;

	dbgMsg.Msg('StopRequest set');
	FStopRequested := True;

	// Send a dummy command to unblock the pipe server
	dbgMsg.Msg('Sending stop command to unblock pipe...');
	try
		hClientPipe := CreateFile(PChar(PIPENAME), GENERIC_WRITE, 0, nil, OPEN_EXISTING, 0, 0);
		if hClientPipe <> INVALID_HANDLE_VALUE then begin
			stopCommand := '{"command":"stop"}';
			WriteFile(hClientPipe, stopCommand[1], Length(stopCommand), bytesWritten, nil);
			CloseHandle(hClientPipe);
			dbgMsg.Msg('Stop command sent');
		end else begin
			dbgMsg.Msg('Could not connect to pipe to send stop command');
		end;
	except
		on E : Exception do
			dbgMsg.Msg('Exception sending stop command: ' + E.Message);
	end;

	// Wait for thread to finish
	// FServerThread.WaitFor;
	FreeAndNil(FServerThread);
	dbgMsg.Msg('Pipe server stopped');
end;

end.
