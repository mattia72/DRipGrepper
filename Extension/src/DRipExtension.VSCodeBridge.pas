unit DRipExtension.VsCodeBridge;

interface

type
	TVsCodeBridge = class
		const
			PIPENAME = '\\.\pipe\vscode_delphi_bridge';

		private
			class procedure parseJsonAndDoTheJob(const s : string);

		public
			// Checks if the IDE window is active (dummy implementation)
			class function IsWindowActive() : Boolean;
			class procedure StartPipeServer();
	end;

implementation

uses
	Windows,
	SysUtils,
	Classes,
	Dialogs,
	ShellApi,
	ActiveX,
	ComObj,
	JSON,
	RipGrepper.Tools.DebugUtils,
	RipGrepper.Common.IOTAUtils;

class function TVsCodeBridge.IsWindowActive : Boolean;
begin
	// TODO: Check if Delphi window is foreground
	Result := True; // Always active (dummy)
end;

class procedure TVsCodeBridge.parseJsonAndDoTheJob(const s : string);
var
	jsonObj : TJSONObject;
	filePath : string;
	line : Integer;
	column : Integer;
	command: string;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TVsCodeBridge.ListenToPipe');

	jsonObj := TJSONObject.ParseJSONValue(s) as TJSONObject;
	if Assigned(jsonObj) then begin
		dbgMsg.Msg('JSON parsed: ' + s);
		try
			filePath := jsonObj.GetValue('filePath').Value;
			line := StrToIntDef(jsonObj.GetValue('line').Value, 1);
			column := StrToIntDef(jsonObj.GetValue('column').Value, 1);
            command := jsonObj.GetValue('command').Value;
            if command = 'gotoFileLocation' then

			TThread.Synchronize(nil,
				procedure
				begin
					var
					dbgMsg := TDebugMsgBeginEnd.New('TThread.Synchronize');
					dbgMsg.Msg('Opening file: ' + filePath + ' at line ' + IntToStr(line) + ', column ' + IntToStr(column));
					IOTAUtils.GxOtaGoToFileLineColumn(filePath, line, column, column - 1);
				end);
		finally
			jsonObj.Free;
		end;
	end else begin
		dbgMsg.Msg('JSON parse failed');
	end;
end;

class procedure TVsCodeBridge.StartPipeServer();
var hPipe : THandle; buffer : array [0 .. 4095] of Byte; bytesRead : DWORD;
	s : string; dbgMsg : TDebugMsgBeginEnd;
begin
	dbgMsg := TDebugMsgBeginEnd.New('TVsCodeBridge.StartPipeServer');
	dbgMsg.Msg('Pipe server started');
	while True do begin
		hPipe := CreateNamedPipe(PChar(PIPENAME), PIPE_ACCESS_INBOUND, PIPE_TYPE_BYTE or PIPE_READMODE_BYTE or PIPE_WAIT, 1, 4096,
			4096, 0, nil);
		if hPipe = INVALID_HANDLE_VALUE then begin
			dbgMsg.Msg('Pipe could not be created!');
			ShowMessage('Pipe konnte nicht erstellt werden!');
			Exit;
		end;
		dbgMsg.Msg('Pipe created, waiting for client...');
		// Wait for client (Node.js)
		if ConnectNamedPipe(hPipe, nil) then begin
			dbgMsg.Msg('Client connected');
			if ReadFile(hPipe, buffer, SizeOf(buffer), bytesRead, nil) and (bytesRead > 0) then begin
				SetString(s, PAnsiChar(@buffer), bytesRead);
				dbgMsg.Msg('Received: ' + s);
				// Process JSON data here
				parseJsonAndDoTheJob(s);
			end else begin
				dbgMsg.Msg('No data received from client');
			end;
		end else begin
			dbgMsg.Msg('Client connection failed');
		end;
		DisconnectNamedPipe(hPipe);
		CloseHandle(hPipe);
		dbgMsg.Msg('Pipe closed');
	end;
end;

end.
