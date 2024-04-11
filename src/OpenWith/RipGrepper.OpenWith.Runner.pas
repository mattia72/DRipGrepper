unit RipGrepper.OpenWith.Runner;

interface

uses
	ToolsAPI,
	System.Types,
	RipGrepper.OpenWith.SimpleTypes;

type
	TOpenWithRunner = class

		private
			class function BuildParams(const _owp: TOpenWithParams; const _sParams: string): string;
			class function GetEditPosition : IOTAEditPosition;
			class function GetParamsFromDelphiIde : TOpenWithParams;
			class function GetErrorText(dwErrorCode : DWORD) : string;

		public
			class procedure RunEditorCommand(const _sEditorCmd: string; const _owp: TOpenWithParams);
	end;

implementation

uses
	GX_OtaUtils,
	Winapi.Windows,
	System.SysUtils,
	Winapi.ShellAPI,
	Vcl.Dialogs,
	RipGrepper.Common.Constants, RipGrepper.Tools.DebugTools;

class function TOpenWithRunner.BuildParams(const _owp: TOpenWithParams; const _sParams: string): string;
var
	sCmdParams : string;
	owp : TOpenWithParams;
begin
	sCmdParams := _sParams;

	owp := GetParamsFromDelphiIde();
	if owp.IsEmpty then begin
		owp := _owp;
	end;

	sCmdParams := StringReplace(sCmdParams, '<DIR>', '%s', [rfReplaceAll]);
	sCmdParams := Format(sCmdParams, [owp.DirPath]);

	sCmdParams := StringReplace(sCmdParams, '<FILE>', '%s', [rfReplaceAll]);
	sCmdParams := Format(sCmdParams, [owp.FileName]);
	// TDebugUtils.DebugMessage((Format('TOpenWithRunner.InternalExecute Params: %s ', [sCmdParams])));

	sCmdParams := StringReplace(sCmdParams, '<LINE>', '%d', [rfReplaceAll]);
	sCmdParams := Format(sCmdParams, [owp.Row]);
	// TDebugUtils.DebugMessage((Format('TOpenWithRunner.InternalExecute Params: %s ', [sCmdParams])));

	sCmdParams := StringReplace(sCmdParams, '<COL>', '%d', [rfReplaceAll]);
	Result := Format(sCmdParams, [owp.Column]);

end;

class function TOpenWithRunner.GetEditPosition : IOTAEditPosition;
var
	aEditorServices : IOTAEditorServices;
	aEditBuffer : IOTAEditBuffer;
	aEditBlock : IOTAEditBlock;
begin
	Result := nil;
	aEditorServices := BorlandIDEServices as IOTAEditorServices;
	if Assigned(aEditorServices) and Assigned(aEditorServices.TopView) then begin
		aEditBlock := aEditorServices.TopView.GetBlock;
		aEditBuffer := aEditorServices.TopView.GetBuffer;
		if Assigned(aEditBlock) and Assigned(aEditBuffer) then begin
			Result := aEditBuffer.EditPosition;
		end;

	end;
end;

class function TOpenWithRunner.GetParamsFromDelphiIde() : TOpenWithParams;
var
	sProjName : string;
	editPosition : IOTAEditPosition;
begin
	editPosition := GetEditPosition;
	if Assigned(editPosition) then begin
		Result.FileName := GxOtaGetCurrentSourceFile;;
		sProjName := GxOtaGetCurrentProjectName;
		TDebugUtils.DebugMessage((Format('TOpenWithRunner.InternalExecute proj: %s ', [sProjName])));
		if (sProjName <> '') then begin
			Result.DirPath := ExtractFileDir(sProjName);
		end else begin
			Result.DirPath := ExtractFileDir(Result.FileName);
		end;
		Result.Row := editPosition.Row;
		Result.Column := editPosition.Column;
		Result.IsEmpty := False;
	end;
end;

class procedure TOpenWithRunner.RunEditorCommand(const _sEditorCmd: string; const _owp: TOpenWithParams);
var
	iPos : Integer;
	err : DWORD;
	sParams : string;
begin

	iPos := Pos('.EXE', AnsiUppercase(_sEditorCmd));
	sParams := Copy(_sEditorCmd, iPos + 4, MaxInt);
	var
	sCmd := '"' + Copy(_sEditorCmd, 1, iPos + 3) + '"';
	// TDebugUtils.DebugMessage((Format('TOpenWithRunner.InternalExecute Editor path: %s ', [_sEditorCmd])));

	sParams := BuildParams(_owp, sParams);

	TDebugUtils.DebugMessage((Format('TOpenWithRunner.InternalExecute cmd: %s %s ', [_sEditorCmd, sParams])));
	ShellExecute(0, 'open', PChar(sCmd), PChar(sParams), nil, SW_SHOW);
	err := GetLastError;
	if err <> 0 then begin
		MessageDlg(Format('%s %s' + CRLF + CRLF + '%s', [sCmd, sParams, GetErrorText(err)]), mtError, [mbOK], 0);
	end;
end;

class function TOpenWithRunner.GetErrorText(dwErrorCode : DWORD) : string;
var
	sFehler : string;
	aMessageBuffer : array [0 .. 255] of Char;
begin
	sFehler := 'Fehler ' + IntToStr(dwErrorCode) + ' bei Dateizugriff!' + CRLF;
	case dwErrorCode of
		ERROR_FILE_NOT_FOUND :
		sFehler := sFehler + 'Datei nicht gefunden!'; // 2
		ERROR_PATH_NOT_FOUND :
		sFehler := sFehler + 'Pfad nicht gefunden!'; // 3
		ERROR_TOO_MANY_OPEN_FILES :
		sFehler := sFehler + 'Zu viele offene Dateien!';
		ERROR_ACCESS_DENIED :
		sFehler := sFehler + 'Zugriff wurde verweigert!';
		ERROR_INVALID_HANDLE :
		sFehler := sFehler + 'Falscher Dateihandle!';
		ERROR_NOT_ENOUGH_MEMORY :
		sFehler := sFehler + 'Speicher ist voll!';
		ERROR_INVALID_BLOCK :
		sFehler := sFehler + 'Falscher Block!';
		ERROR_BAD_ENVIRONMENT :
		sFehler := sFehler + 'Umgebungsvariable fehlerhaft!';
		ERROR_OUTOFMEMORY :
		sFehler := sFehler + 'Speicher ist voll!';
		ERROR_INVALID_DRIVE :
		sFehler := sFehler + 'Laufwerk wurde nicht gefunden!';
		ERROR_WRITE_PROTECT :
		sFehler := sFehler + 'Schreibschutz!';
		ERROR_NOT_READY :
		sFehler := sFehler + 'Das Gerät ist nicht bereit!';
		ERROR_CRC :
		sFehler := sFehler + 'Datenfehler!';
		ERROR_SEEK :
		sFehler := sFehler + 'Zugriffsfehler!';
		ERROR_NOT_DOS_DISK :
		sFehler := sFehler + 'Falsche/Fehlende Formatierung!';
		ERROR_SECTOR_NOT_FOUND :
		sFehler := sFehler + 'Sektor wurde nicht gefunden!';
		ERROR_WRITE_FAULT :
		sFehler := sFehler + 'Allgemeiner Schreibfehler!';
		ERROR_READ_FAULT :
		sFehler := sFehler + 'Allgemeiner Lesefehler!';
		ERROR_GEN_FAILURE :
		sFehler := sFehler + 'Gerät funktioniert nicht!';
		ERROR_SHARING_VIOLATION :
		sFehler := sFehler + 'Datei ist in Benutzung!';
		ERROR_HANDLE_DISK_FULL :
		sFehler := sFehler + 'Datenträger voll!';
		ERROR_BAD_NETPATH :
		sFehler := sFehler + 'Netzwerkpfad nicht gefunden!';
		ERROR_NETWORK_BUSY :
		sFehler := sFehler + 'Netzwerk ist ausgelastet!';
		ERROR_DEV_NOT_EXIST :
		sFehler := sFehler + 'Netzwerkbereich nicht verfügbar!';
		ERROR_FILE_EXISTS :
		sFehler := sFehler + 'Datei bereits vorhanden!';
		ERROR_CANNOT_MAKE :
		sFehler := sFehler + 'Datei konnte nicht angelegt werden!';
		ERROR_DISK_CHANGE :
		sFehler := sFehler + 'Datenträger nicht eingelegt!';
		ERROR_DRIVE_LOCKED :
		sFehler := sFehler + 'Datenträger ist gesprerrt!';
		ERROR_OPEN_FAILED :
		sFehler := sFehler + 'Datei konnte nicht geöffnet werden!';
		ERROR_BUFFER_OVERFLOW :
		sFehler := sFehler + 'Dateiname ist zu lang!';
		ERROR_DISK_FULL :
		sFehler := sFehler + 'Datenträger voll!';
		ERROR_NO_MORE_SEARCH_HANDLES :
		sFehler := sFehler + 'Keine Dateihandles frei!';
		ERROR_INVALID_TARGET_HANDLE :
		sFehler := sFehler + 'Falscher Dateihandle!';
		ERROR_BAD_DRIVER_LEVEL :
		sFehler := sFehler + 'Falscher/Fehlerhafter Gerätetreiber!';
		ERROR_INSUFFICIENT_BUFFER :
		sFehler := sFehler + 'Der Lesepuffer ist zu klein!';
		ERROR_INVALID_NAME :
		sFehler := sFehler + 'Falscher Dateiname!';
		else begin // IK 21.10.04 sonstiges aus dem Betriebssystem
			FormatMessage($1000, // FORMAT_MESSAGE_FROM_SYSTEM
				nil, dwErrorCode, 0, Addr(aMessageBuffer), 255, // 255 = max size of message buffer
				nil);
			sFehler := sFehler + aMessageBuffer;
		end;
	end;
	Result := sFehler;
end;

end.
