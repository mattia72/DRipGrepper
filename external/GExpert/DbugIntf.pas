(*
 * GExperts Debug Window Interface
 * http://www.gexperts.org
 *
 * You are free to use this code in any application to send commands to the
 * GExperts debug window.  This includes usage in commercial, shareware,
 * freeware, public domain, and other applications.
 *)
 
 // The following IFNDEF allows to include this unit via {$I 'dbugintf.pas'} in GExperts,
 // unit source\utils\GX_DbugIntf.pas, so the source code is not duplicated.
 // If you use DbugIntf.pas anywhere else, you won't need it.
{$IFNDEF GX_DBUGINTF}
unit DbugIntf;
{$ENDIF}

interface

uses
  Windows, Dialogs; // We need "Dialogs" for TMsgDlgType

procedure SendBoolean(const Identifier: string; const Value: Boolean);
procedure SendDateTime(const Identifier: string; const Value: TDateTime);
procedure SendDebugEx(const Msg: string; MType: TMsgDlgType);
procedure SendDebug(const Msg: string);
procedure SendDebugError(const Msg: string);
procedure SendDebugWarning(const Msg: string);
procedure SendDebugClear;
procedure SendInteger(const Identifier: string; const Value: Integer);
procedure SendMethodEnter(const MethodName: string);
procedure SendMethodExit(const MethodName: string);
procedure SendIndent;
procedure SendUnIndent;
procedure SendSeparator;
procedure SendDebugFmt(const Msg: string; const Args: array of const);
procedure SendDebugFmtEx(const Msg: string; const Args: array of const; MType: TMsgDlgType);
function StartDebugWin: hWnd;
procedure SendDebugPause;
procedure SendDebugResume;

implementation

uses
  Messages,
  SysUtils,
  Registry,
{$IFDEF GX_DEBUGLOG}
  GX_Debug,
{$ENDIF GX_DEBUGLOG}
  Forms; // We need "Forms" for the Application object

threadvar
  MsgPrefix: string;

const
  chrStringCommand: AnsiChar = {$IFDEF UNICODE} #4 {$ELSE} #1 {$ENDIF};
  chrSQLCommand: AnsiChar = #2; // Old, unused type
  chrClearCommand: AnsiChar = #3;
  chrNull: AnsiChar = #0;

var
  PastFailedAttemptToStartDebugWin: Boolean = False;
  SendPaused: Boolean = False;

function StartDebugWin: hWnd;
var
  DebugFileName: string;
  Buf: array[0..MAX_PATH + 1] of Char;
  si: TStartupInfo;
  pi: TProcessInformation;
begin
  MsgPrefix := '';

  Result := 0;
  if PastFailedAttemptToStartDebugWin then
    Exit;

  with TRegIniFile.Create('\Software\GExperts') do // Do not localize.
  try
    DebugFileName := ReadString('Debug', 'FilePath', ''); // Do not localize.
  finally
    Free;
  end;

  if Trim(DebugFileName) = '' then
  begin
    GetModuleFileName(HINSTANCE, Buf, SizeOf(Buf)-1);
    DebugFileName := ExtractFilePath(StrPas(Buf)) + 'GExpertsDebugWindow.exe'; // Do not localize.
  end;

  if (Trim(DebugFileName) = '') or not FileExists(DebugFileName) then
  begin
    PastFailedAttemptToStartDebugWin := True;
    Exit;
  end;

  FillChar(si, SizeOf(si), #0);
  si.cb := SizeOf(si);
  si.dwFlags := STARTF_USESHOWWINDOW;
  si.wShowWindow := SW_SHOW;
  if not CreateProcess(PChar(DebugFileName), nil, nil, nil,
                       False, 0, nil, nil, si, pi) then
  begin
    PastFailedAttemptToStartDebugWin := True;
    Exit;
  end;

  try
    WaitForInputIdle(pi.hProcess, 3 * 1000); // wait for 3 seconds to get idle
  finally
    CloseHandle(pi.hThread);
    CloseHandle(pi.hProcess);
  end;

  Result := FindWindow('TfmDebug', nil);
end;

procedure SendDebugEx(const Msg: string; MType: TMsgDlgType);
var
  CDS: TCopyDataStruct;
  DebugWin: hWnd;
  MessageString: string;
  MsgBytes: array of Byte;
  MsgType: AnsiChar;
  ByteIndex: Integer;
{$IFDEF GX_DEBUGLOG}
  {$DEFINE NEEDMTYPESTR}
{$ENDIF GX_DEBUGLOG}

  procedure AddByte(B: Byte);
  begin
    MsgBytes[ByteIndex] := B;
    Inc(ByteIndex);
  end;

  procedure AddStringBytes(const Str: AnsiString); overload;
  var
    i: Integer;
  begin
    for i := 0 to ((Length(Str)) * SizeOf(AnsiChar)) - 1 do
      AddByte(Byte(Str[i+1]));
  end;

{$IFDEF UNICODE}
  procedure AddStringBytes(const Str: string); overload;
  begin
    AddStringBytes(AnsiString(Str));
  end;
{$ENDIF}

{$IFDEF NEEDMTYPESTR}
const
  MTypeStr: array[TMsgDlgType] of string =
    ('Warning: ', 'Error: ', 'Information: ', 'Confirmation: ', 'Custom: ');
{$ENDIF NEEDMTYPESTR}
begin
  if SendPaused then
    Exit;

{$IFDEF GX_DEBUGLOG}
  GxAddToDebugLog(MTypeStr[MType] + Msg);
{$ENDIF GX_DEBUGLOG}
{$IFDEF MSWINDOWS}
  DebugWin := FindWindow('TfmDebug', nil);

  if DebugWin = 0 then
    DebugWin := StartDebugWin;

  if DebugWin <> 0 then
  begin
    ByteIndex := 0;
    MessageString := MsgPrefix + Msg;
    SetLength(MsgBytes, 1 + 1 + (Length(MessageString)* SizeOf(Char)) + 1); // Payload, type, message, null
    CDS.cbData := Length(MsgBytes);
    CDS.dwData := 0;
    MsgType := AnsiChar(Ord(MType) + 1);
    if Msg = string(chrClearCommand) then
      AddByte(Byte(chrClearCommand))
    else
      AddByte(Byte(chrStringCommand));
    AddByte(Byte(MsgType));
    AddStringBytes(MessageString);
    AddByte(Byte(chrNull));
    CDS.lpData := Pointer(MsgBytes);
    SendMessage(DebugWin, WM_COPYDATA, WPARAM(Application.Handle), LPARAM(@CDS));
  end;
{$ENDIF MSWINDOWS}
end;

procedure SendDebug(const Msg: string);
begin
  SendDebugEx(Msg, mtInformation);
end;

procedure SendDebugError(const Msg: string);
begin
  SendDebugEx(Msg, mtError);
end;

procedure SendDebugWarning(const Msg: string);
begin
  SendDebugEx(Msg, mtWarning);
end;

procedure SendDebugFmt(const Msg: string; const Args: array of const);
begin
  SendDebugEx(Format(Msg, Args), mtInformation);
end;

procedure SendDebugFmtEx(const Msg: string; const Args: array of const; MType: TMsgDlgType);
begin
  SendDebugEx(Format(Msg, Args), MType);
end;

procedure SendDebugClear;
begin
  SendDebug(string(chrClearCommand));
end;

const
  Indentation = '    ';

procedure SendMethodEnter(const MethodName: string);
begin
  SendDebugEx('Entering ' + MethodName, mtInformation);
  SendIndent;
end;

procedure SendIndent;
begin
  MsgPrefix := MsgPrefix + Indentation;
end;

procedure SendUnIndent;
begin
  Delete(MsgPrefix, 1, Length(Indentation));
end;

procedure SendMethodExit(const MethodName: string);
begin
  SendUnindent;
  SendDebugEx('Exiting ' + MethodName, mtInformation);
end;

procedure SendSeparator;
const
  SeparatorString = '------------------------------';
begin
  SendDebugEx(SeparatorString, mtInformation);
end;

procedure SendBoolean(const Identifier: string; const Value: Boolean);
begin
  // Note: We deliberately leave "True" and "False" as
  // hard-coded string constants, since these are
  // technical terminology which should not be localised.
  if Value then
    SendDebugEx(Identifier + ' = True', mtInformation)
  else
    SendDebugEx(Identifier + ' = False', mtInformation);
end;

procedure SendInteger(const Identifier: string; const Value: Integer);
begin
  SendDebugEx(Format('%s = %d', [Identifier, Value]), mtInformation);
end;

procedure SendDateTime(const Identifier: string; const Value: TDateTime);
begin
  SendDebugEx(Identifier + ' = ' + DateTimeToStr(Value), mtInformation);
end;

procedure SendDebugPause;
begin
  SendPaused := True;
end;

procedure SendDebugResume;
begin
  SendPaused := False;
end;

end.

