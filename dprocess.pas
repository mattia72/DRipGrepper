{ Freepascal TProcess ported to Delphi

  License: FPC Modified LGPL (can use in commercial apps)

  Changes to the code marked with "L505" in comments }

{
  This file is part of the Free Component Library (FCL)
  Copyright (c) 1999-2000 by the Free Pascal development team

  See the file COPYING.FPC, included in this distribution,
  for details about the copyright.

  This program is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ********************************************************************** }

unit dprocess;

interface

uses
  classes,
  dpipes, // L505
  system.types, // L505
  sysutils;

type
  TProcessOption = (poRunSuspended, poWaitOnExit, poUsePipes, poStderrToOutPut,
    poNoConsole, poNewConsole, poDefaultErrorMode, poNewProcessGroup,
    poDebugProcess, poDebugOnlyThisProcess);

  TShowWindowOptions = (swoNone, swoHIDE, swoMaximize, swoMinimize, swoRestore,
    swoShow, swoShowDefault, swoShowMaximized, swoShowMinimized,
    swoshowMinNOActive, swoShowNA, swoShowNoActivate, swoShowNormal);

  TStartupOption = (suoUseShowWindow, suoUseSize, suoUsePosition,
    suoUseCountChars, suoUseFillAttribute);

  TProcessPriority = (ppHigh, ppIdle, ppNormal, ppRealTime);

  TProcessOptions = set of TProcessOption;
  TStartupOptions = set of TStartupOption;

type
{$IFDEF MACOS} // L505
  TProcessForkEvent = procedure(Sender: TObject) of object;
{$ENDIF}
  { TProcess }

  TProcess = class(TComponent)
  private
    FProcessOptions: TProcessOptions;
    FStartupOptions: TStartupOptions;
    FProcessID: Integer;
    // FTerminalProgram: String;
    FThreadID: Integer;
    FProcessHandle: Thandle;
    FThreadHandle: Thandle;
    FFillAttribute: Cardinal;
    FApplicationName: string;
    FConsoleTitle: string;
    FCommandLine: string;
    FCurrentDirectory: string;
    FDesktop: string;
    FEnvironment: Tstrings;
    FExecutable: string;
    FParameters: Tstrings;
    FShowWindow: TShowWindowOptions;
    FInherithandles: Boolean;
{$IFDEF MACOS} // L505
    FForkEvent: TProcessForkEvent;
{$ENDIF}
    FProcessPriority: TProcessPriority;
    dwXCountchars, dwXSize, dwYsize, dwx, dwYcountChars, dwy: Cardinal;
    FXTermProgram: string;
    FPipeBufferSize: Cardinal;
    procedure FreeStreams;
    function GetExitStatus: Integer;
    function GetExitCode: Integer;
    function GetRunning: Boolean;
    function GetWindowRect: TRect;
    procedure SetCommandLine(const AValue: string);
    procedure SetParameters(const AValue: Tstrings);
    procedure SetWindowRect(Value: TRect);
    procedure SetShowWindow(Value: TShowWindowOptions);
    procedure SetWindowColumns(Value: Cardinal);
    procedure SetWindowHeight(Value: Cardinal);
    procedure SetWindowLeft(Value: Cardinal);
    procedure SetWindowRows(Value: Cardinal);
    procedure SetWindowTop(Value: Cardinal);
    procedure SetWindowWidth(Value: Cardinal);
    procedure SetApplicationName(const Value: string);
    procedure SetProcessOptions(const Value: TProcessOptions);
    procedure SetActive(const Value: Boolean);
    procedure SetEnvironment(const Value: Tstrings);
    procedure ConvertCommandLine;
    function PeekExitStatus: Boolean;
  protected
    FRunning: Boolean;
    FExitCode: Cardinal;
    FInputStream: TOutputPipeStream;
    FOutputStream: TInputPipeStream;
    FStderrStream: TInputPipeStream;
    procedure CloseProcessHandles; virtual;
    procedure CreateStreams(InHandle, OutHandle, ErrHandle: Longint); virtual;
    procedure FreeStream(var AStream: THandleStream);
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Execute; virtual;
    procedure CloseInput; virtual;
    procedure CloseOutput; virtual;
    procedure CloseStderr; virtual;
    function Resume: Integer; virtual;
    function Suspend: Integer; virtual;
    function Terminate(AExitCode: Integer): Boolean; virtual;
    function WaitOnExit: Boolean;
    property WindowRect: TRect read GetWindowRect write SetWindowRect;
    property Handle: Thandle read FProcessHandle;
    property ProcessHandle: Thandle read FProcessHandle;
    property ThreadHandle: Thandle read FThreadHandle;
    property ProcessID: Integer read FProcessID;
    property ThreadID: Integer read FThreadID;
    property Input: TOutputPipeStream read FInputStream;
    property Output: TInputPipeStream read FOutputStream;
    property Stderr: TInputPipeStream read FStderrStream;
    property ExitStatus: Integer read GetExitStatus;
    property ExitCode: Integer read GetExitCode;
    property InheritHandles: Boolean read FInherithandles write FInherithandles;
{$IFDEF MACOS} // L505
    property OnForkEvent: TProcessForkEvent read FForkEvent write FForkEvent;
{$ENDIF}
  published
    property PipeBufferSize: Cardinal read FPipeBufferSize write FPipeBufferSize
      default 1024;
    property Active: Boolean read GetRunning write SetActive;
    property ApplicationName: string read FApplicationName
      write SetApplicationName; // deprecated; //L505
    property CommandLine: string read FCommandLine write SetCommandLine;
    // deprecated;  //L505
    property Executable: string read FExecutable write FExecutable;
    property Parameters: Tstrings read FParameters write SetParameters;
    property ConsoleTitle: string read FConsoleTitle write FConsoleTitle;
    property CurrentDirectory: string read FCurrentDirectory
      write FCurrentDirectory;
    property Desktop: string read FDesktop write FDesktop;
    property Environment: Tstrings read FEnvironment write SetEnvironment;
    property Options: TProcessOptions read FProcessOptions
      write SetProcessOptions;
    property Priority: TProcessPriority read FProcessPriority
      write FProcessPriority;
    property StartupOptions: TStartupOptions read FStartupOptions
      write FStartupOptions;
    property Running: Boolean read GetRunning;
    property ShowWindow: TShowWindowOptions read FShowWindow
      write SetShowWindow;
    property WindowColumns: Cardinal read dwXCountchars write SetWindowColumns;
    property WindowHeight: Cardinal read dwYsize write SetWindowHeight;
    property WindowLeft: Cardinal read dwx write SetWindowLeft;
    property WindowRows: Cardinal read dwYcountChars write SetWindowRows;
    property WindowTop: Cardinal read dwy write SetWindowTop;
    property WindowWidth: Cardinal read dwXSize write SetWindowWidth;
    property FillAttribute: Cardinal read FFillAttribute write FFillAttribute;
    property XTermProgram: string read FXTermProgram write FXTermProgram;
  end;

  EProcess = class(Exception);

procedure CommandToList(S: string; List: Tstrings);

{$IFDEF MACOS}

// L505
var
  TryTerminals: array of string;
  XTermProgram: string;
function DetectXTerm: string;
{$ENDIF}
{ // L505: changed to ansistring
  function RunCommandIndir(const curdir:string;const exename:string;const commands:array of string;out outputstring:string; out exitstatus:integer; Options : TProcessOptions = []):integer; overload; //L505
  function RunCommandIndir(const curdir:string;const exename:string;const commands:array of string;out outputstring:string; Options : TProcessOptions = []):boolean; overload; // L505
  function RunCommand(const exename:string;const commands:array of string;out outputstring:string; Options : TProcessOptions = []):boolean; overload;// L505


  function RunCommandInDir(const curdir,cmdline:string;out outputstring:string):boolean; deprecated; overload; // L505
  function RunCommand(const cmdline:string;out outputstring:string):boolean; deprecated; overload; // L505
}

function RunCommandIndir(const curdir: string; const exename: string; const
    commands: array of string; out outputstring: string; out ExitStatus:
    Integer; Options: TProcessOptions = []): Integer; overload;
// L505
function RunCommandIndir(const curdir:string;const exename:string;const
    commands:array of string;out outputstring:string; Options : TProcessOptions
    = []): boolean; overload;
function RunCommand(const exename:string;const commands:array of string;out
    outputstring:string; Options : TProcessOptions = []): boolean; overload;

function RunCommandIndir(const curdir,cmdline:string;out outputstring:string):
    boolean; overload; deprecated;
function RunCommand(const cmdline:string; out outputstring:string): boolean;
    overload; deprecated;

implementation

{$IFDEF MACOS} // L505
{$I process_macos.inc}
{$ENDIF}
{$IFDEF MSWINDOWS}  // L505
{$I process_win.inc}
{$ENDIF}

procedure CommandToList(S: string; List: Tstrings);

  function GetNextWord: string;

  const
    WhiteSpace = [' ', #9, #10, #13];
    Literals = ['"', ''''];

  var
    Wstart, wend: Integer;
    InLiteral: Boolean;
    LastLiteral: char;

  begin
    Wstart := 1;
    // L505 change "in" to CharInSet
    while (Wstart <= Length(S)) and (CharInSet(S[Wstart], WhiteSpace)) do
      Inc(Wstart);
    wend := Wstart;
    InLiteral := False;
    LastLiteral := #0;
    // L505
    // While (Wend<=Length(S)) and (Not (S[Wend] in WhiteSpace) or InLiteral) do
    while (wend <= Length(S)) and (not(CharInSet(S[wend], WhiteSpace)) or
      InLiteral) do begin
      // L505 changed "in" to CharInSet
      if CharInSet(S[wend], Literals) then
        if InLiteral then
          InLiteral := not(S[wend] = LastLiteral)
        else begin
          InLiteral := True;
          LastLiteral := S[wend];
        end;
      Inc(wend);
    end;

    Result := Copy(S, Wstart, wend - Wstart);
    // L505 changed "in" to CharInSet
    if (Length(Result) > 0) and (Result[1] = Result[Length(Result)])
    // if 1st char = last char and..
      and (CharInSet(Result[1], Literals)) then
      // it's one of the literals, then
      Result := Copy(Result, 2, Length(Result) - 2);
    // delete the 2 (but not others in it)
    // L505
    // While (WEnd<=Length(S)) and (S[Wend] in WhiteSpace) do
    while (wend <= Length(S)) and (CharInSet(S[wend], WhiteSpace)) do
      Inc(wend);
    Delete(S, 1, wend - 1);

  end;

var
  W: string;

begin
  while Length(S) > 0 do begin
    W := GetNextWord;
    if (W <> '') then
      List.Add(W);
  end;
end;

constructor TProcess.Create(AOwner: TComponent);
begin
  inherited;
  FProcessPriority := ppNormal;
  FShowWindow := swoNone;
  FInherithandles := True;
{$IFDEF MACOS} // L505
  FForkEvent := nil;
{$ENDIF}
  FPipeBufferSize := 1024;
  FEnvironment := TStringList.Create;
  FParameters := TStringList.Create;
end;

destructor TProcess.Destroy;
begin
  FParameters.Free;
  FEnvironment.Free;
  FreeStreams;
  CloseProcessHandles;
  inherited Destroy;
end;

procedure TProcess.FreeStreams;
begin
  if FStderrStream <> FOutputStream then
    FreeStream(THandleStream(FStderrStream));
  FreeStream(THandleStream(FOutputStream));
  FreeStream(THandleStream(FInputStream));
end;

function TProcess.GetExitStatus: Integer;
begin
  GetRunning;
  Result := FExitCode;
end;

{$IFNDEF OS_HASEXITCODE}

function TProcess.GetExitCode: Integer;
begin
  if not Running then
    Result := GetExitStatus
  else
    Result := 0
end;
{$ENDIF}

function TProcess.GetRunning: Boolean;
begin
  if FRunning then
    FRunning := not PeekExitStatus;
  Result := FRunning;
end;

procedure TProcess.CreateStreams(InHandle, OutHandle, ErrHandle: Longint);

begin
  FreeStreams;
  FInputStream := TOutputPipeStream.Create(InHandle);
  FOutputStream := TInputPipeStream.Create(OutHandle);
  if not(poStderrToOutPut in FProcessOptions) then
    FStderrStream := TInputPipeStream.Create(ErrHandle);
end;

procedure TProcess.FreeStream(var AStream: THandleStream);
begin
  if AStream = nil then
    exit;
  FreeAndNil(AStream);
end;

procedure TProcess.Loaded;
begin
  inherited Loaded;
  if (csDesigning in ComponentState) and (FCommandLine <> '') then
    ConvertCommandLine;
end;

procedure TProcess.CloseInput;
begin
  FreeStream(THandleStream(FInputStream));
end;

procedure TProcess.CloseOutput;
begin
  FreeStream(THandleStream(FOutputStream));
end;

procedure TProcess.CloseStderr;
begin
  FreeStream(THandleStream(FStderrStream));
end;

procedure TProcess.SetWindowColumns(Value: Cardinal);
begin
  if Value <> 0 then
    Include(FStartupOptions, suoUseCountChars);
  dwXCountchars := Value;
end;

procedure TProcess.SetWindowHeight(Value: Cardinal);
begin
  if Value <> 0 then
    Include(FStartupOptions, suoUsePosition);
  dwYsize := Value;
end;

procedure TProcess.SetWindowLeft(Value: Cardinal);
begin
  if Value <> 0 then
    Include(FStartupOptions, suoUseSize);
  dwx := Value;
end;

procedure TProcess.SetWindowTop(Value: Cardinal);
begin
  if Value <> 0 then
    Include(FStartupOptions, suoUsePosition);
  dwy := Value;
end;

procedure TProcess.SetWindowWidth(Value: Cardinal);
begin
  if (Value <> 0) then
    Include(FStartupOptions, suoUseSize);
  dwXSize := Value;
end;

function TProcess.GetWindowRect: TRect;
begin
  with Result do begin
    Left := dwx;
    Right := dwx + dwXSize;
    Top := dwy;
    Bottom := dwy + dwYsize;
  end;
end;

procedure TProcess.SetCommandLine(const AValue: string);
begin
  if FCommandLine = AValue then
    exit;
  FCommandLine := AValue;
  if not(csLoading in ComponentState) then
    ConvertCommandLine;
end;

procedure TProcess.SetParameters(const AValue: Tstrings);
begin
  FParameters.Assign(AValue);
end;

procedure TProcess.SetWindowRect(Value: TRect);
begin
  Include(FStartupOptions, suoUseSize);
  Include(FStartupOptions, suoUsePosition);
  with Value do begin
    dwx := Left;
    dwXSize := Right - Left;
    dwy := Top;
    dwYsize := Bottom - Top;
  end;
end;

procedure TProcess.SetWindowRows(Value: Cardinal);
begin
  if Value <> 0 then
    Include(FStartupOptions, suoUseCountChars);
  dwYcountChars := Value;
end;

procedure TProcess.SetApplicationName(const Value: string);
begin
  FApplicationName := Value;
  if (csDesigning in ComponentState) and (FCommandLine = '') then
    FCommandLine := Value;
end;

procedure TProcess.SetProcessOptions(const Value: TProcessOptions);
begin
  FProcessOptions := Value;
  if poNewConsole in FProcessOptions then
    Exclude(FProcessOptions, poNoConsole);
  if poRunSuspended in FProcessOptions then
    Exclude(FProcessOptions, poWaitOnExit);
end;

procedure TProcess.SetActive(const Value: Boolean);
begin
  if (Value <> GetRunning) then
    if Value then
      Execute
    else
      Terminate(0);
end;

procedure TProcess.SetEnvironment(const Value: Tstrings);
begin
  FEnvironment.Assign(Value);
end;

procedure TProcess.ConvertCommandLine;
begin
  FParameters.Clear;
  CommandToList(FCommandLine, FParameters);
  if FParameters.Count > 0 then begin
    Executable := FParameters[0];
    FParameters.Delete(0);
  end;
end;

const
  READ_BYTES = 65536;
  // not too small to avoid fragmentation when reading large files.

  // helperfunction that does the bulk of the work.
  // We need to also collect stderr output in order to avoid
  // lock out if the stderr pipe is full.
  // L505: changed to ansistring
function internalRuncommand(p: TProcess; out outputstring: string;
  out stderrstring: string; out ExitStatus: Integer): Integer;
// function internalRuncommand(p:TProcess;out outputstring: ansistring;
// out stderrstring: ansistring; out exitstatus:integer):integer;
var
  numbytes, bytesread, available: Integer;
  outputlength, stderrlength: Integer;
  stderrnumbytes, stderrbytesread: Integer;
begin
  // result:=-1;
  bytesread := 0;
  outputlength := 0;
  stderrbytesread := 0;
  stderrlength := 0;

  try
    try
      p.Options := p.Options + [poUsePipes];
      p.Execute;

      while p.Running do begin
        // Only call ReadFromStream if Data from corresponding stream
        // is already available, otherwise, on  linux, the read call
        // is blocking, and thus it is not possible to be sure to handle
        // big data amounts bboth on output and stderr pipes. PM.
        available := p.Output.NumBytesAvailable;
        // writeln('DEBUG: bytesavail: ', P.Output.NumBytesAvailable);
        if available > 0 then begin
          if (bytesread + available > outputlength) then begin
            outputlength := bytesread + READ_BYTES;
            Setlength(outputstring, outputlength);
          end;
          numbytes := p.Output.Read(outputstring[1 + bytesread], available);
          // L505 if in the future above string is unicodestring, above may need work NOTE: pchar is zero based . http://docwiki.embarcadero.com/RADStudio/Seattle/en/Using_Streams_to_Read_or_Write_Data
          // NumBytes := p.Output.Read(pchar(outputstring)[bytesread], available);

          if numbytes > 0 then
            Inc(bytesread, numbytes);
        end
        // The check for assigned(P.stderr) is mainly here so that
        // if we use poStderrToOutput in p.Options, we do not access invalid memory.
        else if assigned(p.Stderr) and (p.Stderr.NumBytesAvailable > 0) then
        begin
          available := p.Stderr.NumBytesAvailable;
          if (stderrbytesread + available > stderrlength) then begin
            stderrlength := stderrbytesread + READ_BYTES;
            Setlength(stderrstring, stderrlength);
          end;
          stderrnumbytes := p.Stderr.Read(stderrstring[1 + stderrbytesread],
            available);
          // L505 in the future if the above is a unicodestring this may need work, and NOTE: pchar is zero based
          // StderrNumBytes := p.StdErr.Read(pchar(stderrstring)[StderrBytesRead], available);

          if stderrnumbytes > 0 then
            Inc(stderrbytesread, stderrnumbytes);
        end
        else
          Sleep(100);
      end;
      // Get left output after end of execution
      available := p.Output.NumBytesAvailable;
      while available > 0 do begin
        if (bytesread + available > outputlength) then begin
          outputlength := bytesread + READ_BYTES;
          Setlength(outputstring, outputlength);
        end;
        numbytes := p.Output.Read(outputstring[1 + bytesread], available);
        // L505 if above is unicodestring in the future it may need work, and NOTE: pchar is zero based
        // NumBytes := p.Output.Read(pchar(outputstring)[bytesread], available);

        if numbytes > 0 then
          Inc(bytesread, numbytes);
        available := p.Output.NumBytesAvailable;
      end;
      Setlength(outputstring, bytesread);
      while assigned(p.Stderr) and (p.Stderr.NumBytesAvailable > 0) do begin
        available := p.Stderr.NumBytesAvailable;
        if (stderrbytesread + available > stderrlength) then begin
          stderrlength := stderrbytesread + READ_BYTES;
          Setlength(stderrstring, stderrlength);
        end;
        stderrnumbytes := p.Stderr.Read(stderrstring[1 + stderrbytesread],
          available);
        // L505 if above is unicodestring in the future, it may need work and NOTE: pchar is zero based
        // StderrNumBytes := p.StdErr.Read(pchar(stderrstring)[StderrBytesRead], available);

        if stderrnumbytes > 0 then
          Inc(stderrbytesread, stderrnumbytes);
      end;
      Setlength(stderrstring, stderrbytesread);
      ExitStatus := p.ExitStatus;
      Result := 0; // we came to here, document that.
    except
      on e: Exception do begin
        Result := 1;
        Setlength(outputstring, bytesread);
      end;
    end;
  finally
    p.Free;
  end;
end;

{ Functions without StderrString }

const
  ForbiddenOptions = [poRunSuspended, poWaitOnExit];

  // L505 changed to ansistring
function RunCommandIndir(const curdir: string; const exename: string;
  const commands: array of string; out outputstring: string;
  out ExitStatus: Integer; Options: TProcessOptions = []): Integer;
// function RunCommandIndir(const curdir: string; const exename: string; const commands:array of string; out outputstring: ansistring;out exitstatus:integer; Options : TProcessOptions = []):integer;
var
  p: TProcess;
  i: Integer;
  ErrorString: string; // L505
  // ErrorString: ansistring;
begin
  p := TProcess.Create(nil);
  if Options <> [] then
    p.Options := Options - ForbiddenOptions;
  p.Executable := exename;
  if curdir <> '' then
    p.CurrentDirectory := curdir;
  if high(commands) >= 0 then
    for i := low(commands) to high(commands) do
      p.Parameters.Add(commands[i]);
  Result := internalRuncommand(p, outputstring, ErrorString, ExitStatus);
end;

// L505 changed to ansistring
function RunCommandInDir(const curdir,cmdline:string;out outputstring:string):boolean; deprecated;
//function RunCommandIndir(const curdir, cmdline: string;
//  out outputstring: ansistring): Boolean; deprecated;
var
  p: TProcess;
  ExitStatus: Integer;
   ErrorString : String;  // L505
//  ErrorString: ansistring; // L505
begin
  p := TProcess.Create(nil);
  p.SetCommandLine(cmdline);
  if curdir <> '' then
    p.CurrentDirectory := curdir;
  Result := internalRuncommand(p, outputstring, ErrorString, ExitStatus) = 0;
  if ExitStatus <> 0 then
    Result := False;
end;

// L505 changed to ansistring
 function RunCommandIndir(const curdir:string;const exename:string;const commands:array of string;out outputstring:string; Options : TProcessOptions = []):boolean;
//function RunCommandIndir(const curdir: string; const exename: string;
//  const commands: array of string; out outputstring: ansistring;
//  Options: TProcessOptions = []): Boolean;
var
  p: TProcess;
  i, ExitStatus: Integer;
   ErrorString : String;  // L505
//  ErrorString: ansistring; // L505
begin
  p := TProcess.Create(nil);
  if Options <> [] then
    p.Options := Options - ForbiddenOptions;
  p.Executable := exename;
  if curdir <> '' then
    p.CurrentDirectory := curdir;
  if high(commands) >= 0 then
    for i := low(commands) to high(commands) do
      p.Parameters.Add(commands[i]);
  Result := internalRuncommand(p, outputstring, ErrorString, ExitStatus) = 0;
  if ExitStatus <> 0 then
    Result := False;
end;

// L505 changed to ansistring
 function RunCommand(const cmdline:string; out outputstring:string):boolean; deprecated;
//function RunCommand(const cmdline: string; out outputstring: ansistring)
//  : Boolean; deprecated;
var
  p: TProcess;
  ExitStatus: Integer;
   ErrorString : String; // L505
//  ErrorString: ansistring; // L505
begin
  p := TProcess.Create(nil);
  p.SetCommandLine(cmdline);
  Result := internalRuncommand(p, outputstring, ErrorString, ExitStatus) = 0;
  if ExitStatus <> 0 then
    Result := False;
end;

// L505: Changed to ansistring
 function RunCommand(const exename:string;const commands:array of string;out outputstring:string; Options : TProcessOptions = []):boolean;
//function RunCommand(const exename: string; const commands: array of string;
//  out outputstring: ansistring; Options: TProcessOptions = []): Boolean;
var
  p: TProcess;
  i, ExitStatus: Integer;
   ErrorString : String;  // L505
//  ErrorString: ansistring; // L505
begin
  p := TProcess.Create(nil);
  if Options <> [] then
    p.Options := Options - ForbiddenOptions;
  p.Executable := exename;
  if high(commands) >= 0 then
    for i := low(commands) to high(commands) do
      p.Parameters.Add(commands[i]);
  Result := internalRuncommand(p, outputstring, ErrorString, ExitStatus) = 0;
  if ExitStatus <> 0 then
    Result := False;
end;

end.
