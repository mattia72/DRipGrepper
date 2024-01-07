unit ProcessTools;

interface

uses
  System.Classes;

type
  INewLineEventHandler = interface
    procedure OnNewResultLine(const _sLine: AnsiString);
  end;

 TNewLineEventHandler = procedure(_obj : INewLineEventHandler; const _s: AnsiString);

type
  TProcessUtils = class(TObject)
  private
    class var FOnNewLineEvent: TNewLineEventHandler;
  protected
    class procedure NewLineEventHandler(_obj : INewLineEventHandler; const _s: AnsiString);
  public
    class function RunProcess(const _exe: string; _args: TStrings; _handler:
        INewLineEventHandler; _onNewLine: TNewLineEventHandler): Boolean;
    class property OnNewLineEvent: TNewLineEventHandler read FOnNewLineEvent
      write FOnNewLineEvent;
  end;


implementation

uses
  dprocess, System.SysUtils, DebugTools;

class procedure TProcessUtils.NewLineEventHandler(_obj : INewLineEventHandler;
    const _s: AnsiString);
begin
  if Assigned(FOnNewLineEvent) then
  begin
    FOnNewLineEvent(_obj, _s);
  end;
end;

class function TProcessUtils.RunProcess(const _exe: string; _args: TStrings;
    _handler: INewLineEventHandler; _onNewLine: TNewLineEventHandler): Boolean;
const
  BufSize = 1024;
var
  p: TProcess;
  // Buf: string;
  Buf: AnsiString;
  Count: integer;
  i: integer;
  LineStart: integer;
  // OutputLine: string;
  OutputLine: AnsiString;
begin
  p := TProcess.Create(nil);
  try
    p.Executable := _exe;

    p.Options := [poUsePipes, poStdErrToOutPut];
    // p.CurrentDirectory := ExtractFilePath(p.Executable);
    p.ShowWindow := swoShowNormal; // Is this needed?

    p.Parameters.Assign(_args);
    TDebugUtils.DebugMessage('Running command ' + p.Executable +
      ' with arguments: ' + p.Parameters.Text);
    p.Execute;

    { Now process the output }
    OutputLine := '';
    SetLength(Buf, BufSize);
    repeat
      if (p.Output <> nil) then
      begin
        Count := p.Output.Read(Buf[1], Length(Buf));
        // Count:=p.Output.Read(pchar(Buf)^, BufSize);  //L505 todo: try this when using unicodestring buffer
        // writeln('DEBUG: len buf: ', length(buf));
      end
      else
      begin
        Count := 0;
      end;
      LineStart := 1;
      i := 1;
      while i <= Count do
      begin
        // L505
        // if Buf[i] in [#10,#13] then
        if CharInSet(Buf[i], [#10, #13]) then
        begin
          OutputLine := OutputLine + Copy(Buf, LineStart, i - LineStart);
          NewLineEventHandler(_handler, OutputLine);
          OutputLine := '';
          // L505
          // if (i<Count) and (Buf[i+1] in [#10,#13]) and (Buf[i]<>Buf[i+1]) then
          if (i < Count) and (CharInSet(Buf[i], [#10, #13])) and
            (Buf[i] <> Buf[i + 1]) then
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
    if not Result then
      NewLineEventHandler(_handler, AnsiString('Command ' + p.Executable +
        ' failed with exit code: ' + p.ExitStatus.ToString));
  finally
    FreeAndNil(p);
  end;
end;


end.
