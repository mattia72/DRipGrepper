unit GX_EditReader;

{$I GX_CondDefine.inc}

interface

uses
  Classes, ToolsAPI;

type
  TModuleMode = (mmModule, mmFile);

type
  TEditReader = class(TObject)
  private
    FSourceInterfaceAllocated: Boolean;
    FModuleNotifier: IOTAModuleNotifier;
    FEditIntf: IOTASourceEditor;
    FEditRead: IOTAEditReader;
    FModIntf: IOTAModule;
    FNotifierIndex: Integer;

    Buf: PAnsiChar;
    FBufSize: Integer;
    FFileName: string;
//    FIsUTF8: Boolean;
    FMode: TModuleMode;
    SFile: TStream;
    procedure AllocateFileData;
    function GetLineCount: Integer;
    procedure SetBufSize(New: Integer);
    procedure InternalGotoLine(Line: Integer; Offset: Boolean);
    function ReadTextFromStream(Stream: TMemoryStream): string;
  protected
    procedure SetFileName(const Value: string);
    procedure ReleaseModuleNotifier;
    procedure SaveToStreamFromPos(Stream: TStream);
    procedure SaveToStreamToPos(Stream: TStream);
    procedure SaveToStream(Stream: TStream);
  public
    class function GetText(const FileName: string): string; overload;
    constructor Create(const FileName: string);
    destructor Destroy; override;
    procedure FreeFileData;
    procedure Reset;
    procedure GotoLine(L: Integer);
    procedure GotoOffsetLine(L: Integer);
    procedure ShowSource;
    procedure ShowForm;
    function GetText: string; overload;
    function GetTextFromPos: string;
    function GetTextToPos: string;
    ///<summary>
    /// Returns the cursor position as offset into the editor buffer,
    /// that is into a possibly UTF-8 encoded buffer without expanding tabs </summary>
    function GetCurrentBufferPos: Integer;
    ///<symmary>
    /// Returns the cursor position in charcacter coordinates, that is without expanding tabs </summary>
    function GetCurrentCharPos: TOTACharPos;
    procedure UnfoldCode;
    property BufSize: Integer read FBufSize write SetBufSize;
    property FileName: string read FFileName write SetFileName;
    property LineCount: Integer read GetLineCount;
    property Mode: TModuleMode read FMode;
//    property IsUTF8: Boolean read FIsUTF8;
  end;

implementation

uses
  {$IFOPT D+} GX_DbugIntf, {$ENDIF}
  SysUtils,
  {$IFDEF GX_VER250_up} AnsiStrings, {$ENDIF GX_VER250_up}
  GX_GenericUtils, GX_OtaUtils, GX_IdeUtils, Math;

type
  TModuleFreeNotifier = class(TNotifierObject, IOTAModuleNotifier)
  private
    FOwner: TEditReader;
  public
    constructor Create(Owner: TEditReader);
    destructor Destroy; override;
    procedure ModuleRenamed(const NewName: string);
    function CheckOverwrite: Boolean;
  end;

{ TModuleFreeNotifier }

function TModuleFreeNotifier.CheckOverwrite: Boolean;
begin
  Result := True;
end;

constructor TModuleFreeNotifier.Create(Owner: TEditReader);
begin
  inherited Create;
  FOwner := Owner;
end;

destructor TModuleFreeNotifier.Destroy;
begin
  Assert(FOwner <> nil);
  FOwner.FreeFileData;
  FOwner.FModuleNotifier := nil;
  inherited;
end;

procedure TModuleFreeNotifier.ModuleRenamed(const NewName: string);
begin
  // We might want to handle this and change the stored file name
end;

resourcestring
  SNoEditReader = 'FEditRead: No editor reader interface (you have found a bug!)';

constructor TEditReader.Create(const FileName: string);
begin
  inherited Create;

  FBufSize := 32760; // Large buffers are faster, but too large causes crashes
  FNotifierIndex := InvalidNotifierIndex;
  FMode := mmModule;
  if FileName <> '' then
    SetFileName(FileName);
end;

// Use the FreeFileData to release the references
// to the internal editor buffer or the external
// file on disk in order not to block the file
// or track the editor (which may disappear) for
// possibly extended periods of time.
//
// Calls to edit reader will always (re-)allocate
// references again by calling the "AllocateFileData"
// method, so calling "FreeFileData" essentially comes
// for free, only reducing the length a reference
// is held to an entity.
procedure TEditReader.FreeFileData;
begin
  FreeAndNil(SFile);
  FEditRead := nil;
  FEditIntf := nil;

  ReleaseModuleNotifier;
  FModIntf := nil;

  FSourceInterfaceAllocated := False;
end;

destructor TEditReader.Destroy;
begin
  FreeFileData;

{$IFDEF GX_VER250_up}AnsiStrings.{$ENDIF}StrDispose(Buf);
  Buf := nil;

  inherited Destroy;
end;

procedure TEditReader.AllocateFileData;
resourcestring
  SFileDoesNotExist = 'File %s does not exist';
  SNoEditorInterface = 'FEditRead: No editor interface';
  SNoModuleNotifier = 'TEditReader: Could not get module notifier';

  procedure AllocateFromDisk;
  var
    UnicodeBOM: Integer;
    Bytes: Integer;
    FileStream: TFileStream;
  begin
    if not FileExists(FFileName) then
      raise Exception.CreateFmt(SFileDoesNotExist, [FFileName]);

    FMode := mmFile;

    FileStream := TFileStream.Create(FFileName, fmOpenRead or fmShareDenyWrite);
    try
      Bytes := FileStream.Read(UnicodeBOM, 3);
      // This does not support other encodings such as UCS-2, UCS-4, etc.
      // but according to a poll I did on Google+, nobody uses these anyway.
      // So we just support ANSI and UTF-8 and hope for the best.
      // -- 2015-06-10 twm
      if (Bytes = 3) and ((UnicodeBOM and $00FFFFFF) = $00BFBBEF) then
//        FIsUTF8 := true
      else
        FileStream.Seek(0, soFromBeginning);
      SFile := TMemoryStream.Create;
      SFile.CopyFrom(FileStream, FileStream.Size - FileStream.Position);
    finally
      FileStream.Free;
    end;
  end;

begin
  if FSourceInterfaceAllocated then
    Exit;

  if BorlandIDEServices = nil then
  begin
    AllocateFromDisk;
    Exit;
  end;

  // Get module interface
  Assert(FModIntf = nil);
  FModIntf := GxOtaGetModule(FFileName);
  if FModIntf = nil then
  begin
    {$IFOPT D+} SendDebug('EditReader: Module not open in the IDE - opening "' + FFilename + '" from disk'); {$ENDIF}
    AllocateFromDisk;
  end
  else
  begin
    FMode := mmModule;
    {$IFOPT D+} SendDebug('EditReader: Got module for ' + FFileName); {$ENDIF}
//    FIsUTF8 := RunningDelphi8OrGreater; // Delphi 8+ convert all edit buffers to UTF-8

    // Allocate notifier for module
    Assert(FModuleNotifier = nil);
    FModuleNotifier := TModuleFreeNotifier.Create(Self);
    {$IFOPT D+} SendDebug('EditReader: Got FModuleNotifier'); {$ENDIF}
    if FModuleNotifier = nil then
    begin
      FModIntf := nil;

      raise Exception.Create(SNoModuleNotifier);
    end;
    FNotifierIndex := FModIntf.AddNotifier(FModuleNotifier);

    // Get Editor Interface
    Assert(FEditIntf = nil);

    FEditIntf := GxOtaGetSourceEditorFromModule(FModIntf, FFileName);
    {$IFOPT D+} SendDebug('EditReader: Got FEditIntf for module'); {$ENDIF}
    if FEditIntf = nil then
    begin
      ReleaseModuleNotifier;
      FModIntf := nil;
      if FileExists(FFileName) then
        AllocateFromDisk
      else
        raise Exception.Create(SNoEditorInterface);
      // Should we call FreeFileData?
      Exit;
    end;

    // Get Reader interface }
    Assert(FEditRead = nil);
    FEditRead := FEditIntf.CreateReader;
    if FEditRead = nil then
    begin
      ReleaseModuleNotifier;
      FModIntf := nil;
      FEditIntf := nil;

      raise Exception.Create(SNoEditReader);
    end;
  end;

  FSourceInterfaceAllocated := True;
end;

procedure TEditReader.SetFileName(const Value: string);
begin
  if SameText(Value, FFileName) then
    Exit;

  FreeFileData;

  // Assigning an empty string clears allocation.
  if Value = '' then
    Exit;

  FFileName := Value;
  Reset;
end;

procedure TEditReader.SetBufSize(New: Integer);
begin
  if (Buf = nil) and (New <> FBufSize) then
    FBufSize := New;
  // 32K is the max we can read from an edit reader at once
  Assert(FBufSize <= 1024 * 32);
end;

procedure TEditReader.ShowSource;
begin
  AllocateFileData;

  Assert(Assigned(FEditIntf));

  if FMode = mmModule then
  begin
    if RunningDelphi8OrGreater then
    begin // This prevents .aspx editor flickering in Delphi 2005+
      if (GxOtaGetTopMostEditBufferFileName <> FFileName) then
        FEditIntf.Show;
    end
    else
      FEditIntf.Show;
  end;
end;

procedure TEditReader.UnfoldCode;
{$IFDEF GX_DELPHI2005_UP}
var
  EditView: IOTAEditView;
{$ENDIF}
begin
{$IFDEF GX_DELPHI2005_UP}
  if GxOtaTryGetTopMostEditView(FEditIntf, EditView) then
    (EditView as IOTAElideActions).UnElideNearestBlock;
{$ENDIF}
end;

procedure TEditReader.ShowForm;
begin
  AllocateFileData;
  Assert(Assigned(FModIntf));

  if FMode = mmModule then
    GxOtaShowFormForModule(FModIntf);
end;

procedure TEditReader.GotoLine(L: Integer);
begin
  InternalGotoLine(L, False);
end;

procedure TEditReader.GotoOffsetLine(L: Integer);
begin
  InternalGotoLine(L, True);
end;

procedure TEditReader.InternalGotoLine(Line: Integer; Offset: Boolean);
var
  EditView: IOTAEditView;
  EditPos: TOTAEditPos;
  S: Integer;
  ViewCount: Integer;
begin
  AllocateFileData;

  //{$IFOPT D+} SendDebug('LineCount ' + IntToStr(LineCount)); {$ENDIF}
  if Line > LineCount then Exit;
  if Line < 1 then
    Line := 1;

  Assert(FModIntf <> nil);
  ShowSource;

  Assert(FEditIntf <> nil);
  ViewCount := FEditIntf.EditViewCount;
  if ViewCount < 1 then
    Exit;

  EditView := FEditIntf.EditViews[0];
  if EditView <> nil then
  begin
    EditPos.Col := 1;
    EditPos.Line := Line;
    if Offset then
    begin
      EditView.CursorPos := EditPos;
      S := Line - (EditView.ViewSize.cy div 2);
      if S < 1 then S := 1;
      EditPos.Line := S;
      EditView.TopPos := EditPos;
    end
    else
    begin
      EditView.TopPos := EditPos;
      EditView.CursorPos := EditPos;
      ShowSource;
    end;
    EditView.Paint;
  end;
end;

function TEditReader.GetLineCount: Integer;
begin
  if FMode = mmModule then
  begin
    AllocateFileData;
    Assert(FEditIntf <> nil);

    Result := FEditIntf.GetLinesInBuffer;
  end
  else
    Result := -1;
end;

function TEditReader.ReadTextFromStream(Stream: TMemoryStream): string;
var
  Buffer: IDEEditBufferString;
begin
  SetLength(Buffer, Stream.Size);
  Stream.Position := 0;
  Stream.ReadBuffer(Buffer[1], Stream.Size);
  // In theory we could rely on UTF-8 having a BOM, meaning that
  // every file not containing a BOM is ANSI. Unfortunately this
  // turns out to be unreliable, so we assume everything to be
  // UTF-8, try to convert it to ANSI.
  // There are four possible cases:
  // 1) If it is ANSI and does not contain any high ASCII charactes the
  //    conversion will do nothing
  // 2) If it is UTF-8, the conversion will work fine
  // 3) If it is ANSI with high ASCII charactes, the conversion will fail
  //    and return an empty string. In this case we take the string as is.
  // 4) If it is neither valid UTF-8 nor ANSI, the conversion will fail
  //    and return an empty string. In this case we still take the string
  //    as is and hope for the best.
  // If this is a non-unicode version of Delphi, all files should be ANSI anyway
  // UTF8ToUnicodeString does nothing.
  // -- 2015-06-14 twm
  Result := UTF8ToUnicodeString(Buffer);
  if (Result = '') and (Buffer <> '') then begin
    Result := String(Buffer);
  end;
end;

function TEditReader.GetText: string;
var
  Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  try
    SaveToStream(Stream);
    Result := ReadTextFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

class function TEditReader.GetText(const FileName: string): string;
var
  edr: TEditReader;
begin
  edr := TEditReader.Create(FileName);
  try
    Result := edr.GetText;
  finally
    edr.Free;
  end;
end;

function TEditReader.GetTextFromPos: string;
var
  Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  try
    SaveToStreamFromPos(Stream);
    Result := ReadTextFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

function TEditReader.GetTextToPos: string;
var
  Stream: TMemoryStream;
begin
  Stream := TMemoryStream.Create;
  try
    SaveToStreamToPos(Stream);
    Result := ReadTextFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TEditReader.Reset;
begin
  if FMode = mmFile then
  begin
    // We do not need to allocate file data
    // in order to set the stream position
    if SFile <> nil then
      SFile.Position := 0;
  end;
end;

procedure TEditReader.SaveToStream(Stream: TStream);
var
  Pos: Integer;
  Size: Integer;
const
  TheEnd: Char = #0; // Leave typed constant as is - needed for streaming code
begin
  Assert(Stream <> nil);

  Reset;

  AllocateFileData;

  if Mode = mmFile then
  begin
    Assert(SFile <> nil);

    SFile.Position := 0;
    Stream.CopyFrom(SFile, SFile.Size);
  end
  else
  begin
    Pos := 0;
    if Buf = nil then
      Buf := AnsiStrAlloc(BufSize + 1);
    if FEditRead = nil then
      raise Exception.Create(SNoEditReader);
    // Delphi 5+ sometimes returns -1 here, for an unknown reason
    Size := FEditRead.GetText(Pos, Buf, BufSize);
    if Size = -1 then
    begin
      FreeFileData;
      AllocateFileData;
      Size := FEditRead.GetText(Pos, Buf, BufSize);
    end;
    if Size > 0 then
    begin
      Pos := Pos + Size;
      while Size = BufSize do
      begin
        Stream.Write(Buf^, Size);
        Size := FEditRead.GetText(Pos, Buf, BufSize);
        Pos := Pos + Size;
      end;
      Stream.Write(Buf^, Size);
    end;
  end;
  Stream.Write(TheEnd, 1);
end;

function TEditReader.GetCurrentBufferPos: Integer;
var
  EditorPos: TOTAEditPos;
  CharPos: TOTACharPos;
  EditView: IOTAEditView;
begin
  AllocateFileData;

  Assert(FEditIntf <> nil);

  Result := -1;
  Assert(FEditIntf.EditViewCount > 0);

  EditView := FEditIntf.EditViews[0];
  if EditView <> nil then
  begin
    EditorPos := EditView.CursorPos;
    EditView.ConvertPos(True, EditorPos, CharPos);
    Result := EditView.CharPosToPos(CharPos);
  end;
end;

function TEditReader.GetCurrentCharPos: TOTACharPos;
var
  EditorPos: TOTAEditPos;
  EditView: IOTAEditView;
begin
  AllocateFileData;

  Assert(FEditIntf <> nil);

  Result.CharIndex := -1;
  Result.Line := -1;
  Assert(FEditIntf.EditViewCount > 0);

  EditView := FEditIntf.EditViews[0];
  if EditView <> nil then
  begin
    EditorPos := EditView.CursorPos;
    EditView.ConvertPos(True, EditorPos, Result);
  end;
end;

procedure TEditReader.SaveToStreamFromPos(Stream: TStream);
var
  Pos: Integer;
  Size: Integer;
begin
  AllocateFileData;

  Reset;

  if Mode = mmFile then
  begin
    Assert(SFile <> nil);
    SFile.Position := 0;
    Stream.CopyFrom(SFile, SFile.Size);
  end
  else
  begin
    Pos := GetCurrentBufferPos;
    if Buf = nil then
      Buf := AnsiStrAlloc(BufSize);
    if FEditRead = nil then
      raise Exception.Create(SNoEditReader);
    Size := FEditRead.GetText(Pos, Buf, BufSize);
    if Size > 0 then
    begin
      Pos := Pos + Size;
      while Size = BufSize do
      begin
        Stream.Write(Buf^, Size);
        Size := FEditRead.GetText(Pos, Buf, BufSize);
        Pos := Pos + Size;
      end;
      Stream.Write(Buf^, Size);
    end;
  end;
end;

// The character at the current position is not written to the stream
procedure TEditReader.SaveToStreamToPos(Stream: TStream);
var
  Pos, AfterPos: Integer;
  ToReadSize, Size: Integer;
  NullChar: Char;
begin
  AllocateFileData;

  Reset;

  if Mode = mmFile then
  begin
    Assert(SFile <> nil);
    SFile.Position := 0;
    Stream.CopyFrom(SFile, SFile.Size);
  end
  else
  begin
    AfterPos := GetCurrentBufferPos;
    Pos := 0;
    if Buf = nil then
      Buf := AnsiStrAlloc(BufSize);
    if FEditRead = nil then
      raise Exception.Create(SNoEditReader);

    ToReadSize := Min(BufSize, AfterPos - Pos);
    Size := FEditRead.GetText(Pos, Buf, ToReadSize);
    if Size > 0 then
    begin
      Pos := Pos + Size;
      while Size = BufSize do
      begin
        Stream.Write(Buf^, Size);
        ToReadSize := Min(BufSize, AfterPos - Pos);
        Size := FEditRead.GetText(Pos, Buf, ToReadSize);
        Pos := Pos + Size;
      end;
      Stream.Write(Buf^, Size);
    end;
  end;
  NullChar := #0;
  Stream.Write(NullChar, SizeOf(NullChar));
end;

procedure TEditReader.ReleaseModuleNotifier;
begin
  if FNotifierIndex <> InvalidNotifierIndex then
    FModIntf.RemoveNotifier(FNotifierIndex);
  FNotifierIndex := InvalidNotifierIndex;
  FModuleNotifier := nil;
end;

end.

