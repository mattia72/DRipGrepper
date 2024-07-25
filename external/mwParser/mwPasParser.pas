{+--------------------------------------------------------------------------+
 | Component:   TmPasParser
 | Created:     11.97
 | Author:      Martin Waldenburg
 | Copyright    1997, all rights reserved.
 | Description: A Pascal parser.
 | Version:     1.91
 | Status:      FreeWare
 | DISCLAIMER:  This is provided as is, expressly without a warranty of any kind.
 |              You use it at your own risk.
 +--------------------------------------------------------------------------+}

unit mwPasParser;

interface

uses
  mwPasParserTypes;

type
  TmPasParser = class;

  TmPasToken = class(TObject)
  private
    function GetData: string;
  public
    ID: TTokenKind;
    LineNumber: Integer;
    LinePos: Integer;
    Position: Integer;
    Length: Integer;
    Origin: PChar;
    constructor Create;
    property Data: string read GetData;
  end;

  TmPasParser = class(TObject)
  private
    FToken: TmPasToken;
    FComment: TCommentState;
    FEndCount: Integer;
    FImplementationsPos: Integer;
    FLastComment: Integer;
    FLastIdentPos: Integer;
    FLastIdentLine: Integer;
    FLastSemiColon: Integer;
    fOrigin: PChar;
    Run: Integer;
    FRoundCount: ShortInt;
    FSquareCount: ShortInt;
    function GetIsJunk: Boolean;
    function IdentKind: TTokenKind;
    procedure SetOrigin(Value: PChar);
    procedure HandleComments;
  public
    constructor Create;
    destructor Destroy; override;
    function GetSubString(StartPos, EndPos: Integer): string;
    procedure NextClassLine;
    procedure NextObjectLine(out _IdentPos, _IdentLine: integer);
    procedure NextID(ID: TTokenKind);
    procedure NextNonComment;
    procedure NextNonJunk;
    procedure NextNonSpace;
    procedure NextToken;
    procedure ToLineStart;
    function GetMethodImpLine(const ClassName, MethodName: string): Integer;
    ///<summary>
    /// @WARNING: Setting the parser's RunPos is a hack, because it should also reset its internal
    //            state, but can't do that. The only proper way to do that would be restart the
    //            parser until it reaches the desired RunPos. So, maybe RunPos should be read only
    //            and any callers that try to set it should be forced to restart it.
    //            Use this method only if you think you know what you are doing. </summary>
    procedure SetRunPos(NewPos: Integer);
    property Comments: TCommentState read FComment;
    ///<summary>
    /// The number of "end" tokens to expect after the current position.
    /// This is only valid if RunPos has not been set explicitly. </summary>
    property EndCount: Integer read FEndCount;
    /// </summary>
    /// Position of the "implementation" section, if it has already been encountered, 0 otherwise </summary>
    property ImplementationsPos: Integer read FImplementationsPos;
    property IsJunk: Boolean read GetIsJunk;
    /// </summary>
    /// The position of the start of the last comment that was encountered </summary>
    property LastComment: Integer read FLastComment;
    ///<summary>
    /// The position of the last identifier that was encountered, -1 otherwise </summary>
    property LastIdentPos: Integer read FLastIdentPos;
    ///<summary>
    /// The line number of the last identifier that was encountered, only valid if LastIdentPos <> -1 </summary>
    property LastIdentLine: Integer read FLastIdentLine;
    ///<summary>
    /// The position of the last semicolon that was encountered, -1 otherwise </summary>
    property LastSemiColon: Integer read FLastSemiColon;
    property Origin: PChar read fOrigin write SetOrigin;
    //<summary>
    /// The current position of the parser.
    /// @NOTE: It is readonly because setting it has side effects.
    ///        Call the SetRunPos method if you think you know what you are doing. </summary>
    property RunPos: Integer read Run;
    property RoundCount: ShortInt read FRoundCount;
    property SquareCount: ShortInt read FSquareCount;
    property Token: TmPasToken read FToken;
  end;

implementation

uses
  SysUtils, GX_GenericUtils;

constructor TmPasToken.Create;
begin
  inherited Create;
  ID := tkNone;
  LineNumber := 0;
  LinePos := 0;
  Position := 0;
  Length := 0;
end; { Create }

function TmPasToken.GetData: string; // This incorrectly returns data as UTF-8 for D8+
begin
  SetString(Result, (Origin + Position), Length);
end;

{ GetData }

destructor TmPasParser.Destroy;
begin
  Token.Free;
  inherited Destroy;
end; { Destroy }

constructor TmPasParser.Create;
begin
  inherited Create;
  FComment := csNo;
  FEndCount := 0;
  FLastIdentPos := -1;
  FImplementationsPos := 0;
  FLastSemiColon := -1;
  FToken := TmPasToken.Create;
end; { Create }

function TmPasParser.GetSubString(StartPos, EndPos: Integer): string;
var
  SubLen: Integer;
begin
  if FOrigin[EndPos] = #10 then Inc(EndPos);
  SubLen := EndPos - StartPos;
  SetString(Result, (FOrigin + StartPos), SubLen);
end; { GetSubString }

procedure TmPasParser.SetOrigin(Value: PChar);
begin
  FOrigin := Value;
  Run := 0;
  FComment := csNo;
  FEndCount := 0;
  FRoundCount := 0;
  FSquareCount := 0;
  FImplementationsPos := 0;
  FToken.Origin := Value;
end; { SetOrigin }

procedure TmPasParser.SetRunPos(NewPos: Integer);
begin
  Run := NewPos;

  // We are assuming that SetRunPos will not be called for any position inside a comment,
  // so we set FComment to csNo. This assumption may be wrong.
  FComment := csNo;

  // The following properties will now possibly be wrong, but we can't do anything about that:
  // * EndCount
  // * RoundCount
  // * SquareCount
  // Fortunately they are not used internally.
  // Any code that wants to use them, should not set the RunPos property.

  // LastIdentPos might still be valid, but we can't know that
  FLastIdentPos := -1;
  // the same goes for LastSemiColon
  FLastSemiColon := -1;

  // lets hope that before somebody tries to use these properties, they are set correctly

  NextToken;
end; { SetRunPos }

procedure TmPasParser.HandleComments;
begin
  Case FComment of
    csAnsi:
      begin
        FLastComment := Run;
        FToken.Position := Run;
        FToken.ID := tkAnsiComment;
        while FOrigin[Run] <> #0 do
        begin
          Case FOrigin[Run] of
            #13:
              begin
                Case FToken.Position = Run of
                  True:
                    begin
                      FToken.Id := tkCRLFCo;
                      FToken.Length := 2;
                      FToken.Position := Run;
                      Inc(Run);
                    end;
                  False:
                    begin
                      FToken.Length := Run - FToken.Position;
                    end;
                end;
                Break;
              end;

            #10:
              begin
                Inc(Run);
                Inc(FToken.LineNumber);
                FToken.LinePos := Run;
                HandleComments;
                Break;
              end;

            '*': if FOrigin[Run + 1] = ')' then
              begin
                Inc(Run, 2);
                FToken.Length := Run - FToken.Position;
                FComment := csNo;
                Break;
              end;
          end;
          Inc(Run);
        end;
      end;

    csBor:
      begin
        FLastComment := Run;
        FToken.Position := Run;
        FToken.ID := tkBorComment;
        while FOrigin[Run] <> #0 do
        begin
          Case FOrigin[Run] of
            #13:
              begin
                Case FToken.Position = Run of
                  True:
                    begin
                      FToken.Id := tkCRLFCo;
                      FToken.Length := 2;
                      FToken.Position := Run;
                      Inc(Run);
                    end;
                  False:
                    begin
                      FToken.Length := Run - FToken.Position;
                    end;
                end;
                Break;
              end;

            #10:
              begin
                Inc(Run);
                Inc(FToken.LineNumber);
                FToken.LinePos := Run;
                HandleComments;
                Break;
              end;

            '}':
              begin
                Inc(Run);
                FToken.Length := Run - FToken.Position;
                FComment := csNo;
                Break;
              end;
          end;
          Inc(Run);
        end;
      end;

    csSlashes:
      begin
        FLastComment := Run;
        FToken.Position := Run;
        FToken.ID := tkSlashesComment;
        while FOrigin[Run] <> #0 do
        begin
          Case FOrigin[Run] of
            #13:
              begin
                FToken.Length := Run - FToken.Position;
                FComment := csNo;
                Break;
              end;
          end;
          Inc(Run);
        end;
      end;

  end;
end; { HandleComments }

function TmPasParser.IdentKind: TTokenKind;
var
  HashKey: Integer;
  aToken: string;

  function KeyHash: Integer;
  var
    I: Integer;
  begin
    Result := 0;
    for i := 1 to Length(aToken) do Result := Result + Ord(aToken[i]);
  end; { KeyHash }

begin
  Result := tkIdentifier;
  aToken := UpperCase(FToken.GetData);
  HashKey := KeyHash;
  Case HashKey of
    143: if aToken = 'IF' then Result := tkIf;
    147: if aToken = 'DO' then Result := tkDo;
    148: if aToken = 'AS' then Result := tkAs;
    149: if aToken = 'OF' then Result := tkOf;
    151: if aToken = 'IN' then Result := tkIn;
    156: if aToken = 'IS' then Result := tkIs;
    161: if aToken = 'OR' then Result := tkOr;
    163: if aToken = 'TO' then Result := tkTo;
    211: if aToken = 'AND' then Result := tkAnd;
    215: if aToken = 'END' then Result := tkEnd;
    217: if aToken = 'FAR' then Result := tkFar;
    224: if aToken = 'MOD' then Result := tkMod;
    225: if aToken = 'ASM' then Result := tkAsm;
    227:
      begin
        if aToken = 'DIV' then Result := tkDiv;
        if aToken = 'NIL' then Result := tkNil;
      end;
    231:
      begin
        if aToken = 'FOR' then Result := tkFor;
        if aToken = 'SHL' then Result := tkShl;
      end;
    233: if aToken = 'VAR' then Result := tkVar;
    236: if aToken = 'SET' then Result := tkSet;
    237: if aToken = 'SHR' then Result := tkShr;
    241: if aToken = 'NOT' then Result := tkNot;
    248: if aToken = 'OUT' then Result := tkOut;
    249: if aToken = 'XOR' then Result := tkXor;
    255: if aToken = 'TRY' then Result := tkTry;
    284:
      begin
        if aToken = 'CASE' then Result := tkCase;
        if aToken = 'READ' then Result := tkRead;
      end;
    288: if aToken = 'FILE' then Result := tkFile;
    289: if aToken = 'NAME' then Result := tkName;
    294: if aToken = 'NEAR' then Result := tkNear;
    297: if aToken = 'ELSE' then Result := tkElse;
    303: if aToken = 'THEN' then Result := tkThen;
    313: if aToken = 'GOTO' then Result := tkGoto;
    316: if aToken = 'WITH' then Result := tkWith;
    320:
      begin
        if aToken = 'UNIT' then Result := tkUnit;
        if aToken = 'USES' then Result := tkUses;
      end;
    322: if aToken = 'TYPE' then Result := tkType;
    341: if aToken = 'INT64' then Result := tkInt64;
    347: if aToken = 'CDECL' then Result := tkCdecl;
    352: if aToken = 'LABEL' then Result := tkLabel;
    357: if aToken = 'BEGIN' then Result := tkBegin;
    372: if aToken = 'RAISE' then Result := tkRaise;
    374: if aToken = 'CLASS' then Result := tkClass;
    376: if aToken = 'INDEX' then Result := tkIndex;
    377: if aToken = 'WHILE' then Result := tkWhile;
    383: if aToken = 'ARRAY' then Result := tkArray;
    391: if aToken = 'CONST' then Result := tkConst;
    395: if aToken = 'WRITE' then Result := tkWrite;
    396: if aToken = 'UNTIL' then Result := tkUntil;
    424: if aToken = 'PACKED' then Result := tkPacked;
    436: if aToken = 'PASCAL' then Result := tkPascal;
    439: if aToken = 'OBJECT' then Result := tkObject;
    445: if aToken = 'DISPID' then Result := tkDispid;
    447:
      begin
        if aToken = 'INLINE' then Result := tkInline;
        if aToken = 'PUBLIC' then Result := tkPublic;
        if aToken = 'RECORD' then Result := tkRecord;
      end;
    449: if aToken = 'REPEAT' then Result := tkRepeat;
    456: if aToken = 'STATIC' then Result := tkStatic;
    457: if aToken = 'EXCEPT' then Result := tkExcept;
    465: if aToken = 'STORED' then Result := tkStored;
    471: if aToken = 'STRING' then Result := tkKeyString;
    473: if aToken = 'STRICT' then Result := tkStrict;
    475: if aToken = 'DOWNTO' then Result := tkDownto;
    482: if aToken = 'EXPORT' then Result := tkExport;
    517:
      begin
        if aToken = 'DEFAULT' then Result := tkDefault;
        if aToken = 'DYNAMIC' then Result := tkDynamic;
        if aToken = 'MESSAGE' then Result := tkMessage;
      end;
    519: if aToken = 'STDCALL' then Result := tkStdcall;
    527: if aToken = 'FINALLY' then Result := tkFinally;
    533:
      begin
        if aToken = 'FORWARD' then Result := tkForward;
        if aToken = 'LIBRARY' then Result := tkLibrary;
      end;
    536: if aToken = 'PROGRAM' then Result := tkProgram;
    539: if aToken = 'PRIVATE' then Result := tkPrivate;
    551: if aToken = 'VIRTUAL' then Result := tkVirtual;
    565: if aToken = 'EXPORTS' then Result := tkExports;
    571: if aToken = 'SAFECALL' then Result := tkSafecall;
    596: if aToken = 'ABSTRACT' then Result := tkAbstract;
    604: if aToken = 'OVERLOAD' then Result := tkOverload;
    606:
      begin
        if aToken = 'READONLY' then Result := tkReadonly;
        if aToken = 'RESIDENT' then Result := tkResident;
      end;
    607: if aToken = 'ABSOLUTE' then Result := tkAbsolute;
    608: if aToken = 'OVERRIDE' then Result := tkOverride;
    611: if aToken = 'EXTERNAL' then Result := tkExternal;
    613: if aToken = 'REGISTER' then Result := tkRegister;
    614: if aToken = 'FUNCTION' then Result := tkFunction;
    620:
      begin
        if aToken = 'LONGWORD' then Result := tkLongWord;
        if aToken = 'OPERATOR' then Result := tkOperator;
      end;
    645: if aToken = 'PROPERTY' then Result := tkProperty;
    657: if aToken = 'INTERFACE' then Result := tkInterface;
    668: if aToken = 'INHERITED' then Result := tkInherited;
    670: if aToken = 'ASSEMBLER' then Result := tkAssembler;
    672: if aToken = 'PUBLISHED' then Result := tkPublished;
    673: if aToken = 'THREADVAR' then Result := tkThreadvar;
    674: if aToken = 'NODEFAULT' then Result := tkNodefault;
    676: if aToken = 'AUTOMATED' then Result := tkAutomated;
    681: if aToken = 'PROCEDURE' then Result := tkProcedure;
    682: if aToken = 'PROTECTED' then Result := tkProtected;
    717: if aToken = 'WRITEONLY' then Result := tkWriteonly;
    766: if aToken = 'IMPLEMENTS' then Result := tkImplements;
    783: if aToken = 'DESTRUCTOR' then Result := tkDestructor;
    836: if aToken = 'REINTRODUCE' then Result := tkReintroduce;
    870: if aToken = 'CONSTRUCTOR' then Result := tkConstructor;
    904: if aToken = 'FINALIZATION' then Result := tkFinalization;
    961: if aToken = 'DISPINTERFACE' then Result := tkDispinterface;
    1062: if aToken = 'IMPLEMENTATION' then Result := tkImplementation;
    1064: if aToken = 'INITIALIZATION' then Result := tkInitialization;
    1087:
      begin
        if aToken = 'RESOURCESTRING' then Result := tkResourcestring;
        if aToken = 'STRINGRESOURCE' then Result := tkStringresource;
      end;
  end;
  Case Result of
    tkIdentifier: begin
      FLastIdentPos := FToken.Position;
      FLastIdentLine := FToken.LineNumber;
    end;
    tkImplementation: FImplementationsPos := FToken.Position;
    tkCase: Inc(FEndCount);
    // todo: Is this correct? Whe have got nested classes nowadays
    tkClass: FEndCount := 1;
    tkBegin: Inc(FEndCount);
    tkEnd: Dec(FEndCount);
    tkRecord: Inc(FEndCount);
    tkObject: Inc(FEndCount);
  end;
end; { IdentKind }

procedure TmPasParser.NextToken;
begin
  Case FOrigin[Run] of
    #0:
      begin
        FToken.Id := tkNull;
        FToken.Length := 0;
        FToken.Position := Run;
        Exit;
      end;
  end;

  FToken.Position := Run;
  if FComment <> csNo then HandleComments
  else
  begin
    Case FOrigin[Run] of
      #10:
        begin
          Inc(Run);
          Inc(FToken.LineNumber);
          FToken.LinePos := Run;
          NextToken;
        end;

      #13:
        begin
          FToken.Id := tkCRLF;
          FToken.Length := 2;
          FToken.Position := Run;
          if not (fOrigin[Run + 1] = #10) then
            Inc(FToken.LineNumber);
          Inc(Run);
        end;

      #1..#9, #11, #12, #14..#32:
        begin
          FToken.Position := Run;
          while CharInSet(FOrigin[Run], [#1..#9, #11, #12, #14..#32]) do Inc(Run);
          FToken.ID := tkSpace;
          FToken.Length := Run - FToken.Position;
        end;

      '/':
        Case FOrigin[Run + 1] of
          '/':
            begin
              FComment := csSlashes;
              HandleComments;
            end;
        else
          begin
            FToken.Position := Run;
            Inc(Run);
            FToken.ID := tkSymbol;
            FToken.Length := 1;
          end;
        end;

      '(':
        Case FOrigin[Run + 1] of
          '*':
            begin
              FComment := csAnsi;
              HandleComments;
            end;
          '.':
            begin
              FToken.Position := Run;
              Inc(Run, 2);
              FToken.ID := tkSquareOpen;
              FToken.Length := 2;
            end;
        else
          begin
            FToken.Position := Run;
            Inc(Run);
            FToken.Length := 1;
            FToken.ID := tkRoundOpen;
            Inc(FRoundCount);
          end;
        end;

      'A'..'Z', 'a'..'z', '_':
        begin
          FToken.Position := Run;
          Inc(Run);
          while CharInSet(FOrigin[Run], ['A'..'Z', 'a'..'z', '0'..'9', '_']) do Inc(Run);
          FToken.Length := Run - FToken.Position;
          FToken.ID := IdentKind;
        end;

      '0'..'9':
        begin
          FToken.Position := Run;
          Inc(Run);
          FToken.ID := tkNumber;
          while CharInSet(FOrigin[Run], ['0'..'9', '.', 'e', 'E']) do
          begin
            Case FOrigin[Run] of
              '.': if FOrigin[Run + 1] <> '.' then FToken.ID := tkFloat else Break;
            end;
            Inc(Run);
          end;
          FToken.Length := Run - FToken.Position;
        end;

      '{':
        begin
          FComment := csBor;
          HandleComments;
        end;

      '!', '"', '%', '&', ')'..'.', ':'..'@', '['..'^', '`', '~':
        begin
          FToken.Position := Run;
          Case FOrigin[Run] of
            ')':
              begin
                FToken.ID := tkRoundClose;
                Dec(FRoundCount);
              end;

            '*': FToken.ID := tkStar;

            '+': FToken.ID := tkPlus;

            ',': FToken.ID := tkComma;

            '-': FToken.ID := tkMinus;

            '.':
              Case FOrigin[Run + 1] of
                ')':
                  begin
                    Inc(Run);
                    FToken.ID := tkSquareClose;
                  end;
              else FToken.ID := tkPoint;
              end;

            ':':
              Case FOrigin[Run + 1] of
                '=':
                  begin
                    Inc(Run);
                    FToken.ID := tkAssign;
                  end;
              else FToken.ID := tkColon;
              end;

            ';':
              begin
                FToken.ID := tkSemiColon;
                FLastSemiColon := Run;
              end;

            '<':
              Case FOrigin[Run + 1] of
                '=':
                  begin
                    Inc(Run);
                    FToken.ID := tkLowerEqual;
                  end;
                '>':
                  begin
                    Inc(Run);
                    FToken.ID := tkNotEqual;
                  end;
              else FToken.ID := tkLower;
              end;

            '=': FToken.ID := tkEqual;

            '>':
              Case FOrigin[Run + 1] of
                '=':
                  begin
                    Inc(Run);
                    FToken.ID := tkGreaterEqual;
                  end;
              else FToken.ID := tkGreater;
              end;

            '[':
              begin
                FToken.ID := tkSquareOpen;
                Inc(FSquareCount);
              end;

            ']':
              begin
                FToken.ID := tkSquareClose;
                Dec(FSquareCount);
              end;

          else FToken.ID := tkSymbol;

          end;
          Inc(Run);
          FToken.Length := Run - FToken.Position;
        end;

      #39:
        begin
          FToken.ID := tkString;
          Inc(Run);
          while True do begin
            if (fOrigin[Run] = #39) then begin
              if (fOrigin[Run + 1] = #39) then
                Inc(Run)
              else begin
                Inc(Run);
                Break;
              end;
            end
            else if IsCharLineEndingOrNull(fOrigin[Run]) then
            begin
              FToken.ID := tkBadString;
              Break;
            end;
            Inc(Run);
          end;
          FToken.Length := Run - FToken.Position;
        end;

      '#':
        begin
          FToken.Position := Run;
          FToken.ID := tkAsciiChar;
          Inc(Run);
          while CharInSet(FOrigin[Run], ['0'..'9']) do Inc(Run);
          FToken.Length := Run - FToken.Position;
        end;

      '$':
        begin
          FToken.Position := Run;
          FToken.ID := tkInteger;
          Inc(Run);
          while CharInSet(FOrigin[Run], ['0'..'9', 'A'..'F', 'a'..'f']) do Inc(Run);
          FToken.Length := Run - FToken.Position;
        end;

    else
      begin
        FToken.Position := Run;
        Inc(Run);
        FToken.ID := tkUnknown;
        FToken.Length := Run - FToken.Position;
      end;
    end;
  end;
end; {NextToken}

procedure TmPasParser.NextID(ID: TTokenKind);
begin
  NextToken;
  while FToken.ID <> ID do
  begin
    Case FToken.ID of
      tkNull: Break;
    else NextToken;
    end;
  end;
end; { NextID }

function TmPasParser.GetIsJunk: Boolean;
begin
  Case FToken.ID of
    tkAnsiComment, tkBorComment, tkCRLF, tkCRLFCo, tkSlashesComment, tkSpace:
      Result := True;
  else Result := False;
  end;
end;

procedure TmPasParser.NextNonComment;
begin
  repeat
    NextToken;
  until not (Token.ID in [tkAnsiComment, tkBorComment, tkCRLFCo,
    tkSlashesComment]);
end; { NextNonComCRLF }

procedure TmPasParser.NextNonJunk;
begin
  repeat
    NextToken;
  until not (Token.ID in [tkAnsiComment, tkBorComment, tkCRLF, tkCRLFCo,
    tkSlashesComment, tkSpace]);
end; { NextNonJunk }

procedure TmPasParser.NextNonSpace;
begin
  NextToken;
  while FToken.ID = tkSpace do NextToken;
end; { NextNonSpace }

procedure TmPasParser.ToLineStart;
begin
  Run := FToken.LinePos;
  NextToken;
end; { ToLineStart }

procedure TmPasParser.NextClassLine;
begin
  while FToken.ID <> tkNull do
  begin
    if FToken.ID in BigIdentDirect then
    begin
      FLastIdentPos := FToken.Position;
      FLastIdentLine := FToken.LineNumber;
      NextNonJunk;
      if FToken.ID = tkEqual then
      begin
        NextNonJunk;
        if FToken.ID = tkClass then
          Break; //==v
      end;
    end;
    NextNonJunk;
  end;
end; { NextClassLine }

procedure TmPasParser.NextObjectLine(out _IdentPos, _IdentLine: integer);
begin
  while FToken.ID <> tkNull do
  begin
    if FToken.ID in BigIdentDirect then
    begin
      FLastIdentPos := FToken.Position;
      FLastIdentLine := FToken.LineNumber;
      NextNonJunk;
      if FToken.ID = tkEqual then
      begin
        NextNonJunk;
        if FToken.ID in [tkClass, tkDispInterface, tkInterface] then
          Break; //==v
      end;
    end;
    NextNonJunk;
  end;
  _IdentPos := FLastIdentPos;
  _IdentLine := FLastIdentLine;
end; { NextObjectLine }

function TmPasParser.GetMethodImpLine(const ClassName, MethodName: string): Integer;
begin
  Result := -1;
  while FToken.ID <> tkNull do
  begin
    NextToken;
    if FToken.ID in [tkClass] + MethodMarkers then
    begin
      // For class methods, skip past the "class" keyword: function/procedure is next
      if FToken.ID = tkClass then
        NextNonJunk;
      NextNonJunk;
      if UpperCase(FToken.Data) = UpperCase(ClassName) then
      begin
        NextNonJunk;
        if FToken.ID = tkPoint then
        begin
          NextNonJunk;
          if UpperCase(FToken.Data) = UpperCase(MethodName) then
          begin
            Result := FToken.LineNumber;
            Break;
          end;
        end;
      end;
    end;
  end;
end; { GetMethodImpLine }

end.

