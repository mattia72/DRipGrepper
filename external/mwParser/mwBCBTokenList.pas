{+--------------------------------------------------------------------------+
 | Unit:        mwBCBTokenList
 | Created:     3.98
 | Author:      Martin Waldenburg
 | Copyright    1997, all rights reserved.
 | Description: TLongIntList is a dynamic array of LongInts.
 |              TmsSearcher is a specialized version of the turbo search engine,
 |              which is based on an article in the German magazine c't (8/97).
 |              TBCBTokenList scans a PChar for BCB tokens and gives full access.
 | Version:     1.0
 | DISCLAIMER:  This is provided as is, expressly without a warranty of any kind.
 |              You use it at your own risc.
 +--------------------------------------------------------------------------+}

 // Modifications/reformatting by Ales Kahanek and Erik Berry (July 2001)

unit mwBCBTokenList;

{$RANGECHECKS OFF}

interface

uses mwLongIntList;

type
  TCTokenKind = (ctkapostrophe, ctk_CHAR_UNSIGNED, ctk_CPPUNWIND, ctk_M_IX86, ctk_WCHAR_T,
    ctk_WCHAR_T_DEFINED, ctk_Windows, ctk__BCOPT__, ctk__BCPLUSPLUS__,
    ctk__BORLANDC__, ctk__CDECL__, ctk__CONSOLE__, ctk__DATE__, ctk__DLL__,
    ctk__FILE__, ctk__LINE__, ctk__MSDOS__, ctk__MT__, ctk__PASCAL__,
    ctk__STDC__, ctk__TCPLUSPLUS__, ctk__TEMPLATES__, ctk__TIME__, ctk__TLS__,
    ctk__TURBOC__, ctk__WIN32__, ctkansicomment, ctk__asm, ctk__automated,
    ctk__cdecl, ctk__classid, ctk__closure, ctk__cplusplus, ctk__declspec,
    ctk__dispid, ctk__except, ctk__export, ctk__fastcall, ctk__finally,
    ctk__import, ctk__int16, ctk__int32, ctk__int64, ctk__int8, ctk__pascal,
    ctk__property, ctk__published, ctk__rtti, ctk__stdcall, ctk__thread, ctk__try,
    ctk_asm, ctk_cdecl, ctk_export, ctk_fastcall, ctk_import, ctk_pascal,
    ctk_stdcall, ctkasm, ctkassign, ctkassignbitand, ctkassigndivision,
    ctkassignexcbitor, ctkassignincbitor, ctkassignment, ctkassignmodulus,
    ctkassignmultible, ctkassignminus, ctkassignplus, ctkassignshiftleft,
    ctkassignshiftright, ctkauto, ctkbitand, ctkbitnegation, ctkblend, ctkbool,
    ctkbraceclose, ctkbraceopen, ctkbracepair, ctkbreak, ctkcase, ctkcatch,
    ctkcdecl, ctkchar, ctkchartype, ctkclass, ctkcomma, ctkcompdirect, ctkcolon,
    ctkcoloncolon, ctkconditional, ctkconst, ctkconst_cast, ctkcontinue, ctkcrlf,
    ctkdefault, ctkdelete, ctkderefpointer, ctkderefpointerpointer, ctkdirdefine,
    ctkdirelif, ctkdirelse, ctkdirendif, ctkdirerror, ctkdirif, ctkdirifdef,
    ctkdirifndef, ctkdirinclude, ctkdirline, ctkdirnull, ctkdirpragma,
    ctkdirundef, ctkdo, ctkdotdot, ctkdouble, ctkdynamic_cast, ctkelse,
    ctkenum, ctkequal, ctkexplicit, ctkextern, ctkfalse, ctkfloat, ctkfor,
    ctkfriend, ctkgoto, ctkgreater, ctkgreaterequal, ctkif, ctkidentifier,
    ctkexcbitor, ctkincbitor, ctkinline, ctkint, ctkInteger, ctklogicand,
    ctklogicor, ctklong, ctklower, ctklowerequal, ctkminus, ctkminusminus,
    ctkmodulus, ctkmutable, ctknamespace, ctknegation, ctknew, ctknotequal,
    ctknull, ctknumber, ctkoperator, ctkpascal, ctkplus, ctkplusplus, ctkpoint,
    ctkpointpoint, ctkprivate, ctkprotected, ctkpublic, ctkregister,
    ctkreinterpret_cast, ctkreturn, ctkroundclose, ctkroundopen, ctkroundpair,
    ctkselectelement, ctksemicolon, ctkshiftleft, ctkshiftright, ctkshort,
    ctksigned, ctksizeof, ctkslash, ctkslashescomment, ctkspace, ctksquareclose,
    ctksquareopen, ctksquarepair, ctkstar, ctkstatstar, ctkstatic,
    ctkstatic_cast, ctkstring, ctkstruct, ctkswitch, ctksymbol, ctktemplate,
    ctkthis, ctkthreepoint, ctkthrow, ctktrue, ctktry, ctktypedef, ctktypeid,
    ctktypename, ctkunion, ctkUnknown, ctkunsigned, ctkusing, ctkvirtual,
    ctkvoid, ctkvolatile, ctkwchar_t, ctkwhile, ctknextline);

  TCTokenKindSet = set of TCTokenKind;

  TCommentState = (csAnsi, csNo, csSlashes);

  TBCBTokenList = class;

  TmsSearcher = class(TObject)
  private
    FBCBTokenList: TBCBTokenList;
    FSearchOrigin: PChar;
    Pat: String;
    fPos: Integer;
    HalfLen: Integer;
    PatLenPlus: Integer;
    SearchLen: Integer;
    Shift: array[0..255] of Integer;
    fFinished: Boolean;
    fFound: Boolean;
    fPosition: Integer;
    FFoundList: TLongintList;
    function GetFinished: Boolean;
    function GetItems(Index: Integer): Integer;
    function GetCount: Integer;
  protected
  public
    ClassList: TLongintList;
    ImplementationsList: TLongintList;
    InterfaceList: TLongintList;
    MethodList: TLongintList;
    PatLen: Integer;
    constructor Create(Value: TBCBTokenList);
    destructor Destroy; override;
    function Next: Integer;
    procedure Add(aPosition: Integer);
    procedure FillClassList;
    procedure Init(const NewPattern: string);
    procedure Retrive(aToken: string);
    function GetMethodImplementation(const aClassName, aMethodIdentifier: string): Longint;
    procedure FillMethodList;
    procedure FillInterfaceList;
    function GetMethodImpLine(const aClassName, aMethodIdentifier: string): Longint;
    property Finished: Boolean read GetFinished;
    property Found: Boolean read fFound;
    property Position: Integer read fPosition write fPos;
    property Items[Index: Integer]: Integer read GetItems; default;
    property Count: Integer read GetCount;
  end; { TmsSearcher }

  TBCBTokenList = class(TObject)
  private
    FTokenPositionsList: TLongintList;
    fOrigin: PChar;
    fPCharSize: Longint;
    fPCharCapacity: Longint;
    FComment: TCommentState;
    FEndCount: Integer;
    Run: Longint;
    FRoundCount: Integer;
    FSquareCount: Integer;
    FBraceCount: Integer;
    FVisibility: TCTokenKind;
    FDirectivesAsComments: Boolean;
    procedure WriteTo(InsPos, DelPos: Longint; const Item: string);
    function GetCount: Integer;
    procedure SetCount(Value: Integer);
    function GetCapacity: Integer;
    procedure ResetPositionsFrom(Index, Value: Longint);
    function GetIsJunk: Boolean;
    function IdentKind(Index: Longint): TCTokenKind;
    function DirKind(StartPos, EndPos: Longint): TCTokenKind;
    procedure SetRunIndex(NewPos: Longint);
    procedure HandleComments;
    function GetTokenID(Index: Longint): TCTokenKind;
    function GetTokenPosition(Index: Integer): Longint;
    function GetRunID: TCTokenKind;
    function GetRunPosition: Longint;
    function GetRunToken: string;
  protected
    function GetToken(Index: Integer): string;
    procedure SetCapacity(NewCapacity: Integer);
    procedure SetToken(Index: Integer; const Item: string);
  public
    Searcher: TmsSearcher;
    constructor Create;
    destructor Destroy; override;
    procedure SetOrigin(NewOrigin: PChar; NewSize: Longint);
    function Add(const Item: string): Integer;
    procedure Clear;
    procedure Delete(Index: Integer);
    procedure Exchange(Index1, Index2: Integer);
    function First: string;
    function IndexOf(const Item: string): Integer;
    procedure Insert(Index: Integer; const Item: string);
    function Last: string;
    procedure Move(CurIndex, NewIndex: Integer);
    function Remove(const Item: string): Integer;
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read GetCount write SetCount;
    property Token[Index: Integer]: string read GetToken write SetToken; default;
    property TokenPositionsList: TLongintList read FTokenPositionsList;
    property Origin: PChar read fOrigin;
    property PCharSize: Longint read fPCharSize;
    property PCharCapacity: Longint read fPCharCapacity;
    function GetSubString(StartPos, EndPos: Longint): string;
    procedure Next;
    procedure Previous;
    procedure NextID(ID: TCTokenKind);
    procedure NextNonComment;
    procedure NextNonJunk;
    procedure NextNonSpace;
    procedure Tokenize;
    procedure ToLineStart;
    procedure PreviousID(ID: TCTokenKind);
    procedure PreviousNonComment;
    procedure PreviousNonJunk;
    procedure PreviousNonSpace;
    function PositionAtLine(aPosition: Longint): Longint;
    function IndexAtLine(anIndex: Longint): Longint;
    function PositionToIndex(aPosition: Longint): Longint;
    property Comments: TCommentState read FComment write FComment;
    property DirectivesAsComments: Boolean read FDirectivesAsComments write FDirectivesAsComments;
    property EndCount: Integer read FEndCount write FEndCount;
    property IsJunk: Boolean read GetIsJunk;
    property RunIndex: Longint read Run write SetRunIndex;
    property BraceCount: Integer read FBraceCount write FBraceCount;
    property RoundCount: Integer read FRoundCount write FRoundCount;
    property SquareCount: Integer read FSquareCount write FSquareCount;
    property Visibility: TCTokenKind read FVisibility write FVisibility;
    property TokenID[Index: Longint]: TCTokenKind read GetTokenID;
    property TokenPosition[Index: Longint]: Longint read GetTokenPosition;
    property RunID: TCTokenKind read GetRunID;
    property RunPosition: Longint read GetRunPosition;
    property RunToken: string read GetRunToken;
  end; { TBCBTokenList }

const
  IdentDirectC: TCTokenKindSet = [ctkdirdefine, ctkdirelif, ctkdirelse,
    ctkdirendif, ctkdirerror, ctkdirif, ctkdirifdef, ctkdirifndef,
    ctkdirinclude, ctkdirline, ctkdirnull, ctkdirpragma, ctkdirundef];

implementation

uses SysUtils, GX_GenericUtils;

constructor TmsSearcher.Create(Value: TBCBTokenList);
begin
  inherited Create;
  FBCBTokenList := Value;
  Pat := '';
  PatLen := 0;
  HalfLen := 0;
  SearchLen := 0;
  fPos := -1;
  fFound := False;
  FFoundList := TLongintList.Create;
  ClassList := TLongintList.Create;
  ImplementationsList := TLongintList.Create;
  InterfaceList := TLongintList.Create;
  MethodList := TLongintList.Create;
end; { Create }

destructor TmsSearcher.Destroy;
begin
  FFoundList.Free;
  ClassList.Free;
  ImplementationsList.Free;
  InterfaceList.Free;
  MethodList.Free;
  inherited Destroy;
end; { Destroy }

function TmsSearcher.GetFinished: Boolean;
begin
  fFinished := False;
  if fPos >= SearchLen - 1 then fFinished := True;
  if PatLen > SearchLen then fFinished := True;
  Result := fFinished;
end; { GetFinished }

procedure TmsSearcher.Init(const NewPattern: string);
var
  I: Byte;
begin
  FFoundList.Clear;
  SearchLen := FBCBTokenList.PCharSize;
  FSearchOrigin := FBCBTokenList.Origin;
  Pat := NewPattern;
  PatLen := Length(Pat);
  PatLenPlus := PatLen + 1;
  HalfLen := PatLen div 2;
  for I := 0 to 255 do Shift[I] := PatLenPlus;
  for I := 1 to PatLen do Shift[ord(Pat[I])] := PatLenPlus - I;
  fPos := -1;
end; { Init }

function TmsSearcher.Next: Integer;
var
  I, J: Integer;
begin
  Result := -1;
  fFound := False;
  Inc(fPos, PatLen);
  fPosition := -1;
  while fPos <= SearchLen do
  begin
    I := PatLen;
    if (Pat[I] <> FSearchOrigin[fPos]) then
      Inc(fPos, Shift[ord(FSearchOrigin[fPos + 1])])
    else
    begin
      J := fPos;
      repeat
        Dec(I); Dec(J);
      until (I = 0) or (Pat[I] <> FSearchOrigin[J]);
      if I = 0 then
      begin
        fFound := True;
        fPosition := fPos - Patlen + 1;
        Result := fPosition;
        Break;
      end else if I < HalfLen then Inc(fPos, PatLenPlus)
      else Inc(fPos, Shift[ord(FSearchOrigin[J + 1])]);
    end;
  end;
end; { Next }

function TmsSearcher.GetItems(Index: Integer): Integer;
begin
  if (Index >= FFoundList.Count) or (Index < 0) then Result := -1 else
    Result := FFoundList[Index];
end; { GetItems }

function TmsSearcher.GetCount: Integer;
begin
  Result := FFoundList.Count;
end; { GetCount }

procedure TmsSearcher.Add(aPosition: Integer);
begin
  FFoundList.Add(aPosition);
end; { Add }

procedure TmsSearcher.FillClassList;
//var
//  RPos: Longint;
//  RIndex: Longint;
begin
  Assert(False);
end; { FillClassList }

procedure TmsSearcher.FillInterfaceList;
//var
//  RPos: Longint;
//  RIndex: Longint;
begin
  Assert(False);
end; { FillInterfaceList }

procedure TmsSearcher.FillMethodList;
//var
//  RPos: Longint;
//  RIndex: Longint;
begin
  Assert(False);
end; { FillMethodList }

procedure TmsSearcher.Retrive(aToken: string);
var
  RPos: Longint;
  RIndex: Longint;
begin
  aToken := UpperCase(aToken);
  Init(aToken);
  while not Finished do
  begin
    RPos := Next;
    if RPos <> -1 then
    begin
      RIndex := FBCBTokenList.PositionToIndex(RPos);
      if (RPos = FBCBTokenList.FTokenPositionsList[RIndex]) then
        if aToken = UpperCase(FBCBTokenList[RIndex]) then Add(RIndex);
    end;
  end;
end; { Retrive }

function TmsSearcher.GetMethodImplementation(const aClassName, aMethodIdentifier: string): Longint;
//var
//  RPos: Longint;
//  RIndex: Longint;
//  ToFind: string;
//  Found: Boolean;
begin
  Assert(False);
  Result := 0;
end; { GetMethodImplementation }

function TmsSearcher.GetMethodImpLine(const aClassName, aMethodIdentifier: string): Longint;
var
  ImpIndex: Longint;
begin
  ImpIndex := GetMethodImplementation(aClassName, aMethodIdentifier);
  Result := FBCBTokenList.IndexAtLine(ImpIndex);
end; { GetMethodImpLine }

constructor TBCBTokenList.Create;
begin
  inherited Create;
  FTokenPositionsList := TLongintList.Create;
  FTokenPositionsList.Add(0);
  FComment := csNo;
  FEndCount := 0;
  Visibility := ctkUnknown;
  Searcher := TmsSearcher.Create(Self);
  FDirectivesAsComments := True;
end; { Create }

destructor TBCBTokenList.Destroy;
begin
  FTokenPositionsList.Free;
  Searcher.Free;
  inherited Destroy;
end; { Destroy }

procedure TBCBTokenList.SetOrigin(NewOrigin: PChar; NewSize: Longint);
begin
  FOrigin := NewOrigin;
  Run := 0;
  fPCharSize := NewSize;
  fPCharCapacity := fPCharSize;
  Tokenize;
  Run := 0;
  //Searcher.FillClassList;
  FRoundCount := 0;
  FSquareCount := 0;
end; { SetOrigin }

procedure TBCBTokenList.WriteTo(InsPos, DelPos: Longint;
  const Item: string);
var
  StringCount, NewSize: Longint;
  aString: string;
begin
  aString := Item + (FOrigin + DelPos);
  StringCount := Length(aString);
  if (InsPos >= 0) and (StringCount >= 0) then
  begin
    NewSize := InsPos + StringCount;
    if NewSize > 0 then
    begin
      if NewSize >= FPCharCapacity then
      begin
        try
          FPCharCapacity := FPCharCapacity + 16384;
          ReAllocMem(FOrigin, PCharCapacity);
        except
          raise Exception.Create('unable to reallocate PChar');
        end;
      end;
      StrECopy((FOrigin + InsPos), PChar(AString)); // ?? Safe? It can change the size of the editor buffer
      FPCharSize := NewSize;
      FOrigin[FPCharSize] := #0;
      aString := '';
    end;
  end;
end; { WriteTo }

function TBCBTokenList.GetCount: Integer;
begin
  Result := FTokenPositionsList.Count - 1;
end; { GetCount }

procedure TBCBTokenList.SetCount(Value: Integer);
begin
  FTokenPositionsList.Count := Value + 1;
end; { SetCount }

function TBCBTokenList.GetCapacity: Integer;
begin
  Result := FTokenPositionsList.Capacity;
end; { GetCapacity }

procedure TBCBTokenList.ResetPositionsFrom(Index, Value: Longint);
begin
  while Index < FTokenPositionsList.Count do
  begin
    FTokenPositionsList[Index] := FTokenPositionsList[Index] + Value;
    Inc(Index);
  end
end; { ResetPositionsFrom }

function TBCBTokenList.GetToken(Index: Integer): string;
var
  StartPos, EndPos, StringLen: Longint;
begin
  StartPos := FTokenPositionsList[Index];
  EndPos := FTokenPositionsList[Index + 1];
  StringLen := EndPos - StartPos;
  SetString(Result, (FOrigin + StartPos), StringLen);
end; { GetToken }

function TBCBTokenList.GetTokenPosition(Index: Integer): Longint;
begin
  Result := FTokenPositionsList[Index];
end; { GetTokenPosition }

procedure TBCBTokenList.SetCapacity(NewCapacity: Integer);
begin
  FTokenPositionsList.Capacity := NewCapacity;
end; { SetCapacity }

procedure TBCBTokenList.SetToken(Index: Integer; const Item: string);
var
  StartPos, EndPos, OldLen, NewLen, Diff: Longint;
begin
  StartPos := FTokenPositionsList[Index];
  EndPos := FTokenPositionsList[Index + 1];
  OldLen := EndPos - StartPos;
  NewLen := Length(Item);
  Diff := NewLen - OldLen;
  WriteTo(StartPos, EndPos, Item);
  ResetPositionsFrom(Index + 1, Diff);
end; { SetItems }

function TBCBTokenList.Add(const Item: string): Integer;
var
  StartPos, EndPos: Longint;
begin
  Result := Count;
  StartPos := FTokenPositionsList[Result];
  EndPos := StartPos + Length(Item);
  FTokenPositionsList.Add(EndPos);
  WriteTo(StartPos, StartPos, Item);
end; { Add }

procedure TBCBTokenList.Clear;
begin
  SetCount(0);
  FTokenPositionsList.Capacity := 1;
  Run := 0;
end; { Clear }

procedure TBCBTokenList.Delete(Index: Integer);
var
  StartPos, EndPos, OldLen: Longint;
begin
  StartPos := FTokenPositionsList[Index];
  EndPos := FTokenPositionsList[Index + 1];
  OldLen := EndPos - StartPos;
  WriteTo(StartPos, EndPos, '');
  FTokenPositionsList.Delete(Index);
  ResetPositionsFrom(Index, -OldLen);
end; { Delete }

procedure TBCBTokenList.Exchange(Index1, Index2: Integer);
var
  Item: string;
begin
  Item := GetToken(Index1);
  SetToken(Index1, GetToken(Index2));
  SetToken(Index2, Item);
end; { Exchange }

function TBCBTokenList.First: string;
begin
  Result := GetToken(0);
end; { First }

function TBCBTokenList.IndexOf(const Item: string): Integer;
begin
  Result := 0;
  while (Result < Count) and (GetToken(Result) <> Item) do Inc(Result);
  if Result = Count then Result := -1;
end; { IndexOf }

procedure TBCBTokenList.Insert(Index: Integer; const Item: string);
var
  StartPos, EndPos, ItemLen: Longint;
begin
  ItemLen := Length(Item);
  StartPos := FTokenPositionsList[Index];
  EndPos := StartPos + ItemLen;
  WriteTo(StartPos, StartPos, Item);
  ResetPositionsFrom(Index + 1, ItemLen);
  FTokenPositionsList.Insert(Index + 1, EndPos);
end; { Insert }

function TBCBTokenList.Last: string;
begin
  Result := GetToken(Count - 1);
end; { Last }

procedure TBCBTokenList.Move(CurIndex, NewIndex: Integer);
var
  Item: string;
begin
  if CurIndex <> NewIndex then
  begin
    Item := GetToken(CurIndex);
    Delete(CurIndex);
    Insert(NewIndex, Item);
  end;
end; { Move }

function TBCBTokenList.Remove(const Item: string): Integer;
begin
  Result := IndexOf(Item);
  if Result <> -1 then Delete(Result);
end; { Remove }

function TBCBTokenList.GetSubString(StartPos, EndPos: Longint): string;
var
  SubLen: Integer;
begin
  if FOrigin[EndPos] = #10 then Inc(EndPos);
  SubLen := EndPos - StartPos;
  SetString(Result, (FOrigin + StartPos), SubLen);
end; { GetSubString }

procedure TBCBTokenList.SetRunIndex(NewPos: Longint);
begin
  Run := NewPos;
end; { SetRunPos }

procedure TBCBTokenList.HandleComments;
begin
  case FComment of
    csAnsi:
      begin
        while FOrigin[Run] <> #0 do
        begin
          case FOrigin[Run] of
            '*': if FOrigin[Run + 1] = '/' then
              begin
                Inc(Run);
                FComment := csNo;
                Break;
              end;
          end;
          Inc(Run);
        end;
      end;

    csSlashes:
      begin
        while FOrigin[Run] <> #0 do
        begin
          Inc(Run);
          case FOrigin[Run] of
            #0, #10, #13:
              begin
                //if FOrigin[Run +1] = #10 then Inc(Run); //do not Inc(Run), it causes skipping the tkcCrlf token at the end of comment line
                FComment := csNo;
                Break;
              end;
          end;
        end;
      end;
  end;
end; { HandleComments }

function TBCBTokenList.IdentKind(Index: Longint): TCTokenKind;
var
  HashKey: Integer;
  aToken: string;
  StartPos, EndPos, StringLen: Longint;

  function KeyHash: Integer;
  var
    I: Integer;
  begin
    Result := 0;
    for i := 1 to StringLen do
      Result := Result + Ord(aToken[i]);
  end; { KeyHash }
begin
  Result := ctkIdentifier;
  StartPos := FTokenPositionsList[Index];
  EndPos := FTokenPositionsList[Index + 1];
  StringLen := EndPos - StartPos;
  SetString(aToken, (FOrigin + StartPos), StringLen);
  HashKey := KeyHash;
  case HashKey of
    207: if aToken = 'if' then Result := ctkif;
    211: if aToken = 'do' then Result := ctkdo;
    321: if aToken = 'asm' then Result := ctkasm;
    327: if aToken = 'for' then Result := ctkfor;
    330: if aToken = 'new' then Result := ctknew;
    331: if aToken = 'int' then Result := ctkint;
    351: if aToken = 'try' then Result := ctktry;
    412: if aToken = 'case' then Result := ctkcase;
    414: if aToken = 'char' then Result := ctkchar;
    416: if aToken = '_asm' then Result := ctk_asm;
    425: if aToken = 'else' then Result := ctkelse;
    428: if aToken = 'bool' then Result := ctkbool;
    432: if aToken = 'long' then Result := ctklong;
    434: if aToken = 'void' then Result := ctkvoid;
    437: if aToken = 'enum' then Result := ctkenum;
    440: if aToken = 'this' then Result := ctkthis;
    441:
      begin
        if aToken = 'auto' then Result := ctkauto else
          if aToken = 'goto' then Result := ctkgoto;
      end;
    448: if aToken = 'true' then Result := ctktrue;
    507: if aToken = 'cdecl' then Result := ctkcdecl;
    511: if aToken = '__asm' then Result := ctk__asm;
    515: if aToken = 'catch' then Result := ctkcatch;
    517: if aToken = 'break' then Result := ctkbreak;
    523: if aToken = 'false' then Result := ctkfalse;
    534:
      begin
        if aToken = 'float' then Result := ctkfloat else
          if aToken = 'class' then Result := ctkclass;
      end;
    537: if aToken = 'while' then Result := ctkwhile;
    538: if aToken = '_M_IX86' then Result := ctk_M_IX86;
    541:
      begin
        if aToken = '__MT__' then Result := ctk__MT__ else
          if aToken = '__try' then Result := ctk__try;
      end;
    550: if aToken = 'using' then Result := ctkusing;
    551: if aToken = 'const' then Result := ctkconst;
    553: if aToken = 'union' then Result := ctkunion;
    560: if aToken = 'short' then Result := ctkshort;
    564: if aToken = 'throw' then Result := ctkthrow;
    577: if aToken = '__int8' then Result := ctk__int8;
    600: if aToken = '__DLL__' then Result := ctk__DLL__;
    602: if aToken = '_cdecl' then Result := ctk_cdecl;
    622: if aToken = '__int32' then Result := ctk__int32;
    623: if aToken = '__TLS__' then Result := ctk__TLS__;
    624: if aToken = '__int16' then Result := ctk__int16;
    627:
      begin
        if aToken = 'delete' then Result := ctkdelete else
          if aToken = '__int64' then Result := ctk__int64;
      end;
    628: if aToken = 'pascal' then Result := ctkpascal;
    632: if aToken = 'friend' then Result := ctkfriend;
    634: if aToken = 'signed' then Result := ctksigned;
    635: if aToken = 'double' then Result := ctkdouble;
    639:
      begin
        if aToken = 'public' then Result := ctkpublic else
          if aToken = 'inline' then Result := ctkinline;
      end;
    641: if aToken = '__rtti' then Result := ctk__rtti;
    647: if aToken = '_WCHAR_T' then Result := ctk_WCHAR_T;
    648: if aToken = 'static' then Result := ctkstatic;
    655: if aToken = 'typeid' then Result := ctktypeid;
    656: if aToken = 'sizeof' then Result := ctksizeof;
    658: if aToken = 'switch' then Result := ctkswitch;
    662: if aToken = 'extern' then Result := ctkextern;
    666: if aToken = '__DATE__' then Result := ctk__DATE__;
    668: if aToken = '__FILE__' then Result := ctk__FILE__;
    672: if aToken = 'return' then Result := ctkreturn;
    676: if aToken = '__LINE__' then Result := ctk__LINE__;
    677: if aToken = 'struct' then Result := ctkstruct;
    682: if aToken = '__STDC__' then Result := ctk__STDC__;
    683: if aToken = '__TIME__' then Result := ctk__TIME__;
    697: if aToken = '__cdecl' then Result := ctk__cdecl;
    719: if aToken = '__WIN32__' then Result := ctk__WIN32__;
    723: if aToken = '_pascal' then Result := ctk_pascal;
    727: if aToken = '__CDECL__' then Result := ctk__CDECL__;
    741: if aToken = 'default' then Result := ctkdefault;
    744: if aToken = 'wchar_t' then Result := ctkwchar_t;
    746: if aToken = 'mutable' then Result := ctkmutable;
    753: if aToken = 'typedef' then Result := ctktypedef;
    756: if aToken = '__BCOPT__' then Result := ctk__BCOPT__;
    762: if aToken = '_import' then Result := ctk_import;
    763: if aToken = 'private' then Result := ctkprivate;
    769: if aToken = '_export' then Result := ctk_export;
    770: if aToken = '__MSDOS__' then Result := ctk__MSDOS__;
    775: if aToken = 'virtual' then Result := ctkvirtual;
    791: if aToken = '_CPPUNWIND' then Result := ctk_CPPUNWIND;
    816: if aToken = '__PASCAL__' then Result := ctk__PASCAL__;
    818: if aToken = '__pascal' then Result := ctk__pascal;
    822: if aToken = '__thread' then Result := ctk__thread;
    827: if aToken = '__dispid' then Result := ctk__dispid;
    838: if aToken = '_stdcall' then Result := ctk_stdcall;
    839: if aToken = '__except' then Result := ctk__except;
    842: if aToken = '_Windows' then Result := ctk_Windows;
    843: if aToken = '__TURBOC__' then Result := ctk__TURBOC__;
    857: if aToken = '__import' then Result := ctk__import;
    860: if aToken = 'template' then Result := ctktemplate;
    861: if aToken = 'unsigned' then Result := ctkunsigned;
    864:
      begin
        if aToken = 'volatile' then Result := ctkvolatile else
          if aToken = '__export' then Result := ctk__export;
      end;
    866: if aToken = 'explicit' then Result := ctkexplicit;
    867: if aToken = 'typename' then Result := ctktypename;
    869:
      begin
        if aToken = 'continue' then Result := ctkcontinue else
          if aToken = 'register' then Result := ctkregister;
      end;
    876: if aToken = 'operator' then Result := ctkoperator;
    911: if aToken = '__CONSOLE__' then Result := ctk__CONSOLE__;
    929: if aToken = '__classid' then Result := ctk__classid;
    933: if aToken = '__stdcall' then Result := ctk__stdcall;
    937: if aToken = '_fastcall' then Result := ctk_fastcall;
    941:
      begin
        if aToken = 'namespace' then Result := ctknamespace else
          if aToken = '__finally' then Result := ctk__finally;
      end;
    955: if aToken = '__closure' then Result := ctk__closure;
    961: if aToken = '__BORLANDC__' then Result := ctk__BORLANDC__;
    970: if aToken = 'protected' then Result := ctkprotected;
    1025: if aToken = '__declspec' then Result := ctk__declspec;
    1032: if aToken = '__fastcall' then Result := ctk__fastcall;
    1067: if aToken = '__TEMPLATES__' then Result := ctk__TEMPLATES__;
    1073: if aToken = 'const_cast' then Result := ctkconst_cast;
    1081: if aToken = '_CHAR_UNSIGNED' then Result := ctk_CHAR_UNSIGNED;
    1091: if aToken = '__property' then Result := ctk__property;
    1150: if aToken = '__published' then Result := ctk__published;
    1154: if aToken = '__automated' then Result := ctk__automated;
    1161: if aToken = '__BCPLUSPLUS__' then Result := ctk__BCPLUSPLUS__;
    1170: if aToken = 'static_cast' then Result := ctkstatic_cast;
    1179: if aToken = '__TCPLUSPLUS__' then Result := ctk__TCPLUSPLUS__;
    1193: if aToken = '__cplusplus' then Result := ctk__cplusplus;
    1237: if aToken = '_WCHAR_T_DEFINED' then Result := ctk_WCHAR_T_DEFINED;
    1263: if aToken = 'dynamic_cast' then Result := ctkdynamic_cast;
    1726: if aToken = 'reinterpret_cast' then Result := ctkreinterpret_cast;
  end;
end; { IdentKind }

function TBCBTokenList.DirKind(StartPos, EndPos: Longint): TCTokenKind;
var
  HashKey: Integer;
  aToken: string;
  StringLen: Longint;

  function KeyHash: Integer;
  var
    I: Integer;
  begin
    Result := 0;
    for i := 1 to StringLen do
      Result := Result + Ord(aToken[i]);
  end; { KeyHash }
begin
  Result := ctkunknown;
  StringLen := EndPos - StartPos;
  SetString(aToken, (FOrigin + StartPos), StringLen);
  HashKey := KeyHash;
  case HashKey of
    207: if aToken = 'if' then Result := ctkdirif;
    416: if aToken = 'elif' then Result := ctkdirelif;
    424: if aToken = 'line' then Result := ctkdirline;
    425: if aToken = 'else' then Result := ctkdirelse;
    510: if aToken = 'ifdef' then Result := ctkdirifdef;
    518: if aToken = 'endif' then Result := ctkdirendif;
    530: if aToken = 'undef' then Result := ctkdirundef;
    554: if aToken = 'error' then Result := ctkdirerror;
    619: if aToken = 'define' then Result := ctkdirdefine;
    620: if aToken = 'ifndef' then Result := ctkdirifndef;
    632: if aToken = 'pragma' then Result := ctkdirpragma;
    740: if aToken = 'include' then Result := ctkdirinclude;
  end;
end; { IdentKind }

procedure TBCBTokenList.Tokenize;
var
  BackSlashCount: Integer;
begin
  BackSlashCount := 0;
  Clear;
  while FOrigin[Run] <> #0 do
  begin
    case FOrigin[Run] of
      #10:
        begin
          Inc(Run);
          FTokenPositionsList.Add(Run);
        end;

      #13:
        begin
          if FOrigin[Run + 1] = #10 then Inc(Run, 2) else Inc(Run);
          FTokenPositionsList.Add(Run);
        end;

      #1..#9, #11, #12, #14..#32:
        begin
          Inc(Run);
          while CharInSet(FOrigin[Run], [#1..#9, #11, #12, #14..#32]) do Inc(Run);
          FTokenPositionsList.Add(Run);
        end;

      'A'..'Z', 'a'..'z', '_', '~':
        begin
          Inc(Run);
          while CharInSet(FOrigin[Run], ['A'..'Z', 'a'..'z', '0'..'9', '_']) do Inc(Run);
          FTokenPositionsList.Add(Run);
        end;

      '0'..'9':
        begin
          Inc(Run);
          while CharInSet(FOrigin[Run], ['0'..'9', '.', 'e', 'E']) do
          begin
            case FOrigin[Run] of
              '.':
                if FOrigin[Run + 1] = '.' then Break;
            end;
            Inc(Run);
          end;
          FTokenPositionsList.Add(Run);
        end;

      '!'..'#', '%', '&', '('..'/', ':'..'@', '['..'^', '`', '{'..'}':
        begin
          case FOrigin[Run] of

            '!':
              case FOrigin[Run + 1] of
                '=': Inc(Run);
              end;

            '"':
              begin
                repeat
                  if FOrigin[Run] = '\' then
                    Inc(BackSlashCount)
                  else
                    BackSlashCount := 0;
                  case FOrigin[Run] of
                    #0: Break;
                  end;
                  Inc(Run);
                until ((FOrigin[Run] = '"') and not Odd(BackSlashCount));
              end; //do not treat \" or \\\" etc. as the end of string, this is escape sequence

            '#':
              case FOrigin[Run + 1] of
                '#': Inc(Run);
                'A'..'Z', 'a'..'z':
                  if FDirectivesAsComments then
                  begin
                    Inc(Run);
                    repeat
                      Inc(Run);
                      if (FOrigin[Run] = '\') then
                      begin
                        while not IsCharLineEndingOrNull(FOrigin[Run + 1]) do
                          Inc(Run);
                        Run := Run + 2;
                      end;
                    until IsCharLineEndingOrNull(FOrigin[Run + 1]);
                    Run := Run + 2;
                  end
                  else
                  begin
                    Inc(Run);
                    while CharInSet(FOrigin[Run], ['A'..'Z', 'a'..'z']) do Inc(Run);
                    Dec(Run)
                  end;
              end;

            '%':
              case FOrigin[Run + 1] of
                '=': Inc(Run);
              end;

            '&':
              case FOrigin[Run + 1] of
                '=', '&': Inc(Run);
              end;

            '(':
              case FOrigin[Run + 1] of
                ')': Inc(Run);
              end;

            '*':
              case FOrigin[Run + 1] of
                '*', '/', '=': Inc(Run);
              end;

            '+':
              case FOrigin[Run + 1] of
                '+', '=': Inc(Run);
              end;

            '-':
              case FOrigin[Run + 1] of
                '-', '=': Inc(Run);
                '>':
                  case FOrigin[Run + 2] of
                    '*': Inc(Run, 2);
                  else Inc(Run);
                  end;
              end;

            '.':
              case FOrigin[Run + 1] of
                '*': Inc(Run);
                '.':
                  case FOrigin[Run + 2] of
                    '.': Inc(Run, 2);
                  else Inc(Run);
                  end;
              end;

            '/':
              case FOrigin[Run + 1] of
                '*':
                  begin
                    FComment := csAnsi;
                    HandleComments;
                  end;
                '/':
                  begin
                    FComment := csSlashes;
                    HandleComments;
                  end;
              end;

            ':':
              case FOrigin[Run + 1] of
                ':': Inc(Run);
              end;

            '<':
              case FOrigin[Run + 1] of
                '=': Inc(Run);
                '<':
                  case FOrigin[Run + 2] of
                    '=': Inc(Run, 2);
                  else Inc(Run);
                  end;
              end;

            '=':
              case FOrigin[Run + 1] of
                '=': Inc(Run);
              end;

            '>':
              case FOrigin[Run + 1] of
                '=': Inc(Run);
                '>':
                  case FOrigin[Run + 2] of
                    '=': Inc(Run, 2);
                  else Inc(Run);
                  end;
              end;

            '?':
              case FOrigin[Run + 1] of
                ':': Inc(Run);
              end;

            '[':
              case FOrigin[Run + 1] of
                ']': Inc(Run);
              end;

            '^':
              case FOrigin[Run + 1] of
                '=': Inc(Run);
              end;

            '{':
              case FOrigin[Run + 1] of
                '}': Inc(Run);
              end;

            '|':
              case FOrigin[Run + 1] of
                '=', '|': Inc(Run);
              end;

            '\':
              case FOrigin[Run + 1] of
                #10, #13:
                  begin
                    Inc(Run);
                    case FOrigin[Run + 1] of
                      #10, #13: Inc(Run);
                    end;
                  end;
              end; // Continuation on the next line

          end;
          Inc(Run);
          FTokenPositionsList.Add(Run);
        end;

      #39:
        begin
          if FOrigin[Run + 2] = #39 then // this is char type ... 'a'
          begin
            Run := Run + 3;
            FTokenPositionsList.Add(Run);
          end
          else if (FOrigin[Run + 1] = '\') and (FOrigin[Run + 3] = #39) then  // this is for example tab escape ... '\t'
          begin
            Run := Run + 4;
            FTokenPositionsList.Add(Run);
          end
          else
          begin
            Inc(Run); //this is apostrophe ... #error Can't do something
            FTokenPositionsList.Add(Run);
          end;
        end

    else
      begin
        Inc(Run);
        FTokenPositionsList.Add(Run);
      end;
    end;
  end;
end; {Tokenize}

function TBCBTokenList.GetTokenID(Index: Longint): TCTokenKind;
var
  Running, TempRun: Longint;
begin
  Result := ctkUnknown;
  Running := FTokenPositionsList[Index];
  case FOrigin[Running] of
    #0: Result := ctknull;

    #10: Result := ctkcrlf;

    #13: Result := ctkcrlf;

    #1..#9, #11, #12, #14..#32: Result := ctkspace;

    'A'..'Z', 'a'..'z', '_', '~': Result := IdentKind(Index);

    '0'..'9':
      begin
        Inc(Running);
        Result := ctknumber;
        while CharInSet(FOrigin[Running], ['0'..'9', '.']) do
        begin
          case FOrigin[Running] of
            '.':
              if FOrigin[Running + 1] <> '.' then Result := ctkfloat else Break;
          end;
          Inc(Running);
        end;
      end;

    '{':
      if FOrigin[Running + 1] = '}' then Result := ctkbracepair
      else
      begin
        Result := ctkbraceopen;
        Inc(FBraceCount);
      end;

    '}':
      begin
        Result := ctkbraceclose;
        Dec(FBraceCount);
      end;

    '!'..'#', '%', '&', '('..'/', ':'..'@', '['..'^', '`':
      begin
        case FOrigin[Running] of

          '!':
            case FOrigin[Running + 1] of
              '=': Result := ctknotequal;
            else Result := ctknegation;
            end;

          '"': Result := ctkstring;

          '#':
            case FOrigin[Running + 1] of
              '#': Result := ctkblend;
              'A'..'Z', 'a'..'z':
                begin
                  if FDirectivesAsComments then
                    Result := ctkslashescomment
                  else
                  begin
                    Inc(Running);
                    TempRun := Running;
                    while CharInSet(FOrigin[Running], ['A'..'Z', 'a'..'z']) do Inc(Running);
                    Result := DirKind(TempRun, Running);
                  end;
                end;
            else Result := ctkdirnull;
            end;

          '%':
            case FOrigin[Running + 1] of
              '=': Result := ctkassignmodulus;
            else Result := ctkmodulus;
            end;

          '&':
            case FOrigin[Running + 1] of
              '&': Result := ctklogicand;
              '=': Result := ctkassignbitand;
            else Result := ctkbitand;
            end;

          '(':
            case FOrigin[Running + 1] of
              ')': Result := ctkroundpair;
            else
              begin
                Result := ctkroundopen;
                Inc(fRoundCount);
              end;
            end;

          ')':
            begin
              Result := ctkroundclose;
              Dec(fRoundCount);
            end;

          '*':
            case FOrigin[Running + 1] of
              '*': Result := ctkstatstar;
              '=': Result := ctkassignmultible;
            else Result := ctkstar
            end;

          '+':
            case FOrigin[Running + 1] of
              '+': Result := ctkplusplus;
              '=': Result := ctkassignplus;
            else Result := ctkplus;
            end;

          ',': Result := ctkcomma;

          '-':
            case FOrigin[Running + 1] of
              '-': Result := ctkminusminus;
              '=': Result := ctkassignminus;
              '>':
                case FOrigin[Running + 2] of
                  '*': Result := ctkderefpointerpointer;
                else Result := ctkselectelement;
                end;
            else Result := ctkminus;
            end;

          '.':
            case FOrigin[Running + 1] of
              '*': Result := ctkderefpointer;
              '.':
                case FOrigin[Running + 2] of
                  '.': Result := ctkthreepoint;
                else Result := ctkpointpoint;
                end;
            else Result := ctkpoint;
            end;

          '/':
            case FOrigin[Running + 1] of
              '*': Result := ctkansicomment;
              '/': Result := ctkslashescomment
            else Result := ctkslash;
            end;

          ':':
            case FOrigin[Running + 1] of
              ':': Result := ctkcoloncolon;
            else Result := ctkcolon;
            end;

          ';': Result := ctksemicolon;

          '<':
            case FOrigin[Running + 1] of
              '=': Result := ctklowerequal;
              '<':
                case FOrigin[Running + 2] of
                  '=': Result := ctkassignshiftleft;
                else Result := ctkshiftleft;
                end;
            else Result := ctklower;
            end;

          '=':
            case FOrigin[Running + 1] of
              '=': Result := ctkequal;
            else Result := ctkassignment;
            end;

          '>':
            case FOrigin[Running + 1] of
              '=': Result := ctkgreaterequal;
              '>':
                case FOrigin[Running + 2] of
                  '=': Result := ctkassignshiftright;
                else Result := ctkshiftright;
                end;
            else Result := ctkgreater;
            end;

          '?':
            case FOrigin[Running + 1] of
              ':': Result := ctkconditional;
            else Result := ctksymbol;
            end;

          '[':
            case FOrigin[Running + 1] of
              ']': Result := ctksquarepair;
            else
              begin
                Result := ctksquareopen;
                Inc(fSquareCount);
              end;
            end;

          ']':
            begin
              Result := ctksquareclose;
              Dec(fSquareCount);
            end;

          '^':
            case FOrigin[Running + 1] of
              '=': Result := ctkexcbitor;
            else Result := ctkassignexcbitor;
            end;

          '{':
            case FOrigin[Running + 1] of
              '}': Result := ctkbracepair;
            else
              begin
                Result := ctkbraceopen;
                Inc(FBraceCount)
              end;
            end;

          '}':
            begin
              Result := ctkbraceclose;
              Dec(FBraceCount)
            end;

          '|':
            case FOrigin[Running + 1] of
              '=': Result := ctkassignincbitor;
              '|': Result := ctklogicor;
            else Result := ctkincbitor;
            end;

          '\':
            case FOrigin[Running + 1] of
              #10, #13: Result := ctknextline;
            end; // Continuation on the next line

        else Result := ctksymbol;
        end;
      end;

    #39: if (FOrigin[Running + 2] = #39) or ((FOrigin[Running + 1] = '\') and (FOrigin[Running + 3] = #39)) then
           Result := ctkCharType;
         else
           Result := ctkapostrophe;
  end;
end; { GetTokenID }

procedure TBCBTokenList.Next;
begin
  if Run < Count - 1 then Inc(Run);
end; { Next }

procedure TBCBTokenList.Previous;
begin
  if Run > 0 then Dec(Run);
end; { Previous }

procedure TBCBTokenList.NextID(ID: TCTokenKind);
begin
  repeat
    case TokenID[Run] of
      ctknull: Break;
    else Inc(Run);
    end;
  until TokenID[Run] = ID;
end; { NextID }

function TBCBTokenList.GetIsJunk: Boolean;
begin
  case TokenID[Run] of
    ctkansicomment, ctkcrlf, ctkslashescomment, ctkspace:
      Result := True;
  else Result := False;
  end;
end;

procedure TBCBTokenList.NextNonComment;
begin
  repeat
    case TokenID[Run] of
      ctknull: Break;
    else Inc(Run);
    end;
  until not (TokenID[Run] in [ctkansicomment, ctkslashescomment]);
end; { NextNonComCRLF }

procedure TBCBTokenList.NextNonJunk;
begin
  repeat
    case TokenID[Run] of
      ctknull: Break;
    else Inc(Run);
    end;
  until not (TokenID[Run] in [ctkansicomment, ctkcrlf,
    ctkslashescomment, ctkspace]);
end; { NextNonJunk }

procedure TBCBTokenList.NextNonSpace;
begin
  repeat
    case TokenID[Run] of
      ctknull: Break;
    else Inc(Run);
    end;
  until not (TokenID[Run] = ctkspace);
end; { NextNonSpace }

procedure TBCBTokenList.ToLineStart;
begin
  while TokenID[Run] <> ctkcrlf do
  begin
    if Run <= 0 then Break;
    Dec(Run);
  end;
  Inc(Run);
end; { ToLineStart }

procedure TBCBTokenList.PreviousID(ID: TCTokenKind);
begin
  repeat
    case Run of
      0: Break;
    else Dec(Run);
    end;
  until TokenID[Run] = ID;
end; { PreviousID }

procedure TBCBTokenList.PreviousNonComment;
begin
  repeat
    case Run of
      0: Break;
    else Dec(Run);
    end;
  until not (TokenID[Run] in [ctkansicomment, ctkslashescomment]);
end; { PreviousNonComment }


procedure TBCBTokenList.PreviousNonJunk;
begin
  repeat
    case Run of
      0: Break;
    else Dec(Run);
    end;
  until not (TokenID[Run] in [ctkansicomment, ctkcrlf,
    ctkslashescomment, ctkspace]);
end; { PreviousNonJunk }

procedure TBCBTokenList.PreviousNonSpace;
begin
  repeat
    case Run of
      0: Break;
    else Dec(Run);
    end;
  until not (TokenID[Run] = ctkspace);
end; { PreviousNonSpace }

function TBCBTokenList.PositionAtLine(aPosition: Longint): Longint;
var
  Running: Longint;
  LastPos: Longint;
begin
  LastPos := FTokenPositionsList[FTokenPositionsList.Count - 1];
  if (aPosition < 0) or (aPosition > LastPos) then
    Result := -1
  else
  begin
    Running := 1;
    Result := 1;
    while (Running <= aPosition) and (Running < LastPos) do
    begin
      case FOrigin[Running - 1] of
        #13: if FOrigin[Running] <> #10 then Inc(Result);
        #10: Inc(Result);
      end;
      Inc(Running);
    end;
  end;
end;

function TBCBTokenList.IndexAtLine(anIndex: Longint): Longint;
begin
  Result := PositionAtLine(TokenPosition[anIndex]);
end;

function TBCBTokenList.GetRunID: TCTokenKind;
begin
  Result := GetTokenID(Run);
end; { GetRunID }

function TBCBTokenList.GetRunPosition: Longint;
begin
  Result := FTokenPositionsList[Run];
end; { GetRunPosition }

function TBCBTokenList.GetRunToken: string;
var
  StartPos, EndPos, StringLen: Longint;
begin
  StartPos := FTokenPositionsList[Run];
  EndPos := FTokenPositionsList[Run + 1];
  StringLen := EndPos - StartPos;
  SetString(Result, (FOrigin + StartPos), StringLen);
end; { GetRunToken }

function TBCBTokenList.PositionToIndex(aPosition: Longint): Longint;
var
  First, Last, I: Longint;
begin
  Result := -1;
  I := 0;
  if (aPosition >= 0) and (aPosition <= fPCharSize) then
  begin
    First := 0;
    Last := FTokenPositionsList.Count - 2;
    while First <= Last do
    begin
      I := (First + Last) shr 1;
      if aPosition < FTokenPositionsList[I] then Last := I - 1 else
      begin
        if aPosition < FTokenPositionsList[I + 1] then Break;
        First := I + 1;
      end;
    end;
    Result := I;
  end;
end; { PositionToIndex }

end.

