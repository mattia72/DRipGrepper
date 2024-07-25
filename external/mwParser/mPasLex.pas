{+--------------------------------------------------------------------------+
 | Class:       TmwPasLex
 | Created:     07.98 - 10.98
 | Author:      Martin Waldenburg
 | Description: A very fast Pascal tokenizer.
 | Version:     1.32
 | Copyright (c) 1998, 1999 Martin Waldenburg
 | All rights reserved.
 |
 | LICENCE CONDITIONS
 |
 | USE OF THE ENCLOSED SOFTWARE
 | INDICATES YOUR ASSENT TO THE
 | FOLLOWING LICENCE CONDITIONS.
 |
 |
 |
 | These Licence Conditions are exlusively
 | governed by the Law and Rules of the
 | Federal Republic of Germany.
 |
 | Redistribution and use in source and binary form, with or without
 | modification, are permitted provided that the following conditions
 | are met:
 |
 | 1. Redistributions of source code must retain the above copyright
 |    notice, this list of conditions and the following disclaimer.
 |    If the source is modified, the complete original and unmodified
 |    source code has to distributed with the modified version.
 |
 | 2. Redistributions in binary form must reproduce the above
 |    copyright notice, these licence conditions and the disclaimer
 |    found at the end of this licence agreement in the documentation
 |    and/or other materials provided with the distribution.
 |
 | 3. Software using this code must contain a visible line of credit.
 |
 | 4. If my code is used in a "for profit" product, you have to donate
 |    to a registered charity in an amount that you feel is fair.
 |    You may use it in as many of your products as you like.
 |    Proof of this donation must be provided to the author of
 |    this software.
 |
 | 5. If you for some reasons don't want to give public credit to the
 |    author, you have to donate three times the price of your software
 |    product, or any other product including this component in any way,
 |    but no more than $500 US and not less than $200 US, or the
 |    equivalent thereof in other currency, to a registered charity.
 |    You have to do this for every of your products, which uses this
 |    code separately.
 |    Proof of this donations must be provided to the author of
 |    this software.
 |
 |
 | DISCLAIMER:
 |
 | THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS'.
 |
 | ALL EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
 | THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 | PARTICULAR PURPOSE ARE DISCLAIMED.
 |
 | IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 | INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 | (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 | OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 | INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
 | WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 | NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 | THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 |
 |  Martin.Waldenburg@T-Online.de
 +--------------------------------------------------------------------------+}

unit mPasLex;

{$I GX_CondDefine.inc}

interface

uses
  Classes, Contnrs, mwPasParserTypes;

var
  Identifiers: array[#0..#127] of ByteBool;
  mHashTable: array[#0..#127] of Integer;

type
  TCommentState = (csAnsi, csBor, csNo);
  TLexArgList = class;

  ///<summary>
  /// Used to temporarily store the Lexer's state so it can be reset to that point later </summary>
  TmwPasLexState = record
    FComment: TCommentState;
    Run: Integer;
    FRoundCount: Integer;
    FSquareCount: Integer;
    fStringLen: Integer;
    fToIdent: PChar;
    fTokenPos: Integer;
    fLineNumber: Integer;
    FTokenID: TTokenKind;
    fLastIdentPos: Integer;
    fLastNoSpace: TTokenKind;
    fLastNoSpacePos: Integer;
    fLinePos: Integer;
    fIsInterface: Boolean;
    fIsClass: Boolean;
  end;

  TmwPasLex = class(TObject)
  private
    fComment: TCommentState;
    fOrigin: PChar;
    fProcTable: array[#0..#255] of procedure of object;
    Run: Integer;
    FRoundCount: Integer;
    FSquareCount: Integer;
    fStringLen: Integer;
    fToIdent: PChar;
    fIdentFuncTable: array[0..191] of function: TTokenKind of object;
    fTokenPos: Integer;
    fLineNumber: Integer;
    FTokenID: TTokenKind;
    fLastIdentPos: Integer;
    fLastNoSpace: TTokenKind;
    fLastNoSpacePos: Integer;
    fLinePos: Integer;
    fIsInterface: Boolean;
    fIsClass: Boolean;
    function KeyHash(ToHash: PChar): Integer;
    function KeyComp(const aKey: string): Boolean;
    function Func15: TTokenKind;
    function Func19: TTokenKind;
    function Func20: TTokenKind;
    function Func21: TTokenKind;
    function Func23: TTokenKind;
    function Func25: TTokenKind;
    function Func27: TTokenKind;
    function Func28: TTokenKind;
    function Func29: TTokenKind;
    function Func32: TTokenKind;
    function Func33: TTokenKind;
    function Func35: TTokenKind;
    function Func37: TTokenKind;
    function Func38: TTokenKind;
    function Func39: TTokenKind;
    function Func40: TTokenKind;
    function Func41: TTokenKind;
    function Func44: TTokenKind;
    function Func45: TTokenKind;
    function Func47: TTokenKind;
    function Func49: TTokenKind;
    function Func52: TTokenKind;
    function Func54: TTokenKind;
    function Func55: TTokenKind;
    function Func56: TTokenKind;
    function Func57: TTokenKind;
    function Func59: TTokenKind;
    function Func60: TTokenKind;
    function Func61: TTokenKind;
    function Func63: TTokenKind;
    function Func64: TTokenKind;
    function Func65: TTokenKind;
    function Func66: TTokenKind;
    function Func69: TTokenKind;
    function Func71: TTokenKind;
    function Func73: TTokenKind;
    function Func75: TTokenKind;
    function Func76: TTokenKind;
    function Func79: TTokenKind;
    function Func81: TTokenKind;
    function Func84: TTokenKind;
    function Func85: TTokenKind;
    function Func87: TTokenKind;
    function Func88: TTokenKind;
    function Func91: TTokenKind;
    function Func92: TTokenKind;
    function Func94: TTokenKind;
    function Func95: TTokenKind;
    function Func96: TTokenKind;
    function Func97: TTokenKind;
    function Func98: TTokenKind;
    function Func99: TTokenKind;
    function Func100: TTokenKind;
    function Func101: TTokenKind;
    function Func102: TTokenKind;
    function Func103: TTokenKind;
    function Func105: TTokenKind;
    function Func106: TTokenKind;
    function Func108: TTokenKind;
    function Func112: TTokenKind;
    function Func117: TTokenKind;
    function Func126: TTokenKind;
    function Func129: TTokenKind;
    function Func132: TTokenKind;
    function Func133: TTokenKind;
    function Func136: TTokenKind;
    function Func141: TTokenKind;
    function Func143: TTokenKind;
    function Func166: TTokenKind;
    function Func168: TTokenKind;
    function Func191: TTokenKind;
    function AltFunc: TTokenKind;
    procedure InitIdent;
    function IdentKind(MayBe: PChar): TTokenKind;
    procedure SetOrigin(NewValue: PChar);
    procedure MakeMethodTables;
    procedure AddressOpProc;
    procedure AsciiCharProc;
    procedure AnsiProc;
    procedure BorProc;
    procedure BraceCloseProc;
    procedure BraceOpenProc;
    procedure ColonProc;
    procedure CommaProc;
    procedure CRProc;
    procedure EqualProc;
    procedure GreaterProc;
    procedure IdentProc;
    procedure IntegerProc;
    procedure LFProc;
    procedure LowerProc;
    procedure MinusProc;
    procedure NullProc;
    procedure NumberProc;
    procedure PlusProc;
    procedure PointerSymbolProc;
    procedure PointProc;
    procedure RoundCloseProc;
    procedure RoundOpenProc;
    procedure SemiColonProc;
    procedure SlashProc;
    procedure SpaceProc;
    procedure SquareCloseProc;
    procedure SquareOpenProc;
    procedure StarProc;
    procedure StringProc;
    procedure SymbolProc;
    procedure UnknownProc;
    function GetToken: string;
    function InSymbols(aChar: Char): Boolean;
    procedure doProcTable(AChar: Char);
    function IsIdentifier(AChar: Char): boolean;
    function CalcHash(AChar: Char): integer;
    procedure SetLineNumber(const Value: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    function CharAhead(Count: Integer): Char;
    function NextChar: Char;
    ///<summary>
    /// @Returns true, if TokenId <> tkNull </summary>
    function Next: Boolean;
    procedure NextID(ID: TTokenKind);
    ///<summary>
    /// Like Next, but skips whitespace and comments
    /// @Returns true, if TokenId <> tkNull </summary>
    function NextNoJunk: Boolean;
    procedure NextClass;
    property IsClass: Boolean read fIsClass;
    property IsInterface: Boolean read fIsInterface;
    property LastIdentPos: Integer read fLastIdentPos;
    property LastNoSpace: TTokenKind read fLastNoSpace;
    property LastNoSpacePos: Integer read fLastNoSpacePos;
    property LineNumber: Integer read fLineNumber write SetLineNumber;
    property LinePos: Integer read fLinePos;
    property Origin: PChar read fOrigin write SetOrigin;
    property RunPos: Integer read Run;
    property TokenPos: Integer read fTokenPos;
    property Token: string read GetToken;
    property TokenID: TTokenKind read FTokenID;
    function GetDottedIdentifierAtPos(StayAtIdentEnd: Boolean = False): string;
    function GoToNextMethodDefinition: Boolean;
    procedure GetClassMethodDetailsFromCurrentPos(var NestedClasses, ClassName, MethodName, Args, ResultType: string);
    procedure GetClassMethodDetailsFromCurrentPosEx(var NestedClasses,
      ClassName, MethodName, Args, ResultType: string;
      ArgList: TLexArgList);
    procedure GetParsingState(out State: TmwPasLexState);
    procedure SetParsingState(const State: TmwPasLexState);
    // replaces setting RunPos to 0 which was used in GX_UnitPositions
    procedure Restart;
  end;

  TLexArgPassMode = (lapmValue, lapmConst, lapmVar, lapmOut);

  TLexArgItem = class
  public
    LName: string;             // Part of argument declaration before type
    CoreName: string;          // Core argument name
    PassMode: TLexArgPassMode; // Pass mode
    ArgType: string;           // Data Type
    DefaultValue: string;      // Default value
    ArgText: string;           // Full text of argument declaration group
  end;

  TLexArgList = class(TObjectList)
  private
    function GetItem(Index: Integer): TLexArgItem;
  public
    property Items[Index: Integer]: TLexArgItem read GetItem; default;
  end;

const
  PossibleIdentifiers = [tkIdentifier, tkRegister];

implementation

uses
  SysUtils,
  {$IFDEF GX_VER200_up} Character, {$ENDIF}
  GX_GenericUtils;

const
  SpaceChars = [#1..#9, #11, #12, #14..#32];

procedure MakeIdentTable;
var
  i, J: Char;
begin
  for i := #0 to #127 do
  begin
    case i of
      '_', '0'..'9', 'a'..'z', 'A'..'Z': Identifiers[i] := True;
    else
      Identifiers[i] := False;
    end;
    J := UpperCase(i)[1];
    case i of
      'a'..'z', 'A'..'Z', '_': mHashTable[i] := Ord(J) - 64;
    else
      mHashTable[Char(i)] := 0;
    end;
  end;
end;

procedure TmwPasLex.InitIdent;
var
  i: Integer;
begin
  for i := 0 to 191 do
    case i of
      15: fIdentFuncTable[i] := Func15;
      19: fIdentFuncTable[i] := Func19;
      20: fIdentFuncTable[i] := Func20;
      21: fIdentFuncTable[i] := Func21;
      23: fIdentFuncTable[i] := Func23;
      25: fIdentFuncTable[i] := Func25;
      27: fIdentFuncTable[i] := Func27;
      28: fIdentFuncTable[i] := Func28;
      29: fIdentFuncTable[i] := Func29;
      32: fIdentFuncTable[i] := Func32;
      33: fIdentFuncTable[i] := Func33;
      35: fIdentFuncTable[i] := Func35;
      37: fIdentFuncTable[i] := Func37;
      38: fIdentFuncTable[i] := Func38;
      39: fIdentFuncTable[i] := Func39;
      40: fIdentFuncTable[i] := Func40;
      41: fIdentFuncTable[i] := Func41;
      44: fIdentFuncTable[i] := Func44;
      45: fIdentFuncTable[i] := Func45;
      47: fIdentFuncTable[i] := Func47;
      49: fIdentFuncTable[i] := Func49;
      52: fIdentFuncTable[i] := Func52;
      54: fIdentFuncTable[i] := Func54;
      55: fIdentFuncTable[i] := Func55;
      56: fIdentFuncTable[i] := Func56;
      57: fIdentFuncTable[i] := Func57;
      59: fIdentFuncTable[i] := Func59;
      60: fIdentFuncTable[i] := Func60;
      61: fIdentFuncTable[i] := Func61;
      63: fIdentFuncTable[i] := Func63;
      64: fIdentFuncTable[i] := Func64;
      65: fIdentFuncTable[i] := Func65;
      66: fIdentFuncTable[i] := Func66;
      69: fIdentFuncTable[i] := Func69;
      71: fIdentFuncTable[i] := Func71;
      73: fIdentFuncTable[i] := Func73;
      75: fIdentFuncTable[i] := Func75;
      76: fIdentFuncTable[i] := Func76;
      79: fIdentFuncTable[i] := Func79;
      81: fIdentFuncTable[i] := Func81;
      84: fIdentFuncTable[i] := Func84;
      85: fIdentFuncTable[i] := Func85;
      87: fIdentFuncTable[i] := Func87;
      88: fIdentFuncTable[i] := Func88;
      91: fIdentFuncTable[i] := Func91;
      92: fIdentFuncTable[i] := Func92;
      94: fIdentFuncTable[i] := Func94;
      95: fIdentFuncTable[i] := Func95;
      96: fIdentFuncTable[i] := Func96;
      97: fIdentFuncTable[i] := Func97;
      98: fIdentFuncTable[i] := Func98;
      99: fIdentFuncTable[i] := Func99;
      100: fIdentFuncTable[i] := Func100;
      101: fIdentFuncTable[i] := Func101;
      102: fIdentFuncTable[i] := Func102;
      103: fIdentFuncTable[i] := Func103;
      105: fIdentFuncTable[i] := Func105;
      106: fIdentFuncTable[i] := Func106;
      108: fIdentFuncTable[i] := Func108;
      112: fIdentFuncTable[i] := Func112;
      117: fIdentFuncTable[i] := Func117;
      126: fIdentFuncTable[i] := Func126;
      129: fIdentFuncTable[i] := Func129;
      132: fIdentFuncTable[i] := Func132;
      133: fIdentFuncTable[i] := Func133;
      136: fIdentFuncTable[i] := Func136;
      141: fIdentFuncTable[i] := Func141;
      143: fIdentFuncTable[i] := Func143;
      166: fIdentFuncTable[i] := Func166;
      168: fIdentFuncTable[i] := Func168;
      191: fIdentFuncTable[i] := Func191;
    else fIdentFuncTable[i] := AltFunc;
    end;
end;

function TmwPasLex.CalcHash(AChar: Char): integer;
begin
  if Ord(AChar) <=127 then
    Result := mHashTable[AChar]
  else
    Result := 0;
end;

function TmwPasLex.KeyHash(ToHash: PChar): Integer;
begin
  Result := 0;
  while CharInSet(ToHash^, ['a'..'z', 'A'..'Z']) do
  begin
    if Ord(ToHash^) <=127 then
      Inc(Result, CalcHash(ToHash^));
    Inc(ToHash);
  end;
  if CharInSet(ToHash^, ['_', '0'..'9']) then Inc(ToHash);
  fStringLen := ToHash - fToIdent;
end; { KeyHash }

function TmwPasLex.KeyComp(const aKey: string): Boolean;
var
  i: Integer;
  Temp: PChar;
begin
  Temp := fToIdent;
  if Length(aKey) = fStringLen then
  begin
    Result := True;
    for i := 1 to fStringLen do
    begin
      if CalcHash(Temp^) <> CalcHash(aKey[i]) then
      begin
        Result := False;
        Break;
      end;
      Inc(Temp);
    end;
  end else Result := False;
end; { KeyComp }

function TmwPasLex.Func15: TTokenKind;
begin
  if KeyComp('If') then Result := tkIf else Result := tkIdentifier;
end;

function TmwPasLex.Func19: TTokenKind;
begin
  if KeyComp('Do') then Result := tkDo else
    if KeyComp('And') then Result := tkAnd else Result := tkIdentifier;
end;

function TmwPasLex.Func20: TTokenKind;
begin
  if KeyComp('As') then Result := tkAs else Result := tkIdentifier;
end;

function TmwPasLex.Func21: TTokenKind;
begin
  if KeyComp('Of') then Result := tkOf else
    if KeyComp('At') then Result := tkAt else Result := tkIdentifier;
end;

function TmwPasLex.Func23: TTokenKind;
begin
  if KeyComp('End') then Result := tkEnd else
    if KeyComp('In') then Result := tkIn else Result := tkIdentifier;
end;

function TmwPasLex.Func25: TTokenKind;
begin
  if KeyComp('Far') then Result := tkFar else Result := tkIdentifier;
end;

function TmwPasLex.Func27: TTokenKind;
begin
  if KeyComp('Cdecl') then Result := tkCdecl else Result := tkIdentifier;
end;

function TmwPasLex.Func28: TTokenKind;
begin
  if KeyComp('Read') then
  begin
    if inSymbols(CharAhead(fStringLen)) then Result := tkIdentifier else
      Result := tkRead
  end else
    if KeyComp('Case') then Result := tkCase else
      if KeyComp('Is') then Result := tkIs else Result := tkIdentifier;
end;

function TmwPasLex.Func29: TTokenKind;
begin
  if KeyComp('On') then Result := tkOn else Result := tkIdentifier;
end;

function TmwPasLex.Func32: TTokenKind;
begin
  if KeyComp('File') then Result := tkFile else
    if KeyComp('Label') then Result := tkLabel else
      if KeyComp('Mod') then Result := tkMod else Result := tkIdentifier;
end;

function TmwPasLex.Func33: TTokenKind;
var
  c: char;
begin
  if KeyComp('Or') then Result := tkOr else
    if KeyComp('Name') then
    begin
      c := CharAhead(fStringLen);
      if (c <> '''') and inSymbols(c) then Result := tkIdentifier else
        Result := tkName
    end else
      if KeyComp('Asm') then Result := tkAsm else Result := tkIdentifier;
end;

function TmwPasLex.Func35: TTokenKind;
begin
  if KeyComp('To') then Result := tkTo else
    if KeyComp('Nil') then Result := tkNil else
      if KeyComp('Div') then Result := tkDiv else Result := tkIdentifier;
end;

function TmwPasLex.Func37: TTokenKind;
begin
  if KeyComp('Begin') then Result := tkBegin else Result := tkIdentifier;
end;

function TmwPasLex.Func38: TTokenKind;
begin
  if KeyComp('Near') then Result := tkNear else Result := tkIdentifier;
end;

function TmwPasLex.Func39: TTokenKind;
begin
  if KeyComp('For') then Result := tkFor else
    if KeyComp('Shl') then Result := tkShl else Result := tkIdentifier;
end;

function TmwPasLex.Func40: TTokenKind;
begin
  if KeyComp('Packed') then Result := tkPacked else Result := tkIdentifier;
end;

function TmwPasLex.Func41: TTokenKind;
begin
  if KeyComp('Else') then Result := tkElse else
    if KeyComp('Var') then Result := tkVar else Result := tkIdentifier;
end;

function TmwPasLex.Func44: TTokenKind;
begin
  if KeyComp('Set') then Result := tkSet else
    if KeyComp('Package') then Result := tkPackage else Result := tkIdentifier;
end;

function TmwPasLex.Func45: TTokenKind;
begin
  if KeyComp('Shr') then Result := tkShr else Result := tkIdentifier;
end;

function TmwPasLex.Func47: TTokenKind;
begin
  if KeyComp('Then') then Result := tkThen else Result := tkIdentifier;
end;

function TmwPasLex.Func49: TTokenKind;
begin
  if KeyComp('Not') then Result := tkNot else Result := tkIdentifier;
end;

function TmwPasLex.Func52: TTokenKind;
begin
  if KeyComp('Raise') then Result := tkRaise else
    if KeyComp('Pascal') then Result := tkPascal else Result := tkIdentifier;
end;

function TmwPasLex.IsIdentifier(AChar: Char): boolean;
begin
  if Ord(AChar)<=127 then
    Result := Identifiers[AChar]
  else
    Result := true;
end;

function TmwPasLex.Func54: TTokenKind;
begin
  if KeyComp('Class') then
  begin
    Result := tkClass;
    if fLastNoSpace = tkEqual then
    begin
      fIsClass := True;
      if IsIdentifier(CharAhead(fStringLen)) then
        fIsClass := False;
    end else fIsClass := False;
  end else Result := tkIdentifier;
end;

function TmwPasLex.Func55: TTokenKind;
begin
  if KeyComp('Object') then Result := tkObject else Result := tkIdentifier;
end;

function TmwPasLex.Func56: TTokenKind;
begin
  if KeyComp('Index') then
  begin
    if inSymbols(CharAhead(fStringLen)) then Result := tkIdentifier else
      Result := tkIndex
  end else
    if KeyComp('Out') then Result := tkOut else
    if KeyComp('Delayed') then Result := tkDelayed else Result := tkIdentifier
end;

function TmwPasLex.Func57: TTokenKind;
begin
  if KeyComp('While') then Result := tkWhile else
    if KeyComp('Goto') then Result := tkGoto else
      if KeyComp('Xor') then Result := tkXor else Result := tkIdentifier;
end;

function TmwPasLex.Func59: TTokenKind;
begin
  if KeyComp('Safecall') then Result := tkSafecall else Result := tkIdentifier;
end;

function TmwPasLex.Func60: TTokenKind;
begin
  if KeyComp('With') then Result := tkWith else Result := tkIdentifier;
end;

function TmwPasLex.Func61: TTokenKind;
begin
  if KeyComp('Dispid') then Result := tkDispid else Result := tkIdentifier;
end;

function TmwPasLex.Func63: TTokenKind;
begin
  if KeyComp('Public') then
  begin
    if inSymbols(CharAhead(fStringLen)) then Result := tkIdentifier else
      Result := tkPublic
  end else
    if KeyComp('Record') then Result := tkRecord else
      if KeyComp('Try') then Result := tkTry else
        if KeyComp('Array') then Result := tkArray else
          if KeyComp('Inline') then Result := tkInline else Result := tkIdentifier;
end;

function TmwPasLex.Func64: TTokenKind;
begin
  if KeyComp('Uses') then Result := tkUses else
    if KeyComp('Unit') then Result := tkUnit else Result := tkIdentifier;
end;

function TmwPasLex.Func65: TTokenKind;
begin
  if KeyComp('Repeat') then Result := tkRepeat else Result := tkIdentifier;
end;

function TmwPasLex.Func66: TTokenKind;
begin
  if KeyComp('Type') then Result := tkType else Result := tkIdentifier;
end;

function TmwPasLex.Func69: TTokenKind;
begin
  if KeyComp('Dynamic') then Result := tkDynamic else
    if KeyComp('Default') then Result := tkDefault else
      if KeyComp('Message') then Result := tkMessage else Result := tkIdentifier;
end;

function TmwPasLex.Func71: TTokenKind;
begin
  if KeyComp('Stdcall') then Result := tkStdcall else
    if KeyComp('Const') then Result := tkConst else Result := tkIdentifier;
end;

function TmwPasLex.Func73: TTokenKind;
begin
  if KeyComp('Except') then Result := tkExcept else Result := tkIdentifier;
end;

function TmwPasLex.Func75: TTokenKind;
begin
  if KeyComp('Write') then
  begin
    if inSymbols(CharAhead(fStringLen)) then Result := tkIdentifier else
      Result := tkWrite
  end else Result := tkIdentifier;
end;

function TmwPasLex.Func76: TTokenKind;
begin
  if KeyComp('Until') then Result := tkUntil else Result := tkIdentifier;
end;

function TmwPasLex.Func79: TTokenKind;
begin
  if KeyComp('Finally') then Result := tkFinally else Result := tkIdentifier;
end;

function TmwPasLex.Func81: TTokenKind;
begin
  if KeyComp('Interface') then
  begin
    Result := tkInterface;
    if fLastNoSpace = tkEqual then
      fIsInterface := True else fIsInterface := False;
  end else
    if KeyComp('Stored') then Result := tkStored else Result := tkIdentifier;
end;

function TmwPasLex.Func84: TTokenKind;
begin
  if KeyComp('Abstract') then Result := tkAbstract else Result := tkIdentifier;
end;

function TmwPasLex.Func85: TTokenKind;
begin
  if KeyComp('Library') then Result := tkLibrary else
    if KeyComp('Forward') then Result := tkForward else Result := tkIdentifier;
end;

function TmwPasLex.Func87: TTokenKind;
begin
  if KeyComp('String') then Result := tkString else Result := tkIdentifier;
end;

function TmwPasLex.Func88: TTokenKind;
begin
  if KeyComp('Program') then Result := tkProgram else Result := tkIdentifier;
end;

function TmwPasLex.Func91: TTokenKind;
begin
  if KeyComp('Private') then
  begin
    if inSymbols(CharAhead(fStringLen)) then Result := tkIdentifier else
      Result := tkPrivate
  end else
    if KeyComp('Downto') then Result := tkDownto else Result := tkIdentifier;
end;

function TmwPasLex.Func92: TTokenKind;
begin
  if KeyComp('overload') then
    Result := tkOverload
  else
    if KeyComp('Inherited') then
      Result := tkInherited
    else
      Result := tkIdentifier;
end;

function TmwPasLex.Func94: TTokenKind;
begin
  if KeyComp('Resident') then Result := tkResident else
    if KeyComp('Readonly') then Result := tkReadonly else
      if KeyComp('Assembler') then Result := tkAssembler else Result := tkIdentifier;
end;

function TmwPasLex.Func95: TTokenKind;
begin
  if KeyComp('Absolute') then Result := tkAbsolute else
    if KeyComp('Contains') then Result := tkContains else Result := tkIdentifier;
end;

function TmwPasLex.Func96: TTokenKind;
begin
  if KeyComp('Published') then
  begin
    if inSymbols(CharAhead(fStringLen)) then Result := tkIdentifier else
      Result := tkPublished
  end else
    if KeyComp('Override') then Result := tkOverride else Result := tkIdentifier;
end;

function TmwPasLex.Func97: TTokenKind;
begin
  if KeyComp('Threadvar') then Result := tkThreadvar else Result := tkIdentifier;
end;

function TmwPasLex.Func98: TTokenKind;
begin
  if KeyComp('Export') then Result := tkExport else
    if KeyComp('Nodefault') then Result := tkNodefault else Result := tkIdentifier;
end;

function TmwPasLex.Func99: TTokenKind;
begin
  if KeyComp('External') then Result := tkExternal else Result := tkIdentifier;
end;

function TmwPasLex.Func100: TTokenKind;
begin
  if KeyComp('Automated') then
  begin
    if inSymbols(CharAhead(fStringLen)) then Result := tkIdentifier else
      Result := tkAutomated
  end else Result := tkIdentifier;
end;

function TmwPasLex.Func101: TTokenKind;
begin
  if KeyComp('Register') then Result := tkRegister else Result := tkIdentifier;
end;

function TmwPasLex.Func102: TTokenKind;
begin
  if KeyComp('Function') then Result := tkFunction else Result := tkIdentifier;
end;

function TmwPasLex.Func103: TTokenKind;
begin
  if KeyComp('Virtual') then Result := tkVirtual else Result := tkIdentifier;
end;

function TmwPasLex.Func105: TTokenKind;
begin
  if KeyComp('Procedure') then Result := tkProcedure else Result := tkIdentifier;
end;

function TmwPasLex.Func106: TTokenKind;
begin
  if KeyComp('Protected') then
  begin
    if inSymbols(CharAhead(fStringLen)) then Result := tkIdentifier else
      Result := tkProtected
  end else Result := tkIdentifier;
end;

function TmwPasLex.Func108: TTokenKind;
begin
  if KeyComp('Operator') then Result := tkOperator else Result := tkIdentifier;
end;

function TmwPasLex.Func112: TTokenKind;
begin
  if KeyComp('Requires') then Result := tkRequires else Result := tkIdentifier;
end;

function TmwPasLex.Func117: TTokenKind;
begin
  if KeyComp('Exports') then Result := tkExports else Result := tkIdentifier;
end;

function TmwPasLex.Func126: TTokenKind;
begin
  if KeyComp('Implements') then
    Result := tkImplements
  else
    Result := tkIdentifier;
end;

function TmwPasLex.Func129: TTokenKind;
begin
  if KeyComp('Dispinterface') then Result := tkDispinterface else Result := tkIdentifier;
end;

function TmwPasLex.Func132: TTokenKind;
begin
  if KeyComp('Reintroduce') then
    Result := tkReintroduce
  else
    Result := tkIdentifier;
end;

function TmwPasLex.Func133: TTokenKind;
begin
  if KeyComp('Property') then Result := tkProperty else Result := tkIdentifier;
end;

function TmwPasLex.Func136: TTokenKind;
begin
  if KeyComp('Finalization') then Result := tkFinalization else Result := tkIdentifier;
end;

function TmwPasLex.Func141: TTokenKind;
begin
  if KeyComp('Writeonly') then Result := tkWriteonly else Result := tkIdentifier;
end;

function TmwPasLex.Func143: TTokenKind;
begin
  if KeyComp('Destructor') then Result := tkDestructor else Result := tkIdentifier;
end;

function TmwPasLex.Func166: TTokenKind;
begin
  if KeyComp('Constructor') then Result := tkConstructor else
    if KeyComp('Implementation') then Result := tkImplementation else Result := tkIdentifier;
end;

function TmwPasLex.Func168: TTokenKind;
begin
  if KeyComp('Initialization') then Result := tkInitialization else Result := tkIdentifier;
end;

function TmwPasLex.Func191: TTokenKind;
begin
  if KeyComp('Resourcestring') then Result := tkResourcestring else
    if KeyComp('Stringresource') then Result := tkStringresource else Result := tkIdentifier;
end;

function TmwPasLex.AltFunc: TTokenKind;
begin
  Result := tkIdentifier
end;

function TmwPasLex.IdentKind(MayBe: PChar): TTokenKind;
var
  HashKey: Integer;
begin
  fToIdent := MayBe;
  HashKey := KeyHash(MayBe);
  if HashKey < 192 then
    Result := fIdentFuncTable[HashKey]
  else
    Result := tkIdentifier;
end;

procedure TmwPasLex.MakeMethodTables;
var
  i: Char;
begin
  for i := #0 to #127 do
    case i of
      #0: fProcTable[i] := NullProc;
      #10: fProcTable[i] := LFProc;
      #13: fProcTable[i] := CRProc;
      #1..#9, #11, #12, #14..#32:
        fProcTable[i] := SpaceProc;
      '#': fProcTable[i] := AsciiCharProc;
      '$': fProcTable[i] := IntegerProc;
      #39: fProcTable[i] := StringProc;
      '0'..'9': fProcTable[i] := NumberProc;
      'A'..'Z', 'a'..'z', '_':
        fProcTable[i] := IdentProc;
      '{': fProcTable[i] := BraceOpenProc;
      '}': fProcTable[i] := BraceCloseProc;
      '!', '"', '%', '&', '('..'/', ':'..'@', '['..'^', '`', '~':
        begin
          case i of
            '(': fProcTable[i] := RoundOpenProc;
            ')': fProcTable[i] := RoundCloseProc;
            '*': fProcTable[i] := StarProc;
            '+': fProcTable[i] := PlusProc;
            ',': fProcTable[i] := CommaProc;
            '-': fProcTable[i] := MinusProc;
            '.': fProcTable[i] := PointProc;
            '/': fProcTable[i] := SlashProc;
            ':': fProcTable[i] := ColonProc;
            ';': fProcTable[i] := SemiColonProc;
            '<': fProcTable[i] := LowerProc;
            '=': fProcTable[i] := EqualProc;
            '>': fProcTable[i] := GreaterProc;
            '@': fProcTable[i] := AddressOpProc;
            '[': fProcTable[i] := SquareOpenProc;
            ']': fProcTable[i] := SquareCloseProc;
            '^': fProcTable[i] := PointerSymbolProc;
          else
            fProcTable[i] := SymbolProc;
          end;
        end;
    else
      fProcTable[i] := UnknownProc;
    end;
end;

constructor TmwPasLex.Create;
begin
  inherited Create;
  InitIdent;
  MakeMethodTables;
end; { Create }

destructor TmwPasLex.Destroy;
begin
  inherited Destroy;
end; { Destroy }

procedure TmwPasLex.SetLineNumber(const Value: Integer);
begin
  fLineNumber := Value;
end;

procedure TmwPasLex.SetOrigin(NewValue: PChar);
begin
  fOrigin := NewValue;
  Restart;
end; { SetOrigin }

procedure TmwPasLex.SetParsingState(const State: TmwPasLexState);
begin
  FComment := State.FComment;
  Run := State.Run;
  FRoundCount := State.FRoundCount;
  FSquareCount := State.FSquareCount;
  fStringLen := State.fStringLen;
  fToIdent := State.fToIdent;
  fTokenPos := State.fTokenPos;
  fLineNumber := State.fLineNumber;
  FTokenID := State.FTokenID;
  fLastIdentPos := State.fLastIdentPos;
  fLastNoSpace := State.fLastNoSpace;
  fLastNoSpacePos := State.fLastNoSpacePos;
  fLinePos := State.fLinePos;
  fIsInterface := State.fIsInterface;
  fIsClass := State.fIsClass;
end;

procedure TmwPasLex.AddressOpProc;
begin
  case FOrigin[Run + 1] of
    '@':
      begin
        fTokenID := tkDoubleAddressOp;
        Inc(Run, 2);
      end;
  else
    begin
      fTokenID := tkAddressOp;
      Inc(Run);
    end;
  end;
end;

procedure TmwPasLex.AsciiCharProc;
begin
  fTokenID := tkAsciiChar;
  Inc(Run);
  while IsCharNumeric(FOrigin[Run]) do Inc(Run);
end;

procedure TmwPasLex.BraceCloseProc;
begin
  Inc(Run);
  fTokenId := tkError;
end;

procedure TmwPasLex.BorProc;
begin
  fTokenID := tkBorComment;
  case FOrigin[Run] of
    #0:
      begin
        NullProc;
        Exit;
      end;

    #10:
      begin
        LFProc;
        Exit;
      end;

    #13:
      begin
        CRProc;
        Exit;
      end;
  end;

  while FOrigin[Run] <> #0 do
    case FOrigin[Run] of
      '}':
        begin
          fComment := csNo;
          Inc(Run);
          Break;
        end;
      #10: Break;

      #13: Break;
    else Inc(Run);
    end;
end;

procedure TmwPasLex.BraceOpenProc;
begin
  case FOrigin[Run + 1] of
    '$': fTokenID := tkCompDirect;
  else
    begin
      fTokenID := tkBorComment;
      fComment := csBor;
    end;
  end;
  Inc(Run);
  while FOrigin[Run] <> #0 do
    case FOrigin[Run] of
      '}':
        begin
          fComment := csNo;
          Inc(Run);
          Break;
        end;
      #10: Break;

      #13: Break;
    else Inc(Run);
    end;
end;

procedure TmwPasLex.ColonProc;
begin
  case FOrigin[Run + 1] of
    '=':
      begin
        Inc(Run, 2);
        fTokenID := tkAssign;
      end;
  else
    begin
      Inc(Run);
      fTokenID := tkColon;
    end;
  end;
end;

procedure TmwPasLex.CommaProc;
begin
  Inc(Run);
  fTokenID := tkComma;
end;

procedure TmwPasLex.CRProc;
begin
  case fComment of
    csBor: fTokenID := tkCRLFCo;
    csAnsi: fTokenID := tkCRLFCo;
  else fTokenID := tkCRLF;
  end;

  case FOrigin[Run + 1] of
    #10: Inc(Run, 2);
  else Inc(Run);
  end;
  Inc(fLineNumber);
  fLinePos := Run;
end;

procedure TmwPasLex.EqualProc;
begin
  Inc(Run);
  fTokenID := tkEqual;
end;

procedure TmwPasLex.GreaterProc;
begin
  case FOrigin[Run + 1] of
    '=':
      begin
        Inc(Run, 2);
        fTokenID := tkGreaterEqual;
      end;
  else
    begin
      Inc(Run);
      fTokenID := tkGreater;
    end;
  end;
end;

function TmwPasLex.InSymbols(aChar: Char): Boolean;
begin
  if IsCharSymbol(aChar) then
    Result := True
  else
    Result := False;
end;

function TmwPasLex.CharAhead(Count: Integer): Char;
var
  Temp: PChar;
begin
  Temp := fOrigin + Run + Count;
  while CharInSet(Temp^, [#1..#9, #11, #12, #14..#32]) do
    Inc(Temp);
  Result := Temp^;
end;

function TmwPasLex.NextChar: Char;
var
  Temp:PChar;
begin
  Temp := fOrigin + Run;
  Result := Temp^;
end;

procedure TmwPasLex.IdentProc;
begin
  fTokenID := IdentKind((fOrigin + Run));
  Inc(Run, fStringLen);
  while IsIdentifier(fOrigin[Run]) do
    Inc(Run);
end;

procedure TmwPasLex.IntegerProc;
begin
  Inc(Run);
  fTokenID := tkInteger;
  while CharInSet(FOrigin[Run], ['0'..'9', 'A'..'F', 'a'..'f']) do
    Inc(Run);
end;

procedure TmwPasLex.LFProc;
begin
  case fComment of
    csBor: fTokenID := tkCRLFCo;
    csAnsi: fTokenID := tkCRLFCo;
  else
    fTokenID := tkCRLF;
  end;
  Inc(Run);
  Inc(fLineNumber);
  fLinePos := Run;
end;

procedure TmwPasLex.LowerProc;
begin
  case FOrigin[Run + 1] of
    '=':
      begin
        Inc(Run, 2);
        fTokenID := tkLowerEqual;
      end;
    '>':
      begin
        Inc(Run, 2);
        fTokenID := tkNotEqual;
      end
  else
    begin
      Inc(Run);
      fTokenID := tkLower;
    end;
  end;
end;

procedure TmwPasLex.MinusProc;
begin
  Inc(Run);
  fTokenID := tkMinus;
end;

procedure TmwPasLex.NullProc;
begin
  fTokenID := tkNull;
end;

procedure TmwPasLex.NumberProc;
begin
  Inc(Run);
  fTokenID := tkNumber;
  while CharInSet(FOrigin[Run], ['0'..'9', '.', 'e', 'E']) do
  begin
    case FOrigin[Run] of
      '.':
        if FOrigin[Run + 1] = '.' then Break else fTokenID := tkFloat
    end;
    Inc(Run);
  end;
end;

procedure TmwPasLex.PlusProc;
begin
  Inc(Run);
  fTokenID := tkPlus;
end;

procedure TmwPasLex.PointerSymbolProc;
begin
  Inc(Run);
  fTokenID := tkPointerSymbol;
end;

procedure TmwPasLex.PointProc;
begin
  case FOrigin[Run + 1] of
    '.':
      begin
        Inc(Run, 2);
        fTokenID := tkDotDot;
      end;
    ')':
      begin
        Inc(Run, 2);
        fTokenID := tkSquareClose;
        Dec(FSquareCount);
      end;
  else
    begin
      Inc(Run);
      fTokenID := tkPoint;
    end;
  end;
end;

procedure TmwPasLex.Restart;
begin
  fComment := csNo;
  Run := 0;
  FRoundCount := 0;
  FSquareCount := 0;
  fStringLen := 0;
  fToIdent := nil;
  fTokenPos := 0;
  fLineNumber := 0;
  FTokenID := tkError;
  fLastIdentPos := 0;
  fLastNoSpace := tkError;
  fLastNoSpacePos := 0;
  fLinePos := 0;
  fIsInterface := False;
  fIsClass := False;

  Next;
end;

procedure TmwPasLex.RoundCloseProc;
begin
  Inc(Run);
  fTokenID := tkRoundClose;
  Dec(FRoundCount);
end;

procedure TmwPasLex.AnsiProc;
begin
  fTokenID := tkAnsiComment;
  case FOrigin[Run] of
    #0:
      begin
        NullProc;
        Exit;
      end;

    #10:
      begin
        LFProc;
        Exit;
      end;

    #13:
      begin
        CRProc;
        Exit;
      end;
  end;

  while fOrigin[Run] <> #0 do
    case fOrigin[Run] of
      '*':
        if fOrigin[Run+1] = ')' then
        begin
          fComment := csNo;
          Inc(Run, 2);
          Break;
        end else Inc(Run);
      #10: Break;

      #13: Break;
    else Inc(Run);
    end;
end;

procedure TmwPasLex.RoundOpenProc;
begin
  Inc(Run);
  case fOrigin[Run] of
    '*':
      begin
        fTokenID := tkAnsiComment;
        if FOrigin[Run + 1] = '$' then fTokenID := tkCompDirect else fComment := csAnsi;
        Inc(Run);
        while fOrigin[Run] <> #0 do
          case fOrigin[Run] of
            '*':
              if fOrigin[Run+1]=')' then
              begin
                fComment := csNo;
                Inc(Run, 2);
                Break;
              end else Inc(Run);
            #10: Break;
            #13: Break;
          else Inc(Run);
          end;
      end;
    '.':
      begin
        Inc(Run);
        fTokenID := tkSquareOpen;
        Inc(FSquareCount);
      end;
  else
    begin
      FTokenID := tkRoundOpen;
      Inc(FRoundCount);
    end;
  end;
end;

procedure TmwPasLex.SemiColonProc;
begin
  Inc(Run);
  fTokenID := tkSemiColon;
end;

procedure TmwPasLex.SlashProc;
begin
  case FOrigin[Run + 1] of
    '/':
      begin
        Inc(Run, 2);
        fTokenID := tkSlashesComment;
        while FOrigin[Run]<>#0 do
        begin
          case FOrigin[Run] of
            #10, #13: Break;
          end;
          Inc(Run);
        end;
      end;
  else
    begin
      Inc(Run);
      fTokenID := tkSlash;
    end;
  end;
end;

procedure TmwPasLex.SpaceProc;
begin
  Inc(Run);
  fTokenID := tkSpace;
  while CharInSet(FOrigin[Run], SpaceChars) do Inc(Run);
end;

procedure TmwPasLex.SquareCloseProc;
begin
  Inc(Run);
  fTokenID := tkSquareClose;
  Dec(FSquareCount);
end;

procedure TmwPasLex.SquareOpenProc;
begin
  Inc(Run);
  fTokenID := tkSquareOpen;
  Inc(FSquareCount);
end;

procedure TmwPasLex.StarProc;
begin
  Inc(Run);
  fTokenID := tkStar;
end;

procedure TmwPasLex.StringProc;
begin
  FTokenID := tkString;
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
    else if CharInSet(fOrigin[Run], [#0, #10, #13]) then
    begin
      FTokenID := tkBadString;
      Break;
    end;
    Inc(Run);
  end;
end;

procedure TmwPasLex.SymbolProc;
begin
  Inc(Run);
  fTokenID := tkSymbol;
end;

procedure TmwPasLex.UnknownProc;
begin
  Inc(Run);
  fTokenID := tkUnknown;
end;

procedure TmwPasLex.doProcTable(AChar: Char);
begin
  if Ord(AChar) <= 127 then begin
    // todo: Shouldn't this be
    // fProcTable[AChar];
    // ?
    fProcTable[fOrigin[Run]];
  end else begin
    // This considers characters > #127 to be identifiers, AChar is a WideChar in Delphi >=2009 !
    IdentProc;
  end;
end;

function TmwPasLex.Next: Boolean;
begin
  case fTokenID of
    tkIdentifier:
      begin
        fLastIdentPos := fTokenPos;
        fLastNoSpace := fTokenID;
        fLastNoSpacePos := fTokenPos;
      end;
    tkSpace: ;
  else
    begin
      fLastNoSpace := fTokenID;
      fLastNoSpacePos := fTokenPos;
    end;
  end;
  fTokenPos := Run;
  case fComment of
    csNo: doProcTable(fOrigin[Run]);
  else
    case fComment of
      csBor: BorProc;
      csAnsi: AnsiProc;
    end;
  end;
  Result := (FTokenID <> tkNull);
end;

function TmwPasLex.GetToken: string;
var
  Len: Longint;
begin
  Len := Run - fTokenPos;
  SetString(Result, (FOrigin + fTokenPos), Len);
end;

procedure TmwPasLex.NextID(ID: TTokenKind);
begin
  repeat
    case fTokenID of
      tkNull: Break;
    else Next;
    end;
  until fTokenID = ID;
end;

function TmwPasLex.NextNoJunk: boolean;
begin
  repeat
    Result := Next;
  until not (fTokenID in [tkSlashesComment, tkAnsiComment, tkBorComment, tkCRLF, tkCRLFCo, tkSpace]);
end;

procedure TmwPasLex.NextClass;
begin
  if fTokenID <> tkNull then next;
  repeat
    case fTokenID of
      tkNull: Break;
    else Next;
    end;
  until (fTokenID = tkClass) and (IsClass);
end;

function TmwPasLex.GetDottedIdentifierAtPos(StayAtIdentEnd: Boolean): string;
begin
  Result := '';
  // We allow string as an identifier here since it is a valid type
  if not (TokenID in [tkIdentifier, tkString]) then
    NextNoJunk;
  while TokenID in [tkIdentifier, tkPoint, tkString, tkNull] do begin
    if TokenID = tkNull then
      Exit;
    Result := Result + Token;
    if StayAtIdentEnd and ((NextChar <> '.') and (not (IsCharIdentifierStart(NextChar)))) then
      Exit;
    Next;
  end;
end;

procedure TmwPasLex.GetParsingState(out State: TmwPasLexState);
begin
  State.FComment := fComment;
  State.Run := Run;
  State.FRoundCount := FRoundCount;
  State.FSquareCount := FSquareCount;
  State.fStringLen := fStringLen;
  State.fToIdent := fToIdent;
  State.fTokenPos := fTokenPos;
  State.fLineNumber := fLineNumber;
  State.FTokenID := FTokenID;
  State.fLastIdentPos := fLastIdentPos;
  State.fLastNoSpace := fLastNoSpace;
  State.fLastNoSpacePos := fLastNoSpacePos;
  State.fLinePos := fLinePos;
  State.fIsInterface := fIsInterface;
  State.fIsClass := fIsClass;
end;

function TmwPasLex.GoToNextMethodDefinition: Boolean;
begin
  Result := True;
  while not (TokenID in ([tkNull] + MethodMarkers)) do
    NextNoJunk;
  if TokenID = tkNull then
    Result := False;
end;

procedure TmwPasLex.GetClassMethodDetailsFromCurrentPos(var NestedClasses,
  ClassName, MethodName, Args, ResultType: string);
begin
  GetClassMethodDetailsFromCurrentPosEx(NestedClasses, ClassName, MethodName,
    Args, ResultType, nil);
end;

procedure TmwPasLex.GetClassMethodDetailsFromCurrentPosEx(var NestedClasses,
  ClassName, MethodName, Args, ResultType: string; ArgList: TLexArgList);

  procedure ResetArg(var ArgToken, ArgText, ArgName, ArgCoreName, ArgType, ArgDefault: string;
    var ArgPassMode: TLexArgPassMode; var AttribTypeOpen: Boolean);
  begin
    ArgToken := '';
    ArgText := '';
    ArgName := '';
    ArgCoreName := '';
    ArgType := '';
    ArgDefault := '';
    ArgPassMode := lapmValue;
    AttribTypeOpen := False;
  end;

  procedure AddSingleArg(const ArgText, ArgName, ArgCoreName, ArgType, ArgDefault: string;
    ArgPassMode: TLexArgPassMode; ArgList: TLexArgList);
  var
    NewItem: TLexArgItem;
  begin
    NewItem := TLexArgItem.Create;
    NewItem.ArgText := ArgText;
    NewItem.LName := Trim(ArgName);
    NewItem.CoreName := ArgCoreName;
    NewItem.PassMode := ArgPassMode;
    NewItem.ArgType := Trim(ArgType);

    if Pos('&', NewItem.ArgType) = 1 then
      Delete(NewItem.ArgType, 1, 1);

    NewItem.DefaultValue := Trim(ArgDefault);

    ArgList.Add(NewItem);
  end;

  procedure AddArg(const ArgText, ArgName, ArgCoreName, ArgType, ArgDefault: string;
    ArgPassMode: TLexArgPassMode; ArgList: TLexArgList);
  var
    i: Integer;
    NameList: TStringList;
  begin
    NameList := TStringList.Create;
    try
      NameList.Delimiter := ',';
      NameList.DelimitedText := ArgCoreName;
      for i := 0 to NameList.Count - 1 do
        AddSingleArg(ArgText, ArgName, Trim(NameList[i]), ArgType, ArgDefault, ArgPassMode, ArgList);
    finally
      NameList.Free;
    end;
  end;

var
  PreviousIdentifier: string;
  CurrentIdentifier: string;
  PrevIdentifiers: string;
  ArgName: string;
  ArgCoreName: string;
  ArgType: string;
  ArgDefault: string;
  ArgPassMode: TLexArgPassMode;
  ArgToken: string;
  ArgTokenText: string;
  ArgAttribOpen: Boolean;
  ArgText: string;
begin
  // Account for nested classes
  PreviousIdentifier := '';
  CurrentIdentifier := '';
  PrevIdentifiers := '';

  if TokenID in MethodMarkers then
    NextNoJunk;

  while TokenID in PossibleIdentifiers + [tkPoint, tkLower] do
  begin
    if TokenID = tkLower then begin
      // It's a Generic, so we need to read until the closing '>'
      CurrentIdentifier := CurrentIdentifier + Token;
      NextNoJunk;
      while TokenID in [tkComma, tkIdentifier] do begin
        CurrentIdentifier := CurrentIdentifier + Token;
        NextNoJunk;
      end;
      if TokenID = tkGreater then begin
        CurrentIdentifier := CurrentIdentifier + Token;
        NextNoJunk;
      end;
    end;
    if TokenID in PossibleIdentifiers then
    begin
      if PrevIdentifiers <> '' then
        PrevIdentifiers := PrevIdentifiers + '.' + CurrentIdentifier
      else
        PrevIdentifiers := CurrentIdentifier;
      PreviousIdentifier := CurrentIdentifier;
      CurrentIdentifier := Token;
    end;
    NextNoJunk;
  end;
  MethodName := CurrentIdentifier;
  ClassName := PreviousIdentifier;
  NestedClasses := PrevIdentifiers;

  // If a open paren is found, gather the method arguments
  if TokenID = tkRoundOpen then
  begin
    NextNoJunk;
    if TokenID in [tkNull, tkUnknown] then
      Exit;
    Args := '';

    ArgAttribOpen := False;
    ResetArg(ArgToken, ArgText, ArgName, ArgCoreName, ArgType, ArgDefault,
      ArgPassMode, ArgAttribOpen);

    while not (TokenID in [tkNull, tkUnknown]) do
    begin
      if TokenID in [tkCRLF, tkCRLFCo, tkSlashesComment, tkBorComment, tkAnsiComment, tkSpace] then
        ArgToken := ' '
      else
        ArgToken := Token;

     if (TokenID <> tkSemiColon) and ((TokenID <> tkRoundClose) or ArgAttribOpen) then
       ArgText := ArgText + ArgToken;

     if (TokenID = tkConst) and (ArgName <> '') then
       ArgTokenText := ArgTokenText + ArgToken;

      case TokenID of
        tkConst:
          if ArgName = '' then
            ArgPassMode := lapmConst;
        tkVar:        ArgPassMode := lapmVar;
        tkOut:        ArgPassMode := lapmOut;
        tkSquareOpen:
          if ArgName = '' then
            ArgAttribOpen := True;
        tkSquareClose:
          if ArgName = '' then
            ArgAttribOpen := False;
        tkColon:
          begin
            ArgName := ArgName + ArgTokenText;
            ArgTokenText := '';
          end;
        tkEqual:
          begin
            ArgType := ArgType + ArgTokenText;
            ArgTokenText := '';
          end;
        tkIdentifier:
          begin
            if (ArgName = '') and (not ArgAttribOpen) then
            begin
              if ArgCoreName <> '' then
                ArgCoreName := ArgCoreName + ',';

              ArgCoreName := ArgCoreName + ArgToken;
            end;

            ArgTokenText := ArgTokenText + ArgToken;
          end;
        tkSemiColon, tkRoundClose:
          if ArgAttribOpen and (TokenID = tkRoundClose) then
            ArgTokenText := ArgTokenText + ArgToken
          else
          begin
            if ArgType = '' then
              ArgType := ArgTokenText
            else
              ArgDefault := ArgTokenText;

            ArgTokenText := '';

            if Assigned(ArgList) and (ArgType<>'') then
              AddArg(ArgText, ArgName, ArgCoreName, ArgType, ArgDefault, ArgPassMode, ArgList);

            if TokenID = tkSemiColon then
              Args := Args + ArgToken;  // Since this gets reset below

            ResetArg(ArgToken, ArgText, ArgName, ArgCoreName, ArgType, ArgDefault,
              ArgPassMode, ArgAttribOpen);
          end;
        else
          ArgTokenText := ArgTokenText + ArgToken;
      end;

      if (TokenID = tkRoundClose) and (not ArgAttribOpen) then
        Break;

      Args := Args + ArgToken;
      Next;
    end;
    // Skip to the colon or semicolon after the ')'
    NextNoJunk;
  end;

  if TokenID in [tkAnsiComment, tkBorComment, tkCRLF, tkCRLFCo, tkSpace] then
    NextNoJunk;

  // If a colon is found, find the next token to get the function result type
  if TokenID = tkColon then
  begin
    NextNoJunk;
    ResultType := GetDottedIdentifierAtPos;
  end;
end;

procedure Initialize;
begin
  MakeIdentTable;
end;

{ TLexArgList }

function TLexArgList.GetItem(Index: Integer): TLexArgItem;
begin
  Result := (inherited Items[Index]) as TLexArgItem;
end;

initialization
  Initialize;

end.

