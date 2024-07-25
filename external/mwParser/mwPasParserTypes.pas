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

unit mwPasParserTypes;

interface

type
  TTokenKind = (
  // tkAbort,
    tkAbsolute,
    tkAbstract,
 // ptAdd,
    tkAddressOp,
 // ptAmpersand,
    tkAnd,
    tkAnsiComment,
 // ptAnsiString,
    tkArray,
    tkAs,
    tkAsciiChar,
    tkAsm,
    tkAssembler,
    tkAssign,
    tkAt,
    tkAutomated,
    tkBegin,
    tkBadString,
    tkBorComment,
 // ptBraceClose,
 // ptBraceOpen,
 // ptBreak, //JThurman 2004-11-8 (flow control routines)
 // ptByte,
 // ptByteBool,
 // ptCardinal,
    tkCase,
    tkCdecl,
 // ptChar,
    tkClass,
 // ptClassForward,
 // ptClassFunction,
 // ptClassProcedure,
    tkColon,
    tkComma,
 // ptComp,
    tkCompDirect,
    tkConst,
    tkConstructor,
    tkContains,
 // ptContinue, //JThurman 2004-11-8 (flow control routines)
    tkCRLF,
    tkCRLFCo,
 // ptCurrency,
    tkDefault,
 // ptDefineDirect,
    tkDelayed,
 // ptDeprecated, // DR 2001-10-20
    tkDestructor,
    tkDispid,
    tkDispinterface,
    tkDiv,
    tkDo,
    tkDotDot,
 // ptDouble,
    tkDoubleAddressOp,
    tkDownto,
 // ptDWORD,
    tkDynamic,
    tkElse,
 // ptElseDirect,
    tkEnd,
 // ptEndIfDirect,
    tkEqual,
    tkError,
    tkExcept,
 // ptExit, //JThurman 2004-11-8 (flow control routine)
    tkExport,
    tkExports,
 // ptExtended,
    tkExternal,
    tkFar,
    tkFile,
{$IFDEF D8_NEWER} //JThurman 2004-03-20
 // ptFinal,
{$ENDIF}
    tkFinalization,
    tkFinally,
    tkFloat,
    tkFor,
    tkForward,
    tkFunction,
    tkGoto,
    tkGreater,
    tkGreaterEqual,
 // ptHalt, //JThurman 2004-11-8 (flow control routines)
{$IFDEF D8_NEWER} //JThurman 2004-04-06
 // ptHelper,
{$ENDIF}
    tkIdentifier,
    tkIf,
 // ptIfDirect,
 // ptIfEndDirect,
 // ptElseIfDirect,
 // ptIfDefDirect,
 // ptIfNDefDirect,
 // ptIfOptDirect,
    tkImplementation,
    tkImplements, // D4-8 items added by Erik Berry
    tkIn,
 // ptIncludeDirect,
    tkIndex,
    tkInherited,
    tkInitialization,
    tkInline,
    tkInt64, // D4-8 items added by Erik Berry
    tkInteger,
 // ptIntegerConst,
    tkInterface,
    tkIs,
    tkKeyString,
    tkLabel,
    tkLibrary,
 // ptLocal,  // DR 2001-11-14
 // ptLongBool,
 // ptLongint,
    tkLongWord, // D4-8 items added by Erik Berry
    tkLower,
    tkLowerEqual,
    tkMessage,
    tkMinus,
    tkMod,
    tkName,
    tkNear,
    tkNil,
    tkNodefault,
    tkNone,
    tkNot,
    tkNotEqual,
    tkNull,
    tkNumber,
    tkObject,
    tkOf,
 // ptOleVariant,
    tkOn,
    tkOperator, // D4-8 items added by Erik Berry
    tkOr,
    tkOut,
    tkOverload, // D4-8 items added by Erik Berry
    tkOverride,
    tkPackage,
    tkPacked,
    tkPascal,
 // ptPChar,
 // ptPlatform, // DR 2001-10-20
    tkPlus,
    tkPoint,
    tkPointerSymbol,
    tkPrivate,
    tkProcedure,
    tkProgram,
    tkProperty,
    tkProtected,
    tkPublic,
    tkPublished,
    tkRaise,
    tkRead,
    tkReadonly,
 // ptReal,
 // ptReal48,
    tkRecord,
{$IFDEF D12_NEWER}
 // ptReference, //JThurman 2008-25-07 (anonymous methods)
{$ENDIF}
    tkRegister,
    tkReintroduce, // D4-8 items added by Erik Berry
 // ptRemove,
    tkRepeat,
    tkRequires,
    tkResident,
 // ptResourceDirect,
    tkResourcestring,
    tkRoundClose,
    tkRoundOpen,
 // ptRunError, //JThurman 2004-11-8 (flow control routines)
    tkSafecall,
{$IFDEF D8_NEWER} //JThurman 2004-03-19
 // ptSealed,
{$ENDIF}
    tkSemiColon,
    tkSet,
    tkShl,
 // ptShortint,
 // ptShortString,
    tkShr,
 // ptSingle,
    tkSlash,
    tkSlashesComment,
 // ptSmallint,
    tkSpace,
    tkSquareClose,
    tkSquareOpen,
    tkStar,
    tkStatic,
    tkStdcall,
    tkStored,
    tkStrict, // D4-8 items added by Erik Berry
    tkString,
 // ptStringConst,
 // ptStringDQConst,	// 2002-01-14
    tkStringresource,
    tkSymbol,
    tkThen,
    tkThreadvar,
    tkTo,
    tkTry,
    tkType,
 // ptUndefDirect,
    tkUnit,
    tkUnknown,
{$IFDEF D8_NEWER} //JThurman 2004-03-2003
 // ptUnsafe,
{$ENDIF}
    tkUntil,
    tkUses,
    tkVar,
 // ptVarargs, // DR 2001-11-14
 // ptVariant,
    tkVirtual,
    tkWhile,
 // ptWideChar,
 // ptWideString,
    tkWith,
 // ptWord,
 // ptWordBool,
    tkWrite,
    tkWriteonly,
    tkXor);

  TTokenKindSet = set of TTokenKind;
  TCommentState = (csAnsi, csBor, csNo, csSlashes);

const
  IdentDirect: TTokenKindSet = [tkAbsolute, tkAbstract, tkAssembler, tkCdecl,
    tkDefault, tkDelayed, tkDispid, tkDynamic, tkExport, tkExternal, tkFar, tkForward,
    tkIdentifier, tkIndex, tkMessage, tkName, tkNear, tkNodefault, tkOverride,
    tkPascal, tkRead, tkReadonly, tkRegister, tkResident, tkSafecall, tkStdcall,
    tkStored, tkVirtual, tkWrite, tkWriteonly, tkReintroduce, tkOverload, tkImplements];

Const
  BigIdentDirect: TTokenKindSet = [tkAbsolute, tkAbstract, tkAssembler,
    tkAutomated, tkCdecl, tkDefault, tkDelayed, tkDispid, tkDynamic, tkExport, tkExternal,
    tkFar, tkForward, tkIdentifier, tkIndex, tkMessage, tkName, tkNear,
    tkNodefault, tkOverride, tkPascal, tkPrivate, tkProtected, tkPublic,
    tkPublished, tkRead, tkReadonly, tkRegister, tkResident, tksafecall,
    tkstdcall, tkStored, tkVirtual, tkWrite, tkWriteonly];

var
  MethodMarkers: set of TTokenKind = [tkProcedure, tkFunction, tkConstructor, tkDestructor, tkOperator];

implementation

end.
