unit GX_StringList;

{$I GX_CondDefine.inc}

interface

uses
  SysUtils,
{$IFNDEF UNICODE}
 // UniSynEdit is required for TGXUnicodeStringList in Delphi 2007 and earlier
  SynUnicode,
{$ENDIF UNICODE}
  Classes;

type
{$IFDEF UNICODE}
  TGXStringList = class(TStringList)
  private
    LoadedEncoding: TEncoding;
  public
    procedure LoadFromFile(const FileName: string); override;
    procedure SaveToFile(const FileName: string); override;
    procedure SortLogical;
  end;

  TGXUnicodeChar = Char;
  PGXUnicodeChar = PChar;
  TGXUnicodeString = string;
  TGXUnicodeStringList = TGXStringList;
{$ELSE ~UNICODE}
  TGXUnicodeChar = WideChar;
  PGXUnicodeChar = PWideChar;
  TGXUnicodeString = WideString;
  TGXUnicodeStringList = class(TUnicodeStringList)
    // SynEdit TUnicodeStringList in D2007 and lower
    procedure SortLogical;
  end;
{$ENDIF}

implementation

uses
  GX_GenericUtils;

{ TGXStringList }

const
  shlwapi32 = 'shlwapi.dll';

function StrCmpLogicalW(psz1, psz2: PWideChar): Integer; stdcall;
  external shlwapi32 Name 'StrCmpLogicalW';

{$IFDEF UNICODE}

function GXCompareStringsLogical(List: TStringList; Index1, Index2: Integer): Integer;
begin
  Result := StrCmpLogicalW(PChar(List[Index1]), PChar(List[Index2]));
end;

procedure TGXStringList.LoadFromFile(const FileName: string);
begin
  LoadedEncoding := GetFileEncoding(FileName);
  inherited LoadFromFile(FileName);
end;

procedure TGXStringList.SaveToFile(const FileName: string);
begin
  if Assigned(LoadedEncoding) then
    SaveToFile(FileName, LoadedEncoding)
  else
    inherited SaveToFile(FileName);
end;

procedure TGXStringList.SortLogical;
begin
  if CheckWin32Version(5, 1) then // Windows XP and up
    CustomSort(GXCompareStringsLogical)
  else
    Self.Sort;
end;

{$ELSE UNICODE}
{ TGXUnicodeStringList }

function GXCompareStringsLogical(AString1, AString2: UnicodeString): Integer;
begin
  Result := StrCmpLogicalW(PWideChar(AString1), PWideChar(AString2));
end;

procedure TGXUnicodeStringList.SortLogical;
begin
  if CheckWin32Version(5, 1) then // Windows XP and up
    CustomSort(GXCompareStringsLogical)
  else
    Self.Sort;
end;

{$ENDIF}

end.
