unit u_dzTypeInfoUtils;

{$INCLUDE 'dzlib.inc'}

interface

uses
  SysUtils,
  TypInfo;

function SafeGetEnumName(TypeInfo: PTypeInfo; Value: Integer): string;

implementation

uses
  u_dzTypes;

function SafeGetEnumName(TypeInfo: PTypeInfo; Value: Integer): string;

  procedure IncPtr(var _p: Pointer);
  begin
    _p := Pointer(Intptr(_p) + 1);
  end;

  // P must point to the length field (that is the first byte) of a ShortString
  function AfterShortString(const P: Pointer): Pointer;
  begin
    Result := Pointer(Intptr(P) + PByte(P)^ + 1);
  end;

{$IFDEF ENUM_NAMES_ARE_UTF8}

{$IFNDEF HAS_UTF8TOWIDESTRING}
  function UTF8ToWideString(const _Utf8: RawByteString): WideString;
  begin
    Result := UTF8Decode(_Utf8);
  end;
{$ENDIF}

// Older Unicode Delphi versions did not have a UTF8ToString overload for a pointer parameter
{$IFNDEF DELPHIX_ATHENS_UP}
  function _UTF8ToString(P: Pointer): WideString;
  var
    Len: Byte;
    Buf: UTF8String;
  begin
    Result := '';
    Len := PByte(P)^;
    if Len <> 0 then begin
      SetLength(Buf, Len);
      IncPtr(P);
      Move(PByte(P)^, Buf[1], Len);
      Result := UTF8ToWideString(Buf);
    end;
  end;
{$ENDIF}
{$ENDIF}

var
  P: Pointer;
  T: PTypeData;
begin
  if TypeInfo^.Kind = tkInteger then begin
    Result := IntToStr(Value);
    Exit;
  end;
  T := GetTypeData(GetTypeData(TypeInfo)^.BaseType^);
  if (TypeInfo = System.TypeInfo(Boolean)) or (T^.MinValue < 0) then begin
    { LongBool/WordBool/ByteBool have MinValue < 0 and arbitrary
      content in Value; Boolean has Value in [0, 1] }
    Result := BooleanIdents[Value <> 0];
    if SameText(HexDisplayPrefix, '0x') then
      Result := LowerCase(Result);
  end else begin
    if (Value < T.MinValue) or (Value > T.MaxValue) then
      Result := Format('OutOfBounds(%d)', [Value])
    else begin
      P := @T^.NameList;
      while Value <> 0 do begin
        P := AfterShortString(P);
        Dec(Value);
      end;
{$IFDEF ENUM_NAMES_ARE_UTF8}
      Result := _UTF8ToString(P);
{$ELSE}
      Result := PShortString(P)^;
{$ENDIF}
    end;
  end;
end;

end.
