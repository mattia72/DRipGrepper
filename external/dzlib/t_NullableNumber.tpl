{$IFNDEF __DZ_NULLABLE_NUMBER_TEMPLATE__}
unit t_NullableNumber;

interface

uses
  SysUtils,
  Variants,
  u_dzNullableTypesUtils;

/// These types must be declared for each class built on this template
type
  // can be integer, int64, single, double, extended and possibly some other
  // numerical types (e.g. currency) which I have not(!) tested.
  _NULLABLE_TYPE_BASE_ = Int64;
const
  _NULLABLE_TYPE_NAME_ = 'TNullableBla';

{$ENDIF __DZ_NULLABLE_NUMBER_TEMPLATE__}

{$IFNDEF __DZ_NULLABLE_NUMBER_TEMPLATE_SECOND_PASS__}

type
  TdzNullableNumber = record
  private
    FIsValid: INullableTypesFlagInterface;
    FValue: _NULLABLE_TYPE_BASE_;
  public
    procedure Invalidate;
    function Value: _NULLABLE_TYPE_BASE_;
    function IsValid: Boolean; inline;
    function GetValue(out _Value: _NULLABLE_TYPE_BASE_): Boolean;
    procedure AssignVariant(_a: Variant);
    function ToVariant: Variant;
    ///<summary>
    /// Tries to convert the given string to a number. If that is possible, it assigns that
    /// number to the record. If not, the record will be invalid.
    procedure AssignStr(const _s: string; const _FormatSettings: TFormatSettings); overload;
    procedure AssignStr(const _s: string; _DecSeparator: Char); overload;
    procedure AssignStr(const _s: string); overload;
    ///<summary>
    /// @returns the number converted to a string if it is valid, or the Default if it is not valid </summary>
    function ToString(const _Default: string = ''): string;
    function Dump: string;
    function Abs: _NULLABLE_TYPE_BASE_;
    function Format(const _FormatStr: string): string; overload;
    function Format(const _FormatStr: string; const _Settings: TFormatSettings): string; overload;
    class operator Negative(_a: TdzNullableNumber): TdzNullableNumber;
    class operator Positive(_a: TdzNullableNumber): TdzNullableNumber;
    class operator Inc(_a: TdzNullableNumber): TdzNullableNumber;
    class operator Dec(_a: TdzNullableNumber): TdzNullableNumber;

    class operator Add(_a, _b: TdzNullableNumber): TdzNullableNumber;
    class operator Add(_a: TdzNullableNumber; _b: _NULLABLE_TYPE_BASE_): TdzNullableNumber;
    class operator Add(_a: _NULLABLE_TYPE_BASE_; _b: TdzNullableNumber): TdzNullableNumber;

    class operator Subtract(_a, _b: TdzNullableNumber): TdzNullableNumber;
    class operator Subtract(_a: TdzNullableNumber; _b: _NULLABLE_TYPE_BASE_): TdzNullableNumber;
    class operator Subtract(_a: _NULLABLE_TYPE_BASE_; _b: TdzNullableNumber): TdzNullableNumber;

    class operator Multiply(_a, _b: TdzNullableNumber): TdzNullableNumber;
    class operator Multiply(_a: TdzNullableNumber; _b: _NULLABLE_TYPE_BASE_): TdzNullableNumber;
    class operator Multiply(_a: _NULLABLE_TYPE_BASE_; _b: TdzNullableNumber): TdzNullableNumber;

    class operator Divide(_a, _b: TdzNullableNumber): TdzNullableNumber;
    class operator Divide(_a: TdzNullableNumber; _b: _NULLABLE_TYPE_BASE_): TdzNullableNumber;
    class operator Divide(_a: _NULLABLE_TYPE_BASE_; _b: TdzNullableNumber): TdzNullableNumber;

    class operator Implicit(_Value: _NULLABLE_TYPE_BASE_): TdzNullableNumber;
    class operator Implicit(_a: TdzNullableNumber): _NULLABLE_TYPE_BASE_;

    class operator Explicit(const _s: string): TdzNullableNumber;
    class operator Explicit(_a: TdzNullableNumber): string;

    class operator LessThan(_a: TdzNullableNumber; _b: _NULLABLE_TYPE_BASE_): Boolean;
    class operator LessThanOrEqual(_a: TdzNullableNumber; _b: _NULLABLE_TYPE_BASE_): Boolean;
    class operator GreaterThan(_a: TdzNullableNumber; _b: _NULLABLE_TYPE_BASE_): Boolean;
    class operator GreaterThanOrEqual(_a: TdzNullableNumber; _b: _NULLABLE_TYPE_BASE_): Boolean;
    class operator Equal(_a: TdzNullableNumber; _b: _NULLABLE_TYPE_BASE_): Boolean;
    class operator NotEqual(_a: TdzNullableNumber; _b: _NULLABLE_TYPE_BASE_): Boolean;

    class operator LessThan(_a, _b: TdzNullableNumber): Boolean;
    class operator LessThanOrEqual(_a, _b: TdzNullableNumber): Boolean;
    class operator GreaterThan(_a, _b: TdzNullableNumber): Boolean;
    class operator GreaterThanOrEqual(_a, _b: TdzNullableNumber): Boolean;
    class operator Equal(_a: TdzNullableNumber; _b: TdzNullableNumber): Boolean;
    class operator NotEqual(_a, _b: TdzNullableNumber): Boolean;

    /// <summary> invalid values are considered smaller than any valid values
    /// and equal to each other </summary>
    class function Compare(_a, _b: TdzNullableNumber): Integer; static;
    /// <summary> invalid values are considered equal to each other </summary>
    class function IsSame(_a, _b: TdzNullableNumber): Boolean; static;
    class function Invalid: TdzNullableNumber; static;
    class function FromVariant(_a: Variant): TdzNullableNumber; static;
    class function FromStr(const _s: string): TdzNullableNumber; static;
  end;

{$ENDIF __DZ_NULLABLE_NUMBER_TEMPLATE_SECOND_PASS__}

{$IFNDEF __DZ_NULLABLE_NUMBER_TEMPLATE__}
{$DEFINE __DZ_NULLABLE_NUMBER_TEMPLATE_SECOND_PASS__}

implementation

uses
  Math,
  u_dzTranslator,
  u_dzStringUtils,
  u_dzVariantUtils;

{$ENDIF __DZ_NULLABLE_NUMBER_TEMPLATE__}

{$IFDEF __DZ_NULLABLE_NUMBER_TEMPLATE_SECOND_PASS__}

{ TdzNullableNumber }

class operator TdzNullableNumber.Negative(_a: TdzNullableNumber): TdzNullableNumber;
begin
  Result := -_a.Value;
end;

class operator TdzNullableNumber.Positive(_a: TdzNullableNumber): TdzNullableNumber;
begin
  Result := _a.Value;
end;

class operator TdzNullableNumber.Inc(_a: TdzNullableNumber): TdzNullableNumber;
begin
  Result := _a.Value + 1;
end;

class operator TdzNullableNumber.Dec(_a: TdzNullableNumber): TdzNullableNumber;
begin
  Result := _a.Value - 1;
end;

class operator TdzNullableNumber.Add(_a, _b: TdzNullableNumber): TdzNullableNumber;
begin
  if not _a.IsValid or not _b.IsValid then
    raise EInvalidValue.Create(_('Cannot add two nullable values if one of them is not valid.'));
  Result := _a.Value + _b.Value;
end;

class operator TdzNullableNumber.Add(_a: TdzNullableNumber; _b: _NULLABLE_TYPE_BASE_): TdzNullableNumber;
begin
  if not _a.IsValid then
    raise EInvalidValue.Create(_('Cannot add to a nullable value if it is not valid'));
  Result := _a.Value + _b;
end;

class operator TdzNullableNumber.Add(_a: _NULLABLE_TYPE_BASE_; _b: TdzNullableNumber): TdzNullableNumber;
begin
  if not _b.IsValid then
    raise EInvalidValue.Create(_('Cannot add to a nullable value if it is not valid'));
  Result := _a + _b.Value;
end;

class operator TdzNullableNumber.Subtract(_a, _b: TdzNullableNumber): TdzNullableNumber;
begin
  if not _a.IsValid or not _b.IsValid then
    raise EInvalidValue.Create(_('Cannot subtract two nullable values if one of them is not valid'));
  Result := _a.Value - _b.Value;
end;

class operator TdzNullableNumber.Subtract(_a: TdzNullableNumber; _b: _NULLABLE_TYPE_BASE_): TdzNullableNumber;
begin
  if not _a.IsValid then
    raise EInvalidValue.Create(_('Cannot subtract from a nullable value if it is not valid'));
  Result := _a.Value - _b;
end;

class operator TdzNullableNumber.Subtract(_a: _NULLABLE_TYPE_BASE_; _b: TdzNullableNumber): TdzNullableNumber;
begin
  if not _b.IsValid then
    raise EInvalidValue.Create(_('Cannot subtract from a value if it is not valid'));
  Result := _a - _b.Value;
end;

class operator TdzNullableNumber.Multiply(_a, _b: TdzNullableNumber): TdzNullableNumber;
begin
  if not _a.IsValid or not _b.IsValid then
    raise EInvalidValue.Create(_('Cannot multiply two nullable values if one of them is not valid'));
  Result := _a.Value * _b.Value;
end;

class operator TdzNullableNumber.Multiply(_a: TdzNullableNumber; _b: _NULLABLE_TYPE_BASE_): TdzNullableNumber;
begin
  if not _a.IsValid then
    raise EInvalidValue.Create(_('Cannot multiply a nullable value if it is not valid'));
  Result := _a.Value * _b;
end;

class operator TdzNullableNumber.Multiply(_a: _NULLABLE_TYPE_BASE_; _b: TdzNullableNumber): TdzNullableNumber;
begin
  if not _b.IsValid then
    raise EInvalidValue.Create(_('Cannot multiply a nullable value if it is not valid'));
  Result := _a * _b.Value;
end;

class operator TdzNullableNumber.Divide(_a, _b: TdzNullableNumber): TdzNullableNumber;
var
  Res: _NULLABLE_TYPE_BASE_;
begin
  if not _a.IsValid or not _b.IsValid then
    raise EInvalidValue.Create(_('Cannot divide two nullable values if one of them is not valid'));
  DivideNumbers(_a.Value, _b.Value, Res);
  Result := Res;
end;

class operator TdzNullableNumber.Divide(_a: TdzNullableNumber; _b: _NULLABLE_TYPE_BASE_): TdzNullableNumber;
var
  Res: _NULLABLE_TYPE_BASE_;
begin
  if not _a.IsValid then
    raise EInvalidValue.Create(_('Cannot divide a nullable value if it is not valid'));
  DivideNumbers(_a.Value, _b, Res);
  Result := Res;
end;

class operator TdzNullableNumber.Divide(_a: _NULLABLE_TYPE_BASE_; _b: TdzNullableNumber): TdzNullableNumber;
var
  Res: _NULLABLE_TYPE_BASE_;
begin
  if not _b.IsValid then
    raise EInvalidValue.Create(_('Cannot divide by a nullable value if it is not valid'));
  DivideNumbers(_a, _b, Res);
  Result := Res;
end;

procedure TdzNullableNumber.AssignStr(const _s: string; const _FormatSettings: TFormatSettings);
begin
  if SameText('NULL', _s) then
    FIsValid := nil
  else if TryStrToNumber(_s, FValue, _FormatSettings) then
    FIsValid := GetNullableTypesFlagInterface
  else
    FIsValid := nil;
end;

procedure TdzNullableNumber.AssignStr(const _s: string; _DecSeparator: Char);
var
  FormatSettings: TFormatSettings;
begin
  if SameText('NULL', _s) then
    FIsValid := nil
  else begin
    if _DecSeparator = #0 then
      _DecSeparator := u_dzStringUtils.GetUserDefaultLocaleSettings.DecimalSeparator;
    FormatSettings := DZ_FORMAT_DECIMAL_POINT;
    FormatSettings.DecimalSeparator := _DecSeparator;

    if TryStrToNumber(_s, FValue, FormatSettings) then
      FIsValid := GetNullableTypesFlagInterface
    else
      FIsValid := nil;
  end;
end;

procedure TdzNullableNumber.AssignStr(const _s: string);
begin
  AssignStr(_s, UserLocaleFormatSettings^);
end;

class operator TdzNullableNumber.Explicit(const _s: string): TdzNullableNumber;
begin
  if TryStrToNumber(_s, Result.FValue, UserLocaleFormatSettings^) then
    Result.FIsValid := GetNullableTypesFlagInterface
  else
    Result.FIsValid := nil;
end;

class operator TdzNullableNumber.Explicit(_a: TdzNullableNumber): string;
begin
  if _a.IsValid then
    Result := NumberToStr(_a.Value)
  else
    Result := '';
end;

class function TdzNullableNumber.FromVariant(_a: Variant): TdzNullableNumber;
begin
  Result.AssignVariant(_a);
end;

class function TdzNullableNumber.FromStr(const _s: string): TdzNullableNumber;
begin
  Result.AssignStr(_s);
end;

class operator TdzNullableNumber.Implicit(_Value: _NULLABLE_TYPE_BASE_): TdzNullableNumber;
begin
  Result.FValue := _Value;
  Result.FIsValid := GetNullableTypesFlagInterface;
end;

class operator TdzNullableNumber.Implicit(_a: TdzNullableNumber): _NULLABLE_TYPE_BASE_;
begin
  Result := _a.Value;
end;

procedure TdzNullableNumber.AssignVariant(_a: Variant);
begin
  if TryVar2Number(_a, FValue) then
    FIsValid := GetNullableTypesFlagInterface
  else
    FIsValid := nil;
end;

class function TdzNullableNumber.Compare(_a, _b: TdzNullableNumber): Integer;
begin
  if _a.IsValid then begin
    if _b.IsValid then
      Result := Math.CompareValue(_a.Value, _b.Value)
    else
      Result := 1;
  end else if not _b.IsValid then
    Result := 0
  else
    Result := -1;
end;

class function TdzNullableNumber.IsSame(_a, _b: TdzNullableNumber): Boolean;
begin
  if _a.IsValid then begin
    if _b.IsValid then
      Result := (_a.FValue = _b.FValue)
    else
      Result := False
  end else begin
    if _b.IsValid then
      Result := False
    else
      Result := True;
  end;
end;

function TdzNullableNumber.Dump: string;
begin
  Result := ToString('<invalid>'); // do not translate
end;

function TdzNullableNumber.ToString(const _Default: string): string;
begin
  if IsValid then
    Result := NumberToStr(FValue)
  else
    Result := _Default;
end;

function TdzNullableNumber.Abs: _NULLABLE_TYPE_BASE_;
begin
  Result := System.Abs(Value);
end;

function TdzNullableNumber.Format(const _FormatStr: string): string;
begin
  Result := SysUtils.Format(_FormatStr, [Value]);
end;

function TdzNullableNumber.Format(const _FormatStr: string; const _Settings: TFormatSettings): string;
begin
  Result := SysUtils.Format(_FormatStr, [Value], _Settings);
end;

function TdzNullableNumber.ToVariant: Variant;
begin
  if IsValid then
    Result := Value
  else
    Result := Variants.Null;
end;

function TdzNullableNumber.GetValue(out _Value: _NULLABLE_TYPE_BASE_): Boolean;
begin
  Result := IsValid;
  if Result then
    _Value := FValue;
end;

procedure TdzNullableNumber.Invalidate;
begin
  FIsValid := nil;
end;

function TdzNullableNumber.IsValid: Boolean;
begin
  Result := FIsValid <> nil;
end;

class operator TdzNullableNumber.LessThan(_a: TdzNullableNumber; _b: _NULLABLE_TYPE_BASE_): Boolean;
begin
  Result := CompareValue(_a.Value, _b) < 0;
end;

class operator TdzNullableNumber.LessThanOrEqual(_a: TdzNullableNumber; _b: _NULLABLE_TYPE_BASE_): Boolean;
begin
  Result := CompareValue(_a.Value, _b) <= 0;
end;

class operator TdzNullableNumber.GreaterThan(_a: TdzNullableNumber; _b: _NULLABLE_TYPE_BASE_): Boolean;
begin
  Result := CompareValue(_a.Value, _b) > 0;
end;

class operator TdzNullableNumber.GreaterThanOrEqual(_a: TdzNullableNumber; _b: _NULLABLE_TYPE_BASE_): Boolean;
begin
  Result := CompareValue(_a.Value, _b) >= 0;
end;

class operator TdzNullableNumber.Equal(_a: TdzNullableNumber; _b: _NULLABLE_TYPE_BASE_): Boolean;
begin
  Result := SameValue(_a.Value, _b);
end;

class operator TdzNullableNumber.NotEqual(_a: TdzNullableNumber; _b: _NULLABLE_TYPE_BASE_): Boolean;
begin
  Result := not SameValue(_a.Value, _b);
end;

class operator TdzNullableNumber.LessThan(_a, _b: TdzNullableNumber): Boolean;
begin
  Result := (Compare(_a, _b) < 0);
end;

class operator TdzNullableNumber.LessThanOrEqual(_a, _b: TdzNullableNumber): Boolean;
begin
  Result := (Compare(_a, _b) <= 0);
end;

class operator TdzNullableNumber.GreaterThan(_a, _b: TdzNullableNumber): Boolean;
begin
  Result := (Compare(_a, _b) > 0);
end;

class operator TdzNullableNumber.GreaterThanOrEqual(_a, _b: TdzNullableNumber): Boolean;
begin
  Result := (Compare(_a, _b) >= 0);
end;

class operator TdzNullableNumber.Equal(_a: TdzNullableNumber; _b: TdzNullableNumber): Boolean;
begin
  if _a.IsValid then begin
    if _b.IsValid then begin
      // both are valid, compare the values
      Result := SameValue(_a.Value, _b.Value);
    end else begin
      // one is valid the other isn't: they are not equal
      Result := False
    end;
  end else begin
    if _b.IsValid then begin
      // one is valid the other isn't: they are not equal
      Result := False;
    end else begin
      // both are invalid: They are equal
      Result := True
    end;
  end;
end;

class operator TdzNullableNumber.NotEqual(_a: TdzNullableNumber; _b: TdzNullableNumber): Boolean;
begin
  Result := not (_a = _b);
end;

class function TdzNullableNumber.Invalid: TdzNullableNumber;
begin
  Result.Invalidate;
end;

function TdzNullableNumber.Value: _NULLABLE_TYPE_BASE_;
begin
  if not IsValid then
    raise EInvalidValue.CreateFmt(_('%s is invalid'), [_NULLABLE_TYPE_NAME_]);
  Result := FValue;
end;

{$ENDIF __DZ_NULLABLE_NUMBER_TEMPLATE_SECOND_PASS__}

{$DEFINE __DZ_NULLABLE_NUMBER_TEMPLATE_SECOND_PASS__}

{$IFNDEF __DZ_NULLABLE_NUMBER_TEMPLATE__}
{$WARNINGS OFF}
end.
{$ENDIF __DZ_NULLABLE_NUMBER_TEMPLATE__}
