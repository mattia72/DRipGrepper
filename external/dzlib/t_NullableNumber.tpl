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
  _NULLABLE_NUMBER_ = record
  private
    FIsValid: IInterface;
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
    class operator Negative(_a: _NULLABLE_NUMBER_): _NULLABLE_NUMBER_;
    class operator Positive(_a: _NULLABLE_NUMBER_): _NULLABLE_NUMBER_;
    class operator Inc(_a: _NULLABLE_NUMBER_): _NULLABLE_NUMBER_;
    class operator Dec(_a: _NULLABLE_NUMBER_): _NULLABLE_NUMBER_;

    class operator Add(_a, _b: _NULLABLE_NUMBER_): _NULLABLE_NUMBER_;
    class operator Add(_a: _NULLABLE_NUMBER_; _b: _NULLABLE_TYPE_BASE_): _NULLABLE_NUMBER_;
    class operator Add(_a: _NULLABLE_TYPE_BASE_; _b: _NULLABLE_NUMBER_): _NULLABLE_NUMBER_;

    class operator Subtract(_a, _b: _NULLABLE_NUMBER_): _NULLABLE_NUMBER_;
    class operator Subtract(_a: _NULLABLE_NUMBER_; _b: _NULLABLE_TYPE_BASE_): _NULLABLE_NUMBER_;
    class operator Subtract(_a: _NULLABLE_TYPE_BASE_; _b: _NULLABLE_NUMBER_): _NULLABLE_NUMBER_;

    class operator Multiply(_a, _b: _NULLABLE_NUMBER_): _NULLABLE_NUMBER_;
    class operator Multiply(_a: _NULLABLE_NUMBER_; _b: _NULLABLE_TYPE_BASE_): _NULLABLE_NUMBER_;
    class operator Multiply(_a: _NULLABLE_TYPE_BASE_; _b: _NULLABLE_NUMBER_): _NULLABLE_NUMBER_;

    class operator Divide(_a, _b: _NULLABLE_NUMBER_): _NULLABLE_NUMBER_;
    class operator Divide(_a: _NULLABLE_NUMBER_; _b: _NULLABLE_TYPE_BASE_): _NULLABLE_NUMBER_;
    class operator Divide(_a: _NULLABLE_TYPE_BASE_; _b: _NULLABLE_NUMBER_): _NULLABLE_NUMBER_;

    class operator Implicit(_Value: _NULLABLE_TYPE_BASE_): _NULLABLE_NUMBER_;
    class operator Implicit(_a: _NULLABLE_NUMBER_): _NULLABLE_TYPE_BASE_;

    class operator Explicit(const _s: string): _NULLABLE_NUMBER_;
    class operator Explicit(_a: _NULLABLE_NUMBER_): string;

    class operator LessThan(_a: _NULLABLE_NUMBER_; _b: _NULLABLE_TYPE_BASE_): Boolean;
    class operator LessThanOrEqual(_a: _NULLABLE_NUMBER_; _b: _NULLABLE_TYPE_BASE_): Boolean;
    class operator GreaterThan(_a: _NULLABLE_NUMBER_; _b: _NULLABLE_TYPE_BASE_): Boolean;
    class operator GreaterThanOrEqual(_a: _NULLABLE_NUMBER_; _b: _NULLABLE_TYPE_BASE_): Boolean;
    class operator Equal(_a: _NULLABLE_NUMBER_; _b: _NULLABLE_TYPE_BASE_): Boolean;
    class operator NotEqual(_a: _NULLABLE_NUMBER_; _b: _NULLABLE_TYPE_BASE_): Boolean;

    class operator LessThan(_a, _b: _NULLABLE_NUMBER_): Boolean;
    class operator LessThanOrEqual(_a, _b: _NULLABLE_NUMBER_): Boolean;
    class operator GreaterThan(_a, _b: _NULLABLE_NUMBER_): Boolean;
    class operator GreaterThanOrEqual(_a, _b: _NULLABLE_NUMBER_): Boolean;
    class operator Equal(_a: _NULLABLE_NUMBER_; _b: _NULLABLE_NUMBER_): Boolean;
    class operator NotEqual(_a, _b: _NULLABLE_NUMBER_): Boolean;

    /// <summary> invalid values are considered smaller than any valid values
    /// and equal to each other </summary>
    class function Compare(_a, _b: _NULLABLE_NUMBER_): Integer; static;
    class function Invalid: _NULLABLE_NUMBER_; static;
    class function FromVariant(_a: Variant): _NULLABLE_NUMBER_; static;
    class function FromStr(const _s: string): _NULLABLE_NUMBER_; static;
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

{ _NULLABLE_NUMBER_ }

class operator _NULLABLE_NUMBER_.Negative(_a: _NULLABLE_NUMBER_): _NULLABLE_NUMBER_;
begin
  Result := -_a.Value;
end;

class operator _NULLABLE_NUMBER_.Positive(_a: _NULLABLE_NUMBER_): _NULLABLE_NUMBER_;
begin
  Result := _a.Value;
end;

class operator _NULLABLE_NUMBER_.Inc(_a: _NULLABLE_NUMBER_): _NULLABLE_NUMBER_;
begin
  Result := _a.Value + 1;
end;

class operator _NULLABLE_NUMBER_.Dec(_a: _NULLABLE_NUMBER_): _NULLABLE_NUMBER_;
begin
  Result := _a.Value - 1;
end;

class operator _NULLABLE_NUMBER_.Add(_a, _b: _NULLABLE_NUMBER_): _NULLABLE_NUMBER_;
begin
  if not _a.IsValid or not _b.IsValid then
    raise EInvalidValue.Create(_('Cannot add two nullable values if one of them is not valid.'));
  Result := _a.Value + _b.Value;
end;

class operator _NULLABLE_NUMBER_.Add(_a: _NULLABLE_NUMBER_; _b: _NULLABLE_TYPE_BASE_): _NULLABLE_NUMBER_;
begin
  if not _a.IsValid then
    raise EInvalidValue.Create(_('Cannot add to a nullable value if it is not valid'));
  Result := _a.Value + _b;
end;

class operator _NULLABLE_NUMBER_.Add(_a: _NULLABLE_TYPE_BASE_; _b: _NULLABLE_NUMBER_): _NULLABLE_NUMBER_;
begin
  if not _b.IsValid then
    raise EInvalidValue.Create(_('Cannot add to a nullable value if it is not valid'));
  Result := _a + _b.Value;
end;

class operator _NULLABLE_NUMBER_.Subtract(_a, _b: _NULLABLE_NUMBER_): _NULLABLE_NUMBER_;
begin
  if not _a.IsValid or not _b.IsValid then
    raise EInvalidValue.Create(_('Cannot subtract two nullable values if one of them is not valid'));
  Result := _a.Value - _b.Value;
end;

class operator _NULLABLE_NUMBER_.Subtract(_a: _NULLABLE_NUMBER_; _b: _NULLABLE_TYPE_BASE_): _NULLABLE_NUMBER_;
begin
  if not _a.IsValid then
    raise EInvalidValue.Create(_('Cannot subtract from a nullable value if it is not valid'));
  Result := _a.Value - _b;
end;

class operator _NULLABLE_NUMBER_.Subtract(_a: _NULLABLE_TYPE_BASE_; _b: _NULLABLE_NUMBER_): _NULLABLE_NUMBER_;
begin
  if not _b.IsValid then
    raise EInvalidValue.Create(_('Cannot subtract from a value if it is not valid'));
  Result := _a - _b.Value;
end;

class operator _NULLABLE_NUMBER_.Multiply(_a, _b: _NULLABLE_NUMBER_): _NULLABLE_NUMBER_;
begin
  if not _a.IsValid or not _b.IsValid then
    raise EInvalidValue.Create(_('Cannot multiply two nullable values if one of them is not valid'));
  Result := _a.Value * _b.Value;
end;

class operator _NULLABLE_NUMBER_.Multiply(_a: _NULLABLE_NUMBER_; _b: _NULLABLE_TYPE_BASE_): _NULLABLE_NUMBER_;
begin
  if not _a.IsValid then
    raise EInvalidValue.Create(_('Cannot multiply a nullable value if it is not valid'));
  Result := _a.Value * _b;
end;

class operator _NULLABLE_NUMBER_.Multiply(_a: _NULLABLE_TYPE_BASE_; _b: _NULLABLE_NUMBER_): _NULLABLE_NUMBER_;
begin
  if not _b.IsValid then
    raise EInvalidValue.Create(_('Cannot multiply a nullable value if it is not valid'));
  Result := _a * _b.Value;
end;

class operator _NULLABLE_NUMBER_.Divide(_a, _b: _NULLABLE_NUMBER_): _NULLABLE_NUMBER_;
var
  Res: _NULLABLE_TYPE_BASE_;
begin
  if not _a.IsValid or not _b.IsValid then
    raise EInvalidValue.Create(_('Cannot divide two nullable values if one of them is not valid'));
  DivideNumbers(_a.Value, _b.Value, Res);
  Result := Res;
end;

class operator _NULLABLE_NUMBER_.Divide(_a: _NULLABLE_NUMBER_; _b: _NULLABLE_TYPE_BASE_): _NULLABLE_NUMBER_;
var
  Res: _NULLABLE_TYPE_BASE_;
begin
  if not _a.IsValid then
    raise EInvalidValue.Create(_('Cannot divide a nullable value if it is not valid'));
  DivideNumbers(_a.Value, _b, Res);
  Result := Res;
end;

class operator _NULLABLE_NUMBER_.Divide(_a: _NULLABLE_TYPE_BASE_; _b: _NULLABLE_NUMBER_): _NULLABLE_NUMBER_;
var
  Res: _NULLABLE_TYPE_BASE_;
begin
  if not _b.IsValid then
    raise EInvalidValue.Create(_('Cannot divide by a nullable value if it is not valid'));
  DivideNumbers(_a, _b, Res);
  Result := Res;
end;

procedure _NULLABLE_NUMBER_.AssignStr(const _s: string; const _FormatSettings: TFormatSettings);
begin
  if SameText('NULL', _s) then
    FIsValid := nil
  else if TryStrToNumber(_s, FValue, _FormatSettings) then
    FIsValid := GetNullableTypesFlagInterface
  else
    FIsValid := nil;
end;

procedure _NULLABLE_NUMBER_.AssignStr(const _s: string; _DecSeparator: Char);
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

procedure _NULLABLE_NUMBER_.AssignStr(const _s: string);
begin
  AssignStr(_s, UserLocaleFormatSettings^);
end;

class operator _NULLABLE_NUMBER_.Explicit(const _s: string): _NULLABLE_NUMBER_;
begin
  if TryStrToNumber(_s, Result.FValue, UserLocaleFormatSettings^) then
    Result.FIsValid := GetNullableTypesFlagInterface
  else
    Result.FIsValid := nil;
end;

class operator _NULLABLE_NUMBER_.Explicit(_a: _NULLABLE_NUMBER_): string;
begin
  if _a.IsValid then
    Result := NumberToStr(_a.Value)
  else
    Result := '';
end;

class function _NULLABLE_NUMBER_.FromVariant(_a: Variant): _NULLABLE_NUMBER_;
begin
  Result.AssignVariant(_a);
end;

class function _NULLABLE_NUMBER_.FromStr(const _s: string): _NULLABLE_NUMBER_;
begin
  Result.AssignStr(_s);
end;

class operator _NULLABLE_NUMBER_.Implicit(_Value: _NULLABLE_TYPE_BASE_): _NULLABLE_NUMBER_;
begin
  Result.FValue := _Value;
  Result.FIsValid := GetNullableTypesFlagInterface;
end;

class operator _NULLABLE_NUMBER_.Implicit(_a: _NULLABLE_NUMBER_): _NULLABLE_TYPE_BASE_;
begin
  Result := _a.Value;
end;

procedure _NULLABLE_NUMBER_.AssignVariant(_a: Variant);
begin
  if TryVar2Number(_a, FValue) then
    FIsValid := GetNullableTypesFlagInterface
  else
    FIsValid := nil;
end;

class function _NULLABLE_NUMBER_.Compare(_a, _b: _NULLABLE_NUMBER_): Integer;
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

function _NULLABLE_NUMBER_.Dump: string;
begin
  Result := ToString('<invalid>'); // do not translate
end;

function _NULLABLE_NUMBER_.ToString(const _Default: string): string;
begin
  if IsValid then
    Result := NumberToStr(FValue)
  else
    Result := _Default;
end;

function _NULLABLE_NUMBER_.Abs: _NULLABLE_TYPE_BASE_;
begin
  Result := System.Abs(Value);
end;

function _NULLABLE_NUMBER_.Format(const _FormatStr: string): string;
begin
  Result := SysUtils.Format(_FormatStr, [Value]);
end;

function _NULLABLE_NUMBER_.Format(const _FormatStr: string; const _Settings: TFormatSettings): string;
begin
  Result := SysUtils.Format(_FormatStr, [Value], _Settings);
end;

function _NULLABLE_NUMBER_.ToVariant: Variant;
begin
  if IsValid then
    Result := Value
  else
    Result := Variants.Null;
end;

function _NULLABLE_NUMBER_.GetValue(out _Value: _NULLABLE_TYPE_BASE_): Boolean;
begin
  Result := IsValid;
  if Result then
    _Value := FValue;
end;

procedure _NULLABLE_NUMBER_.Invalidate;
begin
  FIsValid := nil;
end;

function _NULLABLE_NUMBER_.IsValid: Boolean;
begin
  Result := FIsValid <> nil;
end;

class operator _NULLABLE_NUMBER_.LessThan(_a: _NULLABLE_NUMBER_; _b: _NULLABLE_TYPE_BASE_): Boolean;
begin
  Result := CompareValue(_a.Value, _b) < 0;
end;

class operator _NULLABLE_NUMBER_.LessThanOrEqual(_a: _NULLABLE_NUMBER_; _b: _NULLABLE_TYPE_BASE_): Boolean;
begin
  Result := CompareValue(_a.Value, _b) <= 0;
end;

class operator _NULLABLE_NUMBER_.GreaterThan(_a: _NULLABLE_NUMBER_; _b: _NULLABLE_TYPE_BASE_): Boolean;
begin
  Result := CompareValue(_a.Value, _b) > 0;
end;

class operator _NULLABLE_NUMBER_.GreaterThanOrEqual(_a: _NULLABLE_NUMBER_; _b: _NULLABLE_TYPE_BASE_): Boolean;
begin
  Result := CompareValue(_a.Value, _b) >= 0;
end;

class operator _NULLABLE_NUMBER_.Equal(_a: _NULLABLE_NUMBER_; _b: _NULLABLE_TYPE_BASE_): Boolean;
begin
  Result := SameValue(_a.Value, _b);
end;

class operator _NULLABLE_NUMBER_.NotEqual(_a: _NULLABLE_NUMBER_; _b: _NULLABLE_TYPE_BASE_): Boolean;
begin
  Result := not SameValue(_a.Value, _b);
end;

class operator _NULLABLE_NUMBER_.LessThan(_a, _b: _NULLABLE_NUMBER_): Boolean;
begin
  Result := (Compare(_a, _b) < 0);
end;

class operator _NULLABLE_NUMBER_.LessThanOrEqual(_a, _b: _NULLABLE_NUMBER_): Boolean;
begin
  Result := (Compare(_a, _b) <= 0);
end;

class operator _NULLABLE_NUMBER_.GreaterThan(_a, _b: _NULLABLE_NUMBER_): Boolean;
begin
  Result := (Compare(_a, _b) > 0);
end;

class operator _NULLABLE_NUMBER_.GreaterThanOrEqual(_a, _b: _NULLABLE_NUMBER_): Boolean;
begin
  Result := (Compare(_a, _b) >= 0);
end;

class operator _NULLABLE_NUMBER_.Equal(_a: _NULLABLE_NUMBER_; _b: _NULLABLE_NUMBER_): Boolean;
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

class operator _NULLABLE_NUMBER_.NotEqual(_a: _NULLABLE_NUMBER_; _b: _NULLABLE_NUMBER_): Boolean;
begin
  Result := not (_a = _b);
end;

class function _NULLABLE_NUMBER_.Invalid: _NULLABLE_NUMBER_;
begin
  Result.Invalidate;
end;

function _NULLABLE_NUMBER_.Value: _NULLABLE_TYPE_BASE_;
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
