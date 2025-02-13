unit RipGrepper.Settings.SettingVariant;

interface

uses
	System.Variants,
	System.Generics.Defaults,
	System.IniFiles,
	System.SysUtils;

type

	ESettingsException = class(Exception);

	ISettingVariant = interface
		['{D4A1E2B3-5F6C-4A7D-8B9E-1C2D3E4F5A6B}']
		function Equals(_other : ISettingVariant) : Boolean;
		function IsEmpty : Boolean;

		function GetIsDefaultRelevant : Boolean;
		function GetIsModified : Boolean;
		function GetSaveToIni : Boolean;
		function GetValue : Variant;
		function GetValueType : TVarType;

		procedure SetIsDefaultRelevant(const Value : Boolean);
		procedure SetIsModified(const Value : Boolean);
		procedure SetValue(const Value : Variant);
		procedure SetSaveToIni(const Value : Boolean);

		property IsDefaultRelevant : Boolean read GetIsDefaultRelevant write SetIsDefaultRelevant;
		property IsModified : Boolean read GetIsModified write SetIsModified;
		property Value : Variant read GetValue write SetValue;
		property ValueType : TVarType read GetValueType;
		property SaveToIni : Boolean read GetSaveToIni write SetSaveToIni;

		procedure WriteToMemIni(_ini : TMemIniFile; const _sIniSection, _sKey : string);
	end;

	TSettingVariant = class(TInterfacedObject, ISettingVariant)
		private
			FValue : Variant;
			FValueType : TVarType;
			FIsModified : Boolean;
			FIsDefaultRelevant : Boolean;
			FSaveToIni : Boolean; // New field
			function CompareTo(Value : ISettingVariant) : Integer;
			function GetValue : Variant;
			function GetIsModified : Boolean;
			function GetIsDefaultRelevant : Boolean;
			function GetValueType() : TVarType;
			function GetSaveToIni : Boolean;
			procedure SetValue(const Value : Variant);
			procedure SetIsModified(const Value : Boolean);
			procedure SetIsDefaultRelevant(const Value : Boolean);
			procedure SetSaveToIni(const Value : Boolean);

		public
			constructor Create(const _type : TVarType; const _value : Variant; const _isDefRelevant : Boolean = False;
				const _saveToIni : Boolean = True); overload;
			constructor Create(const _value : Variant); overload;
			destructor Destroy; override;
			function Equals(_other : ISettingVariant) : Boolean; reintroduce;
			function IsEmpty : Boolean;
			procedure WriteToMemIni(_ini : TMemIniFile; const _sIniSection, _sKey : string);
			property Value : Variant read GetValue write SetValue;
			property IsModified : Boolean read GetIsModified write SetIsModified;
			property IsDefaultRelevant : Boolean read GetIsDefaultRelevant write SetIsDefaultRelevant;
			property ValueType : TVarType read GetValueType;
			property SaveToIni : Boolean read GetSaveToIni write SetSaveToIni;
	end;

implementation

uses
	RipGrepper.Tools.DebugUtils;

constructor TSettingVariant.Create(const _type : TVarType; const _value : Variant; const _isDefRelevant : Boolean = False;
	const _saveToIni : Boolean = True);
begin
	FValueType := _type;
	FValue := _value;
	FIsModified := False;
	FIsDefaultRelevant := _isDefRelevant;
	FSaveToIni := _saveToIni;
end;

constructor TSettingVariant.Create(const _value : Variant);
begin
	FValueType := VarType(_value);
	FValue := _value;
	FIsModified := False;
	FIsDefaultRelevant := False;
	FSaveToIni := True;
end;

destructor TSettingVariant.Destroy;
begin
	inherited;
end;

function TSettingVariant.CompareTo(Value : ISettingVariant) : Integer;
var
	res : TVariantRelationship;
begin
	res := VarCompareValue(self.FValue, Value.Value);
	if res = vrEqual then begin
		if FValueType <> Value.ValueType then
			Result := Ord(FValueType) - Ord(Value.ValueType)
		else if FIsModified <> Value.IsModified then
			Result := Ord(FIsModified) - Ord(Value.IsModified)
		else if FIsDefaultRelevant <> Value.IsDefaultRelevant then
			Result := Ord(FIsDefaultRelevant) - Ord(Value.IsDefaultRelevant)
		else if FSaveToIni <> Value.SaveToIni then
			Result := Ord(FSaveToIni) - Ord(Value.SaveToIni)
		else
			Result := 0;
		Exit;
	end;

	Result := Integer(res);
end;

function TSettingVariant.Equals(_other : ISettingVariant) : Boolean;
begin
	Result := (VarCompareValue(FValue, _other.Value) = vrEqual) and
	{ } (FValueType = _other.ValueType) and
	{ } (FIsModified = _other.IsModified) and
	{ } (FIsDefaultRelevant = _other.IsDefaultRelevant) and
	{ } (FSaveToIni = _other.SaveToIni);
end;

function TSettingVariant.IsEmpty : Boolean;
begin
	Result := VarIsEmpty(Value) or VarIsNull(Value);
end;

function TSettingVariant.GetValue : Variant;
begin
	Result := FValue;
end;

function TSettingVariant.GetIsModified : Boolean;
begin
	Result := FIsModified;
end;

function TSettingVariant.GetIsDefaultRelevant : Boolean;
begin
	Result := FIsDefaultRelevant;
end;

function TSettingVariant.GetValueType() : TVarType;
begin
	Result := FValueType;
end;

function TSettingVariant.GetSaveToIni : Boolean;
begin
	Result := FSaveToIni;
end;

procedure TSettingVariant.SetValue(const Value : Variant);
begin
	FValue := Value;
	FValueType := VarType(Value);
end;

procedure TSettingVariant.SetIsModified(const Value : Boolean);
begin
	FIsModified := Value;
end;

procedure TSettingVariant.SetIsDefaultRelevant(const Value : Boolean);
begin
	FIsDefaultRelevant := Value;
end;

procedure TSettingVariant.SetSaveToIni(const Value : Boolean);
begin
	FSaveToIni := Value;
end;

procedure TSettingVariant.WriteToMemIni(_ini : TMemIniFile; const _sIniSection, _sKey : string);
var
	v : Variant;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TSettingVariant.InnerLoadDefaultsFromDict');

	try
		case self.ValueType of
			varString, varUString : begin
				dbgMsg.MsgIf(_sIniSection = 'SearchTextsHistory', Format('SearchTextsHistory %s=%s', [_sKey, self.Value]));
				_ini.WriteString(_sIniSection, _sKey, self.Value);
			end;
			varBoolean : begin
				_ini.WriteBool(_sIniSection, _sKey, self.Value);
			end;
			varInteger : begin
				_ini.WriteInteger(_sIniSection, _sKey, self.Value);
			end;
			$200C, varArray : begin // varTypeMask   ??
				var
				i := VarArrayLowBound(self.Value, 1);
				var
				len := VarArrayHighBound(self.Value, 1);
				dbgMsg.Msg('Write Array');
				while i <= len do begin
					v := self.Value[i]; // v should be string
					_ini.WriteString(_sIniSection, Format('%s_Item%d', [_sKey, i]), v);
					Inc(i);
				end;
			end
			else
			// var and vtTypes are not the same!!!
			raise ESettingsException.Create('Settings Type not supported: ' + { } VarTypeAsText(VarType(self.ValueType)));
		end;
	except
		on E : Exception do
			dbgMsg.ErrorMsgFmt('%s', [E.Message]);
	end;
	if self.ValueType <> $200C then begin
		dbgMsg.MsgFmt('[%s].%s=%s in %s', [_sIniSection, _sKey, VarToStr(self.Value), _ini.FileName]);
	end;
end;

end.
