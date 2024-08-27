unit RipGrepper.Common.Settings.SettingVariant;

interface

uses
	System.Variants,
	System.Generics.Defaults;

type
	ISettingVariant = interface
		function Equals(_other : ISettingVariant) : Boolean;
		function GetDefaultValue : Variant;
		function GetInitialValue : Variant;
		function GetIsDefaultRelevant : Boolean;
		function GetIsModified : Boolean;
		function GetValue : Variant;
		function GetValueType : TVarType;
		function IsEmpty : Boolean;
		procedure SetDefaultValue(const Value : Variant);
		procedure SetInitialValue(const Value : Variant);
		procedure SetIsDefaultRelevant(const Value : Boolean);
		procedure SetIsModified(const Value : Boolean);
		procedure SetValue(const Value : Variant);
		procedure SetValueType(const Value : TVarType);
		property DefaultValue : Variant read GetDefaultValue write SetDefaultValue;
		property InitialValue : Variant read GetInitialValue write SetInitialValue;
		property IsDefaultRelevant : Boolean read GetIsDefaultRelevant write SetIsDefaultRelevant;
		property IsModified : Boolean read GetIsModified write SetIsModified;
		property Value : Variant read GetValue write SetValue;
		property ValueType : TVarType read GetValueType write SetValueType;
	end;

	TSettingVariant = class(TInterfacedObject, ISettingVariant)

		private
			FDefaultValue : Variant;
			FValue : Variant;
			FValueType : TVarType;
			FInitialValue : Variant;
			FIsModified : Boolean;
			FIsDefaultRelevant : Boolean;
			function GetDefaultValue : Variant;
			function GetValue : Variant;
			function GetInitialValue : Variant;
			function GetIsModified : Boolean;
			function GetIsDefaultRelevant : Boolean;
			function GetValueType : TVarType;
			procedure SetDefaultValue(const Value : Variant);
			procedure SetValue(const Value : Variant);
			procedure SetInitialValue(const Value : Variant);
			procedure SetIsModified(const Value : Boolean);
			procedure SetIsDefaultRelevant(const Value : Boolean);
			procedure SetValueType(const Value : TVarType);

		public
			constructor Create(const _type : TVarType; const _value : Variant; const _isDefRelevant : Boolean = False); overload;
			constructor Create(const _value : Variant); overload;
			destructor Destroy; override;
			function CompareTo(Value : ISettingVariant) : Integer;
			function Equals(_other : ISettingVariant) : Boolean;
			function IsEmpty : Boolean;
			property DefaultValue : Variant read GetDefaultValue write SetDefaultValue;
			property Value : Variant read GetValue write SetValue;
			property InitialValue : Variant read GetInitialValue write SetInitialValue;
			property IsModified : Boolean read GetIsModified write SetIsModified;
			property IsDefaultRelevant : Boolean read GetIsDefaultRelevant write SetIsDefaultRelevant;
			property ValueType : TVarType read GetValueType write SetValueType;

	end;

implementation

constructor TSettingVariant.Create(const _type : TVarType; const _value : Variant; const _isDefRelevant : Boolean = False);
begin
	FValueType := _type;
	FValue := _value;
	FInitialValue := _value;
	FIsModified := False;
	FIsDefaultRelevant := _isDefRelevant;
end;

constructor TSettingVariant.Create(const _value : Variant);
begin
	FValueType := VarType(_value);
	FValue := _value;
	FInitialValue := _value;
	FIsModified := False;
	FIsDefaultRelevant := False;
end;

destructor TSettingVariant.Destroy;
begin
	inherited;
end;

function TSettingVariant.CompareTo(Value : ISettingVariant) : Integer;
begin
	Result := Integer(VarCompareValue(self.FValue, Value.Value));
end;

function TSettingVariant.Equals(_other : ISettingVariant) : Boolean;
begin
	Result := _other.Value = Value;
end;

function TSettingVariant.GetDefaultValue : Variant;
begin
	Result := FDefaultValue;
end;

function TSettingVariant.GetValue : Variant;
begin
	Result := FValue;
end;

function TSettingVariant.GetInitialValue : Variant;
begin
	Result := FInitialValue;
end;

function TSettingVariant.GetIsModified : Boolean;
begin
	Result := FIsModified;
end;

function TSettingVariant.GetIsDefaultRelevant : Boolean;
begin
	Result := FIsDefaultRelevant;
end;

function TSettingVariant.GetValueType : TVarType;
begin
	Result := FValueType;
end;

function TSettingVariant.IsEmpty : Boolean;
begin
	Result := VarIsEmpty(Value) or VarIsNull(Value);
end;

procedure TSettingVariant.SetDefaultValue(const Value : Variant);
begin
	FDefaultValue := Value;
end;

procedure TSettingVariant.SetValue(const Value : Variant);
begin
	FValue := Value;
end;

procedure TSettingVariant.SetInitialValue(const Value : Variant);
begin
	FInitialValue := Value;
end;

procedure TSettingVariant.SetIsModified(const Value : Boolean);
begin
	FIsModified := Value;
end;

procedure TSettingVariant.SetIsDefaultRelevant(const Value : Boolean);
begin
	FIsDefaultRelevant := Value;
end;

procedure TSettingVariant.SetValueType(const Value : TVarType);
begin
	FValueType := Value;
end;

end.
