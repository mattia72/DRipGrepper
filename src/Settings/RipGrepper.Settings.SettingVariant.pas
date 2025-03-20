unit RipGrepper.Settings.SettingVariant;

interface

uses
	System.Variants,
	System.Generics.Defaults,
	System.IniFiles,
	RipGrepper.Settings.FilePersister,
	ArrayEx,
	System.SysUtils;

type

	ESettingsException = class(Exception);

	TSettingState = (ssNotSet, ssInitialized, ssModified, ssStored, ssSaved);
	TSettingSaveBehaviour = (ssbNotSet,
		{ } ssbSaveIfModified,
		{ } ssbSaveAfterChangeImmediately, // TODO
		{ } ssbSaveEvenIfNotModified);
	TSettingType = (stNotSet, stString, stInteger, stBool, stStrArray);

	// forward declarations...
	TSettingVariant<T> = class;
	ISettingVariant<T> = interface;

	TStringSetting = class;
	TBoolSetting = class;
	TIntegerSetting = class;
	TArraySetting = class;

	IStringSetting = ISettingVariant<string>;
	IBoolSetting = ISettingVariant<Boolean>;
	IIntegerSetting = ISettingVariant<Integer>;
	IArraySetting = ISettingVariant<TArrayEx<string>>;

	ISetting = interface
		['{289A58E3-A490-4015-9AFE-52EB303B9B89}']
		function Equals(_other : ISetting) : Boolean;

		procedure Copy(_other : ISetting);
		function CompareTo(Value : ISetting) : Integer;

		function GetState() : TSettingState;
		procedure SetState(const Value : TSettingState);

		function GetType() : TSettingType;

		procedure LoadFromPersister();
		procedure StoreToPersister(const _section : string = '');

		function AsStringSetting() : IStringSetting;
		function AsIntegerSetting() : IIntegerSetting;
		function AsBoolSetting() : IBoolSetting;
		function AsArraySetting() : IArraySetting;

		function AsString() : string;
		function AsInteger() : Integer;
		function AsBool() : Boolean;
		function AsArray() : TArrayEx<string>;
		function GetSaveBehaviour() : TSettingSaveBehaviour;
		procedure SetSaveBehaviour(const Value : TSettingSaveBehaviour);

		property State : TSettingState read GetState write SetState;
		property SettingType : TSettingType read GetType;
		property SaveBehaviour : TSettingSaveBehaviour read GetSaveBehaviour write SetSaveBehaviour;

	end;

	ISettingVariant<T> = interface(ISetting)
		['{D4A1E2B3-5F6C-4A7D-8B9E-1C2D3E4F5A6B}']
		function Equals(_other : ISettingVariant<T>) : Boolean;
		function IsEmpty : Boolean;

		function CompareTo(Value : ISettingVariant<T>) : Integer;
		procedure Copy(_other : ISettingVariant<T>);

		function GetValue : T;
		procedure SetValue(const Value : T);

		procedure SetPersister(const Value : IFilePersister<T>);
		function GetPersister() : IFilePersister<T>;

		property Persister : IFilePersister<T> read GetPersister write SetPersister;
		property Value : T read GetValue write SetValue;
	end;

	TSetting = class(TInterfacedObject, ISetting)
		private
			FSaveBehaviour : TSettingSaveBehaviour;
			FState : TSettingState;

			function GetState() : TSettingState;
			procedure SetState(const Value : TSettingState);

		protected
			function GetType() : TSettingType; virtual; abstract;

		public
			constructor Create(); overload;
			procedure LoadFromPersister(); virtual; abstract;
			procedure StoreToPersister(const _section : string = ''); virtual; abstract;

			function AsStringSetting() : IStringSetting;
			function AsIntegerSetting() : IIntegerSetting;
			function AsBoolSetting() : IBoolSetting;
			function AsArraySetting() : IArraySetting;

			function AsString() : string;
			function AsInteger() : Integer;
			function AsBool() : Boolean;
			function AsArray() : TArrayEx<string>;
			function CompareTo(Value : ISetting) : Integer;
			procedure Copy(_other : ISetting);
			class procedure CopySettingValues(_from, _to : ISetting);

			function Equals(_other : ISetting) : Boolean; reintroduce;
			function GetSaveBehaviour() : TSettingSaveBehaviour;
			procedure SetSaveBehaviour(const Value : TSettingSaveBehaviour);

			property State : TSettingState read GetState write SetState;
			property SaveBehaviour : TSettingSaveBehaviour read GetSaveBehaviour write SetSaveBehaviour;

			property SettingType : TSettingType read GetType;
	end;

	TSettingVariant<T> = class(TSetting, ISettingVariant<T>, ISetting)
		private
			FValue : T;
			FPersister : IFilePersister<T>;

			function GetValue() : T;
			procedure SetValue(const Value : T);

		protected
			function GetPersister() : IFilePersister<T>;
			function GetType() : TSettingType; override;
			procedure SetPersister(const Value : IFilePersister<T>);

		public
			constructor Create(const _value : T); overload;
			function CompareTo(Value : ISettingVariant<T>) : Integer;
			procedure Copy(_other : ISettingVariant<T>); reintroduce;
			function Equals(_other : ISettingVariant<T>) : Boolean; reintroduce;
			function IsEmpty : Boolean;
			procedure LoadFromPersister(); override;
			procedure StoreToPersister(const _section : string = ''); override;
			property Persister : IFilePersister<T> read GetPersister write SetPersister;
			property Value : T read GetValue write SetValue;
	end;

	TStringSetting = class(TSettingVariant<string>)
		public
			function GetType() : TSettingType; override;
	end;

	TBoolSetting = class(TSettingVariant<Boolean>)
		public
			function GetType() : TSettingType; override;
	end;

	TIntegerSetting = class(TSettingVariant<integer>)
		public
			function GetType() : TSettingType; override;
	end;

	TArraySetting = class(TSettingVariant < TArrayEx < string >> )
		private
			function GetCount() : Integer;
			function GetItem(Index : Integer) : string;
			function GetSafeItem(index : Integer) : string;
			procedure SetItem(Index : Integer; const Value : string);
			procedure SetSafeItem(index : Integer; const Value : string);

		public
			function AddIfNotContains(const AItem : string) : Integer;
			function GetType() : TSettingType; override;
			property Count : Integer read GetCount;
			property Item[index : Integer] : string read GetItem write SetItem; default;
			property SafeItem[index : Integer] : string read GetSafeItem write SetSafeItem;
	end;

implementation

uses
	RipGrepper.Tools.DebugUtils;

constructor TSettingVariant<T>.Create(const _value : T);
begin
	inherited Create();
	FValue := _value;
	FState := ssInitialized;
end;

function TSettingVariant<T>.CompareTo(Value : ISettingVariant<T>) : Integer;
var
	res : integer;
begin
	res := TComparer<T>.Default.Compare(self.FValue, Value.Value);
	if res = 0 then begin
		res := TComparer<integer>.Default.Compare(integer(FState), integer(Value.State));
	end;
	Result := res;
end;

procedure TSettingVariant<T>.Copy(_other : ISettingVariant<T>);
begin
	inherited Copy(_other);
	FValue := _other.Value;
	FPersister := _other.Persister;
end;

function TSettingVariant<T>.Equals(_other : ISettingVariant<T>) : Boolean;
begin
	Result := inherited Equals(_other);
	Result := Result and (TComparer<T>.Default.Compare(FValue, _other.Value) = 0);
end;

function TSettingVariant<T>.GetPersister() : IFilePersister<T>;
begin
	Result := FPersister;
end;

function TSettingVariant<T>.GetType() : TSettingType;
begin
	Result := stNotSet;
end;

function TSettingVariant<T>.IsEmpty : Boolean;
begin
	Result := (FState = ssNotSet);
end;

procedure TSettingVariant<T>.SetValue(const Value : T);
begin
	if FValue <> Value then begin
		FValue := Value;
		FState := ssModified
	end;
end;

function TSettingVariant<T>.GetValue() : T;
begin
	Result := FValue;
end;

procedure TSettingVariant<T>.LoadFromPersister();
var
	val : T;
begin
	if Persister.TryLoadValue(val) then begin
		Value := val;
	end;
end;

procedure TSettingVariant<T>.StoreToPersister(const _section : string = '');
begin
	if not Assigned(Persister) then begin
		raise ESettingsException.Create('Persister is not assigned.');
	end;

	Persister.StoreToPersister(Value);
	State := ssStored;
end;

procedure TSettingVariant<T>.SetPersister(const Value : IFilePersister<T>);
begin
	FPersister := Value;
end;

constructor TSetting.Create();
begin
	FSaveBehaviour := ssbSaveIfModified;
end;

function TSetting.AsArray() : TArrayEx<string>;
begin
	Result := AsArraySetting.Value;
end;

function TSetting.AsArraySetting() : IArraySetting;
begin
	Result := TArraySetting(self);
end;

function TSetting.AsBool() : Boolean;
begin
	Result := AsBoolSetting.Value;
end;

function TSetting.AsBoolSetting() : IBoolSetting;
begin
	Result := TBoolSetting(self);
end;

function TSetting.AsInteger() : Integer;
begin
	Result := AsIntegerSetting.Value;
end;

function TSetting.AsIntegerSetting() : IIntegerSetting;
begin
	Result := TIntegerSetting(self);
end;

function TSetting.AsString() : string;
begin
	case SettingType of
		stString : begin
			Result := AsStringSetting.Value;
		end;
		stInteger : begin
			Result := IntToStr(AsIntegerSetting.Value);
		end;
		stBool : begin
			Result := BoolToStr(AsBoolSetting.Value, True);
		end;
		stStrArray : begin
			Result := '[' + string.Join(',', AsArraySetting.Value.Items) + ']';
		end;
		else
		raise ESettingsException.Create('Setting Type not supported.');
	end;

end;

function TSetting.AsStringSetting() : IStringSetting;
begin
	Result := TStringSetting(self);
end;

function TSetting.CompareTo(Value : ISetting) : Integer;
begin
	if SettingType = Value.SettingType then begin
		Result := Ord(State) - Ord(Value.State);
	end else begin
		Result := Ord(SettingType) - Ord(Value.SettingType);
	end;
end;

procedure TSetting.Copy(_other : ISetting);
begin
	FState := _other.State;
end;

class procedure TSetting.CopySettingValues(_from, _to : ISetting);
begin
	case _from.SettingType of
		stString :
		TStringSetting(_to).Copy(_from.AsStringSetting);
		stInteger :
		TIntegerSetting(_to).Copy(_from.AsIntegerSetting);
		stBool :
		TBoolSetting(_to).Copy(_from.AsBoolSetting);
		stStrArray :
		TArraySetting(_to).Copy(_from.AsArraySetting);
		else
		raise ESettingsException.Create('Can''t copy unknown setting type');
	end;
end;

function TSetting.Equals(_other : ISetting) : Boolean;
begin
	Result := (FState = _other.State);
end;

function TSetting.GetSaveBehaviour() : TSettingSaveBehaviour;
begin
	Result := FSaveBehaviour;
end;

function TSetting.GetState() : TSettingState;
begin
	Result := FState;
end;

procedure TSetting.SetSaveBehaviour(const Value : TSettingSaveBehaviour);
begin
	FSaveBehaviour := Value;
end;

procedure TSetting.SetState(const Value : TSettingState);
begin
	FState := Value;
end;

function TArraySetting.AddIfNotContains(const AItem : string) : Integer;
begin
	Result := -1;
	if not self.Value.Contains(AItem) then begin
		self.Value.Insert(0, AItem);
		Result := self.Value.Count;
	end;
end;

function TArraySetting.GetCount() : Integer;
begin
	Result := self.Value.Count;
end;

function TArraySetting.GetItem(Index : Integer) : string;
begin
	Result := self.Value[index];
end;

function TArraySetting.GetSafeItem(index : Integer) : string;
begin
	Result := self.SafeItem[index];
end;

function TArraySetting.GetType() : TSettingType;
begin
	Result := stStrArray;
end;

procedure TArraySetting.SetItem(Index : Integer; const Value : string);
begin
	self.Value[index] := Value;
end;

procedure TArraySetting.SetSafeItem(index : Integer; const Value : string);
begin
	self.Value.SafeItem[index] := Value;
end;

function TStringSetting.GetType() : TSettingType;
begin
	Result := stString;
end;

function TBoolSetting.GetType() : TSettingType;
begin
	Result := stBool;
end;

function TIntegerSetting.GetType() : TSettingType;
begin
	Result := stInteger;
end;

end.
