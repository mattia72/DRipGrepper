unit RipGrepper.Settings.SettingVariant;

interface

uses
	System.Variants,
	System.Generics.Defaults,
	System.IniFiles,
	RipGrepper.Settings.Persister.Interfaces,
	RipGrepper.Settings.FilePersister,
	ArrayEx,
	System.SysUtils,
	RipGrepper.Common.Interfaces.StreamPersistable,
	RipGrepper.Helper.SettingStoreBehaviours,
	System.Classes;

type

	ESettingsException = class(Exception);

	TSettingState = (ssNotSet, ssInitialized, ssModified, ssStored, ssSaved);

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
	TSettingChangedEvent = procedure(Sender : TObject) of object;

	ISetting = interface
		['{289A58E3-A490-4015-9AFE-52EB303B9B89}']
		function Equals(_other : ISetting) : Boolean;

		procedure Copy(_other : ISetting);
		procedure Clear();

		function CompareTo(Value : ISetting) : Integer;

		function GetState() : TSettingState;
		procedure SetState(const Value : TSettingState);

		function GetSettingType() : TSettingType;

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
		function GetName() : string;
		function GetSaveBehaviour() : TSettingStoreBehaviours;
		procedure SetSaveBehaviour(const Value : TSettingStoreBehaviours);

		property Name : string read GetName;
		property State : TSettingState read GetState write SetState;
		property SettingType : TSettingType read GetSettingType;
		property SaveBehaviour : TSettingStoreBehaviours read GetSaveBehaviour write SetSaveBehaviour;

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
			FName : string;
			FSettingType : TSettingType;
			FSaveBehaviour : TSettingStoreBehaviours;
			FState : TSettingState;

			function GetName() : string;
			function GetState() : TSettingState;
			procedure SetSettingType(const Value : TSettingType);
			procedure SetState(const Value : TSettingState);

		protected
			function GetSettingType() : TSettingType; virtual;

		public
			constructor Create(const _name : string; { } _state : TSettingState = ssInitialized;
				{ } _saveBehaviour : TSettingStoreBehaviours = [ssbStoreIfModified]); overload;
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
			function AsReversedArray() : TArrayEx<string>;
			procedure Clear(); virtual;
			function CompareTo(Value : ISetting) : Integer;
			procedure Copy(_other : ISetting);
			class procedure CopySettingFields(_from, _to : ISetting);
			class procedure CopySettingValue(_from, _to : ISetting);
			function Equals(_other : ISetting) : Boolean; reintroduce;
			function GetSaveBehaviour() : TSettingStoreBehaviours;
			procedure SetSaveBehaviour(const Value : TSettingStoreBehaviours);

			property Name: string read GetName;
			property State : TSettingState read GetState write SetState;
			property SaveBehaviour : TSettingStoreBehaviours read GetSaveBehaviour write SetSaveBehaviour;

			property SettingType : TSettingType read GetSettingType write SetSettingType;
	end;

	TSettingVariant<T> = class(TSetting, ISettingVariant<T>, ISetting, IStreamReaderWriterPersistable)
		private
			FValue : T;
			FPersister : IFilePersister<T>;

			function GetValue() : T;
			procedure SetValue(const Value : T);

		protected
			function GetPersister() : IFilePersister<T>;
			procedure SetPersister(const Value : IFilePersister<T>);

		public
			constructor Create(const _name : string; const _value : T; { } _state : TSettingState = ssInitialized;
				{ } _saveBehaviour : TSettingStoreBehaviours = [ssbStoreIfModified]); overload;
			procedure Clear(); override;
			function CompareTo(Value : ISettingVariant<T>) : Integer;
			procedure Copy(_other : ISettingVariant<T>); reintroduce;
			function Equals(_other : ISettingVariant<T>) : Boolean; reintroduce;
			function GetValueFromString(const _strValue : string) : T; virtual; abstract;
			function IsEmpty : Boolean;
			procedure LoadFromPersister(); override;
			procedure LoadFromStreamReader(_sr : TStreamReader);
			procedure SaveToStreamWriter(_sw : TStreamWriter);
			procedure StoreToPersister(const _section : string = ''); override;
			property Persister : IFilePersister<T> read GetPersister write SetPersister;
			property Value : T read GetValue write SetValue;
	end;

	TStringSetting = class(TSettingVariant<string>)
		public
			function GetSettingType() : TSettingType; override;
			function GetValueFromString(const _strValue : string) : string; override;
	end;

	TBoolSetting = class(TSettingVariant<Boolean>)
		public
			function GetSettingType() : TSettingType; override;
			function GetValueFromString(const _strValue : string) : Boolean; override;
	end;

	TIntegerSetting = class(TSettingVariant<integer>)
		public
			function GetSettingType() : TSettingType; override;
			function GetValueFromString(const _strValue : string) : integer; override;
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
			function GetValueFromString(const _strValue : string) : TArrayEx<string>; override;

			function GetSettingType() : TSettingType; override;
			property Count : Integer read GetCount;
			property Item[index : Integer] : string read GetItem write SetItem; default;
			property SafeItem[index : Integer] : string read GetSafeItem write SetSafeItem;
	end;

	TSettingFactory = class(TObject)
		public
			class function CreateSetting(const settingType : TSettingType; const _name : string) : ISetting;
	end;

implementation

uses
	RipGrepper.Tools.DebugUtils,
	RipGrepper.Common.Constants,
	System.StrUtils,
	RipGrepper.Helper.StreamReaderWriter;

constructor TSettingVariant<T>.Create(const _name : string; const _value : T;
	{ } _state : TSettingState = ssInitialized;
	{ } _saveBehaviour : TSettingStoreBehaviours = [ssbStoreIfModified]);
begin
	inherited Create(_name, _state, _saveBehaviour);
	FValue := _value;
end;

procedure TSettingVariant<T>.Clear();
begin
	inherited;
	FValue := default (T);
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

function TSettingVariant<T>.IsEmpty : Boolean;
begin
	Result := (FState = ssNotSet);
end;

procedure TSettingVariant<T>.SetValue(const Value : T);
begin
	if FValue <> Value then begin
		FValue := Value;
		FState := ssModified;
		if ssbStoreAfterChangeImmediately in FSaveBehaviour then begin
			FPersister.StoreValue(FValue);
		end;

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

procedure TSettingVariant<T>.LoadFromStreamReader(_sr : TStreamReader);
begin
	// FSettingType := TSettingType(_sr.ReadLine().ToInteger);
	FSaveBehaviour := TSettingStoreBehavioursHelper.FromString(_sr.ReadLineAsString(false, 'SaveBehaviour'));
	FState := TSettingState(_sr.ReadLineAsInteger(Name));
	Value := GetValueFromString(_sr.ReadLineAsString(true, Name)); // Values can be empty
end;

procedure TSettingVariant<T>.SaveToStreamWriter(_sw : TStreamWriter);
begin
	// _sw.WriteLine(Integer(SettingType).ToString);
	_sw.WriteLineAsString(TSettingStoreBehavioursHelper.ToString(SaveBehaviour), false, Name + '.SettingVariant.SaveBehaviour');
	_sw.WriteLineAsInteger(Integer(State), Name + '.SettingVariant.State');
	_sw.WriteLineAsString(AsString, True, Name);
end;

procedure TSettingVariant<T>.StoreToPersister(const _section : string = '');
begin
	if not Assigned(Persister) then begin
		raise ESettingsException.Create('Persister is not assigned.');
	end;

	if (State = ssModified)
	{ } or (ssbStoreOnceEvenIfNotModified in SaveBehaviour) then begin
		Persister.StoreValue(Value);
		State := ssStored;
		Exclude(FSaveBehaviour, ssbStoreOnceEvenIfNotModified);
	end;
end;

procedure TSettingVariant<T>.SetPersister(const Value : IFilePersister<T>);
begin
	FPersister := Value;
end;

constructor TSetting.Create(const _name : string;
	{ } _state : TSettingState = ssInitialized;
	{ } _saveBehaviour : TSettingStoreBehaviours = [ssbStoreIfModified]);
begin
	inherited Create();
	FName := _name;
	FState := _state;
	FSaveBehaviour := _saveBehaviour;
	FSettingType := stNotSet;
end;

function TSetting.AsArray() : TArrayEx<string>;
begin
	Result := AsArraySetting.Value;
end;

function TSetting.AsReversedArray() : TArrayEx<string>;
begin
	Result := AsArraySetting.Value.GetReversedRange();
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

procedure TSetting.Clear();
begin
	FName := '';
	FState := ssInitialized;
	FSaveBehaviour := [ssbStoreIfModified];
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
	FName := _other.Name;
	FState := _other.State;
end;

class procedure TSetting.CopySettingFields(_from, _to : ISetting);
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

class procedure TSetting.CopySettingValue(_from, _to : ISetting);
begin
	case _from.SettingType of
		stString :
		TStringSetting(_to).Value := (_from.AsStringSetting).Value;
		stInteger :
		TIntegerSetting(_to).Value := (_from.AsIntegerSetting).Value;
		stBool :
		TBoolSetting(_to).Value := (_from.AsBoolSetting).Value;
		stStrArray :
		TArraySetting(_to).Value := (_from.AsArraySetting).Value;
		else
		raise ESettingsException.Create('Can''t copy unknown setting type');
	end;
end;

function TSetting.Equals(_other : ISetting) : Boolean;
begin
	Result := (FState = _other.State);
end;

function TSetting.GetName() : string;
begin
	Result := FName;
end;

function TSetting.GetSaveBehaviour() : TSettingStoreBehaviours;
begin
	Result := FSaveBehaviour;
end;

function TSetting.GetState() : TSettingState;
begin
	Result := FState;
end;

function TSetting.GetSettingType() : TSettingType;
begin
	Result := FSettingType;
end;

procedure TSetting.SetSaveBehaviour(const Value : TSettingStoreBehaviours);
begin
	FSaveBehaviour := Value;
end;

procedure TSetting.SetSettingType(const Value : TSettingType);
begin
	if Value <> stNotSet then begin
		FSettingType := Value;
	end;
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

function TArraySetting.GetSettingType() : TSettingType;
begin
	Result := stStrArray;
end;

function TArraySetting.GetValueFromString(const _strValue : string) : TArrayEx<string>;
begin
	Result := _strValue.Split([ARRAY_SEPARATOR]);
end;

procedure TArraySetting.SetItem(Index : Integer; const Value : string);
begin
	self.Value[index] := Value;
end;

procedure TArraySetting.SetSafeItem(index : Integer; const Value : string);
begin
	self.Value.SafeItem[index] := Value;
end;

function TStringSetting.GetSettingType() : TSettingType;
begin
	Result := stString;
end;

function TStringSetting.GetValueFromString(const _strValue : string) : string;
begin
	Result := _strValue;
end;

function TBoolSetting.GetSettingType() : TSettingType;
begin
	Result := stBool;
end;

function TBoolSetting.GetValueFromString(const _strValue : string) : Boolean;
begin
	Result := StrToBool(_strValue);
end;

function TIntegerSetting.GetSettingType() : TSettingType;
begin
	Result := stInteger;
end;

function TIntegerSetting.GetValueFromString(const _strValue : string) : integer;
begin
	Result := _strValue.ToInteger;
end;

class function TSettingFactory.CreateSetting(const settingType : TSettingType; const _name : string) : ISetting;
begin
	case settingType of
		stNotSet :
		raise ESettingsException.Create('Setting Type not set');
		stString :
		Result := TStringSetting.Create(_name, '');
		stInteger :
		Result := TIntegerSetting.Create(_name);
		stBool :
		Result := TBoolSetting.Create(_name);
		stStrArray :
		Result := TArraySetting.Create(_name);
	end;
end;

end.
