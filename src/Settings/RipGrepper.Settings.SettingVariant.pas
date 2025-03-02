unit RipGrepper.Settings.SettingVariant;

interface

uses
	System.Variants,
	System.Generics.Defaults,
	System.IniFiles,
	System.SysUtils,
	RipGrepper.Settings.FilePersister,
	ArrayEx;

type

	ESettingsException = class(Exception);

	TSettingState = (ssNotSet, ssModified);

	ISetting = interface
		['{289A58E3-A490-4015-9AFE-52EB303B9B89}']
		function GetState() : TSettingState;
		procedure LoadFromFile();
		procedure SaveToFile();
		procedure SetState(const Value : TSettingState);
		property State : TSettingState read GetState write SetState;
	end;

	ISettingVariant<T> = interface(ISetting)
		['{D4A1E2B3-5F6C-4A7D-8B9E-1C2D3E4F5A6B}']
		function Equals(_other : ISettingVariant<T>) : Boolean;
		function IsEmpty : Boolean;

		function GetValue : T;
		procedure SetValue(const Value : T);
		function GetPersister() : IFilePersister<T>;
		procedure SetPersister(const Value : IFilePersister<T>);

		property Persister : IFilePersister<T> read GetPersister write SetPersister;
		property Value : T read GetValue write SetValue;
	end;

	TSetting = class(TInterfacedObject, ISetting)

		private
			FState : TSettingState;
			function GetState() : TSettingState;
			procedure SetState(const Value : TSettingState);

		public
			procedure LoadFromFile(); virtual; abstract;
			procedure SaveToFile(); virtual; abstract;
			property State : TSettingState read GetState write SetState;
	end;

	TSettingVariant<T> = class(TSetting, ISettingVariant<T>)
		private
			FPersister : IFilePersister<T>;
			FValue : T;

			function GetPersister() : IFilePersister<T>;
			function GetValue() : T;
			procedure SetPersister(const Value : IFilePersister<T>);
			procedure SetValue(const Value : T);

		public
			constructor Create(const _value : T); overload;
			function CompareTo(Value : ISettingVariant<T>) : Integer;
			function Equals(_other : ISettingVariant<T>) : Boolean; reintroduce;
			function IsEmpty : Boolean;
			procedure LoadFromFile(); override;
			procedure SaveToFile(); override;
			property Persister : IFilePersister<T> read GetPersister write SetPersister;
			property Value : T read GetValue write SetValue;
	end;

	TStringSetting = TSettingVariant<string>;
	TBoolSetting = TSettingVariant<Boolean>;
	TIntegerSetting = TSettingVariant<integer>;

	TArraySetting = class(TSettingVariant < TArrayEx < string >> )

		private
			function GetCount() : Integer;
			function GetItem(Index : Integer) : string;
			procedure SetItem(Index : Integer; const Value : string);

		public
			function AddIfNotContains(const AItem : string): Integer;
			property Count : Integer read GetCount;
			property Item[index : Integer] : string read GetItem write SetItem; default;
	end;

implementation

uses
	RipGrepper.Tools.DebugUtils;

constructor TSettingVariant<T>.Create(const _value : T);
begin
	FValue := _value;
	FState := ssNotSet;
end;

function TSettingVariant<T>.CompareTo(Value : ISettingVariant<T>) : Integer;
var
	res : integer;
begin
	res := TComparer<T>.Default.Compare(self.FValue, Value.Value);
	if res = 0 then begin
		if FState <> Value.State then
			Result := Ord(FState) - Ord(Value.State)
		else
			Result := 0;
		Exit;
	end;

	Result := Integer(res);
end;

function TSettingVariant<T>.Equals(_other : ISettingVariant<T>) : Boolean;
begin
	Result := (TComparer<T>.Default.Compare(FValue, _other.Value) = 0) and
	{ } (FState = _other.State);
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
		FState := ssModified
	end;
end;

function TSettingVariant<T>.GetValue() : T;
begin
	Result := FValue;
end;

procedure TSettingVariant<T>.LoadFromFile();
begin
	Value := Persister.LoadFromFile();
end;

procedure TSettingVariant<T>.SaveToFile();
begin
	Persister.SaveToFile(Value);
end;

procedure TSettingVariant<T>.SetPersister(const Value : IFilePersister<T>);
begin
	FPersister := Value;
end;

function TSetting.GetState() : TSettingState;
begin
	Result := FState;
end;

procedure TSetting.SetState(const Value : TSettingState);
begin
	FState := Value;
end;

function TArraySetting.AddIfNotContains(const AItem : string): Integer;
begin
	Result := -1;
	if not self.Value.Contains(AItem) then
		Result := self.Value.Add(AItem);
end;

function TArraySetting.GetCount() : Integer;
begin
	Result := self.Value.Count;
end;

function TArraySetting.GetItem(Index : Integer) : string;
begin
	Result := self.Value[index];
end;

procedure TArraySetting.SetItem(Index : Integer; const Value : string);
begin
	self.Value[index] := Value;
end;

end.
