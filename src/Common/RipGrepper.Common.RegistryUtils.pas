unit RipGrepper.Common.RegistryUtils;

interface

uses
	System.Classes,
	System.Win.Registry,
	Winapi.Windows;

type
	IRegistryGuard = interface
		['{73A2F402-E0AA-4C70-A17B-65B75EABDE0A}']
		function IsValid : Boolean;
		function TryReadString(const _Item : string; out _Value : string) : Boolean;
		function ReadStringDef(const _Item : string; const _Default : string = '') : string;
		procedure WriteString(const _Item : string; const _Value : string);
		function TryReadStringList(const _SubPath : string; const _sl : TStrings; const _ClearFirst : Boolean = True;
			const _ItemName : string = 'Item') : Boolean;
		procedure WriteStringList(const _SubPath : string; const _sl : TStrings; const _ItemName : string = 'Item');
		function TryReadInteger(const _Item : string; out _Value : Integer) : Boolean;
		function ReadIntegerDef(const _Item : string; const _Default : Integer) : Integer;
		procedure WriteInteger(const _Item : string; _Value : Integer);
		function Reg : TRegistry;
	end;

type
	TRegistryGuard = class(TInterfacedObject, IRegistryGuard)
		private
			FReg : TRegistry;

		private // IRegistryGuard
			function IsValid : Boolean;
			function Reg : TRegistry;
			function TryReadString(const _Item : string; out _Value : string) : Boolean;
			function ReadStringDef(const _Item : string; const _Default : string) : string;
			procedure WriteString(const _Item : string; const _Value : string);
			function TryReadStringList(const _SubPath : string; const _sl : TStrings; const _ClearFirst : Boolean; const _ItemName : string)
				: Boolean;
			procedure WriteStringList(const _SubPath : string; const _sl : TStrings; const _ItemName : string = 'Item');
			function TryReadInteger(const _Item : string; out _Value : Integer) : Boolean;
			function ReadIntegerDef(const _Item : string; const _Default : Integer) : Integer;
			procedure WriteInteger(const _Item : string; _Value : Integer);

		public
			constructor Create(_Reg : TRegistry);
			destructor Destroy; override;
	end;

type
	TRegistryUtils = class(TObject)
		public
			/// <summary>
			/// Recursively deletes all subkeys in the current key of the reg parameter.
			/// @param reg is a TRegistry, opened with a key </summary>
			class procedure DeleteAllSubkeys(_Reg : TRegistry);
			/// <summary>
			/// Deletes all values in the current key of the reg parameter.
			/// @param reg is a TRegistry, opened with a key </summary>
			class procedure DeleteAllValues(_Reg : TRegistry);
			/// <summary>
			/// Deletes the value given by the Item from the given registry key.
			/// @param Key is the registry key (path) from which to delete
			/// @param Item is the name of the value to be deleted
			/// @param HKEY is the root key to use, defaults to HKEY_CURRENT_USER
			/// @raises any exception that TRegistry raises if something goes wrong writing the value,
			/// e.g. the cannot be opened or the value does not exist </summary>
			class procedure DeleteValue(const _Key : string; const _Item : string; _HKEY :
				HKEY = HKEY_CURRENT_USER);
			/// <summary>
			/// @param HKEY is the HKEY value to evaluate
			/// @param Short determines whether to return the short (HKCU) or the long name (HKEY_CURRENT_USER)
			/// there are only short values for HKCU, HKLM and HKCR.
			/// @returns the string representation of the known HKEY values
			/// @raises Exception if  HKEY is not one of the known values </summary>
			class function Hkey2String(_HKEY : HKEY; _Short : Boolean = False): string;
			/// <summary>
			/// Checks whether a key exists in the registry </summary>
			class function KeyExists(const _Key : string; _HKEY : HKEY =
				HKEY_CURRENT_USER): Boolean;
			/// <summary>
			/// Opens a registry key for writing
			/// @param Key is the registry key to open
			/// @param HKEY is the root key to use, defaults to HKEY_CURRENT_USER
			/// @returns a IRegistryGuard interface which closes and frees the TRegistry object automatically
			/// when it goes out of scope. </summary>
			class function OpenKey(const _Key : string; _HKEY : HKEY = HKEY_CURRENT_USER):
				IRegistryGuard;
			/// <summary>
			/// Opens a registry key for reading
			/// @param Key is the registry key to open
			/// @param HKEY is the root key to use, defaults to HKEY_CURRENT_USER
			/// @returns a IRegistryGuard interface which closes and frees the TRegistry object automatically
			/// when it goes out of scope. </summary>
			class function OpenKeyReadonly(const _Key : string; _HKEY : HKEY =
				HKEY_CURRENT_USER): IRegistryGuard;
			/// <summary>
			/// Reads a boolean value from the registry
			/// @param Key is the Key in which to read
			/// @param Entry is the name of the entry to read
			/// @param Default is the value to return if the value does not exist in the registry
			/// @param HKEY is the root key to use, defaults to HKEY_CURRENT_USER
			/// @returns the boolean value that was read, the default if the value did not exist
			/// @raises any exception that TRegistry raises if something goes wrong reading the value,
			/// e.g. the value exists, but is not a boolean value </summary>
			class function ReadBool(const _Key : string; const _Entry : string; _Default :
				Boolean = False; _HKEY : HKEY = HKEY_CURRENT_USER): Boolean;
			class function ReadInteger(const _Key, _Entry : string; _Default : Integer = 0;
				_HKEY : HKEY = HKEY_CURRENT_USER): Integer; overload;
			/// <summary>
			/// Reads an integer value from the given (open) TRegistry instance.
			/// @param Reg is aTRegistry instance where some registry key was opened
			/// @param Entry is the name of the entry to read
			/// @param Default is the value to return if the entry cannot be read
			/// @returns the value if it could be read or the given default if not
			class function ReadInteger(_Reg : TRegistry; const _Entry : string; _Default :
				Integer = 0): Integer; overload;
			/// <summary>
			/// Reads a string from the registry
			/// @param Path is the full path, including the value name of the string
			/// @param Default is the value to return if the value does not exist in the registry
			/// @param HKEY is the root key to use, defaults to HKEY_CURRENT_USER
			/// @returns the string that was read, the default if the value did not exist
			/// @raises any exception that TRegistry raises if something goes wrong reading the value,
			/// e.g. the value exists, but is not a string </summary>
			// function ReadString(const _Path: string; const _Default: string = '';
			// _HKEY: HKEY = HKEY_CURRENT_USER): string; overload; deprecated; // use the overloaded version

			/// <summary>
			/// Reads a string from the registry
			/// @param Key is the Key in which to read
			/// @param Entry is the name of the entry to read
			/// @param Default is the value to return if the value does not exist in the registry
			/// @param HKEY is the root key to use, defaults to HKEY_CURRENT_USER
			/// @returns the string that was read, the default if the value did not exist
			/// @raises any exception that TRegistry raises if something goes wrong reading the value,
			/// e.g. the value exists, but is not a string </summary>
			class function ReadString(const _Key : string; const _Entry : string; const
				_Default : string = ''; _HKEY : HKEY = HKEY_CURRENT_USER): string; overload;
			/// <summary>
			/// Reads a string value from the given (open) TRegistry instance.
			/// @param Reg is aTRegistry instance where some registry key was opened
			/// @param Entry is the name of the entry to read
			/// @param Default is the value to return if the entry cannot be read
			/// @returns the value if it could be read or the given default if not
			class function ReadString(_Reg : TRegistry; const _Entry : string; const
				_Default : string = ''): string; overload;
			class function TryOpenKeyReadonly(const _Key : string; _HKEY : HKEY =
				HKEY_CURRENT_USER; _ReadWow64 : Boolean = False): IRegistryGuard;
			/// <summary>
			/// Tries to read a boolean value from the registry
			/// @param Key is the Key in which to read
			/// @param Entry is the name of the entry to read
			/// @param Value contains the boolean value that was read, only valid if Result = true
			/// @param HKEY is the root key to use, defaults to HKEY_CURRENT_USER
			/// @returns true, if the value specified did exist, falso if it did not exist
			/// @raises any exception that TRegistry raises if something goes wrong reading the value,
			/// e.g. the value exists, but is not a boolean </summary>
			class function TryReadBool(const _Key : string; const _Entry : string; out
				_Value : Boolean; _HKEY : HKEY = HKEY_CURRENT_USER): Boolean;
			class function TryReadInteger(const _Key : string; const _Entry : string; out
				_Value : Integer; _HKEY : HKEY = HKEY_CURRENT_USER): Boolean; overload;
			/// <summary>
			/// Tries to read an integer value from the given (open) TRegistry instance.
			/// @param Reg is aTRegistry instance where some registry key was opened
			/// @param Entry is the name of the entry to read
			/// @param Value will return the value of the entry, if it could be read (only valid if Result = true)
			/// @returns True, if the entry exists, and is a string
			/// False otherwise.
			class function TryReadInteger(_Reg : TRegistry; const _Entry : string; out
				_Value : Integer): Boolean; overload;
			/// <summary>
			/// Tries to read a string from the registry
			/// @param Key is the Key in which to read
			/// @param Entry is the name of the entry to read
			/// @param Value contains the string that was read, only valid if Result = true
			/// @param HKEY is the root key to use, defaults to HKEY_CURRENT_USER
			/// @returns true, if the value specified did exist, falso if it did not exist
			/// @raises any exception that TRegistry raises if something goes wrong reading the value,
			/// e.g. the value exists, but is not a string </summary>
			class function TryReadString(const _Key : string; const _Entry : string; out
				_Value : string; _HKEY : HKEY = HKEY_CURRENT_USER; _ReadWow64 : Boolean =
				False): Boolean; overload;
			/// <summary>
			/// Tries to read a string from the registry
			/// @param Path is the full path, including the value name of the string
			/// @param Value contains the string that was read, only valid if Result = true
			/// @param HKEY is the root key to use, defaults to HKEY_CURRENT_USER
			/// @returns true, if the value specified did exist, falso if it did not exist
			/// @raises any exception that TRegistry raises if something goes wrong reading the value,
			/// e.g. the value exists, but is not a string </summary>
			class function TryReadString(const _Path : string; out _Value : string; _HKEY :
				HKEY = HKEY_CURRENT_USER): Boolean; overload; deprecated;
			/// <summary>
			/// Tries to read a string value from the given (open) TRegistry instance.
			/// @param Reg is aTRegistry instance where some registry key was opened
			/// @param Entry is the name of the entry to read
			/// @param Value will return the value of the entry, if it could be read (only valid if Result = true)
			/// @returns True, if the entry exists, and is a string
			/// False otherwise.
			class function TryReadString(_Reg : TRegistry; const _Entry : string; out
				_Value : string): Boolean; overload;
			class function TryReadStringValues(const _Path : string; _Entries :
				TStringList; _HKEY : HKEY = HKEY_CURRENT_USER): Boolean;
			/// <summary>
			/// Writes a boolean value to the registry
			/// @param Key is the Key in which to read
			/// @param Entry is the name of the entry to read
			/// @param Value is the boolean value to write
			/// @param HKEY is the root key to use, defaults to HKEY_CURRENT_USER
			/// @raises any exception that TRegistry raises if something goes wrong writing the value,
			/// e.g. the cannot be opened or the value exists, but is not a boolean value </summary>
			class procedure WriteBool(const _Key : string; const _Item : string; _Value :
				Boolean; _HKEY : HKEY = HKEY_CURRENT_USER);
			class procedure WriteInteger(const _Key : string; const _Item : string; _Value
				: Integer; _HKEY : HKEY = HKEY_CURRENT_USER);
			class procedure WriteString(const _Key : string; const _Item : string; const
				_Value : string; _HKEY : HKEY = HKEY_CURRENT_USER); overload;
			/// <summary>
			/// Writes a string to the registry
			/// @param Path is the full path, including the value name of the string
			/// @param Value contains the string to write
			/// @param HKEY is the root key to use, defaults to HKEY_CURRENT_USER
			/// @raises any exception that TRegistry raises if something goes wrong writing the value,
			/// e.g. the cannot be opened or the value exists, but is not a string </summary>
			class procedure WriteString(const _Path : string; const _Value : string; _HKEY
				: HKEY = HKEY_CURRENT_USER); overload;
	end;

implementation

uses
  System.SysUtils;

constructor TRegistryGuard.Create(_Reg : TRegistry);
begin
	inherited Create;
	FReg := _Reg;
end;

destructor TRegistryGuard.Destroy;
begin
	if Assigned(FReg) then
		FReg.CloseKey;
	FreeAndNil(FReg);

	inherited Destroy;
end;

function TRegistryGuard.IsValid : Boolean;
begin
	Result := Assigned(FReg);
end;

function TRegistryGuard.Reg : TRegistry;
begin
	Result := FReg;
end;

function TRegistryGuard.ReadIntegerDef(const _Item : string; const _Default : Integer) : Integer;
begin
	if not TryReadInteger(_Item, Result) then
		Result := _Default;
end;

function TRegistryGuard.ReadStringDef(const _Item, _Default : string) : string;
begin
	if not IsValid or not TryReadString(_Item, Result) then
		Result := _Default;
end;

function TRegistryGuard.TryReadStringList(const _SubPath : string; const _sl : TStrings; const _ClearFirst : Boolean;
	const _ItemName : string) : Boolean;
var
	SubReg : TRegistry;
	Path : string;
	cnt : Integer;
	i : Integer;
	ItemName : string;
begin
	_sl.BeginUpdate;
	try
		if _ClearFirst then
			_sl.Clear;

		Result := IsValid and Reg.KeyExists(_SubPath);
		if not Result then
			Exit; // -->

		Path := Reg.CurrentPath + '\' + _SubPath;
		SubReg := TRegistry.Create;
		try
			SubReg.RootKey := Reg.RootKey;
			Result := SubReg.OpenKeyReadOnly(Path);
			if not Result then
				Exit; // -->
			try
				Result := SubReg.ValueExists('Count');
				if not Result then
					Exit; // -->

				cnt := SubReg.ReadInteger('Count');
				if cnt = 0 then begin
					// The list exists but is empty, so we return true, but an empty/unchanged list
					Exit; // -->
				end;
				for i := 0 to cnt - 1 do begin
					ItemName := _ItemName + IntToStr(i);
					if SubReg.ValueExists(ItemName) then
						_sl.Add(SubReg.ReadString(ItemName))
					else
						_sl.Add('');
				end;
			finally
				SubReg.CloseKey;
			end;
		finally
			FreeAndNil(SubReg);
		end;
	finally
		_sl.EndUpdate;
	end;
end;

procedure TRegistryGuard.WriteStringList(const _SubPath : string; const _sl : TStrings; const _ItemName : string);
var
	SubReg : TRegistry;
	Path : string;
	i : Integer;
	ItemName : string;
begin
	if not IsValid then
		Exit; // -->

	Path := Reg.CurrentPath + '\' + _SubPath;
	SubReg := TRegistry.Create;
	try
		SubReg.RootKey := Reg.RootKey;
		if not SubReg.OpenKey(Path, True) then
			Exit; // -->
		try
			SubReg.WriteInteger('Count', _sl.Count);
			for i := 0 to _sl.Count - 1 do begin
				ItemName := _ItemName + IntToStr(i);
				SubReg.WriteString(ItemName, _sl[i]);
			end;
		finally
			SubReg.CloseKey;
		end;
	finally
		FreeAndNil(SubReg);
	end;
end;

function TRegistryGuard.TryReadInteger(const _Item : string; out _Value : Integer) : Boolean;
begin
	Result := FReg.ValueExists(_Item);
	if Result then
		_Value := FReg.ReadInteger(_Item)
end;

function TRegistryGuard.TryReadString(const _Item : string; out _Value : string) : Boolean;
begin
	Result := IsValid and FReg.ValueExists(_Item);
	if Result then
		_Value := FReg.ReadString(_Item)
end;

procedure TRegistryGuard.WriteInteger(const _Item : string; _Value : Integer);
begin
	FReg.WriteInteger(_Item, _Value);
end;

procedure TRegistryGuard.WriteString(const _Item, _Value : string);
begin
	if IsValid then
		FReg.WriteString(_Item, _Value);
end;

class procedure TRegistryUtils.DeleteAllSubkeys(_Reg : TRegistry);
var
	sl : TStringList;
	i : Integer;
begin
	sl := TStringList.Create;
	try
		_Reg.GetKeyNames(sl);
		for i := 0 to sl.Count - 1 do begin
			_Reg.DeleteKey(sl[i]);
		end;
	finally
		FreeAndNil(sl);
	end;
end;

class procedure TRegistryUtils.DeleteAllValues(_Reg : TRegistry);
var
	sl : TStringList;
	i : Integer;
begin
	sl := TStringList.Create;
	try
		_Reg.GetValueNames(sl);
		for i := 0 to sl.Count - 1 do begin
			_Reg.DeleteValue(sl[i]);
		end;
	finally
		FreeAndNil(sl);
	end;
end;

class procedure TRegistryUtils.DeleteValue(const _Key : string; const _Item :
	string; _HKEY : HKEY = HKEY_CURRENT_USER);
var
	Guard : IRegistryGuard;
begin
	Guard := OpenKey(_Key, _HKEY);
	Guard.Reg.DeleteValue(_Item);
end;

{ Xxxx functions }

class function TRegistryUtils.Hkey2String(_HKEY : HKEY; _Short : Boolean =
	False): string;
begin
	if _HKEY = HKEY_CLASSES_ROOT then begin
		Result := 'HKEY_CLASSES_ROOT';
	end else if _HKEY = HKEY_CURRENT_USER then begin
		if _Short then
			Result := 'HKCU'
		else
			Result := 'HKEY_CURRENT_USER';
	end else if _HKEY = HKEY_LOCAL_MACHINE then begin
		if _Short then
			Result := 'HKLM'
		else
			Result := 'HKEY_LOCAL_MACHINE';
	end else if _HKEY = HKEY_USERS then begin
		Result := 'HKEY_USERS'
	end else if _HKEY = HKEY_PERFORMANCE_DATA then begin
		Result := 'HKEY_PERFORMANCE_DATA';
	end else if _HKEY = HKEY_CURRENT_CONFIG then begin
		Result := 'HKEY_CURRENT_CONFIG';
	end else if _HKEY = HKEY_DYN_DATA then begin
		Result := 'HKEY_DYN_DATA';
	end
	else
		raise Exception.CreateFmt('Unknown HKEY value $%.8x', [_HKEY]);
end;

class function TRegistryUtils.KeyExists(const _Key : string; _HKEY : HKEY =
	HKEY_CURRENT_USER): Boolean;
var
	Reg : TRegistry;
begin
	Reg := TRegistry.Create;
	try
		Reg.RootKey := _HKEY;
		Result := Reg.KeyExists(_Key);
	finally
		FreeAndNil(Reg);
	end;
end;

class function TRegistryUtils.OpenKey(const _Key : string; _HKEY : HKEY =
	HKEY_CURRENT_USER): IRegistryGuard;
var
	Reg : TRegistry;
begin
	Reg := TRegistry.Create;
	Reg.RootKey := _HKEY;
	if not Reg.OpenKey(_Key, True) then begin
		FreeAndNil(Reg);
		raise Exception.CreateFmt('Could not open registry key "%s\%s"', [Hkey2String(_HKEY), _Key]);
	end;
	Result := TRegistryGuard.Create(Reg);
end;

class function TRegistryUtils.OpenKeyReadonly(const _Key : string; _HKEY : HKEY
	= HKEY_CURRENT_USER): IRegistryGuard;
begin
	Result := TryOpenKeyReadonly(_Key, _HKEY);

	if not Result.IsValid then
		raise Exception.CreateFmt('Could not open registry key "%s\%s"', [Hkey2String(_HKEY), _Key]);
end;

class function TRegistryUtils.ReadBool(const _Key : string; const _Entry :
	string; _Default : Boolean = False; _HKEY : HKEY = HKEY_CURRENT_USER):
	Boolean;
begin
	if not TryReadBool(_Key, _Entry, Result, _HKEY) then
		Result := _Default;
end;

class function TRegistryUtils.ReadInteger(const _Key, _Entry : string; _Default
	: Integer = 0; _HKEY : HKEY = HKEY_CURRENT_USER): Integer;
begin
	if not TryReadInteger(_Key, _Entry, Result, _HKEY) then
		Result := _Default;
end;

class function TRegistryUtils.ReadInteger(_Reg : TRegistry; const _Entry :
	string; _Default : Integer = 0): Integer;
begin
	if not TryReadInteger(_Reg, _Entry, Result) then
		Result := _Default;
end;

class function TRegistryUtils.ReadString(const _Key : string; const _Entry :
	string; const _Default : string = ''; _HKEY : HKEY = HKEY_CURRENT_USER):
	string;
begin
	if not TryReadString(_Key, _Entry, Result, _HKEY) then
		Result := _Default;
end;

class function TRegistryUtils.ReadString(_Reg : TRegistry; const _Entry :
	string; const _Default : string = ''): string;
begin
	if not TryReadString(_Reg, _Entry, Result) then
		Result := _Default;
end;

class function TRegistryUtils.TryOpenKeyReadonly(const _Key : string; _HKEY :
	HKEY = HKEY_CURRENT_USER; _ReadWow64 : Boolean = False): IRegistryGuard;
var
	Reg : TRegistry;
begin
	Reg := TRegistry.Create(key_Read or KEY_WOW64_64KEY);
	Reg.RootKey := _HKEY;
	if not Reg.OpenKeyReadOnly(_Key) then
		FreeAndNil(Reg);
	Result := TRegistryGuard.Create(Reg);
end;

class function TRegistryUtils.TryReadBool(const _Key : string; const _Entry :
	string; out _Value : Boolean; _HKEY : HKEY = HKEY_CURRENT_USER): Boolean;
var
	Guard : IRegistryGuard;
begin
	Guard := TryOpenKeyReadonly(_Key, _HKEY);
	Result := Guard.IsValid and Guard.Reg.ValueExists(_Entry);
	if not Result then
		Exit;
	_Value := Guard.Reg.ReadBool(_Entry);
end;

class function TRegistryUtils.TryReadInteger(const _Key : string; const _Entry
	: string; out _Value : Integer; _HKEY : HKEY = HKEY_CURRENT_USER): Boolean;
var
	Guard : IRegistryGuard;
begin
	Guard := TryOpenKeyReadonly(_Key, _HKEY);
	Result := Guard.IsValid and TryReadInteger(Guard.Reg, _Entry, _Value);
end;

class function TRegistryUtils.TryReadInteger(_Reg : TRegistry; const _Entry :
	string; out _Value : Integer): Boolean;
begin
	Result := _Reg.ValueExists(_Entry) and (_Reg.GetDataType(_Entry) = rdInteger);
	if Result then
		_Value := _Reg.ReadInteger(_Entry);
end;

class function TRegistryUtils.TryReadString(const _Key : string; const _Entry :
	string; out _Value : string; _HKEY : HKEY = HKEY_CURRENT_USER; _ReadWow64 :
	Boolean = False): Boolean;
var
	Guard : IRegistryGuard;
begin
	Guard := TryOpenKeyReadonly(_Key, _HKEY, _ReadWow64);
	Result := Guard.IsValid and TryReadString(Guard.Reg, _Entry, _Value);
end;

class function TRegistryUtils.TryReadString(const _Path : string; out _Value :
	string; _HKEY : HKEY = HKEY_CURRENT_USER): Boolean;
var
	Key : string;
	Item : string;
begin
	Key := ExtractFileDir(_Path);
	Item := ExtractFileName(_Path);
	Result := TryReadString(Key, Item, _Value, _HKEY);
end;

class function TRegistryUtils.TryReadString(_Reg : TRegistry; const _Entry :
	string; out _Value : string): Boolean;
begin
	Result := _Reg.ValueExists(_Entry) and (_Reg.GetDataType(_Entry) in [rdString, rdExpandString]);
	if Result then
		_Value := _Reg.ReadString(_Entry);
end;

class function TRegistryUtils.TryReadStringValues(const _Path : string;
	_Entries : TStringList; _HKEY : HKEY = HKEY_CURRENT_USER): Boolean;
var
	Item : string;
	Guard : IRegistryGuard;
	ValueNames : TStringList;
	i : Integer;
	Value : string;
begin
	Result := False;
	Guard := TryOpenKeyReadonly(_Path, _HKEY);
	if not Guard.IsValid then
		Exit; // -->
	ValueNames := TStringList.Create;
	try
		Guard.Reg.GetValueNames(ValueNames);
		for i := 0 to ValueNames.Count - 1 do begin
			Item := ValueNames[i];
			Value := Guard.Reg.ReadString(Item);
			_Entries.Values[Item] := Value;
		end;
		Result := True;
	finally
		FreeAndNil(ValueNames);
	end;
end;

class procedure TRegistryUtils.WriteBool(const _Key : string; const _Item :
	string; _Value : Boolean; _HKEY : HKEY = HKEY_CURRENT_USER);
var
	Guard : IRegistryGuard;
begin
	Guard := OpenKey(_Key, _HKEY);
	Guard.Reg.WriteBool(_Item, _Value);
end;

class procedure TRegistryUtils.WriteInteger(const _Key : string; const _Item :
	string; _Value : Integer; _HKEY : HKEY = HKEY_CURRENT_USER);
var
	Guard : IRegistryGuard;
begin
	Guard := OpenKey(_Key, _HKEY);
	Guard.Reg.WriteInteger(_Item, _Value);
end;

class procedure TRegistryUtils.WriteString(const _Key : string; const _Item :
	string; const _Value : string; _HKEY : HKEY = HKEY_CURRENT_USER);
var
	Guard : IRegistryGuard;
begin
	Guard := OpenKey(_Key, _HKEY);
	Guard.Reg.WriteString(_Item, _Value);
end;

class procedure TRegistryUtils.WriteString(const _Path : string; const _Value :
	string; _HKEY : HKEY = HKEY_CURRENT_USER);
var
	Key : string;
	Item : string;
begin
	Key := ExtractFileDir(_Path);
	Item := ExtractFileName(_Path);
	WriteString(Key, Item, _Value, _HKEY);
end;

end.
