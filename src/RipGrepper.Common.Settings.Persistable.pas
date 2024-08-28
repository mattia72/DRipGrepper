unit RipGrepper.Common.Settings.Persistable;

interface

uses
	System.Generics.Defaults,
	System.IniFiles,
	System.Generics.Collections,
	System.SysUtils,
	RipGrepper.Common.Settings.SettingVariant,
	RipGrepper.Common.Settings.SettingsDictionary;

type

	IIniPersistable = interface
		['{A841C46D-56AF-4391-AB88-4C9496589FF4}']
		function GetIniSectionName() : string;
		procedure Read;
		procedure Store;
	end;

	ESettingsException = class(Exception)

	end;

	TPersistableSettings = class(TSingletonImplementation, IIniPersistable)
		private
			FSettingsDict : TSettingsDictionary;
			FbDefaultLoaded : Boolean;
			FbOwnIniFile : Boolean;
			FIniSectionName : string;
			FIsAlreadyRead : Boolean;
			procedure AddOrSet(const _name : string; const _v : Variant); overload;
			procedure AddOrSet(const _name : string; const _sv : ISettingVariant); overload;
			procedure CreateIniFile;
			function GetIniFile : TMemIniFile;
			procedure ReadSettings;
			procedure SetIniSectionName(const Value : string);
			procedure SetIsModified(const Value : Boolean);
			procedure WriteSettings(_bDefaultOnly : Boolean = False);
			procedure WriteToIni(const _sIniSection, _sKey : string; const _setting : ISettingVariant);

		protected
			FIniFile : TMemIniFile;
			FIsModified : Boolean;

			procedure CreateSetting(const _sName : string; const _setting : ISettingVariant); overload;
			procedure CreateSetting(const _sName : string; const _type : TVarType; const _value : Variant;
				const _isDefRelevant : Boolean = False); overload;
			procedure CreateDefaultSetting(const _sName : string; const _type : TVarType; const _value : Variant); overload;
			function GetDictKeyName(const _key : string; const _bForDefault : Boolean = False) : string;
			function GetDefaultDictKeyName(const _key : string) : string;
			function GetIsAlreadyRead: Boolean; virtual;
			function GetIsModified : Boolean; virtual;
			procedure Init; virtual;
			procedure Read; virtual;
			procedure StoreSetting(const _name : string; const _v : Variant; const _bAlsoDefault : Boolean = False);
			function LoadSettingDefaultValue(const _name : string) : Variant;
			procedure Store; virtual;
			function GetIniSectionName : string;
			procedure LoadDefault; virtual;
			function LoadDefaultSetting(const _name : string) : Variant;
			procedure StoreAsDefault; virtual;
			procedure StoreDefaultSetting(const _name : string; const _v : Variant);

		public
			constructor Create(const _ini : TMemIniFile); overload;
			constructor Create; overload;
			procedure Copy(const _other : TPersistableSettings); virtual;
			procedure ReLoad;
			procedure UpdateIniFile;
			property IniFile : TMemIniFile read GetIniFile;
			property IniSectionName : string read GetIniSectionName write SetIniSectionName;
			property IsAlreadyRead : Boolean read GetIsAlreadyRead;
			property IsModified : Boolean read GetIsModified write SetIsModified;
			destructor Destroy; override;
			procedure CopyDefaults; virtual;
			function GetSetting(const _name : string) : Variant; overload;
			procedure SetSettingValue(_sKey : string; const _v : Variant);
			// class property SettingsDict: TSettingsDictionary read FSettingsDict;
	end;

implementation

uses
	System.Classes,
	RipGrepper.Tools.DebugUtils,
	System.Variants,
	RipGrepper.Common.Constants,
	Vcl.Forms,
	RipGrepper.Common.IOTAUtils,
	System.IOUtils,
	System.StrUtils;

constructor TPersistableSettings.Create(const _ini : TMemIniFile);
begin
	inherited Create();
	FIniFile := _ini;
	Create();
	FbOwnIniFile := False;
end;

constructor TPersistableSettings.Create;
begin
	inherited;
	FIsModified := False;
	FIsAlreadyRead := False;
	FSettingsDict := TSettingsDictionary.Create();
	FbDefaultLoaded := False;
	if not Assigned(FIniFile) then begin
		CreateIniFile;
		FbOwnIniFile := True;
	end;
end;

destructor TPersistableSettings.Destroy;
begin
	if FbOwnIniFile then begin
		FIniFile.Free;
	end;
	FSettingsDict.Free;
	inherited;
end;

procedure TPersistableSettings.AddOrSet(const _name : string; const _v : Variant);
var
	bExists : Boolean;
	setting : ISettingVariant;
	sKey : string;
begin
	sKey := GetDictKeyName(_name);
	bExists := FSettingsDict.TryGetValue(sKey, setting);
	if bExists and (setting.IsEmpty or (setting.ValueType <> VarType(_v)) or
		{ } (setting.Value <> _v)) then begin
		setting.Value := _v;
		setting.IsModified := bExists;
		setting.ValueType := VarType(_v);
		if not bExists then begin
			setting.IsDefaultRelevant := False;
		end;
		FSettingsDict.AddOrChange(sKey, setting);
	end else begin
		if not bExists then begin
			setting := TSettingVariant.Create(_v); // ISettingVariant
			FSettingsDict.Add(sKey, setting);
		end else begin
			FSettingsDict.AddOrChange(sKey, setting);
		end;
	end;
end;

procedure TPersistableSettings.AddOrSet(const _name : string; const _sv : ISettingVariant);
var
	setting : ISettingVariant;
	sKey : string;
begin
	sKey := GetDictKeyName(_name);

	FSettingsDict.TryGetValue(sKey, setting);
	if Assigned(setting) then begin
		if not setting.Equals(_sv) then begin
			setting := _sv;
			setting.IsModified := True;
			FSettingsDict.AddOrChange(sKey, setting);
		end;
	end;
end;

procedure TPersistableSettings.Copy(const _other : TPersistableSettings);
begin
	FIsModified := _other.IsModified;
	FIsAlreadyRead := _other.IsAlreadyRead;
	for var key in _other.FSettingsDict.Keys do begin
		if key.StartsWith(IniSectionName) then begin
			FSettingsDict.AddOrChange(key, _other.FSettingsDict[key]);
		end;
	end;
end;

procedure TPersistableSettings.CopyDefaults;
var
	setting : ISettingVariant;
begin
	for var key in FSettingsDict.Keys do begin
		if key.StartsWith(IniSectionName) then begin
			setting := FSettingsDict[key];
			if setting.IsDefaultRelevant then begin
				setting.Value := setting.DefaultValue;
				FSettingsDict.AddOrChange(key, setting);
			end;
		end;
	end;
end;

procedure TPersistableSettings.CreateSetting(const _sName : string; const _setting : ISettingVariant);
begin
	if _setting.IsDefaultRelevant then begin
		FSettingsDict.AddOrChange(GetDefaultDictKeyName(_sName), _setting);
	end;
	FSettingsDict.AddOrChange(GetDictKeyName(_sName), _setting);
end;

procedure TPersistableSettings.CreateIniFile;
begin
	if IOTAUTils.IsStandAlone then begin
		FIniFile := TMemIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'), TEncoding.UTF8);
	end else begin
		FIniFile := TMemIniFile.Create(TPath.Combine(IOTAUTils.GetSettingFilePath, EXTENSION_NAME + '.ini'), TEncoding.UTF8);
	end;
end;

procedure TPersistableSettings.CreateSetting(const _sName : string; const _type : TVarType; const _value : Variant;
	const _isDefRelevant : Boolean = False);
var
	setting : ISettingVariant;
begin
	setting := TSettingVariant.Create(_type, _value, _isDefRelevant); // ISettingVariant
	CreateSetting(_sName, setting);
end;

procedure TPersistableSettings.CreateDefaultSetting(const _sName : string; const _type : TVarType; const _value : Variant);
var
	setting : ISettingVariant;
begin
	setting := TSettingVariant.Create(_type, _value, True); // ISettingVariant
	CreateSetting(_sName, setting);
end;

function TPersistableSettings.GetDictKeyName(const _key : string; const _bForDefault : Boolean = False) : string;
begin
	if _bForDefault then begin
		Result := GetDefaultDictKeyName(_key);
	end else begin
		if _key.Contains(ARRAY_SEPARATOR) then begin
			Result := _key;
		end else begin
			Result := IniSectionName + ARRAY_SEPARATOR + _key;
		end;
	end;
end;

function TPersistableSettings.GetDefaultDictKeyName(const _key : string) : string;
begin
	Result := GetDictKeyName(_key) + DEFAULT_KEY;
end;

function TPersistableSettings.GetIniFile : TMemIniFile;
begin
	Result := FIniFile;
end;

function TPersistableSettings.GetIniSectionName : string;
begin
	Result := FIniSectionName;
end;

function TPersistableSettings.GetIsAlreadyRead: Boolean;
begin
	Result := IsAlreadyRead;
end;

function TPersistableSettings.GetIsModified : Boolean;
begin
	Result := FIsModified;
end;

function TPersistableSettings.GetSetting(const _name : string) : Variant;
var
	setting : ISettingVariant;
begin
	if FSettingsDict.TryGetValue(GetDictKeyName(_name), setting) then begin
		if (not setting.IsEmpty) then begin
			Result := setting.Value;
		end;
		TDebugUtils.DebugMessage('TPersistableSettings.GetSetting: ' + _name + ' ' + VarToStr(Result));
	end else begin
		TDebugUtils.DebugMessage('TPersistableSettings.GetSetting: ' + _name + ' not found!');
	end;
end;

procedure TPersistableSettings.Init;
begin
	//
end;

procedure TPersistableSettings.Read;
begin
	Init();
	ReadSettings();
	FIsAlreadyRead := True;
end;

procedure TPersistableSettings.LoadDefault;
var
	val : ISettingVariant;
begin
	for var key in FSettingsDict.Keys do begin
		if not key.StartsWith(IniSectionName) then
			continue;

		val := FSettingsDict[key];
		if val.IsDefaultRelevant then begin
			val.Value := val.DefaultValue;
		end else begin
			continue;
		end;
	end;
end;

procedure TPersistableSettings.SetIsModified(const Value : Boolean);
begin
	FIsModified := Value;
end;

procedure TPersistableSettings.SetSettingValue(_sKey : string; const _v : Variant);
begin
	AddOrSet(_sKey, _v);
end;

procedure TPersistableSettings.Store;
begin
	WriteSettings();
end;

procedure TPersistableSettings.StoreSetting(const _name : string; const _v : Variant; const _bAlsoDefault : Boolean = False);
begin
	TDebugUtils.DebugMessage('TPersistableSettings.StoreSetting: ' + _name + '=' + VarToStr(_v) + ' store in memory...');
	AddOrSet(_name, _v);
	if _bAlsoDefault then begin
		StoreDefaultSetting(_name, _v);
	end;
end;

function TPersistableSettings.LoadDefaultSetting(const _name : string) : Variant;
begin
	Result := GetSetting(GetDefaultDictKeyName(_name));
end;

function TPersistableSettings.LoadSettingDefaultValue(const _name : string) : Variant;
var
	setting : ISettingVariant;
begin
	setting := FSettingsDict[_name];
	Result := setting.DefaultValue;
	TDebugUtils.DebugMessage('TPersistableSettings.LoadSettingDefaultValue: ' + _name + ' ' + Result);
end;

procedure TPersistableSettings.ReadSettings;
var
	baseName : string;
	strs : TStringList;
	name : string;
	setting : ISettingVariant;
	value : string;
begin
	strs := TStringList.Create();
	try
		if IniSectionName = ROOT_DUMMY_INI_SECTION then
			Exit;
		try
			FIniFile.ReadSectionValues(IniSectionName, strs);

			// create base settings
			for var i : integer := 0 to strs.Count - 1 do begin
				name := strs.Names[i];
				if name.EndsWith(DEFAULT_KEY) then
					continue;
				value := strs.Values[name];
				setting := TSettingVariant.Create(value); // ISettingVariant
				TDebugUtils.DebugMessage(Format('TPersistableSettings.ReadSettings: [%s] %s = %s', [IniSectionName, name, value]));
				AddOrSet(GetDictKeyName(name), setting);
			end;

			// set default settings
			for var i : integer := 0 to strs.Count - 1 do begin
				name := strs.Names[i];
				if not name.EndsWith(DEFAULT_KEY) then
					continue;

				baseName := name.Replace(DEFAULT_KEY, '');
				if FSettingsDict.TryGetValue(baseName, setting) then begin
					value := strs.Values[name];
					setting.IsDefaultRelevant := True;
					setting.DefaultValue := value;
					TDebugUtils.DebugMessage(Format('TPersistableSettings.ReadSettings: [%s] %s = %s', [IniSectionName, name, value]));
					// AddOrSet(GetDictKeyName(baseName), setting);
				end;

			end;
		except
			on E : Exception do
				raise;
		end;
	finally
		strs.Free;
	end;
end;

procedure TPersistableSettings.ReLoad;
begin
	FIsAlreadyRead := False;
end;

procedure TPersistableSettings.SetIniSectionName(const Value : string);
begin
	FIniSectionName := Value;
end;

procedure TPersistableSettings.StoreAsDefault;
begin
	WriteSettings(True);
end;

procedure TPersistableSettings.StoreDefaultSetting(const _name : string; const _v : Variant);
begin
	TDebugUtils.DebugMessage('TPersistableSettings.StoreDefaultSetting: ' + _name + '=' + VarToStr(_v) + ' store in memory...');
	AddOrSet(GetDefaultDictKeyName(_name), _v);
end;

procedure TPersistableSettings.UpdateIniFile;
begin
	FIniFile.UpdateFile;
end;

procedure TPersistableSettings.WriteSettings(_bDefaultOnly : Boolean = False);
var
	setting : ISettingVariant;
begin
	for var key in FSettingsDict.Keys do begin
		if ((not IniSectionName.IsEmpty) and (not key.StartsWith(IniSectionName))) then
			continue;
		if _bDefaultOnly and (not key.EndsWith(DEFAULT_KEY)) then
			continue;
		setting := FSettingsDict[key];
		var
		arr := key.Split([ARRAY_SEPARATOR]);

		WriteToIni(arr[0], arr[1], setting);
		setting.IsModified := False;

		FSettingsDict[key] := setting;
	end;
end;

procedure TPersistableSettings.WriteToIni(const _sIniSection, _sKey : string; const _setting : ISettingVariant);
var
	v : Variant;
begin
	if _setting.IsModified or (_sKey.EndsWith(DEFAULT_KEY)) then begin
		case _setting.ValueType of
			varString, varUString :
			FIniFile.WriteString(_sIniSection, _sKey, _setting.Value);
			varBoolean :
			FIniFile.WriteBool(_sIniSection, _sKey, _setting.Value);
			varInteger :
			FIniFile.WriteInteger(_sIniSection, _sKey, _setting.Value);
			varArray : begin
				var
				i := VarArrayLowBound(_setting.Value, 1);
				var
				len := VarArrayHighBound(_setting.Value, 1);

				while i <= len do begin
					v := _setting.Value[i]; // v should be string
					FIniFile.WriteString(_sIniSection, Format('%s_Item%d', [_sKey, i]), v);
					Inc(i);
				end;
			end
			else
			raise ESettingsException.Create('Settings Type not supported:' + VarTypeAsText(_setting.ValueType));
		end;
		TDebugUtils.DebugMessage('TPersistableSettings.Store: ' + FIniFile.FileName + '[' + _sIniSection + '] ' + _sKey + '=' +
			VarToStr(_setting.Value) + ' stored');
	end;
end;

end.
