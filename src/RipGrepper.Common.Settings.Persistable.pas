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
		procedure Init;
		procedure ReadIni;
		procedure RefreshMembers(const _bWithDefault : Boolean);
		procedure Store;
		procedure StoreAsDefault;
	end;

	ESettingsException = class(Exception)

	end;

	TPersistableSettings = class(TSingletonImplementation, IIniPersistable)
		private
			FbDefaultLoaded : Boolean;
			FbOwnIniFile : Boolean;
			FIniSectionName : string;
			FIsAlreadyRead : Boolean;
			procedure AddOrSet(const _name : string; const _v : Variant); overload;
			procedure AddOrSet(const _name : string; const _sv : ISettingVariant); overload;
			procedure AddOrSetDefaultValue(const _key : string; const _value : Variant; const _bSaveToIni : Boolean);
			procedure CreateIniFile;
			function GetDefaultSetting(const _name : string) : Variant;
			function GetIniFile : TMemIniFile;
			function InnerGetSetting(const _name : string; const _bDefaultValue : Boolean) : Variant; overload;
			procedure ReadSettings;
			procedure SetIniSectionName(const Value : string);
			procedure WriteSettings(_bDefault : Boolean = False);
			procedure WriteToIni(const _sIniSection, _sKey : string; const _setting : ISettingVariant);

		protected
			FIniFile : TMemIniFile;
			FSettingsDict : TSettingsDictionary;

			FIsModified : Boolean;

			procedure CreateSetting(const _sName : string; const _setting : ISettingVariant); overload;
			procedure CreateSetting(const _sName : string; const _type : TVarType; const _value : Variant;
				const _isDefRelevant : Boolean = False); overload;
			procedure CreateDefaultRelevantSetting(const _sName : string; const _type : TVarType; const _value : Variant); overload;
			function GetDictKeyName(const _key : string; const _bForDefault : Boolean = False) : string;
			function GetDefaultDictKeyName(const _key : string) : string;
			function GetIsAlreadyRead : Boolean; virtual;
			function GetIsModified : Boolean; virtual;
			/// <summary>TPersistableSettings.Init
			/// CreateSetting and CreateDefaultRelevantSetting should be called here
			/// </summary>
			procedure Init; virtual; abstract;
			procedure StoreSetting(const _name : string; const _v : Variant; const _bAsDefault : Boolean = False);
			function GetIniSectionName : string; virtual;
			procedure LoadDefault; virtual;
			procedure StoreDefaultSetting(const _name : string; const _v : Variant);
			function ToLogString : string; virtual;

		public
			constructor Create(const _ini : TMemIniFile); overload;
			constructor Create; overload;
			procedure Copy(const _other : TPersistableSettings); virtual;
			procedure ReLoad;
			procedure UpdateIniFile;
			property IniFile : TMemIniFile read GetIniFile;
			property IniSectionName : string read GetIniSectionName write SetIniSectionName;
			property IsAlreadyRead : Boolean read GetIsAlreadyRead;
			property IsModified : Boolean read GetIsModified;
			destructor Destroy; override;
			procedure CopyDefaultsToValues; virtual;
			procedure CopyValuesToDefaults; virtual;
			function GetSetting(const _name : string; const _bDefault : Boolean = False) : Variant; overload;
			/// <summary>TPersistableSettings.ReadIni
			/// Members.RedIni- should be called here
			/// </summary>
			procedure ReadIni; virtual;
			/// <summary>TPersistableSettings.RefreshMembers
			/// Refresh member variables by read settings value or default value
			/// </summary>
			procedure RefreshMembers(const _bWithDefault : Boolean); virtual; abstract;
			procedure SetSettingValue(_sKey : string; const _v : Variant);
			/// <summary>TPersistableSettings.Store
			/// Members.Store should be called here
			/// Writes to ini.
			/// </summary>
			procedure Store; virtual;
			/// <summary>TPersistableSettings.StoreAsDefault
			/// Members.StoreAsDefault should be called here
			/// </summary>
			procedure StoreAsDefault; virtual;
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
	System.StrUtils,
	ArrayEx;

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
		CreateIniFile();
		FbOwnIniFile := True;
	end;
	Init();
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
		setting.ValueType := VarType(_v);
		setting.IsModified := True;
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
	setting := nil;
	if FSettingsDict.TryGetValue(sKey, setting) and Assigned(setting) then begin
		if not setting.Equals(_sv) then begin
			setting := _sv;
			setting.IsModified := True;
			FSettingsDict.AddOrChange(sKey, setting);
		end;
	end else begin
		FSettingsDict.AddOrChange(sKey, _sv);
	end;
end;

procedure TPersistableSettings.AddOrSetDefaultValue(const _key : string; const _value : Variant; const _bSaveToIni : Boolean);
var
	dictKeyName : string;
	setting : ISettingVariant;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TPersistableSettings.AddOrSetDefaultValue');
	setting := TSettingVariant.Create(_value); // ISettingVariant
	setting.SaveToIni := _bSaveToIni;
	dictKeyName := GetDefaultDictKeyName(_key);
	AddOrSet(dictKeyName, setting);
	dbgMsg.MsgFmt('[%s] %s_DEFAULT.Value = %s', [IniSectionName, _key, VartoStr(FSettingsDict[dictKeyName].Value)]);
end;

procedure TPersistableSettings.Copy(const _other : TPersistableSettings);
begin
	if Assigned(_other) then begin
		_other.Store;
		FIsModified := _other.IsModified;
		FIsAlreadyRead := _other.IsAlreadyRead;
		for var key in _other.FSettingsDict.Keys do begin
			if key.StartsWith(IniSectionName) then begin
				FSettingsDict.AddOrChange(key, _other.FSettingsDict[key]);
			end;
		end;
        RefreshMembers(false);
	end;
end;

procedure TPersistableSettings.CopyDefaultsToValues;
var
	setting, defaultSetting : ISettingVariant;
begin
	for var key in FSettingsDict.Keys do begin
		if not key.StartsWith(IniSectionName) then
			continue;
		if key.EndsWith(DEFAULT_KEY) then
			continue;
		setting := FSettingsDict[key];
		if setting.IsDefaultRelevant then begin
			if FSettingsDict.TryGetValue(key + DEFAULT_KEY, defaultSetting) then begin
				setting.Value := defaultSetting.Value;
			end;
			FSettingsDict.AddOrChange(key, setting);
		end;
	end;
	RefreshMembers(True);
end;

procedure TPersistableSettings.CopyValuesToDefaults;
var
	setting, defaultSetting : ISettingVariant;
begin
	RefreshMembers(false);
	for var key in FSettingsDict.Keys do begin
		if not key.StartsWith(IniSectionName) then
			continue;
		if key.EndsWith(DEFAULT_KEY) then
			continue;
		setting := FSettingsDict[key];
		if setting.IsDefaultRelevant then begin
			if not FSettingsDict.TryGetValue(key + DEFAULT_KEY, defaultSetting) then begin
				defaultSetting.Value := setting.Value;
				FSettingsDict.AddOrChange(key, defaultSetting);
			end;
		end;
	end;
    RefreshMembers(true);
end;

procedure TPersistableSettings.CreateSetting(const _sName : string; const _setting : ISettingVariant);
begin
	if _setting.IsDefaultRelevant then begin
		FSettingsDict.AddOrChange(GetDefaultDictKeyName(_sName), _setting);
		TDebugUtils.MsgFmt('TPersistableSettings.CreateSetting - %s', [GetDefaultDictKeyName(_sName)]);
	end;
	FSettingsDict.AddOrChange(GetDictKeyName(_sName), _setting);
	TDebugUtils.MsgFmt('TPersistableSettings.CreateSetting - %s', [GetDictKeyName(_sName)]);
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

procedure TPersistableSettings.CreateDefaultRelevantSetting(const _sName : string; const _type : TVarType; const _value : Variant);
var
	setting : ISettingVariant;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TPersistableSettings.CreateDefaultRelevantSetting');
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

function TPersistableSettings.GetIsAlreadyRead : Boolean;
begin
	Result := FIsAlreadyRead;
end;

function TPersistableSettings.GetIsModified : Boolean;
var
	setting : ISettingVariant;
	arr : TArrayEx<ISettingVariant>;
begin
	if not FIsModified then begin
		arr := FSettingsDict.Values.ToArray;
		setting := TSettingVariant.Create(); // ISettingVariant
		setting.IsModified := True;
		FIsModified := 0 < arr.CountOf(setting, TComparer<ISettingVariant>.Construct(
			function(const Left, Right : ISettingVariant) : Integer
			begin
				Result := TComparer<Boolean>.Default.Compare(Left.IsModified, Right.IsModified);
			end));
	end;
	Result := FIsModified;
end;

function TPersistableSettings.InnerGetSetting(const _name : string; const _bDefaultValue : Boolean) : Variant;
var
	setting : ISettingVariant;
	keyName : string;
begin
	if _bDefaultValue then begin
		keyName := GetDefaultDictKeyName(_name);
	end else begin
		keyName := GetDictKeyName(_name);
	end;
	if FSettingsDict.TryGetValue(keyName, setting) then begin
		if (not setting.IsEmpty) then begin
			Result := setting.Value;
		end;
	end else begin
		var
		dbgMsg := TDebugMsgBeginEnd.New('TPersistableSettings.InnerGetSetting');
		dbgMsg.Msg(keyName + ' not found!');
	end;
end;

procedure TPersistableSettings.ReadIni;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TPersistableSettings.ReadIni');
	if not IsAlreadyRead then begin
		ReadSettings();
	end else begin
		dbgMsg.Msg('IsAlreadyRead');
	end;
end;

procedure TPersistableSettings.LoadDefault;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TPersistableSettings.LoadDefault');

	if not IsAlreadyRead then begin
		dbgMsg.Msg('ReadIni');
		ReadIni;
	end;

	CopyDefaultsToValues;
    RefreshMembers(true);
end;

procedure TPersistableSettings.SetSettingValue(_sKey : string; const _v : Variant);
begin
	AddOrSet(_sKey, _v);
end;

procedure TPersistableSettings.Store;
begin
	WriteSettings();
end;

procedure TPersistableSettings.StoreSetting(const _name : string; const _v : Variant; const _bAsDefault : Boolean = False);
begin
	TDebugUtils.DebugMessageFormat('TPersistableSettings.StoreSetting: in memory %s=%s as default=',
		[_name, VarToStr(_v), BoolToStr(_bAsDefault, True)]);
	if _bAsDefault then begin
		StoreDefaultSetting(_name, _v);
	end else begin
		AddOrSet(_name, _v);
	end;
end;

function TPersistableSettings.GetDefaultSetting(const _name : string) : Variant;
begin
	Result := InnerGetSetting(GetDefaultDictKeyName(_name), False);
end;

function TPersistableSettings.GetSetting(const _name : string; const _bDefault : Boolean = False) : Variant;
begin
	if _bDefault then begin
		Result := GetDefaultSetting(_name);
	end else begin
		Result := InnerGetSetting(_name, False);
	end;
end;

procedure TPersistableSettings.ReadSettings;
var
	baseName : string;
	strs : TStringList;
	name : string;
	setting : ISettingVariant;
	value : string;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TPersistableSettings.ReadSettings');
	strs := TStringList.Create();
	try
		if FIsAlreadyRead or (IniSectionName = ROOT_DUMMY_INI_SECTION) then begin
			dbgMsg.MsgFmt('FIsAlreadyRead %s Section: %s', [BoolToStr(FIsAlreadyRead, True), IniSectionName]);
			Exit;
		end;
		try
			dbgMsg.MsgFmt('Read section: %s', [IniSectionName]);
			FIniFile.ReadSectionValues(IniSectionName, strs);
			FIsAlreadyRead := strs.Count > 0;
			dbgMsg.Msg(strs.DelimitedText);

			dbgMsg.Msg('create base settings');
			// create base settings
			for var i : integer := 0 to strs.Count - 1 do begin
				name := strs.Names[i];
				if name.EndsWith(DEFAULT_KEY) then begin
					dbgMsg.MsgFmt('skip %s', [name]);
					continue;
				end;
				value := strs.Values[name];
				setting := TSettingVariant.Create(value); // ISettingVariant
				dbgMsg.MsgFmt('[%s] %s = %s', [IniSectionName, name, value]);
				AddOrSet(GetDictKeyName(name), setting);
			end;

			dbgMsg.Msg('set default settings');
			// set default settings
			for var i : integer := 0 to strs.Count - 1 do begin
				name := strs.Names[i];
				if name.EndsWith(DEFAULT_KEY) then begin
					value := strs.Values[name];
					baseName := name.Replace(DEFAULT_KEY, '');
					var
					dictKeyName := GetDictKeyName(baseName);
					if FSettingsDict.TryGetValue(dictKeyName, setting) then begin
						setting.IsDefaultRelevant := True;
						setting.Value := value;
						AddOrSet(dictKeyName, setting);
						AddOrSetDefaultValue(baseName, value, True);
						dbgMsg.MsgFmt('[%s] %s.Value = %s',
						{ } [IniSectionName, baseName, VartoStr(FSettingsDict[dictKeyName].Value)]);
						dbgMsg.MsgFmt('[%s] %s.Value = %s',
						{ } [IniSectionName, name, VartoStr(FSettingsDict[GetDefaultDictKeyName(baseName)].Value)]);
					end else begin
						// We didn't write it in INI before...
						AddOrSetDefaultValue(baseName, value, False);
						// raise ESettingsException.CreateFmt('Key not found in ini: %s', [baseName]);
					end;
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
	CopyValuesToDefaults;
	WriteSettings(True); // Write to mem ini, after UpdateIniFile will be saved
	UpdateIniFile;
end;

procedure TPersistableSettings.StoreDefaultSetting(const _name : string; const _v : Variant);
begin
	TDebugUtils.Msg('TPersistableSettings.StoreDefaultSetting: ' + _name + '=' + VarToStr(_v) + ' store in memory...');
	AddOrSet(GetDefaultDictKeyName(_name), _v);
	AddOrSetDefaultValue(_name, _v, True);
end;

function TPersistableSettings.ToLogString : string;
var
	strs : TStrings;
begin
	strs := TStringList.Create();
	try
		FIniFile.GetStrings(strs);
		Result := strs.DelimitedText;
	finally
		strs.Free;
	end
end;

procedure TPersistableSettings.UpdateIniFile;
begin
	FIniFile.UpdateFile;
end;

procedure TPersistableSettings.WriteSettings(_bDefault : Boolean = False);
var
	setting : ISettingVariant;
begin
	for var key in FSettingsDict.Keys do begin
		if ((not IniSectionName.IsEmpty) and (not key.StartsWith(IniSectionName))) then
			continue;
		if _bDefault then begin
			if (not key.EndsWith(DEFAULT_KEY)) then begin
				continue;
			end;
		end else begin
			if (key.EndsWith(DEFAULT_KEY)) then begin
				continue;
			end;
		end;
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
	var
	dbgMsg := TDebugMsgBeginEnd.New('TPersistableSettings.WriteToIni');

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
		dbgMsg.Msg(FIniFile.FileName + '[' + _sIniSection + '] ' + _sKey + '=' + VarToStr(_setting.Value) + ' stored');
	end;
end;

end.
