unit RipGrepper.Common.Settings.Persistable;

interface

uses
	System.Generics.Defaults,
	System.IniFiles,
	System.Generics.Collections,
	System.SysUtils;

type

	IIniPersistable = interface
		['{A841C46D-56AF-4391-AB88-4C9496589FF4}']
		function GetIniSectionName() : string;
		procedure Load;
		procedure Store;
	end;

	ESettingsException = class(Exception)

	end;

	TSettingVariant = record
		ValueType : TVarType;
		DefaultValue : Variant;
		Value : Variant;
		IsModified : Boolean;
		IsDefaultRelevant : Boolean;
		Section : string;
		class function New(const _type : TVarType; const _value : Variant; const _section : string = ''; const _isDefRelevant : Boolean = False)
			: TSettingVariant; static;
		class function NewDefault(const _type : TVarType; const _value : Variant) : TSettingVariant; static;
	end;

	TSettingsDictionary = TDictionary<string, TSettingVariant>;

	TPersistableSettings = class(TSingletonImplementation, IIniPersistable)
		strict private
			class constructor Create;
			class destructor Destroy;
			class var FSettingsDict : TSettingsDictionary;
			class var FbDefaultLoaded : Boolean;

		private

			FbOwnIniFile : Boolean;
			FIniSectionName : string;
			procedure AddOrSet(_settingsDict : TSettingsDictionary; const _name : string; const _v : Variant); overload;
			procedure AddOrSet(_settingsDict : TSettingsDictionary; const _name : string; const _sv : TSettingVariant); overload;
			procedure CreateIniFile;
			function GetIniFile : TMemIniFile;
			function GetSetting(const _name : string; _settingsDict : TSettingsDictionary): Variant; overload;
			procedure LoadSettings(const _sIniSection : string; _settingsDict : TSettingsDictionary);
			procedure SetIniSectionName(const Value : string);
			procedure SetIsModified(const Value : Boolean);
			procedure WriteSettings(_settingsDict : TSettingsDictionary; const _sIniSection : string = '');
			procedure WriteToIni(const _sIniSection, _sKey : string; var _setting : TSettingVariant);

		protected
			FIniFile : TMemIniFile;
			FIsLoaded : Boolean;
			FIsModified : Boolean;

			procedure CreateSetting(const _sName : string; const _setting : TSettingVariant);
			function GetIsLoaded : Boolean; virtual;
			function GetIsModified : Boolean; virtual;
			procedure Init; virtual;
			procedure Load; virtual;
			procedure StoreSetting(const _name : string; const _v : Variant; const _bAlsoDefault : Boolean = False);
			function GetSetting(const _name : string): Variant; overload;
			function LoadSettingDefaultValue(const _name : string) : Variant;
			procedure Store; virtual;
			function GetIniSectionName : string; virtual;
			procedure LoadDefault; virtual;
			function LoadDefaultSetting(const _name : string) : Variant;
			procedure StoreAsDefault; virtual;
			procedure StoreDefaultSetting(const _name : string; const _v : Variant);

		public
			constructor Create(const _ini : TMemIniFile); overload;
			constructor Create; overload;
			destructor Destroy; override;
			procedure Copy(const _other : TPersistableSettings); virtual;
			procedure ReLoad;
			procedure UpdateIniFile;
			property IniFile : TMemIniFile read GetIniFile;
			property IniSectionName : string read FIniSectionName write SetIniSectionName;
			property IsLoaded : Boolean read GetIsLoaded;
			property IsModified : Boolean read GetIsModified write SetIsModified;
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
	FIsLoaded := False;
	FSettingsDict := TSettingsDictionary.Create();
	if not Assigned(FIniFile) then begin
		CreateIniFile;
		FbOwnIniFile := True;
	end;
end;

class constructor TPersistableSettings.Create;
begin
	FSettingsDict := TSettingsDictionary.Create();
	FbDefaultLoaded := False;
end;

destructor TPersistableSettings.Destroy;
begin
	FSettingsDict.Free;
	if FbOwnIniFile then begin
		FIniFile.Free;
	end;
	inherited;
end;

class destructor TPersistableSettings.Destroy;
begin
	FSettingsDict.Free;
end;

procedure TPersistableSettings.AddOrSet(_settingsDict : TSettingsDictionary; const _name : string; const _v : Variant);
var
	setting : TSettingVariant;
begin
	var
	bExists := _settingsDict.TryGetValue(_name, setting);
	if VarIsEmpty(setting.Value) or
	{ } VarIsNull(setting.Value) or
	{ } (VarType(setting.Value) <> VarType(_v)) or
	{if} (setting.Value <> _v) then begin
		setting.Value := _v;
		setting.IsModified := bExists;
		setting.ValueType := VarType(_v);
		if not bExists then begin
			setting.IsDefaultRelevant := False;
		end;
		_settingsDict.AddOrSetValue(_name, setting);
	end;
end;

procedure TPersistableSettings.AddOrSet(_settingsDict : TSettingsDictionary; const _name : string; const _sv : TSettingVariant);
var
	setting : TSettingVariant;
begin
	_settingsDict.TryGetValue(_name, setting);
	if VarIsEmpty(setting.Value) or
	{ } VarIsNull(setting.Value) or
	{ } (VarType(setting.Value) <> VarType(_sv.Value)) or
	{ } (setting.Value <> _sv.Value) then begin
		_settingsDict.AddOrSetValue(_name, _sv);
	end;
end;

procedure TPersistableSettings.Copy(const _other : TPersistableSettings);
begin
	FIsModified := _other.IsModified;
	FIsLoaded := _other.IsLoaded;
	FSettingsDict.Free;
	FSettingsDict := TSettingsDictionary.Create(_other.FSettingsDict);
end;

procedure TPersistableSettings.CreateSetting(const _sName : string; const _setting : TSettingVariant);
begin
	FSettingsDict.Add(_sName, _setting);
end;

procedure TPersistableSettings.CreateIniFile;
begin
	if IOTAUTils.IsStandAlone then begin
		FIniFile := TMemIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'), TEncoding.UTF8);
	end else begin
		FIniFile := TMemIniFile.Create(TPath.Combine(IOTAUTils.GetSettingFilePath, EXTENSION_NAME + '.ini'), TEncoding.UTF8);
	end;
end;

function TPersistableSettings.GetIniFile : TMemIniFile;
begin
	Result := FIniFile;
end;

function TPersistableSettings.GetIniSectionName : string;
begin
	Result := FIniSectionName;
end;

function TPersistableSettings.GetIsLoaded : Boolean;
begin
	Result := FIsLoaded;
end;

function TPersistableSettings.GetIsModified : Boolean;
begin
	Result := FIsModified;
end;

function TPersistableSettings.GetSetting(const _name : string; _settingsDict : TSettingsDictionary) : Variant;
var
	setting : TSettingVariant;
begin
	if FSettingsDict.TryGetValue(_name, setting) then begin
		if not(VarIsEmpty(setting.Value) or VarIsNull(setting.Value)) then begin
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

procedure TPersistableSettings.Load;
begin
	Init();
	LoadSettings(GetIniSectionName, FSettingsDict);
end;

procedure TPersistableSettings.LoadDefault;
begin
	if not FbDefaultLoaded then begin
		for var val in FSettingsDict.Values do begin
			if not val.IsDefaultRelevant then
				continue;
			LoadSettings(val.Section, FSettingsDict);
		end;
		FbDefaultLoaded := True;
	end;
end;

procedure TPersistableSettings.SetIsModified(const Value : Boolean);
begin
	FIsModified := Value;
end;

procedure TPersistableSettings.SetSettingValue(_sKey : string; const _v : Variant);
begin
	AddOrSet(FSettingsDict, _sKey, _v);
end;

procedure TPersistableSettings.Store;
begin
	WriteSettings(FSettingsDict);
end;

procedure TPersistableSettings.StoreSetting(const _name : string; const _v : Variant; const _bAlsoDefault : Boolean = False);
begin
	TDebugUtils.DebugMessage('TPersistableSettings.StoreSetting: ' + _name + '=' + VarToStr(_v) + ' store in memory...');
	AddOrSet(FSettingsDict, _name, _v);
	if _bAlsoDefault then begin
		StoreDefaultSetting(_name, _v);
	end;
end;

function TPersistableSettings.GetSetting(const _name : string): Variant;
begin
	Result := GetSetting(_name, FSettingsDict);
end;

function TPersistableSettings.LoadDefaultSetting(const _name : string) : Variant;
begin
	Result := GetSetting('DEFAULT_' + _name, FSettingsDict);
end;

function TPersistableSettings.LoadSettingDefaultValue(const _name : string) : Variant;
var
	setting : TSettingVariant;
begin
	setting := FSettingsDict[_name];
	Result := setting.DefaultValue;
	TDebugUtils.DebugMessage('TPersistableSettings.LoadSettingDefaultValue: ' + _name + ' ' + Result);
end;

procedure TPersistableSettings.LoadSettings(const _sIniSection : string; _settingsDict : TSettingsDictionary);
var
	strs : TStringList;
	name : string;
	value : string;
begin
	strs := TStringList.Create();
	try
		if _sIniSection = ROOT_DUMMY_INI_SECTION then
			Exit;
		try
			FIniFile.ReadSectionValues(_sIniSection, strs);
			for var i : integer := 0 to strs.Count - 1 do begin
				name := strs.Names[i];
				value := strs.Values[name];
				var
					setting : TSettingVariant;
				setting.Value := value;
				setting.Section := _sIniSection;
				TDebugUtils.DebugMessage(Format('TPersistableSettings.Load: [%s] %s = %s', [_sIniSection, name, value]));
				AddOrSet(_settingsDict, name, setting);
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
	FIsLoaded := False;
end;

procedure TPersistableSettings.SetIniSectionName(const Value : string);
begin
	FIniSectionName := Value;
end;

procedure TPersistableSettings.StoreAsDefault;
begin
	WriteSettings(FSettingsDict, DEFAULTS_INI_SECTION);
end;

procedure TPersistableSettings.StoreDefaultSetting(const _name : string; const _v : Variant);
begin
	TDebugUtils.DebugMessage('TPersistableSettings.StoreDefaultSetting: ' + _name + '=' + VarToStr(_v) + ' store in memory...');
	AddOrSet(FSettingsDict, _name, _v);
end;

procedure TPersistableSettings.UpdateIniFile;
begin
	FIniFile.UpdateFile;
end;

procedure TPersistableSettings.WriteSettings(_settingsDict : TSettingsDictionary; const _sIniSection : string = '');
var
	setting : TSettingVariant;
begin
	for var key in _settingsDict.Keys do begin
		if (_sIniSection = DEFAULTS_INI_SECTION) and (not _settingsDict[key].IsDefaultRelevant) then
			continue;
		setting := _settingsDict[key];

		WriteToIni(
			{ } IfThen(_sIniSection = DEFAULTS_INI_SECTION, DEFAULTS_INI_SECTION, setting.Section), key, setting);
		setting.IsModified := False;
		_settingsDict[key] := setting;
	end;
end;

procedure TPersistableSettings.WriteToIni(const _sIniSection, _sKey : string; var _setting : TSettingVariant);
var
	v : Variant;
begin
	if _setting.IsModified or (_sIniSection = DEFAULTS_INI_SECTION) then begin
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
		TDebugUtils.DebugMessage('TPersistableSettings.Store: ' + FIniFile.FileName + '[' + _sIniSection + '] ' + _sKey + '=' + VarToStr(_setting.Value) +
			' stored');
	end;
end;

class function TSettingVariant.New(const _type : TVarType; const _value : Variant; const _section : string = ''; const _isDefRelevant : Boolean = False)
	: TSettingVariant;
begin
	Result.ValueType := _type;
	Result.Value := _value;
	Result.DefaultValue := _value;
	Result.IsModified := False;
	Result.Section := _section;
	Result.IsDefaultRelevant := _isDefRelevant;
end;

class function TSettingVariant.NewDefault(const _type : TVarType; const _value : Variant) : TSettingVariant;
begin
	Result.ValueType := _type;
	Result.Value := _value;
	Result.DefaultValue := _value;
	Result.IsModified := False;
	Result.Section := DEFAULTS_INI_SECTION;
	Result.IsDefaultRelevant := True;
end;

end.
