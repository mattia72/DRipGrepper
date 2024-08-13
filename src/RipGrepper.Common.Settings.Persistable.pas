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

	TRipGrepperSetting = record
		ValueType : TVarType;
		DefaultValue : Variant;
		Value : Variant;
		IsModified : Boolean;

		class function New(const _type : TVarType; const _value : Variant) : TRipGrepperSetting; static;
	end;

	TSettingsDictionary = TDictionary<string, TRipGrepperSetting>;

	TPersistableSettings = class(TSingletonImplementation, IIniPersistable)
		private
			FSettings : TSettingsDictionary;
			FDefaultSettings : TSettingsDictionary;
			FIniSectionName : string;
			procedure AddOrSet(_settingsDict : TSettingsDictionary; const _name : string; const _v : Variant);
			function GetIniFile : TMemIniFile;
			function GetSetting(const _name : string; _settingsDict : TSettingsDictionary) : string;
			procedure LoadSettings(const _sIniSection : string; _settingsDict : TSettingsDictionary);
			procedure SetIniFile(const Value : TMemIniFile);
			procedure SetIniSectionName(const Value : string);
			procedure SetIsModified(const Value : Boolean);
			procedure WriteSettings(const _sIniSection : string; _settingsDict : TSettingsDictionary);
			procedure WriteToIni(const iniSection, key : string; var setting : TRipGrepperSetting);

		protected
			FIniFile : TMemIniFile;
			FIsLoaded : Boolean;
			FIsModified : Boolean;

			procedure CreateSetting(const _sName : string; const _setting : TRipGrepperSetting; const _bAlsoDefault : Boolean = False);
			procedure CreateDefaultSetting(const _sName : string; const _setting : TRipGrepperSetting);
			function GetIsLoaded : Boolean; virtual;
			function GetIsModified : Boolean; virtual;
			procedure Init; virtual;
			procedure Load; virtual;
			procedure StoreSetting(const _name : string; const _v : Variant; const _bAlsoDefault : Boolean = False);
			function LoadSetting(const _name : string; const _bAlsoDefault : Boolean = False) : Variant;
			function LoadSettingDefaultValue(const _name : string) : Variant;
			procedure Store; virtual;
			function GetIniSectionName : string; virtual;
			function LoadDefaultSetting(const _name : string) : Variant;
			procedure StoreAsDefault; virtual;
			procedure StoreDefaultSetting(const _name : string; const _v : Variant);

		public
			constructor Create(const _ini : TMemIniFile); overload;
			constructor Create; overload;
			destructor Destroy; override;
			procedure Copy(const _other : TPersistableSettings);
			procedure ReLoad;
			procedure UpdateIniFile;
			property IniFile : TMemIniFile read GetIniFile write SetIniFile;
			property IniSectionName : string read FIniSectionName write SetIniSectionName;
			property IsLoaded : Boolean read GetIsLoaded;
			property IsModified : Boolean read GetIsModified write SetIsModified;
	end;

implementation

uses
	System.Classes,
	RipGrepper.Tools.DebugUtils,
	System.Variants,
	RipGrepper.Common.Constants;

constructor TPersistableSettings.Create(const _ini : TMemIniFile);
begin
	inherited Create();
	Create();
	FIniFile := _ini;
end;

constructor TPersistableSettings.Create;
begin
	inherited;
	FIsModified := False;
	FIsLoaded := False;
	FSettings := TSettingsDictionary.Create();
	FDefaultSettings := TSettingsDictionary.Create();
end;

destructor TPersistableSettings.Destroy;
begin
	FSettings.Free;
	FDefaultSettings.Free;
	inherited;
end;

procedure TPersistableSettings.AddOrSet(_settingsDict : TSettingsDictionary; const _name : string; const _v : Variant);
var
	setting : TRipGrepperSetting;
begin
	setting := _settingsDict[_name];
	if VarIsEmpty(setting.Value) or
	{ } VarIsNull(setting.Value) or
	{ } (VarType(setting.Value) <> VarType(_v)) or
	{ } (setting.Value <> _v) then begin
		setting.Value := _v;
		setting.IsModified := True;
		setting.ValueType := VarType(_v);
		_settingsDict.AddOrSetValue(_name, setting);
	end;
end;

procedure TPersistableSettings.Copy(const _other : TPersistableSettings);
begin
	FIsModified := _other.IsModified;
	FIsLoaded := _other.IsLoaded;
	FSettings.Free;
	FSettings := TSettingsDictionary.Create(_other.FSettings);
	FDefaultSettings.Free;
	FDefaultSettings := TSettingsDictionary.Create(_other.FDefaultSettings);
end;

procedure TPersistableSettings.CreateSetting(const _sName : string; const _setting : TRipGrepperSetting; const _bAlsoDefault : Boolean = False);
begin
	FSettings.Add(_sName, _setting);
	if _bAlsoDefault then begin
		CreateDefaultSetting(_sName, _setting);
	end;
end;

procedure TPersistableSettings.CreateDefaultSetting(const _sName : string; const _setting : TRipGrepperSetting);
begin
	FDefaultSettings.Add(_sName, _setting);
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

function TPersistableSettings.GetSetting(const _name : string; _settingsDict : TSettingsDictionary) : string;
var
	setting : TRipGrepperSetting;
begin
	if FSettings.TryGetValue(_name, setting) then begin
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
	LoadSettings(GetIniSectionName, FSettings);
	LoadSettings(DEFAULTS_INI_SECTION, FDefaultSettings);
end;

procedure TPersistableSettings.SetIniFile(const Value : TMemIniFile);
begin
	if Assigned(FIniFile) then
		FIniFile.Free;
	FIniFile := Value;
end;

procedure TPersistableSettings.SetIsModified(const Value : Boolean);
begin
	FIsModified := Value;
end;

procedure TPersistableSettings.Store;
begin
	WriteSettings(GetIniSectionName, FSettings);
end;

procedure TPersistableSettings.StoreSetting(const _name : string; const _v : Variant; const _bAlsoDefault : Boolean = False);
begin
	TDebugUtils.DebugMessage('TPersistableSettings.StoreSetting: ' + _name + '=' + VarToStr(_v) + ' store in memory...');
	AddOrSet(FSettings, _name, _v);
	if _bAlsoDefault then begin
		StoreDefaultSetting(_name, _v);
	end;
end;

function TPersistableSettings.LoadSetting(const _name : string; const _bAlsoDefault : Boolean = False) : Variant;
begin
	GetSetting(_name, FSettings);
	if _bAlsoDefault then begin
		LoadDefaultSetting(_name);
	end;
end;

function TPersistableSettings.LoadDefaultSetting(const _name : string) : Variant;
begin
	GetSetting(_name, FDefaultSettings);
end;

function TPersistableSettings.LoadSettingDefaultValue(const _name : string) : Variant;
var
	setting : TRipGrepperSetting;
begin
	setting := FSettings[_name];
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
		try
			FIniFile.ReadSectionValues(_sIniSection, strs);
			for var i : integer := 0 to strs.Count - 1 do begin
				name := strs.Names[i];
				value := strs.Values[name];
				var
					setting : TRipGrepperSetting;
				if _settingsDict.TryGetValue(name, setting) then begin
					setting.Value := value;
					TDebugUtils.DebugMessage(Format('TPersistableSettings.Load: [%s] %s = %s', [_sIniSection, name, value]));
					_settingsDict.AddOrSetValue(name, setting);
					// end else begin
					// AddOrSet(_settingsDict, name, value);
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
	FIsLoaded := False;
end;

procedure TPersistableSettings.SetIniSectionName(const Value : string);
begin
	FIniSectionName := Value;
end;

procedure TPersistableSettings.StoreAsDefault;
begin
	WriteSettings(DEFAULTS_INI_SECTION, FDefaultSettings);
end;

procedure TPersistableSettings.StoreDefaultSetting(const _name : string; const _v : Variant);
begin
	TDebugUtils.DebugMessage('TPersistableSettings.StoreDefaultSetting: ' + _name + '=' + VarToStr(_v) + ' store in memory...');
	AddOrSet(FDefaultSettings, _name, _v);
end;

procedure TPersistableSettings.UpdateIniFile;
begin
	FIniFile.UpdateFile;
end;

procedure TPersistableSettings.WriteSettings(const _sIniSection : string; _settingsDict : TSettingsDictionary);
var
	setting : TRipGrepperSetting;
begin
	for var key in _settingsDict.Keys do begin
		setting := _settingsDict[key];
		WriteToIni(_sIniSection, key, setting);
		setting.IsModified := False;
		_settingsDict[key] := setting;
	end;
end;

procedure TPersistableSettings.WriteToIni(const iniSection, key : string; var setting : TRipGrepperSetting);
begin
	if setting.IsModified or (iniSection = DEFAULTS_INI_SECTION) then begin
		case setting.ValueType of
			varString, varUString :
			FIniFile.WriteString(iniSection, key, setting.Value);
			varBoolean :
			FIniFile.WriteBool(iniSection, key, setting.Value);
			varInteger :
			FIniFile.WriteInteger(iniSection, key, setting.Value);
			else
			raise ESettingsException.Create('Settings Type not supported:' + VarTypeAsText(setting.ValueType));
		end;
		TDebugUtils.DebugMessage('TPersistableSettings.Store: ' + FIniFile.FileName + '[' + iniSection + '] ' + key + '=' + VarToStr(setting.Value) +
			' stored');
	end;
end;

class function TRipGrepperSetting.New(const _type : TVarType; const _value : Variant) : TRipGrepperSetting;
begin
	Result.ValueType := _type;
	Result.Value := _value;
	Result.DefaultValue := _value;
	Result.IsModified := False;
end;

end.
