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
			FbOwnIniFile : Boolean;
			FSettings : TSettingsDictionary;
			FDefaultSettings : TSettingsDictionary;
			FIniSectionName : string;
			procedure AddOrSet(_settingsDict : TSettingsDictionary; const _name : string; const _v : Variant);
			procedure CreateIniFile;
			function GetIniFile: TMemIniFile;
			function GetSetting(const _name : string; _settingsDict : TSettingsDictionary) : Variant;
			procedure LoadSettings(const _sIniSection : string; _settingsDict : TSettingsDictionary);
			procedure SetIniSectionName(const Value : string);
			procedure SetIsModified(const Value : Boolean);
			procedure WriteSettings(const _sIniSection : string; _settingsDict : TSettingsDictionary);
			procedure WriteToIni(const _sIniSection, _sKey : string; var _setting : TRipGrepperSetting);

		protected
			FIniFile: TMemIniFile;
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
			property IniFile: TMemIniFile read GetIniFile;
			property IniSectionName : string read FIniSectionName write SetIniSectionName;
			property IsLoaded : Boolean read GetIsLoaded;
			property IsModified : Boolean read GetIsModified write SetIsModified;
	end;

implementation

uses
	System.Classes,
	RipGrepper.Tools.DebugUtils,
	System.Variants,
	RipGrepper.Common.Constants,
	Vcl.Forms,
	RipGrepper.Common.IOTAUtils,
	System.IOUtils;

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
	FSettings := TSettingsDictionary.Create();
	FDefaultSettings := TSettingsDictionary.Create();
	if not Assigned(FIniFile) then begin
		CreateIniFile;
		FbOwnIniFile := True;
	end;
end;

destructor TPersistableSettings.Destroy;
begin
	FSettings.Free;
	FDefaultSettings.Free;
	if FbOwnIniFile then begin
		FIniFile.Free;
	end;
	inherited;
end;

procedure TPersistableSettings.AddOrSet(_settingsDict : TSettingsDictionary; const _name : string; const _v : Variant);
var
	setting : TRipGrepperSetting;
begin
	_settingsDict.TryGetValue(_name, setting);
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

procedure TPersistableSettings.CreateIniFile;
begin
	if IOTAUTils.IsStandAlone then begin
		FIniFile := TMemIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'), TEncoding.UTF8);
	end else begin
		FIniFile := TMemIniFile.Create(TPath.Combine(IOTAUTils.GetSettingFilePath, EXTENSION_NAME + '.ini'), TEncoding.UTF8);
	end;
end;

function TPersistableSettings.GetIniFile: TMemIniFile;
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
	if FDefaultSettings.Count = 0 then begin
		LoadSettings(DEFAULTS_INI_SECTION, FDefaultSettings);
	end;
end;

procedure TPersistableSettings.LoadDefault;
begin
	LoadSettings(DEFAULTS_INI_SECTION, FDefaultSettings);
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
	Result := GetSetting(_name, FSettings);
	if _bAlsoDefault then begin
		LoadDefaultSetting(_name);
	end;
end;

function TPersistableSettings.LoadDefaultSetting(const _name : string) : Variant;
begin
	Result := GetSetting(_name, FDefaultSettings);
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
		if _sIniSection = ROOT_DUMMY_INI_SECTION then
			Exit;
		try
			FIniFile.ReadSectionValues(_sIniSection, strs);
			for var i : integer := 0 to strs.Count - 1 do begin
				name := strs.Names[i];
				value := strs.Values[name];
				var
					setting : TRipGrepperSetting;
				setting.Value := value;
				TDebugUtils.DebugMessage(Format('TPersistableSettings.Load: [%s] %s = %s', [_sIniSection, name, value]));
				AddOrSet(_settingsDict, name, value);
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

procedure TPersistableSettings.WriteToIni(const _sIniSection, _sKey : string; var _setting : TRipGrepperSetting);
begin
	if _setting.IsModified or (_sIniSection = DEFAULTS_INI_SECTION) then begin
		case _setting.ValueType of
			varString, varUString :
			FIniFile.WriteString(_sIniSection, _sKey, _setting.Value);
			varBoolean :
			FIniFile.WriteBool(_sIniSection, _sKey, _setting.Value);
			varInteger :
			FIniFile.WriteInteger(_sIniSection, _sKey, _setting.Value);
			else
			raise ESettingsException.Create('Settings Type not supported:' + VarTypeAsText(_setting.ValueType));
		end;
		TDebugUtils.DebugMessage('TPersistableSettings.Store: ' + FIniFile.FileName + '[' + _sIniSection + '] ' + _sKey + '=' + VarToStr(_setting.Value) +
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
