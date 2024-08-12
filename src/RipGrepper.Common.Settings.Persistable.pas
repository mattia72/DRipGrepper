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
		ValueType : integer;
		DefaultValue : Variant;
		Value : Variant;
		IsModified : Boolean;

		class function New(const _type : Integer; const _value : Variant) : TRipGrepperSetting; static;
	end;

	TSettingsDictionary = TDictionary<string, TRipGrepperSetting>;

	TPersistableSettings = class(TSingletonImplementation, IIniPersistable)
		private
			FSettings : TSettingsDictionary;
			FIniSectionName : string;
			function GetIniFile : TMemIniFile;
			procedure SetIniFile(const Value : TMemIniFile);
			procedure SetIniSectionName(const Value : string);
			procedure SetIsModified(const Value : Boolean);
			procedure WriteToIni(const iniSection, key : string; var setting : TRipGrepperSetting);

		protected
			FIniFile : TMemIniFile;
			FIsLoaded : Boolean;
			FIsModified : Boolean;

			procedure CreateSetting(const _sName : string; const _setting : TRipGrepperSetting);
			function GetIsLoaded : Boolean; virtual;
			function GetIsModified : Boolean; virtual;
			procedure Init; virtual;
			procedure Load; virtual;
			procedure StoreSetting(const _name : string; const _v : Variant);
			function LoadSetting(const _name : string) : Variant;
			function LoadDefaultSetting(const _name : string) : Variant;
			procedure Store; virtual;
			function GetIniSectionName : string; virtual;
			procedure StoreAsDefault; virtual;

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
end;

destructor TPersistableSettings.Destroy;
begin
	FSettings.Free;
	inherited;
end;

procedure TPersistableSettings.Copy(const _other : TPersistableSettings);
begin
	FIsModified := _other.IsModified;
	FIsLoaded := _other.IsLoaded;
	FSettings.Free;
	FSettings := TSettingsDictionary.Create(_other.FSettings);
end;

procedure TPersistableSettings.CreateSetting(const _sName : string; const _setting : TRipGrepperSetting);
begin
	FSettings.Add(_sName, _setting);
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

procedure TPersistableSettings.Init;
begin
	//
end;

procedure TPersistableSettings.Load;
var
	strs : TStringList;
	name : string;
	value : string;

begin
	Init();
	strs := TStringList.Create();
	try
		// if GetIniSectionName = ROOT_DUMMY_INI_SECTION then
		try

			FIniFile.ReadSectionValues(GetIniSectionName, strs);
			for var i : integer := 0 to strs.Count - 1 do begin
				name := strs.Names[i];
				value := strs.Values[name];
				var setting : TRipGrepperSetting;
				if FSettings.TryGetValue(name, setting) then begin
					setting.Value := value;
					TDebugUtils.DebugMessage(Format('TPersistableSettings.Load: [%s] %s = %s', [GetIniSectionName, name, value]));
				end;
				FSettings.AddOrSetValue(name, setting);
			end;
		except
			on E : Exception do
				raise;
		end;

	finally
		strs.Free;
	end;
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
var
	setting : TRipGrepperSetting;
begin
	for var key in FSettings.Keys do begin
		setting := FSettings[key];
		var
		iniSection := GetIniSectionName;
		WriteToIni(iniSection, key, setting);
	end;
end;

procedure TPersistableSettings.StoreSetting(const _name : string; const _v : Variant);
var
	setting : TRipGrepperSetting;
begin
	TDebugUtils.DebugMessage('TPersistableSettings.StoreSetting: ' + _name + '=' + VarToStr(_v) + ' stored in memory');

	setting := FSettings[_name];
	if VarIsEmpty(setting.Value) or
	{ } VarIsNull(setting.Value) or
	{ } (VarType(setting.Value) <> VarType(_v)) or
	{ } (setting.Value <> _v) then begin
		setting.Value := _v;
		setting.IsModified := True;
		FSettings.AddOrSetValue(_name, setting);
	end;
end;

function TPersistableSettings.LoadSetting(const _name : string) : Variant;
var
	setting : TRipGrepperSetting;
begin
	setting := FSettings[_name];
	if not(VarIsEmpty(setting.Value) or VarIsNull(setting.Value)) then begin
		Result := setting.Value;
	end;
	TDebugUtils.DebugMessage('TPersistableSettings.LoadSetting: ' + _name + ' ' + VarToStr(Result));
end;

function TPersistableSettings.LoadDefaultSetting(const _name : string) : Variant;
var
	setting : TRipGrepperSetting;
begin
	setting := FSettings[_name];
	Result := setting.DefaultValue;
	TDebugUtils.DebugMessage('TPersistableSettings.LoadDefaultSetting: ' + _name + ' ' + Result);
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
var
	setting : TRipGrepperSetting;
begin
	for var key in FSettings.Keys do begin
		setting := FSettings[key];
		WriteToIni(DEFAULTS_INI_SECTION, key, setting);
	end;
end;

procedure TPersistableSettings.UpdateIniFile;
begin
	FIniFile.UpdateFile;
end;

procedure TPersistableSettings.WriteToIni(const iniSection, key : string; var setting : TRipGrepperSetting);
begin
	if setting.IsModified or (iniSection = DEFAULTS_INI_SECTION) then begin
		case setting.ValueType of
			vtString :
			FIniFile.WriteString(iniSection, key, setting.Value);
			vtBoolean :
			FIniFile.WriteBool(iniSection, key, setting.Value);
			vtInteger :
			FIniFile.WriteInteger(iniSection, key, setting.Value);
			else
			raise ESettingsException.Create('Settings Type not supported:' + setting.ValueType.ToString);
		end;
		TDebugUtils.DebugMessage('TPersistableSettings.Store: ' + FIniFile.FileName + '[' + GetIniSectionName + '] ' + key + '=' +
			VarToStr(setting.Value) + ' stored');
		setting.IsModified := False;
		FSettings[key] := setting;
	end;
end;

class function TRipGrepperSetting.New(const _type : Integer; const _value : Variant) : TRipGrepperSetting;
begin
	Result.ValueType := _type;
	Result.Value := _value;
	Result.DefaultValue := _value;
	Result.IsModified := False;
end;

end.
