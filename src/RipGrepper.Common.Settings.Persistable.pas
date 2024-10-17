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
		procedure LoadFromDict();
		procedure LoadDefaultsFromDict();
		procedure StoreToDict;
		procedure StoreAsDefaultsToDict;
	end;

	ESettingsException = class(Exception)

	end;

	TPersistableSettings = class(TSingletonImplementation, IIniPersistable)
		private
			FbDefaultLoaded : Boolean;
			FbOwnIniFile : Boolean;
			FIniSectionName : string;
			FIsAlreadyRead : Boolean;
			procedure CreateIniFile;
			function GetIniFile: TMemIniFile;
			procedure ReadSettings;
			procedure SetIniFile(const Value: TMemIniFile);
			procedure SetIniSectionName(const Value : string);
			procedure WriteSettings(_bDefault : Boolean = False);
			procedure WriteToIni(const _sIniSection, _sKey : string; const _setting : ISettingVariant);

		protected
			FIniFile : TMemIniFile;
			FSettingsDict : TSettingsDictionary;

			FIsModified : Boolean;

			function GetIsAlreadyRead : Boolean; virtual;
			function GetIsModified : Boolean; virtual;
			/// <summary>TPersistableSettings.Init
			/// CreateSetting and CreateDefaultRelevantSetting should be called here
			/// </summary>
			procedure Init; virtual; abstract;
			function GetIniSectionName : string; virtual;
			procedure InnerLoadDefaultsFromDict; virtual;
			function ToLogString : string; virtual;

		public
			constructor Create(const _ini : TMemIniFile); overload;
			constructor Create; overload;
			procedure Copy(const _other : TPersistableSettings); virtual;
			procedure ReLoad; virtual;
			procedure UpdateIniFile;
			property IniFile: TMemIniFile read GetIniFile write SetIniFile;
			property IniSectionName : string read GetIniSectionName write SetIniSectionName;
			property IsAlreadyRead : Boolean read GetIsAlreadyRead;
			property IsModified : Boolean read GetIsModified;
			property SettingsDict : TSettingsDictionary read FSettingsDict write FSettingsDict;
			destructor Destroy; override;
			procedure CopyDefaultsToValues; virtual;
			procedure CopyValuesToDefaults; virtual;
			/// <summary>TPersistableSettings.ReadIni
			/// Members.RedIni- should be called here
			/// </summary>
			procedure ReadIni; virtual;
			/// <summary>TPersistableSettings.LoadFromDict
			/// Refresh member variables by read settings value or default value
			/// </summary>
			procedure LoadFromDict(); virtual; abstract;
			procedure LoadDefaultsFromDict; virtual; abstract;
			class procedure ReCreateMemIni(var _ini : TMemIniFile); overload;
			procedure ReCreateMemIni; overload; virtual;
			/// <summary>TPersistableSettings.StoreToDict
			/// Members.StoreToDict should be called here
			/// Writes to ini.
			/// </summary>
			procedure StoreToDict; virtual;
			/// <summary>TPersistableSettings.StoreAsDefaultsToDict
			/// Members.StoreAsDefaultsToDict should be called here
			/// </summary>
			procedure StoreAsDefaultsToDict; virtual;
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
	FSettingsDict := TSettingsDictionary.Create(IniSectionName);
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

procedure TPersistableSettings.Copy(const _other : TPersistableSettings);
begin
	if Assigned(_other) then begin
		_other.StoreToDict;
		FIsModified := _other.IsModified;
		FIsAlreadyRead := _other.IsAlreadyRead;
		for var key in _other.SettingsDict.Keys do begin
			if key.StartsWith(IniSectionName) then begin
				FSettingsDict.AddOrChange(key, _other.SettingsDict[key]);
			end;
		end;
		LoadFromDict();
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
	LoadDefaultsFromDict();
end;

procedure TPersistableSettings.CopyValuesToDefaults;
var
	setting, defaultSetting : ISettingVariant;
begin
	LoadFromDict();
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
	LoadDefaultsFromDict;
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

procedure TPersistableSettings.InnerLoadDefaultsFromDict;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TPersistableSettings.InnerLoadDefaultsFromDict');

	if not IsAlreadyRead then begin
		dbgMsg.Msg('ReadIni');
		ReadIni;
	end;

	CopyDefaultsToValues;
	LoadDefaultsFromDict();
end;

procedure TPersistableSettings.StoreToDict;
begin
	WriteSettings();
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
				SettingsDict.AddOrSet(name, setting);
			end;

			dbgMsg.Msg('set default settings');
			// set default settings
			for var i : integer := 0 to strs.Count - 1 do begin
				name := strs.Names[i];
				if name.EndsWith(DEFAULT_KEY) then begin
					value := strs.Values[name];
					baseName := name.Replace(DEFAULT_KEY, '');
					var
					dictKeyName := SettingsDict.GetDictKeyName(baseName);
					if SettingsDict.TryGetValue(dictKeyName, setting) then begin
						setting.IsDefaultRelevant := True;
						setting.Value := value;
						SettingsDict.AddOrSet(baseName, setting);
						SettingsDict.AddOrSetDefault(baseName, value, True);
						dbgMsg.MsgFmt('[%s] %s.Value = %s',
						{ } [IniSectionName, baseName, VartoStr(SettingsDict[dictKeyName].Value)]);
						dbgMsg.MsgFmt('[%s] %s.Value = %s',
						{ } [IniSectionName, name, VartoStr(SettingsDict[SettingsDict.GetDefaultDictKeyName(baseName)].Value)]);
					end else begin
						// We didn't write it in INI before...
						SettingsDict.AddOrSetDefault(baseName, value, False);
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

class procedure TPersistableSettings.ReCreateMemIni(var _ini : TMemIniFile);
begin
	var
	iniFileName := _ini.FileName;
	_ini.Free;
	_ini := TMemIniFile.Create(iniFileName, TEncoding.UTF8);
end;

procedure TPersistableSettings.ReCreateMemIni;
begin
	ReCreateMemIni(FIniFile);
end;

procedure TPersistableSettings.ReLoad;
begin
	FIsAlreadyRead := False;
	ReadIni;
end;

procedure TPersistableSettings.SetIniFile(const Value: TMemIniFile);
begin
	FIniFile := Value;
end;

procedure TPersistableSettings.SetIniSectionName(const Value : string);
begin
	FIniSectionName := Value;
end;

procedure TPersistableSettings.StoreAsDefaultsToDict;
begin
	CopyValuesToDefaults;
	WriteSettings(True); // Write to mem ini, after UpdateIniFile will be saved
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
	var
	dbgMsg := TDebugMsgBeginEnd.New('TPersistableSettings.UpdateIniFile');
	dbgMsg.Msg('IniFile:' + FIniFile.FileName);
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

		SettingsDict[key] := setting;
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
