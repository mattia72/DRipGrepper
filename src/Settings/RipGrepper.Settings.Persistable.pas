unit RipGrepper.Settings.Persistable;

interface

uses
	System.Generics.Defaults,
	System.IniFiles,
	System.Generics.Collections,
	System.SysUtils,
	RipGrepper.Settings.SettingVariant,
	RipGrepper.Settings.SettingsDictionary,
	System.SyncObjs,
	ArrayEx;

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

	TPersistableSettings = class(TNoRefCountObject, IIniPersistable)
		strict private
			class constructor Create;
			class destructor Destroy;

		private
			FIniFile : TMemIniFile;
			FbDefaultLoaded : Boolean;
			FbOwnIniFile : Boolean;
			FIniSectionName : string;
			FIsAlreadyRead : Boolean;
			FOwner : TPersistableSettings;
			procedure CreateIniFile;
			class var FLockObject : TObject;
			procedure FreeOwnIniFile;
			function GetIniFile : TMemIniFile;
			procedure ReadSettings;
			procedure SetIniFile(const Value : TMemIniFile);
			procedure SetIniSectionName(const Value : string);
			procedure WriteSettings(_bDefault : Boolean = False);
			procedure WriteToIni(const _sIniSection, _sKey : string; const _setting : ISettingVariant);

		protected
			FSettingsDict : TSettingsDictionary;
			FChildren : TArrayEx<TPersistableSettings>;
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
			constructor Create(const _Owner : TPersistableSettings); overload;
			constructor Create; overload;
			procedure Copy(const _other : TPersistableSettings); virtual;
			procedure ReLoad; virtual;

			property IniFile : TMemIniFile read GetIniFile write SetIniFile;
			property IniSectionName : string read GetIniSectionName write SetIniSectionName;
			property IsAlreadyRead : Boolean read GetIsAlreadyRead;
			property IsModified : Boolean read GetIsModified;
			property SettingsDict : TSettingsDictionary read FSettingsDict write FSettingsDict;
			destructor Destroy; override;
			function AddChildSettings(_settings : TPersistableSettings) : TPersistableSettings;
			function RemoveChildSettings(_settings : TPersistableSettings) : Boolean;
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

			/// Re-create memini to re-read ini file content
			class procedure ReCreateMemIni(var _ini : TMemIniFile; const _section : string); overload;
			procedure ReCreateMemIni; overload; virtual;
			/// <summary>
			/// Members.StoreToDict should be called here
			/// Writes to ini.
			/// </summary>
			procedure StoreToDict; virtual;
			/// <summary>
			/// Members.StoreAsDefaultsToDict should be called here
			/// </summary>
			procedure StoreAsDefaultsToDict; virtual;
			// <summary>
			// Thread safe write Settings to ini file
			// </summary>
			procedure UpdateIniFile;
	end;

implementation

uses
	System.Classes,
	RipGrepper.Tools.DebugUtils,
	System.Variants,
	RipGrepper.Common.Constants,
	Vcl.Forms,
	{$IFNDEF STANDALONE} RipGrepper.Common.IOTAUtils, {$ENDIF}
	System.IOUtils,
	System.StrUtils,

	RipGrepper.Tools.LockGuard;

constructor TPersistableSettings.Create(const _Owner : TPersistableSettings);
begin
	inherited Create();
	FOwner := _Owner;
	FIniFile := _Owner.FIniFile;
	FbOwnIniFile := False;
	Create();
end;

constructor TPersistableSettings.Create;
begin
	inherited;
	var
	dbgMsg := TDebugMsgBeginEnd.New('TPersistableSettings.Create', True);

	FIsModified := False;
	FIsAlreadyRead := False;
	FSettingsDict := TSettingsDictionary.Create(IniSectionName);
	dbgMsg.MsgFmt('Create FSettingsDict %p for section: %s', [Pointer(FSettingsDict), IniSectionName]);
	FbDefaultLoaded := False;
	if not Assigned(FIniFile) then begin
		CreateIniFile();
		FbOwnIniFile := True;
	end;
	Init();
end;

class constructor TPersistableSettings.Create;
begin
	FLockObject := TObject.Create;
end;

destructor TPersistableSettings.Destroy;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TPersistableSettings.Destroy', True);
	for var s in FChildren do begin
		s.Free;
	end;
	FreeOwnIniFile;
	dbgMsg.MsgFmt('Free FSettingsDict %p for section: %s', [Pointer(FSettingsDict), IniSectionName]);
	FSettingsDict.Free;
	inherited;
end;

class destructor TPersistableSettings.Destroy;
begin
	FLockObject.Free;
	inherited;
end;

function TPersistableSettings.AddChildSettings(_settings : TPersistableSettings) : TPersistableSettings;
begin
	FChildren.Add(_settings);
	Result := _settings;
end;

function TPersistableSettings.RemoveChildSettings(_settings : TPersistableSettings) : Boolean;
begin
	Result := FChildren.Remove(_settings);
	if not Result then begin
		raise Exception.Create('Couldn''t remove child settings.');
	end;
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
	var
	dbgMsg := TDebugMsgBeginEnd.New('TPersistableSettings.CreateIniFile', True);
	{$IFDEF STANDALONE}
	FIniFile := TMemIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'), TEncoding.UTF8);
	{$ELSE}
	FIniFile := TMemIniFile.Create(TPath.Combine(IOTAUTils.GetSettingFilePath, EXTENSION_NAME + '.ini'), TEncoding.UTF8);
	{$ENDIF}
	dbgMsg.MsgFmt('Create FIniFile %p of section: %s', [Pointer(FIniFile), GetIniSectionName()]);
end;

procedure TPersistableSettings.FreeOwnIniFile;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TPersistableSettings.FreeOwnIniFile', True);

	if FbOwnIniFile then begin
		dbgMsg.MsgFmt('Free FIniFile %p of section: %s', [Pointer(FIniFile), GetIniSectionName()]);
		FIniFile.Free;
		FIniFile := nil;
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

	for var s in FChildren do begin
		s.ReadIni();
	end;

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
	for var s in FChildren do begin
		s.StoreToDict;
	end;
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

class procedure TPersistableSettings.ReCreateMemIni(var _ini : TMemIniFile; const _section : string);
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TPersistableSettings.ReCreateMemIni', True);

	var
	iniFileName := _ini.FileName;
	dbgMsg.MsgFmt('Free FIniFile %p of section: %s', [Pointer(_ini), _section]);

	_ini.Free;
	_ini := TMemIniFile.Create(iniFileName, TEncoding.UTF8);

	dbgMsg.MsgFmt('ReCreate FIniFile %p of section: %s', [Pointer(_ini), _section]);
end;

procedure TPersistableSettings.ReCreateMemIni;
begin
	ReCreateMemIni(FIniFile, GetIniSectionName());
	var
	owner := FOwner;
	while Assigned(owner) do begin
		owner.IniFile := FIniFile;
		owner := owner.FOwner;
	end;
	for var s in FChildren do begin
		s.IniFile := IniFile;
	end;
end;

procedure TPersistableSettings.ReLoad;
begin
	for var s in FChildren do begin
		s.ReLoad;
	end;
	FIsAlreadyRead := False;
	ReadIni;
end;

procedure TPersistableSettings.SetIniFile(const Value : TMemIniFile);
begin
	FIniFile := Value;
	if not FbOwnIniFile and Assigned(FOwner) and (FOwner.IniFile <> FIniFile) then begin
		FOwner.IniFile := FIniFile;
	end;
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

	for var s in FChildren do begin
		dbgMsg.MsgFmt('Child update begin on section: %s', [GetIniSectionName()]);
		s.UpdateIniFile;
	end;

	if Assigned(FIniFile) then begin
		var
		lock := TLockGuard.NewLock(FLockObject);
		dbgMsg.Msg('Lock Entered');
		dbgMsg.MsgFmt('IniFile %p update begin on %s', [Pointer(FIniFile), GetIniSectionName()]);
		FIniFile.UpdateFile;
		dbgMsg.Msg('Lock Released');
	end else begin
		dbgMsg.ErrorMsg('IniFile not assigned!' + GetIniSectionName());
	end;

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

	if (not _setting.IsModified) and (not _sKey.EndsWith(DEFAULT_KEY)) then begin
		Exit;
	end;

	var
	lock := TLockGuard.NewLock(FLockObject);
	dbgMsg.MsgFmt('Lock Entered - Write [%s] %s', [_sIniSection, _sKey]);
	try
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
	except
		on E : Exception do
			dbgMsg.ErrorMsgFmt('%s', [E.Message]);
	end;
	dbgMsg.Msg(FIniFile.FileName + '[' + _sIniSection + '] ' + _sKey + '=' + VarToStr(_setting.Value) + ' stored');
end;

end.
