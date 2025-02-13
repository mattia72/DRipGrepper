unit RipGrepper.Settings.SettingsDictionary;

interface

uses
	RipGrepper.Settings.SettingVariant,
	System.Generics.Collections,
	System.Variants,
	System.Classes;

type
	TSettingsDictionary = class(TDictionary<string, ISettingVariant>)

		private
			FSectionName : string;
			function GetDefaultSetting(const _name : string) : Variant;
			function InnerGetSetting(const _name : string; const _bDefaultValue : Boolean) : Variant; overload;
			property SectionName : string read FSectionName;

		protected
		public
			constructor Create(const _section : string); overload;
			constructor Create; overload;
			destructor Destroy; override;

			procedure AddOrChange(const Key : string; Value : ISettingVariant);
			procedure AddOrSet(const _name : string; const _v : Variant); overload;
			procedure AddOrSet(const _name : string; _sv : ISettingVariant); overload;
			procedure AddOrSetDefault(const _key : string; const _value : Variant; const _bSaveToIni : Boolean);

			procedure CreateDefaultRelevantSetting(const _sName : string; const _type : TVarType; const _value : Variant); overload;
			procedure CreateSetting(const _sName : string; _setting : ISettingVariant);
				overload;
			procedure CreateSetting(const _sName : string; const _type : TVarType; const _value : Variant;
				const _isDefRelevant : Boolean = False); overload;

			function GetDefaultDictKeyName(const _key : string) : string;
			function GetDictKeyName(const _key : string; const _bForDefault : Boolean = False) : string;
			function GetSetting(const _name : string; const _bDefault : Boolean = False) : Variant; overload;
			procedure GetSettingsSectionValues(const _sectionName : string; var _sectionValues : TStringList);
			procedure SetSettingValue(_sKey : string; const _v : Variant);
			procedure StoreDefaultSetting(const _name : string; const _v : Variant);
			procedure StoreSetting(const _name : string; const _v : Variant; const _bAsDefault : Boolean = False);
	end;

implementation

uses
	RipGrepper.Common.Constants,
	RipGrepper.Tools.DebugUtils,
	System.StrUtils,
	System.SysUtils,
	System.IniFiles,
	RipGrepper.Helper.Types;

constructor TSettingsDictionary.Create(const _section : string);
begin
	Create;
	var
	dbgMsg := TDebugMsgBeginEnd.New('TSettingsDictionary.Create(_section)', True);
	FSectionName := _section;
	dbgMsg.MsgFmt('Create %p for section: %s', [Pointer(self), FSectionName]);
end;

constructor TSettingsDictionary.Create;
begin
	inherited;
	var
	dbgMsg := TDebugMsgBeginEnd.New('TSettingsDictionary.Create', True);
	dbgMsg.MsgFmt('Create %p for section: ???', [Pointer(self)]);
end;

destructor TSettingsDictionary.Destroy;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TSettingsDictionary.Destroy', True);
	dbgMsg.MsgFmt('Destroy %p for section: %s', [Pointer(self), FSectionName]);

	for var key in Keys do begin
		// dbgMsg.MsgFmt('Destroy key: %s', [key]);
		Self[key] := nil;
	end;
	inherited Destroy;
end;

procedure TSettingsDictionary.AddOrChange(const Key : string; Value : ISettingVariant);
var
	setting : ISettingVariant;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TSettingsDictionary.AddOrChange');
	if self.TryGetValue(Key, setting) {and Assigned(setting) and (not setting.Equals(Value)) } then begin
		self[Key] := nil;
//      self.Remove(Key);
		dbgMsg.MsgFmt('Remove %s', [Key]);
	end;
	dbgMsg.MsgFmt('AddOrSetValue %s=%s', [Key, VarToStr(Value.Value)]);
	AddOrSetValue(Key, Value);
end;

procedure TSettingsDictionary.AddOrSet(const _name : string; const _v : Variant);
var
	bExists : Boolean;
	setting : ISettingVariant;
	sKey : string;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TSettingsDictionary.AddOrSet');

	sKey := GetDictKeyName(_name);
	bExists := self.TryGetValue(sKey, setting);
	if bExists and (setting.IsEmpty or (setting.ValueType <> VarType(_v)) or
		{ } (setting.Value <> _v)) then begin
		setting.Value := _v;
		setting.IsModified := True;
		if not bExists then begin
			setting.IsDefaultRelevant := False;
		end;
		self.AddOrChange(sKey, setting);
		dbgMsg.MsgFmt('AddOrChange %s=%s', [sKey, VarToStr(setting.Value)]);
	end else begin
		if not bExists then begin
			setting := TSettingVariant.Create(_v); // ISettingVariant
			self.Add(sKey, setting);
			dbgMsg.MsgFmt('Add %s=%s', [sKey, VarToStr(setting.Value)]);
		end else begin
        	setting.IsModified := True;
			self.AddOrChange(sKey, setting);
			dbgMsg.MsgFmt('AddOrChange %s=%s', [sKey, VarToStr(setting.Value)]);
		end;
	end;
	dbgMsg.MsgFmt('sKey = %s, Value = %s', [sKey, VarToStr(setting.Value)]);
end;

procedure TSettingsDictionary.AddOrSet(const _name : string; _sv :
	ISettingVariant);
var
	setting : ISettingVariant;
	sKey : string;
begin
	sKey := GetDictKeyName(_name);
	setting := nil;
	if self.TryGetValue(sKey, setting) and Assigned(setting) then begin
		if not setting.Equals(_sv) then begin
			setting := _sv;
			setting.IsModified := True;
			self.AddOrChange(sKey, setting);
		end;
	end else begin
		self.AddOrChange(sKey, _sv);
	end;
end;

procedure TSettingsDictionary.AddOrSetDefault(const _key : string; const _value : Variant; const _bSaveToIni : Boolean);
var
	dictKeyName : string;
	setting : ISettingVariant;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TSettingsDictionary.AddOrSetDefault');
	setting := TSettingVariant.Create(_value); // ISettingVariant
	setting.SaveToIni := _bSaveToIni;
	dictKeyName := GetDefaultDictKeyName(_key);
	AddOrSet(dictKeyName, setting);
	dbgMsg.MsgFmt('[%s] %s_DEFAULT.Value = %s', [SectionName, _key, VartoStr(self[dictKeyName].Value)]);
end;

procedure TSettingsDictionary.CreateDefaultRelevantSetting(const _sName : string; const _type : TVarType; const _value : Variant);
var
	setting : ISettingVariant;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TSettingsDictionary.CreateDefaultRelevantSetting');
	setting := TSettingVariant.Create(_type, _value, True); // ISettingVariant
	CreateSetting(_sName, setting);
end;

procedure TSettingsDictionary.CreateSetting(const _sName : string; _setting : ISettingVariant);
begin
	if _setting.IsDefaultRelevant then begin
		self.AddOrChange(GetDefaultDictKeyName(_sName), _setting);
		TDebugUtils.MsgFmt('TSettingsDictionary.CreateSetting - %s', [GetDefaultDictKeyName(_sName)]);
	end;
	self.AddOrChange(GetDictKeyName(_sName), _setting);
	TDebugUtils.MsgFmt('TSettingsDictionary.CreateSetting - %s', [GetDictKeyName(_sName)]);
end;

procedure TSettingsDictionary.CreateSetting(const _sName : string; const _type : TVarType; const _value : Variant;
	const _isDefRelevant : Boolean = False);
var
	setting : ISettingVariant;
begin
	setting := TSettingVariant.Create(_type, _value, _isDefRelevant); // ISettingVariant
	CreateSetting(_sName, setting);
end;

function TSettingsDictionary.GetDefaultDictKeyName(const _key : string) : string;
begin
	Result := GetDictKeyName(_key) + DEFAULT_KEY;
end;

function TSettingsDictionary.GetDefaultSetting(const _name : string) : Variant;
begin
	Result := InnerGetSetting(GetDefaultDictKeyName(_name), False);
end;

function TSettingsDictionary.GetDictKeyName(const _key : string; const _bForDefault : Boolean = False) : string;
begin
	if _bForDefault then begin
		Result := GetDefaultDictKeyName(_key);
	end else begin
		if _key.Contains(ARRAY_SEPARATOR) then begin
			Result := _key;
		end else begin
			Result := SectionName + ARRAY_SEPARATOR + _key;
		end;
	end;
end;

function TSettingsDictionary.GetSetting(const _name : string; const _bDefault : Boolean = False) : Variant;
begin
	if _bDefault then begin
		Result := GetDefaultSetting(_name);
	end else begin
		Result := InnerGetSetting(_name, False);
	end;
end;

procedure TSettingsDictionary.GetSettingsSectionValues(const _sectionName : string; var _sectionValues : TStringList);
var
	setting : ISettingVariant;
	settingSection : string;
	settingName : string;
	tmpIni : TMemIniFile;
	tmpFile : TAutoDeleteTempFile;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TSettingsDictionary.GetSettingsSectionValues');

	_sectionValues.Clear;
	tmpIni := TMemIniFile.Create(tmpFile.FilePath);
	try
		for var key in Keys do begin
			var
			arr := key.Split([ARRAY_SEPARATOR]);
			settingSection := arr[0];
			settingName := arr[1];
			setting := self[key];
			dbgMsg.MsgFmt('Write [%s] %s', [settingSection, settingName]);
			setting.WriteToMemIni(tmpIni, settingSection, settingName);
		end;
		tmpIni.ReadSectionValues(_sectionName, _sectionValues);
	finally
		tmpIni.Free;
	end;
end;

function TSettingsDictionary.InnerGetSetting(const _name : string; const _bDefaultValue : Boolean) : Variant;
var
	setting : ISettingVariant;
	keyName : string;
begin
	if _bDefaultValue then begin
		keyName := GetDefaultDictKeyName(_name);
	end else begin
		keyName := GetDictKeyName(_name);
	end;
	if self.TryGetValue(keyName, setting) then begin
		if (not setting.IsEmpty) then begin
			Result := setting.Value;
		end;
	end else begin
		var
		dbgMsg := TDebugMsgBeginEnd.New('TSettingsDictionary.InnerGetSetting');
		dbgMsg.Msg(keyName + ' not found!');
	end;
end;

procedure TSettingsDictionary.SetSettingValue(_sKey : string; const _v : Variant);
begin
	AddOrSet(_sKey, _v);
end;

procedure TSettingsDictionary.StoreDefaultSetting(const _name : string; const _v : Variant);
begin
	TDebugUtils.Msg('TSettingsDictionary.StoreToDictDefaultSetting: ' + _name + '=' + VarToStr(_v) + ' StoreToDict in memory...');
	AddOrSet(GetDefaultDictKeyName(_name), _v);
	AddOrSetDefault(_name, _v, True);
end;

procedure TSettingsDictionary.StoreSetting(const _name : string; const _v : Variant; const _bAsDefault : Boolean = False);
begin
	TDebugUtils.DebugMessageFormat('TSettingsDictionary.StoreSetting: in memory %s=%s as default=%s',
		[_name, VarToStr(_v), BoolToStr(_bAsDefault, True)]);
	if _bAsDefault then begin
		StoreDefaultSetting(_name, _v);
	end else begin
		AddOrSet(_name, _v);
	end;
end;

end.
