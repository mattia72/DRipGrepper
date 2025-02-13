unit RipGrepper.Settings.Persistable;

interface

uses
	System.Generics.Defaults,
	System.IniFiles,
	RipGrepper.Helper.MemIniFile,
	System.Generics.Collections,
	System.SysUtils,
	RipGrepper.Common.SimpleTypes,
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

	IFilePersister = interface(IInterface)
		['{57B16806-F8F5-447E-9AB6-767E553CCB65}']
		procedure LoadFromFile(_dict : TSettingsDictionary);
		procedure ReloadFile(_dict : TSettingsDictionary);
		procedure SaveToFile(_dict : TSettingsDictionary);
	end;

	TMemIniPersister = class(TInterfacedObject, IFilePersister)
		procedure LoadFromFile(_dict : TSettingsDictionary);
		procedure ReloadFile(_dict : TSettingsDictionary);
		procedure SaveToFile(_dict : TSettingsDictionary);

		private
			FIniFile : TMemIniFile;

		public
			constructor Create;
			destructor Destroy; override;
	end;

	EWriteSettingsMode = (wsmActual, wsmDefault, wsmAll);

	TPersistableSettings = class(TNoRefCountObject, IIniPersistable)
		strict private
			class constructor Create;
			class destructor Destroy;

		private
			FIniFile : TMemIniFile; // TODO IFilePersister
			FbDefaultLoaded : Boolean;
			FIniSectionName : string;
			FIsAlreadyRead : Boolean;
			FOwner : TPersistableSettings;
			FOwnIniFile : Boolean;
			procedure CreateIniFile;
			procedure DictToLog(_dict : TSettingsDictionary);
			procedure FreeOwnIniFile;
			function GetIniFile : TMemIniFile;
			procedure ReadSettings;
			procedure SetChildrenIniFiles;
			procedure SetIniFile(const Value : TMemIniFile);
			procedure SetIniSectionName(const Value : string);
			// 1 Should be locked by a guard
			procedure WriteToIni(const _sIniSection, _sKey : string; _setting : ISettingVariant);

		protected
			FSettingsDict : TSettingsDictionary;
			FChildren : TArrayEx<TPersistableSettings>;
			FIsModified : Boolean;
			class var FLockObject : TObject;

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
			property OwnIniFile : Boolean read FOwnIniFile write FOwnIniFile;
			property SettingsDict : TSettingsDictionary read FSettingsDict write FSettingsDict;
			destructor Destroy; override;
			function AddChildSettings(_settings : TPersistableSettings) : TPersistableSettings;
			function RemoveChildSettings(_settings : TPersistableSettings) : Boolean;
			procedure CopyDefaultsToValues; virtual;
			procedure CopySettingsDictSection(const _other : TPersistableSettings; const
				_copyAllSections : Boolean = False);
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

			/// ReLoads memini file content
			procedure ReLoadFromDisk;
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
			procedure UpdateIniFile(const _section : string = '');
			procedure WriteSettingsDictToIni(const _wsm : EWriteSettingsMode; const _section : string = '');
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
	if Assigned(FOwner) then begin
		FIniFile := _Owner.IniFile;
	end;
	FOwnIniFile := False;
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
		FOwnIniFile := True;
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
	_settings.FOwner := self;
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

		CopySettingsDictSection(_other);

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

procedure TPersistableSettings.CopySettingsDictSection(const _other :
	TPersistableSettings; const _copyAllSections : Boolean = False);
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TPersistableSettings.CopySettingsDictSection');
	dbgMsg.MsgFmt('Copy TO [%s]', [IniSectionName]);
	for var key in _other.SettingsDict.Keys do begin
		if _copyAllSections or key.StartsWith(IniSectionName) then begin
			dbgMsg.MsgFmt('Copy FROM [%s] %s=%s', [_other.IniSectionName, key, _other.SettingsDict[key].Value]);
			FSettingsDict.AddOrChange(key, _other.SettingsDict[key]);
		end;
	end;
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

procedure TPersistableSettings.DictToLog(_dict : TSettingsDictionary);
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TPersistableSettings.DictToLog');
	{$IFDEF DEBUG}
	var
		a : TArray<TArray<string>>;
	for var p in _dict do begin
		var
			sVal : string := VarToStr(p.Value.Value);
		a := a + [[p.Key, sVal]];
		dbgMsg.MsgFmt('%s=%s', [p.Key, sVal], tftVerbose);
	end;
	{$ENDIF}
end;

procedure TPersistableSettings.FreeOwnIniFile;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TPersistableSettings.FreeOwnIniFile', True);

	if FOwnIniFile then begin
		if Assigned(FIniFile) then begin
			dbgMsg.MsgFmt('Free FIniFile %p of section: %s', [Pointer(IniFile), GetIniSectionName()]);
			FIniFile.Free;
			FIniFile := nil;
		end;
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
	dbgMsg.MsgFmt('Section: %s', [IniSectionName]);
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
	WriteSettingsDictToIni(EWriteSettingsMode.wsmActual);
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
			dbgMsg.MsgFmt('Read section %s from IniFile %p', [IniSectionName, Pointer(IniFile)]);
			IniFile.ReadSectionValues(IniSectionName, strs);
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

procedure TPersistableSettings.ReLoadFromDisk;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TPersistableSettings.ReLoadFromDisk');
	FIniFile.ReLoadIniFile();
	ReLoad;
end;

procedure TPersistableSettings.ReLoad;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TPersistableSettings.ReLoad');

	for var s in FChildren do begin
		dbgMsg.MsgFmt('ReLoad from IniFile %p for section %s', [Pointer(s.IniFile), s.IniSectionName]);
		s.ReLoad;
	end;
	FIsAlreadyRead := False;
	ReadIni;
end;

procedure TPersistableSettings.SetChildrenIniFiles;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TPersistableSettings.SetChildrenIniFiles');

	for var s in FChildren do begin
		dbgMsg.MsgFmt('Change IniFile %p to %p for %s', [Pointer(s.IniFile), Pointer(IniFile), s.IniSectionName]);
		s.IniFile := IniFile;
	end;
end;

procedure TPersistableSettings.SetIniFile(const Value : TMemIniFile);
begin
	FIniFile := Value;
	if not FOwnIniFile and Assigned(FOwner) and (FOwner.IniFile <> IniFile) then begin
		FOwner.IniFile := IniFile;
	end;
	SetChildrenIniFiles();
end;

procedure TPersistableSettings.SetIniSectionName(const Value : string);
begin
	FIniSectionName := Value;
end;

procedure TPersistableSettings.StoreAsDefaultsToDict;
begin
	CopyValuesToDefaults;
	WriteSettingsDictToIni(EWriteSettingsMode.wsmDefault); // Write to mem ini, after UpdateIniFile will be saved
end;

function TPersistableSettings.ToLogString : string;
var
	strs : TStrings;
begin
	strs := TStringList.Create();
	try
		IniFile.GetStrings(strs);
		Result := strs.DelimitedText;
	finally
		strs.Free;
	end
end;

procedure TPersistableSettings.UpdateIniFile(const _section : string = '');
var
	sectionValues : TStringList;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TPersistableSettings.UpdateIniFile');

	for var s in FChildren do begin
		dbgMsg.MsgFmt('Child update begin on section: [%s]', [s.GetIniSectionName()]);
		// s.UpdateIniFile(_section);
		s.UpdateIniFile(s.GetIniSectionName());
	end;

	if Assigned(FOwner) { and (_section = '') } then begin
		FOwner.CopySettingsDictSection(self, True);
		DictToLog(SettingsDict);
		FOwner.WriteSettingsDictToIni(EWriteSettingsMode.wsmAll);
		Exit;
	end;

	DictToLog(SettingsDict);

	if Assigned(IniFile) then begin
		var
		lock := TLockGuard.NewLock(FLockObject);
		dbgMsg.Msg('Lock Entered to UpdateIniFile');
		try
			var
			sectionName := IfThen((_section = ''), GetIniSectionName(), _section);
			dbgMsg.MsgFmt('IniFile %p update begin on [%s]', [Pointer(IniFile), sectionName]);

			if (not sectionName.IsEmpty) and Assigned(FOwner) then begin
				sectionValues := TStringList.Create;
				try
					FSettingsDict.GetSettingsSectionValues(sectionName, sectionValues);
					IniFile.WriteTempSectionIni(sectionName, sectionValues);
				finally
					sectionValues.Free;
				end;
			end else begin // if we don't have Owner, then
				IniFile.ReadTempSectionFiles();
				IniFile.UpdateFile;
			end;

			dbgMsg.Msg('[SearchTextsHistory] Item 0:' + IniFile.ReadString('SearchTextsHistory', 'Item_0', 'not exists'));
		except
			on E : Exception do begin
				dbgMsg.ErrorMsgFmt('%s' + CRLF + '%s', [E.Message, E.StackTrace]);
				raise;
			end;
		end;
		dbgMsg.Msg('Lock Released');
	end else begin
		dbgMsg.ErrorMsg('IniFile not assigned!' + GetIniSectionName());
	end;

end;

procedure TPersistableSettings.WriteSettingsDictToIni(const _wsm : EWriteSettingsMode; const _section : string = '');
var
	setting : ISettingVariant;
	section : string;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TPersistableSettings.WriteSettingsDictToIni');

	section := IfThen(_section = '', IniSectionName, _section);
	var
	lock := TLockGuard.NewLock(FLockObject);
	dbgMsg.MsgFmt('Lock Entered - WriteSettingsDictToIni [%s]', [section]);

	for var key in FSettingsDict.Keys do begin
		if ((not section.IsEmpty) and (not key.StartsWith(section))) then
			continue;
		if _wsm <> EWriteSettingsMode.wsmAll then begin
			if _wsm = EWriteSettingsMode.wsmDefault then begin
				if (not key.EndsWith(DEFAULT_KEY)) then begin
					continue;
				end;
			end else begin
				if (key.EndsWith(DEFAULT_KEY)) then begin
					continue;
				end;
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

// 1 Should be locked by a guard
procedure TPersistableSettings.WriteToIni(const _sIniSection, _sKey : string;
	_setting : ISettingVariant);
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TPersistableSettings.WriteToIni');

	_setting.WriteToMemIni(IniFile, _sIniSection, _sKey);
end;

constructor TMemIniPersister.Create;
begin
	inherited Create;
	{$IFDEF STANDALONE}
	FIniFile := TMemIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'), TEncoding.UTF8);
	{$ELSE}
	FIniFile := TMemIniFile.Create(TPath.Combine(IOTAUTils.GetSettingFilePath, EXTENSION_NAME + '.ini'), TEncoding.UTF8);
	{$ENDIF}
end;

destructor TMemIniPersister.Destroy;
begin
	FIniFile.Free;
	inherited;
end;

procedure TMemIniPersister.LoadFromFile(_dict : TSettingsDictionary);
begin

end;

procedure TMemIniPersister.ReloadFile(_dict : TSettingsDictionary);
begin
	FIniFile.ReLoadIniFile();
end;

procedure TMemIniPersister.SaveToFile(_dict : TSettingsDictionary);
begin

end;

end.
