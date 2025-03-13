unit RipGrepper.Settings.Persistable;

interface

uses
	System.Generics.Defaults,
	System.IniFiles,
	RipGrepper.Helper.MemIniFile,
	System.SysUtils,
	RipGrepper.Common.SimpleTypes,
	RipGrepper.Settings.SettingVariant,
	RipGrepper.Settings.SettingsDictionary,
	System.SyncObjs,
	ArrayEx,
	Spring,
	RipGrepper.Settings.FilePersister;

type

	EWriteSettingsMode = (wsmActual, wsmDefault, wsmAll);

	IIniPersistable = interface
		['{A841C46D-56AF-4391-AB88-4C9496589FF4}']
		function GetIniSectionName() : string;
		procedure Init;
		procedure ReadIni;
		procedure LoadFromDict();
		procedure StoreToDict;
	end;

	TPersistableSettings = class(TNoRefCountObject, IIniPersistable)
		strict private
			class constructor Create;
			class destructor Destroy;

		private
			FPersisterFactory : IPersisterFactory;
			FbDefaultLoaded : Boolean;
			FIniSectionName : string;
			FIsAlreadyRead : Boolean;
			FOwner : TPersistableSettings;
			FIsOwnerOfPersisterFactory : Boolean;
			procedure CopySettingsDictSection(const _section : string; _sdFrom : ISettingKeys); overload;
			procedure FreeOwnIniFile;
			function GetCount() : Integer;
			function GetPersisterFactory() : IPersisterFactory;
			procedure ReadSettings;
			procedure SetChildrenIniFiles;
			procedure SetPersisterFactory(const Value : IPersisterFactory);
			procedure SetIniSectionName(const Value : string);
			procedure SetOwnerSettings(const _section : string = ''; const _bForceWriteIni : Boolean = False;
				const _bClearSection : Boolean = False);

		protected
			FSettingsDict : IShared<TSettingsDictionary>;
			FChildren : TArrayEx<TPersistableSettings>;
			FIsModified : Boolean;
			class var FLockObject : TObject;

			procedure CreateSetting(const _key : string; _setting : ISetting);
			function GetIsAlreadyRead : Boolean; virtual;
			function GetIsModified : Boolean; virtual;
			/// <summary>TPersistableSettings.Init
			/// CreateSetting and CreateDefaultRelevantSetting should be called here
			/// </summary>
			procedure Init; virtual; abstract;
			function GetIniSectionName : string; virtual;
			function ToLogString : string; virtual;

		public
			constructor Create(const _Owner : TPersistableSettings); overload;
			constructor Create; overload;
			procedure Copy(const _other : TPersistableSettings); virtual;
			procedure ReLoad; virtual;

			property Count : Integer read GetCount;
			property PersisterFactory : IPersisterFactory read GetPersisterFactory write SetPersisterFactory;
			property IniSectionName : string read GetIniSectionName write SetIniSectionName;
			property IsAlreadyRead : Boolean read GetIsAlreadyRead;
			property IsModified : Boolean read GetIsModified;
			property IsOwnerOfPersisterFactory : Boolean read FIsOwnerOfPersisterFactory write FIsOwnerOfPersisterFactory;

			property SettingsDict : IShared<TSettingsDictionary> read FSettingsDict write FSettingsDict;
			destructor Destroy; override;
			function AddChildSettings(_settings : TPersistableSettings) : TPersistableSettings;
			function RemoveChildSettings(_settings : TPersistableSettings) : Boolean;
			procedure CopySettingsDictSection(const _from : TPersistableSettings; const _copyAllSections : Boolean = False); overload;
			/// <summary>TPersistableSettings.ReadIni
			/// Members.RedIni- should be called here
			/// </summary>
			procedure ReadIni; virtual;
			/// <summary>TPersistableSettings.LoadFromDict
			/// Refresh member variables by read settings value or default value
			/// </summary>
			procedure LoadFromDict(); virtual; abstract;
			/// ReLoads memini file content
			procedure ReLoadFromDisk;
			/// <summary>
			/// Members.StoreToDict should be called here
			/// Writes to ini.
			/// </summary>
			procedure StoreToDict; virtual;
			// <summary>
			// Thread safe write Settings to ini file
			// </summary>
			procedure UpdateIniFile(const _section : string = ''; const _bForceWriteIni : Boolean = False;
				const _bClearSection : Boolean = False);
			procedure WriteSettingsDictToIni(const _section : string = ''; const _bClearSection : Boolean = False);
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
	RipGrepper.Tools.LockGuard,
	Spring.Collections;

constructor TPersistableSettings.Create(const _Owner : TPersistableSettings);
begin
	inherited Create();
	FOwner := _Owner;
	if Assigned(FOwner) then begin
		FPersisterFactory := _Owner.PersisterFactory;
	end;
	FIsOwnerOfPersisterFactory := False;
	Create();
end;

constructor TPersistableSettings.Create;
begin
	inherited;
	var
	dbgMsg := TDebugMsgBeginEnd.New('TPersistableSettings.Create', True);

	FIsModified := False;
	FIsAlreadyRead := False;
	FSettingsDict := Shared.Make<TSettingsDictionary>(TSettingsDictionary.Create(IniSectionName));
	dbgMsg.MsgFmt('Create FSettingsDict %p for section: %s', [Pointer(FSettingsDict), IniSectionName]);
	FbDefaultLoaded := False;
	if not Assigned(FPersisterFactory) then begin
		FPersisterFactory := TIniPersister.Create();
		FIsOwnerOfPersisterFactory := True;
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
	// FreeOwnIniFile;
	dbgMsg.MsgFmt('Free FSettingsDict %p for section: %s', [Pointer(FSettingsDict()), IniSectionName]);
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
	_settings.SetOwnerSettings();
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
		FIsModified := _other.IsModified;
		FIsAlreadyRead := _other.IsAlreadyRead;
		CopySettingsDictSection(_other);
	end;
end;

procedure TPersistableSettings.CopySettingsDictSection(const _from : TPersistableSettings; const _copyAllSections : Boolean = False);
var
	sdFrom : ISettingKeys;
	section : string;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TPersistableSettings.CopySettingsDictSection');
	dbgMsg.MsgFmt('Copy TO [%s]', [IniSectionName]);

	if not _from.SettingsDict.InnerDictionary.TryGetValue(IniSectionName, sdFrom) and not _copyAllSections then begin
		dbgMsg.MsgFmt('There is no [%s] in source', [IniSectionName]);
		Exit;
	end;

	if _copyAllSections then begin
		for var sd in _from.SettingsDict() do begin
			section := sd.Key;
			if SettingsDict.ContainsSection(section) then begin
				CopySettingsDictSection(section, sd.Value);
			end else begin
				SettingsDict[section] := sd.Value;
			end;
		end;
	end else begin
		CopySettingsDictSection(IniSectionName, sdFrom);
	end;

end;

procedure TPersistableSettings.CopySettingsDictSection(const _section : string; _sdFrom : ISettingKeys);
var
	settingOther : ISetting;
	settingSelf : ISetting;
	sdSelf : ISettingKeys;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TPersistableSettings.CopySettingsDictSection');

	for var key in _sdFrom.Keys do begin
		settingOther := _sdFrom[key];
		sdSelf := SettingsDict[_section];

		if not sdSelf.TryGetValue(key, settingSelf) then begin
			dbgMsg.MsgFmt('There is no [%s] %s in destination', [_section, key]);
			sdSelf[key] := settingOther;
			continue;
		end;

		case settingOther.SettingType of
			stString :
			TStringSetting(settingSelf).Copy(settingOther.AsStringSetting);
			stInteger :
			TIntegerSetting(settingSelf).Copy(settingOther.AsIntegerSetting);
			stBool :
			TBoolSetting(settingSelf).Copy(settingOther.AsBoolSetting);
			stStrArray :
			TArraySetting(settingSelf).Copy(settingOther.AsArraySetting);
			else
			raise ESettingsException.Create('Can''t copy unknown setting type');
		end;
	end;
end;

procedure TPersistableSettings.CreateSetting(const _key : string; _setting : ISetting);
begin
	SettingsDict.CreateSetting(_key, _setting, PersisterFactory);
end;

procedure TPersistableSettings.FreeOwnIniFile;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TPersistableSettings.FreeOwnIniFile', True);

	if FIsOwnerOfPersisterFactory then begin
		if Assigned(FPersisterFactory) then begin
			dbgMsg.MsgFmt('Free FPersisterFactory %p of section: %s', [Pointer(PersisterFactory), GetIniSectionName()]);
			FPersisterFactory := nil;
		end;
	end;
end;

function TPersistableSettings.GetCount() : Integer;
begin
	Result := SettingsDict.Count;
end;

function TPersistableSettings.GetPersisterFactory() : IPersisterFactory;
begin
	Result := FPersisterFactory;
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
begin
	FIsModified := not FSettingsDict.InnerDictionary[IniSectionName].Where(
		function(const p : TPair<string, ISetting>) : Boolean
		begin
			Result := ssModified = p.Value.State;
		end).IsEmpty;

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

procedure TPersistableSettings.StoreToDict;
begin
	for var s in FChildren do begin
		s.StoreToDict;
	end;
	WriteSettingsDictToIni();
end;

procedure TPersistableSettings.ReadSettings;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TPersistableSettings.ReadSettings');

	if FIsAlreadyRead or (IniSectionName = ROOT_DUMMY_INI_SECTION) then begin
		dbgMsg.MsgFmt('FIsAlreadyRead %s Section: %s', [BoolToStr(FIsAlreadyRead, True), IniSectionName]);
		Exit;
	end;

	dbgMsg.MsgFmt('Read section %s from PersisterFactory %p', [IniSectionName, Pointer(PersisterFactory)]);
	SettingsDict.LoadFromFile();
	FIsAlreadyRead := True;
end;

procedure TPersistableSettings.ReLoadFromDisk;
var
	fh : IFileHandler;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TPersistableSettings.ReLoadFromDisk');
	if Supports(FPersisterFactory, IFileHandler, fh) then begin
		fh.ReLoadFile();
	end;
	ReLoad;
end;

procedure TPersistableSettings.ReLoad;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TPersistableSettings.ReLoad');

	for var s in FChildren do begin
		dbgMsg.MsgFmt('ReLoad from PersisterFactory %p for section %s', [Pointer(s.PersisterFactory), s.IniSectionName]);
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
		dbgMsg.MsgFmt('Change PersisterFactory %p to %p for %s', [Pointer(s.PersisterFactory), Pointer(PersisterFactory),
			s.IniSectionName]);
		s.PersisterFactory := PersisterFactory;
	end;
end;

procedure TPersistableSettings.SetPersisterFactory(const Value : IPersisterFactory);
begin
	FPersisterFactory := Value;
	if not FIsOwnerOfPersisterFactory and Assigned(FOwner) and (FOwner.PersisterFactory <> PersisterFactory) then begin
		FOwner.PersisterFactory := PersisterFactory;
	end;
	SetChildrenIniFiles();
end;

procedure TPersistableSettings.SetIniSectionName(const Value : string);
begin
	FIniSectionName := Value;
end;

procedure TPersistableSettings.SetOwnerSettings(const _section : string = ''; const _bForceWriteIni : Boolean = False;
const _bClearSection : Boolean = False);
var
	dbgArr : TArray<TArray<string>>;
begin
	if Assigned(FOwner) { and (_section = '') } then begin
		FOwner.CopySettingsDictSection(self, True);
		dbgArr := TSettingsDictionary.DictToStringArray(SettingsDict);
		FOwner.WriteSettingsDictToIni(IfThen(_bForceWriteIni, _section), _bClearSection);
	end;
end;

function TPersistableSettings.ToLogString : string;
begin
	Result := PersisterFactory.ToLogString();
end;

procedure TPersistableSettings.UpdateIniFile(const _section : string = ''; const _bForceWriteIni : Boolean = False;
const _bClearSection : Boolean = False);
var
	fh : IFileHandler;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TPersistableSettings.UpdateIniFile');

	for var s in FChildren do begin
		dbgMsg.MsgFmt('Child update begin on section: [%s]', [s.GetIniSectionName()]);
		s.UpdateIniFile(s.GetIniSectionName());
	end;

	SetOwnerSettings(_section, _bForceWriteIni, _bClearSection);

	if Assigned(FOwner) { and (_section = '') } and not _bForceWriteIni then begin
		Exit;
	end;

	if _bForceWriteIni then begin
		WriteSettingsDictToIni(IfThen(_bForceWriteIni, _section), _bClearSection)
	end;

	// var arr := DictToLog(SettingsDict);
	if Assigned(PersisterFactory) then begin
		var
		lock := TLockGuard.NewLock(FLockObject);
		dbgMsg.Msg('Lock Entered to UpdateIniFile');
		try
			var
			sectionName := IfThen((_section = ''), IniSectionName, _section);
			dbgMsg.MsgFmt('PersisterFactory %p update begin on [%s]', [Pointer(PersisterFactory), sectionName]);

			if Supports(FPersisterFactory, IFileHandler, fh) then begin
				fh.WriteFile();
			end;

			// dbgMsg.Msg('[SearchTextsHistory] Item 0:' + PersisterFactory.ReadString('SearchTextsHistory', 'Item_0', 'not exists'));
		except
			on E : Exception do begin
				dbgMsg.ErrorMsgFmt('%s' + CRLF + '%s', [E.Message, E.StackTrace]);
				raise;
			end;
		end;
		dbgMsg.Msg('Lock Released');
	end else begin
		dbgMsg.ErrorMsg('PersisterFactory not assigned!' + GetIniSectionName());
	end;

end;

procedure TPersistableSettings.WriteSettingsDictToIni(const _section : string = ''; const _bClearSection : Boolean = False);
var
	fh : IFileHandler;
	section : string;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TPersistableSettings.WriteSettingsDictToIni');

	var
	lock := TLockGuard.NewLock(FLockObject);

	section := IfThen(_section = '', IniSectionName, _section);
	dbgMsg.MsgFmt('Lock Entered - WriteSettingsDictToIni [%s]', [section]);

	if _bClearSection then begin
		dbgMsg.MsgFmt('Clear section [%s]', [section]);

		if Supports(FPersisterFactory, IFileHandler, fh) then begin
			fh.EraseSection(section);
		end
	end;

	SettingsDict.SaveToFile(section);
end;

end.
