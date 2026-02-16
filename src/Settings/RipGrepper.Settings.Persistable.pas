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
	RipGrepper.Settings.FilePersister,
	RipGrepper.Common.Interfaces.StreamPersistable,
	System.Classes,
	RipGrepper.Settings.Persister.Interfaces;

type

	EWriteSettingsMode = (wsmActual, wsmDefault, wsmAll);

	IPersistable = interface(IInterface)
		['{CAA3F298-BEFD-4419-A6DD-385D7A2660F8}']
		procedure Init();
		procedure ReadFile();
		procedure LoadFromDict();
		procedure ReLoad;
		procedure ReLoadFromFile();
		procedure StoreToPersister();
	end;

	IIniPersistable = interface(IPersistable)
		['{A841C46D-56AF-4391-AB88-4C9496589FF4}']
		function GetIniSectionName() : string;
	end;

	IPersistableArray = interface(IPersistable)
		['{8FFC3DE1-C8DC-43F4-AEB6-EC9879B14F9D}']
		procedure Copy(const _other : IPersistableArray);
		function GetArraySetting() : IArraySetting;
		function GetItem(index : Integer) : string;
		procedure SetArraySetting(const Value : IArraySetting);
		procedure SetItem(index : Integer; const Value : string);
		property ArraySetting : IArraySetting read GetArraySetting write SetArraySetting;
		property Item[index : Integer] : string read GetItem write SetItem;
	end;

	ISettings = interface(IInterface)
		['{372EC376-745A-459B-B946-A90EFA0A3FDE}']
	end;

	TPersistableSettings = class( { TInterfacedObject } TNoRefCountObject, ISettings, IPersistable, IIniPersistable,
		IStreamReaderWriterPersistable)
		const
			STREAM_FORMAT_VERSION = 2;

		private
			class var FLockObject : IShared<TObject>;

		var
			FPersisterFactory : IPersisterFactory;
			FbDefaultLoaded : Boolean;
			FIniSectionName : string;
			FIsAlreadyRead : Boolean;
			FOwner : TPersistableSettings;
			FIsOwnerOfPersisterFactory : Boolean;
			FStreamFormatVersion : Integer;
			procedure CopySettingsDictSectionSettingValues(const _section : string; _sdFrom : ISettingKeys;
				const _bForceCopySettingObj : Boolean = False);
			function GetCount() : Integer;
			function GetPersisterFactory() : IPersisterFactory;
			procedure ReadSettings;
			procedure SetChildrenPersister;
			procedure SetPersisterFactory(const Value : IPersisterFactory);
			procedure SetIniSectionName(const Value : string);
			procedure ClearSection(const _section : string);
			function CopySettingsDictToRoot() : TPersistableSettings;
			procedure StoreDictToPersister(const _section : string = ''; const _bClearSection : Boolean = False);

		protected
			FSettingsDict : IShared<TSettingsDictionary>;
			FChildren : TArrayEx<TPersistableSettings>;
			FIsModified : Boolean;
			FManagedByInterface : Boolean;

			procedure CreateSetting(_setting : ISetting); overload;
			procedure CreateSetting(const _section, _key : string; _setting : ISetting); overload;
			function GetIsAlreadyRead : Boolean; virtual;
			function GetIsModified : Boolean; virtual;
			function GetStreamFormatVersion : Integer; virtual;
			procedure LoadVersionDependentSettings(_sr : TStreamReader); virtual;
			procedure SaveVersionDependentSettings(_sw : TStreamWriter); virtual;
			property StreamFormatVersion : Integer read GetStreamFormatVersion;
			/// <summary>TPersistableSettings.Init
			/// CreateSetting should be called here
			/// </summary>
			procedure Init; virtual; abstract;
			function GetIniSectionName : string; virtual;
			///
			/// Update internal classes from Settings
			procedure UpdateSettingsFromInternals(); virtual;
			procedure UpdateInternalsFromSettings(); virtual;
			function GetRootOwner() : TPersistableSettings;
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
			function AddChildSettings(const _settings : TPersistableSettings) : TPersistableSettings;
			procedure AddToOwnerSettings();
			class procedure CallUpdateFileOnFactory(const _factory : IPersisterFactory; const _dict : TSettingsDictionary);
			function RemoveChildSettings(const _settings : TPersistableSettings) : Boolean;
			procedure CopySettingsDictSection(const _from : TPersistableSettings; const _copyAllSections : Boolean = False;
				const _bForceCopySettingObj : Boolean = False); overload;
			class function GetLockObject : TObject;
			/// <summary>TPersistableSettings.ReadFile
			/// Members.RedIni- should be called here
			/// </summary>
			procedure ReadFile(); virtual;
			/// <summary>
			/// TODO: Rename to ConvertSettingValues
			/// </summary>
			procedure LoadFromDict(); virtual;
			/// ReLoads memini file content
			procedure ReLoadFromFile();
			/// <summary>
			/// Members.StoreToPersister should be called here
			/// Writes to ini.
			/// </summary>
			procedure StoreToPersister; virtual;
			// <summary>
			// Thread safe write Settings to file
			// </summary>
			procedure UpdateFile(const _bForceStoreToPersister : Boolean = False; const _bClearSection : Boolean = False);

			procedure SaveToStreamWriter(_sw : TStreamWriter);
			procedure LoadFromStreamReader(_sr : TStreamReader);
	end;

implementation

uses

	RipGrepper.Tools.DebugUtils,
	System.Variants,
	RipGrepper.Common.Constants,
	Vcl.Forms,
	{$IFNDEF STANDALONE} RipGrepper.Common.IOTAUtils, {$ENDIF}
	System.IOUtils,
	System.StrUtils,
	RipGrepper.Tools.LockGuard,
	Spring.Collections,
	RipGrepper.Helper.StreamReaderWriter;

constructor TPersistableSettings.Create(const _Owner : TPersistableSettings);
begin
	inherited Create();
	FStreamFormatVersion := STREAM_FORMAT_VERSION;
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

	FStreamFormatVersion := STREAM_FORMAT_VERSION;
	FIsModified := False;
	FIsAlreadyRead := False;
	FbDefaultLoaded := False;
	FManagedByInterface := False;
	if not Assigned(FPersisterFactory) then begin
		FPersisterFactory := TIniPersister.Create();
		FIsOwnerOfPersisterFactory := True;
	end;

	FSettingsDict := Shared.Make<TSettingsDictionary>(TSettingsDictionary.Create(IniSectionName, FPersisterFactory));
	dbgMsg.MsgFmt('Create FSettingsDict %p for section: %s', [Pointer(FSettingsDict), IniSectionName]);

	Init();
end;

destructor TPersistableSettings.Destroy;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TPersistableSettings.Destroy', True);
	dbgMsg.MsgFmt('Destroying settings for section: %s', [IniSectionName]);

	for var childSetting : TPersistableSettings in FChildren do begin
		if Supports(childSetting, IPersistableArray) then begin
			// do wee need free something?
			dbgMsg.MsgFmt('Section: %s free is not necessary', [IniSectionName]);
		end else begin
			childSetting.Free;
			dbgMsg.MsgFmt('Section: %s free', [IniSectionName]);
		end;
	end;

	// FSettingsDict will be automatically cleaned up by IShared when reference count reaches zero
	// No manual clearing needed

	inherited Destroy();
end;

function TPersistableSettings.AddChildSettings(const _settings : TPersistableSettings) : TPersistableSettings;
begin
	FChildren.Add(_settings);
	_settings.FOwner := self;
	_settings.AddToOwnerSettings();
	Result := _settings;
end;

function TPersistableSettings.RemoveChildSettings(const _settings : TPersistableSettings) : Boolean;
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

procedure TPersistableSettings.CopySettingsDictSection(const _from : TPersistableSettings; const _copyAllSections : Boolean = False;
	const _bForceCopySettingObj : Boolean = False);
var
	sdFrom : ISettingKeys;
	section : string;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TPersistableSettings.CopySettingsDictSection');
	dbgMsg.MsgFmt('Copy TO [%s]', [IniSectionName]);

	if not _from.SettingsDict.InnerDictionary.TryGetValue(IniSectionName, sdFrom)
	{ } and not _copyAllSections then begin
		dbgMsg.MsgFmt('There is no [%s] in source', [IniSectionName]);
		Exit;
	end;

	if _copyAllSections then begin
		for var sd in _from.SettingsDict() do begin
			section := sd.Key;
			if SettingsDict.ContainsSection(section) then begin
				CopySettingsDictSectionSettingValues(section, sd.Value, _bForceCopySettingObj);
			end else begin
				SettingsDict[section] := sd.Value;
			end;
		end;
	end else begin
		CopySettingsDictSectionSettingValues(IniSectionName, sdFrom, _bForceCopySettingObj);
	end;
end;

procedure TPersistableSettings.CopySettingsDictSectionSettingValues(const _section : string; _sdFrom : ISettingKeys;
	const _bForceCopySettingObj : Boolean = False);
var
	key : string;
	settingOther : ISetting;
	settingSelf : ISetting;
	sdSelf : ISettingKeys;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TPersistableSettings.CopySettingsDictSectionSettingValues');
	for key in _sdFrom.Keys do begin
		settingOther := _sdFrom[key];
		sdSelf := SettingsDict[_section];

		if not sdSelf.TryGetValue(key, settingSelf) then begin
			dbgMsg.MsgFmt('Copy non existing: [%s]%s=%s', [_section, key, settingOther.AsString], ETraceFilterType.tftVerbose);
			sdSelf[key] := settingOther;
			continue;
		end else begin
			if (_bForceCopySettingObj) then begin
				dbgMsg.MsgFmt('Force copy: [%s] %s=%s', [_section, key, settingOther.AsString], ETraceFilterType.tftVerbose);
				sdSelf[key] := settingOther;
				continue;
			end;
		end;
		// settingSelf.Copy(settingOther); doesn't work here
		TSetting.CopySettingFields(settingOther, settingSelf);
	end;
end;

procedure TPersistableSettings.CreateSetting(_setting : ISetting);
begin
	SettingsDict.CreateSetting(_setting.Name, _setting, PersisterFactory);
end;

function TPersistableSettings.GetCount() : Integer;
begin
	Result := 0;
	var
	rootOwner := GetRootOwner();
	if rootOwner.SettingsDict.ContainsSection(IniSectionName) then begin
		Result := rootOwner.SettingsDict()[IniSectionName].Count;
	end;
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
	FIsModified := False;
	if FSettingsDict.ContainsSection(IniSectionName) then begin
		FIsModified := not FSettingsDict.InnerDictionary[IniSectionName].Any(
			function(const p : TPair<string, ISetting>) : Boolean
			begin
				Result := ssModified = p.Value.State;
			end);
	end;
	Result := FIsModified;
end;

function TPersistableSettings.GetStreamFormatVersion : Integer;
begin
	Result := FStreamFormatVersion;
end;

procedure TPersistableSettings.LoadVersionDependentSettings(_sr : TStreamReader);
begin
	// Override in derived classes to read version-specific fields
end;

procedure TPersistableSettings.SaveVersionDependentSettings(_sw : TStreamWriter);
begin
	// Override in derived classes to write version-specific fields
end;

procedure TPersistableSettings.ReadFile();
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TPersistableSettings.ReadFile');
	dbgMsg.MsgFmt('Section: %s', [IniSectionName]);
	for var s in FChildren do begin
		s.ReadFile();
	end;

	if not IsAlreadyRead then begin
		ReadSettings();
	end else begin
		dbgMsg.Msg('IsAlreadyRead');
	end;
end;

procedure TPersistableSettings.StoreToPersister; // new name StoreInPersister
begin
	StoreDictToPersister();
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

	var
	dbgArr := TSettingsDictionary.DictToStringArray(SettingsDict());

	SettingsDict.LoadFromPersister();
	FIsAlreadyRead := True;
end;

procedure TPersistableSettings.ReLoadFromFile();
var
	fh : IFileHandler;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TPersistableSettings.ReLoadFromFile');
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
	ReadFile;
end;

procedure TPersistableSettings.SetChildrenPersister;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TPersistableSettings.SetChildrenPersister');

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
	SetChildrenPersister();
end;

procedure TPersistableSettings.SetIniSectionName(const Value : string);
begin
	FIniSectionName := Value;
end;

procedure TPersistableSettings.AddToOwnerSettings();
begin
	CopySettingsDictToRoot();
end;

class procedure TPersistableSettings.CallUpdateFileOnFactory(const _factory : IPersisterFactory; const _dict : TSettingsDictionary);
var
	fh : IFileHandler;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TPersistableSettings.CallUpdateFileOnFactory');
	if Supports(_factory, IFileHandler, fh) then begin
		if _dict.HasState(ssStored) then begin
			var
			lock := TLockGuard.NewLock(GetLockObject());
			dbgMsg.Msg('Lock Entered to UpdateFile');
			var
			dbgArr := TSettingsDictionary.DictToStringArray(_dict);
			fh.UpdateFile();
			_dict.SetState(ssStored, ssSaved);
			dbgMsg.Msg('Lock Released after UpdateFile');
		end;
	end;
end;

procedure TPersistableSettings.ClearSection(const _section : string);
var
	fh : IFileHandler;
begin
	if Supports(FPersisterFactory, IFileHandler, fh) then begin
		fh.EraseSection(_section);
		// SettingsDict.ClearSection(_section);
	end;
end;

function TPersistableSettings.GetRootOwner() : TPersistableSettings;
var
	rootOwner : TPersistableSettings;
begin
	rootOwner := FOwner;
	while Assigned(rootOwner) do begin
		if not Assigned(rootOwner.FOwner) then
			break;
		rootOwner := rootOwner.FOwner;
	end;
	Result := rootOwner;
end;

function TPersistableSettings.CopySettingsDictToRoot() : TPersistableSettings;
var
	childSetting : TPersistableSettings;
	rootOwner : TPersistableSettings;
	section : string;
begin
	rootOwner := FOwner;
	childSetting := self;
	section := IniSectionName;
	while Assigned(rootOwner) do begin
		// var
		// dbgArrChild := TSettingsDictionary.DictToStringArray(childSetting.SettingsDict());

		rootOwner.CopySettingsDictSection(childSetting, True, True);
		// var
		// dbgArrRoot := TSettingsDictionary.DictToStringArray(rootOwner.SettingsDict());

		rootOwner.StoreDictToPersister(section);
		if not Assigned(rootOwner.FOwner) then
			break;
		rootOwner := rootOwner.FOwner;
		childSetting := rootOwner;
	end;
	Result := rootOwner;
end;

procedure TPersistableSettings.CreateSetting(const _section, _key : string; _setting : ISetting);
begin
	SettingsDict.CreateSetting(_section, _key, _setting, PersisterFactory);
end;

class function TPersistableSettings.GetLockObject : TObject;
begin
	if not Assigned(FLockObject) then begin
		FLockObject := Shared.Make<TObject>();
	end;
	Result := FLockObject;
end;

procedure TPersistableSettings.LoadFromDict();
begin
	// overwrite this to convert setting values to other types
	UpdateInternalsFromSettings;
end;

procedure TPersistableSettings.LoadFromStreamReader(_sr : TStreamReader);
begin
	var
	versionStr := _sr.ReadLineAsString(true, 'Version');
	if not versionStr.IsEmpty then begin
		FStreamFormatVersion := StrToIntDef(versionStr, STREAM_FORMAT_VERSION);
	end;

	SettingsDict.LoadFromStreamReader(_sr);
	LoadVersionDependentSettings(_sr);
	LoadFromDict();
end;

procedure TPersistableSettings.SaveToStreamWriter(_sw : TStreamWriter);
begin
	_sw.WriteLineAsString(StreamFormatVersion.ToString, true, 'Version');
	SettingsDict.SaveToStreamWriter(_sw);
	SaveVersionDependentSettings(_sw);
end;

function TPersistableSettings.ToLogString : string;
begin
	Result := PersisterFactory.ToLogString();
end;

procedure TPersistableSettings.UpdateFile(const _bForceStoreToPersister : Boolean = False; const _bClearSection : Boolean = False);
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TPersistableSettings.UpdateFile');

	if Assigned(FOwner) and not _bForceStoreToPersister then begin
		Exit;
	end;

	if _bForceStoreToPersister then begin
		StoreDictToPersister('', _bClearSection);
	end;

	if Assigned(PersisterFactory) then begin
		TPersistableSettings.CallUpdateFileOnFactory(FPersisterFactory, SettingsDict);
	end else begin
		dbgMsg.ErrorMsg('PersisterFactory not assigned!' + GetIniSectionName());
	end;

end;

procedure TPersistableSettings.StoreDictToPersister(const _section : string = ''; const _bClearSection : Boolean = False);
var
	section : string;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TPersistableSettings.StoreDictToPersister');

	UpdateSettingsFromInternals();

	var
	lock := TLockGuard.NewLock(GetLockObject());
	section := IfThen(_section.IsEmpty, IniSectionName, _section);
	dbgMsg.MsgFmt('Lock Entered - StoreDictToPersister [%s]', [section]);

	if _bClearSection then begin
		dbgMsg.MsgFmt('Clear section [%s]', [section]);
		ClearSection(section);
	end;

	SettingsDict.StoreToPersister(section);
end;

procedure TPersistableSettings.UpdateInternalsFromSettings();
begin
	// interne class update should happened here
	var
	dbgMsg := TDebugMsgBeginEnd.New('TPersistableSettings.UpdateInternalsFromSettings');

end;

procedure TPersistableSettings.UpdateSettingsFromInternals();
begin
	// settings update from intenal classe should go here
	var
	dbgMsg := TDebugMsgBeginEnd.New('TPersistableSettings.UpdateSettingsFromInternals');

end;

initialization

finalization

// MemLeak in unittests: TSingleton and class var FLockObject not working properly
// TPersistableSettings.FLockObject := nil;

end.
