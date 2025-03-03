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
	Spring;

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
			FIniFile : IShared<TMemIniFile>; // TODO IFilePersister
			FbDefaultLoaded : Boolean;
			FIniSectionName : string;
			FIsAlreadyRead : Boolean;
			FOwner : TPersistableSettings;
			FOwnIniFile : Boolean;
			procedure CreateIniFile;
			function DictToLog(_dict : TSettingsDictionary) : TArray<TArray<string>>;
			procedure FreeOwnIniFile;
			function GetCount(): Integer;
			function GetIniFile : IShared<TMemIniFile>;
			procedure ReadSettings;
			procedure SetChildrenIniFiles;
			procedure SetIniFile(const Value : IShared<TMemIniFile>);
			procedure SetIniSectionName(const Value : string);
			procedure SetOwnerSetings(const _section : string = ''; const _bForceWriteIni : Boolean = False;
				const _bClearSection : Boolean = False);
		protected
			FSettingsDict : IShared<TSettingsDictionary>;
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
			function ToLogString : string; virtual;
		public
			constructor Create(const _Owner : TPersistableSettings); overload;
			constructor Create; overload;
			procedure Copy(const _other : TPersistableSettings); virtual;
			procedure ReLoad; virtual;

			property Count: Integer read GetCount;
			property IniFile : IShared<TMemIniFile> read GetIniFile write SetIniFile;
			property IniSectionName : string read GetIniSectionName write SetIniSectionName;
			property IsAlreadyRead : Boolean read GetIsAlreadyRead;
			property IsModified : Boolean read GetIsModified;
			property OwnIniFile : Boolean read FOwnIniFile write FOwnIniFile;
			property SettingsDict : IShared<TSettingsDictionary> read FSettingsDict write FSettingsDict;
			destructor Destroy; override;
			function AddChildSettings(_settings : TPersistableSettings) : TPersistableSettings;
			function RemoveChildSettings(_settings : TPersistableSettings) : Boolean;
			procedure CopySettingsDictSection(const _other : TPersistableSettings; const _copyAllSections : Boolean = False);
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
	FSettingsDict := Shared.Make<TSettingsDictionary>(TSettingsDictionary.Create(IniSectionName));
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

procedure TPersistableSettings.CopySettingsDictSection(const _other : TPersistableSettings; const _copyAllSections : Boolean = False);
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TPersistableSettings.CopySettingsDictSection');
	dbgMsg.MsgFmt('Copy TO [%s]', [IniSectionName]);
	SettingsDict.CopySection(IniSectionName, _other.SettingsDict());
end;

procedure TPersistableSettings.CreateIniFile;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TPersistableSettings.CreateIniFile', True);
	{$IFDEF STANDALONE}
	FIniFile := Shared.Make<TMemIniFile>(
		{ } TMemIniFile.Create(ChangeFileExt(Application.ExeName, '.ini'), TEncoding.UTF8));
	{$ELSE}
	FIniFile := Shared.Make<TMemIniFile>(
		{ } TMemIniFile.Create(TPath.Combine(IOTAUTils.GetSettingFilePath, EXTENSION_NAME + '.ini'), TEncoding.UTF8));
	{$ENDIF}
	dbgMsg.MsgFmt('Create FIniFile %p of section: %s', [Pointer(FIniFile), GetIniSectionName()]);
end;

function TPersistableSettings.DictToLog(_dict : TSettingsDictionary) : TArray<TArray<string>>;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TPersistableSettings.DictToLog');
	{$IFDEF DEBUG}
	for var p in _dict.InnerDictionary do begin
		var
			sVal : string := TStringSetting(p.Value).Value;
		Result := Result + [[p.Key, sVal]];
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
			// FIniFile.Free;
			FIniFile := nil;
		end;
	end;
end;

function TPersistableSettings.GetCount(): Integer;
begin
	Result := SettingsDict.Count;
end;

function TPersistableSettings.GetIniFile : IShared<TMemIniFile>;
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

	dbgMsg.MsgFmt('Read section %s from IniFile %p', [IniSectionName, Pointer(IniFile)]);
	SettingsDict.LoadFromFile(IniFile());
    FIsAlreadyRead := True;
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

procedure TPersistableSettings.SetIniFile(const Value : IShared<TMemIniFile>);
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

procedure TPersistableSettings.SetOwnerSetings(const _section : string = ''; const _bForceWriteIni : Boolean = False;
const _bClearSection : Boolean = False);
var
	dbgArr : TArray<TArray<string>>;
begin
	if Assigned(FOwner) { and (_section = '') } then begin
		FOwner.CopySettingsDictSection(self, True);
		dbgArr := DictToLog(SettingsDict);
		FOwner.WriteSettingsDictToIni(IfThen(_bForceWriteIni, _section), _bClearSection);
	end;
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

procedure TPersistableSettings.UpdateIniFile(const _section : string = ''; const _bForceWriteIni : Boolean = False;
const _bClearSection : Boolean = False);
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TPersistableSettings.UpdateIniFile');

	for var s in FChildren do begin
		dbgMsg.MsgFmt('Child update begin on section: [%s]', [s.GetIniSectionName()]);
		s.UpdateIniFile(s.GetIniSectionName());
	end;

	SetOwnerSetings(_section, _bForceWriteIni, _bClearSection);

	if Assigned(FOwner) { and (_section = '') } and not _bForceWriteIni then begin
		Exit;
	end;

	if _bForceWriteIni then begin
		WriteSettingsDictToIni(IfThen(_bForceWriteIni, _section), _bClearSection)
	end;

	// var arr := DictToLog(SettingsDict);
	if Assigned(IniFile) then begin
		var
		lock := TLockGuard.NewLock(FLockObject);
		dbgMsg.Msg('Lock Entered to UpdateIniFile');
		try
			var
			sectionName := IfThen((_section = ''), IniSectionName, _section);
			dbgMsg.MsgFmt('IniFile %p update begin on [%s]', [Pointer(IniFile), sectionName]);

			IniFile.UpdateFile;
			// dbgMsg.Msg('[SearchTextsHistory] Item 0:' + IniFile.ReadString('SearchTextsHistory', 'Item_0', 'not exists'));
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

procedure TPersistableSettings.WriteSettingsDictToIni(const _section : string = ''; const _bClearSection : Boolean = False);
var
	section : string;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TPersistableSettings.WriteSettingsDictToIni');

	section := IfThen(_section = '', IniSectionName, _section);
	var
	lock := TLockGuard.NewLock(FLockObject);
	dbgMsg.MsgFmt('Lock Entered - WriteSettingsDictToIni [%s]', [section]);

	if _bClearSection then begin
		dbgMsg.MsgFmt('Clear section [%s]', [section]);
		IniFile.EraseSection(section);
	end;

    SettingsDict.SaveToFile(IniFile());
end;

end.
