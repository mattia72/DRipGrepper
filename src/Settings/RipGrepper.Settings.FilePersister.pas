unit RipGrepper.Settings.FilePersister;

interface

uses
	System.IniFiles,
	System.SyncObjs,
	ArrayEx,
	Spring,
	RipGrepper.Common.Constants,
	RipGrepper.Settings.Persister.Interfaces;

type

	TAutoLock = record
		private
			FCriticalSection : TCriticalSection;

		public
			class function Create(_criticalSection : TCriticalSection) : TAutoLock; static;
			procedure Initialize;
			procedure Finalize;
	end;

	TMemIniPersister = class(TInterfacedObject, IPersister)
		private
			function GetFilePath() : string;
			function GetIniFile() : TMemIniFile;
			procedure SetFilePath(const Value : string);

		protected
			FIniFile : IShared<TMemIniFile>; // Restore the FIniFile field
			FIniKey : string;
			FIniSection : string;
			procedure checkWritenValue(const _expectedValue : string); overload;
			procedure checkWritenValue(const _section, _key, _valueShould : string);
				overload;

		public
			constructor Create(_ini : IShared<TMemIniFile>); overload;
			destructor Destroy(); override;
			procedure EraseKeysInSection(_section: string);
			property FilePath : string read GetFilePath write SetFilePath;
			property IniFile : TMemIniFile read GetIniFile;
	end;

	TMemIniStringPersister = class(TMemIniPersister, IFilePersister<string>)
		protected
		public
			constructor Create(_ini : IShared<TMemIniFile>; const _sIniSection, _sKey : string); overload;
			function TryLoadValue(var _value : string) : Boolean;
			function LoadValue(const _section, _key : string) : string;
			procedure StoreValue(const _value : string);
	end;

	TMemIniIntegerPersister = class(TMemIniPersister, IFilePersister<integer>)
		public
			constructor Create(_ini : IShared<TMemIniFile>; const _sIniSection, _sKey : string);
			function TryLoadValue(var _value : integer) : Boolean;
			function LoadValue(const _section, _key : string) : integer;
			procedure StoreValue(const _value : integer);
	end;

	TMemIniBoolPersister = class(TMemIniPersister, IFilePersister<Boolean>)
		public
			constructor Create(_ini : IShared<TMemIniFile>; const _sIniSection, _sKey : string);
			function TryLoadValue(var _value : Boolean) : Boolean;
			function LoadValue(const _section, _key : string) : Boolean;
			procedure StoreValue(const _value : Boolean);
	end;

	TMemIniStrArrayPersister = class(TMemIniPersister, IFilePersister < TArrayEx < string >> )

		private
			function LoadArrayFromSection(const _section : string; const _keyPrefix : string = ITEM_KEY_PREFIX) : TArrayEx<string>;

		public
			constructor Create(_ini : IShared<TMemIniFile>; const _sIniSection : string; const _sKeyPrefix : string = '');
			function TryLoadValue(var _value : TArrayEx<string>) : Boolean;
			function LoadValue(const _section, _key : string) : TArrayEx<string>;
			procedure StoreValue(const _value : TArrayEx<string>);
	end;

	TIniPersister = class(TInterfacedObject, IPersisterFactory, IFileHandler)
		private
			FIniFile : IShared<TMemIniFile>; // Instance-specific INI file (for tests)
			class var FIniFileSingleton : IShared<TMemIniFile>;
			class var FIniFileLock : TCriticalSection;
			function GetFilePath() : string;
			procedure SetFilePath(const Value : string);
			class function getIniFileSingleton() : IShared<TMemIniFile>;
			function getIniFile() : IShared<TMemIniFile>; // Get instance or singleton INI file

		protected
			procedure checkWritenValue(const _section, _key, _valueShould : string);

		public
			constructor Create(); overload;
			constructor Create(_ini : IShared<TMemIniFile>); overload;

			function GetStringPersister(const _sIniSection : string = ''; const _sKey : string = '') : IFilePersister<string>;
			function GetIntegerPersister(const _sIniSection : string = ''; const _sKey : string = '') : IFilePersister<Integer>;
			function GetBoolPersister(const _sIniSection : string = ''; const _sKey : string = '') : IFilePersister<Boolean>;
			function GetStrArrayPersister(const _sIniSection : string = ''; const _sKey : string = '') : IFilePersister<TArrayEx<string>>;

			// function GetStringPersister(const _sIniSection : string) : IFilePersister<string>;
			// function GetIntegerPersister(const _sIniSection: string) : IFilePersister<Integer>;
			// function GetBoolPersister(const _sIniSection: string) : IFilePersister<Boolean>;

			function ToLogString() : string;

			{ IFileHandler }
			procedure ReloadFile();
			procedure UpdateFile();
			procedure EraseSection(const _section : string);
			property FilePath : string read GetFilePath write SetFilePath;
	end;

implementation

uses
	RipGrepper.Tools.DebugUtils,
	System.SysUtils,
	RipGrepper.Helper.MemIniFile,
	RipGrepper.Helper.Types,
	{$IFNDEF STANDALONE}
	RipGrepper.Common.IOTAUtils,
	{$ENDIF}
	Vcl.Forms,
	System.Classes,
	System.IOUtils,
	System.StrUtils;

class function TAutoLock.Create(_criticalSection : TCriticalSection) : TAutoLock;
begin
	Result.FCriticalSection := _criticalSection;
	Result.Initialize;
end;

procedure TAutoLock.Initialize;
begin
	if Assigned(FCriticalSection) then
		FCriticalSection.Enter;
end;

procedure TAutoLock.Finalize;
begin
	if Assigned(FCriticalSection) then
		FCriticalSection.Leave;
end;

function TMemIniStringPersister.TryLoadValue(var _value : string) : Boolean;
begin
	var
	autoLock := TAutoLock.Create(TIniPersister.FIniFileLock);
	Result := IniFile.KeyExists(FIniSection, FIniKey);
	if Result then begin
		_value := IniFile.ReadString(FIniSection, FIniKey, '');
	end;
end;

procedure TMemIniStringPersister.StoreValue(const _value : string);
begin
	var
	autoLock := TAutoLock.Create(TIniPersister.FIniFileLock);
	var
	dbgMsg := TDebugMsgBeginEnd.New('TMemIniStringPersister.StoreValue');
	dbgMsg.MsgFmt('[%s] %s = %s', [FIniSection, FIniKey, _value]);
	IniFile.WriteString(FIniSection, FIniKey, _value);

	{$IFDEF DEBUG}
	checkWritenValue(_value);
	{$ENDIF}
end;

constructor TMemIniStringPersister.Create(_ini : IShared<TMemIniFile>; const _sIniSection, _sKey : string);
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TMemIniStringPersister.Create');
	inherited Create(_ini);
	FIniSection := _sIniSection;
	FIniKey := _sKey;
	dbgMsg.MsgFmt('Created persister for [%s] %s', [FIniSection, FIniKey]);
end;

function TMemIniStringPersister.LoadValue(const _section, _key : string) : string;
begin
	Result := IniFile.ReadString(_section, _key, '');
end;

function TMemIniIntegerPersister.TryLoadValue(var _value : integer) : Boolean;
begin
	var
	autoLock := TAutoLock.Create(TIniPersister.FIniFileLock);
	Result := IniFile.KeyExists(FIniSection, FIniKey);

	if Result then begin
		_value := IniFile.ReadInteger(FIniSection, FIniKey, -1);
	end;
end;

procedure TMemIniIntegerPersister.StoreValue(const _value : integer);
begin
	var
	autoLock := TAutoLock.Create(TIniPersister.FIniFileLock);
	var
	dbgMsg := TDebugMsgBeginEnd.New('TMemIniIntegerPersister.StoreValue');
	dbgMsg.MsgFmt('[%s] %s = %d', [FIniSection, FIniKey, _value]);
	IniFile.WriteInteger(FIniSection, FIniKey, _value);
end;

constructor TMemIniIntegerPersister.Create(_ini : IShared<TMemIniFile>; const _sIniSection, _sKey : string);
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TMemIniIntegerPersister.Create');
	inherited Create(_ini);
	FIniSection := _sIniSection;
	FIniKey := _sKey;
	dbgMsg.MsgFmt('Created persister for [%s] %s', [FIniSection, FIniKey]);
end;

function TMemIniIntegerPersister.LoadValue(const _section, _key : string) : integer;
begin
	Result := IniFile.ReadInteger(_section, _key, -1);
end;

function TMemIniBoolPersister.TryLoadValue(var _value : Boolean) : Boolean;
begin
	var
	autoLock := TAutoLock.Create(TIniPersister.FIniFileLock);
	Result := IniFile.KeyExists(FIniSection, FIniKey);
	if Result then begin
		_value := IniFile.ReadBool(FIniSection, FIniKey, False);
	end;
end;

procedure TMemIniBoolPersister.StoreValue(const _value : Boolean);
begin
	var
	autoLock := TAutoLock.Create(TIniPersister.FIniFileLock);
	var
	dbgMsg := TDebugMsgBeginEnd.New('TMemIniBoolPersister.StoreValue');
	dbgMsg.MsgFmt('[%s] %s = %s', [FIniSection, FIniKey, BoolToStr(_value, True)]);
	IniFile.WriteBool(FIniSection, FIniKey, _value);
end;

constructor TMemIniBoolPersister.Create(_ini : IShared<TMemIniFile>; const _sIniSection, _sKey : string);
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TMemIniBoolPersister.Create');
	inherited Create(_ini);
	FIniSection := _sIniSection;
	FIniKey := _sKey;
	dbgMsg.MsgFmt('Created persister for [%s] %s', [FIniSection, FIniKey]);
end;

function TMemIniBoolPersister.LoadValue(const _section, _key : string) : Boolean;
begin
	Result := IniFile.ReadBool(_section, _key, False);
end;

function TMemIniStrArrayPersister.TryLoadValue(var _value : TArrayEx<string>) : Boolean;
begin
	var
	autoLock := TAutoLock.Create(TIniPersister.FIniFileLock);
	var
	key := IfThen(FIniKey = ITEM_KEY_PREFIX, FIniKey + '0', FIniKey);
	Result := IniFile.KeyExists(FIniSection, key);

	if Result then begin
		_value := LoadArrayFromSection(FIniSection);
	end;
end;

procedure TMemIniStrArrayPersister.StoreValue(const _value : TArrayEx<string>);
var
	multiLineVal : TMultiLineString;
begin
	var
	autoLock := TAutoLock.Create(TIniPersister.FIniFileLock);
	var
	dbgMsg := TDebugMsgBeginEnd.New('TMemIniStrArrayPersister.StoreValue');

	dbgMsg.MsgFmt('Erase section: %s', [FIniSection]);
	EraseKeysInSection(FIniSection);

	dbgMsg.Msg('Write Array');
	for var i := 0 to _value.MaxIndex do begin
		multiLineVal := _value[i];
		if not multiLineVal.GetLine(0).IsEmpty then begin
			IniFile.WriteString(FIniSection, Format('%s%d', [FIniKey, i]), multiLineVal.GetLine(0));
		end;
	end;
end;

constructor TMemIniStrArrayPersister.Create(_ini : IShared<TMemIniFile>; const _sIniSection : string; const _sKeyPrefix : string = '');
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TMemIniStrArrayPersister.Create');
	inherited Create(_ini);
	FIniSection := _sIniSection;
	FIniKey := IfThen(_sKeyPrefix.IsEmpty, ITEM_KEY_PREFIX, _sKeyPrefix);
	dbgMsg.MsgFmt('Created array persister for [%s] %s*', [FIniSection, FIniKey]);
end;

function TMemIniStrArrayPersister.LoadArrayFromSection(const _section : string;
	{ } const _keyPrefix : string = ITEM_KEY_PREFIX) : TArrayEx<string>;
var
	i : Integer;
	s : string;
	sNext : string;
begin
	Result := [];
	if not IniFile.SectionExists(FIniSection) then
		Exit;

	i := 0;
	while True do begin
		s := IniFile.ReadString(FIniSection, Format('%s%d', [_keyPrefix, i]), '');
		sNext := IniFile.ReadString(FIniSection, Format('%s%d', [_keyPrefix, i + 1]), '');
		if s.IsEmpty and sNext.IsEmpty then begin
			Break;
		end;
		Result.Add(s);
		Inc(i);
	end;
end;

function TMemIniStrArrayPersister.LoadValue(const _section, _key : string) : TArrayEx<string>;
begin
	Result := LoadArrayFromSection(_section { , ITEM_KEY_PREFIX } );
end;

constructor TIniPersister.Create();
begin
	inherited;
	// Initialize singleton if needed
	getIniFileSingleton();
end;

constructor TIniPersister.Create(_ini : IShared<TMemIniFile>);
begin
	inherited Create();
	// Store the provided ini file for instance-specific operations (like tests)
	FIniFile := _ini;
	// Still initialize singleton for compatibility
	getIniFileSingleton();
end;

procedure TIniPersister.checkWritenValue(const _section, _key, _valueShould : string);
begin
	{$IFDEF DEBUG}
	var
	dbgMsg := TDebugMsgBeginEnd.New('TIniPersister.checkWritenValue');

	var
	stored := getIniFile().ReadString(_section, _key, '???');
	dbgMsg.MsgFmt('Stored [%s] %s = %s', [_section, _key, stored]);
	Assert(stored = _valueShould, Format('Stored [%s] %s = %s <> %s', [_section, _key, stored, _valueShould]));
	{$ENDIF}
end;

procedure TIniPersister.EraseSection(const _section : string);
begin
	var
	autoLock := TAutoLock.Create(FIniFileLock);
	getIniFile().EraseSection(_section);
end;

function TIniPersister.GetBoolPersister(const _sIniSection : string = ''; const _sKey : string = '') : IFilePersister<Boolean>;
begin
	var
	autoLock := TAutoLock.Create(FIniFileLock);
	Result := TMemIniBoolPersister.Create(getIniFile(), _sIniSection, _sKey);
end;

function TIniPersister.GetFilePath() : string;
begin
	var
	autoLock := TAutoLock.Create(FIniFileLock);
	Result := getIniFile().FileName;
end;

function TIniPersister.GetIntegerPersister(const _sIniSection : string = ''; const _sKey : string = '') : IFilePersister<Integer>;
begin
	var
	autoLock := TAutoLock.Create(FIniFileLock);
	Result := TMemIniIntegerPersister.Create(getIniFile(), _sIniSection, _sKey);
end;

function TIniPersister.GetStrArrayPersister(const _sIniSection : string = ''; const _sKey : string = '') : IFilePersister<TArrayEx<string>>;
begin
	var
	autoLock := TAutoLock.Create(FIniFileLock);
	Result := TMemIniStrArrayPersister.Create(getIniFile(), _sIniSection, _sKey);
end;

function TIniPersister.GetStringPersister(const _sIniSection : string = ''; const _sKey : string = '') : IFilePersister<string>;
begin
	var
	autoLock := TAutoLock.Create(FIniFileLock);
	Result := TMemIniStringPersister.Create(getIniFile(), _sIniSection, _sKey);
end;

procedure TIniPersister.ReloadFile();
begin
	var
	autoLock := TAutoLock.Create(FIniFileLock);
	getIniFile().ReloadIniFile;
end;

procedure TIniPersister.SetFilePath(const Value : string);
begin
	var
	autoLock := TAutoLock.Create(FIniFileLock);
	getIniFile().Rename(Value, False);
end;

function TIniPersister.ToLogString() : string;
var
	strs : IShared<TStringList>;
begin
	var
	autoLock := TAutoLock.Create(FIniFileLock);
	strs := Shared.Make<TStringList>();
	getIniFile().GetStrings(strs);
	Result := strs.DelimitedText;
end;

procedure TIniPersister.UpdateFile();
begin
	var
	autoLock := TAutoLock.Create(FIniFileLock);
	var
	dbgMsg := TDebugMsgBeginEnd.New('TIniPersister.UpdateFile');
	{$IFDEF DEBUG}
//  checkWritenValue('RipGrepperSearchSettings', 'Encoding', 'utf8');
	{$ENDIF}
	getIniFile().UpdateFile;
end;

function TIniPersister.getIniFile() : IShared<TMemIniFile>;
begin
	// Use instance-specific INI file if available, otherwise fall back to singleton
	if Assigned(FIniFile) then
		Result := FIniFile
	else
		Result := getIniFileSingleton();
end;

class function TIniPersister.getIniFileSingleton() : IShared<TMemIniFile>;
begin
	if not Assigned(FIniFileSingleton) then begin
		var
		autoLock := TAutoLock.Create(FIniFileLock);
		// Double-checked locking pattern
		if not Assigned(FIniFileSingleton) then begin
			var
			dbgMsg := TDebugMsgBeginEnd.New('TIniPersister.getIniFileSingleton');
			var
			fileName : string;
			{$IFDEF STANDALONE}
			fileName := TPath.ChangeExtension(Application.ExeName, '.ini');
			{$ELSE}
			fileName := TPath.Combine(IOTAUTils.GetSettingFilePath, EXTENSION_NAME + '.ini');
			{$ENDIF}
			dbgMsg.Msg('file name:' + fileName);
			FIniFileSingleton := Shared.Make<TMemIniFile>(TMemIniFile.Create(fileName, TEncoding.UTF8));
		end;
	end;
	Result := FIniFileSingleton;
end;

constructor TMemIniPersister.Create(_ini : IShared<TMemIniFile>);
begin
	// Use the provided ini file if available, otherwise use singleton for compatibility
	if Assigned(_ini) then
		FIniFile := _ini
	else
		FIniFile := TIniPersister.getIniFileSingleton();
end;

destructor TMemIniPersister.Destroy();
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TMemIniPersister.Destroy');
	dbgMsg.MsgFmt('[%s] %s', [FIniSection, FIniKey]);
	FIniFile := nil; // Clean up the reference
	inherited;
end;

procedure TMemIniPersister.checkWritenValue(const _expectedValue : string);
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TMemIniPersister.checkWritenValue');

	var
	stored := IniFile.ReadString(FIniSection, FIniKey, '???');
	dbgMsg.MsgFmt('Stored [%s] %s = %s', [FIniSection, FIniKey, stored]);
	Assert(stored = _expectedValue, Format('Stored [%s] %s = %s <> %s', [FIniSection, FIniKey, stored, _expectedValue]));
end;

procedure TMemIniPersister.checkWritenValue(const _section, _key, _valueShould
	: string);
begin
	{$IFDEF DEBUG}
	var
	dbgMsg := TDebugMsgBeginEnd.New('TMemIniPersister.checkWritenValue');

	var
	stored := TIniPersister.getIniFileSingleton().ReadString(_section, _key, '???');
	dbgMsg.MsgFmt('Stored [%s] %s = %s', [_section, _key, stored]);
	Assert(stored = _valueShould, Format('Stored [%s] %s = %s <> %s', [_section, _key, stored, _valueShould]));
	{$ENDIF}
end;

procedure TMemIniPersister.EraseKeysInSection(_section: string);
var
	slKeys: TStrings;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TMemIniPersister.EraseKeysInSection');
	dbgMsg.MsgFmt('Section %s', [_section]);
	slKeys := TStringList.Create;
	try
		FIniFile.ReadSection(FIniSection, slKeys);
		for var key in slKeys do begin
			dbgMsg.MsgFmt('Key %s', [key]);
			FIniFile.DeleteKey(FIniSection, key);
		end;
	finally
		slKeys.Free;
	end;
end;

function TMemIniPersister.GetFilePath() : string;
begin
	// Use the stored FIniFile if available, otherwise fall back to singleton
	if Assigned(FIniFile) then
		Result := FIniFile.FileName
	else
		Result := TIniPersister.getIniFileSingleton().FileName;
end;

function TMemIniPersister.GetIniFile() : TMemIniFile;
begin
	// Use the stored FIniFile if available, otherwise fall back to singleton
	if Assigned(FIniFile) then
		Result := TMemIniFile(FIniFile())
	else
		Result := TMemIniFile(TIniPersister.getIniFileSingleton()());
end;

procedure TMemIniPersister.SetFilePath(const Value : string);
begin
	// Use the stored FIniFile if available, otherwise fall back to singleton
	if Assigned(FIniFile) then
		FIniFile.Rename(Value, False)
	else
		TIniPersister.getIniFileSingleton().Rename(Value, False);
end;

initialization
	TIniPersister.FIniFileLock := TCriticalSection.Create;

finalization
	TIniPersister.FIniFileSingleton := nil; // Release the singleton before freeing the lock
	TIniPersister.FIniFileLock.Free;

end.
