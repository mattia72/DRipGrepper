unit RipGrepper.Settings.FilePersister;

interface

uses
	System.IniFiles,
	ArrayEx,
	Spring,
	RipGrepper.Common.Constants,
	RipGrepper.Settings.Persister.Interfaces;

type

	TMemIniPersister = class(TInterfacedObject)
		private
			function GetFilePath() : string;
			function GetIniFile() : TMemIniFile;
			procedure SetFilePath(const Value : string);

		protected
			FIniFile : TMemIniFile;
			FIniKey : string;
			FIniSection : string;

		public
			constructor Create(_ini : TMemIniFile); overload;
			destructor Destroy(); override;
			property FilePath : string read GetFilePath write SetFilePath;
			property IniFile : TMemIniFile read GetIniFile;
	end;

	TMemIniStringPersister = class(TMemIniPersister, IFilePersister<string>)
		strict private
		public
			constructor Create(_ini : TMemIniFile; const _sIniSection, _sKey : string); overload;
			function TryLoadValue(var _value : string) : Boolean;
			function LoadValue(const _section, _key : string) : string;
			procedure StoreValue(const _value : string);
	end;

	TMemIniIntegerPersister = class(TMemIniPersister, IFilePersister<integer>)
		public
			constructor Create(_ini : TMemIniFile; const _sIniSection, _sKey : string);
			function TryLoadValue(var _value : integer) : Boolean;
			function LoadValue(const _section, _key : string) : integer;
			procedure StoreValue(const _value : integer);
	end;

	TMemIniBoolPersister = class(TMemIniPersister, IFilePersister<Boolean>)
		public
			constructor Create(_ini : TMemIniFile; const _sIniSection, _sKey : string);
			function TryLoadValue(var _value : Boolean) : Boolean;
			function LoadValue(const _section, _key : string) : Boolean;
			procedure StoreValue(const _value : Boolean);
	end;

	TMemIniStrArrayPersister = class(TMemIniPersister, IFilePersister < TArrayEx < string >> )

		private
			function LoadArrayFromSection(const _section : string; const _keyPrefix : string = ITEM_KEY_PREFIX) : TArrayEx<string>;

		public
			constructor Create(_ini : TMemIniFile; const _sIniSection : string; const _sKeyPrefix : string = '');
			function TryLoadValue(var _value : TArrayEx<string>) : Boolean;
			function LoadValue(const _section, _key : string) : TArrayEx<string>;
			procedure StoreValue(const _value : TArrayEx<string>);
	end;

	TIniPersister = class(TInterfacedObject, IPersisterFactory, IFileHandler)
		private
			FIniFile : IShared<TMemIniFile>;
			function GetFilePath() : string;
			procedure SetFilePath(const Value : string);

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

function TMemIniStringPersister.TryLoadValue(var _value : string) : Boolean;
begin
	Result := IniFile.KeyExists(FIniSection, FIniKey);
	if Result then begin
		_value := IniFile.ReadString(FIniSection, FIniKey, '');
	end;
end;

procedure TMemIniStringPersister.StoreValue(const _value : string);
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TMemIniStringPersister.StoreValue');
	dbgMsg.MsgFmt('[%s] %s = %s', [FIniSection, FIniKey, _value]);
	IniFile.WriteString(FIniSection, FIniKey, _value);
end;

constructor TMemIniStringPersister.Create(_ini : TMemIniFile; const _sIniSection, _sKey : string);
begin
	FIniFile := _ini;
	FIniSection := _sIniSection;
	FIniKey := _sKey;
end;

function TMemIniStringPersister.LoadValue(const _section, _key : string) : string;
begin
	Result := IniFile.ReadString(_section, _key, '');
end;

// function TMemIniStringPersister.Make(): IPersister;
// begin
// Result := ;
// end;

function TMemIniIntegerPersister.TryLoadValue(var _value : integer) : Boolean;
begin
	Result := IniFile.KeyExists(FIniSection, FIniKey);

	if Result then begin
		_value := IniFile.ReadInteger(FIniSection, FIniKey, -1);
	end;
end;

procedure TMemIniIntegerPersister.StoreValue(const _value : integer);
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TMemIniIntegerPersister.StoreValue');
	dbgMsg.MsgFmt('[%s] %s = %d', [FIniSection, FIniKey, _value]);
	IniFile.WriteInteger(FIniSection, FIniKey, _value);
end;

constructor TMemIniIntegerPersister.Create(_ini : TMemIniFile; const _sIniSection, _sKey : string);
begin
	FIniFile := _ini;
	FIniSection := _sIniSection;
	FIniKey := _sKey;
end;

function TMemIniIntegerPersister.LoadValue(const _section, _key : string) : integer;
begin
	Result := IniFile.ReadInteger(_section, _key, -1);
end;

function TMemIniBoolPersister.TryLoadValue(var _value : Boolean) : Boolean;
begin
	Result := IniFile.KeyExists(FIniSection, FIniKey);
	if Result then begin
		_value := IniFile.ReadBool(FIniSection, FIniKey, False);
	end;
end;

procedure TMemIniBoolPersister.StoreValue(const _value : Boolean);
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TMemIniBoolPersister.StoreValue');
	dbgMsg.MsgFmt('[%s] %s = %s', [FIniSection, FIniKey, BoolToStr(_value, True)]);
	IniFile.WriteBool(FIniSection, FIniKey, _value);
end;

constructor TMemIniBoolPersister.Create(_ini : TMemIniFile; const _sIniSection, _sKey : string);
begin
	FIniFile := _ini;
	FIniSection := _sIniSection;
	FIniKey := _sKey;
end;

function TMemIniBoolPersister.LoadValue(const _section, _key : string) : Boolean;
begin
	Result := IniFile.ReadBool(_section, _key, False);
end;

function TMemIniStrArrayPersister.TryLoadValue(var _value : TArrayEx<string>) : Boolean;
begin
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
	dbgMsg := TDebugMsgBeginEnd.New('TMemIniStrArrayPersister.StoreValue');
	dbgMsg.Msg('Write Array');

	for var i := 0 to _value.MaxIndex do begin
		multiLineVal := _value[i];
		IniFile.WriteString(FIniSection, Format('%s%d', [FIniKey, i]), multiLineVal.GetLine(0));
	end;

end;

constructor TMemIniStrArrayPersister.Create(_ini : TMemIniFile; const _sIniSection : string; const _sKeyPrefix : string = '');
begin
	FIniFile := _ini;
	FIniSection := _sIniSection;
	FIniKey := IfThen(_sKeyPrefix.IsEmpty, ITEM_KEY_PREFIX, _sKeyPrefix);
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
var
	fileName : string;
begin
	inherited;
	var
	dbgMsg := TDebugMsgBeginEnd.New('TIniPersister.Create');

	{$IFDEF STANDALONE}
	fileName := TPath.ChangeExtension(Application.ExeName, '.ini');
	{$ELSE}
	fileName := TPath.Combine(IOTAUTils.GetSettingFilePath, EXTENSION_NAME + '.ini');
	{$ENDIF}
	dbgMsg.Msg('file name:' + fileName);
	FIniFile := Shared.Make<TMemIniFile>(TMemIniFile.Create(fileName, TEncoding.UTF8));
end;

constructor TIniPersister.Create(_ini : IShared<TMemIniFile>);
begin
	FIniFile := _ini;
end;

procedure TIniPersister.EraseSection(const _section : string);
begin
	FIniFile.EraseSection(_section);
end;

function TIniPersister.GetBoolPersister(const _sIniSection : string = ''; const _sKey : string = '') : IFilePersister<Boolean>;
begin
	Result := TMemIniBoolPersister.Create(FIniFile, _sIniSection, _sKey);
end;

function TIniPersister.GetFilePath() : string;
begin
	Result := FIniFile.FileName;
end;

function TIniPersister.GetIntegerPersister(const _sIniSection : string = ''; const _sKey : string = '') : IFilePersister<Integer>;
begin
	Result := TMemIniIntegerPersister.Create(FIniFile, _sIniSection, _sKey);
end;

function TIniPersister.GetStrArrayPersister(const _sIniSection : string = ''; const _sKey : string = '') : IFilePersister<TArrayEx<string>>;
begin
	Result := TMemIniStrArrayPersister.Create(FIniFile, _sIniSection, _sKey);
end;

function TIniPersister.GetStringPersister(const _sIniSection : string = ''; const _sKey : string = '') : IFilePersister<string>;
begin
	Result := TMemIniStringPersister.Create(FIniFile, _sIniSection, _sKey);
end;

procedure TIniPersister.ReloadFile();
begin
	FIniFile.ReloadIniFile;
end;

procedure TIniPersister.SetFilePath(const Value : string);
begin
	FIniFile.Rename(Value, False);
end;

function TIniPersister.ToLogString() : string;
var
	strs : IShared<TStringList>;
begin
	strs := Shared.Make<TStringList>();
	FIniFile.GetStrings(strs);
	Result := strs.DelimitedText;
end;

procedure TIniPersister.UpdateFile();
begin
	FIniFile.UpdateFile;
end;

constructor TMemIniPersister.Create(_ini : TMemIniFile);
begin
	FIniFile := _ini;
end;

destructor TMemIniPersister.Destroy();
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TMemIniPersister.Destroy');
	dbgMsg.MsgFmt('[%s] %s', [FIniSection, FIniKey]);
	inherited;
end;

function TMemIniPersister.GetFilePath() : string;
begin
	Result := FIniFile.FileName;
end;

function TMemIniPersister.GetIniFile() : TMemIniFile;
begin
	Assert(FIniFile <> nil, 'FIniFile is nil');
	Result := FIniFile;
end;

procedure TMemIniPersister.SetFilePath(const Value : string);
begin
	FIniFile.Rename(Value, False);
end;

end.
