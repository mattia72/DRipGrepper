unit RipGrepper.Settings.FilePersister;

interface

uses
	System.IniFiles,
	ArrayEx,
	Spring;

type

	IPersister = interface(IInterface)
		['{76ED2AA3-9E4D-4B54-B061-185BF880C508}']
	end;

	IFilePersister<T> = interface(IPersister)
		['{57B16806-F8F5-447E-9AB6-767E553CCB65}']
		function TryLoadValue(var _value : T) : Boolean;
		function LoadSectionKey(const _section, _key : string) : T; // TODO: Exception if not exists
		procedure StoreToPersister(const _value : T);

		function GetFilePath() : string;
		procedure SetFilePath(const Value : string);
		property FilePath : string read GetFilePath write SetFilePath;
	end;

	IPersisterFactory = interface
		['{86CA585C-A4D5-44E4-A778-9C011291F623}']
		function GetStringPersister(const _sIniSection : string = ''; const _sKey : string = '') : IFilePersister<string>;
		function GetIntegerPersister(const _sIniSection : string = ''; const _sKey : string = '') : IFilePersister<Integer>;
		function GetBoolPersister(const _sIniSection : string = ''; const _sKey : string = '') : IFilePersister<Boolean>;
		function GetStrArrayPersister(const _sIniSection : string = ''; const _sKey : string = '') : IFilePersister<TArrayEx<string>>;

		function ToLogString() : string;

		function GetFilePath() : string;
		procedure SetFilePath(const Value : string);
		property FilePath : string read GetFilePath write SetFilePath;

	end;

	IFileHandler = interface(IInterface)
		['{500D0067-F8F2-488B-B3B7-5649DF5879E4}']
		procedure ReloadFile();
		procedure UpdateFile();
		procedure EraseSection(const _section : string);
		function GetFilePath() : string;
		procedure SetFilePath(const Value : string);
		property FilePath : string read GetFilePath write SetFilePath;
	end;

	TMemIniPersister = class(TInterfacedObject)
		private
			function GetFilePath() : string;
			procedure SetFilePath(const Value : string);

		protected
			FIniFile : TMemIniFile;
			FIniKey : string;
			FIniSection : string;

		public
			constructor Create(_ini : TMemIniFile); overload;
			property FilePath : string read GetFilePath write SetFilePath;
	end;

	TMemIniStringPersister = class(TMemIniPersister, IFilePersister<string>)
		strict private
		public
			constructor Create(_ini : TMemIniFile; const _sIniSection, _sKey : string); overload;
			function TryLoadValue(var _value : string) : Boolean;
			function LoadSectionKey(const _section, _key : string) : string;
			procedure StoreToPersister(const _value : string);
	end;

	TMemIniIntegerPersister = class(TMemIniPersister, IFilePersister<integer>)
		public
			constructor Create(_ini : TMemIniFile; const _sIniSection, _sKey : string);
			function TryLoadValue(var _value : integer) : Boolean;
			function LoadSectionKey(const _section, _key : string) : integer;
			procedure StoreToPersister(const _value : integer);
	end;

	TMemIniBoolPersister = class(TMemIniPersister, IFilePersister<Boolean>)
		public
			constructor Create(_ini : TMemIniFile; const _sIniSection, _sKey : string);
			function TryLoadValue(var _value : Boolean) : Boolean;
			function LoadSectionKey(const _section, _key : string) : Boolean;
			procedure StoreToPersister(const _value : Boolean);
	end;

	TMemIniStrArrayPersister = class(TMemIniPersister, IFilePersister < TArrayEx < string >> )
		private
			function LoadArrayFromSection(const _section, _key : string) : TArrayEx<string>;

		public
			constructor Create(_ini : TMemIniFile; const _sIniSection : string);
			function TryLoadValue(var _value : TArrayEx<string>) : Boolean;
			function LoadSectionKey(const _section, _key : string) : TArrayEx<string>;
			procedure StoreToPersister(const _value : TArrayEx<string>);
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
			//

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
	RipGrepper.Common.Constants;

function TMemIniStringPersister.TryLoadValue(var _value : string) : Boolean;
begin
	Result := FIniFile.KeyExists(FIniSection, FIniKey);
	if Result then begin
		_value := FIniFile.ReadString(FIniSection, FIniKey, '');
	end;
end;

procedure TMemIniStringPersister.StoreToPersister(const _value : string);
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TMemIniStringPersister.StoreToPersister');

	FIniFile.WriteString(FIniSection, FIniKey, _value);
end;

constructor TMemIniStringPersister.Create(_ini : TMemIniFile; const _sIniSection, _sKey : string);
begin
	FIniFile := _ini;
	FIniSection := _sIniSection;
	FIniKey := _sKey;
end;

function TMemIniStringPersister.LoadSectionKey(const _section, _key : string) : string;
begin
	Result := FIniFile.ReadString(_section, _key, '');
end;

// function TMemIniStringPersister.Make(): IPersister;
// begin
// Result := ;
// end;

function TMemIniIntegerPersister.TryLoadValue(var _value : integer) : Boolean;
begin
	Result := FIniFile.KeyExists(FIniSection, FIniKey);

	if Result then begin
		_value := FIniFile.ReadInteger(FIniSection, FIniKey, -1);
	end;
end;

procedure TMemIniIntegerPersister.StoreToPersister(const _value : integer);
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TMemIniIntegerPersister.StoreToPersister');
	FIniFile.WriteInteger(FIniSection, FIniKey, _value);
end;

constructor TMemIniIntegerPersister.Create(_ini : TMemIniFile; const _sIniSection, _sKey : string);
begin
	FIniFile := _ini;
	FIniSection := _sIniSection;
	FIniKey := _sKey;
end;

function TMemIniIntegerPersister.LoadSectionKey(const _section, _key : string) : integer;
begin
	Result := FIniFile.ReadInteger(_section, _key, -1);
end;

function TMemIniBoolPersister.TryLoadValue(var _value : Boolean) : Boolean;
begin
	Result := FIniFile.KeyExists(FIniSection, FIniKey);
	if Result then begin
		_value := FIniFile.ReadBool(FIniSection, FIniKey, False);
	end;
end;

procedure TMemIniBoolPersister.StoreToPersister(const _value : Boolean);
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TMemIniBoolPersister.StoreToPersister');
	FIniFile.WriteBool(FIniSection, FIniKey, _value);
end;

constructor TMemIniBoolPersister.Create(_ini : TMemIniFile; const _sIniSection, _sKey : string);
begin
	FIniFile := _ini;
	FIniSection := _sIniSection;
	FIniKey := _sKey;
end;

function TMemIniBoolPersister.LoadSectionKey(const _section, _key : string) : Boolean;
begin
	Result := FIniFile.ReadBool(_section, _key, False);
end;

function TMemIniStrArrayPersister.TryLoadValue(var _value : TArrayEx<string>) : Boolean;
begin
	Result := FIniFile.KeyExists(FIniSection, FIniKey);

	if Result then begin
		_value := LoadArrayFromSection(FIniSection, FIniKey);
	end;
end;

procedure TMemIniStrArrayPersister.StoreToPersister(const _value : TArrayEx<string>);
var
	multiLineVal : TMultiLineString;
	i : Integer;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TMemIniStrArrayPersister.StoreToPersister');
	i := _value.MaxIndex;
	dbgMsg.Msg('Write Array');
	while i >= 0 do begin
		multiLineVal := _value[i];
		FIniFile.WriteString(FIniSection, Format('%s%d', [FIniKey, i]), multiLineVal.GetLine(0));
		Dec(i);
	end;
end;

constructor TMemIniStrArrayPersister.Create(_ini : TMemIniFile; const _sIniSection : string);
begin
	FIniFile := _ini;
	FIniSection := _sIniSection;
	FIniKey := 'Item_';
end;

function TMemIniStrArrayPersister.LoadArrayFromSection(const _section, _key : string) : TArrayEx<string>;
var
	i : Integer;
	s : string;
begin
	Result := [];
	i := 1;
	while True do begin
		s := FIniFile.ReadString(FIniSection, Format('%s%d', [_key, i]), '');
		if s = '' then
			Break;
		Result.Insert(0, s);
		Inc(i);
	end;
end;

function TMemIniStrArrayPersister.LoadSectionKey(const _section, _key : string) : TArrayEx<string>;
begin
	Result := LoadArrayFromSection(_section, _key);
end;

constructor TIniPersister.Create();
begin
	inherited;
	{$IFDEF STANDALONE}
	FIniFile := Shared.Make<TMemIniFile>(
		{ } TMemIniFile.Create(TPath.ChangeExtension(Application.ExeName, '.ini'), TEncoding.UTF8));
	{$ELSE}
	FIniFile := Shared.Make<TMemIniFile>(
		{ } TMemIniFile.Create(TPath.Combine(IOTAUTils.GetSettingFilePath, EXTENSION_NAME + '.ini'), TEncoding.UTF8));
	{$ENDIF}
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
	Result := TMemIniStrArrayPersister.Create(FIniFile, _sKey); // _sSection is ROOT_DUMMY, key will be section
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

function TMemIniPersister.GetFilePath() : string;
begin
	Result := FIniFile.FileName;
end;

procedure TMemIniPersister.SetFilePath(const Value : string);
begin
	FIniFile.Rename(Value, False);
end;

end.
