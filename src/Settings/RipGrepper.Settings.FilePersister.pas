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
		function LoadFromPersister() : T;
		procedure StoreToPersister(const _value : T);

		function GetFilePath() : string;
		procedure SetFilePath(const Value : string);
		property FilePath : string read GetFilePath write SetFilePath;
	end;

	IPersisterFactory = interface
		['{86CA585C-A4D5-44E4-A778-9C011291F623}']
		function GetStringPersister(const _sIniSection, _sKey : string) : IFilePersister<string>;
		function GetIntegerPersister(const _sIniSection, _sKey : string) : IFilePersister<Integer>;
		function GetBoolPersister(const _sIniSection, _sKey : string) : IFilePersister<Boolean>;
		function GetStrArrayPersister(const _sIniSection, _sKey : string) : IFilePersister<TArrayEx<string>>;
		function ToLogString() : string;

		function GetFilePath() : string;
		procedure SetFilePath(const Value : string);
		property FilePath : string read GetFilePath write SetFilePath;

	end;

	IFileHandler = interface(IInterface)
		['{500D0067-F8F2-488B-B3B7-5649DF5879E4}']
		procedure ReloadFile();
		procedure WriteFile();
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
			property FilePath : string read GetFilePath write SetFilePath;
	end;

	TMemIniStringPersister = class(TMemIniPersister, IFilePersister<string>)
		public
			constructor Create(_ini : TMemIniFile; const _sIniSection, _sKey : string);
			function LoadFromPersister() : string;
			procedure StoreToPersister(const _value : string);
	end;

	TMemIniIntegerPersister = class(TMemIniPersister, IFilePersister<integer>)
		public
			constructor Create(_ini : TMemIniFile; const _sIniSection, _sKey : string);
			function LoadFromPersister() : integer;
			procedure StoreToPersister(const _value : integer);
	end;

	TMemIniBoolPersister = class(TMemIniPersister, IFilePersister<Boolean>)
		public
			constructor Create(_ini : TMemIniFile; const _sIniSection, _sKey : string);
			function LoadFromPersister() : Boolean;
			procedure StoreToPersister(const _value : Boolean);
	end;

	TMemIniStrArrayPersister = class(TMemIniPersister, IFilePersister < TArrayEx < string >> )
		public
			constructor Create(_ini : TMemIniFile; const _sIniSection : string);
			function LoadFromPersister() : TArrayEx<string>;
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
			function GetStringPersister(const _sIniSection, _sKey : string) : IFilePersister<string>;
			function GetIntegerPersister(const _sIniSection, _sKey : string) : IFilePersister<Integer>;
			function GetBoolPersister(const _sIniSection, _sKey : string) : IFilePersister<Boolean>;
			function GetStrArrayPersister(const _sIniSection, _sKey : string) : IFilePersister<TArrayEx<string>>;
			function ToLogString() : string;

			{ IFileHandler }
			procedure ReloadFile();
			procedure WriteFile();
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

function TMemIniStringPersister.LoadFromPersister() : string;
begin
	Result := FIniFile.ReadString(FIniSection, FIniKey, '');
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

// function TMemIniStringPersister.Make(): IPersister;
// begin
// Result := ;
// end;

function TMemIniIntegerPersister.LoadFromPersister() : integer;
begin
	Result := FIniFile.ReadInteger(FIniSection, FIniKey, -1);
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

function TMemIniBoolPersister.LoadFromPersister() : Boolean;
begin
	Result := FIniFile.ReadBool(FIniSection, FIniKey, False);
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

function TMemIniStrArrayPersister.LoadFromPersister() : TArrayEx<string>;
var
	i : Integer;
	s : string;
begin
	Result := [];
	i := 1;
	while True do begin
		s := FIniFile.ReadString(FIniSection, Format('%s%d', [FIniKey, i]), '');
		if s = '' then
			Break;
		Result.Insert(0, s);
		Inc(i);
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

function TIniPersister.GetBoolPersister(const _sIniSection, _sKey : string) : IFilePersister<Boolean>;
begin
	Result := TMemIniBoolPersister.Create(FIniFile, _sIniSection, _sKey);
end;

function TIniPersister.GetFilePath() : string;
begin
	Result := FIniFile.FileName;
end;

function TIniPersister.GetIntegerPersister(const _sIniSection, _sKey : string) : IFilePersister<Integer>;
begin
	Result := TMemIniIntegerPersister.Create(FIniFile, _sIniSection, _sKey);
end;

function TIniPersister.GetStrArrayPersister(const _sIniSection, _sKey : string) : IFilePersister<TArrayEx<string>>;
begin
	Result := TMemIniStrArrayPersister.Create(FIniFile, _sKey); // _sSection is ROOT_DUMMY, key will be section

end;

function TIniPersister.GetStringPersister(const _sIniSection, _sKey : string) : IFilePersister<string>;
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

procedure TIniPersister.WriteFile();
begin
	FIniFile.UpdateFile;
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
