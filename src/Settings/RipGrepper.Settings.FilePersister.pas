unit RipGrepper.Settings.FilePersister;

interface

uses
	System.IniFiles,
	ArrayEx;

type
	IFilePersister<T> = interface(IInterface)
		['{57B16806-F8F5-447E-9AB6-767E553CCB65}']
		function LoadFromFile() : T;
		procedure ReloadFile();
		procedure SaveToFile(const _value : T);
	end;

	TMemIniStringPersister = class(TInterfacedObject, IFilePersister<string>)
		private
			FIniFile : TMemIniFile;
			FIniKey : string;
			FIniSection : string;

		public
			constructor Create(_ini : TMemIniFile; _sIniSection, _sKey : string);

			function LoadFromFile() : string;
			procedure ReloadFile();
			procedure SaveToFile(const _value : string);
	end;

	TMemIniIntegerPersister = class(TInterfacedObject, IFilePersister<integer>)
		private
			FIniFile : TMemIniFile;
			FIniKey : string;
			FIniSection : string;

		public
			constructor Create(_ini : TMemIniFile; _sIniSection, _sKey : string);

			function LoadFromFile() : integer;
			procedure ReloadFile();
			procedure SaveToFile(const _value : integer);
	end;

	TMemIniBoolPersister = class(TInterfacedObject, IFilePersister<Boolean>)
		private
			FIniFile : TMemIniFile;
			FIniKey : string;
			FIniSection : string;

		public
			constructor Create(_ini : TMemIniFile; _sIniSection, _sKey : string);

			function LoadFromFile() : Boolean;
			procedure ReloadFile();
			procedure SaveToFile(const _value : Boolean);
	end;

	TMemIniStrArrayPersister = class(TInterfacedObject, IFilePersister < TArrayEx < string >> )
		private
			FIniFile : TMemIniFile;
			FIniKey : string;
			FIniSection : string;

		public
			constructor Create(_ini : TMemIniFile; _sIniSection, _sKey : string);

			function LoadFromFile() : TArrayEx<string>;
			procedure ReloadFile();
			procedure SaveToFile(const _value : TArrayEx<string>);
	end;

implementation

uses
	RipGrepper.Tools.DebugUtils,
	System.SysUtils,
	RipGrepper.Helper.MemIniFile,
	RipGrepper.Helper.Types;

function TMemIniStringPersister.LoadFromFile() : string;
begin

end;

procedure TMemIniStringPersister.ReloadFile();
begin
	FIniFile.ReLoadIniFile();
end;

procedure TMemIniStringPersister.SaveToFile(const _value : string);
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TMemIniStringPersister.SaveToFile');

	FIniFile.WriteString(FIniSection, FIniKey, _value);

end;

constructor TMemIniStringPersister.Create(_ini : TMemIniFile; _sIniSection, _sKey : string);
begin
	FIniFile := _ini;
	FIniSection := _sIniSection;
	FIniKey := _sKey;
end;

function TMemIniIntegerPersister.LoadFromFile() : integer;
begin
	Result := FIniFile.ReadInteger(FIniSection, FIniKey, -1);
end;

procedure TMemIniIntegerPersister.ReloadFile();
begin
	FIniFile.ReLoadIniFile();
end;

procedure TMemIniIntegerPersister.SaveToFile(const _value : integer);
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TMemIniIntegerPersister.SaveToFile');
	FIniFile.WriteInteger(FIniSection, FIniKey, _value);
end;

constructor TMemIniIntegerPersister.Create(_ini : TMemIniFile; _sIniSection, _sKey : string);
begin
	FIniFile := _ini;
	FIniSection := _sIniSection;
	FIniKey := _sKey;
end;

function TMemIniBoolPersister.LoadFromFile() : Boolean;
begin
	Result := FIniFile.ReadBool(FIniSection, FIniKey, False);
end;

procedure TMemIniBoolPersister.ReloadFile();
begin
	FIniFile.ReLoadIniFile();
end;

procedure TMemIniBoolPersister.SaveToFile(const _value : Boolean);
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TMemIniBoolPersister.SaveToFile');
	FIniFile.WriteBool(FIniSection, FIniKey, _value);
end;

constructor TMemIniBoolPersister.Create(_ini : TMemIniFile; _sIniSection, _sKey : string);
begin
	FIniFile := _ini;
	FIniSection := _sIniSection;
	FIniKey := _sKey;
end;

procedure TMemIniStrArrayPersister.ReloadFile();
begin
	FIniFile.ReLoadIniFile();
end;

function TMemIniStrArrayPersister.LoadFromFile(): TArrayEx<string>;
var
	i : Integer;
	s : string;
begin
	Result := [];
	i := 1;
	while True do begin
		s := FIniFile.ReadString(FIniSection, Format('%s_Item%d', [FIniKey, i]), '');
		if s = '' then
			Break;
		Result.Add(s);
		Inc(i);
	end;
end;

procedure TMemIniStrArrayPersister.SaveToFile(const _value : TArrayEx<string>);
var
	multiLineVal : TMultiLineString;
	i : Integer;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TMemIniStrArrayPersister.SaveToFile');
	i := 0;
	dbgMsg.Msg('Write Array');
	while i <= _value.count do begin
		multiLineVal := _value[i];
		FIniFile.WriteString(FIniSection, Format('%s_Item%d', [FIniKey, i]), multiLineVal.GetLine(0));
		Inc(i);
	end;
end;

constructor TMemIniStrArrayPersister.Create(_ini : TMemIniFile; _sIniSection, _sKey : string);
begin
	FIniFile := _ini;
	FIniSection := _sIniSection;
	FIniKey := _sKey;
end;

end.
