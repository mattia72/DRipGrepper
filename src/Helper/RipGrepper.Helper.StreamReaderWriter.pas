unit RipGrepper.Helper.StreamReaderWriter;

interface

uses
	System.Classes;

type
	TStreamReaderHelper = class Helper for TStreamReader
		public
			function ReadLineAsBool() : Boolean;
			function ReadLineAsInteger() : Integer;
	end;

type
	TStreamWriterHelper = class Helper for TStreamWriter
		public
			procedure WriteLineAsBool(const _b : Boolean);
			procedure WriteLineAsInteger(const _i : Integer);
			procedure WriteLineAsString(const _s : string; const _bAllowEmpty : boolean =
				false);
	end;

implementation

uses
	System.SysUtils;

function TStreamReaderHelper.ReadLineAsBool() : Boolean;
var
	s : string;
begin
	s := self.ReadLine();
	Result := s <> '0';
end;

function TStreamReaderHelper.ReadLineAsInteger() : Integer;
var
	s : string;
begin
	s := self.ReadLine();
	if s.IsEmpty then begin
		raise Exception.Create('Try to read empty string from stream.');
	end;

	Result := StrToIntDef(s, 0);
end;

procedure TStreamWriterHelper.WriteLineAsBool(const _b : Boolean);
begin
	self.WriteLine(BoolToStr(_b));
end;

procedure TStreamWriterHelper.WriteLineAsInteger(const _i : Integer);
begin
	self.WriteLine(_i.ToString);
end;

procedure TStreamWriterHelper.WriteLineAsString(const _s : string; const _bAllowEmpty : boolean = false);
begin
	if (not _bAllowEmpty) and _s.IsEmpty then begin
		raise Exception.Create('Try to write empty string in stream.');
	end;
	self.WriteLine(_s);
end;

end.
