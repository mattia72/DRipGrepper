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
			procedure WriteLineAsInteger(const _i: Integer);
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
	Result := StrToIntDef(s, 0);
end;

procedure TStreamWriterHelper.WriteLineAsBool(const _b : Boolean);
begin
	self.WriteLine(BoolToStr(_b));
end;

procedure TStreamWriterHelper.WriteLineAsInteger(const _i: Integer);
begin
	self.WriteLine(_i.ToString);
end;

end.
