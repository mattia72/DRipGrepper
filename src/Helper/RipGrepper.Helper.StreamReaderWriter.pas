unit RipGrepper.Helper.StreamReaderWriter;

interface

uses
  System.Classes;

type
	TStreamReaderHelper = class helper for TStreamReader

	private
	public
		function ReadLineAsBool(): Boolean;
		function ReadLineAsInteger(): Integer;
	end;

implementation

uses
  System.SysUtils;

function TStreamReaderHelper.ReadLineAsBool(): Boolean;
var
	s : string;
begin
	s := self.ReadLine();
	Result := s <> '0';
end;

function TStreamReaderHelper.ReadLineAsInteger(): Integer;
var
	s : string;
begin
	s := self.ReadLine();
	Result := StrToInt(s);
end;

end.
