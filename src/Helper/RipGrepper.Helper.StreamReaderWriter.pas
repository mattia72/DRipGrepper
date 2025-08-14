unit RipGrepper.Helper.StreamReaderWriter;

interface

uses
	System.Classes;

type
	TStreamReaderHelper = class Helper for TStreamReader
		public
			function ReadLineAsBool(const _description : string = '') : Boolean;
			function ReadLineAsInteger(const _description : string = '') : Integer;
			function ReadLineAsString(const _bAllowEmpty : boolean = false; const _description : string = '') : string;
	end;

type
	TStreamWriterHelper = class Helper for TStreamWriter
		public
			procedure WriteLineAsBool(const _b : Boolean);
			procedure WriteLineAsInteger(const _i : Integer);
			procedure WriteLineAsString(const _s : string; const _bAllowEmpty : boolean =
				false; const _description : string = '');
	end;

implementation

uses
	System.SysUtils;

function TStreamReaderHelper.ReadLineAsBool(const _description : string = '') : Boolean;
var
	s : string;
begin
	s := self.ReadLineAsString(false, _description);  // Boolean values should not be empty
	Result := s <> '0';
end;

function TStreamReaderHelper.ReadLineAsInteger(const _description : string = '') : Integer;
var
	s : string;
begin
	s := self.ReadLineAsString(false, _description);  // Integer values should not be empty
	Result := StrToIntDef(s, 0);
end;

function TStreamReaderHelper.ReadLineAsString(const _bAllowEmpty : boolean = false; const _description : string = '') : string;
begin
	Result := self.ReadLine();
	if (not _bAllowEmpty) and Result.IsEmpty then begin
		if _description.IsEmpty then begin
			raise Exception.Create('Try to read empty string from stream.');
		end else begin
			raise Exception.Create('Try to read empty string from stream for: ' + _description);
		end;
	end;
end;

procedure TStreamWriterHelper.WriteLineAsBool(const _b : Boolean);
begin
	self.WriteLine(BoolToStr(_b));
end;

procedure TStreamWriterHelper.WriteLineAsInteger(const _i : Integer);
begin
	self.WriteLine(_i.ToString);
end;

procedure TStreamWriterHelper.WriteLineAsString(const _s : string; const _bAllowEmpty : boolean = false; const _description : string = '');
begin
	if (not _bAllowEmpty) and _s.IsEmpty then begin
		raise Exception.Create('Try to write empty string in stream for: ' + _description);
	end;
	self.WriteLine(_s);
end;

end.
