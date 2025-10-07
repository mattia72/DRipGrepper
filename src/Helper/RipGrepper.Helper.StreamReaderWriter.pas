unit RipGrepper.Helper.StreamReaderWriter;

interface

uses
	System.Classes;

type
	TStreamReaderHelper = class Helper for TStreamReader
		private
			function DecodeEscapedString(const input: string): string;
		public
			function ReadLineAsBool(const _description : string) : Boolean;
			function ReadLineAsInteger(const _description : string) : Integer;
			function ReadLineAsString(const _bAllowEmpty : boolean = false; const _description : string = '') : string;
	end;

type
	TStreamWriterHelper = class Helper for TStreamWriter
		private
			function EncodeEscapedString(const input: string): string;
		public
			procedure WriteLineAsBool(const _b : Boolean; const _description : string);
			procedure WriteLineAsInteger(const _i : Integer; const _description : string);
			procedure WriteLineAsString(const _s : string; const _bAllowEmpty : boolean; const _description : string);
	end;

implementation

uses
	System.SysUtils,
	RipGrepper.Tools.DebugUtils,
	RipGrepper.Common.Constants;

{ TStreamWriterHelper }

function TStreamWriterHelper.EncodeEscapedString(const input: string): string;
var
	i: Integer; 
	ch: Char;
begin
	Result := '';
	for i := 1 to Length(input) do begin
		ch := input[i];
		case ch of
			'\': Result := Result + '\\';  // \ -> \\
			CR:  Result := Result + '\r';  // CR -> \r
			LF:  Result := Result + '\n';  // LF -> \n  
			#9:  Result := Result + '\t';  // TAB -> \t
		else
			Result := Result + ch;         // All other characters as-is
		end;
	end;
end;

function TStreamReaderHelper.ReadLineAsBool(const _description : string) : Boolean;
var
	s : string;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TStreamReaderHelper.ReadLineAsBool');
	dbgMsg.Msg('_description=' + _description);
	s := self.ReadLineAsString(false, _description); // Boolean values should not be empty
	dbgMsg.Msg('s=' + s);
	Result := s <> '0';
end;

function TStreamReaderHelper.ReadLineAsInteger(const _description : string) : Integer;
var
	s : string;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TStreamReaderHelper.ReadLineAsInteger');
	dbgMsg.Msg('_description=' + _description);
	s := self.ReadLineAsString(false, _description); // Integer values should not be empty
	dbgMsg.Msg('s=' + s);
	Result := StrToIntDef(s, 0);
end;

function TStreamReaderHelper.ReadLineAsString(const _bAllowEmpty : boolean = false; const _description : string = '') : string;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TStreamReaderHelper.ReadLineAsString');
	dbgMsg.Msg('_description=' + _description);
	Result := self.ReadLine();
	if (not _bAllowEmpty) and Result.IsEmpty then begin
		var
		msg := 'Try to read empty string from stream for: ' + _description;
		dbgMsg.ErrorMsg(msg);
		raise Exception.Create(msg);
	end;

	// Decode using character-by-character processing to avoid conflicts
	Result := self.DecodeEscapedString(Result);

	dbgMsg.Msg('Result=' + Result);
end;

procedure TStreamWriterHelper.WriteLineAsBool(const _b : Boolean; const _description : string);
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TStreamWriterHelper.WriteLineAsBool');
	dbgMsg.MsgFmt('%s = %s', [_description, BoolToStr(_b)]);
	self.WriteLine(BoolToStr(_b));
end;

procedure TStreamWriterHelper.WriteLineAsInteger(const _i : Integer; const _description : string);
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TStreamWriterHelper.WriteLineAsInteger');
	dbgMsg.MsgFmt('%s = %d', [_description, _i]);
	self.WriteLine(_i.ToString);
end;

procedure TStreamWriterHelper.WriteLineAsString(const _s : string; const _bAllowEmpty : boolean; const _description : string);
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TStreamWriterHelper.WriteLineAsString');
	dbgMsg.MsgFmt('%s = %s', [_description, _s]);
	if (not _bAllowEmpty) and _s.IsEmpty then begin
		var
		msg := 'Try to write empty string in stream for: ' + _description;
		dbgMsg.ErrorMsg(msg);
		raise Exception.Create(msg);
	end;

	// Encode using character-by-character processing to avoid conflicts
	var encodedString := self.EncodeEscapedString(_s);
	self.WriteLine(encodedString);
end;

{ TStreamReaderHelper }

function TStreamReaderHelper.DecodeEscapedString(const input: string): string;
var
	i: Integer;
	ch: Char;
begin
	Result := '';
	i := 1;
	while i <= Length(input) do begin
		ch := input[i];
		if (ch = '\') and (i < Length(input)) then begin
			// Check the next character after backslash
			case input[i + 1] of
				'\': begin Result := Result + '\'; Inc(i, 2); end;	// \\ -> \
				'r': begin Result := Result + CR; Inc(i, 2); end;	// \r -> CR
				'n': begin Result := Result + LF; Inc(i, 2); end;	// \n -> LF
				't': begin Result := Result + #9; Inc(i, 2); end;	// \t -> TAB
			else
				// Not a recognized escape sequence, keep the backslash
				Result := Result + ch;
				Inc(i);
			end;
		end else begin
			// Regular character, just add it
			Result := Result + ch;	
			Inc(i);
		end;
	end;
end;

end.
