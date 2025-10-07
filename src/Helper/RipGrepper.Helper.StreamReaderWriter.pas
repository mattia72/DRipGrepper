unit RipGrepper.Helper.StreamReaderWriter;

interface

uses
	System.Classes;

type
	TStreamReaderHelper = class Helper for TStreamReader
		public
			function ReadLineAsBool(const _description: string): Boolean;
			function ReadLineAsInteger(const _description: string): Integer;
			function ReadLineAsString(const _bAllowEmpty : boolean = false; const _description : string = '') : string;
	end;

type
	TStreamWriterHelper = class Helper for TStreamWriter
		public
			procedure WriteLineAsBool(const _b : Boolean; const _description : string);
			procedure WriteLineAsInteger(const _i : Integer; const _description : string);
			procedure WriteLineAsString(const _s : string; const _bAllowEmpty : boolean; const _description : string);
	end;

implementation

uses
	System.SysUtils,
	RipGrepper.Tools.DebugUtils;

function TStreamReaderHelper.ReadLineAsBool(const _description: string):
	Boolean;
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

function TStreamReaderHelper.ReadLineAsInteger(const _description: string):
	Integer;
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
	self.WriteLine(_s);
end;

end.
