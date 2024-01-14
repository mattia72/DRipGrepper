unit RipGrepper.Common.Types;

interface

uses
	System.Classes;

const
	CR = #13;
	LF = #10;
	CRLF = sLineBreak;

type

	{$M+}
	INewLineEventHandler = interface
		['{A2EB8A24-0281-4AAA-BF91-210A95973652}']
		procedure OnNewOutputLine(const _sLine : string);
	end;

	ITerminateEventProducer = interface
		['{9C259E6F-CED7-41AD-B4F8-3A4BDD981965}']
		function ProcessShouldTerminate() : boolean;
	end;
	{$M-}

	TNewLineEventHandler = procedure(_obj : INewLineEventHandler; const _s : string);
	TTerminateEventProducer = function(_obj : ITerminateEventProducer) : boolean;

	TSortType = (stUnsorted, stAscending, stDescending);

	TParserType = (ptRipGrepSearch, ptRipGrepVersion, ptRipGrepSearchCutParent);

	IParser<T> = interface
		procedure ParseLineParseLine(var _m : T; const _s : string);
	end;

	TStringsHelper = class helper for TStrings
		function Contains(const s : string) : Boolean;
	end;

function PostInc(var Value : Integer) : Integer;
function PreInc(var Value : Integer) : Integer;

implementation

function PostInc(var Value : Integer) : Integer;
begin
	Result := Value;
	inc(Value);
end;

function PreInc(var Value : Integer) : Integer;
begin
	inc(Value);
	Result := Value;
end;

function TStringsHelper.Contains(const s : string) : Boolean;
begin
	Result := self.IndexOf(s) <> -1;
end;

end.
