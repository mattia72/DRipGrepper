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
		['{2AB0567A-EDAB-4FDC-904B-E3F124568978}']
		procedure OnNewOutputLine(const _sLine : string);
	end;
	{$M-}

	TNewLineEventHandler = procedure(_obj : INewLineEventHandler; const _s : string);
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
