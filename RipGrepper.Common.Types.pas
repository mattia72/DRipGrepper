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

	IEOFProcessEventHandler = interface
		['{E6AC51D8-9705-4A56-902B-494B1EC11184}']
		procedure OnEOFProcess();
	end;


	{$M-}
	// TNewLineEventHandler = procedure(_obj : INewLineEventHandler; const _s : string);
	// TTerminateEventProducer = function(_obj : ITerminateEventProducer) : boolean;

	TSortType = (stUnsorted, stAscending, stDescending);
 	TParserType = (ptRipGrepSearch, ptRipGrepVersion, ptRipGrepError, ptRipGrepHelp);
    TFileNameType = (ftAbsolute, ftRelative);

	TStringsHelper = class helper for TStrings
		function Contains(const s : string) : Boolean;
	end;

function PostInc(var Value : Integer) : Integer;
function PreInc(var Value : Integer) : Integer;

function GetElapsedTimeInSeconds(const _dtStart : TDateTime) : TDateTime;

function GetElapsedTime(const _dtStart : TDateTime) : string;

implementation

uses
	System.SysUtils;

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

function GetElapsedTimeInSeconds(const _dtStart : TDateTime) : TDateTime;
begin
	Result := (Now - _dtStart) * 24 * 60 * 60;
end;

function GetElapsedTime(const _dtStart : TDateTime) : string;
begin
	Result := Format('%.2f', [GetElapsedTimeInSeconds(_dtStart)]);
end;

function TStringsHelper.Contains(const s : string) : Boolean;
begin
	Result := self.IndexOf(s) <> -1;
end;

end.
