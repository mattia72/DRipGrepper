unit RipGrepper.Helper.Types;

interface

uses
	System.Diagnostics,
	System.TimeSpan,
	System.Classes;

type

	TStringsHelper = class Helper for TStrings
		function Contains(const s : string) : Boolean;
	end;

function GetElapsedTime(const _swStart : TStopwatch) : string;

function PostInc(var Value : Integer): Integer;
function PreInc(var Value : Integer): Integer;

implementation

uses
	System.SysUtils;

function PostInc(var Value : Integer): Integer;
begin
	Result := Value;
	inc(Value);
end;

function PreInc(var Value : Integer): Integer;
begin
	inc(Value);
	Result := Value;
end;

function GetElapsedTime(const _swStart : TStopwatch) : string;
var
	e : TTimeSpan;
begin
	e := _swStart.Elapsed;
	Result := Format('%d.%.3d', [e.Seconds, e.Milliseconds]);
end;

function TStringsHelper.Contains(const s : string) : Boolean;
begin
	Result := self.IndexOf(s) <> -1;
end;

end.
