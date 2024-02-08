unit RipGrepper.Helper.Types;

interface

uses
	System.Diagnostics,
	System.TimeSpan,
	System.Classes;

type

	TStringsHelper = class Helper for TStrings
		function Contains(const s : string) : Boolean;
		function TryGetDef(const _index : Integer; out _val : string; const _default : string = '') : Boolean;
		function GetValues(_sName : string = '') : TArray<string>;
	end;

function GetElapsedTime(const _swStart : TStopwatch) : string;

function PostInc(var Value : Integer) : Integer;
function PreInc(var Value : Integer) : Integer;

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

function TStringsHelper.GetValues(_sName : string = '') : TArray<string>;
begin
	for var s in self do begin
		var val := s.Remove(0, s.IndexOf('=') + 1);
		if _sName.IsEmpty then begin
			Result := Result + [val];
		end else begin
			if s.StartsWith(_sName + '=') then begin
				Result := Result + [val];
			end;
		end;
	end;
end;

function TStringsHelper.TryGetDef(const _index : Integer; out _val : string; const _default : string = '') : Boolean;
begin
	Result := _index < self.Count;
	if (Result) then begin
		_val := self[_index];
	end else begin
		_val := _default;
	end;
end;

end.
