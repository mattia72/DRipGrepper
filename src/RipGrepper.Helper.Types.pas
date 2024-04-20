unit RipGrepper.Helper.Types;

interface

uses
	System.Diagnostics,
	System.TimeSpan,
	System.Classes;

type

	TStringsHelper = class Helper for TStrings
		private
		public
			function Contains(const s : string) : Boolean;
			function TryGetDef(const _index : Integer; out _val : string; const _default : string = '') : Boolean;
			function GetValues(_sName : string = '') : TArray<string>;

			function AddIfNotContains(const _sValue : string) : Boolean;
			function HasMatch(const _sRegEx : string) : Boolean;
			function DeleteAll(const _arr : TArray<string>) : Integer;
			function IndexOfAny(const _arr : TArray<string>) : Integer;
			function ContainsAny(const _arr : TArray<string>) : Boolean;
			function DeleteAllMatched(const _sRegEx : string) : integer;
			function IndexOfAllMatch(const _sRegEx : string) : TArray<integer>;
			function IndexOfFirstMatch(const _sRegEx : string) : Integer;
	end;

	TBitField = record
		Value : integer;

		// Check if the bit at _idx position is 1 (true) or 0 (false)
		function IsBitSet(const _idx : Integer) : Boolean;
		// Checks 0 and 1 in the array on the appropriate index. Any other value is not relevant.
		function IsEqual(const _arr : TArray<integer>) : Boolean;
		// set the bit at _idx position to 0
		function ResetBit(const _idx : Integer) : Integer;
		// set the bit at _idx position to 1
		function SetBit(const _idx : Integer) : Integer;

		public
			class operator Initialize(out Dest : TBitField);
	end;

function GetElapsedTime(const _swStart : TStopwatch) : string;

function PostInc(var Value : Integer) : Integer;
function PreInc(var Value : Integer) : Integer;

implementation

uses
	System.SysUtils,
	System.RegularExpressions;

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

function TStringsHelper.AddIfNotContains(const _sValue : string) : Boolean;
begin
	Result := not self.Contains(_sValue);
	if Result then begin
		self.Add(_sValue);
	end;
end;

function TStringsHelper.Contains(const s : string) : Boolean;
begin
	Result := self.IndexOf(s) <> -1;
end;

function TStringsHelper.HasMatch(const _sRegEx : string) : Boolean;
begin
	Result := False;
	for var s in self do begin
		if TRegEx.IsMatch(s, _sRegex) then begin
			Result := True;
			Exit
		end;
	end;
end;

function TStringsHelper.DeleteAll(const _arr : TArray<string>) : Integer;
var
	iFoundIdx : Integer;
begin
	Result := 0;
	repeat
		iFoundIdx := self.IndexOfAny(_arr);
		if (iFoundIdx < 0) then
			break;
		self.Delete(iFoundIdx);
		Inc(Result);
 	until False;
end;

function TStringsHelper.GetValues(_sName : string = '') : TArray<string>;
begin
	Result := [];
	for var s in self do begin
		var
		val := s.Remove(0, s.IndexOf('=') + 1);
		if _sName.IsEmpty then begin
			Result := Result + [val];
		end else begin
			if s.StartsWith(_sName + '=') then begin
				Result := Result + [val];
			end;
		end;
	end;
end;

function TStringsHelper.IndexOfAny(const _arr : TArray<string>) : Integer;
begin
	Result := -1;
	for var p in _arr do begin
		Result := self.IndexOf(p);
		if (Result >= 0) then begin
			break; // Already has
		end;
	end;
end;

function TStringsHelper.ContainsAny(const _arr : TArray<string>) : Boolean;
begin
	Result := 0 <= IndexOfAny(_arr);
end;

function TStringsHelper.DeleteAllMatched(const _sRegEx : string) : integer;
var
	iFoundIdx : Integer;
begin
	Result := 0;
	repeat
		iFoundIdx := self.IndexOfFirstMatch(_sRegex);
		if (iFoundIdx < 0) then
			break;
		self.Delete(iFoundIdx);
	until False;
end;

function TStringsHelper.IndexOfAllMatch(const _sRegEx : string) : TArray<integer>;
begin
	Result := [];
	for var i := 0 to self.Count - 1 do begin
		if TRegEx.IsMatch(self[i], _sRegex) then begin
			Result := Result + [i];
		end;
	end;
end;

function TStringsHelper.IndexOfFirstMatch(const _sRegEx : string) : Integer;
begin
	Result := -1;
	for var i := 0 to self.Count - 1 do begin
		if TRegEx.IsMatch(self[i], _sRegex) then begin
			Result := i;
			break;
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

// Check if the bit at _idx position is 1 (true) or 0 (false)
function TBitField.IsBitSet(const _idx : Integer) : Boolean;
begin
	Result := Value and (1 shl _idx) <> 0;
end;

// Checks 0 and 1 in the array on the appropriate index. Any other value is not relevant.
function TBitField.IsEqual(const _arr : TArray<integer>) : Boolean;
begin
	Result := True;
	for var i : integer := 0 to high(_arr) do begin
		case _arr[i] of
			0 : begin
				if IsBitSet(i) then begin
					Result := False;
					break;
				end;
			end;
			1 : begin
				if not IsBitSet(i) then begin
					Result := False;
					break;
				end;
			end;
			else
			continue
		end;
	end;
end;

// set the bit at _idx position to 0
function TBitField.ResetBit(const _idx : Integer) : Integer;
begin
	Value := Value and (not(1 shl _idx));
	Result := Value;
end;

// set the bit at _idx position to 1
function TBitField.SetBit(const _idx : Integer) : Integer;
begin
	Value := Value or (1 shl _idx);
	Result := Value;
end;

class operator TBitField.Initialize(out Dest : TBitField);
begin
	Dest.Value := 0;
end;

end.
