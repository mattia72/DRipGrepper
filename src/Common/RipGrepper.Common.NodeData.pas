unit RipGrepper.Common.NodeData;

interface

type
	TVSMatchData = record
		Row : integer;
		ColBegin : integer;
		LineText : string;

		private
			FColEnd : Integer;
			procedure SetColEnd(const Value : Integer);

		public
			function GetMatchLength() : integer;
			function IsEmpty : Boolean;
			class function New(_row, _col, _colEnd : Integer; _matchText : string) : TVSMatchData; static;
			class operator Initialize(out Dest : TVSMatchData);
			function ToString() : string;
			property ColEnd : Integer read FColEnd write SetColEnd;
	end;

	TVSFileNodeData = record
		FilePath : string;
		// Icon?
		MatchData : TVSMatchData;
		function GetLineText(const _bTrimLeft : Boolean; var _iSpaceCount, _iTabCount : Integer) : string;

		public
			class function New(const _file : string; const _row, _colBegin, _colEnd : Integer; const _textBefore : string;
				_matchText, _textAfter : string) : TVSFileNodeData; overload; static;
			class function New(const _file : string; const _row : Integer = -1; const _colBegin : Integer = -1;
				const _colEnd : Integer = -1; const _matchText : string = '') : TVSFileNodeData; overload; static;
	end;

	PVSMatchData = ^TVSMatchData;
	PVSFileNodeData = ^TVSFileNodeData;

implementation

uses
	System.SysUtils,
	RipGrepper.Helper.Types;

function TVSFileNodeData.GetLineText(const _bTrimLeft : Boolean; var _iSpaceCount, _iTabCount : Integer) : string;
var
	sTrimmed : string;
begin
	_iSpaceCount := 0;
	_iTabCount := 0;
	if _bTrimLeft then begin
		sTrimmed := MatchData.LineText.TrimLeft();
		for var ch in MatchData.LineText.ToCharArray() do begin
			if ch = ' ' then begin
				PostInc(_iSpaceCount);
			end else if ch = #9 then begin
				PostInc(_iTabCount);
			end else begin
				break;
			end;
		end;
		Result := sTrimmed;
	end else begin
		Result := MatchData.LineText;
	end;

end;

class function TVSFileNodeData.New(const _file : string; const _row, _colBegin, _colEnd : Integer; const _textBefore : string;
	_matchText, _textAfter : string) : TVSFileNodeData;
var
	text : string;
begin
	Result.FilePath := _file;
	text := _textBefore + _matchText + _textAfter;
	Result.MatchData := TVSMatchData.New(_row, _colBegin, _colEnd, text);
end;

class function TVSFileNodeData.New(const _file : string; const _row : Integer = -1; const _colBegin : Integer = -1;
	const _colEnd : Integer = -1; const _matchText : string = '') : TVSFileNodeData;
begin
	Result.FilePath := _file;
	Result.MatchData := TVSMatchData.New(_row, _colBegin, _colEnd, _matchText);
end;

function TVSMatchData.GetMatchLength() : integer;
begin
	// Add validation to prevent negative lengths in 32-bit Release builds
	if FColEnd < ColBegin then begin
		{$IFDEF DEBUG}
		TDebugUtils.MsgFmt('Warning: ColEnd (%d) < ColBegin (%d) in GetMatchLength', [FColEnd, ColBegin]);
		{$ENDIF}
		Result := 0; // Return safe value instead of negative
	end else begin
		Result := FColEnd - ColBegin;
	end;
end;

function TVSMatchData.IsEmpty : Boolean;
begin
	Result := (Row = -1) and (ColEnd = -1) and (ColBegin = -1) and (LineText.IsEmpty);
end;

class function TVSMatchData.New(_row, _col, _colEnd : Integer; _matchText : string) : TVSMatchData;
begin
	Result.Row := _row;
	Result.ColBegin := _col;
	Result.ColEnd := _colEnd;
	Result.LineText := _matchText;
end;

procedure TVSMatchData.SetColEnd(const Value : Integer);
begin
	{$IFDEF DEBUG}
	if Value < ColBegin then begin
		TDebugUtils.MsgFmt('Warning: Setting ColEnd (%d) < ColBegin (%d)', [Value, ColBegin]);
	end;
	{$ENDIF}
	FColEnd := Value;
end;

function TVSMatchData.ToString() : string;
begin
	Result := Format('Raw Text: "%s" (R:%d|C:%d|Length: %d): "%s"', [LineText, Row, ColBegin, GetMatchLength,
		LineText.Substring(ColBegin - 1, GetMatchLength)]);
end;

class operator TVSMatchData.Initialize(out Dest : TVSMatchData);
begin
	Dest.Row := -1;
	Dest.ColBegin := -1;
	Dest.ColEnd := -1;
	Dest.LineText := '';
end;

end.
