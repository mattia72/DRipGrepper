unit RipGrepper.Common.NodeData;

interface

type
	TVSMatchData = record
		Row : integer;
		Col : integer;
		MatchLength : integer;
		LineText : string;

		public
			function IsEmpty : Boolean;
			class function New(_row, _col, _colEnd : Integer; _matchText : string) : TVSMatchData; static;
			class operator Initialize(out Dest : TVSMatchData);
	end;

	TVSFileNodeData = record
		FilePath : string;
		// Icon?
		MatchData : TVSMatchData;
		function GetLineText(const _bTrimLeft : Boolean; var _iSpaceCount, _iTabCount : Integer) : string;

		public
			class function New(_file : string; _row, _col : Integer; _textBefore, _matchText, _textAfter : string) : TVSFileNodeData;
				overload; static;
			class function New(_file : string; _row : Integer = -1; _col : Integer = -1; _matchText : string = '') : TVSFileNodeData;
				overload; static;
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

class function TVSFileNodeData.New(_file : string; _row, _col : Integer; _textBefore, _matchText, _textAfter : string) : TVSFileNodeData;
var
	matchLength : integer;
	text : string;
begin
	Result.FilePath := _file;
	text := _textBefore + _matchText + _textAfter;
	matchLength := Length(_matchText);
	Result.MatchData := TVSMatchData.New(_row, _col, matchLength, text);
end;

class function TVSFileNodeData.New(_file : string; _row : Integer = -1; _col : Integer = -1; _matchText : string = '') : TVSFileNodeData;
begin
	Result.FilePath := _file;
	Result.MatchData := TVSMatchData.New(_row, _col, -1, _matchText);
end;

function TVSMatchData.IsEmpty : Boolean;
begin
	Result := (Row = -1) and (Col = -1) and (MatchLength = -1) and (LineText.IsEmpty);
end;

class function TVSMatchData.New(_row, _col, _colEnd : Integer; _matchText : string) : TVSMatchData;
begin
	Result.Row := _row;
	Result.Col := _col;
	Result.MatchLength := _colEnd;
	Result.LineText := _matchText;
end;

class operator TVSMatchData.Initialize(out Dest : TVSMatchData);
begin
	Dest.Row := -1;
	Dest.Col := -1;
	Dest.MatchLength := -1;
	Dest.LineText := '';
end;

end.
