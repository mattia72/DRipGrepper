unit RipGrepperMatches;

interface

uses
	System.Generics.Collections, System.Classes;

type
	TRipGrepMatch = record
		FileName: string;
		Row: integer;
		Col: integer;
		Text: string;
    GroupId: integer;
	private
		IsError: Boolean;
		procedure SetError(const _sLine: string);
	public
		procedure ParseLine(const _sLine: string);
	end;

  TRipGrepperMatchCollection =  TList<TRipGrepMatch>;
	TRipGrepperMatches = class
    Matches : TRipGrepperMatchCollection;
    Groups : TStringList;
	private
		function GetCount: Integer;
	public
		constructor Create;
		destructor Destroy; override;
		procedure Clear;
		property Count: Integer read GetCount;
  end;

implementation

uses
	System.SysUtils;

procedure TRipGrepMatch.ParseLine(const _sLine: string);
var
	iPosMatch: integer;
	iPosRow: integer;
	iPosCol: integer;
begin
	iPosRow := Pos(':', _sLine, 3);
	if iPosRow <> 0 then begin
		// TDebugUtils.DebugMessage(_sLine);
		FileName := _sLine.Substring(0, iPosRow - 1);
		iPosCol := Pos(':', _sLine, iPosRow + 1);
		Row := StrToIntDef(_sLine.Substring(iPosRow, iPosCol - iPosRow - 1), 0);
		if Row = 0 then begin
			SetError(_sLine);
			exit;
		end;
		iPosMatch := Pos(':', _sLine, iPosCol + 1);
		Col := StrToIntDef(_sLine.Substring(iPosCol, iPosMatch - iPosCol - 1), 0);
		if Col = 0 then begin
			SetError(_sLine);
			exit;
		end;
		Text := _sLine.Substring(iPosMatch);
		IsError := False;
	end else begin
		SetError(_sLine);
	end;
end;

procedure TRipGrepMatch.SetError(const _sLine: string);
begin
	FileName := '';
	Text := _sLine;
	IsError := True;
end;

constructor TRipGrepperMatches.Create;
begin
	inherited;
	Groups := TStringList.Create(TDuplicates.dupIgnore, True, True);
  Matches := TRipGrepperMatchCollection.Create;
end;

destructor TRipGrepperMatches.Destroy;
begin
	inherited;
  Groups.Free;
  Matches.Free;
end;

procedure TRipGrepperMatches.Clear;
begin
  Matches.Clear;
  Groups.Clear;
end;

function TRipGrepperMatches.GetCount: Integer;
begin
	Result := Matches.Count;
end;

end.
