unit RipGrepper.Data.Matches;

interface

uses
	System.Generics.Collections,
	System.Classes,
	RipGrepper.Common.Types,
	System.RegularExpressions;

type

	TRipGrepMatch = record
		FileName : string;
		Row : integer;
		Col : integer;
		Text : string;
		GroupId : integer;
		IsError : Boolean;
		ErrorText : string;
		RecId : integer;

		private
			FRgResultLineParseRegex : TRegex;
			procedure SetRgResultLineParseError(const _sLine : string);

		public
			procedure ParseLine(const _sLine : string);
			function Validate : Boolean;
			function ValidatePath : Boolean;
			class operator Finalize(var Dest : TRipGrepMatch);
			class operator Initialize(out Dest : TRipGrepMatch);
	end;

	TRipGrepperMatchCollection = TList<TRipGrepMatch>;

	TRipGrepperMatches = class
		Matches : TRipGrepperMatchCollection;
		MatchFiles : TStringList;

		private
			function GetCount : Integer;

		public
			constructor Create;
			destructor Destroy; override;
			procedure Clear;
			procedure SortByFileName(_bDescending : Boolean = False);
			procedure SortByRow(_bDescending : Boolean = False);
			procedure SortByRecID(_bDescending : Boolean = False);
			property Count : Integer read GetCount;
	end;

implementation

uses
	System.SysUtils,
	System.Generics.Defaults,

	System.IOUtils,
	Vcl.Dialogs,
	RipGrepper.Tools.DebugTools;

const
	RG_RESULT_LINE_PARSE_REGEX = '^(\w:)?(.+?):(\d+):(\d+):(.+)$';

function TRipGrepMatch.ValidatePath : Boolean;
begin
	if FileName.StartsWith(':') then begin
		ErrorText := 'Begins with '':''';
	end else if not TPath.HasValidPathChars(FileName, False) then begin
		ErrorText := 'Invalid chars in path' + FileName;
	end else if not(TPath.IsDriveRooted(FileName) or TPath.IsRelativePath(FileName)) then begin
		ErrorText := 'Not an abs or relative path' + FileName;
	end;
	Result := ErrorText = '';
end;

procedure TRipGrepMatch.ParseLine(const _sLine : string);
var
	m : TMatch;
begin
	m := FRgResultLineParseRegex.Match(_sLine);
	if m.Success then begin
		// TDebugUtils.DebugMessage(_sLine);
		FileName := m.Groups[1].Value + m.Groups[2].Value;
		Row := StrToIntDef(m.Groups[3].Value, -1);
		Col := StrToIntDef(m.Groups[4].Value, -1);
		Text := m.Groups[5].Value;
		IsError := not Validate;
	end else begin
		SetRgResultLineParseError(_sLine);
	end;

	if (IsError) then begin
		TDebugUtils.DebugMessage('Error parsing line: ' + CRLF +
			{ } _sLine + CRLF +
			{ } 'File: ' + FileName + CRLF +
			{ } 'Row: ' + Row.ToString + CRLF +
			{ } 'Col: ' + Col.ToString + CRLF +
			{ } 'Text: ' + Text + CRLF +
			{ } 'ErrorText: ' + ErrorText + CRLF);

	end;
end;

procedure TRipGrepMatch.SetRgResultLineParseError(const _sLine : string);
begin
	FileName := '';
	Text := _sLine;
	ErrorText := 'rg.exe result line couldn''t parsed.';
	IsError := True;
end;

function TRipGrepMatch.Validate : Boolean;
begin
	Result := False;
	IsError := not ValidatePath();
	if IsError then
		Exit;

	IsError := IsError and (Row > 0);
	if IsError then begin
		ErrorText := 'Invalid Row:' + Row.ToString;
		Exit;
	end;
	IsError := IsError and (Col > 0);
	if IsError then begin
		ErrorText := 'Invalid Col:' + Col.ToString;
		Exit
	end;
	// GroupId?
	Result := True;
end;

class operator TRipGrepMatch.Finalize(var Dest : TRipGrepMatch);
begin
	// Dest.FRgResultLineParseRegex;
end;

class operator TRipGrepMatch.Initialize(out Dest : TRipGrepMatch);
begin
	Dest.RecId := 0;
	Dest.FileName := '';
	Dest.Row := -1;
	Dest.Col := -1;
	Dest.IsError := True;
	Dest.GroupId := -1;
	Dest.Text := '';
	Dest.ErrorText := '';
	Dest.FRgResultLineParseRegex := TRegex.Create(RG_RESULT_LINE_PARSE_REGEX);
end;

constructor TRipGrepperMatches.Create;
begin
	inherited;
	MatchFiles := TStringList.Create(TDuplicates.dupIgnore, True, True);
	Matches := TRipGrepperMatchCollection.Create;
end;

destructor TRipGrepperMatches.Destroy;
begin
	inherited;
	MatchFiles.Free;
	Matches.Free;
end;

procedure TRipGrepperMatches.Clear;
begin
	Matches.Clear;
	MatchFiles.Clear;
end;

function TRipGrepperMatches.GetCount : Integer;
begin
	Result := Matches.Count;
end;

procedure TRipGrepperMatches.SortByFileName(_bDescending : Boolean = False);
begin
	Matches.Sort(TComparer<TRipGrepMatch>.Construct(
		function(const Left, Right : TRipGrepMatch) : Integer
		begin
			if _bDescending then begin
				Result := -TComparer<string>.Default.Compare(Left.FileName, Right.FileName);
			end else begin
				Result := TComparer<string>.Default.Compare(Left.FileName, Right.FileName);
			end;
		end));
end;

procedure TRipGrepperMatches.SortByRow(_bDescending : Boolean = False);
begin
	Matches.Sort(TComparer<TRipGrepMatch>.Construct(
		function(const Left, Right : TRipGrepMatch) : Integer
		begin
			if _bDescending then begin
				Result := -TComparer<integer>.Default.Compare(Left.Row, Right.Row);
			end else begin
				Result := TComparer<integer>.Default.Compare(Left.Row, Right.Row);
			end;
		end));
end;

procedure TRipGrepperMatches.SortByRecID(_bDescending : Boolean = False);
begin
	Matches.Sort(TComparer<TRipGrepMatch>.Construct(
		function(const Left, Right : TRipGrepMatch) : Integer
		begin
			if _bDescending then begin
				Result := -TComparer<integer>.Default.Compare(Left.RecId, Right.RecId);
			end else begin
				Result := TComparer<integer>.Default.Compare(Left.RecId, Right.RecId);
			end;
		end));
end;

end.
