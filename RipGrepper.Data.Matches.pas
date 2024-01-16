unit RipGrepper.Data.Matches;

interface

uses
	System.Generics.Collections,
	System.Classes,
	RipGrepper.Common.Types,
	System.RegularExpressions;

type
	ILine = interface
		function GetLineNr : Integer;
		procedure SetLineNr(const Value : Integer);
		property LineNr : Integer read GetLineNr write SetLineNr;
	end;

	ILineParser = interface(ILine)
		function GetIsError : Boolean;
		function GetLineParseRegex : TRegex;
		procedure SetLineParseRegex(const Value : TRegex);
		function GetParserType : TParserType;
		procedure ParseLine(const _iLnNr : integer; const _s : string);
		procedure SetIsError(const Value : Boolean);
		procedure SetParserType(const Value : TParserType);
		property LineParseRegex : TRegex read GetLineParseRegex write SetLineParseRegex;

		property IsError : Boolean read GetIsError write SetIsError;
		property ParserType : TParserType read GetParserType write SetParserType;
	end;

	TRipGrepOutputLine = class(TInterfacedObject, ILineParser)
		private
			FIsError : Boolean;
			FLineNr : Integer;
			FLineParseRegex : TRegex;
			FParserType : TParserType;
			function GetIsError : Boolean;
			function GetParserType : TParserType;
			procedure SetIsError(const Value : Boolean);
			procedure SetParserType(const Value : TParserType);

		protected

			function GetLineParseRegex : TRegex;
			procedure SetLineParseRegex(const Value : TRegex);
			function GetLineNr : Integer;
			procedure SetLineNr(const Value : Integer);

		public

			procedure ParseLine(const _iLnNr : integer; const _s : string); virtual;
			property LineNr : Integer read GetLineNr write SetLineNr;
			property LineParseRegex : TRegex read GetLineParseRegex write SetLineParseRegex;
			property IsError : Boolean read GetIsError write SetIsError;
			property ParserType : TParserType read GetParserType write SetParserType;

	end;

	IRipGrepMatchLine = interface(ILineParser)
		function GetCol : Integer; stdcall;
		function GetFileName : string; stdcall;
		function GetRow : Integer; stdcall;
		function GetText : string; stdcall;
		procedure SetCol(const Value : Integer); stdcall;
		procedure SetFileName(const Value : string); stdcall;
		procedure SetRow(const Value : Integer); stdcall;
		procedure SetText(const Value : string); stdcall;
		property Col : Integer read GetCol write SetCol;
		property FileName : string read GetFileName write SetFileName;
		property Row : Integer read GetRow write SetRow;
		property Text : string read GetText write SetText;

	end;

	IRipGrepMatchLineGroup = interface(IRipGrepMatchLine)
		function GetGroupId : Integer;
		procedure SetGroupId(const Value : Integer);
		property GroupId : Integer read GetGroupId write SetGroupId;
	end;

	TRipGrepperMatchCollection = TList<IRipGrepMatchLineGroup>;

	TRipGrepMatch = class(TRipGrepOutputLine, IRipGrepMatchLineGroup)

		IsError : Boolean;
		ErrorText : string;
		function GetGroupId : Integer;
		procedure SetGroupId(const Value : Integer);

		private
			FCol : Integer;
			FFileName : string;
			FGroupId : Integer;
			FRow : Integer;
			FText : string;
			procedure SetRgResultLineParseError(const _sLine : string);

		protected
			function GetFileName : string; stdcall;
			procedure SetFileName(const Value : string); stdcall;
			function GetRow : Integer; stdcall;
			procedure SetRow(const Value : Integer); stdcall;
			function GetCol : Integer; stdcall;
			function GetText : string; stdcall;
			procedure SetCol(const Value : Integer); stdcall;
			procedure SetText(const Value : string); stdcall;

		public
			procedure ParseLine(const _iLnNr : integer; const _s : string); override;
			function Validate : Boolean;
			function ValidatePath : Boolean;
			destructor Destroy; override;
			constructor Create;
			property Col : Integer read GetCol write SetCol;
			property FileName : string read GetFileName write SetFileName;
			property GroupId : Integer read GetGroupId write SetGroupId;
			property Row : Integer read GetRow write SetRow;
			property Text : string read GetText write SetText;
	end;

	TSortByType = (sbtFile, sbtRow, sbtCol, sbtText);

	TRipGrepperMatches = class
		Matches : TRipGrepperMatchCollection;
		MatchFiles : TStringList;
		ItemGroups : TStringList;
		SortedBy : TList<TSortByType>;

		private
			function GetCount : Integer;

		public
			constructor Create;
			destructor Destroy; override;
			procedure Add(const _item : IRipGrepMatchLineGroup);
			procedure Clear;
			procedure SortBy(const _sbt : TSortByType; const _st : TSortType);
			procedure SortByFileName(_bDescending : Boolean = False);
			procedure SortByRow(_bDescending : Boolean = False);
			procedure SortByLineNr(_bDescending : Boolean = False);
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

procedure TRipGrepMatch.ParseLine(const _iLnNr : integer; const _s : string);
var
	m : TMatch;
begin
	inherited;

	m := FLineParseRegex.Match(_s);
	if m.Success then begin
		// TDebugUtils.DebugMessage(_s);
		FileName := m.Groups[1].Value + m.Groups[2].Value;
		Row := StrToIntDef(m.Groups[3].Value, -1);
		Col := StrToIntDef(m.Groups[4].Value, -1);
		Text := m.Groups[5].Value;
		IsError := not Validate;
	end else begin
		SetRgResultLineParseError(_s);
	end;

	if (IsError) then begin
		TDebugUtils.DebugMessage('Error parsing line: ' + CRLF +
			{ } _s + CRLF +
			{ } 'File: ' + FileName + CRLF +
			{ } 'Row: ' + Row.ToString + CRLF +
			{ } 'Col: ' + Col.ToString + CRLF +
			{ } 'Text: ' + Text + CRLF +
			{ } 'ErrorText: ' + ErrorText);
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

destructor TRipGrepMatch.Destroy;
begin
	// Dest.FLineParseRegex;
end;

constructor TRipGrepMatch.Create;
begin
	LineNr := 0;
	FileName := '';
	Row := -1;
	Col := -1;
	IsError := True;
	GroupId := -1;
	Text := '';
	ErrorText := '';
	FLineParseRegex := TRegex.Create(RG_RESULT_LINE_PARSE_REGEX);
end;

function TRipGrepMatch.GetCol : Integer;
begin
	Result := FCol;
end;

function TRipGrepMatch.GetFileName : string;
begin
	Result := FFileName;
end;

function TRipGrepMatch.GetGroupId : Integer;
begin
	Result := FGroupId;
end;

function TRipGrepMatch.GetRow : Integer;
begin
	Result := FRow;
end;

function TRipGrepMatch.GetText : string;
begin
	Result := FText;
end;

procedure TRipGrepMatch.SetCol(const Value : Integer);
begin
	FCol := Value;
end;

procedure TRipGrepMatch.SetFileName(const Value : string);
begin
	FFileName := Value;
end;

procedure TRipGrepMatch.SetGroupId(const Value : Integer);
begin
	FGroupId := Value;
end;

procedure TRipGrepMatch.SetRow(const Value : Integer);
begin
	FRow := Value;
end;

procedure TRipGrepMatch.SetText(const Value : string);
begin
	FText := Value;
end;

constructor TRipGrepperMatches.Create;
begin
	inherited;
	ItemGroups := TStringList.Create(TDuplicates.dupIgnore, True, True);
	MatchFiles := TStringList.Create(TDuplicates.dupIgnore, True, True);
	Matches := TRipGrepperMatchCollection.Create;
	SortedBy := TList<TSortByType>.Create;
end;

destructor TRipGrepperMatches.Destroy;
begin
	inherited;
	ItemGroups.Free;
	MatchFiles.Free;
	Matches.Free;
	SortedBy.Free;
end;

procedure TRipGrepperMatches.Add(const _item : IRipGrepMatchLineGroup);
begin
	Matches.Add(_item);
	MatchFiles.Add(_item.FileName);
end;

procedure TRipGrepperMatches.Clear;
begin
	Matches.Clear;
	ItemGroups.Clear;
	MatchFiles.Clear;
end;

function TRipGrepperMatches.GetCount : Integer;
begin
	Result := Matches.Count;
end;

procedure TRipGrepperMatches.SortBy(const _sbt : TSortByType; const _st : TSortType);
begin
	if _st <> stUnsorted then begin
		case _sbt of
			sbtFile : begin
				SortByFileName(_st = stDescending);
			end;
			sbtRow : begin
				SortByRow(_st = stDescending);
			end;
		end;
	end else begin
		SortByLineNr(_st = stDescending);
		SortedBy.Remove(_sbt);
	end;
end;

procedure TRipGrepperMatches.SortByFileName(_bDescending : Boolean = False);
begin
	Matches.Sort(TComparer<IRipGrepMatchLineGroup>.Construct(
		function(const Left, Right : IRipGrepMatchLineGroup) : Integer
		begin
			if _bDescending then begin
				Result := -TComparer<string>.Default.Compare(Left.FileName, Right.FileName);
			end else begin
				Result := TComparer<string>.Default.Compare(Left.FileName, Right.FileName);
			end;
		end));
	SortedBy.Add(sbtFile);
end;

procedure TRipGrepperMatches.SortByRow(_bDescending : Boolean = False);
begin
	Matches.Sort(TComparer<IRipGrepMatchLineGroup>.Construct(
		function(const Left, Right : IRipGrepMatchLineGroup) : Integer
		begin
			if _bDescending then begin
				Result := -TComparer<integer>.Default.Compare(Left.Row, Right.Row);
			end else begin
				Result := TComparer<integer>.Default.Compare(Left.Row, Right.Row);
			end;

		end));
	SortedBy.Add(sbtRow);
end;

procedure TRipGrepperMatches.SortByLineNr(_bDescending : Boolean = False);
begin
	Matches.Sort(TComparer<IRipGrepMatchLineGroup>.Construct(
		function(const Left, Right : IRipGrepMatchLineGroup) : Integer
		begin
			if _bDescending then begin
				Result := -TComparer<integer>.Default.Compare(Left.LineNr, Right.LineNr);
			end else begin
				Result := TComparer<integer>.Default.Compare(Left.LineNr, Right.LineNr);
			end;
		end));
	SortedBy.Clear;
end;

function TRipGrepOutputLine.GetIsError : Boolean;
begin
	Result := FIsError;
end;

function TRipGrepOutputLine.GetLineNr : Integer;
begin
	Result := FLineNr;
end;

function TRipGrepOutputLine.GetLineParseRegex : TRegex;
begin
	Result := FLineParseRegex;
end;

function TRipGrepOutputLine.GetParserType : TParserType;
begin
	Result := FParserType;
end;

{ TRipGrepOutputLine }

procedure TRipGrepOutputLine.ParseLine(const _iLnNr : integer; const _s : string);
begin
	LineNr := _iLnNr;
end;

procedure TRipGrepOutputLine.SetIsError(const Value : Boolean);
begin
	FIsError := Value;
end;

procedure TRipGrepOutputLine.SetLineNr(const Value : Integer);
begin
	FLineNr := Value;
end;

procedure TRipGrepOutputLine.SetLineParseRegex(const Value : TRegex);
begin
	FLineParseRegex := Value;
end;

procedure TRipGrepOutputLine.SetParserType(const Value : TParserType);
begin
	FParserType := Value;
end;

end.
