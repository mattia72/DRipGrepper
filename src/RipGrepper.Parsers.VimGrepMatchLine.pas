unit RipGrepper.Parsers.VimGrepMatchLine;

interface

uses
	RipGrepper.Common.Interfaces,
	RipGrepper.Common.ParsedObject,
	ArrayHelper;

type
	TVimGrepMatchLineParser = class(TInterfacedObject, ILineParser)
		private
			FParserData : ILineParserData;
			FParseResult : IParsedObjectRow;
			function GetParseResult : IParsedObjectRow;
			procedure SetParseResult(const Value : IParsedObjectRow);
			procedure SetRgResultLineParseError(out row : TArrayRecord<TColumnData>; const _sLine : string);
			function Validate(var row : TArrayRecord<TColumnData>) : Boolean;
			function ValidatePath(const sFile : string) : Boolean;

		public
			property ParserData : ILineParserData read FParserData write FParserData;
			property ParseResult : IParsedObjectRow read GetParseResult write SetParseResult;
			constructor Create;
			function ParseLine(const _iLnNr : integer; const _s : string; const _bIsLast : Boolean = False) : IParsedObjectRow;
	end;

implementation

uses
	System.RegularExpressions,
	RipGrepper.Tools.DebugTools,
	RipGrepper.Common.Types,
	System.SysUtils,
	System.IOUtils,
	RipGrepper.Data.Parsers;

constructor TVimGrepMatchLineParser.Create;
begin
	inherited;
	FParserData := TRipGrepLineParserData.Create(TParserType.ptRipGrepSearch, RG_MATCH_LINE_REGEX);
	FParseResult := TParsedObjectRow.Create();
end;

function TVimGrepMatchLineParser.GetParseResult : IParsedObjectRow;
begin
	Result := FParseResult;
end;

{ TVimGrepMatchLineParser }

function TVimGrepMatchLineParser.ParseLine(const _iLnNr : integer; const _s : string; const _bIsLast : Boolean = False) : IParsedObjectRow;
var
	m : TMatch;
	cd : TArrayRecord<TColumnData>;
begin
	ParseResult.RowNr := _iLnNr;

	m := ParserData.LineParseRegex.Match(_s);
	if m.Success then begin
		// TDebugUtils.DebugMessage(_s);
		cd.Add(TColumnData.New('File', m.Groups['drive'].Value + m.Groups['path'].Value));
		cd.Add(TColumnData.New('Row', m.Groups['row'].Value));
		cd.Add(TColumnData.New('Col', m.Groups['col'].Value));
		cd.Add(TColumnData.New('Text', m.Groups['text'].Value));
		ParseResult.IsError := not Validate(cd);
	end else begin
		SetRgResultLineParseError(cd, _s);
	end;

	ParseResult.RowNr := _iLnNr;
	ParseResult.Columns := cd;

	if (ParseResult.IsError) then begin
		TDebugUtils.DebugMessage('Error parsing line: ' + CRLF +
			{ } _s + CRLF +
			{ } 'File: ' + cd[Integer(ciFile)].Text + CRLF +
			{ } 'Row: ' + cd[Integer(ciRow)].Text + CRLF +
			{ } 'Col: ' + cd[Integer(ciCol)].Text + CRLF +
			{ } 'Text: ' + cd[Integer(ciText)].Text + CRLF +
			{ } 'ErrorText: ' + ParseResult.ErrorText);
	end;

	Result := ParseResult;
end;

procedure TVimGrepMatchLineParser.SetParseResult(const Value : IParsedObjectRow);
begin
	FParseResult := Value;
end;

procedure TVimGrepMatchLineParser.SetRgResultLineParseError(out row : TArrayRecord<TColumnData>; const _sLine : string);
begin
	row.Add(TColumnData.New('File', _sLine));
	row.Add(TColumnData.New('Row', ''));
	row.Add(TColumnData.New('Col', ''));
	row.Add(TColumnData.New('Text', ''));
	ParseResult.ErrorText := 'rg.exe result line couldn''t parsed.';
	ParseResult.IsError := True;
end;

function TVimGrepMatchLineParser.Validate(var row : TArrayRecord<TColumnData>) : Boolean;
var
	sCol : string;
	sRow : string;
begin
	Result := False;
	ParseResult.IsError := not ValidatePath(row[Integer(ciFile)].Text);
	if ParseResult.IsError then
		Exit;
	sRow := row[Integer(ciRow)].Text;
	ParseResult.IsError := ParseResult.IsError and (StrToIntDef(sRow, -1) > 0);
	if ParseResult.IsError then begin
		ParseResult.ErrorText := 'Invalid Row:' + sRow;
		Exit;
	end;
	sCol := row[Integer(ciCol)].Text;
	ParseResult.IsError := ParseResult.IsError and (StrToIntDef(sCol, -1) > 0);
	if ParseResult.IsError then begin
		ParseResult.ErrorText := 'Invalid Col:' + sCol;
		Exit
	end;
	// GroupId?
	Result := True;
end;

function TVimGrepMatchLineParser.ValidatePath(const sFile : string) : Boolean;
begin
	if sFile.StartsWith(':') then begin
		ParseResult.ErrorText := 'Begins with '':''';
	end else if not TPath.HasValidPathChars(sFile, False) then begin
		ParseResult.ErrorText := 'Invalid chars in path' + sFile;
	end else if not(TPath.IsDriveRooted(sFile) or TPath.IsRelativePath(sFile)) then begin
		ParseResult.ErrorText := 'Not an abs or relative path' + sFile;
	end;
	Result := ParseResult.ErrorText = '';
end;

end.
