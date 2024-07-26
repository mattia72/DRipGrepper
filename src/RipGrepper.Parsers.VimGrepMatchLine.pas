unit RipGrepper.Parsers.VimGrepMatchLine;

interface

uses
	RipGrepper.Common.Interfaces,
	RipGrepper.Common.ParsedObject,
	ArrayEx,
	System.RegularExpressions;

type
	TVimGrepMatchLineParser = class(TInterfacedObject, ISearchResultLineParser)
		private
			FParserData : ILineParserData;
			FParseResult : IParsedObjectRow;
			FPrettyRegex : TRegEx;
			FSearchParams : ISearchParams;
			function GetParseResult : IParsedObjectRow;
			procedure SetParseResult(const Value : IParsedObjectRow);
			procedure SetRgResultLineParseError(out row : TArrayEx<TColumnData>; const _sLine : string);
			function Validate(var row : TArrayEx<TColumnData>) : Boolean; virtual;
			function ValidatePath(const sFile : string) : Boolean;
			function GetSearchParams : ISearchParams;
			procedure SetPrettyRegex;
			procedure SetSearchParams(const Value : ISearchParams);

		public
			property ParserData : ILineParserData read FParserData write FParserData;
			property ParseResult : IParsedObjectRow read GetParseResult write SetParseResult;
			property SearchParams : ISearchParams read GetSearchParams write SetSearchParams;
			constructor Create; virtual;
			destructor Destroy; override;
			procedure ParseLine(const _iLnNr : integer; const _sLine : string; const _bIsLast : Boolean = False); virtual;
	end;

	TVimGrepPrettyMatchLineParser = class(TVimGrepMatchLineParser)
		private
			procedure ParseContextLine(const _m : TMatch; var _cd : TArrayEx<TColumnData>);
			procedure ParsePrettyLine(const m : TMatch; var cd : TArrayEx<TColumnData>);
			function Validate(var row : TArrayEx<TColumnData>) : Boolean; override;

		public
			constructor Create; override;
			procedure ParseLine(const _iLnNr : integer; const _s : string; const _bIsLast : Boolean = False); override;
	end;

implementation

uses
	RipGrepper.Tools.DebugUtils,
	RipGrepper.Common.Constants,
	System.SysUtils,
	System.IOUtils,
	RipGrepper.Data.Parsers;

constructor TVimGrepMatchLineParser.Create;
begin
	inherited;
	FParserData := TRipGrepLineParserData.Create(TParserType.ptRipGrepSearch, RG_MATCH_LINE_REGEX, RG_MATCH_LINE_CONTEXT_REGEX);
	FParseResult := TParsedObjectRow.Create();
end;

destructor TVimGrepMatchLineParser.Destroy;
begin
	FParserData := nil;
	FParseResult := nil;
	inherited;
end;

function TVimGrepMatchLineParser.GetParseResult : IParsedObjectRow;
begin
	Result := FParseResult;
end;

function TVimGrepMatchLineParser.GetSearchParams : ISearchParams;
begin
	Result := FSearchParams;
end;

{ TVimGrepMatchLineParser }

procedure TVimGrepMatchLineParser.ParseLine(const _iLnNr : integer; const _sLine : string; const _bIsLast : Boolean = False);
var
	m : TMatch;
	cd : TArrayEx<TColumnData>;
	s : string;
begin
	if _sLine = RG_CONTEXT_SEPARATOR then
		Exit;

	FParseResult.RowNr := _iLnNr;

	m := ParserData.LineParseRegex.Match(_sLine);
	if m.Success then begin
		// TDebugUtils.DebugMessage(_sLine);
		// according FastMM it is leaky :/
		s := Format('%s%s', [m.Groups['drive'].Value, m.Groups['path'].Value]);
		cd.Add(TColumnData.New(ciFile, s));
		cd.Add(TColumnData.New(ciRow, m.Groups['row'].Value));
		cd.Add(TColumnData.New(ciCol, m.Groups['col'].Value));
		s := m.Groups['text'].Value;
		var  // not used, but so we have less memory leak!
		so := SearchParams.GetGuiSearchParams;
		m := FPrettyRegex.Match(s);
		if m.Groups.Count = 4 then begin
			cd.Add(TColumnData.New(ciText, m.Groups['before'].Value));
			cd.Add(TColumnData.New(ciMatchText, m.Groups['text'].Value));
			cd.Add(TColumnData.New(ciTextAfterMatch, m.Groups['after'].Value));
		end else begin
			cd.Add(TColumnData.New(ciText, s));
			cd.Add(TColumnData.New(ciMatchText, ''));
			cd.Add(TColumnData.New(ciTextAfterMatch, ''));
		end;

	end else begin
		m := ParserData.ContextLineParseRegex.Match(_sLine);
		if m.Success then begin
			s := Format('%s%s', [m.Groups['drive'].Value, m.Groups['path'].Value]);
			cd.Add(TColumnData.New(ciFile, s));
			cd.Add(TColumnData.New(ciRow, m.Groups['row'].Value));
			cd.Add(TColumnData.New(ciCol, ''));
			cd.Add(TColumnData.New(ciText, m.Groups['text'].Value));
		end;
	end;
	if (cd.Count > 0) then
		ParseResult.IsError := not Validate(cd);

	if not m.Success then begin
		SetRgResultLineParseError(cd, _sLine);
		FParseResult.ErrorText := RG_PARSE_ERROR;
		FParseResult.IsError := True;
	end;

	FParseResult.RowNr := _iLnNr;
	FParseResult.Columns := cd;
end;

procedure TVimGrepMatchLineParser.SetParseResult(const Value : IParsedObjectRow);
begin
	FParseResult := Value;
end;

procedure TVimGrepMatchLineParser.SetPrettyRegex;
var
	pattern: string;
	s: string;
begin
	var
	gp := FSearchParams.GetGuiSearchParams;
	var
	so := gp.SearchOptions;

	if not(EGuiOption.soUseRegex in so) then begin
		s := TRegEx.Escape(gp.SearchText);
	end else begin
		s := gp.SearchText;
	end;

	pattern := '(?<before>^.*)(?<text>' + s + ')(?<after>.*$)';
	if (EGuiOption.soMatchCase in so) then begin
		FPrettyRegex := TRegEx.Create(pattern, [roCompiled]);
	end else begin
		FPrettyRegex := TRegEx.Create(pattern, [roIgnoreCase, roCompiled]);
	end;
end;

procedure TVimGrepMatchLineParser.SetRgResultLineParseError(out row : TArrayEx<TColumnData>; const _sLine : string);
begin
	row.Add(TColumnData.New(ciFile, _sLine));
	row.Add(TColumnData.New(ciRow, ''));
	row.Add(TColumnData.New(ciCol, ''));
	row.Add(TColumnData.New(ciText, ''));
end;

procedure TVimGrepMatchLineParser.SetSearchParams(const Value : ISearchParams);
begin
	FSearchParams := Value;
	SetPrettyRegex;
end;

function TVimGrepMatchLineParser.Validate(var row : TArrayEx<TColumnData>) : Boolean;
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

constructor TVimGrepPrettyMatchLineParser.Create;
begin
	FParserData := TRipGrepLineParserData.Create(TParserType.ptRipGrepPrettySearch, RG_MATCH_PRETTY_LINE_REGEX,
		RG_MATCH_PRETTY_LINE_CONTEXT_REGEX);
	FParseResult := TParsedObjectRow.Create();
end;

procedure TVimGrepPrettyMatchLineParser.ParseContextLine(const _m : TMatch; var _cd : TArrayEx<TColumnData>);
begin
	var
	s := Format('%s%s', [_m.Groups['drive'].Value, _m.Groups['path'].Value]);
	_cd.Add(TColumnData.New(ciFile, s));
	_cd.Add(TColumnData.New(ciRow, _m.Groups['row'].Value));
	_cd.Add(TColumnData.New(ciCol, ''));
	_cd.Add(TColumnData.New(ciText, _m.Groups['text'].Value));
	_cd.Add(TColumnData.New(ciMatchText, ''));
	_cd.Add(TColumnData.New(ciTextAfterMatch, ''));
end;

{ TVimGrepPrettyMatchLineParser }

procedure TVimGrepPrettyMatchLineParser.ParseLine(const _iLnNr : integer; const _s : string; const _bIsLast : Boolean = False);
var
	m : TMatch;
	cd : TArrayEx<TColumnData>;
begin
	if _s = RG_CONTEXT_SEPARATOR then
		Exit;

	ParseResult.RowNr := _iLnNr;

	m := ParserData.LineParseRegex.Match(_s);
	if m.Success then begin
		// TDebugUtils.DebugMessage(_s);
		// according FastMM it is leaky :/
		ParsePrettyLine(m, cd);
	end else begin
		m := ParserData.ContextLineParseRegex.Match(_s);
		if m.Success then begin
			ParseContextLine(m, cd);
		end;
	end;
	if (cd.Count > 0) then
		ParseResult.IsError := not Validate(cd);

	if not m.Success then begin
		SetRgResultLineParseError(cd, _s);
		FParseResult.ErrorText := RG_PARSE_ERROR;
		FParseResult.IsError := True;
	end;

	ParseResult.RowNr := _iLnNr;
	ParseResult.Columns := cd;
end;

procedure TVimGrepPrettyMatchLineParser.ParsePrettyLine(const m : TMatch; var cd : TArrayEx<TColumnData>);
begin
	var
	s := Format('%s%s', [m.Groups['drive'].Value, m.Groups['path'].Value]);
	cd.Add(TColumnData.New(ciFile, s));
	cd.Add(TColumnData.New(ciRow, m.Groups['row'].Value));
	cd.Add(TColumnData.New(ciCol, m.Groups['col'].Value));
	cd.Add(TColumnData.New(ciText, m.Groups['text_before_match'].Value));
	cd.Add(TColumnData.New(ciMatchText, m.Groups['match_text'].Value));
	var
	count := cd.Count;
	if m.Groups.Count > count + 2 then begin
		cd.Add(TColumnData.New(ciTextAfterMatch, m.Groups['text_after_match'].Value));
	end else begin
		cd.Add(TColumnData.New(ciTextAfterMatch, ''));
	end;
end;

function TVimGrepPrettyMatchLineParser.Validate(var row : TArrayEx<TColumnData>) : Boolean;
begin
	Result := inherited;
	Result := Result and (not row.IsEmpty);
end;

end.
