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

		protected
			procedure ParseContextLine(const _m : TMatch; var _cd : TArrayEx<TColumnData>); virtual;
			procedure ParseStatsLine(const _m : TMatch; var _cd : TArrayEx<TColumnData>);

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
			procedure ParsePrettyLine(const m : TMatch; var cd : TArrayEx<TColumnData>);
			function Validate(var row : TArrayEx<TColumnData>) : Boolean; override;

		protected
			procedure ParseContextLine(const _m : TMatch; var _cd : TArrayEx<TColumnData>); override;

		public
			constructor Create; override;
			procedure ParseLine(const _iLnNr : integer; const _s : string; const _bIsLast : Boolean = False); override;
	end;

implementation

uses
	RipGrepper.Tools.DebugUtils,
	RipGrepper.Common.Constants,
	RipGrepper.Common.SimpleTypes,
	System.SysUtils,
	System.IOUtils,
	RipGrepper.Data.Parsers,
	RipGrepper.Common.SearchTextWithOptions,
	Spring;

constructor TVimGrepMatchLineParser.Create;
begin
	inherited;
	FParserData := TRipGrepLineParserData.Create(TParserType.ptRipGrepSearch, RG_MATCH_LINE_REGEX, RG_MATCH_LINE_CONTEXT_REGEX);
	FParseResult := TParsedObjectRow.Create();
	// FSearchParams :=
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

procedure TVimGrepMatchLineParser.ParseContextLine(const _m : TMatch; var _cd : TArrayEx<TColumnData>);
var
	s : string;
begin
	s := Format('%s%s', [_m.Groups['drive'].Value, _m.Groups['path'].Value]);
	_cd.Add(TColumnData.New(ciFile, s));
	_cd.Add(TColumnData.New(ciRow, _m.Groups['row'].Value));
	_cd.Add(TColumnData.New(ciColBegin, ''));
	_cd.Add(TColumnData.New(ciColEnd, ''));
	_cd.Add(TColumnData.New(ciText, _m.Groups['text'].Value));
	_cd.Add(TColumnData.New(ciMatchText, ''));
	_cd.Add(TColumnData.New(ciTextAfterMatch, ''));
end;

procedure TVimGrepMatchLineParser.ParseStatsLine(const _m : TMatch; var _cd : TArrayEx<TColumnData>);
begin
	_cd.Add(TColumnData.New(ciFile, RG_STATS_LINE));
	_cd.Add(TColumnData.New(ciRow, ''));
	_cd.Add(TColumnData.New(ciColBegin, ''));
	_cd.Add(TColumnData.New(ciColEnd, ''));
	_cd.Add(TColumnData.New(ciText, _m.Groups[0].Value));
	_cd.Add(TColumnData.New(ciMatchText, ''));
	_cd.Add(TColumnData.New(ciTextAfterMatch, ''));
end;

{ TVimGrepMatchLineParser }

procedure TVimGrepMatchLineParser.ParseLine(const _iLnNr : integer; const _sLine : string; const _bIsLast : Boolean = False);
var
	m : TMatch;
	matchPretty : TMatch;
	cd : TArrayEx<TColumnData>;
begin
	if _sLine = RG_CONTEXT_SEPARATOR then
		Exit;

	m := ParserData.LineParseRegex.Match(_sLine);
	if m.Success then begin
		// TDebugUtils.DebugMessage(_sLine);
		// according FastMM it is leaky :/
		var
		s := Format('%s%s', [m.Groups['drive'].Value, m.Groups['path'].Value]);
		var
		iColBegin := m.Groups['col'].Value.ToInteger;
		cd.Add(TColumnData.New(ciFile, s));
		cd.Add(TColumnData.New(ciRow, m.Groups['row'].Value));
		cd.Add(TColumnData.New(ciColBegin, iColBegin.ToString));
		s := m.Groups['text'].Value;

		var // not used, but so we have less memory leak!
		so := SearchParams; // can stwo
		Assert(Assigned(so));

		matchPretty := FPrettyRegex.Match(s);
		if matchPretty.Groups.Count >= 4 then begin
			var
			sMatchText := matchPretty.Groups['text'].Value;
			cd.Add(TColumnData.New(ciColEnd, (iColBegin + sMatchText.Length - 1).ToString));
			cd.Add(TColumnData.New(ciText, matchPretty.Groups['before'].Value));
			cd.Add(TColumnData.New(ciMatchText, sMatchText));
			cd.Add(TColumnData.New(ciTextAfterMatch, matchPretty.Groups['after'].Value));
		end else begin
			// in this case highlighting doesn't work...
			cd.Add(TColumnData.New(ciColEnd, ''));
			cd.Add(TColumnData.New(ciText, s));
			cd.Add(TColumnData.New(ciMatchText, ''));
			cd.Add(TColumnData.New(ciTextAfterMatch, ''));
		end;

	end else begin
		m := ParserData.ContextLineParseRegex.Match(_sLine);
		if m.Success then begin
			ParseContextLine(m, cd);
		end else begin
			m := ParserData.StatsLineParseRegex.Match(_sLine);
			if m.Success then begin
				ParseStatsLine(m, cd);
				ParseResult.IsStatsLine := True;
			end;
		end;
	end;
	if (cd.Count > 0) then
		ParseResult.IsError := not Validate(cd);

	if not m.Success then begin
		SetRgResultLineParseError(cd, _sLine);
		FParseResult.ErrorText := RG_PARSE_ERROR;
		FParseResult.IsError := True;
	end;

	FParseResult.ParsedRowNr := _iLnNr;
	FParseResult.Columns := cd;
end;

procedure TVimGrepMatchLineParser.SetParseResult(const Value : IParsedObjectRow);
begin
	FParseResult := Value;
end;

procedure TVimGrepMatchLineParser.SetPrettyRegex;
var
	pattern, s : string;
	so : TSearchOptionSet;
	stwo : IShared<TSearchTextWithOptions>;
begin
	stwo := FSearchParams.GetGuiSearchParams; // can stwo
	so := stwo.SearchOptions;

	if not((EGuiOption.soUseRegex in so) or
		{ } (EGuiOption.soMatchWord in so)) then begin
		s := stwo.EscapedSearchText;
	end else begin
		s := stwo.SearchTextAsRgParam;
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
	row.Add(TColumnData.New(ciColBegin, ''));
	row.Add(TColumnData.New(ciColEnd, ''));
	row.Add(TColumnData.New(ciText, ''));
	row.Add(TColumnData.New(ciMatchText, ''));
	row.Add(TColumnData.New(ciTextAfterMatch, ''));
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
	sCol := row[Integer(ciColBegin)].Text;
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
		ParseResult.ErrorText := 'Invalid chars in path: ' + sFile;
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
	inherited ParseContextLine(_m, _cd);
end;

{ TVimGrepPrettyMatchLineParser }

procedure TVimGrepPrettyMatchLineParser.ParseLine(const _iLnNr : integer; const _s : string; const _bIsLast : Boolean = False);
var
	m : TMatch;
	cd : TArrayEx<TColumnData>;
begin
	if _s = RG_CONTEXT_SEPARATOR then
		Exit;

	ParseResult.ParsedRowNr := _iLnNr;

	m := ParserData.LineParseRegex.Match(_s);
	if m.Success then begin
		// TDebugUtils.DebugMessage(_s);
		// according FastMM it is leaky :/
		ParsePrettyLine(m, cd);
	end else begin
		m := ParserData.ContextLineParseRegex.Match(_s);
		if m.Success then begin
			ParseContextLine(m, cd);
		end else begin
			m := ParserData.StatsLineParseRegex.Match(_s);
			if m.Success then begin
				ParseStatsLine(m, cd);
				ParseResult.IsStatsLine := True;
			end;
		end;
	end;

	if (cd.Count > 0) then
		ParseResult.IsError := not Validate(cd);

	if not m.Success then begin
		SetRgResultLineParseError(cd, _s);
		FParseResult.ErrorText := RG_PARSE_ERROR;
		FParseResult.IsError := True;
	end;

	ParseResult.ParsedRowNr := _iLnNr;
	ParseResult.Columns := cd;
end;

procedure TVimGrepPrettyMatchLineParser.ParsePrettyLine(const m : TMatch; var cd : TArrayEx<TColumnData>);
begin
	var
	s := Format('%s%s', [m.Groups['drive'].Value, m.Groups['path'].Value]);
	cd.Add(TColumnData.New(ciFile, s));
	cd.Add(TColumnData.New(ciRow, m.Groups['row'].Value));
	var iColBegin := m.Groups['col'].Value.ToInteger;
	cd.Add(TColumnData.New(ciColBegin, iColBegin.ToString));
	cd.Add(TColumnData.New(ciText, m.Groups['text_before_match'].Value));
	var
	count := cd.Count;
	if m.Groups.Count > count + 2 then begin
		var sMatchText := m.Groups['match_text'].Value;
		cd.Add(TColumnData.New(ciMatchText, sMatchText));
		cd.Add(TColumnData.New(ciColEnd, (iColBegin + sMatchText.Length - 1).ToString));
	end else begin
		cd.Add(TColumnData.New(ciMatchText, ''));
		cd.Add(TColumnData.New(ciColEnd, ''));
	end;

	count := cd.Count;
	if m.Groups.Count > count + 3 then begin
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
