unit RipGrepper.Parsers.JsonMatchLine;

interface

uses
	RipGrepper.Common.Interfaces,
	RipGrepper.Common.ParsedObject,
	ArrayEx,
	System.JSON;

type
	{$DEFINE SKIP_BEGIN_END}
	{$IFDEF TESTINSIGHT}
	{$UNDEF SKIP_BEGIN_END}
	{$ENDIF}
	TJsonMatchLineParser = class(TInterfacedObject, ISearchResultLineParser)
		private
			FParserData : ILineParserData;
			FParseResult : IParsedObjectRow;
			FSearchParams : ISearchParams;
			function GetParseResult : IParsedObjectRow;
			procedure SetParseResult(const Value : IParsedObjectRow);
			function GetSearchParams : ISearchParams;
			procedure SetSearchParams(const Value : ISearchParams);
			procedure setRgResultLineParseError(out row : TArrayEx<TColumnData>; const _sLine : string);
			function Validate(var row : TArrayEx<TColumnData>) : Boolean; virtual;
			function ValidatePath(const sFile : string) : Boolean;
			procedure parseJsonMatchLine(const _jsonObj : TJSONObject; var _cd : TArrayEx<TColumnData>);
			procedure parseJsonBeginLine(const _jsonObj : TJSONObject; var _cd : TArrayEx<TColumnData>);
			procedure parseJsonEndLine(const _jsonObj : TJSONObject; var _cd : TArrayEx<TColumnData>);
			procedure parseJsonSummaryLine(const _jsonObj : TJSONObject; var _cd : TArrayEx<TColumnData>);
			function extractTextParts(const _lineText : string; const _submatches : TJSONArray) : TArray<string>;

		public
			property ParserData : ILineParserData read FParserData write FParserData;
			property ParseResult : IParsedObjectRow read GetParseResult write SetParseResult;
			property SearchParams : ISearchParams read GetSearchParams write SetSearchParams;
			constructor Create; virtual;
			destructor Destroy; override;
			procedure ParseLine(const _iLnNr : integer; const _sLine : string; const _bIsLast : Boolean = False); virtual;
	end;

implementation

uses
	RipGrepper.Tools.DebugUtils,
	RipGrepper.Common.Constants,
	RipGrepper.Common.SimpleTypes,
	System.SysUtils,
	System.IOUtils,
	RipGrepper.Data.Parsers,
	System.Generics.Collections;

constructor TJsonMatchLineParser.Create;
begin
	inherited;
	FParserData := TRipGrepLineParserData.Create(TParserType.ptRipGrepJson, '', '');
	FParseResult := TParsedObjectRow.Create();
end;

destructor TJsonMatchLineParser.Destroy;
begin
	FParserData := nil;
	FParseResult := nil;
	inherited;
end;

function TJsonMatchLineParser.GetParseResult : IParsedObjectRow;
begin
	Result := FParseResult;
end;

function TJsonMatchLineParser.GetSearchParams : ISearchParams;
begin
	Result := FSearchParams;
end;

procedure TJsonMatchLineParser.SetParseResult(const Value : IParsedObjectRow);
begin
	FParseResult := Value;
end;

procedure TJsonMatchLineParser.SetSearchParams(const Value : ISearchParams);
begin
	FSearchParams := Value;
end;

procedure TJsonMatchLineParser.ParseLine(const _iLnNr : integer; const _sLine : string; const _bIsLast : Boolean = False);
var
	jsonObj : TJSONObject;
	jsonValue : TJSONValue;
	cd : TArrayEx<TColumnData>;
	jsonType : string;
begin
	jsonValue := nil;
	FParseResult.ParsedRowNr := _iLnNr;

	// Check if line starts with JSON format
	if not _sLine.StartsWith('{"') then begin
		setRgResultLineParseError(cd, _sLine);
		FParseResult.ErrorText := 'Not a valid JSON line';
		FParseResult.IsError := True;
		FParseResult.Columns := cd;
		Exit;
	end;

	try
		jsonValue := TJSONObject.ParseJSONValue(_sLine);
		if not Assigned(jsonValue) or not(jsonValue is TJSONObject) then begin
			setRgResultLineParseError(cd, _sLine);
			FParseResult.ErrorText := 'Invalid JSON format';
			FParseResult.IsError := True;
		end else begin
			jsonObj := jsonValue as TJSONObject;

			// Get the type field
			var
			typeValue := jsonObj.GetValue('type');
			if Assigned(typeValue) then
				jsonType := typeValue.Value
			else
				jsonType := '';

			if jsonType = 'begin' then begin
				parseJsonBeginLine(jsonObj, cd);
			end else if jsonType = 'match' then begin
				parseJsonMatchLine(jsonObj, cd);
			end else if jsonType = 'end' then begin
				parseJsonEndLine(jsonObj, cd);
			end else if jsonType = 'summary' then begin
				parseJsonSummaryLine(jsonObj, cd);
			end else begin
				setRgResultLineParseError(cd, _sLine);
				FParseResult.ErrorText := 'Unknown JSON type: ' + jsonType;
				FParseResult.IsError := True;
			end;
		end;
	except
		on E : Exception do begin
			setRgResultLineParseError(cd, _sLine);
			FParseResult.ErrorText := 'JSON parsing error: ' + E.Message;
			FParseResult.IsError := True;
		end;
	end;

	if (cd.Count > 0) and not FParseResult.IsError then
		FParseResult.IsError := not Validate(cd);

	FParseResult.Columns := cd;

	if Assigned(jsonValue) then
		jsonValue.Free;
end;

procedure TJsonMatchLineParser.parseJsonBeginLine(const _jsonObj : TJSONObject; var _cd : TArrayEx<TColumnData>);
var
	dataObj : TJSONObject;
	pathObj : TJSONObject;
	filePath : string;
	textValue : TJSONValue;
begin
	{$IFDEF SKIP_BEGIN_END}
	Exit;
	{$ENDIF}
	// Extract file path from {"type":"begin","data":{"path":{"text":"filename"}}}
	var
	dataValue := _jsonObj.GetValue('data');
	if Assigned(dataValue) and (dataValue is TJSONObject) then begin
		dataObj := dataValue as TJSONObject;
		var
		pathValue := dataObj.GetValue('path');
		if Assigned(pathValue) and (pathValue is TJSONObject) then begin
			pathObj := pathValue as TJSONObject;
			textValue := pathObj.GetValue('text');
			if Assigned(textValue) then begin
				filePath := textValue.Value
			end else begin
				filePath := '';
			end;
			_cd.Add(TColumnData.New(ciFile, filePath));
			_cd.Add(TColumnData.New(ciRow, ''));
			_cd.Add(TColumnData.New(ciColBegin, ''));
			_cd.Add(TColumnData.New(ciText, '--- Begin file: ' + filePath + ' ---'));
			_cd.Add(TColumnData.New(ciMatchText, ''));
			_cd.Add(TColumnData.New(ciTextAfterMatch, ''));
		end;
	end;
end;

procedure TJsonMatchLineParser.parseJsonMatchLine(const _jsonObj : TJSONObject; var _cd : TArrayEx<TColumnData>);
var
	dataObj : TJSONObject;
	pathObj : TJSONObject;
	linesObj : TJSONObject;
	submatchesArray : TJSONArray;
	filePath : string;
	lineNumber : Integer;
	lineText : string;
	textParts : TArray<string>;
	textValue : TJSONValue;
	numberValue : TJSONValue;
begin
	// Extract data from {"type":"match","data":{"path":{"text":"..."},"lines":{"text":"..."},"line_number":123,"submatches":[...]}}
	var
	dataValue := _jsonObj.GetValue('data');
	if not Assigned(dataValue) or not(dataValue is TJSONObject) then
		Exit;
	dataObj := dataValue as TJSONObject;

	// Get file path
	var
	pathValue := dataObj.GetValue('path');
	if Assigned(pathValue) and (pathValue is TJSONObject) then begin
		pathObj := pathValue as TJSONObject;
		textValue := pathObj.GetValue('text');
		if Assigned(textValue) then
			filePath := textValue.Value
		else
			filePath := '';
	end;

	// Get line number
	numberValue := dataObj.GetValue('line_number');
	if Assigned(numberValue) then
		lineNumber := StrToIntDef(numberValue.Value, 0)
	else
		lineNumber := 0;

	// Get line text
	var
	linesValue := dataObj.GetValue('lines');
	if Assigned(linesValue) and (linesValue is TJSONObject) then begin
		linesObj := linesValue as TJSONObject;
		textValue := linesObj.GetValue('text');
		if Assigned(textValue) then begin
			lineText := textValue.Value
		end else begin
			lineText := '';
		end;
	end;

	// Get submatches for highlighting
	var
	submatchesValue := dataObj.GetValue('submatches');
	if Assigned(submatchesValue) and (submatchesValue is TJSONArray) then begin
		submatchesArray := submatchesValue as TJSONArray;
	end else begin
		submatchesArray := nil;
	end;

	_cd.Add(TColumnData.New(ciFile, filePath));
	_cd.Add(TColumnData.New(ciRow, IntToStr(lineNumber)));

	// Calculate column from first submatch
	if Assigned(submatchesArray) and (submatchesArray.Count > 0) then begin
		var
		submatchItem := submatchesArray.Items[0];
		if Assigned(submatchItem) and (submatchItem is TJSONObject) then begin
			var
			submatchObj := submatchItem as TJSONObject;
			var
			startValue := submatchObj.GetValue('start');
			var
				startPos : Integer;
			if Assigned(startValue) then
				startPos := StrToIntDef(startValue.Value, 0)
			else
				startPos := 0;
			_cd.Add(TColumnData.New(ciColBegin, IntToStr(startPos + 1))); // 1-based indexing

			// Extract text parts for highlighting
			textParts := extractTextParts(lineText, submatchesArray);
			if Length(textParts) >= 3 then begin
				_cd.Add(TColumnData.New(ciText, textParts[0])); // text before match
				_cd.Add(TColumnData.New(ciMatchText, textParts[1])); // matched text
				_cd.Add(TColumnData.New(ciTextAfterMatch, textParts[2])); // text after match
			end else begin
				_cd.Add(TColumnData.New(ciText, lineText));
				_cd.Add(TColumnData.New(ciMatchText, ''));
				_cd.Add(TColumnData.New(ciTextAfterMatch, ''));
			end;
		end else begin
			_cd.Add(TColumnData.New(ciColBegin, '1'));
			_cd.Add(TColumnData.New(ciText, lineText));
			_cd.Add(TColumnData.New(ciMatchText, ''));
			_cd.Add(TColumnData.New(ciTextAfterMatch, ''));
		end;
	end else begin
		_cd.Add(TColumnData.New(ciColBegin, '1'));
		_cd.Add(TColumnData.New(ciText, lineText));
		_cd.Add(TColumnData.New(ciMatchText, ''));
		_cd.Add(TColumnData.New(ciTextAfterMatch, ''));
	end;
end;

procedure TJsonMatchLineParser.parseJsonEndLine(const _jsonObj : TJSONObject; var _cd : TArrayEx<TColumnData>);
var
	dataObj : TJSONObject;
	pathObj : TJSONObject;
	statsObj : TJSONObject;
	filePath : string;
	statsText : string;
	textValue : TJSONValue;
	matchesValue, matchedLinesValue : TJSONValue;
	matches, matchedLines : Integer;
begin
	{$IFDEF SKIP_BEGIN_END}
	Exit;
	{$ENDIF}
	// Extract data from {"type":"end","data":{"path":{"text":"..."},"stats":{...}}}
	var
	dataValue := _jsonObj.GetValue('data');
	if Assigned(dataValue) and (dataValue is TJSONObject) then begin
		dataObj := dataValue as TJSONObject;
		var
		pathValue := dataObj.GetValue('path');
		if Assigned(pathValue) and (pathValue is TJSONObject) then begin
			pathObj := pathValue as TJSONObject;
			textValue := pathObj.GetValue('text');
			if Assigned(textValue) then
				filePath := textValue.Value
			else
				filePath := '';
		end;

		var
		statsValue := dataObj.GetValue('stats');
		if Assigned(statsValue) and (statsValue is TJSONObject) then begin
			statsObj := statsValue as TJSONObject;
			matchesValue := statsObj.GetValue('matches');
			if Assigned(matchesValue) then
				matches := StrToIntDef(matchesValue.Value, 0)
			else
				matches := 0;

			matchedLinesValue := statsObj.GetValue('matched_lines');
			if Assigned(matchedLinesValue) then
				matchedLines := StrToIntDef(matchedLinesValue.Value, 0)
			else
				matchedLines := 0;

			statsText := Format('--- End file: %s (Matches: %d, Lines: %d) ---', [filePath, matches, matchedLines]);
		end else begin
			statsText := '--- End file: ' + filePath + ' ---';
		end;

		_cd.Add(TColumnData.New(ciFile, RG_STATS_LINE));
		_cd.Add(TColumnData.New(ciRow, ''));
		_cd.Add(TColumnData.New(ciColBegin, ''));
		_cd.Add(TColumnData.New(ciText, statsText));
		_cd.Add(TColumnData.New(ciMatchText, ''));
		_cd.Add(TColumnData.New(ciTextAfterMatch, ''));

		ParseResult.IsStatsLine := True;
	end;
end;

procedure TJsonMatchLineParser.parseJsonSummaryLine(const _jsonObj : TJSONObject; var _cd : TArrayEx<TColumnData>);
var
	dataObj : TJSONObject;
	statsObj : TJSONObject;
	summaryText : string;
	matchesValue, matchedLinesValue, searchesValue : TJSONValue;
	matches, matchedLines, searches : Integer;
begin

	{$IFDEF SKIP_BEGIN_END}
	Exit;
	{$ENDIF}
	// Extract summary data from {"type":"summary","data":{"stats":{...}}}
	var
	dataValue := _jsonObj.GetValue('data');
	if Assigned(dataValue) and (dataValue is TJSONObject) then begin
		dataObj := dataValue as TJSONObject;
		var
		statsValue := dataObj.GetValue('stats');
		if Assigned(statsValue) and (statsValue is TJSONObject) then begin
			statsObj := statsValue as TJSONObject;
			matchesValue := statsObj.GetValue('matches');
			if Assigned(matchesValue) then
				matches := StrToIntDef(matchesValue.Value, 0)
			else
				matches := 0;

			matchedLinesValue := statsObj.GetValue('matched_lines');
			if Assigned(matchedLinesValue) then
				matchedLines := StrToIntDef(matchedLinesValue.Value, 0)
			else
				matchedLines := 0;

			searchesValue := statsObj.GetValue('searches');
			if Assigned(searchesValue) then
				searches := StrToIntDef(searchesValue.Value, 0)
			else
				searches := 0;

			summaryText := Format('=== Summary: %d searches, %d matches, %d matched lines ===', [searches, matches, matchedLines]);
		end else begin
			summaryText := '=== Search completed ===';
		end;

		_cd.Add(TColumnData.New(ciFile, RG_STATS_LINE));
		_cd.Add(TColumnData.New(ciRow, ''));
		_cd.Add(TColumnData.New(ciColBegin, ''));
		_cd.Add(TColumnData.New(ciText, summaryText));
		_cd.Add(TColumnData.New(ciMatchText, ''));
		_cd.Add(TColumnData.New(ciTextAfterMatch, ''));

		ParseResult.IsStatsLine := True;
	end;
end;

function TJsonMatchLineParser.extractTextParts(const _lineText : string; const _submatches : TJSONArray) : TArray<string>;
var
	submatchObj : TJSONObject;
	matchObj : TJSONObject;
	startPos, endPos : Integer;
	beforeText, matchText, afterText : string;
	startValue, endValue, textValue : TJSONValue;
begin
	SetLength(Result, 3);

	if not Assigned(_submatches) or (_submatches.Count = 0) then begin
		Result[0] := _lineText;
		Result[1] := '';
		Result[2] := '';
		Exit;
	end;

	// Get first submatch
	var
	submatchItem := _submatches.Items[0];
	if not Assigned(submatchItem) or not(submatchItem is TJSONObject) then begin
		Result[0] := _lineText;
		Result[1] := '';
		Result[2] := '';
		Exit;
	end;

	submatchObj := submatchItem as TJSONObject;

	startValue := submatchObj.GetValue('start');
	if Assigned(startValue) then begin
		startPos := StrToIntDef(startValue.Value, 0);
	end else begin
		startPos := 0;
	end;

	endValue := submatchObj.GetValue('end');
	if Assigned(endValue) then begin
		endPos := StrToIntDef(endValue.Value, 0);
	end else begin
		endPos := 0;
	end;

	// Extract match text from submatch object
	var
	matchValue := submatchObj.GetValue('match');
	if Assigned(matchValue) and (matchValue is TJSONObject) then begin
		matchObj := matchValue as TJSONObject;
		textValue := matchObj.GetValue('text');
		if Assigned(textValue) then
			matchText := textValue.Value
		else
			matchText := '';
	end else begin
		matchText := '';
	end;

	// Split the line text
	beforeText := Copy(_lineText, 1, startPos);
	afterText := Copy(_lineText, endPos + 1, Length(_lineText));

	Result[0] := beforeText;
	Result[1] := matchText;
	Result[2] := afterText;
end;

procedure TJsonMatchLineParser.setRgResultLineParseError(out row : TArrayEx<TColumnData>; const _sLine : string);
begin
	row.Add(TColumnData.New(ciFile, _sLine));
	row.Add(TColumnData.New(ciRow, ''));
	row.Add(TColumnData.New(ciColBegin, ''));
	row.Add(TColumnData.New(ciText, ''));
	row.Add(TColumnData.New(ciMatchText, ''));
	row.Add(TColumnData.New(ciTextAfterMatch, ''));
end;

function TJsonMatchLineParser.Validate(var row : TArrayEx<TColumnData>) : Boolean;
var
	sCol : string;
	sRow : string;
begin
	Result := False;

	// Skip validation for stats lines
	if ParseResult.IsStatsLine then begin
		Result := True;
		Exit;
	end;

	ParseResult.IsError := not ValidatePath(row[Integer(ciFile)].Text);
	if ParseResult.IsError then
		Exit;

	sRow := row[Integer(ciRow)].Text;
	if sRow <> '' then begin
		ParseResult.IsError := StrToIntDef(sRow, -1) <= 0;
		if ParseResult.IsError then begin
			ParseResult.ErrorText := 'Invalid Row: ' + sRow;
			Exit;
		end;
	end;

	sCol := row[Integer(ciColBegin)].Text;
	if sCol <> '' then begin
		ParseResult.IsError := StrToIntDef(sCol, -1) <= 0;
		if ParseResult.IsError then begin
			ParseResult.ErrorText := 'Invalid Col: ' + sCol;
			Exit;
		end;
	end;

	Result := True;
end;

function TJsonMatchLineParser.ValidatePath(const sFile : string) : Boolean;
begin
	// Skip validation for special lines
	if sFile = RG_STATS_LINE then begin
		Result := True;
		Exit;
	end;

	if sFile.StartsWith(':') then begin
		ParseResult.ErrorText := 'Begins with '':''';
	end else if not TPath.HasValidPathChars(sFile, False) then begin
		ParseResult.ErrorText := 'Invalid chars in path: ' + sFile;
	end else if not(TPath.IsDriveRooted(sFile) or TPath.IsRelativePath(sFile)) then begin
		ParseResult.ErrorText := 'Not an abs or relative path: ' + sFile;
	end;
	Result := ParseResult.ErrorText = '';
end;

end.
