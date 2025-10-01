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
	{$IFDEF CONSOLE}
	{$UNDEF SKIP_BEGIN_END}
	{$ENDIF}
	TJsonMatchLineParser = class(TInterfacedObject, ISearchResultLineParser)
		private
			FParserData : ILineParserData;
			FParseResult : IParsedObjectRow;
			FSearchParams : ISearchParams;
			procedure addNoMatchToColumData(var _cd : TArrayEx<TColumnData>; const _lineText : string);
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
			function getJsonIntValue(const _jsonObj : TJSONObject; const _sKey : string) : integer;
			function getJsonStringValue(const _dataObj : TJSONObject; const _sKey : string) : string;
			function BytePosToStringPos(const _utf8Text : string; _bytePos : Integer) : Integer;

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

procedure TJsonMatchLineParser.addNoMatchToColumData(var _cd : TArrayEx<TColumnData>; const _lineText : string);
begin
	_cd.Add(TColumnData.New(ciColBegin, '1'));
	_cd.Add(TColumnData.New(ciText, _lineText));
	_cd.Add(TColumnData.New(ciMatchText, ''));
	_cd.Add(TColumnData.New(ciTextAfterMatch, ''));
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
			_cd.Add(TColumnData.New(ciColEnd, ''));
			_cd.Add(TColumnData.New(ciText, '--- Begin file: ' + filePath + ' ---'));
			_cd.Add(TColumnData.New(ciMatchText, ''));
			_cd.Add(TColumnData.New(ciTextAfterMatch, ''));
		end;
	end;
end;

procedure TJsonMatchLineParser.parseJsonMatchLine(const _jsonObj : TJSONObject; var _cd : TArrayEx<TColumnData>);
var
	dataObj : TJSONObject;
	submatchesArray : TJSONArray;
	filePath : string;
	lineNumber : Integer;
	lineText : string;
	startBytePos, endBytePos : Integer;
	startCharPos, endCharPos : Integer;
begin
	// Extract data from {"type":"match","data":{"path":{"text":"..."},"lines":{"text":"..."},"line_number":123,"submatches":[...]}}
	var
	dataValue := _jsonObj.GetValue('data');
	if not Assigned(dataValue) or not(dataValue is TJSONObject) then
		Exit;
	dataObj := dataValue as TJSONObject;

	// Get file path
	filePath := getJsonStringValue(dataObj, 'path');
	_cd.Add(TColumnData.New(ciFile, filePath));

	lineNumber := getJsonIntValue(dataObj, 'line_number');
	_cd.Add(TColumnData.New(ciRow, IntToStr(lineNumber)));

	lineText := getJsonStringValue(dataObj, 'lines');

	// Get submatches for highlighting
	var
	submatchesValue := dataObj.GetValue('submatches');
	if Assigned(submatchesValue) and (submatchesValue is TJSONArray) then begin
		submatchesArray := submatchesValue as TJSONArray;
	end else begin
		submatchesArray := nil;
	end;

	// Calculate column from first submatch
	if Assigned(submatchesArray) and (submatchesArray.Count > 0) then begin
		var
		submatchItem := submatchesArray.Items[0];
		if Assigned(submatchItem) and (submatchItem is TJSONObject) then begin
			var
			submatchObj := submatchItem as TJSONObject;

			// Get byte positions from JSON
			startBytePos := getJsonIntValue(submatchObj, 'start');
			endBytePos := getJsonIntValue(submatchObj, 'end');

			// Convert byte positions to string positions
			startCharPos := BytePosToStringPos(lineText, startBytePos);
			endCharPos := BytePosToStringPos(lineText, endBytePos);

			_cd.Add(TColumnData.New(ciColBegin, IntToStr(startCharPos)));
			_cd.Add(TColumnData.New(ciColEnd, IntToStr(endCharPos)));

			// Extract text parts using string positions
			var
			beforeText := Copy(lineText, 1, startCharPos - 1);
			var
			matchText := Copy(lineText, startCharPos, endCharPos - startCharPos);
			var
			afterText := Copy(lineText, endCharPos, Length(lineText));

			_cd.Add(TColumnData.New(ciText, beforeText));
			_cd.Add(TColumnData.New(ciMatchText, matchText));
			_cd.Add(TColumnData.New(ciTextAfterMatch, afterText));

		end else begin
			addNoMatchToColumData(_cd, lineText);
		end;
	end else begin
		addNoMatchToColumData(_cd, lineText);
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
		_cd.Add(TColumnData.New(ciColEnd, ''));
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
		_cd.Add(TColumnData.New(ciColEnd, ''));
		_cd.Add(TColumnData.New(ciText, summaryText));
		_cd.Add(TColumnData.New(ciMatchText, ''));
		_cd.Add(TColumnData.New(ciTextAfterMatch, ''));

		ParseResult.IsStatsLine := True;
	end;
end;

function TJsonMatchLineParser.getJsonIntValue(const _jsonObj : TJSONObject; const _sKey : string) : integer;
begin
	var
	startValue := _jsonObj.GetValue(_sKey);

	if Assigned(startValue) then begin
		Result := StrToIntDef(startValue.Value, 0);
	end else begin
		Result := 0;
	end;
end;

function TJsonMatchLineParser.getJsonStringValue(const _dataObj : TJSONObject; const _sKey : string) : string;
var
	jsonObj : TJSONObject;
	textValue : TJSONValue;
begin
	Result := '';

	var
	linesValue := _dataObj.GetValue(_sKey);
	if Assigned(linesValue) and (linesValue is TJSONObject) then begin
		jsonObj := linesValue as TJSONObject;
		textValue := jsonObj.GetValue('text');
		if Assigned(textValue) then begin
			Result := textValue.Value
		end;
	end;
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

function TJsonMatchLineParser.BytePosToStringPos(const _utf8Text : string; _bytePos : Integer) : Integer;
var
	i, byteCount : Integer;
begin
	Result := 1;
	if _bytePos <= 0 then
		Exit;

	byteCount := 0;

	for i := 1 to Length(_utf8Text) do begin
		if byteCount >= _bytePos then begin
			Result := i;
			Exit;
		end;
		// Count bytes for current character
		Inc(byteCount, TEncoding.UTF8.GetByteCount(_utf8Text[i]));
	end;

	// If we reach here, position is at or beyond end
	Result := Length(_utf8Text) + 1;
end;

end.
