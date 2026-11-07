{
	Plans insertion of a unit into a Pascal unit's interface or implementation
	uses clause without depending on GExperts or the Delphi IDE.

	Recognizes comments, quoted strings, escaped identifiers, namespaced units,
	and explicit unit file paths. Checks both sections for duplicates and refuses
	self-references. Creates a missing uses clause and preserves line endings.

	Returns insertion text and a zero-based UTF-16 offset without modifying the
	source. ToolsAPI callers must convert the source prefix to UTF-8 to obtain
	the byte offset required by IOTAEditWriter.

	This is a focused header parser. Conditional uses clauses, include-based
	headers, ambiguous section boundaries, and unsupported source layouts are
	refused so the caller can request a manual edit.
}
unit RipGrepper.Common.PascalUses;

interface

type

	TAddUnitResult = (aurAdded, aurInInterface, aurInImplementation, aurSameUnit, aurUnsupportedSource);

	TUsesInsertion = record
		// Zero-based UTF-16 offset. Convert the source prefix to UTF-8 before using an OTA writer.
		Position : Integer;
		Text : string;
	end;

	TPascalUses = class
		public
			// Plans a single insertion without changing the source. Ambiguous headers are refused.
			class function PlanInsertion(const _source, _unitName : string; _toImplementation : Boolean;
				out _insertion : TUsesInsertion) : TAddUnitResult; static;
	end;

implementation

uses
	System.Character,
	System.SysUtils;

type
	TTokenKind = (tkEnd, tkIdentifier, tkString, tkSymbol, tkDirective, tkInvalid);

	TToken = record
		Kind : TTokenKind;
		Text : string;
		Position : Integer;
		AfterPosition : Integer;
		Escaped : Boolean;
		function IsText(const _text : string) : Boolean;
	end;

	THeaderReader = record
		Source : string;
		Position : Integer;
		function Next() : TToken;
		function NextHeaderToken() : TToken;
	end;

function IsIdentifierStart(_character : Char) : Boolean;
begin
	Result := (_character = '_') or _character.IsLetter();
end;

function IsIdentifierPart(_character : Char) : Boolean;
begin
	Result := IsIdentifierStart(_character) or _character.IsDigit();
end;

function TToken.IsText(const _text : string) : Boolean;
begin
	Result := (Kind in [tkIdentifier, tkSymbol]) and not Escaped and SameText(Text, _text);
end;

function THeaderReader.Next() : TToken;
var
	start : Integer;
	closing : string;
	directive : Boolean;
begin
	Result := Default(TToken);
	while Position <= Length(Source) do begin
		if (Source[Position] <= ' ') or (Source[Position] = #$FEFF) then begin
			Inc(Position);
			Continue;
		end;
		start := Position;
		if Copy(Source, Position, 2) = '//' then begin
			while (Position <= Length(Source)) and not CharInSet(Source[Position], [#10, #13]) do begin
				Inc(Position);
			end;
			Continue;
		end;
		if (Source[Position] = '{') or (Copy(Source, Position, 2) = '(*') then begin
			if Source[Position] = '{' then begin
				closing := '}';
				Inc(Position);
			end else begin
				closing := '*)';
				Inc(Position, 2);
			end;
			directive := (Position <= Length(Source)) and (Source[Position] = '$');
			var contentStart := Position + 1;
			while (Position <= Length(Source)) and (Copy(Source, Position, Length(closing)) <> closing) do begin
				Inc(Position);
			end;
			if Position > Length(Source) then begin
				Result.Kind := tkInvalid;
				Exit;
			end;
			Result.Text := Trim(Copy(Source, contentStart, Position - contentStart));
			Inc(Position, Length(closing));
			if directive then begin
				Result.Kind := tkDirective;
				Result.Position := start;
				Result.AfterPosition := Position;
				Exit;
			end;
			Continue;
		end;
		Result.Position := start;
		if Source[Position] = '''' then begin
			Inc(Position);
			Result.Kind := tkInvalid;
			while Position <= Length(Source) do begin
				if CharInSet(Source[Position], [#10, #13]) then begin
					Break;
				end;
				if Source[Position] = '''' then begin
					Inc(Position);
					if (Position <= Length(Source)) and (Source[Position] = '''') then begin
						Inc(Position);
						Continue;
					end;
					Result.Kind := tkString;
					Break;
				end;
				Inc(Position);
			end;
		end else begin
			if (Source[Position] = '&') and (Position < Length(Source)) and IsIdentifierStart(Source[Position + 1]) then begin
				Result.Escaped := True;
				Inc(Position);
				start := Position;
			end;
			if IsIdentifierStart(Source[Position]) then begin
				Result.Kind := tkIdentifier;
				Inc(Position);
				while (Position <= Length(Source)) and IsIdentifierPart(Source[Position]) do begin
					Inc(Position);
				end;
			end else begin
				Result.Kind := tkSymbol;
				Inc(Position);
			end;
		end;
		Result.Text := Copy(Source, start, Position - start);
		Result.AfterPosition := Position;
		Exit;
	end;
	Result.Kind := tkEnd;
end;

function DirectiveName(const _token : TToken) : string;
begin
	Result := UpperCase(_token.Text);
	var index := 1;
	while (index <= Length(Result)) and CharInSet(Result[index], ['A'..'Z']) do begin
		Inc(index);
	end;
	Result := Copy(Result, 1, index - 1);
end;

function IsConditionalOrInclude(const _token : TToken) : Boolean;
begin
	var name := DirectiveName(_token);
	Result := (name = 'IF') or (name = 'IFDEF') or (name = 'IFNDEF') or (name = 'IFOPT') or
		(name = 'ELSE') or (name = 'ELSEIF') or (name = 'ENDIF') or (name = 'IFEND') or
		(name = 'I') or (name = 'INCLUDE');
end;

function THeaderReader.NextHeaderToken() : TToken;
begin
	Result := Next();
	while (Result.Kind = tkDirective) and not IsConditionalOrInclude(Result) do begin
		Result := Next();
	end;
end;

function ReadUnitName(var _reader : THeaderReader; var _token : TToken; out _name : string) : Boolean;
begin
	Result := False;
	_name := '';
	if _token.Kind <> tkIdentifier then begin
		Exit;
	end;
	_name := _token.Text;
	_token := _reader.NextHeaderToken();
	while _token.IsText('.') do begin
		_token := _reader.NextHeaderToken();
		if _token.Kind <> tkIdentifier then begin
			Exit;
		end;
		_name := _name + '.' + _token.Text;
		_token := _reader.NextHeaderToken();
	end;
	Result := True;
end;

function ReadUses(var _reader : THeaderReader; var _token : TToken; const _unitName : string;
	out _semicolon : Integer; out _found : Boolean) : Boolean;
var
	name : string;
begin
	Result := True;
	_semicolon := 0;
	_found := False;
	if _token.Kind in [tkDirective, tkInvalid, tkEnd] then begin
		Exit(False);
	end;
	if not _token.IsText('uses') then begin
		Exit;
	end;
	repeat
		_token := _reader.NextHeaderToken();
		if not ReadUnitName(_reader, _token, name) then begin
			Exit(False);
		end;
		_found := _found or SameText(name, _unitName);
		if _token.IsText('in') then begin
			_token := _reader.NextHeaderToken();
			if _token.Kind <> tkString then begin
				Exit(False);
			end;
			_token := _reader.NextHeaderToken();
		end;
		if _token.IsText(';') then begin
			_semicolon := _token.Position;
			Exit;
		end;
		if not _token.IsText(',') then begin
			Exit(False);
		end;
	until False;
end;

class function TPascalUses.PlanInsertion(const _source, _unitName : string; _toImplementation : Boolean;
	out _insertion : TUsesInsertion) : TAddUnitResult;
var
	reader, nameReader : THeaderReader;
	token, nameToken : TToken;
	name : string;
	interfaceEnd, implementationEnd, interfaceUsesEnd, implementationUsesEnd, depth : Integer;
	inInterface, inImplementation : Boolean;
	lineEnding : string;
begin
	Result := aurUnsupportedSource;
	_insertion := Default(TUsesInsertion);
	nameReader.Source := _unitName;
	nameReader.Position := 1;
	nameToken := nameReader.Next();
	if not ReadUnitName(nameReader, nameToken, name) or (name <> _unitName) or (nameToken.Kind <> tkEnd) then begin
		Exit;
	end;
	reader.Source := _source;
	reader.Position := 1;
	token := reader.NextHeaderToken();
	if not token.IsText('unit') then begin
		Exit;
	end;
	token := reader.NextHeaderToken();
	if not ReadUnitName(reader, token, name) or not token.IsText(';') then begin
		Exit;
	end;
	if SameText(name, _unitName) then begin
		Exit(aurSameUnit);
	end;
	token := reader.NextHeaderToken();
	if not token.IsText('interface') then begin
		Exit;
	end;
	interfaceEnd := token.AfterPosition;
	token := reader.NextHeaderToken();
	if not ReadUses(reader, token, _unitName, interfaceUsesEnd, inInterface) then begin
		Exit;
	end;
	depth := 0;
	while not token.IsText('implementation') do begin
		if token.Kind in [tkEnd, tkInvalid] then begin
			Exit;
		end;
		if token.Kind = tkDirective then begin
			var directive := DirectiveName(token);
			if (directive = 'I') or (directive = 'INCLUDE') then begin
				Exit;
			end;
			if (directive = 'IF') or (directive = 'IFDEF') or (directive = 'IFNDEF') or (directive = 'IFOPT') then begin
				Inc(depth);
			end else if (directive = 'ENDIF') or (directive = 'IFEND') then begin
				Dec(depth);
				if depth < 0 then begin
					Exit;
				end;
			end;
		end;
		token := reader.Next();
	end;
	if depth <> 0 then begin
		Exit;
	end;
	implementationEnd := token.AfterPosition;
	token := reader.NextHeaderToken();
	if not ReadUses(reader, token, _unitName, implementationUsesEnd, inImplementation) then begin
		Exit;
	end;
	if inInterface then begin
		Exit(aurInInterface);
	end;
	if inImplementation then begin
		Exit(aurInImplementation);
	end;
	lineEnding := sLineBreak;
	for var index := 1 to Length(_source) do begin
		if _source[index] = #13 then begin
			lineEnding := #13;
			if (index < Length(_source)) and (_source[index + 1] = #10) then begin
				lineEnding := #13#10;
			end;
			Break;
		end;
		if _source[index] = #10 then begin
			lineEnding := #10;
			Break;
		end;
	end;
	var sectionEnd := interfaceEnd;
	var usesEnd := interfaceUsesEnd;
	if _toImplementation then begin
		sectionEnd := implementationEnd;
		usesEnd := implementationUsesEnd;
	end;
	if usesEnd > 0 then begin
		_insertion.Position := usesEnd - 1;
		_insertion.Text := ',' + lineEnding + #9 + _unitName;
	end else begin
		_insertion.Position := sectionEnd - 1;
		_insertion.Text := lineEnding + lineEnding + 'uses' + lineEnding + #9 + _unitName + ';';
	end;
	Result := aurAdded;
end;

end.
