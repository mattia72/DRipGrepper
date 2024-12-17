unit RipGrepper.CommandLine.OptionStrings;

interface

uses
	ArrayEx,
	System.Classes;

type
	TOptionVariants = record
		private
			FShort : string;
			procedure SetShort(const Value : string);

		public
			Long : string;
			Value : string;
			HasValue : Boolean;
			function ToLongString : string;
			function ToShortString : string;
			property Short : string read FShort write SetShort;
	end;

	TOptionStrings = record
		private
			FOptions : TArrayEx<string>;
			procedure RemoveAll(const _sParam : string; _bAsIs : Boolean = False);
			function GetMatchedIndexes(const _sParamRegex : string) : TArrayEx<integer>;
			function GetFirsMatchedIndex(const _sParamRegex : string) : integer;
			function IsConcreteOptionSet(const _sOption : string; const _sParamValue : string = '') : Boolean;
			procedure RemoveAllAsBoundedParam(const _sParam : string);
			procedure ValidateOptions;

		public
			class function New(const _sOptions : string) : TOptionStrings; overload; static;
			class function New(const _arrOptions : TArrayEx<string>) : TOptionStrings; overload; static;

			function AddOption(const _sParamRegex : string) : string;
			procedure AddOptionWithValue(const _paramRegex : string = ''; const _sValue : string = ''; const _bUnique : Boolean = False);
			function AsArray : TArrayEx<string>;
			class function ToArray(const _sOptions : string) : TArrayEx<string>; static;
			function AsString : string;
			function GetOptionValue(const _sOptionRegex : string; var _sValue : string) : Boolean;
			class function GetOptionVariantsAndValue(const _sParamRegex : string; out _opVariants : TOptionVariants) : Boolean; static;
			function IsOptionSet(_sParamRegex : string; const _sParamEqualValue : string = '') : Boolean; overload;
			function IsSetOptionWithValue(const _sOption : string; const _sValue : string = '') : Boolean;
			class function MaybeQuoteIfNotQuoted(const _s : string; const _delimiter : char = '"') : string; static;
			class function MaybeDeQuoteIfQuoted(const _s : string; const _delimiter : char = '"') : string; static;
			procedure RemoveOption(const _paramRegex : string);
			procedure RemoveOptions(const _paramRegexes : TArrayEx<string>);
			procedure UpdateFileMasks(_sNewMasks : string); overload;
	end;

implementation

uses
	System.RegularExpressions,
	System.SysUtils,
	RipGrepper.Common.Constants,
	RipGrepper.CommandLine.OptionHelper,
	RipGrepper.CommandLine.Builder,
	System.StrUtils;

function TOptionStrings.AddOption(const _sParamRegex : string) : string;
begin
	AddOptionWithValue(_sParamRegex);
	ValidateOptions();
	Result := AsString;
end;

procedure TOptionStrings.AddOptionWithValue(const _paramRegex : string = ''; const _sValue : string = ''; const _bUnique : Boolean = False);
var
	params : TArrayEx<string>;
	iFoundIdx : integer;
	sValue : string;
begin
	if not _paramRegex.IsEmpty then begin
		params := _paramRegex.Split(['|']);
		if _sValue.IsEmpty then begin
			iFoundIdx := GetFirsMatchedIndex('^(' + _paramRegex + ')$');
			if (iFoundIdx < 0) then begin
				FOptions.Insert(0, params[RG_PARAM_LONG_INDEX]);
			end;
		end else begin
			iFoundIdx := GetFirsMatchedIndex(Format(RG_PARAM_REGEX_VALUE_FORMAT, [_paramRegex]));
			if (iFoundIdx >= 0) and _bUnique then begin
				FOptions.Delete(iFoundIdx);
			end;
			sValue := _sValue;
			if _sValue.IndexOf(' ') <> -1 then begin
				sValue := MaybeQuoteIfNotQuoted(_sValue);
			end else if _sValue = QuotedStr('') then begin
				sValue := '';
			end;
			FOptions.Insert(0, params[RG_PARAM_LONG_INDEX] + '=' + sValue);
		end;
	end;
end;

function TOptionStrings.AsArray : TArrayEx<string>;
begin
	Result := FOptions;
end;

class function TOptionStrings.ToArray(const _sOptions : string) : TArrayEx<string>;
var
	sLastAllOptions : string;
begin
	var
	arr := _sOptions.Split([' '], TStringSplitOptions.ExcludeEmpty);
	var
	regex := RG_PARAM_WORD_IN_OPTION_LIST;
	for var s in arr do begin
//		var
//		sEsc := EscapeMaskChars(s);
		if TRegEx.IsMatch(s, regex) then begin
			Result.AddIfNotContains(s);
		end else begin
			if Result.Count > 0 then begin
				sLastAllOptions := Result.Last;
				Result.Delete(Result.MaxIndex);
				Result.Add(sLastAllOptions + ' ' + s);
			end else begin
				Result.Add(s);
			end;
		end;
	end;
end;

function TOptionStrings.AsString : string;
begin
	Result := string.Join(' ', FOptions.Items);
end;

procedure TOptionStrings.RemoveAll(const _sParam : string; _bAsIs : Boolean = False);
begin
	if _sParam.IsEmpty then begin
		Exit;
	end;
	if (_sParam = RG_PARAM_END) or (_sParam = '-') or (_sParam = '-.') or _bAsIs then begin
		FOptions.RemoveAll(_sParam);
	end else begin
		RemoveAllAsBoundedParam(_sParam);
	end;
end;

function TOptionStrings.GetMatchedIndexes(const _sParamRegex : string) : TArrayEx<integer>;
begin
	for var i := 0 to FOptions.MaxIndex do begin
		if (not FOptions[i].IsEmpty) and TRegEx.IsMatch(FOptions[i], _sParamRegex) then begin
			Result.Add(i);
		end;
	end;
end;

function TOptionStrings.GetFirsMatchedIndex(const _sParamRegex : string) : integer;
begin
	Result := -1;
	for var i := 0 to FOptions.MaxIndex do begin
		if TRegEx.IsMatch(FOptions[i], _sParamRegex) then begin
			Result := i;
			break;
		end;
	end;
end;

function TOptionStrings.GetOptionValue(const _sOptionRegex : string; var _sValue : string) : Boolean;
var
	sOp : string;
	sOpName : string;
begin
	Result := False;
	for var i : integer := 0 to FOptions.MaxIndex do begin
		sOp := FOptions[i];
		if TRegex.IsMatch(sOp, _sOptionRegex) and TOptionsHelper.IsOptionWithValue(sOp) then begin
			_sValue := TOptionsHelper.GetOptionValue(sOp, sOpName);
			Result := True;
			break;
		end;
	end;

end;

class function TOptionStrings.GetOptionVariantsAndValue(const _sParamRegex : string; out _opVariants : TOptionVariants) : Boolean;
var
	m : TMatch;
begin
	Result := False;
	// if _sParamRegex = RG_PARAM_END then begin
	// _opVariants.Short := RG_PARAM_END;
	// _opVariants.Long := RG_PARAM_END;
	// _opVariants.Value := '';
	// _opVariants.HasValue := False;
	// Result := True;
	// Exit;
	// end;
	m := TRegex.Match(_sParamRegex, RG_PARAM_REGEX_VARIANT_WITH_OPTIONAL_VALUE);
	if m.Success then begin
		case m.Groups.Count of
			4 : begin
				_opVariants.Short := m.Groups[1].Value;
				_opVariants.Long := m.Groups[2].Value;
				_opVariants.Value := m.Groups[3].Value;
				_opVariants.HasValue := True;
			end;
			3 : begin
				_opVariants.Short := m.Groups[1].Value;
				_opVariants.Long := m.Groups[2].Value;
				_opVariants.Value := '';
				_opVariants.HasValue := _sParamRegex.IndexOf('=') >= 0;
			end;
		end;
		Result := True;
		Exit;
	end;

	m := TRegex.Match(_sParamRegex, RG_PARAM_REGEX_SINGLE_WITH_OPTIONAL_VALUE);
	if m.Success then begin
		case m.Groups.Count of
			3 : begin
				var
				val := m.Groups[1].Value;
				_opVariants.Short := IfThen(val.StartsWith('--'), '', val);
				_opVariants.Long := IfThen(val.StartsWith('--'), val, '');
				_opVariants.Value := m.Groups[2].Value;
				_opVariants.HasValue := True;
			end;
			2 : begin
				var
				val := m.Groups[1].Value;
				_opVariants.Short := IfThen(val.StartsWith('--'), '', val);
				_opVariants.Long := IfThen(val.StartsWith('--'), val, '');
				_opVariants.Value := '';
				_opVariants.HasValue := _sParamRegex.IndexOf('=') >= 0;
			end;
		end;
		Result := True;
	end;
end;

function TOptionStrings.IsOptionSet(_sParamRegex : string; const _sParamEqualValue : string = '') : Boolean;
var
	arrOptions : TOptionVariants;
begin
	Result := True; // eg. in case of switch params like -F _sParamEqualValue is empty
	if not _sParamRegex.IsEmpty then begin
		if GetOptionVariantsAndValue(_sParamRegex, arrOptions) then begin
			Result := IsConcreteOptionSet(arrOptions.Short, _sParamEqualValue) or
			{ } IsConcreteOptionSet(arrOptions.Long, _sParamEqualValue);
		end else begin
			Result := IsConcreteOptionSet(_sParamRegex, _sParamEqualValue);
		end;
	end;
end;

function TOptionStrings.IsSetOptionWithValue(const _sOption : string; const _sValue : string = '') : Boolean;
begin
	Result := TRegEx.IsMatch(AsString, TOptionsHelper.GetBoundedParamWithValueRegex(_sOption, _sValue));
end;

class function TOptionStrings.MaybeQuoteIfNotQuoted(const _s : string; const _delimiter : char = '"') : string;
begin
	if (Pos(' ', _s) <> 0) and not TRegEx.IsMatch(_s, '^' + _delimiter + '.*' + _delimiter + '$') then begin
		Result := _s.QuotedString(_delimiter);
	end else begin
		Result := _s;
	end;
end;

class function TOptionStrings.New(const _sOptions : string) : TOptionStrings;
begin
	Result.FOptions := TOptionStrings.ToArray(_sOptions);
end;

function TOptionStrings.IsConcreteOptionSet(const _sOption : string; const _sParamValue : string = '') : Boolean;
begin
	Result := False;
	var
	sOpRegex := TOptionsHelper.GetBoundedParamRegex(_sOption);
	if TRegEx.IsMatch(AsString, sOpRegex) then begin
		if IsSetOptionWithValue(_sOption) then begin
			if IsSetOptionWithValue(_sOption, _sParamValue) then begin
				Result := True;
			end;
		end else begin
			Result := True;
		end;
	end;
end;

class function TOptionStrings.MaybeDeQuoteIfQuoted(const _s : string; const _delimiter : char = '"') : string;
begin
	if (Pos(' ', _s) <> 0) and TRegEx.IsMatch(_s, '^' + _delimiter + '.*' + _delimiter + '$') then begin
		Result := _s.DeQuotedString(_delimiter);
	end else begin
		Result := _s;
	end;
end;

class function TOptionStrings.New(const _arrOptions : TArrayEx<string>) : TOptionStrings;
begin
	Result.FOptions := _arrOptions;
end;

procedure TOptionStrings.RemoveAllAsBoundedParam(const _sParam : string);
var
	regexBoundedParam : string;
	regexBoundedParamWithValue : string;
	idxArr : TArrayEx<integer>;
begin
	regexBoundedParam := TOptionsHelper.GetBoundedParamRegex(_sParam);
	regexBoundedParamWithValue := TOptionsHelper.GetBoundedParamWithValueRegex(_sParam);
	idxArr := GetMatchedIndexes(regexBoundedParamWithValue);
	if idxArr.IsEmpty then begin
		idxArr := GetMatchedIndexes(regexBoundedParam);
	end;
	FOptions.Delete(idxArr);
end;

procedure TOptionStrings.RemoveOption(const _paramRegex : string);
var
	params : TOptionVariants;
begin
	if GetOptionVariantsAndValue(_paramRegex, params) then begin
		if params.HasValue then begin
			RemoveAll(params.ToShortString, True);
			RemoveAll(params.ToLongString, True);
		end else begin
			RemoveAll(params.Short);
			RemoveAll(params.Long);
		end;
	end else begin
		RemoveAll(_paramRegex, True); // so the whole option item should match
	end;
end;

procedure TOptionStrings.RemoveOptions(const _paramRegexes : TArrayEx<string>);
begin
	for var s in _paramRegexes do begin
		RemoveOption(s);
	end;
end;

procedure TOptionStrings.UpdateFileMasks(_sNewMasks : string);
var
	oldMaskOptions : TArrayEx<string>;
	newMaskOptions : string;
	sOptions : string;
begin
	sOptions := AsString;
	oldMaskOptions := TCommandLineBuilder.GetFileMaskParamsFromOptions(sOptions);
	newMaskOptions := TCommandLineBuilder.GetFileMaskParamsFromDelimitedText(_sNewMasks, ';');

	RemoveAll(RG_PARAM_REGEX_GLOB);
	sOptions := AsString() + ' ' + newMaskOptions.Trim([' ']);
	FOptions := ToArray(sOptions);
end;

procedure TOptionStrings.ValidateOptions;
begin
	var
	arr := GetMatchedIndexes(TOptionsHelper.GetBoundedParamRegex(RG_PARAM_END));
	Assert(arr.Count <= 1, AsString() + CRLF + 'Option list is corrupt. -- should appear only once!');
end;

procedure TOptionVariants.SetShort(const Value : string);
begin
	FShort := Value.Replace('\', '');
end;

function TOptionVariants.ToLongString : string;
begin
	Result := Long + '=' + Value;
end;

function TOptionVariants.ToShortString : string;
begin
	Result := Short + '=' + Value;
end;

end.
