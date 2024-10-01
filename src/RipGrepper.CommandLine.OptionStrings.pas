unit RipGrepper.CommandLine.OptionStrings;

interface

uses
	ArrayEx,
	System.Classes;

type
	TOptionStrings = record
		FOptions : TArrayEx<string>;
		class function New(const _sOptions : string) : TOptionStrings; static;

		private
			procedure DeleteAll(const _sParam : string);
			function GetMatchedIndexes(const _sParamRegex : string) : TArrayEx<integer>;
			function GetFirsMatchedIndex(const _sParamRegex : string) : integer;
			function IsConcreteOptionSet(const _sOption : string; const _sParamValue : string = '') : Boolean;
			procedure ValidateOptions;

		public
			function AddOption(const _sParamRegex : string) : string;
			procedure AddOptionWithValue(const _paramRegex : string = ''; const _sValue : string = ''; const _bUnique : Boolean = False);
			function AsArray : TArrayEx<string>;
			class function ToArray(const _sOptions : string) : TArrayEx<string>; static;
			function AsString : string;
			function GetOptionValue(const _sOptionRegex : string; var _sValue : string) : Boolean;
			function IsOptionSet(_sParamRegex : string; const _sParamValue : string = '') : Boolean; overload;
			function IsSetOptionWithValue(const _sOption : string; const _sValue : string = '') : Boolean;
			class function MaybeQuoteIfNotQuoted(const _s : string; const _delimiter : string = '"') : string; static;
			procedure RemoveOption(const _paramRegex : string);
			procedure UpdateFileMasks(_sNewMasks : string); overload;
	end;

implementation

uses
	System.RegularExpressions,
	System.SysUtils,
	RipGrepper.Common.Constants,
	RipGrepper.CommandLine.OptionHelper,
	RipGrepper.CommandLine.Builder;

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
begin
	var
	arr := _sOptions.Split([' '], TStringSplitOptions.ExcludeEmpty);
	for var s in arr do begin
		var
		regex := '^--?[-\w]+(=[''"]?[!.*\/\w -]+[''"]?)?$';
		if TRegEx.IsMatch(s, regex) then begin
			Result.Add(s);
		end else begin
			var
			last := Result.Last;
			Result.Delete(Result.MaxIndex);
			Result.Add(last + ' ' + s);
		end;
	end;
end;

function TOptionStrings.AsString : string;
begin
	Result := string.Join(' ', FOptions.Items);
end;

procedure TOptionStrings.DeleteAll(const _sParam : string);
var
	regexBoundedParam : string;
	regexBoundedParamWithValue : string;
	idxArr : TArrayEx<integer>;
begin
	if (_sParam = RG_PARAM_END) or (_sParam = '-') then begin
		FOptions.RemoveAll(_sParam);
	end else begin;
		regexBoundedParam := TOptionsHelper.GetBoundedParamRegex(_sParam);
		regexBoundedParamWithValue := TOptionsHelper.GetBoundedParamWithValueRegex(_sParam);
		idxArr := GetMatchedIndexes(regexBoundedParam);
		idxArr.AddRange(GetMatchedIndexes(regexBoundedParamWithValue));
		FOptions.Delete(idxArr);
	end;
end;

function TOptionStrings.GetMatchedIndexes(const _sParamRegex : string) : TArrayEx<integer>;
begin
	for var i := 0 to FOptions.MaxIndex do begin
		if TRegEx.IsMatch(FOptions[i], _sParamRegex) then begin
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

function TOptionStrings.IsOptionSet(_sParamRegex : string; const _sParamValue : string = '') : Boolean;
var
	arrOptions : TArrayEx<string>;
begin
	Result := True;
	if not _sParamRegex.IsEmpty then begin
		arrOptions := _sParamRegex.Split(['|']);
		Result := IsConcreteOptionSet(arrOptions[RG_PARAM_SHORT_INDEX]) or IsConcreteOptionSet(arrOptions[RG_PARAM_LONG_INDEX]);
	end;
end;

function TOptionStrings.IsSetOptionWithValue(const _sOption : string; const _sValue : string = '') : Boolean;
begin
	Result := TRegEx.IsMatch(AsString, TOptionsHelper.GetBoundedParamWithValueRegex(_sOption, _sValue));
end;

class function TOptionStrings.MaybeQuoteIfNotQuoted(const _s : string; const _delimiter : string = '"') : string;
begin
	if (Pos(' ', _s) <> 0) and not TRegEx.IsMatch(_s, '^' + _delimiter + '.*' + _delimiter + '$') then begin
		Result := _delimiter + _s + _delimiter
		// _s.QuotedString ?
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

procedure TOptionStrings.RemoveOption(const _paramRegex : string);
var params : TArray<string>;
begin
	params := _paramRegex.Split(['|']);
	DeleteAll(params[RG_PARAM_SHORT_INDEX]);
	DeleteAll(params[RG_PARAM_LONG_INDEX]);
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

	DeleteAll(RG_PARAM_REGEX_GLOB);
	sOptions := AsString() + ' ' + newMaskOptions.Trim([' ']);
	FOptions := ToArray(sOptions);
end;

procedure TOptionStrings.ValidateOptions;
begin
	var
	arr := GetMatchedIndexes(TOptionsHelper.GetBoundedParamRegex(RG_PARAM_END));
	Assert(arr.Count <= 1, AsString() + CRLF + 'Option list is corrupt. -- should appear only once!');
end;

end.
