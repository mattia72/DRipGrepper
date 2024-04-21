unit RipGrepper.Common.CommandLineBuilder;

interface

uses
	RipGrepper.Common.Settings.RipGrepParameterSettings,
	System.Classes,
	ArrayEx;

type
	TCommandLineBuilder = record

		private
			FParameters : TRipGrepParameterSettings;
			class procedure AddArgs(var _params : TRipGrepParameterSettings; const _sName : string; const _args : TArray<string>;
				const _bQuote : Boolean = False); static;
			class procedure AddParamToList(list : TStringList; const _paramRegex : string = ''); static;
			class function GetBoundedParamRegex(const _sOption : string) : string; static;
			class function GetBoundedParamWithValueRegex(const _sOption : string; const _sParamValue : string = '') : string; static;
			class procedure RemoveParamFromList(list : TStringList; const _paramRegex : string = ''); static;
			class procedure PutBetweenWordBoundaries(var _s : string); static;
			class function RemoveFixedStringsParam(var arrRgOptions : TArrayEx<string>) : Boolean; static;
		public
			class function FileMasksToOptions(const _arrMasks, _arrSkipMasks : TArrayEx<string>) : string; static;
			class function New(_params : TRipGrepParameterSettings) : TCommandLineBuilder; static;
			class function GetFileMaskParamsArrFromDelimitedText(const _sFileMasksDelimited : string; const _sSeparator : string = ';')
				: TArray<string>; overload; static;
			class function GetFileMaskParamsFromDelimitedText(const _sFileMasksDelimited : string; const _sSeparator : string = ';')
				: string; overload; static;
			class function GetFileMaskParamsFromOptions(const _sOptions : string) : TArray<string>; static;
			class function GetFileMasksDelimited(const _sOptions : string) : string; static;
			class function GetMissingFileMaskOptions(const _sOptions, _sMasks : string) : string; static;
			class function IsWordBoundOnOneSide(const _s : string) : Boolean; static;
			class function IsWordBoundOnBothSide(const _s : string) : Boolean; static;
			class function IsOptionSet(const _sOptions, _sParamRegex : string; const _sParamValue : string = '') : Boolean; static;
			class procedure RebuildArguments(var _params : TRipGrepParameterSettings); static;
			class function RemoveAllParams(const _sOptions, _argMaskRegex : string; const _bSwitch : Boolean = False) : string; static;
			class function AddRemoveRgExeOptions(const _sOptions : string; const _sParamRegex : string; const _bRemove : Boolean = False)
				: string; static;
			class function GetOptionsValue(const _sOption : string; var _sOptionName : string) : string; overload; static;
			class function GetOptionsValue(const _sOption : string) : string; overload; static;
			class function IsOptionWithValue(const _sOption : string; const _sOptionRegEx : string = '') : Boolean; static;
			class function IsSetOptionWithValue(const _sOptions, _sOption : string; const _sValue : string = '') : Boolean; static;
			class procedure RemoveWordBoundaries(var _s : string); static;
			class function UpdateSearchTextAndRgExeOptions(var _params : TGuiSetSearchParams; var arrRgOptions : TArrayEx<string>)
				: string; static;
			property Parameters : TRipGrepParameterSettings read FParameters write FParameters;
	end;

implementation

uses
	System.SysUtils,
	RipGrepper.Common.Constants,
	RipGrepper.Tools.ProcessUtils,
	System.RegularExpressions,
	RipGrepper.Helper.Types;

class function TCommandLineBuilder.New(_params : TRipGrepParameterSettings) : TCommandLineBuilder;
begin
	Result.Parameters := _params;
end;

class procedure TCommandLineBuilder.AddArgs(var _params : TRipGrepParameterSettings; const _sName : string; const _args : TArray<string>;
	const _bQuote : Boolean = False);
begin
	for var s : string in _args do begin
		if not s.IsEmpty then begin
			if _bQuote then begin
				_params.RipGrepArguments.AddPair(_sName, TProcessUtils.MaybeQuoteIfNotQuoted(s));
			end else begin
				_params.RipGrepArguments.AddPair(_sName, s);
			end;
		end;
	end;
end;

class procedure TCommandLineBuilder.AddParamToList(list : TStringList; const _paramRegex : string = '');
var
	params : TArrayEx<string>;
	iFoundIdx : integer;
begin
	if not _paramRegex.IsEmpty then begin
		params := _paramRegex.Split(['|']);
		iFoundIdx := list.IndexOfAny(params);
		if (iFoundIdx < 0) then begin
			list.Insert(0, params[1]); // long params
		end;
	end;
end;

class procedure TCommandLineBuilder.RemoveParamFromList(list : TStringList; const _paramRegex : string = '');
var
	params : TArrayEx<string>;
begin
	if not _paramRegex.IsEmpty then begin
		params := _paramRegex.Split(['|']);
		for var p in params do begin
			if p = RG_PARAM_END then begin
				list.DeleteAll([p]);
			end else begin
				var
				r := GetBoundedParamRegex(p);
				list.DeleteAllMatched(r);
				list.DeleteAllMatched(GetBoundedParamWithValueRegex(p));
			end;
		end;
	end;
end;

class function TCommandLineBuilder.FileMasksToOptions(const _arrMasks, _arrSkipMasks : TArrayEx<string>) : string;
var
	newOptions : string;
begin
	for var s in _arrMasks do begin
		if (not string.IsNullOrWhiteSpace(s)) and (not _arrSkipMasks.Contains(s)) then begin
			newOptions := newOptions + ' -g=' + s;
		end;
	end;
	Result := newOptions;
end;

class function TCommandLineBuilder.GetFileMaskParamsArrFromDelimitedText(const _sFileMasksDelimited : string;
	const _sSeparator : string = ';') : TArray<string>;
var
	list : TStringList;
begin
	list := TStringList.Create(dupIgnore, False, True);
	list.Delimiter := ' ';
	try
		for var s : string in _sFileMasksDelimited.Split([_sSeparator]) do begin
			list.AddIfNotContains('-g=' + s);
		end;
		Result := list.ToStringArray;
	finally
		list.Free;
	end;
end;

class function TCommandLineBuilder.GetFileMaskParamsFromDelimitedText(const _sFileMasksDelimited : string;
	const _sSeparator : string = ';') : string;
begin
	Result := string.Join(' ', GetFileMaskParamsArrFromDelimitedText(_sFileMasksDelimited, _sSeparator));
end;

class function TCommandLineBuilder.GetFileMaskParamsFromOptions(const _sOptions : string) : TArray<string>;
begin
	Result := GetFileMasksDelimited(_sOptions).Split([';']);
end;

class function TCommandLineBuilder.GetFileMasksDelimited(const _sOptions : string) : string;
var
	fileMask : string;
begin
	for var sOp in _sOptions.Split([' ']) do begin
		if IsOptionWithValue(sOp, RG_PARAM_REGEX_GLOB) then begin
			fileMask := fileMask + ';' + GetOptionsValue(sOp);
		end;
	end;
	Result := fileMask.Trim([';', ' ']);
end;

class function TCommandLineBuilder.GetMissingFileMaskOptions(const _sOptions, _sMasks : string) : string;
var
	existingMasks : TArrayEx<string>;
	masksEdited : TArrayEx<string>;
	newOptions : string;
begin
	existingMasks := TCommandLineBuilder.GetFileMaskParamsFromOptions(_sOptions);
	masksEdited := _sMasks.Split([';']);

	newOptions := FileMasksToOptions(masksEdited, existingMasks);
	Result := newOptions.Trim;
end;

class function TCommandLineBuilder.IsWordBoundOnOneSide(const _s : string) : Boolean;
begin
	Result := (_s.StartsWith(WB, True) and not _s.EndsWith(WB, True))
	{ } or (not _s.StartsWith(WB, True) and _s.EndsWith(WB, True));
end;

class function TCommandLineBuilder.IsWordBoundOnBothSide(const _s : string) : Boolean;
begin
	Result := (_s.StartsWith(WB, True) and _s.EndsWith(WB, True));
end;

class function TCommandLineBuilder.IsOptionSet(const _sOptions, _sParamRegex : string; const _sParamValue : string = '') : Boolean;
var
	arrOptions : TArrayEx<string>;
begin
	Result := True;
	if not _sParamRegex.IsEmpty then begin
		Result := False;
		arrOptions := _sParamRegex.Split(['|']);
		for var i := 0 to arrOptions.MaxIndex do begin
			var
				sOp : string := arrOptions[i];
			var
			sOpRegex := GetBoundedParamRegex(sOp);
			if TRegEx.IsMatch(_sOptions, sOpRegex) then begin
				if IsSetOptionWithValue(_sOptions, sOp) then begin
					if TCommandLineBuilder.IsSetOptionWithValue(_sOptions, sOp, _sParamValue) then begin
						Result := True;
						break
					end;
				end else begin
					Result := True;
					break;
				end;
			end;
		end;
	end;
end;

class procedure TCommandLineBuilder.PutBetweenWordBoundaries(var _s : string);
begin
	if not(_s.StartsWith(WB, True) or _s.EndsWith(WB, True)) then begin
		_s := WB + _s + WB;
	end;
end;

class procedure TCommandLineBuilder.RemoveWordBoundaries(var _s : string);
begin
	if (_s.StartsWith(WB, True) and _s.EndsWith(WB, True)) then begin
		_s := _s.Replace(WB, '', [rfReplaceAll, rfIgnoreCase]);
	end;
end;

class procedure TCommandLineBuilder.RebuildArguments(var _params : TRipGrepParameterSettings);
var
	arrRgOptions : TArrayEx<string>;
	sSearchText : string;
	gsp : TGuiSetSearchParams;
begin
	_params.RipGrepArguments.Clear();
	arrRgOptions := _params.RgExeOptions.Split([' ']);

	for var s in RG_NECESSARY_PARAMS do begin
		arrRgOptions.InsertIfNotContains(0, s);
	end;

	arrRgOptions.AddRange(GetFileMaskParamsArrFromDelimitedText(_params.FileMasks));
	arrRgOptions.Remove(RG_PARAM_END);
	// put it in the end
	arrRgOptions.Add(RG_PARAM_END); // indicates that no more flags will be provided

	gsp := _params.GuiSetSearchParams;
	sSearchText := UpdateSearchTextAndRgExeOptions(gsp, arrRgOptions);
	_params.GuiSetSearchParams := gsp;
	_params.RgExeOptions := string.Join(' ', arrRgOptions.Items);

	AddArgs(_params, RG_ARG_OPTIONS, arrRgOptions);
	AddArgs(_params, RG_ARG_SEARCH_TEXT, [sSearchText]); // order is important!
	AddArgs(_params, RG_ARG_SEARCH_PATH, _params.SearchPath.Split([';']), True { Quote if necessary } );
end;

class function TCommandLineBuilder.RemoveAllParams(const _sOptions, _argMaskRegex : string; const _bSwitch : Boolean = False) : string;
var
	arrOptions : TArrayEx<string>;
	arrRemoveIdxs : TArrayEx<integer>;
begin
	arrOptions := _sOptions.Split([' ']);
	for var i := 0 to arrOptions.MaxIndex do begin
		if TRegEx.IsMatch(arrOptions[i], _argMaskRegex) then begin
			arrRemoveIdxs.Add(i);
		end;
	end;
	arrOptions.Delete(arrRemoveIdxs);
	Result := string.Join(' ', arrOptions.Items);
end;

class function TCommandLineBuilder.RemoveFixedStringsParam(var arrRgOptions : TArrayEx<string>) : Boolean;
begin
	Result := False;
	for var p in RG_PARAM_REGEX_FIXED_STRINGS.Split(['|']) do begin
		if arrRgOptions.Remove(p) then begin
			// word boundaries can't be used in case of --fixed-strings
			Result := True;
		end;
	end;
end;

class function TCommandLineBuilder.AddRemoveRgExeOptions(const _sOptions : string; const _sParamRegex : string;
	const _bRemove : Boolean = False) : string;
var
	listOptions : TStringList;
begin
	listOptions := TStringList.Create(dupIgnore, False, True);
	listOptions.Delimiter := ' ';
	try
		listOptions.AddStrings(_sOptions.Split([' '], TStringSplitOptions.ExcludeEmpty));

		if _bRemove then begin
			RemoveParamFromList(listOptions, _sParamRegex);
		end else begin
			AddParamToList(listOptions, _sParamRegex);
		end;
		Result := listOptions.DelimitedText;
	finally
		listOptions.Free;
	end;
end;

class function TCommandLineBuilder.GetBoundedParamRegex(const _sOption : string) : string;
begin
	Result := '-+' + WB + _sOption.TrimLeft(['-']) + WB;
end;

class function TCommandLineBuilder.GetBoundedParamWithValueRegex(const _sOption : string; const _sParamValue : string = '') : string;
begin
	Result := '-+' + WB + _sOption.TrimLeft(['-']) + '=' + TRegEx.Escape(_sParamValue);
end;

class function TCommandLineBuilder.GetOptionsValue(const _sOption : string; var _sOptionName : string) : string;
begin
	_sOptionName := _sOption.Remove(_sOption.IndexOf('='));
	Result := _sOption.Remove(0, _sOption.IndexOf('=') + 1);
end;

class function TCommandLineBuilder.GetOptionsValue(const _sOption : string) : string;
var
	sOpName : string;
begin
	Result := GetOptionsValue(_sOption, sOpName);
end;

class function TCommandLineBuilder.IsOptionWithValue(const _sOption : string; const _sOptionRegEx : string = '') : Boolean;
begin
	if _sOptionRegEx.IsEmpty then begin
		Result := TRegEx.IsMatch(_sOption, '-+[\w-]+=');
	end else begin
		Result := TRegEx.IsMatch(_sOption, '(' + _sOptionRegEx + ')=');
	end;
end;

class function TCommandLineBuilder.IsSetOptionWithValue(const _sOptions, _sOption : string; const _sValue : string = '') : Boolean;
begin
	Result := TRegEx.IsMatch(_sOptions, GetBoundedParamWithValueRegex(_sOption, _sValue));
end;

class function TCommandLineBuilder.UpdateSearchTextAndRgExeOptions(var _params : TGuiSetSearchParams;
	var arrRgOptions : TArrayEx<string>) : string;
var
	newSearchText : string;
	bRemoved : Boolean;
	bitField : TBitField;
begin
	newSearchText := _params.SearchText;
	bitField := _params.SearchOptionsAsBitField;

	if _params.IsSet([soUseRegex]) then begin
		bRemoved := RemoveFixedStringsParam(arrRgOptions);
		if bRemoved then begin
			_params.EscapedSearchText := TRegEx.Escape(newSearchText); // regex chars like $ should be escaped
			newSearchText := _params.EscapedSearchText;
		end;
	end else begin
		arrRgOptions.InsertIfNotContains(0, RG_PARAM_REGEX_FIXED_STRINGS.Split(['|'])[1]);
	end;

	if _params.IsSet([soMatchWord]) then begin
		PutBetweenWordBoundaries(newSearchText);
		RemoveFixedStringsParam(arrRgOptions);
	end;

	Result := newSearchText;
end;

end.
