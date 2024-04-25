unit RipGrepper.CommandLine.Builder;

interface

uses
	RipGrepper.Common.Settings.RipGrepParameterSettings,
	System.Classes,
	ArrayEx,
	RipGrepper.Common.Constants;

type
	TCommandLineBuilder = class
		private
			class procedure AddArgs(var _params : TRipGrepParameterSettings; const _sName : string; const _args : TArray<string>;
				const _bQuote : Boolean = False); static;
			class function RemoveFixedStringsParam(var arrRgOptions : TArrayEx<string>) : Boolean; static;

		public
			class function FileMasksToOptions(const _arrMasks, _arrSkipMasks : TArrayEx<string>) : string; static;
			class function GetFileMaskParamsArrFromDelimitedText(const _sFileMasksDelimited : string; const _sSeparator : string = ';')
				: TArray<string>; overload; static;
			class function GetFileMaskParamsFromDelimitedText(const _sFileMasksDelimited : string; const _sSeparator : string = ';')
				: string; overload; static;
			class function GetFileMaskParamsFromOptions(const _sOptions : string) : TArray<string>; static;
			class function GetFileMasksDelimited(const _sOptions : string) : string; static;
			class function GetMissingFileMaskOptions(const _sOptions, _sMasks : string) : string; static;
			class procedure RebuildArguments(var _params : TRipGrepParameterSettings); static;
			class procedure RemoveWordBoundaries(var _s : string); static;
			class procedure UpdateSearchTextAndRgExeOptions(var _params : TGuiSetSearchParams; var arrRgOptions : TArrayEx<string>); static;
	end;

implementation

uses
	System.SysUtils,

	RipGrepper.Tools.ProcessUtils,
	System.RegularExpressions,
	RipGrepper.Helper.Types,
	RipGrepper.CommandLine.OptionHelper;

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
		if TOptionsHelper.IsOptionWithValue(sOp, RG_PARAM_REGEX_GLOB) then begin
			fileMask := fileMask + ';' + TOptionsHelper.GetOptionsValue(sOp);
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

class procedure TCommandLineBuilder.RemoveWordBoundaries(var _s : string);
begin
	if (_s.StartsWith(WB, True) and _s.EndsWith(WB, True)) then begin
		_s := _s.Replace(WB, '', [rfReplaceAll, rfIgnoreCase]);
	end;
end;

class procedure TCommandLineBuilder.RebuildArguments(var _params : TRipGrepParameterSettings);
var
	arrRgOptions : TArrayEx<string>;
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
	UpdateSearchTextAndRgExeOptions(gsp, arrRgOptions);
	_params.GuiSetSearchParams := gsp;
	_params.RgExeOptions := string.Join(' ', arrRgOptions.Items);

	AddArgs(_params, RG_ARG_OPTIONS, arrRgOptions);
	AddArgs(_params, RG_ARG_SEARCH_TEXT, [gsp.SearchText]); // order is important!
	AddArgs(_params, RG_ARG_SEARCH_PATH, _params.SearchPath.Split([';']), True { Quote if necessary } );
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

class procedure TCommandLineBuilder.UpdateSearchTextAndRgExeOptions(var _params : TGuiSetSearchParams; var arrRgOptions : TArrayEx<string>);
var
	newSearchText : string;
	bRemoved : Boolean;
	bitField : TBitField;
begin
	newSearchText := _params.SearchText;
	bitField := _params.SearchOptionsAsBitField;

	if _params.IsSet([EGuiOption.soUseRegex]) then begin
		bRemoved := RemoveFixedStringsParam(arrRgOptions);
		if bRemoved then begin
			_params.EscapedSearchText := TRegEx.Escape(newSearchText); // regex chars like $ should be escaped
			newSearchText := _params.EscapedSearchText;
		end;
	end else begin
		arrRgOptions.InsertIfNotContains(0, RG_PARAM_REGEX_FIXED_STRINGS.Split(['|'])[1]);
	end;

	if _params.IsSet([EGuiOption.soMatchWord]) then begin
		TOptionsHelper.PutBetweenWordBoundaries(newSearchText);
		RemoveFixedStringsParam(arrRgOptions);
	end;

	_params.SearchText := newSearchText;
end;

end.
