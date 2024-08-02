unit RipGrepper.CommandLine.Builder;

interface

uses
	RipGrepper.Common.Constants,
	RipGrepper.Common.GuiSearchParams,
	RipGrepper.Common.Settings.RipGrepParameterSettings,
	System.Classes,
	ArrayEx;

type
	TCommandLineBuilder = class
		private
			class procedure AddArgs(var _params : TRipGrepParameterSettings; const _sName : string; const _args : TArray<string>;
				const _bQuote : Boolean = False); static;
			// TODO: RemoveParam
			// class function RemoveParam(var arrRgOptions : TArrayEx<string>; const _paramRegex : string) : Boolean; static;

		public
			class function FileMasksToOptions(const _arrMasks, _arrSkipMasks : TArrayEx<string>) : string; static;
			class function GetFileMaskParamsArrFromDelimitedText(const _sFileMasksDelimited : string; const _sSeparator : string = ';') : TArray<string>;
				overload; static;
			class function GetFileMaskParamsFromDelimitedText(const _sFileMasksDelimited : string; const _sSeparator : string = ';') : string; overload; static;
			class function GetFileMaskParamsFromOptions(const _sOptions : string) : TArray<string>; static;
			class function GetFileMasksDelimited(const _sOptions : string) : string; static;
			class function GetMissingFileMaskOptions(const _sOptions, _sMasks : string) : string; static;
			class procedure RebuildArguments(var _params : TRipGrepParameterSettings); static;
			class procedure RemoveWordBoundaries(var _s : string); static;
	end;

implementation

uses
	System.SysUtils,

	RipGrepper.Tools.ProcessUtils,
	System.RegularExpressions,
	RipGrepper.Helper.Types,
	RipGrepper.CommandLine.OptionHelper,
	RipGrepper.Tools.DebugUtils,
	RipGrepper.Common.IOTAUtils;

class procedure TCommandLineBuilder.AddArgs(var _params : TRipGrepParameterSettings; const _sName : string; const _args : TArray<string>;
	const _bQuote : Boolean = False);
begin
	for var s : string in _args do begin
		if not s.IsEmpty then begin
			TDebugUtils.DebugMessage('TCommandLineBuilder.AddArgs: ' + s);
			if _bQuote then begin
				_params.RipGrepArguments.AddPair(_sName, TProcessUtils.MaybeQuoteIfNotQuoted(s));
			end else begin
				_params.RipGrepArguments.AddPair(_sName, s);
			end;
		end;
	end;
	// TDebugUtils.DebugMessage('AddArgs: ' + string.Join(' ', _params.RipGrepArguments.GetValues()));
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

class function TCommandLineBuilder.GetFileMaskParamsArrFromDelimitedText(const _sFileMasksDelimited : string; const _sSeparator : string = ';')
	: TArray<string>;
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

class function TCommandLineBuilder.GetFileMaskParamsFromDelimitedText(const _sFileMasksDelimited : string; const _sSeparator : string = ';') : string;
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
			fileMask := fileMask + ';' + TOptionsHelper.GetOptionValue(sOp);
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
	arrPaths : TArrayEx<string>;
	paramCount : integer;
begin
	TDebugUtils.DebugMessage('TCommandLineBuilder.RebuildArguments: start');
	_params.RipGrepArguments.Clear();

	TDebugUtils.DebugMessage('TCommandLineBuilder.RebuildArguments: add ' + _params.GuiSearchTextParams.RgAdditionalOptions);
	arrRgOptions := _params.GuiSearchTextParams.RgAdditionalOptions.Split([' ']);

	paramCount := arrRgOptions.Count;
	for var s in _params.GuiSearchTextParams.RgOptions.Split([' ']) do begin
		arrRgOptions.InsertIfNotContains(paramCount, s);
		TDebugUtils.DebugMessage('TCommandLineBuilder.RebuildArguments: add search text param: ' + s);
	end;

	paramCount := arrRgOptions.Count;
	for var s in RG_NECESSARY_PARAMS do begin
		arrRgOptions.InsertIfNotContains(paramCount, s);
		TDebugUtils.DebugMessage('TCommandLineBuilder.RebuildArguments: add necessary param: ' + s);
	end;

	arrRgOptions.AddRange(GetFileMaskParamsArrFromDelimitedText(_params.FileMasks));
	TDebugUtils.DebugMessage('TCommandLineBuilder.RebuildArguments: add all mask param: ' + _params.FileMasks);

	arrRgOptions.RemoveAll(RG_PARAM_END);
	// put it in the end
	arrRgOptions.Add(RG_PARAM_END); // indicates that no more flags will be provided

	_params.RgExeOptions := string.Join(' ', arrRgOptions.Items);

	AddArgs(_params, RG_ARG_OPTIONS, arrRgOptions);
	AddArgs(_params, RG_ARG_SEARCH_TEXT, [_params.GuiSearchTextParams.SearchText]); // order is important!

	for var s in _params.SearchPath.Split([SEARCH_PATH_SEPARATOR]) do begin
		arrPaths.Add(s);
		TDebugUtils.DebugMessage('TCommandLineBuilder.RebuildArguments: Path added:' + s);
	end;
	AddArgs(_params, RG_ARG_SEARCH_PATH, arrPaths, True { Quote if necessary } );

	TDebugUtils.DebugMessage('TCommandLineBuilder.RebuildArguments: after AddArgs: ' + string.Join(' ', _params.RipGrepArguments.GetValues()));
	TDebugUtils.DebugMessage('TCommandLineBuilder.RebuildArguments: GuiSearchTextParams end ' + _params.GuiSearchTextParams.ToString);
end;

// TODO: RemoveParam
// class function TCommandLineBuilder.RemoveParam(var arrRgOptions : TArrayEx<string>; const _paramRegex : string) : Boolean;
// begin
// Result := False;
// for var p in _paramRegex.Split(['|']) do begin
// if arrRgOptions.Remove(p) then begin
// Result := True;
// end;
// end;
// end;

end.
