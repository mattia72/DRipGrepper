unit RipGrepper.CommandLine.Builder;

interface

uses
	RipGrepper.Common.Constants,
	RipGrepper.Common.GuiSearchParams,
	RipGrepper.Settings.RipGrepParameterSettings,
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
			class function GetFileMaskParamsArrFromDelimitedText(const _sFileMasksDelimited : string; const _sSeparator : string = ';')
				: TArray<string>; overload; static;
			class function GetFileMaskParamsFromDelimitedText(const _sFileMasksDelimited : string; const _chSeparator : char = ';')
				: string; overload; static;
			class function GetFileMaskParamsFromOptions(const _sOptions : string; const _chSeparator : char = ';') : TArray<string>; static;
			class function GetUniqueFileMasksDelimited(const _sOptions : string) : string; static;
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
	{$IFNDEF STANDALONE} RipGrepper.Common.IOTAUtils, {$ENDIF}
	RipGrepper.CommandLine.OptionStrings;

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

class function TCommandLineBuilder.GetFileMaskParamsArrFromDelimitedText(const _sFileMasksDelimited : string;
	const _sSeparator : string = ';') : TArray<string>;
var
	fileMasks : TArrayEx<string>;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TCommandLineBuilder.GetFileMaskParamsArrFromDelimitedText');

	for var s : string in _sFileMasksDelimited.Split([_sSeparator]) do begin
		if 0 > fileMasks.AddIfNotContains('-g=' + s) then begin
			dbgMsg.Msg('duplicate file mask skipped:' + s)
		end;
	end;
	Result := fileMasks.Items;
end;

class function TCommandLineBuilder.GetFileMaskParamsFromDelimitedText(const _sFileMasksDelimited : string;
	const _chSeparator : char = ';') : string;
begin
	var
	s := _sFileMasksDelimited.Trim([' ', _chSeparator]);
	Result := string.Join(' ', GetFileMaskParamsArrFromDelimitedText(s, _chSeparator));
end;

class function TCommandLineBuilder.GetFileMaskParamsFromOptions(const _sOptions : string; const _chSeparator : char = ';') : TArray<string>;
begin
	var
	s := _sOptions.Trim([' ', _chSeparator]);
	Result := GetUniqueFileMasksDelimited(s).Split([_chSeparator]);
end;

class function TCommandLineBuilder.GetUniqueFileMasksDelimited(const _sOptions : string) : string;
var
	fileMasks : TArrayEx<string>;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TCommandLineBuilder.GetUniqueFileMasksDelimited');

	for var sOp in _sOptions.Split([' ']) do begin
		if TOptionsHelper.IsOptionWithValue(sOp, RG_PARAM_REGEX_GLOB) then begin
			if 0 > fileMasks.AddIfNotContains(TOptionsHelper.GetOptionValue(sOp)) then begin
				dbgMsg.Msg('duplicate skipped:' + sOp)
			end;
		end;
	end;
	Result := string.Join(';', fileMasks.Items);
	dbgMsg.Msg('Result=' + Result);
end;

class function TCommandLineBuilder.GetMissingFileMaskOptions(const _sOptions, _sMasks : string) : string;
var
	existingMasks : TArrayEx<string>;
	masksEdited : TArrayEx<string>;
	newOptions : string;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TCommandLineBuilder.GetMissingFileMaskOptions');

	existingMasks := TCommandLineBuilder.GetFileMaskParamsFromOptions(_sOptions);
	masksEdited := _sMasks.Split([';']);

	dbgMsg.Msg('masksEdited=' + _sMasks);
	dbgMsg.Msg('existingMasks=' + string.Join(';', existingMasks.Items));
	newOptions := FileMasksToOptions(masksEdited, existingMasks);
	Result := newOptions.Trim;
	dbgMsg.Msg('Result=' + Result);
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
	fileMaskParams : TArrayEx<string>;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TCommandLineBuilder.RebuildArguments');

	_params.RipGrepArguments.Clear();

	dbgMsg.Msg('add additional ops:' + _params.GuiSearchTextParams.ExpertOptions.AsString);
	arrRgOptions := _params.GuiSearchTextParams.ExpertOptions.AsArray;

	fileMaskParams := GetFileMaskParamsArrFromDelimitedText(_params.FileMasks);
	paramCount := arrRgOptions.Count;
	for var s in _params.GuiSearchTextParams.RgOptions.AsArray do begin
		if not fileMaskParams.Contains(s) then begin
			arrRgOptions.InsertIfNotContains(paramCount, s);
			dbgMsg.Msg('add search text param: ' + s);
		end else begin
			dbgMsg.Msg('skipp search text param: ' + s);
		end;
	end;

	paramCount := arrRgOptions.Count;
	for var s in RG_NECESSARY_PARAMS do begin
		arrRgOptions.InsertIfNotContains(paramCount, s);
		dbgMsg.Msg('add necessary param: ' + s);
	end;

	arrRgOptions.AddRange(fileMaskParams);
	dbgMsg.Msg('add all mask param: ' + _params.FileMasks);

	arrRgOptions.RemoveAll(RG_PARAM_END);
	// put it in the end
	arrRgOptions.Add(RG_PARAM_END); // indicates that no more flags will be provided

	_params.RgExeOptions := TOptionStrings.New(arrRgOptions);

	AddArgs(_params, RG_ARG_OPTIONS, arrRgOptions);
	AddArgs(_params, RG_ARG_SEARCH_TEXT, [_params.GuiSearchTextParams.SearchText]); // order is important!
	// --- set search path
	for var s in _params.SearchPath.Split([SEARCH_PATH_SEPARATOR]) do begin
		arrPaths.Add(s);
		TDebugUtils.DebugMessage('Path added:' + s);
	end;
	AddArgs(_params, RG_ARG_SEARCH_PATH, arrPaths, True { Quote if necessary } );

	dbgMsg.Msg('after AddArgs: ' + string.Join(' ', _params.RipGrepArguments.GetValues()));
	dbgMsg.Msg('GuiSearchTextParams end ' + _params.GuiSearchTextParams.ToString);
end;

end.
