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
			procedure AddArgs(const _sName : string; const _args : TArray<string>; const _bQuote : Boolean = False);
			class procedure AddParamToList(list : TStringList; const _paramRegex : string = ''; const _bRemove : Boolean = False); static;
			function QuoteStringIfContainsWhiteSpace(const _s : string) : string;

		public
			class function FileMasksToOptions(const _arrMasks, _arrSkipMasks : TArrayEx<string>) : string; static;
			class function New(_params : TRipGrepParameterSettings) : TCommandLineBuilder; static;
			class function GetFileMaskParamsArrFromDelimitedText(const _sFileMasksDelimited : string; const _sSeparator : string = ';')
				: TArray<string>; overload; static;
			class function GetFileMaskParamsFromDelimitedText(const _sFileMasksDelimited : string; const _sSeparator : string = ';')
				: string; overload; static;
			class function GetFileMaskParamsFromOptions(const _sOptions : string) : TArray<string>; static;
			class function GetFileMasksDelimited(const _sOptions, _argMaskRegex : string) : string; static;
			class function GetMissingFileMaskOptions(const _sOptions, _sMasks : string) : string; static;
			function ReBuildArguments : TStrings;
			class function RemoveAllParams(const _sOptions, _argMaskRegex : string; const _bSwitch : Boolean = False) : string; static;
			class function UpdateRgOptions(const _sOptions : string; const _sParamRegex : string = ''; const _bRemove : Boolean = False)
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

procedure TCommandLineBuilder.AddArgs(const _sName : string; const _args : TArray<string>; const _bQuote : Boolean = False);
begin
	for var s : string in _args do begin
		if not s.IsEmpty then begin
			if _bQuote then begin
				Parameters.RipGrepArguments.AddPair(_sName, TProcessUtils.MaybeQuoteIfNotQuoted(s));
			end else begin
				Parameters.RipGrepArguments.AddPair(_sName, s);
			end;
		end;
	end;
end;

class procedure TCommandLineBuilder.AddParamToList(list : TStringList; const _paramRegex : string = ''; const _bRemove : Boolean = False);
var
	params : TArrayEx<string>;
	bFoundIdx : integer;
begin
	if not _paramRegex.IsEmpty then begin
		bFoundIdx := -1;

		repeat
			params := _paramRegex.Split(['|']);
			for var p in params do begin
				bFoundIdx := list.IndexOf(p);
				if (bFoundIdx >= 0) then begin
					break; // Already has
				end;
			end;

			if (bFoundIdx < 0) then begin
				list.Add(params[1]); // long params
			end else if (_bRemove) then begin
				list.Delete(bFoundIdx);
			end;
		until _bRemove and (bFoundIdx <> -1);
	end;
end;

class function TCommandLineBuilder.FileMasksToOptions(const _arrMasks, _arrSkipMasks : TArrayEx<string>) : string;
var
	newOptions : string;
begin
	for var s in _arrMasks do begin
		if (not string.IsNullOrWhiteSpace(s)) and (not _arrSkipMasks.Contains(s)) then begin
			newOptions := newOptions + ' -g ' + s;
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
			list.Add('-g');
			list.Add(s);
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
	Result := GetFileMasksDelimited(_sOptions, RG_PARAM_REGEX_GLOB).Split([';']);
end;

class function TCommandLineBuilder.GetFileMasksDelimited(const _sOptions, _argMaskRegex : string) : string;
var
	bAddNext : Boolean;
	fileMask : string;
begin
	bAddNext := False;
	for var s in _sOptions.Split([' ']) do begin
		if bAddNext then begin
			fileMask := fileMask + ';' + s;
			bAddNext := False;
		end else begin
			bAddNext := TRegEx.IsMatch(s, _argMaskRegex);
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

function TCommandLineBuilder.QuoteStringIfContainsWhiteSpace(const _s : string) : string;
begin
	Result := '\b' + _s + '\b';
end;

function TCommandLineBuilder.ReBuildArguments : TStrings;
var
	arrRgOptions : TArrayEx<string>;
	s : string;
begin
	Parameters.RipGrepArguments.Clear();
	arrRgOptions := Parameters.Options.Split([' ']);

	for s in RG_NECESSARY_PARAMS do begin
		if not arrRgOptions.Contains(s) then begin
			arrRgOptions.Insert(0, s);
		end;
	end;

	arrRgOptions.AddRange(GetFileMaskParamsArrFromDelimitedText(Parameters.FileMasks));
	if not arrRgOptions.Contains('--') then begin
		arrRgOptions.Add('--'); // indicates that no more flags will be provided
	end;

	AddArgs(RG_ARG_OPTIONS, arrRgOptions);

	s := Parameters.SearchText;
	if TRegex.IsMatch(s, '\s') or Parameters.MatchWholeWord then begin
		s := QuoteStringIfContainsWhiteSpace(s);
	end;

	Parameters.RipGrepArguments.AddPair(RG_ARG_SEARCH_TEXT, s);

	AddArgs(RG_ARG_SEARCH_PATH, Parameters.SearchPath.Split([',', ';']), True);
	Parameters.RipGrepArguments.Delimiter := ' ';
	// sArgs.QuoteChar := '"';
	Result := Parameters.RipGrepArguments;
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
			if not _bSwitch then begin
				arrRemoveIdxs.Add(i + 1);
			end;
		end;
	end;
	arrOptions.Delete(arrRemoveIdxs);
	Result := string.Join(' ', arrOptions.Items);
end;

class function TCommandLineBuilder.UpdateRgOptions(const _sOptions : string; const _sParamRegex : string = '';
	const _bRemove : Boolean = False) : string;
var
	listOptions : TStringList;
begin
	listOptions := TStringList.Create(dupIgnore, False, True);
	listOptions.Delimiter := ' ';
	try

		listOptions.AddStrings(_sOptions.Split([' ']));

		AddParamToList(listOptions, _sParamRegex, _bRemove);
		if not listOptions.Contains('--') then begin
			listOptions.Add('--');
		end;

		Result := listOptions.DelimitedText;
	finally
		listOptions.Free;
	end;
end;

end.
