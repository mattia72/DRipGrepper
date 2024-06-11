unit RipGrepper.CommandLine.OptionHelper;

interface

uses
	RipGrepper.Common.GuiSearchParams,
	System.Classes,
	RipGrepper.Common.Constants;

type

	TParamRegexHelper = class

		public
			class function GetLongParam(const _paramRegex : string) : string;
			class function GetShortParam(const _paramRegex : string) : string;
	end;

	TOptionsHelper = record

		private
			class function GetBoundedParamWithValueRegex(const _sOption : string; const _sParamValue : string = '') : string; static;

		public
			class procedure AddParamToList(list : TStringList; const _paramRegex : string = ''; const _sValue : string = '';
				const _bUnique : Boolean = False); static;
			class function GetBoundedParamRegex(const _sOption : string) : string; static;
			class function GetOptionValue(const _sOption : string) : string; overload; static;
			class function GetOptionValue(const _sOption : string; var _sOptionName : string) : string; overload; static;
			class function GetOptionValueFromOptions(const _sOptions, _sOptionRegex : string; var _sValue : string) : Boolean; static;
			class function IsOptionSet(const _sOptions, _sParamRegex : string; const _sParamValue : string = '') : Boolean;
				overload; static;
			function IsOptionSet(const _guiOption : EGuiOption; const _sActualOptions : string) : Boolean; overload;
			class function IsOptionWithValue(const _sOption : string; const _sOptionRegEx : string = '') : Boolean; static;
			class function IsSetOptionWithValue(const _sOptions, _sOption : string; const _sValue : string = '') : Boolean; static;
			class function IsWordBoundOnBothSide(const _s : string) : Boolean; static;
			class function IsWordBoundOnOneSide(const _s : string) : Boolean; static;
			class procedure PutBetweenWordBoundaries(var _s : string); static;
			class function RemoveAllParams(const _sOptions, _argMaskRegex : string; const _bSwitch : Boolean = False) : string; static;
			class procedure RemoveParamFromList(list : TStringList; const _paramRegex : string = ''); static;
	end;

implementation

uses
	ArrayEx,
	RipGrepper.Helper.Types,
	System.SysUtils,
	System.RegularExpressions;

class procedure TOptionsHelper.AddParamToList(list : TStringList; const _paramRegex : string = ''; const _sValue : string = '';
	const _bUnique : Boolean = False);
var
	params : TArrayEx<string>;
	iFoundIdx : integer;
begin
	if not _paramRegex.IsEmpty then begin
		params := _paramRegex.Split(['|']);
		if _sValue.IsEmpty then begin
			iFoundIdx := list.IndexOfFirstMatch('^('+_paramRegex+')$');
			if (iFoundIdx < 0) then begin
				list.Insert(0, params[RG_PARAM_LONG_INDEX]);
			end;
		end else begin
			iFoundIdx := list.IndexOfFirstMatch('^(' + _paramRegex + ')=?\w+$');
			if (iFoundIdx >= 0) and _bUnique then begin
				list.Delete(iFoundIdx);
			end;
			list.Insert(0, params[RG_PARAM_LONG_INDEX] + '=' + _sValue);
		end;
	end;
end;

class function TOptionsHelper.GetBoundedParamRegex(const _sOption : string) : string;
begin
	Result := '-+' + WB + _sOption.TrimLeft(['-']) + WB;
end;

class function TOptionsHelper.GetBoundedParamWithValueRegex(const _sOption : string; const _sParamValue : string = '') : string;
begin
	Result := '-+' + WB + _sOption.TrimLeft(['-']) + '=' + TRegEx.Escape(_sParamValue);
end;

class function TOptionsHelper.GetOptionValue(const _sOption : string) : string;
var
	sOpName : string;
begin
	Result := GetOptionValue(_sOption, sOpName);
end;

class function TOptionsHelper.GetOptionValue(const _sOption : string; var _sOptionName : string) : string;
begin
	_sOptionName := _sOption.Remove(_sOption.IndexOf('='));
	Result := _sOption.Remove(0, _sOption.IndexOf('=') + 1);
end;

class function TOptionsHelper.GetOptionValueFromOptions(const _sOptions, _sOptionRegex : string; var _sValue : string) : Boolean;
var
	options : TArrayEx<string>;
	sOp : string;
	sOpName : string;
begin
	Result := False;
	options := _sOptions.Split([' '], TStringSplitOptions.ExcludeEmpty);
	for var i : integer := 0 to options.MaxIndex do begin
		sOp := options[i];
		if TRegex.IsMatch(sOp, _sOptionRegex) and TOptionsHelper.IsOptionWithValue(sOp) then begin
			_sValue := TOptionsHelper.GetOptionValue(sOp, sOpName);
			Result := True;
			break;
		end;
	end;

end;

class function TOptionsHelper.IsOptionSet(const _sOptions, _sParamRegex : string; const _sParamValue : string = '') : Boolean;
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
					if TOptionsHelper.IsSetOptionWithValue(_sOptions, sOp, _sParamValue) then begin
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

function TOptionsHelper.IsOptionSet(const _guiOption : EGuiOption; const _sActualOptions : string) : Boolean;
begin
	Result := False;
	case _guiOption of
		EGuiOption.soNotSet :
		{ } Result := IsOptionSet(_sActualOptions, RG_PARAM_REGEX_FIXED_STRINGS);
		EGuiOption.soMatchCase :
		{ } Result := IsOptionSet(_sActualOptions, RG_PARAM_REGEX_CASE_SENSITIVE);
		EGuiOption.soMatchWord :
		{ } Result := not IsOptionSet(_sActualOptions, RG_PARAM_REGEX_FIXED_STRINGS);
		EGuiOption.soUseRegex :
		{ } Result := not IsOptionSet(_sActualOptions, RG_PARAM_REGEX_FIXED_STRINGS);
	end;
end;

class function TOptionsHelper.IsOptionWithValue(const _sOption : string; const _sOptionRegEx : string = '') : Boolean;
begin
	if _sOptionRegEx.IsEmpty then begin
		Result := TRegEx.IsMatch(_sOption, '-+[\w-]+=');
	end else begin
		Result := TRegEx.IsMatch(_sOption, '(' + _sOptionRegEx + ')=');
	end;
end;

class function TOptionsHelper.IsSetOptionWithValue(const _sOptions, _sOption : string; const _sValue : string = '') : Boolean;
begin
	Result := TRegEx.IsMatch(_sOptions, GetBoundedParamWithValueRegex(_sOption, _sValue));
end;

class function TOptionsHelper.IsWordBoundOnBothSide(const _s : string) : Boolean;
begin
	Result := (_s.StartsWith(WB, True) and _s.EndsWith(WB, True));
end;

class function TOptionsHelper.IsWordBoundOnOneSide(const _s : string) : Boolean;
begin
	Result := (_s.StartsWith(WB, True) and not _s.EndsWith(WB, True))
	{ } or (not _s.StartsWith(WB, True) and _s.EndsWith(WB, True));
end;

class procedure TOptionsHelper.PutBetweenWordBoundaries(var _s : string);
begin
	if not(_s.StartsWith(WB, True) or _s.EndsWith(WB, True)) then begin
		_s := WB + _s + WB;
	end;
end;

class function TOptionsHelper.RemoveAllParams(const _sOptions, _argMaskRegex : string; const _bSwitch : Boolean = False) : string;
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

class procedure TOptionsHelper.RemoveParamFromList(list : TStringList; const _paramRegex : string = '');
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
				r := TOptionsHelper.GetBoundedParamRegex(p);
				list.DeleteAllMatched(r);
				list.DeleteAllMatched(TOptionsHelper.GetBoundedParamWithValueRegex(p));
			end;
		end;
	end;
end;

class function TParamRegexHelper.GetLongParam(const _paramRegex : string) : string;
begin
	Result := '';
	if not _paramRegex.IsEmpty then begin
		Result := _paramRegex.Split(['|'])[1];
	end;
end;

class function TParamRegexHelper.GetShortParam(const _paramRegex : string) : string;
begin
	Result := '';
	if not _paramRegex.IsEmpty then begin
		Result := _paramRegex.Split(['|'])[0];
	end;
end;

end.
