unit RipGrepper.CommandLine.OptionHelper;

interface

uses
	RipGrepper.Common.GuiSearchParams,
	System.Classes,
	RipGrepper.Common.Constants,
	ArrayEx;

type

	TParamRegexHelper = class

		public
			class function GetLongParam(const _paramRegex : string) : string;
			class function GetShortParam(const _paramRegex : string) : string;
	end;

	TOptionsHelper = record

		private
			class procedure DeleteAllParam(list: TStringList; const _sParam: string); static;

		public
			class function GetBoundedParamRegex(const _sOption : string) : string; static;
			class function GetBoundedParamWithValueRegex(const _sOption : string; const _sParamValue : string = ''): string; static;
			class function GetOptionValue(const _sOption : string) : string; overload; static;
			class function GetOptionValue(const _sOption : string; var _sOptionName : string) : string; overload; static;
			class function GetOptionValueFromOptions(const _sOptions, _sOptionRegex : string; var _sValue : string) : Boolean; static;
			class function IsOptionWithValue(const _sOption : string; const _sOptionRegEx : string = '') : Boolean; static;
			class function IsSetOptionWithValue(const _sOptions, _sOption : string; const _sValue : string = '') : Boolean; static;
			class function IsWordBoundOnBothSide(const _s : string) : Boolean; static;
			class function IsWordBoundOnOneSide(const _s : string) : Boolean; static;
			class function PutBetweenWordBoundaries(var _s : string): string; static;
			class procedure RemoveParamFromList(list : TStringList; const _paramRegex : string = ''); static;
	end;

implementation

uses

	RipGrepper.Helper.Types,
	System.SysUtils,
	System.RegularExpressions,
	RipGrepper.Tools.ProcessUtils;

class function TOptionsHelper.GetBoundedParamRegex(const _sOption : string) : string;
begin
	// --no-ignore shouldn't match --no-igonore-parent
	if _sOption = RG_PARAM_END then begin
		Result := '\s' + RG_PARAM_END + '(\s|$)';
	end else begin
		Result := '-+' + WB + _sOption.TrimLeft(['-']) + '(\s|=|$)';
	end;
end;

class function TOptionsHelper.GetBoundedParamWithValueRegex(const _sOption : string; const _sParamValue : string = ''): string;
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

class procedure TOptionsHelper.DeleteAllParam(list: TStringList; const _sParam: string);
begin
	if (_sParam = RG_PARAM_END) or (_sParam = '-') then begin
		list.DeleteAll([_sParam]);
	end else begin
		var
		r := TOptionsHelper.GetBoundedParamRegex(_sParam);
		list.DeleteAllMatched(r);
		list.DeleteAllMatched(TOptionsHelper.GetBoundedParamWithValueRegex(_sParam));
	end;
end;

class function TOptionsHelper.PutBetweenWordBoundaries(var _s : string): string;
begin
	if not(_s.StartsWith(WB, True) or _s.EndsWith(WB, True)) then begin
		_s := WB + _s + WB;
	end;
    Result := _s;
end;

class procedure TOptionsHelper.RemoveParamFromList(list : TStringList; const _paramRegex : string = '');
var
	params : TArrayEx<string>;
begin
	if not _paramRegex.IsEmpty then begin
		params := _paramRegex.Split(['|']);
		DeleteAllParam(list, params[RG_PARAM_SHORT_INDEX]);
		DeleteAllParam(list, params[RG_PARAM_LONG_INDEX]);
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
