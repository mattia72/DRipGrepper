unit RipGrepper.Common.GuiSearchParams;

interface

uses
	RipGrepper.Common.Constants,
	RipGrepper.Helper.Types,
	ArrayEx,
	System.Classes;

type
	TGuiSetSearchParams = record
		private
			FSearchText : string;
			FEscapedSearchText : string;
			FWordBoundedSearchText : string;

			FRgOptions: string;

			FIsRgExeOptionSet : Boolean;

			function GetEscapedSearchText : string;
			function GetSearchText : string;
			function GetWordBoundedSearchText : string;
			function ResetRgOptions(const _sParamRegex : string; const _bReset : Boolean = False) : string;
			function SetRgOptions(const _sParamRegex : string; const _bReset : Boolean = False) : string;

		public
			SearchOptions : set of EGuiOption;

			class function AddRemoveRgExeOptions(const _sOptions : string; const _sParamRegex : string; const _bRemove : Boolean = False)
				: string; static;
			class function New(const _sText : string; const _bIC, _bMW, _bUR : Boolean) : TGuiSetSearchParams; static;

			function GetNext(const _newOption : EGuiOption) : TGuiSetSearchParams;
			function IsSet(_options : TArray<EGuiOption>) : Boolean;
			procedure Clear;
			procedure ResetOption(const _searchOption : EGuiOption);
			function SearchOptionsAsBitField : TBitField;
			procedure SetOption(const _searchOption : EGuiOption);
			procedure SetOrReset(const _newOption : EGuiOption); overload;
			function ToString : string;
			class operator Initialize(out Dest : TGuiSetSearchParams);

			property EscapedSearchText : string read GetEscapedSearchText;
			property IsRgExeOptionSet : Boolean read FIsRgExeOptionSet write FIsRgExeOptionSet;
			property RgOptions: string read FRgOptions write FRgOptions;
			property SearchText : string read GetSearchText write FSearchText;
			property WordBoundedSearchText : string read GetWordBoundedSearchText;
	end;

implementation

uses
	System.SysUtils,
	System.RegularExpressions,
	RipGrepper.CommandLine.OptionHelper;

class function TGuiSetSearchParams.AddRemoveRgExeOptions(const _sOptions : string; const _sParamRegex : string;
	const _bRemove : Boolean = False) : string;
var
	listOptions : TStringList;
begin
	listOptions := TStringList.Create(dupIgnore, False, True);
	listOptions.Delimiter := ' ';
	try
		listOptions.AddStrings(_sOptions.Split([' '], TStringSplitOptions.ExcludeEmpty));

		if _bRemove then begin
			TOptionsHelper.RemoveParamFromList(listOptions, _sParamRegex);
		end else begin
			TOptionsHelper.AddParamToList(listOptions, _sParamRegex);
		end;
		Result := listOptions.DelimitedText;
	finally
		listOptions.Free;
	end;
end;

class function TGuiSetSearchParams.New(const _sText : string; const _bIC, _bMW, _bUR : Boolean) : TGuiSetSearchParams;
begin
	Result.Clear();
	Result.SearchText := _sText;
	if _bIC then
		Include(Result.SearchOptions, EGuiOption.soMatchCase);
	if _bMW then
		Include(Result.SearchOptions, EGuiOption.soMatchWord);
	if _bUR then
		Include(Result.SearchOptions, EGuiOption.soUseRegex);
end;

function TGuiSetSearchParams.GetNext(const _newOption : EGuiOption) : TGuiSetSearchParams;
begin
	Result := self;
	case _newOption of
		EGuiOption.soMatchCase : begin
			Result.SetOrReset(EGuiOption.soMatchCase);
		end;

		EGuiOption.soMatchWord : begin
			Result.SetOrReset(EGuiOption.soMatchWord);
		end;

		EGuiOption.soUseRegex : begin
			Result.SetOrReset(EGuiOption.soUseRegex);
		end;
	end;
end;

function TGuiSetSearchParams.IsSet(_options : TArray<EGuiOption>) : Boolean;
begin
	Result := True;
	for var o in _options do begin
		if not(o in SearchOptions) then begin
			Result := False;
			break
		end;
	end;
end;

procedure TGuiSetSearchParams.Clear;
begin
	SearchText := '';
	FEscapedSearchText := '';
	FIsRgExeOptionSet := False;
	SearchOptions := [EGuiOption.soNotSet];
end;

function TGuiSetSearchParams.GetEscapedSearchText : string;
begin
	Result := TRegEx.Escape(FSearchText);
end;

function TGuiSetSearchParams.GetSearchText : string;
begin
	Result := FSearchText;

//	if IsSet([EGuiOption.soUseRegex]) then begin
//		Result := EscapedSearchText;
//	end;

	if IsSet([EGuiOption.soMatchWord]) then begin
		Result := WordBoundedSearchText;
	end;
end;

function TGuiSetSearchParams.GetWordBoundedSearchText : string;
begin
	FWordBoundedSearchText := FSearchText;
	TOptionsHelper.PutBetweenWordBoundaries(FWordBoundedSearchText);
	Result := FWordBoundedSearchText;
end;

procedure TGuiSetSearchParams.ResetOption(const _searchOption : EGuiOption);
var
	searchOption : EGuiOption;
begin
	searchOption := _searchOption;
	Exclude(SearchOptions, searchOption);

	if (SearchOptions = []) or
	{ } (not(EGuiOption.soUseRegex in SearchOptions)) and
	{ } (not(EGuiOption.soMatchWord in SearchOptions)) then begin
		SetRgOptions(RG_PARAM_REGEX_FIXED_STRINGS);
	end;

	case searchOption of
		EGuiOption.soNotSet :
		{ };
		EGuiOption.soMatchCase :
		{ } SetRgOptions(RG_PARAM_REGEX_CASE_SENSITIVE, True);
		EGuiOption.soMatchWord :
		{ };
		EGuiOption.soUseRegex :
		{ };
	end;

end;

function TGuiSetSearchParams.SearchOptionsAsBitField : TBitField;
begin
	for var i in GUI_SEARCH_PARAMS do begin
		if i in SearchOptions then begin
			Result.SetBit(Integer(i));
		end;
	end;
end;

function TGuiSetSearchParams.SetRgOptions(const _sParamRegex : string; const _bReset : Boolean = False) : string;
begin
	if _bReset then begin
		RgOptions := TGuiSetSearchParams.AddRemoveRgExeOptions(RgOptions, _sParamRegex, True);
	end else begin
		RgOptions := TGuiSetSearchParams.AddRemoveRgExeOptions(RgOptions, _sParamRegex);
	end;
end;

function TGuiSetSearchParams.ResetRgOptions(const _sParamRegex : string; const _bReset : Boolean = False) : string;
begin
	if _bReset then begin
		RgOptions := TGuiSetSearchParams.AddRemoveRgExeOptions(RgOptions, _sParamRegex);
	end else begin
		RgOptions := TGuiSetSearchParams.AddRemoveRgExeOptions(RgOptions, _sParamRegex, True);
	end;
end;

procedure TGuiSetSearchParams.SetOption(const _searchOption : EGuiOption);
begin
	Include(SearchOptions, _searchOption);

	if _searchOption <> EGuiOption.soNotSet then begin
		Exclude(SearchOptions, EGuiOption.soNotSet);
	end;

	case _searchOption of
		EGuiOption.soNotSet :
		{ } SetRgOptions(RG_PARAM_REGEX_FIXED_STRINGS);
		EGuiOption.soMatchCase :
		{ } SetRgOptions(RG_PARAM_REGEX_CASE_SENSITIVE);
		EGuiOption.soMatchWord :
		{ } ResetRgOptions(RG_PARAM_REGEX_FIXED_STRINGS);
		EGuiOption.soUseRegex :
		{ } ResetRgOptions(RG_PARAM_REGEX_FIXED_STRINGS);
	end;
end;

procedure TGuiSetSearchParams.SetOrReset(const _newOption : EGuiOption);
begin
	if IsSet([_newOption]) then begin
		ResetOption(_newOption);
	end else begin
		SetOption(_newOption);
	end;
end;

function TGuiSetSearchParams.ToString : string;
var
	arr : TArrayEx<string>;
begin
	Result := '';
	for var i in GUI_SEARCH_PARAMS do begin
		if i in SearchOptions then begin
			case i of
				EGuiOption.soMatchCase :
				arr.Add('MatchCase');
				EGuiOption.soMatchWord :
				arr.Add('MatchWord');
				EGuiOption.soUseRegex :
				arr.Add('UseRegex');
			end;
		end;
	end;

	Result := SearchText + ' [' + string.Join(',', arr.Items) + '] IsRgOpSet:' + BoolToStr(IsRgExeOptionSet, True) + CRLF + RgOptions;
end;

class operator TGuiSetSearchParams.Initialize(out Dest : TGuiSetSearchParams);
begin
	Dest.SetOption(EGuiOption.soNotSet);
end;

end.
