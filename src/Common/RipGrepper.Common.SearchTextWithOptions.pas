unit RipGrepper.Common.SearchTextWithOptions;

interface

uses
	RipGrepper.Common.SimpleTypes;

type
	TSearchTextWithOptions = record
		private
			FEscapedSearchText : string;
			FSearchText : string;
			FWordBoundedSearchText : string;
			function GetSearchText : string;
			procedure SetEscapedSearchText;
			procedure SetSearchText(const Value : string);
			procedure SetWordBoundedSearchText;

		public
			SearchOptions : TSearchOptionSet;
			function AreSet(_options : TArray<EGuiOption>) : Boolean;
			procedure Clear;
			procedure Copy(const _other : TSearchTextWithOptions);
			class function GetAsSearchOptionSet(const _bMC, _bMW, _bUR : Boolean) : TSearchOptionSet; static;
			function GetAsString(const _bGuiOptionsOnly : Boolean = False) : string;
			class function New(const _searchText : string; const _options : TSearchOptionSet) : TSearchTextWithOptions; static;
			procedure ResetOption(const _searchOption : EGuiOption);
			procedure SetOption(const _searchOption : EGuiOption);
			procedure StringToSearchOptions(const _sOptions : string);
			class function StringToSearchParams(const s : string) : TSearchOptionSet; static;
			property EscapedSearchText : string read FEscapedSearchText;
			property SearchText : string read GetSearchText write SetSearchText;
			property WordBoundedSearchText : string read FWordBoundedSearchText;
	end;

implementation

uses
	ArrayEx,
	System.SysUtils,
	System.StrUtils,
	RipGrepper.CommandLine.OptionHelper,
	System.RegularExpressions;

function TSearchTextWithOptions.AreSet(_options : TArray<EGuiOption>) : Boolean;
begin
	Result := True;
	for var o in _options do begin
		if not(o in SearchOptions) then begin
			Result := False;
			break
		end;
	end;
end;

procedure TSearchTextWithOptions.Clear;
begin
	SearchText := '';
	FEscapedSearchText := '';
	FWordBoundedSearchText := '';
	SearchOptions := [EGuiOption.soNotSet];
end;

procedure TSearchTextWithOptions.Copy(const _other : TSearchTextWithOptions);
begin
	SearchOptions := _other.SearchOptions;
	FSearchText := _other.SearchText;
	FEscapedSearchText := _other.EscapedSearchText;
	FWordBoundedSearchText := _other.WordBoundedSearchText;
end;

class function TSearchTextWithOptions.GetAsSearchOptionSet(const _bMC, _bMW, _bUR : Boolean) : TSearchOptionSet;
begin
	Result := [eGuiOption.soNotSet];
	if _bMC then
		Include(Result, EGuiOption.soMatchCase);
	if _bMW then
		Include(Result, EGuiOption.soMatchWord);
	if _bUR then
		Include(Result, EGuiOption.soUseRegex);

	if Result <> [EGuiOption.soNotSet] then begin
		Exclude(Result, EGuiOption.soNotSet);
	end;
end;

function TSearchTextWithOptions.GetAsString(const _bGuiOptionsOnly : Boolean = False) : string;
var arr : TArrayEx<string>; logSearchText : string;
begin
	Result := '';
	for var i in GUI_SEARCH_PARAMS do begin
		if i in SearchOptions then begin
			case i of
				EGuiOption.soMatchCase : begin
					arr.Add('MatchCase');
				end;
				EGuiOption.soMatchWord : begin
					arr.Add('MatchWord');
				end;
				EGuiOption.soUseRegex : begin
					arr.Add('UseRegex');
				end;
			end;
		end;
	end;
	Result := '[' + string.Join(',', arr.Items) + ']';
	if not _bGuiOptionsOnly then begin
		Result := Format('%s', [SearchText]);
	end;
end;

function TSearchTextWithOptions.GetSearchText : string;
begin
	Result := FSearchText;
end;

class function TSearchTextWithOptions.New(const _searchText : string; const _options : TSearchOptionSet) : TSearchTextWithOptions;
begin
	Result.SearchOptions := _options;
	Result.SearchText := _searchText;
end;

procedure TSearchTextWithOptions.ResetOption(const _searchOption : EGuiOption);
var searchOption : EGuiOption;
begin
	searchOption := _searchOption;
	Exclude(SearchOptions, searchOption);
end;

procedure TSearchTextWithOptions.SetEscapedSearchText;
begin
	FEscapedSearchText := TRegEx.Escape(FSearchText);
end;

procedure TSearchTextWithOptions.SetOption(const _searchOption : EGuiOption);
begin
	Include(SearchOptions, _searchOption);

	if _searchOption <> EGuiOption.soNotSet then begin
		Exclude(SearchOptions, EGuiOption.soNotSet);
	end;
end;

procedure TSearchTextWithOptions.SetSearchText(const Value : string);
begin
	if FSearchText <> Value then begin
		FSearchText := Value;
		SetEscapedSearchText();
		SetWordBoundedSearchText();
	end;
end;

procedure TSearchTextWithOptions.SetWordBoundedSearchText;
begin
	if not(EGuiOption.soUseRegex in SearchOptions) then begin
		FWordBoundedSearchText := FEscapedSearchText;
	end else begin
		FWordBoundedSearchText := FSearchText;
	end;
	TOptionsHelper.PutBetweenWordBoundaries(FWordBoundedSearchText);
end;

procedure TSearchTextWithOptions.StringToSearchOptions(const _sOptions : string);
begin
	SearchOptions := TSearchTextWithOptions.StringToSearchParams(_sOptions);
end;

class function TSearchTextWithOptions.StringToSearchParams(const s : string) : TSearchOptionSet;
begin
	Result := TSearchTextWithOptions.GetAsSearchOptionSet(
		{ } s.Contains('MatchCase'),
		{ } s.Contains('MatchWord'),
		{ } s.Contains('UseRegex'));
end;

end.
