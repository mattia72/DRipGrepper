unit RipGrepper.Common.SearchTextWithOptions;

interface

uses
	RipGrepper.Common.SimpleTypes;

type
	TSearchTextWithOptions = record
		private
			FEscapedSearchText : string;
			// FSearchText : string;
			FInnerSearchText : string;
			FWordBoundedSearchText : string;
			function GetSearchText : string;
			function GetSearchTextByOptions() : string;
			procedure UpdateEscapedSearchText();
			procedure SetSearchText(const Value : string);
			procedure UpdateWordBoundedSearchText();

			// property EscapedSearchText: string read FEscapedSearchText;
			// property WordBoundedSearchText: string read FWordBoundedSearchText;

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
			procedure UpdateSearchOptions(const _sOptions : string);
			class function StringToSearchOptionSet(const s : string) : TSearchOptionSet; static;
			property EscapedSearchText : string read FEscapedSearchText;
			property SearchText : string read GetSearchText write SetSearchText;
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
	// FSearchText := _other.SearchText;
	FInnerSearchText := _other.FInnerSearchText;
	FEscapedSearchText := _other.FEscapedSearchText;
	FWordBoundedSearchText := _other.FWordBoundedSearchText;
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
var arr : TArrayEx<string>;
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
	Result := GetSearchTextByOptions();
end;

function TSearchTextWithOptions.GetSearchTextByOptions() : string;
begin
	Result := FInnerSearchText;
	if EGuiOption.soMatchWord in SearchOptions then begin
		Result := FWordBoundedSearchText;
	end;
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
	UpdateWordBoundedSearchText;
end;

procedure TSearchTextWithOptions.UpdateEscapedSearchText();
begin
	FEscapedSearchText := TRegEx.Escape(FInnerSearchText);
end;

procedure TSearchTextWithOptions.SetOption(const _searchOption : EGuiOption);
begin
	Include(SearchOptions, _searchOption);

	if _searchOption <> EGuiOption.soNotSet then begin
		Exclude(SearchOptions, EGuiOption.soNotSet);
	end;
	UpdateWordBoundedSearchText;
end;

procedure TSearchTextWithOptions.SetSearchText(const Value : string);
begin
	if FInnerSearchText <> Value then begin
		FInnerSearchText := Value;
		UpdateEscapedSearchText();
		UpdateWordBoundedSearchText();
	end;
end;

procedure TSearchTextWithOptions.UpdateWordBoundedSearchText();
begin
	var
	s := FInnerSearchText;
	if not((EGuiOption.soUseRegex in SearchOptions)
		{ } or (EGuiOption.soMatchWord in SearchOptions)) then begin
		s := FEscapedSearchText;
	end;
	FWordBoundedSearchText := TOptionsHelper.PutBetweenWordBoundaries(s);
end;

procedure TSearchTextWithOptions.UpdateSearchOptions(const _sOptions : string);
begin
	var
	op := TSearchTextWithOptions.StringToSearchOptionSet(_sOptions);
	if op <> SearchOptions then begin
		SearchOptions := op;
		UpdateWordBoundedSearchText;
	end;
end;

class function TSearchTextWithOptions.StringToSearchOptionSet(const s : string) : TSearchOptionSet;
begin
	Result := TSearchTextWithOptions.GetAsSearchOptionSet(
		{ } s.Contains('MatchCase'),
		{ } s.Contains('MatchWord'),
		{ } s.Contains('UseRegex'));
end;

end.
