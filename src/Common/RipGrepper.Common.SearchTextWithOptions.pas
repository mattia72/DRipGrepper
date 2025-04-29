unit RipGrepper.Common.SearchTextWithOptions;

interface

uses
	RipGrepper.Common.SimpleTypes,
	RipGrepper.Common.Interfaces.StreamPersistable,
	System.Classes;

type
	TSearchTextWithOptions = class(TInterfacedObject, IStreamPersistable)
		private
			FEscapedSearchText : string;
			// FSearchText : string;
			FInnerSearchText : string;
			FSearchOptions : TSearchOptionSet;
			FWordBoundedSearchText : string;
			function GetSearchOptions() : TSearchOptionSet;
			function GetSearchTextAsRgParam : string;
			function GetSearchTextByOptions() : string;
			function GetSearchTextOfUser : string;
			procedure SetSearchOptions(const Value : TSearchOptionSet);
			procedure UpdateEscapedSearchText();
			procedure SetSearchTextOfUser(const Value : string);
			procedure UpdateWordBoundedSearchText();

		public
			function AreSet(_options : TArray<EGuiOption>) : Boolean;
			procedure Clear;
			procedure Copy(const _other : TSearchTextWithOptions);
			class function GetAsSearchOptionSet(const _bMC, _bMW, _bUR : Boolean) : TSearchOptionSet; static;
			function GetAsString(const _bGuiOptionsOnly : Boolean = False) : string;
			procedure LoadFromStream(_stream : TStream);
			constructor Create(const _searchText : string; const _options : TSearchOptionSet); overload;
			constructor Create(); overload;
			procedure LoadFromStreamReader(_sr : TStreamReader);
			procedure ResetOption(const _searchOption : EGuiOption);
			procedure SaveToStream(_stream : TStream);
			procedure SaveToStreamWriter(_sw : TStreamWriter);
			class function SearchOptionSetToString(const _so : TSearchOptionSet) : string; static;
			procedure SetOption(const _searchOption : EGuiOption);
			procedure UpdateSearchOptions(const _sOptions : string);
			class function StringToSearchOptionSet(const s : string) : TSearchOptionSet; static;
			procedure SwitchOption(const _newOption : EGuiOption); overload;
			property EscapedSearchText : string read FEscapedSearchText;
			property SearchOptions : TSearchOptionSet read GetSearchOptions write SetSearchOptions;
			property SearchTextAsRgParam : string read GetSearchTextAsRgParam;
			property SearchTextOfUser : string read GetSearchTextOfUser write SetSearchTextOfUser;
	end;

implementation

uses
	ArrayEx,
	System.SysUtils,
	System.StrUtils,
	RipGrepper.CommandLine.OptionHelper,
	System.RegularExpressions,
	Spring,
	RipGrepper.Tools.DebugUtils;

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
	SearchTextOfUser := '';
	FEscapedSearchText := '';
	FWordBoundedSearchText := '';
	FSearchOptions := [EGuiOption.soNotSet];
end;

procedure TSearchTextWithOptions.Copy(const _other : TSearchTextWithOptions);
begin
	FSearchOptions := _other.SearchOptions;
	// FSearchText := _other.SearchTextAsRgParam;
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
begin
	Result := TSearchTextWithOptions.SearchOptionSetToString(FSearchOptions);

	if not _bGuiOptionsOnly then begin
		Result := Format('%s', [SearchTextAsRgParam]);
	end;
end;

function TSearchTextWithOptions.GetSearchOptions() : TSearchOptionSet;
begin
	Result := FSearchOptions;
end;

function TSearchTextWithOptions.GetSearchTextAsRgParam : string;
begin
	Result := GetSearchTextByOptions();
end;

function TSearchTextWithOptions.GetSearchTextByOptions() : string;
begin
	Result := FInnerSearchText;
	if EGuiOption.soMatchWord in FSearchOptions then begin
		Result := FWordBoundedSearchText;
	end;
end;

function TSearchTextWithOptions.GetSearchTextOfUser : string;
begin
	Result := FInnerSearchText;
end;

constructor TSearchTextWithOptions.Create(const _searchText : string; const _options : TSearchOptionSet);
begin
	SearchTextOfUser := _searchText;
	SearchOptions := _options;
end;

constructor TSearchTextWithOptions.Create();
begin
	SearchTextOfUser := '';
	SearchOptions := [];
end;

procedure TSearchTextWithOptions.ResetOption(const _searchOption : EGuiOption);
var
	searchOption : EGuiOption;
begin
	searchOption := _searchOption;
	Exclude(FSearchOptions, searchOption);
	UpdateWordBoundedSearchText;
end;

procedure TSearchTextWithOptions.LoadFromStream(_stream : TStream);
var
	sr : IShared<TStreamReader>;
begin
	sr := Shared.Make<TStreamReader>(TStreamReader.Create(_stream, TEncoding.UTF8));
	LoadFromStreamReader(sr);
end;

procedure TSearchTextWithOptions.LoadFromStreamReader(_sr : TStreamReader);
begin
	SearchTextOfUser := _sr.ReadLine;
	SearchOptions := StringToSearchOptionSet(_sr.ReadLine);
end;

procedure TSearchTextWithOptions.SaveToStream(_stream : TStream);
var
	sw : IShared<TStreamWriter>;
begin
	sw := Shared.Make<TStreamWriter>(TStreamWriter.Create(_stream));
	SaveToStreamWriter(sw)
end;

procedure TSearchTextWithOptions.SaveToStreamWriter(_sw : TStreamWriter);
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TSearchTextWithOptions.SaveToStreamWriter');

	dbgMsg.Msg('SearchTextOfUser: ' + SearchTextOfUser);
	_sw.WriteLine(SearchTextOfUser);

	var
	s := SearchOptionSetToString(SearchOptions);
	dbgMsg.Msg('SearchOptions: ' + s);
 	_sw.WriteLine(s);
end;

class function TSearchTextWithOptions.SearchOptionSetToString(const _so : TSearchOptionSet) : string;
var
	arr : TArrayEx<string>;
begin
	Result := '';
	for var i in GUI_SEARCH_PARAMS do begin
		if i in _so then begin
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
end;

procedure TSearchTextWithOptions.UpdateEscapedSearchText();
begin
	FEscapedSearchText := TRegEx.Escape(FInnerSearchText);
end;

procedure TSearchTextWithOptions.SetOption(const _searchOption : EGuiOption);
begin
	Include(FSearchOptions, _searchOption);

	if _searchOption <> EGuiOption.soNotSet then begin
		Exclude(FSearchOptions, EGuiOption.soNotSet);
	end;
	UpdateWordBoundedSearchText();
end;

procedure TSearchTextWithOptions.SetSearchOptions(const Value : TSearchOptionSet);
begin
	if FSearchOptions <> Value then begin
		FSearchOptions := Value;
		UpdateWordBoundedSearchText();
	end;
end;

procedure TSearchTextWithOptions.SetSearchTextOfUser(const Value : string);
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
	try
		if (FSearchOptions = []) or (FSearchOptions = [EGuiOption.soNotSet]) then begin
			Exit;
		end;

		if FSearchOptions = [EGuiOption.soMatchWord] then begin
			s := FEscapedSearchText;
		end else if not(EGuiOption.soUseRegex in FSearchOptions) then begin
			s := FEscapedSearchText;
		end;
	finally
		FWordBoundedSearchText := TOptionsHelper.PutBetweenWordBoundaries(s);
	end;
end;

procedure TSearchTextWithOptions.UpdateSearchOptions(const _sOptions : string);
begin
	var
	op := TSearchTextWithOptions.StringToSearchOptionSet(_sOptions);
	if op <> FSearchOptions then begin
		FSearchOptions := op;
		UpdateWordBoundedSearchText();
	end;
end;

class function TSearchTextWithOptions.StringToSearchOptionSet(const s : string) : TSearchOptionSet;
begin
	Result := TSearchTextWithOptions.GetAsSearchOptionSet(
		{ } s.Contains('MatchCase'),
		{ } s.Contains('MatchWord'),
		{ } s.Contains('UseRegex'));
end;

procedure TSearchTextWithOptions.SwitchOption(const _newOption : EGuiOption);
begin
	if AreSet([_newOption]) then begin
		ResetOption(_newOption);
	end else begin
		SetOption(_newOption);
	end;
end;

end.
