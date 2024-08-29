unit RipGrepper.Common.GuiSearchParams;

interface

uses
	RipGrepper.Common.Constants,
	RipGrepper.Helper.Types,
	ArrayEx,
	System.Classes,
	RipGrepper.Common.Settings.Persistable,
	System.IniFiles;

type
	TSearchOptionSet = set of EGuiOption;

	TGuiSearchTextParams = class(TPersistableSettings)
		private
			FSearchText : string;
			FEscapedSearchText : string;
			FWordBoundedSearchText : string;
			FRgOptions : string;
			FIsRgExeOptionSet : Boolean;
			FRgAdditionalOptions : string;

			function GetEscapedSearchText : string;
			function GetSearchText : string;
			function GetWordBoundedSearchText : string;
			function ResetRgOptions(const _sParamRegex : string; const _bReset : Boolean = False) : string;
			class procedure ValidateOptions(listOptions : TStringList); static;

		protected
			procedure Init; override;
			procedure ReadIni; override;

		public
			SearchOptions : TSearchOptionSet;

			constructor Create(const _ini : TMemIniFile); overload;
			constructor Create(const _sText : string; const _bMC : Boolean = False; const _bMW : Boolean = False; const _bUR : Boolean = False); overload;
			constructor Create; overload;
			procedure Clear;
			procedure Copy(const _other : TGuiSearchTextParams); reintroduce;

			class function AddRgExeOptions(const _sOptions, _sParamRegex : string) : string; static;
			class function RemoveRgExeOptions(const _sOptions, _sParamRegex : string) : string; static;
			class function AddRgExeOptionWithValue(const _sOptions, _sParamRegex, _sValue : string; const _bUnique : Boolean = False) : string; static;
			function GetNext(const _newOption : EGuiOption) : TGuiSearchTextParams;
			function IsSet(_options : TArray<EGuiOption>) : Boolean;

			class function GetAsSearchOptionSet(const _bMC, _bMW, _bUR : Boolean) : TSearchOptionSet; static;
			procedure ResetOption(const _searchOption : EGuiOption);
			function SearchOptionsAsBitField : TBitField;
			procedure SetOption(const _searchOption : EGuiOption);
			procedure SetOrReset(const _newOption : EGuiOption); overload;
			function SetRgOptions(const _sParamRegex : string; const _bReset : Boolean = False) : string;
			function SetRgOptionsWithValue(const _sParamRegex, _sValue : string; const _bUnique : Boolean = False) : string;
			procedure StoreAsDefault; override;
			function GetAsString(const _bGuiOptionsOnly : Boolean = False) : string;
			procedure LoadDefault; override;
			procedure RefreshMembers; override;

			property EscapedSearchText : string read GetEscapedSearchText;
			property IsRgExeOptionSet : Boolean read FIsRgExeOptionSet write FIsRgExeOptionSet;
			property RgAdditionalOptions : string read FRgAdditionalOptions write FRgAdditionalOptions;
			property RgOptions : string read FRgOptions write FRgOptions;
			property SearchText : string read GetSearchText write FSearchText;
			property WordBoundedSearchText : string read GetWordBoundedSearchText;
	end;

implementation

uses
	System.SysUtils,
	System.RegularExpressions,
	RipGrepper.CommandLine.OptionHelper,
	RipGrepper.Tools.DebugUtils;

class function TGuiSearchTextParams.AddRgExeOptions(const _sOptions, _sParamRegex : string) : string;
var
	listOptions : TStringList;
begin
	listOptions := TStringList.Create(dupIgnore, False, True);
	listOptions.Delimiter := ' ';
	try
		listOptions.AddStrings(_sOptions.Split([' '], TStringSplitOptions.ExcludeEmpty));
		TOptionsHelper.AddParamToList(listOptions, _sParamRegex);
		ValidateOptions(listOptions);
		Result := listOptions.DelimitedText;
	finally
		listOptions.Free;
	end;
end;

class function TGuiSearchTextParams.RemoveRgExeOptions(const _sOptions, _sParamRegex : string) : string;
var
	listOptions : TStringList;
begin
	listOptions := TStringList.Create(dupIgnore, False, True);
	listOptions.Delimiter := ' ';
	try
		listOptions.AddStrings(_sOptions.Split([' '], TStringSplitOptions.ExcludeEmpty));
		TOptionsHelper.RemoveParamFromList(listOptions, _sParamRegex);
		Result := listOptions.DelimitedText;
	finally
		listOptions.Free;
	end;
end;

class function TGuiSearchTextParams.AddRgExeOptionWithValue(const _sOptions, _sParamRegex, _sValue : string; const _bUnique : Boolean = False) : string;
var
	listOptions : TStringList;
begin
	listOptions := TStringList.Create(dupIgnore, False, True);
	listOptions.Delimiter := ' ';
	try
		listOptions.AddStrings(_sOptions.Split([' '], TStringSplitOptions.ExcludeEmpty));
		TOptionsHelper.AddParamToList(listOptions, _sParamRegex, _sValue, _bUnique);
		ValidateOptions(listOptions);
		Result := listOptions.DelimitedText;
	finally
		listOptions.Free;
	end;
end;

// for UnitTests...
constructor TGuiSearchTextParams.Create(const _sText : string; const _bMC : Boolean = False; const _bMW : Boolean = False; const _bUR : Boolean = False);
begin
	Create();
	SearchText := _sText;
	SearchOptions := GetAsSearchOptionSet(_bMC, _bMW, _bUR);
end;

function TGuiSearchTextParams.GetNext(const _newOption : EGuiOption) : TGuiSearchTextParams;
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

function TGuiSearchTextParams.IsSet(_options : TArray<EGuiOption>) : Boolean;
begin
	Result := True;
	for var o in _options do begin
		if not(o in SearchOptions) then begin
			Result := False;
			break
		end;
	end;
end;

procedure TGuiSearchTextParams.Clear;
begin
	SearchText := '';
	FEscapedSearchText := '';
	FIsRgExeOptionSet := False;
	SearchOptions := [EGuiOption.soNotSet];
end;

function TGuiSearchTextParams.GetEscapedSearchText : string;
begin
	Result := TRegEx.Escape(FSearchText);
end;

class function TGuiSearchTextParams.GetAsSearchOptionSet(const _bMC, _bMW, _bUR : Boolean) : TSearchOptionSet;
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

function TGuiSearchTextParams.GetSearchText : string;
begin
	Result := FSearchText;

	// if IsSet([EGuiOption.soUseRegex]) then begin
	// Result := EscapedSearchText;
	// end;

	if IsSet([EGuiOption.soMatchWord]) then begin
		Result := WordBoundedSearchText;
	end;
end;

function TGuiSearchTextParams.GetWordBoundedSearchText : string;
begin
	FWordBoundedSearchText := FSearchText;
	TOptionsHelper.PutBetweenWordBoundaries(FWordBoundedSearchText);
	Result := FWordBoundedSearchText;
end;

procedure TGuiSearchTextParams.ResetOption(const _searchOption : EGuiOption);
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
		EGuiOption.soMatchCase : begin
			ResetRgOptions(RG_PARAM_REGEX_CASE_SENSITIVE);
			SetRgOptions(RG_PARAM_REGEX_IGNORE_CASE);
		end;
		EGuiOption.soMatchWord, EGuiOption.soUseRegex :
		if not( //
			(SearchOptions = [EGuiOption.soNotSet]) or //
			(SearchOptions = [EGuiOption.soMatchCase]) or //
			(SearchOptions = [])) then begin
			SetRgOptions(RG_PARAM_REGEX_FIXED_STRINGS, True);
		end;
	end;

end;

function TGuiSearchTextParams.SearchOptionsAsBitField : TBitField;
begin
	for var i in GUI_SEARCH_PARAMS do begin
		if i in SearchOptions then begin
			Result.SetBit(Integer(i));
		end;
	end;
end;

function TGuiSearchTextParams.SetRgOptions(const _sParamRegex : string; const _bReset : Boolean = False) : string;
begin
	if _bReset then begin
		RgOptions := TGuiSearchTextParams.RemoveRgExeOptions(RgOptions, _sParamRegex);
	end else begin
		RgOptions := TGuiSearchTextParams.AddRgExeOptions(RgOptions, _sParamRegex);
	end;
end;

function TGuiSearchTextParams.ResetRgOptions(const _sParamRegex : string; const _bReset : Boolean = False) : string;
begin
	if _bReset then begin
		RgOptions := TGuiSearchTextParams.AddRgExeOptions(RgOptions, _sParamRegex);
	end else begin
		RgOptions := TGuiSearchTextParams.RemoveRgExeOptions(RgOptions, _sParamRegex);
	end;
end;

procedure TGuiSearchTextParams.SetOption(const _searchOption : EGuiOption);
begin
	Include(SearchOptions, _searchOption);

	if _searchOption <> EGuiOption.soNotSet then begin
		Exclude(SearchOptions, EGuiOption.soNotSet);
	end;

	case _searchOption of
		EGuiOption.soNotSet :
		{ } SetRgOptions(RG_PARAM_REGEX_FIXED_STRINGS);
		EGuiOption.soMatchCase : begin
			SetRgOptions(RG_PARAM_REGEX_CASE_SENSITIVE);
			ResetRgOptions(RG_PARAM_REGEX_IGNORE_CASE);
		end;
		EGuiOption.soMatchWord :
		{ } ResetRgOptions(RG_PARAM_REGEX_FIXED_STRINGS);
		EGuiOption.soUseRegex :
		{ } ResetRgOptions(RG_PARAM_REGEX_FIXED_STRINGS);
	end;
end;

procedure TGuiSearchTextParams.SetOrReset(const _newOption : EGuiOption);
begin
	if IsSet([_newOption]) then begin
		ResetOption(_newOption);
	end else begin
		SetOption(_newOption);
	end;
end;

function TGuiSearchTextParams.SetRgOptionsWithValue(const _sParamRegex, _sValue : string; const _bUnique : Boolean = False) : string;
begin
	RgOptions := TGuiSearchTextParams.AddRgExeOptionWithValue(RgOptions, _sParamRegex, _sValue, _bUnique);
end;

procedure TGuiSearchTextParams.StoreAsDefault;
begin
	StoreDefaultSetting('SearchParams', GetAsString(True));
	inherited StoreAsDefault;
end;

function TGuiSearchTextParams.GetAsString(const _bGuiOptionsOnly : Boolean = False) : string;
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
	Result := '[' + string.Join(',', arr.Items) + ']';
	if not _bGuiOptionsOnly then begin
		Result := SearchText + ' ' + Result + ' IsRgOpSet:' + BoolToStr(IsRgExeOptionSet, True) + CRLF + RgOptions;
	end;
end;

class procedure TGuiSearchTextParams.ValidateOptions(listOptions : TStringList);
begin
	var
	arr := listOptions.IndexOfAllMatch(TOptionsHelper.GetBoundedParamRegex(RG_PARAM_END));
	Assert(Length(arr) <= 1, listOptions.DelimitedText + CRLF + 'Option list is corrupt. -- should appear only once!');
end;

constructor TGuiSearchTextParams.Create(const _ini : TMemIniFile);
begin
	inherited Create(_ini);
	IniSectionName := 'GuiSearchTextParams';
	TDebugUtils.DebugMessage('TGuiSearchTextParams.Create: ' + FIniFile.FileName + '[' + IniSectionName + ']');
	Create;
end;

constructor TGuiSearchTextParams.Create;
begin
	Clear();
end;

procedure TGuiSearchTextParams.Copy(const _other : TGuiSearchTextParams);
begin
	SearchOptions := _other.SearchOptions;
	FSearchText := _other.SearchText;

	FEscapedSearchText := _other.EscapedSearchText;
	FWordBoundedSearchText := _other.WordBoundedSearchText;
	FRgOptions := _other.RgOptions;
	FIsRgExeOptionSet := _other.IsRgExeOptionSet;
	FRgAdditionalOptions := _other.RgAdditionalOptions;
end;

procedure TGuiSearchTextParams.Init;
begin
	CreateDefaultSetting('SearchParams', varString, '');
end;

procedure TGuiSearchTextParams.ReadIni;
begin
	inherited ReadIni();
end;

procedure TGuiSearchTextParams.LoadDefault;
begin
	inherited LoadDefault();
end;

procedure TGuiSearchTextParams.RefreshMembers;
var
	sParams : string;
begin
	sParams := GetSetting('SearchParams');
	SearchOptions := GetAsSearchOptionSet(
		{ } sParams.Contains('MatchCase'),
		{ } sParams.Contains('MatchWord'),
		{ } sParams.Contains('UseRegex'));
	// SearchText := SearchText;
end;

end.
