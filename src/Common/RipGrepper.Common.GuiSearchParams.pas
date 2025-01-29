unit RipGrepper.Common.GuiSearchParams;

interface

uses
	RipGrepper.Common.Constants,
	ArrayEx,
	System.Classes,
	RipGrepper.Settings.Persistable,
	System.IniFiles,
	RipGrepper.CommandLine.OptionStrings,
	RipGrepper.Helper.Types,
	RipGrepper.Common.SimpleTypes;

type

	TGuiSearchTextParams = class(TPersistableSettings)
		private
			FSearchText : string;
			FEscapedSearchText : string;
			FIsReplaceMode : Boolean;
			FWordBoundedSearchText : string;
			FRgOptions : TOptionStrings;
			FIsRgExeOptionSet : Boolean;
			FReplaceText : string;
			FExpertOptions : TOptionStrings;

			function GetEscapedSearchText : string;
			function GetReplaceText : string;
			function GetSearchText : string;
			function GetWordBoundedSearchText : string;
			procedure LoadSearchOptionsFromDict(const _bDefault : Boolean);
			function ResetRgOption(const _sParamRegex : string; const _bReset : Boolean = False) : string;
			procedure SetIsReplaceMode(const Value : Boolean);
			procedure SetRgOptions(const Value : TOptionStrings);

		protected
			procedure Init; override;

		public
			SearchOptions : TSearchOptionSet;

			constructor Create(const _Owner : TPersistableSettings; const _iniSection : string); overload;
			constructor Create(const _sText, _sRepl : string; const _bMC, _bMW, _bUR : Boolean); overload;
			constructor Create(const _iniSection : string); overload;
			destructor Destroy; override;
			procedure Clear;
			procedure Copy(const _other : TGuiSearchTextParams); reintroduce;

			procedure CopyDefaultsToValues; override;
			procedure CopyValuesToDefaults; override;
			function GetNext(const _newOption : EGuiOption) : TGuiSearchTextParams;
			function IsSet(_options : TArray<EGuiOption>) : Boolean;

			procedure ResetOption(const _searchOption : EGuiOption);
			function SearchOptionsAsBitField : TBitField;
			procedure SetOption(const _searchOption : EGuiOption);
			procedure SwitchOption(const _newOption : EGuiOption); overload;
			function SetRgOption(const _sParamRegex : string; const _bReset : Boolean = False) : string;
			function SetRgOptionWithValue(const _sParamRegex, _sValue : string; const _bUnique : Boolean = False) : string;
			procedure StoreAsDefaultsToDict; override;
			function GetAsString(const _bGuiOptionsOnly : Boolean = False) : string;
			procedure LoadDefaultsFromDict; override;
			procedure LoadFromDict(); override;
			procedure StoreToDict; override;
			function ToLogString : string; override;
			procedure UpdateRgParamsByGuiOptions;
			class procedure ValidateOptions(listOptions : TStringList); static;

			property EscapedSearchText : string read GetEscapedSearchText;
			property IsReplaceMode : Boolean read FIsReplaceMode write SetIsReplaceMode;
			property IsRgExeOptionSet : Boolean read FIsRgExeOptionSet write FIsRgExeOptionSet;
			property ReplaceText : string read GetReplaceText write FReplaceText;
			property ExpertOptions : TOptionStrings read FExpertOptions write FExpertOptions;
			property RgOptions : TOptionStrings read FRgOptions write SetRgOptions;
			property SearchText : string read GetSearchText write FSearchText;
			property WordBoundedSearchText : string read GetWordBoundedSearchText;
	end;

implementation

uses
	System.SysUtils,
	System.RegularExpressions,
	RipGrepper.Tools.DebugUtils,
	RipGrepper.CommandLine.OptionHelper,
	RipGrepper.Common.SearchParams;

// for UnitTests...
constructor TGuiSearchTextParams.Create(const _sText, _sRepl : string; const _bMC, _bMW, _bUR : Boolean);
begin
	Create();
	SearchText := _sText;
	SearchOptions := TSearchParams.GetAsSearchOptionSet(_bMC, _bMW, _bUR);
end;

function TGuiSearchTextParams.GetNext(const _newOption : EGuiOption) : TGuiSearchTextParams;
begin
	Result := self;
	case _newOption of
		EGuiOption.soMatchCase : begin
			Result.SwitchOption(EGuiOption.soMatchCase);
		end;

		EGuiOption.soMatchWord : begin
			Result.SwitchOption(EGuiOption.soMatchWord);
		end;

		EGuiOption.soUseRegex : begin
			Result.SwitchOption(EGuiOption.soUseRegex);
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
		SetRgOption(RG_PARAM_REGEX_FIXED_STRINGS);
		SetRgOption(RG_PARAM_REGEX_IGNORE_CASE);
	end;

	case searchOption of
		EGuiOption.soNotSet :
		{ };
		EGuiOption.soMatchCase : begin
			ResetRgOption(RG_PARAM_REGEX_CASE_SENSITIVE);
			SetRgOption(RG_PARAM_REGEX_IGNORE_CASE);
		end;
		EGuiOption.soMatchWord, EGuiOption.soUseRegex :
		if not( //
			(SearchOptions = [EGuiOption.soNotSet]) or //
			(SearchOptions = [EGuiOption.soMatchCase]) or //
			(SearchOptions = [])) then begin
			ResetRgOption(RG_PARAM_REGEX_FIXED_STRINGS);
			SetRgOption(RG_PARAM_REGEX_IGNORE_CASE);
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

function TGuiSearchTextParams.SetRgOption(const _sParamRegex : string; const _bReset : Boolean = False) : string;
begin
	if _bReset then begin
		RgOptions.RemoveOption(_sParamRegex);
	end else begin
		RgOptions.AddOption(_sParamRegex);
	end;
end;

function TGuiSearchTextParams.ResetRgOption(const _sParamRegex : string; const _bReset : Boolean = False) : string;
begin
	if _bReset then begin
		RgOptions.AddOption(_sParamRegex);
	end else begin
		RgOptions.RemoveOption(_sParamRegex);
	end;
end;

procedure TGuiSearchTextParams.SetOption(const _searchOption : EGuiOption);
begin
	Include(SearchOptions, _searchOption);

	if _searchOption <> EGuiOption.soNotSet then begin
		Exclude(SearchOptions, EGuiOption.soNotSet);
	end;

	case _searchOption of
		EGuiOption.soNotSet : begin
			// ignore case by default
			ResetRgOption(RG_PARAM_REGEX_CASE_SENSITIVE);
			SetRgOption(RG_PARAM_REGEX_IGNORE_CASE);
			SetRgOption(RG_PARAM_REGEX_FIXED_STRINGS);
		end;
		EGuiOption.soMatchCase : begin
			ResetRgOption(RG_PARAM_REGEX_IGNORE_CASE);
			SetRgOption(RG_PARAM_REGEX_CASE_SENSITIVE);
		end;
		EGuiOption.soMatchWord :
		{ } ResetRgOption(RG_PARAM_REGEX_FIXED_STRINGS);
		EGuiOption.soUseRegex :
		{ } ResetRgOption(RG_PARAM_REGEX_FIXED_STRINGS);
	end;
end;

procedure TGuiSearchTextParams.SwitchOption(const _newOption : EGuiOption);
begin
	if IsSet([_newOption]) then begin
		ResetOption(_newOption);
	end else begin
		SetOption(_newOption);
	end;
end;

function TGuiSearchTextParams.SetRgOptionWithValue(const _sParamRegex, _sValue : string; const _bUnique : Boolean = False) : string;
begin
	RgOptions.AddOptionWithValue(_sParamRegex, _sValue, _bUnique);
end;

procedure TGuiSearchTextParams.StoreAsDefaultsToDict;
begin
	SettingsDict.StoreDefaultSetting('SearchParams', GetAsString(True));
	inherited StoreAsDefaultsToDict;
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
		Result := Format('%s %s IsRgOpSet: %s' + CRLF + '%s' + CRLF + 'IsReplMmode:%s - %s',
			[SearchText, Result, BoolToStr(IsRgExeOptionSet, True), RgOptions.AsString, BoolToStr(IsReplaceMode, True), ReplaceText]);
	end;
end;

class procedure TGuiSearchTextParams.ValidateOptions(listOptions : TStringList);
begin
	var
	arr := listOptions.IndexOfAllMatch(TOptionsHelper.GetBoundedParamRegex(RG_PARAM_END));
	Assert(Length(arr) <= 1, listOptions.DelimitedText + CRLF + 'Option list is corrupt. -- should appear only once!');
end;

constructor TGuiSearchTextParams.Create(const _Owner : TPersistableSettings; const _iniSection : string);
begin
	IniSectionName := _iniSection;
	inherited Create(_Owner);
	var
	dbgMsg := TDebugMsgBeginEnd.New('TGuiSearchTextParams.Create', True);
	dbgMsg.MsgFmt('Create %p for section: %s', [Pointer(self), IniSectionName]);
	Clear();
end;

constructor TGuiSearchTextParams.Create(const _iniSection : string);
begin
	IniSectionName := _iniSection;
	inherited Create(); // own ini file
	Clear();
end;

destructor TGuiSearchTextParams.Destroy;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TGuiSearchTextParams.Destroy', True);
	dbgMsg.MsgFmt('Destroy %p for section: %s', [Pointer(self), IniSectionName]);
	inherited Destroy(); // ok
end;

procedure TGuiSearchTextParams.Copy(const _other : TGuiSearchTextParams);
begin
	SearchOptions := _other.SearchOptions;
	FSearchText := _other.SearchText;
	FReplaceText := _other.ReplaceText;
	FIsReplaceMode := _other.IsReplaceMode;

	FEscapedSearchText := _other.EscapedSearchText;
	FWordBoundedSearchText := _other.WordBoundedSearchText;
	FRgOptions := _other.RgOptions;
	FIsRgExeOptionSet := _other.IsRgExeOptionSet;
	FExpertOptions := _other.ExpertOptions;

	inherited Copy(_other as TPersistableSettings);
end;

procedure TGuiSearchTextParams.CopyDefaultsToValues;
begin
	inherited CopyDefaultsToValues;
end;

procedure TGuiSearchTextParams.CopyValuesToDefaults;
begin
	inherited CopyValuesToDefaults;
end;

function TGuiSearchTextParams.GetReplaceText : string;
begin
	Result := FReplaceText;
end;

procedure TGuiSearchTextParams.Init;
begin
	SettingsDict.CreateDefaultRelevantSetting('SearchParams', varString, '');
end;

procedure TGuiSearchTextParams.LoadDefaultsFromDict;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TGuiSearchTextParams.LoadDefaultsFromDict');
	LoadSearchOptionsFromDict(True);
end;

procedure TGuiSearchTextParams.LoadFromDict();
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TGuiSearchTextParams.LoadFromDict');
	LoadSearchOptionsFromDict(False);
end;

procedure TGuiSearchTextParams.LoadSearchOptionsFromDict(const _bDefault : Boolean);
var
	sParams : string;
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TGuiSearchTextParams.LoadSearchOptionsFromDict Default=' + BoolToStr(_bDefault));
	sParams := SettingsDict.GetSetting('SearchParams', _bDefault);
	dbgMsg.Msg(RgOptions.AsString);
	SearchOptions := TSearchParams.StringToSearchParams(sParams);
	if SearchOptions = [] then begin
		SetOption(EGuiOption.soNotSet);
	end else begin
		for var so in SearchOptions do begin
			SetOption(so);
		end;
	end;
	dbgMsg.Msg(RgOptions.AsString);
end;

procedure TGuiSearchTextParams.SetIsReplaceMode(const Value : Boolean);
begin
	FIsReplaceMode := Value;
end;

procedure TGuiSearchTextParams.SetRgOptions(const Value : TOptionStrings);
begin
	FRgOptions := Value;
	{$IFDEF DEBUG}
	if Value.AsArray.Contains('--ignore-case') then begin
		if Value.AsArray.Contains('--case-sensitive') then begin
			Assert(True, '--case-sensitive and --ignore-case shouldn''t be there at the same time');
		end;
	end;
	{$ENDIF}
end;

procedure TGuiSearchTextParams.StoreToDict;
begin
	SettingsDict.StoreSetting('SearchParams', GetAsString(True));
	inherited StoreToDict();
end;

function TGuiSearchTextParams.ToLogString : string;
begin
	Result := GetAsString();
end;

procedure TGuiSearchTextParams.UpdateRgParamsByGuiOptions;
begin
	for var op : TSearchOptionToRgOptions in SEARCH_OPTION_CASES do begin
		if op.SearchOption = self.SearchOptions then begin
			var opArr : TArrayEx<string>;
			for var os in op.RgOptions do begin
				opArr.Add(TParamRegexHelper.GetLongParam(os));
			end;

			self.RgOptions := TOptionStrings.New(opArr);
			break;
		end;
	end;
end;

end.
