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
	RipGrepper.Common.SimpleTypes,
	RipGrepper.Common.SearchTextWithOptions,
	RipGrepper.Settings.SettingVariant;

type
	TGuiSearchTextParams = class(TPersistableSettings)
		private
			FIsReplaceMode : Boolean;
			FRgOptions : TOptionStrings;
			FIsRgExeOptionSet : Boolean;
			FReplaceText: string;
			FExpertOptions : TOptionStrings;
			FSearchParams : IStringSetting;
			FSearchTextWithOptions : TSearchTextWithOptions;
			function GetReplaceText(): string;
			procedure LoadSearchOptionsFromDict(const _bDefault : Boolean);
			// function ResetRgOption(const _sParamRegex : string; const _bReset : Boolean = False) : string;
			procedure SetIsReplaceMode(const Value : Boolean);
			procedure SetReplaceText(const Value: string);
			procedure SetRgOptions(const Value : TOptionStrings);
			procedure UpdateSearchParamsSetting(const _options: TSearchOptionSet);

		protected
			procedure Init; override;

		public
			constructor Create(const _Owner : TPersistableSettings; const _iniSection : string); overload;
			constructor Create(const _sText, _sRepl : string; const _bMC, _bMW, _bUR : Boolean); overload;
			constructor Create(const _iniSection : string); overload;
			destructor Destroy; override;
			function AreSet(_options : TArray<EGuiOption>) : Boolean;
			procedure Clear;
			procedure Copy(const _other : TGuiSearchTextParams); reintroduce;

			procedure ResetOption(const _searchOption : EGuiOption);
			procedure SetOption(const _searchOption : EGuiOption);
			function SetRgOption(const _sParamRegex : string; const _bReset : Boolean = False) : string;
			function SetRgOptionWithValue(const _sParamRegex, _sValue : string; const _bUnique : Boolean = False) : string;
			function GetAsString(const _bGuiOptionsOnly : Boolean = False) : string;
			function GetSearchText : string;
			procedure LoadFromDict(); override;
			procedure SetSearchOptions(const _options : TSearchOptionSet);
			function GetSearchOptions : TSearchOptionSet;
			procedure SetSearchText(const _text : string);
			procedure SwitchOption(const _newOption : EGuiOption); overload;
			function ToLogString : string; override;
			procedure UpdateRgParamsByGuiOptions;
			class procedure ValidateOptions(listOptions : TStringList); static;

			property IsReplaceMode : Boolean read FIsReplaceMode write SetIsReplaceMode;
			property IsRgExeOptionSet : Boolean read FIsRgExeOptionSet write FIsRgExeOptionSet;
			property ReplaceText: string read GetReplaceText write SetReplaceText;
			property ExpertOptions : TOptionStrings read FExpertOptions write FExpertOptions;
			property RgOptions : TOptionStrings read FRgOptions write SetRgOptions;
			property SearchTextWithOptions : TSearchTextWithOptions read FSearchTextWithOptions;
	end;

implementation

uses
	System.SysUtils,
	System.RegularExpressions,
	RipGrepper.Tools.DebugUtils,
	RipGrepper.CommandLine.OptionHelper,
	RipGrepper.Common.SearchParams,
	System.StrUtils;

// for UnitTests...
constructor TGuiSearchTextParams.Create(const _sText, _sRepl : string; const _bMC, _bMW, _bUR : Boolean);
begin
	Create();
	TSearchTextWithOptions.New(_sText,
		{ } TSearchTextWithOptions.GetAsSearchOptionSet(_bMC, _bMW, _bUR));
end;

procedure TGuiSearchTextParams.Clear;
begin
	FIsRgExeOptionSet := False;
	FSearchTextWithOptions.Clear;
end;

procedure TGuiSearchTextParams.ResetOption(const _searchOption : EGuiOption);
begin
	FSearchTextWithOptions.ResetOption(_searchOption);
	UpdateRgParamsByGuiOptions();
end;

function TGuiSearchTextParams.SetRgOption(const _sParamRegex : string; const _bReset : Boolean = False) : string;
begin
	if _bReset then begin
		RgOptions.RemoveOption(_sParamRegex);
	end else begin
		RgOptions.AddOption(_sParamRegex);
	end;
end;

procedure TGuiSearchTextParams.SetOption(const _searchOption : EGuiOption);
begin
	FSearchTextWithOptions.SetOption(_searchOption);
	UpdateRgParamsByGuiOptions();
end;

function TGuiSearchTextParams.SetRgOptionWithValue(const _sParamRegex, _sValue : string; const _bUnique : Boolean = False) : string;
begin
	RgOptions.AddOptionWithValue(_sParamRegex, _sValue, _bUnique);
end;

function TGuiSearchTextParams.GetAsString(const _bGuiOptionsOnly : Boolean = False) : string;
begin
	Result := FSearchTextWithOptions.GetAsString(_bGuiOptionsOnly);
	if not _bGuiOptionsOnly then begin
		Result := Format('%s IsRgOpSet: %s' + CRLF + '%s' + CRLF + 'IsReplMode:%s - %s',
			{ } [Result, BoolToStr(IsRgExeOptionSet, True),
			{ } RgOptions.AsString, BoolToStr(IsReplaceMode, True), ReplaceText]);
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

function TGuiSearchTextParams.AreSet(_options : TArray<EGuiOption>) : Boolean;
begin
	Result := FSearchTextWithOptions.AreSet(_options);
end;

procedure TGuiSearchTextParams.Copy(const _other : TGuiSearchTextParams);
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TGuiSearchTextParams.Copy');
	inherited Copy(_other as TPersistableSettings);

	FSearchTextWithOptions.Copy(_other.FSearchTextWithOptions);

	FReplaceText := _other.ReplaceText;
	FIsReplaceMode := _other.IsReplaceMode;
	dbgMsg.MsgFmt('FReplaceText: %s', [FReplaceText, BoolToStr(FIsReplaceMode, True)]);

	FRgOptions := _other.RgOptions;
	FIsRgExeOptionSet := _other.IsRgExeOptionSet;
	FExpertOptions := _other.ExpertOptions;

	// inherited Copy(_other as TPersistableSettings);
end;

function TGuiSearchTextParams.GetReplaceText(): string;
begin
	Result := FReplaceText;
end;

function TGuiSearchTextParams.GetSearchText : string;
begin
	Result := FSearchTextWithOptions.SearchTextAsRgParam;
end;

procedure TGuiSearchTextParams.Init;
begin
	FSearchParams := TStringSetting.Create('');
	CreateSetting('SearchParams', FSearchParams);
end;

procedure TGuiSearchTextParams.LoadFromDict();   // ok
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
	sParams := FSearchParams.Value;
	dbgMsg.Msg(RgOptions.AsString);
	FSearchTextWithOptions.UpdateSearchOptions(sParams);
	if FSearchTextWithOptions.SearchOptions = [] then begin
		SetOption(EGuiOption.soNotSet);
	end else begin
		for var so in FSearchTextWithOptions.SearchOptions do begin
			SetOption(so);
		end;
	end;
	dbgMsg.Msg(RgOptions.AsString);
end;

procedure TGuiSearchTextParams.SetIsReplaceMode(const Value : Boolean);
begin
	FIsReplaceMode := Value;
end;

procedure TGuiSearchTextParams.SetSearchOptions(const _options : TSearchOptionSet);
begin
	FSearchTextWithOptions.SearchOptions := _options;
	UpdateRgParamsByGuiOptions();
	UpdateSearchParamsSetting(_options);
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

function TGuiSearchTextParams.GetSearchOptions : TSearchOptionSet;
begin
	Result := FSearchTextWithOptions.SearchOptions;
end;

procedure TGuiSearchTextParams.SetReplaceText(const Value: string);
begin
	FReplaceText := Value;
end;

procedure TGuiSearchTextParams.SetSearchText(const _text : string);
begin
	FSearchTextWithOptions.SearchTextOfUser := _text;
	UpdateRgParamsByGuiOptions();
end;

procedure TGuiSearchTextParams.SwitchOption(const _newOption : EGuiOption);
begin
    FSearchTextWithOptions.SwitchOption(_newOption);
end;

function TGuiSearchTextParams.ToLogString : string;
begin
	Result := GetAsString();
end;

procedure TGuiSearchTextParams.UpdateRgParamsByGuiOptions;
var
	backupOptions : TArrayEx<string>;
begin

	// backup non option case options
	for var op in self.RgOptions.AsArray() do begin
		var
		guiSearchOpArr := string.Join(ARRAY_SEPARATOR, RG_GUI_SEARCH_OPTIONS).Split([ARRAY_SEPARATOR]);
		if not MatchStr(op, guiSearchOpArr) then begin
			backupOptions.Add(op);
		end;
	end;

	for var op : TSearchOptionToRgOptions in SEARCH_OPTION_CASES do begin
		if op.SearchOption = self.FSearchTextWithOptions.SearchOptions then begin
			var
				opArr : TArrayEx<string>;
			for var os in op.RgOptions do begin
				opArr.Add(TParamRegexHelper.GetLongParam(os));
			end;

			self.RgOptions := TOptionStrings.New(opArr);
			break;
		end;
	end;

	self.RgOptions.Copy(backupOptions);
end;

procedure TGuiSearchTextParams.UpdateSearchParamsSetting(const _options: TSearchOptionSet);
begin
	FSearchParams.Value := TSearchTextWithOptions.SearchOptionSetToString(_options);
end;

end.
