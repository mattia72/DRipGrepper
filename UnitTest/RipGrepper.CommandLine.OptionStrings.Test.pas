unit RipGrepper.CommandLine.OptionStrings.Test;

interface

uses
	DUnitX.TestFramework,
	RipGrepper.Common.GuiSearchParams,
	RipGrepper.Common.Settings.RipGrepParameterSettings,
	System.IniFiles,
	RipGrepper.Common.Constants;

type

	[TestFixture]
	TOptionStringsTest = class

		private
			FGuiParams : TGuiSearchTextParams;
			FIniFile : TMemIniFile;
			FParams : TRipGrepParameterSettings;
			function SetSearchOptions(const _guiOptionsActual : string) : TSearchOptionSet;

		public
			[Setup]
			procedure Setup;
			[TearDown]
			procedure TearDown;

			[Test]
			[Testcase('test1', '--vimgrep -g=*.txt --fixed-strings -g=*.ini --ignore-case -g=*.bak;' + RG_PARAM_REGEX_IGNORE_CASE, ';')]
			[Testcase('test2', '--vimgrep -g=!*.txt --fixed-strings -g=!*.ini -i         -g=!*.bak;' + RG_PARAM_REGEX_IGNORE_CASE, ';')]
			[Testcase('test not exist', '--vimgrep -g=!*.txt --fixed-strings -g=!*.ini -g=!*.bak;' + '-nop|--noparam;1', ';')]
			procedure TestIsOptionSet(const _sOptions, _sParamRegex : string; _bNotSet : Boolean = False);

			[Test]
			[Testcase('test1', '--vimgrep -g=*.txt --fixed-strings -g=*.ini --ignore-case -g=*.bak;' + RG_PARAM_REGEX_GLOB + ';0', ';')]
			[Testcase('test2', '--vimgrep -g=!*.txt --fixed-strings -g=!*.ini -i         -g=!*.bak;' + RG_PARAM_REGEX_FIXED_STRINGS +
				';1', ';')]
			procedure TestRemoveAllParams(const _sOptions, _sRegEx : string; const _bSwitch : Integer);

			[Test]
			[Testcase('test1', '--vimgrep -g=!*.txt -F -g=*.ini -i -g=!*.bak;' + RG_PARAM_REGEX_GLOB + ';*.ini;1', ';')]
			[Testcase('test2', '--vimgrep -g=!*.txt -F -g=!*.ini -i -g=!*.bak;' + RG_PARAM_REGEX_GLOB + ';*.ini;0', ';')]
			[Testcase('test3', '--vimgrep -g=!*.txt -F -g=!*.ini -i -g=!*.bak;' + RG_PARAM_REGEX_GLOB + ';*.iii;0', ';')]
			procedure TestIsOptionSetWithValue(const _sOptions, _sParamRegex, _sValue : string; const _bOk : integer);

			[Test]
			[Testcase('test1', '--vimgrep --fixed-strings -g=*.ini --ignore-case -g=!*.bak;' + RG_PARAM_REGEX_IGNORE_CASE + ';1', ';')]
			[Testcase('test2', '--vimgrep --fixed-strings -g=*.ini -i         -g=!*.bak;' + RG_PARAM_REGEX_IGNORE_CASE + ';1', ';')]
			[Testcase('test3', '--vimgrep --fixed-strings -g=*.ini -i         -g=!*.bak;' + RG_PARAM_REGEX_IGNORE_CASE + ';1', ';')]
			[Testcase('test4', '--vimgrep --fixed-strings -g=*.ini            -g=!*.bak;' + RG_PARAM_REGEX_IGNORE_CASE + ';0', ';')]
			[Testcase('test5', '--vimgrep --fixed-strings -g=*.ini            -g=!*.bak;' + RG_PARAM_REGEX_FIXED_STRINGS + ';1', ';')]
			[Testcase('test6', '--vimgrep --fixed-strings -g=*.ini            -g=!*.bak;' + RG_PARAM_REGEX_GLOB + ';1', ';')]
			[Testcase('test7', '--vimgrep --fixed-strings -g=*.ini --ignore-case -g=!*.bak;' + RG_PARAM_REGEX_IGNORE_CASE + ';1', ';')]
			[Testcase('test8', '--vimgrep -F              -g=*.ini            -g=!*.bak;' + RG_PARAM_REGEX_FIXED_STRINGS + ';1', ';')]
			[Testcase('test9', '--vimgrep                 -g=*.ini            -g=!*.bak;' + RG_PARAM_REGEX_FIXED_STRINGS + ';0', ';')]
			[Testcase('test10', '--vimgrep --hidden -F    -g=*.ini            -g=!*.bak;' + RG_PARAM_REGEX_HIDDEN + ';1', ';')]
			[Testcase('test11', '--vimgrep -.             -g=*.ini            -g=!*.bak;' + RG_PARAM_REGEX_HIDDEN + ';1', ';')]
			[Testcase('test12', '--vimgrep          -F    -g=*.ini            -g=!*.bak;' + RG_PARAM_REGEX_HIDDEN + ';0', ';')]
			[Testcase('test13', '--vimgrep                -g=*.ini            -g=!*.bak;' + RG_PARAM_REGEX_HIDDEN + ';0', ';')]
			procedure TestUpdateOptions(const _sOptions, _sRegEx : string; const _bRemove : Integer);

			[Test]
			[Testcase('test1', '--vimgrep  --fixed-strings -g=*.ini -g=!*.bak;' + '-g=!*.bak;', ';')]
			[Testcase('test2', '--vimgrep  -g=*.ini -g=!*.bak --fixed-strings -i;' + '-g=*.ini;', ';')]
			[Testcase('test3', '--vimgrep  -r=replace --fixed-strings -i;' + '-r=replace;', ';')]
			[Testcase('test4', '--vimgrep  -r="replace more word" --fixed-strings -i;' + '-r="replace more word";', ';')]
			procedure TestRemoveOptionsWithValue(const _sOptions, _sOptionWithValue : string);

			[Test]
			{ ________________________________________________________________________W|R|B|E_____ }
			{ } [Testcase('Single word  MW  UR', '-p1 --fixed-strings --p2 --|aa1    |1|1|1', '|')]
			{ } [Testcase('Double word  MW  UR', '-p1 --fixed-strings --p2 --|aa2 bbb|1|1|1', '|')]
			{ } [Testcase('Single word nMW nUR', '-p1 --fixed-strings --p2 --|aa3    |0|0|0', '|')]
			{ } [Testcase('Double word nMW nUR', '-p1 --fixed-strings --p2 --|aa4 bbb|0|0|0', '|')]
			{ } [Testcase('Double word nMW nUR', '-p1 --fixed-strings --p2 --|a*5 b$b|0|0|0|1', '|')]
			{ } [Testcase('Single word  MW nUR', '-p1                 --p2 --|aa6    |0|1|0', '|')]
			{ } [Testcase('Double word  MW nUR', '-p1                 --p2 --|aa7 bbb|0|1|0', '|')]
			procedure TestUpdateSearchText(const _sOptions, _sSearchText : string;
				const _bMatchWord, _bUseRegex, _bShouldBounded, _bShouldEscaped : Integer);

			[Test]
			[Testcase('test soMatchCase', '1;1;' + RG_PARAM_REGEX_CASE_SENSITIVE, ';')]
			[Testcase('test soMatchWord', '2;0;' + RG_PARAM_REGEX_FIXED_STRINGS, ';')]
			[Testcase('test soUseRegex ', '3;0;' + RG_PARAM_REGEX_FIXED_STRINGS, ';')]
			procedure TestGetOptionsAndSetFlag(const _guiOption, _bMatch : Integer; const _paramRegex : string);

			[Test]
			[Testcase('test soMatchCase', '1;0;' + RG_PARAM_REGEX_CASE_SENSITIVE, ';')]
			[Testcase('test soMatchWord', '2;1;' + RG_PARAM_REGEX_FIXED_STRINGS, ';')]
			[Testcase('test soUseRegex ', '3;1;' + RG_PARAM_REGEX_FIXED_STRINGS, ';')]
			procedure TestGetOptionsAndSetFlagReset(const _guiOption, bMatch : Integer; const _paramRegex : string);

			[Test]
			[Testcase('test ', '')]
			[Testcase('test soMatchCase', '1')]
			[Testcase('test soMatchWord', '2')]
			[Testcase('test soUseRegex ', '3')]
			[Testcase('test soMatchCasesoMatchWord', '1#2')]
			[Testcase('test soMatchCasesoUseRegex', '1#3')]
			[Testcase('test soMatchWordsoUseRegex', '2#3')]
			[Testcase('test soAll', '1#2#3')]
			procedure TestGetOptionsAndSetFlagsoUseRegex(const _guiOptionsActual : string);

			[Test]
			[Testcase('test1 soNotSet', '0')]
			[Testcase('test2 soMatchCase', '1')]
			[Testcase('test3 soMatchWord', '2')]
			[Testcase('test4 soUseRegex ', '3')]
			[Testcase('test5 soMatchCasesoMatchWord', '1#2')]
			[Testcase('test6 soMatchCasesoUseRegex', '1#3')]
			[Testcase('test7 soMatchWordsoUseRegex', '2#3')]
			[Testcase('test8 soAll', '1#2#3')]
			procedure TestGetOptionsAndSetFlagResetUseRegex(const _guiOptionsActual : string);

			[Test]
			[Testcase('test1 ', '')]
			[Testcase('test2 soMatchCase', '1')]
			[Testcase('test3 soMatchWord', '2')]
			[Testcase('test4 soUseRegex ', '3')]
			[Testcase('test5 soMatchWord', '1#2')]
			[Testcase('test6 soUseRegex ', '1#3')]
			[Testcase('test7 soMatchWord', '2#3')]
			[Testcase('test8 soMatchWord', '1#2#3')]
			procedure TestGetOptionsAndSetFlagResetMatchWord(const _guiOptionsActual : string);

			[Test]
			[Testcase('test1 ', '')]
			[Testcase('test2 soMatchCase', '1')]
			[Testcase('test3 soMatchWord', '2')]
			[Testcase('test4 soUseRegex ', '3')]
			[Testcase('test5 soMatchWord', '1#2')]
			[Testcase('test6 soUseRegex ', '1#3')]
			[Testcase('test7 soMatchWord', '2#3')]
			[Testcase('test8 soMatchWord', '1#2#3')]
			procedure TestGetOptionsAndSetFlagMatchWord(const _guiOptionsActual : string);

			[Test]
			[Testcase('test1', '--vimgrep  --fixed-strings -g=*.ini -g=!*.bak;' + RG_PARAM_REGEX_GLOB + ';*.bbb;0', ';')]
			[Testcase('test2', '--vimgrep  -g=*.ini -g=!*.bak --fixed-strings -i;' + RG_PARAM_REGEX_CONTEXT + ';99;1', ';')]
			[Testcase('test3', '--vimgrep  -C=11 -g=!*.bak --fixed-strings -i;' + RG_PARAM_REGEX_CONTEXT + ';99;1', ';')]
			[Testcase('test4', '--vimgrep  -C=99 -g=!*.bak --fixed-strings -i;' + RG_PARAM_REGEX_CONTEXT + ';99;1', ';')]
			[Testcase('test5', '--vimgrep  --context=11 -g=!*.bak --fixed-strings -i;' + RG_PARAM_REGEX_CONTEXT + ';99;1', ';')]
			[Testcase('test6', '--vimgrep  --context=99 -g=!*.bak --fixed-strings -i;' + RG_PARAM_REGEX_CONTEXT + ';99;1', ';')]
			[Testcase('test7', '--vimgrep  --context=99 -g=!*.bak --fixed-strings -i;' + RG_PARAM_REGEX_REPLACE + ';replace;1', ';')]
			[Testcase('test8', '--vimgrep  --context=99 -g=!*.bak --fixed-strings -i;' + RG_PARAM_REGEX_REPLACE +
				';replace text more world;1', ';')]
			procedure TestAddOptionsWithValue(const _sOptions, _sRegEx, _sValue : string; const _bUnique : integer);

			[Test]
			[Testcase('test1', RG_PARAM_REGEX_IGNORE_CASE)]
			[Testcase('test2', RG_PARAM_REGEX_CASE_SENSITIVE)]
			[Testcase('test3', RG_PARAM_REGEX_FIXED_STRINGS)]
			[Testcase('test4', RG_PARAM_REGEX_WORD_REGEX)]
			[Testcase('test5', RG_PARAM_REGEX_GLOB)]
			[Testcase('test6', RG_PARAM_REGEX_HIDDEN)]
			[Testcase('test7', RG_PARAM_REGEX_NO_IGNORE)]
			[Testcase('test8', RG_PARAM_REGEX_PRETTY)]
			[Testcase('test9', RG_PARAM_REGEX_CONTEXT)]
			[Testcase('test10', RG_PARAM_REGEX_ENCODING)]
			[Testcase('test11', RG_PARAM_REGEX_REPLACE)]
			// [Testcase('test12', RG_PARAM_END)]
			[Testcase('1_test1', RG_PARAM_REGEX_GLOB + '=')]
			[Testcase('1_test2', RG_PARAM_REGEX_CONTEXT + '=')]
			[Testcase('1_test3', RG_PARAM_REGEX_ENCODING + '=')]
			[Testcase('1_test4', RG_PARAM_REGEX_REPLACE + '=')]
			[Testcase('2_test1', RG_PARAM_REGEX_GLOB + '=value')]
			[Testcase('2_test2', RG_PARAM_REGEX_CONTEXT + '=value')]
			[Testcase('2_test3', RG_PARAM_REGEX_ENCODING + '=value')]
			[Testcase('2_test4', RG_PARAM_REGEX_REPLACE + '=value')]
			[Testcase('2_test4', RG_PARAM_REGEX_REPLACE + '=more world value')]
			procedure TestGetOptionVariantsAndValue(const _sParamRegex : string);

			[Test]

			[Testcase('test1', '-x --YYYY --vimgrep  -r="replace more word" --fixed-strings -i;', ';')]
			[Testcase('test1', '-x --YYYY --vimgrep  --replace=replace_word --fixed-strings -i;', ';')]
			[Testcase('test1', '-x --YYYY --vimgrep  --replace=replace_word --fixed-strings -i;', ';')]
			[Testcase('test2', '-x --YYYY --vimgrep  -g=*.ini -g=!*.bak --fixed-strings -i;', ';')]
			[Testcase('test2', '-x --YYYY --vimgrep  -C=11 -g=!*.bak --fixed-strings -i;', ';')]
			[Testcase('test2', '-x --YYYY --vimgrep  --context=99 -g=!*.bak --fixed-strings -i;', ';')]
			procedure TestRemoveMultipleOptionsWithValue(const _sOptions : string);

			[Test]
			[Testcase('test1', '--vimgrep  --fixed-strings -g=*.ini -g=!*.bak;' + RG_PARAM_REGEX_GLOB + ';*.bbb;0', ';')]
			[Testcase('test2', '--vimgrep  -g=*.ini -g=!*.bak --fixed-strings -i;' + RG_PARAM_REGEX_CONTEXT + ';99;1', ';')]
			[Testcase('test3', '--vimgrep  -C=11 -g=!*.bak --fixed-strings -i;' + RG_PARAM_REGEX_CONTEXT + ';99;1', ';')]
			[Testcase('test4', '--vimgrep  -C=99 -g=!*.bak --fixed-strings -i;' + RG_PARAM_REGEX_CONTEXT + ';99;1', ';')]
			[Testcase('test5', '--vimgrep  --context=11 -g=!*.bak --fixed-strings -i;' + RG_PARAM_REGEX_CONTEXT + ';99;1', ';')]
			[Testcase('test6', '--vimgrep  --context=99 -g=!*.bak --fixed-strings -i;' + RG_PARAM_REGEX_CONTEXT + ';99;1', ';')]
			[Testcase('test7', '--vimgrep  --context=99 -g=!*.bak --fixed-strings -i;' + RG_PARAM_REGEX_REPLACE + ';replace;1', ';')]
			[Testcase('test8', '--vimgrep  --context=99 -g=!*.bak --fixed-strings -i;' + RG_PARAM_REGEX_REPLACE +
				';replace text more world;1', ';')]
			procedure TestToArray(const _sOptions, _sRegEx, _sValue : string; const _bUnique : integer);
			[Test]
			{ } [Testcase('test1', '--vimgrep  --fixed-strings -g=*.ini -g=!*.bak|' + '*.pas;*.dfm;!ext/', '|')]
			{ } [Testcase('test2', '--vimgrep  -g=!*.txt -F -g=*.ini -i -g=!*.bak -i|' + '*.pas;*.dfm;!ext/', '|')]
			{ } [Testcase('test3', '--vimgrep  -C=11 -g=!*.txt -F -g=*.ini -i -g=!*.bak|' + '*.pas;*.dfm;!ext/', '|')]
			{ } [Testcase('test4', '--vimgrep  -C=99 -g=!*.txt -F -g=*.ini -i -g=!*.bak|' + '*.pas;*.dfm;!ext/', '|')]
			{ } [Testcase('test5', '--vimgrep  --context=11 -g=!*.txt -F -g=*.ini -i -g=!*.bak|' + '*.pas;*.dfm;!ext/', '|')]
			{ } [Testcase('test6', '--vimgrep  --context=99 -g=!*.txt -F -g=*.ini -i -g=!*.bak|' + '*.pas;*.dfm;!ext/', '|')]
			{ } [Testcase('test7', '--vimgrep  --context=99 -g=!*.txt -F -g=*.ini -i -g=!*.bak|' + '*.pas;*.dfm;!ext/', '|')]
			{ } [Testcase('test8', '--vimgrep  --context=99 -g=!*.txt -F -g=*.ini -i -g=!*.bak|' + '*.pas;*.dfm;!ext/', '|')]
			procedure TestUpdateFileMasks(const _sOptions, _sNewMasks : string);
	end;

implementation

uses
	RipGrepper.CommandLine.OptionStrings,
	RipGrepper.CommandLine.OptionHelper,
	System.SysUtils,
	System.RegularExpressions,
	ArrayEx,
	System.StrUtils,
	System.Math;

function TOptionStringsTest.SetSearchOptions(const _guiOptionsActual : string) : TSearchOptionSet;
begin
	Result := [];
	for var s : string in _guiOptionsActual.Split(['#']) do begin
		Result := Result + [EGuiOption(integer.Parse(s))];
	end;
end;

procedure TOptionStringsTest.Setup;
begin
	FIniFile := TMemIniFile.Create('DripGrepperUnittest.ini', TEncoding.UTF8);
	FParams := TRipGrepParameterSettings.Create(FIniFile);
	FGuiParams := FParams.GuiSearchTextParams;
	FGuiParams.SearchText := 'search text';
end;

procedure TOptionStringsTest.TearDown;
begin
	FParams.Free;
	FIniFile.Free;
end;

procedure TOptionStringsTest.TestIsOptionSet(const _sOptions, _sParamRegex : string; _bNotSet : Boolean = False);
begin
	var
	op := TOptionStrings.New(_sOptions);
	if _bNotSet then begin
		Assert.IsFalse(op.IsOptionSet(_sParamRegex), '''' + _sParamRegex + ''' should NOT be in the array');
	end else begin
		Assert.IsTrue(op.IsOptionSet(_sParamRegex), '''' + _sParamRegex + ''' should be in the array');
	end;
end;

procedure TOptionStringsTest.TestGetOptionsAndSetFlag(const _guiOption, _bMatch : Integer; const _paramRegex : string);
var
	newGuiSearchOption : EGuiOption;
	bIsOpOk : Boolean;
	rgExeOps : string;

begin
	newGuiSearchOption := EGuiOption(_guiOption);

	rgExeOps := string.Join(' ', RG_NECESSARY_PARAMS);
	FguiParams.SearchOptions := [];
	bIsOpOk := TRegex.IsMatch(rgExeOps, TOptionsHelper.GetBoundedParamRegex(_paramRegex.Split(['|'])[1]));

	FguiParams.SetOption(newGuiSearchOption);

	Assert.IsTrue(not bIsOpOk, '''' + _paramRegex + ''' should not contains initially');
	Assert.AreEqual(_bMatch = 1, FguiParams.RgOptions.IsOptionSet(_paramRegex), '''' + _paramRegex + ''' should be in the options');
	Assert.IsTrue(FguiParams.IsSet([newGuiSearchOption]), Integer(newGuiSearchOption).ToString + ' should be in the options');
end;

procedure TOptionStringsTest.TestGetOptionsAndSetFlagReset(const _guiOption, bMatch : Integer; const _paramRegex : string);
var
	_resetGuiSearchOption : EGuiOption;
	bIsOpOk : Boolean;
	rgExeOps : string;
begin
	_resetGuiSearchOption := EGuiOption(_guiOption);

	var
	sLongParam := _paramRegex.Split(['|'])[1];

	rgExeOps := string.Join(' ', RG_NECESSARY_PARAMS) + ' ' + sLongParam;
	FguiParams.SearchOptions := [_resetGuiSearchOption];

	bIsOpOk := TRegex.IsMatch(rgExeOps, TOptionsHelper.GetBoundedParamRegex(sLongParam));

	FGuiParams.ReSetOption(_resetGuiSearchOption);

	Assert.IsTrue(bIsOpOk, '''' + _paramRegex + ''' should contain initially');
	Assert.AreEqual(bMatch = 1, FGuiParams.RgOptions.IsOptionSet(_paramRegex), '''' + _paramRegex + ''' should not be in the options');
	Assert.IsTrue(not FGuiParams.IsSet([_resetGuiSearchOption]), Integer(_resetGuiSearchOption).ToString + ' should not be in the options');
end;

procedure TOptionStringsTest.TestGetOptionsAndSetFlagsoUseRegex(const _guiOptionsActual : string);
var
	bIsOpOk : Boolean;
	rgExeOps : string;
begin
	var
	sLongParam := RG_PARAM_REGEX_FIXED_STRINGS.Split(['|'])[1];

	rgExeOps := string.Join(' ', RG_NECESSARY_PARAMS) + ' ' + sLongParam;

	FGuiParams.SearchOptions := SetSearchOptions(_guiOptionsActual);

	bIsOpOk := TRegex.IsMatch(rgExeOps, TOptionsHelper.GetBoundedParamRegex(sLongParam));

	FGuiParams.SetOption(EGuiOption.soUseRegex);

	Assert.IsTrue(bIsOpOk, '''' + sLongParam + ''' should contain initially');
	Assert.IsTrue(not FGuiParams.RgOptions.IsOptionSet(RG_PARAM_REGEX_FIXED_STRINGS),
		{ } '''' + sLongParam + ''' should not be in the options');
	Assert.IsTrue(FGuiParams.IsSet([EGuiOption.soUseRegex]),
		{ } Integer(EGuiOption.soUseRegex).ToString + ' should not be in the options');
end;

procedure TOptionStringsTest.TestGetOptionsAndSetFlagResetUseRegex(const _guiOptionsActual : string);
var
	bIsOpOk : Boolean;
	rgExeOps : string;
begin
	var
	sLongParam := RG_PARAM_REGEX_FIXED_STRINGS.Split(['|'])[1];
	rgExeOps := string.Join(' ', RG_NECESSARY_PARAMS) + ' ' + sLongParam;
	FGuiParams.SearchOptions := SetSearchOptions(_guiOptionsActual);
	bIsOpOk := TRegex.IsMatch(rgExeOps, TOptionsHelper.GetBoundedParamRegex(sLongParam));
	FGuiParams.ResetOption(EGuiOption.soUseRegex);
	Assert.IsTrue(bIsOpOk, '''' + sLongParam + ''' should contain initially');
	if (EGuiOption.soUseRegex in FGuiParams.SearchOptions) or (EGuiOption.soMatchWord in FGuiParams.SearchOptions) then begin
		Assert.IsTrue(not FGuiParams.RgOptions.IsOptionSet(RG_PARAM_REGEX_FIXED_STRINGS),
			{ } '''' + sLongParam + ''' should not be in the options');
	end else begin
		Assert.IsTrue(FGuiParams.RgOptions.IsOptionSet(RG_PARAM_REGEX_FIXED_STRINGS),
			{ } '''' + sLongParam + ''' should be in the options');

	end;
	Assert.IsTrue(not FGuiParams.IsSet([EGuiOption.soUseRegex]),
		{ } Integer(EGuiOption.soUseRegex).ToString + ' should not be in the options');

	if EGuiOption.soMatchWord in FGuiParams.SearchOptions then begin
		Assert.IsTrue(TOptionsHelper.IsWordBoundOnBothSide(FGuiParams.SearchText),
			{ } FGuiParams.SearchText + ' should be word bounded');
	end else begin
		Assert.IsTrue(not TOptionsHelper.IsWordBoundOnBothSide(FGuiParams.SearchText),
			{ } FGuiParams.SearchText + ' should not be word bounded');
	end;
end;

procedure TOptionStringsTest.TestGetOptionsAndSetFlagResetMatchWord(const _guiOptionsActual : string);
var
	bIsOpOk : Boolean;
	rgExeOps : string;
begin
	var
	sLongParam := RG_PARAM_REGEX_FIXED_STRINGS.Split(['|'])[1];
	rgExeOps := string.Join(' ', RG_NECESSARY_PARAMS) + ' ' + sLongParam;
	FGuiParams.SearchOptions := SetSearchOptions(_guiOptionsActual);

	bIsOpOk := TRegex.IsMatch(rgExeOps, TOptionsHelper.GetBoundedParamRegex(sLongParam));
	FGuiParams.ResetOption(EGuiOption.soMatchWord);
	Assert.IsTrue(bIsOpOk, '''' + sLongParam + ''' should contain initially');

	if EGuiOption.soUseRegex in FGuiParams.SearchOptions then begin
		Assert.IsTrue(not FGuiParams.RgOptions.IsOptionSet(RG_PARAM_REGEX_FIXED_STRINGS),
			{ } '''' + sLongParam + ''' should not be in the options')
	end else begin
		Assert.IsTrue(FGuiParams.RgOptions.IsOptionSet(RG_PARAM_REGEX_FIXED_STRINGS),
			{ } '''' + sLongParam + ''' should be in the options')

	end;
	Assert.IsTrue(not FGuiParams.IsSet([EGuiOption.soMatchWord]),
		{ } Integer(EGuiOption.soMatchWord).ToString + ' should not be in the options');

	Assert.IsTrue(not TOptionsHelper.IsWordBoundOnBothSide(FGuiParams.SearchText),
		{ } FGuiParams.SearchText + ' should not be bounded');

end;

procedure TOptionStringsTest.TestGetOptionsAndSetFlagMatchWord(const _guiOptionsActual : string);
var
	bIsOpOk : Boolean;
	rgExeOps : string;
begin
	var
	sLongParam := RG_PARAM_REGEX_FIXED_STRINGS.Split(['|'])[1];
	rgExeOps := string.Join(' ', RG_NECESSARY_PARAMS) + ' ' + sLongParam;
	FGuiParams.SearchOptions := SetSearchOptions(_guiOptionsActual);
	bIsOpOk := TRegex.IsMatch(rgExeOps, TOptionsHelper.GetBoundedParamRegex(sLongParam));
	FGuiParams.ResetOption(EGuiOption.soMatchWord);
	Assert.IsTrue(bIsOpOk, '''' + sLongParam + ''' should contain initially');
	if (EGuiOption.soUseRegex in FGuiParams.SearchOptions) or (EGuiOption.soMatchWord in FGuiParams.SearchOptions) then begin
		Assert.IsTrue(not FGuiParams.RgOptions.IsOptionSet(RG_PARAM_REGEX_FIXED_STRINGS),
			{ } '''' + sLongParam + ''' should not be in the options');
	end else begin
		Assert.IsTrue(FGuiParams.RgOptions.IsOptionSet(RG_PARAM_REGEX_FIXED_STRINGS),
			{ } '''' + sLongParam + ''' should be in the options');

	end;
	Assert.IsTrue(not FGuiParams.IsSet([EGuiOption.soMatchWord]),
		{ } Integer(EGuiOption.soMatchWord).ToString + ' should not be in the options');

	if EGuiOption.soMatchWord in FGuiParams.SearchOptions then begin
		Assert.IsTrue(TOptionsHelper.IsWordBoundOnBothSide(FGuiParams.SearchText),
			{ } FGuiParams.SearchText + ' should be word bounded');
	end else begin
		Assert.IsTrue(not TOptionsHelper.IsWordBoundOnBothSide(FGuiParams.SearchText),
			{ } FGuiParams.SearchText + ' should not be word bounded');
	end;

end;

procedure TOptionStringsTest.TestRemoveAllParams(const _sOptions, _sRegEx : string; const _bSwitch : Integer);
begin
	var
	op := TOptionStrings.New(_sOptions);
	op.RemoveOption(_sRegex);
	for var s in op.AsArray do begin
		Assert.IsTrue(TRegEx.IsMatch(s, '^-+\w+'), '''' + s + ''' invalid param (maybe a glob) should not bee in the options array');
		Assert.IsFalse(TRegEx.IsMatch(s, _sRegex), '''' + s + ''' should not bee in the options array');
	end;
end;

procedure TOptionStringsTest.TestIsOptionSetWithValue(const _sOptions, _sParamRegex, _sValue : string; const _bOk : integer);
begin
	if _bOk = 1 then begin
		Assert.IsTrue(TOptionStrings.New(_sOptions).IsOptionSet(_sParamRegex, _sValue),
			{ } '''' + _sParamRegex + '=' + _sValue + ''' should be in the array');
	end else begin
		Assert.IsFalse(TOptionStrings.New(_sOptions).IsOptionSet(_sParamRegex, _sValue),
			{ } '''' + _sParamRegex + '=' + _sValue + ''' should NOT be in the array');
	end;
end;

procedure TOptionStringsTest.TestUpdateOptions(const _sOptions, _sRegEx : string; const _bRemove : Integer);
begin
	var
	op := TOptionStrings.New(_sOptions);
	var
		sOps : string := '';
	if _bRemove = 1 then begin
		op.RemoveOption(_sRegex);
	end else begin
		op.AddOption(_sRegex);
	end;

	for var s : string in op.AsArray do begin
		if _bRemove = 1 then begin
			Assert.AreEqual(_bRemove <> 1, TRegEx.IsMatch(s, _sRegex), '''' + s + ''' should''t be in the options array:' + sOps);
		end else if (s = _sRegex.Split(['|'])[RG_PARAM_LONG_INDEX]) then begin
			Assert.AreEqual(_bRemove <> 1, TRegEx.IsMatch(s, _sRegex), '''' + s + ''' should''t be in the options array:' + sOps);
		end;
	end;
end;

procedure TOptionStringsTest.TestRemoveOptionsWithValue(const _sOptions, _sOptionWithValue : string);
begin
	var
	op := TOptionStrings.New(_sOptions);
	op.RemoveOption(_sOptionWithValue);
	for var s in op.AsArray do begin
		Assert.IsTrue(TRegEx.IsMatch(s, '^-+\w+'), '''' + s + ''' invalid param (maybe a glob) should not bee in the options array');
		Assert.IsTrue(s <> _sOptionWithValue, '''' + _sOptionWithValue + ''' should not bee in the options array');
	end;
end;

procedure TOptionStringsTest.TestAddOptionsWithValue(const _sOptions, _sRegEx, _sValue : string; const _bUnique : integer);
var
	arrOptions : TArrayEx<string>;
	bFound : Boolean;
	sOpWithVal : string;
begin
	bFound := False;

	var
	op := TOptionStrings.New(_sOptions);
	op.AddOptionWithValue(_sRegex, _sValue, _bUnique = 1);
	arrOptions := op.AsArray;
	sOpWithVal := _sRegex.split(['|'])[RG_PARAM_LONG_INDEX] + '=' + TOptionStrings.MaybeQuoteIfNotQuoted(_sValue);
	for var s in arrOptions do begin
		if TRegEx.IsMatch(s, '^' + TRegEx.Escape(sOpWithVal)) then begin
			bFound := True;
		end;
		Assert.IsTrue(TRegEx.IsMatch(s, '^-+\w+'), '''' + s + ''' invalid param (maybe a glob) should not bee in the options array');
	end;
	Assert.IsTrue(bFound, '''' + sOpWithVal + ''' should bee in the options array');
end;

procedure TOptionStringsTest.TestGetOptionVariantsAndValue(const _sParamRegex : string);
var
	idxEq : Integer;
	idxOr : Integer;
	long : string;
	short : string;
	value : string;
begin

	idxOr := _sParamRegex.IndexOf('|');
	idxEq := IfThen(_sParamRegex.IndexOf('=') < 0, _sParamRegex.Length, _sParamRegex.IndexOf('=') - 1);
	short := _sParamRegex.Substring(0, idxOr).Replace('\', '');
	long := _sParamRegex.Substring(idxOr + 1, idxEq - 2);
	value := Ifthen(_sParamRegex.IndexOf('=') < 0, '', _sParamRegex.Substring(idxEq + 2));
	var
		op : TOptionVariants;
	if TOptionStrings.GetOptionVariantsAndValue(_sParamRegex, op) then begin
		Assert.AreEqual(op.Short, short, 'short should be equal');
		Assert.AreEqual(op.Long, long, 'long should be equal');
		Assert.AreEqual(op.Value, value, 'value should be equal');
		Assert.AreEqual(op.HasValue, _sParamRegex.IndexOf('=') >= 0, 'hasvalue should be equal');
	end else begin
		Assert.IsTrue(False, '''' + _sParamRegex + ''' TOptionStrings.GetOptionVariantsAndValue should return true');
	end;
end;

procedure TOptionStringsTest.TestRemoveMultipleOptionsWithValue(const _sOptions : string);
begin
	var
	op := TOptionStrings.New(_sOptions);
	op.RemoveOptions(RG_NECESSARY_PARAMS + RG_GUI_SET_PARAMS);
	Assert.IsTrue(op.IsOptionSet('-x'), '-x should be set');
	Assert.IsTrue(op.IsOptionSet('-YYYY'), '-YYYY should be set');

	for var s in op.AsArray do begin
		Assert.IsTrue(TRegEx.IsMatch(s, '^-+\w+'), '''' + s + ''' invalid param (maybe a glob) should not bee in the options array');

		for var r in RG_NECESSARY_PARAMS + RG_GUI_SET_PARAMS do begin
			if r = RG_PARAM_END then continue;
			Assert.IsFalse(TRegEx.IsMatch(s, r), '''' + s + ''' should not bee in the options array. ''' + r + ''' removed');
		end;
	end;
end;

procedure TOptionStringsTest.TestToArray(const _sOptions, _sRegEx, _sValue : string; const _bUnique : integer);
begin
	var
	op := TOptionStrings.ToArray(_sOptions);
	Assert.IsTrue(op.Count > 0, 'count should bee in the options array');
end;

procedure TOptionStringsTest.TestUpdateFileMasks(const _sOptions, _sNewMasks : string);
begin
	var
	op := TOptionStrings.New(_sOptions);
	op.UpdateFileMasks(_sNewMasks);
	for var s in op.AsArray do begin
		Assert.IsFalse(TRegEx.IsMatch(s, '(\*\.txt|\*\.ini|\*\.bak)'), '''' + s + ''' should not bee in the options array');
	end;

	for var s in _sNewMasks.Split([';']) do begin
		Assert.IsTrue(op.IsOptionSet('-g=' + s), '''' + s + ''' should bee in the options array');
	end;
end;

procedure TOptionStringsTest.TestUpdateSearchText(const _sOptions, _sSearchText : string;
	const _bMatchWord, _bUseRegex, _bShouldBounded, _bShouldEscaped : Integer);
var
	sAct : string;
	p : string;
begin
	FParams.RgExeOptions := TOptionStrings.New(_sOptions);
	FParams.FileMasks := '';
	FGuiParams.SearchText := _sSearchText;
	FGuiParams.SearchOptions := TGuiSearchTextParams.GetAsSearchOptionSet(False, _bMatchWord = 1, _bUseRegex = 1);
	FGuiParams.RgOptions := TOptionStrings.New(_sOptions);

	if (_bMatchWord = 1) then
		FGuiParams.SetOption(EGuiOption.soMatchWord);
	if (_bUseRegex = 1) then
		FGuiParams.SetOption(EGuiOption.soUseRegex);

	sAct := FGuiParams.SearchText;
	if _bShouldBounded = 1 then begin
		Assert.AreEqual(WB + _sSearchText + WB, sAct, 'the search text should surrounded: ' + sAct);
	end else begin
		Assert.AreEqual(_sSearchText, sAct, 'if MatchWord is not set, then search text should equal ' + sAct);
	end;

	if (_bShouldBounded = 1) and not(EGuiOption.soUseRegex in FGuiParams.SearchOptions) then begin
		Assert.IsFalse(FParams.RgExeOptions.IsOptionSet(RG_PARAM_REGEX_FIXED_STRINGS),
			p + ' mustn''t be contained between options while searching ' + sAct)
	end;

	if not(EGuiOption.soUseRegex in FGuiParams.SearchOptions) then begin
		Assert.IsTrue(FParams.RgExeOptions.IsOptionSet(RG_PARAM_REGEX_FIXED_STRINGS),
			p + ' should contained between options while searching ' + sAct)
	end;
end;

initialization

TDUnitX.RegisterTestFixture(TOptionStringsTest);

end.
