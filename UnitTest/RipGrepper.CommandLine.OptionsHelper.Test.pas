unit RipGrepper.CommandLine.OptionsHelper.Test;

interface

uses
	DUnitX.TestFramework,
	System.Classes,
	RipGrepper.Common.Constants,
	RipGrepper.CommandLine.Builder,
	RipGrepper.Common.Settings.RipGrepParameterSettings,
	System.IniFiles;

type

	[TestFixture]
	TOptionsHelperTest = class

		private
			FIniFile : TIniFile;
			FParams : TRipGrepParameterSettings;

		public
			[Setup]
			procedure Setup;
			[TearDown]
			procedure TearDown;

			[Test]
			[Testcase('test1', '--vimgrep -g=*.txt --fixed-strings -g=*.ini --ignore-case -g=*.bak;' + RG_PARAM_REGEX_IGNORE_CASE, ';')]
			[Testcase('test2', '--vimgrep -g=!*.txt --fixed-strings -g=!*.ini -i         -g=!*.bak;' + RG_PARAM_REGEX_IGNORE_CASE, ';')]
			[Testcase('test not exist', '--vimgrep -g=!*.txt --fixed-strings -g=!*.ini - -g=!*.bak;' + '-nop|--noparam;1', ';')]
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
			[Testcase('test1', '--vimgrep -g=*.txt --fixed-strings -g=*.ini --ignore-case -g=*.bak;' + RG_PARAM_REGEX_GLOB + ';0', ';')]
			[Testcase('test2', '--vimgrep -g=!*.txt --fixed-strings -g=!*.ini -i         -g=!*.bak;' + RG_PARAM_REGEX_FIXED_STRINGS +
				';1', ';')]
			procedure TestRemoveAllParams1(const _sOptions, _sRegEx : string; const _bSwitch : Integer);

			[Test]
			[Testcase('test1', '--vimgrep --fixed-strings -g=*.ini --ignore-case -g=!*.bak;' + RG_PARAM_REGEX_IGNORE_CASE + ';1', ';')]
			[Testcase('test2', '--vimgrep --fixed-strings -g=*.ini -i         -g=!*.bak;' + RG_PARAM_REGEX_IGNORE_CASE + ';1', ';')]
			[Testcase('test3', '--vimgrep --fixed-strings -g=*.ini            -g=!*.bak;' + RG_PARAM_REGEX_FIXED_STRINGS + ';1', ';')]
			[Testcase('test4', '--vimgrep --fixed-strings -g=*.ini            -g=!*.bak;' + RG_PARAM_REGEX_GLOB + ';1', ';')]
			[Testcase('test5', '--vimgrep --fixed-strings -g=*.ini --ignore-case -g=!*.bak;' + RG_PARAM_REGEX_IGNORE_CASE + ';0', ';')]
			[Testcase('test6', '--vimgrep --fixed-strings -g=*.ini -i         -g=!*.bak;' + RG_PARAM_REGEX_IGNORE_CASE + ';0', ';')]
			[Testcase('test7', '--vimgrep --fixed-strings -g=*.ini            -g=!*.bak;' + RG_PARAM_REGEX_IGNORE_CASE + ';0', ';')]
			[Testcase('test8', '--vimgrep -F              -g=*.ini            -g=!*.bak;' + RG_PARAM_REGEX_FIXED_STRINGS + ';0', ';')]
			[Testcase('test9', '--vimgrep                 -g=*.ini            -g=!*.bak;' + RG_PARAM_REGEX_FIXED_STRINGS + ';0', ';')]
			procedure TestUpdateRgExeOptions(const _sOptions, _sRegEx : string; const _bRemove : Boolean);
			[Test]
			[Testcase('test1', '--vimgrep  --fixed-strings -g=*.ini -g=!*.bak;' + RG_PARAM_REGEX_GLOB + ';1', ';')]
			[Testcase('test2', '--vimgrep  -g=*.ini -g=!*.bak --fixed-strings -i;' + RG_PARAM_REGEX_GLOB + ';1', ';')]
			procedure TestUpdateRgExeOptionsWthValue(const _sOptions, _sRegEx : string; const _bRemove : Boolean);
			[Test]
			{ ______________________________________________________________________________W|R|B|E_____ }
			{ } [Testcase('Single word  MW  UR      ', '-p1 --fixed-strings --p2 --|aa1    |1|1|1', '|')]
			{ } [Testcase('Double word  MW  UR      ', '-p1 --fixed-strings --p2 --|aa2 bbb|1|1|1', '|')]
			{ } [Testcase('Single word nMW nUR      ', '-p1 --fixed-strings --p2 --|aa3    |0|0|0', '|')]
			{ } [Testcase('Double word nMW nUR      ', '-p1 --fixed-strings --p2 --|aa4 bbb|0|0|0', '|')]
			{ } [Testcase('Double word nMW nUR      ', '-p1 --fixed-strings --p2 --|a*4 b$b|0|0|0|1', '|')]
			{ } [Testcase('Single word  MW nUR      ', '-p1                 --p2 --|aa5    |0|1|0', '|')]
			{ } [Testcase('Double word  MW nUR      ', '-p1                 --p2 --|aa6 bbb|0|1|0', '|')]
			procedure TestUpdateSearchText(const _sOptions, _sSearchText : string;
				const _bMatchWord, _bUseRegex, _bShouldBounded, _bShouldEscaped : Integer);

			[Test]
			[Testcase('test soMatchCase', '0;' + RG_PARAM_REGEX_CASE_SENSITIVE, ';')]
			[Testcase('test soMatchWord', '1;' + RG_PARAM_REGEX_FIXED_STRINGS, ';')]
			[Testcase('test soUseRegex ', '2;' + RG_PARAM_REGEX_FIXED_STRINGS, ';')]
			procedure TestGetOptionsAndSetFlag(const _guiOption : Integer; const _paramRegex : string);
			[Test]
			[Testcase('test soMatchCase', '0;' + RG_PARAM_REGEX_CASE_SENSITIVE, ';')]
			[Testcase('test soMatchWord', '1;' + RG_PARAM_REGEX_FIXED_STRINGS, ';')]
			[Testcase('test soUseRegex ', '2;' + RG_PARAM_REGEX_FIXED_STRINGS, ';')]
			procedure TestGetOptionsAndSetFlagReset(const _guiOption : Integer; const _paramRegex : string);
			[Test]
			[Testcase('test ', '')]
			[Testcase('test soMatchCase', '0')]
			[Testcase('test soMatchWord', '1')]
			[Testcase('test soUseRegex ', '2')]
			[Testcase('test soMatchCasesoMatchWord', '0#1')]
			[Testcase('test soMatchCasesoUseRegex', '0#2')]
			[Testcase('test soMatchWordsoUseRegex', '1#2')]
			[Testcase('test soAll', '0#1#2')]
			procedure TestGetOptionsAndSetFlagsoUseRegex(const _guiOptionsActual : string);
			[Test]
			[Testcase('test ', '')]
			[Testcase('test soMatchCase', '0')]
			[Testcase('test soMatchWord', '1')]
			[Testcase('test soUseRegex ', '2')]
			[Testcase('test soMatchCasesoMatchWord', '0#1')]
			[Testcase('test soMatchCasesoUseRegex', '0#2')]
			[Testcase('test soMatchWordsoUseRegex', '1#2')]
			[Testcase('test soAll', '0#1#2')]
			procedure TestGetOptionsAndSetFlagResetUseRegex(const _guiOptionsActual : string);
			[Test]
			[Testcase('test ', '')]
			[Testcase('test soMatchCase', '0')]
			[Testcase('test soMatchWord', '1')]
			[Testcase('test soUseRegex ', '2')]
			[Testcase('test soMatchWord', '0#1')]
			[Testcase('test soUseRegex ', '0#2')]
			[Testcase('test soMatchWord', '1#2')]
			[Testcase('test soMatchWord', '0#1#2')]
			procedure TestGetOptionsAndSetFlagResetMatchWord(const _guiOptionsActual : string);
			[Test]
			[Testcase('test ', '')]
			[Testcase('test soMatchCase', '0')]
			[Testcase('test soMatchWord', '1')]
			[Testcase('test soUseRegex ', '2')]
			[Testcase('test soMatchWord', '0#1')]
			[Testcase('test soUseRegex ', '0#2')]
			[Testcase('test soMatchWord', '1#2')]
			[Testcase('test soMatchWord', '0#1#2')]
			procedure TestGetOptionsAndSetFlagMatchWord(const _guiOptionsActual : string);
	end;

implementation

uses
	RipGrepper.Helper.Types,
	System.StrUtils,
	System.SysUtils,
	RipGrepper.Common.Settings,
	ArrayEx,
	System.RegularExpressions,
	System.Math, RipGrepper.CommandLine.OptionHelper,
  RipGrepper.Common.GuiSearchParams;

procedure TOptionsHelperTest.Setup;
begin
	FIniFile := TIniFile.Create('DripGrepperUnittest.ini');
	FParams := TRipGrepParameterSettings.Create(FIniFile);
end;

procedure TOptionsHelperTest.TearDown;
begin
	FParams.Free;
	FIniFile.Free;
end;

procedure TOptionsHelperTest.TestIsOptionSet(const _sOptions, _sParamRegex : string; _bNotSet : Boolean = False);
begin
	if _bNotSet then begin
		Assert.IsFalse(TOptionsHelper.IsOptionSet(_sOptions, _sParamRegex), '''' + _sParamRegex + ''' should NOT be in the array');
	end else begin
		Assert.IsTrue(TOptionsHelper.IsOptionSet(_sOptions, _sParamRegex), '''' + _sParamRegex + ''' should be in the array');
	end;
end;

procedure TOptionsHelperTest.TestGetOptionsAndSetFlag(const _guiOption : Integer; const _paramRegex : string);
var
	_newGuiSearchOption : EGuiOption;
	_bIsOpOk : Boolean;
	_rgExeOps : string;
	_guiParams : TGuiSetSearchParams;

begin
	_newGuiSearchOption := EGuiOption(_guiOption);
	_guiParams.SearchText := 'search_text';

	_rgExeOps := string.Join(' ', RG_NECESSARY_PARAMS);
	_guiParams.SearchOptions := [];
	_bIsOpOk := TRegex.IsMatch(_rgExeOps, TOptionsHelper.GetBoundedParamRegex(_paramRegex.Split(['|'])[1]));

	var
	sNewOptions := TOptionsHelper.SetFlagAndOption(
		{ } _newGuiSearchOption, _paramRegex, _rgExeOps, _guiParams);

	Assert.IsTrue(not _bIsOpOk, '''' + _paramRegex + ''' should not contains initially');
	Assert.IsTrue(TOptionsHelper.IsOptionSet(sNewOptions, _paramRegex), '''' + _paramRegex + ''' should be in the options');
	Assert.IsTrue(_guiParams.IsSet([_newGuiSearchOption]), Integer(_newGuiSearchOption).ToString + ' should be in the options');
end;

procedure TOptionsHelperTest.TestGetOptionsAndSetFlagReset(const _guiOption : Integer; const _paramRegex : string);
var
	_resetGuiSearchOption : EGuiOption;
	_bIsOpOk : Boolean;
	_rgExeOps : string;
	_guiParams : TGuiSetSearchParams;

begin
	_resetGuiSearchOption := EGuiOption(_guiOption);
	_guiParams.SearchText := 'search_text';
	var
	sLongParam := _paramRegex.Split(['|'])[1];

	_rgExeOps := string.Join(' ', RG_NECESSARY_PARAMS) + ' ' + sLongParam;
	_guiParams.SearchOptions := [_resetGuiSearchOption];

	_bIsOpOk := TRegex.IsMatch(_rgExeOps, TOptionsHelper.GetBoundedParamRegex(sLongParam));

	var
	sNewOptions := TOptionsHelper.SetFlagAndOption(
		{ } _resetGuiSearchOption, _paramRegex, _rgExeOps, _guiParams, True);

	Assert.IsTrue(_bIsOpOk, '''' + _paramRegex + ''' should contain initially');
	Assert.IsTrue(not TOptionsHelper.IsOptionSet(sNewOptions, _paramRegex), '''' + _paramRegex + ''' should not be in the options');
	Assert.IsTrue(not _guiParams.IsSet([_resetGuiSearchOption]), Integer(_resetGuiSearchOption).ToString + ' should not be in the options');
end;

procedure TOptionsHelperTest.TestGetOptionsAndSetFlagsoUseRegex(const _guiOptionsActual : string);
var
	_bIsOpOk : Boolean;
	_rgExeOps : string;
	_guiParams : TGuiSetSearchParams;

begin
	_guiParams.SearchText := 'search_text';
	var
	sLongParam := RG_PARAM_REGEX_FIXED_STRINGS.Split(['|'])[1];

	_rgExeOps := string.Join(' ', RG_NECESSARY_PARAMS) + ' ' + sLongParam;

	for var s : string in _guiOptionsActual.Split(['#']) do begin
		_guiParams.SearchOptions := _guiParams.SearchOptions + [EGuiOption(integer.Parse(s))];
	end;

	_bIsOpOk := TRegex.IsMatch(_rgExeOps, TOptionsHelper.GetBoundedParamRegex(sLongParam));

	var
	sNewOptions := TOptionsHelper.SetFlagAndResetOption(
		{ } EGuiOption.soUseRegex, RG_PARAM_REGEX_FIXED_STRINGS, _rgExeOps, _guiParams);

	Assert.IsTrue(_bIsOpOk, '''' + sLongParam + ''' should contain initially');
	Assert.IsTrue(not TOptionsHelper.IsOptionSet(sNewOptions, RG_PARAM_REGEX_FIXED_STRINGS),
		{ } '''' + sLongParam + ''' should not be in the options');
	Assert.IsTrue(_guiParams.IsSet([EGuiOption.soUseRegex]),
		{ } Integer(EGuiOption.soUseRegex).ToString + ' should not be in the options');
end;

procedure TOptionsHelperTest.TestGetOptionsAndSetFlagResetUseRegex(const _guiOptionsActual : string);
var
	_bIsOpOk : Boolean;
	_rgExeOps : string;
	_guiParams : TGuiSetSearchParams;

begin
	_guiParams.SearchText := 'search_text';
	var
	sLongParam := RG_PARAM_REGEX_FIXED_STRINGS.Split(['|'])[1];

	_rgExeOps := string.Join(' ', RG_NECESSARY_PARAMS) + ' ' + sLongParam;

	for var s : string in _guiOptionsActual.Split(['#']) do begin
		_guiParams.SearchOptions := _guiParams.SearchOptions + [EGuiOption(integer.Parse(s))];
	end;

	_bIsOpOk := TRegex.IsMatch(_rgExeOps, TOptionsHelper.GetBoundedParamRegex(sLongParam));

	var
	sNewOptions := TOptionsHelper.SetFlagAndResetOption(
		{ } EGuiOption.soUseRegex, RG_PARAM_REGEX_FIXED_STRINGS, _rgExeOps, _guiParams, False);

	Assert.IsTrue(_bIsOpOk, '''' + sLongParam + ''' should contain initially');
	Assert.IsTrue(not TOptionsHelper.IsOptionSet(sNewOptions, RG_PARAM_REGEX_FIXED_STRINGS),
		{ } '''' + sLongParam + ''' should not be in the options');
	Assert.IsTrue(not _guiParams.IsSet([EGuiOption.soUseRegex]),
		{ } Integer(EGuiOption.soUseRegex).ToString + ' should not be in the options');

	Assert.IsTrue(not TOptionsHelper.IsWordBoundOnBothSide(_guiParams.SearchText),
		{ } _guiParams.SearchText + ' should not be in the options');

end;

procedure TOptionsHelperTest.TestGetOptionsAndSetFlagResetMatchWord(const _guiOptionsActual : string);
var
	_bIsOpOk : Boolean;
	_rgExeOps : string;
	_guiParams : TGuiSetSearchParams;

begin
	_guiParams.SearchText := 'search_text';
	var
	sLongParam := RG_PARAM_REGEX_FIXED_STRINGS.Split(['|'])[1];

	_rgExeOps := string.Join(' ', RG_NECESSARY_PARAMS) + ' ' + sLongParam;

	for var s : string in _guiOptionsActual.Split(['#']) do begin
		_guiParams.SearchOptions := _guiParams.SearchOptions + [EGuiOption(integer.Parse(s))];
	end;

	_bIsOpOk := TRegex.IsMatch(_rgExeOps, TOptionsHelper.GetBoundedParamRegex(sLongParam));

	var
	sNewOptions := TOptionsHelper.SetFlagAndResetOption(
		{ } EGuiOption.soMatchWord, RG_PARAM_REGEX_FIXED_STRINGS, _rgExeOps, _guiParams, False);

	Assert.IsTrue(_bIsOpOk, '''' + sLongParam + ''' should contain initially');
	Assert.IsTrue(TOptionsHelper.IsOptionSet(sNewOptions, RG_PARAM_REGEX_FIXED_STRINGS),
		{ } '''' + sLongParam + ''' should be in the options');
	Assert.IsTrue(not _guiParams.IsSet([EGuiOption.soMatchWord]),
		{ } Integer(EGuiOption.soMatchWord).ToString + ' should not be in the options');

	Assert.IsTrue(not TOptionsHelper.IsWordBoundOnBothSide(_guiParams.SearchText),
		{ } _guiParams.SearchText + ' should not be in the options');

end;

procedure TOptionsHelperTest.TestGetOptionsAndSetFlagMatchWord(const _guiOptionsActual : string);
var
	_bIsOpOk : Boolean;
	_rgExeOps : string;
	_guiParams : TGuiSetSearchParams;

begin
	_guiParams.SearchText := 'search_text';
	var
	sLongParam := RG_PARAM_REGEX_FIXED_STRINGS.Split(['|'])[1];

	_rgExeOps := string.Join(' ', RG_NECESSARY_PARAMS) + ' ' + sLongParam;

	for var s : string in _guiOptionsActual.Split(['#']) do begin
		_guiParams.SearchOptions := _guiParams.SearchOptions + [EGuiOption(integer.Parse(s))];
	end;

	_bIsOpOk := TRegex.IsMatch(_rgExeOps, TOptionsHelper.GetBoundedParamRegex(sLongParam));

	var
	sNewOptions := TOptionsHelper.SetFlagAndResetOption(
		{ } EGuiOption.soMatchWord, RG_PARAM_REGEX_FIXED_STRINGS, _rgExeOps, _guiParams);

	Assert.IsTrue(_bIsOpOk, '''' + sLongParam + ''' should contain initially');
	Assert.IsTrue(not TOptionsHelper.IsOptionSet(sNewOptions, RG_PARAM_REGEX_FIXED_STRINGS),
		{ } '''' + sLongParam + ''' should not be in the options');
	Assert.IsTrue(_guiParams.IsSet([EGuiOption.soMatchWord]),
		{ } Integer(EGuiOption.soMatchWord).ToString + ' should not be in the options');


	Assert.IsTrue(TOptionsHelper.IsWordBoundOnBothSide(_guiParams.SearchText),
		{ } _guiParams.SearchText + ' should not be in the options');

end;

procedure TOptionsHelperTest.TestRemoveAllParams(const _sOptions, _sRegEx : string; const _bSwitch : Integer);
var
	arrOptions : TArrayEx<string>;
begin
	var
	s := TOptionsHelper.RemoveAllParams(_sOptions, _sRegex, (_bSwitch = 1));
	arrOptions := s.Split([' ']);
	for s in arrOptions do begin
		Assert.IsFalse(TRegEx.IsMatch(s, _sRegex), '''' + s + ''' should not bee in the options array');
	end;
end;

procedure TOptionsHelperTest.TestIsOptionSetWithValue(const _sOptions, _sParamRegex, _sValue : string; const _bOk : integer);
begin
	if _bOk = 1 then begin
		Assert.IsTrue(TOptionsHelper.IsOptionSet(_sOptions, _sParamRegex, _sValue),
			{ } '''' + _sParamRegex + '=' + _sValue + ''' should be in the array');
	end else begin
		Assert.IsFalse(TOptionsHelper.IsOptionSet(_sOptions, _sParamRegex, _sValue),
			{ } '''' + _sParamRegex + '=' + _sValue + ''' should NOT be in the array');
	end;
end;

procedure TOptionsHelperTest.TestRemoveAllParams1(const _sOptions, _sRegEx : string; const _bSwitch : Integer);
var
	arrOptions : TArrayEx<string>;
begin
	var
	s := TOptionsHelper.RemoveAllParams(_sOptions, _sRegex, (_bSwitch = 1));
	arrOptions := s.Split([' ']);
	for s in arrOptions do begin
		Assert.IsFalse(TRegEx.IsMatch(s, _sRegex), '''' + s + ''' should not bee in the options array');
	end;
end;

procedure TOptionsHelperTest.TestUpdateRgExeOptions(const _sOptions, _sRegEx : string; const _bRemove : Boolean);
var
	arrOptions : TArrayEx<string>;
begin
	var
	s := TOptionsHelper.AddRemoveRgExeOptions(_sOptions, _sRegex, _bRemove); // Remove
	arrOptions := s.Split([' ']);
	for s in arrOptions do begin
		Assert.IsFalse(TRegEx.IsMatch(s, _sRegex), '''' + s + ''' should not bee in the options array');
	end;
end;

procedure TOptionsHelperTest.TestUpdateRgExeOptionsWthValue(const _sOptions, _sRegEx : string; const _bRemove : Boolean);
var
	arrOptions : TArrayEx<string>;
begin
	var
	sArgs := TOptionsHelper.AddRemoveRgExeOptions(_sOptions, _sRegex, _bRemove); // Remove
	arrOptions := sArgs.Split([' '], TStringSplitOptions.ExcludeEmpty);
	for var s in arrOptions do begin
		Assert.IsTrue(TRegEx.IsMatch(s, '^-+\w+'), '''' + s + ''' invalid param (maybe a glob) should not bee in the options array');
		Assert.IsFalse(TRegEx.IsMatch(s, _sRegex), '''' + s + ''' should not bee in the options array');
	end;
end;

procedure TOptionsHelperTest.TestUpdateSearchText(const _sOptions, _sSearchText : string;
	const _bMatchWord, _bUseRegex, _bShouldBounded, _bShouldEscaped : Integer);
var
	arrRgOptions : TArrayEx<string>;
	sAct : string;
	p : string;
	gsp : TGuiSetSearchParams;
begin
	FParams.RgExeOptions := _sOptions;
	FParams.FileMasks := '';

	gsp := TGuiSetSearchParams.New(_sSearchText, False, _bMatchWord = 1, _bUseRegex = 1);
	FParams.GuiSetSearchParams := gsp;

	arrRgOptions := FParams.RgExeOptions.Split([' ']);
	TCommandLineBuilder.UpdateSearchTextAndRgExeOptions(gsp, arrRgOptions);
	sAct := gsp.SearchText;
	if _bShouldBounded = 1 then begin
		Assert.AreEqual(WB + _sSearchText + WB, sAct, 'the search text should surrounded: ' + sAct);
	end else begin
		Assert.AreEqual(_sSearchText, sAct, 'if MatchWord is not set, then search text should equal ' + sAct);
	end;

	if (_bShouldBounded = 1) and not(EGuiOption.soUseRegex in gsp.SearchOptions) then begin
		for p in RG_PARAM_REGEX_FIXED_STRINGS.Split(['|']) do begin
			Assert.IsFalse(FParams.RgExeOptions.Contains(p), p + ' mustn''t be contained between options while searching ' + sAct)
		end;
	end;

	if not(EGuiOption.soUseRegex in gsp.SearchOptions) then begin
		p := RG_PARAM_REGEX_FIXED_STRINGS.Split(['|'])[1];
		Assert.IsTrue(FParams.RgExeOptions.Contains(p), p + ' should contained between options while searching ' + sAct)
	end;

end;

initialization

TDUnitX.RegisterTestFixture(TOptionsHelperTest);

end.
