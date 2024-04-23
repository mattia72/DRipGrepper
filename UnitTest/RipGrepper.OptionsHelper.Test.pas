unit RipGrepper.OptionsHelper.Test;

interface

uses
	DUnitX.TestFramework,
	System.Classes,
	RipGrepper.Common.Constants,
	RipGrepper.Common.CommandLineBuilder,
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
	end;

implementation

uses
	RipGrepper.Helper.Types,
	System.StrUtils,
	System.SysUtils,
	RipGrepper.Common.Settings,
	ArrayEx,
	System.RegularExpressions,
	System.Math;

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
	sAct := TCommandLineBuilder.UpdateSearchTextAndRgExeOptions(gsp, arrRgOptions);

	if _bShouldBounded = 1 then begin
		Assert.AreEqual(WB + _sSearchText + WB, sAct, 'the search text should surrounded: ' + sAct);
	end else begin
		Assert.AreEqual(_sSearchText, sAct, 'if MatchWord is not set, then search text should equal ' + sAct);
	end;

	if (_bShouldBounded = 1) and not(soUseRegex in gsp.SearchOptions) then begin
		for p in RG_PARAM_REGEX_FIXED_STRINGS.Split(['|']) do begin
			Assert.IsFalse(FParams.RgExeOptions.Contains(p), p + ' mustn''t be contained between options while searching ' + sAct)
		end;
	end;

	if not(soUseRegex in gsp.SearchOptions) then begin
		p := RG_PARAM_REGEX_FIXED_STRINGS.Split(['|'])[1];
		Assert.IsTrue(FParams.RgExeOptions.Contains(p), p + ' should contained between options while searching ' + sAct)
	end;

end;

initialization

TDUnitX.RegisterTestFixture(TOptionsHelperTest);

end.
