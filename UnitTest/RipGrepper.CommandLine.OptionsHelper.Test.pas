unit RipGrepper.CommandLine.OptionsHelper.Test;

interface

uses
	DUnitX.TestFramework,
	System.Classes,
	RipGrepper.Common.Constants,
	RipGrepper.CommandLine.Builder,
	RipGrepper.Settings.RipGrepParameterSettings,
	System.IniFiles,
	RipGrepper.Common.GuiSearchParams;

type

	[TestFixture]
	TOptionsHelperTest = class

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
			[Testcase('test1', '--vimgrep  --fixed-strings --no-ignore;--no-ignore;1', ';')]
			[Testcase('test2', '--vimgrep  --fixeda --no-ignore-parent;--no-ignore;0', ';')]
			[Testcase('test3', '--vimgrep --no-ignore --fixed-stringsa;--no-ignore;1', ';')]
			[Testcase('test4', '--vimgrep --no-ignore-no --fixed-strig;--no-ignore;0', ';')]
			[Testcase('test5', '--vimgrep --no-ignore -- text_to_find;--;1', ';')]
			[Testcase('test6', '--vimgrep --no-ignore-no --fixed-strig;--;0', ';')]
			procedure TestBoundedParamRegex(const _sOptions, _sOption : string; const _bOk : integer);
	end;

implementation

uses
	RipGrepper.Helper.Types,
	System.StrUtils,
	System.SysUtils,
	RipGrepper.Settings.AppSettings,
	ArrayEx,
	System.RegularExpressions,
	System.Math,
	RipGrepper.CommandLine.OptionHelper,
	RipGrepper.CommandLine.OptionStrings;

function TOptionsHelperTest.SetSearchOptions(const _guiOptionsActual : string) : TSearchOptionSet;
begin
	Result := [];
	for var s : string in _guiOptionsActual.Split(['#']) do begin
		Result := Result + [EGuiOption(integer.Parse(s))];
	end;
end;

procedure TOptionsHelperTest.Setup;
begin
	FIniFile := TMemIniFile.Create('DripGrepperUnittest.ini', TEncoding.UTF8);
	FParams := TRipGrepParameterSettings.Create(FIniFile);
	FGuiParams := FParams.GuiSearchTextParams;
	FGuiParams.SearchText := 'search text';
end;

procedure TOptionsHelperTest.TearDown;
begin
	FParams.Free;
	FIniFile.Free;
end;

procedure TOptionsHelperTest.TestGetOptionsAndSetFlag(const _guiOption, _bMatch : Integer; const _paramRegex : string);
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
	var
	sNewOptions := FguiParams.RgOptions;

	Assert.IsTrue(not bIsOpOk, '''' + _paramRegex + ''' should not contains initially');
	Assert.AreEqual(_bMatch = 1, sNewOptions.IsOptionSet(_paramRegex), '''' + _paramRegex + ''' should be in the options');
	Assert.IsTrue(FguiParams.IsSet([newGuiSearchOption]), Integer(newGuiSearchOption).ToString + ' should be in the options');

end;

procedure TOptionsHelperTest.TestGetOptionsAndSetFlagReset(const _guiOption, bMatch : Integer; const _paramRegex : string);
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
	var
	sNewOptions := FGuiParams.RgOptions;

	Assert.IsTrue(bIsOpOk, '''' + _paramRegex + ''' should contain initially');
	Assert.AreEqual(bMatch = 1, sNewOptions.IsOptionSet(_paramRegex), '''' + _paramRegex + ''' should not be in the options');
	Assert.IsTrue(not FGuiParams.IsSet([_resetGuiSearchOption]), Integer(_resetGuiSearchOption).ToString + ' should not be in the options');
end;

procedure TOptionsHelperTest.TestGetOptionsAndSetFlagsoUseRegex(const _guiOptionsActual : string);
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
	var
	sNewOptions := FGuiParams.RgOptions;

	Assert.IsTrue(bIsOpOk, '''' + sLongParam + ''' should contain initially');
	Assert.IsTrue(not sNewOptions.IsOptionSet(RG_PARAM_REGEX_FIXED_STRINGS),
		{ } '''' + sLongParam + ''' should not be in the options');
	Assert.IsTrue(FGuiParams.IsSet([EGuiOption.soUseRegex]),
		{ } Integer(EGuiOption.soUseRegex).ToString + ' should not be in the options');
end;

procedure TOptionsHelperTest.TestGetOptionsAndSetFlagResetUseRegex(const _guiOptionsActual : string);
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
	var
	sNewOptions := FGuiParams.RgOptions;

	Assert.IsTrue(bIsOpOk, '''' + sLongParam + ''' should contain initially');
	if (EGuiOption.soUseRegex in FGuiParams.SearchOptions) or (EGuiOption.soMatchWord in FGuiParams.SearchOptions) then begin
		Assert.IsTrue(not sNewOptions.IsOptionSet(RG_PARAM_REGEX_FIXED_STRINGS),
			{ } '''' + sLongParam + ''' should not be in the options');
	end else begin
		Assert.IsTrue(sNewOptions.IsOptionSet(RG_PARAM_REGEX_FIXED_STRINGS),
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

procedure TOptionsHelperTest.TestGetOptionsAndSetFlagResetMatchWord(const _guiOptionsActual : string);
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
	var
	sNewOptions := FGuiParams.RgOptions;

	Assert.IsTrue(bIsOpOk, '''' + sLongParam + ''' should contain initially');

	if EGuiOption.soUseRegex in FGuiParams.SearchOptions then begin
		Assert.IsTrue(not sNewOptions.IsOptionSet(RG_PARAM_REGEX_FIXED_STRINGS),
			{ } '''' + sLongParam + ''' should not be in the options')
	end else begin
		Assert.IsTrue(sNewOptions.IsOptionSet(RG_PARAM_REGEX_FIXED_STRINGS),
			{ } '''' + sLongParam + ''' should be in the options')

	end;
	Assert.IsTrue(not FGuiParams.IsSet([EGuiOption.soMatchWord]),
		{ } Integer(EGuiOption.soMatchWord).ToString + ' should not be in the options');

	Assert.IsTrue(not TOptionsHelper.IsWordBoundOnBothSide(FGuiParams.SearchText),
		{ } FGuiParams.SearchText + ' should not be bounded');

end;

procedure TOptionsHelperTest.TestGetOptionsAndSetFlagMatchWord(const _guiOptionsActual : string);
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
	var
	sNewOptions := FGuiParams.RgOptions;

	Assert.IsTrue(bIsOpOk, '''' + sLongParam + ''' should contain initially');
	if (EGuiOption.soUseRegex in FGuiParams.SearchOptions) or (EGuiOption.soMatchWord in FGuiParams.SearchOptions) then begin
		Assert.IsTrue(not sNewOptions.IsOptionSet(RG_PARAM_REGEX_FIXED_STRINGS),
			{ } '''' + sLongParam + ''' should not be in the options');
	end else begin
		Assert.IsTrue(sNewOptions.IsOptionSet(RG_PARAM_REGEX_FIXED_STRINGS),
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

procedure TOptionsHelperTest.TestBoundedParamRegex(const _sOptions, _sOption : string; const _bOk : integer);
begin
	var
	bIsOpOk := TRegex.IsMatch(_sOptions, TOptionsHelper.GetBoundedParamRegex(_sOption));
	if _bOk = 1 then begin
		Assert.IsTrue(bIsOpOk, 'Option ' + _sOption + ' should be ok in ' + _sOptions);
	end else begin
		Assert.IsFalse(bIsOpOk, 'Option ' + _sOption + ' shouldn''t be ok in ' + _sOptions);
	end;
end;

initialization

TDUnitX.RegisterTestFixture(TOptionsHelperTest);

end.
