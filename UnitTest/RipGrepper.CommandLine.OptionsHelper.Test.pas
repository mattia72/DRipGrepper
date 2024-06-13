unit RipGrepper.CommandLine.OptionsHelper.Test;

interface

uses
  DUnitX.TestFramework,
  System.Classes,
  RipGrepper.Common.Constants,
  RipGrepper.CommandLine.Builder,
  RipGrepper.Common.Settings.RipGrepParameterSettings,
  System.IniFiles,
  RipGrepper.Common.GuiSearchParams;

type

  [TestFixture]
  TOptionsHelperTest = class

    private
      FIniFile : TIniFile;
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
      [Testcase('test not exist', '--vimgrep -g=!*.txt --fixed-strings -g=!*.ini - -g=!*.bak;' + '-nop|--noparam;1', ';')]
      procedure TestIsOptionSet(const _sOptions, _sParamRegex : string; _bNotSet : Boolean = False);

      [Test]
      [Testcase('test1', '--vimgrep -g=*.txt --fixed-strings -g=*.ini --ignore-case -g=*.bak;' + RG_PARAM_REGEX_GLOB + ';0', ';')]
      [Testcase('test2', '--vimgrep -g=!*.txt --fixed-strings -g=!*.ini -i         -g=!*.bak;' + RG_PARAM_REGEX_FIXED_STRINGS + ';1', ';')]
      procedure TestRemoveAllParams(const _sOptions, _sRegEx : string; const _bSwitch : Integer);

      [Test]
      [Testcase('test1', '--vimgrep -g=!*.txt -F -g=*.ini -i -g=!*.bak;' + RG_PARAM_REGEX_GLOB + ';*.ini;1', ';')]
      [Testcase('test2', '--vimgrep -g=!*.txt -F -g=!*.ini -i -g=!*.bak;' + RG_PARAM_REGEX_GLOB + ';*.ini;0', ';')]
      [Testcase('test3', '--vimgrep -g=!*.txt -F -g=!*.ini -i -g=!*.bak;' + RG_PARAM_REGEX_GLOB + ';*.iii;0', ';')]
      procedure TestIsOptionSetWithValue(const _sOptions, _sParamRegex, _sValue : string; const _bOk : integer);
      [Test]
      [Testcase('test1', '--vimgrep -g=*.txt --fixed-strings -g=*.ini --ignore-case -g=*.bak;' + RG_PARAM_REGEX_GLOB + ';0', ';')]
      [Testcase('test2', '--vimgrep -g=!*.txt --fixed-strings -g=!*.ini -i         -g=!*.bak;' + RG_PARAM_REGEX_FIXED_STRINGS + ';1', ';')]
      procedure TestRemoveAllParams1(const _sOptions, _sRegEx : string; const _bSwitch : Integer);

      [Test]
      [Testcase('test1', '--vimgrep --fixed-strings -g=*.ini --ignore-case -g=!*.bak;' + RG_PARAM_REGEX_IGNORE_CASE + ';1', ';')]
      [Testcase('test2', '--vimgrep --fixed-strings -g=*.ini -i         -g=!*.bak;' + RG_PARAM_REGEX_IGNORE_CASE + ';1', ';')]
      [Testcase('test3', '--vimgrep --fixed-strings -g=*.ini            -g=!*.bak;' + RG_PARAM_REGEX_FIXED_STRINGS + ';1', ';')]
      [Testcase('test4', '--vimgrep --fixed-strings -g=*.ini            -g=!*.bak;' + RG_PARAM_REGEX_GLOB + ';1', ';')]
      [Testcase('test5', '--vimgrep --fixed-strings -g=*.ini --ignore-case -g=!*.bak;' + RG_PARAM_REGEX_IGNORE_CASE + ';1', ';')]
      [Testcase('test6', '--vimgrep --fixed-strings -g=*.ini -i         -g=!*.bak;' + RG_PARAM_REGEX_IGNORE_CASE + ';1', ';')]
      [Testcase('test7', '--vimgrep --fixed-strings -g=*.ini            -g=!*.bak;' + RG_PARAM_REGEX_IGNORE_CASE + ';0', ';')]
      [Testcase('test8', '--vimgrep -F              -g=*.ini            -g=!*.bak;' + RG_PARAM_REGEX_FIXED_STRINGS + ';1', ';')]
      [Testcase('test9', '--vimgrep                 -g=*.ini            -g=!*.bak;' + RG_PARAM_REGEX_FIXED_STRINGS + ';0', ';')]
      procedure TestUpdateRgExeOptions(const _sOptions, _sRegEx : string; const _bRemove : Integer);
      [Test]
      [Testcase('test1', '--vimgrep  --fixed-strings -g=*.ini -g=!*.bak;' + RG_PARAM_REGEX_GLOB + ';1', ';')]
      [Testcase('test2', '--vimgrep  -g=*.ini -g=!*.bak --fixed-strings -i;' + RG_PARAM_REGEX_GLOB + ';1', ';')]
      procedure TestRemoveRgExeOptionsWthValue(const _sOptions, _sRegEx : string);
      [Test]
      { ________________________________________________________________________W|R|B|E_____ }
      { } [Testcase('Single word  MW  UR', '-p1 --fixed-strings --p2 --|aa1    |1|1|1', '|')]
      { } [Testcase('Double word  MW  UR', '-p1 --fixed-strings --p2 --|aa2 bbb|1|1|1', '|')]
      { } [Testcase('Single word nMW nUR', '-p1 --fixed-strings --p2 --|aa3    |0|0|0', '|')]
      { } [Testcase('Double word nMW nUR', '-p1 --fixed-strings --p2 --|aa4 bbb|0|0|0', '|')]
      { } [Testcase('Double word nMW nUR', '-p1 --fixed-strings --p2 --|a*5 b$b|0|0|0|1', '|')]
      { } [Testcase('Single word  MW nUR', '-p1                 --p2 --|aa6    |0|1|0', '|')]
      { } [Testcase('Double word  MW nUR', '-p1                 --p2 --|aa7 bbb|0|1|0', '|')]
      procedure TestUpdateSearchText(const _sOptions, _sSearchText : string; const _bMatchWord, _bUseRegex, _bShouldBounded,
          _bShouldEscaped : Integer);

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
      procedure TestAddRgExeOptionsWthValue(const _sOptions, _sRegEx, _sValue : string; const _bUnique : integer);
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
  RipGrepper.Common.Settings,
  ArrayEx,
  System.RegularExpressions,
  System.Math,
  RipGrepper.CommandLine.OptionHelper;

function TOptionsHelperTest.SetSearchOptions(const _guiOptionsActual : string) : TSearchOptionSet;
begin
  Result := [];
  for var s : string in _guiOptionsActual.Split(['#']) do begin
    Result := Result + [EGuiOption(integer.Parse(s))];
  end;
end;

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

procedure TOptionsHelperTest.TestGetOptionsAndSetFlag(const _guiOption, _bMatch : Integer; const _paramRegex : string);
var
  newGuiSearchOption : EGuiOption;
  bIsOpOk : Boolean;
  rgExeOps : string;
  guiParams : TGuiSearchTextParams;
begin
  newGuiSearchOption := EGuiOption(_guiOption);
  guiParams.SearchText := 'search_text';

  rgExeOps := string.Join(' ', RG_NECESSARY_PARAMS);
  guiParams.SearchOptions := [];
  bIsOpOk := TRegex.IsMatch(rgExeOps, TOptionsHelper.GetBoundedParamRegex(_paramRegex.Split(['|'])[1]));

  guiParams.SetOption(newGuiSearchOption);
  var
  sNewOptions := guiParams.RgOptions;

  Assert.IsTrue(not bIsOpOk, '''' + _paramRegex + ''' should not contains initially');
  Assert.AreEqual(_bMatch = 1, TOptionsHelper.IsOptionSet(sNewOptions, _paramRegex), '''' + _paramRegex + ''' should be in the options');
  Assert.IsTrue(guiParams.IsSet([newGuiSearchOption]), Integer(newGuiSearchOption).ToString + ' should be in the options');
end;

procedure TOptionsHelperTest.TestGetOptionsAndSetFlagReset(const _guiOption, bMatch : Integer; const _paramRegex : string);
var
  _resetGuiSearchOption : EGuiOption;
  bIsOpOk : Boolean;
  rgExeOps : string;
  guiParams : TGuiSearchTextParams;

begin
  _resetGuiSearchOption := EGuiOption(_guiOption);
  guiParams.SearchText := 'search_text';
  var
  sLongParam := _paramRegex.Split(['|'])[1];

  rgExeOps := string.Join(' ', RG_NECESSARY_PARAMS) + ' ' + sLongParam;
  guiParams.SearchOptions := [_resetGuiSearchOption];

  bIsOpOk := TRegex.IsMatch(rgExeOps, TOptionsHelper.GetBoundedParamRegex(sLongParam));

  guiParams.ReSetOption(_resetGuiSearchOption);
  var
  sNewOptions := guiParams.RgOptions;

  Assert.IsTrue(bIsOpOk, '''' + _paramRegex + ''' should contain initially');
  Assert.AreEqual(bMatch = 1, TOptionsHelper.IsOptionSet(sNewOptions, _paramRegex), '''' + _paramRegex + ''' should not be in the options');
  Assert.IsTrue(not guiParams.IsSet([_resetGuiSearchOption]), Integer(_resetGuiSearchOption).ToString + ' should not be in the options');
end;

procedure TOptionsHelperTest.TestGetOptionsAndSetFlagsoUseRegex(const _guiOptionsActual : string);
var
  bIsOpOk : Boolean;
  rgExeOps : string;
  guiParams : TGuiSearchTextParams;

begin
  guiParams.SearchText := 'search_text';
  var
  sLongParam := RG_PARAM_REGEX_FIXED_STRINGS.Split(['|'])[1];

  rgExeOps := string.Join(' ', RG_NECESSARY_PARAMS) + ' ' + sLongParam;

  guiParams.SearchOptions := SetSearchOptions(_guiOptionsActual);

  bIsOpOk := TRegex.IsMatch(rgExeOps, TOptionsHelper.GetBoundedParamRegex(sLongParam));

  guiParams.SetOption(EGuiOption.soUseRegex);
  var
  sNewOptions := guiParams.RgOptions;

  Assert.IsTrue(bIsOpOk, '''' + sLongParam + ''' should contain initially');
  Assert.IsTrue(not TOptionsHelper.IsOptionSet(sNewOptions, RG_PARAM_REGEX_FIXED_STRINGS),
      { } '''' + sLongParam + ''' should not be in the options');
  Assert.IsTrue(guiParams.IsSet([EGuiOption.soUseRegex]),
      { } Integer(EGuiOption.soUseRegex).ToString + ' should not be in the options');
end;

procedure TOptionsHelperTest.TestGetOptionsAndSetFlagResetUseRegex(const _guiOptionsActual : string);
var
  bIsOpOk : Boolean;
  rgExeOps : string;
  guiParams : TGuiSearchTextParams;
begin
  guiParams.SearchText := 'search_text';
  var
  sLongParam := RG_PARAM_REGEX_FIXED_STRINGS.Split(['|'])[1];

  rgExeOps := string.Join(' ', RG_NECESSARY_PARAMS) + ' ' + sLongParam;

  guiParams.SearchOptions := SetSearchOptions(_guiOptionsActual);

  bIsOpOk := TRegex.IsMatch(rgExeOps, TOptionsHelper.GetBoundedParamRegex(sLongParam));

  guiParams.ResetOption(EGuiOption.soUseRegex);
  var
  sNewOptions := guiParams.RgOptions;

  Assert.IsTrue(bIsOpOk, '''' + sLongParam + ''' should contain initially');
  if (EGuiOption.soUseRegex in guiParams.SearchOptions) or (EGuiOption.soMatchWord in guiParams.SearchOptions) then begin
    Assert.IsTrue(not TOptionsHelper.IsOptionSet(sNewOptions, RG_PARAM_REGEX_FIXED_STRINGS),
        { } '''' + sLongParam + ''' should not be in the options');
  end else begin
    Assert.IsTrue(TOptionsHelper.IsOptionSet(sNewOptions, RG_PARAM_REGEX_FIXED_STRINGS),
        { } '''' + sLongParam + ''' should be in the options');

  end;
  Assert.IsTrue(not guiParams.IsSet([EGuiOption.soUseRegex]),
      { } Integer(EGuiOption.soUseRegex).ToString + ' should not be in the options');

  if EGuiOption.soMatchWord in guiParams.SearchOptions then begin
    Assert.IsTrue(TOptionsHelper.IsWordBoundOnBothSide(guiParams.SearchText),
        { } guiParams.SearchText + ' should be word bounded');
  end else begin
    Assert.IsTrue(not TOptionsHelper.IsWordBoundOnBothSide(guiParams.SearchText),
        { } guiParams.SearchText + ' should not be word bounded');
  end;

end;

procedure TOptionsHelperTest.TestGetOptionsAndSetFlagResetMatchWord(const _guiOptionsActual : string);
var
  bIsOpOk : Boolean;
  rgExeOps : string;
  guiParams : TGuiSearchTextParams;

begin
  guiParams.SearchText := 'search_text';
  var
  sLongParam := RG_PARAM_REGEX_FIXED_STRINGS.Split(['|'])[1];

  rgExeOps := string.Join(' ', RG_NECESSARY_PARAMS) + ' ' + sLongParam;

  guiParams.SearchOptions := SetSearchOptions(_guiOptionsActual);

  bIsOpOk := TRegex.IsMatch(rgExeOps, TOptionsHelper.GetBoundedParamRegex(sLongParam));
  guiParams.ResetOption(EGuiOption.soMatchWord);
  var
  sNewOptions := guiParams.RgOptions;

  Assert.IsTrue(bIsOpOk, '''' + sLongParam + ''' should contain initially');

  if EGuiOption.soUseRegex in guiParams.SearchOptions then begin
    Assert.IsTrue(not TOptionsHelper.IsOptionSet(sNewOptions, RG_PARAM_REGEX_FIXED_STRINGS),
        { } '''' + sLongParam + ''' should not be in the options')
  end else begin
    Assert.IsTrue(TOptionsHelper.IsOptionSet(sNewOptions, RG_PARAM_REGEX_FIXED_STRINGS),
        { } '''' + sLongParam + ''' should be in the options')

  end;
  Assert.IsTrue(not guiParams.IsSet([EGuiOption.soMatchWord]),
      { } Integer(EGuiOption.soMatchWord).ToString + ' should not be in the options');

  Assert.IsTrue(not TOptionsHelper.IsWordBoundOnBothSide(guiParams.SearchText),
      { } guiParams.SearchText + ' should not be bounded');

end;

procedure TOptionsHelperTest.TestGetOptionsAndSetFlagMatchWord(const _guiOptionsActual : string);
var
  bIsOpOk : Boolean;
  rgExeOps : string;
  guiParams : TGuiSearchTextParams;

begin
  guiParams.SearchText := 'search_text';
  var
  sLongParam := RG_PARAM_REGEX_FIXED_STRINGS.Split(['|'])[1];

  rgExeOps := string.Join(' ', RG_NECESSARY_PARAMS) + ' ' + sLongParam;

  guiParams.SearchOptions := SetSearchOptions(_guiOptionsActual);

  bIsOpOk := TRegex.IsMatch(rgExeOps, TOptionsHelper.GetBoundedParamRegex(sLongParam));

  guiParams.ResetOption(EGuiOption.soMatchWord);
  var
  sNewOptions := guiParams.RgOptions;

  Assert.IsTrue(bIsOpOk, '''' + sLongParam + ''' should contain initially');
  if (EGuiOption.soUseRegex in guiParams.SearchOptions) or (EGuiOption.soMatchWord in guiParams.SearchOptions) then begin
    Assert.IsTrue(not TOptionsHelper.IsOptionSet(sNewOptions, RG_PARAM_REGEX_FIXED_STRINGS),
        { } '''' + sLongParam + ''' should not be in the options');
  end else begin
    Assert.IsTrue(TOptionsHelper.IsOptionSet(sNewOptions, RG_PARAM_REGEX_FIXED_STRINGS),
        { } '''' + sLongParam + ''' should be in the options');

  end;
  Assert.IsTrue(not guiParams.IsSet([EGuiOption.soMatchWord]),
      { } Integer(EGuiOption.soMatchWord).ToString + ' should not be in the options');

  if EGuiOption.soMatchWord in guiParams.SearchOptions then begin
    Assert.IsTrue(TOptionsHelper.IsWordBoundOnBothSide(guiParams.SearchText),
        { } guiParams.SearchText + ' should be word bounded');
  end else begin
    Assert.IsTrue(not TOptionsHelper.IsWordBoundOnBothSide(guiParams.SearchText),
        { } guiParams.SearchText + ' should not be word bounded');
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

procedure TOptionsHelperTest.TestUpdateRgExeOptions(const _sOptions, _sRegEx : string; const _bRemove : Integer);
var
  arrOptions : TArrayEx<string>;
begin
  var
    sOps : string := '';
  if _bRemove = 1 then begin
    sOps := TGuiSearchTextParams.RemoveRgExeOptions(_sOptions, _sRegex);
  end else begin
    sOps := TGuiSearchTextParams.AddRgExeOptions(_sOptions, _sRegex);
  end;
  arrOptions := sOps.Split([' ']);
  for var s : string in arrOptions do begin
    if _bRemove = 1 then begin
      Assert.AreEqual(_bRemove <> 1, TRegEx.IsMatch(s, _sRegex), '''' + s + ''' should''t be in the options array:' + sOps);
    end else if (s = _sRegex.Split(['|'])[RG_PARAM_LONG_INDEX]) then begin
      Assert.AreEqual(_bRemove <> 1, TRegEx.IsMatch(s, _sRegex), '''' + s + ''' should''t be in the options array:' + sOps);
    end;
  end;
end;

procedure TOptionsHelperTest.TestRemoveRgExeOptionsWthValue(const _sOptions, _sRegEx : string);
var
  arrOptions : TArrayEx<string>;
begin
  var
    sArgs : string := '';

  sArgs := TGuiSearchTextParams.RemoveRgExeOptions(_sOptions, _sRegex); // Remove

  arrOptions := sArgs.Split([' '], TStringSplitOptions.ExcludeEmpty);
  for var s in arrOptions do begin
    Assert.IsTrue(TRegEx.IsMatch(s, '^-+\w+'), '''' + s + ''' invalid param (maybe a glob) should not bee in the options array');
    Assert.IsFalse(TRegEx.IsMatch(s, _sRegex), '''' + s + ''' should not bee in the options array');
  end;
end;

procedure TOptionsHelperTest.TestAddRgExeOptionsWthValue(const _sOptions, _sRegEx, _sValue : string; const _bUnique : integer);
var
  arrOptions : TArrayEx<string>;
  sArgs : string;
  bFound : Boolean;
  sOpWthVal : string;
begin
  bFound := False;

  sArgs := TGuiSearchTextParams.AddRgExeOptionWithValue(_sOptions, _sRegex, _sValue, _bUnique = 1);
  arrOptions := sArgs.Split([' '], TStringSplitOptions.ExcludeEmpty);
  sOpWthVal := _sRegex.split(['|'])[RG_PARAM_LONG_INDEX] + '=' + _sValue;
  for var s in arrOptions do begin
    if TRegEx.IsMatch(s, TRegEx.Escape(sOpWthVal)) then
      bFound := True;
    Assert.IsTrue(TRegEx.IsMatch(s, '^-+\w+'), '''' + s + ''' invalid param (maybe a glob) should not bee in the options array');
  end;
  Assert.IsTrue(bFound, '''' + sOpWthVal + ''' should bee in the options array');
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

procedure TOptionsHelperTest.TestUpdateSearchText(const _sOptions, _sSearchText : string;
    const _bMatchWord, _bUseRegex, _bShouldBounded, _bShouldEscaped : Integer);
var
  sAct : string;
  p : string;
  gsp : TGuiSearchTextParams;
begin
  FParams.RgExeOptions := _sOptions;
  FParams.FileMasks := '';

  gsp := TGuiSearchTextParams.New(_sSearchText, False, _bMatchWord = 1, _bUseRegex = 1);
  gsp.RgOptions := _sOptions;
  FParams.GuiSetSearchParams := gsp;

  if (_bMatchWord = 1) then
    gsp.SetOption(EGuiOption.soMatchWord);
  if (_bUseRegex = 1) then
    gsp.SetOption(EGuiOption.soUseRegex);

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
