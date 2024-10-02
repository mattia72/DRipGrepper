unit RipGrepper.CommandLine.Builder.Test;

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
	TCommandLineBuilderTest = class

		private
			FGuiParams : TGuiSearchTextParams;
			FIniFile : TMemIniFile;
			FParams : TRipGrepParameterSettings;

		public
			[Setup]
			procedure Setup;
			[TearDown]
			procedure TearDown;

			[Test]
			[Testcase('test1', '--vimgrep -g=*.txt --fixed-strings -g=*.ini --ignore-case -g=*.bak|*.txt;*.ini;*.bak', '|')]
			[Testcase('test2', '--vimgrep -g=!*.txt --fixed-strings -g=!*.ini -i         -g=!*.bak|!*.txt;!*.ini;!*.bak', '|')]
			procedure TestGetMaskParamsFromOptions(const _sOptions, _sMasks : string);
			[Test]
			[Testcase('test1', '--vimgrep -g=*.txt  -f -g=*.imi  -i -g=*.buk |*.txt;*.ini;*.bak', '|')]
			[Testcase('test2', '--vimgrep -g=!*.tyt -f -g=!*.imi -i -g=!*.bak|!*.txt;!*.ini;!*.bak', '|')]
			procedure TestGetMissingOptions(const _sOptions, _sMasks : string);
			[Test]
			[Testcase('Options', '--param1 --param2|*.txt;*.ini;*.bak|1', '|')]
			[Testcase('Options with necessary', '--vimgrep --param2|*.txt;*.ini;*.bak|1', '|')]
			[Testcase('Options ends with one --', '--vimgrep --param2 --|*.txt;*.ini;*.bak|1', '|')]
			procedure TestReBuildArgumentsOptions(const _sOptions, _sMasksDelimited : string; const _bMatchWord : Integer);
			[Test]
			[Testcase('Single word              ', 'aaa      |1|1', '|')]
			[Testcase('Double word              ', 'aaa bbb  |1|1', '|')]
			[Testcase('Double word              ', 'aaa bbb  |1|1', '|')]
			[Testcase('Start Bounded word       ', '\baaa    |0|0', '|')]
			[Testcase('End Bounded word         ', 'aaa\b    |0|0', '|')]
			[Testcase('Start Bounded double word', '\Baaa bbb|0|0', '|')]
			procedure TestReBuildArgumentsSearchText(const _sSearchText : string; const _bMatchWord, _bShouldBounded : Integer);
	end;

implementation

uses
	RipGrepper.Helper.Types,
	System.StrUtils,
	System.SysUtils,
	RipGrepper.Common.Settings.AppSettings,
	ArrayEx,
	System.RegularExpressions,
	System.Math, 
	RipGrepper.CommandLine.OptionStrings;

procedure TCommandLineBuilderTest.Setup;
begin
	FIniFile := TMemIniFile.Create('DripGrepperUnittest.ini');
	FParams := TRipGrepParameterSettings.Create(FIniFile);
	FGuiParams := TGuiSearchTextParams.Create('');
end;

procedure TCommandLineBuilderTest.TearDown;
begin
//	FGuiParams.Free;
	FParams.Free;
	FIniFile.Free;
end;

procedure TCommandLineBuilderTest.TestGetMaskParamsFromOptions(const _sOptions, _sMasks : string);
var
	arrMasks : TArrayEx<string>;
begin
	arrMasks := TCommandLineBuilder.GetFileMaskParamsFromOptions(_sOptions);

	for var s in _sMasks.Split([';']) do begin
		Assert.IsTrue(arrMasks.Contains(s), '''' + s + ''' should be in the mask array');
	end;
end;

procedure TCommandLineBuilderTest.TestGetMissingOptions(const _sOptions, _sMasks : string);
var
	sMaskOptions : string;
	arrMissingMaskOptions : TArrayEx<string>;
	arrAllOptions : TArrayEx<string>;
	arrMasks : TArrayEx<string>;
begin
	sMaskOptions := TCommandLineBuilder.GetMissingFileMaskOptions(_sOptions, _sMasks);
	arrMissingMaskOptions := sMaskOptions.Split([' ']);

	for var i : integer := 0 to arrMissingMaskOptions.MaxIndex do begin
		Assert.IsTrue(TRegex.IsMatch(arrMissingMaskOptions[i], '-g='), Format('%d. param should be -g= not ',
			[i, arrMissingMaskOptions[i]]));
	end;

	arrMasks := _sMasks.Split([';']);
	arrAllOptions := (_sOptions + ' ' + sMaskOptions).Split([' ']);

	for var i : integer := 0 to arrMasks.MaxIndex do begin
		var
		idx := arrAllOptions.IndexOf('-g=' + arrMasks[i]);
		Assert.IsTrue(idx >= 0, 'All options should contain added mask:' + arrMasks[i]);
	end;
end;

procedure TCommandLineBuilderTest.TestReBuildArgumentsOptions(const _sOptions, _sMasksDelimited : string; const _bMatchWord : Integer);
var
	v : TArrayEx<string>;
	a : TArrayEx<integer>;
begin
	FParams.RgExeOptions := TOptionStrings.New(_sOptions);
	FParams.FileMasks := _sMasksDelimited;
	FGuiParams.SearchOptions := TGuiSearchTextParams.GetAsSearchOptionSet(False, _bMatchWord = 1, False);
	FParams.GuiSearchTextParams := FGuiParams;

	TCommandLineBuilder.RebuildArguments(FParams);
	v := FParams.RipGrepArguments.GetValues(RG_ARG_OPTIONS);
	for var s in RG_NECESSARY_PARAMS do begin
		Assert.IsTrue(v.Contains(s), s + ' necessary option should be contained.');
		a := v.AllIndexOf(s);
		Assert.IsTrue(1 = a.Count, s + ' necessary option should appear only once.');
	end;

	for var s in _sMasksDelimited.Split([';']) do begin
		Assert.IsTrue(v.Contains('-g=' + s), s + ' mask should be contained');
	end;

	Assert.AreEqual(RG_PARAM_END, v.Last, 'The last option should be --');
	Assert.AreEqual(1, v.CountOf(RG_PARAM_END), 'The last option should be unique');

end;

procedure TCommandLineBuilderTest.TestReBuildArgumentsSearchText(const _sSearchText : string; const _bMatchWord, _bShouldBounded : Integer);
begin
	FParams.RgExeOptions := TOptionStrings.New('');
	FParams.FileMasks := '';
	FGuiParams.SearchText := _sSearchText;
	FGuiParams.SearchOptions := TGuiSearchTextParams.GetAsSearchOptionSet(False, _bMatchWord = 1, False);

	FParams.GuiSearchTextParams := FGuiParams;
	if _bMatchWord = 1 then
		FParams.GuiSearchTextParams.SetOption(EGuiOption.soMatchWord);

	TCommandLineBuilder.RebuildArguments(FParams);

	if _bShouldBounded = 1 then begin
		Assert.AreEqual(WB + _sSearchText + WB, FParams.RipGrepArguments.Values[RG_ARG_SEARCH_TEXT],
			'the search text should surrounded: ' + WB + _sSearchText + WB);
		if EGuiOption.soMatchWord in FParams.GuiSearchTextParams.SearchOptions then begin
			for var p in RG_PARAM_REGEX_FIXED_STRINGS.Split(['|']) do begin
				Assert.IsFalse(FParams.RgExeOptions.IsOptionSet(p), p + ' mustn''t be contained between options')
			end;
		end;

	end else begin
		Assert.AreEqual(_sSearchText, FParams.RipGrepArguments.Values[RG_ARG_SEARCH_TEXT],
			'if MatchWord is not set, then search text should equal' + _sSearchText);
	end;

end;

initialization

TDUnitX.RegisterTestFixture(TCommandLineBuilderTest);

end.
