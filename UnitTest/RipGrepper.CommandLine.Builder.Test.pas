unit RipGrepper.CommandLine.Builder.Test;

interface

uses
	DUnitX.TestFramework,
	System.Classes,
	RipGrepper.Common.Constants,
	RipGrepper.CommandLine.Builder,
	RipGrepper.Settings.RipGrepParameterSettings,
	System.IniFiles,
	RipGrepper.Common.GuiSearchParams,
	RipGrepper.Settings.Persistable,
	RipGrepper.Settings.TestOwnerSettings,
	Spring;

type

	[TestFixture]
	TCommandLineBuilderTest = class

		private
			FGuiParams : IShared<TGuiSearchTextParams>;
			// FIniFile : TMemIniFile;
			FParams : TRipGrepParameterSettings;
			FOwner : TPersistableSettings;

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

			[Test]
			[Testcase('Simple equals', 'key=value', '|')]
			[Testcase('Multiple equals', 'key1=value1=extra', '|')]
			[Testcase('Equals at start', '=value', '|')]
			[Testcase('Equals at end', 'key=', '|')]
			[Testcase('Only equals', '=', '|')]
			[Testcase('Complex config', 'CONFIG_FILE=C:\config\app.ini', '|')]
			[Testcase('URL with equals', 'http://example.com?param=value&other=test', '|')]
			procedure TestReBuildArgumentsWithEqualsInSearchText(const _sSearchText : string);
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
	RipGrepper.CommandLine.OptionStrings,
	RipGrepper.Common.SearchTextWithOptions,
	RipGrepper.Common.SimpleTypes;

procedure TCommandLineBuilderTest.Setup;
begin
	FOwner := TTestOwnerSettings.Create(OWNER_INI_SECTION);
	// FIniFile := FOwner.PersisterFactory;
	FParams := TRipGrepParameterSettings.Create(FOwner);
	FGuiParams := Shared.Make<TGuiSearchTextParams>(TGuiSearchTextParams.Create(''));
end;

procedure TCommandLineBuilderTest.TearDown;
begin
	// FGuiParams.Free;
	FParams.Free;
	FOwner.Free;
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
	FGuiParams.SetSearchOptions(TSearchTextWithOptions.GetAsSearchOptionSet(False, _bMatchWord = 1, False));
	FParams.GuiSearchTextParams := FGuiParams;

	TCommandLineBuilder.RebuildArguments(FParams);
	v := FParams.RipGrepArguments.GetOptions();
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
	FGuiParams.SetSearchText(_sSearchText);
	FGuiParams.SetSearchOptions(TSearchTextWithOptions.GetAsSearchOptionSet(False, _bMatchWord = 1, False));

	FParams.GuiSearchTextParams := FGuiParams;
	if _bMatchWord = 1 then
		FParams.GuiSearchTextParams.SetOption(EGuiOption.soMatchWord);

	TCommandLineBuilder.RebuildArguments(FParams);

	if _bShouldBounded = 1 then begin
		Assert.AreEqual(WB + _sSearchText + WB, FParams.RipGrepArguments.Values[RG_ARG_SEARCH_TEXT],
			'the search text should surrounded: ' + WB + _sSearchText + WB);
		if EGuiOption.soMatchWord in FParams.GuiSearchTextParams.GetSearchOptions then begin
			for var p in RG_PARAM_REGEX_FIXED_STRINGS.Split(['|']) do begin
				Assert.IsFalse(FParams.RgExeOptions.IsOptionSet(p), p + ' mustn''t be contained between options')
			end;
		end;

	end else begin
		Assert.AreEqual(_sSearchText, FParams.RipGrepArguments.Values[RG_ARG_SEARCH_TEXT],
			'if MatchWord is not set, then search text should equal' + _sSearchText);
	end;

end;

procedure TCommandLineBuilderTest.TestReBuildArgumentsWithEqualsInSearchText(const _sSearchText : string);
var
	actualSearchText : string;
	argValues : TArray<string>;
	v : TArrayEx<string>;
	commandLine : string;
	stCmd : TShellType;
begin
	// Setup: Clear options and set search text with equals
	FParams.RgExeOptions := TOptionStrings.New('');
	FParams.FileMasks := '';
	FGuiParams.SetSearchText(_sSearchText);
	FGuiParams.SetSearchOptions([]); // No special options
	FParams.GuiSearchTextParams := FGuiParams;

	// Action: Build arguments
	TCommandLineBuilder.RebuildArguments(FParams);

	// Assert: Check that search text with equals is preserved correctly
	actualSearchText := FParams.RipGrepArguments.Values[RG_ARG_SEARCH_TEXT];
	Assert.AreEqual(_sSearchText, actualSearchText, Format('Search text with equals should be preserved exactly. Expected: "%s", Got: "%s"',
		[_sSearchText, actualSearchText]));

	// Assert: Check that the search text appears in the arguments
	argValues := FParams.RipGrepArguments.GetValues(RG_ARG_SEARCH_TEXT);
	Assert.AreEqual(Integer(1), Integer(Length(argValues)), 'Should have exactly one search text argument');
	Assert.AreEqual(_sSearchText, argValues[0], 'Search text in arguments should match input');

	// Assert: Verify that necessary parameters are still present
	v := FParams.RipGrepArguments.GetOptions();
	for var s in RG_NECESSARY_PARAMS do begin
		Assert.IsTrue(v.Contains(s), s + ' necessary option should be contained even with equals in search text.');
	end;

	// Assert: Check that the command line can be built without errors
	stCmd := TShellType.stPowershell;
	commandLine := FParams.GetCommandLine(stCmd);
	Assert.IsFalse(commandLine.IsEmpty, 'Command line should not be empty');

	// Assert: Verify search text appears quoted in command line if it contains special characters
	if _sSearchText.Contains(' ') or _sSearchText.Contains('=') or _sSearchText.Contains('&') then begin
		Assert.IsTrue(commandLine.Contains('"' + _sSearchText + '"') or commandLine.Contains('''' + _sSearchText + ''''),
			'Search text:' + _sSearchText + ' with special characters should be quoted in command line');
	end;
end;

initialization

TDUnitX.RegisterTestFixture(TCommandLineBuilderTest);

end.
