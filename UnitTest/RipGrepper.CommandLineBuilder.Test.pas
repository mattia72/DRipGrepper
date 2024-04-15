unit RipGrepper.CommandLineBuilder.Test;

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
	TCommandLineBuilderTest = class

		private
			FIniFile : TIniFile;
			FParams : TRipGrepParameterSettings;

		public
			[Setup]
			procedure Setup;
			[TearDown]
			procedure TearDown;

			[Test]
			[Testcase('test1', '--vimgrep -g *.txt --fixed-strings -g *.ini --ignore-case -g *.bak|*.txt;*.ini;*.bak', '|')]
			[Testcase('test2', '--vimgrep -g !*.txt --fixed-strings -g !*.ini -i         -g !*.bak|!*.txt;!*.ini;!*.bak', '|')]
			procedure TestGetMaskParamsFromOptions(const _sOptions, _sMasks : string);
			[Test]
			[Testcase('test1', '--vimgrep -g *.txt --fixed-strings -g *.ini --ignore-case -g *.bak;' + RG_PARAM_REGEX_GLOB + ';0', ';')]
			[Testcase('test2', '--vimgrep -g !*.txt --fixed-strings -g !*.ini -i         -g !*.bak;' + RG_PARAM_REGEX_FIXED_STRINGS +
				';1', ';')]
			procedure TestRemoveAllParams(const _sOptions, _sRegEx : string; const _bSwitch : Integer);

			[Test]
			[Testcase('test1', '--vimgrep -g *.txt  -f -g *.imi  -i -g *.buk |*.txt;*.ini;*.bak', '|')]
			[Testcase('test2', '--vimgrep -g !*.tyt -f -g !*.imi -i -g !*.bak|!*.txt;!*.ini;!*.bak', '|')]
			procedure TestGetMissingOptions(const _sOptions, _sMasks : string);

			[Test]
			[Testcase('Options', '--param1 --param2|*.txt;*.ini;*.bak|1', '|')]
			[Testcase('Options with necessary', '--vimgrep --param2|*.txt;*.ini;*.bak|1', '|')]
			[Testcase('Options ends with one --', '--vimgrep --param2 --|*.txt;*.ini;*.bak|1', '|')]
			procedure TestReBuildArgumentsOptions(const _sOptions, _sMasksDelimited : string; const _bMatchWord : Integer);
			[Test]
			[Testcase('Single word              ', 'aaa|1|1', '|')]
			[Testcase('Double word              ', 'aaa bbb|1|1', '|')]
			[Testcase('Double word              ', 'aaa bbb|1|1', '|')]
			[Testcase('Start Bounded word       ', '\baaa|0|0', '|')]
			[Testcase('End Bounded word         ', 'aaa\b|0|0', '|')]
			[Testcase('Start Bounded double word', '\Baaa bbb|0|0', '|')]
			procedure TestReBuildArgumentsSearchText(const _sSearchText : string; const _bMatchWord, _bShouldBounded : Integer);
	end;

implementation

uses
	RipGrepper.Helper.Types,
	System.StrUtils,
	System.SysUtils,
	RipGrepper.Common.Settings,
	ArrayEx,
	System.RegularExpressions;

procedure TCommandLineBuilderTest.Setup;
begin
	FIniFile := TIniFile.Create('DripGrepperUnittest.ini');
	FParams := TRipGrepParameterSettings.Create(FIniFile);
end;

procedure TCommandLineBuilderTest.TearDown;
begin
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

procedure TCommandLineBuilderTest.TestRemoveAllParams(const _sOptions, _sRegEx : string; const _bSwitch : Integer);
var
	arrOptions : TArrayEx<string>;
begin
	var
	s := TCommandLineBuilder.RemoveAllParams(_sOptions, _sRegex, (_bSwitch = 1));
	arrOptions := s.Split([' ']);
	for s in arrOptions do begin
		Assert.IsFalse(TRegEx.IsMatch(s, _sRegex), '''' + s + ''' should not bee in the options array');
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
		if (i mod 2 = 0) then
			Assert.IsTrue(arrMissingMaskOptions[i] = '-g', Format('%d. param should be -g not ', [i, arrMissingMaskOptions[i]]));
	end;

	arrMasks := _sMasks.Split([';']);
	arrAllOptions := (_sOptions + ' ' + sMaskOptions).Split([' ']);

	for var i : integer := 0 to arrMasks.MaxIndex do begin
		var
		idx := arrAllOptions.IndexOf(arrMasks[i]);
		Assert.IsTrue(idx >= 0, 'All options should contain added mask:' + arrMasks[i]);
		Assert.IsTrue(arrAllOptions[idx - 1] = '-g', arrMasks[i] + ' should preceed -g');
	end;
end;

procedure TCommandLineBuilderTest.TestReBuildArgumentsOptions(const _sOptions, _sMasksDelimited : string; const _bMatchWord : Integer);
var
	v : TArrayEx<string>;
	a : TArrayEx<integer>;
	i : integer;
begin
	FParams.Options := _sOptions;
	FParams.FileMasks := _sMasksDelimited;

	FParams.SearchText := '';
	FParams.MatchWholeWord := _bMatchWord = 1;

	TCommandLineBuilder.RebuildArguments(FParams);
	v := FParams.RipGrepArguments.GetValues(RG_ARG_OPTIONS);
	for var s in RG_NECESSARY_PARAMS do begin
		Assert.IsTrue(v.Contains(s), s + ' necessary option should be contained.');
		a := v.AllIndexOf(s);
		Assert.IsTrue(1 = a.Count, s + ' necessary option should appear only once.');
	end;

	for var s in _sMasksDelimited.Split([';']) do begin
		Assert.IsTrue(v.Contains(s), s + ' mask should be contained');
		i := v.IndexOf(s);
		Assert.AreEqual('-g', v.Items[i - 1], s + ' mask should be preceeded by -g');
	end;

	Assert.AreEqual(RG_PARAM_END, v.Last, 'The last option should be --');
	Assert.AreEqual(1, v.CountOf(RG_PARAM_END), 'The last option should be unique');
end;

procedure TCommandLineBuilderTest.TestReBuildArgumentsSearchText(const _sSearchText : string; const _bMatchWord, _bShouldBounded : Integer);
begin
	FParams.Options := '';
	FParams.FileMasks := '';

	FParams.SearchText := _sSearchText;
	FParams.MatchWholeWord := _bMatchWord = 1;

	TCommandLineBuilder.RebuildArguments(FParams);

	if _bShouldBounded = 1 then begin
		Assert.AreEqual(WB + _sSearchText + WB, FParams.RipGrepArguments.Values[RG_ARG_SEARCH_TEXT],
			'the search text should surrounded: ' + WB + _sSearchText + WB);
	end else begin
		Assert.AreEqual(_sSearchText, FParams.RipGrepArguments.Values[RG_ARG_SEARCH_TEXT],
			'if MatchWord is not set, then search text should equal' + _sSearchText);
	end;

	if FParams.MatchWholeWord then begin
		for var p in RG_PARAM_REGEX_FIXED_STRINGS.Split(['|']) do begin
			Assert.IsFalse(FParams.Options.Contains(p), p + ' mustn''t be contained between options')
		end;
	end;

end;

initialization

TDUnitX.RegisterTestFixture(TCommandLineBuilderTest);

end.
