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
			procedure TestReBuildArguments(const _sOptions, _sMasksDelimited : string; const _bMatchWord : Integer);
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

end;

procedure TCommandLineBuilderTest.TearDown;
begin

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

procedure TCommandLineBuilderTest.TestReBuildArguments(const _sOptions, _sMasksDelimited : string; const _bMatchWord : Integer);
var
	rgParams : TRipGrepParameterSettings;
	ini : TIniFile;
	v : TArrayEx<string>;
	a : TArrayEx<integer>;
begin
	ini := TIniFile.Create('DripGrepperUnittest.ini');
	rgParams := TRipGrepParameterSettings.Create(ini);
	TCommandLineBuilder.ReBuildArguments(rgParams);
	for var s in RG_NECESSARY_PARAMS do begin
		v := rgParams.RipGrepArguments.GetValues(RG_ARG_OPTIONS);
		Assert.IsTrue(v.Contains(s), s + ' necessary option should be contained.');
		a := v.AllIndexOf(s);
		Assert.IsTrue(1 = a.Count, s + ' necessary option should appear only once.');
	end;
end;

initialization

TDUnitX.RegisterTestFixture(TCommandLineBuilderTest);

end.
