unit RipGrepper.Settings.Test;

interface

uses
	DUnitX.TestFramework,
	System.Classes,
	RipGrepper.Common.Constants;

type

	[TestFixture]
	TSettingsTest = class

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
			[Testcase('test1', '--vimgrep -g *.txt --fixed-strings -g *.ini --ignore-case -g *.bak|' + RG_PARAM_REGEX_GLOB + '|0', '|')]
			[Testcase('test2', '--vimgrep -g !*.txt --fixed-strings -g !*.ini -i         -g !*.bak|!*.txt;!*.ini;!*.bak', '|')]
			procedure TestRemoveAllParams(const _sOptions, _sRegEx : string; const _bSwitch : Integer);

			[Test]
			[Testcase('test1', '--vimgrep -g *.txt  -f -g *.imi  -i -g *.buk |*.txt;*.ini;*.bak', '|')]
			[Testcase('test2', '--vimgrep -g !*.tyt -f -g !*.imi -i -g !*.bak|!*.txt;!*.ini;!*.bak', '|')]
			procedure TestGetMissingOptions(const _sOptions, _sMasks : string);
	end;

implementation

uses
	RipGrepper.Helper.Types,
	System.StrUtils,
	System.SysUtils,
	RipGrepper.Common.Settings,
	ArrayEx,
	System.RegularExpressions;

procedure TSettingsTest.Setup;
begin

end;

procedure TSettingsTest.TearDown;
begin

end;

procedure TSettingsTest.TestGetMaskParamsFromOptions(const _sOptions, _sMasks : string);
var
	arrMasks : TArrayEx<string>;
begin
	arrMasks := TRipGrepParameterSettings.GetFileMaskParamsFromOptions(_sOptions);

	for var s in _sMasks.Split([';']) do begin
		Assert.IsTrue(arrMasks.Contains(s), '''' + s + ''' should be in the mask array');
	end;
end;

procedure TSettingsTest.TestRemoveAllParams(const _sOptions, _sRegEx : string; const _bSwitch : Integer);
var
	arrOptions : TArrayEx<string>;
begin
	var
	s := TRipGrepParameterSettings.RemoveAllParams(_sOptions, _sRegex, (_bSwitch = 1));
	arrOptions := s.Split([' ']);
	for s in arrOptions do begin
		Assert.IsFalse(TRegEx.IsMatch(s, _sRegex), '''' + s + ''' should not bee in the options array');
	end;
end;

procedure TSettingsTest.TestGetMissingOptions(const _sOptions, _sMasks : string);
var
	sMaskOptions : string;
	arrMissingMaskOptions : TArrayEx<string>;
	arrAllOptions : TArrayEx<string>;
	arrMasks : TArrayEx<string>;
begin
	sMaskOptions := TRipGrepParameterSettings.GetMissingFileMaskOptions(_sOptions, _sMasks);
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

initialization

TDUnitX.RegisterTestFixture(TSettingsTest);

end.
