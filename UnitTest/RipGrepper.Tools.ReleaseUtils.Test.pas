unit RipGrepper.Tools.ReleaseUtils.Test;

interface

uses
	DUnitX.TestFramework,
	RipGrepper.Tools.ReleaseUtils,
	System.IOUtils,
	System.SysUtils;

{$IFDEF TESTINSIGHT}
{$DEFINE DUNITX}
{$ENDIF}

type

	[TestFixture]
	TTestReleaseUtils = class
		public
			[Test]
			procedure Test_CompilerVersion_Constant;
			[Test]
			procedure Test_GetAppDirectory_ReturnsValidDirectory;

			[Test]
			procedure Test_GetAppNameAndVersion_ReturnsExpectedFormat;
	end;

implementation

uses
	RipGrepper.Common.Constants,
	System.RegularExpressions;

procedure TTestReleaseUtils.Test_CompilerVersion_Constant;
begin
	// This test checks that the CompilerVersion macro is defined and has a plausible value
	// Delphi 12 = 36, Delphi 11 = 35, Delphi 10.4 = 34, etc.
	{$IF CompilerVersion = COMPILER_VERSION_DELPHI_11}
	Assert.IsTrue(CompilerVersion = COMPILER_VERSION_DELPHI_11, 'CompilerVersion is ' + FloatToStr(CompilerVersion));
	{$IFEND}
	{$IF CompilerVersion = COMPILER_VERSION_DELPHI_12}
	Assert.IsTrue(CompilerVersion = COMPILER_VERSION_DELPHI_12, 'CompilerVersion is ' + FloatToStr(CompilerVersion));
	{$IFEND}
end;

procedure TTestReleaseUtils.Test_GetAppDirectory_ReturnsValidDirectory();
var
	dir : string;
begin
	dir := TReleaseUtils.GetAppDirectory();
	// Check that the result is not empty and the directory exists
	Assert.IsFalse(dir.IsEmpty, 'App directory should not be empty');
	Assert.IsTrue(TDirectory.Exists(dir), 'App directory does not exist: ' + dir);

	// Check if directory ends with UnitTest\Release or UnitTest\Debug
	var
	dirLower := dir.ToLower;
	// Use regex to check if directory matches UnitTest\[win32|win64]\[release|debug]
	var
	regex := TRegEx.Create('unittest\\(win32|win64)\\(release|debug)$', [roIgnoreCase]);
	Assert.IsTrue(regex.IsMatch(dirLower), 'App directory should match UnitTest\[win32|win64]\[release|debug], but was: ' + dir);
end;

procedure TTestReleaseUtils.Test_GetAppNameAndVersion_ReturnsExpectedFormat();
var
	exePath : string;
	resultStr : string;
begin
	exePath := TReleaseUtils.GetRunningModulePath();
	resultStr := TReleaseUtils.GetAppNameAndVersion(exePath);
	// Should contain name, platform and version separated by spaces or expected format
	Assert.IsFalse(resultStr.IsEmpty, 'App name and version string should not be empty');
	Assert.IsTrue(resultStr.Contains(' '), 'App name and version string should contain spaces');
	// Optionally check for known platform or version substring
end;

initialization

TDUnitX.RegisterTestFixture(TTestReleaseUtils);

end.
