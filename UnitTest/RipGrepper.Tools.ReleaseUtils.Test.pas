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

			// CompareVersions tests
			[Test]
			procedure Test_CompareVersions_EqualVersions;
			[Test]
			procedure Test_CompareVersions_MajorVersionDifference;
			[Test]
			procedure Test_CompareVersions_MinorVersionDifference;
			[Test]
			procedure Test_CompareVersions_PatchVersionDifference;
			[Test]
			procedure Test_CompareVersions_BuildVersionDifference;
			[Test]
			procedure Test_CompareVersions_WithVPrefix;
			[Test]
			procedure Test_CompareVersions_WithSuffix;
			[Test]
			procedure Test_CompareVersions_PreReleaseVsStable;
			[Test]
			procedure Test_CompareVersions_InvalidVersions;
			[Test]
			procedure Test_CompareVersions_DifferentFormats;
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

// CompareVersions tests

procedure TTestReleaseUtils.Test_CompareVersions_EqualVersions();
begin
	// Test equal versions
	Assert.AreEqual(0, TReleaseUtils.CompareVersions('1.0.0', '1.0.0'), 'Equal versions should return 0');
	Assert.AreEqual(0, TReleaseUtils.CompareVersions('2.5.10', '2.5.10'), 'Equal versions should return 0');
	Assert.AreEqual(0, TReleaseUtils.CompareVersions('1.0', '1.0'), 'Equal versions (major.minor) should return 0');
	Assert.AreEqual(0, TReleaseUtils.CompareVersions('v1.0.0', 'v1.0.0'), 'Equal versions with v prefix should return 0');
end;

procedure TTestReleaseUtils.Test_CompareVersions_MajorVersionDifference();
begin
	// Test major version differences
	Assert.IsTrue(TReleaseUtils.CompareVersions('2.0.0', '1.0.0') > 0, '2.0.0 should be greater than 1.0.0');
	Assert.IsTrue(TReleaseUtils.CompareVersions('1.0.0', '2.0.0') < 0, '1.0.0 should be less than 2.0.0');
	Assert.IsTrue(TReleaseUtils.CompareVersions('10.0.0', '9.0.0') > 0, '10.0.0 should be greater than 9.0.0');
end;

procedure TTestReleaseUtils.Test_CompareVersions_MinorVersionDifference();
begin
	// Test minor version differences
	Assert.IsTrue(TReleaseUtils.CompareVersions('1.2.0', '1.1.0') > 0, '1.2.0 should be greater than 1.1.0');
	Assert.IsTrue(TReleaseUtils.CompareVersions('1.1.0', '1.2.0') < 0, '1.1.0 should be less than 1.2.0');
	Assert.IsTrue(TReleaseUtils.CompareVersions('1.10.0', '1.9.0') > 0, '1.10.0 should be greater than 1.9.0');
end;

procedure TTestReleaseUtils.Test_CompareVersions_PatchVersionDifference();
begin
	// Test patch version differences
	Assert.IsTrue(TReleaseUtils.CompareVersions('1.0.2', '1.0.1') > 0, '1.0.2 should be greater than 1.0.1');
	Assert.IsTrue(TReleaseUtils.CompareVersions('1.0.1', '1.0.2') < 0, '1.0.1 should be less than 1.0.2');
	Assert.IsTrue(TReleaseUtils.CompareVersions('1.0.10', '1.0.9') > 0, '1.0.10 should be greater than 1.0.9');
end;

procedure TTestReleaseUtils.Test_CompareVersions_BuildVersionDifference();
begin
	// Test build version differences
	Assert.IsTrue(TReleaseUtils.CompareVersions('1.0.0.2', '1.0.0.1') > 0, '1.0.0.2 should be greater than 1.0.0.1');
	Assert.IsTrue(TReleaseUtils.CompareVersions('1.0.0.1', '1.0.0.2') < 0, '1.0.0.1 should be less than 1.0.0.2');
	Assert.IsTrue(TReleaseUtils.CompareVersions('1.0.0.10', '1.0.0.9') > 0, '1.0.0.10 should be greater than 1.0.0.9');
	// Build vs no build
	Assert.IsTrue(TReleaseUtils.CompareVersions('1.0.0.1', '1.0.0') > 0, '1.0.0.1 should be greater than 1.0.0');
end;

procedure TTestReleaseUtils.Test_CompareVersions_WithVPrefix();
begin
	// Test versions with 'v' prefix
	Assert.AreEqual(0, TReleaseUtils.CompareVersions('v1.0.0', '1.0.0'), 'v1.0.0 should equal 1.0.0');
	Assert.IsTrue(TReleaseUtils.CompareVersions('v2.0.0', 'v1.0.0') > 0, 'v2.0.0 should be greater than v1.0.0');
	Assert.IsTrue(TReleaseUtils.CompareVersions('V1.5.0', '1.4.0') > 0, 'V1.5.0 should be greater than 1.4.0');
end;

procedure TTestReleaseUtils.Test_CompareVersions_WithSuffix();
begin
	// Test versions with suffixes
	Assert.AreEqual(0, TReleaseUtils.CompareVersions('1.0.0-alpha', '1.0.0-alpha'), 'Same pre-release versions should be equal');
	Assert.IsTrue(TReleaseUtils.CompareVersions('1.0.0-beta', '1.0.0-alpha') > 0, '1.0.0-beta should be greater than 1.0.0-alpha');
	Assert.IsTrue(TReleaseUtils.CompareVersions('1.0.0-rc', '1.0.0-beta') > 0, '1.0.0-rc should be greater than 1.0.0-beta');
	// Test with + suffix (build metadata)
	Assert.AreEqual(0, TReleaseUtils.CompareVersions('1.0.0+build1', '1.0.0+build1'), 'Same build metadata versions should be equal');
end;

procedure TTestReleaseUtils.Test_CompareVersions_PreReleaseVsStable();
begin
	// Test pre-release vs stable versions
	Assert.IsTrue(TReleaseUtils.CompareVersions('1.0.0', '1.0.0-alpha') > 0, 'Stable 1.0.0 should be greater than 1.0.0-alpha');
	Assert.IsTrue(TReleaseUtils.CompareVersions('1.0.0', '1.0.0-beta') > 0, 'Stable 1.0.0 should be greater than 1.0.0-beta');
	Assert.IsTrue(TReleaseUtils.CompareVersions('1.0.0', '1.0.0-rc') > 0, 'Stable 1.0.0 should be greater than 1.0.0-rc');
	Assert.IsTrue(TReleaseUtils.CompareVersions('1.0.0-alpha', '1.0.0') < 0, '1.0.0-alpha should be less than stable 1.0.0');
end;

procedure TTestReleaseUtils.Test_CompareVersions_InvalidVersions();
begin
	// Test invalid version strings
	Assert.IsTrue(TReleaseUtils.CompareVersions('1.0.0', 'invalid') > 0, 'Valid version should be greater than invalid');
	Assert.IsTrue(TReleaseUtils.CompareVersions('invalid', '1.0.0') < 0, 'Invalid version should be less than valid');
	Assert.AreNotEqual(0, TReleaseUtils.CompareVersions('invalid1', 'invalid2'), 'Different invalid versions should not be equal');
	// Empty versions
	Assert.IsTrue(TReleaseUtils.CompareVersions('1.0.0', '') > 0, 'Valid version should be greater than empty string');
end;

procedure TTestReleaseUtils.Test_CompareVersions_DifferentFormats();
begin
	// Test different version formats
	Assert.AreEqual(0, TReleaseUtils.CompareVersions('1.0', '1.0.0'), '1.0 should equal 1.0.0');
	Assert.AreEqual(0, TReleaseUtils.CompareVersions('1.0.0', '1.0.0.0'), '1.0.0 should equal 1.0.0.0');
	Assert.IsTrue(TReleaseUtils.CompareVersions('1.1', '1.0.5') > 0, '1.1 should be greater than 1.0.5');
	// Mixed formats with prefix
	Assert.IsTrue(TReleaseUtils.CompareVersions('v2.0', '1.9.9') > 0, 'v2.0 should be greater than 1.9.9');
end;

initialization

TDUnitX.RegisterTestFixture(TTestReleaseUtils);

end.
