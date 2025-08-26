unit RipGrepper.Common.IOTAUtils.TPathProcessor.Test;

interface

uses
	DUnitX.TestFramework,
	System.Classes,
	System.SysUtils,
	Variants,
	ToolsAPI,
	Delphi.Mocks,
	RipGrepper.Common.IOTAUtils.PathProcessor;

type

	[TestFixture]
	TPathProcessorTest = class
		const
			IDE_PATH = 'C:\Program Files\Embarcadero\Studio\22.0';

		private
			// FOptionNameArray : TOTAOptionNameArray;
			// FIProjectOptionsMock : TMock<IOTAProjectOptions>;
			// FIProjectMock : TMock<IOTAProject>;
			FPathProcessor : TPathProcessor;

		public
			[Setup]
			procedure Setup;

			[TearDown]
			procedure TearDown;

			[Test]
			[TestCase('BDS Macro', '$(BDS)\lib,' + IDE_PATH + '\lib')]
			[TestCase('DELPHI Macro', '$(DELPHI)\source,' + IDE_PATH + '\source')]
			[TestCase('BCB Macro', '$(BCB)\include,' + IDE_PATH + '\include')]
			procedure TestIdeBaseMacros(const Input, Expected : string);

			[Test]
			[TestCase('Platform Win32', '$(Platform)\release,Win32\release')]
			[TestCase('Platform Win64', '$(Platform)\debug,Win64\debug')]
			procedure TestPlatformMacro(const Input, Expected : string);

			[Test]
			[TestCase('Config Debug', '$(Config)\temp,Debug\temp')]
			[TestCase('Config Release', '$(Config)\output,Release\output')]
			procedure TestConfigMacro(const Input, Expected : string);

			[Test]
			[TestCase('Multiple Macros', '$(BDS)\$(Platform)\$(Config),' + IDE_PATH + '\Win32\Debug')]
			procedure TestMultipleMacros(const Input, Expected : string);

			[Test]
			[TestCase('No Macros', 'C:\SomeFolder\Test,C:\SomeFolder\Test')]
			[TestCase('Partial Macro', 'C:\$(NotAMacro)\Test,C:\$(NotAMacro)\Test')]
			procedure TestNoMacroReplacement(const Input, Expected : string);

			[Test]
			procedure TestEnvironmentVariables;

//   [Test]
			procedure TestProjectDefines;

			[Test]
			procedure TestRelativePathHandling;

			[Test]
			procedure TestOriginalBugScenario;

			[Test]
			procedure TestReplaceMacroMethod;
	end;

implementation

uses
	Winapi.Windows,
	System.StrUtils,
	System.Rtti;

{ TPathProcessorTest }

procedure TPathProcessorTest.Setup;
begin
	// FIProjectOptionsMock := TMock<IOTAProjectOptions>.Create;
	// var
	// opname : TOTAOptionName;
	// opname.Name := 'name';
	// opname.Kind := tkString;
	// FOptionNameArray := FOptionNameArray + [opname];
	// FIProjectOptionsMock.Setup.WillReturn(TValue.From<TOTAOptionNameArray>(FOptionNameArray)).When.GetOptionNames();
	// FIProjectOptionsMock.Setup.WillReturn(TValue.From<Variant>('val')).When.GetOptionValue(It0.IsAny<string>());
	//
	// FIProjectMock := TMock<IOTAProject>.Create;
	// FIProjectMock.Setup.WillReturn('Win32').When.GetPlatform;
	// FIProjectMock.Setup.WillReturn('Release').When.GetConfiguration;
	// FIProjectMock.Setup.WillReturn(TValue.From<IOTAProjectOptions>(FIProjectOptionsMock.Instance)).When.GetProjectOptions;
	FPathProcessor := TPathProcessor.Create('');
	FPathProcessor.IdeBasePath := IDE_PATH;
end;

procedure TPathProcessorTest.TearDown;
begin
	FreeAndNil(FPathProcessor);
end;

procedure TPathProcessorTest.TestIdeBaseMacros(const Input, Expected : string);
var
	Result : string;
begin
	Result := FPathProcessor.Process(Input, True);
	Assert.AreEqual(Expected, Result, Format('Failed to process IDE base macro. Input: %s', [Input]));
end;

procedure TPathProcessorTest.TestPlatformMacro(const Input, Expected : string);
var
	Result : string;
begin
	// Test both Win32 and Win64
	FPathProcessor.PlatformName := 'Win32';
	if Expected.Contains('Win64') then
		FPathProcessor.PlatformName := 'Win64';

	Result := FPathProcessor.Process(Input, True);
	Assert.AreEqual(Expected, Result, Format('Failed to process Platform macro. Input: %s', [Input]));
end;

procedure TPathProcessorTest.TestConfigMacro(const Input, Expected : string);
var
	Result : string;
begin
	// Test both Debug and Release
	FPathProcessor.ConfigName := 'Debug';
	if Expected.Contains('Release') then
		FPathProcessor.ConfigName := 'Release';

	Result := FPathProcessor.Process(Input, True);
	Assert.AreEqual(Expected, Result, Format('Failed to process Config macro. Input: %s', [Input]));
end;

procedure TPathProcessorTest.TestMultipleMacros(const Input, Expected : string);
var
	Result : string;
begin
	FPathProcessor.PlatformName := 'Win32';
	FPathProcessor.ConfigName := 'Debug';
	Result := FPathProcessor.Process(Input, True);
	Assert.AreEqual(Expected, Result, Format('Failed to process multiple macros. Input: %s', [Input]));
end;

procedure TPathProcessorTest.TestNoMacroReplacement(const Input, Expected : string);
var
	Result : string;
begin
	Result := FPathProcessor.Process(Input, True);
	Assert.AreEqual(Expected, Result, Format('Unexpected macro replacement. Input: %s', [Input]));
end;

procedure TPathProcessorTest.TestEnvironmentVariables;
var
	TestPath : string;
	Result : string;
	TempVar : string;
begin
	// Set a test environment variable
	TempVar := 'DRIPGREPPER_TEST_VAR';
	SetEnvironmentVariable(PChar(TempVar), PChar('C:\TestValue'));
	try
		// Recreate processor to pick up new environment variable
		FreeAndNil(FPathProcessor);
		FPathProcessor := TPathProcessor.Create('');

		TestPath := '$(' + TempVar + ')\subfolder';
		Result := FPathProcessor.Process(TestPath);
		Assert.AreEqual('C:\TestValue\subfolder', Result, 'Failed to process environment variable macro');
	finally
		SetEnvironmentVariable(PChar(TempVar), nil); // Remove test variable
	end;
end;

procedure TPathProcessorTest.TestProjectDefines;
var
	TestPath : string;
	Result : string;
begin
	// Add test defines
	// .AddProjectDefine('TESTDEFINE', 'TestValue');
	// .AddProjectDefine('SIMPLE_DEFINE', ''); // Empty value should default to '1'

	TestPath := '$(TESTDEFINE)\test\$(SIMPLE_DEFINE)';
	Result := FPathProcessor.Process(TestPath, True);
	Assert.AreEqual('TestValue\test\1', Result, 'Failed to process project defines');
end;

procedure TPathProcessorTest.TestRelativePathHandling;
var
	RelativePath : string;
	Result : string;
begin
	// Create processor with prefix for relative path handling
	FreeAndNil(FPathProcessor);
	FPathProcessor := TPathProcessor.Create('C:\TestProject');

	RelativePath := 'relative\path';
	Result := FPathProcessor.Process(RelativePath, True);
	Assert.AreEqual('C:\TestProject\relative\path', Result, 'Failed to handle relative path correctly');
end;

procedure TPathProcessorTest.TestOriginalBugScenario;
var
	TestPath : string;
	Result : string;
begin
	// Test the original bug scenario: $(BDS)\$(Platform)\release
	// Should expand to: C:\Program Files\Embarcadero\Studio\22.0\Win32\release
	TestPath := '$(BDS)\$(Platform)\release';

	FPathProcessor.PlatformName := 'Win32';

	Result := FPathProcessor.Process(TestPath);
	Assert.AreEqual(IDE_PATH + '\Win32\release', Result,
		'Failed to process original bug scenario: $(BDS)\$(Platform)\release');

	// Test with Win64 platform
	FPathProcessor.PlatformName := 'Win64';
	Result := FPathProcessor.Process(TestPath);
	Assert.AreEqual('C:\Program Files\Embarcadero\Studio\22.0\Win64\release', Result, 'Failed to process Win64 platform scenario');
end;

procedure TPathProcessorTest.TestReplaceMacroMethod;
var
	TestStr : string;
	Result : string;
begin
	TestStr := 'This is a $(TEST) string with $(TEST) macros';
	Result := TPathProcessor.ReplaceMacro(TestStr, 'TEST', 'REPLACED');
	Assert.AreEqual('This is a REPLACED string with REPLACED macros', Result, 'ReplaceMacro method failed');

	// Test case insensitive
	Result := TPathProcessor.ReplaceMacro('$(test)', 'TEST', 'replaced');
	Assert.AreEqual('replaced', Result, 'ReplaceMacro should be case insensitive');

	// Test no replacement
	Result := TPathProcessor.ReplaceMacro('No macros here', 'TEST', 'replaced');
	Assert.AreEqual('No macros here', Result, 'ReplaceMacro should not change string without macros');
end;

end.
