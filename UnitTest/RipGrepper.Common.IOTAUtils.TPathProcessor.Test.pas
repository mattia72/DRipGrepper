unit RipGrepper.Common.IOTAUtils.TPathProcessor.Test;

interface

uses
	DUnitX.TestFramework,
	System.Classes,
	System.SysUtils,
	Variants;

type
	// Mock interface for project options
	ITestProjectOptions = interface
		function GetValues(const Name: string): Variant;
		property Values[const Name: string]: Variant read GetValues;
	end;

	// Mock implementation for project options
	TTestProjectOptions = class(TInterfacedObject, ITestProjectOptions)
	private
		FOptions: TStringList;
		FDefineValue: string;
	public
		constructor Create;
		destructor Destroy; override;
		function GetValues(const Name: string): Variant;
		procedure SetDefineValue(const Value: string);
	end;

	// Testable version of path processing logic that mirrors the real TPathProcessor
	TTestablePathProcessor = class
	private
		FIdeBasePath: string;
		FConfigName: string;
		FPlatformName: string;
		FPrefix: string;
		FEnvironment: TStringList;
		FProjectOptions: ITestProjectOptions;
		class function ReplaceMacro(const _str, _oldValue, _newValue: string): string;
	public
		constructor Create(const _Prefix: string; const AIdeBasePath: string = 'C:\Program Files\Embarcadero\Studio\22.0');
		destructor Destroy; override;
		procedure GetEnvironmentVariables(Strings: TStrings);
		procedure SetProjectOptions(Options: ITestProjectOptions);
		procedure AddProjectDefine(const Name, Value: string);
		procedure ClearProjectDefines;
		function Process(const _Path: string): string;
		property PlatformName: string read FPlatformName write FPlatformName;
		property ConfigName: string read FConfigName write FConfigName;
		property IdeBasePath: string read FIdeBasePath write FIdeBasePath;
	end;

	[TestFixture]
	TPathProcessorTest = class
	private
		FPathProcessor: TTestablePathProcessor;
		
	public
		[Setup]
		procedure Setup;
		
		[TearDown]
		procedure TearDown;
		
		[Test]
		[TestCase('BDS Macro', '$(BDS)\lib,C:\Program Files\Embarcadero\Studio\22.0\lib')]
		[TestCase('DELPHI Macro', '$(DELPHI)\source,C:\Program Files\Embarcadero\Studio\22.0\source')]
		[TestCase('BCB Macro', '$(BCB)\include,C:\Program Files\Embarcadero\Studio\22.0\include')]
		procedure TestIdeBaseMacros(const Input, Expected: string);
		
		[Test]
		[TestCase('Platform Win32', '$(Platform)\release,Win32\release')]
		[TestCase('Platform Win64', '$(Platform)\debug,Win64\debug')]
		procedure TestPlatformMacro(const Input, Expected: string);
		
		[Test]
		[TestCase('Config Debug', '$(Config)\temp,Debug\temp')]
		[TestCase('Config Release', '$(Config)\output,Release\output')]
		procedure TestConfigMacro(const Input, Expected: string);
		
		[Test]
		[TestCase('Multiple Macros', '$(BDS)\$(Platform)\$(Config),C:\Program Files\Embarcadero\Studio\22.0\Win32\Debug')]
		procedure TestMultipleMacros(const Input, Expected: string);
		
		[Test]
		[TestCase('No Macros', 'C:\SomeFolder\Test,C:\SomeFolder\Test')]
		[TestCase('Partial Macro', 'C:\$(NotAMacro)\Test,C:\$(NotAMacro)\Test')]
		procedure TestNoMacroReplacement(const Input, Expected: string);
		
		[Test]
		procedure TestEnvironmentVariables;
		
		[Test]
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
	System.StrUtils;

{ TTestProjectOptions }

constructor TTestProjectOptions.Create;
begin
	inherited;
	FOptions := TStringList.Create;
	FOptions.NameValueSeparator := '=';
end;

destructor TTestProjectOptions.Destroy;
begin
	FreeAndNil(FOptions);
	inherited;
end;

function TTestProjectOptions.GetValues(const Name: string): Variant;
begin
	if Name = 'DCC_Define' then
		Result := FDefineValue
	else
		Result := FOptions.Values[Name];
end;

procedure TTestProjectOptions.SetDefineValue(const Value: string);
begin
	FDefineValue := Value;
end;

{ TTestablePathProcessor }

constructor TTestablePathProcessor.Create(const _Prefix: string; const AIdeBasePath: string);
begin
	inherited Create;
	FPrefix := _Prefix;
	FIdeBasePath := ExcludeTrailingPathDelimiter(AIdeBasePath);
	FConfigName := 'Release'; // Default config
	FPlatformName := 'Win32'; // Default platform
	FEnvironment := TStringList.Create;
	FEnvironment.NameValueSeparator := '=';
	
	GetEnvironmentVariables(FEnvironment);
	
	// Create default project options
	FProjectOptions := TTestProjectOptions.Create;
end;

destructor TTestablePathProcessor.Destroy;
begin
	FreeAndNil(FEnvironment);
	FProjectOptions := nil;
	inherited;
end;

procedure TTestablePathProcessor.GetEnvironmentVariables(Strings: TStrings);
var
	EnvStart, EnvCurrent: PChar;
begin
	EnvStart := GetEnvironmentStrings();
	try
		EnvCurrent := EnvStart;
		while EnvCurrent^ <> #0 do begin
			Strings.Add(EnvCurrent);
			Inc(EnvCurrent, StrLen(EnvCurrent) + 1);
		end;
	finally
		FreeEnvironmentStrings(EnvStart);
	end;
end;

procedure TTestablePathProcessor.SetProjectOptions(Options: ITestProjectOptions);
begin
	FProjectOptions := Options;
end;

procedure TTestablePathProcessor.AddProjectDefine(const Name, Value: string);
var
	DefineOptions: TTestProjectOptions;
	DefineList: TStringList;
	DefineString: string;
begin
	DefineOptions := FProjectOptions as TTestProjectOptions;
	DefineList := TStringList.Create;
	try
		DefineList.Delimiter := ';';
		DefineList.StrictDelimiter := True;
		
		// Get existing defines
		if VarToStr(FProjectOptions.Values['DCC_Define']) <> '' then
			DefineList.DelimitedText := VarToStr(FProjectOptions.Values['DCC_Define']);
		
		// Add or update define
		if Value <> '' then
			DefineString := Name + '=' + Value
		else
			DefineString := Name;
			
		DefineList.Add(DefineString);
		// Use semicolon-separated list, not comma-separated
		DefineOptions.SetDefineValue(StringReplace(DefineList.DelimitedText, ',', ';', [rfReplaceAll]));
	finally
		FreeAndNil(DefineList);
	end;
end;

procedure TTestablePathProcessor.ClearProjectDefines;
var
	DefineOptions: TTestProjectOptions;
begin
	DefineOptions := FProjectOptions as TTestProjectOptions;
	DefineOptions.SetDefineValue('');
end;

function TTestablePathProcessor.Process(const _Path: string): string;
const
	IDEBaseMacros: array [0..2] of string = ('BDS', 'DELPHI', 'BCB');
var
	i: Integer;
	EnvName: string;
	EnvValue: string;
	DefineList: TStringList;
	DefineValue: string;
begin
	Result := _Path;

	// Expand the IDE base folder names $([DELPHI,BCB,BDS])
	for i := Low(IDEBaseMacros) to High(IDEBaseMacros) do begin
		Result := ReplaceMacro(Result, IDEBaseMacros[i], FIdeBasePath);
	end;

	// Expand any environment variable macros
	for i := 0 to FEnvironment.Count - 1 do begin
		EnvName := Trim(FEnvironment.Names[i]);
		EnvValue := Trim(FEnvironment.Values[EnvName]);
		if (EnvName <> '') and (EnvValue <> '') then begin
			Result := ReplaceMacro(Result, EnvName, EnvValue);
		end;
	end;

	// Expand project preprocessor constants (DCC_Define) - same logic as real TPathProcessor
	if Assigned(FProjectOptions) then begin
		DefineValue := VarToStr(FProjectOptions.Values['DCC_Define']);
		if DefineValue <> '' then begin
			DefineList := TStringList.Create;
			try
				DefineList.Delimiter := ';';
				DefineList.DelimitedText := DefineValue;
				for i := 0 to DefineList.Count - 1 do begin
					var Define := DefineList[i];
					var EqualPos := Pos('=', Define);
					if EqualPos > 0 then begin
						// Define with value: SYMBOL=VALUE
						var SymbolName := Copy(Define, 1, EqualPos - 1);
						var SymbolValue := Copy(Define, EqualPos + 1, Length(Define));
						Result := ReplaceMacro(Result, SymbolName, SymbolValue);
					end else begin
						// Define without value: SYMBOL (assume '1')
						Result := ReplaceMacro(Result, Define, '1');
					end;
				end;
			finally
				FreeAndNil(DefineList);
			end;
		end;
	end;

	if FPlatformName <> '' then begin
		Result := ReplaceMacro(Result, 'Platform', FPlatformName);
	end;

	if FConfigName <> '' then begin
		Result := ReplaceMacro(Result, 'Config', FConfigName);
	end;

	// Simple relative path handling for testing
	if not ((Length(Result) >= 2) and (Result[2] = ':')) then begin // Not absolute path
		if FPrefix <> '' then begin
			Result := IncludeTrailingPathDelimiter(FPrefix) + Result;
		end;
	end;
end;

class function TTestablePathProcessor.ReplaceMacro(const _str, _oldValue, _newValue: string): string;
var
	replaceVal: string;
begin
	replaceVal := '$(' + _oldValue + ')';
	Result := StringReplace(_str, replaceVal, _newValue, [rfReplaceAll, rfIgnoreCase]);
end;

{ TPathProcessorTest }

procedure TPathProcessorTest.Setup;
begin
	FPathProcessor := TTestablePathProcessor.Create('');
end;

procedure TPathProcessorTest.TearDown;
begin
	FreeAndNil(FPathProcessor);
end;

procedure TPathProcessorTest.TestIdeBaseMacros(const Input, Expected: string);
var
	Result: string;
begin
	Result := FPathProcessor.Process(Input);
	Assert.AreEqual(Expected, Result, Format('Failed to process IDE base macro. Input: %s', [Input]));
end;

procedure TPathProcessorTest.TestPlatformMacro(const Input, Expected: string);
var
	Result: string;
begin
	// Test both Win32 and Win64
	FPathProcessor.PlatformName := 'Win32';
	if Expected.Contains('Win64') then
		FPathProcessor.PlatformName := 'Win64';
		
	Result := FPathProcessor.Process(Input);
	Assert.AreEqual(Expected, Result, Format('Failed to process Platform macro. Input: %s', [Input]));
end;

procedure TPathProcessorTest.TestConfigMacro(const Input, Expected: string);
var
	Result: string;
begin
	// Test both Debug and Release
	FPathProcessor.ConfigName := 'Debug';
	if Expected.Contains('Release') then
		FPathProcessor.ConfigName := 'Release';
		
	Result := FPathProcessor.Process(Input);
	Assert.AreEqual(Expected, Result, Format('Failed to process Config macro. Input: %s', [Input]));
end;

procedure TPathProcessorTest.TestMultipleMacros(const Input, Expected: string);
var
	Result: string;
begin
	Result := FPathProcessor.Process(Input);
	Assert.AreEqual(Expected, Result, Format('Failed to process multiple macros. Input: %s', [Input]));
end;

procedure TPathProcessorTest.TestNoMacroReplacement(const Input, Expected: string);
var
	Result: string;
begin
	Result := FPathProcessor.Process(Input);
	Assert.AreEqual(Expected, Result, Format('Unexpected macro replacement. Input: %s', [Input]));
end;

procedure TPathProcessorTest.TestEnvironmentVariables;
var
	TestPath: string;
	Result: string;
	TempVar: string;
begin
	// Set a test environment variable
	TempVar := 'DRIPGREPPER_TEST_VAR';
	SetEnvironmentVariable(PChar(TempVar), PChar('C:\TestValue'));
	try
		// Recreate processor to pick up new environment variable
		FreeAndNil(FPathProcessor);
		FPathProcessor := TTestablePathProcessor.Create('');
		
		TestPath := '$(' + TempVar + ')\subfolder';
		Result := FPathProcessor.Process(TestPath);
		Assert.AreEqual('C:\TestValue\subfolder', Result, 'Failed to process environment variable macro');
	finally
		SetEnvironmentVariable(PChar(TempVar), nil); // Remove test variable
	end;
end;

procedure TPathProcessorTest.TestProjectDefines;
var
	TestPath: string;
	Result: string;
begin
	// Add test defines
	FPathProcessor.AddProjectDefine('TESTDEFINE', 'TestValue');
	FPathProcessor.AddProjectDefine('SIMPLE_DEFINE', ''); // Empty value should default to '1'
	
	TestPath := '$(TESTDEFINE)\test\$(SIMPLE_DEFINE)';
	Result := FPathProcessor.Process(TestPath);
	Assert.AreEqual('TestValue\test\1', Result, 'Failed to process project defines');
end;

procedure TPathProcessorTest.TestRelativePathHandling;
var
	RelativePath: string;
	Result: string;
begin
	// Create processor with prefix for relative path handling
	FreeAndNil(FPathProcessor);
	FPathProcessor := TTestablePathProcessor.Create('C:\TestProject');
	
	RelativePath := 'relative\path';
	Result := FPathProcessor.Process(RelativePath);
	Assert.AreEqual('C:\TestProject\relative\path', Result, 'Failed to handle relative path correctly');
end;

procedure TPathProcessorTest.TestOriginalBugScenario;
var
	TestPath: string;
	Result: string;
begin
	// Test the original bug scenario: $(BDS)\$(Platform)\release
	// Should expand to: C:\Program Files\Embarcadero\Studio\22.0\Win32\release
	TestPath := '$(BDS)\$(Platform)\release';
	Result := FPathProcessor.Process(TestPath);
	Assert.AreEqual('C:\Program Files\Embarcadero\Studio\22.0\Win32\release', Result, 
		'Failed to process original bug scenario: $(BDS)\$(Platform)\release');
	
	// Test with Win64 platform
	FPathProcessor.PlatformName := 'Win64';
	Result := FPathProcessor.Process(TestPath);
	Assert.AreEqual('C:\Program Files\Embarcadero\Studio\22.0\Win64\release', Result, 
		'Failed to process Win64 platform scenario');
end;

procedure TPathProcessorTest.TestReplaceMacroMethod;
var
	TestStr: string;
	Result: string;
begin
	TestStr := 'This is a $(TEST) string with $(TEST) macros';
	Result := TTestablePathProcessor.ReplaceMacro(TestStr, 'TEST', 'REPLACED');
	Assert.AreEqual('This is a REPLACED string with REPLACED macros', Result, 'ReplaceMacro method failed');
	
	// Test case insensitive
	Result := TTestablePathProcessor.ReplaceMacro('$(test)', 'TEST', 'replaced');
	Assert.AreEqual('replaced', Result, 'ReplaceMacro should be case insensitive');
	
	// Test no replacement
	Result := TTestablePathProcessor.ReplaceMacro('No macros here', 'TEST', 'replaced');
	Assert.AreEqual('No macros here', Result, 'ReplaceMacro should not change string without macros');
end;

end.
