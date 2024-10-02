program DRipGrepperUnittest;

{$IFNDEF TESTINSIGHT}
{$APPTYPE CONSOLE}
{$ENDIF}
{$STRONGLINKTYPES ON}

{$WARN DUPLICATE_CTOR_DTOR OFF}

uses
  {$IFDEF FASTMM4_OPT}
  FastMM4,
  {$ENDIF }
  System.SysUtils,
  {$IFDEF TESTINSIGHT}
  TestInsight.DUnitX,
  {$ELSE}
  DUnitX.Loggers.Console,
  DUnitX.Loggers.Xml.NUnit,
  {$ENDIF }
  DUnitX.TestFramework,
  RipGrepper.Parser.MatchTest in 'RipGrepper.Parser.MatchTest.pas',
  RipGrepper.Data.Matches in '..\src\RipGrepper.Data.Matches.pas',
  RipGrepper.ProcessUtils.Test in 'RipGrepper.ProcessUtils.Test.pas',
  RipGrepper.Tools.ProcessUtils in '..\src\RipGrepper.Tools.ProcessUtils.pas',
  RipGrepper.Helper.Types.Test in 'RipGrepper.Helper.Types.Test.pas',
  RipGrepper.Helper.Types in '..\src\RipGrepper.Helper.Types.pas',
  RipGrepper.Parsers.VimGrepMatchLine in '..\src\RipGrepper.Parsers.VimGrepMatchLine.pas',
  RipGrepper.Common.Constants in '..\src\RipGrepper.Common.Constants.pas',
  ArrayEx in '..\external\ArrayEx.pas',
  RipGrepper.CommandLine.Builder.Test in 'RipGrepper.CommandLine.Builder.Test.pas',
  RipGrepper.Common.Settings.RipGrepParameterSettings in '..\src\RipGrepper.Common.Settings.RipGrepParameterSettings.pas',
  RipGrepper.CommandLine.Builder in '..\src\RipGrepper.CommandLine.Builder.pas',
  RipGrepper.CommandLine.OptionsHelper.Test in 'RipGrepper.CommandLine.OptionsHelper.Test.pas',
  RipGrepper.CommandLine.OptionHelper in '..\src\RipGrepper.CommandLine.OptionHelper.pas',
  RipGrepper.Common.GuiSearchParams in '..\src\RipGrepper.Common.GuiSearchParams.pas',
  RipGrepper.Common.Settings.SearchFormSettingsTest in 'RipGrepper.Common.Settings.SearchFormSettingsTest.pas',
  RipGrepper.Common.Settings.SearchFormSettings in '..\src\RipGrepper.Common.Settings.SearchFormSettings.pas',
  RipGrepper.Common.Settings.Persistable in '..\src\RipGrepper.Common.Settings.Persistable.pas',
  RipGrepper.Common.Settings.SettingVariant in '..\src\RipGrepper.Common.Settings.SettingVariant.pas',
  RipGrepper.Common.Settings.PersistableSettingsTest in 'RipGrepper.Common.Settings.PersistableSettingsTest.pas',
  RipGrepper.Common.Settings.JsonTest in 'RipGrepper.Common.Settings.JsonTest.pas',
  Pkg.Json.DTO in 'Pkg.Json.DTO.pas',
  RootUnit in 'RootUnit.pas',
  RipGrepper.Common.Settings.RipGrepperSettingsTest in 'RipGrepper.Common.Settings.RipGrepperSettingsTest.pas',
  RipGrepper.Common.Settings.RipGrepperSettings in '..\src\RipGrepper.Common.Settings.RipGrepperSettings.pas',
  RipGrepper.CommandLine.OptionStrings in '..\src\RipGrepper.CommandLine.OptionStrings.pas',
  RipGrepper.CommandLine.OptionStrings.Test in 'RipGrepper.CommandLine.OptionStrings.Test.pas';

{$IFNDEF TESTINSIGHT}

var
	runner : ITestRunner;
	results : IRunResults;
	logger : ITestLogger;
	nunitLogger : ITestLogger;
	{$ENDIF}

begin
	{$IFDEF TESTINSIGHT}
	TestInsight.DUnitX.RunRegisteredTests;
	{$ELSE}
	try
		// Check command line options, will exit if invalid
		TDUnitX.CheckCommandLine;
		// Create the test runner
		runner := TDUnitX.CreateRunner;
		// Tell the runner to use RTTI to find Fixtures
		runner.UseRTTI := True;
		// When true, Assertions must be made during tests;
		runner.FailsOnNoAsserts := False;

		// tell the runner how we will log things
		// Log to the console window if desired
		if TDUnitX.Options.ConsoleMode <> TDunitXConsoleMode.Off then begin
			logger := TDUnitXConsoleLogger.Create(TDUnitX.Options.ConsoleMode = TDunitXConsoleMode.Quiet);
			runner.AddLogger(logger);
		end;
		// Generate an NUnit compatible XML File
		nunitLogger := TDUnitXXMLNUnitFileLogger.Create(TDUnitX.Options.XMLOutputFile);
		runner.AddLogger(nunitLogger);

		// Run tests
		results := runner.Execute;
		if not results.AllPassed then
			System.ExitCode := EXIT_ERRORS;

		{$IFNDEF CI}
		// We don't want this happening when running under CI.
		if TDUnitX.Options.ExitBehavior = TDUnitXExitBehavior.Pause then begin
			System.Write('Done.. press <Enter> key to quit.');
			System.Readln;
		end;
		{$ENDIF}
	except
		on E : Exception do
			System.Writeln(E.ClassName, ': ', E.Message);
	end;
	{$ENDIF}

end.
