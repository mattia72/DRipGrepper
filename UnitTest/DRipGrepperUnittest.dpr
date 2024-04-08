program DRipGrepperUnittest;

{$IFNDEF TESTINSIGHT}
{$APPTYPE CONSOLE}
{$ENDIF}
{$STRONGLINKTYPES ON}

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
	RipGrepMatchTest in 'RipGrepMatchTest.pas',
	RipGrepper.Data.Matches in '..\src\RipGrepper.Data.Matches.pas',
	RipGrepper.ProcessUtils.Test in 'RipGrepper.ProcessUtils.Test.pas',
	RipGrepper.Tools.ProcessUtils in '..\src\RipGrepper.Tools.ProcessUtils.pas',
	RipGrepper.Helper.Types.Test in 'RipGrepper.Helper.Types.Test.pas',
	RipGrepper.Helper.Types in '..\src\RipGrepper.Helper.Types.pas',
	RipGrepper.Parsers.VimGrepMatchLine in '..\src\RipGrepper.Parsers.VimGrepMatchLine.pas',
	RipGrepper.Common.Types in '..\src\RipGrepper.Common.Types.pas';

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
