program DRipGrepperUnittest.D12;

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
  {$ENDIF }
  DUnitX.TestFramework,
  RipGrepper.Parser.MatchTest in 'RipGrepper.Parser.MatchTest.pas',
  RipGrepper.Data.Matches in '..\src\RipGrepper.Data.Matches.pas',
  RipGrepper.ProcessUtils.Test in 'RipGrepper.ProcessUtils.Test.pas',
  RipGrepper.Tools.ProcessUtils in '..\src\RipGrepper.Tools.ProcessUtils.pas',
  RipGrepper.Helper.Types.Test in 'RipGrepper.Helper.Types.Test.pas',
  RipGrepper.Helper.Types in '..\src\RipGrepper.Helper.Types.pas',
  RipGrepper.Parsers.VimGrepMatchLine in '..\src\RipGrepper.Parsers.VimGrepMatchLine.pas',
  ArrayEx in '..\external\ArrayEx.pas',
  RipGrepper.CommandLine.Builder.Test in 'RipGrepper.CommandLine.Builder.Test.pas',
  RipGrepper.CommandLine.Builder in '..\src\RipGrepper.CommandLine.Builder.pas',
  RipGrepper.CommandLine.OptionsHelper.Test in 'RipGrepper.CommandLine.OptionsHelper.Test.pas',
  RipGrepper.CommandLine.OptionHelper in '..\src\RipGrepper.CommandLine.OptionHelper.pas',
  Pkg.Json.DTO in 'Pkg.Json.DTO.pas',
  RootUnit in 'RootUnit.pas',
  RipGrepper.CommandLine.OptionStrings in '..\src\RipGrepper.CommandLine.OptionStrings.pas',
  RipGrepper.CommandLine.OptionStrings.Test in 'RipGrepper.CommandLine.OptionStrings.Test.pas',
  RipGrepper.Settings.RipGrepParameterSettings in '..\src\Settings\RipGrepper.Settings.RipGrepParameterSettings.pas',
  RipGrepper.Settings.Persistable in '..\src\Settings\RipGrepper.Settings.Persistable.pas',
  RipGrepper.Common.GuiSearchParams in '..\src\Common\RipGrepper.Common.GuiSearchParams.pas',
  RipGrepper.Settings.RipGrepperSettings in '..\src\Settings\RipGrepper.Settings.RipGrepperSettings.pas',
  RipGrepper.Common.SimpleTypes in '..\src\Common\RipGrepper.Common.SimpleTypes.pas',
  RipGrepper.Helper.MemIniFile in '..\src\Helper\RipGrepper.Helper.MemIniFile.pas',
  RipGrepper.Settings.ExtensionSettings in '..\src\Settings\RipGrepper.Settings.ExtensionSettings.pas',
  RipGrepper.Tools.FileUtils in '..\src\Tools\RipGrepper.Tools.FileUtils.pas',
  RipGrepper.Common.Constants in '..\src\Common\RipGrepper.Common.Constants.pas',
  RipGrepper.Common.SearchTextWithOptions in '..\src\Common\RipGrepper.Common.SearchTextWithOptions.pas',
  RipGrepper.Common.SearchParamsWithOptionsTest in 'RipGrepper.Common.SearchParamsWithOptionsTest.pas',
  RipGrepper.Common.Interfaces in '..\src\Common\RipGrepper.Common.Interfaces.pas',
  RipGrepper.Settings.SettingVariant in '..\src\Settings\RipGrepper.Settings.SettingVariant.pas',
  RipGrepper.Tools.Replacer.Test in 'RipGrepper.Tools.Replacer.Test.pas',
  RipGrepper.Tools.Replacer in '..\src\Tools\RipGrepper.Tools.Replacer.pas',
  RipGrepper.OpenWith.RunnerTest in 'RipGrepper.OpenWith.RunnerTest.pas',
  RipGrepper.OpenWith.Params in '..\src\OpenWith\RipGrepper.OpenWith.Params.pas',
  RipGrepper.OpenWith.Runner in '..\src\OpenWith\RipGrepper.OpenWith.Runner.pas',
  RipGrepper.Helper.UI.DarkMode in '..\src\RipGrepper.Helper.UI.DarkMode.pas',
  RipGrepper.Settings.FilePersister in '..\src\Settings\RipGrepper.Settings.FilePersister.pas',
  RipGrepper.Settings.SettingsDictionary in '..\src\Settings\RipGrepper.Settings.SettingsDictionary.pas',
  RipGrepper.Settings.PersistableSettingsTest in 'RipGrepper.Settings.PersistableSettingsTest.pas',
  RipGrepper.Settings.SettingsDictionaryTest in 'RipGrepper.Settings.SettingsDictionaryTest.pas',
  RipGrepper.Settings.TestOwnerSettings in 'RipGrepper.Settings.TestOwnerSettings.pas',
  RipGrepper.Settings.SettingVariantTest in 'RipGrepper.Settings.SettingVariantTest.pas',
  RipGrepper.Settings.NoodeLookSettingsTest in 'RipGrepper.Settings.NoodeLookSettingsTest.pas',
  RipGrepper.Settings.RipGrepperSettingsTest in 'RipGrepper.Settings.RipGrepperSettingsTest.pas',
  RipGrepper.Settings.SearchFormSettingsTest in 'RipGrepper.Settings.SearchFormSettingsTest.pas',
  RipGrepper.Settings.JsonTest in 'RipGrepper.Settings.JsonTest.pas',
  RipGrepper.Settings.MemIniFileHelperTest in 'RipGrepper.Settings.MemIniFileHelperTest.pas',
  RipGrepper.Common.ParsedObject in '..\src\Common\RipGrepper.Common.ParsedObject.pas',
  RipGrepper.Tools.DebugUtils in '..\src\Tools\RipGrepper.Tools.DebugUtils.pas',
  RipGrepper.Settings.AppSettings in '..\src\Settings\RipGrepper.Settings.AppSettings.pas',
  RipGrepper.OpenWith.Constants in '..\src\OpenWith\RipGrepper.OpenWith.Constants.pas',
  RipGrepper.Common.IOTAUtils in '..\src\Common\RipGrepper.Common.IOTAUtils.pas',
  RipGrepper.Tools.LockGuard in '..\src\Tools\RipGrepper.Tools.LockGuard.pas',
  RipGrepper.Settings.FontColors in '..\src\Settings\RipGrepper.Settings.FontColors.pas',
  RipGrepper.Settings.RipGrepSettingsTest in 'RipGrepper.Settings.RipGrepSettingsTest.pas',
  RipGrepper.Settings.OpenWithSettingsTest in 'RipGrepper.Settings.OpenWithSettingsTest.pas',
  RipGrepper.Settings.OpenWithSettings in '..\src\Settings\RipGrepper.Settings.OpenWithSettings.pas',
  RipGrepper.Settings.FontColorsTest in 'RipGrepper.Settings.FontColorsTest.pas',
  RipGrepper.Common.ArrayExTest in 'RipGrepper.Common.ArrayExTest.pas',
  RipGrepper.Data.HistoryItemObjectTest in 'RipGrepper.Data.HistoryItemObjectTest.pas',
  RipGrepper.Helper.SettingStoreBehaviours in '..\src\Helper\RipGrepper.Helper.SettingStoreBehaviours.pas',
  RipGrepper.Helper.StreamReaderWriter in '..\src\Helper\RipGrepper.Helper.StreamReaderWriter.pas';

// This comment has to guard the following IFDEF, that may be delted by delphi, if new unit added to the project
{$IFNDEF TESTINSIGHT}

var
	runner : ITestRunner;
	results : IRunResults;
	logger : ITestLogger;
	nunitLogger : ITestLogger;
	{$ENDIF}

begin

	TFileUtils.DeleteTempDirectory('DRipGrepperUnittest.D12.*', True);

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

	TFileUtils.DeleteTempDirectory('DRipGrepperUnittest.D12.*', True);
end.
