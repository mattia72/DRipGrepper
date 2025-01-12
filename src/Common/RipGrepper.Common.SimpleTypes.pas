unit RipGrepper.Common.SimpleTypes;

interface

uses
	System.Classes,
	System.SysUtils;

type
	TParserType = (ptEmpty, ptRipGrepSearch, ptRipGrepPrettySearch, ptRipGrepStats, ptRipGrepVersion, ptRipGrepError, ptRipGrepHelp);
	// TODO : use this instead of IsError IsStats
	EParsedLineType = (pltErrorLine, pltStatsLine, pltContexLine);
	TParsedLineType = set of EParsedLineType;

	TFileNameType = (ftAbsolute, ftRelative);

	TRipGrepArguments = TStringList;

	{$SCOPEDENUMS ON}
	EGuiReplaceMode = (grmRGReplace, grmEditEnabled, grmActive, grmSaveEnabled, grmCaseSensitive, grmUseRegex);
	EFilterMode = (fmFilterFile, fmFilterText, fmCaseSensitive, fmUseRegex);

	TGuiReplaceModes = set of EGuiReplaceMode;
	TFilterModes = set of EFilterMode;

	EGuiOption = (soNotSet = 0, soMatchCase = 1, soMatchWord = 2, soUseRegex = 3);

	TSearchOptionSet = set of EGuiOption;

	ESkipFileReplaceException = class(Exception);
	EFileOpenException = class(Exception);

	TErrorCounters = record
		FSumOfErrors : Integer;
		FParserErrors : Integer;
		FStatLineCount : Integer;
		FIsNoOutputError : Boolean;
		FIsRGReportedError : Boolean;
		procedure Reset;
	end;
 	{$SCOPEDENUMS OFF}
const
	GUI_SEARCH_PARAMS : TArray<EGuiOption> = [
	{ } EGuiOption.soMatchCase,
	{ } EGuiOption.soMatchWord,
	{ } EGuiOption.soUseRegex];

implementation

procedure TErrorCounters.Reset;
begin
	FSumOfErrors := 0;
	FParserErrors := 0;
	FStatLineCount := 0;
	FIsNoOutputError := False;
	FIsRGReportedError := False;
end;

end.
