unit RipGrepper.Common.SimpleTypes;

interface

uses
	System.Classes,
	System.SysUtils,
	ArrayEx;

const
	EXT_SEARCH_NOT_SET = -1;
	EXT_SEARCH_ACTIVE_FILE = 0;
	EXT_SEARCH_OPEN_FILES = 1;
	EXT_SEARCH_PROJECT_FILES = 2;
	EXT_SEARCH_GIVEN_PATH = 3;

type
	TParserType = (ptEmpty, ptRipGrepSearch, ptRipGrepPrettySearch, ptRipGrepStats, ptRipGrepVersion, ptRipGrepError, ptRipGrepHelp);
	// TODO : use this instead of IsError IsStats
	EParsedLineType = (pltErrorLine, pltStatsLine, pltContexLine);
	TParsedLineType = set of EParsedLineType;

	TFileNameType = (ftAbsolute, ftRelative);

	{$SCOPEDENUMS ON}

	EGuiReplaceMode = (grmRGReplace, grmEditEnabled, grmActive, grmSaveEnabled, grmCaseSensitive, grmUseRegex);
	EFilterMode = (fmFilterFile, fmFilterText, fmCaseSensitive, fmUseRegex);

	TGuiReplaceModes = set of EGuiReplaceMode;
	TFilterModes = set of EFilterMode;

	EGuiOption = (soNotSet = 0, soMatchCase = 1, soMatchWord = 2, soUseRegex = 3);

	TSearchOptionSet = set of EGuiOption;

	ERipGrepperExtensionContext = (
		{ } rgecNotSet = -1,
		{ } rgecActiveFile = EXT_SEARCH_ACTIVE_FILE,
		{ } rgecOpeneFiles = EXT_SEARCH_OPEN_FILES,
		{ } rgecProjectFiles = EXT_SEARCH_PROJECT_FILES,
		{ } rgecPath = EXT_SEARCH_GIVEN_PATH
		{ } );

	TShellType = (stNone = -1, stPowershell = 0, stCmd = 1);

	{$SCOPEDENUMS OFF}

	TSearchOptionToRgOptions = record
		SearchOption : TSearchOptionSet;
		RgOptions : TArray<string>;
	end;

	ESkipFileReplaceException = class(Exception);
	EFileOpenException = class(Exception);

	TSearchFormCtrlValueProxy = record
		SearchText : string;
		SearchTextHist : TArrayEx<string>;
		SearchOptions : TSearchOptionSet;
		IsReplaceMode : Boolean;
		ReplaceText : string;
		ReplaceTextHist : TArrayEx<string>;
		ExtensionContext : ERipGrepperExtensionContext;
		SearchPath : string;
		SearchPathHist : TArrayEx<string>;
		FileMasks : string;
		FileMasksHist : TArrayEx<string>;

		IsHiddenChecked : Boolean;
		IsNoIgnoreChecked : Boolean;
		Encoding : string;
		EncodingItems : TArrayEx<string>;
		IsPrettyChecked : Boolean;
		LineContext : integer;

		AdditionalExpertOptions : string;
		AdditionalExpertOptionsHist : TArrayEx<string>;
	end;

	TErrorCounters = record
		FSumOfErrors : Integer;
		FParserErrors : Integer;
		FStatLineCount : Integer;
		FIsNoOutputError : Boolean;
		FIsRGReportedError : Boolean;
		procedure Reset;
	end;

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
