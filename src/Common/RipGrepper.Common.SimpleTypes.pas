unit RipGrepper.Common.SimpleTypes;

interface

uses
	System.Classes,
	System.SysUtils,
	ArrayEx,
	Spring;

type
	TParserType = (ptEmpty, ptRipGrepSearch, ptRipGrepPrettySearch, ptRipGrepJson, ptRipGrepStats, ptRipGrepVersion, ptRipGrepError,
		ptRipGrepHelp);
	// TODO : use this instead of IsError IsStats
	EParsedLineType = (pltErrorLine, pltStatsLine, pltContexLine);
	TParsedLineType = set of EParsedLineType;

	TFileNameType = (ftAbsolute, ftRelative);

	{$SCOPEDENUMS ON}
	EGuiReplaceMode = (grmRGReplace, grmRgJson, grmEditEnabled, grmActive { GuiReplaceModeActive } ,
		{ } grmSaveEnabled,
		{ } grmCaseSensitive, grmUseRegex);
	EFilterMode = (fmFilterFile, fmFilterText, fmCaseSensitive, fmUseRegex);

	TGuiReplaceModes = set of EGuiReplaceMode;
	TFilterModes = set of EFilterMode;

	EGuiOption = (soNotSet = 0, soMatchCase = 1, soMatchWord = 2, soUseRegex = 3);

	TSearchOptionSet = set of EGuiOption;

	TShellType = (stNone = -1, stPowershell = 0, stCmd = 1);

	{$SCOPEDENUMS OFF}

	TSearchOptionToRgOptions = record
		SearchOption : TSearchOptionSet;
		RgOptions : TArray<string>;
	end;

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

	EReplaceMode = (rmUseRegex, rmIgnoreCase);
	TReplaceModes = set of EReplaceMode;
	ESaveReplacementResult = (srrDone, srrCancel, srrError);
	ESaveModifiedFilesResults = (smfrNotSet, smfrActSaved, smfrAllSaved, smfrActNotSaved, smfrCancel, smfrError);

	TNullableString = Nullable<string>;

	EMemoTextFormat = (mtfOneLine, mtfSeparateLines);
	ESearchFormLayout = (sflReplace, sflExtension, sflExpert);

	TSearchFormLayout = set of ESearchFormLayout;

const
	GUI_SEARCH_PARAMS : TArray<EGuiOption> = [
	{ } EGuiOption.soMatchCase,
	{ } EGuiOption.soMatchWord,
	{ } EGuiOption.soUseRegex];

implementation

uses
	RipGrepper.Tools.DebugUtils,
	RipGrepper.Common.Constants;

procedure TErrorCounters.Reset;
begin
	FSumOfErrors := 0;
	FParserErrors := 0;
	FStatLineCount := 0;
	FIsNoOutputError := False;
	FIsRGReportedError := False;
end;

end.
