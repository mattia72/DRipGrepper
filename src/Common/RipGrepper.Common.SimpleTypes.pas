unit RipGrepper.Common.SimpleTypes;

interface

uses
	System.Classes, System.SysUtils;

type
	TParserType = (ptEmpty, ptRipGrepSearch, ptRipGrepPrettySearch, ptRipGrepStats, ptRipGrepVersion, ptRipGrepError, ptRipGrepHelp);
	// TODO : use this instead of IsError IsStats
	EParsedLineType = (pltErrorLine, pltStatsLine, pltContexLine);
	TParsedLineType  = set of  EParsedLineType;

	TFileNameType = (ftAbsolute, ftRelative);

	TRipGrepArguments = TStringList;

	{$SCOPEDENUMS ON}
	EGuiReplaceMode = (grmRGReplace, grmEditEnabled, grmActive, grmSaveEnabled);
	EFilterMode = (fmFilterFile, fmFilterText, fmCaseSensitive, fmUseRegex);
	{$SCOPEDENUMS OFF}
	TGuiReplaceModes = set of EGuiReplaceMode;
    TFilterModes = set of EFilterMode;

    ESkipFileReplaceException = class(Exception);
    EFileOpenException = class(Exception);

	TErrorCounters = record
		FSumOfErrors : Integer;
		FParserErrors : Integer;
		FIsNoOutputError : Boolean;
		FIsRGReportedError : Boolean;
		procedure Reset;
	end;


implementation

procedure TErrorCounters.Reset;
begin
	FSumOfErrors := 0;
	FParserErrors := 0;
	FIsNoOutputError := False;
	FIsRGReportedError := False;
end;

end.
