unit RipGrepper.Common.SimpleTypes;

interface

uses
	System.Classes;

type
	TParserType = (ptEmpty, ptRipGrepSearch, ptRipGrepPrettySearch, ptRipGrepVersion, ptRipGrepError, ptRipGrepHelp);
	TFileNameType = (ftAbsolute, ftRelative);

	TRipGrepArguments = TStringList;

	{$SCOPEDENUMS ON}
	EGuiReplaceMode = (grmRGReplace, grmEditEnabled, grmActive, grmSaveEnabled);
	EFilterMode = (fmFilterFile, fmFilterText, fmCaseSensitive, fmRegex);
	{$SCOPEDENUMS OFF}
	TGuiReplaceModes = set of EGuiReplaceMode;
    TFilterModes = set of EFilterMode;
implementation

end.
