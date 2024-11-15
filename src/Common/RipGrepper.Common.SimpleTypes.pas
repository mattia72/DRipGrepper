unit RipGrepper.Common.SimpleTypes;

interface

uses
	System.Classes, System.SysUtils;

type
	TParserType = (ptEmpty, ptRipGrepSearch, ptRipGrepPrettySearch, ptRipGrepVersion, ptRipGrepError, ptRipGrepHelp);
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


implementation

end.
