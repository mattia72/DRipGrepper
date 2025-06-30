unit RipGrepper.Common.SimpleTypes;

interface

uses
	System.Classes,
	System.SysUtils,
	ArrayEx,
	Spring;

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

	EDelphiIDESearchContext = (
		{ } dicNotSet = -1,
		{ } dicActiveFile = 0,
		{ } dicOpeneFiles = 1,
		{ } dicProjectFiles = 2,
		{ } dicPath = 3
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
		SearchTextHist : TArrayEx<string>;
		SearchOptions : TSearchOptionSet;
		IsReplaceMode : Boolean;
		ReplaceText : string;
		ReplaceTextHist : TArrayEx<string>;
		ExtensionContext : EDelphiIDESearchContext;
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
		function IsEmpty() : Boolean;

		private
			FSearchText : string;
			function GetSearchText() : string;
			procedure SetSearchText(const Value : string);

		public
			function ToString() : string;
			property SearchText : string read GetSearchText write SetSearchText;
	end;

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

	TNullableString = Nullable<string>;
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

function TSearchFormCtrlValueProxy.GetSearchText() : string;
begin
	Result := FSearchText;
end;

function TSearchFormCtrlValueProxy.IsEmpty() : Boolean;
begin
	Result := SearchText.IsEmpty and SearchTextHist.IsEmpty
	{ } and SearchPath.IsEmpty and SearchPathHist.IsEmpty
	{ } and FileMasks.IsEmpty and FileMasksHist.IsEmpty
	{ } and ReplaceText.IsEmpty and ReplaceTextHist.IsEmpty;
end;

procedure TSearchFormCtrlValueProxy.SetSearchText(const Value : string);
begin
	var
	dbgMsg := TDebugMsgBeginEnd.New('TSearchFormCtrlValueProxy.SetSearchText');
	dbgMsg.MsgFmt('Value=%s', [Value]);
	FSearchText := Value;
end;

function TSearchFormCtrlValueProxy.ToString() : string;
begin
	Result := Format('SearchText=%s, ' + CRLF +
		// 'SearchOptions=%s,' + CRLF +
		'IsReplaceMode=%s,' + CRLF + ' ReplaceText=%s,' + CRLF + ' ExtensionContext=%d,' + CRLF + ' SearchPath=%s,' + CRLF +
		' FileMasks=%s,' + CRLF + ' IsHiddenChecked=%s,' + CRLF + ' IsNoIgnoreChecked=%s,' + CRLF + ' Encoding=%s,' + CRLF +
		' IsPrettyChecked=%s,' + CRLF + ' LineContext=%d', [FSearchText,
		// SearchOptions.ToString,
		BoolToStr(IsReplaceMode, True), ReplaceText, Ord(ExtensionContext), SearchPath, FileMasks, BoolToStr(IsHiddenChecked, True),
		BoolToStr(IsNoIgnoreChecked, True), Encoding, BoolToStr(IsPrettyChecked, True), LineContext]);
end;

end.
