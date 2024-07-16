unit RipGrepper.Common.Constants;

interface

uses
	System.Classes,
	Vcl.ComCtrls,
	ArrayEx,
	System.Generics.Defaults,
	Vcl.Graphics,
	Vcl.Menus;

const
	APPNAME = 'DRipGrepper';
	EXTENSION_NAME = 'DRipExtension';
	CAPTION_EXTENSION_MENU = 'DRipGrepper';
	HOME_PAGE = 'https://github.com/mattia72/DRipGrepper';

	CAPTION_GRPBX_EXPERT_MODE = 'Expert Settings';

	WWW_LINK_RG_MAN_PAGE = 'https://www.mankier.com/1/rg#Options';
	WWW_LINK_GLOBBING_HELP = 'https://www.w3schools.com/git/git_ignore.asp'; // https://www.mankier.com/5/gitignore

	FORMAT_VERSION_INFO = 'v%d.%d.%d-beta';
	FORMAT_NAME_VERSION_INFO = '%s ' + FORMAT_VERSION_INFO;
	FORMAT_VERSION_INFO_IN_STATUSBAR = '%s   ';

	CR = #13;
	LF = #10;
	CRLF = sLineBreak;
	TAB = #9;
	SPACE = #32;
	ESC_CHAR = #$01B;
	ESC = ESC_CHAR + '\[';
	SEARCH_PATH_SEPARATOR = ';';

	BUFF_LENGTH = 1024; // Todo: put into settings

	MAX_HISTORY_COUNT = 20;

	DRAW_RESULT_ON_EVERY_LINE_COUNT = 100; // Todo: put into settings
	DRAW_RESULT_UNTIL_FIRST_LINE_COUNT = 100;

	LISTVIEW_TYPES : TArray<TViewStyle> = [vsList, vsIcon, vsReport, vsSmallIcon];
	LISTVIEW_TYPE_TEXTS : TArray<string> = ['List', 'Icon', 'Report', 'SmallIcon'];

	IMG_IDX_SHOW_ABS_PATH = 11;
	IMG_IDX_SHOW_RELATIVE_PATH = 12;
	IMG_IDX_SHOW_FILE_ICON_TRUE = 5;
	IMG_IDX_SHOW_FILE_ICON_FALSE = 2;

	IMG_IDX_UNSORTED = 3;
	IMG_IDX_DESCENDING_SORTED = 4;
	IMG_IDX_ASCENDING_SORTED = 5;

	PNL_STATTS_IDX = 0;
	PNL_STATUS_IDX = 1;
	PNL_MESSAGE_IDX = 2;

	LV_IMG_IDX_OK = 0;
	LV_IMG_IDX_ERROR = 1;
	LV_IMG_IDX_INFO = 2;

	DT_ALIGN : array [TAlignment] of TTextFormats = (
		{ } tfLeft,
		{ } tfRight,
		{ } tfCenter);
	ALL_ALPHANUMERIC_CHARS = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz1234567890';

	RG_PROCESSING_LINE_COUNT_LIMIT = 10000; // Todo: put into settings

	RG_MATCH_LINE_REGEX = '^(?<drive>\w:)?(?<path>.+?):(?<row>\d+):(?<col>\d+):(?<text>.+)$';
	RG_MATCH_LINE_CONTEXT_REGEX = '^(?<drive>\w:)?(?<path>.+?)-(?<row>\d+)-(?<text>.*)$';

	RG_PRETTY_BLUE = ESC + '0m' + ESC + '36m';
	RG_PRETTY_GREEN = ESC + '0m' + ESC + '32m';
	RG_PRETTY_BOLD_RED = ESC + '1m' + ESC + '31m';
	RG_PRETTY_RESET = ESC + '0m';

	RG_MATCH_PRETTY_LINE_REGEX = '^' +
	{ } RG_PRETTY_BLUE + '(?<drive>\w:)?(?<path>.+?)' +
	{ } RG_PRETTY_RESET + ':' +
	{ } RG_PRETTY_GREEN + '(?<row>\d+)' +
	{ } RG_PRETTY_RESET + ':' +
	{ } RG_PRETTY_RESET + '(?<col>\d+)' +
	{ } RG_PRETTY_RESET + ':(?<text_before_match>.+?)?' +
	{ } RG_PRETTY_RESET + RG_PRETTY_BOLD_RED + '(?<match_text>.+)' +
	{ } RG_PRETTY_RESET + '(?<text_after_match>.+?)?$';

	RG_MATCH_PRETTY_LINE_CONTEXT_REGEX = '^' +
	{ } RG_PRETTY_BLUE + '(?<drive>\w:)?(?<path>.+?)' +
	{ } RG_PRETTY_RESET + '-' +
	{ } RG_PRETTY_GREEN + '(?<row>\d+)' +
	{ } RG_PRETTY_RESET + '-' + '(?<text>.*)$';

	RG_HELP_LONG_PARAM_REGEX = '(?<long>--[\-a-zA-Z0-9]+)';
	RG_HELP_LINE_REGEX = '^\s*(?<short>-[a-zA-Z.0-9])?(?<comma>, )?(?<long>--[\-a-zA-Z0-9]+)(?<value>=[\-A-Z]+)?\s*(?<desc>.*)$';

	RG_CONTEXT_SEPARATOR = '--';

	RG_ARG_SEARCH_PATH = 'SearchPath';
	RG_ARG_SEARCH_TEXT = 'SearchText';
	RG_ARG_OPTIONS = 'Options';

	RG_INI_KEY_RGPATH = 'Path';

	WB = '\b'; // word boundary

	RG_PARAM_REGEX_IGNORE_CASE = '-i|--ignore-case';
	RG_PARAM_REGEX_CASE_SENSITIVE = '-s|--case-sensitive';
	RG_PARAM_REGEX_FIXED_STRINGS = '-F|--fixed-strings';
	RG_PARAM_REGEX_WORD_REGEX = '-w|--word-regexp';
	RG_PARAM_REGEX_GLOB = '-g|--glob';
	RG_PARAM_REGEX_HIDDEN = '-\.|--hidden';
	RG_PARAM_REGEX_NO_IGNORE = '-u|--no-ignore';
	RG_PARAM_REGEX_PRETTY = '-p|--pretty';
	RG_PARAM_REGEX_CONTEXT = '-C|--context';
	RG_PARAM_END = '--';

	RG_GUI_SET_PARAMS : TArray<string> = [
	{ } RG_PARAM_REGEX_IGNORE_CASE,
	{ } RG_PARAM_REGEX_CASE_SENSITIVE,
	{ } RG_PARAM_REGEX_FIXED_STRINGS,
	{ } RG_PARAM_REGEX_GLOB,
	{ } RG_PARAM_REGEX_HIDDEN,
	{ } RG_PARAM_REGEX_NO_IGNORE,
	{ } RG_PARAM_REGEX_PRETTY,
	{ } RG_PARAM_REGEX_CONTEXT,
	{ } RG_PARAM_END
	{ } ];

	RG_NECESSARY_PARAMS : TArray<string> = [
	{ } '--vimgrep',
	{ } '--line-buffered', // some big search couldn't be catched without this
	{ } // '--pretty',
	{ } '--no-ignore-parent',
	{ } '--follow',
	{ } '--crlf'];

	RG_PARAM_SHORT_INDEX = 0;
	RG_PARAM_LONG_INDEX = 1;

	RG_ERROR = 2;
	RG_NO_MATCH = 1;
	RG_SUCCESS = 0;

	ICON_RESOURCE_DLL = 'System32\shell32.dll';
	ICON_IDX_ERROR = 234;
	ICON_IDX_PARSE_ERROR = 66;

	RG_ERROR_MSG_PREFIX = 'rg:';
	RG_PARSE_ERROR = 'not parsed output';
	RG_ENDED_ERROR = ' failed with exit code: ';
	RG_HAS_NO_OUTUT = ' has no output.';

	EXT_SEARCH_ACTIVE_FILE = 0;
	EXT_SEARCH_OPEN_FILES = 1;
	EXT_SEARCH_PROJECT_FILES = 2;
	EXT_SEARCH_GIVEN_PATH = 3;

	SHOW_CMD_IN_ONE_LINE = 'Show command in one line';
	SHOW_CMD_IN_SEPARATE_LINES = 'Show command in lines';
	FORMAT_RIPGREP_EXE_NOT_FOUND = 'RipGrep executable (rg.exe) not found.' + CRLF +
	{ } 'Check your PATH or in settings file: ' + CRLF +
	{ } '%s';

	MAX_COMMAND_LINE_LENGTH = 32767;

type

	TParserType = (ptEmpty, ptRipGrepSearch, ptRipGrepPrettySearch, ptRipGrepVersion, ptRipGrepError, ptRipGrepHelp);
	TFileNameType = (ftAbsolute, ftRelative);
	EColumnIndex = (ciFile, ciRow, ciCol, ciText, ciMatchText, ciTextAfterMatch, ciRowNr);
	TRipGrepArguments = TStringList;

	{$SCOPEDENUMS ON}
	EGuiOption = (soNotSet = 0, soMatchCase = 1, soMatchWord = 2, soUseRegex = 3);
	{$SCOPEDENUMS OFF}

	TDefaults = class
		class var
			EXT_DEFAULT_SHORTCUT_SEARCH : string;
			class constructor Create;
	end;

const
	GUI_SEARCH_PARAMS : TArray<EGuiOption> = [
	{ } EGuiOption.soMatchCase,
	{ } EGuiOption.soMatchWord,
	{ } EGuiOption.soUseRegex];

implementation

uses
	System.SysUtils;

class constructor TDefaults.Create;
begin
	inherited;
	EXT_DEFAULT_SHORTCUT_SEARCH := ShortCutToText(ShortCut(Word('R'), [ssShift, ssAlt]));
end;

end.
