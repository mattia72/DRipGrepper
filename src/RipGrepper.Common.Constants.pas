unit RipGrepper.Common.Constants;

interface

uses
	System.Classes,
	Vcl.ComCtrls,
	ArrayEx,
	System.Generics.Defaults,
	Vcl.Graphics;

const
	APPNAME = 'DRipGrepper';
	EXTENSION_NAME = 'DRipExtension';
	EXTENSION_MENU_CAPTION = 'DRipGrepper';

    EXPERT_GRPBX_CAPTIONS = 'Expert Settings';

	FORMAT_VERSION_INFO = '%s v%d.%d.%d-beta';
	FORMAT_VERSION_INFO_IN_STATUSBAR = '%s   ';

	SHORTCUT_DRIPGREPPER = 'Umsch+Alt+R';

	CR = #13;
	LF = #10;
	CRLF = sLineBreak;
	TAB = #9;
	SPACE = #32;
	ESC_CHAR = #$01B;
	ESC = ESC_CHAR + '\[';

	BUFF_LENGTH = 1024; // Todo: put into settings

	MAX_HISTORY_COUNT = 20;

	RIPGREP_ERROR = 1;
	RIPGREP_NO_MATCH = 2;

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

    GROUPBOX_EXPERT_HEIGHT = 135;

	DT_ALIGN : array [TAlignment] of TTextFormats = (
		{ } tfLeft,
		{ } tfRight,
		{ } tfCenter);
	ALL_ALPHANUMERIC_CHARS = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz1234567890';

	RG_PROCESSING_LINE_COUNT_LIMIT = 10000; // Todo: put into settings
	RG_NECESSARY_PARAMS : TArray<string> = [
	{ } '--vimgrep',
	{ } '--line-buffered', // some big search couldn't be catched without this
	{ } '--pretty',
	{ } '--no-ignore-parent',
	{ } '--follow',
	{ } '--crlf'];

	RG_MATCH_LINE_REGEX = '^(?<drive>\w:)?(?<path>.+?):(?<row>\d+):(?<col>\d+):(?<text>.+)$';

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

	RG_HELP_LONG_PARAM_REGEX = '(?<long>--[\-a-zA-Z0-9]+)';
	RG_HELP_LINE_REGEX = '^\s*(?<short>-[a-zA-Z.0-9])?(?<comma>, )?(?<long>--[\-a-zA-Z0-9]+)(?<value>=[\-A-Z]+)?\s*(?<desc>.*)$';

	RG_ARG_SEARCH_PATH = 'SearchPath';
	RG_ARG_SEARCH_TEXT = 'SearchText';
	RG_ARG_OPTIONS = 'Options';

	RG_INI_KEY_RGPATH = 'Path';

	RG_PARSE_ERROR = 'rg.exe result line couldn''t parsed.';

	WB = '\b';
	RG_PARAM_REGEX_IGNORE_CASE = '-i|--ignore-case';
	RG_PARAM_REGEX_CASE_SENSITIVE = '-s|--case-sensitive';
	RG_PARAM_REGEX_FIXED_STRINGS = '-F|--fixed-strings';
	RG_PARAM_REGEX_WORD_REGEX = '-w|--word-regexp';
	RG_PARAM_REGEX_GLOB = '-g|--glob';
	RG_PARAM_END = '--';

type

	TParserType = (ptEmpty, ptRipGrepSearch, ptRipGrepPrettySearch, ptRipGrepVersion, ptRipGrepError, ptRipGrepHelp);
	TFileNameType = (ftAbsolute, ftRelative);
	TColumnIndex = (ciFile, ciRow, ciCol, ciText, ciMatchText, ciTextAfterMatch, ciRowNr);
	TRipGrepArguments = TStringList;

implementation

uses
	System.SysUtils;

end.
