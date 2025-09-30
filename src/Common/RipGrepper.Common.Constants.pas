unit RipGrepper.Common.Constants;

interface

uses
	System.Classes,
	Vcl.ComCtrls,
	ArrayEx,
	System.Generics.Defaults,
	Vcl.Graphics,
	Vcl.Menus,
	Winapi.Messages,
	RipGrepper.Common.SimpleTypes;

const

	COMPILER_VERSION_DELPHI_11 = 35; // Delphi 11 Alexandria
	COMPILER_VERSION_DELPHI_12 = 36; // Delphi 12.1 Athens
	// COMPILER_VERSION_DELPHI_12_1 = 36; // Delphi 12.1 Athens
	// COMPILER_VERSION_DELPHI_12_3 = 36; // Delphi 12.3 Athens Update

	APP_PLATFORM = {$IFDEF WIN64} 'x64' {$ELSE} 'x86' {$ENDIF};
	IS_EXTENSION = {$IFNDEF STANDALONE} TRUE {$ELSE} FALSE {$ENDIF};
	IS_STANDALONE = {$IFDEF STANDALONE} TRUE {$ELSE} FALSE {$ENDIF};

	RG_EXE = 'rg.exe';

	DRIPGREPPER_APPNAME = 'DRipGrepper';
	EXTENSION_NAME = 'DRipExtensions';

	{$IFDEF STANDALONE}
	APPNAME = DRIPGREPPER_APPNAME;
	{$ELSE}
	APPNAME = EXTENSION_NAME;
	EXTENSION_MENU_ROOT_TEXT = '&' + EXTENSION_NAME + '...';
	{$ENDIF}
	MENU_ITEM_OPEN_WITH = 'Open With...';
	MENU_ITEM_SEARCH_WITH_DRIPGREPPER = 'Search with DRipGrepper...';
	MENU_ITEM_SETTINGS = 'Settings...';

	SEARCH_HISTORY_DRH = 'SearchHistory.drh';
	BAK_FILE_EXTENSION = '.bak';
	FONTS_AND_COLORS_CAPTION = 'Appearance';
	ABOUT_CAPTION = 'About';

	DRIPGREPPER_WIZARD_NAME = 'DRipGrepper';
	HOME_PAGE = 'https://github.com/mattia72/DRipGrepper';

	VSCODE_RG_EXE_FIND_PATH = 'resources\app';

	CAPTION_GRPBX_EXPERT_MODE = 'Expert Settings';

	WWW_LINK_RG_MAN_PAGE = 'https://www.mankier.com/1/rg#Options';
	WWW_LINK_RG_REPLACE_MAN_PAGE = 'https://www.mankier.com/1/rg#-r';
	WWW_LINK_GLOBBING_HELP = 'https://www.w3schools.com/git/git_ignore.asp'; // https://www.mankier.com/5/gitignore

	FORMAT_VERSION_INFO = 'v%d.%d.%d.%d-beta';
	FORMAT_NAME_VERSION_INFO = '%s (%s) %s';
	FORMAT_VERSION_INFO_IN_STATUSBAR = '%s   ';

	CR = #13;
	LF = #10;
	CRLF = sLineBreak;
	CRLF2 = CRLF + CRLF;
	TAB = #9;
	SPACE = #32;
	EOS = #0;

	ITEM_KEY_PREFIX = 'Item_';

	SEARCH_PATH_SEPARATOR = ';';
	ARRAY_SEPARATOR = '|';

	BUFF_LENGTH = 1024; // Todo: put into settings
	MAX_HISTORY_COUNT = 20;

	DRAW_RESULT_ON_EVERY_LINE_COUNT = 100; // Todo: put into settings
	DRAW_RESULT_UNTIL_FIRST_LINE_COUNT = 100;

	LISTVIEW_TYPES : TArray<TViewStyle> = [vsList, vsIcon, vsReport, vsSmallIcon];
	LISTVIEW_TYPE_TEXTS : TArray<string> = ['List', 'Icon', 'Report', 'SmallIcon'];

	IMG_IDX_EXPAND = 4;
	IMG_IDX_COLLAPSE = 5;

	IMG_IDX_SHOW_ABS_PATH = 6;
	IMG_IDX_SHOW_RELATIVE_PATH = 7;

	IMG_IDX_ALTERNATE_BG_ON = 8;

	IMG_IDX_SHOW_FILE_ICON_TRUE = 9;
	// IMG_IDX_SHOW_FILE_ICON_FALSE = 9;

	IMG_IDX_INDENT_OFF = 10;
	IMG_IDX_INDENT_ON = 11;

	IMG_IDX_FILTER_ON = 12;
	IMG_IDX_FILTER_OFF = 13;

	IMG_IDX_REPLACE_OFF = 14;
	IMG_IDX_REPLACE_ON = 15;

	// IMG_IDX_UNSORTED = 3;
	// IMG_IDX_DESCENDING_SORTED = 4;
	// IMG_IDX_ASCENDING_SORTED = 5;

	PNL_STATISTIC_IDX = 0;
	PNL_STATUS_IDX = 1;
	PNL_MESSAGE_IDX = 2;

	LV_IMG_IDX_OK = 0;
	LV_IMG_IDX_ERROR = 1;
	LV_IMG_IDX_INFO = 2;
	LV_IMG_IDX_X = 3;

	DT_ALIGN : array [TAlignment] of TTextFormats = (
		{ } tfLeft,
		{ } tfRight,
		{ } tfCenter);
	ALL_ALPHANUMERIC_CHARS = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz1234567890';

	RG_PROCESSING_LINE_COUNT_LIMIT = 1000; // Todo: put into settings

	RG_MATCH_LINE_REGEX = '^(?<drive>\w:)?(?<path>.+?):(?<row>\d+):(?<col>\d+):(?<text>.+)$';
	RG_MATCH_LINE_CONTEXT_REGEX = '^(?<drive>\w:)?(?<path>.+?)-(?<row>\d+)-(?<text>.*)$';

	ESC_CHAR = #$01B;
	ESC = ESC_CHAR + '\[';
	RG_PRETTY_BLUE = ESC + '0m' + ESC + '36m';
	RG_PRETTY_GREEN = ESC + '0m' + ESC + '32m';
	RG_PRETTY_BOLD_RED = ESC + '1m' + ESC + '31m';
	RG_PRETTY_RESET = ESC + '0m';

	// saved regex here: https://regex101.com/r/Q7WJcd/1
	// ^#\$1B'\[0m'#\$1B'\[36m(?<drive>\w:)?(?<path>.+?)'#\$1B'\[0m:'#\$1B'\[0m'#\$1B'\[32m(?<row>\d+)'#\$1B'\[0m:'#\$1B'\[0m(?<col>\d+)'#\$1B'\[0m:(?<text_before_match>.*?)?('#\$1B'\[0m'#\$1B'\[1m'#\$1B'\[31m(?<match_text>.+)'#\$1B'\[0m(?<text_after_match>.*?)?)?$
	RG_MATCH_PRETTY_LINE_REGEX = '^' +
	{ } RG_PRETTY_BLUE + '(?<drive>\w:)?(?<path>.+?)' + // +? one or unlimited as few as possible (lazy )
	{ } RG_PRETTY_RESET + ':' +
	{ } RG_PRETTY_GREEN + '(?<row>\d+)' +
	{ } RG_PRETTY_RESET + ':' +
	{ } RG_PRETTY_RESET + '(?<col>\d+)' +
	{ } RG_PRETTY_RESET + ':(?<text_before_match>.*?)?' + // *? zero or unlimited as few as possible (lazy)
	{ } '(' + RG_PRETTY_RESET + RG_PRETTY_BOLD_RED + '(?<match_text>.+)' +
	{ } RG_PRETTY_RESET + '(?<text_after_match>.*?)?' + ')?$';

	RG_MATCH_PRETTY_LINE_CONTEXT_REGEX = '^' +
	{ } RG_PRETTY_BLUE + '(?<drive>\w:)?(?<path>.+?)' +
	{ } RG_PRETTY_RESET + '-' +
	{ } RG_PRETTY_GREEN + '(?<row>\d+)' +
	{ } RG_PRETTY_RESET + '-' + '(?<text>.*)$';

	RG_HELP_LONG_PARAM_REGEX = '(?<long>--[\-a-zA-Z0-9]+)';
	RG_HELP_LINE_REGEX = '^\s*(?<short>-[a-zA-Z.0-9])?(?<comma>, )?(?<long>--[\-a-zA-Z0-9]+)(?<value>=[\-A-Z]+)?\s*(?<desc>.*)$';

	RG_CONTEXT_SEPARATOR = '--';
	RG_PARAM_QUOTE_CHAR = '''';

	RG_ARG_SEARCH_PATH = 'SearchPath';
	RG_ARG_SEARCH_TEXT = 'SearchText';
	// RG_ARG_REPLACE_TEXT = 'ReplaceText';
	RG_ARG_OPTIONS = 'Options';

	RG_INI_KEY_RGPATH = 'RgExePath';

	WB = '\b'; // word boundary

	RG_PARAM_REGEX_IGNORE_CASE = '-i|--ignore-case';
	RG_PARAM_REGEX_CASE_SENSITIVE = '-s|--case-sensitive';
	RG_PARAM_REGEX_FIXED_STRINGS = '-F|--fixed-strings'; // if not set, then regex
	RG_PARAM_REGEX_WORD_REGEX = '-w|--word-regexp'; // match word-regex ... not used by us
	RG_PARAM_REGEX_GLOB = '-g|--glob';
	RG_PARAM_REGEX_HIDDEN = '-\.|--hidden';
	RG_PARAM_REGEX_NO_IGNORE = '-u|--no-ignore';
	RG_PARAM_REGEX_PRETTY = '-p|--pretty';
	RG_PARAM_REGEX_CONTEXT = '-C|--context';
	RG_PARAM_REGEX_ENCODING = '-E|--encoding';
	RG_PARAM_REGEX_REPLACE = '-r|--replace';
	RG_PARAM_END = '--';

	RG_PARAM_REGEX_VALUE_FORMAT = '^(%s)=?(.+)?$';
	RG_PARAM_REGEX_VARIANT_WITH_OPTIONAL_VALUE = '^(-\\?[\w.])\|(--\w[\w-]+)=?(.+)?$';
	RG_PARAM_REGEX_SINGLE_WITH_OPTIONAL_VALUE = '^(-+[\w.\\-]+)=?([^|]+)?$';
	RG_PARAM_WORD_IN_OPTION_LIST = '^--?\\?[-\w.]+(=[''"]?[!.*\/\w \[\]-]+[''"]?)?$';

	RG_GUI_SEARCH_OPTIONS : TArray<string> = [
	{ } RG_PARAM_REGEX_IGNORE_CASE,
	{ } RG_PARAM_REGEX_CASE_SENSITIVE,
	{ } // RG_PARAM_REGEX_WORD_REGEX,
	{ } RG_PARAM_REGEX_FIXED_STRINGS
	{ } ];

	RG_GUI_SET_PARAMS : TArray<string> = [
	{ } RG_PARAM_REGEX_IGNORE_CASE,
	{ } RG_PARAM_REGEX_CASE_SENSITIVE,
	{ } RG_PARAM_REGEX_FIXED_STRINGS,

	{ } RG_PARAM_REGEX_GLOB,
	{ } RG_PARAM_REGEX_HIDDEN,
	{ } RG_PARAM_REGEX_NO_IGNORE,
	{ } RG_PARAM_REGEX_PRETTY,
	{ } RG_PARAM_REGEX_CONTEXT,
	{ } RG_PARAM_REGEX_ENCODING,
	{ } RG_PARAM_REGEX_REPLACE,
	{ } RG_PARAM_END
	{ } ];

	RG_NECESSARY_PARAMS : TArray<string> = [
	{ } '--vimgrep',
	{ } '--line-buffered', // some big search couldn't be catched without this
	{ } '--no-ignore-parent',
	{ } '--follow',
	{ } '--crlf'];

	SEARCH_OPTIONS : array [0 .. 2] of EGuiOption =
	{ } (EGuiOption.soMatchCase, EGuiOption.soMatchWord, EGuiOption.soUseRegex);

	SEARCH_OPTION_CASES : array [0 .. 8] of TSearchOptionToRgOptions = (
		{ } (SearchOption : [];
		{ }{ } RgOptions : [RG_PARAM_REGEX_FIXED_STRINGS, RG_PARAM_REGEX_IGNORE_CASE]),
		{ } (SearchOption : [EGuiOption.soNotSet];
		{ }{ } RgOptions : [RG_PARAM_REGEX_FIXED_STRINGS, RG_PARAM_REGEX_IGNORE_CASE]),
		{ } (SearchOption : [EGuiOption.soMatchCase];
		{ }{ } RgOptions : [RG_PARAM_REGEX_FIXED_STRINGS, RG_PARAM_REGEX_CASE_SENSITIVE]),
		{ } (SearchOption : [EGuiOption.soMatchWord];
		{ }{ } RgOptions : [RG_PARAM_REGEX_IGNORE_CASE { , RG_PARAM_REGEX_WORD_REGEX } ]),
		{ } (SearchOption : [EGuiOption.soUseRegex];
		{ }{ } RgOptions : [RG_PARAM_REGEX_IGNORE_CASE { , RG_PARAM_REGEX_WORD_REGEX } ]),
		{ } (SearchOption : [EGuiOption.soMatchCase, EGuiOption.soMatchWord];
		{ }{ } RgOptions : [RG_PARAM_REGEX_CASE_SENSITIVE { , RG_PARAM_REGEX_WORD_REGEX } ]),
		{ } (SearchOption : [EGuiOption.soMatchCase, EGuiOption.soUseRegex];
		{ }{ } RgOptions : [RG_PARAM_REGEX_CASE_SENSITIVE { , RG_PARAM_REGEX_WORD_REGEX } ]),
		{ } (SearchOption : [EGuiOption.soMatchWord, EGuiOption.soUseRegex];
		{ }{ } RgOptions : [RG_PARAM_REGEX_IGNORE_CASE { , RG_PARAM_REGEX_WORD_REGEX } ]),
		{ } (SearchOption : [EGuiOption.soMatchCase, EGuiOption.soMatchWord, EGuiOption.soUseRegex];
		{ }{ } RgOptions : [RG_PARAM_REGEX_CASE_SENSITIVE { , RG_PARAM_REGEX_WORD_REGEX } ])
		{ } );

	RG_PARAM_SHORT_INDEX = 0;
	RG_PARAM_LONG_INDEX = 1;

	RG_ERROR = 2;
	RG_NO_MATCH = 1;
	RG_SUCCESS = 0;

	ICON_RESOURCE_DLL = 'System32\shell32.dll';
	ICON_IDX_ERROR = 234;
	ICON_IDX_PARSE_ERROR = 66;
	ICON_IDX_STATISTIC = 249;

	RG_ERROR_MSG_PREFIX = 'rg:';
	RG_PARSE_ERROR = 'not parsed output';
	RG_PARSE_ERROR_MSG = 'There are some not parsed output lines.' + CRLF +
	{ } 'Set encoding parameter and try again.';

	RG_PRODUCED_NO_OUTPUT_MSG = 'rg.exe produced nothing to stdout.';
	RG_REPORTED_ERROR_MSG = 'rg.exe reported error.';

	RG_ENDED_ERROR = ' failed with exit code: ';
	RG_HAS_NO_OUTPUT = ' has no output.';
	RG_STATS_LINE = 'search statistics:';

	MSG_FORMAT_TOO_MANY_RESULTS = 'Too many results.' + CRLF + 'The first %d will be shown. Try to be more specific.';

	SHOW_CMD_IN_ONE_LINE = 'Show command in one line';
	SHOW_CMD_IN_SEPARATE_LINES = 'Show command in lines';
	FORMAT_RIPGREP_EXE_NOT_FOUND = 'RipGrep executable (' + RG_EXE + ') not found.' + CRLF +
	{ } 'Please provide a valid path in the settings, or in' + CRLF +
	{ } '%s';

	MAX_COMMAND_LINE_LENGTH = 32767;

	TREEVIEW_INDENT_TAB_AS_SPACES = '    ';
	TREEVIEW_FONTSPACE = 2;

	// See it in VsCode Segoe ui
	TREEVIEW_HISTORY_REPLACE_PREFIX = '⇄';
	TREEVIEW_HISTORY_COUNTER_ERROR_PREFIX = '⚠';
	TREEVIEW_HISTORY_COUNTER_OK_PREFIX = '✔';
	TREEVIEW_HISTORY_LOADED_PREFIX = ' ↻'; // 🔄; ↻
	TREEVIEW_HISTORY_COUNTER_NOTHING_FOUND_PREFIX = '⛒';

	DEFAULTS_INI_SECTION = 'RipGrepperSettingsDefaults';
	ROOT_DUMMY_INI_SECTION = 'Root Dummy Section';

	COMPONENT_NAME_COLORSELECTOR = '_ColorSelector';

	USERMESSAGE_VALIDATE_INPUT = WM_USER + 100;

type

	EColumnIndex = (ciFile, ciRow, ciColBegin, ciColEnd, ciText, ciMatchText, ciTextAfterMatch);

const
	FILE_COLUMN = 'File';
	TREEVIEW_COLUMN_TITLES : TArray<string> = [FILE_COLUMN, 'Row', 'Col', 'Text', 'MatchText', 'TextAfterMatch'];

type
	TDefaults = class
		private
			class function GetColumnIndex(Index : string) : integer; static;
			class function GetColumnTitle(Index : EColumnIndex) : string; static;

		public const
			RG_PARAM_ENCODING_VALUES : TArray<string> = [
			{ } 'none',
			{ } 'utf8',
			{ } 'windows-1252'];
			class var EXT_DEFAULT_SHORTCUT_SEARCH : string;
			class var EXT_DEFAULT_SHORTCUT_OPEN_WITH : string;
			class var EXT_DEFAULT_SHORTCUT_SETTINGS : string;
			class constructor Create;
			class property ColumnIndex[index : string] : integer read GetColumnIndex;
			class property ColumnTitle[index : EColumnIndex] : string read GetColumnTitle; default;
	end;

implementation

uses
	System.SysUtils;

class constructor TDefaults.Create;
begin
	inherited;
	EXT_DEFAULT_SHORTCUT_SEARCH := ShortCutToText(ShortCut(Word('R'), [ssShift, ssAlt]));
	EXT_DEFAULT_SHORTCUT_OPEN_WITH := ShortCutToText(ShortCut(Word('O'), [ssShift, ssAlt]));
	EXT_DEFAULT_SHORTCUT_SETTINGS := ''; // no default. it can be set in INI
end;

class function TDefaults.GetColumnIndex(Index : string) : integer;
var
	arrTitles : TArrayEx<string>;
begin
	arrTitles := TREEVIEW_COLUMN_TITLES;
	Result := arrTitles.IndexOf(index);
end;

class function TDefaults.GetColumnTitle(Index : EColumnIndex) : string;
begin
	Result := TREEVIEW_COLUMN_TITLES[Integer(index)];
end;

end.
