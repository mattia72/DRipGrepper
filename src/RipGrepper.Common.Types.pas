unit RipGrepper.Common.Types;

interface

uses
	System.Classes,
	Vcl.ComCtrls,
	ArrayHelper,
	System.Generics.Defaults,
	Vcl.Graphics;

const
	CR = #13;
	LF = #10;
	CRLF = sLineBreak;
	BUFF_LENGTH = 1024; // Todo: put into settings

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

	IMAGE_IDX_UNSORTED = 3;
	IMAGE_IDX_DESCENDING_SORTED = 4;
	IMAGE_IDX_ASCENDING_SORTED = 5;

	EXE_AND_VERSION_FORMAT = '%s   ';
	PNL_STATTS_IDX = 0;
	PNL_STATUS_IDX = 1;
	PNL_MESSAGE_IDX = 2;

	LV_IMAGE_IDX_OK = 0;
	LV_IMAGE_IDX_ERROR = 1;
	LV_IMAGE_IDX_INFO = 2;

	DT_ALIGN : array [TAlignment] of TTextFormats = (
		{ } tfLeft,
		{ } tfRight,
		{ } tfCenter);
	ALL_ALPHANUMERIC_CHARS = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz1234567890';

	RG_PROCESSING_LINE_COUNT_LIMIT = 10000; // Todo: put into settings
	RG_NECESSARY_PARAMS : TArray<string> = ['--vimgrep', '--line-buffered' // ,// some big search couldn't be catched without this
	// '--pretty' // TODO: parse color escape
		];

	RG_MATCH_LINE_REGEX = '^(?<drive>\w:)?(?<path>.+?):(?<row>\d+):(?<col>\d+):(?<text>.+)$';
	RG_HELP_LONG_PARAM_REGEX = '(?<long>--[\-a-zA-Z0-9]+)';
	RG_HELP_LINE_REGEX = '^\s*(?<short>-[a-zA-Z.0-9])?(?<comma>, )?(?<long>--[\-a-zA-Z0-9]+)(?<value>=[\-A-Z]+)?\s*(?<desc>.*)$';

type

	TParserType = (ptEmpty, ptRipGrepSearch, ptRipGrepVersion, ptRipGrepError, ptRipGrepHelp);
	TFileNameType = (ftAbsolute, ftRelative);

	TColumnIndex = (ciFile, ciRow, ciCol, ciText, ciRowNr);

implementation

uses
	System.SysUtils;

end.
