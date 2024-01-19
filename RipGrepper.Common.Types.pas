unit RipGrepper.Common.Types;

interface

uses
	System.Classes,
	Vcl.ComCtrls,
	ArrayHelper;

const

	CR = #13;
	LF = #10;
	CRLF = sLineBreak;
	BUFF_LENGTH = 1024;

	RIPGREP_ERROR = 1;
	RIPGREP_NO_MATCH = 2;

	DRAW_RESULT_ON_EVERY_LINE_COUNT = 100;

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

	ALL_ALPHANUMERIC_CHARS = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz1234567890';

	RG_PROCESSING_LINE_COUNT_LIMIT = 100000;
	RG_NECESSARY_PARAMS : TArray<string> = ['--vimgrep', '--line-buffered' // ,// some big search couldn't be catched without this
	// '--pretty' // TODO: parse color escape
		];

	RG_MATCH_LINE_REGEX = '^(\w:)?(.+?):(\d+):(\d+):(.+)$';

type

	// TNewLineEventHandler = procedure(_obj : INewLineEventHandler; const _s : string);
	// TTerminateEventProducer = function(_obj : ITerminateEventProducer) : boolean;

	TParserType = (ptRipGrepSearch, ptRipGrepVersion, ptRipGrepError, ptRipGrepHelp);
	TFileNameType = (ftAbsolute, ftRelative);

	TSortDirectionType = (stUnsorted, stAscending, stDescending);
	TSortByType = (sbtFile, sbtRow, sbtCol, sbtText);

	TSortTypeDirection = record
		SortType : TSortByType;
		Direction : TSortDirectionType;
		class function New(const _sbt : TSortByType; const _st : TSortDirectionType) : TSortTypeDirection; overload; static;
		class function New(const _sbt : TSortByType; const _bDescending : Boolean = False) : TSortTypeDirection; overload; static;
	end;

	TSortTypeDirectionList = record
		Items : TArrayRecord<TSortTypeDirection>;
		procedure Delete(const _sbt : TSortByType);
	end;

implementation

uses
	System.SysUtils;

procedure TSortTypeDirectionList.Delete(const _sbt : TSortByType);
begin
	var
	i := Items.Find(
		function(const val : TSortTypeDirection) : boolean
		begin
			Result := val.SortType = _sbt;
		end, 0);
	if i >= 0 then
		Items.Delete(i);
end;

class function TSortTypeDirection.New(const _sbt : TSortByType; const _st : TSortDirectionType) : TSortTypeDirection;
begin
	Result.SortType := _sbt;
	Result.Direction := _st;
end;

class function TSortTypeDirection.New(const _sbt : TSortByType; const _bDescending : Boolean = False) : TSortTypeDirection;
begin
	Result.SortType := _sbt;
	if _bDescending then begin
		Result.Direction := stDescending;
	end else begin
		Result.Direction := stAscending;
	end;
end;

end.
