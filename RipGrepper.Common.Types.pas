unit RipGrepper.Common.Types;

interface

uses
	System.Classes;

const
	CR = #13;
	LF = #10;
	CRLF = sLineBreak;

	DRAW_RESULT_ON_EVERY_LINE_COUNT = 100;

	IMG_IDX_SHOW_ABS_PATH = 11;
	IMG_IDX_SHOW_RELATIVE_PATH = 12;
	IMG_IDX_SHOW_FILE_ICON_TRUE = 5;
	IMG_IDX_SHOW_FILE_ICON_FALSE = 2;

	IMAGE_IDX_UNSORTED = 3;
	IMAGE_IDX_DESCENDING_SORTED = 4;
	IMAGE_IDX_ASCENDING_SORTED = 5;

	LV_IMAGE_IDX_OK = 0;
	LV_IMAGE_IDX_ERROR = 1;
	LV_IMAGE_IDX_INFO = 2;

type

	// TNewLineEventHandler = procedure(_obj : INewLineEventHandler; const _s : string);
	// TTerminateEventProducer = function(_obj : ITerminateEventProducer) : boolean;

	TSortDirectionType = (stUnsorted, stAscending, stDescending);
	TParserType = (ptRipGrepSearch, ptRipGrepVersion, ptRipGrepError, ptRipGrepHelp);
	TFileNameType = (ftAbsolute, ftRelative);

	TStringsHelper = class helper for TStrings
		function Contains(const s : string) : Boolean;
	end;

function PostInc(var Value : Integer) : Integer;
function PreInc(var Value : Integer) : Integer;
function GetElapsedTimeInSeconds(const _dtStart : TDateTime) : TDateTime;
function GetElapsedTime(const _dtStart : TDateTime) : string;

implementation

uses
	System.SysUtils;

function PostInc(var Value : Integer) : Integer;
begin
	Result := Value;
	inc(Value);
end;

function PreInc(var Value : Integer) : Integer;
begin
	inc(Value);
	Result := Value;
end;

function GetElapsedTimeInSeconds(const _dtStart : TDateTime) : TDateTime;
begin
	Result := (Now - _dtStart) * 24 * 60 * 60;
end;

function GetElapsedTime(const _dtStart : TDateTime) : string;
begin
	Result := Format('%.2f', [GetElapsedTimeInSeconds(_dtStart)]);
end;

function TStringsHelper.Contains(const s : string) : Boolean;
begin
	Result := self.IndexOf(s) <> -1;
end;

end.
