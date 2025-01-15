unit RipGrepper.Parsers.ParallelParser;

interface

uses
	System.Classes,
	RipGrepper.Data.Matches,
	RipGrepper.Common.Interfaces;

type
	TLastLineEvent = procedure(const _iLineNr : Integer) of object;
	TProgressEvent = procedure() of object;

	TParseLineThread = class(TThread)

		private
			FData : TRipGrepperData;
			FHistObject : IHistoryItemObject;

			FOnLastLine : TLastLineEvent;
			FOnProgress : TProgressEvent;

		protected
			procedure Execute; override;

		public
			constructor Create(_data : TRipGrepperData; _histObj : IHistoryItemObject); overload;
			property OnLastLine : TLastLineEvent read FOnLastLine write FOnLastLine;
			property OnProgress : TProgressEvent read FOnProgress write FOnProgress;
	end;

	TParallelParser = class // class(TThread)
		private
			FHistObject : IHistoryItemObject;
			FData : TRipGrepperData;
			FIsLast : Boolean;
			FLine : string;
			FLineNr : Integer;
			FOnLastLine : TLastLineEvent;
			FOnProgress : TProgressEvent;
			FThread : TParseLineThread;
			procedure CallOnProgress(const _iLineNr : Integer; const _bIsLast : Boolean);
			procedure ParseLine(const _iLineNr : Integer; const _sLine : string; const _bIsLast : Boolean);

		public
			constructor Create(_data : TRipGrepperData; _histObj : IHistoryItemObject);
			destructor Destroy; override;
			procedure Parse;
			procedure SetNewLine(const _iLineNr : Integer; const _sLine : string; const _bIsLast : Boolean);
			property OnLastLine : TLastLineEvent read FOnLastLine write FOnLastLine;
			property OnProgress : TProgressEvent read FOnProgress write FOnProgress;
	end;

implementation

uses
	RipGrepper.Common.ParsedObject,
	RipGrepper.Tools.DebugUtils,
	System.SysUtils,
	RipGrepper.Parsers.Factory,
	RipGrepper.Helper.Types,
	RipGrepper.Common.Constants,
	Winapi.Windows,
	RipGrepper.Common.SearchParams;

constructor TParallelParser.Create(_data : TRipGrepperData; _histObj : IHistoryItemObject);
begin
	inherited Create();
	FHistObject := _histObj;
	FData := _data;
	FThread := TParseLineThread.Create(FData, FHistObject);
end;

destructor TParallelParser.Destroy;
begin
	inherited;
	FThread.Free;
end;

procedure TParallelParser.CallOnProgress(const _iLineNr : Integer; const _bIsLast : Boolean);
begin

	if (_iLineNr < DRAW_RESULT_UNTIL_FIRST_LINE_COUNT) or ((_iLineNr mod DRAW_RESULT_ON_EVERY_LINE_COUNT) = 0) or _bIsLast then begin
		OnProgress();
	end;
end;

procedure TParallelParser.Parse;
begin
	ParseLine(FLineNr, FLine, FIsLast);
end;

procedure TParallelParser.ParseLine(const _iLineNr : Integer; const _sLine : string; const _bIsLast : Boolean);

begin
	if (_iLineNr > RG_PROCESSING_LINE_COUNT_LIMIT) then begin
		TDebugUtils.DebugMessage('TParallelParser.ParseLine - Remove every Queued Events');
		TThread.RemoveQueuedEvents(FThread);
		Exit;
	end;

	FThread.Queue(
		procedure
		var
			ifParser : ISearchResultLineParser;
			ifSearchParam : ISearchParams;
			oParsed : IParsedObjectRow;
		begin
			if _bIsLast then begin
				OnLastLine(_iLineNr);
				TDebugUtils.DebugMessage(Format('TParallelParser.ParseLine - Before parse last line: %d in %d err: %d',
					[FHistObject.TotalMatchCount, FHistObject.FileCount, FHistObject.GetErrorCounters().FSumOfErrors]));
			end;

			if (not _sLine.IsEmpty) then begin
				ifParser := TRipGrepperParsersFactory.GetParser(FHistObject.ParserType);
				ifSearchParam := TSearchParams.Create(FHistObject.GuiSearchTextParams);
				ifParser.SearchParams := ifSearchParam;
				ifParser.ParseLine(_iLineNr, _sLine, _bIsLast);
				oParsed := TParsedObjectRow.Create(ifParser.ParseResult, FHistObject.ParserType);
				try
					FData.Add(oParsed);
				finally
					ifParser := nil;
					oParsed := nil;
				end;
			end;
			CallOnProgress(_iLineNr, _bIsLast);
		end);
end;

procedure TParallelParser.SetNewLine(const _iLineNr : Integer; const _sLine : string; const _bIsLast : Boolean);
begin
	FIsLast := _bIsLast;
	FLine := _sLine;
	FLineNr := _iLineNr;
end;

constructor TParseLineThread.Create(_data : TRipGrepperData; _histObj : IHistoryItemObject);
begin
	inherited Create(True);
	FHistObject := _histObj;
	FData := _data;
end;

procedure TParseLineThread.Execute;
begin
	inherited;
end;

end.
