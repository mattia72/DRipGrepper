unit RipGrepper.Parsers.ParallelParser;

interface

uses
	System.Classes,
	RipGrepper.Data.Matches,
	RipGrepper.Common.Interfaces;

type

	TLastLineEvent = procedure(const _iLineNr : Integer) of object;
	TProgressEvent = procedure(const _bLastLine : Boolean) of object;
	TAfterAllFinished = procedure() of object;

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

	TParallelParser = class // (TInterfacedObject, IParallelParser) // class(TThread)
		private
			FHistObject : IHistoryItemObject;
			FData : TRipGrepperData;
			FIsLast : Boolean;
			FIsParsingDone : Boolean;
			FLine : string;
			FLineNr : Integer;
			FOnAfterAllFinished : TAfterAllFinished;
			FOnLastLine : TLastLineEvent;
			FOnProgress : TProgressEvent;
			// FThread : TParseLineThread;
			procedure CallOnProgress(const _iLineNr : Integer; const _bIsLast : Boolean);
			function GetOnAfterAllFinished : TAfterAllFinished;
			function GetOnLastLine : TLastLineEvent;
			function GetOnProgress : TProgressEvent;
			procedure ParseLine(const _iLineNr : Integer; const _sLine : string);
			procedure ParserProc;
			procedure SetOnAfterAllFinished(const Value : TAfterAllFinished);
			procedure SetOnLastLine(const Value : TLastLineEvent);
			procedure SetOnProgress(const Value : TProgressEvent);

		public
			constructor Create(_data : TRipGrepperData; _histObj : IHistoryItemObject);
			destructor Destroy; override;
			procedure AbortQueuedParses;
			procedure Parse;
			procedure SetNewLine(const _iLineNr : Integer; const _sLine : string; const _bIsLast : Boolean);
			property LineNr : Integer read FLineNr;
			property IsParsingDone : Boolean read FIsParsingDone;
			property OnAfterAllFinished : TAfterAllFinished read GetOnAfterAllFinished write SetOnAfterAllFinished;
			property OnLastLine : TLastLineEvent read GetOnLastLine write SetOnLastLine;
			property OnProgress : TProgressEvent read GetOnProgress write SetOnProgress;
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
	FIsParsingDone := False;
	// FThread := TParseLineThread.Create(FData, FHistObject);
end;

destructor TParallelParser.Destroy;
begin
	AbortQueuedParses;
	inherited;
end;

procedure TParallelParser.AbortQueuedParses;
begin
	TThread.RemoveQueuedEvents(ParserProc);
end;

procedure TParallelParser.CallOnProgress(const _iLineNr : Integer; const _bIsLast : Boolean);
begin
	if (_iLineNr < DRAW_RESULT_UNTIL_FIRST_LINE_COUNT) or
	{ } ((_iLineNr mod DRAW_RESULT_ON_EVERY_LINE_COUNT) = 0) or _bIsLast then begin
		OnProgress(_bIsLast);
	end;
end;

function TParallelParser.GetOnAfterAllFinished : TAfterAllFinished;
begin
	Result := FOnAfterAllFinished;
end;

function TParallelParser.GetOnLastLine : TLastLineEvent;
begin
	Result := FOnLastLine;
end;

function TParallelParser.GetOnProgress : TProgressEvent;
begin
	Result := FOnProgress;
end;

procedure TParallelParser.Parse;
begin
	ParseLine(FLineNr, FLine);
end;

procedure TParallelParser.ParseLine(const _iLineNr : Integer; const _sLine : string);
begin
	if (_iLineNr = RG_PROCESSING_LINE_COUNT_LIMIT) then begin
		FIsLast := True;
	end else if (_iLineNr > RG_PROCESSING_LINE_COUNT_LIMIT) then begin
		TDebugUtils.DebugMessage('TParallelParser.ParseLine - Remove every Queued Events');
		Exit;
	end;

	// https://stackoverflow.com/questions/23359546/ensure-all-tthread-queue-methods-complete-before-thread-self-destructs
	if FIsLast then begin
		TThread.Synchronize(nil, ParserProc); // waits till everything finished
		OnAfterAllFinished();
	end else begin
		TThread.Queue(nil, ParserProc);
	end;
end;

procedure TParallelParser.ParserProc;
var
	ifParser : ISearchResultLineParser;
	ifSearchParam : ISearchParams;
	oParsed : IParsedObjectRow;
begin
	try
		if FIsLast then begin
			OnLastLine(FLineNr);
			TDebugUtils.DebugMessage(Format('TParallelParser.ParseLine - Before parse last line: %d in %d err: %d',
				[FHistObject.TotalMatchCount, FHistObject.FileCount, FHistObject.GetErrorCounters().FSumOfErrors]));
		end;

		if (not FLine.IsEmpty) then begin
			ifParser := TRipGrepperParsersFactory.GetParser(FHistObject.ParserType);
			ifSearchParam := TSearchParams.Create(FHistObject.GetSearchTextWithOptions);
			ifParser.SearchParams := ifSearchParam;
			ifParser.ParseLine(FLineNr, FLine, FIsLast);
			oParsed := TParsedObjectRow.Create(ifParser.ParseResult, FHistObject.ParserType);
			try
				FData.Add(oParsed);
			finally
				ifParser := nil;
				oParsed := nil;
			end;
		end;
		CallOnProgress(FLineNr, FIsLast);
	finally
		FIsParsingDone := True;
	end;
end;

procedure TParallelParser.SetNewLine(const _iLineNr : Integer; const _sLine : string; const _bIsLast : Boolean);
begin
	FIsLast := _bIsLast;
	FLine := _sLine;
	FLineNr := _iLineNr;
end;

procedure TParallelParser.SetOnAfterAllFinished(const Value : TAfterAllFinished);
begin
	FOnAfterAllFinished := Value;
end;

procedure TParallelParser.SetOnLastLine(const Value : TLastLineEvent);
begin
	FOnLastLine := Value;
end;

procedure TParallelParser.SetOnProgress(const Value : TProgressEvent);
begin
	FOnProgress := Value;
end;

constructor TParseLineThread.Create(_data : TRipGrepperData; _histObj : IHistoryItemObject);
begin
	inherited Create(True);
	FHistObject := _histObj;
	FData := _data;
	FreeOnTerminate := True;
end;

procedure TParseLineThread.Execute;
begin
	inherited;
end;

end.
