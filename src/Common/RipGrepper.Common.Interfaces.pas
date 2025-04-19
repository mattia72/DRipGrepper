unit RipGrepper.Common.Interfaces;

interface

uses
	System.RegularExpressions,
	RipGrepper.Common.Constants,
	RipGrepper.Common.SimpleTypes,
	ArrayEx,
	System.Generics.Collections,
	System.Classes,
	RipGrepper.Common.ParsedObject,
	RipGrepper.Common.SearchTextWithOptions,
	RipGrepper.Settings.SearchFormSettings,
	RipGrepper.Settings.RipGrepperSettings,
	RipGrepper.Common.EncodedStringList,
	RipGrepper.Common.GuiSearchParams,
	RipGrepper.Common.Interfaces.StreamPersistable,
	Spring;

type

	{$M+}
	IEOFProcessEventHandler = interface
		['{E6AC51D8-9705-4A56-902B-494B1EC11184}']
		procedure OnEOFProcess();
	end;

	INewLineEventHandler = interface
		['{A2EB8A24-0281-4AAA-BF91-210A95973652}']
		procedure OnNewOutputLine(const _iLineNr : integer; const _sLine : string; _bIsLast : Boolean = False);
	end;

	ITerminateEventProducer = interface
		['{9C259E6F-CED7-41AD-B4F8-3A4BDD981965}']
		function ProcessShouldTerminate() : boolean;
	end;

	ILineParserData = interface
		['{B33D6808-8A4F-49AD-A711-F226B55DEE5F}']
		function GetLineParseRegex : TRegex;
		function GetContextLineParseRegex : TRegex;
		function GetStatsLineParseRegex : TRegex;
		function GetParserType : TParserType;
		procedure SetLineParseRegex(const Value : TRegex);
		procedure SetContextLineParseRegex(const Value : TRegex);
		procedure SetStatsLineParseRegex(const Value : TRegex);
		procedure SetParserType(const Value : TParserType);

		property LineParseRegex : TRegex read GetLineParseRegex write SetLineParseRegex;
		property ContextLineParseRegex : TRegex read GetContextLineParseRegex write SetContextLineParseRegex;
		property StatsLineParseRegex : TRegex read GetStatsLineParseRegex write SetStatsLineParseRegex;
		property ParserType : TParserType read GetParserType write SetParserType;
	end;

	ILineParser = interface
		['{C7B3E1CC-635C-4D67-9681-2352898822CD}']
		function GetParseResult : IParsedObjectRow;
		procedure SetParseResult(const Value : IParsedObjectRow);
		property ParseResult : IParsedObjectRow read GetParseResult write SetParseResult;

		procedure ParseLine(const _iLnNr : integer; const _s : string; const _bIsLast : Boolean = False);

	end;

	// IParallelParser = interface(IInterface) //It destructs the parser too early, use the class instead'
	// ['{951E2EB3-F39A-47CB-9DCA-BD15B02B6F94}']
	// function GetOnLastLine: TLastLineEvent;
	// function GetOnProgress: TProgressEvent;
	// procedure SetNewLine(const _iLineNr : Integer; const _sLine : string; const _bIsLast : Boolean);
	// procedure SetOnLastLine(const Value: TLastLineEvent);
	// procedure SetOnProgress(const Value: TProgressEvent);
	// property OnLastLine: TLastLineEvent read GetOnLastLine write SetOnLastLine;
	// property OnProgress: TProgressEvent read GetOnProgress write SetOnProgress;
	// procedure Parse;
	// end;

	ISearchParams = interface
		['{CF7C5401-4CBE-4B08-8D4D-62C6E2E70983}']
		function GetGuiSearchParams() : IShared<TSearchTextWithOptions>;
	end;

	ISearchResultLineParser = interface(ILineParser)
		['{6C902F54-68CC-441A-A576-201EF460DB04}']
		function GetSearchParams : ISearchParams;
		procedure SetSearchParams(const Value : ISearchParams);
		property SearchParams : ISearchParams read GetSearchParams write SetSearchParams;
	end;

	IHistoryItemObject = interface(IStreamReaderWriterPersistable)
		['{C95F78AF-4011-460F-8721-5C3D7FC682D7}']
		procedure ClearMatches;
		procedure CopyToSettings(const _settings : TRipGrepperSettings);
		function GetElapsedTimeText : string;
		function GetErrorCounters : TErrorCounters;
		function GetFileCount : integer;
		function GetGuiSearchTextParams : IShared<TGuiSearchTextParams>;
		function GetIsReplaceMode : Boolean;
		function GetMatches() : TParsedObjectRowCollection;
		function GetNoMatchFound() : Boolean;
		function GetParserType() : TParserType;
		procedure SetMatches(const Value : TParsedObjectRowCollection);
		function GetRipGrepArguments : IShared<TRipGrepArguments>;
		function GetSearchFormSettings() : TSearchFormSettings;
		function GetRipGrepResult() : Integer;
		function GetSearchText() : string;
		function GetReplaceText() : string;
		function GetSearchTextWithOptions() : IShared<TSearchTextWithOptions>;
		procedure SetRipGrepArguments(const Value : IShared<TRipGrepArguments>);
		function GetTotalMatchCount() : integer;
		function HasResult() : Boolean;
		procedure LoadFromSettings(const _settings : TRipGrepperSettings);
		procedure SetElapsedTimeText(const Value : string);
		procedure SetErrorCounters(const Value : TErrorCounters);
		procedure SetFileCount(const Value : integer);
		procedure SetGuiSearchTextParams(const Value : IShared<TGuiSearchTextParams>);
		procedure SetNoMatchFound(const Value : Boolean);
		procedure SetParserType(const Value : TParserType);
		procedure SetSearchFormSettings(const Value : TSearchFormSettings);
		procedure SetRipGrepResult(const Value : Integer);
		function UpdateParserType : TParserType;

		property ElapsedTimeText : string read GetElapsedTimeText write SetElapsedTimeText;
		property Matches : TParsedObjectRowCollection read GetMatches write SetMatches;
		property RipGrepArguments : IShared<TRipGrepArguments> read GetRipGrepArguments write SetRipGrepArguments;
		property FileCount : integer read GetFileCount write SetFileCount;
		property GuiSearchTextParams : IShared<TGuiSearchTextParams> read GetGuiSearchTextParams write SetGuiSearchTextParams;
		property NoMatchFound : Boolean read GetNoMatchFound write SetNoMatchFound;
		property ParserType : TParserType read GetParserType write SetParserType;
		property SearchFormSettings : TSearchFormSettings read GetSearchFormSettings write SetSearchFormSettings;
		property RipGrepResult : Integer read GetRipGrepResult write SetRipGrepResult;
		property SearchText : string read GetSearchText;
		property IsReplaceMode : Boolean read GetIsReplaceMode;
		property ReplaceText : string read GetReplaceText;
		property TotalMatchCount : integer read GetTotalMatchCount;
	end;

	IReplaceContext = interface(IInterface)
		['{44381611-EDB5-4204-B325-B1F266897843}']
		procedure GetFileLines(_file : string; _list : TEncodedStringList);
		procedure WriteFileLines(_file : string; _list : TEncodedStringList);
	end;

	{$M-}

implementation

end.
