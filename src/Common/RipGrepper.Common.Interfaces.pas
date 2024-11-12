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
	RipGrepper.Common.GuiSearchParams,
	RipGrepper.Settings.SearchFormSettings,
	RipGrepper.Settings.RipGrepperSettings, RipGrepper.Common.EncodedStringList;

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
		function GetParserType : TParserType;
		procedure SetLineParseRegex(const Value : TRegex);
		procedure SetContextLineParseRegex(const Value : TRegex);
		procedure SetParserType(const Value : TParserType);

		property LineParseRegex : TRegex read GetLineParseRegex write SetLineParseRegex;
		property ContextLineParseRegex : TRegex read GetContextLineParseRegex write SetContextLineParseRegex;
		property ParserType : TParserType read GetParserType write SetParserType;
	end;

	ILineParser = interface
		['{C7B3E1CC-635C-4D67-9681-2352898822CD}']
		function GetParseResult : IParsedObjectRow;
		procedure SetParseResult(const Value : IParsedObjectRow);
		property ParseResult : IParsedObjectRow read GetParseResult write SetParseResult;

		procedure ParseLine(const _iLnNr : integer; const _s : string; const _bIsLast : Boolean = False);

	end;

	ISearchParams = interface
		['{CF7C5401-4CBE-4B08-8D4D-62C6E2E70983}']
		function GetGuiSearchParams : TGuiSearchTextParams;
	end;

	ISearchResultLineParser = interface(ILineParser)
		['{6C902F54-68CC-441A-A576-201EF460DB04}']
		function GetSearchParams : ISearchParams;
		procedure SetSearchParams(const Value : ISearchParams);
		property SearchParams : ISearchParams read GetSearchParams write SetSearchParams;
	end;

	IHistoryItemObject = interface(IInterface)
		['{C95F78AF-4011-460F-8721-5C3D7FC682D7}']
		procedure ClearMatches;
		procedure CopyToSettings(const _settings : TRipGrepperSettings);
		function GetElapsedTimeText : string;
		function GetErrorCount : Integer;
		function GetFileCount : integer;
		function GetGuiSearchTextParams : TGuiSearchTextParams;
		function GetIsReplaceMode : Boolean;
		function GetMatches : TParsedObjectRowCollection;
		function GetNoMatchFound : Boolean;
		function GetParserType : TParserType;
		procedure SetMatches(const Value : TParsedObjectRowCollection);
		function GetRipGrepArguments : TRipGrepArguments;
		function GetSearchFormSettings : TSearchFormSettings;
		function GetRipGrepResult : Integer;
		function GetSearchText : string;
		function GetReplaceText : string;
		procedure SetRipGrepArguments(const Value : TRipGrepArguments);
		function GetTotalMatchCount : integer;
		function HasResult : Boolean;
		procedure LoadFromSettings(const _settings : TRipGrepperSettings);
		procedure SetElapsedTimeText(const Value : string);
		procedure SetErrorCount(const Value : Integer);
		procedure SetFileCount(const Value : integer);
		procedure SetGuiSearchTextParams(const Value : TGuiSearchTextParams);
		procedure SetNoMatchFound(const Value : Boolean);
		procedure SetParserType(const Value : TParserType);
		procedure SetSearchFormSettings(const Value : TSearchFormSettings);
		procedure SetRipGrepResult(const Value : Integer);
		function UpdateParserType : TParserType;

		property ElapsedTimeText : string read GetElapsedTimeText write SetElapsedTimeText;
		property ErrorCount : Integer read GetErrorCount write SetErrorCount;
		property Matches : TParsedObjectRowCollection read GetMatches write SetMatches;
		property RipGrepArguments : TRipGrepArguments read GetRipGrepArguments write SetRipGrepArguments;
		property FileCount : integer read GetFileCount write SetFileCount;
		property GuiSearchTextParams : TGuiSearchTextParams read GetGuiSearchTextParams write SetGuiSearchTextParams;
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
