unit RipGrepper.Common.Interfaces;

interface

uses
	System.RegularExpressions,
	RipGrepper.Common.Types,
	ArrayEx,
	System.Generics.Collections,
	System.Classes,
	RipGrepper.Common.ParsedObject;

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
		function GetParserType : TParserType;
		procedure SetLineParseRegex(const Value : TRegex);
		procedure SetParserType(const Value : TParserType);

		property LineParseRegex : TRegex read GetLineParseRegex write SetLineParseRegex;
		property ParserType : TParserType read GetParserType write SetParserType;
	end;

	ILineParser = interface
		['{C7B3E1CC-635C-4D67-9681-2352898822CD}']
		function ParseLine(const _iLnNr : integer; const _s : string; const _bIsLast : Boolean = False): IParsedObjectRow;

    end;

	IHistoryItem = interface(IInterface)
		['{C95F78AF-4011-460F-8721-5C3D7FC682D7}']
		function GetErrorCount : Integer;
		function GetFileCount : integer;
		function GetMatches : TParsedObjectRowCollection;
		procedure SetMatches(const Value : TParsedObjectRowCollection);
		function GetRipGrepArguments : TRipGrepArguments;
		procedure SetRipGrepArguments(const Value : TRipGrepArguments);
		function GetTotalMatchCount : integer;

		property ErrorCount : Integer read GetErrorCount;
		property Matches : TParsedObjectRowCollection read GetMatches write SetMatches;
		property RipGrepArguments : TRipGrepArguments read GetRipGrepArguments write SetRipGrepArguments;
		property FileCount : integer read GetFileCount;
		property TotalMatchCount : integer read GetTotalMatchCount;
	end;

	{$M-}

implementation

end.
