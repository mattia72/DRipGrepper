unit RipGrepper.Common.Interfaces;

interface

uses
	System.RegularExpressions,
	RipGrepper.Common.Types,
	ArrayHelper, System.Generics.Collections;

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

	ILine = interface
		['{691C646B-26C8-453F-87BB-7C57F572D34C}']
		function GetLineNr : Integer;
		procedure SetLineNr(const Value : Integer);
		property LineNr : Integer read GetLineNr write SetLineNr;
	end;

	ILineError = interface(ILine)
		function GetIsError : Boolean;
		procedure SetIsError(const Value : Boolean);
		property IsError : Boolean read GetIsError write SetIsError;

	end;

	ILineParser = interface(ILineError)
		['{B33D6808-8A4F-49AD-A711-F226B55DEE5F}']
		function GetLineParseRegex : TRegex;
		function GetParserType : TParserType;
		procedure SetLineParseRegex(const Value : TRegex);
		procedure SetParserType(const Value : TParserType);

		procedure ParseLine(const _iLnNr : integer; const _s : string; const _bIsLast : Boolean = False);
		property LineParseRegex : TRegex read GetLineParseRegex write SetLineParseRegex;
		property ParserType : TParserType read GetParserType write SetParserType;
	end;

	IRipGrepMatchLine = interface(ILineParser)
		['{2358D1CB-390A-4491-95FE-79BF12019C87}']
		function GetCol : Integer; stdcall;
		function GetFileName : string; stdcall;
		function GetRow : Integer; stdcall;
		function GetText : string; stdcall;
		procedure SetCol(const Value : Integer); stdcall;
		procedure SetFileName(const Value : string); stdcall;
		procedure SetRow(const Value : Integer); stdcall;
		procedure SetText(const Value : string); stdcall;
		property Col : Integer read GetCol write SetCol;
		property FileName : string read GetFileName write SetFileName;
		property Row : Integer read GetRow write SetRow;
		property Text : string read GetText write SetText;

	end;

	IRipGrepMatchLineGroup = interface(IRipGrepMatchLine)
		['{154707F1-9ECB-4FAE-943D-249A9DB6FFAF}']
		function GetGroupId : Integer;
		procedure SetGroupId(const Value : Integer);
		property GroupId : Integer read GetGroupId write SetGroupId;
	end;

	{$M-}

	TRipGrepMatchLineCollection = TList<IRipGrepMatchLine>;

	TRipGrepMatchLineGroupCollection = TArrayRecord<IRipGrepMatchLineGroup>;

	IHistoryItem = interface(IInterface)
		['{C95F78AF-4011-460F-8721-5C3D7FC682D7}']
		function GetFileCount: integer;
		function GetMatches: TRipGrepMatchLineCollection;
		function GetRipGrepArguments: TArrayRecord<string>;
		function GetTotalMatchCount: integer;
		procedure SetFileCount(const Value: integer);
		procedure SetMatches(const Value: TRipGrepMatchLineCollection);
		procedure SetRipGrepArguments(const Value: TArrayRecord<string>);
		procedure SetTotalMatchCount(const Value: integer);

		property Matches: TRipGrepMatchLineCollection read GetMatches write SetMatches;
		property RipGrepArguments: TArrayRecord<string> read GetRipGrepArguments write SetRipGrepArguments;
		property FileCount: integer read GetFileCount write SetFileCount;
		property TotalMatchCount: integer read GetTotalMatchCount write SetTotalMatchCount;
	end;

implementation

end.
