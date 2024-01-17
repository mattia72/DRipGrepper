unit RipGrepper.Common.Interfaces;

interface

uses
	System.RegularExpressions,
	RipGrepper.Common.Types;

type

	{$M+}
	IEOFProcessEventHandler = interface
		['{E6AC51D8-9705-4A56-902B-494B1EC11184}']
		procedure OnEOFProcess();
	end;

	INewLineEventHandler = interface
		['{A2EB8A24-0281-4AAA-BF91-210A95973652}']
		procedure OnNewOutputLine(const _sLine : string; _bIsLast : Boolean = False);
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

	ILineParser = interface(ILine)
	['{B33D6808-8A4F-49AD-A711-F226B55DEE5F}']
		function GetIsError : Boolean;
		function GetLineParseRegex : TRegex;
		function GetParserType : TParserType;
		procedure SetIsError(const Value : Boolean);
		procedure SetLineParseRegex(const Value : TRegex);
		procedure SetParserType(const Value : TParserType);

		procedure ParseLine(const _iLnNr : integer; const _s : string; const _bIsLast : Boolean = False);

		property IsError : Boolean read GetIsError write SetIsError;
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

implementation

end.
