unit RipGrepper.Data.Parsers;

interface

uses
	RipGrepper.Data.Matches,
	RipGrepper.Common.Interfaces,
	System.RegularExpressions,
	RipGrepper.Common.Types,
	RipGrepper.Common.ParsedObject,
	ArrayEx;

type

	TRipGrepLineParserData = class(TInterfacedObject, ILineParserData) // inharitance not supported of interface types
		private
			FLineParseRegex : TRegex;
			FParserType : TParserType;
			function GetLineParseRegex : TRegex;
			procedure SetLineParseRegex(const Value : TRegex);

			function GetParserType : TParserType;
			procedure SetParserType(const Value : TParserType);

		public
			constructor Create(const _type: TParserType; _regex: string); virtual;
			destructor Destroy; override;
			property LineParseRegex : TRegex read GetLineParseRegex write SetLineParseRegex;
			property ParserType : TParserType read GetParserType write SetParserType;
	end;

	// TRipGrepErrorLineParser = class(TRipGrepLineParserData)    // inharitance not supported of interface types
	//
	// end;

	TRipGrepHelpLineParser = class(TInterfacedObject, ILineParser) // inharitance not supported of interface implemetation!!!
		private
			FParserData : ILineParserData;

		public
			constructor Create;
			function ParseLine(const _iLnNr : integer; const _s : string; const _bIsLast : Boolean = False) : IParsedObjectRow;
	end;

implementation

uses
	System.IOUtils,
	RipGrepper.Tools.DebugTools,
	System.Classes,
	System.SysUtils;

constructor TRipGrepLineParserData.Create(const _type: TParserType; _regex: string);
begin
	inherited Create();
	FParserType := _type;
    FLineParseRegex := TRegex.Create(_regex);
end;

destructor TRipGrepLineParserData.Destroy;
begin
	inherited;
end;

function TRipGrepLineParserData.GetLineParseRegex : TRegex;
begin
	Result := FLineParseRegex;
end;

function TRipGrepLineParserData.GetParserType : TParserType;
begin
	Result := FParserType;
end;

procedure TRipGrepLineParserData.SetLineParseRegex(const Value : TRegex);
begin
	FLineParseRegex := Value;
end;

procedure TRipGrepLineParserData.SetParserType(const Value : TParserType);
begin
	FParserType := Value;
end;

{ TRipGrepHelpLineParser }

constructor TRipGrepHelpLineParser.Create;
begin
	inherited;
	FParserData := TRipGrepLineParserData.Create(ptRipGrepHelp, RG_HELP_LINE_REGEX);
end;

function TRipGrepHelpLineParser.ParseLine(const _iLnNr : integer; const _s : string; const _bIsLast : Boolean = False) : IParsedObjectRow;
begin
	Result := nil;
end;

end.
