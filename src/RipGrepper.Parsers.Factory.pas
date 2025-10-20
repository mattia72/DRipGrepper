unit RipGrepper.Parsers.Factory;

interface

uses
	RipGrepper.Common.Interfaces,
	RipGrepper.Common.SimpleTypes,
	System.Classes,
	ArrayEx;

type
	TRipGrepperParsersFactory = class
		public
			class function GetParser(_type : TParserType) : ISearchResultLineParser;
			class function TryGetParserType(_ripGrepArgs : TArrayEx<string>) : TParserType;
	end;

implementation

uses
	RipGrepper.Parsers.VimGrepMatchLine,
	RipGrepper.Parsers.JsonMatchLine,
	RipGrepper.Helper.Types,
	RipGrepper.Common.Constants,
	System.SysUtils,
	System.StrUtils;

class function TRipGrepperParsersFactory.GetParser(_type : TParserType) : ISearchResultLineParser;
begin
	case _type of
		ptRipGrepSearch :
		{ } Result := TVimGrepMatchLineParser.Create();
		ptRipGrepPrettySearch :
		{ } Result := TVimGrepPrettyMatchLineParser.Create();
		ptRipGrepJson :
		{ } Result := TJsonMatchLineParser.Create();
	end;
end;

class function TRipGrepperParsersFactory.TryGetParserType(_ripGrepArgs : TArrayEx<string>) : TParserType;
begin
	if _ripGrepArgs.HasMatch([OUTPUT_FORMAT_JSON]) then begin
		Result := ptRipGrepJson;
	end else if _ripGrepArgs.HasMatch(RG_PARAM_REGEX_PRETTY.Split(['|'])) then begin
		Result := ptRipGrepPrettySearch;
	end else begin
		Result := ptRipGrepSearch;
	end;
end;

end.
