unit RipGrepper.Parsers.Factory;

interface

uses
	RipGrepper.Common.Interfaces,
	RipGrepper.Common.Types,
	System.Classes,
	ArrayEx;

type
	TRipGrepperParsersFactory = class

		private
		public
			class function GetParser(_type : TParserType) : ILineParser;
			class function TryGetParserType(_ripGrepArgs : TArrayEx<string>) : TParserType;
	end;

implementation

uses
	RipGrepper.Parsers.VimGrepMatchLine,
	RipGrepper.Helper.Types;

class function TRipGrepperParsersFactory.GetParser(_type : TParserType) : ILineParser;
begin
	case _type of
		ptRipGrepSearch :
		{ } Result := TVimGrepMatchLineParser.Create();
		ptRipGrepPrettySearch :
		{ } Result := TVimGrepPrettyMatchLineParser.Create();
	end;
end;

class function TRipGrepperParsersFactory.TryGetParserType(_ripGrepArgs : TArrayEx<string>) : TParserType;
begin
	if _ripGrepArgs.HasMatch(['-p','--pretty']) then begin
		Result := ptRipGrepPrettySearch;
	end else begin
		Result := ptRipGrepSearch;
	end;
end;

end.
