unit RipGrepper.Common.Types;

interface

uses
  System.Classes;

type
	TSortType = (stUnsorted, stAscending, stDescending);

    TParserType = (ptRipGrepSearch, ptRipGrepVersion, ptRipGrepSearchCutParent);

    IParser<T> = interface
        procedure ParseLineParseLine(var _m : T; const _s : string);
    end;

	TStringsHelper = class helper for TStrings
		function Contains(const s : string) : Boolean;
	end;

implementation

function TStringsHelper.Contains(const s : string) : Boolean;
begin
	Result := self.IndexOf(s) <> -1;
end;

end.
