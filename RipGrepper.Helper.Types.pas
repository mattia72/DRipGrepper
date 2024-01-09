unit RipGrepper.Helper.Types;

interface

uses
  System.Classes;

type
	TSortType = (stUnsorted, stAscending, stDescending);
    TParserType = (ptRipGrepSearch, ptRipGrepVersion, ptRipGrepSearchCutParent);

type
	TStringsHelper = class helper for TStrings
		function Contains(const s : string) : Boolean;
	end;

implementation

function TStringsHelper.Contains(const s : string) : Boolean;
begin
	Result := self.IndexOf(s) <> -1;
end;

end.
