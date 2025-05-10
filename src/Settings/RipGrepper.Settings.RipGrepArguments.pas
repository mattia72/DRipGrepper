unit RipGrepper.Settings.RipGrepArguments;

interface

uses
	System.Classes,
	ArrayEx;

type
	TRipGrepArguments = class(TStringList)
		public
			function GetOptions() : TArrayEx<string>;
			function GetSearchText(): string;
			function GetSearchPath(): TArrayEx<string>;
	end;

implementation

uses
	RipGrepper.Helper.Types,
	RipGrepper.Common.Constants;

function TRipGrepArguments.GetOptions() : TArrayEx<string>;
begin
	Result := self.GetValues(RG_ARG_OPTIONS);
end;

function TRipGrepArguments.GetSearchText(): string;
begin
	Result := self.GetValues(RG_ARG_SEARCH_TEXT)[0];
end;

function TRipGrepArguments.GetSearchPath(): TArrayEx<string>;
begin
	Result := self.GetValues(RG_ARG_SEARCH_Path);
end;

end.
