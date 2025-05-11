unit RipGrepper.Settings.RipGrepArguments;

interface

uses
	System.Classes;

type
	TRipGrepArguments = class(TStringList)
		public
			function GetOptions() : TArray<string>;
			function GetSearchText(): string;
			function GetSearchPath(): TArray<string>;
	end;

implementation

uses
	RipGrepper.Helper.Types,
	RipGrepper.Common.Constants;

function TRipGrepArguments.GetOptions(): TArray<string>;
begin
	Result := self.GetValues(RG_ARG_OPTIONS);
end;

function TRipGrepArguments.GetSearchText(): string;
begin
	Result := self.GetValues(RG_ARG_SEARCH_TEXT)[0];
end;

function TRipGrepArguments.GetSearchPath(): TArray<string>;
begin
	Result := self.GetValues(RG_ARG_SEARCH_Path);
end;

end.
