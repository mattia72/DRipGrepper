unit RipGrepper.Settings.RipGrepArguments;

interface

uses
	System.Classes;

type
	TRipGrepArguments = class(TStringList)
		public
			function GetAdditionalExpertOptions : TArray<string>;
			function GetOptions() : TArray<string>;
			function GetSearchText() : string;
			function GetSearchPath() : TArray<string>;
	end;

implementation

uses
	RipGrepper.Helper.Types,
	RipGrepper.Common.Constants,
	RipGrepper.CommandLine.OptionStrings;

function TRipGrepArguments.GetAdditionalExpertOptions : TArray<string>;
var
	os : TOptionStrings;
begin
	os := TOptionStrings.New(self.GetOptions);
	// Remove necessary options
	os.RemoveOptions(RG_NECESSARY_PARAMS + RG_GUI_SET_PARAMS);
	Result := os.AsArray;
end;

function TRipGrepArguments.GetOptions() : TArray<string>;
begin
	Result := self.GetValues(RG_ARG_OPTIONS);
end;

function TRipGrepArguments.GetSearchText() : string;
begin
	Result := self.GetValues(RG_ARG_SEARCH_TEXT)[0];
end;

function TRipGrepArguments.GetSearchPath() : TArray<string>;
begin
	Result := self.GetValues(RG_ARG_SEARCH_Path);
end;

end.
