unit RipGrepper.Common.SearchParams;

interface

uses
	RipGrepper.Common.GuiSearchParams,
	RipGrepper.Common.Interfaces,
	RipGrepper.Common.SimpleTypes;

type
	TSearchParams = class(TInterfacedObject, ISearchParams)
		private
			FGuiSearchTextParams : TGuiSearchTextParams;

		public
			constructor Create(const _gstParams : TGuiSearchTextParams);
			class function GetAsSearchOptionSet(const _bMC, _bMW, _bUR : Boolean) : TSearchOptionSet; static;
			function GetGuiSearchParams : TGuiSearchTextParams;
			class function StringToSearchParams(const s : string): TSearchOptionSet;
	end;

implementation

uses
  System.SysUtils;

constructor TSearchParams.Create(const _gstParams : TGuiSearchTextParams);
begin
	inherited Create();
	FGuiSearchTextParams := _gstParams;
end;

class function TSearchParams.GetAsSearchOptionSet(const _bMC, _bMW, _bUR : Boolean) : TSearchOptionSet;
begin
	Result := [eGuiOption.soNotSet];
	if _bMC then
		Include(Result, EGuiOption.soMatchCase);
	if _bMW then
		Include(Result, EGuiOption.soMatchWord);
	if _bUR then
		Include(Result, EGuiOption.soUseRegex);

	if Result <> [EGuiOption.soNotSet] then begin
		Exclude(Result, EGuiOption.soNotSet);
	end;
end;

function TSearchParams.GetGuiSearchParams : TGuiSearchTextParams;
begin
	// if not FGuiSearchTextParams.IsAlreadyRead then begin
	// FGuiSearchTextParams.ReadIni;
	// FGuiSearchTextParams.LoadDefault;
	// end;
	Result := FGuiSearchTextParams;
end;

class function TSearchParams.StringToSearchParams(const s : string) : TSearchOptionSet;
begin
	Result := GetAsSearchOptionSet(
		{ } s.Contains('MatchCase'),
		{ } s.Contains('MatchWord'),
		{ } s.Contains('UseRegex'));
end;

end.
