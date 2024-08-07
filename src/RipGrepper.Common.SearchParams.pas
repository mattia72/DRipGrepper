unit RipGrepper.Common.SearchParams;

interface

uses
	RipGrepper.Common.GuiSearchParams,
	RipGrepper.Common.Interfaces;

type
	TSearchParams = class(TInterfacedObject, ISearchParams)
		private
			FGuiSearchTextParams : TGuiSearchTextParams;

		public
			constructor Create(const _gstParams : TGuiSearchTextParams);
			function GetGuiSearchParams : TGuiSearchTextParams;
	end;

implementation

constructor TSearchParams.Create(const _gstParams : TGuiSearchTextParams);
begin
	inherited Create();
	FGuiSearchTextParams := _gstParams;
end;

function TSearchParams.GetGuiSearchParams : TGuiSearchTextParams;
begin
	Result := FGuiSearchTextParams;
end;

end.
