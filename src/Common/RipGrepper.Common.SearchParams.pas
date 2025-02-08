unit RipGrepper.Common.SearchParams;

interface

uses
	RipGrepper.Common.Interfaces,
	RipGrepper.Common.SimpleTypes,
	RipGrepper.Common.SearchTextWithOptions;

type

	TSearchParams = class(TInterfacedObject, ISearchParams)
		private
			FGuiSearchTextParams : TSearchTextWithOptions;

		public
			constructor Create(const _gstParams : TSearchTextWithOptions);
			function GetGuiSearchParams : TSearchTextWithOptions;
	end;

implementation

uses
	System.SysUtils;

constructor TSearchParams.Create(const _gstParams : TSearchTextWithOptions);
begin
	inherited Create();
	FGuiSearchTextParams := _gstParams;
end;

function TSearchParams.GetGuiSearchParams : TSearchTextWithOptions;
begin
	Result := FGuiSearchTextParams;
end;

end.
