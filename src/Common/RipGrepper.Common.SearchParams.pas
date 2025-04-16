unit RipGrepper.Common.SearchParams;

interface

uses
	RipGrepper.Common.Interfaces,
	RipGrepper.Common.SimpleTypes,
	RipGrepper.Common.SearchTextWithOptions, Spring;

type

	TSearchParams = class(TInterfacedObject, ISearchParams)
		private
			FGuiSearchTextParams : IShared<TSearchTextWithOptions>;

		public
			constructor Create(_gstParams : IShared<TSearchTextWithOptions>);
			function GetGuiSearchParams() : IShared<TSearchTextWithOptions>;
	end;

implementation

constructor TSearchParams.Create(_gstParams : IShared<TSearchTextWithOptions>);
begin
	inherited Create();
	FGuiSearchTextParams := _gstParams;
end;

function TSearchParams.GetGuiSearchParams : IShared<TSearchTextWithOptions>;
begin
	Result := FGuiSearchTextParams;
end;

end.
