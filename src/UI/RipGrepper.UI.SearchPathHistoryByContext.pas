unit RipGrepper.UI.SearchPathHistoryByContext;

interface

uses
	System.SysUtils,
	ArrayEx,
	RipGrepper.Common.IDEContextValues,
	Spring.Collections;

type
	TSearchPathHistoryByContext = class
	private
		FItemsByContext : IDictionary<Integer, TArrayEx<string>>;

	public
		constructor Create();
		function GetForContext(const _context : EDelphiIDESearchContext) : TArrayEx<string>;
		procedure SetForContext(const _context : EDelphiIDESearchContext; const _items : TArrayEx<string>);
		procedure StorePathForContext(const _context : EDelphiIDESearchContext; const _searchPath : string);
	end;

implementation

constructor TSearchPathHistoryByContext.Create();
begin
	inherited Create;
	FItemsByContext := TCollections.CreateDictionary<Integer, TArrayEx<string>>();
end;

function TSearchPathHistoryByContext.GetForContext(const _context : EDelphiIDESearchContext) : TArrayEx<string>;
begin
	var
		contextKey := Ord(_context);
	if FItemsByContext.ContainsKey(contextKey) then begin
		Result := FItemsByContext[contextKey];
	end else begin
		Result.Clear;
	end;
end;

procedure TSearchPathHistoryByContext.SetForContext(const _context : EDelphiIDESearchContext;
	const _items : TArrayEx<string>);
begin
	var
		contextKey := Ord(_context);
	if FItemsByContext.ContainsKey(contextKey) then begin
		FItemsByContext[contextKey] := _items;
	end else begin
		FItemsByContext.Add(contextKey, _items);
	end;
end;

procedure TSearchPathHistoryByContext.StorePathForContext(const _context : EDelphiIDESearchContext;
	const _searchPath : string);
begin
	var
	path := _searchPath.Trim();
	if path.IsEmpty then begin
		Exit;
	end;

	var
	contextHist := GetForContext(_context);
	contextHist.InsertUnique(0, path);
	SetForContext(_context, contextHist);
end;

end.
