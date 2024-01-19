unit RipGrepper.Common.Sorter;

interface

uses
	System.Generics.Defaults,
	System.Generics.Collections, ArrayHelper;

type
	TSortCriterion<T> = class(TObject)
		Ascending : Boolean;
		Comparer : IComparer<T>;
	end;

	TSortCriteriaComparer<T> = class(TComparer<T>)
		private
			SortCriteria : TObjectList<TSortCriterion<T>>;

		public
			constructor Create;
			destructor Destroy; override;
			function Compare(const Right, Left : T) : Integer; override;
			procedure ClearCriteria; virtual;
			procedure AddCriterion(NewCriterion : TSortCriterion<T>); virtual;
	end;

	TSortDirectionType = (stUnsorted, stAscending, stDescending);
	TSortByType = (sbtFile, sbtRow, sbtCol, sbtText, sbtLineNr);

	TSortColumnDirection = record
		Column : TSortByType;
		Direction : TSortDirectionType;
		class function New(const _sbt : TSortByType; const _st : TSortDirectionType) : TSortColumnDirection; overload; static;
		class function New(const _sbt : TSortByType; const _bDescending : Boolean = False) : TSortColumnDirection; overload; static;
	end;

	TSortTypeDirectionList = record
		Items : TArrayRecord<TSortColumnDirection>;
		procedure Add(_scd : TSortColumnDirection);
		procedure Delete(const _sbt : TSortByType);

		public
			procedure MoveToEnd(const _sbt : TSortByType; const _st : TSortDirectionType);
			procedure MoveToStart(const _sbt : TSortByType; const _st : TSortDirectionType);
	end;

implementation

{ TSortCriteriaComparer<T> }

procedure TSortCriteriaComparer<T>.AddCriterion(NewCriterion : TSortCriterion<T>);
begin
	SortCriteria.Add(NewCriterion);
end;

procedure TSortCriteriaComparer<T>.ClearCriteria;
begin
	SortCriteria.Clear;
end;

function TSortCriteriaComparer<T>.Compare(const Right, Left : T) : Integer;
var
	Criterion : TSortCriterion<T>;
begin
	for Criterion in SortCriteria do begin
		Result := Criterion.Comparer.Compare(Right, Left);
		if not Criterion.Ascending then
			Result := -Result;
		if Result <> 0 then
			Exit;
	end;
end;

constructor TSortCriteriaComparer<T>.Create;
begin
	inherited;
	SortCriteria := TObjectList < TSortCriterion < T >>.Create(True);
end;

destructor TSortCriteriaComparer<T>.Destroy;
begin
	SortCriteria.Free;
	inherited;
end;

class function TSortColumnDirection.New(const _sbt : TSortByType; const _st : TSortDirectionType) : TSortColumnDirection;
begin
	Result.Column := _sbt;
	Result.Direction := _st;
end;

class function TSortColumnDirection.New(const _sbt : TSortByType; const _bDescending : Boolean = False) : TSortColumnDirection;
begin
	Result.Column := _sbt;
	if _bDescending then begin
		Result.Direction := stDescending;
	end else begin
		Result.Direction := stAscending;
	end;
end;

procedure TSortTypeDirectionList.Add(_scd : TSortColumnDirection);
begin
	Items.Add(_scd);
end;

procedure TSortTypeDirectionList.Delete(const _sbt : TSortByType);
begin
	var
	i := Items.Find(
		function(const val : TSortColumnDirection) : boolean
		begin
			Result := val.Column = _sbt;
		end, 0);
	if i >= 0 then
		Items.Delete(i);
end;

procedure TSortTypeDirectionList.MoveToEnd(const _sbt : TSortByType; const _st : TSortDirectionType);
begin
	Delete(_sbt);
	Items.Add(TSortColumnDirection.New(_sbt, _st));
end;

procedure TSortTypeDirectionList.MoveToStart(const _sbt : TSortByType; const _st : TSortDirectionType);
var
	new : TArrayRecord<TSortColumnDirection>;
begin
	new.Add(TSortColumnDirection.New(_sbt, _st));
	Delete(_sbt);
	new.AddRange(Items);
	Items := new;
end;

end.
