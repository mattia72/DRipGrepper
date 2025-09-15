unit ArrayEx;

/// ////////////////////////////////////////////////////////////////////////////
//
// ArrayHelper  version 1.3
// extends class TArray and add TArrayRecord<T> to make dynamic arrays
// as simple, as TList
//
// Copyright(c) 2017 by Willi Commer (wcs)
// Licence GNU
//
// Dynamic arrays are smart because its memore usage is handled by the memory
// manager. But the funtion libraries are lean and differs from object based.
// Based on TArray class, that gives Sort and Binary search, this unit will
// extend TArray with functions available for TList or TStrings.
// The next level is TArrayRecord<T> record type. It wraps a record around
// the dynamic array. This give us the ability to use dynamic arrays like
// objects with out the pain to organize the final Free call.
//
// var
// A: TArrayRecord<string>;
// S: string;
// begin
// A.SetValues(['a','b','c']);
// A.Add('d');
// assert(  A.Count = 4 );    // same as length(A.Items);
// assert(  A[1] = 'b' );
// assert(  A.IndexOf('a') = 0 );
// for S in A do
// ..
//
// For more examples see procedure Test_All_Helper_Functions
// For updates check https://github.com/WilliCommer/ArrayHelper
//
//
// History:
// version 1.3
// Enumeration added
// new functions 'Unique' and 'CopyArray'
//
// version 1.2
// TArrayRecord<T>
//
/// ////////////////////////////////////////////////////////////////////////////

{ $DEFINE TEST_FUNCTION }  // change to active test function

interface

uses
	System.Classes,
	System.SysUtils,
	System.RTLConsts,
	System.Generics.Defaults,
	System.Generics.Collections;

type

	// callback function for function ForEach
	TArrayForEachCallback<T> = reference to procedure(var Value : T; index : integer);

	// callback function for function Map
	TArrayMapCallback<T> = reference to function(var Value : T; index : integer) : boolean;

	// callback function for function MapTo
	TArrayConvert<T, TTo> = reference to function(const Value : T) : TTo;

	// callback function for function Find
	TArrayFindCallback<T> = reference to function(const Value : T) : boolean;

	// extends class TArray
	TArrayHelper = class helper for TArray
		// add item to array
		class function Add<T>(var Values : TArray<T>; const Item : T) : integer; static;

		// delete item at index
		class procedure Delete<T>(var Values : TArray<T>; Index : integer); static;

		// insert item at index
		class procedure Insert<T>(var Values: TArray<T>; Index: integer; const Value:
			T); static;

		// append array
		class procedure AddRange<T>(var Values : TArray<T>; const ValuesToInsert : array of T); static;
		// get index of equal item
		class function CountOf<T>(var Values: TArray<T>; const Item: T; const Comparer:
			IComparer<T>): integer; overload; static;

		// insert array at index
		class procedure InsertRange<T>(var Values : TArray<T>; Index : Integer; const ValuesToInsert : array of T); static;

		// get index of equal item
		class function IndexOf<T>(var Values: TArray<T>; const Item: T): integer;
			overload; static;

		// get index of equal item (using IComparer)
		class function IndexOf<T>(var Values: TArray<T>; const Item: T; const Comparer:
			IComparer<T>): integer; overload; static;

		// get index of maximal item
		class function IndexOfMax<T>(var Values : TArray<T>) : integer; overload; static;

		// get index of maximal item (using IComparer)
		class function IndexOfMax<T>(var Values : TArray<T>; const Comparer : IComparer<T>) : integer; overload; static;

		// get index of minimal item
		class function IndexOfMin<T>(var Values : TArray<T>) : integer; overload; static;

		// get index of minimal item (using IComparer)
		class function IndexOfMin<T>(var Values : TArray<T>; const Comparer : IComparer<T>) : integer; overload; static;

		// is a equal item is member of values
		class function Contains<T>(var Values : TArray<T>; const Item : T) : boolean; overload; static;

		// is a equal item is member of values (using IComparer)
		class function Contains<T>(var Values : TArray<T>; const Item : T; const Comparer : IComparer<T>) : boolean; overload; static;

		// compare two arrays
		class function Compare<T>(const Values, ValuesToCompare : array of T) : boolean; overload; static;

		// compare two arrays (using IComparer)
		class function Compare<T>(const Values, ValuesToCompare : array of T; const Comparer : IComparer<T>) : boolean; overload; static;

		// ForEach
		class procedure ForEach<T>(var Values : TArray<T>; const Callback : TArrayForEachCallback<T>); static;

		// find with callback
		class function Find<T>(const Values : TArray<T>; const Callback : TArrayFindCallback<T>; const StartIndex : integer = 0) : integer;
			overload; static;
		// get index of equal item
		class function AllIndexOf<T>(var Values : TArray<T>; Item : T; const Comparer : IComparer<T>) : TArray<integer>; overload; static;

		// return an array filtered and converted by callback function
		class function Map<T>(const Values : TArray<T>; const Callback : TArrayMapCallback<T>) : TArray<T>; static;

		{$IFDEF TEST_FUNCTION}
		// test, debug and example function
		class procedure Test_All_Helper_Functions;
		{$ENDIF TEST_FUNCTION}
	end;

type
	TArrayEx<T> = record
		strict private
		type
			TEnumerator = class
				private
					FValue : ^TArrayEx<T>;
					FIndex : integer;
					function GetCurrent : T;

				public
					constructor Create(var AValue : TArrayEx<T>);
					function MoveNext : Boolean;
					property Current : T read GetCurrent;
			end;

		public
			function GetEnumerator() : TEnumerator;

		private
			function GetCount : integer;
			function GetIsEmpty : Boolean;
			function GetFirst : T;
			procedure SetCount(const Value : integer);
			function GetItemAt(Index : integer) : T;
			function GetSafeItem(index : Integer) : T;
			function GetLast : T;
			function GetMaxIndex : Integer;
			procedure SetItemAt(Index : integer; Value : T);
			procedure SetSafeItem(index : Integer; const Value : T);

		public
			Items : TArray<T>;
			property Count : integer read GetCount write SetCount;
			property IsEmpty : Boolean read GetIsEmpty;
			property First : T read GetFirst;
			property ItemAt[index : Integer] : T read GetItemAt write SetItemAt; default;
			property SafeItem[index : Integer] : T read GetSafeItem write SetSafeItem;
			property Last : T read GetLast;
			property MaxIndex : Integer read GetMaxIndex;

			constructor Create(ACapacity : integer); overload;
			constructor Create(const AValues : array of T); overload;
			procedure Clear;
			procedure SetItems(const Values : array of T);
			function Add(const Value : T) : integer;
			procedure Delete(Index : integer); overload;
			procedure Insert(Index: integer; const Value: T);
			function Remove(const AItem : T) : boolean;
			function AddIfNotContains(const AItem : T) : Integer;
			function InsertIfNotContains(const Index : Integer; const AItem : T) : boolean;

			procedure AddRange(const ValuesToInsert : array of T); overload;
			procedure AddRange(const ValuesToInsert : TArrayEx<T>); overload;

			procedure InsertRange(Index : Integer; const ValuesToInsert : array of T); overload;
			procedure InsertRange(Index : Integer; const ValuesToInsert : TArrayEx<T>); overload;

			function IndexOf(const Item: T): integer; overload;
			function IndexOf(const Item: T; const Comparer: IComparer<T>): integer;
				overload;

			function IndexOfMax : integer; overload;
			function IndexOfMax(const Comparer : IComparer<T>) : integer; overload;
			function IndexOfMin : integer; overload;
			function IndexOfMin(const Comparer : IComparer<T>) : integer; overload;

			function Contains(const Item : T) : boolean; overload;
			function Contains(const Item : T; const Comparer : IComparer<T>) : boolean; overload;

			function Compare(const ValuesToCompare : array of T) : boolean; overload;
			function Compare(const ValuesToCompare : array of T; const Comparer : IComparer<T>) : boolean; overload;
			function Compare(const ValuesToCompare : TArrayEx<T>) : boolean; overload;
			function Compare(const ValuesToCompare : TArrayEx<T>; const Comparer : IComparer<T>) : boolean; overload;

			procedure ForEach(const Callback : TArrayForEachCallback<T>);
			function Find(const Callback : TArrayFindCallback<T>; const StartIndex : integer = 0) : integer; overload;
			function Map(const Callback : TArrayMapCallback<T>) : TArrayEx<T>;
			function Convert<TTo>(const Callback : TArrayConvert<T, TTo>) : TArrayEx<TTo>;

			procedure Sort; overload;
			procedure Sort(const AComparer : IComparer<T>); overload;
			procedure Sort(const AComparer : IComparer<T>; AIndex, ACount : Integer); overload;
			function BinarySearch(const AItem : T; out AFoundIndex : Integer; const AComparer : IComparer<T>; AIndex, ACount : Integer)
				: Boolean; overload;
			function BinarySearch(const AItem : T; out AFoundIndex : Integer; const AComparer : IComparer<T>) : Boolean; overload;
			function BinarySearch(const AItem : T; out AFoundIndex : Integer) : Boolean; overload;
			function HasMatch(const Values : TArray<T>) : boolean; overload;
			function GetFirstMatchIndex(const Values : TArray<T>) : integer; overload;

			procedure Unique; // remove duplicates
			function GetRange(const _idx : integer; const _count : integer = -1) : TArrayEx<T>;
			procedure Delete(Indexes : TArrayEx<integer>); overload;
			procedure Delete(Indexes : TArray<integer>); overload;
			function AllIndexOf(Item : T; const Comparer : IComparer<T>) : TArray<integer>; overload;
			function AllIndexOf(Item : T) : TArray<integer>; overload;
			function GetReversedRange(const _idx : integer = -1; const _count : integer = -1) : TArrayEx<T>;
			function CountOf(const Item: T): integer; overload;
			function CountOf(const Item: T; const Comparer: IComparer<T>): integer;
				overload;
			function InsertUnique(const Index : Integer; const AItem : T) : boolean;
			function RemoveAll(const AItem : T) : boolean;
			// operator overloads
			class operator Equal(const L, R : TArrayEx<T>) : boolean;
			class operator Implicit(const Values : array of T) : TArrayEx<T>;
			class operator Implicit(const Values : TArray<T>) : TArrayEx<T>;
			class operator Implicit(const Values : TArrayEx<T>) : TArray<T>;
			class operator NotEqual(const L, R : TArrayEx<T>) : boolean;
	end;

implementation

uses
	System.Math;

{ TArrayHelper }

class function TArrayHelper.Add<T>(var Values : TArray<T>; const Item : T) : integer;
begin
	Result := Length(Values);
	SetLength(Values, Result + 1);
	Values[Result] := Item;
end;

class procedure TArrayHelper.Delete<T>(var Values : TArray<T>; Index : integer);
var
	I : Integer;
begin
	if (index < low(Values)) or (index > high(Values)) then
		raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
	for I := index + 1 to high(Values) do
		Values[I - 1] := Values[I];
	SetLength(Values, length(Values) - 1);
end;

class procedure TArrayHelper.Insert<T>(var Values: TArray<T>; Index: integer;
	const Value: T);
var
	I, H : Integer;
begin
	if (index < low(Values)) or (index > length(Values)) then
		raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
	H := high(Values);
	SetLength(Values, length(Values) + 1);
	for I := H downto index do
		Values[I + 1] := Values[I];
	Values[index] := Value;
end;

class procedure TArrayHelper.InsertRange<T>(var Values : TArray<T>; Index : Integer; const ValuesToInsert : array of T);
var
	I, L, H : Integer;
begin
	L := length(ValuesToInsert);
	if L = 0 then
		EXIT;
	if (index < low(Values)) or (index > length(Values)) then
		raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
	H := high(Values);
	SetLength(Values, length(Values) + L);
	for I := H downto index do
		Values[I + L] := Values[I];
	for I := low(ValuesToInsert) to high(ValuesToInsert) do
		Values[index + I] := ValuesToInsert[I];
end;

class procedure TArrayHelper.AddRange<T>(var Values : TArray<T>; const ValuesToInsert : array of T);
var
	I, Index : Integer;
begin
	index := length(Values);
	SetLength(Values, length(Values) + length(ValuesToInsert));
	for I := low(ValuesToInsert) to high(ValuesToInsert) do
		Values[index + I] := ValuesToInsert[I];
end;

class function TArrayHelper.CountOf<T>(var Values: TArray<T>; const Item: T;
	const Comparer: IComparer<T>): integer;
begin
	Result := 0;
	for var i := low(Values) to high(Values) do begin
		if Comparer.Compare(Values[i], Item) = 0 then begin
			Inc(Result);
		end;
	end;
end;

class function TArrayHelper.IndexOf<T>(var Values: TArray<T>; const Item: T;
	const Comparer: IComparer<T>): integer;
begin
	for Result := low(Values) to high(Values) do
		if Comparer.Compare(Values[Result], Item) = 0 then
			EXIT;
	Result := -1;
end;

class function TArrayHelper.IndexOf<T>(var Values: TArray<T>; const Item: T):
	integer;
begin
	Result := IndexOf<T>(Values, Item, TComparer<T>.Default);
end;

class function TArrayHelper.IndexOfMax<T>(var Values : TArray<T>) : integer;
begin
	Result := IndexOfMax<T>(Values, TComparer<T>.Default);
end;

class function TArrayHelper.IndexOfMax<T>(var Values : TArray<T>; const Comparer : IComparer<T>) : integer;
var
	I : Integer;
begin
	if length(Values) = 0 then
		raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
	Result := 0;
	for I := 1 to high(Values) do
		if Comparer.Compare(Values[I], Values[Result]) > 0 then
			Result := I;
end;

class function TArrayHelper.IndexOfMin<T>(var Values : TArray<T>) : integer;
begin
	Result := IndexOfMin<T>(Values, TComparer<T>.Default);
end;

class function TArrayHelper.IndexOfMin<T>(var Values : TArray<T>; const Comparer : IComparer<T>) : integer;
var
	I : Integer;
begin
	if length(Values) = 0 then
		raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
	Result := 0;
	for I := 1 to high(Values) do
		if Comparer.Compare(Values[I], Values[Result]) < 0 then
			Result := I;
end;

class function TArrayHelper.Contains<T>(var Values : TArray<T>; const Item : T; const Comparer : IComparer<T>) : boolean;
begin
	Result := IndexOf<T>(Values, Item, Comparer) <> -1;
end;

class function TArrayHelper.Contains<T>(var Values : TArray<T>; const Item : T) : boolean;
begin
	Result := contains<T>(Values, Item, TComparer<T>.Default);
end;

class function TArrayHelper.Compare<T>(const Values, ValuesToCompare : array of T; const Comparer : IComparer<T>) : boolean;
var
	I : Integer;
begin
	if length(Values) <> length(ValuesToCompare) then
		EXIT(FALSE);
	for I := low(Values) to high(Values) do
		if Comparer.Compare(Values[I], ValuesToCompare[I]) <> 0 then
			EXIT(FALSE);
	Result := TRUE;
end;

class function TArrayHelper.Compare<T>(const Values, ValuesToCompare : array of T) : boolean;
begin
	Result := Compare<T>(Values, ValuesToCompare, TComparer<T>.Default);
end;

class procedure TArrayHelper.ForEach<T>(var Values : TArray<T>; const Callback : TArrayForEachCallback<T>);
var
	I : Integer;
begin
	for I := low(Values) to high(Values) do
		Callback(Values[I], I);
end;

class function TArrayHelper.Find<T>(const Values : TArray<T>; const Callback : TArrayFindCallback<T>; const StartIndex : integer) : integer;
begin
	if (length(Values) = 0) or (StartIndex < 0) or (StartIndex > high(Values)) then
		EXIT(-1);
	for Result := StartIndex to high(Values) do
		if Callback(Values[Result]) then
			EXIT;
	Result := -1;
end;

class function TArrayHelper.AllIndexOf<T>(var Values : TArray<T>; Item : T; const Comparer : IComparer<T>) : TArray<integer>;
begin
	Result := [];
	for var i := low(Values) to high(Values) do begin
		if Comparer.Compare(Values[i], Item) = 0 then begin
			Result := Result + [i];
		end;
	end;
end;

class function TArrayHelper.Map<T>(const Values : TArray<T>; const Callback : TArrayMapCallback<T>) : TArray<T>;
var
	Item : T;
	I : Integer;
begin
	Result := nil;
	for I := low(Values) to high(Values) do begin
		Item := Values[I];
		if Callback(Item, I) then
			Add<T>(Result, Item);
	end;
end;

{ TArrayRecord<T>.TEnumerator }

constructor TArrayEx<T>.TEnumerator.Create(var AValue : TArrayEx<T>);
begin
	FValue := @AValue;
	FIndex := -1;
end;

function TArrayEx<T>.TEnumerator.GetCurrent : T;
begin
	Result := FValue^.Items[FIndex];
end;

function TArrayEx<T>.TEnumerator.MoveNext : Boolean;
begin
	Result := FIndex < high(FValue^.Items);
	Inc(FIndex);
end;

{ TArrayRecord<T> }

constructor TArrayEx<T>.Create(ACapacity : integer);
begin
	SetLength(Items, ACapacity);
end;

constructor TArrayEx<T>.Create(const AValues : array of T);
begin
	SetLength(Items, 0);
	AddRange(AValues);
end;

procedure TArrayEx<T>.Clear;
begin
	SetLength(Items, 0);
end;

class operator TArrayEx<T>.Equal(const L, R : TArrayEx<T>) : boolean;
begin
	Result := L.Compare(R);
end;

class operator TArrayEx<T>.NotEqual(const L, R : TArrayEx<T>) : boolean;
begin
	Result := not L.Compare(R);
end;

function TArrayEx<T>.GetCount : integer;
begin
	Result := length(Items);
end;

function TArrayEx<T>.GetEnumerator : TEnumerator;
begin
	Result := TEnumerator.Create(Self);
end;

procedure TArrayEx<T>.SetCount(const Value : integer);
begin
	SetLength(Items, Value);
end;

procedure TArrayEx<T>.SetItemAt(Index : integer; Value : T);
begin
	Items[index] := Value;
end;

procedure TArrayEx<T>.SetItems(const Values : array of T);
begin
	SetLength(Items, 0);
	AddRange(Values);
end;

function TArrayEx<T>.GetItemAt(Index : integer) : T;
begin
	Result := Items[index];
end;

procedure TArrayEx<T>.AddRange(const ValuesToInsert : array of T);
begin
	TArray.AddRange<T>(Items, ValuesToInsert);
end;

procedure TArrayEx<T>.AddRange(const ValuesToInsert : TArrayEx<T>);
begin
	TArray.AddRange<T>(Items, ValuesToInsert.Items);
end;

function TArrayEx<T>.BinarySearch(const AItem : T; out AFoundIndex : Integer; const AComparer : IComparer<T>; AIndex, ACount : Integer)
	: Boolean;
begin
	Result := TArray.BinarySearch<T>(Items, AItem, AFoundIndex, AComparer, AIndex, ACount);
end;

function TArrayEx<T>.BinarySearch(const AItem : T; out AFoundIndex : Integer; const AComparer : IComparer<T>) : Boolean;
begin
	Result := TArray.BinarySearch<T>(Items, AItem, AFoundIndex, AComparer);
end;

function TArrayEx<T>.BinarySearch(const AItem : T; out AFoundIndex : Integer) : Boolean;
begin
	Result := TArray.BinarySearch<T>(Items, AItem, AFoundIndex);
end;

procedure TArrayEx<T>.Delete(Index : integer);
begin
	TArray.Delete<T>(Items, index);
end;

function TArrayEx<T>.Remove(const AItem : T) : boolean;
var
	I : integer;
begin
	I := IndexOf(AItem);
	if I < 0 then
		Result := FALSE
	else begin
		Delete(I);
		Result := TRUE;
	end;
end;

function TArrayEx<T>.AddIfNotContains(const AItem : T) : Integer;
begin
	Result := -1;
	if not contains(AItem) then
		Result := Add(AItem);
end;

function TArrayEx<T>.Find(const Callback : TArrayFindCallback<T>; const StartIndex : integer) : integer;
begin
	Result := TArray.Find<T>(Items, Callback, StartIndex);
end;

procedure TArrayEx<T>.ForEach(const Callback : TArrayForEachCallback<T>);
begin
	TArray.ForEach<T>(Items, Callback);
end;

function TArrayEx<T>.Compare(const ValuesToCompare : TArrayEx<T>) : boolean;
begin
	Result := TArray.Compare<T>(Items, ValuesToCompare.Items);
end;

function TArrayEx<T>.Compare(const ValuesToCompare : TArrayEx<T>; const Comparer : IComparer<T>) : boolean;
begin
	Result := TArray.Compare<T>(Items, ValuesToCompare.Items, Comparer);
end;

function TArrayEx<T>.Compare(const ValuesToCompare : array of T) : boolean;
begin
	Result := TArray.Compare<T>(Items, ValuesToCompare);
end;

function TArrayEx<T>.Compare(const ValuesToCompare : array of T; const Comparer : IComparer<T>) : boolean;
begin
	Result := TArray.Compare<T>(Items, ValuesToCompare, Comparer);
end;

function TArrayEx<T>.Contains(const Item : T; const Comparer : IComparer<T>) : boolean;
begin
	Result := TArray.Contains<T>(Items, Item, Comparer);
end;

function TArrayEx<T>.Contains(const Item : T) : boolean;
begin
	Result := TArray.Contains<T>(Items, Item);
end;

function TArrayEx<T>.IndexOf(const Item: T; const Comparer: IComparer<T>):
	integer;
begin
	Result := TArray.IndexOf<T>(Items, Item, Comparer);
end;

function TArrayEx<T>.IndexOfMax : integer;
begin
	Result := TArray.IndexOfMax<T>(Items);
end;

function TArrayEx<T>.IndexOfMax(const Comparer : IComparer<T>) : integer;
begin
	Result := TArray.IndexOfMax<T>(Items, Comparer);
end;

function TArrayEx<T>.IndexOfMin : integer;
begin
	Result := TArray.IndexOfMin<T>(Items);
end;

function TArrayEx<T>.IndexOfMin(const Comparer : IComparer<T>) : integer;
begin
	Result := TArray.IndexOfMin<T>(Items, Comparer);
end;

function TArrayEx<T>.IndexOf(const Item: T): integer;
begin
	Result := TArray.IndexOf<T>(Items, Item);
end;

procedure TArrayEx<T>.Insert(Index: integer; const Value: T);
begin
	TArray.Insert<T>(Items, index, Value);
end;

procedure TArrayEx<T>.InsertRange(Index : Integer; const ValuesToInsert : TArrayEx<T>);
begin
	TArray.InsertRange<T>(Items, index, ValuesToInsert.Items);
end;

procedure TArrayEx<T>.InsertRange(Index : Integer; const ValuesToInsert : array of T);
begin
	TArray.InsertRange<T>(Items, index, ValuesToInsert);
end;

function TArrayEx<T>.Map(const Callback : TArrayMapCallback<T>) : TArrayEx<T>;
begin
	Result.Items := TArray.Map<T>(Items, Callback);
end;

function TArrayEx<T>.Convert<TTo>(const Callback : TArrayConvert<T, TTo>) : TArrayEx<TTo>;
var
	I : Integer;
begin
	Result.Clear;
	for I := low(Items) to high(Items) do
		Result.Add(Callback(Items[I]));
end;

function TArrayEx<T>.GetRange(const _idx : integer; const _count : integer = -1) : TArrayEx<T>;
var
	copyCount : integer;
begin
	Result.Clear;
	copyCount := _count;
	if _count < 0 then
		copyCount := Count;
	if Count < (_idx + _count) then
		copyCount := Count - _idx;
	if copyCount > 0 then begin
		SetLength(Result.Items, copyCount);
		for var i := _idx to copyCount - 1 do begin
			Result.Items[i] := Items[i];
		end;
	end;
end;

function TArrayEx<T>.CountOf(const Item: T): integer;
begin
	Result := TArray.CountOf<T>(Items, Item, TComparer<T>.Default);
end;

function TArrayEx<T>.CountOf(const Item: T; const Comparer: IComparer<T>):
	integer;
begin
	Result := TArray.CountOf<T>(Items, Item, Comparer);
end;

procedure TArrayEx<T>.Sort;
begin
	TArray.Sort<T>(Items);
end;

procedure TArrayEx<T>.Sort(const AComparer : IComparer<T>);
begin
	TArray.Sort<T>(Items, AComparer);
end;

procedure TArrayEx<T>.Sort(const AComparer : IComparer<T>; AIndex, ACount : Integer);
begin
	TArray.Sort<T>(Items, AComparer, AIndex, ACount);
end;

function TArrayEx<T>.Add(const Value : T) : integer;
begin
	Result := TArray.Add<T>(Items, Value);
end;

function TArrayEx<T>.InsertIfNotContains(const Index : Integer; const AItem : T) : boolean;
begin
	Result := not contains(AItem);
	if Result then begin
		Insert(index, AItem);
	end;
end;

procedure TArrayEx<T>.Delete(Indexes : TArrayEx<integer>);
begin
	Delete(Indexes.Items);
end;

procedure TArrayEx<T>.Delete(Indexes : TArray<integer>);
begin
	TArray.Sort<integer>(Indexes);
	for var i := Length(Indexes) - 1 downto 0 do begin
		Delete(Indexes[i]);
	end;
end;

function TArrayEx<T>.GetMaxIndex : Integer;
begin
	Result := Count - 1;
end;

function TArrayEx<T>.HasMatch(const Values : TArray<T>) : boolean;
begin
	Result := False;
	for var v : T in Values do begin
		if TArray.Contains<T>(Items, v) then begin
			Result := True;
			Exit
		end;
	end;
end;

function TArrayEx<T>.AllIndexOf(Item : T; const Comparer : IComparer<T>) : TArray<integer>;
begin
	Result := TArray.AllIndexOf<T>(Items, Item, Comparer);
end;

function TArrayEx<T>.AllIndexOf(Item : T) : TArray<integer>;
begin
	Result := TArray.AllIndexOf<T>(Items, Item, TComparer<T>.Default);
end;

function TArrayEx<T>.GetReversedRange(const _idx : integer = -1; const _count : integer = -1) : TArrayEx<T>;
var
	copyCount : integer;
	idx : Integer;
begin
	Result.Clear;
	idx := IfThen(_idx < 0, MaxIndex, _idx);

	copyCount := _count;
	if _count < 0 then
		copyCount := Count;
	if _count > idx + 1 then
		copyCount := idx + 1;

	SetLength(Result.Items, copyCount);
	var
	j := 0;
	for var i := idx downto 0 do begin
		Result.Items[j] := Items[i];
		if copyCount = (j + 1) then begin
			break;
		end;
		Inc(j);
	end;
end;

function TArrayEx<T>.GetIsEmpty : Boolean;
begin
	Result := length(Items) = 0;
end;

function TArrayEx<T>.GetFirst : T;
begin
	Result := Items[0];
end;

function TArrayEx<T>.GetSafeItem(index : Integer) : T;
begin
	if index <= MaxIndex then begin
		Result := Items[index];
	end else begin
		Result := default (T);
	end;
end;

function TArrayEx<T>.GetLast : T;
begin
	Result := Items[MaxIndex];
end;

function TArrayEx<T>.InsertUnique(const Index : Integer; const AItem : T) : boolean;
begin
	Result := not contains(AItem);
	if Result then begin
		Insert(index, AItem);
	end else begin
		Remove(AItem);
		Insert(index, AItem);
	end;
end;

function TArrayEx<T>.RemoveAll(const AItem : T) : boolean;
var
	I : integer;
begin
	Result := FALSE;
	I := IndexOf(AItem);
	while I >= 0 do begin
		Delete(I);
		I := IndexOf(AItem);
		Result := TRUE;
	end;
end;

procedure TArrayEx<T>.SetSafeItem(index : Integer; const Value : T);
begin
	if index <= MaxIndex then begin
		Items[index] := Value;
	end else begin
		var
		newIdx := MaxIndex + 1;
		var arr : TArray<T> := [default (T)];
		while newIdx < index do begin
			arr := arr + [default (T)];
			Inc(newIdx);
		end;

		AddRange(arr);
	end;
end;

procedure TArrayEx<T>.Unique;
var
	Hash : TDictionary<T, integer>;
	I : Integer;
begin
	Hash := TDictionary<T, integer>.Create(length(Items));
	try
		for I := low(Items) to high(Items) do
			Hash.AddOrSetValue(Items[I], 0);
		Items := Hash.Keys.ToArray;
	finally
		Hash.Free;
	end;
end;

class operator TArrayEx<T>.Implicit(const Values : array of T) : TArrayEx<T>;
begin
	Result := TArrayEx<T>.Create(Values);
end;

class operator TArrayEx<T>.Implicit(const Values : TArray<T>) : TArrayEx<T>;
begin
	Result := TArrayEx<T>.Create(Values);
end;

class operator TArrayEx<T>.Implicit(const Values : TArrayEx<T>) : TArray<T>;
begin
	Result := Values.Items;
end;

function TArrayEx<T>.GetFirstMatchIndex(const Values: TArray<T>): integer;
begin
	Result := -1;
	for var i : integer := Low(Values) to High(Values) do begin
		if TArray.Contains<T>(Items, Values[i]) then begin
			Result := i;
			Exit
		end;
	end;
end;

end.

