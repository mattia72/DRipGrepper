{: This unit implements a Quicksort procedure that can
   be used to sort anything as well as a binary sarch
   function.
   @author(Thomas Mueller http://www.dummzeuch.de)
}
unit u_dzQuicksort;

{$INCLUDE 'dzlib.inc'}

interface

uses
  u_dzSortUtils;

type
  // for binary search
  TCompareToItemMeth1 = function(const _Key; _Idx: Integer): Integer of object;
  TCompareToItemMeth2 = function(_Key: pointer; _Idx: Integer): Integer of object;

///<summary>
/// Call Quicksort with two method pointers for
///   comparing and swapping two elements.
///   @longcode(##
///     Quicksort(0, Count-1, self.CompareItems, self.SwapItems);
///   ##) }
procedure QuickSort(_Left, _Right: Integer; _CompareMeth: TCompareItemsMeth;
  _SwapMeth: TSwapItemsMeth); overload;

///<summary>
/// Call Quicksort with an interface that does the comparison and swapping
///   comparing and swapping two elements.
///   @longcode(##
///     Quicksort(0, Count-1, CompareAndSwapInterface);
///   ##) }
procedure QuickSort(_Left, _Right: Integer; _DataHandler: ISortDataHandler); overload;

///<summary>
/// Call BinarySearch with a method pointer that
///   compares an index to the Item sought.
///   @param Index contains the index where the item is supposed to be
///                (Its index, if it was found or the index where it would be inserted if not)
///   @param Duplicates determines whether duplicates are allowed in the list or not
///   @returns true, if the item was found, false otherwise
///   @longcode(##
///     Found := BinarySearch(0, count-1, Idx, Key, Self.CompareToKey);
///   ##) }
function BinarySearch(_Left, _Right: Integer; out _Index: Integer;
  const _Key; _CompareMeth: TCompareToItemMeth1;
  _Duplicates: Boolean = False): Boolean; overload;

function BinarySearch(_Left, _Right: Integer; out _Index: Integer;
  _Key: pointer; _CompareMeth: TCompareToItemMeth2;
  _Duplicates: Boolean = False): Boolean; overload;

type
  ICompareToKey = interface ['{CEB61050-D71F-4F67-B9BC-FD496A079F75}']
    function CompareTo(_Idx: Integer): Integer;
  end;

function BinarySearch(_Left, _Right: Integer; out _Index: Integer;
  _CompareInt: ICompareToKey; _Duplicates: Boolean = False): Boolean; overload;

implementation

procedure QuickSort(_Left, _Right: Integer; _DataHandler: ISortDataHandler); overload;
var
  I, J, P: Integer;
begin
  if _Left >= _Right then
    Exit; //==>
  repeat
    I := _Left;
    J := _Right;
//    P := (_Left + _Right) shr 1;
    // Chosing the pivot element can make a big difference:
    // In my unit tests its factor 100 for the TestPartSortedPartReverse test
    P := GetPivot(I, J, _DataHandler);
    repeat
      while _DataHandler.Compare(I, P) < 0 do
        Inc(I);
      while _DataHandler.Compare(J, P) > 0 do
        Dec(J);
      if I <= J then begin
        if I < J then
          _DataHandler.Swap(I, J);
        if P = I then
          P := J
        else if P = J then
          P := I;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if _Left < J then
      QuickSort(_Left, J, _DataHandler);
    _Left := I;
  until I >= _Right;
end;

procedure QuickSort(_Left, _Right: Integer; _CompareMeth: TCompareItemsMeth;
  _SwapMeth: TSwapItemsMeth);
var
  I, J, P: Integer;
begin
  if _Left >= _Right then
    Exit; //==>
  repeat
    I := _Left;
    J := _Right;
//    P := (_Left + _Right) shr 1;
    // Chosing the pivot element can make a big difference:
    // In my unit tests its factor 100 for the TestPartSortedPartReverse test
    P := GetPivot(I, J, _CompareMeth);
    repeat
      while _CompareMeth(I, P) < 0 do
        Inc(I);
      while _CompareMeth(J, P) > 0 do
        Dec(J);
      if I <= J then begin
        if I < J then
          _SwapMeth(I, J);
        if P = I then
          P := J
        else if P = J then
          P := I;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if _Left < J then
      QuickSort(_Left, J, _CompareMeth, _SwapMeth);
    _Left := I;
  until I >= _Right;
end;

function BinarySearch(_Left, _Right: Integer; out _Index: Integer;
  const _Key; _CompareMeth: TCompareToItemMeth1;
  _Duplicates: Boolean = False): Boolean;
var
  p, c: LongInt;
begin
  Result := False;
  while _Left <= _Right do begin
    p := (_Left + _Right) shr 1;
    c := _CompareMeth(_Key, p);
    if c > 0 then
      _Left := p + 1
    else begin
      _Right := p - 1;
      if c = 0 then begin
        Result := True;
        if not _Duplicates then
          _Left := p;
      end;
    end;
  end;
  _Index := _Left;
end;

function BinarySearch(_Left, _Right: Integer; out _Index: Integer;
  _Key: pointer; _CompareMeth: TCompareToItemMeth2;
  _Duplicates: Boolean = False): Boolean;
var
  p, c: LongInt;
begin
  Result := False;
  while _Left <= _Right do begin
    p := (_Left + _Right) shr 1;
    c := _CompareMeth(_Key, p);
    if c > 0 then
      _Left := p + 1
    else begin
      _Right := p - 1;
      if c = 0 then begin
        Result := True;
        if not _Duplicates then
          _Left := p;
      end;
    end;
  end;
  _Index := _Left;
end;

function BinarySearch(_Left, _Right: Integer; out _Index: Integer;
  _CompareInt: ICompareToKey; _Duplicates: Boolean = False): Boolean;
var
  p, c: LongInt;
begin
  Result := False;
  while _Left <= _Right do begin
    p := (_Left + _Right) shr 1;
    c := _CompareInt.CompareTo(p);
    if c > 0 then
      _Left := p + 1
    else begin
      _Right := p - 1;
      if c = 0 then begin
        Result := True;
        if not _Duplicates then
          _Left := p;
      end;
    end;
  end;
  _Index := _Left;
end;

end.

