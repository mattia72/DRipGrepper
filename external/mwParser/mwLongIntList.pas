unit mwLongIntList;

{$RANGECHECKS OFF}

interface

type
  PLongintArray = ^TLongintArray;
  TLongintArray = array[0..0] of Longint;

  TLongintList = class(TObject)
  private
    FCapacity: Integer;
    FCount: Integer;
    FLongintList: PLongintArray;
  protected
    function GetItems(Index: Integer): Longint;
    procedure SetCapacity(NewCapacity: Integer);
    procedure SetCount(NewCount: Integer);
    procedure SetItems(Index: Integer; Item: Longint);
  public
    destructor Destroy; override;
    function Add(Item: Longint): Integer;
    procedure Clear;
    procedure Delete(Index: Integer);
    procedure Exchange(Index1, Index2: Integer);
    function First: Longint;
    function IndexOf(Item: Longint): Integer;
    procedure Insert(Index: Integer; Item: Longint);
    function Last: Longint;
    procedure Move(CurIndex, NewIndex: Integer);
    function Remove(Item: Longint): Integer;
    procedure Sort;
    procedure DeleteGroup(StartIndex: Longint; GroupCount: Longint);
    property Capacity: Integer read FCapacity write SetCapacity;
    property Count: Integer read FCount write SetCount;
    property Items[Index: Integer]: Longint read GetItems write SetItems; default;
    property LongintList: PLongintArray read FLongintList;
  end; { TLongintList }

implementation

destructor TLongintList.Destroy;
begin
  Clear;
  inherited Destroy;
end; { Destroy }

{ Based on a non-recursive QuickSort from the SWAG-Archive.
  ( TV Sorting Unit by Brad Williams ) }

procedure TLongintList.Sort;
var
  Left, Right, SubArray, SubLeft, SubRight, Temp, Pivot: Longint;
  Stack: array[1..32] of record First, Last: Longint; end;
begin
  if Count > 1 then
  begin
    SubArray := 1;
    Stack[SubArray].First := 0;
    Stack[SubArray].Last := Count - 1;
    repeat
      Left := Stack[SubArray].First;
      Right := Stack[SubArray].Last;
      Dec(SubArray);
      repeat
        SubLeft := Left;
        SubRight := Right;
        Pivot := FLongintList[(Left + Right) shr 1];
        repeat
          while FLongintList[SubLeft] < Pivot do Inc(SubLeft);
          while FLongintList[SubRight] > Pivot do Dec(SubRight);
          if SubLeft <= SubRight then
          begin
            Temp := FLongintList[SubLeft];
            FLongintList[SubLeft] := FLongintList[SubRight];
            FLongintList[SubRight] := Temp;
            Inc(SubLeft);
            Dec(SubRight);
          end;
        until SubLeft > SubRight;
        if SubLeft < Right then
        begin
          Inc(SubArray);
          Stack[SubArray].First := SubLeft;
          Stack[SubArray].Last := Right;
        end;
        Right := SubRight;
      until Left >= Right;
    until SubArray = 0;
  end;
end; { Sort }

function TLongintList.GetItems(Index: Integer): Longint;
begin
  Result := FLongintList[Index];
end; { GetItems }

procedure TLongintList.SetCapacity(NewCapacity: Integer);
begin
  if NewCapacity < FCount then
    FCount := NewCapacity;
  if NewCapacity <> FCapacity then
  begin
    ReallocMem(FLongintList, NewCapacity * SizeOf(Longint));
    FCapacity := NewCapacity;
  end;
end; { SetCapacity }

procedure TLongintList.SetCount(NewCount: Integer);
begin
  if NewCount > FCapacity then
    SetCapacity(NewCount);
  FCount := NewCount;
end; { SetCount }

procedure TLongintList.SetItems(Index: Integer; Item: Longint);
begin
  FLongintList[Index] := Item;
end; { SetItems }

function TLongintList.Add(Item: Longint): Integer;
begin
  Result := FCount;
  if Result + 1 >= FCapacity then
    SetCapacity(FCapacity + 1024);
  FLongintList[Result] := Item;
  Inc(FCount);
end; { Add }

procedure TLongintList.Clear;
begin
  SetCount(0);
  SetCapacity(0);
end; { Clear }

procedure TLongintList.Delete(Index: Integer);
begin
  Dec(FCount);
  if Index < FCount then
    System.Move(FLongintList[Index + 1], FLongintList[Index],
      (FCount - Index) * SizeOf(Longint));
end; { Delete }

procedure TLongintList.DeleteGroup(StartIndex: Longint; GroupCount: Longint);
begin
  Dec(FCount, GroupCount);
  if StartIndex < FCount then
    System.Move(FLongintList[StartIndex + GroupCount], FLongintList[StartIndex],
      (FCount - StartIndex) * SizeOf(Longint));
end;  { DeleteGroup }

procedure TLongintList.Exchange(Index1, Index2: Integer);
var
  Item: Longint;
begin
  Item := FLongintList[Index1];
  FLongintList[Index1] := FLongintList[Index2];
  FLongintList[Index2] := Item;
end; { Exchange }

function TLongintList.First: Longint;
begin
  Result := GetItems(0);
end; { First }

function TLongintList.IndexOf(Item: Longint): Integer;
begin
  Result := 0;
  while (Result < FCount) and (FLongintList[Result] <> Item) do Inc(Result);
  if Result = FCount then
    Result := -1;
end; { IndexOf }

procedure TLongintList.Insert(Index: Integer; Item: Longint);
begin
  if FCount = FCapacity then
    SetCapacity(FCapacity + 1024);
  if Index < FCount then
    System.Move(FLongintList[Index], FLongintList[Index + 1],
      (FCount - Index) * SizeOf(Longint));
  FLongintList[Index] := Item;
  Inc(FCount);
end; { Insert }

function TLongintList.Last: Longint;
begin
  Result := GetItems(FCount - 1);
end; { Last }

procedure TLongintList.Move(CurIndex, NewIndex: Integer);
var
  Item: Longint;
begin
  if CurIndex <> NewIndex then
  begin
    Item := GetItems(CurIndex);
    Delete(CurIndex);
    Insert(NewIndex, Item);
  end;
end; { Move }

function TLongintList.Remove(Item: Longint): Integer;
begin
  Result := IndexOf(Item);
  if Result <> -1 then
    Delete(Result);
end; { Remove }

end.
