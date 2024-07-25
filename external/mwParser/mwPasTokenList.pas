{+--------------------------------------------------------------------------+
 | Unit:        mwPasTokenList
 | Created:     10.97
 | Author:      Martin Waldenburg
 | Copyright    1997, all rights reserved.
 | Description: TLongIntList is a dynamic array of LongInts.
 |              TmSearcher is a specialized version of the turbo search engine,
 |              which is based on an article in the German magazine c't (8/97).
 |              TPasTokenList scans a PChar for Pascal tokens and gives full access.
 | Version:     1.2
 | DISCLAIMER:  This is provided as is, expressly without a warranty of any kind.
 |              You use it at your own risc.
 +--------------------------------------------------------------------------+}

unit mwPasTokenList;

{$RANGECHECKS OFF}

interface

uses
  mwPasParserTypes,
  mwLongIntList;

type
  TPasTokenList = class;

  TmSearcher = class(TObject)
  private
    FPasTokenList: TPasTokenList;
    FSearchOrigin: PChar;
    Pat: string;
    fPos: Integer;
    HalfLen: Integer;
    PatLenPlus: Integer;
    SearchLen: Integer;
    Shift: array[0..255] of Integer;
    CompTable: array[#0..#255] of Byte;
    fFinished: Boolean;
    fFound: Boolean;
    fPosition: Integer;
    FFoundList: TLongintList;
    function GetFinished: Boolean;
    function GetItems(Index: Integer): Integer;
    function GetCount: Integer;
  protected
  public
    ClassList: TLongintList;
    ImplementationsList: TLongintList;
    InterfaceList: TLongintList;
    MethodList: TLongintList;
    PatLen: Integer;
    constructor Create(Value: TPasTokenList);
    destructor Destroy; override;
    function GetImplementationsIndex: Longint;
    function Next: Integer;
    procedure Add(aPosition: Integer);
    procedure FillClassList;
    procedure Init(const NewPattern: string);
    procedure Retrive(aToken: string);
    function GetMethodImplementation(const aClassName: string; aMethodIdentifier: string): Longint;
    procedure FillMethodList;
    procedure FillInterfaceList;
    function GetMethodImpLine(const aClassName, aMethodIdentifier: string): Longint;
    property Finished: Boolean read GetFinished;
    property Found: Boolean read fFound;
    property Position: Integer read fPosition write fPos;
    property Items[Index: Integer]: Integer read GetItems; default;
    property Count: Integer read GetCount;
  end; { TmSearcher }

  TPasTokenList = class(TObject)
  private
    FTokenPositionsList: TLongintList;
    fOrigin: PChar;
    fPCharSize: Longint;
    fPCharCapacity: Longint;
    FComment: TCommentState;
    FEndCount: Integer;
    Run: Longint;
    Walker: Longint;
    FRoundCount: Integer;
    FSquareCount: Integer;
    FVisibility: TTokenKind;
    FLines: TLongintList;
    procedure WriteTo(InsPos, DelPos: Longint; const Item: string);
    function GetCount: Integer;
    procedure SetCount(Value: Integer);
    function GetCapacity: Integer;
    procedure ResetPositionsFrom(Index, Value: Longint);
    procedure ResetLines(Index, Value: Longint);
    function GetIsJunk: Boolean;
    function IdentKind(Index: Longint): TTokenKind;
    procedure SetRunIndex(NewPos: Longint);
    procedure HandleComments;
    function GetTokenID(Index: Longint): TTokenKind;
    function GetTokenPosition(Index: Integer): Longint;
    function GetRunID: TTokenKind;
    function GetRunPosition: Longint;
    function GetRunToken: string;
    function GetTokenLine(anIndex: Integer): Longint;
    function GetRunLine: Longint;
  protected
    function GetToken(Index: Integer): string;
    procedure SetCapacity(NewCapacity: Integer);
    procedure SetToken(Index: Integer; const Item: string);
  public
    Searcher: TmSearcher;
    constructor Create;
    destructor Destroy; override;
    procedure SetOrigin(NewOrigin: PChar; NewSize: Longint);
    procedure Clear;
    procedure Delete(Index: Integer);
    procedure Exchange(Index1, Index2: Integer);
    function First: string;
    function IndexOf(const Item: string): Integer;
    procedure Insert(Index: Integer; const Item: string);
    function Last: string;
    procedure Move(CurIndex, NewIndex: Integer);
    function Remove(const Item: string): Integer;
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Count: Integer read GetCount write SetCount;
    property Token[Index: Integer]: string read GetToken write SetToken; default;
    property TokenPositionsList: TLongintList read FTokenPositionsList;
    property Origin: PChar read fOrigin;
    property PCharSize: Longint read fPCharSize;
    property PCharCapacity: Longint read fPCharCapacity;
    function GetSubString(StartPos, EndPos: Longint): string;
    procedure ScanForLines;
    procedure Next;
    procedure Previous;
    procedure NextID(ID: TTokenKind);
    procedure NextNonComment;
    procedure NextNonJunk;
    procedure NextNonSpace;
    procedure Tokenize;
    procedure ToLineStart;
    procedure PreviousID(ID: TTokenKind);
    procedure PreviousNonComment;
    procedure PreviousNonJunk;
    procedure PreviousNonSpace;
    function PositionAtLine(aPosition: Longint): Longint;
    function IndexAtLine(anIndex: Longint): Longint;
    function PositionToIndex(aPosition: Longint): Longint;
    procedure DeleteGroup(StartIndex: Longint; GroupCount: Longint);
    function InsertString(StartIndex: Longint; const ToInsert: String): Longint;
    function MoveGroup(OldStartIndex: Longint; NewStartIndex: Longint; GroupCount: Longint): Boolean;
    property Comments: TCommentState read FComment write FComment;
    property EndCount: Integer read FEndCount write FEndCount;
    property IsJunk: Boolean read GetIsJunk;
    property RunIndex: Longint read Run write SetRunIndex;
    property RoundCount: Integer read FRoundCount write FRoundCount;
    property SquareCount: Integer read FSquareCount write FSquareCount;
    property Visibility: TTokenKind read FVisibility write FVisibility;
    property TokenID[Index: Longint]: TTokenKind read GetTokenID;
    property TokenPosition[Index: Longint]: Longint read GetTokenPosition;
    property RunID: TTokenKind read GetRunID;
    property RunPosition: Longint read GetRunPosition;
    property RunToken: string read GetRunToken;
    property Lines: TLongintList read FLines;
    property TokenLine[Index: Integer]: Longint read GetTokenLine;
    property RunLine: Longint read GetRunLine;
  end; { TPasTokenList }

implementation

uses
  SysUtils, GX_GenericUtils;

constructor TmSearcher.Create(Value: TPasTokenList);
begin
  inherited Create;
  FPasTokenList := Value;
  Pat := '';
  PatLen := 0;
  HalfLen := 0;
  SearchLen := 0;
  fPos := -1;
  fFound := False;
  FFoundList := TLongintList.Create;
  ClassList := TLongintList.Create;
  ImplementationsList := TLongintList.Create;
  InterfaceList := TLongintList.Create;
  MethodList := TLongintList.Create;
end; { Create }

destructor TmSearcher.Destroy;
begin
  FFoundList.Free;
  ClassList.Free;
  ImplementationsList.Free;
  InterfaceList.Free;
  MethodList.Free;
  inherited Destroy;
end; { Destroy }

function TmSearcher.GetFinished: Boolean;
begin
  fFinished := False;
  if fPos >= SearchLen - 1 then fFinished := True;
  if PatLen > SearchLen then fFinished := True;
  Result := fFinished;
end; { GetFinished }

procedure TmSearcher.Init(const NewPattern: string);
var
  I: Byte;
begin
  FFoundList.Clear;
  SearchLen := FPasTokenList.PCharSize;
  FSearchOrigin := FPasTokenList.Origin;
  Pat := NewPattern;
  PatLen := Length(Pat);
  PatLenPlus := PatLen + 1;
  HalfLen := PatLen div 2;
  for I := 0 to 255 do
    CompTable[Char(I)] := ord(AnsiLowerCase(Char(I))[1]);
  for I := 0 to 255 do Shift[I] := PatLenPlus;
  for I := 1 to PatLen do Shift[CompTable[Pat[i]]] := PatLenPlus - I;
  fPos := -1;
end; { Init }

function TmSearcher.Next: Integer;
var
  I, J: Integer;
begin
  Result := -1;
  fFound := False;
  Inc(fPos, PatLen);
  fPosition := -1;
  while fPos <= SearchLen do
  begin
    I := PatLen;
    if (CompTable[Pat[I]] <> CompTable[FSearchOrigin[fPos]]) then
      Inc(fPos, Shift[CompTable[FSearchOrigin[fPos + 1]]])
    else
    begin
      J := fPos;
      repeat
        Dec(I); Dec(J);
      until (I = 0) or (CompTable[Pat[I]] <> CompTable[FSearchOrigin[J]]);
      if I = 0 then
      begin
        fFound := True;
        fPosition := fPos - Patlen + 1;
        Result := fPosition;
        Break;
      end else if I < HalfLen then Inc(fPos, PatLenPlus)
      else Inc(fPos, Shift[CompTable[FSearchOrigin[J + 1]]]);
    end;
  end;
end; { Next }

function TmSearcher.GetItems(Index: Integer): Integer;
begin
  if (Index >= FFoundList.Count) or (Index < 0) then
    Result := -1
  else
    Result := FFoundList[Index];
end; { GetItems }

function TmSearcher.GetCount: Integer;
begin
  Result := FFoundList.Count;
end; { GetCount }

procedure TmSearcher.Add(aPosition: Integer);
begin
  FFoundList.Add(aPosition);
end; { Add }

procedure TmSearcher.FillClassList;
var
  RPos: Longint;
  RIndex: Longint;
begin
  ClassList.Clear;
  Init('CLASS');
  while not Finished do
  begin
    RPos := Next;
    if RPos <> -1 then
    begin
      RIndex := FPasTokenList.PositionToIndex(RPos);
      if (RPos = FPasTokenList.FTokenPositionsList[RIndex]) then
      begin
        FPasTokenList.RunIndex := RIndex;
        FPasTokenList.PreviousNonJunk;
        if FPasTokenList.RunId = tkEqual then
        begin
          if 'CLASS' = UpperCase(FPasTokenList[RIndex]) then
          begin
            RIndex := FPasTokenList.RunIndex - 1;
            while not (FPasTokenList.TokenID[RIndex] in BigIdentDirect) do begin
              if FPasTokenList.TokenID[RIndex] = tkGreater then begin
                // It's a Generic, find the matching '<'
                while (RIndex > 1) and (FPasTokenList.TokenID[RIndex] <> tkLower) do begin
                  Dec(RIndex);
                end;
              end;
              Dec(RIndex);
            end;
            ClassList.Add(RIndex);
          end;
        end;
      end;
    end;
  end;
end; { FillClassList }

procedure TmSearcher.FillInterfaceList;
var
  RPos: Longint;
  RIndex: Longint;
begin
  InterfaceList.Clear;
  Init('INTERFACE');
  while not Finished do
  begin
    RPos := Next;
    if RPos <> -1 then
    begin
      RIndex := FPasTokenList.PositionToIndex(RPos);
      if (RPos = FPasTokenList.FTokenPositionsList[RIndex]) then
      begin
        FPasTokenList.RunIndex := RIndex;
        FPasTokenList.PreviousNonJunk;
        if FPasTokenList.RunId = tkEqual then
        begin
          if 'INTERFACE' = UpperCase(FPasTokenList[RIndex]) then
          begin
            RIndex := FPasTokenList.RunIndex - 1;
            while not (FPasTokenList.TokenID[RIndex] in BigIdentDirect) do begin
              if FPasTokenList.TokenID[RIndex] = tkGreater then begin
                // It's a Generic, find the matching '<'
                while (RIndex > 1) and (FPasTokenList.TokenID[RIndex] <> tkLower) do begin
                  Dec(RIndex);
                end;
              end;
              Dec(RIndex);
            end;
            InterfaceList.Add(RIndex);
          end;
        end;
      end;
    end;
  end;
  Init('DISPINTERFACE');
  while not Finished do
  begin
    RPos := Next;
    if RPos <> -1 then
    begin
      RIndex := FPasTokenList.PositionToIndex(RPos);
      if (RPos = FPasTokenList.FTokenPositionsList[RIndex]) then
      begin
        FPasTokenList.RunIndex := RIndex;
        FPasTokenList.PreviousNonJunk;
        if FPasTokenList.RunId = tkEqual then
        begin
          if 'DISPINTERFACE' = UpperCase(FPasTokenList[RIndex]) then
          begin
            RIndex := FPasTokenList.RunIndex - 1;
            while not (FPasTokenList.TokenID[RIndex] in BigIdentDirect) do
              Dec(RIndex);
            InterfaceList.Add(RIndex);
          end;
        end;
      end;
    end;
  end;
  InterfaceList.Sort;
end; { FillInterfaceList }

procedure TmSearcher.FillMethodList;
var
  RPos: Longint;
  RIndex: Longint;
begin
  MethodList.Clear;
  Init('CONSTRUCTOR');
  while not Finished do
  begin
    RPos := Next;
    if RPos <> -1 then
    begin
      RIndex := FPasTokenList.PositionToIndex(RPos);
      if (RPos = FPasTokenList.FTokenPositionsList[RIndex]) then
        if 'CONSTRUCTOR' = UpperCase(FPasTokenList[RIndex]) then
        begin
          while FPasTokenList.TokenID[RIndex - 1] <> tkCRLF do
            Dec(RIndex);
          MethodList.Add(RIndex);
        end;
    end;
  end;
  Init('DESTRUCTOR');
  while not Finished do
  begin
    RPos := Next;
    if RPos <> -1 then
    begin
      RIndex := FPasTokenList.PositionToIndex(RPos);
      if (RPos = FPasTokenList.FTokenPositionsList[RIndex]) then
        if 'DESTRUCTOR' = UpperCase(FPasTokenList[RIndex]) then
        begin
          while FPasTokenList.TokenID[RIndex - 1] <> tkCRLF do
            Dec(RIndex);
          MethodList.Add(RIndex);
        end;
    end;
  end;
  Init('FUNCTION');
  while not Finished do
  begin
    RPos := Next;
    if RPos <> -1 then
    begin
      RIndex := FPasTokenList.PositionToIndex(RPos);
      if (RPos = FPasTokenList.FTokenPositionsList[RIndex]) then
        if 'FUNCTION' = UpperCase(FPasTokenList[RIndex]) then
        begin
          while FPasTokenList.TokenID[RIndex - 1] <> tkCRLF do
            Dec(RIndex);
          MethodList.Add(RIndex);
        end;
    end;
  end;
  Init('PROCEDURE');
  while not Finished do
  begin
    RPos := Next;
    if RPos <> -1 then
    begin
      RIndex := FPasTokenList.PositionToIndex(RPos);
      if (RPos = FPasTokenList.FTokenPositionsList[RIndex]) then
        if 'PROCEDURE' = UpperCase(FPasTokenList[RIndex]) then
        begin
          while FPasTokenList.TokenID[RIndex - 1] <> tkCRLF do
            Dec(RIndex);
          MethodList.Add(RIndex);
        end;
    end;
  end;
  MethodList.Sort;
end; { FillMethodList }

procedure TmSearcher.Retrive(aToken: string);
var
  RPos: Longint;
  RIndex: Longint;
begin
  aToken := UpperCase(aToken);
  Init(aToken);
  while not Finished do
  begin
    RPos := Next;
    if RPos <> -1 then
    begin
      RIndex := FPasTokenList.PositionToIndex(RPos);
      if (RPos = FPasTokenList.FTokenPositionsList[RIndex]) then
        if aToken = UpperCase(FPasTokenList[RIndex]) then
          Add(RIndex);
    end;
  end;
end; { Retrive }

function TmSearcher.GetImplementationsIndex: Longint;
var
  RPos: Longint;
  RIndex: Longint;
begin
  Result := -1;
  Init('IMPLEMENTATION');
  while not Finished do
  begin
    RPos := Next;
    if RPos <> -1 then
    begin
      RIndex := FPasTokenList.PositionToIndex(RPos);
      if (RPos = FPasTokenList.FTokenPositionsList[RIndex]) then
        if 'IMPLEMENTATION' = UpperCase(FPasTokenList[RIndex]) then
        begin
          Result := RIndex;
          Break;
        end;
    end;
  end;
end; { GetImplementationsIndex }

function TmSearcher.GetMethodImplementation(const aClassName: string; aMethodIdentifier: string): Longint;
var
  RPos: Longint;
  RIndex: Longint;
  ToFind: string;
  Found: Boolean;
begin
  Result := -1;
  ImplementationsList.Clear;
  ToFind := aClassName + '.' + aMethodIdentifier;
  if ToFind <> '.' then
  begin
    Init(aClassName + '.' + aMethodIdentifier);
    aMethodIdentifier := UpperCase(aMethodIdentifier);
    while not Finished do
    begin
      RPos := Next;
      Found := False;
      if RPos <> -1 then
      begin
        RIndex := FPasTokenList.PositionToIndex(RPos);
        if (RPos = FPasTokenList.FTokenPositionsList[RIndex]) then
          if aMethodIdentifier = UpperCase(FPasTokenList[RIndex + 2]) then
          begin
            while FPasTokenList.TokenID[RIndex - 1] <> tkCRLF do
            begin
              Dec(RIndex);
              if FPasTokenList.TokenID[RIndex] in [tkConstructor, tkDestructor, tkFunction, tkProcedure] then
                Found := True;
            end;
            if Found then
            begin
              Result := RIndex;
              ImplementationsList.Add(RIndex);
              Break;
            end;
          end;
      end;
    end;
    while not Finished do
    begin
      Found := False;
      RPos := Next;
      if RPos <> -1 then
      begin
        RIndex := FPasTokenList.PositionToIndex(RPos);
        if (RPos = FPasTokenList.FTokenPositionsList[RIndex]) then
          if aMethodIdentifier = UpperCase(FPasTokenList[RIndex + 2]) then
          begin
            while FPasTokenList.TokenID[RIndex - 1] <> tkCRLF do
            begin
              Dec(RIndex);
              if FPasTokenList.TokenID[RIndex] in [tkConstructor, tkDestructor, tkFunction, tkProcedure] then
                Found := True;
            end;
            if Found then ImplementationsList.Add(RIndex);
          end;
      end;
    end;
  end;
end; { GetMethodImplementation }

function TmSearcher.GetMethodImpLine(const aClassName, aMethodIdentifier: string): Longint;
var
  ImpIndex: Longint;
begin
  ImpIndex := GetMethodImplementation(aClassName, aMethodIdentifier);
  Result := FPasTokenList.IndexAtLine(ImpIndex);
end; { GetMethodImpLine }

constructor TPasTokenList.Create;
begin
  inherited Create;
  FTokenPositionsList := TLongintList.Create;
  FTokenPositionsList.Add(0);
  FComment := csNo;
  FEndCount := 0;
  Visibility := tkUnknown;
  Searcher := TmSearcher.Create(Self);
  FLines := TLongintList.Create;
end; { Create }

destructor TPasTokenList.Destroy;
begin
  FTokenPositionsList.Free;
  Searcher.Free;
  FLines.Free;
  inherited Destroy;
end; { Destroy }

procedure TPasTokenList.SetOrigin(NewOrigin: PChar; NewSize: Longint);
begin
  FOrigin := NewOrigin;
  Run := 0;
  fPCharSize := NewSize;
  fPCharCapacity := fPCharSize;
  Tokenize;
  Searcher.FillClassList;
  FRoundCount := 0;
  FSquareCount := 0;
end; { SetOrigin }

procedure TPasTokenList.WriteTo(InsPos, DelPos: Longint;
  const Item: string);
var
  StringCount, NewSize: Longint;
  aString: String;
begin
  aString := Item + (FOrigin + DelPos);
  StringCount := Length(aString);
  if (InsPos >= 0) and (StringCount >= 0) then
  begin
    NewSize := InsPos + StringCount;
    if NewSize > 0 then
    begin
      if NewSize >= FPCharCapacity then
      begin
        try
          FPCharCapacity := FPCharCapacity + StringCount +1024;
          ReAllocMem(FOrigin, PCharCapacity);
        except
          raise exception.Create('unable to reallocate PChar');
        end;
      end;
      StrECopy((FOrigin + InsPos), PChar(aString));
      FPCharSize := NewSize;
      FOrigin[FPCharSize] := #0;
      aString := '';
    end;
  end;
end; { WriteTo }

function TPasTokenList.GetCount: Integer;
begin
  Result := FTokenPositionsList.Count - 1;
end; { GetCount }

procedure TPasTokenList.SetCount(Value: Integer);
begin
  FTokenPositionsList.Count := Value + 1;
end; { SetCount }

function TPasTokenList.GetCapacity: Integer;
begin
  Result := FTokenPositionsList.Capacity;
end; { GetCapacity }

procedure TPasTokenList.ResetPositionsFrom(Index, Value: Longint);
begin
  while Index < FTokenPositionsList.Count do
  begin
    FTokenPositionsList[Index] := FTokenPositionsList[Index] + Value;
    Inc(Index);
  end
end; { ResetPositionsFrom }

procedure TPasTokenList.ResetLines(Index, Value: Longint);
begin
  Index:= IndexAtLine(Index) +1;
  while Index < FLines.Count do
  begin
    FLines[Index] := FLines[Index] + Value;
    Inc(Index);
  end
end; { ResetPositionsFrom }

procedure TPasTokenList.ScanForLines;
begin
end; { ScanForLines }

function TPasTokenList.GetToken(Index: Integer): string;
var
  StartPos, EndPos, StringLen: Longint;
begin
  StartPos := FTokenPositionsList[Index];
  EndPos := FTokenPositionsList[Index + 1];
  StringLen := EndPos - StartPos;
  SetString(Result, (FOrigin + StartPos), StringLen);
end; { GetToken }

function TPasTokenList.GetTokenPosition(Index: Integer): Longint;
begin
  Result := FTokenPositionsList[Index];
end; { GetTokenPosition }

procedure TPasTokenList.SetCapacity(NewCapacity: Integer);
begin
  FTokenPositionsList.Capacity := NewCapacity;
end; { SetCapacity }

procedure TPasTokenList.SetToken(Index: Integer; const Item: string);
var
  StartPos, EndPos, OldLen, NewLen, Diff: Longint;
begin
  StartPos := FTokenPositionsList[Index];
  EndPos := FTokenPositionsList[Index + 1];
  OldLen := EndPos - StartPos;
  NewLen := Length(Item);
  Diff := NewLen - OldLen;
  ResetLines(Index, Diff);
  WriteTo(StartPos, EndPos, Item);
  ResetPositionsFrom(Index + 1, Diff);
end; { SetItems }

procedure TPasTokenList.Clear;
begin
  SetCount(0);
  FTokenPositionsList.Capacity := 1;
  Run := 0;
end; { Clear }

procedure TPasTokenList.Delete(Index: Integer);
var
  StartPos, EndPos, OldLen: Longint;
begin
  StartPos := FTokenPositionsList[Index];
  EndPos := FTokenPositionsList[Index + 1];
  OldLen := EndPos - StartPos;
  ResetLines(Index -1, -OldLen);
  WriteTo(StartPos, EndPos, '');
  FTokenPositionsList.Delete(Index);
  ResetPositionsFrom(Index, -OldLen);
end; { Delete }

procedure TPasTokenList.DeleteGroup(StartIndex: Longint; GroupCount: Longint);
var
  StartPos, EndPos, OldLen: Longint;
begin
  StartPos := FTokenPositionsList[StartIndex];
  EndPos := FTokenPositionsList[StartIndex + GroupCount];
  OldLen := EndPos - StartPos;
  ResetLines(StartIndex -1, -OldLen);
  WriteTo(StartPos, EndPos, '');
  FTokenPositionsList.DeleteGroup(StartIndex, GroupCount);
  ResetPositionsFrom(StartIndex, -OldLen);
end;  { DeleteGroup }

procedure TPasTokenList.Exchange(Index1, Index2: Integer);
var
  Item: string;
begin
  Item := GetToken(Index1);
  SetToken(Index1, GetToken(Index2));
  SetToken(Index2, Item);
end; { Exchange }

function TPasTokenList.First: string;
begin
  Result := GetToken(0);
end; { First }

function TPasTokenList.IndexOf(const Item: string): Integer;
begin
  Result := 0;
  while (Result < Count) and (GetToken(Result) <> Item) do Inc(Result);
  if Result = Count then Result := -1;
end; { IndexOf }

procedure TPasTokenList.Insert(Index: Integer; const Item: string);
var
  StartPos, EndPos, ItemLen: Longint;
begin
  ItemLen := Length(Item);
  StartPos := FTokenPositionsList[Index];
  EndPos := StartPos + ItemLen;
  ResetLines(Index, ItemLen);
  WriteTo(StartPos, StartPos, Item);
  ResetPositionsFrom(Index + 1, ItemLen);
  FTokenPositionsList.Insert(Index + 1, EndPos);
end; { Insert }

function TPasTokenList.InsertString(StartIndex: Longint; const ToInsert: String): Longint;
var
  I, StartPos, EndPos, ItemLen: Longint;
  TempHelper: TPasTokenList;
begin
  TempHelper := TPasTokenList.Create;
  ItemLen := Length(ToInsert);
  TempHelper.SetOrigin(PChar(ToInsert), ItemLen);
  Result := TempHelper.Count;
  StartPos := FTokenPositionsList[StartIndex];
  ResetLines(StartIndex, ItemLen);
  WriteTo(StartPos, StartPos, ToInsert);
  ResetPositionsFrom(StartIndex + 1, ItemLen);
  for I := 0 to TempHelper.Count -1 do
  begin
    StartPos := FTokenPositionsList[StartIndex + I];
    EndPos := StartPos + Length(TempHelper[I]);
    FTokenPositionsList.Insert(StartIndex + I + 1, EndPos);
  end;
  TempHelper.Free;
end;   { InsertString }

function TPasTokenList.Last: string;
begin
  Result := GetToken(Count - 1);
end; { Last }

procedure TPasTokenList.Move(CurIndex, NewIndex: Integer);
var
  Item: string;
begin
  if CurIndex <> NewIndex then
  begin
    Item := GetToken(CurIndex);
    Delete(CurIndex);
    Insert(NewIndex, Item);
  end;
end; { Move }

function TPasTokenList.MoveGroup(OldStartIndex: Longint; NewStartIndex: Longint; GroupCount: Longint): Boolean;
var
  TempString: string;
  TempStringLen, StartPos, EndPos: Longint;
begin
  Result:= False;
  if NewStartIndex < OldStartIndex then
  begin
    Result:= True;
    StartPos := FTokenPositionsList[OldStartIndex];
    EndPos := FTokenPositionsList[OldStartIndex + GroupCount];
    TempStringLen := EndPos - StartPos;
    SetString(TempString, (FOrigin + StartPos), TempStringLen);
    InsertString(NewStartIndex, TempString);
    OldStartIndex := OldStartIndex + GroupCount;
    DeleteGroup(OldStartIndex, GroupCount);
    TempString := '';
  end;

  if (NewStartIndex > OldStartIndex + GroupCount)then
  begin
    Result:= True;
    StartPos := FTokenPositionsList[OldStartIndex];
    EndPos := FTokenPositionsList[OldStartIndex + GroupCount];
    TempStringLen := EndPos - StartPos;
    SetString(TempString, (FOrigin + StartPos), TempStringLen);
    InsertString(NewStartIndex, TempString);
    DeleteGroup(OldStartIndex, GroupCount);
    TempString := '';
  end;
end;  { MoveGroup }

function TPasTokenList.Remove(const Item: string): Integer;
begin
  Result := IndexOf(Item);
  if Result <> -1 then Delete(Result);
end; { Remove }

function TPasTokenList.GetSubString(StartPos, EndPos: Longint): string;
var
  SubLen: Integer;
begin
  if FOrigin[EndPos] = #10 then Inc(EndPos);
  SubLen := EndPos - StartPos;
  SetString(Result, (FOrigin + StartPos), SubLen);
end; { GetSubString }

procedure TPasTokenList.SetRunIndex(NewPos: Longint);
begin
  Run := NewPos;
end; { SetRunPos }

procedure TPasTokenList.HandleComments;
begin
  case FComment of
    csAnsi:
      begin
        while FOrigin[Walker] <> #0 do
        begin
          case FOrigin[Walker] of
            '*': if FOrigin[Walker + 1] = ')' then
              begin
                Inc(Walker, 2);
                FComment := csNo;
                Break;
              end;
            #10:
              begin
                Inc(Walker);
                fLines.Add(Walker);
              end;

            #13:
              begin
                 // Fix by Stefan (4/18/98)
                if FOrigin[Walker + 1] = #10 then Inc(Walker);
                fLines.Add(Walker);
                //if FOrigin[Walker + 1] = #10 then Inc(Walker, 2) else Inc(Walker);
                //fLines.Add(Walker);
              end;
          end;
          Inc(Walker);
        end;
      end;

    csBor:
      begin
        while FOrigin[Walker] <> #0 do
        begin
          case FOrigin[Walker] of
            '}':
              begin
                Inc(Walker);
                FComment := csNo;
                Break;
              end;
            #10:
              begin
                Inc(Walker);
                fLines.Add(Walker);
              end;

            #13:
              begin
                // Fix by Stefan (4/18/98)
                if FOrigin[Walker + 1] = #10 then Inc(Walker);
                fLines.Add(Walker);
                //if FOrigin[Walker + 1] = #10 then Inc(Walker, 2) else Inc(Walker);
                //fLines.Add(Walker);
              end;
          end;
          Inc(Walker);
        end;
      end;

    csSlashes:
      begin
        while FOrigin[Walker] <> #0 do
        begin
          Inc(Walker);
          case FOrigin[Walker] of
            #0, #10, #13:
              begin
                FComment := csNo;
                Break;
              end;
          end;
        end;
      end;
  end;
  FTokenPositionsList.Add(Walker);
end; { HandleComments }

function TPasTokenList.IdentKind(Index: Longint): TTokenKind;
var
  HashKey: Integer;
  aToken: string;
  StartPos, EndPos, StringLen: Longint;

  function KeyHash: Integer;
  var
    I: Integer;
  begin
    Result := 0;
    for i := 1 to StringLen do
      Result := Result + Ord(aToken[i]);
  end; { KeyHash }
begin
  Result := tkIdentifier;
  StartPos := FTokenPositionsList[Index];
  EndPos := FTokenPositionsList[Index + 1];
  StringLen := EndPos - StartPos;
  SetString(aToken, (FOrigin + StartPos), StringLen);
  aToken := UpperCase(aToken);
  HashKey := KeyHash;
  case HashKey of
    143: if aToken = 'IF' then Result := tkIf;
    147: if aToken = 'DO' then Result := tkDo;
    148: if aToken = 'AS' then Result := tkAs;
    149: if aToken = 'OF' then Result := tkOf;
    151: if aToken = 'IN' then Result := tkIn;
    156: if aToken = 'IS' then Result := tkIs;
    161: if aToken = 'OR' then Result := tkOr;
    163: if aToken = 'TO' then Result := tkTo;
    211: if aToken = 'AND' then Result := tkAnd;
    215: if aToken = 'END' then Result := tkEnd;
    217: if aToken = 'FAR' then Result := tkFar;
    224: if aToken = 'MOD' then Result := tkMod;
    225: if aToken = 'ASM' then Result := tkAsm;
    227:
      begin
        if aToken = 'DIV' then Result := tkDiv else
          if aToken = 'NIL' then Result := tkNil;
      end;
    231:
      begin
        if aToken = 'FOR' then Result := tkFor else
          if aToken = 'SHL' then Result := tkShl;
      end;
    233: if aToken = 'VAR' then Result := tkVar;
    236: if aToken = 'SET' then Result := tkSet;
    237: if aToken = 'SHR' then Result := tkShr;
    241: if aToken = 'NOT' then Result := tkNot;
    248: if aToken = 'OUT' then Result := tkOut;
    249: if aToken = 'XOR' then Result := tkXor;
    255: if aToken = 'TRY' then Result := tkTry;
    284:
      begin
        if aToken = 'CASE' then Result := tkCase else
          if aToken = 'READ' then Result := tkRead;
      end;
    288: if aToken = 'FILE' then Result := tkFile;
    289: if aToken = 'NAME' then Result := tkName;
    294: if aToken = 'NEAR' then Result := tkNear;
    297: if aToken = 'ELSE' then Result := tkElse;
    303: if aToken = 'THEN' then Result := tkThen;
    313: if aToken = 'GOTO' then Result := tkGoto;
    316: if aToken = 'WITH' then Result := tkWith;
    320:
      begin
        if aToken = 'UNIT' then Result := tkUnit else
          if aToken = 'USES' then Result := tkUses;
      end;
    322: if aToken = 'TYPE' then Result := tkType;
    347: if aToken = 'CDECL' then Result := tkCdecl;
    352: if aToken = 'LABEL' then Result := tkLabel;
    357: if aToken = 'BEGIN' then Result := tkBegin;
    372: if aToken = 'RAISE' then Result := tkRaise;
    374: if aToken = 'CLASS' then Result := tkClass;
    376: if aToken = 'INDEX' then Result := tkIndex;
    377: if aToken = 'WHILE' then Result := tkWhile;
    383: if aToken = 'ARRAY' then Result := tkArray;
    391: if aToken = 'CONST' then Result := tkConst;
    395: if aToken = 'WRITE' then Result := tkWrite;
    396: if aToken = 'UNTIL' then Result := tkUntil;
    424: if aToken = 'PACKED' then Result := tkPacked;
    436: if aToken = 'PASCAL' then Result := tkPascal;
    439: if aToken = 'OBJECT' then Result := tkObject;
    445: if aToken = 'DISPID' then Result := tkDispid;
    447:
      begin
        if aToken = 'INLINE' then Result := tkInline else
          if aToken = 'PUBLIC' then Result := tkPublic else
            if aToken = 'RECORD' then Result := tkRecord;
      end;
    449: if aToken = 'REPEAT' then Result := tkRepeat;
    457: if aToken = 'EXCEPT' then Result := tkExcept;
    465: if aToken = 'STORED' then Result := tkStored;
    471: if aToken = 'STRING' then Result := tkKeyString;
    475: if aToken = 'DOWNTO' then Result := tkDownto;
    482: if aToken = 'EXPORT' then Result := tkExport;
    517:
      begin
        if aToken = 'DEFAULT' then Result := tkDefault else
          if aToken = 'DYNAMIC' then Result := tkDynamic else
            if aToken = 'MESSAGE' then Result := tkMessage;
      end;
    519: if aToken = 'STDCALL' then Result := tkStdcall;
    527: if aToken = 'FINALLY' then Result := tkFinally;
    533:
      begin
        if aToken = 'FORWARD' then Result := tkForward else
          if aToken = 'LIBRARY' then Result := tkLibrary;
      end;
    536: if aToken = 'PROGRAM' then Result := tkProgram;
    539: if aToken = 'PRIVATE' then Result := tkPrivate;
    551: if aToken = 'VIRTUAL' then Result := tkVirtual;
    565: if aToken = 'EXPORTS' then Result := tkExports;
    571: if aToken = 'SAFECALL' then Result := tkSafecall;
    596: if aToken = 'ABSTRACT' then Result := tkAbstract;
    606:
      begin
        if aToken = 'READONLY' then Result := tkReadonly else
          if aToken = 'RESIDENT' then Result := tkResident;
      end;
    607: if aToken = 'ABSOLUTE' then Result := tkAbsolute;
    608: if aToken = 'OVERRIDE' then Result := tkOverride;
    611: if aToken = 'EXTERNAL' then Result := tkExternal;
    613: if aToken = 'REGISTER' then Result := tkRegister;
    614: if aToken = 'FUNCTION' then Result := tkFunction;
    645: if aToken = 'PROPERTY' then Result := tkProperty;
    657: if aToken = 'INTERFACE' then Result := tkInterface;
    668: if aToken = 'INHERITED' then Result := tkInherited;
    670: if aToken = 'ASSEMBLER' then Result := tkAssembler;
    672: if aToken = 'PUBLISHED' then Result := tkPublished;
    673: if aToken = 'THREADVAR' then Result := tkThreadvar;
    674: if aToken = 'NODEFAULT' then Result := tkNodefault;
    676: if aToken = 'AUTOMATED' then Result := tkAutomated;
    681: if aToken = 'PROCEDURE' then Result := tkProcedure;
    682: if aToken = 'PROTECTED' then Result := tkProtected;
    717: if aToken = 'WRITEONLY' then Result := tkWriteonly;
    783: if aToken = 'DESTRUCTOR' then Result := tkDestructor;
    870: if aToken = 'CONSTRUCTOR' then Result := tkConstructor;
    904: if aToken = 'FINALIZATION' then Result := tkFinalization;
    961: if aToken = 'DISPINTERFACE' then Result := tkDispinterface;
    1062: if aToken = 'IMPLEMENTATION' then Result := tkImplementation;
    1064: if aToken = 'INITIALIZATION' then Result := tkInitialization;
    1087:
      begin
        if aToken = 'RESOURCESTRING' then Result := tkResourcestring else
          if aToken = 'STRINGRESOURCE' then Result := tkStringresource;
      end;
  end;
  case Result of
    tkCase: Inc(FEndCount);
    tkClass: FEndCount := 1;
    tkBegin: Inc(FEndCount);
    tkEnd: Dec(FEndCount);
    tkRecord: Inc(FEndCount);
    tkObject: Inc(FEndCount);
  end;
end; { IdentKind }

procedure TPasTokenList.Tokenize;
begin
  Walker := 0;
  FLines.Clear;
  fLines.Add(0);
  Clear;
  while FOrigin[Walker] <> #0 do
  begin
    case FOrigin[Walker] of
      #10:
        begin
          Inc(Walker);
          FTokenPositionsList.Add(Walker);
          fLines.Add(Walker);
        end;

      #13:
        begin
          if FOrigin[Walker + 1] = #10 then Inc(Walker, 2) else Inc(Walker);
          FTokenPositionsList.Add(Walker);
          fLines.Add(Walker);
        end;

      #1..#9, #11, #12, #14..#32:
        begin
          Inc(Walker);
          while CharInSet(FOrigin[Walker], [#1..#9, #11, #12, #14..#32]) do Inc(Walker);
          FTokenPositionsList.Add(Walker);
        end;

      '/':
        case FOrigin[Walker + 1] of
          '/':
            begin
              FComment := csSlashes;
              HandleComments;
            end;
        else
          begin
            Inc(Walker);
            FTokenPositionsList.Add(Walker);
          end;
        end;

      'A'..'Z', 'a'..'z', '_':
        begin
          Inc(Walker);
          while CharInSet(FOrigin[Walker], ['A'..'Z', 'a'..'z', '0'..'9', '_']) do Inc(Walker);
          FTokenPositionsList.Add(Walker);
        end;

      '0'..'9':
        begin
          Inc(Walker);
          while CharInSet(FOrigin[Walker], ['0'..'9', '.', 'e', 'E']) do
          begin
            case FOrigin[Walker] of
              '.':
                if FOrigin[Walker + 1] = '.' then Break;
            end;
            Inc(Walker);
          end;
          FTokenPositionsList.Add(Walker);
        end;

      '{':
        begin
          FComment := csBor;
          HandleComments;
        end;

      '!', '"', '%', '&', '('..'.', ':'..'@', '['..'^', '`', '~':
        begin
          case FOrigin[Walker] of
            '(':
              case FOrigin[Walker + 1] of
                '*':
                  begin
                    FComment := csAnsi;
                    HandleComments;
                  end;
                '.': Inc(Walker);
              end;

            '.':
              case FOrigin[Walker + 1] of
                '.': Inc(Walker);
                ')': Inc(Walker);
              end;

            ':':
              case FOrigin[Walker + 1] of
                '=': Inc(Walker);
              end;

            '<':
              case FOrigin[Walker + 1] of
                '=': Inc(Walker);

                '>': Inc(Walker);
              end;

            '>':
              case FOrigin[Walker + 1] of
                '=': Inc(Walker);
              end;
          end;
          Inc(Walker);
          FTokenPositionsList.Add(Walker);
        end;

      #39:
        begin
          Inc(Walker);
          while True do begin
            if (FOrigin[Walker] = #39) then
            begin
              if (FOrigin[Walker + 1] = #39) then
                Inc(Walker)
              else begin
                Inc(Walker);
                Break;
              end;
            end
            else if IsCharLineEndingOrNull(FOrigin[Walker]) then
              Break;
            Inc(Walker);
          end;
          FTokenPositionsList.Add(Walker);
        end;

      '#':
        begin
          Inc(Walker);
          while CharInSet(FOrigin[Walker], ['0'..'9']) do Inc(Walker);
          FTokenPositionsList.Add(Walker);
        end;

      '$':
        begin
          Inc(Walker);
          while CharInSet(FOrigin[Walker], ['0'..'9', 'A'..'F', 'a'..'f']) do Inc(Walker);
          FTokenPositionsList.Add(Walker);
        end;

    else
      begin
        Inc(Walker);
        FTokenPositionsList.Add(Walker);
      end;
    end;
  end;
end; {Tokenize}

function TPasTokenList.GetTokenID(Index: Longint): TTokenKind;
var
  Running: Longint;
begin
  Result := tkUnknown;
  Running := FTokenPositionsList[Index];
  case FOrigin[Running] of
    #0: Result := tkNull;

    #10: Result := tkCRLF;

    #13: Result := tkCRLF;

    #1..#9, #11, #12, #14..#32: Result := tkSpace;

    'A'..'Z', 'a'..'z', '_': Result := IdentKind(Index);

    '0'..'9':
      begin
        Inc(Running);
        Result := tkNumber;
        while CharInSet(FOrigin[Running], ['0'..'9', '.']) do
        begin
          case FOrigin[Running] of
            '.':
              if FOrigin[Running + 1] <> '.' then Result := tkFloat else Break;
          end;
          Inc(Running);
        end;
      end;

    '{':
      begin
        Result := tkBorComment;
        if FOrigin[Running + 1] = '$' then Result := tkCompDirect;
      end;

    '!', '"', '%', '&', '('..'/', ':'..'@', '['..'^', '`', '~':
      begin
        case FOrigin[Running] of
          '(':
            case FOrigin[Running + 1] of
              '*':
                begin
                  Result := tkAnsiComment;
                  if FOrigin[Running + 2] = '$' then Result := tkCompDirect;
                end;
              '.':
                begin
                  Result := tkSquareOpen;
                  Inc(FSquareCount);
                end;
            else
              begin
                Result := tkRoundOpen;
                Inc(FRoundCount);
              end;
            end;

          ')':
            begin
              Result := tkRoundClose;
              Dec(FRoundCount);
            end;
          '*': Result := tkStar;
          '+': Result := tkPlus;
          ',': Result := tkComma;
          '-': Result := tkMinus;

          '.':
            case FOrigin[Running + 1] of
              '.': Result := tkDotDot;
              ')':
                begin
                  Result := tkSquareClose;
                  Dec(FSquareCount);
                end;
            else Result := tkPoint;
            end;

          '/':
            case FOrigin[Running + 1] of
              '/': Result := tkSlashesComment;
            else Result := tkSlash;
            end;

          ':':
            case FOrigin[Running + 1] of
              '=': Result := tkAssign;
            else Result := tkColon;
            end;

          ';': Result := tkSemiColon;

          '<':
            case FOrigin[Running + 1] of
              '=': Result := tkLowerEqual;
              '>': Result := tkNotEqual;
            else Result := tkLower;
            end;

          '=': Result := tkEqual;

          '>':
            case FOrigin[Running + 1] of
              '=': Result := tkGreaterEqual;
            else Result := tkGreater;
            end;

          '[':
            begin
              Result := tkSquareOpen;
              Inc(FSquareCount);
            end;
          ']':
            begin
              Result := tkSquareClose;
              Dec(FSquareCount);
            end;

        else Result := tkSymbol;
        end;
      end;

    #39: Result := tkString;

    '#': Result := tkAsciiChar;

    '$': Result := tkInteger;

  end;
end; { GetTokenID }

procedure TPasTokenList.Next;
begin
  if Run < Count then Inc(Run);
end; { Next }

procedure TPasTokenList.Previous;
begin
  if Run > 0 then Dec(Run);
end; { Previous }

procedure TPasTokenList.NextID(ID: TTokenKind);
begin
  repeat
    case TokenID[Run] of
      tkNull: Break;
    else Inc(Run);
    end;
  until TokenID[Run] = ID;
end; { NextID }

function TPasTokenList.GetIsJunk: Boolean;
begin
  case TokenID[Run] of
    tkAnsiComment, tkBorComment, tkCRLF, tkSlashesComment, tkSpace:
      Result := True;
  else Result := False;
  end;
end;

procedure TPasTokenList.NextNonComment;
begin
  repeat
    case TokenID[Run] of
      tkNull: Break;
    else Inc(Run);
    end;
  until not (TokenID[Run] in [tkAnsiComment, tkBorComment, tkSlashesComment]);
end; { NextNonComCRLF }

procedure TPasTokenList.NextNonJunk;
begin
  repeat
    case TokenID[Run] of
      tkNull: Break;
    else Inc(Run);
    end;
  until not (TokenID[Run] in [tkAnsiComment, tkBorComment, tkCRLF,
    tkSlashesComment, tkSpace]);
end; { NextNonJunk }

procedure TPasTokenList.NextNonSpace;
begin
  repeat
    case TokenID[Run] of
      tkNull: Break;
    else Inc(Run);
    end;
  until not (TokenID[Run] = tkSpace);
end; { NextNonSpace }

procedure TPasTokenList.ToLineStart;
begin
  while TokenID[Run] <> tkCRLF do
  begin
    if Run <= 0 then Break;
    Dec(Run);
  end;
  Inc(Run);
end; { ToLineStart }

procedure TPasTokenList.PreviousID(ID: TTokenKind);
begin
  repeat
    case Run of
      0: Break;
    else Dec(Run);
    end;
  until TokenID[Run] = ID;
end; { PreviousID }

procedure TPasTokenList.PreviousNonComment;
begin
  repeat
    case Run of
      0: Break;
    else Dec(Run);
    end;
  until not (TokenID[Run] in [tkAnsiComment, tkBorComment, tkSlashesComment]);
end; { PreviousNonComment }


procedure TPasTokenList.PreviousNonJunk;
begin
  repeat
    case Run of
      0: Break;
    else Dec(Run);
    end;
  until not (TokenID[Run] in [tkAnsiComment, tkBorComment, tkCRLF,
    tkSlashesComment, tkSpace]);
end; { PreviousNonJunk }

procedure TPasTokenList.PreviousNonSpace;
begin
  repeat
    case Run of
      0: Break;
    else Dec(Run);
    end;
  until not (TokenID[Run] = tkSpace);
end; { PreviousNonSpace }

function TPasTokenList.PositionAtLine(aPosition: Longint): Longint;
var
  First, Last, I: Longint;
begin
  Result := -1;
  I := 0;
  if (aPosition >= 0) and (aPosition <= fPCharSize) then
  begin
    First := 0;
    Last := FLines.Count - 2;
    while First <= Last do
    begin
      I := (First + Last) shr 1;
      if aPosition < FLines[I] then Last := I - 1 else
      begin
        if aPosition < FLines[I + 1] then Break;
        First := I + 1;
      end;
    end;
    Result := I;
  end;
end;

function TPasTokenList.IndexAtLine(anIndex: Longint): Longint;
begin
  Result := PositionAtLine(TokenPosition[anIndex]);
end;

function TPasTokenList.GetRunID: TTokenKind;
begin
  Result := GetTokenID(Run);
end; { GetRunID }

function TPasTokenList.GetRunPosition: Longint;
begin
  Result := FTokenPositionsList[Run];
end; { GetRunPosition }

function TPasTokenList.GetRunToken: string;
var
  StartPos, EndPos, StringLen: Longint;
begin
  StartPos := FTokenPositionsList[Run];
  EndPos := FTokenPositionsList[Run + 1];
  StringLen := EndPos - StartPos;
  SetString(Result, (FOrigin + StartPos), StringLen);
end; { GetRunToken }

function TPasTokenList.PositionToIndex(aPosition: Longint): Longint;
var
  First, Last, I: Longint;
begin
  Result := -1;
  I := 0;
  if (aPosition >= 0) and (aPosition <= fPCharSize) then
  begin
    First := 0;
    Last := FTokenPositionsList.Count - 2;
    while First <= Last do
    begin
      I := (First + Last) shr 1;
      if aPosition < FTokenPositionsList[I] then Last := I - 1 else
      begin
        if aPosition < FTokenPositionsList[I + 1] then Break;
        First := I + 1;
      end;
    end;
    Result := I;
  end;
end; { PositionToIndex }

function TPasTokenList.GetTokenLine(anIndex: Longint): Longint;
begin
  Result:= IndexAtLine(anIndex);
end;  { GetTokenLine }

function TPasTokenList.GetRunLine: Longint;
begin
  Result:= IndexAtLine(Run);
end;  { GetRunLine }

end.

