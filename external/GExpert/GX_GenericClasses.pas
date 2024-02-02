unit GX_GenericClasses;

{$I GX_CondDefine.inc}

interface

uses
  Classes;

type
  // We use the NoRefCount type to assign interface instances without invoking
  // reference-counting.  Only use this if you have a very good reason.
  NoRefCount = Pointer;

  TSingletonInterfacedObject = class(TInterfacedObject)
  protected
    // IUnknown overrides for private life-time management.
    // Enforce singleton semantics for interfaces.
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
  end;

{ TGxObjectDictionary
  List of objects that can be found by a specified code
  Code can be specified by overriding GetObjectCode or
  by using AddWithCode.
  Objects are owned by this list - it means that it frees
  object if the object is deleted.
}
  TGxListEntryHandler = procedure(Sender: TObject; AIndex: Integer; AData: TObject) of object;

  TGxObjectDictionary = class
  private
    FOwnsObjects: Boolean;
    function GetItem(Index: Integer): TObject;
    function GetCode(Index: Integer): string;
  protected
    FList: TStringList;
    function GetObjectCode(AObject: TObject): string; virtual;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear; virtual;
    procedure Delete(AIndex: Integer);
    procedure Remove(AObject: TObject);
    procedure Extract(Item: Pointer);

    function Add(AObject: TObject): Integer;
    function AddWithCode(const ACode: string; AObject: TObject): Integer;
    function IndexOf(const ACode: string): Integer;
    function FindObject(const ACode: string): TObject;
    function Count: Integer;
    function ObjectByCode(const ACode: string): TObject;
    procedure ForEach(AHandler: TGxListEntryHandler; AData: TObject = nil); virtual;

    property Items[Index: Integer]: TObject read GetItem; default;
    property Codes[Index: Integer]: string read GetCode;
    property OwnsObjects: Boolean read FOwnsObjects write FOwnsObjects;
  end;

implementation

uses
  SysUtils;
  
{ TGxObjectList }
{ TSingletonInterfacedObject }

function TSingletonInterfacedObject._AddRef: Integer;
begin
  Result := 1;
end;

function TSingletonInterfacedObject._Release: Integer;
begin
  Result := 1;
end;

{ TGxObjectDictionary }

constructor TGxObjectDictionary.Create;
begin
  inherited;
  FList := TStringList.Create;
  FList.Sorted := True;
  FList.Duplicates := dupError;
  FOwnsObjects := True;
end;

destructor TGxObjectDictionary.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited;
end;

procedure TGxObjectDictionary.Clear;
begin
  while Count>0 do
    Delete(Count-1);
end;

function TGxObjectDictionary.Add(AObject: TObject): Integer;
begin
  Result := AddWithCode(GetObjectCode(AObject), AObject);
end;

function TGxObjectDictionary.AddWithCode(const ACode: string; AObject: TObject): Integer;
begin
  Result := FList.AddObject(ACode, AObject);
end;

function TGxObjectDictionary.Count: Integer;
begin
  Result := FList.Count;
end;

procedure TGxObjectDictionary.Delete(AIndex: Integer);
var
  Obj: TObject;
begin
  Obj := FList.Objects[AIndex];
  FList.Delete(AIndex);
  if OwnsObjects then
    FreeAndNil(Obj);
end;

// remove but do not free
procedure TGxObjectDictionary.Extract(Item: Pointer);
var
  Idx: Integer;
begin
  Idx := FList.IndexOfObject(Item);
  if Idx >= 0 then
    FList.Delete(Idx);
end;

function TGxObjectDictionary.GetObjectCode(AObject: TObject): string;
resourcestring
  SCodeReadNotImp = 'Object code reader not implemented';
begin
  raise Exception.Create(SCodeReadNotImp);
end;

function TGxObjectDictionary.IndexOf(const ACode: string): Integer;
begin
  Result := FList.IndexOf(ACode);
end;

procedure TGxObjectDictionary.Remove(AObject: TObject);
var
  Idx: Integer;
begin
  Idx := FList.IndexOfObject(AObject);
  if Idx >= 0 then
    Delete(Idx);
end;

function TGxObjectDictionary.GetItem(Index: Integer): TObject;
begin
  Result := FList.Objects[Index];
end;

function TGxObjectDictionary.ObjectByCode(const ACode: string): TObject;
resourcestring
  SObjNF = 'Object with code "%s" not found';
var
  Idx: Integer;
begin
  Idx := IndexOf(ACode);
  if Idx < 0 then
    raise Exception.Create(Format(SObjNF, [ACode]));
  Result := FList.Objects[Idx];  
end;

function TGxObjectDictionary.FindObject(const ACode: string): TObject;
var
  Idx: Integer;
begin
  Idx := IndexOf(ACode);
  if Idx < 0 then
    Result := nil
  else
    Result := FList.Objects[Idx];
end;

procedure TGxObjectDictionary.ForEach(AHandler: TGxListEntryHandler; AData: TObject);
var
  i: Integer;
begin
  for i := 0 to Count-1 do
    AHandler(Self, i, AData);
end;

function TGxObjectDictionary.GetCode(Index: Integer): string;
begin
  Result := FList[Index];
end;

end.
