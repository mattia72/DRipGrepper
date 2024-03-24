unit RipGrepper.Common.SyncObjLock;

interface

uses
  System.SyncObjs;

type
  TLock<T> = class
  private
	FLock: TCriticalSection;
	FData: T;
  public
    constructor Create;
    destructor Destroy; override;
	procedure SetData(Value: T);
	function GetData: T;
  end;

implementation

constructor TLock<T>.Create;
begin
  FLock := TCriticalSection.Create;
end;

destructor TLock<T>.Destroy;
begin
  FLock.Free;
  inherited;
end;

procedure TLock<T>.SetData(Value: T);
begin
  FLock.Enter;
  try
    FData := Value;
  finally
    FLock.Leave;
  end;
end;

function TLock<T>.GetData: T;
begin
  FLock.Enter;
  try
    Result := FData;
  finally
    FLock.Leave;
  end;
end;



end.
