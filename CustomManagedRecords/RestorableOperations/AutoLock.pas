unit AutoLock;

interface

uses
  System.SyncObjs;

type
  TAutoLock = record
  private
    FSyncObj: TSynchroObject; // Reference
  public
    constructor Create(const ASyncObj: TSynchroObject);
  public
    class operator Initialize(out ADest: TAutoLock);
    class operator Finalize(var ADest: TAutoLock);
    class operator Assign(var ADest: TAutoLock;
      const [ref] ASrc: TAutoLock);
  end;

implementation

uses
  System.Classes;

{ TAutoLock }

constructor TAutoLock.Create(const ASyncObj: TSynchroObject);
begin
  Assert(Assigned(ASyncObj));
  FSyncObj := ASyncObj;
  FSyncObj.Acquire;
end;

class operator TAutoLock.Initialize(out ADest: TAutoLock);
begin
  ADest.FSyncObj := nil;
end;

class operator TAutoLock.Finalize(var ADest: TAutoLock);
begin
  if (ADest.FSyncObj <> nil) then
    ADest.FSyncObj.Release;
end;

class operator TAutoLock.Assign(var ADest: TAutoLock;
  const [ref] ASrc: TAutoLock);
begin
  raise EInvalidOperation.Create(
    'TAutoLock records cannot be copied');
end;

end.
