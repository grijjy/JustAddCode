unit SmartPointer;

interface

type
  { Base class for objects that *can* be used with smart pointers. }
  TRefCountable = class abstract
  protected
    [volatile] FRefCount: Integer;
  end;

type
  TSmartPtr<T: TRefCountable> = record
  private
    FRef: T;
    procedure Retain; inline;
    procedure Release; inline;
  public
    constructor Create(const ARef: T);
    class operator Initialize(out ADest: TSmartPtr<T>);
    class operator Finalize(var ADest: TSmartPtr<T>);
    class operator Assign(var ADest: TSmartPtr<T>;
      const [ref] ASrc: TSmartPtr<T>);

    { For testing only }
    function GetRefCount: Integer;

    property Ref: T read FRef;
  end;

implementation

{ TSmartPtr<T> }

constructor TSmartPtr<T>.Create(const ARef: T);
begin
  Assert((ARef = nil) or (ARef.FRefCount = 0));
  FRef := ARef;
  Retain;
end;

class operator TSmartPtr<T>.Initialize(out ADest: TSmartPtr<T>);
begin
  ADest.FRef := nil;
end;

class operator TSmartPtr<T>.Finalize(var ADest: TSmartPtr<T>);
begin
  ADest.Release;
end;

class operator TSmartPtr<T>.Assign(var ADest: TSmartPtr<T>;
  const [ref] ASrc: TSmartPtr<T>);
begin
  if (ADest.FRef <> ASrc.FRef) then
  begin
    ADest.Release;
    ADest.FRef := ASrc.FRef;
    ADest.Retain;
  end;
end;

procedure TSmartPtr<T>.Retain;
begin
  if (FRef <> nil) then
    AtomicIncrement(FRef.FRefCount);
end;

procedure TSmartPtr<T>.Release;
begin
  if (FRef <> nil) then
  begin
    if (AtomicDecrement(FRef.FRefCount) = 0) then
      FRef.Free;

    FRef := nil;
  end;
end;

function TSmartPtr<T>.GetRefCount: Integer;
begin
  if (FRef = nil) then
    Result := 0
  else
    Result := FRef.FRefCount;
end;

end.
