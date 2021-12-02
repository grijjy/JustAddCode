unit SmartPointer;

interface

type
  TSmartPtr<T: class> = record
  private
    FRef: T;
    function GetRefCountPtr: PInteger; inline;
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
  FRef := ARef;
  Assert((FRef = nil) or (GetRefCountPtr^ = 0));
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

function TSmartPtr<T>.GetRefCountPtr: PInteger;
begin
  if (FRef = nil) then
    Result := nil
  else
    Result := PInteger(IntPtr(FRef) + FRef.InstanceSize - hfFieldSize + hfMonitorOffset);
end;

procedure TSmartPtr<T>.Retain;
begin
  var RefCountPtr := GetRefCountPtr;
  if (RefCountPtr <> nil) then
    AtomicIncrement(RefCountPtr^);
end;

procedure TSmartPtr<T>.Release;
begin
  var RefCountPtr := GetRefCountPtr;
  if (RefCountPtr <> nil) then
  begin
    if (AtomicDecrement(RefCountPtr^) = 0) then
    begin
      { Before destroying the object, we must make sure that the hidden monitor
        is set to nil or 0. Otherwise, the destructor will try to free the
        monitor, which isn't actually a monitor but a reference count. This
        would most likely result in an access violation.
        Fortunately, this line of code only gets executed when reference count
        has dropped to 0, which has the same effect as setting the monitor to
        nil. }
      FRef.Free;
    end;

    FRef := nil;
  end;
end;

function TSmartPtr<T>.GetRefCount: Integer;
begin
  var RefCountPtr := GetRefCountPtr;
  if (RefCountPtr = nil) then
    Result := 0
  else
    Result := RefCountPtr^;
end;

end.
