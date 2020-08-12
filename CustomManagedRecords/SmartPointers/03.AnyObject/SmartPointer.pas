unit SmartPointer;

interface

type
  TSmartPtr<T: class> = record
  private
    FRef: T;
    FRefCount: PInteger;
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
  if (ARef <> nil) then
  begin
    GetMem(FRefCount, SizeOf(Integer));
    FRefCount^ := 0;
  end;
  Retain;
end;

class operator TSmartPtr<T>.Initialize(out ADest: TSmartPtr<T>);
begin
  ADest.FRef := nil;
  ADest.FRefCount := nil;
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
    ADest.FRefCount := ASrc.FRefCount;
    ADest.Retain;
  end;
end;

procedure TSmartPtr<T>.Retain;
begin
  if (FRefCount <> nil) then
    AtomicIncrement(FRefCount^);
end;

procedure TSmartPtr<T>.Release;
begin
  if (FRefCount <> nil) then
  begin
    if (AtomicDecrement(FRefCount^) = 0) then
    begin
      FRef.Free;
      FreeMem(FRefCount);
    end;

    FRef := nil;
    FRefCount := nil;
  end;
end;

function TSmartPtr<T>.GetRefCount: Integer;
begin
  if (FRefCount = nil) then
    Result := 0
  else
    Result := FRefCount^;
end;

end.
