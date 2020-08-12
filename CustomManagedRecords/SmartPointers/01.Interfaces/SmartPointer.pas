unit SmartPointer;

interface

type
  ISmartPtr<T: class> = interface
    function GetRef: T;

    { For testing only }
    function GetRefCount: Integer;

    property Ref: T read GetRef;
  end;

type
  TSmartPtr<T: class> = class(TInterfacedObject, ISmartPtr<T>)
  private
    FRef: T;
  protected
    { ISmartPtr<T> }
    function GetRef: T;
    function GetRefCount: Integer;
  public
    constructor Create(const ARef: T);
    destructor Destroy; override;
  end;

implementation

{ TSmartPtr<T> }

constructor TSmartPtr<T>.Create(const ARef: T);
begin
  inherited Create;
  FRef := ARef;
end;

destructor TSmartPtr<T>.Destroy;
begin
  FRef.Free;
  inherited;
end;

function TSmartPtr<T>.GetRef: T;
begin
  Result := FRef;
end;

function TSmartPtr<T>.GetRefCount: Integer;
begin
  Result := FRefCount;
end;

end.
