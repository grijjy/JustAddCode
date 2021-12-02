unit AutoFree;

interface

type
  TAutoFree = record
  private
    FInstance: TObject; // Reference
  public
    constructor Create(const AInstance: TObject);
    class operator Initialize(out ADest: TAutoFree);
    class operator Finalize(var ADest: TAutoFree);
    class operator Assign(var ADest: TAutoFree;
      const [ref] ASrc: TAutoFree);
  end;

implementation

uses
  System.Classes;

{ TAutoFree }

constructor TAutoFree.Create(const AInstance: TObject);
begin
  Assert(Assigned(AInstance));
  FInstance := AInstance;
end;

class operator TAutoFree.Initialize(out ADest: TAutoFree);
begin
  ADest.FInstance := nil;
end;

class operator TAutoFree.Finalize(var ADest: TAutoFree);
begin
  ADest.FInstance.Free;
end;

class operator TAutoFree.Assign(var ADest: TAutoFree;
  const [ref] ASrc: TAutoFree);
begin
  raise EInvalidOperation.Create(
    'TAutoFree records cannot be copied')
end;

end.
