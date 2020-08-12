unit AutoUpdate;

interface

uses
  FMX.Controls;

type
  TAutoUpdate = record
  private
    FControl: TControl; // Reference
  public
    constructor Create(const AControl: TControl);
  public
    class operator Initialize(out ADest: TAutoUpdate);
    class operator Finalize(var ADest: TAutoUpdate);
    class operator Assign(var ADest: TAutoUpdate;
      const [ref] ASrc: TAutoUpdate);
  end;

implementation

uses
  System.Classes;

{ TAutoUpdate }

constructor TAutoUpdate.Create(const AControl: TControl);
begin
  Assert(Assigned(AControl));
  FControl := AControl;
  FControl.BeginUpdate;
end;

class operator TAutoUpdate.Initialize(out ADest: TAutoUpdate);
begin
  ADest.FControl := nil;
end;

class operator TAutoUpdate.Finalize(var ADest: TAutoUpdate);
begin
  if (ADest.FControl <> nil) then
    ADest.FControl.EndUpdate;
end;

class operator TAutoUpdate.Assign(var ADest: TAutoUpdate;
  const [ref] ASrc: TAutoUpdate);
begin
  raise EInvalidOperation.Create(
    'TAutoUpdate records cannot be copied')
end;

end.
