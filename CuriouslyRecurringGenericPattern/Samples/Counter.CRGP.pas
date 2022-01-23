unit Counter.CRGP;

interface

type
  TCounter<T> = class
  private class var
    FInstanceCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;

    class property InstanceCount: Integer read FInstanceCount;
  end;

type
  TFoo = class(TCounter<TFoo>)

  end;

type
  TBar = class(TCounter<TBar>)

  end;

procedure TestCounterCRGP;

implementation

uses
  Utils;

procedure TestCounterCRGP;
begin
  StartSection;
  WriteLn('A simple instance counter using CRGP:');

  { NOTE: We are creating memory leaks here. }
  TFoo.Create;
  TBar.Create;
  TFoo.Create;
  TFoo.Create;
  TBar.Create;

  WriteLn('* Number of TFoo instances: ', TFoo.InstanceCount);
  WriteLn('* Number of TBar instances: ', TBar.InstanceCount);
end;

{ TCounter<T> }

constructor TCounter<T>.Create;
begin
  inherited;
  Inc(FInstanceCount);
end;

destructor TCounter<T>.Destroy;
begin
  Dec(FInstanceCount);
  inherited;
end;

end.
