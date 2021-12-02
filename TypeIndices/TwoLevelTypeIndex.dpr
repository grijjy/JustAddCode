program TwoLevelTypeIndex;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils;

type
  TTypeIndex<TCategory> = class // static
  private type
    TIndex<T> = class // static
    private
      class var FValue: Integer;
    public
      class constructor Create;
    end;
  private
    class var FNextIndex: Integer;
  public
    class constructor Create;
  public
    class function Get<T>: Integer; inline; static;
  end;

{ TTypeIndex<TCategory> }

class constructor TTypeIndex<TCategory>.Create;
begin
  FNextIndex := 0;
end;

class function TTypeIndex<TCategory>.Get<T>: Integer;
begin
  Result := TIndex<T>.FValue;
end;

{ TTypeIndex<TCategory>.TIndex<T> }

class constructor TTypeIndex<TCategory>.TIndex<T>.Create;
begin
  FValue := FNextIndex;
  Inc(FNextIndex);
end;

{ TypeIndexExample }

type
  TCategory1 = type Integer;
  TCategory2 = type Integer;

procedure TypeIndexExample;
begin
  WriteLn('Type index for category 1, type Integer: ',
    TTypeIndex<TCategory1>.Get<Integer>);

  WriteLn('Type index for category 1, type Single: ',
    TTypeIndex<TCategory1>.Get<Single>);

  WriteLn('Type index for category 1, type String: ',
    TTypeIndex<TCategory1>.Get<String>);

  WriteLn('Type index for category 1, type Single: ',
    TTypeIndex<TCategory1>.Get<Single>);

  WriteLn('Type index for category 2, type Single: ',
    TTypeIndex<TCategory2>.Get<Single>);
end;

begin
  try
    TypeIndexExample;
    ReadLn;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
