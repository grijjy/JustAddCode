program SimpleTypeIndex;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  System.Classes;

type
  { Utility to generate type indices (partiallly) at compile time }
  TTypeIndex<T> = class // static
  private
    class var FValue: Integer;
  public
    class constructor Create;

    { Returns the type index for the type parameter T }
    class property Value: Integer read FValue;
  end;

var
  GNextTypeIndex: Integer = 0;

{ TTypeIndex<T> }

class constructor TTypeIndex<T>.Create;
begin
  FValue := GNextTypeIndex;
  Inc(GNextTypeIndex);
end;

type
  TIntegerAlias = Integer;
  TDistinctInteger = type Integer;

{ TypeIndexExample }

procedure TypeIndexExample;
begin
  WriteLn('Index for type Integer: ', TTypeIndex<Integer>.Value);
  WriteLn('Index for type String: ', TTypeIndex<String>.Value);
  WriteLn('Index for type TStream: ', TTypeIndex<TStream>.Value);

  { To check if the type index for String is still the same as before: }
  WriteLn('Index for type String: ', TTypeIndex<String>.Value);

  { Type aliases have the same index as their aliased type: }
  WriteLn('Index for type TIntegerAlias: ', TTypeIndex<TIntegerAlias>.Value);

  { Discinct types will have their own unique index though: }
  WriteLn('Index for type TDistinctInteger: ', TTypeIndex<TDistinctInteger>.Value);
end;

type
  TTypeMap = record
  private
    FBits: UInt32;
  public
    procedure Init; inline;
    procedure Include<T>; inline;
    procedure Exclude<T>; inline;
    function Has<T>: Boolean; inline;
  end;

{ TTypeMap }

procedure TTypeMap.Init;
begin
  FBits := 0;
end;

procedure TTypeMap.Include<T>;
var
  Index: Integer;
begin
  Index := TTypeIndex<T>.Value;
  Assert(Index < 32);
  FBits := FBits or (1 shl Index);
end;

procedure TTypeMap.Exclude<T>;
var
  Index: Integer;
begin
  Index := TTypeIndex<T>.Value;
  Assert(Index < 32);
  FBits := FBits and not (1 shl Index);
end;

function TTypeMap.Has<T>: Boolean;
var
  Index: Integer;
begin
  Index := TTypeIndex<T>.Value;
  Assert(Index < 32);
  Result := ((FBits and (1 shl Index)) <> 0);
end;

{ TypeMapExample }

procedure TypeMapExample;
var
  TypeMap: TTypeMap;
begin
  TypeMap.Init;

  { "Register" Integer and TStream types }
  TypeMap.Include<Integer>;
  TypeMap.Include<TStream>;

  Assert(TypeMap.Has<Integer>);
  Assert(TypeMap.Has<TStream>);
  Assert(not TypeMap.Has<String>);

  { "Unregister" Integer type }
  TypeMap.Exclude<Integer>;

  Assert(not TypeMap.Has<Integer>);
  Assert(TypeMap.Has<TStream>);
end;

{ Entry Point }

begin
  try
    TypeIndexExample;
    TypeMapExample;
    ReadLn;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
