unit USimpleStackList;

interface

type
  { A simple list that should live on the stack and can hold at most 256 bytes
    of data. This list can only hold value types (including records with fields
    that are only of value types). }
  TStackList<T: record> = record
  private type
    P = ^T;
  private
    FData: array [0..255] of Byte;
    FCapacity: Integer;
    FCount: Integer;
    function GetItem(const AIndex: Integer): T;
  public
    { Initializes the list.
      You *must* call this method before using this list. It acts like a
      constructor. }
    procedure Initialize;

    { Clears the list }
    procedure Clear;

    { Adds an item to the list.
      Raises EInvalidOperation if the list is full and the item cannot be added. }
    procedure Add(const AItem: T);

    { The number of items in the list. }
    property Count: Integer read FCount;

    { The items in the list.
      Raises EArgumentOutOfRangeException if AIndex is invalid. }
    property Items[const AIndex: Integer]: T read GetItem; default;
  end;

implementation

uses
  System.Classes,
  System.SysUtils;

{ TStackList<T> }

procedure TStackList<T>.Add(const AItem: T);
var
  Target: P;
begin
  if (FCount >= FCapacity) then
    raise EInvalidOperation.Create('List is full');

  Target := @FData[FCount * SizeOf(T)];
  Target^ := AItem;
  Inc(FCount);
end;

procedure TStackList<T>.Clear;
begin
  FCount := 0;
end;

function TStackList<T>.GetItem(const AIndex: Integer): T;
var
  Item: P;
begin
  if (AIndex < 0) or (AIndex >= FCount) then
    raise EArgumentOutOfRangeException.Create('List index out of range');

  Item := @FData[AIndex * SizeOf(T)];
  Result := Item^;
end;

procedure TStackList<T>.Initialize;
begin
  if IsManagedType(T) then
    raise EInvalidOperation.Create('A stack based collection cannot contain managed types');

  FCapacity := SizeOf(FData) div SizeOf(T);
  FCount := 0;
end;

end.
