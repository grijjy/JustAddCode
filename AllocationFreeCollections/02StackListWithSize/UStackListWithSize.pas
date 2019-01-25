unit UStackListWithSize;

interface

type
  { A list that should live on the stack. It can hold a configurable amount of
    data. The number of bytes of data it can contain is equal to the size of the
    TSize type parameter. The TSize type parameter has no other purpose. You can
    use one of the predefined T*Bytes types (where * is the number of bytes), or
    define your own size type.
    This list can only hold value types (including records with fields that are
    only of value types). }
  TStackList<T: record; TSize: record> = record
  private type
    P = ^T;
  private
    FData: TSize;
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

type
  { Some predefined types that can be used as the TSize type parameter for the
    TStackList<T, TSize> type. }
  T128Bytes = record Data: array [0..127] of Byte end;
  T256Bytes = record Data: array [0..255] of Byte end;
  T512Bytes = record Data: array [0..511] of Byte end;
  T1024Bytes = record Data: array [0..1023] of Byte end;

implementation

uses
  System.Classes,
  System.SysUtils;

{ TStackList<T, TSize> }

procedure TStackList<T, TSize>.Add(const AItem: T);
var
  Target: P;
begin
  if (FCount >= FCapacity) then
    raise EInvalidOperation.Create('List is full');

  Target := P(IntPtr(@FData) + (FCount * SizeOf(T)));
  Target^ := AItem;
  Inc(FCount);
end;

procedure TStackList<T, TSize>.Clear;
begin
  FCount := 0;
end;

function TStackList<T, TSize>.GetItem(const AIndex: Integer): T;
var
  Item: P;
begin
  if (AIndex < 0) or (AIndex >= FCount) then
    raise EArgumentOutOfRangeException.Create('List index out of range');

  Item := P(IntPtr(@FData) + (AIndex * SizeOf(T)));
  Result := Item^;
end;

procedure TStackList<T, TSize>.Initialize;
begin
  if IsManagedType(T) or IsManagedType(TSize) then
    raise EInvalidOperation.Create('A stack based collection cannot contain managed types');

  FCapacity := SizeOf(FData) div SizeOf(T);
  FCount := 0;
end;

end.

