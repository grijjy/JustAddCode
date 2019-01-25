unit UStackListWithManagedTypes;

interface

type
  { A list that should live on the stack. It can hold a configurable amount of
    data. The number of bytes of data it can contain is equal to the size of the
    TSize type parameter. The TSize type parameter has no other purpose. You can
    use one of the predefined T*Bytes types (where * is the number of bytes), or
    define your own size type. }
  TStackList<T; TSize: record> = record
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
      constructor.
      You also *must* call Finalize when you are done with the list. }
    procedure Initialize;

    { Finalizes the list.
      You *must* call this method when you are done with the list. It acts like
      a destructor and releases any managed items the list may contain. }
    procedure Finalize;

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
  { If T is a managed type, then we need to finalize all items in the list. This
    will decrease reference counts and free memory where needed. }
  if IsManagedType(T) then
  begin
    FinalizeArray(@FData, TypeInfo(T), FCount);
    FillChar(FData, FCount * SizeOf(T), 0);
  end;
  FCount := 0;
end;

procedure TStackList<T, TSize>.Finalize;
begin
  Clear;
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
  if IsManagedType(TSize) then
    raise EInvalidOperation.Create('A stack based collection cannot use a managed type for its TSize parameter');

  FCapacity := SizeOf(FData) div SizeOf(T);
  FCount := 0;

  { The FData field may contain random data, which causes access violations
    when T is a managed type. So we need to clear it.
    Note that IsManagedType is a compiler-magic function. This means that the
    if-condition is performed at compile time, and the FillChar statement is not
    compiled at all if T is not a managed type. }
  if IsManagedType(T) then
    FillChar(FData, SizeOf(FData), 0);
end;

end.

