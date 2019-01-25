unit UGrowableStackList;

interface

type
  { A list that should live on the stack. It can hold a configurable amount of
    data on the stack. The number of bytes of data it can contain is equal to
    the size of the TSize type parameter. The TSize type parameter has no other
    purpose.

    Once the stack capacity has been reached, the list switches to the heap for
    any additional items. }
  TStackList<T: record; TSize: record> = record
  private type
    P = ^T;
  private
    FStackData: TSize;
    FStackCapacity: Integer;
    FHeapData: Pointer;
    FHeapCapacity: Integer;
    FCount: Integer;
    function GetItem(const AIndex: Integer): T;
    function GetHeapCount: Integer;
  private
    procedure Grow;
  public
    { Initializes the list.
      You *must* call this method before using this list. It acts like a
      constructor.
      You also *must* call Finalize when you are done with the list. }
    procedure Initialize;

    { Finalizes the list.
      You *must* call this method when you are done with the list. It acts like
      a destructor and frees any heap memory that may have been allocated. }
    procedure Finalize;

    { Clears the list }
    procedure Clear;

    { Adds an item to the list.
      Raises EInvalidOperation if the list is full and the item cannot be added. }
    procedure Add(const AItem: T);

    { The number of items in the list. }
    property Count: Integer read FCount;

    { The number of items that are stored in heap memory.
      FOR TESTING ONLY. }
    property HeapCount: Integer read GetHeapCount;

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
  System.Math,
  System.Classes,
  System.SysUtils;

{ TStackList<T, TSize> }

procedure TStackList<T, TSize>.Add(const AItem: T);
var
  Target: P;
  Index: Integer;
begin
  if (FCount < FStackCapacity) then
  begin
    { We can still add this item to the memory stack. }
    Target := @FStackData;
    Inc(Target, FCount);
  end
  else
  begin
    { We need to add this item to heap memory.
      First calculate the index into heap memory. }
    Index := FCount - FStackCapacity;

    { Grow heap memory if needed to accommodate Index }
    if (Index >= FHeapCapacity) then
      Grow;

    Target := FHeapData;
    Inc(Target, Index);
  end;

  Target^ := AItem;
  Inc(FCount);
end;

procedure TStackList<T, TSize>.Clear;
begin
  FCount := 0;
end;

procedure TStackList<T, TSize>.Finalize;
begin
  FreeMem(FHeapData);
  FHeapData := nil;
  FHeapCapacity := 0;
  FCount := 0;
end;

function TStackList<T, TSize>.GetHeapCount: Integer;
begin
  Result := Max(0, FCount - FStackCapacity);
end;

function TStackList<T, TSize>.GetItem(const AIndex: Integer): T;
var
  Item: P;
begin
  if (AIndex < 0) or (AIndex >= FCount) then
    raise EArgumentOutOfRangeException.Create('List index out of range');

  if (AIndex < FStackCapacity) then
  begin
    Item := @FStackData;
    Inc(Item, AIndex);
  end
  else
  begin
    Item := FHeapData;
    Inc(Item, AIndex - FStackCapacity);
  end;

  Result := Item^;
end;

{$IF (RTLVersion < 33)}
procedure TStackList<T, TSize>.Grow;
begin
  { Pre-Rio growth strategy: double collection size }
  if (FHeapCapacity = 0) then
    FHeapCapacity := 4
  else
    FHeapCapacity := FHeapCapacity * 2;
  ReallocMem(FHeapData, FHeapCapacity * SizeOf(T));
end;
{$ELSE}
procedure TStackList<T, TSize>.Grow;
begin
  { Delphi Rio introduced a user-configurable growth strategy }
  FHeapCapacity := GrowCollection(FHeapCapacity, FHeapCapacity + 1);
  ReallocMem(FHeapData, FHeapCapacity * SizeOf(T));
end;
{$ENDIF}

procedure TStackList<T, TSize>.Initialize;
begin
  if IsManagedType(T) or IsManagedType(TSize) then
    raise EInvalidOperation.Create('A stack based collection cannot contain managed types');

  FStackCapacity := SizeOf(FStackData) div SizeOf(T);
  FHeapData := nil;
  FHeapCapacity := 0;
  FCount := 0;
end;

end.

