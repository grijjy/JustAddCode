unit InstanceHashMap;

{ Specialized hash map (dictionary) to keep track of instance counts.
  Similar to using TDictionary<TClass, Integer>, but doesn't create any
  temporary (enumerator) objects when enumerating the dictionary. }

interface

type
  { Information about object instances. }
  TInstanceInfo = record
    { The object class. }
    Clazz: TClass;

    { The number of live instances of Clazz. }
    Count: Integer;
  end;

type
  TInstanceHashMap = class
  private const
    EMPTY_HASH = -1;
  private type
    TItem = record
      HashCode: Integer;
      Clazz: TClass;
      Count: Integer;
    end;
  private
    FItems: TArray<TItem>;
    FCount: Integer;
    FGrowThreshold: Integer;
    procedure Resize(ANewSize: Integer);
    procedure UpdateCount(const AClass: TClass; const ADelta: Integer);
  public
    { Increments the instance count for the given class }
    procedure Increment(const AClass: TClass);

    { Decrements the instance count for the given class }
    procedure Decrement(const AClass: TClass);

    { Returns all classes with instance counts in an array. }
    function ToArray: TArray<TInstanceInfo>;
  end;

implementation

{ TInstanceHashMap }

procedure TInstanceHashMap.Decrement(const AClass: TClass);
begin
  UpdateCount(AClass, -1);
end;

procedure TInstanceHashMap.Increment(const AClass: TClass);
begin
  UpdateCount(AClass, +1);
end;

procedure TInstanceHashMap.Resize(ANewSize: Integer);
var
  NewMask, I, NewIndex: Integer;
  OldItems, NewItems: TArray<TItem>;
begin
  if (ANewSize < 4) then
    ANewSize := 4;
  NewMask := ANewSize - 1;
  SetLength(NewItems, ANewSize);
  for I := 0 to ANewSize - 1 do
    NewItems[I].HashCode := EMPTY_HASH;
  OldItems := FItems;

  for I := 0 to Length(OldItems) - 1 do
  begin
    if (OldItems[I].HashCode <> EMPTY_HASH) then
    begin
      NewIndex := OldItems[I].HashCode and NewMask;
      while (NewItems[NewIndex].HashCode <> EMPTY_HASH) do
        NewIndex := (NewIndex + 1) and NewMask;
      NewItems[NewIndex] := OldItems[I];
    end;
  end;

  FItems := NewItems;
  FGrowThreshold := (ANewSize * 3) shr 2;
end;

function TInstanceHashMap.ToArray: TArray<TInstanceInfo>;
var
  I, Count: Integer;
begin
  SetLength(Result, FCount);
  Count := 0;
  for I := 0 to Length(FItems) - 1 do
  begin
    if (FItems[I].HashCode <> EMPTY_HASH) then
    begin
      Result[Count].Clazz := FItems[I].Clazz;
      Result[Count].Count := FItems[I].Count;
      Inc(Count);
    end;
  end;
  Assert(Count = FCount);
end;

procedure TInstanceHashMap.UpdateCount(const AClass: TClass;
  const ADelta: Integer);
var
  Mask, Index, HashCode, HC: Integer;
begin
  if (FCount >= FGrowThreshold) then
    Resize(Length(FItems) * 2);

  {$IF Defined(CPU64BITS)}
  { On 64-bit CPUs, calculate a hash code by discarding the lowest 3 bits
    of the class pointer (since those are always 0). Mix result with upper
    31 bits of the class pointer.}
  HashCode := (UIntPtr(AClass) shr 3) xor Integer(UIntPtr(AClass) shr 33);
  {$ELSE}
  { On 64-bit CPUs, calculate a hash code by discarding the lowest 2 bits
    of the class pointer (since those are always 0). }
  HashCode := UIntPtr(AClass) shr 2;
  {$ENDIF}

  Mask := Length(FItems) - 1;
  Index := HashCode and Mask;

  while True do
  begin
    HC := FItems[Index].HashCode;
    if (HC = EMPTY_HASH) then
      Break;

    if (HC = HashCode) and (FItems[Index].Clazz = AClass) then
    begin
      { Item already exists. Update count. }
      FItems[Index].Count := FItems[Index].Count + ADelta;
      Exit;
    end;

    Index := (Index + 1) and Mask;
  end;

  { Item does not exist. Add it, but only if we are incrementing. }
  if (ADelta = 1) then
  begin
    FItems[Index].HashCode := HashCode;
    FItems[Index].Clazz := AClass;
    FItems[Index].Count := 1;
    Inc(FCount);
  end;
end;

end.
