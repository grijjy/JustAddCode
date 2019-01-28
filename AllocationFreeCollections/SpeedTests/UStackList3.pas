unit UStackList3;

interface

type
  { As TStackList2, but add inlining }
  TStackList3<T: record; TSize: record> = record
  private type
    P = ^T;
  private
    FData: TSize;
    FCapacity: Integer;
    FCount: Integer;
    function GetItem(const AIndex: Integer): T; inline;
  public
    procedure Initialize;
    procedure Add(const AItem: T); inline;

    property Count: Integer read FCount;
    property Items[const AIndex: Integer]: T read GetItem; default;
  end;

implementation

uses
  System.Classes,
  System.SysUtils;

{ TStackList3<T, TSize> }

procedure TStackList3<T, TSize>.Add(const AItem: T);
var
  Target: P;
begin
  if (FCount < FCapacity) then
  begin
    Target := @FData;
    Inc(Target, FCount);
    Target^ := AItem;

    Inc(FCount);
  end
  else
    raise EInvalidOperation.Create('List is full');
end;

function TStackList3<T, TSize>.GetItem(const AIndex: Integer): T;
var
  Item: P;
begin
  if (AIndex >= 0) and (AIndex < FCount) then
  begin
    Item := @FData;
    Inc(Item, AIndex);
    Result := Item^;
  end
  else
    raise EArgumentOutOfRangeException.Create('List index out of range');
end;

procedure TStackList3<T, TSize>.Initialize;
begin
  if IsManagedType(T) or IsManagedType(TSize) then
    raise EInvalidOperation.Create('A stack based collection cannot contain managed types');

  FCapacity := SizeOf(FData) div SizeOf(T);
  FCount := 0;
end;

end.

