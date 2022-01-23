unit LinkedList.CRGP;

interface

{ Does not compile: "Undeclared identifier: 'TLinkedListItem'"
type                 |
  TLinkedListItem<T: TLinkedListItem> = class
  private
    FNext: T;
  end; }

type
  TLinkedListItem = class abstract
  private
    FNext: TLinkedListItem;
  end;

{type
  TLinkedListItem<T: TLinkedListItem> = class abstract(TLinkedListItem)

  end;

type
  Does not compile: "'T' is not compatible with type 'TLinkedListItem'"
                                  |
  TPerson = class(TLinkedListItem<TPerson>)
  end;}

type
  TLinkedListItem<T: class> = class abstract(TLinkedListItem)
  private
    function GetNext: T; inline;
  public
    class constructor Create;
  public
    property Next: T read GetNext;
  end;

type
  TLinkedList<T: TLinkedListItem> = class
  private
    FHead: T;
  public
    procedure Add(const AItem: T);

    property Head: T read FHead;
  end;

type
  TPerson = class(TLinkedListItem<TPerson>)
  private
    FName: String;
  public
    constructor Create(const AName: String);

    property Name: String read FName;
  end;

procedure TestLinkedListCRGP;

implementation

uses
  Utils;

procedure TestLinkedListCRGP;
begin
  StartSection;
  WriteLn('A linked list implementation using CRGP.');
  WriteLn('Enumerating person names:');

  { NOTE: We are creating memory leaks here. }
  var List := TLinkedList<TPerson>.Create;
  List.Add(TPerson.Create('Alice'));
  List.Add(TPerson.Create('Bob'));
  List.Add(TPerson.Create('Charlie'));

  var Person := List.Head;
  while (Person <> nil) do
  begin
    WriteLn('* ', Person.Name);
    Person := Person.Next;
  end;
end;

{ TLinkedListItem<T> }

class constructor TLinkedListItem<T>.Create;
begin
  Assert(T.InheritsFrom(TLinkedListItem));
end;

function TLinkedListItem<T>.GetNext: T;
begin
  Result := T(FNext);
end;

{ TLinkedList<T> }

procedure TLinkedList<T>.Add(const AItem: T);
begin
  if (AItem <> nil) then
  begin
    AItem.FNext := FHead;
    FHead := AItem;
  end;
end;

{ TPerson }

constructor TPerson.Create(const AName: String);
begin
  inherited Create;
  FName := AName;
end;

end.
