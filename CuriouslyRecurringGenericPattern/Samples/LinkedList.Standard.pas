unit LinkedList.Standard;

interface

type
  TLinkedListItem = class abstract
  private
    FNext: TLinkedListItem;
  public
    property Next: TLinkedListItem read FNext;
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
  TPerson = class(TLinkedListItem)
  private
    FName: String;
  public
    constructor Create(const AName: String);

    property Name: String read FName;
  end;

procedure TestLinkedListStandard;

implementation

uses
  Utils;

procedure TestLinkedListStandard;
begin
  StartSection;
  WriteLn('A standard linked list implementation.');
  WriteLn('Enumerating person names:');

  { NOTE: We are creating memory leaks here. }
  var List := TLinkedList<TPerson>.Create;
  List.Add(TPerson.Create('Alice'));
  List.Add(TPerson.Create('Bob'));
  List.Add(TPerson.Create('Charlie'));

  var Person := List.Head;
  while (Person <> nil) do
  begin
    WriteLn('* ',  Person.Name);

    { This does not compile:

        Incompatible types: 'TPerson' and 'TLinkedListItem'

      because Person is of type TPerson, and Person.Next returns a
      TLinkedListItem, which is not assignment compatible }
    // Person := Person.Next;

    { So we need to typecast (if you are absolutely sure of the type) or use
      the "as" operator }
    Person := Person.Next as TPerson;
  end;
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
