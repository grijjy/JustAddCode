unit UBase;

interface

uses
  System.Messaging;

type
  { Abstract base class for node objects. Nodes can have a parent and zero or
    more children.

    On non-ARC platforms, parent nodes own their children, and freeing a parent
    object will automatically free its children.

    On ARC platforms, memory is managed using the usual reference counting, but
    parent and children can still be used to build a node tree.

    To free a node and remove it from its parent, call the Delete method.

    This provides a form a semi-automatic memory management on all platforms.

    NOTE: By default, this class uses a singly-linked list to keep track of its
    children. You can set the DOUBLY_LINKED_LIST conditional define to use
    a double-linked list instead. This uses a bit more memory, but increases
    the speed of Delete operations. }
  TNode = class abstract
  strict private
    [unsafe] FParent: TNode;
    FFirstChild: TNode;
    FNextSibling: TNode;
    {$IFDEF DOUBLY_LINKED_LIST}
    [unsafe] FPrevSibling: TNode;
    {$ENDIF !DOUBLY_LINKED_LIST}
  public
    { Creates a new instance.

      Parameters:
        AParent: the parent for this node.

      On non-ARC platforms, when the parent is destroyed, this node is
      destroyed as well. }
    constructor Create(const AParent: TNode);
    destructor Destroy; override;

    { Deletes this node from the parent node and frees it.
      Note that this is different than calling Free, because on ARC platforms,
      a parent node keeps a strong reference to its children, so Free will not
      actually free the node. }
    procedure Delete;

    { Parent of this node. When the parent is destroyed, this node is destroyed
      as well. }
    property Parent: TNode read FParent;

    { The first child node. The enumerate all children, you can walk the list
      like this:

        var
          Child: TNode;
        begin
          Child := SomeObject.FirstChild;
          while Assigned(Child) do
          begin
            ..Use Child object here..
            Child := Child.NextSibling;
          end;
        end;

      Note that children are walked in reverse order that the order they were
      added. This is because new objects are added to the FRONT of the linked
      list. }
    property FirstChild: TNode read FFirstChild;

    { The next sibling in the parents list of children. Can be used to walk the
      list of children (see FirstChild for an example). }
    property NextSibling: TNode read FNextSibling;
  end;

type
  { The type of message that is broadcast when a node is about to be
    destroyed. See TNode.EnableFreeNotification. }
  TFreeNotificationMessage = class(TMessage);

type
  { Abstract base class that sends a global notification when the object is
    being destroyed.

    Because this class is derived from TNode, it supports semi-automatic
    memory management.

    NOTE: You could merge the functionality of this class into TNode, so
    that all TNode-derived classes can benefit from sending free
    notifications. I keep it separate here for demonstration purposes. }
  TFreeNotificationBase = class abstract(TNode)
  private class var
    FNotificationMessage: TFreeNotificationMessage;
  private
    FNotifyOnFree: Boolean;
  public
    class constructor Create;
    class destructor Destroy;
  public
    destructor Destroy; override;

    { Call this method if you want to get notified when this object is about to
      be destroyed. You will get this notification by listening for messages of
      type TFreeNotification. }
    procedure EnableFreeNotification;
  end;

implementation

{ TNode }

constructor TNode.Create(const AParent: TNode);
begin
  inherited Create;
  if Assigned(AParent) then
  begin
    { Add ourself to the linked list of the parent }
    FParent := AParent;
    FNextSibling := AParent.FFirstChild;

    {$IFDEF DOUBLY_LINKED_LIST}
    if (AParent.FFirstChild <> nil) then
      AParent.FFirstChild.FPrevSibling := Self;
    {$ENDIF}

    AParent.FFirstChild := Self;
  end;
end;

procedure TNode.Delete;
{$IFNDEF DOUBLY_LINKED_LIST}
var
  Cur: TNode;
{$ENDIF}
begin
  { Remove ourself from the parents linked list }

  {$IFDEF DOUBLY_LINKED_LIST}
  { When using a doubly-linked list, we only have to update some links. }
  if (FPrevSibling <> nil) then
    FPrevSibling.FNextSibling := FNextSibling;

  if (FNextSibling <> nil) then
    FNextSibling.FPrevSibling := FPrevSibling;

  if (FParent <> nil) and (FParent.FFirstChild = Self) then
    FParent.FFirstChild := FNextSibling;
  {$ELSE}
  { When using a singly-linked list, we may need to perform a linear search in
    that list to find ourself. }
  if (FParent <> nil) then
  begin
    Cur := FParent.FFirstChild;
    if (Cur = Self) then
      { We are at the head of the linked list. We only need to update a link. }
      FParent.FFirstChild := FNextSibling
    else
    begin
      { We are somewhere in the middle of the linked list. Find ourself and
        update the link. }
      while (Cur <> nil) and (Cur.FNextSibling <> Self) do
        Cur := Cur.FNextSibling;

      if Assigned(Cur) then
        Cur.FNextSibling := FNextSibling;
    end;
  end;
  {$ENDIF}

  { Now we can free ourself }
  DisposeOf;
end;

{$IFDEF AUTOREFCOUNT}
destructor TNode.Destroy;
begin
  { Memory is managed by ARC }
  inherited;
end;
{$ELSE !AUTOREFCOUNT}
destructor TNode.Destroy;
var
  Cur, Next: TNode;
begin
  { Free all children on non-ARC platforms. }
  Cur := FFirstChild;
  while Assigned(Cur) do
  begin
    Next := Cur.FNextSibling;
    Cur.Free;
    Cur := Next;
  end;
  inherited;
end;
{$ENDIF !AUTOREFCOUNT}

{ TFreeNotificationBase }

class constructor TFreeNotificationBase.Create;
begin
  { We create a single instance of TFreeNotification for all free notification
    messages. }
  FNotificationMessage := TFreeNotificationMessage.Create;
end;

class destructor TFreeNotificationBase.Destroy;
begin
  FNotificationMessage.Free;
end;

destructor TFreeNotificationBase.Destroy;
begin
  if (FNotifyOnFree) then
    { Send a free notification message to anyone who is listening. We set the
      third parameter to False so that the message will not be destroyed, and
      we can reuse if for other notifications. }
    TMessageManager.DefaultManager.SendMessage(Self, FNotificationMessage, False);
  inherited;
end;

procedure TFreeNotificationBase.EnableFreeNotification;
begin
  FNotifyOnFree := True;
end;

end.
