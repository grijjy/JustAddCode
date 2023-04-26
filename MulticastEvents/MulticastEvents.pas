unit MulticastEvents;

interface

type
  { A simple event handler that doesn't take any parameters other than a
    sender. }
  TEventHandler = procedure(const ASender: TObject) of object;

type
  { A multicast event that can fire events to multiple listeners.
    The events do not take any parameters, other than a sender.

    This multicast event is very lightweight and doesn't require more resources
    than a regular Delphi event (like TNotifyEvent) if there is only 1 listener
    (or no listeners). With a bit of code, it is also compatible with standard
    Delphi events so you can use them in your own published components and still
    be able to assign events using Delphi's object inspector.

    To use a multicast event, all you have to do is to add it to a class and
    provide access to it, for example through a read-only public (*not*
    published) property:

      type
        TMyButton = class(TControl)
        private
          FClickEvent: TMulticastEvent;
        public
          property ClickEvent: TMulticastEvent read FClickEvent;
        end;

    There is no need to create or initialize the multicast event. This is done
    automatically.

    Then multiple parties can listen for the click event like this:

      type
        TMyForm = class(TForm)
          TestButton: TMyButton;
        private
          procedure HandleTestButtonClick(const ASender: TObject);
        public
          constructor Create(const AOwner: TComponent); override;
        end;

      constructor TMyForm.Create(const AOwner: TComponent);
      begin
        inherited;
        TestButton.ClickEvent.Add(HandleTestButtonClick);
      end;

    This will call the HandleTestButtonClick method whenever the button is
    clicked. But another party can listen for click events as well:

      MyForm.TestButton.ClickEvent.Add(AnotherClickHandler);

    Now, whenever the button is clicked, two event handlers will be called.

    You can make the event compatible with "traditional" Delphi events by adding
    a regular (published) OnClick property with a custom getter and setter:

      type
        TMyButton = class(TControl)
        private
          FClickEvent: TMulticastEvent;
          function GetOnClick: TEventHandler;
          procedure SetOnClick(const AValue: TEventHandler);
        public
          property ClickEvent: TMulticastEvent read FClickEvent;
        published
          property OnClick: TEventHandler read GetOnClick write SetOnClick;
        end;

      function TMyButton.GetOnClick: TEventHandler;
      begin
        // A TMulticastEvent can be implicitly assigned to a TEventHandler
        Result := FClickEvent;
      end;

      procedure TMyButton.SetOnClick(const AValue: TEventHandler);
      begin
        // This will clear any existing listeners and add the given event handler
        FClickEvent.Assign(AValue);
      end;

    Now you can assign the OnClick event as you normally would (though code or
    the object inspector in Delphi's IDE).

    For the sender to fire the Click event, it only has to call its Invoke
    method:

      type
        TMyButton = class(TControl)
        private
          FClickEvent: TMulticastEvent;
        protected
          procedure DoClick;
        public
          property ClickEvent: TMulticastEvent read FClickEvent;
        end;

      procedure TMyButton.DoClick;
      begin
        FClickEvent.Invoke(Self);
      end;

    This will call the event handler of all added listeners, passing Self as the
    ASender parameter. }
  TMulticastEvent = record
  {$REGION 'Internal Declarations'}
  private type
    PEventHandler = ^TEventHandler;
  private type
    { A variant record that can hold either a single event handler, or a list
      of event handlers. An event handler is stored as a TMethod value (as
      declared in the System unit). Any Delphi method is just a pair of
      pointers: a pointer to the address of the code for the method
      (TMethod.Code) and a pointer to the object (class instance) on which to
      call the method (TMethod.Data).

      We need a way to know if this multicast event contains a single event
      handler or a list of event handlers. We could add a Boolean flag to TData
      to indicate this, but this adds additional overhead. Instead, we take
      advantage of the fact the Delphi objects are always at least 8-byte
      aligned. This means that the lower 3 bits of the TMethod.Data pointer (and
      thus also the _List value) are not used and always 0. We can use one of
      these bits to indicate if event has a single handler (0) or a list of
      handlers (1). We use the lowest bit for this purpose.

      Because this is a variant record, the Count and _List fields together
      occupy the same space as the Method field. Count *must* be declared as a
      NativeInt so that it is the same size as Method.Code on both 32-bit and
      64-bit platforms. Then, the _List pointer occupies (aliases) the same
      space as the Method.Data pointer. So the total size of a TMulticastEvent
      is the same as the size of a TMethod (which is 8 bytes on 32-bit
      platforms, and 16 bytes on 64-bit platforms) }
    TData = record
    case Byte of
      0: (Method: TMethod); // In case of a single event handler
      1: (Count: NativeInt; // In case of a list of event handlers. Aliases TMethod.Code
          _List: UIntPtr);  // Aliases TMethod.Data. Lowest bit indicates type
                            // (0=single handler, 1=list)
    end;
  private
    FData: TData;
  private
    function IsList: Boolean; inline;
    function GetList: PEventHandler; inline;
    procedure SetList(const AList: PEventHandler);
    function IndexOf(const AHandler: TEventHandler): Integer;
  {$ENDREGION 'Internal Declarations'}
  public
    { This is a custom managed record (CMR). For more use cases of CMR's, see:
      * https://blog.grijjy.com/2020/07/20/wrapping-c-apis-with-custom-managed-records/
      * https://blog.grijjy.com/2020/08/03/automate-restorable-operations-with-custom-managed-records/
      * https://blog.grijjy.com/2020/08/12/custom-managed-records-for-smart-pointers/

      This finalizer is automatically called when this record goes out of scope.
      In this case, it frees any memory used to maintain a list of event
      listeners. }
    class operator Finalize(var ADest: TMulticastEvent);

    { The Assign operator is automatically called when one CMR is assigned to
      another one. We don't allow that here, so we just raise an exception. }
    class operator Assign(var ADest: TMulticastEvent;
      const [ref] ASrc: TMulticastEvent);

    { A multicast event can be implicitly converted to a TEventHandler }
    class operator Implicit(const AEvent: TMulticastEvent): TEventHandler;

    { Adds an event handler to the multicast event. When the event is fired, all
      added event handlers (listeners) are called in order of addition.
      Note that you can add the same event handler multiple times, but you
      usually want to avoid that. }
    procedure Add(const AHandler: TEventHandler);

    { Assigns an event handler to the multicast event. This will remove all
      existing handlers and just add this one. If the AHandler parameter is nil,
      then it will just clear the list of handlers. }
    procedure Assign(const AHandler: TEventHandler);

    { Removes the given event handler from the multicast event. }
    procedure Remove(const AHandler: TEventHandler);

    { Invokes the multicast event. This will call all added event handlers in
      order of addition. The ASender parameter will be passed to each event
      handler. }
    procedure Invoke(const ASender: TObject);
  end;

type
  { An event handler that takes a single parameter (in addition to the sender).
    This is a generic handler with a parameter of type T. }
  TEventHandler<T> = procedure(const ASender: TObject; const AArg: T) of object;

type
  { A multicast event that can fire events to multiple listeners.
    The events take a single parameter of type T.

    This multicast event works identical to the one without parameters.
    See TMulticastEvent above for more information. }
  TMulticastEvent<T> = record
  {$REGION 'Internal Declarations'}
  private type
    TEventHandlerT = TEventHandler<T>;
    PEventHandlerT = ^TEventHandlerT;
  private
    FBase: TMulticastEvent;
  {$ENDREGION 'Internal Declarations'}
  public
    { A multicast event can be implicitly converted to a TEventHandler<T> }
    class operator Implicit(const AEvent: TMulticastEvent<T>): TEventHandler<T>; inline;

    { Adds an event handler to the multicast event. When the event is fired, all
      added event handlers (listeners) are called in order of addition.
      Note that you can add the same event handler multiple times, but you
      usually want to avoid that. }
    procedure Add(const AHandler: TEventHandler<T>); inline;

    { Assigns an event handler to the multicast event. This will remove all
      existing handlers and just add this one. If the AHandler parameter is nil,
      then it will just clear the list of handlers. }
    procedure Assign(const AHandler: TEventHandler<T>); inline;

    { Removes the given event handler from the multicast event. }
    procedure Remove(const AHandler: TEventHandler<T>); inline;

    { Invokes the multicast event. This will call all added event handlers in
      order of addition. The ASender and AArg parameters will be passed to each
      event handler. }
    procedure Invoke(const ASender: TObject; const AArg: T);
  end;

implementation

uses
  System.Classes;

{ TMulticastEvent }

procedure TMulticastEvent.Add(const AHandler: TEventHandler);
{ Adds the AHandler event listener }
var
  List: PEventHandler;
begin
  if (not Assigned(AHandler)) then
    Exit;

  if (FData.Method.Data = nil) then
    { There are no other listeners (yet) so we can assign this listener directly
      to the TData.Method field (using a typecast). }
    FData.Method := TMethod(AHandler)

    { Otherwise, we need to check if this multicast event already contains a
      list of event handler (using the IsList helper function). }
  else if (IsList) then
  begin
    { This multicast event already contains a list of event handlers. In this
      case, the number of handlers in the list should be at least 2 (otherwise,
      the single-handler situation above would be used). }
    Assert(FData.Count > 1);

    { Retrieve the list of handlers (using the GetList helper) and grow it to
      store one additional handler. Then store the list pointer again (using the
      SetList helper). }
    List := GetList;
    ReallocMem(List, (FData.Count + 1) * SizeOf(TEventHandler));
    SetList(List);

    { We use an old fashioned pointer to a list of items. To locate the correct
      element in the list, we need to increase this pointer with the number of
      items already in the list. }
    Inc(List, FData.Count);

    { Now we can assign the new event handler to this list item... }
    List^ := AHandler;

    { ...and increase the count }
    Inc(FData.Count);
  end
  else
  begin
    { The multicast event currently only contains a single event handler, stored
      in the FData.Method field. We need to turn this into a list of 2 items,
      where the first item will be set to the current handler in the
      FData.Method field, and the second item will be set to the new event
      handler we need to add.

      First, allocate a list to hold 2 items. }
    GetMem(List, 2 * SizeOf(TEventHandler));

    { Assign the existing event handler to the first item in the list (using a
      typecast) and store the list pointer (using the SetList helper). }
    List^ := TEventHandler(FData.Method);
    SetList(List);

    { We can now increase the list pointer to point to the second item in the
      list and assign our new event handler... }
    Inc(List);
    List^ := AHandler;

    { ...and set the count to 2 }
    FData.Count := 2;
  end;
end;

class operator TMulticastEvent.Assign(var ADest: TMulticastEvent;
  const [ref] ASrc: TMulticastEvent);
{ This Assign operator is automatically called when this multicast event is
  is assigned to another one. We don't allow that here, so we just raise an
  exception. }
begin
  raise EInvalidOperation.Create('Multicast events cannot be copied');
end;

procedure TMulticastEvent.Assign(const AHandler: TEventHandler);
{ Assigns an event handler to the multicast event. This will remove all
  existing handlers and just add this one. }
begin
  { If this is already a list, we free the list. }
  if (IsList) then
    FreeMem(GetList);

  { Start when a fresh unassigned event handler }
  FData.Method.Code := nil;
  FData.Method.Data := nil;

  { Add the new one. This will do nothing if AHandler is nil. }
  Add(AHandler);
end;

class operator TMulticastEvent.Finalize(var ADest: TMulticastEvent);
{ This finalizer is automatically called when this multicast event goes out of
  scope. }
begin
  { If this is a list of event handlers, then we need to free it. }
  if (ADest.IsList) then
    FreeMem(ADest.GetList);
end;

function TMulticastEvent.GetList: PEventHandler;
{ This is a helper that returns a list of event handlers.
  It is only used when this multicast event contains a list of handlers (that
  is, when GetList returns True). }
begin
  { We can just return the FData._List pointer, but we need to clear its lowest
    bit since that bit was so to 1 to indicate that the multicast event contains
    a list (instead of a single event handler). }
  Result := PEventHandler(FData._List and not 1);
end;

class operator TMulticastEvent.Implicit(
  const AEvent: TMulticastEvent): TEventHandler;
{ This operator is automatically called when a multicast event is assigned to
  a regular event handler. }
begin
  if (AEvent.IsList) then
  begin
    { If this is a list, then we return the first item in the list }
    Assert(AEvent.FData.Count > 1);
    var List := AEvent.GetList;
    Result := TEventHandler(List^);
  end
  else
    { Otherwise, the event handler is stored directly in FData.Method }
    Result := TEventHandler(AEvent.FData.Method);
end;

function TMulticastEvent.IndexOf(const AHandler: TEventHandler): Integer;
{ This helper function returns the index of the given event handler in a
  potential list of event handlers. }
begin
  if (IsList) then
  begin
    { If this is a list, then we need to iterate over all items in the list to
      find the event handler. }
    var List := GetList;
    for var I := 0 to FData.Count - 1 do
    begin
      { To compare event handlers, we need to cast them to TMethod records,
        which can be compared (because they implement an Equal operator) }
      if (PMethod(List)^ = TMethod(AHandler)) then
        Exit(I);

      Inc(List);
    end;
  end
  { Otherwise, if this a single event handler, we compare it directly to the
    FData.Method field. }
  else if (FData.Method = TMethod(AHandler)) then
    Exit(0);

  { Return -1 if not found }
  Result := -1;
end;

procedure TMulticastEvent.Invoke(const ASender: TObject);
{ Invokes the multicast event. This will call all added event handlers in order
  of addition. }
begin
  if (IsList) then
  begin
    { If this is a list, then iterate of all handlers in the list and call them,
      passing ASender as the parameter }
    Assert(FData.Count > 1);
    var List := GetList;
    for var I := 0 to FData.Count - 1 do
    begin
      { List is of type PEventHandler, so we need to dereference it (using ^) to
        call the method. }
      List^(ASender);
      Inc(List);
    end;
  end
  { Otherwise, if FData.Method is assigned, we can call it directly (by
    typecasting it to a TEventHandler). }
  else if (FData.Method.Data <> nil) then
    TEventHandler(FData.Method)(ASender);
end;

function TMulticastEvent.IsList: Boolean;
{ This helper checks if this multicast contains a single event or a list of
  event handlers. }
begin
  { The lowest bit of the FData._List field (or the FData.Method.Data field it
    aliases) is used to indicate if there is a single event (0) or a list of
    events (1) }
  Result := Boolean(FData._List and 1);
end;

procedure TMulticastEvent.Remove(const AHandler: TEventHandler);
{ Removes the given event handler from the multicast event. }
begin
  { Lookup the handler first. Just exit if not found. }
  var Index := IndexOf(AHandler);
  if (Index < 0) then
    Exit;

  if (not IsList) then
  begin
    { This multicast event contains a single event handler.
      Just clear its FData.Method field }
    Assert(Index = 0);
    FData.Method.Code := nil;
    FData.Method.Data := nil;
  end
  else
  begin
    { This multicast event contains a list of handlers.
      We need to remove it from the list. }
    var List := GetList;
    if (FData.Count = 2) then
    begin
      { This list currently contains 2 items. In this special case, we must
        remove the list feature and turn this multicast event into holding a
        single handler in its FData.Method field }
      Assert(Index < 2);
      if (Index = 0) then
      begin
        { The first item must be removed. This means we need to assign the
          second item to the FData.Method field. }
        Inc(List);
        FData.Method := TMethod(List^);
        Dec(List);
      end
      else
        { The second item must be removed. So we just assign the first item to
          the FData.Method field }
        FData.Method := TMethod(List^);

      { We must now free this list, since we don't use a list anymore. }
      FreeMem(List);
    end
    else
    begin
      { The list contains more than 2 items. We must remove the handler at
        position Index in the list. }
      Assert(FData.Count > 2);
      Dec(FData.Count);
      if (Index < FData.Count) then
      begin
        { There is a hole in the list. We need to move all items starting at
          poisition Index+1 in the list, to position Index. }
        var DstList := List;
        Inc(DstList, Index);

        var SrcList := DstList;
        Inc(SrcList);

        Move(SrcList^, DstList^, (FData.Count - Index) * SizeOf(TEventHandler));
      end;

      { And reallocate the memory used by the list to hold one less item }
      ReallocMem(List, FData.Count * SizeOf(TEventHandler));
      SetList(List);
    end;
  end;
end;

procedure TMulticastEvent.SetList(const AList: PEventHandler);
{ This is a helper that stores the given AList inside the FData._List field. }
begin
  { We need to set the lowest bit to 1 to indicate that we are using a list
    instead of a single event handler }
  FData._List := UIntPtr(AList) or 1;
end;

{ TMulticastEvent<T> }

procedure TMulticastEvent<T>.Add(const AHandler: TEventHandler<T>);
begin
  { Just call the base ("inherited") version, typecasting the handler to
    TEventHandler (which is safe, because every method, no matter its signature,
    is just a pair of 2 pointers). }
  FBase.Add(TEventHandler(AHandler));
end;

procedure TMulticastEvent<T>.Assign(const AHandler: TEventHandler<T>);
begin
  { Just call the base ("inherited") version. }
  FBase.Assign(TEventHandler(AHandler));
end;

class operator TMulticastEvent<T>.Implicit(
  const AEvent: TMulticastEvent<T>): TEventHandler<T>;
begin
  { Just call the base ("inherited") version.
    We need to typecast a TEventHandler to a TEventHandler<T>. }
  var Handler: TEventHandler := AEvent.FBase;
  Result := TEventHandler<T>(AEvent.FBase);
end;

procedure TMulticastEvent<T>.Invoke(const ASender: TObject; const AArg: T);
begin
  { This method works similar to the base ("inherited") version.
    We just need to pass an extra parameters (AArg) to the handlers. }
  if (FBase.IsList) then
  begin
    Assert(FBase.FData.Count > 1);
    var List := PEventHandlerT(FBase.GetList);
    for var I := 0 to FBase.FData.Count - 1 do
    begin
      List^(ASender, AArg);
      Inc(List);
    end;
  end
  else if (FBase.FData.Method.Data <> nil) then
    TEventHandlerT(FBase.FData.Method)(ASender, AArg);
end;

procedure TMulticastEvent<T>.Remove(const AHandler: TEventHandler<T>);
begin
  { Just call the base ("inherited") version. }
  FBase.Remove(TEventHandler(AHandler));
end;

end.
