unit InstanceTracker;

{ When using this unit, it will track the number of instanced of most classes
  in the application. For most accurate results, it is recommended to put this
  unit at the top of the uses-clause of the project (.dpr) file.

  On Windows, macOS, iOS Simulator and Linux, it will track allocations and
  deallocations of all classes.

  On iOS and Android, it will only track allocations of classes that have RTTI
  (Run-Time Type Information).

  Call GetInstanceCounts to retrieve an array if the number of live instances
  at the time of the call. }

interface

uses
   InstanceHashMap;

{ Returns an array of instance counts. Each entry contains a class and the
  number of live instances of that class.
  Note that some classes may have an instance count of 0 in case all instances
  of that class have been destroyed. }
function GetInstanceCounts: TArray<TInstanceInfo>;

implementation

uses
  System.Rtti,
  System.SysUtils,
  System.SyncObjs,
  Hooking;

type
  { These "class opener" types give us access to the protected FRefCount
    fields of TObject and TInterfacedObject. }
  TObjectOpener = class(TObject);
  TInterfacedObjectOpener = class(TInterfacedObject);

var
  { This hash map keeps track of a number of instances per class. }
  GInstanceCounts: TInstanceHashMap = nil;

  { Lock to make GInstanceCounts thread-safe. }
  GLock: TCriticalSection = nil;

procedure TrackInstance(const AInstance: TObject);
begin
  { This routine keeps track of AInstance. In this example, this means
    incrementing the instance count of the class of AInstance.

    Note that multiple threads can create object simultaneously. So we need to
    synchronize access to the hash map that keeps track of the instance
    counts. }
  if Assigned(AInstance) and Assigned(GLock) and Assigned(GInstanceCounts) then
  begin
    GLock.Acquire;
    try
      GInstanceCounts.Increment(AInstance.ClassType);
    finally
      GLock.Release;
    end;
  end;
end;

procedure UntrackInstance(const AInstance: TObject);
begin
  { AInstance is about to be destroyed, so we need to decrement the instance
    count of the class of AInstance. }
  if Assigned(AInstance) and Assigned(GLock) and Assigned(GInstanceCounts) then
  begin
    GLock.Acquire;
    try
      GInstanceCounts.Decrement(AInstance.ClassType);
    finally
      GLock.Release;
    end;
  end;
end;

function GetInstanceCounts: TArray<TInstanceInfo>;
begin
  { Convert the hash map with instance counts to an array of TInstanceInfo
    records. }
  Result := nil;
  if Assigned(GLock) and Assigned(GInstanceCounts) then
  begin
    GLock.Acquire;
    try
      Result := GInstanceCounts.ToArray;
    finally
      GLock.Release;
    end;
  end;
end;

{ The following 3 routines implement the hooks for TObject.NewInstance,
  TInterfacedObject.NewInstance and TObject.FreeInstance.

  The implementation of these routines is identical to the original NewInstance
  and FreeInstance methods, but in addition it tracks (or untracks) an
  instance. }

function HookedObjectNewInstance(const Self: TClass): TObject {$IFDEF AUTOREFCOUNT} unsafe {$ENDIF};
var
  Instance: Pointer;
begin
  { This is the hook for TObject.NewInstance. Since this method is a
    (non-static) class method, it has an implicit Self parameter. But since it
    is a class method, this Self parameter represents a class, not an object.

    We start by mimicking the original source code for TObject.NewInstance: }
  GetMem(Instance, Self.InstanceSize);
  Result := Self.InitInstance(Instance);
  {$IFDEF AUTOREFCOUNT}
  { On ARC platforms, each object has a FRefCount field that must be
    initialized to 1. }
  TObjectOpener(Result).FRefCount := 1;
  {$ENDIF}

  { Now we can keep track of this instance. }
  TrackInstance(Result);
end;

function HookedInterfacedObjectNewInstance(const Self: TClass): TObject {$IFDEF AUTOREFCOUNT} unsafe {$ENDIF};
var
  Instance: Pointer;
begin
  { This is the hook for TInterfacedObject.NewInstance. This method is mostly
    similar to TObject.NewInstance, with the exception that interfaced objects
    also have a FRefCount field on non-ARC platforms. }
  GetMem(Instance, Self.InstanceSize);
  Result := Self.InitInstance(Instance);
  TInterfacedObjectOpener(Result).FRefCount := 1;

  { Now we can keep track of this instance. }
  TrackInstance(Result);
end;

procedure HookedObjectFreeInstance(const Self: TObject);
begin
  { This is the hook for TObject.FreeInstance. Since this is a (regular) method,
    it has an implicit Self parameter containing the instance. We first stop
    tracking this instance... }
  UntrackInstance(Self);

  { ...and then execute the original code in TObject.FreeInstance: }
  Self.CleanupInstance;
  FreeMem(Pointer(Self));
end;

function InitializeCodeHooks: Boolean;
begin
  { This function tries HookCode to hook the implementations of the
    TObject.NewInstance and TObject.FreeInstance methods. This will most likely
    only succeed on Windows, macOS, iOS Simulator and Linux. }
  Result := HookCode(@TObject.NewInstance, @HookedObjectNewInstance)
        and HookCode(@TObject.FreeInstance, @HookedObjectFreeInstance);
end;

{ We are using the vmtNewInstance and vmtFreeInstance constants, which have been
  deprecated for a long time, but are still available. Turn off warnings for
  these. }
{$WARN SYMBOL_DEPRECATED OFF}

procedure InitializeVMTHooks;
var
  Rtti: TRttiContext;
  RttiType: TRttiType;
  InstanceType: TRttiInstanceType;
  VMTEntryNewInstance, VMTEntryFreeInstance: PPointer;
  ObjectNewInstance, ObjectFreeInstance, InterfacedObjectNewInstance: Pointer;
begin
  { This version uses HookVMT instead of HookCode to hook the
    TObject.NewInstance and TObject.FreeInstance methods.

    Each Delphi class has its own Virtual Method Table. This means that we need
    to hook the VMT's for all classes we care about. In this case, we use
    TRttiContext.GetTypes to get a list of all Delphi classes (and other types)
    linked into the application. We then change the VMT entries of each class
    in that list.

    The problem with this kind of hooking is that some classes may have
    overridden the NewInstance and/or FreeInstance methods. Changing the VMT of
    those classes would ignore any customizations those classes made to those
    methods, and we don't want that. Fortunately, there are very few classes
    that have overridden these methods.

    So we only change the VMT's of those classes that have not overridden
    NewInstance or FreeInstance. This single exception is the TInterfacedObject
    class, which is so common that we want to support it. This class has
    overridden the NewInstance method, so we need a separate hook for this
    version.

    First, we retrieve the code addresses of the original NewInstance and
    FreeInstance methods. We use these to check if they are overridden by a
    certain class. }
  ObjectNewInstance := @TObject.NewInstance;
  ObjectFreeInstance := @TObject.FreeInstance;
  InterfacedObjectNewInstance := @TInterfacedObject.NewInstance;

  { Get a list of all Delphi types in the application with RTTI support. }
  Rtti := TRttiContext.Create;
  for RttiType in Rtti.GetTypes do
  begin
    { Check if the type is a class type. }
    if (RttiType.TypeKind = tkClass) then
    begin
      { We can now safely typecase to TRttiInstanceType }
      InstanceType := TRttiInstanceType(RttiType);

      { Retrieve the entry in the VMT of the FreeInstance method for this class. }
      VMTEntryFreeInstance := PPointer(PByte(InstanceType.MetaclassType) + vmtFreeInstance);

      { Only track classes that didn't override TObject.FreeInstance. }
      if (VMTEntryFreeInstance^ = ObjectFreeInstance) then
      begin
        { Retrieve the entry in the VMT of the NewInstance method for this class. }
        VMTEntryNewInstance := PPointer(PByte(InstanceType.MetaclassType) + vmtNewInstance);

        { Only track classes that didn't override TObject.NewInstance or
          TInterfacedObject.NewInstance. }
        if (VMTEntryNewInstance^ = ObjectNewInstance) then
        begin
          { This class uses NewInstance and FreeInstance from TObject.
            Hook those VMT entries. }
          HookVMT(VMTEntryNewInstance, @HookedObjectNewInstance);
          HookVMT(VMTEntryFreeInstance, @HookedObjectFreeInstance);
        end
        else if (VMTEntryNewInstance^ = InterfacedObjectNewInstance) then
        begin
          { This class is (ultimately) derived from TInterfacedObject, so
            we need to hook to a separate version of NewInstance. }
          HookVMT(VMTEntryNewInstance, @HookedInterfacedObjectNewInstance);
          HookVMT(VMTEntryFreeInstance, @HookedObjectFreeInstance);
        end;
      end;
    end;
  end;
end;

{$WARN SYMBOL_DEPRECATED ON}

procedure InitializeGlobals;
begin
  { These globals are used to keep track of instances. }
  GLock := TCriticalSection.Create;
  GInstanceCounts := TInstanceHashMap.Create;
end;

procedure FinalizeGlobals;
begin
  FreeAndNil(GLock);
  FreeAndNil(GInstanceCounts);
end;

initialization
  { First we try code hooking to hook into the NewInstance and FreeInstance
    methods. This is fastest and tracks all classes. }
  if (not InitializeCodeHooks) then
    { If the first method fails, try VMT hooking instead. This hooks the
      NewInstance and FreeInstance entries in the Virtual Method Tables of all
      classes that have RTTI. }
    InitializeVMTHooks;

  { Initialize some global variables. }
  InitializeGlobals;

finalization
  FinalizeGlobals;

end.
