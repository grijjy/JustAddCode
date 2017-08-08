unit USampleClasses;

interface

uses
  UBase;

type
  { We use an "M" prefix to indicate "Managed" types.

    On ARC platforms, all classes are managed, so the "M" prefix is on regular
    classes.

    On non-ARC platforms, we use an object interface to enable ARC. So the "M"
    prefix is applied to the interface. }
  {$IFDEF AUTOREFCOUNT}
  TSample = class;
  MSample = TSample;
  {$ELSE}
  MSample = interface
    { Property access methods }
    function GetLink: MSample;
    procedure SetLink(const AValue: MSample);

    { Properties }
    property Link: MSample read GetLink write SetLink;
  end;
  {$ENDIF}

  { This is the actual class. You should always define variables of this class
    of type MSample (and NOT of type TSample). You only use TSample to create
    MSample instances, or to access class properties/methods.

    On ARC platforms, MSample is just an alias for TSample.
    On non-ARC platforms, MSample defines the interface that is implemented in
    TSample. }
  TSample = class{$IFNDEF AUTOREFCOUNT}(TInterfacedObject, MSample){$ENDIF}
  private class var
    FInstanceCount: Integer;
  private
    [weak] FLink: MSample;
  protected
    { MSample implementation }
    function GetLink: MSample;
    procedure SetLink(const AValue: MSample);
  public
    { These methods are just overridded to update the FInstanceCount class
      variable. We want to do that at the locations where memory for the object
      is actually allocated and freed. So we should not use the constructor or
      destructor for this. }
    class function NewInstance: TObject {$IFDEF AUTOREFCOUNT} unsafe {$ENDIF}; override;
    procedure FreeInstance; override;
  public
    { Use this property to link two objects together and create a reference
      cycle.

      Note: on ARC platforms, this property is access directly and very
      efficiently. On non-ARC platforms, this property will be access through
      the MSample.GetLink method. }
    property Link: MSample read FLink write FLink;

    { The number of instances of this class that are live. }
    class property InstanceCount: Integer read FInstanceCount;
  end;

type
  { A simple node class. }
  TSampleNode = class(TNode)
  private class var
    FInstanceCount: Integer;
  private
    [weak] FLink: TSampleNode;
  public
    { These methods are just overridded to update the FInstanceCount class
      variable. We want to do that at the locations where memory for the object
      is actually allocated and freed. So we should not use the constructor or
      destructor for this. }
    class function NewInstance: TObject {$IFDEF AUTOREFCOUNT} unsafe {$ENDIF}; override;
    procedure FreeInstance; override;
  public
    { Use this property to link two objects together and create a reference
      cycle. }
    property Link: TSampleNode read FLink write FLink;

    { The number of instances of this class that are live. }
    class property InstanceCount: Integer read FInstanceCount;
  end;

type
  { A simple class that can send a free notification when it is destroyed. }
  TFreeNotificationObject = class(TFreeNotificationBase)
  private class var
    FInstanceCount: Integer;
  private
    FName: String;
  public
    { These methods are just overridded to update the FInstanceCount class
      variable. We want to do that at the locations where memory for the object
      is actually allocated and freed. So we should not use the constructor or
      destructor for this. }
    class function NewInstance: TObject {$IFDEF AUTOREFCOUNT} unsafe {$ENDIF}; override;
    procedure FreeInstance; override;
  public
    constructor Create(const AParent: TNode; const AName: String);

    { The name of the object for identification purposes. }
    property Name: String read FName;

    { The number of instances of this class that are live. }
    class property InstanceCount: Integer read FInstanceCount;
  end;

implementation

{ TSample }

procedure TSample.FreeInstance;
begin
  inherited;
  Dec(FInstanceCount);
end;

function TSample.GetLink: MSample;
begin
  Result := FLink;
end;

class function TSample.NewInstance: TObject;
begin
  Inc(FInstanceCount);
  Result := inherited NewInstance;
end;

procedure TSample.SetLink(const AValue: MSample);
begin
  FLink := AValue;
end;

{ TSampleNode }

procedure TSampleNode.FreeInstance;
begin
  inherited;
  Dec(FInstanceCount);
end;

class function TSampleNode.NewInstance: TObject;
begin
  Inc(FInstanceCount);
  Result := inherited NewInstance;
end;

{ TFreeNotificationObject }

constructor TFreeNotificationObject.Create(const AParent: TNode;
  const AName: String);
begin
  inherited Create(AParent);
  FName := AName;
end;

procedure TFreeNotificationObject.FreeInstance;
begin
  inherited;
  Dec(FInstanceCount);
end;

class function TFreeNotificationObject.NewInstance: TObject;
begin
  Inc(FInstanceCount);
  Result := inherited NewInstance;
end;

end.
