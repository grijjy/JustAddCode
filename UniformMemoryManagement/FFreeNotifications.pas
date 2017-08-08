unit FFreeNotifications;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Messaging,
  FMX.Types,
  FMX.Graphics,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,
  FMX.StdCtrls,
  FMX.Controls.Presentation,
  FMX.ScrollBox,
  FMX.Memo,
  FBase,
  UBase,
  USampleClasses;

type
  TFrameFreeNotifications = class(TFrameBase)
    ButtonCreateObjects: TButton;
    ButtonFreeFirst: TButton;
    ButtonFreeSecond: TButton;
    procedure ButtonCreateObjectsClick(Sender: TObject);
    procedure ButtonFreeFirstClick(Sender: TObject);
    procedure ButtonFreeSecondClick(Sender: TObject);
  private
    { Private declarations }
    FObject1, FObject2: TFreeNotificationObject;
    procedure FreeNotificationListener(const Sender: TObject; const M: TMessage);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.fmx}

procedure TFrameFreeNotifications.ButtonCreateObjectsClick(Sender: TObject);
begin
  ButtonCreateObjects.Enabled := False;
  ButtonFreeFirst.Enabled := True;
  ButtonFreeSecond.Enabled := True;

  { Create two objects. }
  FObject1 := TFreeNotificationObject.Create(nil, 'Object1');
  FObject2 := TFreeNotificationObject.Create(nil, 'Object2');

  { Enable free notifications for both objects.
    Because we subscribed to the TFreeNotification message in the constructor,
    we will get notified when these objects are destroyed. }
  FObject1.EnableFreeNotification;
  FObject2.EnableFreeNotification;

  Log('----------------------------------------------------');
  Log('Created two objects and enabled free notifications for both objects.');
  Log(' * TFreeNotificationObject.InstanceCount = %d', [TFreeNotificationObject.InstanceCount]);
  Log('');
end;

procedure TFrameFreeNotifications.ButtonFreeFirstClick(Sender: TObject);
begin
  ButtonFreeFirst.Enabled := False;

  Log('About to free the first object. This will result in a TFreeNotification message.');

  { Because we subscribed to the TFreeNotification message, freeing this object
    will send a notification message. Note that we need to set the reference
    to nil to really free the object on ARC platforms. }
  FreeAndNil(FObject1);

  Log('Completed freeing the first object.');
  Log(' * TFreeNotificationObject.InstanceCount = %d', [TFreeNotificationObject.InstanceCount]);
  Log('');

  if (TFreeNotificationObject.InstanceCount = 0) then
    ButtonCreateObjects.Enabled := True;
end;

procedure TFrameFreeNotifications.ButtonFreeSecondClick(Sender: TObject);
begin
  ButtonFreeSecond.Enabled := False;

  Log('About to free the second object. This will result in a TFreeNotification message.');

  { Because we subscribed to the TFreeNotification message, freeing this object
    will send a notification message. Note that we need to set the reference
    to nil to really free the object on ARC platforms. }
  FreeAndNil(FObject2);

  Log('Completed freeing the second object.');
  Log(' * TFreeNotificationObject.InstanceCount = %d', [TFreeNotificationObject.InstanceCount]);
  Log('');

  if (TFreeNotificationObject.InstanceCount = 0) then
    ButtonCreateObjects.Enabled := True;
end;

constructor TFrameFreeNotifications.Create(AOwner: TComponent);
begin
  inherited;
  { Subscribe to the TFreeNotification message to get notified when objects
    derived from TFreeNotificationBase are freed. }
  TMessageManager.DefaultManager.SubscribeToMessage(TFreeNotificationMessage,
    FreeNotificationListener);
end;

destructor TFrameFreeNotifications.Destroy;
begin
  { Unsubscribe from the TFreeNotification message. }
  TMessageManager.DefaultManager.Unsubscribe(TFreeNotificationMessage,
    FreeNotificationListener);
  inherited;
end;

procedure TFrameFreeNotifications.FreeNotificationListener(
  const Sender: TObject; const M: TMessage);
begin
  { This method is called when an object derived from TFreeNotificationBase
    (whose EnableFreeNotification has been called) method is about to be freed.
    The Sender parameter contains the object that is about to freed. We don't
    care about the M parameter, since it is just used to describe the message
    type (which should be TFreeNotification). }
  Assert(M is TFreeNotificationMessage);
  Assert(Sender is TFreeNotificationObject);
  Log('TFreeNotificationMessage received for: '
    + TFreeNotificationObject(Sender).Name);
end;

end.
