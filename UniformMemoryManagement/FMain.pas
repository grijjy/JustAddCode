unit FMain;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Controls.Presentation,
  FMX.TabControl,
  FBase,
  FOwnership,
  FFreeNotifications,
  FManagedTypes;

type
  TFormMain = class(TForm)
    TabControl: TTabControl;
    TabItemManaged: TTabItem;
    TabItemOwnership: TTabItem;
    TabItemFreeNotification: TTabItem;
    FrameOwnership: TFrameOwnership;
    FrameFreeNotifications: TFrameFreeNotifications;
    FrameManagedTypes: TFrameManagedTypes;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.fmx}

end.
