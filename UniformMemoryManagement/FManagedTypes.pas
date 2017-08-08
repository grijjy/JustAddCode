unit FManagedTypes;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
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
  USampleClasses;

type
  TFrameManagedTypes = class(TFrameBase)
    ButtonCreateObjects: TButton;
    ButtonReleaseFirst: TButton;
    ButtonReleaseSecond: TButton;
    procedure ButtonCreateObjectsClick(Sender: TObject);
    procedure ButtonReleaseFirstClick(Sender: TObject);
    procedure ButtonReleaseSecondClick(Sender: TObject);
  private
    { Private declarations }
    FObject1, FObject2: MSample;
  public
    { Public declarations }
  end;

implementation

{$R *.fmx}

procedure TFrameManagedTypes.ButtonCreateObjectsClick(Sender: TObject);
begin
  ButtonCreateObjects.Enabled := False;
  ButtonReleaseFirst.Enabled := True;
  ButtonReleaseSecond.Enabled := True;

  { Create two objects. }
  FObject1 := TSample.Create;
  FObject2 := TSample.Create;

  { Create a reference cycle between these two objects. Since the Link property
    is implemented using a [weak] reference, this will NOT lead to a memory
    leak.

    Note that [weak] references for object interfaces on non-ARC platforms are
    only supported since Delphi 10.1 Berlin. So this code will create a memory
    leak when compiled for Windows or macOS with an older compiler. }
  FObject1.Link := FObject2;
  FObject2.Link := FObject1;

  Log('----------------------------------------------------');
  Log('Created two objects with a reference cycle.');
  Log(' * TSample.InstanceCount = %d', [TSample.InstanceCount]);
  Log('');
end;

procedure TFrameManagedTypes.ButtonReleaseFirstClick(Sender: TObject);
begin
  ButtonReleaseFirst.Enabled := False;

  { Release first object. If FObject2 still exists and has a link to FObject1,
    then this link will automatically be set to nil since it is a weak
    reference. }
  FObject1 := nil;

  if Assigned(FObject2) then
    Assert(FObject2.Link = nil);

  Log('Released first object.');
  Log(' * TSample.InstanceCount = %d', [TSample.InstanceCount]);
  Log('');

  if (TSample.InstanceCount = 0) then
    ButtonCreateObjects.Enabled := True;
end;

procedure TFrameManagedTypes.ButtonReleaseSecondClick(Sender: TObject);
begin
  ButtonReleaseSecond.Enabled := False;

  { Release first object. If FObject1 still exists and has a link to FObject2,
    then this link will automatically be set to nil since it is a weak
    reference. }
  FObject2 := nil;

  if Assigned(FObject1) then
    Assert(FObject1.Link = nil);

  Log('Released second object.');
  Log(' * TSample.InstanceCount = %d', [TSample.InstanceCount]);
  Log('');

  if (TSample.InstanceCount = 0) then
    ButtonCreateObjects.Enabled := True;
end;

end.
