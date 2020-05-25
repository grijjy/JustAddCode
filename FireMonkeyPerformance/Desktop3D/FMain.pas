unit FMain;
{ Measurements on 2019 13" macBook Pro:

                                 FPS     CPU%   GPU%
  --------------------------------------------------
  Default, without 2D control     30       96     33
  Default, with 2D control        30       94     33
  Metal, without 2D control       30       88     25
  Metal, with 2D control          30       89     24 }

interface

uses
  System.Math,
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Math.Vectors,
  System.Diagnostics,
  System.Generics.Collections,
  FMX.Types,
  FMX.Controls,
  FMX.Forms3D,
  FMX.Types3D,
  FMX.Objects3D,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.Controls3D,
  FMX.TextLayout,
  FMX.Layers3D,
  FMX.MaterialSources,
  FMX.Controls.Presentation,
  FMX.StdCtrls,
  FMX.ListBox,
  FMX.Edit;

const
  SPHERE_SIZE  = 4;
  SUBDIVISIONS = 50; { High number of subdivisions to stress test }

type
  TFormMain = class(TForm3D)
    Layer3DTop: TLayer3D;
    MaterialDay: TLightMaterialSource;
    MaterialNight: TLightMaterialSource;
    Light: TLight;
    Camera: TCamera;
    TimerAnimate: TTimer;
    PanelTop: TPanel;
    LabelFPS: TLabel;
    CheckBoxShow2DControl: TCheckBox;
    LabelInfo: TLabel;
    Layer3D: TLayer3D;
    Text3: TLabel;
    TextBox1: TEdit;
    CheckBox1: TCheckBox;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    AniIndicator1: TAniIndicator;
    Label1: TLabel;
    StringComboBox1: TComboBox;
    ListBoxItem51: TListBoxItem;
    ListBoxItem52: TListBoxItem;
    ListBoxItem53: TListBoxItem;
    ListBoxItem54: TListBoxItem;
    ListBoxItem55: TListBoxItem;
    ListBoxItem56: TListBoxItem;
    ListBoxItem57: TListBoxItem;
    ListBoxItem58: TListBoxItem;
    ListBoxItem59: TListBoxItem;
    ListBoxItem60: TListBoxItem;
    ListBoxItem61: TListBoxItem;
    ListBoxItem62: TListBoxItem;
    ListBoxItem63: TListBoxItem;
    ListBoxItem64: TListBoxItem;
    ListBoxItem65: TListBoxItem;
    ListBoxItem66: TListBoxItem;
    ListBoxItem67: TListBoxItem;
    ListBoxItem68: TListBoxItem;
    ListBoxItem69: TListBoxItem;
    ListBoxItem70: TListBoxItem;
    ListBoxItem71: TListBoxItem;
    ListBoxItem72: TListBoxItem;
    TrackBar6: TTrackBar;
    ScrollBar1: TScrollBar;
    procedure Form3DCreate(Sender: TObject);
    procedure TimerAnimateTimer(Sender: TObject);
    procedure Form3DDestroy(Sender: TObject);
    procedure Form3DRender(Sender: TObject; Context: TContext3D);
    procedure CheckBoxShow2DControlChange(Sender: TObject);
  private
    { Private declarations }
    FTicksPerSecond: Int64;
    FNextSecond: Int64;
    FPrevTicks: Int64;
    FFrameCount: Integer;
    FSpheres: TList<TSphere>;
    procedure AddSphere(const ARow, ACol, ADepth: Integer);
    procedure ApplicationIdle(Sender: TObject; var Done: Boolean);
    procedure Animate;
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.fmx}

procedure TFormMain.AddSphere(const ARow, ACol, ADepth: Integer);
begin
  var Sphere := TSphere.Create(Self);
  Sphere.SubdivisionsAxes := SUBDIVISIONS;
  Sphere.SubdivisionsHeight := SUBDIVISIONS;
  Sphere.SetSize(SPHERE_SIZE, SPHERE_SIZE, SPHERE_SIZE);

  Sphere.Position.X := ACol * (SPHERE_SIZE * 1.1);
  Sphere.Position.Y := ARow * (SPHERE_SIZE * 1.1);
  Sphere.Position.Z := ADepth * (SPHERE_SIZE * 1.1);

  if Odd(ARow + ACol) then
    Sphere.MaterialSource := MaterialNight
  else
    Sphere.MaterialSource := MaterialDay;

  AddObject(Sphere);
  FSpheres.Add(Sphere);
end;

procedure TFormMain.Animate;
begin
  var Ticks := TStopwatch.GetTimeStamp;

  if (FPrevTicks <> 0) then
  begin
    var Change := (Ticks - FPrevTicks) / FTicksPerSecond;
    var Angle := FSpheres[0].RotationAngle.Y;
    Angle := Angle + (20 * Change);
    Angle := FMod(Angle, 360); // Wrap around at 360.0
    for var I := 0 to FSpheres.Count - 1 do
    begin
      if (Odd(I)) then
        FSpheres[I].RotationAngle.Y := -Angle
      else
        FSpheres[I].RotationAngle.Y := Angle;
    end;
  end;

  FPrevTicks := Ticks;

  if (Ticks >= FNextSecond) then
  begin
    LabelFPS.Text := Format('%d fps', [FFrameCount]);
    LabelFPS.Repaint;
    FFrameCount := 0;
    Inc(FNextSecond, FTicksPerSecond);
    if (Ticks >= FNextSecond) then
      { Catch up }
      FNextSecond := Ticks + FTicksPerSecond;
  end;
end;

procedure TFormMain.ApplicationIdle(Sender: TObject; var Done: Boolean);
begin
  { To try and repaint as fast as possible, we use both a timer and the OnIdle
    event. }
  Animate;
end;

procedure TFormMain.CheckBoxShow2DControlChange(Sender: TObject);
begin
  Layer3D.Visible := CheckBoxShow2DControl.IsChecked;
end;

procedure TFormMain.Form3DCreate(Sender: TObject);
begin
  FSpheres := TList<TSphere>.Create;

  Application.OnIdle := ApplicationIdle;
  TStopwatch.StartNew; // Init high-res timer
  FTicksPerSecond := TStopwatch.Frequency;
  FNextSecond := TStopwatch.GetTimeStamp + FTicksPerSecond;

  LabelInfo.Text := Format('Canvas class: %s, Text layout class: %s, 3D Context class: %s', [
    PanelTop.Canvas.ClassName, TTextLayoutManager.DefaultTextLayout.ClassName,
    Context.ClassName]);

  for var Depth := 0 to 4 do
    for var Row := -2 to 2 do
      for var Col := -4 to 4 do
        AddSphere(Row, Col, Depth);
end;

procedure TFormMain.Form3DDestroy(Sender: TObject);
begin
  FSpheres.Free;
end;

procedure TFormMain.Form3DRender(Sender: TObject; Context: TContext3D);
begin
  Inc(FFrameCount);
end;

procedure TFormMain.TimerAnimateTimer(Sender: TObject);
begin
  Animate;
end;

end.
