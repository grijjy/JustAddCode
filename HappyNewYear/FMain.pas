unit FMain;

interface

uses
  System.SysUtils,
  System.Math,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Math.Vectors,
  System.Diagnostics,
  System.RTLConsts,
  FMX.Types,
  FMX.Controls,
  FMX.Forms3D,
  FMX.Types3D,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.Controls.Presentation,
  FMX.StdCtrls,
  FMX.Controls3D,
  FMX.Layers3D,
  FMX.Objects3D,
  FMX.Ani,
  Materials, FMX.Objects, FMX.Layouts, FMX.MaterialSources;

type
  TFormMain = class(TForm3D)
    MeshFireworks: TMesh;
    Layer3D: TLayer3D;
    LayoutFireworks: TLayout;
    TextFireworks: TText;
    TrackBarFireworks: TTrackBar;
    LayoutSparks: TLayout;
    TextSparks: TText;
    TrackBarSparks: TTrackBar;
    PlaneHappyNewYear: TPlane;
    MaterialSourceHappyNewYear: TTextureMaterialSource;
    FloatAnimationRotationX: TFloatAnimation;
    FloatAnimationRotationY: TFloatAnimation;
    DummyHappyNewYear: TDummy;
    procedure Form3DCreate(Sender: TObject);
    procedure Form3DResize(Sender: TObject);
    procedure TrackBarFireworksChange(Sender: TObject);
    procedure TrackBarSparksChange(Sender: TObject);
    procedure FloatAnimationRotationXProcess(Sender: TObject);
  private
    { Private declarations }
    FMaterialSourceFireworks: TFireworksMaterialSource;
    FStopwatch: TStopwatch;
    procedure UpdateTextureCoordinates;
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.fmx}

procedure TFormMain.FloatAnimationRotationXProcess(Sender: TObject);
begin
  if (FMaterialSourceFireworks <> nil) then
    FMaterialSourceFireworks.TimeInSeconds := FStopwatch.Elapsed.TotalSeconds;
end;

procedure TFormMain.Form3DCreate(Sender: TObject);
begin
  { Build vertex buffer for plane }
  var VB := MeshFireworks.Data.VertexBuffer;
  VB.Length := 4;
  VB.Vertices[0] := Point3D(-0.5, -0.5, 0);
  VB.Vertices[1] := Point3D( 0.5, -0.5, 0);
  VB.Vertices[2] := Point3D(-0.5,  0.5, 0);
  VB.Vertices[3] := Point3D( 0.5,  0.5, 0);
  UpdateTextureCoordinates;

  { Build index buffer for plane }
  var IB := MeshFireworks.Data.IndexBuffer;
  IB.Length := 6;
  IB[0] := 0;
  IB[1] := 1;
  IB[2] := 2;
  IB[3] := 2;
  IB[4] := 1;
  IB[5] := 3;

  FMaterialSourceFireworks := TFireworksMaterialSource.Create(Self);
  MeshFireworks.MaterialSource := FMaterialSourceFireworks;

  TrackBarFireworks.Value := FMaterialSourceFireworks.FireworksCount;
  TrackBarSparks.Value := FMaterialSourceFireworks.SparkCount;

  FStopwatch := TStopwatch.StartNew;
end;

procedure TFormMain.Form3DResize(Sender: TObject);
begin
  UpdateTextureCoordinates;

 MeshFireworks.Position.Point := Point3D(ClientWidth div 2, ClientHeight div 2, 0);
  MeshFireworks.SetSize(ClientWidth, ClientHeight, 0.001);

  var W := 0.5 * ClientWidth;
  var Texture := MaterialSourceHappyNewYear.Texture;
  DummyHappyNewYear.Position.Point := Point3D(ClientWidth div 2, (ClientHeight + Layer3D.Height) * 0.5, -100);
  PlaneHappyNewYear.SetSize(W, W * (Texture.Height / Texture.Width), 0.001);

  { Workaround for Thumb not rendering sometimes. }
  TrackBarFireworks.RecalcSize;
  TrackBarSparks.RecalcSize;
end;

procedure TFormMain.TrackBarFireworksChange(Sender: TObject);
begin
  var NewCount := Trunc(TrackBarFireworks.Value);
  FMaterialSourceFireworks.FireworksCount := NewCount;
  TextFireworks.Text := Format('#Fireworks: %d', [NewCount]);
end;

procedure TFormMain.TrackBarSparksChange(Sender: TObject);
begin
  var NewCount := Trunc(TrackBarSparks.Value);
  FMaterialSourceFireworks.SparkCount := NewCount;
  TextSparks.Text := Format('#Sparks: %d', [NewCount]);
end;

procedure TFormMain.UpdateTextureCoordinates;
begin
  var U: Single := ClientWidth / ClientHeight;

  var VB := MeshFireworks.Data.VertexBuffer;
  VB.TexCoord0[0] := PointF(0, 1);
  VB.TexCoord0[1] := PointF(U, 1);
  VB.TexCoord0[2] := PointF(0, 0);
  VB.TexCoord0[3] := PointF(U, 0);
end;

end.
