unit FMain;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Math.Vectors,
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
  Materials;

type
  TFormMain = class(TForm3D)
    Layer3D: TLayer3D;
    LabelBackend: TLabel;
    Plane: TPlane;
    FloatAnimationRotationY: TFloatAnimation;
    procedure Form3DCreate(Sender: TObject);
    procedure Form3DRender(Sender: TObject; Context: TContext3D);
  private
    { Private declarations }
    FMaterialSource: TImageMaterialSource;
    class function LoadImage(const AResource: String): TBitmap; static;
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.fmx}
{$R '..\Textures\Textures.res'}

procedure TFormMain.Form3DCreate(Sender: TObject);
begin
  LabelBackend.Text := 'Backend: ' + Context.ClassName;

  FMaterialSource := TImageMaterialSource.Create(Self);
  var Image := LoadImage('LENA');
  try
    FMaterialSource.Image := Image;
  finally
    Image.Free;
  end;

  Plane.MaterialSource := FMaterialSource;
end;

procedure TFormMain.Form3DRender(Sender: TObject; Context: TContext3D);
begin
  {$IF Defined(MACOS) and not Defined(IOS)}
  { Workaround for https://quality.embarcadero.com/browse/RSP-32121 }
  if (GlobalUseMetal) then
    Context.FillRect(Point3D(-Width, -Height, 100), Point3D(Width, Height, 100), 1, Color);
  {$ENDIF}
end;

class function TFormMain.LoadImage(const AResource: String): TBitmap;
begin
  Result := TBitmap.Create;
  var Stream := TResourceStream.Create(HInstance, AResource, RT_RCDATA);
  try
    Result.LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

end.
