unit FMain;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Math,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.Objects,
  FMX.StdCtrls,
  FMX.ExtCtrls,
  FMX.Controls.Presentation,
  Grijjy.FaceDetection;

type
  TFormMain = class(TForm)
    ToolBar: TToolBar;
    LabelImage: TLabel;
    PopupBoxImage: TPopupBox;
    PaintBox: TPaintBox;
    procedure PopupBoxImageChange(Sender: TObject);
    procedure PaintBoxPaint(Sender: TObject; Canvas: TCanvas);
  private
    { Private declarations }
    FFaceDetector: IgoFaceDetector;
    FFaces: TArray<TgoFace>;
    FBitmap: TBitmap;
  private
    procedure ProcessImage(const AResourceName: String);
  public
    { Public declarations }
    destructor Destroy; override;
  end;

var
  FormMain: TFormMain;

implementation

{$R *.fmx}

{ TFormMain }

destructor TFormMain.Destroy;
begin
  FBitmap.Free;
  inherited;
end;

procedure TFormMain.PaintBoxPaint(Sender: TObject; Canvas: TCanvas);
var
  SrcRect, DstRect, Rect: TRectF;
  Scale, DstWidth, DstHeight: Single;
  Face: TgoFace;

  procedure PaintEye(const APosition: TPointF; const AColor: TAlphaColor);
  var
    P: TPointF;
    R: TRectF;
    Radius: Single;
  begin
    { Exit if eye position is not available }
    if (APosition.X = 0) and (APosition.Y = 0) then
      Exit;

    P.X := APosition.X * Scale;
    P.Y := APosition.Y * Scale;
    P.Offset(DstRect.TopLeft);

    Radius := Face.EyesDistance * Scale * 0.2;
    R := RectF(P.X - Radius, P.Y - Radius, P.X + Radius, P.Y + Radius);
    Canvas.Stroke.Color := AColor;
    Canvas.DrawEllipse(R, 1);
  end;

begin
  { Paint bitmap to fit the paint box while preserving aspect ratio. }
  SrcRect := RectF(0, 0, FBitmap.Width, FBitmap.Height);
  Scale := Min(PaintBox.Width / FBitmap.Width, PaintBox.Height / FBitmap.Height);
  DstWidth := Round(FBitmap.Width * Scale);
  DstHeight := Round(FBitmap.Height * Scale);
  DstRect.Left := Floor(0.5 * (PaintBox.Width - DstWidth));
  DstRect.Top := Floor(0.5 * (PaintBox.Height - DstHeight));
  DstRect.Width := DstWidth;
  DstRect.Height := DstHeight;
  Canvas.DrawBitmap(FBitmap, SrcRect, DstRect, 1);

  Canvas.Stroke.Kind := TBrushKind.Solid;
  Canvas.Stroke.Thickness := 2;

  { Paint each face with its features }
  for Face in FFaces do
  begin
    { Paint bounds around entire face }
    Rect := Face.Bounds;
    Rect.Left := Floor(Rect.Left * Scale);
    Rect.Top := Floor(Rect.Top * Scale);
    Rect.Right := Ceil(Rect.Right * Scale);
    Rect.Bottom := Ceil(Rect.Bottom * Scale);
    Rect.Offset(DstRect.TopLeft);

    Canvas.Stroke.Color := TAlphaColors.Lime;
    Canvas.DrawRect(Rect, 4, 4, AllCorners, 1);

    { Paint the eyes }
    PaintEye(Face.LeftEyePosition, TAlphaColors.Magenta);
    PaintEye(Face.RightEyePosition, TAlphaColors.Cyan);
  end;
end;

procedure TFormMain.PopupBoxImageChange(Sender: TObject);
begin
  ProcessImage(PopupBoxImage.Text);
end;

procedure TFormMain.ProcessImage(const AResourceName: String);
var
  Stream: TResourceStream;
begin
  { Create bitmap and face detector if needed. }
  if (FBitmap = nil) then
    FBitmap := TBitmap.Create;

  if (FFaceDetector = nil) then
    { Create a face detector using high accuracy and a maximum detection of
      10 faces. }
    FFaceDetector := TgoFaceDetector.Create(TgoFaceDetectionAccuracy.High, 10);

  { Load test bitmap from resource }
  Stream := TResourceStream.Create(HInstance, AResourceName, RT_RCDATA);
  try
    FBitmap.LoadFromStream(Stream);
  finally
    Stream.Free;
  end;

  { Detect faces in bitmap }
  FFaces := FFaceDetector.DetectFaces(FBitmap);

  { Show bitmap and detected features }
  PaintBox.Repaint;
end;

end.
