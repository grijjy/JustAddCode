unit Grijjy.FaceDetection.Android;
{< Face Detection for Android. }

interface

uses
  FMX.Graphics,
  Androidapi.JNI.Media,
  Grijjy.FaceDetection;

type
  { IgoFaceDetector implementation }
  TgoFaceDetectorImplementation = class(TInterfacedObject, IgoFaceDetector)
  private
    FDetector: JFaceDetector;
    FMaxFaces: Integer;
    FWidth: Integer;
    FHeight: Integer;
  protected
    { IgoFaceDetector }
    function DetectFaces(const ABitmap: TBitmap): TArray<TgoFace>;
  public
    constructor Create(const AAccuracy: TgoFaceDetectionAccuracy;
      const AMaxFaces: Integer);
  end;

implementation

uses
  System.Types,
  Androidapi.Bitmap,
  Androidapi.JNIBridge,
  Androidapi.JNI,
  Androidapi.JNI.GraphicsContentViewText;

{ TgoFaceDetectorImplementation }

constructor TgoFaceDetectorImplementation.Create(
  const AAccuracy: TgoFaceDetectionAccuracy; const AMaxFaces: Integer);
begin
  inherited Create;
  FMaxFaces := AMaxFaces;
  { Accuracy is not supported on Android. }
end;

function TgoFaceDetectorImplementation.DetectFaces(
  const ABitmap: TBitmap): TArray<TgoFace>;
var
  Width, Height, X, Y, R, G, B, I, Count: Integer;
  SrcBitmap: TBitmapData;
  Bitmap: JBitmap;
  BitmapId: JNIObject;
  Src: PCardinal;
  Dst: PWord;
  OddWidth: Boolean;
  C: Cardinal;
  Faces: TJavaObjectArray<JFaceDetector_Face>;
  SrcFace: JFaceDetector_Face;
  DstFace: TgoFace;
  Point: JPointF;
  P: TPointF;
  Distance: Single;
begin
  { Android's FaceDetector class requires Width to be even }
  Width := ABitmap.Width;
  OddWidth := Odd(Width);
  if (OddWidth) then
    Dec(Width);
  Height := ABitmap.Height;

  { Use previously cache FaceDetector class if available and dimensions
    haven't changed. }
  if (FDetector = nil) or (Width <> FWidth) or (Height <> FHeight) then
  begin
    FDetector := nil;
    FWidth := Width;
    FHeight := Height;
    FDetector := TJFaceDetector.JavaClass.init(Width, Height, FMaxFaces);
  end;

  { The FaceDetector class works with the Android Bitmap class.
    FaceDetector requires that the bitmap is in 565 format }
  Bitmap := TJBitmap.JavaClass.createBitmap(Width, Height,
    TJBitmap_Config.JavaClass.RGB_565);
  BitmapId := (Bitmap as ILocalObject).GetObjectID;

  { Use NDK AndroidBitmap APIs for fast access to native Android bitmaps. }
  if (AndroidBitmap_lockPixels(TJNIResolver.GetJNIEnv, BitmapId, @Dst) <> 0) then
    Exit(nil);
  try
    { Copy the FireMonkey bitmap to the native Android bitmap, converting to
      RGB565 format in the process. }
    if (not ABitmap.Map(TMapAccess.Read, SrcBitmap)) then
      Exit(nil);
    try
      Src := SrcBitmap.Data;
      for Y := 0 to Height - 1 do
      begin
        for X := 0 to Width - 1 do
        begin
          C := Src^;
          R := (C shr (16 + 3)) and $1F; // 5 bits
          G := (C shr ( 8 + 2)) and $3F; // 6 bits
          B := (C shr ( 0 + 3)) and $1F; // 5 bits
          Dst^ := (R shl 11) or (G shl 5) or B;
          Inc(Src);
          Inc(Dst);
        end;
        if OddWidth then
          Inc(Src);
      end;
    finally
      ABitmap.Unmap(SrcBitmap);
    end;
  finally
    AndroidBitmap_unlockPixels(TJNIResolver.GetJNIEnv, BitmapId);
  end;

  Assert(Assigned(FDetector));
  { Create a Java array of JFaceDetector_Face objects. }
  Faces := TJavaObjectArray<JFaceDetector_Face>.Create(FMaxFaces);
  { Pass this array to the SrcFace detector to find the faces. }
  Count := FDetector.findFaces(Bitmap, Faces);
  if (Count = 0) then
    Exit(nil);

  { Convert the JFaceDetector_Face objects to TgoFace records. }
  SetLength(Result, Count);
  Point := TJPointF.Create;
  for I := 0 to Count - 1 do
  begin
    { Get Java SrcFace from array }
    SrcFace := TJFaceDetector_Face.Wrap(Faces.GetRawItem(I));
    SrcFace.getMidPoint(Point);
    P.X := Point.x;
    P.Y := Point.y;
    Distance := SrcFace.eyesDistance;

    { Calculate the position of the eyes based on the mid point of the SrcFace
      and the distance between the eyes.
      NOTE: We should use SrcFace.pose to rotate the position of the eyes around
      the midpoint. However, on most Android devices, Pose always returns 0,
      so there is not much point in using it. }
    DstFace.LeftEyePosition := PointF(P.X - 0.5 * Distance, P.Y);
    DstFace.RightEyePosition := PointF(P.X + 0.5 * Distance, P.Y);
    DstFace.EyesDistance := Distance;

    { Android does not return the bounds of the SrcFace. Instead, we set it
      ourselves based on the eye positions. We set it in such a way to match
      the way iOS does it. }
    Distance := Distance * 1.35;
    DstFace.Bounds := RectF(P.X - Distance, P.Y - 0.7 * Distance,
                            P.X + Distance, P.Y + 1.3 * Distance);
    Result[I] := DstFace;
  end;
end;

end.
