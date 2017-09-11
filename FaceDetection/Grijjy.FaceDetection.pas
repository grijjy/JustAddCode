unit Grijjy.FaceDetection;
{< Face Detection for iOS and Android.
   Compiles on other platforms as well, but will not detect faces on those
   platforms. }

interface

uses
  FMX.Graphics,
  System.Types;

type
  { The features of a face as detected by IgoFaceDetector }
  TgoFace = record
    { Bounds of the face in the image. }
    Bounds: TRectF;

    { Position of the left eye in the image. Is (0, 0) if not available. }
    LeftEyePosition: TPointF;

    { Position of the right eye in the image. Is (0, 0) if not available. }
    RightEyePosition: TPointF;

    { Distance between the eyes. Is 0 if not available. }
    EyesDistance: Single;
  end;

type
  { Face detection accuracy }
  TgoFaceDetectionAccuracy = (
    { Lower accuracy at higher speed }
    Low,

    { Higher accuracy at lower speed }
    High);

type
  { Face detector interface. To create an instance, use TgoFaceDetector.Create.
    Currently works on iOS and Android.
    You can also use this interface on other platforms, but the DetectFaces
    function will always return an empty array in those cases. }
  IgoFaceDetector = interface
    { Detects the faces in a bitmap.

      Parameters:
        ABitmap: the bitmap the check for faces. Cannot be nil.

      Returns:
        An array of TgoFaceFeature records with the features of each detected
        face. Returns an empty array if no faces are found in the bitmap. }
    function DetectFaces(const ABitmap: TBitmap): TArray<TgoFace>;
  end;

type
  { Class factory for IgoFaceDetector. }
  TgoFaceDetector = class // static
  public
    { Creates a face detector.

      Parameters:
        AAccuracy: (optional) the detection accuracy. Low accuracy is fast.
          High accuracy is thorough but slower. This parameter is currently only
          used on iOS. Defaults to High.
        AMaxFaces: (optional) the maximum number of faces that will be detected
          in each image. Defaults to 5. }
    class function Create(
      const AAccuracy: TgoFaceDetectionAccuracy = TgoFaceDetectionAccuracy.High;
      const AMaxFaces: Integer = 5): IgoFaceDetector; static;
  end;

implementation

uses
  {$IF Defined(IOS)}
  Grijjy.FaceDetection.iOS;
  {$ELSEIF Defined(ANDROID)}
  Grijjy.FaceDetection.Android;
  {$ELSE}
  Grijjy.FaceDetection.Null;
  {$ENDIF}

{ TgoFaceDetector }

class function TgoFaceDetector.Create(const AAccuracy: TgoFaceDetectionAccuracy;
  const AMaxFaces: Integer): IgoFaceDetector;
begin
  Result := TgoFaceDetectorImplementation.Create(AAccuracy, AMaxFaces);
end;

end.
