unit Grijjy.FaceDetection.Null;
{< Do-nothing Face Detector for platforms that do no have native suppor fo
   face detection. }

interface

uses
  FMX.Graphics,
  Grijjy.FaceDetection;

type
  { IgoFaceDetector implementation }
  TgoFaceDetectorImplementation = class(TInterfacedObject, IgoFaceDetector)
  protected
    { IgoFaceDetector }
    function DetectFaces(const ABitmap: TBitmap): TArray<TgoFace>;
  public
    constructor Create(const AAccuracy: TgoFaceDetectionAccuracy;
      const AMaxFaces: Integer);
  end;

implementation

{ TgoFaceDetectorImplementation }

constructor TgoFaceDetectorImplementation.Create(
  const AAccuracy: TgoFaceDetectionAccuracy; const AMaxFaces: Integer);
begin
  inherited Create;
  { Ignore arguments }
end;

function TgoFaceDetectorImplementation.DetectFaces(
  const ABitmap: TBitmap): TArray<TgoFace>;
begin
  { Just return an empty array. }
  Result := nil;
end;

end.
