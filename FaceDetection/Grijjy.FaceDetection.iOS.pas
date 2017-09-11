unit Grijjy.FaceDetection.iOS;
{< Face Detection for iOS. }

interface

uses
  iOSapi.CoreImage,
  FMX.Graphics,
  Grijjy.FaceDetection;

type
  { IgoFaceDetector implementation }
  TgoFaceDetectorImplementation = class(TInterfacedObject, IgoFaceDetector)
  private
    FMaxFaces: Integer;
    FContext: CIContext;
    FDetector: CIDetector;
  protected
    { IgoFaceDetector }
    function DetectFaces(const ABitmap: TBitmap): TArray<TgoFace>;
  public
    constructor Create(const AAccuracy: TgoFaceDetectionAccuracy;
      const AMaxFaces: Integer);
    destructor Destroy; override;
  end;

implementation

uses
  System.Types,
  System.Math,
  iOSapi.Foundation;

{ These APIs are missing from iOSapi.CoreImage: }

function CIDetectorAccuracyLow: Pointer;
begin
  Result := Pointer(CocoaPointerConst(libCoreImage, 'CIDetectorAccuracyLow')^);
end;

function CIDetectorAccuracyHigh: Pointer;
begin
  Result := Pointer(CocoaPointerConst(libCoreImage, 'CIDetectorAccuracyLow')^);
end;

function CIDetectorAccuracy: Pointer;
begin
  Result := Pointer(CocoaPointerConst(libCoreImage, 'CIDetectorAccuracy')^);
end;

function CIDetectorTypeFace: NSString;
begin
  Result := CocoaNSStringConst(libCoreImage, 'CIDetectorTypeFace');
end;

function kCIFormatBGRA8: CIFormat;
begin
  Result := PInteger(CocoaPointerConst(libCoreImage, 'kCIFormatBGRA8'))^;
end;

{ TgoFaceDetectorImplementation }

constructor TgoFaceDetectorImplementation.Create(
  const AAccuracy: TgoFaceDetectionAccuracy; const AMaxFaces: Integer);
var
  Options: NSDictionary;
  Value: Pointer;
begin
  inherited Create;
  FMaxFaces := AMaxFaces;

  { Create a CoreImage context. This is required by the CIFaceDetector class. }
  FContext := TCIContext.Wrap(TCIContext.OCClass.contextWithOptions(nil));
  { We need to make sure the context stays alive for the duration of this
    object. So retain it. (We will release it in the destructor). }
  FContext.retain;

  { Create an options dictionary with a single option:
    the face detection accuracy. }
  if (AAccuracy = TgoFaceDetectionAccuracy.Low) then
    Value := CIDetectorAccuracyLow
  else
    Value := CIDetectorAccuracyHigh;
  Options := TNSDictionary.Wrap(TNSDictionary.OCClass.dictionaryWithObject(
    Value, CIDetectorAccuracy));

  { Create a face detector using the context and options we just created. }
  FDetector := TCIDetector.Wrap(TCIDetector.OCClass.detectorOfType(
    CIDetectorTypeFace, FContext, Options));

  { Like the context, we need to keep the detector alive. }
  FDetector.retain;
end;

destructor TgoFaceDetectorImplementation.Destroy;
begin
  { We retained the detector and context in the constructor, so we need to
    release it here. }
  if Assigned(FDetector) then
    FDetector.release;
  if Assigned(FContext) then
    FContext.release;
  inherited;
end;

function TgoFaceDetectorImplementation.DetectFaces(
  const ABitmap: TBitmap): TArray<TgoFace>;
var
  SrcData: TBitmapData;
  Data: NSData;
  Size: NSSize;
  Image: CIImage;
  Format: CIFormat;
  Features: NSArray;
  I, Count: Integer;
  SrcFeature: CIFaceFeature;
  DstFace: TgoFace;
  R: NSRect;
  P: NSPoint;
begin
  Assert(Assigned(ABitmap));

  { Create a CIImage with the contents of our FireMonkey bitmap.
    First, we need to create a NSData object with the raw bitmap data of the
    FireMonkey bitmap. }
  if (not ABitmap.Map(TMapAccess.Read, SrcData)) then
    Exit(nil);
  try
    Data := TNSData.Wrap(TNSData.OCClass.dataWithBytes(SrcData.Data,
      SrcData.Width * SrcData.Height * SrcData.BytesPerPixel));
  finally
    ABitmap.Unmap(SrcData);
  end;

  { Now we can create a CIImage using this data.
    We create it in BGRA8 format to match the format in the FireMonkey bitmap. }
  Size.width := ABitmap.Width;
  Size.height := ABitmap.Height;
  Format := kCIFormatBGRA8;
  Image := TCIImage.Wrap(TCIImage.OCClass.imageWithBitmapData(Data,
    ABitmap.Width * 4, Size, Format, nil));

  { Pass the image to the face detector. }
  Features := FDetector.featuresInImage(Image, nil);
  if (Features = nil) then
    Exit(nil);

  { Convert the CIFaceFeature objects to TgoFace records. }
  Count := Min(Features.count, FMaxFaces);
  SetLength(Result, Count);

  for I := 0 to Count - 1 do
  begin
    { Extract the face feature from the array. }
    SrcFeature := TCIFaceFeature.Wrap(Features.objectAtIndex(I));

    { Calculate the face bounds.
      NOTE: Images on iOS are upside-down }
    R := SrcFeature.bounds;
    DstFace.Bounds.Left := R.origin.x;
    DstFace.Bounds.Top := ABitmap.Height - R.origin.y - R.size.height;
    DstFace.Bounds.Width := R.size.width;
    DstFace.Bounds.Height := R.size.height;

    { Convert the left eye position, again taking into account the image
      is upside-down. }
    if (SrcFeature.hasLeftEyePosition) then
    begin
      P := SrcFeature.leftEyePosition;
      DstFace.LeftEyePosition := PointF(P.x, ABitmap.Height - P.y);
    end
    else
      DstFace.LeftEyePosition := PointF(0, 0);

    { Convert the right eye position. }
    if (SrcFeature.hasRightEyePosition) then
    begin
      P := SrcFeature.rightEyePosition;
      DstFace.RightEyePosition := PointF(P.x, ABitmap.Height - P.y);
    end
    else
      DstFace.RightEyePosition := PointF(0, 0);

    { Calculate the distance between the eyes manually. }
    DstFace.EyesDistance := DstFace.LeftEyePosition.Distance(DstFace.RightEyePosition);

    Result[I] := DstFace;
  end;
end;

end.
