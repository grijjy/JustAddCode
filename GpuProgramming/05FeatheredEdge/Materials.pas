unit Materials;

interface

uses
  System.Classes,
  System.SysUtils,
  FMX.Types3D,
  FMX.Graphics,
  FMX.Materials,
  FMX.MaterialSources;

type
  TFeatheredEdgeMaterialSource = class(TMaterialSource)
  private
    FImage: TBitmap;
    procedure SetImage(const AValue: TBitmap);
    function GetFeather: Single;
    procedure SetFeather(const AValue: Single);
    procedure HandleImageChanged(Sender: TObject);
  protected
    function CreateMaterial: TMaterial; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Image: TBitmap read FImage write SetImage;
    property Feather: Single read GetFeather write SetFeather;
  end;

type
  TFeatheredEdgeMaterial = class(TCustomMaterial)
  private class var
    FShaderArch: TContextShaderArch;
    FVertexShaderData: TBytes;
    FPixelShaderData: TBytes;
    FMatrixIndex: Integer;
    FMatrixSize: Integer;
    FFloatSize: Integer;
  private
    FTexture: TTexture; // Reference
    FFeather: Single;
    procedure SetTexture(const AValue: TTexture);
    procedure SetFeather(const AValue: Single);
  private
    class procedure LoadShaders; static;
    class function LoadShader(const AResourceName: String): TBytes; static;
  protected
    procedure DoInitialize; override;
    procedure DoApply(const Context: TContext3D); override;
  public
    property Texture: TTexture read FTexture write SetTexture;
    property Feather: Single read FFeather write SetFeather;
  end;

implementation

uses
  System.Types,
  System.Math.Vectors,
  {$IF Defined(MSWINDOWS)}
  FMX.Context.DX9,
  FMX.Context.DX11;
  {$ELSEIF Defined(ANDROID)}
  FMX.Context.GLES;
  {$ELSEIF Defined(IOS)}
  FMX.Context.GLES,
  FMX.Context.Metal;
  {$ELSEIF Defined(MACOS)}
  FMX.Context.Mac,
  FMX.Context.Metal;
  {$ELSE}
    {$MESSAGE Error 'Unsupported platform'}
  {$ENDIF}

{$R 'Shaders\Shaders.res'}

{ TFeatheredEdgeMaterialSource }

constructor TFeatheredEdgeMaterialSource.Create(AOwner: TComponent);
begin
  inherited;
  FImage := TTextureBitmap.Create;
  FImage.OnChange := HandleImageChanged;
end;

function TFeatheredEdgeMaterialSource.CreateMaterial: TMaterial;
begin
  Result := TFeatheredEdgeMaterial.Create;
end;

destructor TFeatheredEdgeMaterialSource.Destroy;
begin
  FImage.Free;
  inherited;
end;

function TFeatheredEdgeMaterialSource.GetFeather: Single;
begin
  Result := TFeatheredEdgeMaterial(Material).Feather;
end;

procedure TFeatheredEdgeMaterialSource.HandleImageChanged(Sender: TObject);
begin
  if (not FImage.IsEmpty) then
    TFeatheredEdgeMaterial(Material).Texture := TTextureBitmap(FImage).Texture;
end;

procedure TFeatheredEdgeMaterialSource.SetFeather(const AValue: Single);
begin
  TFeatheredEdgeMaterial(Material).Feather := AValue;
end;

procedure TFeatheredEdgeMaterialSource.SetImage(const AValue: TBitmap);
begin
  FImage.Assign(AValue);
end;

{ TFeatheredEdgeMaterial }

procedure TFeatheredEdgeMaterial.DoApply(const Context: TContext3D);
begin
  inherited;
  Context.SetShaderVariable('Texture', FTexture);
  Context.SetShaderVariable('Feather', [Vector3D(FFeather, 0, 0, 0)]);
end;

procedure TFeatheredEdgeMaterial.DoInitialize;
begin
  inherited;
  if (FShaderArch = TContextShaderArch.Undefined) then
    LoadShaders;

  FVertexShader := TShaderManager.RegisterShaderFromData('FeatheredEdge.fvs',
    TContextShaderKind.VertexShader, '', [
    TContextShaderSource.Create(FShaderArch, FVertexShaderData,
    [TContextShaderVariable.Create('MVPMatrix', TContextShaderVariableKind.Matrix,
      FMatrixIndex, FMatrixSize)])
  ]);

  FPixelShader := TShaderManager.RegisterShaderFromData('FeatheredEdge.fps',
    TContextShaderKind.PixelShader, '', [
    TContextShaderSource.Create(FShaderArch, FPixelShaderData,
    [TContextShaderVariable.Create('Texture', TContextShaderVariableKind.Texture, 0, 0),
     TContextShaderVariable.Create('Feather', TContextShaderVariableKind.Float, 0, FFloatSize)])
  ]);
end;

class function TFeatheredEdgeMaterial.LoadShader(const AResourceName: String): TBytes;
begin
  var Stream := TResourceStream.Create(HInstance, AResourceName, RT_RCDATA);
  try
    SetLength(Result, Stream.Size);
    Stream.ReadBuffer(Result, Length(Result));
  finally
    Stream.Free;
  end;
end;

class procedure TFeatheredEdgeMaterial.LoadShaders;
begin
  var Suffix := '';
  var ContextClass := TContextManager.DefaultContextClass;

  {$IF Defined(MSWINDOWS)}
  if (ContextClass.InheritsFrom(TCustomDX9Context)) then
  begin
    FShaderArch := TContextShaderArch.DX9;
    FMatrixIndex := 0;
    FMatrixSize := 4;
    FFloatSize := 1;
    Suffix := 'DX9';
  end
  else if (ContextClass.InheritsFrom(TCustomDX11Context)) then
  begin
    FShaderArch := TContextShaderArch.DX11;
    FMatrixIndex := 0;
    FMatrixSize := 64;
    FFloatSize := 4;
    Suffix := 'DX11';
  end;
  {$ELSE}
  if (ContextClass.InheritsFrom(TCustomContextOpenGL)) then
  begin
    FShaderArch := TContextShaderArch.GLSL;
    FMatrixIndex := 0;
    FMatrixSize := 4;
    FFloatSize := 1;
    Suffix := 'GL';
  end;
  {$ENDIF}

  {$IF Defined(MACOS)}
  if (ContextClass.InheritsFrom(TCustomContextMetal)) then
  begin
    FShaderArch := TContextShaderArch.Metal;
    FMatrixIndex := 1;
    FMatrixSize := 4;
    FFloatSize := 1;
    Suffix := 'MTL';
  end;
  {$ENDIF}

  if (FShaderArch = TContextShaderArch.Undefined) then
    raise EContext3DException.Create('Unknown or unsupported 3D context class');

  FVertexShaderData := LoadShader('VERTEX_SHADER_' + Suffix);
  FPixelShaderData := LoadShader('PIXEL_SHADER_' + Suffix);
end;

procedure TFeatheredEdgeMaterial.SetFeather(const AValue: Single);
begin
  if (AValue <> FFeather) then
  begin
    FFeather := AValue;
    DoChange;
  end;
end;

procedure TFeatheredEdgeMaterial.SetTexture(const AValue: TTexture);
begin
  FTexture := AValue;
  DoChange;
end;

end.
