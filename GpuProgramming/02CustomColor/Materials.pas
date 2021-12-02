unit Materials;

interface

uses
  System.UITypes,
  System.SysUtils,
  FMX.Types3D,
  FMX.Materials,
  FMX.MaterialSources;

type
  TCustomColorMaterialSource = class(TMaterialSource)
  private
    function GetColor: TAlphaColor;
    procedure SetColor(const AValue: TAlphaColor);
  protected
    function CreateMaterial: TMaterial; override;
  published
    property Color: TAlphaColor read GetColor write SetColor;
  end;

type
  TCustomColorMaterial = class(TCustomMaterial)
  private class var
    FShaderArch: TContextShaderArch;
    FVertexShaderData: TBytes;
    FPixelShaderData: TBytes;
    FMatrixIndex: Integer;
    FMatrixSize: Integer;
    FColorSize: Integer;
  private
    FColor: TAlphaColor;
    procedure SetColor(const AValue: TAlphaColor);
  private
    class procedure LoadShaders; static;
    class function LoadShader(const AResourceName: String): TBytes; static;
  protected
    procedure DoApply(const Context: TContext3D); override;
    procedure DoInitialize; override;
  public
    constructor Create; override;

    property Color: TAlphaColor read FColor write SetColor;
  end;

implementation

uses
  System.Types,
  System.Classes,
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

{ TCustomColorMaterialSource }

function TCustomColorMaterialSource.CreateMaterial: TMaterial;
begin
  Result := TCustomColorMaterial.Create;
end;

function TCustomColorMaterialSource.GetColor: TAlphaColor;
begin
  Result := TCustomColorMaterial(Material).Color;
end;

procedure TCustomColorMaterialSource.SetColor(const AValue: TAlphaColor);
begin
  TCustomColorMaterial(Material).Color := AValue;
end;

{ TCustomColorMaterial }

constructor TCustomColorMaterial.Create;
begin
  inherited;
  FColor := TAlphaColors.Blue;
end;

procedure TCustomColorMaterial.DoApply(const Context: TContext3D);
begin
  inherited;
  Context.SetShaderVariable('Color', FColor);
end;

procedure TCustomColorMaterial.DoInitialize;
begin
  inherited;
  if (FShaderArch = TContextShaderArch.Undefined) then
    LoadShaders;

  FVertexShader := TShaderManager.RegisterShaderFromData('CustomColor.fvs',
    TContextShaderKind.VertexShader, '', [
    TContextShaderSource.Create(FShaderArch, FVertexShaderData,
    [TContextShaderVariable.Create('MVPMatrix', TContextShaderVariableKind.Matrix,
      FMatrixIndex, FMatrixSize)])
  ]);

  FPixelShader := TShaderManager.RegisterShaderFromData('CustomColor.fps',
    TContextShaderKind.PixelShader, '', [
    TContextShaderSource.Create(FShaderArch, FPixelShaderData,
    [TContextShaderVariable.Create('Color', TContextShaderVariableKind.Vector, 0, FColorSize)])
  ]);
end;

class function TCustomColorMaterial.LoadShader(const AResourceName: String): TBytes;
begin
  var Stream := TResourceStream.Create(HInstance, AResourceName, RT_RCDATA);
  try
    SetLength(Result, Stream.Size);
    Stream.ReadBuffer(Result, Length(Result));
  finally
    Stream.Free;
  end;
end;

class procedure TCustomColorMaterial.LoadShaders;
begin
  var Suffix := '';
  var ContextClass := TContextManager.DefaultContextClass;

  {$IF Defined(MSWINDOWS)}
  if (ContextClass.InheritsFrom(TCustomDX9Context)) then
  begin
    FShaderArch := TContextShaderArch.DX9;
    FMatrixIndex := 0;
    FMatrixSize := 4;
    FColorSize := 1;
    Suffix := 'DX9';
  end
  else if (ContextClass.InheritsFrom(TCustomDX11Context)) then
  begin
    FShaderArch := TContextShaderArch.DX11;
    FMatrixIndex := 0;
    FMatrixSize := 64;
    FColorSize := 16;
    Suffix := 'DX11';
  end;
  {$ELSE}
  if (ContextClass.InheritsFrom(TCustomContextOpenGL)) then
  begin
    FShaderArch := TContextShaderArch.GLSL;
    FMatrixIndex := 0;
    FMatrixSize := 4;
    FColorSize := 1;
    Suffix := 'GL';
  end;
  {$ENDIF}

  {$IF Defined(MACOS)}
  if (ContextClass.InheritsFrom(TCustomContextMetal)) then
  begin
    FShaderArch := TContextShaderArch.Metal;
    FMatrixIndex := 1;
    FMatrixSize := 4;
    FColorSize := 1;
    Suffix := 'MTL';
  end;
  {$ENDIF}

  if (FShaderArch = TContextShaderArch.Undefined) then
    raise EContext3DException.Create('Unknown or unsupported 3D context class');

  FVertexShaderData := LoadShader('VERTEX_SHADER_' + Suffix);
  FPixelShaderData := LoadShader('PIXEL_SHADER_' + Suffix);
end;

procedure TCustomColorMaterial.SetColor(const AValue: TAlphaColor);
begin
  if (AValue <> FColor) then
  begin
    FColor := AValue;
    DoChange;
  end;
end;

end.
