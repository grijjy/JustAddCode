unit Materials;

interface

uses
  System.SysUtils,
  FMX.Types3D,
  FMX.Materials,
  FMX.MaterialSources;

type
  TBlueMaterialSource = class(TMaterialSource)
  protected
    function CreateMaterial: TMaterial; override;
  end;

type
  TBlueMaterial = class(TCustomMaterial)
  private class var
    FShaderArch: TContextShaderArch;
    FVertexShaderData: TBytes;
    FPixelShaderData: TBytes;
    FMatrixIndex: Integer;
    FMatrixSize: Integer;
  private
    class procedure LoadShaders; static;
    class function LoadShader(const AResourceName: String): TBytes; static;
  protected
    procedure DoInitialize; override;
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

{ TBlueMaterialSource }

function TBlueMaterialSource.CreateMaterial: TMaterial;
begin
  Result := TBlueMaterial.Create;
end;

{ TBlueMaterial }

procedure TBlueMaterial.DoInitialize;
begin
  inherited;
  if (FShaderArch = TContextShaderArch.Undefined) then
    LoadShaders;

  FVertexShader := TShaderManager.RegisterShaderFromData('blue.fvs',
    TContextShaderKind.VertexShader, '', [
    TContextShaderSource.Create(FShaderArch, FVertexShaderData,
    [TContextShaderVariable.Create('MVPMatrix', TContextShaderVariableKind.Matrix,
      FMatrixIndex, FMatrixSize)])
  ]);

  FPixelShader := TShaderManager.RegisterShaderFromData('blue.fps',
    TContextShaderKind.PixelShader, '', [
    TContextShaderSource.Create(FShaderArch, FPixelShaderData, [])
  ]);
end;

class function TBlueMaterial.LoadShader(const AResourceName: String): TBytes;
begin
  var Stream := TResourceStream.Create(HInstance, AResourceName, RT_RCDATA);
  try
    SetLength(Result, Stream.Size);
    Stream.ReadBuffer(Result, Length(Result));
  finally
    Stream.Free;
  end;
end;

class procedure TBlueMaterial.LoadShaders;
begin
  var Suffix := '';
  var ContextClass := TContextManager.DefaultContextClass;

  {$IF Defined(MSWINDOWS)}
  if (ContextClass.InheritsFrom(TCustomDX9Context)) then
  begin
    FShaderArch := TContextShaderArch.DX9;
    FMatrixIndex := 0;
    FMatrixSize := 4;
    Suffix := 'DX9';
  end
  else if (ContextClass.InheritsFrom(TCustomDX11Context)) then
  begin
    FShaderArch := TContextShaderArch.DX11;
    FMatrixIndex := 0;
    FMatrixSize := 64;
    Suffix := 'DX11';
  end;
  {$ELSE}
  if (ContextClass.InheritsFrom(TCustomContextOpenGL)) then
  begin
    FShaderArch := TContextShaderArch.GLSL;
    FMatrixIndex := 0;
    FMatrixSize := 4;
    Suffix := 'GL';
  end;
  {$ENDIF}

  {$IF Defined(MACOS)}
  if (ContextClass.InheritsFrom(TCustomContextMetal)) then
  begin
    FShaderArch := TContextShaderArch.Metal;
    FMatrixIndex := 1;
    FMatrixSize := 4;
    Suffix := 'MTL';
  end;
  {$ENDIF}

  if (FShaderArch = TContextShaderArch.Undefined) then
    raise EContext3DException.Create('Unknown or unsupported 3D context class');

  FVertexShaderData := LoadShader('VERTEX_SHADER_' + Suffix);
  FPixelShaderData := LoadShader('PIXEL_SHADER_' + Suffix);
end;

end.
