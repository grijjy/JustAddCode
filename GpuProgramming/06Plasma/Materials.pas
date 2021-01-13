unit Materials;
{ Plasma shader based on klk's Simple Plasma:
  https://www.shadertoy.com/view/XsVSzW }

interface

uses
  System.Classes,
  System.SysUtils,
  FMX.Types3D,
  FMX.Graphics,
  FMX.Materials,
  FMX.MaterialSources;

type
  TPlasmaMaterialSource = class(TMaterialSource)
  private
    function GetTimeInSeconds: Single;
    procedure SetTimeInSeconds(const AValue: Single);
  protected
    function CreateMaterial: TMaterial; override;
  published
    property TimeInSeconds: Single read GetTimeInSeconds write SetTimeInSeconds;
  end;

type
  TPlasmaMaterial = class(TCustomMaterial)
  private class var
    FShaderArch: TContextShaderArch;
    FVertexShaderData: TBytes;
    FPixelShaderData: TBytes;
    FMatrixIndex: Integer;
    FMatrixSize: Integer;
    FFloatSize: Integer;
  private
    FTimeInSeconds: Single;
    procedure SetTimeInSeconds(const AValue: Single);
  private
    class procedure LoadShaders; static;
    class function LoadShader(const AResourceName: String): TBytes; static;
  protected
    procedure DoInitialize; override;
    procedure DoApply(const Context: TContext3D); override;
  public
    property TimeInSeconds: Single read FTimeInSeconds write SetTimeInSeconds;
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

{ TPlasmaMaterialSource }

function TPlasmaMaterialSource.CreateMaterial: TMaterial;
begin
  Result := TPlasmaMaterial.Create;
end;

function TPlasmaMaterialSource.GetTimeInSeconds: Single;
begin
  Result := TPlasmaMaterial(Material).TimeInSeconds;
end;

procedure TPlasmaMaterialSource.SetTimeInSeconds(const AValue: Single);
begin
  TPlasmaMaterial(Material).TimeInSeconds := AValue;
end;

{ TPlasmaMaterial }

procedure TPlasmaMaterial.DoApply(const Context: TContext3D);
begin
  inherited;
  Context.SetShaderVariable('Time', [Vector3D(FTimeInSeconds, 0, 0, 0)]);
end;

procedure TPlasmaMaterial.DoInitialize;
begin
  inherited;
  if (FShaderArch = TContextShaderArch.Undefined) then
    LoadShaders;

  FVertexShader := TShaderManager.RegisterShaderFromData('plasma.fvs',
    TContextShaderKind.VertexShader, '', [
    TContextShaderSource.Create(FShaderArch, FVertexShaderData,
    [TContextShaderVariable.Create('MVPMatrix', TContextShaderVariableKind.Matrix,
      FMatrixIndex, FMatrixSize)])
  ]);

  FPixelShader := TShaderManager.RegisterShaderFromData('plasma.fps',
    TContextShaderKind.PixelShader, '', [
    TContextShaderSource.Create(FShaderArch, FPixelShaderData,
    [TContextShaderVariable.Create('Time', TContextShaderVariableKind.Float, 0, FFloatSize)])
  ]);
end;

class function TPlasmaMaterial.LoadShader(const AResourceName: String): TBytes;
begin
  var Stream := TResourceStream.Create(HInstance, AResourceName, RT_RCDATA);
  try
    SetLength(Result, Stream.Size);
    Stream.ReadBuffer(Result, Length(Result));
  finally
    Stream.Free;
  end;
end;

class procedure TPlasmaMaterial.LoadShaders;
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

procedure TPlasmaMaterial.SetTimeInSeconds(const AValue: Single);
begin
  if (AValue <> FTimeInSeconds) then
  begin
    FTimeInSeconds := AValue;
    DoChange;
  end;
end;

end.
