unit Materials;
{ Fireworks shader based on Martijn Steinrucken's "[SH17A] Fireworks" shader.
  https://www.shadertoy.com/view/ldBfzw }

interface

uses
  System.Classes,
  System.SysUtils,
  FMX.Types3D,
  FMX.Graphics,
  FMX.Materials,
  FMX.MaterialSources;

type
  TFireworksMaterialSource = class(TMaterialSource)
  private
    function GetTimeInSeconds: Single;
    procedure SetTimeInSeconds(const AValue: Single);
    function GetFireworksCount: Integer;
    procedure SetFireworksCount(const AValue: Integer);
    function GetSparkCount: Integer;
    procedure SetSparkCount(const AValue: Integer);
  protected
    function CreateMaterial: TMaterial; override;
  published
    property TimeInSeconds: Single read GetTimeInSeconds write SetTimeInSeconds;
    property SparkCount: Integer read GetSparkCount write SetSparkCount;
    property FireworksCount: Integer read GetFireworksCount write SetFireworksCount;
  end;

type
  TFireworksMaterial = class(TCustomMaterial)
  private class var
    FShaderArch: TContextShaderArch;
    FVertexShaderData: TBytes;
    FPixelShaderData: TBytes;
    FMatrixIndex: Integer;
    FMatrixSize: Integer;
    FFloatSize: Integer;
  private
    FTimeInSeconds: Single;
    FFireworksCount: Integer;
    FSparkCount: Integer;
    procedure SetTimeInSeconds(const AValue: Single);
    procedure SetFireworksCount(const AValue: Integer);
    procedure SetSparkCount(const AValue: Integer);
  private
    class procedure LoadShaders; static;
    class function LoadShader(const AResourceName: String): TBytes; static;
  protected
    procedure DoInitialize; override;
    procedure DoApply(const Context: TContext3D); override;
  public
    constructor Create; override;

    property TimeInSeconds: Single read FTimeInSeconds write SetTimeInSeconds;
    property FireworksCount: Integer read FFireworksCount write SetFireworksCount;
    property SparkCount: Integer read FSparkCount write SetSparkCount;
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

{ TFireworksMaterialSource }

function TFireworksMaterialSource.CreateMaterial: TMaterial;
begin
  Result := TFireworksMaterial.Create;
end;

function TFireworksMaterialSource.GetFireworksCount: Integer;
begin
  Result := TFireworksMaterial(Material).FireworksCount;
end;

function TFireworksMaterialSource.GetSparkCount: Integer;
begin
  Result := TFireworksMaterial(Material).SparkCount;
end;

function TFireworksMaterialSource.GetTimeInSeconds: Single;
begin
  Result := TFireworksMaterial(Material).TimeInSeconds;
end;

procedure TFireworksMaterialSource.SetFireworksCount(const AValue: Integer);
begin
  TFireworksMaterial(Material).FireworksCount := AValue;
end;

procedure TFireworksMaterialSource.SetSparkCount(const AValue: Integer);
begin
  TFireworksMaterial(Material).SparkCount := AValue;
end;

procedure TFireworksMaterialSource.SetTimeInSeconds(const AValue: Single);
begin
  TFireworksMaterial(Material).TimeInSeconds := AValue;
end;

{ TFireworksMaterial }

constructor TFireworksMaterial.Create;
begin
  inherited;
  FFireworksCount := 9;
  FSparkCount := 50;
end;

procedure TFireworksMaterial.DoApply(const Context: TContext3D);
begin
  inherited;
  Context.SetShaderVariable('Time', [Vector3D(FTimeInSeconds, 0, 0, 0)]);

  { NOTE: In the shader, to loop variable starts at -2, so we need to subtract
    2 from FFireworksCount to set the correct count. }
  Context.SetShaderVariable('FireworksCount', [Vector3D(FFireworksCount - 2, 0, 0, 0)]);

  Context.SetShaderVariable('SparkCount', [Vector3D(FSparkCount, 0, 0, 0)]);
end;

procedure TFireworksMaterial.DoInitialize;
begin
  inherited;
  if (FShaderArch = TContextShaderArch.Undefined) then
    LoadShaders;

  FVertexShader := TShaderManager.RegisterShaderFromData('fireworks.fvs',
    TContextShaderKind.VertexShader, '', [
    TContextShaderSource.Create(FShaderArch, FVertexShaderData,
    [TContextShaderVariable.Create('MVPMatrix', TContextShaderVariableKind.Matrix,
      FMatrixIndex, FMatrixSize)])
  ]);

  FPixelShader := TShaderManager.RegisterShaderFromData('fireworks.fps',
    TContextShaderKind.PixelShader, '', [
    TContextShaderSource.Create(FShaderArch, FPixelShaderData,
    [TContextShaderVariable.Create('Time', TContextShaderVariableKind.Float, 0, FFloatSize),
     TContextShaderVariable.Create('FireworksCount', TContextShaderVariableKind.Float, FFloatSize, FFloatSize),
     TContextShaderVariable.Create('SparkCount', TContextShaderVariableKind.Float, FFloatSize * 2, FFloatSize)])
  ]);
end;

class function TFireworksMaterial.LoadShader(const AResourceName: String): TBytes;
begin
  var Stream := TResourceStream.Create(HInstance, AResourceName, RT_RCDATA);
  try
    SetLength(Result, Stream.Size);
    Stream.ReadBuffer(Result, Length(Result));
  finally
    Stream.Free;
  end;
end;

class procedure TFireworksMaterial.LoadShaders;
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

procedure TFireworksMaterial.SetFireworksCount(const AValue: Integer);
begin
  if (AValue <> FFireworksCount) then
  begin
    FFireworksCount := AValue;
    DoChange;
  end;
end;

procedure TFireworksMaterial.SetSparkCount(const AValue: Integer);
begin
  if (AValue <> FSparkCount) then
  begin
    FSparkCount := AValue;
    DoChange;
  end;
end;

procedure TFireworksMaterial.SetTimeInSeconds(const AValue: Single);
begin
  if (AValue <> FTimeInSeconds) then
  begin
    FTimeInSeconds := AValue;
    DoChange;
  end;
end;

end.
