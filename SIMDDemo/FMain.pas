unit FMain;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Diagnostics,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.ScrollBox,
  FMX.Memo,
  FMX.StdCtrls,
  FMX.Controls.Presentation,
  CalcTests;

type
  TFormMain = class(TForm)
    ToolBar: TToolBar;
    LabelHeader: TLabel;
    ButtonAdd: TButton;
    Memo: TMemo;
    ButtonAddAndSaturate: TButton;
    ButtonDistanceSquared: TButton;
    procedure FormCreate(Sender: TObject);
    procedure ButtonAddClick(Sender: TObject);
    procedure ButtonAddAndSaturateClick(Sender: TObject);
    procedure ButtonDistanceSquaredClick(Sender: TObject);
  private
    { Private declarations }
    procedure RunTest(const ACaption: String; const ADelphiProc,
      ASIMDProc: TTestProc);
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.fmx}

procedure TFormMain.ButtonAddAndSaturateClick(Sender: TObject);
begin
  RunTest('Add and saturate', TestAddAndSaturateDelphi, TestAddAndSaturateSIMD);
end;

procedure TFormMain.ButtonAddClick(Sender: TObject);
begin
  RunTest('Add 16 bytes', TestAddDelphi, TestAddSIMD);
end;

procedure TFormMain.ButtonDistanceSquaredClick(Sender: TObject);
begin
  RunTest('Squared distance between 2 vectors', TestDistanceSquaredDelphi, TestDistanceSquaredSIMD);
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  {$IF Defined(MSWINDOWS)}
    {$IF Defined(CPU64BITS)}
    LabelHeader.Text := 'Windows - 64 bit';
    {$ELSE}
    LabelHeader.Text := 'Windows - 32 bit';
    {$ENDIF}
  {$ELSEIF Defined(ANDROID)}
  LabelHeader.Text := 'Android - 32 bit';
  {$ELSEIF Defined(IOS)}
    {$IF Defined(CPUX86)}
    LabelHeader.Text := 'iOS Simulator - 32 bit';
    {$ELSEIF Defined(CPU64BITS)}
    LabelHeader.Text := 'iOS - 64 bit';
    {$ELSE}
    LabelHeader.Text := 'iOS - 32 bit';
    {$ENDIF}
  {$ELSEIF Defined(MACOS)}
  LabelHeader.Text := 'macOS - 32 bit';
  {$ENDIF}

  UnitTestAdd;
  UnitTestAddAndSaturate;
  UnitTestDistanceSquared;
end;

procedure TFormMain.RunTest(const ACaption: String; const ADelphiProc,
  ASIMDProc: TTestProc);
var
  Stopwatch: TStopwatch;
  DelphiTimeMS, SIMDTimeMS: Double;
begin
  Stopwatch := TStopwatch.StartNew;
  ADelphiProc();
  DelphiTimeMS := Stopwatch.Elapsed.TotalMilliseconds;

  Stopwatch := TStopwatch.StartNew;
  ASIMDProc();
  SIMDTimeMS := Stopwatch.Elapsed.TotalMilliseconds;

  Memo.Lines.Add(ACaption);
  Memo.Lines.Add(Format(' * Delphi: %.2f ms, SIMD: %.2f ms',
    [DelphiTimeMS, SIMDTimeMS]));
  Memo.Lines.Add(Format(' * SIMD Speedup: x %.2f', [DelphiTimeMS / SIMDTimeMS]));

  Memo.SelStart := Memo.Text.Length;
end;

end.
