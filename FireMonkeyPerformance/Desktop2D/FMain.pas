unit FMain;
{ Measurements on 2019 13" macBook Pro:

                                 FPS     CPU%   GPU%
  --------------------------------------------------
  Default, without 3D control     20      104    0.0
  Default, with 3D control        16       98    2.8
  Metal, without 3D control       60       95   10.8
  Metal, with 3D control          57       99   15.1 }

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Diagnostics,
  System.Math.Vectors,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.StdCtrls,
  FMX.Layouts,
  FMX.ListBox,
  FMX.Edit,
  FMX.TextLayout,
  FMX.Controls.Presentation,
  FMX.Objects,
  FMX.Colors,
  FMX.ComboEdit,
  FMX.SpinBox,
  FMX.TreeView,
  FMX.DateTimeCtrls,
  FMX.Effects,
  FMX.EditBox,
  FMX.NumberBox,
  FMX.Calendar,
  FMX.ExtCtrls,
  FMX.Controls3D,
  FMX.Objects3D,
  FMX.Viewport3D,
  FMX.MaterialSources,
  FMX.Types3D;

type
  TFormMain = class(TForm)
    LayoutTop: TLayout;
    RectangleBackground: TRectangle;
    Text3: TLabel;
    TextBox1: TEdit;
    CheckBox1: TCheckBox;
    RadioButton1: TRadioButton;
    RadioButton2: TRadioButton;
    StringComboBox1: TComboBox;
    ListBoxItem51: TListBoxItem;
    ListBoxItem52: TListBoxItem;
    ListBoxItem53: TListBoxItem;
    ListBoxItem54: TListBoxItem;
    ListBoxItem55: TListBoxItem;
    ListBoxItem56: TListBoxItem;
    ListBoxItem57: TListBoxItem;
    ListBoxItem58: TListBoxItem;
    ListBoxItem59: TListBoxItem;
    ListBoxItem60: TListBoxItem;
    ListBoxItem61: TListBoxItem;
    ListBoxItem62: TListBoxItem;
    ListBoxItem63: TListBoxItem;
    ListBoxItem64: TListBoxItem;
    ListBoxItem65: TListBoxItem;
    ListBoxItem66: TListBoxItem;
    ListBoxItem67: TListBoxItem;
    ListBoxItem68: TListBoxItem;
    ListBoxItem69: TListBoxItem;
    ListBoxItem70: TListBoxItem;
    ListBoxItem71: TListBoxItem;
    ListBoxItem72: TListBoxItem;
    TrackBar6: TTrackBar;
    ScrollBar1: TScrollBar;
    ProgressBar1: TProgressBar;
    ScrollBar2: TScrollBar;
    SmallScrollBar1: TSmallScrollBar;
    Label1: TLabel;
    AniIndicator1: TAniIndicator;
    Text2: TLabel;
    StringListBox1: TListBox;
    ListBoxItem29: TListBoxItem;
    ListBoxItem30: TListBoxItem;
    ListBoxItem31: TListBoxItem;
    ListBoxItem32: TListBoxItem;
    ListBoxItem33: TListBoxItem;
    ListBoxItem34: TListBoxItem;
    ListBoxItem35: TListBoxItem;
    ListBoxItem36: TListBoxItem;
    ListBoxItem37: TListBoxItem;
    ListBoxItem38: TListBoxItem;
    ListBoxItem39: TListBoxItem;
    ListBoxItem40: TListBoxItem;
    ListBoxItem41: TListBoxItem;
    ListBoxItem42: TListBoxItem;
    ListBoxItem43: TListBoxItem;
    ListBoxItem44: TListBoxItem;
    ListBoxItem45: TListBoxItem;
    ListBoxItem46: TListBoxItem;
    ListBoxItem47: TListBoxItem;
    ListBoxItem48: TListBoxItem;
    ListBoxItem49: TListBoxItem;
    ListBoxItem50: TListBoxItem;
    CheckBox2: TCheckBox;
    Button4: TButton;
    CalloutPanel1: TCalloutPanel;
    calloutRight: TRadioButton;
    Label29: TLabel;
    calloutTop: TRadioButton;
    calloutLeft: TRadioButton;
    calloutBottom: TRadioButton;
    Panel1: TPanel;
    Label19: TLabel;
    SpeedButton2: TSpeedButton;
    SpeedButton3: TSpeedButton;
    TimerAnimate: TTimer;
    LabelFPS: TLabel;
    LabelInfo: TLabel;
    Expander1: TExpander;
    Label8: TLabel;
    Button3: TButton;
    PopupBox1: TPopupBox;
    TextBox3: TEdit;
    Calendar1: TCalendar;
    Label12: TLabel;
    TrackBar1: TTrackBar;
    NumberBox1: TNumberBox;
    GlowEffect2: TGlowEffect;
    Label10: TLabel;
    DateEdit1: TDateEdit;
    Label16: TLabel;
    AngleButton1: TArcDial;
    AngleButton3: TArcDial;
    AngleButton2: TArcDial;
    ListBox1: TListBox;
    ListBoxItem1: TListBoxItem;
    ListBoxItem2: TListBoxItem;
    Image1: TImage;
    ListBoxItem3: TListBoxItem;
    Path1: TPath;
    ListBoxItem4: TListBoxItem;
    TextBox2: TEdit;
    ListBoxItem6: TListBoxItem;
    Rectangle1: TRectangle;
    Text6: TLabel;
    Ellipse1: TEllipse;
    TreeView1: TTreeView;
    TreeViewItem1: TTreeViewItem;
    TreeViewItem2: TTreeViewItem;
    TreeViewItem15: TTreeViewItem;
    TreeViewItem16: TTreeViewItem;
    TreeViewItem20: TTreeViewItem;
    TreeViewItem21: TTreeViewItem;
    TreeViewItem22: TTreeViewItem;
    TreeViewItem23: TTreeViewItem;
    TreeViewItem24: TTreeViewItem;
    TreeViewItem17: TTreeViewItem;
    TreeViewItem18: TTreeViewItem;
    TreeViewItem25: TTreeViewItem;
    TreeViewItem26: TTreeViewItem;
    TreeViewItem19: TTreeViewItem;
    TreeViewItem3: TTreeViewItem;
    TreeViewItem4: TTreeViewItem;
    TreeViewItem27: TTreeViewItem;
    TreeViewItem28: TTreeViewItem;
    TreeViewItem29: TTreeViewItem;
    TreeViewItem34: TTreeViewItem;
    TreeViewItem35: TTreeViewItem;
    TreeViewItem36: TTreeViewItem;
    TreeViewItem37: TTreeViewItem;
    TreeViewItem30: TTreeViewItem;
    TreeViewItem31: TTreeViewItem;
    TreeViewItem32: TTreeViewItem;
    TreeViewItem33: TTreeViewItem;
    TreeViewItem5: TTreeViewItem;
    TreeViewItem6: TTreeViewItem;
    TreeViewItem7: TTreeViewItem;
    TreeViewItem8: TTreeViewItem;
    TreeViewItem9: TTreeViewItem;
    TreeViewItem10: TTreeViewItem;
    TreeViewItem11: TTreeViewItem;
    TreeViewItem12: TTreeViewItem;
    TreeViewItem13: TTreeViewItem;
    TreeViewItem14: TTreeViewItem;
    Ellipse2: TEllipse;
    CornerButton1: TCornerButton;
    SpinBox1: TSpinBox;
    CornerButton2: TCornerButton;
    CornerButton3: TCornerButton;
    CornerButton4: TCornerButton;
    CornerButton5: TCornerButton;
    Path2: TPath;
    CornerButton6: TCornerButton;
    Path3: TPath;
    comboedit1: TComboEdit;
    ComboColorBox1: TComboColorBox;
    AlphaTrackBar1: TAlphaTrackBar;
    BWTrackBar1: TBWTrackBar;
    HueTrackBar1: THueTrackBar;
    Label28: TLabel;
    Rectangle2: TPanel;
    Label23: TLabel;
    VertScrollBox1: TVertScrollBox;
    Expander2: TExpander;
    Button7: TButton;
    TrackBar5: TTrackBar;
    TextBox5: TEdit;
    Expander3: TExpander;
    ButtonE3: TButton;
    TrackBarE3: TTrackBar;
    EditE3: TEdit;
    Expander4: TExpander;
    ButtonE4: TButton;
    TrackBarE4: TTrackBar;
    EditE4: TEdit;
    Viewport3D: TViewport3D;
    Sphere: TSphere;
    LightMaterialSource1: TLightMaterialSource;
    Light1: TLight;
    CheckBoxShow3DControl: TCheckBox;
    procedure TimerAnimateTimer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject; Canvas: TCanvas; const ARect: TRectF);
    procedure CheckBoxShow3DControlChange(Sender: TObject);
  private
    { Private declarations }
    FHue: Single;
    FTicksPerSecond: Int64;
    FNextSecond: Int64;
    FPrevTicks: Int64;
    FFrameCount: Integer;
    procedure ApplicationIdle(Sender: TObject; var Done: Boolean);
    procedure Animate;
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.fmx}

uses
  System.Math,
  System.UIConsts;

procedure TFormMain.Animate;
begin
  var Ticks := TStopwatch.GetTimeStamp;

  if (FPrevTicks <> 0) then
  begin
    var Change := (Ticks - FPrevTicks) / FTicksPerSecond;
    FHue := FHue + (0.2 * Change);
    FHue := Frac(FHue); // Wrap around at 1.0

    var Angle := Sphere.RotationAngle.Y;
    Angle := Angle + (20 * Change);
    Angle := FMod(Angle, 360); // Wrap around at 360.0
    Sphere.RotationAngle.Y := Angle;
  end;

  FPrevTicks := Ticks;

  RectangleBackground.Fill.Color := HSLtoRGB(FHue, 1, 0.75);

  if (Ticks >= FNextSecond) then
  begin
    LabelFPS.Text := Format('%d fps', [FFrameCount]);
    LabelFPS.Repaint;
    FFrameCount := 0;
    Inc(FNextSecond, FTicksPerSecond);
    if (Ticks >= FNextSecond) then
      { Catch up }
      FNextSecond := Ticks + FTicksPerSecond;
  end;
end;

procedure TFormMain.ApplicationIdle(Sender: TObject; var Done: Boolean);
begin
  { To try and repaint as fast as possible, we use both a timer and the OnIdle
    event. }
  Animate;
end;

procedure TFormMain.CheckBoxShow3DControlChange(Sender: TObject);
begin
  Viewport3D.Visible := CheckBoxShow3DControl.IsChecked;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  Application.OnIdle := ApplicationIdle;
  TStopwatch.StartNew; // Init high-res timer
  FTicksPerSecond := TStopwatch.Frequency;
  FNextSecond := TStopwatch.GetTimeStamp + FTicksPerSecond;

  LabelInfo.Text := Format('Canvas class: %s, Text layout class: %s, 3D Context class: %s', [
    Canvas.ClassName, TTextLayoutManager.DefaultTextLayout.ClassName,
    Viewport3D.Context.ClassName]);
end;

procedure TFormMain.FormPaint(Sender: TObject; Canvas: TCanvas;
  const ARect: TRectF);
begin
  Inc(FFrameCount);
end;

procedure TFormMain.TimerAnimateTimer(Sender: TObject);
begin
  Animate;
end;

end.
