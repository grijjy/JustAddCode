unit FBase;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Graphics,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,
  FMX.StdCtrls,
  FMX.Controls.Presentation,
  FMX.ScrollBox,
  FMX.Memo;

type
  TFrameBase = class(TFrame)
    Memo: TMemo;
    ToolBar: TToolBar;
    LabelCaption: TLabel;
  private
    { Private declarations }
  protected
    procedure Log(const AMsg: String); overload;
    procedure Log(const AMsg: String; const AArgs: array of const); overload;
  public
    { Public declarations }
  end;

implementation

{$R *.fmx}

{ TFrameBase }

procedure TFrameBase.Log(const AMsg: String);
begin
  Memo.Lines.Add(AMsg);
  Memo.SelStart := Integer.MaxValue; // Scroll to end
end;

procedure TFrameBase.Log(const AMsg: String; const AArgs: array of const);
begin
  Log(Format(AMsg, AArgs));
end;

end.
