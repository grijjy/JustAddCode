unit FMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Memo.Types,
  FMX.ScrollBox, FMX.Memo, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts;

type
  TFormMain = class(TForm)
    LayoutTop: TLayout;
    ButtonTest: TButton;
    MemoLog: TMemo;
    ButtonTestCycle: TButton;
    procedure FormCreate(Sender: TObject);
    procedure ButtonTestClick(Sender: TObject);
    procedure ButtonTestCycleClick(Sender: TObject);
  private
    { Private declarations }
    procedure Log(const AMsg: String; const AArgs: array of const);
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.fmx}

uses
  SmartPointer;

type
  TBar = class;

  TFoo = class(TRefCountable)
  private
    FBar: TSmartPtr<TRefCountable>;
    FIntVal: Integer;
    FStrVal: String;
  public
    property IntVal: Integer read FIntVal write FIntVal;
    property StrVal: String read FStrVal write FStrVal;
    property Bar: TSmartPtr<TRefCountable> read FBar write FBar;
  end;

  TBar = class(TRefCountable)
  private
    FFoo: TSmartPtr<TFoo>;
  public
    property Foo: TSmartPtr<TFoo> read FFoo write FFoo;
  end;

procedure TFormMain.ButtonTestClick(Sender: TObject);
begin
  MemoLog.Lines.Clear;

  { Create a smart pointer for a TFoo instance. }
  var Foo1 := TSmartPtr<TFoo>.Create(TFoo.Create);

  Log('Reference count: %d (should be 1)', [Foo1.GetRefCount]);

  { Set some properties }
  Foo1.Ref.IntVal := 42;
  Foo1.Ref.StrVal := 'Foo';

  begin
    { Copy the smart pointer }
    var Foo2 := Foo1;
    Log('Reference count: %d (should be 2)', [Foo1.GetRefCount]);

    { Check properties }
    Log('IntVal: %d (should be 42)', [Foo2.Ref.IntVal]);
    Log('StrVal: %s (should be Foo)', [Foo2.Ref.StrVal]);

    { Foo2 will go out of scope here, so only Foo1
      will keep a reference to the TFoo object. }
  end;

  { There should be only 1 reference left. }
  Log('Reference count: %d (should be 1)', [Foo1.GetRefCount]);

  { Check properties again }
  Log('IntVal: %d (should be 42)', [Foo1.Ref.IntVal]);

  { TFoo instance will automatically be destroyed when the last smart pointer
    goes out of scope }
end;

procedure TFormMain.ButtonTestCycleClick(Sender: TObject);
begin
  { Create smart pointers for TFoo and TBar }
  var Foo := TSmartPtr<TFoo>.Create(TFoo.Create);
  var Bar := TSmartPtr<TBar>.Create(TBar.Create);

  { Make a reference cycle }
  Foo.Ref.Bar := TSmartPtr<TRefCountable>(Bar);
  Bar.Ref.Foo := Foo;

  { This will result in a memory leak since the smart pointers reference
    each other. When you shut down the application (on Windows), you should
    get a message saying that 2 objects have been leaked: TFoo and TBar. }
  MemoLog.Lines.Text := 'Created a Reference Cycle, resulting in a memory leak';
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  { To test the smart pointer implementation
    for memory leaks. }
  ReportMemoryLeaksOnShutdown := True;
end;

procedure TFormMain.Log(const AMsg: String; const AArgs: array of const);
begin
  MemoLog.Lines.Add(Format(AMsg, AArgs));
  MemoLog.GoToTextEnd;
end;

end.
