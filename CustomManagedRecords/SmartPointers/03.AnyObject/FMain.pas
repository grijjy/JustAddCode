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

  TFoo = class
  private
    FBar: TSmartPtr<TBar>;
  public
    property Bar: TSmartPtr<TBar> read FBar write FBar;
  end;

  TBar = class
  private
    FFoo: TSmartPtr<TFoo>;
  public
    property Foo: TSmartPtr<TFoo> read FFoo write FFoo;
  end;

procedure TFormMain.ButtonTestClick(Sender: TObject);
begin
  MemoLog.Lines.Clear;

  { Create a smart pointer for a TStringList.
    Note that you *cannot* use type inference here, since that would make
    List1 of type TSmartPtr<> instead of ISmartPtr<>! }
  var List1 := TSmartPtr<TStringList>.Create(TStringList.Create);

  Log('Reference count: %d (should be 1)', [List1.GetRefCount]);

  { Add some strings }
  List1.Ref.Add('Foo');
  List1.Ref.Add('Bar');

  begin
    { Copy the smart pointer }
    var List2 := List1;
    Log('Reference count: %d (should be 2)', [List1.GetRefCount]);

    { Check contents of List2 }
    Log('Number of items in list: %d (should be 2)', [List2.Ref.Count]);
    Log('First item: %s (should be Foo)', [List2.Ref[0]]);
    Log('Second item: %s (should be Bar)', [List2.Ref[1]]);

    { List2 will go out of scope here, so only List1
      will keep a reference to the string list. }
  end;

  { There should be only 1 reference left. }
  Log('Reference count: %d (should be 1)', [List1.GetRefCount]);

  { Check contents of List1 again }
  Log('Number of items in list: %d (should be 2)', [List1.Ref.Count]);

  { TStringList will automatically be destroyed when the last ISmartPtr
    reference goes out of scope }
end;

procedure TFormMain.ButtonTestCycleClick(Sender: TObject);
begin
  { Create smart pointers for TFoo and TBar }
  var Foo := TSmartPtr<TFoo>.Create(TFoo.Create);
  var Bar := TSmartPtr<TBar>.Create(TBar.Create);

  { Make a reference cycle }
  Foo.Ref.Bar := Bar;
  Bar.Ref.Foo := Foo;

  { This will result in a memory leak since the smart pointers reference
    each other. When you shut down the application (on Windows), you should
    get a message reporting 4 memory leaks: TFoo, TBar and 2 unknown pointers
    (for the reference counts). }
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
