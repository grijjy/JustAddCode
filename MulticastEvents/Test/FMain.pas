unit FMain;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.Memo.Types,
  FMX.ScrollBox,
  FMX.Memo,
  FMX.Controls.Presentation,
  FMX.StdCtrls;

type
  TFormMain = class(TForm)
    ButtonRunTests: TButton;
    Memo: TMemo;
    procedure ButtonRunTestsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    FCurLine: String;
    procedure LogFlush;
    procedure LogSep;
    procedure Log(const AStr: String); overload;
    procedure Log(const AStr: String; const AArgs: array of const); overload;
    procedure LogLn(const AStr: String);
  private
    procedure Handler1(const ASender: TObject; const AValue: Integer);
    procedure Handler2(const ASender: TObject; const AValue: Integer);
    procedure Handler3(const ASender: TObject; const AValue: Integer);
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.fmx}

uses
  MulticastEvents;

type
  { An event handler that takes in Integer parameter (in addition to a Sender) }
  TSimpleEvent = TEventHandler<Integer>;

type
  { An class that emulates some sort of control }
  TSampleControl = class(TControl)
  private
    FSimpleEvent: TMulticastEvent<Integer>;
    function GetOnSimpleEvent: TSimpleEvent;
    procedure SetOnSimpleEvent(const AValue: TSimpleEvent);
  public
    { This emulates the firing of an event. It takes a single parameter. }
    procedure FireEvent(const AValue: Integer);

    { The multicast event that you can listen to }
    property SimpleEvent: TMulticastEvent<Integer> read FSimpleEvent;
  published
    { For compatibility with standard Delphi events }
    property OnSimpleEvent: TSimpleEvent read GetOnSimpleEvent write SetOnSimpleEvent;
  end;

{ TSampleControl }

procedure TSampleControl.FireEvent(const AValue: Integer);
begin
  { Fires an event by invoking the multicast event with the given parameter
    (in addition to the Self sender) }
  FSimpleEvent.Invoke(Self, AValue);
end;

function TSampleControl.GetOnSimpleEvent: TSimpleEvent;
begin
  { Returns the multicast event as a standard Delphi event }
  Result := FSimpleEvent;
end;

procedure TSampleControl.SetOnSimpleEvent(const AValue: TSimpleEvent);
begin
  { Assignes a standard Delphi event to the multicast event. This will clear
    any event handlers that were already added to the multicast event. }
  FSimpleEvent.Assign(AValue);
end;

{ TFormMain }

procedure TFormMain.ButtonRunTestsClick(Sender: TObject);
begin
  Memo.BeginUpdate;
  try
    Memo.Lines.Clear;
    { Run some tests! }
    var SampleControl := TSampleControl.Create(nil);
    try
      { The first test is the base case with no event handlers.
        It shouldn't fire any events. }
      LogLn('Testing unassigned event');
      LogLn('Expected output:');
      Log('Actual output  :');
      SampleControl.FireEvent(1);
      LogSep;

      { This tests a single event handler }
      LogLn('Assigning a simple event handler');
      LogLn('Expected output: H1.2');
      Log('Actual output  :');
      SampleControl.OnSimpleEvent := Handler1;
      SampleControl.FireEvent(2);
      LogSep;

      { This adds a second event handler to the same multicast event }
      LogLn('Adding a second handler');
      LogLn('Expected output: H1.3 H2.3');
      Log('Actual output  :');
      SampleControl.SimpleEvent.Add(Handler2);
      SampleControl.FireEvent(3);
      LogSep;

      { And a third event handler }
      LogLn('Adding a third handler');
      LogLn('Expected output: H1.4 H2.4 H3.4');
      Log('Actual output  :');
      SampleControl.SimpleEvent.Add(Handler3);
      SampleControl.FireEvent(4);
      LogSep;

      { Remove Handler2 (so only Handler1 and Handler3 remain) }
      LogLn('Removing the second handler');
      LogLn('Expected output: H1.5 H3.5');
      Log('Actual output  :');
      SampleControl.SimpleEvent.Remove(Handler2);
      SampleControl.FireEvent(5);
      LogSep;

      { Remove Handler1 (so only Handler3 remains) }
      LogLn('Removing the first handler');
      LogLn('Expected output: H3.6');
      Log('Actual output  :');
      SampleControl.SimpleEvent.Remove(Handler1);
      SampleControl.FireEvent(6);
      LogSep;

      { Remove Handler3 (so there are no more handlers) }
      LogLn('Removing the third handler');
      LogLn('Expected output:');
      Log('Actual output  :');
      SampleControl.SimpleEvent.Remove(Handler3);
      SampleControl.FireEvent(7);
      LogSep;

      { Add Handler1 and Handler3 (again) }
      LogLn('Adding the first and third handlers');
      LogLn('Expected output: H1.8 H3.8');
      Log('Actual output  :');
      SampleControl.SimpleEvent.Add(Handler1);
      SampleControl.SimpleEvent.Add(Handler3);
      SampleControl.FireEvent(8);
      LogSep;

      { Assigned Handler2 as a standard Delphi event handler (using the
        OnSimpleEvent property instead of SimpleEvent). This will remove the
        existing handlers (Handler1 and Handler3) from the list. }
      LogLn('Assigning the second handler');
      LogLn('Expected output: H2.9');
      Log('Actual output  :');
      SampleControl.OnSimpleEvent := Handler2;
      SampleControl.FireEvent(9);
      LogSep;

      { Assign nil to the standard Delphi event handler. This removes any
        handlers from the list. }
      LogLn('Assigning a nil handler');
      LogLn('Expected output:');
      Log('Actual output  :');
      SampleControl.OnSimpleEvent := nil;
      SampleControl.FireEvent(10);
      LogFlush;
    finally
      SampleControl.Free;
    end;
  finally
    Memo.EndUpdate;
  end;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  { To check if our multicast event system leaks memory }
  ReportMemoryLeaksOnShutdown := True;
end;

procedure TFormMain.Handler1(const ASender: TObject; const AValue: Integer);
begin
  Log(' H1.%d', [AValue]);
end;

procedure TFormMain.Handler2(const ASender: TObject; const AValue: Integer);
begin
  Log(' H2.%d', [AValue]);
end;

procedure TFormMain.Handler3(const ASender: TObject; const AValue: Integer);
begin
  Log(' H3.%d', [AValue]);
end;

procedure TFormMain.Log(const AStr: String);
begin
  FCurLine := FCurLine + AStr;
end;

procedure TFormMain.Log(const AStr: String; const AArgs: array of const);
begin
  Log(Format(AStr, AArgs));
end;

procedure TFormMain.LogFlush;
begin
  if (FCurLine <> '') then
  begin
    Memo.Lines.Add(FCurLine);
    FCurLine := '';
  end;
end;

procedure TFormMain.LogLn(const AStr: String);
begin
  LogFlush;
  Memo.Lines.Add(AStr);
end;

procedure TFormMain.LogSep;
begin
  LogFlush;
  LogLn('---------------------');
end;

end.
