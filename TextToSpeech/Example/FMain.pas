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
  FMX.StdCtrls,
  FMX.Controls.Presentation,
  FMX.ScrollBox,
  FMX.Memo,
  FMX.ExtCtrls,
  FMX.Layouts,
  Grijjy.TextToSpeech;

type
  TFormMain = class(TForm)
    Memo: TMemo;
    MemoLog: TMemo;
    GridPanelLayout2: TGridPanelLayout;
    ButtonSpeak: TButton;
    ButtonStop: TButton;
    procedure FormCreate(Sender: TObject);
    procedure ButtonSpeakClick(Sender: TObject);
    procedure ButtonStopClick(Sender: TObject);
  private
    { Private declarations }
    FTextToSpeech: IgoTextToSpeech;
    procedure Log(const AMsg: String);
    procedure TextToSpeechAvailable(Sender: TObject);
    procedure TextToSpeechStarted(Sender: TObject);
    procedure TextToSpeechFinished(Sender: TObject);
    procedure UpdateControls;
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

{$R *.fmx}

procedure TFormMain.ButtonSpeakClick(Sender: TObject);
begin
  if (not FTextToSpeech.Speak(Memo.Text)) then
    Log('Unable to speak text');
end;

procedure TFormMain.ButtonStopClick(Sender: TObject);
begin
  FTextToSpeech.Stop;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  FTextToSpeech := TgoTextToSpeech.Create;
  FTextToSpeech.OnAvailable := TextToSpeechAvailable;
  FTextToSpeech.OnSpeechStarted := TextToSpeechStarted;
  FTextToSpeech.OnSpeechFinished := TextToSpeechFinished;
end;

procedure TFormMain.Log(const AMsg: String);
begin
  MemoLog.Lines.Add(AMsg);
end;

procedure TFormMain.TextToSpeechAvailable(Sender: TObject);
begin
  Log('Text-to-Speech engine is available');
  UpdateControls;
end;

procedure TFormMain.TextToSpeechFinished(Sender: TObject);
begin
  Log('Speech finished');
  UpdateControls;
end;

procedure TFormMain.TextToSpeechStarted(Sender: TObject);
begin
  Log('Speech started');
  UpdateControls;
end;

procedure TFormMain.UpdateControls;
begin
  ButtonSpeak.Enabled := (not FTextToSpeech.IsSpeaking);
  ButtonStop.Enabled := (not ButtonSpeak.Enabled);
end;

end.
