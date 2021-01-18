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
  Grijjy.TextToSpeech, FMX.Edit;

type
  TFormMain = class(TForm)
    Memo: TMemo;
    MemoLog: TMemo;
    GridPanelLayout2: TGridPanelLayout;
    ButtonSpeak: TButton;
    ButtonStop: TButton;
    btnListVoices: TButton;
    btnClearLog: TButton;
    Label1: TLabel;
    edFemaleVoiceLang: TEdit;
    btnSetVoice: TButton;
    edMaleVoiceLang: TEdit;
    procedure FormCreate(Sender: TObject);
    procedure ButtonSpeakClick(Sender: TObject);
    procedure ButtonStopClick(Sender: TObject);
    procedure btnListVoicesClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnClearLogClick(Sender: TObject);
    procedure btnSetVoiceClick(Sender: TObject);
  private
    { Private declarations }
    FTextToSpeech: IgoTextToSpeech;
    // implemented a speech queue. If speaking, the string goes to the queue and waits for the terminated event
    // - This is necessary for Android, which truncates the speech if another string is spoken
    // - Gives more control on the speech queue

    fSpeechQueue:TStringList;          // local speech queue

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

procedure TFormMain.btnClearLogClick(Sender: TObject);
begin
  MemoLog.Lines.Clear;
end;

procedure TFormMain.btnListVoicesClick(Sender: TObject);
var SL:TStringList;
begin
  SL := TStringList.Create;
  if FTextToSpeech.getVoices(SL) then
    MemoLog.Lines.Assign(SL)
    else MemoLog.Lines.Add('error loading voices');
  SL.Free;
end;

procedure TFormMain.btnSetVoiceClick(Sender: TObject);
var aMV,aFV:String;
begin
  // set male and female voices language
  aMV :=  Trim( edMaleVoiceLang.Text );       // 'pt-BR' 'en-US' 'es-MX' ...
  aFV :=  Trim( edFemaleVoiceLang.Text );

  fTextToSpeech.SetVoice( aMV, aFV );
end;

procedure TFormMain.ButtonSpeakClick(Sender: TObject);  // <-- Do speak
var i:integer; s:String;
begin
  for i := 0 to Memo.Lines.Count-1 do    // speak one line at a time ( for dialogs )
    begin
      s := Memo.Lines[i];
      if Trim(S)='' then continue;

      if not FTextToSpeech.IsSpeaking then  // avoid using the OS queue (some don't have it)
       begin
         if FTextToSpeech.Speak(s)  then  // <-- do speak
            begin
              // great
            end
            else begin
              // add to queue ?
              Log('Unable to speak text');
              exit;
            end;
       end
       else begin  //already speaking. Add s to queue
         fSpeechQueue.Add(s);
       end;
    end;
end;

procedure TFormMain.ButtonStopClick(Sender: TObject);
begin
  FTextToSpeech.Stop;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  FTextToSpeech := TgoTextToSpeech.Create;

  FTextToSpeech.OnAvailable      := TextToSpeechAvailable;
  FTextToSpeech.OnSpeechStarted  := TextToSpeechStarted;
  FTextToSpeech.OnSpeechFinished := TextToSpeechFinished;

  fSpeechQueue := TStringList.Create;    // local speech queue

  MemoLog.Lines.Add( 'default language= '+NativeSpeechLanguage );  // show OS languege settings
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  fSpeechQueue.Free;
end;

procedure TFormMain.Log(const AMsg: String);
begin
  MemoLog.Lines.Add(AMsg);
end;

procedure TFormMain.TextToSpeechAvailable(Sender: TObject);   // speech callback
begin
  Log('Text-to-Speech engine is available');
  UpdateControls;
end;

procedure TFormMain.TextToSpeechFinished(Sender: TObject);    // speech callback
var s:String;
begin
  // retrieve speech s from queue, if any
  if (not FTextToSpeech.IsSpeaking) and (fSpeechQueue.Count>0) then  // avoid using the OS queue (some don't have it)
       begin
         s := fSpeechQueue.Strings[0];
         fSpeechQueue.Delete(0);

         if FTextToSpeech.Speak(s)  then  // <-- do speak
            begin  // great
            end
            else begin //error ?
              // add to queue ?
              Log('Unable to speak text');
              exit;
            end;
       end;

  Log('Speech finished');
  UpdateControls;
end;

procedure TFormMain.TextToSpeechStarted(Sender: TObject);     // speech callback
begin
  Log('Speech started');
  UpdateControls;
end;

procedure TFormMain.UpdateControls;    // upd ui with speech engine state
begin
  ButtonSpeak.Enabled := (not FTextToSpeech.IsSpeaking);
  ButtonStop.Enabled  := (not ButtonSpeak.Enabled);
end;

end.
