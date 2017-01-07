unit Grijjy.TextToSpeech.Android;
{< Text To Speech engine implementation for Android }

{$INCLUDE 'Grijjy.inc'}

interface

uses
  Androidapi.JNIBridge,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNI.Speech,
  Grijjy.TextToSpeech.Base;

type
  { IgoSpeechToText implementation }
  TgoTextToSpeechImplementation = class(TgoTextToSpeechBase)
  {$REGION 'Internal Declarations'}
  private type
    TInitListener = class(TJavaLocal, JTextToSpeech_OnInitListener)
    private
      [weak] FImplementation: TgoTextToSpeechImplementation;
    public
      { JTextToSpeech_OnInitListener }
      procedure onInit(status: Integer); cdecl;
    public
      constructor Create(const AImplementation: TgoTextToSpeechImplementation);
    end;
  private type
    TCompletedListener = class(TJavaLocal, JTextToSpeech_OnUtteranceCompletedListener)
    private
      [weak] FImplementation: TgoTextToSpeechImplementation;
    public
      { JTextToSpeech_OnUtteranceCompletedListener }
      procedure onUtteranceCompleted(utteranceId: JString); cdecl;
    public
      constructor Create(const AImplementation: TgoTextToSpeechImplementation);
    end;
  private
    FTextToSpeech: JTextToSpeech;
    FInitListener: TInitListener;
    FCompletedListener: TCompletedListener;
    FParams: JHashMap;
  private
    procedure Initialize(const AStatus: Integer);
  protected
    { IgoTextToSpeech }
    function Speak(const AText: String): Boolean; override;
    procedure Stop; override;
    function IsSpeaking: Boolean; override;
  {$ENDREGION 'Internal Declarations'}
  public
    constructor Create;
  end;

implementation

uses
  System.SysUtils,
  Androidapi.Helpers;

{ TgoTextToSpeechImplementation }

constructor TgoTextToSpeechImplementation.Create;
begin
  inherited;
  FInitListener := TInitListener.Create(Self);
  FTextToSpeech := TJTextToSpeech.JavaClass.init(TAndroidHelper.Context, FInitListener);
end;

procedure TgoTextToSpeechImplementation.Initialize(const AStatus: Integer);
begin
  FInitListener := nil;
  if (AStatus = TJTextToSpeech.JavaClass.SUCCESS) then
  begin
    Available := True;
    DoAvailable;

    FTextToSpeech.setLanguage(TJLocale.JavaClass.getDefault);

    { We need a hash map with a KEY_PARAM_UTTERANCE_ID parameter.
      Otherwise, onUtteranceCompleted will not get called. }
    FParams := TJHashMap.Create;
    FParams.put(TJTextToSpeech_Engine.JavaClass.KEY_PARAM_UTTERANCE_ID, StringToJString('DummyUtteranceId'));
    FCompletedListener := TCompletedListener.Create(Self);
    FTextToSpeech.setOnUtteranceCompletedListener(FCompletedListener);
  end
  else
    FTextToSpeech := nil;
end;

function TgoTextToSpeechImplementation.IsSpeaking: Boolean;
begin
  if Assigned(FTextToSpeech) then
    Result := FTextToSpeech.isSpeaking
  else
    Result := False;
end;

function TgoTextToSpeechImplementation.Speak(const AText: String): Boolean;
begin
  if (AText.Trim = '') then
    Exit(True);

  if Assigned(FTextToSpeech) then
  begin
    Result := (FTextToSpeech.speak(StringToJString(AText),
      TJTextToSpeech.JavaClass.QUEUE_FLUSH, FParams) = TJTextToSpeech.JavaClass.SUCCESS);
    if (Result) then
      DoSpeechStarted;
  end
  else
    Result := False;
end;

procedure TgoTextToSpeechImplementation.Stop;
begin
  if Assigned(FTextToSpeech) then
    FTextToSpeech.stop;
end;

{ TgoTextToSpeechImplementation.TInitListener }

constructor TgoTextToSpeechImplementation.TInitListener.Create(
  const AImplementation: TgoTextToSpeechImplementation);
begin
  Assert(Assigned(AImplementation));
  inherited Create;
  FImplementation := AImplementation;
end;

procedure TgoTextToSpeechImplementation.TInitListener.onInit(status: Integer);
begin
  if Assigned(FImplementation) then
    FImplementation.Initialize(status);
end;

{ TgoTextToSpeechImplementation.TCompletedListener }

constructor TgoTextToSpeechImplementation.TCompletedListener.Create(
  const AImplementation: TgoTextToSpeechImplementation);
begin
  Assert(Assigned(AImplementation));
  inherited Create;
  FImplementation := AImplementation;
end;

procedure TgoTextToSpeechImplementation.TCompletedListener.onUtteranceCompleted(
  utteranceId: JString);
begin
  if Assigned(FImplementation) then
    FImplementation.DoSpeechFinished;
end;

end.
