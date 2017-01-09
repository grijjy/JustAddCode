unit Grijjy.TextToSpeech.macOS;
{< Text To Speech engine implementation for macOS }

interface

uses
  Macapi.AppKit,
  Macapi.ObjectiveC,
  Grijjy.TextToSpeech.Base;

type
  { The declaration of NSSpeechSynthesizerDelegate in Macapi.AppKit is
    incomplete. So we add the events we are interested in here. }
  NSSpeechSynthesizerDelegateEx = interface(NSSpeechSynthesizerDelegate)
  ['{D0AE9338-9D9B-4857-A404-255F40E4EB09}']
    [MethodName('speechSynthesizer:willSpeakPhoneme:')]
    procedure speechSynthesizerWillSpeakPhoneme(sender: NSSpeechSynthesizer;
      phonemeOpcode: Smallint); cdecl;

    [MethodName('speechSynthesizer:didFinishSpeaking:')]
    procedure speechSynthesizerDidFinishSpeaking(sender: NSSpeechSynthesizer;
      finishedSpeaking: Boolean); cdecl;
  end;

type
  { IgoSpeechToText implementation }
  TgoTextToSpeechImplementation = class(TgoTextToSpeechBase)
  {$REGION 'Internal Declarations'}
  private type
    TDelegate = class(TOCLocal, NSSpeechSynthesizerDelegate, NSSpeechSynthesizerDelegateEx)
    private
      FTextToSpeech: TgoTextToSpeechImplementation;
      FStartedSpeaking: Boolean;
    public
      constructor Create(const ATextToSpeech: TgoTextToSpeechImplementation);
    public
      { AVSpeechSynthesizerDelegate }
      [MethodName('speechSynthesizer:willSpeakPhoneme:')]
      procedure speechSynthesizerWillSpeakPhoneme(sender: NSSpeechSynthesizer;
        phonemeOpcode: Smallint); cdecl;

      [MethodName('speechSynthesizer:didFinishSpeaking:')]
      procedure speechSynthesizerDidFinishSpeaking(sender: NSSpeechSynthesizer;
        finishedSpeaking: Boolean); cdecl;
    end;
  private
    FSpeechSynthesizer: NSSpeechSynthesizer;
    FDelegate: NSSpeechSynthesizerDelegateEx;
  protected
    { IgoTextToSpeech }
    function Speak(const AText: String): Boolean; override;
    procedure Stop; override;
    function IsSpeaking: Boolean; override;
  {$ENDREGION 'Internal Declarations'}
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  Macapi.Helpers;

{ TgoTextToSpeechImplementation }

constructor TgoTextToSpeechImplementation.Create;
begin
  inherited Create;
  FSpeechSynthesizer := TNSSpeechSynthesizer.Create;
  if (FSpeechSynthesizer <> nil) then
  begin
    FDelegate := TDelegate.Create(Self);
    FSpeechSynthesizer.setDelegate(FDelegate);
    Available := True;
  end;
end;

destructor TgoTextToSpeechImplementation.Destroy;
begin
  if (FSpeechSynthesizer <> nil) then
    FSpeechSynthesizer.release;
  inherited;
end;

function TgoTextToSpeechImplementation.IsSpeaking: Boolean;
begin
  Result := (FSpeechSynthesizer <> nil) and (FSpeechSynthesizer.isSpeaking);
end;

function TgoTextToSpeechImplementation.Speak(const AText: String): Boolean;
begin
  if (FSpeechSynthesizer = nil) then
    Result := False
  else
    Result := FSpeechSynthesizer.startSpeakingString(StrToNSStr(AText));
end;

procedure TgoTextToSpeechImplementation.Stop;
begin
  if (FSpeechSynthesizer <> nil) then
    FSpeechSynthesizer.stopSpeaking;
end;

{ TgoTextToSpeechImplementation.TDelegate }

constructor TgoTextToSpeechImplementation.TDelegate.Create(
  const ATextToSpeech: TgoTextToSpeechImplementation);
begin
  Assert(Assigned(ATextToSpeech));
  inherited Create;
  FTextToSpeech := ATextToSpeech;
end;

procedure TgoTextToSpeechImplementation.TDelegate.speechSynthesizerDidFinishSpeaking(
  sender: NSSpeechSynthesizer; finishedSpeaking: Boolean);
begin
  FStartedSpeaking := False;
  if Assigned(FTextToSpeech) then
    FTextToSpeech.DoSpeechFinished;
end;

procedure TgoTextToSpeechImplementation.TDelegate.speechSynthesizerWillSpeakPhoneme(
  sender: NSSpeechSynthesizer; phonemeOpcode: Smallint);
begin
  if (not FStartedSpeaking) then
  begin
    FStartedSpeaking := True;
    if Assigned(FTextToSpeech) then
      FTextToSpeech.DoSpeechStarted;
  end;
end;

end.
