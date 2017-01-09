unit Grijjy.TextToSpeech.iOS;
{< Text To Speech engine implementation for iOS }

interface

uses
  Macapi.ObjectiveC,
  iOSapi.Foundation,
  iOSapi.CocoaTypes,
  iOSapi.AVFoundation,
  Grijjy.TextToSpeech.Base;

{ These declarations are missing from iOSapi.AVFoundation }
type
  AVSpeechBoundary = NSInteger;

const
  AVSpeechBoundaryImmediate = 0;
  AVSpeechBoundaryWord      = 1;

type
  AVSpeechSynthesisVoice = interface;

  AVSpeechSynthesisVoiceClass = interface(NSObjectClass)
  ['{A2006345-086C-4416-AAE7-3B1DD6B47BE1}']
    {class} function speechVoices: NSArray{<AVSpeechSynthesisVoice>}; cdecl;
    {class} function currentLanguageCode: NSString; cdecl;
    {class} function voiceWithLanguage(language: NSString): AVSpeechSynthesisVoice; cdecl;
  end;

  AVSpeechSynthesisVoice = interface(NSObject)
  ['{FBFD24DF-08F6-43A3-8A9B-32D583B0B8B5}']
    function language: NSString; cdecl;
  end;

  TAVSpeechSynthesisVoice = class(TOCGenericImport<AVSpeechSynthesisVoiceClass, AVSpeechSynthesisVoice>) end;

type
  AVSpeechUtterance = interface;

  AVSpeechUtteranceClass = interface(NSObjectClass)
  ['{E6695EAF-6909-4D1E-AFFA-DFB7CDC256EF}']
    {class} function speechUtteranceWithString(str: NSString): AVSpeechUtterance; cdecl;
  end;

  AVSpeechUtterance = interface(NSObject)
  ['{5D2DDD5B-688B-4193-B0F3-26C6C755AEDC}']
    function initWithString(str: NSString): AVSpeechUtterance; cdecl;
    function voice: AVSpeechSynthesisVoice; cdecl;
    procedure setVoice(voice: AVSpeechSynthesisVoice); cdecl;
    function speechString: NSString; cdecl;
    function rate: Single; cdecl;
    procedure setRate(rate: Single); cdecl;
    function pitchMultiplier: Single; cdecl;
    procedure setPitchMultiplier(pitchMultiplier: Single); cdecl;
    function volume: Single; cdecl;
    procedure setVolume(volume: Single); cdecl;
    function preUtteranceDelay: NSTimeInterval; cdecl;
    procedure setPreUtteranceDelay(preUtteranceDelay: NSTimeInterval); cdecl;
    function postUtteranceDelay: NSTimeInterval; cdecl;
    procedure setPostUtteranceDelay(postUtteranceDelay: NSTimeInterval); cdecl;
  end;

  TAVSpeechUtterance = class(TOCGenericImport<AVSpeechUtteranceClass, AVSpeechUtterance>) end;

type
  AVSpeechSynthesizer = interface;
  AVSpeechSynthesizerDelegate = interface;

  AVSpeechSynthesizerClass = interface(NSObjectClass)
  ['{4F761699-0210-47EB-802B-DAC900C9979B}']
  end;

  AVSpeechSynthesizer = interface(NSObject)
  ['{EC1850A7-B7EA-4C5D-A47B-D3EDDC3D4146}']
    function delegate: Pointer; cdecl;
    procedure setDelegate(delegate: AVSpeechSynthesizerDelegate); cdecl;
    function isSpeaking: Boolean; cdecl;
    function isPaused: Boolean; cdecl;
    procedure speakUtterance(utterance: AVSpeechUtterance); cdecl;
    function stopSpeakingAtBoundary(boundary: AVSpeechBoundary): Boolean; cdecl;
    function pauseSpeakingAtBoundary(boundary: AVSpeechBoundary): Boolean; cdecl;
    function continueSpeaking: Boolean; cdecl;
  end;

  TAVSpeechSynthesizer = class(TOCGenericImport<AVSpeechSynthesizerClass, AVSpeechSynthesizer>) end;

  AVSpeechSynthesizerDelegate = interface(IObjectiveC)
  ['{EF579B2B-6CB1-47E4-AD77-07F580876F8F}']
    [MethodName('speechSynthesizer:didStartSpeechUtterance:')]
    procedure speechSynthesizerDidStartSpeechUtterance(synthesizer: AVSpeechSynthesizer; utterance: AVSpeechUtterance); cdecl;

    [MethodName('speechSynthesizer:didFinishSpeechUtterance:')]
    procedure speechSynthesizerDidFinishSpeechUtterance(synthesizer: AVSpeechSynthesizer; utterance: AVSpeechUtterance); cdecl;

    [MethodName('speechSynthesizer:didCancelSpeechUtterance:')]
    procedure speechSynthesizerDidCancelSpeechUtterance(synthesizer: AVSpeechSynthesizer; utterance: AVSpeechUtterance); cdecl;
  end;

type
  { IgoSpeechToText implementation }
  TgoTextToSpeechImplementation = class(TgoTextToSpeechBase)
  {$REGION 'Internal Declarations'}
  private const
    { AVSpeechUtterance.Rate ranges from 0.0 to 1.0, where 0.5 is the default.
      On iOS 9 (and up?), the default right is fine.
      On iOS 8 and earlier, it is much too fast. }
    DEFAULT_SPEECH_RATE_IOS8_DOWN = 0.1;
  private type
    TDelegate = class(TOCLocal, AVSpeechSynthesizerDelegate)
    private
      [weak] FTextToSpeech: TgoTextToSpeechImplementation;
      FFireEvents: Boolean;
    public
      constructor Create(const ATextToSpeech: TgoTextToSpeechImplementation);
    public
      { AVSpeechSynthesizerDelegate }
      [MethodName('speechSynthesizer:didStartSpeechUtterance:')]
      procedure speechSynthesizerDidStartSpeechUtterance(synthesizer: AVSpeechSynthesizer; utterance: AVSpeechUtterance); cdecl;

      [MethodName('speechSynthesizer:didFinishSpeechUtterance:')]
      procedure speechSynthesizerDidFinishSpeechUtterance(synthesizer: AVSpeechSynthesizer; utterance: AVSpeechUtterance); cdecl;

      [MethodName('speechSynthesizer:didCancelSpeechUtterance:')]
      procedure speechSynthesizerDidCancelSpeechUtterance(synthesizer: AVSpeechSynthesizer; utterance: AVSpeechUtterance); cdecl;
    end;
  private
    FSpeechSynthesizer: AVSpeechSynthesizer;
    FDelegate: TDelegate;
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
  System.SysUtils,
  Macapi.Helpers;

{ TgoTextToSpeechImplementation }

constructor TgoTextToSpeechImplementation.Create;
begin
  inherited;
  FSpeechSynthesizer := TAVSpeechSynthesizer.Create;
  FDelegate := TgoTextToSpeechImplementation.TDelegate.Create(Self);
  FSpeechSynthesizer.setDelegate(FDelegate);
  Available := True;
end;

destructor TgoTextToSpeechImplementation.Destroy;
begin
  if (FSpeechSynthesizer <> nil) then
    FSpeechSynthesizer.release;
  inherited;
end;

function TgoTextToSpeechImplementation.IsSpeaking: Boolean;
begin
  Result := FSpeechSynthesizer.isSpeaking;
end;

function TgoTextToSpeechImplementation.Speak(const AText: String): Boolean;
var
  Utterance: AVSpeechUtterance;
begin
  if (AText.Trim = '') then
    Exit(True);

  if (FSpeechSynthesizer.isSpeaking) then
  begin
    { Calling stopSpeakingAtBoundary will also call
      speechSynthesizerDidCancelSpeechUtterance at some point. We don't want
      that event to fire here, so we set FFireEvents to False. That flag is
      set to True again when the next speech is started. }
    FDelegate.FFireEvents := False;
    FSpeechSynthesizer.stopSpeakingAtBoundary(AVSpeechBoundaryImmediate);
  end;

  Utterance := TAVSpeechUtterance.OCClass.speechUtteranceWithString(StrToNSStr(AText));
  if (not TOSVersion.Check(9)) then
    Utterance.setRate(DEFAULT_SPEECH_RATE_IOS8_DOWN);
  FSpeechSynthesizer.speakUtterance(Utterance);
  Result := True;
end;

procedure TgoTextToSpeechImplementation.Stop;
begin
  if (FSpeechSynthesizer.isSpeaking) then
    { This will also call speechSynthesizerDidCancelSpeechUtterance }
    FSpeechSynthesizer.stopSpeakingAtBoundary(AVSpeechBoundaryImmediate);
end;

{ TgoTextToSpeechImplementation.TDelegate }

constructor TgoTextToSpeechImplementation.TDelegate.Create(
  const ATextToSpeech: TgoTextToSpeechImplementation);
begin
  Assert(Assigned(ATextToSpeech));
  inherited Create;
  FTextToSpeech := ATextToSpeech;
end;

procedure TgoTextToSpeechImplementation.TDelegate.speechSynthesizerDidCancelSpeechUtterance(
  synthesizer: AVSpeechSynthesizer; utterance: AVSpeechUtterance);
begin
  if Assigned(FTextToSpeech) and (FFireEvents) then
    FTextToSpeech.DoSpeechFinished;
end;

procedure TgoTextToSpeechImplementation.TDelegate.speechSynthesizerDidFinishSpeechUtterance(
  synthesizer: AVSpeechSynthesizer; utterance: AVSpeechUtterance);
begin
  if Assigned(FTextToSpeech) and (FFireEvents) then
    FTextToSpeech.DoSpeechFinished;
end;

procedure TgoTextToSpeechImplementation.TDelegate.speechSynthesizerDidStartSpeechUtterance(
  synthesizer: AVSpeechSynthesizer; utterance: AVSpeechUtterance);
begin
  FFireEvents := True;
  if Assigned(FTextToSpeech) then
    FTextToSpeech.DoSpeechStarted;
end;

end.
