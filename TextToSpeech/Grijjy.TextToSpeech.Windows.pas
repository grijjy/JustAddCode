unit Grijjy.TextToSpeech.Windows;
{< Text To Speech engine implementation for Windows }

interface

uses
  Winapi.Windows,
  Winapi.ActiveX,
  Grijjy.TextToSpeech.Base;

{ Partial import of sapi.dll type library }

const
  CLASS_SpVoice: TGUID = '{96749377-3391-11D2-9EE3-00C04F797396}';

type
  SPVISEMES = TOleEnum;
  SPVPRIORITY = TOleEnum;
  SPEVENTENUM = TOleEnum;

type
  {$ALIGN 8}
  PSPEVENT = ^SPEVENT;
  SPEVENT = record
    eEventId: Word;
    elParamType: Word;
    ulStreamNum: ULONG;
    ullAudioStreamOffset: ULONGLONG;
    wParam: WPARAM;
    lParam: LPARAM;
  end;

type
  {$ALIGN 8}
  PSPEVENTSOURCEINFO = ^SPEVENTSOURCEINFO;
  SPEVENTSOURCEINFO = record
    ullEventInterest: ULONGLONG;
    ullQueuedInterest: ULONGLONG;
    ulCount: ULONG;
  end;

type
  {$ALIGN 4}
  PSPVOICESTATUS = ^SPVOICESTATUS;
  SPVOICESTATUS = record
    ulCurrentStream: ULONG;
    ulLastStreamQueued: ULONG;
    hrLastResult: HResult;
    dwRunningState: LongWord;
    ulInputWordPos: ULONG;
    ulInputWordLen: ULONG;
    ulInputSentPos: ULONG;
    ulInputSentLen: ULONG;
    lBookmarkId: LONG;
    PhonemeId: WideChar;
    VisemeId: SPVISEMES;
    dwReserved1: LongWord;
    dwReserved2: LongWord;
  end;

type
  SPNOTIFYCALLBACK = procedure (wParam: WPARAM; lParam: LPARAM); stdcall;

type
  // *********************************************************************//
  // Interface: ISpNotifySink
  // Flags:     (512) Restricted
  // GUID:      {259684DC-37C3-11D2-9603-00C04F8EE628}
  // *********************************************************************//
  ISpNotifySink = interface(IUnknown)
    ['{259684DC-37C3-11D2-9603-00C04F8EE628}']
    function Notify: HResult; stdcall;
  end;

type
  // *********************************************************************//
  // Interface: ISpNotifySource
  // Flags:     (512) Restricted
  // GUID:      {5EFF4AEF-8487-11D2-961C-00C04F8EE628}
  // *********************************************************************//
  ISpNotifySource = interface(IUnknown)
    ['{5EFF4AEF-8487-11D2-961C-00C04F8EE628}']
    function SetNotifySink(const pNotifySink: ISpNotifySink): HResult; stdcall;
    function SetNotifyWindowMessage(hWnd: HWND; Msg: UINT; wParam: WPARAM;
      lParam: LPARAM): HResult; stdcall;
    function SetNotifyCallbackFunction(pfnCallback: SPNOTIFYCALLBACK;
      wParam: WPARAM; lParam: LPARAM): HResult; stdcall;
    function SetNotifyCallbackInterface(pSpCallback: Pointer;
      wParam: WPARAM; lParam: LPARAM): HResult; stdcall;
    function SetNotifyWin32Event: HResult; stdcall;
    function WaitForNotifyEvent(dwMilliseconds: LongWord): HResult; stdcall;
    function GetNotifyEventHandle: Pointer; stdcall;
  end;

type
  // *********************************************************************//
  // Interface: ISpEventSource
  // Flags:     (512) Restricted
  // GUID:      {BE7A9CCE-5F9E-11D2-960F-00C04F8EE628}
  // *********************************************************************//
  ISpEventSource = interface(ISpNotifySource)
    ['{BE7A9CCE-5F9E-11D2-960F-00C04F8EE628}']
    function SetInterest(ullEventInterest: ULONGLONG; ullQueuedInterest: ULONGLONG): HResult; stdcall;
    function GetEvents(ulCount: ULONG; pEventArray: PSPEVENT; out pulFetched: ULONG): HResult; stdcall;
    function GetInfo(out pInfo: SPEVENTSOURCEINFO): HResult; stdcall;
  end;

type
  // *********************************************************************//
  // Interface: ISpVoice
  // Flags:     (512) Restricted
  // GUID:      {6C44DF74-72B9-4992-A1EC-EF996E0422D4}
  // *********************************************************************//
  ISpVoice = interface(ISpEventSource)
    ['{6C44DF74-72B9-4992-A1EC-EF996E0422D4}']
    function SetOutput(const pUnkOutput: IUnknown;
      fAllowFormatChanges: BOOL): HResult; stdcall;
    function GetOutputObjectToken(out ppObjectToken: IUnknown): HResult; stdcall;
    function GetOutputStream(out ppStream: IUnknown): HResult; stdcall;
    function Pause: HResult; stdcall;
    function Resume: HResult; stdcall;
    function SetVoice(const pToken: IUnknown): HResult; stdcall;
    function GetVoice(out ppToken: IUnknown): HResult; stdcall;
    function Speak(pwcs: LPCWSTR; dwFlags: LongWord;
      pulStreamNumber: PULONG): HResult; stdcall;
    function SpeakStream(const pStream: IUnknown; dwFlags: LongWord;
      out pulStreamNumber: LongWord): HResult; stdcall;
    function GetStatus(out pStatus: SPVOICESTATUS;
      ppszLastBookmark: PPWideChar): HResult; stdcall;
    function Skip(pItemType: LPCWSTR; lNumItems: Integer;
      out pulNumSkipped: ULONG): HResult; stdcall;
    function SetPriority(ePriority: SPVPRIORITY): HResult; stdcall;
    function GetPriority(out pePriority: SPVPRIORITY): HResult; stdcall;
    function SetAlertBoundary(eBoundary: SPEVENTENUM): HResult; stdcall;
    function GetAlertBoundary(out peBoundary: SPEVENTENUM): HResult; stdcall;
    function SetRate(RateAdjust: Integer): HResult; stdcall;
    function GetRate(out pRateAdjust: Integer): HResult; stdcall;
    function SetVolume(usVolume: Word): HResult; stdcall;
    function GetVolume(out pusVolume: Word): HResult; stdcall;
    function WaitUntilDone(msTimeout: LongWord): HResult; stdcall;
    function SetSyncSpeakTimeout(msTimeout: LongWord): HResult; stdcall;
    function GetSyncSpeakTimeout(out pmsTimeout: LongWord): HResult; stdcall;
    function SpeakCompleteEvent: Pointer; stdcall;
    function IsUISupported(pszTypeOfUI: PWideChar; pvExtraData: Pointer;
      cbExtraData: LongWord; out pfSupported: Integer): HResult; stdcall;
    function DisplayUI(hWndParent: HWND; pszTitle: PWideChar;
      pszTypeOfUI: PWideChar; pvExtraData: Pointer;
      cbExtraData: LongWord): HResult; stdcall;
  end;

const
  // SPEVENTENUM values
  SPEI_UNDEFINED               = $00000000;
  SPEI_START_INPUT_STREAM      = $00000001;
  SPEI_END_INPUT_STREAM        = $00000002;
  SPEI_VOICE_CHANGE            = $00000003;
  SPEI_TTS_BOOKMARK            = $00000004;
  SPEI_WORD_BOUNDARY           = $00000005;
  SPEI_PHONEME                 = $00000006;
  SPEI_SENTENCE_BOUNDARY       = $00000007;
  SPEI_VISEME                  = $00000008;
  SPEI_TTS_AUDIO_LEVEL         = $00000009;
  SPEI_TTS_PRIVATE             = $0000000F;
  SPEI_MIN_TTS                 = $00000001;
  SPEI_MAX_TTS                 = $0000000F;
  SPEI_END_SR_STREAM           = $00000022;
  SPEI_SOUND_START             = $00000023;
  SPEI_SOUND_END               = $00000024;
  SPEI_PHRASE_START            = $00000025;
  SPEI_RECOGNITION             = $00000026;
  SPEI_HYPOTHESIS              = $00000027;
  SPEI_SR_BOOKMARK             = $00000028;
  SPEI_PROPERTY_NUM_CHANGE     = $00000029;
  SPEI_PROPERTY_STRING_CHANGE  = $0000002A;
  SPEI_FALSE_RECOGNITION       = $0000002B;
  SPEI_INTERFERENCE            = $0000002C;
  SPEI_REQUEST_UI              = $0000002D;
  SPEI_RECO_STATE_CHANGE       = $0000002E;
  SPEI_ADAPTATION              = $0000002F;
  SPEI_START_SR_STREAM         = $00000030;
  SPEI_RECO_OTHER_CONTEXT      = $00000031;
  SPEI_SR_AUDIO_LEVEL          = $00000032;
  SPEI_SR_RETAINEDAUDIO        = $00000033;
  SPEI_SR_PRIVATE              = $00000034;
  SPEI_ACTIVE_CATEGORY_CHANGED = $00000035;
  SPEI_RESERVED5               = $00000036;
  SPEI_RESERVED6               = $00000037;
  SPEI_MIN_SR                  = $00000022;
  SPEI_MAX_SR                  = $00000037;
  SPEI_RESERVED1               = $0000001E;
  SPEI_RESERVED2               = $00000021;
  SPEI_RESERVED3               = $0000003F;

const
  // SPRUNSTATE flags
  SPRS_DONE        = 1 shl 0;
  SPRS_IS_SPEAKING = 1 shl 1;

const
  // SPEAKFLAGS flags
  SPF_DEFAULT          = 0;
  SPF_ASYNC            = 1 shl 0;
  SPF_PURGEBEFORESPEAK = 1 shl 1;
  SPF_IS_FILENAME      = 1 shl 2;
  SPF_IS_XML           = 1 shl 3;
  SPF_IS_NOT_XML       = 1 shl 4;
  SPF_PERSIST_XML      = 1 shl 5;
  SPF_NLP_SPEAK_PUNC   = 1 shl 6;
  SPF_PARSE_SAPI       = 1 shl 7;
  SPF_PARSE_SSML       = 1 shl 8;
  SPF_PARSE_AUTODETECT = 0;
  SPF_NLP_MASK         = SPF_NLP_SPEAK_PUNC;
  SPF_PARSE_MASK       = SPF_PARSE_SAPI or SPF_PARSE_SSML;
  SPF_VOICE_MASK       = SPF_ASYNC or SPF_PURGEBEFORESPEAK or SPF_IS_FILENAME
                      or SPF_IS_XML or SPF_IS_NOT_XML or SPF_NLP_MASK
                      or SPF_PERSIST_XML or SPF_PARSE_MASK;
  SPF_UNUSED_FLAGS     = not SPF_VOICE_MASK;

type
  { IgoSpeechToText implementation }
  TgoTextToSpeechImplementation = class(TgoTextToSpeechBase)
  {$REGION 'Internal Declarations'}
  private
    FVoice: ISpVoice;
  protected
    { IgoTextToSpeech }
    function Speak(const AText: String): Boolean; override;
    procedure Stop; override;
    function IsSpeaking: Boolean; override;
  private
     class procedure VoiceCallback(wParam: WPARAM; lParam: LPARAM); stdcall; static;
     procedure HandleVoiceEvent;
  {$ENDREGION 'Internal Declarations'}
  public
    constructor Create;
    destructor Destroy; override;
  end;

implementation

uses
  System.Win.ComObj;

{ These constants and functions come from sapi53.h from the Windows SDK }

const
  SPFEI_FLAGCHECK = (UInt64(1) shl SPEI_RESERVED1) or (UInt64(1) shl SPEI_RESERVED2);

function SPFEI(const AFlag: Longword): UInt64; inline;
begin
  Result := (UInt64(1) shl AFlag) or SPFEI_FLAGCHECK;
end;

{ TgoTextToSpeechImplementation }

constructor TgoTextToSpeechImplementation.Create;
var
  Events: ULONGLONG;
begin
  inherited Create;
  FVoice := CreateComObject(CLASS_SpVoice) as ISpVoice;
  if (FVoice <> nil) then
  begin
    { We want to be notified when speech synthesis has started and when it
      has stopped. }
    Events := SPFEI(SPEI_START_INPUT_STREAM) or SPFEI(SPEI_END_INPUT_STREAM);

    { Tell speech API what events we are interested in.
      * The first parameter tells the events we want to be notified about.
      * The second parameter tells which events should be queued in the event
        queue, so we can extract them later with GetEvents. We pass the same
        events here since we need to know which events were fired. }
    OleCheck(FVoice.SetInterest(Events, Events));

    { Tell speech API how to notify us. We use a callback mechanism here. }
    OleCheck(FVoice.SetNotifyCallbackFunction(VoiceCallback, 0, NativeInt(Self)));

    Available := True;
  end;
end;

destructor TgoTextToSpeechImplementation.Destroy;
begin
  if (FVoice <> nil) then
  begin
    { Remove callback to make sure this object doesn't get called anymore. }
    FVoice.SetNotifyCallbackFunction(nil, 0, 0);

    { According to MSDN documentation, we need to (also) call this to unregister
      the callback. }
    FVoice.SetNotifySink(nil);
  end;
  inherited;
end;

procedure TgoTextToSpeechImplementation.HandleVoiceEvent;
var
  Event: SPEVENT;
  NumEvents: ULONG;
begin
  if (FVoice = nil) then
    Exit;

  { Handle all events in the event queue.
    Before calling GetEvents, the Event record should be cleared. }
  FillChar(Event, SizeOf(Event), 0);
  while (FVoice.GetEvents(1, @Event, NumEvents) = S_OK) do
  begin
    case Event.eEventId of
      SPEI_START_INPUT_STREAM:
        DoSpeechStarted;

      SPEI_END_INPUT_STREAM:
        DoSpeechFinished;
    end;

    FillChar(Event, SizeOf(Event), 0);
  end;
end;

function TgoTextToSpeechImplementation.IsSpeaking: Boolean;
var
  Status: SPVOICESTATUS;
begin
  if (FVoice = nil) or (FVoice.GetStatus(Status, nil) <> S_OK) then
    Result := False
  else
    Result := ((Status.dwRunningState and SPRS_IS_SPEAKING) <> 0)
          and ((Status.dwRunningState and SPRS_DONE) = 0);
end;

function TgoTextToSpeechImplementation.Speak(const AText: String): Boolean;
begin
  if (FVoice = nil) then
    Result := False
  else
    Result := (FVoice.Speak(PWideChar(AText), SPF_ASYNC, nil) = S_OK);
end;

procedure TgoTextToSpeechImplementation.Stop;
var
  NumSkipped: ULONG;
begin
  if (FVoice <> nil) then
    FVoice.Skip('SENTENCE', MaxInt, NumSkipped);
end;

class procedure TgoTextToSpeechImplementation.VoiceCallback(wParam: WPARAM;
  lParam: LPARAM);
begin
  Assert(lParam <> 0);
  Assert(TObject(lParam) is TgoTextToSpeechImplementation);
  TgoTextToSpeechImplementation(lParam).HandleVoiceEvent;
end;

end.
