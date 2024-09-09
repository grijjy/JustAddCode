unit Grijjy.TextToSpeech.Windows;
{< Text To Speech engine implementation for Windows }

interface

uses
  Winapi.Windows,
  Winapi.ActiveX,

  System.Variants,
  System.SysUtils,

  System.Classes,  //TStrings
  Grijjy.TextToSpeech,
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


type  // Om: added
// for code from https://edn.embarcadero.com/article/29583#EnumVoices


// *********************************************************************//
// Interface: ISpeechObjectToken
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {C74A3ADC-B727-4500-A84A-B526721C8B8C}
// *********************************************************************//
  ISpeechObjectToken = interface(IDispatch)       //only using description
    ['{C74A3ADC-B727-4500-A84A-B526721C8B8C}']
    // function Get_Id: WideString; safecall;
    // function Get_DataKey: ISpeechDataKey; safecall;
    // function Get_Category: ISpeechObjectTokenCategory; safecall;
    function GetDescription(Locale: Integer): WideString; safecall;
    // procedure SetId(const Id: WideString; const CategoryID: WideString; CreateIfNotExist: WordBool); safecall;
    // function GetAttribute(const AttributeName: WideString): WideString; safecall;
    // function CreateInstance(const pUnkOuter: IUnknown; ClsContext: SpeechTokenContext): IUnknown; safecall;
    // procedure Remove(const ObjectStorageCLSID: WideString); safecall;
    // function GetStorageFileName(const ObjectStorageCLSID: WideString; const KeyName: WideString;
    //                             const FileName: WideString; Folder: SpeechTokenShellFolder): WideString; safecall;
    // procedure RemoveStorageFileName(const ObjectStorageCLSID: WideString;
    //                                 const KeyName: WideString; DeleteFile: WordBool); safecall;
    // function IsUISupported(const TypeOfUI: WideString; const ExtraData: OleVariant;
    //                        const Object_: IUnknown): WordBool; safecall;
    // procedure DisplayUI(hWnd: Integer; const Title: WideString; const TypeOfUI: WideString;
    //                     const ExtraData: OleVariant; const Object_: IUnknown); safecall;
    // function MatchesAttributes(const Attributes: WideString): WordBool; safecall;
    // property Id: WideString read Get_Id;
    // property DataKey: ISpeechDataKey read Get_DataKey;
    // property Category: ISpeechObjectTokenCategory read Get_Category;
  end;

// Om: added
// *********************************************************************//
// Interface: ISpeechObjectTokens
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {9285B776-2E7B-4BC0-B53E-580EB6FA967F}
// *********************************************************************//
  ISpeechObjectTokens = interface(IDispatch)
    ['{9285B776-2E7B-4BC0-B53E-580EB6FA967F}']
    function Get_Count: Integer; safecall;
    function Item(Index: Integer): ISpeechObjectToken; safecall;
    //function Get__NewEnum: IUnknown; safecall;
    property Count: Integer read Get_Count;
    //property _NewEnum: IUnknown read Get__NewEnum;
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

    fCOMVoice: OLEVariant;
  protected
    { IgoTextToSpeech }
    function getVoices(aList:TStrings):boolean; override;   // Om: mar20: get list of available voices ( only for iOS at this time)
    function getVoiceGender:TVoiceGender;       override;   // Om: mar20:
    function setVoice(const aMaleVoiceLang,aFemaleVoiceLang:String):boolean; override;  // Om: mar20: set voice w/ spec like 'pt-BR'


    function Speak(const AText: String): Boolean; override;
    procedure Stop; override;
    function IsSpeaking: Boolean; override;
  private

     class procedure VoiceCallback(wParam: WPARAM; lParam: LPARAM); stdcall; static;
     procedure HandleVoiceEvent;
     procedure getNativeVoices;
  {$ENDREGION 'Internal Declarations'}
  public
     fNativeVoice :OLEVariant;  //male and female voices
     fMaleVoice   :OLEVariant;
     fFemaleVoice :OLEVariant;

    constructor Create;
    destructor  Destroy; override;
  end;

implementation  //----------------------------------------------------------------------------

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

  fNativeVoice := varNull;
  fMaleVoice   := varNull;
  fFemaleVoice := varNull;

  //
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

    fCOMVoice := CreateOLEObject('SAPI.SpVoice');  //use OLE auto to get voices

    getNativeVoices;    //Om:

    Available := True;
  end
  else fCOMVoice := varNull; //?? no voice
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
  if (FVoice = nil) or (FVoice.GetStatus(Status, nil) <> S_OK) then  Result := False
  else Result := ((Status.dwRunningState and SPRS_IS_SPEAKING) <> 0) and ((Status.dwRunningState and SPRS_DONE) = 0);
end;

procedure TgoTextToSpeechImplementation.getNativeVoices;  // Om: mar20: get list of available voices ( only for iOS at this time)
var
  i: Integer;
  s:String;
  vozes:OLEVariant;
  aVoiceToken:OLEVariant;
begin
  fNativeVoice := varNull;
  fMaleVoice   := varNull;
  fFemaleVoice := varNull;

  if not VarIsEmpty( fCOMVoice ) then
    begin
       vozes := fCOMVoice.getVoices;
       for i := 0 to vozes.Count - 1 do
         begin
           aVoiceToken := vozes.item(i);
           s := Lowercase(aVoiceToken.GetDescription);
           // locate the best male and female voices available and memoise objects
           if      Pos('portuguese',s)>0 then
             begin
               fFemaleVoice := aVoiceToken;
               //aVoiceToken.AddRef;             // <-- necessary ??
             end
           else if Pos('english',s)>0    then
             begin
               fMaleVoice   := aVoiceToken;
               //aVoiceToken.AddRef;
             end;
         end;
    end;

  if not VarIsNull(fMaleVoice)   then
    fNativeVoice := fMaleVoice;     //any voice will do, but..
  if not VarIsNull(fFemaleVoice) then
    fNativeVoice := fFemaleVoice;   //.. default = female
end;

// from https://edn.embarcadero.com/article/29583#EnumVoices
function TgoTextToSpeechImplementation.getVoices(aList:TStrings):boolean;  // Om: mar20: get list of available voices ( only for iOS at this time)
var
  i: Integer; S:String;
  SOToken: OLEVariant;   //ISpeechObjectToken;
  SOTokens: OLEVariant;  //ISpeechObjectTokens;
begin
  // fVoice..EventInterests := SVEAllEvents;
  //Log('About to enumerate voices');
  Result := false;

  if VarIsNull(fCOMVoice) then exit;  //sanity check

  SOTokens := fCOMVoice.GetVoices('', '');   //
  for I := 0 to SOTokens.Count - 1 do
  begin
    //For each voice, store the descriptor in the TStrings list
    SOToken := SOTokens.Item(I);
    S := SOToken.GetDescription(0);
    aList.Add(S);
    // cbVoices.Items.AddObject(SOToken.GetDescription(0), TObject(SOToken));
    //Increment descriptor reference count to ensure it's not destroyed
    // SOToken._AddRef;
    Result := true;
  end;

  // aList.Add('------------------------');
  // aList.Add(fMaleVoice.GetDescription);    //test show saved voices
  // aList.Add(fFemaleVoice.GetDescription);

  // if cbVoices.Items.Count > 0 then
  // begin
  //   cbVoices.ItemIndex := 0; //Select 1st voice
  //   cbVoices.OnChange(cbVoices); //& ensure OnChange triggers
  // end;
  // Log('Enumerated voices');
  // Log('About to check attributes');
  // tbRate.Position := SpVoice.Rate;
  // lblRate.Caption := IntToStr(tbRate.Position);
  // tbVolume.Position := SpVoice.Volume;
  // lblVolume.Caption := IntToStr(tbVolume.Position);
  // Log('Checked attributes');
  //
end;


// getVoices() using dispatch interfaces
// var
//   i: Integer;
//   s:String;
//   voz:OLEVariant;
//   vozes:OLEVariant;
//   aVoiceToken:OLEVariant;
//
// begin
//   Result := false;
//   if Assigned(fVoice) then
//     begin
//        voz := CreateOLEObject('SAPI.SpVoice');  //use OLE auto to get voices
//        if not VarIsEmpty( voz ) then
//          begin
//            vozes := voz.getVoices;
//            for i := 0 to vozes.Count - 1 do
//              begin
//                aVoiceToken := vozes.item(i);
//                s := aVoiceToken.GetDescription;
//                aList.Add( s );
//                Result := true;
//              end;
//          end;
//     end;
// end;

// Om: mar20:

function TgoTextToSpeechImplementation.getVoiceGender:TVoiceGender;  // Om: mar20:
begin
  Result := vgUnkown;    // not implemented for windows yet
  // if not VarIsNull(fNativeVoice) then
  //    begin
  //      if      (not VarIsNull(fFemaleVoice) ) and (fNativeVoice.getDescription=fFemaleVoice.getDescription) then Result := vgFemale
  //      else if (not VarIsNull(fMaleVoice) )   and (fNativeVoice.getDescription=fMaleVoice.getDescription)   then Result := vgFemale;
  //    end;
end;

function TgoTextToSpeechImplementation.setVoice(const aMaleVoiceLang,aFemaleVoiceLang:String ):boolean;  // Om: mar20: set voice w/ spec like 'pt-BR'
begin
  Result := false;      // not implemented
  //TODO:
end;

function TgoTextToSpeechImplementation.Speak(const AText: String): Boolean;
var s:String;
begin
  if (FVoice = nil) then Result := False
 else begin
    // // alternating male-female voices
    // if not ( VarIsNull(fFemaleVoice) or  VarIsNull(fMaleVoice) ) then      //
    //   begin
    //     if (fNativeVoice.getDescription=fFemaleVoice.getDescription) then fNativeVoice:=fMaleVoice
    //       else fNativeVoice:=fFemaleVoice;
    //   end;

     // // commented voice selection. Neither of the SetVoice calls work

     // // set voice
     // if not ( VarIsNull(fNativeVoice) or VarIsNull(fCOMVoice)) then
     //   begin
     //     //fCOMVoice.SetVoice(fNativeVoice);       //nem um dos jeitos funcionou, desabilitei
     //      fVoice.SetVoice( fNativeVoice );   // this breaks the code
     //   end;


    Result := ( fVoice.Speak( PWideChar(AText), SPF_ASYNC, nil) = S_OK );  // do speak

    // if not VarIsNull(fNativeVoice) then  //test
    //   begin
    //     s:= fNativeVoice.getDescription;
    //     Result := (FVoice.Speak( PWideChar(s), SPF_ASYNC, nil) = S_OK);
    //   end;

  end;
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

//-----------------------------------------------------------

// from https://stackoverflow.com/questions/19369809/delphi-get-country-codes-by-localeid/37981772#37981772
function LCIDToLocaleName(Locale: LCID; lpName: LPWSTR; cchName: Integer;
  dwFlags: DWORD): Integer; stdcall;external kernel32 name 'LCIDToLocaleName';


function LocaleIDString():string;
var
   strNameBuffer : array [0..255] of WideChar; // 84 was len from original process online
   //localID : TLocaleID;
   // localID was 0, so didn't initialize, but still returned proper code page.
   // using 0 in lieu of localID : nets the same result, var not required.
   i : integer;
begin
  Result := '';

  // LOCALE_USER_DEFAULT  vs. LOCALE_SYSTEM_DEFAULT
  // since XP LOCALE_USER_DEFAULT is considered good practice for compatibility
  if (LCIDToLocaleName(LOCALE_USER_DEFAULT, strNameBuffer, 255, 0) > 0) then
    for i := 0 to 255 do
     begin
      if strNameBuffer[i] = #0 then  break
      else Result := Result + strNameBuffer[i];
    end;

  if (Length(Result) = 0) and (LCIDToLocaleName(0, strNameBuffer, 255, 0) > 0) then
   for i := 0 to 255 do
     begin
      if strNameBuffer[i] = #0 then break
      else Result := Result + strNameBuffer[i];
    end;

  if Length(Result) = 0 then
    Result := 'NR-NR' // defaulting to [No Reply - No Reply]
end;

procedure getNativeVoiceLanguage;
begin
  NativeSpeechLanguage := LocaleIDString; // 'pt-BR'
end;

initialization
   getNativeVoiceLanguage;     // get OS language settings
end.
