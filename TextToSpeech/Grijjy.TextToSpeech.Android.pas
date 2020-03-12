unit Grijjy.TextToSpeech.Android;
{< Text To Speech engine implementation for Android }

// Om: prefix = changes by oMAR mar20

interface

uses
  System.Classes,  //Om: for TStrings
  FMX.Platform,    //Om: plat services

  Androidapi.JNIBridge,
  Androidapi.JNI.JavaTypes,


  {$IF RTLVersion >= 31}
  Androidapi.JNI.Speech,
  {$ELSE}
  Androidapi.JNI.GraphicsContentViewText,
  {$ENDIF}
  Grijjy.TextToSpeech,
  Grijjy.TextToSpeech.Base;

{$IF RTLVersion < 31}
{ Delphi 10 Seattle and ealier versions don't have imports for the Java
  Text-to-Speech classes. So we imported the parts we need ourselves using
  Java2OP. }

type
  JTextToSpeech_OnInitListenerClass = interface(IJavaClass)
    ['{B01450B5-524A-4B99-95DC-9158B7B8CC15}']
  end;

  [JavaSignature('android/speech/tts/TextToSpeech$OnInitListener')]
  JTextToSpeech_OnInitListener = interface(IJavaInstance)
    ['{94CC537C-E958-4EA5-B613-1465AEF6014B}']
    procedure onInit(status: Integer); cdecl;
  end;
  TJTextToSpeech_OnInitListener = class(TJavaGenericImport<JTextToSpeech_OnInitListenerClass, JTextToSpeech_OnInitListener>) end;

type
  JTextToSpeech_OnUtteranceCompletedListenerClass = interface(IJavaClass)
    ['{83D093B7-6FB6-46FE-A08E-1B0D25BDA841}']
  end;

  [JavaSignature('android/speech/tts/TextToSpeech$OnUtteranceCompletedListener')]
  JTextToSpeech_OnUtteranceCompletedListener = interface(IJavaInstance)
    ['{3EA0D21E-74E4-4204-A18F-2F68FE126E18}']
    procedure onUtteranceCompleted(utteranceId: JString); cdecl;
  end;
  TJTextToSpeech_OnUtteranceCompletedListener = class(TJavaGenericImport<JTextToSpeech_OnUtteranceCompletedListenerClass, JTextToSpeech_OnUtteranceCompletedListener>) end;

type
  JUtteranceProgressListener = interface;

  JUtteranceProgressListenerClass = interface(JObjectClass)
    ['{9D335A6E-78BE-4060-B3C1-6028E603073D}']
    {class} function init: JUtteranceProgressListener; cdecl;
  end;

  [JavaSignature('android/speech/tts/UtteranceProgressListener')]
  JUtteranceProgressListener = interface(JObject)
    ['{75D1A7E1-86E7-47D6-B9EC-96F0D69DC535}']
    procedure onDone(utteranceId: JString); cdecl;
    procedure onError(utteranceId: JString); cdecl;
    procedure onStart(utteranceId: JString); cdecl;
  end;
  TJUtteranceProgressListener = class(TJavaGenericImport<JUtteranceProgressListenerClass, JUtteranceProgressListener>) end;

type
  JTextToSpeech = interface;

  JTextToSpeechClass = interface(JObjectClass)
    ['{BE260883-0916-456E-B84C-6B237C8382DA}']
    {class} function _GetACTION_TTS_QUEUE_PROCESSING_COMPLETED: JString;
    {class} function _GetERROR: Integer;
    {class} function _GetLANG_AVAILABLE: Integer;
    {class} function _GetLANG_COUNTRY_AVAILABLE: Integer;
    {class} function _GetLANG_COUNTRY_VAR_AVAILABLE: Integer;
    {class} function _GetLANG_MISSING_DATA: Integer;
    {class} function _GetLANG_NOT_SUPPORTED: Integer;
    {class} function _GetQUEUE_ADD: Integer;
    {class} function _GetQUEUE_FLUSH: Integer;
    {class} function _GetSUCCESS: Integer;
    {class} function init(context: JContext; listener: JTextToSpeech_OnInitListener): JTextToSpeech; cdecl; overload;
    {class} function init(context: JContext; listener: JTextToSpeech_OnInitListener; engine: JString): JTextToSpeech; cdecl; overload;
//    {class} function getMaxSpeechInputLength: Integer; cdecl; { Requires Android 4.3 }
    {class} property ACTION_TTS_QUEUE_PROCESSING_COMPLETED: JString read _GetACTION_TTS_QUEUE_PROCESSING_COMPLETED;
    {class} property ERROR: Integer read _GetERROR;
    {class} property LANG_AVAILABLE: Integer read _GetLANG_AVAILABLE;
    {class} property LANG_COUNTRY_AVAILABLE: Integer read _GetLANG_COUNTRY_AVAILABLE;
    {class} property LANG_COUNTRY_VAR_AVAILABLE: Integer read _GetLANG_COUNTRY_VAR_AVAILABLE;
    {class} property LANG_MISSING_DATA: Integer read _GetLANG_MISSING_DATA;
    {class} property LANG_NOT_SUPPORTED: Integer read _GetLANG_NOT_SUPPORTED;
    {class} property QUEUE_ADD: Integer read _GetQUEUE_ADD;
    {class} property QUEUE_FLUSH: Integer read _GetQUEUE_FLUSH;
    {class} property SUCCESS: Integer read _GetSUCCESS;
  end;

  [JavaSignature('android/speech/tts/TextToSpeech')]
  JTextToSpeech = interface(JObject)
    ['{38B05C3C-B672-4FEC-849B-0CF4D89AA507}']
    function addEarcon(earcon: JString; packagename: JString; resourceId: Integer): Integer; cdecl; overload;
    function addEarcon(earcon: JString; filename: JString): Integer; cdecl; overload;
    function addSpeech(text: JString; packagename: JString; resourceId: Integer): Integer; cdecl; overload;
    function addSpeech(text: JString; filename: JString): Integer; cdecl; overload;
    function areDefaultsEnforced: Boolean; cdecl;
    function getDefaultEngine: JString; cdecl;
    function getDefaultLanguage: JLocale; cdecl;
    function getEngines: JList; cdecl;
    function getFeatures(locale: JLocale): JSet; cdecl;
    function getLanguage: JLocale; cdecl;
    function isLanguageAvailable(loc: JLocale): Integer; cdecl;
    function isSpeaking: Boolean; cdecl;
    function playEarcon(earcon: JString; queueMode: Integer; params: JHashMap): Integer; cdecl;
    function playSilence(durationInMs: Int64; queueMode: Integer; params: JHashMap): Integer; cdecl;
    function setEngineByPackageName(enginePackageName: JString): Integer; cdecl;//Deprecated
    function setLanguage(loc: JLocale): Integer; cdecl;
    function setOnUtteranceCompletedListener(listener: JTextToSpeech_OnUtteranceCompletedListener): Integer; cdecl;//Deprecated
    function setOnUtteranceProgressListener(listener: JUtteranceProgressListener): Integer; cdecl;
    function setPitch(pitch: Single): Integer; cdecl;
    function setSpeechRate(speechRate: Single): Integer; cdecl;
    procedure shutdown; cdecl;
    function speak(text: JString; queueMode: Integer; params: JHashMap): Integer; cdecl;
    function stop: Integer; cdecl;
    function synthesizeToFile(text: JString; params: JHashMap; filename: JString): Integer; cdecl;
  end;
  TJTextToSpeech = class(TJavaGenericImport<JTextToSpeechClass, JTextToSpeech>) end;

type
  JTextToSpeech_Engine = interface;

  JTextToSpeech_EngineClass = interface(JObjectClass)
    ['{75457E65-C0B1-4AF3-A166-A553887479C5}']
    {class} function _GetACTION_CHECK_TTS_DATA: JString;
    {class} function _GetACTION_GET_SAMPLE_TEXT: JString;
    {class} function _GetACTION_INSTALL_TTS_DATA: JString;
    {class} function _GetACTION_TTS_DATA_INSTALLED: JString;
    {class} function _GetCHECK_VOICE_DATA_BAD_DATA: Integer;
    {class} function _GetCHECK_VOICE_DATA_FAIL: Integer;
    {class} function _GetCHECK_VOICE_DATA_MISSING_DATA: Integer;
    {class} function _GetCHECK_VOICE_DATA_MISSING_VOLUME: Integer;
    {class} function _GetCHECK_VOICE_DATA_PASS: Integer;
    {class} function _GetDEFAULT_STREAM: Integer;
    {class} function _GetEXTRA_AVAILABLE_VOICES: JString;
    {class} function _GetEXTRA_CHECK_VOICE_DATA_FOR: JString;
    {class} function _GetEXTRA_SAMPLE_TEXT: JString;
    {class} function _GetEXTRA_TTS_DATA_INSTALLED: JString;
    {class} function _GetEXTRA_UNAVAILABLE_VOICES: JString;
    {class} function _GetEXTRA_VOICE_DATA_FILES: JString;
    {class} function _GetEXTRA_VOICE_DATA_FILES_INFO: JString;
    {class} function _GetEXTRA_VOICE_DATA_ROOT_DIRECTORY: JString;
    {class} function _GetINTENT_ACTION_TTS_SERVICE: JString;
    {class} function _GetKEY_FEATURE_EMBEDDED_SYNTHESIS: JString;
    {class} function _GetKEY_FEATURE_NETWORK_SYNTHESIS: JString;
    {class} function _GetKEY_PARAM_PAN: JString;
    {class} function _GetKEY_PARAM_STREAM: JString;
    {class} function _GetKEY_PARAM_UTTERANCE_ID: JString;
    {class} function _GetKEY_PARAM_VOLUME: JString;
    {class} function _GetSERVICE_META_DATA: JString;
    {class} function init: JTextToSpeech_Engine; cdecl;
    {class} property ACTION_CHECK_TTS_DATA: JString read _GetACTION_CHECK_TTS_DATA;
    {class} property ACTION_GET_SAMPLE_TEXT: JString read _GetACTION_GET_SAMPLE_TEXT;
    {class} property ACTION_INSTALL_TTS_DATA: JString read _GetACTION_INSTALL_TTS_DATA;
    {class} property ACTION_TTS_DATA_INSTALLED: JString read _GetACTION_TTS_DATA_INSTALLED;
    {class} property CHECK_VOICE_DATA_BAD_DATA: Integer read _GetCHECK_VOICE_DATA_BAD_DATA;
    {class} property CHECK_VOICE_DATA_FAIL: Integer read _GetCHECK_VOICE_DATA_FAIL;
    {class} property CHECK_VOICE_DATA_MISSING_DATA: Integer read _GetCHECK_VOICE_DATA_MISSING_DATA;
    {class} property CHECK_VOICE_DATA_MISSING_VOLUME: Integer read _GetCHECK_VOICE_DATA_MISSING_VOLUME;
    {class} property CHECK_VOICE_DATA_PASS: Integer read _GetCHECK_VOICE_DATA_PASS;
    {class} property DEFAULT_STREAM: Integer read _GetDEFAULT_STREAM;
    {class} property EXTRA_AVAILABLE_VOICES: JString read _GetEXTRA_AVAILABLE_VOICES;
    {class} property EXTRA_CHECK_VOICE_DATA_FOR: JString read _GetEXTRA_CHECK_VOICE_DATA_FOR;
    {class} property EXTRA_SAMPLE_TEXT: JString read _GetEXTRA_SAMPLE_TEXT;
    {class} property EXTRA_TTS_DATA_INSTALLED: JString read _GetEXTRA_TTS_DATA_INSTALLED;
    {class} property EXTRA_UNAVAILABLE_VOICES: JString read _GetEXTRA_UNAVAILABLE_VOICES;
    {class} property EXTRA_VOICE_DATA_FILES: JString read _GetEXTRA_VOICE_DATA_FILES;
    {class} property EXTRA_VOICE_DATA_FILES_INFO: JString read _GetEXTRA_VOICE_DATA_FILES_INFO;
    {class} property EXTRA_VOICE_DATA_ROOT_DIRECTORY: JString read _GetEXTRA_VOICE_DATA_ROOT_DIRECTORY;
    {class} property INTENT_ACTION_TTS_SERVICE: JString read _GetINTENT_ACTION_TTS_SERVICE;
    {class} property KEY_FEATURE_EMBEDDED_SYNTHESIS: JString read _GetKEY_FEATURE_EMBEDDED_SYNTHESIS;
    {class} property KEY_FEATURE_NETWORK_SYNTHESIS: JString read _GetKEY_FEATURE_NETWORK_SYNTHESIS;
    {class} property KEY_PARAM_PAN: JString read _GetKEY_PARAM_PAN;
    {class} property KEY_PARAM_STREAM: JString read _GetKEY_PARAM_STREAM;
    {class} property KEY_PARAM_UTTERANCE_ID: JString read _GetKEY_PARAM_UTTERANCE_ID;
    {class} property KEY_PARAM_VOLUME: JString read _GetKEY_PARAM_VOLUME;
    {class} property SERVICE_META_DATA: JString read _GetSERVICE_META_DATA;
  end;

  [JavaSignature('android/speech/tts/TextToSpeech$Engine')]
  JTextToSpeech_Engine = interface(JObject)
    ['{A876F830-EEA2-4A8E-B40D-B7AA567205EE}']
  end;
  TJTextToSpeech_Engine = class(TJavaGenericImport<JTextToSpeech_EngineClass, JTextToSpeech_Engine>) end;
{$ENDIF}

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
    FSpeechStarted: Boolean;

    fNativeVoice :JVoice;        // male and female voices
    fMaleVoice   :JVoice;
    fFemaleVoice :JVoice;

  private
    procedure Initialize(const AStatus: Integer);
    procedure getNativeVoices;
  protected
    { IgoTextToSpeech }
    function getVoices(aList:TStrings):boolean;          override;   // Om: mar20: get list of available voices ( only for iOS at this time)
    function getVoiceGender:TVoiceGender;                override;  // Om: mar20:
    function setVoice(const aMaleVoiceLang,aFemaleVoiceLang:String):boolean;  override; // Om: mar20: set voice w/ spec like 'pt-br'  (lang-country)

    function  Speak(const AText: String): Boolean; override;
    procedure Stop; override;
    function  IsSpeaking: Boolean; override;
  {$ENDREGION 'Internal Declarations'}
  public
    constructor Create;
  end;

// function getDeviceCountryCode:String;  //platform specific get country code


implementation  //-----------------------------------

uses
  System.SysUtils,
  Androidapi.Helpers;

function getDeviceCountryCode:String;  //platform specific get country code
var Locale: JLocale;
begin
  Result:='Unknown';

  Locale := TJLocale.JavaClass.getDefault;
  Result := JStringToString(Locale.getISO3Country);

  if Length(Result) > 2 then Delete(Result, 3, MaxInt);
end;

function getOSLanguage:String;
var LocServ: IFMXLocaleService;
begin
  if TPlatformServices.Current.SupportsPlatformService(IFMXLocaleService, IInterface(LocServ)) then
    Result := LocServ.GetCurrentLangID
    else Result := 'Unknown';
end;

{ TgoTextToSpeechImplementation }

constructor TgoTextToSpeechImplementation.Create;
begin
  inherited;
  FInitListener := TInitListener.Create(Self);
  FTextToSpeech := TJTextToSpeech.JavaClass.init(TAndroidHelper.Context, FInitListener);

  fNativeVoice := nil;       //not set yet
  fMaleVoice   := nil;
  fFemaleVoice := nil;
end;

// Om: mar20:
function TgoTextToSpeechImplementation.getVoices(aList: TStrings): boolean;
var aVoicesLst:JSet;
  it:Jiterator;
  v :JVoice;
  s :String;
  n :integer;

  vname,vlang,vcountry:String;

begin
  Result := false;
  aVoicesLst := FTextToSpeech.getVoices;
  it := aVoicesLst.iterator;
  n  :=0;
  while it.hasNext do
  begin
    inc(n);
    v := TJVoice.Wrap( it.next );

    vname    := jstringtostring( v.getName                );  //
    vlang    := jstringtostring( v.getLocale.getLanguage  );  // por
    vcountry := jstringtostring( v.getLocale.getCountry   );  // BRA

    s := IntToStr(n)  +' '+   // str descr of voice
         vname        +' '+   // tipo pt-BR-SMTm00
         vlang        +' '+   // por
         vcountry;            // BRA

    aList.Add( s );

    s := jstringtostring( v.toString );  // Voice[Name: en-US-SMTf00, locale:...
    aList.Add( s );

    Result := true;
  end;
end;

function TgoTextToSpeechImplementation.getVoiceGender:TVoiceGender;    // Om: mar20:
begin
  if    (fNativeVoice=fFemaleVoice) then Result := vgFemale
  else if (fNativeVoice=fMaleVoice) then Result := vgMale
  else Result := vgUnkown;
end;

function TgoTextToSpeechImplementation.setVoice(const aMaleVoiceLang,aFemaleVoiceLang:String):boolean;  // Om: mar20: set voice w/ spec like 'pt-BR'
var aVoicesLst:JSet;
    it:Jiterator;
    v :JVoice;
    vname,vlang,vcountry,aLangCode,Lang2:String;
    Sex:Char;

begin
  fNativeVoice := nil;
  fMaleVoice   := nil;
  fFemaleVoice := nil;

  aVoicesLst := FTextToSpeech.getVoices;
  it := aVoicesLst.iterator;

  while it.hasNext do
  begin
    v := TJVoice.Wrap( it.next );                 //  123456789.123
    vname     := jstringtostring( v.getName );    // 'es-MX-SMTf00'
    aLangCode := Copy(vname,1,5);                 // 'es-MX'
    Lang2     := Copy(vname,7,6);                 // 'SMTf00'
    if (Pos('f',Lang2)>0) then Sex:='f' else Sex:='m';  //extract gender from Lang2

    vlang    := jstringtostring( v.getLocale.getLanguage  );  // por
    vcountry := jstringtostring( v.getLocale.getCountry   );  // BRA

    if (CompareText(aLangCode,aFemaleVoiceLang)=0) and (Sex='f') then  //found language
      fFemaleVoice := v;
    if (CompareText(aLangCode,aMaleVoiceLang)=0) and (Sex='m') then  //found language
      fMaleVoice := v;

    /// if ( CompareText(vlang,'por')=0 ) and ( CompareText(vcountry,'BRA')=0 ) then
    //    fMaleVoice := v;    // CHECK: Can we save the inteface for latter use ?
    //  // Android não tem brazuka mulher. Usa a mexicana..
    //  if ( CompareText(vlang,'spa')=0 ) and ( CompareText(vcountry,'MEX')=0 ) then
    //    fFemaleVoice := v;
  end;

  if Assigned(fMaleVoice)    then  fNativeVoice := fMaleVoice;     //any voice will do, but..
  if Assigned(fFemaleVoice)  then  fNativeVoice := fFemaleVoice;   //.. default = female
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

    getNativeVoices;  //try to locate two suitable voices ( one male one female )
  end
  else FTextToSpeech := nil;
end;

procedure TgoTextToSpeechImplementation.getNativeVoices;  //Om:
var aVoicesLst:JSet;
    it:Jiterator;
    v :JVoice;
    vname,vlang,vcountry:String;

begin
  fNativeVoice := nil;
  fMaleVoice   := nil;
  fFemaleVoice := nil;

  aVoicesLst := FTextToSpeech.getVoices;
  it := aVoicesLst.iterator;

  while it.hasNext do
  begin
    v := TJVoice.Wrap( it.next );

    vname    := jstringtostring( v.getName                );  // es-MEX-SMTf00
    vlang    := jstringtostring( v.getLocale.getLanguage  );  // por
    vcountry := jstringtostring( v.getLocale.getCountry   );  // BRA

    //
    if ( CompareText(vlang,'por')=0 ) and ( CompareText(vcountry,'BRA')=0 ) then
      fMaleVoice := v;    // CHECK: Can we save the inteface for latter use ?
    // não tem brazuka mulher. Usa a mexicana..
    if ( CompareText(vlang,'spa')=0 ) and ( CompareText(vcountry,'MEX')=0 ) then
      fFemaleVoice := v;
  end;

  if Assigned(fMaleVoice)    then  fNativeVoice := fMaleVoice;     //any voice will do, but..
  if Assigned(fFemaleVoice)  then  fNativeVoice := fFemaleVoice;   //.. default = female
end;

function TgoTextToSpeechImplementation.IsSpeaking: Boolean;
begin
  Result := FSpeechStarted;
end;

function TgoTextToSpeechImplementation.Speak(const AText: String): Boolean;
begin
  if (AText.Trim = '') then
    Exit(True);

  if Assigned(FTextToSpeech) then
  begin

    // Om: Use saved voice, if any
    if Assigned(fFemaleVoice) and Assigned(fMaleVoice) then      //alternating male-female voices
       begin
         if (fNativeVoice=fFemaleVoice) then fNativeVoice:=fMaleVoice
           else fNativeVoice:=fFemaleVoice;
       end;

    if Assigned(fNativeVoice) then
      FTextToSpeech.setVoice( fNativeVoice );

    Result := (FTextToSpeech.speak(StringToJString(AText),
      TJTextToSpeech.JavaClass.QUEUE_FLUSH, FParams) = TJTextToSpeech.JavaClass.SUCCESS);
    if (Result) then
   begin
      FSpeechStarted := True;
      DoSpeechStarted;
    end;
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

constructor TgoTextToSpeechImplementation.TInitListener.Create(const AImplementation: TgoTextToSpeechImplementation);
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

procedure TgoTextToSpeechImplementation.TCompletedListener.onUtteranceCompleted( utteranceId: JString);
begin
  if Assigned(FImplementation) then
  begin
    FImplementation.FSpeechStarted := False;
    FImplementation.DoSpeechFinished;
  end;
end;

end.
