unit Grijjy.TextToSpeech.iOS;
{< Text To Speech engine implementation for iOS }

interface

uses
  System.Classes,      //Om: for TStrings
  Macapi.ObjectiveC,
  iOSapi.Foundation,
  iOSapi.CocoaTypes,
  iOSapi.AVFoundation,
  Grijjy.TextToSpeech,
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
    // Om: added the fns below
    function identifier: NSString; cdecl;   //Om: from https://github.com/FMXExpress/ios-object-pascal-wrapper/blob/master/iOSapi.AVFoundation.pas
    function name: NSString; cdecl;         //Om:
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

    fNativeVoice:AVSpeechSynthesisVoice;  //Om:

    fMaleVoice, fFemaleVoice: AVSpeechSynthesisVoice;

  protected
    Procedure getNativeVoice(const aVoiceLang:String);  // aVoiceSpec in format 'pt-BR'
    { IgoTextToSpeech }
    function getVoices(aList:TStrings):boolean; override;   // Om: mar20: get list of available voices ( only for iOS at this time)
    function getVoiceGender:TVoiceGender;       override;   // Om: mar20:
    function setVoice(const aVoiceLang:String):boolean; override;  // Om: mar20: set voice w/ spec like 'pt-BR'

    function  Speak(const AText: String): Boolean; override;
    procedure Stop; override;
    function  IsSpeaking: Boolean; override;
  {$ENDREGION 'Internal Declarations'}

  public
    constructor Create;
    destructor Destroy; override;
  end;

function getDeviceCountryCode:String;  //platform specific get country code
function getOSLanguage:String;


implementation //---------------------------------------------------

uses
  System.SysUtils,
  Macapi.Helpers;


// On iOS I could not find a way to tell male from female voices, so I made this little
// table with guesses  :(     Not sure about the genders for the names from exotic places
Type
  RVoiceGenderRec=record
    Lang:String;
    VoiceName:String;
    Gender:Char;
  end;

const
  Num_iOS_Voices=59;
  iOSVoiceGenders:Array[0..Num_iOS_Voices-1] of RVoiceGenderRec=(
    ( Lang: 'ar-SA'; VoiceName : 'Maged';                 Gender:'f'),
    ( Lang: 'cs-CZ'; VoiceName : 'Zuzana';                Gender:'f'),
    ( Lang: 'da-DK'; VoiceName : 'Sara';                  Gender:'f'),
    ( Lang: 'de-DE'; VoiceName : 'Anna';                  Gender:'f'),
    ( Lang: 'de-DE'; VoiceName : 'Helena';                Gender:'f'),
    ( Lang: 'de-DE'; VoiceName : 'Martin';                Gender:'m'),
    ( Lang: 'el-GR'; VoiceName : 'Melina';                Gender:'f'),
    ( Lang: 'en-AU'; VoiceName : 'Catherine';             Gender:'f'),
    ( Lang: 'en-AU'; VoiceName : 'Gordon';                Gender:'m'),
    ( Lang: 'en-AU'; VoiceName : 'Karen';                 Gender:'f'),
    ( Lang: 'en-GB'; VoiceName : 'Arthur';                Gender:'m'),
    ( Lang: 'en-GB'; VoiceName : 'Daniel';                Gender:'m'),
    ( Lang: 'en-GB'; VoiceName : 'Martha';                Gender:'f'),
    ( Lang: 'en-IE'; VoiceName : 'Moira';                 Gender:'f'),
    ( Lang: 'en-IN'; VoiceName : 'Rishi';                 Gender:'m'), //?
    ( Lang: 'en-US'; VoiceName : 'Aaron';                 Gender:'m'),
    ( Lang: 'en-US'; VoiceName : 'Fred';                  Gender:'m'),
    ( Lang: 'en-US'; VoiceName : 'Nicky';                 Gender:'f'),
    ( Lang: 'en-US'; VoiceName : 'Samantha';              Gender:'f'),
    ( Lang: 'en-ZA'; VoiceName : 'Tessa';                 Gender:'f'),
    ( Lang: 'es-ES'; VoiceName : 'Mónica';                Gender:'f'),
    ( Lang: 'es-MX'; VoiceName : 'Paulina';               Gender:'f'),
    ( Lang: 'fi-FI'; VoiceName : 'Satu';                  Gender:'m'),
    ( Lang: 'fr-CA'; VoiceName : 'Amélie';                Gender:'f'),
    ( Lang: 'fr-FR'; VoiceName : 'Daniel';                Gender:'m'),
    ( Lang: 'fr-FR'; VoiceName : 'Marie';                 Gender:'f'),
    ( Lang: 'fr-FR'; VoiceName : 'Thomas';                Gender:'m'),
    ( Lang: 'he-IL'; VoiceName : 'Carmit';                Gender:'m'),
    ( Lang: 'hi-IN'; VoiceName : 'Lekha';                 Gender:'f'),
    ( Lang: 'hu-HU'; VoiceName : 'Mariska';               Gender:'f'),
    ( Lang: 'id-ID'; VoiceName : 'Damayantict';           Gender:'m'),
    ( Lang: 'it-IT'; VoiceName : 'Alice';                 Gender:'f'),
    ( Lang: 'ja-JP'; VoiceName : 'Hattori';               Gender:'m'),
    ( Lang: 'ja-JP'; VoiceName : 'Kyoko';                 Gender:'f'),
    ( Lang: 'ja-JP'; VoiceName : 'O-ren';                 Gender:'m'),
    ( Lang: 'ko-KR'; VoiceName : 'Yuna';                  Gender:'f'),
    ( Lang: 'nl-BE'; VoiceName : 'Ellen';                 Gender:'f'),
    ( Lang: 'nl-NL'; VoiceName : 'Xander';                Gender:'m'),
    ( Lang: 'no-NO'; VoiceName : 'Nora';                  Gender:'f'),
    ( Lang: 'pl-PL'; VoiceName : 'Zosia';                 Gender:'f'),
    ( Lang: 'pt-BR'; VoiceName : 'Felipe (Aprimorado)';   Gender:'m'),
    ( Lang: 'pt-BR'; VoiceName : 'Luciana (Aprimorado)';  Gender:'f'),
    ( Lang: 'pt-BR'; VoiceName : 'Felipe';                Gender:'m'),
    ( Lang: 'pt-BR'; VoiceName : 'Luciana';               Gender:'f'),
    ( Lang: 'pt-PT'; VoiceName : 'Catarina (Aprimorado)'; Gender:'f'),
    ( Lang: 'pt-PT'; VoiceName : 'Catarina';              Gender:'f'),
    ( Lang: 'pt-PT'; VoiceName : 'Joana';                 Gender:'f'),
    ( Lang: 'ro-RO'; VoiceName : 'Ioana';                 Gender:'f'),
    ( Lang: 'ru-RU'; VoiceName : 'Milena';                Gender:'f'),
    ( Lang: 'sk-SK'; VoiceName : 'Laura';                 Gender:'f'),
    ( Lang: 'sv-SE'; VoiceName : 'Alva';                  Gender:'f'),
    ( Lang: 'th-TH'; VoiceName : 'Kanya';                 Gender:'f'),
    ( Lang: 'tr-TR'; VoiceName : 'Yelda';                 Gender:'f'),
    ( Lang: 'zh-CN'; VoiceName : 'Li-mu';                 Gender:'f'),
    ( Lang: 'zh-CN'; VoiceName : 'Tian-Tian';             Gender:'m'),
    ( Lang: 'zh-CN'; VoiceName : 'Yu-shu';                Gender:'f'),
    ( Lang: 'zh-HK'; VoiceName : 'Sin-Ji';                Gender:'f'),
    ( Lang: 'zh-TW'; VoiceName : 'Mei-Jia';               Gender:'m'),
    ( Lang: 'en-US'; VoiceName : 'Alex';                  Gender:'m') );

function getGenderOfName(const aName:String):Char;  // Name --> gender on iOS ( 'm' or 'f')
var i:integer;
begin
  Result := '?';  //unknown
  for i:=0 to Num_iOS_Voices-1 do
    if CompareText(iOSVoiceGenders[i].VoiceName,aName)=0 then  //found
      begin
         Result := iOSVoiceGenders[i].Gender;
         exit;
      end;
end;

function getDeviceCountryCode:String;
const FoundationFwk: string = '/System/Library/Frameworks/Foundation.framework/Foundation';
var
  CurrentLocale: NSLocale;
  CountryISO: NSString;
begin
  Result:='Unknown';

  CurrentLocale := TNSLocale.Wrap(TNSLocale.OCClass.currentLocale);
  CountryISO := TNSString.Wrap(CurrentLocale.objectForKey((CocoaNSStringConst(FoundationFwk, 'NSLocaleCountryCode') as ILocalObject).GetObjectID));
  Result := UTF8ToString(CountryISO.UTF8String);

  if (Length(Result)>2) then Delete(Result, 3, MaxInt);   //trim tail
end;

function getOSLanguage:String;
var
  Languages: NSArray;
begin
  Languages := TNSLocale.OCClass.preferredLanguages;
  Result := TNSString.Wrap(Languages.objectAtIndex(0)).UTF8String;
end;

{ TgoTextToSpeechImplementation }

constructor TgoTextToSpeechImplementation.Create;
begin
  inherited;
  FSpeechSynthesizer := TAVSpeechSynthesizer.Create;
  FDelegate := TgoTextToSpeechImplementation.TDelegate.Create(Self);
  FSpeechSynthesizer.setDelegate(FDelegate);
  Available := True;

  fNativeVoice := nil;       // not set yet
  // alternating mode: One line for the boy, one for the girl
  fMaleVoice   := nil;
  fFemaleVoice := nil;

  getNativeVoice('pt');   //on iOS, choose 'Luciana's'  pt-BR
end;

destructor TgoTextToSpeechImplementation.Destroy;
begin
  if (FSpeechSynthesizer <> nil) then
    FSpeechSynthesizer.release;
  inherited;
end;

// Om: mar20:
Procedure TgoTextToSpeechImplementation.getNativeVoice(const aVoiceLang:String);  // aVoiceLang in format 'pt'
var
  aLangArray:NSArray;
  aVoice:AVSpeechSynthesisVoice;
  i:integer;
  Slang,Sname:String;
begin
  fNativeVoice := nil;
  fMaleVoice   := nil;
  fFemaleVoice := nil;

  aLangArray := TAVSpeechSynthesisVoice.OCClass.speechVoices;  //get list of voices
  for i:=0 to aLangArray.count-1 do
    begin
      aVoice := TAVSpeechSynthesisVoice.Wrap( aLangArray.objectAtIndex(i) );
      Slang  := NSStrToStr( aVoice.language );
      Sname  := NSStrToStr( aVoice.name );

      if (Slang='pt-BR') then
        begin
           if ( Copy(Sname,1,7)='Luciana' ) then // '1234567'
             fFemaleVoice := aVoice;             // 'Luciana'    casuismos ! :(

           if ( Copy(Sname,1,6)='Felipe' )  then // '123456'
             fMaleVoice := aVoice;               // 'Felipe'
        end
    end;

  if Assigned(fMaleVoice)    then  fNativeVoice := fMaleVoice;     //any voice will do, but..
  if Assigned(fFemaleVoice)  then  fNativeVoice := fFemaleVoice;   //.. default = female
end;

// Om:
function TgoTextToSpeechImplementation.getVoices(aList: TStrings): boolean;
var
  aLangArray:NSArray;
  aVoice:AVSpeechSynthesisVoice;
  i:integer;
  Slang,Sname,SIdentifier:String;

begin
  Result     := false;
  aLangArray := TAVSpeechSynthesisVoice.OCClass.speechVoices;  //get list of voices
  for i:=0 to aLangArray.count-1 do
    begin
      aVoice := TAVSpeechSynthesisVoice.Wrap( aLangArray.objectAtIndex(i) );  //pode?

      Slang       := NSStrToStr( aVoice.language );     // 'pt-BR'
      Sname       := NSStrToStr( aVoice.name );         // 'Fred'
      SIdentifier := NSStrToStr( aVoice.identifier );   // 'com.apple.ttsbundle_fred-compact'

      aList.Add( IntToStr(i)+' '+Slang );
      aList.Add( Sname                  );
      aList.Add( SIdentifier            );

      //if (Slang='pt-PT') then
      //   fNativeVoice := aVoice;  //save native voice

      Result := true;
    end;

  aList.Add('current voice: '+ NSStrToStr( TAVSpeechSynthesisVoice.OCClass.currentLanguageCode ) );
end;

function TgoTextToSpeechImplementation.getVoiceGender:TVoiceGender;    // Om: mar20:
begin
  if    (fNativeVoice=fFemaleVoice) then Result := vgFemale
  else if (fNativeVoice=fMaleVoice) then Result := vgMale
  else Result := vgUnkown;
end;

function TgoTextToSpeechImplementation.setVoice(const aVoiceLang:String):boolean;  // Om: mar20: set voice w/ spec like 'pt-BR'
var
  aLangArray:NSArray;
  aVoice:AVSpeechSynthesisVoice;
  i:integer;
  Slang,Sname,sLangCode,sCountryCode:String;
  Sex:Char;   // 'f' or 'm'
begin
  fNativeVoice := nil;
  fMaleVoice   := nil;
  fFemaleVoice := nil;

  aLangArray := TAVSpeechSynthesisVoice.OCClass.speechVoices;  //get list of voices
  for i:=0 to aLangArray.count-1 do
    begin
      aVoice := TAVSpeechSynthesisVoice.Wrap( aLangArray.objectAtIndex(i) );
      Slang  := NSStrToStr( aVoice.language );    // 'pt-BR'
      Sname  := Trim(NSStrToStr( aVoice.name ));        // 'Maria'

      sLangCode    := Copy(Slang,1,2);   // 'pt'
      sCountryCode := Copy(Slang,4,2);   // 'BR'
      Sex          := getGenderOfName( Sname );

      if CompareText(sLang, aVoiceLang)=0 then  //found language
        begin
          if (Sex='f') then fFemaleVoice := aVoice
            else fMaleVoice := aVoice;

          // Omar: add hoc
          //  if ( Copy(Sname,1,7)='Luciana' ) then // '1234567'
          //    fFemaleVoice := aVoice;             // 'Luciana'    casuismos ! :(
          //
          //  if ( Copy(Sname,1,6)='Felipe' )  then // '123456'
          //    fMaleVoice := aVoice;               // 'Felipe'

        end
    end;

  if Assigned(fMaleVoice)    then  fNativeVoice := fMaleVoice;     //any voice will do, but..
  if Assigned(fFemaleVoice)  then  fNativeVoice := fFemaleVoice;   //.. default = female
end;

function TgoTextToSpeechImplementation.IsSpeaking: Boolean;
begin
  Result := FSpeechSynthesizer.isSpeaking;
end;

function TgoTextToSpeechImplementation.Speak(const AText: String): Boolean;
var
  Utterance: AVSpeechUtterance;
  aVoice:AVSpeechSynthesisVoice;  //AVSpeechSynthesisVoice;

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

  // Om: Use saved voice, if any
  if Assigned(fFemaleVoice) and Assigned(fMaleVoice) then      //alternating male-female voices
     begin
       if (fNativeVoice=fFemaleVoice) then fNativeVoice:=fMaleVoice
         else fNativeVoice:=fFemaleVoice;
     end;

  if Assigned(fNativeVoice) then
    Utterance.setVoice(fNativeVoice);

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
