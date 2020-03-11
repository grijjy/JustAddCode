unit Grijjy.TextToSpeech;
{< Universal Text To Speech for iOS, Android, Windows and macOS }

// Om: mar20: added fn getVoices - get list of available voices ( only for iOS at this time)

interface

uses
  System.Classes;

type

  TVoiceGender=(vgMale,vgFemale,vgUnkown);  //Om: Kinds of voices

  { Universal Text To Speech engine.
    Works on iOS, Android, Windows and macOS. Does nothing on other platforms.
    To create an instance, use TgoTextToSpeech.Create. }

  IgoTextToSpeech = interface
  ['{7797ED2A-0695-445A-BA84-495E280F86AB}']
    {$REGION 'Internal Declarations'}
    function _GetAvailable: Boolean;
    function _GetOnAvailable: TNotifyEvent;
    procedure _SetOnAvailable(const AValue: TNotifyEvent);
    function _GetOnSpeechFinished: TNotifyEvent;
    procedure _SetOnSpeechFinished(const AValue: TNotifyEvent);
    function _GetOnSpeechStarted: TNotifyEvent;
    procedure _SetOnSpeechStarted(const AValue: TNotifyEvent);
    {$ENDREGION 'Internal Declarations'}

    function  getVoices(aList:TStrings):boolean;         // Om: mar20: get list of available voices ( only for iOS at this time)
    function  getVoiceGender:TVoiceGender;               // Om: mar20:
    function  setVoice(const aVoiceLang:String):boolean; // Om: mar20: set voice w/ spec like 'pt' or 'en'  (lang-country)

    { Speaks a string of text.

      Parameters:
        AText: the text to speak.

      Returns:
        True if the engine can speak the text, or False if the text could not
        be spoken for some reason.

      If the engine is already speaking some text, then the current speech will
      be terminated (without calling OnSpeechFinished).

      This method is asynchronous. It returns immediately and speaks the text
      in the background. IsSpeaking will return False until the engine actually
      starts speaking.

      The OnSpeechStarted event is fired when the engine actually starts to
      speak. }
    function Speak(const AText: String): Boolean;

    { Stops speaking any current speech. If the engine was speaking, then
      OnSpeechFinished will be fired as well. }
    procedure Stop;

    { Whether the engine is currently speaking.

      Returns:
        True when the engine has started speaking (and OnSpeechStarted has been
          fired).
        False when the engine has finished speaking (and OnSpeechFinished has
          been fired). }
    function IsSpeaking: Boolean;

    { Whether the engine is available and can be used for speaking text (see
      Speak). Always returns True on Windows, macOS and iOS. On Android,
      returns False until the engine has been fully initialized (see also
      OnAvailable) }
    property Available: Boolean read _GetAvailable;

    { Is fired when the engine becomes available. Is fired immediately after
      construction on Windows, macOS and iOS. On Android, it is fired once the
      engine has been fully initialized and can be used for speaking.

      Is always fired in the main (UI) thread }
    property OnAvailable: TNotifyEvent read _GetOnAvailable write _SetOnAvailable;

    { Is fired when the engine starts speaking the text. This may be a little
      while after Speak has been called.

      Is always fired in the main (UI) thread }
    property OnSpeechStarted: TNotifyEvent read _GetOnSpeechStarted write _SetOnSpeechStarted;

    { Is fired when the engine has finished speaking the text, or when Stop is
      called while the engine was speaking.

      Is always fired in the main (UI) thread }
    property OnSpeechFinished: TNotifyEvent read _GetOnSpeechFinished write _SetOnSpeechFinished;
  end;

type
  { Class factory for IgoTextToSpeech. }
  TgoTextToSpeech = class // static
  public
    { Creates a Text To Speech engine. The engine will be initialized with the
      default voice/language for the user's locale.

      NOTE: On Android, the speech engine will not be available immediately.
      The Available property will return False until it becomes available. You
      can also set the OnAvailable event to get notified of this. }
    class function Create: IgoTextToSpeech; static;
  end;

implementation

uses
  {$IF Defined(IOS)}
  Grijjy.TextToSpeech.iOS;
  {$ELSEIF Defined(ANDROID)}
  Grijjy.TextToSpeech.Android;
  {$ELSEIF Defined(MSWINDOWS)}
  Grijjy.TextToSpeech.Windows;
  {$ELSEIF Defined(MACOS)}
  Grijjy.TextToSpeech.macOS;
  {$ELSE}
    {$MESSAGE Error 'Text-to-Speech not supported on this platform'}
  {$ENDIF}


{ TgoTextToSpeech }

class function TgoTextToSpeech.Create: IgoTextToSpeech;
begin
  Result := TgoTextToSpeechImplementation.Create;
end;

end.
