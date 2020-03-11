unit Grijjy.TextToSpeech.Base;
{< Base class for text-to-speech implementations }

interface

uses
  System.Classes,
  Grijjy.TextToSpeech;

type
  { Base class that implements IgoTextToSpeech.
    Platform-specific implementations derive from this class. }
  TgoTextToSpeechBase = class abstract(TInterfacedObject, IgoTextToSpeech)
  {$REGION 'Internal Declarations'}
  private
    FAvailable: Boolean;
    FOnAvailable: TNotifyEvent;
    FOnSpeechStarted: TNotifyEvent;
    FOnSpeechFinished: TNotifyEvent;
  protected
    { IgoTextToSpeech }
    function _GetAvailable: Boolean;
    function _GetOnAvailable: TNotifyEvent;
    procedure _SetOnAvailable(const AValue: TNotifyEvent);
    function _GetOnSpeechFinished: TNotifyEvent;
    procedure _SetOnSpeechFinished(const AValue: TNotifyEvent);
    function _GetOnSpeechStarted: TNotifyEvent;
    procedure _SetOnSpeechStarted(const AValue: TNotifyEvent);

    function getVoices(aList:TStrings):boolean; virtual; abstract;   // Om: mar20: get list of available voices ( only for iOS at this time)
    function getVoiceGender:TVoiceGender;       virtual; abstract;   // Om: mar20:
    function setVoice(const aVoiceLang:String):boolean; virtual; abstract;  // Om: mar20: set voice w/ spec like 'pt'  (lang-country)

    function Speak(const AText: String): Boolean; virtual; abstract;
    procedure Stop; virtual; abstract;
    function IsSpeaking: Boolean; virtual; abstract;
  protected
    { Fires the FOn* events in the main thread }
    procedure DoAvailable;
    procedure DoSpeechStarted;
    procedure DoSpeechFinished;

    property Available: Boolean read FAvailable write FAvailable;
  {$ENDREGION 'Internal Declarations'}
  end;

implementation

{ TgoTextToSpeechBase }

procedure TgoTextToSpeechBase.DoAvailable;
begin
  if Assigned(FOnAvailable) then
  begin
    { Fire event from main thread. Use Queue instead of Synchronize to avoid
      blocking. }
    TThread.Queue(nil,
      procedure
      begin
        FOnAvailable(Self);
      end);
  end;
end;

procedure TgoTextToSpeechBase.DoSpeechFinished;
begin
  if Assigned(FOnSpeechFinished) then
  begin
    TThread.Queue(nil,
      procedure
      begin
        FOnSpeechFinished(Self);
      end);
  end;
end;

procedure TgoTextToSpeechBase.DoSpeechStarted;
begin
  if Assigned(FOnSpeechStarted) then
  begin
    TThread.Queue(nil,
      procedure
      begin
        FOnSpeechStarted(Self);
      end);
  end;
end;

function TgoTextToSpeechBase._GetAvailable: Boolean;
begin
  Result := FAvailable;
end;

function TgoTextToSpeechBase._GetOnAvailable: TNotifyEvent;
begin
  Result := FOnAvailable;
end;

function TgoTextToSpeechBase._GetOnSpeechFinished: TNotifyEvent;
begin
  Result := FOnSpeechFinished;
end;

function TgoTextToSpeechBase._GetOnSpeechStarted: TNotifyEvent;
begin
  Result := FOnSpeechStarted;
end;

procedure TgoTextToSpeechBase._SetOnAvailable(const AValue: TNotifyEvent);
begin
  FOnAvailable := AValue;
  if (FAvailable) then
    DoAvailable;
end;

procedure TgoTextToSpeechBase._SetOnSpeechFinished(const AValue: TNotifyEvent);
begin
  FOnSpeechFinished := AValue;
end;

procedure TgoTextToSpeechBase._SetOnSpeechStarted(const AValue: TNotifyEvent);
begin
  FOnSpeechStarted := AValue;
end;

end.
