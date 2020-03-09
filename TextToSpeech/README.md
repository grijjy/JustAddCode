# Cross Platform Text-to-Speech

The code in this directory is a small exercise in designing a cross platform abstraction layer for platform-specific functionality. In particular, we present a small Delphi library to add cross platform text-to-speech to your app. It works on Windows, macOS, iOS and Android.

If you are only interested in the end result, then you can stick to the first part of this document and bail when we get to the implementation details.

# In this Fork, by oMAR mar20
* Add getVoices ( a list of voices available to Text-to-speech - This is returnes in a TStrings variable )
status: Ok for iOS, Android and Windows 
* Capture one male and one female voices, to allow 2 person dialogs.
* Set voices alternating, one line at a time
   ok for iOS and Android. Not working for Windows.
   Windows SAPI COM code needs fixing, for selection voices
Hard coded voice selection    
- for iOS there are one male and one female voices available in portuguese-BR
- for Android, there is a brasilian male voice and a spanish-mexican female that kinda make a funny couple :) 

## Choosing a feature set

A common issue with abstracting platform differences is that you must decide on a feature set. A specific feature may be supported on one platform, but not on another. When it comes to text-to-speech, some platforms support choosing a voice, changing the pitch or speech rate, customize pronunciation with markup in the text to speak etc. Other platforms may not support some of these features, or only in an incompatible way.

You can choose to go for the *lowest common denominator* approach and only expose those features that are supported on all platforms. Or you can choose to support more features which either do nothing on certain platforms or raise some kind of "not supported" exception. Or you can have a combination of both.

Also, there may be some features (or issues) on one platform that also affect the API for other platforms. For example, on all platforms except Android, you can start speaking immediately after you create the text-to-speech engine. On Android though, you have to wait until the engine has fully initialized in the background before you can use it. This means that we have to add some sort of notification to the engine to let clients know that the engine has initialized. On non-Android platforms, we just fire this event immediately after construction.

To keep the size of this document somewhat manageable, we only support the most basic text-to-speech features: speaking some text (using the default voice and settings) and stopping it. At the end of this post, you should be able to add other features yourself.

## IgoTextToSpeech API

In our blog post about [Cross Platform Abstraction](http://blog.grijjy.com/2017/01/06/cross-platform-abstraction/) we presented different ways to abstract platform-specific API differences. For the text-to-speech library we use the "object interface" approach as discussed in that post. The text-to-speech API is defined in an interface called `IgoTextToSpeech` (in the unit `Grijjy.TextToSpeech`):

```Delphi
type
  IgoTextToSpeech = interface
  ['{7797ED2A-0695-445A-BA84-495E280F86AB}']
    {$REGION 'Internal Declarations'}
    function _GetAvailable: Boolean;
    function _GetOnAvailable: TNotifyEvent;
    ..etc...
    {$ENDREGION 'Internal Declarations'}

    function Speak(const AText: String): Boolean;
    procedure Stop;
    function IsSpeaking: Boolean;

    property Available: Boolean read _GetAvailable;
    property OnAvailable: TNotifyEvent read _GetOnAvailable write _SetOnAvailable;
    property OnSpeechStarted: TNotifyEvent read _GetOnSpeechStarted write _SetOnSpeechStarted;
    property OnSpeechFinished: TNotifyEvent read _GetOnSpeechFinished write _SetOnSpeechFinished;
  end;
```

To speak some text, just call the `Speak` method and supply the text to speak. If the engine was already speaking some text, then the current speech will be terminated. This method is asynchronous and returns immediately while the text is spoken in the background. Once the engine actually starts to speak, it will fire the `OnSpeechStarted` event.

You can use `Stop` to stop speaking. Depending on the platform, this will stop speaking immediately or wait until the current word has been finished. The `OnSpeechFinished` event will be fired when the speech has stopped, either because there is no more text to speak, or you called `Stop` to terminate the speech.

Finally, you will note the `Available` property and `OnAvailable` event. As said above, on all platforms except Android, the speech engine will be available immediately. The `Available` property will be `True` and `OnAvailable` will be fired immediately after creating the engine.

On Android however, it takes time to initialize the engine and it may not be available immediately. You have to wait until the engine has fully initialized (by checking the `Available` property), or you can use the `OnAvailable` event to get notified when this happens.

In all cases, it is probably easiest and safest to always use the OnAvailable event (on all platforms) and don't speak any text until this event has been fired.

> You may have noticed that I started the names of all property/event getter and setter methods with an underscore (like `_GetAvailable`). This is to persuade the user of the interface to use the property/event instead of calling the method directly. These methods won't even show up in Code Insight if you prefix them with an underscore (although you can still call them if you insist).

You create an object that implements this interface by simply calling:

```Delphi
TextToSpeech := TgoTextToSpeech.Create;
```

This uses the *static class function* approach to create the platform-specific instance, as discussed in the [Cross Platform Abstraction](http://blog.grijjy.com/2017/01/06/cross-platform-abstraction/) post.

All in all not too complicated. Try out the [sample application](Example/) in the repository to see how it works on all platforms.

![Super Simple Text-to-Speech](https://bloggrijjy.files.wordpress.com/2017/01/texttospeechapp.png)

The remainder of this document discusses how text-to-speech is implemented on the various platforms. Feel free to skip this if you don't care about the details. However, if you are new to COM or using (and defining) Java classes and Objective-C classes, and want to learn a bit about it, then stick around.

## TgoTextToSpeechBase class

A base implementation of the `IgoTextToSpeech` interface is provided in the abstract `TgoTextToSpeechBase` class. This class provides the fields for the `Available` property and the various events, as well as helper methods to fire the events from the main thread (so you can update the UI from those events if you want to).

The actual API methods (`Speak`, `Stop` and `IsSpeaking`) are all `virtual` and `abstract` and overridden by the platform-specific derived classes.

## Text-to-Speech on Windows

On Windows, we use the Speech API to provide text-to-speech. This API is built using the Component Object Model (COM). Unfortunately, Delphi does not provide the translations of the Speech API header files. However, it is pretty easy to import them as a type library:

* In Delphi, pick the *"Component | Import Component..."* menu option.
* Select *"Import a Type Library"*.
* The next page shows all registered type libraries. There should be a *"Microsoft Speech Object Library"* in there.
* You can finish the wizard and create an import unit.

![Import the Speech API Type Library](https://bloggrijjy.files.wordpress.com/2017/01/importsapi.png)

Unfortunately the type library importer imports some declarations incorrectly, which can be especially problematic when used inside a 64-bit Windows app. So instead, I extracted the declarations we care about from the imported type library, fixed them and put them at the top of the `Grijjy.TextToSpeech.Windows` unit.

The main COM object for text-to-speech is exposed through the `ISpVoice` interface. You create this COM object with the following code:

```Delphi
FVoice := CreateComObject(CLASS_SpVoice) as ISpVoice;
```

Then we can speak some text using its `Speak` method:

```Delphi
function TgoTextToSpeechImplementation.Speak(const AText: String): Boolean;
begin
  if (FVoice = nil) then
    Result := False
  else
    Result := (FVoice.Speak(PWideChar(AText), SPF_ASYNC, nil) = S_OK);
end;
```

We pass the `SPF_ASYNC` flag so the method returns immediately and speaks the text in the background.

If we want to get notified when the system has finished speaking, then we need to subscribe to an event. We need to let the engine know which events we are interested in (through the `SetInterest` method) and how we should get notified (by calling the `SetNotifyCallbackFunction` method):

```Delphi
constructor TgoTextToSpeechImplementation.Create;
var
  Events: ULONGLONG;
begin
  inherited Create;
  FVoice := CreateComObject(CLASS_SpVoice) as ISpVoice;
  if (FVoice <> nil) then
  begin
    Events := SPFEI(SPEI_START_INPUT_STREAM) or SPFEI(SPEI_END_INPUT_STREAM);
    OleCheck(FVoice.SetInterest(Events, Events));
    OleCheck(FVoice.SetNotifyCallbackFunction(VoiceCallback, 0, NativeInt(Self)));
  end;
end;
```

> Note that `SetInterest` and `SetNotifyCallbackFunction` methods are not defined in `ISpVoice`. They are declared in parent interfaces of `JSpVoice`, called `ISpEventSource` and `ISpNotifySource` respectively.

Here we say we are interested in when the system starts and stops speaking (see the `Events` variable). We pass this variable twice to the `SetInterest` method. The first one is to tell the system what events we are interested in. The second one (which must be the same as or a subset of the first one) tells the system what events should be queued in the event queue. We need to enable this queuing because it is the only way to know *what* event has been fired.

The actual notification occurs by calling the function that is passed to the `SetNotifyCallbackFunction` method. This must be a `stdcall` function with the following signature:

```Delphi
class procedure TgoTextToSpeechImplementation.VoiceCallback(
  wParam: WPARAM; lParam: LPARAM);
begin
  TgoTextToSpeechImplementation(lParam).HandleVoiceEvent;
end;
```

> Note that there are other ways to be notified than using a callback function. For example, you can also have the engine send a Window Message, or you can create a "notification sink" by implementing the `ISpNotifySink` interface.

You can declare the callback as a global function, or you can make it a *static class method*. We choose the second approach here to keep things organized.

The callback receives two parameters, which are the same parameters as passed to the `SetNotifyCallbackFunction` API. We passed `Self` as the second parameter there so that we can access our object from the (global) callback function (as `lParam`). Here, the callback just forwards the notification to the `HandleVoiceEvent` method of our class:

```Delphi
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
```

This method just processes the event queue for start and finish notifications and fires the `OnSpeechStarted` and `OnSpeechFinished` events accordingly.

That covers the most important concepts on the Windows side.

## Text-to-Speech on macOS

On macOS, you will use the Objective-C (or Swift) class `NSSpeechSynthesizer`, which is declared in the unit `Macapi.AppKit`. This is a bit easier than creating a COM object on Windows:

```Delphi
constructor TgoTextToSpeechImplementation.Create;
begin
  inherited Create;
  FSpeechSynthesizer := TNSSpeechSynthesizer.Create;
end;

destructor TgoTextToSpeechImplementation.Destroy;
begin
  if (FSpeechSynthesizer <> nil) then
    FSpeechSynthesizer.release;
  inherited;
end;
```

Since we are creating a new instance here, we need to `release` it in the destructor.

Speaking some text is trivial:

```Delphi
function TgoTextToSpeechImplementation.Speak(const AText: String): Boolean;
begin
  if (FSpeechSynthesizer = nil) then
    Result := False
  else
    Result := FSpeechSynthesizer.startSpeakingString(StrToNSStr(AText));
end;
```

It just converts a Delphi string to a `NSString` and passes it to the `startSpeakingString` method.

Notifications work a bit different on macOS (and iOS). If you want to get notified about certain events (like when the engine has finished speaking), then you need to implement a delegate and assign it to the speech synthesizer.

The delegate you need to implement is called `NSSpeechSynthesizerDelegate`. Delphi choose not to provide translations for the methods in this delegate, so we have to do that ourselves. We can do so by deriving an interface from `NSSpeechSynthesizerDelegate` and add the notification methods we are interested in:

```Delphi
type
  NSSpeechSynthesizerDelegateEx = interface(NSSpeechSynthesizerDelegate)
  ['{D0AE9338-9D9B-4857-A404-255F40E4EB09}']
    [MethodName('speechSynthesizer:willSpeakPhoneme:')]
    procedure speechSynthesizerWillSpeakPhoneme(sender: NSSpeechSynthesizer;
      phonemeOpcode: Smallint); cdecl;

    [MethodName('speechSynthesizer:didFinishSpeaking:')]
    procedure speechSynthesizerDidFinishSpeaking(sender: NSSpeechSynthesizer;
      finishedSpeaking: Boolean); cdecl;
  end;
```

The delegate contains more methods than these, but these are the only ones we are interested in. (In Objective-C, delegates are *protocols* and often its methods are optional, so you only need to implement those you care about).

> You may note the `MethodName` attributes above. These are needed here because of the way Objective-C methods are named. In Objective-C, a method name includes the names of its parameters (after the first one). Usually, when these are translated to Delphi, we only translate the first part of the Objective-C method name to a Delphi method name, and make sure that the names of the parameters match those on the Objective-C side. That way, Delphi can link the Delphi method to the corresponding Objective-C method. In this case however, both methods start with the same text (`speechSynthesizer`), so we use the `MethodName` attribute to let Delphi now what the full name of the method is so it link it to the correct method.

Now, we need to implement this delegate inside a Delphi class. We do this by creating a class derived from `TOCLocal` and implement the delegate interface. In this case, we need to list both the `NSSpeechSynthesizerDelegate` and `NSSpeechSynthesizerDelegateEx` interfaces. In the sample code, the delegate is implemented in a private *nested* class:

```Delphi
type
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
  public
    ...etc...
  end;
```

We create and assign the delegate as follows:

```
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
```

The implementation of the delegate is fairly simple:

```Delphi
constructor TgoTextToSpeechImplementation.TDelegate.Create(
  const ATextToSpeech: TgoTextToSpeechImplementation);
begin
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
```

The only caveat here is that the `speechSynthesizerWillSpeakPhoneme` method is called multiple times per sentence, and we only want to send the `OnSpeechStarted` event once. We take care of this with an extra `Boolean` flag.

That's it for macOS.

## Text-to-Speech on iOS

You would expect that the iOS version would be very similar to the macOS version. To some degree it is. For example, you create a speech synthesizer (of type `AVSpeechSynthesizer` this time) and implement a delegate (`AVSpeechSynthesizerDelegate`).

> Delphi does not provide imports for these classes, so we imported them ourselves and added them to the top of the `Grijjy.TextToSpeech.iOS` unit.

But that is where the similarities end. To speak some text, you need to create a *speech utterance* and pass it to the synthesizer. Also, in contrast to macOS, you need to explicitly stop any current speech before you start some new speech. Otherwise, the new speech will be queued:

```Delphi
function TgoTextToSpeechImplementation.Speak(const AText: String): Boolean;
var
  Utterance: AVSpeechUtterance;
begin
  if (FSpeechSynthesizer.isSpeaking) then
    FSpeechSynthesizer.stopSpeakingAtBoundary(AVSpeechBoundaryImmediate);

  Utterance := TAVSpeechUtterance.OCClass.speechUtteranceWithString(StrToNSStr(AText));
  if (not TOSVersion.Check(9)) then
    Utterance.setRate(DEFAULT_SPEECH_RATE_IOS8_DOWN);
  FSpeechSynthesizer.speakUtterance(Utterance);
  Result := True;
end;
```

You'll also note some version specific code in there. An `AVSpeechUtterance` has a (speech) `Rate` property, that ranges from 0.0 to 1.0. The default 0.5 is supposed to represent a "normal" speech rate. Lower values slow down the speech and higher values speed it up. However, on iOS 8 and earlier the default of 0.5 is way too fast and we have to lower the value to about 0.1 to make it comparable to the default on iOS 9 and later. Don't you just love these version-specific idiosyncrasies!

Other than that, the code is similar to the macOS version.

## Text-to-Speech on Android

Finally, we consider the Android side of things. It's not too different from the macOS/iOS version. Instead of an Objective-C class we have a Java class here called `TextToSpeech` (which is implemented in Delphi through the `JTextToSpeech` interface).

And instead of a delegate, we implement a listener to get notified when speech has completed. This listener is a *nested* interface of the `TextToSpeech` class called `OnUtteranceCompletedListener`. Because it is a nested type, it is translated to Delphi using the fully qualified name `JTextToSpeech_OnUtteranceCompletedListener`.

> Note that the `JTextToSpeech` and related interfaces are only available in Delphi 10.1 Berlin (and later). For earlier versions of Delphi, you would have to import this interfaces yourself, preferably using [Java2OP](http://docwiki.embarcadero.com/RADStudio/Berlin/en/Java2OP.exe,_the_Native_Bridge_File_Generator_for_Android). We did this for you and added the interfaces we care about to the top of the `Grijjy.TextToSpeech.Android` unit.

But as you may remember from the beginning of this article, the engine works a bit differently on Android: the engine will not be immediately available. When you create a `JTextToSpeech` class on Android, you need to pass (another) listener that gets called when the engine has finished initialization. On the Delphi side, this means that you need to implement the `JTextToSpeech_OnInitListener` interface. You do that by deriving a class from `TJavaLocal` and implement the interface. We choose to do this in a nested type again:

```Delphi
type
  TgoTextToSpeechImplementation = class(TgoTextToSpeechBase)
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
  private
    FTextToSpeech: JTextToSpeech;
    FInitListener: TInitListener;
  public
    ...etc...
  end;
```

> You'll notice that in the listener class, I made the reference to the outer implementation class a `[weak]` reference. This is needed because the implementation class references the listener, and the listener class references the implementation class. Without this `[weak]` reference we would have a reference cycle and a memory leak. We have the same issue in the iOS version, but I sort of skipped over that detail in the discussion.

The listener just forwards the notification to the implementation class:

```Delphi
procedure TgoTextToSpeechImplementation.TInitListener.onInit(status: Integer);
begin
  if Assigned(FImplementation) then
    FImplementation.Initialize(status);
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
```

Here we release the listener (by assigning `nil`) because we don't need it anymore. If the engine was successfully initialized, we fire the `OnAvailable` event and further customize our engine. In particular:

* We create a (Java) hash map with a single item containing a (dummy) identifier for speech utterances. We pass this hash map later to the `speak` method. The only reason we do this is because the system will not send a *"utterance completed"* notification without this.
* We implement and create an *"utterance completed"* listener and assign it to the `JTextToSpeech` object. The implementation of this listener is trivial and not shown here.

Then we can finally speak some text:

```Delphi
function TgoTextToSpeechImplementation.Speak(const AText: String): Boolean;
begin
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
```

We pass the hash map we created before, as well as a `QUEUE_FLUSH` flag that is used to tell the engine to terminate any current speech.

