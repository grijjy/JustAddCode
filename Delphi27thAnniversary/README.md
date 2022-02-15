This week is Delphi's 27th anniversary and the team here at Grijjy is excited about all the great things that have happened over the past year in the Delphi world.  Just seeing the thousands of attendees this week for the Delphi 27th webinar was amazing, but we can really feel the growing momentum in the community as of late.  Delphi continues to add new capabilities and target new platforms making it the best solution for developing cross-platform native applications.

In honor of this occasion and to celebrate the 27th we decided to revisit a topic that is not often discussed, command-line parameters and unifying application startup settings and parameters.

# Introduction

Here at Grijjy we have built quite a few server applications in Delphi.  These applications run primarily as Windows x64 processes or services or Linux x64 processes, so in Delphi they start out as Console applications.   Most console applications, whether they are servers, services or just command-line utilities require initialization parameters or settings.

Delphi provides some basic command line helper routines, but here at Grijjy we like and prefer Linux style argv and argc command line arguments.  Parameters are preceded by a hyphen (ex: -runfast) and name/value pairs are separated by spaces. (ex: -name value) In order for us to take a unified approach to Windows and Linux, we create a simple helper unit that follows more closely to the Linux approach.  

Additionally, console applications might initialize with parameters from the command-line, but they may also derive their settings from a settings file on disk or from a remote location over the Internet.  In the past command-line parameters or settings contained in INI files was sufficient.  These days with the prevalence of cloud computing data centers, developers have to think about how they can make their modules elastic and containerized.  This means you need to consider reduce your initialization configuration requirements to the minimal amount so you can expand and contract your service dynamically without worrying about manually configuring initialization files and parameters.

We will discuss this and a framework for how to do this easily in Delphi to cover all the variations.

## Loading parameters from multiple sources

We shouldn't really be limited to loading startup settings or parameters from a single source, nor should be be dependent on any source specifically.  The concept is that initialization parameters could be located in multiple different locations (remote, disk or command line) and could override each other.  So for example you could have a centralized settings server for your server containers, but you might want to locally override some initialization parameters so you create a Settings file on disk or pass a parameter on the command-line.

At the heart of our framework for handling initialization parameters and settings is TSettings.  TSettings is a simple record that attempts to initialize parameters by cascading through various sources.  

TSettings exposes a methods including Load() which simply cascades through the various sources starting with remote, then command-line then disk.  You can optionally skip searching for initialization parameters remotely by not passing a Url parameter to the Load() method.

```pascal
  TSettings = record
  private
    class procedure ApplySettings; static;
  public
    { Loads settings locally based upon the module name

      Parameters:
        AFoldername: the root foldername under ProgramData

      Returns:
        True if the settings document was loaded, otherwise False }
    class function LoadFromDisk(const AFoldername: String = PROGRAM_DATA_FOLDERNAME): Boolean; static;

    { Loads settings from the remote Http Api for settings

      Returns:
        True if the settings document was loaded, otherwise False }
    class function LoadFromUrl: Boolean; static;

    { Loads settings from the command line }
    class procedure LoadFromCommandLine; static;

    { Apply any settings }
    class procedure Apply; static;

    { Loads settings from local sources only }
    class procedure Load; overload; static;

    { Loads settings from all sources
      Parameters:
        AUrl: the default listening url }
    class procedure Load(const AUrl: String = ''); overload; static;

    { Reloads settings }
    class procedure Reload; static;
  public
    { Command line helpers }

    { Returns True if the command line parameter exists and provides the index }
    class function Contains(const AParam: String; out AIndex: Integer): Boolean; overload; static;

    { Returns True if the command line parameter exists }
    class function Contains(const AParam: String): Boolean; overload; static;

    { Returns True if the command line parameter exists and provides the related value as a string }
    class function TryGet(const AParam: String; var AValue: String): Boolean; overload; static;

    { Returns True if the command line parameter exists and provides the related value as an integer }
    class function TryGet(const AParam: String; var AValue: Integer) : Boolean; overload; static;

    { Returns True if the command line parameter exists and provides the related value as an boolean }
    class function TryGet(const AParam: String; var AValue: Boolean) : Boolean; overload; static;
  end;
```

The actual initialization parameters or settings are handled as global variables, although you could take any desired approach.  In the Settings example unit we define the following example variables:

```pascal
var
  { Global command line settings }
  _global_string_setting: String;
  _global_tbytes_setting: TBytes;
  _global_boolean_setting: Boolean = True;
  _global_integer_setting: Integer;
```

This allows us to access our parameters anywhere in the entire project by adding the Settings.pas unit to our uses clause anywhere in the project where we need these parameters.  The initial Load() happens once in the main project unit.  There is also a Reload() if we change a setting locally or remotely and want to apply it at run-time.

### Loading parameters from disk

To load parameters from disk, TSettings.LoadFromDisk() defaults to loading a filename called ModuleName.settings located in the ProgramData folder on local computer.  ModuleName is the actual compiled process name.  The file should be a Json document with name and value pairs.   We utilize ProgramData because it a system-wide location available on Windows and Linux and accessible to Windows services in the System content, pre-logon.

The settings located in the file are loaded and applied to the global variables in the TSettings.ApplySettings routine.

```pascal
class procedure TSettings.ApplySettings;
var
  BsonValue: TgoBsonValue;
begin
  if FSettingsDoc.TryGetValue('_global_string_setting', BsonValue) and BsonValue.IsString then
    _global_string_setting := BsonValue;
  if FSettingsDoc.TryGetValue('_global_tbytes_setting', BsonValue) and BsonValue.IsBsonBinaryData then
    _global_tbytes_setting := BsonValue;
  if FSettingsDoc.TryGetValue('_global_boolean_setting', BsonValue) and BsonValue.IsBoolean then
    _global_boolean_setting := BsonValue;
  if FSettingsDoc.TryGetValue('_global_integer_setting', BsonValue) and BsonValue.IsInt64 then
    _global_integer_setting := BsonValue;
end;
```

Here we validate the name and value pairs in the Json are of the proper type and assign them to the global values.

### Loading parameters over http remotely

To load parameters remotely, TSettings.LoadFromUrl() defaults to loading the Json settings over Http.  If you don't need to load remotely, simply omit the Url parameter when calling Load().  For our example we recommend you implement the Http component of your choice.  Loading parameters and settings over Http allows us to handle our remote server or service without parameters making it easier to elastically scale in cloud compute data centers and modules or containers.  

```pascal
class function TSettings.LoadFromUrl: Boolean;
const
  METHOD_NAME = 'LoadFromUrl';
var
  ResponseDoc: TgoBsonDocument;
  LastAttempt: TDateTime;
begin
  Result := False;

  LastAttempt := 0;
  while True do
  begin
    if CtrlC then
      Break;

    if Now.SecondsBetween(LastAttempt) > INTERVAL_RETRY then { Retry }
    begin
      { Note: Use FUrl here to retrieve the Json settings document over Http.  You could use any
        Http component to fetch the document. Deserialize the http response body into
        the document ResponseDoc using TgoBsonDocument.Load related methods }

      LastAttempt := Now;
      if ResponseDoc.IsNil then
        Writeln(Format('Unable to retrieve settings from %s, trying again... [Error = Timeout]', [FUrl]));
    end
    else
      Sleep(25);
  end;
end;
```

### Loading parameters from the command-line

The method TSettings.LoadFromCommandLine() allows us to extract any command line parameters that might initialize or override those provided remotely.  The TSettings record contains methods such as Contains() to return True or False if a name is present (with or without a value) and TryGet() to extract the value associated with a name/value pair.

```pascal
class procedure TSettings.LoadFromCommandLine;
begin
  TryGet('-global_string_setting', _global_string_setting);
  TryGet('-global_boolean_setting', _global_boolean_setting);
  TryGet('-global_integer_setting', _global_integer_setting);
end;
```

You could call Contains() in your main unit initialization to make sure the minimum parameters exist from one of the various sources.

## The console application example

I like easy, simple and straightforward examples.  Ultimately your main DPR for your console application could be as simple as follows.  TSettings.Load() will attempt to initialize the settings and parameters from all sources.  From that point forward you can either use the global variables that represent the settings directly or use TSettings help methods such as Contains() to verify a parameter was provided.

```pascal
program ConsoleExample;
{ Example of console application with unified command line parameters and settings }

{$APPTYPE CONSOLE}

{$R *.res}

uses
  Grijjy.System.Console,
  Settings;

begin
  { Load settings from remote, command line and/or disk }
  TSettings.Load('https://mydomain.com/settings');

  { Run loop }
  Writeln('Press Ctrl-C to Exit');
  WaitForCtrlC;
end.
```

We included a new unit called [Grijjy.System.Console](https://github.com/grijjy/GrijjyFoundation/blob/master/Grijjy.System.Console.pas) that implements Ctrl-C process termination on Linux and Windows using a unified approach.

## Conclusion

We hope you enjoyed this discussion and example of how to model and implement initialization settings and parameters in a more flexible but straightforward way.

The example console app and supporting Settings unit are contained in our [JustAddCode](https://github.com/grijjy/JustAddCode/tree/master/Delphi27thAnniversary) repository. As always this project requires the [GrijjyFoundation](https://github.com/grijjy/GrijjyFoundation) installed and in your Delphi search path.