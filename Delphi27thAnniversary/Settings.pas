unit Settings;
{ Global settings unit }

{$INCLUDE 'Grijjy.inc'}

interface

uses
  System.SysUtils,
  System.IOUtils,
  System.DateUtils,
  Grijjy.Bson,
  Grijjy.System.Console;

const
  { ProgramData foldername }
  PROGRAM_DATA_FOLDERNAME = 'CompanyOrProductName';

  { Interval rate in seconds to retry }
  INTERVAL_RETRY = 3;

type
  TSettings = record
  private
    class var FUrl: String;
    class var FSettingsDoc: TgoBsonDocument;
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
    class procedure Load(const AUrl: String); overload; static;

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

var
  { Global command line settings }
  _global_string_setting: String;
  _global_tbytes_setting: TBytes;
  _global_boolean_setting: Boolean = True;
  _global_integer_setting: Integer;

implementation

{ TSettings }

class function TSettings.Contains(const AParam: String; out AIndex: Integer): Boolean;
var
  I: Integer;
begin
  for I := 0 to ParamCount do
    if ParamStr(I).ToLower = AParam.ToLower then
    begin
      AIndex := I;
      Exit(True);
    end;

  Result := False;
end;

class function TSettings.Contains(const AParam: String): Boolean;
var
  Index: Integer;
begin
  Result := Contains(AParam, Index);
end;

class function TSettings.TryGet(const AParam: String; var AValue: String): Boolean;
var
  Index: Integer;
begin
  if Contains(AParam, Index) and (Index < ParamCount) then
  begin
    AValue := ParamStr(Index + 1);
    Result := True;
  end
  else
    Result := False;
end;

class function TSettings.TryGet(const AParam: String; var AValue: Integer): Boolean;
var
  Index: Integer;
begin
  if Contains(AParam, Index) and (Index < ParamCount) then
  begin
    AValue := StrToIntDef(ParamStr(Index + 1), 0);
    Result := True;
  end
  else
    Result := False;
end;

class function TSettings.TryGet(const AParam: String; var AValue: Boolean): Boolean;
var
  Index: Integer;
begin
  if Contains(AParam, Index) and (Index < ParamCount) then
  begin
    AValue := StrToBoolDef(ParamStr(Index + 1), False);
    Result := True;
  end
  else
    Result := False;
end;

class function TSettings.LoadFromDisk(const AFoldername: String): Boolean;
var
  Filename, Path, PathToFilename: String;
  LastChar: Char;
begin
  Filename := TPath.GetFileNameWithoutExtension(ParamStr(0));
  Path := TPath.GetPublicPath;
  LastChar := Path[High(Path)]; { Workaround for Linux64 }
  if LastChar <> TPath.DirectorySeparatorChar then
    Path := Path + TPath.DirectorySeparatorChar;
  PathToFilename := Path + AFoldername + TPath.DirectorySeparatorChar + Filename + '.settings';
  if not TFile.Exists(PathToFilename) then
    Exit(False);

  try
    FSettingsDoc := TgoBsonDocument.LoadFromJsonFile(PathToFilename);
  except
    on e: exception do
    begin
      Writeln(Format('Error! Deserializing settings LoadFromDisk Exception (%s, %s)',
        [e.ClassName, e.Message]));
      Exit(False);
    end;
  end;

  ApplySettings;
  Result := True;
end;

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

class procedure TSettings.Apply;
begin
end;

class procedure TSettings.Load;
begin
  { Load settings from disk }
  LoadFromDisk;

  { Load settings from command line, if needed }
  LoadFromCommandLine;

  { Apply any settings }
  Apply;
end;

class procedure TSettings.Load(const AUrl: String);
begin
  { Default url }
  FUrl := AUrl;

  { Load settings from disk }
  LoadFromDisk;

  { Load settings from command line, if needed }
  LoadFromCommandLine;

  { Load settings from remote location over http }
  LoadFromUrl;

  { Apply any settings }
  Apply;
end;

class procedure TSettings.Reload;
begin
  { Load settings from disk }
  LoadFromDisk;

  { Load settings from command line, if needed }
  LoadFromCommandLine;

  { Load settings from remote location over http }
  LoadFromUrl;

  { Apply any settings }
  Apply;
end;

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

class procedure TSettings.LoadFromCommandLine;
begin
  TryGet('-global_string_setting', _global_string_setting);
  TryGet('-global_boolean_setting', _global_boolean_setting);
  TryGet('-global_integer_setting', _global_integer_setting);
end;

end.
