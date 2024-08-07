unit MachO;
{<Utilities for reading (debug) sections from a Mach-O file (such as a
  Intel macOS64 executable or .dSYM file).

  For an introduction to the Mach-O file format:
  * https://h3adsh0tzz.com/2020/01/macho-file-format/

  For the specification (no longer provided by Apple):
  * http://idea2ic.com/File_Formats/MachORuntime.pdf }

interface

uses
  System.Classes,
  System.SysUtils,
  System.Generics.Collections,
  Grijjy.MachOApi;

type
  { Type of exception raised for errors encountered during parsing of a Mach-O
    file. }
  EMachOError = class(Exception);

type
  { Represents a section in a Mach-O file }
  TSection = class
  {$REGION 'Internal Declarations'}
  private
    FStream: TStream;
    FSection: section_64;
    function GetSectionName: String;
    function GetSegmentName: String;
  protected
    constructor Create(const AStream: TStream; const ASection: section_64);
  {$ENDREGION 'Internal Declarations'}
  public
    function Load: TBytes;

    property SegmentName: String read GetSegmentName;
    property SectionName: String read GetSectionName;
  end;

type
  { Limited Mach-O file parser.
    Only supports Intel 64-bit macOS Mach-O files. }
  TMachOFile = class
  {$REGION 'Internal Declarations'}
  private
    FStream: TStream;
    FSections: TObjectList<TSection>;
    FID: TGUID;
    FOwnsStream: Boolean;
  {$ENDREGION 'Internal Declarations'}
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    procedure Load(const AFilename: String); overload;
    procedure Load(const AStream: TStream; const AOwnsStream: Boolean); overload;

    { Unique ID for this Mach-O file.
      An executable and corresponding dSYM must have the same ID. }
    property ID: TGUID read FID;

    property Sections: TObjectList<TSection> read FSections;
  end;

implementation

{ TSection }

constructor TSection.Create(const AStream: TStream;
  const ASection: section_64);
begin
  Assert(Assigned(AStream));
  inherited Create;
  FStream := AStream;
  FSection := ASection;
end;

function TSection.GetSectionName: String;
var
  Name: array [0..16] of AnsiChar;
begin
  Move(FSection.sectname, Name, 16);
  Name[16] := #0;
  Result := String(AnsiString(Name));
end;

function TSection.GetSegmentName: String;
var
  Name: array [0..16] of AnsiChar;
begin
  Move(FSection.segname, Name, 16);
  Name[16] := #0;
  Result := String(AnsiString(Name));
end;

function TSection.Load: TBytes;
begin
  if (FSection.offset = 0) or (FSection.size = 0) then
    Exit(nil);

  SetLength(Result, FSection.size);
  FStream.Position := FSection.offset;
  FStream.ReadBuffer(Result[0], FSection.size);
end;

{ TMachOFile }

procedure TMachOFile.Clear;
begin
  if (FOwnsStream) then
    FStream.Free;
  FStream := nil;
  FSections.Clear;
  FID := TGUID.Empty;
end;

constructor TMachOFile.Create;
begin
  inherited;
  FSections := TObjectList<TSection>.Create;
end;

destructor TMachOFile.Destroy;
begin
  FSections.Free;
  if (FOwnsStream) then
    FStream.Free;
  inherited;
end;

procedure TMachOFile.Load(const AStream: TStream; const AOwnsStream: Boolean);
var
  Header: mach_header_64;
  I, J: Integer;
  CmdPos: Int64;
  CmdHeader: load_command;
  SegCmd: segment_command_64;
  Section: section_64;
begin
  Assert(Assigned(AStream));
  Clear;
  FStream := AStream;
  FOwnsStream := AOwnsStream;
  AStream.ReadBuffer(Header, SizeOf(Header));
  if (Header.magic <> MH_MAGIC_64) then
    raise EMachOError.Create('Not a valid Mach-O file. Only 64-bit, little-endian Mach-O files are supported.');

  if (Header.cputype <> CPU_TYPE_X86_64) and (Header.cputype <> CPU_TYPE_ARM64) then
    raise EMachOError.Create('Only 64-bit Intel and ARM CPU types supported.');

  for I := 0 to Header.ncmds - 1 do
  begin
    CmdPos := AStream.Position;
    AStream.ReadBuffer(CmdHeader, SizeOf(CmdHeader));
    if (CmdHeader.cmdsize < SizeOf(CmdHeader)) then
      raise EMachOError.Create('Invalid load command size.');

    if (CmdHeader.cmd = LC_SEGMENT_64) then
    begin
      AStream.ReadBuffer(SegCmd.segname, SizeOf(SegCmd) - SizeOf(CmdHeader));
      if (CmdHeader.cmdsize <> (SizeOf(SegCmd) + (Cardinal(SegCmd.nsects) * SizeOf(Section)))) then
        raise EMachOError.Create('Invalid LC_SEGMENT_64 size');

      for J := 0 to SegCmd.nsects - 1 do
      begin
        AStream.ReadBuffer(Section, SizeOf(Section));
        FSections.Add(TSection.Create(AStream, Section));
      end;
    end
    else if (CmdHeader.cmd = LC_UUID) then
    begin
      if (CmdHeader.cmdsize <> (SizeOf(CmdHeader) + SizeOf(TGUID))) then
        raise EMachOError.Create('Invalid LC_UUID size');

      if (FID <> TGUID.Empty) then
        raise EMachOError.Create('Duplicate LC_UUID command');

      AStream.ReadBuffer(FID, SizeOf(FID));
    end;

    AStream.Position := CmdPos + CmdHeader.cmdsize;
  end;

  if (FID = TGUID.Empty) then
    raise EMachOError.Create('Missing LC_UUID command');
end;

procedure TMachOFile.Load(const AFilename: String);
begin
  Load(TFileStream.Create(AFilename, fmOpenRead or fmShareDenyWrite), True);
end;

end.
