unit Grijjy.LineNumberInfo;
{< Utilities for loading line number information from a .gol file.
  Is used by the exception logger to retrieve line numbers from code addresses.
  Currently only used for Intel 64-bit macOS.

  Line number information files are generated by the Line Number Generator (LNG)
  tool in the Tools directory.

  The .gol file format is loosely modelled after the DWARF __debug_line section
  format:
  * 4 bytes: signature ('GOLN')
  * 4 bytes: version in $MMMMmmmm format. M=Major version, m=minor version.
    A change in minor version should not break the format.
    A reader cannot read a file with a major version that is larger then the
    one the reader is built for.
  * 4 bytes: total size of the .gol file (use for sanity checking)
  * 4 bytes: Number of line number entries in the file.
  * 16 bytes: GUID of the mach-o executable and dSYM that was used to generate
    the file.
  * 8 bytes: starting VM address.
  * 4 bytes: line number of starting VM address

  What follows is a simple state machine program with instructions to build a
  line number table. Each instruction starts with a single byte (Opcode) and can
  optionally have arguments.

  A simple compression algorithm is used that takes advantage of the facts that
  VM addresses always increase, and the difference between consecutive VM
  addresses is usually small. Similarly, it is efficient if the difference
  between two consecutive line numbers is small. A lot of the time, line numbers
  are increasing, but because of optimization and (compilation) unit changes,
  line numbers may also decrease.

  Opcodes 0-251 are used to update the address and line number, and add a new
  entry all at once. It is used when the difference between the new address and
  the previous address is at most 63, and the difference between the new line
  number and previous line number is between 1 and 4 (inclusive). The Opcode is
  than calculated as:

    Opcode := (LineAdvance - 1) + (AddressAdvance - 1) * 4

  So the mapping looks like:

    Opcode       LineAdvance      AddressAdvance
       0              1                 1
       1              2                 1
       2              3                 1
       3              4                 1
       4              1                 2
       5              2                 2
      ...
      247             4                 62
      248             1                 63
      249             2                 63
      250             3                 63
      251             4                 63

  Opcodes 252-255 have specific meaning. Only 252-254 are currently used:

  252 OP_ADVANCE_ADDR: adds the value 63 to the current address. Is used when
      the total address advance is between 63 and 126 and the line advance is
      between 1 and 4. This opcode should be followed by a regular opcode in
      the 0-247 range to set the final address and line.
  253 OP_ADVANCE_REL: takes 2 VarInt arguments:
      * the address advance. Increments the current address with this value +1
      * the line advance. Increments the current line with this value +1
      Adds an entry to the line number table with the new address and line.
  254 OP_ADVANCE_ABS: takes 2 VarInt arguments:
      * the address advance. Increments the current address with this value +1
      * the line number. Sets the current line to this value (*not +1)
      Adds an entry to the line number table with the new address and line.

  The VarInt format is identical to an unsigned LEB128 format in the DWARF
  specification. Basically, an unsigned integer value is split into chunks of
  7 bits. Each 7-bit chunk is encoded in a 8-bit byte, where the highest bit
  indicates if another 7-bit chunk follows. }

{$SCOPEDENUMS ON}

interface

const
  { Signature of line number files. }
  LINE_NUMBER_SIGNATURE = $4E4C4F47; // 'GOLN'

  { Current version of line number information. }
  LINE_NUMBER_VERSION   = $00010000;

const
  { Opcodes for the state machine program }
  OP_ADVANCE_ADDR = 252;
  OP_ADVANCE_REL  = 253;
  OP_ADVANCE_ABS  = 254;

type
  { .gol file header }
  TgoLineNumberHeader = packed record
  public
    { File signature. Should be LINE_NUMBER_SIGNATURE }
    Signature: UInt32;

    { File version.
      A change in minor version should not break the format.
      A reader cannot read a file with a major version that is larger then the
      one the reader is built for. }
    Version: UInt32;

    { Total size of the file }
    Size: UInt32;

    { Number of line number entries in the file }
    Count: Int32;

    { ID of the mach-o executable and dSYM that was used to generate the file. }
    ID: TGUID;

    { Starting VM address. }
    StartVMAddress: UInt64;

    { Line number of start address. }
    StartLine: UInt32;
  end;

type
  { Possible values for TgoLineNumberInfo.Status }
  TgoLineNumberStatus = (
    { Line number information is available }
    Available,

    { Line number information file (.gol) not found }
    FileNotFound,

    { Line number information file has invalid size (incomplete) }
    InvalidSize,

    { Line number information file has invalid signature }
    InvalidSignature,

    { Line number information file has unsupported version }
    UnsupportedVersion,

    { Line number information file does not match corresponding executable }
    ExecutableIDMismatch,

    { Line number information file is corrupt }
    FileCorrupt,

    { Exception occurred during loading of the file number information file }
    Exception);

type
  { Contains line number information for the currently loaded application. }
  TgoLineNumberInfo = class
  {$REGION 'Internal Declarations'}
  private type
    TLine = record
      RelAddress: UInt32;
      Line: Int32;
    end;
  private
    FBaseAddress: UInt64;
    FLines: TArray<TLine>;
    FStatus: TgoLineNumberStatus;
    function Load: TgoLineNumberStatus;
  private
    {$IF Defined(MACOS64) and not Defined(IOS)}
    class function ReadVarInt(var ABuf: PByte; const ABufEnd: PByte;
      out AValue: UInt32): Boolean; static;
    {$ENDIF}
  {$ENDREGION 'Internal Declarations'}
  public
    constructor Create;

    { Returns source line number corresponding to given address, or 0 if not
      available. }
    function Lookup(const AAddress: UIntPtr): Integer;

    { Status of the line number information file. }
    property Status: TgoLineNumberStatus read FStatus;
  end;

implementation

uses
  System.Generics.Defaults,
  System.Generics.Collections,
  {$IF Defined(MACOS64) and not Defined(IOS)}
  Grijjy.MachOApi,
  Macapi.Foundation,
  Macapi.Helpers,
  System.IOUtils,
  {$ENDIF}
  System.Classes,
  System.SysUtils;

{$IF Defined(MACOS64) and not Defined(IOS)}
const
  libDyld = '/usr/lib/system/libdyld.dylib';

function _dyld_get_image_header(image_index: UInt32): Pmach_header_64;
  cdecl; external libDyld;

function _dyld_get_image_vmaddr_slide(image_index: UInt32): UInt32;
  cdecl; external libDyld;

function GetID: TGUID;
var
  Header: Pmach_header_64;
  Cmd: Pload_command;
  Uuid: Puuid_command absolute Cmd;
  I: Integer;
begin
  Header := _dyld_get_image_header(0);
  Cmd := Pointer(UIntPtr(Header) + SizeOf(mach_header_64));

  for I := 0 to Header.ncmds - 1 do
  begin
    if (Cmd.cmd = LC_UUID) then
      Exit(TGUID(Uuid.uuid));
    Cmd := Pointer(UIntPtr(Cmd) + Cmd.cmdsize);
  end;
  Result := TGUID.Empty;
end;
{$ENDIF}


{ TgoLineNumberInfo }

constructor TgoLineNumberInfo.Create;
begin
  inherited;
  FStatus := Load;
end;

{$IF Defined(MACOS64) and not Defined(IOS)}
function TgoLineNumberInfo.Load: TgoLineNumberStatus;
var
  Filename: String;
  Stream: TFileStream;
  Header: TgoLineNumberHeader;
  Line: TLine;
  Lines: TArray<TLine>;
  StateMachineProgram: TBytes;
  PC, PCEnd: PByte;
  Opcode: Byte;
  I: Integer;
  A1, A2: UInt32;
  ContentsResourcesPath: String;
begin
  FBaseAddress := 0;
  FLines := nil;

  Filename := ParamStr(0) + '.gol';
  if (not FileExists(Filename)) then
  begin
    { MacOS only allows modules that are signable in the /Contents/MacOS/ path for signed/notarized apps }
    ContentsResourcesPath := NSStrToStr(TNSBundle.Wrap(TNSBundle.OCClass.mainBundle).bundlePath) + '/Contents/Resources/';
    Filename := ContentsResourcesPath + TPath.GetFileName(ParamStr(0)) + '.gol';
  end;

  if (not FileExists(Filename)) then
    Exit(TgoLineNumberStatus.FileNotFound);

  try
    Stream := TFileStream.Create(Filename, fmOpenRead or fmShareDenyWrite);
    try
      if (Stream.Read(Header, SizeOf(Header)) <> SizeOf(Header)) then
        Exit(TgoLineNumberStatus.InvalidSize);

      if (Header.Signature <> LINE_NUMBER_SIGNATURE) then
        Exit(TgoLineNumberStatus.InvalidSignature);

      if ((Header.Version shr 16) > (LINE_NUMBER_VERSION shr 16)) then
        Exit(TgoLineNumberStatus.UnsupportedVersion);

      if (Header.Size <> Stream.Size) then
        Exit(TgoLineNumberStatus.InvalidSize);

      if (Header.ID <> GetID) then
        Exit(TgoLineNumberStatus.ExecutableIDMismatch);

      SetLength(StateMachineProgram, Header.Size - SizeOf(Header));
      if (Stream.Read(StateMachineProgram[0], Length(StateMachineProgram)) <> Length(StateMachineProgram)) then
        Exit(TgoLineNumberStatus.InvalidSize);
    finally
      Stream.Free;
    end;

    FBaseAddress := Header.StartVMAddress + _dyld_get_image_vmaddr_slide(0);
    SetLength(Lines, Header.Count + 1);

    { Run state machine program }
    Line.RelAddress := 0;
    Line.Line := Header.StartLine;
    Lines[0] := Line;
    PC := @StateMachineProgram[0];
    PCEnd := @StateMachineProgram[Length(StateMachineProgram)];
    for I := 1 to Header.Count do
    begin
      if (PC >= PCEnd) then
        Exit(TgoLineNumberStatus.FileCorrupt);

      Opcode := PC^;
      Inc(PC);

      if (OpCode < OP_ADVANCE_ADDR) then
      begin
        { 1-byte opcode }
        Inc(Line.RelAddress, (OpCode shr 2) + 1);
        Inc(Line.Line, (OpCode and 3) + 1);
      end
      else case OpCode of
        OP_ADVANCE_ADDR:
          begin
            { 2-byte opcode }
            if (PC >= PCEnd) then
              Exit(TgoLineNumberStatus.FileCorrupt);

            Inc(Line.RelAddress, 63);
            Opcode := PC^;
            Inc(PC);

            if (Opcode < OP_ADVANCE_ADDR) then
            begin
              Inc(Line.RelAddress, (OpCode shr 2) + 1);
              Inc(Line.Line, (OpCode and 3) + 1);
            end
            else
              Exit(TgoLineNumberStatus.FileCorrupt);
          end;

        OP_ADVANCE_REL,
        OP_ADVANCE_ABS:
          begin
            { Updates are provided in 2 VarInt parameters }
            if (not ReadVarInt(PC, PCEnd, A1)) then
              Exit(TgoLineNumberStatus.FileCorrupt);

            if (not ReadVarInt(PC, PCEnd, A2)) then
              Exit(TgoLineNumberStatus.FileCorrupt);

            Inc(Line.RelAddress, A1 + 1);
            if (Opcode = OP_ADVANCE_REL) then
              Inc(Line.Line, A2 + 1)
            else
              Line.Line := A2;
          end;
      else
        Exit(TgoLineNumberStatus.FileCorrupt);
      end;

      Lines[I] := Line;
    end;

    FLines := Lines;
    Result := TgoLineNumberStatus.Available;
  except
    Result := TgoLineNumberStatus.Exception;
  end;
end;
{$ELSE}
function TgoLineNumberInfo.Load: TgoLineNumberStatus;
begin
  Result := TgoLineNumberStatus.FileNotFound;
end;
{$ENDIF}

function TgoLineNumberInfo.Lookup(const AAddress: UIntPtr): Integer;
var
  RelAddress, ActualAddress: UInt64;
  Line: TLine;
  Index: Integer;
begin
  if (FStatus <> TgoLineNumberStatus.Available) then
    Exit(0);

  if (AAddress < FBaseAddress) then
    Exit(0);

  RelAddress := AAddress - FBaseAddress;
  if (RelAddress > $FFFFFFFF) then
    Exit(0);

  Line.RelAddress := RelAddress;
  if (not TArray.BinarySearch<TLine>(FLines, Line, Index, TComparer<TLine>.Construct(
    function(const ALeft, ARight: TLine): Integer
    begin
      Result := ALeft.RelAddress - ARight.RelAddress;
    end)))
  then
    { When there is not an exact match, Index is set to the index in the array
      *before* which we would place the value. }
    Dec(Index);

  if (Index < 0) or (Index >= Length(FLines)) then
    Exit(0);

  { Check if address at this index is close enough to the requested address.
    If not, the resulting line number will be unreliable. This is common for
    addresses which were not available in the dSYM, for example for RTL/VCL/FMX
    addresses when the "Use debug .dcus" project option is not set. }
  ActualAddress := FBaseAddress + FLines[Index].RelAddress;
  if ((AAddress - ActualAddress) > 200) then
    Exit(0);

  Result := FLines[Index].Line;
end;

{$IF Defined(MACOS64) and not Defined(IOS)}
class function TgoLineNumberInfo.ReadVarInt(var ABuf: PByte; const ABufEnd: PByte;
  out AValue: UInt32): Boolean;
var
  Value: UInt32;
  Shift: Integer;
  B: Byte;
  P: PByte;
begin
  Value := 0;
  Shift := 0;
  P := ABuf;
  while (True) do
  begin
    if (P >= ABufEnd) then
      Exit(False);

    B := P^;
    Inc(P);

    Value := Value or ((B and $7F) shl Shift);
    if ((B and $80) = 0) then
      Break;

    Inc(Shift, 7);
  end;
  ABuf := P;
  AValue := Value;
  Result := True;
end;
{$ENDIF}

end.
