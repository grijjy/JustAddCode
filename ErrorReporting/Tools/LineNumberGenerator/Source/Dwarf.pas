unit Dwarf;
{>Some utilities for extracting line number information from DWARF debug
  sections in a MACH-O file (like the .dSYM files generated by the Intel MacOS64
  compiler for Delphi).

  For an introduction to the DWARD Debugging Format:
  * https://dwarfstd.org/doc/Debugging%20using%20DWARF-2012.pdf

  And the official specification:
  * https://dwarfstd.org/doc/DWARF5.pdf }

{$SCOPEDENUMS ON}

interface

uses
  System.Classes,
  System.SysUtils,
  System.Generics.Collections;

const
  DW_TAG_array_type             = $01;
  DW_TAG_class_type             = $02;
  DW_TAG_entry_point            = $03;
  DW_TAG_enumeration_type       = $04;
  DW_TAG_formal_parameter       = $05;
  DW_TAG_imported_declaration   = $08;
  DW_TAG_label                  = $0a;
  DW_TAG_lexical_block          = $0b;
  DW_TAG_member                 = $0d;
  DW_TAG_pointer_type           = $0f;
  DW_TAG_reference_type         = $10;
  DW_TAG_compile_unit           = $11;
  DW_TAG_string_type            = $12;
  DW_TAG_structure_type         = $13;
  DW_TAG_subroutine_type        = $15;
  DW_TAG_typedef                = $16;
  DW_TAG_union_type             = $17;
  DW_TAG_unspecified_parameters = $18;
  DW_TAG_variant                = $19;
  DW_TAG_common_block           = $1a;
  DW_TAG_common_inclusion       = $1b;
  DW_TAG_inheritance            = $1c;
  DW_TAG_inlined_subroutine     = $1d;
  DW_TAG_module                 = $1e;
  DW_TAG_ptr_to_member_type     = $1f;
  DW_TAG_set_type               = $20;
  DW_TAG_subrange_type          = $21;
  DW_TAG_with_stmt              = $22;
  DW_TAG_access_declaration     = $23;
  DW_TAG_base_type              = $24;
  DW_TAG_catch_block            = $25;
  DW_TAG_const_type             = $26;
  DW_TAG_constant               = $27;
  DW_TAG_enumerator             = $28;
  DW_TAG_file_type              = $29;
  DW_TAG_friend                 = $2a;
  DW_TAG_namelist               = $2b;
  DW_TAG_namelist_item          = $2c;
  DW_TAG_packed_type            = $2d;
  DW_TAG_subprogram             = $2e;
  DW_TAG_template_type_param    = $2f;
  DW_TAG_template_value_param   = $30;
  DW_TAG_thrown_type            = $31;
  DW_TAG_try_block              = $32;
  DW_TAG_variant_part           = $33;
  DW_TAG_variable               = $34;
  DW_TAG_volatile_type          = $35;
  DW_TAG_lo_user                = $4080;
  DW_TAG_hi_user                = $ffff;

const
  DW_CHILDREN_no  = 0;
  DW_CHILDREN_yes = 1;

const
  DW_AT_sibling              = $01; // reference
  DW_AT_location             = $02; // block, constant
  DW_AT_name                 = $03; // string
  DW_AT_ordering             = $09; // constant
  DW_AT_byte_size            = $0b; // constant
  DW_AT_bit_offset           = $0c; // constant
  DW_AT_bit_size             = $0d; // constant
  DW_AT_stmt_list            = $10; // constant
  DW_AT_low_pc               = $11; // address
  DW_AT_high_pc              = $12; // address
  DW_AT_language             = $13; // constant
  DW_AT_discr                = $15; // reference
  DW_AT_discr_value          = $16; // constant
  DW_AT_visibility           = $17; // constant
  DW_AT_import               = $18; // reference
  DW_AT_string_length        = $19; // block, constant
  DW_AT_common_reference     = $1a; // reference
  DW_AT_comp_dir             = $1b; // string
  DW_AT_const_value          = $1c; // string, constant, block
  DW_AT_containing_type      = $1d; // reference
  DW_AT_default_value        = $1e; // reference
  DW_AT_inline               = $20; // constant
  DW_AT_is_optional          = $21; // flag
  DW_AT_lower_bound          = $22; // constant, reference
  DW_AT_producer             = $25; // string
  DW_AT_prototyped           = $27; // flag
  DW_AT_return_addr          = $2a; // block, constant
  DW_AT_start_scope          = $2c; // constant
  DW_AT_stride_size          = $2e; // constant
  DW_AT_upper_bound          = $2f; // constant, reference
  DW_AT_abstract_origin      = $31; // reference
  DW_AT_accessibility        = $32; // constant
  DW_AT_address_class        = $33; // constant
  DW_AT_artificial           = $34; // flag
  DW_AT_base_types           = $35; // reference
  DW_AT_calling_convention   = $36; // constant
  DW_AT_count                = $37; // constant, reference
  DW_AT_data_member_location = $38; // block, reference
  DW_AT_decl_column          = $39; // constant
  DW_AT_decl_file            = $3a; // constant
  DW_AT_decl_line            = $3b; // constant
  DW_AT_declaration          = $3c; // flag
  DW_AT_discr_list           = $3d; // block
  DW_AT_encoding             = $3e; // constant
  DW_AT_external             = $3f; // flag
  DW_AT_frame_base           = $40; // block, constant
  DW_AT_friend               = $41; // reference
  DW_AT_identifier_case      = $42; // constant
  DW_AT_macro_info           = $43; // constant
  DW_AT_namelist_item        = $44; // block
  DW_AT_priority             = $45; // reference
  DW_AT_segment              = $46; // block, constant
  DW_AT_specification        = $47; // reference
  DW_AT_static_link          = $48; // block, constant
  DW_AT_type                 = $49; // reference
  DW_AT_use_location         = $4a; // block, constant
  DW_AT_variable_parameter   = $4b; // flag
  DW_AT_virtuality           = $4c; // constant
  DW_AT_vtable_elem_location = $4d; // block, reference
  DW_AT_lo_user              = $2000;
  DW_AT_hi_user              = $3fff;

const
  DW_FORM_addr         = $01; // address
  DW_FORM_block2       = $03; // block
  DW_FORM_block4       = $04; // block
  DW_FORM_data2        = $05; // constant
  DW_FORM_data4        = $06; // constant
  DW_FORM_data8        = $07; // constant
  DW_FORM_string       = $08; // string
  DW_FORM_block        = $09; // block
  DW_FORM_block1       = $0a; // block
  DW_FORM_data1        = $0b; // constant
  DW_FORM_flag         = $0c; // flag
  DW_FORM_sdata        = $0d; // constant
  DW_FORM_strp         = $0e; // string
  DW_FORM_udata        = $0f; // constant
  DW_FORM_ref_addr     = $10; // reference
  DW_FORM_ref1         = $11; // reference
  DW_FORM_ref2         = $12; // reference
  DW_FORM_ref4         = $13; // reference
  DW_FORM_ref8         = $14; // reference
  DW_FORM_ref_udata    = $15; // reference
  DW_FORM_indirect     = $16; // (see section 7.5.3)
  DW_FORM_sec_offset   = $17; // lineptr, loclistptr, macptr, rangelistptr
  DW_FORM_exprloc      = $18; // exprloc
  DW_FORM_flag_present = $19; // flag
  DW_FORM_ref_sig8     = $20; // reference

const
  DW_LANG_C89         = $0001;
  DW_LANG_C           = $0002;
  DW_LANG_Ada83       = $0003;
  DW_LANG_C_plus_plus = $0004;
  DW_LANG_Cobol74     = $0005;
  DW_LANG_Cobol85     = $0006;
  DW_LANG_Fortran77   = $0007;
  DW_LANG_Fortran90   = $0008;
  DW_LANG_Pascal83    = $0009;
  DW_LANG_Modula2     = $000a;
  DW_LANG_lo_user     = $8000;
  DW_LANG_hi_user     = $ffff;

type
  EDwarfError = class(Exception);

type
  TDwarfCursor = record
  {$REGION 'Internal Declarations'}
  private
    FCur: PByte;
    FEnd: PByte;
  private
    class procedure ReadError; static;
  private
  {$ENDREGION 'Internal Declarations'}
  public
    class operator Implicit(const ABytes: TBytes): TDwarfCursor; static;

    function Eof: Boolean; inline;
    procedure Seek(const APtr: PByte); overload; inline;
    procedure Require(const ANumBytes: Integer); inline;
    procedure Skip(const ANumBytes: Integer); inline;
    procedure SkipString;
    function ReadUInt8: UInt8; inline;
    function ReadUInt16: UInt16; inline;
    function ReadUInt32: UInt32; inline;
    function ReadUInt64: UInt64; inline;
    function ReadULeb128: UInt64; inline;
    function ReadSInt8: Int8; inline;
    function ReadSLeb128: Int64; inline;
    function ReadString: String;
    procedure Read(var AData; const ANumBytes: Integer); inline;

    property Cur: PByte read FCur;
  end;

type
  TDwarfAttributeDesc = record
  public
    Name: Integer;
    Form: Integer;
  public
    procedure SkipValue(const ACursor: TDwarfCursor);
    function StringValue(const ACursor: TDwarfCursor;
      const AStringSection: TBytes): String;
    function UIntValue(const ACursor: TDwarfCursor): UInt64;
  end;

type
  TDwarfAbbreviation = class
  {$REGION 'Internal Declarations'}
  private
    FTag: Integer;
    FAttributes: TArray<TDwarfAttributeDesc>;
    FHasChildren: Boolean;
  protected
    procedure Load(const ACursor: TDwarfCursor);
  {$ENDREGION 'Internal Declarations'}
  public
    property Tag: Integer read FTag;
    property Attributes: TArray<TDwarfAttributeDesc> read FAttributes;
    property HasChildren: Boolean read FHasChildren;
  end;

type
  TDwarfAbbreviationTable = class
  {$REGION 'Internal Declarations'}
  private
    FAbbreviations: TObjectDictionary<Integer, TDwarfAbbreviation>;
  protected
    procedure Load(const ACursor: TDwarfCursor; const AOffset: Integer);
  {$ENDREGION 'Internal Declarations'}
  public
    constructor Create;
    destructor Destroy; override;

    function Get(const AAbbrevCode: Integer): TDwarfAbbreviation; inline;
  end;

type
  TDwarfSections = record
  public
    Abbrev: TBytes;
    Info: TBytes;
    Line: TBytes;
    Str: TBytes;
  end;

type
  TDwarfFile = record
  public
    Name: String;

    { Index into TDwarfCompilationUnit.IncludeDirectories.
      0 if in current directory. }
    DirectoryIndex: Integer;

    Time: Int64;
    Size: Int64;
  end;

type
  TDwarfLineFlag = (IsStatement, BasicBlock, EndSequence, PrologueEnd,
    EpilogueBegin);
  TDwarfLineFlags = set of TDwarfLineFlag;

type
  TDwarfLine = packed record
    Address: UInt64;
    FileIndex: Integer;
    Line: Integer;
    Column: Integer;
    Flags: TDwarfLineFlags;
  end;

type
  TDwarfInfo = class;

  TDwarfCompilationUnit = class
  {$REGION 'Internal Declarations'}
  private
    FOwner: TDwarfInfo; // Reference
    FAbbreviations: TDwarfAbbreviationTable; // Reference
    FProducer: String;
    FName: String;
    FDirectory: String;
    FLanguage: Integer;
    FIncludeDirectories: TStringList;
    FFiles: TList<TDwarfFile>;
    FLines: TList<TDwarfLine>;
  protected
    procedure Load(const ACursor: TDwarfCursor);
    procedure LoadLineNumbers(const AOffset: Integer);
  {$ENDREGION 'Internal Declarations'}
  public
    constructor Create(const AOwner: TDwarfInfo;
      const AAbbreviations: TDwarfAbbreviationTable);
    destructor Destroy; override;

    property Producer: String read FProducer;
    property Name: String read FName;
    property Directory: String read FDirectory;

    { DW_LANG_* }
    property Language: Integer read FLanguage;

    property IncludeDirectories: TStringList read FIncludeDirectories;
    property Files: TList<TDwarfFile> read FFiles;
    property Lines: TList<TDwarfLine> read FLines;
  end;

  TDwarfInfo = class
  {$REGION 'Internal Declarations'}
  private
    FSections: TDwarfSections;
    FCompilationUnits: TObjectList<TDwarfCompilationUnit>;
    FAbbreviationTables: TObjectDictionary<Integer, TDwarfAbbreviationTable>;
  private
    procedure LoadInfo(const ACursor: TDwarfCursor);
  {$ENDREGION 'Internal Declarations'}
  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    procedure Load(const ASections: TDwarfSections);

    property CompilationUnits: TObjectList<TDwarfCompilationUnit> read FCompilationUnits;
  end;

implementation

const
  DW_LNS_copy               = 1;
  DW_LNS_advance_pc         = 2;
  DW_LNS_advance_line       = 3;
  DW_LNS_set_file           = 4;
  DW_LNS_set_column         = 5;
  DW_LNS_negate_stmt        = 6;
  DW_LNS_set_basic_block    = 7;
  DW_LNS_const_add_pc       = 8;
  DW_LNS_fixed_advance_pc   = 9;
  DW_LNS_set_prologue_end   = 10;
  DW_LNS_set_epilogue_begin = 11;
  DW_LNS_set_isa            = 12;

const
  DW_LNE_end_sequence = 1;
  DW_LNE_set_address  = 2;
  DW_LNE_define_file  = 3;

{ TDwarfCursor }

function TDwarfCursor.Eof: Boolean;
begin
  Result := (FCur >= FEnd);
end;

class operator TDwarfCursor.Implicit(const ABytes: TBytes): TDwarfCursor;
begin
  if (ABytes = nil) then
  begin
    Result.FCur := nil;
    Result.FEnd := nil;
  end
  else
  begin
    Result.FCur := @ABytes[0];
    Result.FEnd := @ABytes[Length(ABytes)];
  end;
end;

procedure TDwarfCursor.Read(var AData; const ANumBytes: Integer);
begin
  Move(FCur^, AData, ANumBytes);
  Inc(FCur, ANumBytes);
end;

class procedure TDwarfCursor.ReadError;
begin
  raise EDwarfError.Create('Read beyond end of DWARF section');
end;

function TDwarfCursor.ReadSInt8: Int8;
begin
  Result := Int8(FCur^);
  Inc(FCur, SizeOf(Int8));
end;

function TDwarfCursor.ReadSLeb128: Int64;
var
  Shift: Integer;
  B: Byte;
begin
  Result := 0;
  Shift := 0;
  while (True) do
  begin
    B := FCur^;
    Inc(FCur);

    Result := Result or ((B and $7F) shl Shift);
    Inc(Shift, 7);

    if ((B and $80) = 0) then
      Break;
  end;

  if ((B and $40) <> 0) then
  begin
    if (Shift < 63) then
      Result := Result or -Int64(UInt64(1) shl Shift)
    else
      Result := Result or (UInt64(1) shl Shift);
  end;
end;

function TDwarfCursor.ReadString: String;
var
  Start: PByte;
  I, Len: Integer;
  P: PChar;
begin
  Start := FCur;
  while (FCur < FEnd) and (FCur^ <> 0) do
    Inc(FCur);

  Len := FCur - Start;
  Inc(FCur);
  if (Len = 0) then
    Exit('');

  SetLength(Result, Len);
  P := Pointer(Result);
  for I := 0 to Len - 1 do
  begin
    P^ := Char(Start^);
    Inc(Start);
    Inc(P);
  end;
end;

function TDwarfCursor.ReadUInt16: UInt16;
begin
  Result := PWord(FCur)^;
  Inc(FCur, SizeOf(UInt16));
end;

function TDwarfCursor.ReadUInt32: UInt32;
begin
  Result := PUInt32(FCur)^;
  Inc(FCur, SizeOf(UInt32));
end;

function TDwarfCursor.ReadUInt64: UInt64;
begin
  Result := PUInt64(FCur)^;
  Inc(FCur, SizeOf(UInt64));
end;

function TDwarfCursor.ReadUInt8: UInt8;
begin
  Result := FCur^;
  Inc(FCur, SizeOf(UInt8));
end;

function TDwarfCursor.ReadULeb128: UInt64;
var
  Shift: Integer;
  B: Byte;
begin
  Result := 0;
  Shift := 0;
  while (True) do
  begin
    B := FCur^;
    Inc(FCur);

    Result := Result or ((B and $7F) shl Shift);
    if ((B and $80) = 0) then
      Break;

    Inc(Shift, 7);
  end;
end;

procedure TDwarfCursor.Require(const ANumBytes: Integer);
begin
  if (ANumBytes > (FEnd - FCur)) then
    ReadError;
end;

procedure TDwarfCursor.Seek(const APtr: PByte);
begin
  FCur := APtr;
end;

procedure TDwarfCursor.Skip(const ANumBytes: Integer);
begin
  Inc(FCur, ANumBytes);
end;

procedure TDwarfCursor.SkipString;
begin
  while (FCur < FEnd) and (FCur^ <> 0) do
    Inc(FCur);
  Inc(FCur);
end;

{ TDwarfAttributeDesc }

procedure TDwarfAttributeDesc.SkipValue(const ACursor: TDwarfCursor);
var
  Size: UInt32;
  Indirect: TDwarfAttributeDesc;
begin
  case Form of
    DW_FORM_flag_present:
      { No data };

    DW_FORM_flag,
    DW_FORM_data1,
    DW_FORM_ref1:
      ACursor.Skip(1);

    DW_FORM_data2,
    DW_FORM_ref2:
      ACursor.Skip(2);

    DW_FORM_data4,
    DW_FORM_ref4,
    DW_FORM_strp:
      ACursor.Skip(4);

    DW_FORM_ref_addr,
    DW_FORM_sec_offset:
      ACursor.Skip(4); // Only 32-bit DWARF sections are supported (this is checked in TDwarfInfo.LoadInfo)

    DW_FORM_data8,
    DW_FORM_ref8,
    DW_FORM_ref_sig8:
      ACursor.Skip(8);

    DW_FORM_addr:
      ACursor.Skip(8); // Only 64-bit addresses are supported (this is checked in TDwarfInfo.LoadInfo)

    DW_FORM_block,
    DW_FORM_exprloc:
      begin
        Size := ACursor.ReadULeb128;
        ACursor.Skip(Size);
      end;

    DW_FORM_block1:
      begin
        Size := ACursor.ReadUInt8;
        ACursor.Skip(Size);
      end;

    DW_FORM_block2:
      begin
        Size := ACursor.ReadUInt16;
        ACursor.Skip(Size);
      end;

    DW_FORM_block4:
      begin
        Size := ACursor.ReadUInt32;
        ACursor.Skip(Size);
      end;

    DW_FORM_udata,
    DW_FORM_ref_udata:
      ACursor.ReadULeb128;

    DW_FORM_sdata:
      ACursor.ReadSLeb128;

    DW_FORM_string:
      ACursor.SkipString;

    DW_FORM_indirect:
      begin
        Indirect.Name := 0;
        Indirect.Form := ACursor.ReadULeb128;
        Indirect.SkipValue(ACursor);
      end
  else
    raise EDwarfError.CreateFmt('Unsupported attribute form (%d)', [Form]);
  end;
end;

function TDwarfAttributeDesc.StringValue(const ACursor: TDwarfCursor;
  const AStringSection: TBytes): String;
var
  Offset: Integer;
  StrCursor: TDwarfCursor;
begin
  case Form of
    DW_FORM_string:
      Result := ACursor.ReadString;

    DW_FORM_strp:
      begin
        Offset := ACursor.ReadUInt32;
        StrCursor := AStringSection;
        StrCursor.Skip(Offset);
        Result := StrCursor.ReadString;
      end;
  else
    SkipValue(ACursor);
    Result := '';
  end;
end;

function TDwarfAttributeDesc.UIntValue(const ACursor: TDwarfCursor): UInt64;
begin
  case Form of
    DW_FORM_data1:
      Result := ACursor.ReadUInt8;

    DW_FORM_data2:
      Result := ACursor.ReadUInt16;

    DW_FORM_data4:
      Result := ACursor.ReadUInt32;

    DW_FORM_data8:
      Result := ACursor.ReadUInt64;

    DW_FORM_udata:
      Result := ACursor.ReadULeb128;

    DW_FORM_sdata:
      Result := ACursor.ReadSLeb128;
  else
    SkipValue(ACursor);
    Result := 0;
  end;
end;

{ TDwarfAbbreviation }

procedure TDwarfAbbreviation.Load(const ACursor: TDwarfCursor);
var
  Attr: TDwarfAttributeDesc;
  AttrCount: Integer;
begin
  FTag := ACursor.ReadULeb128;
  FHasChildren := (ACursor.ReadUInt8 = DW_CHILDREN_yes);
  AttrCount := 0;
  while (not ACursor.Eof) do
  begin
    Attr.Name := ACursor.ReadULeb128;
    Attr.Form := ACursor.ReadULeb128;
    if (Attr.Name = 0) and (Attr.Form = 0) then
      Break;

    if (AttrCount >= Length(FAttributes)) then
      SetLength(FAttributes, AttrCount + 8);

    FAttributes[AttrCount] := Attr;
    Inc(AttrCount);
  end;
  SetLength(FAttributes, AttrCount);
end;

{ TDwarfAbbreviationTable }

constructor TDwarfAbbreviationTable.Create;
begin
  inherited;
  FAbbreviations := TObjectDictionary<Integer, TDwarfAbbreviation>.Create([doOwnsValues]);
end;

destructor TDwarfAbbreviationTable.Destroy;
begin
  FAbbreviations.Free;
  inherited;
end;

function TDwarfAbbreviationTable.Get(
  const AAbbrevCode: Integer): TDwarfAbbreviation;
begin
  FAbbreviations.TryGetValue(AAbbrevCode, Result);
end;

procedure TDwarfAbbreviationTable.Load(const ACursor: TDwarfCursor;
  const AOffset: Integer);
var
  Code: Integer;
  Abbrev: TDwarfAbbreviation;
begin
  ACursor.Skip(AOffset);
  while (not ACursor.Eof) do
  begin
    Code := ACursor.ReadULeb128;
    if (Code = 0) then
      Break;

    Assert(not FAbbreviations.ContainsKey(Code));
    Abbrev := TDwarfAbbreviation.Create;
    FAbbreviations.Add(Code, Abbrev);

    Abbrev.Load(ACursor);
  end;
end;

{ TDwarfCompilationUnit }

constructor TDwarfCompilationUnit.Create(const AOwner: TDwarfInfo;
  const AAbbreviations: TDwarfAbbreviationTable);
begin
  inherited Create;
  FOwner := AOwner;
  FAbbreviations := AAbbreviations;

  FIncludeDirectories := TStringList.Create;
  FFiles := TList<TDwarfFile>.Create;
  FLines := TList<TDwarfLine>.Create;
end;

destructor TDwarfCompilationUnit.Destroy;
begin
  FLines.Free;
  FFiles.Free;
  FIncludeDirectories.Free;
  inherited;
end;

procedure TDwarfCompilationUnit.Load(const ACursor: TDwarfCursor);
var
  AbbrevCode, StmtList: Integer;
  Abbrev: TDwarfAbbreviation;
  Attr: TDwarfAttributeDesc;
begin
  { For now, we only load the top-level DIE for the compilation unit. We don't
    load any child DIE's (yet) }
  AbbrevCode := ACursor.ReadULeb128;
  Abbrev := FAbbreviations.Get(AbbrevCode);
  if (Abbrev = nil) then
    raise EDwarfError.CreateFmt('Invalid DWARF abbreviation code (%d)', [AbbrevCode]);

  if (Abbrev.Tag <> DW_TAG_compile_unit) then
    raise EDwarfError.Create('DWARF compilation unit must start with a DW_TAG_compile_unit tag');

  StmtList := -1;
  for Attr in Abbrev.Attributes do
  begin
    case Attr.Name of
      DW_AT_producer:
        FProducer := Attr.StringValue(ACursor, FOwner.FSections.Str);

      DW_AT_language:
        FLanguage := Attr.UIntValue(ACursor);

      DW_AT_name:
        FName := Attr.StringValue(ACursor, FOwner.FSections.Str);

      DW_AT_comp_dir:
        FDirectory := Attr.StringValue(ACursor, FOwner.FSections.Str);

      DW_AT_stmt_list:
        StmtList := Attr.UIntValue(ACursor);
    else
      Attr.SkipValue(ACursor);
    end;
  end;

  if (StmtList >= 0) then
    LoadLineNumbers(StmtList);
end;

procedure TDwarfCompilationUnit.LoadLineNumbers(const AOffset: Integer);
var
  Cursor: TDwarfCursor;
  Size: Cardinal;
  Version, PrologueLength, MinimumInstructionLength, OpcodeBase: Integer;
  LineBase, LineRange, LineAdv, AddrAdv: Integer;
  DefaultIsStmt: Boolean;
  ProgramStart, ProgramEnd, Next: PByte;
  Opcode: Byte;
  StandardOpcodeLengths: array [0..255] of Byte;
  Line: TDwarfLine;
  S: String;
  F: TDwarfFile;
begin
  if (FOwner.FSections.Line = nil) then
    raise EDwarfError.Create('A __debug_line DWARF debug section is required');

  Cursor := FOwner.FSections.Line;
  Cursor.Skip(AOffset);
  Cursor.Require(SizeOf(UInt32));
  Size := Cursor.ReadUInt32;
  if (Size >= $FFFFFFF0) then
    raise EDwarfError.Create('Only 32-bit DWARF sections are supported');
  Cursor.Require(Size);

  ProgramEnd := Cursor.Cur + Size;

  Version := Cursor.ReadUInt16;
  if (Version <> 2) then
    raise EDwarfError.Create('Only DWARF version 2 is supported for line numbers');

  PrologueLength := Cursor.ReadUInt32;
  ProgramStart := Cursor.Cur + PrologueLength;
  MinimumInstructionLength := Cursor.ReadUInt8;
  DefaultIsStmt := (Cursor.ReadUInt8 <> 0);
  LineBase := Cursor.ReadSInt8;
  LineRange := Cursor.ReadUInt8;
  OpcodeBase := Cursor.ReadUInt8;
  Cursor.Read(StandardOpcodeLengths[1], OpcodeBase - 1);

  FIncludeDirectories.Clear;
  FIncludeDirectories.Add(''); // First entry is current directory
  while (True) do
  begin
    S := Cursor.ReadString;
    if (S = '') then
      Break;

    FIncludeDirectories.Add(S);
  end;

  FFiles.Clear;
  FillChar(F, SizeOf(F), 0);
  FFiles.Add(F); // Files start at index 1
  while (True) do
  begin
    F.Name := Cursor.ReadString;
    if (F.Name = '') then
      Break;

    F.DirectoryIndex := Cursor.ReadULeb128;
    F.Time := Cursor.ReadULeb128;
    F.Size := Cursor.ReadULeb128;
    FFiles.Add(F);
  end;

  { Execute statement program }
  FLines.Clear;
  Cursor.Seek(ProgramStart);
  while (Cursor.Cur < ProgramEnd) do
  begin
    Line.Address := 0;
    Line.FileIndex := 1;
    Line.Line := 1;
    Line.Column := 0;
    if (DefaultIsStmt) then
      Line.Flags := [TDwarfLineFlag.IsStatement]
    else
      Line.Flags := [];

    while (True) do
    begin
      Opcode := Cursor.ReadUInt8;
      if (Opcode = 0) then
      begin
        { Extended opcode }
        Size := Cursor.ReadULeb128;
        Next := Cursor.Cur + Size;
        Opcode := Cursor.ReadUInt8;
        case Opcode of
          DW_LNE_end_sequence:
            begin
              Include(Line.Flags, TDwarfLineFlag.EndSequence);
              FLines.Add(Line);
              Break;
            end;

          DW_LNE_set_address:
            { We only support 64-bit apps }
            Line.Address := Cursor.ReadUInt64;

          DW_LNE_define_file:
            raise EDwarfError.Create('DW_LNE_define_file opcode not supported');
        else
          raise EDwarfError.CreateFmt('Invalid extended opcode (%d)', [Opcode]);
        end;
        Cursor.Seek(Next);
      end
      else if (Opcode >= OpcodeBase) then
      begin
        { Special opcode }
        Dec(Opcode, OpcodeBase);
        LineAdv := LineBase + (Opcode mod LineRange);
        AddrAdv := (Opcode div LineRange) * MinimumInstructionLength;
        Inc(Line.Address, AddrAdv);
        Inc(Line.Line, LineAdv);
        FLines.Add(Line);
        Line.Flags := Line.Flags - [TDwarfLineFlag.BasicBlock, TDwarfLineFlag.PrologueEnd, TDwarfLineFlag.EpilogueBegin];
      end
      else
      begin
        { Standard opcode }
        case Opcode of
          DW_LNS_copy:
            begin
              FLines.Add(Line);
              Line.Flags := Line.Flags - [TDwarfLineFlag.BasicBlock, TDwarfLineFlag.PrologueEnd, TDwarfLineFlag.EpilogueBegin];
            end;

          DW_LNS_advance_pc:
            Inc(Line.Address, Cursor.ReadULeb128 * Cardinal(MinimumInstructionLength));

          DW_LNS_advance_line:
            Inc(Line.Line, Cursor.ReadSLeb128);

          DW_LNS_set_file:
            Line.FileIndex := Cursor.ReadULeb128;

          DW_LNS_set_column:
            Line.Column := Cursor.ReadULeb128;

          DW_LNS_negate_stmt:
            if (TDwarfLineFlag.IsStatement in Line.Flags) then
              Exclude(Line.Flags, TDwarfLineFlag.IsStatement)
            else
              Include(Line.Flags, TDwarfLineFlag.IsStatement);

          DW_LNS_set_basic_block:
            Include(Line.Flags, TDwarfLineFlag.BasicBlock);

          DW_LNS_const_add_pc:
            Inc(Line.Address, MinimumInstructionLength * ((255 - OpcodeBase) div LineRange));

          DW_LNS_fixed_advance_pc:
            Inc(Line.Address, Cursor.ReadUInt16);

          DW_LNS_set_prologue_end:
            Include(Line.Flags, TDwarfLineFlag.PrologueEnd);

          DW_LNS_set_epilogue_begin:
            Include(Line.Flags, TDwarfLineFlag.EpilogueBegin);

          DW_LNS_set_isa:
            Cursor.ReadULeb128; // Skip ISA
        else
          { Skip unknown/unsupported opcode }
          Cursor.Skip(StandardOpcodeLengths[Opcode]);
        end;
      end;
    end;
  end;
end;

{ TDwarfInfo }

procedure TDwarfInfo.Clear;
begin
  FCompilationUnits.Clear;
end;

constructor TDwarfInfo.Create;
begin
  inherited;
  FCompilationUnits := TObjectList<TDwarfCompilationUnit>.Create;
  FAbbreviationTables := TObjectDictionary<Integer, TDwarfAbbreviationTable>.Create([doOwnsValues]);
end;

destructor TDwarfInfo.Destroy;
begin
  FAbbreviationTables.Free;
  FCompilationUnits.Free;
  inherited;
end;

procedure TDwarfInfo.Load(const ASections: TDwarfSections);
begin
  Clear;
  FSections := ASections; // Keep data alive

  if (ASections.Abbrev = nil) then
    raise EDwarfError.Create('A __debug_abbrev DWARF debug section is required');

  if (ASections.Info = nil) then
    raise EDwarfError.Create('A __debug_info DWARF debug section is required');

  if (ASections.Line = nil) then
    raise EDwarfError.Create('A __debug_line DWARF debug section is required');

  if (ASections.Str = nil) then
    raise EDwarfError.Create('A __debug_str DWARF debug section is required');

  LoadInfo(ASections.Info);
end;

procedure TDwarfInfo.LoadInfo(const ACursor: TDwarfCursor);
var
  Size: Cardinal;
  Version, AbbrevOffset: Integer;
  Next: PByte;
  CU: TDwarfCompilationUnit;
  Abbrevs: TDwarfAbbreviationTable;
begin
  while (not ACursor.Eof) do
  begin
    ACursor.Require(SizeOf(UInt32));
    Size := ACursor.ReadUInt32;
    if (Size >= $FFFFFFF0) then
      raise EDwarfError.Create('Only 32-bit DWARF sections are supported');
    ACursor.Require(Size);

    Next := ACursor.Cur + Size;

    Version := ACursor.ReadUInt16;
    if (Version < 2) or (Version > 5) then
      raise EDwarfError.Create('Only DWARF versions 2-5 are supported');

    AbbrevOffset := ACursor.ReadUInt32;

    if (ACursor.ReadUInt8 <> 8) then
      raise EDwarfError.Create('Only 64-bit apps are supported');

    if (not FAbbreviationTables.TryGetValue(AbbrevOffset, Abbrevs)) then
    begin
      Abbrevs := TDwarfAbbreviationTable.Create;
      FAbbreviationTables.Add(AbbrevOffset, Abbrevs);
      Abbrevs.Load(FSections.Abbrev, AbbrevOffset);
    end;

    CU := TDwarfCompilationUnit.Create(Self, Abbrevs);
    FCompilationUnits.Add(CU);

    CU.Load(ACursor);

    ACursor.Seek(Next);
  end;
end;

end.
