unit Grijjy.MachOApi;
{< Partial translations of some Mach-O related kernel headers. }

interface

{$REGION 'machine.h'}
type
  cpu_type_t    = Integer;
  cpu_subtype_t = Integer;

const
  CPU_ARCH_ABI64  = $01000000; (* 64 bit ABI *)
  CPU_TYPE_X86    = 7;
  CPU_TYPE_X86_64 = CPU_TYPE_X86 or CPU_ARCH_ABI64;
{$ENDREGION 'machine.h'}

{$REGION 'vm_prot.h'}
type
  vm_prot_t = Integer;
{$ENDREGION 'vm_prot.h'}

{$REGION 'loader.h'}
(*
 * This file describes the format of mach object files.
 *)

type
  (*
   * The 32-bit mach header appears at the very beginning of the object file for
   * 32-bit architectures.
   *)
  mach_header = packed record
    magic: UInt32;             (* mach magic number identifier *)
    cputype: cpu_type_t;       (* cpu specifier *)
    cpusubtype: cpu_subtype_t; (* machine specifier *)
    filetype: UInt32;          (* type of file *)
    ncmds: Int32;              (* number of load commands *)
    sizeofcmds: UInt32;        (* the size of all the load commands *)
    flags: UInt32;             (* flags *)
  end;
  Pmach_header = ^mach_header;

const
  (* Constant for the magic field of the mach_header (32-bit architectures) *)
  MH_MAGIC = $feedface; (* the mach magic number *)
  MH_CIGAM = $cefaedfe; (* NXSwapInt(MH_MAGIC) *)

type
  (*
   * The 64-bit mach header appears at the very beginning of object files for
   * 64-bit architectures.
   *)
  mach_header_64 = packed record
    magic: UInt32;             (* mach magic number identifier *)
    cputype: cpu_type_t;       (* cpu specifier *)
    cpusubtype: cpu_subtype_t; (* machine specifier *)
    filetype: UInt32;          (* type of file *)
    ncmds: Int32;              (* number of load commands *)
    sizeofcmds: UInt32;        (* the size of all the load commands *)
    flags: UInt32;             (* flags *)
    reserved: UInt32;          (* reserved *)
  end;
  Pmach_header_64 = ^mach_header_64;

const
  (* Constant for the magic field of the mach_header_64 (64-bit architectures) *)
  MH_MAGIC_64 = $feedfacf; (* the 64-bit mach magic number *)
  MH_CIGAM_64 = $cffaedfe; (* NXSwapInt(MH_MAGIC_64) *)

const
  (*
   * The layout of the file depends on the filetype.  For all but the MH_OBJECT
   * file type the segments are padded out and aligned on a segment alignment
   * boundary for efficient demand pageing.  The MH_EXECUTE, MH_FVMLIB, MH_DYLIB,
   * MH_DYLINKER and MH_BUNDLE file types also have the headers included as part
   * of their first segment.
   *
   * The file type MH_OBJECT is a compact format intended as output of the
   * assembler and input (and possibly output) of the link editor (the .o
   * format).  All sections are in one unnamed segment with no segment padding.
   * This format is used as an executable format when the file is so small the
   * segment padding greatly increases its size.
   *
   * The file type MH_PRELOAD is an executable format intended for things that
   * are not executed under the kernel (proms, stand alones, kernels, etc).  The
   * format can be executed under the kernel but may demand paged it and not
   * preload it before execution.
   *
   * A core file is in MH_CORE format and can be any in an arbritray legal
   * Mach-O file.
   *
   * Constants for the filetype field of the mach_header
   *)
  MH_OBJECT      = $1;  (* relocatable object file *)
  MH_EXECUTE     = $2;  (* demand paged executable file *)
  MH_FVMLIB      = $3;  (* fixed VM shared library file *)
  MH_CORE        = $4;  (* core file *)
  MH_PRELOAD     = $5;  (* preloaded executable file *)
  MH_DYLIB       = $6;  (* dynamically bound shared library *)
  MH_DYLINKER    = $7;  (* dynamic link editor *)
  MH_BUNDLE      = $8;  (* dynamically bound bundle file *)
  MH_DYLIB_STUB  = $9;  (* shared library stub for static
                           linking only, no section contents *)
  MH_DSYM        = $a;  (* companion file with only debug sections *)
  MH_KEXT_BUNDLE = $b;  (* x86_64 kexts *)

const
  (* Constants for the flags field of the mach_header *)
  MH_NOUNDEFS                = $1;         (* the object file has no undefined references *)
  MH_INCRLINK                = $2;         (* the object file is the output of an
                                              incremental link against a base file
                                              and can't be link edited again *)
  MH_DYLDLINK                = $4;         (* the object file is input for the
                                              dynamic linker and can't be staticly
                                              link edited again *)
  MH_BINDATLOAD              = $8;         (* the object file's undefined
                                              references are bound by the dynamic
                                              linker when loaded. *)
  MH_PREBOUND                = $10;        (* the file has its dynamic undefined
                                              references prebound. *)
  MH_SPLIT_SEGS              = $20;        (* the file has its read-only and
                                              read-write segments split *)
  MH_LAZY_INIT               = $40;        (* the shared library init routine is
                                              to be run lazily via catching memory
                                              faults to its writeable segments
                                              (obsolete) *)
  MH_TWOLEVEL                = $80;        (* the image is using two-level name
                                              space bindings *)
  MH_FORCE_FLAT              = $100;       (* the executable is forcing all images
                                              to use flat name space bindings *)
  MH_NOMULTIDEFS             = $200;       (* this umbrella guarantees no multiple
                                              defintions of symbols in its
                                              sub-images so the two-level namespace
                                              hints can always be used. *)
  MH_NOFIXPREBINDING         = $400;       (* do not have dyld notify the
                                              prebinding agent about this
                                              executable *)
  MH_PREBINDABLE             = $800;       (* the binary is not prebound but can
                                              have its prebinding redone. only used
                                              when MH_PREBOUND is not set. *)
  MH_ALLMODSBOUND            = $1000;      (* indicates that this binary binds to
                                              all two-level namespace modules of
                                              its dependent libraries. only used
                                              when MH_PREBINDABLE and MH_TWOLEVEL
                                              are both set. *)
  MH_SUBSECTIONS_VIA_SYMBOLS = $2000;      (* safe to divide up the sections into
                                              sub-sections via symbols for dead
                                              code stripping *)
  MH_CANONICAL               = $4000;      (* the binary has been canonicalized
                                              via the unprebind operation *)
  MH_WEAK_DEFINES            = $8000;      (* the final linked image contains
                                              external weak symbols *)
  MH_BINDS_TO_WEAK           = $10000;     (* the final linked image uses
                                              weak symbols *)
  MH_ALLOW_STACK_EXECUTION   = $20000;     (* When this bit is set, all stacks
                                              in the task will be given stack
                                              execution privilege.  Only used in
                                              MH_EXECUTE filetypes. *)
  MH_ROOT_SAFE               = $40000;     (* When this bit is set, the binary
                                              declares it is safe for use in
                                              processes with uid zero *)
  MH_SETUID_SAFE             = $80000;     (* When this bit is set, the binary
                                              declares it is safe for use in
                                              processes when issetugid() is true *)
  MH_NO_REEXPORTED_DYLIBS    = $100000;    (* When this bit is set on a dylib,
                                              the static linker does not need to
                                              examine dependent dylibs to see
                                              if any are re-exported *)
  MH_PIE                     = $200000;    (* When this bit is set, the OS will
                                              load the main executable at a
                                              random address.  Only used in
                                              MH_EXECUTE filetypes. *)
  MH_DEAD_STRIPPABLE_DYLIB   = $400000;    (* Only for use on dylibs.  When
                                              linking against a dylib that
                                              has this bit set, the static linker
                                              will automatically not create a
                                              LC_LOAD_DYLIB load command to the
                                              dylib if no symbols are being
                                              referenced from the dylib. *)
  MH_HAS_TLV_DESCRIPTORS     = $800000;    (* Contains a section of type
                                              S_THREAD_LOCAL_VARIABLES *)
  MH_NO_HEAP_EXECUTION       = $1000000;   (* When this bit is set, the OS will
                                              run the main executable with
                                              a non-executable heap even on
                                              platforms (e.g. i386) that don't
                                              require it. Only used in MH_EXECUTE
                                              filetypes. *)
  MH_APP_EXTENSION_SAFE      = $02000000;  (* The code was linked for use in an
                                              application extension. *)

type
  (*
   * The load commands directly follow the mach_header.  The total size of all
   * of the commands is given by the sizeofcmds field in the mach_header.  All
   * load commands must have as their first two fields cmd and cmdsize.  The cmd
   * field is filled in with a constant for that command type.  Each command type
   * has a structure specifically for it.  The cmdsize field is the size in bytes
   * of the particular load command structure plus anything that follows it that
   * is a part of the load command (i.e. section structures, strings, etc.).  To
   * advance to the next load command the cmdsize can be added to the offset or
   * pointer of the current load command.  The cmdsize for 32-bit architectures
   * MUST be a multiple of 4 bytes and for 64-bit architectures MUST be a multiple
   * of 8 bytes (these are forever the maximum alignment of any load commands).
   * The padded bytes must be zero.  All tables in the object file must also
   * follow these rules so the file can be memory mapped.  Otherwise the pointers
   * to these tables will not work well or at all on some machines.  With all
   * padding zeroed like objects will compare byte for byte.
   *)
  load_command = record
    cmd: UInt32;     (* type of load command *)
    cmdsize: UInt32; (* total size of command in bytes *)
  end;
  Pload_command = ^load_command;

const
  (*
   * After MacOS X 10.1 when a new load command is added that is required to be
   * understood by the dynamic linker for the image to execute properly the
   * LC_REQ_DYLD bit will be or'ed into the load command constant.  If the dynamic
   * linker sees such a load command it it does not understand will issue a
   * "unknown load command required for execution" error and refuse to use the
   * image.  Other load commands without this bit that are not understood will
   * simply be ignored.
   *)
  LC_REQ_DYLD = $80000000;

const
  (* Constants for the cmd field of all load commands, the type *)
  LC_SEGMENT                  = $1;  (* segment of this file to be mapped *)
  LC_SYMTAB                   = $2;  (* link-edit stab symbol table info *)
  LC_SYMSEG                   = $3;  (* link-edit gdb symbol table info (obsolete) *)
  LC_THREAD                   = $4;  (* thread *)
  LC_UNIXTHREAD               = $5;  (* unix thread (includes a stack) *)
  LC_LOADFVMLIB               = $6;  (* load a specified fixed VM shared library *)
  LC_IDFVMLIB                 = $7;  (* fixed VM shared library identification *)
  LC_IDENT                    = $8;  (* object identification info (obsolete) *)
  LC_FVMFILE                  = $9;  (* fixed VM file inclusion (internal use) *)
  LC_PREPAGE                  = $a;  (* prepage command (internal use) *)
  LC_DYSYMTAB                 = $b;  (* dynamic link-edit symbol table info *)
  LC_LOAD_DYLIB               = $c;  (* load a dynamically linked shared library *)
  LC_ID_DYLIB                 = $d;  (* dynamically linked shared lib ident *)
  LC_LOAD_DYLINKER            = $e;  (* load a dynamic linker *)
  LC_ID_DYLINKER              = $f;  (* dynamic linker identification *)
  LC_PREBOUND_DYLIB           = $10; (* modules prebound for a dynamically
                                        linked shared library *)
  LC_ROUTINES                 = $11; (* image routines *)
  LC_SUB_FRAMEWORK            = $12; (* sub framework *)
  LC_SUB_UMBRELLA             = $13; (* sub umbrella *)
  LC_SUB_CLIENT               = $14; (* sub client *)
  LC_SUB_LIBRARY              = $15; (* sub library *)
  LC_TWOLEVEL_HINTS           = $16; (* two-level namespace lookup hints *)
  LC_PREBIND_CKSUM            = $17; (* prebind checksum *)

  (*
   * load a dynamically linked shared library that is allowed to be missing
   * (all symbols are weak imported).
   *)
  LC_LOAD_WEAK_DYLIB          = $18 or LC_REQ_DYLD;

  LC_SEGMENT_64               = $19; (* 64-bit segment of this file to be
                                        mapped *)
  LC_ROUTINES_64              = $1a; (* 64-bit image routines *)
  LC_UUID                     = $1b; (* the uuid *)
  LC_RPATH                    = $1c or LC_REQ_DYLD; (* runpath additions *)
  LC_CODE_SIGNATURE           = $1d; (* local of code signature *)
  LC_SEGMENT_SPLIT_INFO       = $1e; (* local of info to split segments *)
  LC_REEXPORT_DYLIB           = $1f or LC_REQ_DYLD; (* load and re-export dylib *)
  LC_LAZY_LOAD_DYLIB          = $20; (* delay load of dylib until first use *)
  LC_ENCRYPTION_INFO          = $21; (* encrypted segment information *)
  LC_DYLD_INFO                = $22; (* compressed dyld information *)
  LC_DYLD_INFO_ONLY           = $22 or LC_REQ_DYLD; (* compressed dyld information only *)
  LC_LOAD_UPWARD_DYLIB        = $23 or LC_REQ_DYLD; (* load upward dylib *)
  LC_VERSION_MIN_MACOSX       = $24; (* build for MacOSX min OS version *)
  LC_VERSION_MIN_IPHONEOS     = $25; (* build for iPhoneOS min OS version *)
  LC_FUNCTION_STARTS          = $26; (* compressed table of function start addresses *)
  LC_DYLD_ENVIRONMENT         = $27; (* string for dyld to treat
                                        like environment variable *)
  LC_MAIN                     = $28 or LC_REQ_DYLD; (* replacement for LC_UNIXTHREAD *)
  LC_DATA_IN_CODE             = $29; (* table of non-instructions in __text *)
  LC_SOURCE_VERSION           = $2A; (* source version used to build binary *)
  LC_DYLIB_CODE_SIGN_DRS      = $2B; (* Code signing DRs copied from linked dylibs *)
  LC_ENCRYPTION_INFO_64       = $2C; (* 64-bit encrypted segment information *)
  LC_LINKER_OPTION            = $2D; (* linker options in MH_OBJECT files *)
  LC_LINKER_OPTIMIZATION_HINT = $2E; (* optimization hints in MH_OBJECT files *)
  LC_VERSION_MIN_TVOS         = $2F; (* build for AppleTV min OS version *)
  LC_VERSION_MIN_WATCHOS      = $30; (* build for Watch min OS version *)

type
  (*
   * A variable length string in a load command is represented by an lc_str
   * union.  The strings are stored just after the load command structure and
   * the offset is from the start of the load command structure.  The size
   * of the string is reflected in the cmdsize field of the load command.
   * Once again any padded bytes to bring the cmdsize field to a multiple
   * of 4 bytes must be zero.
   *)
  lc_str = record
    case Byte of
      0: (offset: UInt32); (* offset to the string *)
      {$IFDEF CPU32BITS}
      1: (ptr: PAnsiChar); (* pointer to the string *)
      {$ENDIF}
  end;
  Plc_str = ^lc_str;

type
  (*
   * The segment load command indicates that a part of this file is to be
   * mapped into the task's address space.  The size of this segment in memory,
   * vmsize, maybe equal to or larger than the amount to map from this file,
   * filesize.  The file is mapped starting at fileoff to the beginning of
   * the segment in memory, vmaddr.  The rest of the memory of the segment,
   * if any, is allocated zero fill on demand.  The segment's maximum virtual
   * memory protection and initial virtual memory protection are specified
   * by the maxprot and initprot fields.  If the segment has sections then the
   * section structures directly follow the segment command and their size is
   * reflected in cmdsize.
   *)
  segment_command = record (* for 32-bit architectures *)
    cmd: UInt32;         (* LC_SEGMENT *)
    cmdsize: UInt32;     (* includes sizeof section structs *)
    segname: array [0..15] of AnsiChar; (* segment name *)
    vmaddr: UInt32;      (* memory address of this segment *)
    vmsize: UInt32;      (* memory size of this segment *)
    fileoff: UInt32;     (* file offset of this segment *)
    filesize: UInt32;    (* amount to map from the file *)
    maxprot: vm_prot_t;  (* maximum VM protection *)
    initprot: vm_prot_t; (* initial VM protection *)
    nsects: Int32;       (* number of sections in segment *)
    flags: UInt32;       (* flags *)
  end;
  Psegment_command = ^segment_command;

type
  (*
   * The 64-bit segment load command indicates that a part of this file is to be
   * mapped into a 64-bit task's address space.  If the 64-bit segment has
   * sections then section_64 structures directly follow the 64-bit segment
   * command and their size is reflected in cmdsize.
   *)
  segment_command_64 = record (* for 64-bit architectures *)
    cmd: UInt32;         (* LC_SEGMENT *)
    cmdsize: UInt32;     (* includes sizeof section structs *)
    segname: array [0..15] of AnsiChar; (* segment name *)
    vmaddr: UInt64;      (* memory address of this segment *)
    vmsize: UInt64;      (* memory size of this segment *)
    fileoff: UInt64;     (* file offset of this segment *)
    filesize: UInt64;    (* amount to map from the file *)
    maxprot: vm_prot_t;  (* maximum VM protection *)
    initprot: vm_prot_t; (* initial VM protection *)
    nsects: Int32;       (* number of sections in segment *)
    flags: UInt32;       (* flags *)
  end;
  Psegment_command_64 = ^segment_command_64;

const
  (* Constants for the flags field of the segment_command *)
  SG_HIGHVM              = $1; (* the file contents for this segment is for
                                  the high part of the VM space, the low part
                                  is zero filled (for stacks in core files) *)
  SG_FVMLIB              = $2; (* this segment is the VM that is allocated by
                                  a fixed VM library, for overlap checking in
                                  the link editor *)
  SG_NORELOC             = $4; (* this segment has nothing that was relocated
                                  in it and nothing relocated to it, that is
                                  it maybe safely replaced without relocation*)
  SG_PROTECTED_VERSION_1 = $8; (* This segment is protected.  If the
                                  segment starts at file offset 0, the
                                  first page of the segment is not
                                  protected.  All other pages of the
                                  segment are protected. *)

type
  (*
   * A segment is made up of zero or more sections.  Non-MH_OBJECT files have
   * all of their segments with the proper sections in each, and padded to the
   * specified segment alignment when produced by the link editor.  The first
   * segment of a MH_EXECUTE and MH_FVMLIB format file contains the mach_header
   * and load commands of the object file before its first section.  The zero
   * fill sections are always last in their segment (in all formats).  This
   * allows the zeroed segment padding to be mapped into memory where zero fill
   * sections might be. The gigabyte zero fill sections, those with the section
   * type S_GB_ZEROFILL, can only be in a segment with sections of this type.
   * These segments are then placed after all other segments.
   *
   * The MH_OBJECT format has all of its sections in one segment for
   * compactness.  There is no padding to a specified segment boundary and the
   * mach_header and load commands are not part of the segment.
   *
   * Sections with the same section name, sectname, going into the same segment,
   * segname, are combined by the link editor.  The resulting section is aligned
   * to the maximum alignment of the combined sections and is the new section's
   * alignment.  The combined sections are aligned to their original alignment in
   * the combined section.  Any padded bytes to get the specified alignment are
   * zeroed.
   *
   * The format of the relocation entries referenced by the reloff and nreloc
   * fields of the section structure for mach object files is described in the
   * header file <reloc.h>.
   *)
  section = record (* for 32-bit architectures *)
    sectname: array [0..15] of AnsiChar; (* name of this section *)
    segname: array [0..15] of AnsiChar;  (* segment this section goes in *)
    addr: UInt32;      (* memory address of this section *)
    size: UInt32;      (* size in bytes of this section *)
    offset: UInt32;    (* file offset of this section *)
    align: UInt32;     (* section alignment (power of 2) *)
    reloff: UInt32;    (* file offset of relocation entries *)
    nreloc: Int32;     (* number of relocation entries *)
    flags: UInt32;     (* flags (section type and attributes)*)
    reserved1: UInt32; (* reserved (for offset or index) *)
    reserved2: UInt32; (* reserved (for count or sizeof) *)
  end;
  Psection = ^section;

type
  section_64 = record (* for 64-bit architectures *)
    sectname: array [0..15] of AnsiChar; (* name of this section *)
    segname: array [0..15] of AnsiChar;  (* segment this section goes in *)
    addr: UInt64;      (* memory address of this section *)
    size: UInt64;      (* size in bytes of this section *)
    offset: UInt32;    (* file offset of this section *)
    align: UInt32;     (* section alignment (power of 2) *)
    reloff: UInt32;    (* file offset of relocation entries *)
    nreloc: Int32;     (* number of relocation entries *)
    flags: UInt32;     (* flags (section type and attributes)*)
    reserved1: UInt32; (* reserved (for offset or index) *)
    reserved2: UInt32; (* reserved (for count or sizeof) *)
    reserved3: UInt32; (* reserved *)
  end;
  Psection_64 = ^section_64;

const
  (*
   * The flags field of a section structure is separated into two parts a section
   * type and section attributes.  The section types are mutually exclusive (it
   * can only have one type) but the section attributes are not (it may have more
   * than one attribute).
   *)
  SECTION_TYPE       = $000000ff; (* 256 section types *)
  SECTION_ATTRIBUTES = $ffffff00; (*  24 section attributes *)

const
  (* Constants for the type of a section *)
  S_REGULAR                             = $0;  (* regular section *)
  S_ZEROFILL                            = $1;  (* zero fill on demand section *)
  S_CSTRING_LITERALS                    = $2;  (* section with only literal C strings*)
  S_4BYTE_LITERALS                      = $3;  (* section with only 4 byte literals *)
  S_8BYTE_LITERALS                      = $4;  (* section with only 8 byte literals *)
  S_LITERAL_POINTERS                    = $5;  (* section with only pointers to *)
  (*  literals *)
  (*
   * For the two types of symbol pointers sections and the symbol stubs section
   * they have indirect symbol table entries.  For each of the entries in the
   * section the indirect symbol table entries, in corresponding order in the
   * indirect symbol table, start at the index stored in the reserved1 field
   * of the section structure.  Since the indirect symbol table entries
   * correspond to the entries in the section the number of indirect symbol table
   * entries is inferred from the size of the section divided by the size of the
   * entries in the section.  For symbol pointers sections the size of the entries
   * in the section is 4 bytes and for symbol stubs sections the byte size of the
   * stubs is stored in the reserved2 field of the section structure.
   *)
  S_NON_LAZY_SYMBOL_POINTERS            = $6;  (* section with only non-lazy
                                                  symbol pointers *)
  S_LAZY_SYMBOL_POINTERS                = $7;  (* section with only lazy symbol
                                                  pointers *)
  S_SYMBOL_STUBS                        = $8;  (* section with only symbol
                                                  stubs, byte size of stub in
                                                  the reserved2 field *)
  S_MOD_INIT_FUNC_POINTERS              = $9;  (* section with only function
                                                  pointers for initialization*)
  S_MOD_TERM_FUNC_POINTERS              = $a;  (* section with only function
                                                  pointers for termination *)
  S_COALESCED                           = $b;  (* section contains symbols that
                                                  are to be coalesced *)
  S_GB_ZEROFILL                         = $c;  (* zero fill on demand section
                                                  (that can be larger than 4
                                                  gigabytes) *)
  S_INTERPOSING                         = $d;  (* section with only pairs of
                                                  function pointers for
                                                  interposing *)
  S_16BYTE_LITERALS                     = $e;  (* section with only 16 byte
                                                  literals *)
  S_DTRACE_DOF                          = $f;  (* section contains
                                                  DTrace Object Format *)
  S_LAZY_DYLIB_SYMBOL_POINTERS          = $10; (* section with only lazy
                                                  symbol pointers to lazy
                                                  loaded dylibs *)
  (*
   * Section types to support thread local variables
   *)
  S_THREAD_LOCAL_REGULAR                = $11; (* template of initial
                                                  values for TLVs *)
  S_THREAD_LOCAL_ZEROFILL               = $12; (* template of initial
                                                  values for TLVs *)
  S_THREAD_LOCAL_VARIABLES              = $13; (* TLV descriptors *)
  S_THREAD_LOCAL_VARIABLE_POINTERS      = $14; (* pointers to TLV
                                                  descriptors *)
  S_THREAD_LOCAL_INIT_FUNCTION_POINTERS = $15; (* functions to call
                                                  to initialize TLV
                                                  values *)

const
  (*
   * Constants for the section attributes part of the flags field of a section
   * structure.
   *)
  SECTION_ATTRIBUTES_USR     = $ff000000; (* User setable attributes *)
  S_ATTR_PURE_INSTRUCTIONS   = $80000000; (* section contains only true
                                             machine instructions *)
  S_ATTR_NO_TOC              = $40000000; (* section contains coalesced
                                             symbols that are not to be
                                             in a ranlib table of
                                             contents *)
  S_ATTR_STRIP_STATIC_SYMS   = $20000000; (* ok to strip static symbols
                                             in this section in files
                                             with the MH_DYLDLINK flag *)
  S_ATTR_NO_DEAD_STRIP       = $10000000; (* no dead stripping *)
  S_ATTR_LIVE_SUPPORT        = $08000000; (* blocks are live if they
                                             reference live blocks *)
  S_ATTR_SELF_MODIFYING_CODE = $04000000; (* Used with i386 code stubs
                                             written on by dyld *)
  (*
   * If a segment contains any sections marked with S_ATTR_DEBUG then all
   * sections in that segment must have this attribute.  No section other than
   * a section marked with this attribute may reference the contents of this
   * section.  A section with this attribute may contain no symbols and must have
   * a section type S_REGULAR.  The static linker will not copy section contents
   * from sections with this attribute into its output file.  These sections
   * generally contain DWARF debugging info.
   *)
  S_ATTR_DEBUG               = $02000000; (* a debug section *)
  SECTION_ATTRIBUTES_SYS     = $00ffff00; (* system setable attributes *)
  S_ATTR_SOME_INSTRUCTIONS   = $00000400; (* section contains some
                                             machine instructions *)
  S_ATTR_EXT_RELOC           = $00000200; (* section has external
                                             relocation entries *)
  S_ATTR_LOC_RELOC           = $00000100; (* section has local
                                             relocation entries *)

const
  (*
   * The names of segments and sections in them are mostly meaningless to the
   * link-editor.  But there are few things to support traditional UNIX
   * executables that require the link-editor and assembler to use some names
   * agreed upon by convention.
   *
   * The initial protection of the "__TEXT" segment has write protection turned
   * off (not writeable).
   *
   * The link-editor will allocate common symbols at the end of the "__common"
   * section in the "__DATA" segment.  It will create the section and segment
   * if needed.
   *)

  (* The currently known segment names and the section names in those segments *)
  SEG_PAGEZERO      = '__PAGEZERO';      (* the pagezero segment which has no
                                            protections and catches NULL
                                            references for MH_EXECUTE files *)
  SEG_TEXT          = '__TEXT';          (* the tradition UNIX text segment *)
  SECT_TEXT         = '__text';          (* the real text part of the text
                                            section no headers, and no padding *)
  SECT_FVMLIB_INIT0 = '__fvmlib_init0';  (* the fvmlib initialization section *)
  SECT_FVMLIB_INIT1 = '__fvmlib_init1';  (* the section following the
                                            fvmlib initialization section *)
  SEG_DATA          = '__DATA';          (* the tradition UNIX data segment *)
  SECT_DATA         = '__data';          (* the real initialized data section
                                            no padding, no bss overlap *)
  SECT_BSS          = '__bss';           (* the real uninitialized data section
                                            no padding *)
  SECT_COMMON       = '__common';        (* the section common symbols are
                                            allocated in by the link editor *)
  SEG_OBJC          = '__OBJC';          (* objective-C runtime segment *)
  SECT_OBJC_SYMBOLS = '__symbol_table';  (* symbol table *)
  SECT_OBJC_MODULES = '__module_info';   (* module information *)
  SECT_OBJC_STRINGS = '__selector_strs'; (* string table *)
  SECT_OBJC_REFS    = '__selector_refs'; (* string table *)
  SEG_ICON          = '__ICON';          (* the icon segment *)
  SECT_ICON_HEADER  = '__header';        (* the icon headers *)
  SECT_ICON_TIFF    = '__tiff';          (* the icons in tiff format *)

  SEG_LINKEDIT      = '__LINKEDIT';      (* the segment containing all structs
                                            created and maintained by the link
                                            editor.  Created with -seglinkedit
                                            option to ld(1) for MH_EXECUTE and
                                            FVMLIB file types only *)
  SEG_UNIXSTACK     = '__UNIXSTACK';     (* the unix stack segment *)
  SEG_IMPORT        = '__IMPORT';        (* the segment for the self (dyld)
                                            modifing code stubs that has read,
                                            write and execute permissions *)

type
  (*
   * Fixed virtual memory shared libraries are identified by two things.  The
   * target pathname (the name of the library as found for execution), and the
   * minor version number.  The address of where the headers are loaded is in
   * header_addr. (THIS IS OBSOLETE and no longer supported).
   *)
  fvmlib = record
    name: lc_str;          (* library's target pathname *)
    minor_version: UInt32; (* library's minor version number *)
    header_addr: UInt32;   (* library's header address *)
  end;
  Pfvmlib = ^fvmlib;

type
  (*
   * A fixed virtual shared library (filetype == MH_FVMLIB in the mach header)
   * contains a fvmlib_command (cmd == LC_IDFVMLIB) to identify the library.
   * An object that uses a fixed virtual shared library also contains a
   * fvmlib_command (cmd == LC_LOADFVMLIB) for each library it uses.
   * (THIS IS OBSOLETE and no longer supported).
   *)
  fvmlib_command = record
    cmd: UInt32;     (* LC_IDFVMLIB or LC_LOADFVMLIB *)
    cmdsize: UInt32; (* includes pathname string *)
    fvmlib: fvmlib;  (* the library identification *)
  end;
  Pfvmlib_command = ^fvmlib_command;

type
  (*
   * Dynamicly linked shared libraries are identified by two things.  The
   * pathname (the name of the library as found for execution), and the
   * compatibility version number.  The pathname must match and the compatibility
   * number in the user of the library must be greater than or equal to the
   * library being used.  The time stamp is used to record the time a library was
   * built and copied into user so it can be use to determined if the library used
   * at runtime is exactly the same as used to built the program.
   *)
  dylib = record
    name: lc_str;                  (* library's path name *)
    timestamp: UInt32;             (* library's build time stamp *)
    current_version: UInt32;       (* library's current version number *)
    compatibility_version: UInt32; (* library's compatibility vers number*)
  end;
  Pdylib = ^dylib;

type
  (*
   * A dynamically linked shared library (filetype == MH_DYLIB in the mach header)
   * contains a dylib_command (cmd == LC_ID_DYLIB) to identify the library.
   * An object that uses a dynamically linked shared library also contains a
   * dylib_command (cmd == LC_LOAD_DYLIB, LC_LOAD_WEAK_DYLIB, or
   * LC_REEXPORT_DYLIB) for each library it uses.
   *)
  dylib_command = record
    cmd: UInt32;     (* LC_ID_DYLIB, LC_LOAD_{,WEAK_}DYLIB, LC_REEXPORT_DYLIB *)
    cmdsize: UInt32; (* includes pathname string *)
    dylib: dylib;    (* the library identification *)
  end;
  Pdylib_command = ^dylib_command;

type
  (*
   * A dynamically linked shared library may be a subframework of an umbrella
   * framework.  If so it will be linked with "-umbrella umbrella_name" where
   * Where "umbrella_name" is the name of the umbrella framework. A subframework
   * can only be linked against by its umbrella framework or other subframeworks
   * that are part of the same umbrella framework.  Otherwise the static link
   * editor produces an error and states to link against the umbrella framework.
   * The name of the umbrella framework for subframeworks is recorded in the
   * following structure.
   *)
  sub_framework_command = record
    cmd: UInt32;      (* LC_SUB_FRAMEWORK *)
    cmdsize: UInt32;  (* includes umbrella string *)
    umbrella: lc_str; (* the umbrella framework name *)
  end;
  Psub_framework_command = ^sub_framework_command;

type
  (*
   * For dynamically linked shared libraries that are subframework of an umbrella
   * framework they can allow clients other than the umbrella framework or other
   * subframeworks in the same umbrella framework.  To do this the subframework
   * is built with "-allowable_client client_name" and an LC_SUB_CLIENT load
   * command is created for each -allowable_client flag.  The client_name is
   * usually a framework name.  It can also be a name used for bundles clients
   * where the bundle is built with "-client_name client_name".
   *)
  sub_client_command = record
    cmd: UInt32;     (* LC_SUB_CLIENT *)
    cmdsize: UInt32; (* includes client string *)
    client: lc_str;  (* the client name *)
  end;
  Psub_client_command = ^sub_client_command;

type
  (*
   * A dynamically linked shared library may be a sub_umbrella of an umbrella
   * framework.  If so it will be linked with "-sub_umbrella umbrella_name" where
   * Where "umbrella_name" is the name of the sub_umbrella framework.  When
   * staticly linking when -twolevel_namespace is in effect a twolevel namespace
   * umbrella framework will only cause its subframeworks and those frameworks
   * listed as sub_umbrella frameworks to be implicited linked in.  Any other
   * dependent dynamic libraries will not be linked it when -twolevel_namespace
   * is in effect.  The primary library recorded by the static linker when
   * resolving a symbol in these libraries will be the umbrella framework.
   * Zero or more sub_umbrella frameworks may be use by an umbrella framework.
   * The name of a sub_umbrella framework is recorded in the following structure.
   *)
  sub_umbrella_command = record
    cmd: UInt32;          (* LC_SUB_UMBRELLA *)
    cmdsize: UInt32;      (* includes sub_umbrella string *)
    sub_umbrella: lc_str; (* the sub_umbrella framework name *)
  end;

type
  (*
   * A dynamically linked shared library may be a sub_library of another shared
   * library.  If so it will be linked with "-sub_library library_name" where
   * Where "library_name" is the name of the sub_library shared library.  When
   * staticly linking when -twolevel_namespace is in effect a twolevel namespace
   * shared library will only cause its subframeworks and those frameworks
   * listed as sub_umbrella frameworks and libraries listed as sub_libraries to
   * be implicited linked in.  Any other dependent dynamic libraries will not be
   * linked it when -twolevel_namespace is in effect.  The primary library
   * recorded by the static linker when resolving a symbol in these libraries
   * will be the umbrella framework (or dynamic library). Zero or more sub_library
   * shared libraries may be use by an umbrella framework or (or dynamic library).
   * The name of a sub_library framework is recorded in the following structure.
   * For example /usr/lib/libobjc_profile.A.dylib would be recorded as "libobjc".
   *)
  sub_library_command = record
    cmd: UInt32;         (* LC_SUB_LIBRARY *)
    cmdsize: UInt32;     (* includes sub_library string *)
    sub_library: lc_str; (* the sub_library name *)
  end;
  Psub_library_command = ^sub_library_command;

type
  (*
   * A program (filetype == MH_EXECUTE) that is
   * prebound to its dynamic libraries has one of these for each library that
   * the static linker used in prebinding.  It contains a bit vector for the
   * modules in the library.  The bits indicate which modules are bound (1) and
   * which are not (0) from the library.  The bit for module 0 is the low bit
   * of the first byte.  So the bit for the Nth module is:
   * (linked_modules[N/8] >> N%8) & 1
   *)
  prebound_dylib_command = record
    cmd: UInt32;            (* LC_PREBOUND_DYLIB *)
    cmdsize: UInt32;        (* includes strings *)
    name: lc_str;           (* library's path name *)
    nmodules: Int32;        (* number of modules in library *)
    linked_modules: lc_str; (* bit vector of linked modules *)
  end;
  Pprebound_dylib_command = ^prebound_dylib_command;

type
  (*
   * A program that uses a dynamic linker contains a dylinker_command to identify
   * the name of the dynamic linker (LC_LOAD_DYLINKER).  And a dynamic linker
   * contains a dylinker_command to identify the dynamic linker (LC_ID_DYLINKER).
   * A file can have at most one of these.
   * This struct is also used for the LC_DYLD_ENVIRONMENT load command and
   * contains string for dyld to treat like environment variable.
   *)
  dylinker_command = record
    cmd: UInt32;     (* LC_ID_DYLINKER, LC_LOAD_DYLINKER or LC_DYLD_ENVIRONMENT *)
    cmdsize: UInt32; (* includes pathname string *)
    name: lc_str;    (* dynamic linker's path name *)
  end;
  Pdylinker_command = ^dylinker_command;

type
  (*
   * Thread commands contain machine-specific data structures suitable for
   * use in the thread state primitives.  The machine specific data structures
   * follow the struct thread_command as follows.
   * Each flavor of machine specific data structure is preceded by an unsigned
   * long constant for the flavor of that data structure, an uint32_t
   * that is the count of longs of the size of the state data structure and then
   * the state data structure follows.  This triple may be repeated for many
   * flavors.  The constants for the flavors, counts and state data structure
   * definitions are expected to be in the header file <machine/thread_status.h>.
   * These machine specific data structures sizes must be multiples of
   * 4 bytes  The cmdsize reflects the total size of the thread_command
   * and all of the sizes of the constants for the flavors, counts and state
   * data structures.
   *
   * For executable objects that are unix processes there will be one
   * thread_command (cmd == LC_UNIXTHREAD) created for it by the link-editor.
   * This is the same as a LC_THREAD, except that a stack is automatically
   * created (based on the shell's limit for the stack size).  Command arguments
   * and environment variables are copied onto that stack.
   *)
  thread_command = record
    cmd: UInt32;  (* LC_THREAD or  LC_UNIXTHREAD *)
    cmdsize: UInt32; (* total size of this command *)
    (* uint32_t flavor     flavor of thread state *)
    (* uint32_t count     count of longs in thread state *)
    (* struct XXX_thread_state state   thread state for this flavor *)
    (* ... *)
  end;
  Pthread_command = ^thread_command;

type
  (*
   * The routines command contains the address of the dynamic shared library
   * initialization routine and an index into the module table for the module
   * that defines the routine.  Before any modules are used from the library the
   * dynamic linker fully binds the module that defines the initialization routine
   * and then calls it.  This gets called before any module initialization
   * routines (used for C++ static constructors) in the library.
   *)
  routines_command = record (* for 32-bit architectures *)
    cmd: UInt32;          (* LC_ROUTINES *)
    cmdsize: UInt32;      (* total size of this command *)
    init_address: UInt32; (* address of initialization routine *)
    init_module: UInt32;  (* index into the module table that
                             the init routine is defined in *)
    reserved1: UInt32;
    reserved2: UInt32;
    reserved3: UInt32;
    reserved4: UInt32;
    reserved5: UInt32;
    reserved6: UInt32;
  end;
  Proutines_command = ^routines_command;

type
  (*
   * The 64-bit routines command.  Same use as above.
   *)
  routines_command_64 = record (* for 64-bit architectures *)
    cmd: UInt32;          (* LC_ROUTINE_64 *)
    cmdsize: UInt32;      (* total size of this command *)
    init_address: UInt64; (* address of initialization routine *)
    init_module: UInt64;  (* index into the module table that
                             the init routine is defined in *)
    reserved1: UInt64;
    reserved2: UInt64;
    reserved3: UInt64;
    reserved4: UInt64;
    reserved5: UInt64;
    reserved6: UInt64;
  end;
  Proutines_command_64 = ^routines_command_64;

type
  (*
   * The symtab_command contains the offsets and sizes of the link-edit 4.3BSD
   * "stab" style symbol table information as described in the header files
   * <nlist.h> and <stab.h>.
   *)
  symtab_command = record
    cmd: UInt32;     (* LC_SYMTAB *)
    cmdsize: UInt32; (* sizeof(struct symtab_command) *)
    symoff: UInt32;  (* symbol table offset *)
    nsyms: Int32;    (* number of symbol table entries *)
    stroff: UInt32;  (* string table offset *)
    strsize: UInt32; (* string table size in bytes *)
  end;
  Psymtab_command = ^symtab_command;

type
  (*
   * This is the second set of the symbolic information which is used to support
   * the data structures for the dynamically link editor.
   *
   * The original set of symbolic information in the symtab_command which contains
   * the symbol and string tables must also be present when this load command is
   * present.  When this load command is present the symbol table is organized
   * into three groups of symbols:
   * local symbols (static and debugging symbols) - grouped by module
   * defined external symbols - grouped by module (sorted by name if not lib)
   * undefined external symbols (sorted by name if MH_BINDATLOAD is not set,
   *             and in order the were seen by the static
   *        linker if MH_BINDATLOAD is set)
   * In this load command there are offsets and counts to each of the three groups
   * of symbols.
   *
   * This load command contains a the offsets and sizes of the following new
   * symbolic information tables:
   * table of contents
   * module table
   * reference symbol table
   * indirect symbol table
   * The first three tables above (the table of contents, module table and
   * reference symbol table) are only present if the file is a dynamically linked
   * shared library.  For executable and object modules, which are files
   * containing only one module, the information that would be in these three
   * tables is determined as follows:
   *  table of contents - the defined external symbols are sorted by name
   * module table - the file contains only one module so everything in the
   *         file is part of the module.
   * reference symbol table - is the defined and undefined external symbols
   *
   * For dynamically linked shared library files this load command also contains
   * offsets and sizes to the pool of relocation entries for all sections
   * separated into two groups:
   * external relocation entries
   * local relocation entries
   * For executable and object modules the relocation entries continue to hang
   * off the section structures.
   *)
  dysymtab_command = record
    cmd: UInt32;     (* LC_DYSYMTAB *)
    cmdsize: UInt32; (* sizeof(struct dysymtab_command) *)

    (*
     * The symbols indicated by symoff and nsyms of the LC_SYMTAB load command
     * are grouped into the following three groups:
     *    local symbols (further grouped by the module they are from)
     *    defined external symbols (further grouped by the module they are from)
     *    undefined symbols
     *
     * The local symbols are used only for debugging.  The dynamic binding
     * process may have to use them to indicate to the debugger the local
     * symbols for a module that is being bound.
     *
     * The last two groups are used by the dynamic binding process to do the
     * binding (indirectly through the module table and the reference symbol
     * table when this is a dynamically linked shared library file).
     *)
    ilocalsym: UInt32; (* index to local symbols *)
    nlocalsym: Int32;  (* number of local symbols *)

    iextdefsym: UInt32;(* index to externally defined symbols *)
    nextdefsym: Int32; (* number of externally defined symbols *)

    iundefsym: UInt32; (* index to undefined symbols *)
    nundefsym: Int32;  (* number of undefined symbols *)

    (*
     * For the for the dynamic binding process to find which module a symbol
     * is defined in the table of contents is used (analogous to the ranlib
     * structure in an archive) which maps defined external symbols to modules
     * they are defined in.  This exists only in a dynamically linked shared
     * library file.  For executable and object modules the defined external
     * symbols are sorted by name and is use as the table of contents.
     *)
    tocoff: UInt32; (* file offset to table of contents *)
    ntoc: Int32;    (* number of entries in table of contents *)

    (*
     * To support dynamic binding of "modules" (whole object files) the symbol
     * table must reflect the modules that the file was created from.  This is
     * done by having a module table that has indexes and counts into the merged
     * tables for each module.  The module structure that these two entries
     * refer to is described below.  This exists only in a dynamically linked
     * shared library file.  For executable and object modules the file only
     * contains one module so everything in the file belongs to the module.
     *)
    modtaboff: UInt32; (* file offset to module table *)
    nmodtab: Int32;    (* number of module table entries *)

    (*
     * To support dynamic module binding the module structure for each module
     * indicates the external references (defined and undefined) each module
     * makes.  For each module there is an offset and a count into the
     * reference symbol table for the symbols that the module references.
     * This exists only in a dynamically linked shared library file.  For
     * executable and object modules the defined external symbols and the
     * undefined external symbols indicates the external references.
     *)
    extrefsymoff: UInt32; (* offset to referenced symbol table *)
    nextrefsyms: Int32;   (* number of referenced symbol table entries *)

    (*
     * The sections that contain "symbol pointers" and "routine stubs" have
     * indexes and (implied counts based on the size of the section and fixed
     * size of the entry) into the "indirect symbol" table for each pointer
     * and stub.  For every section of these two types the index into the
     * indirect symbol table is stored in the section header in the field
     * reserved1.  An indirect symbol table entry is simply a 32bit index into
     * the symbol table to the symbol that the pointer or stub is referring to.
     * The indirect symbol table is ordered to match the entries in the section.
     *)
    indirectsymoff: UInt32; (* file offset to the indirect symbol table *)
    nindirectsyms: Int32;   (* number of indirect symbol table entries *)

    (*
     * To support relocating an individual module in a library file quickly the
     * external relocation entries for each module in the library need to be
     * accessed efficiently.  Since the relocation entries can't be accessed
     * through the section headers for a library file they are separated into
     * groups of local and external entries further grouped by module.  In this
     * case the presents of this load command who's extreloff, nextrel,
     * locreloff and nlocrel fields are non-zero indicates that the relocation
     * entries of non-merged sections are not referenced through the section
     * structures (and the reloff and nreloc fields in the section headers are
     * set to zero).
     *
     * Since the relocation entries are not accessed through the section headers
     * this requires the r_address field to be something other than a section
     * offset to identify the item to be relocated.  In this case r_address is
     * set to the offset from the vmaddr of the first LC_SEGMENT command.
     * For MH_SPLIT_SEGS images r_address is set to the the offset from the
     * vmaddr of the first read-write LC_SEGMENT command.
     *
     * The relocation entries are grouped by module and the module table
     * entries have indexes and counts into them for the group of external
     * relocation entries for that the module.
     *
     * For sections that are merged across modules there must not be any
     * remaining external relocation entries for them (for merged sections
     * remaining relocation entries must be local).
     *)
    extreloff: UInt32; (* offset to external relocation entries *)
    nextrel: Int32;    (* number of external relocation entries *)

    (*
     * All the local relocation entries are grouped together (they are not
     * grouped by their module since they are only used if the object is moved
     * from it staticly link edited address).
     *)
    locreloff: UInt32; (* offset to local relocation entries *)
    nlocrel: Int32;    (* number of local relocation entries *)
  end;
  Pdysymtab_command = ^dysymtab_command;

const
  (*
   * An indirect symbol table entry is simply a 32bit index into the symbol table
   * to the symbol that the pointer or stub is refering to.  Unless it is for a
   * non-lazy symbol pointer section for a defined symbol which strip(1) as
   * removed.  In which case it has the value INDIRECT_SYMBOL_LOCAL.  If the
   * symbol was also absolute INDIRECT_SYMBOL_ABS is or'ed with that.
   *)
  INDIRECT_SYMBOL_LOCAL = $80000000;
  INDIRECT_SYMBOL_ABS   = $40000000;

type
  (* a table of contents entry *)
  dylib_table_of_contents = record
    symbol_index: UInt32; (* the defined external symbol
                             (index into the symbol table) *)
    module_index: UInt32; (* index into the module table this symbol
                             is defined in *)
  end;
  Pdylib_table_of_contents = ^dylib_table_of_contents;

type
  (* a module table entry *)
  dylib_module = record
    module_name: UInt32; (* the module name (index into string table) *)

    iextdefsym: UInt32;  (* index into externally defined symbols *)
    nextdefsym: Int32;   (* number of externally defined symbols *)
    irefsym: UInt32;     (* index into reference symbol table *)
    nrefsym: Int32;      (* number of reference symbol table entries *)
    ilocalsym: UInt32;   (* index into symbols for local symbols *)
    nlocalsym: Int32;    (* number of local symbols *)

    iextrel: UInt32;     (* index into external relocation entries *)
    nextrel: Int32;      (* number of external relocation entries *)

    iinit_iterm: UInt32; (* low 16 bits are the index into the init
                            section, high 16 bits are the index into
                            the term section *)
    ninit_nterm: UInt32; (* low 16 bits are the number of init section
                            entries, high 16 bits are the number of
                            term section entries *)

    objc_module_info_addr: UInt32; (* for this module address of the start of
                                      the (__OBJC,__module_info) section *)
    objc_module_info_size: UInt32; (* for this module size of the
                                      (__OBJC,__module_info) section *)
  end;
  Pdylib_module = ^dylib_module;

type
  (* a 64-bit module table entry *)
  dylib_module_64 = record
    module_name: UInt32; (* the module name (index into string table) *)

    iextdefsym: UInt32;  (* index into externally defined symbols *)
    nextdefsym: Int32;   (* number of externally defined symbols *)
    irefsym: UInt32;     (* index into reference symbol table *)
    nrefsym: Int32;      (* number of reference symbol table entries *)
    ilocalsym: UInt32;   (* index into symbols for local symbols *)
    nlocalsym: Int32;    (* number of local symbols *)

    iextrel: UInt32;     (* index into external relocation entries *)
    nextrel: Int32;      (* number of external relocation entries *)

    iinit_iterm: UInt32; (* low 16 bits are the index into the init
                            section, high 16 bits are the index into
                            the term section *)
    ninit_nterm: UInt32; (* low 16 bits are the number of init section
                            entries, high 16 bits are the number of
                            term section entries *)

    objc_module_info_size: UInt32; (* for this module size of the
                                      (__OBJC,__module_info) section *)
    objc_module_info_addr: UInt64; (* for this module address of the start of
                                      the (__OBJC,__module_info) section *)
  end;
  Pdylib_module_64 = ^dylib_module_64;

type
  (*
   * The entries in the reference symbol table are used when loading the module
   * (both by the static and dynamic link editors) and if the module is unloaded
   * or replaced.  Therefore all external symbols (defined and undefined) are
   * listed in the module's reference table.  The flags describe the type of
   * reference that is being made.  The constants for the flags are defined in
   * <mach-o/nlist.h> as they are also used for symbol table entries.
   *)
  dylib_reference = record
    isym_and_flags: UInt32; (* lower 24 bits: index into the symbol table
                               upper 8 bits: flags to indicate the type of reference *)
  end;
  Pdylib_reference = ^dylib_reference;

type
  (*
   * The twolevel_hints_command contains the offset and number of hints in the
   * two-level namespace lookup hints table.
   *)
  twolevel_hints_command = record
    cmd: UInt32;     (* LC_TWOLEVEL_HINTS *)
    cmdsize: UInt32; (* sizeof(struct twolevel_hints_command) *)
    offset: UInt32;  (* offset to the hint table *)
    nhints: Int32;   (* number of hints in the hint table *)
  end;
  Ptwolevel_hints_command = ^twolevel_hints_command;

type
  (*
   * The entries in the two-level namespace lookup hints table are twolevel_hint
   * structs.  These provide hints to the dynamic link editor where to start
   * looking for an undefined symbol in a two-level namespace image.  The
   * isub_image field is an index into the sub-images (sub-frameworks and
   * sub-umbrellas list) that made up the two-level image that the undefined
   * symbol was found in when it was built by the static link editor.  If
   * isub-image is 0 the the symbol is expected to be defined in library and not
   * in the sub-images.  If isub-image is non-zero it is an index into the array
   * of sub-images for the umbrella with the first index in the sub-images being
   * 1. The array of sub-images is the ordered list of sub-images of the umbrella
   * that would be searched for a symbol that has the umbrella recorded as its
   * primary library.  The table of contents index is an index into the
   * library's table of contents.  This is used as the starting point of the
   * binary search or a directed linear search.
   *)
  twolevel_hint = record
    isub_image_and_itoc: UInt32; (* Lower 8 bits: index into the sub images
                                    Upper 24 bits: index into the table of contents *)
  end;
  Ptwolevel_hint = ^twolevel_hint;

type
  (*
   * The prebind_cksum_command contains the value of the original check sum for
   * prebound files or zero.  When a prebound file is first created or modified
   * for other than updating its prebinding information the value of the check sum
   * is set to zero.  When the file has it prebinding re-done and if the value of
   * the check sum is zero the original check sum is calculated and stored in
   * cksum field of this load command in the output file.  If when the prebinding
   * is re-done and the cksum field is non-zero it is left unchanged from the
   * input file.
   *)
  prebind_cksum_command = record
    cmd: UInt32;     (* LC_PREBIND_CKSUM *)
    cmdsize: UInt32; (* sizeof(struct prebind_cksum_command) *)
    cksum: UInt32;   (* the check sum or zero *)
  end;
  Pprebind_cksum_command = ^prebind_cksum_command;

type
  (*
   * The uuid load command contains a single 128-bit unique random number that
   * identifies an object produced by the static link editor.
   *)
  uuid_command = record
    cmd: UInt32;     (* LC_UUID *)
    cmdsize: UInt32; (* sizeof(struct uuid_command) *)
    uuid: array [0..15] of Byte; (* the 128-bit uuid *)
  end;
  Puuid_command = ^uuid_command;

type
  (*
   * The rpath_command contains a path which at runtime should be added to
   * the current run path used to find @rpath prefixed dylibs.
   *)
  rpath_command = record
    cmd: UInt32;     (* LC_RPATH *)
    cmdsize: UInt32; (* includes string *)
    path: lc_str;    (* path to add to run path *)
  end;
  Prpath_command = ^rpath_command;

type
  (*
   * The linkedit_data_command contains the offsets and sizes of a blob
   * of data in the __LINKEDIT segment.
   *)
  linkedit_data_command = record
    cmd: UInt32;      (* LC_CODE_SIGNATURE, LC_SEGMENT_SPLIT_INFO,
                         LC_FUNCTION_STARTS, LC_DATA_IN_CODE,
                         LC_DYLIB_CODE_SIGN_DRS or
                         LC_LINKER_OPTIMIZATION_HINT. *)
    cmdsize: UInt32;  (* sizeof(struct linkedit_data_command) *)
    dataoff: UInt32;  (* file offset of data in __LINKEDIT segment *)
    datasize: UInt32; (* file size of data in __LINKEDIT segment  *)
  end;
  Plinkedit_data_command = ^linkedit_data_command;

type
  (*
   * The encryption_info_command contains the file offset and size of an
   * of an encrypted segment.
   *)
  encryption_info_command = record
    cmd: UInt32;       (* LC_ENCRYPTION_INFO *)
    cmdsize: UInt32;   (* sizeof(struct encryption_info_command) *)
    cryptoff: UInt32;  (* file offset of encrypted range *)
    cryptsize: UInt32; (* file size of encrypted range *)
    cryptid: UInt32;   (* which enryption system,
                          0 means not-encrypted yet *)
  end;
  Pencryption_info_command = ^encryption_info_command;

type
  (*
   * The encryption_info_command_64 contains the file offset and size of an
   * of an encrypted segment (for use in x86_64 targets).
   *)
  encryption_info_command_64 = record
    cmd: UInt32;       (* LC_ENCRYPTION_INFO_64 *)
    cmdsize: UInt32;   (* sizeof(struct encryption_info_command) *)
    cryptoff: UInt32;  (* file offset of encrypted range *)
    cryptsize: UInt32; (* file size of encrypted range *)
    cryptid: UInt32;   (* which enryption system,
                          0 means not-encrypted yet *)
    pad: UInt32;       (* padding to make this struct's size a multiple
                          of 8 bytes *)
  end;
  Pencryption_info_command_64 = ^encryption_info_command_64;

type
  (*
   * The version_min_command contains the min OS version on which this
   * binary was built to run.
   *)
  version_min_command = record
    cmd: UInt32;     (* LC_VERSION_MIN_MACOSX or LC_VERSION_MIN_IPHONEOS or LC_VERSION_MIN_WATCHOS or LC_VERSION_MIN_TVOS *)
    cmdsize: UInt32; (* sizeof(struct min_version_command) *)
    version: UInt32; (* X.Y.Z is encoded in nibbles xxxx.yy.zz *)
    sdk: UInt32;     (* X.Y.Z is encoded in nibbles xxxx.yy.zz *)
  end;
  Pversion_min_command = ^version_min_command;

type
  (*
   * The dyld_info_command contains the file offsets and sizes of
   * the new compressed form of the information dyld needs to
   * load the image.  This information is used by dyld on Mac OS X
   * 10.6 and later.  All information pointed to by this command
   * is encoded using byte streams, so no endian swapping is needed
   * to interpret it.
   *)
  dyld_info_command = record
    cmd: UInt32;     (* LC_DYLD_INFO or LC_DYLD_INFO_ONLY *)
    cmdsize: UInt32; (* sizeof(struct dyld_info_command) *)

    (*
     * Dyld rebases an image whenever dyld loads it at an address different
     * from its preferred address.  The rebase information is a stream
     * of byte sized opcodes whose symbolic names start with REBASE_OPCODE_.
     * Conceptually the rebase information is a table of tuples:
     *    <seg-index, seg-offset, type>
     * The opcodes are a compressed way to encode the table by only
     * encoding when a column changes.  In addition simple patterns
     * like "every n'th offset for m times" can be encoded in a few
     * bytes.
     *)
    rebase_off: UInt32;  (* file offset to rebase info  *)
    rebase_size: UInt32; (* size of rebase info   *)

    (*
     * Dyld binds an image during the loading process, if the image
     * requires any pointers to be initialized to symbols in other images.
     * The bind information is a stream of byte sized
     * opcodes whose symbolic names start with BIND_OPCODE_.
     * Conceptually the bind information is a table of tuples:
     *    <seg-index, seg-offset, type, symbol-library-ordinal, symbol-name, addend>
     * The opcodes are a compressed way to encode the table by only
     * encoding when a column changes.  In addition simple patterns
     * like for runs of pointers initialzed to the same value can be
     * encoded in a few bytes.
     *)
    bind_off: UInt32;  (* file offset to binding info   *)
    bind_size: UInt32; (* size of binding info  *)

    (*
     * Some C++ programs require dyld to unique symbols so that all
     * images in the process use the same copy of some code/data.
     * This step is done after binding. The content of the weak_bind
     * info is an opcode stream like the bind_info.  But it is sorted
     * alphabetically by symbol name.  This enable dyld to walk
     * all images with weak binding information in order and look
     * for collisions.  If there are no collisions, dyld does
     * no updating.  That means that some fixups are also encoded
     * in the bind_info.  For instance, all calls to "operator new"
     * are first bound to libstdc++.dylib using the information
     * in bind_info.  Then if some image overrides operator new
     * that is detected when the weak_bind information is processed
     * and the call to operator new is then rebound.
     *)
    weak_bind_off: UInt32;  (* file offset to weak binding info   *)
    weak_bind_size: UInt32; (* size of weak binding info  *)

    (*
     * Some uses of external symbols do not need to be bound immediately.
     * Instead they can be lazily bound on first use.  The lazy_bind
     * are contains a stream of BIND opcodes to bind all lazy symbols.
     * Normal use is that dyld ignores the lazy_bind section when
     * loading an image.  Instead the static linker arranged for the
     * lazy pointer to initially point to a helper function which
     * pushes the offset into the lazy_bind area for the symbol
     * needing to be bound, then jumps to dyld which simply adds
     * the offset to lazy_bind_off to get the information on what
     * to bind.
     *)
    lazy_bind_off: UInt32;  (* file offset to lazy binding info *)
    lazy_bind_size: UInt32; (* size of lazy binding infs *)

    (*
     * The symbols exported by a dylib are encoded in a trie.  This
     * is a compact representation that factors out common prefixes.
     * It also reduces LINKEDIT pages in RAM because it encodes all
     * information (name, address, flags) in one small, contiguous range.
     * The export area is a stream of nodes.  The first node sequentially
     * is the start node for the trie.
     *
     * Nodes for a symbol start with a uleb128 that is the length of
     * the exported symbol information for the string so far.
     * If there is no exported symbol, the node starts with a zero byte.
     * If there is exported info, it follows the length.
     *
     * First is a uleb128 containing flags. Normally, it is followed by
     * a uleb128 encoded offset which is location of the content named
     * by the symbol from the mach_header for the image.  If the flags
     * is EXPORT_SYMBOL_FLAGS_REEXPORT, then following the flags is
     * a uleb128 encoded library ordinal, then a zero terminated
     * UTF8 string.  If the string is zero length, then the symbol
     * is re-export from the specified dylib with the same name.
     * If the flags is EXPORT_SYMBOL_FLAGS_STUB_AND_RESOLVER, then following
     * the flags is two uleb128s: the stub offset and the resolver offset.
     * The stub is used by non-lazy pointers.  The resolver is used
     * by lazy pointers and must be called to get the actual address to use.
     *
     * After the optional exported symbol information is a byte of
     * how many edges (0-255) that this node has leaving it,
     * followed by each edge.
     * Each edge is a zero terminated UTF8 of the addition chars
     * in the symbol, followed by a uleb128 offset for the node that
     * edge points to.
     *)
    export_off: UInt32;  (* file offset to lazy binding info *)
    export_size: UInt32; (* size of lazy binding infs *)
  end;
  Pdyld_info_command = ^dyld_info_command;

const
  (*
   * The following are used to encode rebasing information
   *)
  REBASE_TYPE_POINTER         = 1;
  REBASE_TYPE_TEXT_ABSOLUTE32 = 2;
  REBASE_TYPE_TEXT_PCREL32    = 3;

  REBASE_OPCODE_MASK                               = $F0;
  REBASE_IMMEDIATE_MASK                            = $0F;
  REBASE_OPCODE_DONE                               = $00;
  REBASE_OPCODE_SET_TYPE_IMM                       = $10;
  REBASE_OPCODE_SET_SEGMENT_AND_OFFSET_ULEB        = $20;
  REBASE_OPCODE_ADD_ADDR_ULEB                      = $30;
  REBASE_OPCODE_ADD_ADDR_IMM_SCALED                = $40;
  REBASE_OPCODE_DO_REBASE_IMM_TIMES                = $50;
  REBASE_OPCODE_DO_REBASE_ULEB_TIMES               = $60;
  REBASE_OPCODE_DO_REBASE_ADD_ADDR_ULEB            = $70;
  REBASE_OPCODE_DO_REBASE_ULEB_TIMES_SKIPPING_ULEB = $80;

const
  (*
   * The following are used to encode binding information
   *)
  BIND_TYPE_POINTER         = 1;
  BIND_TYPE_TEXT_ABSOLUTE32 = 2;
  BIND_TYPE_TEXT_PCREL32    = 3;

  BIND_SPECIAL_DYLIB_SELF            =  0;
  BIND_SPECIAL_DYLIB_MAIN_EXECUTABLE = -1;
  BIND_SPECIAL_DYLIB_FLAT_LOOKUP     = -2;

  BIND_SYMBOL_FLAGS_WEAK_IMPORT         = $1;
  BIND_SYMBOL_FLAGS_NON_WEAK_DEFINITION = $8;

  BIND_OPCODE_MASK                              = $F0;
  BIND_IMMEDIATE_MASK                           = $0F;
  BIND_OPCODE_DONE                              = $00;
  BIND_OPCODE_SET_DYLIB_ORDINAL_IMM             = $10;
  BIND_OPCODE_SET_DYLIB_ORDINAL_ULEB            = $20;
  BIND_OPCODE_SET_DYLIB_SPECIAL_IMM             = $30;
  BIND_OPCODE_SET_SYMBOL_TRAILING_FLAGS_IMM     = $40;
  BIND_OPCODE_SET_TYPE_IMM                      = $50;
  BIND_OPCODE_SET_ADDEND_SLEB                   = $60;
  BIND_OPCODE_SET_SEGMENT_AND_OFFSET_ULEB       = $70;
  BIND_OPCODE_ADD_ADDR_ULEB                     = $80;
  BIND_OPCODE_DO_BIND                           = $90;
  BIND_OPCODE_DO_BIND_ADD_ADDR_ULEB             = $A0;
  BIND_OPCODE_DO_BIND_ADD_ADDR_IMM_SCALED       = $B0;
  BIND_OPCODE_DO_BIND_ULEB_TIMES_SKIPPING_ULEB  = $C0;

const
  (*
   * The following are used on the flags byte of a terminal node
   * in the export information.
   *)
  EXPORT_SYMBOL_FLAGS_KIND_MASK         = $03;
  EXPORT_SYMBOL_FLAGS_KIND_REGULAR      = $00;
  EXPORT_SYMBOL_FLAGS_KIND_THREAD_LOCAL = $01;
  EXPORT_SYMBOL_FLAGS_WEAK_DEFINITION   = $04;
  EXPORT_SYMBOL_FLAGS_REEXPORT          = $08;
  EXPORT_SYMBOL_FLAGS_STUB_AND_RESOLVER = $10;

type
  (*
   * The linker_option_command contains linker options embedded in object files.
   *)
  linker_option_command = record
    cmd: UInt32;     (* LC_LINKER_OPTION only used in MH_OBJECT filetypes *)
    cmdsize: UInt32;
    count: Int32;    (* number of strings *)
                     (* concatenation of zero terminated UTF8 strings.
                        Zero filled at end to align *)
  end;
  Plinker_option_command = ^linker_option_command;

type
  (*
   * The symseg_command contains the offset and size of the GNU style
   * symbol table information as described in the header file <symseg.h>.
   * The symbol roots of the symbol segments must also be aligned properly
   * in the file.  So the requirement of keeping the offsets aligned to a
   * multiple of a 4 bytes translates to the length field of the symbol
   * roots also being a multiple of a long.  Also the padding must again be
   * zeroed. (THIS IS OBSOLETE and no longer supported).
   *)
  symseg_command = record
    cmd: UInt32;     (* LC_SYMSEG *)
    cmdsize: UInt32; (* sizeof(struct symseg_command) *)
    offset: UInt32;  (* symbol segment offset *)
    size: UInt32;    (* symbol segment size in bytes *)
  end;
  Psymseg_command = ^symseg_command;

type
  (*
   * The ident_command contains a free format string table following the
   * ident_command structure.  The strings are null terminated and the size of
   * the command is padded out with zero bytes to a multiple of 4 bytes/
   * (THIS IS OBSOLETE and no longer supported).
   *)
  ident_command = record
    cmd: UInt32;     (* LC_IDENT *)
    cmdsize: UInt32; (* strings that follow this command *)
  end;
  Pident_command = ^ident_command;

type
  (*
   * The fvmfile_command contains a reference to a file to be loaded at the
   * specified virtual address.  (Presently, this command is reserved for
   * internal use.  The kernel ignores this command when loading a program into
   * memory).
   *)
  fvmfile_command = record
    cmd: UInt32;         (* LC_FVMFILE *)
    cmdsize: UInt32;     (* includes pathname string *)
    name: lc_str;        (* files pathname *)
    header_addr: UInt32; (* files virtual address *)
  end;
  Pfvmfile_command = ^fvmfile_command;

type
  (*
   * The entry_point_command is a replacement for thread_command.
   * It is used for main executables to specify the location (file offset)
   * of main().  If -stack_size was used at link time, the stacksize
   * field will contain the stack size need for the main thread.
   *)
  entry_point_command = record
    cmd: UInt32;       (* LC_MAIN only used in MH_EXECUTE filetypes *)
    cmdsize: UInt32;   (* 24 *)
    entryoff: UInt64;  (* file (__TEXT) offset of main() *)
    stacksize: UInt64; (* if not zero, initial stack size *)
  end;
  Pentry_point_command = ^entry_point_command;

type
  (*
   * The source_version_command is an optional load command containing
   * the version of the sources used to build the binary.
   *)
  source_version_command = record
    cmd: UInt32;     (* LC_SOURCE_VERSION *)
    cmdsize: UInt32; (* 16 *)
    version: UInt64; (* A.B.C.D.E packed as a24.b10.c10.d10.e10 *)
  end;
  Psource_version_command = ^source_version_command;

type
  (*
   * The LC_DATA_IN_CODE load commands uses a linkedit_data_command
   * to point to an array of data_in_code_entry entries. Each entry
   * describes a range of data in a code section.
   *)
  data_in_code_entry = record
    offset: UInt32;  (* from mach_header to start of data range*)
    length: UInt16;  (* number of bytes in data range *)
    kind: UInt16;    (* a DICE_KIND_* value  *)
  end;
  Pdata_in_code_entry = ^data_in_code_entry;

const
  DICE_KIND_DATA              = $0001;
  DICE_KIND_JUMP_TABLE8       = $0002;
  DICE_KIND_JUMP_TABLE16      = $0003;
  DICE_KIND_JUMP_TABLE32      = $0004;
  DICE_KIND_ABS_JUMP_TABLE32  = $0005;

type
  (*
   * Sections of type S_THREAD_LOCAL_VARIABLES contain an array
   * of tlv_descriptor structures.
   *)
  Ptlv_descriptor = ^tlv_descriptor;
  tlv_descriptor = record
    thunk: function (descriptor: Ptlv_descriptor): Pointer; cdecl;
    key: Cardinal;
    offset: Cardinal;
  end;
{$ENDREGION 'loader.h'}

{$REGION 'nlist.h'}
type
  (*
   * Format of a symbol table entry of a Mach-O file for 32-bit architectures.
   * Modified from the BSD format.  The modifications from the original format
   * were changing n_other (an unused field) to n_sect and the addition of the
   * N_SECT type.  These modifications are required to support symbols in a larger
   * number of sections not just the three sections (text, data and bss) in a BSD
   * file.
   *)
  nlist = record
    n_un: record
      {$IFDEF CPU32BITS}
      n_name: PAnsiChar; (* for use when in-core *)
      {$ENDIF}
      n_strx: UInt32;    (* index into the string table *)
    end;
   n_type: UInt8;   (* type flag, see below *)
   n_sect: UInt8;   (* section number or NO_SECT *)
   n_desc: Int16;   (* see <mach-o/stab.h> *)
   n_value: UInt32; (* value of this symbol (or stab offset) *)
  end;
  Pnlist = ^nlist;

type
  (*
   * This is the symbol table entry structure for 64-bit architectures.
   *)
  nlist_64 = record
    n_un: record
      n_strx: UInt32;    (* index into the string table *)
    end;
   n_type: UInt8;   (* type flag, see below *)
   n_sect: UInt8;   (* section number or NO_SECT *)
   n_desc: Int16;   (* see <mach-o/stab.h> *)
   n_value: UInt64; (* value of this symbol (or stab offset) *)
  end;
  Pnlist_64 = ^nlist_64;

const
  (*
   * Symbols with a index into the string table of zero (n_un.n_strx == 0) are
   * defined to have a null, "", name.  Therefore all string indexes to non null
   * names must not have a zero string index.  This is bit historical information
   * that has never been well documented.
   *)

  (*
   * The n_type field really contains four fields:
   * unsigned char N_STAB:3,
   *        N_PEXT:1,
   *        N_TYPE:3,
   *        N_EXT:1;
   * which are used via the following masks.
   *)
  N_STAB = $e0; (* if any of these bits set, a symbolic debugging entry *)
  N_PEXT = $10; (* private external symbol bit *)
  N_TYPE = $0e; (* mask for the type bits *)
  N_EXT  = $01; (* external symbol bit, set for external symbols *)

const
  (*
   * Only symbolic debugging entries have some of the N_STAB bits set and if any
   * of these bits are set then it is a symbolic debugging entry (a stab).  In
   * which case then the values of the n_type field (the entire field) are given
   * in <mach-o/stab.h>
   *)

  (*
   * Values for N_TYPE bits of the n_type field.
   *)
  N_UNDF = $0; (* undefined, n_sect == NO_SECT *)
  N_ABS  = $2; (* absolute, n_sect == NO_SECT *)
  N_SECT = $e; (* defined in section number n_sect *)
  N_PBUD = $c; (* prebound undefined (defined in a dylib) *)
  N_INDR = $a; (* indirect *)

const
  (*
   * If the type is N_INDR then the symbol is defined to be the same as another
   * symbol.  In this case the n_value field is an index into the string table
   * of the other symbol's name.  When the other symbol is defined then they both
   * take on the defined type and value.
   *)

  (*
   * If the type is N_SECT then the n_sect field contains an ordinal of the
   * section the symbol is defined in.  The sections are numbered from 1 and
   * refer to sections in order they appear in the load commands for the file
   * they are in.  This means the same ordinal may very well refer to different
   * sections in different files.
   *
   * The n_value field for all symbol table entries (including N_STAB's) gets
   * updated by the link editor based on the value of it's n_sect field and where
   * the section n_sect references gets relocated.  If the value of the n_sect
   * field is NO_SECT then it's n_value field is not changed by the link editor.
   *)
  NO_SECT  = 0;   (* symbol is not in any section *)
  MAX_SECT = 255; (* 1 thru 255 inclusive *)

(*
 * Common symbols are represented by undefined (N_UNDF) external (N_EXT) types
 * who's values (n_value) are non-zero.  In which case the value of the n_value
 * field is the size (in bytes) of the common symbol.  The n_sect field is set
 * to NO_SECT.  The alignment of a common symbol may be set as a power of 2
 * between 2^1 and 2^15 as part of the n_desc field using the macros below. If
 * the alignment is not set (a value of zero) then natural alignment based on
 * the size is used.
 *)
function GET_COMM_ALIGN(n_desc: UInt32): UInt32; inline;
procedure SET_COMM_ALIGN(var n_desc: UInt32; align: UInt32); inline;

const
  (*
   * To support the lazy binding of undefined symbols in the dynamic link-editor,
   * the undefined symbols in the symbol table (the nlist structures) are marked
   * with the indication if the undefined reference is a lazy reference or
   * non-lazy reference.  If both a non-lazy reference and a lazy reference is
   * made to the same symbol the non-lazy reference takes precedence.  A reference
   * is lazy only when all references to that symbol are made through a symbol
   * pointer in a lazy symbol pointer section.
   *
   * The implementation of marking nlist structures in the symbol table for
   * undefined symbols will be to use some of the bits of the n_desc field as a
   * reference type.  The mask REFERENCE_TYPE will be applied to the n_desc field
   * of an nlist structure for an undefined symbol to determine the type of
   * undefined reference (lazy or non-lazy).
   *
   * The constants for the REFERENCE FLAGS are propagated to the reference table
   * in a shared library file.  In that case the constant for a defined symbol,
   * REFERENCE_FLAG_DEFINED, is also used.
   *)
  (* Reference type bits of the n_desc field of undefined symbols *)
  REFERENCE_TYPE                            = $7;
  (* types of references *)
  REFERENCE_FLAG_UNDEFINED_NON_LAZY         = 0;
  REFERENCE_FLAG_UNDEFINED_LAZY             = 1;
  REFERENCE_FLAG_DEFINED                    = 2;
  REFERENCE_FLAG_PRIVATE_DEFINED            = 3;
  REFERENCE_FLAG_PRIVATE_UNDEFINED_NON_LAZY = 4;
  REFERENCE_FLAG_PRIVATE_UNDEFINED_LAZY     = 5;

const
  (*
   * To simplify stripping of objects that use are used with the dynamic link
   * editor, the static link editor marks the symbols defined an object that are
   * referenced by a dynamicly bound object (dynamic shared libraries, bundles).
   * With this marking strip knows not to strip these symbols.
   *)
  REFERENCED_DYNAMICALLY = $0010;

(*
 * For images created by the static link editor with the -twolevel_namespace
 * option in effect the flags field of the mach header is marked with
 * MH_TWOLEVEL.  And the binding of the undefined references of the image are
 * determined by the static link editor.  Which library an undefined symbol is
 * bound to is recorded by the static linker in the high 8 bits of the n_desc
 * field using the SET_LIBRARY_ORDINAL macro below.  The ordinal recorded
 * references the libraries listed in the Mach-O's LC_LOAD_DYLIB,
 * LC_LOAD_WEAK_DYLIB, LC_REEXPORT_DYLIB, LC_LOAD_UPWARD_DYLIB, and
 * LC_LAZY_LOAD_DYLIB, etc. load commands in the order they appear in the
 * headers.   The library ordinals start from 1.
 * For a dynamic library that is built as a two-level namespace image the
 * undefined references from module defined in another use the same nlist struct
 * an in that case SELF_LIBRARY_ORDINAL is used as the library ordinal.  For
 * defined symbols in all images they also must have the library ordinal set to
 * SELF_LIBRARY_ORDINAL.  The EXECUTABLE_ORDINAL refers to the executable
 * image for references from plugins that refer to the executable that loads
 * them.
 *
 * The DYNAMIC_LOOKUP_ORDINAL is for undefined symbols in a two-level namespace
 * image that are looked up by the dynamic linker with flat namespace semantics.
 * This ordinal was added as a feature in Mac OS X 10.3 by reducing the
 * value of MAX_LIBRARY_ORDINAL by one.  So it is legal for existing binaries
 * or binaries built with older tools to have 0xfe (254) dynamic libraries.  In
 * this case the ordinal value 0xfe (254) must be treated as a library ordinal
 * for compatibility.
 *)
function GET_LIBRARY_ORDINAL(n_desc: UInt32): UInt32; inline;
procedure SET_LIBRARY_ORDINAL(var n_desc: UInt32; ordinal: UInt32); inline;

const
  SELF_LIBRARY_ORDINAL   = $0;
  MAX_LIBRARY_ORDINAL    = $fd;
  DYNAMIC_LOOKUP_ORDINAL = $fe;
  EXECUTABLE_ORDINAL     = $ff;

const
  (*
   * The bit 0x0020 of the n_desc field is used for two non-overlapping purposes
   * and has two different symbolic names, N_NO_DEAD_STRIP and N_DESC_DISCARDED.
   *)

  (*
   * The N_NO_DEAD_STRIP bit of the n_desc field only ever appears in a
   * relocatable .o file (MH_OBJECT filetype). And is used to indicate to the
   * static link editor it is never to dead strip the symbol.
   *)
  N_NO_DEAD_STRIP   = $0020; (* symbol is not to be dead stripped *)

  (*
   * The N_DESC_DISCARDED bit of the n_desc field never appears in linked image.
   * But is used in very rare cases by the dynamic link editor to mark an in
   * memory symbol as discared and longer used for linking.
   *)
  N_DESC_DISCARDED  = $0020; (* symbol is discarded *)

  (*
   * The N_WEAK_REF bit of the n_desc field indicates to the dynamic linker that
   * the undefined symbol is allowed to be missing and is to have the address of
   * zero when missing.
   *)
  N_WEAK_REF        = $0040; (* symbol is weak referenced *)

  (*
   * The N_WEAK_DEF bit of the n_desc field indicates to the static and dynamic
   * linkers that the symbol definition is weak, allowing a non-weak symbol to
   * also be used which causes the weak definition to be discared.  Currently this
   * is only supported for symbols in coalesed sections.
   *)
  N_WEAK_DEF        = $0080; (* coalesed symbol is a weak definition *)

  (*
   * The N_REF_TO_WEAK bit of the n_desc field indicates to the dynamic linker
   * that the undefined symbol should be resolved using flat namespace searching.
   *)
  N_REF_TO_WEAK     = $0080; (* reference to a weak symbol *)

  (*
   * The N_ARM_THUMB_DEF bit of the n_desc field indicates that the symbol is
   * a defintion of a Thumb function.
   *)
  N_ARM_THUMB_DEF   = $0008; (* symbol is a Thumb function (ARM) *)

  (*
   * The N_SYMBOL_RESOLVER bit of the n_desc field indicates that the
   * that the function is actually a resolver function and should
   * be called to get the address of the real function to use.
   * This bit is only available in .o files (MH_OBJECT filetype)
   *)
  N_SYMBOL_RESOLVER = $0100;

  (*
   * The N_ALT_ENTRY bit of the n_desc field indicates that the
   * symbol is pinned to the previous content.
   *)
  N_ALT_ENTRY       = $0200;
{$ENDREGION 'nlist.h'}

{$REGION 'reloc.h'}
type
  (*
   * Format of a relocation entry of a Mach-O file.  Modified from the 4.3BSD
   * format.  The modifications from the original format were changing the value
   * of the r_symbolnum field for "local" (r_extern == 0) relocation entries.
   * This modification is required to support symbols in an arbitrary number of
   * sections not just the three sections (text, data and bss) in a 4.3BSD file.
   * Also the last 4 bits have had the r_type tag added to them.
   *)
  relocation_info = record
    r_address: Int32; (* offset in the section to what is being relocated *)
    bits: UInt32;     (* r_symbolnum:24, symbol index if r_extern == 1 or section
                           ordinal if r_extern == 0
                         r_pcrel:1, was relocated pc relative already
                         r_length:2, 0=byte, 1=word, 2=long, 3=quad
                         r_extern:1, does not include value of sym referenced
                         r_type:4; if not 0, machine specific relocation type *)
  end;
  Prelocation_info = ^relocation_info;

const
  R_ABS = 0; (* absolute relocation type for Mach-O files *)

(*
 * The r_address is not really the address as it's name indicates but an offset.
 * In 4.3BSD a.out objects this offset is from the start of the "segment" for
 * which relocation entry is for (text or data).  For Mach-O object files it is
 * also an offset but from the start of the "section" for which the relocation
 * entry is for.  See comments in <mach-o/loader.h> about the r_address feild
 * in images for used with the dynamic linker.
 *
 * In 4.3BSD a.out objects if r_extern is zero then r_symbolnum is an ordinal
 * for the segment the symbol being relocated is in.  These ordinals are the
 * symbol types N_TEXT, N_DATA, N_BSS or N_ABS.  In Mach-O object files these
 * ordinals refer to the sections in the object file in the order their section
 * structures appear in the headers of the object file they are in.  The first
 * section has the ordinal 1, the second 2, and so on.  This means that the
 * same ordinal in two different object files could refer to two different
 * sections.  And further could have still different ordinals when combined
 * by the link-editor.  The value R_ABS is used for relocation entries for
 * absolute symbols which need no further relocation.
 *)

(*
 * For RISC machines some of the references are split across two instructions
 * and the instruction does not contain the complete value of the reference.
 * In these cases a second, or paired relocation entry, follows each of these
 * relocation entries, using a PAIR r_type, which contains the other part of the
 * reference not contained in the instruction.  This other part is stored in the
 * pair's r_address field.  The exact number of bits of the other part of the
 * reference store in the r_address field is dependent on the particular
 * relocation type for the particular architecture.
 *)

(*
 * To make scattered loading by the link editor work correctly "local"
 * relocation entries can't be used when the item to be relocated is the value
 * of a symbol plus an offset (where the resulting expresion is outside the
 * block the link editor is moving, a blocks are divided at symbol addresses).
 * In this case. where the item is a symbol value plus offset, the link editor
 * needs to know more than just the section the symbol was defined.  What is
 * needed is the actual value of the symbol without the offset so it can do the
 * relocation correctly based on where the value of the symbol got relocated to
 * not the value of the expression (with the offset added to the symbol value).
 * So for the NeXT 2.0 release no "local" relocation entries are ever used when
 * there is a non-zero offset added to a symbol.  The "external" and "local"
 * relocation entries remain unchanged.
 *
 * The implemention is quite messy given the compatibility with the existing
 * relocation entry format.  The ASSUMPTION is that a section will never be
 * bigger than 2**24 - 1 (0x00ffffff or 16,777,215) bytes.  This assumption
 * allows the r_address (which is really an offset) to fit in 24 bits and high
 * bit of the r_address field in the relocation_info structure to indicate
 * it is really a scattered_relocation_info structure.  Since these are only
 * used in places where "local" relocation entries are used and not where
 * "external" relocation entries are used the r_extern field has been removed.
 *
 * For scattered loading to work on a RISC machine where some of the references
 * are split across two instructions the link editor needs to be assured that
 * each reference has a unique 32 bit reference (that more than one reference is
 * NOT sharing the same high 16 bits for example) so it move each referenced
 * item independent of each other.  Some compilers guarantees this but the
 * compilers don't so scattered loading can be done on those that do guarantee
 * this.
 *)
const
  R_SCATTERED = $80000000; (* mask to be applied to the r_address field
                              of a relocation_info structure to tell that
                              is is really a scattered_relocation_info
                              stucture *)

type
  scattered_relocation_info = record
    bits: Int32;    (* r_address:24, offset in the section to what is being relocated
                       r_type:4, if not 0, machine specific relocation type
                       r_length:2, 0=byte, 1=word, 2=long, 3=quad
                       r_pcrel:1,  was relocated pc relative already
                       r_scattered:1; 1=scattered, 0=non-scattered (see above) *)
    r_value: Int32; (* the value the item to be relocated is
                       refering to (without any offset added) *)
  end;
  Pscattered_relocation_info = ^scattered_relocation_info;

const
  (*
   * Relocation types used in a generic implementation.  Relocation entries for
   * normal things use the generic relocation as discribed above and their r_type
   * is GENERIC_RELOC_VANILLA (a value of zero).
   *
   * Another type of generic relocation, GENERIC_RELOC_SECTDIFF, is to support
   * the difference of two symbols defined in different sections.  That is the
   * expression "symbol1 - symbol2 + constant" is a relocatable expression when
   * both symbols are defined in some section.  For this type of relocation the
   * both relocations entries are scattered relocation entries.  The value of
   * symbol1 is stored in the first relocation entry's r_value field and the
   * value of symbol2 is stored in the pair's r_value field.
   *
   * A special case for a prebound lazy pointer is needed to beable to set the
   * value of the lazy pointer back to its non-prebound state.  This is done
   * using the GENERIC_RELOC_PB_LA_PTR r_type.  This is a scattered relocation
   * entry where the r_value feild is the value of the lazy pointer not prebound.
   *)
  GENERIC_RELOC_VANILLA        = 0; (* generic relocation as discribed above *)
  GENERIC_RELOC_PAIR           = 1; (* Only follows a GENERIC_RELOC_SECTDIFF *)
  GENERIC_RELOC_SECTDIFF       = 2;
  GENERIC_RELOC_PB_LA_PTR      = 3; (* prebound lazy pointer *)
  GENERIC_RELOC_LOCAL_SECTDIFF = 4;
  GENERIC_RELOC_TLV            = 5; (* thread local variables *)
{$ENDREGION 'reloc.h'}

{$REGION 'fat.h'}
(*
 * This header file describes the structures of the file format for "fat"
 * architecture specific file (wrapper design).  At the begining of the file
 * there is one fat_header structure followed by a number of fat_arch
 * structures.  For each architecture in the file, specified by a pair of
 * cputype and cpusubtype, the fat_header describes the file offset, file
 * size and alignment in the file of the architecture specific member.
 * The padded bytes in the file to place each member on it's specific alignment
 * are defined to be read as zeros and can be left as "holes" if the file system
 * can support them as long as they read as zeros.
 *
 * All structures defined here are always written and read to/from disk
 * in big-endian order.
 *)

const
  FAT_MAGIC = $cafebabe;
  FAT_CIGAM = $bebafeca; (* NXSwapLong(FAT_MAGIC) *)

type
  fat_header = record
    magic: UInt32;    (* FAT_MAGIC or FAT_MAGIC_64 *)
    nfat_arch: Int32; (* number of structs that follow *)
  end;
  Pfat_header = ^fat_header;

type
  fat_arch = record
    cputype: cpu_type_t;       (* cpu specifier (int) *)
    cpusubtype: cpu_subtype_t; (* machine specifier (int) *)
    offset: UInt32;            (* file offset to this object file *)
    size: UInt32;              (* size of this object file *)
    align: UInt32;             (* alignment as a power of 2 *)
  end;
  Pfat_arch = ^fat_arch;

const
  (*
   * The support for the 64-bit fat file format described here is a work in
   * progress and not yet fully supported in all the Apple Developer Tools.
   *
   * When a slice is greater than 4mb or an offset to a slice is greater than 4mb
   * then the 64-bit fat file format is used.
   *)
  FAT_MAGIC_64 = $cafebabf;
  FAT_CIGAM_64 = $bfbafeca; (* NXSwapLong(FAT_MAGIC_64) *)

type
  fat_arch_64 = record
    cputype: cpu_type_t;       (* cpu specifier (int) *)
    cpusubtype: cpu_subtype_t; (* machine specifier (int) *)
    offset: UInt64;            (* file offset to this object file *)
    size: UInt64;              (* size of this object file *)
    align: UInt32;             (* alignment as a power of 2 *)
    reserved: UInt32;          (* reserved *)
  end;
  Pfat_arch_64 = ^fat_arch_64;
{$ENDREGION 'fat.h'}

implementation

{$REGION 'nlist.h'}
function GET_COMM_ALIGN(n_desc: UInt32): UInt32; inline;
begin
  Result := (n_desc shr 8) and $0f;
end;

procedure SET_COMM_ALIGN(var n_desc: UInt32; align: UInt32); inline;
begin
  n_desc := (n_desc and $f0ff) or ((align and $0f) shl 8);
end;

function GET_LIBRARY_ORDINAL(n_desc: UInt32): UInt32; inline;
begin
  Result := (n_desc shr 8) and $ff;
end;

procedure SET_LIBRARY_ORDINAL(var n_desc: UInt32; ordinal: UInt32); inline;
begin
  n_desc := (n_desc and $00ff) or ((ordinal and $ff) shl 8);
end;
{$ENDREGION 'nlist.h'}

end.
