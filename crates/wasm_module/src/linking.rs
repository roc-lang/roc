use bumpalo::collections::vec::Vec;
use bumpalo::Bump;

use super::parse::{parse_fixed_size_items, Parse, ParseError, SkipBytes};
use super::sections::SectionId;
use super::serialize::{overwrite_padded_i32, overwrite_padded_u32};

/*******************************************************************
 *
 * Relocation sections
 *
 * https://github.com/WebAssembly/tool-conventions/blob/main/Linking.md#relocation-sections
 *
 *******************************************************************/

#[repr(u8)]
#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum IndexRelocType {
    /// a function index encoded as a 5-byte [varuint32]. Used for the immediate argument of a `call` instruction.
    FunctionIndexLeb = 0,
    /// a function table index encoded as a 5-byte [varint32].
    /// Used to refer to the immediate argument of a `i32.const` instruction, e.g. taking the address of a function.
    TableIndexSleb = 1,
    /// a function table index encoded as a [uint32], e.g. taking the address of a function in a static data initializer.
    TableIndexI32 = 2,
    /// a type index encoded as a 5-byte [varuint32], e.g. the type immediate in a `call_indirect`.
    TypeIndexLeb = 6,
    /// a global index encoded as a   5-byte [varuint32], e.g. the index immediate in a `get_global`.
    GlobalIndexLeb = 7,
    /// an event index encoded as a 5-byte [varuint32]. Used for the immediate argument of a `throw` and `if_except`   instruction.
    EventIndexLeb = 10,
    /// a global index encoded as [uint32].
    GlobalIndexI32 = 13,
    /// the 64-bit counterpart of  `R_WASM_TABLE_INDEX_SLEB`. A function table index encoded as a 10-byte [varint64].
    /// Used to refer to the immediate argument of a `i64.const` instruction, e.g. taking the address of a function in Wasm64.
    TableIndexSleb64 = 18,
    /// the 64-bit counterpart of `R_WASM_TABLE_INDEX_I32`.
    /// A function table index encoded as a [uint64], e.g. taking the address of a function in a static data initializer.
    TableIndexI64 = 19,
    /// a table number encoded as a 5-byte [varuint32]. Used for the table immediate argument in the table.*   instructions.
    TableNumberLeb = 20,
}

impl IndexRelocType {
    fn from_u8(x: u8) -> Option<IndexRelocType> {
        match x {
            0 => Some(Self::FunctionIndexLeb),
            1 => Some(Self::TableIndexSleb),
            2 => Some(Self::TableIndexI32),
            6 => Some(Self::TypeIndexLeb),
            7 => Some(Self::GlobalIndexLeb),
            10 => Some(Self::EventIndexLeb),
            13 => Some(Self::GlobalIndexI32),
            18 => Some(Self::TableIndexSleb64),
            19 => Some(Self::TableIndexI64),
            20 => Some(Self::TableNumberLeb),
            _ => None,
        }
    }
}

#[repr(u8)]
#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum OffsetRelocType {
    /// a linear memory index encoded as a 5-byte [varuint32].
    /// Used for the immediate argument of a `load` or `store` instruction, e.g. directly loading from or storing to a C++ global.
    MemoryAddrLeb = 3,
    /// a linear memory index encoded as a 5-byte [varint32].
    /// Used for the immediate argument of a `i32.const` instruction, e.g. taking the address of a C++ global.
    MemoryAddrSleb = 4,
    /// a linear memory index encoded as a [uint32], e.g. taking the address of a C++ global in a static data initializer.
    MemoryAddrI32 = 5,
    /// a byte offset within code section for the specific function encoded as a [uint32].
    /// The offsets start at the actual function code excluding its size field.
    FunctionOffsetI32 = 8,
    /// a byte offset from start of the specified section encoded as a [uint32].
    SectionOffsetI32 = 9,
    /// the 64-bit counterpart of `R_WASM_MEMORY_ADDR_LEB`. A 64-bit linear memory index encoded as a 10-byte [varuint64],
    /// Used for the immediate argument of a `load` or `store` instruction on a 64-bit linear memory array.
    MemoryAddrLeb64 = 14,
    /// the 64-bit counterpart of `R_WASM_MEMORY_ADDR_SLEB`. A 64-bit linear memory index encoded as a 10-byte [varint64].
    /// Used for the immediate argument of a `i64.const` instruction.
    MemoryAddrSleb64 = 15,
    /// the 64-bit counterpart of `R_WASM_MEMORY_ADDR`. A 64-bit linear memory index encoded as a [uint64],
    /// e.g. taking the 64-bit address of a C++ global in a static data initializer.
    MemoryAddrI64 = 16,
}

impl OffsetRelocType {
    fn from_u8(x: u8) -> Option<OffsetRelocType> {
        match x {
            3 => Some(Self::MemoryAddrLeb),
            4 => Some(Self::MemoryAddrSleb),
            5 => Some(Self::MemoryAddrI32),
            8 => Some(Self::FunctionOffsetI32),
            9 => Some(Self::SectionOffsetI32),
            14 => Some(Self::MemoryAddrLeb64),
            15 => Some(Self::MemoryAddrSleb64),
            16 => Some(Self::MemoryAddrI64),
            _ => None,
        }
    }
}

#[derive(Debug, Clone)]
pub enum RelocationEntry {
    Index {
        type_id: IndexRelocType,
        offset: u32,       // offset 0 means the next byte after section id and size
        symbol_index: u32, // index in symbol table
    },
    Offset {
        type_id: OffsetRelocType,
        offset: u32,       // offset 0 means the next byte after section id and size
        symbol_index: u32, // index in symbol table
        addend: i32,       // addend to add to the address
    },
}

impl Parse<()> for RelocationEntry {
    fn parse(_: (), bytes: &[u8], cursor: &mut usize) -> Result<Self, ParseError> {
        let type_id_byte = bytes[*cursor];
        *cursor += 1;
        let offset = u32::parse((), bytes, cursor)?;
        let symbol_index = u32::parse((), bytes, cursor)?;

        if let Some(type_id) = IndexRelocType::from_u8(type_id_byte) {
            return Ok(RelocationEntry::Index {
                type_id,
                offset,
                symbol_index,
            });
        }

        if let Some(type_id) = OffsetRelocType::from_u8(type_id_byte) {
            let addend = i32::parse((), bytes, cursor)?;
            return Ok(RelocationEntry::Offset {
                type_id,
                offset,
                symbol_index,
                addend,
            });
        }

        Err(ParseError {
            offset: *cursor,
            message: format!("Unknown relocation type 0x{:2x}", type_id_byte),
        })
    }
}

#[derive(Debug)]
pub struct RelocationSection<'a> {
    pub name: &'a str,
    /// The *index* (not ID!) of the target section in the module
    pub target_section_index: u32,
    pub entries: Vec<'a, RelocationEntry>,
}

impl<'a> RelocationSection<'a> {
    pub(crate) fn new(arena: &'a Bump, name: &'a str) -> Self {
        RelocationSection {
            name,
            target_section_index: 0,
            entries: bumpalo::vec![in arena],
        }
    }

    pub fn apply_relocs_u32(&self, section_bytes: &mut [u8], sym_index: u32, value: u32) {
        for entry in self.entries.iter() {
            match entry {
                RelocationEntry::Index {
                    type_id,
                    offset,
                    symbol_index,
                } if *symbol_index == sym_index => {
                    use IndexRelocType::*;
                    let idx = *offset as usize;
                    match type_id {
                        FunctionIndexLeb | TypeIndexLeb | GlobalIndexLeb | EventIndexLeb
                        | TableNumberLeb => {
                            overwrite_padded_u32(&mut section_bytes[idx..], value);
                        }
                        _ => todo!("Linking relocation type {:?}", type_id),
                    }
                }
                RelocationEntry::Offset {
                    type_id,
                    offset,
                    symbol_index,
                    addend,
                } if *symbol_index == sym_index => {
                    use OffsetRelocType::*;
                    let idx = *offset as usize;
                    match type_id {
                        MemoryAddrLeb => {
                            overwrite_padded_u32(&mut section_bytes[idx..], value + *addend as u32);
                        }
                        MemoryAddrSleb => {
                            overwrite_padded_i32(&mut section_bytes[idx..], value as i32 + *addend);
                        }
                        _ => todo!("Linking relocation type {:?}", type_id),
                    }
                }
                _ => {}
            }
        }
    }
}

type RelocCtx<'a> = (&'a Bump, &'static str);

impl<'a> Parse<RelocCtx<'a>> for RelocationSection<'a> {
    fn parse(ctx: RelocCtx<'a>, bytes: &[u8], cursor: &mut usize) -> Result<Self, ParseError> {
        let (arena, name) = ctx;

        if *cursor >= bytes.len() || bytes[*cursor] != SectionId::Custom as u8 {
            // The section we're looking for is missing, which is the same as being empty.
            return Ok(RelocationSection::new(arena, name));
        }
        *cursor += 1;
        u32::skip_bytes(bytes, cursor)?; // section body size

        let actual_name = <&'a str>::parse(arena, bytes, cursor)?;
        if actual_name != name {
            // The section we're looking for is missing, which is the same as being empty.
            return Ok(RelocationSection::new(arena, name));
        }

        let target_section_index = u32::parse((), bytes, cursor)?;
        let entries = parse_fixed_size_items(arena, bytes, cursor)?;
        Ok(RelocationSection {
            name,
            target_section_index,
            entries,
        })
    }
}

/*******************************************************************
 *
 * Linking section
 *
 * https://github.com/WebAssembly/tool-conventions/blob/main/Linking.md#linking-metadata-section
 *
 *******************************************************************/

/// Linking metadata for data segments
#[derive(Debug)]
pub struct LinkingSegment<'a> {
    pub name: &'a str,
    pub align_bytes_pow2: u32,
    pub flags: u32,
}

impl<'a> Parse<&'a Bump> for LinkingSegment<'a> {
    fn parse(arena: &'a Bump, bytes: &[u8], cursor: &mut usize) -> Result<Self, ParseError> {
        let name = <&'a str>::parse(arena, bytes, cursor)?;
        let align_bytes_pow2 = u32::parse((), bytes, cursor)?;
        let flags = u32::parse((), bytes, cursor)?;
        Ok(LinkingSegment {
            name,
            align_bytes_pow2,
            flags,
        })
    }
}

/// Linking metadata for init (start) functions
#[derive(Debug)]
pub struct LinkingInitFunc {
    pub priority: u32,
    pub symbol_index: u32, // index in the symbol table, not the function index
}

//------------------------------------------------
// Common data
//------------------------------------------------

#[repr(u8)]
#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum ComdatSymKind {
    Data = 0,
    Function = 1,
    Global = 2,
    Event = 3,
    Table = 4,
    Section = 5,
}

#[derive(Debug)]
pub struct ComdatSym {
    pub kind: ComdatSymKind,
    pub index: u32,
}

/// Linking metadata for common data
/// A COMDAT group may contain one or more functions, data segments, and/or custom sections.
/// The linker will include all of these elements with a given group name from one object file,
/// and will exclude any element with this group name from all other object files.
#[allow(dead_code)]
#[derive(Debug)]
pub struct LinkingComdat<'a> {
    name: &'a str,
    flags: u32,
    syms: Vec<'a, ComdatSym>,
}

//------------------------------------------------
// Symbol table
//------------------------------------------------

/// Indicating that this is a weak symbol.  When
/// linking multiple modules defining the same symbol, all weak definitions are
/// discarded if any strong definitions exist; then if multiple weak definitions
/// exist all but one (unspecified) are discarded; and finally it is an error if
/// more than one definition remains.
pub const WASM_SYM_BINDING_WEAK: u32 = 1;

/// Indicating that this is a local symbol (this is exclusive with `WASM_SYM_BINDING_WEAK`).
/// Local symbols are not to be exported, or linked to other modules/sections.
/// The names of all non-local symbols must be unique, but the names of local symbols
/// are not considered for uniqueness. A local function or global symbol cannot reference an import.
pub const WASM_SYM_BINDING_LOCAL: u32 = 2;

/// Indicating that this is a hidden symbol.
/// Hidden symbols are not to be exported when performing the final link, but
/// may be linked to other modules.
pub const WASM_SYM_VISIBILITY_HIDDEN: u32 = 4;

/// Indicating that this symbol is not defined.
/// For non-data symbols, this must match whether the symbol is an import
/// or is defined; for data symbols, determines whether a segment is specified.
pub const WASM_SYM_UNDEFINED: u32 = 0x10; // required if the symbol refers to an import

/// The symbol is intended to be exported from the
/// wasm module to the host environment. This differs from the visibility flags
/// in that it effects the static linker.
pub const WASM_SYM_EXPORTED: u32 = 0x20;

/// The symbol uses an explicit symbol name,
/// rather than reusing the name from a wasm import. This allows it to remap
/// imports from foreign WebAssembly modules into local symbols with different
/// names.
pub const WASM_SYM_EXPLICIT_NAME: u32 = 0x40; // use the name from the symbol table, not from the import

/// The symbol is intended to be included in the
/// linker output, regardless of whether it is used by the program.
pub const WASM_SYM_NO_STRIP: u32 = 0x80;

#[derive(Clone, Debug)]
pub enum WasmObjectSymbol<'a> {
    ExplicitlyNamed {
        flags: u32,
        index: u32,
        name: &'a str,
    },
    ImplicitlyNamed {
        flags: u32,
        index: u32,
    },
}

impl<'a> Parse<&'a Bump> for WasmObjectSymbol<'a> {
    fn parse(arena: &'a Bump, bytes: &[u8], cursor: &mut usize) -> Result<Self, ParseError> {
        let flags = u32::parse((), bytes, cursor)?;
        let index = u32::parse((), bytes, cursor)?;

        // If a symbol refers to an import, then we already have the name in the import section.
        // The linking section doesn't repeat it, unless the "explicit name" flag is set (used for renaming).
        // ("Undefined symbol" is linker jargon, and "import" is Wasm jargon. For functions, they're equivalent.)
        let is_import = (flags & WASM_SYM_UNDEFINED) != 0;
        let external_syms_have_explicit_names = (flags & WASM_SYM_EXPLICIT_NAME) != 0;
        let has_explicit_name = !is_import || external_syms_have_explicit_names;

        if has_explicit_name {
            let name = <&'a str>::parse(arena, bytes, cursor)?;
            Ok(Self::ExplicitlyNamed { flags, index, name })
        } else {
            Ok(Self::ImplicitlyNamed { flags, index })
        }
    }
}

#[derive(Clone, Debug)]
pub enum DataSymbol<'a> {
    Defined {
        flags: u32,
        name: &'a str,
        segment_index: u32,
        segment_offset: u32,
        size: u32,
    },
    Imported {
        flags: u32,
        name: &'a str,
    },
}

impl<'a> Parse<&'a Bump> for DataSymbol<'a> {
    fn parse(arena: &'a Bump, bytes: &[u8], cursor: &mut usize) -> Result<Self, ParseError> {
        let flags = u32::parse((), bytes, cursor)?;
        let name = <&'a str>::parse(arena, bytes, cursor)?;

        if (flags & WASM_SYM_UNDEFINED) != 0 {
            Ok(Self::Imported { flags, name })
        } else {
            let segment_index = u32::parse((), bytes, cursor)?;
            let segment_offset = u32::parse((), bytes, cursor)?;
            let size = u32::parse((), bytes, cursor)?;

            Ok(Self::Defined {
                flags,
                name,
                segment_index,
                segment_offset,
                size,
            })
        }
    }
}

/// We don't use this, but we need it in the symbol table so the indices are correct!
/// If we ever use it, note that it refers to section index, not section id.
#[derive(Clone, Debug)]
pub struct SectionSymbol {
    _flags: u32,
    _index: u32,
}

impl Parse<()> for SectionSymbol {
    fn parse(_: (), bytes: &[u8], cursor: &mut usize) -> Result<Self, ParseError> {
        let flags = u32::parse((), bytes, cursor)?;
        let index = u32::parse((), bytes, cursor)?;
        Ok(SectionSymbol {
            _flags: flags,
            _index: index,
        })
    }
}

#[derive(Clone, Debug)]
pub enum SymInfo<'a> {
    Function(WasmObjectSymbol<'a>),
    Data(DataSymbol<'a>),
    Global(WasmObjectSymbol<'a>),
    Section(SectionSymbol),
    Event(WasmObjectSymbol<'a>),
    Table(WasmObjectSymbol<'a>),
}

impl<'a> SymInfo<'a> {
    pub fn name(&self) -> Option<&'a str> {
        match self {
            Self::Function(WasmObjectSymbol::ExplicitlyNamed { name, .. }) => Some(name),
            Self::Data(DataSymbol::Defined { name, .. }) => Some(name),
            Self::Data(DataSymbol::Imported { name, .. }) => Some(name),
            Self::Global(WasmObjectSymbol::ExplicitlyNamed { name, .. }) => Some(name),
            Self::Event(WasmObjectSymbol::ExplicitlyNamed { name, .. }) => Some(name),
            Self::Table(WasmObjectSymbol::ExplicitlyNamed { name, .. }) => Some(name),
            _ => None, // ImplicitlyNamed or SectionSymbols
        }
    }
}

#[repr(u8)]
#[derive(Debug)]
enum SymType {
    Function = 0,
    Data = 1,
    Global = 2,
    Section = 3,
    Event = 4,
    Table = 5,
}

impl Parse<()> for SymType {
    fn parse(_: (), bytes: &[u8], cursor: &mut usize) -> Result<Self, ParseError> {
        let offset = *cursor;
        let type_id = bytes[offset];
        *cursor += 1;
        match type_id {
            0 => Ok(Self::Function),
            1 => Ok(Self::Data),
            2 => Ok(Self::Global),
            3 => Ok(Self::Section),
            4 => Ok(Self::Event),
            5 => Ok(Self::Table),
            x => Err(ParseError {
                offset,
                message: format!("Invalid symbol info type in linking section: {}", x),
            }),
        }
    }
}

impl<'a> Parse<&'a Bump> for SymInfo<'a> {
    fn parse(arena: &'a Bump, bytes: &[u8], cursor: &mut usize) -> Result<Self, ParseError> {
        let type_id = SymType::parse((), bytes, cursor)?;
        match type_id {
            SymType::Function => WasmObjectSymbol::parse(arena, bytes, cursor).map(Self::Function),
            SymType::Data => DataSymbol::parse(arena, bytes, cursor).map(Self::Data),
            SymType::Global => WasmObjectSymbol::parse(arena, bytes, cursor).map(Self::Global),
            SymType::Section => SectionSymbol::parse((), bytes, cursor).map(Self::Section),
            SymType::Event => WasmObjectSymbol::parse(arena, bytes, cursor).map(Self::Event),
            SymType::Table => WasmObjectSymbol::parse(arena, bytes, cursor).map(Self::Table),
        }
    }
}

//----------------------------------------------------------------
//  Linking subsections
//----------------------------------------------------------------

#[repr(u8)]
#[derive(Debug)]
enum SubSectionId {
    SegmentInfo = 5,
    InitFuncs = 6,
    ComdatInfo = 7,
    SymbolTable = 8,
}

impl Parse<()> for SubSectionId {
    fn parse(_: (), bytes: &[u8], cursor: &mut usize) -> Result<Self, ParseError> {
        let id = bytes[*cursor];
        let offset = *cursor;
        *cursor += 1;
        match id {
            5 => Ok(Self::SegmentInfo),
            6 => Ok(Self::InitFuncs),
            7 => Ok(Self::ComdatInfo),
            8 => Ok(Self::SymbolTable),
            x => Err(ParseError {
                offset,
                message: format!("Invalid linking subsection ID {}", x),
            }),
        }
    }
}

//----------------------------------------------------------------
//  Linking metadata section
//----------------------------------------------------------------

const LINKING_VERSION: u8 = 2;

/// The spec describes this in very weird way, so we're doing something saner.
/// They call it an "array" of subsections with different variants, BUT this "array"
/// has an implicit length, and none of the items can be repeated, so a struct is better.
/// No point writing code to "find" the symbol table, when we know there's exactly one.
/// The only one we really use is the symbol table
#[derive(Debug)]
pub struct LinkingSection<'a> {
    pub symbol_table: Vec<'a, SymInfo<'a>>,
    pub segment_info: Vec<'a, LinkingSegment<'a>>,
    pub init_funcs: Vec<'a, LinkingInitFunc>,
    pub comdat_info: Vec<'a, LinkingComdat<'a>>,
}

impl<'a> LinkingSection<'a> {
    const NAME: &'static str = "linking";

    pub fn new(arena: &'a Bump) -> Self {
        LinkingSection {
            symbol_table: Vec::with_capacity_in(16, arena),
            segment_info: Vec::with_capacity_in(16, arena),
            init_funcs: Vec::with_capacity_in(0, arena),
            comdat_info: Vec::with_capacity_in(0, arena),
        }
    }

    pub fn find_internal_symbol(&self, target_name: &str) -> Result<usize, String> {
        self.symbol_table
            .iter()
            .position(|sym| sym.name() == Some(target_name))
            .ok_or_else(|| {
                format!(
                    "Linking failed! Can't find `{}` in host symbol table",
                    target_name
                )
            })
    }

    pub fn find_imported_fn_sym_index(&mut self, fn_index: u32) -> Result<u32, String> {
        self.symbol_table
            .iter_mut()
            .position(|sym| match sym {
                SymInfo::Function(WasmObjectSymbol::ImplicitlyNamed { flags, index, .. })
                | SymInfo::Function(WasmObjectSymbol::ExplicitlyNamed { flags, index, .. }) => {
                    *flags & WASM_SYM_UNDEFINED != 0 && *index == fn_index
                }
                _ => false,
            })
            .map(|sym_index| sym_index as u32)
            .ok_or_else(|| format!("Can't find fn #{} in host symbol table", fn_index))
    }

    pub fn find_and_reindex_imported_fn(
        &mut self,
        old_fn_index: u32,
        new_fn_index: u32,
    ) -> Result<u32, String> {
        self.symbol_table
            .iter_mut()
            .position(|sym| match sym {
                SymInfo::Function(WasmObjectSymbol::ImplicitlyNamed { flags, index, .. })
                | SymInfo::Function(WasmObjectSymbol::ExplicitlyNamed { flags, index, .. }) => {
                    let found = *flags & WASM_SYM_UNDEFINED != 0 && *index == old_fn_index;
                    if found {
                        *index = new_fn_index;
                    }
                    found
                }
                _ => false,
            })
            .map(|sym_index| sym_index as u32)
            .ok_or_else(|| {
                format!(
                    "Linking failed! Can't find fn #{} in host symbol table",
                    old_fn_index
                )
            })
    }
}

impl<'a> Parse<&'a Bump> for LinkingSection<'a> {
    fn parse(arena: &'a Bump, bytes: &[u8], cursor: &mut usize) -> Result<Self, ParseError> {
        if *cursor >= bytes.len() || bytes[*cursor] != SectionId::Custom as u8 {
            return Ok(LinkingSection::new(arena));
        }
        *cursor += 1;
        let body_size = u32::parse((), bytes, cursor)?;
        let section_end = *cursor + body_size as usize;

        // Don't fail if it's the wrong section. Let the WasmModule validate presence/absence of sections
        let actual_name = <&'a str>::parse(arena, bytes, cursor)?;
        if actual_name != Self::NAME {
            return Ok(LinkingSection::new(arena));
        }

        let linking_version = bytes[*cursor];
        if linking_version != LINKING_VERSION {
            return Err(ParseError {
                offset: *cursor,
                message: format!(
                    "This file uses version {} of Wasm linking data, but only version {} is supported.",
                    linking_version, LINKING_VERSION
                ),
            });
        }
        *cursor += 1;

        // Linking section is encoded as an array of subsections, but we prefer a struct internally.
        // The order is not defined in the spec, so we loop over them and organise them into our struct.
        // In theory, there could even be more than one of each. That would be weird, but easy to handle.
        let mut section = LinkingSection::new(arena);
        while *cursor < section_end {
            let subsection_id = SubSectionId::parse((), bytes, cursor)?;
            let len = u32::parse((), bytes, cursor)?; // bytes in the subsection
            match subsection_id {
                SubSectionId::SymbolTable => {
                    let count = u32::parse((), bytes, cursor)?;
                    for _ in 0..count {
                        let item = SymInfo::parse(arena, bytes, cursor)?;
                        section.symbol_table.push(item);
                    }
                }
                SubSectionId::SegmentInfo => {
                    let count = u32::parse((), bytes, cursor)?;
                    for _ in 0..count {
                        let item = LinkingSegment::parse(arena, bytes, cursor)?;
                        section.segment_info.push(item);
                    }
                }
                SubSectionId::InitFuncs | SubSectionId::ComdatInfo => {
                    // We don't use these subsections, just skip over them.
                    *cursor += len as usize;
                }
            }
        }

        Ok(section)
    }
}
