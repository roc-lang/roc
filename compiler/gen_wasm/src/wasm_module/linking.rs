use bumpalo::collections::vec::Vec;
use bumpalo::Bump;

use super::sections::{update_section_size, write_custom_section_header};
use super::serialize::{SerialBuffer, Serialize};
use super::Align;

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

impl RelocationEntry {
    pub fn offset(&self) -> u32 {
        match self {
            Self::Index { offset, .. } => *offset,
            Self::Offset { offset, .. } => *offset,
        }
    }

    pub fn offset_mut(&mut self) -> &mut u32 {
        match self {
            Self::Index { offset, .. } => offset,
            Self::Offset { offset, .. } => offset,
        }
    }
}

impl RelocationEntry {
    pub fn for_function_call(offset: u32, symbol_index: u32) -> Self {
        RelocationEntry::Index {
            type_id: IndexRelocType::FunctionIndexLeb,
            offset,
            symbol_index,
        }
    }
}

impl Serialize for RelocationEntry {
    fn serialize<T: SerialBuffer>(&self, buffer: &mut T) {
        match self {
            Self::Index {
                type_id,
                offset,
                symbol_index,
            } => {
                buffer.append_u8(*type_id as u8);
                buffer.encode_u32(*offset);
                buffer.encode_u32(*symbol_index);
            }
            Self::Offset {
                type_id,
                offset,
                symbol_index,
                addend,
            } => {
                buffer.append_u8(*type_id as u8);
                buffer.encode_u32(*offset);
                buffer.encode_u32(*symbol_index);
                buffer.encode_i32(*addend);
            }
        }
    }
}

#[derive(Debug)]
pub struct RelocationSection<'a> {
    pub name: &'a str,
    /// The *index* (not ID!) of the target section in the module
    pub target_section_index: Option<u32>,
    pub entries: Vec<'a, RelocationEntry>,
}

impl<'a> RelocationSection<'a> {
    pub fn new(arena: &'a Bump, name: &'a str) -> Self {
        RelocationSection {
            name,
            target_section_index: None,
            entries: Vec::with_capacity_in(64, arena),
        }
    }
}

impl<'a> Serialize for RelocationSection<'a> {
    fn serialize<T: SerialBuffer>(&self, buffer: &mut T) {
        if !self.entries.is_empty() {
            let header_indices = write_custom_section_header(buffer, self.name);
            buffer.encode_u32(self.target_section_index.unwrap());
            self.entries.serialize(buffer);
            update_section_size(buffer, header_indices);
        }
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
pub struct LinkingSegment {
    pub name: String,
    pub alignment: Align,
    pub flags: u32,
}

impl Serialize for LinkingSegment {
    fn serialize<T: SerialBuffer>(&self, buffer: &mut T) {
        buffer.encode_u32(self.name.len() as u32);
        buffer.append_slice(self.name.as_bytes());
        let align_bytes_pow2 = self.alignment as u32;
        buffer.encode_u32(align_bytes_pow2);
        buffer.encode_u32(self.flags);
    }
}

/// Linking metadata for init (start) functions
#[derive(Debug)]
pub struct LinkingInitFunc {
    pub priority: u32,
    pub symbol_index: u32, // index in the symbol table, not the function index
}

impl Serialize for LinkingInitFunc {
    fn serialize<T: SerialBuffer>(&self, _buffer: &mut T) {
        todo!();
    }
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

impl Serialize for ComdatSym {
    fn serialize<T: SerialBuffer>(&self, _buffer: &mut T) {
        todo!();
    }
}

/// Linking metadata for common data
/// A COMDAT group may contain one or more functions, data segments, and/or custom sections.
/// The linker will include all of these elements with a given group name from one object file,
/// and will exclude any element with this group name from all other object files.
#[allow(dead_code)]
#[derive(Debug)]
pub struct LinkingComdat<'a> {
    name: String,
    flags: u32,
    syms: Vec<'a, ComdatSym>,
}

impl<'a> Serialize for LinkingComdat<'a> {
    fn serialize<T: SerialBuffer>(&self, _buffer: &mut T) {
        todo!();
    }
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
pub enum WasmObjectSymbol {
    Defined {
        flags: u32,
        index: u32,
        name: String,
    },
    Imported {
        flags: u32,
        index: u32,
    },
}

impl Serialize for WasmObjectSymbol {
    fn serialize<T: SerialBuffer>(&self, buffer: &mut T) {
        match self {
            Self::Defined { flags, index, name } => {
                buffer.encode_u32(*flags);
                buffer.encode_u32(*index);
                buffer.encode_u32(name.len() as u32);
                buffer.append_slice(name.as_bytes());
            }
            Self::Imported { flags, index } => {
                buffer.encode_u32(*flags);
                buffer.encode_u32(*index);
            }
        }
    }
}

#[derive(Clone, Debug)]
pub enum DataSymbol {
    Defined {
        flags: u32,
        name: String,
        segment_index: u32,
        segment_offset: u32,
        size: u32,
    },
    Imported {
        flags: u32,
        name: String,
    },
}

impl Serialize for DataSymbol {
    fn serialize<T: SerialBuffer>(&self, buffer: &mut T) {
        match self {
            Self::Defined {
                flags,
                name,
                segment_index,
                segment_offset,
                size,
            } => {
                buffer.encode_u32(*flags);
                buffer.encode_u32(name.len() as u32);
                buffer.append_slice(name.as_bytes());
                buffer.encode_u32(*segment_index);
                buffer.encode_u32(*segment_offset);
                buffer.encode_u32(*size);
            }
            Self::Imported { flags, name } => {
                buffer.encode_u32(*flags);
                buffer.encode_u32(name.len() as u32);
                buffer.append_slice(name.as_bytes());
            }
        }
    }
}

/// section index (not section id!)
#[derive(Clone, Debug)]
pub struct SectionSymbol {
    flags: u32,
    index: u32,
}

impl Serialize for SectionSymbol {
    fn serialize<T: SerialBuffer>(&self, buffer: &mut T) {
        self.flags.serialize(buffer);
        self.index.serialize(buffer);
    }
}

#[derive(Clone, Debug)]
pub enum SymInfo {
    Function(WasmObjectSymbol),
    Data(DataSymbol),
    Global(WasmObjectSymbol),
    Section(SectionSymbol),
    Event(WasmObjectSymbol),
    Table(WasmObjectSymbol),
}

impl SymInfo {
    pub fn for_function(wasm_function_index: u32, name: String) -> Self {
        SymInfo::Function(WasmObjectSymbol::Defined {
            flags: 0,
            index: wasm_function_index,
            name,
        })
    }
}

impl Serialize for SymInfo {
    fn serialize<T: SerialBuffer>(&self, buffer: &mut T) {
        buffer.append_u8(match self {
            Self::Function(_) => 0,
            Self::Data(_) => 1,
            Self::Global(_) => 2,
            Self::Section(_) => 3,
            Self::Event(_) => 4,
            Self::Table(_) => 5,
        });
        match self {
            Self::Function(x) => x.serialize(buffer),
            Self::Data(x) => x.serialize(buffer),
            Self::Global(x) => x.serialize(buffer),
            Self::Section(x) => x.serialize(buffer),
            Self::Event(x) => x.serialize(buffer),
            Self::Table(x) => x.serialize(buffer),
        };
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

fn serialize_subsection<I: Serialize, T: SerialBuffer>(
    buffer: &mut T,
    id: SubSectionId,
    items: &[I],
) {
    if !items.is_empty() {
        buffer.append_u8(id as u8);
        let payload_len_index = buffer.reserve_padded_u32();
        let payload_start_index = buffer.size();
        items.serialize(buffer);
        buffer.overwrite_padded_u32(
            payload_len_index,
            (buffer.size() - payload_start_index) as u32,
        );
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
#[derive(Debug)]
pub struct LinkingSection<'a> {
    pub symbol_table: Vec<'a, SymInfo>,
    pub segment_info: Vec<'a, LinkingSegment>,
    pub init_funcs: Vec<'a, LinkingInitFunc>,
    pub comdat_info: Vec<'a, LinkingComdat<'a>>,
}

impl<'a> LinkingSection<'a> {
    pub fn new(arena: &'a Bump) -> Self {
        LinkingSection {
            symbol_table: Vec::with_capacity_in(16, arena),
            segment_info: Vec::with_capacity_in(16, arena),
            init_funcs: Vec::with_capacity_in(0, arena),
            comdat_info: Vec::with_capacity_in(0, arena),
        }
    }
}

impl<'a> Serialize for LinkingSection<'a> {
    fn serialize<T: SerialBuffer>(&self, buffer: &mut T) {
        let header_indices = write_custom_section_header(buffer, "linking");
        buffer.append_u8(LINKING_VERSION);

        serialize_subsection(buffer, SubSectionId::SymbolTable, &self.symbol_table);
        serialize_subsection(buffer, SubSectionId::SegmentInfo, &self.segment_info);
        serialize_subsection(buffer, SubSectionId::InitFuncs, &self.init_funcs);
        serialize_subsection(buffer, SubSectionId::ComdatInfo, &self.comdat_info);

        update_section_size(buffer, header_indices);
    }
}
