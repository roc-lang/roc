use bumpalo::collections::vec::Vec;

use crate::code_builder::Align;
use crate::serialize::{SerialBuffer, Serialize};

#[repr(u8)]
#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum SectionId {
    Custom = 0,
    Type = 1,
    Import = 2,
    Function = 3,
    Table = 4,
    Memory = 5,
    Global = 6,
    Export = 7,
    Start = 8,
    Element = 9,
    Code = 10,
    Data = 11,
    DataCount = 12,
}

struct SectionHeaderIndices {
    size_index: usize,
    body_index: usize,
}

/// Write a section header, returning the position of the encoded length
fn _write_section_header<T: SerialBuffer>(buffer: &mut T, id: SectionId) -> SectionHeaderIndices {
    buffer.append_byte(id as u8);
    let size_index = buffer.reserve_padded_u32();
    let body_index = buffer.size();
    SectionHeaderIndices {
        size_index,
        body_index,
    }
}

/// Write a custom section header, returning the position of the encoded length
fn write_custom_section_header<T: SerialBuffer>(
    buffer: &mut T,
    name: &str,
) -> SectionHeaderIndices {
    // buffer.append_byte(SectionId::Custom as u8); // TODO: uncomment when we get rid of parity_wasm
    let size_index = buffer.reserve_padded_u32();
    let body_index = buffer.size();
    buffer.append_slice(name.as_bytes());
    SectionHeaderIndices {
        size_index,
        body_index,
    }
}

/// Update a section header with its final size, after writing the bytes
fn update_section_size<T: SerialBuffer>(buffer: &mut T, header_indices: SectionHeaderIndices) {
    let size = buffer.size() - header_indices.body_index;
    buffer.overwrite_padded_u32(header_indices.size_index, size as u32);
}

fn serialize_vector_with_count<'a, SB, S>(buffer: &mut SB, items: &Vec<'a, S>)
where
    SB: SerialBuffer,
    S: Serialize,
{
    buffer.encode_u32(items.len() as u32);
    for item in items.iter() {
        item.serialize(buffer);
    }
}

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
                buffer.append_byte(*type_id as u8);
                buffer.encode_u32(*offset);
                buffer.encode_u32(*symbol_index);
            }
            Self::Offset {
                type_id,
                offset,
                symbol_index,
                addend,
            } => {
                buffer.append_byte(*type_id as u8);
                buffer.encode_u32(*offset);
                buffer.encode_u32(*symbol_index);
                buffer.encode_i32(*addend);
            }
        }
    }
}

pub struct RelocationSection<'a> {
    pub name: String,
    pub entries: Vec<'a, RelocationEntry>,
}

impl<'a> Serialize for RelocationSection<'a> {
    fn serialize<T: SerialBuffer>(&self, buffer: &mut T) {
        let header_indices = write_custom_section_header(buffer, &self.name);
        for entry in self.entries.iter() {
            entry.serialize(buffer);
        }
        update_section_size(buffer, header_indices);
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
pub struct LinkingSegment {
    pub name: String,
    pub alignment: Align,
    pub flags: u32,
}
impl Serialize for LinkingSegment {
    fn serialize<T: SerialBuffer>(&self, _buffer: &mut T) {
        todo!();
    }
}

/// Linking metadata for init (start) functions
pub struct LinkingInitFunc {
    pub priority: u32,
    pub symbol_index: u32, // index in the symbol table, not the function index
}
impl Serialize for LinkingInitFunc {
    fn serialize<T: SerialBuffer>(&self, _buffer: &mut T) {
        todo!();
    }
}

//----------------
//
// Common data
//
//----------------

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

//----------------
//
// Symbol table
//
//----------------

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

pub enum WasmObjectSymbol {
    Defined { index: u32, name: String },
    Imported { index: u32 },
}
impl Serialize for WasmObjectSymbol {
    fn serialize<T: SerialBuffer>(&self, buffer: &mut T) {
        match self {
            Self::Defined { index, name } => {
                buffer.encode_u32(*index);
                buffer.encode_u32(name.len() as u32);
                buffer.append_slice(name.as_bytes());
            }
            Self::Imported { index } => {
                buffer.encode_u32(*index);
            }
        }
    }
}

pub enum DataSymbol {
    Defined {
        name: String,
        index: u32,
        offset: u32,
        size: u32,
    },
    Imported {
        name: String,
    },
}
impl Serialize for DataSymbol {
    fn serialize<T: SerialBuffer>(&self, buffer: &mut T) {
        match self {
            Self::Defined {
                name,
                index,
                offset,
                size,
            } => {
                buffer.encode_u32(name.len() as u32);
                buffer.append_slice(name.as_bytes());
                buffer.encode_u32(*index);
                buffer.encode_u32(*offset);
                buffer.encode_u32(*size);
            }
            Self::Imported { name } => {
                buffer.encode_u32(name.len() as u32);
                buffer.append_slice(name.as_bytes());
            }
        }
    }
}

/// section index (not section id!)
#[derive(Clone, Copy, Debug)]
pub struct SectionIndex(u32);

pub enum SymInfoFields {
    Function(WasmObjectSymbol),
    Data(DataSymbol),
    Global(WasmObjectSymbol),
    Section(SectionIndex),
    Event(WasmObjectSymbol),
    Table(WasmObjectSymbol),
}

pub struct SymInfo {
    flags: u32,
    info: SymInfoFields,
}
impl SymInfo {
    pub fn for_function(wasm_function_index: u32, name: String) -> Self {
        let linking_symbol = WasmObjectSymbol::Defined {
            index: wasm_function_index,
            name,
        };
        SymInfo {
            flags: 0,
            info: SymInfoFields::Function(linking_symbol),
        }
    }
}

impl Serialize for SymInfo {
    fn serialize<T: SerialBuffer>(&self, buffer: &mut T) {
        buffer.append_byte(match self.info {
            SymInfoFields::Function(_) => 0,
            SymInfoFields::Data(_) => 1,
            SymInfoFields::Global(_) => 2,
            SymInfoFields::Section(_) => 3,
            SymInfoFields::Event(_) => 4,
            SymInfoFields::Table(_) => 5,
        });
        buffer.encode_u32(self.flags);
        match &self.info {
            SymInfoFields::Function(x) => x.serialize(buffer),
            SymInfoFields::Data(x) => x.serialize(buffer),
            SymInfoFields::Global(x) => x.serialize(buffer),
            SymInfoFields::Section(SectionIndex(x)) => {
                buffer.encode_u32(*x);
            }
            SymInfoFields::Event(x) => x.serialize(buffer),
            SymInfoFields::Table(x) => x.serialize(buffer),
        };
    }
}

//--------------------------------
//
//  Linking subsections
//
//--------------------------------

pub enum LinkingSubSection<'a> {
    /// Extra metadata about the data segments.
    SegmentInfo(Vec<'a, LinkingSegment>),
    /// Specifies a list of constructor functions to be called at startup.
    /// These constructors will be called in priority order after memory has been initialized.
    InitFuncs(Vec<'a, LinkingInitFunc>),
    /// Specifies the COMDAT groups of associated linking objects, which are linked only once and all together.
    ComdatInfo(Vec<'a, LinkingComdat<'a>>),
    /// Specifies extra information about the symbols present in the module.
    SymbolTable(Vec<'a, SymInfo>),
}
impl<'a> Serialize for LinkingSubSection<'a> {
    fn serialize<T: SerialBuffer>(&self, buffer: &mut T) {
        buffer.append_byte(match self {
            Self::SegmentInfo(_) => 5,
            Self::InitFuncs(_) => 6,
            Self::ComdatInfo(_) => 7,
            Self::SymbolTable(_) => 8,
        });
        let payload_len_index = buffer.reserve_padded_u32();
        let payload_start_index = buffer.size();
        match self {
            Self::SegmentInfo(items) => serialize_vector_with_count(buffer, items),
            Self::InitFuncs(items) => serialize_vector_with_count(buffer, items),
            Self::ComdatInfo(items) => serialize_vector_with_count(buffer, items),
            Self::SymbolTable(items) => serialize_vector_with_count(buffer, items),
        }
        buffer.overwrite_padded_u32(
            payload_len_index,
            (buffer.size() - payload_start_index) as u32,
        );
    }
}

const LINKING_VERSION: u8 = 2;

pub struct LinkingSection<'a> {
    pub subsections: Vec<'a, LinkingSubSection<'a>>,
}
impl<'a> Serialize for LinkingSection<'a> {
    fn serialize<T: SerialBuffer>(&self, buffer: &mut T) {
        let header_indices = write_custom_section_header(buffer, "linking");
        buffer.append_byte(LINKING_VERSION);
        for subsection in self.subsections.iter() {
            subsection.serialize(buffer);
        }
        update_section_size(buffer, header_indices);
    }
}
