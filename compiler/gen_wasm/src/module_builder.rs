use bumpalo::collections::vec::Vec;
use bumpalo::Bump;
use roc_module::symbol::{Interns, Symbol};

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

/// Write a section header, returning the position of the encoded length
fn _write_section_header<T: SerialBuffer>(buffer: &mut T, id: SectionId) -> usize {
    buffer.append_byte(id as u8);
    buffer.encode_padded_u32(u32::MAX)
}

/// Write a custom section header, returning the position of the encoded length
fn write_custom_section_header<'s, T: SerialBuffer>(
    buffer: &mut T,
    id: SectionId,
    name: &'s str,
) -> usize {
    buffer.append_byte(id as u8);
    let size_position = buffer.encode_padded_u32(u32::MAX);
    buffer.append_slice(name.as_bytes());
    size_position
}

/*******************************************************************
 *
 * Relocation sections
 *
 * https://github.com/WebAssembly/tool-conventions/blob/main/Linking.md#relocation-sections
 *
 *******************************************************************/

pub struct IndexRelocation {
    offset: u32,       // offset 0 means the next byte after section id and size
    symbol_index: u32, // index in symbol table
}

pub struct OffsetRelocation {
    offset: u32,       // offset 0 means the next byte after section id and size
    symbol_index: u32, // index in symbol table
    addend: u32,
}

pub enum RelocationEntry {
    FunctionIndexLeb(IndexRelocation),
    TableIndexSleb(IndexRelocation),
    TableIndexI32(IndexRelocation),
    MemoryAddrLeb(OffsetRelocation),
    MemoryAddrSleb(OffsetRelocation),
    MemoryAddrI32(OffsetRelocation),
    TypeIndexLeb(IndexRelocation),
    GlobalIndexLeb(IndexRelocation),
    FunctionOffsetI32(OffsetRelocation),
    SectionOffsetI32(OffsetRelocation),
    EventIndexLeb(IndexRelocation),
    // ... gap ...
    GlobalIndexI32(IndexRelocation),
    MemoryAddrLeb64(OffsetRelocation),
    MemoryAddrSleb64(OffsetRelocation),
    MemoryAddrI64(OffsetRelocation),
    // ... gap ...
    TableIndexSleb64(IndexRelocation),
    TableIndexI64(IndexRelocation),
    TableNumberLeb(IndexRelocation),
}

impl Serialize for RelocationEntry {
    fn serialize<T: SerialBuffer>(&self, buffer: &mut T) {
        let type_id = match self {
            Self::FunctionIndexLeb(_) => 0,
            Self::TableIndexSleb(_) => 1,
            Self::TableIndexI32(_) => 2,
            Self::MemoryAddrLeb(_) => 3,
            Self::MemoryAddrSleb(_) => 4,
            Self::MemoryAddrI32(_) => 5,
            Self::TypeIndexLeb(_) => 6,
            Self::GlobalIndexLeb(_) => 7,
            Self::FunctionOffsetI32(_) => 8,
            Self::SectionOffsetI32(_) => 9,
            Self::EventIndexLeb(_) => 10,
            // ... gap ...
            Self::GlobalIndexI32(_) => 13,
            Self::MemoryAddrLeb64(_) => 14,
            Self::MemoryAddrSleb64(_) => 15,
            Self::MemoryAddrI64(_) => 16,
            // ... gap ...
            Self::TableIndexSleb64(_) => 18,
            Self::TableIndexI64(_) => 19,
            Self::TableNumberLeb(_) => 20,
        };

        buffer.append_byte(type_id);
        buffer.encode_u32(self.offset);
        buffer.encode_u32(self.symbol_index);
        if let Some(addend) = self.addend {
            buffer.encode_i32(addend);
        }
    }
}

pub struct RelocationSection<'s, 'bump> {
    name: &'s str,
    entries: Vec<'bump, RelocationEntry>,
}

impl<'s, 'bump> Serialize for RelocationSection<'s, 'bump> {
    fn serialize<T: SerialBuffer>(&self, buffer: &mut T) {
        let size_position = write_custom_section_header(buffer, SectionId::Custom, self.name);
        for entry in self.entries.iter() {
            entry.serialize(buffer);
        }
        let size = buffer.size() - size_position;
        buffer.overwrite_padded_u32(size_position, size as u32);
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
pub struct LinkingSegment<'s> {
    name: &'s str,
    alignment: Align,
    flags: u32,
}

/// Linking metadata for init (start) functions
pub struct LinkingInitFunc {
    priority: u32,
    symbol_index: u32, // index in the symbol table, not the function index
}

/*****************
 * Common data
 *****************/

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
    kind: ComdatSymKind,
    index: u32,
}

/// Linking metadata for common data
/// A COMDAT group may contain one or more functions, data segments, and/or custom sections.
/// The linker will include all of these elements with a given group name from one object file,
/// and will exclude any element with this group name from all other object files.
pub struct LinkingComdat<'s, 'bump> {
    name: &'s str,
    flags: u32,
    syms: Vec<'bump, ComdatSym>,
}

/*****************
 * Symbol table
*****************/

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

pub struct DefinedWasmObjectSymbol<'s> {
    wasm_object_index: u32, // Wasm index of the function/global/event/table
    name: &'s str,
}

pub enum WasmObjectSymbol<'s> {
    Defined(DefinedWasmObjectSymbol<'s>),
    Imported(u32),
}

pub struct DefinedDataSymbol<'s> {
    name: &'s str,
    index: u32,
    offset: u32,
    size: u32,
}

pub enum DataSymbol<'s> {
    Defined(DefinedDataSymbol<'s>),
    Imported(&'s str),
}

pub enum SymTableInfo<'s> {
    Data(DataSymbol<'s>),
    Function(WasmObjectSymbol<'s>),
    Global(WasmObjectSymbol<'s>),
    /// section index (not id)
    Section(u32),
    Event(WasmObjectSymbol<'s>),
    Table(WasmObjectSymbol<'s>),
}

pub struct SymTableEntry<'s> {
    flags: u32,
    kind: SymTableInfo<'s>,
}

/**********************************
 * Linking section & subsections
 **********************************/

pub enum LinkingSubSection<'s, 'bump> {
    SegmentInfo(Vec<'bump, LinkingSegment<'s>>),
    InitFuncs(Vec<'bump, LinkingInitFunc>),
    ComdatInfo(Vec<'bump, LinkingComdat<'s, 'bump>>),
    SymbolTable(Vec<'bump, SymTableEntry<'s>>),
}

#[repr(u8)]
#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum LinkingVersion {
    V2 = 2,
}

pub struct LinkingSection<'s, 'bump> {
    version: LinkingVersion,
    subsections: Vec<'bump, LinkingSubSection<'s, 'bump>>,
}
