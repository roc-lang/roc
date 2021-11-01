use bumpalo::collections::vec::Vec;
use bumpalo::Bump;

use crate::code_builder::{Align, ValueType};
use crate::opcodes;
use crate::serialize::{SerialBuffer, Serialize};

/*******************************************************************
 *
 * Helpers
 *
 *******************************************************************/

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

#[repr(u8)]
#[derive(PartialEq, Eq, Clone, Copy, Debug)]
enum ImportExportType {
    Func = 0,
    Table = 1,
    Mem = 2,
    Global = 3,
}

struct SectionHeaderIndices {
    size_index: usize,
    body_index: usize,
}

/// Write a section header, returning the position of the encoded length
fn write_section_header<T: SerialBuffer>(buffer: &mut T, id: SectionId) -> SectionHeaderIndices {
    buffer.append_u8(id as u8);
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
    // buffer.append_u8(SectionId::Custom as u8);
    let size_index = buffer.reserve_padded_u32();
    let body_index = buffer.size();
    name.serialize(buffer);
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

fn serialize_vector_with_count<'a, SB, S>(buffer: &mut SB, items: &[S])
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
 * Type section (function signature definitions)
 *
 *******************************************************************/

impl<'a> Serialize for [ValueType] {
    fn serialize<T: SerialBuffer>(&self, buffer: &mut T) {
        // reserve one byte for num_batches
        let start = buffer.size();
        buffer.append_u8(0);

        if self.is_empty() {
            return;
        }

        // Write declarations in batches of the same ValueType
        let mut num_batches: u32 = 0;
        let mut batch_type = self[0];
        let mut batch_size = 0;
        for t in self {
            if *t == batch_type {
                batch_size += 1;
            } else {
                buffer.encode_u32(batch_size);
                buffer.append_u8(batch_type as u8);
                batch_type = *t;
                batch_size = 1;
                num_batches += 1;
            }
        }
        buffer.encode_u32(batch_size);
        buffer.append_u8(batch_type as u8);
        num_batches += 1;

        // Go back and write the number of batches at the start
        if num_batches < 128 {
            buffer.overwrite_u8(start, num_batches as u8);
        } else {
            // We need more than 1 byte to encode num_batches!
            // This is a ridiculous edge case, so just pad to 5 bytes for simplicity
            buffer.insert_space_at(1, 4);
            buffer.overwrite_padded_u32(0, num_batches);
        }
    }
}

struct Signature<'a> {
    param_types: Vec<'a, ValueType>,
    ret_type: Option<ValueType>,
}

impl<'a> Serialize for Signature<'a> {
    fn serialize<T: SerialBuffer>(&self, buffer: &mut T) {
        buffer.append_u8(0x60);
        self.param_types.serialize(buffer);
        match self.ret_type {
            Some(t) => [t].serialize(buffer),
            None => buffer.append_u8(0), // vector of length zero
        }
    }
}

pub struct TypeSection<'a> {
    signatures: Vec<'a, Signature<'a>>,
}

impl<'a> TypeSection<'a> {
    pub fn new(arena: &'a Bump) -> Self {
        TypeSection {
            signatures: Vec::with_capacity_in(8, arena),
        }
    }
}

impl<'a> Serialize for TypeSection<'a> {
    fn serialize<T: SerialBuffer>(&self, buffer: &mut T) {
        let header_indices = write_section_header(buffer, SectionId::Type);
        serialize_vector_with_count(buffer, &self.signatures);
        update_section_size(buffer, header_indices);
    }
}

/*******************************************************************
 *
 * Import section
 *
 *******************************************************************/

#[repr(u8)]
#[derive(PartialEq, Eq, Clone, Copy, Debug)]
enum RefType {
    Func = 0x70,
    Extern = 0x6f,
}

struct TableType {
    ref_type: RefType,
    limits: Limits,
}

impl Serialize for TableType {
    fn serialize<T: SerialBuffer>(&self, buffer: &mut T) {
        buffer.append_u8(self.ref_type as u8);
        self.limits.serialize(buffer);
    }
}

enum ImportDesc {
    Func { signature_index: u32 },
    Table { ty: TableType },
    Mem { limits: Limits },
    Global { ty: GlobalType },
}

struct Import {
    module: String,
    name: String,
    description: ImportDesc,
}

impl Serialize for Import {
    fn serialize<T: SerialBuffer>(&self, buffer: &mut T) {
        self.module.serialize(buffer);
        self.name.serialize(buffer);
        match &self.description {
            ImportDesc::Func { signature_index } => {
                buffer.append_u8(0);
                buffer.encode_u32(*signature_index);
            }
            ImportDesc::Table { ty } => {
                buffer.append_u8(1);
                ty.serialize(buffer);
            }
            ImportDesc::Mem { limits } => {
                buffer.append_u8(2);
                limits.serialize(buffer);
            }
            ImportDesc::Global { ty } => {
                buffer.append_u8(3);
                ty.serialize(buffer);
            }
        }
    }
}

pub struct ImportSection<'a>(Vec<'a, Import>);

impl<'a> ImportSection<'a> {
    pub fn new(arena: &'a Bump) -> Self {
        ImportSection(bumpalo::vec![in arena])
    }
}

impl<'a> Serialize for ImportSection<'a> {
    fn serialize<T: SerialBuffer>(&self, buffer: &mut T) {
        if self.0.is_empty() {
            return;
        }
        let header_indices = write_section_header(buffer, SectionId::Import);
        serialize_vector_with_count(buffer, &self.0);
        update_section_size(buffer, header_indices);
    }
}

/*******************************************************************
 *
 * Function section (map function index to signature index)
 *
 *******************************************************************/

pub struct FunctionSection<'a> {
    pub signature_indices: Vec<'a, u32>,
}

impl<'a> FunctionSection<'a> {
    pub fn new(arena: &'a Bump) -> Self {
        FunctionSection {
            signature_indices: Vec::with_capacity_in(8, arena),
        }
    }
}

impl<'a> Serialize for FunctionSection<'a> {
    fn serialize<T: SerialBuffer>(&self, buffer: &mut T) {
        let header_indices = write_section_header(buffer, SectionId::Function);
        serialize_vector_with_count(buffer, &self.signature_indices);
        update_section_size(buffer, header_indices);
    }
}

/*******************************************************************
 *
 * Memory section
 *
 *******************************************************************/

enum Limits {
    Min(u32),
    MinMax(u32, u32),
}

impl Serialize for Limits {
    fn serialize<T: SerialBuffer>(&self, buffer: &mut T) {
        match self {
            Self::Min(min) => {
                buffer.append_u8(0);
                buffer.encode_u32(*min);
            }
            Self::MinMax(min, max) => {
                buffer.append_u8(1);
                buffer.encode_u32(*min);
                buffer.encode_u32(*max);
            }
        }
    }
}

pub struct MemorySection(Option<Limits>);

impl MemorySection {
    pub const PAGE_SIZE: u32 = 64 * 1024;

    pub fn new(bytes: u32) -> Self {
        if bytes == 0 {
            MemorySection(None)
        } else {
            let pages = (bytes + Self::PAGE_SIZE - 1) / Self::PAGE_SIZE;
            MemorySection(Some(Limits::Min(pages)))
        }
    }
}

impl Serialize for MemorySection {
    fn serialize<T: SerialBuffer>(&self, buffer: &mut T) {
        if let Some(limits) = &self.0 {
            let header_indices = write_section_header(buffer, SectionId::Memory);
            buffer.append_u8(1);
            limits.serialize(buffer);
            update_section_size(buffer, header_indices);
        }
    }
}

/*******************************************************************
 *
 * Global section
 *
 *******************************************************************/

struct GlobalType {
    value_type: ValueType,
    is_mutable: bool,
}

impl Serialize for GlobalType {
    fn serialize<T: SerialBuffer>(&self, buffer: &mut T) {
        buffer.append_u8(self.value_type as u8);
        buffer.append_u8(self.is_mutable as u8);
    }
}

enum GlobalInitValue {
    I32(i32),
    I64(i64),
    F32(f32),
    F64(f64),
}

struct Global {
    ty: GlobalType,
    init_value: GlobalInitValue,
}

impl Serialize for Global {
    fn serialize<T: SerialBuffer>(&self, buffer: &mut T) {
        self.ty.serialize(buffer);
        match self.init_value {
            GlobalInitValue::I32(x) => {
                buffer.append_u8(opcodes::I32CONST);
                buffer.encode_i32(x);
            }
            GlobalInitValue::I64(x) => {
                buffer.append_u8(opcodes::I64CONST);
                buffer.encode_i64(x);
            }
            GlobalInitValue::F32(x) => {
                buffer.append_u8(opcodes::F32CONST);
                buffer.encode_f32(x);
            }
            GlobalInitValue::F64(x) => {
                buffer.append_u8(opcodes::F64CONST);
                buffer.encode_f64(x);
            }
        }
    }
}

pub struct GlobalSection<'a>(Vec<'a, Global>);

impl<'a> GlobalSection<'a> {
    pub fn new(arena: &'a Bump) -> Self {
        GlobalSection(Vec::with_capacity_in(1, arena))
    }
}

impl<'a> Serialize for GlobalSection<'a> {
    fn serialize<T: SerialBuffer>(&self, buffer: &mut T) {
        todo!();
    }
}

/*******************************************************************
 *
 * Export section
 *
 *******************************************************************/

struct Export {
    name: String,
    ty: ImportExportType,
    index: u32,
}

pub struct ExportSection<'a> {
    todo: &'a str,
}

impl<'a> ExportSection<'a> {
    pub fn new(arena: &'a Bump) {}
}

impl<'a> Serialize for ExportSection<'a> {
    fn serialize<T: SerialBuffer>(&self, buffer: &mut T) {
        todo!();
    }
}

/*******************************************************************
 *
 * Code section
 *
 *******************************************************************/

pub struct CodeSection<'a> {
    bytes: Vec<'a, u8>,
}

impl<'a> CodeSection<'a> {
    pub fn new(arena: &'a Bump) -> Self {
        CodeSection {
            bytes: Vec::with_capacity_in(4096, arena),
        }
    }
}

impl<'a> Serialize for CodeSection<'a> {
    fn serialize<T: SerialBuffer>(&self, buffer: &mut T) {
        todo!();
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

#[derive(Debug)]
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
    fn new(arena: &'a Bump, name: &'a str) -> Self {
        RelocationSection {
            name,
            target_section_index: None,
            entries: Vec::with_capacity_in(64, arena),
        }
    }
}

impl<'a> Serialize for RelocationSection<'a> {
    fn serialize<T: SerialBuffer>(&self, buffer: &mut T) {
        let header_indices = write_custom_section_header(buffer, self.name);
        buffer.encode_u32(self.target_section_index.unwrap());
        serialize_vector_with_count(buffer, &self.entries);
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
    fn serialize<T: SerialBuffer>(&self, buffer: &mut T) {
        todo!();
    }
}

/// Linking metadata for init (start) functions
pub struct LinkingInitFunc {
    pub priority: u32,
    pub symbol_index: u32, // index in the symbol table, not the function index
}

impl Serialize for LinkingInitFunc {
    fn serialize<T: SerialBuffer>(&self, buffer: &mut T) {
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

pub struct ComdatSym {
    pub kind: ComdatSymKind,
    pub index: u32,
}

impl Serialize for ComdatSym {
    fn serialize<T: SerialBuffer>(&self, buffer: &mut T) {
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
    fn serialize<T: SerialBuffer>(&self, buffer: &mut T) {
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
        buffer.append_u8(match self.info {
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

//----------------------------------------------------------------
//  Linking subsections
//----------------------------------------------------------------

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
        buffer.append_u8(match self {
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

//----------------------------------------------------------------
//  Linking metadata section
//----------------------------------------------------------------

const LINKING_VERSION: u8 = 2;

pub struct LinkingSection<'a> {
    pub subsections: Vec<'a, LinkingSubSection<'a>>,
}

impl<'a> LinkingSection<'a> {
    fn new(arena: &'a Bump) -> Self {
        LinkingSection {
            subsections: Vec::with_capacity_in(1, arena),
        }
    }
}

impl<'a> Serialize for LinkingSection<'a> {
    fn serialize<T: SerialBuffer>(&self, buffer: &mut T) {
        let header_indices = write_custom_section_header(buffer, "linking");
        buffer.append_u8(LINKING_VERSION);
        for subsection in self.subsections.iter() {
            subsection.serialize(buffer);
        }
        update_section_size(buffer, header_indices);
    }
}

/*******************************************************************
 *
 * Module
 *
 * https://webassembly.github.io/spec/core/binary/modules.html
 *
 *******************************************************************/

type UnusedSection<'a> = &'a str;

pub struct WasmModule<'a> {
    pub types: &'a str,      // TODO
    pub import: &'a str,     // TODO
    pub function: &'a str,   // TODO
    pub table: &'a str,      // TODO
    pub memory: &'a str,     // TODO
    pub global: &'a str,     // TODO
    pub export: &'a str,     // TODO
    pub start: &'a str,      // TODO
    pub element: &'a str,    // TODO
    pub data_count: &'a str, // TODO
    pub code: &'a str,       // TODO
    pub data: &'a str,       // TODO
    pub linking: LinkingSection<'a>,
    pub reloc_code: RelocationSection<'a>,
    pub reloc_data: RelocationSection<'a>,
}

fn maybe_increment_section(size: usize, prev_size: &mut usize, index: &mut u32) {
    if size > *prev_size {
        *index += 1;
        *prev_size = size;
    }
}

impl<'a> WasmModule<'a> {
    const WASM_VERSION: u32 = 1;

    pub fn new(arena: &'a Bump) -> Self {
        WasmModule {
            types: "",
            import: "",
            function: "",
            table: "",
            memory: "",
            global: "",
            export: "",
            start: "",
            element: "",
            data_count: "",
            code: "",
            data: "",
            linking: LinkingSection::new(arena),
            reloc_code: RelocationSection::new(arena, "reloc.CODE"),
            reloc_data: RelocationSection::new(arena, "reloc.DATA"),
        }
    }

    #[allow(dead_code)]
    fn serialize<T: SerialBuffer>(&mut self, buffer: &mut T) {
        buffer.append_u8(0);
        buffer.append_slice("asm".as_bytes());
        buffer.write_unencoded_u32(Self::WASM_VERSION);

        let mut index: u32 = 0;
        let mut prev_size = buffer.size();

        self.types.serialize(buffer);
        maybe_increment_section(buffer.size(), &mut prev_size, &mut index);

        self.import.serialize(buffer);
        maybe_increment_section(buffer.size(), &mut prev_size, &mut index);

        self.function.serialize(buffer);
        maybe_increment_section(buffer.size(), &mut prev_size, &mut index);

        self.table.serialize(buffer);
        maybe_increment_section(buffer.size(), &mut prev_size, &mut index);

        self.memory.serialize(buffer);
        maybe_increment_section(buffer.size(), &mut prev_size, &mut index);

        self.global.serialize(buffer);
        maybe_increment_section(buffer.size(), &mut prev_size, &mut index);

        self.export.serialize(buffer);
        maybe_increment_section(buffer.size(), &mut prev_size, &mut index);

        self.start.serialize(buffer);
        maybe_increment_section(buffer.size(), &mut prev_size, &mut index);

        self.element.serialize(buffer);
        maybe_increment_section(buffer.size(), &mut prev_size, &mut index);

        self.data_count.serialize(buffer);
        maybe_increment_section(buffer.size(), &mut prev_size, &mut index);

        self.code.serialize(buffer);
        self.reloc_code.target_section_index = Some(index);
        maybe_increment_section(buffer.size(), &mut prev_size, &mut index);

        self.data.serialize(buffer);
        self.reloc_data.target_section_index = Some(index);

        self.linking.serialize(buffer);
        self.reloc_code.serialize(buffer);
        self.reloc_data.serialize(buffer);
    }
}
