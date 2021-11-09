use bumpalo::collections::vec::Vec;
use bumpalo::Bump;

use super::linking::{LinkingSection, RelocationEntry, RelocationSection};
use super::opcodes;
use super::serialize::{SerialBuffer, Serialize};
use super::{CodeBuilder, ValueType};

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

pub struct SectionHeaderIndices {
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
pub fn write_custom_section_header<T: SerialBuffer>(
    buffer: &mut T,
    name: &str,
) -> SectionHeaderIndices {
    buffer.append_u8(SectionId::Custom as u8);
    let size_index = buffer.reserve_padded_u32();
    let body_index = buffer.size();
    name.serialize(buffer);
    SectionHeaderIndices {
        size_index,
        body_index,
    }
}

/// Update a section header with its final size, after writing the bytes
pub fn update_section_size<T: SerialBuffer>(buffer: &mut T, header_indices: SectionHeaderIndices) {
    let size = buffer.size() - header_indices.body_index;
    buffer.overwrite_padded_u32(header_indices.size_index, size as u32);
}

/// Serialize a section that is just a vector of some struct
fn serialize_vector_section<B: SerialBuffer, T: Serialize>(
    buffer: &mut B,
    section_id: SectionId,
    subsections: &[T],
) {
    if !subsections.is_empty() {
        let header_indices = write_section_header(buffer, section_id);
        subsections.serialize(buffer);
        update_section_size(buffer, header_indices);
    }
}

/*******************************************************************
 *
 * Type section
 * Deduplicated list of function type signatures
 *
 *******************************************************************/

#[derive(PartialEq, Eq)]
pub struct Signature<'a> {
    pub param_types: Vec<'a, ValueType>,
    pub ret_type: Option<ValueType>,
}

impl<'a> Serialize for Signature<'a> {
    fn serialize<T: SerialBuffer>(&self, buffer: &mut T) {
        buffer.append_u8(0x60);
        self.param_types.serialize(buffer);
        self.ret_type.serialize(buffer);
    }
}

pub struct TypeSection<'a> {
    /// Private. See WasmModule::add_function_signature
    signatures: Vec<'a, Signature<'a>>,
}

impl<'a> TypeSection<'a> {
    pub fn new(arena: &'a Bump, capacity: usize) -> Self {
        TypeSection {
            signatures: Vec::with_capacity_in(capacity, arena),
        }
    }

    /// Find a matching signature or insert a new one. Return the index.
    fn insert(&mut self, signature: Signature<'a>) -> u32 {
        // Using linear search because we need to preserve indices stored in
        // the Function section. (Also for practical sizes it's fast)
        let maybe_index = self.signatures.iter().position(|s| *s == signature);
        match maybe_index {
            Some(index) => index as u32,
            None => {
                let index = self.signatures.len();
                self.signatures.push(signature);
                index as u32
            }
        }
    }
}

impl<'a> Serialize for TypeSection<'a> {
    fn serialize<T: SerialBuffer>(&self, buffer: &mut T) {
        serialize_vector_section(buffer, SectionId::Type, &self.signatures);
    }
}

/*******************************************************************
 *
 * Import section
 *
 *******************************************************************/

#[repr(u8)]
#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum RefType {
    Func = 0x70,
    Extern = 0x6f,
}

pub struct TableType {
    pub ref_type: RefType,
    pub limits: Limits,
}

impl Serialize for TableType {
    fn serialize<T: SerialBuffer>(&self, buffer: &mut T) {
        buffer.append_u8(self.ref_type as u8);
        self.limits.serialize(buffer);
    }
}

pub enum ImportDesc {
    Func { signature_index: u32 },
    Table { ty: TableType },
    Mem { limits: Limits },
    Global { ty: GlobalType },
}

pub struct Import {
    pub module: String,
    pub name: String,
    pub description: ImportDesc,
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

pub struct ImportSection<'a> {
    entries: Vec<'a, Import>,
}

impl<'a> ImportSection<'a> {
    pub fn new(arena: &'a Bump) -> Self {
        ImportSection {
            entries: bumpalo::vec![in arena],
        }
    }
}

impl<'a> Serialize for ImportSection<'a> {
    fn serialize<T: SerialBuffer>(&self, buffer: &mut T) {
        serialize_vector_section(buffer, SectionId::Import, &self.entries);
    }
}

/*******************************************************************
 *
 * Function section
 * Maps function indices (Code section) to signature indices (Type section)
 *
 *******************************************************************/

pub struct FunctionSection<'a> {
    /// Private. See WasmModule::add_function_signature
    signature_indices: Vec<'a, u32>,
}

impl<'a> FunctionSection<'a> {
    pub fn new(arena: &'a Bump, capacity: usize) -> Self {
        FunctionSection {
            signature_indices: Vec::with_capacity_in(capacity, arena),
        }
    }
}
impl<'a> Serialize for FunctionSection<'a> {
    fn serialize<T: SerialBuffer>(&self, buffer: &mut T) {
        serialize_vector_section(buffer, SectionId::Function, &self.signature_indices);
    }
}

/*******************************************************************
 *
 * Memory section
 *
 *******************************************************************/

pub enum Limits {
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

    pub fn min_size(&self) -> Option<u32> {
        match self {
            MemorySection(Some(Limits::Min(min))) | MemorySection(Some(Limits::MinMax(min, _))) => {
                Some(min * Self::PAGE_SIZE)
            }
            MemorySection(None) => None,
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

pub struct GlobalType {
    pub value_type: ValueType,
    pub is_mutable: bool,
}

impl Serialize for GlobalType {
    fn serialize<T: SerialBuffer>(&self, buffer: &mut T) {
        buffer.append_u8(self.value_type as u8);
        buffer.append_u8(self.is_mutable as u8);
    }
}

/// Constant expression for initialising globals or data segments
/// Note: This is restricted for simplicity, but the spec allows arbitrary constant expressions
pub enum ConstExpr {
    I32(i32),
    I64(i64),
    F32(f32),
    F64(f64),
}

impl Serialize for ConstExpr {
    fn serialize<T: SerialBuffer>(&self, buffer: &mut T) {
        match self {
            ConstExpr::I32(x) => {
                buffer.append_u8(opcodes::I32CONST);
                buffer.encode_i32(*x);
            }
            ConstExpr::I64(x) => {
                buffer.append_u8(opcodes::I64CONST);
                buffer.encode_i64(*x);
            }
            ConstExpr::F32(x) => {
                buffer.append_u8(opcodes::F32CONST);
                buffer.encode_f32(*x);
            }
            ConstExpr::F64(x) => {
                buffer.append_u8(opcodes::F64CONST);
                buffer.encode_f64(*x);
            }
        }
        buffer.append_u8(opcodes::END);
    }
}

pub struct Global {
    /// Type and mutability of the global
    pub ty: GlobalType,
    /// Initial value of the global.
    pub init: ConstExpr,
}

impl Serialize for Global {
    fn serialize<T: SerialBuffer>(&self, buffer: &mut T) {
        self.ty.serialize(buffer);
        self.init.serialize(buffer);
    }
}

pub struct GlobalSection<'a> {
    pub entries: Vec<'a, Global>,
}

impl<'a> Serialize for GlobalSection<'a> {
    fn serialize<T: SerialBuffer>(&self, buffer: &mut T) {
        serialize_vector_section(buffer, SectionId::Global, &self.entries);
    }
}

/*******************************************************************
 *
 * Export section
 *
 *******************************************************************/

#[repr(u8)]
#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum ExportType {
    Func = 0,
    Table = 1,
    Mem = 2,
    Global = 3,
}

pub struct Export {
    pub name: String,
    pub ty: ExportType,
    pub index: u32,
}
impl Serialize for Export {
    fn serialize<T: SerialBuffer>(&self, buffer: &mut T) {
        self.name.serialize(buffer);
        buffer.append_u8(self.ty as u8);
        buffer.encode_u32(self.index);
    }
}

pub struct ExportSection<'a> {
    pub entries: Vec<'a, Export>,
}

impl<'a> ExportSection<'a> {
    pub fn new(arena: &'a Bump) -> Self {
        ExportSection {
            entries: bumpalo::vec![in arena],
        }
    }
}

impl<'a> Serialize for ExportSection<'a> {
    fn serialize<T: SerialBuffer>(&self, buffer: &mut T) {
        serialize_vector_section(buffer, SectionId::Export, &self.entries);
    }
}

/*******************************************************************
 *
 * Code section (see also code_builder.rs)
 *
 *******************************************************************/

#[derive(Debug)]
pub struct CodeSection<'a> {
    pub code_builders: Vec<'a, CodeBuilder<'a>>,
}

impl<'a> CodeSection<'a> {
    pub fn new(arena: &'a Bump) -> Self {
        CodeSection {
            code_builders: Vec::with_capacity_in(8, arena),
        }
    }

    /// Serialize the code builders for all functions, and get code relocations with final offsets
    pub fn serialize_with_relocs<T: SerialBuffer>(
        &self,
        buffer: &mut T,
        relocations: &mut Vec<'a, RelocationEntry>,
    ) {
        let header_indices = write_section_header(buffer, SectionId::Code);
        buffer.encode_u32(self.code_builders.len() as u32);

        for code_builder in self.code_builders.iter() {
            code_builder.serialize_with_relocs(buffer, relocations, header_indices.body_index);
        }

        update_section_size(buffer, header_indices);
    }
}

/*******************************************************************
 *
 * Data section
 *
 *******************************************************************/

pub enum DataMode {
    /// A data segment that auto-loads into memory on instantiation
    Active { offset: ConstExpr },
    /// A data segment that can be loaded with the `memory.init` instruction
    Passive,
}

pub struct DataSegment<'a> {
    pub mode: DataMode,
    pub init: Vec<'a, u8>,
}

impl Serialize for DataSegment<'_> {
    fn serialize<T: SerialBuffer>(&self, buffer: &mut T) {
        match &self.mode {
            DataMode::Active { offset } => {
                buffer.append_u8(0);
                offset.serialize(buffer);
            }
            DataMode::Passive => {
                buffer.append_u8(1);
            }
        }

        self.init.serialize(buffer);
    }
}

pub struct DataSection<'a> {
    pub segments: Vec<'a, DataSegment<'a>>,
}

impl<'a> DataSection<'a> {
    fn is_empty(&self) -> bool {
        self.segments.is_empty() || self.segments.iter().all(|seg| seg.init.is_empty())
    }
}

impl Serialize for DataSection<'_> {
    fn serialize<T: SerialBuffer>(&self, buffer: &mut T) {
        if !self.is_empty() {
            serialize_vector_section(buffer, SectionId::Data, &self.segments);
        }
    }
}

/*******************************************************************
 *
 * Data Count section
 *
 * Pre-declares the number of segments in the Data section.
 * This helps the runtime to validate the module in a single pass.
 * The order of sections is DataCount -> Code -> Data
 *
 *******************************************************************/

struct DataCountSection {
    count: u32,
}

impl DataCountSection {
    fn new(data_section: &DataSection<'_>) -> Self {
        let count = data_section
            .segments
            .iter()
            .filter(|seg| !seg.init.is_empty())
            .count() as u32;
        DataCountSection { count }
    }
}

impl Serialize for DataCountSection {
    fn serialize<T: SerialBuffer>(&self, buffer: &mut T) {
        if self.count > 0 {
            let header_indices = write_section_header(buffer, SectionId::DataCount);
            buffer.encode_u32(self.count);
            update_section_size(buffer, header_indices);
        }
    }
}

/*******************************************************************
 *
 * Module
 *
 * https://webassembly.github.io/spec/core/binary/modules.html
 *
 *******************************************************************/

/// Helper struct to count non-empty sections.
/// Needed to generate linking data, which refers to target sections by index.
struct SectionCounter {
    buffer_size: usize,
    section_index: u32,
}

impl SectionCounter {
    /// Update the section counter if buffer size increased since last call
    #[inline]
    fn update<SB: SerialBuffer>(&mut self, buffer: &mut SB) {
        let new_size = buffer.size();
        if new_size > self.buffer_size {
            self.section_index += 1;
            self.buffer_size = new_size;
        }
    }

    #[inline]
    fn serialize_and_count<SB: SerialBuffer, S: Serialize>(
        &mut self,
        buffer: &mut SB,
        section: &S,
    ) {
        section.serialize(buffer);
        self.update(buffer);
    }
}

pub struct WasmModule<'a> {
    pub types: TypeSection<'a>,
    pub import: ImportSection<'a>,
    pub function: FunctionSection<'a>,
    /// Dummy placeholder for tables (used for function pointers and host references)
    pub table: (),
    pub memory: MemorySection,
    pub global: GlobalSection<'a>,
    pub export: ExportSection<'a>,
    /// Dummy placeholder for start function. In Roc, this would be part of the platform.
    pub start: (),
    /// Dummy placeholder for table elements. Roc does not use tables.
    pub element: (),
    pub code: CodeSection<'a>,
    pub data: DataSection<'a>,
    pub linking: LinkingSection<'a>,
    pub relocations: RelocationSection<'a>,
}

impl<'a> WasmModule<'a> {
    pub const WASM_VERSION: u32 = 1;

    /// Create entries in the Type and Function sections for a function signature
    pub fn add_function_signature(&mut self, signature: Signature<'a>) {
        let index = self.types.insert(signature);
        self.function.signature_indices.push(index);
    }

    #[allow(clippy::unit_arg)]
    pub fn serialize<T: SerialBuffer>(&mut self, buffer: &mut T) {
        buffer.append_u8(0);
        buffer.append_slice("asm".as_bytes());
        buffer.write_unencoded_u32(Self::WASM_VERSION);

        // Keep track of (non-empty) section indices for linking
        let mut counter = SectionCounter {
            buffer_size: buffer.size(),
            section_index: 0,
        };

        counter.serialize_and_count(buffer, &self.types);
        counter.serialize_and_count(buffer, &self.import);
        counter.serialize_and_count(buffer, &self.function);
        counter.serialize_and_count(buffer, &self.table);
        counter.serialize_and_count(buffer, &self.memory);
        counter.serialize_and_count(buffer, &self.global);
        counter.serialize_and_count(buffer, &self.export);
        counter.serialize_and_count(buffer, &self.start);
        counter.serialize_and_count(buffer, &self.element);

        // Data Count section forward-declares the size of the Data section
        // so that Code section can be validated in one pass
        let data_count_section = DataCountSection::new(&self.data);
        counter.serialize_and_count(buffer, &data_count_section);

        // Code section is the only one with relocations so we can stop counting
        let code_section_index = counter.section_index;
        self.code
            .serialize_with_relocs(buffer, &mut self.relocations.entries);

        self.data.serialize(buffer);

        self.linking.serialize(buffer);

        self.relocations.target_section_index = Some(code_section_index);
        self.relocations.serialize(buffer);
    }
}
