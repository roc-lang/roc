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
    pub fn new(arena: &'a Bump) -> Self {
        TypeSection {
            signatures: Vec::with_capacity_in(8, arena),
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
    pub fn new(arena: &'a Bump) -> Self {
        FunctionSection {
            signature_indices: Vec::with_capacity_in(8, arena),
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

pub enum GlobalInitValue {
    I32(i32),
    I64(i64),
    F32(f32),
    F64(f64),
}

pub struct Global {
    pub ty: GlobalType,
    pub init_value: GlobalInitValue,
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
        buffer.append_u8(opcodes::END);
    }
}

pub struct GlobalSection<'a> {
    pub entries: Vec<'a, Global>,
}

impl<'a> GlobalSection<'a> {
    pub fn new(arena: &'a Bump) -> Self {
        GlobalSection {
            entries: Vec::with_capacity_in(1, arena),
        }
    }
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
    pub fn serialize_mut<T: SerialBuffer>(
        &mut self,
        buffer: &mut T,
        relocations: &mut Vec<'a, RelocationEntry>,
    ) {
        let header_indices = write_section_header(buffer, SectionId::Code);
        buffer.encode_u32(self.code_builders.len() as u32);

        for code_builder in self.code_builders.iter_mut() {
            code_builder.serialize_with_relocs(buffer, relocations, header_indices.body_index);
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
    /// Dummy placeholder for data count section, not yet implemented
    pub data_count: (),
    pub code: CodeSection<'a>,
    /// Dummy placeholder for data section, not yet implemented
    pub data: (),
    pub linking: LinkingSection<'a>,
    pub reloc_code: RelocationSection<'a>,
    pub reloc_data: RelocationSection<'a>,
}

impl<'a> WasmModule<'a> {
    pub const WASM_VERSION: u32 = 1;

    pub fn new(arena: &'a Bump) -> Self {
        WasmModule {
            types: TypeSection::new(arena),
            import: ImportSection::new(arena),
            function: FunctionSection::new(arena),
            table: (), // Unused in Roc (mainly for function pointers)
            memory: MemorySection::new(1024 * 1024),
            global: GlobalSection::new(arena),
            export: ExportSection::new(arena),
            start: (),      // Entry function. In Roc this would be part of the platform.
            element: (),    // Unused in Roc (related to table section)
            data_count: (), // TODO, related to data section
            code: CodeSection::new(arena),
            data: (), // TODO: program constants (e.g. string literals)
            linking: LinkingSection::new(arena),
            reloc_code: RelocationSection::new(arena, "reloc.CODE"),
            reloc_data: RelocationSection::new(arena, "reloc.DATA"),
        }
    }

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

        self.reloc_code.target_section_index = Some(index);
        self.code
            .serialize_mut(buffer, &mut self.reloc_code.entries);
        maybe_increment_section(buffer.size(), &mut prev_size, &mut index);

        self.data.serialize(buffer);
        self.reloc_data.target_section_index = Some(index);

        self.linking.serialize(buffer);
        self.reloc_code.serialize(buffer);
        self.reloc_data.serialize(buffer);
    }
}

fn maybe_increment_section(size: usize, prev_size: &mut usize, index: &mut u32) {
    if size > *prev_size {
        *index += 1;
        *prev_size = size;
    }
}
