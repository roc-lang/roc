use bumpalo::collections::vec::Vec;
use bumpalo::Bump;

use super::linking::{
    IndexRelocType, LinkingSection, RelocationEntry, RelocationSection, SymInfo, WasmObjectSymbol,
};
use super::opcodes::OpCode;
use super::serialize::{decode_u32_or_panic, SerialBuffer, Serialize};
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
    /// DataCount section is unused. Only needed for single-pass validation of
    /// memory.init and data.drop, which we don't use
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

/// Serialize a section that is stored as bytes and a count
fn serialize_bytes_section<B: SerialBuffer>(
    buffer: &mut B,
    section_id: SectionId,
    count: u32,
    bytes: &[u8],
) {
    if !bytes.is_empty() {
        let header_indices = write_section_header(buffer, section_id);
        buffer.encode_u32(count);
        buffer.append_slice(bytes);
        update_section_size(buffer, header_indices);
    }
}

/*******************************************************************
 *
 * Type section
 * Deduplicated list of function type signatures
 *
 *******************************************************************/

#[derive(PartialEq, Eq, Debug)]
pub struct Signature<'a> {
    pub param_types: Vec<'a, ValueType>,
    pub ret_type: Option<ValueType>,
}

impl Signature<'_> {
    pub const SEPARATOR: u8 = 0x60;
}

impl<'a> Serialize for Signature<'a> {
    fn serialize<T: SerialBuffer>(&self, buffer: &mut T) {
        buffer.append_u8(Self::SEPARATOR);
        self.param_types.serialize(buffer);
        self.ret_type.serialize(buffer);
    }
}

#[derive(Debug)]
pub struct TypeSection<'a> {
    /// Private. See WasmModule::add_function_signature
    arena: &'a Bump,
    bytes: Vec<'a, u8>,
    offsets: Vec<'a, usize>,
}

impl<'a> TypeSection<'a> {
    pub fn new(arena: &'a Bump, capacity: usize) -> Self {
        TypeSection {
            arena,
            bytes: Vec::with_capacity_in(capacity * 4, arena),
            offsets: Vec::with_capacity_in(capacity, arena),
        }
    }

    /// Find a matching signature or insert a new one. Return the index.
    pub fn insert(&mut self, signature: Signature<'a>) -> u32 {
        let mut sig_bytes = Vec::with_capacity_in(signature.param_types.len() + 4, self.arena);
        signature.serialize(&mut sig_bytes);

        let sig_len = sig_bytes.len();
        let bytes_len = self.bytes.len();

        for (i, offset) in self.offsets.iter().enumerate() {
            let end = offset + sig_len;
            if end > bytes_len {
                break;
            }
            if &self.bytes[*offset..end] == sig_bytes.as_slice() {
                return i as u32;
            }
        }

        let sig_id = self.offsets.len();
        self.offsets.push(bytes_len);
        self.bytes.extend_from_slice(&sig_bytes);

        sig_id as u32
    }

    pub fn preload(arena: &'a Bump, section_body: &[u8]) -> Self {
        if section_body.is_empty() {
            return TypeSection {
                arena,
                bytes: Vec::new_in(arena),
                offsets: Vec::new_in(arena),
            };
        }

        let (count, content_offset) = decode_u32_or_panic(section_body);

        let mut bytes = Vec::with_capacity_in(section_body.len() * 2, arena);
        bytes.extend_from_slice(&section_body[content_offset..]);

        let mut offsets = Vec::with_capacity_in((count * 2) as usize, arena);

        let mut i = 0;
        while i < bytes.len() {
            offsets.push(i);

            let sep = bytes[i];
            debug_assert!(sep == Signature::SEPARATOR);
            i += 1;

            let (n_params, n_params_size) = decode_u32_or_panic(&bytes[i..]);
            i += n_params_size; // skip over the array length that we just decoded
            i += n_params as usize; // skip over one byte per param type

            let n_return_values = bytes[i];
            i += 1 + n_return_values as usize;
        }

        TypeSection {
            arena,
            bytes,
            offsets,
        }
    }
}

impl<'a> Serialize for TypeSection<'a> {
    fn serialize<T: SerialBuffer>(&self, buffer: &mut T) {
        serialize_bytes_section(
            buffer,
            SectionId::Type,
            self.offsets.len() as u32,
            &self.bytes,
        );
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

#[derive(Debug)]
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

#[derive(Debug)]
pub enum ImportDesc {
    Func { signature_index: u32 },
    Table { ty: TableType },
    Mem { limits: Limits },
    Global { ty: GlobalType },
}

#[derive(Debug)]
pub struct Import {
    pub module: &'static str,
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

#[derive(Debug)]
pub struct ImportSection<'a> {
    pub entries: Vec<'a, Import>,
}

impl<'a> ImportSection<'a> {
    pub fn new(arena: &'a Bump) -> Self {
        ImportSection {
            entries: bumpalo::vec![in arena],
        }
    }

    pub fn function_count(&self) -> usize {
        self.entries
            .iter()
            .filter(|import| matches!(import.description, ImportDesc::Func { .. }))
            .count()
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

#[derive(Debug)]
pub struct FunctionSection<'a> {
    pub count: u32,
    pub bytes: Vec<'a, u8>,
}

impl<'a> FunctionSection<'a> {
    pub fn new(arena: &'a Bump, capacity: usize) -> Self {
        FunctionSection {
            count: 0,
            bytes: Vec::with_capacity_in(capacity, arena),
        }
    }

    fn add_sig(&mut self, sig_id: u32) {
        self.bytes.encode_u32(sig_id);
        self.count += 1;
    }
}
impl<'a> Serialize for FunctionSection<'a> {
    fn serialize<T: SerialBuffer>(&self, buffer: &mut T) {
        serialize_bytes_section(buffer, SectionId::Function, self.count, &self.bytes);
    }
}

/*******************************************************************
 *
 * Memory section
 *
 *******************************************************************/

#[derive(Debug)]
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

#[derive(Debug)]
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

#[derive(Debug)]
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
#[derive(Debug)]
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
                buffer.append_u8(OpCode::I32CONST as u8);
                buffer.encode_i32(*x);
            }
            ConstExpr::I64(x) => {
                buffer.append_u8(OpCode::I64CONST as u8);
                buffer.encode_i64(*x);
            }
            ConstExpr::F32(x) => {
                buffer.append_u8(OpCode::F32CONST as u8);
                buffer.encode_f32(*x);
            }
            ConstExpr::F64(x) => {
                buffer.append_u8(OpCode::F64CONST as u8);
                buffer.encode_f64(*x);
            }
        }
        buffer.append_u8(OpCode::END as u8);
    }
}

#[derive(Debug)]
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

#[derive(Debug)]
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

#[derive(Debug)]
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

#[derive(Debug)]
pub struct ExportSection<'a> {
    pub entries: Vec<'a, Export>,
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
    pub preloaded_count: u32,
    pub preloaded_bytes: Vec<'a, u8>,
    pub code_builders: Vec<'a, CodeBuilder<'a>>,
}

impl<'a> CodeSection<'a> {
    /// Serialize the code builders for all functions, and get code relocations with final offsets
    pub fn serialize_with_relocs<T: SerialBuffer>(
        &self,
        buffer: &mut T,
        relocations: &mut Vec<'a, RelocationEntry>,
    ) -> usize {
        let header_indices = write_section_header(buffer, SectionId::Code);
        buffer.encode_u32(self.preloaded_count + self.code_builders.len() as u32);

        buffer.append_slice(&self.preloaded_bytes);

        for code_builder in self.code_builders.iter() {
            code_builder.serialize_with_relocs(buffer, relocations, header_indices.body_index);
        }

        let code_section_body_index = header_indices.body_index;
        update_section_size(buffer, header_indices);
        code_section_body_index
    }
}

/*******************************************************************
 *
 * Data section
 *
 *******************************************************************/

#[derive(Debug)]
pub enum DataMode {
    /// A data segment that auto-loads into memory on instantiation
    Active { offset: ConstExpr },
    /// A data segment that can be loaded with the `memory.init` instruction
    Passive,
}

impl DataMode {
    pub fn active_at(offset: u32) -> Self {
        DataMode::Active {
            offset: ConstExpr::I32(offset as i32),
        }
    }
}

#[derive(Debug)]
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

#[derive(Debug)]
pub struct DataSection<'a> {
    segment_count: u32,
    bytes: Vec<'a, u8>,
}

impl<'a> DataSection<'a> {
    pub fn new(arena: &'a Bump) -> Self {
        DataSection {
            segment_count: 0,
            bytes: bumpalo::vec![in arena],
        }
    }

    pub fn append_segment(&mut self, segment: DataSegment<'a>) -> u32 {
        let index = self.segment_count;
        self.segment_count += 1;
        segment.serialize(&mut self.bytes);
        index
    }
}

impl Serialize for DataSection<'_> {
    fn serialize<T: SerialBuffer>(&self, buffer: &mut T) {
        if !self.bytes.is_empty() {
            serialize_bytes_section(buffer, SectionId::Data, self.segment_count, &self.bytes);
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

/// A Wasm module section that we don't use for Roc code,
/// but may be present in a preloaded binary
#[derive(Debug, Default)]
pub struct OpaqueSection<'a> {
    bytes: &'a [u8],
}

impl<'a> OpaqueSection<'a> {
    pub fn new(bytes: &'a [u8]) -> Self {
        Self { bytes }
    }
}

impl Serialize for OpaqueSection<'_> {
    fn serialize<T: SerialBuffer>(&self, buffer: &mut T) {
        if !self.bytes.is_empty() {
            buffer.append_slice(self.bytes);
        }
    }
}

#[derive(Debug)]
pub struct WasmModule<'a> {
    pub types: TypeSection<'a>,
    pub import: ImportSection<'a>,
    pub function: FunctionSection<'a>,
    pub table: OpaqueSection<'a>,
    pub memory: MemorySection,
    pub global: GlobalSection<'a>,
    pub export: ExportSection<'a>,
    pub start: OpaqueSection<'a>,
    pub element: OpaqueSection<'a>,
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
        self.function.add_sig(index);
    }

    /// Serialize the module to bytes
    /// (Mutates some data related to linking)
    pub fn serialize_mut<T: SerialBuffer>(&mut self, buffer: &mut T) {
        buffer.append_u8(0);
        buffer.append_slice("asm".as_bytes());
        buffer.write_unencoded_u32(Self::WASM_VERSION);

        // Keep track of (non-empty) section indices for linking
        let mut counter = SectionCounter {
            buffer_size: buffer.size(),
            section_index: 0,
        };

        // If we have imports, then references to other functions need to be re-indexed.
        // Modify exports before serializing them, since we don't have linker data for them
        let n_imported_fns = self.import.function_count() as u32;
        if n_imported_fns > 0 {
            self.finalize_exported_fn_indices(n_imported_fns);
        }

        counter.serialize_and_count(buffer, &self.types);
        counter.serialize_and_count(buffer, &self.import);
        counter.serialize_and_count(buffer, &self.function);
        counter.serialize_and_count(buffer, &self.table);
        counter.serialize_and_count(buffer, &self.memory);
        counter.serialize_and_count(buffer, &self.global);
        counter.serialize_and_count(buffer, &self.export);
        counter.serialize_and_count(buffer, &self.start);
        counter.serialize_and_count(buffer, &self.element);

        // Code section is the only one with relocations so we can stop counting
        let code_section_index = counter.section_index;
        let code_section_body_index = self
            .code
            .serialize_with_relocs(buffer, &mut self.relocations.entries);

        // If we have imports, references to other functions need to be re-indexed.
        // Simplest to do after serialization, using linker data
        if n_imported_fns > 0 {
            self.finalize_code_fn_indices(buffer, code_section_body_index, n_imported_fns);
        }

        self.data.serialize(buffer);

        self.linking.serialize(buffer);

        self.relocations.target_section_index = Some(code_section_index);
        self.relocations.serialize(buffer);
    }

    /// Shift indices of exported functions to make room for imported functions,
    /// which come first in the function index space.
    /// Must be called after traversing the full IR, but before export section is serialized.
    fn finalize_exported_fn_indices(&mut self, n_imported_fns: u32) {
        for export in self.export.entries.iter_mut() {
            if export.ty == ExportType::Func {
                export.index += n_imported_fns;
            }
        }
    }

    /// Re-index internally-defined functions to make room for imported functions.
    /// Imported functions come first in the index space, but we didn't know how many we needed until now.
    /// We do this after serializing the code section, since we have linker data that is literally
    /// *designed* for changing function indices in serialized code!
    fn finalize_code_fn_indices<T: SerialBuffer>(
        &mut self,
        buffer: &mut T,
        code_section_body_index: usize,
        n_imported_fns: u32,
    ) {
        // Lookup vector of symbol index to new function index
        let mut new_index_lookup = std::vec::Vec::with_capacity(self.linking.symbol_table.len());

        // Modify symbol table entries and fill the lookup vector
        for sym_info in self.linking.symbol_table.iter_mut() {
            match sym_info {
                SymInfo::Function(WasmObjectSymbol::Defined { index, .. }) => {
                    let new_fn_index = *index + n_imported_fns;
                    *index = new_fn_index;
                    new_index_lookup.push(new_fn_index);
                }
                SymInfo::Function(WasmObjectSymbol::Imported { index, .. }) => {
                    new_index_lookup.push(*index);
                }
                _ => {
                    // Symbol is not a function, so we won't look it up. Use a dummy value.
                    new_index_lookup.push(u32::MAX);
                }
            }
        }

        // Modify call instructions, using linker data
        for reloc in &self.relocations.entries {
            if let RelocationEntry::Index {
                type_id: IndexRelocType::FunctionIndexLeb,
                offset,
                symbol_index,
            } = reloc
            {
                let new_fn_index = new_index_lookup[*symbol_index as usize];
                let buffer_index = code_section_body_index + (*offset as usize);
                buffer.overwrite_padded_u32(buffer_index, new_fn_index);
            }
        }
    }
}

/// Assertion to run on the generated Wasm module in every test
pub fn test_assert_preload<'a>(arena: &'a Bump, wasm_module: &WasmModule<'a>) {
    test_assert_types_preload(arena, &wasm_module.types);
}

fn test_assert_types_preload<'a>(arena: &'a Bump, original: &TypeSection<'a>) {
    // Serialize the Type section that we built from Roc code
    let mut original_serialized = Vec::with_capacity_in(original.bytes.len() + 10, arena);
    original.serialize(&mut original_serialized);

    debug_assert!(original_serialized[0] == SectionId::Type as u8);

    // Reconstruct a new TypeSection by "pre-loading" the bytes of the original!
    let body = &original_serialized[6..];
    let preloaded = TypeSection::preload(arena, body);

    let mut preloaded_serialized = Vec::with_capacity_in(original.bytes.len() + 10, arena);
    preloaded.serialize(&mut preloaded_serialized);

    debug_assert_eq!(original_serialized, preloaded_serialized);
}
