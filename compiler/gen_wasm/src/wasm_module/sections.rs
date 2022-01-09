use bumpalo::collections::vec::Vec;
use bumpalo::Bump;
use roc_collections::all::MutMap;

use super::linking::RelocationEntry;
use super::opcodes::OpCode;
use super::serialize::{decode_u32_or_panic, parse_u32_or_panic, SerialBuffer, Serialize};
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

const MAX_SIZE_ENCODED_U32: usize = 5;
const MAX_SIZE_SECTION_HEADER: usize = std::mem::size_of::<SectionId>() + 2 * MAX_SIZE_ENCODED_U32;

pub trait Section<'a>: Sized {
    const ID: SectionId;

    fn get_bytes(&self) -> &[u8];
    fn get_count(&self) -> u32;

    fn size(&self) -> usize {
        MAX_SIZE_SECTION_HEADER + self.get_bytes().len()
    }

    fn preload(arena: &'a Bump, module_bytes: &[u8], cursor: &mut usize) -> Self;
}

macro_rules! section_impl {
    ($structname: ident, $id: expr) => {
        impl<'a> Section<'a> for $structname<'a> {
            const ID: SectionId = $id;

            fn get_bytes(&self) -> &[u8] {
                &self.bytes
            }

            fn get_count(&self) -> u32 {
                self.count
            }

            fn preload(arena: &'a Bump, module_bytes: &[u8], cursor: &mut usize) -> Self {
                let (count, initial_bytes) = parse_section(Self::ID, module_bytes, cursor);
                let mut bytes = Vec::with_capacity_in(initial_bytes.len() * 2, arena);
                bytes.extend_from_slice(initial_bytes);
                $structname { bytes, count }
            }

            fn size(&self) -> usize {
                let id = 1;
                let encoded_length = 5;
                let encoded_count = 5;

                id + encoded_length + encoded_count + self.bytes.len()
            }
        }
    };
}

impl<'a, Sec> Serialize for Sec
where
    Sec: Section<'a>,
{
    fn serialize<B: SerialBuffer>(&self, buffer: &mut B) {
        if !self.get_bytes().is_empty() {
            let header_indices = write_section_header(buffer, Self::ID);
            buffer.encode_u32(self.get_count());
            buffer.append_slice(self.get_bytes());
            update_section_size(buffer, header_indices);
        }
    }
}

fn parse_section<'a>(id: SectionId, module_bytes: &'a [u8], cursor: &mut usize) -> (u32, &'a [u8]) {
    if module_bytes[*cursor] != id as u8 {
        return (0, &[]);
    }
    *cursor += 1;

    let section_size = parse_u32_or_panic(module_bytes, cursor);
    let count_start = *cursor;
    let count = parse_u32_or_panic(module_bytes, cursor);
    let body_start = *cursor;

    let next_section_start = count_start + section_size as usize;
    let body = &module_bytes[body_start..next_section_start];

    *cursor = next_section_start;

    (count, body)
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

    pub fn cache_offsets(&mut self) {
        self.offsets.clear();
        let mut i = 0;
        while i < self.bytes.len() {
            self.offsets.push(i);

            debug_assert!(self.bytes[i] == Signature::SEPARATOR);
            i += 1;

            let (n_params, n_params_size) = decode_u32_or_panic(&self.bytes[i..]);
            i += n_params_size; // skip over the array length that we just decoded
            i += n_params as usize; // skip over one byte per param type

            let n_return_values = self.bytes[i];
            i += 1 + n_return_values as usize;
        }
    }
}

impl<'a> Section<'a> for TypeSection<'a> {
    const ID: SectionId = SectionId::Type;

    fn get_bytes(&self) -> &[u8] {
        &self.bytes
    }
    fn get_count(&self) -> u32 {
        self.offsets.len() as u32
    }

    fn preload(arena: &'a Bump, module_bytes: &[u8], cursor: &mut usize) -> Self {
        let (count, initial_bytes) = parse_section(Self::ID, module_bytes, cursor);
        let mut bytes = Vec::with_capacity_in(initial_bytes.len() * 2, arena);
        bytes.extend_from_slice(initial_bytes);
        TypeSection {
            arena,
            bytes,
            offsets: Vec::with_capacity_in(2 * count as usize, arena),
        }
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
    pub count: u32,
    pub bytes: Vec<'a, u8>,
}

impl<'a> ImportSection<'a> {
    pub fn append(&mut self, import: Import) {
        import.serialize(&mut self.bytes);
        self.count += 1;
    }
}

section_impl!(ImportSection, SectionId::Import);

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
    pub fn add_sig(&mut self, sig_id: u32) {
        self.bytes.encode_u32(sig_id);
        self.count += 1;
    }
}

section_impl!(FunctionSection, SectionId::Function);

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
pub struct MemorySection<'a> {
    pub count: u32,
    pub bytes: Vec<'a, u8>,
}

impl<'a> MemorySection<'a> {
    pub const PAGE_SIZE: u32 = 64 * 1024;

    pub fn new(arena: &'a Bump, memory_bytes: u32) -> Self {
        if memory_bytes == 0 {
            MemorySection {
                count: 0,
                bytes: bumpalo::vec![in arena],
            }
        } else {
            let pages = (memory_bytes + Self::PAGE_SIZE - 1) / Self::PAGE_SIZE;
            let limits = Limits::Min(pages);

            let mut bytes = Vec::with_capacity_in(12, arena);
            limits.serialize(&mut bytes);

            MemorySection { count: 1, bytes }
        }
    }
}

section_impl!(MemorySection, SectionId::Memory);

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
    pub count: u32,
    pub bytes: Vec<'a, u8>,
}

impl<'a> GlobalSection<'a> {
    pub fn new(arena: &'a Bump, globals: &[Global]) -> Self {
        let capacity = 13 * globals.len();
        let mut bytes = Vec::with_capacity_in(capacity, arena);
        for global in globals {
            global.serialize(&mut bytes);
        }
        GlobalSection {
            count: globals.len() as u32,
            bytes,
        }
    }

    pub fn append(&mut self, global: Global) {
        global.serialize(&mut self.bytes);
        self.count += 1;
    }
}

section_impl!(GlobalSection, SectionId::Global);

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
pub struct Export<'a> {
    pub name: &'a [u8],
    pub ty: ExportType,
    pub index: u32,
}

impl Serialize for Export<'_> {
    fn serialize<T: SerialBuffer>(&self, buffer: &mut T) {
        self.name.serialize(buffer);
        buffer.append_u8(self.ty as u8);
        buffer.encode_u32(self.index);
    }
}

#[derive(Debug)]
pub struct ExportSection<'a> {
    pub count: u32,
    pub bytes: Vec<'a, u8>,
}

impl<'a> ExportSection<'a> {
    pub fn append(&mut self, export: Export) {
        export.serialize(&mut self.bytes);
        self.count += 1;
    }

    pub fn function_index_map(&self, arena: &'a Bump) -> MutMap<&'a [u8], u32> {
        let mut map = MutMap::default();

        let mut cursor = 0;
        while cursor < self.bytes.len() {
            let name_len = parse_u32_or_panic(&self.bytes, &mut cursor);
            let name_end = cursor + name_len as usize;
            let name_bytes = &self.bytes[cursor..name_end];
            let ty = self.bytes[name_end];

            cursor = name_end + 1;
            let index = parse_u32_or_panic(&self.bytes, &mut cursor);

            if ty == ExportType::Func as u8 {
                let name: &'a [u8] = arena.alloc_slice_clone(name_bytes);
                map.insert(name, index);
            }
        }

        map
    }
}

section_impl!(ExportSection, SectionId::Export);

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

    pub fn size(&self) -> usize {
        let builders_size: usize = self.code_builders.iter().map(|cb| cb.size()).sum();

        MAX_SIZE_SECTION_HEADER + self.preloaded_bytes.len() + builders_size
    }

    pub fn preload(arena: &'a Bump, module_bytes: &[u8], cursor: &mut usize) -> Self {
        let (preloaded_count, initial_bytes) = parse_section(SectionId::Code, module_bytes, cursor);
        let mut preloaded_bytes = Vec::with_capacity_in(initial_bytes.len() * 2, arena);
        preloaded_bytes.extend_from_slice(initial_bytes);
        CodeSection {
            preloaded_count,
            preloaded_bytes,
            code_builders: Vec::with_capacity_in(0, arena),
        }
    }
}

impl<'a> Serialize for CodeSection<'a> {
    fn serialize<T: SerialBuffer>(&self, buffer: &mut T) {
        let header_indices = write_section_header(buffer, SectionId::Code);
        buffer.encode_u32(self.preloaded_count + self.code_builders.len() as u32);

        buffer.append_slice(&self.preloaded_bytes);

        for code_builder in self.code_builders.iter() {
            code_builder.serialize(buffer);
        }

        update_section_size(buffer, header_indices);
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
    count: u32,
    bytes: Vec<'a, u8>,
}

impl<'a> DataSection<'a> {
    pub fn append_segment(&mut self, segment: DataSegment<'a>) -> u32 {
        let index = self.count;
        self.count += 1;
        segment.serialize(&mut self.bytes);
        index
    }
}

section_impl!(DataSection, SectionId::Data);

/*******************************************************************
 *
 * Module
 *
 * https://webassembly.github.io/spec/core/binary/modules.html
 *
 *******************************************************************/

/// A Wasm module section that we don't use for Roc code,
/// but may be present in a preloaded binary
#[derive(Debug, Default)]
pub struct OpaqueSection<'a> {
    bytes: &'a [u8],
}

impl<'a> OpaqueSection<'a> {
    pub fn size(&self) -> usize {
        self.bytes.len()
    }

    pub fn preload(
        id: SectionId,
        arena: &'a Bump,
        module_bytes: &[u8],
        cursor: &mut usize,
    ) -> Self {
        let bytes: &[u8];

        if module_bytes[*cursor] != id as u8 {
            bytes = &[];
        } else {
            let section_start = *cursor;
            *cursor += 1;
            let section_size = parse_u32_or_panic(module_bytes, cursor);
            let next_section_start = *cursor + section_size as usize;
            bytes = &module_bytes[section_start..next_section_start];
            *cursor = next_section_start;
        };

        OpaqueSection {
            bytes: arena.alloc_slice_clone(bytes),
        }
    }
}

impl Serialize for OpaqueSection<'_> {
    fn serialize<T: SerialBuffer>(&self, buffer: &mut T) {
        buffer.append_slice(self.bytes);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use bumpalo::{self, collections::Vec, Bump};

    fn test_assert_types_preload<'a>(arena: &'a Bump, original: &TypeSection<'a>) {
        // Serialize the Type section that we built from Roc code
        let mut original_serialized = Vec::with_capacity_in(6 + original.bytes.len(), arena);
        original.serialize(&mut original_serialized);

        // Reconstruct a new TypeSection by "pre-loading" the bytes of the original
        let mut cursor = 0;
        let mut preloaded = TypeSection::preload(arena, &original_serialized, &mut cursor);
        preloaded.cache_offsets();

        debug_assert_eq!(original.offsets, preloaded.offsets);
        debug_assert_eq!(original.bytes, preloaded.bytes);
    }

    #[test]
    fn test_type_section() {
        use ValueType::*;
        let arena = &Bump::new();
        let signatures = [
            Signature {
                param_types: bumpalo::vec![in arena],
                ret_type: None,
            },
            Signature {
                param_types: bumpalo::vec![in arena; I32, I64, F32, F64],
                ret_type: None,
            },
            Signature {
                param_types: bumpalo::vec![in arena; I32, I32, I32],
                ret_type: Some(I32),
            },
        ];
        let capacity = signatures.len();
        let mut section = TypeSection {
            arena,
            bytes: Vec::with_capacity_in(capacity * 4, arena),
            offsets: Vec::with_capacity_in(capacity, arena),
        };

        for sig in signatures {
            section.insert(sig);
        }
        test_assert_types_preload(arena, &section);
    }
}
