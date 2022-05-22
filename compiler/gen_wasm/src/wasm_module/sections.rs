use std::fmt::Debug;

use bumpalo::collections::vec::Vec;
use bumpalo::Bump;
use roc_collections::all::MutMap;
use roc_error_macros::internal_error;

use super::dead_code::{
    copy_preloads_shrinking_dead_fns, parse_preloads_call_graph, trace_call_graph,
    PreloadsCallGraph,
};
use super::linking::RelocationEntry;
use super::opcodes::OpCode;
use super::parse::{Parse, SkipBytes};
use super::serialize::{SerialBuffer, Serialize, MAX_SIZE_ENCODED_U32};
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
    ($structname: ident, $id: expr, $from_count_and_bytes: expr) => {
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
                $from_count_and_bytes(count, bytes)
            }

            fn size(&self) -> usize {
                section_size(self.get_bytes())
            }
        }
    };

    ($structname: ident, $id: expr) => {
        section_impl!($structname, $id, |count, bytes| $structname {
            bytes,
            count
        });
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

fn section_size(bytes: &[u8]) -> usize {
    let id = 1;
    let encoded_length = MAX_SIZE_ENCODED_U32;
    let encoded_count = MAX_SIZE_ENCODED_U32;

    id + encoded_length + encoded_count + bytes.len()
}

fn parse_section<'a>(id: SectionId, module_bytes: &'a [u8], cursor: &mut usize) -> (u32, &'a [u8]) {
    if (*cursor >= module_bytes.len()) || (module_bytes[*cursor] != id as u8) {
        return (0, &[]);
    }
    *cursor += 1;

    let section_size = u32::parse((), module_bytes, cursor).unwrap();
    let count_start = *cursor;
    let count = u32::parse((), module_bytes, cursor).unwrap();
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

    pub fn parse_offsets(&mut self) {
        self.offsets.clear();

        let mut i = 0;
        while i < self.bytes.len() {
            self.offsets.push(i);

            debug_assert!(self.bytes[i] == Signature::SEPARATOR);
            i += 1;

            let n_params = u32::parse((), &self.bytes, &mut i).unwrap();
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

#[repr(u8)]
#[derive(Debug)]
enum ImportTypeId {
    Func = 0,
    Table = 1,
    Mem = 2,
    Global = 3,
}

impl From<u8> for ImportTypeId {
    fn from(x: u8) -> Self {
        match x {
            0 => Self::Func,
            1 => Self::Table,
            2 => Self::Mem,
            3 => Self::Global,
            _ => internal_error!(
                "Invalid ImportTypeId {} in platform/builtins object file",
                x
            ),
        }
    }
}

impl Serialize for Import {
    fn serialize<T: SerialBuffer>(&self, buffer: &mut T) {
        self.module.serialize(buffer);
        self.name.serialize(buffer);
        match &self.description {
            ImportDesc::Func { signature_index } => {
                buffer.append_u8(ImportTypeId::Func as u8);
                buffer.encode_u32(*signature_index);
            }
            ImportDesc::Table { ty } => {
                buffer.append_u8(ImportTypeId::Table as u8);
                ty.serialize(buffer);
            }
            ImportDesc::Mem { limits } => {
                buffer.append_u8(ImportTypeId::Mem as u8);
                limits.serialize(buffer);
            }
            ImportDesc::Global { ty } => {
                buffer.append_u8(ImportTypeId::Global as u8);
                ty.serialize(buffer);
            }
        }
    }
}

#[derive(Debug)]
pub struct ImportSection<'a> {
    pub count: u32,
    pub function_count: u32,
    pub bytes: Vec<'a, u8>,
}

impl<'a> ImportSection<'a> {
    pub fn append(&mut self, import: Import) {
        import.serialize(&mut self.bytes);
        self.count += 1;
    }

    pub fn parse(&mut self, arena: &'a Bump) -> Result<Vec<'a, u32>, String> {
        let mut fn_signatures = bumpalo::vec![in arena];
        let mut cursor = 0;
        while cursor < self.bytes.len() {
            String::skip_bytes(&self.bytes, &mut cursor)?; // import namespace
            String::skip_bytes(&self.bytes, &mut cursor)?; // import name

            let type_id = ImportTypeId::from(self.bytes[cursor]);
            cursor += 1;

            match type_id {
                ImportTypeId::Func => {
                    let sig = u32::parse((), &self.bytes, &mut cursor)?;
                    fn_signatures.push(sig);
                }
                ImportTypeId::Table => {
                    TableType::skip_bytes(&self.bytes, &mut cursor)?;
                }
                ImportTypeId::Mem => {
                    Limits::skip_bytes(&self.bytes, &mut cursor)?;
                }
                ImportTypeId::Global => {
                    GlobalType::skip_bytes(&self.bytes, &mut cursor)?;
                }
            }
        }

        self.function_count = fn_signatures.len() as u32;
        Ok(fn_signatures)
    }

    pub fn from_count_and_bytes(count: u32, bytes: Vec<'a, u8>) -> Self {
        ImportSection {
            bytes,
            count,
            function_count: 0,
        }
    }
}

section_impl!(
    ImportSection,
    SectionId::Import,
    ImportSection::from_count_and_bytes
);

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

    pub fn parse(&self, arena: &'a Bump) -> Vec<'a, u32> {
        let count = self.count as usize;
        let mut signatures = Vec::with_capacity_in(count, arena);
        let mut cursor = 0;
        for _ in 0..count {
            signatures.push(u32::parse((), &self.bytes, &mut cursor).unwrap());
        }
        signatures
    }
}

section_impl!(FunctionSection, SectionId::Function);

/*******************************************************************
 *
 * Table section
 *
 * Defines tables used for indirect references to host memory.
 * The table *contents* are elsewhere, in the ElementSection.
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

impl SkipBytes for TableType {
    fn skip_bytes(bytes: &[u8], cursor: &mut usize) -> Result<(), String> {
        u8::skip_bytes(bytes, cursor)?;
        Limits::skip_bytes(bytes, cursor)?;
        Ok(())
    }
}

#[derive(Debug)]
pub struct TableSection {
    pub function_table: TableType,
}

impl TableSection {
    const ID: SectionId = SectionId::Table;

    pub fn preload(module_bytes: &[u8], mod_cursor: &mut usize) -> Self {
        let (count, section_bytes) = parse_section(Self::ID, module_bytes, mod_cursor);

        match count {
            0 => TableSection {
                function_table: TableType {
                    ref_type: RefType::Func,
                    limits: Limits::MinMax(0, 0),
                },
            },
            1 => {
                if section_bytes[0] != RefType::Func as u8 {
                    internal_error!("Only funcref tables are supported")
                }
                let mut section_cursor = 1;
                let limits = Limits::parse(section_bytes, &mut section_cursor);

                TableSection {
                    function_table: TableType {
                        ref_type: RefType::Func,
                        limits,
                    },
                }
            }
            _ => internal_error!("Multiple tables are not supported"),
        }
    }

    pub fn size(&self) -> usize {
        let section_id_bytes = 1;
        let section_length_bytes = 1;
        let num_tables_bytes = 1;
        let ref_type_bytes = 1;
        let limits_bytes = match self.function_table.limits {
            Limits::Min(_) => MAX_SIZE_ENCODED_U32,
            Limits::MinMax(..) => 2 * MAX_SIZE_ENCODED_U32,
        };

        section_id_bytes + section_length_bytes + num_tables_bytes + ref_type_bytes + limits_bytes
    }
}

impl Serialize for TableSection {
    fn serialize<T: SerialBuffer>(&self, buffer: &mut T) {
        let header_indices = write_section_header(buffer, Self::ID);

        let num_tables: u32 = 1;
        num_tables.serialize(buffer);
        self.function_table.serialize(buffer);

        update_section_size(buffer, header_indices);
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

#[repr(u8)]
enum LimitsId {
    Min = 0,
    MinMax = 1,
}

impl Serialize for Limits {
    fn serialize<T: SerialBuffer>(&self, buffer: &mut T) {
        match self {
            Self::Min(min) => {
                buffer.append_u8(LimitsId::Min as u8);
                buffer.encode_u32(*min);
            }
            Self::MinMax(min, max) => {
                buffer.append_u8(LimitsId::MinMax as u8);
                buffer.encode_u32(*min);
                buffer.encode_u32(*max);
            }
        }
    }
}

impl SkipBytes for Limits {
    fn skip_bytes(bytes: &[u8], cursor: &mut usize) -> Result<(), String> {
        let variant_id = bytes[*cursor];
        u8::skip_bytes(bytes, cursor)?; // advance past the variant byte
        u32::skip_bytes(bytes, cursor)?; // skip "min"
        if variant_id == LimitsId::MinMax as u8 {
            u32::skip_bytes(bytes, cursor)?; // skip "max"
        }
        Ok(())
    }
}

impl Limits {
    fn parse(bytes: &[u8], cursor: &mut usize) -> Self {
        let variant_id = bytes[*cursor];
        *cursor += 1;

        let min = u32::parse((), bytes, cursor).unwrap();
        if variant_id == LimitsId::MinMax as u8 {
            let max = u32::parse((), bytes, cursor).unwrap();
            Limits::MinMax(min, max)
        } else {
            Limits::Min(min)
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

impl SkipBytes for GlobalType {
    fn skip_bytes(_bytes: &[u8], cursor: &mut usize) -> Result<(), String> {
        *cursor += 2;
        Ok(())
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

impl ConstExpr {
    fn parse_u32(bytes: &[u8], cursor: &mut usize) -> Result<u32, String> {
        let err = Err("Invalid ConstExpr. Expected i32.".into());

        if bytes[*cursor] != OpCode::I32CONST as u8 {
            return err;
        }
        *cursor += 1;

        let value = u32::parse((), bytes, cursor).unwrap();

        if bytes[*cursor] != OpCode::END as u8 {
            return err;
        }
        *cursor += 1;

        Ok(value)
    }

    fn unwrap_i32(&self) -> i32 {
        match self {
            Self::I32(x) => *x,
            _ => internal_error!("Expected ConstExpr to be I32"),
        }
    }
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

impl SkipBytes for ConstExpr {
    fn skip_bytes(bytes: &[u8], cursor: &mut usize) -> Result<(), String> {
        while bytes[*cursor] != OpCode::END as u8 {
            OpCode::skip_bytes(bytes, cursor)?;
        }
        *cursor += 1;
        Ok(())
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
    pub fn parse_u32_at_index(&self, index: u32) -> Result<u32, String> {
        let mut cursor = 0;
        for _ in 0..index {
            GlobalType::skip_bytes(&self.bytes, &mut cursor)?;
            ConstExpr::skip_bytes(&self.bytes, &mut cursor)?;
        }
        GlobalType::skip_bytes(&self.bytes, &mut cursor)?;
        ConstExpr::parse_u32(&self.bytes, &mut cursor)
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

impl From<u8> for ExportType {
    fn from(x: u8) -> Self {
        match x {
            0 => Self::Func,
            1 => Self::Table,
            2 => Self::Mem,
            3 => Self::Global,
            _ => internal_error!("invalid ExportType {:2x?}", x),
        }
    }
}

#[derive(Debug)]
pub struct Export<'a> {
    pub name: &'a [u8],
    pub ty: ExportType,
    pub index: u32,
}

impl<'a> Export<'a> {
    fn parse(arena: &'a Bump, bytes: &[u8], cursor: &mut usize) -> Self {
        let name = <&'a [u8]>::parse(arena, bytes, cursor).unwrap();

        let ty = ExportType::from(bytes[*cursor]);
        *cursor += 1;

        let index = u32::parse((), bytes, cursor).unwrap();

        Export { name, ty, index }
    }
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
    pub exports: Vec<'a, Export<'a>>,
}

impl<'a> ExportSection<'a> {
    const ID: SectionId = SectionId::Export;

    pub fn append(&mut self, export: Export<'a>) {
        self.exports.push(export);
    }

    pub fn size(&self) -> usize {
        self.exports
            .iter()
            .map(|ex| ex.name.len() + 1 + MAX_SIZE_ENCODED_U32)
            .sum()
    }

    /// Preload from object file.
    pub fn preload(arena: &'a Bump, module_bytes: &[u8], cursor: &mut usize) -> Self {
        let (num_exports, body_bytes) = parse_section(Self::ID, module_bytes, cursor);

        let mut export_section = ExportSection {
            exports: Vec::with_capacity_in(num_exports as usize, arena),
        };

        let mut body_cursor = 0;
        while body_cursor < body_bytes.len() {
            let export = Export::parse(arena, body_bytes, &mut body_cursor);
            export_section.exports.push(export);
        }

        export_section
    }
}

impl<'a> Serialize for ExportSection<'a> {
    fn serialize<T: SerialBuffer>(&self, buffer: &mut T) {
        if !self.exports.is_empty() {
            let header_indices = write_section_header(buffer, Self::ID);
            self.exports.serialize(buffer);
            update_section_size(buffer, header_indices);
        }
    }
}

/*******************************************************************
 *
 * Element section
 *
 * Elements are entries in tables (see Table section)
 * For example, Wasm uses a function table instead of function pointers,
 * and each entry in that function table is an element.
 * The call_indirect instruction uses element indices to refer to functions.
 * This section therefore enumerates all indirectly-called functions.
 *
 *******************************************************************/

#[repr(u8)]
enum ElementSegmentFormatId {
    /// Currently only supporting the original Wasm MVP format since it's the only one in wide use.
    /// There are newer formats for other table types, with complex encodings to preserve backward compatibility
    /// (Already going down the same path as x86!)
    ActiveImplicitTableIndex = 0x00,
}

/// A Segment initialises a subrange of elements in a table. Normally there's just one Segment.
#[derive(Debug)]
struct ElementSegment<'a> {
    offset: ConstExpr, // The starting table index for the segment
    fn_indices: Vec<'a, u32>,
}

impl<'a> ElementSegment<'a> {
    fn parse(arena: &'a Bump, bytes: &[u8], cursor: &mut usize) -> Self {
        // In practice we only need the original MVP format
        let format_id = bytes[*cursor];
        debug_assert!(format_id == ElementSegmentFormatId::ActiveImplicitTableIndex as u8);
        *cursor += 1;

        // The table index offset is encoded as a ConstExpr, but only I32 makes sense
        let const_expr_opcode = bytes[*cursor];
        debug_assert!(const_expr_opcode == OpCode::I32CONST as u8);
        *cursor += 1;
        let offset = u32::parse((), bytes, cursor).unwrap();
        debug_assert!(bytes[*cursor] == OpCode::END as u8);
        *cursor += 1;

        let num_elems = u32::parse((), bytes, cursor).unwrap();
        let mut fn_indices = Vec::with_capacity_in(num_elems as usize, arena);
        for _ in 0..num_elems {
            let fn_idx = u32::parse((), bytes, cursor).unwrap();

            fn_indices.push(fn_idx);
        }

        ElementSegment {
            offset: ConstExpr::I32(offset as i32),
            fn_indices,
        }
    }

    fn size(&self) -> usize {
        let variant_id = 1;
        let constexpr_opcode = 1;
        let constexpr_value = MAX_SIZE_ENCODED_U32;
        let vec_len = MAX_SIZE_ENCODED_U32;
        let vec_contents = MAX_SIZE_ENCODED_U32 * self.fn_indices.len();
        variant_id + constexpr_opcode + constexpr_value + vec_len + vec_contents
    }
}

impl<'a> Serialize for ElementSegment<'a> {
    fn serialize<T: SerialBuffer>(&self, buffer: &mut T) {
        buffer.append_u8(ElementSegmentFormatId::ActiveImplicitTableIndex as u8);
        self.offset.serialize(buffer);
        self.fn_indices.serialize(buffer);
    }
}

/// An Element is an entry in a Table (see TableSection)
/// The only currently supported Element type is a function reference, used for indirect calls.
#[derive(Debug)]
pub struct ElementSection<'a> {
    segments: Vec<'a, ElementSegment<'a>>,
}

impl<'a> ElementSection<'a> {
    const ID: SectionId = SectionId::Element;

    pub fn preload(arena: &'a Bump, module_bytes: &[u8], cursor: &mut usize) -> Self {
        let (num_segments, body_bytes) = parse_section(Self::ID, module_bytes, cursor);

        if num_segments == 0 {
            let seg = ElementSegment {
                offset: ConstExpr::I32(1),
                fn_indices: bumpalo::vec![in arena],
            };
            ElementSection {
                segments: bumpalo::vec![in arena; seg],
            }
        } else {
            let mut segments = Vec::with_capacity_in(num_segments as usize, arena);

            let mut body_cursor = 0;
            for _ in 0..num_segments {
                let seg = ElementSegment::parse(arena, body_bytes, &mut body_cursor);
                segments.push(seg);
            }
            ElementSection { segments }
        }
    }

    /// Get a table index for a function (equivalent to a function pointer)
    /// The function will be inserted into the table if it's not already there.
    /// This index is what the call_indirect instruction expects.
    /// (This works mostly the same as function pointers, except hackers can't jump to arbitrary code)
    pub fn get_fn_table_index(&mut self, fn_index: u32) -> i32 {
        // In practice there is always one segment. We allow a bit more generality by using the last one.
        let segment = self.segments.last_mut().unwrap();
        let offset = segment.offset.unwrap_i32();
        let pos = segment.fn_indices.iter().position(|f| *f == fn_index);
        if let Some(existing_table_index) = pos {
            offset + existing_table_index as i32
        } else {
            let new_table_index = segment.fn_indices.len();
            segment.fn_indices.push(fn_index);
            offset + new_table_index as i32
        }
    }

    /// Number of elements in the table
    pub fn max_table_index(&self) -> u32 {
        let mut result = 0;
        for s in self.segments.iter() {
            let max_index = s.offset.unwrap_i32() + s.fn_indices.len() as i32;
            if max_index > result {
                result = max_index;
            }
        }
        result as u32
    }

    /// Approximate serialized byte size (for buffer capacity)
    pub fn size(&self) -> usize {
        self.segments.iter().map(|seg| seg.size()).sum()
    }

    pub fn indirect_callees(&self, arena: &'a Bump) -> Vec<'a, u32> {
        let mut result = bumpalo::vec![in arena];
        for segment in self.segments.iter() {
            result.extend_from_slice(&segment.fn_indices);
        }
        result
    }
}

impl<'a> Serialize for ElementSection<'a> {
    fn serialize<T: SerialBuffer>(&self, buffer: &mut T) {
        let header_indices = write_section_header(buffer, Self::ID);
        self.segments.serialize(buffer);
        update_section_size(buffer, header_indices);
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
    pub preloaded_bytes: &'a [u8],
    pub code_builders: Vec<'a, CodeBuilder<'a>>,
    dead_code_metadata: PreloadsCallGraph<'a>,
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

    pub fn preload(
        arena: &'a Bump,
        module_bytes: &[u8],
        cursor: &mut usize,
        import_signatures: &[u32],
        function_signatures: &[u32],
        indirect_callees: &[u32],
    ) -> Self {
        let (preloaded_count, initial_bytes) = parse_section(SectionId::Code, module_bytes, cursor);
        let preloaded_bytes = arena.alloc_slice_copy(initial_bytes);

        // TODO: Try to move this call_graph preparation to platform build time
        let dead_code_metadata = parse_preloads_call_graph(
            arena,
            initial_bytes,
            import_signatures,
            function_signatures,
            indirect_callees,
        );

        CodeSection {
            preloaded_count,
            preloaded_bytes,
            code_builders: Vec::with_capacity_in(0, arena),
            dead_code_metadata,
        }
    }

    pub(super) fn remove_dead_preloads<T: IntoIterator<Item = u32>>(
        &mut self,
        arena: &'a Bump,
        import_fn_count: u32,
        exported_fns: &[u32],
        called_preload_fns: T,
    ) {
        let live_ext_fn_indices = trace_call_graph(
            arena,
            &self.dead_code_metadata,
            exported_fns,
            called_preload_fns,
        );

        let mut buffer = Vec::with_capacity_in(self.preloaded_bytes.len(), arena);

        copy_preloads_shrinking_dead_fns(
            arena,
            &mut buffer,
            &self.dead_code_metadata,
            self.preloaded_bytes,
            import_fn_count,
            live_ext_fn_indices,
        );

        self.preloaded_bytes = buffer.into_bump_slice();
    }
}

impl<'a> Serialize for CodeSection<'a> {
    fn serialize<T: SerialBuffer>(&self, buffer: &mut T) {
        let header_indices = write_section_header(buffer, SectionId::Code);
        buffer.encode_u32(self.preloaded_count + self.code_builders.len() as u32);

        buffer.append_slice(self.preloaded_bytes);

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
                buffer.append_u8(0); // variant ID
                offset.serialize(buffer);
            }
            DataMode::Passive => {
                buffer.append_u8(1); // variant ID
            }
        }

        self.init.serialize(buffer);
    }
}

#[derive(Debug)]
pub struct DataSection<'a> {
    count: u32,
    pub bytes: Vec<'a, u8>, // public so backend.rs can calculate addr of first string
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
 * Opaque section
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
            let section_size = u32::parse((), module_bytes, cursor).unwrap();
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

/*******************************************************************
 *
 * Name section
 * https://webassembly.github.io/spec/core/appendix/custom.html#name-section
 *
 *******************************************************************/

#[repr(u8)]
#[allow(dead_code)]
enum NameSubSections {
    ModuleName = 0,
    FunctionNames = 1,
    LocalNames = 2,
}

pub struct NameSection<'a> {
    pub bytes: Vec<'a, u8>,
    pub functions: MutMap<&'a [u8], u32>,
}

impl<'a> NameSection<'a> {
    const ID: SectionId = SectionId::Custom;
    const NAME: &'static str = "name";

    pub fn size(&self) -> usize {
        self.bytes.len()
    }

    pub fn append_function(&mut self, index: u32, name: &'a [u8]) {
        index.serialize(&mut self.bytes);
        name.serialize(&mut self.bytes);
        self.functions.insert(name, index);
    }

    pub fn parse(arena: &'a Bump, module_bytes: &[u8], cursor: &mut usize) -> Self {
        // If we're already past the end of the preloaded file then there is no Name section
        if *cursor >= module_bytes.len() {
            return NameSection {
                bytes: bumpalo::vec![in arena],
                functions: MutMap::default(),
            };
        }

        // Custom section ID
        let section_id_byte = module_bytes[*cursor];
        if section_id_byte != Self::ID as u8 {
            internal_error!(
                "Expected section ID 0x{:x}, but found 0x{:x} at offset 0x{:x}",
                Self::ID as u8,
                section_id_byte,
                *cursor
            );
        }
        *cursor += 1;

        // Section size
        let section_size = u32::parse((), module_bytes, cursor).unwrap() as usize;
        let section_end = *cursor + section_size;

        let mut section = NameSection {
            bytes: Vec::with_capacity_in(section_size, arena),
            functions: MutMap::default(),
        };

        section.parse_body(arena, module_bytes, cursor, section_end);
        section
    }

    fn parse_body(
        &mut self,
        arena: &'a Bump,
        module_bytes: &[u8],
        cursor: &mut usize,
        section_end: usize,
    ) {
        let section_name = <&'a [u8]>::parse(arena, module_bytes, cursor).unwrap();
        if section_name != Self::NAME.as_bytes() {
            internal_error!(
                "Expected Custom section {:?}, found {:?}",
                Self::NAME,
                std::str::from_utf8(section_name)
            );
        }

        // Find function names subsection
        let mut found_function_names = false;
        for _possible_subsection_id in 0..2 {
            let subsection_id = module_bytes[*cursor];
            *cursor += 1;
            let subsection_size = u32::parse((), module_bytes, cursor).unwrap();
            if subsection_id == NameSubSections::FunctionNames as u8 {
                found_function_names = true;
                break;
            }
            *cursor += subsection_size as usize;
            if *cursor >= section_end {
                internal_error!("Failed to parse Name section");
            }
        }
        if !found_function_names {
            internal_error!("Failed to parse Name section");
        }

        // Function names
        let num_entries = u32::parse((), module_bytes, cursor).unwrap() as usize;
        let fn_names_start = *cursor;
        for _ in 0..num_entries {
            let fn_index = u32::parse((), module_bytes, cursor).unwrap();
            let name_bytes = <&'a [u8]>::parse(arena, module_bytes, cursor).unwrap();
            self.functions
                .insert(arena.alloc_slice_copy(name_bytes), fn_index);
        }

        // Copy only the bytes for the function names segment
        self.bytes
            .extend_from_slice(&module_bytes[fn_names_start..*cursor]);

        *cursor = section_end;
    }
}

impl<'a> Serialize for NameSection<'a> {
    fn serialize<T: SerialBuffer>(&self, buffer: &mut T) {
        if !self.bytes.is_empty() {
            let header_indices = write_custom_section_header(buffer, Self::NAME);

            let subsection_id = NameSubSections::FunctionNames as u8;
            subsection_id.serialize(buffer);

            let subsection_byte_size = (MAX_SIZE_ENCODED_U32 + self.bytes.len()) as u32;
            subsection_byte_size.serialize(buffer);

            let num_entries = self.functions.len() as u32;
            buffer.encode_padded_u32(num_entries);

            buffer.append_slice(&self.bytes);

            update_section_size(buffer, header_indices);
        }
    }
}

impl<'a> Debug for NameSection<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "NameSection")?;

        // We want to display index->name because it matches the binary format and looks nicer.
        // But our hashmap is name->index because that's what code gen wants to look up.
        let mut by_index = std::vec::Vec::with_capacity(self.functions.len());
        for (name, index) in self.functions.iter() {
            by_index.push((*index, name));
        }
        by_index.sort_unstable();

        for (index, name) in by_index.iter() {
            let name_str = unsafe { std::str::from_utf8_unchecked(name) };
            writeln!(f, "  {:4}: {}", index, name_str)?;
        }

        Ok(())
    }
}

/*******************************************************************
 *
 * Unit tests
 *
 *******************************************************************/

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
        preloaded.parse_offsets();

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
