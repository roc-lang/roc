use std::fmt::{Debug, Formatter};
use std::io::Write;

use bumpalo::collections::vec::Vec;
use bumpalo::Bump;
use roc_error_macros::internal_error;

use crate::{Value, DUMMY_FUNCTION};

use super::linking::{LinkingSection, SymInfo, WasmObjectSymbol};
use super::opcodes::OpCode;
use super::parse::{Parse, ParseError, SkipBytes};
use super::serialize::{SerialBuffer, Serialize, MAX_SIZE_ENCODED_U32};
use super::ValueType;

/*******************************************************************
 *
 * Helpers
 *
 *******************************************************************/

#[repr(u8)]
#[derive(PartialEq, Eq, Clone, Copy)]
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

impl Debug for SectionId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Custom => write!(f, "Custom"),
            Self::Type => write!(f, "Type"),
            Self::Import => write!(f, "Import"),
            Self::Function => write!(f, "Function"),
            Self::Table => write!(f, "Table"),
            Self::Memory => write!(f, "Memory"),
            Self::Global => write!(f, "Global"),
            Self::Export => write!(f, "Export"),
            Self::Start => write!(f, "Start"),
            Self::Element => write!(f, "Element"),
            Self::Code => write!(f, "Code"),
            Self::Data => write!(f, "Data"),
            Self::DataCount => write!(f, "DataCount"),
            #[allow(unreachable_patterns)]
            unknown => write!(f, "<unknown section ID 0x{:2x}>", *unknown as u8),
        }
    }
}

const MAX_SIZE_SECTION_HEADER: usize = std::mem::size_of::<SectionId>() + 2 * MAX_SIZE_ENCODED_U32;

// Trait to help serialize simple sections that we just store as bytes
pub trait Section<'a>: Sized {
    const ID: SectionId;
    fn size(&self) -> usize;
}

// Boilerplate for simple sections that we just store as bytes
macro_rules! section_impl {
    ($structname: ident, $id: expr, $from_count_and_bytes: expr) => {
        impl<'a> Section<'a> for $structname<'a> {
            const ID: SectionId = $id;

            fn size(&self) -> usize {
                MAX_SIZE_SECTION_HEADER + self.bytes.len()
            }
        }

        impl<'a> Parse<&'a Bump> for $structname<'a> {
            fn parse(
                arena: &'a Bump,
                module_bytes: &[u8],
                cursor: &mut usize,
            ) -> Result<Self, ParseError> {
                let (count, range) = parse_section(Self::ID, module_bytes, cursor)?;
                let mut bytes = Vec::<u8>::with_capacity_in(range.len() * 2, arena);
                *cursor = range.end;
                bytes.extend_from_slice(&module_bytes[range]);
                #[allow(clippy::redundant_closure_call)]
                Ok($from_count_and_bytes(count, bytes))
            }
        }

        impl<'a> Serialize for $structname<'a> {
            fn serialize<B: SerialBuffer>(&self, buffer: &mut B) {
                serialize_bytes_section(Self::ID, self.count, &self.bytes, buffer);
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

fn serialize_bytes_section<B: SerialBuffer>(
    id: SectionId,
    count: u32,
    bytes: &[u8],
    buffer: &mut B,
) {
    if !bytes.is_empty() {
        let header_indices = write_section_header(buffer, id);
        buffer.encode_u32(count);
        buffer.append_slice(bytes);
        update_section_size(buffer, header_indices);
    }
}

fn parse_section(
    expected_id: SectionId,
    module_bytes: &[u8],
    cursor: &mut usize,
) -> Result<(u32, std::ops::Range<usize>), ParseError> {
    if *cursor >= module_bytes.len() {
        return Err(ParseError {
            offset: *cursor,
            message: "End of file".into(),
        });
    }

    // If we see the wrong section ID, assume the one we were looking for is just empty
    if module_bytes[*cursor] != expected_id as u8 {
        return Ok((0, *cursor..*cursor));
    }
    *cursor += 1;

    let section_size = u32::parse((), module_bytes, cursor)?;
    let count_start = *cursor;
    let count = u32::parse((), module_bytes, cursor)?;
    let body_start = *cursor;
    let next_section_start = count_start + section_size as usize;

    Ok((count, body_start..next_section_start))
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
pub struct SignatureParamsIter<'a> {
    bytes: &'a [u8],
    index: usize,
    end: usize,
}

impl<'a> Iterator for SignatureParamsIter<'a> {
    type Item = ValueType;

    fn next(&mut self) -> Option<Self::Item> {
        if self.index >= self.end {
            None
        } else {
            self.bytes.get(self.index).map(|b| {
                self.index += 1;
                ValueType::from(*b)
            })
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let size = self.end - self.index;
        (size, Some(size))
    }
}

impl<'a> ExactSizeIterator for SignatureParamsIter<'a> {}

impl<'a> DoubleEndedIterator for SignatureParamsIter<'a> {
    fn next_back(&mut self) -> Option<Self::Item> {
        if self.end == 0 {
            None
        } else {
            self.end -= 1;
            self.bytes.get(self.end).map(|b| ValueType::from(*b))
        }
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
    pub fn new(arena: &'a Bump) -> Self {
        TypeSection {
            arena,
            bytes: Vec::new_in(arena),
            offsets: Vec::new_in(arena),
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

    pub fn is_empty(&self) -> bool {
        self.bytes.is_empty()
    }

    pub fn look_up(&'a self, sig_index: u32) -> (SignatureParamsIter<'a>, Option<ValueType>) {
        let mut offset = self.offsets[sig_index as usize];
        offset += 1; // separator
        let param_count = u32::parse((), &self.bytes, &mut offset).unwrap() as usize;
        let params_iter = SignatureParamsIter {
            bytes: &self.bytes[offset..][..param_count],
            index: 0,
            end: param_count,
        };
        offset += param_count;

        let return_type = if self.bytes[offset] == 0 {
            None
        } else {
            Some(ValueType::from(self.bytes[offset + 1]))
        };
        (params_iter, return_type)
    }
}

impl<'a> Section<'a> for TypeSection<'a> {
    const ID: SectionId = SectionId::Type;

    fn size(&self) -> usize {
        MAX_SIZE_SECTION_HEADER + self.bytes.len()
    }
}

impl<'a> Parse<&'a Bump> for TypeSection<'a> {
    fn parse(arena: &'a Bump, module_bytes: &[u8], cursor: &mut usize) -> Result<Self, ParseError> {
        let (count, range) = parse_section(Self::ID, module_bytes, cursor)?;
        let mut bytes = Vec::<u8>::with_capacity_in(range.len() * 2, arena);
        *cursor = range.end;
        bytes.extend_from_slice(&module_bytes[range]);

        let mut offsets = Vec::with_capacity_in(2 * count as usize, arena);
        let mut i = 0;
        while i < bytes.len() {
            offsets.push(i);

            if bytes[i] != Signature::SEPARATOR {
                return Err(ParseError {
                    message: "Invalid signature separator in TypeSection".into(),
                    offset: *cursor,
                });
            }
            i += 1;

            let n_params = u32::parse((), &bytes, &mut i).unwrap();
            i += n_params as usize; // skip over one byte per param type

            let n_return_values = bytes[i];
            i += 1 + n_return_values as usize;
        }

        Ok(TypeSection {
            arena,
            bytes,
            offsets,
        })
    }
}

impl<'a> Serialize for TypeSection<'a> {
    fn serialize<B: SerialBuffer>(&self, buffer: &mut B) {
        serialize_bytes_section(Self::ID, self.offsets.len() as u32, &self.bytes, buffer);
    }
}

/*******************************************************************
 *
 * Import section
 *
 *******************************************************************/

#[derive(Debug, PartialEq, Eq)]
pub enum ImportDesc {
    Func { signature_index: u32 },
    Table { ty: TableType },
    Mem { limits: Limits },
    Global { ty: GlobalType },
}

impl Parse<()> for ImportDesc {
    fn parse(_: (), bytes: &[u8], cursor: &mut usize) -> Result<Self, ParseError> {
        let type_id = ImportTypeId::from(bytes[*cursor]);
        *cursor += 1;
        match type_id {
            ImportTypeId::Func => {
                let signature_index = u32::parse((), bytes, cursor)?;
                Ok(ImportDesc::Func { signature_index })
            }
            ImportTypeId::Table => {
                let ty = TableType::parse((), bytes, cursor)?;
                Ok(ImportDesc::Table { ty })
            }
            ImportTypeId::Mem => {
                let limits = Limits::parse((), bytes, cursor)?;
                Ok(ImportDesc::Mem { limits })
            }
            ImportTypeId::Global => {
                let ty = GlobalType::parse((), bytes, cursor)?;
                Ok(ImportDesc::Global { ty })
            }
        }
    }
}

impl Serialize for ImportDesc {
    fn serialize<T: SerialBuffer>(&self, buffer: &mut T) {
        match self {
            Self::Func { signature_index } => {
                buffer.append_u8(ImportTypeId::Func as u8);
                signature_index.serialize(buffer);
            }
            Self::Table { ty } => {
                buffer.append_u8(ImportTypeId::Table as u8);
                ty.serialize(buffer);
            }
            Self::Mem { limits } => {
                buffer.append_u8(ImportTypeId::Mem as u8);
                limits.serialize(buffer);
            }
            Self::Global { ty } => {
                buffer.append_u8(ImportTypeId::Global as u8);
                ty.serialize(buffer);
            }
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Import<'a> {
    pub module: &'a str,
    pub name: &'a str,
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

impl<'a> Import<'a> {
    fn size(&self) -> usize {
        self.module.len()
            + self.name.len()
            + match self.description {
                ImportDesc::Func { .. } => MAX_SIZE_ENCODED_U32,
                ImportDesc::Table { .. } => 4,
                ImportDesc::Mem { .. } => 3,
                ImportDesc::Global { .. } => 2,
            }
    }

    pub fn is_function(&self) -> bool {
        matches!(self.description, ImportDesc::Func { .. })
    }
}

impl<'a> Serialize for Import<'a> {
    fn serialize<T: SerialBuffer>(&self, buffer: &mut T) {
        self.module.serialize(buffer);
        self.name.serialize(buffer);
        self.description.serialize(buffer);
    }
}

#[derive(Debug)]
pub struct ImportSection<'a> {
    pub imports: Vec<'a, Import<'a>>,
}

impl<'a> ImportSection<'a> {
    const ID: SectionId = SectionId::Import;

    pub fn new(arena: &'a Bump) -> Self {
        ImportSection {
            imports: Vec::new_in(arena),
        }
    }

    pub fn size(&self) -> usize {
        self.imports.iter().map(|imp| imp.size()).sum()
    }

    pub fn function_signatures(&self, arena: &'a Bump) -> Vec<'a, u32> {
        let sig_iter = self.imports.iter().filter_map(|imp| match imp.description {
            ImportDesc::Func { signature_index } => Some(signature_index),
            _ => None,
        });
        Vec::from_iter_in(sig_iter, arena)
    }

    pub fn function_count(&self) -> usize {
        self.imports.iter().filter(|imp| imp.is_function()).count()
    }
}

impl<'a> Parse<&'a Bump> for ImportSection<'a> {
    fn parse(arena: &'a Bump, module_bytes: &[u8], cursor: &mut usize) -> Result<Self, ParseError> {
        let (count, range) = parse_section(Self::ID, module_bytes, cursor)?;
        let mut imports = Vec::with_capacity_in(count as usize, arena);

        let end = range.end;

        while *cursor < end {
            let module = <&'a str>::parse(arena, module_bytes, cursor)?;
            let name = <&'a str>::parse(arena, module_bytes, cursor)?;
            let description = ImportDesc::parse((), module_bytes, cursor)?;

            imports.push(Import {
                module,
                name,
                description,
            });
        }

        Ok(ImportSection { imports })
    }
}

impl<'a> Serialize for ImportSection<'a> {
    fn serialize<B: SerialBuffer>(&self, buffer: &mut B) {
        if !self.imports.is_empty() {
            let header_indices = write_section_header(buffer, Self::ID);
            self.imports.serialize(buffer);
            update_section_size(buffer, header_indices);
        }
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
    pub signatures: Vec<'a, u32>,
}

impl<'a> FunctionSection<'a> {
    pub fn new(arena: &'a Bump) -> Self {
        FunctionSection {
            signatures: Vec::new_in(arena),
        }
    }

    pub fn add_sig(&mut self, sig_id: u32) {
        self.signatures.push(sig_id);
    }
}

impl<'a> Parse<&'a Bump> for FunctionSection<'a> {
    fn parse(arena: &'a Bump, module_bytes: &[u8], cursor: &mut usize) -> Result<Self, ParseError> {
        let (count, _) = parse_section(SectionId::Function, module_bytes, cursor)?;

        let mut signatures = Vec::with_capacity_in(count as usize, arena);
        for _ in 0..count {
            signatures.push(u32::parse((), module_bytes, cursor)?);
        }

        Ok(FunctionSection { signatures })
    }
}

impl<'a> Section<'a> for FunctionSection<'a> {
    const ID: SectionId = SectionId::Function;
    fn size(&self) -> usize {
        MAX_SIZE_SECTION_HEADER + self.signatures.len() * MAX_SIZE_ENCODED_U32
    }
}

impl<'a> Serialize for FunctionSection<'a> {
    fn serialize<B: SerialBuffer>(&self, buffer: &mut B) {
        if !self.signatures.is_empty() {
            let header_indices = write_section_header(buffer, Self::ID);
            self.signatures.serialize(buffer);
            update_section_size(buffer, header_indices);
        }
    }
}

/*******************************************************************
 *
 * Table section
 *
 * Defines tables used for indirect references to external code or data.
 * The table *contents* are in the ElementSection.
 *
 *******************************************************************/

#[repr(u8)]
#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum RefType {
    Func = 0x70,
    Extern = 0x6f,
}

impl Parse<()> for RefType {
    fn parse(_: (), bytes: &[u8], cursor: &mut usize) -> Result<Self, ParseError> {
        let byte = bytes[*cursor];
        *cursor += 1;
        match byte {
            0x70 => Ok(Self::Func),
            0x6f => Ok(Self::Extern),
            _ => Err(ParseError {
                offset: *cursor - 1,
                message: format!("Invalid RefType 0x{byte:2x}"),
            }),
        }
    }
}
#[derive(Debug, PartialEq, Eq)]
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
    fn skip_bytes(bytes: &[u8], cursor: &mut usize) -> Result<(), ParseError> {
        u8::skip_bytes(bytes, cursor)?;
        Limits::skip_bytes(bytes, cursor)?;
        Ok(())
    }
}

impl Parse<()> for TableType {
    fn parse(_: (), bytes: &[u8], cursor: &mut usize) -> Result<Self, ParseError> {
        let ref_type = RefType::parse((), bytes, cursor)?;
        let limits = Limits::parse((), bytes, cursor)?;
        Ok(TableType { ref_type, limits })
    }
}

#[derive(Debug)]
pub struct TableSection {
    pub function_table: TableType,
}

impl TableSection {
    const ID: SectionId = SectionId::Table;

    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        TableSection {
            function_table: TableType {
                ref_type: RefType::Func,
                limits: Limits::Min(0),
            },
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

impl Parse<()> for TableSection {
    fn parse(_ctx: (), module_bytes: &[u8], cursor: &mut usize) -> Result<Self, ParseError> {
        let (count, range) = parse_section(Self::ID, module_bytes, cursor)?;

        match count {
            0 => {
                *cursor = range.end;
                Ok(TableSection {
                    function_table: TableType {
                        ref_type: RefType::Func,
                        limits: Limits::MinMax(0, 0),
                    },
                })
            }
            1 => {
                if module_bytes[range.start] != RefType::Func as u8 {
                    Err(ParseError {
                        offset: *cursor,
                        message: "Only funcref tables are supported".into(),
                    })
                } else {
                    let limits = Limits::parse((), module_bytes, cursor)?;
                    *cursor = range.end;
                    Ok(TableSection {
                        function_table: TableType {
                            ref_type: RefType::Func,
                            limits,
                        },
                    })
                }
            }
            _ => Err(ParseError {
                offset: *cursor,
                message: "Multiple tables are not supported".into(),
            }),
        }
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

#[derive(Debug, PartialEq, Eq)]
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
    fn skip_bytes(bytes: &[u8], cursor: &mut usize) -> Result<(), ParseError> {
        let variant_id = bytes[*cursor];
        u8::skip_bytes(bytes, cursor)?; // advance past the variant byte
        u32::skip_bytes(bytes, cursor)?; // skip "min"
        if variant_id == LimitsId::MinMax as u8 {
            u32::skip_bytes(bytes, cursor)?; // skip "max"
        }
        Ok(())
    }
}

impl Parse<()> for Limits {
    fn parse(_: (), bytes: &[u8], cursor: &mut usize) -> Result<Self, ParseError> {
        if *cursor >= bytes.len() {
            return Ok(Limits::Min(0));
        }
        let variant_id = bytes[*cursor];
        *cursor += 1;

        let min = u32::parse((), bytes, cursor).unwrap();
        if variant_id == LimitsId::MinMax as u8 {
            let max = u32::parse((), bytes, cursor).unwrap();
            Ok(Limits::MinMax(min, max))
        } else {
            Ok(Limits::Min(min))
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

    pub fn min_bytes(&self) -> Result<u32, ParseError> {
        let mut cursor = 0;
        let memory_limits = Limits::parse((), &self.bytes, &mut cursor)?;
        let min_pages = match memory_limits {
            Limits::Min(pages) | Limits::MinMax(pages, _) => pages,
        };
        Ok(min_pages * MemorySection::PAGE_SIZE)
    }

    pub fn max_bytes(&self) -> Result<Option<u32>, ParseError> {
        let mut cursor = 0;
        let memory_limits = Limits::parse((), &self.bytes, &mut cursor)?;
        let bytes = match memory_limits {
            Limits::Min(_) => None,
            Limits::MinMax(_, pages) => Some(pages * MemorySection::PAGE_SIZE),
        };
        Ok(bytes)
    }
}

section_impl!(MemorySection, SectionId::Memory);

/*******************************************************************
 *
 * Global section
 *
 *******************************************************************/

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
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
    fn skip_bytes(_bytes: &[u8], cursor: &mut usize) -> Result<(), ParseError> {
        *cursor += 2;
        Ok(())
    }
}

impl Parse<()> for GlobalType {
    fn parse(_: (), bytes: &[u8], cursor: &mut usize) -> Result<Self, ParseError> {
        let value_type = ValueType::from(bytes[*cursor]);
        *cursor += 1;
        let is_mutable = bytes[*cursor] != 0;
        *cursor += 1;
        Ok(GlobalType {
            value_type,
            is_mutable,
        })
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
    fn parse_u32(bytes: &[u8], cursor: &mut usize) -> Result<u32, ParseError> {
        let err = Err(ParseError {
            offset: *cursor,
            message: "Invalid ConstExpr. Expected i32.".into(),
        });

        if bytes[*cursor] != OpCode::I32CONST as u8 {
            return err;
        }
        *cursor += 1;

        let value = u32::parse((), bytes, cursor)?;

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

    // ConstExpr and Value are separate types in case we ever need to support
    // arbitrary constant expressions, rather than just i32.const and friends.
    fn as_value(&self) -> Value {
        match self {
            ConstExpr::I32(x) => Value::I32(*x),
            ConstExpr::I64(x) => Value::I64(*x),
            ConstExpr::F32(x) => Value::F32(*x),
            ConstExpr::F64(x) => Value::F64(*x),
        }
    }
}

impl Parse<()> for ConstExpr {
    fn parse(_ctx: (), bytes: &[u8], cursor: &mut usize) -> Result<Self, ParseError> {
        let opcode = OpCode::from(bytes[*cursor]);
        *cursor += 1;

        let result = match opcode {
            OpCode::I32CONST => {
                let x = i32::parse((), bytes, cursor)?;
                Ok(ConstExpr::I32(x))
            }
            OpCode::I64CONST => {
                let x = i64::parse((), bytes, cursor)?;
                Ok(ConstExpr::I64(x))
            }
            OpCode::F32CONST => {
                let mut b = [0; 4];
                b.copy_from_slice(&bytes[*cursor..][..4]);
                Ok(ConstExpr::F32(f32::from_le_bytes(b)))
            }
            OpCode::F64CONST => {
                let mut b = [0; 8];
                b.copy_from_slice(&bytes[*cursor..][..8]);
                Ok(ConstExpr::F64(f64::from_le_bytes(b)))
            }
            _ => Err(ParseError {
                offset: *cursor,
                message: format!("Unsupported opcode {opcode:?} in constant expression."),
            }),
        };

        if bytes[*cursor] != OpCode::END as u8 {
            return Err(ParseError {
                offset: *cursor,
                message: "Expected END opcode in constant expression.".into(),
            });
        }
        *cursor += 1;

        result
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
    fn skip_bytes(bytes: &[u8], cursor: &mut usize) -> Result<(), ParseError> {
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
    pub fn new(arena: &'a Bump) -> Self {
        GlobalSection {
            count: 0,
            bytes: Vec::new_in(arena),
        }
    }

    pub fn parse_u32_at_index(&self, index: u32) -> Result<u32, ParseError> {
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

    pub fn initial_values<'b>(&self, arena: &'b Bump) -> Vec<'b, Value> {
        let mut cursor = 0;
        let iter = (0..self.count)
            .map(|_| {
                GlobalType::skip_bytes(&self.bytes, &mut cursor)?;
                ConstExpr::parse((), &self.bytes, &mut cursor).map(|x| x.as_value())
            })
            .filter_map(|r| r.ok());
        Vec::from_iter_in(iter, arena)
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
    pub name: &'a str,
    pub ty: ExportType,
    pub index: u32,
}

impl<'a> Parse<&'a Bump> for Export<'a> {
    fn parse(arena: &'a Bump, bytes: &[u8], cursor: &mut usize) -> Result<Self, ParseError> {
        let name = <&'a str>::parse(arena, bytes, cursor)?;

        let ty = ExportType::from(bytes[*cursor]);
        *cursor += 1;

        let index = u32::parse((), bytes, cursor)?;

        Ok(Export { name, ty, index })
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

    pub fn new(arena: &'a Bump) -> Self {
        ExportSection {
            exports: Vec::new_in(arena),
        }
    }

    pub fn append(&mut self, export: Export<'a>) {
        self.exports.push(export);
    }

    pub fn size(&self) -> usize {
        self.exports
            .iter()
            .map(|ex| ex.name.len() + 1 + MAX_SIZE_ENCODED_U32)
            .sum()
    }
}

impl<'a> Parse<&'a Bump> for ExportSection<'a> {
    fn parse(arena: &'a Bump, module_bytes: &[u8], cursor: &mut usize) -> Result<Self, ParseError> {
        let (num_exports, range) = parse_section(Self::ID, module_bytes, cursor)?;

        let mut export_section = ExportSection {
            exports: Vec::with_capacity_in(num_exports as usize, arena),
        };

        while *cursor < range.end {
            let export = Export::parse(arena, module_bytes, cursor)?;
            export_section.exports.push(export);
        }

        Ok(export_section)
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
pub struct ElementSegment<'a> {
    pub offset: ConstExpr, // The starting table index for the segment
    pub fn_indices: Vec<'a, u32>,
}

impl<'a> ElementSegment<'a> {
    pub fn new(arena: &'a Bump) -> Self {
        ElementSegment {
            offset: ConstExpr::I32(0),
            fn_indices: Vec::new_in(arena),
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

impl<'a> Parse<&'a Bump> for ElementSegment<'a> {
    fn parse(arena: &'a Bump, bytes: &[u8], cursor: &mut usize) -> Result<Self, ParseError> {
        // In practice we only need the original MVP format
        let format_id = bytes[*cursor];
        debug_assert!(format_id == ElementSegmentFormatId::ActiveImplicitTableIndex as u8);
        *cursor += 1;

        // The table index offset is encoded as a ConstExpr, but only I32 makes sense
        let const_expr_opcode = bytes[*cursor];
        debug_assert!(const_expr_opcode == OpCode::I32CONST as u8);
        *cursor += 1;
        let offset = u32::parse((), bytes, cursor)?;
        debug_assert!(bytes[*cursor] == OpCode::END as u8);
        *cursor += 1;

        let num_elems = u32::parse((), bytes, cursor)?;
        let mut fn_indices = Vec::with_capacity_in(num_elems as usize, arena);
        for _ in 0..num_elems {
            let fn_idx = u32::parse((), bytes, cursor)?;

            fn_indices.push(fn_idx);
        }

        Ok(ElementSegment {
            offset: ConstExpr::I32(offset as i32),
            fn_indices,
        })
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
    pub segments: Vec<'a, ElementSegment<'a>>,
}

impl<'a> ElementSection<'a> {
    const ID: SectionId = SectionId::Element;

    pub fn new(arena: &'a Bump) -> Self {
        ElementSection {
            segments: Vec::new_in(arena),
        }
    }

    /// Get a table index for a function (equivalent to a function pointer)
    /// The function will be inserted into the table if it's not already there.
    /// This index is what the call_indirect instruction expects.
    /// (This works mostly the same as function pointers, except hackers can't jump to arbitrary code)
    pub fn get_or_insert_fn(&mut self, fn_index: u32) -> i32 {
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

    pub fn is_empty(&self) -> bool {
        self.segments.iter().all(|seg| seg.fn_indices.is_empty())
    }

    /// Look up a "function pointer" (element index) and return the function index.
    pub fn lookup(&self, element_index: u32) -> Option<u32> {
        self.segments.iter().find_map(|seg| {
            let adjusted_index = element_index as usize - seg.offset.unwrap_i32() as usize;
            seg.fn_indices.get(adjusted_index).copied()
        })
    }
}

impl<'a> Parse<&'a Bump> for ElementSection<'a> {
    fn parse(arena: &'a Bump, module_bytes: &[u8], cursor: &mut usize) -> Result<Self, ParseError> {
        let (num_segments, range) = parse_section(Self::ID, module_bytes, cursor)?;

        if num_segments == 0 {
            let seg = ElementSegment {
                offset: ConstExpr::I32(1),
                fn_indices: bumpalo::vec![in arena],
            };
            *cursor = range.end;
            Ok(ElementSection {
                segments: bumpalo::vec![in arena; seg],
            })
        } else {
            let mut segments = Vec::with_capacity_in(num_segments as usize, arena);

            for _ in 0..num_segments {
                let seg = ElementSegment::parse(arena, module_bytes, cursor)?;
                segments.push(seg);
            }
            *cursor = range.end;
            Ok(ElementSection { segments })
        }
    }
}

impl<'a> Serialize for ElementSection<'a> {
    fn serialize<T: SerialBuffer>(&self, buffer: &mut T) {
        if !self.is_empty() {
            let header_indices = write_section_header(buffer, Self::ID);
            self.segments.serialize(buffer);
            update_section_size(buffer, header_indices);
        }
    }
}

/*******************************************************************
 *
 * Code section (see also code_builder.rs)
 *
 *******************************************************************/

#[derive(Debug)]
pub struct CodeSection<'a> {
    pub function_count: u32,
    pub section_offset: u32,
    pub bytes: Vec<'a, u8>,
    /// The start of each function
    pub function_offsets: Vec<'a, u32>,
    /// Dead imports are replaced with dummy functions in CodeSection
    pub dead_import_dummy_count: u32,
}

impl<'a> CodeSection<'a> {
    pub fn new(arena: &'a Bump) -> Self {
        CodeSection {
            function_count: 0,
            section_offset: 0,
            bytes: Vec::new_in(arena),
            function_offsets: Vec::new_in(arena),
            dead_import_dummy_count: 0,
        }
    }

    pub fn size(&self) -> usize {
        MAX_SIZE_SECTION_HEADER + self.bytes.len()
    }

    pub fn parse(
        arena: &'a Bump,
        module_bytes: &[u8],
        cursor: &mut usize,
    ) -> Result<Self, ParseError> {
        if module_bytes[*cursor] != SectionId::Code as u8 {
            return Err(ParseError {
                offset: *cursor,
                message: "Missing code section!".into(),
            });
        }
        *cursor += 1;
        let section_size = u32::parse((), module_bytes, cursor)? as usize;
        let section_body_start = *cursor;
        let function_count = u32::parse((), module_bytes, cursor)?;
        let next_section_start = section_body_start + section_size;

        // `bytes` must include the function count for linker offsets to be correct.
        let mut bytes = Vec::with_capacity_in(section_size + section_size / 2, arena);
        bytes.extend_from_slice(&module_bytes[section_body_start..*cursor]);

        let mut function_offsets = Vec::with_capacity_in(function_count as usize, arena);

        // While copying the code bytes, also note where each function starts & ends
        // Later we will use this for dead code elimination
        while *cursor < next_section_start {
            let fn_start = *cursor;
            function_offsets.push((fn_start - section_body_start) as u32);
            let fn_length = u32::parse((), module_bytes, cursor)? as usize;
            *cursor += fn_length;
            bytes.extend_from_slice(&module_bytes[fn_start..*cursor]);
        }

        debug_assert_eq!(function_offsets.len(), function_count as usize);

        Ok(CodeSection {
            function_count,
            section_offset: section_body_start as u32,
            bytes,
            function_offsets,
            dead_import_dummy_count: 0,
        })
    }
}

impl<'a> Serialize for CodeSection<'a> {
    fn serialize<T: SerialBuffer>(&self, buffer: &mut T) {
        let header_indices = write_section_header(buffer, SectionId::Code);
        buffer.encode_u32(self.dead_import_dummy_count + self.function_count);

        // Insert dummy functions, requested by our linking logic.
        // This helps to minimise the number of functions we need to move around during linking.
        for _ in 0..self.dead_import_dummy_count {
            DUMMY_FUNCTION.serialize(buffer);
        }

        // real functions
        let first_fn_start = self.function_offsets[0] as usize;
        buffer.append_slice(&self.bytes[first_fn_start..]);

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
    const ACTIVE: u8 = 0;
    const PASSIVE: u8 = 1;

    pub fn active_at(offset: u32) -> Self {
        DataMode::Active {
            offset: ConstExpr::I32(offset as i32),
        }
    }
}

impl Serialize for DataMode {
    fn serialize<T: SerialBuffer>(&self, buffer: &mut T) {
        match self {
            Self::Active { offset } => {
                buffer.append_u8(Self::ACTIVE);
                offset.serialize(buffer);
            }
            Self::Passive => {
                buffer.append_u8(Self::PASSIVE);
            }
        }
    }
}

impl Parse<()> for DataMode {
    fn parse(_: (), bytes: &[u8], cursor: &mut usize) -> Result<Self, ParseError> {
        let variant_id = bytes[*cursor];
        *cursor += 1;

        if variant_id == Self::ACTIVE {
            let offset = ConstExpr::parse_u32(bytes, cursor)?;
            Ok(DataMode::Active {
                offset: ConstExpr::I32(offset as i32),
            })
        } else if variant_id == Self::PASSIVE {
            Ok(DataMode::Passive)
        } else {
            Err(ParseError {
                offset: *cursor - 1,
                message: format!("Data section: invalid DataMode variant 0x{variant_id:x}"),
            })
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
        self.mode.serialize(buffer);
        self.init.serialize(buffer);
    }
}

#[derive(Debug)]
pub struct DataSection<'a> {
    pub end_addr: u32,
    count: u32,
    bytes: Vec<'a, u8>,
}

impl<'a> DataSection<'a> {
    const ID: SectionId = SectionId::Data;

    pub fn new(arena: &'a Bump) -> Self {
        DataSection {
            end_addr: 0,
            count: 0,
            bytes: Vec::new_in(arena),
        }
    }

    pub fn size(&self) -> usize {
        MAX_SIZE_SECTION_HEADER + self.bytes.len()
    }

    pub fn append_segment(&mut self, segment: DataSegment<'a>) -> u32 {
        let index = self.count;
        self.count += 1;
        segment.serialize(&mut self.bytes);
        index
    }

    pub fn load_into(&self, memory: &mut [u8]) -> Result<(), String> {
        let mut cursor = 0;
        for _ in 0..self.count {
            let mode =
                DataMode::parse((), &self.bytes, &mut cursor).map_err(|e| format!("{e:?}"))?;
            let start = match mode {
                DataMode::Active {
                    offset: ConstExpr::I32(addr),
                } => addr as usize,
                _ => {
                    continue;
                }
            };
            let len32 = u32::parse((), &self.bytes, &mut cursor).map_err(|e| format!("{e:?}"))?;
            let len = len32 as usize;
            let mut target_slice = &mut memory[start..][..len];
            target_slice
                .write(&self.bytes[cursor..][..len])
                .map_err(|e| format!("{e:?}"))?;
            cursor += len;
        }
        Ok(())
    }
}

impl<'a> Parse<&'a Bump> for DataSection<'a> {
    fn parse(arena: &'a Bump, module_bytes: &[u8], cursor: &mut usize) -> Result<Self, ParseError> {
        if *cursor >= module_bytes.len() {
            return Ok(DataSection {
                end_addr: 0,
                count: 0,
                bytes: Vec::<u8>::new_in(arena),
            });
        }
        let (count, range) = parse_section(Self::ID, module_bytes, cursor)?;

        let end = range.end;
        let mut bytes = Vec::<u8>::with_capacity_in(range.len() * 2, arena);
        bytes.extend_from_slice(&module_bytes[range]);

        let mut end_addr = 0;
        for _ in 0..count {
            let mode = DataMode::parse((), module_bytes, cursor)?;
            match mode {
                DataMode::Active {
                    offset: ConstExpr::I32(offset_addr),
                } if offset_addr > end_addr => {
                    end_addr = offset_addr;
                }
                _ => {}
            }
            let segment_bytes_len = u32::parse((), module_bytes, cursor)?;
            *cursor += segment_bytes_len as usize;
        }

        debug_assert_eq!(*cursor, end);

        Ok(DataSection {
            end_addr: end_addr as u32,
            count,
            bytes,
        })
    }
}

impl<'a> Serialize for DataSection<'a> {
    fn serialize<B: SerialBuffer>(&self, buffer: &mut B) {
        serialize_bytes_section(Self::ID, self.count, &self.bytes, buffer);
    }
}

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
    pub fn new() -> Self {
        OpaqueSection { bytes: &[] }
    }

    pub fn size(&self) -> usize {
        self.bytes.len()
    }
}

impl<'a> Parse<(&'a Bump, SectionId)> for OpaqueSection<'a> {
    fn parse(
        (arena, id): (&'a Bump, SectionId),
        module_bytes: &[u8],
        cursor: &mut usize,
    ) -> Result<Self, ParseError> {
        let bytes: &[u8];

        if module_bytes[*cursor] != id as u8 {
            bytes = &[];
        } else {
            let section_start = *cursor;
            *cursor += 1;
            let section_size = u32::parse((), module_bytes, cursor)?;
            let next_section_start = *cursor + section_size as usize;
            bytes = &module_bytes[section_start..next_section_start];
            *cursor = next_section_start;
        };

        Ok(OpaqueSection {
            bytes: arena.alloc_slice_clone(bytes),
        })
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
    pub function_names: Vec<'a, (u32, &'a str)>,
}

impl<'a> NameSection<'a> {
    const ID: SectionId = SectionId::Custom;
    const NAME: &'static str = "name";

    pub fn size(&self) -> usize {
        self.function_names
            .iter()
            .map(|(_, s)| MAX_SIZE_ENCODED_U32 + s.len())
            .sum()
    }

    pub fn append_function(&mut self, index: u32, name: &'a str) {
        self.function_names.push((index, name));
    }

    pub fn new(arena: &'a Bump) -> Self {
        NameSection {
            function_names: bumpalo::vec![in arena],
        }
    }

    pub fn from_imports_and_linking_data(
        arena: &'a Bump,
        import: &ImportSection<'a>,
        linking: &LinkingSection<'a>,
    ) -> Self {
        let import_fns = import.imports.iter().filter(|imp| imp.is_function());
        let import_names = Vec::from_iter_in(import_fns.map(|imp| imp.name), arena);

        let symbols = linking.symbol_table.iter();
        let names = symbols.filter_map(|sym_info| match sym_info {
            SymInfo::Function(WasmObjectSymbol::ExplicitlyNamed { index, name, .. }) => {
                Some((*index, *name))
            }
            SymInfo::Function(WasmObjectSymbol::ImplicitlyNamed { index, .. }) => {
                Some((*index, import_names[*index as usize]))
            }
            _ => None,
        });

        let mut function_names = Vec::from_iter_in(names, arena);
        function_names.sort_by_key(|(idx, _name)| *idx);

        NameSection { function_names }
    }
}

impl<'a> Parse<&'a Bump> for NameSection<'a> {
    fn parse(arena: &'a Bump, module_bytes: &[u8], cursor: &mut usize) -> Result<Self, ParseError> {
        let cursor_start = *cursor;

        // If we're already past the end of the preloaded file then there is no Name section
        if *cursor >= module_bytes.len() {
            return Ok(Self::new(arena));
        }

        // Custom section ID
        if module_bytes[*cursor] != Self::ID as u8 {
            return Ok(Self::new(arena));
        }
        *cursor += 1;

        // Section size
        let section_size = u32::parse((), module_bytes, cursor)? as usize;
        let section_end = *cursor + section_size;

        let section_name = <&'a str>::parse(arena, module_bytes, cursor)?;
        if section_name != Self::NAME {
            // This is a different Custom section. This host has no debug info.
            // Not a parse error, just an empty section.
            *cursor = cursor_start;
            return Ok(Self::new(arena));
        }

        // Find function names subsection
        let mut found_function_names = false;
        for _possible_subsection_id in 0..2 {
            let subsection_id = module_bytes[*cursor];
            *cursor += 1;
            let subsection_size = u32::parse((), module_bytes, cursor)?;
            if subsection_id == NameSubSections::FunctionNames as u8 {
                found_function_names = true;
                break;
            }
            *cursor += subsection_size as usize;
            if *cursor >= section_end {
                return Err(ParseError {
                    message: "Failed to parse Name section".into(),
                    offset: *cursor,
                });
            }
        }
        if !found_function_names {
            return Err(ParseError {
                message: "Failed to parse Name section".into(),
                offset: *cursor,
            });
        }

        let count = u32::parse((), module_bytes, cursor)?;
        let mut section = NameSection {
            function_names: Vec::with_capacity_in(count as usize, arena),
        };

        // Function names
        for _ in 0..count {
            let index = u32::parse((), module_bytes, cursor)?;
            let name = <&'a str>::parse(arena, module_bytes, cursor)?;
            section.function_names.push((index, name));
        }

        *cursor = section_end;

        Ok(section)
    }
}

impl<'a> Serialize for NameSection<'a> {
    fn serialize<T: SerialBuffer>(&self, buffer: &mut T) {
        if !self.function_names.is_empty() {
            let header_indices = write_custom_section_header(buffer, Self::NAME);

            let subsection_id = NameSubSections::FunctionNames as u8;
            subsection_id.serialize(buffer);

            let subsection_size_index = buffer.encode_padded_u32(0);
            let subsection_start = buffer.size();

            self.function_names.serialize(buffer);

            buffer.overwrite_padded_u32(
                subsection_size_index,
                (buffer.size() - subsection_start) as u32,
            );

            update_section_size(buffer, header_indices);
        }
    }
}

impl<'a> Debug for NameSection<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "NameSection")?;

        for (index, name) in self.function_names.iter() {
            writeln!(f, "  {index:4}: {name}")?;
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
        let preloaded = TypeSection::parse(arena, &original_serialized, &mut cursor).unwrap();

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
