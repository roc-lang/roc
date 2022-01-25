pub mod code_builder;
mod dead_code;
pub mod linking;
pub mod opcodes;
pub mod sections;
pub mod serialize;

use bumpalo::Bump;
pub use code_builder::{Align, CodeBuilder, LocalId, ValueType, VmSymbolState};
pub use linking::SymInfo;
use roc_error_macros::internal_error;
pub use sections::{ConstExpr, Export, ExportType, Global, GlobalType, Signature};

use crate::wasm_module::serialize::SkipBytes;

use self::linking::{LinkingSection, RelocationSection};
use self::sections::{
    CodeSection, DataSection, ExportSection, FunctionSection, GlobalSection, ImportSection,
    MemorySection, NameSection, OpaqueSection, Section, SectionId, TypeSection,
};
use self::serialize::{SerialBuffer, Serialize};

/// A representation of the WebAssembly binary file format
/// https://webassembly.github.io/spec/core/binary/modules.html
#[derive(Debug)]
pub struct WasmModule<'a> {
    pub types: TypeSection<'a>,
    pub import: ImportSection<'a>,
    pub function: FunctionSection<'a>,
    pub table: OpaqueSection<'a>,
    pub memory: MemorySection<'a>,
    pub global: GlobalSection<'a>,
    pub export: ExportSection<'a>,
    pub start: OpaqueSection<'a>,
    pub element: OpaqueSection<'a>,
    pub code: CodeSection<'a>,
    pub data: DataSection<'a>,
    pub names: NameSection<'a>,
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
    pub fn serialize<T: SerialBuffer>(&self, buffer: &mut T) {
        buffer.append_u8(0);
        buffer.append_slice("asm".as_bytes());
        buffer.write_unencoded_u32(Self::WASM_VERSION);

        self.types.serialize(buffer);
        self.import.serialize(buffer);
        self.function.serialize(buffer);
        self.table.serialize(buffer);
        self.memory.serialize(buffer);
        self.global.serialize(buffer);
        self.export.serialize(buffer);
        self.start.serialize(buffer);
        self.element.serialize(buffer);
        self.code.serialize(buffer);
        self.data.serialize(buffer);
    }

    /// Serialize the module to bytes
    /// (Mutates some data related to linking)
    pub fn serialize_with_linker_data_mut<T: SerialBuffer>(&mut self, buffer: &mut T) {
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

        // Code section is the only one with relocations so we can stop counting
        let code_section_index = counter.section_index;
        self.code
            .serialize_with_relocs(buffer, &mut self.relocations.entries);

        self.data.serialize(buffer);

        self.linking.serialize(buffer);

        self.relocations.target_section_index = Some(code_section_index);
        self.relocations.serialize(buffer);
    }

    /// Module size in bytes (assuming no linker data)
    /// May be slightly overestimated. Intended for allocating buffer capacity.
    pub fn size(&self) -> usize {
        self.types.size()
            + self.import.size()
            + self.function.size()
            + self.table.size()
            + self.memory.size()
            + self.global.size()
            + self.export.size()
            + self.start.size()
            + self.element.size()
            + self.code.size()
            + self.data.size()
    }

    pub fn preload(arena: &'a Bump, bytes: &[u8]) -> Self {
        let is_valid_magic_number = &bytes[0..4] == "\0asm".as_bytes();
        let is_valid_version = bytes[4..8] == Self::WASM_VERSION.to_le_bytes();
        if !is_valid_magic_number || !is_valid_version {
            internal_error!("Invalid Wasm object file header for platform & builtins");
        }

        let mut cursor: usize = 8;

        let mut types = TypeSection::preload(arena, bytes, &mut cursor);
        types.parse_offsets();

        let import = ImportSection::preload(arena, bytes, &mut cursor);
        let function = FunctionSection::preload(arena, bytes, &mut cursor);
        let table = OpaqueSection::preload(SectionId::Table, arena, bytes, &mut cursor);
        let memory = MemorySection::preload(arena, bytes, &mut cursor);
        let global = GlobalSection::preload(arena, bytes, &mut cursor);

        ExportSection::skip_bytes(bytes, &mut cursor);
        let export = ExportSection::empty(arena);

        let start = OpaqueSection::preload(SectionId::Start, arena, bytes, &mut cursor);
        let element = OpaqueSection::preload(SectionId::Element, arena, bytes, &mut cursor);
        let code = CodeSection::preload(arena, bytes, &mut cursor, import.function_count);

        let data = DataSection::preload(arena, bytes, &mut cursor);

        // Metadata sections
        let names = NameSection::parse(arena, bytes, &mut cursor);
        let linking = LinkingSection::new(arena);
        let relocations = RelocationSection::new(arena, "reloc.CODE");

        WasmModule {
            types,
            import,
            function,
            table,
            memory,
            global,
            export,
            start,
            element,
            code,
            data,
            names,
            linking,
            relocations,
        }
    }

    pub fn remove_dead_preloads<T: IntoIterator<Item = u32>>(
        &mut self,
        arena: &'a Bump,
        called_preload_fns: T,
    ) {
        self.code.remove_dead_preloads(
            arena,
            self.import.function_count,
            &self.export.function_indices,
            called_preload_fns,
        )
    }
}

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
