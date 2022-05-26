pub mod code_builder;
mod dead_code;
pub mod linking;
pub mod opcodes;
pub mod parse;
pub mod sections;
pub mod serialize;

use bumpalo::{collections::Vec, Bump};
pub use code_builder::{Align, CodeBuilder, LocalId, ValueType, VmSymbolState};
pub use linking::SymInfo;
pub use sections::{ConstExpr, Export, ExportType, Global, GlobalType, Signature};

use self::linking::{LinkingSection, RelocationSection};
use self::parse::{Parse, ParseError};
use self::sections::{
    CodeSection, DataSection, ElementSection, ExportSection, FunctionSection, GlobalSection,
    ImportSection, MemorySection, NameSection, OpaqueSection, Section, SectionId, TableSection,
    TypeSection,
};
use self::serialize::{SerialBuffer, Serialize};

/// A representation of the WebAssembly binary file format
/// https://webassembly.github.io/spec/core/binary/modules.html
#[derive(Debug)]
pub struct WasmModule<'a> {
    pub data_end: u32,
    pub types: TypeSection<'a>,
    pub import: ImportSection<'a>,
    pub function: FunctionSection<'a>,
    pub table: TableSection,
    pub memory: MemorySection<'a>,
    pub global: GlobalSection<'a>,
    pub export: ExportSection<'a>,
    pub start: OpaqueSection<'a>,
    pub element: ElementSection<'a>,
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
        self.names.serialize(buffer);
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
            + self.names.size()
    }

    pub fn preload(arena: &'a Bump, bytes: &[u8]) -> Result<Self, ParseError> {
        let is_valid_magic_number = &bytes[0..4] == "\0asm".as_bytes();
        let is_valid_version = bytes[4..8] == Self::WASM_VERSION.to_le_bytes();
        if !is_valid_magic_number || !is_valid_version {
            return Err(ParseError {
                offset: 0,
                message: "This file is not a WebAssembly binary. The file header is not valid."
                    .into(),
            });
        }

        let mut cursor: usize = 8;

        let types = TypeSection::parse(arena, bytes, &mut cursor)?;
        let import = ImportSection::parse(arena, bytes, &mut cursor)?;
        let function = FunctionSection::parse(arena, bytes, &mut cursor)?;
        let table = TableSection::parse((), bytes, &mut cursor)?;
        let memory = MemorySection::parse(arena, bytes, &mut cursor)?;
        let global = GlobalSection::parse(arena, bytes, &mut cursor)?;
        let export = ExportSection::parse(arena, bytes, &mut cursor)?;
        let start = OpaqueSection::parse((arena, SectionId::Start), bytes, &mut cursor)?;
        let element = ElementSection::parse(arena, bytes, &mut cursor)?;
        let indirect_callees = element.indirect_callees(arena);

        let code = CodeSection::parse(
            arena,
            bytes,
            &mut cursor,
            &import.fn_signatures,
            &function.signatures,
            &indirect_callees,
        )?;

        let data = DataSection::parse(arena, bytes, &mut cursor)?;

        // Metadata sections
        let names = NameSection::parse(arena, bytes, &mut cursor)?;
        let linking = LinkingSection::new(arena);
        let relocations = RelocationSection::new(arena, "reloc.CODE");

        let mut module = WasmModule {
            data_end: 0,
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
        };

        if let Some(data_end) = module.get_exported_global_u32("__data_end") {
            module.data_end = data_end;
            Ok(module)
        } else {
            let message = "The Wasm module must export the __data_end global so that I know where its constant data ends (including zero data)".into();
            Err(ParseError { offset: 0, message })
        }
    }

    pub fn remove_dead_preloads<T: IntoIterator<Item = u32>>(
        &mut self,
        arena: &'a Bump,
        called_preload_fns: T,
    ) {
        let exported_fn_iter = self
            .export
            .exports
            .iter()
            .filter(|ex| ex.ty == ExportType::Func)
            .map(|ex| ex.index);
        let function_indices = Vec::from_iter_in(exported_fn_iter, arena);

        self.code.remove_dead_preloads(
            arena,
            self.import.fn_signatures.len(),
            &function_indices,
            called_preload_fns,
        )
    }

    pub fn get_exported_global_u32(&self, name: &str) -> Option<u32> {
        self.export
            .exports
            .iter()
            .find(|ex| ex.name == name)
            .and_then(|ex| self.global.parse_u32_at_index(ex.index).ok())
    }
}
