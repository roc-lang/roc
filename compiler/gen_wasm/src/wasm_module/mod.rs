pub mod code_builder;
mod dead_code;
pub mod linking;
pub mod opcodes;
pub mod parse;
pub mod sections;
pub mod serialize;

use bumpalo::{collections::Vec, Bump};
pub use code_builder::{Align, CodeBuilder, LocalId, ValueType, VmSymbolState};
pub use linking::{OffsetRelocType, RelocationEntry, SymInfo};
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
    pub linking: LinkingSection<'a>,
    pub reloc_code: RelocationSection<'a>,
    pub reloc_data: RelocationSection<'a>,
    pub names: NameSection<'a>,
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

        let imported_fn_signatures = import.function_signatures(arena);
        let code = CodeSection::parse(
            arena,
            bytes,
            &mut cursor,
            &imported_fn_signatures,
            &function.signatures,
            &indirect_callees,
        )?;

        let data = DataSection::parse(arena, bytes, &mut cursor)?;

        let linking = LinkingSection::parse(arena, bytes, &mut cursor)?;
        let reloc_code = RelocationSection::parse((arena, "reloc.CODE"), bytes, &mut cursor)?;
        let reloc_data = RelocationSection::parse((arena, "reloc.DATA"), bytes, &mut cursor)?;
        let names = NameSection::parse(arena, bytes, &mut cursor)?;

        let mut module_errors = String::new();
        if types.is_empty() {
            module_errors.push_str("Missing Type section\n");
        }
        if function.signatures.is_empty() {
            module_errors.push_str("Missing Function section\n");
        }
        if code.preloaded_bytes.is_empty() {
            module_errors.push_str("Missing Code section\n");
        }
        if linking.symbol_table.is_empty() {
            module_errors.push_str("Missing \"linking\" Custom section\n");
        }
        if reloc_code.entries.is_empty() {
            module_errors.push_str("Missing \"reloc.CODE\" Custom section\n");
        }
        if global.count != 0 {
            let global_err_msg =
                format!("All globals in a relocatable Wasm module should be imported, but found {} internally defined", global.count);
            module_errors.push_str(&global_err_msg);
        }

        if !module_errors.is_empty() {
            return Err(ParseError {
                offset: 0,
                message: format!("{}\n{}\n{}",
                    "The host file has the wrong structure. I need a relocatable WebAssembly binary file.",
                    "If you're using wasm-ld, try the --relocatable option.",
                    module_errors,
                )
            });
        }

        Ok(WasmModule {
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
            linking,
            reloc_code,
            reloc_data,
            names,
        })
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
            self.import.function_signature_count(),
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

    pub fn relocate_preloaded_code(&mut self, sym_name: &str, value: u32) -> u32 {
        let sym_index = self
            .linking
            .find_symbol_index(sym_name)
            .unwrap_or_else(|| panic!("Linking failed! Can't find host symbol `{}`", sym_name));

        self.reloc_code.apply_relocs_u32(
            &mut self.code.preloaded_bytes,
            self.code.preloaded_reloc_offset,
            sym_index,
            value,
        );

        sym_index
    }
}
