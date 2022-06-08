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
    ImportDesc, ImportSection, MemorySection, NameSection, OpaqueSection, Section, SectionId,
    TableSection, TypeSection,
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
        let exported_fn_indices = Vec::from_iter_in(exported_fn_iter, arena);

        self.code.remove_dead_preloads(
            arena,
            self.import.function_count(),
            &exported_fn_indices,
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

    pub fn relocate_internal_symbol(&mut self, sym_name: &str, value: u32) -> u32 {
        let sym_index = self
            .linking
            .find_internal_symbol(sym_name)
            .unwrap_or_else(|| panic!("Linking failed! Can't find host symbol `{}`", sym_name));

        self.reloc_code.apply_relocs_u32(
            &mut self.code.preloaded_bytes,
            self.code.preloaded_reloc_offset,
            sym_index,
            value,
        );

        sym_index
    }

    /// Linking steps for host-to-app functions like `roc__mainForHost_1_exposed`
    /// (See further explanation in the gen_wasm README)
    /// - Remove the target function from the ImportSection. It's not a JS import but the host declared it as one.
    /// - Update all of its call sites to the new index in the app
    /// - Swap the _last_ JavaScript import into the slot we just vacated
    /// - Update all call sites for the swapped JS function
    /// - Update the FunctionSection to show the correct type signature for the swapped JS function
    /// - Insert a dummy function in the CodeSection, at the same index as the swapped JS function
    pub fn link_host_to_app_calls(&mut self, host_to_app_map: Vec<'a, (&'a str, u32)>) {
        for (app_fn_name, app_fn_index) in host_to_app_map.into_iter() {
            // Find the host import, and the last imported function to swap with it.
            // Not all imports are functions, so the function index and import index may be different
            // (We could support imported globals if we relocated them, although we don't at the time of this comment)
            let mut host_fn = None;
            let mut swap_fn = None;
            self.import
                .imports
                .iter()
                .enumerate()
                .filter(|(_import_index, import)| {
                    matches!(import.description, ImportDesc::Func { .. })
                })
                .enumerate()
                .for_each(|(fn_index, (import_index, import))| {
                    swap_fn = Some((import_index, fn_index));
                    if import.name == app_fn_name {
                        host_fn = Some((import_index, fn_index));
                    }
                });

            let (host_import_index, host_fn_index) = match host_fn {
                Some(x) => x,
                None => {
                    // The Wasm host doesn't call our app function, so it must be called from JS. Export it.
                    self.export.append(Export {
                        name: app_fn_name,
                        ty: ExportType::Func,
                        index: app_fn_index,
                    });
                    continue;
                }
            };
            let (swap_import_index, swap_fn_index) = swap_fn.unwrap();

            // Note: swap_remove will not work, because some imports may not be functions.
            let swap_import = self.import.imports.remove(swap_import_index);
            if swap_import_index != host_import_index {
                self.import.imports[host_import_index] = swap_import;
            }

            // Find the host's symbol for the function we're linking
            let host_sym_index = self
                .linking
                .find_imported_function_symbol(host_fn_index as u32)
                .unwrap_or_else(|| {
                    panic!(
                        "Linking failed! Can't find fn #{} ({}) in host symbol table",
                        host_fn_index, app_fn_name
                    )
                });

            // Update calls to use the app function instead of the host import
            self.reloc_code.apply_relocs_u32(
                &mut self.code.preloaded_bytes,
                self.code.preloaded_reloc_offset,
                host_sym_index,
                app_fn_index,
            );

            if swap_import_index != host_import_index {
                // get the name using the old host import index because we already swapped it!
                let swap_fn_name = self.import.imports[host_import_index].name;

                // Find the symbol for the swapped JS import
                let swap_sym_index = self
                    .linking
                    .find_imported_function_symbol(swap_fn_index as u32)
                    .unwrap_or_else(|| {
                        panic!(
                            "Linking failed! Can't find fn #{} ({}) in host symbol table",
                            swap_fn_index, swap_fn_name
                        )
                    });

                // Update calls to the swapped JS import
                self.reloc_code.apply_relocs_u32(
                    &mut self.code.preloaded_bytes,
                    self.code.preloaded_reloc_offset,
                    swap_sym_index,
                    host_fn_index as u32,
                );

                // Update the name in the debug info
                let (_, debug_name) = self
                    .names
                    .function_names
                    .iter_mut()
                    .find(|(i, _)| *i as usize == host_fn_index)
                    .unwrap();
                debug_name.clone_from(&swap_fn_name);
            }

            // Remember to insert a dummy function at the beginning of the code section
            // to compensate for having one less import, so that function indices don't change.
            self.code.linking_dummy_count += 1;

            // Insert any type signature for the dummy. Signature index 0 will do.
            self.function.signatures.insert(0, 0);

            // Update the debug name for the dummy
            let (_, debug_name) = self
                .names
                .function_names
                .iter_mut()
                .find(|(i, _)| *i as usize == swap_fn_index)
                .unwrap();
            debug_name.clone_from(&"linking_dummy");
        }
    }
}
