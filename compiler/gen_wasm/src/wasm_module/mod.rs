pub mod code_builder;
pub mod linking;
pub mod opcodes;
pub mod parse;
pub mod sections;
pub mod serialize;

pub use code_builder::{Align, CodeBuilder, LocalId, ValueType, VmSymbolState};
pub use linking::{OffsetRelocType, RelocationEntry, SymInfo};
pub use sections::{ConstExpr, Export, ExportType, Global, GlobalType, Signature};

use bitvec::vec::BitVec;
use bumpalo::{collections::Vec, Bump};

use crate::DEBUG_SETTINGS;

use self::linking::{IndexRelocType, LinkingSection, RelocationSection, WasmObjectSymbol};
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
        let _data_count = OpaqueSection::parse((arena, SectionId::DataCount), bytes, &mut cursor)?;
        let code = CodeSection::parse(arena, bytes, &mut cursor)?;
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

    pub fn eliminate_dead_code(&mut self, arena: &'a Bump, called_host_fns: &[u32]) {
        if DEBUG_SETTINGS.skip_dead_code_elim {
            return;
        }
        //
        // Mark all live host functions
        //
        let host_fn_min = self.import.imports.len() as u32 + self.code.dead_import_dummy_count;
        let host_fn_max = host_fn_min + self.code.preloaded_count;

        // All functions exported to JS must be kept alive
        let exported_fns = self
            .export
            .exports
            .iter()
            .filter(|ex| ex.ty == ExportType::Func)
            .map(|ex| ex.index);

        // We conservatively assume that all functions declared as indirectly callable, are live
        let indirect_callees = self
            .element
            .segments
            .iter()
            .flat_map(|seg| seg.fn_indices.iter().copied());

        // Trace callees of the live functions, and mark those as live too
        let live_flags = self.trace_live_host_functions(
            arena,
            called_host_fns,
            exported_fns,
            indirect_callees,
            host_fn_min,
            host_fn_max,
        );

        //
        // Remove all unused JS imports
        // We don't want to force the web page to provide dummy JS functions, it's a pain!
        //

        let mut fn_index = 0;
        self.import.imports.retain(|import| {
            if !matches!(import.description, ImportDesc::Func { .. }) {
                true
            } else {
                let live = live_flags[fn_index];
                fn_index += 1;
                live
            }
        });

        //
        // Update function signatures & debug names for imports that changed index
        //
        let live_import_fns = Vec::from_iter_in(
            live_flags
                .iter_ones()
                .take_while(|i| *i < self.import.imports.len()),
            arena,
        );
        for (new_index, old_index) in live_import_fns.iter().enumerate() {
            // Safe because `old_index >= new_index`
            self.function.signatures[new_index] = self.function.signatures[*old_index];
            self.names.function_names[new_index] = self.names.function_names[*old_index];
        }
        let first_dead_import_index = live_import_fns.last().map(|x| x + 1).unwrap_or(0) as u32;
        for i in first_dead_import_index..host_fn_min {
            self.names.function_names[i as usize] = (i, "unused_host_import");
        }

        //
        // Relocate Wasm calls to JS imports
        // This must happen *before* we run dead code elimination on the code section,
        // so that byte offsets in the host's linking data will still be valid.
        //
        for (new_index, &old_index) in live_import_fns.iter().enumerate() {
            if new_index == old_index {
                continue;
            }
            let sym_index = self
                .linking
                .find_and_reindex_imported_fn(old_index as u32, new_index as u32)
                .unwrap();
            self.reloc_code.apply_relocs_u32(
                &mut self.code.preloaded_bytes,
                self.code.preloaded_reloc_offset,
                sym_index,
                new_index as u32,
            );
        }

        //
        // For every eliminated JS import, insert a dummy Wasm function at the same index.
        // This avoids shifting the indices of Wasm functions, which would require more linking work.
        //
        let dead_import_count = host_fn_min - live_import_fns.len() as u32;
        self.code.dead_import_dummy_count += dead_import_count;

        //
        // Dead code elimination. Replace dead functions with tiny dummies.
        // This is fast. Live function indices are unchanged, so no relocations are needed.
        //
        let dummy = CodeBuilder::dummy(arena);
        let mut dummy_bytes = Vec::with_capacity_in(dummy.size(), arena);
        dummy.serialize(&mut dummy_bytes);

        let mut buffer = Vec::with_capacity_in(self.code.preloaded_bytes.len(), arena);
        for (i, fn_index) in (host_fn_min..host_fn_max).enumerate() {
            if live_flags[fn_index as usize] {
                let code_start = self.code.preloaded_offsets[i] as usize;
                let code_end = self.code.preloaded_offsets[i + 1] as usize;
                buffer.extend_from_slice(&self.code.preloaded_bytes[code_start..code_end]);
            } else {
                buffer.extend_from_slice(&dummy_bytes);
            }
        }

        self.code.preloaded_bytes = buffer;
    }

    fn trace_live_host_functions<E: Iterator<Item = u32>, I: Iterator<Item = u32>>(
        &self,
        arena: &'a Bump,
        called_host_fns: &[u32],
        exported_fns: E,
        indirectly_called_fns: I,
        host_fn_min: u32,
        host_fn_max: u32,
    ) -> BitVec<usize> {
        let call_offsets_and_symbols: Vec<(u32, u32)> = Vec::from_iter_in(
            self.reloc_code
                .entries
                .iter()
                .filter_map(|entry| match entry {
                    RelocationEntry::Index {
                        type_id: IndexRelocType::FunctionIndexLeb,
                        offset,
                        symbol_index,
                    } => Some((*offset, *symbol_index)),
                    _ => None,
                }),
            arena,
        );

        // Create a fast lookup from symbol index to function index, for the inner loop below
        // (Do all the matching and dereferencing outside the loop)
        let symbol_fn_indices: Vec<'a, u32> = Vec::from_iter_in(
            self.linking
                .symbol_table
                .iter()
                .map(|sym_info| match sym_info {
                    SymInfo::Function(WasmObjectSymbol::ExplicitlyNamed { index, .. }) => *index,
                    SymInfo::Function(WasmObjectSymbol::ImplicitlyNamed { index, .. }) => *index,
                    _ => u32::MAX, // just use a dummy value for non-function symbols
                }),
            arena,
        );

        // Loop variables for the main loop below
        let capacity = host_fn_max as usize + self.code.code_builders.len();
        let mut live_flags = BitVec::repeat(false, capacity);
        let mut next_pass_fns = Vec::with_capacity_in(capacity, arena);
        let mut current_pass_fns = Vec::with_capacity_in(capacity, arena);

        // Start with everything called from Roc and everything exported to JS
        // Also include everything that can be indirectly called (crude, but good enough!)
        current_pass_fns.extend(
            called_host_fns
                .iter()
                .copied()
                .chain(exported_fns)
                .chain(indirectly_called_fns),
        );

        while !current_pass_fns.is_empty() {
            current_pass_fns.sort_unstable();
            current_pass_fns.dedup();

            // For each live function in the current pass
            for fn_index in current_pass_fns.iter().copied() {
                live_flags.set(fn_index as usize, true);

                // Find where the function body is
                let offset_index = (fn_index - host_fn_min) as usize;
                let code_start = self.code.preloaded_offsets[offset_index];
                let code_end = self.code.preloaded_offsets[offset_index + 1];

                // For each call in the body
                for (offset, symbol) in call_offsets_and_symbols.iter() {
                    if *offset > code_start && *offset < code_end {
                        // Find out which other function is being called
                        let called_fn_index = symbol_fn_indices[*symbol as usize];

                        // If it's not already marked live, include it in the next pass
                        if !live_flags[called_fn_index as usize] {
                            next_pass_fns.push(called_fn_index);
                        }
                    }
                }
            }
            current_pass_fns.clone_from(&next_pass_fns);
            next_pass_fns.clear();
        }

        live_flags
    }

    pub fn relocate_internal_symbol(&mut self, sym_name: &str, value: u32) -> Result<u32, String> {
        self.linking
            .find_internal_symbol(sym_name)
            .map(|sym_index| {
                self.reloc_code.apply_relocs_u32(
                    &mut self.code.preloaded_bytes,
                    self.code.preloaded_reloc_offset,
                    sym_index as u32,
                    value,
                );

                sym_index as u32
            })
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
                .find_and_reindex_imported_fn(host_fn_index as u32, app_fn_index)
                .unwrap();

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
                    .find_and_reindex_imported_fn(swap_fn_index as u32, host_fn_index as u32)
                    .unwrap();

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
            self.code.dead_import_dummy_count += 1;

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

    /// Create a name->index lookup table for host functions that may be called from the app
    pub fn get_host_function_lookup(&self, arena: &'a Bump) -> Vec<'a, (u32, &'a str)> {
        // Functions beginning with `roc_` go first, since they're most likely to be called
        let roc_global_fns =
            self.linking
                .symbol_table
                .iter()
                .filter_map(|sym_info| match sym_info {
                    SymInfo::Function(WasmObjectSymbol::ExplicitlyNamed { flags, index, name })
                        if flags & linking::WASM_SYM_BINDING_LOCAL == 0
                            && name.starts_with("roc_") =>
                    {
                        Some((*index, *name))
                    }
                    _ => None,
                });

        let other_global_fns =
            self.linking
                .symbol_table
                .iter()
                .filter_map(|sym_info| match sym_info {
                    SymInfo::Function(WasmObjectSymbol::ExplicitlyNamed { flags, index, name })
                        if flags & linking::WASM_SYM_BINDING_LOCAL == 0
                            && !name.starts_with("roc_") =>
                    {
                        Some((*index, *name))
                    }
                    _ => None,
                });

        let import_fns = self
            .import
            .imports
            .iter()
            .filter(|import| matches!(import.description, ImportDesc::Func { .. }))
            .enumerate()
            .map(|(fn_index, import)| (fn_index as u32, import.name));

        Vec::from_iter_in(
            roc_global_fns.chain(other_global_fns).chain(import_fns),
            arena,
        )
    }
}
