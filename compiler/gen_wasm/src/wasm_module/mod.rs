pub mod code_builder;
pub mod linking;
pub mod opcodes;
pub mod parse;
pub mod sections;
pub mod serialize;

use std::iter::repeat;

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

    pub fn eliminate_dead_code(&mut self, arena: &'a Bump, called_host_fns: BitVec<usize>) {
        if DEBUG_SETTINGS.skip_dead_code_elim {
            return;
        }
        //
        // Mark all live host functions
        //

        let import_count = self.import.imports.len();
        let host_fn_min = import_count as u32 + self.code.dead_import_dummy_count;
        let host_fn_max = host_fn_min + self.code.preloaded_count;

        // All functions exported to JS must be kept alive
        let exported_fns = self
            .export
            .exports
            .iter()
            .filter(|ex| ex.ty == ExportType::Func)
            .map(|ex| ex.index);

        // The ElementSection lists all functions whose "address" is taken.
        // Find their signatures so we can trace all possible indirect calls.
        // (The call_indirect instruction specifies a function signature.)
        let indirect_callees_and_signatures = Vec::from_iter_in(
            self.element
                .segments
                .iter()
                .flat_map(|seg| seg.fn_indices.iter().copied())
                .map(|fn_index| {
                    let sig = self.function.signatures[fn_index as usize - import_count];
                    (fn_index, sig)
                }),
            arena,
        );

        // Trace callees of the live functions, and mark those as live too
        let live_flags = self.trace_live_host_functions(
            arena,
            called_host_fns,
            exported_fns,
            indirect_callees_and_signatures,
            host_fn_min,
            host_fn_max,
        );

        //
        // Remove all unused JS imports
        // We don't want to force the web page to provide dummy JS functions, it's a pain!
        //
        let mut live_import_fns = Vec::with_capacity_in(import_count, arena);
        let mut fn_index = 0;
        let mut eliminated_import_count = 0;
        self.import.imports.retain(|import| {
            if !matches!(import.description, ImportDesc::Func { .. }) {
                true
            } else {
                let live = live_flags[fn_index];
                if live {
                    live_import_fns.push(fn_index);
                } else {
                    eliminated_import_count += 1;
                }
                fn_index += 1;
                live
            }
        });

        // Update the count of JS imports to replace with Wasm dummies
        // (In addition to the ones we already replaced for each host-to-app call)
        self.code.dead_import_dummy_count += eliminated_import_count as u32;

        // FunctionSection
        // Insert function signatures for the new Wasm dummy functions
        let signature_count = self.function.signatures.len();
        self.function
            .signatures
            .extend(repeat(0).take(eliminated_import_count));
        self.function
            .signatures
            .copy_within(0..signature_count, eliminated_import_count);

        // NameSection
        // For each live import, swap its debug name to the right position
        for (new_index, &old_index) in live_import_fns.iter().enumerate() {
            let old_name: &str = self.names.function_names[old_index].1;
            let new_name: &str = self.names.function_names[new_index].1;
            self.names.function_names[new_index].1 = old_name;
            self.names.function_names[old_index].1 = new_name;
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
                sym_index,
                new_index as u32,
            );
        }

        //
        // Dead code elimination. Replace dead functions with tiny dummies.
        // Live function indices are unchanged, so no relocations are needed.
        //
        let dummy = CodeBuilder::dummy(arena);
        let mut dummy_bytes = Vec::with_capacity_in(dummy.size(), arena);
        dummy.serialize(&mut dummy_bytes);

        let mut buffer = Vec::with_capacity_in(self.code.preloaded_bytes.len(), arena);
        self.code.preloaded_count.serialize(&mut buffer);
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

    fn trace_live_host_functions<I: Iterator<Item = u32>>(
        &self,
        arena: &'a Bump,
        called_host_fns: BitVec<usize>,
        exported_fns: I,
        indirect_callees_and_signatures: Vec<'a, (u32, u32)>,
        host_fn_min: u32,
        host_fn_max: u32,
    ) -> BitVec<usize> {
        let reloc_len = self.reloc_code.entries.len();

        let mut call_offsets_and_symbols = Vec::with_capacity_in(reloc_len, arena);
        let mut indirect_call_offsets_and_types = Vec::with_capacity_in(reloc_len, arena);
        for entry in self.reloc_code.entries.iter() {
            match entry {
                RelocationEntry::Index {
                    type_id: IndexRelocType::FunctionIndexLeb,
                    offset,
                    symbol_index,
                } => call_offsets_and_symbols.push((*offset, *symbol_index)),
                RelocationEntry::Index {
                    type_id: IndexRelocType::TypeIndexLeb,
                    offset,
                    symbol_index,
                } => indirect_call_offsets_and_types.push((*offset, *symbol_index)),
                _ => {}
            }
        }

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
        let mut live_flags = BitVec::repeat(false, called_host_fns.len());
        let mut next_pass_fns = BitVec::repeat(false, called_host_fns.len());
        let mut current_pass_fns = called_host_fns;
        for index in exported_fns.filter(|i| *i < host_fn_max) {
            current_pass_fns.set(index as usize, true);
        }

        while current_pass_fns.count_ones() > 0 {
            // All functions in this pass are live (they have been reached by earlier passes)
            debug_assert_eq!(live_flags.len(), current_pass_fns.len());
            live_flags |= &current_pass_fns;

            // For each live function in the current pass
            for fn_index in current_pass_fns.iter_ones() {
                // Skip JS imports and Roc functions
                if fn_index < host_fn_min as usize || fn_index >= host_fn_max as usize {
                    continue;
                }

                // Find where the function body is
                let offset_index = fn_index - host_fn_min as usize;
                let code_start = self.code.preloaded_offsets[offset_index];
                let code_end = self.code.preloaded_offsets[offset_index + 1];

                // For each call in the body
                for (offset, symbol) in call_offsets_and_symbols.iter() {
                    if *offset > code_start && *offset < code_end {
                        // Find out which other function is being called
                        let callee = symbol_fn_indices[*symbol as usize];

                        // If it's not already marked live, include it in the next pass
                        if live_flags.get(callee as usize).as_deref() == Some(&false) {
                            next_pass_fns.set(callee as usize, true);
                        }
                    }
                }

                // For each indirect call in the body
                for (offset, signature) in indirect_call_offsets_and_types.iter() {
                    if *offset > code_start && *offset < code_end {
                        // Find which indirect callees have the right type signature
                        let potential_callees = indirect_callees_and_signatures
                            .iter()
                            .filter(|(_, sig)| sig == signature)
                            .map(|(f, _)| *f);
                        // Mark them all as live
                        for f in potential_callees {
                            if live_flags.get(f as usize).as_deref() == Some(&false) {
                                next_pass_fns.set(f as usize, true);
                            }
                        }
                    }
                }
            }

            std::mem::swap(&mut current_pass_fns, &mut next_pass_fns);
            next_pass_fns.fill(false);
        }

        live_flags
    }

    pub fn relocate_internal_symbol(&mut self, sym_name: &str, value: u32) -> Result<u32, String> {
        self.linking
            .find_internal_symbol(sym_name)
            .map(|sym_index| {
                self.reloc_code.apply_relocs_u32(
                    &mut self.code.preloaded_bytes,
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
    pub fn link_host_to_app_calls(
        &mut self,
        arena: &'a Bump,
        host_to_app_map: Vec<'a, (&'a str, u32)>,
    ) {
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
                    swap_sym_index,
                    host_fn_index as u32,
                );

                // Update the name in the debug info
                if let Some((_, debug_name)) = self
                    .names
                    .function_names
                    .iter_mut()
                    .find(|(i, _)| *i as usize == host_fn_index)
                {
                    debug_name.clone_from(&swap_fn_name);
                }
            }

            // Remember to insert a dummy function at the beginning of the code section
            // to compensate for having one less import, so that function indices don't change.
            self.code.dead_import_dummy_count += 1;

            // Insert any type signature for the dummy. Signature index 0 will do.
            self.function.signatures.insert(0, 0);

            // Update the debug name for the dummy
            if let Some((_, debug_name)) = self
                .names
                .function_names
                .iter_mut()
                .find(|(i, _)| *i as usize == swap_fn_index)
            {
                debug_name.clone_from(
                    &bumpalo::format!(in arena, "linking_dummy_{}", debug_name).into_bump_str(),
                );
            }
        }
    }

    /// Create a name->index lookup table for host functions that may be called from the app
    pub fn get_host_function_lookup(&self, arena: &'a Bump) -> Vec<'a, (&'a str, u32)> {
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
                        Some((*name, *index))
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
                        Some((*name, *index))
                    }
                    _ => None,
                });

        let import_fns = self
            .import
            .imports
            .iter()
            .filter(|import| matches!(import.description, ImportDesc::Func { .. }))
            .enumerate()
            .map(|(fn_index, import)| (import.name, fn_index as u32));

        Vec::from_iter_in(
            roc_global_fns.chain(other_global_fns).chain(import_fns),
            arena,
        )
    }
}
