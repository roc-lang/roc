pub mod linking;
pub mod opcodes;
pub mod parse;
pub mod sections;
pub mod serialize;

use std::iter::repeat;

pub use linking::{OffsetRelocType, RelocationEntry, SymInfo};
use opcodes::OpCode;
use roc_error_macros::internal_error;
pub use sections::{ConstExpr, Export, ExportType, Global, GlobalType, Signature};

use bitvec::vec::BitVec;
use bumpalo::{collections::Vec, Bump};

use self::linking::{IndexRelocType, LinkingSection, RelocationSection, WasmObjectSymbol};
use self::parse::{Parse, ParseError};
use self::sections::{
    CodeSection, DataSection, ElementSection, ExportSection, FunctionSection, GlobalSection,
    ImportDesc, ImportSection, MemorySection, NameSection, OpaqueSection, Section, SectionId,
    TableSection, TypeSection,
};
pub use self::serialize::{SerialBuffer, Serialize};

pub const STACK_POINTER_GLOBAL_ID: u32 = 0;
pub const FRAME_ALIGNMENT_BYTES: i32 = 16;

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

    pub fn new(arena: &'a Bump) -> Self {
        WasmModule {
            types: TypeSection::new(arena),
            import: ImportSection::new(arena),
            function: FunctionSection::new(arena),
            table: TableSection::new(),
            memory: MemorySection::new(arena, 0),
            global: GlobalSection::new(arena),
            export: ExportSection::new(arena),
            start: OpaqueSection::new(),
            element: ElementSection::new(arena),
            code: CodeSection::new(arena),
            data: DataSection::new(arena),
            linking: LinkingSection::new(arena),
            reloc_code: RelocationSection::new(arena, "reloc.CODE"),
            reloc_data: RelocationSection::new(arena, "reloc.DATA"),
            names: NameSection::new(arena),
        }
    }

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
        if !self.element.is_empty() {
            self.table.serialize(buffer);
        }
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

    pub fn preload(
        arena: &'a Bump,
        bytes: &[u8],
        require_relocatable: bool,
    ) -> Result<Self, ParseError> {
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

        // Initialise the Custom sections that we care about. All empty.
        let mut linking = LinkingSection::new(arena);
        let mut reloc_code = RelocationSection::new(arena, "reloc.CODE");
        let mut reloc_data = RelocationSection::new(arena, "reloc.DATA");
        let mut names = NameSection::new(arena);

        // Consume all remaining Custom sections
        while let Ok((section_name, section_end)) = Self::peek_custom_section(arena, bytes, cursor)
        {
            match section_name {
                "linking" => {
                    linking = LinkingSection::parse(arena, bytes, &mut cursor)?;
                }
                "reloc.CODE" => {
                    reloc_code =
                        RelocationSection::parse((arena, "reloc.CODE"), bytes, &mut cursor)?;
                }
                "reloc.DATA" => {
                    reloc_data =
                        RelocationSection::parse((arena, "reloc.DATA"), bytes, &mut cursor)?;
                }
                "name" => {
                    names = NameSection::parse(arena, bytes, &mut cursor)?;
                }
                _ => {
                    cursor = section_end;
                }
            }
        }

        let mut module_errors = String::new();
        if types.is_empty() {
            module_errors.push_str("Missing Type section\n");
        }
        if function.signatures.is_empty() {
            module_errors.push_str("Missing Function section\n");
        }
        if code.bytes.is_empty() {
            module_errors.push_str("Missing Code section\n");
        }

        if require_relocatable {
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
        }

        if !module_errors.is_empty() {
            let message = if require_relocatable {
                format!(
                    "{}\n{}\n{}",
                    "The host file has the wrong structure. I need a relocatable WebAssembly binary file.",
                    "If you're using wasm-ld, try the --relocatable option.",
                    module_errors,
                )
            } else {
                format!("I wasn't able to understand this WebAssembly file.\n{module_errors}",)
            };
            return Err(ParseError { offset: 0, message });
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

    fn peek_custom_section(
        arena: &'a Bump,
        module_bytes: &[u8],
        immutable_cursor: usize,
    ) -> Result<(&'a str, usize), ParseError> {
        let mut cursor = immutable_cursor;

        if cursor >= module_bytes.len() {
            return Err(ParseError {
                message: "EOF".into(),
                offset: cursor,
            });
        }
        if module_bytes[cursor] != SectionId::Custom as u8 {
            return Err(ParseError {
                message: format!(
                    "Expected Custom section but found section ID 0x{:02x}",
                    module_bytes[cursor]
                ),
                offset: cursor,
            });
        }
        cursor += 1;

        let section_size = u32::parse((), module_bytes, &mut cursor)?;
        let section_end = cursor + section_size as usize;
        let section_name = <&'a str>::parse(arena, module_bytes, &mut cursor)?;

        Ok((section_name, section_end))
    }

    pub fn eliminate_dead_code(&mut self, arena: &'a Bump, called_fns: BitVec<usize>) {
        if DEBUG_SETTINGS.skip_dead_code_elim {
            return;
        }
        //
        // Mark all live functions
        //

        let import_count = self.import.imports.len();
        let fn_index_min = import_count as u32 + self.code.dead_import_dummy_count;
        let fn_index_max = called_fns.len() as u32;

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
        let live_flags = self.trace_live_functions(
            arena,
            called_fns,
            exported_fns,
            indirect_callees_and_signatures,
            fn_index_min,
            fn_index_max,
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

        // Relocate calls to JS imports
        // This must happen *before* we run dead code elimination on the code section,
        // so that byte offsets in the linking data will still be valid.
        for (new_index, &old_index) in live_import_fns.iter().enumerate() {
            if new_index == old_index {
                continue;
            }
            let sym_index = self
                .linking
                .find_and_reindex_imported_fn(old_index as u32, new_index as u32)
                .unwrap();
            self.reloc_code
                .apply_relocs_u32(&mut self.code.bytes, sym_index, new_index as u32);
        }

        //
        // Code section: Replace dead functions with tiny dummies.
        // Live function indices are unchanged, so no relocations are needed.
        //
        let mut buffer = Vec::with_capacity_in(self.code.bytes.len(), arena);
        self.code.function_count.serialize(&mut buffer);
        for (i, fn_index) in (fn_index_min..fn_index_max).enumerate() {
            if live_flags[fn_index as usize] {
                let code_start = self.code.function_offsets[i] as usize;
                let code_end = if i < self.code.function_offsets.len() - 1 {
                    self.code.function_offsets[i + 1] as usize
                } else {
                    self.code.bytes.len()
                };
                buffer.extend_from_slice(&self.code.bytes[code_start..code_end]);
            } else {
                DUMMY_FUNCTION.serialize(&mut buffer);
            }
        }

        self.code.bytes = buffer;
    }

    fn trace_live_functions<I: Iterator<Item = u32>>(
        &self,
        arena: &'a Bump,
        called_fns: BitVec<usize>,
        exported_fns: I,
        indirect_callees_and_signatures: Vec<'a, (u32, u32)>,
        fn_index_min: u32,
        fn_index_max: u32,
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
        let mut live_flags = BitVec::repeat(false, called_fns.len());
        let mut next_pass_fns = BitVec::repeat(false, called_fns.len());
        let mut current_pass_fns = called_fns;
        for index in exported_fns {
            current_pass_fns.set(index as usize, true);
        }

        while current_pass_fns.count_ones() > 0 {
            // All functions in this pass are live (they have been reached by earlier passes)
            debug_assert_eq!(live_flags.len(), current_pass_fns.len());
            live_flags |= &current_pass_fns;

            // For each live function in the current pass
            for fn_index in current_pass_fns.iter_ones() {
                // Skip JS imports and Roc functions
                if fn_index < fn_index_min as usize || fn_index >= fn_index_max as usize {
                    continue;
                }

                // Find where the function body is
                let offset_index = fn_index - fn_index_min as usize;
                let code_start = self.code.function_offsets[offset_index];
                let code_end = if offset_index < self.code.function_offsets.len() - 1 {
                    self.code.function_offsets[offset_index + 1]
                } else {
                    self.code.bytes.len() as u32
                };

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
                self.reloc_code
                    .apply_relocs_u32(&mut self.code.bytes, sym_index as u32, value);

                sym_index as u32
            })
    }

    /// Linking steps for host-to-app functions like `roc__main_for_host_1_exposed`
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
            self.reloc_code
                .apply_relocs_u32(&mut self.code.bytes, host_sym_index, app_fn_index);

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
                    &mut self.code.bytes,
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

        // There are names available in the import section too, so let's use them!
        // We don't know how the host was compiled, so we might as well just grab all the info we can get.
        // If we end up with duplicate entries, that's OK. The backend will use the first matching entry.
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

/*******************************************************************
 *
 * Common types & utility functions
 *
 *******************************************************************/

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct LocalId(pub u32);

/// Wasm value type. (Rust representation matches Wasm encoding)
#[repr(u8)]
#[derive(PartialEq, Eq, Clone, Copy, Debug)]
pub enum ValueType {
    I32 = 0x7f,
    I64 = 0x7e,
    F32 = 0x7d,
    F64 = 0x7c,
}

impl ValueType {
    pub const VOID: u8 = 0x40;
}

impl Serialize for ValueType {
    fn serialize<T: SerialBuffer>(&self, buffer: &mut T) {
        buffer.append_u8(*self as u8);
    }
}

impl From<u8> for ValueType {
    fn from(x: u8) -> Self {
        match x {
            0x7f => Self::I32,
            0x7e => Self::I64,
            0x7d => Self::F32,
            0x7c => Self::F64,
            _ => internal_error!("Invalid ValueType 0x{:02x}", x),
        }
    }
}

impl From<Value> for ValueType {
    fn from(x: Value) -> Self {
        match x {
            Value::I32(_) => Self::I32,
            Value::I64(_) => Self::I64,
            Value::F32(_) => Self::F32,
            Value::F64(_) => Self::F64,
        }
    }
}

impl Parse<()> for ValueType {
    fn parse(_: (), bytes: &[u8], cursor: &mut usize) -> Result<Self, ParseError> {
        let byte = u8::parse((), bytes, cursor)?;
        Ok(ValueType::from(byte))
    }
}

// A group of local variable declarations
impl Parse<()> for (u32, ValueType) {
    fn parse(_: (), bytes: &[u8], cursor: &mut usize) -> Result<Self, ParseError> {
        let count = u32::parse((), bytes, cursor)?;
        let ty = ValueType::parse((), bytes, cursor)?;
        Ok((count, ty))
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Value {
    I32(i32),
    I64(i64),
    F32(f32),
    F64(f64),
}

impl Value {
    pub fn expect_i32(&self) -> Result<i32, (ValueType, ValueType)> {
        match self {
            Value::I32(x) => Ok(*x),
            _ => Err((ValueType::I32, ValueType::from(*self))),
        }
    }
    pub fn expect_i64(&self) -> Result<i64, (ValueType, ValueType)> {
        match self {
            Value::I64(x) => Ok(*x),
            _ => Err((ValueType::I64, ValueType::from(*self))),
        }
    }
    pub fn expect_f32(&self) -> Result<f32, (ValueType, ValueType)> {
        match self {
            Value::F32(x) => Ok(*x),
            _ => Err((ValueType::F32, ValueType::from(*self))),
        }
    }
    pub fn expect_f64(&self) -> Result<f64, (ValueType, ValueType)> {
        match self {
            Value::F64(x) => Ok(*x),
            _ => Err((ValueType::F64, ValueType::from(*self))),
        }
    }
}

impl From<u32> for Value {
    fn from(x: u32) -> Self {
        Value::I32(i32::from_ne_bytes(x.to_ne_bytes()))
    }
}

impl From<u64> for Value {
    fn from(x: u64) -> Self {
        Value::I64(i64::from_ne_bytes(x.to_ne_bytes()))
    }
}

impl From<i32> for Value {
    fn from(x: i32) -> Self {
        Value::I32(x)
    }
}

impl From<i64> for Value {
    fn from(x: i64) -> Self {
        Value::I64(x)
    }
}

/// Wasm memory alignment for load/store instructions.
/// Rust representation matches Wasm encoding.
/// It's an error to specify alignment higher than the "natural" alignment of the instruction
#[repr(u8)]
#[derive(Clone, Copy, Debug, PartialEq, Eq, PartialOrd)]
pub enum Align {
    Bytes1 = 0,
    Bytes2 = 1,
    Bytes4 = 2,
    Bytes8 = 3,
}

impl Align {
    /// Calculate the largest possible alignment for a load/store at a given stack frame offset
    /// Assumes the stack frame is aligned to at least 8 bytes
    pub fn from_stack_offset(max_align: Align, offset: u32) -> Align {
        if (max_align == Align::Bytes8) && (offset & 7 == 0) {
            return Align::Bytes8;
        }
        if (max_align >= Align::Bytes4) && (offset & 3 == 0) {
            return Align::Bytes4;
        }
        if (max_align >= Align::Bytes2) && (offset & 1 == 0) {
            return Align::Bytes2;
        }
        Align::Bytes1
    }
}

impl From<u32> for Align {
    fn from(x: u32) -> Align {
        match x {
            1 => Align::Bytes1,
            2 => Align::Bytes2,
            4 => Align::Bytes4,
            _ => {
                if x.count_ones() == 1 {
                    Align::Bytes8 // Max value supported by any Wasm instruction
                } else {
                    internal_error!("Cannot align to {} bytes", x);
                }
            }
        }
    }
}

/// Round up to alignment_bytes (which must be a power of 2)
#[macro_export]
macro_rules! round_up_to_alignment {
    ($unaligned: expr, $alignment_bytes: expr) => {
        if $alignment_bytes <= 1 {
            $unaligned
        } else if $alignment_bytes.count_ones() != 1 {
            internal_error!(
                "Cannot align to {} bytes. Not a power of 2.",
                $alignment_bytes
            );
        } else {
            let mut aligned = $unaligned;
            aligned += $alignment_bytes - 1; // if lower bits are non-zero, push it over the next boundary
            aligned &= !$alignment_bytes + 1; // mask with a flag that has upper bits 1, lower bits 0
            aligned
        }
    };
}

/// # dbg_hex
/// display dbg result in hexadecimal `{:#x?}` format.
#[macro_export]
macro_rules! dbg_hex {
    // NOTE: We cannot use `concat!` to make a static string as a format argument
    // of `eprintln!` because `file!` could contain a `{` or
    // `$val` expression could be a block (`{ .. }`), in which case the `eprintln!`
    // will be malformed.
    () => {
        eprintln!("[{}:{}]", file!(), line!());
    };
    ($val:expr $(,)?) => {
        // Use of `match` here is intentional because it affects the lifetimes
        // of temporaries - https://stackoverflow.com/a/48732525/1063961
        match $val {
            tmp => {
                eprintln!("[{}:{}] {} = {:#x?}",
                    file!(), line!(), stringify!($val), &tmp);
                tmp
            }
        }
    };
    ($($val:expr),+ $(,)?) => {
        ($($crate::dbg_hex!($val)),+,)
    };
}

/// Bytes for a dummy function with just a single `unreachable` instruction.
/// Used in dead code elimination to replace unused functions.
const DUMMY_FUNCTION: [u8; 3] = [
    0,                         // number of local variable declarations
    OpCode::UNREACHABLE as u8, // panic if we were wrong to eliminate!
    OpCode::END as u8,         // end of function (required for validation)
];

// TODO: make this an environment variable
pub struct WasmDebugSettings {
    pub skip_dead_code_elim: bool,
}

pub const DEBUG_SETTINGS: WasmDebugSettings = WasmDebugSettings {
    skip_dead_code_elim: false && cfg!(debug_assertions),
};
