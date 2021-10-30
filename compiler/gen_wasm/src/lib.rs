mod backend;
pub mod code_builder;
pub mod from_wasm32_memory;
mod layout;
pub mod module_builder;
pub mod serialize;
mod storage;

#[allow(dead_code)]
mod opcodes;

use bumpalo::{self, collections::Vec, Bump};
use parity_wasm::builder;

use parity_wasm::elements::{Instruction, Internal, Module, Section};
use roc_collections::all::{MutMap, MutSet};
use roc_module::symbol::{Interns, Symbol};
use roc_mono::ir::{Proc, ProcLayout};
use roc_mono::layout::LayoutIds;

use crate::backend::WasmBackend;
use crate::code_builder::{Align, CodeBuilder, ValueType};
use crate::module_builder::{
    LinkingSection, LinkingSubSection, RelocationSection, SectionId, SymInfo,
};
use crate::serialize::{SerialBuffer, Serialize};

const PTR_SIZE: u32 = 4;
const PTR_TYPE: ValueType = ValueType::I32;

pub const STACK_POINTER_GLOBAL_ID: u32 = 0;
pub const FRAME_ALIGNMENT_BYTES: i32 = 16;

/// Code section ID from spec
/// https://webassembly.github.io/spec/core/binary/modules.html#sections
pub const CODE_SECTION_ID: u8 = 10;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct LocalId(pub u32);

pub struct Env<'a> {
    pub arena: &'a Bump,
    pub interns: Interns,
    pub exposed_to_host: MutSet<Symbol>,
}

pub fn build_module<'a>(
    env: &'a Env,
    procedures: MutMap<(Symbol, ProcLayout<'a>), Proc<'a>>,
) -> Result<std::vec::Vec<u8>, String> {
    let (builder, code_section_bytes) = build_module_help(env, procedures)?;
    let mut module = builder.build();
    replace_code_section(&mut module, code_section_bytes);

    module
        .into_bytes()
        .map_err(|e| -> String { format!("Error serialising Wasm module {:?}", e) })
}

pub fn build_module_help<'a>(
    env: &'a Env,
    procedures: MutMap<(Symbol, ProcLayout<'a>), Proc<'a>>,
) -> Result<(builder::ModuleBuilder, std::vec::Vec<u8>), String> {
    let proc_symbols = Vec::from_iter_in(procedures.keys().map(|(sym, _)| *sym), env.arena);
    let mut backend = WasmBackend::new(env, &proc_symbols);
    let mut layout_ids = LayoutIds::default();
    let mut symbol_table_entries = Vec::with_capacity_in(procedures.len(), env.arena);

    for (i, ((sym, layout), proc)) in procedures.into_iter().enumerate() {
        let proc_name = LayoutIds::default()
            .get(proc.name, &proc.ret_layout)
            .to_symbol_string(proc.name, &env.interns);
        symbol_table_entries.push(SymInfo::for_function(i as u32, proc_name));

        let function_index = backend.build_proc(proc, sym)?;

        if env.exposed_to_host.contains(&sym) {
            let fn_name = layout_ids
                .get_toplevel(sym, &layout)
                .to_symbol_string(sym, &env.interns);

            let export = builder::export()
                .field(fn_name.as_str())
                .with_internal(Internal::Function(function_index))
                .build();

            backend.module_builder.push_export(export);
        }
    }

    // Update code section length
    let inner_length = (backend.code_section_bytes.len() - 5) as u32;
    backend
        .code_section_bytes
        .overwrite_padded_u32(0, inner_length);

    // linking metadata section
    let mut linking_section_bytes = std::vec::Vec::with_capacity(symbol_table_entries.len() * 20);
    let linking_section = LinkingSection {
        subsections: bumpalo::vec![in env.arena;
            LinkingSubSection::SymbolTable(symbol_table_entries)
        ],
    };
    linking_section.serialize(&mut linking_section_bytes);
    backend.module_builder = backend.module_builder.with_section(Section::Unparsed {
        id: SectionId::Custom as u8,
        payload: linking_section_bytes,
    });

    // Code relocations
    let code_reloc_section = RelocationSection {
        name: "reloc.CODE".to_string(),
        entries: &backend.code_relocations,
    };

    let mut code_reloc_section_bytes = std::vec::Vec::with_capacity(256);
    code_reloc_section.serialize(&mut code_reloc_section_bytes);

    // Must come after linking section
    backend.module_builder = backend.module_builder.with_section(Section::Unparsed {
        id: SectionId::Custom as u8,
        payload: code_reloc_section_bytes,
    });

    const MIN_MEMORY_SIZE_KB: u32 = 1024;
    const PAGE_SIZE_KB: u32 = 64;

    let memory = builder::MemoryBuilder::new()
        .with_min(MIN_MEMORY_SIZE_KB / PAGE_SIZE_KB)
        .build();
    backend.module_builder.push_memory(memory);
    let memory_export = builder::export()
        .field("memory")
        .with_internal(Internal::Memory(0))
        .build();
    backend.module_builder.push_export(memory_export);

    let stack_pointer_global = builder::global()
        .with_type(parity_wasm::elements::ValueType::I32)
        .mutable()
        .init_expr(Instruction::I32Const((MIN_MEMORY_SIZE_KB * 1024) as i32))
        .build();
    backend.module_builder.push_global(stack_pointer_global);

    Ok((backend.module_builder, backend.code_section_bytes))
}

/// Replace parity-wasm's code section with our own handmade one
pub fn replace_code_section(module: &mut Module, code_section_bytes: std::vec::Vec<u8>) {
    let sections = module.sections_mut();

    let code_section_index = sections
        .iter()
        .position(|s| match s {
            Section::Code(_) => true,
            _ => false,
        })
        .unwrap();

    sections[code_section_index] = Section::Unparsed {
        id: SectionId::Code as u8,
        payload: code_section_bytes,
    };
}

pub struct CopyMemoryConfig {
    from_ptr: LocalId,
    from_offset: u32,
    to_ptr: LocalId,
    to_offset: u32,
    size: u32,
    alignment_bytes: u32,
}

pub fn copy_memory(code_builder: &mut CodeBuilder, config: CopyMemoryConfig) {
    if config.from_ptr == config.to_ptr && config.from_offset == config.to_offset {
        return;
    }

    let alignment = Align::from(config.alignment_bytes);
    let mut i = 0;
    while config.size - i >= 8 {
        code_builder.get_local(config.to_ptr);
        code_builder.get_local(config.from_ptr);
        code_builder.i64_load(alignment, i + config.from_offset);
        code_builder.i64_store(alignment, i + config.to_offset);
        i += 8;
    }
    if config.size - i >= 4 {
        code_builder.get_local(config.to_ptr);
        code_builder.get_local(config.from_ptr);
        code_builder.i32_load(alignment, i + config.from_offset);
        code_builder.i32_store(alignment, i + config.to_offset);
        i += 4;
    }
    while config.size - i > 0 {
        code_builder.get_local(config.to_ptr);
        code_builder.get_local(config.from_ptr);
        code_builder.i32_load8_u(alignment, i + config.from_offset);
        code_builder.i32_store8(alignment, i + config.to_offset);
        i += 1;
    }
}

/// Round up to alignment_bytes (which must be a power of 2)
pub fn round_up_to_alignment(unaligned: i32, alignment_bytes: i32) -> i32 {
    debug_assert!(alignment_bytes.count_ones() == 1);
    let mut aligned = unaligned;
    aligned += alignment_bytes - 1; // if lower bits are non-zero, push it over the next boundary
    aligned &= -alignment_bytes; // mask with a flag that has upper bits 1, lower bits 0
    aligned
}

pub fn debug_panic<E: std::fmt::Debug>(error: E) {
    panic!("{:?}", error);
}
