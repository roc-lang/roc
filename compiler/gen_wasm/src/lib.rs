mod backend;
pub mod from_wasm32_memory;
mod layout;
mod storage;

#[allow(dead_code)]
pub mod code_builder;

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
    let (builder, code_section_bytes, _) = build_module_help(env, procedures)?;
    let mut module = builder.build();
    replace_code_section(&mut module, code_section_bytes);

    module
        .into_bytes()
        .map_err(|e| -> String { format!("Error serialising Wasm module {:?}", e) })
}

pub fn build_module_help<'a>(
    env: &'a Env,
    procedures: MutMap<(Symbol, ProcLayout<'a>), Proc<'a>>,
) -> Result<(builder::ModuleBuilder, std::vec::Vec<u8>, u32), String> {
    let proc_symbols = Vec::from_iter_in(procedures.keys().map(|(sym, _)| *sym), env.arena);
    let mut backend = WasmBackend::new(env, proc_symbols);
    let mut layout_ids = LayoutIds::default();

    let mut main_function_index = None;

    for ((sym, layout), proc) in procedures.into_iter() {
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
            main_function_index = Some(function_index);
        }
    }

    main_function_index.ok_or(format!("No functions exposed to host"))?;

    // Update code section length
    let inner_length = (backend.code_section_bytes.len() - 5) as u32;
    overwrite_padded_u32(&mut backend.code_section_bytes[0..5], inner_length);

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

    Ok((
        backend.module_builder,
        backend.code_section_bytes,
        main_function_index.unwrap(),
    ))
}

/// Replace parity-wasm's code section with our own handmade one
pub fn replace_code_section(module: &mut Module, code_section_bytes: std::vec::Vec<u8>) {
    let sections = module.sections_mut();
    let mut code_section_index = usize::MAX;
    for (i, s) in sections.iter().enumerate() {
        if let Section::Code(_) = s {
            code_section_index = i;
        }
    }
    sections[code_section_index] = Section::Unparsed {
        id: CODE_SECTION_ID,
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

/// Write an unsigned value into the provided buffer in LEB-128 format, returning byte length
///
/// All integers in Wasm are variable-length encoded, which saves space for small values.
/// The most significant bit indicates "more bytes are coming", and the other 7 are payload.
macro_rules! encode_uleb128 {
    ($name: ident, $ty: ty) => {
        pub fn $name<'a>(buffer: &mut Vec<'a, u8>, value: $ty) -> usize {
            let mut x = value;
            let start_len = buffer.len();
            while x >= 0x80 {
                buffer.push(0x80 | ((x & 0x7f) as u8));
                x >>= 7;
            }
            buffer.push(x as u8);
            buffer.len() - start_len
        }
    };
}

encode_uleb128!(encode_u32, u32);
encode_uleb128!(encode_u64, u64);

/// Write a *signed* value into the provided buffer in LEB-128 format, returning byte length
macro_rules! encode_sleb128 {
    ($name: ident, $ty: ty) => {
        pub fn $name<'a>(buffer: &mut Vec<'a, u8>, value: $ty) -> usize {
            let mut x = value;
            let start_len = buffer.len();
            loop {
                let byte = (x & 0x7f) as u8;
                x >>= 7;
                let byte_is_negative = (byte & 0x40) != 0;
                if ((x == 0 && !byte_is_negative) || (x == -1 && byte_is_negative)) {
                    buffer.push(byte);
                    break;
                }
                buffer.push(byte | 0x80);
            }
            buffer.len() - start_len
        }
    };
}

encode_sleb128!(encode_i32, i32);
encode_sleb128!(encode_i64, i64);

/// No LEB encoding, and always little-endian regardless of compiler host.
macro_rules! encode_float {
    ($name: ident, $ty: ty) => {
        pub fn $name<'a>(buffer: &mut Vec<'a, u8>, value: $ty) {
            let mut x = value.to_bits();
            let size = std::mem::size_of::<$ty>();
            for _ in 0..size {
                buffer.push((x & 0xff) as u8);
                x >>= 8;
            }
        }
    };
}

encode_float!(encode_f32, f32);
encode_float!(encode_f64, f64);

/// Overwrite a LEB-128 encoded u32 value, padded to maximum length (5 bytes)
///
/// We need some fixed-length values so we can overwrite them without moving all following bytes.
/// Many parts of the binary format are prefixed with their length, which we only know at the end.
/// And relocation values get updated during linking.
/// This can help us to avoid copies, which is good for speed, but there's a tradeoff with output size.
///
/// The value 3 is encoded as 0x83 0x80 0x80 0x80 0x00.
/// https://github.com/WebAssembly/tool-conventions/blob/main/Linking.md#relocation-sections
pub fn overwrite_padded_u32(buffer: &mut [u8], value: u32) {
    let mut x = value;
    for byte in buffer.iter_mut().take(4) {
        *byte = 0x80 | ((x & 0x7f) as u8);
        x >>= 7;
    }
    buffer[4] = x as u8;
}

pub fn encode_padded_u32<'a>(buffer: &mut Vec<'a, u8>, value: u32) {
    let old_len = buffer.len();
    let new_len = old_len + 5;
    buffer.resize(new_len, 0);
    overwrite_padded_u32(&mut buffer[old_len..new_len], value);
}

#[cfg(test)]
mod tests {
    use super::*;
    use bumpalo::{self, collections::Vec, Bump};

    fn help_u32<'a>(arena: &'a Bump, value: u32) -> Vec<'a, u8> {
        let mut buffer = Vec::with_capacity_in(5, arena);
        encode_u32(&mut buffer, value);
        buffer
    }

    #[test]
    fn test_encode_u32() {
        let a = &Bump::new();
        assert_eq!(help_u32(a, 0), &[0]);
        assert_eq!(help_u32(a, 64), &[64]);
        assert_eq!(help_u32(a, 0x7f), &[0x7f]);
        assert_eq!(help_u32(a, 0x80), &[0x80, 0x01]);
        assert_eq!(help_u32(a, 0x3fff), &[0xff, 0x7f]);
        assert_eq!(help_u32(a, 0x4000), &[0x80, 0x80, 0x01]);
        assert_eq!(help_u32(a, u32::MAX), &[0xff, 0xff, 0xff, 0xff, 0x0f]);
    }

    fn help_u64<'a>(arena: &'a Bump, value: u64) -> Vec<'a, u8> {
        let mut buffer = Vec::with_capacity_in(10, arena);
        encode_u64(&mut buffer, value);
        buffer
    }

    #[test]
    fn test_encode_u64() {
        let a = &Bump::new();
        assert_eq!(help_u64(a, 0), &[0]);
        assert_eq!(help_u64(a, 64), &[64]);
        assert_eq!(help_u64(a, 0x7f), &[0x7f]);
        assert_eq!(help_u64(a, 0x80), &[0x80, 0x01]);
        assert_eq!(help_u64(a, 0x3fff), &[0xff, 0x7f]);
        assert_eq!(help_u64(a, 0x4000), &[0x80, 0x80, 0x01]);
        assert_eq!(
            help_u64(a, u64::MAX),
            &[0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x01],
        );
    }

    fn help_i32<'a>(arena: &'a Bump, value: i32) -> Vec<'a, u8> {
        let mut buffer = Vec::with_capacity_in(5, arena);
        encode_i32(&mut buffer, value);
        buffer
    }

    #[test]
    fn test_encode_i32() {
        let a = &Bump::new();
        assert_eq!(help_i32(a, 0), &[0]);
        assert_eq!(help_i32(a, 1), &[1]);
        assert_eq!(help_i32(a, -1), &[0x7f]);
        assert_eq!(help_i32(a, 63), &[63]);
        assert_eq!(help_i32(a, 64), &[0xc0, 0x0]);
        assert_eq!(help_i32(a, -64), &[0x40]);
        assert_eq!(help_i32(a, -65), &[0xbf, 0x7f]);
        assert_eq!(help_i32(a, i32::MAX), &[0xff, 0xff, 0xff, 0xff, 0x07]);
        assert_eq!(help_i32(a, i32::MIN), &[0x80, 0x80, 0x80, 0x80, 0x78]);
    }

    fn help_i64<'a>(arena: &'a Bump, value: i64) -> Vec<'a, u8> {
        let mut buffer = Vec::with_capacity_in(10, arena);
        encode_i64(&mut buffer, value);
        buffer
    }

    #[test]
    fn test_encode_i64() {
        let a = &Bump::new();
        assert_eq!(help_i64(a, 0), &[0]);
        assert_eq!(help_i64(a, 1), &[1]);
        assert_eq!(help_i64(a, -1), &[0x7f]);
        assert_eq!(help_i64(a, 63), &[63]);
        assert_eq!(help_i64(a, 64), &[0xc0, 0x0]);
        assert_eq!(help_i64(a, -64), &[0x40]);
        assert_eq!(help_i64(a, -65), &[0xbf, 0x7f]);
        assert_eq!(
            help_i64(a, i64::MAX),
            &[0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0x00],
        );
        assert_eq!(
            help_i64(a, i64::MIN),
            &[0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x80, 0x7f],
        );
    }

    #[test]
    fn test_overwrite_u32_padded() {
        let mut buffer = [0, 0, 0, 0, 0];

        overwrite_padded_u32(&mut buffer, u32::MAX);
        assert_eq!(buffer, [0xff, 0xff, 0xff, 0xff, 0x0f]);

        overwrite_padded_u32(&mut buffer, 0);
        assert_eq!(buffer, [0x80, 0x80, 0x80, 0x80, 0x00]);

        overwrite_padded_u32(&mut buffer, 127);
        assert_eq!(buffer, [0xff, 0x80, 0x80, 0x80, 0x00]);

        overwrite_padded_u32(&mut buffer, 128);
        assert_eq!(buffer, [0x80, 0x81, 0x80, 0x80, 0x00]);
    }
}
