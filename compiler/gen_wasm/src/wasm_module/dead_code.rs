use bumpalo::collections::vec::Vec;
use bumpalo::Bump;

use super::opcodes::OpCode;
use super::serialize::{parse_u32_or_panic, SerialBuffer, Serialize, SkipBytes};
use super::{CodeBuilder, ValueType};

#[derive(Debug)]
pub struct DeadCodeMetadata<'a> {
    /// Byte offset where each function body can be found
    code_offsets: Vec<'a, u32>,
    /// Vector with one entry per *call*, containing the called function's index
    calls: Vec<'a, u32>,
    /// Vector with one entry per *function*, indicating its offset in `calls`
    calls_offsets: Vec<'a, u32>,
    /// Return types of each function (for making almost-empty dummy replacements)
    ret_types: Vec<'a, Option<ValueType>>,
}

impl<'a> DeadCodeMetadata<'a> {
    pub fn new(arena: &'a Bump, func_count: usize) -> Self {
        DeadCodeMetadata {
            code_offsets: Vec::with_capacity_in(func_count, arena),
            ret_types: Vec::with_capacity_in(func_count, arena),
            calls: Vec::with_capacity_in(2 * func_count, arena),
            calls_offsets: Vec::with_capacity_in(1 + func_count, arena),
        }
    }
}

/// Parse a Code section, collecting metadata that we can use to figure out
/// which functions are actually called, and which are not.
/// This would normally be done in a linker optimisation, but we want to be able to
/// use this backend without a linker.
pub fn parse_dead_code_metadata<'a>(
    arena: &'a Bump,
    func_count: u32,
    code_section_body: &[u8],
    ret_types: Vec<'a, Option<ValueType>>,
    signature_ids: Vec<'a, u32>,
) -> DeadCodeMetadata<'a> {
    let mut metadata = DeadCodeMetadata::new(arena, func_count as usize);
    metadata
        .ret_types
        .extend(signature_ids.iter().map(|sig| ret_types[*sig as usize]));

    let mut cursor: usize = 0;
    while cursor < code_section_body.len() {
        metadata.code_offsets.push(cursor as u32);
        metadata.calls_offsets.push(metadata.calls.len() as u32);

        let func_size = parse_u32_or_panic(code_section_body, &mut cursor);
        let func_end = cursor + func_size as usize;

        // Local variable declarations
        let local_groups_count = parse_u32_or_panic(code_section_body, &mut cursor);
        for _ in 0..local_groups_count {
            parse_u32_or_panic(code_section_body, &mut cursor);
            cursor += 1; // ValueType
        }

        // Instructions
        while cursor < func_end {
            let opcode_byte: u8 = code_section_body[cursor];
            if opcode_byte == OpCode::CALL as u8 {
                cursor += 1;
                let call_index = parse_u32_or_panic(code_section_body, &mut cursor);
                metadata.calls.push(call_index as u32);
            } else {
                OpCode::skip_bytes(code_section_body, &mut cursor);
            }
        }
    }

    // Extra entries to mark the end of the last function
    metadata.code_offsets.push(cursor as u32);
    metadata.calls_offsets.push(metadata.calls.len() as u32);

    metadata
}

/// Trace the dependencies of a list of functions
/// We've already collected metadata saying which functions call each other
/// Now we need to trace the dependency graphs of a specific subset of them
/// Result is the full set of builtins and platform functions used in the app.
/// The rest are "dead code" and can be eliminated.
pub fn trace_function_deps<'a, Indices: IntoIterator<Item = u32>>(
    arena: &'a Bump,
    metadata: &DeadCodeMetadata<'a>,
    called_from_app: Indices,
) -> Vec<'a, u32> {
    let mut live_fn_indices: Vec<'a, u32> = Vec::with_capacity_in(metadata.calls.len(), arena);
    live_fn_indices.extend(called_from_app);

    let num_funcs = metadata.calls_offsets.len();

    // Current batch of functions whose call graphs we want to trace
    let mut current_trace: Vec<'a, u32> = Vec::with_capacity_in(num_funcs, arena);
    current_trace.clone_from(&live_fn_indices);

    // The next batch (don't want to modify the current one while we're iterating over it!)
    let mut next_trace: Vec<'a, u32> = Vec::with_capacity_in(num_funcs, arena);

    // Fast lookup for what's already traced so we don't need to do it again
    let mut already_traced: Vec<'a, bool> = Vec::from_iter_in((0..num_funcs).map(|_| false), arena);

    loop {
        live_fn_indices.extend_from_slice(&current_trace);

        for func_idx in current_trace.iter() {
            let i = *func_idx as usize;
            already_traced[i] = true;
            let calls_start = metadata.calls_offsets[i] as usize;
            let calls_end = metadata.calls_offsets[i + 1] as usize;
            let called_indices: &[u32] = &metadata.calls[calls_start..calls_end];
            for called_idx in called_indices {
                if !already_traced[*called_idx as usize] {
                    next_trace.push(*called_idx);
                }
            }
        }
        if next_trace.is_empty() {
            break;
        }
        current_trace.clone_from(&next_trace);
        next_trace.clear();
    }

    if true {
        println!("Hey Brian, don't forget to remove this debug code");
        let unsorted_len = live_fn_indices.len();
        live_fn_indices.dedup();
        debug_assert!(unsorted_len == live_fn_indices.len());
    }

    live_fn_indices
}

/// Create a set of minimum-size dummy functions for each possible return type
fn create_dummy_functions(arena: &Bump) -> [Vec<'_, u8>; 5] {
    let mut code_builder_i32 = CodeBuilder::new(arena);
    code_builder_i32.i32_const(0);

    let mut code_builder_i64 = CodeBuilder::new(arena);
    code_builder_i64.i64_const(0);

    let mut code_builder_f32 = CodeBuilder::new(arena);
    code_builder_f32.f32_const(0.0);

    let mut code_builder_f64 = CodeBuilder::new(arena);
    code_builder_f64.f64_const(0.0);

    let mut code_builder_nil = CodeBuilder::new(arena);

    code_builder_i32.build_fn_header_and_footer(&[], 0, None);
    code_builder_i64.build_fn_header_and_footer(&[], 0, None);
    code_builder_f32.build_fn_header_and_footer(&[], 0, None);
    code_builder_f64.build_fn_header_and_footer(&[], 0, None);
    code_builder_nil.build_fn_header_and_footer(&[], 0, None);

    let capacity = code_builder_f64.size();
    let mut dummy_i32 = Vec::with_capacity_in(capacity, arena);
    let mut dummy_i64 = Vec::with_capacity_in(capacity, arena);
    let mut dummy_f32 = Vec::with_capacity_in(capacity, arena);
    let mut dummy_f64 = Vec::with_capacity_in(capacity, arena);
    let mut dummy_nil = Vec::with_capacity_in(capacity, arena);

    code_builder_i32.serialize(&mut dummy_i32);
    code_builder_i64.serialize(&mut dummy_i64);
    code_builder_f32.serialize(&mut dummy_f32);
    code_builder_f64.serialize(&mut dummy_f64);
    code_builder_nil.serialize(&mut dummy_nil);

    [dummy_i32, dummy_i64, dummy_f32, dummy_f64, dummy_nil]
}

/// Copy used functions from an external module into our Code section
/// Replace unused functions with very small dummies, to avoid changing any indices
pub fn copy_live_and_replace_dead<'a, T: SerialBuffer>(
    arena: &'a Bump,
    buffer: &mut T,
    metadata: &DeadCodeMetadata<'a>,
    external_code: &[u8],
    live_ext_fn_indices: &'a mut [u32],
) {
    live_ext_fn_indices.sort_unstable();

    let [dummy_i32, dummy_i64, dummy_f32, dummy_f64, dummy_nil] = create_dummy_functions(arena);

    let mut prev = 0;
    for live32 in live_ext_fn_indices.iter() {
        let live = *live32 as usize;

        // Replace dead functions with the minimal code body that will pass validation checks
        for dead in prev..live {
            let dummy_bytes = match metadata.ret_types[dead] {
                Some(ValueType::I32) => &dummy_i32,
                Some(ValueType::I64) => &dummy_i64,
                Some(ValueType::F32) => &dummy_f32,
                Some(ValueType::F64) => &dummy_f64,
                None => &dummy_nil,
            };
            buffer.append_slice(dummy_bytes);
        }

        // Copy the body of the live function from the external module
        let live_body_start = metadata.code_offsets[live] as usize;
        let live_body_end = metadata.code_offsets[live + 1] as usize;
        buffer.append_slice(&external_code[live_body_start..live_body_end]);

        prev = live + 1;
    }
}
