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
    pub fn new(arena: &'a Bump, import_fn_count: u32, fn_count: u32) -> Self {
        let capacity = (import_fn_count + fn_count) as usize;

        let mut code_offsets = Vec::with_capacity_in(capacity, arena);
        let mut ret_types = Vec::with_capacity_in(capacity, arena);
        let calls = Vec::with_capacity_in(2 * capacity, arena);
        let mut calls_offsets = Vec::with_capacity_in(1 + capacity, arena);

        // Imported functions have zero code length and no calls
        code_offsets.extend(std::iter::repeat(0).take(import_fn_count as usize));
        calls_offsets.extend(std::iter::repeat(0).take(import_fn_count as usize));

        // We don't care about import return types
        // Return types are for replacing dead functions with dummies, which doesn't apply to imports
        ret_types.extend(std::iter::repeat(None).take(import_fn_count as usize));

        DeadCodeMetadata {
            code_offsets,
            ret_types,
            calls,
            calls_offsets,
        }
    }
}

/// Parse a Code section, collecting metadata that we can use to figure out
/// which functions are actually called, and which are not.
/// This would normally be done in a linker optimisation, but we want to be able to
/// use this backend without a linker.
pub fn parse_dead_code_metadata<'a>(
    arena: &'a Bump,
    fn_count: u32,
    code_section_body: &[u8],
    signature_ret_types: Vec<'a, Option<ValueType>>,
    internal_fn_sig_ids: Vec<'a, u32>,
    import_fn_count: u32,
) -> DeadCodeMetadata<'a> {
    let mut metadata = DeadCodeMetadata::new(arena, import_fn_count, fn_count);
    metadata.ret_types.extend(
        internal_fn_sig_ids
            .iter()
            .map(|sig| signature_ret_types[*sig as usize]),
    );

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
    let num_funcs = metadata.ret_types.len();

    // All functions that get called from the app, directly or indirectly
    let mut live_fn_indices = Vec::with_capacity_in(num_funcs, arena);

    // Current & next batch of functions whose call graphs we want to trace through the metadata
    // (2 separate vectors so that we're not iterating over the same one we're changing)
    // If the max call depth is N then we will do N traces or less
    let mut current_trace = Vec::with_capacity_in(num_funcs, arena);
    current_trace.extend(called_from_app);
    let mut next_trace = Vec::with_capacity_in(num_funcs, arena);

    // Fast per-function lookup table to see if its dependencies have already been traced
    let mut already_traced = Vec::from_iter_in(std::iter::repeat(false).take(num_funcs), arena);

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
    import_fn_count: u32,
    mut live_ext_fn_indices: Vec<'a, u32>,
) {
    live_ext_fn_indices.sort_unstable();

    let [dummy_i32, dummy_i64, dummy_f32, dummy_f64, dummy_nil] = create_dummy_functions(arena);

    let mut prev = import_fn_count as usize;
    for live32 in live_ext_fn_indices.into_iter() {
        if live32 < import_fn_count {
            continue;
        }

        let live = live32 as usize;

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

    let num_preloaded_fns = metadata.ret_types.len();
    // Replace dead functions with the minimal code body that will pass validation checks
    for dead in prev..num_preloaded_fns {
        if dead < import_fn_count as usize {
            continue;
        }
        let ret_type = metadata.ret_types[dead];
        let dummy_bytes = match ret_type {
            Some(ValueType::I32) => &dummy_i32,
            Some(ValueType::I64) => &dummy_i64,
            Some(ValueType::F32) => &dummy_f32,
            Some(ValueType::F64) => &dummy_f64,
            None => &dummy_nil,
        };
        buffer.append_slice(dummy_bytes);
    }
}
