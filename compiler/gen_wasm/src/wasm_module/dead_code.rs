use bumpalo::collections::vec::Vec;
use bumpalo::Bump;

use super::opcodes::OpCode;
use super::serialize::{parse_u32_or_panic, SerialBuffer, Serialize, SkipBytes};
use super::CodeBuilder;

/*

DEAD CODE ELIMINATION

Or, more specifically, "dead function replacement"

- On pre-loading the object file:
    - Analyse its call graph by finding all `call` instructions in the Code section,
      and checking which function index they refer to. Store this in a `PreloadsCallGraph`
- While compiling Roc code:
    - Run the backend as usual, adding more data into various sections of the Wasm module
    - Whenever a call to a builtin or platform function is made, record its index.
      These are the "live" preloaded functions that we are not allowed to eliminate.
- Call graph analysis:
    - Starting with the live preloaded functions, trace their call graphs using the info we
      collected earlier in `PreloadsCallGraph`. Mark all function indices in the call graph as "live".
- Dead function replacement:
    - We actually don't want to just *delete* dead functions, because that would change the indices
      of the live functions, invalidating all references to them, such as `call` instructions.
    - Instead, during serialization, we replace its body with a single `unreachable` instruction
*/

#[derive(Debug)]
pub struct PreloadsCallGraph<'a> {
    num_preloads: usize,
    /// Byte offset where each function body can be found
    code_offsets: Vec<'a, u32>,
    /// Vector with one entry per *call*, containing the called function's index
    calls: Vec<'a, u32>,
    /// Vector with one entry per *function*, indicating its offset in `calls`
    calls_offsets: Vec<'a, u32>,
}

impl<'a> PreloadsCallGraph<'a> {
    pub fn new(arena: &'a Bump, import_fn_count: u32, fn_count: u32) -> Self {
        let num_preloads = (import_fn_count + fn_count) as usize;

        let mut code_offsets = Vec::with_capacity_in(num_preloads, arena);
        let calls = Vec::with_capacity_in(2 * num_preloads, arena);
        let mut calls_offsets = Vec::with_capacity_in(1 + num_preloads, arena);

        // Imported functions have zero code length and no calls
        code_offsets.extend(std::iter::repeat(0).take(import_fn_count as usize));
        calls_offsets.extend(std::iter::repeat(0).take(import_fn_count as usize));

        PreloadsCallGraph {
            num_preloads,
            code_offsets,
            calls,
            calls_offsets,
        }
    }
}

/// Parse a Code section, collecting metadata that we can use to figure out
/// which functions are actually called, and which are not.
/// This would normally be done in a linker optimisation, but we want to be able to
/// use this backend without a linker.
pub fn parse_preloads_call_graph<'a>(
    arena: &'a Bump,
    fn_count: u32,
    code_section_body: &[u8],
    import_fn_count: u32,
) -> PreloadsCallGraph<'a> {
    let mut call_graph = PreloadsCallGraph::new(arena, import_fn_count, fn_count);

    // Iterate over the bytes of the Code section
    let mut cursor: usize = 0;
    while cursor < code_section_body.len() {
        // Record the start of a function
        call_graph.code_offsets.push(cursor as u32);
        call_graph.calls_offsets.push(call_graph.calls.len() as u32);

        let func_size = parse_u32_or_panic(code_section_body, &mut cursor);
        let func_end = cursor + func_size as usize;

        // Skip over local variable declarations
        let local_groups_count = parse_u32_or_panic(code_section_body, &mut cursor);
        for _ in 0..local_groups_count {
            parse_u32_or_panic(code_section_body, &mut cursor);
            cursor += 1; // ValueType
        }

        // Parse `call` instructions and skip over all other instructions
        while cursor < func_end {
            let opcode_byte: u8 = code_section_body[cursor];
            if opcode_byte == OpCode::CALL as u8 {
                cursor += 1;
                let call_index = parse_u32_or_panic(code_section_body, &mut cursor);
                call_graph.calls.push(call_index as u32);
            } else {
                OpCode::skip_bytes(code_section_body, &mut cursor);
            }
        }
    }

    // Extra entries to mark the end of the last function
    call_graph.code_offsets.push(cursor as u32);
    call_graph.calls_offsets.push(call_graph.calls.len() as u32);

    call_graph
}

/// Trace the dependencies of a list of functions
/// We've already collected call_graph saying which functions call each other
/// Now we need to trace the dependency graphs of a specific subset of them
/// Result is the full set of builtins and platform functions used in the app.
/// The rest are "dead code" and can be eliminated.
pub fn trace_call_graph<'a, Indices: IntoIterator<Item = u32>>(
    arena: &'a Bump,
    call_graph: &PreloadsCallGraph<'a>,
    exported_fns: &[u32],
    called_from_app: Indices,
) -> Vec<'a, u32> {
    let num_preloads = call_graph.num_preloads;

    // All functions that get called from the app, directly or indirectly
    let mut live_fn_indices = Vec::with_capacity_in(num_preloads, arena);

    // Current & next batch of functions whose call graphs we want to trace through the call_graph
    // (2 separate vectors so that we're not iterating over the same one we're changing)
    // If the max call depth is N then we will do N traces or less
    let mut current_trace = Vec::with_capacity_in(num_preloads, arena);
    let mut next_trace = Vec::with_capacity_in(num_preloads, arena);

    // Start with preloaded functions called from the app or exported directly to Wasm host
    current_trace.extend(called_from_app);
    current_trace.extend(
        exported_fns
            .iter()
            .filter(|idx| **idx < num_preloads as u32),
    );
    current_trace.sort_unstable();
    current_trace.dedup();

    // Fast per-function lookup table to see if its dependencies have already been traced
    let mut already_traced = Vec::from_iter_in(std::iter::repeat(false).take(num_preloads), arena);

    loop {
        live_fn_indices.extend_from_slice(&current_trace);

        for func_idx in current_trace.iter() {
            let i = *func_idx as usize;
            already_traced[i] = true;
            let calls_start = call_graph.calls_offsets[i] as usize;
            let calls_end = call_graph.calls_offsets[i + 1] as usize;
            let called_indices: &[u32] = &call_graph.calls[calls_start..calls_end];
            for called_idx in called_indices {
                if !already_traced[*called_idx as usize] {
                    next_trace.push(*called_idx);
                }
            }
        }
        if next_trace.is_empty() {
            break;
        }
        next_trace.sort_unstable();
        next_trace.dedup();

        current_trace.clone_from(&next_trace);
        next_trace.clear();
    }

    live_fn_indices
}

/// Copy used functions from preloaded object file into our Code section
/// Replace unused functions with very small dummies, to avoid changing any indices
pub fn copy_preloads_shrinking_dead_fns<'a, T: SerialBuffer>(
    arena: &'a Bump,
    buffer: &mut T,
    call_graph: &PreloadsCallGraph<'a>,
    external_code: &[u8],
    import_fn_count: u32,
    mut live_preload_indices: Vec<'a, u32>,
) {
    let preload_idx_start = import_fn_count as usize;

    // Create a dummy function with just a single `unreachable` instruction
    let mut dummy_builder = CodeBuilder::new(arena);
    dummy_builder.unreachable_();
    dummy_builder.build_fn_header_and_footer(&[], 0, None);
    let mut dummy_bytes = Vec::with_capacity_in(dummy_builder.size(), arena);
    dummy_builder.serialize(&mut dummy_bytes);

    live_preload_indices.sort_unstable();
    live_preload_indices.dedup();

    let mut live_iter = live_preload_indices.iter();
    let mut next_live_idx = live_iter.next();
    for i in preload_idx_start..call_graph.num_preloads {
        match next_live_idx {
            Some(live) if *live as usize == i => {
                next_live_idx = live_iter.next();
                let live_body_start = call_graph.code_offsets[i] as usize;
                let live_body_end = call_graph.code_offsets[i + 1] as usize;
                buffer.append_slice(&external_code[live_body_start..live_body_end]);
            }
            _ => {
                buffer.append_slice(&dummy_bytes);
            }
        }
    }
}
