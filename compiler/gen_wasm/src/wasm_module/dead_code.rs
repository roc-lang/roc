
#[derive(Debug)]
pub struct DeadCodeMetadata<'a> {
    /// Byte offset (in the module) where each function body can be found
    code_offsets: Vec<'a, u32>,
    /// Vector with one entry per *call*, containing the called function's index
    calls: Vec<'a, u32>,
    /// Vector with one entry per *function*, indicating its offset in `calls`
    calls_offsets: Vec<'a, u32>,
    /// Return types of each function (for making almost-empty dummy replacements)
    ret_types: Vec<'a, u8>,
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
    module_bytes: &[u8],
    cursor: &mut usize,
) -> DeadCodeMetadata<'a> {
    if module_bytes[*cursor] != SectionId::Code as u8 {
        internal_error!("Expected Code section in object file at offset {}", *cursor);
    }
    *cursor += 1;

    let section_size = parse_u32_or_panic(module_bytes, cursor);
    let count_start = *cursor;
    let section_end = count_start + section_size as usize;
    let func_count = parse_u32_or_panic(module_bytes, cursor);

    let mut metadata = DeadCodeMetadata::new(arena, func_count as usize);

    while *cursor < section_end {
        metadata.code_offsets.push(*cursor as u32);
        metadata.calls_offsets.push(metadata.calls.len() as u32);

        let func_size = parse_u32_or_panic(module_bytes, cursor);
        let func_end = *cursor + func_size as usize;

        // Local variable declarations
        let local_groups_count = parse_u32_or_panic(module_bytes, cursor);
        for _ in 0..local_groups_count {
            let _group_len = parse_u32_or_panic(module_bytes, cursor);
            *cursor += 1; // ValueType
        }

        // Instructions
        while *cursor < func_end {
            let opcode_byte: u8 = module_bytes[*cursor];
            if opcode_byte == OpCode::CALL as u8 {
                *cursor += 1;
                let call_index = parse_u32_or_panic(module_bytes, cursor);
                metadata.calls.push(call_index as u32);
            } else {
                OpCode::skip_bytes(module_bytes, cursor);
            }
        }
    }

    // Extra entries to mark the end of the last function
    metadata.code_offsets.push(*cursor as u32);
    metadata.calls_offsets.push(metadata.calls.len() as u32);

    metadata
}

/// Copy used functions (and their dependencies!) from an external module into our Code section
/// Replace unused functions with very small dummies, to avoid changing any indices
pub fn copy_used_functions<'a, T: SerialBuffer>(
    arena: &'a Bump,
    buffer: &mut T,
    metadata: DeadCodeMetadata<'a>,
    external_module: &[u8],
    sorted_used_func_indices: &[u32],
) {
    let [dummy_i32, dummy_i64, dummy_f32, dummy_f64, dummy_nil] = create_dummy_functions(arena);
}

/// Create a set of dummy functions that just return a constant of each possible type
fn create_dummy_functions<'a>(arena: &'a Bump) -> [Vec<'a, u8>; 5] {
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

    let mut dummy_i32 = Vec::with_capacity_in(code_builder_i32.size(), arena);
    let mut dummy_i64 = Vec::with_capacity_in(code_builder_i64.size(), arena);
    let mut dummy_f32 = Vec::with_capacity_in(code_builder_f32.size(), arena);
    let mut dummy_f64 = Vec::with_capacity_in(code_builder_f64.size(), arena);
    let mut dummy_nil = Vec::with_capacity_in(code_builder_nil.size(), arena);

    code_builder_i32.serialize(&mut dummy_i32);
    code_builder_i64.serialize(&mut dummy_i64);
    code_builder_f32.serialize(&mut dummy_f32);
    code_builder_f64.serialize(&mut dummy_f64);
    code_builder_nil.serialize(&mut dummy_nil);

    [dummy_i32, dummy_i64, dummy_f32, dummy_f64, dummy_nil]
}
