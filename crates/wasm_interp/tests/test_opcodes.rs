#![cfg(test)]

use bumpalo::{collections::Vec, Bump};
use roc_wasm_interp::{Action, ExecutionState, ValueStack};
use roc_wasm_module::{
    opcodes::OpCode,
    sections::{DataMode, DataSegment, MemorySection},
    ConstExpr, Export, ExportType, SerialBuffer, Serialize, Signature, Value, ValueType,
    WasmModule,
};

fn default_state(arena: &Bump) -> ExecutionState {
    let pages = 1;
    let program_counter = 0;
    let globals = [];
    ExecutionState::new(arena, pages, program_counter, globals)
}

// #[test]
// fn test_block() {}

// #[test]
// fn test_loop() {}

// #[test]
// fn test_if() {}

// #[test]
// fn test_else() {}

// #[test]
// fn test_end() {}

// #[test]
// fn test_br() {}

// #[test]
// fn test_brif() {}

// #[test]
// fn test_brtable() {}

#[test]
fn test_call_return_no_args() {
    let arena = Bump::new();
    let mut state = default_state(&arena);
    let mut module = WasmModule::new(&arena);

    // Function 0
    let func0_offset = module.code.bytes.len() as u32;
    module.code.function_offsets.push(func0_offset);
    module.add_function_signature(Signature {
        param_types: Vec::new_in(&arena),
        ret_type: Some(ValueType::I32),
    });
    [
        0, // no locals
        OpCode::CALL as u8,
        1, // function 1
        OpCode::END as u8,
    ]
    .serialize(&mut module.code.bytes);
    let func0_first_instruction = func0_offset + 2; // skip function length and locals length

    // Function 1
    let func1_offset = module.code.bytes.len() as u32;
    module.code.function_offsets.push(func1_offset);
    module.add_function_signature(Signature {
        param_types: Vec::new_in(&arena),
        ret_type: Some(ValueType::I32),
    });
    [
        0, // no locals
        OpCode::I32CONST as u8,
        42, // constant value (<64 so that LEB-128 is just one byte)
        OpCode::END as u8,
    ]
    .serialize(&mut module.code.bytes);

    state.program_counter = func0_first_instruction as usize;

    while let Action::Continue = state.execute_next_instruction(&module) {}

    assert_eq!(state.value_stack.peek(), Value::I32(42));
}

#[test]
fn test_call_return_with_args() {
    let arena = Bump::new();
    let mut state = default_state(&arena);
    let mut module = WasmModule::new(&arena);

    // Function 0: calculate 2+2
    let func0_offset = module.code.bytes.len() as u32;
    module.code.function_offsets.push(func0_offset);
    module.add_function_signature(Signature {
        param_types: bumpalo::vec![in &arena;],
        ret_type: Some(ValueType::I32),
    });
    [
        0, // no locals
        OpCode::I32CONST as u8,
        2,
        OpCode::I32CONST as u8,
        2,
        OpCode::CALL as u8,
        1,
        OpCode::END as u8,
    ]
    .serialize(&mut module.code.bytes);
    let func0_first_instruction = func0_offset + 2; // skip function length and locals length

    // Function 1: add two numbers
    let func1_offset = module.code.bytes.len() as u32;
    module.code.function_offsets.push(func1_offset);
    module.add_function_signature(Signature {
        param_types: bumpalo::vec![in &arena; ValueType::I32, ValueType::I32],
        ret_type: Some(ValueType::I32),
    });
    [
        0, // no locals
        OpCode::GETLOCAL as u8,
        0,
        OpCode::GETLOCAL as u8,
        1,
        OpCode::I32ADD as u8,
        OpCode::END as u8,
    ]
    .serialize(&mut module.code.bytes);

    state.program_counter = func0_first_instruction as usize;

    while let Action::Continue = state.execute_next_instruction(&module) {}

    assert_eq!(state.value_stack.peek(), Value::I32(4));
}

// #[test]
// fn test_callindirect() {}

// #[test]
// fn test_drop() {}

// #[test]
// fn test_select() {}

#[test]
fn test_set_get_local() {
    let arena = Bump::new();
    let mut state = default_state(&arena);
    let mut module = WasmModule::new(&arena);
    let mut vs = ValueStack::new(&arena);

    let mut buffer = vec![];
    let mut cursor = 0;
    [
        (1u32, ValueType::F32),
        (1u32, ValueType::F64),
        (1u32, ValueType::I32),
        (1u32, ValueType::I64),
    ]
    .serialize(&mut buffer);
    state
        .call_stack
        .push_frame(0x1234, 0, 0, &mut vs, &buffer, &mut cursor);

    module.code.bytes.push(OpCode::I32CONST as u8);
    module.code.bytes.encode_i32(12345);
    module.code.bytes.push(OpCode::SETLOCAL as u8);
    module.code.bytes.encode_u32(2);

    module.code.bytes.push(OpCode::GETLOCAL as u8);
    module.code.bytes.encode_u32(2);

    state.execute_next_instruction(&module);
    state.execute_next_instruction(&module);
    state.execute_next_instruction(&module);
    assert_eq!(state.value_stack.len(), 1);
    assert_eq!(state.value_stack.pop(), Value::I32(12345));
}

#[test]
fn test_tee_get_local() {
    let arena = Bump::new();
    let mut state = default_state(&arena);
    let mut module = WasmModule::new(&arena);
    let mut vs = ValueStack::new(&arena);

    let mut buffer = vec![];
    let mut cursor = 0;
    [
        (1u32, ValueType::F32),
        (1u32, ValueType::F64),
        (1u32, ValueType::I32),
        (1u32, ValueType::I64),
    ]
    .serialize(&mut buffer);
    state
        .call_stack
        .push_frame(0x1234, 0, 0, &mut vs, &buffer, &mut cursor);

    module.code.bytes.push(OpCode::I32CONST as u8);
    module.code.bytes.encode_i32(12345);
    module.code.bytes.push(OpCode::TEELOCAL as u8);
    module.code.bytes.encode_u32(2);

    module.code.bytes.push(OpCode::GETLOCAL as u8);
    module.code.bytes.encode_u32(2);

    state.execute_next_instruction(&module);
    state.execute_next_instruction(&module);
    state.execute_next_instruction(&module);
    assert_eq!(state.value_stack.len(), 2);
    assert_eq!(state.value_stack.pop(), Value::I32(12345));
    assert_eq!(state.value_stack.pop(), Value::I32(12345));
}

#[test]
fn test_global() {
    let arena = Bump::new();
    let mut state = default_state(&arena);
    state
        .globals
        .extend_from_slice(&[Value::F64(1.11), Value::I32(222), Value::F64(3.33)]);
    let mut module = WasmModule::new(&arena);

    module.code.bytes.push(OpCode::GETGLOBAL as u8);
    module.code.bytes.encode_u32(1);
    module.code.bytes.push(OpCode::I32CONST as u8);
    module.code.bytes.encode_i32(555);
    module.code.bytes.push(OpCode::SETGLOBAL as u8);
    module.code.bytes.encode_u32(1);
    module.code.bytes.push(OpCode::GETGLOBAL as u8);
    module.code.bytes.encode_u32(1);

    state.execute_next_instruction(&module);
    state.execute_next_instruction(&module);
    state.execute_next_instruction(&module);
    state.execute_next_instruction(&module);
    assert_eq!(state.value_stack.len(), 2);
    assert_eq!(state.value_stack.pop(), Value::I32(555));
    assert_eq!(state.value_stack.pop(), Value::I32(222));
}

fn create_exported_function_no_locals<'a, F>(
    module: &mut WasmModule<'a>,
    name: &'a str,
    signature: Signature<'a>,
    write_instructions: F,
) where
    F: FnOnce(&mut Vec<'a, u8>),
{
    let internal_fn_index = module.code.function_offsets.len();
    let fn_index = module.import.function_count() + internal_fn_index;
    module.export.exports.push(Export {
        name,
        ty: ExportType::Func,
        index: fn_index as u32,
    });
    module.add_function_signature(signature);

    let offset = module.code.bytes.encode_padded_u32(0);
    let start = module.code.bytes.len();
    module.code.bytes.push(0); // no locals
    write_instructions(&mut module.code.bytes);
    let len = module.code.bytes.len() - start;
    module.code.bytes.overwrite_padded_u32(offset, len as u32);

    module.code.function_count += 1;
    module.code.function_offsets.push(offset as u32);
}

fn test_load(load_op: OpCode, ty: ValueType, data: &[u8], addr: u32, offset: u32) -> Value {
    let arena = Bump::new();
    let mut module = WasmModule::new(&arena);

    let is_debug_mode = false;
    let start_fn_name = "test";

    module.memory = MemorySection::new(&arena, MemorySection::PAGE_SIZE);

    module.data.append_segment(DataSegment {
        mode: DataMode::Active {
            offset: ConstExpr::I32(addr as i32),
        },
        init: Vec::from_iter_in(data.iter().copied(), &arena),
    });

    let signature = Signature {
        param_types: bumpalo::vec![in &arena],
        ret_type: Some(ty),
    };

    create_exported_function_no_locals(&mut module, start_fn_name, signature, |buf| {
        buf.append_u8(OpCode::I32CONST as u8);
        buf.encode_u32(addr);
        buf.append_u8(load_op as u8);
        buf.encode_u32(0); // align
        buf.encode_u32(offset);
        buf.append_u8(OpCode::END as u8);
    });

    if false {
        let mut outfile_buf = Vec::new_in(&arena);
        module.serialize(&mut outfile_buf);
        std::fs::write("/tmp/roc/interp_load_test.wasm", outfile_buf).unwrap();
    }

    let mut state =
        ExecutionState::for_module(&arena, &module, start_fn_name, is_debug_mode).unwrap();

    while let Action::Continue = state.execute_next_instruction(&module) {}

    state.value_stack.pop()
}

#[test]
fn test_i32load() {
    let bytes = "abcdefgh".as_bytes();
    assert_eq!(
        test_load(OpCode::I32LOAD, ValueType::I32, bytes, 0x11, 0),
        Value::I32(0x64636261)
    );
    assert_eq!(
        test_load(OpCode::I32LOAD, ValueType::I32, bytes, 0x11, 2),
        Value::I32(0x66656463)
    );
}

#[test]
fn test_i64load() {
    let bytes = "abcdefghijkl".as_bytes();
    assert_eq!(
        test_load(OpCode::I64LOAD, ValueType::I64, bytes, 0x11, 0),
        Value::I64(0x6867666564636261)
    );
    assert_eq!(
        test_load(OpCode::I64LOAD, ValueType::I64, bytes, 0x11, 2),
        Value::I64(0x6a69686766656463)
    );
}

#[test]
fn test_f32load() {
    let value: f32 = 1.23456;
    let bytes = value.to_le_bytes();
    assert_eq!(
        test_load(OpCode::F32LOAD, ValueType::F32, &bytes, 0x11, 0),
        Value::F32(value)
    );
}

#[test]
fn test_f64load() {
    let value: f64 = 1.23456;
    let bytes = value.to_le_bytes();
    assert_eq!(
        test_load(OpCode::F64LOAD, ValueType::F64, &bytes, 0x11, 0),
        Value::F64(value)
    );
}

#[test]
fn test_i32load8s() {
    let value: i8 = -42;
    let bytes = value.to_le_bytes();
    assert_eq!(
        test_load(OpCode::I32LOAD8S, ValueType::I32, &bytes, 0x11, 0),
        Value::I32(value as i32)
    );
}

#[test]
fn test_i32load8u() {
    let value: u8 = 42;
    let bytes = value.to_le_bytes();
    assert_eq!(
        test_load(OpCode::I32LOAD8U, ValueType::I32, &bytes, 0x11, 0),
        Value::I32(value as i32)
    );
}

#[test]
fn test_i32load16s() {
    let value: i16 = -42;
    let bytes = value.to_le_bytes();
    assert_eq!(
        test_load(OpCode::I32LOAD16S, ValueType::I32, &bytes, 0x11, 0),
        Value::I32(value as i32)
    );
}

#[test]
fn test_i32load16u() {
    let value: u16 = 42;
    let bytes = value.to_le_bytes();
    assert_eq!(
        test_load(OpCode::I32LOAD16U, ValueType::I32, &bytes, 0x11, 0),
        Value::I32(value as i32)
    );
}

#[test]
fn test_i64load8s() {
    let value: i8 = -42;
    let bytes = value.to_le_bytes();
    assert_eq!(
        test_load(OpCode::I64LOAD8S, ValueType::I64, &bytes, 0x11, 0),
        Value::I64(value as i64)
    );
}

#[test]
fn test_i64load8u() {
    let value: u8 = 42;
    let bytes = value.to_le_bytes();
    assert_eq!(
        test_load(OpCode::I32LOAD8U, ValueType::I32, &bytes, 0x11, 0),
        Value::I32(value as i32)
    );
}

#[test]
fn test_i64load16s() {
    let value: i16 = -42;
    let bytes = value.to_le_bytes();
    assert_eq!(
        test_load(OpCode::I64LOAD8S, ValueType::I64, &bytes, 0x11, 0),
        Value::I64(value as i64)
    );
}

#[test]
fn test_i64load16u() {
    let value: u16 = 42;
    let bytes = value.to_le_bytes();
    assert_eq!(
        test_load(OpCode::I32LOAD8U, ValueType::I32, &bytes, 0x11, 0),
        Value::I32(value as i32)
    );
}

#[test]
fn test_i64load32s() {
    let value: i32 = -42;
    let bytes = value.to_le_bytes();
    assert_eq!(
        test_load(OpCode::I64LOAD8S, ValueType::I64, &bytes, 0x11, 0),
        Value::I64(value as i64)
    );
}

#[test]
fn test_i64load32u() {
    let value: u32 = 42;
    let bytes = value.to_le_bytes();
    assert_eq!(
        test_load(OpCode::I32LOAD8U, ValueType::I32, &bytes, 0x11, 0),
        Value::I32(value as i32)
    );
}

fn test_store<'a>(
    arena: &'a Bump,
    module: &mut WasmModule<'a>,
    addr: u32,
    store_op: OpCode,
    offset: u32,
    value: Value,
) -> Vec<'a, u8> {
    let is_debug_mode = false;
    let start_fn_name = "test";

    module.memory = MemorySection::new(arena, MemorySection::PAGE_SIZE);

    let signature = Signature {
        param_types: bumpalo::vec![in arena],
        ret_type: None,
    };

    create_exported_function_no_locals(module, start_fn_name, signature, |buf| {
        buf.append_u8(OpCode::I32CONST as u8);
        buf.encode_u32(addr);
        match value {
            Value::I32(x) => {
                buf.append_u8(OpCode::I32CONST as u8);
                buf.encode_i32(x);
            }
            Value::I64(x) => {
                buf.append_u8(OpCode::I64CONST as u8);
                buf.encode_i64(x);
            }
            Value::F32(x) => {
                buf.append_u8(OpCode::F32CONST as u8);
                buf.encode_f32(x);
            }
            Value::F64(x) => {
                buf.append_u8(OpCode::F64CONST as u8);
                buf.encode_f64(x);
            }
        }
        buf.append_u8(store_op as u8);
        buf.encode_u32(0); // align
        buf.encode_u32(offset);
        buf.append_u8(OpCode::END as u8);
    });

    let mut state =
        ExecutionState::for_module(arena, module, start_fn_name, is_debug_mode).unwrap();

    while let Action::Continue = state.execute_next_instruction(module) {}

    state.memory
}

#[test]
fn test_i32store() {
    let arena = Bump::new();
    let mut module = WasmModule::new(&arena);

    let addr: u32 = 0x11;
    let store_op = OpCode::I32STORE;
    let offset = 1;
    let value = Value::I32(0x12345678);
    let memory = test_store(&arena, &mut module, addr, store_op, offset, value);

    let index = (addr + offset) as usize;
    assert_eq!(&memory[index..][..4], &[0x78, 0x56, 0x34, 0x12]);
}

#[test]
fn test_i64store() {
    let arena = Bump::new();
    let mut module = WasmModule::new(&arena);

    let addr: u32 = 0x11;
    let store_op = OpCode::I64STORE;
    let offset = 1;
    let value = Value::I64(0x123456789abcdef0);
    let memory = test_store(&arena, &mut module, addr, store_op, offset, value);

    let index = (addr + offset) as usize;
    assert_eq!(
        &memory[index..][..8],
        &[0xf0, 0xde, 0xbc, 0x9a, 0x78, 0x56, 0x34, 0x12]
    );
}

#[test]
fn test_f32store() {
    let arena = Bump::new();
    let mut module = WasmModule::new(&arena);

    let addr: u32 = 0x11;
    let store_op = OpCode::F32STORE;
    let offset = 1;
    let inner: f32 = 1.23456;
    let value = Value::F32(inner);
    let memory = test_store(&arena, &mut module, addr, store_op, offset, value);

    let index = (addr + offset) as usize;
    assert_eq!(&memory[index..][..4], &inner.to_le_bytes());
}

#[test]
fn test_f64store() {
    let arena = Bump::new();
    let mut module = WasmModule::new(&arena);

    let addr: u32 = 0x11;
    let store_op = OpCode::F64STORE;
    let offset = 1;
    let inner: f64 = 1.23456;
    let value = Value::F64(inner);
    let memory = test_store(&arena, &mut module, addr, store_op, offset, value);

    let index = (addr + offset) as usize;
    assert_eq!(&memory[index..][..8], &inner.to_le_bytes());
}

#[test]
fn test_i32store8() {
    let arena = Bump::new();
    let mut module = WasmModule::new(&arena);

    let addr: u32 = 0x11;
    let store_op = OpCode::I32STORE8;
    let offset = 1;
    let value = Value::I32(0x12345678);
    let memory = test_store(&arena, &mut module, addr, store_op, offset, value);

    let index = (addr + offset) as usize;
    assert_eq!(&memory[index..][..4], &[0x78, 0x00, 0x00, 0x00]);
}

#[test]
fn test_i32store16() {
    let arena = Bump::new();
    let mut module = WasmModule::new(&arena);

    let addr: u32 = 0x11;
    let store_op = OpCode::I32STORE16;
    let offset = 1;
    let value = Value::I32(0x12345678);
    let memory = test_store(&arena, &mut module, addr, store_op, offset, value);

    let index = (addr + offset) as usize;
    assert_eq!(&memory[index..][..4], &[0x78, 0x56, 0x00, 0x00]);
}

#[test]
fn test_i64store8() {
    let arena = Bump::new();
    let mut module = WasmModule::new(&arena);

    let addr: u32 = 0x11;
    let store_op = OpCode::I64STORE8;
    let offset = 1;
    let value = Value::I64(0x123456789abcdef0);
    let memory = test_store(&arena, &mut module, addr, store_op, offset, value);

    let index = (addr + offset) as usize;
    assert_eq!(
        &memory[index..][..8],
        &[0xf0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]
    );
}

#[test]
fn test_i64store16() {
    let arena = Bump::new();
    let mut module = WasmModule::new(&arena);

    let addr: u32 = 0x11;
    let store_op = OpCode::I64STORE16;
    let offset = 1;
    let value = Value::I64(0x123456789abcdef0);
    let memory = test_store(&arena, &mut module, addr, store_op, offset, value);

    let index = (addr + offset) as usize;
    assert_eq!(
        &memory[index..][..8],
        &[0xf0, 0xde, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00]
    );
}

#[test]
fn test_i64store32() {
    let arena = Bump::new();
    let mut module = WasmModule::new(&arena);

    let addr: u32 = 0x11;
    let store_op = OpCode::I64STORE32;
    let offset = 1;
    let value = Value::I64(0x123456789abcdef0);
    let memory = test_store(&arena, &mut module, addr, store_op, offset, value);

    let index = (addr + offset) as usize;
    assert_eq!(
        &memory[index..][..8],
        &[0xf0, 0xde, 0xbc, 0x9a, 0x00, 0x00, 0x00, 0x00]
    );
}

#[test]
fn test_currentmemory() {
    let arena = Bump::new();
    let mut module = WasmModule::new(&arena);

    let pages = 3;
    let pc = 0;
    module.memory = MemorySection::new(&arena, pages * MemorySection::PAGE_SIZE);
    module.code.bytes.push(OpCode::CURRENTMEMORY as u8);

    let mut state = ExecutionState::new(&arena, pages, pc, []);
    state.execute_next_instruction(&module);
    assert_eq!(state.value_stack.pop(), Value::I32(3))
}

#[test]
fn test_growmemory() {
    let arena = Bump::new();
    let mut module = WasmModule::new(&arena);

    let existing_pages = 3;
    let grow_pages = 2;
    let pc = 0;
    module.memory = MemorySection::new(&arena, existing_pages * MemorySection::PAGE_SIZE);
    module.code.bytes.push(OpCode::I32CONST as u8);
    module.code.bytes.encode_i32(grow_pages);
    module.code.bytes.push(OpCode::GROWMEMORY as u8);

    let mut state = ExecutionState::new(&arena, existing_pages, pc, []);
    state.execute_next_instruction(&module);
    state.execute_next_instruction(&module);
    assert_eq!(state.memory.len(), 5 * MemorySection::PAGE_SIZE as usize);
}

#[test]
fn test_i32const() {
    let arena = Bump::new();
    let mut state = default_state(&arena);
    let mut module = WasmModule::new(&arena);

    module.code.bytes.push(OpCode::I32CONST as u8);
    module.code.bytes.encode_i32(12345);

    state.execute_next_instruction(&module);
    assert_eq!(state.value_stack.pop(), Value::I32(12345))
}

#[test]
fn test_i64const() {
    let arena = Bump::new();
    let mut state = default_state(&arena);
    let mut module = WasmModule::new(&arena);

    module.code.bytes.push(OpCode::I64CONST as u8);
    module.code.bytes.encode_i64(1234567890);

    state.execute_next_instruction(&module);
    assert_eq!(state.value_stack.pop(), Value::I64(1234567890))
}

#[test]
fn test_f32const() {
    let arena = Bump::new();
    let mut state = default_state(&arena);
    let mut module = WasmModule::new(&arena);

    module.code.bytes.push(OpCode::F32CONST as u8);
    module.code.bytes.encode_f32(123.45);

    state.execute_next_instruction(&module);
    assert_eq!(state.value_stack.pop(), Value::F32(123.45))
}

#[test]
fn test_f64const() {
    let arena = Bump::new();
    let mut state = default_state(&arena);
    let mut module = WasmModule::new(&arena);

    module.code.bytes.push(OpCode::F64CONST as u8);
    module.code.bytes.encode_f64(12345.67890);

    state.execute_next_instruction(&module);
    assert_eq!(state.value_stack.pop(), Value::F64(12345.67890))
}

// #[test]
// fn test_i32eqz() {}

// #[test]
// fn test_i32eq() {}

// #[test]
// fn test_i32ne() {}

// #[test]
// fn test_i32lts() {}

// #[test]
// fn test_i32ltu() {}

// #[test]
// fn test_i32gts() {}

// #[test]
// fn test_i32gtu() {}

// #[test]
// fn test_i32les() {}

// #[test]
// fn test_i32leu() {}

// #[test]
// fn test_i32ges() {}

// #[test]
// fn test_i32geu() {}

// #[test]
// fn test_i64eqz() {}

// #[test]
// fn test_i64eq() {}

// #[test]
// fn test_i64ne() {}

// #[test]
// fn test_i64lts() {}

// #[test]
// fn test_i64ltu() {}

// #[test]
// fn test_i64gts() {}

// #[test]
// fn test_i64gtu() {}

// #[test]
// fn test_i64les() {}

// #[test]
// fn test_i64leu() {}

// #[test]
// fn test_i64ges() {}

// #[test]
// fn test_i64geu() {}

// #[test]
// fn test_f32eq() {}

// #[test]
// fn test_f32ne() {}

// #[test]
// fn test_f32lt() {}

// #[test]
// fn test_f32gt() {}

// #[test]
// fn test_f32le() {}

// #[test]
// fn test_f32ge() {}

// #[test]
// fn test_f64eq() {}

// #[test]
// fn test_f64ne() {}

// #[test]
// fn test_f64lt() {}

// #[test]
// fn test_f64gt() {}

// #[test]
// fn test_f64le() {}

// #[test]
// fn test_f64ge() {}

// #[test]
// fn test_i32clz() {}

// #[test]
// fn test_i32ctz() {}

// #[test]
// fn test_i32popcnt() {}

#[test]
fn test_i32add() {
    let arena = Bump::new();
    let mut state = default_state(&arena);
    let mut module = WasmModule::new(&arena);

    module.code.bytes.push(OpCode::I32CONST as u8);
    module.code.bytes.encode_i32(123);
    module.code.bytes.push(OpCode::I32CONST as u8);
    module.code.bytes.encode_i32(321);
    module.code.bytes.push(OpCode::I32ADD as u8);

    state.execute_next_instruction(&module);
    state.execute_next_instruction(&module);
    state.execute_next_instruction(&module);
    assert_eq!(state.value_stack.pop(), Value::I32(444))
}

#[test]
fn test_i32sub() {
    let arena = Bump::new();
    let mut state = default_state(&arena);
    let mut module = WasmModule::new(&arena);

    module.code.bytes.push(OpCode::I32CONST as u8);
    module.code.bytes.encode_i32(123);
    module.code.bytes.push(OpCode::I32CONST as u8);
    module.code.bytes.encode_i32(321);
    module.code.bytes.push(OpCode::I32SUB as u8);

    state.execute_next_instruction(&module);
    state.execute_next_instruction(&module);
    state.execute_next_instruction(&module);
    assert_eq!(state.value_stack.pop(), Value::I32(-198))
}

#[test]
fn test_i32mul() {
    let arena = Bump::new();
    let mut state = default_state(&arena);
    let mut module = WasmModule::new(&arena);

    module.code.bytes.push(OpCode::I32CONST as u8);
    module.code.bytes.encode_i32(123);
    module.code.bytes.push(OpCode::I32CONST as u8);
    module.code.bytes.encode_i32(321);
    module.code.bytes.push(OpCode::I32MUL as u8);

    state.execute_next_instruction(&module);
    state.execute_next_instruction(&module);
    state.execute_next_instruction(&module);
    assert_eq!(state.value_stack.pop(), Value::I32(39483))
}

// #[test]
// fn test_i32divs() {}

// #[test]
// fn test_i32divu() {}

// #[test]
// fn test_i32rems() {}

// #[test]
// fn test_i32remu() {}

// #[test]
// fn test_i32and() {}

// #[test]
// fn test_i32or() {}

// #[test]
// fn test_i32xor() {}

// #[test]
// fn test_i32shl() {}

// #[test]
// fn test_i32shrs() {}

// #[test]
// fn test_i32shru() {}

// #[test]
// fn test_i32rotl() {}

// #[test]
// fn test_i32rotr() {}

// #[test]
// fn test_i64clz() {}

// #[test]
// fn test_i64ctz() {}

// #[test]
// fn test_i64popcnt() {}

// #[test]
// fn test_i64add() {}

// #[test]
// fn test_i64sub() {}

// #[test]
// fn test_i64mul() {}

// #[test]
// fn test_i64divs() {}

// #[test]
// fn test_i64divu() {}

// #[test]
// fn test_i64rems() {}

// #[test]
// fn test_i64remu() {}

// #[test]
// fn test_i64and() {}

// #[test]
// fn test_i64or() {}

// #[test]
// fn test_i64xor() {}

// #[test]
// fn test_i64shl() {}

// #[test]
// fn test_i64shrs() {}

// #[test]
// fn test_i64shru() {}

// #[test]
// fn test_i64rotl() {}

// #[test]
// fn test_i64rotr() {}

// #[test]
// fn test_f32abs() {}

// #[test]
// fn test_f32neg() {}

// #[test]
// fn test_f32ceil() {}

// #[test]
// fn test_f32floor() {}

// #[test]
// fn test_f32trunc() {}

// #[test]
// fn test_f32nearest() {}

// #[test]
// fn test_f32sqrt() {}

// #[test]
// fn test_f32add() {}

// #[test]
// fn test_f32sub() {}

// #[test]
// fn test_f32mul() {}

// #[test]
// fn test_f32div() {}

// #[test]
// fn test_f32min() {}

// #[test]
// fn test_f32max() {}

// #[test]
// fn test_f32copysign() {}

// #[test]
// fn test_f64abs() {}

// #[test]
// fn test_f64neg() {}

// #[test]
// fn test_f64ceil() {}

// #[test]
// fn test_f64floor() {}

// #[test]
// fn test_f64trunc() {}

// #[test]
// fn test_f64nearest() {}

// #[test]
// fn test_f64sqrt() {}

// #[test]
// fn test_f64add() {}

// #[test]
// fn test_f64sub() {}

// #[test]
// fn test_f64mul() {}

// #[test]
// fn test_f64div() {}

// #[test]
// fn test_f64min() {}

// #[test]
// fn test_f64max() {}

// #[test]
// fn test_f64copysign() {}

// #[test]
// fn test_i32wrapi64() {}

// #[test]
// fn test_i32truncsf32() {}

// #[test]
// fn test_i32truncuf32() {}

// #[test]
// fn test_i32truncsf64() {}

// #[test]
// fn test_i32truncuf64() {}

// #[test]
// fn test_i64extendsi32() {}

// #[test]
// fn test_i64extendui32() {}

// #[test]
// fn test_i64truncsf32() {}

// #[test]
// fn test_i64truncuf32() {}

// #[test]
// fn test_i64truncsf64() {}

// #[test]
// fn test_i64truncuf64() {}

// #[test]
// fn test_f32convertsi32() {}

// #[test]
// fn test_f32convertui32() {}

// #[test]
// fn test_f32convertsi64() {}

// #[test]
// fn test_f32convertui64() {}

// #[test]
// fn test_f32demotef64() {}

// #[test]
// fn test_f64convertsi32() {}

// #[test]
// fn test_f64convertui32() {}

// #[test]
// fn test_f64convertsi64() {}

// #[test]
// fn test_f64convertui64() {}

// #[test]
// fn test_f64promotef32() {}

// #[test]
// fn test_i32reinterpretf32() {}

// #[test]
// fn test_i64reinterpretf64() {}

// #[test]
// fn test_f32reinterpreti32() {}

// #[test]
// fn test_f64reinterpreti64() {}
