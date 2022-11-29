use bumpalo::{collections::Vec, Bump};
use roc_wasm_interp::{test_utils::create_exported_function_no_locals, Action, Instance};
use roc_wasm_module::{
    opcodes::OpCode,
    sections::{DataMode, DataSegment, MemorySection},
    ConstExpr, SerialBuffer, Signature, Value, ValueType, WasmModule,
};

#[test]
fn test_currentmemory() {
    let arena = Bump::new();
    let mut module = WasmModule::new(&arena);

    let pages = 3;
    let pc = 0;
    module.memory = MemorySection::new(&arena, pages * MemorySection::PAGE_SIZE);
    module.code.bytes.push(OpCode::CURRENTMEMORY as u8);

    let mut state = Instance::new(&arena, pages, pc, []);
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

    let mut state = Instance::new(&arena, existing_pages, pc, []);
    state.execute_next_instruction(&module);
    state.execute_next_instruction(&module);
    assert_eq!(state.memory.len(), 5 * MemorySection::PAGE_SIZE as usize);
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
        Instance::for_module(&arena, &module, start_fn_name, is_debug_mode, []).unwrap();

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

    let mut state = Instance::for_module(arena, module, start_fn_name, is_debug_mode, []).unwrap();

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
