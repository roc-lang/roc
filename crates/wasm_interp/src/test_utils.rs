use crate::{Action, ExecutionState};
use bumpalo::{collections::Vec, Bump};
use roc_wasm_module::{opcodes::OpCode, SerialBuffer, Value, WasmModule};

pub fn default_state(arena: &Bump) -> ExecutionState {
    let pages = 1;
    let program_counter = 0;
    let globals = [];
    ExecutionState::new(arena, pages, program_counter, globals)
}

pub fn const_value(buf: &mut Vec<'_, u8>, value: Value) {
    use Value::*;
    match value {
        I32(x) => {
            buf.push(OpCode::I32CONST as u8);
            buf.encode_i32(x);
        }
        I64(x) => {
            buf.push(OpCode::I64CONST as u8);
            buf.encode_i64(x);
        }
        F32(x) => {
            buf.push(OpCode::F32CONST as u8);
            buf.encode_f32(x);
        }
        F64(x) => {
            buf.push(OpCode::F64CONST as u8);
            buf.encode_f64(x);
        }
    }
}

pub fn test_op_example<A>(op: OpCode, args: A, expected: Value)
where
    A: IntoIterator<Item = Value>,
{
    let arena = Bump::new();
    let mut module = WasmModule::new(&arena);

    {
        let buf = &mut module.code.bytes;
        buf.push(0); // no locals
        for arg in args {
            const_value(buf, arg);
        }
        buf.push(op as u8);
        buf.push(OpCode::END as u8); // end function
    }

    let mut state = default_state(&arena);
    state.call_stack.push_frame(
        0,
        0,
        &[],
        &mut state.value_stack,
        &module.code.bytes,
        &mut state.program_counter,
    );

    while let Action::Continue = state.execute_next_instruction(&module) {}

    assert_eq!(state.value_stack.pop(), expected);
}
