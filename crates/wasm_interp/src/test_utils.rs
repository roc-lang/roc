use crate::{Action, Instance};
use bumpalo::{collections::Vec, Bump};
use roc_wasm_module::{
    opcodes::OpCode, Export, ExportType, SerialBuffer, Signature, Value, ValueType, WasmModule,
};

pub fn default_state(arena: &Bump) -> Instance {
    let pages = 1;
    let program_counter = 0;
    let globals = [];
    Instance::new(arena, pages, program_counter, globals)
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
        let func_len_index = buf.encode_padded_u32(0);
        let start = buf.len();
        buf.push(0); // no locals
        for arg in args {
            const_value(buf, arg);
        }
        buf.push(op as u8);
        buf.push(OpCode::END as u8); // end function

        buf.overwrite_padded_u32(func_len_index, (buf.len() - start) as u32);

        module.code.function_count = 1;
        module.code.function_offsets.push(0);
        module.add_function_signature(Signature {
            param_types: Vec::new_in(&arena),
            ret_type: Some(ValueType::from(expected)),
        });
        module.export.append(Export {
            name: "test",
            ty: ExportType::Func,
            index: 0,
        });
    }

    // Dump the generated module to a file (this is mainly for debugging the test itself)
    if std::env::var("DEBUG_WASM_INTERP_TEST").is_ok() {
        let filename = format!("/tmp/{:?}.wasm", op);
        println!("\nDumping test module to {}\n", &filename);
        let mut outfile_buf = Vec::new_in(&arena);
        module.serialize(&mut outfile_buf);
        std::fs::write(&filename, outfile_buf).unwrap();
    }

    let mut state = Instance::for_module(&arena, &module, "test", true, []).unwrap();

    while let Action::Continue = state.execute_next_instruction(&module) {}

    assert_eq!(state.value_stack.pop(), expected);
}

pub fn create_exported_function_no_locals<'a, F>(
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
