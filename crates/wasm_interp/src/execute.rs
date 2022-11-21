use bumpalo::{collections::Vec, Bump};
use roc_wasm_module::opcodes::OpCode;
use roc_wasm_module::WasmModule;

use crate::call_stack::CallStack;
use crate::value_stack::ValueStack;

pub struct ExecutionState<'a> {
    memory: Vec<'a, u8>,
    call_stack: CallStack<'a>,
    value_stack: ValueStack<'a>,
    program_counter: usize,
}

impl<'a> ExecutionState<'a> {
    fn next(&mut self, module: WasmModule<'a>) {
        use OpCode::*;

        let op_code = OpCode::from(module.code.bytes[self.program_counter]);
        self.program_counter += 1;
        match op_code {
            UNREACHABLE => {
                unreachable!("WebAssembly tried to execute an `unreachable` instruction.");
            }
            NOP => {}
            BLOCK => {
                todo!("{:?}", op_code);
            }
            LOOP => {
                todo!("{:?}", op_code);
            }
            IF => {
                todo!("{:?}", op_code);
            }
            ELSE => {
                todo!("{:?}", op_code);
            }
            END => {
                todo!("{:?}", op_code);
            }
            BR => {
                todo!("{:?}", op_code);
            }
            BRIF => {
                todo!("{:?}", op_code);
            }
            BRTABLE => {
                todo!("{:?}", op_code);
            }
            RETURN => {
                todo!("{:?}", op_code);
            }
            CALL => {
                todo!("{:?}", op_code);
            }
            CALLINDIRECT => {
                todo!("{:?}", op_code);
            }
            DROP => {
                todo!("{:?}", op_code);
            }
            SELECT => {
                todo!("{:?}", op_code);
            }
            GETLOCAL => {
                todo!("{:?}", op_code);
            }
            SETLOCAL => {
                todo!("{:?}", op_code);
            }
            TEELOCAL => {
                todo!("{:?}", op_code);
            }
            GETGLOBAL => {
                todo!("{:?}", op_code);
            }
            SETGLOBAL => {
                todo!("{:?}", op_code);
            }
            I32LOAD => {
                todo!("{:?}", op_code);
            }
            I64LOAD => {
                todo!("{:?}", op_code);
            }
            F32LOAD => {
                todo!("{:?}", op_code);
            }
            F64LOAD => {
                todo!("{:?}", op_code);
            }
            I32LOAD8S => {
                todo!("{:?}", op_code);
            }
            I32LOAD8U => {
                todo!("{:?}", op_code);
            }
            I32LOAD16S => {
                todo!("{:?}", op_code);
            }
            I32LOAD16U => {
                todo!("{:?}", op_code);
            }
            I64LOAD8S => {
                todo!("{:?}", op_code);
            }
            I64LOAD8U => {
                todo!("{:?}", op_code);
            }
            I64LOAD16S => {
                todo!("{:?}", op_code);
            }
            I64LOAD16U => {
                todo!("{:?}", op_code);
            }
            I64LOAD32S => {
                todo!("{:?}", op_code);
            }
            I64LOAD32U => {
                todo!("{:?}", op_code);
            }
            I32STORE => {
                todo!("{:?}", op_code);
            }
            I64STORE => {
                todo!("{:?}", op_code);
            }
            F32STORE => {
                todo!("{:?}", op_code);
            }
            F64STORE => {
                todo!("{:?}", op_code);
            }
            I32STORE8 => {
                todo!("{:?}", op_code);
            }
            I32STORE16 => {
                todo!("{:?}", op_code);
            }
            I64STORE8 => {
                todo!("{:?}", op_code);
            }
            I64STORE16 => {
                todo!("{:?}", op_code);
            }
            I64STORE32 => {
                todo!("{:?}", op_code);
            }
            CURRENTMEMORY => {
                todo!("{:?}", op_code);
            }
            GROWMEMORY => {
                todo!("{:?}", op_code);
            }
            I32CONST => {
                todo!("{:?}", op_code);
            }
            I64CONST => {
                todo!("{:?}", op_code);
            }
            F32CONST => {
                todo!("{:?}", op_code);
            }
            F64CONST => {
                todo!("{:?}", op_code);
            }
            I32EQZ => {
                todo!("{:?}", op_code);
            }
            I32EQ => {
                todo!("{:?}", op_code);
            }
            I32NE => {
                todo!("{:?}", op_code);
            }
            I32LTS => {
                todo!("{:?}", op_code);
            }
            I32LTU => {
                todo!("{:?}", op_code);
            }
            I32GTS => {
                todo!("{:?}", op_code);
            }
            I32GTU => {
                todo!("{:?}", op_code);
            }
            I32LES => {
                todo!("{:?}", op_code);
            }
            I32LEU => {
                todo!("{:?}", op_code);
            }
            I32GES => {
                todo!("{:?}", op_code);
            }
            I32GEU => {
                todo!("{:?}", op_code);
            }
            I64EQZ => {
                todo!("{:?}", op_code);
            }
            I64EQ => {
                todo!("{:?}", op_code);
            }
            I64NE => {
                todo!("{:?}", op_code);
            }
            I64LTS => {
                todo!("{:?}", op_code);
            }
            I64LTU => {
                todo!("{:?}", op_code);
            }
            I64GTS => {
                todo!("{:?}", op_code);
            }
            I64GTU => {
                todo!("{:?}", op_code);
            }
            I64LES => {
                todo!("{:?}", op_code);
            }
            I64LEU => {
                todo!("{:?}", op_code);
            }
            I64GES => {
                todo!("{:?}", op_code);
            }
            I64GEU => {
                todo!("{:?}", op_code);
            }

            F32EQ => {
                todo!("{:?}", op_code);
            }
            F32NE => {
                todo!("{:?}", op_code);
            }
            F32LT => {
                todo!("{:?}", op_code);
            }
            F32GT => {
                todo!("{:?}", op_code);
            }
            F32LE => {
                todo!("{:?}", op_code);
            }
            F32GE => {
                todo!("{:?}", op_code);
            }

            F64EQ => {
                todo!("{:?}", op_code);
            }
            F64NE => {
                todo!("{:?}", op_code);
            }
            F64LT => {
                todo!("{:?}", op_code);
            }
            F64GT => {
                todo!("{:?}", op_code);
            }
            F64LE => {
                todo!("{:?}", op_code);
            }
            F64GE => {
                todo!("{:?}", op_code);
            }

            I32CLZ => {
                todo!("{:?}", op_code);
            }
            I32CTZ => {
                todo!("{:?}", op_code);
            }
            I32POPCNT => {
                todo!("{:?}", op_code);
            }
            I32ADD => {
                todo!("{:?}", op_code);
            }
            I32SUB => {
                todo!("{:?}", op_code);
            }
            I32MUL => {
                todo!("{:?}", op_code);
            }
            I32DIVS => {
                todo!("{:?}", op_code);
            }
            I32DIVU => {
                todo!("{:?}", op_code);
            }
            I32REMS => {
                todo!("{:?}", op_code);
            }
            I32REMU => {
                todo!("{:?}", op_code);
            }
            I32AND => {
                todo!("{:?}", op_code);
            }
            I32OR => {
                todo!("{:?}", op_code);
            }
            I32XOR => {
                todo!("{:?}", op_code);
            }
            I32SHL => {
                todo!("{:?}", op_code);
            }
            I32SHRS => {
                todo!("{:?}", op_code);
            }
            I32SHRU => {
                todo!("{:?}", op_code);
            }
            I32ROTL => {
                todo!("{:?}", op_code);
            }
            I32ROTR => {
                todo!("{:?}", op_code);
            }

            I64CLZ => {
                todo!("{:?}", op_code);
            }
            I64CTZ => {
                todo!("{:?}", op_code);
            }
            I64POPCNT => {
                todo!("{:?}", op_code);
            }
            I64ADD => {
                todo!("{:?}", op_code);
            }
            I64SUB => {
                todo!("{:?}", op_code);
            }
            I64MUL => {
                todo!("{:?}", op_code);
            }
            I64DIVS => {
                todo!("{:?}", op_code);
            }
            I64DIVU => {
                todo!("{:?}", op_code);
            }
            I64REMS => {
                todo!("{:?}", op_code);
            }
            I64REMU => {
                todo!("{:?}", op_code);
            }
            I64AND => {
                todo!("{:?}", op_code);
            }
            I64OR => {
                todo!("{:?}", op_code);
            }
            I64XOR => {
                todo!("{:?}", op_code);
            }
            I64SHL => {
                todo!("{:?}", op_code);
            }
            I64SHRS => {
                todo!("{:?}", op_code);
            }
            I64SHRU => {
                todo!("{:?}", op_code);
            }
            I64ROTL => {
                todo!("{:?}", op_code);
            }
            I64ROTR => {
                todo!("{:?}", op_code);
            }
            F32ABS => {
                todo!("{:?}", op_code);
            }
            F32NEG => {
                todo!("{:?}", op_code);
            }
            F32CEIL => {
                todo!("{:?}", op_code);
            }
            F32FLOOR => {
                todo!("{:?}", op_code);
            }
            F32TRUNC => {
                todo!("{:?}", op_code);
            }
            F32NEAREST => {
                todo!("{:?}", op_code);
            }
            F32SQRT => {
                todo!("{:?}", op_code);
            }
            F32ADD => {
                todo!("{:?}", op_code);
            }
            F32SUB => {
                todo!("{:?}", op_code);
            }
            F32MUL => {
                todo!("{:?}", op_code);
            }
            F32DIV => {
                todo!("{:?}", op_code);
            }
            F32MIN => {
                todo!("{:?}", op_code);
            }
            F32MAX => {
                todo!("{:?}", op_code);
            }
            F32COPYSIGN => {
                todo!("{:?}", op_code);
            }
            F64ABS => {
                todo!("{:?}", op_code);
            }
            F64NEG => {
                todo!("{:?}", op_code);
            }
            F64CEIL => {
                todo!("{:?}", op_code);
            }
            F64FLOOR => {
                todo!("{:?}", op_code);
            }
            F64TRUNC => {
                todo!("{:?}", op_code);
            }
            F64NEAREST => {
                todo!("{:?}", op_code);
            }
            F64SQRT => {
                todo!("{:?}", op_code);
            }
            F64ADD => {
                todo!("{:?}", op_code);
            }
            F64SUB => {
                todo!("{:?}", op_code);
            }
            F64MUL => {
                todo!("{:?}", op_code);
            }
            F64DIV => {
                todo!("{:?}", op_code);
            }
            F64MIN => {
                todo!("{:?}", op_code);
            }
            F64MAX => {
                todo!("{:?}", op_code);
            }
            F64COPYSIGN => {
                todo!("{:?}", op_code);
            }

            I32WRAPI64 => {
                todo!("{:?}", op_code);
            }
            I32TRUNCSF32 => {
                todo!("{:?}", op_code);
            }
            I32TRUNCUF32 => {
                todo!("{:?}", op_code);
            }
            I32TRUNCSF64 => {
                todo!("{:?}", op_code);
            }
            I32TRUNCUF64 => {
                todo!("{:?}", op_code);
            }
            I64EXTENDSI32 => {
                todo!("{:?}", op_code);
            }
            I64EXTENDUI32 => {
                todo!("{:?}", op_code);
            }
            I64TRUNCSF32 => {
                todo!("{:?}", op_code);
            }
            I64TRUNCUF32 => {
                todo!("{:?}", op_code);
            }
            I64TRUNCSF64 => {
                todo!("{:?}", op_code);
            }
            I64TRUNCUF64 => {
                todo!("{:?}", op_code);
            }
            F32CONVERTSI32 => {
                todo!("{:?}", op_code);
            }
            F32CONVERTUI32 => {
                todo!("{:?}", op_code);
            }
            F32CONVERTSI64 => {
                todo!("{:?}", op_code);
            }
            F32CONVERTUI64 => {
                todo!("{:?}", op_code);
            }
            F32DEMOTEF64 => {
                todo!("{:?}", op_code);
            }
            F64CONVERTSI32 => {
                todo!("{:?}", op_code);
            }
            F64CONVERTUI32 => {
                todo!("{:?}", op_code);
            }
            F64CONVERTSI64 => {
                todo!("{:?}", op_code);
            }
            F64CONVERTUI64 => {
                todo!("{:?}", op_code);
            }
            F64PROMOTEF32 => {
                todo!("{:?}", op_code);
            }

            I32REINTERPRETF32 => {
                todo!("{:?}", op_code);
            }
            I64REINTERPRETF64 => {
                todo!("{:?}", op_code);
            }
            F32REINTERPRETI32 => {
                todo!("{:?}", op_code);
            }
            F64REINTERPRETI64 => {
                todo!("{:?}", op_code);
            }
        }
    }
}
