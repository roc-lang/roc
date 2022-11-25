use bumpalo::{collections::Vec, Bump};
use roc_wasm_module::opcodes::OpCode;
use roc_wasm_module::parse::Parse;
use roc_wasm_module::sections::{ImportDesc, MemorySection};
use roc_wasm_module::Value;
use roc_wasm_module::{ExportType, WasmModule};

use crate::call_stack::CallStack;
use crate::value_stack::ValueStack;

pub enum Action {
    Continue,
    Break,
}

#[derive(Debug)]
pub struct ExecutionState<'a> {
    #[allow(dead_code)]
    memory: Vec<'a, u8>,

    pub call_stack: CallStack<'a>,
    pub value_stack: ValueStack<'a>,
    pub globals: Vec<'a, Value>,
    pub program_counter: usize,
    block_depth: u32,
    import_signatures: Vec<'a, u32>,
    is_debug_mode: bool,
}

impl<'a> ExecutionState<'a> {
    pub fn new<G>(arena: &'a Bump, memory_pages: u32, program_counter: usize, globals: G) -> Self
    where
        G: IntoIterator<Item = Value>,
    {
        let mem_bytes = memory_pages * MemorySection::PAGE_SIZE;
        ExecutionState {
            memory: Vec::with_capacity_in(mem_bytes as usize, arena),
            call_stack: CallStack::new(arena),
            value_stack: ValueStack::new(arena),
            globals: Vec::from_iter_in(globals, arena),
            program_counter,
            block_depth: 0,
            import_signatures: Vec::new_in(arena),
            is_debug_mode: false,
        }
    }

    pub fn for_module(
        arena: &'a Bump,
        module: &WasmModule<'a>,
        start_fn_name: &str,
        is_debug_mode: bool,
    ) -> Result<Self, String> {
        let mem_bytes = module.memory.min_bytes().map_err(|e| {
            format!(
                "Error parsing Memory section at offset {:#x}:\n{}",
                e.offset, e.message
            )
        })?;

        let globals = module.global.initial_values(arena);

        // Gather imported function signatures into a vector, for simpler lookup
        let import_signatures = {
            let imports_iter = module.import.imports.iter();
            let sig_iter = imports_iter.filter_map(|imp| match imp.description {
                ImportDesc::Func { signature_index } => Some(signature_index),
                _ => None,
            });
            Vec::from_iter_in(sig_iter, arena)
        };

        let mut program_counter = {
            let mut export_iter = module.export.exports.iter();
            let start_fn_index = export_iter
                .find_map(|ex| {
                    if ex.ty == ExportType::Func && ex.name == start_fn_name {
                        Some(ex.index)
                    } else {
                        None
                    }
                })
                .ok_or(format!(
                    "I couldn't find an exported function '{}' in this WebAssembly module",
                    start_fn_name
                ))?;
            let internal_fn_index = start_fn_index as usize - module.import.function_count();
            let mut cursor = module.code.function_offsets[internal_fn_index] as usize;
            let _start_fn_byte_length = u32::parse((), &module.code.bytes, &mut cursor);
            cursor
        };

        let mut value_stack = ValueStack::new(arena);
        let mut call_stack = CallStack::new(arena);
        call_stack.push_frame(
            0, // return_addr
            0, // return_block_depth
            0, // n_args
            &mut value_stack,
            &module.code.bytes,
            &mut program_counter,
        );

        Ok(ExecutionState {
            memory: Vec::with_capacity_in(mem_bytes as usize, arena),
            call_stack,
            value_stack,
            globals,
            program_counter,
            block_depth: 0,
            import_signatures,
            is_debug_mode,
        })
    }

    fn fetch_immediate_u32(&mut self, module: &WasmModule<'a>) -> u32 {
        u32::parse((), &module.code.bytes, &mut self.program_counter).unwrap()
    }

    fn do_return(&mut self) -> Action {
        if let Some((return_addr, block_depth)) = self.call_stack.pop_frame() {
            if self.call_stack.is_empty() {
                // We just popped the stack frame for the entry function. Terminate the program.
                Action::Break
            } else {
                self.program_counter = return_addr as usize;
                self.block_depth = block_depth;
                Action::Continue
            }
        } else {
            // We should never get here with real programs but maybe in tests. Terminate the program.
            Action::Break
        }
    }

    pub fn execute_next_instruction(&mut self, module: &WasmModule<'a>) -> Action {
        use OpCode::*;

        let file_offset = self.program_counter as u32 + module.code.section_offset;
        let op_code = OpCode::from(module.code.bytes[self.program_counter]);
        self.program_counter += 1;

        let mut action = Action::Continue;

        match op_code {
            UNREACHABLE => {
                unreachable!(
                    "WebAssembly `unreachable` instruction at file offset {:#x?}.",
                    file_offset
                );
            }
            NOP => {}
            BLOCK => {
                self.block_depth += 1;
                todo!("{:?} @ {:#x}", op_code, file_offset);
            }
            LOOP => {
                self.block_depth += 1;
                todo!("{:?} @ {:#x}", op_code, file_offset);
            }
            IF => todo!("{:?} @ {:#x}", op_code, file_offset),
            ELSE => todo!("{:?} @ {:#x}", op_code, file_offset),
            END => {
                if self.block_depth == 0 {
                    // implicit RETURN at end of function
                    action = self.do_return();
                } else {
                    self.block_depth -= 1;
                }
            }
            BR => todo!("{:?} @ {:#x}", op_code, file_offset),
            BRIF => todo!("{:?} @ {:#x}", op_code, file_offset),
            BRTABLE => todo!("{:?} @ {:#x}", op_code, file_offset),
            RETURN => {
                action = self.do_return();
            }
            CALL => {
                let index = self.fetch_immediate_u32(module) as usize;

                let signature_index = if index < self.import_signatures.len() {
                    self.import_signatures[index]
                } else {
                    let internal_fn_index = index - self.import_signatures.len();
                    module.function.signatures[internal_fn_index]
                };
                let arg_count = module.types.look_up_arg_count(signature_index);

                let return_addr = self.program_counter as u32;
                self.program_counter = module.code.function_offsets[index] as usize;

                let return_block_depth = self.block_depth;
                self.block_depth = 0;

                let _function_byte_length =
                    u32::parse((), &module.code.bytes, &mut self.program_counter).unwrap();
                self.call_stack.push_frame(
                    return_addr,
                    return_block_depth,
                    arg_count,
                    &mut self.value_stack,
                    &module.code.bytes,
                    &mut self.program_counter,
                );
            }
            CALLINDIRECT => todo!("{:?} @ {:#x}", op_code, file_offset),
            DROP => {
                self.value_stack.pop();
            }
            SELECT => todo!("{:?} @ {:#x}", op_code, file_offset),
            GETLOCAL => {
                let index = self.fetch_immediate_u32(module);
                let value = self.call_stack.get_local(index);
                self.value_stack.push(value);
            }
            SETLOCAL => {
                let index = self.fetch_immediate_u32(module);
                let value = self.value_stack.pop();
                self.call_stack.set_local(index, value);
            }
            TEELOCAL => {
                let index = self.fetch_immediate_u32(module);
                let value = self.value_stack.peek();
                self.call_stack.set_local(index, value);
            }
            GETGLOBAL => {
                let index = self.fetch_immediate_u32(module);
                self.value_stack.push(self.globals[index as usize]);
            }
            SETGLOBAL => {
                let index = self.fetch_immediate_u32(module);
                self.globals[index as usize] = self.value_stack.pop();
            }
            I32LOAD => todo!("{:?} @ {:#x}", op_code, file_offset),
            I64LOAD => todo!("{:?} @ {:#x}", op_code, file_offset),
            F32LOAD => todo!("{:?} @ {:#x}", op_code, file_offset),
            F64LOAD => todo!("{:?} @ {:#x}", op_code, file_offset),
            I32LOAD8S => todo!("{:?} @ {:#x}", op_code, file_offset),
            I32LOAD8U => todo!("{:?} @ {:#x}", op_code, file_offset),
            I32LOAD16S => todo!("{:?} @ {:#x}", op_code, file_offset),
            I32LOAD16U => todo!("{:?} @ {:#x}", op_code, file_offset),
            I64LOAD8S => todo!("{:?} @ {:#x}", op_code, file_offset),
            I64LOAD8U => todo!("{:?} @ {:#x}", op_code, file_offset),
            I64LOAD16S => todo!("{:?} @ {:#x}", op_code, file_offset),
            I64LOAD16U => todo!("{:?} @ {:#x}", op_code, file_offset),
            I64LOAD32S => todo!("{:?} @ {:#x}", op_code, file_offset),
            I64LOAD32U => todo!("{:?} @ {:#x}", op_code, file_offset),
            I32STORE => todo!("{:?} @ {:#x}", op_code, file_offset),
            I64STORE => todo!("{:?} @ {:#x}", op_code, file_offset),
            F32STORE => todo!("{:?} @ {:#x}", op_code, file_offset),
            F64STORE => todo!("{:?} @ {:#x}", op_code, file_offset),
            I32STORE8 => todo!("{:?} @ {:#x}", op_code, file_offset),
            I32STORE16 => todo!("{:?} @ {:#x}", op_code, file_offset),
            I64STORE8 => todo!("{:?} @ {:#x}", op_code, file_offset),
            I64STORE16 => todo!("{:?} @ {:#x}", op_code, file_offset),
            I64STORE32 => todo!("{:?} @ {:#x}", op_code, file_offset),
            CURRENTMEMORY => todo!("{:?} @ {:#x}", op_code, file_offset),
            GROWMEMORY => todo!("{:?} @ {:#x}", op_code, file_offset),
            I32CONST => {
                let value = i32::parse((), &module.code.bytes, &mut self.program_counter).unwrap();
                self.value_stack.push(Value::I32(value));
            }
            I64CONST => {
                let value = i64::parse((), &module.code.bytes, &mut self.program_counter).unwrap();
                self.value_stack.push(Value::I64(value));
            }
            F32CONST => {
                let mut bytes = [0; 4];
                bytes.copy_from_slice(&module.code.bytes[self.program_counter..][..4]);
                self.value_stack.push(Value::F32(f32::from_le_bytes(bytes)));
                self.program_counter += 4;
            }
            F64CONST => {
                let mut bytes = [0; 8];
                bytes.copy_from_slice(&module.code.bytes[self.program_counter..][..8]);
                self.value_stack.push(Value::F64(f64::from_le_bytes(bytes)));
                self.program_counter += 8;
            }
            I32EQZ => todo!("{:?} @ {:#x}", op_code, file_offset),
            I32EQ => todo!("{:?} @ {:#x}", op_code, file_offset),
            I32NE => todo!("{:?} @ {:#x}", op_code, file_offset),
            I32LTS => todo!("{:?} @ {:#x}", op_code, file_offset),
            I32LTU => todo!("{:?} @ {:#x}", op_code, file_offset),
            I32GTS => todo!("{:?} @ {:#x}", op_code, file_offset),
            I32GTU => todo!("{:?} @ {:#x}", op_code, file_offset),
            I32LES => todo!("{:?} @ {:#x}", op_code, file_offset),
            I32LEU => todo!("{:?} @ {:#x}", op_code, file_offset),
            I32GES => todo!("{:?} @ {:#x}", op_code, file_offset),
            I32GEU => todo!("{:?} @ {:#x}", op_code, file_offset),
            I64EQZ => todo!("{:?} @ {:#x}", op_code, file_offset),
            I64EQ => todo!("{:?} @ {:#x}", op_code, file_offset),
            I64NE => todo!("{:?} @ {:#x}", op_code, file_offset),
            I64LTS => todo!("{:?} @ {:#x}", op_code, file_offset),
            I64LTU => todo!("{:?} @ {:#x}", op_code, file_offset),
            I64GTS => todo!("{:?} @ {:#x}", op_code, file_offset),
            I64GTU => todo!("{:?} @ {:#x}", op_code, file_offset),
            I64LES => todo!("{:?} @ {:#x}", op_code, file_offset),
            I64LEU => todo!("{:?} @ {:#x}", op_code, file_offset),
            I64GES => todo!("{:?} @ {:#x}", op_code, file_offset),
            I64GEU => todo!("{:?} @ {:#x}", op_code, file_offset),

            F32EQ => todo!("{:?} @ {:#x}", op_code, file_offset),
            F32NE => todo!("{:?} @ {:#x}", op_code, file_offset),
            F32LT => todo!("{:?} @ {:#x}", op_code, file_offset),
            F32GT => todo!("{:?} @ {:#x}", op_code, file_offset),
            F32LE => todo!("{:?} @ {:#x}", op_code, file_offset),
            F32GE => todo!("{:?} @ {:#x}", op_code, file_offset),

            F64EQ => todo!("{:?} @ {:#x}", op_code, file_offset),
            F64NE => todo!("{:?} @ {:#x}", op_code, file_offset),
            F64LT => todo!("{:?} @ {:#x}", op_code, file_offset),
            F64GT => todo!("{:?} @ {:#x}", op_code, file_offset),
            F64LE => todo!("{:?} @ {:#x}", op_code, file_offset),
            F64GE => todo!("{:?} @ {:#x}", op_code, file_offset),

            I32CLZ => todo!("{:?} @ {:#x}", op_code, file_offset),
            I32CTZ => todo!("{:?} @ {:#x}", op_code, file_offset),
            I32POPCNT => todo!("{:?} @ {:#x}", op_code, file_offset),
            I32ADD => {
                let x = self.value_stack.pop_i32();
                let y = self.value_stack.pop_i32();
                self.value_stack.push(Value::I32(y + x));
            }
            I32SUB => {
                let x = self.value_stack.pop_i32();
                let y = self.value_stack.pop_i32();
                self.value_stack.push(Value::I32(y - x));
            }
            I32MUL => {
                let x = self.value_stack.pop_i32();
                let y = self.value_stack.pop_i32();
                self.value_stack.push(Value::I32(y * x));
            }
            I32DIVS => todo!("{:?} @ {:#x}", op_code, file_offset),
            I32DIVU => todo!("{:?} @ {:#x}", op_code, file_offset),
            I32REMS => todo!("{:?} @ {:#x}", op_code, file_offset),
            I32REMU => todo!("{:?} @ {:#x}", op_code, file_offset),
            I32AND => todo!("{:?} @ {:#x}", op_code, file_offset),
            I32OR => todo!("{:?} @ {:#x}", op_code, file_offset),
            I32XOR => todo!("{:?} @ {:#x}", op_code, file_offset),
            I32SHL => todo!("{:?} @ {:#x}", op_code, file_offset),
            I32SHRS => todo!("{:?} @ {:#x}", op_code, file_offset),
            I32SHRU => todo!("{:?} @ {:#x}", op_code, file_offset),
            I32ROTL => todo!("{:?} @ {:#x}", op_code, file_offset),
            I32ROTR => todo!("{:?} @ {:#x}", op_code, file_offset),

            I64CLZ => todo!("{:?} @ {:#x}", op_code, file_offset),
            I64CTZ => todo!("{:?} @ {:#x}", op_code, file_offset),
            I64POPCNT => todo!("{:?} @ {:#x}", op_code, file_offset),
            I64ADD => todo!("{:?} @ {:#x}", op_code, file_offset),
            I64SUB => todo!("{:?} @ {:#x}", op_code, file_offset),
            I64MUL => todo!("{:?} @ {:#x}", op_code, file_offset),
            I64DIVS => todo!("{:?} @ {:#x}", op_code, file_offset),
            I64DIVU => todo!("{:?} @ {:#x}", op_code, file_offset),
            I64REMS => todo!("{:?} @ {:#x}", op_code, file_offset),
            I64REMU => todo!("{:?} @ {:#x}", op_code, file_offset),
            I64AND => todo!("{:?} @ {:#x}", op_code, file_offset),
            I64OR => todo!("{:?} @ {:#x}", op_code, file_offset),
            I64XOR => todo!("{:?} @ {:#x}", op_code, file_offset),
            I64SHL => todo!("{:?} @ {:#x}", op_code, file_offset),
            I64SHRS => todo!("{:?} @ {:#x}", op_code, file_offset),
            I64SHRU => todo!("{:?} @ {:#x}", op_code, file_offset),
            I64ROTL => todo!("{:?} @ {:#x}", op_code, file_offset),
            I64ROTR => todo!("{:?} @ {:#x}", op_code, file_offset),
            F32ABS => todo!("{:?} @ {:#x}", op_code, file_offset),
            F32NEG => todo!("{:?} @ {:#x}", op_code, file_offset),
            F32CEIL => todo!("{:?} @ {:#x}", op_code, file_offset),
            F32FLOOR => todo!("{:?} @ {:#x}", op_code, file_offset),
            F32TRUNC => todo!("{:?} @ {:#x}", op_code, file_offset),
            F32NEAREST => todo!("{:?} @ {:#x}", op_code, file_offset),
            F32SQRT => todo!("{:?} @ {:#x}", op_code, file_offset),
            F32ADD => todo!("{:?} @ {:#x}", op_code, file_offset),
            F32SUB => todo!("{:?} @ {:#x}", op_code, file_offset),
            F32MUL => todo!("{:?} @ {:#x}", op_code, file_offset),
            F32DIV => todo!("{:?} @ {:#x}", op_code, file_offset),
            F32MIN => todo!("{:?} @ {:#x}", op_code, file_offset),
            F32MAX => todo!("{:?} @ {:#x}", op_code, file_offset),
            F32COPYSIGN => todo!("{:?} @ {:#x}", op_code, file_offset),
            F64ABS => todo!("{:?} @ {:#x}", op_code, file_offset),
            F64NEG => todo!("{:?} @ {:#x}", op_code, file_offset),
            F64CEIL => todo!("{:?} @ {:#x}", op_code, file_offset),
            F64FLOOR => todo!("{:?} @ {:#x}", op_code, file_offset),
            F64TRUNC => todo!("{:?} @ {:#x}", op_code, file_offset),
            F64NEAREST => todo!("{:?} @ {:#x}", op_code, file_offset),
            F64SQRT => todo!("{:?} @ {:#x}", op_code, file_offset),
            F64ADD => todo!("{:?} @ {:#x}", op_code, file_offset),
            F64SUB => todo!("{:?} @ {:#x}", op_code, file_offset),
            F64MUL => todo!("{:?} @ {:#x}", op_code, file_offset),
            F64DIV => todo!("{:?} @ {:#x}", op_code, file_offset),
            F64MIN => todo!("{:?} @ {:#x}", op_code, file_offset),
            F64MAX => todo!("{:?} @ {:#x}", op_code, file_offset),
            F64COPYSIGN => todo!("{:?} @ {:#x}", op_code, file_offset),

            I32WRAPI64 => todo!("{:?} @ {:#x}", op_code, file_offset),
            I32TRUNCSF32 => todo!("{:?} @ {:#x}", op_code, file_offset),
            I32TRUNCUF32 => todo!("{:?} @ {:#x}", op_code, file_offset),
            I32TRUNCSF64 => todo!("{:?} @ {:#x}", op_code, file_offset),
            I32TRUNCUF64 => todo!("{:?} @ {:#x}", op_code, file_offset),
            I64EXTENDSI32 => todo!("{:?} @ {:#x}", op_code, file_offset),
            I64EXTENDUI32 => todo!("{:?} @ {:#x}", op_code, file_offset),
            I64TRUNCSF32 => todo!("{:?} @ {:#x}", op_code, file_offset),
            I64TRUNCUF32 => todo!("{:?} @ {:#x}", op_code, file_offset),
            I64TRUNCSF64 => todo!("{:?} @ {:#x}", op_code, file_offset),
            I64TRUNCUF64 => todo!("{:?} @ {:#x}", op_code, file_offset),
            F32CONVERTSI32 => todo!("{:?} @ {:#x}", op_code, file_offset),
            F32CONVERTUI32 => todo!("{:?} @ {:#x}", op_code, file_offset),
            F32CONVERTSI64 => todo!("{:?} @ {:#x}", op_code, file_offset),
            F32CONVERTUI64 => todo!("{:?} @ {:#x}", op_code, file_offset),
            F32DEMOTEF64 => todo!("{:?} @ {:#x}", op_code, file_offset),
            F64CONVERTSI32 => todo!("{:?} @ {:#x}", op_code, file_offset),
            F64CONVERTUI32 => todo!("{:?} @ {:#x}", op_code, file_offset),
            F64CONVERTSI64 => todo!("{:?} @ {:#x}", op_code, file_offset),
            F64CONVERTUI64 => todo!("{:?} @ {:#x}", op_code, file_offset),
            F64PROMOTEF32 => todo!("{:?} @ {:#x}", op_code, file_offset),

            I32REINTERPRETF32 => todo!("{:?} @ {:#x}", op_code, file_offset),
            I64REINTERPRETF64 => todo!("{:?} @ {:#x}", op_code, file_offset),
            F32REINTERPRETI32 => todo!("{:?} @ {:#x}", op_code, file_offset),
            F64REINTERPRETI64 => todo!("{:?} @ {:#x}", op_code, file_offset),
        }

        if self.is_debug_mode {
            self.print_debug_log_line(file_offset, op_code);
        }

        action
    }

    fn print_debug_log_line(&self, file_offset: u32, op_code: OpCode) {
        let base = self.call_stack.value_stack_base();
        let slice = self.value_stack.get_slice(base as usize);
        eprintln!(
            "{:#07x} {:17}\t{:?}",
            file_offset,
            format!("{:?}", op_code),
            slice
        );
    }
}
