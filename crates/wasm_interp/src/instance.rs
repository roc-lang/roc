use bumpalo::{collections::Vec, Bump};
use std::fmt::{self, Write};
use std::iter;

use roc_wasm_module::opcodes::OpCode;
use roc_wasm_module::parse::{Parse, SkipBytes};
use roc_wasm_module::sections::{ImportDesc, MemorySection};
use roc_wasm_module::{ExportType, WasmModule};
use roc_wasm_module::{Value, ValueType};

use crate::call_stack::CallStack;
use crate::value_stack::ValueStack;

pub enum Action {
    Continue,
    Break,
}

#[derive(Debug)]
pub struct Instance<'a> {
    /// Contents of the WebAssembly instance's memory
    pub memory: Vec<'a, u8>,
    /// Metadata for every currently-active function call
    pub call_stack: CallStack<'a>,
    /// The WebAssembly stack machine's stack of values
    pub value_stack: ValueStack<'a>,
    /// Values of any global variables
    pub globals: Vec<'a, Value>,
    /// Index in the code section of the current instruction
    pub program_counter: usize,
    /// One entry per nested block. For loops, stores the address of the first instruction.
    block_loop_addrs: Vec<'a, Option<u32>>,
    /// Outermost block depth for the currently-executing function.
    outermost_block: u32,
    /// Signature indices (in the TypeSection) of all imported (non-WebAssembly) functions
    import_signatures: Vec<'a, u32>,
    /// temporary storage for output using the --debug option
    debug_string: Option<String>,
}

impl<'a> Instance<'a> {
    pub fn new<G>(arena: &'a Bump, memory_pages: u32, program_counter: usize, globals: G) -> Self
    where
        G: IntoIterator<Item = Value>,
    {
        let mem_bytes = memory_pages * MemorySection::PAGE_SIZE;
        Instance {
            memory: Vec::from_iter_in(iter::repeat(0).take(mem_bytes as usize), arena),
            call_stack: CallStack::new(arena),
            value_stack: ValueStack::new(arena),
            globals: Vec::from_iter_in(globals, arena),
            program_counter,
            block_loop_addrs: Vec::new_in(arena),
            outermost_block: 0,
            import_signatures: Vec::new_in(arena),
            debug_string: Some(String::new()),
        }
    }

    pub fn for_module<'arg, A>(
        arena: &'a Bump,
        module: &WasmModule<'a>,
        start_fn_name: &str,
        is_debug_mode: bool,
        arg_strings: A,
    ) -> Result<Self, std::string::String>
    where
        A: IntoIterator<Item = &'arg str>,
    {
        let mem_bytes = module.memory.min_bytes().map_err(|e| {
            format!(
                "Error parsing Memory section at offset {:#x}:\n{}",
                e.offset, e.message
            )
        })?;
        let mut memory = Vec::from_iter_in(iter::repeat(0).take(mem_bytes as usize), arena);
        module.data.load_into(&mut memory)?;

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

        let start_fn_index = {
            let mut export_iter = module.export.exports.iter();
            export_iter
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
                ))?
        };

        let arg_type_bytes = {
            let internal_fn_index = start_fn_index as usize - import_signatures.len();
            let signature_index = module.function.signatures[internal_fn_index];
            module.types.look_up_arg_type_bytes(signature_index)
        };

        let mut program_counter = {
            let internal_fn_index = start_fn_index as usize - module.import.function_count();
            let mut cursor = module.code.function_offsets[internal_fn_index] as usize;
            let _start_fn_byte_length = u32::parse((), &module.code.bytes, &mut cursor);
            cursor
        };

        let mut value_stack = ValueStack::new(arena);
        for (value_str, type_byte) in arg_strings.into_iter().zip(arg_type_bytes.iter().copied()) {
            use ValueType::*;
            let value = match ValueType::from(type_byte) {
                I32 => Value::I32(value_str.parse::<i32>().map_err(|e| e.to_string())?),
                I64 => Value::I64(value_str.parse::<i64>().map_err(|e| e.to_string())?),
                F32 => Value::F32(value_str.parse::<f32>().map_err(|e| e.to_string())?),
                F64 => Value::F64(value_str.parse::<f64>().map_err(|e| e.to_string())?),
            };
            value_stack.push(value);
        }

        let mut call_stack = CallStack::new(arena);
        call_stack.push_frame(
            0, // return_addr
            0, // return_block_depth
            arg_type_bytes,
            &mut value_stack,
            &module.code.bytes,
            &mut program_counter,
        );

        let debug_string = if is_debug_mode {
            Some(String::new())
        } else {
            None
        };

        Ok(Instance {
            memory,
            call_stack,
            value_stack,
            globals,
            program_counter,
            block_loop_addrs: Vec::new_in(arena),
            outermost_block: 0,
            import_signatures,
            debug_string,
        })
    }

    fn fetch_immediate_u32(&mut self, module: &WasmModule<'a>) -> u32 {
        let x = u32::parse((), &module.code.bytes, &mut self.program_counter).unwrap();
        if let Some(debug_string) = self.debug_string.as_mut() {
            write!(debug_string, "{} ", x).unwrap();
        }
        x
    }

    fn do_return(&mut self) -> Action {
        if let Some((return_addr, block_depth)) = self.call_stack.pop_frame() {
            if self.call_stack.is_empty() {
                // We just popped the stack frame for the entry function. Terminate the program.
                Action::Break
            } else {
                dbg!(return_addr, block_depth);
                self.program_counter = return_addr as usize;
                self.outermost_block = block_depth;
                Action::Continue
            }
        } else {
            // We should never get here with real programs, but maybe in tests. Terminate the program.
            Action::Break
        }
    }

    fn get_load_address(&mut self, module: &WasmModule<'a>) -> u32 {
        // Alignment is not used in the execution steps from the spec! Maybe it's just an optimization hint?
        // https://webassembly.github.io/spec/core/exec/instructions.html#memory-instructions
        // Also note: in the text format we can specify the useless `align=` but not the useful `offset=`!
        let _alignment = self.fetch_immediate_u32(module);
        let offset = self.fetch_immediate_u32(module);
        let base_addr = self.value_stack.pop_u32();
        base_addr + offset
    }

    fn get_store_addr_value(&mut self, module: &WasmModule<'a>) -> (usize, Value) {
        // Alignment is not used in the execution steps from the spec! Maybe it's just an optimization hint?
        // https://webassembly.github.io/spec/core/exec/instructions.html#memory-instructions
        // Also note: in the text format we can specify the useless `align=` but not the useful `offset=`!
        let _alignment = self.fetch_immediate_u32(module);
        let offset = self.fetch_immediate_u32(module);
        let value = self.value_stack.pop();
        let base_addr = self.value_stack.pop_u32();
        let addr = (base_addr + offset) as usize;
        (addr, value)
    }

    fn write_debug<T: fmt::Debug>(&mut self, value: T) {
        if let Some(debug_string) = self.debug_string.as_mut() {
            std::write!(debug_string, "{:?} ", value).unwrap();
        }
    }

    fn do_break(&mut self, relative_blocks_outward: u32, module: &WasmModule<'a>) {
        let block_index = self.block_loop_addrs.len() - 1 - relative_blocks_outward as usize;
        match self.block_loop_addrs[block_index] {
            Some(addr) => {
                self.block_loop_addrs.truncate(block_index + 1);
                self.program_counter = addr as usize;
            }
            None => {
                self.break_forward(relative_blocks_outward, module);
            }
        }
    }

    // Break to an outer block, going forward in the program
    fn break_forward(&mut self, relative_blocks_outward: u32, module: &WasmModule<'a>) {
        use OpCode::*;

        let mut depth = self.block_loop_addrs.len();
        let target_block_depth = depth - (relative_blocks_outward + 1) as usize;
        loop {
            let skipped_op = OpCode::from(module.code.bytes[self.program_counter]);
            OpCode::skip_bytes(&module.code.bytes, &mut self.program_counter).unwrap();
            match skipped_op {
                BLOCK | LOOP | IF => {
                    depth += 1;
                }
                END => {
                    depth -= 1;
                    if depth == target_block_depth {
                        break;
                    }
                }
                _ => {}
            }
        }
        self.block_loop_addrs.truncate(target_block_depth);
    }

    fn do_call(
        &mut self,
        expected_signature: Option<u32>,
        fn_index: usize,
        module: &WasmModule<'a>,
    ) {
        let n_imports = self.import_signatures.len();
        let signature_index: u32 = if fn_index < n_imports {
            self.import_signatures[fn_index]
        } else {
            module.function.signatures[fn_index - n_imports]
        };

        if let Some(expected) = expected_signature {
            assert_eq!(
                expected, signature_index,
                "Indirect function call failed. Expected signature {} but found {}",
                expected, signature_index,
            );
        }

        let arg_type_bytes = module.types.look_up_arg_type_bytes(signature_index);

        let return_addr = self.program_counter as u32;
        self.program_counter = module.code.function_offsets[fn_index] as usize;

        let return_block_depth = self.outermost_block;
        self.outermost_block = self.block_loop_addrs.len() as u32;

        let _function_byte_length =
            u32::parse((), &module.code.bytes, &mut self.program_counter).unwrap();
        self.call_stack.push_frame(
            return_addr,
            return_block_depth,
            arg_type_bytes,
            &mut self.value_stack,
            &module.code.bytes,
            &mut self.program_counter,
        );
    }

    pub fn execute_next_instruction(&mut self, module: &WasmModule<'a>) -> Action {
        use OpCode::*;

        let file_offset = self.program_counter as u32 + module.code.section_offset;
        let op_code = OpCode::from(module.code.bytes[self.program_counter]);
        self.program_counter += 1;

        if let Some(debug_string) = self.debug_string.as_mut() {
            debug_string.clear();
            self.write_debug(op_code);
        }

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
                self.fetch_immediate_u32(module); // blocktype (ignored)
                self.block_loop_addrs.push(None);
            }
            LOOP => {
                self.fetch_immediate_u32(module); // blocktype (ignored)
                self.block_loop_addrs
                    .push(Some(self.program_counter as u32));
            }
            IF => {
                self.fetch_immediate_u32(module); // blocktype (ignored)
                let condition = self.value_stack.pop_i32();
                self.block_loop_addrs.push(None);
                if condition == 0 {
                    let mut depth = self.block_loop_addrs.len();
                    loop {
                        let skipped_op = OpCode::from(module.code.bytes[self.program_counter]);
                        OpCode::skip_bytes(&module.code.bytes, &mut self.program_counter).unwrap();
                        match skipped_op {
                            BLOCK | LOOP | IF => {
                                depth += 1;
                            }
                            END => {
                                depth -= 1;
                            }
                            ELSE => {
                                if depth == self.block_loop_addrs.len() {
                                    break;
                                }
                            }
                            _ => {}
                        }
                    }
                }
            }
            ELSE => {
                // We only reach this point when we finish executing the "then" block of an IF statement
                // (For a false condition, we would have skipped past the ELSE when we saw the IF)
                // We don't want to execute the ELSE block, so we skip it, just like `br 0` would.
                self.do_break(0, module);
            }
            END => {
                if self.block_loop_addrs.len() == self.outermost_block as usize {
                    // implicit RETURN at end of function
                    action = self.do_return();
                } else {
                    self.block_loop_addrs.pop().unwrap();
                }
            }
            BR => {
                let relative_blocks_outward = self.fetch_immediate_u32(module);
                self.do_break(relative_blocks_outward, module);
            }
            BRIF => {
                let relative_blocks_outward = self.fetch_immediate_u32(module);
                let condition = self.value_stack.pop_i32();
                if condition != 0 {
                    self.do_break(relative_blocks_outward, module);
                }
            }
            BRTABLE => {
                let selector = self.value_stack.pop_u32();
                let nondefault_condition_count = self.fetch_immediate_u32(module);
                let mut selected = None;
                for i in 0..nondefault_condition_count {
                    let rel_blocks = self.fetch_immediate_u32(module);
                    if i == selector {
                        selected = Some(rel_blocks);
                    }
                }
                let fallback = self.fetch_immediate_u32(module);
                let relative_blocks_outward = selected.unwrap_or(fallback);
                self.do_break(relative_blocks_outward, module);
            }
            RETURN => {
                action = self.do_return();
            }
            CALL => {
                let fn_index = self.fetch_immediate_u32(module) as usize;
                self.do_call(None, fn_index, module);
            }
            CALLINDIRECT => {
                let table_index = self.fetch_immediate_u32(module);
                let expected_signature = self.fetch_immediate_u32(module);
                let element_index = self.value_stack.pop_u32();

                // So far, all compilers seem to be emitting MVP-compatible code. (Rust, Zig, Roc...)
                assert_eq!(
                    table_index, 0,
                    "Table index {} not supported at file offset {:#x}. This interpreter only supports Wasm MVP.",
                    table_index, file_offset
                );

                // Dereference the function pointer (look up the element index in the function table)
                let fn_index = module.element.lookup(element_index).unwrap_or_else(|| {
                    panic!(
                        "Indirect function call failed. There is no function with element index {}",
                        element_index
                    )
                });

                self.do_call(Some(expected_signature), fn_index as usize, module);
            }
            DROP => {
                self.value_stack.pop();
            }
            SELECT => {
                let c = self.value_stack.pop_i32();
                let val2 = self.value_stack.pop();
                let val1 = self.value_stack.pop();
                assert_eq!(ValueType::from(val1), ValueType::from(val2));
                let result = if c != 0 { val1 } else { val2 };
                self.value_stack.push(result);
            }
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
            I32LOAD => {
                let addr = self.get_load_address(module) as usize;
                let mut bytes = [0; 4];
                bytes.copy_from_slice(&self.memory[addr..][..4]);
                let value = i32::from_le_bytes(bytes);
                self.value_stack.push(Value::I32(value));
            }
            I64LOAD => {
                let addr = self.get_load_address(module) as usize;
                let mut bytes = [0; 8];
                bytes.copy_from_slice(&self.memory[addr..][..8]);
                let value = i64::from_le_bytes(bytes);
                self.value_stack.push(Value::I64(value));
            }
            F32LOAD => {
                let addr = self.get_load_address(module) as usize;
                let mut bytes = [0; 4];
                bytes.copy_from_slice(&self.memory[addr..][..4]);
                let value = f32::from_le_bytes(bytes);
                self.value_stack.push(Value::F32(value));
            }
            F64LOAD => {
                let addr = self.get_load_address(module) as usize;
                let mut bytes = [0; 8];
                bytes.copy_from_slice(&self.memory[addr..][..8]);
                let value = f64::from_le_bytes(bytes);
                self.value_stack.push(Value::F64(value));
            }
            I32LOAD8S => {
                let addr = self.get_load_address(module) as usize;
                let mut bytes = [0; 1];
                bytes.copy_from_slice(&self.memory[addr..][..1]);
                let value = i8::from_le_bytes(bytes);
                self.value_stack.push(Value::I32(value as i32));
            }
            I32LOAD8U => {
                let addr = self.get_load_address(module) as usize;
                let value = self.memory[addr];
                self.value_stack.push(Value::I32(value as i32));
            }
            I32LOAD16S => {
                let addr = self.get_load_address(module) as usize;
                let mut bytes = [0; 2];
                bytes.copy_from_slice(&self.memory[addr..][..2]);
                let value = i16::from_le_bytes(bytes);
                self.value_stack.push(Value::I32(value as i32));
            }
            I32LOAD16U => {
                let addr = self.get_load_address(module) as usize;
                let mut bytes = [0; 2];
                bytes.copy_from_slice(&self.memory[addr..][..2]);
                let value = u16::from_le_bytes(bytes);
                self.value_stack.push(Value::I32(value as i32));
            }
            I64LOAD8S => {
                let addr = self.get_load_address(module) as usize;
                let mut bytes = [0; 1];
                bytes.copy_from_slice(&self.memory[addr..][..1]);
                let value = i8::from_le_bytes(bytes);
                self.value_stack.push(Value::I64(value as i64));
            }
            I64LOAD8U => {
                let addr = self.get_load_address(module) as usize;
                let value = self.memory[addr];
                self.value_stack.push(Value::I64(value as i64));
            }
            I64LOAD16S => {
                let addr = self.get_load_address(module) as usize;
                let mut bytes = [0; 2];
                bytes.copy_from_slice(&self.memory[addr..][..2]);
                let value = i16::from_le_bytes(bytes);
                self.value_stack.push(Value::I64(value as i64));
            }
            I64LOAD16U => {
                let addr = self.get_load_address(module) as usize;
                let mut bytes = [0; 2];
                bytes.copy_from_slice(&self.memory[addr..][..2]);
                let value = u16::from_le_bytes(bytes);
                self.value_stack.push(Value::I64(value as i64));
            }
            I64LOAD32S => {
                let addr = self.get_load_address(module) as usize;
                let mut bytes = [0; 4];
                bytes.copy_from_slice(&self.memory[addr..][..4]);
                let value = i32::from_le_bytes(bytes);
                self.value_stack.push(Value::I64(value as i64));
            }
            I64LOAD32U => {
                let addr = self.get_load_address(module) as usize;
                let mut bytes = [0; 4];
                bytes.copy_from_slice(&self.memory[addr..][..4]);
                let value = u32::from_le_bytes(bytes);
                self.value_stack.push(Value::I64(value as i64));
            }
            I32STORE => {
                let (addr, value) = self.get_store_addr_value(module);
                let unwrapped = value.unwrap_i32();
                let target = &mut self.memory[addr..][..4];
                target.copy_from_slice(&unwrapped.to_le_bytes());
            }
            I64STORE => {
                let (addr, value) = self.get_store_addr_value(module);
                let unwrapped = value.unwrap_i64();
                let target = &mut self.memory[addr..][..8];
                target.copy_from_slice(&unwrapped.to_le_bytes());
            }
            F32STORE => {
                let (addr, value) = self.get_store_addr_value(module);
                let unwrapped = value.unwrap_f32();
                let target = &mut self.memory[addr..][..4];
                target.copy_from_slice(&unwrapped.to_le_bytes());
            }
            F64STORE => {
                let (addr, value) = self.get_store_addr_value(module);
                let unwrapped = value.unwrap_f64();
                let target = &mut self.memory[addr..][..8];
                target.copy_from_slice(&unwrapped.to_le_bytes());
            }
            I32STORE8 => {
                let (addr, value) = self.get_store_addr_value(module);
                let unwrapped = value.unwrap_i32();
                let target = &mut self.memory[addr..][..1];
                target.copy_from_slice(&unwrapped.to_le_bytes()[..1]);
            }
            I32STORE16 => {
                let (addr, value) = self.get_store_addr_value(module);
                let unwrapped = value.unwrap_i32();
                let target = &mut self.memory[addr..][..2];
                target.copy_from_slice(&unwrapped.to_le_bytes()[..2]);
            }
            I64STORE8 => {
                let (addr, value) = self.get_store_addr_value(module);
                let unwrapped = value.unwrap_i64();
                let target = &mut self.memory[addr..][..1];
                target.copy_from_slice(&unwrapped.to_le_bytes()[..1]);
            }
            I64STORE16 => {
                let (addr, value) = self.get_store_addr_value(module);
                let unwrapped = value.unwrap_i64();
                let target = &mut self.memory[addr..][..2];
                target.copy_from_slice(&unwrapped.to_le_bytes()[..2]);
            }
            I64STORE32 => {
                let (addr, value) = self.get_store_addr_value(module);
                let unwrapped = value.unwrap_i64();
                let target = &mut self.memory[addr..][..4];
                target.copy_from_slice(&unwrapped.to_le_bytes()[..4]);
            }
            CURRENTMEMORY => {
                let size = self.memory.len() as i32 / MemorySection::PAGE_SIZE as i32;
                self.value_stack.push(Value::I32(size));
            }
            GROWMEMORY => {
                let old_bytes = self.memory.len() as u32;
                let old_pages = old_bytes / MemorySection::PAGE_SIZE as u32;
                let grow_pages = self.value_stack.pop_u32();
                let grow_bytes = grow_pages * MemorySection::PAGE_SIZE;
                let new_bytes = old_bytes + grow_bytes;

                let success = match module.memory.max_bytes().unwrap() {
                    Some(max_bytes) => new_bytes <= max_bytes,
                    None => true,
                };
                if success {
                    self.memory
                        .extend(iter::repeat(0).take(grow_bytes as usize));
                    self.value_stack.push(Value::I32(old_pages as i32));
                } else {
                    self.value_stack.push(Value::I32(-1));
                }
            }
            I32CONST => {
                let value = i32::parse((), &module.code.bytes, &mut self.program_counter).unwrap();
                self.write_debug(value);
                self.value_stack.push(Value::I32(value));
            }
            I64CONST => {
                let value = i64::parse((), &module.code.bytes, &mut self.program_counter).unwrap();
                self.write_debug(value);
                self.value_stack.push(Value::I64(value));
            }
            F32CONST => {
                let mut bytes = [0; 4];
                bytes.copy_from_slice(&module.code.bytes[self.program_counter..][..4]);
                let value = f32::from_le_bytes(bytes);
                self.write_debug(value);
                self.value_stack.push(Value::F32(value));
                self.program_counter += 4;
            }
            F64CONST => {
                let mut bytes = [0; 8];
                bytes.copy_from_slice(&module.code.bytes[self.program_counter..][..8]);
                let value = f64::from_le_bytes(bytes);
                self.write_debug(value);
                self.value_stack.push(Value::F64(value));
                self.program_counter += 8;
            }

            I32EQZ => {
                let arg = self.value_stack.pop_i32();
                let result: bool = arg == 0;
                self.value_stack.push(Value::I32(result as i32));
            }
            I32EQ => {
                let arg2 = self.value_stack.pop_i32();
                let arg1 = self.value_stack.pop_i32();
                let result: bool = arg1 == arg2;
                self.value_stack.push(Value::I32(result as i32));
            }
            I32NE => {
                let arg2 = self.value_stack.pop_i32();
                let arg1 = self.value_stack.pop_i32();
                let result: bool = arg1 != arg2;
                self.value_stack.push(Value::I32(result as i32));
            }
            I32LTS => {
                let arg2 = self.value_stack.pop_i32();
                let arg1 = self.value_stack.pop_i32();
                let result: bool = arg1 < arg2;
                self.value_stack.push(Value::I32(result as i32));
            }
            I32LTU => {
                let arg2 = self.value_stack.pop_u32();
                let arg1 = self.value_stack.pop_u32();
                let result: bool = arg1 < arg2;
                self.value_stack.push(Value::I32(result as i32));
            }
            I32GTS => {
                let arg2 = self.value_stack.pop_i32();
                let arg1 = self.value_stack.pop_i32();
                let result: bool = arg1 > arg2;
                self.value_stack.push(Value::I32(result as i32));
            }
            I32GTU => {
                let arg2 = self.value_stack.pop_u32();
                let arg1 = self.value_stack.pop_u32();
                let result: bool = arg1 > arg2;
                self.value_stack.push(Value::I32(result as i32));
            }
            I32LES => {
                let arg2 = self.value_stack.pop_i32();
                let arg1 = self.value_stack.pop_i32();
                let result: bool = arg1 <= arg2;
                self.value_stack.push(Value::I32(result as i32));
            }
            I32LEU => {
                let arg2 = self.value_stack.pop_u32();
                let arg1 = self.value_stack.pop_u32();
                let result: bool = arg1 <= arg2;
                self.value_stack.push(Value::I32(result as i32));
            }
            I32GES => {
                let arg2 = self.value_stack.pop_i32();
                let arg1 = self.value_stack.pop_i32();
                let result: bool = arg1 >= arg2;
                self.value_stack.push(Value::I32(result as i32));
            }
            I32GEU => {
                let arg2 = self.value_stack.pop_u32();
                let arg1 = self.value_stack.pop_u32();
                let result: bool = arg1 >= arg2;
                self.value_stack.push(Value::I32(result as i32));
            }

            I64EQZ => {
                let arg = self.value_stack.pop_i64();
                let result: bool = arg == 0;
                self.value_stack.push(Value::I32(result as i32));
            }
            I64EQ => {
                let arg2 = self.value_stack.pop_i64();
                let arg1 = self.value_stack.pop_i64();
                let result: bool = arg1 == arg2;
                self.value_stack.push(Value::I32(result as i32));
            }
            I64NE => {
                let arg2 = self.value_stack.pop_i64();
                let arg1 = self.value_stack.pop_i64();
                let result: bool = arg1 != arg2;
                self.value_stack.push(Value::I32(result as i32));
            }
            I64LTS => {
                let arg2 = self.value_stack.pop_i64();
                let arg1 = self.value_stack.pop_i64();
                let result: bool = arg1 < arg2;
                self.value_stack.push(Value::I32(result as i32));
            }
            I64LTU => {
                let arg2 = self.value_stack.pop_u64();
                let arg1 = self.value_stack.pop_u64();
                let result: bool = arg1 < arg2;
                self.value_stack.push(Value::I32(result as i32));
            }
            I64GTS => {
                let arg2 = self.value_stack.pop_i64();
                let arg1 = self.value_stack.pop_i64();
                let result: bool = arg1 > arg2;
                self.value_stack.push(Value::I32(result as i32));
            }
            I64GTU => {
                let arg2 = self.value_stack.pop_u64();
                let arg1 = self.value_stack.pop_u64();
                let result: bool = arg1 > arg2;
                self.value_stack.push(Value::I32(result as i32));
            }
            I64LES => {
                let arg2 = self.value_stack.pop_i64();
                let arg1 = self.value_stack.pop_i64();
                let result: bool = arg1 <= arg2;
                self.value_stack.push(Value::I32(result as i32));
            }
            I64LEU => {
                let arg2 = self.value_stack.pop_u64();
                let arg1 = self.value_stack.pop_u64();
                let result: bool = arg1 <= arg2;
                self.value_stack.push(Value::I32(result as i32));
            }
            I64GES => {
                let arg2 = self.value_stack.pop_i64();
                let arg1 = self.value_stack.pop_i64();
                let result: bool = arg1 >= arg2;
                self.value_stack.push(Value::I32(result as i32));
            }
            I64GEU => {
                let arg2 = self.value_stack.pop_u64();
                let arg1 = self.value_stack.pop_u64();
                let result: bool = arg1 >= arg2;
                self.value_stack.push(Value::I32(result as i32));
            }

            F32EQ => {
                let arg2 = self.value_stack.pop_f32();
                let arg1 = self.value_stack.pop_f32();
                let result: bool = arg1 == arg2;
                self.value_stack.push(Value::I32(result as i32));
            }
            F32NE => {
                let arg2 = self.value_stack.pop_f32();
                let arg1 = self.value_stack.pop_f32();
                let result: bool = arg1 != arg2;
                self.value_stack.push(Value::I32(result as i32));
            }
            F32LT => {
                let arg2 = self.value_stack.pop_f32();
                let arg1 = self.value_stack.pop_f32();
                let result: bool = arg1 < arg2;
                self.value_stack.push(Value::I32(result as i32));
            }
            F32GT => {
                let arg2 = self.value_stack.pop_f32();
                let arg1 = self.value_stack.pop_f32();
                let result: bool = arg1 > arg2;
                self.value_stack.push(Value::I32(result as i32));
            }
            F32LE => {
                let arg2 = self.value_stack.pop_f32();
                let arg1 = self.value_stack.pop_f32();
                let result: bool = arg1 <= arg2;
                self.value_stack.push(Value::I32(result as i32));
            }
            F32GE => {
                let arg2 = self.value_stack.pop_f32();
                let arg1 = self.value_stack.pop_f32();
                let result: bool = arg1 >= arg2;
                self.value_stack.push(Value::I32(result as i32));
            }

            F64EQ => {
                let arg2 = self.value_stack.pop_f64();
                let arg1 = self.value_stack.pop_f64();
                let result: bool = arg1 == arg2;
                self.value_stack.push(Value::I32(result as i32));
            }
            F64NE => {
                let arg2 = self.value_stack.pop_f64();
                let arg1 = self.value_stack.pop_f64();
                let result: bool = arg1 != arg2;
                self.value_stack.push(Value::I32(result as i32));
            }
            F64LT => {
                let arg2 = self.value_stack.pop_f64();
                let arg1 = self.value_stack.pop_f64();
                let result: bool = arg1 < arg2;
                self.value_stack.push(Value::I32(result as i32));
            }
            F64GT => {
                let arg2 = self.value_stack.pop_f64();
                let arg1 = self.value_stack.pop_f64();
                let result: bool = arg1 > arg2;
                self.value_stack.push(Value::I32(result as i32));
            }
            F64LE => {
                let arg2 = self.value_stack.pop_f64();
                let arg1 = self.value_stack.pop_f64();
                let result: bool = arg1 <= arg2;
                self.value_stack.push(Value::I32(result as i32));
            }
            F64GE => {
                let arg2 = self.value_stack.pop_f64();
                let arg1 = self.value_stack.pop_f64();
                let result: bool = arg1 >= arg2;
                self.value_stack.push(Value::I32(result as i32));
            }

            I32CLZ => {
                let arg = self.value_stack.pop_u32();
                self.value_stack.push(Value::from(arg.leading_zeros()));
            }
            I32CTZ => {
                let arg = self.value_stack.pop_u32();
                self.value_stack.push(Value::from(arg.trailing_zeros()));
            }
            I32POPCNT => {
                let arg = self.value_stack.pop_u32();
                self.value_stack.push(Value::from(arg.count_ones()));
            }
            I32ADD => {
                let arg2 = self.value_stack.pop_i32();
                let arg1 = self.value_stack.pop_i32();
                self.value_stack.push(Value::from(arg1.wrapping_add(arg2)));
            }
            I32SUB => {
                let arg2 = self.value_stack.pop_i32();
                let arg1 = self.value_stack.pop_i32();
                self.value_stack.push(Value::from(arg1.wrapping_sub(arg2)));
            }
            I32MUL => {
                let arg2 = self.value_stack.pop_i32();
                let arg1 = self.value_stack.pop_i32();
                self.value_stack.push(Value::from(arg1.wrapping_mul(arg2)));
            }
            I32DIVS => {
                let arg2 = self.value_stack.pop_i32();
                let arg1 = self.value_stack.pop_i32();
                self.value_stack.push(Value::from(arg1.wrapping_div(arg2)));
            }
            I32DIVU => {
                let arg2 = self.value_stack.pop_u32();
                let arg1 = self.value_stack.pop_u32();
                self.value_stack.push(Value::from(arg1.wrapping_div(arg2)));
            }
            I32REMS => {
                let arg2 = self.value_stack.pop_i32();
                let arg1 = self.value_stack.pop_i32();
                self.value_stack.push(Value::from(arg1.wrapping_rem(arg2)));
            }
            I32REMU => {
                let arg2 = self.value_stack.pop_u32();
                let arg1 = self.value_stack.pop_u32();
                self.value_stack.push(Value::from(arg1.wrapping_rem(arg2)));
            }
            I32AND => {
                let arg2 = self.value_stack.pop_u32();
                let arg1 = self.value_stack.pop_u32();
                self.value_stack.push(Value::from(arg1 & arg2));
            }
            I32OR => {
                let arg2 = self.value_stack.pop_u32();
                let arg1 = self.value_stack.pop_u32();
                self.value_stack.push(Value::from(arg1 | arg2));
            }
            I32XOR => {
                let arg2 = self.value_stack.pop_u32();
                let arg1 = self.value_stack.pop_u32();
                self.value_stack.push(Value::from(arg1 ^ arg2));
            }
            I32SHL => {
                let arg2 = self.value_stack.pop_u32();
                let arg1 = self.value_stack.pop_u32();
                // Take modulo N as per the spec https://webassembly.github.io/spec/core/exec/numerics.html#op-ishl
                let k = arg2 % 32;
                self.value_stack.push(Value::from(arg1 << k));
            }
            I32SHRS => {
                let arg2 = self.value_stack.pop_i32();
                let arg1 = self.value_stack.pop_i32();
                let k = arg2 % 32;
                self.value_stack.push(Value::from(arg1 >> k));
            }
            I32SHRU => {
                let arg2 = self.value_stack.pop_u32();
                let arg1 = self.value_stack.pop_u32();
                let k = arg2 % 32;
                self.value_stack.push(Value::from(arg1 >> k));
            }
            I32ROTL => {
                let arg2 = self.value_stack.pop_u32();
                let arg1 = self.value_stack.pop_u32();
                let k = arg2 % 32;
                self.value_stack.push(Value::from(arg1.rotate_left(k)));
            }
            I32ROTR => {
                let arg2 = self.value_stack.pop_u32();
                let arg1 = self.value_stack.pop_u32();
                let k = arg2 % 32;
                self.value_stack.push(Value::from(arg1.rotate_right(k)));
            }

            I64CLZ => {
                let arg = self.value_stack.pop_u64();
                self.value_stack
                    .push(Value::from(arg.leading_zeros() as u64));
            }
            I64CTZ => {
                let arg = self.value_stack.pop_u64();
                self.value_stack
                    .push(Value::from(arg.trailing_zeros() as u64));
            }
            I64POPCNT => {
                let arg = self.value_stack.pop_u64();
                self.value_stack.push(Value::from(arg.count_ones() as u64));
            }
            I64ADD => {
                let arg2 = self.value_stack.pop_i64();
                let arg1 = self.value_stack.pop_i64();
                self.value_stack.push(Value::from(arg1.wrapping_add(arg2)));
            }
            I64SUB => {
                let arg2 = self.value_stack.pop_i64();
                let arg1 = self.value_stack.pop_i64();
                self.value_stack.push(Value::from(arg1.wrapping_sub(arg2)));
            }
            I64MUL => {
                let arg2 = self.value_stack.pop_i64();
                let arg1 = self.value_stack.pop_i64();
                self.value_stack.push(Value::from(arg1.wrapping_mul(arg2)));
            }
            I64DIVS => {
                let arg2 = self.value_stack.pop_i64();
                let arg1 = self.value_stack.pop_i64();
                self.value_stack.push(Value::from(arg1.wrapping_div(arg2)));
            }
            I64DIVU => {
                let arg2 = self.value_stack.pop_u64();
                let arg1 = self.value_stack.pop_u64();
                self.value_stack.push(Value::from(arg1.wrapping_div(arg2)));
            }
            I64REMS => {
                let arg2 = self.value_stack.pop_i64();
                let arg1 = self.value_stack.pop_i64();
                self.value_stack.push(Value::from(arg1.wrapping_rem(arg2)));
            }
            I64REMU => {
                let arg2 = self.value_stack.pop_u64();
                let arg1 = self.value_stack.pop_u64();
                self.value_stack.push(Value::from(arg1.wrapping_rem(arg2)));
            }
            I64AND => {
                let arg2 = self.value_stack.pop_u64();
                let arg1 = self.value_stack.pop_u64();
                self.value_stack.push(Value::from(arg1 & arg2));
            }
            I64OR => {
                let arg2 = self.value_stack.pop_u64();
                let arg1 = self.value_stack.pop_u64();
                self.value_stack.push(Value::from(arg1 | arg2));
            }
            I64XOR => {
                let arg2 = self.value_stack.pop_u64();
                let arg1 = self.value_stack.pop_u64();
                self.value_stack.push(Value::from(arg1 ^ arg2));
            }
            I64SHL => {
                let arg2 = self.value_stack.pop_u64();
                let arg1 = self.value_stack.pop_u64();
                // Take modulo N as per the spec https://webassembly.github.io/spec/core/exec/numerics.html#op-ishl
                let k = arg2 % 64;
                self.value_stack.push(Value::from(arg1 << k));
            }
            I64SHRS => {
                let arg2 = self.value_stack.pop_i64();
                let arg1 = self.value_stack.pop_i64();
                let k = arg2 % 64;
                self.value_stack.push(Value::from(arg1 >> k));
            }
            I64SHRU => {
                let arg2 = self.value_stack.pop_u64();
                let arg1 = self.value_stack.pop_u64();
                let k = arg2 % 64;
                self.value_stack.push(Value::from(arg1 >> k));
            }
            I64ROTL => {
                let arg2 = self.value_stack.pop_u64();
                let arg1 = self.value_stack.pop_u64();
                let k = (arg2 % 64) as u32;
                self.value_stack.push(Value::from(arg1.rotate_left(k)));
            }
            I64ROTR => {
                let arg2 = self.value_stack.pop_u64();
                let arg1 = self.value_stack.pop_u64();
                let k = (arg2 % 64) as u32;
                self.value_stack.push(Value::from(arg1.rotate_right(k)));
            }

            F32ABS => {
                let arg = self.value_stack.pop_f32();
                self.value_stack.push(Value::F32(arg.abs()));
            }
            F32NEG => {
                let arg = self.value_stack.pop_f32();
                self.value_stack.push(Value::F32(-arg));
            }
            F32CEIL => {
                let arg = self.value_stack.pop_f32();
                self.value_stack.push(Value::F32(arg.ceil()));
            }
            F32FLOOR => {
                let arg = self.value_stack.pop_f32();
                self.value_stack.push(Value::F32(arg.floor()));
            }
            F32TRUNC => {
                let arg = self.value_stack.pop_f32();
                self.value_stack.push(Value::F32(arg.trunc()));
            }
            F32NEAREST => {
                // https://webassembly.github.io/spec/core/exec/numerics.html#op-fnearest
                let arg = self.value_stack.pop_f32();
                let rounded = arg.round(); // "Rounds half-way cases away from 0.0"
                let frac = arg - rounded;
                let result = if frac == 0.5 || frac == -0.5 {
                    let rounded_half = rounded / 2.0;
                    let is_rounded_even = rounded_half.trunc() == rounded_half;
                    if is_rounded_even {
                        rounded
                    } else if rounded < arg {
                        rounded + 1.0
                    } else {
                        rounded - 1.0
                    }
                } else {
                    rounded
                };
                self.value_stack.push(Value::F32(result));
            }
            F32SQRT => {
                let arg = self.value_stack.pop_f32();
                self.value_stack.push(Value::F32(arg.sqrt()));
            }
            F32ADD => {
                let arg2 = self.value_stack.pop_f32();
                let arg1 = self.value_stack.pop_f32();
                self.value_stack.push(Value::F32(arg1 + arg2));
            }
            F32SUB => {
                let arg2 = self.value_stack.pop_f32();
                let arg1 = self.value_stack.pop_f32();
                self.value_stack.push(Value::F32(arg1 - arg2));
            }
            F32MUL => {
                let arg2 = self.value_stack.pop_f32();
                let arg1 = self.value_stack.pop_f32();
                self.value_stack.push(Value::F32(arg1 * arg2));
            }
            F32DIV => {
                let arg2 = self.value_stack.pop_f32();
                let arg1 = self.value_stack.pop_f32();
                self.value_stack.push(Value::F32(arg1 / arg2));
            }
            F32MIN => {
                let arg2 = self.value_stack.pop_f32();
                let arg1 = self.value_stack.pop_f32();
                let result = if arg1 < arg2 { arg1 } else { arg2 };
                self.value_stack.push(Value::F32(result));
            }
            F32MAX => {
                let arg2 = self.value_stack.pop_f32();
                let arg1 = self.value_stack.pop_f32();
                let result = if arg1 > arg2 { arg1 } else { arg2 };
                self.value_stack.push(Value::F32(result));
            }
            F32COPYSIGN => {
                let arg2 = self.value_stack.pop_f32();
                let arg1 = self.value_stack.pop_f32();
                let result = if arg1.is_sign_negative() == arg2.is_sign_negative() {
                    arg1
                } else {
                    arg2
                };
                self.value_stack.push(Value::F32(result));
            }

            F64ABS => {
                let arg = self.value_stack.pop_f64();
                self.value_stack.push(Value::F64(arg.abs()));
            }
            F64NEG => {
                let arg = self.value_stack.pop_f64();
                self.value_stack.push(Value::F64(-arg));
            }
            F64CEIL => {
                let arg = self.value_stack.pop_f64();
                self.value_stack.push(Value::F64(arg.ceil()));
            }
            F64FLOOR => {
                let arg = self.value_stack.pop_f64();
                self.value_stack.push(Value::F64(arg.floor()));
            }
            F64TRUNC => {
                let arg = self.value_stack.pop_f64();
                self.value_stack.push(Value::F64(arg.trunc()));
            }
            F64NEAREST => {
                // https://webassembly.github.io/spec/core/exec/numerics.html#op-fnearest
                let arg = self.value_stack.pop_f64();
                let rounded = arg.round(); // "Rounds half-way cases away from 0.0"
                let frac = arg - rounded;
                let result = if frac == 0.5 || frac == -0.5 {
                    let rounded_half = rounded / 2.0;
                    let is_rounded_even = rounded_half.trunc() == rounded_half;
                    if is_rounded_even {
                        rounded
                    } else if rounded < arg {
                        rounded + 1.0
                    } else {
                        rounded - 1.0
                    }
                } else {
                    rounded
                };
                self.value_stack.push(Value::F64(result));
            }
            F64SQRT => {
                let arg = self.value_stack.pop_f64();
                self.value_stack.push(Value::F64(arg.sqrt()));
            }
            F64ADD => {
                let arg2 = self.value_stack.pop_f64();
                let arg1 = self.value_stack.pop_f64();
                self.value_stack.push(Value::F64(arg1 + arg2));
            }
            F64SUB => {
                let arg2 = self.value_stack.pop_f64();
                let arg1 = self.value_stack.pop_f64();
                self.value_stack.push(Value::F64(arg1 - arg2));
            }
            F64MUL => {
                let arg2 = self.value_stack.pop_f64();
                let arg1 = self.value_stack.pop_f64();
                self.value_stack.push(Value::F64(arg1 * arg2));
            }
            F64DIV => {
                let arg2 = self.value_stack.pop_f64();
                let arg1 = self.value_stack.pop_f64();
                self.value_stack.push(Value::F64(arg1 / arg2));
            }
            F64MIN => {
                let arg2 = self.value_stack.pop_f64();
                let arg1 = self.value_stack.pop_f64();
                let result = if arg1 < arg2 { arg1 } else { arg2 };
                self.value_stack.push(Value::F64(result));
            }
            F64MAX => {
                let arg2 = self.value_stack.pop_f64();
                let arg1 = self.value_stack.pop_f64();
                let result = if arg1 > arg2 { arg1 } else { arg2 };
                self.value_stack.push(Value::F64(result));
            }
            F64COPYSIGN => {
                let arg2 = self.value_stack.pop_f64();
                let arg1 = self.value_stack.pop_f64();
                let result = if arg1.is_sign_negative() == arg2.is_sign_negative() {
                    arg1
                } else {
                    arg2
                };
                self.value_stack.push(Value::F64(result));
            }

            I32WRAPI64 => {
                let arg = self.value_stack.pop_u64();
                let wrapped: u32 = (arg & 0xffff_ffff) as u32;
                self.value_stack.push(Value::from(wrapped));
            }
            I32TRUNCSF32 => {
                let arg = self.value_stack.pop_f32();
                if arg < i32::MIN as f32 || arg > i32::MAX as f32 {
                    panic!("Cannot truncate {} from F32 to I32", arg);
                }
                self.value_stack.push(Value::I32(arg as i32));
            }
            I32TRUNCUF32 => {
                let arg = self.value_stack.pop_f32();
                if arg < u32::MIN as f32 || arg > u32::MAX as f32 {
                    panic!("Cannot truncate {} from F32 to unsigned I32", arg);
                }
                self.value_stack.push(Value::from(arg as u32));
            }
            I32TRUNCSF64 => {
                let arg = self.value_stack.pop_f64();
                if arg < i32::MIN as f64 || arg > i32::MAX as f64 {
                    panic!("Cannot truncate {} from F64 to I32", arg);
                }
                self.value_stack.push(Value::I32(arg as i32));
            }
            I32TRUNCUF64 => {
                let arg = self.value_stack.pop_f64();
                if arg < u32::MIN as f64 || arg > u32::MAX as f64 {
                    panic!("Cannot truncate {} from F64 to unsigned I32", arg);
                }
                self.value_stack.push(Value::from(arg as u32));
            }
            I64EXTENDSI32 => {
                let arg = self.value_stack.pop_i32();
                self.value_stack.push(Value::I64(arg as i64));
            }
            I64EXTENDUI32 => {
                let arg = self.value_stack.pop_u32();
                self.value_stack.push(Value::from(arg as u64));
            }
            I64TRUNCSF32 => {
                let arg = self.value_stack.pop_f32();
                if arg < i64::MIN as f32 || arg > i64::MAX as f32 {
                    panic!("Cannot truncate {} from F32 to I64", arg);
                }
                self.value_stack.push(Value::I64(arg as i64));
            }
            I64TRUNCUF32 => {
                let arg = self.value_stack.pop_f32();
                if arg < u64::MIN as f32 || arg > u64::MAX as f32 {
                    panic!("Cannot truncate {} from F32 to unsigned I64", arg);
                }
                self.value_stack.push(Value::from(arg as u64));
            }
            I64TRUNCSF64 => {
                let arg = self.value_stack.pop_f64();
                if arg < i64::MIN as f64 || arg > i64::MAX as f64 {
                    panic!("Cannot truncate {} from F64 to I64", arg);
                }
                self.value_stack.push(Value::I64(arg as i64));
            }
            I64TRUNCUF64 => {
                let arg = self.value_stack.pop_f64();
                if arg < u64::MIN as f64 || arg > u64::MAX as f64 {
                    panic!("Cannot truncate {} from F64 to unsigned I64", arg);
                }
                self.value_stack.push(Value::from(arg as u64));
            }
            F32CONVERTSI32 => {
                let arg = self.value_stack.pop_i32();
                self.value_stack.push(Value::F32(arg as f32));
            }
            F32CONVERTUI32 => {
                let arg = self.value_stack.pop_u32();
                self.value_stack.push(Value::F32(arg as f32));
            }
            F32CONVERTSI64 => {
                let arg = self.value_stack.pop_i64();
                self.value_stack.push(Value::F32(arg as f32));
            }
            F32CONVERTUI64 => {
                let arg = self.value_stack.pop_u64();
                self.value_stack.push(Value::F32(arg as f32));
            }
            F32DEMOTEF64 => {
                let arg = self.value_stack.pop_f64();
                self.value_stack.push(Value::F32(arg as f32));
            }
            F64CONVERTSI32 => {
                let arg = self.value_stack.pop_i32();
                self.value_stack.push(Value::F64(arg as f64));
            }
            F64CONVERTUI32 => {
                let arg = self.value_stack.pop_u32();
                self.value_stack.push(Value::F64(arg as f64));
            }
            F64CONVERTSI64 => {
                let arg = self.value_stack.pop_i64();
                self.value_stack.push(Value::F64(arg as f64));
            }
            F64CONVERTUI64 => {
                let arg = self.value_stack.pop_u64();
                self.value_stack.push(Value::F64(arg as f64));
            }
            F64PROMOTEF32 => {
                let arg = self.value_stack.pop_f32();
                self.value_stack.push(Value::F64(arg as f64));
            }

            I32REINTERPRETF32 => {
                let x = self.value_stack.pop_f32();
                self.value_stack
                    .push(Value::I32(i32::from_ne_bytes(x.to_ne_bytes())));
            }
            I64REINTERPRETF64 => {
                let x = self.value_stack.pop_f64();
                self.value_stack
                    .push(Value::I64(i64::from_ne_bytes(x.to_ne_bytes())));
            }
            F32REINTERPRETI32 => {
                let x = self.value_stack.pop_i32();
                self.value_stack
                    .push(Value::F32(f32::from_ne_bytes(x.to_ne_bytes())));
            }
            F64REINTERPRETI64 => {
                let x = self.value_stack.pop_i64();
                self.value_stack
                    .push(Value::F64(f64::from_ne_bytes(x.to_ne_bytes())));
            }
        }

        if let Some(debug_string) = &self.debug_string {
            let base = self.call_stack.value_stack_base();
            let slice = self.value_stack.get_slice(base as usize);
            eprintln!("{:#07x} {:17} {:?}", file_offset, debug_string, slice);
        }

        action
    }
}
