use bumpalo::{collections::Vec, Bump};
use std::fmt::{self, Write};
use std::iter::{self, once, Iterator};

use roc_wasm_module::opcodes::{MemoryInstruction, OpCode};
use roc_wasm_module::parse::{Parse, SkipBytes};
use roc_wasm_module::sections::{ImportDesc, MemorySection, SignatureParamsIter};
use roc_wasm_module::{ExportType, WasmModule};
use roc_wasm_module::{Value, ValueType};

use crate::frame::Frame;
use crate::value_store::ValueStore;
use crate::{Error, ImportDispatcher};

#[derive(Debug)]
pub enum Action {
    Continue,
    Break,
}

#[derive(Debug, Clone, Copy)]
enum BlockType {
    Loop(usize), // Loop block, with start address to loop back to
    Normal,      // Block created by `block` instruction
    #[allow(dead_code)]
    Locals(usize), // Special "block" for locals. Holds function index for debug
    #[allow(dead_code)]
    FunctionBody(usize), // Special block surrounding the function body. Holds function index for debug
}

#[derive(Debug, Clone, Copy)]
struct Block {
    ty: BlockType,
    vstack: usize,
}

#[derive(Debug, Clone)]
struct BranchCacheEntry {
    addr: u32,
    argument: u32,
    target: u32,
}

#[derive(Debug)]
pub struct Instance<'a, I: ImportDispatcher> {
    pub(crate) module: &'a WasmModule<'a>,
    /// Contents of the WebAssembly instance's memory
    pub memory: Vec<'a, u8>,
    /// The current call frame
    pub(crate) current_frame: Frame,
    /// Previous call frames
    previous_frames: Vec<'a, Frame>,
    /// The WebAssembly stack machine's stack of values
    pub(crate) value_store: ValueStore<'a>,
    /// Values of any global variables
    pub(crate) globals: Vec<'a, Value>,
    /// Index in the code section of the current instruction
    pub(crate) program_counter: usize,
    /// One entry per nested block. For loops, stores the address of the first instruction.
    blocks: Vec<'a, Block>,
    /// Cache for branching instructions, split into buckets for each function.
    branch_cache: Vec<'a, Vec<'a, BranchCacheEntry>>,
    /// Number of imports in the module
    import_count: usize,
    /// Import dispatcher from user code
    pub import_dispatcher: I,
    /// Temporary storage for import arguments
    import_arguments: Vec<'a, Value>,
    /// temporary storage for output using the --debug option
    debug_string: Option<String>,
}

impl<'a, I: ImportDispatcher> Instance<'a, I> {
    #[cfg(test)]
    pub(crate) fn new<G>(
        arena: &'a Bump,
        memory_pages: u32,
        program_counter: usize,
        globals: G,
        import_dispatcher: I,
    ) -> Self
    where
        G: IntoIterator<Item = Value>,
    {
        let mem_bytes = memory_pages * MemorySection::PAGE_SIZE;
        Instance {
            module: arena.alloc(WasmModule::new(arena)),
            memory: Vec::from_iter_in(iter::repeat(0).take(mem_bytes as usize), arena),
            current_frame: Frame::new(),
            previous_frames: Vec::new_in(arena),
            value_store: ValueStore::new(arena),
            globals: Vec::from_iter_in(globals, arena),
            program_counter,
            blocks: Vec::new_in(arena),
            branch_cache: bumpalo::vec![in arena; bumpalo::vec![in arena]],
            import_count: 0,
            import_dispatcher,
            import_arguments: Vec::new_in(arena),
            debug_string: Some(String::new()),
        }
    }

    pub fn from_bytes(
        arena: &'a Bump,
        module_bytes: &[u8],
        import_dispatcher: I,
        is_debug_mode: bool,
    ) -> Result<Self, std::string::String> {
        let module =
            WasmModule::preload(arena, module_bytes, false).map_err(|e| format!("{e:?}"))?;
        Self::for_module(arena, arena.alloc(module), import_dispatcher, is_debug_mode)
    }

    pub fn for_module(
        arena: &'a Bump,
        module: &'a WasmModule<'a>,
        import_dispatcher: I,
        is_debug_mode: bool,
    ) -> Result<Self, std::string::String> {
        let mem_bytes = module.memory.min_bytes().map_err(|e| {
            format!(
                "Error parsing Memory section at offset {:#x}:\n{}",
                e.offset, e.message
            )
        })?;
        let mut memory = Vec::from_iter_in(iter::repeat(0).take(mem_bytes as usize), arena);
        module.data.load_into(&mut memory)?;

        let globals = module.global.initial_values(arena);

        // We don't handle non-function import types (memories, tables, and globals),
        // and it's nice for lookups to assume they're all functions, so let's assert that.
        let all_imports_are_functions = module.import.imports.iter().all(|imp| imp.is_function());
        assert!(
            all_imports_are_functions,
            "This Wasm interpreter doesn't support non-function imports"
        );

        let value_store = ValueStore::new(arena);

        let debug_string = if is_debug_mode {
            Some(String::new())
        } else {
            None
        };

        let import_count = module.import.imports.len();
        let branch_cache = {
            let num_functions = import_count + module.code.function_count as usize;
            let empty_caches_iter = iter::repeat(Vec::new_in(arena)).take(num_functions);
            Vec::from_iter_in(empty_caches_iter, arena)
        };

        Ok(Instance {
            module,
            memory,
            current_frame: Frame::new(),
            previous_frames: Vec::new_in(arena),
            value_store,
            globals,
            program_counter: usize::MAX,
            blocks: Vec::new_in(arena),
            branch_cache,
            import_count,
            import_dispatcher,
            import_arguments: Vec::new_in(arena),
            debug_string,
        })
    }

    pub fn call_export<A>(&mut self, fn_name: &str, arg_values: A) -> Result<Option<Value>, String>
    where
        A: IntoIterator<Item = Value>,
    {
        let (fn_index, param_type_iter, ret_type) =
            self.call_export_help_before_arg_load(self.module, fn_name)?;
        let n_args = param_type_iter.len();

        for (i, (value, expected_type)) in arg_values.into_iter().zip(param_type_iter).enumerate() {
            let actual_type = ValueType::from(value);
            if actual_type != expected_type {
                return Err(format!(
                    "Type mismatch on argument {i} of {fn_name}. Expected {expected_type:?} but got {value:?}"
                ));
            }
            self.value_store.push(value);
        }

        self.call_export_help_after_arg_load(self.module, fn_index, n_args, ret_type)
    }

    pub fn call_export_from_cli(
        &mut self,
        module: &WasmModule<'a>,
        fn_name: &str,
        arg_strings: &'a [&'a [u8]],
    ) -> Result<Option<Value>, String> {
        // We have two different mechanisms for handling CLI arguments!
        // 1. Basic numbers:
        //      e.g. `roc_wasm_interp fibonacci 12`
        //      Below, we check if the called Wasm function takes numeric arguments and, if so, parse them from the CLI.
        //      This is good for low-level test cases, for example while developing this interpreter.
        // 2. WASI:
        //      POSIX-style array of strings. Much more high-level and complex than the "basic" version.
        //      The WASI `_start` function itself takes no arguments (its Wasm type signature is `() -> nil`).
        //      The program uses WASI syscalls to copy strings into Wasm memory and process them.
        //      But that happens *elsewhere*! Here, `arg_strings` is ignored because `_start` takes no arguments.

        // Implement the "basic numbers" CLI
        // Check if the called Wasm function takes numeric arguments, and if so, try to parse them from the CLI.
        let (fn_index, param_type_iter, ret_type) =
            self.call_export_help_before_arg_load(module, fn_name)?;
        let n_args = param_type_iter.len();
        for (value_bytes, value_type) in arg_strings
            .iter()
            .skip(1) // first string is the .wasm filename
            .zip(param_type_iter)
        {
            use ValueType::*;
            let value_str = String::from_utf8_lossy(value_bytes);
            let value = match value_type {
                I32 => Value::I32(value_str.parse::<i32>().map_err(|e| e.to_string())?),
                I64 => Value::I64(value_str.parse::<i64>().map_err(|e| e.to_string())?),
                F32 => Value::F32(value_str.parse::<f32>().map_err(|e| e.to_string())?),
                F64 => Value::F64(value_str.parse::<f64>().map_err(|e| e.to_string())?),
            };
            self.value_store.push(value);
        }

        self.call_export_help_after_arg_load(module, fn_index, n_args, ret_type)
    }

    fn call_export_help_before_arg_load<'m>(
        &mut self,
        module: &'m WasmModule<'a>,
        fn_name: &str,
    ) -> Result<(usize, SignatureParamsIter<'m>, Option<ValueType>), String> {
        let fn_index = {
            let mut export_iter = module.export.exports.iter();
            export_iter
                // First look up the name in exports
                .find_map(|ex| {
                    if ex.ty == ExportType::Func && ex.name == fn_name {
                        Some(ex.index)
                    } else {
                        None
                    }
                })
                .or_else(|| {
                    // Then look it up in the debug info!
                    // This is non-spec behaviour that Wasm3 seems to implement,
                    // and that our wasm_linking tests accidentally rely on!
                    let mut names = module.names.function_names.iter();
                    names.find_map(
                        |(index, name)| {
                            if *name == fn_name {
                                Some(*index)
                            } else {
                                None
                            }
                        },
                    )
                })
                .ok_or_else(|| {
                    format!("I couldn't find a function '{fn_name}' in this WebAssembly module")
                })? as usize
        };

        let internal_fn_index = fn_index - self.import_count;

        self.program_counter = {
            let mut cursor = module.code.function_offsets[internal_fn_index] as usize;
            let _start_fn_byte_length = u32::parse((), &module.code.bytes, &mut cursor);
            cursor
        };

        let (param_type_iter, return_type) = {
            let signature_index = module.function.signatures[internal_fn_index];
            module.types.look_up(signature_index)
        };

        if self.debug_string.is_some() {
            println!(
                "Calling export func[{}] '{}' at address {:#x}",
                fn_index,
                fn_name,
                self.program_counter + module.code.section_offset as usize
            );
        }

        Ok((fn_index, param_type_iter, return_type))
    }

    fn call_export_help_after_arg_load(
        &mut self,
        module: &WasmModule<'a>,
        fn_index: usize,
        n_args: usize,
        return_type: Option<ValueType>,
    ) -> Result<Option<Value>, String> {
        self.previous_frames.clear();
        self.blocks.clear();
        self.blocks.push(Block {
            ty: BlockType::Locals(fn_index),
            vstack: self.value_store.depth(),
        });
        self.current_frame = Frame::enter(
            fn_index,
            0, // return_addr
            self.blocks.len(),
            n_args,
            return_type,
            &module.code.bytes,
            &mut self.value_store,
            &mut self.program_counter,
        );
        self.blocks.push(Block {
            ty: BlockType::FunctionBody(fn_index),
            vstack: self.value_store.depth(),
        });

        loop {
            match self.execute_next_instruction(module) {
                Ok(Action::Continue) => {}
                Ok(Action::Break) => {
                    break;
                }
                Err(e) => {
                    let file_offset = self.program_counter + module.code.section_offset as usize;
                    let mut message = e.to_string_at(file_offset);
                    self.debug_stack_trace(&mut message).unwrap();
                    return Err(message);
                }
            };
        }

        let return_value = if !self.value_store.is_empty() {
            Some(self.value_store.pop())
        } else {
            None
        };

        Ok(return_value)
    }

    fn fetch_immediate_u32(&mut self, module: &WasmModule<'a>) -> u32 {
        let x = u32::parse((), &module.code.bytes, &mut self.program_counter).unwrap();
        if let Some(debug_string) = self.debug_string.as_mut() {
            write!(debug_string, "{x} ").unwrap();
        }
        x
    }

    fn do_return(&mut self) -> Action {
        // self.debug_values_and_blocks("start do_return");

        let Frame {
            return_addr,
            body_block_index,
            return_type,
            ..
        } = self.current_frame;

        // Throw away all locals and values except the return value
        let locals_block_index = body_block_index - 1;
        let locals_block = &self.blocks[locals_block_index];
        let new_stack_depth = if return_type.is_some() {
            self.value_store
                .set(locals_block.vstack, self.value_store.peek());
            locals_block.vstack + 1
        } else {
            locals_block.vstack
        };
        self.value_store.truncate(new_stack_depth);

        // Resume executing at the next instruction in the caller function
        let new_block_len = locals_block_index; // don't need a -1 because one is a length and the other is an index!
        self.blocks.truncate(new_block_len);
        self.program_counter = return_addr;

        // self.debug_values_and_blocks("end do_return");

        if let Some(caller_frame) = self.previous_frames.pop() {
            self.current_frame = caller_frame;
            Action::Continue
        } else {
            // We just popped the stack frame for the entry function. Terminate the program.
            Action::Break
        }
    }

    fn get_load_address(&mut self, module: &WasmModule<'a>) -> Result<u32, Error> {
        // Alignment is not used in the execution steps from the spec! Maybe it's just an optimization hint?
        // https://webassembly.github.io/spec/core/exec/instructions.html#memory-instructions
        // Also note: in the text format we can specify the useless `align=` but not the useful `offset=`!
        let _alignment = self.fetch_immediate_u32(module);
        let offset = self.fetch_immediate_u32(module);
        let base_addr = self.value_store.pop_u32()?;
        let addr = base_addr + offset;
        let memory_size = self.memory.len() as u32;
        if addr >= memory_size {
            Err(Error::MemoryAccessOutOfBounds(addr, memory_size))
        } else {
            Ok(addr)
        }
    }

    fn get_store_addr_value(&mut self, module: &WasmModule<'a>) -> Result<(usize, Value), Error> {
        // Alignment is not used in the execution steps from the spec! Maybe it's just an optimization hint?
        // https://webassembly.github.io/spec/core/exec/instructions.html#memory-instructions
        // Also note: in the text format we can specify the useless `align=` but not the useful `offset=`!
        let _alignment = self.fetch_immediate_u32(module);
        let offset = self.fetch_immediate_u32(module);
        let value = self.value_store.pop();
        let base_addr = self.value_store.pop_u32()?;
        let addr = base_addr + offset;
        let memory_size = self.memory.len() as u32;
        if addr >= memory_size {
            Err(Error::MemoryAccessOutOfBounds(addr, memory_size))
        } else {
            Ok((addr as usize, value))
        }
    }

    fn write_debug<T: fmt::Debug>(&mut self, value: T) {
        if let Some(debug_string) = self.debug_string.as_mut() {
            std::write!(debug_string, "{value:?} ").unwrap();
        }
    }

    fn do_break(&mut self, relative_blocks_outward: u32, module: &WasmModule<'a>) {
        let block_index = self.blocks.len() - 1 - relative_blocks_outward as usize;
        let Block { ty, vstack } = self.blocks[block_index];
        match ty {
            BlockType::Loop(start_addr) => {
                self.blocks.truncate(block_index + 1);
                self.value_store.truncate(vstack);
                self.program_counter = start_addr;
            }
            BlockType::FunctionBody(_) | BlockType::Normal => {
                self.break_forward(relative_blocks_outward, module);
                self.value_store.truncate(vstack);
            }
            BlockType::Locals(_) => unreachable!(),
        }
    }

    // Break to an outer block, going forward in the program
    fn break_forward(&mut self, relative_blocks_outward: u32, module: &WasmModule<'a>) {
        use OpCode::*;

        let addr = self.program_counter as u32;
        let cache_result = self.branch_cache[self.current_frame.fn_index]
            .iter()
            .find(|entry| entry.addr == addr && entry.argument == relative_blocks_outward);

        let mut depth = self.blocks.len();
        let target_block_depth = depth - (relative_blocks_outward + 1) as usize;

        if let Some(entry) = cache_result {
            self.program_counter = entry.target as usize;
        } else {
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
            self.branch_cache[self.current_frame.fn_index].push(BranchCacheEntry {
                addr,
                argument: relative_blocks_outward,
                target: self.program_counter as u32,
            });
        }
        self.blocks.truncate(target_block_depth);
    }

    fn do_call(
        &mut self,
        expected_signature: Option<u32>,
        fn_index: usize,
        module: &WasmModule<'a>,
    ) -> Result<(), Error> {
        // self.debug_values_and_blocks(&format!("start do_call {}", fn_index));

        let (signature_index, opt_import) = if fn_index < self.import_count {
            // Imported non-Wasm function
            let import = &module.import.imports[fn_index];
            let sig = match import.description {
                ImportDesc::Func { signature_index } => signature_index,
                _ => unreachable!(),
            };
            (sig, Some(import))
        } else {
            // Wasm function
            let sig = module.function.signatures[fn_index - self.import_count];
            (sig, None)
        };

        if let Some(expected) = expected_signature {
            assert_eq!(
                expected, signature_index,
                "Indirect function call failed. Expected signature {expected} but found {signature_index}",
            );
        }

        let (arg_type_iter, ret_type) = module.types.look_up(signature_index);
        let n_args = arg_type_iter.len();
        if self.debug_string.is_some() {
            self.debug_call(n_args, ret_type);
        }

        if let Some(import) = opt_import {
            self.import_arguments.clear();
            self.import_arguments
                .extend(std::iter::repeat(Value::I64(0)).take(n_args));
            for (i, expected) in arg_type_iter.enumerate().rev() {
                let arg = self.value_store.pop();
                let actual = ValueType::from(arg);
                if actual != expected {
                    return Err(Error::Type(expected, actual));
                }
                self.import_arguments[i] = arg;
            }

            let optional_return_val = self.import_dispatcher.dispatch(
                import.module,
                import.name,
                &self.import_arguments,
                &mut self.memory,
            );
            if let Some(return_val) = optional_return_val {
                self.value_store.push(return_val);
            }
            if let Some(debug_string) = self.debug_string.as_mut() {
                write!(debug_string, " {}.{}", import.module, import.name).unwrap();
            }
        } else {
            let return_addr = self.program_counter;
            // set PC to start of function bytes
            let internal_fn_index = fn_index - self.import_count;
            self.program_counter = module.code.function_offsets[internal_fn_index] as usize;
            // advance PC to the start of the local variable declarations
            u32::parse((), &module.code.bytes, &mut self.program_counter).unwrap();

            self.blocks.push(Block {
                ty: BlockType::Locals(fn_index),
                vstack: self.value_store.depth() - n_args,
            });
            let body_block_index = self.blocks.len();

            let mut swap_frame = Frame::enter(
                fn_index,
                return_addr,
                body_block_index,
                n_args,
                ret_type,
                &module.code.bytes,
                &mut self.value_store,
                &mut self.program_counter,
            );
            std::mem::swap(&mut swap_frame, &mut self.current_frame);
            self.previous_frames.push(swap_frame);

            self.blocks.push(Block {
                ty: BlockType::FunctionBody(fn_index),
                vstack: self.value_store.depth(),
            });
        }
        // self.debug_values_and_blocks("end do_call");

        Ok(())
    }

    fn debug_call(&mut self, n_args: usize, return_type: Option<ValueType>) {
        if let Some(debug_string) = self.debug_string.as_mut() {
            write!(debug_string, "         args=[").unwrap();
            let arg_iter = self
                .value_store
                .iter()
                .skip(self.value_store.depth() - n_args);
            let mut first = true;
            for arg in arg_iter {
                if first {
                    first = false;
                } else {
                    write!(debug_string, ", ").unwrap();
                }
                write!(debug_string, "{arg:x?}").unwrap();
            }
            writeln!(debug_string, "] return_type={return_type:?}").unwrap();
        }
    }

    pub(crate) fn execute_next_instruction(
        &mut self,
        module: &WasmModule<'a>,
    ) -> Result<Action, Error> {
        use OpCode::*;

        let file_offset = self.program_counter as u32 + module.code.section_offset;
        let op_code = OpCode::from(module.code.bytes[self.program_counter]);
        self.program_counter += 1;

        if let Some(debug_string) = self.debug_string.as_mut() {
            debug_string.clear();
            self.write_debug(op_code);
        }

        let mut action = Action::Continue;
        let mut implicit_return = false;

        match op_code {
            UNREACHABLE => {
                return Err(Error::UnreachableOp);
            }
            NOP => {}
            BLOCK => {
                self.fetch_immediate_u32(module); // blocktype (ignored)
                self.blocks.push(Block {
                    ty: BlockType::Normal,
                    vstack: self.value_store.depth(),
                });
            }
            LOOP => {
                self.fetch_immediate_u32(module); // blocktype (ignored)
                self.blocks.push(Block {
                    ty: BlockType::Loop(self.program_counter),
                    vstack: self.value_store.depth(),
                });
            }
            IF => {
                self.fetch_immediate_u32(module); // blocktype (ignored)
                let condition = self.value_store.pop_i32()?;
                self.blocks.push(Block {
                    ty: BlockType::Normal,
                    vstack: self.value_store.depth(),
                });
                if condition == 0 {
                    let addr = self.program_counter as u32;
                    let cache_result = self.branch_cache[self.current_frame.fn_index]
                        .iter()
                        .find(|entry| entry.addr == addr);
                    if let Some(entry) = cache_result {
                        self.program_counter = entry.target as usize;
                    } else {
                        let target_depth = self.blocks.len();
                        let mut depth = target_depth;
                        loop {
                            let skipped_op = OpCode::from(module.code.bytes[self.program_counter]);
                            OpCode::skip_bytes(&module.code.bytes, &mut self.program_counter)
                                .unwrap();
                            match skipped_op {
                                BLOCK | LOOP | IF => {
                                    depth += 1;
                                }
                                END => {
                                    if depth == target_depth {
                                        // `if` without `else`
                                        self.blocks.pop();
                                        break;
                                    } else {
                                        depth -= 1;
                                    }
                                }
                                ELSE => {
                                    if depth == target_depth {
                                        break;
                                    }
                                }
                                _ => {}
                            }
                        }
                        self.branch_cache[self.current_frame.fn_index].push(BranchCacheEntry {
                            addr,
                            argument: 0,
                            target: self.program_counter as u32,
                        });
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
                if self.blocks.len() == (self.current_frame.body_block_index + 1) {
                    // implicit RETURN at end of function
                    action = self.do_return();
                    implicit_return = true;
                } else {
                    self.blocks.pop().unwrap();
                }
            }
            BR => {
                let relative_blocks_outward = self.fetch_immediate_u32(module);
                self.do_break(relative_blocks_outward, module);
            }
            BRIF => {
                let relative_blocks_outward = self.fetch_immediate_u32(module);
                let condition = self.value_store.pop_i32()?;
                if condition != 0 {
                    self.do_break(relative_blocks_outward, module);
                }
            }
            BRTABLE => {
                let selector = self.value_store.pop_u32()?;
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
                self.do_call(None, fn_index, module)?;
            }
            CALLINDIRECT => {
                let expected_signature = self.fetch_immediate_u32(module);
                let table_index = self.fetch_immediate_u32(module);
                let element_index = self.value_store.pop_u32()?;

                // So far, all compilers seem to be emitting MVP-compatible code. (Rust, Zig, Roc...)
                assert_eq!(
                    table_index, 0,
                    "Table index {table_index} not supported at file offset {file_offset:#x}. This interpreter only supports Wasm MVP."
                );

                // Dereference the function pointer (look up the element index in the function table)
                let fn_index = module.element.lookup(element_index).unwrap_or_else(|| {
                    panic!(
                        "Indirect function call failed. There is no function with element index {element_index}"
                    )
                });

                self.do_call(Some(expected_signature), fn_index as usize, module)?;
            }
            DROP => {
                self.value_store.pop();
            }
            SELECT => {
                let c = self.value_store.pop_i32()?;
                let val2 = self.value_store.pop();
                let val1 = self.value_store.pop();
                let actual = ValueType::from(val2);
                let expected = ValueType::from(val1);
                if actual != expected {
                    return Err(Error::Type(expected, actual));
                }
                let result = if c != 0 { val1 } else { val2 };
                self.value_store.push(result);
            }
            GETLOCAL => {
                let index = self.fetch_immediate_u32(module);
                let value = self.current_frame.get_local(&self.value_store, index);
                self.value_store.push(value);
            }
            SETLOCAL => {
                let index = self.fetch_immediate_u32(module);
                let value = self.value_store.pop();
                self.current_frame
                    .set_local(&mut self.value_store, index, value);
            }
            TEELOCAL => {
                let index = self.fetch_immediate_u32(module);
                let value = self.value_store.peek();
                self.current_frame
                    .set_local(&mut self.value_store, index, value);
            }
            GETGLOBAL => {
                let index = self.fetch_immediate_u32(module);
                self.value_store.push(self.globals[index as usize]);
            }
            SETGLOBAL => {
                let index = self.fetch_immediate_u32(module);
                self.globals[index as usize] = self.value_store.pop();
            }
            I32LOAD => {
                let addr = self.get_load_address(module)? as usize;
                let mut bytes = [0; 4];
                bytes.copy_from_slice(&self.memory[addr..][..4]);
                let value = i32::from_le_bytes(bytes);
                self.value_store.push(Value::I32(value));
            }
            I64LOAD => {
                let addr = self.get_load_address(module)? as usize;
                let mut bytes = [0; 8];
                bytes.copy_from_slice(&self.memory[addr..][..8]);
                let value = i64::from_le_bytes(bytes);
                self.value_store.push(Value::I64(value));
            }
            F32LOAD => {
                let addr = self.get_load_address(module)? as usize;
                let mut bytes = [0; 4];
                bytes.copy_from_slice(&self.memory[addr..][..4]);
                let value = f32::from_le_bytes(bytes);
                self.value_store.push(Value::F32(value));
            }
            F64LOAD => {
                let addr = self.get_load_address(module)? as usize;
                let mut bytes = [0; 8];
                bytes.copy_from_slice(&self.memory[addr..][..8]);
                let value = f64::from_le_bytes(bytes);
                self.value_store.push(Value::F64(value));
            }
            I32LOAD8S => {
                let addr = self.get_load_address(module)? as usize;
                let mut bytes = [0; 1];
                bytes.copy_from_slice(&self.memory[addr..][..1]);
                let value = i8::from_le_bytes(bytes);
                self.value_store.push(Value::I32(value as i32));
            }
            I32LOAD8U => {
                let addr = self.get_load_address(module)? as usize;
                let value = self.memory[addr];
                self.value_store.push(Value::I32(value as i32));
            }
            I32LOAD16S => {
                let addr = self.get_load_address(module)? as usize;
                let mut bytes = [0; 2];
                bytes.copy_from_slice(&self.memory[addr..][..2]);
                let value = i16::from_le_bytes(bytes);
                self.value_store.push(Value::I32(value as i32));
            }
            I32LOAD16U => {
                let addr = self.get_load_address(module)? as usize;
                let mut bytes = [0; 2];
                bytes.copy_from_slice(&self.memory[addr..][..2]);
                let value = u16::from_le_bytes(bytes);
                self.value_store.push(Value::I32(value as i32));
            }
            I64LOAD8S => {
                let addr = self.get_load_address(module)? as usize;
                let mut bytes = [0; 1];
                bytes.copy_from_slice(&self.memory[addr..][..1]);
                let value = i8::from_le_bytes(bytes);
                self.value_store.push(Value::I64(value as i64));
            }
            I64LOAD8U => {
                let addr = self.get_load_address(module)? as usize;
                let value = self.memory[addr];
                self.value_store.push(Value::I64(value as i64));
            }
            I64LOAD16S => {
                let addr = self.get_load_address(module)? as usize;
                let mut bytes = [0; 2];
                bytes.copy_from_slice(&self.memory[addr..][..2]);
                let value = i16::from_le_bytes(bytes);
                self.value_store.push(Value::I64(value as i64));
            }
            I64LOAD16U => {
                let addr = self.get_load_address(module)? as usize;
                let mut bytes = [0; 2];
                bytes.copy_from_slice(&self.memory[addr..][..2]);
                let value = u16::from_le_bytes(bytes);
                self.value_store.push(Value::I64(value as i64));
            }
            I64LOAD32S => {
                let addr = self.get_load_address(module)? as usize;
                let mut bytes = [0; 4];
                bytes.copy_from_slice(&self.memory[addr..][..4]);
                let value = i32::from_le_bytes(bytes);
                self.value_store.push(Value::I64(value as i64));
            }
            I64LOAD32U => {
                let addr = self.get_load_address(module)? as usize;
                let mut bytes = [0; 4];
                bytes.copy_from_slice(&self.memory[addr..][..4]);
                let value = u32::from_le_bytes(bytes);
                self.value_store.push(Value::I64(value as i64));
            }
            I32STORE => {
                let (addr, value) = self.get_store_addr_value(module)?;
                let unwrapped = value.expect_i32().map_err(Error::from)?;
                let target = &mut self.memory[addr..][..4];
                target.copy_from_slice(&unwrapped.to_le_bytes());
            }
            I64STORE => {
                let (addr, value) = self.get_store_addr_value(module)?;
                let unwrapped = value.expect_i64().map_err(Error::from)?;
                let target = &mut self.memory[addr..][..8];
                target.copy_from_slice(&unwrapped.to_le_bytes());
            }
            F32STORE => {
                let (addr, value) = self.get_store_addr_value(module)?;
                let unwrapped = value.expect_f32().map_err(Error::from)?;
                let target = &mut self.memory[addr..][..4];
                target.copy_from_slice(&unwrapped.to_le_bytes());
            }
            F64STORE => {
                let (addr, value) = self.get_store_addr_value(module)?;
                let unwrapped = value.expect_f64().map_err(Error::from)?;
                let target = &mut self.memory[addr..][..8];
                target.copy_from_slice(&unwrapped.to_le_bytes());
            }
            I32STORE8 => {
                let (addr, value) = self.get_store_addr_value(module)?;
                let unwrapped = value.expect_i32().map_err(Error::from)?;
                let target = &mut self.memory[addr..][..1];
                target.copy_from_slice(&unwrapped.to_le_bytes()[..1]);
            }
            I32STORE16 => {
                let (addr, value) = self.get_store_addr_value(module)?;
                let unwrapped = value.expect_i32().map_err(Error::from)?;
                let target = &mut self.memory[addr..][..2];
                target.copy_from_slice(&unwrapped.to_le_bytes()[..2]);
            }
            I64STORE8 => {
                let (addr, value) = self.get_store_addr_value(module)?;
                let unwrapped = value.expect_i64().map_err(Error::from)?;
                let target = &mut self.memory[addr..][..1];
                target.copy_from_slice(&unwrapped.to_le_bytes()[..1]);
            }
            I64STORE16 => {
                let (addr, value) = self.get_store_addr_value(module)?;
                let unwrapped = value.expect_i64().map_err(Error::from)?;
                let target = &mut self.memory[addr..][..2];
                target.copy_from_slice(&unwrapped.to_le_bytes()[..2]);
            }
            I64STORE32 => {
                let (addr, value) = self.get_store_addr_value(module)?;
                let unwrapped = value.expect_i64().map_err(Error::from)?;
                let target = &mut self.memory[addr..][..4];
                target.copy_from_slice(&unwrapped.to_le_bytes()[..4]);
            }
            CURRENTMEMORY => {
                let memory_index = self.fetch_immediate_u32(module);
                assert_eq!(memory_index, 0);
                let size = self.memory.len() as i32 / MemorySection::PAGE_SIZE as i32;
                self.value_store.push(Value::I32(size));
            }
            GROWMEMORY => {
                let memory_index = self.fetch_immediate_u32(module);
                assert_eq!(memory_index, 0);
                let old_bytes = self.memory.len() as u32;
                let old_pages = old_bytes / MemorySection::PAGE_SIZE;
                let grow_pages = self.value_store.pop_u32()?;
                let grow_bytes = grow_pages * MemorySection::PAGE_SIZE;
                let new_bytes = old_bytes + grow_bytes;

                let success = match module.memory.max_bytes().unwrap() {
                    Some(max_bytes) => new_bytes <= max_bytes,
                    None => true,
                };
                if success {
                    self.memory
                        .extend(iter::repeat(0).take(grow_bytes as usize));
                    self.value_store.push(Value::I32(old_pages as i32));
                } else {
                    self.value_store.push(Value::I32(-1));
                }
            }
            MEMORY => {
                // the first argument determines exactly which memory operation we have
                match MemoryInstruction::try_from(module.code.bytes[self.program_counter]) {
                    Ok(op) => match op {
                        MemoryInstruction::MemoryInit => todo!("WASM instruction: memory.init"),
                        MemoryInstruction::DataDrop => todo!("WASM instruction: data.drop"),
                        MemoryInstruction::MemoryCopy => {
                            let size = self.value_store.pop_u32()? as usize;
                            let source = self.value_store.pop_u32()? as usize;
                            let destination = self.value_store.pop_u32()? as usize;

                            // skip the op byte and an extra two zero bytes.
                            // in future versions of WebAssembly this byte may be used to index additional memories
                            self.program_counter += 1 + 2;

                            self.memory.copy_within(source..source + size, destination)
                        }
                        MemoryInstruction::MemoryFill => {
                            let size = self.value_store.pop_u32()? as usize;
                            let byte_value = self.value_store.pop_u32()? as u8;
                            let destination = self.value_store.pop_u32()? as usize;

                            // skip the op byte and an extra zero byte.
                            // in future versions of WebAssembly this byte may be used to index additional memories
                            self.program_counter += 1 + 1;

                            self.memory[destination..][..size].fill(byte_value);
                        }
                    },
                    Err(other) => unreachable!("invalid memory instruction {other:?}"),
                };
            }
            I32CONST => {
                let value = i32::parse((), &module.code.bytes, &mut self.program_counter).unwrap();
                self.write_debug(value);
                self.value_store.push(Value::I32(value));
            }
            I64CONST => {
                let value = i64::parse((), &module.code.bytes, &mut self.program_counter).unwrap();
                self.write_debug(value);
                self.value_store.push(Value::I64(value));
            }
            F32CONST => {
                let mut bytes = [0; 4];
                bytes.copy_from_slice(&module.code.bytes[self.program_counter..][..4]);
                let value = f32::from_le_bytes(bytes);
                self.write_debug(value);
                self.value_store.push(Value::F32(value));
                self.program_counter += 4;
            }
            F64CONST => {
                let mut bytes = [0; 8];
                bytes.copy_from_slice(&module.code.bytes[self.program_counter..][..8]);
                let value = f64::from_le_bytes(bytes);
                self.write_debug(value);
                self.value_store.push(Value::F64(value));
                self.program_counter += 8;
            }

            I32EQZ => {
                let arg = self.value_store.pop_i32()?;
                let result: bool = arg == 0;
                self.value_store.push(Value::I32(result as i32));
            }
            I32EQ => {
                let arg2 = self.value_store.pop_i32()?;
                let arg1 = self.value_store.pop_i32()?;
                let result: bool = arg1 == arg2;
                self.value_store.push(Value::I32(result as i32));
            }
            I32NE => {
                let arg2 = self.value_store.pop_i32()?;
                let arg1 = self.value_store.pop_i32()?;
                let result: bool = arg1 != arg2;
                self.value_store.push(Value::I32(result as i32));
            }
            I32LTS => {
                let arg2 = self.value_store.pop_i32()?;
                let arg1 = self.value_store.pop_i32()?;
                let result: bool = arg1 < arg2;
                self.value_store.push(Value::I32(result as i32));
            }
            I32LTU => {
                let arg2 = self.value_store.pop_u32()?;
                let arg1 = self.value_store.pop_u32()?;
                let result: bool = arg1 < arg2;
                self.value_store.push(Value::I32(result as i32));
            }
            I32GTS => {
                let arg2 = self.value_store.pop_i32()?;
                let arg1 = self.value_store.pop_i32()?;
                let result: bool = arg1 > arg2;
                self.value_store.push(Value::I32(result as i32));
            }
            I32GTU => {
                let arg2 = self.value_store.pop_u32()?;
                let arg1 = self.value_store.pop_u32()?;
                let result: bool = arg1 > arg2;
                self.value_store.push(Value::I32(result as i32));
            }
            I32LES => {
                let arg2 = self.value_store.pop_i32()?;
                let arg1 = self.value_store.pop_i32()?;
                let result: bool = arg1 <= arg2;
                self.value_store.push(Value::I32(result as i32));
            }
            I32LEU => {
                let arg2 = self.value_store.pop_u32()?;
                let arg1 = self.value_store.pop_u32()?;
                let result: bool = arg1 <= arg2;
                self.value_store.push(Value::I32(result as i32));
            }
            I32GES => {
                let arg2 = self.value_store.pop_i32()?;
                let arg1 = self.value_store.pop_i32()?;
                let result: bool = arg1 >= arg2;
                self.value_store.push(Value::I32(result as i32));
            }
            I32GEU => {
                let arg2 = self.value_store.pop_u32()?;
                let arg1 = self.value_store.pop_u32()?;
                let result: bool = arg1 >= arg2;
                self.value_store.push(Value::I32(result as i32));
            }

            I64EQZ => {
                let arg = self.value_store.pop_i64()?;
                let result: bool = arg == 0;
                self.value_store.push(Value::I32(result as i32));
            }
            I64EQ => {
                let arg2 = self.value_store.pop_i64()?;
                let arg1 = self.value_store.pop_i64()?;
                let result: bool = arg1 == arg2;
                self.value_store.push(Value::I32(result as i32));
            }
            I64NE => {
                let arg2 = self.value_store.pop_i64()?;
                let arg1 = self.value_store.pop_i64()?;
                let result: bool = arg1 != arg2;
                self.value_store.push(Value::I32(result as i32));
            }
            I64LTS => {
                let arg2 = self.value_store.pop_i64()?;
                let arg1 = self.value_store.pop_i64()?;
                let result: bool = arg1 < arg2;
                self.value_store.push(Value::I32(result as i32));
            }
            I64LTU => {
                let arg2 = self.value_store.pop_u64()?;
                let arg1 = self.value_store.pop_u64()?;
                let result: bool = arg1 < arg2;
                self.value_store.push(Value::I32(result as i32));
            }
            I64GTS => {
                let arg2 = self.value_store.pop_i64()?;
                let arg1 = self.value_store.pop_i64()?;
                let result: bool = arg1 > arg2;
                self.value_store.push(Value::I32(result as i32));
            }
            I64GTU => {
                let arg2 = self.value_store.pop_u64()?;
                let arg1 = self.value_store.pop_u64()?;
                let result: bool = arg1 > arg2;
                self.value_store.push(Value::I32(result as i32));
            }
            I64LES => {
                let arg2 = self.value_store.pop_i64()?;
                let arg1 = self.value_store.pop_i64()?;
                let result: bool = arg1 <= arg2;
                self.value_store.push(Value::I32(result as i32));
            }
            I64LEU => {
                let arg2 = self.value_store.pop_u64()?;
                let arg1 = self.value_store.pop_u64()?;
                let result: bool = arg1 <= arg2;
                self.value_store.push(Value::I32(result as i32));
            }
            I64GES => {
                let arg2 = self.value_store.pop_i64()?;
                let arg1 = self.value_store.pop_i64()?;
                let result: bool = arg1 >= arg2;
                self.value_store.push(Value::I32(result as i32));
            }
            I64GEU => {
                let arg2 = self.value_store.pop_u64()?;
                let arg1 = self.value_store.pop_u64()?;
                let result: bool = arg1 >= arg2;
                self.value_store.push(Value::I32(result as i32));
            }

            F32EQ => {
                let arg2 = self.value_store.pop_f32()?;
                let arg1 = self.value_store.pop_f32()?;
                let result: bool = arg1 == arg2;
                self.value_store.push(Value::I32(result as i32));
            }
            F32NE => {
                let arg2 = self.value_store.pop_f32()?;
                let arg1 = self.value_store.pop_f32()?;
                let result: bool = arg1 != arg2;
                self.value_store.push(Value::I32(result as i32));
            }
            F32LT => {
                let arg2 = self.value_store.pop_f32()?;
                let arg1 = self.value_store.pop_f32()?;
                let result: bool = arg1 < arg2;
                self.value_store.push(Value::I32(result as i32));
            }
            F32GT => {
                let arg2 = self.value_store.pop_f32()?;
                let arg1 = self.value_store.pop_f32()?;
                let result: bool = arg1 > arg2;
                self.value_store.push(Value::I32(result as i32));
            }
            F32LE => {
                let arg2 = self.value_store.pop_f32()?;
                let arg1 = self.value_store.pop_f32()?;
                let result: bool = arg1 <= arg2;
                self.value_store.push(Value::I32(result as i32));
            }
            F32GE => {
                let arg2 = self.value_store.pop_f32()?;
                let arg1 = self.value_store.pop_f32()?;
                let result: bool = arg1 >= arg2;
                self.value_store.push(Value::I32(result as i32));
            }

            F64EQ => {
                let arg2 = self.value_store.pop_f64()?;
                let arg1 = self.value_store.pop_f64()?;
                let result: bool = arg1 == arg2;
                self.value_store.push(Value::I32(result as i32));
            }
            F64NE => {
                let arg2 = self.value_store.pop_f64()?;
                let arg1 = self.value_store.pop_f64()?;
                let result: bool = arg1 != arg2;
                self.value_store.push(Value::I32(result as i32));
            }
            F64LT => {
                let arg2 = self.value_store.pop_f64()?;
                let arg1 = self.value_store.pop_f64()?;
                let result: bool = arg1 < arg2;
                self.value_store.push(Value::I32(result as i32));
            }
            F64GT => {
                let arg2 = self.value_store.pop_f64()?;
                let arg1 = self.value_store.pop_f64()?;
                let result: bool = arg1 > arg2;
                self.value_store.push(Value::I32(result as i32));
            }
            F64LE => {
                let arg2 = self.value_store.pop_f64()?;
                let arg1 = self.value_store.pop_f64()?;
                let result: bool = arg1 <= arg2;
                self.value_store.push(Value::I32(result as i32));
            }
            F64GE => {
                let arg2 = self.value_store.pop_f64()?;
                let arg1 = self.value_store.pop_f64()?;
                let result: bool = arg1 >= arg2;
                self.value_store.push(Value::I32(result as i32));
            }

            I32CLZ => {
                let arg = self.value_store.pop_u32()?;
                self.value_store.push(Value::from(arg.leading_zeros()));
            }
            I32CTZ => {
                let arg = self.value_store.pop_u32()?;
                self.value_store.push(Value::from(arg.trailing_zeros()));
            }
            I32POPCNT => {
                let arg = self.value_store.pop_u32()?;
                self.value_store.push(Value::from(arg.count_ones()));
            }
            I32ADD => {
                let arg2 = self.value_store.pop_i32()?;
                let arg1 = self.value_store.pop_i32()?;
                self.value_store.push(Value::from(arg1.wrapping_add(arg2)));
            }
            I32SUB => {
                let arg2 = self.value_store.pop_i32()?;
                let arg1 = self.value_store.pop_i32()?;
                self.value_store.push(Value::from(arg1.wrapping_sub(arg2)));
            }
            I32MUL => {
                let arg2 = self.value_store.pop_i32()?;
                let arg1 = self.value_store.pop_i32()?;
                self.value_store.push(Value::from(arg1.wrapping_mul(arg2)));
            }
            I32DIVS => {
                let arg2 = self.value_store.pop_i32()?;
                let arg1 = self.value_store.pop_i32()?;
                self.value_store.push(Value::from(arg1.wrapping_div(arg2)));
            }
            I32DIVU => {
                let arg2 = self.value_store.pop_u32()?;
                let arg1 = self.value_store.pop_u32()?;
                self.value_store.push(Value::from(arg1.wrapping_div(arg2)));
            }
            I32REMS => {
                let arg2 = self.value_store.pop_i32()?;
                let arg1 = self.value_store.pop_i32()?;
                self.value_store.push(Value::from(arg1.wrapping_rem(arg2)));
            }
            I32REMU => {
                let arg2 = self.value_store.pop_u32()?;
                let arg1 = self.value_store.pop_u32()?;
                self.value_store.push(Value::from(arg1.wrapping_rem(arg2)));
            }
            I32AND => {
                let arg2 = self.value_store.pop_u32()?;
                let arg1 = self.value_store.pop_u32()?;
                self.value_store.push(Value::from(arg1 & arg2));
            }
            I32OR => {
                let arg2 = self.value_store.pop_u32()?;
                let arg1 = self.value_store.pop_u32()?;
                self.value_store.push(Value::from(arg1 | arg2));
            }
            I32XOR => {
                let arg2 = self.value_store.pop_u32()?;
                let arg1 = self.value_store.pop_u32()?;
                self.value_store.push(Value::from(arg1 ^ arg2));
            }
            I32SHL => {
                let arg2 = self.value_store.pop_u32()?;
                let arg1 = self.value_store.pop_u32()?;
                // Take modulo N as per the spec https://webassembly.github.io/spec/core/exec/numerics.html#op-ishl
                let k = arg2 % 32;
                self.value_store.push(Value::from(arg1 << k));
            }
            I32SHRS => {
                let arg2 = self.value_store.pop_i32()?;
                let arg1 = self.value_store.pop_i32()?;
                let k = arg2 % 32;
                self.value_store.push(Value::from(arg1 >> k));
            }
            I32SHRU => {
                let arg2 = self.value_store.pop_u32()?;
                let arg1 = self.value_store.pop_u32()?;
                let k = arg2 % 32;
                self.value_store.push(Value::from(arg1 >> k));
            }
            I32ROTL => {
                let arg2 = self.value_store.pop_u32()?;
                let arg1 = self.value_store.pop_u32()?;
                let k = arg2 % 32;
                self.value_store.push(Value::from(arg1.rotate_left(k)));
            }
            I32ROTR => {
                let arg2 = self.value_store.pop_u32()?;
                let arg1 = self.value_store.pop_u32()?;
                let k = arg2 % 32;
                self.value_store.push(Value::from(arg1.rotate_right(k)));
            }

            I64CLZ => {
                let arg = self.value_store.pop_u64()?;
                self.value_store
                    .push(Value::from(arg.leading_zeros() as u64));
            }
            I64CTZ => {
                let arg = self.value_store.pop_u64()?;
                self.value_store
                    .push(Value::from(arg.trailing_zeros() as u64));
            }
            I64POPCNT => {
                let arg = self.value_store.pop_u64()?;
                self.value_store.push(Value::from(arg.count_ones() as u64));
            }
            I64ADD => {
                let arg2 = self.value_store.pop_i64()?;
                let arg1 = self.value_store.pop_i64()?;
                self.value_store.push(Value::from(arg1.wrapping_add(arg2)));
            }
            I64SUB => {
                let arg2 = self.value_store.pop_i64()?;
                let arg1 = self.value_store.pop_i64()?;
                self.value_store.push(Value::from(arg1.wrapping_sub(arg2)));
            }
            I64MUL => {
                let arg2 = self.value_store.pop_i64()?;
                let arg1 = self.value_store.pop_i64()?;
                self.value_store.push(Value::from(arg1.wrapping_mul(arg2)));
            }
            I64DIVS => {
                let arg2 = self.value_store.pop_i64()?;
                let arg1 = self.value_store.pop_i64()?;
                self.value_store.push(Value::from(arg1.wrapping_div(arg2)));
            }
            I64DIVU => {
                let arg2 = self.value_store.pop_u64()?;
                let arg1 = self.value_store.pop_u64()?;
                self.value_store.push(Value::from(arg1.wrapping_div(arg2)));
            }
            I64REMS => {
                let arg2 = self.value_store.pop_i64()?;
                let arg1 = self.value_store.pop_i64()?;
                self.value_store.push(Value::from(arg1.wrapping_rem(arg2)));
            }
            I64REMU => {
                let arg2 = self.value_store.pop_u64()?;
                let arg1 = self.value_store.pop_u64()?;
                self.value_store.push(Value::from(arg1.wrapping_rem(arg2)));
            }
            I64AND => {
                let arg2 = self.value_store.pop_u64()?;
                let arg1 = self.value_store.pop_u64()?;
                self.value_store.push(Value::from(arg1 & arg2));
            }
            I64OR => {
                let arg2 = self.value_store.pop_u64()?;
                let arg1 = self.value_store.pop_u64()?;
                self.value_store.push(Value::from(arg1 | arg2));
            }
            I64XOR => {
                let arg2 = self.value_store.pop_u64()?;
                let arg1 = self.value_store.pop_u64()?;
                self.value_store.push(Value::from(arg1 ^ arg2));
            }
            I64SHL => {
                let arg2 = self.value_store.pop_u64()?;
                let arg1 = self.value_store.pop_u64()?;
                // Take modulo N as per the spec https://webassembly.github.io/spec/core/exec/numerics.html#op-ishl
                let k = arg2 % 64;
                self.value_store.push(Value::from(arg1 << k));
            }
            I64SHRS => {
                let arg2 = self.value_store.pop_i64()?;
                let arg1 = self.value_store.pop_i64()?;
                let k = arg2 % 64;
                self.value_store.push(Value::from(arg1 >> k));
            }
            I64SHRU => {
                let arg2 = self.value_store.pop_u64()?;
                let arg1 = self.value_store.pop_u64()?;
                let k = arg2 % 64;
                self.value_store.push(Value::from(arg1 >> k));
            }
            I64ROTL => {
                let arg2 = self.value_store.pop_u64()?;
                let arg1 = self.value_store.pop_u64()?;
                let k = (arg2 % 64) as u32;
                self.value_store.push(Value::from(arg1.rotate_left(k)));
            }
            I64ROTR => {
                let arg2 = self.value_store.pop_u64()?;
                let arg1 = self.value_store.pop_u64()?;
                let k = (arg2 % 64) as u32;
                self.value_store.push(Value::from(arg1.rotate_right(k)));
            }

            F32ABS => {
                let arg = self.value_store.pop_f32()?;
                self.value_store.push(Value::F32(arg.abs()));
            }
            F32NEG => {
                let arg = self.value_store.pop_f32()?;
                self.value_store.push(Value::F32(-arg));
            }
            F32CEIL => {
                let arg = self.value_store.pop_f32()?;
                self.value_store.push(Value::F32(arg.ceil()));
            }
            F32FLOOR => {
                let arg = self.value_store.pop_f32()?;
                self.value_store.push(Value::F32(arg.floor()));
            }
            F32TRUNC => {
                let arg = self.value_store.pop_f32()?;
                self.value_store.push(Value::F32(arg.trunc()));
            }
            F32NEAREST => {
                // https://webassembly.github.io/spec/core/exec/numerics.html#op-fnearest
                let arg = self.value_store.pop_f32()?;
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
                self.value_store.push(Value::F32(result));
            }
            F32SQRT => {
                let arg = self.value_store.pop_f32()?;
                self.value_store.push(Value::F32(arg.sqrt()));
            }
            F32ADD => {
                let arg2 = self.value_store.pop_f32()?;
                let arg1 = self.value_store.pop_f32()?;
                self.value_store.push(Value::F32(arg1 + arg2));
            }
            F32SUB => {
                let arg2 = self.value_store.pop_f32()?;
                let arg1 = self.value_store.pop_f32()?;
                self.value_store.push(Value::F32(arg1 - arg2));
            }
            F32MUL => {
                let arg2 = self.value_store.pop_f32()?;
                let arg1 = self.value_store.pop_f32()?;
                self.value_store.push(Value::F32(arg1 * arg2));
            }
            F32DIV => {
                let arg2 = self.value_store.pop_f32()?;
                let arg1 = self.value_store.pop_f32()?;
                self.value_store.push(Value::F32(arg1 / arg2));
            }
            F32MIN => {
                let arg2 = self.value_store.pop_f32()?;
                let arg1 = self.value_store.pop_f32()?;
                let result = if arg1 < arg2 { arg1 } else { arg2 };
                self.value_store.push(Value::F32(result));
            }
            F32MAX => {
                let arg2 = self.value_store.pop_f32()?;
                let arg1 = self.value_store.pop_f32()?;
                let result = if arg1 > arg2 { arg1 } else { arg2 };
                self.value_store.push(Value::F32(result));
            }
            F32COPYSIGN => {
                let arg2 = self.value_store.pop_f32()?;
                let arg1 = self.value_store.pop_f32()?;
                let result = if arg1.is_sign_negative() == arg2.is_sign_negative() {
                    arg1
                } else {
                    arg2
                };
                self.value_store.push(Value::F32(result));
            }

            F64ABS => {
                let arg = self.value_store.pop_f64()?;
                self.value_store.push(Value::F64(arg.abs()));
            }
            F64NEG => {
                let arg = self.value_store.pop_f64()?;
                self.value_store.push(Value::F64(-arg));
            }
            F64CEIL => {
                let arg = self.value_store.pop_f64()?;
                self.value_store.push(Value::F64(arg.ceil()));
            }
            F64FLOOR => {
                let arg = self.value_store.pop_f64()?;
                self.value_store.push(Value::F64(arg.floor()));
            }
            F64TRUNC => {
                let arg = self.value_store.pop_f64()?;
                self.value_store.push(Value::F64(arg.trunc()));
            }
            F64NEAREST => {
                // https://webassembly.github.io/spec/core/exec/numerics.html#op-fnearest
                let arg = self.value_store.pop_f64()?;
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
                self.value_store.push(Value::F64(result));
            }
            F64SQRT => {
                let arg = self.value_store.pop_f64()?;
                self.value_store.push(Value::F64(arg.sqrt()));
            }
            F64ADD => {
                let arg2 = self.value_store.pop_f64()?;
                let arg1 = self.value_store.pop_f64()?;
                self.value_store.push(Value::F64(arg1 + arg2));
            }
            F64SUB => {
                let arg2 = self.value_store.pop_f64()?;
                let arg1 = self.value_store.pop_f64()?;
                self.value_store.push(Value::F64(arg1 - arg2));
            }
            F64MUL => {
                let arg2 = self.value_store.pop_f64()?;
                let arg1 = self.value_store.pop_f64()?;
                self.value_store.push(Value::F64(arg1 * arg2));
            }
            F64DIV => {
                let arg2 = self.value_store.pop_f64()?;
                let arg1 = self.value_store.pop_f64()?;
                self.value_store.push(Value::F64(arg1 / arg2));
            }
            F64MIN => {
                let arg2 = self.value_store.pop_f64()?;
                let arg1 = self.value_store.pop_f64()?;
                let result = if arg1 < arg2 { arg1 } else { arg2 };
                self.value_store.push(Value::F64(result));
            }
            F64MAX => {
                let arg2 = self.value_store.pop_f64()?;
                let arg1 = self.value_store.pop_f64()?;
                let result = if arg1 > arg2 { arg1 } else { arg2 };
                self.value_store.push(Value::F64(result));
            }
            F64COPYSIGN => {
                let arg2 = self.value_store.pop_f64()?;
                let arg1 = self.value_store.pop_f64()?;
                let result = if arg1.is_sign_negative() == arg2.is_sign_negative() {
                    arg1
                } else {
                    arg2
                };
                self.value_store.push(Value::F64(result));
            }

            I32WRAPI64 => {
                let arg = self.value_store.pop_u64()?;
                let wrapped: u32 = (arg & 0xffff_ffff) as u32;
                self.value_store.push(Value::from(wrapped));
            }
            I32TRUNCSF32 => {
                let arg = self.value_store.pop_f32()?;
                if arg < i32::MIN as f32 || arg > i32::MAX as f32 {
                    panic!("Cannot truncate {arg} from F32 to I32");
                }
                self.value_store.push(Value::I32(arg as i32));
            }
            I32TRUNCUF32 => {
                let arg = self.value_store.pop_f32()?;
                if arg < u32::MIN as f32 || arg > u32::MAX as f32 {
                    panic!("Cannot truncate {arg} from F32 to unsigned I32");
                }
                self.value_store.push(Value::from(arg as u32));
            }
            I32TRUNCSF64 => {
                let arg = self.value_store.pop_f64()?;
                if arg < i32::MIN as f64 || arg > i32::MAX as f64 {
                    panic!("Cannot truncate {arg} from F64 to I32");
                }
                self.value_store.push(Value::I32(arg as i32));
            }
            I32TRUNCUF64 => {
                let arg = self.value_store.pop_f64()?;
                if arg < u32::MIN as f64 || arg > u32::MAX as f64 {
                    panic!("Cannot truncate {arg} from F64 to unsigned I32");
                }
                self.value_store.push(Value::from(arg as u32));
            }
            I64EXTENDSI32 => {
                let arg = self.value_store.pop_i32()?;
                self.value_store.push(Value::I64(arg as i64));
            }
            I64EXTENDUI32 => {
                let arg = self.value_store.pop_u32()?;
                self.value_store.push(Value::from(arg as u64));
            }
            I64TRUNCSF32 => {
                let arg = self.value_store.pop_f32()?;
                if arg < i64::MIN as f32 || arg > i64::MAX as f32 {
                    panic!("Cannot truncate {arg} from F32 to I64");
                }
                self.value_store.push(Value::I64(arg as i64));
            }
            I64TRUNCUF32 => {
                let arg = self.value_store.pop_f32()?;
                if arg < u64::MIN as f32 || arg > u64::MAX as f32 {
                    panic!("Cannot truncate {arg} from F32 to unsigned I64");
                }
                self.value_store.push(Value::from(arg as u64));
            }
            I64TRUNCSF64 => {
                let arg = self.value_store.pop_f64()?;
                if arg < i64::MIN as f64 || arg > i64::MAX as f64 {
                    panic!("Cannot truncate {arg} from F64 to I64");
                }
                self.value_store.push(Value::I64(arg as i64));
            }
            I64TRUNCUF64 => {
                let arg = self.value_store.pop_f64()?;
                if arg < u64::MIN as f64 || arg > u64::MAX as f64 {
                    panic!("Cannot truncate {arg} from F64 to unsigned I64");
                }
                self.value_store.push(Value::from(arg as u64));
            }
            F32CONVERTSI32 => {
                let arg = self.value_store.pop_i32()?;
                self.value_store.push(Value::F32(arg as f32));
            }
            F32CONVERTUI32 => {
                let arg = self.value_store.pop_u32()?;
                self.value_store.push(Value::F32(arg as f32));
            }
            F32CONVERTSI64 => {
                let arg = self.value_store.pop_i64()?;
                self.value_store.push(Value::F32(arg as f32));
            }
            F32CONVERTUI64 => {
                let arg = self.value_store.pop_u64()?;
                self.value_store.push(Value::F32(arg as f32));
            }
            F32DEMOTEF64 => {
                let arg = self.value_store.pop_f64()?;
                self.value_store.push(Value::F32(arg as f32));
            }
            F64CONVERTSI32 => {
                let arg = self.value_store.pop_i32()?;
                self.value_store.push(Value::F64(arg as f64));
            }
            F64CONVERTUI32 => {
                let arg = self.value_store.pop_u32()?;
                self.value_store.push(Value::F64(arg as f64));
            }
            F64CONVERTSI64 => {
                let arg = self.value_store.pop_i64()?;
                self.value_store.push(Value::F64(arg as f64));
            }
            F64CONVERTUI64 => {
                let arg = self.value_store.pop_u64()?;
                self.value_store.push(Value::F64(arg as f64));
            }
            F64PROMOTEF32 => {
                let arg = self.value_store.pop_f32()?;
                self.value_store.push(Value::F64(arg as f64));
            }

            I32REINTERPRETF32 => {
                let x = self.value_store.pop_f32()?;
                self.value_store
                    .push(Value::I32(i32::from_ne_bytes(x.to_ne_bytes())));
            }
            I64REINTERPRETF64 => {
                let x = self.value_store.pop_f64()?;
                self.value_store
                    .push(Value::I64(i64::from_ne_bytes(x.to_ne_bytes())));
            }
            F32REINTERPRETI32 => {
                let x = self.value_store.pop_i32()?;
                self.value_store
                    .push(Value::F32(f32::from_ne_bytes(x.to_ne_bytes())));
            }
            F64REINTERPRETI64 => {
                let x = self.value_store.pop_i64()?;
                self.value_store
                    .push(Value::F64(f64::from_ne_bytes(x.to_ne_bytes())));
            }
            I32EXTEND8S => {
                let x = self.value_store.pop_i32()?;
                self.value_store.push(Value::I32(x as i8 as i32));
            }
            I32EXTEND16S => {
                let x = self.value_store.pop_i32()?;
                self.value_store.push(Value::I32(x as i16 as i32));
            }
            I64EXTEND8S => {
                let x = self.value_store.pop_i64()?;
                self.value_store.push(Value::I64(x as i8 as i64));
            }
            I64EXTEND16S => {
                let x = self.value_store.pop_i64()?;
                self.value_store.push(Value::I64(x as i16 as i64));
            }
            I64EXTEND32S => {
                let x = self.value_store.pop_i64()?;
                self.value_store.push(Value::I64(x as i32 as i64));
            }
        }

        if let Some(debug_string) = &self.debug_string {
            if matches!(op_code, CALL | CALLINDIRECT) {
                eprintln!("\n{file_offset:06x} {debug_string}");
            } else {
                // For calls, we print special debug stuff in do_call
                let base = self.current_frame.locals_start + self.current_frame.locals_count;
                let slice = self.value_store.get_slice(base);
                eprintln!("{file_offset:06x} {debug_string:17} {slice:x?}");
            }
            let is_return = op_code == RETURN || (op_code == END && implicit_return);
            let is_program_end = self.program_counter == 0;
            if is_return && !is_program_end {
                eprintln!(
                    "returning to function {} at {:06x}",
                    self.current_frame.fn_index,
                    self.program_counter + self.module.code.section_offset as usize,
                );
            }
        }

        Ok(action)
    }

    #[allow(dead_code)]
    fn debug_values_and_blocks(&self, label: &str) {
        eprintln!("\n========== {label} ==========");

        let mut block_str = String::new();
        let mut block_iter = self.blocks.iter().enumerate();
        let mut block = block_iter.next();

        let mut print_blocks = |i| {
            block_str.clear();
            while let Some((b, Block { vstack, ty })) = block {
                if *vstack > i {
                    break;
                }
                write!(block_str, "{b}:{ty:?} ").unwrap();
                block = block_iter.next();
            }
            if !block_str.is_empty() {
                eprintln!("--------------- {block_str}");
            }
        };

        for (i, v) in self.value_store.iter().enumerate() {
            print_blocks(i);
            eprintln!("{i:3} {v:x?}");
        }
        print_blocks(self.value_store.depth());

        eprintln!();
    }

    /// Dump a stack trace when an error occurs
    /// --------------
    /// func[123]
    ///   address  0x12345
    ///   args     0: I64(234), 1: F64(7.15)
    ///   locals   2: I32(412), 3: F64(3.14)
    ///   stack    [I64(111), F64(3.14)]
    /// --------------
    fn debug_stack_trace(&self, buffer: &mut String) -> fmt::Result {
        let divider = "-------------------";
        writeln!(buffer, "{divider}")?;

        let frames = self.previous_frames.iter().chain(once(&self.current_frame));
        let next_frames = frames.clone().skip(1);

        // Find the code address to display for each frame
        // For previous frames, show the address of the CALL instruction
        // For the current frame, show the program counter value
        let mut execution_addrs = {
            // for each previous_frame, find return address of the *next* frame
            let return_addrs = next_frames.clone().map(|f| f.return_addr);
            // roll back to the CALL instruction before that return address, it's more meaningful.
            let call_addrs = return_addrs.map(|ra| self.debug_return_addr_to_call_addr(ra));
            // For the current frame, show the program_counter
            call_addrs.chain(once(self.program_counter))
        };

        let mut frame_ends = next_frames.map(|f| f.locals_start);

        for frame in frames {
            let Frame {
                fn_index,
                locals_count,
                locals_start,
                ..
            } = frame;

            let arg_count = {
                let signature_index = if *fn_index < self.import_count {
                    match self.module.import.imports[*fn_index].description {
                        ImportDesc::Func { signature_index } => signature_index,
                        _ => unreachable!(),
                    }
                } else {
                    self.module.function.signatures[fn_index - self.import_count]
                };
                self.module.types.look_up(signature_index).0.len()
            };

            let fn_name = self
                .module
                .names
                .function_names
                .iter()
                .find(|(idx, _)| *idx == *fn_index as u32)
                .map(|(_, name)| *name)
                .unwrap_or("");

            // Function and address match wasm-objdump formatting, for easy copy & find
            writeln!(buffer, "func[{fn_index}]  {fn_name}")?;
            writeln!(buffer, "  address  {:06x}", execution_addrs.next().unwrap())?;

            write!(buffer, "  args     ")?;
            for local_index in 0..*locals_count {
                let value = self.value_store.get(locals_start + local_index).unwrap();
                if local_index == arg_count {
                    write!(buffer, "\n  locals   ")?;
                } else if local_index != 0 {
                    write!(buffer, ", ")?;
                }
                write!(buffer, "{local_index}: {value:?}")?;
            }

            write!(buffer, "\n  stack    [")?;
            let frame_end = frame_ends
                .next()
                .unwrap_or_else(|| self.value_store.depth());
            let stack_start = locals_start + locals_count;
            for i in stack_start..frame_end {
                let value = self.value_store.get(i).unwrap();
                if i != stack_start {
                    write!(buffer, ", ")?;
                }
                write!(buffer, "{value:?}")?;
            }
            writeln!(buffer, "]")?;
            writeln!(buffer, "{divider}")?;
        }

        Ok(())
    }

    // Call address is more intuitive than the return address in the stack trace. Search backward for it.
    fn debug_return_addr_to_call_addr(&self, return_addr: usize) -> usize {
        // return_addr is pointing at the next instruction after the CALL/CALLINDIRECT.
        // Just before that is the LEB-128 function index or type index.
        // The last LEB-128 byte is <128, but the others are >=128 so we can't mistake them for CALL/CALLINDIRECT
        let mut call_addr = return_addr - 2;
        loop {
            let byte = self.module.code.bytes[call_addr];
            if byte == OpCode::CALL as u8 || byte == OpCode::CALLINDIRECT as u8 {
                break;
            } else {
                call_addr -= 1;
            }
        }
        call_addr
    }
}
