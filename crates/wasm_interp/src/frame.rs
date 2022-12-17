use roc_wasm_module::{parse::Parse, Value, ValueType, WasmModule};
use std::fmt;
use std::iter::repeat;

use crate::value_stack::ValueStack;

#[derive(Debug)]
pub struct Frame {
    /// The function this frame belongs to
    pub fn_index: usize,
    /// Address in the code section where this frame returns to
    pub return_addr: usize,
    /// Depth of the "function body block" for this frame
    pub body_block_index: usize,
    /// Offset in the ValueStack where the args & locals begin
    pub locals_start: usize,
    /// Number of args & locals in the frame
    pub locals_count: usize,
    /// Expected return type, if any
    pub return_type: Option<ValueType>,
}

impl Frame {
    pub fn new() -> Self {
        Frame {
            fn_index: 0,
            return_addr: 0,
            body_block_index: 0,
            locals_start: 0,
            locals_count: 0,
            return_type: None,
        }
    }

    pub fn enter(
        fn_index: usize,
        return_addr: usize,
        body_block_index: usize,
        n_args: usize,
        return_type: Option<ValueType>,
        code_bytes: &[u8],
        value_stack: &mut ValueStack<'_>,
        pc: &mut usize,
    ) -> Self {
        let locals_start = value_stack.depth() - n_args;

        // Parse local variable declarations in the function header. They're grouped by type.
        let local_group_count = u32::parse((), code_bytes, pc).unwrap();
        for _ in 0..local_group_count {
            let (group_size, ty) = <(u32, ValueType)>::parse((), code_bytes, pc).unwrap();
            let n = group_size as usize;
            let zero = match ty {
                ValueType::I32 => Value::I32(0),
                ValueType::I64 => Value::I64(0),
                ValueType::F32 => Value::F32(0.0),
                ValueType::F64 => Value::F64(0.0),
            };
            value_stack.extend(repeat(zero).take(n));
        }

        let locals_count = value_stack.depth() - locals_start;

        Frame {
            fn_index,
            return_addr,
            body_block_index,
            locals_start,
            locals_count,
            return_type,
        }
    }

    pub fn get_local(&self, values: &ValueStack<'_>, index: u32) -> Value {
        debug_assert!((index as usize) < self.locals_count);
        *values.get(self.locals_start + index as usize).unwrap()
    }

    pub fn set_local(&self, values: &mut ValueStack<'_>, index: u32, value: Value) {
        debug_assert!((index as usize) < self.locals_count);
        values.set(self.locals_start + index as usize, value)
    }
}

#[allow(dead_code)]
pub fn write_stack_trace(
    _current_frame: &Frame,
    _previous_frames: &[Frame],
    _module: &WasmModule<'_>,
    value_stack: &ValueStack<'_>,
    _pc: usize,
    _buffer: &mut String,
) -> fmt::Result {
    let _ = value_stack.iter();

    // let divider = "-------------------";
    // writeln!(buffer, "{}", divider)?;

    // let mut value_stack_iter = value_stack.iter();

    // for frame in 0..self.frame_offsets.len() {
    //     let next_frame = frame + 1;
    //     let op_offset = if next_frame < self.frame_offsets.len() {
    //         // return address of next frame = next op in this frame
    //         let next_op = self.return_addrs_and_block_depths[next_frame].0 as usize;
    //         // Call address is more intuitive than the return address when debugging. Search backward for it.
    //         // Skip last byte of function index to avoid a false match with CALL/CALLINDIRECT.
    //         // The more significant bytes won't match because of LEB-128 encoding.
    //         let mut call_op = next_op - 2;
    //         loop {
    //             let byte = module.code.bytes[call_op];
    //             if byte == OpCode::CALL as u8 || byte == OpCode::CALLINDIRECT as u8 {
    //                 break;
    //             } else {
    //                 call_op -= 1;
    //             }
    //         }
    //         call_op
    //     } else {
    //         pc
    //     };

    //     let fn_index = pc_to_fn_index(op_offset, module);
    //     let address = op_offset + module.code.section_offset as usize;
    //     writeln!(buffer, "function {}", fn_index)?;
    //     writeln!(buffer, "  address  {:06x}", address)?; // format matches wasm-objdump, for easy search

    //     write!(buffer, "  args     ")?;
    //     let arg_count = {
    //         let n_import_fns = module.import.imports.len();
    //         let signature_index = if fn_index < n_import_fns {
    //             match module.import.imports[fn_index].description {
    //                 ImportDesc::Func { signature_index } => signature_index,
    //                 _ => unreachable!(),
    //             }
    //         } else {
    //             module.function.signatures[fn_index - n_import_fns]
    //         };
    //         module.types.look_up_arg_type_bytes(signature_index).len()
    //     };
    //     let args_and_locals_count = {
    //         let frame_offset = self.frame_offsets[frame] as usize;
    //         let next_frame_offset = if frame == self.frame_offsets.len() - 1 {
    //             self.locals.len()
    //         } else {
    //             self.frame_offsets[frame + 1] as usize
    //         };
    //         next_frame_offset - frame_offset
    //     };
    //     for index in 0..args_and_locals_count {
    //         let value = self.get_local_help(frame, index as u32);
    //         if index != 0 {
    //             write!(buffer, ", ")?;
    //         }
    //         if index == arg_count {
    //             write!(buffer, "\n  locals   ")?;
    //         }
    //         write!(buffer, "{}: {:?}", index, value)?;
    //     }
    //     write!(buffer, "\n  stack    [")?;

    //     let frame_value_count = {
    //         let value_stack_base = self.value_stack_bases[frame];
    //         let next_value_stack_base = if frame == self.frame_offsets.len() - 1 {
    //             value_stack.depth() as u32
    //         } else {
    //             self.value_stack_bases[frame + 1]
    //         };
    //         next_value_stack_base - value_stack_base
    //     };
    //     for i in 0..frame_value_count {
    //         if i != 0 {
    //             write!(buffer, ", ")?;
    //         }
    //         if let Some(value) = value_stack_iter.next() {
    //             write!(buffer, "{:?}", value)?;
    //         }
    //     }

    //     writeln!(buffer, "]")?;
    //     writeln!(buffer, "{}", divider)?;
    // }

    // Ok(())

    todo!()
}
