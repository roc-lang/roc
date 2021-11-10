use roc_builtins::bitcode::{self, FloatWidth};
use roc_module::low_level::LowLevel;
use roc_module::symbol::Symbol;

use crate::layout::WasmLayout;
use crate::storage::Storage;
use crate::wasm_module::{CodeBuilder, ValueType};

pub enum LowlevelBuildResult {
    WasmInstructions,
    BuiltinCall(&'static str),
    NotImplemented,
}

pub fn build_call_low_level<'a>(
    code_builder: &mut CodeBuilder<'a>,
    storage: &mut Storage<'a>,
    lowlevel: &LowLevel,
    args: &'a [Symbol],
    ret_layout: &WasmLayout,
) -> LowlevelBuildResult {
    use LowlevelBuildResult::*;

    let panic_ret_type = || panic!("Invalid return layout for {:?}: {:?}", lowlevel, ret_layout);

    match lowlevel {
        LowLevel::NumAdd => match ret_layout.value_type() {
            ValueType::I32 => code_builder.i32_add(),
            ValueType::I64 => code_builder.i64_add(),
            ValueType::F32 => code_builder.f32_add(),
            ValueType::F64 => code_builder.f64_add(),
        },
        LowLevel::NumSub => match ret_layout.value_type() {
            ValueType::I32 => code_builder.i32_sub(),
            ValueType::I64 => code_builder.i64_sub(),
            ValueType::F32 => code_builder.f32_sub(),
            ValueType::F64 => code_builder.f64_sub(),
        },
        LowLevel::NumMul => match ret_layout.value_type() {
            ValueType::I32 => code_builder.i32_mul(),
            ValueType::I64 => code_builder.i64_mul(),
            ValueType::F32 => code_builder.f32_mul(),
            ValueType::F64 => code_builder.f64_mul(),
        },
        LowLevel::NumGt => match storage.get(&args[0]).value_type() {
            ValueType::I32 => code_builder.i32_gt_s(),
            ValueType::I64 => code_builder.i64_gt_s(),
            ValueType::F32 => code_builder.f32_gt(),
            ValueType::F64 => code_builder.f64_gt(),
        },
        LowLevel::Eq => match storage.get(&args[0]).value_type() {
            ValueType::I32 => code_builder.i32_eq(),
            ValueType::I64 => code_builder.i64_eq(),
            ValueType::F32 => code_builder.f32_eq(),
            ValueType::F64 => code_builder.f64_eq(),
        },
        LowLevel::NumNeg => match ret_layout.value_type() {
            // TODO: it would be better to subtract the arg from zero.
            // But we'd need to insert the zero constant *before* the argument
            // in the VM stack, and we don't have a good way to do that yet!
            // Before solving this one case, let's see what other issues we run into.
            ValueType::I32 => {
                code_builder.i32_const(-1);
                code_builder.i32_mul();
            }
            ValueType::I64 => {
                code_builder.i64_const(-1);
                code_builder.i64_mul();
            }
            ValueType::F32 => code_builder.f32_neg(),
            ValueType::F64 => code_builder.f64_neg(),
        },
        LowLevel::NumAtan => {
            let name: &'static str = match ret_layout.value_type() {
                ValueType::F32 => &bitcode::NUM_ATAN[FloatWidth::F32],
                ValueType::F64 => &bitcode::NUM_ATAN[FloatWidth::F64],
                _ => panic_ret_type(),
            };
            return BuiltinCall(name);
        }
        _ => {
            return NotImplemented;
        }
    };
    WasmInstructions
}
