use roc_builtins::bitcode::{self, FloatWidth};
use roc_module::low_level::{LowLevel, LowLevel::*};
use roc_module::symbol::Symbol;

use crate::layout::WasmLayout;
use crate::storage::Storage;
use crate::wasm_module::{CodeBuilder, ValueType::*};

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
        NumAdd => match ret_layout.value_type() {
            I32 => code_builder.i32_add(),
            I64 => code_builder.i64_add(),
            F32 => code_builder.f32_add(),
            F64 => code_builder.f64_add(),
        },
        NumSub => match ret_layout.value_type() {
            I32 => code_builder.i32_sub(),
            I64 => code_builder.i64_sub(),
            F32 => code_builder.f32_sub(),
            F64 => code_builder.f64_sub(),
        },
        NumMul => match ret_layout.value_type() {
            I32 => code_builder.i32_mul(),
            I64 => code_builder.i64_mul(),
            F32 => code_builder.f32_mul(),
            F64 => code_builder.f64_mul(),
        },
        NumGt => match storage.get(&args[0]).value_type() {
            I32 => code_builder.i32_gt_s(),
            I64 => code_builder.i64_gt_s(),
            F32 => code_builder.f32_gt(),
            F64 => code_builder.f64_gt(),
        },
        Eq => match storage.get(&args[0]).value_type() {
            I32 => code_builder.i32_eq(),
            I64 => code_builder.i64_eq(),
            F32 => code_builder.f32_eq(),
            F64 => code_builder.f64_eq(),
        },
        NumNeg => match ret_layout.value_type() {
            // TODO: it would be better to subtract the arg from zero.
            // But we'd need to insert the zero constant *before* the argument
            // in the VM stack, and we don't have a good way to do that yet!
            // Before solving this one case, let's see what other issues we run into.
            I32 => {
                code_builder.i32_const(-1);
                code_builder.i32_mul();
            }
            I64 => {
                code_builder.i64_const(-1);
                code_builder.i64_mul();
            }
            F32 => code_builder.f32_neg(),
            F64 => code_builder.f64_neg(),
        },
        NumAtan => {
            let name: &'static str = match ret_layout.value_type() {
                F32 => &bitcode::NUM_ATAN[FloatWidth::F32],
                F64 => &bitcode::NUM_ATAN[FloatWidth::F64],
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
