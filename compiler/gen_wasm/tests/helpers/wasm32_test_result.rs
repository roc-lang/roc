use parity_wasm::elements::{Instruction, Instruction::*};

use roc_gen_wasm::from_wasm32_memory::FromWasm32Memory;
use roc_gen_wasm::*;
use roc_std::{RocDec, RocList, RocOrder, RocStr};

pub trait Wasm32TestResult {
    fn gen_wrapper(main_function_index: u32) -> Vec<Instruction>;
}

const RESULT_POINTER_LOCAL_ID: u32 = 0;

fn gen_wrapper_prelude(stack_memory_size: usize) -> Vec<Instruction> {
    vec![
        GetGlobal(STACK_POINTER_GLOBAL_ID),
        TeeLocal(RESULT_POINTER_LOCAL_ID),
        I32Const(stack_memory_size as i32),
        I32Add,
        SetGlobal(STACK_POINTER_GLOBAL_ID),
    ]
}

macro_rules! gen_wrapper_primitive {
    ($store_instruction: expr, $align: expr) => {
        fn gen_wrapper(main_function_index: u32) -> Vec<Instruction> {
            const MAX_ALIGNED_SIZE: usize = 16;
            let mut instructions = gen_wrapper_prelude(MAX_ALIGNED_SIZE);
            instructions.extend([
                //
                // Call the main function with no arguments. Get primitive back.
                Call(main_function_index),
                //
                // Store the result at the allocated address
                GetLocal(RESULT_POINTER_LOCAL_ID),
                $store_instruction($align, 0),
                //
                // Return the result pointer
                GetLocal(RESULT_POINTER_LOCAL_ID),
                End,
            ]);
            instructions
        }
    };
}

macro_rules! wasm_test_result_primitive {
    ($type_name: ident, $store_instruction: expr, $align: expr) => {
        impl Wasm32TestResult for $type_name {
            gen_wrapper_primitive!($store_instruction, $align);
        }
    };
}

fn gen_wrapper_stack_memory(main_function_index: u32, size: usize) -> Vec<Instruction> {
    let mut instructions = gen_wrapper_prelude(size);
    instructions.extend([
        //
        // Call the main function with a result pointer it can write to.
        // No Wasm return value, it just writes to memory.
        GetLocal(RESULT_POINTER_LOCAL_ID),
        Call(main_function_index),
        //
        // Return the result pointer
        GetLocal(RESULT_POINTER_LOCAL_ID),
        End,
    ]);
    instructions
}

macro_rules! wasm_test_result_stack_memory {
    ($type_name: ident) => {
        impl Wasm32TestResult for $type_name {
            fn gen_wrapper(main_function_index: u32) -> Vec<Instruction> {
                gen_wrapper_stack_memory(main_function_index, $type_name::ACTUAL_WIDTH)
            }
        }
    };
}

wasm_test_result_primitive!(bool, I32Store8, ALIGN_1);
wasm_test_result_primitive!(RocOrder, I32Store8, ALIGN_1);

wasm_test_result_primitive!(u8, I32Store8, ALIGN_1);
wasm_test_result_primitive!(i8, I32Store8, ALIGN_1);
wasm_test_result_primitive!(u16, I32Store16, ALIGN_2);
wasm_test_result_primitive!(i16, I32Store16, ALIGN_2);
wasm_test_result_primitive!(u32, I32Store, ALIGN_4);
wasm_test_result_primitive!(i32, I32Store, ALIGN_4);
wasm_test_result_primitive!(u64, I64Store, ALIGN_8);
wasm_test_result_primitive!(i64, I64Store, ALIGN_8);

wasm_test_result_primitive!(f32, F32Store, ALIGN_8);
wasm_test_result_primitive!(f64, F64Store, ALIGN_8);

wasm_test_result_stack_memory!(u128);
wasm_test_result_stack_memory!(i128);
wasm_test_result_stack_memory!(RocDec);
wasm_test_result_stack_memory!(RocStr);

impl<T: Wasm32TestResult> Wasm32TestResult for RocList<T> {
    fn gen_wrapper(main_function_index: u32) -> Vec<Instruction> {
        gen_wrapper_stack_memory(main_function_index, 12)
    }
}

impl<T: Wasm32TestResult> Wasm32TestResult for &'_ T {
    gen_wrapper_primitive!(I32Store, ALIGN_4);
}

impl<T, const N: usize> Wasm32TestResult for [T; N]
where
    T: Wasm32TestResult + FromWasm32Memory,
{
    fn gen_wrapper(main_function_index: u32) -> Vec<Instruction> {
        gen_wrapper_stack_memory(main_function_index, N * T::ACTUAL_WIDTH)
    }
}

impl<T, U> Wasm32TestResult for (T, U)
where
    T: Wasm32TestResult + FromWasm32Memory,
    U: Wasm32TestResult + FromWasm32Memory,
{
    fn gen_wrapper(main_function_index: u32) -> Vec<Instruction> {
        gen_wrapper_stack_memory(main_function_index, T::ACTUAL_WIDTH + U::ACTUAL_WIDTH)
    }
}

impl<T, U, V> Wasm32TestResult for (T, U, V)
where
    T: Wasm32TestResult + FromWasm32Memory,
    U: Wasm32TestResult + FromWasm32Memory,
    V: Wasm32TestResult + FromWasm32Memory,
{
    fn gen_wrapper(main_function_index: u32) -> Vec<Instruction> {
        gen_wrapper_stack_memory(
            main_function_index,
            T::ACTUAL_WIDTH + U::ACTUAL_WIDTH + V::ACTUAL_WIDTH,
        )
    }
}
