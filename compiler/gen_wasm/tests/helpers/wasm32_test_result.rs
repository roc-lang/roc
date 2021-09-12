use parity_wasm::builder;
use parity_wasm::builder::ModuleBuilder;
use parity_wasm::elements::{Instruction, Instruction::*, Instructions, Internal, ValueType};

use roc_gen_wasm::from_wasm32_memory::FromWasm32Memory;
use roc_gen_wasm::*;
use roc_std::{RocDec, RocList, RocOrder, RocStr};

pub trait Wasm32TestResult {
    fn insert_test_wrapper(
        module_builder: &mut ModuleBuilder,
        wrapper_name: &str,
        main_function_index: u32,
    ) {
        let instructions = Self::build_wrapper_body(main_function_index);

        let signature = builder::signature().with_result(ValueType::I32).build_sig();

        let function_def = builder::function()
            .with_signature(signature)
            .body()
            .with_instructions(Instructions::new(instructions))
            .build() // body
            .build(); // function

        let location = module_builder.push_function(function_def);
        let export = builder::export()
            .field(wrapper_name)
            .with_internal(Internal::Function(location.body))
            .build();

        module_builder.push_export(export);
    }

    fn build_wrapper_body(main_function_index: u32) -> Vec<Instruction>;
}

fn build_wrapper_body_prelude(stack_memory_size: usize) -> Vec<Instruction> {
    vec![
        GetGlobal(STACK_POINTER_GLOBAL_ID),
        I32Const(stack_memory_size as i32),
        I32Sub,
        SetGlobal(STACK_POINTER_GLOBAL_ID),
    ]
}

macro_rules! build_wrapper_body_primitive {
    ($store_instruction: expr, $align: expr) => {
        fn build_wrapper_body(main_function_index: u32) -> Vec<Instruction> {
            const MAX_ALIGNED_SIZE: usize = 16;
            let mut instructions = build_wrapper_body_prelude(MAX_ALIGNED_SIZE);
            instructions.extend([
                GetGlobal(STACK_POINTER_GLOBAL_ID),
                //
                // Call the main function with no arguments. Get primitive back.
                Call(main_function_index),
                //
                // Store the primitive at the allocated address
                $store_instruction($align, 0),
                //
                // Return the result pointer
                GetGlobal(STACK_POINTER_GLOBAL_ID),
                End,
            ]);
            instructions
        }
    };
}

macro_rules! wasm_test_result_primitive {
    ($type_name: ident, $store_instruction: expr, $align: expr) => {
        impl Wasm32TestResult for $type_name {
            build_wrapper_body_primitive!($store_instruction, $align);
        }
    };
}

fn build_wrapper_body_stack_memory(main_function_index: u32, size: usize) -> Vec<Instruction> {
    let mut instructions = build_wrapper_body_prelude(size);
    instructions.extend([
        //
        // Call the main function with the allocated address to write the result.
        // No value is returned to the VM stack. This is the same as in compiled C.
        GetGlobal(STACK_POINTER_GLOBAL_ID),
        Call(main_function_index),
        //
        // Return the result address
        GetGlobal(STACK_POINTER_GLOBAL_ID),
        End,
    ]);
    instructions
}

macro_rules! wasm_test_result_stack_memory {
    ($type_name: ident) => {
        impl Wasm32TestResult for $type_name {
            fn build_wrapper_body(main_function_index: u32) -> Vec<Instruction> {
                build_wrapper_body_stack_memory(main_function_index, $type_name::ACTUAL_WIDTH)
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
    fn build_wrapper_body(main_function_index: u32) -> Vec<Instruction> {
        build_wrapper_body_stack_memory(main_function_index, 12)
    }
}

impl<T: Wasm32TestResult> Wasm32TestResult for &'_ T {
    build_wrapper_body_primitive!(I32Store, ALIGN_4);
}

impl<T, const N: usize> Wasm32TestResult for [T; N]
where
    T: Wasm32TestResult + FromWasm32Memory,
{
    fn build_wrapper_body(main_function_index: u32) -> Vec<Instruction> {
        build_wrapper_body_stack_memory(main_function_index, N * T::ACTUAL_WIDTH)
    }
}

impl<T, U> Wasm32TestResult for (T, U)
where
    T: Wasm32TestResult + FromWasm32Memory,
    U: Wasm32TestResult + FromWasm32Memory,
{
    fn build_wrapper_body(main_function_index: u32) -> Vec<Instruction> {
        build_wrapper_body_stack_memory(main_function_index, T::ACTUAL_WIDTH + U::ACTUAL_WIDTH)
    }
}

impl<T, U, V> Wasm32TestResult for (T, U, V)
where
    T: Wasm32TestResult + FromWasm32Memory,
    U: Wasm32TestResult + FromWasm32Memory,
    V: Wasm32TestResult + FromWasm32Memory,
{
    fn build_wrapper_body(main_function_index: u32) -> Vec<Instruction> {
        build_wrapper_body_stack_memory(
            main_function_index,
            T::ACTUAL_WIDTH + U::ACTUAL_WIDTH + V::ACTUAL_WIDTH,
        )
    }
}
