use parity_wasm::builder;
use parity_wasm::elements::Internal;

use roc_gen_wasm::code_builder::{Align, CodeBuilder, ValueType};
use roc_gen_wasm::from_wasm32_memory::FromWasm32Memory;
use roc_gen_wasm::{encode_u32_padded, LocalId};
use roc_std::{RocDec, RocList, RocOrder, RocStr};

pub trait Wasm32TestResult {
    fn insert_test_wrapper<'a>(
        arena: &'a bumpalo::Bump,
        module_builder: &mut builder::ModuleBuilder,
        code_section_bytes: &mut std::vec::Vec<u8>,
        wrapper_name: &str,
        main_function_index: u32,
    ) {
        let signature = builder::signature()
            .with_result(parity_wasm::elements::ValueType::I32)
            .build_sig();

        // parity-wasm FunctionDefinition with no instructions
        let empty_fn_def = builder::function().with_signature(signature).build();
        let location = module_builder.push_function(empty_fn_def);
        let export = builder::export()
            .field(wrapper_name)
            .with_internal(Internal::Function(location.body))
            .build();
        module_builder.push_export(export);

        let mut code_builder = CodeBuilder::new(arena);
        Self::build_wrapper_body(&mut code_builder, main_function_index);

        code_builder.serialize(code_section_bytes).unwrap();
        finalize_code_section(&mut code_section_bytes);
    }

    fn build_wrapper_body(code_builder: &mut CodeBuilder, main_function_index: u32);
}

macro_rules! build_wrapper_body_primitive {
    ($store_instruction: ident, $align: expr) => {
        fn build_wrapper_body(code_builder: &mut CodeBuilder, main_function_index: u32) {
            let frame_pointer_id = LocalId(0);
            let frame_pointer = Some(frame_pointer_id);
            let local_types = &[ValueType::I32];
            let frame_size = 8;

            code_builder.get_local(frame_pointer_id);
            code_builder.call(main_function_index, 0, true);
            code_builder.$store_instruction($align, 0);
            code_builder.get_local(frame_pointer_id);

            code_builder.finalize(local_types, frame_size, frame_pointer);
        }
    };
}

macro_rules! wasm_test_result_primitive {
    ($type_name: ident, $store_instruction: ident, $align: expr) => {
        impl Wasm32TestResult for $type_name {
            build_wrapper_body_primitive!($store_instruction, $align);
        }
    };
}

fn build_wrapper_body_stack_memory(
    code_builder: &mut CodeBuilder,
    main_function_index: u32,
    size: usize,
) {
    let local_id = LocalId(0);
    let local_types = &[ValueType::I32];
    let frame_pointer = Some(local_id);

    code_builder.get_local(local_id);
    code_builder.call(main_function_index, 0, true);
    code_builder.get_local(local_id);
    code_builder.finalize(local_types, size as i32, frame_pointer);
}

macro_rules! wasm_test_result_stack_memory {
    ($type_name: ident) => {
        impl Wasm32TestResult for $type_name {
            fn build_wrapper_body(code_builder: &mut CodeBuilder, main_function_index: u32) {
                build_wrapper_body_stack_memory(
                    code_builder,
                    main_function_index,
                    $type_name::ACTUAL_WIDTH,
                )
            }
        }
    };
}

wasm_test_result_primitive!(bool, i32_store8, Align::Bytes1);
wasm_test_result_primitive!(RocOrder, i32_store8, Align::Bytes1);

wasm_test_result_primitive!(u8, i32_store8, Align::Bytes1);
wasm_test_result_primitive!(i8, i32_store8, Align::Bytes1);
wasm_test_result_primitive!(u16, i32_store16, Align::Bytes2);
wasm_test_result_primitive!(i16, i32_store16, Align::Bytes2);
wasm_test_result_primitive!(u32, i32_store, Align::Bytes4);
wasm_test_result_primitive!(i32, i32_store, Align::Bytes4);
wasm_test_result_primitive!(u64, i64_store, Align::Bytes8);
wasm_test_result_primitive!(i64, i64_store, Align::Bytes8);

wasm_test_result_primitive!(f32, f32_store, Align::Bytes8);
wasm_test_result_primitive!(f64, f64_store, Align::Bytes8);

wasm_test_result_stack_memory!(u128);
wasm_test_result_stack_memory!(i128);
wasm_test_result_stack_memory!(RocDec);
wasm_test_result_stack_memory!(RocStr);

impl<T: Wasm32TestResult> Wasm32TestResult for RocList<T> {
    fn build_wrapper_body(code_builder: &mut CodeBuilder, main_function_index: u32) {
        build_wrapper_body_stack_memory(code_builder, main_function_index, 12)
    }
}

impl<T: Wasm32TestResult> Wasm32TestResult for &'_ T {
    build_wrapper_body_primitive!(i32_store, Align::Bytes4);
}

impl<T, const N: usize> Wasm32TestResult for [T; N]
where
    T: Wasm32TestResult + FromWasm32Memory,
{
    fn build_wrapper_body(code_builder: &mut CodeBuilder, main_function_index: u32) {
        build_wrapper_body_stack_memory(code_builder, main_function_index, N * T::ACTUAL_WIDTH)
    }
}

impl<T, U> Wasm32TestResult for (T, U)
where
    T: Wasm32TestResult + FromWasm32Memory,
    U: Wasm32TestResult + FromWasm32Memory,
{
    fn build_wrapper_body(code_builder: &mut CodeBuilder, main_function_index: u32) {
        build_wrapper_body_stack_memory(
            code_builder,
            main_function_index,
            T::ACTUAL_WIDTH + U::ACTUAL_WIDTH,
        )
    }
}

impl<T, U, V> Wasm32TestResult for (T, U, V)
where
    T: Wasm32TestResult + FromWasm32Memory,
    U: Wasm32TestResult + FromWasm32Memory,
    V: Wasm32TestResult + FromWasm32Memory,
{
    fn build_wrapper_body(code_builder: &mut CodeBuilder, main_function_index: u32) {
        build_wrapper_body_stack_memory(
            code_builder,
            main_function_index,
            T::ACTUAL_WIDTH + U::ACTUAL_WIDTH + V::ACTUAL_WIDTH,
        )
    }
}

impl<T, U, V, W> Wasm32TestResult for (T, U, V, W)
where
    T: Wasm32TestResult + FromWasm32Memory,
    U: Wasm32TestResult + FromWasm32Memory,
    V: Wasm32TestResult + FromWasm32Memory,
    W: Wasm32TestResult + FromWasm32Memory,
{
    fn build_wrapper_body(code_builder: &mut CodeBuilder, main_function_index: u32) {
        build_wrapper_body_stack_memory(
            code_builder,
            main_function_index,
            T::ACTUAL_WIDTH + U::ACTUAL_WIDTH + V::ACTUAL_WIDTH + W::ACTUAL_WIDTH,
        )
    }
}

impl<T, U, V, W, X> Wasm32TestResult for (T, U, V, W, X)
where
    T: Wasm32TestResult + FromWasm32Memory,
    U: Wasm32TestResult + FromWasm32Memory,
    V: Wasm32TestResult + FromWasm32Memory,
    W: Wasm32TestResult + FromWasm32Memory,
    X: Wasm32TestResult + FromWasm32Memory,
{
    fn build_wrapper_body(code_builder: &mut CodeBuilder, main_function_index: u32) {
        build_wrapper_body_stack_memory(
            code_builder,
            main_function_index,
            T::ACTUAL_WIDTH + U::ACTUAL_WIDTH + V::ACTUAL_WIDTH + W::ACTUAL_WIDTH + X::ACTUAL_WIDTH,
        )
    }
}

impl<T, U, V, W, X, Y> Wasm32TestResult for (T, U, V, W, X, Y)
where
    T: Wasm32TestResult + FromWasm32Memory,
    U: Wasm32TestResult + FromWasm32Memory,
    V: Wasm32TestResult + FromWasm32Memory,
    W: Wasm32TestResult + FromWasm32Memory,
    X: Wasm32TestResult + FromWasm32Memory,
    Y: Wasm32TestResult + FromWasm32Memory,
{
    fn build_wrapper_body(code_builder: &mut CodeBuilder, main_function_index: u32) {
        build_wrapper_body_stack_memory(
            code_builder,
            main_function_index,
            T::ACTUAL_WIDTH
                + U::ACTUAL_WIDTH
                + V::ACTUAL_WIDTH
                + W::ACTUAL_WIDTH
                + X::ACTUAL_WIDTH
                + Y::ACTUAL_WIDTH,
        )
    }
}

impl<T, U, V, W, X, Y, Z> Wasm32TestResult for (T, U, V, W, X, Y, Z)
where
    T: Wasm32TestResult + FromWasm32Memory,
    U: Wasm32TestResult + FromWasm32Memory,
    V: Wasm32TestResult + FromWasm32Memory,
    W: Wasm32TestResult + FromWasm32Memory,
    X: Wasm32TestResult + FromWasm32Memory,
    Y: Wasm32TestResult + FromWasm32Memory,
    Z: Wasm32TestResult + FromWasm32Memory,
{
    fn build_wrapper_body(code_builder: &mut CodeBuilder, main_function_index: u32) {
        build_wrapper_body_stack_memory(
            code_builder,
            main_function_index,
            T::ACTUAL_WIDTH
                + U::ACTUAL_WIDTH
                + V::ACTUAL_WIDTH
                + W::ACTUAL_WIDTH
                + X::ACTUAL_WIDTH
                + Y::ACTUAL_WIDTH
                + Z::ACTUAL_WIDTH,
        )
    }
}

impl<T, U, V, W, X, Y, Z, A> Wasm32TestResult for (T, U, V, W, X, Y, Z, A)
where
    T: Wasm32TestResult + FromWasm32Memory,
    U: Wasm32TestResult + FromWasm32Memory,
    V: Wasm32TestResult + FromWasm32Memory,
    W: Wasm32TestResult + FromWasm32Memory,
    X: Wasm32TestResult + FromWasm32Memory,
    Y: Wasm32TestResult + FromWasm32Memory,
    Z: Wasm32TestResult + FromWasm32Memory,
    A: Wasm32TestResult + FromWasm32Memory,
{
    fn build_wrapper_body(code_builder: &mut CodeBuilder, main_function_index: u32) {
        build_wrapper_body_stack_memory(
            code_builder,
            main_function_index,
            T::ACTUAL_WIDTH
                + U::ACTUAL_WIDTH
                + V::ACTUAL_WIDTH
                + W::ACTUAL_WIDTH
                + X::ACTUAL_WIDTH
                + Y::ACTUAL_WIDTH
                + Z::ACTUAL_WIDTH
                + A::ACTUAL_WIDTH,
        )
    }
}
