use bumpalo::collections::Vec;

use crate::helpers::from_wasm32_memory::FromWasm32Memory;
use roc_gen_wasm::wasm_module::{
    linking::SymInfo, linking::WasmObjectSymbol, Align, CodeBuilder, Export, ExportType, LocalId,
    Signature, ValueType, WasmModule,
};
use roc_std::{RocDec, RocList, RocOrder, RocStr};

pub trait Wasm32TestResult {
    fn insert_test_wrapper<'a>(
        arena: &'a bumpalo::Bump,
        module: &mut WasmModule<'a>,
        wrapper_name: &str,
        main_function_index: u32,
    ) {
        let index = module.import.function_count
            + module.code.preloaded_count
            + module.code.code_builders.len() as u32;

        module.add_function_signature(Signature {
            param_types: Vec::with_capacity_in(0, arena),
            ret_type: Some(ValueType::I32),
        });

        module.export.append(Export {
            name: arena.alloc_slice_copy(wrapper_name.as_bytes()),
            ty: ExportType::Func,
            index,
        });

        let linker_symbol = SymInfo::Function(WasmObjectSymbol::Defined {
            flags: 0,
            index,
            name: wrapper_name.to_string(),
        });
        module.linking.symbol_table.push(linker_symbol);

        let mut code_builder = CodeBuilder::new(arena);
        Self::build_wrapper_body(&mut code_builder, main_function_index);
        module.code.code_builders.push(code_builder);
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
            // Main's symbol index is the same as its function index, since the first symbols we created were for procs
            let main_symbol_index = main_function_index;

            code_builder.get_local(frame_pointer_id);
            code_builder.call(main_function_index, main_symbol_index, 0, true);
            code_builder.$store_instruction($align, 0);
            code_builder.get_local(frame_pointer_id);

            code_builder.build_fn_header_and_footer(local_types, frame_size, frame_pointer);
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
    // Main's symbol index is the same as its function index, since the first symbols we created were for procs
    let main_symbol_index = main_function_index;

    code_builder.get_local(local_id);
    code_builder.call(main_function_index, main_symbol_index, 0, true);
    code_builder.get_local(local_id);
    code_builder.build_fn_header_and_footer(local_types, size as i32, frame_pointer);
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
wasm_test_result_primitive!(usize, i32_store, Align::Bytes4);

wasm_test_result_primitive!(f32, f32_store, Align::Bytes4);
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

impl Wasm32TestResult for () {
    fn build_wrapper_body(code_builder: &mut CodeBuilder, main_function_index: u32) {
        // Main's symbol index is the same as its function index, since the first symbols we created were for procs
        let main_symbol_index = main_function_index;
        code_builder.call(main_function_index, main_symbol_index, 0, false);
        code_builder.get_global(0);
        code_builder.build_fn_header_and_footer(&[], 0, None);
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
