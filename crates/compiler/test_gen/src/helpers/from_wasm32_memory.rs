use roc_error_macros::internal_error;
use roc_gen_wasm::wasm32_sized::Wasm32Sized;
use roc_mono::layout::Builtin;
use roc_std::{RocBox, RocDec, RocList, RocOrder, RocRefcounted, RocResult, RocStr, I128, U128};
use roc_wasm_module::round_up_to_alignment;
use std::convert::TryInto;

pub trait FromWasm32Memory: Wasm32Sized {
    fn decode(memory_bytes: &[u8], offset: u32) -> Self;
}

macro_rules! from_wasm_memory_primitive_decode {
    ($type_name:ident) => {
        fn decode(memory_bytes: &[u8], offset: u32) -> Self {
            use core::mem::MaybeUninit;

            let mut output: MaybeUninit<Self> = MaybeUninit::uninit();
            let width = std::mem::size_of::<Self>();

            let ptr = output.as_mut_ptr();
            let raw_ptr = ptr as *mut u8;
            let slice = unsafe { std::slice::from_raw_parts_mut(raw_ptr, width) };

            let index = offset as usize;
            let wasm_slice = &memory_bytes[index..][..width];

            slice.copy_from_slice(wasm_slice);

            unsafe { output.assume_init() }
        }
    };
}

macro_rules! from_wasm_memory_primitive {
    ($($type_name:ident ,)+) => {
        $(
            impl FromWasm32Memory for $type_name {
                from_wasm_memory_primitive_decode!($type_name);
            }
        )*
    }
}

from_wasm_memory_primitive!(
    u8, i8, u16, i16, u32, i32, char, u64, i64, u128, i128, f32, f64, bool,
);
from_wasm_memory_primitive!(RocDec, RocOrder, I128, U128,);

impl FromWasm32Memory for () {
    fn decode(_: &[u8], _: u32) -> Self {}
}

impl FromWasm32Memory for RocStr {
    fn decode(memory_bytes: &[u8], addr: u32) -> Self {
        let index = addr as usize;

        let mut str_bytes = [0; 12];
        str_bytes.copy_from_slice(&memory_bytes[index..][..12]);

        let str_words: &[u32; 3] = unsafe { std::mem::transmute(&str_bytes) };

        let big_elem_ptr = str_words[Builtin::WRAPPER_PTR as usize] as usize;
        // If the str is a seamless slice, it's highest bit will be set to 1.
        // We need to remove that bit or we will get an incorrect negative length.
        // Since wasm length is 32bits, and with i32::MAX (0 followed by all 1s in 32 bit).
        let big_length = str_words[Builtin::WRAPPER_LEN as usize] as usize & (i32::MAX as usize);
        let big_capacity = str_words[Builtin::WRAPPER_CAPACITY as usize] as usize;

        let last_byte = str_bytes[11];
        let is_small_str = last_byte >= 0x80;

        let slice = if is_small_str {
            let small_length = (last_byte & 0x7f) as usize;
            &str_bytes[0..small_length]
        } else {
            &memory_bytes[big_elem_ptr..][..big_length]
        };

        let mut roc_str = unsafe { RocStr::from_slice_unchecked(slice) };
        if !is_small_str {
            roc_str.reserve(big_capacity - big_length)
        }
        roc_str
    }
}

impl<T: FromWasm32Memory + Clone> FromWasm32Memory for RocList<T>
where
    T: RocRefcounted,
{
    fn decode(memory: &[u8], offset: u32) -> Self {
        let elements = <u32 as FromWasm32Memory>::decode(memory, offset + 4 * Builtin::WRAPPER_PTR);
        let length = <u32 as FromWasm32Memory>::decode(memory, offset + 4 * Builtin::WRAPPER_LEN);
        let capacity =
            <u32 as FromWasm32Memory>::decode(memory, offset + 4 * Builtin::WRAPPER_CAPACITY);

        let step = <T as Wasm32Sized>::SIZE_OF_WASM;

        let items: Vec<_> = (0..length)
            .map(|i| <T as FromWasm32Memory>::decode(memory, elements + i * step as u32))
            .collect();

        let mut list = RocList::with_capacity(capacity as usize);
        list.extend_from_slice(&items);
        list
    }
}

impl<T: FromWasm32Memory + Clone> FromWasm32Memory for RocBox<T>
where
    T: RocRefcounted,
{
    fn decode(memory: &[u8], offset: u32) -> Self {
        let ptr = <u32 as FromWasm32Memory>::decode(memory, offset + 4 * Builtin::WRAPPER_PTR);
        debug_assert_ne!(ptr, 0);

        let value = <T as FromWasm32Memory>::decode(memory, ptr);

        RocBox::new(value)
    }
}

impl<T, E> FromWasm32Memory for RocResult<T, E>
where
    T: FromWasm32Memory + Wasm32Sized,
    E: FromWasm32Memory + Wasm32Sized,
{
    fn decode(memory: &[u8], offset: u32) -> Self {
        let data_align = Ord::max(T::ALIGN_OF_WASM, E::ALIGN_OF_WASM);
        let data_width = Ord::max(T::ACTUAL_WIDTH, E::ACTUAL_WIDTH);
        let tag_offset = round_up_to_alignment!(data_width, data_align);
        let tag = <u8 as FromWasm32Memory>::decode(memory, offset + tag_offset as u32);
        if tag == 1 {
            let value = <T as FromWasm32Memory>::decode(memory, offset);
            RocResult::ok(value)
        } else {
            let payload = <E as FromWasm32Memory>::decode(memory, offset);
            RocResult::err(payload)
        }
    }
}

impl<T: FromWasm32Memory> FromWasm32Memory for &'_ T {
    fn decode(memory: &[u8], offset: u32) -> Self {
        let elements = <u32 as FromWasm32Memory>::decode(memory, offset);

        let actual = <T as FromWasm32Memory>::decode(memory, elements);

        let b = Box::new(actual);

        std::boxed::Box::<T>::leak(b)
    }
}

impl<T: FromWasm32Memory + Clone, const N: usize> FromWasm32Memory for [T; N] {
    fn decode(memory_bytes: &[u8], offset: u32) -> Self {
        let index = offset as usize;

        debug_assert!(memory_bytes.len() >= index + (N * <T as Wasm32Sized>::SIZE_OF_WASM));

        let slice_bytes: &[u8] = &memory_bytes[index..][..N];
        let slice: &[T] = unsafe { std::mem::transmute(slice_bytes) };
        let array: &[T; N] = slice.try_into().expect("incorrect length");

        array.clone()
    }
}

impl FromWasm32Memory for usize {
    fn decode(memory: &[u8], offset: u32) -> Self {
        <u32 as FromWasm32Memory>::decode(memory, offset) as usize
    }
}

impl<T: FromWasm32Memory, U: FromWasm32Memory> FromWasm32Memory for (T, U) {
    fn decode(memory: &[u8], offset: u32) -> Self {
        debug_assert!(
            T::ALIGN_OF_WASM >= U::ALIGN_OF_WASM,
            "this function does not handle alignment"
        );

        let t = <T as FromWasm32Memory>::decode(memory, offset);

        let u = <U as FromWasm32Memory>::decode(memory, offset + T::ACTUAL_WIDTH as u32);

        (t, u)
    }
}

impl<T, U, V> FromWasm32Memory for (T, U, V)
where
    T: FromWasm32Memory,
    U: FromWasm32Memory,
    V: FromWasm32Memory,
{
    fn decode(memory: &[u8], offset: u32) -> Self {
        debug_assert!(
            T::ALIGN_OF_WASM >= U::ALIGN_OF_WASM,
            "this function does not handle alignment"
        );

        debug_assert!(
            U::ALIGN_OF_WASM >= V::ALIGN_OF_WASM,
            "this function does not handle alignment"
        );

        let t = <T as FromWasm32Memory>::decode(memory, offset);

        let u = <U as FromWasm32Memory>::decode(memory, offset + T::ACTUAL_WIDTH as u32);

        let v = <V as FromWasm32Memory>::decode(
            memory,
            offset + T::ACTUAL_WIDTH as u32 + U::ACTUAL_WIDTH as u32,
        );

        (t, u, v)
    }
}

impl<T, U, V, W> FromWasm32Memory for (T, U, V, W)
where
    T: FromWasm32Memory,
    U: FromWasm32Memory,
    V: FromWasm32Memory,
    W: FromWasm32Memory,
{
    fn decode(memory: &[u8], offset: u32) -> Self {
        debug_assert!(
            T::ALIGN_OF_WASM >= U::ALIGN_OF_WASM,
            "this function does not handle alignment"
        );

        debug_assert!(
            U::ALIGN_OF_WASM >= V::ALIGN_OF_WASM,
            "this function does not handle alignment"
        );

        debug_assert!(
            V::ALIGN_OF_WASM >= W::ALIGN_OF_WASM,
            "this function does not handle alignment"
        );

        let t = <T as FromWasm32Memory>::decode(memory, offset);

        let u = <U as FromWasm32Memory>::decode(memory, offset + T::ACTUAL_WIDTH as u32);

        let v = <V as FromWasm32Memory>::decode(
            memory,
            offset + T::ACTUAL_WIDTH as u32 + U::ACTUAL_WIDTH as u32,
        );

        let w = <W as FromWasm32Memory>::decode(
            memory,
            offset + T::ACTUAL_WIDTH as u32 + U::ACTUAL_WIDTH as u32 + V::ACTUAL_WIDTH as u32,
        );

        (t, u, v, w)
    }
}

impl FromWasm32Memory for std::convert::Infallible {
    fn decode(_memory_bytes: &[u8], _offset: u32) -> Self {
        unreachable!()
    }
}
