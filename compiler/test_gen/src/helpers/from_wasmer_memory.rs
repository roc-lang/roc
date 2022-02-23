use roc_gen_wasm::wasm32_sized::Wasm32Sized;
use roc_std::{ReferenceCount, RocDec, RocList, RocOrder, RocStr};

pub trait FromWasmerMemory: Wasm32Sized {
    fn decode(memory: &wasmer::Memory, offset: u32) -> Self;
}

macro_rules! from_wasm_memory_primitive_decode {
    ($type_name:ident) => {
        fn decode(memory: &wasmer::Memory, offset: u32) -> Self {
            use core::mem::MaybeUninit;

            let mut output: MaybeUninit<Self> = MaybeUninit::uninit();
            let width = std::mem::size_of::<Self>();

            let ptr = output.as_mut_ptr();
            let raw_ptr = ptr as *mut u8;
            let slice = unsafe { std::slice::from_raw_parts_mut(raw_ptr, width) };

            let ptr: wasmer::WasmPtr<u8, wasmer::Array> = wasmer::WasmPtr::new(offset);
            let foobar = (ptr.deref(memory, 0, width as u32)).unwrap();
            let wasm_slice = unsafe { std::mem::transmute(foobar) };

            slice.copy_from_slice(wasm_slice);

            unsafe { output.assume_init() }
        }
    };
}

macro_rules! from_wasm_memory_primitive {
    ($($type_name:ident ,)+) => {
        $(
            impl FromWasmerMemory for $type_name {
                from_wasm_memory_primitive_decode!($type_name);
            }
        )*
    }
}

from_wasm_memory_primitive!(
    u8, i8, u16, i16, u32, i32, u64, i64, u128, i128, f32, f64, bool, RocDec, RocOrder,
);

impl FromWasmerMemory for () {
    fn decode(_: &wasmer::Memory, _: u32) -> Self {}
}

impl FromWasmerMemory for RocStr {
    fn decode(memory: &wasmer::Memory, offset: u32) -> Self {
        let bytes = <u64 as FromWasmerMemory>::decode(memory, offset);

        let length = (bytes >> 32) as u32;
        let elements = bytes as u32;

        if length == 0 {
            RocStr::default()
        } else if (length as i32) < 0 {
            // this is a small string
            let last_byte = bytes.to_ne_bytes()[7];
            let actual_length = (last_byte ^ 0b1000_0000) as usize;

            let slice = &bytes.to_ne_bytes()[..actual_length as usize];
            unsafe { RocStr::from_slice(slice) }
        } else {
            // this is a big string
            let ptr: wasmer::WasmPtr<u8, wasmer::Array> = wasmer::WasmPtr::new(elements);
            let foobar = (ptr.deref(memory, 0, length)).unwrap();
            let wasm_slice = unsafe { std::mem::transmute(foobar) };

            unsafe { RocStr::from_slice(wasm_slice) }
        }
    }
}

impl<T: FromWasmerMemory + Clone + ReferenceCount> FromWasmerMemory for RocList<T> {
    fn decode(memory: &wasmer::Memory, offset: u32) -> Self {
        let bytes = <u64 as FromWasmerMemory>::decode(memory, offset);

        let length = (bytes >> 32) as u32;
        let elements = bytes as u32;

        let mut items = Vec::with_capacity(length as usize);

        for i in 0..length {
            let item = <T as FromWasmerMemory>::decode(
                memory,
                elements + i * <T as Wasm32Sized>::SIZE_OF_WASM as u32,
            );
            items.push(item);
        }

        RocList::from_slice(&items)
    }
}

impl<T: FromWasmerMemory> FromWasmerMemory for &'_ T {
    fn decode(memory: &wasmer::Memory, offset: u32) -> Self {
        let elements = <u32 as FromWasmerMemory>::decode(memory, offset);

        let actual = <T as FromWasmerMemory>::decode(memory, elements);

        let b = Box::new(actual);

        std::boxed::Box::<T>::leak(b)
    }
}

impl<T: FromWasmerMemory + Clone, const N: usize> FromWasmerMemory for [T; N] {
    fn decode(memory: &wasmer::Memory, offset: u32) -> Self {
        let ptr: wasmer::WasmPtr<u8, wasmer::Array> = wasmer::WasmPtr::new(offset);
        let width = <T as Wasm32Sized>::SIZE_OF_WASM as u32 * N as u32;
        let foobar = (ptr.deref(memory, 0, width)).unwrap();
        let wasm_slice: &[T; N] = unsafe { &*(foobar as *const _ as *const [T; N]) };

        wasm_slice.clone()
    }
}

impl FromWasmerMemory for usize {
    fn decode(memory: &wasmer::Memory, offset: u32) -> Self {
        <u32 as FromWasmerMemory>::decode(memory, offset) as usize
    }
}

impl<T: FromWasmerMemory, U: FromWasmerMemory> FromWasmerMemory for (T, U) {
    fn decode(memory: &wasmer::Memory, offset: u32) -> Self {
        debug_assert!(
            T::ALIGN_OF_WASM >= U::ALIGN_OF_WASM,
            "this function does not handle alignment"
        );

        let t = <T as FromWasmerMemory>::decode(memory, offset);

        let u = <U as FromWasmerMemory>::decode(memory, offset + T::ACTUAL_WIDTH as u32);

        (t, u)
    }
}

impl<T: FromWasmerMemory, U: FromWasmerMemory, V: FromWasmerMemory> FromWasmerMemory for (T, U, V) {
    fn decode(memory: &wasmer::Memory, offset: u32) -> Self {
        debug_assert!(
            T::ALIGN_OF_WASM >= U::ALIGN_OF_WASM,
            "this function does not handle alignment"
        );

        debug_assert!(
            U::ALIGN_OF_WASM >= V::ALIGN_OF_WASM,
            "this function does not handle alignment"
        );

        let t = <T as FromWasmerMemory>::decode(memory, offset);

        let u = <U as FromWasmerMemory>::decode(memory, offset + T::ACTUAL_WIDTH as u32);

        let v = <V as FromWasmerMemory>::decode(
            memory,
            offset + T::ACTUAL_WIDTH as u32 + U::ACTUAL_WIDTH as u32,
        );

        (t, u, v)
    }
}
