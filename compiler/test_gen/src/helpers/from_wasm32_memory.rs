use roc_std::{RocDec, RocList, RocOrder, RocStr};

pub trait FromWasm32Memory: Sized {
    const SIZE_OF_WASM: usize;
    const ALIGN_OF_WASM: usize;
    const ACTUAL_WIDTH: usize = if (Self::SIZE_OF_WASM % Self::ALIGN_OF_WASM) == 0 {
        Self::SIZE_OF_WASM
    } else {
        Self::SIZE_OF_WASM + (Self::ALIGN_OF_WASM - (Self::SIZE_OF_WASM % Self::ALIGN_OF_WASM))
    };

    fn decode(memory: &wasmer::Memory, offset: u32) -> Self;
}

macro_rules! from_wasm_memory_primitive_decode {
    ($type_name:ident) => {
        const SIZE_OF_WASM: usize = core::mem::size_of::<$type_name>();
        const ALIGN_OF_WASM: usize = core::mem::align_of::<$type_name>();

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
            impl FromWasm32Memory for $type_name {
                from_wasm_memory_primitive_decode!($type_name);
            }
        )*
    }
}

from_wasm_memory_primitive!(
    u8, i8, u16, i16, u32, i32, u64, i64, u128, i128, f32, f64, bool, RocDec, RocOrder,
);

impl FromWasm32Memory for () {
    const SIZE_OF_WASM: usize = 0;
    const ALIGN_OF_WASM: usize = 0;

    fn decode(_: &wasmer::Memory, _: u32) -> Self {}
}

impl FromWasm32Memory for RocStr {
    const SIZE_OF_WASM: usize = 8;
    const ALIGN_OF_WASM: usize = 4;

    fn decode(memory: &wasmer::Memory, offset: u32) -> Self {
        let bytes = <u64 as FromWasm32Memory>::decode(memory, offset);

        let length = (bytes >> 32) as u32;
        let elements = bytes as u32;

        if length == 0 {
            RocStr::default()
        } else if (length as i32) < 0 {
            // this is a small string
            let last_byte = bytes.to_ne_bytes()[7];
            let actual_length = (last_byte ^ 0b1000_0000) as usize;

            let slice = &bytes.to_ne_bytes()[..actual_length as usize];
            RocStr::from_slice(slice)
        } else {
            // this is a big string
            let ptr: wasmer::WasmPtr<u8, wasmer::Array> = wasmer::WasmPtr::new(elements);
            let foobar = (ptr.deref(memory, 0, length)).unwrap();
            let wasm_slice = unsafe { std::mem::transmute(foobar) };

            RocStr::from_slice(wasm_slice)
        }
    }
}

impl<T: FromWasm32Memory + Clone> FromWasm32Memory for RocList<T> {
    const SIZE_OF_WASM: usize = 8;
    const ALIGN_OF_WASM: usize = 4;

    fn decode(memory: &wasmer::Memory, offset: u32) -> Self {
        let bytes = <u64 as FromWasm32Memory>::decode(memory, offset);

        let length = (bytes >> 32) as u32;
        let elements = bytes as u32;

        let mut items = Vec::with_capacity(length as usize);

        for i in 0..length {
            let item = <T as FromWasm32Memory>::decode(
                memory,
                elements + i * <T as FromWasm32Memory>::SIZE_OF_WASM as u32,
            );
            items.push(item);
        }

        RocList::from_slice(&items)
    }
}

impl<T: FromWasm32Memory> FromWasm32Memory for &'_ T {
    const SIZE_OF_WASM: usize = 4;
    const ALIGN_OF_WASM: usize = 4;

    fn decode(memory: &wasmer::Memory, offset: u32) -> Self {
        let elements = <u32 as FromWasm32Memory>::decode(memory, offset);

        let actual = <T as FromWasm32Memory>::decode(memory, elements);

        let b = Box::new(actual);

        std::boxed::Box::<T>::leak(b)
    }
}

impl<T: FromWasm32Memory + Clone, const N: usize> FromWasm32Memory for [T; N] {
    const SIZE_OF_WASM: usize = N * T::SIZE_OF_WASM;
    const ALIGN_OF_WASM: usize = T::ALIGN_OF_WASM;

    fn decode(memory: &wasmer::Memory, offset: u32) -> Self {
        let ptr: wasmer::WasmPtr<u8, wasmer::Array> = wasmer::WasmPtr::new(offset);
        let width = <T as FromWasm32Memory>::SIZE_OF_WASM as u32 * N as u32;
        let foobar = (ptr.deref(memory, 0, width)).unwrap();
        let wasm_slice: &[T; N] = unsafe { &*(foobar as *const _ as *const [T; N]) };

        wasm_slice.clone()
    }
}

impl FromWasm32Memory for usize {
    const SIZE_OF_WASM: usize = 4;
    const ALIGN_OF_WASM: usize = 4;

    fn decode(memory: &wasmer::Memory, offset: u32) -> Self {
        <u32 as FromWasm32Memory>::decode(memory, offset) as usize
    }
}

impl<T: FromWasm32Memory, U: FromWasm32Memory> FromWasm32Memory for (T, U) {
    const SIZE_OF_WASM: usize = T::SIZE_OF_WASM + U::SIZE_OF_WASM;
    const ALIGN_OF_WASM: usize = max2(T::SIZE_OF_WASM, U::SIZE_OF_WASM);

    fn decode(memory: &wasmer::Memory, offset: u32) -> Self {
        debug_assert!(
            T::ALIGN_OF_WASM >= U::ALIGN_OF_WASM,
            "this function does not handle alignment"
        );

        let t = <T as FromWasm32Memory>::decode(memory, offset);

        let u = <U as FromWasm32Memory>::decode(memory, offset + T::ACTUAL_WIDTH as u32);

        (t, u)
    }
}

const fn max2(a: usize, b: usize) -> usize {
    if a > b {
        a
    } else {
        b
    }
}

const fn max3(a: usize, b: usize, c: usize) -> usize {
    max2(max2(a, b), c)
}

impl<T: FromWasm32Memory, U: FromWasm32Memory, V: FromWasm32Memory> FromWasm32Memory for (T, U, V) {
    const SIZE_OF_WASM: usize = T::SIZE_OF_WASM + U::SIZE_OF_WASM + V::SIZE_OF_WASM;
    const ALIGN_OF_WASM: usize = max3(T::SIZE_OF_WASM, U::SIZE_OF_WASM, V::SIZE_OF_WASM);

    fn decode(memory: &wasmer::Memory, offset: u32) -> Self {
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
