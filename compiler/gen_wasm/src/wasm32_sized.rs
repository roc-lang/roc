use roc_std::{ReferenceCount, RocDec, RocList, RocOrder, RocStr};

pub trait Wasm32Sized: Sized {
    const SIZE_OF_WASM: usize;
    const ALIGN_OF_WASM: usize;
    const ACTUAL_WIDTH: usize = if (Self::SIZE_OF_WASM % Self::ALIGN_OF_WASM) == 0 {
        Self::SIZE_OF_WASM
    } else {
        Self::SIZE_OF_WASM + (Self::ALIGN_OF_WASM - (Self::SIZE_OF_WASM % Self::ALIGN_OF_WASM))
    };
}

macro_rules! wasm32_sized_primitive {
    ($($type_name:ident ,)+) => {
        $(
            impl Wasm32Sized for $type_name {
                const SIZE_OF_WASM: usize = core::mem::size_of::<$type_name>();
                const ALIGN_OF_WASM: usize = core::mem::align_of::<$type_name>();
            }
        )*
    }
}

wasm32_sized_primitive!(
    u8, i8, u16, i16, u32, i32, u64, i64, u128, i128, f32, f64, bool, RocDec, RocOrder,
);

impl Wasm32Sized for () {
    const SIZE_OF_WASM: usize = 0;
    const ALIGN_OF_WASM: usize = 0;
}

impl Wasm32Sized for RocStr {
    const SIZE_OF_WASM: usize = 8;
    const ALIGN_OF_WASM: usize = 4;
}

impl<T: Wasm32Sized + ReferenceCount> Wasm32Sized for RocList<T> {
    const SIZE_OF_WASM: usize = 8;
    const ALIGN_OF_WASM: usize = 4;
}

impl<T: Wasm32Sized> Wasm32Sized for &'_ T {
    const SIZE_OF_WASM: usize = 4;
    const ALIGN_OF_WASM: usize = 4;
}

impl<T: Wasm32Sized, const N: usize> Wasm32Sized for [T; N] {
    const SIZE_OF_WASM: usize = N * T::SIZE_OF_WASM;
    const ALIGN_OF_WASM: usize = T::ALIGN_OF_WASM;
}

impl Wasm32Sized for usize {
    const SIZE_OF_WASM: usize = 4;
    const ALIGN_OF_WASM: usize = 4;
}

impl<T: Wasm32Sized, U: Wasm32Sized> Wasm32Sized for (T, U) {
    const SIZE_OF_WASM: usize = T::SIZE_OF_WASM + U::SIZE_OF_WASM;
    const ALIGN_OF_WASM: usize = max2(T::SIZE_OF_WASM, U::SIZE_OF_WASM);
}

impl<T: Wasm32Sized, U: Wasm32Sized, V: Wasm32Sized> Wasm32Sized for (T, U, V) {
    const SIZE_OF_WASM: usize = T::SIZE_OF_WASM + U::SIZE_OF_WASM + V::SIZE_OF_WASM;
    const ALIGN_OF_WASM: usize = max3(T::SIZE_OF_WASM, U::SIZE_OF_WASM, V::SIZE_OF_WASM);
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
