use std::mem::size_of;

pub trait FromMemory<M: AppMemory> {
    fn from_memory(memory: &M, address: usize) -> Self;
}

// TODO: check if AppMemory trait is really needed!
// I wrote it to try to help with Rust type inference but it didn't really help
// Rust is complaining in eval.rs that it can't find instances like `u8: FromMemory<M>`
// I thought that the AppMemory trait might give it some more hints, but it's still not working.
pub trait AppMemory {}
impl AppMemory for AppMemoryInternal {}
impl<'a> AppMemory for AppMemoryExternal<'a> {}

/// A block of app memory in the same address space as the compiler
pub struct AppMemoryInternal;

/// A block of app memory in a separate address space from the compiler
/// (e.g. compiler and app are in separate Wasm modules)
pub struct AppMemoryExternal<'a> {
    bytes: &'a [u8],
}

macro_rules! impl_number_type {
    ($t: ty) => {
        impl FromMemory<AppMemoryInternal> for $t {
            fn from_memory(_: &AppMemoryInternal, address: usize) -> Self {
                let ptr = address as *const _;
                unsafe { *ptr }
            }
        }

        impl FromMemory<AppMemoryExternal<'_>> for $t {
            fn from_memory(memory: &AppMemoryExternal, address: usize) -> Self {
                const N: usize = size_of::<$t>();
                let mut array = [0; N];
                array.copy_from_slice(&memory.bytes[address..][..N]);
                Self::from_le_bytes(array)
            }
        }
    };
}

impl FromMemory<AppMemoryInternal> for u8 {
    fn from_memory(_: &AppMemoryInternal, address: usize) -> Self {
        let ptr = address as *const _;
        unsafe { *ptr }
    }
}

impl FromMemory<AppMemoryExternal<'_>> for u8 {
    fn from_memory(memory: &AppMemoryExternal, address: usize) -> Self {
        const N: usize = size_of::<u8>();
        let mut array = [0; N];
        array.copy_from_slice(&memory.bytes[address..][..N]);
        Self::from_le_bytes(array)
    }
}

// impl_number_type!(u8);
impl_number_type!(u16);
impl_number_type!(u32);
impl_number_type!(u64);
impl_number_type!(u128);
impl_number_type!(usize);

impl_number_type!(i8);
impl_number_type!(i16);
impl_number_type!(i32);
impl_number_type!(i64);
impl_number_type!(i128);
impl_number_type!(isize);

impl_number_type!(f32);
impl_number_type!(f64);

impl FromMemory<AppMemoryInternal> for bool {
    fn from_memory(_: &AppMemoryInternal, address: usize) -> Self {
        let ptr = address as *const _;
        unsafe { *ptr }
    }
}

impl FromMemory<AppMemoryExternal<'_>> for bool {
    fn from_memory(memory: &AppMemoryExternal, address: usize) -> Self {
        memory.bytes[address] != 0
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn internal_u8() {
        let value: u8 = 123;
        let ptr = &value as *const u8;
        let addr = ptr as usize;
        let memory = AppMemoryInternal;
        let recovered: u8 = u8::from_memory(&memory, addr);
        assert_eq!(value, recovered);
    }

    #[test]
    fn external_u8() {
        let value: u8 = 123;
        let memory = AppMemoryExternal {
            bytes: &[0, 0, value, 0, 0],
        };
        let addr = 2;
        let recovered: u8 = u8::from_memory(&memory, addr);
        assert_eq!(value, recovered);
    }

    #[test]
    fn internal_i64() {
        let value: i64 = -123 << 33;
        let ptr = &value as *const i64;
        let addr = ptr as usize;
        let memory = AppMemoryInternal;
        let recovered: i64 = i64::from_memory(&memory, addr);
        assert_eq!(value, recovered);
    }

    #[test]
    fn external_i64() {
        let value: i64 = -1 << 33;
        let memory = AppMemoryExternal {
            bytes: &[
                0, 0, //
                0, 0, 0, 0, 0xfe, 0xff, 0xff, 0xff, //
                0, 0,
            ],
        };
        let addr = 2;
        let recovered: i64 = i64::from_memory(&memory, addr);
        assert_eq!(value, recovered);
    }
}
