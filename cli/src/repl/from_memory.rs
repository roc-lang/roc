use std::mem::size_of;

pub trait AppMemory<T> {
    fn from_memory(&self, addr: usize) -> T;
}

/// A block of app memory in the same address space as the compiler
pub struct AppMemoryInternal;

/// A block of app memory in a separate address space from the compiler
/// (e.g. compiler and app are in separate Wasm modules)
pub struct AppMemoryExternal<'a> {
    bytes: &'a [u8],
}

macro_rules! impl_number_type {
    ($t: ty) => {
        impl AppMemory<$t> for AppMemoryInternal {
            fn from_memory(&self, addr: usize) -> $t {
                let ptr = addr as *const _;
                unsafe { *ptr }
            }
        }

        impl AppMemory<$t> for AppMemoryExternal<'_> {
            fn from_memory(&self, address: usize) -> $t {
                const N: usize = size_of::<$t>();
                let mut array = [0; N];
                array.copy_from_slice(&self.bytes[address..][..N]);
                <$t>::from_le_bytes(array)
            }
        }
    };
}

impl_number_type!(u8);
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

impl AppMemory<bool> for AppMemoryInternal {
    fn from_memory(&self, addr: usize) -> bool {
        let ptr = addr as *const _;
        unsafe { *ptr }
    }
}

impl AppMemory<bool> for AppMemoryExternal<'_> {
    fn from_memory(&self, address: usize) -> bool {
        self.bytes[address] != 0
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
        let recovered: u8 = memory.from_memory(addr);
        assert_eq!(value, recovered);
    }

    #[test]
    fn external_u8() {
        let value: u8 = 123;
        let memory = AppMemoryExternal {
            bytes: &[0, 0, value, 0, 0],
        };
        let addr = 2;
        let recovered: u8 = memory.from_memory(addr);
        assert_eq!(value, recovered);
    }

    #[test]
    fn internal_i64() {
        let value: i64 = -123 << 33;
        let ptr = &value as *const i64;
        let addr = ptr as usize;
        let memory = AppMemoryInternal;
        let recovered: i64 = memory.from_memory(addr);
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
        let recovered: i64 = memory.from_memory(addr);
        assert_eq!(value, recovered);
    }
}
