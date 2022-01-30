use std::mem::size_of;

pub trait AppMemory {
    fn deref_bool(&self, addr: usize) -> bool;

    fn deref_u8(&self, addr: usize) -> u8;
    fn deref_u16(&self, addr: usize) -> u16;
    fn deref_u32(&self, addr: usize) -> u32;
    fn deref_u64(&self, addr: usize) -> u64;
    fn deref_u128(&self, addr: usize) -> u128;
    fn deref_usize(&self, addr: usize) -> usize;

    fn deref_i8(&self, addr: usize) -> i8;
    fn deref_i16(&self, addr: usize) -> i16;
    fn deref_i32(&self, addr: usize) -> i32;
    fn deref_i64(&self, addr: usize) -> i64;
    fn deref_i128(&self, addr: usize) -> i128;
    fn deref_isize(&self, addr: usize) -> isize;

    fn deref_f32(&self, addr: usize) -> f32;
    fn deref_f64(&self, addr: usize) -> f64;

    fn deref_str(&self, addr: usize) -> &str;
}

/// A block of app memory in the same address space as the compiler
pub struct AppMemoryInternal;

macro_rules! internal_number_type {
    ($name: ident, $t: ty) => {
        fn $name(&self, addr: usize) -> $t {
            let ptr = addr as *const _;
            unsafe { *ptr }
        }
    };
}

impl AppMemory for AppMemoryInternal {
    internal_number_type!(deref_bool, bool);

    internal_number_type!(deref_u8, u8);
    internal_number_type!(deref_u16, u16);
    internal_number_type!(deref_u32, u32);
    internal_number_type!(deref_u64, u64);
    internal_number_type!(deref_u128, u128);
    internal_number_type!(deref_usize, usize);

    internal_number_type!(deref_i8, i8);
    internal_number_type!(deref_i16, i16);
    internal_number_type!(deref_i32, i32);
    internal_number_type!(deref_i64, i64);
    internal_number_type!(deref_i128, i128);
    internal_number_type!(deref_isize, isize);

    internal_number_type!(deref_f32, f32);
    internal_number_type!(deref_f64, f64);

    fn deref_str(&self, addr: usize) -> &str {
        unsafe { *(addr as *const &'static str) }
    }
}

/// A block of app memory copied from an exteral address space outside the compiler
/// (e.g. compiler and app are in separate Wasm modules)
pub struct AppMemoryExternal<'a> {
    bytes: &'a [u8],
}

macro_rules! external_number_type {
    ($name: ident, $t: ty) => {
        fn $name(&self, address: usize) -> $t {
            const N: usize = size_of::<$t>();
            let mut array = [0; N];
            array.copy_from_slice(&self.bytes[address..][..N]);
            <$t>::from_le_bytes(array)
        }
    };
}

impl<'a> AppMemory for AppMemoryExternal<'a> {
    fn deref_bool(&self, address: usize) -> bool {
        self.bytes[address] != 0
    }

    external_number_type!(deref_u8, u8);
    external_number_type!(deref_u16, u16);
    external_number_type!(deref_u32, u32);
    external_number_type!(deref_u64, u64);
    external_number_type!(deref_u128, u128);
    external_number_type!(deref_usize, usize);

    external_number_type!(deref_i8, i8);
    external_number_type!(deref_i16, i16);
    external_number_type!(deref_i32, i32);
    external_number_type!(deref_i64, i64);
    external_number_type!(deref_i128, i128);
    external_number_type!(deref_isize, isize);

    external_number_type!(deref_f32, f32);
    external_number_type!(deref_f64, f64);

    fn deref_str(&self, addr: usize) -> &str {
        let elems_addr = self.deref_usize(addr);
        let len = self.deref_usize(addr + size_of::<usize>());
        let bytes = &self.bytes[elems_addr..][..len];
        std::str::from_utf8(bytes).unwrap()
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
        let recovered: u8 = memory.deref_u8(addr);
        assert_eq!(value, recovered);
    }

    #[test]
    fn external_u8() {
        let value: u8 = 123;
        let memory = AppMemoryExternal {
            bytes: &[0, 0, value, 0, 0],
        };
        let addr = 2;
        let recovered: u8 = memory.deref_u8(addr);
        assert_eq!(value, recovered);
    }

    #[test]
    fn internal_i64() {
        let value: i64 = -123 << 33;
        let ptr = &value as *const i64;
        let addr = ptr as usize;
        let memory = AppMemoryInternal;
        let recovered: i64 = memory.deref_i64(addr);
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
        let recovered: i64 = memory.deref_i64(addr);
        assert_eq!(value, recovered);
    }
}
