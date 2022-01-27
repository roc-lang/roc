use std::mem::size_of;

pub trait FromMemory<M: AppMemory> {
    fn from_memory(memory: &M, address: usize) -> Self;
}

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

impl FromMemory<AppMemoryInternal> for &str {
    fn from_memory(_: &AppMemoryInternal, address: usize) -> Self {
        let ptr = address as *const _;
        unsafe { *ptr }
    }
}

impl<'a> FromMemory<AppMemoryExternal<'a>> for &'a str {
    fn from_memory(memory: &AppMemoryExternal<'a>, address: usize) -> Self {
        let len = usize::from_memory(memory, address + std::mem::size_of::<usize>());
        let content_addr = usize::from_memory(memory, address);
        let content_bytes: &'a [u8] = &memory.bytes[content_addr..][..len];
        std::str::from_utf8(content_bytes).unwrap()
    }
}
