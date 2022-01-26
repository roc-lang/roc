use std::mem::size_of;

pub trait FromMemory<M> {
    fn from_memory(memory: M, address: usize) -> Self;
}

/// A block of app memory in the same address space as the compiler
pub struct AppMemoryInternal;

/// A block of app memory in a separate address space from the compiler
/// (e.g. compiler and app are in separate Wasm modules)
pub struct AppMemoryExternal {
    bytes: Vec<u8>,
}

macro_rules! impl_primitive {
    ($t: ty) => {
        impl FromMemory<AppMemoryInternal> for $t {
            fn from_memory(_: AppMemoryInternal, address: usize) -> Self {
                let ptr = address as *const _;
                unsafe { *ptr }
            }
        }

        impl FromMemory<AppMemoryExternal> for $t {
            fn from_memory(memory: AppMemoryExternal, address: usize) -> Self {
                const N: usize = size_of::<$t>();
                let mut array = [0; N];
                array.copy_from_slice(&memory.bytes[address..][..N]);
                Self::from_le_bytes(array)
            }
        }
    };
}

impl_primitive!(u8);
impl_primitive!(u16);
impl_primitive!(u32);
impl_primitive!(u64);
impl_primitive!(u128);
impl_primitive!(usize);

impl_primitive!(i8);
impl_primitive!(i16);
impl_primitive!(i32);
impl_primitive!(i64);
impl_primitive!(i128);
impl_primitive!(isize);

impl_primitive!(f32);
impl_primitive!(f64);
