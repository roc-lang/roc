// Adapted from https://github.com/TheDan64/scoped_alloca
// by Daniel Kolsoi, licensed under the Apache License 2.0
// Thank you, Dan!

use libc::{c_void, size_t};

#[link(name = "alloca")]
extern "C" {
    #[no_mangle]
    fn c_alloca(_: size_t) -> *mut c_void;
}

/// This calls C's `alloca` function to allocate some bytes on the stack,
/// then provides those bytes to the given callback function.
///
/// Note that this function is `#[inline(always)]`, which means it will allocate
/// the bytes on the stack of whoever calls this function.
///
/// Naturally, if you give this a large number of bytes, it may cause
/// stack overflows, so be careful!
///
/// Due to how Rust FFI works with inlining, in debug builds this actually
/// allocates on the heap (using `malloc`) and then deallocates the memory
/// (using `free`) before the callback returns. In debug builds, this can lead
/// to memory leaks if the callback panics - but release builds should be fine,
/// because they only ever allocate on the stack.
#[inline(always)]
pub unsafe fn with_stack_bytes<F, R>(bytes: usize, callback: F) -> R
where
    F: FnOnce(*mut c_void) -> R,
{
    let ptr = malloc_or_alloca(bytes);
    let answer = callback(ptr);

    free_or_noop(ptr);

    answer
}

#[cfg(debug_assertions)]
#[inline(always)]
unsafe fn malloc_or_alloca(bytes: usize) -> *mut c_void {
    libc::malloc(bytes)
}

#[cfg(not(debug_assertions))]
#[inline(always)]
unsafe fn malloc_or_alloca(bytes: usize) -> *mut c_void {
    c_alloca(bytes)
}

#[cfg(debug_assertions)]
#[inline(always)]
unsafe fn free_or_noop(ptr: *mut c_void) {
    libc::free(ptr)
}

#[cfg(not(debug_assertions))]
#[inline(always)]
fn free_or_noop(_ptr: *mut c_void) {
    // In release builds, we'll have used alloca,
    // so there's nothing to free.
}

#[cfg(test)]
mod tests {
    use super::with_stack_bytes;
    use core::mem::size_of;

    #[repr(C)]
    #[derive(Debug, PartialEq)]
    struct TestStruct {
        x: u8,
        y: u16,
        z: u64,
    }

    #[test]
    fn test_alloca() {
        let test_struct = TestStruct {
            x: 123,
            y: 4567,
            z: 89012345678,
        };
        let sum: u64 = unsafe {
            with_stack_bytes(size_of::<TestStruct>(), |ptr| {
                let ptr = ptr as *mut TestStruct;

                (*ptr).x = test_struct.x;
                (*ptr).y = test_struct.y;
                (*ptr).z = test_struct.z;

                assert_eq!(*ptr, test_struct);

                (*ptr).x as u64 + (*ptr).y as u64 + (*ptr).z as u64
            })
        };

        assert_eq!(
            sum,
            test_struct.x as u64 + test_struct.y as u64 + test_struct.z as u64
        );
    }
}
