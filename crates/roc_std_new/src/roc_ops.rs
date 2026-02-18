//! RocOps and related types for the new host ABI.
//!
//! This module defines the interface between Roc programs and their host platforms.
//! Instead of using global symbols (extern "C" functions), the new ABI passes
//! a RocOps struct containing function pointers for memory management and other
//! host-provided operations.

use core::ffi::c_void;

/// Arguments passed to the host's allocation function.
///
/// The host reads `alignment` and `length`, allocates memory accordingly,
/// and writes the allocated pointer into `answer`.
#[repr(C)]
#[derive(Debug)]
pub struct RocAlloc {
    pub alignment: usize,
    pub length: usize,
    /// The host writes the allocated pointer here.
    pub answer: *mut c_void,
}

/// Arguments passed to the host's deallocation function.
///
/// Note: The length of the allocation is not provided because it is not
/// always known at runtime (due to seamless slices).
#[repr(C)]
#[derive(Debug)]
pub struct RocDealloc {
    pub alignment: usize,
    pub ptr: *mut c_void,
}

/// Arguments passed to the host's reallocation function.
///
/// The `answer` field serves as both input (old pointer) and output (new pointer).
/// The host reads the old pointer from `answer`, reallocates, and writes
/// the new pointer back into `answer`.
#[repr(C)]
#[derive(Debug)]
pub struct RocRealloc {
    pub alignment: usize,
    pub new_length: usize,
    /// Input: the old pointer. Output: the new pointer.
    pub answer: *mut c_void,
}

/// Information passed to the host when `dbg` is called in Roc code.
#[repr(C)]
#[derive(Debug)]
pub struct RocDbg {
    pub utf8_bytes: *mut u8,
    pub len: usize,
}

/// Information passed to the host when an `expect` fails.
#[repr(C)]
#[derive(Debug)]
pub struct RocExpectFailed {
    pub utf8_bytes: *mut u8,
    pub len: usize,
}

/// Information passed to the host when a Roc program crashes.
#[repr(C)]
#[derive(Debug)]
pub struct RocCrashed {
    pub utf8_bytes: *mut u8,
    pub len: usize,
}

/// Function pointer type for hosted functions provided by the platform.
/// All hosted functions follow the RocCall ABI: (ops, ret_ptr, args_ptr).
pub type HostedFn = extern "C" fn(*const RocOps, *mut c_void, *mut c_void);

/// Array of hosted function pointers provided by the platform.
/// These are sorted alphabetically by function name during canonicalization.
#[repr(C)]
#[derive(Debug, Clone, Copy)]
pub struct HostedFunctions {
    pub count: u32,
    pub fns: *const HostedFn,
}

/// The operations (function pointers) that a running Roc program needs
/// the host to provide.
///
/// This struct is passed to Roc functions and used for memory management,
/// debug output, crash handling, and calling platform-specific effects.
///
/// # Safety
///
/// The `RocOps` struct must remain valid for the entire duration that any
/// Roc values (RocList, RocStr, RocBox) created using it are alive.
/// This is because those values store a pointer to the RocOps and use it
/// during Drop to deallocate memory.
#[repr(C)]
pub struct RocOps {
    /// Host-provided context pointer. Roc passes this to each of the
    /// function pointers as their second argument. This lets hosts use
    /// custom allocators (e.g., arena allocators) by storing the arena here.
    /// Can be null if unused.
    pub env: *mut c_void,

    /// Memory allocation function. Similar to `aligned_alloc`.
    pub roc_alloc: extern "C" fn(*mut RocAlloc, *mut c_void),

    /// Memory deallocation function. Similar to `aligned_free`.
    pub roc_dealloc: extern "C" fn(*mut RocDealloc, *mut c_void),

    /// Memory reallocation function. Similar to `aligned_realloc`.
    pub roc_realloc: extern "C" fn(*mut RocRealloc, *mut c_void),

    /// Called when the Roc program calls `dbg`.
    pub roc_dbg: extern "C" fn(*const RocDbg, *mut c_void),

    /// Called when an inline `expect` fails.
    pub roc_expect_failed: extern "C" fn(*const RocExpectFailed, *mut c_void),

    /// Called when the Roc program crashes (e.g., integer overflow).
    pub roc_crashed: extern "C" fn(*const RocCrashed, *mut c_void),

    /// Hosted functions provided by the platform (sorted alphabetically).
    pub hosted_fns: HostedFunctions,
}

impl RocOps {
    /// Allocate memory with the given alignment and length.
    ///
    /// # Safety
    ///
    /// The returned pointer is valid until deallocated. The caller must
    /// ensure proper alignment and that the allocation is eventually freed.
    #[inline]
    pub unsafe fn alloc(&self, alignment: usize, length: usize) -> *mut c_void {
        let mut args = RocAlloc {
            alignment,
            length,
            answer: core::ptr::null_mut(),
        };
        (self.roc_alloc)(&mut args, self.env);
        args.answer
    }

    /// Deallocate memory previously allocated with `alloc` or `realloc`.
    ///
    /// # Safety
    ///
    /// The pointer must have been allocated by this RocOps instance with
    /// the same alignment. After this call, the pointer is invalid.
    #[inline]
    pub unsafe fn dealloc(&self, ptr: *mut c_void, alignment: usize) {
        let mut args = RocDealloc { alignment, ptr };
        (self.roc_dealloc)(&mut args, self.env);
    }

    /// Reallocate memory to a new size.
    ///
    /// # Safety
    ///
    /// The old pointer must have been allocated by this RocOps instance.
    /// The returned pointer replaces the old one (which becomes invalid).
    #[inline]
    pub unsafe fn realloc(
        &self,
        old_ptr: *mut c_void,
        alignment: usize,
        new_length: usize,
    ) -> *mut c_void {
        let mut args = RocRealloc {
            alignment,
            new_length,
            answer: old_ptr,
        };
        (self.roc_realloc)(&mut args, self.env);
        args.answer
    }

    /// Send a debug message to the host.
    #[inline]
    pub fn dbg(&self, message: &str) {
        let args = RocDbg {
            utf8_bytes: message.as_ptr() as *mut u8,
            len: message.len(),
        };
        (self.roc_dbg)(&args, self.env);
    }

    /// Report a crash to the host.
    #[inline]
    pub fn crash(&self, message: &str) {
        let args = RocCrashed {
            utf8_bytes: message.as_ptr() as *mut u8,
            len: message.len(),
        };
        (self.roc_crashed)(&args, self.env);
    }
}

// RocOps contains raw pointers, so it's not automatically Send/Sync.
// The host is responsible for ensuring thread safety when needed.
// We don't implement Send/Sync to force explicit handling of multi-threading.

#[cfg(test)]
pub mod test_helpers {
    use super::*;

    /// Create a RocOps instance for testing that uses libc malloc/free.
    ///
    /// # Safety
    ///
    /// The returned RocOps is valid for the duration of the test.
    /// All Roc values created with it must be dropped before the test ends.
    pub fn test_roc_ops() -> RocOps {
        RocOps {
            env: core::ptr::null_mut(),
            roc_alloc: test_alloc,
            roc_dealloc: test_dealloc,
            roc_realloc: test_realloc,
            roc_dbg: test_dbg,
            roc_expect_failed: test_expect_failed,
            roc_crashed: test_crashed,
            hosted_fns: HostedFunctions {
                count: 0,
                fns: core::ptr::null(),
            },
        }
    }

    extern "C" fn test_alloc(args: *mut RocAlloc, _env: *mut c_void) {
        unsafe {
            let args = &mut *args;
            args.answer = libc::malloc(args.length);
        }
    }

    extern "C" fn test_dealloc(args: *mut RocDealloc, _env: *mut c_void) {
        unsafe {
            let args = &*args;
            libc::free(args.ptr);
        }
    }

    extern "C" fn test_realloc(args: *mut RocRealloc, _env: *mut c_void) {
        unsafe {
            let args = &mut *args;
            args.answer = libc::realloc(args.answer, args.new_length);
        }
    }

    extern "C" fn test_dbg(args: *const RocDbg, _env: *mut c_void) {
        unsafe {
            let args = &*args;
            let msg = core::slice::from_raw_parts(args.utf8_bytes, args.len);
            let msg = core::str::from_utf8_unchecked(msg);
            eprintln!("[dbg] {}", msg);
        }
    }

    extern "C" fn test_expect_failed(args: *const RocExpectFailed, _env: *mut c_void) {
        unsafe {
            let args = &*args;
            let msg = core::slice::from_raw_parts(args.utf8_bytes, args.len);
            let msg = core::str::from_utf8_unchecked(msg);
            eprintln!("[expect failed] {}", msg);
        }
    }

    extern "C" fn test_crashed(args: *const RocCrashed, _env: *mut c_void) {
        unsafe {
            let args = &*args;
            let msg = core::slice::from_raw_parts(args.utf8_bytes, args.len);
            let msg = core::str::from_utf8_unchecked(msg);
            panic!("[crashed] {}", msg);
        }
    }
}
