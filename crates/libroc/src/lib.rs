#![cfg_attr(not(feature = "std"), no_std)]

use core::alloc::Layout;

use mimalloc::MiMalloc;

#[global_allocator]
static GLOBAL_ALLOCATOR: MiMalloc = MiMalloc;

// The alignment of allocations that get exposed via externs, e.g. error messages and repl eval output
const EXTERN_ALLOC_ALIGNMENT: usize = 2 * core::mem::size_of::<usize>();

/// The persistent state of Roc compilation.
#[repr(C)]
pub struct RocEnv {
    // TODO put things like modules in here, Src64s, etc...
    // remember, sometimes we'll just be editing a buffer, no
    // filename to go with it. Could be a repl string to eval, or maybe a playground?
    //
    // We might want to intern all the module IDs, for example, based on...file path? module name?
    // (module name is shorter, so probably a better default choice)
    //
    // interesting question: do we want to store active buffers differently from closed files?
    // e.g. should we try to load the entire project into memory? or is that a waste of time?
    // prob not a waste of time. full application builds will need the whole thing anyway, right?
}

/// The input pointer should point to bytes (of length input_len) which are supposed to
/// be interpreted as UTF-8 Roc source code. This function will check that they are valid
/// UTF-8; if you already know for certain that they are valid UTF-8, and checking them again
/// would be a waste of time, you can use roc_eval_unchecked to skip UTF-8 validation.
///
/// The returned pointer will be to utf-8 bytes allocated on the heap by this library,
/// and the number of bytes in that allocation will be written to output_len.
/// To free the allocation, call roc_free passing the returned pointer and output_len.
#[no_mangle]
pub extern "C" fn roc_check(
    env: *mut RocEnv,
    input_module_name: *const u8, // TODO maybe have like a Document struct which has module name etc.
    input_module_name_len: usize,
    input: *const u8,
    input_len: usize,
    output_len: *mut usize,
) -> *const u8 {
    // TODO is this the function we want? Like we want to call this whenever the source code changes.
    // Do we want to accept a source code range maybe? So we can modify the underlying source?
    // (Maybe it's a rope? Maybe not!)
    todo!()
}

/// The input pointer should point to bytes (of length input_len) which are supposed to
/// be interpreted as UTF-8 Roc source code. This function will check that they are valid
/// UTF-8; if you already know for certain that they are valid UTF-8, and checking them again
/// would be a waste of time, you can use roc_eval_unchecked to skip UTF-8 validation.
///
/// The returned pointer will be to utf-8 bytes allocated on the heap by this library,
/// and the number of bytes in that allocation will be written to output_len.
/// To free the allocation, call roc_free passing the returned pointer and output_len.
#[no_mangle]
pub extern "C" fn roc_eval(
    env: *mut RocEnv,
    input: *const u8,
    input_len: usize,
    output_len: *mut usize,
) -> *const u8 {
    // TODO figure out how to represent errors. Maybe return a struct of output_len plus *const u8?
    // Maybe return a tagged union?
    todo!()
}

/// This is the same as roc_eval except it does not validate UTF-8.
/// Giving it invalid UTF-8 will result in undefined behavior, so if there is any chance
/// these bytes are not valid UTF-8, use roc_eval instead.
#[no_mangle]
pub extern "C" fn roc_eval_unchecked(
    input: *const u8,
    input_len: usize,
    output_len: *mut usize,
) -> *const u8 {
    // TODO this is like eval except we assume it's valid utf-8 already
    // (e.g. because the editor already validated it)
    todo!()
}

/// Free an allocation returned by one of these roc_ functions. It's undefined behavior to pass
/// this a null pointer, or a pointer that wasn't returned by a roc_ function.
#[no_mangle]
// Note: this is named roc_free and not roc_dealloc in case this is being used in a project
// that's a roc host, and which already has something named roc_dealloc
pub extern "C" fn roc_free(ptr: *mut u8, len: usize) {
    use core::alloc::GlobalAlloc;

    unsafe {
        // Safety: we know EXTERN_ALLOC_ALIGNMENT is a power of 2.
        let layout = Layout::from_size_align_unchecked(len, EXTERN_ALLOC_ALIGNMENT);

        // Safety: this is not safe at all, but callers know that.
        GLOBAL_ALLOCATOR.dealloc(ptr, layout)
    }
}
