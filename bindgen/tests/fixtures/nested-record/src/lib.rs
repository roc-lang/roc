mod bindings;

extern "C" {
    #[link_name = "roc__mainForHost_1_exposed_generic"]
    fn roc_main(_: *mut bindings::Outer);
}

#[no_mangle]
pub extern "C" fn rust_main() -> i32 {
    use std::cmp::Ordering;

    let outer = unsafe {
        let mut ret: core::mem::MaybeUninit<bindings::Outer> = core::mem::MaybeUninit::uninit();

        roc_main(ret.as_mut_ptr());

        ret.assume_init()
    };

    // Verify that `inner` has all the expected traits.
    {
        let inner = outer.x;

        assert!(inner == inner); // PartialEq
        assert!(inner.clone() == inner.clone()); // Clone

        // Since this is a move, later uses of `inner` will fail unless `inner` has Copy
        let inner2 = inner; // Copy

        assert!(inner2 != Default::default()); // Default
        assert!(inner.partial_cmp(&inner) == Some(Ordering::Equal)); // PartialOrd
    }

    // Verify that `outer` has all the expected traits.
    {
        assert!(outer == outer); // PartialEq
        assert!(outer.clone() == outer.clone()); // Clone
        assert!(outer != Default::default()); // Default
        assert!(outer.partial_cmp(&outer) == Some(Ordering::Equal)); // PartialOrd
    }

    println!("Record was: {:?}", outer);

    // Exit code
    0
}

// Externs required by roc_std and by the Roc app

use core::ffi::c_void;
use std::ffi::CStr;
use std::os::raw::c_char;

#[no_mangle]
pub unsafe extern "C" fn roc_alloc(size: usize, _alignment: u32) -> *mut c_void {
    return libc::malloc(size);
}

#[no_mangle]
pub unsafe extern "C" fn roc_realloc(
    c_ptr: *mut c_void,
    new_size: usize,
    _old_size: usize,
    _alignment: u32,
) -> *mut c_void {
    return libc::realloc(c_ptr, new_size);
}

#[no_mangle]
pub unsafe extern "C" fn roc_dealloc(c_ptr: *mut c_void, _alignment: u32) {
    return libc::free(c_ptr);
}

#[no_mangle]
pub unsafe extern "C" fn roc_panic(c_ptr: *mut c_void, tag_id: u32) {
    match tag_id {
        0 => {
            let slice = CStr::from_ptr(c_ptr as *const c_char);
            let string = slice.to_str().unwrap();
            eprintln!("Roc hit a panic: {}", string);
            std::process::exit(1);
        }
        _ => todo!(),
    }
}

#[no_mangle]
pub unsafe extern "C" fn roc_memcpy(dst: *mut c_void, src: *mut c_void, n: usize) -> *mut c_void {
    libc::memcpy(dst, src, n)
}

#[no_mangle]
pub unsafe extern "C" fn roc_memset(dst: *mut c_void, c: i32, n: usize) -> *mut c_void {
    libc::memset(dst, c, n)
}

#[no_mangle]
pub unsafe extern "C" fn roc_shm_open(name: *const i8, oflag: i32, mode: i32) -> i32 {
    libc::shm_open(name, oflag, mode)
}

#[no_mangle]
pub unsafe extern "C" fn roc_mmap(
    addr: *mut c_void,
    len: usize,
    prot: i32,
    flags: i32,
    fd: i32,
    offset: i64,
) -> *mut c_void {
    libc::mmap(addr, len, prot, flags, fd, offset)
}

#[no_mangle]
pub unsafe extern "C" fn roc_kill(pid: i32, sig: i32) -> i32 {
    libc::kill(pid, sig)
}

#[no_mangle]
pub unsafe extern "C" fn roc_getppid() -> i32 {
    libc::getppid()
}
