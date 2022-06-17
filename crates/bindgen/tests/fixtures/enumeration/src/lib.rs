mod bindings;

extern "C" {
    #[link_name = "roc__mainForHost_1_exposed_generic"]
    fn roc_main(_: *mut bindings::MyEnum);
}

#[no_mangle]
pub extern "C" fn rust_main() -> i32 {
    use std::cmp::Ordering;
    use std::collections::hash_set::HashSet;

    let tag_union = unsafe {
        let mut ret: core::mem::MaybeUninit<bindings::MyEnum> = core::mem::MaybeUninit::uninit();

        roc_main(ret.as_mut_ptr());

        ret.assume_init()
    };

    // Verify that it has all the expected traits.

    assert!(tag_union == tag_union); // PartialEq
    assert!(tag_union.clone() == tag_union.clone()); // Clone

    // Since this is a move, later uses of `tag_union` will fail unless `tag_union` has Copy
    let union2 = tag_union; // Copy

    assert!(tag_union.partial_cmp(&tag_union) == Some(Ordering::Equal)); // PartialOrd
    assert!(tag_union.cmp(&tag_union) == Ordering::Equal); // Ord

    let mut set = HashSet::new();

    set.insert(tag_union); // Eq, Hash
    set.insert(union2);

    assert_eq!(set.len(), 1);

    println!(
        "tag_union was: {:?}, Bar is: {:?}, Baz is: {:?}",
        tag_union,
        bindings::MyEnum::Bar,
        bindings::MyEnum::Baz,
    ); // Debug

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
pub unsafe extern "C" fn roc_shm_open(name: *const i8, oflag: i32, mode: u32) -> i32 {
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
