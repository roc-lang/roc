use roc_app;

use indoc::indoc;
use roc_app::StrRoseTree;
use roc_std::{RocList, RocStr};

extern "C" {
    #[link_name = "roc__main_for_host_1_exposed_generic"]
    fn roc_main(_: *mut StrRoseTree);
}

#[no_mangle]
pub extern "C" fn rust_main() {
    use std::cmp::Ordering;
    use std::collections::hash_set::HashSet;

    init();

    let tag_union = unsafe {
        let mut ret: core::mem::MaybeUninit<StrRoseTree> = core::mem::MaybeUninit::uninit();

        roc_main(ret.as_mut_ptr());

        ret.assume_init()
    };

    // Verify that it has all the expected traits.

    assert!(tag_union == tag_union); // PartialEq
    assert!(tag_union.clone() == tag_union.clone()); // Clone

    assert!(tag_union.partial_cmp(&tag_union) == Some(Ordering::Equal)); // PartialOrd
    assert!(tag_union.cmp(&tag_union) == Ordering::Equal); // Ord

    print!(
        indoc!(
            r#"
                tag_union was: {:?}
                Tree "foo" [] is: {:?}
            "#
        ),
        tag_union,
        StrRoseTree::Tree("foo".into(), RocList::empty())
    ); // Debug

    let mut set = HashSet::new();

    set.insert(tag_union.clone()); // Eq, Hash
    set.insert(tag_union);

    assert_eq!(set.len(), 1);
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
pub unsafe extern "C" fn roc_panic(msg: *mut RocStr, tag_id: u32) {
    match tag_id {
        0 => {
            eprintln!("Roc standard library hit a panic: {}", &*msg);
        }
        1 => {
            eprintln!("Application hit a panic: {}", &*msg);
        }
        _ => unreachable!(),
    }
    std::process::exit(1);
}

#[no_mangle]
pub unsafe extern "C" fn roc_dbg(loc: *mut RocStr, msg: *mut RocStr, src: *mut RocStr) {
    eprintln!("[{}] {} = {}", &*loc, &*src, &*msg);
}

#[no_mangle]
pub unsafe extern "C" fn roc_memset(dst: *mut c_void, c: i32, n: usize) -> *mut c_void {
    libc::memset(dst, c, n)
}

pub fn init() {
    if cfg!(unix) {
        let unix_funcs: &[*const extern "C" fn()] =
            &[roc_getppid as _, roc_mmap as _, roc_shm_open as _];
        #[allow(forgetting_references)]
        std::mem::forget(std::hint::black_box(unix_funcs));
    }
}

/// # Safety
///
/// This function is unsafe.
#[cfg(unix)]
#[no_mangle]
pub unsafe extern "C" fn roc_getppid() -> libc::pid_t {
    libc::getppid()
}

/// # Safety
///
/// This function should be called with a valid addr pointer.
#[cfg(unix)]
#[no_mangle]
pub unsafe extern "C" fn roc_mmap(
    addr: *mut libc::c_void,
    len: libc::size_t,
    prot: libc::c_int,
    flags: libc::c_int,
    fd: libc::c_int,
    offset: libc::off_t,
) -> *mut libc::c_void {
    libc::mmap(addr, len, prot, flags, fd, offset)
}

/// # Safety
///
/// This function should be called with a valid name pointer.
#[cfg(unix)]
#[no_mangle]
pub unsafe extern "C" fn roc_shm_open(
    name: *const libc::c_char,
    oflag: libc::c_int,
    mode: libc::mode_t,
) -> libc::c_int {
    libc::shm_open(name, oflag, mode as libc::c_uint)
}
