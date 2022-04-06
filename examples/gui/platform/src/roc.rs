use crate::graphics::colors::Rgba;
use core::ffi::c_void;
use core::mem::{self, ManuallyDrop};
use roc_std::{ReferenceCount, RocList, RocStr};
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

#[repr(transparent)]
#[cfg(target_pointer_width = "64")] // on a 64-bit system, the tag fits in this pointer's spare 3 bits
pub struct RocElem {
    entry: *const RocElemEntry,
}

impl RocElem {
    #[cfg(target_pointer_width = "64")]
    pub fn tag(&self) -> RocElemTag {
        // On a 64-bit system, the last 3 bits of the pointer store the tag
        unsafe { mem::transmute::<u8, RocElemTag>((self.entry as u8) & 0b0000_0111) }
    }

    pub fn entry(&self) -> &RocElemEntry {
        // On a 64-bit system, the last 3 bits of the pointer store the tag
        let cleared = self.entry as usize & !0b111;

        unsafe { &*(cleared as *const RocElemEntry) }
    }
}

#[repr(u8)]
#[allow(unused)] // This is actually used, just via a mem::transmute from u8
#[derive(Debug, Clone, Copy)]
pub enum RocElemTag {
    Button = 0,
    Col,
    Row,
    Text,
}

#[repr(C)]
pub struct RocButton {
    pub child: ManuallyDrop<RocElem>,
    pub styles: ButtonStyles,
}

#[repr(C)]
pub struct RocRowOrCol {
    pub children: RocList<RocElem>,
}

unsafe impl ReferenceCount for RocElem {
    /// Increment the reference count.
    fn increment(&self) {
        use RocElemTag::*;

        match self.tag() {
            Button => unsafe { &*self.entry().button.child }.increment(),
            Text => unsafe { &*self.entry().text }.increment(),
            Row | Col => {
                let children = unsafe { &self.entry().row_or_col.children };

                for child in children.as_slice().iter() {
                    child.increment();
                }
            }
        }
    }

    /// Decrement the reference count.
    ///
    /// # Safety
    ///
    /// The caller must ensure that `ptr` points to a value with a non-zero
    /// reference count.
    unsafe fn decrement(ptr: *const Self) {
        use RocElemTag::*;

        let elem = &*ptr;

        match elem.tag() {
            Button => ReferenceCount::decrement(&*elem.entry().button.child),
            Text => ReferenceCount::decrement(&*elem.entry().text),
            Row | Col => {
                let children = &elem.entry().row_or_col.children;

                for child in children.as_slice().iter() {
                    ReferenceCount::decrement(child);
                }
            }
        }
    }
}

#[repr(C)]
#[derive(Copy, Clone, Debug)]
pub struct ButtonStyles {
    pub bg_color: Rgba,
    pub border_color: Rgba,
    pub border_width: f32,
    pub text_color: Rgba,
}

#[repr(C)]
pub union RocElemEntry {
    pub button: ManuallyDrop<RocButton>,
    pub text: ManuallyDrop<RocStr>,
    pub row_or_col: ManuallyDrop<RocRowOrCol>,
}
