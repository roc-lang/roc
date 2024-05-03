use crate::graphics::colors::Rgba;
use core::ffi::c_void;
use core::mem::{self, ManuallyDrop};
use roc_std::{RocList, RocStr};
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

#[repr(transparent)]
#[cfg(target_pointer_width = "64")] // on a 64-bit system, the tag fits in this pointer's spare 3 bits
pub struct RocElem {
    entry: *mut RocElemEntry,
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
#[derive(Clone)]
pub struct RocButton {
    pub child: ManuallyDrop<RocElem>,
    pub styles: ButtonStyles,
}

#[repr(C)]
#[derive(Clone)]
pub struct RocRowOrCol {
    pub children: RocList<RocElem>,
}

impl Clone for RocElem {
    fn clone(&self) -> Self {
        unsafe {
            match self.tag() {
                RocElemTag::Button => Self {
                    entry: &mut RocElemEntry {
                        button: (*self.entry).button.clone(),
                    },
                },
                RocElemTag::Text => Self {
                    entry: &mut RocElemEntry {
                        text: (*self.entry).text.clone(),
                    },
                },
                RocElemTag::Col | RocElemTag::Row => Self {
                    entry: &mut RocElemEntry {
                        row_or_col: (*self.entry).row_or_col.clone(),
                    },
                },
            }
        }
    }
}

impl Drop for RocElem {
    fn drop(&mut self) {
        unsafe {
            match self.tag() {
                RocElemTag::Button => mem::drop(ManuallyDrop::take(&mut (*self.entry).button)),
                RocElemTag::Text => mem::drop(ManuallyDrop::take(&mut (*self.entry).text)),
                RocElemTag::Col | RocElemTag::Row => {
                    mem::drop(ManuallyDrop::take(&mut (*self.entry).row_or_col))
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
