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
        unsafe { &*self.entry_ptr() }
    }

    pub fn entry_ptr(&self) -> *const RocElemEntry {
        // On a 64-bit system, the last 3 bits of the pointer store the tag
        let cleared = self.entry as usize & !0b111;

        cleared as *const RocElemEntry
    }

    fn diff(self, other: RocElem, patches: &mut Vec<(usize, Patch)>, index: usize) {
        use RocElemTag::*;

        let tag = self.tag();

        if tag != other.tag() {
            // They were totally different elem types!

            // TODO should we handle Row -> Col or Col -> Row differently?
            // Elm doesn't: https://github.com/elm/virtual-dom/blob/5a5bcf48720bc7d53461b3cd42a9f19f119c5503/src/Elm/Kernel/VirtualDom.js#L714
            return;
        }

        match tag {
            Button => unsafe {
                let button_self = &*self.entry().button;
                let button_other = &*other.entry().button;

                // TODO compute a diff and patch for the button
            },
            Text => unsafe {
                let str_self = &*self.entry().text;
                let str_other = &*other.entry().text;

                if str_self != str_other {
                    todo!("fix this");
                    // let roc_str = other.entry().text;
                    // let patch = Patch::Text(ManuallyDrop::into_inner(roc_str));

                    // patches.push((index, patch));
                }
            },
            Row => unsafe {
                let children_self = &self.entry().row_or_col.children;
                let children_other = &other.entry().row_or_col.children;

                // TODO diff children
            },
            Col => unsafe {
                let children_self = &self.entry().row_or_col.children;
                let children_other = &other.entry().row_or_col.children;

                // TODO diff children
            },
        }
    }

    pub fn is_focusable(&self) -> bool {
        use RocElemTag::*;

        match self.tag() {
            Button => true,
            Text | Row | Col => false,
        }
    }

    pub fn button(styles: ButtonStyles, child: RocElem) -> RocElem {
        let button = RocButton {
            child: ManuallyDrop::new(child),
            styles,
        };
        let entry = RocElemEntry {
            button: ManuallyDrop::new(button),
        };

        Self::elem_from_tag(entry, RocElemTag::Button)
    }

    pub fn text<T: Into<RocStr>>(into_roc_str: T) -> RocElem {
        let entry = RocElemEntry {
            text: ManuallyDrop::new(into_roc_str.into()),
        };

        Self::elem_from_tag(entry, RocElemTag::Text)
    }

    fn elem_from_tag(entry: RocElemEntry, tag: RocElemTag) -> Self {
        let entry_box = Box::new(entry);
        let entry_ptr = entry_box.as_ref() as *const RocElemEntry;
        let tagged_ptr = entry_ptr as usize | tag as usize;

        Self {
            entry: tagged_ptr as *const RocElemEntry,
        }
    }
}

#[repr(u8)]
#[allow(unused)] // This is actually used, just via a mem::transmute from u8
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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
#[derive(Copy, Clone, Debug, Default)]
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

enum Patch {
    Text(RocStr),
}
