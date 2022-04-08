use crate::graphics::colors::Rgba;
use core::alloc::Layout;
use core::ffi::c_void;
use core::mem::ManuallyDrop;
use roc_std::{ReferenceCount, RocList, RocStr};
use std::ffi::CStr;
use std::fmt::Debug;
use std::mem::MaybeUninit;
use std::os::raw::c_char;

extern "C" {
    #[link_name = "roc__programForHost_1_exposed_generic"]
    fn roc_program() -> ();

    #[link_name = "roc__programForHost_1_Render_caller"]
    fn call_Render(state: *const State, closure_data: *const u8, output: *mut RocList<RocElem>);

    #[link_name = "roc__programForHost_size"]
    fn roc_program_size() -> i64;

    #[allow(dead_code)]
    #[link_name = "roc__programForHost_1_Render_size"]
    fn size_Render() -> i64;

    #[link_name = "roc__programForHost_1_Render_result_size"]
    fn size_Render_result() -> i64;
}

#[derive(Debug)]
#[repr(C)]
pub struct State {
    pub height: f32,
    pub width: f32,
}

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
#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub struct ElemId(*const RocElemEntry);

#[repr(C)]
pub union RocElemEntry {
    pub rect: ManuallyDrop<RocRect>,
    pub text: ManuallyDrop<RocStr>,
}

#[repr(u8)]
#[allow(unused)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RocElemTag {
    Rect = 0,
    Text = 1,
}

#[repr(C)]
#[cfg(target_pointer_width = "64")] // on a 64-bit system, the tag fits in this pointer's spare 3 bits
pub struct RocElem {
    entry: RocElemEntry,
    tag: RocElemTag,
}

impl Debug for RocElem {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use RocElemTag::*;

        match self.tag() {
            Rect => unsafe { &*self.entry().rect }.fmt(f),
            Text => unsafe { &*self.entry().text }.fmt(f),
        }
    }
}

impl RocElem {
    #[cfg(target_pointer_width = "64")]
    pub fn tag(&self) -> RocElemTag {
        self.tag
    }

    #[allow(unused)]
    pub fn entry(&self) -> &RocElemEntry {
        &self.entry
    }

    #[allow(unused)]
    pub fn rect(styles: ButtonStyles) -> RocElem {
        todo!("restore rect() method")
        // let rect = RocRect { styles };
        // let entry = RocElemEntry {
        //     rect: ManuallyDrop::new(rect),
        // };

        // Self::elem_from_tag(entry, RocElemTag::Rect)
    }

    #[allow(unused)]
    pub fn text<T: Into<RocStr>>(into_roc_str: T) -> RocElem {
        todo!("TODO restore text method")
        // let entry = RocElemEntry {
        //     text: ManuallyDrop::new(into_roc_str.into()),
        // };

        // Self::elem_from_tag(entry, RocElemTag::Text)
    }

    fn elem_from_tag(entry: RocElemEntry, tag: RocElemTag) -> Self {
        Self { entry, tag }
    }
}

#[repr(C)]
#[derive(Debug)]
pub struct RocRect {
    pub color: Rgba,

    // These must be in this order for alphabetization!
    pub height: f32,
    pub left: f32,
    pub top: f32,
    pub width: f32,
}

unsafe impl ReferenceCount for RocElem {
    /// Increment the reference count.
    fn increment(&self) {
        use RocElemTag::*;

        match self.tag() {
            Rect => { /* nothing to increment! */ }
            Text => unsafe { &*self.entry().text }.increment(),
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
            Rect => { /* nothing to decrement! */ }
            Text => ReferenceCount::decrement(&*elem.entry().text),
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

pub fn app_render(state: State) -> RocList<RocElem> {
    let size = unsafe { roc_program_size() } as usize;
    let layout = Layout::array::<u8>(size).unwrap();

    unsafe {
        roc_program();

        // TODO allocate on the stack if it's under a certain size
        let buffer = std::alloc::alloc(layout);

        // Call the program's render function
        let result = call_the_closure(state, buffer);

        std::alloc::dealloc(buffer, layout);

        result
    }
}

unsafe fn call_the_closure(state: State, closure_data_ptr: *const u8) -> RocList<RocElem> {
    let mut output = MaybeUninit::uninit();

    call_Render(&state, closure_data_ptr as *const u8, output.as_mut_ptr());

    output.assume_init()
}
