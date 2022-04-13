use crate::graphics::colors::Rgba;
use core::alloc::Layout;
use core::ffi::c_void;
use core::mem::ManuallyDrop;
use roc_std::{ReferenceCount, RocList, RocStr};
use std::ffi::CStr;
use std::fmt::Debug;
use std::mem::MaybeUninit;
use std::os::raw::c_char;
use winit::event::VirtualKeyCode;

extern "C" {
    // program

    #[link_name = "roc__programForHost_1_exposed_generic"]
    fn roc_program() -> ();

    #[link_name = "roc__programForHost_size"]
    fn roc_program_size() -> i64;

    // init

    #[link_name = "roc__programForHost_1_Init_caller"]
    fn call_init(size: *const Bounds, closure_data: *const u8, output: *mut Model);

    #[link_name = "roc__programForHost_1_Init_size"]
    fn init_size() -> i64;

    #[link_name = "roc__mainForHost_1_Init_result_size"]
    fn init_result_size() -> i64;

    // update

    #[link_name = "roc__programForHost_1_Update_caller"]
    fn call_update(
        model: *const Model,
        event: *const RocEvent,
        closure_data: *const u8,
        output: *mut Model,
    );

    #[link_name = "roc__programForHost_1_Update_size"]
    fn update_size() -> i64;

    #[link_name = "roc__mainForHost_1_Update_result_size"]
    fn update_result_size() -> i64;

    // render

    #[link_name = "roc__programForHost_1_Render_caller"]
    fn call_render(model: *const Model, closure_data: *const u8, output: *mut RocList<RocElem>);

    #[link_name = "roc__programForHost_1_Render_size"]
    fn roc_render_size() -> i64;
}

#[repr(C)]
pub union RocEventEntry {
    pub key_down: winit::event::VirtualKeyCode,
    pub key_up: winit::event::VirtualKeyCode,
    pub resize: Bounds,
}

#[repr(u8)]
#[allow(unused)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RocEventTag {
    KeyDown = 0,
    KeyUp = 1,
    Resize = 2,
}

#[repr(C)]
#[cfg(target_pointer_width = "64")] // on a 64-bit system, the tag fits in this pointer's spare 3 bits
pub struct RocEvent {
    entry: RocEventEntry,
    tag: RocEventTag,
}

impl Debug for RocEvent {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use RocEventTag::*;

        match self.tag() {
            KeyDown => unsafe { self.entry().key_down }.fmt(f),
            KeyUp => unsafe { self.entry().key_up }.fmt(f),
            Resize => unsafe { self.entry().resize }.fmt(f),
        }
    }
}

impl RocEvent {
    #[cfg(target_pointer_width = "64")]
    pub fn tag(&self) -> RocEventTag {
        self.tag
    }

    pub fn entry(&self) -> &RocEventEntry {
        &self.entry
    }

    pub fn resize(size: Bounds) -> Self {
        Self {
            tag: RocEventTag::Resize,
            entry: RocEventEntry { resize: size },
        }
    }

    pub fn key_down(keycode: VirtualKeyCode) -> Self {
        Self {
            tag: RocEventTag::KeyDown,
            entry: RocEventEntry { key_down: keycode },
        }
    }

    pub fn key_up(keycode: VirtualKeyCode) -> Self {
        Self {
            tag: RocEventTag::KeyUp,
            entry: RocEventEntry { key_up: keycode },
        }
    }
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

pub fn render(event: RocEvent) -> RocList<RocElem> {
    let mut output = MaybeUninit::uninit();

    // Call the program's render function
    unsafe {
        let layout = Layout::array::<u8>(roc_render_size() as usize).unwrap();

        // TODO allocate on the stack if it's under a certain size
        let buffer = std::alloc::alloc(layout);

        if true {
            todo!("call render here");
            // call_render(&event, buffer, output.as_mut_ptr());
        }

        std::alloc::dealloc(buffer, layout);

        output.assume_init()
    }
}

#[derive(Copy, Clone, Debug, Default)]
#[repr(C)]
pub struct Bounds {
    pub height: f32,
    pub width: f32,
}

type Model = c_void;

/// Call the app's init function
pub fn init_and_render(bounds: Bounds) -> (*const Model, RocList<RocElem>) {
    let closure_data_buf;
    let layout;

    // Call init to get the initial model
    let model = unsafe {
        let mut ret_val = MaybeUninit::uninit();

        layout = Layout::array::<u8>(init_size() as usize).unwrap();

        // TODO allocate on the stack if it's under a certain size
        closure_data_buf = std::alloc::alloc(layout);

        dbg!(&bounds);

        call_init(&bounds, closure_data_buf, ret_val.as_mut_ptr());

        ret_val.assume_init()
    };

    unsafe {
        let model_returned_by_init: Bounds = *std::mem::transmute::<&c_void, *const Bounds>(&model);
        dbg!(model_returned_by_init);
    }

    // Call render passing the model to get the initial Elems
    let elems = unsafe {
        let mut ret_val = MaybeUninit::uninit();

        // Reuse the buffer from the previous closure if possible
        let closure_data_buf =
            std::alloc::realloc(closure_data_buf, layout, roc_render_size() as usize);

        call_render(&model, closure_data_buf, ret_val.as_mut_ptr());

        std::alloc::dealloc(closure_data_buf, layout);

        ret_val.assume_init()
    };

    (&model, elems)
}
