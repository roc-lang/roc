use crate::graphics::colors::Rgba;
use core::alloc::Layout;
use core::ffi::c_void;
use core::mem::{self, ManuallyDrop};
use roc_std::{RocList, RocStr};
use std::ffi::CStr;
use std::fmt::Debug;
use std::mem::MaybeUninit;
use std::os::raw::c_char;
use std::time::Duration;
use winit::event::VirtualKeyCode;

extern "C" {
    // program

    #[link_name = "roc__programForHost_1_exposed_generic"]
    fn roc_program();

    #[link_name = "roc__programForHost_size"]
    fn roc_program_size() -> i64;

    // init

    #[link_name = "roc__programForHost_1__Init_caller"]
    fn call_init(size: *const Bounds, closure_data: *const u8, output: *mut Model);

    #[link_name = "roc__programForHost_1__Init_size"]
    fn init_size() -> i64;

    #[link_name = "roc__programForHost_1__Init_result_size"]
    fn init_result_size() -> i64;

    // update

    #[link_name = "roc__programForHost_1__Update_caller"]
    fn call_update(
        model: *const Model,
        event: *const RocEvent,
        closure_data: *const u8,
        output: *mut Model,
    );

    #[link_name = "roc__programForHost_1__Update_size"]
    fn update_size() -> i64;

    #[link_name = "roc__programForHost_1__Update_result_size"]
    fn update_result_size() -> i64;

    // render

    #[link_name = "roc__programForHost_1__Render_caller"]
    fn call_render(model: *const Model, closure_data: *const u8, output: *mut RocList<RocElem>);

    #[link_name = "roc__programForHost_1__Render_size"]
    fn roc_render_size() -> i64;
}

#[repr(C)]
pub union RocEventEntry {
    pub key_down: RocKeyCode,
    pub key_up: RocKeyCode,
    pub resize: Bounds,
    pub tick: [u8; 16], // u128 is unsupported in repr(C)
}

#[repr(u8)]
#[allow(unused)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RocEventTag {
    KeyDown = 0,
    KeyUp,
    Resize,
    Tick,
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
            Tick => unsafe { self.entry().tick }.fmt(f),
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

    #[allow(non_snake_case)]
    pub fn Resize(size: Bounds) -> Self {
        Self {
            tag: RocEventTag::Resize,
            entry: RocEventEntry { resize: size },
        }
    }

    #[allow(non_snake_case)]
    pub fn KeyDown(keycode: RocKeyCode) -> Self {
        Self {
            tag: RocEventTag::KeyDown,
            entry: RocEventEntry { key_down: keycode },
        }
    }

    #[allow(non_snake_case)]
    pub fn KeyUp(keycode: RocKeyCode) -> Self {
        Self {
            tag: RocEventTag::KeyUp,
            entry: RocEventEntry { key_up: keycode },
        }
    }

    #[allow(non_snake_case)]
    pub fn Tick(duration: Duration) -> Self {
        Self {
            tag: RocEventTag::Tick,
            entry: RocEventEntry {
                tick: duration.as_nanos().to_ne_bytes(),
            },
        }
    }
}

#[repr(u8)]
#[allow(unused)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RocKeyCode {
    Left = 0,
    Other,
    Right,
}

impl From<VirtualKeyCode> for RocKeyCode {
    fn from(keycode: VirtualKeyCode) -> Self {
        use VirtualKeyCode::*;

        match keycode {
            Left => RocKeyCode::Left,
            Right => RocKeyCode::Right,
            _ => RocKeyCode::Other,
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
#[derive(Debug, Clone, Copy)]
pub struct RocRect {
    pub color: Rgba,

    // These must be in this order for alphabetization!
    pub height: f32,
    pub left: f32,
    pub top: f32,
    pub width: f32,
}

impl Clone for RocElem {
    fn clone(&self) -> Self {
        unsafe {
            match self.tag() {
                RocElemTag::Rect => Self {
                    tag: RocElemTag::Rect,
                    entry: RocElemEntry {
                        rect: self.entry.rect.clone(),
                    },
                },
                RocElemTag::Text => Self {
                    tag: RocElemTag::Text,
                    entry: RocElemEntry {
                        text: self.entry.text.clone(),
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
                RocElemTag::Rect => mem::drop(ManuallyDrop::take(&mut self.entry.rect)),
                RocElemTag::Text => mem::drop(ManuallyDrop::take(&mut self.entry.text)),
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

#[derive(Copy, Clone, Debug, Default)]
#[repr(C)]
pub struct Bounds {
    pub height: f32,
    pub width: f32,
}

type Model = c_void;

/// Call the app's init function, then render and return that result
pub fn init_and_render(bounds: Bounds) -> (*const Model, RocList<RocElem>) {
    let closure_data_buf;
    let closure_layout;

    // Call init to get the initial model
    let model = unsafe {
        let ret_val_layout = Layout::array::<u8>(init_result_size() as usize).unwrap();

        // TODO allocate on the stack if it's under a certain size
        let ret_val_buf = std::alloc::alloc(ret_val_layout) as *mut Model;

        closure_layout = Layout::array::<u8>(init_size() as usize).unwrap();

        // TODO allocate on the stack if it's under a certain size
        closure_data_buf = std::alloc::alloc(closure_layout);

        call_init(&bounds, closure_data_buf, ret_val_buf);

        ret_val_buf
    };

    // Call render passing the model to get the initial Elems
    let elems = unsafe {
        let mut ret_val: MaybeUninit<RocList<RocElem>> = MaybeUninit::uninit();

        // Reuse the buffer from the previous closure if possible
        let closure_data_buf =
            std::alloc::realloc(closure_data_buf, closure_layout, roc_render_size() as usize);

        call_render(model, closure_data_buf, ret_val.as_mut_ptr());

        std::alloc::dealloc(closure_data_buf, closure_layout);

        ret_val.assume_init()
    };

    (model, elems)
}

/// Call the app's update function, then render and return that result
pub fn update(model: *const Model, event: RocEvent) -> *const Model {
    let closure_data_buf;
    let closure_layout;

    // Call update to get the new model
    unsafe {
        let ret_val_layout = Layout::array::<u8>(update_result_size() as usize).unwrap();

        // TODO allocate on the stack if it's under a certain size
        let ret_val_buf = std::alloc::alloc(ret_val_layout) as *mut Model;

        closure_layout = Layout::array::<u8>(update_size() as usize).unwrap();

        // TODO allocate on the stack if it's under a certain size
        closure_data_buf = std::alloc::alloc(closure_layout);

        call_update(model, &event, closure_data_buf, ret_val_buf);

        ret_val_buf
    }
}

/// Call the app's update function, then render and return that result
pub fn update_and_render(model: *const Model, event: RocEvent) -> (*const Model, RocList<RocElem>) {
    let closure_data_buf;
    let closure_layout;

    // Call update to get the new model
    let model = unsafe {
        let ret_val_layout = Layout::array::<u8>(update_result_size() as usize).unwrap();

        // TODO allocate on the stack if it's under a certain size
        let ret_val_buf = std::alloc::alloc(ret_val_layout) as *mut Model;

        closure_layout = Layout::array::<u8>(update_size() as usize).unwrap();

        // TODO allocate on the stack if it's under a certain size
        closure_data_buf = std::alloc::alloc(closure_layout);

        call_update(model, &event, closure_data_buf, ret_val_buf);

        ret_val_buf
    };

    // Call render passing the model to get the initial Elems
    let elems = unsafe {
        let mut ret_val: MaybeUninit<RocList<RocElem>> = MaybeUninit::uninit();

        // Reuse the buffer from the previous closure if possible
        let closure_data_buf =
            std::alloc::realloc(closure_data_buf, closure_layout, roc_render_size() as usize);

        call_render(model, closure_data_buf, ret_val.as_mut_ptr());

        std::alloc::dealloc(closure_data_buf, closure_layout);

        ret_val.assume_init()
    };

    (model, elems)
}
