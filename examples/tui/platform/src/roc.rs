
use core::alloc::Layout;
use core::ffi::c_void;
// use core::mem::{self, ManuallyDrop};
use roc_std::{
    RocList,
    // RocStr,
};
use std::ffi::CStr;
// use std::fmt::Debug;
use std::mem::MaybeUninit;
use std::os::raw::c_char;
// use std::time::Duration;
use crate::glue::{
    Model,
    Event,
    Bounds,
    Elem,
};

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
        event: *const Event,
        closure_data: *const u8,
        output: *mut Model,
    );

    #[link_name = "roc__programForHost_1__Update_size"]
    fn update_size() -> i64;

    #[link_name = "roc__programForHost_1__Update_result_size"]
    fn update_result_size() -> i64;

    // render

    #[link_name = "roc__programForHost_1__Render_caller"]
    fn call_render(model: *const Model, closure_data: *const u8, output: *mut RocList<Elem>);

    #[link_name = "roc__programForHost_1__Render_size"]
    fn roc_render_size() -> i64;
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

/// Call the app's init function, then render and return that result
pub fn init_and_render(bounds: Bounds) -> (*const Model, RocList<Elem>) {
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
        let mut ret_val: MaybeUninit<RocList<Elem>> = MaybeUninit::uninit();

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
pub fn update(model: *const Model, event: Event) -> *const Model {
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

/// Call the app's render and return that result
pub fn render(model: *const Model) -> RocList<Elem> {
    let closure_layout;
    let closure_data_buf;
    unsafe {
        closure_layout = Layout::array::<u8>(update_size() as usize).unwrap();
        closure_data_buf = std::alloc::alloc(closure_layout);
    }

    let elems = unsafe {
        let mut ret_val: MaybeUninit<RocList<Elem>> = MaybeUninit::uninit();
        
        let closure_data_buf =
            std::alloc::realloc(closure_data_buf, closure_layout, roc_render_size() as usize);

        call_render(model, closure_data_buf, ret_val.as_mut_ptr());

        std::alloc::dealloc(closure_data_buf, closure_layout);

        ret_val.assume_init()
    };

    elems
}
