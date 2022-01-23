#![allow(non_snake_case)]

use core::ffi::c_void;
use roc_std::RocStr;
use std::ffi::CStr;
use std::os::raw::c_char;

extern "C" {
    #[link_name = "roc__mainForHost_1_exposed"]
    fn roc_main() -> RocStr;
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

#[no_mangle]
pub extern "C" fn rust_main() -> i32 {
    unsafe {
        let roc_str = roc_main();

        let len = roc_str.len();
        let str_bytes = roc_str.get_bytes() as *const libc::c_void;

        if libc::write(1, str_bytes, len) < 0 {
            panic!("Writing to stdout failed!");
        }
        use winit::{
            event::{Event, WindowEvent},
            event_loop::{ControlFlow, EventLoop},
            window::WindowBuilder,
        };

        let event_loop = EventLoop::new();
        let window = WindowBuilder::new().build(&event_loop).unwrap();

        event_loop.run(move |event, _, control_flow| {
            // ControlFlow::Poll continuously runs the event loop, even if the OS hasn't
            // dispatched any events. This is ideal for games and similar applications.
            *control_flow = ControlFlow::Poll;

            // ControlFlow::Wait pauses the event loop if no events are available to process.
            // This is ideal for non-game applications that only update in response to user
            // input, and uses significantly less power/CPU time than ControlFlow::Poll.
            *control_flow = ControlFlow::Wait;

            match event {
                Event::WindowEvent {
                    event: WindowEvent::CloseRequested,
                    ..
                } => {
                    println!("The close button was pressed; stopping");
                    *control_flow = ControlFlow::Exit
                }
                Event::MainEventsCleared => {
                    // Application update code.

                    // Queue a RedrawRequested event.
                    //
                    // You only need to call this if you've determined that you need to redraw, in
                    // applications which do not always need to. Applications that redraw continuously
                    // can just render here instead.
                    window.request_redraw();
                }
                Event::RedrawRequested(_) => {
                    // Redraw the application.
                    //
                    // It's preferable for applications that do not render continuously to render in
                    // this event rather than in MainEventsCleared, since rendering in here allows
                    // the program to gracefully handle redraws requested by the OS.
                }
                _ => (),
            }
        });
    }

    // Exit code
    0
}
