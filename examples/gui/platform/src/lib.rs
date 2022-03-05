mod focus;
mod graphics;
mod gui;
mod roc;

use crate::roc::RocElem;
use core::alloc::Layout;
use core::mem::MaybeUninit;

extern "C" {
    #[link_name = "roc__programForHost_1_exposed_generic"]
    fn roc_program(args: State, output: *mut u8) -> ();

    #[link_name = "roc__programForHost_size"]
    fn roc_program_size() -> i64;

    #[link_name = "roc__programForHost_1_Render_caller"]
    fn call_Render(flags: *const u8, closure_data: *const u8, output: *mut u8) -> RocElem;

    #[allow(dead_code)]
    #[link_name = "roc__programForHost_1_Render_size"]
    fn size_Render() -> i64;

    #[link_name = "roc__programForHost_1_Render_result_size"]
    fn size_Render_result() -> i64;
}

#[repr(C)]
struct State {
    height: u32,
    width: u32,
}

#[no_mangle]
pub extern "C" fn rust_main() -> i32 {
    let size = unsafe { roc_program_size() } as usize;
    let layout = Layout::array::<u8>(size).unwrap();
    let state = State {
        height: 42,
        width: 123,
    };

    let root_elem = unsafe {
        // TODO allocate on the stack if it's under a certain size
        let buffer = std::alloc::alloc(layout);

        roc_program(state, buffer);

        // Call the program's render function
        let result = call_the_closure(buffer);

        std::alloc::dealloc(buffer, layout);

        result
    };

    gui::render("test title".into(), root_elem);

    // Exit code
    0
}

unsafe fn call_the_closure(closure_data_ptr: *const u8) -> RocElem {
    let size = size_Render_result() as usize;
    let layout = Layout::array::<u8>(size).unwrap();
    let buffer = std::alloc::alloc(layout) as *mut u8;

    let answer = call_Render(
        // This flags pointer will never get dereferenced
        MaybeUninit::uninit().as_ptr(),
        closure_data_ptr as *const u8,
        buffer as *mut u8,
    );

    std::alloc::dealloc(buffer, layout);

    answer
}
