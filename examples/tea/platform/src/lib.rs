#![allow(non_snake_case)]

use roc_std::alloca;
use roc_std::RocCallResult;
use roc_std::RocStr;
use std::alloc::Layout;
use std::time::SystemTime;

type Msg = RocStr;
type Model = i64;

extern "C" {
    #[link_name = "roc__mainForHost_1_exposed"]
    fn roc_main(output: *mut u8) -> ();

    #[link_name = "roc__mainForHost_1_size"]
    fn roc_main_size() -> i64;

    #[link_name = "roc__mainForHost_1_Init_caller"]
    fn _call_Init(
        flags: &(),
        function_pointer: *const u8,
        closure_data: *const u8,
        output: *mut u8,
    ) -> ();

    #[link_name = "roc__mainForHost_1_Init_size"]
    fn size_Init() -> i64;

    #[link_name = "roc__mainForHost_1_Init_result_size"]
    fn size_Init_result() -> i64;

    #[link_name = "roc__mainForHost_1_Update_caller"]
    fn call_Update(
        msg: &Msg,
        model: &Model,
        function_pointer: *const u8,
        closure_data: *const u8,
        output: *mut u8,
    ) -> ();

    #[link_name = "roc__mainForHost_1_Update_size"]
    fn size_Update() -> i64;

    #[link_name = "roc__mainForHost_1_Update_result_size"]
    fn size_Update_result() -> i64;

    #[link_name = "roc__mainForHost_1_Fx_caller"]
    fn _call_Fx(
        unit: &(),
        function_pointer: *const u8,
        closure_data: *const u8,
        output: *mut u8,
    ) -> ();

    #[link_name = "roc__mainForHost_1_Fx_size"]
    fn size_Fx() -> i64;

    #[link_name = "roc__mainForHost_1_Fx_result_size"]
    fn size_Fx_result() -> i64;

    #[link_name = "roc__mainForHost_1_Model_size"]
    fn size_Model() -> i64;
}

unsafe fn call_Fx(function_pointer: *const u8, closure_data: *const u8, output: *mut u8) -> () {
    // Fx (or Cmd on the roc side) is a thunk, so we know its first argument is a (pointer to) unit
    _call_Fx(&(), function_pointer, closure_data, output)
}

unsafe fn call_Init(function_pointer: *const u8, closure_data: *const u8, output: *mut u8) -> () {
    // for now, we hardcode flags to be `()` (or `{}` on the roc side)
    let flags = ();
    _call_Init(&flags, function_pointer, closure_data, output)
}

#[no_mangle]
pub fn roc_fx_putChar(foo: i64) -> () {
    let character = foo as u8 as char;
    print!("{}", character);

    ()
}

#[no_mangle]
pub fn roc_fx_putLine(line: RocStr) -> () {
    let bytes = line.as_slice();
    let string = unsafe { std::str::from_utf8_unchecked(bytes) };
    println!("{}", string);

    ()
}

#[no_mangle]
pub fn roc_fx_getLine() -> RocStr {
    use std::io::{self, BufRead};

    let stdin = io::stdin();
    let line1 = stdin.lock().lines().next().unwrap().unwrap();

    RocStr::from_slice_with_capacity(line1.as_bytes(), line1.len())
}

unsafe fn run_fx(function_pointer: *const u8, closure_data_ptr: *const u8) -> Msg {
    let size = size_Fx_result() as usize;

    alloca::with_stack_bytes(size, |buffer| {
        let buffer: *mut std::ffi::c_void = buffer;
        let buffer: *mut u8 = buffer as *mut u8;

        call_Fx(
            function_pointer,
            closure_data_ptr as *const u8,
            buffer as *mut u8,
        );

        let output = &*(buffer as *mut RocCallResult<()>);

        match output.into() {
            Ok(()) => {
                let mut bytes = *(buffer.add(8) as *const (u64, u64));
                let msg = std::mem::transmute::<(u64, u64), RocStr>(bytes);

                msg
            }

            Err(e) => panic!("failed with {}", e),
        }
    })
}

unsafe fn run_init(function_pointer: *const u8, closure_data_ptr: *const u8) -> (Model, Msg) {
    let size = size_Init_result() as usize;

    alloca::with_stack_bytes(size, |buffer| {
        let buffer: *mut std::ffi::c_void = buffer;
        let buffer: *mut u8 = buffer as *mut u8;

        call_Init(function_pointer, 0 as *const u8, buffer as *mut u8);

        // cmd < model, so the command comes first
        let output = &*(buffer as *mut RocCallResult<()>);

        match output.into() {
            Ok(_) => {
                let offset = 8 + size_Fx();
                let model_ptr = buffer.add(offset as usize);
                let model: i64 = *(model_ptr as *const i64);

                let cmd_fn_ptr_ptr = buffer.add(8) as *const i64;
                let cmd_fn_ptr = (*cmd_fn_ptr_ptr) as *const u8;
                let cmd_closure_data_ptr = buffer.add(16);

                let msg = run_fx(cmd_fn_ptr, cmd_closure_data_ptr);

                (model, msg)
            }

            Err(e) => panic!("failed with {}", e),
        }
    })
}

unsafe fn run_update(
    msg: RocStr,
    model: Model,
    function_pointer: *const u8,
    closure_data_ptr: *const u8,
) -> (Model, Msg) {
    let size = size_Update_result() as usize;

    alloca::with_stack_bytes(size, |buffer| {
        let buffer: *mut std::ffi::c_void = buffer;
        let buffer: *mut u8 = buffer as *mut u8;

        println!("let's try update!");

        call_Update(
            &msg,
            &model,
            function_pointer,
            closure_data_ptr,
            buffer as *mut u8,
        );

        // cmd < model, so the command comes first
        let output = &*(buffer as *mut RocCallResult<()>);

        match output.into() {
            Ok(_) => {
                let offset = 8 + size_Fx();
                let model_ptr = buffer.add(offset as usize);
                let model: i64 = *(model_ptr as *const i64);

                let cmd_fn_ptr_ptr = buffer.add(8) as *const i64;
                let cmd_fn_ptr = (*cmd_fn_ptr_ptr) as *const u8;
                let cmd_closure_data_ptr = buffer.add(16);

                let msg = run_fx(cmd_fn_ptr, cmd_closure_data_ptr);

                (model, msg)
            }

            Err(e) => panic!("failed with {}", e),
        }
    })
}

#[no_mangle]
pub fn rust_main() -> isize {
    let start_time = SystemTime::now();

    let size = unsafe { roc_main_size() } as usize;
    let layout = Layout::array::<u8>(size).unwrap();
    let answer = unsafe {
        let buffer = std::alloc::alloc(layout);

        roc_main(buffer);

        let output = &*(buffer as *mut RocCallResult<(*const u8, *const u8)>);

        match output.into() {
            Ok((init_fn_ptr, update_fn_ptr)) => {
                // let closure_data_ptr = buffer.offset(16);
                let closure_data_ptr = 0 as *const u8;

                let (mut model, mut msg) =
                    run_init(init_fn_ptr as *const u8, closure_data_ptr as *const u8);

                for _ in 0..5 {
                    let result = run_update(
                        msg,
                        model,
                        update_fn_ptr as *const u8,
                        closure_data_ptr as *const u8,
                    );

                    model = result.0;
                    msg = result.1;
                }

                std::alloc::dealloc(buffer, layout);

                model
            }
            Err(msg) => {
                std::alloc::dealloc(buffer, layout);

                panic!("Roc failed with message: {}", msg);
            }
        }
    };
    let end_time = SystemTime::now();
    let duration = end_time.duration_since(start_time).unwrap();

    println!(
        "Roc closure took {:.4} ms to compute this answer: {:?}",
        duration.as_secs_f64() * 1000.0,
        // truncate the answer, so stdout is not swamped
        answer
    );

    // Exit code
    0
}

/*
#[no_mangle]
pub fn old_rust_main() -> isize {
    println!("Running Roc closure");
    let start_time = SystemTime::now();

    let size = unsafe { roc_main_size() } as usize;
    let layout = Layout::array::<u8>(size).unwrap();
    let answer = unsafe {
        let buffer = std::alloc::alloc(layout);

        roc_main(buffer);

        let output = &*(buffer as *mut RocCallResult<()>);

        match output.into() {
            Ok(()) => {
                let function_pointer = {
                    // this is a pointer to the location where the function pointer is stored
                    // we pass just the function pointer
                    let temp = buffer.offset(8) as *const i64;

                    (*temp) as *const u8
                };

                let closure_data_ptr = buffer.offset(16);

                let result =
                    call_the_closure(function_pointer as *const u8, closure_data_ptr as *const u8);

                std::alloc::dealloc(buffer, layout);

                result
            }
            Err(msg) => {
                std::alloc::dealloc(buffer, layout);

                panic!("Roc failed with message: {}", msg);
            }
        }
    };
    let end_time = SystemTime::now();
    let duration = end_time.duration_since(start_time).unwrap();

    println!(
        "Roc closure took {:.4} ms to compute this answer: {:?}",
        duration.as_secs_f64() * 1000.0,
        // truncate the answer, so stdout is not swamped
        answer
    );

    // Exit code
    0
}
*/
