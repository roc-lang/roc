#![allow(non_snake_case)]

use roc_std::RocCallResult;
use roc_std::RocStr;
use std::alloc::Layout;
use std::ffi::c_void;
use std::time::SystemTime;

type Model = *const u8;

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
        msg: Msg,
        model: Model,
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

    fn malloc(size: usize) -> *mut c_void;
    fn realloc(c_ptr: *mut c_void, size: usize) -> *mut c_void;
    fn free(c_ptr: *mut c_void);
}

#[no_mangle]
pub unsafe fn roc_alloc(_alignment: usize, size: usize) -> *mut c_void {
    return malloc(size);
}

#[no_mangle]
pub unsafe fn roc_realloc(
    _alignment: usize,
    c_ptr: *mut c_void,
    _old_size: usize,
    new_size: usize,
) -> *mut c_void {
    return realloc(c_ptr, new_size);
}

#[no_mangle]
pub unsafe fn roc_dealloc(_alignment: usize, c_ptr: *mut c_void) {
    return free(c_ptr);
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
    println!("{}", unsafe { line.as_str() });

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

    let layout = Layout::array::<u8>(size).unwrap();
    let buffer = std::alloc::alloc(layout);

    call_Fx(
        function_pointer,
        closure_data_ptr as *const u8,
        buffer as *mut u8,
    );

    let output = &*(buffer as *mut RocCallResult<()>);

    match output.into() {
        Ok(()) => Msg { msg: buffer.add(8) },

        Err(e) => panic!("failed with {}", e),
    }
}

struct Msg {
    msg: *mut u8,
}

impl Msg {
    unsafe fn alloc(size: usize) -> Self {
        let size = size_Fx_result() as usize;
        let layout = Layout::array::<u8>(size).unwrap();
        let msg = std::alloc::alloc(layout);

        Self { msg }
    }
}

impl Drop for Msg {
    fn drop(&mut self) {
        unsafe {
            let size = size_Fx_result() as usize;
            let layout = Layout::array::<u8>(size).unwrap();
            std::alloc::dealloc(self.msg.offset(-8), layout);
        }
    }
}

struct ModelCmd {
    buffer: *mut u8,
    cmd_fn_ptr_ptr: *const u8,
    cmd_closure_data_ptr: *const u8,
    model: *const u8,
}

impl ModelCmd {
    unsafe fn alloc() -> Self {
        let size = 8 + size_Fx() as usize + size_Model() as usize;

        let layout = Layout::array::<u8>(size).unwrap();
        let buffer = std::alloc::alloc(layout);

        let cmd_fn_ptr_ptr = buffer.add(8);
        let cmd_closure_data_ptr = buffer.add(8 + 8);
        let model = buffer.add(8 + size_Fx() as usize);

        Self {
            buffer,
            cmd_fn_ptr_ptr,
            cmd_closure_data_ptr,
            model,
        }
    }
}

impl Drop for ModelCmd {
    fn drop(&mut self) {
        unsafe {
            let size = 8 + size_Fx() as usize + size_Model() as usize;
            let layout = Layout::array::<u8>(size).unwrap();
            std::alloc::dealloc(self.buffer, layout);
        }
    }
}

unsafe fn run_init(
    function_pointer: *const u8,
    closure_data_ptr: *const u8,
) -> Result<ModelCmd, String> {
    debug_assert_eq!(size_Init_result(), 8 + size_Fx() + size_Model());
    let size = size_Init_result() as usize;

    let model_cmd = ModelCmd::alloc();
    let buffer = model_cmd.buffer;

    call_Init(function_pointer, 0 as *const u8, buffer as *mut u8);

    // cmd < model, so the command comes first
    let output = &*(buffer as *mut RocCallResult<()>);

    match output.into() {
        Ok(_) => Ok(model_cmd),
        Err(e) => Err(e.to_string()),
    }
}

unsafe fn run_update(
    msg: Msg,
    model: Model,
    function_pointer: *const u8,
    closure_data_ptr: *const u8,
) -> Result<ModelCmd, String> {
    debug_assert_eq!(size_Update_result(), 8 + size_Fx() + size_Model());
    let size = size_Update_result() as usize;

    let model_cmd = ModelCmd::alloc();
    let buffer = model_cmd.buffer;

    call_Update(
        msg,
        model,
        function_pointer,
        closure_data_ptr,
        buffer as *mut u8,
    );

    // cmd < model, so the command comes first
    let output = &*(buffer as *mut RocCallResult<()>);

    match output.into() {
        Ok(_) => Ok(model_cmd),
        Err(e) => Err(e.to_string()),
    }
}

fn run_roc() -> Result<(), String> {
    let size = unsafe { roc_main_size() } as usize;
    let layout = Layout::array::<u8>(size).unwrap();
    unsafe {
        let buffer = std::alloc::alloc(layout);

        roc_main(buffer);

        let output = &*(buffer as *mut RocCallResult<(*const u8, *const u8)>);

        match output.into() {
            Ok((init_fn_ptr, update_fn_ptr)) => {
                //let closure_data_ptr = buffer.offset(16);
                let closure_data_ptr = 0 as *const u8;

                let model_cmd =
                    &mut run_init(init_fn_ptr as *const u8, closure_data_ptr as *const u8).unwrap();

                for _ in 0..5 {
                    let model = model_cmd.model;
                    let cmd_fn_ptr = *(model_cmd.cmd_fn_ptr_ptr as *const usize) as *const u8;
                    let msg = run_fx(cmd_fn_ptr, model_cmd.cmd_closure_data_ptr);

                    let mut result = run_update(
                        msg,
                        model,
                        update_fn_ptr as *const u8,
                        closure_data_ptr as *const u8,
                    )
                    .unwrap();

                    std::mem::swap(model_cmd, &mut result);

                    // implictly drops `result` and `msg`
                }

                std::alloc::dealloc(buffer, layout);
                Ok(())
            }
            Err(msg) => {
                std::alloc::dealloc(buffer, layout);

                Err(msg.to_string())
            }
        }
    }
}

#[no_mangle]
pub fn rust_main() -> isize {
    let start_time = SystemTime::now();

    let end_time = SystemTime::now();
    let duration = end_time.duration_since(start_time).unwrap();

    match run_roc() {
        Ok(answer) => {
            println!(
                "Roc closure took {:.4} ms to compute this answer: {:?}",
                duration.as_secs_f64() * 1000.0,
                answer
            );
        }
        Err(e) => {
            eprintln!("Roc failed with message {:?}", e);
        }
    }

    // Exit code
    0
}
