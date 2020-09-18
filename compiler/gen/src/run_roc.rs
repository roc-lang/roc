use std::ffi::CString;
use std::os::raw::c_char;

#[repr(C)]
union Payload<T: Copy> {
    success: T,
    failure: *mut c_char,
}

#[repr(C)]
pub struct RocCallResult<T: Copy> {
    pub flag: u64,
    payload: Payload<T>,
}

impl<T: Copy> Into<Result<T, String>> for RocCallResult<T> {
    fn into(self) -> Result<T, String> {
        if self.flag == 0 {
            Ok(unsafe { self.payload.success })
        } else {
            Err(unsafe {
                let raw = CString::from_raw(self.payload.failure);

                let result = format!("{:?}", raw);

                std::mem::forget(raw);

                result
            })
        }
    }
}

#[macro_export]
macro_rules! run_jit_function {
    ($execution_engine: expr, $main_fn_name: expr, $ty:ty, $transform:expr) => {{
        let v: std::vec::Vec<roc_problem::can::Problem> = std::vec::Vec::new();
        run_jit_function!($execution_engine, $main_fn_name, $ty, $transform, v)
    }};

    ($execution_engine: expr, $main_fn_name: expr, $ty:ty, $transform:expr, $errors:expr) => {{
        use inkwell::context::Context;
        use inkwell::execution_engine::JitFunction;
        use roc_gen::run_roc::RocCallResult;

        unsafe {
            let main: JitFunction<unsafe extern "C" fn() -> RocCallResult<$ty>> = $execution_engine
                .get_function($main_fn_name)
                .ok()
                .ok_or(format!("Unable to JIT compile `{}`", $main_fn_name))
                .expect("errored");

            match main.call().into() {
                Ok(success) => {
                    // only if there are no exceptions thrown, check for errors
                    assert_eq!(
                        $errors,
                        std::vec::Vec::new(),
                        "Encountered errors: {:?}",
                        $errors
                    );

                    $transform(success)
                }
                Err(error_msg) => panic!("Roc failed with message: {}", error_msg),
            }
        }
    }};
}

/// In the repl, we don't know the type that is returned; it it's large enough to not fit in 2
/// registers (i.e. size bigger than 16 bytes on 64-bit systems), then we use this macro.
/// It explicitly allocates a buffer that the roc main function can write its result into.
#[macro_export]
macro_rules! run_jit_function_dynamic_type {
    ($execution_engine: expr, $main_fn_name: expr, $bytes:expr, $transform:expr) => {{
        let v: std::vec::Vec<roc_problem::can::Problem> = std::vec::Vec::new();
        run_jit_function_dynamic_type!($execution_engine, $main_fn_name, $bytes, $transform, v)
    }};

    ($execution_engine: expr, $main_fn_name: expr, $bytes:expr, $transform:expr, $errors:expr) => {{
        use inkwell::context::Context;
        use inkwell::execution_engine::JitFunction;
        use roc_gen::run_roc::RocCallResult;

        unsafe {
            let main: JitFunction<unsafe extern "C" fn(*const u8)> = $execution_engine
                .get_function($main_fn_name)
                .ok()
                .ok_or(format!("Unable to JIT compile `{}`", $main_fn_name))
                .expect("errored");

            let layout = std::alloc::Layout::array::<u8>($bytes).unwrap();
            let result = std::alloc::alloc(layout);
            main.call(result);

            let flag = *result;

            if flag == 0 {
                $transform(result.offset(8) as *const u8)
            } else {
                use std::ffi::CString;
                use std::os::raw::c_char;

                let ptr_as_int = *(result as *const u64).offset(1);
                let ptr = std::mem::transmute::<u64, *mut c_char>(ptr_as_int);

                let raw = CString::from_raw(ptr);

                let result = format!("{:?}", raw);

                std::mem::forget(raw);

                eprintln!("{}", result);
                panic!("Roc hit an error");
            }
        }
    }};
}
