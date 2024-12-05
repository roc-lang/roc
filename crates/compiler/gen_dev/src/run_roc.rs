#[macro_export]
/// run_jit_function_raw runs an unwrapped jit function.
/// The function could throw an exception and break things, or worse, it could not throw an exception and break things.
/// This functions is generally a bad idea with an untrused backend, but is being used for now for development purposes.
macro_rules! run_jit_function_raw {
    ($lib: expr, $main_fn_name: expr, $ty:ty, $transform:expr) => {{
        let v: std::vec::Vec<roc_problem::can::Problem> = std::vec::Vec::new();
        run_jit_function_raw!($lib, $main_fn_name, $ty, $transform, v)
    }};

    ($lib: expr, $main_fn_name: expr, $ty:ty, $transform:expr, $errors:expr) => {{
        unsafe {
            let main: libloading::Symbol<unsafe extern "C" fn() -> $ty> = $lib
                .get($main_fn_name.as_bytes())
                .ok()
                .ok_or(format!("Unable to JIT compile `{}`", $main_fn_name))
                .expect("errored");

            let result = main();

            if !$errors.is_empty() {
                eprintln!("{:?}", &$errors);

                assert_eq!(
                    $errors,
                    std::vec::Vec::new(),
                    "Encountered errors: {:?}",
                    $errors
                );
            }

            $transform(result)
        }
    }};
}
