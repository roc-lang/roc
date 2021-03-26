#[macro_export]
macro_rules! assert_evals_to {
    ($src:expr, $expected:expr, $ty:ty) => {{
        assert_evals_to!($src, $expected, $ty, (|val| val));
    }};
    ($src:expr, $expected:expr, $ty:ty, $transform:expr) => {
        // Same as above, except with an additional transformation argument.
        {
            assert_evals_to!($src, $expected, $ty, $transform, true);
        }
    };
    ($src:expr, $expected:expr, $ty:ty, $transform:expr, $leak:expr) => {
        // Run both with and without lazy literal optimization.
        {
            assert_evals_to!($src, $expected, $ty, $transform, $leak, false);
        }
        {
            assert_evals_to!($src, $expected, $ty, $transform, $leak, true);
        }
    };
    ($src:expr, $expected:expr, $ty:ty, $transform:expr, $leak:expr, $lazy_literals:expr) => {
        use bumpalo::Bump;
        use roc_gen_dev::run_jit_function_raw;
        let stdlib = roc_builtins::std::standard_stdlib();

        let arena = Bump::new();
        let (main_fn_name, errors, lib) =
            $crate::helpers::eval::helper(&arena, $src, stdlib, $leak, $lazy_literals);

        let transform = |success| {
            let expected = $expected;
            let given = $transform(success);
            assert_eq!(&given, &expected);
        };
        run_jit_function_raw!(lib, main_fn_name, $ty, transform, errors)
    };
}
