/// `internal_error!` should be used whenever a compiler invariant is broken.
/// It is a wrapper around panic that tells the user to file a bug.
/// This should only be used in cases where there would be a compiler bug and the user can't fix it.
/// If there is simply an unimplemented feature, please use `unimplemented!`
/// If there is a user error, please use roc_reporting to print a nice error message.
#[macro_export]
macro_rules! internal_error {
    ($($arg:tt)*) => ({
        eprintln!("An internal compiler expectation was broken.");
        eprintln!("This is definitely a compiler bug.");
        // TODO: update this to the new bug template.
        eprintln!("Please file an issue here: https://github.com/roc-lang/roc/issues/new/choose");
        #[allow(clippy::panic)] {
            panic!($($arg)*);
        }
    })
}

/// `user_error!` should only ever be used temporarily.
/// It is a way to document locations where we do not yet have nice error reporting.
/// All cases of `user_error!` should eventually be replaced with pretty error printing using roc_reporting.
#[macro_export]
macro_rules! user_error {
    ($($arg:tt)*) => ({
        eprintln!("We ran into an issue while compiling your code.");
        eprintln!("Sadly, we don't havs a pretty error message for this case yet.");
        eprintln!("If you can't figure out the problem from the context below, please reach out at: https://roc.zulipchat.com/");
        eprintln!($($arg)*);
        std::process::exit(1);
    })
}

/// Assert that a type has the expected size on ARM
#[macro_export]
macro_rules! assert_sizeof_aarch64 {
    ($t: ty, $expected_size: expr) => {
        #[cfg(target_arch = "aarch64")]
        static_assertions::assert_eq_size!($t, [u8; $expected_size]);
    };
}

/// Assert that a type has the expected size in Wasm
#[macro_export]
macro_rules! assert_sizeof_wasm {
    ($t: ty, $expected_size: expr) => {
        #[cfg(target_family = "wasm")]
        static_assertions::assert_eq_size!($t, [u8; $expected_size]);
    };
}

/// Assert that a type has the expected size on any target not covered above
/// In practice we use this for x86_64, and add specific macros for other targets
#[macro_export]
macro_rules! assert_sizeof_default {
    ($t: ty, $expected_size: expr) => {
        #[cfg(not(any(target_family = "wasm", target_arch = "aarch64")))]
        static_assertions::assert_eq_size!($t, [u8; $expected_size]);
    };
}

/// Assert that a type has the expected size on all targets
#[macro_export]
macro_rules! assert_sizeof_all {
    ($t: ty, $expected_size: expr) => {
        static_assertions::assert_eq_size!($t, [u8; $expected_size]);
    };
}

/// Assert that a type has the expected size on all targets except wasm
#[macro_export]
macro_rules! assert_sizeof_non_wasm {
    ($t: ty, $expected_size: expr) => {
        #[cfg(not(target_family = "wasm"))]
        static_assertions::assert_eq_size!($t, [u8; $expected_size]);
    };
}

/// Assert that a type has `Copy`
#[macro_export]
macro_rules! assert_copyable {
    ($t: ty) => {
        static_assertions::assert_impl_all!($t: Copy);
    };
}

// LARGE SCALE PROJECTS
//
// This section is for "todo!"-style macros enabled in sections where large-scale changes to the
// language are in progress.

#[macro_export]
macro_rules! _incomplete_project {
    ($project_name:literal, $tracking_issue_no:literal) => {
        panic!(
            "[{}] not yet implemented. Tracking issue: https://github.com/roc-lang/roc/issues/{}",
            $project_name, $tracking_issue_no,
        )
    };
    ($project_name:literal, $tracking_issue_no:literal, $($arg:tt)+) => {
        panic!(
            "[{}] not yet implemented. Tracking issue: https://github.com/roc-lang/roc/issues/{}.\nAdditional information: {}",
            $project_name, $tracking_issue_no,
            format_args!($($arg)+),
        )
    };
}

#[macro_export]
macro_rules! todo_abilities {
    () => {
        $crate::_incomplete_project!("Abilities", 2463)
    };
    ($($arg:tt)+) => {
        $crate::_incomplete_project!("Abilities", 2463, $($arg)+)
    };
}

// END LARGE SCALE PROJECTS
