#![warn(clippy::dbg_macro)]
// See github.com/rtfeldman/roc/issues/800 for discussion of the large_enum_variant check.
#![allow(clippy::large_enum_variant)]

pub mod error;
pub mod report;

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
        eprintln!("Please file an issue here: https://github.com/rtfeldman/roc/issues/new/choose");
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
