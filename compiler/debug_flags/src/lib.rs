//! Flags for debugging the Roc compiler.
//!
//! Lists environment variable flags that can be enabled for verbose debugging features in debug
//! builds of the compiler.
//!
//! For example, I might define the following alias to run cargo with all unifications and
//! expanded type aliases printed:
//!
//! ```bash
//! alias cargo="\
//!            ROC_PRINT_UNIFICATIONS=1 \
//!   ROC_PRETTY_PRINT_ALIAS_CONTENTS=1 \
//!   cargo"
//! ```
//!
//! More generally, I have the following:
//!
//! ```bash
//! alias cargo="\
//!     ROC_PRETTY_PRINT_ALIAS_CONTENTS=0 \
//!              ROC_PRINT_UNIFICATIONS=0 \
//!                ROC_PRINT_MISMATCHES=0 \
//!   ROC_PRINT_IR_AFTER_SPECIALIZATION=0 \
//!      ROC_PRINT_IR_AFTER_RESET_REUSE=0 \
//!         ROC_PRINT_IR_AFTER_REFCOUNT=0 \
//!         ROC_PRETTY_PRINT_IR_SYMBOLS=0 \
//!   cargo"
//! ```
//!
//! Now you can turn debug flags on and off as you like.

#[macro_export]
macro_rules! dbg_do {
    ($flag:path, $expr:expr) => {
        #[cfg(debug_assertions)]
        {
            let flag = std::env::var($flag);
            if !flag.is_err() && flag.as_deref() != Ok("0") {
                $expr
            }
        }
    };
}

macro_rules! flags {
    ($($(#[doc = $doc:expr])+ $flag:ident)*) => {$(
        $(#[doc = $doc])+
        pub static $flag: &str = stringify!($flag);
    )*};
}

flags! {
    // ===Types===

    /// Expands the contents of aliases during pretty-printing of types.
    ROC_PRETTY_PRINT_ALIAS_CONTENTS

    // ===Solve===

    /// Prints type unifications, before and after they happen.
    ROC_PRINT_UNIFICATIONS

    /// Prints all type mismatches hit during type unification.
    ROC_PRINT_MISMATCHES

    // ===Mono===

    /// Writes a pretty-printed mono IR to stderr after function specialization.
    ROC_PRINT_IR_AFTER_SPECIALIZATION

    /// Writes a pretty-printed mono IR to stderr after insertion of reset/reuse
    /// instructions.
    ROC_PRINT_IR_AFTER_RESET_REUSE

    /// Writes a pretty-printed mono IR to stderr after insertion of refcount
    /// instructions.
    ROC_PRINT_IR_AFTER_REFCOUNT

    /// Prints debug information during the alias analysis pass.
    ROC_DEBUG_ALIAS_ANALYSIS

    // ===LLVM Gen===

    /// Prints LLVM function verification output.
    ROC_PRINT_LLVM_FN_VERIFICATION

    // ===Load===

    /// Print load phases as they complete.
    ROC_PRINT_LOAD_LOG
}
