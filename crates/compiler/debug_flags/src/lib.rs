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
//!         # ...other flags
//!   cargo"
//! ```
//!
//! Now you can turn debug flags on and off as you like.
//!
//! These flags are also set in .cargo/config found at the repository root. You can modify them
//! there to avoid maintaining a separate script.

#[macro_export]
macro_rules! dbg_set {
    ($flag:path) => {{
        #[cfg(not(debug_assertions))]
        {
            false
        }
        #[cfg(debug_assertions)]
        {
            let flag = std::env::var($flag);
            flag.is_ok() && flag.as_deref() != Ok("0")
        }
    }};
}

#[macro_export]
macro_rules! dbg_do {
    ($flag:path, $expr:expr) => {
        #[cfg(debug_assertions)]
        {
            if $crate::dbg_set!($flag) {
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
    /// Only use this in single-threaded mode!
    ROC_PRINT_UNIFICATIONS

    /// Prints types whose ability impls failed to be derived.
    ROC_PRINT_UNDERIVABLE

    /// Prints traces of unspecialized lambda set compaction
    ROC_TRACE_COMPACTION

    /// Like ROC_PRINT_UNIFICATIONS, in the context of typechecking derived implementations.
    /// Only use this in single-threaded mode!
    ROC_PRINT_UNIFICATIONS_DERIVED

    /// Prints all type mismatches hit during type unification.
    ROC_PRINT_MISMATCHES

    /// Verifies that after let-generalization of a def, any rigid variables in the type annotation
    /// of the def are indeed generalized.
    ///
    /// Note that rigids need not always be generalized in a def. For example, they may be
    /// constrained by a type from a lower rank, as `b` is in the following def:
    ///
    ///   F a : { foo : a }
    ///   foo = \arg ->
    ///     x : F b
    ///     x = arg
    ///     x.foo
    ///
    /// Instead, this flag is useful for checking that in general, introduction is correct, when
    /// chainging how defs are constrained.
    ROC_VERIFY_RIGID_LET_GENERALIZED

    /// Verifies that an `occurs` check indeed only contains non-recursive types that need to be
    /// fixed-up.
    ///
    /// This flag is disabled by default because an occurs check may pass through an inferred
    /// partially-recursive structure if a part of that structure also has type errors. However, in
    /// the presence of programs without type errors, occurs checks should always consist of only
    /// non-recursive types, and this flag should pass.
    ROC_VERIFY_OCCURS_RECURSION

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

    /// Print to stderr when a runtime error function is generated.
    ROC_PRINT_RUNTIME_ERROR_GEN

    /// Generate a layout error when an unbound type variable is found, rather than generating the
    /// void layout.
    ROC_NO_UNBOUND_LAYOUT

    // ===LLVM Gen===

    /// Prints LLVM function verification output.
    ROC_PRINT_LLVM_FN_VERIFICATION

    // ===WASM Gen===

    /// Writes a `final.wasm` file to /tmp
    ROC_WRITE_FINAL_WASM

    // ===Load===

    /// Print load phases as they complete.
    ROC_PRINT_LOAD_LOG

    /// Don't build and use the subs cache (speeds up compilation of load and previous crates)
    ROC_SKIP_SUBS_CACHE
}
