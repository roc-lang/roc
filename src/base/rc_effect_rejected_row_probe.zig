//! Compile-failure probe for the `RcEffect` structural validator.
//!
//! This file must NOT compile. `zig build run-test-rc-effect-row-rejected`
//! builds it and requires the compile to fail with the rule the row breaks, so
//! the guarantee "a wrong row is a compile error" is itself tested rather than
//! assumed.
//!
//! The row below is the one PR roc-lang/roc#10023 removed: `str_drop_prefix`
//! returns a slice of its argument's allocation, so claiming `result_unique`
//! alongside `result_shares_args` made ARC count a fresh birth on top of a
//! link to the lender, leaking one reference per call.

const base = @import("base");

const rc_effect_rules = base.rc_effect_rules;
const RcEffect = base.LowLevel.RcEffect;

comptime {
    var reintroduced = RcEffect.retainsSharingArgs(1);
    reintroduced.result_unique = true;
    rc_effect_rules.assertRowConforms("str_drop_prefix", reintroduced);
}
