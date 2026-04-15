//! Scaffolding for structural ownership-boundary grep invariants.
//!
//! This suite will later run the repository boundary checker and assert that
//! ordinary interpreter/backend code paths do not perform ownership-sensitive
//! work outside the explicit LIR RC choke points.
//!
//! Target invariants for the real test body:
//! - ordinary code may not call raw RC engines directly
//! - ordinary code may not query refcounted-ness directly
//! - only explicit-RC and builtin-internal wrappers may reach raw RC emitters
//! - forbidden aggregate-coercion islands stay isolated until deleted

const std = @import("std");

test "ownership boundary grep invariants scaffold" {
    _ = std.testing;
}
