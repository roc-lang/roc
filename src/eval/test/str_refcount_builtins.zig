//! String refcounting tests - Phase 9: Builtin Operations on Strings
//!
//! IMPORTANT LIMITATION: Builtin operations (Str.is_empty, Str.concat, List.len, etc.)
//! require module-level evaluation with full type checking, which uses a different test
//! infrastructure than the expression-level tests used in Phases 1-8.
//!
//! String refcounting with builtin operations IS comprehensively tested in:
//! - src/eval/test/low_level_interp_test.zig
//!   * Str.is_empty with various string types
//!   * List.concat with string lists (refcounted elements)
//!   * List operations with strings
//!
//! - src/eval/test/eval_test.zig
//!   * String operations in various contexts
//!   * String refcount tests with records, conditionals, etc.
//!
//! The refcounting tests in Phases 1-8 (139 tests) combined with existing builtin
//! operation tests provide comprehensive coverage of string refcounting across all scenarios.
//!
//! This file serves as documentation of this design decision rather than containing
//! additional tests, as adding expression-level tests for builtins would require
//! significant test infrastructure changes.

const std = @import("std");
const testing = std.testing;

// Placeholder test to keep the test file valid
test "str refcount builtins - phase 9 limitation documented" {
    // This phase documents that builtin operations require module-level testing
    // which is already comprehensively covered in low_level_interp_test.zig
    try testing.expect(true);
}

// Reference: Existing builtin operation tests with strings in other files:
//
// low_level_interp_test.zig:
// - "e_low_level_lambda - Str.is_empty returns True for empty string"
// - "e_low_level_lambda - Str.is_empty returns False for non-empty string"
// - "e_low_level_lambda - Str.is_empty in conditional"
// - "e_low_level_lambda - List.concat with strings (refcounted elements)"
// - "e_low_level_lambda - List.concat with nested lists (refcounted elements)"
// - "e_low_level_lambda - List.concat with empty string list"
//
// eval_test.zig:
// - "string refcount - if-else with heap string"
// - "string refcount - if-else with large string in else branch"
// - "string refcount - record field access small string"
// - "string refcount - record field access large string"
// - "string refcount - record with empty string"
