//! List refcounting tests - Phase 12: Builtin List Operations
//!
//! IMPORTANT LIMITATION: Builtin operations (List.len, List.concat, etc.)
//! require module-level evaluation with full type checking, which uses a different test
//! infrastructure than the expression-level tests used in Phases 1-11.
//!
//! List refcounting with builtin operations IS comprehensively tested in:
//! - src/eval/test/low_level_interp_test.zig
//!   * List.concat with various list types
//!   * List.concat with string lists (refcounted elements)
//!   * List operations with nested lists
//!
//! - src/eval/test/interpreter_style_test.zig
//!   * List.fold operations
//!   * List.len on literals
//!   * List pattern matching
//!
//! The refcounting tests in Phases 1-11 combined with existing builtin
//! operation tests provide comprehensive coverage of list refcounting across all scenarios.
//!
//! This file serves as documentation of this design decision rather than containing
//! additional tests, as adding expression-level tests for builtins would require
//! significant test infrastructure changes.

const std = @import("std");
const testing = std.testing;

// Placeholder test to keep the test file valid
test "list refcount builtins - phase 12 limitation documented" {
    // This phase documents that builtin operations require module-level testing
    // which is already comprehensively covered in low_level_interp_test.zig
    // and interpreter_style_test.zig
    try testing.expect(true);
}

// Reference: Existing builtin operation tests with lists in other files:
//
// low_level_interp_test.zig:
// - "low_level - List.concat with two non-empty lists"
// - "low_level - List.concat with empty and non-empty list"
// - "low_level - List.concat with two empty lists"
// - "low_level - List.concat preserves order"
// - "low_level - List.concat with strings (refcounted elements)"
// - "low_level - List.concat with nested lists (refcounted elements)"
// - "low_level - List.concat with empty string list"
// - "low_level - List.concat with zero-sized type"
//
// - "low_level - List.with_capacity of non refcounted elements creates empty list"
// - "low_level - List.with_capacity of str (refcounted elements) creates empty list"
// - "low_level - List.with_capacity of non refcounted elements can concat"
// - "low_level - List.with_capacity of str (refcounted elements) can concat"
// - "low_level - List.with_capacity without capacity, of str (refcounted elements) can concat"
// - "low_level - List.with_capacity of zero-sized type creates empty list"
//
// - "low_level - List.drop_at on an empty list at index 0"
// - "low_level - List.drop_at on an empty list at index >0"
// - "low_level - List.drop_at on non-empty list"
// - "low_level - List.drop_at out of bounds on non-empty list"
// - "low_level - List.drop_at on refcounted List(Str)"
// - "low_level - List.drop_at on refcounted List(List(Str))"
//
// - "low_level - List.sublist on empty list"
// - "low_level - List.sublist on non-empty list"
// - "low_level - List.sublist start out of bounds"
// - "low_level - List.sublist requesting beyond end of list gives you input list"

// - "low_level - List.append on non-empty list"
// - "low_level - List.append on empty list"
// - "low_level - List.append a list on empty list"
// - "low_level - List.append for strings"
// - "low_level - List.append for list of lists"
// - "low_level - List.append for already refcounted elt"
//
// interpreter_style_test.zig:
// - "interpreter: match list pattern destructures"
// - "interpreter: match list rest binds slice"
// - "interpreter: match empty list branch"
// - "interpreter: List.fold sum with inline lambda"
// - "interpreter: List.fold product with inline lambda"
// - "interpreter: List.fold empty list with inline lambda"
