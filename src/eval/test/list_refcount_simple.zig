//! List refcounting tests - Phase 1: MINIMAL
//!
//! These tests verify the most fundamental list operations with integer elements.
//! Starting with integers (non-refcounted) isolates list container refcounting
//! from element refcounting complexity.
//!
//! Each test should pass with correct refcounting (no leaks, no corruption)

const std = @import("std");
const helpers = @import("helpers.zig");
const testing = std.testing;

const runExpectI64 = helpers.runExpectI64;

test "list refcount minimal - empty list pattern match" {
    // Most basic test: create an empty list and match it
    try runExpectI64(
        \\match [] { [] => 42, _ => 0 }
    , 42, .no_trace);
}

test "list refcount minimal - single element list pattern match" {
    // Single element list - match and extract
    try runExpectI64(
        \\match [1] { [x] => x, _ => 0 }
    , 1, .no_trace);
}

test "list refcount minimal - multi-element list pattern match" {
    // Multiple elements - match and sum
    try runExpectI64(
        \\match [1, 2, 3] { [a, b, c] => a + b + c, _ => 0 }
    , 6, .no_trace);
}
