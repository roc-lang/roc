//! List refcounting tests - Phase 2: Aliases and References
//!
//! These tests verify list container refcounting when lists are aliased or referenced
//! multiple times. Still using integer elements to isolate list container refcounting.
//!
//! Each test should pass with correct refcounting (no leaks, no corruption)

const helpers = @import("helpers.zig");

const runExpectI64 = helpers.runExpectI64;

test "list refcount alias - variable aliasing" {
    // Alias a list to another variable and return the alias
    try runExpectI64(
        \\{
        \\    x = [1, 2, 3]
        \\    y = x
        \\    match y { [a, b, c] => a + b + c, _ => 0 }
        \\}
    , 6);
}

test "list refcount alias - return original after aliasing" {
    // Alias a list but return the original
    try runExpectI64(
        \\{
        \\    x = [1, 2, 3]
        \\    _y = x
        \\    match x { [a, b, c] => a + b + c, _ => 0 }
        \\}
    , 6);
}

test "list refcount alias - triple aliasing" {
    // Create multiple levels of aliasing
    try runExpectI64(
        \\{
        \\    x = [1, 2]
        \\    y = x
        \\    z = y
        \\    match z { [a, b] => a + b, _ => 0 }
        \\}
    , 3);
}

test "list refcount alias - mutable reassignment decrefs old list" {
    // Reassign a mutable list - old list should be decreffed
    try runExpectI64(
        \\{
        \\    var $x = [1, 2]
        \\    $x = [3, 4]
        \\    match $x { [a, b] => a + b, _ => 0 }
        \\}
    , 7);
}

test "list refcount alias - multiple independent lists" {
    // Multiple independent lists should not interfere
    try runExpectI64(
        \\{
        \\    x = [1, 2]
        \\    _y = [3, 4]
        \\    match x { [a, b] => a + b, _ => 0 }
        \\}
    , 3);
}

test "list refcount alias - empty list aliasing" {
    // Empty list aliasing should work correctly
    try runExpectI64(
        \\{
        \\    x = []
        \\    y = x
        \\    match y { [] => 42, _ => 0 }
        \\}
    , 42);
}

test "list refcount alias - alias then shadow" {
    // Alias a list, then reassign the original mutable binding
    try runExpectI64(
        \\{
        \\    var $x = [1, 2]
        \\    y = $x
        \\    $x = [3, 4]
        \\    match y { [a, b] => a + b, _ => 0 }
        \\}
    , 3);
}

test "list refcount alias - both references used" {
    // Use both the original and alias in computation
    try runExpectI64(
        \\{
        \\    x = [1, 2]
        \\    y = x
        \\    a = match x { [first, ..] => first, _ => 0 }
        \\    b = match y { [first, ..] => first, _ => 0 }
        \\    a + b
        \\}
    , 2);
}
