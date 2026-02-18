//! List refcounting tests - Phase 3: Basic List Expressions
//!
//! More comprehensive integer list tests covering various sizes and patterns.
//! Still using integer elements to isolate list container refcounting.
//!
//! Each test should pass with correct refcounting (no leaks, no corruption)

const helpers = @import("helpers.zig");

const runExpectI64 = helpers.runExpectI64;

test "list refcount basic - various small list sizes" {
    // Single element
    try runExpectI64(
        \\match [5] { [x] => x, _ => 0 }
    , 5, .no_trace);
}

test "list refcount basic - two elements" {
    try runExpectI64(
        \\match [10, 20] { [a, b] => a + b, _ => 0 }
    , 30, .no_trace);
}

test "list refcount basic - five elements" {
    try runExpectI64(
        \\match [1, 2, 3, 4, 5] { [a, b, c, d, e] => a + b + c + d + e, _ => 0 }
    , 15, .no_trace);
}

test "list refcount basic - larger list with pattern" {
    // Use list rest pattern for larger lists
    try runExpectI64(
        \\match [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] { [first, second, .. as rest] => first + second, _ => 0 }
    , 3, .no_trace);
}

test "list refcount basic - sequential independent lists" {
    // Multiple lists in same scope
    try runExpectI64(
        \\{
        \\    a = [1]
        \\    b = [2, 3]
        \\    c = [4, 5, 6]
        \\    match a { [x] => x, _ => 0 }
        \\}
    , 1, .no_trace);
}

test "list refcount basic - return middle list" {
    try runExpectI64(
        \\{
        \\    a = [1]
        \\    b = [2, 3]
        \\    c = [4, 5, 6]
        \\    match b { [x, y] => x + y, _ => 0 }
        \\}
    , 5, .no_trace);
}

test "list refcount basic - return last list" {
    try runExpectI64(
        \\{
        \\    a = [1]
        \\    b = [2, 3]
        \\    c = [4, 5, 6]
        \\    match c { [x, y, z] => x + y + z, _ => 0 }
        \\}
    , 15, .no_trace);
}

test "list refcount basic - mix of empty and non-empty" {
    try runExpectI64(
        \\{
        \\    x = []
        \\    y = [1, 2]
        \\    z = []
        \\    match y { [a, b] => a + b, _ => 0 }
        \\}
    , 3, .no_trace);
}

test "list refcount basic - return empty from mix" {
    try runExpectI64(
        \\{
        \\    x = []
        \\    y = [1, 2]
        \\    z = []
        \\    match x { [] => 42, _ => 0 }
        \\}
    , 42, .no_trace);
}

test "list refcount basic - nested blocks with lists" {
    try runExpectI64(
        \\{
        \\    outer = [1, 2, 3]
        \\    result = {
        \\        inner = outer
        \\        match inner { [a, b, c] => a + b + c, _ => 0 }
        \\    }
        \\    result
        \\}
    , 6, .no_trace);
}

test "list refcount basic - list created and used in inner block" {
    try runExpectI64(
        \\{
        \\    result = {
        \\        lst = [10, 20, 30]
        \\        match lst { [a, b, c] => a + b + c, _ => 0 }
        \\    }
        \\    result
        \\}
    , 60, .no_trace);
}

test "list refcount basic - multiple lists chained" {
    try runExpectI64(
        \\{
        \\    a = [1]
        \\    b = a
        \\    c = [2, 3]
        \\    d = c
        \\    x = match b { [v] => v, _ => 0 }
        \\    y = match d { [v1, v2] => v1 + v2, _ => 0 }
        \\    x + y
        \\}
    , 6, .no_trace);
}
