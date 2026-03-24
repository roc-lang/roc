//! Focused tests for the LLVM backend.
//! Runs dev + LLVM backends and compares their Str.inspect output.
//! Use: zig build test-eval -- --test-filter "llvm"
const helpers = @import("helpers.zig");

const runDevAndLlvmExpectStr = helpers.runDevAndLlvmExpectStr;

// Integers (U32 suffix to avoid Dec default)

test "llvm u32 literal" {
    try runDevAndLlvmExpectStr("15.U32", "15");
}

// TODO: LLVM crashes on comparison low-level ops
// test "llvm u32 comparison" {
//     try runDevAndLlvmExpectStr("1.U32 <= 5.U32", "True");
// }

// Strings

test "llvm string literal" {
    try runDevAndLlvmExpectStr(
        \\"hello"
    , "\"hello\"");
}

test "llvm string concat" {
    try runDevAndLlvmExpectStr(
        \\Str.concat("hello ", "world")
    , "\"hello world\"");
}

// Booleans

test "llvm bool true" {
    try runDevAndLlvmExpectStr("Bool.True", "True");
}

test "llvm bool false" {
    try runDevAndLlvmExpectStr("Bool.False", "False");
}

// Lists

test "llvm empty list" {
    try runDevAndLlvmExpectStr("[] : List U32", "[]");
}

// Functions

// TODO: LLVM crashes on proc calls with Dec args
// test "llvm function call" {
//     try runDevAndLlvmExpectStr(
//         \\add = |a, b| a + b
//         \\add(3, 4)
//     , "7");
// }

// Records

// TODO: LLVM crashes on Dec arithmetic in record access
// test "llvm record field access" {
//     try runDevAndLlvmExpectStr(
//         \\rec = { x: 10, y: 20 }
//         \\rec.x + rec.y
//     , "30");
// }

// Tags

// TODO: LLVM SIGABRT during tag union test teardown
// test "llvm tag union" {
//     try runDevAndLlvmExpectStr(
//         \\x : [A, B, C] = A
//         \\x
//     , "A");
// }
