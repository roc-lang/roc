//! String refcounting tests - Phase 4: Records with Strings
//!
//! These tests verify string refcounting when strings are stored in records.
//!
//! Each test should pass with correct refcounting (no leaks, no corruption)

const std = @import("std");
const helpers = @import("helpers.zig");
const testing = std.testing;

const runExpectStr = helpers.runExpectStr;

// Basic record construction and field access

test "str refcount record - single field record" {
    // Verify we can create a record with a string field
    try runExpectStr(
        \\{
        \\    x = "hi"
        \\    r = {a: x}
        \\    r.a
        \\}
    , "hi", .no_trace);
}

test "str refcount record - inline literal access" {
    // Verify direct field access on record literal
    try runExpectStr("{foo: \"hello\"}.foo", "hello", .no_trace);
}

test "str refcount record - multiple fields different strings" {
    // Verify multiple different strings in record
    try runExpectStr(
        \\{
        \\    x = "first"
        \\    y = "second"
        \\    r = {a: x, b: y}
        \\    r.a
        \\}
    , "first", .no_trace);
}

test "str refcount record - access second field" {
    // Verify accessing different field works
    try runExpectStr(
        \\{
        \\    x = "first"
        \\    y = "second"
        \\    r = {a: x, b: y}
        \\    r.b
        \\}
    , "second", .no_trace);
}

// String duplication in records

test "str refcount record - string duplicated in two fields" {
    // Verify same string can appear in multiple fields
    try runExpectStr(
        \\{
        \\    x = "hi"
        \\    r = {a: x, b: x}
        \\    r.a
        \\}
    , "hi", .no_trace);
}

test "str refcount record - duplicated string access second field" {
    // Verify both fields have valid reference
    try runExpectStr(
        \\{
        \\    x = "hi"
        \\    r = {a: x, b: x}
        \\    r.b
        \\}
    , "hi", .no_trace);
}

test "str refcount record - string tripled in record" {
    // Verify string used in three fields
    try runExpectStr(
        \\{
        \\    x = "test"
        \\    r = {a: x, b: x, c: x}
        \\    r.b
        \\}
    , "test", .no_trace);
}

// Records with mixed types

test "str refcount record - mixed string and int" {
    // Verify records with mixed types work
    try runExpectStr(
        \\{
        \\    x = "text"
        \\    r = {s: x, n: 42}
        \\    r.s
        \\}
    , "text", .no_trace);
}

test "str refcount record - multiple mixed fields" {
    // Verify complex mixed-type record
    try runExpectStr(
        \\{
        \\    x = "hello"
        \\    y = "world"
        \\    r = {a: x, n1: 1, b: y, n2: 2}
        \\    r.b
        \\}
    , "world", .no_trace);
}

// Nested records

test "str refcount record - nested record single level" {
    // Verify nested record with string
    try runExpectStr(
        \\{
        \\    x = "nested"
        \\    inner = {s: x}
        \\    outer = {r: inner}
        \\    outer.r.s
        \\}
    , "nested", .no_trace);
}

test "str refcount record - nested record inline" {
    // Verify inline nested record literal
    try runExpectStr("{outer: {inner: \"deep\"}}.outer.inner", "deep", .no_trace);
}

test "str refcount record - nested with multiple strings" {
    // Verify nested record with multiple strings
    try runExpectStr(
        \\{
        \\    x = "outer"
        \\    y = "inner"
        \\    r = {a: x, nested: {b: y}}
        \\    r.nested.b
        \\}
    , "inner", .no_trace);
}

// String aliasing before record use

test "str refcount record - aliased string in record" {
    // Verify aliased string works in record
    try runExpectStr(
        \\{
        \\    x = "original"
        \\    y = x
        \\    r = {a: y}
        \\    r.a
        \\}
    , "original", .no_trace);
}

test "str refcount record - multiple aliases in record" {
    // Verify multiple aliases of same string in record
    try runExpectStr(
        \\{
        \\    x = "shared"
        \\    y = x
        \\    z = x
        \\    r = {a: y, b: z}
        \\    r.a
        \\}
    , "shared", .no_trace);
}

// Edge cases

test "str refcount record - empty string field" {
    // Verify empty string in record
    try runExpectStr(
        \\{
        \\    x = ""
        \\    r = {a: x}
        \\    r.a
        \\}
    , "", .no_trace);
}

test "str refcount record - large string field" {
    // Verify large heap-allocated string in record
    const large = "This is a very long string that exceeds small string optimization";
    try runExpectStr(
        \\{
        \\    x = "This is a very long string that exceeds small string optimization"
        \\    r = {data: x}
        \\    r.data
        \\}
    , large, .no_trace);
}

test "str refcount record - small string optimization" {
    // Verify small string (inline storage) in record
    try runExpectStr(
        \\{
        \\    x = "small"
        \\    r = {val: x}
        \\    r.val
        \\}
    , "small", .no_trace);
}

// Multiple record operations

test "str refcount record - create two records with same string" {
    // Verify same string in multiple independent records
    try runExpectStr(
        \\{
        \\    x = "shared"
        \\    r1 = {a: x}
        \\    r2 = {b: x}
        \\    r1.a
        \\}
    , "shared", .no_trace);
}

test "str refcount record - access from second record" {
    // Verify second record also has valid reference
    try runExpectStr(
        \\{
        \\    x = "shared"
        \\    r1 = {a: x}
        \\    r2 = {b: x}
        \\    r2.b
        \\}
    , "shared", .no_trace);
}

test "str refcount record - nested records share string" {
    // Verify string shared across nested record structure
    try runExpectStr(
        \\{
        \\    x = "everywhere"
        \\    r = {a: x, nested: {b: x}}
        \\    r.a
        \\}
    , "everywhere", .no_trace);
}

test "str refcount record - complex sharing pattern" {
    // Verify complex pattern: aliasing + multiple records + nested
    try runExpectStr(
        \\{
        \\    x = "complex"
        \\    y = x
        \\    r1 = {a: x}
        \\    r2 = {b: y, nested: {c: x}}
        \\    r2.nested.c
        \\}
    , "complex", .no_trace);
}
