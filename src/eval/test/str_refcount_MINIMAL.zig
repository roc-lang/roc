//! MINIMAL test to reproduce the refcounting bug
//! This file contains ONLY the absolute minimal failing case

const std = @import("std");
const helpers = @import("helpers.zig");
const testing = std.testing;

const runExpectStr = helpers.runExpectStr;

test "MINIMAL BUG - x,a,b,c return b with string x" {
    try runExpectStr(
        \\{
        \\    x = "x"
        \\    a = x
        \\    b = x
        \\    c = x
        \\    b
        \\}
    , "x", .no_trace);
}

test "CONTROL - x,a,b,c return b with string y" {
    try runExpectStr(
        \\{
        \\    x = "y"
        \\    a = x
        \\    b = x
        \\    c = x
        \\    b
        \\}
    , "y", .no_trace);
}

test "CONTROL - y,a,b,c return b with string x" {
    try runExpectStr(
        \\{
        \\    y = "x"
        \\    a = y
        \\    b = y
        \\    c = y
        \\    b
        \\}
    , "x", .no_trace);
}

test "CONTROL - a,b,c,d return b with string x" {
    try runExpectStr(
        \\{
        \\    a = "x"
        \\    b = a
        \\    c = a
        \\    d = a
        \\    b
        \\}
    , "x", .no_trace);
}

test "HYPOTHESIS - a=a pattern" {
    try runExpectStr(
        \\{
        \\    a = "a"
        \\    b = a
        \\    c = a
        \\    d = a
        \\    b
        \\}
    , "a", .no_trace);
}

test "HYPOTHESIS - m=m pattern" {
    try runExpectStr(
        \\{
        \\    m = "m"
        \\    a = m
        \\    b = m
        \\    c = m
        \\    b
        \\}
    , "m", .no_trace);
}

test "HYPOTHESIS - z=z pattern" {
    try runExpectStr(
        \\{
        \\    z = "z"
        \\    a = z
        \\    b = z
        \\    c = z
        \\    b
        \\}
    , "z", .no_trace);
}

test "HYPOTHESIS - w=w pattern" {
    try runExpectStr(
        \\{
        \\    w = "w"
        \\    a = w
        \\    b = w
        \\    c = w
        \\    b
        \\}
    , "w", .no_trace);
}

test "HYPOTHESIS - y=y pattern" {
    try runExpectStr(
        \\{
        \\    y = "y"
        \\    a = y
        \\    b = y
        \\    c = y
        \\    b
        \\}
    , "y", .no_trace);
}
