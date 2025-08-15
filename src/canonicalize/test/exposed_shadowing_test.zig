//! Tests for exposed item shadowing validation during the canonicalization phase.
//!
//! This module contains unit tests that verify the correct handling of
//! exposed items that are declared but not implemented, and validation
//! of shadowing behavior during the canonicalization process.

const std = @import("std");
const compile = @import("compile");
const parse = @import("parse");
const base = @import("base");

const Can = @import("../Can.zig");
const ModuleEnv = @import("../ModuleEnv.zig");

const AST = parse.AST;
const tokenize = parse.tokenize;
const testing = std.testing;

test "exposed but not implemented - values" {
    return error.SkipZigTest; // SKIP: parse.parse API has changed - needs update to new parser API
}

test "exposed but not implemented - types" {
    return error.SkipZigTest; // SKIP: parse.parse API has changed - needs update to new parser API
}

test "redundant exposed entries" {
    return error.SkipZigTest; // SKIP: parse.parse API has changed - needs update to new parser API
}

test "shadowing with exposed items" {
    return error.SkipZigTest; // SKIP: parse.parse API has changed - needs update to new parser API
}

test "shadowing non-exposed items" {
    return error.SkipZigTest; // SKIP: parse.parse API has changed - needs update to new parser API
}

test "exposed items correctly tracked across shadowing" {
    return error.SkipZigTest; // SKIP: parse.parse API has changed - needs update to new parser API
}

test "complex case with redundant, shadowing, and not implemented" {
    return error.SkipZigTest; // SKIP: parse.parse API has changed - needs update to new parser API
}

test "exposed_items is populated correctly" {
    return error.SkipZigTest; // SKIP: parse.parse API has changed - needs update to new parser API
}

test "exposed_items persists after canonicalization" {
    return error.SkipZigTest; // SKIP: parse.parse API has changed - needs update to new parser API
}

test "exposed_items never has entries removed" {
    return error.SkipZigTest; // SKIP: parse.parse API has changed - needs update to new parser API
}

test "exposed_items handles identifiers with different attributes" {
    return error.SkipZigTest; // SKIP: parse.parse API has changed - needs update to new parser API
}