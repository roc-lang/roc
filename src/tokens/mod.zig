//! Converts Roc source code into an Abstract Syntax Tree (AST) through tokenization and parsing.
//!
//! This module provides the entry point for the parsing phase of compilation, transforming
//! raw source text into a structured AST representation that subsequent compiler phases can process.

const std = @import("std");
const base = @import("base");
const collections = @import("collections");
const tracy = @import("tracy");

pub const tokenize = @import("tokenize.zig");

const CommonEnv = base.CommonEnv;

test "tokenization tests" {
    std.testing.refAllDecls(@import("tokenize.zig"));
}
