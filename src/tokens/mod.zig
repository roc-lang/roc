//! Tokenization functionality for the Roc parser.
//!
//! This module provides the tokenizer that converts Roc source code into
//! a stream of tokens for parsing. It handles all Roc language tokens including
//! keywords, identifiers, literals, operators, and punctuation, representing
//! them as offsets into the source code with additional metadata.

const std = @import("std");
const base = @import("base");
const collections = @import("collections");
const tracy = @import("tracy");

pub const Token = @import("Token.zig");
pub const Tokenizer = @import("Tokenizer.zig");

const CommonEnv = base.CommonEnv;

test "tokenization tests" {
    std.testing.refAllDecls(@import("Tokenizer.zig"));
}
