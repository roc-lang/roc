//! Documentation extraction for Roc modules.
//!
//! This module provides the data model and extraction logic for generating
//! documentation from compiled Roc modules. It extracts doc comments, type
//! signatures, and module structure into a stable format suitable for
//! rendering to HTML, Markdown, JSON, or other output formats.

pub const DocModel = @import("DocModel.zig");
pub const extract = @import("extract.zig");
pub const render_type = @import("render_type.zig");

test "docs tests" {
    std.testing.refAllDecls(@import("DocModel.zig"));
    std.testing.refAllDecls(@import("extract.zig"));
    std.testing.refAllDecls(@import("render_type.zig"));
}

const std = @import("std");
