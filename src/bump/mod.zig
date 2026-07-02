//! Elm-style semver checking for Roc packages (`roc bump`).
//!
//! Compares the public API of two versions of a package and classifies the
//! change as patch, minor, or major. The pipeline is:
//!
//! 1. `PackageApi` — a canonical, compiler-run-independent model of a
//!    package's public API.
//! 2. `extract` — builds a `PackageApi` from checked compiler artifacts.
//! 3. `diff` — compares two `PackageApi` values into a magnitude and a
//!    structured change list.

pub const PackageApi = @import("PackageApi.zig");
pub const diff = @import("diff.zig");

test "bump tests" {
    std.testing.refAllDecls(@import("PackageApi.zig"));
    std.testing.refAllDecls(@import("diff.zig"));
    std.testing.refAllDecls(@import("test/diff_test.zig"));
}

const std = @import("std");
