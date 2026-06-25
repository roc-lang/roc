//! Resolved source location carried through the post-check IRs so that
//! backends can emit debug info and runtime failures can report where in the
//! source they happened.
//!
//! Locations are resolved (file table index plus 1-based line and column)
//! rather than byte-offset regions so that consumers of serialized LIR images
//! never need the original source text or its line-start table.

const std = @import("std");

/// A resolved source location. `none` marks compiler-generated code with no
/// single source counterpart (the debug-info convention is line 0).
pub const SourceLoc = extern struct {
    /// Index into the owning program's source file table, or `no_file`.
    file: u32,
    /// 1-based source line; 0 means no location.
    line: u32,
    /// 1-based byte column within the line; 0 means no location.
    column: u32,

    pub const no_file: u32 = std.math.maxInt(u32);

    pub const none: SourceLoc = .{ .file = no_file, .line = 0, .column = 0 };

    pub fn hasLocation(self: SourceLoc) bool {
        return self.file != no_file;
    }
};
