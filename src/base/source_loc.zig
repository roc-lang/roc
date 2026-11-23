//! Resolved source location carried through the post-check IRs so that
//! backends can emit debug info and runtime failures can report where in the
//! source they happened.
//!
//! Locations are resolved (file table index plus 1-based line and column)
//! rather than byte-offset regions so that consumers of serialized LIR images
//! never need the original source text or its line-start table.

const std = @import("std");
const collections = @import("collections");

/// A resolved source location. `none` marks compiler-generated code with no
/// single source counterpart (the debug-info convention is line 0).
pub const SourceLoc = extern struct {
    /// Index into the owning program's source file table; absent for no
    /// location.
    file: collections.OptionalU32,
    /// 1-based source line; 0 means no location.
    line: u32,
    /// 1-based byte column within the line; 0 means no location.
    column: u32,

    pub const none: SourceLoc = .{ .file = .none, .line = 0, .column = 0 };

    /// A location in source file table entry `file`.
    pub fn at(file: u32, line: u32, column: u32) SourceLoc {
        return .{ .file = .some(file), .line = line, .column = column };
    }

    pub fn hasLocation(self: SourceLoc) bool {
        return self.file != .none;
    }

    /// The source file table entry of a location that has one.
    pub fn fileIndex(self: SourceLoc) u32 {
        return self.file.get().?;
    }
};

/// One entry of a program's source file table, addressed by `SourceLoc.file`.
///
/// `name` is the module's bare display name (what debug info and reports
/// show); `qualified_name` is the coordinator's package-qualified module name
/// (e.g. `pf.Utils`), which tells apart same-named modules of different
/// packages in reports. `module_identity` is the module's deep content
/// identity (see `base.module_identity`). Identity comparisons ("does this
/// location belong to that module?") must use `module_identity`: names are
/// display data, and identical modules in different packages are one source
/// file with one identity.
pub const SourceFileEntry = struct {
    name: []const u8,
    qualified_name: []const u8,
    module_identity: [32]u8,
};

/// How a staged copy of a file lines up with the file the user wrote. The CLI
/// compiles a default app from such a copy (see `cli/default_app.zig`): the
/// staging prepends `header_lines` whole lines, appends wiring from byte
/// `footer_start` of the copy on (`no_footer` when it appends none), and leaves
/// every line in between as the user wrote it.
pub const StagedSourceMap = struct {
    header_lines: u32,
    footer_start: u32,

    /// The `footer_start` of a staging that appends no wiring: past every
    /// byte offset of the copy, so no offset reaches it.
    pub const no_footer: u32 = std.math.maxInt(u32);

    /// The user's 0-based line for 0-based line `line_idx` of the staged copy,
    /// at byte `offset` of it; null for the wiring the staging wrote.
    pub fn userLineIdx(self: StagedSourceMap, line_idx: u32, offset: u32) ?u32 {
        if (line_idx < self.header_lines or offset >= self.footer_start) return null;
        return line_idx - self.header_lines;
    }
};

test "StagedSourceMap maps the user's lines and drops the staged wiring" {
    const map: StagedSourceMap = .{ .header_lines = 6, .footer_start = 100 };
    try std.testing.expectEqual(@as(?u32, null), map.userLineIdx(4, 40));
    try std.testing.expectEqual(@as(?u32, 0), map.userLineIdx(6, 60));
    try std.testing.expectEqual(@as(?u32, 3), map.userLineIdx(9, 99));
    try std.testing.expectEqual(@as(?u32, null), map.userLineIdx(10, 100));
    const unwired: StagedSourceMap = .{ .header_lines = 6, .footer_start = StagedSourceMap.no_footer };
    try std.testing.expectEqual(@as(?u32, 4), unwired.userLineIdx(10, 100));
}
