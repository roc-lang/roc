//! Problem storage for type checking errors
//!
//! The Store collects problems during type checking and provides backing
//! storage for extra data like formatted strings and missing patterns.

const std = @import("std");

const types = @import("types.zig");

const Allocator = std.mem.Allocator;
const Problem = types.Problem;
const ExtraStringIdx = types.ExtraStringIdx;
const MissingPatternsRange = types.MissingPatternsRange;

const ByteList = std.array_list.Managed(u8);

/// Whenever a type error occurs, we update the `Var` in the type store to
/// have `.err` content. This is necessary to continue type-checking but
/// looses essential error information. So before doing this, we create a fully
/// resolved snapshot of the type that we can use in reporting
///
/// Entry point is `appendProblem`
pub const Store = struct {
    const Self = @This();
    const ALIGNMENT = std.mem.Alignment.@"16";

    problems: std.ArrayListAligned(Problem, ALIGNMENT) = .{},

    /// Backing storage for formatted pattern strings
    extra_strings_backing: ByteList,
    /// Backing storage for missing patterns index arrays
    missing_patterns_backing: std.array_list.Managed(ExtraStringIdx),

    pub fn init(gpa: Allocator) std.mem.Allocator.Error!Self {
        return .{
            .problems = try std.ArrayListAligned(Problem, ALIGNMENT).initCapacity(gpa, 16),
            .extra_strings_backing = try ByteList.initCapacity(gpa, 512),
            .missing_patterns_backing = try std.array_list.Managed(ExtraStringIdx).initCapacity(gpa, 64),
        };
    }

    pub fn initCapacity(gpa: Allocator, capacity: usize) std.mem.Allocator.Error!Self {
        return .{
            .problems = try std.ArrayListAligned(Problem, ALIGNMENT).initCapacity(gpa, capacity),
            .extra_strings_backing = try ByteList.initCapacity(gpa, 512),
            .missing_patterns_backing = try std.array_list.Managed(ExtraStringIdx).initCapacity(gpa, 64),
        };
    }

    pub fn deinit(self: *Self, gpa: Allocator) void {
        self.extra_strings_backing.deinit();
        self.missing_patterns_backing.deinit();
        self.problems.deinit(gpa);
    }

    /// Put an extra string in the backing store, returning an "id" (range)
    pub fn putExtraString(self: *Self, str: []const u8) std.mem.Allocator.Error!ExtraStringIdx {
        const start = self.extra_strings_backing.items.len;
        try self.extra_strings_backing.appendSlice(str);
        const end = self.extra_strings_backing.items.len;
        return ExtraStringIdx{ .start = start, .count = end - start };
    }

    /// Put an extra string in the backing store, returning an "id" (range)
    pub fn putFmtExtraString(self: *Self, comptime format: []const u8, args: anytype) std.mem.Allocator.Error!ExtraStringIdx {
        const start = self.extra_strings_backing.items.len;
        var writer = self.extra_strings_backing.writer();
        try writer.print(format, args);
        const end = self.extra_strings_backing.items.len;
        return ExtraStringIdx{ .start = start, .count = end - start };
    }

    /// Get a stored pattern string by its range
    pub fn getExtraString(self: *const Self, idx: ExtraStringIdx) []const u8 {
        return self.extra_strings_backing.items[idx.start..][0..idx.count];
    }

    /// Get the slice of missing pattern indices by range
    pub fn getMissingPatterns(self: *const Self, range: MissingPatternsRange) []const ExtraStringIdx {
        return self.missing_patterns_backing.items[range.start..][0..range.count];
    }

    /// Create a deep snapshot from a Var, storing it in this SnapshotStore
    pub fn appendProblem(self: *Self, gpa: Allocator, problem: Problem) std.mem.Allocator.Error!Problem.Idx {
        const idx: Problem.Idx = @enumFromInt(self.problems.items.len);
        try self.problems.append(gpa, problem);
        return idx;
    }

    pub fn get(self: *Self, idx: Problem.Idx) Problem {
        return self.problems.items[@intFromEnum(idx)];
    }

    pub fn len(self: *Self) usize {
        return self.problems.items.len;
    }
};
