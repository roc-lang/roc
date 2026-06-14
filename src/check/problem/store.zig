//! Problem storage for type checking errors
//!
//! The Store collects problems during type checking and provides backing
//! storage for extra data like formatted strings and missing patterns.

const std = @import("std");
const base = @import("base");

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

    pub const EmpiricalSiteKind = enum {
        match,
        destructure,
    };

    pub const PendingStaticExhaustivenessMode = enum {
        static,
        empirical,
    };

    pub const PendingStaticExhaustiveness = struct {
        kind: EmpiricalSiteKind,
        mode: PendingStaticExhaustivenessMode,
        region: base.Region,
        problem: Problem,
    };

    problems: std.ArrayListAligned(Problem, ALIGNMENT) = .empty,
    pending_static_exhaustiveness: std.ArrayList(PendingStaticExhaustiveness) = .empty,

    /// Backing storage for formatted pattern strings
    extra_strings_backing: ByteList,
    /// Backing storage for missing patterns index arrays
    missing_patterns_backing: std.array_list.Managed(ExtraStringIdx),

    pub fn init(gpa: Allocator) std.mem.Allocator.Error!Self {
        return .{
            .problems = try std.ArrayListAligned(Problem, ALIGNMENT).initCapacity(gpa, 16),
            .pending_static_exhaustiveness = .empty,
            .extra_strings_backing = try ByteList.initCapacity(gpa, 512),
            .missing_patterns_backing = try std.array_list.Managed(ExtraStringIdx).initCapacity(gpa, 64),
        };
    }

    pub fn initCapacity(gpa: Allocator, capacity: usize) std.mem.Allocator.Error!Self {
        return .{
            .problems = try std.ArrayListAligned(Problem, ALIGNMENT).initCapacity(gpa, capacity),
            .pending_static_exhaustiveness = .empty,
            .extra_strings_backing = try ByteList.initCapacity(gpa, 512),
            .missing_patterns_backing = try std.array_list.Managed(ExtraStringIdx).initCapacity(gpa, 64),
        };
    }

    pub fn deinit(self: *Self, gpa: Allocator) void {
        self.extra_strings_backing.deinit();
        self.missing_patterns_backing.deinit();
        self.pending_static_exhaustiveness.deinit(gpa);
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
        try self.extra_strings_backing.print(format, args);
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

    pub fn appendPendingStaticExhaustiveness(
        self: *Self,
        gpa: Allocator,
        kind: EmpiricalSiteKind,
        mode: PendingStaticExhaustivenessMode,
        region: base.Region,
        pending_problem: Problem,
    ) std.mem.Allocator.Error!void {
        try self.pending_static_exhaustiveness.append(gpa, .{
            .kind = kind,
            .mode = mode,
            .region = region,
            .problem = pending_problem,
        });
    }

    pub fn resolvePendingStaticExhaustiveness(self: *Self, kind: EmpiricalSiteKind, region: base.Region) void {
        var write: usize = 0;
        for (self.pending_static_exhaustiveness.items) |pending| {
            if (pending.mode == .empirical and pending.kind == kind and regionsEqual(pending.region, region)) continue;
            self.pending_static_exhaustiveness.items[write] = pending;
            write += 1;
        }
        self.pending_static_exhaustiveness.shrinkRetainingCapacity(write);
    }

    pub fn appendEmpiricalExhaustivenessFailure(
        self: *Self,
        gpa: Allocator,
        kind: EmpiricalSiteKind,
        region: base.Region,
    ) std.mem.Allocator.Error!bool {
        var index: usize = 0;
        while (index < self.pending_static_exhaustiveness.items.len) {
            const pending = self.pending_static_exhaustiveness.items[index];
            if (pending.kind != kind or !regionsEqual(pending.region, region)) {
                index += 1;
                continue;
            }
            var problem = pending.problem;
            if (pending.mode == .empirical) {
                switch (problem) {
                    .non_exhaustive_match => |*match| match.empirical = true,
                    .non_exhaustive_destructure => |*destructure| destructure.empirical = true,
                    else => unreachable,
                }
            }
            _ = try self.appendProblem(gpa, problem);
            _ = self.pending_static_exhaustiveness.swapRemove(index);
            return true;
        }
        return false;
    }

    pub fn flushPendingStaticExhaustiveness(self: *Self, gpa: Allocator) std.mem.Allocator.Error!usize {
        var count: usize = 0;
        for (self.pending_static_exhaustiveness.items) |pending| {
            if (pending.mode != .static) continue;
            _ = try self.appendProblem(gpa, pending.problem);
            count += 1;
        }
        self.pending_static_exhaustiveness.clearRetainingCapacity();
        return count;
    }

    pub fn flushAllPendingStaticExhaustiveness(self: *Self, gpa: Allocator) std.mem.Allocator.Error!usize {
        const count = self.pending_static_exhaustiveness.items.len;
        for (self.pending_static_exhaustiveness.items) |pending| {
            _ = try self.appendProblem(gpa, pending.problem);
        }
        self.pending_static_exhaustiveness.clearRetainingCapacity();
        return count;
    }

    pub fn get(self: *Self, idx: Problem.Idx) Problem {
        return self.problems.items[@intFromEnum(idx)];
    }

    pub fn len(self: *Self) usize {
        return self.problems.items.len;
    }
};

fn regionsEqual(a: base.Region, b: base.Region) bool {
    return a.start.offset == b.start.offset and a.end.offset == b.end.offset;
}
