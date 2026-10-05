//! Cache statistics reporting using the Roc reporting framework.
//!
//! This module provides functions to create structured cache statistics reports
//! that can be rendered to different output formats consistently with the rest
//! of the compiler's reporting system.

const std = @import("std");

const Allocator = std.mem.Allocator;

/// Statistics for cache operations (optional, for debugging)
pub const Stats = struct {
    hits: u32 = 0,
    misses: u32 = 0,
    writes: u32 = 0,
    errors: u32 = 0,
    total_bytes_written: u64 = 0,
    total_bytes_read: u64 = 0,

    pub fn reset(self: *Stats) void {
        self.* = .{};
    }

    pub fn hitRate(self: *const Stats) f64 {
        const total = self.hits + self.misses;
        if (total == 0) return 0.0;
        return @as(f64, @floatFromInt(self.hits)) / @as(f64, @floatFromInt(total));
    }

    pub fn print(self: *const Stats, writer: *std.Io.Writer) Allocator.Error!void {
        try writer.print("Cache Stats:\n", .{});
        try writer.print("  Hits: {d}\n", .{self.hits});
        try writer.print("  Misses: {d}\n", .{self.misses});
        try writer.print("  Writes: {d}\n", .{self.writes});
        try writer.print("  Errors: {d}\n", .{self.errors});
        try writer.print("  Hit Rate: {d:.2}%\n", .{self.hitRate() * 100.0});
        try writer.print("  Bytes Written: {d}\n", .{self.total_bytes_written});
        try writer.print("  Bytes Read: {d}\n", .{self.total_bytes_read});
    }
};
