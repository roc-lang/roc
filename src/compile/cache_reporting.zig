//! Cache statistics reporting using the Roc reporting framework.
//!
//! This module provides functions to create structured cache statistics reports
//! that can be rendered to different output formats consistently with the rest
//! of the compiler's reporting system.

const std = @import("std");
const reporting = @import("reporting");

const Allocator = std.mem.Allocator;
const CacheStats = @import("cache_config.zig").CacheStats;
const Report = reporting.Report;
const Document = reporting.Document;
const Severity = reporting.Severity;
const RenderTarget = reporting.RenderTarget;
const ReportingConfig = reporting.ReportingConfig;

/// Data size unit for formatting
pub const DataSizeUnit = enum {
    bytes,
    kb,
    mb,
    gb,
};

/// Formatted data size with value and unit
const FormattedSize = struct {
    value: f64,
    unit: DataSizeUnit,
};

/// Format a byte count into an appropriate unit (B, KB, MB, GB)
pub fn formatDataSize(bytes: u64) FormattedSize {
    const bytes_f = @as(f64, @floatFromInt(bytes));

    if (bytes < 1024) {
        return FormattedSize{ .value = bytes_f, .unit = .bytes };
    } else if (bytes < 1024 * 1024) {
        return FormattedSize{ .value = bytes_f / 1024.0, .unit = .kb };
    } else if (bytes < 1024 * 1024 * 1024) {
        return FormattedSize{ .value = bytes_f / (1024.0 * 1024.0), .unit = .mb };
    } else {
        return FormattedSize{ .value = bytes_f / (1024.0 * 1024.0 * 1024.0), .unit = .gb };
    }
}

/// Get the unit string for a data size unit
pub fn getUnitString(unit: DataSizeUnit) []const u8 {
    return switch (unit) {
        .bytes => "B",
        .kb => "KB",
        .mb => "MB",
        .gb => "GB",
    };
}

/// Create a cache statistics report using the Roc reporting framework.
pub fn createCacheStatsReport(allocator: Allocator, stats: CacheStats) !Report {
    var report = Report.init(allocator, "CACHE STATISTICS", .info);

    const total_ops = stats.getTotalOps();
    if (total_ops == 0) {
        try report.document.addReflowingText("INFO: No cache operations performed");
        return report;
    }

    // Total operations
    const total_ops_str = try std.fmt.allocPrint(allocator, "{}", .{total_ops});
    defer allocator.free(total_ops_str);
    const hits_str = try std.fmt.allocPrint(allocator, "{}", .{stats.hits});
    defer allocator.free(hits_str);
    const misses_str = try std.fmt.allocPrint(allocator, "{}", .{stats.misses});
    defer allocator.free(misses_str);
    const owned_total = try report.addOwnedString(total_ops_str);
    const owned_hits = try report.addOwnedString(hits_str);
    const owned_misses = try report.addOwnedString(misses_str);

    try report.document.addText("INFO: Total operations: ");
    try report.document.addAnnotated(owned_total, .emphasized);
    try report.document.addText(" (");
    try report.document.addAnnotated(owned_hits, .emphasized);
    try report.document.addText(" hits, ");
    try report.document.addAnnotated(owned_misses, .emphasized);
    try report.document.addText(" misses)");
    try report.document.addLineBreak();

    // Hit rate
    const hit_rate_str = try std.fmt.allocPrint(allocator, "{d:.1}%", .{stats.getHitRate()});
    defer allocator.free(hit_rate_str);
    const owned_hit_rate = try report.addOwnedString(hit_rate_str);
    try report.document.addText("INFO: Hit rate: ");
    try report.document.addAnnotated(owned_hit_rate, .emphasized);
    try report.document.addLineBreak();

    // Data read
    const read_size = formatDataSize(stats.bytes_read);
    const read_str = try std.fmt.allocPrint(allocator, "{d:.1} {s}", .{ read_size.value, getUnitString(read_size.unit) });
    defer allocator.free(read_str);
    const owned_read = try report.addOwnedString(read_str);
    try report.document.addText("INFO: Data read: ");
    try report.document.addAnnotated(owned_read, .emphasized);
    try report.document.addLineBreak();

    // Data written
    const written_size = formatDataSize(stats.bytes_written);
    const written_str = try std.fmt.allocPrint(allocator, "{d:.1} {s}", .{ written_size.value, getUnitString(written_size.unit) });
    defer allocator.free(written_str);
    const owned_written = try report.addOwnedString(written_str);
    try report.document.addText("INFO: Data written: ");
    try report.document.addAnnotated(owned_written, .emphasized);
    try report.document.addLineBreak();

    // Stores
    const stores_str = try std.fmt.allocPrint(allocator, "{}", .{stats.stores});
    defer allocator.free(stores_str);
    const store_failures_str = try std.fmt.allocPrint(allocator, "{}", .{stats.store_failures});
    defer allocator.free(store_failures_str);
    const owned_stores = try report.addOwnedString(stores_str);
    const owned_store_failures = try report.addOwnedString(store_failures_str);
    try report.document.addText("INFO: Stores: ");
    try report.document.addAnnotated(owned_stores, .emphasized);
    try report.document.addText(" successful, ");
    try report.document.addAnnotated(owned_store_failures, .emphasized);
    try report.document.addText(" failed");
    try report.document.addLineBreak();

    // Invalidations (only show if > 0)
    if (stats.invalidations > 0) {
        const invalidations_str = try std.fmt.allocPrint(allocator, "{}", .{stats.invalidations});
        defer allocator.free(invalidations_str);
        const owned_invalidations = try report.addOwnedString(invalidations_str);
        try report.document.addText("INFO: Invalidations: ");
        try report.document.addAnnotated(owned_invalidations, .emphasized);
        try report.document.addLineBreak();
    }

    return report;
}

/// Render cache statistics to a writer using the specified target format.
pub fn renderCacheStats(allocator: Allocator, stats: CacheStats, writer: anytype, target: RenderTarget) !void {
    var report = try createCacheStatsReport(allocator, stats);
    defer report.deinit();

    try reporting.renderReport(&report, writer, target);
}

/// Render cache statistics to the terminal (convenience function).
pub fn renderCacheStatsToTerminal(allocator: Allocator, stats: CacheStats, writer: anytype) !void {
    try renderCacheStats(allocator, stats, writer, .color_terminal);
}

/// Render cache statistics to markdown (convenience function).
pub fn renderCacheStatsToMarkdown(allocator: Allocator, stats: CacheStats, writer: anytype) !void {
    try renderCacheStats(allocator, stats, writer, .markdown);
}

/// Render cache statistics to HTML (convenience function).
pub fn renderCacheStatsToHtml(allocator: Allocator, stats: CacheStats, writer: anytype) !void {
    try renderCacheStats(allocator, stats, writer, .html);
}

/// Render cache statistics to LSP format (convenience function).
pub fn renderCacheStatsToLsp(allocator: Allocator, stats: CacheStats, writer: anytype) !void {
    try renderCacheStats(allocator, stats, writer, .language_server);
}
