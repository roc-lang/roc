//! Active-root diagnostic ownership and deterministic publication.
const std = @import("std");
const D = @import("diagnostics.zig");
const Allocator = std.mem.Allocator;

/// Stores diagnostic sets contributed by each active open root.
pub const Contributions = struct {
    roots: std.StringHashMapUnmanaged([]D.PublishDiagnostics) = .empty,

    pub fn deinit(self: *Contributions, allocator: Allocator) void {
        var it = self.roots.iterator();
        while (it.next()) |entry| {
            allocator.free(entry.key_ptr.*);
            freeSets(allocator, entry.value_ptr.*);
        }
        self.roots.deinit(allocator);
    }

    pub fn remove(self: *Contributions, allocator: Allocator, root: []const u8) void {
        if (self.roots.fetchRemove(root)) |old| {
            allocator.free(old.key);
            freeSets(allocator, old.value);
        }
    }

    /// Transfer ownership only after all allocations needed for insertion succeed.
    pub fn replace(self: *Contributions, allocator: Allocator, root: []const u8, sets: []D.PublishDiagnostics) Allocator.Error!void {
        const key = try allocator.dupe(u8, root);
        errdefer allocator.free(key);
        const entry = try self.roots.getOrPut(allocator, key);
        if (entry.found_existing) {
            allocator.free(key);
            freeSets(allocator, entry.value_ptr.*);
        }
        entry.value_ptr.* = sets;
    }

    /// A root or any contributed set keeps its URI eligible for publication,
    /// including sets with no diagnostics.
    pub fn referencesUri(self: *const Contributions, uri: []const u8) bool {
        if (self.roots.contains(uri)) return true;
        var roots = self.roots.valueIterator();
        while (roots.next()) |sets| {
            for (sets.*) |set| {
                if (std.mem.eql(u8, set.uri, uri)) return true;
            }
        }
        return false;
    }

    pub fn collect(self: *Contributions, allocator: Allocator, uri: []const u8) Allocator.Error!D.PublishDiagnostics {
        var list: std.ArrayList(D.Diagnostic) = .empty;
        errdefer {
            for (list.items) |diag| allocator.free(diag.message);
            list.deinit(allocator);
        }
        if (self.roots.get(uri)) |own| {
            try append(allocator, &list, own, uri);
        } else {
            var it = self.roots.valueIterator();
            while (it.next()) |sets| try append(allocator, &list, sets.*, uri);
        }
        std.mem.sort(D.Diagnostic, list.items, {}, lessThan);
        const owned_uri = try allocator.dupe(u8, uri);
        errdefer allocator.free(owned_uri);
        return .{ .uri = owned_uri, .diagnostics = try list.toOwnedSlice(allocator) };
    }

    fn append(allocator: Allocator, list: *std.ArrayList(D.Diagnostic), sets: []const D.PublishDiagnostics, uri: []const u8) Allocator.Error!void {
        for (sets) |set| {
            if (!std.mem.eql(u8, uri, set.uri)) continue;
            for (set.diagnostics) |diag| {
                var duplicate = false;
                for (list.items) |existing| {
                    if (!lessThan({}, existing, diag) and !lessThan({}, diag, existing)) {
                        duplicate = true;
                        break;
                    }
                }
                if (duplicate) continue;
                var copy = diag;
                copy.message = try allocator.dupe(u8, diag.message);
                errdefer allocator.free(copy.message);
                try list.append(allocator, copy);
            }
        }
    }

    fn lessThan(_: void, a: D.Diagnostic, b: D.Diagnostic) bool {
        inline for (.{ "start", "end" }) |field| {
            const ap = @field(a.range, field);
            const bp = @field(b.range, field);
            if (ap.line != bp.line) return ap.line < bp.line;
            if (ap.character != bp.character) return ap.character < bp.character;
        }
        if ((a.severity == null) != (b.severity == null)) return a.severity == null;
        if (a.severity != b.severity) return (a.severity orelse 0) < (b.severity orelse 0);
        if ((a.source == null) != (b.source == null)) return a.source == null;
        const source_order = std.mem.order(u8, a.source orelse "", b.source orelse "");
        if (source_order != .eq) return source_order == .lt;
        return std.mem.lessThan(u8, a.message, b.message);
    }

    pub fn freeSets(allocator: Allocator, sets: []D.PublishDiagnostics) void {
        for (sets) |*set| set.deinit(allocator);
        allocator.free(sets);
    }
};

test "ROC-12252 own empty result takes precedence and closing exposes saved reports" {
    const a = std.testing.allocator;
    var contributions: Contributions = .{};
    defer contributions.deinit(a);
    const imported = try a.alloc(D.PublishDiagnostics, 1);
    imported[0] = .{ .uri = try a.dupe(u8, "dependency"), .diagnostics = try a.alloc(D.Diagnostic, 1) };
    imported[0].diagnostics[0] = .{ .range = .{ .start = .{ .line = 0, .character = 0 }, .end = .{ .line = 0, .character = 1 } }, .message = try a.dupe(u8, "saved error") };
    try contributions.replace(a, "importer", imported);
    try contributions.replace(a, "dependency", try a.alloc(D.PublishDiagnostics, 0));
    var own = try contributions.collect(a, "dependency");
    defer own.deinit(a);
    try std.testing.expectEqual(@as(usize, 0), own.diagnostics.len);
    contributions.remove(a, "dependency");
    var saved = try contributions.collect(a, "dependency");
    defer saved.deinit(a);
    try std.testing.expectEqual(@as(usize, 1), saved.diagnostics.len);
}

fn testDiagnostic(allocator: Allocator, message: []const u8, start_line: u32, start_character: u32, end_line: u32, end_character: u32, severity: ?u32, source: ?[]const u8) Allocator.Error!D.Diagnostic {
    return .{
        .range = .{
            .start = .{ .line = start_line, .character = start_character },
            .end = .{ .line = end_line, .character = end_character },
        },
        .severity = severity,
        .source = source,
        .message = try allocator.dupe(u8, message),
    };
}

fn testReport(allocator: Allocator, uri: []const u8, diagnostics: []D.Diagnostic) Allocator.Error![]D.PublishDiagnostics {
    const sets = try allocator.alloc(D.PublishDiagnostics, 1);
    sets[0] = .{ .uri = try allocator.dupe(u8, uri), .diagnostics = diagnostics };
    return sets;
}

test "closed URI collects the union across roots in either insertion order" {
    const a = std.testing.allocator;
    const uri = "file:///dependency.roc";
    for ([_]bool{ false, true }) |reverse| {
        var contributions: Contributions = .{};
        defer contributions.deinit(a);

        const first_diag = try testDiagnostic(a, "from first", 0, 0, 0, 1, 1, "compiler");
        const first_items = try a.alloc(D.Diagnostic, 1);
        first_items[0] = first_diag;
        const first_sets = try testReport(a, uri, first_items);
        const second_diag = try testDiagnostic(a, "from second", 1, 0, 1, 2, 2, "compiler");
        const second_items = try a.alloc(D.Diagnostic, 1);
        second_items[0] = second_diag;
        const second_sets = try testReport(a, uri, second_items);

        if (reverse) {
            try contributions.replace(a, "root-b", second_sets);
            try contributions.replace(a, "root-a", first_sets);
        } else {
            try contributions.replace(a, "root-a", first_sets);
            try contributions.replace(a, "root-b", second_sets);
        }
        var collected = try contributions.collect(a, uri);
        defer collected.deinit(a);
        try std.testing.expectEqual(@as(usize, 2), collected.diagnostics.len);
        try std.testing.expectEqualStrings("from first", collected.diagnostics[0].message);
        try std.testing.expectEqualStrings("from second", collected.diagnostics[1].message);
    }
}

test "closed URI deduplicates complete payload and stably retains distinct payloads" {
    const a = std.testing.allocator;
    const uri = "file:///dependency.roc";
    var contributions: Contributions = .{};
    defer contributions.deinit(a);

    const left_items = try a.alloc(D.Diagnostic, 4);
    left_items[0] = try testDiagnostic(a, "same", 0, 0, 0, 1, 1, "compiler");
    left_items[1] = try testDiagnostic(a, "same", 0, 0, 0, 1, 2, "compiler");
    left_items[2] = try testDiagnostic(a, "same", 0, 0, 0, 1, 1, "other");
    left_items[3] = try testDiagnostic(a, "same", 0, 0, 0, 2, 1, "compiler");
    try contributions.replace(a, "root-a", try testReport(a, uri, left_items));

    const right_items = try a.alloc(D.Diagnostic, 2);
    right_items[0] = try testDiagnostic(a, "same", 0, 0, 0, 1, 1, "compiler");
    right_items[1] = try testDiagnostic(a, "different message", 0, 0, 0, 1, 1, "compiler");
    try contributions.replace(a, "root-b", try testReport(a, uri, right_items));

    var collected = try contributions.collect(a, uri);
    defer collected.deinit(a);
    try std.testing.expectEqual(@as(usize, 5), collected.diagnostics.len);
    try expectDiagnostic(collected.diagnostics[0], 0, 1, "compiler", "different message");
    try expectDiagnostic(collected.diagnostics[1], 0, 1, "compiler", "same");
    try expectDiagnostic(collected.diagnostics[2], 0, 1, "other", "same");
    try expectDiagnostic(collected.diagnostics[3], 0, 2, "compiler", "same");
    try expectDiagnostic(collected.diagnostics[4], 0, 1, "compiler", "same");
    try std.testing.expectEqual(@as(u32, 1), collected.diagnostics[0].severity.?);
    try std.testing.expectEqual(@as(u32, 2), collected.diagnostics[3].severity.?);
    try std.testing.expectEqual(@as(u32, 2), collected.diagnostics[4].range.end.character);
}

fn expectDiagnostic(diag: D.Diagnostic, start_character: u32, severity: u32, source: []const u8, message: []const u8) error{TestExpectedEqual}!void {
    try std.testing.expectEqual(@as(u32, 0), diag.range.start.line);
    try std.testing.expectEqual(start_character, diag.range.start.character);
    try std.testing.expectEqual(severity, diag.severity.?);
    try std.testing.expectEqualStrings(source, diag.source.?);
    try std.testing.expectEqualStrings(message, diag.message);
}

test "replacing or removing a root leaves other root contributions intact" {
    const a = std.testing.allocator;
    const uri = "file:///dependency.roc";
    var contributions: Contributions = .{};
    defer contributions.deinit(a);

    const retained = try a.alloc(D.Diagnostic, 1);
    retained[0] = try testDiagnostic(a, "retained", 2, 0, 2, 1, 1, "compiler");
    try contributions.replace(a, "root-b", try testReport(a, uri, retained));
    const original = try a.alloc(D.Diagnostic, 1);
    original[0] = try testDiagnostic(a, "old", 0, 0, 0, 1, 1, "compiler");
    try contributions.replace(a, "root-a", try testReport(a, uri, original));

    const replacement = try a.alloc(D.Diagnostic, 1);
    replacement[0] = try testDiagnostic(a, "replacement", 1, 0, 1, 1, 1, "compiler");
    try contributions.replace(a, "root-a", try testReport(a, uri, replacement));
    var after_replace = try contributions.collect(a, uri);
    defer after_replace.deinit(a);
    try std.testing.expectEqual(@as(usize, 2), after_replace.diagnostics.len);
    try std.testing.expectEqualStrings("replacement", after_replace.diagnostics[0].message);
    try std.testing.expectEqualStrings("retained", after_replace.diagnostics[1].message);

    contributions.remove(a, "root-a");
    var after_remove = try contributions.collect(a, uri);
    defer after_remove.deinit(a);
    try std.testing.expectEqual(@as(usize, 1), after_remove.diagnostics.len);
    try std.testing.expectEqualStrings("retained", after_remove.diagnostics[0].message);
}

test "open URI conflicting own report takes precedence over other roots" {
    const a = std.testing.allocator;
    const uri = "file:///dependency.roc";
    var contributions: Contributions = .{};
    defer contributions.deinit(a);

    const saved_items = try a.alloc(D.Diagnostic, 1);
    saved_items[0] = try testDiagnostic(a, "saved", 0, 0, 0, 1, 1, "compiler");
    try contributions.replace(a, "importer", try testReport(a, uri, saved_items));
    const own_items = try a.alloc(D.Diagnostic, 1);
    own_items[0] = try testDiagnostic(a, "open buffer", 0, 0, 0, 1, 1, "compiler");
    try contributions.replace(a, uri, try testReport(a, uri, own_items));

    var collected = try contributions.collect(a, uri);
    defer collected.deinit(a);
    try std.testing.expectEqual(@as(usize, 1), collected.diagnostics.len);
    try std.testing.expectEqualStrings("open buffer", collected.diagnostics[0].message);
}
