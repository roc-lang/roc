//! "Did you mean ...?" suggestions: which of the names valid where a name was
//! written is close enough to it to be what the writer meant.

const std = @import("std");
const Allocator = std.mem.Allocator;

/// The index of the candidate `written` most likely misspells, or null when
/// no candidate is close enough. Closeness is the optimal string alignment
/// distance (insertions, deletions, substitutions, and swaps of adjacent
/// characters), and a candidate is close enough when that distance is at
/// most a third of `written`'s length, or 1 for names shorter than 6
/// characters. Ties go to the earlier candidate.
pub fn closestCandidate(allocator: Allocator, written: []const u8, candidates: []const []const u8) Allocator.Error!?usize {
    const max_distance = @max(1, written.len / 3);
    var best: ?usize = null;
    var best_distance: usize = max_distance + 1;
    for (candidates, 0..) |candidate, index| {
        const distance = try editDistance(allocator, written, candidate);
        if (distance < best_distance) {
            best = index;
            best_distance = distance;
        }
    }
    return best;
}

/// The optimal string alignment distance between `a` and `b`.
pub fn editDistance(allocator: Allocator, a: []const u8, b: []const u8) Allocator.Error!usize {
    const width = b.len + 1;
    const rows = try allocator.alloc(usize, 3 * width);
    defer allocator.free(rows);
    var two_back = rows[0..width];
    var previous = rows[width .. 2 * width];
    var current = rows[2 * width ..];

    for (previous, 0..) |*cell, j| cell.* = j;
    for (a, 1..) |a_char, i| {
        current[0] = i;
        for (b, 1..) |b_char, j| {
            const cost: usize = @intFromBool(a_char != b_char);
            var distance = @min(@min(previous[j] + 1, current[j - 1] + 1), previous[j - 1] + cost);
            if (i > 1 and j > 1 and a_char == b[j - 2] and a[i - 2] == b_char) {
                distance = @min(distance, two_back[j - 2] + 1);
            }
            current[j] = distance;
        }
        const recycled = two_back;
        two_back = previous;
        previous = current;
        current = recycled;
    }
    return previous[b.len];
}

test "editDistance counts edits and adjacent swaps" {
    const gpa = std.testing.allocator;
    try std.testing.expectEqual(@as(usize, 0), try editDistance(gpa, "speed", "speed"));
    try std.testing.expectEqual(@as(usize, 1), try editDistance(gpa, "sped", "speed"));
    try std.testing.expectEqual(@as(usize, 1), try editDistance(gpa, "seped", "speed"));
    try std.testing.expectEqual(@as(usize, 1), try editDistance(gpa, "spead", "speed"));
    try std.testing.expectEqual(@as(usize, 3), try editDistance(gpa, "", "dev"));
    try std.testing.expectEqual(@as(usize, 3), try editDistance(gpa, "dev", ""));
}

test "closestCandidate suggests only likely misspellings" {
    const gpa = std.testing.allocator;
    const levels = [_][]const u8{ "dev", "interpreter", "speed", "size" };
    try std.testing.expectEqual(@as(?usize, 2), try closestCandidate(gpa, "sped", &levels));
    try std.testing.expectEqual(@as(?usize, 1), try closestCandidate(gpa, "interpeter", &levels));
    try std.testing.expectEqual(@as(?usize, null), try closestCandidate(gpa, "less_nice", &levels));
    try std.testing.expectEqual(@as(?usize, null), try closestCandidate(gpa, "x", &levels));
}
