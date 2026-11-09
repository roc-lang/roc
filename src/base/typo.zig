//! Edit distance between short strings, for suggesting the intended spelling
//! of a misspelled name in error reports.

const std = @import("std");

/// Calculate Damerau-Levenshtein edit distance between two strings.
/// Returns the minimum number of edits (insertions, deletions, substitutions,
/// or transpositions of adjacent characters) needed to transform a into b.
pub fn editDistance(a: []const u8, b: []const u8) u32 {
    // Short-circuit for empty strings
    if (a.len == 0) return @intCast(b.len);
    if (b.len == 0) return @intCast(a.len);

    // Use a simple matrix approach for small strings
    // For strings up to ~50 chars, this is efficient enough
    const max_len = 64;
    if (a.len > max_len or b.len > max_len) {
        // For very long strings, fall back to max length as distance
        return @intCast(@max(a.len, b.len));
    }

    // Damerau-Levenshtein with two-row optimization
    var prev_prev: [max_len + 1]u32 = undefined;
    var prev: [max_len + 1]u32 = undefined;
    var curr: [max_len + 1]u32 = undefined;

    // Initialize first row
    for (0..b.len + 1) |j| {
        prev[j] = @intCast(j);
    }

    for (a, 0..) |ca, i| {
        curr[0] = @intCast(i + 1);

        for (b, 0..) |cb, j| {
            const cost: u32 = if (ca == cb) 0 else 1;

            curr[j + 1] = @min(
                @min(
                    prev[j + 1] + 1, // deletion
                    curr[j] + 1, // insertion
                ),
                prev[j] + cost, // substitution
            );

            // Transposition
            if (i > 0 and j > 0 and ca == b[j - 1] and a[i - 1] == cb) {
                curr[j + 1] = @min(curr[j + 1], prev_prev[j - 1] + cost);
            }
        }

        // Rotate rows
        const tmp = prev_prev;
        prev_prev = prev;
        prev = curr;
        curr = tmp;
    }

    return prev[b.len];
}

/// Check if a string is a likely typo of another.
/// Uses edit distance with length-dependent thresholds.
pub fn isLikelyTypo(typo_len: usize, correct_len: usize, dist: u32) bool {
    // dist > 0 ensures we don't suggest exact matches
    if (dist == 0) return false;

    const min_len = @min(typo_len, correct_len);

    // Short strings (1-2 chars): only allow edit distance of 1
    if (min_len <= 2) return dist == 1;

    // Longer strings: allow edit distance of 1-2
    return dist <= 2;
}

test "editDistance - identical strings" {
    try std.testing.expectEqual(@as(u32, 0), editDistance("hello", "hello"));
}

test "editDistance - empty strings" {
    try std.testing.expectEqual(@as(u32, 5), editDistance("", "hello"));
    try std.testing.expectEqual(@as(u32, 5), editDistance("hello", ""));
    try std.testing.expectEqual(@as(u32, 0), editDistance("", ""));
}

test "editDistance - single substitution" {
    try std.testing.expectEqual(@as(u32, 1), editDistance("name", "nme"));
    try std.testing.expectEqual(@as(u32, 1), editDistance("cat", "car"));
}

test "editDistance - transposition" {
    // spellchecker:ignore-next-line
    try std.testing.expectEqual(@as(u32, 1), editDistance("teh", "the"));
    // spellchecker:ignore-next-line
    try std.testing.expectEqual(@as(u32, 1), editDistance("ab", "ba"));
}

test "editDistance - insertion" {
    // spellchecker:ignore-next-line
    try std.testing.expectEqual(@as(u32, 1), editDistance("name", "naame"));

    // spellchecker:ignore-next-line
    try std.testing.expectEqual(@as(u32, 1), editDistance("helo", "hello"));
}

test "editDistance - deletion" {
    // spellchecker:ignore-next-line
    try std.testing.expectEqual(@as(u32, 1), editDistance("hello", "helo"));
}

test "isLikelyTypo - valid typos" {
    try std.testing.expect(blk: {
        const typo = "nme";
        const actual = "name";
        const dist = editDistance(typo, actual);
        break :blk isLikelyTypo(typo.len, actual.len, dist);
    });
    try std.testing.expect(blk: {
        // spellchecker:ignore-next-line
        const typo = "teh";
        const actual = "the";
        const dist = editDistance(typo, actual);
        break :blk isLikelyTypo(typo.len, actual.len, dist);
    });
    try std.testing.expect(blk: {
        // spellchecker:ignore-next-line
        const typo = "feild";
        const actual = "field";
        const dist = editDistance(typo, actual);
        break :blk isLikelyTypo(typo.len, actual.len, dist);
    });
    try std.testing.expect(blk: {
        // spellchecker:ignore-next-line
        const typo = "recieve";
        const actual = "receive";
        const dist = editDistance(typo, actual);
        break :blk isLikelyTypo(typo.len, actual.len, dist);
    });
}

test "isLikelyTypo - not typos" {
    try std.testing.expect(blk: {
        const typo = "foo";
        const actual = "bar";
        const dist = editDistance(typo, actual);
        break :blk !isLikelyTypo(typo.len, actual.len, dist);
    });
    try std.testing.expect(blk: {
        const typo = "completely";
        const actual = "different";
        const dist = editDistance(typo, actual);
        break :blk !isLikelyTypo(typo.len, actual.len, dist);
    });
    try std.testing.expect(blk: {
        const typo = "name";
        const actual = "name";
        const dist = editDistance(typo, actual);
        break :blk !isLikelyTypo(typo.len, actual.len, dist);
    }); // exact match
}

test "isLikelyTypo - short strings" {
    try std.testing.expect(blk: {
        const typo = "a";
        const actual = "b";
        const dist = editDistance(typo, actual);
        break :blk isLikelyTypo(typo.len, actual.len, dist);
    }); // single char, dist=1
    try std.testing.expect(blk: {
        const typo = "a";
        const actual = "abc";
        const dist = editDistance(typo, actual);
        break :blk !isLikelyTypo(typo.len, actual.len, dist);
    }); // dist=2, but len=1, so no
}
