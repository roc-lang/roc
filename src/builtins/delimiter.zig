//! First complete delimiter search for string patterns. Short literals fit an
//! inline comparison; long literals use a Two-Way search with a constant-size
//! plan derived from the literal, so repeated prefixes cannot multiply work.

const std = @import("std");

/// The longest delimiter searched by inline comparison instead of a `Plan`.
pub const max_inline_len = 16;

/// The Two-Way critical factorization of a delimiter.
pub const Plan = struct {
    /// Where the delimiter splits into the prefix and its maximal suffix.
    cut: usize,
    /// Zero when the prefix does not repeat with the suffix's period.
    period: usize,

    pub fn init(text: []const u8) Plan {
        std.debug.assert(text.len != 0);
        const a = maximalSuffix(text, false);
        const b = maximalSuffix(text, true);
        const plan = if (b.cut > a.cut) b else a;
        return if (std.mem.eql(u8, text[0..plan.cut], text[plan.period..][0..plan.cut]))
            plan
        else
            .{ .cut = plan.cut, .period = 0 };
    }

    /// The maximal suffix under one of the two byte orders, and its period.
    fn maximalSuffix(text: []const u8, reverse: bool) Plan {
        var cut: usize = 0;
        var j: usize = 0;
        var k: usize = 1;
        var period: usize = 1;
        while (k < text.len - j) {
            const a = text[cut + k - 1];
            const b = text[j + k];
            if (a == b) {
                if (k == period) {
                    j += period;
                    k = 1;
                } else k += 1;
            } else if ((a > b) != reverse) {
                j += k;
                k = 1;
                period = j + 1 - cut;
            } else {
                cut = j + 1;
                j = cut;
                k = 1;
                period = 1;
            }
        }
        return .{ .cut = cut, .period = period };
    }

    /// Search from `start`, returning the source length on a miss. The
    /// nonempty needle makes this sentinel distinct from every valid start.
    pub fn find(plan: Plan, source: []const u8, start: usize, needle: []const u8) usize {
        std.debug.assert(start <= source.len and needle.len != 0);
        if (needle.len > source.len - start) return source.len;
        // Ordinary text often contains no false starts. One vectorized
        // scan and comparison handles it without entering the Two-Way
        // loop. Do this only once: repeating the full comparison for each
        // candidate would make periodic input quadratic.
        const last = source.len - needle.len;
        const first = std.mem.findScalarPos(u8, source[0 .. last + 1], start, needle[0]) orelse return source.len;
        if (std.mem.eql(u8, source[first..][0..needle.len], needle)) return first;
        const shift = if (plan.period != 0) plan.period else @max(plan.cut, needle.len - plan.cut + 1);
        const overlap = if (plan.period != 0) needle.len - shift else 0;
        var pos = first + 1;
        var known: usize = 0;
        while (pos <= source.len - needle.len) {
            // Bulk-skip positions whose final byte cannot match. The
            // searched suffix advances monotonically, including when the
            // needle is periodic; a skip discards the overlap proof.
            const end = pos + (needle.len - 1);
            if (source[end] != needle[needle.len - 1]) {
                const next = std.mem.findScalarPos(u8, source, end + 1, needle[needle.len - 1]) orelse return source.len;
                pos += next - end;
                known = 0;
            }
            var i = @max(plan.cut, known);
            while (i < needle.len and needle[i] == source[pos + i]) i += 1;
            if (i < needle.len) {
                pos += i - plan.cut + 1;
                known = 0;
                continue;
            }
            i = plan.cut;
            while (i > known and needle[i - 1] == source[pos + i - 1]) i -= 1;
            if (i <= known) return pos;
            pos += shift;
            known = overlap;
        }
        return source.len;
    }
};

test "Two-Way search agrees with a direct search over every short binary word" {
    var source: [8]u8 = undefined;
    var needle: [5]u8 = undefined;
    for (1..needle.len + 1) |m| {
        for (0..@as(usize, 1) << @intCast(m)) |bits| {
            for (needle[0..m], 0..) |*b, i| b.* = @intCast((bits >> @intCast(i)) & 1);
            const plan = Plan.init(needle[0..m]);
            for (0..1 << source.len) |word| {
                for (&source, 0..) |*b, i| b.* = @intCast((word >> @intCast(i)) & 1);
                for (0..source.len + 1) |start| {
                    const expected = std.mem.findPos(u8, &source, start, needle[0..m]) orelse source.len;
                    try std.testing.expectEqual(expected, plan.find(&source, start, needle[0..m]));
                }
            }
        }
    }
}
