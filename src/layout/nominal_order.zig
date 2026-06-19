//! Nominal record field ordering.
//!
//! Structural records lay their fields out by descending alignment, so source
//! order never reaches memory. Nominal records instead keep declared order so a
//! Roc type can mirror a chosen C struct exactly. The one invariant both share
//! is that a committed struct has no internal alignment padding between fields.
//!
//! This module turns a nominal record's declared field order into a no-padding
//! order that stays as close to declared order as that invariant allows. When
//! the declared order is already padding-free it is kept verbatim; otherwise it
//! is repaired into the no-padding order that is lexicographically closest to
//! declared order. A repaired order always exists because descending alignment
//! witnesses one from offset zero, so this is total and never an error.
//!
//! See design.md "Nominal Record Field Order".

const std = @import("std");
const Allocator = std.mem.Allocator;

/// The size and alignment of one declared field, in declared order.
pub const FieldShape = struct {
    /// Size in bytes. Always a multiple of `alignment` for well-formed layouts.
    size: u32,
    /// Alignment in bytes; a power of two. Unnamed spacer fields pass 1, since
    /// their bytes are uninitialized and impose no alignment requirement.
    alignment: u32,
};

/// Computes the in-memory field order for a nominal record.
///
/// `fields` are in declared order. `out_order` is filled with a permutation of
/// `0..fields.len`: `out_order[k]` is the index into `fields` of the field that
/// occupies memory slot `k`. Laying the fields out in that order introduces no
/// internal alignment padding, and the order is the one lexicographically
/// closest to declared order with that property.
pub fn computeNominalFieldOrder(
    gpa: Allocator,
    fields: []const FieldShape,
    out_order: []u16,
) Allocator.Error!void {
    std.debug.assert(out_order.len == fields.len);

    for (out_order, 0..) |*slot, i| slot.* = @intCast(i);

    // Fast path: the declared order is already padding-free. This is the common
    // case, including hand-tuned C-mirroring layouts, and keeps them verbatim.
    if (orderIsPaddingFree(fields, out_order)) return;

    try repair(gpa, fields, out_order);
}

/// Whether laying `fields` out in `order` leaves no internal padding.
fn orderIsPaddingFree(fields: []const FieldShape, order: []const u16) bool {
    var offset: u64 = 0;
    for (order) |i| {
        const field = fields[i];
        if (offset % field.alignment != 0) return false;
        offset += field.size;
    }
    return true;
}

/// Repairs a declared order that would require internal padding into the
/// no-padding order lexicographically closest to it.
fn repair(gpa: Allocator, fields: []const FieldShape, out_order: []u16) Allocator.Error!void {
    const n = fields.len;

    // Distinct (size, alignment) signatures. Feasibility of completing a partial
    // layout depends only on the running offset modulo the maximum alignment and
    // the multiset of remaining signatures, so the solver works over signatures
    // and their counts rather than individual fields.
    var signatures = std.ArrayList(FieldShape).empty;
    defer signatures.deinit(gpa);
    const field_sig = try gpa.alloc(u16, n);
    defer gpa.free(field_sig);

    var max_alignment: u32 = 1;
    for (fields, 0..) |field, i| {
        max_alignment = @max(max_alignment, field.alignment);
        field_sig[i] = blk: {
            for (signatures.items, 0..) |sig, s| {
                if (sig.size == field.size and sig.alignment == field.alignment) break :blk @intCast(s);
            }
            const new_id: u16 = @intCast(signatures.items.len);
            try signatures.append(gpa, field);
            break :blk new_id;
        };
    }

    // Remaining field count per signature, decremented as fields are placed.
    const counts = try gpa.alloc(u16, signatures.items.len);
    defer gpa.free(counts);
    @memset(counts, 0);
    for (field_sig) |sid| counts[sid] += 1;

    const placed = try gpa.alloc(bool, n);
    defer gpa.free(placed);
    @memset(placed, false);

    // Memo-key buffer, sized to the exact worst case (`offset_mod` u32 + one u16
    // per distinct signature) so it can never overflow regardless of field count.
    const key_scratch = try gpa.alloc(u8, 4 + signatures.items.len * 2);
    defer gpa.free(key_scratch);

    var solver = Solver{
        .signatures = signatures.items,
        .counts = counts,
        .max_alignment = max_alignment,
        .memo = std.StringHashMap(bool).init(gpa),
        .key_arena = std.heap.ArenaAllocator.init(gpa),
        .key_scratch = key_scratch,
    };
    defer solver.deinit();

    var offset_mod: u32 = 0;
    var slot: usize = 0;
    while (slot < n) : (slot += 1) {
        // Pick the earliest declared field that fits at the current offset and
        // still leaves the remaining fields completable with no padding.
        var chosen: ?usize = null;
        for (0..n) |i| {
            if (placed[i]) continue;
            const field = fields[i];
            if (offset_mod % field.alignment != 0) continue;

            const sid = field_sig[i];
            counts[sid] -= 1;
            const completable = try solver.feasible((offset_mod + field.size) % max_alignment);
            counts[sid] += 1;
            if (completable) {
                chosen = i;
                break;
            }
        }

        // A completion always exists from a feasible state, and every committed
        // placement preserves feasibility, so a choice is always available.
        const pick = chosen.?;
        placed[pick] = true;
        counts[field_sig[pick]] -= 1;
        out_order[slot] = @intCast(pick);
        offset_mod = (offset_mod + fields[pick].size) % max_alignment;
    }
}

/// Memoized feasibility solver over remaining-signature counts.
const Solver = struct {
    signatures: []const FieldShape,
    /// Remaining count per signature; mutated in place during the search and
    /// restored before returning, so it is the live count between calls.
    counts: []u16,
    max_alignment: u32,
    memo: std.StringHashMap(bool),
    key_arena: std.heap.ArenaAllocator,
    /// Memo-key scratch, sized by the caller to `4 + signatures.len * 2`.
    key_scratch: []u8,

    fn deinit(self: *Solver) void {
        self.memo.deinit();
        self.key_arena.deinit();
    }

    /// Whether the remaining fields (per `counts`) can be laid out with no
    /// internal padding starting from an offset congruent to `offset_mod`.
    fn feasible(self: *Solver, offset_mod: u32) Allocator.Error!bool {
        var any_remaining = false;
        for (self.counts) |c| {
            if (c != 0) {
                any_remaining = true;
                break;
            }
        }
        if (!any_remaining) return true;

        if (self.memo.get(self.encodeKey(offset_mod))) |cached| return cached;

        var result = false;
        for (self.signatures, 0..) |sig, s| {
            if (self.counts[s] == 0) continue;
            if (offset_mod % sig.alignment != 0) continue;

            self.counts[s] -= 1;
            const rest = try self.feasible((offset_mod + sig.size) % self.max_alignment);
            self.counts[s] += 1;
            if (rest) {
                result = true;
                break;
            }
        }

        // `counts` is restored to its on-entry value, so re-encoding reproduces
        // the same key bytes; the recursive search clobbered the scratch buffer.
        const owned_key = try self.key_arena.allocator().dupe(u8, self.encodeKey(offset_mod));
        try self.memo.put(owned_key, result);
        return result;
    }

    /// Encodes (offset_mod, counts) into an exact byte key, written into the
    /// instance scratch buffer. The key is unique per state, so the memo never
    /// returns a false hit. The returned slice is valid until the next call.
    fn encodeKey(self: *Solver, offset_mod: u32) []const u8 {
        const needed = 4 + self.counts.len * 2;
        std.debug.assert(needed <= self.key_scratch.len);
        std.mem.writeInt(u32, self.key_scratch[0..4], offset_mod, .little);
        for (self.counts, 0..) |c, i| {
            std.mem.writeInt(u16, self.key_scratch[4 + i * 2 ..][0..2], c, .little);
        }
        return self.key_scratch[0..needed];
    }
};

const testing = std.testing;

/// Convenience for tests: build shapes from (size, alignment) pairs.
fn shapes(comptime pairs: anytype) [pairs.len]FieldShape {
    var out: [pairs.len]FieldShape = undefined;
    inline for (pairs, 0..) |pair, i| {
        out[i] = .{ .size = pair[0], .alignment = pair[1] };
    }
    return out;
}

fn expectOrder(fields: []const FieldShape, expected: []const u16) anyerror!void {
    var order: [64]u16 = undefined;
    try computeNominalFieldOrder(testing.allocator, fields, order[0..fields.len]);
    try testing.expect(orderIsPaddingFree(fields, order[0..fields.len]));
    try testing.expectEqualSlices(u16, expected, order[0..fields.len]);
}

test "padding-free declared order is kept verbatim" {
    // tb_account_t: six u128s, a u64, three u32s, two u16s, then a trailing u64.
    // The two u16s carry the offset to an 8-aligned boundary, so the trailing
    // u64 needs no padding even though it follows lower-alignment fields.
    const fields = shapes(.{
        .{ 16, 16 }, .{ 16, 16 }, .{ 16, 16 }, .{ 16, 16 }, .{ 16, 16 }, .{ 16, 16 },
        .{ 8, 8 },
        .{ 4, 4 }, .{ 4, 4 }, .{ 4, 4 },
        .{ 2, 2 }, .{ 2, 2 },
        .{ 8, 8 },
    });
    try expectOrder(&fields, &.{ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 });
}

test "simple aligned declared order is kept" {
    const fields = shapes(.{ .{ 8, 8 }, .{ 4, 4 }, .{ 2, 2 }, .{ 1, 1 } });
    try expectOrder(&fields, &.{ 0, 1, 2, 3 });
}

test "repair swaps the minimum needed to avoid padding" {
    // [u32, u8, u16]: u16 at offset 5 would need padding. The lexicographically
    // closest no-padding order keeps u32 first, then pulls u16 ahead of u8.
    const fields = shapes(.{ .{ 4, 4 }, .{ 1, 1 }, .{ 2, 2 } });
    try expectOrder(&fields, &.{ 0, 2, 1 });
}

test "repair defers a low-alignment field that would strand a later one" {
    // [u8, u16, u64]: placing u8 first strands everything; the only no-padding
    // orders start with the u64. Lexicographically closest keeps u16 before u8.
    const fields = shapes(.{ .{ 1, 1 }, .{ 2, 2 }, .{ 8, 8 } });
    try expectOrder(&fields, &.{ 2, 1, 0 });
}

test "repair preserves the longest valid declared prefix" {
    // [u64, u8, u32]: the u64 prefix is fine; u8 then u32 needs the u32 first.
    const fields = shapes(.{ .{ 8, 8 }, .{ 1, 1 }, .{ 4, 4 } });
    try expectOrder(&fields, &.{ 0, 2, 1 });
}

test "alignment-1 spacers can fill arbitrary gaps during repair" {
    // A u8 real field followed by three align-1 spacers (size 1) and a u32.
    // Declared order is already padding-free: u8 at 0, spacers at 1..3, u32 at 4.
    const fields = shapes(.{ .{ 1, 1 }, .{ 1, 1 }, .{ 1, 1 }, .{ 1, 1 }, .{ 4, 4 } });
    try expectOrder(&fields, &.{ 0, 1, 2, 3, 4 });
}

test "single field is trivially ordered" {
    const fields = shapes(.{.{ 16, 16 }});
    try expectOrder(&fields, &.{0});
}

test "all-equal fields keep declared order" {
    const fields = shapes(.{ .{ 4, 4 }, .{ 4, 4 }, .{ 4, 4 } });
    try expectOrder(&fields, &.{ 0, 1, 2 });
}

test "repair result is always padding-free for a larger shuffled record" {
    const fields = shapes(.{
        .{ 1, 1 }, .{ 8, 8 }, .{ 2, 2 }, .{ 16, 16 }, .{ 4, 4 },
        .{ 1, 1 }, .{ 8, 8 }, .{ 2, 2 }, .{ 4, 4 }, .{ 16, 16 },
    });
    var order: [10]u16 = undefined;
    try computeNominalFieldOrder(testing.allocator, &fields, &order);
    try testing.expect(orderIsPaddingFree(&fields, &order));
    // Every field index appears exactly once.
    var seen = [_]bool{false} ** 10;
    for (order) |i| {
        try testing.expect(!seen[i]);
        seen[i] = true;
    }
}
