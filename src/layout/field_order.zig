//! Structural record/tuple field ordering, shared by the layout store and the
//! `roc glue` generator so the two can never disagree on how a record lays out in
//! memory.
//!
//! Structural (anonymous) records and no-`_` nominal records lay their fields out
//! by descending sort key, then ascending field name, so source order never
//! reaches memory. The sort key is target-independent (a pointer sorts between
//! 4- and 8-byte alignment), so the field order is identical on 32-bit and 64-bit
//! targets. `computeStructuralFieldOrder` produces that permutation.
//!
//! Nominal records that opt into declared-order layout (by including an unnamed
//! `_` field) are laid out verbatim with C-style padding directly by the layout
//! store and the glue generator, not here.

const std = @import("std");
const SortKey = @import("layout.zig").SortKey;

/// The sort key and name of one structural-record field.
pub const StructuralField = struct {
    /// Target-independent ordering key. A pointer sorts between 4- and 8-byte
    /// alignment, so the resulting field order is the same on 32-bit and 64-bit
    /// targets. Padding fields pass `.align_1`.
    sort_key: SortKey,
    /// Field name, used only as the tie-break among equal-sort-key fields.
    /// Pass `""` to fall back to a pure stable sort-key sort (the layout
    /// store's pair-struct path, whose callers presort by name elsewhere).
    name: []const u8,
};

/// Stable sort of structural-record fields: sort key descending, then field
/// name ascending. Empty names degrade to a pure stable sort-key sort,
/// preserving input order for equal sort keys (matching the layout store's
/// existing pair-struct behavior).
///
/// `out_order` is filled with a permutation of `0..fields.len`: `out_order[k]`
/// is the index into `fields` of the field that occupies memory slot `k`. No
/// allocation is required (the block sort works in place).
pub fn computeStructuralFieldOrder(
    fields: []const StructuralField,
    out_order: []u16,
) void {
    std.debug.assert(out_order.len == fields.len);

    for (out_order, 0..) |*slot, i| slot.* = @intCast(i);

    const Ctx = struct {
        fields: []const StructuralField,

        pub fn lessThan(ctx: @This(), a: u16, b: u16) bool {
            const fa = ctx.fields[a];
            const fb = ctx.fields[b];
            if (fa.sort_key != fb.sort_key) return fa.sort_key.sortsBefore(fb.sort_key);
            return std.mem.order(u8, fa.name, fb.name) == .lt;
        }
    };

    // `std.sort.block` is a stable sort, so equal-key fields keep input order.
    std.sort.block(u16, out_order, Ctx{ .fields = fields }, Ctx.lessThan);
}

const testing = std.testing;

fn expectStructuralOrder(fields: []const StructuralField, expected: []const u16) error{TestExpectedEqual}!void {
    var order: [64]u16 = undefined;
    computeStructuralFieldOrder(fields, order[0..fields.len]);
    try testing.expectEqualSlices(u16, expected, order[0..fields.len]);
}

test "structural order sorts by descending sort key" {
    // Declared [u8, u64, u16] sorts to [u64, u16, u8] by descending sort key.
    const fields = [_]StructuralField{
        .{ .sort_key = .align_1, .name = "a" },
        .{ .sort_key = .align_8, .name = "b" },
        .{ .sort_key = .align_2, .name = "c" },
    };
    try expectStructuralOrder(&fields, &.{ 1, 2, 0 });
}

test "structural order tie-breaks by ascending field name" {
    // All same sort key, so the order is purely by ascending name: c, m, z.
    const fields = [_]StructuralField{
        .{ .sort_key = .align_4, .name = "z" },
        .{ .sort_key = .align_4, .name = "m" },
        .{ .sort_key = .align_4, .name = "c" },
    };
    try expectStructuralOrder(&fields, &.{ 2, 1, 0 });
}

test "structural order tie-breaks by name within each sort-key band" {
    // align-8 band {y, a} sorts to a, y; align-4 band {x, b} sorts to b, x;
    // the align-8 band precedes the align-4 band.
    const fields = [_]StructuralField{
        .{ .sort_key = .align_8, .name = "y" },
        .{ .sort_key = .align_4, .name = "x" },
        .{ .sort_key = .align_8, .name = "a" },
        .{ .sort_key = .align_4, .name = "b" },
    };
    try expectStructuralOrder(&fields, &.{ 2, 0, 3, 1 });
}

test "structural order places a pointer between 4- and 8-byte alignment" {
    // A pointer field sorts after align-8 scalars and before align-4 scalars,
    // regardless of target — the property that makes field order target-independent.
    const fields = [_]StructuralField{
        .{ .sort_key = .align_4, .name = "a" }, // U32
        .{ .sort_key = .pointer, .name = "b" }, // a pointer (Box/List/Str/...)
        .{ .sort_key = .align_8, .name = "c" }, // U64
    };
    try expectStructuralOrder(&fields, &.{ 2, 1, 0 });
}

test "structural order with empty names is a stable sort-key sort" {
    // Empty names degrade to a pure stable sort-key sort: equal-key fields keep
    // their input order (matching the layout store's pair structs).
    const fields = [_]StructuralField{
        .{ .sort_key = .align_4, .name = "" },
        .{ .sort_key = .align_8, .name = "" },
        .{ .sort_key = .align_4, .name = "" },
        .{ .sort_key = .align_8, .name = "" },
    };
    try expectStructuralOrder(&fields, &.{ 1, 3, 0, 2 });
}

test "structural order of a single field is trivial" {
    const fields = [_]StructuralField{.{ .sort_key = .align_16, .name = "only" }};
    try expectStructuralOrder(&fields, &.{0});
}
