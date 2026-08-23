//! Structural record/tuple field ordering used by the layout store when it
//! commits how a record lays out in memory. The `roc glue` generator does not
//! reorder fields itself; it consumes the committed field order the layout store
//! produced here, so both describe the same memory layout.
//!
//! Structural (anonymous) records and no-`_` nominal records lay their fields out
//! by descending sort key, then ascending canonical semantic index, so source
//! order never reaches memory. A record's semantic index is its alphabetical
//! field-name rank; a tuple's is its element index. The sort key is
//! target-independent (a pointer sorts between 4- and 8-byte alignment), so the
//! field order is identical on 32-bit and 64-bit targets.
//!
//! Nominal records that opt into declared-order layout (by including an unnamed
//! `_` field) are laid out verbatim with C-style padding directly by the layout
//! store and the glue generator, not here.

const std = @import("std");
const SortKey = @import("layout.zig").SortKey;

/// The total ordering key of one structural field.
pub const StructuralField = struct {
    /// Target-independent ordering key. A pointer sorts between 4- and 8-byte
    /// alignment, so the resulting field order is the same on 32-bit and 64-bit
    /// targets. Padding fields pass `.align_1`.
    sort_key: SortKey,
    /// Canonical semantic field index, used as the tie-break among equal sort
    /// keys. For records this is alphabetical field-name rank; for tuples it is
    /// the original element index.
    semantic_index: u16,
};

/// Whether `a` precedes `b` in canonical structural field order.
pub fn comesBefore(a: StructuralField, b: StructuralField) bool {
    if (a.sort_key != b.sort_key) return a.sort_key.sortsBefore(b.sort_key);
    return a.semantic_index < b.semantic_index;
}

/// Sort structural fields by their total canonical key.
///
/// `out_order` is filled with a permutation of `0..fields.len`: `out_order[k]`
/// is the index into `fields` of the field that occupies memory slot `k`. No
/// allocation is required.
pub fn computeStructuralFieldOrder(
    fields: []const StructuralField,
    out_order: []u16,
) void {
    std.debug.assert(out_order.len == fields.len);

    for (out_order, 0..) |*slot, i| slot.* = @intCast(i);

    const Ctx = struct {
        fields: []const StructuralField,

        pub fn lessThan(ctx: @This(), a: u16, b: u16) bool {
            return comesBefore(ctx.fields[a], ctx.fields[b]);
        }
    };

    std.sort.pdq(u16, out_order, Ctx{ .fields = fields }, Ctx.lessThan);
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
        .{ .sort_key = .align_1, .semantic_index = 0 },
        .{ .sort_key = .align_8, .semantic_index = 1 },
        .{ .sort_key = .align_2, .semantic_index = 2 },
    };
    try expectStructuralOrder(&fields, &.{ 1, 2, 0 });
}

test "structural order tie-breaks by ascending semantic index" {
    // All fields have the same sort key, so semantic index decides their order.
    const fields = [_]StructuralField{
        .{ .sort_key = .align_4, .semantic_index = 2 },
        .{ .sort_key = .align_4, .semantic_index = 1 },
        .{ .sort_key = .align_4, .semantic_index = 0 },
    };
    try expectStructuralOrder(&fields, &.{ 2, 1, 0 });
}

test "structural order tie-breaks by semantic index within each sort-key band" {
    const fields = [_]StructuralField{
        .{ .sort_key = .align_8, .semantic_index = 1 },
        .{ .sort_key = .align_4, .semantic_index = 3 },
        .{ .sort_key = .align_8, .semantic_index = 0 },
        .{ .sort_key = .align_4, .semantic_index = 2 },
    };
    try expectStructuralOrder(&fields, &.{ 2, 0, 3, 1 });
}

test "structural order places a pointer between 4- and 8-byte alignment" {
    // A pointer field sorts after align-8 scalars and before align-4 scalars,
    // regardless of target—the property that makes field order target-independent.
    const fields = [_]StructuralField{
        .{ .sort_key = .align_4, .semantic_index = 0 }, // U32
        .{ .sort_key = .pointer, .semantic_index = 1 }, // a pointer (Box/List/Str/...)
        .{ .sort_key = .align_8, .semantic_index = 2 }, // U64
    };
    try expectStructuralOrder(&fields, &.{ 2, 1, 0 });
}

test "structural order does not depend on input order" {
    const fields = [_]StructuralField{
        .{ .sort_key = .align_4, .semantic_index = 2 },
        .{ .sort_key = .align_8, .semantic_index = 1 },
        .{ .sort_key = .align_4, .semantic_index = 0 },
        .{ .sort_key = .align_8, .semantic_index = 3 },
    };
    try expectStructuralOrder(&fields, &.{ 1, 3, 2, 0 });
}

test "structural order of a single field is trivial" {
    const fields = [_]StructuralField{.{ .sort_key = .align_16, .semantic_index = 0 }};
    try expectStructuralOrder(&fields, &.{0});
}
