//! How each field of a compiler phase's mutable state relates to that phase's
//! definitional frames (design.md "Frame-Partitioned Phase State").
//!
//! An entry recorded while one lambda, declaration, or binding group is being
//! processed must not be consumed by a different one. A phase that keeps such
//! entries in shared storage and relies on every consumer to skip the entries
//! that are not its own is one forgotten filter away from resolving an entry
//! against the wrong frame. Every field of `Check` and `Can` therefore carries
//! exactly one `Discipline` in an `inventory` declared next to the struct; the
//! inventory is an exhaustive `std.enums.EnumArray` over the struct's fields,
//! so adding a field without classifying it, or classifying a field that does
//! not exist, is a compile error.

const std = @import("std");

/// The one discipline a field of phase state follows.
pub const Discipline = enum {
    /// Borrowed inputs, configuration, and the stores the phase writes its
    /// results into. Not deferred work and not frame-owned.
    input,
    /// A reusable buffer whose contents mean nothing between uses: each use
    /// starts it empty, or pushes above the length it found and truncates
    /// back to that length before returning, so nested uses compose.
    scratch,
    /// An explicit stack of the frames currently open. Pushes and pops pair up,
    /// a pop names the frame it closes, and the stack is empty when the
    /// phase's entry point returns.
    frame_stack,
    /// Storage whose entries belong to one frame by construction: the frame
    /// records where its entries begin (or the entries are keyed by the
    /// frame's identity), and the frame consumes exactly its own entries when
    /// it closes. No consumer filters a shared list by tag to find its entries.
    frame_partitioned,
    /// What is in flight right now: a flag, a current-item slot, or an
    /// in-flight membership set, saved on entry to a construct and restored on
    /// every exit from it.
    lexical,
    /// Work judged at each generalization boundary against the boundary's own
    /// rank: an entry whose rank the boundary generalizes is the boundary's to
    /// judge, and an entry at an enclosing rank stays pending for the boundary
    /// that owns that rank, with the module's final pass judging what remains.
    /// Rank is the solver's own record of frame ownership, so the decision is
    /// an exact ownership test, not a tag the producer chose.
    boundary,
    /// Work deliberately judged once, after every definition is checked,
    /// against the settled solved state. The field's doc comment states why
    /// judging it any earlier would be wrong.
    settled,
    /// Memos, indices, and output keyed by stable identities. An entry means
    /// the same thing to every frame that reads it, so it is never drained
    /// per frame.
    keyed,
};

/// The classification of every field of `T`, built by `inventory`.
pub fn Inventory(comptime T: type) type {
    @setEvalBranchQuota(evalBranchQuota(T));
    return std.enums.EnumArray(std.meta.FieldEnum(T), Discipline);
}

fn evalBranchQuota(comptime T: type) u32 {
    return 64 * @typeInfo(T).@"struct".field_names.len + 1000;
}

/// Classify every field of `T`: `inventory(T, .{ .field = .discipline, ... })`.
/// A missing or unknown field name is a compile error.
pub fn inventory(
    comptime T: type,
    comptime classes: std.enums.EnumFieldStruct(std.meta.FieldEnum(T), Discipline, null),
) Inventory(T) {
    @setEvalBranchQuota(evalBranchQuota(T));
    return comptime Inventory(T).init(classes);
}

/// The names of `T`'s fields that `classes` gives one of `disciplines`, in
/// declaration order.
pub fn fieldsWith(
    comptime T: type,
    comptime classes: Inventory(T),
    comptime disciplines: []const Discipline,
) []const [:0]const u8 {
    comptime {
        @setEvalBranchQuota(evalBranchQuota(T) * (disciplines.len + 1));
        const names = @typeInfo(T).@"struct".field_names;
        var selected: [names.len][:0]const u8 = undefined;
        var count: usize = 0;
        for (names, 0..) |name, index| {
            const discipline = classes.values[index];
            for (disciplines) |wanted| {
                if (discipline == wanted) {
                    selected[count] = name;
                    count += 1;
                    break;
                }
            }
        }
        const result = selected[0..count].*;
        return &result;
    }
}

/// How many entries a frame-owned field holds: a stack or list's length, a
/// map's count, or a scratch buffer's top.
pub fn entryCount(value: anytype) usize {
    const T = @TypeOf(value);
    if (comptime @hasDecl(T, "count")) return value.count();
    if (comptime @hasDecl(T, "top")) return value.top();
    return value.items.len;
}

test "an inventory classifies exactly the fields of its struct" {
    const Example = struct {
        input: u32,
        frames: std.ArrayListUnmanaged(u32),
        owned: std.ArrayListUnmanaged(u32),
    };
    const classes = comptime inventory(Example, .{
        .input = .input,
        .frames = .frame_stack,
        .owned = .frame_partitioned,
    });
    const frame_owned = comptime fieldsWith(Example, classes, &.{ .frame_stack, .frame_partitioned });
    try std.testing.expectEqual(@as(usize, 2), frame_owned.len);
    try std.testing.expectEqualStrings("frames", frame_owned[0]);
    try std.testing.expectEqualStrings("owned", frame_owned[1]);
    try std.testing.expectEqual(@as(usize, 3), Inventory(Example).len);
}
