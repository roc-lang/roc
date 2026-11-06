//! The two kinds of checked type id. A source id is a variable of the
//! module's sealed solver store, the class's checked representative, with the
//! variable's own bit pattern. A synthesized id names an entry of the module's
//! synthesized type store: the high bit set, the entry index below it. Only
//! checking code tells the kinds apart; every other reader treats an id as
//! opaque and reads it through the module's type view.

const std = @import("std");
const types = @import("types");
const CheckedTypeId = @import("checked_ids.zig").CheckedTypeId;

const synthesized_bit: u32 = 1 << 31;

/// The id of `var_`, a checked representative of a sealed store.
pub fn fromVar(var_: types.Var) CheckedTypeId {
    const raw: u32 = @backingInt(var_);
    std.debug.assert(raw & synthesized_bit == 0);
    return @fromBackingInt(raw);
}

/// The ids of `vars`, checked representatives of a sealed store, aliasing
/// the same memory.
pub fn fromVars(vars: []const types.Var) []const CheckedTypeId {
    return @ptrCast(vars);
}

/// The id of entry `index` of a synthesized type store.
pub fn fromSynthesized(index: usize) CheckedTypeId {
    std.debug.assert(index < synthesized_bit);
    return @fromBackingInt(@as(u32, @intCast(index)) | synthesized_bit);
}

/// Whether `id` names a synthesized entry rather than a source variable.
pub fn isSynthesized(id: CheckedTypeId) bool {
    return @backingInt(id) & synthesized_bit != 0;
}

/// The source variable `id` names.
pub fn asVar(id: CheckedTypeId) types.Var {
    std.debug.assert(!isSynthesized(id));
    return @fromBackingInt(@backingInt(id));
}

/// The synthesized entry `id` names.
pub fn synthesizedIndex(id: CheckedTypeId) usize {
    std.debug.assert(isSynthesized(id));
    return @backingInt(id) & ~synthesized_bit;
}

test "source and synthesized ids are disjoint and round-trip" {
    const var_: types.Var = @fromBackingInt(41);
    const source = fromVar(var_);
    try std.testing.expect(!isSynthesized(source));
    try std.testing.expectEqual(var_, asVar(source));
    const synthesized = fromSynthesized(41);
    try std.testing.expect(isSynthesized(synthesized));
    try std.testing.expectEqual(@as(usize, 41), synthesizedIndex(synthesized));
    try std.testing.expect(source != synthesized);
    const vars = [_]types.Var{ @fromBackingInt(3), @fromBackingInt(9) };
    try std.testing.expectEqualSlices(CheckedTypeId, &.{ fromVar(vars[0]), fromVar(vars[1]) }, fromVars(&vars));
}
