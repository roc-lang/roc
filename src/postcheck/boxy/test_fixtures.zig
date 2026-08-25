//! Shared checked-type fixtures for the Boxy planning, layout, and lowering
//! tests. The three stages build the same synthetic checked payloads, so the
//! builders live here once rather than being copied into each test section.

const check = @import("check");

const checked = check.CheckedModule;

/// Index into a fixture-local table, named so the literal reads as a table
/// position rather than a magic number.
pub fn tableIndex(comptime index: u32) u32 {
    return index;
}

/// A stored nominal for a builtin type, rooted at the fixture tables.
pub fn builtinNominal(
    builtin: checked.CheckedBuiltinNominal,
    _: checked.CheckedTypeId,
    args: checked.CheckedTypeRange,
) checked.StoredNominal {
    return .{
        .name = @enumFromInt(tableIndex(0)),
        .origin_module = @enumFromInt(tableIndex(0)),
        .owner_module = .{},
        .builtin = builtin,
        .is_opaque = false,
        .representation = .{ .builtin = builtin },
        .args = args,
    };
}
