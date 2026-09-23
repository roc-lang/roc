//! Shared checked-type fixtures for the Boxy planning, layout, and lowering
//! tests. The three stages build the same synthetic checked payloads, so the
//! builders live here once rather than being copied into each test section.

const check = @import("check");
const std = @import("std");

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

/// Add Bool's closed checked row to a fixture containing a Bool nominal.
pub fn addBoolDeclaration(
    allocator: std.mem.Allocator,
    module: *checked.CheckedModuleArtifact,
    bool_ty: checked.CheckedTypeId,
) std.mem.Allocator.Error!void {
    const types = &module.checked_types;
    const nominal = types.payloads.items[@intFromEnum(bool_ty)].nominal;
    const empty: checked.CheckedTypeId = @enumFromInt(@as(u32, @intCast(types.payloads.items.len)));
    try types.payloads.append(allocator, .empty_tag_union);
    const tags_start: u32 = @intCast(types.tag_pool.items.len);
    for ([_][]const u8{ "False", "True" }) |name| {
        const label = try module.canonical_names.internTagLabel(name);
        try types.tag_pool.append(allocator, .{ .name = label, .args_start = 0, .args_len = 0 });
    }
    const backing: checked.CheckedTypeId = @enumFromInt(@as(u32, @intCast(types.payloads.items.len)));
    try types.payloads.append(allocator, .{ .tag_union = .{
        .tags = .{ .start = tags_start, .len = 2 },
        .ext = empty,
    } });
    const declaration_index: u32 = @intCast(types.nominal_declarations.items.len);
    try types.nominal_declarations.append(allocator, .{
        .id = @enumFromInt(declaration_index),
        .nominal = .{ .module = nominal.origin_module, .type_name = nominal.name, .source_decl = nominal.source_decl },
        .source_statement = 0,
        .declaration_root = bool_ty,
        .backing = backing,
    });
    types.builtin_nominal_declarations[@intFromEnum(checked.CheckedBuiltinNominal.bool)] = declaration_index + 1;
}
