//! Shared helpers for explicit platform requirement lookups.

const std = @import("std");
const base = @import("base");
const can = @import("can");
const check = @import("check");

const ModuleEnv = can.ModuleEnv;
const TypedCIR = check.TypedCIR;

/// Public value `IdentMap`.
pub const IdentMap = std.AutoHashMap(base.Ident.Idx, base.Ident.Idx);

/// Public function `buildPlatformToAppIdents`.
pub fn buildPlatformToAppIdents(
    allocator: std.mem.Allocator,
    platform_env: *const ModuleEnv,
    app_env: *ModuleEnv,
) !IdentMap {
    var platform_to_app_idents = IdentMap.init(allocator);
    errdefer platform_to_app_idents.deinit();

    try app_env.common.idents.interner.enableRuntimeInserts(app_env.gpa);

    for (platform_env.requires_types.items.items) |required_type| {
        const platform_ident_text = platform_env.getIdent(required_type.ident);
        if (app_env.common.findIdent(platform_ident_text)) |app_ident| {
            try platform_to_app_idents.put(required_type.ident, app_ident);
        }

        const all_aliases = platform_env.for_clause_aliases.items.items;
        const type_aliases_slice = all_aliases[@intFromEnum(required_type.type_aliases.start)..][0..required_type.type_aliases.count];
        for (type_aliases_slice) |alias| {
            const alias_name_text = platform_env.getIdent(alias.alias_name);
            const alias_app_ident = app_env.common.findIdent(alias_name_text) orelse
                try app_env.common.insertIdent(app_env.gpa, base.Ident.for_text(alias_name_text));
            try platform_to_app_idents.put(alias.alias_name, alias_app_ident);

            const rigid_name_text = platform_env.getIdent(alias.rigid_name);
            const rigid_app_ident = app_env.common.findIdent(rigid_name_text) orelse
                try app_env.common.insertIdent(app_env.gpa, base.Ident.for_text(rigid_name_text));
            try platform_to_app_idents.put(alias.rigid_name, rigid_app_ident);
        }
    }

    return platform_to_app_idents;
}

/// Public function `populateRequiredLookupTargets`.
pub fn populateRequiredLookupTargets(
    typed_modules: *TypedCIR.Modules,
    app_module_idx: ?u32,
) !void {
    const resolved_app_module_idx = app_module_idx orelse return;
    const app_module = typed_modules.module(resolved_app_module_idx);

    var source_module_idx: u32 = 0;
    while (source_module_idx < typed_modules.moduleCount()) : (source_module_idx += 1) {
        if (source_module_idx == resolved_app_module_idx) continue;

        const source_module = typed_modules.module(source_module_idx);
        const requires_items = source_module.requiresTypes();

        for (requires_items, 0..) |required_type, requires_idx| {
            const app_ident = app_module.identStoreConst().findByString(
                source_module.getIdent(required_type.ident),
            ) orelse continue;
            const target_def_idx = app_module.topLevelDefByIdent(app_ident) orelse continue;
            try typed_modules.setRequiredLookupTarget(
                source_module_idx,
                @intCast(requires_idx),
                target_def_idx,
            );
        }
    }
}
