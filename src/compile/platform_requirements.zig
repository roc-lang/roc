//! Shared helpers for explicit platform requirement lookups.

const std = @import("std");
const base = @import("base");
const can = @import("can");

const ModuleEnv = can.ModuleEnv;

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
