//! Shared helpers for method identifier lookup and definition discovery

const std = @import("std");
const Allocator = std.mem.Allocator;

const base = @import("base");
const can = @import("can");

const Ident = base.Ident;
const ModuleEnv = can.ModuleEnv;
const CIR = can.CIR;

/// Build a fully qualified method name, preferring `Type.method` when the
/// type name equals the module name, otherwise `Module.Type.method`.
pub fn buildQualifiedMethodName(
    allocator: Allocator,
    module_name: []const u8,
    type_name: []const u8,
    method_name: []const u8,
) Allocator.Error![]u8 {
    if (std.mem.eql(u8, type_name, module_name)) {
        return std.fmt.allocPrint(allocator, "{s}.{s}", .{ type_name, method_name });
    } else {
        return std.fmt.allocPrint(allocator, "{s}.{s}.{s}", .{ module_name, type_name, method_name });
    }
}

/// Find a method identifier in the origin module's ident store by checking a
/// sequence of qualified names.
/// Order: `Type.method` or `Module.Type.method` (depending on module/type match),
/// then `Module.Type.method`, then `Module.method`, then bare `method`.
pub fn findMethodIdent(
    allocator: Allocator,
    origin_env: *const ModuleEnv,
    type_name: []const u8,
    method_name: []const u8,
) Allocator.Error!?Ident.Idx {
    const ident_store = origin_env.getIdentStoreConst();

    const primary = try buildQualifiedMethodName(allocator, origin_env.module_name, type_name, method_name);
    defer allocator.free(primary);
    if (ident_store.findByString(primary)) |ident| return ident;

    const module_type = try std.fmt.allocPrint(allocator, "{s}.{s}.{s}", .{ origin_env.module_name, type_name, method_name });
    defer allocator.free(module_type);
    if (ident_store.findByString(module_type)) |ident| return ident;

    const module_method = try std.fmt.allocPrint(allocator, "{s}.{s}", .{ origin_env.module_name, method_name });
    defer allocator.free(module_method);
    if (ident_store.findByString(module_method)) |ident| return ident;

    return ident_store.findByString(method_name);
}

/// Find the def index with an assigned pattern whose ident matches `ident_idx`.
pub fn findDefIdxByIdent(origin_env: *const ModuleEnv, ident_idx: Ident.Idx) ?CIR.Def.Idx {
    const defs = origin_env.store.sliceDefs(origin_env.all_defs);
    for (defs) |def_idx| {
        const def = origin_env.store.getDef(def_idx);
        const pattern = origin_env.store.getPattern(def.pattern);

        if (pattern == .assign and pattern.assign.ident == ident_idx) {
            return def_idx;
        }
    }

    return null;
}
