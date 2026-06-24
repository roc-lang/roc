//! Resolution of a module's symbolic external references against its (now
//! compiled) imported modules.
//!
//! Canonicalization records cross-module references symbolically as
//! `CIR.ExternalRef` ({import index, referenced name}) so that CIR stays a pure
//! function of this module's source. This pass — run once the module's imports
//! are available — walks that flat worklist and resolves each reference to the
//! imported module's node index, producing a `CIR.ResolvedExternal` table that is
//! index-parallel to `ModuleEnv.external_refs`.
//!
//! The table depends on the imported modules' contents, so it is stored with the
//! type-checked artifact rather than with the source-pure CIR. Any
//! exposure/existence diagnostics (which likewise depend on imported contents)
//! are collected here rather than during canonicalization.

const std = @import("std");
const base = @import("base");
const collections = @import("collections");

const ModuleEnv = @import("ModuleEnv.zig");
const CIR = @import("CIR.zig");

const Allocator = std.mem.Allocator;
const Diagnostic = CIR.Diagnostic;
const ExposedItemTarget = collections.ExposedItemTarget;

/// Diagnostics produced by resolution, to be rendered into the module's reports
/// at type-check time (they belong with the dep-dependent type info, never with
/// the source-pure CIR's own diagnostics).
pub const Diagnostics = std.ArrayList(Diagnostic);

/// Resolve every symbolic external reference in `env` against `imported_envs`
/// (indexed by resolved module index). Returns a table index-parallel to
/// `env.external_refs`; appends a diagnostic to `diagnostics` for each reference
/// that cannot be resolved (its table entry is marked `unresolved`).
pub fn resolveExternalRefs(
    gpa: Allocator,
    env: *const ModuleEnv,
    imported_envs: []const *ModuleEnv,
    diagnostics: *Diagnostics,
) Allocator.Error!CIR.ResolvedExternal.SafeList {
    const refs = env.external_refs.items.items;
    var table = try CIR.ResolvedExternal.SafeList.initCapacity(gpa, refs.len);
    errdefer table.deinit(gpa);

    // Reused scratch for building "Module.item" qualified text.
    var scratch: std.ArrayList(u8) = .empty;
    defer scratch.deinit(gpa);

    for (refs) |ref| {
        const resolved = try resolveOne(gpa, env, imported_envs, ref, &scratch, diagnostics);
        _ = try table.append(gpa, resolved);
    }

    return table;
}

const unresolved: CIR.ResolvedExternal = .{
    .resolved_module_idx = 0,
    .target_node_idx = 0,
    .status = @intFromEnum(CIR.ResolvedExternal.Status.unresolved),
};

fn ok(module_idx: u32, node_idx: u32) CIR.ResolvedExternal {
    return .{
        .resolved_module_idx = module_idx,
        .target_node_idx = node_idx,
        .status = @intFromEnum(CIR.ResolvedExternal.Status.ok),
    };
}

fn resolveOne(
    gpa: Allocator,
    env: *const ModuleEnv,
    imported_envs: []const *ModuleEnv,
    ref: CIR.ExternalRef,
    scratch: *std.ArrayList(u8),
    diagnostics: *Diagnostics,
) Allocator.Error!CIR.ResolvedExternal {
    const module_name_ident = env.imports.getIdentIdx(ref.import_idx) orelse base.Ident.Idx.NONE;
    const name_text = env.getIdent(ref.name_ident);

    const module_idx = env.imports.getResolvedModule(ref.import_idx) orelse {
        // The import never resolved to a compiled module. If an earlier stage
        // already reported it, stay silent; otherwise surface it here.
        if (!env.imports.importFailedBeforeChecking(ref.import_idx)) {
            try diagnostics.append(gpa, .{ .type_from_missing_module = .{
                .module_name = module_name_ident,
                .type_name = ref.name_ident,
                .region = ref.region,
            } });
        }
        return unresolved;
    };

    if (module_idx >= imported_envs.len) {
        try diagnostics.append(gpa, .{ .type_from_missing_module = .{
            .module_name = module_name_ident,
            .type_name = ref.name_ident,
            .region = ref.region,
        } });
        return unresolved;
    }

    const imported = imported_envs[module_idx];

    switch (ref.kind) {
        .value => {
            const node = (try lookupExposedValueNode(gpa, imported, name_text, scratch)) orelse {
                try diagnostics.append(gpa, .{ .type_not_exposed = .{
                    .module_name = module_name_ident,
                    .type_name = ref.name_ident,
                    .region = ref.region,
                } });
                return unresolved;
            };
            return ok(module_idx, node);
        },
        .type => {
            const node = (try lookupExposedTypeNode(gpa, imported, name_text, scratch)) orelse {
                try diagnostics.append(gpa, .{ .type_not_exposed = .{
                    .module_name = module_name_ident,
                    .type_name = ref.name_ident,
                    .region = ref.region,
                } });
                return unresolved;
            };
            return ok(module_idx, node);
        },
        .nominal_type => {
            const node = (try lookupExposedTypeNode(gpa, imported, name_text, scratch)) orelse {
                try diagnostics.append(gpa, .{ .type_not_exposed = .{
                    .module_name = module_name_ident,
                    .type_name = ref.name_ident,
                    .region = ref.region,
                } });
                return unresolved;
            };
            // A nominal-type reference must resolve to a nominal declaration, not
            // a type alias.
            switch (imported.store.getStatement(@enumFromInt(node))) {
                .s_nominal_decl => return ok(module_idx, node),
                .s_alias_decl => {
                    try diagnostics.append(gpa, .{ .type_alias_but_needed_nominal = .{
                        .name = ref.name_ident,
                        .region = ref.region,
                    } });
                    return unresolved;
                },
                else => {
                    try diagnostics.append(gpa, .{ .type_not_exposed = .{
                        .module_name = module_name_ident,
                        .type_name = ref.name_ident,
                        .region = ref.region,
                    } });
                    return unresolved;
                },
            }
        },
    }
}

/// Build "Module.item" into `scratch`, returning the slice (valid until the next
/// call that reuses `scratch`).
fn qualifiedText(gpa: Allocator, scratch: *std.ArrayList(u8), parent: []const u8, child: []const u8) Allocator.Error![]const u8 {
    scratch.clearRetainingCapacity();
    try scratch.ensureUnusedCapacity(gpa, parent.len + 1 + child.len);
    scratch.appendSliceAssumeCapacity(parent);
    scratch.appendAssumeCapacity('.');
    scratch.appendSliceAssumeCapacity(child);
    return scratch.items;
}

/// Resolve the explicit target for an item exposed by `imported_env`, handling
/// both module-style exposure (bare name) and type-module associated items
/// (exposed under `<MainType>.<item>`, since a type module's main type name
/// equals its module name).
fn lookupExposedTarget(
    gpa: Allocator,
    imported_env: *const ModuleEnv,
    item_text: []const u8,
    scratch: *std.ArrayList(u8),
) Allocator.Error!?ExposedItemTarget {
    const module_qualified = try qualifiedText(gpa, scratch, imported_env.module_name, item_text);
    if (lookupTargetByText(imported_env, module_qualified)) |target| return target;
    return lookupTargetByText(imported_env, item_text);
}

fn lookupExposedValueNode(gpa: Allocator, imported_env: *const ModuleEnv, item_text: []const u8, scratch: *std.ArrayList(u8)) Allocator.Error!?u32 {
    const target = (try lookupExposedTarget(gpa, imported_env, item_text, scratch)) orelse return null;
    return target.valueDefNode();
}

fn lookupExposedTypeNode(gpa: Allocator, imported_env: *const ModuleEnv, item_text: []const u8, scratch: *std.ArrayList(u8)) Allocator.Error!?u32 {
    const target = (try lookupExposedTarget(gpa, imported_env, item_text, scratch)) orelse return null;
    return target.typeDeclNode();
}

fn lookupTargetByText(imported_env: *const ModuleEnv, text: []const u8) ?ExposedItemTarget {
    if (imported_env.common.findIdent(text)) |qualified_ident| {
        if (imported_env.getExposedTargetById(qualified_ident)) |target| return target;
    }
    return null;
}

test "qualifiedText builds Module.item" {
    const gpa = std.testing.allocator;
    var scratch: std.ArrayList(u8) = .empty;
    defer scratch.deinit(gpa);
    const out = try qualifiedText(gpa, &scratch, "Foo", "bar");
    try std.testing.expectEqualStrings("Foo.bar", out);
    // Reuse resets the buffer rather than appending.
    const out2 = try qualifiedText(gpa, &scratch, "Pkg.Mod", "baz");
    try std.testing.expectEqualStrings("Pkg.Mod.baz", out2);
}

test "resolveExternalRefs on a module with no external refs yields an empty table" {
    const gpa = std.testing.allocator;
    var env = try ModuleEnv.init(gpa, "");
    defer env.deinit();

    var diags: Diagnostics = .empty;
    defer diags.deinit(gpa);

    var table = try resolveExternalRefs(gpa, &env, &.{}, &diags);
    defer table.deinit(gpa);

    try std.testing.expectEqual(@as(usize, 0), table.len());
    try std.testing.expectEqual(@as(usize, 0), diags.items.len);
}
