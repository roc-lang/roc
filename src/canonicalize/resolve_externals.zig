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

/// Validate every symbolic external reference in `env` against `imported_envs`
/// (indexed by resolved module index), appending a diagnostic for each reference
/// that cannot be resolved. Returns the number of unresolved references so the
/// caller can block artifact publication.
///
/// Consumers (the checker, artifact publication, hoisting analysis) recover the
/// imported node index on the fly from the same exposed-items lookup, so no
/// resolution table is stored; this pass exists solely to emit the
/// dependency-dependent diagnostics exactly once per module.
pub fn resolveExternalRefs(
    gpa: Allocator,
    env: *const ModuleEnv,
    imported_envs: []const *ModuleEnv,
    diagnostics: *Diagnostics,
) Allocator.Error!u32 {
    const refs = env.external_refs.items.items;

    // Reused scratch for building "Module.item" qualified text.
    var scratch: std.ArrayList(u8) = .empty;
    defer scratch.deinit(gpa);

    var unresolved_count: u32 = 0;
    for (refs) |ref| {
        if (!try resolveOneOk(gpa, env, imported_envs, ref, &scratch, diagnostics)) {
            unresolved_count += 1;
        }
    }

    return unresolved_count;
}

fn resolveOneOk(
    gpa: Allocator,
    env: *const ModuleEnv,
    imported_envs: []const *ModuleEnv,
    ref: CIR.ExternalRef,
    scratch: *std.ArrayList(u8),
    diagnostics: *Diagnostics,
) Allocator.Error!bool {
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
        return false;
    };

    if (module_idx >= imported_envs.len) {
        try diagnostics.append(gpa, .{ .type_from_missing_module = .{
            .module_name = module_name_ident,
            .type_name = ref.name_ident,
            .region = ref.region,
        } });
        return false;
    }

    const imported = imported_envs[module_idx];

    switch (ref.kind) {
        .value => {
            if ((try lookupExposedValueNode(gpa, imported, name_text, scratch)) != null) return true;
            try diagnostics.append(gpa, .{ .type_not_exposed = .{
                .module_name = module_name_ident,
                .type_name = ref.name_ident,
                .region = ref.region,
            } });
            return false;
        },
        .type => {
            if ((try lookupExposedTypeNode(gpa, imported, name_text, scratch)) != null) return true;
            try diagnostics.append(gpa, .{ .type_not_exposed = .{
                .module_name = module_name_ident,
                .type_name = ref.name_ident,
                .region = ref.region,
            } });
            return false;
        },
        .nominal_type => {
            const node = (try lookupExposedTypeNode(gpa, imported, name_text, scratch)) orelse {
                try diagnostics.append(gpa, .{ .type_not_exposed = .{
                    .module_name = module_name_ident,
                    .type_name = ref.name_ident,
                    .region = ref.region,
                } });
                return false;
            };
            // A nominal-type reference must resolve to a nominal declaration, not
            // a type alias.
            switch (imported.store.getStatement(@enumFromInt(node))) {
                .s_nominal_decl => return true,
                .s_alias_decl => {
                    try diagnostics.append(gpa, .{ .type_alias_but_needed_nominal = .{
                        .name = ref.name_ident,
                        .region = ref.region,
                    } });
                    return false;
                },
                else => {
                    try diagnostics.append(gpa, .{ .type_not_exposed = .{
                        .module_name = module_name_ident,
                        .type_name = ref.name_ident,
                        .region = ref.region,
                    } });
                    return false;
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

/// On-the-fly resolution of a single external reference's target node in
/// `imported_env`, by name and kind, without emitting diagnostics. Used by
/// consumers that inspect a module other than the one currently being checked
/// (e.g. cross-module hoisting analysis), where the per-checked-module
/// resolution is not available as a precomputed table.
pub fn lookupExposedNode(
    gpa: Allocator,
    imported_env: *const ModuleEnv,
    name_text: []const u8,
    kind: CIR.ExternalRef.Kind,
) Allocator.Error!?u32 {
    var scratch: std.ArrayList(u8) = .empty;
    defer scratch.deinit(gpa);
    return switch (kind) {
        .value => lookupExposedValueNode(gpa, imported_env, name_text, &scratch),
        .type, .nominal_type => lookupExposedTypeNode(gpa, imported_env, name_text, &scratch),
    };
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

test "resolveExternalRefs on a module with no external refs reports nothing" {
    const gpa = std.testing.allocator;
    var env = try ModuleEnv.init(gpa, "");
    defer env.deinit();

    var diags: Diagnostics = .empty;
    defer diags.deinit(gpa);

    const unresolved_count = try resolveExternalRefs(gpa, &env, &.{}, &diags);

    try std.testing.expectEqual(@as(u32, 0), unresolved_count);
    try std.testing.expectEqual(@as(usize, 0), diags.items.len);
}
