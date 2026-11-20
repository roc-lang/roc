//! Platform-header input to independent compilation of hosted library units.
//! A header slot exists before its target module is checked. Resolving just
//! that module's slots needs its exposed declarations, not the platform body
//! or the implementation of a sibling named by a different slot.

const std = @import("std");
const parse = @import("parse");
const can = @import("can");
const checked = @import("check").CheckedArtifact;
const discovery = @import("module_discovery.zig");

/// Source slots by logical module and their checked projections for this compilation.
pub const Plan = struct {
    arena: std.heap.ArenaAllocator,
    by_module: std.StringHashMapUnmanaged(std.ArrayList(Slot)) = .empty,
    resolved: std.AutoHashMapUnmanaged([32]u8, []const checked.UnitHostedBinding) = .empty,

    const Slot = struct { path: []const u8, symbol: []const u8, index: u32 };

    /// Copy only the selected header's binding inputs; the parse may be released immediately.
    pub fn init(gpa: std.mem.Allocator, ast: *const parse.AST) std.mem.Allocator.Error!Plan {
        var plan: Plan = .{ .arena = .init(gpa) };
        errdefer plan.deinit();
        const arena = plan.arena.allocator();
        const file = ast.store.getFile();
        const entries = ast.store.symbolMapEntrySlice(ast.store.getHeader(file.header).platform.hosted);
        if (entries.len == 0) return plan;

        const Target = struct { module: []const u8, nested: ?[]const u8 };
        var targets: std.StringHashMapUnmanaged(?Target) = .empty;
        defer targets.deinit(gpa);
        for (ast.decl_index.imports.items) |imp| {
            const ident = imp.file_binding orelse continue;
            // Only this platform package's declarations undergo the hosted
            // transform; an external import cannot define its hosted worker.
            const module = if (imp.origin == .local) try discovery.resolveLocalImportLogicalPath(arena, "", .{
                .import_name = ast.env.getIdent(imp.module_name),
                .base = imp.base,
                .parent_count = imp.parent_count,
            }) else null;
            try targets.put(gpa, ast.env.getIdent(ident), if (module) |m| .{
                .module = m,
                .nested = if (imp.nested_type_path) |nested| ast.env.getIdent(nested) else null,
            } else null);
        }
        var scratch: std.ArrayList(u8) = .empty;
        defer scratch.deinit(gpa);
        for (entries, 0..) |idx, index| {
            const entry = ast.store.getSymbolMapEntry(idx);
            // Bare targets belong to the platform root, not a library
            // import. That root has its own checked binding table.
            const owner = entry.module orelse continue;
            const ident = ast.tokens.resolveIdentifier(owner) orelse continue;
            const name = ast.env.getIdent(ident);
            // A hosted target without an explicit import auto-imports the
            // sibling module, as in extractImportsFromDeclIndex.
            const target: Target = if (targets.get(name)) |maybe| maybe orelse continue else .{ .module = name, .nested = null };
            const row = try plan.by_module.getOrPut(arena, target.module);
            if (!row.found_existing) {
                row.key_ptr.* = try arena.dupe(u8, target.module);
                row.value_ptr.* = .empty;
            }
            const path = try entry.functionPath(ast, gpa, &scratch) orelse continue;
            try row.value_ptr.append(arena, .{
                .path = if (target.nested) |n| try std.mem.join(arena, ".", &.{ n, path }) else try arena.dupe(u8, path),
                .symbol = try arena.dupe(u8, ast.resolve(entry.symbol)),
                .index = @intCast(index),
            });
        }
        return plan;
    }

    /// Release source and checked binding records together, after the build's workers.
    pub fn deinit(self: *Plan) void {
        self.arena.deinit();
    }

    /// Called on the coordinator before submitting a unit; the worker
    /// borrows the resulting immutable slice until the build completes.
    pub fn bindings(self: *Plan, module_name: []const u8, artifact: *const checked.CheckedModuleArtifact) std.mem.Allocator.Error![]const checked.UnitHostedBinding {
        const slots = self.by_module.get(module_name) orelse return &.{};
        if (self.resolved.get(artifact.key.bytes)) |known| return known;
        const gpa = self.arena.child_allocator;
        var templates: std.AutoHashMapUnmanaged(u32, @import("check").CanonicalNames.CheckedProcedureTemplateId) = .empty;
        defer templates.deinit(gpa);
        for (artifact.hosted_procs.procs.items()) |proc| try templates.put(gpa, @backingInt(proc.def_idx), proc.template.template);
        var scratch: std.ArrayList(u8) = .empty;
        defer scratch.deinit(gpa);
        const arena = self.arena.allocator();
        var result: std.ArrayList(checked.UnitHostedBinding) = .empty;
        for (slots.items) |slot| {
            const def = try can.ImportResolution.hostedTarget(gpa, &scratch, artifact.moduleEnvConst(), slot.path) orelse continue;
            const template = templates.get(def) orelse continue;
            try result.append(arena, .{ .template = template, .dispatch_index = slot.index, .symbol = slot.symbol });
        }
        try self.resolved.put(arena, artifact.key.bytes, result.items);
        return result.items;
    }
};
