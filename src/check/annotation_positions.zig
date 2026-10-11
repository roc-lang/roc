//! Canonical declaration adapter for the shared polarity equations.
const std = @import("std");
const base = @import("base");
const can = @import("can");
const Allocator = std.mem.Allocator;
const ModuleEnv = can.ModuleEnv;
const CIR = can.CIR;
/// Shared exact occurrence classes.
pub const Positions = base.annotation_positions.Positions;
/// Explicit declaration owners available to the checker.
pub const OwnerResolver = struct {
    context: *const anyopaque,
    builtin_owner: ?*const ModuleEnv,
    resolve: *const fn (*const anyopaque, *const ModuleEnv, base.ModuleIdentity.Idx) *const ModuleEnv,
};

const Analysis = base.annotation_positions.Solver(Adapter);
const Adapter = struct {
    pub const Key = struct { owner: *const ModuleEnv, statement: CIR.Statement.Idx };
    pub const Annotation = CIR.TypeAnno.Idx;
    resolver: OwnerResolver,
    allocator: Allocator,
    children: std.ArrayList(Annotation) = .empty,
    fn declarationReference(key: Key) Analysis.Reference {
        // Platform for-clause aliases carry an abstract rigid backing. Their
        // application arguments are explicit source bookkeeping, with no
        // structural function position supplied by the abstract declaration.
        for (key.owner.for_clause_aliases.items.items) |alias| {
            if (alias.alias_stmt_idx == key.statement) return .builtin;
        }
        return .{ .declaration = key };
    }

    fn reference(self: *const Adapter, owner: *const ModuleEnv, base_ref: CIR.TypeAnno.LocalOrExternal) Analysis.Reference {
        return switch (base_ref) {
            .builtin => .builtin,
            .local => |local| declarationReference(.{ .owner = owner, .statement = local.decl_idx }),
            .external => |external| blk: {
                const import_name = owner.common.getString(owner.imports.imports.items.items[@backingInt(external.module_idx)]);
                if (CIR.Import.isCompilerBuiltinImportName(import_name)) {
                    break :blk declarationReference(.{
                        .owner = self.resolver.builtin_owner orelse unreachable,
                        .statement = @fromBackingInt(@intCast(external.target_node_idx)),
                    });
                }
                const identity = owner.importIdentity(external.module_idx) orelse {
                    // A resolved ordinary import must carry the content
                    // identity produced by the canonical import drain.
                    std.debug.assert(owner.imports.getResolvedModule(external.module_idx) == null);
                    break :blk .invalid;
                };
                break :blk declarationReference(.{
                    .owner = self.resolver.resolve(self.resolver.context, owner, identity),
                    .statement = @fromBackingInt(@intCast(external.target_node_idx)),
                });
            },
            .external_identity => |external| declarationReference(.{
                .owner = self.resolver.resolve(self.resolver.context, owner, external.module_identity),
                .statement = @fromBackingInt(@intCast(external.target_node_idx)),
            }),
            .pending => .invalid,
        };
    }

    pub fn declaration(_: *Adapter, key: Key) Allocator.Error!?struct { body: Annotation, formal_count: usize, nominal: bool } {
        const header, const body = switch (key.owner.store.getStatement(key.statement)) {
            .s_alias_decl => |decl| .{ decl.header, decl.anno },
            .s_nominal_decl => |decl| .{ decl.header, decl.anno },
            .s_decl,
            .s_var,
            .s_var_uninitialized,
            .s_reassign,
            .s_crash,
            .s_dbg,
            .s_expr,
            .s_expect,
            .s_for,
            .s_while,
            .s_infinite_loop,
            .s_breakable_loop,
            .s_break,
            .s_return,
            .s_import,
            .s_type_anno,
            .s_type_var_alias,
            => unreachable,
            .s_where_alias_decl, .s_runtime_error => return null,
        };
        if (body == .placeholder) return null;
        const formals = key.owner.store.sliceTypeAnnos(key.owner.store.getTypeHeader(header).args);

        return .{ .body = body, .formal_count = formals.len, .nominal = key.owner.store.getStatement(key.statement) == .s_nominal_decl };
    }

    pub fn node(self: *Adapter, key: Key, initial: Annotation) Allocator.Error!Analysis.Node {
        const owner = key.owner;
        var annotation = initial;
        while (owner.store.getTypeAnno(annotation) == .rigid_var_lookup) annotation = owner.store.getTypeAnno(annotation).rigid_var_lookup.ref;
        const header = switch (owner.store.getStatement(key.statement)) {
            .s_alias_decl => |decl| decl.header,
            .s_nominal_decl => |decl| decl.header,
            .s_decl,
            .s_var,
            .s_var_uninitialized,
            .s_reassign,
            .s_crash,
            .s_dbg,
            .s_expr,
            .s_expect,
            .s_for,
            .s_while,
            .s_infinite_loop,
            .s_breakable_loop,
            .s_break,
            .s_return,
            .s_import,
            .s_type_anno,
            .s_type_var_alias,
            .s_where_alias_decl,
            .s_runtime_error,
            => unreachable,
        };
        for (owner.store.sliceTypeAnnos(owner.store.getTypeHeader(header).args), 0..) |formal, index| {
            if (formal == annotation) return .{ .formal = index };
        }
        self.children.clearRetainingCapacity();
        switch (owner.store.getTypeAnno(annotation)) {
            .parens => |parens| try self.children.append(self.allocator, parens.anno),
            .@"fn" => |func| return .{ .function = .{ .args = owner.store.sliceTypeAnnos(func.args), .ret = func.ret } },
            .tag_union => |union_| {
                try self.children.appendSlice(self.allocator, owner.store.sliceTypeAnnos(union_.tags));
                if (union_.ext) |ext| try self.children.append(self.allocator, ext);
            },
            .tag => |tag| return .{ .children = owner.store.sliceTypeAnnos(tag.args) },
            .tuple => |tuple| return .{ .children = owner.store.sliceTypeAnnos(tuple.elems) },
            .record => |record| {
                for (owner.store.sliceAnnoRecordFields(record.fields)) |field| try self.children.append(self.allocator, owner.store.getAnnoRecordField(field).ty);
                if (record.ext) |ext| try self.children.append(self.allocator, ext);
            },
            .apply => |apply| return .{ .apply = .{ .reference = self.reference(owner, apply.base), .args = owner.store.sliceTypeAnnos(apply.args) } },
            .rigid_var, .rigid_var_lookup, .lookup, .underscore => return .leaf,
            .malformed => return .invalid,
        }
        return .{ .children = self.children.items };
    }
};

/// Analyze a canonical application; null belongs to invalid-source recovery.
pub fn analyze(allocator: Allocator, resolver: OwnerResolver, owner: *const ModuleEnv, apply: CIR.TypeAnno.Apply) Allocator.Error!?[]Positions {
    var adapter = Adapter{ .allocator = allocator, .resolver = resolver };
    const args = owner.store.sliceTypeAnnos(apply.args);
    return switch (adapter.reference(owner, apply.base)) {
        .builtin => blk: {
            const result = try allocator.alloc(Positions, args.len);
            @memset(result, .{ .inherited = true });
            break :blk result;
        },
        .invalid => null,
        .declaration => |key| blk: {
            const declaration = (try adapter.declaration(key)) orelse break :blk null;
            if (declaration.formal_count != args.len) break :blk null;
            break :blk try analyzeDeclaration(allocator, resolver, key.owner, key.statement);
        },
    };
}

/// Analyze a canonical declaration; the caller owns the result.
pub fn analyzeDeclaration(allocator: Allocator, resolver: OwnerResolver, owner: *const ModuleEnv, statement: CIR.Statement.Idx) Allocator.Error!?[]Positions {
    var analysis = Analysis.init(allocator, .{ .allocator = allocator, .resolver = resolver });
    defer analysis.adapter.children.deinit(allocator);
    defer analysis.deinit();
    return try analysis.analyze(.{ .owner = owner, .statement = statement });
}
