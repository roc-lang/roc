//! Lexical type dependencies of declarations, before type identity is published.
//! Captures propagate along declaration references rather than rediscovering
//! transitive backing graphs at each application or specialization.

const std = @import("std");
const can = @import("can");
const CIR = can.CIR;
const Allocator = std.mem.Allocator;

/// Sparse, module-owned capture lists; declarations without captures allocate
/// no retained list. The dependency graph exists only during construction.
pub const Table = struct {
    ready: bool = false,
    captures: std.AutoHashMapUnmanaged(CIR.Statement.Idx, []CIR.TypeAnno.Idx) = .empty,

    /// Release complete or partially constructed capture lists.
    pub fn deinit(self: *Table, allocator: Allocator) void {
        var values = self.captures.valueIterator();
        while (values.next()) |value| allocator.free(value.*);
        self.captures.deinit(allocator);
    }

    /// Lexical binder identities in deterministic declaration order.
    pub fn get(self: *const Table, declaration: CIR.Statement.Idx) []const CIR.TypeAnno.Idx {
        std.debug.assert(self.ready);
        return self.captures.get(declaration) orelse &.{};
    }

    /// Build once, before any nominal application is made or generalized.
    pub fn ensure(self: *Table, allocator: Allocator, module: *const can.ModuleEnv) Allocator.Error!void {
        if (self.ready) return;
        var analysis = Analysis{ .allocator = allocator, .module = module };
        defer analysis.deinit();
        for (0..module.type_decls.span.len) |offset| {
            _ = try analysis.declaration(module.store.statementAt(module.type_decls, offset));
        }
        // Canonicalization publishes body-local declarations in all_statements,
        // separately from the module-level constructor inventory.
        for (0..module.all_statements.span.len) |offset| {
            _ = try analysis.declaration(module.store.statementAt(module.all_statements, offset));
        }
        var index: usize = 0;
        while (index < analysis.nodes.items.len) : (index += 1) try analysis.scan(@intCast(index));
        index = 0;
        while (index < analysis.pending.items.len) : (index += 1) {
            const item = analysis.pending.items[index];
            const node = &analysis.nodes.items[item.declaration];
            const is_formal = for (node.formals) |formal| {
                if (formal == item.variable) break true;
            } else false;
            if (is_formal or node.bound.contains(item.variable)) continue;
            if ((try node.captures.getOrPut(allocator, item.variable)).found_existing) continue;
            for (node.dependents.items) |dependent| {
                try analysis.pending.append(allocator, .{ .declaration = dependent, .variable = item.variable });
            }
        }
        for (analysis.nodes.items) |node| {
            if (node.captures.count() == 0) continue;
            // Aliases participate in dependency propagation, but only nominals
            // need a retained hidden-argument list.
            if (module.store.getStatement(node.statement) != .s_nominal_decl) continue;
            const captures = try allocator.alloc(CIR.TypeAnno.Idx, node.captures.count());
            errdefer allocator.free(captures);
            var variables = node.captures.keyIterator();
            for (captures) |*capture| capture.* = variables.next().?.*;
            std.mem.sort(CIR.TypeAnno.Idx, captures, {}, struct {
                fn lessThan(_: void, a: CIR.TypeAnno.Idx, b: CIR.TypeAnno.Idx) bool {
                    return @backingInt(a) < @backingInt(b);
                }
            }.lessThan);
            try self.captures.put(allocator, node.statement, captures);
        }
        self.ready = true;
    }
};

const Node = struct {
    statement: CIR.Statement.Idx,
    formals: []const CIR.TypeAnno.Idx,
    body: CIR.TypeAnno.Idx,
    bound: std.AutoHashMapUnmanaged(CIR.TypeAnno.Idx, void) = .empty,
    captures: std.AutoHashMapUnmanaged(CIR.TypeAnno.Idx, void) = .empty,
    dependents: std.ArrayList(u32) = .empty,
};

const Analysis = struct {
    allocator: Allocator,
    module: *const can.ModuleEnv,
    indices: std.AutoHashMapUnmanaged(CIR.Statement.Idx, u32) = .empty,
    nodes: std.ArrayList(Node) = .empty,
    pending: std.ArrayList(struct { declaration: u32, variable: CIR.TypeAnno.Idx }) = .empty,
    annotations: std.ArrayList(CIR.TypeAnno.Idx) = .empty,
    seen: std.AutoHashMapUnmanaged(CIR.TypeAnno.Idx, void) = .empty,
    references: std.AutoHashMapUnmanaged(u32, void) = .empty,

    fn deinit(self: *Analysis) void {
        for (self.nodes.items) |*node| {
            node.bound.deinit(self.allocator);
            node.captures.deinit(self.allocator);
            node.dependents.deinit(self.allocator);
        }
        self.nodes.deinit(self.allocator);
        self.indices.deinit(self.allocator);
        self.pending.deinit(self.allocator);
        self.annotations.deinit(self.allocator);
        self.seen.deinit(self.allocator);
        self.references.deinit(self.allocator);
    }

    fn declaration(self: *Analysis, statement: CIR.Statement.Idx) Allocator.Error!?u32 {
        if (self.indices.get(statement)) |index| return index;
        const header, const body = switch (self.module.store.getStatement(statement)) {
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
            .s_where_alias_decl,
            .s_type_anno,
            .s_type_var_alias,
            .s_runtime_error,
            => return null, // Only alias and nominal declarations have a backing.
        };
        if (body == .placeholder) return null;
        const index: u32 = @intCast(self.nodes.items.len);
        try self.nodes.append(self.allocator, .{
            .statement = statement,
            .formals = self.module.store.sliceTypeAnnos(self.module.store.getTypeHeader(header).args),
            .body = body,
        });
        try self.indices.put(self.allocator, statement, index);
        return index;
    }

    fn reference(self: *Analysis, parent: u32, ref: CIR.TypeAnno.LocalOrExternal) Allocator.Error!void {
        switch (ref) {
            .local => |local| if (try self.declaration(local.decl_idx)) |child| {
                if (!(try self.references.getOrPut(self.allocator, child)).found_existing) {
                    try self.nodes.items[child].dependents.append(self.allocator, parent);
                }
            },
            .builtin, .external, .external_identity, .pending => {},
        }
    }

    fn scan(self: *Analysis, index: u32) Allocator.Error!void {
        const module = self.module;
        const node = self.nodes.items[index];
        self.seen.clearRetainingCapacity();
        self.references.clearRetainingCapacity();
        try self.annotations.append(self.allocator, node.body);
        while (self.annotations.pop()) |annotation| {
            if ((try self.seen.getOrPut(self.allocator, annotation)).found_existing) continue;
            switch (module.store.getTypeAnno(annotation)) {
                .rigid_var => try self.nodes.items[index].bound.put(self.allocator, annotation, {}),
                .rigid_var_lookup => |lookup| {
                    var variable = lookup.ref;
                    while (module.store.getTypeAnno(variable) == .rigid_var_lookup) {
                        variable = module.store.getTypeAnno(variable).rigid_var_lookup.ref;
                    }
                    try self.pending.append(self.allocator, .{ .declaration = index, .variable = variable });
                },
                .lookup => |lookup| try self.reference(index, lookup.base),
                .apply => |apply| {
                    try self.reference(index, apply.base);
                    try self.annotations.appendSlice(self.allocator, module.store.sliceTypeAnnos(apply.args));
                },
                .parens => |parens| try self.annotations.append(self.allocator, parens.anno),
                .@"fn" => |func| {
                    try self.annotations.appendSlice(self.allocator, module.store.sliceTypeAnnos(func.args));
                    try self.annotations.append(self.allocator, func.ret);
                },
                .tag_union => |union_| {
                    try self.annotations.appendSlice(self.allocator, module.store.sliceTypeAnnos(union_.tags));
                    if (union_.ext) |ext| try self.annotations.append(self.allocator, ext);
                },
                .tag => |tag| try self.annotations.appendSlice(self.allocator, module.store.sliceTypeAnnos(tag.args)),
                .tuple => |tuple| try self.annotations.appendSlice(self.allocator, module.store.sliceTypeAnnos(tuple.elems)),
                .record => |record| {
                    for (module.store.sliceAnnoRecordFields(record.fields)) |field| {
                        try self.annotations.append(self.allocator, module.store.getAnnoRecordField(field).ty);
                    }
                    if (record.ext) |ext| try self.annotations.append(self.allocator, ext);
                },
                .underscore, .malformed => {},
            }
        }
    }
};
