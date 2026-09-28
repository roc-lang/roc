//! Exact source-formal positions from canonical declaration structure.
//!
//! Declaration equations carry three occurrence bits. Exact formal-flow
//! components collect recurring nominal position witnesses; transparent aliases
//! retain ordinary composition and function resets. Witness bits grow, then
//! ordinary composition removes overwritten bits. Every phase operates on a
//! finite CIR/formal graph, including for rejected recursive declarations.
const std = @import("std");
const base = @import("base");
const can = @import("can");
const types = @import("types");
const Allocator = std.mem.Allocator;
const ModuleEnv = can.ModuleEnv;
const CIR = can.CIR;

/// Exact occurrence classes of one source formal.
pub const Positions = packed struct(u8) {
    inherited: bool = false,
    input: bool = false,
    output: bool = false,
    padding: u5 = 0,

    fn join(self: Positions, other: Positions) Positions {
        return @bitCast(@as(u8, @bitCast(self)) | @as(u8, @bitCast(other)));
    }

    pub fn isEmpty(self: Positions) bool {
        return @as(u8, @bitCast(self)) == 0;
    }

    fn compose(self: Positions, reference: Positions) Positions {
        var result: Positions = if (self.inherited) reference else .{};
        result.input = result.input or self.input;
        result.output = result.output or self.output;
        return result;
    }

    pub fn polarity(self: Positions, reference: types.Polarity) types.Polarity {
        if (self.input) return .neg;
        if (self.inherited or self.isEmpty()) return reference;
        std.debug.assert(self.output);
        return .pos;
    }
};

/// Explicit declaration owners available to the checker.
pub const OwnerResolver = struct {
    context: *const anyopaque,
    builtin_owner: ?*const ModuleEnv,
    resolve: *const fn (*const anyopaque, *const ModuleEnv, base.ModuleIdentity.Idx) *const ModuleEnv,
};

const DeclarationKey = struct { owner: *const ModuleEnv, statement: CIR.Statement.Idx };
const Declaration = struct {
    key: DeclarationKey,
    body: CIR.TypeAnno.Idx,
    formals: []const CIR.TypeAnno.Idx,
    positions: []Positions,
    used: []bool,
    nominal: bool,
    formal_base: u32,
};
const Reference = union(enum) { builtin, declaration: DeclarationKey, invalid };
const Pending = struct { annotation: CIR.TypeAnno.Idx, positions: Positions, dependency: ?usize };
const Dependency = struct { formal: u32, parent: ?usize };
const Edge = struct { from: u32, to: u32 };

const Equations = struct {
    allocator: Allocator,
    resolver: OwnerResolver,
    declarations: std.ArrayList(Declaration) = .empty,
    by_declaration: std.AutoHashMapUnmanaged(DeclarationKey, usize) = .empty,
    pending: std.ArrayList(Pending) = .empty,
    next_inherited: std.ArrayList(bool) = .empty,
    next_positions: std.ArrayList(Positions) = .empty,
    dependencies: std.ArrayList(Dependency) = .empty,
    edges: std.ArrayList(Edge) = .empty,
    active_dependency: ?usize = null,
    formal_count: u32 = 0,
    components: []const u32 = &.{},
    active_component: u32 = 0,
    invalid: bool = false,

    fn deinit(self: *Equations) void {
        for (self.declarations.items) |decl| {
            self.allocator.free(decl.positions);
            self.allocator.free(decl.used);
        }
        self.declarations.deinit(self.allocator);
        self.by_declaration.deinit(self.allocator);
        self.pending.deinit(self.allocator);
        self.next_inherited.deinit(self.allocator);
        self.next_positions.deinit(self.allocator);
        self.dependencies.deinit(self.allocator);
        self.edges.deinit(self.allocator);
    }

    fn declarationReference(key: DeclarationKey) Reference {
        // Platform for-clause aliases carry an abstract rigid backing. Their
        // application arguments are explicit source bookkeeping, with no
        // structural function position supplied by the abstract declaration.
        for (key.owner.for_clause_aliases.items.items) |alias| {
            if (alias.alias_stmt_idx == key.statement) return .builtin;
        }
        return .{ .declaration = key };
    }

    fn reference(self: *const Equations, owner: *const ModuleEnv, base_ref: CIR.TypeAnno.LocalOrExternal) Reference {
        return switch (base_ref) {
            .builtin => .builtin,
            .local => |local| declarationReference(.{ .owner = owner, .statement = local.decl_idx }),
            .external => |external| blk: {
                const import_name = owner.common.getString(owner.imports.imports.items.items[@intFromEnum(external.module_idx)]);
                if (CIR.Import.isCompilerBuiltinImportName(import_name)) {
                    break :blk declarationReference(.{
                        .owner = self.resolver.builtin_owner orelse unreachable,
                        .statement = @enumFromInt(external.target_node_idx),
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
                    .statement = @enumFromInt(external.target_node_idx),
                });
            },
            .external_identity => |external| declarationReference(.{
                .owner = self.resolver.resolve(self.resolver.context, owner, external.module_identity),
                .statement = @enumFromInt(external.target_node_idx),
            }),
            .pending => .invalid,
        };
    }

    fn register(self: *Equations, key: DeclarationKey) Allocator.Error!?usize {
        if (self.by_declaration.get(key)) |index| return index;
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
        const positions = try self.allocator.alloc(Positions, formals.len);
        errdefer self.allocator.free(positions);
        @memset(positions, .{});
        const used = try self.allocator.alloc(bool, formals.len);
        errdefer self.allocator.free(used);
        @memset(used, false);
        const index = self.declarations.items.len;
        try self.by_declaration.put(self.allocator, key, index);
        try self.declarations.append(self.allocator, .{
            .key = key,
            .body = body,
            .formals = formals,
            .positions = positions,
            .used = used,
            .nominal = key.owner.store.getStatement(key.statement) == .s_nominal_decl,
            .formal_base = self.formal_count,
        });
        self.formal_count += @intCast(formals.len);
        return index;
    }

    fn push(self: *Equations, annotation: CIR.TypeAnno.Idx, positions: Positions) Allocator.Error!void {
        // Even a temporarily empty equation retains its argument syntax; a
        // nested function establishes positions independently of that equation.
        try self.pending.append(self.allocator, .{ .annotation = annotation, .positions = positions, .dependency = self.active_dependency });
    }

    fn pushSlice(self: *Equations, annotations: []const CIR.TypeAnno.Idx, positions: Positions) Allocator.Error!void {
        for (annotations) |annotation| try self.push(annotation, positions);
    }

    const Phase = enum { discover, inheritance, dependencies, witnesses, settle };

    fn evaluate(self: *Equations, index: usize, phase: Phase) Allocator.Error!bool {
        const decl = self.declarations.items[index];
        if (phase == .witnesses or phase == .settle) {
            var participates = false;
            for (0..decl.formals.len) |formal| {
                participates = participates or self.components[decl.formal_base + formal] == self.active_component;
            }
            if (!participates) return false;
        }
        const owner = decl.key.owner;
        var changed = false;
        if (phase == .inheritance) {
            try self.next_inherited.resize(self.allocator, decl.formals.len);
            @memset(self.next_inherited.items, false);
        }
        if (phase == .settle) {
            try self.next_positions.resize(self.allocator, decl.formals.len);
            @memset(self.next_positions.items, .{});
        }
        self.active_dependency = null;
        self.pending.clearRetainingCapacity();
        try self.push(decl.body, .{ .inherited = true });
        while (self.pending.pop()) |item| {
            self.active_dependency = item.dependency;
            var annotation = item.annotation;
            while (owner.store.getTypeAnno(annotation) == .rigid_var_lookup) {
                annotation = owner.store.getTypeAnno(annotation).rigid_var_lookup.ref;
            }
            var formal_found = false;
            for (decl.formals, 0..) |formal, formal_index| {
                if (formal != annotation) continue;
                if (phase == .discover) {
                    decl.used[formal_index] = true;
                    formal_found = true;
                    break;
                }
                if (phase == .inheritance) {
                    self.next_inherited.items[formal_index] = self.next_inherited.items[formal_index] or item.positions.inherited;
                    formal_found = true;
                    break;
                }
                const formal_id = decl.formal_base + @as(u32, @intCast(formal_index));
                if (phase == .dependencies) {
                    var dependency = item.dependency;
                    while (dependency) |dep| {
                        const edge = self.dependencies.items[dep];
                        try self.edges.append(self.allocator, .{ .from = formal_id, .to = edge.formal });
                        dependency = edge.parent;
                    }
                    formal_found = true;
                    break;
                }
                if (self.components[formal_id] != self.active_component) {
                    formal_found = true;
                    break;
                }
                if (phase == .settle) {
                    self.next_positions.items[formal_index] = self.next_positions.items[formal_index].join(item.positions);
                    formal_found = true;
                    break;
                }
                const old = decl.positions[formal_index];
                const joined = old.join(item.positions);
                changed = changed or @as(u8, @bitCast(old)) != @as(u8, @bitCast(joined));
                decl.positions[formal_index] = joined;
                formal_found = true;
                break;
            }
            if (formal_found) continue;
            switch (owner.store.getTypeAnno(annotation)) {
                .parens => |parens| try self.push(parens.anno, item.positions),
                .@"fn" => |func| {
                    self.active_dependency = null;
                    try self.pushSlice(owner.store.sliceTypeAnnos(func.args), .{ .input = true });
                    try self.push(func.ret, .{ .output = true });
                },
                .tag_union => |union_| {
                    try self.pushSlice(owner.store.sliceTypeAnnos(union_.tags), item.positions);
                    if (union_.ext) |ext| try self.push(ext, item.positions);
                },
                .tag => |tag| try self.pushSlice(owner.store.sliceTypeAnnos(tag.args), item.positions),
                .tuple => |tuple| try self.pushSlice(owner.store.sliceTypeAnnos(tuple.elems), item.positions),
                .record => |record| {
                    for (owner.store.sliceAnnoRecordFields(record.fields)) |field| {
                        try self.push(owner.store.getAnnoRecordField(field).ty, item.positions);
                    }
                    if (record.ext) |ext| try self.push(ext, item.positions);
                },
                .apply => |apply| {
                    const args = owner.store.sliceTypeAnnos(apply.args);
                    switch (self.reference(owner, apply.base)) {
                        .builtin => try self.pushSlice(args, item.positions),
                        .invalid => self.invalid = true,
                        .declaration => |key| {
                            const target_index = (try self.register(key)) orelse {
                                self.invalid = true;
                                continue;
                            };
                            const target = self.declarations.items[target_index];
                            if (target.positions.len != args.len) {
                                self.invalid = true;
                                continue;
                            }
                            for (args, target.positions, target.used, 0..) |arg, positions, used, target_formal| {
                                // Source argument storage retains a phantom
                                // actual, including functions inside it. Usedness
                                // is complete before solving positions: an empty
                                // equation mid-iteration does not prove unusedness.
                                const target_id = target.formal_base + @as(u32, @intCast(target_formal));
                                self.active_dependency = item.dependency;
                                if (phase == .dependencies and used) {
                                    const dependency = self.dependencies.items.len;
                                    try self.dependencies.append(self.allocator, .{ .formal = target_id, .parent = item.dependency });
                                    self.active_dependency = dependency;
                                }
                                var transfer = positions;
                                if (phase == .witnesses and target.nominal and self.components[target_id] == self.active_component) transfer.inherited = used;
                                const argument_positions = if (phase == .discover or !used) item.positions else transfer.compose(item.positions);
                                try self.push(arg, argument_positions);
                            }
                        },
                    }
                },
                .rigid_var, .rigid_var_lookup, .lookup, .underscore => {},
                .malformed => self.invalid = true,
            }
        }
        if (phase == .inheritance) {
            for (decl.positions, self.next_inherited.items) |*positions, inherited| {
                std.debug.assert(positions.inherited or !inherited);
                changed = changed or positions.inherited != inherited;
                positions.inherited = inherited;
            }
        }
        if (phase == .settle) {
            for (decl.positions, self.next_positions.items, 0..) |*positions, next, formal_index| {
                if (self.components[decl.formal_base + formal_index] != self.active_component) continue;
                const old_bits: u8 = @bitCast(positions.*);
                const next_bits: u8 = @bitCast(next);
                std.debug.assert((old_bits | next_bits) == old_bits);
                changed = changed or old_bits != next_bits;
                positions.* = next;
            }
        }
        return changed;
    }

    fn solve(self: *Equations) Allocator.Error!void {
        // Discover every declaration and every retained source-formal
        // occurrence without depending on the position equations' initial bottom.
        var index: usize = 0;
        while (index < self.declarations.items.len and !self.invalid) : (index += 1) {
            _ = try self.evaluate(index, .discover);
        }
        for (self.declarations.items) |decl| {
            for (decl.positions, decl.used) |*positions, used| positions.inherited = used;
        }
        // Pure recursive argument storage preserves inheritance. Function
        // boundaries remove it, so compute this bit by descending from true.
        var inheritance_changed = true;
        while (inheritance_changed and !self.invalid) {
            inheritance_changed = false;
            for (0..self.declarations.items.len) |decl_index| inheritance_changed = (try self.evaluate(decl_index, .inheritance)) or inheritance_changed;
        }
        if (self.invalid) return;
        const inherited = try self.allocator.alloc(bool, self.formal_count);
        defer self.allocator.free(inherited);
        for (self.declarations.items) |decl| {
            for (decl.positions, 0..) |positions, formal| inherited[decl.formal_base + formal] = positions.inherited;
        }
        for (0..self.declarations.items.len) |decl_index| _ = try self.evaluate(decl_index, .dependencies);
        const adjacency = try self.allocator.alloc(std.ArrayListUnmanaged(u32), self.formal_count);
        defer {
            for (adjacency) |*edges| edges.deinit(self.allocator);
            self.allocator.free(adjacency);
        }
        @memset(adjacency, .empty);
        for (self.edges.items) |edge| try adjacency[edge.from].append(self.allocator, edge.to);
        const components = try self.allocator.alloc(u32, self.formal_count);
        defer self.allocator.free(components);
        try stronglyConnectedComponents(self.allocator, adjacency, components);
        self.components = components;
        var component_count: u32 = 0;
        for (components) |component| component_count = @max(component_count, component + 1);
        for (0..component_count) |component| {
            self.active_component = @intCast(component);
            for (self.declarations.items) |decl| {
                for (decl.positions, 0..) |*positions, formal| {
                    if (components[decl.formal_base + formal] == component) positions.* = .{};
                }
            }
            // Witness only actual contexts at recurring nominal boundaries.
            // Transparent aliases still compose their own function resets.
            var changed = true;
            while (changed) {
                changed = false;
                for (0..self.declarations.items.len) |decl_index| changed = (try self.evaluate(decl_index, .witnesses)) or changed;
            }
            for (self.declarations.items) |decl| {
                for (decl.positions, 0..) |*positions, formal| {
                    if (components[decl.formal_base + formal] == component) positions.inherited = inherited[decl.formal_base + formal];
                }
            }
            // The witness closure is an upper bound. Ordinary composition now
            // removes contexts overwritten before the next recurring boundary.
            changed = true;
            while (changed) {
                changed = false;
                for (0..self.declarations.items.len) |decl_index| changed = (try self.evaluate(decl_index, .settle)) or changed;
            }
        }
    }
};

/// The caller owns the result. Null means a rejected/unresolved declaration or
/// arity, so the caller retains its existing diagnostic recovery path.
pub fn analyze(allocator: Allocator, resolver: OwnerResolver, owner: *const ModuleEnv, apply: CIR.TypeAnno.Apply) Allocator.Error!?[]Positions {
    var equations = Equations{ .allocator = allocator, .resolver = resolver };
    defer equations.deinit();
    const args = owner.store.sliceTypeAnnos(apply.args);
    const root = switch (equations.reference(owner, apply.base)) {
        .builtin => {
            const positions = try allocator.alloc(Positions, args.len);
            @memset(positions, .{ .inherited = true });
            return positions;
        },
        .invalid => return null,
        .declaration => |key| (try equations.register(key)) orelse return null,
    };
    if (equations.declarations.items[root].positions.len != args.len) return null;
    try equations.solve();
    if (equations.invalid) return null;
    return try allocator.dupe(Positions, equations.declarations.items[root].positions);
}

/// Analyze one explicit declaration for raw nominal instantiation. The caller
/// owns the result; null denotes invalid source as in `analyze`.
pub fn analyzeDeclaration(allocator: Allocator, resolver: OwnerResolver, owner: *const ModuleEnv, statement: CIR.Statement.Idx) Allocator.Error!?[]Positions {
    var equations = Equations{ .allocator = allocator, .resolver = resolver };
    defer equations.deinit();
    const root = (try equations.register(.{ .owner = owner, .statement = statement })) orelse return null;
    try equations.solve();
    if (equations.invalid) return null;
    return try allocator.dupe(Positions, equations.declarations.items[root].positions);
}

// Components are emitted after all components they depend on.
fn stronglyConnectedComponents(
    allocator: Allocator,
    adjacency: []const std.ArrayListUnmanaged(u32),
    component_of: []u32,
) std.mem.Allocator.Error!void {
    const node_count = adjacency.len;
    const unvisited = std.math.maxInt(u32);
    const index_of = try allocator.alloc(u32, node_count);
    defer allocator.free(index_of);
    @memset(index_of, unvisited);
    const low_of = try allocator.alloc(u32, node_count);
    defer allocator.free(low_of);
    const on_stack = try allocator.alloc(bool, node_count);
    defer allocator.free(on_stack);
    @memset(on_stack, false);

    var component_stack: std.ArrayListUnmanaged(u32) = .empty;
    defer component_stack.deinit(allocator);
    const WalkFrame = struct { node: u32, edge: u32 };
    var walk_frames: std.ArrayListUnmanaged(WalkFrame) = .empty;
    defer walk_frames.deinit(allocator);

    var next_index: u32 = 0;
    var next_component: u32 = 0;
    for (0..node_count) |start| {
        if (index_of[start] != unvisited) continue;
        index_of[start] = next_index;
        low_of[start] = next_index;
        next_index += 1;
        try component_stack.append(allocator, @intCast(start));
        on_stack[start] = true;
        try walk_frames.append(allocator, .{ .node = @intCast(start), .edge = 0 });
        while (walk_frames.items.len != 0) {
            const frame = &walk_frames.items[walk_frames.items.len - 1];
            const node = frame.node;
            if (frame.edge < adjacency[node].items.len) {
                const next = adjacency[node].items[frame.edge];
                frame.edge += 1;
                if (index_of[next] == unvisited) {
                    index_of[next] = next_index;
                    low_of[next] = next_index;
                    next_index += 1;
                    try component_stack.append(allocator, next);
                    on_stack[next] = true;
                    try walk_frames.append(allocator, .{ .node = next, .edge = 0 });
                } else if (on_stack[next]) {
                    low_of[node] = @min(low_of[node], index_of[next]);
                }
                continue;
            }
            walk_frames.items.len -= 1;
            if (walk_frames.items.len != 0) {
                const parent = walk_frames.items[walk_frames.items.len - 1].node;
                low_of[parent] = @min(low_of[parent], low_of[node]);
            }
            if (low_of[node] == index_of[node]) {
                while (true) {
                    const member = component_stack.pop().?;
                    on_stack[member] = false;
                    component_of[member] = next_component;
                    if (member == node) break;
                }
                next_component += 1;
            }
        }
    }
}
