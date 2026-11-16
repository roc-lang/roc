//! Lambda Specialize: give every instantiation of a generalized binding
//! group its own lifted functions (design.md "Lambda Specialize").
//!
//! Lambda solving generalizes each finished binding group over the callable
//! slots its function types own and records, for every reference to such a
//! group, the instantiation the reference was solved at
//! (`Ast.Polymorphism`). This pass consumes that polymorphic program and
//! produces an ordinary monomorphic one: every reference and every
//! lambda-set member names one lifted function, and every lifted function
//! has one solved type per expression, local and pattern.
//!
//! Groups are visited callers first, so every instantiation of a group is
//! known when the group is visited: each was made at a reference in an
//! already specialized body, or copied there with a member that names it.
//! Instantiations whose images are equal as infinite types are one
//! instance: they share one copy, and their images are linked into one
//! class. The first instance keeps the group's own functions; its scheme
//! nodes are linked to that instance's images, which instantiation built as
//! copies of them and unification then only extended. Each further instance
//! copies the group's bodies with fresh locals, join points, function ids
//! and symbols, at the types the instance substitutes.

const std = @import("std");
const collections = @import("collections");
const GuardedList = collections.GuardedList;
const diagnostics = @import("base").dev_diagnostics;

const Common = @import("../common.zig");
const Lifted = @import("../monotype_lifted/ast.zig");
const Ast = @import("ast.zig");
const Type = @import("type.zig");

const Allocator = std.mem.Allocator;

/// What one specialization did, for the `lambda_solve_stats` census.
pub const Census = struct {
    groups: usize = 0,
    generic_groups: usize = 0,
    /// References that were solved at an instantiation of a generic group.
    instantiated_references: usize = 0,
    /// Distinct instances over every generic group that has one, and the
    /// most any group has.
    instances: usize = 0,
    max_instances: usize = 0,
    /// Lifted functions this pass added, and their expressions, against
    /// the expressions of the program it was given.
    cloned_fns: usize = 0,
    cloned_exprs: usize = 0,
    exprs: usize = 0,
};

const none_class = std.math.maxInt(u32);

/// Specialize `program` in place. Returns what it did.
pub fn run(allocator: Allocator, program: *Ast.Program) Allocator.Error!Census {
    var pass = try Pass.init(allocator, program);
    defer pass.deinit();
    try pass.run();
    if (diagnostics.isSet(.lambda_solve_stats)) {
        const census = pass.census;
        std.debug.print("lambda-specialize groups={d} generic_groups={d} instantiated_references={d} instances={d} max_instances={d} cloned_fns={d} cloned_exprs={d} exprs={d}\n", .{
            census.groups,       census.generic_groups, census.instantiated_references,
            census.instances,    census.max_instances,  census.cloned_fns,
            census.cloned_exprs, census.exprs,
        });
    }
    return pass.census;
}

const Pass = struct {
    allocator: Allocator,
    program: *Ast.Program,
    census: Census = .{},
    /// Per binding group, every instantiation of it made so far.
    group_insts: []std.ArrayList(Type.InstId) = &.{},
    /// Per instantiation: the instance of its group it belongs to.
    inst_class: std.ArrayList(u32) = .empty,
    /// Per binding group: the functions of each of its instances, instance
    /// after instance, each in group order. Instance 0 is the group itself.
    instance_fns: []std.ArrayList(Lifted.FnId) = &.{},
    /// Per binding group: the local copies of each further instance
    /// (instance `k` at `k - 1`), for the captures of members naming it.
    instance_locals: []std.ArrayList(collections.DenseMapUnmanaged(Lifted.LocalId, Lifted.LocalId)) = &.{},
    /// Each original lifted function's group position, by symbol.
    fn_by_symbol: std.AutoHashMapUnmanaged(Common.Symbol, Lifted.FnId) = .empty,
    group_positions: std.ArrayList(u32) = .empty,
    symbols: Common.SymbolGen = .{},
    next_join_point: u32 = 0,

    // Equality scratch.
    assumed: std.AutoHashMapUnmanaged(u64, void) = .empty,
    assumed_insts: std.AutoHashMapUnmanaged(u64, void) = .empty,
    pending: std.ArrayList([2]Type.TypeVarId) = .empty,
    /// Pairs of equal images to link once their group's instances are made.
    merges: std.ArrayList([2]Type.TypeVarId) = .empty,

    fn init(allocator: Allocator, program: *Ast.Program) Allocator.Error!Pass {
        return .{ .allocator = allocator, .program = program };
    }

    fn deinit(self: *Pass) void {
        for (self.group_insts) |*list| list.deinit(self.allocator);
        self.allocator.free(self.group_insts);
        for (self.instance_fns) |*list| list.deinit(self.allocator);
        self.allocator.free(self.instance_fns);
        for (self.instance_locals) |*list| {
            for (list.items) |*map| map.deinit(self.allocator);
            list.deinit(self.allocator);
        }
        self.allocator.free(self.instance_locals);
        self.inst_class.deinit(self.allocator);
        self.fn_by_symbol.deinit(self.allocator);
        self.group_positions.deinit(self.allocator);
        self.assumed.deinit(self.allocator);
        self.assumed_insts.deinit(self.allocator);
        self.pending.deinit(self.allocator);
        self.merges.deinit(self.allocator);
    }

    fn poly(self: *Pass) *Ast.Polymorphism {
        return &self.program.poly;
    }

    fn types(self: *Pass) *Type.Store {
        return &self.program.types;
    }

    fn run(self: *Pass) Allocator.Error!void {
        const groups = self.program.binding_groups;
        self.census.groups = groups.count();
        self.census.exprs = self.program.lifted.exprCount();
        self.census.instantiated_references = self.poly().expr_insts.count();
        const any_generic = for (self.poly().schemes.items) |scheme| {
            if (scheme.isGeneric()) break true;
        } else false;
        if (any_generic) try self.specializeGroups();
        try self.finishMembers();
        self.rerootTables();
        self.program.poly.deinit(self.allocator);
        self.program.poly = .{};
    }

    fn specializeGroups(self: *Pass) Allocator.Error!void {
        const groups = self.program.binding_groups;
        const group_count = groups.count();
        const poly_ = self.poly();
        const fn_count = self.program.lifted.fnCount();

        self.group_insts = try self.allocator.alloc(std.ArrayList(Type.InstId), group_count);
        @memset(self.group_insts, .empty);
        self.instance_fns = try self.allocator.alloc(std.ArrayList(Lifted.FnId), group_count);
        @memset(self.instance_fns, .empty);
        self.instance_locals = try self.allocator.alloc(std.ArrayList(collections.DenseMapUnmanaged(Lifted.LocalId, Lifted.LocalId)), group_count);
        @memset(self.instance_locals, .empty);
        try self.inst_class.appendNTimes(self.allocator, none_class, poly_.instantiations.items.len);
        for (poly_.instantiations.items, 0..) |row, index| {
            if (row.images == null) continue;
            try self.group_insts[row.group].append(self.allocator, @fromBackingInt(@intCast(index)));
        }
        try self.group_positions.resize(self.allocator, fn_count);
        for (0..group_count) |group_index| {
            for (groups.group(group_index), 0..) |fn_id, position| {
                self.group_positions.items[@backingInt(fn_id)] = @intCast(position);
                try self.fn_by_symbol.put(self.allocator, self.program.lifted.getFn(fn_id).symbol, fn_id);
            }
            try self.instance_fns[group_index].appendSlice(self.allocator, groups.group(group_index));
        }

        for (0..group_count) |group_index| {
            if (!poly_.schemes.items[group_index].isGeneric()) self.inst_class.items[group_index] = 0;
        }

        self.symbols = .{ .next = self.program.lifted.next_symbol };
        self.next_join_point = self.maxJoinPoint() + 1;

        var group_index = group_count;
        while (group_index > 0) {
            group_index -= 1;
            if (!poly_.schemes.items[group_index].isGeneric()) continue;
            self.census.generic_groups += 1;
            try self.specializeGroup(@intCast(group_index));
        }
        self.program.lifted.next_symbol = self.symbols.next;
        try self.rewriteReferences();
    }

    fn maxJoinPoint(self: *Pass) u32 {
        var max: u32 = 0;
        for (self.program.lifted.exprsView()) |expr| {
            if (expr.data == .join_point) max = @max(max, @backingInt(expr.data.join_point.id));
        }
        return @max(max, @as(u32, @intCast(self.program.lifted.exprCount())));
    }

    fn specializeGroup(self: *Pass, group: u32) Allocator.Error!void {
        const poly_ = self.poly();
        var reps: std.ArrayList(Type.InstId) = .empty;
        defer reps.deinit(self.allocator);
        self.merges.clearRetainingCapacity();

        // Instances are numbered once every instantiation is placed, so
        // comparisons meet this group's instantiations as types, not as
        // numbered instances.
        var classes: std.ArrayList([2]u32) = .empty;
        defer classes.deinit(self.allocator);
        var index: usize = 0;
        while (index < self.group_insts[group].items.len) : (index += 1) {
            const inst = self.group_insts[group].items[index];
            const class = for (reps.items, 0..) |rep, class_index| {
                const merges_before = self.merges.items.len;
                if (try self.instancesEqual(rep, inst)) break class_index;
                self.merges.shrinkRetainingCapacity(merges_before);
            } else blk: {
                try reps.append(self.allocator, inst);
                break :blk reps.items.len - 1;
            };
            try classes.append(self.allocator, .{ @backingInt(inst), @intCast(class) });
        }
        for (classes.items) |entry| self.inst_class.items[entry[0]] = entry[1];
        if (reps.items.len == 0) return;
        self.census.instances += reps.items.len;
        self.census.max_instances = @max(self.census.max_instances, reps.items.len);

        if (reps.items.len > 1) {
            var region = try Region.init(self, group);
            defer region.deinit(self.allocator);
            for (reps.items[1..], 1..) |rep, class| try self.cloneInstance(&region, group, rep, @intCast(class));
        }

        for (self.merges.items) |pair| {
            const kept = self.types().rootCompressed(pair[0]);
            const merged = self.types().rootCompressed(pair[1]);
            if (kept != merged) try self.types().setLink(merged, kept);
        }

        // The first instance keeps the group's functions: each scheme node
        // becomes its image, which was copied from it and since only extended.
        const scheme = poly_.schemes.items[group];
        const images = poly_.instImages(reps.items[0]);
        for (poly_.schemeNodes(scheme), images) |generic, image| {
            const image_root = self.types().rootCompressed(image);
            if (self.types().rootCompressed(generic) != image_root) try self.types().setLink(generic, image_root);
        }
        self.inst_class.items[@backingInt(Ast.Polymorphism.identity(group))] = 0;
    }

    // Equality of instantiations as infinite types.

    fn pairKey(a: u32, b: u32) u64 {
        return (@as(u64, a) << 32) | b;
    }

    /// Whether `rep` and `other`, two instantiations of one group, give
    /// every scheme node equal images. Each pair of types compared is
    /// assumed equal while its parts are compared, so cycles close; any
    /// difference fails the whole comparison, so success means the assumed
    /// pairs are a bisimulation. On success the pairs to link are appended
    /// to `merges`.
    fn instancesEqual(self: *Pass, rep: Type.InstId, other: Type.InstId) Allocator.Error!bool {
        self.assumed.clearRetainingCapacity();
        self.assumed_insts.clearRetainingCapacity();
        self.pending.clearRetainingCapacity();
        if (!try self.pushInstPair(rep, other)) return false;
        while (self.pending.pop()) |pair| {
            const a = self.types().rootCompressed(pair[0]);
            const b = self.types().rootCompressed(pair[1]);
            if (a == b) continue;
            const entry = try self.assumed.getOrPut(self.allocator, pairKey(@backingInt(a), @backingInt(b)));
            if (entry.found_existing) continue;
            if (!try self.contentsMatch(a, b)) return false;
            try self.merges.append(self.allocator, .{ a, b });
        }
        return true;
    }

    fn pushInstPair(self: *Pass, a: Type.InstId, b: Type.InstId) Allocator.Error!bool {
        if (a == b) return true;
        const poly_ = self.poly();
        const row_a = poly_.instantiations.items[@backingInt(a)];
        const row_b = poly_.instantiations.items[@backingInt(b)];
        if (row_a.group != row_b.group) return false;
        const class_a = self.classOf(a);
        const class_b = self.classOf(b);
        if (class_a != none_class or class_b != none_class) return class_a == class_b;
        if (row_a.images == null or row_b.images == null) return false;
        const entry = try self.assumed_insts.getOrPut(self.allocator, pairKey(@backingInt(a), @backingInt(b)));
        if (entry.found_existing) return true;
        const scheme = poly_.schemes.items[row_a.group];
        const group_size = self.program.binding_groups.group(row_a.group).len;
        const roots = poly_.fn_roots.items[scheme.fn_roots_start..][0..group_size];
        const images_a = poly_.instImages(a);
        const images_b = poly_.instImages(b);
        for (roots) |position| try self.pending.append(self.allocator, .{ images_a[position], images_b[position] });
        return true;
    }

    fn classOf(self: *Pass, inst: Type.InstId) u32 {
        const index = @backingInt(inst);
        if (index >= self.inst_class.items.len) return none_class;
        return self.inst_class.items[index];
    }

    fn pushSpanPairs(self: *Pass, a: Type.Span, b: Type.Span) Allocator.Error!bool {
        if (a.count() != b.count()) return false;
        for (0..a.count()) |index| try self.pending.append(self.allocator, .{ self.types().spanItem(a, index), self.types().spanItem(b, index) });
        return true;
    }

    fn contentsMatch(self: *Pass, a: Type.TypeVarId, b: Type.TypeVarId) Allocator.Error!bool {
        const store = self.types();
        const left = store.get(a);
        const right = store.get(b);
        if (std.meta.activeTag(left) != std.meta.activeTag(right)) return false;
        switch (left) {
            .link, .unbound, .mono => return false,
            .zst, .hole => return true,
            .primitive => |primitive| return primitive == right.primitive,
            .list => |elem| try self.pending.append(self.allocator, .{ elem, right.list }),
            .box => |elem| try self.pending.append(self.allocator, .{ elem, right.box }),
            .tuple => |items| return try self.pushSpanPairs(items, right.tuple),
            .func => |func| {
                if (!try self.pushSpanPairs(func.args, right.func.args)) return false;
                try self.pending.append(self.allocator, .{ func.callable, right.func.callable });
                try self.pending.append(self.allocator, .{ func.ret, right.func.ret });
            },
            .record => |fields| {
                const other = right.record;
                if (fields.count() != other.count()) return false;
                for (0..fields.count()) |index| {
                    const lf = store.fieldItem(fields, index);
                    const rf = store.fieldItem(other, index);
                    if (lf.name != rf.name or !std.meta.eql(lf.default, rf.default)) return false;
                    try self.pending.append(self.allocator, .{ lf.ty, rf.ty });
                    const lv = lf.value_ty.get();
                    const rv = rf.value_ty.get();
                    if ((lv == null) != (rv == null)) return false;
                    if (lv) |value| try self.pending.append(self.allocator, .{ value, rv.? });
                }
            },
            .tag_union => |tags| {
                const other = right.tag_union;
                if (tags.count() != other.count()) return false;
                for (0..tags.count()) |index| {
                    const lt = store.tagItem(tags, index);
                    const rt = store.tagItem(other, index);
                    if (lt.name != rt.name or lt.checked_name != rt.checked_name) return false;
                    if (!try self.pushSpanPairs(lt.payloads, rt.payloads)) return false;
                }
            },
            .named => |named| {
                const other = right.named;
                if (!std.meta.eql(named.named_type, other.named_type) or !std.meta.eql(named.def, other.def) or
                    named.kind != other.kind or named.builtin_owner != other.builtin_owner) return false;
                if (!try self.pushSpanPairs(named.args, other.args)) return false;
                const lb = named.backing.get();
                const rb = other.backing.get();
                if ((lb == null) != (rb == null)) return false;
                if (lb) |backing| {
                    if (backing.use != rb.?.use or backing.authority != rb.?.authority) return false;
                    try self.pending.append(self.allocator, .{ backing.ty, rb.?.ty });
                }
                if (named.declared_order.count() != other.declared_order.count()) return false;
                for (0..named.declared_order.count()) |index| {
                    switch (store.declaredFieldItem(named.declared_order, index)) {
                        .named => |name| switch (store.declaredFieldItem(other.declared_order, index)) {
                            .named => |other_name| if (name != other_name) return false,
                            .padding => return false,
                        },
                        .padding => |padding| switch (store.declaredFieldItem(other.declared_order, index)) {
                            .named => return false,
                            .padding => |other_padding| try self.pending.append(self.allocator, .{ padding, other_padding }),
                        },
                    }
                }
            },
            .lambda_set => |members| return try self.membersMatch(members, right.lambda_set),
            .erased => |erased| {
                const other = right.erased;
                if (!std.mem.eql(u8, &erased.source_fn_ty.bytes, &other.source_fn_ty.bytes)) return false;
                if ((erased.abi_fn == null) != (other.abi_fn == null)) return false;
                if (erased.abi_fn) |abi_fn| try self.pending.append(self.allocator, .{ abi_fn, other.abi_fn.? });
                return try self.membersMatch(erased.members, other.members);
            },
        }
        return true;
    }

    fn membersMatch(self: *Pass, a: Type.Span, b: Type.Span) Allocator.Error!bool {
        const store = self.types();
        if (a.count() != b.count()) return false;
        for (0..a.count()) |index| {
            const lm = store.memberItem(a, index);
            const rm = store.memberItem(b, index);
            if (lm.lambda != rm.lambda) return false;
            if (!try self.pushInstPair(lm.inst, rm.inst)) return false;
            if (lm.captures.count() != rm.captures.count()) return false;
            for (0..lm.captures.count()) |capture| {
                try self.pending.append(self.allocator, .{ store.captureItem(lm.captures, capture).ty, store.captureItem(rm.captures, capture).ty });
            }
        }
        return true;
    }

    // A generic group's body region and its copies.

    /// The parts of a generic group's bodies, and which of their types
    /// depend on the group's instance.
    const Region = struct {
        group: u32,
        owner: Type.Owner,
        exprs: std.ArrayList(Lifted.ExprId) = .empty,
        stmts: std.ArrayList(Lifted.StmtId) = .empty,
        pats: std.ArrayList(Lifted.PatId) = .empty,
        /// Scheme position by scheme node.
        positions: collections.DenseMapUnmanaged(Type.TypeVarId, u32) = .empty,
        /// The body types that reach a scheme node or one of the group's
        /// own lambdas, outside the scheme: a further instance copies them.
        copied: collections.DenseMapUnmanaged(Type.TypeVarId, u32) = .empty,
        copied_nodes: std.ArrayList(Type.TypeVarId) = .empty,
        /// The instantiations in the bodies whose images such a type is.
        copied_insts: collections.DenseMapUnmanaged(Type.InstId, void) = .empty,

        fn deinit(self: *Region, allocator: Allocator) void {
            self.exprs.deinit(allocator);
            self.stmts.deinit(allocator);
            self.pats.deinit(allocator);
            self.positions.deinit(allocator);
            self.copied.deinit(allocator);
            self.copied_nodes.deinit(allocator);
            self.copied_insts.deinit(allocator);
        }

        fn init(pass: *Pass, group: u32) Allocator.Error!Region {
            var region: Region = .{ .group = group, .owner = Type.first_group_owner + group };
            errdefer region.deinit(pass.allocator);
            const poly_ = pass.poly();
            const scheme = poly_.schemes.items[group];
            for (poly_.schemeNodes(scheme), 0..) |node, position| try region.positions.put(pass.allocator, node, @intCast(position));
            try region.collectBodies(pass);
            try region.findCopied(pass);
            return region;
        }

        /// Every expression, statement and pattern of the group's bodies.
        fn collectBodies(self: *Region, pass: *Pass) Allocator.Error!void {
            const lifted = &pass.program.lifted;
            var seen_exprs = std.DynamicBitSetUnmanaged{};
            defer seen_exprs.deinit(pass.allocator);
            seen_exprs = try std.DynamicBitSetUnmanaged.initEmpty(pass.allocator, lifted.exprCount());
            var stack: std.ArrayList(Lifted.ExprChild) = .empty;
            defer stack.deinit(pass.allocator);
            var pat_stack: std.ArrayList(Lifted.PatId) = .empty;
            defer pat_stack.deinit(pass.allocator);
            for (pass.program.binding_groups.group(self.group)) |fn_id| {
                switch (lifted.getFn(fn_id).body) {
                    .roc => |body| try stack.append(pass.allocator, .{ .expr = body }),
                    .hosted => {},
                }
            }
            while (stack.pop()) |child| switch (child) {
                .stmt => |stmt_id| {
                    try self.stmts.append(pass.allocator, stmt_id);
                    switch (lifted.getStmt(stmt_id)) {
                        .uninitialized => |pat| try pat_stack.append(pass.allocator, pat),
                        .let_ => |let_| try pat_stack.append(pass.allocator, let_.pat),
                        .expr, .expect, .dbg, .return_, .crash, .checked_error => {},
                    }
                    try Lifted.appendStmtChildren(pass.allocator, lifted, stmt_id, &stack);
                },
                .expr => |expr_id| {
                    if (seen_exprs.isSet(@backingInt(expr_id))) continue;
                    seen_exprs.set(@backingInt(expr_id));
                    try self.exprs.append(pass.allocator, expr_id);
                    const data = lifted.getExpr(expr_id).data;
                    if (data == .let_) try pat_stack.append(pass.allocator, data.let_.bind);
                    if (data == .match_) {
                        const branches = lifted.branchSpan(data.match_.branches);
                        for (0..branches.len) |index| try pat_stack.append(pass.allocator, GuardedList.at(branches, index).pat);
                    }
                    try Lifted.appendChildren(pass.allocator, lifted, expr_id, &stack);
                },
            };
            while (pat_stack.pop()) |pat_id| {
                try self.pats.append(pass.allocator, pat_id);
                switch (lifted.getPat(pat_id).data) {
                    .as => |as| try pat_stack.append(pass.allocator, as.pattern),
                    .nominal => |inner| try pat_stack.append(pass.allocator, inner),
                    .tuple => |items| {
                        const pats = lifted.patSpan(items);
                        for (0..pats.len) |index| try pat_stack.append(pass.allocator, GuardedList.at(pats, index));
                    },
                    .tag => |tag| {
                        const pats = lifted.patSpan(tag.payloads);
                        for (0..pats.len) |index| try pat_stack.append(pass.allocator, GuardedList.at(pats, index));
                    },
                    .record => |fields| {
                        const destructs = lifted.recordDestructSpan(fields);
                        for (0..destructs.len) |index| try pat_stack.append(pass.allocator, GuardedList.at(destructs, index).pattern);
                    },
                    .list => |list| {
                        const pats = lifted.patSpan(list.patterns);
                        for (0..pats.len) |index| try pat_stack.append(pass.allocator, GuardedList.at(pats, index));
                        if (list.rest.get()) |rest| if (rest.pattern.get()) |rest_pat| try pat_stack.append(pass.allocator, rest_pat);
                    },
                    .str_pattern => |str| {
                        const steps = lifted.strPatternStepSpan(str.steps);
                        for (0..steps.len) |index| if (GuardedList.at(steps, index).capture.get()) |capture| try pat_stack.append(pass.allocator, capture);
                    },
                    .bind, .wildcard, .int_lit, .dec_lit, .frac_f32_lit, .frac_f64_lit, .str_lit => {},
                }
            }
        }

        /// The group's body types outside its scheme that a further
        /// instance must copy: those the group owns that reach a scheme node,
        /// a callable naming one of the group's own lambdas, or an
        /// instantiation whose images are such types. Type nodes are
        /// numbered from zero; instantiation nodes carry `inst_bit`.
        fn findCopied(self: *Region, pass: *Pass) Allocator.Error!void {
            const store = pass.types();
            const poly_ = pass.poly();
            const lifted = &pass.program.lifted;
            const identity = Ast.Polymorphism.identity(self.group);
            var graph: Graph = .{};
            defer graph.deinit(pass.allocator);

            for (pass.program.binding_groups.group(self.group)) |fn_id| {
                try graph.push(pass.allocator, pass.program.fn_tys.items[@backingInt(fn_id)], Graph.no_parent);
                const fn_ = lifted.getFn(fn_id);
                try graph.pushLocals(pass, fn_.args);
                try graph.pushLocals(pass, fn_.captures);
            }
            for (self.exprs.items) |expr_id| {
                try graph.push(pass.allocator, pass.program.expr_tys.items[@backingInt(expr_id)], Graph.no_parent);
                if (poly_.expr_insts.get(expr_id)) |inst| _ = try graph.noteInst(pass, inst, Graph.no_parent);
                for (exprLocals(lifted, expr_id).slice()) |local| try graph.push(pass.allocator, pass.program.local_tys.items[@backingInt(local)], Graph.no_parent);
                const data = lifted.getExpr(expr_id).data;
                if (data == .loop_) try graph.pushLocals(pass, data.loop_.params);
                if (data == .join_point) {
                    try graph.pushLocals(pass, data.join_point.params);
                    try graph.pushLocals(pass, data.join_point.retained);
                }
                if (data == .jump) try graph.pushLocals(pass, data.jump.loop_params);
            }
            for (self.pats.items) |pat_id| {
                try graph.push(pass.allocator, pass.program.pat_tys.items[@backingInt(pat_id)], Graph.no_parent);
                const data = lifted.getPat(pat_id).data;
                if (data == .bind) try graph.push(pass.allocator, pass.program.local_tys.items[@backingInt(data.bind)], Graph.no_parent);
                if (data == .as) try graph.push(pass.allocator, pass.program.local_tys.items[@backingInt(data.as.local)], Graph.no_parent);
            }

            var children: std.ArrayList(Type.TypeVarId) = .empty;
            defer children.deinit(pass.allocator);
            var member_insts: std.ArrayList(Type.InstId) = .empty;
            defer member_insts.deinit(pass.allocator);
            while (graph.work.pop()) |item| {
                const root = store.rootCompressed(item.ty);
                // A class no link has given an owner was first read by the
                // body that reaches it.
                const owner = store.owners.items[@backingInt(root)];
                if (owner != self.owner and owner != Type.unowned) continue;
                const entry = try graph.index_of.getOrPut(pass.allocator, root);
                if (entry.found_existing) {
                    try graph.edge(pass.allocator, item.parent, entry.value_ptr.*);
                    continue;
                }
                const index: u32 = @intCast(graph.nodes.items.len);
                entry.value_ptr.* = index;
                try graph.nodes.append(pass.allocator, root);
                try graph.edge(pass.allocator, item.parent, index);
                if (self.positions.contains(root)) {
                    // A scheme node: substituted, never copied, and a seed.
                    try graph.reaches.append(pass.allocator, true);
                    continue;
                }
                children.clearRetainingCapacity();
                member_insts.clearRetainingCapacity();
                const own = try appendChildren(store, root, &children, &member_insts, identity, pass.allocator);
                try graph.reaches.append(pass.allocator, own);
                for (children.items) |child| try graph.push(pass.allocator, child, index);
                for (member_insts.items) |inst| {
                    if (poly_.isIdentity(inst)) continue;
                    _ = try graph.noteInst(pass, inst, index);
                }
            }

            var changed = true;
            while (changed) {
                changed = false;
                for (graph.edges.items) |edge_| {
                    if (!graph.get(edge_[1]) or graph.get(edge_[0])) continue;
                    graph.set(edge_[0]);
                    changed = true;
                }
            }
            for (graph.nodes.items, graph.reaches.items) |node, reaches| {
                if (!reaches or self.positions.contains(node)) continue;
                try self.copied.put(pass.allocator, node, @intCast(self.copied_nodes.items.len));
                try self.copied_nodes.append(pass.allocator, node);
            }
            for (graph.insts.items, graph.inst_reaches.items) |inst, reaches| {
                if (reaches) try self.copied_insts.put(pass.allocator, inst, {});
            }
        }

        /// The reference graph `findCopied` walks.
        const Graph = struct {
            const no_parent = std.math.maxInt(u32);
            const inst_bit: u32 = 1 << 31;
            const Work = struct { ty: Type.TypeVarId, parent: u32 };

            work: std.ArrayList(Work) = .empty,
            nodes: std.ArrayList(Type.TypeVarId) = .empty,
            reaches: std.ArrayList(bool) = .empty,
            index_of: collections.DenseMapUnmanaged(Type.TypeVarId, u32) = .empty,
            insts: std.ArrayList(Type.InstId) = .empty,
            inst_reaches: std.ArrayList(bool) = .empty,
            inst_index: collections.DenseMapUnmanaged(Type.InstId, u32) = .empty,
            /// Parent to child.
            edges: std.ArrayList([2]u32) = .empty,

            fn deinit(self: *Graph, allocator: Allocator) void {
                self.work.deinit(allocator);
                self.nodes.deinit(allocator);
                self.reaches.deinit(allocator);
                self.index_of.deinit(allocator);
                self.insts.deinit(allocator);
                self.inst_reaches.deinit(allocator);
                self.inst_index.deinit(allocator);
                self.edges.deinit(allocator);
            }

            fn push(self: *Graph, allocator: Allocator, ty: Type.TypeVarId, parent: u32) Allocator.Error!void {
                try self.work.append(allocator, .{ .ty = ty, .parent = parent });
            }

            fn pushLocals(self: *Graph, pass: *Pass, span: Lifted.Span(Lifted.TypedLocal)) Allocator.Error!void {
                const locals = pass.program.lifted.typedLocalSpan(span);
                for (0..locals.len) |index| try self.push(pass.allocator, pass.program.local_tys.items[@backingInt(GuardedList.at(locals, index).local)], no_parent);
            }

            fn edge(self: *Graph, allocator: Allocator, parent: u32, child: u32) Allocator.Error!void {
                if (parent == no_parent) return;
                try self.edges.append(allocator, .{ parent, child });
            }

            fn noteInst(self: *Graph, pass: *Pass, inst: Type.InstId, parent: u32) Allocator.Error!u32 {
                const entry = try self.inst_index.getOrPut(pass.allocator, inst);
                if (!entry.found_existing) {
                    entry.value_ptr.* = @as(u32, @intCast(self.insts.items.len)) | inst_bit;
                    try self.insts.append(pass.allocator, inst);
                    try self.inst_reaches.append(pass.allocator, false);
                    for (pass.poly().instImages(inst)) |image| try self.push(pass.allocator, image, entry.value_ptr.*);
                }
                try self.edge(pass.allocator, parent, entry.value_ptr.*);
                return entry.value_ptr.*;
            }

            fn get(self: *const Graph, index: u32) bool {
                if (index & inst_bit != 0) return self.inst_reaches.items[index & ~inst_bit];
                return self.reaches.items[index];
            }

            fn set(self: *Graph, index: u32) void {
                if (index & inst_bit != 0) {
                    self.inst_reaches.items[index & ~inst_bit] = true;
                } else {
                    self.reaches.items[index] = true;
                }
            }
        };
    };

    /// The locals an expression names in fields of its own.
    const ExprLocals = struct {
        items: [3]Lifted.LocalId = undefined,
        len: usize = 0,

        fn add(self: *ExprLocals, local: Lifted.LocalId) void {
            self.items[self.len] = local;
            self.len += 1;
        }

        fn slice(self: *const ExprLocals) []const Lifted.LocalId {
            return self.items[0..self.len];
        }
    };

    fn exprLocals(lifted: *const Lifted.Program, expr_id: Lifted.ExprId) ExprLocals {
        var out: ExprLocals = .{};
        const data = lifted.getExpr(expr_id).data;
        if (data == .local) out.add(data.local);
        if (data == .uninitialized_payload) out.add(data.uninitialized_payload.condition);
        if (data == .if_initialized_payload) out.add(data.if_initialized_payload.payload);
        if (data == .try_sequence) out.add(data.try_sequence.ok_local);
        if (data == .try_record_sequence) {
            out.add(data.try_record_sequence.value_local);
            out.add(data.try_record_sequence.rest_local);
        }
        return out;
    }

    /// `root`'s child types into `children` and the instantiations its
    /// members name into `insts`; true when it names `identity`.
    fn appendChildren(
        store: *Type.Store,
        root: Type.TypeVarId,
        children: *std.ArrayList(Type.TypeVarId),
        insts: *std.ArrayList(Type.InstId),
        identity: Type.InstId,
        allocator: Allocator,
    ) Allocator.Error!bool {
        var own = false;
        switch (store.get(root)) {
            .link => Common.invariant("Lambda Solved root returned a link"),
            .unbound, .primitive, .zst, .hole, .mono => {},
            .list, .box => |elem| try children.append(allocator, elem),
            .tuple => |items| for (0..items.count()) |index| try children.append(allocator, store.spanItem(items, index)),
            .record => |fields| for (0..fields.count()) |index| {
                const field = store.fieldItem(fields, index);
                try children.append(allocator, field.ty);
                if (field.value_ty.get()) |value_ty| try children.append(allocator, value_ty);
            },
            .tag_union => |tags| for (0..tags.count()) |tag_index| {
                const tag = store.tagItem(tags, tag_index);
                for (0..tag.payloads.count()) |index| try children.append(allocator, store.spanItem(tag.payloads, index));
            },
            .func => |func| {
                for (0..func.args.count()) |index| try children.append(allocator, store.spanItem(func.args, index));
                try children.append(allocator, func.callable);
                try children.append(allocator, func.ret);
            },
            .named => |named| {
                for (0..named.args.count()) |index| try children.append(allocator, store.spanItem(named.args, index));
                if (named.backing.get()) |backing| try children.append(allocator, backing.ty);
                for (0..named.declared_order.count()) |index| switch (store.declaredFieldItem(named.declared_order, index)) {
                    .named => {},
                    .padding => |padding| try children.append(allocator, padding),
                };
            },
            .lambda_set => |members| own = try appendMembers(store, members, children, insts, identity, allocator),
            .erased => |erased| {
                own = try appendMembers(store, erased.members, children, insts, identity, allocator);
                if (erased.abi_fn) |abi_fn| try children.append(allocator, abi_fn);
            },
        }
        return own;
    }

    fn appendMembers(
        store: *Type.Store,
        members: Type.Span,
        children: *std.ArrayList(Type.TypeVarId),
        insts: *std.ArrayList(Type.InstId),
        identity: Type.InstId,
        allocator: Allocator,
    ) Allocator.Error!bool {
        var own = false;
        for (0..members.count()) |member_index| {
            const member = store.memberItem(members, member_index);
            if (member.inst == identity) own = true;
            try insts.append(allocator, member.inst);
            for (0..member.captures.count()) |index| try children.append(allocator, store.captureItem(member.captures, index).ty);
        }
        return own;
    }

    /// One further instance of a generic group: its substitution and the
    /// copies it has made.
    const Instance = struct {
        group: u32,
        class: u32,
        rep: Type.InstId,
        region: *const Region,
        copies: []Type.TypeVarId,
        inst_copies: collections.DenseMapUnmanaged(Type.InstId, Type.InstId) = .empty,
        locals: *collections.DenseMapUnmanaged(Lifted.LocalId, Lifted.LocalId),
        fns: []const Lifted.FnId,
        clones: []const Lifted.FnId,
    };

    fn subst(self: *Pass, instance: *const Instance, ty: Type.TypeVarId) Type.TypeVarId {
        const root = self.types().rootCompressed(ty);
        // Copying instantiations appends images, so the representative's
        // are read afresh.
        if (instance.region.positions.get(root)) |position| return self.poly().instImages(instance.rep)[position];
        if (instance.region.copied.get(root)) |index| return instance.copies[index];
        return root;
    }

    fn substSpan(self: *Pass, instance: *const Instance, span: Type.Span) Allocator.Error!Type.Span {
        const slots = try self.types().addSpanSlots(span.count());
        for (slots.slots, 0..) |*slot, index| slot.* = self.subst(instance, self.types().spanItem(span, index));
        return slots.span;
    }

    /// The instantiation a copied body or member names in `instance`.
    fn substInst(self: *Pass, instance: *Instance, inst: Type.InstId) Allocator.Error!Type.InstId {
        if (inst == Ast.Polymorphism.identity(instance.group)) return instance.rep;
        if (!instance.region.copied_insts.contains(inst)) return inst;
        if (instance.inst_copies.get(inst)) |copied| return copied;
        const poly_ = self.poly();
        const row = poly_.instantiations.items[@backingInt(inst)];
        const len = poly_.schemes.items[row.group].nodes_len;
        const start: u32 = @intCast(poly_.images.items.len);
        try poly_.images.ensureUnusedCapacity(self.allocator, len);
        for (0..len) |index| poly_.images.appendAssumeCapacity(self.subst(instance, poly_.images.items[row.images.? + index]));
        const context = if (row.context) |fn_id| cloneOf(instance, fn_id) else null;
        const copied: Type.InstId = @fromBackingInt(@intCast(poly_.instantiations.items.len));
        try poly_.instantiations.append(self.allocator, .{ .group = row.group, .context = context, .images = start });
        try self.inst_class.append(self.allocator, none_class);
        try self.group_insts[row.group].append(self.allocator, copied);
        try instance.inst_copies.put(self.allocator, inst, copied);
        return copied;
    }

    fn cloneOf(instance: *const Instance, fn_id: Lifted.FnId) Lifted.FnId {
        for (instance.fns, instance.clones) |original, clone| if (original == fn_id) return clone;
        return fn_id;
    }

    fn substContent(self: *Pass, instance: *Instance, content: Type.Content) Allocator.Error!Type.Content {
        const store = self.types();
        return switch (content) {
            .link, .mono => Common.invariant("a copied body type was not a materialized class root"),
            .unbound, .primitive, .zst, .hole => content,
            .list => |elem| .{ .list = self.subst(instance, elem) },
            .box => |elem| .{ .box = self.subst(instance, elem) },
            .tuple => |items| .{ .tuple = try self.substSpan(instance, items) },
            .record => |fields| blk: {
                const copied = try self.allocator.alloc(Type.Field, fields.count());
                defer self.allocator.free(copied);
                for (copied, 0..) |*field, index| {
                    field.* = store.fieldItem(fields, index);
                    field.ty = self.subst(instance, field.ty);
                    if (field.value_ty.get()) |value_ty| field.value_ty = .of(self.subst(instance, value_ty));
                }
                break :blk .{ .record = try store.addFields(copied) };
            },
            .tag_union => |tags| blk: {
                const copied = try self.allocator.alloc(Type.Tag, tags.count());
                defer self.allocator.free(copied);
                for (copied, 0..) |*tag, index| {
                    tag.* = store.tagItem(tags, index);
                    tag.payloads = try self.substSpan(instance, tag.payloads);
                }
                break :blk .{ .tag_union = try store.addTags(copied) };
            },
            .func => |func| .{ .func = .{
                .args = try self.substSpan(instance, func.args),
                .callable = self.subst(instance, func.callable),
                .ret = self.subst(instance, func.ret),
            } },
            .named => |named| blk: {
                var copied = named;
                copied.args = try self.substSpan(instance, named.args);
                if (named.backing.get()) |backing| {
                    var mapped = backing;
                    mapped.ty = self.subst(instance, backing.ty);
                    copied.backing = .some(mapped);
                }
                if (named.declared_order.count() != 0) {
                    const declared = try self.allocator.alloc(Type.DeclaredField, named.declared_order.count());
                    defer self.allocator.free(declared);
                    for (declared, 0..) |*field, index| field.* = switch (store.declaredFieldItem(named.declared_order, index)) {
                        .named => |name| .{ .named = name },
                        .padding => |padding| .{ .padding = self.subst(instance, padding) },
                    };
                    copied.declared_order = try store.addDeclaredFields(declared);
                }
                break :blk .{ .named = copied };
            },
            .lambda_set => |members| .{ .lambda_set = try self.substMembers(instance, members) },
            .erased => |erased| .{ .erased = .{
                .source_fn_ty = erased.source_fn_ty,
                .members = try self.substMembers(instance, erased.members),
                .abi_fn = if (erased.abi_fn) |abi_fn| self.subst(instance, abi_fn) else null,
            } },
        };
    }

    fn substMembers(self: *Pass, instance: *Instance, members: Type.Span) Allocator.Error!Type.Span {
        if (members.count() == 0) return members;
        const store = self.types();
        const copied = try self.allocator.alloc(Type.FnMember, members.count());
        defer self.allocator.free(copied);
        for (copied, 0..) |*member, index| {
            member.* = store.memberItem(members, index);
            member.inst = try self.substInst(instance, member.inst);
            const slots = try store.addCaptureSlots(member.captures.count());
            for (slots.slots, 0..) |*slot, capture| {
                slot.* = store.captureItem(member.captures, capture);
                slot.ty = self.subst(instance, slot.ty);
            }
            member.captures = slots.span;
        }
        return try store.addMembers(copied);
    }

    /// Give `rep`'s instance of `group` its own copy of the group's
    /// functions.
    fn cloneInstance(self: *Pass, region: *Region, group: u32, rep: Type.InstId, class: u32) Allocator.Error!void {
        const store = self.types();
        const fns = self.program.binding_groups.group(group);
        const clones = try self.allocator.alloc(Lifted.FnId, fns.len);
        defer self.allocator.free(clones);
        for (clones) |*clone| clone.* = try self.program.lifted.reserveFnSlot();
        try self.instance_fns[group].appendSlice(self.allocator, clones);

        try self.instance_locals[group].append(self.allocator, .empty);
        const locals = &self.instance_locals[group].items[self.instance_locals[group].items.len - 1];

        const copies = try self.allocator.alloc(Type.TypeVarId, region.copied_nodes.items.len);
        defer self.allocator.free(copies);
        const previous_owner = store.current_owner;
        store.current_owner = Type.global_owner;
        defer store.current_owner = previous_owner;
        for (copies) |*copy| copy.* = try store.add(.unbound);

        var instance: Instance = .{
            .group = group,
            .class = class,
            .rep = rep,
            .region = region,
            .copies = copies,
            .locals = locals,
            .fns = fns,
            .clones = clones,
        };
        defer instance.inst_copies.deinit(self.allocator);

        var cloner: BodyCloner = .{ .pass = self, .instance = &instance };
        defer cloner.deinit();
        try cloner.cloneBodies();

        for (region.copied_nodes.items, copies) |node, copy| {
            const content = try self.substContent(&instance, store.get(node));
            try store.set(copy, content);
            if (content == .named) if (content.named.backing.get()) |backing| store.markNamedBacking(backing.ty);
        }

        for (fns, clones) |original, clone| {
            var fn_ = self.program.lifted.getFn(original);
            fn_.symbol = self.symbols.fresh();
            fn_.args = try cloner.cloneTypedLocals(fn_.args);
            fn_.captures = try cloner.cloneTypedLocals(fn_.captures);
            fn_.body = switch (fn_.body) {
                .roc => |body| .{ .roc = cloner.expr_map.get(body).? },
                .hosted => .hosted,
            };
            self.program.lifted.setFn(clone, fn_);
            if (self.program.lifted.procDebugName(self.program.lifted.getFn(original).symbol)) |name| try self.program.lifted.setProcDebugName(fn_.symbol, name);
            const fn_ty = self.subst(&instance, self.program.fn_tys.items[@backingInt(original)]);
            try self.program.fn_tys.append(self.allocator, fn_ty);
            try self.program.defs.append(self.allocator, .{ .symbol = fn_.symbol, .ty = fn_ty, .body = switch (fn_.body) {
                .roc => |body| .{ .roc = body },
                .hosted => .hosted,
            } });
            try self.group_positions.append(self.allocator, self.group_positions.items[@backingInt(original)]);
            try self.poly().fn_groups.append(self.allocator, group);
        }
        // Locals reserved or added while cloning get their own types.
        self.census.cloned_fns += fns.len;
    }

    /// Copies one instance's bodies.
    const BodyCloner = struct {
        pass: *Pass,
        instance: *Instance,
        expr_map: collections.DenseMapUnmanaged(Lifted.ExprId, Lifted.ExprId) = .empty,
        pat_map: collections.DenseMapUnmanaged(Lifted.PatId, Lifted.PatId) = .empty,
        stmt_map: collections.DenseMapUnmanaged(Lifted.StmtId, Lifted.StmtId) = .empty,
        join_map: collections.DenseMapUnmanaged(Lifted.JoinPointId, Lifted.JoinPointId) = .empty,
        exprs: std.ArrayList(Lifted.ExprId) = .empty,
        pats: std.ArrayList(Lifted.PatId) = .empty,
        stmts: std.ArrayList(Lifted.StmtId) = .empty,
        typed_locals: std.ArrayList(Lifted.TypedLocal) = .empty,
        operands: std.ArrayList(Lifted.CaptureOperand) = .empty,
        fields: std.ArrayList(Lifted.FieldExpr) = .empty,
        branches: std.ArrayList(Lifted.Branch) = .empty,
        if_branches: std.ArrayList(Lifted.IfBranch) = .empty,
        destructs: std.ArrayList(Lifted.RecordDestruct) = .empty,
        steps: std.ArrayList(@import("../monotype/ast.zig").StrPatternStep) = .empty,

        fn deinit(self: *BodyCloner) void {
            const allocator = self.pass.allocator;
            self.expr_map.deinit(allocator);
            self.pat_map.deinit(allocator);
            self.stmt_map.deinit(allocator);
            self.join_map.deinit(allocator);
            self.exprs.deinit(allocator);
            self.pats.deinit(allocator);
            self.stmts.deinit(allocator);
            self.typed_locals.deinit(allocator);
            self.operands.deinit(allocator);
            self.fields.deinit(allocator);
            self.branches.deinit(allocator);
            self.if_branches.deinit(allocator);
            self.destructs.deinit(allocator);
            self.steps.deinit(allocator);
        }

        fn lifted(self: *BodyCloner) *Lifted.Program {
            return &self.pass.program.lifted;
        }

        fn local(self: *BodyCloner, source: Lifted.LocalId) Allocator.Error!Lifted.LocalId {
            const entry = try self.instance.locals.getOrPut(self.pass.allocator, source);
            if (entry.found_existing) return entry.value_ptr.*;
            const copy = try self.lifted().addLocalCopy(source, self.pass.symbols.fresh());
            entry.value_ptr.* = copy;
            try self.pass.program.local_tys.append(self.pass.allocator, self.pass.subst(self.instance, self.pass.program.local_tys.items[@backingInt(source)]));
            return copy;
        }

        fn join(self: *BodyCloner, source: Lifted.JoinPointId) Allocator.Error!Lifted.JoinPointId {
            const entry = try self.join_map.getOrPut(self.pass.allocator, source);
            if (entry.found_existing) return entry.value_ptr.*;
            entry.value_ptr.* = @fromBackingInt(self.pass.next_join_point);
            self.pass.next_join_point += 1;
            return entry.value_ptr.*;
        }

        fn expr(self: *BodyCloner, source: Lifted.ExprId) Lifted.ExprId {
            return self.expr_map.get(source).?;
        }

        fn optExpr(self: *BodyCloner, source: ?Lifted.ExprId) ?Lifted.ExprId {
            return if (source) |id| self.expr(id) else null;
        }

        fn pat(self: *BodyCloner, source: Lifted.PatId) Lifted.PatId {
            return self.pat_map.get(source).?;
        }

        fn fnRef(self: *BodyCloner, fn_id: Lifted.FnId) Lifted.FnId {
            return cloneOf(self.instance, fn_id);
        }

        fn exprSpan(self: *BodyCloner, span: Lifted.Span(Lifted.ExprId)) Allocator.Error!Lifted.Span(Lifted.ExprId) {
            const borrow = self.lifted().exprSpan(span);
            self.exprs.clearRetainingCapacity();
            for (0..borrow.len) |index| try self.exprs.append(self.pass.allocator, self.expr(GuardedList.at(borrow, index)));
            return try self.lifted().addExprSpan(self.exprs.items);
        }

        fn patSpan(self: *BodyCloner, span: Lifted.Span(Lifted.PatId)) Allocator.Error!Lifted.Span(Lifted.PatId) {
            const borrow = self.lifted().patSpan(span);
            self.pats.clearRetainingCapacity();
            for (0..borrow.len) |index| try self.pats.append(self.pass.allocator, self.pat(GuardedList.at(borrow, index)));
            return try self.lifted().addPatSpan(self.pats.items);
        }

        fn stmtSpan(self: *BodyCloner, span: Lifted.Span(Lifted.StmtId)) Allocator.Error!Lifted.Span(Lifted.StmtId) {
            const borrow = self.lifted().stmtSpan(span);
            self.stmts.clearRetainingCapacity();
            for (0..borrow.len) |index| try self.stmts.append(self.pass.allocator, self.stmt_map.get(GuardedList.at(borrow, index)).?);
            return try self.lifted().addStmtSpan(self.stmts.items);
        }

        fn cloneTypedLocals(self: *BodyCloner, span: Lifted.Span(Lifted.TypedLocal)) Allocator.Error!Lifted.Span(Lifted.TypedLocal) {
            const borrow = self.lifted().typedLocalSpan(span);
            self.typed_locals.clearRetainingCapacity();
            for (0..borrow.len) |index| {
                const typed = GuardedList.at(borrow, index);
                try self.typed_locals.append(self.pass.allocator, .{ .local = try self.local(typed.local), .ty = typed.ty });
            }
            return try self.lifted().addTypedLocalSpan(self.typed_locals.items);
        }

        fn captureOperands(self: *BodyCloner, span: Lifted.Span(Lifted.CaptureOperand)) Allocator.Error!Lifted.Span(Lifted.CaptureOperand) {
            if (span.len == 0) return span;
            const borrow = self.lifted().captureOperandSpan(span);
            self.operands.clearRetainingCapacity();
            for (0..borrow.len) |index| {
                const operand = GuardedList.at(borrow, index);
                try self.operands.append(self.pass.allocator, .{ .id = operand.id, .value = self.expr(operand.value) });
            }
            return try self.lifted().addCaptureOperandSpan(self.operands.items);
        }

        fn fieldExprs(self: *BodyCloner, span: Lifted.Span(Lifted.FieldExpr)) Allocator.Error!Lifted.Span(Lifted.FieldExpr) {
            const borrow = self.lifted().fieldExprSpan(span);
            self.fields.clearRetainingCapacity();
            for (0..borrow.len) |index| {
                const field = GuardedList.at(borrow, index);
                try self.fields.append(self.pass.allocator, .{ .name = field.name, .value = self.expr(field.value) });
            }
            return try self.lifted().addFieldExprSpan(self.fields.items);
        }

        fn cloneBodies(self: *BodyCloner) Allocator.Error!void {
            const pass = self.pass;
            const region = self.instance.region;
            const program = pass.program;
            const saved_loc = program.lifted.current_loc;
            const saved_region = program.lifted.current_region;
            const saved_scope = program.lifted.current_inline_scope;
            defer {
                program.lifted.current_loc = saved_loc;
                program.lifted.current_region = saved_region;
                program.lifted.current_inline_scope = saved_scope;
            }

            // Every expression first, as a placeholder, so every reference
            // to one has its copy's id.
            for (region.exprs.items) |source| {
                const original = program.lifted.getExpr(source);
                program.lifted.current_loc = program.lifted.exprLoc(source);
                program.lifted.current_region = program.lifted.exprRegion(source);
                program.lifted.current_inline_scope = program.lifted.exprInlineScope(source);
                const copy = try program.lifted.addExpr(.{ .ty = original.ty, .data = .unit });
                try self.expr_map.put(pass.allocator, source, copy);
                try program.expr_tys.append(pass.allocator, pass.subst(self.instance, program.expr_tys.items[@backingInt(source)]));
                pass.census.cloned_exprs += 1;
            }

            // Patterns after the patterns they contain.
            var pat_index = region.pats.items.len;
            while (pat_index > 0) {
                pat_index -= 1;
                const source = region.pats.items[pat_index];
                const original = program.lifted.getPat(source);
                const data: Lifted.PatData = switch (original.data) {
                    .bind => |local_id| .{ .bind = try self.local(local_id) },
                    .as => |as| .{ .as = .{ .pattern = self.pat(as.pattern), .local = try self.local(as.local) } },
                    .nominal => |inner| .{ .nominal = self.pat(inner) },
                    .tuple => |items| .{ .tuple = try self.patSpan(items) },
                    .tag => |tag| .{ .tag = .{ .name = tag.name, .payloads = try self.patSpan(tag.payloads) } },
                    .record => |fields| blk: {
                        const borrow = program.lifted.recordDestructSpan(fields);
                        self.destructs.clearRetainingCapacity();
                        for (0..borrow.len) |index| {
                            const destruct = GuardedList.at(borrow, index);
                            try self.destructs.append(pass.allocator, .{ .name = destruct.name, .pattern = self.pat(destruct.pattern) });
                        }
                        break :blk .{ .record = try program.lifted.addRecordDestructSpan(self.destructs.items) };
                    },
                    .list => |list| blk: {
                        var rest = list.rest;
                        if (list.rest.get()) |rest_pattern| {
                            var mapped = rest_pattern;
                            if (rest_pattern.pattern.get()) |inner| mapped.pattern = .of(self.pat(inner));
                            rest = .some(mapped);
                        }
                        break :blk .{ .list = .{ .patterns = try self.patSpan(list.patterns), .rest = rest } };
                    },
                    .str_pattern => |str| blk: {
                        const borrow = program.lifted.strPatternStepSpan(str.steps);
                        self.steps.clearRetainingCapacity();
                        for (0..borrow.len) |index| {
                            var step = GuardedList.at(borrow, index);
                            if (step.capture.get()) |capture| step.capture = .of(self.pat(capture));
                            try self.steps.append(pass.allocator, step);
                        }
                        break :blk .{ .str_pattern = .{ .prefix = str.prefix, .steps = try program.lifted.addStrPatternStepSpan(self.steps.items), .end = str.end } };
                    },
                    .wildcard, .int_lit, .dec_lit, .frac_f32_lit, .frac_f64_lit, .str_lit => original.data,
                };
                const copy = try program.lifted.addPat(.{ .ty = original.ty, .data = data });
                try self.pat_map.put(pass.allocator, source, copy);
                try program.pat_tys.append(pass.allocator, pass.subst(self.instance, program.pat_tys.items[@backingInt(source)]));
            }

            for (region.stmts.items) |source| {
                if (self.stmt_map.contains(source)) continue;
                const stmt: Lifted.Stmt = switch (program.lifted.getStmt(source)) {
                    .uninitialized => |pat_id| .{ .uninitialized = self.pat(pat_id) },
                    .let_ => |let_| .{ .let_ = .{ .pat = self.pat(let_.pat), .value = self.expr(let_.value), .recursive = let_.recursive, .comptime_site = let_.comptime_site } },
                    .expr => |id| .{ .expr = self.expr(id) },
                    .expect => |id| .{ .expect = self.expr(id) },
                    .dbg => |id| .{ .dbg = self.expr(id) },
                    .return_ => |ret| .{ .return_ = .{ .value = self.expr(ret.value), .target = ret.target } },
                    .crash => |message| .{ .crash = message },
                    .checked_error => |message| .{ .checked_error = message },
                };
                program.lifted.current_loc = program.lifted.stmtLoc(source);
                program.lifted.current_region = program.lifted.stmtRegion(source);
                program.lifted.current_inline_scope = program.lifted.stmtInlineScope(source);
                const copy = try program.lifted.addStmt(stmt);
                try self.stmt_map.put(pass.allocator, source, copy);
            }

            for (region.exprs.items) |source| {
                const copy = self.expr(source);
                const data = try self.cloneData(program.lifted.getExpr(source).data);
                program.lifted.setExprData(copy, data);
                if (pass.poly().expr_insts.get(source)) |inst| {
                    const target = namedFn(data);
                    if (pass.poly().fn_groups.items[@backingInt(target)] != self.instance.group) {
                        try pass.poly().expr_insts.put(pass.allocator, copy, try pass.substInst(self.instance, inst));
                    }
                }
            }
        }

        fn cloneData(self: *BodyCloner, data: Lifted.ExprData) Allocator.Error!Lifted.ExprData {
            const program = self.pass.program;
            return switch (data) {
                .lambda, .def_ref, .fn_def => Common.invariant("pre-lift function expression reached Lambda Specialize"),
                .local => |local_id| .{ .local = try self.local(local_id) },
                .unit, .@"unreachable", .int_lit, .frac_f32_lit, .frac_f64_lit, .dec_lit, .str_lit, .bytes_lit, .uninitialized, .crash, .checked_error, .comptime_exhaustiveness_failed => data,
                .static_data_candidate => |candidate| blk: {
                    var copied = candidate;
                    copied.value = switch (candidate.value) {
                        .runtime => |runtime| .{ .runtime = self.expr(runtime) },
                        .stored => candidate.value,
                    };
                    break :blk .{ .static_data_candidate = copied };
                },
                .comptime_value => |value| .{ .comptime_value = .{ .root = value.root, .initializer = self.expr(value.initializer) } },
                .typed_boundary => |boundary| .{ .typed_boundary = .{ .value = self.expr(boundary.value) } },
                .row_widen => |widen| .{ .row_widen = .{ .value = self.expr(widen.value) } },
                .list => |items| .{ .list = try self.exprSpan(items) },
                .tuple => |items| .{ .tuple = try self.exprSpan(items) },
                .record => |fields| .{ .record = try self.fieldExprs(fields) },
                .record_update => |update| .{ .record_update = .{ .base = self.expr(update.base), .fields = try self.fieldExprs(update.fields) } },
                .tag => |tag| .{ .tag = .{ .name = tag.name, .payloads = try self.exprSpan(tag.payloads) } },
                .nominal => |backing| .{ .nominal = self.expr(backing) },
                .let_ => |let_| .{ .let_ = .{ .bind = self.pat(let_.bind), .value = self.expr(let_.value), .rest = self.expr(let_.rest), .comptime_site = let_.comptime_site } },
                .fn_ref => |ref| .{ .fn_ref = .{ .fn_id = self.fnRef(ref.fn_id), .captures = try self.captureOperands(ref.captures) } },
                .call_value => |call| .{ .call_value = .{ .callee = self.expr(call.callee), .args = try self.exprSpan(call.args) } },
                .call_proc => |call| blk: {
                    var copied = call;
                    copied.callee = switch (call.callee) {
                        .lifted => |callee| .{ .lifted = self.fnRef(callee) },
                        .func => Common.invariant("unlifted call target reached Lambda Specialize"),
                    };
                    copied.args = try self.exprSpan(call.args);
                    copied.captures = try self.captureOperands(call.captures);
                    break :blk .{ .call_proc = copied };
                },
                .low_level => |call| .{ .low_level = .{ .op = call.op, .args = try self.exprSpan(call.args) } },
                .field_access => |access| .{ .field_access = .{ .receiver = self.expr(access.receiver), .segments = access.segments } },
                .tuple_access => |access| .{ .tuple_access = .{ .tuple = self.expr(access.tuple), .elem_index = access.elem_index } },
                .structural_eq => |eq| .{ .structural_eq = .{ .lhs = self.expr(eq.lhs), .rhs = self.expr(eq.rhs), .negated = eq.negated } },
                .structural_hash => |hash| .{ .structural_hash = .{ .value = self.expr(hash.value), .hasher = self.expr(hash.hasher) } },
                .match_ => |match| blk: {
                    const borrow = program.lifted.branchSpan(match.branches);
                    var branches: std.ArrayList(Lifted.Branch) = .empty;
                    defer branches.deinit(self.pass.allocator);
                    for (0..borrow.len) |index| {
                        const branch = GuardedList.at(borrow, index);
                        try branches.append(self.pass.allocator, .{
                            .pat = self.pat(branch.pat),
                            .bindings = try self.stmtSpan(branch.bindings),
                            .guard = if (branch.guard.get()) |guard| .of(self.expr(guard)) else .none,
                            .body = self.expr(branch.body),
                        });
                    }
                    break :blk .{ .match_ = .{ .scrutinee = self.expr(match.scrutinee), .branches = try program.lifted.addBranchSpan(branches.items), .comptime_site = match.comptime_site } };
                },
                .if_ => |if_| blk: {
                    const borrow = program.lifted.ifBranchSpan(if_.branches);
                    self.if_branches.clearRetainingCapacity();
                    for (0..borrow.len) |index| {
                        const branch = GuardedList.at(borrow, index);
                        try self.if_branches.append(self.pass.allocator, .{ .cond = self.expr(branch.cond), .body = self.expr(branch.body) });
                    }
                    break :blk .{ .if_ = .{ .branches = try program.lifted.addIfBranchSpan(self.if_branches.items), .final_else = self.expr(if_.final_else) } };
                },
                .uninitialized_payload => |payload| .{ .uninitialized_payload = .{ .condition = try self.local(payload.condition), .mask = payload.mask } },
                .if_initialized_payload => |payload| blk: {
                    var copied = payload;
                    copied.cond = self.expr(payload.cond);
                    copied.payload = try self.local(payload.payload);
                    copied.initialized = self.expr(payload.initialized);
                    copied.uninitialized = self.expr(payload.uninitialized);
                    break :blk .{ .if_initialized_payload = copied };
                },
                .try_sequence => |seq| blk: {
                    var copied = seq;
                    copied.try_expr = self.expr(seq.try_expr);
                    copied.ok_local = try self.local(seq.ok_local);
                    copied.err_target = if (seq.err_target) |target| try self.join(target) else null;
                    copied.ok_body = self.expr(seq.ok_body);
                    break :blk .{ .try_sequence = copied };
                },
                .try_record_sequence => |seq| blk: {
                    var copied = seq;
                    copied.try_expr = self.expr(seq.try_expr);
                    copied.value_local = try self.local(seq.value_local);
                    copied.rest_local = try self.local(seq.rest_local);
                    copied.err_target = if (seq.err_target) |target| try self.join(target) else null;
                    copied.ok_body = self.expr(seq.ok_body);
                    break :blk .{ .try_record_sequence = copied };
                },
                .block => |block| .{ .block = .{ .statements = try self.stmtSpan(block.statements), .final_expr = self.expr(block.final_expr) } },
                .loop_ => |loop| .{ .loop_ = .{ .params = try self.cloneTypedLocals(loop.params), .initial_values = try self.exprSpan(loop.initial_values), .body = self.expr(loop.body) } },
                .break_ => |value| .{ .break_ = self.optExpr(value) },
                .continue_ => |cont| .{ .continue_ = .{ .values = try self.exprSpan(cont.values) } },
                .join_point => |join_point| blk: {
                    var copied = join_point;
                    copied.id = try self.join(join_point.id);
                    copied.params = try self.cloneTypedLocals(join_point.params);
                    copied.retained = try self.cloneTypedLocals(join_point.retained);
                    copied.body = self.expr(join_point.body);
                    copied.remainder = self.expr(join_point.remainder);
                    break :blk .{ .join_point = copied };
                },
                .jump => |jump| blk: {
                    var copied = jump;
                    copied.target = try self.join(jump.target);
                    copied.args = try self.exprSpan(jump.args);
                    copied.loop_params = try self.cloneTypedLocals(jump.loop_params);
                    copied.loop_values = try self.exprSpan(jump.loop_values);
                    break :blk .{ .jump = copied };
                },
                .return_ => |ret| .{ .return_ = .{ .value = self.expr(ret.value), .target = ret.target } },
                .dbg => |inner| .{ .dbg = self.expr(inner) },
                .expect => |inner| .{ .expect = self.expr(inner) },
                .expect_err => |err| .{ .expect_err = .{ .msg = self.expr(err.msg), .region = err.region } },
                .literal_rejected => |rejected| .{ .literal_rejected = .{ .msg = self.expr(rejected.msg), .site = rejected.site } },
            };
        }
    };

    /// Point every reference at the function its instance has.
    fn rewriteReferences(self: *Pass) Allocator.Error!void {
        const poly_ = self.poly();
        var entries = poly_.expr_insts.iterator();
        while (entries.next()) |entry| {
            const expr_id = entry.key_ptr.*;
            const inst = entry.value_ptr.*;
            const class = self.classOf(inst);
            if (class == none_class) continue;
            const group = poly_.instantiations.items[@backingInt(inst)].group;
            const data = self.program.lifted.getExpr(expr_id).data;
            const target = self.instanceFn(group, class, namedFn(data));
            if (data == .fn_ref) {
                var copied = data.fn_ref;
                copied.fn_id = target;
                self.program.lifted.setExprData(expr_id, .{ .fn_ref = copied });
            } else {
                var copied = data.call_proc;
                copied.callee = .{ .lifted = target };
                self.program.lifted.setExprData(expr_id, .{ .call_proc = copied });
            }
        }
    }

    /// The function a reference names: only `fn_ref` and direct
    /// `call_proc` expressions record an instantiation.
    fn namedFn(data: Lifted.ExprData) Lifted.FnId {
        if (data == .fn_ref) return data.fn_ref.fn_id;
        if (data == .call_proc) return Lifted.localDirectCallee(data.call_proc).?;
        Common.invariant("an instantiation was recorded for an expression that names no function");
    }

    fn instanceFn(self: *Pass, group: u32, class: u32, fn_id: Lifted.FnId) Lifted.FnId {
        const size = self.program.binding_groups.group(group).len;
        const position = self.group_positions.items[@backingInt(fn_id)];
        return self.instance_fns[group].items[class * size + position];
    }

    /// Give every member the lifted function of its instance, then order
    /// every set by lambda symbol, one member per lambda.
    fn finishMembers(self: *Pass) Allocator.Error!void {
        const store = self.types();
        const poly_ = self.poly();
        const generic = self.instance_fns.len != 0;
        for (store.fn_members.items) |*member| {
            defer member.inst = .specialized;
            if (!generic or member.inst == .specialized) continue;
            const class = self.classOf(member.inst);
            if (class == none_class or class == 0) continue;
            const group = poly_.instantiations.items[@backingInt(member.inst)].group;
            const fn_id = self.fn_by_symbol.get(member.lambda) orelse continue;
            const target = self.instanceFn(group, class, fn_id);
            member.lambda = self.program.lifted.getFn(target).symbol;
            const locals = &self.instance_locals[group].items[class - 1];
            if (member.captures.count() == 0) continue;
            const slots = try store.addCaptureSlots(member.captures.count());
            for (slots.slots, 0..) |*slot, index| {
                slot.* = store.captureItem(member.captures, index);
                if (locals.get(slot.local)) |copy| {
                    slot.local = copy;
                    slot.symbol = self.program.lifted.getLocal(copy).symbol;
                }
            }
            member.captures = slots.span;
        }
        if (!generic) return;

        var sorted: std.ArrayList(Type.FnMember) = .empty;
        defer sorted.deinit(self.allocator);
        for (0..store.varCount()) |index| {
            const ty: Type.TypeVarId = @fromBackingInt(@intCast(index));
            const content = store.get(ty);
            if (content == .lambda_set) {
                if (try self.canonical(content.lambda_set, &sorted)) |span| try store.set(ty, .{ .lambda_set = span });
            } else if (content == .erased) {
                if (try self.canonical(content.erased.members, &sorted)) |span| {
                    var copied = content.erased;
                    copied.members = span;
                    try store.set(ty, .{ .erased = copied });
                }
            }
        }
    }

    fn canonical(self: *Pass, members: Type.Span, sorted: *std.ArrayList(Type.FnMember)) Allocator.Error!?Type.Span {
        if (members.count() < 2) return null;
        const store = self.types();
        const ascending = for (1..members.count()) |index| {
            if (@backingInt(store.memberItem(members, index - 1).lambda) >= @backingInt(store.memberItem(members, index).lambda)) break false;
        } else true;
        if (ascending) return null;
        sorted.clearRetainingCapacity();
        for (0..members.count()) |index| try sorted.append(self.allocator, store.memberItem(members, index));
        std.mem.sort(Type.FnMember, sorted.items, {}, struct {
            fn lessThan(_: void, lhs: Type.FnMember, rhs: Type.FnMember) bool {
                return @backingInt(lhs.lambda) < @backingInt(rhs.lambda);
            }
        }.lessThan);
        var kept: usize = 0;
        for (sorted.items) |member| {
            if (kept != 0 and sorted.items[kept - 1].lambda == member.lambda) continue;
            sorted.items[kept] = member;
            kept += 1;
        }
        return try store.addMembers(sorted.items[0..kept]);
    }

    fn rerootTables(self: *Pass) void {
        const store = self.types();
        for (self.program.expr_tys.items) |*ty| ty.* = store.rootCompressed(ty.*);
        for (self.program.pat_tys.items) |*ty| ty.* = store.rootCompressed(ty.*);
        for (self.program.local_tys.items) |*ty| ty.* = store.rootCompressed(ty.*);
        for (self.program.fn_tys.items) |*ty| ty.* = store.rootCompressed(ty.*);
        for (self.program.defs.items) |*def| def.ty = store.rootCompressed(def.ty);
        for (self.program.layout_requests.items) |*request| request.ty = store.rootCompressed(request.ty);
        for (self.program.runtime_schema_requests.items) |*request| request.ty = store.rootCompressed(request.ty);
    }
};
