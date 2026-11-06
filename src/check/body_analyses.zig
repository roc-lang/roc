//! Facts about a checked module's bodies that lowering reads through the body
//! reader (`checked_bodies.CheckedBodies`).
//!
//! The module index is a pure function of the module's CIR and the checker's
//! promoted local procedures. It records which nodes the checked module's
//! bodies reach, which top-level unit (definition, top-level `expect`,
//! declaration statement, or defaulted-field expression) owns each reached
//! node, and the per-binding facts lowering reads from declarations.
//!
//! Every other fact here is computed on first demand into a write-once slot
//! owned by the module and is never serialized: each top-level unit has one
//! slot holding its divergence, inspect-elision, state-merge and
//! loop-mutation facts; the module has one slot for its diagnostic-error
//! facts, whose recovery path follows constant references across units, one
//! for its single-source calls, and one indexing its exhaustiveness checks by
//! node.

const std = @import("std");
const base = @import("base");
const can = @import("can");
const collections = @import("collections");

const canonical = @import("canonical_names.zig");
const checked_artifact = @import("checked_artifact.zig");
const checked_bodies = @import("checked_bodies.zig");
const static_dispatch = @import("static_dispatch_registry.zig");

const Allocator = std.mem.Allocator;
const CIR = can.CIR;
const ModuleEnv = can.ModuleEnv;
const CheckedBodies = checked_bodies.CheckedBodies;
const ExprId = checked_bodies.ExprId;
const PatternId = checked_bodies.PatternId;
const StatementId = checked_bodies.StatementId;
const BinderId = checked_bodies.BinderId;

fn invariant(comptime message: []const u8) noreturn {
    base.invariant("checked body analysis invariant violated: " ++ message, .{});
}

/// The kind of node a reached CIR node is.
pub const NodeKind = enum(u8) { unreached, expr, pattern, statement };

/// Per-node declaration facts of the module index.
pub const NodeFlags = packed struct(u8) {
    /// The pattern of a global value definition.
    top_level_def_pattern: bool = false,
    /// A method declaration statement whose right-hand side checking
    /// rejected.
    rejected_method_decl: bool = false,
    /// The binding pattern of a promoted local procedure.
    promoted_pattern: bool = false,
    /// A reached `s_decl` statement of a recursive value.
    recursive_decl: bool = false,
    /// A binder bound by a recursive value declaration.
    recursive_binder: bool = false,
    /// The binder of a generalized callable lookup binding.
    scheme_alias_binder: bool = false,
    /// A defaulted-field default expression.
    default: bool = false,
    _: u1 = 0,
};

/// What owns a top-level unit's nodes.
pub const UnitKind = enum(u8) {
    /// A global value definition: its pattern and expression.
    definition,
    /// A top-level `expect` statement.
    expect,
    /// A top-level type, import, or annotation statement.
    declaration,
    /// A defaulted record field's default expression.
    default,
};

/// One top-level unit and the range of `ModuleIndex.unit_nodes` it owns.
pub const Unit = struct {
    kind: UnitKind,
    /// The definition's expression, the statement, or the default expression.
    root: u32,
    start: u32,
    len: u32,
};

/// The id of a top-level unit in its module index.
pub const UnitId = enum(u32) { _ };

const TopLevelDef = struct { pattern: u32, def: CIR.Def.Idx };

/// The CIR-only index of a checked module's bodies.
pub const ModuleIndex = struct {
    /// Per node: the kind of node the bodies reach it as.
    node_kinds: []NodeKind = &.{},
    /// Per node: its declaration facts.
    node_flags: []NodeFlags = &.{},
    /// Per node: its owning unit, valid where `node_kinds` is not `unreached`.
    unit_of: []UnitId = &.{},
    /// Per node: its position within its unit's nodes, valid where
    /// `node_kinds` is not `unreached`.
    position_in_unit: []u32 = &.{},
    units: []Unit = &.{},
    /// Every reached node, grouped by unit, ascending within each unit.
    unit_nodes: []u32 = &.{},
    /// Global value definitions by pattern node, ascending.
    top_level_defs: []TopLevelDef = &.{},
    /// Defaulted-field default expressions, ascending.
    defaults: []u32 = &.{},

    /// The index of `env`'s bodies. `promoted` lists the module's promoted
    /// local procedures, each with its binding `pattern`.
    pub fn build(allocator: Allocator, env: *const ModuleEnv, promoted: anytype) Allocator.Error!ModuleIndex {
        var builder = try Builder.init(allocator, env);
        defer builder.deinitScratch();
        errdefer builder.deinitAll();
        for (promoted) |entry| builder.node_flags[@backingInt(entry.pattern)].promoted_pattern = true;
        try builder.walk();
        return builder.finish();
    }

    pub fn deinit(self: *ModuleIndex, allocator: Allocator) void {
        allocator.free(self.node_kinds);
        allocator.free(self.node_flags);
        allocator.free(self.unit_of);
        allocator.free(self.position_in_unit);
        allocator.free(self.units);
        allocator.free(self.unit_nodes);
        allocator.free(self.top_level_defs);
        allocator.free(self.defaults);
        self.* = .{};
    }

    pub fn nodeKind(self: *const ModuleIndex, raw_node: u32) NodeKind {
        std.debug.assert(raw_node < self.node_kinds.len);
        return self.node_kinds[raw_node];
    }

    fn flags(self: *const ModuleIndex, raw_node: u32) NodeFlags {
        std.debug.assert(raw_node < self.node_flags.len);
        return self.node_flags[raw_node];
    }

    pub fn reachesExpr(self: *const ModuleIndex, expr: ExprId) bool {
        return self.nodeKind(@backingInt(expr)) == .expr;
    }

    pub fn reachesPattern(self: *const ModuleIndex, pattern: PatternId) bool {
        return self.nodeKind(@backingInt(pattern)) == .pattern;
    }

    pub fn reachesStatement(self: *const ModuleIndex, statement: StatementId) bool {
        return self.nodeKind(@backingInt(statement)) == .statement;
    }

    /// Whether a loop node (a `for` expression or a loop statement) is reached.
    pub fn reachesLoop(self: *const ModuleIndex, raw_node: u32) bool {
        return switch (self.nodeKind(raw_node)) {
            .expr, .statement => true,
            .unreached, .pattern => false,
        };
    }

    /// The unit that owns a reached node.
    pub fn unitOf(self: *const ModuleIndex, raw_node: u32) UnitId {
        if (self.nodeKind(raw_node) == .unreached) invariant("a body fact was requested for a node the bodies do not reach");
        return self.unit_of[raw_node];
    }

    pub fn unit(self: *const ModuleIndex, id: UnitId) Unit {
        return self.units[@backingInt(id)];
    }

    pub fn unitNodeSlice(self: *const ModuleIndex, id: UnitId) []const u32 {
        const owned = self.unit(id);
        return self.unit_nodes[owned.start..][0..owned.len];
    }

    /// The position of a reached node within its unit's nodes.
    pub fn positionInUnit(self: *const ModuleIndex, raw_node: u32) u32 {
        std.debug.assert(self.nodeKind(raw_node) != .unreached);
        return self.position_in_unit[raw_node];
    }

    /// The global value definition whose pattern is `pattern`.
    pub fn topLevelDef(self: *const ModuleIndex, pattern: PatternId) ?CIR.Def.Idx {
        const raw = @backingInt(pattern);
        if (!self.flags(raw).top_level_def_pattern) return null;
        const index = std.sort.binarySearch(TopLevelDef, self.top_level_defs, raw, orderTopLevelDef) orelse invariant("a global definition pattern was missing from the definitions");
        return self.top_level_defs[index].def;
    }

    pub fn isRejectedMethodDecl(self: *const ModuleIndex, statement: StatementId) bool {
        return self.flags(@backingInt(statement)).rejected_method_decl;
    }

    pub fn isPromotedPattern(self: *const ModuleIndex, pattern: PatternId) bool {
        return self.flags(@backingInt(pattern)).promoted_pattern;
    }

    pub fn isRecursiveDecl(self: *const ModuleIndex, statement: StatementId) bool {
        return self.flags(@backingInt(statement)).recursive_decl;
    }

    pub fn isRecursiveBinder(self: *const ModuleIndex, pattern: PatternId) bool {
        return self.flags(@backingInt(pattern)).recursive_binder;
    }

    pub fn isSchemeAlias(self: *const ModuleIndex, pattern: PatternId) bool {
        return self.flags(@backingInt(pattern)).scheme_alias_binder;
    }

    pub fn isDefault(self: *const ModuleIndex, expr: ExprId) bool {
        return self.flags(@backingInt(expr)).default;
    }

    fn orderTopLevelDef(key: u32, item: TopLevelDef) std.math.Order {
        return std.math.order(key, item.pattern);
    }

    const Pending = union(enum) {
        expr: ExprId,
        pattern: PatternId,
        statement: StatementId,
    };

    const Builder = struct {
        allocator: Allocator,
        env: *const ModuleEnv,
        node_kinds: []NodeKind,
        node_flags: []NodeFlags,
        unit_of: []UnitId,
        position_in_unit: []u32,
        units: std.ArrayList(Unit) = .empty,
        unit_nodes: std.ArrayList(u32) = .empty,
        work: std.ArrayList(Pending) = .empty,
        top_level_defs: std.ArrayList(TopLevelDef) = .empty,
        defaults: std.ArrayList(u32) = .empty,

        fn init(allocator: Allocator, env: *const ModuleEnv) Allocator.Error!Builder {
            const node_count = env.store.nodes.len();
            const node_kinds = try allocator.alloc(NodeKind, node_count);
            errdefer allocator.free(node_kinds);
            @memset(node_kinds, .unreached);
            const node_flags = try allocator.alloc(NodeFlags, node_count);
            errdefer allocator.free(node_flags);
            @memset(node_flags, .{});
            const unit_of = try allocator.alloc(UnitId, node_count);
            errdefer allocator.free(unit_of);
            const position_in_unit = try allocator.alloc(u32, node_count);
            return .{
                .allocator = allocator,
                .env = env,
                .node_kinds = node_kinds,
                .node_flags = node_flags,
                .unit_of = unit_of,
                .position_in_unit = position_in_unit,
            };
        }

        fn deinitScratch(self: *Builder) void {
            self.work.deinit(self.allocator);
        }

        fn deinitAll(self: *Builder) void {
            self.allocator.free(self.node_kinds);
            self.allocator.free(self.node_flags);
            self.allocator.free(self.unit_of);
            self.allocator.free(self.position_in_unit);
            self.units.deinit(self.allocator);
            self.unit_nodes.deinit(self.allocator);
            self.top_level_defs.deinit(self.allocator);
            self.defaults.deinit(self.allocator);
        }

        fn finish(self: *Builder) Allocator.Error!ModuleIndex {
            std.mem.sort(TopLevelDef, self.top_level_defs.items, {}, struct {
                fn lessThan(_: void, a: TopLevelDef, b: TopLevelDef) bool {
                    return a.pattern < b.pattern;
                }
            }.lessThan);
            std.mem.sort(u32, self.defaults.items, {}, std.sort.asc(u32));
            dedupeSorted(&self.defaults);
            const allocator = self.allocator;
            const units = try self.units.toOwnedSlice(allocator);
            errdefer allocator.free(units);
            const unit_nodes = try self.unit_nodes.toOwnedSlice(allocator);
            errdefer allocator.free(unit_nodes);
            const top_level_defs = try self.top_level_defs.toOwnedSlice(allocator);
            errdefer allocator.free(top_level_defs);
            const defaults = try self.defaults.toOwnedSlice(allocator);
            return .{
                .node_kinds = self.node_kinds,
                .node_flags = self.node_flags,
                .unit_of = self.unit_of,
                .position_in_unit = self.position_in_unit,
                .units = units,
                .unit_nodes = unit_nodes,
                .top_level_defs = top_level_defs,
                .defaults = defaults,
            };
        }

        fn walk(self: *Builder) Allocator.Error!void {
            const env = self.env;
            const store = &env.store;

            for (store.sliceDefs(env.global_value_defs)) |def_idx| {
                const def = store.getDef(def_idx);
                try self.top_level_defs.append(self.allocator, .{ .pattern = @backingInt(def.pattern), .def = def_idx });
                self.node_flags[@backingInt(def.pattern)].top_level_def_pattern = true;
                const id = try self.beginUnit(.definition, @backingInt(def.expr));
                try self.mark(.{ .pattern = def.pattern }, id);
                try self.mark(.{ .expr = def.expr }, id);
                try self.drain(id);
                try self.endUnit(id);
            }

            // Top-level expects have no containing expression, so they are
            // units of their own. Other value statements are reached from
            // their containing expression or from global definitions;
            // reaching them globally would walk into the original children
            // of an expression that checking replaced with `e_runtime_error`.
            for (store.sliceStatements(env.all_statements)) |statement_idx| {
                const kind: UnitKind = switch (store.getStatement(statement_idx)) {
                    .s_import,
                    .s_alias_decl,
                    .s_nominal_decl,
                    .s_where_alias_decl,
                    .s_type_anno,
                    .s_type_var_alias,
                    => .declaration,
                    .s_expect => .expect,
                    .s_decl,
                    .s_var,
                    .s_var_uninitialized,
                    .s_reassign,
                    .s_crash,
                    .s_dbg,
                    .s_for,
                    .s_expr,
                    .s_return,
                    .s_while,
                    .s_infinite_loop,
                    .s_breakable_loop,
                    .s_runtime_error,
                    .s_break,
                    => continue,
                };
                if (self.node_kinds[@backingInt(statement_idx)] != .unreached) continue;
                const id = try self.beginUnit(kind, @backingInt(statement_idx));
                try self.mark(.{ .statement = statement_idx }, id);
                try self.drain(id);
                try self.endUnit(id);
            }

            // Defaulted-field annotations hold value expressions no
            // definition reaches; lowering materializes each at every
            // construction site that omits its field.
            const node_count: u32 = @intCast(store.nodes.len());
            var raw: u32 = 0;
            while (raw < node_count) : (raw += 1) {
                const node: CIR.Node.Idx = @fromBackingInt(@intCast(raw));
                if (store.nodes.get(node).tag != .ty_record_field_defaulted) continue;
                const default_raw = store.nodes.get(node).getPayload().ty_record_field_defaulted.default_value;
                try self.defaults.append(self.allocator, default_raw);
                self.node_flags[default_raw].default = true;
                if (self.node_kinds[default_raw] != .unreached) continue;
                const id = try self.beginUnit(.default, default_raw);
                try self.mark(.{ .expr = @fromBackingInt(@intCast(default_raw)) }, id);
                try self.drain(id);
                try self.endUnit(id);
            }

            try self.indexDeclarations();
        }

        fn beginUnit(self: *Builder, kind: UnitKind, root: u32) Allocator.Error!UnitId {
            const id: UnitId = @fromBackingInt(@intCast(self.units.items.len));
            try self.units.append(self.allocator, .{
                .kind = kind,
                .root = root,
                .start = @intCast(self.unit_nodes.items.len),
                .len = 0,
            });
            return id;
        }

        fn endUnit(self: *Builder, id: UnitId) Allocator.Error!void {
            const owned = &self.units.items[@backingInt(id)];
            owned.len = @as(u32, @intCast(self.unit_nodes.items.len)) - owned.start;
            const nodes = self.unit_nodes.items[owned.start..];
            std.mem.sort(u32, nodes, {}, std.sort.asc(u32));
            for (nodes, 0..) |raw, position| self.position_in_unit[raw] = @intCast(position);
        }

        fn mark(self: *Builder, pending: Pending, id: UnitId) Allocator.Error!void {
            const raw: u32, const kind: NodeKind = switch (pending) {
                .expr => |expr| .{ @backingInt(expr), .expr },
                .pattern => |pattern| .{ @backingInt(pattern), .pattern },
                .statement => |statement| .{ @backingInt(statement), .statement },
            };
            if (raw >= self.node_kinds.len) invariant("a body node was out of range");
            if (self.node_kinds[raw] != .unreached) return;
            self.node_kinds[raw] = kind;
            self.unit_of[raw] = id;
            try self.unit_nodes.append(self.allocator, raw);
            try self.work.append(self.allocator, pending);
        }

        fn drain(self: *Builder, id: UnitId) Allocator.Error!void {
            while (self.work.pop()) |pending| switch (pending) {
                .expr => |expr| try self.markExprChildren(expr, id),
                .pattern => |pattern| try self.markPatternChildren(pattern, id),
                .statement => |statement| try self.markStatementChildren(statement, id),
            };
        }

        fn markExprs(self: *Builder, exprs: []const ExprId, id: UnitId) Allocator.Error!void {
            for (exprs) |expr| try self.mark(.{ .expr = expr }, id);
        }

        fn markPatterns(self: *Builder, patterns: []const PatternId, id: UnitId) Allocator.Error!void {
            for (patterns) |pattern| try self.mark(.{ .pattern = pattern }, id);
        }

        fn markExprChildren(self: *Builder, expr_idx: ExprId, id: UnitId) Allocator.Error!void {
            const store = &self.env.store;
            switch (store.getExpr(expr_idx)) {
                .e_str => |str| try self.markExprs(store.sliceExpr(str.span), id),
                .e_list => |list| try self.markExprs(store.sliceExpr(list.elems), id),
                .e_tuple => |tuple| try self.markExprs(store.sliceExpr(tuple.elems), id),
                .e_match => |match| {
                    try self.mark(.{ .expr = match.cond }, id);
                    for (store.sliceMatchBranches(match.branches)) |branch_idx| {
                        const branch = store.getMatchBranch(branch_idx);
                        for (store.sliceMatchBranchPatterns(branch.patterns)) |branch_pattern_idx| {
                            try self.mark(.{ .pattern = store.getMatchBranchPattern(branch_pattern_idx).pattern }, id);
                        }
                        if (branch.guard) |guard| try self.mark(.{ .expr = guard }, id);
                        try self.mark(.{ .expr = branch.value }, id);
                    }
                },
                .e_if => |if_| {
                    for (store.sliceIfBranches(if_.branches)) |branch_idx| {
                        const branch = store.getIfBranch(branch_idx);
                        try self.mark(.{ .expr = branch.cond }, id);
                        try self.mark(.{ .expr = branch.body }, id);
                    }
                    try self.mark(.{ .expr = if_.final_else }, id);
                },
                .e_call => |call| {
                    try self.mark(.{ .expr = call.func }, id);
                    try self.markExprs(store.sliceExpr(call.args), id);
                },
                .e_record => |record| {
                    for (store.sliceRecordFields(record.fields)) |field_idx| {
                        try self.mark(.{ .expr = store.getRecordField(field_idx).value }, id);
                    }
                    if (record.ext) |ext| try self.mark(.{ .expr = ext }, id);
                },
                .e_block => |block| {
                    for (store.sliceStatements(block.stmts)) |statement| try self.mark(.{ .statement = statement }, id);
                    try self.mark(.{ .expr = block.final_expr }, id);
                },
                .e_tag => |tag| try self.markExprs(store.sliceExpr(tag.args), id),
                .e_nominal => |nominal| try self.mark(.{ .expr = nominal.backing_expr }, id),
                .e_nominal_external => |nominal| try self.mark(.{ .expr = nominal.backing_expr }, id),
                .e_closure => |closure| {
                    try self.mark(.{ .expr = closure.lambda_idx }, id);
                    for (store.sliceCaptures(closure.captures)) |capture_idx| {
                        try self.mark(.{ .pattern = store.getCapture(capture_idx).pattern_idx }, id);
                    }
                },
                .e_lambda => |lambda| {
                    try self.markPatterns(store.slicePatterns(lambda.args), id);
                    try self.mark(.{ .expr = lambda.body }, id);
                },
                .e_binop => |binop| {
                    try self.mark(.{ .expr = binop.lhs }, id);
                    try self.mark(.{ .expr = binop.rhs }, id);
                },
                .e_unary_minus => |unary| try self.mark(.{ .expr = unary.expr }, id),
                .e_field_access => |field| try self.mark(.{ .expr = field.receiver }, id),
                inline .e_method_call, .e_dispatch_call => |call| {
                    try self.mark(.{ .expr = call.receiver }, id);
                    try self.markExprs(store.sliceExpr(call.args), id);
                },
                .e_interpolation => |interpolation| {
                    try self.mark(.{ .expr = interpolation.first }, id);
                    try self.markExprs(store.sliceExpr(interpolation.parts), id);
                },
                .e_structural_eq => |eq| {
                    try self.mark(.{ .expr = eq.lhs }, id);
                    try self.mark(.{ .expr = eq.rhs }, id);
                },
                .e_structural_hash => |hash| {
                    try self.mark(.{ .expr = hash.value }, id);
                    try self.mark(.{ .expr = hash.hasher }, id);
                },
                .e_method_eq => |eq| {
                    try self.mark(.{ .expr = eq.lhs }, id);
                    try self.mark(.{ .expr = eq.rhs }, id);
                },
                .e_type_method_call => |call| {
                    try self.mark(.{ .statement = call.type_dispatch_stmt }, id);
                    try self.markExprs(store.sliceExpr(call.args), id);
                },
                .e_type_dispatch_call => |call| {
                    switch (call.owner) {
                        .statement => |statement| try self.mark(.{ .statement = statement }, id),
                        .dispatcher => {},
                    }
                    try self.markExprs(store.sliceExpr(call.args), id);
                },
                .e_tuple_access => |access| try self.mark(.{ .expr = access.tuple }, id),
                .e_dbg => |dbg| try self.mark(.{ .expr = dbg.expr }, id),
                .e_expect_err => |expect_err| try self.mark(.{ .expr = expect_err.expr }, id),
                .e_expect => |expect| try self.mark(.{ .expr = expect.body }, id),
                .e_return => |ret| {
                    try self.mark(.{ .expr = ret.expr }, id);
                    try self.mark(.{ .expr = ret.lambda }, id);
                },
                .e_for => |for_| {
                    try self.mark(.{ .pattern = for_.patt }, id);
                    try self.mark(.{ .expr = for_.expr }, id);
                    try self.mark(.{ .expr = for_.body }, id);
                },
                .e_hosted_lambda => |hosted| try self.markPatterns(store.slicePatterns(hosted.args), id),
                .e_run_low_level => |run| try self.markExprs(store.sliceExpr(run.args), id),
                .e_runtime_error => |runtime_error| try self.markExprs(store.sliceExpr(runtime_error.evaluated), id),
                .e_num,
                .e_frac_f32,
                .e_frac_f64,
                .e_dec,
                .e_dec_small,
                .e_num_from_numeral,
                .e_typed_int,
                .e_typed_frac,
                .e_typed_num_from_numeral,
                .e_str_segment,
                .e_bytes_literal,
                .e_lookup_local,
                .e_lookup_external,
                .e_lookup_associated_resolved,
                .e_lookup_required,
                .e_empty_list,
                .e_empty_record,
                .e_zero_argument_tag,
                .e_crash,
                .e_ellipsis,
                .e_anno_only,
                .e_derived_method,
                .e_break,
                => {},
                .e_lookup_associated_local, .e_lookup_associated => invariant("an unresolved associated lookup reached a checked body"),
                .e_deferred_import_ref => invariant("a deferred import reference reached a checked body"),
            }
        }

        fn markPatternChildren(self: *Builder, pattern_idx: PatternId, id: UnitId) Allocator.Error!void {
            const store = &self.env.store;
            switch (store.getPattern(pattern_idx)) {
                .as => |as| try self.mark(.{ .pattern = as.pattern }, id),
                .applied_tag => |tag| try self.markPatterns(store.slicePatterns(tag.args), id),
                .nominal => |nominal| try self.mark(.{ .pattern = nominal.backing_pattern }, id),
                .nominal_external => |nominal| try self.mark(.{ .pattern = nominal.backing_pattern }, id),
                .record_destructure => |record| for (store.sliceRecordDestructs(record.destructs)) |destruct_idx| {
                    try self.mark(.{ .pattern = store.getRecordDestruct(destruct_idx).kind.toPatternIdx() }, id);
                },
                .list => |list| {
                    try self.markPatterns(store.slicePatterns(list.patterns), id);
                    if (list.rest_info) |rest| if (rest.pattern) |rest_pattern| try self.mark(.{ .pattern = rest_pattern }, id);
                },
                .tuple => |tuple| try self.markPatterns(store.slicePatterns(tuple.patterns), id),
                .str_interpolation => |str| {
                    var offset: u32 = 0;
                    while (offset < str.steps.span.len) : (offset += 1) {
                        if (store.getStrPatternStep(str.steps, offset).capture) |capture| try self.mark(.{ .pattern = capture }, id);
                    }
                },
                .assign,
                .var_assign,
                .num_literal,
                .num_from_numeral_literal,
                .small_dec_literal,
                .dec_literal,
                .frac_f32_literal,
                .frac_f64_literal,
                .str_literal,
                .underscore,
                .runtime_error,
                => {},
                .deferred_import_ref => invariant("a deferred import reference pattern reached a checked body"),
            }
        }

        fn markStatementChildren(self: *Builder, statement_idx: StatementId, id: UnitId) Allocator.Error!void {
            const store = &self.env.store;
            switch (store.getStatement(statement_idx)) {
                .s_decl => |decl| {
                    try self.mark(.{ .pattern = decl.pattern }, id);
                    try self.mark(.{ .expr = decl.expr }, id);
                },
                .s_var => |var_| {
                    try self.mark(.{ .pattern = var_.pattern_idx }, id);
                    try self.mark(.{ .expr = var_.expr }, id);
                },
                .s_var_uninitialized => |var_| try self.mark(.{ .pattern = var_.pattern_idx }, id),
                .s_reassign => |reassign| {
                    try self.mark(.{ .pattern = reassign.pattern_idx }, id);
                    try self.mark(.{ .expr = reassign.expr }, id);
                },
                .s_dbg => |dbg| try self.mark(.{ .expr = dbg.expr }, id),
                .s_expr => |expr| try self.mark(.{ .expr = expr.expr }, id),
                .s_expect => |expect| try self.mark(.{ .expr = expect.body }, id),
                .s_for => |for_| {
                    try self.mark(.{ .pattern = for_.patt }, id);
                    try self.mark(.{ .expr = for_.expr }, id);
                    try self.mark(.{ .expr = for_.body }, id);
                },
                inline .s_while, .s_infinite_loop, .s_breakable_loop => |loop| {
                    try self.mark(.{ .expr = loop.cond }, id);
                    try self.mark(.{ .expr = loop.body }, id);
                },
                .s_return => |ret| {
                    try self.mark(.{ .expr = ret.expr }, id);
                    try self.mark(.{ .expr = ret.lambda }, id);
                },
                .s_runtime_error => |runtime_error| try self.markExprs(store.sliceExpr(runtime_error.evaluated), id),
                .s_crash,
                .s_break,
                .s_import,
                .s_alias_decl,
                .s_nominal_decl,
                .s_where_alias_decl,
                .s_type_anno,
                .s_type_var_alias,
                => {},
            }
        }

        /// Rejected method declarations, recursive value declarations and
        /// their binders, and generalized callable lookup bindings.
        fn indexDeclarations(self: *Builder) Allocator.Error!void {
            const env = self.env;
            const store = &env.store;
            for (env.method_defs.entries.items) |entry| {
                if (store.nodes.get(entry.value.type_node_idx).tag != .statement_decl) continue;
                const statement: StatementId = @fromBackingInt(@intCast(@backingInt(entry.value.type_node_idx)));
                const decl = store.getStatement(statement).s_decl;
                if (store.getExpr(decl.expr) != .e_runtime_error) continue;
                self.node_flags[@backingInt(statement)].rejected_method_decl = true;
            }

            var recursively_referenced = collections.DenseMap(u32, void).init(self.allocator);
            defer recursively_referenced.deinit();
            for (env.scheme_uses.items.items) |record| {
                if (record.slot_kind != @backingInt(ModuleEnv.SchemeUseRecord.Slot.recursive_value_reference)) continue;
                const node: CIR.Node.Idx = @fromBackingInt(@intCast(record.node_idx));
                if (store.nodes.get(node).tag != .expr_var) continue;
                const lookup = store.getExpr(@fromBackingInt(@intCast(record.node_idx)));
                if (lookup != .e_lookup_local) continue;
                try recursively_referenced.put(@backingInt(lookup.e_lookup_local.pattern_idx), {});
            }

            var binders: std.ArrayList(PatternId) = .empty;
            defer binders.deinit(self.allocator);
            var scratch: std.ArrayList(PatternId) = .empty;
            defer scratch.deinit(self.allocator);
            for (self.node_kinds, 0..) |kind, raw| {
                if (kind != .statement) continue;
                const statement: StatementId = @fromBackingInt(@intCast(raw));
                const data = store.getStatement(statement);
                if (data != .s_decl) continue;
                const decl = data.s_decl;
                if (self.node_flags[@backingInt(decl.pattern)].promoted_pattern) continue;
                if (bindsProcedureAlias(env, decl.pattern, decl.expr)) {
                    self.node_flags[@backingInt(decl.pattern)].scheme_alias_binder = true;
                }
                if (recursively_referenced.count() == 0) continue;
                binders.clearRetainingCapacity();
                try can.DependencyGraph.appendPatternBinders(env, decl.pattern, &binders, &scratch, self.allocator);
                const recursive = for (binders.items) |binder| {
                    if (recursively_referenced.contains(@backingInt(binder))) break true;
                } else false;
                if (!recursive) continue;
                self.node_flags[@backingInt(statement)].recursive_decl = true;
                for (binders.items) |binder| self.node_flags[@backingInt(binder)].recursive_binder = true;
            }
        }
    };
};

/// Whether `pattern = expr` binds an exact procedure alias, local or top
/// level (design.md "Procedure Aliases").
pub fn bindsProcedureAlias(env: *const ModuleEnv, pattern: PatternId, expr: ExprId) bool {
    const rhs = env.store.getExpr(expr);
    const is_lookup = rhs == .e_lookup_local or rhs == .e_lookup_external or
        rhs == .e_lookup_required or rhs == .e_lookup_associated_resolved;
    return is_lookup and env.store.getPattern(pattern) == .assign and
        env.nodeIsBindingScheme(ModuleEnv.nodeIdxFrom(pattern)) and
        env.types.varResolvesToFunction(ModuleEnv.varFrom(pattern));
}

fn dedupeSorted(list: *std.ArrayList(u32)) void {
    var len: usize = 0;
    for (list.items) |item| {
        if (len > 0 and list.items[len - 1] == item) continue;
        list.items[len] = item;
        len += 1;
    }
    list.shrinkRetainingCapacity(len);
}

/// A range of a unit's binder pool.
const BinderSpan = struct { start: u32 = 0, len: u32 = 0 };

/// Whether inline expects run when lowering evaluates a body.
pub const InlineExpectMode = enum { run, omit };

/// The divergence bit of one inline-expect mode.
const diverges_run: u8 = 1;
const diverges_omit: u8 = 2;
const diverges_both: u8 = diverges_run | diverges_omit;

/// The body-level facts of one top-level unit, parallel to the unit's nodes.
const UnitFacts = struct {
    const inspect_elidable: u8 = 4;

    /// Per unit node: `diverges_run`, `diverges_omit` and `inspect_elidable`.
    flags: []u8,
    /// For an `if`, `match`, or `for` expression and for an expression
    /// statement: the binders it reassigns anywhere inside it.
    state_merges: []?BinderSpan,
    /// For a `for` expression or a loop statement: the binders its condition
    /// and body reassign.
    loop_mutations: []?BinderSpan,
    binders: []BinderId,

    fn destroy(self: *UnitFacts, allocator: Allocator) void {
        allocator.free(self.flags);
        allocator.free(self.state_merges);
        allocator.free(self.loop_mutations);
        allocator.free(self.binders);
        allocator.destroy(self);
    }
};

const UnitSlot = collections.OnceSlot(*UnitFacts, .body_unit_facts);

/// One bit per node of the module.
pub const NodeBits = struct {
    bits: std.DynamicBitSetUnmanaged = .{},

    pub fn isSet(self: *const NodeBits, raw_node: u32) bool {
        std.debug.assert(raw_node < self.bits.bit_length);
        return self.bits.isSet(raw_node);
    }

    pub fn deinit(self: *NodeBits, allocator: Allocator) void {
        self.bits.deinit(allocator);
    }
};

/// The module's templates with a single-source call
/// (`computeSingleSourceTemplates`), ascending.
const SingleSourceTemplates = struct {
    templates: []canonical.CheckedProcedureTemplateId,

    fn destroy(self: *SingleSourceTemplates, allocator: Allocator) void {
        allocator.free(self.templates);
        allocator.destroy(self);
    }
};

/// The module's nested procedure site expressions with a single-source call
/// (`computeSingleSourceSites`), ascending.
const SingleSourceSites = struct {
    sites: []u32,

    fn destroy(self: *SingleSourceSites, allocator: Allocator) void {
        allocator.free(self.sites);
        allocator.destroy(self);
    }
};

fn orderU32(key: u32, item: u32) std.math.Order {
    return std.math.order(key, item);
}

/// The module's exhaustiveness checks by node: each match expression's and
/// each destructuring pattern's check, whose site id is its index in
/// `ModuleEnv.exhaustiveness_obligations`.
const ExhaustivenessIndex = struct {
    /// Ordered by node, then by site.
    entries: []Entry,

    const Entry = struct {
        node: u32,
        source: std.meta.Tag(ModuleEnv.ExhaustivenessObligation.Source),
        site: checked_artifact.CheckedExhaustivenessSiteId,

        fn lessThan(_: void, left: Entry, right: Entry) bool {
            if (left.node != right.node) return left.node < right.node;
            return @backingInt(left.site) < @backingInt(right.site);
        }
    };

    fn compute(allocator: Allocator, env: *const ModuleEnv) Allocator.Error!*ExhaustivenessIndex {
        const obligations = env.exhaustiveness_obligations.items.items;
        const entries = try allocator.alloc(Entry, obligations.len);
        errdefer allocator.free(entries);
        for (obligations, entries, 0..) |obligation, *entry, raw_site| {
            entry.* = .{
                .node = switch (obligation.source) {
                    .match_expr => |expr| @backingInt(expr),
                    .destructure_pattern => |pattern| @backingInt(pattern),
                },
                .source = obligation.source,
                .site = @fromBackingInt(@intCast(raw_site)),
            };
        }
        std.mem.sort(Entry, entries, {}, Entry.lessThan);
        const index = try allocator.create(ExhaustivenessIndex);
        index.* = .{ .entries = entries };
        return index;
    }

    /// The checks at `raw_node`, in site order.
    fn atNode(self: *const ExhaustivenessIndex, raw_node: u32) []const Entry {
        var low: usize = 0;
        var high: usize = self.entries.len;
        while (low < high) {
            const mid = low + (high - low) / 2;
            if (self.entries[mid].node < raw_node) low = mid + 1 else high = mid;
        }
        var end = low;
        while (end < self.entries.len and self.entries[end].node == raw_node) end += 1;
        return self.entries[low..end];
    }

    /// The first check, in site order, of `source` kind at `raw_node`.
    fn find(self: *const ExhaustivenessIndex, raw_node: u32, source: std.meta.Tag(ModuleEnv.ExhaustivenessObligation.Source)) ?checked_artifact.CheckedExhaustivenessSiteId {
        for (self.atNode(raw_node)) |entry| {
            if (entry.source == source) return entry.site;
        }
        return null;
    }

    fn destroy(self: *ExhaustivenessIndex, allocator: Allocator) void {
        allocator.free(self.entries);
        allocator.destroy(self);
    }
};

/// The body facts of one checked module, owned by the module. `allocator`
/// must be safe to use from every thread that reads the module.
pub const BodyAnalyses = struct {
    allocator: Allocator,
    io: std.Io,
    env: *const ModuleEnv,
    index: ModuleIndex,
    unit_slots: []UnitSlot,
    diagnostic_errors: collections.OnceSlot(*NodeBits, .body_diagnostic_errors) = .empty,
    single_source_templates: collections.OnceSlot(*SingleSourceTemplates, .body_single_source_calls) = .empty,
    single_source_sites: collections.OnceSlot(*SingleSourceSites, .body_single_source_sites) = .empty,
    exhaustiveness_index: collections.OnceSlot(*ExhaustivenessIndex, .body_exhaustiveness_index) = .empty,
    /// Set once the module is published (`CheckedModuleArtifact.openToAnalyses`):
    /// no slot of the module runs before it.
    published: std.atomic.Value(bool) = .init(false),

    /// The analyses of `env`'s bodies. `promoted` lists the module's
    /// promoted local procedures, each with its binding `pattern`.
    pub fn create(
        allocator: Allocator,
        io: std.Io,
        env: *const ModuleEnv,
        promoted: anytype,
    ) Allocator.Error!*BodyAnalyses {
        const analyses = try allocator.create(BodyAnalyses);
        errdefer allocator.destroy(analyses);
        var index = try ModuleIndex.build(allocator, env, promoted);
        errdefer index.deinit(allocator);
        const unit_slots = try allocator.alloc(UnitSlot, index.units.len);
        @memset(unit_slots, .empty);
        analyses.* = .{
            .allocator = allocator,
            .io = io,
            .env = env,
            .index = index,
            .unit_slots = unit_slots,
        };
        return analyses;
    }

    /// Record that the module is published.
    pub fn markPublished(self: *BodyAnalyses) void {
        self.published.store(true, .release);
    }

    fn assertPublished(self: *const BodyAnalyses) void {
        if (std.debug.runtime_safety and !self.published.load(.acquire)) invariant("a body analysis of a module was claimed before the module was published");
    }

    pub fn destroy(self: *BodyAnalyses) void {
        const allocator = self.allocator;
        for (self.unit_slots) |*slot| if (slot.peek()) |facts| facts.destroy(allocator);
        allocator.free(self.unit_slots);
        if (self.diagnostic_errors.peek()) |bits| {
            bits.deinit(allocator);
            allocator.destroy(bits);
        }
        if (self.single_source_templates.peek()) |facts| facts.destroy(allocator);
        if (self.single_source_sites.peek()) |facts| facts.destroy(allocator);
        if (self.exhaustiveness_index.peek()) |index| index.destroy(allocator);
        self.index.deinit(allocator);
        allocator.destroy(self);
    }

    fn unitFacts(self: *BodyAnalyses, bodies: CheckedBodies, raw_node: u32) Allocator.Error!struct { *const UnitFacts, u32 } {
        const unit_id = self.index.unitOf(raw_node);
        const facts = try self.unit_slots[@backingInt(unit_id)].getOrCompute(
            self.io,
            Allocator.Error,
            UnitRequest{ .analyses = self, .bodies = bodies, .unit = unit_id },
            UnitRequest.compute,
        );
        return .{ facts, self.index.position_in_unit[raw_node] };
    }

    const UnitRequest = struct {
        analyses: *BodyAnalyses,
        bodies: CheckedBodies,
        unit: UnitId,

        fn compute(request: UnitRequest) Allocator.Error!*UnitFacts {
            return computeUnitFacts(request.analyses.allocator, request.bodies, request.unit);
        }
    };

    pub fn exprDiverges(self: *BodyAnalyses, bodies: CheckedBodies, expr: ExprId, mode: InlineExpectMode) Allocator.Error!bool {
        self.assertPublished();
        const facts, const position = try self.unitFacts(bodies, @backingInt(expr));
        return facts.flags[position] & divergenceFlag(mode) != 0;
    }

    pub fn statementDiverges(self: *BodyAnalyses, bodies: CheckedBodies, statement: StatementId, mode: InlineExpectMode) Allocator.Error!bool {
        self.assertPublished();
        const facts, const position = try self.unitFacts(bodies, @backingInt(statement));
        return facts.flags[position] & divergenceFlag(mode) != 0;
    }

    pub fn exprEvaluationMayBeElidedForInspect(self: *BodyAnalyses, bodies: CheckedBodies, expr: ExprId) Allocator.Error!bool {
        self.assertPublished();
        const facts, const position = try self.unitFacts(bodies, @backingInt(expr));
        return facts.flags[position] & UnitFacts.inspect_elidable != 0;
    }

    pub fn stateMergeBinders(self: *BodyAnalyses, bodies: CheckedBodies, raw_node: u32) Allocator.Error![]const BinderId {
        self.assertPublished();
        const facts, const position = try self.unitFacts(bodies, raw_node);
        const span = facts.state_merges[position] orelse invariant("a state merge was requested for a node that owns none");
        return facts.binders[span.start..][0..span.len];
    }

    pub fn loopMutationBinders(self: *BodyAnalyses, bodies: CheckedBodies, raw_node: u32) Allocator.Error![]const BinderId {
        self.assertPublished();
        const facts, const position = try self.unitFacts(bodies, raw_node);
        const span = facts.loop_mutations[position] orelse invariant("loop mutations were requested for a node that is not a loop");
        return facts.binders[span.start..][0..span.len];
    }

    pub fn exprContainsDiagnosticError(self: *BodyAnalyses, bodies: CheckedBodies, expr: ExprId) Allocator.Error!bool {
        self.assertPublished();
        const bits = try self.diagnostic_errors.getOrCompute(self.io, Allocator.Error, DiagnosticRequest{ .analyses = self, .bodies = bodies }, DiagnosticRequest.compute);
        return bits.isSet(@backingInt(expr));
    }

    pub fn patternContainsDiagnosticError(self: *BodyAnalyses, bodies: CheckedBodies, pattern: PatternId) Allocator.Error!bool {
        self.assertPublished();
        const bits = try self.diagnostic_errors.getOrCompute(self.io, Allocator.Error, DiagnosticRequest{ .analyses = self, .bodies = bodies }, DiagnosticRequest.compute);
        return bits.isSet(@backingInt(pattern));
    }

    const DiagnosticRequest = struct {
        analyses: *BodyAnalyses,
        bodies: CheckedBodies,

        fn compute(request: DiagnosticRequest) Allocator.Error!*NodeBits {
            const allocator = request.analyses.allocator;
            const bits = try allocator.create(NodeBits);
            errdefer allocator.destroy(bits);
            bits.* = try diagnosticErrors(allocator, request.bodies, .final);
            return bits;
        }
    };

    pub fn templateHasSingleSourceCall(self: *BodyAnalyses, bodies: CheckedBodies, template: canonical.CheckedProcedureTemplateId) Allocator.Error!bool {
        self.assertPublished();
        const facts = try self.single_source_templates.getOrCompute(self.io, Allocator.Error, SingleSourceRequest{ .analyses = self, .bodies = bodies }, SingleSourceRequest.templates);
        return std.sort.binarySearch(canonical.CheckedProcedureTemplateId, facts.templates, template, struct {
            fn order(key: canonical.CheckedProcedureTemplateId, item: canonical.CheckedProcedureTemplateId) std.math.Order {
                return std.math.order(@backingInt(key), @backingInt(item));
            }
        }.order) != null;
    }

    pub fn nestedProcSiteIsSingleSourceCall(self: *BodyAnalyses, bodies: CheckedBodies, site: canonical.NestedProcSiteId) Allocator.Error!bool {
        self.assertPublished();
        const facts = try self.single_source_sites.getOrCompute(self.io, Allocator.Error, SingleSourceRequest{ .analyses = self, .bodies = bodies }, SingleSourceRequest.sites);
        return std.sort.binarySearch(u32, facts.sites, @backingInt(site), orderU32) != null;
    }

    /// The exhaustiveness check checking left to evaluation at a match
    /// expression or destructuring pattern node.
    pub fn exhaustivenessSiteOf(
        self: *BodyAnalyses,
        bodies: CheckedBodies,
        raw_node: u32,
        source: std.meta.Tag(ModuleEnv.ExhaustivenessObligation.Source),
    ) Allocator.Error!?checked_artifact.CheckedExhaustivenessSiteId {
        const index = try self.exhaustiveness_index.getOrCompute(self.io, Allocator.Error, ExhaustivenessIndexRequest{ .analyses = self, .bodies = bodies }, ExhaustivenessIndexRequest.compute);
        return index.find(raw_node, source);
    }

    /// Append every exhaustiveness check of `source` kind at `raw_node`, in
    /// site order, to `found`.
    pub fn appendExhaustivenessSitesAt(
        self: *BodyAnalyses,
        bodies: CheckedBodies,
        raw_node: u32,
        source: std.meta.Tag(ModuleEnv.ExhaustivenessObligation.Source),
        allocator: Allocator,
        found: *std.ArrayList(checked_artifact.CheckedExhaustivenessSiteId),
    ) Allocator.Error!void {
        const index = try self.exhaustiveness_index.getOrCompute(self.io, Allocator.Error, ExhaustivenessIndexRequest{ .analyses = self, .bodies = bodies }, ExhaustivenessIndexRequest.compute);
        for (index.atNode(raw_node)) |entry| {
            if (entry.source == source) try found.append(allocator, entry.site);
        }
    }

    const ExhaustivenessIndexRequest = struct {
        analyses: *BodyAnalyses,
        bodies: CheckedBodies,

        fn compute(request: ExhaustivenessIndexRequest) Allocator.Error!*ExhaustivenessIndex {
            return ExhaustivenessIndex.compute(request.analyses.allocator, request.bodies.env);
        }
    };

    const SingleSourceRequest = struct {
        analyses: *BodyAnalyses,
        bodies: CheckedBodies,

        fn templates(request: SingleSourceRequest) Allocator.Error!*SingleSourceTemplates {
            return computeSingleSourceTemplates(request.analyses.allocator, request.bodies);
        }

        fn sites(request: SingleSourceRequest) Allocator.Error!*SingleSourceSites {
            return computeSingleSourceSites(request.analyses.allocator, request.bodies);
        }
    };
};

fn divergenceFlag(mode: InlineExpectMode) u8 {
    return switch (mode) {
        .run => diverges_run,
        .omit => diverges_omit,
    };
}

/// The nodes whose dispatch, `from_numeral` or `from_quote` plan has a
/// resolution `selected` accepts.
fn dispatchNodes(
    allocator: Allocator,
    bodies: CheckedBodies,
    comptime selected: fn (static_dispatch.CheckedCallResolution) bool,
) Allocator.Error!NodeBits {
    const plans = bodies.tables.plans orelse invariant("a body analysis ran without the module's dispatch plans");
    var bits = try std.DynamicBitSetUnmanaged.initEmpty(allocator, bodies.nodeCount());
    errdefer bits.deinit(allocator);
    for ([_][]const static_dispatch.PlanKV{ plans.by_node, plans.numeral_by_node, plans.quote_by_node }) |entries| {
        for (entries) |entry| {
            const dispatch = bodies.tables.dispatch orelse invariant("a body analysis read a dispatch plan without the module's dispatch resolution");
            if (selected((try dispatch.plan(@fromBackingInt(entry.val))).resolution)) bits.set(entry.key);
        }
    }
    return .{ .bits = bits };
}

fn computeUnitFacts(allocator: Allocator, bodies: CheckedBodies, unit_id: UnitId) Allocator.Error!*UnitFacts {
    const index = bodies.index();
    const nodes = index.unitNodeSlice(unit_id);
    const facts = try allocator.create(UnitFacts);
    errdefer allocator.destroy(facts);
    const flags = try allocator.alloc(u8, nodes.len);
    errdefer allocator.free(flags);
    @memset(flags, 0);

    {
        var scan = DivergenceScan{ .allocator = allocator, .bodies = bodies, .unit = unit_id, .states = flags };
        defer scan.deinit();
        for (nodes) |raw| switch (index.nodeKind(raw)) {
            .expr, .statement => _ = try scan.eval(raw),
            .pattern => {},
            .unreached => unreachable, // a unit's nodes are all reached
        };
        for (flags) |*flag| flag.* &= diverges_both;
    }

    {
        const memo = try allocator.alloc(InspectElision.Memo, nodes.len);
        defer allocator.free(memo);
        @memset(memo, .unknown);
        var elision = InspectElision{ .allocator = allocator, .bodies = bodies, .memo = memo };
        defer elision.deinit();
        for (nodes, flags) |raw, *flag| {
            if (index.nodeKind(raw) != .expr) continue;
            if (try elision.mayBeElided(@fromBackingInt(@intCast(raw)))) flag.* |= UnitFacts.inspect_elidable;
        }
    }

    var publisher = ReassignmentPublisher.init(allocator, bodies, unit_id, nodes);
    defer publisher.deinitScratch();
    errdefer publisher.deinitResults();
    try publisher.run();

    facts.* = .{
        .flags = flags,
        .state_merges = publisher.state_merges,
        .loop_mutations = publisher.loop_mutations,
        .binders = try publisher.binders.toOwnedSlice(allocator),
    };
    return facts;
}

/// The checked expression operands of the dispatch plan at `node`.
fn appendDispatchOperands(bodies: CheckedBodies, raw_node: u32, out: anytype) Allocator.Error!void {
    const plans = bodies.tables.plans orelse invariant("a body analysis ran without the module's dispatch plans");
    const plan_id = plans.lookupByNode(raw_node) orelse return;
    const plan = plans.plans[@backingInt(plan_id)];
    for (plan.argsSlice(plans)) |operand| switch (operand) {
        .checked_expr => |expr| try out.add(expr),
        .generated_interpolation_segments, .generated_numeral, .generated_quote, .literal_pattern_value, .literal_pattern_conversion => {},
    };
}

/// Whether node `raw`'s own static dispatch cannot run: a dispatch,
/// `from_numeral` or `from_quote` plan keyed by the node resolves to a
/// checked error or unreachable.
fn nodeDispatchCrashes(bodies: CheckedBodies, raw: u32) Allocator.Error!bool {
    const plans = bodies.tables.plans orelse invariant("a body analysis ran without the module's dispatch plans");
    for ([_]?static_dispatch.StaticDispatchPlanId{ plans.lookupByNode(raw), plans.numeralPlanForNode(raw), plans.quotePlanForNode(raw) }) |found| {
        const plan = found orelse continue;
        const dispatch = bodies.tables.dispatch orelse invariant("a body analysis read a dispatch plan without the module's dispatch resolution");
        if (dispatchResolutionCrashes((try dispatch.plan(plan)).resolution)) return true;
    }
    return false;
}

fn dispatchResolutionCrashes(resolution: static_dispatch.CheckedCallResolution) bool {
    return switch (resolution) {
        .checked_error, .@"unreachable" => true,
        .direct_pending, .direct_closed, .direct_parametric, .evidence_dependent, .structural => false,
    };
}

fn dispatchResolutionIsCheckedError(resolution: static_dispatch.CheckedCallResolution) bool {
    return resolution == .checked_error;
}

/// The kind of a normalized expression.
pub const ExprKind = std.meta.Tag(checked_bodies.ExprData);

/// How an expression of one kind can come to be marked divergent.
///
/// This is the one classification shared by the producer of divergence facts
/// (`DivergenceScan`) and every consumer that lowers an expression those
/// facts mark divergent. Each consumer's arm for kinds that are never marked
/// divergent checks this classification at compile time.
pub const ExprDivergence = struct {
    /// Marked divergent by evaluating it: the kind always terminates, or an
    /// operand it evaluates is marked divergent.
    evaluation: bool,
    /// Marked divergent when its static dispatch plan cannot run (a
    /// `checked_error` or `unreachable` resolution, for the plan or under one
    /// specialization's evidence).
    dispatch_plan: bool,

    /// Whether no expression of this kind is ever marked divergent.
    pub fn never(self: ExprDivergence) bool {
        return !self.evaluation and !self.dispatch_plan;
    }
};

/// The divergence classification of an expression kind.
pub fn exprDivergence(kind: ExprKind) ExprDivergence {
    return switch (kind) {
        .crash,
        .ellipsis,
        .break_,
        .return_,
        .runtime_error,
        .expect_err,
        .list,
        .tuple,
        .match_,
        .if_,
        .call,
        .record,
        .block,
        .tag,
        .nominal,
        .binop,
        .unary_minus,
        .dbg,
        .expect,
        .field_access,
        .structural_eq,
        .structural_hash,
        .tuple_access,
        .for_,
        .run_low_level,
        => .{ .evaluation = true, .dispatch_plan = false },
        .str,
        .dispatch_call,
        .method_eq,
        .type_dispatch_call,
        .interpolation,
        => .{ .evaluation = true, .dispatch_plan = true },
        .numeral,
        => .{ .evaluation = false, .dispatch_plan = true },
        .str_segment,
        .bytes_literal,
        .lookup_local,
        .lookup_external,
        .lookup_required,
        .empty_list,
        .empty_record,
        .zero_argument_tag,
        .closure,
        .lambda,
        .anno_only,
        .hosted_lambda,
        => .{ .evaluation = false, .dispatch_plan = false },
    };
}

/// Compile-time check for a consumer's never-divergent arm: fails the build,
/// naming the kind, when the shared classification says an expression of
/// `kind` can be marked divergent.
pub fn assertExprNeverDiverges(comptime kind: ExprKind) void {
    comptime if (!exprDivergence(kind).never()) {
        @compileError("expression kind ." ++ @tagName(kind) ++
            " can be marked divergent (exprDivergence) but this consumer lowers it as never divergent");
    };
}

/// Decides, in both inline-expect modes at once, whether expressions and
/// statements always diverge, memoizing each one's answer in a dense array:
/// one unit's nodes by their position in the unit, or every node of the
/// module by node. Evaluated on explicit stacks, so expression nesting never
/// becomes native call depth.
const DivergenceScan = struct {
    const active: u8 = 4;
    const done: u8 = 8;

    allocator: Allocator,
    bodies: CheckedBodies,
    /// The unit whose nodes `states` covers; null when it covers every node.
    unit: ?UnitId,
    /// Per node, one specialization's evidence: true where that evidence
    /// resolves the node's dispatch to a checked error or unreachable value.
    evidence_crashes: []const bool = &.{},
    /// Per covered node: its divergence bits, and whether it is active or
    /// done.
    states: []u8,
    frames: std.ArrayList(Frame) = .empty,
    /// The children of every active frame: each frame's `any` children,
    /// then its `all` children.
    children: std.ArrayList(u32) = .empty,

    /// A node diverges when any of its `any` children does, or when it has
    /// an `all` group and every child of that group does; `mask` keeps the
    /// modes in which that applies.
    const Frame = struct {
        raw: u32,
        start: u32,
        any_end: u32,
        end: u32,
        next: u32,
        all: AllGroup,
        mask: u8,
    };

    const AllGroup = enum {
        none,
        all,
        /// A group with a member that never diverges.
        never,
    };

    const Expansion = union(enum) {
        value: u8,
        group: struct { any_end: u32, all: AllGroup = .none, mask: u8 = diverges_both },
    };

    fn deinit(self: *DivergenceScan) void {
        self.frames.deinit(self.allocator);
        self.children.deinit(self.allocator);
    }

    fn slot(self: *const DivergenceScan, raw: u32) u32 {
        const unit = self.unit orelse return raw;
        const index = self.bodies.index();
        std.debug.assert(index.unitOf(raw) == unit);
        return index.position_in_unit[raw];
    }

    /// The divergence bits of an expression or statement.
    fn eval(self: *DivergenceScan, raw: u32) Allocator.Error!u8 {
        const root = self.slot(raw);
        if (self.states[root] & done == 0) try self.run(raw);
        return self.states[root] & diverges_both;
    }

    fn run(self: *DivergenceScan, root: u32) Allocator.Error!void {
        try self.enter(root);
        while (self.frames.items.len != 0) {
            const frame = &self.frames.items[self.frames.items.len - 1];
            if (frame.next < frame.end) {
                const child = self.children.items[frame.next];
                frame.next += 1;
                const state = self.states[self.slot(child)];
                if (state & done != 0) continue;
                if (state & active != 0) invariant("divergence of a body contains a cycle");
                try self.enter(child);
                continue;
            }
            const finished = self.frames.pop().?;
            var value: u8 = 0;
            for (self.children.items[finished.start..finished.any_end]) |child| value |= self.states[self.slot(child)];
            switch (finished.all) {
                .none, .never => {},
                .all => {
                    var all = diverges_both;
                    for (self.children.items[finished.any_end..finished.end]) |child| all &= self.states[self.slot(child)];
                    value |= all;
                },
            }
            self.children.shrinkRetainingCapacity(finished.start);
            self.states[self.slot(finished.raw)] = (value & finished.mask & diverges_both) | done;
        }
    }

    fn enter(self: *DivergenceScan, raw: u32) Allocator.Error!void {
        const start: u32 = @intCast(self.children.items.len);
        const expansion = switch (self.bodies.index().nodeKind(raw)) {
            .expr => try self.expandExpr(@fromBackingInt(@intCast(raw))),
            .statement => try self.expandStatement(@fromBackingInt(@intCast(raw))),
            .pattern, .unreached => invariant("divergence was requested for a node that is neither an expression nor a statement"),
        };
        switch (expansion) {
            .value => |value| {
                self.children.shrinkRetainingCapacity(start);
                self.states[self.slot(raw)] = value | done;
            },
            .group => |group| {
                self.states[self.slot(raw)] = active;
                try self.frames.append(self.allocator, .{
                    .raw = raw,
                    .start = start,
                    .any_end = group.any_end,
                    .end = @intCast(self.children.items.len),
                    .next = start,
                    .all = group.all,
                    .mask = group.mask,
                });
            },
        }
    }

    fn add(self: *DivergenceScan, id: ExprId) Allocator.Error!void {
        try self.children.append(self.allocator, @backingInt(id));
    }

    fn addAll(self: *DivergenceScan, ids: []const ExprId) Allocator.Error!void {
        for (ids) |id| try self.add(id);
    }

    fn any(self: *const DivergenceScan) Expansion {
        return .{ .group = .{ .any_end = @intCast(self.children.items.len) } };
    }

    fn anyOf(self: *DivergenceScan, ids: []const ExprId) Allocator.Error!Expansion {
        try self.addAll(ids);
        return self.any();
    }

    fn one(self: *DivergenceScan, id: ExprId) Allocator.Error!Expansion {
        try self.add(id);
        return self.any();
    }

    /// Diverges when the child does, in run mode only: an `expect` body runs
    /// only when inline expects do.
    fn expectBody(self: *DivergenceScan, id: ExprId) Allocator.Error!Expansion {
        try self.add(id);
        return .{ .group = .{ .any_end = @intCast(self.children.items.len), .mask = diverges_run } };
    }

    fn expandExpr(self: *DivergenceScan, id: ExprId) Allocator.Error!Expansion {
        const raw = @backingInt(id);
        if (raw < self.evidence_crashes.len and self.evidence_crashes[raw]) return .{ .value = diverges_both };
        if (try nodeDispatchCrashes(self.bodies, raw)) return .{ .value = diverges_both };
        const bodies = self.bodies;
        return switch (bodies.exprData(id)) {
            .crash,
            .ellipsis,
            .break_,
            .return_,
            .runtime_error,
            .expect_err,
            => .{ .value = diverges_both },
            .str, .list, .tuple => |elems| try self.anyOf(elems),
            .interpolation => |interpolation| blk: {
                try self.add(interpolation.first);
                break :blk try self.anyOf(interpolation.parts);
            },
            // Diverges when its condition does, or when it has branches, none
            // guarded, that all diverge.
            .match_ => |match| blk: {
                try self.add(match.cond);
                const any_end: u32 = @intCast(self.children.items.len);
                if (match.branches.len == 0) break :blk .{ .group = .{ .any_end = any_end } };
                for (match.branches) |branch_idx| {
                    const branch = bodies.matchBranch(branch_idx);
                    if (branch.guard != null) {
                        self.children.shrinkRetainingCapacity(any_end);
                        break :blk .{ .group = .{ .any_end = any_end, .all = .never } };
                    }
                    try self.add(branch.value);
                }
                break :blk .{ .group = .{ .any_end = any_end, .all = .all } };
            },
            // Diverges when its first condition does, or when every branch
            // body and the final else do.
            .if_ => |if_| blk: {
                if (if_.branches.len > 0) try self.add(bodies.ifBranch(if_.branches[0]).cond);
                const any_end: u32 = @intCast(self.children.items.len);
                for (if_.branches) |branch| try self.add(bodies.ifBranch(branch).body);
                try self.add(if_.final_else);
                break :blk .{ .group = .{ .any_end = any_end, .all = .all } };
            },
            .call => |call| blk: {
                try self.add(call.func);
                break :blk try self.anyOf(call.args);
            },
            .record => |record| blk: {
                if (record.ext) |ext| try self.add(ext);
                for (record.fields) |field| try self.add(bodies.recordFieldValue(field));
                break :blk self.any();
            },
            .block => |block| blk: {
                for (block.statements) |child| try self.children.append(self.allocator, @backingInt(child));
                break :blk try self.one(block.final_expr);
            },
            .tag => |tag| try self.anyOf(tag.args),
            .nominal => |nominal| try self.one(nominal.backing_expr),
            .closure, .lambda, .hosted_lambda => .{ .value = 0 },
            .binop => |binop| try self.anyOf(&.{ binop.lhs, binop.rhs }),
            .unary_minus, .dbg => |child| try self.one(child),
            .expect => |child| try self.expectBody(child),
            .field_access => |field| try self.one(field.receiver),
            .structural_eq => |eq| try self.anyOf(&.{ eq.lhs, eq.rhs }),
            .structural_hash => |hash| try self.anyOf(&.{ hash.value, hash.hasher }),
            .tuple_access => |access| try self.one(access.tuple),
            .for_ => |for_| try self.one(for_.expr),
            .run_low_level => |run_op| if (run_op.op == .crash) .{ .value = diverges_both } else try self.anyOf(run_op.args),
            .dispatch_call,
            .method_eq,
            .type_dispatch_call,
            => blk: {
                try appendDispatchOperands(bodies, raw, self);
                break :blk self.any();
            },
            .numeral,
            .str_segment,
            .bytes_literal,
            .lookup_local,
            .lookup_external,
            .lookup_required,
            .empty_list,
            .empty_record,
            .zero_argument_tag,
            .anno_only,
            => .{ .value = 0 },
        };
    }

    fn expandStatement(self: *DivergenceScan, id: StatementId) Allocator.Error!Expansion {
        return switch (self.bodies.statementData(id)) {
            .crash,
            .break_,
            .return_,
            .runtime_error,
            .infinite_loop,
            => .{ .value = diverges_both },
            .decl => |decl| try self.one(decl.expr),
            // Declaring a promoted procedure evaluates nothing here.
            .promoted_proc => .{ .value = 0 },
            .var_ => |var_| try self.one(var_.expr),
            .var_uninitialized => .{ .value = 0 },
            .reassign => |reassign| try self.one(reassign.expr),
            .dbg, .expr => |child| try self.one(child),
            .expect => |child| try self.expectBody(child),
            .for_ => |for_| try self.one(for_.expr),
            .while_, .breakable_loop => |loop| try self.one(loop.cond),
            .rejected_method_decl,
            .import_,
            .alias_decl,
            .where_alias_decl,
            .nominal_decl,
            .type_anno,
            .type_var_alias,
            => .{ .value = 0 },
        };
    }
};

/// Divergence of every reached expression and statement of a module, by
/// node, under one specialization's dispatch evidence.
pub const DispatchDivergence = struct {
    exprs: []bool,
    statements: []bool,
    exprs_without_inline_expects: []bool,
    statements_without_inline_expects: []bool,

    pub fn deinit(self: *DispatchDivergence, allocator: Allocator) void {
        allocator.free(self.exprs);
        allocator.free(self.statements);
        allocator.free(self.exprs_without_inline_expects);
        allocator.free(self.statements_without_inline_expects);
        self.* = undefined;
    }
};

/// Divergence of every reached node of the module under one specialization.
/// `evidence_crashes` is empty or one bool per node of the module; true
/// means the specialization's evidence resolves the dispatch at that node to
/// a checked error or unreachable value, for which lowering emits a
/// non-returning crash. Plan-level rejected resolutions are always included.
/// Each result column has one entry per node of the module.
pub fn dispatchDivergenceForEvidence(
    allocator: Allocator,
    bodies: CheckedBodies,
    evidence_crashes: []const bool,
) Allocator.Error!DispatchDivergence {
    const index = bodies.index();
    const node_count = index.node_kinds.len;
    if (evidence_crashes.len != 0 and evidence_crashes.len != node_count) invariant("a dispatch evidence crash column did not cover the module's nodes");
    var result: DispatchDivergence = undefined;
    result.exprs = try allocator.alloc(bool, node_count);
    errdefer allocator.free(result.exprs);
    result.statements = try allocator.alloc(bool, node_count);
    errdefer allocator.free(result.statements);
    result.exprs_without_inline_expects = try allocator.alloc(bool, node_count);
    errdefer allocator.free(result.exprs_without_inline_expects);
    result.statements_without_inline_expects = try allocator.alloc(bool, node_count);
    errdefer allocator.free(result.statements_without_inline_expects);
    inline for (.{ "exprs", "statements", "exprs_without_inline_expects", "statements_without_inline_expects" }) |name| @memset(@field(result, name), false);

    const states = try allocator.alloc(u8, node_count);
    defer allocator.free(states);
    @memset(states, 0);
    var scan = DivergenceScan{
        .allocator = allocator,
        .bodies = bodies,
        .unit = null,
        .evidence_crashes = evidence_crashes,
        .states = states,
    };
    defer scan.deinit();
    for (index.node_kinds, 0..) |kind, raw| switch (kind) {
        .expr => {
            const value = try scan.eval(@intCast(raw));
            result.exprs[raw] = value & diverges_run != 0;
            result.exprs_without_inline_expects[raw] = value & diverges_omit != 0;
        },
        .statement => {
            const value = try scan.eval(@intCast(raw));
            result.statements[raw] = value & diverges_run != 0;
            result.statements_without_inline_expects[raw] = value & diverges_omit != 0;
        },
        .pattern, .unreached => {},
    };
    return result;
}

/// Whether evaluating an expression can be elided when only inspected: a
/// lambda, closure, or lookup, or a single-field record literal's one
/// required field read whose value can be. A row-coerced expression reaches
/// its parent through its coercion, which is never elided.
const InspectElision = struct {
    const Memo = enum(u8) { unknown, kept, elided };

    allocator: Allocator,
    bodies: CheckedBodies,
    /// Per node of the unit, by position: its answer once known.
    memo: []Memo,
    chain: std.ArrayList(u32) = .empty,

    fn deinit(self: *InspectElision) void {
        self.chain.deinit(self.allocator);
    }

    fn mayBeElided(self: *InspectElision, expr: ExprId) Allocator.Error!bool {
        const bodies = self.bodies;
        const index = bodies.index();
        const unit = index.unitOf(@backingInt(expr));
        self.chain.clearRetainingCapacity();
        var current = expr;
        const result = while (true) {
            const raw = @backingInt(current);
            std.debug.assert(index.unitOf(raw) == unit);
            const position = index.position_in_unit[raw];
            switch (self.memo[position]) {
                .unknown => {},
                .kept => break false,
                .elided => break true,
            }
            try self.chain.append(self.allocator, position);
            if (!elisionCandidate(bodies.nodeTag(raw))) break false;
            if (bodies.hasRowCoercion(current)) break false;
            const data = bodies.exprData(current);
            switch (data) {
                .lambda, .closure, .lookup_local, .lookup_external, .lookup_required => break true,
                .field_access => {},
                .numeral, .str_segment, .str, .bytes_literal, .list, .empty_list, .tuple, .match_, .if_, .call, .record, .empty_record, .block, .tag, .nominal, .zero_argument_tag, .binop, .unary_minus, .dispatch_call, .interpolation, .structural_eq, .structural_hash, .method_eq, .type_dispatch_call, .tuple_access, .runtime_error, .crash, .dbg, .expect_err, .expect, .ellipsis, .anno_only, .break_, .return_, .for_, .hosted_lambda, .run_low_level => break false,
            }
            const access = data.field_access;
            if (bodies.hasRowCoercion(access.receiver)) break false;
            const receiver = bodies.exprData(access.receiver);
            if (receiver != .record) break false;
            const record = receiver.record;
            if (record.ext != null or record.fields.len != 1) break false;
            // Only a single-segment required access reads the literal's one
            // field directly; `.?` segments and longer chains evaluate more
            // than the field value, so they are never elided here.
            if (access.segments.len != 1) break false;
            const segment = try bodies.fieldAccessSegment(access.segments, 0);
            if (segment.mode != .required) break false;
            const field = bodies.recordField(record.fields[0]);
            if (field.label != segment.field_name) break false;
            current = field.value;
        };
        for (self.chain.items) |position| self.memo[position] = if (result) .elided else .kept;
        return result;
    }

    /// Whether an expression node of this tag can be elided: a lambda,
    /// closure, lookup, or field access.
    fn elisionCandidate(tag: CIR.Node.Tag) bool {
        return tag == .expr_lambda or tag == .expr_closure or tag == .expr_var or
            tag == .expr_external_lookup or tag == .expr_associated_lookup_resolved or
            tag == .expr_required_lookup or tag == .expr_field_access;
    }
};

/// Publishes loop mutations and state merges for one unit. A walk that
/// reaches a nested loop, control-flow expression, or expression statement
/// reuses that construct's published range instead of revisiting its parts,
/// so each node is walked only by its nearest enclosing plan owner.
const ReassignmentPublisher = struct {
    const no_position = std.math.maxInt(u32);

    allocator: Allocator,
    bodies: CheckedBodies,
    unit: UnitId,
    nodes: []const u32,
    state_merges: []?BinderSpan = &.{},
    loop_mutations: []?BinderSpan = &.{},
    binders: std.ArrayList(BinderId) = .empty,
    scratch: std.ArrayList(BinderId) = .empty,
    work: std.ArrayList(Work) = .empty,
    /// Per node of the unit, by position: for a binder being deduplicated,
    /// its position in `scratch`.
    scratch_positions: []u32 = &.{},

    const PlanRef = union(enum) {
        /// A loop's mutations: its condition and body. The node is a `for`
        /// expression or a loop statement.
        loop: u32,
        /// An `if`, `match`, or `for` expression's state merge.
        state: ExprId,
        /// An expression statement's state merge.
        expr_statement: StatementId,
    };

    const LoopParts = struct {
        /// A condition loop's condition runs every iteration, so it is part
        /// of the loop's plan.
        cond: ?ExprId,
        body: ExprId,
        /// A `for` iterable evaluates once before the loop, so it belongs to
        /// the enclosing context rather than to the loop's plan.
        iterable: ?ExprId,
    };

    /// Children are pushed in reverse so they are collected in source order.
    const Work = union(enum) {
        expr: ExprId,
        statement: StatementId,
        /// A plan owner reached from an enclosing walk: its binders join the
        /// walk, publishing its plan first when it has none yet.
        nested: PlanRef,
        /// Deduplicate and publish a plan whose binders were collected above
        /// `scratch_start`; a nested plan then contributes to its encloser.
        finish: struct { plan: PlanRef, scratch_start: usize, nested: bool },
        /// An expression statement whose expression is control flow shares
        /// that expression's plan once it is published.
        share_state: struct { statement: StatementId, expr: ExprId },
        append_binders: BinderSpan,
    };

    fn init(allocator: Allocator, bodies: CheckedBodies, unit: UnitId, nodes: []const u32) ReassignmentPublisher {
        return .{
            .allocator = allocator,
            .bodies = bodies,
            .unit = unit,
            .nodes = nodes,
        };
    }

    fn deinitScratch(self: *ReassignmentPublisher) void {
        self.scratch.deinit(self.allocator);
        self.work.deinit(self.allocator);
        self.allocator.free(self.scratch_positions);
    }

    fn deinitResults(self: *ReassignmentPublisher) void {
        self.allocator.free(self.state_merges);
        self.allocator.free(self.loop_mutations);
        self.binders.deinit(self.allocator);
    }

    fn run(self: *ReassignmentPublisher) Allocator.Error!void {
        self.state_merges = try self.allocator.alloc(?BinderSpan, self.nodes.len);
        @memset(self.state_merges, null);
        self.loop_mutations = try self.allocator.alloc(?BinderSpan, self.nodes.len);
        @memset(self.loop_mutations, null);
        self.scratch_positions = try self.allocator.alloc(u32, self.nodes.len);
        @memset(self.scratch_positions, no_position);
        const index = self.bodies.index();
        for (self.nodes) |raw| switch (index.nodeKind(raw)) {
            .expr => {
                const id: ExprId = @fromBackingInt(@intCast(raw));
                switch (self.planOwnerKind(id)) {
                    .for_loop => {
                        try self.publishPlan(.{ .loop = raw });
                        try self.publishPlan(.{ .state = id });
                    },
                    .branching => try self.publishPlan(.{ .state = id }),
                    .none => {},
                }
            },
            .statement => {
                const id: StatementId = @fromBackingInt(@intCast(raw));
                switch (self.bodies.statementData(id)) {
                    .for_, .while_, .infinite_loop, .breakable_loop => try self.publishPlan(.{ .loop = raw }),
                    .expr => try self.publishPlan(.{ .expr_statement = id }),
                    .decl, .promoted_proc, .var_, .var_uninitialized, .reassign, .crash, .dbg, .expect, .break_, .return_, .rejected_method_decl, .import_, .alias_decl, .where_alias_decl, .nominal_decl, .type_anno, .type_var_alias, .runtime_error => {},
                }
            },
            .pattern => {},
            .unreached => unreachable, // a unit's nodes are all reached
        };
    }

    const PlanOwnerKind = enum { none, branching, for_loop };

    /// Which plans an expression owns, from its node tag: reference
    /// resolution retires only lookups and calls, so a retired expression
    /// owns none either way.
    fn planOwnerKind(self: *const ReassignmentPublisher, id: ExprId) PlanOwnerKind {
        const tag = self.bodies.nodeTag(@backingInt(id));
        if (tag == .expr_for) return .for_loop;
        if (tag == .expr_if_then_else or tag == .expr_match) return .branching;
        return .none;
    }

    fn position(self: *const ReassignmentPublisher, raw: u32) u32 {
        const index = self.bodies.index();
        std.debug.assert(index.unitOf(raw) == self.unit);
        return index.position_in_unit[raw];
    }

    fn loopParts(self: *ReassignmentPublisher, raw: u32) LoopParts {
        const index = self.bodies.index();
        return switch (index.nodeKind(raw)) {
            .expr => blk: {
                const for_ = self.bodies.exprData(@fromBackingInt(@intCast(raw))).for_;
                break :blk .{ .cond = null, .body = for_.body, .iterable = for_.expr };
            },
            .statement => switch (self.bodies.statementData(@fromBackingInt(@intCast(raw)))) {
                .for_ => |for_| .{ .cond = null, .body = for_.body, .iterable = for_.expr },
                .while_, .infinite_loop, .breakable_loop => |loop| .{ .cond = loop.cond, .body = loop.body, .iterable = null },
                .decl, .promoted_proc, .var_, .var_uninitialized, .reassign, .crash, .dbg, .expr, .expect, .break_, .return_, .rejected_method_decl, .import_, .alias_decl, .where_alias_decl, .nominal_decl, .type_anno, .type_var_alias, .runtime_error => invariant("loop mutations referenced a statement that is not a loop"),
            },
            .pattern, .unreached => invariant("loop mutations referenced a node that is not a loop"),
        };
    }

    fn statementExpr(self: *ReassignmentPublisher, id: StatementId) ExprId {
        return switch (self.bodies.statementData(id)) {
            .expr => |expr| expr,
            .decl, .promoted_proc, .var_, .var_uninitialized, .reassign, .crash, .dbg, .expect, .for_, .while_, .infinite_loop, .breakable_loop, .break_, .return_, .rejected_method_decl, .import_, .alias_decl, .where_alias_decl, .nominal_decl, .type_anno, .type_var_alias, .runtime_error => invariant("a state merge referenced a statement that is not an expression statement"),
        };
    }

    fn ownsStateMerge(self: *const ReassignmentPublisher, id: ExprId) bool {
        return switch (self.planOwnerKind(id)) {
            .branching, .for_loop => true,
            .none => false,
        };
    }

    /// The published range of a plan owner, if it has one yet.
    fn publishedRange(self: *const ReassignmentPublisher, plan: PlanRef) ?BinderSpan {
        return switch (plan) {
            .loop => |raw| self.loop_mutations[self.position(raw)],
            .state => |expr| self.state_merges[self.position(@backingInt(expr))],
            .expr_statement => |statement| self.state_merges[self.position(@backingInt(statement))],
        };
    }

    fn push(self: *ReassignmentPublisher, work: Work) Allocator.Error!void {
        try self.work.append(self.allocator, work);
    }

    fn publishPlan(self: *ReassignmentPublisher, plan: PlanRef) Allocator.Error!void {
        if (self.publishedRange(plan) != null) return;
        if (plan == .expr_statement) {
            const expr = self.statementExpr(plan.expr_statement);
            if (self.ownsStateMerge(expr)) {
                try self.push(.{ .share_state = .{ .statement = plan.expr_statement, .expr = expr } });
                if (self.publishedRange(.{ .state = expr }) == null) try self.begin(.{ .state = expr }, false);
                return self.drain();
            }
        }
        try self.begin(plan, false);
        try self.drain();
    }

    fn drain(self: *ReassignmentPublisher) Allocator.Error!void {
        while (self.work.pop()) |work| switch (work) {
            .expr => |id| try self.visitExpr(id),
            .statement => |id| try self.visitStatement(id),
            .nested => |plan| try self.visitNested(plan),
            .finish => |item| {
                const range = try self.finish(item.plan, item.scratch_start);
                if (item.nested) try self.pushContribution(item.plan, range);
            },
            .share_state => |item| {
                self.state_merges[self.position(@backingInt(item.statement))] = self.state_merges[self.position(@backingInt(item.expr))].?;
            },
            .append_binders => |range| {
                try self.scratch.appendSlice(self.allocator, self.binders.items[range.start..][0..range.len]);
            },
        };
    }

    fn visitNested(self: *ReassignmentPublisher, plan: PlanRef) Allocator.Error!void {
        if (self.publishedRange(plan)) |range| return self.pushContribution(plan, range);
        if (plan == .expr_statement) {
            const expr = self.statementExpr(plan.expr_statement);
            if (self.ownsStateMerge(expr)) {
                try self.push(.{ .share_state = .{ .statement = plan.expr_statement, .expr = expr } });
                return self.visitNested(.{ .state = expr });
            }
        }
        try self.begin(plan, true);
    }

    /// What a plan owner adds to an enclosing walk: a `for` loop's iterable
    /// evaluates in the enclosing context, then its published binders.
    fn pushContribution(self: *ReassignmentPublisher, plan: PlanRef, range: BinderSpan) Allocator.Error!void {
        try self.push(.{ .append_binders = range });
        switch (plan) {
            .loop => |raw| if (self.loopParts(raw).iterable) |iterable| try self.push(.{ .expr = iterable }),
            .state, .expr_statement => {},
        }
    }

    /// Collect an unpublished plan owner's parts into a fresh scratch region,
    /// then finish it.
    fn begin(self: *ReassignmentPublisher, plan: PlanRef, nested: bool) Allocator.Error!void {
        try self.push(.{ .finish = .{ .plan = plan, .scratch_start = self.scratch.items.len, .nested = nested } });
        switch (plan) {
            .loop => |raw| {
                const parts = self.loopParts(raw);
                try self.push(.{ .expr = parts.body });
                if (parts.cond) |cond| try self.push(.{ .expr = cond });
            },
            .state => |expr| try self.pushExprParts(expr),
            .expr_statement => |statement| try self.push(.{ .expr = self.statementExpr(statement) }),
        }
    }

    fn finish(self: *ReassignmentPublisher, plan: PlanRef, start: usize) Allocator.Error!BinderSpan {
        defer self.scratch.shrinkRetainingCapacity(start);

        // Nested plans publish before this deduplication begins, so one
        // position column serves every plan without per-plan clearing.
        var end = start;
        const count = self.scratch.items.len;
        for (start..count) |i| {
            const binder = self.scratch.items[i];
            const slot = &self.scratch_positions[self.position(@backingInt(binder))];
            if (slot.* == no_position) {
                slot.* = @intCast(end);
                self.scratch.items[end] = binder;
                end += 1;
            }
        }
        for (self.scratch.items[start..end]) |binder| self.scratch_positions[self.position(@backingInt(binder))] = no_position;
        const binders_start: u32 = @intCast(self.binders.items.len);
        try self.binders.appendSlice(self.allocator, self.scratch.items[start..end]);
        const range: BinderSpan = .{ .start = binders_start, .len = @intCast(end - start) };
        switch (plan) {
            .loop => |raw| self.loop_mutations[self.position(raw)] = range,
            .state => |expr| self.state_merges[self.position(@backingInt(expr))] = range,
            .expr_statement => |statement| self.state_merges[self.position(@backingInt(statement))] = range,
        }
        return range;
    }

    fn beginChildren(self: *const ReassignmentPublisher) usize {
        return self.work.items.len;
    }

    fn endChildren(self: *ReassignmentPublisher, start: usize) void {
        std.mem.reverse(Work, self.work.items[start..]);
    }

    fn pushExpr(self: *ReassignmentPublisher, id: ExprId) Allocator.Error!void {
        try self.push(.{ .expr = id });
    }

    fn add(self: *ReassignmentPublisher, id: ExprId) Allocator.Error!void {
        try self.pushExpr(id);
    }

    /// An expression reached by a walk: a control-flow expression contributes
    /// its own plan, and any other expression contributes its parts.
    fn visitExpr(self: *ReassignmentPublisher, id: ExprId) Allocator.Error!void {
        if (self.ownsStateMerge(id)) return self.push(.{ .nested = .{ .state = id } });
        try self.pushExprParts(id);
    }

    fn pushExprParts(self: *ReassignmentPublisher, id: ExprId) Allocator.Error!void {
        const bodies = self.bodies;
        const children = self.beginChildren();
        defer self.endChildren(children);
        switch (bodies.exprData(id)) {
            .str, .list, .tuple => |items| for (items) |item| try self.pushExpr(item),
            .interpolation => |interpolation| {
                try self.pushExpr(interpolation.first);
                for (interpolation.parts) |part| try self.pushExpr(part);
            },
            .match_ => |match| {
                try self.pushExpr(match.cond);
                for (match.branches) |branch_idx| {
                    const branch = bodies.matchBranch(branch_idx);
                    if (branch.guard) |guard| try self.pushExpr(guard);
                    try self.pushExpr(branch.value);
                }
            },
            .if_ => |if_| {
                for (if_.branches) |branch_idx| {
                    const branch = bodies.ifBranch(branch_idx);
                    try self.pushExpr(branch.cond);
                    try self.pushExpr(branch.body);
                }
                try self.pushExpr(if_.final_else);
            },
            .call => |call| {
                try self.pushExpr(call.func);
                for (call.args) |arg| try self.pushExpr(arg);
            },
            .record => |record| {
                if (record.ext) |ext| try self.pushExpr(ext);
                for (record.fields) |field| try self.pushExpr(bodies.recordFieldValue(field));
            },
            .block => |block| {
                for (block.statements) |statement| try self.push(.{ .statement = statement });
                try self.pushExpr(block.final_expr);
            },
            .tag => |tag| for (tag.args) |arg| try self.pushExpr(arg),
            .nominal => |nominal| try self.pushExpr(nominal.backing_expr),
            .binop => |binop| {
                try self.pushExpr(binop.lhs);
                try self.pushExpr(binop.rhs);
            },
            .unary_minus, .dbg => |child| try self.pushExpr(child),
            // An expect body can only reassign vars declared inside it
            // (canonicalization rejects the rest), so it carries no state
            // through an enclosing construct.
            .expect => {},
            .expect_err => |child| try self.pushExpr(child.expr),
            .field_access => |field| try self.pushExpr(field.receiver),
            .structural_eq => |eq| {
                try self.pushExpr(eq.lhs);
                try self.pushExpr(eq.rhs);
            },
            .structural_hash => |hash| {
                try self.pushExpr(hash.value);
                try self.pushExpr(hash.hasher);
            },
            .tuple_access => |access| try self.pushExpr(access.tuple),
            .return_ => |ret| try self.pushExpr(ret.expr),
            // A `for` expression's iterable, then its loop's mutations.
            .for_ => try self.push(.{ .nested = .{ .loop = @backingInt(id) } }),
            .run_low_level => |low| for (low.args) |arg| try self.pushExpr(arg),
            // A lambda's body executes at invocation, not at this expression.
            .lambda,
            .closure,
            .hosted_lambda,
            .numeral,
            .str_segment,
            .bytes_literal,
            .lookup_local,
            .lookup_external,
            .lookup_required,
            .empty_list,
            .empty_record,
            .zero_argument_tag,
            .runtime_error,
            .crash,
            .ellipsis,
            .anno_only,
            .break_,
            => {},
            .dispatch_call, .method_eq, .type_dispatch_call => try appendDispatchOperands(bodies, @backingInt(id), self),
        }
    }

    fn visitStatement(self: *ReassignmentPublisher, id: StatementId) Allocator.Error!void {
        switch (self.bodies.statementData(id)) {
            .decl => |decl| try self.pushExpr(decl.expr),
            .var_ => |decl| try self.pushExpr(decl.expr),
            .reassign => |reassign| {
                try self.bodies.appendReassignedBinders(self.allocator, reassign.pattern, &self.scratch);
                try self.pushExpr(reassign.expr);
            },
            .dbg => |child| try self.pushExpr(child),
            .expr => try self.push(.{ .nested = .{ .expr_statement = id } }),
            .expect => {},
            .for_, .while_, .infinite_loop, .breakable_loop => try self.push(.{ .nested = .{ .loop = @backingInt(id) } }),
            .return_ => |ret| try self.pushExpr(ret.expr),
            .promoted_proc, .var_uninitialized, .crash, .break_, .rejected_method_decl, .import_, .alias_decl, .where_alias_decl, .nominal_decl, .type_anno, .type_var_alias, .runtime_error => {},
        }
    }
};

/// Which diagnostic-error facts to compute.
pub const DiagnosticMode = enum {
    /// Errors checking recorded in the bodies, propagated through their
    /// children: the facts publication reads before total dispatch
    /// resolution.
    ordinary,
    /// The ordinary facts plus every rejected total dispatch resolution, and,
    /// when there is one, propagation through resolved local constant
    /// references.
    final,
};

/// Whether each reached expression contains a diagnostic error: its own
/// (`ModuleEnv.nodeIsErroneous`, or an expression lowering evaluates as a
/// runtime error), or one of a node it evaluates or binds.
pub fn diagnosticErrors(allocator: Allocator, bodies: CheckedBodies, mode: DiagnosticMode) Allocator.Error!NodeBits {
    const index = bodies.index();
    const node_count = index.node_kinds.len;
    var errors = try std.DynamicBitSetUnmanaged.initEmpty(allocator, node_count);
    errdefer errors.deinit(allocator);
    const states = try allocator.alloc(DiagnosticState, node_count);
    defer allocator.free(states);
    var erroneous = try std.DynamicBitSetUnmanaged.initEmpty(allocator, node_count);
    defer erroneous.deinit(allocator);
    for (bodies.env.erroneous_nodes.items.items) |entry| erroneous.set(entry.node_idx);
    var scan = DiagnosticScan{
        .allocator = allocator,
        .bodies = bodies,
        .errors = errors,
        .erroneous = &erroneous,
        .states = states,
    };
    defer scan.frames.deinit(allocator);
    defer scan.items.deinit(allocator);
    @memset(scan.states, .fresh);

    var rejected_dispatch: NodeBits = .{};
    defer rejected_dispatch.deinit(allocator);
    switch (mode) {
        .ordinary => {},
        .final => {
            const plans = bodies.tables.plans orelse invariant("diagnostic errors were computed without the module's dispatch plans");
            const dispatch = bodies.tables.dispatch orelse invariant("diagnostic errors were computed without the module's dispatch resolution");
            for (0..plans.plans.len) |raw| {
                if ((try dispatch.plan(@fromBackingInt(@intCast(raw)))).resolution == .checked_error) {
                    rejected_dispatch = try dispatchNodes(allocator, bodies, dispatchResolutionIsCheckedError);
                    scan.rejected_dispatch = &rejected_dispatch;
                    break;
                }
            }
        },
    }
    var graph: ?ConstantGraph = if (scan.rejected_dispatch != null) try ConstantGraph.init(allocator, bodies) else null;
    defer if (graph) |*value| value.deinit();
    scan.graph = if (graph) |*value| value else null;

    for (index.node_kinds, 0..) |kind, raw| {
        if (kind == .unreached) continue;
        if (try scan.node(@intCast(raw))) scan.errors.set(raw);
    }
    if (scan.graph) |value| try value.propagate(&scan.errors);
    errors = scan.errors;
    return .{ .bits = errors };
}

const DiagnosticState = enum(u8) { fresh, active, done };

/// Recovery-only edges recorded by the scan: each entered node's enclosing
/// node, and each constant lookup's initializer. Constant references can
/// close cycles through delayed function bodies; a monotone worklist
/// propagates errors across those cycles.
const ConstantGraph = struct {
    const no_edge = std.math.maxInt(usize);
    const Edge = struct { parent: u32, next: usize };
    allocator: Allocator,
    heads: collections.DenseMap(u32, usize),
    edges: std.ArrayList(Edge) = .empty,
    /// The initializer expression a local constant lookup reads.
    lookup_values: collections.DenseMap(u32, ExprId),
    current: ?u32 = null,

    fn init(allocator: Allocator, bodies: CheckedBodies) Allocator.Error!ConstantGraph {
        var graph = ConstantGraph{
            .allocator = allocator,
            .heads = collections.DenseMap(u32, usize).init(allocator),
            .lookup_values = collections.DenseMap(u32, ExprId).init(allocator),
        };
        errdefer graph.deinit();
        const refs = bodies.tables.refs orelse invariant("diagnostic errors were computed without the module's value references");
        const roots = bodies.tables.roots orelse invariant("diagnostic errors were computed without the module's compile-time roots");
        const const_templates = bodies.tables.const_templates orelse invariant("diagnostic errors were computed without the module's constant templates");
        const module = bodies.tables.module orelse invariant("diagnostic errors were computed without the module's identity");
        var by_pattern = collections.DenseMap(u32, ExprId).init(allocator);
        defer by_pattern.deinit();
        for (roots.roots) |root| {
            if (root.pattern) |pattern| try by_pattern.put(@backingInt(pattern), root.rootExpr());
        }
        for (refs.records) |record| {
            const ref = switch (record.ref) {
                .top_level_const => |use| use.const_ref,
                .selected_hoisted_const => |use| use.const_use.const_ref,
                .local_param,
                .local_value,
                .local_mutable_version,
                .pattern_binder,
                .local_proc,
                .imported_const,
                .top_level_proc,
                .imported_proc,
                .hosted_proc,
                .platform_required_declaration,
                .platform_required_checked_error,
                .platform_required_const,
                .platform_required_proc,
                .promoted_top_level_proc,
                => continue,
            };
            if (!ref.artifact.eql(module)) continue;
            if (const_templates.get(ref).state == .unimplemented) continue;
            try graph.lookup_values.put(@backingInt(record.expr), switch (ref.owner) {
                .top_level_binding => |owner| by_pattern.get(@backingInt(owner.pattern)) orelse
                    invariant("a constant dependency had no initializer root"),
                .hoisted_root => |owner| roots.root(owner.root).bodyParts().expr orelse continue,
            });
        }
        return graph;
    }

    fn deinit(self: *ConstantGraph) void {
        self.heads.deinit();
        self.edges.deinit(self.allocator);
        self.lookup_values.deinit();
    }

    fn enter(self: *ConstantGraph, raw: u32) Allocator.Error!?u32 {
        const previous = self.current;
        if (previous) |parent| {
            const head = try self.heads.getOrPut(raw);
            const next = if (head.found_existing) head.value_ptr.* else no_edge;
            try self.edges.append(self.allocator, .{ .parent = parent, .next = next });
            head.value_ptr.* = self.edges.items.len - 1;
        }
        self.current = raw;
        return previous;
    }

    fn propagate(self: *ConstantGraph, errors: *std.DynamicBitSetUnmanaged) Allocator.Error!void {
        var work = std.ArrayList(u32).empty;
        defer work.deinit(self.allocator);
        var heads = self.heads.constIterator();
        while (heads.next()) |entry| {
            if (errors.isSet(entry.key_ptr.*)) try work.append(self.allocator, entry.key_ptr.*);
        }
        while (work.pop()) |raw| {
            var edge_index = self.heads.get(raw) orelse continue;
            while (edge_index != no_edge) {
                const edge = self.edges.items[edge_index];
                edge_index = edge.next;
                if (errors.isSet(edge.parent)) continue;
                errors.set(edge.parent);
                try work.append(self.allocator, edge.parent);
            }
        }
    }
};

/// A memoized depth-first search over expressions, patterns, and statements
/// on explicit frame and item stacks, so body nesting depth never becomes
/// native call depth. A node stops at its first item that contains an error.
const DiagnosticScan = struct {
    allocator: Allocator,
    bodies: CheckedBodies,
    errors: std.DynamicBitSetUnmanaged,
    /// The nodes checking recorded as erroneous in their own right.
    erroneous: *const std.DynamicBitSetUnmanaged,
    states: []DiagnosticState,
    /// The nodes whose dispatch plan's total resolution rejected, when
    /// rejected total dispatch resolutions seed errors, which also makes
    /// errors follow constant references (`graph`).
    rejected_dispatch: ?*const NodeBits = null,
    graph: ?*ConstantGraph = null,
    frames: std.ArrayList(Frame) = .empty,
    items: std.ArrayList(Item) = .empty,

    const Item = union(enum) {
        node: u32,
        diagnostic_error,
    };

    const Frame = struct {
        node: u32,
        items_start: usize,
        next: usize,
        previous: ?u32,
    };

    fn node(self: *DiagnosticScan, raw: u32) Allocator.Error!bool {
        std.debug.assert(self.frames.items.len == 0);
        if (try self.begin(raw)) |known| return known;
        while (true) {
            const frame = &self.frames.items[self.frames.items.len - 1];
            var result = false;
            if (frame.next < self.items.items.len) {
                const item = self.items.items[frame.next];
                frame.next += 1;
                const item_contains_error = switch (item) {
                    .node => |child| (try self.begin(child)) orelse continue,
                    .diagnostic_error => true,
                };
                if (!item_contains_error) continue;
                result = true;
            }
            // Finish this node; an error also finishes every enclosing node
            // that was waiting on it.
            while (true) {
                const finished = self.frames.pop().?;
                self.finish(finished, result);
                if (self.frames.items.len == 0) return result;
                if (!result) break;
            }
        }
    }

    /// Enter a node. Returns its known result when it needs no frame, or null
    /// after pushing a frame for it.
    fn begin(self: *DiagnosticScan, raw: u32) Allocator.Error!?bool {
        const previous = if (self.graph) |graph| try graph.enter(raw) else null;
        switch (self.states[raw]) {
            .done => {
                if (self.graph) |graph| graph.current = previous;
                return self.errors.isSet(raw);
            },
            .active => {
                if (self.graph) |graph| {
                    graph.current = previous;
                    return false;
                }
                invariant("the diagnostic-error relation of a body contains a cycle");
            },
            .fresh => {},
        }
        self.states[raw] = .active;
        const items_start = self.items.items.len;
        try self.pushItems(raw);
        try self.frames.append(self.allocator, .{ .node = raw, .items_start = items_start, .next = items_start, .previous = previous });
        return null;
    }

    fn finish(self: *DiagnosticScan, frame: Frame, result: bool) void {
        if (result) self.errors.set(frame.node);
        self.states[frame.node] = .done;
        self.items.shrinkRetainingCapacity(frame.items_start);
        if (self.graph) |graph| graph.current = frame.previous;
    }

    fn pushItem(self: *DiagnosticScan, item: Item) Allocator.Error!void {
        try self.items.append(self.allocator, item);
    }

    fn pushNode(self: *DiagnosticScan, raw: u32) Allocator.Error!void {
        try self.pushItem(.{ .node = raw });
    }

    fn pushExpr(self: *DiagnosticScan, id: ExprId) Allocator.Error!void {
        try self.pushNode(@backingInt(id));
    }

    fn add(self: *DiagnosticScan, id: ExprId) Allocator.Error!void {
        try self.pushExpr(id);
    }

    fn pushExprs(self: *DiagnosticScan, ids: []const ExprId) Allocator.Error!void {
        for (ids) |id| try self.pushExpr(id);
    }

    fn pushPattern(self: *DiagnosticScan, id: PatternId) Allocator.Error!void {
        try self.pushNode(@backingInt(id));
    }

    fn pushPatterns(self: *DiagnosticScan, ids: []const PatternId) Allocator.Error!void {
        for (ids) |id| try self.pushPattern(id);
    }

    fn pushItems(self: *DiagnosticScan, raw: u32) Allocator.Error!void {
        const bodies = self.bodies;
        if (self.erroneous.isSet(raw)) return self.pushItem(.diagnostic_error);
        if (self.rejected_dispatch) |rejected| if (rejected.isSet(raw)) return self.pushItem(.diagnostic_error);
        switch (bodies.index().nodeKind(raw)) {
            .expr => try self.pushExprItems(@fromBackingInt(@intCast(raw))),
            .pattern => try self.pushPatternItems(@fromBackingInt(@intCast(raw))),
            .statement => try self.pushStatementItems(@fromBackingInt(@intCast(raw))),
            .unreached => invariant("the diagnostic-error scan reached a node the bodies do not reach"),
        }
    }

    fn pushExprItems(self: *DiagnosticScan, id: ExprId) Allocator.Error!void {
        const bodies = self.bodies;
        switch (bodies.exprData(id)) {
            .runtime_error => try self.pushItem(.diagnostic_error),
            .str, .list, .tuple => |items| try self.pushExprs(items),
            .match_ => |match| {
                try self.pushExpr(match.cond);
                for (match.branches) |branch_idx| {
                    const branch = bodies.matchBranch(branch_idx);
                    for (branch.patterns) |branch_pattern| try self.pushPattern(bodies.matchBranchPattern(branch_pattern).pattern);
                    if (branch.guard) |guard| try self.pushExpr(guard);
                    try self.pushExpr(branch.value);
                }
            },
            .if_ => |if_| {
                for (if_.branches) |branch_idx| {
                    const branch = bodies.ifBranch(branch_idx);
                    try self.pushExpr(branch.cond);
                    try self.pushExpr(branch.body);
                }
                try self.pushExpr(if_.final_else);
            },
            .call => |call| {
                try self.pushExpr(call.func);
                try self.pushExprs(call.args);
            },
            .record => |record| {
                if (record.ext) |ext| try self.pushExpr(ext);
                for (record.fields) |field| try self.pushExpr(bodies.recordFieldValue(field));
            },
            .block => |block| {
                for (block.statements) |statement| try self.pushNode(@backingInt(statement));
                try self.pushExpr(block.final_expr);
            },
            .tag => |tag| try self.pushExprs(tag.args),
            .nominal => |nominal| try self.pushExpr(nominal.backing_expr),
            .closure => |closure| {
                try self.pushExpr(closure.lambda);
                for (closure.captures) |capture| try self.pushPattern(bodies.capture(capture).pattern);
            },
            .lambda => |lambda| {
                try self.pushPatterns(lambda.args);
                try self.pushExpr(lambda.body);
            },
            .binop => |binop| {
                try self.pushExpr(binop.lhs);
                try self.pushExpr(binop.rhs);
            },
            .unary_minus, .dbg, .expect => |child| try self.pushExpr(child),
            .field_access => |field| try self.pushExpr(field.receiver),
            .dispatch_call, .method_eq, .type_dispatch_call => try appendDispatchOperands(bodies, @backingInt(id), self),
            .interpolation => |interpolation| {
                try self.pushExpr(interpolation.first);
                try self.pushExprs(interpolation.parts);
            },
            .structural_eq => |eq| {
                try self.pushExpr(eq.lhs);
                try self.pushExpr(eq.rhs);
            },
            .structural_hash => |hash| {
                try self.pushExpr(hash.value);
                try self.pushExpr(hash.hasher);
            },
            .tuple_access => |access| try self.pushExpr(access.tuple),
            .expect_err => |expect_err| try self.pushExpr(expect_err.expr),
            .return_ => |ret| try self.pushExpr(ret.expr),
            .for_ => |for_| {
                try self.pushPattern(for_.pattern);
                try self.pushExpr(for_.expr);
                try self.pushExpr(for_.body);
            },
            .hosted_lambda => |hosted| try self.pushPatterns(hosted.args),
            .run_low_level => |run| try self.pushExprs(run.args),
            .numeral,
            .str_segment,
            .bytes_literal,
            .lookup_local,
            .lookup_external,
            .lookup_required,
            .empty_list,
            .empty_record,
            .zero_argument_tag,
            .crash,
            .ellipsis,
            .anno_only,
            .break_,
            => {},
        }
        if (self.graph) |graph| {
            if (graph.lookup_values.get(@backingInt(id))) |value| try self.pushExpr(value);
        }
    }

    fn pushPatternItems(self: *DiagnosticScan, id: PatternId) Allocator.Error!void {
        const bodies = self.bodies;
        switch (bodies.patternData(id)) {
            .runtime_error => try self.pushItem(.diagnostic_error),
            .as => |as| try self.pushPattern(as.pattern),
            .applied_tag => |tag| try self.pushPatterns(tag.args),
            .nominal => |nominal| try self.pushPattern(nominal.backing_pattern),
            .record_destructure => |destructs| for (destructs) |destruct| try self.pushPattern(bodies.recordDestructPattern(destruct)),
            .list => |list| {
                try self.pushPatterns(list.patterns);
                if (list.rest) |rest| if (rest.pattern) |rest_pattern| try self.pushPattern(rest_pattern);
            },
            .tuple => |items| try self.pushPatterns(items),
            .str_interpolation => |str| {
                var offset: u32 = 0;
                while (offset < str.steps.span.len) : (offset += 1) {
                    if (bodies.strPatternStep(str.steps, offset).capture) |capture| try self.pushPattern(capture);
                }
            },
            .assign, .underscore, .numeral_literal, .str_literal => {},
        }
    }

    fn pushStatementItems(self: *DiagnosticScan, id: StatementId) Allocator.Error!void {
        switch (self.bodies.statementData(id)) {
            .runtime_error => try self.pushItem(.diagnostic_error),
            .decl => |decl| {
                try self.pushPattern(decl.pattern);
                try self.pushExpr(decl.expr);
            },
            // The promoted procedure's body is its own template.
            .promoted_proc => {},
            .var_ => |var_| {
                try self.pushPattern(var_.pattern);
                try self.pushExpr(var_.expr);
            },
            .var_uninitialized => |var_| try self.pushPattern(var_.pattern),
            .reassign => |reassign| {
                try self.pushPattern(reassign.pattern);
                try self.pushExpr(reassign.expr);
            },
            .dbg, .expr, .expect => |child| try self.pushExpr(child),
            .for_ => |for_| {
                try self.pushPattern(for_.pattern);
                try self.pushExpr(for_.expr);
                try self.pushExpr(for_.body);
            },
            .while_, .infinite_loop, .breakable_loop => |loop| {
                try self.pushExpr(loop.cond);
                try self.pushExpr(loop.body);
            },
            .return_ => |ret| try self.pushExpr(ret.expr),
            .crash,
            .break_,
            .rejected_method_decl,
            .import_,
            .alias_decl,
            .where_alias_decl,
            .nominal_decl,
            .type_anno,
            .type_var_alias,
            => {},
        }
    }
};

/// The procedure templates whose every call, in every program, comes from
/// one site of their module's source. A template qualifies when it is
/// private (a promoted local, or a source definition the module neither
/// exposes nor registers as a method), no dispatch plan targets it, and the
/// module's source references it exactly once, as the function of a call.
/// Dev builds inline by these facts rather than by how many callers one
/// program has, so the decision is the same in every program that links the
/// code (design.md "Object cache").
fn computeSingleSourceTemplates(allocator: Allocator, bodies: CheckedBodies) Allocator.Error!*SingleSourceTemplates {
    const tables = bodies.tables;
    const templates = tables.templates orelse invariant("single-source calls were computed without the module's templates");
    const refs = tables.refs orelse invariant("single-source calls were computed without the module's value references");
    const bindings = tables.top_level_procedure_bindings orelse invariant("single-source calls were computed without the module's procedure bindings");
    const module = tables.module orelse invariant("single-source calls were computed without the module's identity");
    const env = bodies.env;

    const count = templates.templates.items.len;
    const eligible = try allocator.alloc(bool, count);
    defer allocator.free(eligible);
    @memset(eligible, false);
    const references = try allocator.alloc(u32, count);
    defer allocator.free(references);
    @memset(references, 0);
    const calls = try allocator.alloc(u32, count);
    defer allocator.free(calls);
    @memset(calls, 0);

    const Local = struct {
        module: canonical.CheckedModuleDigest,

        fn template(self: @This(), ref: canonical.ProcedureTemplateRef) ?canonical.CheckedProcedureTemplateId {
            if (!ref.artifact.eql(self.module)) return null;
            return ref.template;
        }
    };
    const local = Local{ .module = module };

    // Private templates: promoted locals, and source definitions that are
    // neither exposed nor methods.
    var public_defs = collections.DenseMap(CIR.Def.Idx, void).init(allocator);
    defer public_defs.deinit();
    for (tables.exported_defs) |def| try public_defs.put(def, {});
    for (env.method_defs.entries.items) |entry| try public_defs.put(entry.value.def_idx, {});
    for (templates.by_def) |entry| {
        const id = local.template(entry.template) orelse continue;
        if (!public_defs.contains(entry.def)) eligible[id.index()] = true;
    }
    for (templates.promoted) |entry| {
        const id = local.template(entry.template) orelse continue;
        eligible[id.index()] = true;
    }
    for (templates.templates.items, eligible) |template, *is_eligible| {
        if (template.target != .roc or template.body != .checked_body) is_eligible.* = false;
    }

    // A dispatch target can be reached without a reference. A plan's direct
    // target is the procedure a method registry entry of its method names,
    // and only this module's entries can name its private templates, so only
    // the plans of those entries' methods can target an eligible template.
    const dispatch = tables.dispatch orelse invariant("single-source calls were computed without the module's dispatch resolution");
    const plans = tables.plans orelse invariant("single-source calls were computed without the module's dispatch plans");
    var target_methods = collections.DenseMap(canonical.MethodNameId, void).init(allocator);
    defer target_methods.deinit();
    for (dispatch.source.method_registry.entries) |entry| {
        const target = entry.target orelse continue;
        switch (target.kind) {
            .procedure => |procedure| if (local.template(procedure.template)) |id| {
                if (eligible[id.index()]) try target_methods.put(entry.key.method, {});
            },
            .local_proc, .structural => {},
        }
    }
    for (templates.templates.items, 0..) |template, raw_template| {
        if (target_methods.count() == 0) break;
        const plan_refs = templates.dispatch_plan_refs[template.static_dispatch_plans.start..][0..template.static_dispatch_plans.len];
        for (plan_refs) |plan_id| {
            if (target_methods.contains(plans.plans[@backingInt(plan_id)].method)) break;
        } else continue;
        for ((try dispatch.templateRelations(@fromBackingInt(@intCast(raw_template)))).direct) |plan_id| {
            if (!target_methods.contains(plans.plans[@backingInt(plan_id)].method)) continue;
            const direct = switch ((try dispatch.plan(plan_id)).resolution) {
                .direct_closed, .direct_parametric => |direct| direct,
                .direct_pending, .evidence_dependent, .structural, .checked_error, .@"unreachable" => continue,
            };
            const target = switch ((try dispatch.evidenceNode(direct.evidence)).target.kind) {
                .procedure => |procedure| switch (procedure.runtime_target) {
                    .procedure => local.template(procedure.template),
                    .low_level, .intrinsic, .graph_participating => null,
                },
                .local_proc, .structural => null,
            };
            if (target) |id| eligible[id.index()] = false;
        }
    }

    var shapes = try SourceCallShapes.collect(allocator, bodies);
    defer shapes.deinit(allocator);
    const callees = &shapes.callees;
    for (refs.records) |record| {
        const procedure = switch (record.ref) {
            .top_level_proc, .promoted_top_level_proc => |procedure| procedure,
            .platform_required_proc => |required| required.procedure,
            .local_param,
            .local_value,
            .local_mutable_version,
            .pattern_binder,
            .local_proc,
            .selected_hoisted_const,
            .top_level_const,
            .imported_const,
            .imported_proc,
            .hosted_proc,
            .platform_required_declaration,
            .platform_required_checked_error,
            .platform_required_const,
            => continue,
        };
        const binding = switch (procedure.binding) {
            .top_level => |binding| binding,
            .platform_required => |required| checked_artifact.ArtifactTopLevelProcedureBindingRef{ .artifact = required.artifact, .binding = required.procedure_binding },
            .imported, .hosted => continue,
        };
        if (!binding.artifact.eql(module)) continue;
        const id = switch (bindings.get(binding.binding).body) {
            .direct_template => |direct| switch (direct.template) {
                .checked => |template| local.template(template),
                .lifted, .synthetic => null,
            },
            .callable_eval_template, .checked_error => null,
        } orelse continue;
        references[id.index()] += 1;
        if (callees.isSet(@backingInt(record.expr))) calls[id.index()] += 1;
    }

    var published = std.ArrayList(canonical.CheckedProcedureTemplateId).empty;
    errdefer published.deinit(allocator);
    for (eligible, references, calls, 0..) |is_eligible, reference_count, call_count, raw| {
        if (is_eligible and reference_count == 1 and call_count == 1) {
            try published.append(allocator, @fromBackingInt(@intCast(@as(u32, @intCast(raw)))));
        }
    }

    const facts = try allocator.create(SingleSourceTemplates);
    errdefer allocator.destroy(facts);
    facts.* = .{ .templates = try published.toOwnedSlice(allocator) };
    return facts;
}

/// The nested procedure sites whose function the module's source calls at
/// one site and never otherwise uses.
fn computeSingleSourceSites(allocator: Allocator, bodies: CheckedBodies) Allocator.Error!*SingleSourceSites {
    var shapes = try SourceCallShapes.collect(allocator, bodies);
    defer shapes.deinit(allocator);
    const sites = try singleSourceSites(allocator, try bodies.moduleNestedProcSites(), &shapes);
    errdefer allocator.free(sites);
    const facts = try allocator.create(SingleSourceSites);
    facts.* = .{ .sites = sites };
    return facts;
}

/// The call shapes of the module's source, from one pass over its nodes:
/// which expressions are a call's function, every local lookup, and the
/// pattern each expression is the whole value of for a non-recursive binding
/// of one name.
const SourceCallShapes = struct {
    callees: std.DynamicBitSetUnmanaged,
    /// Each local lookup expression and the pattern it reads.
    local_lookups: std.ArrayList(LocalLookup) = .empty,
    bound_by: collections.DenseMap(u32, u32),

    const LocalLookup = struct { expr: u32, pattern: u32 };

    fn collect(allocator: Allocator, bodies: CheckedBodies) Allocator.Error!SourceCallShapes {
        const index = bodies.index();
        var shapes = SourceCallShapes{
            .callees = try std.DynamicBitSetUnmanaged.initEmpty(allocator, index.node_kinds.len),
            .bound_by = collections.DenseMap(u32, u32).init(allocator),
        };
        errdefer shapes.deinit(allocator);
        const store = &bodies.env.store;
        for (index.node_kinds, 0..) |kind, raw_index| {
            const raw: u32 = @intCast(raw_index);
            switch (kind) {
                .expr => {
                    const tag = bodies.nodeTag(raw);
                    if (tag != .expr_call and tag != .expr_var) continue;
                    const id: ExprId = @fromBackingInt(raw);
                    // A retired call or lookup evaluates as a runtime error.
                    if (bodies.exprIsRetired(id)) continue;
                    const data = store.getExpr(id);
                    if (data == .e_call) {
                        shapes.callees.set(@backingInt(data.e_call.func));
                    } else if (data == .e_lookup_local) {
                        try shapes.local_lookups.append(allocator, .{ .expr = raw, .pattern = @backingInt(data.e_lookup_local.pattern_idx) });
                    }
                },
                .statement => {
                    if (bodies.nodeTag(raw) != .statement_decl) continue;
                    const id: StatementId = @fromBackingInt(raw);
                    switch (bodies.statementData(id)) {
                        .decl => |decl| {
                            if (bodies.declIsRecursive(id)) continue;
                            if (bodies.patternData(decl.pattern) != .assign) continue;
                            try shapes.bound_by.put(@backingInt(decl.expr), @backingInt(decl.pattern));
                        },
                        .promoted_proc, .var_, .var_uninitialized, .reassign, .crash, .dbg, .expr, .expect, .for_, .while_, .infinite_loop, .breakable_loop, .break_, .return_, .rejected_method_decl, .import_, .alias_decl, .where_alias_decl, .nominal_decl, .type_anno, .type_var_alias, .runtime_error => {},
                    }
                },
                .pattern, .unreached => {},
            }
        }
        return shapes;
    }

    fn deinit(self: *SourceCallShapes, allocator: Allocator) void {
        self.callees.deinit(allocator);
        self.local_lookups.deinit(allocator);
        self.bound_by.deinit();
    }
};

/// The expressions of the template-owned sites whose function the module's
/// source calls at one site and never otherwise uses, ascending: either the
/// site's expression is that call's function, or it is the whole value of a
/// non-recursive binding of one name whose one reference is. A default-root
/// site is lowered at every construction site that omits its field, and an
/// expression more than one owner constructs a procedure at is lowered once
/// per owner, so neither qualifies.
fn singleSourceSites(
    allocator: Allocator,
    sites: *const checked_artifact.ModuleNestedProcSites,
    shapes: *const SourceCallShapes,
) Allocator.Error![]u32 {
    // Each pattern's references, and how many of them are a call's function.
    var references = collections.DenseMap(u32, u32).init(allocator);
    defer references.deinit();
    var calls = collections.DenseMap(u32, u32).init(allocator);
    defer calls.deinit();
    for (shapes.local_lookups.items) |lookup| {
        (try references.getOrPutValue(lookup.pattern, 0)).value_ptr.* += 1;
        if (shapes.callees.isSet(lookup.expr)) (try calls.getOrPutValue(lookup.pattern, 0)).value_ptr.* += 1;
    }

    var qualifying = std.ArrayList(u32).empty;
    errdefer qualifying.deinit(allocator);
    for (sites.exprs, sites.first_templates, sites.owner_counts) |expr, first_template, owner_count| {
        if (owner_count != 1 or first_template == null) continue;
        const raw = @backingInt(expr);
        const single = shapes.callees.isSet(raw) or if (shapes.bound_by.get(raw)) |pattern|
            (references.get(pattern) orelse 0) == 1 and (calls.get(pattern) orelse 0) == 1
        else
            false;
        if (single) try qualifying.append(allocator, raw);
    }
    return qualifying.toOwnedSlice(allocator);
}
