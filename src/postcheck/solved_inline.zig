//! Explicit inline eligibility analysis over Lambda Solved IR.

const std = @import("std");
const collections = @import("collections");

const Common = @import("common.zig");
const Lifted = @import("monotype_lifted/ast.zig");
const WrapperBody = @import("monotype_lifted/wrapper_body.zig");
const Solved = @import("lambda_solved/ast.zig");
const SolvedType = @import("lambda_solved/type.zig");
const Instances = @import("lambda_solved/instances.zig");
const SpecFnId = Instances.SpecFnId;
const GuardedList = collections.GuardedList;

/// Post-check inline analysis mode.
pub const Mode = enum {
    /// Inline only wrappers of compiler-provided operations with type-only
    /// operands (`LowLevel.typeOnlyOperands`), as every mode does: such an
    /// operand has no runtime value, and a wrapper procedure would take it as
    /// a runtime argument, keeping it live until the call.
    none,
    /// Retain standalone procedures. Only
    /// wrappers whose bodies the compiler provides (`provided_low_level`) are
    /// inlined, since substituting them makes no caller's code depend on
    /// another procedure's source.
    procedures,
    /// Inline wrappers, and bodies the whole program calls exactly once.
    wrappers,
    /// Inline wrappers, and bodies whose module's source calls them at
    /// exactly one site (`FnTemplate.single_source_call`). Both depend only on
    /// source, never on which other procedures a program contains, so every
    /// program inlines the same calls, including separately compiled callers.
    wrappers_and_source_single_use,
};

/// What an inline body is.
pub const InlineKind = enum(u8) {
    none,
    /// A small call-through or low-level wrapper, inlined at every call site.
    wrapper,
    /// Called exactly once in the whole program and lowered only there.
    single_use,
    /// Called at one site of its module's source. Inlined wherever that call
    /// is lowered outside a wrapper body: a wrapper body can be lowered more
    /// than once into one procedure, and a single-use body must not be.
    source_single_use,
};

/// Each function's inline body at each instance of its group, when it has
/// one, by spec function id.
const InlineBodies = collections.OptionalIndexColumn(Lifted.ExprId);

/// Immutable inline eligibility table consumed by later lowering stages.
pub const Plan = struct {
    inline_bodies: InlineBodies.View = .{},
    kinds: []const InlineKind = &.{},
    /// Per spec function, whether its inline body holds a `return`. An
    /// inlined `return` delivers the call's result; it never leaves the
    /// caller.
    returns: []const bool = &.{},

    /// Whether `fn_id`'s inline body holds a `return`.
    pub fn bodyReturns(self: Plan, fn_id: SpecFnId) bool {
        if (self.returns.len == 0) return false;
        const index = @backingInt(fn_id);
        if (index >= self.returns.len) {
            Common.invariant("inline plan did not contain a spec function");
        }
        return self.returns[index];
    }

    pub fn kind(self: Plan, fn_id: SpecFnId) InlineKind {
        if (self.kinds.len == 0) return .none;
        const index = @backingInt(fn_id);
        if (index >= self.kinds.len) {
            Common.invariant("inline plan did not contain a spec function");
        }
        return self.kinds[index];
    }

    /// The body to inline for a direct call to `callee` lowered while
    /// lowering `owner`'s body, if the plan inlines that call.
    pub fn bodyForCall(self: Plan, callee: SpecFnId, owner: ?SpecFnId) ?Lifted.ExprId {
        const body = self.bodyForFn(callee) orelse return null;
        if (self.kind(callee) == .source_single_use) {
            if (owner) |owning_fn| {
                if (self.kind(owning_fn) == .wrapper) return null;
            }
        }
        return body;
    }

    pub fn bodyForFn(self: Plan, fn_id: SpecFnId) ?Lifted.ExprId {
        if (self.inline_bodies.len() == 0) return null;

        const index = @backingInt(fn_id);
        if (index >= self.inline_bodies.len()) {
            Common.invariant("inline plan did not contain a spec function");
        }
        return self.inline_bodies.get(index);
    }
};

/// Allocator-owned storage for a post-check inline plan.
pub const OwnedPlan = struct {
    allocator: std.mem.Allocator,
    inline_bodies: InlineBodies,
    kinds: []InlineKind,
    returns: []bool,

    pub fn empty(allocator: std.mem.Allocator) OwnedPlan {
        return .{ .allocator = allocator, .inline_bodies = .{}, .kinds = &.{}, .returns = &.{} };
    }

    pub fn deinit(self: *OwnedPlan) void {
        self.inline_bodies.deinit(self.allocator);
        if (self.kinds.len != 0) self.allocator.free(self.kinds);
        if (self.returns.len != 0) self.allocator.free(self.returns);
        self.* = empty(self.allocator);
    }

    pub fn view(self: *const OwnedPlan) Plan {
        return .{ .inline_bodies = self.inline_bodies.view(), .kinds = self.kinds, .returns = self.returns };
    }
};

/// Analyze a Lambda Solved program and produce explicit inline decisions for
/// every function at every instance of its group (`Instances.SpecFnId`).
/// `procedure_usage` is indexed by spec function.
pub fn analyze(
    allocator: std.mem.Allocator,
    mode: Mode,
    procedure_usage: []const Instances.ProcedureUse,
    solved: *const Solved.Program,
) std.mem.Allocator.Error!OwnedPlan {
    return switch (mode) {
        .none => try InlineAnalyzer.run(allocator, procedure_usage, solved, .type_only_operands),
        .procedures => try InlineAnalyzer.run(allocator, procedure_usage, solved, .provided_low_level),
        .wrappers => try InlineAnalyzer.run(allocator, procedure_usage, solved, .program),
        .wrappers_and_source_single_use => try InlineAnalyzer.run(allocator, procedure_usage, solved, .source),
    };
}

const Decision = union(enum) {
    unknown,
    never,
    inline_body: Candidate,
};

const Candidate = struct {
    body: Lifted.ExprId,
    kind: InlineKind,
};

fn specIndex(id: SpecFnId) usize {
    return @backingInt(id);
}

/// Which bodies are candidates: wrappers and bodies used once by the whole
/// program's calls or by their module's source, or only wrappers of
/// compiler-provided low-level templates, or only those whose operation has
/// type-only operands. The last two need no use information.
const CandidateRule = enum { program, source, provided_low_level, type_only_operands };

const MaterializationState = enum {
    unknown,
    visiting,
    once,
    multiple,
};

const InlineAnalyzer = struct {
    allocator: std.mem.Allocator,
    procedure_usage: []const Instances.ProcedureUse,
    solved: *const Solved.Program,
    solved_types: SolvedType.Store.View,
    decisions: []Decision,
    rule: CandidateRule,

    fn run(
        allocator: std.mem.Allocator,
        procedure_usage: []const Instances.ProcedureUse,
        solved: *const Solved.Program,
        rule: CandidateRule,
    ) std.mem.Allocator.Error!OwnedPlan {
        const spec_fn_count = solved.instances.specFnCount();
        if (rule != .provided_low_level and rule != .type_only_operands and procedure_usage.len != spec_fn_count) {
            Common.invariant("optimized inline analysis requires exact use information for every spec function");
        }
        const decisions = try allocator.alloc(Decision, spec_fn_count);
        errdefer allocator.free(decisions);
        @memset(decisions, .unknown);

        var analyzer = InlineAnalyzer{
            .allocator = allocator,
            .procedure_usage = procedure_usage,
            .solved = solved,
            .solved_types = solved.types.view(),
            .decisions = decisions,
            .rule = rule,
        };
        try analyzer.decideCandidates();

        const materialization_states = try allocator.alloc(MaterializationState, decisions.len);
        defer allocator.free(materialization_states);
        @memset(materialization_states, .unknown);
        for (0..spec_fn_count) |index| {
            const fn_id: SpecFnId = @fromBackingInt(@as(u32, @intCast(index)));
            try analyzer.resolveSingleUseMaterialization(fn_id, materialization_states);
        }

        var inline_bodies = try InlineBodies.initFilled(allocator, decisions.len);
        errdefer inline_bodies.deinit(allocator);
        const kinds = try allocator.alloc(InlineKind, decisions.len);
        errdefer allocator.free(kinds);
        // The low-level wrapper rules run without use information, and a
        // wrapper body holds no `return`.
        const returns = try allocator.alloc(bool, if (procedure_usage.len == 0) 0 else decisions.len);
        errdefer allocator.free(returns);
        for (returns, 0..) |*body_returns, index| {
            body_returns.* = decisions[index] == .inline_body and procedure_usage[index].contains_return;
        }
        for (decisions, kinds, 0..) |decision, *body_kind, index| {
            switch (decision) {
                .inline_body => |candidate| inline_bodies.set(index, candidate.body),
                .unknown,
                .never,
                => {},
            }
            body_kind.* = if (decision == .inline_body) decision.inline_body.kind else .none;
        }

        allocator.free(decisions);
        analyzer.decisions = &.{};

        return .{
            .allocator = allocator,
            .inline_bodies = inline_bodies,
            .kinds = kinds,
            .returns = returns,
        };
    }

    fn use(self: *const InlineAnalyzer, fn_id: SpecFnId) Instances.ProcedureUse {
        return self.procedure_usage[specIndex(fn_id)];
    }

    fn source(self: *const InlineAnalyzer, fn_id: SpecFnId) Lifted.FnId {
        return self.solved.instances.specFnSource(fn_id);
    }

    fn inlineCandidate(self: *const InlineAnalyzer, fn_id: SpecFnId) std.mem.Allocator.Error!?Candidate {
        const template = self.solved.lifted.getFn(self.source(fn_id)).source;
        const low_level_rule_admits: ?bool = switch (self.rule) {
            .provided_low_level => (template orelse return null).provided_low_level != .none,
            .type_only_operands => (template orelse return null).provided_low_level == .operation_with_type_only_operands,
            .program, .source => null,
        };
        if (low_level_rule_admits) |admits| {
            if (!admits) return null;
            return if (try self.wrapperCandidate(fn_id)) |body| .{ .body = body, .kind = .wrapper } else null;
        }
        // A pack program inlines a wrapper exactly as any program does.
        if (try self.wrapperCandidate(fn_id)) |body| return .{ .body = body, .kind = .wrapper };
        if (self.rule == .source) {
            if (self.sourceSingleUseCandidate(fn_id)) |body| return .{ .body = body, .kind = .source_single_use };
            return null;
        }
        if (self.singleUseCandidate(fn_id)) |body| return .{ .body = body, .kind = .single_use };
        return null;
    }

    /// A body its module's source calls at one site. Every call to it in any
    /// program is that site, lowered in some specialization of the function
    /// containing it, so the decision needs no program-wide count.
    fn sourceSingleUseCandidate(self: *const InlineAnalyzer, fn_id: SpecFnId) ?Lifted.ExprId {
        const source_fn = self.solved.lifted.getFn(self.source(fn_id));
        const template = source_fn.source orelse return null;
        if (!template.single_source_call) return null;
        return switch (source_fn.body) {
            .roc => |body_expr| body_expr,
            .hosted => null,
        };
    }

    fn singleUseCandidate(self: *const InlineAnalyzer, fn_id: SpecFnId) ?Lifted.ExprId {
        const fn_use = self.use(fn_id);
        if (fn_use.external_calls != 1 or fn_use.value_refs != 0 or fn_use.is_root) return null;

        const call_expr_id = fn_use.external_call_expr orelse
            Common.invariant("single-use function had no external call expression");
        const call_expr = self.solved.lifted.getExpr(call_expr_id);
        if (call_expr.data != .call_proc) {
            Common.invariant("single-use function external call was not a direct call expression");
        }
        const call = call_expr.data.call_proc;
        if (Lifted.localDirectCallee(call) != self.source(fn_id)) {
            Common.invariant("single-use function external call changed callee");
        }
        if (call.is_cold) return null;

        const source_fn = self.solved.lifted.getFn(self.source(fn_id));
        const body = switch (source_fn.body) {
            .roc => |body_expr| body_expr,
            .hosted => return null,
        };
        return body;
    }

    /// How many captures `fn_id`'s own member carries; the same at every
    /// instance of its group.
    fn solvedCaptureCount(self: *const InlineAnalyzer, fn_id: Lifted.FnId) usize {
        const captures = self.solvedCapturesForFn(fn_id);
        return self.solved_types.captureSpan(captures).len;
    }

    fn solvedCapturesForFn(self: *const InlineAnalyzer, fn_id: Lifted.FnId) SolvedType.Span {
        const fn_symbol = self.solved.lifted.getFn(fn_id).symbol;
        const fn_content = self.solved.types.rootContent(self.solved.fn_tys.items[@backingInt(fn_id)]);
        if (fn_content != .func) Common.invariant("direct Lambda Mono function table contains a non-function type");
        const callable_content = self.solved.types.rootContent(fn_content.func.callable);
        const callable = if (callable_content == .lambda_set)
            callable_content.lambda_set
        else if (callable_content == .erased)
            callable_content.erased.members
        else
            Common.invariant("callable value did not have a resolved callable slot");
        for (self.solved_types.memberSpan(callable)) |member| {
            if (member.lambda == fn_symbol) return member.captures;
        }
        return .empty();
    }

    /// A function whose candidate body is closed, and the range of
    /// `CandidateGraph.callees` its body calls directly.
    const CandidateNode = struct {
        fn_id: SpecFnId,
        candidate: Candidate,
        callees_start: u32,
        callees_end: u32,
    };

    /// A position in `CandidateGraph.nodes`.
    const CandidateNodeIndex = enum(u32) { _ };

    const CandidateGraph = struct {
        nodes: std.ArrayList(CandidateNode) = .empty,
        callees: std.ArrayList(SpecFnId) = .empty,
        /// Per spec function, its index in `nodes`.
        node_of: collections.OptionalIndexColumn(CandidateNodeIndex),

        fn deinit(self: *CandidateGraph, allocator: std.mem.Allocator) void {
            self.nodes.deinit(allocator);
            self.callees.deinit(allocator);
            self.node_of.deinit(allocator);
        }

        fn calleeNodes(self: *const CandidateGraph, node: u32) []const SpecFnId {
            const n = self.nodes.items[node];
            return self.callees.items[n.callees_start..n.callees_end];
        }
    };

    /// Decide every function. A function is inlined when it has a candidate
    /// body that is closed and leaving it inlined lets no cycle of direct
    /// calls among such bodies inline without bound (`rejectCandidateCycles`),
    /// so inlining always terminates. Cycles are the graph's strongly
    /// connected components, which depend on the call graph alone: a
    /// procedure's identity names the same bodies and calls in every program,
    /// so every program decides it the same way, whatever order its functions
    /// are numbered in.
    fn decideCandidates(self: *InlineAnalyzer) std.mem.Allocator.Error!void {
        const fn_count = self.decisions.len;
        var graph = CandidateGraph{ .node_of = try collections.OptionalIndexColumn(CandidateNodeIndex).initFilled(self.allocator, fn_count) };
        defer graph.deinit(self.allocator);

        for (0..fn_count) |index| {
            const fn_id: SpecFnId = @fromBackingInt(@as(u32, @intCast(index)));
            const candidate = try self.inlineCandidate(fn_id) orelse {
                self.decisions[index] = .never;
                continue;
            };
            const callees_start: u32 = @intCast(graph.callees.items.len);
            switch (try self.scanCandidateBody(candidate.body, self.solved.instances.specFnEnv(fn_id), &graph.callees)) {
                .closed => {},
                .open => {
                    graph.callees.shrinkRetainingCapacity(callees_start);
                    self.decisions[index] = .never;
                    continue;
                },
            }
            graph.node_of.set(index, @fromBackingInt(@as(u32, @intCast(graph.nodes.items.len))));
            try graph.nodes.append(self.allocator, .{
                .fn_id = fn_id,
                .candidate = candidate,
                .callees_start = callees_start,
                .callees_end = @intCast(graph.callees.items.len),
            });
        }

        try self.rejectCandidateCycles(&graph, .all);
        // Rejecting a cycle's wrappers can leave a cycle of single-use
        // bodies alone, which every member of then leaves.
        try self.rejectCandidateCycles(&graph, .accepted);
    }

    const BodyShape = enum {
        /// No escaping return, break, or continue.
        closed,
        /// The body can transfer control out of itself.
        open,
    };

    const WalkItem = struct { child: Lifted.ExprChild, loop_depth: usize };

    /// Prove that every break and continue in `body`, read under instance
    /// `env`, is owned by a loop inside it, appending each procedure it calls
    /// directly to `callees`.
    fn scanCandidateBody(self: *InlineAnalyzer, body: Lifted.ExprId, env: Instances.Env, callees: *std.ArrayList(SpecFnId)) std.mem.Allocator.Error!BodyShape {
        var walk: std.ArrayList(WalkItem) = .empty;
        defer walk.deinit(self.allocator);
        var children: std.ArrayList(Lifted.ExprChild) = .empty;
        defer children.deinit(self.allocator);
        try walk.append(self.allocator, .{ .child = .{ .expr = body }, .loop_depth = 0 });
        while (walk.pop()) |item| {
            const depth = item.loop_depth;
            children.clearRetainingCapacity();
            switch (item.child) {
                .stmt => |stmt_id| switch (self.solved.lifted.getStmt(stmt_id)) {
                    .return_ => return .open,
                    .let_, .expr, .expect, .dbg, .uninitialized, .crash, .checked_error => try Lifted.appendStmtChildren(self.allocator, &self.solved.lifted, stmt_id, &children),
                },
                .expr => |expr_id| switch (self.solved.lifted.getExpr(expr_id).data) {
                    .return_ => return .open,
                    .break_, .continue_ => if (depth == 0) return .open else try Lifted.appendChildren(self.allocator, &self.solved.lifted, expr_id, &children),
                    .loop_ => |loop| {
                        const initial_values = self.solved.lifted.exprSpan(loop.initial_values);
                        for (0..initial_values.len) |index| try children.append(self.allocator, .{ .expr = GuardedList.at(initial_values, index) });
                        try walk.append(self.allocator, .{ .child = .{ .expr = loop.body }, .loop_depth = depth + 1 });
                    },
                    .lambda,
                    .fn_def,
                    .uninitialized,
                    .uninitialized_payload,
                    .crash,
                    .checked_error,
                    .comptime_exhaustiveness_failed,
                    => {},
                    .call_proc => |call| {
                        if (Lifted.localDirectCallee(call)) |callee| {
                            const callee_env = Instances.referenceEnv(self.solved, expr_id, callee, env);
                            try callees.append(self.allocator, self.solved.instances.specFn(callee, callee_env));
                        }
                        try Lifted.appendChildren(self.allocator, &self.solved.lifted, expr_id, &children);
                    },
                    .@"unreachable",
                    .local,
                    .unit,
                    .int_lit,
                    .frac_f32_lit,
                    .frac_f64_lit,
                    .dec_lit,
                    .str_lit,
                    .bytes_lit,
                    .def_ref,
                    .fn_ref,
                    .list,
                    .tuple,
                    .record,
                    .record_update,
                    .tag,
                    .typed_boundary,
                    .row_widen,
                    .static_data_candidate,
                    .comptime_value,
                    .nominal,
                    .dbg,
                    .expect,
                    .expect_err,
                    .literal_rejected,
                    .let_,
                    .call_value,
                    .low_level,
                    .field_access,
                    .tuple_access,
                    .structural_eq,
                    .structural_hash,
                    .match_,
                    .if_,
                    .block,
                    .join_point,
                    .jump,
                    .if_initialized_payload,
                    .try_sequence,
                    .try_record_sequence,
                    => try Lifted.appendChildren(self.allocator, &self.solved.lifted, expr_id, &children),
                },
            }
            for (children.items) |child| try walk.append(self.allocator, .{ .child = child, .loop_depth = depth });
        }
        return .closed;
    }

    /// Which nodes of the candidate graph one cycle pass considers.
    const CyclePass = enum {
        /// Every node: a node on no cycle takes its candidate, and a cycle
        /// leaves only its wrappers, or every member when it has none.
        all,
        /// The nodes the first pass accepted: a cycle among them, which holds
        /// no wrapper, leaves every member.
        accepted,
    };

    /// Keep every cycle of candidates (a strongly connected component of more
    /// than one node, or a node that calls itself) from inlining without
    /// bound. Leaving any one member a procedure breaks a cycle, so a cycle
    /// first rejects only its wrappers: a wrapper is inlined at every call as
    /// an optimization, while a single-use body is lowered only at its one
    /// call, and keeping those inlined keeps a recursion through a lambda a
    /// self-call. Tarjan's algorithm, run on an explicit stack so call chains
    /// never become native call depth.
    fn rejectCandidateCycles(self: *InlineAnalyzer, graph: *const CandidateGraph, pass: CyclePass) std.mem.Allocator.Error!void {
        const node_count = graph.nodes.items.len;
        // Visit orders are issued from one, so zero marks an unvisited node.
        const unvisited: u32 = 0;
        const order = try self.allocator.alloc(u32, node_count);
        defer self.allocator.free(order);
        const low = try self.allocator.alloc(u32, node_count);
        defer self.allocator.free(low);
        const on_stack = try self.allocator.alloc(bool, node_count);
        defer self.allocator.free(on_stack);
        @memset(order, unvisited);
        @memset(on_stack, false);

        const Frame = struct { node: u32, next_callee: u32 };
        var frames: std.ArrayList(Frame) = .empty;
        defer frames.deinit(self.allocator);
        var component_stack: std.ArrayList(u32) = .empty;
        defer component_stack.deinit(self.allocator);
        var next_order: u32 = 1;

        for (0..node_count) |root_index| {
            const root: u32 = @intCast(root_index);
            if (order[root] != unvisited) continue;
            if (pass == .accepted and !self.acceptedNode(graph, root)) continue;
            order[root] = next_order;
            low[root] = next_order;
            next_order += 1;
            try component_stack.append(self.allocator, root);
            on_stack[root] = true;
            try frames.append(self.allocator, .{ .node = root, .next_callee = 0 });

            while (frames.items.len != 0) {
                const frame = &frames.items[frames.items.len - 1];
                const node = frame.node;
                const callees = graph.calleeNodes(node);
                if (frame.next_callee < callees.len) {
                    const callee_fn = callees[frame.next_callee];
                    frame.next_callee += 1;
                    const callee: u32 = @backingInt(graph.node_of.get(@backingInt(callee_fn)) orelse continue);
                    if (pass == .accepted and !self.acceptedNode(graph, callee)) continue;
                    if (order[callee] == unvisited) {
                        order[callee] = next_order;
                        low[callee] = next_order;
                        next_order += 1;
                        try component_stack.append(self.allocator, callee);
                        on_stack[callee] = true;
                        try frames.append(self.allocator, .{ .node = callee, .next_callee = 0 });
                    } else if (on_stack[callee]) {
                        low[node] = @min(low[node], order[callee]);
                    }
                    continue;
                }

                _ = frames.pop();
                if (frames.items.len != 0) {
                    const parent = frames.items[frames.items.len - 1].node;
                    low[parent] = @min(low[parent], low[node]);
                }
                if (low[node] != order[node]) continue;

                const top = component_stack.items.len;
                var start = top;
                while (true) {
                    start -= 1;
                    on_stack[component_stack.items[start]] = false;
                    if (component_stack.items[start] == node) break;
                }
                const component = component_stack.items[start..top];
                const on_cycle = component.len > 1 or callsItself(graph, node);
                var has_wrapper = false;
                for (component) |member| {
                    if (graph.nodes.items[member].candidate.kind == .wrapper) has_wrapper = true;
                }
                for (component) |member| {
                    const member_node = graph.nodes.items[member];
                    const index = @backingInt(member_node.fn_id);
                    if (on_cycle) {
                        if (!has_wrapper or member_node.candidate.kind == .wrapper) self.decisions[index] = .never;
                    } else if (pass == .all) {
                        self.decisions[index] = .{ .inline_body = member_node.candidate };
                    }
                }
                // A cycle that kept its single-use bodies is settled by the
                // second pass.
                if (on_cycle and has_wrapper and pass == .all) for (component) |member| {
                    const member_node = graph.nodes.items[member];
                    if (member_node.candidate.kind != .wrapper) self.decisions[@backingInt(member_node.fn_id)] = .{ .inline_body = member_node.candidate };
                };
                component_stack.shrinkRetainingCapacity(start);
            }
        }
    }

    fn acceptedNode(self: *const InlineAnalyzer, graph: *const CandidateGraph, node: u32) bool {
        return self.decisions[@backingInt(graph.nodes.items[node].fn_id)] != .never;
    }

    fn callsItself(graph: *const CandidateGraph, node: u32) bool {
        const fn_id = graph.nodes.items[node].fn_id;
        for (graph.calleeNodes(node)) |callee| {
            if (callee == fn_id) return true;
        }
        return false;
    }

    /// Resolve outer single-use candidates before their descendants. Demoting
    /// an outer body creates a procedure boundary, which can make a nested
    /// single-use body safe to inline exactly once.
    fn resolveSingleUseMaterialization(
        self: *InlineAnalyzer,
        fn_id: SpecFnId,
        states: []MaterializationState,
    ) std.mem.Allocator.Error!void {
        _ = try self.runMaterialization(.{ .resolve = fn_id }, states);
    }

    /// One step of the materialization proof. `resolve` settles a single-use
    /// candidate; `body_once` asks whether a body is lowered in exactly one
    /// place. Both follow unique call owners outward, so the chain of owners
    /// is held on an explicit stack.
    const MaterializationFrame = union(enum) {
        resolve: SpecFnId,
        body_once: SpecFnId,
    };

    fn runMaterialization(self: *InlineAnalyzer, root: MaterializationFrame, states: []MaterializationState) std.mem.Allocator.Error!bool {
        var frames: std.ArrayList(MaterializationFrame) = .empty;
        defer frames.deinit(self.allocator);
        try frames.append(self.allocator, root);
        // The result of the frame that just finished, for its parent.
        var result: ?bool = null;
        while (frames.items.len != 0) {
            const frame = frames.items[frames.items.len - 1];
            switch (frame) {
                .resolve => |fn_id| {
                    const index = @backingInt(fn_id);
                    if (result) |once| {
                        if (!once) self.decisions[index] = .never;
                        states[index] = .once;
                        _ = frames.pop();
                        result = true;
                        continue;
                    }
                    const candidate = switch (self.decisions[index]) {
                        .inline_body => |candidate| candidate,
                        .never => {
                            states[index] = .once;
                            _ = frames.pop();
                            result = true;
                            continue;
                        },
                        .unknown => Common.invariant("inline materialization analysis saw an unfinished decision"),
                    };
                    if (candidate.kind != .single_use) {
                        _ = frames.pop();
                        result = true;
                        continue;
                    }

                    switch (states[index]) {
                        .unknown => states[index] = .visiting,
                        .visiting => Common.invariant("single-use call-owner graph contained a selected cycle"),
                        .once => {
                            _ = frames.pop();
                            result = true;
                            continue;
                        },
                        .multiple => Common.invariant("resolved single-use body still had multiple materializations"),
                    }

                    const owner = self.use(fn_id).external_call_owner orelse
                        Common.invariant("single-use function had no external call owner");
                    try frames.append(self.allocator, .{ .body_once = owner });
                },
                .body_once => |fn_id| {
                    const index = @backingInt(fn_id);
                    if (result) |once| {
                        states[index] = if (once) .once else .multiple;
                        _ = frames.pop();
                        result = once;
                        continue;
                    }
                    switch (self.decisions[index]) {
                        .never => {
                            states[index] = .once;
                            _ = frames.pop();
                            result = true;
                            continue;
                        },
                        .inline_body => |candidate| if (candidate.kind == .single_use) {
                            // The resolved single-use body answers for this one.
                            frames.items[frames.items.len - 1] = .{ .resolve = fn_id };
                            continue;
                        },
                        .unknown => Common.invariant("inline materialization analysis saw an unfinished owner decision"),
                    }

                    switch (states[index]) {
                        .unknown => states[index] = .visiting,
                        .visiting => Common.invariant("selected wrapper call-owner graph contained a cycle"),
                        .once => {
                            _ = frames.pop();
                            result = true;
                            continue;
                        },
                        .multiple => {
                            _ = frames.pop();
                            result = false;
                            continue;
                        },
                    }

                    const fn_use = self.use(fn_id);
                    // One reference to a wrapper as a value can lead to many
                    // dispatch sites, each materializing the wrapper's body.
                    const settled: ?bool = if (fn_use.value_refs != 0)
                        false
                    else if (fn_use.external_calls == 0)
                        true
                    else if (fn_use.external_calls != 1)
                        false
                    else blk: {
                        const call_expr_id = fn_use.external_call_expr orelse
                            Common.invariant("single-call inline owner had no external call expression");
                        const call_expr = self.solved.lifted.getExpr(call_expr_id);
                        if (call_expr.data != .call_proc) {
                            Common.invariant("single-call inline owner use was not a direct call expression");
                        }
                        if (call_expr.data.call_proc.is_cold) break :blk true;
                        if (fn_use.is_root) break :blk false;
                        break :blk null;
                    };
                    if (settled) |once| {
                        states[index] = if (once) .once else .multiple;
                        _ = frames.pop();
                        result = once;
                        continue;
                    }
                    const owner = fn_use.external_call_owner orelse
                        Common.invariant("single-call inline owner had no external call owner");
                    try frames.append(self.allocator, .{ .body_once = owner });
                },
            }
            result = null;
        }
        return result.?;
    }

    fn wrapperCandidate(self: *const InlineAnalyzer, fn_id: SpecFnId) std.mem.Allocator.Error!?Lifted.ExprId {
        const source_fn = self.source(fn_id);
        if (self.solvedCaptureCount(source_fn) != 0) return null;
        return WrapperBody.wrapperBody(self.allocator, &self.solved.lifted, source_fn);
    }
};
