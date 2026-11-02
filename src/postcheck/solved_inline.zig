//! Explicit inline eligibility analysis over Lambda Solved IR.

const std = @import("std");
const collections = @import("collections");

const Common = @import("common.zig");
const Lifted = @import("monotype_lifted/ast.zig");
const SpecConstr = @import("monotype_lifted/spec_constr.zig");
const WrapperBody = @import("monotype_lifted/wrapper_body.zig");
const Solved = @import("lambda_solved/ast.zig");
const SolvedType = @import("lambda_solved/type.zig");
const GuardedList = collections.GuardedList;

/// Post-check inline analysis mode.
pub const Mode = enum {
    none,
    /// Retain standalone procedures, with iterator fusion in SpecConstr. Only
    /// wrappers whose bodies the compiler provides (`provides_low_level`) are
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

/// Immutable inline eligibility table consumed by later lowering stages.
pub const Plan = struct {
    inline_bodies: []const ?Lifted.ExprId = &.{},
    kinds: []const InlineKind = &.{},

    pub fn kind(self: Plan, fn_id: Lifted.FnId) InlineKind {
        if (self.kinds.len == 0) return .none;
        const index = @backingInt(fn_id);
        if (index >= self.kinds.len) {
            Common.invariant("inline plan did not contain a lifted function");
        }
        return self.kinds[index];
    }

    /// The body to inline for a direct call to `callee` lowered while
    /// lowering `owner`'s body, if the plan inlines that call.
    pub fn bodyForCall(self: Plan, callee: Lifted.FnId, owner: ?Lifted.FnId) ?Lifted.ExprId {
        const body = self.bodyForFn(callee) orelse return null;
        if (self.kind(callee) == .source_single_use) {
            if (owner) |owning_fn| {
                if (self.kind(owning_fn) == .wrapper) return null;
            }
        }
        return body;
    }

    pub fn bodyForFn(self: Plan, fn_id: Lifted.FnId) ?Lifted.ExprId {
        if (self.inline_bodies.len == 0) return null;

        const index = @backingInt(fn_id);
        if (index >= self.inline_bodies.len) {
            Common.invariant("inline plan did not contain a lifted function");
        }
        return self.inline_bodies[index];
    }
};

/// Allocator-owned storage for a post-check inline plan.
pub const OwnedPlan = struct {
    allocator: std.mem.Allocator,
    inline_bodies: []?Lifted.ExprId,
    kinds: []InlineKind,

    pub fn empty(allocator: std.mem.Allocator) OwnedPlan {
        return .{ .allocator = allocator, .inline_bodies = &.{}, .kinds = &.{} };
    }

    pub fn deinit(self: *OwnedPlan) void {
        if (self.inline_bodies.len != 0) self.allocator.free(self.inline_bodies);
        if (self.kinds.len != 0) self.allocator.free(self.kinds);
        self.* = empty(self.allocator);
    }

    pub fn view(self: *const OwnedPlan) Plan {
        return .{ .inline_bodies = self.inline_bodies, .kinds = self.kinds };
    }
};

/// Analyze a Lambda Solved program and produce explicit inline decisions.
/// With `keep_keyed_specializations`, a function that is a keyed template
/// specialization is never inlined away by the whole-program single-use
/// rule: a pack program offers those procedures from its manifest, so each
/// must survive as a procedure even when its only caller is the export
/// wrapper. Wrapper and source-single-use decisions depend on source alone,
/// so a pack program makes them as any program does.
pub fn analyze(
    allocator: std.mem.Allocator,
    mode: Mode,
    procedure_usage: SpecConstr.ProcedureUsage,
    solved: *const Solved.Program,
    keep_keyed_specializations: bool,
) std.mem.Allocator.Error!OwnedPlan {
    return switch (mode) {
        .none => OwnedPlan.empty(allocator),
        .procedures => try InlineAnalyzer.run(allocator, procedure_usage, solved, keep_keyed_specializations, .provided_low_level),
        .wrappers => try InlineAnalyzer.run(allocator, procedure_usage, solved, keep_keyed_specializations, .program),
        .wrappers_and_source_single_use => try InlineAnalyzer.run(allocator, procedure_usage, solved, keep_keyed_specializations, .source),
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

/// Which bodies are candidates: wrappers and bodies used once by the whole
/// program's calls or by their module's source, or only wrappers of
/// compiler-provided low-level templates, which needs no use information.
const CandidateRule = enum { program, source, provided_low_level };

const MaterializationState = enum {
    unknown,
    visiting,
    once,
    multiple,
};

const InlineAnalyzer = struct {
    allocator: std.mem.Allocator,
    procedure_usage: SpecConstr.ProcedureUsage,
    solved: *const Solved.Program,
    solved_types: SolvedType.Store.View,
    decisions: []Decision,
    keep_keyed_specializations: bool,
    rule: CandidateRule,

    fn run(
        allocator: std.mem.Allocator,
        procedure_usage: SpecConstr.ProcedureUsage,
        solved: *const Solved.Program,
        keep_keyed_specializations: bool,
        rule: CandidateRule,
    ) std.mem.Allocator.Error!OwnedPlan {
        if (rule != .provided_low_level and procedure_usage.items.len != solved.lifted.fnCount()) {
            Common.invariant("optimized inline analysis requires exact use information for every lifted function");
        }
        const decisions = try allocator.alloc(Decision, solved.lifted.fnCount());
        errdefer allocator.free(decisions);
        @memset(decisions, .unknown);

        var analyzer = InlineAnalyzer{
            .allocator = allocator,
            .procedure_usage = procedure_usage,
            .solved = solved,
            .solved_types = solved.types.view(),
            .decisions = decisions,
            .keep_keyed_specializations = keep_keyed_specializations,
            .rule = rule,
        };
        try analyzer.decideCandidates();

        const materialization_states = try allocator.alloc(MaterializationState, decisions.len);
        defer allocator.free(materialization_states);
        @memset(materialization_states, .unknown);
        for (0..solved.lifted.fnCount()) |index| {
            const fn_id: Lifted.FnId = @fromBackingInt(@intCast(@as(u32, @intCast(index))));
            try analyzer.resolveSingleUseMaterialization(fn_id, materialization_states);
        }

        const inline_bodies = try allocator.alloc(?Lifted.ExprId, decisions.len);
        errdefer allocator.free(inline_bodies);
        const kinds = try allocator.alloc(InlineKind, decisions.len);
        errdefer allocator.free(kinds);
        for (decisions, inline_bodies, kinds) |decision, *body, *body_kind| {
            body.* = switch (decision) {
                .inline_body => |candidate| candidate.body,
                .unknown,
                .never,
                => null,
            };
            body_kind.* = if (decision == .inline_body) decision.inline_body.kind else .none;
        }

        allocator.free(decisions);
        analyzer.decisions = &.{};

        return .{
            .allocator = allocator,
            .inline_bodies = inline_bodies,
            .kinds = kinds,
        };
    }

    fn inlineCandidate(self: *const InlineAnalyzer, fn_id: Lifted.FnId) std.mem.Allocator.Error!?Candidate {
        if (self.rule == .provided_low_level) {
            if (!(self.solved.lifted.getFn(fn_id).source orelse return null).provides_low_level) return null;
            return if (try self.wrapperCandidate(fn_id)) |body| .{ .body = body, .kind = .wrapper } else null;
        }
        // A pack program inlines a wrapper exactly as any program does.
        if (try self.wrapperCandidate(fn_id)) |body| return .{ .body = body, .kind = .wrapper };
        if (self.rule == .source) {
            if (self.sourceSingleUseCandidate(fn_id)) |body| return .{ .body = body, .kind = .source_single_use };
            return null;
        }
        // A single-use body is lowered only at its call site, and a keyed
        // specialization a pack program offers must survive as a procedure.
        if (self.keep_keyed_specializations) {
            if (self.solved.lifted.getFn(fn_id).source) |template| {
                if (template.spec_key != null) return null;
            }
        }
        if (self.singleUseCandidate(fn_id)) |body| return .{ .body = body, .kind = .single_use };
        return null;
    }

    /// A body its module's source calls at one site. Every call to it in any
    /// program is that site, lowered in some specialization of the function
    /// containing it, so the decision needs no program-wide count.
    fn sourceSingleUseCandidate(self: *const InlineAnalyzer, fn_id: Lifted.FnId) ?Lifted.ExprId {
        const source_fn = self.solved.lifted.getFn(fn_id);
        const template = source_fn.source orelse return null;
        if (!template.single_source_call) return null;
        if (self.procedure_usage.get(fn_id).contains_return) return null;
        return switch (source_fn.body) {
            .roc => |body_expr| body_expr,
            .hosted => null,
        };
    }

    fn singleUseCandidate(self: *const InlineAnalyzer, fn_id: Lifted.FnId) ?Lifted.ExprId {
        const use = self.procedure_usage.get(fn_id);
        if (use.external_calls != 1 or use.value_refs != 0) return null;
        if (use.contains_return) return null;

        const call_expr_id = use.external_call_expr orelse
            Common.invariant("single-use function had no external call expression");
        const call_expr = self.solved.lifted.getExpr(call_expr_id);
        if (call_expr.data != .call_proc) {
            Common.invariant("single-use function external call was not a direct call expression");
        }
        const call = call_expr.data.call_proc;
        if (Lifted.localDirectCallee(call) != fn_id) {
            Common.invariant("single-use function external call changed callee");
        }
        if (call.is_cold) return null;

        const source_fn = self.solved.lifted.getFn(fn_id);
        const body = switch (source_fn.body) {
            .roc => |body_expr| body_expr,
            .hosted => return null,
        };
        return body;
    }

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
        fn_id: Lifted.FnId,
        candidate: Candidate,
        callees_start: u32,
        callees_end: u32,
    };

    const CandidateGraph = struct {
        nodes: std.ArrayList(CandidateNode) = .empty,
        callees: std.ArrayList(Lifted.FnId) = .empty,
        /// Per lifted function, its index in `nodes`.
        node_of: []?u32,

        fn deinit(self: *CandidateGraph, allocator: std.mem.Allocator) void {
            self.nodes.deinit(allocator);
            self.callees.deinit(allocator);
            allocator.free(self.node_of);
        }

        fn calleeNodes(self: *const CandidateGraph, node: u32) []const Lifted.FnId {
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
        const fn_count = self.solved.lifted.fnCount();
        var graph = CandidateGraph{ .node_of = try self.allocator.alloc(?u32, fn_count) };
        defer graph.deinit(self.allocator);
        @memset(graph.node_of, null);

        for (0..fn_count) |index| {
            const fn_id: Lifted.FnId = @fromBackingInt(@intCast(@as(u32, @intCast(index))));
            const candidate = try self.inlineCandidate(fn_id) orelse {
                self.decisions[index] = .never;
                continue;
            };
            const callees_start: u32 = @intCast(graph.callees.items.len);
            switch (try self.scanCandidateBody(candidate.body, &graph.callees)) {
                .closed => {},
                .open => {
                    graph.callees.shrinkRetainingCapacity(callees_start);
                    self.decisions[index] = .never;
                    continue;
                },
            }
            graph.node_of[index] = @intCast(graph.nodes.items.len);
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

    /// Prove that every break and continue in `body` is owned by a loop
    /// inside it, appending each procedure it calls directly to `callees`.
    fn scanCandidateBody(self: *InlineAnalyzer, body: Lifted.ExprId, callees: *std.ArrayList(Lifted.FnId)) std.mem.Allocator.Error!BodyShape {
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
                        if (Lifted.localDirectCallee(call)) |callee| try callees.append(self.allocator, callee);
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
        const unvisited = std.math.maxInt(u32);
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
        var next_order: u32 = 0;

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
                    const callee = graph.node_of[@backingInt(callee_fn)] orelse continue;
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
        fn_id: Lifted.FnId,
        states: []MaterializationState,
    ) std.mem.Allocator.Error!void {
        _ = try self.runMaterialization(.{ .resolve = fn_id }, states);
    }

    /// One step of the materialization proof. `resolve` settles a single-use
    /// candidate; `body_once` asks whether a body is lowered in exactly one
    /// place. Both follow unique call owners outward, so the chain of owners
    /// is held on an explicit stack.
    const MaterializationFrame = union(enum) {
        resolve: Lifted.FnId,
        body_once: Lifted.FnId,
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

                    const use = self.procedure_usage.get(fn_id);
                    const owner = use.external_call_owner orelse
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

                    const use = self.procedure_usage.get(fn_id);
                    const settled: ?bool = if (use.external_calls == 0)
                        true
                    else if (use.external_calls != 1)
                        false
                    else blk: {
                        const call_expr_id = use.external_call_expr orelse
                            Common.invariant("single-call inline owner had no external call expression");
                        const call_expr = self.solved.lifted.getExpr(call_expr_id);
                        if (call_expr.data != .call_proc) {
                            Common.invariant("single-call inline owner use was not a direct call expression");
                        }
                        if (call_expr.data.call_proc.is_cold) break :blk true;
                        if (use.value_refs != 0) break :blk false;
                        break :blk null;
                    };
                    if (settled) |once| {
                        states[index] = if (once) .once else .multiple;
                        _ = frames.pop();
                        result = once;
                        continue;
                    }
                    const owner = use.external_call_owner orelse
                        Common.invariant("single-call inline owner had no external call owner");
                    try frames.append(self.allocator, .{ .body_once = owner });
                },
            }
            result = null;
        }
        return result.?;
    }

    fn wrapperCandidate(self: *const InlineAnalyzer, fn_id: Lifted.FnId) std.mem.Allocator.Error!?Lifted.ExprId {
        if (self.solvedCaptureCount(fn_id) != 0) return null;
        return WrapperBody.wrapperBody(self.allocator, &self.solved.lifted, fn_id);
    }
};
