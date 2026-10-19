//! Separates checked-plan discovery from procedure execution demand.
//!
//! The producer supplies complete, exact instantiated dependency summaries.
//! This module never resolves a callable from its type or reads source syntax.
//! Its retained plan lets runtime continuation add demand without repeating
//! checked discovery or changing the compile-time execution closure.

const std = @import("std");
const base = @import("base");

const Allocator = std.mem.Allocator;

/// Session-local address of an exact template/type/evidence/capture instance.
/// The producer owns the structural identity and its instantiated type graph.
pub const InstanceId = enum(u32) { _ };

/// Why an already-resolved instance contributes to an instance's meaning.
/// Transfers retain their exact checked value-flow relation upstream; these
/// tags are observations, not permission to infer a callee.
pub const DependencyKind = enum {
    direct_call,
    parameter,
    result,
    record_field,
    tuple_element,
    lambda,
    capture,
    local_binding,
    constant_read,
    static_dispatch,
};

/// An instance this instance depends on, and the relation that makes it a dependency.
pub const Dependency = struct {
    instance: InstanceId,
    kind: DependencyKind,
};

/// The check a `CheckedPlan` requires.
pub const CheckedPlanKind = enum {
    literal_conversion,
    static_dispatch,
    codec,
    empirical_match,
    empirical_destructure,
    pairing,
};

/// The producer retains the checked owner, span, and report destination behind
/// this address. Two specializations of one source site have distinct records.
pub const CheckedPlanId = enum(u32) { _ };

/// A checked plan an instance contributes when checked demand reaches it.
pub const CheckedPlan = struct {
    id: CheckedPlanId,
    kind: CheckedPlanKind,
    /// Null for a checked plan discharged by exact type/evidence validation.
    /// Otherwise this is the exact procedure instance the evaluator must run.
    execution: ?InstanceId,
};

/// A complete upstream proof. Every dependency is resolved under the exact
/// instance substitution and callable environment, including function values
/// delivered through aggregates and captures. An unresolved callable is not a
/// complete summary and must not be recorded through this API.
pub const CompleteSummary = struct {
    dependencies: []const Dependency,
    checked_plans: []const CheckedPlan,
};

/// Borrowed immutable summaries; all identities and report records remain
/// producer-owned and must outlive the plan.
pub const CompleteGraph = struct {
    instances: []const CompleteSummary,
};

/// The demand lanes in which an instance has been visited.
pub const Demand = struct {
    checked: bool = false,
    evaluation: bool = false,
    runtime: bool = false,
};

/// Counts of instance visits per demand lane, and of checked_plans recorded.
pub const Metrics = struct {
    checked_instances: usize = 0,
    evaluation_instances: usize = 0,
    runtime_instances: usize = 0,
    checked_plan_instances: usize = 0,
};

const Lane = enum { checked, evaluation, runtime };
const Pending = struct { instance: InstanceId, lane: Lane };

/// Owns demand state, not a prepared body graph. Each instance is visited once
/// per declared demand lane, even through recursive value-flow edges.
pub const Plan = struct {
    allocator: Allocator,
    graph: CompleteGraph,
    demands: []Demand,
    checked_plans: std.ArrayList(CheckedPlan),
    metrics: Metrics = .{},
    failed: bool = false,

    pub fn init(
        allocator: Allocator,
        graph: CompleteGraph,
        checked_roots: []const InstanceId,
        evaluation_roots: []const InstanceId,
    ) Allocator.Error!Plan {
        const demands = try allocator.alloc(Demand, graph.instances.len);
        @memset(demands, .{});
        var plan = Plan{
            .allocator = allocator,
            .graph = graph,
            .demands = demands,
            .checked_plans = .empty,
        };
        errdefer plan.deinit();
        try plan.extend(checked_roots, .checked);
        try plan.extend(evaluation_roots, .evaluation);
        return plan;
    }

    pub fn deinit(self: *Plan) void {
        self.allocator.free(self.demands);
        self.checked_plans.deinit(self.allocator);
        self.* = undefined;
    }

    /// Continue the declared producer. Evaluation demand and its observations
    /// are immutable under runtime demand; no checked summary is revisited.
    pub fn continueRuntime(self: *Plan, roots: []const InstanceId) Allocator.Error!void {
        if (self.failed) base.invariant("failed evaluation demand plan cannot be continued", .{});
        for (roots) |root| {
            if (!self.demand(root).checked) base.invariant("runtime continuation was not declared under checked demand", .{});
        }
        try self.extend(roots, .runtime);
    }

    pub fn demand(self: *const Plan, instance: InstanceId) Demand {
        if (self.failed) base.invariant("failed evaluation demand plan cannot be observed", .{});
        const index = @backingInt(instance);
        if (index >= self.demands.len) base.invariant("evaluation demand referenced an unrecorded instance", .{});
        return self.demands[index];
    }

    fn extend(self: *Plan, roots: []const InstanceId, lane: Lane) Allocator.Error!void {
        // A resource failure terminates this producer session; callers must
        // release it rather than retrying a partially accepted demand queue.
        errdefer self.failed = true;
        var pending = std.ArrayList(Pending).empty;
        defer pending.deinit(self.allocator);
        for (roots) |instance| try pending.append(self.allocator, .{ .instance = instance, .lane = lane });
        while (pending.pop()) |item| {
            const index = @backingInt(item.instance);
            if (index >= self.graph.instances.len) base.invariant("complete demand summary referenced an unrecorded instance", .{});
            const state = &self.demands[index];
            const visited = switch (item.lane) {
                .checked => &state.checked,
                .evaluation => &state.evaluation,
                .runtime => &state.runtime,
            };
            if (visited.*) continue;
            // Reserve all output before committing the visit. On allocation
            // failure no instance is marked visited without its observations.
            const summary = self.graph.instances[index];
            const extra = switch (item.lane) {
                .checked => summary.checked_plans.len,
                .evaluation => 1,
                .runtime => 0,
            };
            try pending.ensureUnusedCapacity(self.allocator, summary.dependencies.len + extra);
            if (item.lane == .checked) {
                try self.checked_plans.ensureUnusedCapacity(self.allocator, summary.checked_plans.len);
            }
            visited.* = true;
            switch (item.lane) {
                .checked => {
                    self.metrics.checked_instances += 1;
                    for (summary.checked_plans) |checked_plan| {
                        self.checked_plans.appendAssumeCapacity(checked_plan);
                        self.metrics.checked_plan_instances += 1;
                        if (checked_plan.execution) |instance| {
                            pending.appendAssumeCapacity(.{ .instance = instance, .lane = .evaluation });
                        }
                    }
                },
                .evaluation => {
                    self.metrics.evaluation_instances += 1;
                    pending.appendAssumeCapacity(.{ .instance = item.instance, .lane = .checked });
                },
                .runtime => self.metrics.runtime_instances += 1,
            }
            for (summary.dependencies) |dependency| {
                pending.appendAssumeCapacity(.{ .instance = dependency.instance, .lane = item.lane });
            }
        }
    }
};

/// The instance ids of a test graph's summaries, in summary order.
fn testInstances(comptime count: usize) [count]InstanceId {
    var instances: [count]InstanceId = undefined;
    for (&instances, 0..) |*instance, index| instance.* = @fromBackingInt(@intCast(index));
    return instances;
}

test "evaluation demand discovers checked_plans without executing runtime procedures" {
    const root, const callee, const executed, const unreached = testInstances(4);
    const edge = [_]Dependency{.{ .instance = callee, .kind = .direct_call }};
    const checked_plans = [_]CheckedPlan{.{
        .id = @fromBackingInt(@intCast(5)),
        .kind = .literal_conversion,
        .execution = executed,
    }};
    const graph = [_]CompleteSummary{
        .{ .dependencies = &edge, .checked_plans = &.{} },
        .{ .dependencies = &.{}, .checked_plans = &checked_plans },
        .{ .dependencies = &.{}, .checked_plans = &.{} },
        .{ .dependencies = &.{}, .checked_plans = &.{} },
    };
    var plan = try Plan.init(std.testing.allocator, .{ .instances = &graph }, &.{root}, &.{});
    defer plan.deinit();
    try std.testing.expect(plan.demand(root).checked);
    try std.testing.expect(!plan.demand(root).evaluation);
    try std.testing.expect(plan.demand(callee).checked);
    try std.testing.expect(!plan.demand(callee).evaluation);
    try std.testing.expect(plan.demand(executed).evaluation);
    try std.testing.expect(!plan.demand(unreached).checked);
    try std.testing.expectEqual(@as(usize, 1), plan.checked_plans.items.len);
}

test "evaluation demand preserves every exact callable transfer and recursive checked_plan" {
    const kinds = std.enums.values(DependencyKind);
    const instances = testInstances(kinds.len + 1);
    var dependencies: [kinds.len][1]Dependency = undefined;
    var graph: [instances.len]CompleteSummary = undefined;
    for (kinds, 0..) |kind, index| {
        dependencies[index][0] = .{ .instance = instances[index + 1], .kind = kind };
        graph[index] = .{ .dependencies = &dependencies[index], .checked_plans = &.{} };
    }
    const root = instances[0];
    const recursive = instances[kinds.len];
    const recursion = [_]Dependency{.{ .instance = root, .kind = .direct_call }};
    const checked_plans = [_]CheckedPlan{.{
        .id = @fromBackingInt(@intCast(5)),
        .kind = .empirical_destructure,
        .execution = recursive,
    }};
    graph[kinds.len] = .{ .dependencies = &recursion, .checked_plans = &checked_plans };
    var plan = try Plan.init(std.testing.allocator, .{ .instances = &graph }, &.{root}, &.{});
    defer plan.deinit();
    try std.testing.expectEqual(graph.len, plan.metrics.checked_instances);
    try std.testing.expectEqual(graph.len, plan.metrics.evaluation_instances);
    try std.testing.expectEqual(@as(usize, 1), plan.metrics.checked_plan_instances);
}

test "evaluation demand runtime continuation does not repeat discovery" {
    const root, const captured = testInstances(2);
    const edge = [_]Dependency{.{ .instance = captured, .kind = .capture }};
    const checked_plans = [_]CheckedPlan{.{
        .id = @fromBackingInt(@intCast(5)),
        .kind = .pairing,
        .execution = captured,
    }};
    const graph = [_]CompleteSummary{
        .{ .dependencies = &edge, .checked_plans = &checked_plans },
        .{ .dependencies = &.{}, .checked_plans = &.{} },
    };
    var plan = try Plan.init(std.testing.allocator, .{ .instances = &graph }, &.{root}, &.{});
    defer plan.deinit();
    const before = plan.metrics;
    try plan.continueRuntime(&.{root});
    try plan.continueRuntime(&.{root});
    try std.testing.expectEqual(before.checked_instances, plan.metrics.checked_instances);
    try std.testing.expectEqual(before.evaluation_instances, plan.metrics.evaluation_instances);
    try std.testing.expectEqual(before.checked_plan_instances, plan.metrics.checked_plan_instances);
    try std.testing.expectEqual(@as(usize, 2), plan.metrics.runtime_instances);
}

test "evaluation demand releases every failed producer session" {
    const Case = struct {
        fn run(allocator: Allocator) (Allocator.Error || error{TestExpectedEqual})!void {
            const root, const result = testInstances(2);
            const edge = [_]Dependency{.{ .instance = result, .kind = .result }};
            const checked_plans = [_]CheckedPlan{.{
                .id = @fromBackingInt(@intCast(5)),
                .kind = .literal_conversion,
                .execution = result,
            }};
            const graph = [_]CompleteSummary{
                .{ .dependencies = &edge, .checked_plans = &checked_plans },
                .{ .dependencies = &.{}, .checked_plans = &.{} },
            };
            var plan = try Plan.init(allocator, .{ .instances = &graph }, &.{root}, &.{});
            defer plan.deinit();
            try plan.continueRuntime(&.{root});
            try std.testing.expectEqual(@as(usize, 2), plan.metrics.checked_instances);
            try std.testing.expectEqual(@as(usize, 1), plan.metrics.evaluation_instances);
        }
    };
    try std.testing.checkAllAllocationFailures(std.testing.allocator, Case.run, .{});
}
