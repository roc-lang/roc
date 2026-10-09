//! Separates semantic-obligation discovery from executable procedure demand.
//!
//! The producer supplies complete, exact instantiated dependency summaries.
//! This module never resolves a callable from its type or reads source syntax.
//! Its retained plan lets runtime continuation add demand without repeating
//! semantic discovery or changing the compile-time execution closure.

const std = @import("std");

const Allocator = std.mem.Allocator;

/// Session-local address of an exact template/type/evidence/capture instance.
/// The producer owns the structural identity and its instantiated type graph.
pub const InstanceId = enum(u32) { _ };

/// Why an already-resolved instance contributes to an instance's semantics.
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

pub const Dependency = struct {
    instance: InstanceId,
    kind: DependencyKind,
};

pub const ObligationKind = enum {
    literal_conversion,
    static_dispatch,
    codec,
    empirical_match,
    empirical_destructure,
    pairing,
};

/// The producer retains the checked owner, span, and report destination behind
/// this address. Two specializations of one source site have distinct records.
pub const ObligationId = enum(u32) { _ };

pub const Obligation = struct {
    id: ObligationId,
    kind: ObligationKind,
    /// Null for an obligation discharged by exact type/evidence validation.
    /// Otherwise this is the exact procedure instance the evaluator must run.
    execution: ?InstanceId,
};

/// A complete upstream proof. Every dependency is resolved under the exact
/// instance substitution and callable environment, including function values
/// delivered through aggregates and captures. An unresolved callable is not a
/// complete summary and must not be published through this API.
pub const CompleteSummary = struct {
    dependencies: []const Dependency,
    obligations: []const Obligation,
};

/// Borrowed immutable summaries; all identities and report records remain
/// producer-owned and must outlive the plan.
pub const CompleteGraph = struct {
    instances: []const CompleteSummary,
};

pub const Demand = struct {
    semantic: bool = false,
    evaluation: bool = false,
    runtime: bool = false,
};

pub const Metrics = struct {
    semantic_instances: usize = 0,
    evaluation_instances: usize = 0,
    runtime_instances: usize = 0,
    obligation_instances: usize = 0,
};

const Lane = enum { semantic, evaluation, runtime };
const Pending = struct { instance: InstanceId, lane: Lane };

/// Owns demand state, not a prepared body graph. Each instance is visited once
/// per declared demand lane, even through recursive value-flow edges.
pub const Plan = struct {
    allocator: Allocator,
    graph: CompleteGraph,
    demands: []Demand,
    obligations: std.ArrayList(Obligation),
    metrics: Metrics = .{},
    failed: bool = false,

    pub fn init(
        allocator: Allocator,
        graph: CompleteGraph,
        semantic_roots: []const InstanceId,
        evaluation_roots: []const InstanceId,
    ) Allocator.Error!Plan {
        const demands = try allocator.alloc(Demand, graph.instances.len);
        @memset(demands, .{});
        var plan = Plan{
            .allocator = allocator,
            .graph = graph,
            .demands = demands,
            .obligations = .empty,
        };
        errdefer plan.deinit();
        try plan.extend(semantic_roots, .semantic);
        try plan.extend(evaluation_roots, .evaluation);
        return plan;
    }

    pub fn deinit(self: *Plan) void {
        self.allocator.free(self.demands);
        self.obligations.deinit(self.allocator);
        self.* = undefined;
    }

    /// Continue the declared producer. Evaluation demand and its observations
    /// are immutable under runtime demand; no semantic summary is revisited.
    pub fn continueRuntime(self: *Plan, roots: []const InstanceId) Allocator.Error!void {
        if (self.failed) @panic("failed evaluation demand plan cannot be continued");
        for (roots) |root| {
            if (!self.demand(root).semantic) @panic("runtime continuation was not declared under semantic demand");
        }
        try self.extend(roots, .runtime);
    }

    pub fn demand(self: *const Plan, instance: InstanceId) Demand {
        if (self.failed) @panic("failed evaluation demand plan cannot be observed");
        const index = @intFromEnum(instance);
        if (index >= self.demands.len) @panic("evaluation demand referenced an unpublished instance");
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
            const index = @intFromEnum(item.instance);
            if (index >= self.graph.instances.len) @panic("complete demand summary referenced an unpublished instance");
            const state = &self.demands[index];
            const visited = switch (item.lane) {
                .semantic => &state.semantic,
                .evaluation => &state.evaluation,
                .runtime => &state.runtime,
            };
            if (visited.*) continue;
            // Reserve all output before committing the visit. On allocation
            // failure no instance is marked visited without its observations.
            const summary = self.graph.instances[index];
            const extra = switch (item.lane) {
                .semantic => summary.obligations.len,
                .evaluation => 1,
                .runtime => 0,
            };
            try pending.ensureUnusedCapacity(self.allocator, summary.dependencies.len + extra);
            if (item.lane == .semantic) {
                try self.obligations.ensureUnusedCapacity(self.allocator, summary.obligations.len);
            }
            visited.* = true;
            switch (item.lane) {
                .semantic => {
                    self.metrics.semantic_instances += 1;
                    for (summary.obligations) |obligation| {
                        self.obligations.appendAssumeCapacity(obligation);
                        self.metrics.obligation_instances += 1;
                        if (obligation.execution) |instance| {
                            pending.appendAssumeCapacity(.{ .instance = instance, .lane = .evaluation });
                        }
                    }
                },
                .evaluation => {
                    self.metrics.evaluation_instances += 1;
                    pending.appendAssumeCapacity(.{ .instance = item.instance, .lane = .semantic });
                },
                .runtime => self.metrics.runtime_instances += 1,
            }
            for (summary.dependencies) |dependency| {
                pending.appendAssumeCapacity(.{ .instance = dependency.instance, .lane = item.lane });
            }
        }
    }
};

test "evaluation demand discovers obligations without executing runtime procedures" {
    const edge = [_]Dependency{.{ .instance = @enumFromInt(1), .kind = .direct_call }};
    const obligations = [_]Obligation{.{
        .id = @enumFromInt(0),
        .kind = .literal_conversion,
        .execution = @enumFromInt(2),
    }};
    const graph = [_]CompleteSummary{
        .{ .dependencies = &edge, .obligations = &.{} },
        .{ .dependencies = &.{}, .obligations = &obligations },
        .{ .dependencies = &.{}, .obligations = &.{} },
        .{ .dependencies = &.{}, .obligations = &.{} },
    };
    var plan = try Plan.init(std.testing.allocator, .{ .instances = &graph }, &.{@enumFromInt(0)}, &.{});
    defer plan.deinit();
    try std.testing.expect(plan.demand(@enumFromInt(0)).semantic);
    try std.testing.expect(!plan.demand(@enumFromInt(0)).evaluation);
    try std.testing.expect(plan.demand(@enumFromInt(1)).semantic);
    try std.testing.expect(!plan.demand(@enumFromInt(1)).evaluation);
    try std.testing.expect(plan.demand(@enumFromInt(2)).evaluation);
    try std.testing.expect(!plan.demand(@enumFromInt(3)).semantic);
    try std.testing.expectEqual(@as(usize, 1), plan.obligations.items.len);
}

test "evaluation demand preserves every exact callable transfer and recursive obligation" {
    const kinds = std.enums.values(DependencyKind);
    var dependencies: [kinds.len][1]Dependency = undefined;
    var graph: [kinds.len + 1]CompleteSummary = undefined;
    for (kinds, 0..) |kind, index| {
        dependencies[index][0] = .{ .instance = @enumFromInt(index + 1), .kind = kind };
        graph[index] = .{ .dependencies = &dependencies[index], .obligations = &.{} };
    }
    const recursion = [_]Dependency{.{ .instance = @enumFromInt(0), .kind = .direct_call }};
    const obligations = [_]Obligation{.{
        .id = @enumFromInt(0),
        .kind = .empirical_destructure,
        .execution = @enumFromInt(kinds.len),
    }};
    graph[kinds.len] = .{ .dependencies = &recursion, .obligations = &obligations };
    var plan = try Plan.init(std.testing.allocator, .{ .instances = &graph }, &.{@enumFromInt(0)}, &.{});
    defer plan.deinit();
    try std.testing.expectEqual(graph.len, plan.metrics.semantic_instances);
    try std.testing.expectEqual(graph.len, plan.metrics.evaluation_instances);
    try std.testing.expectEqual(@as(usize, 1), plan.metrics.obligation_instances);
}

test "evaluation demand runtime continuation does not repeat discovery" {
    const edge = [_]Dependency{.{ .instance = @enumFromInt(1), .kind = .capture }};
    const obligations = [_]Obligation{.{
        .id = @enumFromInt(0),
        .kind = .pairing,
        .execution = @enumFromInt(1),
    }};
    const graph = [_]CompleteSummary{
        .{ .dependencies = &edge, .obligations = &obligations },
        .{ .dependencies = &.{}, .obligations = &.{} },
    };
    var plan = try Plan.init(std.testing.allocator, .{ .instances = &graph }, &.{@enumFromInt(0)}, &.{});
    defer plan.deinit();
    const before = plan.metrics;
    try plan.continueRuntime(&.{@enumFromInt(0)});
    try plan.continueRuntime(&.{@enumFromInt(0)});
    try std.testing.expectEqual(before.semantic_instances, plan.metrics.semantic_instances);
    try std.testing.expectEqual(before.evaluation_instances, plan.metrics.evaluation_instances);
    try std.testing.expectEqual(before.obligation_instances, plan.metrics.obligation_instances);
    try std.testing.expectEqual(@as(usize, 2), plan.metrics.runtime_instances);
}

test "evaluation demand releases every failed producer session" {
    const Case = struct {
        fn run(allocator: Allocator) !void {
            const edge = [_]Dependency{.{ .instance = @enumFromInt(1), .kind = .result }};
            const obligations = [_]Obligation{.{
                .id = @enumFromInt(0),
                .kind = .literal_conversion,
                .execution = @enumFromInt(1),
            }};
            const graph = [_]CompleteSummary{
                .{ .dependencies = &edge, .obligations = &obligations },
                .{ .dependencies = &.{}, .obligations = &.{} },
            };
            var plan = try Plan.init(allocator, .{ .instances = &graph }, &.{@enumFromInt(0)}, &.{});
            defer plan.deinit();
            try plan.continueRuntime(&.{@enumFromInt(0)});
            try std.testing.expectEqual(@as(usize, 2), plan.metrics.semantic_instances);
            try std.testing.expectEqual(@as(usize, 1), plan.metrics.evaluation_instances);
        }
    };
    try std.testing.checkAllAllocationFailures(std.testing.allocator, Case.run, .{});
}
