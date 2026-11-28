//! Checked root graph tests over checked programs built from Roc source.
//! Building a checked program runs the whole front end, so these tests live
//! in `compiler_suites_test`, which links the whole compiler, rather than in
//! the `eval` module's own tests.

const std = @import("std");
const base = @import("base");
const check = @import("check");
const eval = @import("eval");

const Allocator = std.mem.Allocator;
const checked = check.CheckedArtifact;
const static_dispatch = check.StaticDispatchRegistry;
const Inspected = eval.Inspected;
const graph_mod = eval.checked_root_graph;
const Root = graph_mod.Root;
const TemplateKey = graph_mod.TemplateKey;
const Walker = graph_mod.Walker;
const FunctionTypeMemo = graph_mod.FunctionTypeMemo;
const rootHoldsFunction = graph_mod.rootHoldsFunction;

test "Boxy root graph shares cyclic summaries across roots" {
    const allocator = std.testing.allocator;
    var parsed = try Inspected.parseProgram(allocator, .module,
        \\module [first, second, third]
        \\import Helpers
        \\first = Helpers.read(2.I64)
        \\second = Helpers.read(3.I64)
        \\third = Helpers.clean(4.I64)
    , &.{
        .{ .name = "Helpers", .source =
        \\module [read, clean]
        \\make = |n| |x| x + n
        \\stored = make(9.I64)
        \\read : I64 -> I64
        \\read = |n| if n == 0 stored(0.I64) else again(n - 1)
        \\again : I64 -> I64
        \\again = |n| read(n)
        \\clean : I64 -> I64
        \\clean = |n| n + 1
        },
        .{ .name = "Unused", .source =
        \\module [unused]
        \\unused = |x| x
        },
    }, .{ .io = std.testing.io, .roots = .published_only });
    defer parsed.deinit(allocator);
    try std.testing.expectEqual(@as(usize, 0), parsed.checker.problems.problems.items.len);
    const consumer = &parsed.checked_artifact;
    const views = try allocator.alloc(checked.ImportedModuleView, parsed.import_artifacts.len + 1);
    defer allocator.free(views);
    views[0] = checked.importedView(consumer);
    for (parsed.import_artifacts, views[1..]) |*artifact, *view| view.* = checked.importedView(artifact);
    try std.testing.expectEqual(@as(usize, 3), consumer.root_requests.compile_time_requests.items().len);
    var roots: [3]Root = undefined;
    for (&roots, consumer.root_requests.compile_time_requests.items()) |*root, request| {
        root.* = .{ .module = 0, .root = request.compile_time_root.get().?, .request = request };
    }
    const artifacts = try allocator.alloc(*const checked.CheckedModuleArtifact, views.len);
    defer allocator.free(artifacts);
    artifacts[0] = consumer;
    for (parsed.import_artifacts, artifacts[1..]) |*artifact, *out| out.* = artifact;
    var graph = try graph_mod.build(allocator, artifacts, &roots);
    defer graph.deinit(allocator);
    // The imported values are visible, not roots this batch owns or schedules.
    for (graph.deps) |deps| try std.testing.expectEqual(@as(usize, 0), deps.len);

    // Construct cold summaries in both orders, not only replay them backwards.
    for ([_]bool{ false, true }) |reverse| {
        var walker = try Walker.initViews(allocator, views, &roots);
        defer walker.deinit();
        for (0..roots.len) |position| {
            const ordinal = if (reverse) roots.len - 1 - position else position;
            const deps = try walker.dependencies(roots[ordinal], @intCast(ordinal));
            defer allocator.free(deps);
            try std.testing.expectEqualSlices(u32, graph.deps[ordinal], deps);
        }
        const decoded = walker.summaries.count();
        const edges = walker.summary_templates.items.len;
        try std.testing.expect(decoded > 0);
        for (0..32) |round| {
            for (0..roots.len) |position| {
                const ordinal = if (round % 2 == 0) position else roots.len - 1 - position;
                const deps = try walker.dependencies(roots[ordinal], @intCast(ordinal));
                defer allocator.free(deps);
                try std.testing.expectEqualSlices(u32, graph.deps[ordinal], deps);
            }
        }
        try std.testing.expectEqual(decoded, walker.summaries.count());
        try std.testing.expectEqual(edges, walker.summary_templates.items.len);
        // Unused is visible but its template bodies are never decoded.
        for (walker.visited[views.len - 1]) |stamp| try std.testing.expectEqual(@as(u32, 0), stamp);
    }

    const FailureCase = struct {
        fn run(a: Allocator, modules: []const *const checked.CheckedModuleArtifact, requested: []const Root) Allocator.Error!void {
            var candidate = try graph_mod.build(a, modules, requested);
            defer candidate.deinit(a);
        }
    };
    try std.testing.checkAllAllocationFailures(allocator, FailureCase.run, .{ @as([]const *const checked.CheckedModuleArtifact, artifacts), @as([]const Root, &roots) });
}

test "CTFE root classification resolves nominal backings throughout the checked program" {
    const allocator = std.testing.allocator;
    for ([_]struct { source: []const u8, holds_function: bool }{
        .{ .source =
        \\Holder := { value : I64 }.{
        \\    make : I64 -> Holder
        \\    make = |n| { value: n }
        \\}
        , .holds_function = false },
        .{ .source =
        \\Holder := { call : I64 -> I64 }.{
        \\    make : I64 -> Holder
        \\    make = |n| { call: |x| x + n }
        \\}
        , .holds_function = true },
    }) |case| {
        var parsed = try Inspected.parseProgram(allocator, .module,
            \\module [value]
            \\import Holder
            \\value : Holder
            \\value = Holder.make(7)
        , &.{.{ .name = "Holder", .source = case.source }}, .{ .io = std.testing.io, .roots = .published_only });
        defer parsed.deinit(allocator);
        try std.testing.expectEqual(@as(usize, 0), parsed.checker.problems.problems.items.len);
        const consumer = &parsed.checked_artifact;
        // The import artifacts list the builtin module, then the program's
        // imports in order; `Holder` is the program's only import.
        try std.testing.expectEqual(@as(usize, 2), parsed.import_artifacts.len);
        const holder = &parsed.import_artifacts[1];
        try std.testing.expectEqual(@as(usize, 1), consumer.root_requests.compile_time_requests.items().len);
        const request = consumer.root_requests.compile_time_requests.items()[0];
        // A nominal can be supplied by the ownership anchor, a platform
        // relation, or an ordinary import. None needs a pending CTFE root.
        const root: Root = .{ .module = 0, .root = request.compile_time_root.get().?, .request = request };
        // Each program configuration gets its own memo, so each call walks.
        var memos: [3]FunctionTypeMemo = .{ .{}, .{}, .{} };
        defer for (&memos) |*memo| memo.deinit(allocator);
        try std.testing.expectEqual(case.holds_function, try rootHoldsFunction(allocator, &memos[0], &.{consumer}, .{ .root = checked.loweringView(holder), .imports = &.{checked.importedView(consumer)} }, root));
        try std.testing.expectEqual(case.holds_function, try rootHoldsFunction(allocator, &memos[1], &.{consumer}, .{ .root = checked.loweringViewWithRelations(consumer, &.{checked.importedView(holder)}) }, root));
        try std.testing.expectEqual(case.holds_function, try rootHoldsFunction(allocator, &memos[2], &.{consumer}, .{ .root = checked.loweringView(consumer), .imports = &.{checked.importedView(holder)} }, root));
    }
}

test "a publication's strategy alone decides which evaluator stores a function-holding root" {
    const allocator = std.testing.allocator;
    for ([_]base.SpecializationStrategy{ .lss, .boxy }) |strategy| {
        var parsed = try Inspected.parseProgram(allocator, .module,
            \\module [table, plain, total]
            \\add_one : I64 -> I64
            \\add_one = |x| x + 1
            \\table : List(I64 -> I64)
            \\table = [add_one, |x| x * 2]
            \\plain : I64
            \\plain = 40 + 2
            \\total : I64
            \\total = table.fold(plain, |acc, f| f(acc))
        , &.{}, .{ .io = std.testing.io, .roots = .published_only, .strategy = strategy });
        defer parsed.deinit(allocator);
        try std.testing.expectEqual(@as(usize, 0), parsed.checker.problems.problems.items.len);
        const module = &parsed.checked_artifact;
        const views = try allocator.alloc(checked.ImportedModuleView, parsed.import_artifacts.len);
        defer allocator.free(views);
        for (parsed.import_artifacts, views) |*artifact, *view| view.* = checked.importedView(artifact);
        const other: base.SpecializationStrategy = switch (strategy) {
            .lss => .boxy,
            .boxy => .lss,
        };
        var constants: usize = 0;
        var holding: usize = 0;
        var memo: FunctionTypeMemo = .{};
        defer memo.deinit(allocator);
        for (module.root_requests.compile_time_requests.items()) |request| {
            const root_id = request.compile_time_root.get().?;
            const root = module.compile_time_roots.root(root_id);
            if (root.kind != .constant) continue;
            constants += 1;
            const holds = try rootHoldsFunction(allocator, &memo, &.{module}, .{ .root = checked.loweringView(module), .imports = views }, .{ .module = 0, .root = root_id, .request = request });
            if (holds) holding += 1;
            // The publication's strategy has every value.
            try std.testing.expect(!root.needsEvaluation(strategy));
            // A value holding no function is the same value under either
            // strategy, whichever evaluator stored it, a reader of the
            // function-holding root's among them. A function value exists
            // only in the representation of the evaluator that stored it.
            try std.testing.expectEqual(holds, root.needsEvaluation(other));
            try std.testing.expectEqual(holds and strategy == .boxy, root.boxy_value != null);
            try std.testing.expectEqual(holds and strategy == .boxy, root.payload == .unevaluated);
        }
        try std.testing.expectEqual(@as(usize, 3), constants);
        try std.testing.expectEqual(@as(usize, 1), holding);
    }
}

fn expectSharedDependencies(source: []const u8, imports: []const Inspected.ModuleSource) (Inspected.Error || Allocator.Error || error{ TestExpectedEqual, TestUnexpectedResult })!void {
    const allocator = std.testing.allocator;
    var parsed = try Inspected.parseProgram(allocator, .module, source, imports, .{ .io = std.testing.io, .roots = .published_only });
    defer parsed.deinit(allocator);
    try std.testing.expectEqual(@as(usize, 0), parsed.checker.problems.problems.items.len);
    var modules = std.ArrayList(*checked.CheckedModuleArtifact).empty;
    defer modules.deinit(allocator);
    try modules.append(allocator, &parsed.checked_artifact);
    for (parsed.import_artifacts) |*artifact| try modules.append(allocator, artifact);
    var roots = std.ArrayList(Root).empty;
    defer roots.deinit(allocator);
    for (modules.items, 0..) |module, index| {
        for (module.root_requests.compile_time_requests.items()) |request| {
            try roots.append(allocator, .{ .module = @intCast(index), .root = request.compile_time_root.get().?, .request = request });
        }
    }
    try std.testing.expect(roots.items.len >= 3);
    // Evidence can be shared between templates. Each summary must decode
    // it independently even when both templates belong to one root walk.
    var evidence_probe = try Walker.init(allocator, modules.items, roots.items);
    defer evidence_probe.deinit();
    evidence_probe.visit = 1;
    var probed_evidence = false;
    evidence: for (modules.items, 0..) |module, module_index| {
        for (module.static_dispatch_plans.evidence_nodes.items(), 0..) |node, node_index| {
            if (node.target.kind != .procedure or evidence_probe.moduleOf(node.target.kind.procedure.template.artifact) == null) continue;
            const id: static_dispatch.EvidenceNodeId = @fromBackingInt(@intCast(node_index));
            evidence_probe.evidence_visit = 1;
            try evidence_probe.nodes.append(allocator, id);
            try evidence_probe.drainNodes(@intCast(module_index));
            try std.testing.expect(evidence_probe.pending.items.len > 0);
            const targets = try allocator.dupe(TemplateKey, evidence_probe.pending.items);
            defer allocator.free(targets);
            const reads = try allocator.dupe(u32, evidence_probe.reads.items);
            defer allocator.free(reads);
            evidence_probe.pending.clearRetainingCapacity();
            evidence_probe.reads.clearRetainingCapacity();
            evidence_probe.evidence_visit = 2;
            try evidence_probe.nodes.append(allocator, id);
            try evidence_probe.drainNodes(@intCast(module_index));
            try std.testing.expectEqualSlices(TemplateKey, targets, evidence_probe.pending.items);
            try std.testing.expectEqualSlices(u32, reads, evidence_probe.reads.items);
            probed_evidence = true;
            break :evidence;
        }
    }
    try std.testing.expect(probed_evidence);
    var shared = try Walker.init(allocator, modules.items, roots.items);
    defer shared.deinit();
    var expected = std.ArrayList([]const u32).empty;
    defer {
        for (expected.items) |deps| allocator.free(deps);
        expected.deinit(allocator);
    }
    var total_reads: usize = 0;
    for (roots.items, 0..) |root, ordinal| {
        var isolated = try Walker.init(allocator, modules.items, roots.items);
        defer isolated.deinit();
        const deps = try isolated.dependencies(root, @intCast(ordinal));
        expected.append(allocator, deps) catch |err| {
            allocator.free(deps);
            return err;
        };
        total_reads += deps.len;
        const actual = try shared.dependencies(root, @intCast(ordinal));
        defer allocator.free(actual);
        try std.testing.expectEqualSlices(u32, deps, actual);
        for (actual) |dependency| try std.testing.expect(dependency != ordinal);
    }
    try std.testing.expect(total_reads > 0);
    const decoded = shared.summaries.count();
    const templates_len = shared.summary_templates.items.len;
    const reads_len = shared.summary_reads.items.len;
    try std.testing.expect(decoded > 0);
    // A second traversal in reverse root order must reuse complete summaries,
    // including evidence that another template happened to visit first.
    var ordinal = roots.items.len;
    while (ordinal > 0) {
        ordinal -= 1;
        const actual = try shared.dependencies(roots.items[ordinal], @intCast(ordinal));
        defer allocator.free(actual);
        try std.testing.expectEqualSlices(u32, expected.items[ordinal], actual);
    }
    try std.testing.expectEqual(decoded, shared.summaries.count());
    try std.testing.expectEqual(templates_len, shared.summary_templates.items.len);
    try std.testing.expectEqual(reads_len, shared.summary_reads.items.len);
}

test "checked dependency summaries preserve shared calls, evidence and literal conversions" {
    try expectSharedDependencies(
        \\module [first, second]
        \\base = 37.I64
        \\shared : I64 -> I64
        \\shared = |x| base + x
        \\left = |x| shared(x)
        \\right = |x| shared(x)
        \\first = left(1.I64)
        \\second = right(2.I64)
    , &.{});
}

test "checked dependency summaries terminate mutual recursion and preserve imported reads" {
    try expectSharedDependencies(
        \\module [first, second]
        \\import Helpers
        \\first = Helpers.down(3.I64)
        \\second = Helpers.down(4.I64)
    , &.{.{ .name = "Helpers", .source =
        \\module [down]
        \\base = 37.I64
        \\down : I64 -> I64
        \\down = |n| if n == 0 base else bounce(n - 1)
        \\bounce : I64 -> I64
        \\bounce = |n| down(n)
    }});
}

test "checked dependency summaries preserve function-valued constant reads" {
    try expectSharedDependencies(
        \\module [first, second]
        \\base = 37.I64
        \\add : I64 -> I64
        \\add = |n| base + n
        \\sub : I64 -> I64
        \\sub = |n| base - n
        \\choose : Bool -> (I64 -> I64)
        \\choose = |pick_add| if pick_add add else sub
        \\chosen = choose(Bool.True)
        \\first = chosen(1.I64)
        \\second = chosen(2.I64)
    , &.{});
}
