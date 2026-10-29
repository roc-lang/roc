//! Productivity of SpecConstr's direct-call inlining.
//!
//! The value-aware clone simplifies a known value in exactly three places: a
//! `match` on a known tag selects its arm, a call of a known callable inlines
//! the callable, and a loop entered with a known constructor scalarizes it. A
//! constructed value also gains where it is read: a field or item read of a
//! known record or tuple takes the part without building the aggregate.
//! Inlining a direct call pays for its copied body only when it lets one of
//! those consumers meet a known value: a known part of an argument or
//! capture reaches a match, call, or loop in the callee, or the caller
//! consumes a part of the result that the callee constructs on every path that
//! returns.
//!
//! Before any clone runs, this analysis summarizes where each function
//! consumes its parameters and captures, what each call's surroundings consume
//! of its result, and which result parts each function constructs. The clone
//! combines those summaries with the values it knows at a call. See design.md
//! "SpecConstr And Loop Scalarization".

const std = @import("std");
const collections = @import("collections");
const Common = @import("../common.zig");
const Ast = @import("ast.zig");
const Mono = @import("../monotype/ast.zig");
const WrapperBody = @import("wrapper_body.zig");
const names = @import("check").CheckedNames;

const Allocator = std.mem.Allocator;
const GuardedList = collections.GuardedList;

/// A consumer that gains when it meets a known value.
pub const Consumer = enum {
    match,
    call,
    loop,
    /// A field or item read, or a record or tuple pattern. A call-pattern
    /// worker already takes a known record or tuple argument apart without
    /// inlining, so a field or item read makes an inline productive only when it
    /// reads the call's result.
    project,
};

/// The consumers a known argument or capture can meet in a callee.
pub const argument_consumers: Consumers = .initMany(&.{ .match, .call, .loop });
/// A set of consumers.
pub const Consumers = std.EnumSet(Consumer);

/// One step from a value into a part of it. Field and tag labels are
/// interned ids, so equal labels have equal ids.
pub const Step = struct {
    kind: Kind,
    label: u32,
    index: u32,

    pub const Kind = enum(u32) { field, item, payload };

    pub fn field(name: names.RecordFieldNameId) Step {
        return .{ .kind = .field, .label = @backingInt(name), .index = 0 };
    }

    pub fn item(index: u32) Step {
        return .{ .kind = .item, .label = 0, .index = index };
    }

    pub fn payload(tag: names.TagNameId, index: u32) Step {
        return .{ .kind = .payload, .label = @backingInt(tag), .index = index };
    }

    pub fn eql(a: Step, b: Step) bool {
        return a.kind == b.kind and a.label == b.label and a.index == b.index;
    }
};

/// Append `step` to a path held as steps. A value's parts recur only through
/// a tag payload, so a path that re-enters a payload step it already contains
/// names a recursive position, and folds to that step's first occurrence.
/// Every path is therefore bounded by the program's labels.
pub fn appendStep(allocator: Allocator, steps: *std.ArrayList(Step), step: Step) Allocator.Error!void {
    if (step.kind == .payload) {
        for (steps.items, 0..) |existing, index| {
            if (existing.eql(step)) {
                steps.shrinkRetainingCapacity(index + 1);
                return;
            }
        }
    }
    try steps.append(allocator, step);
}

/// An interned path of steps; `root` is the value itself.
pub const PathId = enum(u32) { root = 0, _ };

/// Interned paths, folded as `appendStep` folds them.
const PathTable = struct {
    nodes: std.ArrayList(Node) = .empty,
    children: std.AutoHashMapUnmanaged(ChildKey, PathId) = .empty,

    const Node = struct { parent: PathId, step: Step, depth: u32 };
    const ChildKey = struct { parent: PathId, step: Step };

    fn init(allocator: Allocator) Allocator.Error!PathTable {
        var table: PathTable = .{};
        try table.nodes.append(allocator, .{ .parent = .root, .step = Step.item(0), .depth = 0 });
        return table;
    }

    fn node(self: *const PathTable, path: PathId) Node {
        return self.nodes.items[@backingInt(path)];
    }

    /// The ancestor-or-self of `parent` that ends in `step`, when `step` is a
    /// payload step: the position a repeated payload step folds to.
    fn folded(self: *const PathTable, parent: PathId, step: Step) ?PathId {
        if (step.kind != .payload) return null;
        var path = parent;
        while (path != .root) : (path = self.node(path).parent) {
            if (self.node(path).step.eql(step)) return path;
        }
        return null;
    }

    fn child(self: *PathTable, allocator: Allocator, parent: PathId, step: Step) Allocator.Error!PathId {
        if (self.folded(parent, step)) |path| return path;
        const gop = try self.children.getOrPut(allocator, .{ .parent = parent, .step = step });
        if (!gop.found_existing) {
            gop.value_ptr.* = @fromBackingInt(@intCast(self.nodes.items.len));
            try self.nodes.append(allocator, .{ .parent = parent, .step = step, .depth = self.node(parent).depth + 1 });
        }
        return gop.value_ptr.*;
    }

    fn lookupChild(self: *const PathTable, parent: PathId, step: Step) ?PathId {
        if (self.folded(parent, step)) |path| return path;
        return self.children.get(.{ .parent = parent, .step = step });
    }

    fn lookup(self: *const PathTable, steps: []const Step) ?PathId {
        var path: PathId = .root;
        for (steps) |step| path = self.lookupChild(path, step) orelse return null;
        return path;
    }

    fn concat(self: *PathTable, allocator: Allocator, base: PathId, steps: []const Step) Allocator.Error!PathId {
        var path = base;
        for (steps) |step| path = try self.child(allocator, path, step);
        return path;
    }

    /// Replace `out` with the steps of `path`, root first.
    fn stepsOf(self: *const PathTable, allocator: Allocator, path: PathId, out: *std.ArrayList(Step)) Allocator.Error!void {
        const depth = self.node(path).depth;
        try out.resize(allocator, depth);
        var current = path;
        var index = depth;
        while (index > 0) {
            index -= 1;
            out.items[index] = self.node(current).step;
            current = self.node(current).parent;
        }
    }

    /// Replace `out` with the steps of `path` below `prefix`, root first, or
    /// return false when `prefix` is not an ancestor-or-self of `path`.
    fn stepsBelow(self: *const PathTable, allocator: Allocator, path: PathId, prefix: PathId, out: *std.ArrayList(Step)) Allocator.Error!bool {
        const depth = self.node(path).depth;
        const prefix_depth = self.node(prefix).depth;
        if (prefix_depth > depth) return false;
        try out.resize(allocator, depth - prefix_depth);
        var current = path;
        var index = depth - prefix_depth;
        while (index > 0) {
            index -= 1;
            out.items[index] = self.node(current).step;
            current = self.node(current).parent;
        }
        return current == prefix;
    }
};

/// A path of a value read by `consumers`.
pub const Read = struct { path: PathId, consumers: Consumers };

/// The value's part at `value` sits at `result` of the enclosing function's
/// result, so whatever reads that result part reads this value part.
pub const Placement = struct { value: PathId, result: PathId };

/// What the code around a value consumes of it.
pub const DemandSet = struct {
    /// Each read path once, in first-read order, with its consumers at the
    /// same position. A value's distinct read paths are few, so a scan finds
    /// a path's position.
    read_paths: std.ArrayList(PathId) = .empty,
    read_consumers: std.ArrayList(Consumers) = .empty,
    placements: std.AutoArrayHashMapUnmanaged(Placement, void) = .empty,

    fn addRead(self: *DemandSet, allocator: Allocator, path: PathId, consumers: Consumers) Allocator.Error!bool {
        if (std.mem.findScalar(PathId, self.read_paths.items, path)) |index| {
            const existing = &self.read_consumers.items[index];
            if (existing.supersetOf(consumers)) return false;
            existing.setUnion(consumers);
            return true;
        }
        try self.read_paths.append(allocator, path);
        try self.read_consumers.append(allocator, consumers);
        return true;
    }

    fn removeReadAt(self: *DemandSet, index: usize) void {
        _ = self.read_paths.swapRemove(index);
        _ = self.read_consumers.swapRemove(index);
    }

    fn addPlacement(self: *DemandSet, allocator: Allocator, placement: Placement) Allocator.Error!bool {
        const gop = try self.placements.getOrPut(allocator, placement);
        return !gop.found_existing;
    }

    fn addAll(self: *DemandSet, allocator: Allocator, other: Demand) Allocator.Error!bool {
        var changed = false;
        for (other.read_paths, other.read_consumers) |path, consumers| {
            if (try self.addRead(allocator, path, consumers)) changed = true;
        }
        for (other.placements) |placement| {
            if (try self.addPlacement(allocator, placement)) changed = true;
        }
        return changed;
    }

    pub fn view(self: *const DemandSet) Demand {
        return .{
            .read_paths = self.read_paths.items,
            .read_consumers = self.read_consumers.items,
            .placements = self.placements.keys(),
        };
    }
};

/// An immutable view of a `DemandSet`.
pub const Demand = struct {
    read_paths: []const PathId,
    read_consumers: []const Consumers,
    placements: []const Placement,

    pub const none: Demand = .{ .read_paths = &.{}, .read_consumers = &.{}, .placements = &.{} };
};

/// A read of a value whose placements in an enclosing result are resolved,
/// as the clone sees it at one call.
pub const ResolvedRead = struct { steps: []const Step, consumers: Consumers };

/// A parameter or capture of a function.
pub const Slot = struct {
    kind: Kind,
    index: u32,

    pub const Kind = enum(u32) { arg, capture };
};

/// On every path that returns normally, the function's result part at
/// `result` is the part at `value` of its parameter or capture `slot`.
pub const PassThrough = struct { result: PathId, slot: Slot, value: PathId };

/// How one function consumes its parameters and captures and which parts
/// of its result it constructs.
pub const FnSummary = struct {
    /// How the body consumes each parameter, by the function's arguments.
    params: []DemandSet,
    /// How the body consumes each capture, by the function's capture slots.
    captures: []DemandSet,
    /// Result parts the function constructs on every path that returns
    /// normally, with the consumers a known constructor there serves.
    constructed: []const Read = &.{},
    passes: []const PassThrough = &.{},
    /// The body does less work than a call to it costs (`WrapperBody`), so
    /// inlining it never grows the program. It lies on no cycle of direct
    /// calls, where inlining it would only unroll the recursion, and passes
    /// none of its calls a value it builds where the callee consumes it, which
    /// would inline that callee into every copy of the body.
    wrapper: bool = false,
};

/// The productivity summaries of every function in a program, computed
/// before any clone runs.
pub const Analysis = struct {
    arena: collections.SingleThreadArena,
    program: *const Ast.Program,
    paths: PathTable,
    fns: []FnSummary,
    /// What each direct or value call's surroundings in a source body consume
    /// of its result.
    call_demand: collections.DenseMapUnmanaged(Ast.ExprId, DemandSet) = .empty,

    pub fn deinit(self: *Analysis) void {
        self.arena.deinit();
    }

    pub fn summary(self: *const Analysis, fn_id: Ast.FnId) *const FnSummary {
        const raw = @backingInt(fn_id);
        if (raw >= self.fns.len) Common.invariant("inline productivity was queried for a function created after it was analyzed");
        return &self.fns[raw];
    }

    pub fn callDemand(self: *const Analysis, call: Ast.ExprId) Demand {
        const set = self.call_demand.getPtrConst(call) orelse return .none;
        return set.view();
    }

    /// The capture slot of `fn_id` that receives capture operand `id`.
    pub fn captureSlot(self: *const Analysis, fn_id: Ast.FnId, id: anytype) usize {
        const slots = self.program.typedLocalSpan(self.program.getFn(fn_id).captures);
        for (0..slots.len) |slot| {
            if (self.program.captureIdOfLocal(GuardedList.at(slots, slot).local) == id) return slot;
        }
        Common.invariant("capture operand named no capture slot of its target function");
    }

    /// `demand` with each placement resolved against `result`, the reads of
    /// the enclosing function's result.
    pub fn resolve(self: *const Analysis, allocator: Allocator, demand: Demand, result: []const ResolvedRead) Allocator.Error![]const ResolvedRead {
        var out: std.ArrayList(ResolvedRead) = .empty;
        var steps: std.ArrayList(Step) = .empty;
        for (demand.read_paths, demand.read_consumers) |path, consumers| {
            try self.paths.stepsOf(allocator, path, &steps);
            try out.append(allocator, .{ .steps = try allocator.dupe(Step, steps.items), .consumers = consumers });
        }
        var placed: std.ArrayList(Step) = .empty;
        var rest: std.ArrayList(Step) = .empty;
        for (demand.placements) |placement| {
            try self.paths.stepsOf(allocator, placement.result, &placed);
            for (result) |read| {
                if (!startsWith(read.steps, placed.items)) continue;
                try self.paths.stepsOf(allocator, placement.value, &rest);
                for (read.steps[placed.items.len..]) |step| try appendStep(allocator, &rest, step);
                try out.append(allocator, .{ .steps = try allocator.dupe(Step, rest.items), .consumers = read.consumers });
            }
        }
        return out.items;
    }

    /// Whether `fn_id` constructs, on every path that returns, a result part
    /// at `steps` that serves one of `consumers`.
    pub fn constructs(self: *const Analysis, fn_id: Ast.FnId, steps: []const Step, consumers: Consumers) bool {
        const path = self.paths.lookup(steps) orelse return false;
        for (self.summary(fn_id).constructed) |part| {
            if (part.path == path and part.consumers.intersectWith(consumers).count() != 0) return true;
        }
        return false;
    }

    /// Steps of `path`, allocated in `allocator`.
    pub fn stepsOf(self: *const Analysis, allocator: Allocator, path: PathId) Allocator.Error![]const Step {
        var steps: std.ArrayList(Step) = .empty;
        try self.paths.stepsOf(allocator, path, &steps);
        return steps.items;
    }
};

/// Whether `steps` begins with every step of `prefix`.
pub fn startsWith(steps: []const Step, prefix: []const Step) bool {
    if (prefix.len > steps.len) return false;
    for (prefix, steps[0..prefix.len]) |a, b| if (!a.eql(b)) return false;
    return true;
}

/// Analyze every function of `program` whose body `bodies` gives.
pub fn analyze(gpa: Allocator, program: *const Ast.Program, bodies: []const ?Ast.ExprId) Allocator.Error!Analysis {
    var analysis: Analysis = .{
        .arena = collections.SingleThreadArena.init(gpa),
        .program = program,
        .paths = undefined,
        .fns = &.{},
    };
    errdefer analysis.deinit();
    const arena = analysis.arena.allocator();
    analysis.paths = try PathTable.init(arena);
    analysis.fns = try arena.alloc(FnSummary, bodies.len);
    for (analysis.fns, 0..) |*fn_summary, index| {
        const fn_ = program.getFnAt(index);
        fn_summary.* = .{
            .params = try arena.alloc(DemandSet, program.typedLocalSpan(fn_.args).len),
            .captures = try arena.alloc(DemandSet, program.typedLocalSpan(fn_.captures).len),
            .wrapper = bodies[index] != null and
                try WrapperBody.wrapperBody(gpa, program, @fromBackingInt(@as(u32, @intCast(index)))) != null,
        };
        for (fn_summary.params) |*set| set.* = .{};
        for (fn_summary.captures) |*set| set.* = .{};
    }

    var builder = Builder{
        .gpa = gpa,
        .analysis = &analysis,
        .scratch = collections.SingleThreadArena.init(gpa),
        .callers = try gpa.alloc(std.ArrayList(Ast.FnId), bodies.len),
    };
    for (builder.callers) |*list| list.* = .empty;
    defer builder.deinit();

    try builder.fixpoint(bodies, Builder.demandFn);
    try builder.fixpoint(bodies, Builder.constructedFn);
    try builder.dropRecursiveWrappers();
    try builder.dropSpecializingWrappers(bodies);
    return analysis;
}

const Builder = struct {
    gpa: Allocator,
    analysis: *Analysis,
    /// Per-function walk state, reset between functions.
    scratch: collections.SingleThreadArena,
    /// Callers of each function, recorded by the demand walk.
    callers: []std.ArrayList(Ast.FnId),
    fn_id: Ast.FnId = undefined,
    locals: collections.DenseMapUnmanaged(Ast.LocalId, DemandSet) = .empty,
    locals_changed: bool = false,
    joins: collections.DenseMapUnmanaged(Ast.JoinPointId, Ast.Span(Ast.TypedLocal)) = .empty,
    loops: std.ArrayList(Loop) = .empty,
    tasks: std.ArrayList(Task) = .empty,
    steps: std.ArrayList(Step) = .empty,
    rest: std.ArrayList(Step) = .empty,

    /// An enclosing loop: the demand on its result, which its `break` values
    /// meet, and its parameters, which its `continue` values feed.
    const Loop = struct { result: Demand, params: Ast.Span(Ast.TypedLocal) };

    const Task = union(enum) {
        expr: struct { id: Ast.ExprId, demand: Demand },
        /// A value bound by a pattern, demanded by the pattern's uses.
        bound: struct { pat: Ast.PatId, value: Ast.ExprId },
        stmt: Ast.StmtId,
        /// A value that flows into a local: a loop's initial value, a
        /// `continue` value, or a jump argument.
        into_local: struct { value: Ast.ExprId, local: Ast.LocalId, enters_loop: bool },
        /// A match scrutinee, demanded by every branch pattern.
        scrutinee: Mono.MatchExpr,
        pop_loop,
    };

    fn deinit(self: *Builder) void {
        for (self.callers) |*list| list.deinit(self.gpa);
        self.gpa.free(self.callers);
        self.scratch.deinit();
    }

    fn program(self: *const Builder) *const Ast.Program {
        return self.analysis.program;
    }

    fn persistent(self: *Builder) Allocator {
        return self.analysis.arena.allocator();
    }

    fn temp(self: *Builder) Allocator {
        return self.scratch.allocator();
    }

    fn resetFunction(self: *Builder, fn_id: Ast.FnId) void {
        _ = self.scratch.reset(.retain_capacity);
        self.fn_id = fn_id;
        self.locals = .empty;
        self.joins = .empty;
        self.loops = .empty;
        self.tasks = .empty;
        self.steps = .empty;
        self.rest = .empty;
    }

    fn noteCaller(self: *Builder, callee: Ast.FnId) Allocator.Error!void {
        const raw = @backingInt(callee);
        if (raw >= self.callers.len) Common.invariant("inline productivity met a call to a function created after it began");
        const list = &self.callers[raw];
        if (list.items.len != 0 and list.items[list.items.len - 1] == self.fn_id) return;
        for (list.items) |caller| if (caller == self.fn_id) return;
        try list.append(self.gpa, self.fn_id);
    }

    /// Recompute function summaries until none changes. Every summary only
    /// grows, within the finite set of paths the program's labels allow, so
    /// the worklist empties.
    fn fixpoint(
        self: *Builder,
        bodies: []const ?Ast.ExprId,
        comptime visit: fn (*Builder, Ast.FnId, Ast.ExprId) Allocator.Error!bool,
    ) Allocator.Error!void {
        const queued = try self.gpa.alloc(bool, bodies.len);
        defer self.gpa.free(queued);
        @memset(queued, true);
        var pending: std.ArrayList(Ast.FnId) = .empty;
        defer pending.deinit(self.gpa);
        var index = bodies.len;
        while (index > 0) {
            index -= 1;
            try pending.append(self.gpa, @fromBackingInt(@as(u32, @intCast(index))));
        }
        while (pending.pop()) |fn_id| {
            const raw = @backingInt(fn_id);
            queued[raw] = false;
            const body = bodies[raw] orelse continue;
            if (!try visit(self, fn_id, body)) continue;
            for (self.callers[raw].items) |caller| {
                const caller_raw = @backingInt(caller);
                if (queued[caller_raw]) continue;
                queued[caller_raw] = true;
                try pending.append(self.gpa, caller);
            }
        }
    }

    /// Clear `wrapper` on every function that lies on a cycle of direct
    /// calls: a strongly connected component of more than one function, or
    /// one that calls itself. The caller lists are the call graph reversed,
    /// which has the same components. Tarjan's algorithm, on an explicit
    /// stack so call chains never become native call depth.
    fn dropRecursiveWrappers(self: *Builder) Allocator.Error!void {
        const count = self.callers.len;
        const unvisited = std.math.maxInt(u32);
        const order = try self.gpa.alloc(u32, count);
        defer self.gpa.free(order);
        const low = try self.gpa.alloc(u32, count);
        defer self.gpa.free(low);
        const on_stack = try self.gpa.alloc(bool, count);
        defer self.gpa.free(on_stack);
        @memset(order, unvisited);
        @memset(on_stack, false);

        const Frame = struct { node: u32, next: u32 };
        var frames: std.ArrayList(Frame) = .empty;
        defer frames.deinit(self.gpa);
        var component_stack: std.ArrayList(u32) = .empty;
        defer component_stack.deinit(self.gpa);
        var next_order: u32 = 0;

        for (0..count) |root_index| {
            const root: u32 = @intCast(root_index);
            if (order[root] != unvisited) continue;
            order[root] = next_order;
            low[root] = next_order;
            next_order += 1;
            try component_stack.append(self.gpa, root);
            on_stack[root] = true;
            try frames.append(self.gpa, .{ .node = root, .next = 0 });

            while (frames.items.len != 0) {
                const frame = &frames.items[frames.items.len - 1];
                const node = frame.node;
                const edges = self.callers[node].items;
                if (frame.next < edges.len) {
                    const target: u32 = @backingInt(edges[frame.next]);
                    frame.next += 1;
                    if (order[target] == unvisited) {
                        order[target] = next_order;
                        low[target] = next_order;
                        next_order += 1;
                        try component_stack.append(self.gpa, target);
                        on_stack[target] = true;
                        try frames.append(self.gpa, .{ .node = target, .next = 0 });
                    } else if (on_stack[target]) {
                        low[node] = @min(low[node], order[target]);
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
                const on_cycle = component.len > 1 or self.callsItself(node);
                if (on_cycle) for (component) |member| {
                    self.analysis.fns[member].wrapper = false;
                };
                component_stack.shrinkRetainingCapacity(start);
            }
        }
    }

    /// Clear `wrapper` on every function whose body passes a direct call a
    /// value it builds itself, where the callee consumes that value. The
    /// clone of the body inlines such a call wherever the body is, so the
    /// body only stays smaller than a call while it stays one procedure.
    fn dropSpecializingWrappers(self: *Builder, bodies: []const ?Ast.ExprId) Allocator.Error!void {
        for (bodies, 0..) |maybe_body, index| {
            const body = maybe_body orelse continue;
            if (!self.analysis.fns[index].wrapper) continue;
            const fn_id: Ast.FnId = @fromBackingInt(@as(u32, @intCast(index)));
            if (try self.bodySpecializesCall(fn_id, body)) self.analysis.fns[index].wrapper = false;
        }
    }

    /// Whether a direct call in `body` gets, from a value `body` builds, a
    /// part its callee consumes, so that the call inlines productively with
    /// nothing known about `fn_id`'s arguments or result.
    fn bodySpecializesCall(self: *Builder, fn_id: Ast.FnId, body: Ast.ExprId) Allocator.Error!bool {
        self.resetFunction(fn_id);
        const program_ = self.program();
        var slots: collections.DenseMapUnmanaged(Ast.LocalId, Slot) = .empty;
        const args = program_.typedLocalSpan(program_.getFn(fn_id).args);
        for (0..args.len) |index| try slots.put(self.temp(), GuardedList.at(args, index).local, .{ .kind = .arg, .index = @intCast(index) });
        var defs: collections.DenseMapUnmanaged(Ast.LocalId, ?Ast.ExprId) = .empty;
        var returns: std.ArrayList(Ast.ExprId) = .empty;
        try self.collectDefinitions(body, &defs, &returns);
        const context: BuiltContext = .{ .defs = &defs, .slots = &slots };
        var memo: collections.DenseMapUnmanaged(Ast.ExprId, Built) = .empty;

        var stack: std.ArrayList(Ast.ExprChild) = .empty;
        try stack.append(self.temp(), .{ .expr = body });
        while (stack.pop()) |child| switch (child) {
            .stmt => |stmt_id| try Ast.appendStmtChildren(self.temp(), program_, stmt_id, &stack),
            .expr => |expr_id| {
                switch (program_.getExpr(expr_id).data) {
                    .call_proc => |call| if (Ast.localDirectCallee(call)) |callee| {
                        const callee_summary = self.analysis.summary(callee);
                        const call_args = program_.exprSpan(call.args);
                        for (0..call_args.len) |index| {
                            const arg = try self.built(GuardedList.at(call_args, index), context, &memo);
                            if (meetsDemand(arg, callee_summary.params[index].view())) return true;
                        }
                        const operands = program_.captureOperandSpan(call.captures);
                        for (0..operands.len) |index| {
                            const operand = GuardedList.at(operands, index);
                            const value = try self.built(operand.value, context, &memo);
                            if (meetsDemand(value, callee_summary.captures[self.analysis.captureSlot(callee, operand.id)].view())) return true;
                        }
                    },
                    // A compile-time value is an opaque leaf to the clone.
                    .comptime_value => continue,
                    .local, .unit, .@"unreachable", .int_lit, .frac_f32_lit, .frac_f64_lit, .dec_lit, .str_lit, .bytes_lit, .static_data_candidate, .typed_boundary, .row_widen, .list, .tuple, .record, .record_update, .tag, .nominal, .let_, .lambda, .def_ref, .fn_def, .fn_ref, .call_value, .low_level, .field_access, .tuple_access, .structural_eq, .structural_hash, .match_, .if_, .uninitialized, .uninitialized_payload, .if_initialized_payload, .try_sequence, .try_record_sequence, .block, .loop_, .break_, .continue_, .join_point, .jump, .return_, .crash, .checked_error, .comptime_exhaustiveness_failed, .dbg, .expect_err, .expect, .literal_rejected => {},
                }
                try Ast.appendChildren(self.temp(), program_, expr_id, &stack);
            },
        };
        return false;
    }

    /// Whether a part `value` builds meets a consumer of it that `demand`
    /// reads directly.
    fn meetsDemand(value: Built, demand: Demand) bool {
        for (value.parts) |part| {
            for (demand.read_paths, demand.read_consumers) |path, consumers| {
                if (path != part.path) continue;
                if (part.consumers.intersectWith(consumers).intersectWith(argument_consumers).count() != 0) return true;
            }
        }
        return false;
    }

    fn callsItself(self: *const Builder, node: u32) bool {
        for (self.callers[node].items) |caller| {
            if (@backingInt(caller) == node) return true;
        }
        return false;
    }

    // Demand walk

    /// Walk one body until its locals' demand stops growing, then fold the
    /// parameters' and captures' demand into the function's summary.
    fn demandFn(self: *Builder, fn_id: Ast.FnId, body: Ast.ExprId) Allocator.Error!bool {
        self.resetFunction(fn_id);
        const whole_result = try self.resultDemand();
        while (true) {
            self.locals_changed = false;
            try self.tasks.append(self.temp(), .{ .expr = .{ .id = body, .demand = whole_result } });
            while (self.tasks.pop()) |task| try self.step(task);
            if (!self.locals_changed) break;
        }
        const fn_ = self.program().getFn(fn_id);
        const fn_summary = &self.analysis.fns[@backingInt(fn_id)];
        var changed = false;
        const args = self.program().typedLocalSpan(fn_.args);
        for (0..args.len) |index| {
            if (try fn_summary.params[index].addAll(self.persistent(), self.localDemand(GuardedList.at(args, index).local))) changed = true;
        }
        const captures = self.program().typedLocalSpan(fn_.captures);
        for (0..captures.len) |index| {
            if (try fn_summary.captures[index].addAll(self.persistent(), self.localDemand(GuardedList.at(captures, index).local))) changed = true;
        }
        return changed;
    }

    fn localDemand(self: *const Builder, local: Ast.LocalId) Demand {
        const set = self.locals.getPtrConst(local) orelse return .none;
        return set.view();
    }

    fn demandOf(self: *Builder, reads: []const Read, placements: []const Placement) Allocator.Error!Demand {
        var set: DemandSet = .{};
        for (reads) |read| _ = try set.addRead(self.temp(), read.path, read.consumers);
        for (placements) |placement| _ = try set.addPlacement(self.temp(), placement);
        return set.view();
    }

    /// Demand on a value the function returns: it is the result, read by
    /// whatever reads the result at each call. Returning a value is not
    /// itself a use that gains from knowing it, since a callee that builds
    /// its result already builds it where the caller receives it.
    fn resultDemand(self: *Builder) Allocator.Error!Demand {
        return self.demandOf(&.{}, &.{.{ .value = .root, .result = .root }});
    }

    fn consumedAtRoot(self: *Builder, consumer: Consumer) Allocator.Error!Demand {
        return self.demandOf(&.{.{ .path = .root, .consumers = Consumers.initOne(consumer) }}, &.{});
    }

    fn push(self: *Builder, id: Ast.ExprId, demand: Demand) Allocator.Error!void {
        try self.tasks.append(self.temp(), .{ .expr = .{ .id = id, .demand = demand } });
    }

    fn pushSpan(self: *Builder, span: Ast.Span(Ast.ExprId), demand: Demand) Allocator.Error!void {
        const exprs = self.program().exprSpan(span);
        for (0..exprs.len) |index| try self.push(GuardedList.at(exprs, index), demand);
    }

    /// Demand on the part at `step` of a value demanded by `demand`.
    fn project(self: *Builder, demand: Demand, step_: Step) Allocator.Error!Demand {
        const paths = &self.analysis.paths;
        var set: DemandSet = .{};
        for (demand.read_paths, demand.read_consumers) |path, consumers| {
            try paths.stepsOf(self.temp(), path, &self.steps);
            if (self.steps.items.len == 0 or !self.steps.items[0].eql(step_)) continue;
            const below = try paths.concat(self.persistent(), .root, self.steps.items[1..]);
            _ = try set.addRead(self.temp(), below, consumers);
        }
        for (demand.placements) |placement| {
            if (placement.value == .root) {
                const result = try paths.child(self.persistent(), placement.result, step_);
                _ = try set.addPlacement(self.temp(), .{ .value = .root, .result = result });
                continue;
            }
            try paths.stepsOf(self.temp(), placement.value, &self.steps);
            if (!self.steps.items[0].eql(step_)) continue;
            const below = try paths.concat(self.persistent(), .root, self.steps.items[1..]);
            _ = try set.addPlacement(self.temp(), .{ .value = below, .result = placement.result });
        }
        return set.view();
    }

    /// Demand on a value whose part at `prefix` is demanded by `demand`.
    fn underPrefix(self: *Builder, demand: Demand, prefix: []const Step) Allocator.Error!Demand {
        const paths = &self.analysis.paths;
        const base = try paths.concat(self.persistent(), .root, prefix);
        var set: DemandSet = .{};
        for (demand.read_paths, demand.read_consumers) |path, consumers| {
            try paths.stepsOf(self.temp(), path, &self.steps);
            _ = try set.addRead(self.temp(), try paths.concat(self.persistent(), base, self.steps.items), consumers);
        }
        for (demand.placements) |placement| {
            try paths.stepsOf(self.temp(), placement.value, &self.steps);
            _ = try set.addPlacement(self.temp(), .{ .value = try paths.concat(self.persistent(), base, self.steps.items), .result = placement.result });
        }
        return set.view();
    }

    /// Demand on the constructor operand at `step` of a value demanded by
    /// `demand`: what reads that part, and the placement itself.
    fn constructorOperand(self: *Builder, demand: Demand, step_: Step) Allocator.Error!Demand {
        var set: DemandSet = .{};
        _ = try set.addAll(self.temp(), try self.project(demand, step_));
        return set.view();
    }

    /// Demand on a receiver read along `steps`, whose read part is demanded by
    /// `demand`: each record or tuple the read passes through is projected.
    fn projected(self: *Builder, demand: Demand, steps: []const Step) Allocator.Error!Demand {
        const under = try self.underPrefix(demand, steps);
        var set: DemandSet = .{};
        _ = try set.addAll(self.temp(), under);
        var path: PathId = .root;
        for (steps) |step_| {
            _ = try set.addRead(self.temp(), path, Consumers.initOne(.project));
            path = try self.analysis.paths.child(self.persistent(), path, step_);
        }
        return set.view();
    }

    /// Demand on an argument whose callee summarizes its use as `param`, at a
    /// call whose result is demanded by `call`.
    fn instantiate(self: *Builder, param: Demand, call: Demand) Allocator.Error!Demand {
        const paths = &self.analysis.paths;
        var set: DemandSet = .{};
        for (param.read_paths, param.read_consumers) |path, consumers| {
            _ = try set.addRead(self.temp(), path, consumers);
        }
        for (param.placements) |placement| {
            for (call.read_paths, call.read_consumers) |path, consumers| {
                if (!try paths.stepsBelow(self.temp(), path, placement.result, &self.steps)) continue;
                _ = try set.addRead(self.temp(), try paths.concat(self.persistent(), placement.value, self.steps.items), consumers);
            }
            for (call.placements) |outer| {
                // The call result's part `outer.value` sits in the caller's
                // result; the parameter's part sits in the call result.
                if (try paths.stepsBelow(self.temp(), outer.value, placement.result, &self.steps)) {
                    _ = try set.addPlacement(self.temp(), .{
                        .value = try paths.concat(self.persistent(), placement.value, self.steps.items),
                        .result = outer.result,
                    });
                } else if (try paths.stepsBelow(self.temp(), placement.result, outer.value, &self.steps)) {
                    _ = try set.addPlacement(self.temp(), .{
                        .value = placement.value,
                        .result = try paths.concat(self.persistent(), outer.result, self.steps.items),
                    });
                }
            }
        }
        return set.view();
    }

    fn addToLocal(self: *Builder, local: Ast.LocalId, demand: Demand) Allocator.Error!void {
        if (demand.read_paths.len == 0 and demand.placements.len == 0) return;
        const gop = try self.locals.getOrPut(self.temp(), local);
        if (!gop.found_existing) gop.value_ptr.* = .{};
        if (try gop.value_ptr.addAll(self.temp(), demand)) self.locals_changed = true;
    }

    fn recordCallDemand(self: *Builder, call: Ast.ExprId, demand: Demand) Allocator.Error!void {
        const gop = try self.analysis.call_demand.getOrPut(self.persistent(), call);
        if (!gop.found_existing) gop.value_ptr.* = .{};
        _ = try gop.value_ptr.addAll(self.persistent(), demand);
    }

    /// Demand a pattern's uses place on the value it binds.
    fn patternDemand(self: *Builder, pat_id: Ast.PatId) Allocator.Error!Demand {
        var set: DemandSet = .{};
        try self.addPatternDemand(pat_id, &.{}, &set);
        return set.view();
    }

    fn addPatternDemand(self: *Builder, pat_id: Ast.PatId, prefix: []const Step, out: *DemandSet) Allocator.Error!void {
        const program_ = self.program();
        switch (program_.getPat(pat_id).data) {
            .bind => |local| try self.addLocalUnder(local, prefix, out),
            .as => |as_| {
                try self.addLocalUnder(as_.local, prefix, out);
                try self.addPatternDemand(as_.pattern, prefix, out);
            },
            .record => |span| {
                _ = try out.addRead(self.temp(), try self.analysis.paths.concat(self.persistent(), .root, prefix), Consumers.initOne(.project));
                const destructs = program_.recordDestructSpan(span);
                for (0..destructs.len) |index| {
                    const destruct = GuardedList.at(destructs, index);
                    try self.addPatternDemand(destruct.pattern, try self.extended(prefix, Step.field(destruct.name)), out);
                }
            },
            .tuple => |span| {
                _ = try out.addRead(self.temp(), try self.analysis.paths.concat(self.persistent(), .root, prefix), Consumers.initOne(.project));
                const pats = program_.patSpan(span);
                for (0..pats.len) |index| {
                    try self.addPatternDemand(GuardedList.at(pats, index), try self.extended(prefix, Step.item(@intCast(index))), out);
                }
            },
            .tag => |tag| {
                _ = try out.addRead(self.temp(), try self.analysis.paths.concat(self.persistent(), .root, prefix), Consumers.initOne(.match));
                const pats = program_.patSpan(tag.payloads);
                for (0..pats.len) |index| {
                    try self.addPatternDemand(GuardedList.at(pats, index), try self.extended(prefix, Step.payload(tag.name, @intCast(index))), out);
                }
            },
            .nominal => |inner| try self.addPatternDemand(inner, prefix, out),
            // A list, literal, or string pattern reads no part this pass can
            // know.
            .wildcard, .list, .int_lit, .dec_lit, .frac_f32_lit, .frac_f64_lit, .str_lit, .str_pattern => {},
        }
    }

    fn extended(self: *Builder, prefix: []const Step, step_: Step) Allocator.Error![]const Step {
        var steps: std.ArrayList(Step) = .empty;
        try steps.appendSlice(self.temp(), prefix);
        try appendStep(self.temp(), &steps, step_);
        return steps.items;
    }

    fn addLocalUnder(self: *Builder, local: Ast.LocalId, prefix: []const Step, out: *DemandSet) Allocator.Error!void {
        const under = try self.underPrefix(self.localDemand(local), prefix);
        _ = try out.addAll(self.temp(), under);
    }

    fn step(self: *Builder, task: Task) Allocator.Error!void {
        switch (task) {
            .expr => |expr| try self.walkExpr(expr.id, expr.demand),
            .bound => |bound| try self.push(bound.value, try self.patternDemand(bound.pat)),
            .stmt => |stmt_id| try self.walkStmt(stmt_id),
            .into_local => |into| {
                const local_demand = self.localDemand(into.local);
                const demand = if (into.enters_loop) blk: {
                    var set: DemandSet = .{};
                    _ = try set.addAll(self.temp(), local_demand);
                    _ = try set.addRead(self.temp(), .root, Consumers.initOne(.loop));
                    break :blk set.view();
                } else local_demand;
                try self.push(into.value, demand);
            },
            .scrutinee => |match| {
                var set: DemandSet = .{};
                _ = try set.addRead(self.temp(), .root, Consumers.initOne(.match));
                const branches = self.program().branchSpan(match.branches);
                for (0..branches.len) |index| {
                    _ = try set.addAll(self.temp(), try self.patternDemand(GuardedList.at(branches, index).pat));
                }
                try self.push(match.scrutinee, set.view());
            },
            .pop_loop => _ = self.loops.pop() orelse Common.invariant("inline productivity loop stack underflow"),
        }
    }

    fn walkStmt(self: *Builder, stmt_id: Ast.StmtId) Allocator.Error!void {
        switch (self.program().getStmt(stmt_id)) {
            .let_ => |let_| try self.push(let_.value, try self.patternDemand(let_.pat)),
            .expr, .expect, .dbg => |expr| try self.push(expr, .none),
            .return_ => |ret| try self.push(ret.value, try self.resultDemand()),
            .uninitialized, .crash, .checked_error => {},
        }
    }

    /// Push the tasks for `expr_id`. The stack runs them last-pushed first, so
    /// a binding's uses are walked before its value.
    fn walkExpr(self: *Builder, expr_id: Ast.ExprId, demand: Demand) Allocator.Error!void {
        const program_ = self.program();
        switch (program_.getExpr(expr_id).data) {
            .local => |local| try self.addToLocal(local, demand),
            .typed_boundary => |boundary| try self.push(boundary.value, demand),
            .row_widen => |widen| try self.push(widen.value, demand),
            .nominal => |backing| try self.push(backing, demand),
            .static_data_candidate => |candidate| switch (candidate.value) {
                .runtime => |runtime| try self.push(runtime, demand),
                .stored => {},
            },
            .tag => |tag| {
                const payloads = program_.exprSpan(tag.payloads);
                for (0..payloads.len) |index| {
                    try self.push(GuardedList.at(payloads, index), try self.constructorOperand(demand, Step.payload(tag.name, @intCast(index))));
                }
            },
            .record => |span| {
                const fields = program_.fieldExprSpan(span);
                for (0..fields.len) |index| {
                    const field = GuardedList.at(fields, index);
                    try self.push(field.value, try self.constructorOperand(demand, Step.field(field.name)));
                }
            },
            .record_update => |update| {
                const fields = program_.fieldExprSpan(update.fields);
                var base: DemandSet = .{};
                _ = try base.addAll(self.temp(), demand);
                for (0..fields.len) |index| {
                    const field = GuardedList.at(fields, index);
                    const field_step = Step.field(field.name);
                    try self.push(field.value, try self.constructorOperand(demand, field_step));
                    // The update supplies this field, so the base's field is
                    // never read.
                    var read_index: usize = 0;
                    while (read_index < base.read_paths.items.len) {
                        try self.analysis.paths.stepsOf(self.temp(), base.read_paths.items[read_index], &self.steps);
                        if (self.steps.items.len != 0 and self.steps.items[0].eql(field_step)) {
                            base.removeReadAt(read_index);
                        } else {
                            read_index += 1;
                        }
                    }
                }
                try self.push(update.base, base.view());
            },
            .tuple => |span| {
                const items = program_.exprSpan(span);
                for (0..items.len) |index| {
                    try self.push(GuardedList.at(items, index), try self.constructorOperand(demand, Step.item(@intCast(index))));
                }
            },
            .field_access => |access| {
                const segments = program_.fieldAccessSegmentSpan(access.segments);
                var prefix: std.ArrayList(Step) = .empty;
                for (0..segments.len) |index| {
                    try appendStep(self.temp(), &prefix, Step.field(GuardedList.at(segments, index).field));
                }
                try self.push(access.receiver, try self.projected(demand, prefix.items));
            },
            .tuple_access => |access| try self.push(access.tuple, try self.projected(demand, &.{Step.item(access.elem_index)})),
            .let_ => |let_| {
                try self.tasks.append(self.temp(), .{ .bound = .{ .pat = let_.bind, .value = let_.value } });
                try self.push(let_.rest, demand);
            },
            .block => |block| {
                const stmts = program_.stmtSpan(block.statements);
                for (0..stmts.len) |index| try self.tasks.append(self.temp(), .{ .stmt = GuardedList.at(stmts, index) });
                try self.push(block.final_expr, demand);
            },
            .if_ => |if_| {
                const branches = program_.ifBranchSpan(if_.branches);
                for (0..branches.len) |index| {
                    const branch = GuardedList.at(branches, index);
                    try self.push(branch.cond, .none);
                    try self.push(branch.body, demand);
                }
                try self.push(if_.final_else, demand);
            },
            .if_initialized_payload => |payload_switch| {
                try self.push(payload_switch.cond, .none);
                try self.push(payload_switch.initialized, demand);
                try self.push(payload_switch.uninitialized, demand);
            },
            .match_ => |match| {
                try self.tasks.append(self.temp(), .{ .scrutinee = match });
                const branches = program_.branchSpan(match.branches);
                for (0..branches.len) |index| {
                    const branch = GuardedList.at(branches, index);
                    const bindings = program_.stmtSpan(branch.bindings);
                    for (0..bindings.len) |binding| try self.tasks.append(self.temp(), .{ .stmt = GuardedList.at(bindings, binding) });
                    if (branch.guard) |guard| try self.push(guard, .none);
                    try self.push(branch.body, demand);
                }
            },
            // The clone binds the `Ok` payload as a runtime value, so nothing
            // read through it reaches the tried expression.
            .try_sequence => |sequence| {
                try self.push(sequence.try_expr, .none);
                try self.push(sequence.ok_body, demand);
            },
            .try_record_sequence => |sequence| {
                try self.push(sequence.try_expr, .none);
                try self.push(sequence.ok_body, demand);
            },
            .loop_ => |loop| {
                const params = program_.typedLocalSpan(loop.params);
                const initial_values = program_.exprSpan(loop.initial_values);
                if (params.len != initial_values.len) Common.invariant("loop initial values differ in count from its parameters");
                for (0..initial_values.len) |index| {
                    try self.tasks.append(self.temp(), .{ .into_local = .{
                        .value = GuardedList.at(initial_values, index),
                        .local = GuardedList.at(params, index).local,
                        .enters_loop = true,
                    } });
                }
                try self.tasks.append(self.temp(), .pop_loop);
                try self.push(loop.body, .none);
                try self.loops.append(self.temp(), .{ .result = demand, .params = loop.params });
            },
            .break_ => |maybe_value| if (maybe_value) |value| {
                if (self.loops.items.len == 0) Common.invariant("break outside a loop reached inline productivity");
                try self.push(value, self.loops.items[self.loops.items.len - 1].result);
            },
            .continue_ => |continue_| {
                if (self.loops.items.len == 0) Common.invariant("continue outside a loop reached inline productivity");
                try self.pushIntoLocals(continue_.values, self.loops.items[self.loops.items.len - 1].params, false);
            },
            .join_point => |join| {
                try self.joins.put(self.temp(), join.id, join.params);
                try self.push(join.remainder, demand);
                try self.push(join.body, demand);
            },
            .jump => |jump| {
                const params_span = self.joins.get(jump.target) orelse Common.invariant("jump to an undeclared join point reached inline productivity");
                try self.pushIntoLocals(jump.args, params_span, false);
                try self.pushIntoLocals(jump.loop_values, jump.loop_params, false);
            },
            .return_ => |ret| try self.push(ret.value, try self.resultDemand()),
            .fn_ref => |fn_ref| try self.walkFnRef(fn_ref, demand),
            .call_value => |call| {
                try self.recordCallDemand(expr_id, demand);
                try self.push(call.callee, try self.consumedAtRoot(.call));
                try self.pushSpan(call.args, .none);
            },
            .call_proc => |call| try self.walkCallProc(expr_id, call, demand),
            .dbg => |value| try self.push(value, .none),
            .expect => |value| try self.push(value, .none),
            .list => |span| try self.pushSpan(span, .none),
            .low_level, .structural_eq, .structural_hash, .expect_err, .literal_rejected => {
                var children: std.ArrayList(Ast.ExprChild) = .empty;
                try Ast.appendChildren(self.temp(), program_, expr_id, &children);
                for (children.items) |child| switch (child) {
                    .expr => |child_expr| try self.push(child_expr, .none),
                    .stmt => |stmt_id| try self.tasks.append(self.temp(), .{ .stmt = stmt_id }),
                };
            },
            .unit, .@"unreachable", .int_lit, .frac_f32_lit, .frac_f64_lit, .dec_lit, .str_lit, .bytes_lit, .comptime_value, .uninitialized, .uninitialized_payload, .crash, .checked_error, .comptime_exhaustiveness_failed => {},
            .lambda, .def_ref, .fn_def => Common.invariant("pre-lift function expression reached inline productivity"),
        }
    }

    fn pushIntoLocals(self: *Builder, values_span: Ast.Span(Ast.ExprId), params_span: Ast.Span(Ast.TypedLocal), enters_loop: bool) Allocator.Error!void {
        const values = self.program().exprSpan(values_span);
        const params = self.program().typedLocalSpan(params_span);
        if (values.len != params.len) Common.invariant("jump values differ in count from their target's parameters");
        for (0..values.len) |index| {
            try self.tasks.append(self.temp(), .{ .into_local = .{
                .value = GuardedList.at(values, index),
                .local = GuardedList.at(params, index).local,
                .enters_loop = enters_loop,
            } });
        }
    }

    fn walkCallProc(self: *Builder, expr_id: Ast.ExprId, call: Mono.CallProc, demand: Demand) Allocator.Error!void {
        const program_ = self.program();
        const callee = Ast.localDirectCallee(call) orelse {
            try self.pushSpan(call.args, .none);
            const operands = program_.captureOperandSpan(call.captures);
            for (0..operands.len) |index| try self.push(GuardedList.at(operands, index).value, .none);
            return;
        };
        try self.recordCallDemand(expr_id, demand);
        try self.noteCaller(callee);
        const callee_summary = self.analysis.summary(callee);
        const args = program_.exprSpan(call.args);
        if (args.len != callee_summary.params.len) Common.invariant("direct call arity differed from its lifted function's arity in inline productivity");
        for (0..args.len) |index| {
            try self.push(GuardedList.at(args, index), try self.instantiate(callee_summary.params[index].view(), demand));
        }
        const operands = program_.captureOperandSpan(call.captures);
        for (0..operands.len) |index| {
            const operand = GuardedList.at(operands, index);
            const slot = self.analysis.captureSlot(callee, operand.id);
            try self.push(operand.value, try self.instantiate(callee_summary.captures[slot].view(), demand));
        }
    }

    /// A function value's captures are consumed as its body consumes them
    /// whenever the value may be called: when the code around it calls it,
    /// or when it flows into the result, where any caller may call it.
    fn walkFnRef(self: *Builder, fn_ref: Mono.LiftedFunctionValue, demand: Demand) Allocator.Error!void {
        const target = fn_ref.fn_id;
        try self.noteCaller(target);
        var called = false;
        for (demand.read_paths, demand.read_consumers) |path, consumers| {
            if (path == .root and consumers.contains(.call)) called = true;
        }
        for (demand.placements) |placement| {
            if (placement.value == .root) called = true;
        }
        const operands = self.program().captureOperandSpan(fn_ref.captures);
        const target_summary = self.analysis.summary(target);
        for (0..operands.len) |index| {
            const operand = GuardedList.at(operands, index);
            if (!called) {
                try self.push(operand.value, .none);
                continue;
            }
            // The target's own result is not this function's result, so only
            // the capture's reads carry over.
            const slot = self.analysis.captureSlot(target, operand.id);
            const capture_demand = target_summary.captures[slot].view();
            try self.push(operand.value, .{
                .read_paths = capture_demand.read_paths,
                .read_consumers = capture_demand.read_consumers,
                .placements = &.{},
            });
        }
    }

    // Constructed-result walk

    /// What an expression's value is known to be on every path that
    /// produces it: parts it constructs, and parts that are a parameter's or
    /// capture's parts.
    const Built = struct {
        /// No path produces a value.
        diverges: bool = false,
        parts: []const Read = &.{},
        passes: []const PassThrough = &.{},

        const diverging: Built = .{ .diverges = true };
    };

    fn constructedFn(self: *Builder, fn_id: Ast.FnId, body: Ast.ExprId) Allocator.Error!bool {
        self.resetFunction(fn_id);
        var slots: collections.DenseMapUnmanaged(Ast.LocalId, Slot) = .empty;
        const fn_ = self.program().getFn(fn_id);
        const args = self.program().typedLocalSpan(fn_.args);
        for (0..args.len) |index| try slots.put(self.temp(), GuardedList.at(args, index).local, .{ .kind = .arg, .index = @intCast(index) });
        const captures = self.program().typedLocalSpan(fn_.captures);
        for (0..captures.len) |index| try slots.put(self.temp(), GuardedList.at(captures, index).local, .{ .kind = .capture, .index = @intCast(index) });
        var defs: collections.DenseMapUnmanaged(Ast.LocalId, ?Ast.ExprId) = .empty;
        var returns: std.ArrayList(Ast.ExprId) = .empty;
        try self.collectDefinitions(body, &defs, &returns);
        const context: BuiltContext = .{ .defs = &defs, .slots = &slots };
        var memo: collections.DenseMapUnmanaged(Ast.ExprId, Built) = .empty;
        var result = try self.built(body, context, &memo);
        for (returns.items) |value| result = try self.meet(result, try self.built(value, context, &memo));
        // A function that never returns normally builds nothing a caller can
        // consume.
        const parts: []const Read = if (result.diverges) &.{} else try self.mergedParts(result.parts);
        const passes: []const PassThrough = if (result.diverges) &.{} else try self.distinctPasses(result.passes);
        const fn_summary = &self.analysis.fns[@backingInt(fn_id)];
        if (sameParts(fn_summary.constructed, parts) and samePasses(fn_summary.passes, passes)) return false;
        if (std.debug.runtime_safety) {
            // Summaries only grow, which is what ends the fixpoint.
            for (fn_summary.constructed) |old| {
                const kept = for (parts) |new| {
                    if (new.path == old.path and new.consumers.supersetOf(old.consumers)) break true;
                } else false;
                std.debug.assert(kept);
            }
            for (fn_summary.passes) |old| {
                const kept = for (passes) |new| {
                    if (std.meta.eql(new, old)) break true;
                } else false;
                std.debug.assert(kept);
            }
        }
        fn_summary.constructed = try self.persistent().dupe(Read, parts);
        fn_summary.passes = try self.persistent().dupe(PassThrough, passes);
        return true;
    }

    const BuiltContext = struct {
        defs: *const collections.DenseMapUnmanaged(Ast.LocalId, ?Ast.ExprId),
        slots: *const collections.DenseMapUnmanaged(Ast.LocalId, Slot),
    };

    /// `parts` with the consumers of each path merged into one read.
    fn mergedParts(self: *Builder, parts: []const Read) Allocator.Error![]const Read {
        var merged: DemandSet = .{};
        for (parts) |part| _ = try merged.addRead(self.temp(), part.path, part.consumers);
        const out = try self.temp().alloc(Read, merged.read_paths.items.len);
        for (out, merged.read_paths.items, merged.read_consumers.items) |*read, path, consumers| read.* = .{ .path = path, .consumers = consumers };
        return out;
    }

    fn distinctPasses(self: *Builder, passes: []const PassThrough) Allocator.Error![]const PassThrough {
        var distinct: std.AutoArrayHashMapUnmanaged(PassThrough, void) = .empty;
        for (passes) |pass| try distinct.put(self.temp(), pass, {});
        return distinct.keys();
    }

    fn sameParts(a: []const Read, b: []const Read) bool {
        if (a.len != b.len) return false;
        for (a) |x| {
            const found = for (b) |y| {
                if (x.path == y.path and x.consumers.eql(y.consumers)) break true;
            } else false;
            if (!found) return false;
        }
        return true;
    }

    fn samePasses(a: []const PassThrough, b: []const PassThrough) bool {
        if (a.len != b.len) return false;
        for (a) |x| {
            const found = for (b) |y| {
                if (std.meta.eql(x, y)) break true;
            } else false;
            if (!found) return false;
        }
        return true;
    }

    /// Record every local bound exactly once by a plain binding, and every
    /// `return` value.
    fn collectDefinitions(
        self: *Builder,
        body: Ast.ExprId,
        defs: *collections.DenseMapUnmanaged(Ast.LocalId, ?Ast.ExprId),
        returns: *std.ArrayList(Ast.ExprId),
    ) Allocator.Error!void {
        const program_ = self.program();
        var stack: std.ArrayList(Ast.ExprChild) = .empty;
        try stack.append(self.temp(), .{ .expr = body });
        while (stack.pop()) |child| switch (child) {
            .stmt => |stmt_id| {
                switch (program_.getStmt(stmt_id)) {
                    .let_ => |let_| try self.noteDefinition(defs, let_.pat, if (let_.recursive) null else let_.value),
                    .uninitialized => |pat| try self.noteDefinition(defs, pat, null),
                    .return_ => |ret| try returns.append(self.temp(), ret.value),
                    .expr, .expect, .dbg, .crash, .checked_error => {},
                }
                try Ast.appendStmtChildren(self.temp(), program_, stmt_id, &stack);
            },
            .expr => |expr_id| {
                switch (program_.getExpr(expr_id).data) {
                    .let_ => |let_| try self.noteDefinition(defs, let_.bind, let_.value),
                    .return_ => |ret| try returns.append(self.temp(), ret.value),
                    .comptime_value => continue,
                    .local, .unit, .@"unreachable", .int_lit, .frac_f32_lit, .frac_f64_lit, .dec_lit, .str_lit, .bytes_lit, .static_data_candidate, .typed_boundary, .row_widen, .list, .tuple, .record, .record_update, .tag, .nominal, .lambda, .def_ref, .fn_def, .fn_ref, .call_value, .call_proc, .low_level, .field_access, .tuple_access, .structural_eq, .structural_hash, .match_, .if_, .uninitialized, .uninitialized_payload, .if_initialized_payload, .try_sequence, .try_record_sequence, .block, .loop_, .break_, .continue_, .join_point, .jump, .crash, .checked_error, .comptime_exhaustiveness_failed, .dbg, .expect_err, .expect, .literal_rejected => {},
                }
                try Ast.appendChildren(self.temp(), program_, expr_id, &stack);
            },
        };
    }

    fn noteDefinition(self: *Builder, defs: *collections.DenseMapUnmanaged(Ast.LocalId, ?Ast.ExprId), pat_id: Ast.PatId, value: ?Ast.ExprId) Allocator.Error!void {
        switch (self.program().getPat(pat_id).data) {
            .bind => |local| {
                const gop = try defs.getOrPut(self.temp(), local);
                gop.value_ptr.* = if (gop.found_existing) null else value;
            },
            .wildcard, .as, .tag, .nominal, .list, .int_lit, .dec_lit, .frac_f32_lit, .frac_f64_lit, .str_lit, .str_pattern, .record, .tuple => {},
        }
    }

    fn meet(self: *Builder, a: Built, b: Built) Allocator.Error!Built {
        if (a.diverges) return b;
        if (b.diverges) return a;
        var parts: std.ArrayList(Read) = .empty;
        for (a.parts) |x| for (b.parts) |y| {
            if (x.path != y.path) continue;
            const consumers = x.consumers.intersectWith(y.consumers);
            if (consumers.count() != 0) try parts.append(self.temp(), .{ .path = x.path, .consumers = consumers });
        };
        var passes: std.ArrayList(PassThrough) = .empty;
        for (a.passes) |x| for (b.passes) |y| {
            if (std.meta.eql(x, y)) try passes.append(self.temp(), x);
        };
        return .{ .parts = parts.items, .passes = passes.items };
    }

    /// `b` as the part at `prefix` of a larger value.
    fn builtUnder(self: *Builder, b: Built, prefix: []const Step) Allocator.Error!Built {
        if (b.diverges) return b;
        const paths = &self.analysis.paths;
        const base = try paths.concat(self.persistent(), .root, prefix);
        const parts = try self.temp().alloc(Read, b.parts.len);
        for (parts, b.parts) |*out, part| {
            try paths.stepsOf(self.temp(), part.path, &self.steps);
            out.* = .{ .path = try paths.concat(self.persistent(), base, self.steps.items), .consumers = part.consumers };
        }
        const passes = try self.temp().alloc(PassThrough, b.passes.len);
        for (passes, b.passes) |*out, pass| {
            try paths.stepsOf(self.temp(), pass.result, &self.steps);
            out.* = .{ .result = try paths.concat(self.persistent(), base, self.steps.items), .slot = pass.slot, .value = pass.value };
        }
        return .{ .parts = parts, .passes = passes };
    }

    /// The part of `b` at `steps`.
    fn builtBelow(self: *Builder, b: Built, steps: []const Step) Allocator.Error!Built {
        if (b.diverges) return b;
        const paths = &self.analysis.paths;
        const base = try paths.concat(self.persistent(), .root, steps);
        var parts: std.ArrayList(Read) = .empty;
        for (b.parts) |part| {
            if (!try paths.stepsBelow(self.temp(), part.path, base, &self.steps)) continue;
            try parts.append(self.temp(), .{ .path = try paths.concat(self.persistent(), .root, self.steps.items), .consumers = part.consumers });
        }
        var passes: std.ArrayList(PassThrough) = .empty;
        for (b.passes) |pass| {
            if (try paths.stepsBelow(self.temp(), pass.result, base, &self.steps)) {
                try passes.append(self.temp(), .{ .result = try paths.concat(self.persistent(), .root, self.steps.items), .slot = pass.slot, .value = pass.value });
            } else if (try paths.stepsBelow(self.temp(), base, pass.result, &self.steps)) {
                try passes.append(self.temp(), .{ .result = .root, .slot = pass.slot, .value = try paths.concat(self.persistent(), pass.value, self.steps.items) });
            }
        }
        return .{ .parts = parts.items, .passes = passes.items };
    }

    /// `b` with the parts of `extra` added.
    fn builtWith(self: *Builder, b: Built, extra: Built) Allocator.Error!Built {
        if (extra.diverges) return b;
        var parts: std.ArrayList(Read) = .empty;
        try parts.appendSlice(self.temp(), b.parts);
        try parts.appendSlice(self.temp(), extra.parts);
        var passes: std.ArrayList(PassThrough) = .empty;
        try passes.appendSlice(self.temp(), b.passes);
        try passes.appendSlice(self.temp(), extra.passes);
        return .{ .parts = parts.items, .passes = passes.items };
    }

    /// What `root` is built from, computed bottom-up over the expressions
    /// that supply its value, each once.
    fn built(
        self: *Builder,
        root: Ast.ExprId,
        context: BuiltContext,
        memo: *collections.DenseMapUnmanaged(Ast.ExprId, Built),
    ) Allocator.Error!Built {
        const Frame = struct { expr: Ast.ExprId, expanded: bool };
        var stack: std.ArrayList(Frame) = .empty;
        var sources: std.ArrayList(Ast.ExprId) = .empty;
        try stack.append(self.temp(), .{ .expr = root, .expanded = false });
        while (stack.pop()) |frame| {
            if (memo.contains(frame.expr)) continue;
            if (!frame.expanded) {
                sources.clearRetainingCapacity();
                try self.valueSources(frame.expr, context, &sources);
                try stack.append(self.temp(), .{ .expr = frame.expr, .expanded = true });
                for (sources.items) |source| {
                    if (!memo.contains(source)) try stack.append(self.temp(), .{ .expr = source, .expanded = false });
                }
                continue;
            }
            try memo.put(self.temp(), frame.expr, try self.combine(frame.expr, context, memo));
        }
        return memo.get(root).?;
    }

    /// The expressions whose values make up `expr_id`'s value.
    fn valueSources(
        self: *Builder,
        expr_id: Ast.ExprId,
        context: BuiltContext,
        out: *std.ArrayList(Ast.ExprId),
    ) Allocator.Error!void {
        const program_ = self.program();
        const a = self.temp();
        switch (program_.getExpr(expr_id).data) {
            .typed_boundary => |boundary| try out.append(a, boundary.value),
            .row_widen => |widen| try out.append(a, widen.value),
            .nominal => |backing| try out.append(a, backing),
            .static_data_candidate => |candidate| switch (candidate.value) {
                .runtime => |runtime| try out.append(a, runtime),
                .stored => {},
            },
            .tag => |tag| {
                const payloads = program_.exprSpan(tag.payloads);
                for (0..payloads.len) |index| try out.append(a, GuardedList.at(payloads, index));
            },
            .record => |span| {
                const fields = program_.fieldExprSpan(span);
                for (0..fields.len) |index| try out.append(a, GuardedList.at(fields, index).value);
            },
            .record_update => |update| {
                const fields = program_.fieldExprSpan(update.fields);
                for (0..fields.len) |index| try out.append(a, GuardedList.at(fields, index).value);
            },
            .tuple => |span| {
                const items = program_.exprSpan(span);
                for (0..items.len) |index| try out.append(a, GuardedList.at(items, index));
            },
            .field_access => |access| try out.append(a, access.receiver),
            .tuple_access => |access| try out.append(a, access.tuple),
            .call_proc => |call| if (Ast.localDirectCallee(call) != null) {
                const args = program_.exprSpan(call.args);
                for (0..args.len) |index| try out.append(a, GuardedList.at(args, index));
                const operands = program_.captureOperandSpan(call.captures);
                for (0..operands.len) |index| try out.append(a, GuardedList.at(operands, index).value);
            },
            .local => |local| if (context.defs.get(local)) |maybe_def| if (maybe_def) |def| try out.append(a, def),
            .let_ => |let_| try out.append(a, let_.rest),
            .block => |block| try out.append(a, block.final_expr),
            .if_ => |if_| {
                const branches = program_.ifBranchSpan(if_.branches);
                for (0..branches.len) |index| try out.append(a, GuardedList.at(branches, index).body);
                try out.append(a, if_.final_else);
            },
            .match_ => |match| {
                const branches = program_.branchSpan(match.branches);
                for (0..branches.len) |index| try out.append(a, GuardedList.at(branches, index).body);
            },
            .join_point => |join| {
                try out.append(a, join.body);
                try out.append(a, join.remainder);
            },
            .unit, .@"unreachable", .int_lit, .frac_f32_lit, .frac_f64_lit, .dec_lit, .str_lit, .bytes_lit, .comptime_value, .list, .lambda, .def_ref, .fn_def, .fn_ref, .call_value, .low_level, .structural_eq, .structural_hash, .uninitialized, .uninitialized_payload, .if_initialized_payload, .try_sequence, .try_record_sequence, .loop_, .break_, .continue_, .jump, .return_, .crash, .checked_error, .comptime_exhaustiveness_failed, .dbg, .expect_err, .expect, .literal_rejected => {},
        }
    }

    fn combine(
        self: *Builder,
        expr_id: Ast.ExprId,
        context: BuiltContext,
        memo: *const collections.DenseMapUnmanaged(Ast.ExprId, Built),
    ) Allocator.Error!Built {
        const program_ = self.program();
        const a = self.temp();
        switch (program_.getExpr(expr_id).data) {
            .typed_boundary => |boundary| return memo.get(boundary.value).?,
            .row_widen => |widen| return memo.get(widen.value).?,
            .nominal => |backing| return memo.get(backing).?,
            .static_data_candidate => |candidate| return switch (candidate.value) {
                .runtime => |runtime| memo.get(runtime).?,
                // A stored candidate is an opaque leaf to the clone.
                .stored => .{},
            },
            .local => |local| {
                if (context.defs.get(local)) |maybe_def| {
                    const def = maybe_def orelse return .{};
                    return memo.get(def).?;
                }
                const slot = context.slots.get(local) orelse return .{};
                return .{ .passes = try a.dupe(PassThrough, &.{.{ .result = .root, .slot = slot, .value = .root }}) };
            },
            .let_ => |let_| return memo.get(let_.rest).?,
            .block => |block| return memo.get(block.final_expr).?,
            .if_ => |if_| {
                var result = memo.get(if_.final_else).?;
                const branches = program_.ifBranchSpan(if_.branches);
                for (0..branches.len) |index| result = try self.meet(result, memo.get(GuardedList.at(branches, index).body).?);
                return result;
            },
            .match_ => |match| {
                var result = Built.diverging;
                const branches = program_.branchSpan(match.branches);
                for (0..branches.len) |index| result = try self.meet(result, memo.get(GuardedList.at(branches, index).body).?);
                return result;
            },
            .join_point => |join| return self.meet(memo.get(join.body).?, memo.get(join.remainder).?),
            .tag => |tag| {
                var result: Built = .{ .parts = try a.dupe(Read, &.{.{ .path = .root, .consumers = Consumers.initMany(&.{ .match, .loop }) }}) };
                const payloads = program_.exprSpan(tag.payloads);
                for (0..payloads.len) |index| {
                    const payload = memo.get(GuardedList.at(payloads, index)).?;
                    result = try self.builtWith(result, try self.builtUnder(payload, &.{Step.payload(tag.name, @intCast(index))}));
                }
                return result;
            },
            .record => |span| {
                var result: Built = .{ .parts = try a.dupe(Read, &.{.{ .path = .root, .consumers = Consumers.initMany(&.{ .loop, .project }) }}) };
                const fields = program_.fieldExprSpan(span);
                for (0..fields.len) |index| {
                    const field = GuardedList.at(fields, index);
                    result = try self.builtWith(result, try self.builtUnder(memo.get(field.value).?, &.{Step.field(field.name)}));
                }
                return result;
            },
            .record_update => |update| {
                var result: Built = .{ .parts = try a.dupe(Read, &.{.{ .path = .root, .consumers = Consumers.initMany(&.{ .loop, .project }) }}) };
                const fields = program_.fieldExprSpan(update.fields);
                for (0..fields.len) |index| {
                    const field = GuardedList.at(fields, index);
                    result = try self.builtWith(result, try self.builtUnder(memo.get(field.value).?, &.{Step.field(field.name)}));
                }
                return result;
            },
            .tuple => |span| {
                var result: Built = .{ .parts = try a.dupe(Read, &.{.{ .path = .root, .consumers = Consumers.initMany(&.{ .loop, .project }) }}) };
                const items = program_.exprSpan(span);
                for (0..items.len) |index| {
                    result = try self.builtWith(result, try self.builtUnder(memo.get(GuardedList.at(items, index)).?, &.{Step.item(@intCast(index))}));
                }
                return result;
            },
            .field_access => |access| {
                const segments = program_.fieldAccessSegmentSpan(access.segments);
                var steps: std.ArrayList(Step) = .empty;
                for (0..segments.len) |index| try appendStep(a, &steps, Step.field(GuardedList.at(segments, index).field));
                return self.builtBelow(memo.get(access.receiver).?, steps.items);
            },
            .tuple_access => |access| return self.builtBelow(memo.get(access.tuple).?, &.{Step.item(access.elem_index)}),
            .fn_ref => return .{ .parts = try a.dupe(Read, &.{.{ .path = .root, .consumers = Consumers.initOne(.call) }}) },
            .call_proc => |call| {
                const callee = Ast.localDirectCallee(call) orelse return .{};
                const callee_summary = self.analysis.summary(callee);
                var result: Built = .{ .parts = callee_summary.constructed };
                const args = program_.exprSpan(call.args);
                const operands = program_.captureOperandSpan(call.captures);
                for (callee_summary.passes) |pass| {
                    const source = switch (pass.slot.kind) {
                        .arg => GuardedList.at(args, pass.slot.index),
                        .capture => for (0..operands.len) |index| {
                            const operand = GuardedList.at(operands, index);
                            if (self.analysis.captureSlot(callee, operand.id) == pass.slot.index) break operand.value;
                        } else Common.invariant("direct call supplied no operand for a capture its callee passes through"),
                    };
                    try self.analysis.paths.stepsOf(a, pass.value, &self.rest);
                    const part = try self.builtBelow(memo.get(source).?, try a.dupe(Step, self.rest.items));
                    try self.analysis.paths.stepsOf(a, pass.result, &self.rest);
                    result = try self.builtWith(result, try self.builtUnder(part, try a.dupe(Step, self.rest.items)));
                }
                return result;
            },
            .@"unreachable", .return_, .jump, .break_, .continue_, .crash, .checked_error, .comptime_exhaustiveness_failed, .literal_rejected, .expect_err => return Built.diverging,
            .unit, .int_lit, .frac_f32_lit, .frac_f64_lit, .dec_lit, .str_lit, .bytes_lit, .comptime_value, .list, .call_value, .low_level, .structural_eq, .structural_hash, .uninitialized, .uninitialized_payload, .if_initialized_payload, .try_sequence, .try_record_sequence, .loop_, .dbg, .expect => return .{},
            .lambda, .def_ref, .fn_def => Common.invariant("pre-lift function expression reached inline productivity"),
        }
    }
};
