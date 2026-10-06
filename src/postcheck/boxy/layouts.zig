//! Boxy runtime layout planning.
//!
//! This stage consumes the target-independent boxy representation plan and
//! commits storage layouts into the LIR layout store. Dynamic boxy values keep
//! their descriptor-governed meaning in this table instead of encoding it in
//! the ordinary layout id; the layout id only describes storage width,
//! alignment, and aggregate placement.

const std = @import("std");
const base = @import("base");
const check = @import("check");
const collections = @import("collections");
const layout = @import("layout");

const Common = @import("../common.zig");
const Plan = @import("plan.zig");
const test_fixtures = @import("test_fixtures.zig");

/// Shared Boxy stage-test fixtures, aliased so every stage builds the same
/// synthetic checked payloads from one definition.
const fixtureTableIndex = test_fixtures.tableIndex;
const builtinNominal = test_fixtures.builtinNominal;

const Allocator = std.mem.Allocator;
const checked = check.CheckedModule;
const checked_names = check.CanonicalNames;
const RecordFieldLabelId = @TypeOf(@as(checked.CheckedRecordField, undefined).name);
const TagLabelId = @TypeOf(@as(checked.CheckedTag, undefined).name);

/// Storage layout and descriptor requirement for a dynamic Boxy value.
pub const DynamicBoxLayout = struct {
    storage_layout: layout.Idx,
    desc: Plan.DescriptorRequirementId,
};

/// Concrete or descriptor-governed storage committed for one representation.
pub const RuntimeLayout = union(enum) {
    concrete: layout.Idx,
    dynamic_box: DynamicBoxLayout,

    pub fn layoutIdx(self: RuntimeLayout) layout.Idx {
        return switch (self) {
            .concrete => |idx| idx,
            .dynamic_box => |dynamic| dynamic.storage_layout,
        };
    }

    pub fn descriptor(self: RuntimeLayout) ?Plan.DescriptorRequirementId {
        return switch (self) {
            .concrete => null,
            .dynamic_box => |dynamic| dynamic.desc,
        };
    }
};

/// Storage and descriptor-payload layouts for one planned representation.
pub const RepLayouts = struct {
    worker: RuntimeLayout,
    descriptor_payload_layout: ?layout.Idx = null,
};

/// Committed argument, capture, return, and value layouts for one worker.
pub const WorkerLayouts = struct {
    worker: Plan.WorkerPlanId,
    args: Plan.Span = .{},
    hidden_descs: Plan.Span = .{},
    hidden_dicts: Plan.Span = .{},
    /// One layout per valued context input, after the hidden dictionaries.
    context: Plan.Span = .{},
    erased_capture_layout: layout.Idx = .zst,
    ret: ?RuntimeLayout = null,
    value: RuntimeLayout,
};

/// Host-facing layouts committed for one requested root.
pub const RootLayouts = struct {
    root: Plan.RootPlanId,
    worker: Plan.WorkerPlanId,
    host_args: Plan.Span = .{},
    host_ret: ?RuntimeLayout = null,
    host_value: ?RuntimeLayout = null,
};

/// Fixed compiler-owned layouts used by generated parser evidence values.
pub const GeneratedEvidenceLayouts = struct {
    field: layout.Idx,
    field_list: layout.Idx,
    field_names: layout.Idx,
    field_names_list: layout.Idx,
    tag_union_spec: layout.Idx,
};

/// Complete target-specific layout assignment for a Boxy program plan.
pub const LayoutPlan = struct {
    allocator: Allocator,
    rep_layouts: []RepLayouts,
    worker_layouts: []WorkerLayouts,
    worker_layout_values: std.ArrayList(RuntimeLayout),
    roots: std.ArrayList(RootLayouts),
    root_layout_values: std.ArrayList(RuntimeLayout),
    dynamic_storage_layout: layout.Idx,
    generated_evidence: GeneratedEvidenceLayouts,

    pub fn deinit(self: *LayoutPlan) void {
        self.root_layout_values.deinit(self.allocator);
        self.roots.deinit(self.allocator);
        self.worker_layout_values.deinit(self.allocator);
        self.allocator.free(self.worker_layouts);
        self.allocator.free(self.rep_layouts);
        self.* = undefined;
    }

    pub fn workerLayoutFor(self: *const LayoutPlan, worker: Plan.WorkerPlanId) WorkerLayouts {
        const index = @intFromEnum(worker);
        if (index >= self.worker_layouts.len) boxyLayoutInvariant("worker layout id exceeded worker layout table");
        const layouts = self.worker_layouts[index];
        if (layouts.worker != worker) boxyLayoutInvariant("worker layout table disagreed with worker plan order");
        return layouts;
    }

    pub fn workerLayoutSlice(self: *const LayoutPlan, span: Plan.Span) []const RuntimeLayout {
        return self.worker_layout_values.items[span.start .. span.start + span.len];
    }

    pub fn rootLayoutSlice(self: *const LayoutPlan, span: Plan.Span) []const RuntimeLayout {
        return self.root_layout_values.items[span.start .. span.start + span.len];
    }
};

/// Configuration for committing a Boxy representation plan to layouts.
pub const BuildOptions = struct {};

/// Commit target layouts for every representation, worker, and root in a plan.
pub fn build(
    allocator: Allocator,
    program: *const Plan.ProgramPlan,
    store: *layout.Store,
    _: BuildOptions,
) Allocator.Error!LayoutPlan {
    var builder = Builder.init(allocator, program, store);
    defer builder.deinit();
    return try builder.finish();
}

/// Commit only checked ABI roots. No worker descriptors, generated evidence
/// layouts, or LIR procedures are constructed for layout-only clients.
pub fn commitHostAbi(allocator: Allocator, program: *const Plan.ProgramPlan, store: *layout.Store) Allocator.Error![]layout.Idx {
    var builder = Builder.init(allocator, program, store);
    defer builder.deinit();
    builder.caches = try allocator.alloc(?RuntimeLayout, program.representations.items.len);
    @memset(builder.caches, null);
    const roots = try allocator.alloc(layout.Idx, program.root_reps.items.len);
    errdefer allocator.free(roots);
    for (program.root_reps.items, roots) |rep, *root| root.* = (try builder.runtimeLayoutForRep(rep)).layoutIdx();
    return roots;
}

const Builder = struct {
    allocator: Allocator,
    program: *const Plan.ProgramPlan,
    store: *layout.Store,
    caches: []?RuntimeLayout,
    graph_nodes: collections.DenseMap(Plan.TypeRepId, layout.GraphNodeId),
    worker_layout_values: std.ArrayList(RuntimeLayout),
    root_layouts: std.ArrayList(RootLayouts),
    root_layout_values: std.ArrayList(RuntimeLayout),
    dynamic_storage_layout: ?layout.Idx = null,
    generated_field_layout: ?layout.Idx = null,
    generated_field_list_layout: ?layout.Idx = null,
    generated_field_names_layout: ?layout.Idx = null,
    generated_field_names_list_layout: ?layout.Idx = null,
    generated_tag_union_spec_layout: ?layout.Idx = null,

    /// Shared read-only queries over the representation plan.
    fn repQuery(self: *const Builder) Plan.RepQuery {
        return .{ .plan = self.program, .allocator = self.allocator };
    }

    fn init(allocator: Allocator, program: *const Plan.ProgramPlan, store: *layout.Store) Builder {
        return .{
            .allocator = allocator,
            .program = program,
            .store = store,
            .caches = &.{},
            .graph_nodes = collections.DenseMap(Plan.TypeRepId, layout.GraphNodeId).init(allocator),
            .worker_layout_values = .empty,
            .root_layouts = .empty,
            .root_layout_values = .empty,
        };
    }

    fn deinit(self: *Builder) void {
        self.root_layout_values.deinit(self.allocator);
        self.root_layouts.deinit(self.allocator);
        self.worker_layout_values.deinit(self.allocator);
        self.allocator.free(self.caches);
        self.graph_nodes.deinit();
    }

    fn finish(self: *Builder) Allocator.Error!LayoutPlan {
        self.caches = try self.allocator.alloc(?RuntimeLayout, self.program.representations.items.len);
        @memset(self.caches, null);

        for (self.program.representations.items, 0..) |_, index| {
            _ = try self.runtimeLayoutForRep(@enumFromInt(index));
        }
        for (self.program.roots.items) |root| {
            try self.appendRoot(root);
        }

        const rep_layouts = try self.allocator.alloc(RepLayouts, self.program.representations.items.len);
        errdefer self.allocator.free(rep_layouts);
        for (rep_layouts, 0..) |*out, index| {
            const rep_id: Plan.TypeRepId = @enumFromInt(index);
            const worker = self.caches[index] orelse boxyLayoutInvariant("worker layout cache was not populated");
            out.* = .{
                .worker = worker,
                .descriptor_payload_layout = try self.descriptorPayloadLayout(rep_id),
            };
        }

        const worker_layouts = try self.allocator.alloc(WorkerLayouts, self.program.workers.items.len);
        errdefer self.allocator.free(worker_layouts);
        for (self.program.workers.items, worker_layouts) |worker, *out| {
            out.* = try self.layoutForWorker(worker);
        }

        const dynamic_storage_layout = try self.dynamicStorageLayout();
        const generated_evidence = GeneratedEvidenceLayouts{
            .field = try self.generatedFieldLayout(),
            .field_list = try self.generatedFieldListLayout(),
            .field_names = try self.generatedFieldNamesLayout(),
            .field_names_list = try self.generatedFieldNamesListLayout(),
            .tag_union_spec = try self.generatedTagUnionSpecLayout(),
        };
        const worker_layout_values = self.worker_layout_values;
        const roots = self.root_layouts;
        const root_layout_values = self.root_layout_values;
        self.worker_layout_values = .empty;
        self.root_layouts = .empty;
        self.root_layout_values = .empty;

        return .{
            .allocator = self.allocator,
            .rep_layouts = rep_layouts,
            .worker_layouts = worker_layouts,
            .worker_layout_values = worker_layout_values,
            .roots = roots,
            .root_layout_values = root_layout_values,
            .dynamic_storage_layout = dynamic_storage_layout,
            .generated_evidence = generated_evidence,
        };
    }

    fn layoutForWorker(self: *Builder, worker: Plan.WorkerPlan) Allocator.Error!WorkerLayouts {
        const worker_value = try self.runtimeLayoutForRep(worker.rep);
        var worker_layout: WorkerLayouts = .{
            .worker = worker.id,
            .value = worker_value,
        };

        if (self.repQuery().functionChildren(worker.rep)) |function| {
            const start = self.layoutValueStart(&self.worker_layout_values);
            try self.appendFunctionLayouts(&self.worker_layout_values, function);
            worker_layout.args = self.layoutSpanFrom(start, function.arg_count);
            worker_layout.ret = self.worker_layout_values.items[start + function.arg_count];
        }
        const hidden_descs = self.program.hiddenDescriptorParamSlice(worker.hidden_descs);
        if (hidden_descs.len != 0) {
            const hidden_start = self.layoutValueStart(&self.worker_layout_values);
            for (hidden_descs) |_| {
                try self.worker_layout_values.append(self.allocator, .{ .concrete = .opaque_ptr });
            }
            worker_layout.hidden_descs = self.layoutSpanFrom(hidden_start, @intCast(hidden_descs.len));
        }
        const hidden_dicts = self.program.hiddenDictionaryParamSlice(worker.hidden_dicts);
        if (hidden_dicts.len != 0) {
            const hidden_start = self.layoutValueStart(&self.worker_layout_values);
            for (hidden_dicts) |_| {
                try self.worker_layout_values.append(self.allocator, .{ .concrete = .opaque_ptr });
            }
            worker_layout.hidden_dicts = self.layoutSpanFrom(hidden_start, @intCast(hidden_dicts.len));
        }
        const context = self.program.contextInputSlice(worker.context);
        if (context.len != 0) {
            const context_start = self.layoutValueStart(&self.worker_layout_values);
            var value_count: u32 = 0;
            for (context) |input| {
                const rep = input.rep orelse continue;
                try self.worker_layout_values.append(self.allocator, try self.runtimeLayoutForRep(rep));
                value_count += 1;
            }
            worker_layout.context = self.layoutSpanFrom(context_start, value_count);
        }
        worker_layout.erased_capture_layout = try self.erasedCaptureLayout(worker.erased_captures);

        return worker_layout;
    }

    fn erasedCaptureLayout(self: *Builder, span: Plan.Span) Allocator.Error!layout.Idx {
        const captures = self.program.erasedCaptureSlice(span);
        if (captures.len == 0) return .zst;

        const fields = try self.allocator.alloc(layout.StructField, captures.len);
        defer self.allocator.free(fields);
        for (captures, fields, 0..) |capture, *field, index| {
            const field_layout: layout.Idx = switch (capture.kind) {
                .captured_value => (try self.runtimeLayoutForRep(capture.rep)).layoutIdx(),
                .hidden_desc, .hidden_dict, .hidden_literal => .opaque_ptr,
            };
            field.* = .{ .index = @intCast(index), .layout = field_layout };
        }
        return try self.store.putStructFields(fields);
    }

    fn appendRoot(self: *Builder, root: Plan.RootPlan) Allocator.Error!void {
        var root_layout: RootLayouts = .{
            .root = root.id,
            .worker = root.worker,
        };

        if (self.repQuery().functionChildren(if (root.wrapper_kind == .host_shaped_wrapper) self.program.hostRepFor(root.host_rep) else root.host_rep)) |function| {
            if (root.wrapper_kind == .host_shaped_wrapper) {
                const host_start = self.layoutValueStart(&self.root_layout_values);
                try self.appendFunctionLayouts(&self.root_layout_values, function);
                root_layout.host_args = self.layoutSpanFrom(host_start, function.arg_count);
                root_layout.host_ret = self.root_layout_values.items[host_start + function.arg_count];
            }
        } else if (root.wrapper_kind == .host_shaped_wrapper) {
            root_layout.host_value = try self.runtimeLayoutForRep(self.program.hostRepFor(root.host_rep));
        }

        try self.root_layouts.append(self.allocator, root_layout);
    }

    fn appendFunctionLayouts(
        self: *Builder,
        values: *std.ArrayList(RuntimeLayout),
        function: Plan.FunctionChildren,
    ) Allocator.Error!void {
        const identity_children = self.program.childSlice(self.program.representations.items[@intFromEnum(function.rep)].children);
        for (identity_children[function.args_start..][0..function.arg_count]) |child| {
            try values.append(self.allocator, try self.runtimeLayoutForRep(child.rep));
        }
        try values.append(self.allocator, try self.runtimeLayoutForRep(function.ret));
    }

    fn layoutValueStart(_: *const Builder, values: *const std.ArrayList(RuntimeLayout)) u32 {
        return @intCast(values.items.len);
    }

    fn layoutSpanFrom(_: *const Builder, start: u32, len: u32) Plan.Span {
        return .{ .start = start, .len = len };
    }

    /// A representation's runtime layout. A representation whose layout is
    /// another's (a builtin nominal's backing, or a box's dynamic or callable
    /// payload) shares it; the whole chain is cached.
    fn runtimeLayoutForRep(self: *Builder, root: Plan.TypeRepId) Allocator.Error!RuntimeLayout {
        var chain: std.ArrayList(Plan.TypeRepId) = .empty;
        defer chain.deinit(self.allocator);
        var rep_id = root;
        const runtime = while (true) {
            if (self.caches[@intFromEnum(rep_id)]) |cached| break cached;
            switch (try self.immediateRuntimeStep(rep_id)) {
                .layout => |immediate| {
                    self.caches[@intFromEnum(rep_id)] = immediate;
                    break immediate;
                },
                .same_as => |next| {
                    try chain.append(self.allocator, rep_id);
                    rep_id = next;
                },
                .graph => break try self.graphRuntimeLayout(rep_id),
            }
        };
        for (chain.items) |shared| self.caches[@intFromEnum(shared)] = runtime;
        return runtime;
    }

    /// The runtime layout of a representation built as a layout graph.
    fn graphRuntimeLayout(self: *Builder, rep_id: Plan.TypeRepId) Allocator.Error!RuntimeLayout {
        const index = @intFromEnum(rep_id);

        var graph = layout.Graph{};
        defer graph.deinit(self.allocator);
        const local_nodes = &self.graph_nodes;
        local_nodes.clearRetainingCapacity();

        var graph_builder = GraphBuilder{
            .parent = self,
            .graph = &graph,
            .local_nodes = local_nodes,
        };
        const root = try graph_builder.inputForRep(rep_id);
        var commit = try self.store.commitGraph(&graph, root);
        defer commit.deinit(self.allocator);

        const root_layout_idx = switch (root) {
            .canonical => |layout_idx| layout_idx,
            .local => |node| commit.value_layouts[@intFromEnum(node)],
        };
        const runtime: RuntimeLayout = .{ .concrete = root_layout_idx };
        self.caches[index] = runtime;

        var nodes = local_nodes.iterator();
        while (nodes.next()) |entry| {
            self.caches[@intFromEnum(entry.key_ptr.*)] = .{ .concrete = commit.value_layouts[@intFromEnum(entry.value_ptr.*)] };
        }

        return self.caches[index].?;
    }

    /// A representation's runtime layout when it needs no layout graph of
    /// its own, or null.
    fn immediateRuntimeLayout(self: *Builder, rep_id: Plan.TypeRepId) Allocator.Error!?RuntimeLayout {
        return switch (try self.immediateRuntimeStep(rep_id)) {
            .layout => |runtime| runtime,
            .same_as => |next| try self.runtimeLayoutForRep(next),
            .graph => null,
        };
    }

    const ImmediateRuntimeStep = union(enum) {
        layout: RuntimeLayout,
        /// The representation has this other representation's layout.
        same_as: Plan.TypeRepId,
        /// The representation's layout is a layout graph.
        graph,
    };

    fn immediateRuntimeStep(self: *Builder, rep_id: Plan.TypeRepId) Allocator.Error!ImmediateRuntimeStep {
        const rep = self.program.representations.items[@intFromEnum(rep_id)];
        if (rep.abi_boxed_backing) return .graph;
        return switch (rep.kind) {
            .in_progress => boxyLayoutInvariant("in-progress representation reached boxy layout planning"),
            .dynamic => .{ .layout = .{ .dynamic_box = .{
                .storage_layout = try self.dynamicStorageLayout(),
                .desc = rep.descriptor orelse boxyLayoutInvariant("dynamic layout had no descriptor requirement"),
            } } },
            .primitive => |primitive| .{ .layout = .{ .concrete = Common.primitiveLayout(primitive) } },
            .bool_tag_union => .{ .layout = .{ .concrete = .bool } },
            .empty_record, .empty_tag_union => .{ .layout = .{ .concrete = .zst } },
            .erased_callable => .{ .layout = .{ .concrete = try self.store.insertErasedCallable() } },
            .generated_field => .{ .layout = .{ .concrete = try self.generatedFieldLayout() } },
            .generated_field_names => .{ .layout = .{ .concrete = try self.generatedFieldNamesLayout() } },
            .generated_tag_union_spec => .{ .layout = .{ .concrete = try self.generatedTagUnionSpecLayout() } },
            .box => self.boxRuntimeStep(rep_id),
            .nominal => |kind| switch (kind) {
                .opaque_nominal => .{ .layout = .{ .concrete = try self.dynamicStorageLayout() } },
                .builtin_other => if (self.singleChild(rep_id, .nominal_backing)) |child|
                    .{ .same_as = child.rep }
                else
                    .graph,
                .transparent => .graph,
            },
            .alias,
            .record,
            .tuple,
            .list,
            .tag_union,
            => .graph,
        };
    }

    fn generatedFieldLayout(self: *Builder) Allocator.Error!layout.Idx {
        if (self.generated_field_layout) |existing| return existing;
        const committed = try self.store.putStructFields(&.{
            .{ .index = 0, .layout = .str },
            .{ .index = 1, .layout = .u64 },
            .{ .index = 2, .layout = .u64 },
        });
        self.generated_field_layout = committed;
        return committed;
    }

    fn generatedFieldNamesLayout(self: *Builder) Allocator.Error!layout.Idx {
        if (self.generated_field_names_layout) |existing| return existing;
        const committed = try self.store.putStructFields(&.{
            .{ .index = 0, .layout = try self.generatedFieldListLayout() },
            .{ .index = 1, .layout = .u64 },
            .{ .index = 2, .layout = .u64 },
        });
        self.generated_field_names_layout = committed;
        return committed;
    }

    fn generatedTagUnionSpecLayout(self: *Builder) Allocator.Error!layout.Idx {
        if (self.generated_tag_union_spec_layout) |existing| return existing;
        const committed = try self.store.putStructFields(&.{
            .{ .index = 0, .layout = try self.generatedFieldNamesListLayout() },
            .{ .index = 1, .layout = .u64 },
        });
        self.generated_tag_union_spec_layout = committed;
        return committed;
    }

    fn generatedFieldListLayout(self: *Builder) Allocator.Error!layout.Idx {
        if (self.generated_field_list_layout) |existing| return existing;
        const committed = try self.store.insertList(try self.generatedFieldLayout());
        self.generated_field_list_layout = committed;
        return committed;
    }

    fn generatedFieldNamesListLayout(self: *Builder) Allocator.Error!layout.Idx {
        if (self.generated_field_names_list_layout) |existing| return existing;
        const committed = try self.store.insertList(try self.generatedFieldNamesLayout());
        self.generated_field_names_list_layout = committed;
        return committed;
    }

    fn boxRuntimeStep(self: *Builder, rep_id: Plan.TypeRepId) ImmediateRuntimeStep {
        const child = self.repQuery().requiredSingleChild(rep_id, .box_payload);
        // Box payloads reach here behind alias chains (`I64ToI64 : I64 -> I64`);
        // the erased-callable collapse below is a host ABI convention keyed on
        // the underlying value type, so resolve aliases before classifying.
        const payload_rep_id = self.aliasResolvedRep(child.rep);
        const child_rep = self.program.representations.items[@intFromEnum(payload_rep_id)];
        if (child_rep.kind == .dynamic) return .{ .same_as = payload_rep_id };
        // A boxed erased callable is one flat refcounted allocation whose
        // data pointer IS the callable value (see builtins.erased_callable),
        // so Box(fn) shares the callable's layout instead of boxing it.
        if (child_rep.kind == .erased_callable) return .{ .same_as = payload_rep_id };
        return .graph;
    }

    fn aliasResolvedRep(self: *Builder, rep_id: Plan.TypeRepId) Plan.TypeRepId {
        var current = rep_id;
        while (true) {
            const rep = self.program.representations.items[@intFromEnum(current)];
            if (rep.kind != .alias) return current;
            current = self.repQuery().requiredSingleChild(current, .alias_backing).rep;
        }
    }

    /// The payload layout a descriptor for `root` describes. A wrapper's
    /// payload is its backing's; a backing without a descriptor of its own
    /// is described by its runtime layout.
    fn descriptorPayloadLayout(self: *Builder, root: Plan.TypeRepId) Allocator.Error!?layout.Idx {
        var rep_id = root;
        var at_root = true;
        while (true) : (at_root = false) {
            const rep = self.program.representations.items[@intFromEnum(rep_id)];
            if (rep.descriptor == null) {
                if (at_root) return null;
                return (try self.runtimeLayoutForRep(rep_id)).layoutIdx();
            }
            const backing: ?Plan.TypeRepId = if (rep.kind == .alias)
                self.repQuery().requiredSingleChild(rep_id, .alias_backing).rep
            else if (rep.kind == .nominal) switch (rep.kind.nominal) {
                .transparent => if (rep.declared_fields.len == 0)
                    self.repQuery().requiredSingleChild(rep_id, .nominal_backing).rep
                else
                    null,
                .builtin_other => if (self.singleChild(rep_id, .nominal_backing)) |child| child.rep else null,
                .opaque_nominal => null,
            } else null;
            if (backing) |next| {
                rep_id = next;
                continue;
            }
            if (rep.kind == .dynamic and rep.tag_variants.len != 0) {
                return try self.aggregatePayloadLayout(.tag_union, rep_id);
            }
            if (rep.kind == .dynamic and repHasRecordFields(self.program, rep)) {
                return try self.aggregatePayloadLayout(.record, rep_id);
            }
            return (try self.runtimeLayoutForRep(rep_id)).layoutIdx();
        }
    }

    /// The descriptor payload layout of the record or tag union `rep_id`.
    fn aggregatePayloadLayout(self: *Builder, comptime shape: enum { record, tag_union }, rep_id: Plan.TypeRepId) Allocator.Error!layout.Idx {
        var graph = layout.Graph{};
        defer graph.deinit(self.allocator);

        const local_nodes = &self.graph_nodes;
        local_nodes.clearRetainingCapacity();

        var graph_builder = GraphBuilder{
            .parent = self,
            .descriptor_payload = true,
            .graph = &graph,
            .local_nodes = local_nodes,
        };
        const root = try graph.reserveNode(self.allocator);
        try local_nodes.put(rep_id, root);
        try graph_builder.buildNode(.{ .state = switch (shape) {
            .record => .{ .fields = .{ .node = root, .rep_id = rep_id, .kind = .record } },
            .tag_union => .{ .tag = .{ .node = root, .rep_id = rep_id, .mode = .descriptor_payload } },
        } });

        var commit = try self.store.commitGraph(&graph, .{ .local = root });
        defer commit.deinit(self.allocator);
        return commit.value_layouts[@intFromEnum(root)];
    }

    fn dynamicStorageLayout(self: *Builder) Allocator.Error!layout.Idx {
        if (self.dynamic_storage_layout) |existing| return existing;
        const idx = try self.store.insertErasedBox();
        self.dynamic_storage_layout = idx;
        return idx;
    }

    fn singleChild(self: *Builder, rep_id: Plan.TypeRepId, role: Plan.ChildRole) ?Plan.RepChild {
        var found: ?Plan.RepChild = null;
        const rep = self.program.representations.items[@intFromEnum(rep_id)];
        for (self.program.childSlice(rep.children)) |child| {
            if (Plan.sameChildRoleKind(child.role, role)) {
                if (found != null) boxyLayoutInvariant("representation had duplicate required child role");
                found = child;
            }
        }
        return found;
    }
};

const GraphBuilder = struct {
    parent: *Builder,
    descriptor_payload: bool = false,
    graph: *layout.Graph,
    local_nodes: *collections.DenseMap(Plan.TypeRepId, layout.GraphNodeId),

    // A node's contents are built from its children's inputs, and children
    // follow type nesting, so each node still waiting on a child input is a
    // frame on one heap-backed stack. Nodes are reserved, and field and ref
    // spans appended, in the order a direct recursive build would.

    const TagPayloadMode = enum {
        concrete_runtime,
        descriptor_payload,
    };

    /// A graph node waiting on its children's inputs.
    const GraphFrame = struct {
        /// Whether the frame's last requested child input is still due.
        awaiting: bool = false,
        state: union(enum) {
            /// A node with one child input.
            single: struct { node: layout.GraphNodeId, kind: enum { list, box, nominal }, child: Plan.TypeRepId },
            /// A record's field nodes, or a tuple's element nodes.
            fields: struct {
                node: layout.GraphNodeId,
                rep_id: Plan.TypeRepId,
                kind: enum { record, tuple },
                fields: std.ArrayList(layout.GraphField) = .empty,
                next: usize = 0,
            },
            /// A transparent nominal's declared fields.
            declared: struct {
                node: layout.GraphNodeId,
                rep_id: Plan.TypeRepId,
                fields: []layout.GraphField,
                next: usize = 0,
            },
            tag: struct {
                node: layout.GraphNodeId,
                rep_id: Plan.TypeRepId,
                mode: TagPayloadMode,
                refs: std.ArrayList(layout.GraphInput) = .empty,
                variant: usize = 0,
            },
            /// A variant's several payloads, as a struct reserved once they
            /// are built.
            payload: struct {
                payloads: []Plan.TypeRepId,
                fields: []layout.GraphField,
                next: usize = 0,
            },
        },

        fn deinit(self: *GraphFrame, allocator: Allocator) void {
            switch (self.state) {
                .single => {},
                .fields => |*fields| fields.fields.deinit(allocator),
                .declared => |declared| allocator.free(declared.fields),
                .tag => |*tag| tag.refs.deinit(allocator),
                .payload => |payload| {
                    allocator.free(payload.payloads);
                    allocator.free(payload.fields);
                },
            }
        }
    };

    const GraphStep = union(enum) {
        /// The frame needs this representation's input next.
        request: Plan.TypeRepId,
        /// The frame pushed a frame of its own to run next.
        pushed,
        /// The frame finished with this input and was popped.
        done: layout.GraphInput,
    };

    fn inputForRep(self: *GraphBuilder, rep_id: Plan.TypeRepId) Allocator.Error!layout.GraphInput {
        var frames: std.ArrayList(GraphFrame) = .empty;
        defer {
            for (frames.items) |*frame| frame.deinit(self.parent.allocator);
            frames.deinit(self.parent.allocator);
        }
        const input = try self.beginInput(rep_id, &frames);
        return try self.runGraphFrames(&frames, input);
    }

    /// Build the node `frame` describes, which the caller reserved.
    fn buildNode(self: *GraphBuilder, frame: GraphFrame) Allocator.Error!void {
        var frames: std.ArrayList(GraphFrame) = .empty;
        defer {
            for (frames.items) |*pending| pending.deinit(self.parent.allocator);
            frames.deinit(self.parent.allocator);
        }
        try frames.append(self.parent.allocator, frame);
        _ = try self.runGraphFrames(&frames, null);
    }

    /// Run `frames` to completion; the result is the input the bottom frame
    /// finished with, or `first` when there were no frames.
    fn runGraphFrames(self: *GraphBuilder, frames: *std.ArrayList(GraphFrame), first: ?layout.GraphInput) Allocator.Error!layout.GraphInput {
        var delivered = first;
        while (frames.items.len != 0) {
            switch (try self.stepGraphFrame(frames, delivered)) {
                .request => |child| delivered = try self.beginInput(child, frames),
                .pushed => delivered = null,
                .done => |input| delivered = input,
            }
        }
        return delivered orelse boxyLayoutInvariant("boxy layout graph finished without an input");
    }

    /// The input for `rep_id` when it needs no new node, or null with the
    /// frame that builds its node pushed.
    fn beginInput(self: *GraphBuilder, root_rep_id: Plan.TypeRepId, frames: *std.ArrayList(GraphFrame)) Allocator.Error!?layout.GraphInput {
        var rep_id = root_rep_id;
        while (true) {
            const index = @intFromEnum(rep_id);
            if (self.local_nodes.get(rep_id)) |node| return .{ .local = node };

            const rep = self.parent.program.representations.items[index];
            if (self.descriptor_payload) {
                if (rep.kind == .alias) {
                    rep_id = self.parent.repQuery().requiredSingleChild(rep_id, .alias_backing).rep;
                    continue;
                }
                if (rep.kind == .nominal) {
                    switch (rep.kind.nominal) {
                        .transparent => {
                            if (rep.declared_fields.len == 0) {
                                rep_id = self.parent.repQuery().requiredSingleChild(rep_id, .nominal_backing).rep;
                                continue;
                            }
                        },
                        .opaque_nominal, .builtin_other => {},
                    }
                }
                if (rep.kind == .dynamic and rep.descriptor != null) {
                    if (rep.tag_variants.len != 0) {
                        const node = try self.reserveLocalNode(rep_id);
                        try self.pushGraphFrame(frames, .{ .tag = .{ .node = node, .rep_id = rep_id, .mode = .descriptor_payload } });
                        return null;
                    }
                    if (repHasRecordFields(self.parent.program, rep)) {
                        const node = try self.reserveLocalNode(rep_id);
                        try self.pushGraphFrame(frames, .{ .fields = .{ .node = node, .rep_id = rep_id, .kind = .record } });
                        return null;
                    }
                }
            }

            if (self.parent.caches[index]) |runtime| return .{ .canonical = runtime.layoutIdx() };
            if (rep.abi_boxed_backing) {
                const node = try self.reserveLocalNode(rep_id);
                try self.pushGraphFrame(frames, .{ .single = .{
                    .node = node,
                    .kind = .box,
                    .child = self.parent.repQuery().requiredSingleChild(rep_id, .nominal_backing).rep,
                } });
                return null;
            }

            // Stay in this graph when opening nominal wrappers. Calling the
            // top-level resolver here would overwrite its reusable graph scratch.
            if (rep.kind == .nominal and rep.kind.nominal == .builtin_other) {
                rep_id = self.parent.repQuery().requiredSingleChild(rep_id, .nominal_backing).rep;
                continue;
            }
            if (try self.parent.immediateRuntimeLayout(rep_id)) |runtime| {
                self.parent.caches[index] = runtime;
                return .{ .canonical = runtime.layoutIdx() };
            }

            if (rep.kind == .alias) {
                rep_id = self.parent.repQuery().requiredSingleChild(rep_id, .alias_backing).rep;
                continue;
            }
            if (rep.kind == .nominal) {
                switch (rep.kind.nominal) {
                    .transparent => {
                        const needs_field_node = rep.record_field_order == .declared or
                            (self.descriptor_payload and rep.declared_fields.len != 0);
                        if (needs_field_node) {
                            const node = try self.reserveLocalNode(rep_id);
                            const fields = try self.parent.allocator.alloc(layout.GraphField, rep.declared_fields.len);
                            errdefer self.parent.allocator.free(fields);
                            try self.pushGraphFrame(frames, .{ .declared = .{ .node = node, .rep_id = rep_id, .fields = fields } });
                            return null;
                        }
                        rep_id = self.parent.repQuery().requiredSingleChild(rep_id, .nominal_backing).rep;
                        continue;
                    },
                    .opaque_nominal, .builtin_other => {},
                }
            }

            const node = try self.reserveLocalNode(rep_id);
            try self.pushGraphFrame(frames, switch (rep.kind) {
                .record => .{ .fields = .{ .node = node, .rep_id = rep_id, .kind = .record } },
                .tuple => .{ .fields = .{ .node = node, .rep_id = rep_id, .kind = .tuple } },
                .list => .{ .single = .{ .node = node, .kind = .list, .child = self.parent.repQuery().requiredSingleChild(rep_id, .list_elem).rep } },
                .box => .{ .single = .{ .node = node, .kind = .box, .child = self.parent.repQuery().requiredSingleChild(rep_id, .box_payload).rep } },
                .tag_union => .{ .tag = .{ .node = node, .rep_id = rep_id, .mode = .concrete_runtime } },
                .nominal => |kind| switch (kind) {
                    .transparent => .{ .single = .{ .node = node, .kind = .nominal, .child = self.parent.repQuery().requiredSingleChild(rep_id, .nominal_backing).rep } },
                    .opaque_nominal, .builtin_other => boxyLayoutInvariant("opaque or unsupported builtin nominal reached graph layout"),
                },
                .alias,
                .in_progress,
                .dynamic,
                .primitive,
                .bool_tag_union,
                .erased_callable,
                .generated_field,
                .generated_field_names,
                .generated_tag_union_spec,
                .empty_record,
                .empty_tag_union,
                => boxyLayoutInvariant("non-aggregate representation reached graph layout"),
            });
            return null;
        }
    }

    fn reserveLocalNode(self: *GraphBuilder, rep_id: Plan.TypeRepId) Allocator.Error!layout.GraphNodeId {
        const node = try self.graph.reserveNode(self.parent.allocator);
        try self.local_nodes.put(rep_id, node);
        return node;
    }

    fn pushGraphFrame(self: *GraphBuilder, frames: *std.ArrayList(GraphFrame), state: @FieldType(GraphFrame, "state")) Allocator.Error!void {
        var frame: GraphFrame = .{ .state = state };
        frames.append(self.parent.allocator, frame) catch |err| {
            frame.deinit(self.parent.allocator);
            return err;
        };
    }

    fn stepGraphFrame(self: *GraphBuilder, frames: *std.ArrayList(GraphFrame), delivered: ?layout.GraphInput) Allocator.Error!GraphStep {
        const allocator = self.parent.allocator;
        const frame = &frames.items[frames.items.len - 1];
        const child_input: ?layout.GraphInput = if (frame.awaiting)
            delivered orelse boxyLayoutInvariant("boxy layout frame resumed without its child input")
        else
            null;
        frame.awaiting = false;
        const program = self.parent.program;
        switch (frame.state) {
            .single => |single| {
                const input = child_input orelse {
                    frame.awaiting = true;
                    return .{ .request = single.child };
                };
                self.graph.setNode(single.node, switch (single.kind) {
                    .list => .{ .list = input },
                    .box => .{ .box = input },
                    .nominal => .{ .nominal = input },
                });
                return self.finishGraphFrame(frames, .{ .local = single.node });
            },
            .fields => |*fields| {
                const rep = program.representations.items[@intFromEnum(fields.rep_id)];
                const children = program.childSlice(rep.children);
                if (child_input) |input| {
                    const child = children[fields.next - 1];
                    try fields.fields.append(allocator, .{
                        .index = switch (fields.kind) {
                            .record => @intCast(fields.fields.items.len),
                            .tuple => @intCast(child.role.tuple_elem),
                        },
                        .child = input,
                    });
                } else if (fields.next == 0 and fields.kind == .record) {
                    try self.requireClosedRecord(children);
                }
                while (fields.next < children.len) {
                    const child = children[fields.next];
                    fields.next += 1;
                    const wanted = switch (fields.kind) {
                        .record => child.role == .record_field,
                        .tuple => child.role == .tuple_elem,
                    };
                    if (!wanted) continue;
                    frame.awaiting = true;
                    return .{ .request = child.rep };
                }
                const span = try self.graph.appendFields(allocator, fields.fields.items);
                self.graph.setNode(fields.node, .{ .struct_ = span });
                return self.finishGraphFrame(frames, .{ .local = fields.node });
            },
            .declared => |*declared| {
                const rep = program.representations.items[@intFromEnum(declared.rep_id)];
                const declared_fields = program.declaredFieldSlice(rep.declared_fields);
                if (child_input) |input| {
                    const field = declared_fields[declared.next - 1];
                    declared.fields[declared.next - 1] = .{
                        .index = field.index,
                        .child = input,
                        .is_padding = field.is_padding,
                    };
                }
                if (declared.next < declared_fields.len) {
                    declared.next += 1;
                    frame.awaiting = true;
                    return .{ .request = declared_fields[declared.next - 1].rep };
                }
                const fields = if (declared_fields.len == 0)
                    layout.GraphFieldSpan.empty()
                else
                    try self.graph.appendFields(allocator, declared.fields);
                self.graph.setNode(declared.node, .{ .struct_ = if (rep.record_field_order == .declared)
                    self.graph.declaredOrder(fields)
                else
                    fields });
                return self.finishGraphFrame(frames, .{ .local = declared.node });
            },
            .tag => |*tag| {
                if (child_input) |input| try tag.refs.append(allocator, input);
                const rep = program.representations.items[@intFromEnum(tag.rep_id)];
                const variants = program.tagVariantSlice(rep.tag_variants);
                while (tag.variant < variants.len) {
                    const variant = variants[tag.variant];
                    tag.variant += 1;
                    const payload_children = program.childSlice(variant.payloads);
                    for (payload_children, 0..) |child, index| {
                        if (child.role != .tag_payload) {
                            boxyLayoutInvariant("tag variant payload span included a non-payload child");
                        }
                        const payload = child.role.tag_payload;
                        if (payload.tag != variant.name or payload.index != index) {
                            boxyLayoutInvariant("tag variant payload span did not match its payload child roles");
                        }
                    }
                    switch (payload_children.len) {
                        0 => try tag.refs.append(allocator, .{ .canonical = .zst }),
                        1 => {
                            frame.awaiting = true;
                            return .{ .request = payload_children[0].rep };
                        },
                        else => {
                            const payloads = try allocator.alloc(Plan.TypeRepId, payload_children.len);
                            errdefer allocator.free(payloads);
                            for (payload_children, payloads) |child, *payload| payload.* = child.rep;
                            const fields = try allocator.alloc(layout.GraphField, payload_children.len);
                            errdefer allocator.free(fields);
                            frame.awaiting = true;
                            try self.pushGraphFrame(frames, .{ .payload = .{ .payloads = payloads, .fields = fields } });
                            return .pushed;
                        },
                    }
                }
                if (self.tagExtensionPayload(program.childSlice(rep.children)) != null) {
                    switch (tag.mode) {
                        .concrete_runtime => boxyLayoutInvariant("open tag-union layout reached concrete boxy layout planning"),
                        .descriptor_payload => try tag.refs.append(allocator, .{ .canonical = try self.parent.dynamicStorageLayout() }),
                    }
                }
                const span = try self.graph.appendRefs(allocator, tag.refs.items);
                self.graph.setNode(tag.node, .{ .tag_union = span });
                return self.finishGraphFrame(frames, .{ .local = tag.node });
            },
            .payload => |*payload| {
                if (child_input) |input| {
                    payload.fields[payload.next - 1] = .{ .index = @intCast(payload.next - 1), .child = input };
                }
                if (payload.next < payload.payloads.len) {
                    payload.next += 1;
                    frame.awaiting = true;
                    return .{ .request = payload.payloads[payload.next - 1] };
                }
                const node = try self.graph.reserveNode(allocator);
                self.graph.setNode(node, .{ .struct_ = try self.graph.appendFields(allocator, payload.fields) });
                return self.finishGraphFrame(frames, .{ .local = node });
            },
        }
    }

    fn finishGraphFrame(self: *GraphBuilder, frames: *std.ArrayList(GraphFrame), input: layout.GraphInput) GraphStep {
        var frame = frames.pop().?;
        frame.deinit(self.parent.allocator);
        return .{ .done = input };
    }

    fn requireClosedRecord(self: *GraphBuilder, children: []const Plan.RepChild) Allocator.Error!void {
        for (children) |child| {
            if (child.role != .record_ext) continue;
            const ext_rep = self.parent.program.representations.items[@intFromEnum(child.rep)];
            if (ext_rep.kind != .empty_record) {
                boxyLayoutInvariant("open record layout reached boxy layout planning without an explicit closed row");
            }
        }
    }

    fn tagExtensionPayload(self: *GraphBuilder, children: []const Plan.RepChild) ?Plan.TypeRepId {
        for (children) |child| {
            if (child.role != .tag_ext) continue;
            const ext_rep = self.parent.program.representations.items[@intFromEnum(child.rep)];
            if (ext_rep.kind == .empty_tag_union) return null;
            return child.rep;
        }
        return null;
    }
};

fn repHasRecordFields(program: *const Plan.ProgramPlan, rep: Plan.TypeRepresentation) bool {
    for (program.childSlice(rep.children)) |child| {
        if (child.role == .record_field) return true;
    }
    return false;
}

fn boxyLayoutInvariant(comptime message: []const u8) noreturn {
    if (@import("builtin").mode == .Debug) {
        base.invariant("boxy layout invariant violated: {s}", .{message});
    }
    unreachable;
}

test "boxy layout planner records dynamic worker boxes separately from storage layout" {
    const gpa = std.testing.allocator;

    const payloads = [_]checked.StoredCheckedTypePayload{
        .{ .flex = .{} },
    };
    const view = checked.CheckedTypeStoreView{ .stored_payloads = &payloads };

    var program = try Plan.analyzeCheckedTypes(gpa, view, &.{@as(checked.CheckedTypeId, @enumFromInt(fixtureTableIndex(0)))}, .{});
    defer program.deinit();

    var store = try layout.Store.init(gpa, .u64);
    defer store.deinit();

    var layouts = try build(gpa, &program, &store, .{});
    defer layouts.deinit();

    const rep_layout = layouts.rep_layouts[@intFromEnum(program.root_reps.items[0])].worker;
    try std.testing.expectEqual(std.meta.Tag(RuntimeLayout).dynamic_box, std.meta.activeTag(rep_layout));
    try std.testing.expectEqual(layout.LayoutTag.erased_box, store.getLayout(rep_layout.layoutIdx()).tag);
    try std.testing.expect(rep_layout.descriptor() != null);
}

test "boxy layout planner reuses dynamic storage for Box(a) worker layout" {
    const gpa = std.testing.allocator;

    const type_pool = [_]checked.CheckedTypeId{@enumFromInt(fixtureTableIndex(0))};
    const payloads = [_]checked.StoredCheckedTypePayload{
        .{ .flex = .{} },
        .{ .nominal = builtinNominal(.box, @enumFromInt(1), .{ .start = 0, .len = 1 }) },
    };
    const view = checked.CheckedTypeStoreView{
        .stored_payloads = &payloads,
        .type_id_pool = &type_pool,
    };

    var program = try Plan.analyzeCheckedTypes(gpa, view, &.{@as(checked.CheckedTypeId, @enumFromInt(1))}, .{});
    defer program.deinit();

    var store = try layout.Store.init(gpa, .u64);
    defer store.deinit();

    var layouts = try build(gpa, &program, &store, .{});
    defer layouts.deinit();

    const box_layouts = layouts.rep_layouts[@intFromEnum(program.root_reps.items[0])];
    const host_layout = layouts.rep_layouts[@intFromEnum(program.hostRepFor(program.root_reps.items[0]))].worker;
    try std.testing.expectEqual(layout.LayoutTag.box_of_zst, store.getLayout(host_layout.layoutIdx()).tag);
    try std.testing.expectEqual(std.meta.Tag(RuntimeLayout).dynamic_box, std.meta.activeTag(box_layouts.worker));
    try std.testing.expectEqual(layout.LayoutTag.erased_box, store.getLayout(box_layouts.worker.layoutIdx()).tag);
}

test "boxy layout planner substitutes dynamic boxes into list elements" {
    const gpa = std.testing.allocator;

    const type_pool = [_]checked.CheckedTypeId{@enumFromInt(fixtureTableIndex(0))};
    const payloads = [_]checked.StoredCheckedTypePayload{
        .{ .flex = .{} },
        .{ .nominal = builtinNominal(.list, @enumFromInt(1), .{ .start = 0, .len = 1 }) },
    };
    const view = checked.CheckedTypeStoreView{
        .stored_payloads = &payloads,
        .type_id_pool = &type_pool,
    };

    var program = try Plan.analyzeCheckedTypes(gpa, view, &.{@as(checked.CheckedTypeId, @enumFromInt(1))}, .{});
    defer program.deinit();

    var store = try layout.Store.init(gpa, .u64);
    defer store.deinit();

    var layouts = try build(gpa, &program, &store, .{});
    defer layouts.deinit();

    const list_runtime = layouts.rep_layouts[@intFromEnum(program.root_reps.items[0])].worker;
    const list_layout = store.getLayout(list_runtime.layoutIdx());
    try std.testing.expectEqual(layout.LayoutTag.list, list_layout.tag);
    try std.testing.expectEqual(layouts.dynamic_storage_layout, list_layout.getIdx());
}

test "boxy layout planner preserves zero-payload tag variants" {
    const gpa = std.testing.allocator;

    const tag_a: TagLabelId = @enumFromInt(1);
    const tag_b: TagLabelId = @enumFromInt(2);
    const type_pool = [_]checked.CheckedTypeId{@enumFromInt(fixtureTableIndex(0))};
    const tags = [_]checked.CheckedTag{
        .{ .name = tag_a, .args_start = 0, .args_len = 0 },
        .{ .name = tag_b, .args_start = 0, .args_len = 1 },
    };
    const payloads = [_]checked.StoredCheckedTypePayload{
        .{ .nominal = builtinNominal(.u64, @enumFromInt(fixtureTableIndex(0)), .{}) },
        .empty_tag_union,
        .{ .tag_union = .{ .tags = .{ .start = 0, .len = tags.len }, .ext = @enumFromInt(1) } },
    };
    const view = checked.CheckedTypeStoreView{
        .stored_payloads = &payloads,
        .type_id_pool = &type_pool,
        .tag_pool = &tags,
    };

    var program = try Plan.analyzeCheckedTypes(gpa, view, &.{@as(checked.CheckedTypeId, @enumFromInt(2))}, .{});
    defer program.deinit();

    var store = try layout.Store.init(gpa, .u64);
    defer store.deinit();

    var layouts = try build(gpa, &program, &store, .{});
    defer layouts.deinit();

    const runtime = layouts.rep_layouts[@intFromEnum(program.root_reps.items[0])].worker;
    const tag_layout = store.getLayout(runtime.layoutIdx());
    try std.testing.expectEqual(layout.LayoutTag.tag_union, tag_layout.tag);

    const info = store.getTagUnionInfo(tag_layout);
    try std.testing.expectEqual(@as(usize, 2), info.variants.len);
    try std.testing.expectEqual(layout.Idx.zst, info.variants.get(0).payload_layout);
    try std.testing.expectEqual(layout.Idx.u64, info.variants.get(1).payload_layout);
}

test "boxy layout planner gives open tag descriptors a row-extension payload layout" {
    const gpa = std.testing.allocator;

    const tag_exit: TagLabelId = @enumFromInt(1);
    const type_pool = [_]checked.CheckedTypeId{@enumFromInt(fixtureTableIndex(0))};
    const tags = [_]checked.CheckedTag{
        .{ .name = tag_exit, .args_start = 0, .args_len = 1 },
    };
    const payloads = [_]checked.StoredCheckedTypePayload{
        .{ .nominal = builtinNominal(.i64, @enumFromInt(fixtureTableIndex(0)), .{}) },
        .{ .flex = .{} },
        .{ .tag_union = .{ .tags = .{ .start = 0, .len = tags.len }, .ext = @enumFromInt(1) } },
    };
    const view = checked.CheckedTypeStoreView{
        .stored_payloads = &payloads,
        .type_id_pool = &type_pool,
        .tag_pool = &tags,
    };

    var program = try Plan.analyzeCheckedTypes(gpa, view, &.{@as(checked.CheckedTypeId, @enumFromInt(2))}, .{});
    defer program.deinit();

    var store = try layout.Store.init(gpa, .u64);
    defer store.deinit();

    var layouts = try build(gpa, &program, &store, .{});
    defer layouts.deinit();

    const rep_layouts = layouts.rep_layouts[@intFromEnum(program.root_reps.items[0])];
    try std.testing.expectEqual(std.meta.Tag(RuntimeLayout).dynamic_box, std.meta.activeTag(rep_layouts.worker));
    try std.testing.expectEqual(layout.LayoutTag.erased_box, store.getLayout(rep_layouts.worker.layoutIdx()).tag);

    const payload_layout = rep_layouts.descriptor_payload_layout orelse return error.TestExpectedEqual;
    const tag_layout = store.getLayout(payload_layout);
    try std.testing.expectEqual(layout.LayoutTag.tag_union, tag_layout.tag);

    const info = store.getTagUnionInfo(tag_layout);
    try std.testing.expectEqual(@as(usize, 2), info.variants.len);
    try std.testing.expectEqual(layout.Idx.i64, info.variants.get(0).payload_layout);
    try std.testing.expectEqual(layout.LayoutTag.erased_box, store.getLayout(info.variants.get(1).payload_layout).tag);
}

test "boxy layout planner records private worker function arg and return layouts" {
    const gpa = std.testing.allocator;

    const type_pool = [_]checked.CheckedTypeId{
        @enumFromInt(fixtureTableIndex(0)), // List(a) argument.
        @enumFromInt(fixtureTableIndex(0)), // Function argument a.
    };
    const payloads = [_]checked.StoredCheckedTypePayload{
        .{ .flex = .{} },
        .{ .nominal = builtinNominal(.list, @enumFromInt(1), .{ .start = 0, .len = 1 }) },
        .{ .function = .{
            .kind = .pure,
            .args = .{ .start = 1, .len = 1 },
            .ret = @enumFromInt(1),
        } },
    };
    const view = checked.CheckedTypeStoreView{
        .stored_payloads = &payloads,
        .type_id_pool = &type_pool,
    };
    const roots = [_]checked.RootRequest{
        .{
            .order = 0,
            .module_idx = 0,
            .kind = .runtime_entrypoint,
            .source = .{ .def = @enumFromInt(fixtureTableIndex(0)) },
            .checked_type = @enumFromInt(2),
            .abi = .roc,
            .exposure = .private,
            .procedure_binding = @enumFromInt(fixtureTableIndex(0)),
        },
    };
    const template_ref = checked_names.ProcedureTemplateRef{
        .proc_base = @enumFromInt(fixtureTableIndex(0)),
        .template = @enumFromInt(fixtureTableIndex(0)),
    };
    var templates = [_]checked.CheckedProcedureTemplate{.{
        .proc_base = template_ref.proc_base,
        .template_id = template_ref.template,
        .body = .{ .checked_body = @enumFromInt(fixtureTableIndex(0)) },
        .checked_fn_scheme = .{},
        .checked_fn_root = @enumFromInt(2),
        .static_dispatch_plans = .{},
        .direct_dispatch_plans = .{},
        .dispatch_relations = .{},
        .resolved_value_refs = .{},
        .top_level_value_uses = .{},
        .nested_proc_sites = .{},
        .target = .roc,
    }};
    var template_table = checked.CheckedProcedureTemplateTable{ .templates = .{ .items = &templates, .capacity = templates.len } };
    var bindings = [_]checked.TopLevelProcedureBinding{.{
        .source_scheme = .{},
        .body = .{ .direct_template = .{
            .proc_value = .{
                .proc_base = template_ref.proc_base,
            },
            .template = .{ .checked = template_ref },
        } },
    }};
    var binding_table = checked.TopLevelProcedureBindingTable{ .bindings = .{ .items = &bindings, .capacity = bindings.len } };
    const root_view = Plan.ModuleView{
        .checked_types = view,
        .checked_procedure_templates = &template_table,
        .top_level_procedure_bindings = &binding_table,
    };

    var program = try Plan.analyzeProgram(gpa, .{ .root_view = root_view, .roots = &roots }, .{});
    defer program.deinit();
    const extra_source = program.workers.items[0].source;
    const extra_rep = program.root_reps.items[0];
    try program.workers.append(gpa, .{
        .id = @enumFromInt(1),
        .root_request = roots[0],
        .source = extra_source,
        .checked_type = .{ .ty = @enumFromInt(2) },
        .rep = extra_rep,
    });

    var store = try layout.Store.init(gpa, .u64);
    defer store.deinit();

    var layouts = try build(gpa, &program, &store, .{});
    defer layouts.deinit();

    try std.testing.expectEqual(@as(usize, 1), layouts.roots.items.len);
    try std.testing.expectEqual(@as(usize, 2), layouts.worker_layouts.len);
    const root = layouts.roots.items[0];
    try std.testing.expectEqual(program.workers.items[0].id, root.worker);

    const root_worker = layouts.workerLayoutFor(root.worker);
    const worker_args = layouts.workerLayoutSlice(root_worker.args);
    try std.testing.expectEqual(@as(usize, 1), worker_args.len);
    try std.testing.expectEqual(std.meta.Tag(RuntimeLayout).dynamic_box, std.meta.activeTag(worker_args[0]));

    const ret = root_worker.ret orelse return error.TestUnexpectedResult;
    const ret_layout = store.getLayout(ret.layoutIdx());
    try std.testing.expectEqual(layout.LayoutTag.list, ret_layout.tag);
    try std.testing.expectEqual(layouts.dynamic_storage_layout, ret_layout.getIdx());
    try std.testing.expectEqual(@as(usize, 0), layouts.rootLayoutSlice(root.host_args).len);
    try std.testing.expect(root.host_ret == null);

    const extra_worker = layouts.workerLayoutFor(@enumFromInt(1));
    try std.testing.expectEqual(@as(usize, 1), layouts.workerLayoutSlice(extra_worker.args).len);
    try std.testing.expect(extra_worker.ret != null);
}

test "boxy layout planner commits nominal declared fields through shared layout store" {
    const gpa = std.testing.allocator;

    const field_a: RecordFieldLabelId = @enumFromInt(1);
    const field_b: RecordFieldLabelId = @enumFromInt(2);
    const type_pool = [_]checked.CheckedTypeId{@enumFromInt(fixtureTableIndex(0))};
    const record_fields = [_]checked.CheckedRecordField{
        .{ .name = field_a, .ty = @enumFromInt(fixtureTableIndex(0)) },
        .{ .name = field_b, .ty = @enumFromInt(1) },
    };
    const declared_fields = [_]checked.CheckedDeclaredField{
        .{ .named = field_a },
        .{ .padding = 0 },
        .{ .named = field_b },
    };
    const nominal_declarations = [_]checked.CheckedNominalDeclaration{.{
        .id = @enumFromInt(fixtureTableIndex(0)),
        .nominal = .{ .module = @enumFromInt(4), .type_name = @enumFromInt(3), .source_decl = null },
        .source_statement = 0,
        .declaration_root = @enumFromInt(4),
        .backing = @enumFromInt(3),
        .pf_start = 0,
        .pf_len = 1,
        .df_start = 0,
        .df_len = declared_fields.len,
    }};
    const payloads = [_]checked.StoredCheckedTypePayload{
        .{ .nominal = builtinNominal(.u8, @enumFromInt(fixtureTableIndex(0)), .{}) },
        .{ .nominal = builtinNominal(.u16, @enumFromInt(1), .{}) },
        .{ .empty_record = {} },
        .{ .record = .{ .fields = .{ .start = 0, .len = 2 }, .ext = @enumFromInt(2) } },
        .{ .nominal = .{
            .name = @enumFromInt(3),
            .origin_module = @enumFromInt(4),
            .owner_module = .{},
            .is_opaque = false,
            .representation = .{ .local_declaration = @enumFromInt(fixtureTableIndex(0)) },
            .padding_field_types = .{ .start = 0, .len = 1 },
            .declared_fields = .{ .start = 0, .len = 3 },
        } },
    };
    const view = checked.CheckedTypeStoreView{
        .stored_payloads = &payloads,
        .nominal_declarations = &nominal_declarations,
        .type_id_pool = &type_pool,
        .record_field_pool = &record_fields,
        .declared_field_pool = &declared_fields,
    };

    var program = try Plan.analyzeCheckedTypes(gpa, view, &.{@as(checked.CheckedTypeId, @enumFromInt(4))}, .{});
    defer program.deinit();
    const nominal = program.representations.items[@intFromEnum(program.root_reps.items[0])];
    try std.testing.expectEqual(Plan.RecordFieldOrder.declared, nominal.record_field_order);

    var store = try layout.Store.init(gpa, .u64);
    defer store.deinit();

    var layouts = try build(gpa, &program, &store, .{});
    defer layouts.deinit();

    const runtime = layouts.rep_layouts[@intFromEnum(program.root_reps.items[0])].worker;
    const struct_idx = store.getLayout(runtime.layoutIdx()).getStruct().idx;
    try std.testing.expectEqual(@as(u32, 0), store.getStructFieldOffsetByOriginalIndex(struct_idx, 0));
    try std.testing.expectEqual(@as(u32, 2), store.getStructFieldOffsetByOriginalIndex(struct_idx, 1));
    try std.testing.expectEqual(@as(u32, 4), store.getStructSize(struct_idx));
}

test "boxy layout planner reuses structural backing order without padding" {
    const gpa = std.testing.allocator;

    const field_a: RecordFieldLabelId = @enumFromInt(1);
    const field_b: RecordFieldLabelId = @enumFromInt(2);
    const record_fields = [_]checked.CheckedRecordField{
        .{ .name = field_a, .ty = @enumFromInt(fixtureTableIndex(0)) },
        .{ .name = field_b, .ty = @enumFromInt(fixtureTableIndex(0)) },
    };
    const declared_fields = [_]checked.CheckedDeclaredField{
        .{ .named = field_b },
        .{ .named = field_a },
    };
    const nominal_declarations = [_]checked.CheckedNominalDeclaration{.{
        .id = @enumFromInt(fixtureTableIndex(0)),
        .nominal = .{ .module = @enumFromInt(4), .type_name = @enumFromInt(3), .source_decl = null },
        .source_statement = 0,
        .declaration_root = @enumFromInt(3),
        .backing = @enumFromInt(2),
        .df_start = 0,
        .df_len = declared_fields.len,
    }};
    const payloads = [_]checked.StoredCheckedTypePayload{
        .{ .nominal = builtinNominal(.f32, @enumFromInt(fixtureTableIndex(0)), .{}) },
        .{ .empty_record = {} },
        .{ .record = .{ .fields = .{ .start = 0, .len = 2 }, .ext = @enumFromInt(1) } },
        .{ .nominal = .{
            .name = @enumFromInt(3),
            .origin_module = @enumFromInt(4),
            .owner_module = .{},
            .is_opaque = false,
            .representation = .{ .local_declaration = @enumFromInt(fixtureTableIndex(0)) },
            .declared_fields = .{ .start = 0, .len = 2 },
        } },
    };
    const view = checked.CheckedTypeStoreView{
        .stored_payloads = &payloads,
        .nominal_declarations = &nominal_declarations,
        .record_field_pool = &record_fields,
        .declared_field_pool = &declared_fields,
    };

    var program = try Plan.analyzeCheckedTypes(gpa, view, &.{@as(checked.CheckedTypeId, @enumFromInt(3))}, .{});
    defer program.deinit();
    const nominal = program.representations.items[@intFromEnum(program.root_reps.items[0])];
    try std.testing.expectEqual(@as(usize, 2), program.declaredFieldSlice(nominal.declared_fields).len);
    try std.testing.expectEqual(Plan.RecordFieldOrder.structural, nominal.record_field_order);

    var store = try layout.Store.init(gpa, .u64);
    defer store.deinit();

    var layouts = try build(gpa, &program, &store, .{});
    defer layouts.deinit();

    const runtime = layouts.rep_layouts[@intFromEnum(program.root_reps.items[0])].worker;
    const struct_idx = store.getLayout(runtime.layoutIdx()).getStruct().idx;
    try std.testing.expectEqual(@as(u32, 0), store.getStructFieldOffsetByOriginalIndex(struct_idx, 0));
    try std.testing.expectEqual(@as(u32, 4), store.getStructFieldOffsetByOriginalIndex(struct_idx, 1));
    try std.testing.expectEqual(@as(u32, 8), store.getStructSize(struct_idx));
}
