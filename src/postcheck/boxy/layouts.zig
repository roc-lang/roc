//! Boxy runtime layout planning.
//!
//! This stage consumes the target-independent boxy representation plan and
//! commits storage layouts into the LIR layout store. Dynamic boxy values keep
//! their descriptor-governed meaning in this table instead of encoding it in
//! the ordinary layout id; the layout id only describes storage width,
//! alignment, and aggregate placement.

const std = @import("std");
const check = @import("check");
const layout = @import("layout");

const Plan = @import("plan.zig");

const Allocator = std.mem.Allocator;
const checked = check.CheckedModule;
const RecordFieldLabelId = @TypeOf(@as(checked.CheckedRecordField, undefined).name);
const TagLabelId = @TypeOf(@as(checked.CheckedTag, undefined).name);

pub const DynamicBoxLayout = struct {
    storage_layout: layout.Idx,
    desc: Plan.DescriptorRequirementId,
};

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

pub const RepLayouts = struct {
    worker: RuntimeLayout,
    host: RuntimeLayout,
    descriptor_payload_layout: ?layout.Idx = null,
};

pub const RootLayouts = struct {
    root: Plan.RootPlanId,
    host_args: Plan.Span = .{},
    host_ret: ?RuntimeLayout = null,
    host_value: ?RuntimeLayout = null,
    worker_args: Plan.Span = .{},
    worker_ret: ?RuntimeLayout = null,
    worker_value: RuntimeLayout,
};

pub const LayoutPlan = struct {
    allocator: Allocator,
    rep_layouts: []RepLayouts,
    roots: std.ArrayList(RootLayouts),
    root_layout_values: std.ArrayList(RuntimeLayout),
    dynamic_storage_layout: layout.Idx,

    pub fn deinit(self: *LayoutPlan) void {
        self.root_layout_values.deinit(self.allocator);
        self.roots.deinit(self.allocator);
        self.allocator.free(self.rep_layouts);
        self.* = undefined;
    }

    pub fn rootLayoutSlice(self: *const LayoutPlan, span: Plan.Span) []const RuntimeLayout {
        return self.root_layout_values.items[span.start .. span.start + span.len];
    }
};

pub const BuildOptions = struct {};

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

const LayoutMode = enum {
    host,
    worker,
};

const RepLayoutCache = struct {
    worker: ?RuntimeLayout = null,
    host: ?RuntimeLayout = null,

    fn get(self: RepLayoutCache, mode: LayoutMode) ?RuntimeLayout {
        return switch (mode) {
            .host => self.host,
            .worker => self.worker,
        };
    }

    fn set(self: *RepLayoutCache, mode: LayoutMode, value: RuntimeLayout) void {
        switch (mode) {
            .host => self.host = value,
            .worker => self.worker = value,
        }
    }
};

const Builder = struct {
    allocator: Allocator,
    program: *const Plan.ProgramPlan,
    store: *layout.Store,
    caches: []RepLayoutCache,
    root_layouts: std.ArrayList(RootLayouts),
    root_layout_values: std.ArrayList(RuntimeLayout),
    dynamic_storage_layout: ?layout.Idx = null,

    fn init(allocator: Allocator, program: *const Plan.ProgramPlan, store: *layout.Store) Builder {
        return .{
            .allocator = allocator,
            .program = program,
            .store = store,
            .caches = &.{},
            .root_layouts = .empty,
            .root_layout_values = .empty,
        };
    }

    fn deinit(self: *Builder) void {
        self.root_layout_values.deinit(self.allocator);
        self.root_layouts.deinit(self.allocator);
        self.allocator.free(self.caches);
    }

    fn finish(self: *Builder) Allocator.Error!LayoutPlan {
        self.caches = try self.allocator.alloc(RepLayoutCache, self.program.representations.items.len);
        @memset(self.caches, .{});

        for (self.program.representations.items, 0..) |_, index| {
            _ = try self.runtimeLayoutForRep(.worker, @enumFromInt(index));
            _ = try self.runtimeLayoutForRep(.host, @enumFromInt(index));
        }
        for (self.program.roots.items) |root| {
            try self.appendRoot(root);
        }

        const rep_layouts = try self.allocator.alloc(RepLayouts, self.program.representations.items.len);
        errdefer self.allocator.free(rep_layouts);
        for (rep_layouts, 0..) |*out, index| {
            const rep_id: Plan.TypeRepId = @enumFromInt(index);
            const worker = self.caches[index].worker orelse boxyLayoutInvariant("worker layout cache was not populated");
            const host = self.caches[index].host orelse boxyLayoutInvariant("host layout cache was not populated");
            out.* = .{
                .worker = worker,
                .host = host,
                .descriptor_payload_layout = try self.descriptorPayloadLayout(rep_id),
            };
        }

        const dynamic_storage_layout = try self.dynamicStorageLayout();
        const roots = self.root_layouts;
        const root_layout_values = self.root_layout_values;
        self.root_layouts = .empty;
        self.root_layout_values = .empty;

        return .{
            .allocator = self.allocator,
            .rep_layouts = rep_layouts,
            .roots = roots,
            .root_layout_values = root_layout_values,
            .dynamic_storage_layout = dynamic_storage_layout,
        };
    }

    fn appendRoot(self: *Builder, root: Plan.RootPlan) Allocator.Error!void {
        const worker_value = try self.runtimeLayoutForRep(.worker, root.worker_rep);
        var root_layout: RootLayouts = .{
            .root = root.id,
            .worker_value = worker_value,
        };

        if (try self.functionChildren(root.host_rep)) |function| {
            const worker_start = self.rootLayoutStart();
            try self.appendFunctionLayouts(.worker, function);
            root_layout.worker_args = self.rootLayoutSpanFrom(worker_start, function.arg_count);
            root_layout.worker_ret = self.root_layout_values.items[worker_start + function.arg_count];

            if (root.wrapper_kind == .host_shaped_wrapper) {
                const host_start = self.rootLayoutStart();
                try self.appendFunctionLayouts(.host, function);
                root_layout.host_args = self.rootLayoutSpanFrom(host_start, function.arg_count);
                root_layout.host_ret = self.root_layout_values.items[host_start + function.arg_count];
            }
        } else if (root.wrapper_kind == .host_shaped_wrapper) {
            root_layout.host_value = try self.runtimeLayoutForRep(.host, root.host_rep);
        }

        try self.root_layouts.append(self.allocator, root_layout);
    }

    const FunctionChildren = struct {
        rep: Plan.TypeRepId,
        args_start: u32,
        arg_count: u32,
        ret: Plan.TypeRepId,
    };

    fn functionChildren(self: *Builder, rep_id: Plan.TypeRepId) Allocator.Error!?FunctionChildren {
        const canonical = try self.canonicalFunctionRep(rep_id);
        const rep = self.program.representations.items[@intFromEnum(canonical)];
        return switch (rep.kind) {
            .erased_callable => blk: {
                const children = self.program.childSlice(rep.children);
                var args_start: ?u32 = null;
                var arg_count: u32 = 0;
                var ret: ?Plan.TypeRepId = null;
                for (children, 0..) |child, i| {
                    switch (child.role) {
                        .function_arg => {
                            if (args_start == null) args_start = @intCast(i);
                            arg_count += 1;
                        },
                        .function_ret => ret = child.rep,
                        else => {},
                    }
                }
                break :blk .{
                    .rep = canonical,
                    .args_start = args_start orelse 0,
                    .arg_count = arg_count,
                    .ret = ret orelse boxyLayoutInvariant("function representation had no return child"),
                };
            },
            else => null,
        };
    }

    fn canonicalFunctionRep(self: *Builder, rep_id: Plan.TypeRepId) Allocator.Error!Plan.TypeRepId {
        var current = rep_id;
        var depth: u16 = 0;
        while (true) {
            if (depth == 1024) boxyLayoutInvariant("function root alias chain exceeded boxy layout limit");
            depth += 1;

            const rep = self.program.representations.items[@intFromEnum(current)];
            switch (rep.kind) {
                .alias => current = self.requiredSingleChild(current, .alias_backing).rep,
                .nominal => |kind| switch (kind) {
                    .transparent => current = self.requiredSingleChild(current, .nominal_backing).rep,
                    .opaque_nominal, .builtin_other => return current,
                },
                else => return current,
            }
        }
    }

    fn appendFunctionLayouts(self: *Builder, mode: LayoutMode, function: FunctionChildren) Allocator.Error!void {
        const canonical_children = self.program.childSlice(self.program.representations.items[@intFromEnum(function.rep)].children);
        for (canonical_children[function.args_start..][0..function.arg_count]) |child| {
            try self.root_layout_values.append(self.allocator, try self.runtimeLayoutForRep(mode, child.rep));
        }
        try self.root_layout_values.append(self.allocator, try self.runtimeLayoutForRep(mode, function.ret));
    }

    fn rootLayoutStart(self: *const Builder) u32 {
        return @intCast(self.root_layout_values.items.len);
    }

    fn rootLayoutSpanFrom(_: *const Builder, start: u32, len: u32) Plan.Span {
        return .{ .start = start, .len = len };
    }

    fn runtimeLayoutForRep(self: *Builder, mode: LayoutMode, rep_id: Plan.TypeRepId) Allocator.Error!RuntimeLayout {
        const index = @intFromEnum(rep_id);
        if (self.caches[index].get(mode)) |cached| return cached;
        if (try self.immediateRuntimeLayout(mode, rep_id)) |runtime| {
            self.caches[index].set(mode, runtime);
            return runtime;
        }

        var graph = layout.Graph{};
        defer graph.deinit(self.allocator);
        const local_nodes = try self.allocator.alloc(?layout.GraphNodeId, self.program.representations.items.len);
        defer self.allocator.free(local_nodes);
        @memset(local_nodes, null);

        var graph_builder = GraphBuilder{
            .parent = self,
            .mode = mode,
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
        self.caches[index].set(mode, runtime);

        for (local_nodes, 0..) |maybe_node, rep_index| {
            if (maybe_node) |node| {
                self.caches[rep_index].set(mode, .{ .concrete = commit.value_layouts[@intFromEnum(node)] });
            }
        }

        return self.caches[index].get(mode).?;
    }

    fn immediateRuntimeLayout(self: *Builder, mode: LayoutMode, rep_id: Plan.TypeRepId) Allocator.Error!?RuntimeLayout {
        const rep = self.program.representations.items[@intFromEnum(rep_id)];
        return switch (rep.kind) {
            .in_progress => boxyLayoutInvariant("in-progress representation reached boxy layout planning"),
            .dynamic => switch (mode) {
                .worker => .{ .dynamic_box = .{
                    .storage_layout = try self.dynamicStorageLayout(),
                    .desc = rep.descriptor orelse boxyLayoutInvariant("dynamic worker layout had no descriptor requirement"),
                } },
                .host => .{ .concrete = try self.dynamicStorageLayout() },
            },
            .primitive => |primitive| .{ .concrete = primitiveLayout(primitive) },
            .bool_tag_union => .{ .concrete = .bool },
            .empty_record, .empty_tag_union => .{ .concrete = .zst },
            .erased_callable => .{ .concrete = try self.store.insertErasedCallable() },
            .box => try self.immediateBoxLayout(mode, rep_id),
            .nominal => |kind| switch (kind) {
                .opaque_nominal => .{ .concrete = try self.dynamicStorageLayout() },
                .transparent, .builtin_other => null,
            },
            .alias,
            .record,
            .record_unbound,
            .tuple,
            .list,
            .tag_union,
            => null,
        };
    }

    fn immediateBoxLayout(self: *Builder, mode: LayoutMode, rep_id: Plan.TypeRepId) Allocator.Error!?RuntimeLayout {
        const child = self.requiredSingleChild(rep_id, .box_payload);
        const child_rep = self.program.representations.items[@intFromEnum(child.rep)];
        switch (child_rep.kind) {
            .dynamic => return switch (mode) {
                .worker => try self.runtimeLayoutForRep(.worker, child.rep),
                .host => .{ .concrete = try self.dynamicStorageLayout() },
            },
            .erased_callable => return try self.runtimeLayoutForRep(mode, child.rep),
            else => return null,
        }
    }

    fn descriptorPayloadLayout(self: *Builder, rep_id: Plan.TypeRepId) Allocator.Error!?layout.Idx {
        const rep = self.program.representations.items[@intFromEnum(rep_id)];
        if (rep.descriptor == null) return null;
        return (try self.runtimeLayoutForRep(.worker, rep_id)).layoutIdx();
    }

    fn dynamicStorageLayout(self: *Builder) Allocator.Error!layout.Idx {
        if (self.dynamic_storage_layout) |existing| return existing;
        const idx = try self.store.insertLayout(layout.Layout.boxOfZst());
        self.dynamic_storage_layout = idx;
        return idx;
    }

    fn requiredSingleChild(self: *Builder, rep_id: Plan.TypeRepId, role: Plan.ChildRole) Plan.RepChild {
        var found: ?Plan.RepChild = null;
        const rep = self.program.representations.items[@intFromEnum(rep_id)];
        for (self.program.childSlice(rep.children)) |child| {
            if (sameChildRole(child.role, role)) {
                if (found != null) boxyLayoutInvariant("representation had duplicate required child role");
                found = child;
            }
        }
        return found orelse boxyLayoutInvariant("representation was missing required child role");
    }
};

const GraphBuilder = struct {
    parent: *Builder,
    mode: LayoutMode,
    graph: *layout.Graph,
    local_nodes: []?layout.GraphNodeId,

    fn inputForRep(self: *GraphBuilder, rep_id: Plan.TypeRepId) Allocator.Error!layout.GraphInput {
        const index = @intFromEnum(rep_id);
        if (self.parent.caches[index].get(self.mode)) |runtime| return .{ .canonical = runtime.layoutIdx() };
        if (try self.parent.immediateRuntimeLayout(self.mode, rep_id)) |runtime| {
            self.parent.caches[index].set(self.mode, runtime);
            return .{ .canonical = runtime.layoutIdx() };
        }
        if (self.local_nodes[index]) |node| return .{ .local = node };

        const rep = self.parent.program.representations.items[index];
        switch (rep.kind) {
            .alias => return try self.inputForRep(self.parent.requiredSingleChild(rep_id, .alias_backing).rep),
            .nominal => |kind| switch (kind) {
                .transparent => {
                    if (rep.declared_fields.len != 0) {
                        const node = try self.graph.reserveNode(self.parent.allocator);
                        self.local_nodes[index] = node;
                        self.graph.setNode(node, .{ .struct_ = try self.nominalDeclaredFields(rep) });
                        try self.graph.markNominalStruct(self.parent.allocator, node);
                        return .{ .local = node };
                    }
                    return try self.inputForRep(self.parent.requiredSingleChild(rep_id, .nominal_backing).rep);
                },
                .opaque_nominal, .builtin_other => {},
            },
            else => {},
        }

        const node = try self.graph.reserveNode(self.parent.allocator);
        self.local_nodes[index] = node;
        self.graph.setNode(node, try self.nodeForRep(rep_id));
        return .{ .local = node };
    }

    fn nodeForRep(self: *GraphBuilder, rep_id: Plan.TypeRepId) Allocator.Error!layout.GraphNode {
        const rep = self.parent.program.representations.items[@intFromEnum(rep_id)];
        return switch (rep.kind) {
            .record, .record_unbound => .{ .struct_ = try self.recordFields(rep) },
            .tuple => .{ .struct_ = try self.tupleFields(rep) },
            .list => .{ .list = try self.inputForRep(self.parent.requiredSingleChild(rep_id, .list_elem).rep) },
            .box => .{ .box = try self.inputForRep(self.parent.requiredSingleChild(rep_id, .box_payload).rep) },
            .tag_union => .{ .tag_union = try self.tagPayloads(rep) },
            .nominal => |kind| switch (kind) {
                .transparent => .{ .nominal = try self.inputForRep(self.parent.requiredSingleChild(rep_id, .nominal_backing).rep) },
                .opaque_nominal, .builtin_other => boxyLayoutInvariant("opaque or unsupported builtin nominal reached graph layout"),
            },
            .alias,
            .in_progress,
            .dynamic,
            .primitive,
            .bool_tag_union,
            .erased_callable,
            .empty_record,
            .empty_tag_union,
            => boxyLayoutInvariant("non-aggregate representation reached graph layout"),
        };
    }

    fn recordFields(self: *GraphBuilder, rep: Plan.TypeRepresentation) Allocator.Error!layout.GraphFieldSpan {
        const children = self.parent.program.childSlice(rep.children);
        try self.requireClosedRecord(children);

        var fields = std.ArrayList(layout.GraphField).empty;
        defer fields.deinit(self.parent.allocator);
        for (children) |child| {
            switch (child.role) {
                .record_field => try fields.append(self.parent.allocator, .{
                    .index = @intCast(fields.items.len),
                    .child = try self.inputForRep(child.rep),
                }),
                .record_ext => {},
                else => {},
            }
        }
        return try self.graph.appendFields(self.parent.allocator, fields.items);
    }

    fn nominalDeclaredFields(self: *GraphBuilder, rep: Plan.TypeRepresentation) Allocator.Error!layout.GraphFieldSpan {
        const declared_fields = self.parent.program.declaredFieldSlice(rep.declared_fields);
        if (declared_fields.len == 0) return layout.GraphFieldSpan.empty();

        const fields = try self.parent.allocator.alloc(layout.GraphField, declared_fields.len);
        defer self.parent.allocator.free(fields);
        for (declared_fields, fields) |field, *out| {
            out.* = .{
                .index = field.index,
                .child = try self.inputForRep(field.rep),
                .is_padding = field.is_padding,
            };
        }
        return try self.graph.appendFields(self.parent.allocator, fields);
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

    fn tupleFields(self: *GraphBuilder, rep: Plan.TypeRepresentation) Allocator.Error!layout.GraphFieldSpan {
        const children = self.parent.program.childSlice(rep.children);
        var fields = std.ArrayList(layout.GraphField).empty;
        defer fields.deinit(self.parent.allocator);
        for (children) |child| {
            switch (child.role) {
                .tuple_elem => |index| try fields.append(self.parent.allocator, .{
                    .index = @intCast(index),
                    .child = try self.inputForRep(child.rep),
                }),
                else => {},
            }
        }
        return try self.graph.appendFields(self.parent.allocator, fields.items);
    }

    fn tagPayloads(self: *GraphBuilder, rep: Plan.TypeRepresentation) Allocator.Error!layout.GraphRefSpan {
        const children = self.parent.program.childSlice(rep.children);
        try self.requireClosedTagUnion(children);

        var refs = std.ArrayList(layout.GraphInput).empty;
        defer refs.deinit(self.parent.allocator);
        var payloads = std.ArrayList(Plan.TypeRepId).empty;
        defer payloads.deinit(self.parent.allocator);
        var active_tag: ?TagLabelId = null;

        for (children) |child| {
            switch (child.role) {
                .tag_payload => |payload| {
                    if (active_tag == null) active_tag = payload.tag;
                    if (active_tag.? != payload.tag) {
                        try refs.append(self.parent.allocator, try self.payloadInput(payloads.items));
                        payloads.clearRetainingCapacity();
                        active_tag = payload.tag;
                    }
                    try payloads.append(self.parent.allocator, child.rep);
                },
                .tag_ext => {},
                else => {},
            }
        }
        if (active_tag != null) {
            try refs.append(self.parent.allocator, try self.payloadInput(payloads.items));
        }
        return try self.graph.appendRefs(self.parent.allocator, refs.items);
    }

    fn requireClosedTagUnion(self: *GraphBuilder, children: []const Plan.RepChild) Allocator.Error!void {
        for (children) |child| {
            if (child.role != .tag_ext) continue;
            const ext_rep = self.parent.program.representations.items[@intFromEnum(child.rep)];
            if (ext_rep.kind != .empty_tag_union) {
                boxyLayoutInvariant("open tag-union layout reached boxy layout planning without an explicit closed row");
            }
        }
    }

    fn payloadInput(self: *GraphBuilder, payloads: []const Plan.TypeRepId) Allocator.Error!layout.GraphInput {
        return switch (payloads.len) {
            0 => .{ .canonical = .zst },
            1 => try self.inputForRep(payloads[0]),
            else => blk: {
                const fields = try self.parent.allocator.alloc(layout.GraphField, payloads.len);
                defer self.parent.allocator.free(fields);
                for (payloads, fields, 0..) |payload, *field, index| {
                    field.* = .{ .index = @intCast(index), .child = try self.inputForRep(payload) };
                }
                const node = try self.graph.reserveNode(self.parent.allocator);
                self.graph.setNode(node, .{ .struct_ = try self.graph.appendFields(self.parent.allocator, fields) });
                break :blk .{ .local = node };
            },
        };
    }
};

fn primitiveLayout(primitive: checked.CheckedPrimitive) layout.Idx {
    return switch (primitive) {
        .bool => .bool,
        .str => .str,
        .u8 => .u8,
        .i8 => .i8,
        .u16 => .u16,
        .i16 => .i16,
        .u32 => .u32,
        .i32 => .i32,
        .u64 => .u64,
        .i64 => .i64,
        .u128 => .u128,
        .i128 => .i128,
        .f32 => .f32,
        .f64 => .f64,
        .dec => .dec,
    };
}

fn sameChildRole(a: Plan.ChildRole, b: Plan.ChildRole) bool {
    return switch (a) {
        .alias_backing => b == .alias_backing,
        .nominal_backing => b == .nominal_backing,
        .record_ext => b == .record_ext,
        .tag_ext => b == .tag_ext,
        .list_elem => b == .list_elem,
        .box_payload => b == .box_payload,
        else => false,
    };
}

fn boxyLayoutInvariant(comptime message: []const u8) noreturn {
    if (@import("builtin").mode == .Debug) {
        std.debug.panic("boxy layout invariant violated: {s}", .{message});
    }
    unreachable;
}

test "boxy layout planner records dynamic worker boxes separately from storage layout" {
    const gpa = std.testing.allocator;

    const payloads = [_]checked.StoredCheckedTypePayload{
        .{ .flex = .{} },
    };
    const view = checked.CheckedTypeStoreView{ .stored_payloads = &payloads };

    var program = try Plan.analyzeCheckedTypes(gpa, view, &.{@as(checked.CheckedTypeId, @enumFromInt(0))}, .{});
    defer program.deinit();

    var store = try layout.Store.init(gpa, .u64);
    defer store.deinit();

    var layouts = try build(gpa, &program, &store, .{});
    defer layouts.deinit();

    const rep_layout = layouts.rep_layouts[@intFromEnum(program.root_reps.items[0])].worker;
    try std.testing.expectEqual(std.meta.Tag(RuntimeLayout).dynamic_box, std.meta.activeTag(rep_layout));
    try std.testing.expectEqual(layout.LayoutTag.box_of_zst, store.getLayout(rep_layout.layoutIdx()).tag);
    try std.testing.expect(rep_layout.descriptor() != null);
}

test "boxy layout planner reuses dynamic storage for Box(a) worker layout" {
    const gpa = std.testing.allocator;

    const type_pool = [_]checked.CheckedTypeId{@enumFromInt(0)};
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
    try std.testing.expectEqual(layout.LayoutTag.box_of_zst, store.getLayout(box_layouts.host.layoutIdx()).tag);
    try std.testing.expectEqual(std.meta.Tag(RuntimeLayout).dynamic_box, std.meta.activeTag(box_layouts.worker));
    try std.testing.expectEqual(box_layouts.host.layoutIdx(), box_layouts.worker.layoutIdx());
}

test "boxy layout planner substitutes dynamic boxes into list elements" {
    const gpa = std.testing.allocator;

    const type_pool = [_]checked.CheckedTypeId{@enumFromInt(0)};
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

test "boxy layout planner records private worker function arg and return layouts" {
    const gpa = std.testing.allocator;

    const type_pool = [_]checked.CheckedTypeId{
        @enumFromInt(0), // List(a) argument.
        @enumFromInt(0), // Function argument a.
    };
    const payloads = [_]checked.StoredCheckedTypePayload{
        .{ .flex = .{} },
        .{ .nominal = builtinNominal(.list, @enumFromInt(1), .{ .start = 0, .len = 1 }) },
        .{ .function = .{
            .kind = .pure,
            .args = .{ .start = 1, .len = 1 },
            .ret = @enumFromInt(1),
            .needs_instantiation = false,
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
            .source = .{ .def = @enumFromInt(0) },
            .checked_type = @enumFromInt(2),
            .abi = .roc,
            .exposure = .private,
        },
    };

    var program = try Plan.analyzeProgram(gpa, .{ .checked_types = view, .roots = &roots }, .{});
    defer program.deinit();

    var store = try layout.Store.init(gpa, .u64);
    defer store.deinit();

    var layouts = try build(gpa, &program, &store, .{});
    defer layouts.deinit();

    try std.testing.expectEqual(@as(usize, 1), layouts.roots.items.len);
    const root = layouts.roots.items[0];
    const worker_args = layouts.rootLayoutSlice(root.worker_args);
    try std.testing.expectEqual(@as(usize, 1), worker_args.len);
    try std.testing.expectEqual(std.meta.Tag(RuntimeLayout).dynamic_box, std.meta.activeTag(worker_args[0]));

    const ret = root.worker_ret orelse return error.TestUnexpectedResult;
    const ret_layout = store.getLayout(ret.layoutIdx());
    try std.testing.expectEqual(layout.LayoutTag.list, ret_layout.tag);
    try std.testing.expectEqual(layouts.dynamic_storage_layout, ret_layout.getIdx());
    try std.testing.expectEqual(@as(usize, 0), layouts.rootLayoutSlice(root.host_args).len);
    try std.testing.expect(root.host_ret == null);
}

test "boxy layout planner commits nominal declared fields through shared layout store" {
    const gpa = std.testing.allocator;

    const field_a: RecordFieldLabelId = @enumFromInt(1);
    const field_b: RecordFieldLabelId = @enumFromInt(2);
    const type_pool = [_]checked.CheckedTypeId{@enumFromInt(0)};
    const record_fields = [_]checked.CheckedRecordField{
        .{ .name = field_a, .ty = @enumFromInt(0) },
        .{ .name = field_b, .ty = @enumFromInt(1) },
    };
    const declared_fields = [_]checked.CheckedDeclaredField{
        .{ .named = field_a },
        .{ .padding = 0 },
        .{ .named = field_b },
    };
    const payloads = [_]checked.StoredCheckedTypePayload{
        .{ .nominal = builtinNominal(.u8, @enumFromInt(0), .{}) },
        .{ .nominal = builtinNominal(.u16, @enumFromInt(1), .{}) },
        .{ .empty_record = {} },
        .{ .record = .{ .fields = .{ .start = 0, .len = 2 }, .ext = @enumFromInt(2) } },
        .{ .nominal = .{
            .name = @enumFromInt(3),
            .origin_module = @enumFromInt(4),
            .is_opaque = false,
            .backing = @enumFromInt(3),
            .representation = .{ .local_declaration = @enumFromInt(0) },
            .padding_field_types = .{ .start = 0, .len = 1 },
            .declared_fields = .{ .start = 0, .len = 3 },
        } },
    };
    const view = checked.CheckedTypeStoreView{
        .stored_payloads = &payloads,
        .type_id_pool = &type_pool,
        .record_field_pool = &record_fields,
        .declared_field_pool = &declared_fields,
    };

    var program = try Plan.analyzeCheckedTypes(gpa, view, &.{@as(checked.CheckedTypeId, @enumFromInt(4))}, .{});
    defer program.deinit();

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

fn builtinNominal(
    builtin: checked.CheckedBuiltinNominal,
    backing: checked.CheckedTypeId,
    args: checked.CheckedTypeRange,
) checked.StoredNominal {
    return .{
        .name = @enumFromInt(0),
        .origin_module = @enumFromInt(0),
        .builtin = builtin,
        .is_opaque = false,
        .backing = backing,
        .representation = .{ .builtin = builtin },
        .args = args,
    };
}
