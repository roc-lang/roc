//! Store LIR interpreter results as checked constants.

const std = @import("std");
const builtin = @import("builtin");
const collections = @import("collections");
const base = @import("base");
const builtins = @import("builtins");
const can = @import("can");
const check = @import("check");
const layout = @import("layout");
const lir = @import("lir");

const Interpreter = @import("interpreter.zig").Interpreter;
const Value = @import("value.zig").Value;

const Allocator = std.mem.Allocator;

/// Allocation failures and terminal invalid callable registry reports.
pub const Error = Allocator.Error || error{RuntimeError};
const checked = check.CheckedArtifact;
const const_store = check.ConstStore;
const LirProgram = lir.Program;
const RocList = builtins.list.RocList;
const RocStr = builtins.str.RocStr;

const RuntimeValueAddress = struct {
    ptr: usize,
    len: usize,
    plan: u32,
    layout: u32,
    storage: LirProgram.CaptureSlotStorage,
};

/// Runtime erased-callable identity decoded into the LIR proc and capture data.
pub const ErasedCallableResolution = struct {
    proc: lir.LIR.LirProcSpecId,
    capture_ptr: [*]u8,
};

/// Resolves erased-callable runtime data for the active evaluator.
pub const ErasedCallableResolver = struct {
    context: ?*anyopaque = null,
    resolve: *const fn (?*anyopaque, [*]u8) error{RuntimeError}!ErasedCallableResolution = interpreterErasedCallable,
};

const TagBase = struct {
    value: Value,
    layout_idx: layout.Idx,
};

const StrBacking = struct {
    data: const_store.ConstBlobDataId,
    len: usize,
};

/// Stores interpreted compile-time roots into a checked ConstStore.
pub const Writer = struct {
    allocator: Allocator,
    module: *checked.CheckedModuleArtifact,
    program: *const LirProgram.Result,
    stored_values: std.AutoHashMap(RuntimeValueAddress, checked.ConstNodeId),
    visited_str_values: std.AutoHashMap(RuntimeValueAddress, void),
    str_backings: std.AutoHashMap(usize, StrBacking),
    erased_callable_resolver: ErasedCallableResolver,
    product_plans: std.AutoHashMap(ProductPlanKey, ?lir.PackedData.Plan),
    product_eligibility: collections.DenseMap(LirProgram.ConstPlanId, bool),

    const ProductPlanKey = struct { plan: LirProgram.ConstPlanId, layout_idx: layout.Idx };

    pub fn init(
        allocator: Allocator,
        module: *checked.CheckedModuleArtifact,
        program: *const LirProgram.Result,
    ) Writer {
        return .{
            .allocator = allocator,
            .module = module,
            .program = program,
            .stored_values = std.AutoHashMap(RuntimeValueAddress, checked.ConstNodeId).init(allocator),
            .visited_str_values = std.AutoHashMap(RuntimeValueAddress, void).init(allocator),
            .str_backings = std.AutoHashMap(usize, StrBacking).init(allocator),
            .erased_callable_resolver = .{},
            .product_plans = std.AutoHashMap(ProductPlanKey, ?lir.PackedData.Plan).init(allocator),
            .product_eligibility = collections.DenseMap(LirProgram.ConstPlanId, bool).init(allocator),
        };
    }

    pub fn setErasedCallableResolver(self: *Writer, resolver: ErasedCallableResolver) void {
        self.erased_callable_resolver = resolver;
    }

    pub fn deinit(self: *Writer) void {
        var plans = self.product_plans.valueIterator();
        while (plans.next()) |plan| if (plan.*) |*present| present.deinit();
        self.product_plans.deinit();
        self.product_eligibility.deinit();
        self.str_backings.deinit();
        self.visited_str_values.deinit();
        self.stored_values.deinit();
    }

    pub fn storeRoot(
        self: *Writer,
        root: LirProgram.ConstRootPlan,
        value: Value,
    ) Error!checked.CompileTimeRootPayload {
        // Runtime addresses are only meaningful while storing the current
        // evaluated root. The interpreter drops each root after storage, so
        // later roots may reuse those addresses for unrelated values.
        self.stored_values.clearRetainingCapacity();
        self.visited_str_values.clearRetainingCapacity();
        self.str_backings.clearRetainingCapacity();

        const plan = self.constPlan(root.plan);
        try self.collectStrBackings(root.plan, root.ret_layout, value);
        return switch (root.request.kind) {
            .compile_time_constant, .repl_expr => .{ .const_node = try self.storeValue(root.plan, root.ret_layout, value) },
            .compile_time_callable => switch (plan) {
                .fn_value => |set| .{ .fn_value = try self.storeFnValue(set, root.ret_layout, value) },
                .erased_fn => |set| .{ .fn_value = try self.storeErasedFn(set, value) },
                .pending,
                .boxy_box,
                .layout_only,
                .zst,
                .scalar,
                .str,
                .list,
                .box,
                .tuple,
                .record,
                .tag_union,
                .named,
                => writerInvariant("compile-time callable root did not have a function const plan"),
            },
            .runtime_entrypoint,
            .provided_export,
            .platform_required_binding,
            .hosted_export,
            .test_expect,
            .dev_expr,
            => writerInvariant("non compile-time root reached ConstStore writer"),
        };
    }

    /// Preserve the exact producer-owned representation of a compile-time
    /// root in the checked module's durable ConstStore type table.
    pub fn storeRootType(
        self: *Writer,
        root: LirProgram.ConstRootPlan,
    ) Error!const_store.ConstTypeId {
        return self.module.const_store.type_store.cloneTypeFromTranslated(
            &self.program.const_types,
            &self.program.const_type_names,
            &self.module.canonical_names,
            root.ret_type,
        );
    }

    /// One value to store or scan.
    const ValueRequest = struct {
        plan: LirProgram.ConstPlanId,
        layout_idx: layout.Idx,
        value: Value,
        storage: LirProgram.CaptureSlotStorage = .value,
    };

    const StoreFinish = union(enum) {
        list: checked.ConstNodeId,
        box: checked.ConstNodeId,
        tuple: checked.ConstNodeId,
        record: checked.ConstNodeId,
        nominal: struct { node: checked.ConstNodeId, named: @FieldType(LirProgram.ConstPlan, "named") },
        tag: struct { node: checked.ConstNodeId, name: []const u8 },
        /// A function value; `node` is null for a callable root, whose
        /// function id is the store's result.
        fn_value: struct { node: ?checked.ConstNodeId, template: LirProgram.FnTemplate },
    };

    /// A composite value whose components are stored in order, each
    /// component's whole subtree before the next component starts. Every
    /// component's node is reserved when it starts, and the composite fills
    /// its own node once every component is stored.
    const StoreFrame = struct {
        finish: StoreFinish,
        requests: []ValueRequest,
        nodes: []checked.ConstNodeId,
        /// Capture slots of a function value: each slot's type is cloned just
        /// before its value is stored.
        slots: []const LirProgram.CaptureSlot = &.{},
        capture_types: []const_store.ConstTypeId = &.{},
        index: usize = 0,
    };

    fn storeValue(
        self: *Writer,
        plan_id: LirProgram.ConstPlanId,
        layout_idx: layout.Idx,
        value: Value,
    ) Error!checked.ConstNodeId {
        var frames = std.ArrayList(StoreFrame).empty;
        defer self.releaseStoreFrames(&frames);
        const node = try self.startStore(&frames, .{ .plan = plan_id, .layout_idx = layout_idx, .value = value });
        _ = try self.drainStore(&frames);
        return node;
    }

    fn storeFnValue(
        self: *Writer,
        set_id: LirProgram.FnSetId,
        layout_idx: layout.Idx,
        value: Value,
    ) Error!checked.ConstFnId {
        var frames = std.ArrayList(StoreFrame).empty;
        defer self.releaseStoreFrames(&frames);
        try self.pushFnValue(&frames, null, set_id, layout_idx, value);
        return (try self.drainStore(&frames)).?;
    }

    fn storeErasedFn(
        self: *Writer,
        set_id: LirProgram.ErasedFnsId,
        value: Value,
    ) Error!checked.ConstFnId {
        var frames = std.ArrayList(StoreFrame).empty;
        defer self.releaseStoreFrames(&frames);
        try self.pushErasedFn(&frames, null, set_id, value);
        return (try self.drainStore(&frames)).?;
    }

    fn releaseStoreFrames(self: *Writer, frames: *std.ArrayList(StoreFrame)) void {
        for (frames.items) |*frame| self.freeStoreFrame(frame);
        frames.deinit(self.allocator);
    }

    fn freeStoreFrame(self: *Writer, frame: *StoreFrame) void {
        self.allocator.free(frame.requests);
        self.allocator.free(frame.nodes);
        self.allocator.free(frame.capture_types);
    }

    /// Stores every pending component, and returns the function id of a
    /// callable root.
    fn drainStore(self: *Writer, frames: *std.ArrayList(StoreFrame)) Error!?checked.ConstFnId {
        var root_fn: ?checked.ConstFnId = null;
        while (frames.items.len > 0) {
            const top = frames.items.len - 1;
            const frame = &frames.items[top];
            if (frame.index < frame.requests.len) {
                const index = frame.index;
                frame.index += 1;
                const request = frame.requests[index];
                if (frame.slots.len > 0) {
                    frame.capture_types[index] = try self.cloneCaptureType(frame.slots[index].ty);
                }
                const node = try self.startStore(frames, request);
                frames.items[top].nodes[index] = node;
                continue;
            }
            var finished = frames.pop().?;
            defer self.freeStoreFrame(&finished);
            if (try self.finishStore(&finished)) |fn_id| root_fn = fn_id;
        }
        return root_fn;
    }

    /// Reserves the request's node, fills it when the value has no
    /// components, and pushes the frame that stores its components otherwise.
    fn startStore(self: *Writer, frames: *std.ArrayList(StoreFrame), request: ValueRequest) Error!checked.ConstNodeId {
        if (self.memoAddress(request.plan, request.layout_idx, request.value, request.storage)) |address| {
            if (self.stored_values.get(address)) |existing| return existing;
            const node = try self.module.const_store.reserve();
            try self.stored_values.put(address, node);
            try self.beginFresh(frames, node, request);
            return node;
        }
        const node = try self.module.const_store.reserve();
        try self.beginFresh(frames, node, request);
        return node;
    }

    fn beginFresh(
        self: *Writer,
        frames: *std.ArrayList(StoreFrame),
        node: checked.ConstNodeId,
        request: ValueRequest,
    ) Error!void {
        var layout_idx = request.layout_idx;
        var value = request.value;
        switch (request.storage) {
            .value => {},
            .recursive_box => {
                const boxed = self.program.layouts.getLayout(layout_idx);
                if (boxed.tag != .box) writerInvariant("recursive capture slot did not use box storage");
                const payload = self.readBoxDataPointer(value) orelse
                    writerInvariant("recursive capture slot had null payload pointer");
                layout_idx = boxed.getIdx();
                value = .{ .ptr = payload };
            },
        }
        var children = std.ArrayList(ValueRequest).empty;
        defer children.deinit(self.allocator);
        const finish: StoreFinish = switch (self.constPlan(request.plan)) {
            .boxy_box => writerInvariant("Boxy-only box reached ConstStore writer"),
            .pending => writerInvariant("pending const plan reached ConstStore writer"),
            .layout_only => writerInvariant("layout-only const plan reached ConstStore writer"),
            .zst => return self.module.const_store.fill(node, .zst),
            .scalar => return self.module.const_store.fill(node, .{ .scalar = self.storeScalar(layout_idx, value) }),
            .str => return self.module.const_store.fill(node, try self.storeStr(value)),
            .list => |elem_plan| blk: {
                if (try self.storeListLeaf(node, elem_plan, layout_idx, value)) return;
                try self.appendListRequests(&children, elem_plan, layout_idx, value);
                break :blk .{ .list = node };
            },
            .box => |elem_plan| blk: {
                try self.appendBoxRequest(&children, elem_plan, layout_idx, value);
                break :blk .{ .box = node };
            },
            .tuple => |items| blk: {
                try self.appendStructRequests(&children, items, layout_idx, value);
                break :blk .{ .tuple = node };
            },
            .record => |fields| blk: {
                try self.appendStructRequests(&children, fields, layout_idx, value);
                break :blk .{ .record = node };
            },
            .tag_union => |variants| blk: {
                const tag_base = self.resolveTagBase(layout_idx, value);
                const selected = self.selectTagVariant(variants, tag_base.layout_idx, tag_base.value);
                const payload_layout = self.tagPayloadLayout(tag_base.layout_idx, selected.discriminant);
                try self.appendTagPayloadRequests(&children, selected.payloads, payload_layout, tag_base.value);
                break :blk .{ .tag = .{ .node = node, .name = selected.name } };
            },
            .named => |named| blk: {
                try children.append(self.allocator, .{ .plan = named.backing, .layout_idx = layout_idx, .value = value });
                break :blk .{ .nominal = .{ .node = node, .named = named } };
            },
            .fn_value => |set| return self.pushFnValue(frames, node, set, layout_idx, value),
            .erased_fn => |set| return self.pushErasedFn(frames, node, set, value),
        };
        try self.pushStoreFrame(frames, finish, &children, &.{});
    }

    fn pushStoreFrame(
        self: *Writer,
        frames: *std.ArrayList(StoreFrame),
        finish: StoreFinish,
        children: *std.ArrayList(ValueRequest),
        slots: []const LirProgram.CaptureSlot,
    ) Allocator.Error!void {
        try frames.ensureUnusedCapacity(self.allocator, 1);
        const nodes = try self.allocator.alloc(checked.ConstNodeId, children.items.len);
        errdefer self.allocator.free(nodes);
        const capture_types = try self.allocator.alloc(const_store.ConstTypeId, slots.len);
        errdefer self.allocator.free(capture_types);
        const requests = try children.toOwnedSlice(self.allocator);
        frames.appendAssumeCapacity(.{
            .finish = finish,
            .requests = requests,
            .nodes = nodes,
            .slots = slots,
            .capture_types = capture_types,
        });
    }

    fn pushFnValue(
        self: *Writer,
        frames: *std.ArrayList(StoreFrame),
        node: ?checked.ConstNodeId,
        set_id: LirProgram.FnSetId,
        layout_idx: layout.Idx,
        value: Value,
    ) Error!void {
        const set = self.program.fn_sets.items[@backingInt(set_id)];
        const tag_base = self.resolveTagBase(layout_idx, value);
        const variant = self.selectFnVariant(set, tag_base.layout_idx, tag_base.value);
        var children = std.ArrayList(ValueRequest).empty;
        defer children.deinit(self.allocator);
        try self.appendCaptureRequests(&children, variant.captures, variant.payload_layout, tag_base.value);
        try self.pushStoreFrame(frames, .{ .fn_value = .{ .node = node, .template = variant.template } }, &children, variant.captures);
    }

    fn pushErasedFn(
        self: *Writer,
        frames: *std.ArrayList(StoreFrame),
        node: ?checked.ConstNodeId,
        set_id: LirProgram.ErasedFnsId,
        value: Value,
    ) Error!void {
        const entry = try self.resolveErasedEntry(set_id, value);
        var children = std.ArrayList(ValueRequest).empty;
        defer children.deinit(self.allocator);
        try self.appendCaptureRequests(&children, entry.entry.captures, entry.entry.capture_layout, .{ .ptr = entry.capture_ptr });
        const template = entry.entry.template orelse writerInvariant("Boxy frozen environment has no ConstStore provenance");
        try self.pushStoreFrame(frames, .{ .fn_value = .{ .node = node, .template = template } }, &children, entry.entry.captures);
    }

    const ResolvedErasedEntry = struct {
        entry: LirProgram.ErasedFn,
        capture_ptr: [*]u8,
    };

    fn resolveErasedEntry(self: *Writer, set_id: LirProgram.ErasedFnsId, value: Value) Error!ResolvedErasedEntry {
        const set = self.program.erased_fns.items[@backingInt(set_id)];
        const data_ptr = self.readErasedCallablePointer(value);
        const resolved = try self.erased_callable_resolver.resolve(self.erased_callable_resolver.context, data_ptr);
        for (set.entries) |entry| {
            if (entry.entry != resolved.proc) continue;
            return .{ .entry = entry, .capture_ptr = resolved.capture_ptr };
        }
        writerInvariant("erased callable result did not match an explicit erased function entry");
    }

    /// Fills a finished composite's node, and returns the function id when
    /// the composite is a callable root.
    fn finishStore(self: *Writer, frame: *const StoreFrame) Error!?checked.ConstFnId {
        const nodes = frame.nodes;
        switch (frame.finish) {
            .list => |node| self.module.const_store.fill(node, .{ .list = .{ .nodes = nodes } }),
            .box => |node| self.module.const_store.fill(node, .{ .box = nodes[0] }),
            .tuple => |node| self.module.const_store.fill(node, .{ .tuple = nodes }),
            .record => |node| self.module.const_store.fill(node, .{ .record = nodes }),
            .nominal => |nominal| self.module.const_store.fill(nominal.node, .{ .nominal = .{
                .named_type = nominal.named.named_type,
                .backing = nodes[0],
            } }),
            .tag => |tag| {
                const tag_name = try self.module.const_store.allocator.dupe(u8, tag.name);
                defer self.module.const_store.allocator.free(tag_name);
                self.module.const_store.fill(tag.node, .{ .tag = .{
                    .tag_name = tag_name,
                    .payloads = nodes,
                } });
            },
            .fn_value => |fn_value| {
                const captures = try self.module.const_store.allocator.alloc(const_store.ConstCapture, frame.slots.len);
                defer self.module.const_store.allocator.free(captures);
                for (captures, frame.slots, frame.capture_types, nodes) |*capture, slot, ty, node| {
                    capture.* = .{ .id = slot.id, .ty = ty, .value = node };
                }
                const template = fn_value.template;
                const fn_id = try self.module.const_store.appendFn(.{
                    .fn_def = template.fn_def,
                    .source_fn_ty = template.source_fn_ty,
                    .source_fn_key = template.source_fn_key,
                    .captures = captures,
                    .evidence = template.evidence,
                    .evidence_frames = template.evidence_frames,
                    .evidence_frame_head = template.evidence_frame_head,
                });
                if (fn_value.node) |node| {
                    self.module.const_store.fill(node, .{ .fn_value = fn_id });
                    return null;
                }
                return fn_id;
            },
        }
        return null;
    }

    /// Stores a list whose elements need no nodes of their own (an empty,
    /// packed-scalar, or packed-product list); false when its elements are
    /// stored as nodes.
    fn storeListLeaf(
        self: *Writer,
        target_node: checked.ConstNodeId,
        elem_plan: LirProgram.ConstPlanId,
        layout_idx: layout.Idx,
        value: Value,
    ) Error!bool {
        const layout_value = self.program.layouts.getLayout(layout_idx);
        if (layout_value.tag != .list and layout_value.tag != .list_of_zst) {
            writerInvariant("list const plan had non-list layout");
        }
        const roc_list: *const RocList = @ptrCast(@alignCast(value.ptr));
        if (roc_list.len() == 0) {
            self.module.const_store.fill(target_node, .{ .list = .{ .empty = roc_list.getCapacity() } });
            return true;
        }
        if (self.planIsScalar(elem_plan)) {
            try self.storePackedList(target_node, layout_value, roc_list);
            return true;
        }
        const elem_layout = if (layout_value.tag == .list_of_zst) layout.Idx.zst else layout_value.getIdx();
        if (try self.productPlan(elem_plan, elem_layout)) |plan| {
            try self.storeProductList(target_node, plan, roc_list);
            return true;
        }
        return false;
    }

    fn appendListRequests(
        self: *Writer,
        out: *std.ArrayList(ValueRequest),
        elem_plan: LirProgram.ConstPlanId,
        layout_idx: layout.Idx,
        value: Value,
    ) Allocator.Error!void {
        const layout_value = self.program.layouts.getLayout(layout_idx);
        const roc_list: *const RocList = @ptrCast(@alignCast(value.ptr));
        try out.ensureUnusedCapacity(self.allocator, roc_list.len());
        if (layout_value.tag == .list_of_zst) {
            for (0..roc_list.len()) |_| out.appendAssumeCapacity(.{ .plan = elem_plan, .layout_idx = .zst, .value = Value.zst });
            return;
        }
        const elem_layout = layout_value.getIdx();
        const elem_size: usize = self.program.layouts.layoutSize(self.program.layouts.getLayout(elem_layout));
        if (roc_list.bytes) |bytes| {
            for (0..roc_list.len()) |index| {
                out.appendAssumeCapacity(.{ .plan = elem_plan, .layout_idx = elem_layout, .value = .{ .ptr = bytes + index * elem_size } });
            }
        } else if (roc_list.len() != 0) {
            writerInvariant("non-empty list had null element pointer");
        }
    }

    fn appendBoxRequest(
        self: *Writer,
        out: *std.ArrayList(ValueRequest),
        elem_plan: LirProgram.ConstPlanId,
        layout_idx: layout.Idx,
        value: Value,
    ) Allocator.Error!void {
        const layout_value = self.program.layouts.getLayout(layout_idx);
        try out.append(self.allocator, switch (layout_value.tag) {
            .box_of_zst => .{ .plan = elem_plan, .layout_idx = .zst, .value = Value.zst },
            .box => .{
                .plan = elem_plan,
                .layout_idx = layout_value.getIdx(),
                .value = .{ .ptr = self.readBoxDataPointer(value) orelse writerInvariant("boxed value had null payload pointer") },
            },
            .erased_callable => .{ .plan = elem_plan, .layout_idx = layout_idx, .value = value },
            .scalar,
            .erased_box,
            .list,
            .list_of_zst,
            .struct_,
            .closure,
            .zst,
            .tag_union,
            .ptr,
            => writerInvariant("box const plan had incompatible layout"),
        });
    }

    fn appendStructRequests(
        self: *Writer,
        out: *std.ArrayList(ValueRequest),
        plans: []const LirProgram.ConstPlanId,
        struct_layout: layout.Idx,
        struct_value: Value,
    ) Allocator.Error!void {
        if (plans.len == 0) return;
        var layout_idx = struct_layout;
        var value = struct_value;
        var layout_value = self.program.layouts.getLayout(layout_idx);
        while (layout_value.tag == .box) {
            const ptr = self.readBoxDataPointer(value) orelse writerInvariant("boxed struct value had null payload pointer");
            layout_idx = layout_value.getIdx();
            value = .{ .ptr = ptr };
            layout_value = self.program.layouts.getLayout(layout_idx);
        }
        try out.ensureUnusedCapacity(self.allocator, plans.len);
        if (layout_value.tag == .zst or layout_value.tag == .box_of_zst) {
            for (plans) |plan| out.appendAssumeCapacity(.{ .plan = plan, .layout_idx = .zst, .value = Value.zst });
            return;
        }
        if (layout_value.tag != .struct_) writerInvariant("struct const plan had non-struct layout");
        for (plans, 0..) |plan, index| {
            const field_layout = self.program.layouts.getStructFieldLayoutByOriginalIndex(layout_value.getStruct().idx, @intCast(index));
            const offset = self.program.layouts.getStructFieldOffsetByOriginalIndex(layout_value.getStruct().idx, @intCast(index));
            out.appendAssumeCapacity(.{ .plan = plan, .layout_idx = field_layout, .value = value.offset(offset) });
        }
    }

    fn appendTagPayloadRequests(
        self: *Writer,
        out: *std.ArrayList(ValueRequest),
        plans: []const LirProgram.ConstPlanId,
        payload_layout: layout.Idx,
        value: Value,
    ) Allocator.Error!void {
        if (plans.len == 0) return;
        try out.ensureUnusedCapacity(self.allocator, plans.len);
        if (plans.len == 1) {
            out.appendAssumeCapacity(.{ .plan = plans[0], .layout_idx = payload_layout, .value = value });
            return;
        }
        const layout_value = self.program.layouts.getLayout(payload_layout);
        if (layout_value.tag == .zst) {
            for (plans) |plan| out.appendAssumeCapacity(.{ .plan = plan, .layout_idx = .zst, .value = Value.zst });
        } else if (layout_value.tag == .struct_) {
            for (plans, 0..) |plan, index| {
                const field_layout = self.program.layouts.getStructFieldLayoutByOriginalIndex(layout_value.getStruct().idx, @intCast(index));
                const offset = self.program.layouts.getStructFieldOffsetByOriginalIndex(layout_value.getStruct().idx, @intCast(index));
                out.appendAssumeCapacity(.{ .plan = plan, .layout_idx = field_layout, .value = value.offset(offset) });
            }
        } else {
            writerInvariant("multi-payload tag did not use a struct payload layout");
        }
    }

    fn appendCaptureRequests(
        self: *Writer,
        out: *std.ArrayList(ValueRequest),
        slots: []const LirProgram.CaptureSlot,
        payload_layout: layout.Idx,
        payload_value: Value,
    ) Allocator.Error!void {
        if (slots.len == 0) return;
        try out.ensureUnusedCapacity(self.allocator, slots.len);
        const layout_value = self.program.layouts.getLayout(payload_layout);
        if (layout_value.tag == .zst) {
            for (slots) |slot| out.appendAssumeCapacity(.{ .plan = slot.plan, .layout_idx = .zst, .value = Value.zst, .storage = slot.storage });
        } else if (layout_value.tag == .struct_) {
            for (slots) |slot| {
                const field_layout = self.program.layouts.getStructFieldLayoutByOriginalIndex(layout_value.getStruct().idx, slot.slot);
                const offset = self.program.layouts.getStructFieldOffsetByOriginalIndex(layout_value.getStruct().idx, slot.slot);
                out.appendAssumeCapacity(.{ .plan = slot.plan, .layout_idx = field_layout, .value = payload_value.offset(offset), .storage = slot.storage });
            }
        } else if (slots.len == 1) {
            out.appendAssumeCapacity(.{ .plan = slots[0].plan, .layout_idx = payload_layout, .value = payload_value, .storage = slots[0].storage });
        } else {
            writerInvariant("multi-capture function did not use a struct capture layout");
        }
    }

    fn storeScalar(self: *Writer, layout_idx: layout.Idx, value: Value) checked.ConstScalar {
        const layout_value = self.program.layouts.getLayout(layout_idx);
        if (layout_value.tag != .scalar) writerInvariant("scalar const plan had non-scalar layout");
        const scalar = layout_value.getScalar();
        return switch (scalar.tag) {
            .str => writerInvariant("string scalar layout reached scalar const plan"),
            .int => switch (scalar.getInt()) {
                .u8 => .{ .u8 = value.read(u8) },
                .i8 => .{ .i8 = value.read(i8) },
                .u16 => .{ .u16 = value.read(u16) },
                .i16 => .{ .i16 = value.read(i16) },
                .u32 => .{ .u32 = value.read(u32) },
                .i32 => .{ .i32 = value.read(i32) },
                .u64 => .{ .u64 = value.read(u64) },
                .i64 => .{ .i64 = value.read(i64) },
                .u128 => .{ .u128 = value.read(u128) },
                .i128 => .{ .i128 = value.read(i128) },
            },
            // A stored NaN is Roc's one NaN, whatever bits the evaluation
            // produced.
            .frac => switch (scalar.getFrac()) {
                .f32 => .{ .f32_bits = builtins.float_bits.normalizeF32NanBits(@bitCast(value.read(f32))) },
                .f64 => .{ .f64_bits = builtins.float_bits.normalizeF64NanBits(@bitCast(value.read(f64))) },
                .dec => .{ .dec_bits = value.read(builtins.dec.RocDec).num },
            },
            .opaque_ptr => writerInvariant("opaque pointer scalar layout reached scalar const plan"),
            .vector => .{ .u128 = value.read(u128) },
        };
    }

    fn storeStr(self: *Writer, value: Value) Error!checked.ConstValue {
        const roc_str: *const RocStr = @ptrCast(@alignCast(value.ptr));
        const slice = roc_str.asSlice();
        const len = checkedU32(slice.len, "string length exceeds ConstStore limit");

        if (!roc_str.isSmallStr()) {
            if (roc_str.isSeamlessSlice()) {
                if (roc_str.getAllocationPtr()) |alloc_ptr| {
                    const alloc_address = @intFromPtr(alloc_ptr);
                    const slice_address = @intFromPtr(slice.ptr);
                    if (slice_address < alloc_address) {
                        writerInvariant("string slice view started before its recorded backing bytes");
                    }
                    const slice_start = slice_address - alloc_address;
                    if (self.str_backings.get(alloc_address)) |backing| {
                        if (slice_start > backing.len or slice.len > backing.len - slice_start) {
                            writerInvariant("string slice view was outside its recorded backing bytes");
                        }
                        return .{ .str = .{
                            .data = backing.data,
                            .offset = checkedU32(slice_start, "string slice offset exceeds ConstStore limit"),
                            .len = len,
                        } };
                    }
                }
            } else if (roc_str.bytes) |bytes| {
                const address = @intFromPtr(bytes);
                const backing = self.str_backings.get(address) orelse try self.addStrBacking(address, slice);
                return .{ .str = .{
                    .data = backing.data,
                    .offset = 0,
                    .len = len,
                } };
            }
        }

        const data = try self.module.const_store.addBlobData(slice);
        return .{ .str = .{
            .data = data,
            .offset = 0,
            .len = len,
        } };
    }

    fn planIsProduct(self: *Writer, root: LirProgram.ConstPlanId) Allocator.Error!bool {
        const Frame = struct {
            id: LirProgram.ConstPlanId,
            children: []const LirProgram.ConstPlanId,
            index: usize = 0,
        };
        var frames = std.ArrayList(Frame).empty;
        defer frames.deinit(self.allocator);
        // The answer of the plan just entered or finished; null when entering
        // it pushed a frame.
        var answer = try self.enterProductPlan(&frames, root);
        while (frames.items.len > 0) {
            const top = &frames.items[frames.items.len - 1];
            const failed = if (answer) |child_answer| !child_answer else false;
            if (!failed and top.index < top.children.len) {
                const child = top.children[top.index];
                top.index += 1;
                answer = try self.enterProductPlan(&frames, child);
                continue;
            }
            const done = frames.pop().?;
            try self.product_eligibility.put(done.id, !failed);
            answer = !failed;
        }
        return answer.?;
    }

    /// Answers a plan whose eligibility is known or needs no components, and
    /// pushes a frame for a plan whose components decide it.
    fn enterProductPlan(self: *Writer, frames: anytype, id: LirProgram.ConstPlanId) Allocator.Error!?bool {
        if (self.product_eligibility.get(id)) |result| return result;
        // A cycle cannot be a fixed product. Recursive edges remain graph data.
        try self.product_eligibility.put(id, false);
        const raw = @backingInt(id);
        if (raw >= self.program.const_plans.items.len) writerInvariant("const plan id is out of range");
        const children: []const LirProgram.ConstPlanId = switch (self.program.const_plans.items[raw]) {
            .scalar, .zst => {
                try self.product_eligibility.put(id, true);
                return true;
            },
            .named => |*named| (&named.backing)[0..1],
            .record, .tuple => |children| children,
            .pending, .layout_only => unreachable,
            .str, .list, .box, .boxy_box, .tag_union, .fn_value, .erased_fn => return false,
        };
        try frames.append(self.allocator, .{ .id = id, .children = children });
        return null;
    }

    fn productPlan(self: *Writer, id: LirProgram.ConstPlanId, idx: layout.Idx) Allocator.Error!?*const lir.PackedData.Plan {
        const key = ProductPlanKey{ .plan = id, .layout_idx = idx };
        if (self.product_plans.getPtr(key)) |entry| return if (entry.*) |*plan| plan else null;
        const plan: ?lir.PackedData.Plan = if (try self.planIsProduct(id))
            try lir.PackedData.Plan.init(self.allocator, &self.program.layouts, idx)
        else
            null;
        errdefer if (plan) |value| {
            var owned = value;
            owned.deinit();
        };
        try self.product_plans.put(key, plan);
        return if (self.product_plans.getPtr(key).?.*) |*value| value else null;
    }

    fn storeProductList(self: *Writer, node: checked.ConstNodeId, plan: *const lir.PackedData.Plan, list: *const RocList) Allocator.Error!void {
        const byte_len = std.math.mul(usize, list.len(), plan.packed_width) catch unreachable;
        const memory_len = std.math.mul(usize, list.len(), plan.memory_width) catch unreachable;
        const memory = if (memory_len == 0) &.{} else list.bytes.?[0..memory_len];
        var converted: ?[]u8 = null;
        defer if (converted) |owned| self.allocator.free(owned);
        const bytes = if (plan.isIdentity() and builtin.cpu.arch.endian() == .little) memory else blk: {
            const out = try self.allocator.alloc(u8, byte_len);
            converted = out;
            plan.encode(out, memory, list.len(), builtin.cpu.arch.endian());
            break :blk out;
        };
        const data = try self.module.const_store.addBlobData(bytes);
        self.module.const_store.fill(node, .{ .list = .{ .packed_bytes = .{
            .bytes = .{ .data = data, .offset = 0, .len = checkedU32(byte_len, "packed product bytes exceed ConstStore limit") },
            .len = checkedU32(list.len(), "packed product length exceeds ConstStore limit"),
            .element = null,
            .product_width = plan.packed_width,
        } } });
    }

    fn planIsScalar(self: *const Writer, root: LirProgram.ConstPlanId) bool {
        var plan_id = root;
        while (true) return switch (self.constPlan(plan_id)) {
            .scalar => true,
            .named => |named| {
                plan_id = named.backing;
                continue;
            },
            .pending,
            .boxy_box,
            .layout_only,
            .zst,
            .str,
            .list,
            .box,
            .tuple,
            .record,
            .tag_union,
            .fn_value,
            .erased_fn,
            => false,
        };
    }

    fn packedScalarForLayout(self: *const Writer, layout_idx: layout.Idx) ?const_store.ConstPackedScalar {
        const layout_value = self.program.layouts.getLayout(layout_idx);
        if (layout_value.tag != .scalar) return null;
        const scalar = layout_value.getScalar();
        return switch (scalar.tag) {
            .str, .opaque_ptr => null,
            .int => switch (scalar.getInt()) {
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
            },
            .frac => switch (scalar.getFrac()) {
                .f32 => .f32,
                .f64 => .f64,
                .dec => .dec,
            },
            .vector => switch (scalar.getVector()) {
                .u8x16 => .u8x16,
                .i8x16 => .i8x16,
                .u16x8 => .u16x8,
                .i16x8 => .i16x8,
                .u32x4 => .u32x4,
                .i32x4 => .i32x4,
                .u64x2 => .u64x2,
                .i64x2 => .i64x2,
            },
        };
    }

    fn storePackedList(
        self: *Writer,
        target_node: checked.ConstNodeId,
        list_layout: layout.Layout,
        roc_list: *const RocList,
    ) Error!void {
        if (list_layout.tag != .list) writerInvariant("packed scalar list had non-list layout");
        const elem_layout = list_layout.getIdx();
        const element = self.packedScalarForLayout(elem_layout) orelse
            writerInvariant("scalar const plan had a non-packable list element layout");

        const byte_len = std.math.mul(usize, roc_list.len(), element.byteWidth()) catch
            writerInvariant("packed list byte length overflowed");
        const bytes = try self.allocator.alloc(u8, byte_len);
        defer self.allocator.free(bytes);

        if (roc_list.bytes) |source| {
            const width: usize = element.byteWidth();
            for (0..roc_list.len()) |index| {
                const scalar = self.storeScalar(elem_layout, .{ .ptr = source + index * width });
                writePackedScalar(bytes[index * width ..][0..width], element, scalar);
            }
        } else if (roc_list.len() != 0) {
            writerInvariant("non-empty packed list had null element pointer");
        }

        const data = try self.module.const_store.addBlobData(bytes);
        self.module.const_store.fill(target_node, .{ .list = .{ .packed_bytes = .{
            .bytes = .{ .data = data, .offset = 0, .len = checkedU32(byte_len, "packed list byte length exceeds ConstStore limit") },
            .len = checkedU32(roc_list.len(), "packed list length exceeds ConstStore limit"),
            .element = element,
        } } });
    }

    fn cloneCaptureType(self: *Writer, ty: const_store.ConstTypeId) Error!const_store.ConstTypeId {
        return self.module.const_store.type_store.cloneTypeFromTranslated(
            &self.program.const_types,
            &self.program.const_type_names,
            &self.module.canonical_names,
            ty,
        );
    }

    /// Records the backing bytes of every heap string reachable from the
    /// value, visiting components in order.
    fn collectStrBackings(
        self: *Writer,
        plan_id: LirProgram.ConstPlanId,
        layout_idx: layout.Idx,
        value: Value,
    ) Error!void {
        var pending = std.ArrayList(ValueRequest).empty;
        defer pending.deinit(self.allocator);
        try pending.append(self.allocator, .{ .plan = plan_id, .layout_idx = layout_idx, .value = value });
        while (pending.pop()) |request| {
            // Components are appended in order and reversed so the first
            // component is visited next.
            const mark = pending.items.len;
            try self.appendStrBackingComponents(&pending, request);
            std.mem.reverse(ValueRequest, pending.items[mark..]);
        }
    }

    fn appendStrBackingComponents(
        self: *Writer,
        out: *std.ArrayList(ValueRequest),
        request: ValueRequest,
    ) Error!void {
        const layout_idx = request.layout_idx;
        const value = request.value;
        if (self.memoAddress(request.plan, layout_idx, value, request.storage)) |address| {
            const entry = try self.visited_str_values.getOrPut(address);
            if (entry.found_existing) return;
            entry.value_ptr.* = {};
        }
        if (request.storage == .recursive_box) {
            const boxed = self.program.layouts.getLayout(layout_idx);
            if (boxed.tag != .box) writerInvariant("recursive capture slot did not use box storage");
            const payload = self.readBoxDataPointer(value) orelse
                writerInvariant("recursive capture slot had null payload pointer");
            return try out.append(self.allocator, .{ .plan = request.plan, .layout_idx = boxed.getIdx(), .value = .{ .ptr = payload } });
        }

        switch (self.constPlan(request.plan)) {
            .boxy_box => writerInvariant("Boxy-only box reached ConstStore writer"),
            .pending => writerInvariant("pending const plan reached string backing collection"),
            .layout_only => writerInvariant("layout-only const plan reached string backing collection"),
            .zst,
            .scalar,
            => {},
            .str => try self.collectStrValue(value),
            .list => |elem_plan| {
                const layout_value = self.program.layouts.getLayout(layout_idx);
                if (layout_value.tag != .list and layout_value.tag != .list_of_zst) {
                    writerInvariant("list const plan had non-list layout");
                }
                if (try self.planIsProduct(elem_plan)) return;
                try self.appendListRequests(out, elem_plan, layout_idx, value);
            },
            .box => |elem_plan| try self.appendBoxRequest(out, elem_plan, layout_idx, value),
            .tuple => |items| try self.appendStructRequests(out, items, layout_idx, value),
            .record => |fields| try self.appendStructRequests(out, fields, layout_idx, value),
            .tag_union => |variants| {
                const tag_base = self.resolveTagBase(layout_idx, value);
                const selected = self.selectTagVariant(variants, tag_base.layout_idx, tag_base.value);
                const payload_layout = self.tagPayloadLayout(tag_base.layout_idx, selected.discriminant);
                try self.appendTagPayloadRequests(out, selected.payloads, payload_layout, tag_base.value);
            },
            .named => |named| try out.append(self.allocator, .{ .plan = named.backing, .layout_idx = layout_idx, .value = value }),
            .fn_value => |set_id| {
                const set = self.program.fn_sets.items[@backingInt(set_id)];
                const tag_base = self.resolveTagBase(layout_idx, value);
                const variant = self.selectFnVariant(set, tag_base.layout_idx, tag_base.value);
                try self.appendCaptureRequests(out, variant.captures, variant.payload_layout, tag_base.value);
            },
            .erased_fn => |set_id| {
                const entry = try self.resolveErasedEntry(set_id, value);
                try self.appendCaptureRequests(out, entry.entry.captures, entry.entry.capture_layout, .{ .ptr = entry.capture_ptr });
            },
        }
    }

    fn collectStrValue(self: *Writer, value: Value) Error!void {
        const roc_str: *const RocStr = @ptrCast(@alignCast(value.ptr));
        if (roc_str.isSmallStr() or roc_str.isSeamlessSlice()) return;
        const bytes = roc_str.bytes orelse {
            if (roc_str.len() == 0) return;
            writerInvariant("non-empty string had null bytes pointer");
        };
        const address = @intFromPtr(bytes);
        if (self.str_backings.contains(address)) return;
        _ = try self.addStrBacking(address, roc_str.asSlice());
    }

    fn addStrBacking(self: *Writer, address: usize, bytes: []const u8) Error!StrBacking {
        const data = try self.module.const_store.addBlobData(bytes);
        const backing: StrBacking = .{
            .data = data,
            .len = bytes.len,
        };
        try self.str_backings.put(address, backing);
        return backing;
    }

    fn selectFnVariant(
        self: *Writer,
        set: LirProgram.FnSet,
        layout_idx: layout.Idx,
        value: Value,
    ) LirProgram.FnVariant {
        if (set.variants.len == 1) return set.variants[0];
        const discriminant = self.readTagDiscriminant(layout_idx, value);
        for (set.variants) |variant| {
            if (variant.discriminant == discriminant) return variant;
        }
        writerInvariant("finite callable result discriminant did not match an explicit variant");
    }

    fn selectTagVariant(
        self: *Writer,
        variants: []const LirProgram.ConstTagVariant,
        layout_idx: layout.Idx,
        value: Value,
    ) LirProgram.ConstTagVariant {
        const discriminant = self.readTagDiscriminant(layout_idx, value);
        for (variants) |variant| {
            if (variant.discriminant == discriminant) return variant;
        }
        writerInvariant("tag result discriminant did not match an explicit const variant");
    }

    fn readTagDiscriminant(self: *Writer, layout_idx: layout.Idx, value: Value) u32 {
        const layout_value = self.program.layouts.getLayout(layout_idx);
        return switch (layout_value.tag) {
            .zst => 0,
            .tag_union => self.program.layouts.getTagUnionData(layout_value.getTagUnion().idx).readDiscriminant(value.ptr, self.program.layouts.targetUsize()),
            .scalar,
            .box,
            .box_of_zst,
            .erased_box,
            .list,
            .list_of_zst,
            .struct_,
            .closure,
            .erased_callable,
            .ptr,
            => writerInvariant("tag discriminant read had non-tag-union layout"),
        };
    }

    fn tagPayloadLayout(self: *Writer, layout_idx: layout.Idx, discriminant: u32) layout.Idx {
        const layout_value = self.program.layouts.getLayout(layout_idx);
        if (layout_value.tag == .zst) return .zst;
        if (layout_value.tag != .tag_union) writerInvariant("tag payload read had non-tag-union layout");
        const data = self.program.layouts.getTagUnionData(layout_value.getTagUnion().idx);
        const variants = self.program.layouts.getTagUnionVariants(data);
        const index: usize = discriminant;
        if (index >= variants.len) writerInvariant("tag discriminant was outside variant layouts");
        return variants.get(@intCast(index)).payload_layout;
    }

    fn readBoxDataPointer(self: *Writer, value: Value) ?[*]u8 {
        const raw = self.readPointerSizedInt(value);
        if (raw == 0) return null;
        return @ptrFromInt(raw);
    }

    fn resolveTagBase(self: *Writer, layout_idx: layout.Idx, value: Value) TagBase {
        const layout_value = self.program.layouts.getLayout(layout_idx);
        return switch (layout_value.tag) {
            .zst, .tag_union => .{
                .value = value,
                .layout_idx = layout_idx,
            },
            .box => .{
                .value = .{
                    .ptr = self.readBoxDataPointer(value) orelse writerInvariant("boxed tag value had null payload pointer"),
                },
                .layout_idx = layout_value.getIdx(),
            },
            .scalar,
            .box_of_zst,
            .erased_box,
            .list,
            .list_of_zst,
            .struct_,
            .closure,
            .erased_callable,
            .ptr,
            => writerInvariant("tag value read had non-tag layout"),
        };
    }

    fn readErasedCallablePointer(self: *Writer, value: Value) [*]u8 {
        const ptr = self.readPointerSizedInt(value);
        if (ptr == 0) writerInvariant("erased callable result had null pointer");
        return @ptrFromInt(ptr);
    }

    fn readPointerSizedInt(self: *Writer, value: Value) usize {
        return if (self.program.layouts.targetUsize().size() == 8)
            value.read(usize)
        else
            @as(usize, value.read(u32));
    }

    fn memoAddress(
        self: *Writer,
        plan_id: LirProgram.ConstPlanId,
        layout_idx: layout.Idx,
        value: Value,
        storage: LirProgram.CaptureSlotStorage,
    ) ?RuntimeValueAddress {
        const layout_value = self.program.layouts.getLayout(layout_idx);
        const ptr: ?usize = switch (layout_value.tag) {
            .box => if (self.readBoxDataPointer(value)) |payload| @intFromPtr(payload) else null,
            .list => blk: {
                const roc_list: *const RocList = @ptrCast(@alignCast(value.ptr));
                break :blk if (roc_list.bytes) |bytes| @intFromPtr(bytes) else null;
            },
            .scalar => if (layout_value.getScalar().tag == .str) blk: {
                const roc_str: *const RocStr = @ptrCast(@alignCast(value.ptr));
                break :blk @intFromPtr(roc_str.asSlice().ptr);
            } else null,
            .box_of_zst,
            .erased_box,
            .list_of_zst,
            .struct_,
            .closure,
            .erased_callable,
            .zst,
            .tag_union,
            .ptr,
            => null,
        };
        return if (ptr) |raw| .{
            .ptr = raw,
            .len = switch (layout_value.tag) {
                .list => blk: {
                    const roc_list: *const RocList = @ptrCast(@alignCast(value.ptr));
                    break :blk roc_list.len();
                },
                .scalar => if (layout_value.getScalar().tag == .str) blk: {
                    const roc_str: *const RocStr = @ptrCast(@alignCast(value.ptr));
                    break :blk roc_str.len();
                } else 0,
                .box,
                .box_of_zst,
                .erased_box,
                .list_of_zst,
                .struct_,
                .closure,
                .erased_callable,
                .zst,
                .tag_union,
                .ptr,
                => 0,
            },
            .plan = @backingInt(plan_id),
            .layout = @backingInt(layout_idx),
            .storage = storage,
        } else null;
    }

    fn constPlan(self: *const Writer, id: LirProgram.ConstPlanId) LirProgram.ConstPlan {
        const raw = @backingInt(id);
        if (raw >= self.program.const_plans.items.len) writerInvariant("const plan id is out of range");
        return self.program.const_plans.items[raw];
    }
};

fn interpreterErasedCallable(_: ?*anyopaque, data_ptr: [*]u8) error{RuntimeError}!ErasedCallableResolution {
    return .{
        .proc = Interpreter.erasedCallableInterpreterProcId(data_ptr),
        .capture_ptr = Interpreter.erasedCallableInterpreterCaptureValuePtr(data_ptr),
    };
}

fn writePackedScalar(out: []u8, element: const_store.ConstPackedScalar, scalar: checked.ConstScalar) void {
    if (out.len != element.byteWidth()) writerInvariant("packed scalar output width disagreed with its encoding");
    switch (element) {
        .u8 => out[0] = packedScalarValue(.u8, scalar, "packed U8 list element had different scalar data"),
        .i8 => out[0] = @bitCast(packedScalarValue(.i8, scalar, "packed I8 list element had different scalar data")),
        .u16 => base.byte_encoding.writeIntLittle(u16, out, packedScalarValue(.u16, scalar, "packed U16 list element had different scalar data")),
        .i16 => base.byte_encoding.writeIntLittle(i16, out, packedScalarValue(.i16, scalar, "packed I16 list element had different scalar data")),
        .u32 => base.byte_encoding.writeIntLittle(u32, out, packedScalarValue(.u32, scalar, "packed U32 list element had different scalar data")),
        .i32 => base.byte_encoding.writeIntLittle(i32, out, packedScalarValue(.i32, scalar, "packed I32 list element had different scalar data")),
        .u64 => base.byte_encoding.writeIntLittle(u64, out, packedScalarValue(.u64, scalar, "packed U64 list element had different scalar data")),
        .i64 => base.byte_encoding.writeIntLittle(i64, out, packedScalarValue(.i64, scalar, "packed I64 list element had different scalar data")),
        .u128 => writePackedU128(out, packedScalarValue(.u128, scalar, "packed U128 list element had different scalar data")),
        .i128 => base.byte_encoding.writeIntLittle(i128, out, packedScalarValue(.i128, scalar, "packed I128 list element had different scalar data")),
        .f32 => base.byte_encoding.writeIntLittle(u32, out, packedScalarValue(.f32_bits, scalar, "packed F32 list element had different scalar data")),
        .f64 => base.byte_encoding.writeIntLittle(u64, out, packedScalarValue(.f64_bits, scalar, "packed F64 list element had different scalar data")),
        .dec => base.byte_encoding.writeIntLittle(i128, out, packedScalarValue(.dec_bits, scalar, "packed Dec list element had different scalar data")),
        .u8x16,
        .i8x16,
        .u16x8,
        .i16x8,
        .u32x4,
        .i32x4,
        .u64x2,
        .i64x2,
        => writePackedU128(out, packedScalarValue(.u128, scalar, "packed vector list element had different scalar data")),
    }
}

fn packedScalarValue(
    comptime tag: std.meta.Tag(checked.ConstScalar),
    scalar: checked.ConstScalar,
    comptime mismatch_message: []const u8,
) @FieldType(checked.ConstScalar, @tagName(tag)) {
    if (std.meta.activeTag(scalar) != tag) writerInvariant(mismatch_message);
    return @field(scalar, @tagName(tag));
}

fn writePackedU128(out: []u8, value: u128) void {
    base.byte_encoding.writeIntLittle(u128, out, value);
}

fn checkedU32(value: usize, comptime message: []const u8) u32 {
    if (value > std.math.maxInt(u32)) writerInvariant(message);
    return @intCast(value);
}

fn writerInvariant(comptime message: []const u8) noreturn {
    if (@import("builtin").mode == .debug) {
        base.invariant("ConstStore writer invariant violated: {s}", .{message});
    }
    unreachable;
}

test "const store writer declarations are referenced" {
    std.testing.refAllDecls(@This());
}

fn initTestArtifact(allocator: Allocator, module_env: *can.ModuleEnv) Allocator.Error!checked.CheckedModuleArtifact {
    var names = check.CanonicalNames.CanonicalNameStore.init(allocator);
    const module_name = try names.internModuleName("Test");
    return .{
        .key = .{},
        .canonical_names = names,
        .module_identity = .{
            .module_idx = 0,
            .module_name = module_name,
            .display_module_name = module_name,
            .kind = .package,
        },
        .checking_context_identity = .{},
        .module_env = .{ .checked_source = module_env },
        .exports = .{},
        .provides_requires = .{},
        .method_registry = .{},
        .static_dispatch_plans = .{},
        .resolved_value_refs = .{},
        .checked_procedure_templates = .{},
        .intrinsic_wrappers = .{},
        .top_level_procedure_bindings = .{},
        .root_requests = .{},
        .hosted_procs = .{},
        .platform_required_declarations = .{},
        .platform_required_bindings = .{},
        .interface_capabilities = .{},
        .compile_time_roots = .{},
        .top_level_values = .{},
        .hoisted_constants = .{},
        .const_templates = .{},
        .const_store = const_store.ConstStore.init(allocator),
    };
}

fn deinitTestArtifact(artifact: *checked.CheckedModuleArtifact, allocator: Allocator) void {
    artifact.const_templates.deinit(allocator);
    artifact.const_store.deinit();
    artifact.canonical_names.deinit();
}

fn testConstRoot(plan: LirProgram.ConstPlanId, ret_layout: layout.Idx) LirProgram.ConstRootPlan {
    return .{
        .root_order = 0,
        .owner = .first,
        .request = .{
            .order = 0,
            .module_idx = 0,
            .kind = .compile_time_constant,
            .source = undefined,
            .checked_type = undefined,
            .abi = .compile_time,
            .exposure = .private,
        },
        .proc = undefined,
        .ret_layout = ret_layout,
        .ret_type = undefined,
        .plan = plan,
    };
}

test "const store writer pointer memoization is scoped to one root" {
    const testing = std.testing;

    var module_env = try can.ModuleEnv.init(testing.allocator, "");
    defer module_env.deinit();

    var artifact = try initTestArtifact(testing.allocator, &module_env);
    defer deinitTestArtifact(&artifact, testing.allocator);

    var program = try LirProgram.Result.init(testing.allocator, .u64);
    defer program.deinit();
    const str_plan: LirProgram.ConstPlanId = @fromBackingInt(@intCast(program.const_plans.items.len));
    try program.const_plans.append(testing.allocator, .str);

    var writer = Writer.init(testing.allocator, &artifact, &program);
    defer writer.deinit();

    const root = testConstRoot(str_plan, .str);

    const first_bytes = "alpha root payload 000";
    const second_bytes = "omega root payload 111";
    comptime std.debug.assert(first_bytes.len == second_bytes.len);

    const bytes = try testing.allocator.dupe(u8, first_bytes);
    defer testing.allocator.free(bytes);

    var roc_str = RocStr{
        .bytes = bytes.ptr,
        .capacity_or_alloc_ptr = RocStr.encodeCapacity(bytes.len),
        .length = bytes.len,
    };

    const first = try writer.storeRoot(root, .{ .ptr = @ptrCast(&roc_str) });
    @memcpy(bytes, second_bytes);
    const second = try writer.storeRoot(root, .{ .ptr = @ptrCast(&roc_str) });

    try testing.expect(first.const_node != second.const_node);

    const first_value = artifact.const_store.get(first.const_node);
    const second_value = artifact.const_store.get(second.const_node);
    try testing.expect(first_value == .str);
    try testing.expect(second_value == .str);
    try testing.expectEqualStrings(first_bytes, artifact.const_store.strBytes(first_value.str));
    try testing.expectEqualStrings(second_bytes, artifact.const_store.strBytes(second_value.str));
}

test "const store writer stores every NaN as Roc's one NaN" {
    const testing = std.testing;

    var module_env = try can.ModuleEnv.init(testing.allocator, "");
    defer module_env.deinit();

    var artifact = try initTestArtifact(testing.allocator, &module_env);
    defer deinitTestArtifact(&artifact, testing.allocator);

    var program = try LirProgram.Result.init(testing.allocator, .u64);
    defer program.deinit();
    const scalar_plan: LirProgram.ConstPlanId = @fromBackingInt(@intCast(program.const_plans.items.len));
    try program.const_plans.append(testing.allocator, .scalar);

    var writer = Writer.init(testing.allocator, &artifact, &program);
    defer writer.deinit();

    var f64_payload: u64 = 0xfff9_2345_6789_abcd;
    const stored_f64 = try writer.storeRoot(testConstRoot(scalar_plan, .f64), .{ .ptr = @ptrCast(&f64_payload) });
    const f64_value = artifact.const_store.get(stored_f64.const_node);
    try testing.expect(f64_value == .scalar and f64_value.scalar == .f64_bits);
    try testing.expectEqual(builtins.float_bits.normalized_f64_nan_bits, f64_value.scalar.f64_bits);

    var f32_payload: u32 = 0xffc1_2345;
    const stored_f32 = try writer.storeRoot(testConstRoot(scalar_plan, .f32), .{ .ptr = @ptrCast(&f32_payload) });
    const f32_value = artifact.const_store.get(stored_f32.const_node);
    try testing.expect(f32_value == .scalar and f32_value.scalar == .f32_bits);
    try testing.expectEqual(builtins.float_bits.normalized_f32_nan_bits, f32_value.scalar.f32_bits);

    var finite: u64 = @bitCast(@as(f64, -2.5));
    const stored_finite = try writer.storeRoot(testConstRoot(scalar_plan, .f64), .{ .ptr = @ptrCast(&finite) });
    try testing.expectEqual(finite, artifact.const_store.get(stored_finite.const_node).scalar.f64_bits);
}

// Repro for https://github.com/roc-lang/roc/issues/10177
test "const store writer stores 20KB scalar lists as shared blob" {
    const testing = std.testing;

    var module_env = try can.ModuleEnv.init(testing.allocator, "");
    defer module_env.deinit();

    var artifact = try initTestArtifact(testing.allocator, &module_env);
    defer deinitTestArtifact(&artifact, testing.allocator);

    var program = try LirProgram.Result.init(testing.allocator, .u64);
    defer program.deinit();

    const str_plan: LirProgram.ConstPlanId = @fromBackingInt(@intCast(program.const_plans.items.len));
    try program.const_plans.append(testing.allocator, .str);
    const scalar_plan: LirProgram.ConstPlanId = @fromBackingInt(@intCast(program.const_plans.items.len));
    try program.const_plans.append(testing.allocator, .scalar);
    const list_plan: LirProgram.ConstPlanId = @fromBackingInt(@intCast(program.const_plans.items.len));
    try program.const_plans.append(testing.allocator, .{ .list = scalar_plan });
    const u8_list_layout = try program.layouts.insertLayout(layout.Layout.list(.u8));
    const u16_list_layout = try program.layouts.insertLayout(layout.Layout.list(.u16));

    var writer = Writer.init(testing.allocator, &artifact, &program);
    defer writer.deinit();

    const bytes = try testing.allocator.alloc(u8, 20 * 1024);
    defer testing.allocator.free(bytes);
    @memset(bytes, 'A');

    var roc_str = RocStr{
        .bytes = bytes.ptr,
        .capacity_or_alloc_ptr = RocStr.encodeCapacity(bytes.len),
        .length = bytes.len,
    };
    const stored_str = try writer.storeRoot(testConstRoot(str_plan, .str), .{ .ptr = @ptrCast(&roc_str) });

    var roc_list = RocList{
        .bytes = bytes.ptr,
        .length = bytes.len,
        .capacity_or_alloc_ptr = RocList.encodeCapacity(bytes.len),
    };
    const stored_list = try writer.storeRoot(testConstRoot(list_plan, u8_list_layout), .{ .ptr = @ptrCast(&roc_list) });

    var roc_u16_list = RocList{
        .bytes = bytes.ptr,
        .length = bytes.len / @sizeOf(u16),
        .capacity_or_alloc_ptr = RocList.encodeCapacity(bytes.len / @sizeOf(u16)),
    };
    const stored_u16_list = try writer.storeRoot(testConstRoot(list_plan, u16_list_layout), .{ .ptr = @ptrCast(&roc_u16_list) });

    const str_value = artifact.const_store.get(stored_str.const_node);
    try testing.expect(str_value == .str);
    const list_value = artifact.const_store.get(stored_list.const_node);
    try testing.expect(list_value == .list);
    try testing.expect(list_value.list == .packed_bytes);
    const scalar_bytes = list_value.list.packed_bytes;
    try testing.expectEqual(@as(u32, 20 * 1024), scalar_bytes.len);
    try testing.expectEqual(const_store.ConstPackedScalar.u8, scalar_bytes.element);
    try testing.expectEqual(str_value.str.data, scalar_bytes.bytes.data);
    try testing.expectEqualSlices(u8, bytes, artifact.const_store.blobBytes(scalar_bytes.bytes));

    const u16_scalar_bytes = artifact.const_store.get(stored_u16_list.const_node).list.packed_bytes;
    try testing.expectEqual(@as(u32, 10 * 1024), u16_scalar_bytes.len);
    try testing.expectEqual(const_store.ConstPackedScalar.u16, u16_scalar_bytes.element);

    // An empty list keeps the capacity it was evaluated with.
    var roc_empty_list = RocList{
        .bytes = bytes.ptr,
        .length = 0,
        .capacity_or_alloc_ptr = RocList.encodeCapacity(bytes.len),
    };
    const stored_empty_list = try writer.storeRoot(testConstRoot(list_plan, u8_list_layout), .{ .ptr = @ptrCast(&roc_empty_list) });
    try testing.expectEqual(@as(u64, bytes.len), artifact.const_store.get(stored_empty_list.const_node).list.empty);
    try testing.expectEqual(str_value.str.data, u16_scalar_bytes.bytes.data);
}
