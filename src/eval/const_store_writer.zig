//! Store LIR interpreter results as checked constants.

const std = @import("std");
const builtins = @import("builtins");
const check = @import("check");
const layout = @import("layout");
const lir = @import("lir");

const Interpreter = @import("interpreter.zig").Interpreter;
const Value = @import("value.zig").Value;

const Allocator = std.mem.Allocator;
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
};

/// Runtime erased-callable identity decoded into the LIR proc and capture data.
pub const ErasedCallableResolution = struct {
    proc: lir.LIR.LirProcSpecId,
    capture_ptr: [*]u8,
};

/// Resolves erased-callable runtime data for the active evaluator.
pub const ErasedCallableResolver = struct {
    context: ?*anyopaque = null,
    resolve: *const fn (?*anyopaque, [*]u8) ErasedCallableResolution = interpreterErasedCallable,
};

const TagBase = struct {
    value: Value,
    layout_idx: layout.Idx,
};

const StrBacking = struct {
    data: const_store.ConstStrDataId,
    len: usize,
};

/// Stores interpreted compile-time roots into a checked ConstStore.
pub const Writer = struct {
    allocator: Allocator,
    module: *checked.CheckedModuleArtifact,
    program: *const LirProgram.Result,
    stored_values: std.AutoHashMap(RuntimeValueAddress, checked.ConstNodeId),
    str_backings: std.AutoHashMap(usize, StrBacking),
    erased_callable_resolver: ErasedCallableResolver,

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
            .str_backings = std.AutoHashMap(usize, StrBacking).init(allocator),
            .erased_callable_resolver = .{},
        };
    }

    pub fn setErasedCallableResolver(self: *Writer, resolver: ErasedCallableResolver) void {
        self.erased_callable_resolver = resolver;
    }

    pub fn deinit(self: *Writer) void {
        self.str_backings.deinit();
        self.stored_values.deinit();
    }

    pub fn storeRoot(
        self: *Writer,
        root: LirProgram.ConstRootPlan,
        value: Value,
    ) Allocator.Error!checked.CompileTimeRootPayload {
        // Runtime addresses are only meaningful while storing the current
        // evaluated root. The interpreter drops each root after storage, so
        // later roots may reuse those addresses for unrelated values.
        self.stored_values.clearRetainingCapacity();
        self.str_backings.clearRetainingCapacity();

        const plan = self.constPlan(root.plan);
        try self.collectStrBackings(root.plan, root.ret_layout, value);
        return switch (root.request.kind) {
            .compile_time_constant => .{ .const_node = try self.storeValue(root.plan, root.ret_layout, value) },
            .compile_time_callable => switch (plan) {
                .fn_value => |set| .{ .fn_value = try self.storeFnValue(set, root.ret_layout, value) },
                .erased_fn => |set| .{ .fn_value = try self.storeErasedFn(set, value) },
                else => writerInvariant("compile-time callable root did not have a function const plan"),
            },
            else => writerInvariant("non compile-time root reached ConstStore writer"),
        };
    }

    fn storeValue(
        self: *Writer,
        plan_id: LirProgram.ConstPlanId,
        layout_idx: layout.Idx,
        value: Value,
    ) Allocator.Error!checked.ConstNodeId {
        if (self.memoAddress(plan_id, layout_idx, value)) |address| {
            if (self.stored_values.get(address)) |existing| return existing;
            const node = try self.storeValueFresh(plan_id, layout_idx, value);
            try self.stored_values.put(address, node);
            return node;
        }
        return try self.storeValueFresh(plan_id, layout_idx, value);
    }

    fn storeValueFresh(
        self: *Writer,
        plan_id: LirProgram.ConstPlanId,
        layout_idx: layout.Idx,
        value: Value,
    ) Allocator.Error!checked.ConstNodeId {
        return switch (self.constPlan(plan_id)) {
            .pending => writerInvariant("pending const plan reached ConstStore writer"),
            .zst => try self.module.const_store.append(.zst),
            .scalar => try self.module.const_store.append(.{ .scalar = self.storeScalar(layout_idx, value) }),
            .str => try self.storeStr(value),
            .list => |elem_plan| try self.storeList(elem_plan, layout_idx, value),
            .box => |elem_plan| try self.storeBox(elem_plan, layout_idx, value),
            .tuple => |items| try self.storeTuple(items, layout_idx, value),
            .record => |fields| try self.storeRecord(fields, layout_idx, value),
            .tag_union => |variants| try self.storeTag(variants, layout_idx, value),
            .named => |named| blk: {
                const backing = try self.storeValue(named.backing, layout_idx, value);
                break :blk try self.module.const_store.append(.{ .nominal = .{
                    .named_type = named.named_type,
                    .backing = backing,
                } });
            },
            .fn_value => |set| blk: {
                const fn_id = try self.storeFnValue(set, layout_idx, value);
                break :blk try self.module.const_store.append(.{ .fn_value = fn_id });
            },
            .erased_fn => |set| blk: {
                const fn_id = try self.storeErasedFn(set, value);
                break :blk try self.module.const_store.append(.{ .fn_value = fn_id });
            },
        };
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
            .frac => switch (scalar.getFrac()) {
                .f32 => .{ .f32_bits = @bitCast(value.read(f32)) },
                .f64 => .{ .f64_bits = @bitCast(value.read(f64)) },
                .dec => .{ .dec_bits = value.read(builtins.dec.RocDec).num },
            },
            .opaque_ptr => writerInvariant("opaque pointer scalar layout reached scalar const plan"),
        };
    }

    fn storeStr(self: *Writer, value: Value) Allocator.Error!checked.ConstNodeId {
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
                        return try self.module.const_store.append(.{ .str = .{
                            .data = backing.data,
                            .offset = checkedU32(slice_start, "string slice offset exceeds ConstStore limit"),
                            .len = len,
                        } });
                    }
                }
            } else if (roc_str.bytes) |bytes| {
                const address = @intFromPtr(bytes);
                const backing = self.str_backings.get(address) orelse try self.addStrBacking(address, slice);
                return try self.module.const_store.append(.{ .str = .{
                    .data = backing.data,
                    .offset = 0,
                    .len = len,
                } });
            }
        }

        const data = try self.module.const_store.addStrData(slice);
        return try self.module.const_store.append(.{ .str = .{
            .data = data,
            .offset = 0,
            .len = len,
        } });
    }

    fn storeList(
        self: *Writer,
        elem_plan: LirProgram.ConstPlanId,
        layout_idx: layout.Idx,
        value: Value,
    ) Allocator.Error!checked.ConstNodeId {
        const layout_value = self.program.layouts.getLayout(layout_idx);
        if (layout_value.tag != .list and layout_value.tag != .list_of_zst) {
            writerInvariant("list const plan had non-list layout");
        }
        const roc_list: *const RocList = @ptrCast(@alignCast(value.ptr));
        const nodes = try self.module.const_store.allocator.alloc(checked.ConstNodeId, roc_list.len());
        // `nodes` is owned here for its whole lifetime: the store copies from it (it
        // never frees inputs), so free on every path — build failure, append failure,
        // and success alike.
        defer self.module.const_store.allocator.free(nodes);
        if (layout_value.tag == .list_of_zst) {
            for (nodes) |*node| node.* = try self.storeValue(elem_plan, .zst, Value.zst);
        } else {
            const elem_layout = layout_value.getIdx();
            const elem_size: usize = self.program.layouts.layoutSize(self.program.layouts.getLayout(elem_layout));
            if (roc_list.bytes) |bytes| {
                for (nodes, 0..) |*node, index| {
                    node.* = try self.storeValue(elem_plan, elem_layout, .{ .ptr = bytes + index * elem_size });
                }
            } else if (roc_list.len() != 0) {
                writerInvariant("non-empty list had null element pointer");
            }
        }
        return try self.module.const_store.append(.{ .list = nodes });
    }

    fn storeBox(
        self: *Writer,
        elem_plan: LirProgram.ConstPlanId,
        layout_idx: layout.Idx,
        value: Value,
    ) Allocator.Error!checked.ConstNodeId {
        const layout_value = self.program.layouts.getLayout(layout_idx);
        const child = switch (layout_value.tag) {
            .box_of_zst => try self.storeValue(elem_plan, .zst, Value.zst),
            .box => blk: {
                const ptr = self.readBoxDataPointer(value) orelse writerInvariant("boxed value had null payload pointer");
                break :blk try self.storeValue(elem_plan, layout_value.getIdx(), .{ .ptr = ptr });
            },
            .erased_callable => try self.storeValue(elem_plan, layout_idx, value),
            else => writerInvariant("box const plan had incompatible layout"),
        };
        return try self.module.const_store.append(.{ .box = child });
    }

    fn storeTuple(
        self: *Writer,
        items: []const LirProgram.ConstPlanId,
        layout_idx: layout.Idx,
        value: Value,
    ) Allocator.Error!checked.ConstNodeId {
        const nodes = try self.storeStructChildren(items, layout_idx, value);
        defer self.module.const_store.allocator.free(nodes);
        return try self.module.const_store.append(.{ .tuple = nodes });
    }

    fn storeRecord(
        self: *Writer,
        fields: []const LirProgram.ConstPlanId,
        layout_idx: layout.Idx,
        value: Value,
    ) Allocator.Error!checked.ConstNodeId {
        const nodes = try self.storeStructChildren(fields, layout_idx, value);
        defer self.module.const_store.allocator.free(nodes);
        return try self.module.const_store.append(.{ .record = nodes });
    }

    fn storeStructChildren(
        self: *Writer,
        plans: []const LirProgram.ConstPlanId,
        layout_idx: layout.Idx,
        value: Value,
    ) Allocator.Error![]const checked.ConstNodeId {
        if (plans.len == 0) return try self.module.const_store.allocator.alloc(checked.ConstNodeId, 0);

        const layout_value = self.program.layouts.getLayout(layout_idx);
        if (layout_value.tag == .box) {
            const ptr = self.readBoxDataPointer(value) orelse writerInvariant("boxed struct value had null payload pointer");
            return try self.storeStructChildren(plans, layout_value.getIdx(), .{ .ptr = ptr });
        }

        const nodes = try self.module.const_store.allocator.alloc(checked.ConstNodeId, plans.len);
        errdefer self.module.const_store.allocator.free(nodes);
        if (layout_value.tag == .zst) {
            for (nodes, 0..) |*node, index| {
                node.* = try self.storeValue(plans[index], .zst, Value.zst);
            }
            return nodes;
        }
        if (layout_value.tag == .box_of_zst) {
            for (nodes, 0..) |*node, index| {
                node.* = try self.storeValue(plans[index], .zst, Value.zst);
            }
            return nodes;
        }
        if (layout_value.tag != .struct_) writerInvariant("struct const plan had non-struct layout");

        for (nodes, 0..) |*node, index| {
            const field_layout = self.program.layouts.getStructFieldLayoutByOriginalIndex(layout_value.getStruct().idx, @intCast(index));
            const offset = self.program.layouts.getStructFieldOffsetByOriginalIndex(layout_value.getStruct().idx, @intCast(index));
            node.* = try self.storeValue(plans[index], field_layout, value.offset(offset));
        }
        return nodes;
    }

    fn storeTag(
        self: *Writer,
        variants: []const LirProgram.ConstTagVariant,
        layout_idx: layout.Idx,
        value: Value,
    ) Allocator.Error!checked.ConstNodeId {
        const tag_base = self.resolveTagBase(layout_idx, value);
        const selected = self.selectTagVariant(variants, tag_base.layout_idx, tag_base.value);
        const payload_layout = self.tagPayloadLayout(tag_base.layout_idx, selected.discriminant);
        const payload_nodes = try self.storeTagPayloads(selected.payloads, payload_layout, tag_base.value);
        defer self.module.const_store.allocator.free(payload_nodes);
        const tag_name = try self.module.const_store.allocator.dupe(u8, selected.name);
        defer self.module.const_store.allocator.free(tag_name);
        return try self.module.const_store.append(.{ .tag = .{
            .tag_name = tag_name,
            .payloads = payload_nodes,
        } });
    }

    fn storeTagPayloads(
        self: *Writer,
        plans: []const LirProgram.ConstPlanId,
        payload_layout: layout.Idx,
        value: Value,
    ) Allocator.Error![]const checked.ConstNodeId {
        const nodes = try self.module.const_store.allocator.alloc(checked.ConstNodeId, plans.len);
        errdefer self.module.const_store.allocator.free(nodes);
        if (plans.len == 0) return nodes;
        if (plans.len == 1) {
            nodes[0] = try self.storeValue(plans[0], payload_layout, value);
            return nodes;
        }
        const layout_value = self.program.layouts.getLayout(payload_layout);
        if (layout_value.tag == .zst) {
            for (nodes, 0..) |*node, index| {
                node.* = try self.storeValue(plans[index], .zst, Value.zst);
            }
        } else if (layout_value.tag == .struct_) {
            for (nodes, 0..) |*node, index| {
                const field_layout = self.program.layouts.getStructFieldLayoutByOriginalIndex(layout_value.getStruct().idx, @intCast(index));
                const offset = self.program.layouts.getStructFieldOffsetByOriginalIndex(layout_value.getStruct().idx, @intCast(index));
                node.* = try self.storeValue(plans[index], field_layout, value.offset(offset));
            }
        } else {
            writerInvariant("multi-payload tag did not use a struct payload layout");
        }
        return nodes;
    }

    fn storeFnValue(
        self: *Writer,
        set_id: LirProgram.FnSetId,
        layout_idx: layout.Idx,
        value: Value,
    ) Allocator.Error!checked.ConstFnId {
        const set = self.program.fn_sets.items[@intFromEnum(set_id)];
        const tag_base = self.resolveTagBase(layout_idx, value);
        const variant = self.selectFnVariant(set, tag_base.layout_idx, tag_base.value);
        const captures = try self.storeCaptures(variant.captures, variant.payload_layout, tag_base.value);
        defer self.module.const_store.allocator.free(captures);
        return try self.module.const_store.appendFn(.{
            .fn_def = variant.template.fn_def,
            .source_fn_ty = variant.template.source_fn_ty,
            .source_fn_key = variant.template.source_fn_key,
            .captures = captures,
        });
    }

    fn storeErasedFn(
        self: *Writer,
        set_id: LirProgram.ErasedFnsId,
        value: Value,
    ) Allocator.Error!checked.ConstFnId {
        const set = self.program.erased_fns.items[@intFromEnum(set_id)];
        const data_ptr = self.readErasedCallablePointer(value);
        const resolved = self.erased_callable_resolver.resolve(self.erased_callable_resolver.context, data_ptr);
        for (set.entries) |entry| {
            if (entry.entry != resolved.proc) continue;
            const captures = try self.storeCaptures(entry.captures, entry.capture_layout, .{ .ptr = resolved.capture_ptr });
            defer self.module.const_store.allocator.free(captures);
            return try self.module.const_store.appendFn(.{
                .fn_def = entry.template.fn_def,
                .source_fn_ty = entry.template.source_fn_ty,
                .source_fn_key = entry.template.source_fn_key,
                .captures = captures,
            });
        }
        writerInvariant("erased callable result did not match an explicit erased function entry");
    }

    fn storeCaptures(
        self: *Writer,
        slots: []const LirProgram.CaptureSlot,
        payload_layout: layout.Idx,
        payload_value: Value,
    ) Allocator.Error![]const const_store.ConstCapture {
        const captures = try self.module.const_store.allocator.alloc(const_store.ConstCapture, slots.len);
        errdefer self.module.const_store.allocator.free(captures);
        if (slots.len == 0) return captures;

        const layout_value = self.program.layouts.getLayout(payload_layout);
        if (layout_value.tag == .zst) {
            for (slots, 0..) |slot, index| {
                captures[index] = .{
                    .id = slot.id,
                    .ty = try self.cloneCaptureType(slot.ty),
                    .value = try self.storeValue(slot.plan, .zst, Value.zst),
                };
            }
        } else if (layout_value.tag == .struct_) {
            for (slots, 0..) |slot, index| {
                const field_layout = self.program.layouts.getStructFieldLayoutByOriginalIndex(layout_value.getStruct().idx, slot.slot);
                const offset = self.program.layouts.getStructFieldOffsetByOriginalIndex(layout_value.getStruct().idx, slot.slot);
                captures[index] = .{
                    .id = slot.id,
                    .ty = try self.cloneCaptureType(slot.ty),
                    .value = try self.storeValue(slot.plan, field_layout, payload_value.offset(offset)),
                };
            }
        } else if (slots.len == 1) {
            captures[0] = .{
                .id = slots[0].id,
                .ty = try self.cloneCaptureType(slots[0].ty),
                .value = try self.storeValue(slots[0].plan, payload_layout, payload_value),
            };
        } else {
            writerInvariant("multi-capture function did not use a struct capture layout");
        }
        return captures;
    }

    fn cloneCaptureType(self: *Writer, ty: const_store.ConstTypeId) Allocator.Error!const_store.ConstTypeId {
        return self.module.const_store.type_store.cloneTypeFromTranslated(
            &self.program.const_types,
            &self.program.const_type_names,
            &self.module.canonical_names,
            ty,
        );
    }

    fn collectStrBackings(
        self: *Writer,
        plan_id: LirProgram.ConstPlanId,
        layout_idx: layout.Idx,
        value: Value,
    ) Allocator.Error!void {
        switch (self.constPlan(plan_id)) {
            .pending => writerInvariant("pending const plan reached string backing collection"),
            .zst,
            .scalar,
            => {},
            .str => try self.collectStrValue(value),
            .list => |elem_plan| try self.collectListStrBackings(elem_plan, layout_idx, value),
            .box => |elem_plan| try self.collectBoxStrBackings(elem_plan, layout_idx, value),
            .tuple => |items| try self.collectStructStrBackings(items, layout_idx, value),
            .record => |fields| try self.collectStructStrBackings(fields, layout_idx, value),
            .tag_union => |variants| try self.collectTagStrBackings(variants, layout_idx, value),
            .named => |named| try self.collectStrBackings(named.backing, layout_idx, value),
            .fn_value => |set| try self.collectFnValueStrBackings(set, layout_idx, value),
            .erased_fn => |set| try self.collectErasedFnStrBackings(set, value),
        }
    }

    fn collectStrValue(self: *Writer, value: Value) Allocator.Error!void {
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

    fn addStrBacking(self: *Writer, address: usize, bytes: []const u8) Allocator.Error!StrBacking {
        const data = try self.module.const_store.addStrData(bytes);
        const backing: StrBacking = .{
            .data = data,
            .len = bytes.len,
        };
        try self.str_backings.put(address, backing);
        return backing;
    }

    fn collectListStrBackings(
        self: *Writer,
        elem_plan: LirProgram.ConstPlanId,
        layout_idx: layout.Idx,
        value: Value,
    ) Allocator.Error!void {
        const layout_value = self.program.layouts.getLayout(layout_idx);
        if (layout_value.tag != .list and layout_value.tag != .list_of_zst) {
            writerInvariant("list const plan had non-list layout");
        }
        const roc_list: *const RocList = @ptrCast(@alignCast(value.ptr));
        if (layout_value.tag == .list_of_zst) {
            for (0..roc_list.len()) |_| try self.collectStrBackings(elem_plan, .zst, Value.zst);
            return;
        }
        const elem_layout = layout_value.getIdx();
        const elem_size: usize = self.program.layouts.layoutSize(self.program.layouts.getLayout(elem_layout));
        if (roc_list.bytes) |bytes| {
            for (0..roc_list.len()) |index| {
                try self.collectStrBackings(elem_plan, elem_layout, .{ .ptr = bytes + index * elem_size });
            }
        } else if (roc_list.len() != 0) {
            writerInvariant("non-empty list had null element pointer");
        }
    }

    fn collectBoxStrBackings(
        self: *Writer,
        elem_plan: LirProgram.ConstPlanId,
        layout_idx: layout.Idx,
        value: Value,
    ) Allocator.Error!void {
        const layout_value = self.program.layouts.getLayout(layout_idx);
        switch (layout_value.tag) {
            .box_of_zst => try self.collectStrBackings(elem_plan, .zst, Value.zst),
            .box => {
                const ptr = self.readBoxDataPointer(value) orelse writerInvariant("boxed value had null payload pointer");
                try self.collectStrBackings(elem_plan, layout_value.getIdx(), .{ .ptr = ptr });
            },
            .erased_callable => try self.collectStrBackings(elem_plan, layout_idx, value),
            else => writerInvariant("box const plan had incompatible layout"),
        }
    }

    fn collectStructStrBackings(
        self: *Writer,
        plans: []const LirProgram.ConstPlanId,
        layout_idx: layout.Idx,
        value: Value,
    ) Allocator.Error!void {
        if (plans.len == 0) return;
        const layout_value = self.program.layouts.getLayout(layout_idx);
        if (layout_value.tag == .zst) {
            for (plans) |plan| try self.collectStrBackings(plan, .zst, Value.zst);
            return;
        }
        if (layout_value.tag == .box_of_zst) {
            for (plans) |plan| try self.collectStrBackings(plan, .zst, Value.zst);
            return;
        }
        if (layout_value.tag == .box) {
            const ptr = self.readBoxDataPointer(value) orelse writerInvariant("boxed struct value had null payload pointer");
            try self.collectStructStrBackings(plans, layout_value.getIdx(), .{ .ptr = ptr });
            return;
        }
        if (layout_value.tag != .struct_) writerInvariant("struct const plan had non-struct layout");

        for (plans, 0..) |plan, index| {
            const field_layout = self.program.layouts.getStructFieldLayoutByOriginalIndex(layout_value.getStruct().idx, @intCast(index));
            const offset = self.program.layouts.getStructFieldOffsetByOriginalIndex(layout_value.getStruct().idx, @intCast(index));
            try self.collectStrBackings(plan, field_layout, value.offset(offset));
        }
    }

    fn collectTagStrBackings(
        self: *Writer,
        variants: []const LirProgram.ConstTagVariant,
        layout_idx: layout.Idx,
        value: Value,
    ) Allocator.Error!void {
        const tag_base = self.resolveTagBase(layout_idx, value);
        const selected = self.selectTagVariant(variants, tag_base.layout_idx, tag_base.value);
        const payload_layout = self.tagPayloadLayout(tag_base.layout_idx, selected.discriminant);
        try self.collectTagPayloadStrBackings(selected.payloads, payload_layout, tag_base.value);
    }

    fn collectTagPayloadStrBackings(
        self: *Writer,
        plans: []const LirProgram.ConstPlanId,
        payload_layout: layout.Idx,
        value: Value,
    ) Allocator.Error!void {
        if (plans.len == 0) return;
        if (plans.len == 1) {
            try self.collectStrBackings(plans[0], payload_layout, value);
            return;
        }
        const layout_value = self.program.layouts.getLayout(payload_layout);
        if (layout_value.tag == .zst) {
            for (plans) |plan| try self.collectStrBackings(plan, .zst, Value.zst);
        } else if (layout_value.tag == .struct_) {
            for (plans, 0..) |plan, index| {
                const field_layout = self.program.layouts.getStructFieldLayoutByOriginalIndex(layout_value.getStruct().idx, @intCast(index));
                const offset = self.program.layouts.getStructFieldOffsetByOriginalIndex(layout_value.getStruct().idx, @intCast(index));
                try self.collectStrBackings(plan, field_layout, value.offset(offset));
            }
        } else {
            writerInvariant("multi-payload tag did not use a struct payload layout");
        }
    }

    fn collectFnValueStrBackings(
        self: *Writer,
        set_id: LirProgram.FnSetId,
        layout_idx: layout.Idx,
        value: Value,
    ) Allocator.Error!void {
        const set = self.program.fn_sets.items[@intFromEnum(set_id)];
        const tag_base = self.resolveTagBase(layout_idx, value);
        const variant = self.selectFnVariant(set, tag_base.layout_idx, tag_base.value);
        try self.collectCaptureStrBackings(variant.captures, variant.payload_layout, tag_base.value);
    }

    fn collectErasedFnStrBackings(
        self: *Writer,
        set_id: LirProgram.ErasedFnsId,
        value: Value,
    ) Allocator.Error!void {
        const set = self.program.erased_fns.items[@intFromEnum(set_id)];
        const data_ptr = self.readErasedCallablePointer(value);
        const resolved = self.erased_callable_resolver.resolve(self.erased_callable_resolver.context, data_ptr);
        for (set.entries) |entry| {
            if (entry.entry != resolved.proc) continue;
            try self.collectCaptureStrBackings(entry.captures, entry.capture_layout, .{ .ptr = resolved.capture_ptr });
            return;
        }
        writerInvariant("erased callable result did not match an explicit erased function entry");
    }

    fn collectCaptureStrBackings(
        self: *Writer,
        slots: []const LirProgram.CaptureSlot,
        payload_layout: layout.Idx,
        payload_value: Value,
    ) Allocator.Error!void {
        if (slots.len == 0) return;
        const layout_value = self.program.layouts.getLayout(payload_layout);
        if (layout_value.tag == .zst) {
            for (slots) |slot| try self.collectStrBackings(slot.plan, .zst, Value.zst);
        } else if (layout_value.tag == .struct_) {
            for (slots) |slot| {
                const field_layout = self.program.layouts.getStructFieldLayoutByOriginalIndex(layout_value.getStruct().idx, slot.slot);
                const offset = self.program.layouts.getStructFieldOffsetByOriginalIndex(layout_value.getStruct().idx, slot.slot);
                try self.collectStrBackings(slot.plan, field_layout, payload_value.offset(offset));
            }
        } else if (slots.len == 1) {
            try self.collectStrBackings(slots[0].plan, payload_layout, payload_value);
        } else {
            writerInvariant("multi-capture function did not use a struct capture layout");
        }
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
            else => writerInvariant("tag discriminant read had non-tag-union layout"),
        };
    }

    fn tagPayloadLayout(self: *Writer, layout_idx: layout.Idx, discriminant: u16) layout.Idx {
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
            else => writerInvariant("tag value read had non-tag layout"),
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

    fn memoAddress(self: *Writer, plan_id: LirProgram.ConstPlanId, layout_idx: layout.Idx, value: Value) ?RuntimeValueAddress {
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
            else => null,
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
                else => 0,
            },
            .plan = @intFromEnum(plan_id),
            .layout = @intFromEnum(layout_idx),
        } else null;
    }

    fn constPlan(self: *const Writer, id: LirProgram.ConstPlanId) LirProgram.ConstPlan {
        const raw = @intFromEnum(id);
        if (raw >= self.program.const_plans.items.len) writerInvariant("const plan id is out of range");
        return self.program.const_plans.items[raw];
    }
};

fn interpreterErasedCallable(_: ?*anyopaque, data_ptr: [*]u8) ErasedCallableResolution {
    return .{
        .proc = Interpreter.erasedCallableInterpreterProcId(data_ptr),
        .capture_ptr = Interpreter.erasedCallableInterpreterCaptureValuePtr(data_ptr),
    };
}

fn checkedU32(value: usize, comptime message: []const u8) u32 {
    if (value > std.math.maxInt(u32)) writerInvariant(message);
    return @intCast(value);
}

fn writerInvariant(comptime message: []const u8) noreturn {
    if (@import("builtin").mode == .Debug) {
        std.debug.panic("ConstStore writer invariant violated: {s}", .{message});
    }
    unreachable;
}

test "const store writer declarations are referenced" {
    std.testing.refAllDecls(@This());
}

test "const store writer pointer memoization is scoped to one root" {
    const testing = std.testing;
    const can = @import("can");

    var names = check.CanonicalNames.CanonicalNameStore.init(testing.allocator);
    const module_name = try names.internModuleName("Test");

    var module_env = try can.ModuleEnv.init(testing.allocator, "");
    defer module_env.deinit();

    var artifact = checked.CheckedModuleArtifact{
        .key = .{},
        .canonical_names = names,
        .module_identity = .{
            .module_idx = 0,
            .module_name = module_name,
            .display_module_name = module_name,
            .qualified_module_name = module_name,
            .kind = .package,
        },
        .checking_context_identity = .{},
        .module_env = .{ .checked_source = &module_env },
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
        .const_templates = .{},
        .const_store = const_store.ConstStore.init(testing.allocator),
    };
    defer {
        artifact.const_templates.deinit(testing.allocator);
        artifact.const_store.deinit();
        artifact.canonical_names.deinit();
    }

    var program = try LirProgram.Result.init(testing.allocator, .u64);
    defer program.deinit();
    const str_plan: LirProgram.ConstPlanId = @enumFromInt(program.const_plans.items.len);
    try program.const_plans.append(testing.allocator, .str);

    var writer = Writer.init(testing.allocator, &artifact, &program);
    defer writer.deinit();

    const root = LirProgram.ConstRootPlan{
        .root_order = 0,
        .request = .{
            .order = 0,
            .module_idx = 0,
            .kind = .compile_time_constant,
            .source = undefined, // storeRoot only reads request.kind for this focused writer test.
            .checked_type = undefined, // storeRoot only reads request.kind for this focused writer test.
            .abi = .compile_time,
            .exposure = .private,
        },
        .proc = undefined, // storeRoot does not inspect the root procedure for stored values.
        .ret_layout = .str,
        .plan = str_plan,
    };

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
    try testing.expectEqual(.str, first_value);
    try testing.expectEqual(.str, second_value);
    try testing.expectEqualStrings(first_bytes, artifact.const_store.strBytes(first_value.str));
    try testing.expectEqualStrings(second_bytes, artifact.const_store.strBytes(second_value.str));
}
