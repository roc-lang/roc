//! Freeze one evaluated native LIR root into readonly data and relocations.
//!
//! This is a forward consumer of the root's producer-owned const/layout plans.
//! It never restores ConstStore values or evaluates a static initializer. The
//! evaluator keeps the source value and its allocations alive until freezeRoot
//! returns; the returned exports then own every reachable byte independently.

const std = @import("std");
const base = @import("base");
const compilerInvariant = base.invariant;
const builtins = @import("builtins");
const layout = @import("layout");
const lir = @import("lir");
const static_data = @import("static_data");
const Value = @import("value.zig").Value;
const frozen_graph = @import("frozen_graph.zig");
const Allocator = std.mem.Allocator;
const Program = lir.Program;
const Error = Allocator.Error || error{RuntimeError};
const word_size = @sizeOf(usize);

/// The evaluator supplies identities recorded when it emitted native code.
/// Native code addresses are never recovered by searching procedure names or
/// by reconstructing capture/drop choices from a runtime pointer.
pub const CallableResolution = struct {
    proc: lir.LIR.LirProcSpecId,
    capture_ptr: [*]u8,
};

/// Evaluator callback supplying exact callable procedure and capture identities.
/// Missing evaluator registry evidence propagates its terminal runtime error.
pub const CallableResolver = struct {
    context: ?*anyopaque = null,
    resolve: *const fn (?*anyopaque, [*]u8) error{RuntimeError}!CallableResolution = missingCallableResolver,
};

/// The first export is the requested slot; all data-symbol relocation indices
/// belong to this returned slice. Concatenating export slices must rebase those
/// indices. Procedure relocations retain identities in `program`.
/// Release the result with static_data.deinitStaticData.
pub fn freezeRoot(
    allocator: Allocator,
    program: *const Program.Result,
    slot: lir.LIR.StaticDataId,
    root: Program.ConstRootPlan,
    value: Value,
    callables: CallableResolver,
) Error![]static_data.StaticDataExport {
    return freezeRootIntoSlot(allocator, program, slot, root.shape(), value, callables, root.ret_layout);
}

/// Freeze a root whose slot may hold a pointer to the value rather than the
/// value itself. A constant folded into a recursive tag's payload lands in such
/// a slot: the payload position is a pointer, while the root's own procedure
/// returns the union unboxed. The value is the same either way, so the slot
/// gets a relocation to the frozen payload.
pub fn freezeRootIntoSlot(
    allocator: Allocator,
    program: *const Program.Result,
    slot: lir.LIR.StaticDataId,
    root: Program.RootShape,
    value: Value,
    callables: CallableResolver,
    slot_layout: layout.Idx,
) Error![]static_data.StaticDataExport {
    if (program.layouts.targetUsize().size() != word_size) {
        invariant("native root export requires host-width LIR");
    }
    var arena = base.SingleThreadArena.init(allocator);
    defer arena.deinit();
    var builder = Builder{
        .graph = .{ .allocator = arena.allocator(), .output = allocator, .program = program, .slot = slot },
        .callables = callables,
    };
    defer builder.graph.deinit();
    const root_symbol = try builder.graph.addNode(
        try Program.staticDataSymbolName(allocator, slot),
        builder.graph.size(slot_layout),
        builder.graph.alignment(slot_layout),
    );
    if (slot_layout == root.ret_layout) {
        try builder.enqueue(root.plan, root.ret_layout, value, .{ .symbol = root_symbol }, .value);
    } else {
        const slot_physical = program.layouts.getLayout(slot_layout);
        if (slot_physical.tag != .box or slot_physical.getIdx() != root.ret_layout) {
            invariant("evaluated root slot was neither the root's layout nor a box around it");
        }
        const payload_layout = root.ret_layout;
        const result = try builder.graph.reserveAllocation(.{
            .address = @intFromPtr(value.ptr),
            .plan = root.plan,
            .layout_idx = payload_layout,
            .count = 1,
            .kind = .value,
        }, builder.graph.size(payload_layout), builder.graph.alignment(payload_layout), program.layouts.layoutContainsRefcounted(program.layouts.getLayout(payload_layout)), null);
        try builder.graph.relocate(.{ .symbol = root_symbol }, result.dest);
        if (result.fresh) try builder.enqueue(root.plan, payload_layout, value, result.dest, .value);
    }
    var next: usize = 0;
    while (next < builder.jobs.items.len) : (next += 1) {
        const job = builder.jobs.items[next];
        try builder.visit(job);
    }
    return try builder.graph.finish();
}

/// A frozen root as one block (`frozen_graph.ImageGraph`), the stored image
/// of a value that is data alone, at the 64-bit layout of its type whatever
/// the width of the evaluation that left it in memory. The program is the
/// host's, at the host's width. The result's slices belong to `graph`,
/// which the caller deinitializes.
pub fn imageRoot(
    graph: *ImageGraph,
    root: Program.RootShape,
    value: Value,
) Error!ImageGraph.Result {
    // Compiler hosts are little-endian (as the checked cache's raw-byte
    // format requires), so an image's bytes are the evaluation's.
    if (comptime @import("builtin").cpu.arch.endian() != .little) @compileError("stored images require a little-endian compiler host");
    // An in-process evaluation runs at the host's pointer width; the image
    // is written at the 64-bit layout whatever that width is.
    if (std.debug.runtime_safety and graph.program.layouts.targetUsize().size() != word_size) invariant("an image's source evaluation is not at the host's width");
    var builder = ImageBuilder{ .graph = graph.*, .callables = .{} };
    defer graph.* = builder.graph;
    defer builder.jobs.deinit(builder.graph.allocator);
    const dest = try builder.graph.root(builder.graph.size(root.ret_layout), builder.graph.alignment(root.ret_layout));
    try builder.enqueue(root.plan, root.ret_layout, value, dest, .value);
    var next: usize = 0;
    while (next < builder.jobs.items.len) : (next += 1) try builder.visit(builder.jobs.items[next]);
    return try builder.graph.finish();
}

/// Write `value`, in memory at the host's width, as part `plan` at
/// `layout_idx` of an image `graph` is building, at `dest`: the copying pass
/// `imageRoot` runs over a root's parts.
pub fn imagePart(graph: *ImageGraph, plan: Program.ConstPlanId, layout_idx: layout.Idx, value: Value, dest: Destination) Error!void {
    var builder = ImageBuilder{ .graph = graph.*, .callables = .{} };
    defer graph.* = builder.graph;
    defer builder.jobs.deinit(builder.graph.allocator);
    try builder.enqueue(plan, layout_idx, value, dest, .value);
    var next: usize = 0;
    while (next < builder.jobs.items.len) : (next += 1) try builder.visit(builder.jobs.items[next]);
}

/// The image of `value`, evaluated at shape `source`, written at shape
/// `stored`: the shape of the type the value is stored under, whose tag
/// unions may hold fewer variants than the evaluation's
/// (`Program.ConstRootPlan.stored`). Each tag is written as the stored
/// union's variant of the same name, and each part at its stored layout; a
/// part whose two shapes are one is copied as `imageRoot` copies it. The
/// result's slices belong to `graph`, which the caller deinitializes.
pub fn imageRootAt(
    graph: *ImageGraph,
    source: Program.RootShape,
    stored: Program.RootShape,
    value: Value,
) Error!ImageGraph.Result {
    if (comptime @import("builtin").cpu.arch.endian() != .little) @compileError("stored images require a little-endian compiler host");
    if (std.debug.runtime_safety and graph.program.layouts.targetUsize().size() != word_size) invariant("an image's source evaluation is not at the host's width");
    var writer = StoredShapeWriter{ .graph = graph };
    defer writer.jobs.deinit(graph.allocator);
    const dest = try graph.root(graph.size(stored.ret_layout), graph.alignment(stored.ret_layout));
    try writer.push(source, stored, value, dest);
    var next: usize = 0;
    while (next < writer.jobs.items.len) : (next += 1) try writer.visit(writer.jobs.items[next]);
    return try graph.finish();
}

/// One part `imageRootAt` writes: the value at `source`, written at `stored`.
const StoredShapeJob = struct {
    source: Program.RootShape,
    stored: Program.RootShape,
    value: Value,
    dest: Destination,
};

/// Writes a value evaluated at one shape into an image at another shape of
/// the same type with narrower tag unions. Parts wait on an explicit queue,
/// so nesting never becomes native call depth.
const StoredShapeWriter = struct {
    graph: *ImageGraph,
    jobs: std.ArrayList(StoredShapeJob) = .empty,

    fn push(self: *StoredShapeWriter, source: Program.RootShape, stored: Program.RootShape, value: Value, dest: Destination) Allocator.Error!void {
        try self.jobs.append(self.graph.allocator, .{ .source = source, .stored = stored, .value = value, .dest = dest });
    }

    fn plan(self: *const StoredShapeWriter, id: Program.ConstPlanId) Program.ConstPlan {
        return self.graph.program.const_plans.items[@backingInt(id)];
    }

    fn visit(self: *StoredShapeWriter, job: StoredShapeJob) Error!void {
        if (job.source.plan == job.stored.plan and job.source.ret_layout == job.stored.ret_layout) {
            return imagePart(self.graph, job.stored.plan, job.stored.ret_layout, job.value, job.dest);
        }
        // A stored zero-sized part, such as a union narrowed to one tag
        // without payloads, has no bytes to write.
        if (self.graph.size(job.stored.ret_layout) == 0) return;
        const layouts = &self.graph.program.layouts;
        const stored_physical = layouts.getLayout(job.stored.ret_layout);
        switch (self.plan(job.stored.plan)) {
            .named => |stored_named| {
                const source_named = switch (self.plan(job.source.plan)) {
                    .named => |named| named,
                    .pending, .layout_only, .zst, .scalar, .str, .list, .box, .tuple, .record, .tag_union, .fn_value, .erased_fn, .boxy_fn => invariant("a stored named type's evaluated part was not named"),
                };
                try self.push(
                    .{ .plan = source_named.backing, .ret_layout = job.source.ret_layout },
                    .{ .plan = stored_named.backing, .ret_layout = job.stored.ret_layout },
                    job.value,
                    job.dest,
                );
            },
            .tuple, .record => |stored_children| {
                const source_children = switch (self.plan(job.source.plan)) {
                    .tuple, .record => |parts| parts,
                    .pending, .layout_only, .zst, .scalar, .str, .list, .box, .tag_union, .named, .fn_value, .erased_fn, .boxy_fn => invariant("a stored aggregate's evaluated part was not an aggregate"),
                };
                if (try self.boxedPart(job, stored_physical)) return;
                try self.children(source_children, job.source.ret_layout, stored_children, job.stored.ret_layout, job.value, job.dest, false);
            },
            .tag_union => |stored_variants| {
                const source_variants = switch (self.plan(job.source.plan)) {
                    .tag_union => |variants| variants,
                    .pending, .layout_only, .zst, .scalar, .str, .list, .box, .tuple, .record, .named, .fn_value, .erased_fn, .boxy_fn => invariant("a stored tag union's evaluated part was not a tag union"),
                };
                if (try self.boxedPart(job, stored_physical)) return;
                const source_discriminant = self.readDiscriminant(job.source.ret_layout, job.value);
                const source_variant = for (source_variants) |variant| {
                    if (variant.discriminant == source_discriminant) break variant;
                } else invariant("an evaluated tag did not match an explicit const variant");
                // The declared row a value inhabits holds each of its labels
                // once (`declaredRootType`).
                const stored_variant = for (stored_variants) |variant| {
                    if (variant.checked_name == source_variant.checked_name) break variant;
                } else invariant("an evaluated tag has no variant of its name in the type it is stored at");
                if (stored_physical.tag != .tag_union) invariant("a stored tag union of nonzero size did not have a tag union layout");
                const stored_data = layouts.getTagUnionData(stored_physical.getTagUnion().idx);
                stored_data.writeDiscriminant(self.graph.bytes(job.dest, self.graph.size(job.stored.ret_layout)).ptr, stored_variant.discriminant, self.graph.width());
                try self.children(
                    source_variant.payloads,
                    self.payloadLayout(job.source.ret_layout, source_discriminant),
                    stored_variant.payloads,
                    self.payloadLayout(job.stored.ret_layout, stored_variant.discriminant),
                    job.value,
                    job.dest,
                    true,
                );
            },
            .list => |stored_element| {
                const source_element = switch (self.plan(job.source.plan)) {
                    .list => |element| element,
                    .pending, .layout_only, .zst, .scalar, .str, .box, .tuple, .record, .tag_union, .named, .fn_value, .erased_fn, .boxy_fn => invariant("a stored list's evaluated part was not a list"),
                };
                try self.list(job, source_element, stored_element, stored_physical);
            },
            .box => |stored_element| {
                const source_element = switch (self.plan(job.source.plan)) {
                    .box => |element| element,
                    .pending, .layout_only, .zst, .scalar, .str, .list, .tuple, .record, .tag_union, .named, .fn_value, .erased_fn, .boxy_fn => invariant("a stored box's evaluated part was not a box"),
                };
                switch (stored_physical.tag) {
                    .box_of_zst => try self.graph.writeWord(job.dest, 0),
                    .box => try self.boxed(job, source_element, stored_element, stored_physical),
                    .erased_callable, .hole, .dynamic, .scalar, .list, .list_of_zst, .struct_, .closure, .zst, .tag_union, .ptr => invariant("a stored box did not have a box layout"),
                }
            },
            // Narrowing a type's rows changes only its tag unions, so a
            // scalar or string part keeps its layout.
            .scalar, .str => {
                if (job.source.ret_layout != job.stored.ret_layout) invariant("a stored scalar or string was evaluated at another layout");
                try imagePart(self.graph, job.stored.plan, job.stored.ret_layout, job.value, job.dest);
            },
            .zst => {},
            .pending, .layout_only => invariant("an incomplete const plan reached a stored image"),
            .fn_value, .erased_fn, .boxy_fn => invariant("a function value reached a stored image"),
        }
    }

    /// Write an aggregate or union that either layout holds behind a box,
    /// and report whether one did. A box around an aggregate or union is
    /// representation alone: the evaluated layout may box a part the stored
    /// layout holds inline, or the reverse, so the part is read through the
    /// evaluated box and written into a stored one.
    fn boxedPart(self: *StoredShapeWriter, job: StoredShapeJob, stored_physical: layout.Layout) Allocator.Error!bool {
        const source_physical = self.graph.program.layouts.getLayout(job.source.ret_layout);
        const source_boxed = switch (source_physical.tag) {
            .box => true,
            .box_of_zst => invariant("a part evaluated as a box of a zero-sized value is stored with bytes"),
            .erased_callable, .hole, .dynamic, .scalar, .list, .list_of_zst, .struct_, .closure, .zst, .tag_union, .ptr => false,
        };
        const stored_boxed = switch (stored_physical.tag) {
            .box => true,
            .box_of_zst => {
                try self.graph.writeWord(job.dest, 0);
                return true;
            },
            .erased_callable, .hole, .dynamic, .scalar, .list, .list_of_zst, .struct_, .closure, .zst, .tag_union, .ptr => false,
        };
        if (!source_boxed and !stored_boxed) return false;
        const source: Program.RootShape = if (source_boxed) .{ .plan = job.source.plan, .ret_layout = source_physical.getIdx() } else job.source;
        const value: Value = if (source_boxed) blk: {
            const address = job.value.read(usize);
            if (address == 0) invariant("nonzero-sized boxed root value had a null payload");
            break :blk .{ .ptr = @ptrFromInt(address) };
        } else job.value;
        if (!stored_boxed) {
            try self.push(source, job.stored, value, job.dest);
            return true;
        }
        const layouts = &self.graph.program.layouts;
        const stored_child = stored_physical.getIdx();
        const result = try self.graph.reserveAllocation(.{
            .address = @intFromPtr(value.ptr),
            .plan = job.stored.plan,
            .layout_idx = stored_child,
            .count = 1,
            .kind = .value,
        }, self.graph.size(stored_child), self.graph.alignment(stored_child), layouts.layoutContainsRefcounted(layouts.getLayout(stored_child)), null);
        try self.graph.relocate(job.dest, result.dest);
        if (result.fresh) try self.push(source, .{ .plan = job.stored.plan, .ret_layout = stored_child }, value, result.dest);
        return true;
    }

    /// Write the boxed payload of `job`, of plan `source_plan` where it was
    /// evaluated and `stored_plan` where it is stored.
    fn boxed(self: *StoredShapeWriter, job: StoredShapeJob, source_plan: Program.ConstPlanId, stored_plan: Program.ConstPlanId, stored_physical: layout.Layout) Allocator.Error!void {
        const layouts = &self.graph.program.layouts;
        const source_physical = layouts.getLayout(job.source.ret_layout);
        if (source_physical.tag != .box) invariant("a part stored boxed was evaluated unboxed");
        const address = job.value.read(usize);
        if (address == 0) invariant("nonzero-sized boxed root value had a null payload");
        const stored_child = stored_physical.getIdx();
        const result = try self.graph.reserveAllocation(.{
            .address = address,
            .plan = stored_plan,
            .layout_idx = stored_child,
            .count = 1,
            .kind = .value,
        }, self.graph.size(stored_child), self.graph.alignment(stored_child), layouts.layoutContainsRefcounted(layouts.getLayout(stored_child)), null);
        try self.graph.relocate(job.dest, result.dest);
        if (result.fresh) try self.push(
            .{ .plan = source_plan, .ret_layout = source_physical.getIdx() },
            .{ .plan = stored_plan, .ret_layout = stored_child },
            .{ .ptr = @ptrFromInt(address) },
            result.dest,
        );
    }

    fn list(self: *StoredShapeWriter, job: StoredShapeJob, source_element: Program.ConstPlanId, stored_element: Program.ConstPlanId, stored_physical: layout.Layout) Allocator.Error!void {
        if (stored_physical.tag != .list and stored_physical.tag != .list_of_zst) invariant("a stored list did not have a list layout");
        const layouts = &self.graph.program.layouts;
        const source_physical = layouts.getLayout(job.source.ret_layout);
        if (source_physical.tag != .list and source_physical.tag != .list_of_zst) invariant("an evaluated list did not have a list layout");
        const list_value = job.value.read(builtins.list.RocList);
        // `RocList`'s words: bytes, length, capacity, as `imageRoot` writes
        // them.
        try self.graph.writeWord(job.dest.offsetBy(self.graph.word()), list_value.len());
        const capacity = list_value.getCapacity();
        try self.graph.writeWord(job.dest.offsetBy(2 * self.graph.word()), builtins.list.RocList.encodeCapacityForWidth(capacity));
        if (stored_physical.tag == .list_of_zst or capacity == 0) return;
        if (source_physical.tag != .list) invariant("a list stored with sized items was evaluated with zero-sized items");
        const ptr = list_value.bytes orelse invariant("a native list with capacity had a null pointer");
        const source_item = source_physical.getIdx();
        const stored_item = stored_physical.getIdx();
        const source_size = layouts.layoutSize(layouts.getLayout(source_item));
        const stored_size = self.graph.size(stored_item);
        const result = try self.graph.reserveAllocation(.{
            .address = @intFromPtr(ptr),
            .plan = stored_element,
            .layout_idx = stored_item,
            .count = list_value.len(),
            .kind = .list,
        }, stored_size * list_value.len(), self.graph.alignment(stored_item), layouts.layoutContainsRefcounted(layouts.getLayout(stored_item)), list_value.len());
        try self.graph.relocate(job.dest, result.dest);
        if (!result.fresh) return;
        for (0..list_value.len()) |index| {
            try self.push(
                .{ .plan = source_element, .ret_layout = source_item },
                .{ .plan = stored_element, .ret_layout = stored_item },
                .{ .ptr = ptr + index * source_size },
                result.dest.offsetBy(index * stored_size),
            );
        }
    }

    /// Write an aggregate's or a tag payload's parts, each from its offset
    /// in the evaluated layout to its offset in the stored one.
    fn children(
        self: *StoredShapeWriter,
        source_plans: []const Program.ConstPlanId,
        source_layout: layout.Idx,
        stored_plans: []const Program.ConstPlanId,
        stored_layout: layout.Idx,
        value: Value,
        dest: Destination,
        tag_payload: bool,
    ) Allocator.Error!void {
        if (source_plans.len != stored_plans.len) invariant("a stored part has another count of children than its evaluated part");
        if (stored_plans.len == 0 or self.graph.size(stored_layout) == 0) return;
        if (tag_payload and stored_plans.len == 1) {
            return self.push(.{ .plan = source_plans[0], .ret_layout = source_layout }, .{ .plan = stored_plans[0], .ret_layout = stored_layout }, value, dest);
        }
        const layouts = &self.graph.program.layouts;
        const source_physical = layouts.getLayout(source_layout);
        const stored_physical = layouts.getLayout(stored_layout);
        if (source_physical.tag != .struct_ or stored_physical.tag != .struct_) invariant("an aggregate's parts did not have struct layouts");
        const source_struct = source_physical.getStruct().idx;
        const stored_struct = stored_physical.getStruct().idx;
        for (source_plans, stored_plans, 0..) |source_plan, stored_plan, original_index| {
            const index: u32 = @intCast(original_index);
            try self.push(
                .{ .plan = source_plan, .ret_layout = layouts.getStructFieldLayoutByOriginalIndex(source_struct, index) },
                .{ .plan = stored_plan, .ret_layout = layouts.getStructFieldLayoutByOriginalIndex(stored_struct, index) },
                value.offset(layouts.getStructFieldOffsetByOriginalIndex(source_struct, index)),
                dest.offsetBy(layouts.getStructFieldOffsetByOriginalIndexAt(stored_struct, index, self.graph.width())),
            );
        }
    }

    fn readDiscriminant(self: *const StoredShapeWriter, idx: layout.Idx, value: Value) u32 {
        const layouts = &self.graph.program.layouts;
        const physical = layouts.getLayout(idx);
        if (physical.tag == .zst) return 0;
        if (physical.tag != .tag_union) invariant("an evaluated tag did not have a tag union layout");
        const data = layouts.getTagUnionData(physical.getTagUnion().idx);
        return @intCast(data.readDiscriminant(value.ptr, layouts.targetUsize()));
    }

    fn payloadLayout(self: *const StoredShapeWriter, idx: layout.Idx, discriminant: u32) layout.Idx {
        const layouts = &self.graph.program.layouts;
        const physical = layouts.getLayout(idx);
        if (physical.tag == .zst) return .zst;
        const data = layouts.getTagUnionData(physical.getTagUnion().idx);
        const variants = layouts.getTagUnionVariants(data);
        if (discriminant >= variants.len) invariant("a tag discriminant exceeded its layout's variants");
        return variants.get(discriminant).payload_layout;
    }
};

/// The graph an image root is written into.
pub const ImageGraph = frozen_graph.ImageGraph(AllocationKey);
const ImageBuilder = BuilderOf(ImageGraph);

const Destination = frozen_graph.Destination;

const Job = struct {
    plan: Program.ConstPlanId,
    layout_idx: layout.Idx,
    source: Value,
    dest: Destination,
    storage: Program.CaptureSlotStorage,
};

/// A typed view is explicit in the producer's plan. Reserving its destination
/// before visiting children preserves sharing and closes recursive graphs.
const AllocationKey = struct {
    address: usize,
    plan: Program.ConstPlanId,
    layout_idx: layout.Idx,
    count: usize,
    kind: enum { value, list, string, erased },
};

/// The root freezer over graph `GraphT`: a `frozen_graph.Graph` of
/// separate symbols, or a `frozen_graph.ImageGraph` holding one block.
fn BuilderOf(comptime GraphT: type) type {
    return struct {
        const Self = @This();
        graph: GraphT,
        callables: CallableResolver,
        jobs: std.ArrayList(Job) = .empty,

        fn enqueue(self: *Self, plan: Program.ConstPlanId, idx: layout.Idx, source: Value, dest: Destination, storage: Program.CaptureSlotStorage) Allocator.Error!void {
            try self.jobs.append(self.graph.allocator, .{ .plan = plan, .layout_idx = idx, .source = source, .dest = dest, .storage = storage });
        }

        fn boxed(self: *Self, job: Job, child_plan: Program.ConstPlanId, child_layout: layout.Idx) Allocator.Error!void {
            const address = job.source.read(usize);
            if (address == 0) invariant("nonzero-sized boxed root value had a null payload");
            const child_value = Value{ .ptr = @ptrFromInt(address) };
            const result = try self.graph.reserveAllocation(.{
                .address = address,
                .plan = child_plan,
                .layout_idx = child_layout,
                .count = 1,
                .kind = .value,
            }, self.graph.size(child_layout), self.graph.alignment(child_layout), self.graph.program.layouts.layoutContainsRefcounted(self.graph.program.layouts.getLayout(child_layout)), null);
            try self.graph.relocate(job.dest, result.dest);
            if (result.fresh) try self.enqueue(child_plan, child_layout, child_value, result.dest, .value);
        }

        fn visit(self: *Self, job: Job) Error!void {
            const physical = self.graph.program.layouts.getLayout(job.layout_idx);
            if (job.storage == .recursive_box) {
                if (physical.tag != .box) invariant("recursive capture plan lacked box storage");
                return self.boxed(job, job.plan, physical.getIdx());
            }
            const plan = self.graph.program.const_plans.items[@backingInt(job.plan)];
            switch (plan) {
                .pending, .layout_only => invariant("incomplete const plan reached native root export"),
                .boxy_fn => invariant("a function value's root was published to a slot"),
                .zst => {},
                .scalar => self.scalars(job.layout_idx, job.source, job.dest, 1),
                .str => try self.string(job),
                .list => |element| try self.list(job, element),
                .box => |element| switch (physical.tag) {
                    .box_of_zst => try self.graph.writeWord(job.dest, 0),
                    .box => try self.boxed(job, element, physical.getIdx()),
                    .erased_callable => try self.enqueue(element, job.layout_idx, job.source, job.dest, .value),
                    .hole => invariant("a demand hole reached a native root export"),
                    .dynamic => invariant("a dynamic value reached a native root export"),
                    .scalar, .list, .list_of_zst, .struct_, .closure, .zst, .tag_union, .ptr => invariant("box export plan had incompatible layout"),
                },
                .tuple, .record => |child_plans| {
                    if (physical.tag == .box) return self.boxed(job, job.plan, physical.getIdx());
                    if (physical.tag == .box_of_zst) {
                        try self.graph.writeWord(job.dest, 0);
                        return;
                    }
                    try self.children(child_plans, job.layout_idx, job.source, job.dest, false);
                },
                .named => |named| try self.enqueue(named.backing, job.layout_idx, job.source, job.dest, .value),
                .tag_union => |variants| {
                    if (physical.tag == .box) return self.boxed(job, job.plan, physical.getIdx());
                    const discriminant = self.tagDiscriminant(job);
                    for (variants) |variant| {
                        if (variant.discriminant != discriminant) continue;
                        try self.children(variant.payloads, self.tagPayload(job.layout_idx, discriminant), job.source, job.dest, true);
                        return;
                    }
                    invariant("native tag did not match an explicit const variant");
                },
                .fn_value => |set_id| {
                    if (physical.tag == .box) return self.boxed(job, job.plan, physical.getIdx());
                    const set = self.graph.program.fn_sets.items[@backingInt(set_id)];
                    const discriminant = self.tagDiscriminant(job);
                    for (set.variants) |variant| {
                        if (variant.discriminant != discriminant) continue;
                        try self.captures(variant.captures, variant.payload_layout, job.source, job.dest);
                        return;
                    }
                    invariant("native callable did not match an explicit finite variant");
                },
                .erased_fn => |set_id| if (comptime @hasDecl(GraphT, "is_image"))
                    invariant("a function value reached a stored image")
                else
                    try self.erased(job, set_id),
            }
        }

        /// Copy `count` adjacent scalars of layout `idx` from `source` to `dest`.
        /// Frozen data is canonical: a NaN keeps whatever bits the evaluation
        /// produced until here, where it becomes Roc's one NaN.
        fn scalars(self: *Self, idx: layout.Idx, source: Value, dest: Destination, count: usize) void {
            const physical = self.graph.program.layouts.getLayout(idx);
            if (physical.tag != .scalar or physical.getScalar().tag == .opaque_ptr or physical.getScalar().tag == .str) invariant("scalar export plan did not name pointer-free scalar bytes");
            const byte_count = self.graph.size(idx) * count;
            const out = self.graph.bytes(dest, byte_count);
            @memcpy(out, source.readBytes(byte_count));
            if (idx == .f32) {
                for (0..count) |index| {
                    const bits = out[index * 4 ..][0..4];
                    std.mem.writeInt(u32, bits, builtins.float_bits.normalizeF32NanBits(std.mem.readInt(u32, bits, .little)), .little);
                }
            } else if (idx == .f64) {
                for (0..count) |index| {
                    const bits = out[index * 8 ..][0..8];
                    std.mem.writeInt(u64, bits, builtins.float_bits.normalizeF64NanBits(std.mem.readInt(u64, bits, .little)), .little);
                }
            }
        }

        /// Whether a value of `plan` is one scalar: a scalar plan, or a named
        /// type over one, which stores its backing's bytes.
        fn isScalarPlan(self: *const Self, plan: Program.ConstPlanId) bool {
            var current = plan;
            while (true) switch (self.graph.program.const_plans.items[@backingInt(current)]) {
                .scalar => return true,
                .named => |named| current = named.backing,
                .pending, .layout_only, .zst, .str, .list, .box, .tuple, .record, .tag_union, .fn_value, .erased_fn, .boxy_fn => return false,
            };
        }

        fn string(self: *Self, job: Job) Allocator.Error!void {
            const str = job.source.read(builtins.str.RocStr);
            const slice = str.asSlice();
            // The descriptor keeps the capacity the string was evaluated with;
            // the allocation holds the bytes alone. A string held inline has no
            // capacity of its own beyond its bytes, and a seamless slice's is its
            // window, so either is inline again whenever its bytes fit the
            // destination's descriptor.
            const capacity = if (str.isSmallStr()) slice.len else str.getCapacity();
            const descriptor_size = builtins.str.RocStr.word_count * self.graph.word();
            if (capacity < descriptor_size) {
                const descriptor = self.graph.bytes(job.dest, descriptor_size);
                @memcpy(descriptor[0..slice.len], slice);
                descriptor[descriptor_size - 1] = builtins.str.RocStr.smallStrFlagByte(slice.len);
                return;
            }
            const result = try self.graph.reserveAllocation(.{
                .address = @intFromPtr(slice.ptr),
                .plan = job.plan,
                .layout_idx = job.layout_idx,
                .count = slice.len,
                .kind = .string,
            }, slice.len, 1, false, null);
            if (result.fresh) @memcpy(self.graph.bytes(result.dest, slice.len), slice);
            try self.graph.relocate(job.dest, result.dest);
            // `RocStr`'s words: bytes, capacity, length.
            try self.graph.writeWord(job.dest.offsetBy(self.graph.word()), builtins.str.RocStr.encodeCapacityForWidth(capacity));
            try self.graph.writeWord(job.dest.offsetBy(2 * self.graph.word()), slice.len);
        }

        fn list(self: *Self, job: Job, element: Program.ConstPlanId) Allocator.Error!void {
            const physical = self.graph.program.layouts.getLayout(job.layout_idx);
            if (physical.tag != .list and physical.tag != .list_of_zst) invariant("list export plan had non-list layout");
            const list_value = job.source.read(builtins.list.RocList);
            // `RocList`'s words: bytes, length, capacity.
            try self.graph.writeWord(job.dest.offsetBy(self.graph.word()), list_value.len());
            // The descriptor keeps the capacity the list was evaluated with; the
            // allocation holds the items alone. A list of zero-sized items holds
            // no allocation, so its capacity is a count alone.
            const capacity = list_value.getCapacity();
            try self.graph.writeWord(job.dest.offsetBy(2 * self.graph.word()), builtins.list.RocList.encodeCapacityForWidth(capacity));
            if (physical.tag == .list_of_zst or capacity == 0) return;
            const ptr = list_value.bytes orelse invariant("a native list with capacity had a null pointer");
            const element_layout = physical.getIdx();
            // An item's size where it was evaluated and where it is written:
            // they differ for an image written from a 32-bit evaluation.
            const source_size = self.graph.program.layouts.layoutSize(self.graph.program.layouts.getLayout(element_layout));
            const element_size = self.graph.size(element_layout);
            const result = try self.graph.reserveAllocation(.{
                .address = @intFromPtr(ptr),
                .plan = element,
                .layout_idx = element_layout,
                .count = list_value.len(),
                .kind = .list,
            }, element_size * list_value.len(), self.graph.alignment(element_layout), self.graph.program.layouts.layoutContainsRefcounted(self.graph.program.layouts.getLayout(element_layout)), list_value.len());
            try self.graph.relocate(job.dest, result.dest);
            if (!result.fresh) return;
            // A list of scalars holds no pointer and nothing to visit: its items
            // are one run of bytes.
            if (self.isScalarPlan(element)) return self.scalars(element_layout, .{ .ptr = ptr }, result.dest, list_value.len());
            for (0..list_value.len()) |index| {
                try self.enqueue(element, element_layout, .{ .ptr = ptr + index * source_size }, result.dest.offsetBy(index * element_size), .value);
            }
        }

        fn children(self: *Self, plans: []const Program.ConstPlanId, idx: layout.Idx, source: Value, dest: Destination, tag_payload: bool) Allocator.Error!void {
            if (plans.len == 0 or self.graph.size(idx) == 0) return;
            if (tag_payload and plans.len == 1) return self.enqueue(plans[0], idx, source, dest, .value);
            const physical = self.graph.program.layouts.getLayout(idx);
            if (physical.tag != .struct_) invariant("aggregate export plan had non-struct layout");
            const layouts = &self.graph.program.layouts;
            for (plans, 0..) |plan, original_index| {
                const child_layout = layouts.getStructFieldLayoutByOriginalIndex(physical.getStruct().idx, @intCast(original_index));
                const source_offset = layouts.getStructFieldOffsetByOriginalIndex(physical.getStruct().idx, @intCast(original_index));
                const dest_offset = layouts.getStructFieldOffsetByOriginalIndexAt(physical.getStruct().idx, @intCast(original_index), self.graph.width());
                try self.enqueue(plan, child_layout, source.offset(source_offset), dest.offsetBy(dest_offset), .value);
            }
        }

        fn tagDiscriminant(self: *Self, job: Job) u32 {
            const physical = self.graph.program.layouts.getLayout(job.layout_idx);
            if (physical.tag == .zst) return 0;
            if (physical.tag != .tag_union) invariant("native tag export had non-tag layout");
            const data = self.graph.program.layouts.getTagUnionData(physical.getTagUnion().idx);
            const discriminant = data.readDiscriminant(job.source.ptr, self.graph.program.layouts.targetUsize());
            data.writeDiscriminant(self.graph.bytes(job.dest, self.graph.size(job.layout_idx)).ptr, discriminant, self.graph.width());
            return @intCast(discriminant);
        }

        fn tagPayload(self: *Self, idx: layout.Idx, discriminant: u32) layout.Idx {
            const physical = self.graph.program.layouts.getLayout(idx);
            if (physical.tag == .zst) return .zst;
            const data = self.graph.program.layouts.getTagUnionData(physical.getTagUnion().idx);
            const variants = self.graph.program.layouts.getTagUnionVariants(data);
            if (discriminant >= variants.len) invariant("native tag discriminant exceeded layout variants");
            return variants.get(discriminant).payload_layout;
        }

        fn captures(self: *Self, slots: []const Program.CaptureSlot, idx: layout.Idx, source: Value, dest: Destination) Allocator.Error!void {
            if (slots.len == 0 or self.graph.size(idx) == 0) return;
            const physical = self.graph.program.layouts.getLayout(idx);
            if (physical.tag == .struct_) {
                for (slots) |slot| {
                    const child_layout = self.graph.program.layouts.getStructFieldLayoutByOriginalIndex(physical.getStruct().idx, slot.slot);
                    const offset = self.graph.program.layouts.getStructFieldOffsetByOriginalIndex(physical.getStruct().idx, slot.slot);
                    try self.enqueue(slot.plan, child_layout, source.offset(offset), dest.offsetBy(offset), slot.storage);
                }
            } else if (slots.len == 1) {
                try self.enqueue(slots[0].plan, idx, source, dest, slots[0].storage);
            } else invariant("native callable capture plan had non-struct multiple captures");
        }

        fn erased(self: *Self, job: Job, set_id: Program.ErasedFnsId) Error!void {
            const address = job.source.read(usize);
            if (address == 0) invariant("native erased callable had a null payload");
            const resolved = try self.callables.resolve(self.callables.context, @ptrFromInt(address));
            const set = self.graph.program.erased_fns.items[@backingInt(set_id)];
            for (set.entries) |entry| {
                if (entry.entry != resolved.proc) continue;
                // The program's erased capture prefix stays zeroed: a static value
                // is never dropped, so nothing reads the header it reserves.
                const prefix = self.graph.program.erased_capture_prefix;
                const capture_offset = builtins.erased_callable.capture_offset + prefix;
                const capture_size = prefix + self.graph.size(entry.capture_layout);
                const allocation_size = builtins.erased_callable.payloadSize(capture_size);
                const result = try self.graph.reserveAllocation(.{
                    .address = address,
                    .plan = job.plan,
                    .layout_idx = job.layout_idx,
                    .count = 1,
                    .kind = .erased,
                }, allocation_size, builtins.erased_callable.payload_alignment, builtins.erased_callable.allocation_has_refcounted_children, null);
                try self.graph.relocate(job.dest, result.dest);
                if (!result.fresh) return;
                const proc_name = try static_data.procSymbolName(self.graph.allocator, self.graph.program.store.getProcSpecImmediate(resolved.proc).identity);
                try self.graph.node(result.dest).relocations.append(self.graph.allocator, .{
                    .offset = result.dest.offset,
                    .target_symbol_name = proc_name,
                    .kind = .function_pointer,
                    .callable_capture_offset = .some(capture_offset),
                    .procedure = .some(resolved.proc),
                });
                const on_drop: ?layout.RcHelperKey = switch (entry.on_drop) {
                    .none => null,
                    .rc_helper => |helper| helper,
                };
                if (on_drop) |helper| {
                    try self.graph.node(result.dest).relocations.append(self.graph.allocator, .{
                        .offset = result.dest.offset + word_size,
                        .target_symbol_name = try static_data.atomicRcHelperSymbolName(self.graph.allocator, &self.graph.program.layouts, helper),
                        .kind = .function_pointer,
                        .rc_helper = helper,
                    });
                    const copy_helper = layout.RcHelperKey{ .op = .incref, .layout_idx = helper.layout_idx };
                    try self.graph.node(result.dest).relocations.append(self.graph.allocator, .{
                        .offset = result.dest.offset + 2 * word_size,
                        .target_symbol_name = try static_data.atomicRcHelperSymbolName(self.graph.allocator, &self.graph.program.layouts, copy_helper),
                        .kind = .function_pointer,
                        .rc_helper = copy_helper,
                    });
                }
                try self.graph.writeWord(result.dest.offsetBy(3 * word_size), capture_size);
                try self.captures(entry.captures, entry.capture_layout, .{ .ptr = resolved.capture_ptr }, result.dest.offsetBy(capture_offset));
                return;
            }
            invariant("native erased callable did not match an explicit entry");
        }
    };
}

const Builder = BuilderOf(frozen_graph.Graph(AllocationKey));

fn missingCallableResolver(_: ?*anyopaque, _: [*]u8) error{RuntimeError}!CallableResolution {
    invariant("native root exporter requires explicit erased callable identities");
}

fn invariant(comptime message: []const u8) noreturn {
    compilerInvariant("native root export invariant violated: {s}", .{message});
}

fn testSlot(program: *Program.Result, idx: layout.Idx) Allocator.Error!lir.LIR.StaticDataId {
    const id: lir.LIR.StaticDataId = @fromBackingInt(@intCast(program.static_data_values.items.len));
    try program.static_data_values.append(program.store.allocator, .{ .initializer = .none, .layout_idx = idx });
    return id;
}

// Exporting consumes only the root plan/layout; request and solved-type metadata are not read.
fn testRoot(plan: Program.ConstPlanId, ret_layout: layout.Idx) Program.ConstRootPlan {
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

test "native root export keeps an empty list's capacity behind an allocation holding no items" {
    const allocator = std.testing.allocator;
    var program = try Program.Result.init(allocator, @import("base").target.TargetUsize.native);
    defer program.deinit();
    const elem_plan: Program.ConstPlanId = @fromBackingInt(@intCast(program.const_plans.items.len));
    try program.const_plans.append(allocator, .scalar);
    const list_plan: Program.ConstPlanId = @fromBackingInt(@intCast(program.const_plans.items.len));
    try program.const_plans.append(allocator, .{ .list = elem_plan });
    const list_layout = try program.layouts.insertList(.u32);
    var backing: [16]u32 = undefined;
    var list_value = builtins.list.RocList{
        .bytes = @ptrCast(&backing),
        .length = 0,
        .capacity_or_alloc_ptr = builtins.list.RocList.encodeCapacity(backing.len),
    };
    const slot = try testSlot(&program, list_layout);
    const exports = try freezeRoot(allocator, &program, slot, testRoot(list_plan, list_layout), .{ .ptr = @ptrCast(&list_value) }, .{});
    defer static_data.deinitStaticData(allocator, exports);
    // The descriptor points just past a static refcount word and records the
    // sixteen items the list had room for.
    try std.testing.expectEqual(@as(usize, 2), exports.len);
    try std.testing.expectEqual(@as(usize, 1), exports[0].relocations.len);
    const frozen: *const builtins.list.RocList = @ptrCast(@alignCast(exports[0].bytes.ptr));
    try std.testing.expectEqual(@as(usize, 0), frozen.length);
    try std.testing.expectEqual(builtins.list.RocList.encodeCapacity(backing.len), frozen.capacity_or_alloc_ptr);
    try std.testing.expectEqual(@as(i64, word_size), exports[0].relocations[0].addend);
    try std.testing.expectEqualSlices(u8, &@as([word_size]u8, @splat(0)), exports[1].bytes);
}

test "native root export owns list strings and preserves shared typed pointers" {
    const allocator = std.testing.allocator;
    var program = try Program.Result.init(allocator, @import("base").target.TargetUsize.native);
    defer program.deinit();
    const str_plan: Program.ConstPlanId = @fromBackingInt(@intCast(program.const_plans.items.len));
    try program.const_plans.append(allocator, .str);
    const list_plan: Program.ConstPlanId = @fromBackingInt(@intCast(program.const_plans.items.len));
    try program.const_plans.append(allocator, .{ .list = str_plan });
    const list_layout = try program.layouts.insertList(.str);
    const text = "a native compile-time string exceeding inline capacity";
    const backing = try allocator.dupe(u8, text);
    defer allocator.free(backing);
    const str = builtins.str.RocStr{
        .bytes = backing.ptr,
        .length = backing.len,
        .capacity_or_alloc_ptr = builtins.str.RocStr.encodeCapacity(backing.len),
    };
    var items = [_]builtins.str.RocStr{ str, str };
    var list_value = builtins.list.RocList{
        .bytes = @ptrCast(&items),
        .length = items.len,
        .capacity_or_alloc_ptr = builtins.list.RocList.encodeCapacity(items.len),
    };
    const slot = try testSlot(&program, list_layout);
    const exports = try freezeRoot(allocator, &program, slot, testRoot(list_plan, list_layout), .{ .ptr = @ptrCast(&list_value) }, .{});
    defer static_data.deinitStaticData(allocator, exports);
    @memset(backing, 'x');
    try std.testing.expectEqual(@as(usize, 3), exports.len);
    try std.testing.expectEqual(slot, exports[0].value_id.?);
    try std.testing.expectEqual(@as(usize, 1), exports[0].relocations.len);
    const elements_relocation = exports[0].relocations[0];
    const elements = exports[@backingInt(elements_relocation.target.data_symbol)];
    try std.testing.expectEqual(@as(usize, 2), elements.relocations.len);
    const count_header = elements.bytes[@intCast(elements_relocation.addend - 2 * word_size)..][0..word_size];
    try std.testing.expectEqual(@as(usize, 2), std.mem.readInt(usize, count_header[0..word_size], .little));
    try std.testing.expectEqual(elements.relocations[0].target.data_symbol, elements.relocations[1].target.data_symbol);
    const first_string_offset: usize = @intCast(elements_relocation.addend);
    const frozen_string = std.mem.bytesAsValue(builtins.str.RocStr, elements.bytes[first_string_offset..][0..@sizeOf(builtins.str.RocStr)]).*;
    try std.testing.expectEqual(text.len, frozen_string.length);
    try std.testing.expectEqual(builtins.str.RocStr.encodeCapacity(text.len), frozen_string.capacity_or_alloc_ptr);
    const string_relocation = elements.relocations[0];
    const string_bytes = exports[@backingInt(string_relocation.target.data_symbol)];
    try std.testing.expectEqualStrings(text, string_bytes.bytes[@intCast(string_relocation.addend)..]);
    const header = string_bytes.bytes[@intCast(string_relocation.addend - word_size)..][0..word_size];
    try std.testing.expectEqual(@as(usize, 0), std.mem.readInt(usize, header[0..word_size], .little));
}

test "native root export removes seamless-slice native pointers" {
    const allocator = std.testing.allocator;
    var program = try Program.Result.init(allocator, @import("base").target.TargetUsize.native);
    defer program.deinit();
    const plan: Program.ConstPlanId = @fromBackingInt(@intCast(program.const_plans.items.len));
    try program.const_plans.append(allocator, .str);
    var backing = "prefix:the native slice is longer than the inline string representation:suffix".*;
    const expected = backing[7 .. backing.len - 7];
    var str = builtins.str.RocStr{
        .bytes = expected.ptr,
        .length = expected.len,
        .capacity_or_alloc_ptr = builtins.str.RocStr.encodeSliceAllocationPtr(&backing),
    };
    const exports = try freezeRoot(allocator, &program, try testSlot(&program, .str), testRoot(plan, .str), .{ .ptr = @ptrCast(&str) }, .{});
    defer static_data.deinitStaticData(allocator, exports);
    const root = exports[0];
    try std.testing.expectEqual(@as(usize, 1), root.relocations.len);
    try std.testing.expectEqual(@as(usize, 0), std.mem.readInt(usize, root.bytes[0..word_size], .little));
    const frozen_string = std.mem.bytesAsValue(builtins.str.RocStr, root.bytes[0..@sizeOf(builtins.str.RocStr)]).*;
    try std.testing.expectEqual(expected.len, frozen_string.length);
    try std.testing.expectEqual(builtins.str.RocStr.encodeCapacity(expected.len), frozen_string.capacity_or_alloc_ptr);
    const relocation = root.relocations[0];
    const bytes_ = exports[@backingInt(relocation.target.data_symbol)].bytes;
    try std.testing.expectEqualStrings(expected, bytes_[@intCast(relocation.addend)..]);
}

// Native graph export does not inspect the source template/type metadata.
fn testTemplate() Program.FnTemplate {
    return .{ .fn_def = undefined, .source_fn_ty = undefined, .source_fn_key = undefined };
}

// Only slot, plan and storage are read by these synthetic capture export tests.
fn testCapture(plan: Program.ConstPlanId, storage: Program.CaptureSlotStorage) Program.CaptureSlot {
    return .{ .id = undefined, .kind = .lexical, .slot = 0, .ty = undefined, .plan = plan, .storage = storage };
}

test "native root export closes recursive finite callable capture graphs" {
    const allocator = std.testing.allocator;
    var program = try Program.Result.init(allocator, @import("base").target.TargetUsize.native);
    defer program.deinit();
    const plan: Program.ConstPlanId = @fromBackingInt(@intCast(program.const_plans.items.len));
    const set: Program.FnSetId = @fromBackingInt(@intCast(program.fn_sets.items.len));
    try program.const_plans.append(allocator, .{ .fn_value = set });
    // A recursive function captures itself through the producer's recursive_box
    // capture slot. The tag payload is that box, with no explicit discriminant.
    const box_layout = try program.layouts.reserveLayout(layout.Layout.box(.zst));
    const fn_layout = try program.layouts.putTagUnion(&.{box_layout});
    program.layouts.updateLayout(box_layout, layout.Layout.box(fn_layout));
    const captures = try allocator.dupe(Program.CaptureSlot, &.{testCapture(plan, .recursive_box)});
    const variants = try allocator.dupe(Program.FnVariant, &.{.{
        .id = undefined,
        .discriminant = 0,
        .variant_index = 0,
        .payload_layout = box_layout,
        .template = testTemplate(),
        .captures = captures,
    }});
    // Fill every variant identity from its allocated slice index before publication.
    for (variants, 0..) |*variant, index| variant.id = @fromBackingInt(@intCast(index));
    try program.fn_sets.append(allocator, .{ .layout = fn_layout, .variants = variants });
    var recursive_capture: usize = undefined;
    recursive_capture = @intFromPtr(&recursive_capture);
    const exports = try freezeRoot(allocator, &program, try testSlot(&program, fn_layout), testRoot(plan, fn_layout), .{ .ptr = @ptrCast(&recursive_capture) }, .{});
    defer static_data.deinitStaticData(allocator, exports);
    try std.testing.expectEqual(@as(usize, 2), exports.len);
    const root_pointer = exports[0].relocations[0];
    const capture_export = exports[@backingInt(root_pointer.target.data_symbol)];
    try std.testing.expectEqual(@as(usize, 1), capture_export.relocations.len);
    const self_pointer = capture_export.relocations[0];
    try std.testing.expectEqual(root_pointer.target.data_symbol, self_pointer.target.data_symbol);
    try std.testing.expectEqual(root_pointer.addend, self_pointer.addend);
    try std.testing.expectEqual(@as(usize, 0), std.mem.readInt(usize, capture_export.bytes[@intCast(root_pointer.addend - word_size)..][0..word_size], .little));
}

test "native root export selects explicit finite callable tag and captured string" {
    const allocator = std.testing.allocator;
    var program = try Program.Result.init(allocator, @import("base").target.TargetUsize.native);
    defer program.deinit();
    const str_plan: Program.ConstPlanId = @fromBackingInt(@intCast(program.const_plans.items.len));
    const fn_layout = try program.layouts.putTagUnion(&.{ .zst, .str });
    try program.const_plans.append(allocator, .str);
    const fn_plan: Program.ConstPlanId = @fromBackingInt(@intCast(program.const_plans.items.len));
    try program.const_plans.append(allocator, .{ .fn_value = @fromBackingInt(@intCast(program.fn_sets.items.len)) });
    const captures = try allocator.dupe(Program.CaptureSlot, &.{testCapture(str_plan, .value)});
    const variants = try allocator.dupe(Program.FnVariant, &.{
        .{ .id = undefined, .discriminant = 0, .variant_index = 0, .payload_layout = .zst, .template = testTemplate() },
        .{ .id = undefined, .discriminant = 1, .variant_index = 1, .payload_layout = .str, .template = testTemplate(), .captures = captures },
    });
    // Fill every variant identity from its allocated slice index before publication.
    for (variants, 0..) |*variant, index| variant.id = @fromBackingInt(@intCast(index));
    try program.fn_sets.append(allocator, .{ .layout = fn_layout, .variants = variants });
    const text = "the selected callable captures a large native string";
    var str = builtins.str.RocStr{ .bytes = @constCast(text.ptr), .length = text.len, .capacity_or_alloc_ptr = builtins.str.RocStr.encodeCapacity(text.len) };
    const data = program.layouts.getTagUnionData(program.layouts.getLayout(fn_layout).getTagUnion().idx);
    const native = try allocator.alloc(u8, program.layouts.layoutSize(program.layouts.getLayout(fn_layout)));
    defer allocator.free(native);
    @memset(native, 0xaa);
    @memcpy(native[0..@sizeOf(builtins.str.RocStr)], std.mem.asBytes(&str));
    data.writeDiscriminant(native.ptr, 1, program.layouts.targetUsize());
    const exports = try freezeRoot(allocator, &program, try testSlot(&program, fn_layout), testRoot(fn_plan, fn_layout), .{ .ptr = native.ptr }, .{});
    defer static_data.deinitStaticData(allocator, exports);
    try std.testing.expectEqual(@as(u32, 1), data.readDiscriminant(exports[0].bytes.ptr, program.layouts.targetUsize()));
    try std.testing.expectEqual(@as(usize, 1), exports[0].relocations.len);
    const relocation = exports[0].relocations[0];
    try std.testing.expectEqualStrings(text, exports[@backingInt(relocation.target.data_symbol)].bytes[@intCast(relocation.addend)..]);
}

test "native root export preserves erased callable procedure and drop helper identities" {
    const allocator = std.testing.allocator;
    var program = try Program.Result.init(allocator, @import("base").target.TargetUsize.native);
    defer program.deinit();
    const proc = try program.store.addProcSpec(.{ .name = lir.Symbol.fromRaw(42), .identity = lir.LIR.ProcIdentity.forTest(1), .args = .empty(), .ret_layout = .zst }, .none);
    const str_plan: Program.ConstPlanId = @fromBackingInt(@intCast(program.const_plans.items.len));
    const fn_layout = try program.layouts.insertErasedCallable();
    try program.const_plans.append(allocator, .str);
    const fn_plan: Program.ConstPlanId = @fromBackingInt(@intCast(program.const_plans.items.len));
    try program.const_plans.append(allocator, .{ .erased_fn = @fromBackingInt(@intCast(program.erased_fns.items.len)) });
    const captures = try allocator.dupe(Program.CaptureSlot, &.{testCapture(str_plan, .value)});
    const entries = try allocator.dupe(Program.ErasedFn, &.{.{ .entry = proc, .capture_layout = .str, .template = testTemplate(), .captures = captures, .on_drop = .{ .rc_helper = .{ .op = .host_drop, .layout_idx = .str } } }});
    try program.erased_fns.append(allocator, .{ .layout = fn_layout, .entries = entries });
    const text = "an erased callable retains this exact native capture";
    var str = builtins.str.RocStr{ .bytes = @constCast(text.ptr), .length = text.len, .capacity_or_alloc_ptr = builtins.str.RocStr.encodeCapacity(text.len) };
    var payload: [builtins.erased_callable.capture_offset + @sizeOf(builtins.str.RocStr)]u8 align(16) = @splat(0);
    std.mem.writeInt(usize, payload[0..word_size], 0x1111, .little);
    std.mem.writeInt(usize, payload[word_size..][0..word_size], 0x2222, .little);
    @memcpy(payload[builtins.erased_callable.capture_offset..], std.mem.asBytes(&str));
    var pointer = @intFromPtr(&payload);
    const Resolver = struct {
        proc: lir.LIR.LirProcSpecId,
        payload: [*]u8,
        fn resolve(context: ?*anyopaque, data_ptr: [*]u8) error{RuntimeError}!CallableResolution {
            const self: *@This() = @ptrCast(@alignCast(context.?));
            std.debug.assert(data_ptr == self.payload);
            return .{ .proc = self.proc, .capture_ptr = data_ptr + builtins.erased_callable.capture_offset };
        }
    };
    var resolver = Resolver{ .proc = proc, .payload = &payload };
    const exports = try freezeRoot(allocator, &program, try testSlot(&program, fn_layout), testRoot(fn_plan, fn_layout), .{ .ptr = @ptrCast(&pointer) }, .{ .context = &resolver, .resolve = Resolver.resolve });
    defer static_data.deinitStaticData(allocator, exports);
    const payload_pointer = exports[0].relocations[0];
    const payload_export = exports[@backingInt(payload_pointer.target.data_symbol)];
    try std.testing.expectEqual(@as(usize, 4), payload_export.relocations.len);
    try std.testing.expectEqual(proc, payload_export.relocations[0].procedure.get().?);
    try std.testing.expectEqual(builtins.erased_callable.capture_offset, payload_export.relocations[0].callable_capture_offset.get().?);
    try std.testing.expectEqual(layout.RcHelperKey{ .op = .host_drop, .layout_idx = .str }, payload_export.relocations[1].rc_helper.?);
    try std.testing.expectEqual(layout.RcHelperKey{ .op = .incref, .layout_idx = .str }, payload_export.relocations[2].rc_helper.?);
    const copied_header = payload_export.bytes[@intCast(payload_pointer.addend)..][0 .. 2 * word_size];
    try std.testing.expectEqualSlices(u8, &(@as([2 * word_size]u8, @splat(0))), copied_header);
    const capture_pointer = payload_export.relocations[3];
    try std.testing.expectEqualStrings(text, exports[@backingInt(capture_pointer.target.data_symbol)].bytes[@intCast(capture_pointer.addend)..]);

    const MissingRegistry = struct {
        fn resolve(_: ?*anyopaque, _: [*]u8) error{RuntimeError}!CallableResolution {
            return error.RuntimeError;
        }
    };
    // Missing producer evidence terminates freezing; no empty callable or
    // partially materialized graph may be published in its place.
    try std.testing.expectError(error.RuntimeError, freezeRoot(allocator, &program, try testSlot(&program, fn_layout), testRoot(fn_plan, fn_layout), .{ .ptr = @ptrCast(&pointer) }, .{ .resolve = MissingRegistry.resolve }));
}

test "native root export follows only selected tag payload and clears inactive bytes" {
    const allocator = std.testing.allocator;
    var program = try Program.Result.init(allocator, @import("base").target.TargetUsize.native);
    defer program.deinit();
    const str_plan: Program.ConstPlanId = @fromBackingInt(@intCast(program.const_plans.items.len));
    const tag_layout = try program.layouts.putTagUnion(&.{ .zst, .str });
    try program.const_plans.append(allocator, .str);
    const variants = try allocator.alloc(Program.ConstTagVariant, 2);
    variants[0] = .{ .name = try allocator.dupe(u8, "Absent"), .checked_name = undefined, .discriminant = 0, .payloads = try allocator.alloc(Program.ConstPlanId, 0) };
    variants[1] = .{ .name = try allocator.dupe(u8, "Present"), .checked_name = undefined, .discriminant = 1, .payloads = try allocator.dupe(Program.ConstPlanId, &.{str_plan}) };
    const tag_plan: Program.ConstPlanId = @fromBackingInt(@intCast(program.const_plans.items.len));
    try program.const_plans.append(allocator, .{ .tag_union = variants });
    const data = program.layouts.getTagUnionData(program.layouts.getLayout(tag_layout).getTagUnion().idx);
    const native = try allocator.alloc(u8, program.layouts.layoutSize(program.layouts.getLayout(tag_layout)));
    defer allocator.free(native);
    for ([_]u32{ 0, 1 }) |discriminant| {
        @memset(native, 0xaa);
        const text = "a selected tag payload larger than an inline string";
        var str = builtins.str.RocStr{ .bytes = @constCast(text.ptr), .length = text.len, .capacity_or_alloc_ptr = builtins.str.RocStr.encodeCapacity(text.len) };
        if (discriminant == 1) @memcpy(native[0..@sizeOf(builtins.str.RocStr)], std.mem.asBytes(&str));
        data.writeDiscriminant(native.ptr, discriminant, program.layouts.targetUsize());
        const exports = try freezeRoot(allocator, &program, try testSlot(&program, tag_layout), testRoot(tag_plan, tag_layout), .{ .ptr = native.ptr }, .{});
        defer static_data.deinitStaticData(allocator, exports);
        try std.testing.expectEqual(discriminant, data.readDiscriminant(exports[0].bytes.ptr, program.layouts.targetUsize()));
        if (discriminant == 0) {
            try std.testing.expectEqual(@as(usize, 1), exports.len);
            try std.testing.expectEqual(@as(usize, 0), exports[0].relocations.len);
            for (exports[0].bytes) |byte| try std.testing.expectEqual(@as(u8, 0), byte);
        } else {
            try std.testing.expectEqual(@as(usize, 2), exports.len);
            const pointer = exports[0].relocations[0];
            try std.testing.expectEqualStrings(text, exports[@backingInt(pointer.target.data_symbol)].bytes[@intCast(pointer.addend)..]);
        }
    }
}

test "native root export freezes every NaN as Roc's one NaN" {
    const allocator = std.testing.allocator;
    var program = try Program.Result.init(allocator, @import("base").target.TargetUsize.native);
    defer program.deinit();
    const scalar_plan: Program.ConstPlanId = @fromBackingInt(@intCast(program.const_plans.items.len));
    try program.const_plans.append(allocator, .scalar);

    var nan_payload: u64 = 0xfff9_2345_6789_abcd;
    const nan_slot = try testSlot(&program, .f64);
    const nan_exports = try freezeRoot(allocator, &program, nan_slot, testRoot(scalar_plan, .f64), .{ .ptr = @ptrCast(&nan_payload) }, .{});
    defer static_data.deinitStaticData(allocator, nan_exports);
    try std.testing.expectEqual(@as(usize, 1), nan_exports.len);
    try std.testing.expectEqual(builtins.float_bits.normalized_f64_nan_bits, std.mem.readInt(u64, nan_exports[0].bytes[nan_exports[0].symbol_offset..][0..8], .little));

    var f32_payload: u32 = 0xffc1_2345;
    const f32_slot = try testSlot(&program, .f32);
    const f32_exports = try freezeRoot(allocator, &program, f32_slot, testRoot(scalar_plan, .f32), .{ .ptr = @ptrCast(&f32_payload) }, .{});
    defer static_data.deinitStaticData(allocator, f32_exports);
    try std.testing.expectEqual(builtins.float_bits.normalized_f32_nan_bits, std.mem.readInt(u32, f32_exports[0].bytes[f32_exports[0].symbol_offset..][0..4], .little));

    var finite: u64 = @bitCast(@as(f64, -2.5));
    const finite_slot = try testSlot(&program, .f64);
    const finite_exports = try freezeRoot(allocator, &program, finite_slot, testRoot(scalar_plan, .f64), .{ .ptr = @ptrCast(&finite) }, .{});
    defer static_data.deinitStaticData(allocator, finite_exports);
    try std.testing.expectEqual(finite, std.mem.readInt(u64, finite_exports[0].bytes[finite_exports[0].symbol_offset..][0..8], .little));
}
