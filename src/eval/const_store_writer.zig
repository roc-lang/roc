//! Store compile-time values as checked constants: an evaluator's results,
//! read from the memory its program left them in, and value-form roots,
//! built in memory from their checked expressions and from the stored values
//! of the constants those read (design.md "Value-Form Roots"). A value that
//! is data alone is stored as its image (design.md "Stored Images"); a value
//! holding a function is stored as nodes by one plan-directed walk.

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
const NativeRootExport = @import("native_root_export.zig");
const frozen_graph = @import("frozen_graph.zig");

const Allocator = std.mem.Allocator;

/// Allocation failures and terminal invalid callable registry reports.
pub const Error = Allocator.Error || error{RuntimeError};
const checked = check.CheckedArtifact;
const const_store = check.ConstStore;
const LirProgram = lir.Program;
const RocList = builtins.list.RocList;
const RocStr = builtins.str.RocStr;
const CIR = can.CIR;
const exact_numeral = checked.exact_numeral;

/// A part of a value-form root: a checked expression in value form, or the
/// stored value of a constant such an expression reads.
pub const Part = union(enum) {
    checked: CIR.Expr.Idx,
    stored: StoredPart,
};

/// The stored value of a constant, in the store of the constant's module,
/// which may be the store being written.
pub const StoredPart = struct {
    store: *const const_store.ConstStore,
    node: checked.ConstNodeId,
};

/// What storing parts from checked expressions reads: the module's checked
/// bodies the expressions belong to, and the schema whose plans and layouts
/// the writer stores them by.
pub const CheckedValues = struct {
    bodies: checked.CheckedBodies,
    schema: *const lir.CheckedPipeline.BoxyValueSchema,
};

const RuntimeValueAddress = struct {
    ptr: usize,
    len: usize,
    /// A list's or string's evaluated capacity, which its stored node records. Two views
    /// of one allocation with the same length can differ in it: a seamless
    /// slice has none beyond its window.
    capacity: u64,
    plan: u32,
    layout: u32,
    storage: LirProgram.CaptureSlotStorage,
};

/// Runtime erased-callable identity decoded into the LIR proc and capture data.
pub const ErasedCallableResolution = struct {
    proc: lir.LIR.LirProcSpecId = undefined,
    external: ?struct { owner: [32]u8, identity: lir.LIR.ProcIdentity } = null,
    capture_ptr: [*]u8,
};

/// Resolves erased-callable runtime data for the active evaluator.
/// How a `.boxy` program's function values are read.
pub const BoxyFunctions = struct {
    retained: *lir.CheckedPipeline.BoxyStoredFunctions,
    /// The program's result, which reading a value extends.
    result: *LirProgram.Result,
    /// The module whose constants the writer stores into.
    store_module: checked.ModuleId,
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

/// Stores interpreted compile-time roots into a checked ConstStore.
pub const Writer = struct {
    allocator: Allocator,
    store: *const_store.ConstStore,
    /// The names stored types are translated into; null for a writer that
    /// stores values only (`storeLiteral`).
    names: ?*check.CanonicalNames.CanonicalNameStore,
    program: *const LirProgram.Result,
    /// The program's stored types and their names this writer has copied
    /// into the store, each once (`storeType`).
    type_translation: const_store.ConstTypeStore.Translation,
    stored_values: std.AutoHashMap(RuntimeValueAddress, checked.ConstNodeId),
    /// Set for a writer that stores value-form roots from their checked
    /// expressions.
    checked: ?CheckedValues = null,
    /// The module a writer of value-form roots stores into, whose roots a
    /// value-form root may read from the store being written.
    checked_module: ?*const checked.CheckedModuleArtifact = null,
    /// Whether the value-form root being stored reads other constants
    /// (`CompileTimeRoot.reads_constants`); a root that reads none has no
    /// part that is a lookup.
    checked_reads_constants: bool = false,
    /// Scratch for a checked string's bytes.
    checked_text: std.ArrayList(u8) = .empty,
    /// Scratch for a checked record's field values in plan order.
    checked_components: std.ArrayList(CIR.Expr.Idx) = .empty,
    erased_callable_resolver: ErasedCallableResolver,
    boxy_functions: ?BoxyFunctions = null,
    product_plans: std.AutoHashMap(ProductPlanKey, ?lir.PackedData.Plan),
    product_eligibility: collections.DenseMap(LirProgram.ConstPlanId, bool),
    /// Composites being stored, innermost last, and the shared stacks their
    /// component requests, nodes and capture types occupy runs of.
    store_frames: std.ArrayList(StoreFrame) = .empty,
    store_requests: std.ArrayList(ValueRequest) = .empty,
    store_nodes: std.ArrayList(checked.ConstNodeId) = .empty,
    store_capture_types: std.ArrayList(const_store.ConstTypeId) = .empty,
    /// Content digests of the program's layouts, for the images it stores.
    layout_digests: ?layout.Digests = null,

    const ProductPlanKey = struct { plan: LirProgram.ConstPlanId, layout_idx: layout.Idx };

    pub fn init(
        allocator: Allocator,
        module: *checked.CheckedModuleArtifact,
        program: *const LirProgram.Result,
    ) Writer {
        return initStore(allocator, &module.const_store, &module.canonical_names, program);
    }

    /// A writer into `store`, translating stored types into `names`.
    pub fn initStore(
        allocator: Allocator,
        store: *const_store.ConstStore,
        names: ?*check.CanonicalNames.CanonicalNameStore,
        program: *const LirProgram.Result,
    ) Writer {
        return .{
            .allocator = allocator,
            .store = store,
            .names = names,
            .program = program,
            .type_translation = .init(allocator),
            .stored_values = std.AutoHashMap(RuntimeValueAddress, checked.ConstNodeId).init(allocator),
            .erased_callable_resolver = .{},
            .product_plans = std.AutoHashMap(ProductPlanKey, ?lir.PackedData.Plan).init(allocator),
            .product_eligibility = collections.DenseMap(LirProgram.ConstPlanId, bool).init(allocator),
        };
    }

    /// A writer storing value-form roots of `module` from their checked
    /// expressions, by `values.schema`'s plans, layouts and stored types.
    pub fn initChecked(allocator: Allocator, module: *checked.CheckedModuleArtifact, values: CheckedValues) Writer {
        var writer = init(allocator, module, values.schema.result());
        writer.checked = values;
        writer.checked_module = module;
        return writer;
    }

    pub fn setErasedCallableResolver(self: *Writer, resolver: ErasedCallableResolver) void {
        self.erased_callable_resolver = resolver;
    }

    /// Store this `.boxy` program's function values, reading them through
    /// `functions` (`postcheck.Boxy.StoredFn`).
    pub fn setBoxyFunctions(self: *Writer, functions: BoxyFunctions) void {
        self.boxy_functions = functions;
    }

    /// The producer representation of functions in this plan, including
    /// nested aggregates. One program's plans use one callable representation.
    pub fn planFunctionStrategy(self: *Writer, id: LirProgram.ConstPlanId) Allocator.Error!?base.SpecializationStrategy {
        var seen = collections.DenseMap(LirProgram.ConstPlanId, void).init(self.allocator);
        defer seen.deinit();
        var pending = std.ArrayList(LirProgram.ConstPlanId).empty;
        defer pending.deinit(self.allocator);
        try pending.append(self.allocator, id);
        while (pending.pop()) |plan_id| {
            if (seen.contains(plan_id)) continue;
            try seen.put(plan_id, {});
            switch (self.constPlan(plan_id).*) {
                .boxy_fn => return .boxy,
                .fn_value, .erased_fn => return .lss,
                .pending, .layout_only, .zst, .scalar, .str => {},
                .list, .box => |child| try pending.append(self.allocator, child),
                .tuple, .record => |children| try pending.appendSlice(self.allocator, children),
                .tag_union => |variants| for (variants) |variant| try pending.appendSlice(self.allocator, variant.payloads),
                .named => |named| try pending.append(self.allocator, named.backing),
            }
        }
        return null;
    }

    fn pushBoxyFn(self: *Writer, node: checked.ConstNodeId, value: Value) Error!void {
        const functions = self.boxy_functions orelse writerInvariant("a function value reached a program that stores none");
        const data_ptr = self.readErasedCallablePointer(value);
        const resolved = try self.erased_callable_resolver.resolve(self.erased_callable_resolver.context, data_ptr);
        const described = if (resolved.external) |external|
            try functions.retained.describeImported(self.allocator, functions.result, functions.store_module, external.owner, external.identity, resolved.capture_ptr)
        else
            try functions.retained.describe(self.allocator, functions.result, functions.store_module, resolved.proc, resolved.capture_ptr);
        // The frame owns the recipe only after pushStoreFrame succeeds.
        errdefer self.allocator.free(described.recipe);
        defer self.allocator.free(described.captures);
        const requests_start = self.store_requests.items.len;
        for (described.captures) |capture| {
            try self.store_requests.append(self.allocator, .{ .plan = capture.plan, .layout_idx = capture.layout_idx, .value = .{ .ptr = capture.ptr } });
        }
        try self.pushStoreFrame(.{ .boxy_fn = .{ .node = node, .recipe = described.recipe } }, requests_start, &.{});
    }

    pub fn deinit(self: *Writer) void {
        self.clearStoreStacks();
        self.store_capture_types.deinit(self.allocator);
        self.store_nodes.deinit(self.allocator);
        self.store_requests.deinit(self.allocator);
        self.store_frames.deinit(self.allocator);
        var plans = self.product_plans.valueIterator();
        while (plans.next()) |plan| if (plan.*) |*present| present.deinit();
        self.product_plans.deinit();
        self.product_eligibility.deinit();
        self.checked_text.deinit(self.allocator);
        self.checked_components.deinit(self.allocator);
        self.stored_values.deinit();
        self.type_translation.deinit();
        if (self.layout_digests) |*digests| digests.deinit();
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

        const plan = self.constPlan(root.plan).*;
        return switch (root.request.kind) {
            // A value whose plan can hold no function is data alone, which
            // the store holds as its image (design.md "Stored Images").
            .compile_time_constant, .repl_expr => if (try self.planFunctionStrategy(root.plan) == null)
                .{ .const_node = try self.storeImage(root.shape(), try self.storeRootType(root), value) }
            else
                .{ .const_node = try self.storeValue(root.plan, root.ret_layout, value) },
            .compile_time_callable => switch (plan) {
                .fn_value => |set| .{ .fn_value = try self.storeFnValue(set, root.ret_layout, value) },
                .erased_fn => |set| .{ .fn_value = try self.storeErasedFn(set, value) },
                .boxy_fn => .{ .const_node = try self.storeValue(root.plan, root.ret_layout, value) },
                .pending,
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

    /// Store value `value` of `shape`, whose type is `root_type` in the
    /// store, as an image, the value of a new node.
    fn storeImage(self: *Writer, shape: LirProgram.RootShape, root_type: const_store.ConstTypeId, value: Value) Error!checked.ConstNodeId {
        var graph = NativeRootExport.ImageGraph{ .allocator = self.allocator, .program = self.program };
        defer graph.deinit();
        const image = try NativeRootExport.imageRoot(&graph, shape, value);
        return try self.store.appendImage(.{
            .bytes = image.bytes,
            .pointers = image.pointers,
            .alignment = image.alignment,
            .root_size = @intCast(graph.size(shape.ret_layout)),
            .root_type = root_type,
            .layout_digest = try self.layoutDigest(shape.ret_layout),
            .type_digest = try self.store.type_store.identityDigest(self.allocator, self.typeNames(), root_type),
        });
    }

    /// The content digest of the program's layout `idx`.
    fn layoutDigest(self: *Writer, idx: layout.Idx) Allocator.Error![32]u8 {
        if (self.layout_digests == null) self.layout_digests = layout.Digests.init(self.allocator, &self.program.layouts);
        return self.layout_digests.?.get(idx);
    }

    /// Store a value-form root from its checked expression `expr`, by the
    /// plan and layout of its result (`BoxyValueSchema.root`), and from the
    /// stored values of the constants it reads when `reads_constants`, as
    /// the image of type `root_type` an evaluation of the expression would
    /// store (`ValueFormImage`). A value-form root is data alone.
    pub fn storeCheckedRoot(
        self: *Writer,
        plan: LirProgram.ConstPlanId,
        ret_layout: layout.Idx,
        root_type: const_store.ConstTypeId,
        expr: CIR.Expr.Idx,
        reads_constants: bool,
    ) Error!checked.ConstNodeId {
        // A value-form value builds no function, whatever its type can
        // hold (an empty list of functions, a tag of a union with a function
        // payload), so it is data alone.
        if (self.checked == null) writerInvariant("a writer of evaluated values stored a checked expression");
        self.checked_reads_constants = reads_constants;
        var image: ValueFormImage = .{ .writer = self, .graph = .{ .allocator = self.allocator, .program = self.program } };
        defer image.deinit();
        const result = try image.build(plan, ret_layout, expr);
        return try self.store.appendImage(.{
            .bytes = result.bytes,
            .pointers = result.pointers,
            .alignment = result.alignment,
            .root_size = @intCast(image.graph.size(ret_layout)),
            .root_type = root_type,
            .layout_digest = try self.layoutDigest(ret_layout),
            .type_digest = try self.store.type_store.identityDigest(self.allocator, self.typeNames(), root_type),
        });
    }

    fn typeNames(self: *const Writer) *check.CanonicalNames.CanonicalNameStore {
        return self.names orelse writerInvariant("a value-only ConstStore writer stored a type");
    }

    /// Preserve the exact producer-owned representation of a compile-time
    /// root in the checked module's durable ConstStore type table.
    pub fn storeRootType(
        self: *Writer,
        root: LirProgram.ConstRootPlan,
    ) Error!const_store.ConstTypeId {
        return self.storeType(root.ret_type);
    }

    /// Preserve stored type `ty` of the writer's program in the module's
    /// durable ConstStore type table. The table holds one copy of each of
    /// the program's types this writer stores, however many roots, captures
    /// and other stored types refer to it.
    pub fn storeType(self: *Writer, ty: const_store.ConstTypeId) Error!const_store.ConstTypeId {
        return self.store.type_store.cloneTypeFromTranslated(
            &self.program.const_types,
            &self.program.const_type_names,
            self.typeNames(),
            &self.type_translation,
            ty,
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
        list: struct { node: checked.ConstNodeId, capacity: u64 },
        box: checked.ConstNodeId,
        tuple: checked.ConstNodeId,
        record: checked.ConstNodeId,
        /// The nominal's plan, by id: the plan table may grow while the
        /// frame's components are stored.
        nominal: struct { node: checked.ConstNodeId, plan: LirProgram.ConstPlanId },
        tag: struct { node: checked.ConstNodeId, name: []const u8 },
        /// A function value; `node` is null for a callable root, whose
        /// function id is the store's result.
        fn_value: struct { node: ?checked.ConstNodeId, template: *const LirProgram.FnTemplate },
        /// A `.boxy` function value; `recipe` is the writer's to free.
        boxy_fn: struct { node: checked.ConstNodeId, recipe: []const u8 },
    };

    /// A composite value whose components are stored in order, each
    /// component's whole subtree before the next component starts. Every
    /// component's node is reserved when it starts, and the composite fills
    /// its own node once every component is stored.
    const StoreFrame = struct {
        finish: StoreFinish,
        /// The frame's run of `store_requests`; `store_nodes` holds the
        /// node of each request from `nodes_start`.
        requests_start: usize,
        requests_len: usize,
        nodes_start: usize,
        /// Capture slots of a function value: each slot's type is cloned just
        /// before its value is stored, into `store_capture_types` from
        /// `capture_types_start`.
        slots: []const LirProgram.CaptureSlot = &.{},
        capture_types_start: usize,
        index: usize = 0,
    };

    // A frame is pushed and finished for every stored composite node, so it
    // names a function's template and a nominal's plan rather than copying
    // them.
    comptime {
        if (@sizeOf(usize) == 8) std.debug.assert(@sizeOf(StoreFrame) == 88);
    }

    fn storeValue(
        self: *Writer,
        plan_id: LirProgram.ConstPlanId,
        layout_idx: layout.Idx,
        value: Value,
    ) Error!checked.ConstNodeId {
        self.clearStoreStacks();
        const node = try self.startStore(.{ .plan = plan_id, .layout_idx = layout_idx, .value = value });
        _ = try self.drainStore();
        return node;
    }

    fn storeFnValue(
        self: *Writer,
        set_id: LirProgram.FnSetId,
        layout_idx: layout.Idx,
        value: Value,
    ) Error!checked.ConstFnId {
        self.clearStoreStacks();
        try self.pushFnValue(null, set_id, layout_idx, value);
        return (try self.drainStore()).?;
    }

    fn storeErasedFn(
        self: *Writer,
        set_id: LirProgram.ErasedFnsId,
        value: Value,
    ) Error!checked.ConstFnId {
        self.clearStoreStacks();
        try self.pushErasedFn(null, set_id, value);
        return (try self.drainStore()).?;
    }

    fn clearStoreStacks(self: *Writer) void {
        // A popped frame transfers its recipe to finishStore, which frees it
        // even on failure. Only frames still pending belong to this stack.
        for (self.store_frames.items) |frame| switch (frame.finish) {
            .boxy_fn => |function| self.allocator.free(function.recipe),
            .list, .box, .tuple, .record, .nominal, .tag, .fn_value => {},
        };
        self.store_frames.clearRetainingCapacity();
        self.store_requests.clearRetainingCapacity();
        self.store_nodes.clearRetainingCapacity();
        self.store_capture_types.clearRetainingCapacity();
    }

    /// Stores every pending component, and returns the function id of a
    /// callable root. Frames finish in reverse order of starting, so each
    /// finished frame's runs are the tops of the shared stacks.
    fn drainStore(self: *Writer) Error!?checked.ConstFnId {
        var root_fn: ?checked.ConstFnId = null;
        while (self.store_frames.items.len > 0) {
            const top = self.store_frames.items.len - 1;
            const frame = &self.store_frames.items[top];
            if (frame.index < frame.requests_len) {
                const index = frame.index;
                frame.index += 1;
                const request = self.store_requests.items[frame.requests_start + index];
                if (frame.slots.len > 0) {
                    const ty = try self.storeType(frame.slots[index].ty);
                    self.store_capture_types.items[frame.capture_types_start + index] = ty;
                }
                const node = try self.startStore(request);
                self.store_nodes.items[self.store_frames.items[top].nodes_start + index] = node;
                continue;
            }
            const finished = self.store_frames.pop().?;
            if (try self.finishStore(&finished)) |fn_id| root_fn = fn_id;
            self.store_requests.shrinkRetainingCapacity(finished.requests_start);
            self.store_nodes.shrinkRetainingCapacity(finished.nodes_start);
            self.store_capture_types.shrinkRetainingCapacity(finished.capture_types_start);
        }
        return root_fn;
    }

    /// Reserves the request's node, fills it when the value has no
    /// components, and pushes the frame that stores its components otherwise.
    /// A part already stored for the root (the same runtime allocation) is
    /// that node.
    fn startStore(self: *Writer, request: ValueRequest) Error!checked.ConstNodeId {
        if (self.memoAddress(request.plan, request.layout_idx, request.value, request.storage)) |address| {
            if (self.stored_values.get(address)) |existing| return existing;
            const node = try self.store.reserve();
            try self.stored_values.put(address, node);
            try self.beginFresh(node, request);
            return node;
        }
        const node = try self.store.reserve();
        try self.beginFresh(node, request);
        return node;
    }

    fn beginFresh(
        self: *Writer,
        node: checked.ConstNodeId,
        request: ValueRequest,
    ) Error!void {
        var layout_idx = request.layout_idx;
        var part = request.value;
        switch (request.storage) {
            .value => {},
            .recursive_box => {
                const boxed = self.program.layouts.getLayout(layout_idx);
                if (boxed.tag != .box) writerInvariant("recursive capture slot did not use box storage");
                const payload = self.readBoxDataPointer(part) orelse
                    writerInvariant("recursive capture slot had null payload pointer");
                layout_idx = boxed.getIdx();
                part = .{ .ptr = payload };
            },
        }
        const requests_start = self.store_requests.items.len;
        const children = &self.store_requests;
        const finish: StoreFinish = switch (self.constPlan(request.plan).*) {
            .pending => writerInvariant("pending const plan reached ConstStore writer"),
            .layout_only => writerInvariant("layout-only const plan reached ConstStore writer"),
            .zst => return self.store.fill(node, .zst),
            .scalar => return self.store.fill(node, .{ .scalar = self.storeScalar(layout_idx, part) }),
            .str => return self.store.fill(node, try self.storeStr(part)),
            .list => |elem_plan| blk: {
                if (try self.storeListLeaf(node, elem_plan, layout_idx, part)) return;
                try self.appendListRequests(children, elem_plan, layout_idx, part);
                break :blk .{ .list = .{ .node = node, .capacity = listCapacity(part) } };
            },
            .box => |elem_plan| blk: {
                try self.appendBoxRequest(children, elem_plan, layout_idx, part);
                break :blk .{ .box = node };
            },
            .tuple => |items| blk: {
                try self.appendStructRequests(children, items, layout_idx, part);
                break :blk .{ .tuple = node };
            },
            .record => |fields| blk: {
                try self.appendStructRequests(children, fields, layout_idx, part);
                break :blk .{ .record = node };
            },
            .tag_union => |variants| blk: {
                const tag_base = self.resolveTagBase(layout_idx, part);
                const selected = self.selectTagVariant(variants, tag_base.layout_idx, tag_base.value);
                const payload_layout = self.tagPayloadLayout(tag_base.layout_idx, selected.discriminant);
                try self.appendTagPayloadRequests(children, selected.payloads, payload_layout, tag_base.value);
                break :blk .{ .tag = .{ .node = node, .name = selected.name } };
            },
            .named => |named| blk: {
                try children.append(self.allocator, .{ .plan = named.backing, .layout_idx = layout_idx, .value = part });
                break :blk .{ .nominal = .{ .node = node, .plan = request.plan } };
            },
            .fn_value => |set| return self.pushFnValue(node, set, layout_idx, part),
            .erased_fn => |set| return self.pushErasedFn(node, set, part),
            .boxy_fn => return self.pushBoxyFn(node, part),
        };
        try self.pushStoreFrame(finish, requests_start, &.{});
    }

    /// Push the frame storing the requests appended to `store_requests`
    /// since `requests_start`.
    fn pushStoreFrame(
        self: *Writer,
        finish: StoreFinish,
        requests_start: usize,
        slots: []const LirProgram.CaptureSlot,
    ) Allocator.Error!void {
        const requests_len = self.store_requests.items.len - requests_start;
        try self.store_frames.ensureUnusedCapacity(self.allocator, 1);
        const nodes_start = self.store_nodes.items.len;
        try self.store_nodes.resize(self.allocator, nodes_start + requests_len);
        const capture_types_start = self.store_capture_types.items.len;
        try self.store_capture_types.resize(self.allocator, capture_types_start + slots.len);
        const frame = self.store_frames.addOneAssumeCapacity();
        frame.finish = finish;
        frame.requests_start = requests_start;
        frame.requests_len = requests_len;
        frame.nodes_start = nodes_start;
        frame.slots = slots;
        frame.capture_types_start = capture_types_start;
        frame.index = 0;
    }

    fn pushFnValue(
        self: *Writer,
        node: ?checked.ConstNodeId,
        set_id: LirProgram.FnSetId,
        layout_idx: layout.Idx,
        value: Value,
    ) Error!void {
        const set = &self.program.fn_sets.items[@backingInt(set_id)];
        const tag_base = self.resolveTagBase(layout_idx, value);
        const variant = self.selectFnVariant(set, tag_base.layout_idx, tag_base.value);
        const requests_start = self.store_requests.items.len;
        try self.appendCaptureRequests(&self.store_requests, variant.captures, variant.payload_layout, tag_base.value);
        try self.pushStoreFrame(.{ .fn_value = .{ .node = node, .template = &variant.template } }, requests_start, variant.captures);
    }

    fn pushErasedFn(
        self: *Writer,
        node: ?checked.ConstNodeId,
        set_id: LirProgram.ErasedFnsId,
        value: Value,
    ) Error!void {
        const entry = try self.resolveErasedEntry(set_id, value);
        const requests_start = self.store_requests.items.len;
        try self.appendCaptureRequests(&self.store_requests, entry.entry.captures, entry.entry.capture_layout, .{ .ptr = entry.capture_ptr });
        const template = if (entry.entry.template) |*template| template else writerInvariant("erased callable entry has no ConstStore template");
        try self.pushStoreFrame(.{ .fn_value = .{ .node = node, .template = template } }, requests_start, entry.entry.captures);
    }

    const ResolvedErasedEntry = struct {
        entry: *const LirProgram.ErasedFn,
        capture_ptr: [*]u8,
    };

    fn resolveErasedEntry(self: *Writer, set_id: LirProgram.ErasedFnsId, value: Value) Error!ResolvedErasedEntry {
        const set = self.program.erased_fns.items[@backingInt(set_id)];
        const data_ptr = self.readErasedCallablePointer(value);
        const resolved = try self.erased_callable_resolver.resolve(self.erased_callable_resolver.context, data_ptr);
        for (set.entries) |*entry| {
            if (entry.entry != resolved.proc) continue;
            return .{ .entry = entry, .capture_ptr = resolved.capture_ptr };
        }
        writerInvariant("erased callable result did not match an explicit erased function entry");
    }

    /// Fills a finished composite's node, and returns the function id when
    /// the composite is a callable root.
    fn finishStore(self: *Writer, frame: *const StoreFrame) Error!?checked.ConstFnId {
        const nodes = self.store_nodes.items[frame.nodes_start..][0..frame.requests_len];
        const capture_types = self.store_capture_types.items[frame.capture_types_start..][0..frame.slots.len];
        switch (frame.finish) {
            .list => |list| self.store.fill(list.node, .{ .list = .{ .items = .{ .nodes = nodes }, .capacity = list.capacity } }),
            .box => |node| self.store.fill(node, .{ .box = nodes[0] }),
            .tuple => |node| self.store.fill(node, .{ .tuple = nodes }),
            .record => |node| self.store.fill(node, .{ .record = nodes }),
            .nominal => |nominal| self.store.fill(nominal.node, .{ .nominal = .{
                .named_type = self.constPlan(nominal.plan).named.named_type,
                .backing = nodes[0],
            } }),
            .tag => |tag| {
                const tag_name = try self.store.allocator.dupe(u8, tag.name);
                defer self.store.allocator.free(tag_name);
                self.store.fill(tag.node, .{ .tag = .{
                    .tag_name = tag_name,
                    .payloads = nodes,
                } });
            },
            .boxy_fn => |function| {
                defer self.allocator.free(function.recipe);
                self.store.fill(function.node, .{ .boxy_fn = .{ .recipe = try self.storeBytes(function.recipe), .captures = nodes } });
            },
            .fn_value => |fn_value| {
                const captures = try self.store.allocator.alloc(const_store.ConstCapture, frame.slots.len);
                defer self.store.allocator.free(captures);
                for (captures, frame.slots, capture_types, nodes) |*capture, slot, ty, node| {
                    capture.* = .{ .id = slot.id, .kind = slot.kind, .ty = ty, .value = node };
                }
                const template = fn_value.template;
                const fn_id = try self.store.appendFn(.{
                    .fn_def = template.fn_def,
                    .source_fn_ty = template.source_fn_ty,
                    .source_fn_key = template.source_fn_key,
                    .owner_fn_ty = if (template.owner_fn_ty.get()) |owner_fn_ty| try self.storeType(owner_fn_ty) else null,
                    .captures = captures,
                    .evidence = template.evidence,
                    .evidence_frames = template.evidence_frames,
                    .evidence_frame_head = template.evidence_frame_head.get(),
                });
                if (fn_value.node) |node| {
                    self.store.fill(node, .{ .fn_value = fn_id });
                    return null;
                }
                return fn_id;
            },
        }
        return null;
    }

    /// Stores a list whose elements need no nodes of their own (an empty,
    /// packed-scalar, or packed-product list, discriminant-only tag unions
    /// among a product's leaves); false when its elements are stored as
    /// nodes.
    fn storeListLeaf(
        self: *Writer,
        target_node: checked.ConstNodeId,
        elem_plan: LirProgram.ConstPlanId,
        layout_idx: layout.Idx,
        part: Value,
    ) Error!bool {
        const layout_value = self.program.layouts.getLayout(layout_idx);
        if (layout_value.tag != .list and layout_value.tag != .list_of_zst) {
            writerInvariant("list const plan had non-list layout");
        }
        const roc_list: *const RocList = @ptrCast(@alignCast(part.ptr));
        const len = roc_list.len();
        const capacity = listCapacity(part);
        if (len == 0) {
            self.store.fill(target_node, .{ .list = .{ .items = .empty, .capacity = capacity } });
            return true;
        }
        if (self.planIsScalar(elem_plan)) {
            try self.storePackedList(target_node, layout_value, part, capacity);
            return true;
        }
        const elem_layout = if (layout_value.tag == .list_of_zst) layout.Idx.zst else layout_value.getIdx();
        if (try self.productPlan(elem_plan, elem_layout)) |plan| {
            const memory_len = std.math.mul(usize, len, plan.memory_width) catch unreachable; // the list's elements occupy this many bytes of live memory
            const memory = if (memory_len == 0) &.{} else roc_list.bytes.?[0..memory_len];
            try self.storeProductList(target_node, elem_plan, elem_layout, plan, memory, len, capacity);
            return true;
        }
        return false;
    }

    /// The capacity the list at `value` was evaluated with, in items. A list
    /// of zero-sized items holds no allocation and records its capacity as a
    /// count alone.
    fn listCapacity(value: Value) u64 {
        const roc_list: *const RocList = @ptrCast(@alignCast(value.ptr));
        return roc_list.getCapacity();
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
            .hole => writerInvariant("a demand hole reached a compile-time value"),
            .dynamic => writerInvariant("a dynamic value reached a compile-time value"),
            .box_of_zst => .{ .plan = elem_plan, .layout_idx = .zst, .value = Value.zst },
            .box => .{
                .plan = elem_plan,
                .layout_idx = layout_value.getIdx(),
                .value = .{ .ptr = self.readBoxDataPointer(value) orelse writerInvariant("boxed value had null payload pointer") },
            },
            .erased_callable => .{ .plan = elem_plan, .layout_idx = layout_idx, .value = value },
            .scalar,
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

    /// The component requests of a tuple or record whose components' plans
    /// are `plans`.
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
            for (plans) |component| out.appendAssumeCapacity(.{ .plan = component, .layout_idx = .zst, .value = Value.zst });
            return;
        }
        if (layout_value.tag != .struct_) writerInvariant("struct const plan had non-struct layout");
        for (plans, 0..) |component, index| {
            const field_layout = self.program.layouts.getStructFieldLayoutByOriginalIndex(layout_value.getStruct().idx, @intCast(index));
            const offset = self.program.layouts.getStructFieldOffsetByOriginalIndex(layout_value.getStruct().idx, @intCast(index));
            out.appendAssumeCapacity(.{ .plan = component, .layout_idx = field_layout, .value = value.offset(offset) });
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

    /// A string is stored as exactly its own bytes and the capacity it was
    /// evaluated with. How the evaluator's allocations share bytes between
    /// strings is runtime representation, which never enters the ConstStore;
    /// equal bytes share one blob because `addBlobData` interns by content.
    fn storeStr(self: *Writer, value: Value) Allocator.Error!checked.ConstValue {
        const roc_str: *const RocStr = @ptrCast(@alignCast(value.ptr));
        return .{ .str = .{ .bytes = try self.storeBytes(roc_str.asSlice()), .capacity = strCapacity(roc_str) } };
    }

    /// The capacity the string was evaluated with. One held inline in its
    /// descriptor has no capacity of its own beyond its bytes, and neither
    /// does a seamless slice, whose capacity is its window.
    fn strCapacity(roc_str: *const RocStr) u64 {
        if (roc_str.isSmallStr()) return roc_str.len();
        return roc_str.getCapacity();
    }

    fn checkedValues(self: *const Writer) CheckedValues {
        return self.checked orelse writerInvariant("a writer of evaluated values read a checked expression");
    }

    /// A value-form numeral's value at the number type of `layout_idx`, as
    /// the evaluator's memory holds it (`exact_numeral.encodeBits`, which the
    /// lowering's numerals are built from).
    fn checkedScalarBytes(self: *Writer, layout_idx: layout.Idx, expr: CIR.Expr.Idx) Allocator.Error![16]u8 {
        return self.checkedNumeralBytes(self.numeralTarget(layout_idx), expr);
    }

    /// The number type a value-form numeral at `layout_idx` is encoded at.
    fn numeralTarget(self: *const Writer, layout_idx: layout.Idx) exact_numeral.Target {
        const layout_value = self.program.layouts.getLayout(layout_idx);
        if (layout_value.tag != .scalar) writerInvariant("a value-form numeral's layout is not a scalar");
        const scalar = layout_value.getScalar();
        return switch (scalar.tag) {
            .int => switch (scalar.getInt()) {
                inline .u8, .i8, .u16, .i16, .u32, .i32, .u64, .i64, .u128, .i128 => |int| @field(exact_numeral.Target, @tagName(int)),
            },
            .frac => switch (scalar.getFrac()) {
                .f32 => .f32,
                .f64 => .f64,
                .dec => .dec,
            },
            .str, .opaque_ptr, .vector => writerInvariant("a value-form numeral's layout is not a number"),
        };
    }

    /// Value-form numeral `expr` at `target`, as the evaluator's memory
    /// holds it.
    fn checkedNumeralBytes(self: *Writer, target: exact_numeral.Target, expr: CIR.Expr.Idx) Allocator.Error![16]u8 {
        const bodies = self.checkedValues().bodies;
        const value_expr = checked.valueFormValueExpr(bodies, expr);
        if (bodies.exprData(value_expr) != .numeral) writerInvariant("a value-form scalar is not a numeral");
        const exact = bodies.env.exactNumeral(bodies.numeralLiteral(@backingInt(value_expr)));
        return (try exact_numeral.encodeBits(self.allocator, exact, target, builtin.cpu.arch.endian())) orelse
            writerInvariant("a value-form numeral does not fit its type");
    }

    /// A value-form string's bytes: its segments in order.
    fn checkedStrText(self: *Writer, expr: CIR.Expr.Idx) Allocator.Error![]const u8 {
        const bodies = self.checkedValues().bodies;
        self.checked_text.clearRetainingCapacity();
        for (checkedStrSegments(bodies, expr)) |segment| {
            const data = bodies.exprData(segment);
            if (data != .str_segment) writerInvariant("a value-form string's part is not a literal segment");
            try self.checked_text.appendSlice(self.allocator, bodies.text(data.str_segment));
        }
        return self.checked_text.items;
    }

    /// The items of a value-form list.
    fn checkedListItems(self: *const Writer, expr: CIR.Expr.Idx) []const CIR.Expr.Idx {
        const bodies = self.checkedValues().bodies;
        const data = bodies.exprData(checked.valueFormValueExpr(bodies, expr));
        if (data == .empty_list) return &.{};
        if (data != .list) writerInvariant("a value-form list is not a list literal");
        return data.list;
    }

    /// The part of a value-form value a named plan's backing stores: a
    /// nominal construction's backing expression, or the expression itself
    /// where the checked type names the nominal and the expression builds
    /// its backing (a `Bool` or `Try` tag).
    fn checkedBacking(self: *const Writer, expr: CIR.Expr.Idx) CIR.Expr.Idx {
        const bodies = self.checkedValues().bodies;
        const value_expr = checked.valueFormValueExpr(bodies, expr);
        const data = bodies.exprData(value_expr);
        return if (data == .nominal) data.nominal.backing_expr else value_expr;
    }

    /// The parts of value-form tuple or record `expr` in the order plan
    /// `plan` stores its components: a tuple's items, or a record's field
    /// values, each placed at the component the schema gives its label. The
    /// slice is valid until the next call.
    fn checkedComponents(self: *Writer, plan: LirProgram.ConstPlanId, expr: CIR.Expr.Idx, count: usize) Allocator.Error![]const CIR.Expr.Idx {
        const values = self.checkedValues();
        const bodies = values.bodies;
        const data = bodies.exprData(checked.valueFormValueExpr(bodies, expr));
        if (data == .tuple) {
            if (data.tuple.len != count) writerInvariant("a value-form tuple does not have its plan's components");
            return data.tuple;
        }
        if (data != .record) writerInvariant("a value-form product is not a tuple or record");
        if (data.record.fields.len != count) writerInvariant("a value-form record does not have its plan's components");
        try self.checked_components.resize(self.allocator, count);
        for (data.record.fields) |field_idx| {
            const field = bodies.recordField(field_idx);
            const component = try values.schema.recordComponent(plan, bodies.names.recordFieldLabelText(field.label));
            self.checked_components.items[component] = field.value;
        }
        return self.checked_components.items;
    }

    /// `part`, or, when it is a value-form expression reading a constant,
    /// the constant's stored value (design.md "Value-Form Roots").
    fn resolvePart(self: *const Writer, part: Part) Part {
        const expr = switch (part) {
            .checked => |expr| expr,
            .stored => return part,
        };
        if (!self.checked_reads_constants) return part;
        const values = self.checkedValues();
        const value_expr = checked.valueFormValueExpr(values.bodies, expr);
        switch (values.bodies.exprData(value_expr)) {
            .lookup_local, .lookup_external => {},
            .numeral, .str, .str_segment, .bytes_literal, .lookup_required, .list, .empty_list, .tuple, .match_, .if_, .call, .record, .empty_record, .block, .tag, .nominal, .zero_argument_tag, .closure, .lambda, .binop, .unary_minus, .field_access, .dispatch_call, .interpolation, .structural_eq, .structural_hash, .method_eq, .type_dispatch_call, .tuple_access, .runtime_error, .crash, .dbg, .expect_err, .expect, .ellipsis, .anno_only, .break_, .return_, .for_, .hosted_lambda, .run_low_level => return part,
        }
        const const_ref = values.bodies.constLookupRef(value_expr) orelse writerInvariant("a value-form root reads a lookup that names no constant");
        return .{ .stored = self.storedConst(const_ref) };
    }

    /// The stored value of the constant `const_ref` names, which the
    /// checked-value writer completed before the root reading it: a root of
    /// the module being written, from the store being written, since every
    /// artifact of one checked identity owns its own store, or a root of a
    /// module the schema holds.
    fn storedConst(self: *const Writer, const_ref: checked.ConstRef) StoredPart {
        const module = self.checked_module orelse writerInvariant("a writer of evaluated values read a constant");
        const store: *const const_store.ConstStore, const roots: *const checked.CompileTimeRootTable = if (const_ref.artifact.eql(module.key.digest()))
            .{ self.store, &module.compile_time_roots }
        else blk: {
            const view = self.checkedValues().schema.moduleView(const_ref.artifact);
            break :blk .{ view.const_store, view.compile_time_roots };
        };
        const root_id = switch (const_ref.owner) {
            .top_level_binding => |top_level| roots.lookupIdByPattern(top_level.pattern) orelse writerInvariant("a constant a value-form root reads has no compile-time root"),
            .hoisted_root => |hoisted| hoisted.root,
        };
        const root = roots.root(root_id);
        if (builtin.mode == .debug) std.debug.assert(root.completion == .write_value);
        return switch (root.payload) {
            .const_node => |node| .{ .store = store, .node = node },
            .pending, .unevaluated, .fn_value, .discarded, .expect, .runtime => writerInvariant("a constant a value-form root reads is not complete"),
        };
    }

    fn storeBytes(self: *Writer, bytes: []const u8) Allocator.Error!const_store.ConstStr {
        return try self.store.addBlobData(bytes);
    }

    fn planIsProduct(self: *Writer, root: LirProgram.ConstPlanId) Allocator.Error!bool {
        const Frame = struct {
            id: LirProgram.ConstPlanId,
            children: []const LirProgram.ConstPlanId,
            index: usize = 0,
            /// A tag union's variants, whose payloads are its components,
            /// one variant's after another's.
            variants: []const LirProgram.ConstTagVariant = &.{},
            variant: usize = 0,
        };
        var frames = std.ArrayList(Frame).empty;
        defer frames.deinit(self.allocator);
        // The answer of the plan just entered or finished; null when entering
        // it pushed a frame.
        var answer = try self.enterProductPlan(&frames, root);
        while (frames.items.len > 0) {
            const top = &frames.items[frames.items.len - 1];
            const failed = if (answer) |child_answer| !child_answer else false;
            if (!failed and top.index == top.children.len and top.variant < top.variants.len) {
                top.children = top.variants[top.variant].payloads;
                top.index = 0;
                top.variant += 1;
                answer = null;
                continue;
            }
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
            // A tag union's payloads are its components. Its layout decides
            // whether it packs: only one whose payloads are all zero-sized
            // is its discriminant alone (`productPlan`).
            .tag_union => |variants| {
                try frames.append(self.allocator, .{ .id = id, .children = &.{}, .variants = variants });
                return null;
            },
            .pending, .layout_only => writerInvariant("product eligibility reached an unfinished const plan"),
            .str, .list, .box, .fn_value, .erased_fn, .boxy_fn => return false,
        };
        try frames.append(self.allocator, .{ .id = id, .children = children });
        return null;
    }

    fn productPlan(self: *Writer, id: LirProgram.ConstPlanId, idx: layout.Idx) Allocator.Error!?*const lir.PackedData.Plan {
        const key = ProductPlanKey{ .plan = id, .layout_idx = idx };
        if (self.product_plans.getPtr(key)) |entry| return if (entry.*) |*plan| plan else null;
        const plan: ?lir.PackedData.Plan = if (try self.planIsProduct(id) and try lir.PackedData.Plan.layoutPacks(self.allocator, &self.program.layouts, idx))
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

    /// Store a list of `len` products of plan `elem_plan` at `elem_layout`
    /// whose elements lie in `memory` as the evaluation's memory holds them.
    fn storeProductList(
        self: *Writer,
        node: checked.ConstNodeId,
        elem_plan: LirProgram.ConstPlanId,
        elem_layout: layout.Idx,
        plan: *const lir.PackedData.Plan,
        memory: []const u8,
        len: usize,
        capacity: u64,
    ) Allocator.Error!void {
        const byte_len = std.math.mul(usize, len, plan.packed_width) catch unreachable; // packed leaves are disjoint parts of an element, so this is at most memory_len
        const data = if (plan.memoryIsCanonical(builtin.cpu.arch.endian()))
            try self.store.addBlobData(memory)
        else blk: {
            plan.encode(try self.store.reserveBlobData(byte_len), memory, len, builtin.cpu.arch.endian());
            break :blk try self.store.commitBlobData(byte_len);
        };
        if (builtin.mode == .debug) try self.assertPackedTags(elem_plan, elem_layout, plan, self.store.blobData(data), memory, len);
        self.store.fill(node, .{ .list = .{ .items = .{ .packed_bytes = .{
            .bytes = data,
            .len = try checkedU32(len),
            .element = null,
            .product_width = plan.packed_width,
        } }, .capacity = capacity } });
    }

    /// Every discriminant-only tag of the `len` products `memory` holds is
    /// packed in `canonical` as the index, in byte-wise name order, of the
    /// variant storing the item on its own selects (`selectTagVariant`).
    fn assertPackedTags(
        self: *Writer,
        elem_plan: LirProgram.ConstPlanId,
        elem_layout: layout.Idx,
        plan: *const lir.PackedData.Plan,
        canonical: []const u8,
        memory: []const u8,
        len: usize,
    ) Allocator.Error!void {
        if (plan.tags.items.len == 0) return;
        const TagPart = struct { variants: []const LirProgram.ConstTagVariant, layout_idx: layout.Idx, offset: usize };
        var tags = std.ArrayList(TagPart).empty;
        defer tags.deinit(self.allocator);
        // The plan's tag unions in the order the packing plan's leaves are:
        // components in plan order, which is their fields' original order.
        const Pending = struct { plan: LirProgram.ConstPlanId, layout_idx: layout.Idx, offset: usize };
        var pending = std.ArrayList(Pending).empty;
        defer pending.deinit(self.allocator);
        try pending.append(self.allocator, .{ .plan = elem_plan, .layout_idx = elem_layout, .offset = 0 });
        while (pending.pop()) |part| {
            const layout_value = self.program.layouts.getLayout(part.layout_idx);
            if (layout_value.tag == .zst) continue;
            switch (self.constPlan(part.plan).*) {
                .scalar, .zst => {},
                .named => |named| try pending.append(self.allocator, .{ .plan = named.backing, .layout_idx = part.layout_idx, .offset = part.offset }),
                .tag_union => |variants| try tags.append(self.allocator, .{ .variants = variants, .layout_idx = part.layout_idx, .offset = part.offset }),
                .tuple, .record => |components| {
                    const struct_idx = layout_value.getStruct().idx;
                    var index = components.len;
                    while (index > 0) {
                        index -= 1;
                        try pending.append(self.allocator, .{
                            .plan = components[index],
                            .layout_idx = self.program.layouts.getStructFieldLayoutByOriginalIndex(struct_idx, @intCast(index)),
                            .offset = part.offset + self.program.layouts.getStructFieldOffsetByOriginalIndex(struct_idx, @intCast(index)),
                        });
                    }
                },
                .pending, .layout_only, .str, .list, .box, .fn_value, .erased_fn, .boxy_fn => writerInvariant("a packed product plan holds a part other than a number, discriminant-only tag union, record, tuple or named type"),
            }
        }
        std.debug.assert(tags.items.len == plan.tags.items.len);
        for (tags.items, plan.tags.items) |tag, leaf| for (0..len) |item| {
            const selected = self.selectTagVariant(tag.variants, tag.layout_idx, .{ .ptr = @constCast(memory[item * plan.memory_width + tag.offset ..].ptr) });
            var rank: u64 = 0;
            for (tag.variants) |variant| {
                if (std.mem.order(u8, variant.name, selected.name) == .lt) rank += 1;
            }
            var index: u64 = 0;
            for (canonical[item * plan.packed_width + leaf.packed_offset ..][0..leaf.width], 0..) |byte, shift| index |= @as(u64, byte) << @intCast(shift * 8);
            std.debug.assert(index == rank);
        };
    }

    fn planIsScalar(self: *const Writer, root: LirProgram.ConstPlanId) bool {
        var plan_id = root;
        while (true) return switch (self.constPlan(plan_id).*) {
            .scalar => true,
            .named => |named| {
                plan_id = named.backing;
                continue;
            },
            .pending,
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
            .boxy_fn,
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
        value: Value,
        capacity: u64,
    ) Error!void {
        if (list_layout.tag != .list) writerInvariant("packed scalar list had non-list layout");
        const elem_layout = list_layout.getIdx();
        const element = self.packedScalarForLayout(elem_layout) orelse
            writerInvariant("scalar const plan had a non-packable list element layout");
        const roc_list: *const RocList = @ptrCast(@alignCast(value.ptr));
        if (roc_list.bytes == null and roc_list.len() != 0) {
            writerInvariant("non-empty packed list had null element pointer");
        }
        self.store.fill(target_node, .{ .list = .{ .items = .{ .packed_bytes = .{
            .bytes = try self.packMemory(elem_layout, element, roc_list.bytes, roc_list.len()),
            .len = try checkedU32(roc_list.len()),
            .element = element,
        } }, .capacity = capacity } });
    }

    /// The interned packed bytes of the `len` scalars of `element` at
    /// `elem_layout` that a list's memory at `source` holds.
    fn packMemory(
        self: *Writer,
        elem_layout: layout.Idx,
        element: const_store.ConstPackedScalar,
        source: ?[*]const u8,
        len: usize,
    ) Allocator.Error!const_store.ConstStr {
        const width: usize = element.byteWidth();
        if (builtin.mode == .debug) std.debug.assert(self.program.layouts.layoutSize(self.program.layouts.getLayout(elem_layout)) == width);
        const byte_len = std.math.mul(usize, len, width) catch
            writerInvariant("packed list byte length overflowed");
        const bytes = try self.store.reserveBlobData(byte_len);
        if (source) |memory| switch (builtin.cpu.arch.endian()) {
            .little => {
                packMemoryScalars(element, bytes, memory[0..byte_len]);
                // The list's bytes are what storing each item on its own
                // writes.
                if (builtin.mode == .debug) for (0..len) |index| {
                    var item: [16]u8 = undefined;
                    writePackedScalar(item[0..width], element, self.storeScalar(elem_layout, .{ .ptr = @constCast(memory + index * width) }));
                    for (item[0..width], bytes[index * width ..][0..width]) |expected, stored| std.debug.assert(expected == stored);
                };
            },
            // A big-endian program's memory holds no scalar wider than a
            // byte as its packed little-endian bytes, so each item is
            // packed on its own.
            .big => for (0..len) |index| {
                const scalar = self.storeScalar(elem_layout, .{ .ptr = @constCast(memory + index * width) });
                writePackedScalar(bytes[index * width ..][0..width], element, scalar);
            },
        };
        return try self.store.commitBlobData(byte_len);
    }

    fn selectFnVariant(
        self: *Writer,
        set: *const LirProgram.FnSet,
        layout_idx: layout.Idx,
        value: Value,
    ) *const LirProgram.FnVariant {
        if (set.variants.len == 1) return &set.variants[0];
        const discriminant = self.readTagDiscriminant(layout_idx, value);
        for (set.variants) |*variant| {
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
            .hole => writerInvariant("a demand hole reached a compile-time value"),
            .dynamic => writerInvariant("a dynamic value reached a compile-time value"),
            .zst => 0,
            .tag_union => self.program.layouts.getTagUnionData(layout_value.getTagUnion().idx).readDiscriminant(value.ptr, self.program.layouts.targetUsize()),
            .scalar,
            .box,
            .box_of_zst,
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
            .hole => writerInvariant("a demand hole reached a compile-time value"),
            .dynamic => writerInvariant("a dynamic value reached a compile-time value"),
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
            .hole => writerInvariant("a demand hole reached a compile-time value"),
            .dynamic => writerInvariant("a dynamic value reached a compile-time value"),
            .box_of_zst,
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
            .capacity = switch (layout_value.tag) {
                .list => listCapacity(value),
                .scalar => if (layout_value.getScalar().tag == .str) strCapacity(@ptrCast(@alignCast(value.ptr))) else 0,
                .hole, .dynamic, .box, .box_of_zst, .list_of_zst, .struct_, .closure, .erased_callable, .zst, .tag_union, .ptr => 0,
            },
            .len = switch (layout_value.tag) {
                .list => blk: {
                    const roc_list: *const RocList = @ptrCast(@alignCast(value.ptr));
                    break :blk roc_list.len();
                },
                .scalar => if (layout_value.getScalar().tag == .str) blk: {
                    const roc_str: *const RocStr = @ptrCast(@alignCast(value.ptr));
                    break :blk roc_str.len();
                } else 0,
                .hole => writerInvariant("a demand hole reached a compile-time value"),
                .dynamic => writerInvariant("a dynamic value reached a compile-time value"),
                .box,
                .box_of_zst,
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

    fn constPlan(self: *const Writer, id: LirProgram.ConstPlanId) *const LirProgram.ConstPlan {
        const raw = @backingInt(id);
        if (raw >= self.program.const_plans.items.len) writerInvariant("const plan id is out of range");
        return &self.program.const_plans.items[raw];
    }
};

fn interpreterErasedCallable(_: ?*anyopaque, data_ptr: [*]u8) error{RuntimeError}!ErasedCallableResolution {
    return .{
        .proc = Interpreter.erasedCallableInterpreterProcId(data_ptr),
        .capture_ptr = Interpreter.erasedCallableInterpreterCaptureValuePtr(data_ptr),
    };
}

/// Pack the scalars of `element` that a little-endian program's memory holds
/// in `source` into `out`, as `writePackedScalar` packs each one. Every
/// packed scalar is its little-endian bytes, so the bytes are copied as they
/// are, a float's with every NaN made Roc's one NaN as it is copied.
fn packMemoryScalars(element: const_store.ConstPackedScalar, out: []u8, source: []const u8) void {
    if (builtin.mode == .debug) std.debug.assert(builtin.cpu.arch.endian() == .little and out.len == source.len);
    switch (element) {
        .f32 => builtins.float_bits.copyNormalizingF32NanBits(out, source),
        .f64 => builtins.float_bits.copyNormalizingF64NanBits(out, source),
        .u8, .i8, .u16, .i16, .u32, .i32, .u64, .i64, .u128, .i128, .dec, .u8x16, .i8x16, .u16x8, .i16x8, .u32x4, .i32x4, .u64x2, .i64x2 => @memcpy(out, source),
    }
}

/// A value-form root written as its image (`Writer.storeCheckedRoot`): each
/// part at the 64-bit layout of its type, as `NativeRootExport.imageRoot`
/// writes an evaluation of the same expression, each list's items in an
/// allocation of their own, and a part reading a constant the constant's
/// stored image, copied through the part's plan as an evaluation's memory
/// is.
const ValueFormImage = struct {
    writer: *Writer,
    graph: NativeRootExport.ImageGraph,
    jobs: std.ArrayList(Job) = .empty,
    /// Distinct allocation keys: every allocation of a value-form root is
    /// its own, until the finished image merges those of equal content.
    allocations: usize = 0,

    const Job = struct {
        plan: LirProgram.ConstPlanId,
        layout_idx: layout.Idx,
        part: Part,
        dest: frozen_graph.Destination,
    };

    fn deinit(self: *ValueFormImage) void {
        self.jobs.deinit(self.writer.allocator);
        self.graph.deinit();
    }

    fn layouts(self: *const ValueFormImage) *const layout.Store {
        return &self.writer.program.layouts;
    }

    /// Write value-form expression `expr` of plan `plan` at `layout_idx`.
    fn build(self: *ValueFormImage, plan: LirProgram.ConstPlanId, layout_idx: layout.Idx, expr: CIR.Expr.Idx) Error!NativeRootExport.ImageGraph.Result {
        const root = try self.graph.root(self.graph.size(layout_idx), self.graph.alignment(layout_idx));
        try self.jobs.append(self.writer.allocator, .{ .plan = plan, .layout_idx = layout_idx, .part = checkedPart(expr), .dest = root });
        while (self.jobs.pop()) |job| try self.write(job);
        return try self.graph.finish();
    }

    /// A new allocation of `size` bytes at `alignment` the word at `dest`
    /// points at.
    fn allocation(self: *ValueFormImage, dest: frozen_graph.Destination, plan: LirProgram.ConstPlanId, layout_idx: layout.Idx, size: usize, alignment: u32, contains_refcounted: bool, element_count: ?usize) Allocator.Error!frozen_graph.Destination {
        self.allocations += 1;
        const reserved = try self.graph.reserveAllocation(.{ .address = self.allocations, .plan = plan, .layout_idx = layout_idx, .count = element_count orelse 1, .kind = .value }, size, alignment, contains_refcounted, element_count);
        try self.graph.relocate(dest, reserved.dest);
        return reserved.dest;
    }

    fn write(self: *ValueFormImage, start: Job) Error!void {
        const writer = self.writer;
        const layouts_ = self.layouts();
        var job = start;
        job.part = writer.resolvePart(job.part);
        const plan = writer.constPlan(job.plan);
        // A recursive type's back edge is a box the committed layout
        // inserted, which holds the part's own bytes.
        const physical = layouts_.getLayout(job.layout_idx);
        if (physical.tag == .box and plan.* != .box) {
            const payload_layout = physical.getIdx();
            job.dest = try self.allocation(job.dest, job.plan, payload_layout, self.graph.size(payload_layout), self.graph.alignment(payload_layout), layouts_.layoutContainsRefcounted(layouts_.getLayout(payload_layout)), null);
            job.layout_idx = payload_layout;
        }
        const size = self.graph.size(job.layout_idx);
        if (size == 0) return;
        const expr = switch (job.part) {
            .checked => |expr| expr,
            .stored => |stored| return try self.copyStored(stored, job.plan, job.layout_idx, job.dest, size),
        };
        const word = self.graph.word();
        switch (plan.*) {
            .pending, .layout_only => writerInvariant("an incomplete const plan reached a value-form root"),
            .fn_value, .erased_fn, .boxy_fn => writerInvariant("a value-form root holds a function"),
            // Value form builds no box.
            .box => writerInvariant("a value-form root holds a box"),
            .zst => {},
            .scalar => {
                const bytes = try writer.checkedScalarBytes(job.layout_idx, expr);
                @memcpy(self.graph.bytes(job.dest, size), bytes[0..size]);
            },
            .str => {
                // A literal has no capacity beyond its bytes (design.md
                // "Static Lists And Strings Keep Their Capacity").
                const text = try writer.checkedStrText(expr);
                if (text.len < size) {
                    const descriptor = self.graph.bytes(job.dest, size);
                    @memcpy(descriptor[0..text.len], text);
                    descriptor[size - 1] = RocStr.smallStrFlagByte(text.len);
                    return;
                }
                const bytes = try self.allocation(job.dest, job.plan, job.layout_idx, text.len, 1, false, null);
                @memcpy(self.graph.bytes(bytes, text.len), text);
                // `RocStr`'s words: bytes, capacity, length.
                try self.graph.writeWord(job.dest.offsetBy(word), RocStr.encodeCapacityForWidth(text.len));
                try self.graph.writeWord(job.dest.offsetBy(2 * word), text.len);
            },
            .list => |elem_plan| {
                const items = writer.checkedListItems(expr);
                // `RocList`'s words: bytes, length, capacity.
                try self.graph.writeWord(job.dest.offsetBy(word), items.len);
                try self.graph.writeWord(job.dest.offsetBy(2 * word), RocList.encodeCapacityForWidth(items.len));
                if (items.len == 0 or physical.tag == .list_of_zst) return;
                const elem_layout = physical.getIdx();
                const elem_size = self.graph.size(elem_layout);
                const elements = try self.allocation(
                    job.dest,
                    elem_plan,
                    elem_layout,
                    std.math.mul(usize, elem_size, items.len) catch return error.OutOfMemory,
                    self.graph.alignment(elem_layout),
                    layouts_.layoutContainsRefcounted(layouts_.getLayout(elem_layout)),
                    items.len,
                );
                try self.jobs.ensureUnusedCapacity(writer.allocator, items.len);
                var index = items.len;
                while (index > 0) {
                    index -= 1;
                    self.jobs.appendAssumeCapacity(.{ .plan = elem_plan, .layout_idx = elem_layout, .part = checkedPart(items[index]), .dest = elements.offsetBy(index * elem_size) });
                }
            },
            .tuple, .record => |components| {
                const struct_layout = layouts_.getLayout(job.layout_idx);
                if (struct_layout.tag != .struct_) writerInvariant("a product's layout is not a struct");
                const parts = try writer.checkedComponents(job.plan, expr, components.len);
                try self.pushFields(components, parts, struct_layout, job.dest);
            },
            .tag_union => |variants| {
                const bodies = writer.checkedValues().bodies;
                const data = bodies.exprData(checked.valueFormValueExpr(bodies, expr));
                const name: []const u8, const args: []const CIR.Expr.Idx = if (data == .tag)
                    .{ bodies.names.tagLabelText(bodies.tagLabel(data.tag.name)), data.tag.args }
                else if (data == .zero_argument_tag)
                    .{ bodies.names.tagLabelText(bodies.tagLabel(data.zero_argument_tag.name)), &.{} }
                else
                    writerInvariant("a value-form tag union value is not a tag");
                const variant = variants[try writer.checkedValues().schema.tagVariant(job.plan, name)];
                if (variant.payloads.len != args.len) writerInvariant("a value-form tag does not have its variant's payloads");
                switch (physical.tag) {
                    // A union of one tag holds no discriminant.
                    .zst => if (builtin.mode == .debug) std.debug.assert(variant.discriminant == 0),
                    .tag_union => layouts_.getTagUnionData(physical.getTagUnion().idx).writeDiscriminant(self.graph.bytes(job.dest, size).ptr, variant.discriminant, .u64),
                    .hole, .dynamic, .scalar, .box, .box_of_zst, .list, .list_of_zst, .struct_, .closure, .erased_callable, .ptr => writerInvariant("a value-form tag's layout is not a tag union"),
                }
                if (args.len == 0) return;
                const payload_layout = writer.tagPayloadLayout(job.layout_idx, variant.discriminant);
                if (self.graph.size(payload_layout) == 0) return;
                if (args.len == 1) {
                    return try self.jobs.append(writer.allocator, .{ .plan = variant.payloads[0], .layout_idx = payload_layout, .part = checkedPart(args[0]), .dest = job.dest });
                }
                const payload_struct = layouts_.getLayout(payload_layout);
                if (payload_struct.tag != .struct_) writerInvariant("multi-payload tag did not use a struct payload layout");
                try self.pushFields(variant.payloads, args, payload_struct, job.dest);
            },
            .named => |named| try self.jobs.append(writer.allocator, .{ .plan = named.backing, .layout_idx = job.layout_idx, .part = checkedPart(writer.checkedBacking(expr)), .dest = job.dest }),
        }
    }

    /// Push the components `parts` of plans `plans` of the struct
    /// `struct_layout` at `dest`, first on top.
    fn pushFields(self: *ValueFormImage, plans: []const LirProgram.ConstPlanId, parts: []const CIR.Expr.Idx, struct_layout: layout.Layout, dest: frozen_graph.Destination) Allocator.Error!void {
        const layouts_ = self.layouts();
        const struct_idx = struct_layout.getStruct().idx;
        try self.jobs.ensureUnusedCapacity(self.writer.allocator, plans.len);
        var index = plans.len;
        while (index > 0) {
            index -= 1;
            self.jobs.appendAssumeCapacity(.{
                .plan = plans[index],
                .layout_idx = layouts_.getStructFieldLayoutByOriginalIndex(struct_idx, @intCast(index)),
                .part = checkedPart(parts[index]),
                .dest = dest.offsetBy(layouts_.getStructFieldOffsetByOriginalIndexAt(struct_idx, @intCast(index), .u64)),
            });
        }
    }

    /// Write the value of the constant `stored` holds, an image, at `dest`
    /// at `layout_idx`: its root's bytes, pointing into the image's block,
    /// which is embedded once.
    fn copyStored(self: *ValueFormImage, stored: StoredPart, plan: LirProgram.ConstPlanId, layout_idx: layout.Idx, dest: frozen_graph.Destination, size: usize) Error!void {
        const image_id = stored.store.imageOf(stored.node) orelse writerInvariant("a constant a value-form root reads is not stored as an image");
        const image = stored.store.image(image_id);
        if (builtin.mode == .debug) {
            const own = try self.writer.layoutDigest(layout_idx);
            if (!std.meta.eql(own, image.layout_digest)) writerInvariant("a value-form root reads a constant at another layout than its image's");
        }
        if (image.root_size != size) writerInvariant("a constant's image differs in size from the part reading it");
        // An image that points nowhere is its root's bytes alone.
        if (image.pointers.len == 0) return @memcpy(self.graph.bytes(dest, size), image.bytes[0..size]);
        // Otherwise the image, relocated into memory, is copied part by part
        // through the reading part's plan, as an evaluation reading the
        // constant leaves it to be copied: each allocation at its own place.
        const memory = try self.writer.allocator.alignedAlloc(u8, .fromByteUnits(max_image_alignment), image.bytes.len);
        defer self.writer.allocator.free(memory);
        if (image.alignment > max_image_alignment) writerInvariant("a stored image is aligned beyond any layout's alignment");
        @memcpy(memory, image.bytes);
        for (image.pointers) |offset| {
            const word = memory[offset..][0..8];
            std.mem.writeInt(u64, word, @intFromPtr(memory.ptr) + std.mem.readInt(u64, word, .little), .little);
        }
        try NativeRootExport.imagePart(&self.graph, plan, layout_idx, .{ .ptr = memory.ptr }, dest);
    }

    /// The largest alignment of any layout, and so of any image.
    const max_image_alignment = 16;
};

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

/// `value` as a ConstStore width. A value past it is a constant larger than
/// the store holds, which the evaluated program could reach.
fn checkedU32(value: usize) Allocator.Error!u32 {
    return std.math.cast(u32, value) orelse error.OutOfMemory;
}

/// The segments of value-form string `expr`.
fn checkedStrSegments(bodies: checked.CheckedBodies, expr: CIR.Expr.Idx) []const CIR.Expr.Idx {
    const data = bodies.exprData(checked.valueFormValueExpr(bodies, expr));
    if (data != .str) writerInvariant("a value-form string is not a string literal");
    return data.str;
}

/// A part of value-form expression `expr`.
fn checkedPart(expr: CIR.Expr.Idx) Part {
    return .{ .checked = expr };
}

/// Write discriminant `discriminant` of the tag union at `layout_idx` into
/// the memory at `ptr` a value of it occupies.
fn writeDiscriminant(layouts: *const layout.Store, ptr: [*]u8, layout_idx: layout.Idx, discriminant: u32) void {
    const layout_value = layouts.getLayout(layout_idx);
    switch (layout_value.tag) {
        // A union of one tag holds no discriminant.
        .zst => if (builtin.mode == .debug) std.debug.assert(discriminant == 0),
        .tag_union => layouts.getTagUnionData(layout_value.getTagUnion().idx).writeDiscriminant(ptr, discriminant, layouts.targetUsize()),
        .hole, .dynamic, .scalar, .box, .box_of_zst, .list, .list_of_zst, .struct_, .closure, .erased_callable, .ptr => writerInvariant("a discriminant-only tag's layout is not a tag union"),
    }
}

fn writerInvariant(comptime message: []const u8) noreturn {
    base.invariant("ConstStore writer invariant violated: {s}", .{message});
}

test "const store writer declarations are referenced" {
    std.testing.refAllDecls(@This());
}

test "strategy root readiness classifies nested producer plans with allocation failure" {
    const allocator = std.testing.allocator;
    var program = try LirProgram.Result.init(allocator, .native);
    defer program.deinit();
    const leaf: LirProgram.ConstPlanId = @fromBackingInt(@intCast(program.const_plans.items.len));
    try program.const_plans.append(allocator, .scalar);
    const recursive: LirProgram.ConstPlanId = @fromBackingInt(@intCast(program.const_plans.items.len));
    try program.const_plans.append(allocator, .pending);
    const aggregate: LirProgram.ConstPlanId = @fromBackingInt(@intCast(program.const_plans.items.len));
    const children = try allocator.dupe(LirProgram.ConstPlanId, &.{ leaf, recursive, leaf });
    program.const_plans.append(allocator, .{ .record = children }) catch |err| {
        allocator.free(children);
        return err;
    };
    program.const_plans.items[@backingInt(recursive)] = .{ .list = aggregate };
    // The classification consumes plan tags/edges, never callable-set ids.
    // The integration test exercises actual callable sets produced by LSS.
    for ([_]struct { plan: LirProgram.ConstPlan, strategy: ?base.SpecializationStrategy }{
        .{ .plan = .{ .fn_value = undefined }, .strategy = .lss },
        .{ .plan = .{ .erased_fn = undefined }, .strategy = .lss },
        .{ .plan = .boxy_fn, .strategy = .boxy },
        .{ .plan = .scalar, .strategy = null },
    }) |case| {
        program.const_plans.items[@backingInt(leaf)] = case.plan;
        try std.testing.checkAllAllocationFailures(allocator, expectPlanFunctionStrategy, .{ &program, aggregate, case.strategy });
    }
}

fn expectPlanFunctionStrategy(allocator: Allocator, program: *const LirProgram.Result, root: LirProgram.ConstPlanId, expected: ?base.SpecializationStrategy) (Allocator.Error || error{TestExpectedEqual})!void {
    var store = const_store.ConstStore.init(std.testing.allocator);
    defer store.deinit();
    var writer = Writer.initStore(allocator, &store, null, program);
    defer writer.deinit();
    try std.testing.expectEqual(expected, try writer.planFunctionStrategy(root));
}

fn initTestArtifact(allocator: Allocator, module_env: *can.ModuleEnv) Allocator.Error!checked.CheckedModuleArtifact {
    var names = check.CanonicalNames.CanonicalNameStore.init(allocator);
    const module_name = try names.internModuleName("Test");
    return .{
        .key = .{},
        .source_hash = @as([32]u8, @splat(0)),
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

fn testConstRoot(plan: LirProgram.ConstPlanId, ret_layout: layout.Idx, ret_type: const_store.ConstTypeId) LirProgram.ConstRootPlan {
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
        .ret_type = ret_type,
        .plan = plan,
    };
}

/// Store `value` of `plan` at `ret_layout` as nodes, as a part of a value
/// holding a function is stored.
fn testStoreNodes(writer: *Writer, plan: LirProgram.ConstPlanId, ret_layout: layout.Idx, value: Value) Error!checked.CompileTimeRootPayload {
    writer.stored_values.clearRetainingCapacity();
    return .{ .const_node = try writer.storeValue(plan, ret_layout, value) };
}

/// A stored type `ty` of `program`, for a test root.
fn testRootType(program: *LirProgram.Result, ty: const_store.ConstType) Allocator.Error!const_store.ConstTypeId {
    const id = try program.const_types.reserve();
    try program.const_types.fill(id, ty);
    return id;
}

/// The image the payload `stored` of a data root holds.
fn testImage(artifact: *const checked.CheckedModuleArtifact, stored: checked.CompileTimeRootPayload) const_store.ConstImageView {
    return artifact.const_store.image(artifact.const_store.imageOf(stored.const_node).?);
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

    const root = testConstRoot(str_plan, .str, try testRootType(&program, .{ .primitive = .str }));

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
    try testing.expectEqualStrings(first_bytes, testImage(&artifact, first).strAt(0));
    try testing.expectEqualStrings(second_bytes, testImage(&artifact, second).strAt(0));
}

test "const store writer stores each string's own bytes when strings share an allocation" {
    const testing = std.testing;

    var module_env = try can.ModuleEnv.init(testing.allocator, "");
    defer module_env.deinit();

    var artifact = try initTestArtifact(testing.allocator, &module_env);
    defer deinitTestArtifact(&artifact, testing.allocator);

    var program = try LirProgram.Result.init(testing.allocator, .u64);
    defer program.deinit();

    const str_plan: LirProgram.ConstPlanId = @fromBackingInt(@intCast(program.const_plans.items.len));
    try program.const_plans.append(testing.allocator, .str);
    const list_plan: LirProgram.ConstPlanId = @fromBackingInt(@intCast(program.const_plans.items.len));
    try program.const_plans.append(testing.allocator, .{ .list = str_plan });
    const str_list_layout = try program.layouts.insertLayout(layout.Layout.list(.str));
    const str_type = try testRootType(&program, .{ .primitive = .str });
    const list_type = try testRootType(&program, .{ .list = str_type });

    var writer = Writer.init(testing.allocator, &artifact, &program);
    defer writer.deinit();

    const text = "a,bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb";
    const bytes = try testing.allocator.alignedAlloc(u8, .of(usize), text.len);
    defer testing.allocator.free(bytes);
    @memcpy(bytes, text);

    // The pieces as `Str.split_on` leaves them: the first keeps the whole
    // allocation as its capacity with a shortened length, and the second is
    // a seamless slice of bytes past that length.
    var pieces = [_]RocStr{
        .{ .bytes = bytes.ptr, .capacity_or_alloc_ptr = RocStr.encodeCapacity(bytes.len), .length = 1 },
        .{ .bytes = bytes.ptr + 2, .capacity_or_alloc_ptr = RocStr.encodeSliceAllocationPtr(bytes.ptr), .length = bytes.len - 2 },
    };
    var roc_list = RocList{
        .bytes = @ptrCast(&pieces),
        .length = pieces.len,
        .capacity_or_alloc_ptr = RocList.encodeCapacity(pieces.len),
    };
    const stored = try writer.storeRoot(testConstRoot(list_plan, str_list_layout, list_type), .{ .ptr = @ptrCast(&roc_list) });

    const image = testImage(&artifact, stored);
    const word = const_store.image_word_size;
    try testing.expectEqual(@as(u64, 2), std.mem.readInt(u64, image.bytes[word..][0..word], .little));
    const items: u32 = @intCast(std.mem.readInt(u64, image.bytes[0..word], .little));
    const expected = [_][]const u8{ "a", text[2..] };
    for (expected, 0..) |expected_piece, index| {
        try testing.expectEqualStrings(expected_piece, image.strAt(items + @as(u32, @intCast(index * 3 * word))));
    }
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
    const f64_type = try testRootType(&program, .{ .primitive = .f64 });
    const f32_type = try testRootType(&program, .{ .primitive = .f32 });

    var writer = Writer.init(testing.allocator, &artifact, &program);
    defer writer.deinit();

    var f64_payload: u64 = 0xfff9_2345_6789_abcd;
    const stored_f64 = try writer.storeRoot(testConstRoot(scalar_plan, .f64, f64_type), .{ .ptr = @ptrCast(&f64_payload) });
    try testing.expectEqual(builtins.float_bits.normalized_f64_nan_bits, std.mem.readInt(u64, testImage(&artifact, stored_f64).rootBytes()[0..8], .little));

    var f32_payload: u32 = 0xffc1_2345;
    const stored_f32 = try writer.storeRoot(testConstRoot(scalar_plan, .f32, f32_type), .{ .ptr = @ptrCast(&f32_payload) });
    try testing.expectEqual(builtins.float_bits.normalized_f32_nan_bits, std.mem.readInt(u32, testImage(&artifact, stored_f32).rootBytes()[0..4], .little));

    var finite: u64 = @bitCast(@as(f64, -2.5));
    const stored_finite = try writer.storeRoot(testConstRoot(scalar_plan, .f64, f64_type), .{ .ptr = @ptrCast(&finite) });
    try testing.expectEqual(finite, std.mem.readInt(u64, testImage(&artifact, stored_finite).rootBytes()[0..8], .little));
}

// Repro for https://github.com/roc-lang/roc/issues/10177
test "const store writer stores a 20KB scalar list's items once in its image" {
    const testing = std.testing;

    var module_env = try can.ModuleEnv.init(testing.allocator, "");
    defer module_env.deinit();

    var artifact = try initTestArtifact(testing.allocator, &module_env);
    defer deinitTestArtifact(&artifact, testing.allocator);

    var program = try LirProgram.Result.init(testing.allocator, .u64);
    defer program.deinit();

    const scalar_plan: LirProgram.ConstPlanId = @fromBackingInt(@intCast(program.const_plans.items.len));
    try program.const_plans.append(testing.allocator, .scalar);
    const list_plan: LirProgram.ConstPlanId = @fromBackingInt(@intCast(program.const_plans.items.len));
    try program.const_plans.append(testing.allocator, .{ .list = scalar_plan });
    const u8_list_layout = try program.layouts.insertLayout(layout.Layout.list(.u8));
    const u8_list_type = try testRootType(&program, .{ .list = try testRootType(&program, .{ .primitive = .u8 }) });

    var writer = Writer.init(testing.allocator, &artifact, &program);
    defer writer.deinit();

    const bytes = try testing.allocator.alloc(u8, 20 * 1024);
    defer testing.allocator.free(bytes);
    @memset(bytes, 'A');

    var roc_list = RocList{
        .bytes = bytes.ptr,
        .length = bytes.len,
        .capacity_or_alloc_ptr = RocList.encodeCapacity(bytes.len),
    };
    const image = testImage(&artifact, try writer.storeRoot(testConstRoot(list_plan, u8_list_layout, u8_list_type), .{ .ptr = @ptrCast(&roc_list) }));
    const word = const_store.image_word_size;
    // The descriptor, then the items behind one static header word.
    try testing.expectEqual(@as(usize, 4 * word + bytes.len), image.bytes.len);
    const items: usize = @intCast(std.mem.readInt(u64, image.bytes[0..word], .little));
    try testing.expectEqualSlices(u8, bytes, image.bytes[items..][0..bytes.len]);

    // An empty list keeps the capacity it was evaluated with.
    var roc_empty_list = RocList{
        .bytes = bytes.ptr,
        .length = 0,
        .capacity_or_alloc_ptr = RocList.encodeCapacity(bytes.len),
    };
    const empty = testImage(&artifact, try writer.storeRoot(testConstRoot(list_plan, u8_list_layout, u8_list_type), .{ .ptr = @ptrCast(&roc_empty_list) }));
    try testing.expectEqual(@as(u64, bytes.len), builtins.list.RocList.decodeCapacityForWidth(std.mem.readInt(u64, empty.bytes[2 * word ..][0..word], .little)));
}

/// Publish a real pending frame with an independently owned recipe, without
/// needing a lowered callable to exercise the writer's ownership lifecycle.
fn pushTestBoxyRecipe(writer: *Writer, node: checked.ConstNodeId, bytes: []const u8) Allocator.Error!void {
    const recipe = try writer.allocator.dupe(u8, bytes);
    errdefer writer.allocator.free(recipe);
    try writer.pushStoreFrame(.{ .boxy_fn = .{ .node = node, .recipe = recipe } }, writer.store_requests.items.len, &.{});
}

test "const store writer boxy recipe ownership clears pending frames and deinitializes" {
    const a = std.testing.allocator;
    var program = try LirProgram.Result.init(a, .u64);
    defer program.deinit();
    var store = const_store.ConstStore.init(a);
    defer store.deinit();
    const node = try store.reserve();
    for ([_]bool{ false, true }) |explicit_clear| {
        var tracking = std.testing.FailingAllocator.init(a, .{});
        {
            var writer = Writer.initStore(tracking.allocator(), &store, null, &program);
            defer writer.deinit();
            try pushTestBoxyRecipe(&writer, node, "parent");
            try writer.pushStoreFrame(.{ .record = node }, 0, &.{});
            try pushTestBoxyRecipe(&writer, node, "child");
            if (explicit_clear) {
                const freed = tracking.freed_bytes;
                writer.clearStoreStacks();
                try std.testing.expectEqual(@as(usize, 0), writer.store_frames.items.len);
                try std.testing.expectEqual(freed + "parent".len + "child".len, tracking.freed_bytes);
                writer.clearStoreStacks();
                try std.testing.expectEqual(freed + "parent".len + "child".len, tracking.freed_bytes);
            }
        }
        try std.testing.expectEqual(tracking.allocated_bytes, tracking.freed_bytes);
    }
}

test "const store writer boxy recipe ownership survives successful frame completion" {
    const a = std.testing.allocator;
    var program = try LirProgram.Result.init(a, .u64);
    defer program.deinit();
    var store = const_store.ConstStore.init(a);
    defer store.deinit();
    const node = try store.reserve();
    var tracking = std.testing.FailingAllocator.init(a, .{});
    {
        var writer = Writer.initStore(tracking.allocator(), &store, null, &program);
        defer writer.deinit();
        try pushTestBoxyRecipe(&writer, node, "owned recipe");
        const freed = tracking.freed_bytes;
        _ = try writer.drainStore();
        try std.testing.expectEqual(@as(usize, 0), writer.store_frames.items.len);
        try std.testing.expectEqualStrings("owned recipe", store.blobData(store.get(node).boxy_fn.recipe));
        try std.testing.expectEqual(freed + "owned recipe".len, tracking.freed_bytes);
        writer.clearStoreStacks();
        try std.testing.expectEqual(freed + "owned recipe".len, tracking.freed_bytes);
    }
    try std.testing.expectEqual(tracking.allocated_bytes, tracking.freed_bytes);
}

test "const store writer boxy recipe ownership frees a failed popped frame only once" {
    const a = std.testing.allocator;
    var program = try LirProgram.Result.init(a, .u64);
    defer program.deinit();
    var failing_store = std.testing.FailingAllocator.init(a, .{});
    var store = const_store.ConstStore.init(failing_store.allocator());
    defer store.deinit();
    const parent = try store.reserve();
    const child = try store.reserve();
    var tracking = std.testing.FailingAllocator.init(a, .{});
    {
        var writer = Writer.initStore(tracking.allocator(), &store, null, &program);
        defer writer.deinit();
        try pushTestBoxyRecipe(&writer, parent, "parent");
        try pushTestBoxyRecipe(&writer, child, "child");
        const freed = tracking.freed_bytes;
        // The child has no remaining requests: drainStore pops it before
        // finishStore tries to intern its recipe into the empty blob index.
        failing_store.fail_index = failing_store.alloc_index;
        failing_store.resize_fail_index = failing_store.resize_index;
        try std.testing.expectError(error.OutOfMemory, writer.drainStore());
        try std.testing.expectEqual(@as(usize, 1), writer.store_frames.items.len);
        try std.testing.expectEqual(freed + "child".len, tracking.freed_bytes);
        writer.clearStoreStacks();
        try std.testing.expectEqual(freed + "child".len + "parent".len, tracking.freed_bytes);
        writer.clearStoreStacks();
        try std.testing.expectEqual(freed + "child".len + "parent".len, tracking.freed_bytes);
    }
    try std.testing.expectEqual(tracking.allocated_bytes, tracking.freed_bytes);
}

test "packing a little-endian float list's memory matches packing each item" {
    if (builtin.cpu.arch.endian() != .little) return error.SkipZigTest;
    const f32_items = [_]u32{
        0x7f80_0001, 0x7fa1_2345, 0x7fc0_0000, 0xff80_0001, 0xffc1_2345, 0xffff_ffff, 0x7f80_0000, 0xff80_0000,
        0x0000_0000, 0x8000_0000, 0x0000_0001, 0x807f_ffff, 0x7f7f_ffff, 0x3f80_0000, 0xbf80_0000, 0x4049_0fdb,
        0x7fbf_ffff, 0xff80_0002, 0x0040_0000, 0x8000_0001, 0x7fc0_0001, 0x4000_0000, 0xc000_0000,
    };
    var f32_memory: [f32_items.len * 4]u8 = undefined;
    for (f32_items, 0..) |bits, index| std.mem.writeInt(u32, f32_memory[index * 4 ..][0..4], bits, .native);
    var f32_packed: [f32_memory.len]u8 = undefined;
    packMemoryScalars(.f32, &f32_packed, &f32_memory);
    var f32_expected: [f32_memory.len]u8 = undefined;
    for (f32_items, 0..) |bits, index| {
        writePackedScalar(f32_expected[index * 4 ..][0..4], .f32, .{ .f32_bits = builtins.float_bits.normalizeF32NanBits(@bitCast(@as(f32, @bitCast(bits)))) });
    }
    try std.testing.expectEqualSlices(u8, &f32_expected, &f32_packed);

    const f64_items = [_]u64{
        0x7ff0_0000_0000_0001, 0x7ff1_2345_6789_abcd, 0x7ff8_0000_0000_0000, 0xfff0_0000_0000_0001,
        0xfff9_2345_6789_abcd, 0xffff_ffff_ffff_ffff, 0x7ff0_0000_0000_0000, 0xfff0_0000_0000_0000,
        0x0000_0000_0000_0000, 0x8000_0000_0000_0000, 0x0000_0000_0000_0001, 0x800f_ffff_ffff_ffff,
        0x7fef_ffff_ffff_ffff, 0x3ff0_0000_0000_0000, 0xbff0_0000_0000_0000,
    };
    var f64_memory: [f64_items.len * 8]u8 = undefined;
    for (f64_items, 0..) |bits, index| std.mem.writeInt(u64, f64_memory[index * 8 ..][0..8], bits, .native);
    var f64_packed: [f64_memory.len]u8 = undefined;
    packMemoryScalars(.f64, &f64_packed, &f64_memory);
    var f64_expected: [f64_memory.len]u8 = undefined;
    for (f64_items, 0..) |bits, index| {
        writePackedScalar(f64_expected[index * 8 ..][0..8], .f64, .{ .f64_bits = builtins.float_bits.normalizeF64NanBits(@bitCast(@as(f64, @bitCast(bits)))) });
    }
    try std.testing.expectEqualSlices(u8, &f64_expected, &f64_packed);
}

test "const store writer stores an evaluated scalar list as storing each item would" {
    const testing = std.testing;
    if (builtin.cpu.arch.endian() != .little) return error.SkipZigTest;

    var module_env = try can.ModuleEnv.init(testing.allocator, "");
    defer module_env.deinit();
    var artifact = try initTestArtifact(testing.allocator, &module_env);
    defer deinitTestArtifact(&artifact, testing.allocator);
    var program = try LirProgram.Result.init(testing.allocator, .u64);
    defer program.deinit();
    const scalar_plan: LirProgram.ConstPlanId = @fromBackingInt(@intCast(program.const_plans.items.len));
    try program.const_plans.append(testing.allocator, .scalar);
    const list_plan: LirProgram.ConstPlanId = @fromBackingInt(@intCast(program.const_plans.items.len));
    try program.const_plans.append(testing.allocator, .{ .list = scalar_plan });

    var writer = Writer.init(testing.allocator, &artifact, &program);
    defer writer.deinit();

    // 67 items cross every whole vector of the float packing and leave a
    // partial one.
    const max_items = 67;
    var memory: [max_items * 16]u8 align(16) = undefined;
    var prng = std.Random.DefaultPrng.init(0x5eed);
    const elems = [_]layout.Idx{ .u8, .i8, .u16, .i16, .u32, .i32, .u64, .i64, .u128, .i128, .f32, .f64, .dec, .u8x16, .i8x16, .u16x8, .i16x8, .u32x4, .i32x4, .u64x2, .i64x2 };
    for (elems) |elem| {
        const list_layout = try program.layouts.insertLayout(layout.Layout.list(elem));
        const element = writer.packedScalarForLayout(elem).?;
        const width: usize = element.byteWidth();
        for ([_]usize{ 0, 1, max_items }) |len| {
            prng.random().bytes(&memory);
            // NaNs with payloads and signs, which a float list stores as
            // Roc's one NaN.
            for (0..len) |index| switch (element) {
                .f32 => if (index % 3 == 0) std.mem.writeInt(u32, memory[index * 4 ..][0..4], 0xff80_0000 | @as(u32, @intCast(index + 1)), .little),
                .f64 => if (index % 3 == 0) std.mem.writeInt(u64, memory[index * 8 ..][0..8], 0x7ff0_0000_0000_0000 | @as(u64, index + 1), .little),
                .u8, .i8, .u16, .i16, .u32, .i32, .u64, .i64, .u128, .i128, .dec, .u8x16, .i8x16, .u16x8, .i16x8, .u32x4, .i32x4, .u64x2, .i64x2 => {},
            };
            var roc_list = RocList{
                .bytes = &memory,
                .length = len,
                .capacity_or_alloc_ptr = RocList.encodeCapacity(max_items),
            };
            const stored = try testStoreNodes(&writer, list_plan, list_layout, .{ .ptr = @ptrCast(&roc_list) });
            const list = artifact.const_store.get(stored.const_node).list;
            try testing.expectEqual(@as(u64, max_items), list.capacity);
            if (len == 0) {
                try testing.expect(list.items == .empty);
                continue;
            }
            const items = list.items.packed_bytes;
            try testing.expectEqual(element, items.element.?);
            try testing.expectEqual(len, items.len);
            const blob = artifact.const_store.blobData(items.bytes);
            try testing.expectEqual(len * width, blob.len);
            for (0..len) |index| {
                const item = try testStoreNodes(&writer, scalar_plan, elem, .{ .ptr = memory[index * width ..].ptr });
                var expected: [16]u8 = undefined;
                writePackedScalar(expected[0..width], element, artifact.const_store.get(item.const_node).scalar);
                try testing.expectEqualSlices(u8, expected[0..width], blob[index * width ..][0..width]);
            }
            if (element == .f32 or element == .f64) {
                // The first item is a NaN whose low byte Roc's one NaN
                // clears.
                try testing.expect(blob[0] != memory[0]);
            } else {
                try testing.expectEqualSlices(u8, memory[0 .. len * width], blob);
            }
        }
    }
}

test "const store writer stores every NaN of an evaluated product list as Roc's one NaN" {
    const testing = std.testing;
    if (builtin.cpu.arch.endian() != .little) return error.SkipZigTest;

    var module_env = try can.ModuleEnv.init(testing.allocator, "");
    defer module_env.deinit();
    var artifact = try initTestArtifact(testing.allocator, &module_env);
    defer deinitTestArtifact(&artifact, testing.allocator);
    var program = try LirProgram.Result.init(testing.allocator, .u64);
    defer program.deinit();
    const scalar_plan: LirProgram.ConstPlanId = @fromBackingInt(@intCast(program.const_plans.items.len));
    try program.const_plans.append(testing.allocator, .scalar);
    const record_plan: LirProgram.ConstPlanId = @fromBackingInt(@intCast(program.const_plans.items.len));
    const children = try testing.allocator.dupe(LirProgram.ConstPlanId, &.{ scalar_plan, scalar_plan, scalar_plan });
    program.const_plans.append(testing.allocator, .{ .record = children }) catch |err| {
        testing.allocator.free(children);
        return err;
    };
    const list_plan: LirProgram.ConstPlanId = @fromBackingInt(@intCast(program.const_plans.items.len));
    try program.const_plans.append(testing.allocator, .{ .list = record_plan });
    const record_layout = try program.layouts.putStructFields(&.{
        .{ .index = 0, .layout = .f64 },
        .{ .index = 1, .layout = .f32 },
        .{ .index = 2, .layout = .u32 },
    });
    const list_layout = try program.layouts.insertLayout(layout.Layout.list(record_layout));

    var writer = Writer.init(testing.allocator, &artifact, &program);
    defer writer.deinit();

    const Item = extern struct { f64_bits: u64, f32_bits: u32, int: u32 };
    var items = [_]Item{
        .{ .f64_bits = 0xfff8_0000_0000_0000, .f32_bits = 0xffc0_0000, .int = 0x7fc0_0001 },
        .{ .f64_bits = 0x7ff0_0000_0000_0001, .f32_bits = 0x7f80_0001, .int = 0xffff_ffff },
        .{ .f64_bits = 0x8000_0000_0000_0000, .f32_bits = 0x7f80_0000, .int = 0 },
    };
    var roc_list = RocList{
        .bytes = @ptrCast(&items),
        .length = items.len,
        .capacity_or_alloc_ptr = RocList.encodeCapacity(items.len),
    };
    const stored = try testStoreNodes(&writer, list_plan, list_layout, .{ .ptr = @ptrCast(&roc_list) });
    const packed_items = artifact.const_store.get(stored.const_node).list.items.packed_bytes;
    try testing.expectEqual(@as(?const_store.ConstPackedScalar, null), packed_items.element);
    var expected: [items.len * 16]u8 = undefined;
    for (items, 0..) |item, index| {
        const canonical = expected[index * 16 ..][0..16];
        std.mem.writeInt(u64, canonical[0..8], builtins.float_bits.normalizeF64NanBits(item.f64_bits), .little);
        std.mem.writeInt(u32, canonical[8..12], builtins.float_bits.normalizeF32NanBits(item.f32_bits), .little);
        std.mem.writeInt(u32, canonical[12..16], item.int, .little);
    }
    try testing.expectEqualSlices(u8, &expected, artifact.const_store.blobData(packed_items.bytes));
}

/// Append a plan of a union of the tags `names` with no payloads, which are
/// in byte-wise name order, the order that assigns discriminants.
fn appendTestTagPlan(program: *LirProgram.Result, names: []const []const u8) Allocator.Error!LirProgram.ConstPlanId {
    const none = [_]LirProgram.ConstPlanId{};
    var payloads: [300][]const LirProgram.ConstPlanId = undefined;
    @memset(payloads[0..names.len], &none);
    return appendTestTagPlanWithPayloads(program, names, payloads[0..names.len]);
}

/// Append a plan of a union of the tags `names`, which are in byte-wise name
/// order, each with the payloads of its entry of `payloads`.
fn appendTestTagPlanWithPayloads(program: *LirProgram.Result, names: []const []const u8, payloads: []const []const LirProgram.ConstPlanId) Allocator.Error!LirProgram.ConstPlanId {
    const allocator = std.testing.allocator;
    const variants = try allocator.alloc(LirProgram.ConstTagVariant, names.len);
    var filled: usize = 0;
    errdefer {
        for (variants[0..filled]) |variant| {
            allocator.free(variant.name);
            allocator.free(variant.payloads);
        }
        allocator.free(variants);
    }
    for (variants, names, payloads, 0..) |*variant, name, variant_payloads, index| {
        const owned = try allocator.dupe(u8, name);
        errdefer allocator.free(owned);
        variant.* = .{ .name = owned, .checked_name = undefined, .discriminant = @intCast(index), .payloads = try allocator.dupe(LirProgram.ConstPlanId, variant_payloads) };
        filled += 1;
    }
    const id: LirProgram.ConstPlanId = @fromBackingInt(@intCast(program.const_plans.items.len));
    try program.const_plans.append(allocator, .{ .tag_union = variants });
    return id;
}

/// The index in `names` of the tag stored node `node` holds, through any
/// nominal wrapping.
fn testStoredTagIndex(store: *const const_store.ConstStore, node: checked.ConstNodeId, names: []const []const u8) error{TestUnexpectedResult}!usize {
    var value = store.get(node);
    while (value == .nominal) value = store.get(value.nominal.backing);
    try std.testing.expect(value == .tag);
    for (names, 0..) |name, index| {
        if (std.mem.order(u8, name, value.tag.tag_name) == .eq) return index;
    }
    return error.TestUnexpectedResult;
}

/// The little-endian unsigned integer `bytes` hold.
fn testLittleEndian(bytes: []const u8) u64 {
    var value: u64 = 0;
    for (bytes, 0..) |byte, shift| value |= @as(u64, byte) << @intCast(shift * 8);
    return value;
}

test "const store writer packs an evaluated list of tags with zero-sized payloads as storing each item would" {
    const testing = std.testing;
    if (builtin.cpu.arch.endian() != .little) return error.SkipZigTest;

    var module_env = try can.ModuleEnv.init(testing.allocator, "");
    defer module_env.deinit();
    var artifact = try initTestArtifact(testing.allocator, &module_env);
    defer deinitTestArtifact(&artifact, testing.allocator);
    var program = try LirProgram.Result.init(testing.allocator, .u64);
    defer program.deinit();

    var wide_names: [300][]const u8 = undefined;
    var wide_text: [300][4]u8 = undefined;
    for (&wide_names, &wide_text, 0..) |*name, *text, index| {
        _ = try std.fmt.bufPrint(text, "T{d:0>3}", .{index});
        name.* = text;
    }
    var wide_layouts: [300]layout.Idx = undefined;
    @memset(&wide_layouts, .zst);

    const bool_names = [_][]const u8{ "False", "True" };
    const pair_names = [_][]const u8{ "No", "Yes" };
    const color_names = [_][]const u8{ "Blue", "Cyan", "Green", "Magenta", "Red", "Yellow" };
    const only_names = [_][]const u8{"Only"};

    const bool_tags = try appendTestTagPlan(&program, &bool_names);
    // `Bool` is a nominal type wrapping its tags.
    const bool_plan: LirProgram.ConstPlanId = @fromBackingInt(@intCast(program.const_plans.items.len));
    try program.const_plans.append(testing.allocator, .{ .named = .{ .named_type = undefined, .backing = bool_tags } });
    const pair_plan = try appendTestTagPlan(&program, &pair_names);
    const color_plan = try appendTestTagPlan(&program, &color_names);
    const wide_plan = try appendTestTagPlan(&program, &wide_names);
    const only_plan = try appendTestTagPlan(&program, &only_names);
    // Tags whose payloads are zero-sized: `[A({}), B]`, `[A, B({}), C({},
    // {})]`, `[A([Only]), B]`, and a nominal type wrapping the first.
    const unit_plan: LirProgram.ConstPlanId = @fromBackingInt(@intCast(program.const_plans.items.len));
    try program.const_plans.append(testing.allocator, .{ .record = try testing.allocator.alloc(LirProgram.ConstPlanId, 0) });
    const unit_layout = try program.layouts.putStructFields(&.{});
    const ab_names = [_][]const u8{ "A", "B" };
    const abc_names = [_][]const u8{ "A", "B", "C" };
    const unit_plan_payload = try appendTestTagPlanWithPayloads(&program, &ab_names, &.{ &.{unit_plan}, &.{} });
    const mixed_plan = try appendTestTagPlanWithPayloads(&program, &abc_names, &.{ &.{}, &.{unit_plan}, &.{ unit_plan, unit_plan } });
    const nested_plan = try appendTestTagPlanWithPayloads(&program, &ab_names, &.{ &.{only_plan}, &.{} });
    const wrapped_plan: LirProgram.ConstPlanId = @fromBackingInt(@intCast(program.const_plans.items.len));
    try program.const_plans.append(testing.allocator, .{ .named = .{ .named_type = undefined, .backing = unit_plan_payload } });
    const units_layout = try program.layouts.putStructFields(&.{ .{ .index = 0, .layout = unit_layout }, .{ .index = 1, .layout = unit_layout } });
    const unit_payload_layout = try program.layouts.putTagUnion(&.{ unit_layout, .zst });

    const Case = struct { names: []const []const u8, plan: LirProgram.ConstPlanId, tags: LirProgram.ConstPlanId, layout_idx: layout.Idx, width: usize };
    const cases = [_]Case{
        .{ .names = &bool_names, .plan = bool_plan, .tags = bool_tags, .layout_idx = .bool, .width = 1 },
        .{ .names = &pair_names, .plan = pair_plan, .tags = pair_plan, .layout_idx = try program.layouts.putTagUnion(&.{ .zst, .zst }), .width = 1 },
        .{ .names = &color_names, .plan = color_plan, .tags = color_plan, .layout_idx = try program.layouts.putTagUnion(wide_layouts[0..color_names.len]), .width = 1 },
        // More tags than one byte numbers.
        .{ .names = &wide_names, .plan = wide_plan, .tags = wide_plan, .layout_idx = try program.layouts.putTagUnion(&wide_layouts), .width = 2 },
        // One tag: zero-sized items.
        .{ .names = &only_names, .plan = only_plan, .tags = only_plan, .layout_idx = try program.layouts.putTagUnion(&.{.zst}), .width = 0 },
        .{ .names = &ab_names, .plan = unit_plan_payload, .tags = unit_plan_payload, .layout_idx = unit_payload_layout, .width = 1 },
        .{ .names = &abc_names, .plan = mixed_plan, .tags = mixed_plan, .layout_idx = try program.layouts.putTagUnion(&.{ .zst, unit_layout, units_layout }), .width = 1 },
        .{ .names = &ab_names, .plan = nested_plan, .tags = nested_plan, .layout_idx = try program.layouts.putTagUnion(&.{ try program.layouts.putTagUnion(&.{.zst}), .zst }), .width = 1 },
        .{ .names = &ab_names, .plan = wrapped_plan, .tags = unit_plan_payload, .layout_idx = unit_payload_layout, .width = 1 },
    };

    var writer = Writer.init(testing.allocator, &artifact, &program);
    defer writer.deinit();

    const max_items = 67;
    var memory: [max_items * 2]u8 align(16) = undefined;
    var prng = std.Random.DefaultPrng.init(0x7a65);
    for (cases) |case| {
        const list_plan: LirProgram.ConstPlanId = @fromBackingInt(@intCast(program.const_plans.items.len));
        try program.const_plans.append(testing.allocator, .{ .list = case.plan });
        const list_layout = try program.layouts.insertLayout(if (case.width == 0) layout.Layout.listOfZst() else layout.Layout.list(case.layout_idx));
        try testing.expectEqual(case.width, program.layouts.layoutSize(program.layouts.getLayout(case.layout_idx)));
        for ([_]usize{ 0, 1, max_items }) |len| {
            for (0..len) |index| {
                const discriminant = prng.random().uintLessThan(usize, case.names.len);
                for (0..case.width) |byte| memory[index * case.width + byte] = @truncate(discriminant >> @intCast(byte * 8));
            }
            var roc_list = RocList{
                .bytes = if (case.width == 0) null else &memory,
                .length = len,
                .capacity_or_alloc_ptr = RocList.encodeCapacity(max_items),
            };
            const stored = try testStoreNodes(&writer, list_plan, list_layout, .{ .ptr = @ptrCast(&roc_list) });
            const list = artifact.const_store.get(stored.const_node).list;
            try testing.expectEqual(@as(u64, max_items), list.capacity);
            if (len == 0) {
                try testing.expect(list.items == .empty);
                continue;
            }
            const items = list.items.packed_bytes;
            try testing.expectEqual(@as(?const_store.ConstPackedScalar, null), items.element);
            try testing.expectEqual(case.width, items.product_width);
            try testing.expectEqual(len, items.len);
            const blob = artifact.const_store.blobData(items.bytes);
            try testing.expectEqual(len * case.width, blob.len);
            for (0..len) |index| {
                const item_value: Value = if (case.width == 0) Value.zst else .{ .ptr = memory[index * case.width ..].ptr };
                const item = try testStoreNodes(&writer, case.tags, case.layout_idx, item_value);
                const expected = try testStoredTagIndex(&artifact.const_store, item.const_node, case.names);
                try testing.expectEqual(@as(u64, expected), testLittleEndian(blob[index * case.width ..][0..case.width]));
            }
        }
    }
}

test "const store writer packs an evaluated list of records holding tags with zero-sized payloads as storing each item would" {
    const testing = std.testing;
    if (builtin.cpu.arch.endian() != .little) return error.SkipZigTest;

    var module_env = try can.ModuleEnv.init(testing.allocator, "");
    defer module_env.deinit();
    var artifact = try initTestArtifact(testing.allocator, &module_env);
    defer deinitTestArtifact(&artifact, testing.allocator);
    var program = try LirProgram.Result.init(testing.allocator, .u64);
    defer program.deinit();

    const bool_names = [_][]const u8{ "False", "True" };
    const color_names = [_][]const u8{ "Blue", "Green", "Red" };
    const scalar_plan: LirProgram.ConstPlanId = @fromBackingInt(@intCast(program.const_plans.items.len));
    try program.const_plans.append(testing.allocator, .scalar);
    const bool_tags = try appendTestTagPlan(&program, &bool_names);
    const bool_plan: LirProgram.ConstPlanId = @fromBackingInt(@intCast(program.const_plans.items.len));
    try program.const_plans.append(testing.allocator, .{ .named = .{ .named_type = undefined, .backing = bool_tags } });
    const unit_plan: LirProgram.ConstPlanId = @fromBackingInt(@intCast(program.const_plans.items.len));
    try program.const_plans.append(testing.allocator, .{ .record = try testing.allocator.alloc(LirProgram.ConstPlanId, 0) });
    // `[Blue({}), Green, Red]`.
    const color_plan = try appendTestTagPlanWithPayloads(&program, &color_names, &.{ &.{unit_plan}, &.{}, &.{} });
    const record_plan: LirProgram.ConstPlanId = @fromBackingInt(@intCast(program.const_plans.items.len));
    const children = try testing.allocator.dupe(LirProgram.ConstPlanId, &.{ color_plan, scalar_plan, bool_plan });
    program.const_plans.append(testing.allocator, .{ .record = children }) catch |err| {
        testing.allocator.free(children);
        return err;
    };
    const list_plan: LirProgram.ConstPlanId = @fromBackingInt(@intCast(program.const_plans.items.len));
    try program.const_plans.append(testing.allocator, .{ .list = record_plan });
    const color_layout = try program.layouts.putTagUnion(&.{ try program.layouts.putStructFields(&.{}), .zst, .zst });
    const record_layout = try program.layouts.putStructFields(&.{
        .{ .index = 0, .layout = color_layout },
        .{ .index = 1, .layout = .u32 },
        .{ .index = 2, .layout = .bool },
    });
    const list_layout = try program.layouts.insertLayout(layout.Layout.list(record_layout));
    const struct_idx = program.layouts.getLayout(record_layout).getStruct().idx;
    const width: usize = program.layouts.layoutSize(program.layouts.getLayout(record_layout));
    const offsets = [3]usize{
        program.layouts.getStructFieldOffsetByOriginalIndex(struct_idx, 0),
        program.layouts.getStructFieldOffsetByOriginalIndex(struct_idx, 1),
        program.layouts.getStructFieldOffsetByOriginalIndex(struct_idx, 2),
    };

    var writer = Writer.init(testing.allocator, &artifact, &program);
    defer writer.deinit();

    const max_items = 67;
    var memory: [max_items * 8]u8 align(16) = undefined;
    var prng = std.Random.DefaultPrng.init(0x7a66);
    for ([_]usize{ 0, 1, max_items }) |len| {
        // Padding bytes hold whatever the evaluation left there.
        prng.random().bytes(&memory);
        for (0..len) |index| {
            const item = memory[index * width ..][0..width];
            item[offsets[0]] = prng.random().uintLessThan(u8, color_names.len);
            item[offsets[2]] = prng.random().uintLessThan(u8, bool_names.len);
        }
        var roc_list = RocList{
            .bytes = &memory,
            .length = len,
            .capacity_or_alloc_ptr = RocList.encodeCapacity(len),
        };
        const stored = try testStoreNodes(&writer, list_plan, list_layout, .{ .ptr = @ptrCast(&roc_list) });
        const list = artifact.const_store.get(stored.const_node).list;
        if (len == 0) {
            try testing.expect(list.items == .empty);
            continue;
        }
        const items = list.items.packed_bytes;
        try testing.expectEqual(@as(?const_store.ConstPackedScalar, null), items.element);
        // The color, the U32 and the Bool, without padding.
        try testing.expectEqual(@as(u32, 6), items.product_width);
        const blob = artifact.const_store.blobData(items.bytes);
        try testing.expectEqual(len * 6, blob.len);
        for (0..len) |index| {
            const item = try testStoreNodes(&writer, record_plan, record_layout, .{ .ptr = memory[index * width ..].ptr });
            const fields = artifact.const_store.get(item.const_node).record;
            const packed_item = blob[index * 6 ..][0..6];
            try testing.expectEqual(@as(u64, try testStoredTagIndex(&artifact.const_store, fields[0], &color_names)), packed_item[0]);
            try testing.expectEqual(artifact.const_store.get(fields[1]).scalar.u32, std.mem.readInt(u32, packed_item[1..5], .little));
            try testing.expectEqual(@as(u64, try testStoredTagIndex(&artifact.const_store, fields[2], &bool_names)), packed_item[5]);
        }
    }
}

test "const store writer stores a list of tags with a payload that has bytes one node per item" {
    const testing = std.testing;

    var module_env = try can.ModuleEnv.init(testing.allocator, "");
    defer module_env.deinit();
    var artifact = try initTestArtifact(testing.allocator, &module_env);
    defer deinitTestArtifact(&artifact, testing.allocator);
    var program = try LirProgram.Result.init(testing.allocator, .u64);
    defer program.deinit();

    const scalar_plan: LirProgram.ConstPlanId = @fromBackingInt(@intCast(program.const_plans.items.len));
    try program.const_plans.append(testing.allocator, .scalar);
    // `[A(U8), B]`.
    const names = [_][]const u8{ "A", "B" };
    const tag_plan = try appendTestTagPlanWithPayloads(&program, &names, &.{ &.{scalar_plan}, &.{} });
    const list_plan: LirProgram.ConstPlanId = @fromBackingInt(@intCast(program.const_plans.items.len));
    try program.const_plans.append(testing.allocator, .{ .list = tag_plan });
    const tag_layout = try program.layouts.putTagUnion(&.{ .u8, .zst });
    const list_layout = try program.layouts.insertLayout(layout.Layout.list(tag_layout));
    const data = program.layouts.getTagUnionData(program.layouts.getLayout(tag_layout).getTagUnion().idx);
    const width: usize = program.layouts.layoutSize(program.layouts.getLayout(tag_layout));

    var writer = Writer.init(testing.allocator, &artifact, &program);
    defer writer.deinit();

    const max_items = 67;
    var memory: [max_items * 2]u8 align(16) = @splat(0);
    for (0..max_items) |index| {
        const item = memory[index * width ..].ptr;
        data.writeDiscriminant(item, @intCast(index % 2), program.layouts.targetUsize());
        item[0] = @truncate(index);
    }
    for ([_]usize{ 1, max_items }) |len| {
        var roc_list = RocList{ .bytes = &memory, .length = len, .capacity_or_alloc_ptr = RocList.encodeCapacity(len) };
        const stored = try testStoreNodes(&writer, list_plan, list_layout, .{ .ptr = @ptrCast(&roc_list) });
        const items = artifact.const_store.get(stored.const_node).list.items;
        try testing.expect(items == .nodes);
        try testing.expectEqual(len, items.nodes.len);
        for (items.nodes, 0..) |node, index| {
            const tag = artifact.const_store.get(node).tag;
            try testing.expectEqual(index % 2, try testStoredTagIndex(&artifact.const_store, node, &names));
            try testing.expectEqual(@as(usize, if (index % 2 == 0) 1 else 0), tag.payloads.len);
        }
    }
}
