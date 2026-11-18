//! Store compile-time values as checked constants: an evaluator's results,
//! read from the memory its program left them in, and value-form roots,
//! read from their checked expressions and from the stored values of the
//! constants those read (design.md "Value-Form Roots"). All go through one
//! plan-directed walk, which differs between them only in where a value's
//! parts come from (`Part`).

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
const CIR = can.CIR;
const exact_numeral = checked.exact_numeral;

/// A value being stored, or one of its parts: the memory an evaluated
/// program left it in, a checked expression in value form, or a node of a
/// stored constant such an expression reads.
pub const Part = union(enum) {
    runtime: Value,
    checked: CIR.Expr.Idx,
    stored: StoredPart,
};

/// A node of a constant's stored value, in the store of the constant's
/// module, which may be the store being written.
pub const StoredPart = struct {
    store: *const const_store.ConstStore,
    node: checked.ConstNodeId,

    fn value(self: StoredPart) checked.ConstValue {
        return self.store.get(self.node);
    }

    fn child(self: StoredPart, node: checked.ConstNodeId) Part {
        return .{ .stored = .{ .store = self.store, .node = node } };
    }
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
            switch (self.constPlan(plan_id)) {
                .boxy_fn => return .boxy,
                .fn_value, .erased_fn => return .lss,
                .pending, .layout_only, .zst, .scalar, .str => {},
                .list, .box => |child| try pending.append(self.allocator, child),
                .boxed_sequence => |sequence| try pending.append(self.allocator, sequence.payload),
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
            try self.store_requests.append(self.allocator, .{ .plan = capture.plan, .layout_idx = capture.layout_idx, .value = .{ .runtime = .{ .ptr = capture.ptr } } });
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

        const plan = self.constPlan(root.plan);
        return switch (root.request.kind) {
            .compile_time_constant, .repl_expr => .{ .const_node = try self.storeValue(root.plan, root.ret_layout, .{ .runtime = value }) },
            .compile_time_callable => switch (plan) {
                .fn_value => |set| .{ .fn_value = try self.storeFnValue(set, root.ret_layout, value) },
                .erased_fn => |set| .{ .fn_value = try self.storeErasedFn(set, value) },
                .boxy_fn => .{ .const_node = try self.storeValue(root.plan, root.ret_layout, .{ .runtime = value }) },
                .pending,
                .layout_only,
                .zst,
                .scalar,
                .str,
                .list,
                .box,
                .boxed_sequence,
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

    /// Store a literal root's value: one value graph, no stored type.
    pub fn storeLiteral(
        self: *Writer,
        plan: LirProgram.ConstPlanId,
        ret_layout: layout.Idx,
        value: Value,
    ) Error!checked.ConstNodeId {
        self.stored_values.clearRetainingCapacity();
        return self.storeValue(plan, ret_layout, .{ .runtime = value });
    }

    /// Store a value-form root from its checked expression `expr`, by the
    /// plan and layout of its result (`BoxyValueSchema.root`), and from the
    /// stored values of the constants it reads when `reads_constants`. Every
    /// part of the expression is a node of its own.
    pub fn storeCheckedRoot(
        self: *Writer,
        plan: LirProgram.ConstPlanId,
        ret_layout: layout.Idx,
        expr: CIR.Expr.Idx,
        reads_constants: bool,
    ) Error!checked.ConstNodeId {
        if (self.checked == null) writerInvariant("a writer of evaluated values stored a checked expression");
        self.checked_reads_constants = reads_constants;
        return self.storeValue(plan, ret_layout, self.resolvePart(.{ .checked = expr }));
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
    /// durable ConstStore type table.
    pub fn storeType(self: *Writer, ty: const_store.ConstTypeId) Error!const_store.ConstTypeId {
        return self.store.type_store.cloneTypeFromTranslated(
            &self.program.const_types,
            &self.program.const_type_names,
            self.typeNames(),
            ty,
        );
    }

    /// Preserve the concrete type a literal root converted its literal at.
    pub fn storeLiteralType(
        self: *Writer,
        root: LirProgram.LiteralRootPlan,
    ) Error!const_store.ConstTypeId {
        return self.store.type_store.cloneTypeFromTranslated(
            &self.program.const_types,
            &self.program.const_type_names,
            self.typeNames(),
            root.ret_type,
        );
    }

    /// One value to store or scan.
    const ValueRequest = struct {
        plan: LirProgram.ConstPlanId,
        layout_idx: layout.Idx,
        value: Part,
        storage: LirProgram.CaptureSlotStorage = .value,
    };

    const StoreFinish = union(enum) {
        list: struct { node: checked.ConstNodeId, capacity: u64 },
        box: checked.ConstNodeId,
        tuple: checked.ConstNodeId,
        record: checked.ConstNodeId,
        nominal: struct { node: checked.ConstNodeId, named: @FieldType(LirProgram.ConstPlan, "named") },
        tag: struct { node: checked.ConstNodeId, name: []const u8 },
        /// A function value; `node` is null for a callable root, whose
        /// function id is the store's result.
        fn_value: struct { node: ?checked.ConstNodeId, template: LirProgram.FnTemplate },
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

    fn storeValue(
        self: *Writer,
        plan_id: LirProgram.ConstPlanId,
        layout_idx: layout.Idx,
        value: Part,
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
                    const ty = try self.cloneCaptureType(frame.slots[index].ty);
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
    /// An evaluated part already stored for the root (the same runtime
    /// allocation) is that node; a checked expression's part is always a
    /// node of its own.
    fn startStore(self: *Writer, request: ValueRequest) Error!checked.ConstNodeId {
        switch (request.value) {
            .runtime => |value| if (self.memoAddress(request.plan, request.layout_idx, value, request.storage)) |address| {
                if (self.stored_values.get(address)) |existing| return existing;
                const node = try self.store.reserve();
                try self.stored_values.put(address, node);
                try self.beginFresh(node, request);
                return node;
            },
            .checked, .stored => {},
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
                const payload = self.readBoxDataPointer(runtimePart(part)) orelse
                    writerInvariant("recursive capture slot had null payload pointer");
                layout_idx = boxed.getIdx();
                part = .{ .runtime = .{ .ptr = payload } };
            },
        }
        const requests_start = self.store_requests.items.len;
        const children = &self.store_requests;
        const finish: StoreFinish = switch (self.constPlan(request.plan)) {
            .pending => writerInvariant("pending const plan reached ConstStore writer"),
            .layout_only => writerInvariant("layout-only const plan reached ConstStore writer"),
            .zst => return self.store.fill(node, .zst),
            .scalar => return self.store.fill(node, .{ .scalar = try self.partScalar(layout_idx, part) }),
            .str => return self.store.fill(node, try self.partStr(part)),
            .list => |elem_plan| blk: {
                if (try self.storeListLeaf(node, elem_plan, layout_idx, part)) return;
                try self.appendListRequests(children, elem_plan, layout_idx, part);
                break :blk .{ .list = .{ .node = node, .capacity = self.partListCapacity(part) } };
            },
            .box => |elem_plan| blk: {
                try self.appendBoxRequest(children, elem_plan, layout_idx, runtimePart(part));
                break :blk .{ .box = node };
            },
            .boxed_sequence => |sequence| blk: {
                const handle = self.readBoxDataPointer(runtimePart(part)) orelse writerInvariant("boxed sequence had a null handle");
                try children.append(self.allocator, .{
                    .plan = sequence.payload,
                    .layout_idx = sequence.view_layout,
                    .value = .{ .runtime = .{ .ptr = builtins.boxed_sequence.recordPtr(handle) } },
                });
                break :blk .{ .box = node };
            },
            .tuple => |items| blk: {
                try self.appendStructRequests(children, request.plan, items, layout_idx, part);
                break :blk .{ .tuple = node };
            },
            .record => |fields| blk: {
                try self.appendStructRequests(children, request.plan, fields, layout_idx, part);
                break :blk .{ .record = node };
            },
            .tag_union => |variants| blk: {
                const name = switch (part) {
                    .runtime => |value| name: {
                        const tag_base = self.resolveTagBase(layout_idx, value);
                        const selected = self.selectTagVariant(variants, tag_base.layout_idx, tag_base.value);
                        const payload_layout = self.tagPayloadLayout(tag_base.layout_idx, selected.discriminant);
                        try self.appendTagPayloadRequests(children, selected.payloads, payload_layout, tag_base.value);
                        break :name selected.name;
                    },
                    .checked => |expr| try self.appendCheckedTagRequests(children, request.plan, variants, layout_idx, expr),
                    .stored => |stored| try self.appendStoredTagRequests(children, request.plan, variants, layout_idx, stored),
                };
                break :blk .{ .tag = .{ .node = node, .name = name } };
            },
            .named => |named| blk: {
                const backing: Part = switch (part) {
                    .runtime => part,
                    .checked => |expr| self.resolvePart(.{ .checked = self.checkedBacking(expr) }),
                    .stored => |stored| storedBacking(stored),
                };
                try children.append(self.allocator, .{ .plan = named.backing, .layout_idx = layout_idx, .value = backing });
                break :blk .{ .nominal = .{ .node = node, .named = named } };
            },
            .fn_value => |set| return self.pushFnValue(node, set, layout_idx, runtimePart(part)),
            .erased_fn => |set| return self.pushErasedFn(node, set, runtimePart(part)),
            .boxy_fn => return self.pushBoxyFn(node, runtimePart(part)),
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
        self.store_frames.appendAssumeCapacity(.{
            .finish = finish,
            .requests_start = requests_start,
            .requests_len = requests_len,
            .nodes_start = nodes_start,
            .slots = slots,
            .capture_types_start = capture_types_start,
        });
    }

    fn pushFnValue(
        self: *Writer,
        node: ?checked.ConstNodeId,
        set_id: LirProgram.FnSetId,
        layout_idx: layout.Idx,
        value: Value,
    ) Error!void {
        const set = self.program.fn_sets.items[@backingInt(set_id)];
        const tag_base = self.resolveTagBase(layout_idx, value);
        const variant = self.selectFnVariant(set, tag_base.layout_idx, tag_base.value);
        const requests_start = self.store_requests.items.len;
        try self.appendCaptureRequests(&self.store_requests, variant.captures, variant.payload_layout, tag_base.value);
        try self.pushStoreFrame(.{ .fn_value = .{ .node = node, .template = variant.template } }, requests_start, variant.captures);
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
        const template = entry.entry.template orelse writerInvariant("erased callable entry has no ConstStore template");
        try self.pushStoreFrame(.{ .fn_value = .{ .node = node, .template = template } }, requests_start, entry.entry.captures);
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
        const nodes = self.store_nodes.items[frame.nodes_start..][0..frame.requests_len];
        const capture_types = self.store_capture_types.items[frame.capture_types_start..][0..frame.slots.len];
        switch (frame.finish) {
            .list => |list| self.store.fill(list.node, .{ .list = .{ .items = .{ .nodes = nodes }, .capacity = list.capacity } }),
            .box => |node| self.store.fill(node, .{ .box = nodes[0] }),
            .tuple => |node| self.store.fill(node, .{ .tuple = nodes }),
            .record => |node| self.store.fill(node, .{ .record = nodes }),
            .nominal => |nominal| self.store.fill(nominal.node, .{ .nominal = .{
                .named_type = nominal.named.named_type,
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
                    .owner_fn_ty = if (template.owner_fn_ty.get()) |owner_fn_ty| try self.cloneCaptureType(owner_fn_ty) else null,
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
    /// packed-scalar, or packed-product list); false when its elements are
    /// stored as nodes.
    fn storeListLeaf(
        self: *Writer,
        target_node: checked.ConstNodeId,
        elem_plan: LirProgram.ConstPlanId,
        layout_idx: layout.Idx,
        part: Part,
    ) Error!bool {
        const layout_value = self.program.layouts.getLayout(layout_idx);
        if (layout_value.tag != .list and layout_value.tag != .list_of_zst) {
            writerInvariant("list const plan had non-list layout");
        }
        const len = self.partListLen(part);
        const capacity = self.partListCapacity(part);
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
            switch (part) {
                .runtime => |value| {
                    const roc_list: *const RocList = @ptrCast(@alignCast(value.ptr));
                    const memory = if (memory_len == 0) &.{} else roc_list.bytes.?[0..memory_len];
                    try self.storeProductList(target_node, plan, memory, len, capacity);
                },
                .checked => |expr| {
                    const memory = try self.checkedProductMemory(memory_len, plan.memory_width, elem_plan, elem_layout, self.checkedListItems(expr));
                    defer self.allocator.free(memory);
                    try self.storeProductList(target_node, plan, memory, len, capacity);
                },
                // A constant of this list's type stored its items packed by
                // this same product plan.
                .stored => |stored| {
                    const items = storedPackedItems(stored);
                    if (builtin.mode == .debug) std.debug.assert(items.element == null and items.product_width == plan.packed_width);
                    try self.storeCopiedPackedList(target_node, stored, items, capacity);
                },
            }
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

    fn partListLen(self: *const Writer, part: Part) usize {
        return switch (part) {
            .runtime => |value| @as(*const RocList, @ptrCast(@alignCast(value.ptr))).len(),
            .checked => |expr| self.checkedListItems(expr).len,
            .stored => |stored| @intCast(storedListItems(stored).len()),
        };
    }

    /// The capacity a list part was built with. A list literal has none
    /// beyond its items (design.md "Static Lists And Strings Keep Their
    /// Capacity").
    fn partListCapacity(self: *const Writer, part: Part) u64 {
        return switch (part) {
            .runtime => |value| listCapacity(value),
            .checked => |expr| self.checkedListItems(expr).len,
            // A read of a constant is the constant's value, capacity
            // included.
            .stored => |stored| storedList(stored).capacity,
        };
    }

    fn appendListRequests(
        self: *Writer,
        out: *std.ArrayList(ValueRequest),
        elem_plan: LirProgram.ConstPlanId,
        layout_idx: layout.Idx,
        part: Part,
    ) Allocator.Error!void {
        const layout_value = self.program.layouts.getLayout(layout_idx);
        const value = switch (part) {
            .runtime => |value| value,
            .checked => |expr| {
                const items = self.checkedListItems(expr);
                const elem_layout = if (layout_value.tag == .list_of_zst) layout.Idx.zst else layout_value.getIdx();
                try out.ensureUnusedCapacity(self.allocator, items.len);
                for (items) |item| out.appendAssumeCapacity(.{ .plan = elem_plan, .layout_idx = elem_layout, .value = self.resolvePart(.{ .checked = item }) });
                return;
            },
            .stored => |stored| {
                // Items stored as nodes of their own: `storeListLeaf` stored
                // every list whose items are packed or absent.
                const items = storedItemNodes(stored);
                const elem_layout = if (layout_value.tag == .list_of_zst) layout.Idx.zst else layout_value.getIdx();
                try out.ensureUnusedCapacity(self.allocator, items.len);
                for (items) |item| out.appendAssumeCapacity(.{ .plan = elem_plan, .layout_idx = elem_layout, .value = stored.child(item) });
                return;
            },
        };
        const roc_list: *const RocList = @ptrCast(@alignCast(value.ptr));
        try out.ensureUnusedCapacity(self.allocator, roc_list.len());
        if (layout_value.tag == .list_of_zst) {
            for (0..roc_list.len()) |_| out.appendAssumeCapacity(.{ .plan = elem_plan, .layout_idx = .zst, .value = .{ .runtime = Value.zst } });
            return;
        }
        const elem_layout = layout_value.getIdx();
        const elem_size: usize = self.program.layouts.layoutSize(self.program.layouts.getLayout(elem_layout));
        if (roc_list.bytes) |bytes| {
            for (0..roc_list.len()) |index| {
                out.appendAssumeCapacity(.{ .plan = elem_plan, .layout_idx = elem_layout, .value = .{ .runtime = .{ .ptr = bytes + index * elem_size } } });
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
            .box_of_zst => .{ .plan = elem_plan, .layout_idx = .zst, .value = .{ .runtime = Value.zst } },
            .box => .{
                .plan = elem_plan,
                .layout_idx = layout_value.getIdx(),
                .value = .{ .runtime = .{ .ptr = self.readBoxDataPointer(value) orelse writerInvariant("boxed value had null payload pointer") } },
            },
            .erased_callable => .{ .plan = elem_plan, .layout_idx = layout_idx, .value = .{ .runtime = value } },
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

    /// The component requests of a tuple or record `plan` whose components
    /// are `plans`.
    fn appendStructRequests(
        self: *Writer,
        out: *std.ArrayList(ValueRequest),
        plan: LirProgram.ConstPlanId,
        plans: []const LirProgram.ConstPlanId,
        struct_layout: layout.Idx,
        part: Part,
    ) Allocator.Error!void {
        if (plans.len == 0) return;
        const struct_value = switch (part) {
            .runtime => |value| value,
            .checked => |expr| return self.appendComponentRequests(out, plans, struct_layout, try self.checkedComponents(plan, expr, plans.len), CheckedParts{ .writer = self }),
            .stored => |stored| return self.appendComponentRequests(out, plans, struct_layout, storedComponents(stored, plans.len), stored),
        };
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
            for (plans) |component| out.appendAssumeCapacity(.{ .plan = component, .layout_idx = .zst, .value = .{ .runtime = Value.zst } });
            return;
        }
        if (layout_value.tag != .struct_) writerInvariant("struct const plan had non-struct layout");
        for (plans, 0..) |component, index| {
            const field_layout = self.program.layouts.getStructFieldLayoutByOriginalIndex(layout_value.getStruct().idx, @intCast(index));
            const offset = self.program.layouts.getStructFieldOffsetByOriginalIndex(layout_value.getStruct().idx, @intCast(index));
            out.appendAssumeCapacity(.{ .plan = component, .layout_idx = field_layout, .value = .{ .runtime = value.offset(offset) } });
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
            out.appendAssumeCapacity(.{ .plan = plans[0], .layout_idx = payload_layout, .value = .{ .runtime = value } });
            return;
        }
        const layout_value = self.program.layouts.getLayout(payload_layout);
        if (layout_value.tag == .zst) {
            for (plans) |plan| out.appendAssumeCapacity(.{ .plan = plan, .layout_idx = .zst, .value = .{ .runtime = Value.zst } });
        } else if (layout_value.tag == .struct_) {
            for (plans, 0..) |plan, index| {
                const field_layout = self.program.layouts.getStructFieldLayoutByOriginalIndex(layout_value.getStruct().idx, @intCast(index));
                const offset = self.program.layouts.getStructFieldOffsetByOriginalIndex(layout_value.getStruct().idx, @intCast(index));
                out.appendAssumeCapacity(.{ .plan = plan, .layout_idx = field_layout, .value = .{ .runtime = value.offset(offset) } });
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
            for (slots) |slot| out.appendAssumeCapacity(.{ .plan = slot.plan, .layout_idx = .zst, .value = .{ .runtime = Value.zst }, .storage = slot.storage });
        } else if (layout_value.tag == .struct_) {
            for (slots) |slot| {
                const field_layout = self.program.layouts.getStructFieldLayoutByOriginalIndex(layout_value.getStruct().idx, slot.slot);
                const offset = self.program.layouts.getStructFieldOffsetByOriginalIndex(layout_value.getStruct().idx, slot.slot);
                out.appendAssumeCapacity(.{ .plan = slot.plan, .layout_idx = field_layout, .value = .{ .runtime = payload_value.offset(offset) }, .storage = slot.storage });
            }
        } else if (slots.len == 1) {
            out.appendAssumeCapacity(.{ .plan = slots[0].plan, .layout_idx = payload_layout, .value = .{ .runtime = payload_value }, .storage = slots[0].storage });
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

    fn partScalar(self: *Writer, layout_idx: layout.Idx, part: Part) Allocator.Error!checked.ConstScalar {
        return switch (part) {
            .runtime => |value| self.storeScalar(layout_idx, value),
            .checked => |expr| try self.checkedScalar(layout_idx, expr),
            .stored => |stored| self.storedScalar(layout_idx, stored),
        };
    }

    fn partStr(self: *Writer, part: Part) Allocator.Error!checked.ConstValue {
        return switch (part) {
            .runtime => |value| try self.storeStr(value),
            // A literal has no capacity beyond its bytes (design.md "Static
            // Lists And Strings Keep Their Capacity").
            .checked => |expr| blk: {
                const text = try self.checkedStrText(expr);
                break :blk .{ .str = .{ .bytes = try self.storeBytes(text), .capacity = text.len } };
            },
            // A read of a constant is the constant's value, capacity
            // included.
            .stored => |stored| blk: {
                const value = stored.value();
                if (value != .str) writerInvariant("a constant read at the string type is not a stored string");
                const string = value.str;
                break :blk .{ .str = .{ .bytes = try self.copyBlob(stored.store, string.bytes), .capacity = string.capacity } };
            },
        };
    }

    fn checkedValues(self: *const Writer) CheckedValues {
        return self.checked orelse writerInvariant("a writer of evaluated values read a checked expression");
    }

    /// The scalar a value-form numeral stores at `layout_idx`: its value's
    /// bytes at the layout's number type, read as the evaluator's result
    /// memory is read.
    fn checkedScalar(self: *Writer, layout_idx: layout.Idx, expr: CIR.Expr.Idx) Allocator.Error!checked.ConstScalar {
        var bytes = try self.checkedScalarBytes(layout_idx, expr);
        return self.storeScalar(layout_idx, .{ .ptr = &bytes });
    }

    /// A value-form numeral's value at the number type of `layout_idx`, as
    /// the evaluator's memory holds it (`exact_numeral.encodeBits`, which the
    /// lowering's numerals are built from).
    fn checkedScalarBytes(self: *Writer, layout_idx: layout.Idx, expr: CIR.Expr.Idx) Allocator.Error![16]u8 {
        const layout_value = self.program.layouts.getLayout(layout_idx);
        if (layout_value.tag != .scalar) writerInvariant("a value-form numeral's layout is not a scalar");
        const scalar = layout_value.getScalar();
        const target: exact_numeral.Target = switch (scalar.tag) {
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

    /// The packed bytes of value-form numerals `items` at element layout
    /// `elem_layout`, interned.
    fn checkedPackedBlob(self: *Writer, elem_layout: layout.Idx, element: const_store.ConstPackedScalar, items: []const CIR.Expr.Idx) Allocator.Error!const_store.ConstStr {
        const width: usize = element.byteWidth();
        const byte_len = std.math.mul(usize, items.len, width) catch return error.OutOfMemory;
        const bytes = try self.allocator.alloc(u8, byte_len);
        defer self.allocator.free(bytes);
        // The items of a root that reads no constant are all literals, which
        // convert with no per-item check for a read.
        if (self.checked_reads_constants) {
            for (items, 0..) |item, index| {
                writePackedScalar(bytes[index * width ..][0..width], element, try self.partScalar(elem_layout, self.resolvePart(checkedPart(item))));
            }
        } else {
            for (items, 0..) |item, index| {
                writePackedScalar(bytes[index * width ..][0..width], element, try self.checkedScalar(elem_layout, item));
            }
        }
        return try self.store.addBlobData(bytes);
    }

    /// The memory `items`, value-form products of plan `elem_plan`, would
    /// occupy as a list's elements at layout `elem_layout`, each
    /// `memory_width` bytes, padding zero. A part reading a constant takes
    /// the constant's stored value.
    fn checkedProductMemory(
        self: *Writer,
        memory_len: usize,
        memory_width: usize,
        elem_plan: LirProgram.ConstPlanId,
        elem_layout: layout.Idx,
        items: []const CIR.Expr.Idx,
    ) Allocator.Error![]u8 {
        const memory = try self.allocator.alloc(u8, memory_len);
        errdefer self.allocator.free(memory);
        @memset(memory, 0);
        const Leaf = struct { plan: LirProgram.ConstPlanId, layout_idx: layout.Idx, part: Part, offset: usize };
        var pending = std.ArrayList(Leaf).empty;
        defer pending.deinit(self.allocator);
        for (items, 0..) |item, index| {
            try pending.append(self.allocator, .{ .plan = elem_plan, .layout_idx = elem_layout, .part = checkedPart(item), .offset = index * memory_width });
            while (pending.pop()) |unresolved| {
                var leaf = unresolved;
                leaf.part = self.resolvePart(leaf.part);
                switch (self.constPlan(leaf.plan)) {
                    .zst => {},
                    .scalar => {
                        const bytes = switch (leaf.part) {
                            .checked => |expr| try self.checkedScalarBytes(leaf.layout_idx, expr),
                            .stored => |stored| scalarMemoryBytes(self.storedScalar(leaf.layout_idx, stored)),
                            .runtime => writerInvariant("a value-form product holds an evaluated part"),
                        };
                        const size = self.program.layouts.layoutSize(self.program.layouts.getLayout(leaf.layout_idx));
                        @memcpy(memory[leaf.offset..][0..size], bytes[0..size]);
                    },
                    .named => |named| try pending.append(self.allocator, .{
                        .plan = named.backing,
                        .layout_idx = leaf.layout_idx,
                        .part = switch (leaf.part) {
                            .checked => |expr| checkedPart(self.checkedBacking(expr)),
                            .stored => |stored| storedBacking(stored),
                            .runtime => writerInvariant("a value-form product holds an evaluated part"),
                        },
                        .offset = leaf.offset,
                    }),
                    .tuple, .record => |components| {
                        const layout_value = self.program.layouts.getLayout(leaf.layout_idx);
                        if (components.len == 0 or layout_value.tag == .zst) continue;
                        if (layout_value.tag != .struct_) writerInvariant("a product's layout is not a struct");
                        const struct_idx = layout_value.getStruct().idx;
                        try pending.ensureUnusedCapacity(self.allocator, components.len);
                        switch (leaf.part) {
                            .checked => |expr| {
                                const parts = try self.checkedComponents(leaf.plan, expr, components.len);
                                for (components, parts, 0..) |component, part, component_index| pending.appendAssumeCapacity(.{
                                    .plan = component,
                                    .layout_idx = self.program.layouts.getStructFieldLayoutByOriginalIndex(struct_idx, @intCast(component_index)),
                                    .part = checkedPart(part),
                                    .offset = leaf.offset + self.program.layouts.getStructFieldOffsetByOriginalIndex(struct_idx, @intCast(component_index)),
                                });
                            },
                            .stored => |stored| {
                                const parts = storedComponents(stored, components.len);
                                for (components, parts, 0..) |component, part, component_index| pending.appendAssumeCapacity(.{
                                    .plan = component,
                                    .layout_idx = self.program.layouts.getStructFieldLayoutByOriginalIndex(struct_idx, @intCast(component_index)),
                                    .part = stored.child(part),
                                    .offset = leaf.offset + self.program.layouts.getStructFieldOffsetByOriginalIndex(struct_idx, @intCast(component_index)),
                                });
                            },
                            .runtime => writerInvariant("a value-form product holds an evaluated part"),
                        }
                    },
                    .pending, .layout_only, .str, .list, .box, .boxed_sequence, .tag_union, .fn_value, .erased_fn, .boxy_fn => writerInvariant("a product plan holds a part other than a number, record, tuple or named type"),
                }
            }
        }
        return memory;
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

    /// The component requests of a value-form tuple or record, or of a
    /// stored one such a value reads, whose components in plan order are
    /// `items`, each made a part by `source` (`componentPart`), as
    /// `appendStructRequests` gives an evaluated one's.
    fn appendComponentRequests(
        self: *Writer,
        out: *std.ArrayList(ValueRequest),
        plans: []const LirProgram.ConstPlanId,
        struct_layout: layout.Idx,
        items: anytype,
        source: anytype,
    ) Allocator.Error!void {
        var layout_value = self.program.layouts.getLayout(struct_layout);
        while (layout_value.tag == .box) layout_value = self.program.layouts.getLayout(layout_value.getIdx());
        const zero_sized = layout_value.tag == .zst or layout_value.tag == .box_of_zst;
        if (!zero_sized and layout_value.tag != .struct_) writerInvariant("struct const plan had non-struct layout");
        try out.ensureUnusedCapacity(self.allocator, plans.len);
        for (plans, items, 0..) |component, item, index| out.appendAssumeCapacity(.{
            .plan = component,
            .layout_idx = if (zero_sized) .zst else self.program.layouts.getStructFieldLayoutByOriginalIndex(layout_value.getStruct().idx, @intCast(index)),
            .value = componentPart(source, item),
        });
    }

    /// Select the variant value-form tag `expr` builds and append its
    /// payloads' requests, as an evaluated tag's (`appendTagPayloadRequests`);
    /// the variant's name.
    fn appendCheckedTagRequests(
        self: *Writer,
        out: *std.ArrayList(ValueRequest),
        union_plan: LirProgram.ConstPlanId,
        variants: []const LirProgram.ConstTagVariant,
        layout_idx: layout.Idx,
        expr: CIR.Expr.Idx,
    ) Allocator.Error![]const u8 {
        const values = self.checkedValues();
        const bodies = values.bodies;
        const data = bodies.exprData(checked.valueFormValueExpr(bodies, expr));
        const name: []const u8, const args: []const CIR.Expr.Idx = if (data == .tag)
            .{ bodies.names.tagLabelText(bodies.tagLabel(data.tag.name)), data.tag.args }
        else if (data == .zero_argument_tag)
            .{ bodies.names.tagLabelText(bodies.tagLabel(data.zero_argument_tag.name)), &.{} }
        else
            writerInvariant("a value-form tag union value is not a tag");
        const selected = variants[try values.schema.tagVariant(union_plan, name)];
        try self.appendPayloadRequests(out, selected, layout_idx, args, CheckedParts{ .writer = self });
        return selected.name;
    }

    /// Select the variant of `variants` stored tag `stored` holds and append
    /// its payloads' requests, as `appendCheckedTagRequests` does for a
    /// value-form tag; the variant's name, which the plan owns.
    fn appendStoredTagRequests(
        self: *Writer,
        out: *std.ArrayList(ValueRequest),
        union_plan: LirProgram.ConstPlanId,
        variants: []const LirProgram.ConstTagVariant,
        layout_idx: layout.Idx,
        stored: StoredPart,
    ) Allocator.Error![]const u8 {
        const value = stored.value();
        if (value != .tag) writerInvariant("a constant read at a tag union type is not a stored tag");
        // The stored name is read before anything appends to the store
        // holding it, which may be the store being written.
        const selected = variants[try self.checkedValues().schema.tagVariant(union_plan, value.tag.tag_name)];
        try self.appendPayloadRequests(out, selected, layout_idx, value.tag.payloads, stored);
        return selected.name;
    }

    /// The requests of the payloads `items` of a value-form or stored tag of
    /// variant `selected`, at `layout_idx`, each made a part by `source`
    /// (`componentPart`).
    fn appendPayloadRequests(
        self: *Writer,
        out: *std.ArrayList(ValueRequest),
        selected: LirProgram.ConstTagVariant,
        layout_idx: layout.Idx,
        items: anytype,
        source: anytype,
    ) Allocator.Error!void {
        if (selected.payloads.len != items.len) writerInvariant("a value-form tag does not have its variant's payloads");
        if (items.len == 0) return;
        const base_layout = self.program.layouts.getLayout(layout_idx);
        const tag_layout: layout.Idx = switch (base_layout.tag) {
            .zst, .tag_union => layout_idx,
            .box => base_layout.getIdx(),
            .hole, .dynamic, .scalar, .box_of_zst, .list, .list_of_zst, .struct_, .closure, .erased_callable, .ptr => writerInvariant("tag value read had non-tag layout"),
        };
        const payload_layout = self.tagPayloadLayout(tag_layout, selected.discriminant);
        try out.ensureUnusedCapacity(self.allocator, items.len);
        if (items.len == 1) {
            out.appendAssumeCapacity(.{ .plan = selected.payloads[0], .layout_idx = payload_layout, .value = componentPart(source, items[0]) });
            return;
        }
        const payload_value = self.program.layouts.getLayout(payload_layout);
        if (payload_value.tag != .zst and payload_value.tag != .struct_) writerInvariant("multi-payload tag did not use a struct payload layout");
        for (selected.payloads, items, 0..) |plan, item, index| out.appendAssumeCapacity(.{
            .plan = plan,
            .layout_idx = if (payload_value.tag == .zst) .zst else self.program.layouts.getStructFieldLayoutByOriginalIndex(payload_value.getStruct().idx, @intCast(index)),
            .value = componentPart(source, item),
        });
    }

    /// The part a value-form or stored value's component reads: a checked
    /// expression `source` resolves (`CheckedParts`), or a node of the
    /// stored value `source` holds.
    fn componentPart(source: anytype, item: anytype) Part {
        return if (@TypeOf(source) == StoredPart) source.child(item) else source.part(item);
    }

    /// The parts of a value-form expression's components, each a checked
    /// expression or, when it reads a constant, the constant's stored
    /// value. A part is resolved where it is made, so storing a part never
    /// asks again.
    const CheckedParts = struct {
        writer: *const Writer,

        fn part(self: CheckedParts, expr: CIR.Expr.Idx) Part {
            return self.writer.resolvePart(.{ .checked = expr });
        }
    };

    /// `part`, or, when it is a value-form expression reading a constant,
    /// the constant's stored value (design.md "Value-Form Roots").
    fn resolvePart(self: *const Writer, part: Part) Part {
        const expr = switch (part) {
            .checked => |expr| expr,
            .runtime, .stored => return part,
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

    /// The scalar stored node `stored` holds, which a constant of the
    /// number type of `layout_idx` stored.
    fn storedScalar(self: *Writer, layout_idx: layout.Idx, stored: StoredPart) checked.ConstScalar {
        const value = stored.value();
        if (value != .scalar) writerInvariant("a constant read at a number type is not a stored scalar");
        if (builtin.mode == .debug) {
            var bytes = scalarMemoryBytes(value.scalar);
            std.debug.assert(std.meta.eql(self.storeScalar(layout_idx, .{ .ptr = &bytes }), value.scalar));
        }
        return value.scalar;
    }

    /// The blob `blob` of `source` as a blob of the store being written: the
    /// same blob when `source` is that store, which interns blobs by
    /// content, and otherwise its bytes interned there.
    fn copyBlob(self: *Writer, source: *const const_store.ConstStore, blob: const_store.ConstBlobDataId) Allocator.Error!const_store.ConstBlobDataId {
        if (source == @as(*const const_store.ConstStore, self.store)) return blob;
        return try self.store.addBlobData(source.blobData(blob));
    }

    /// Store the list of packed items `items`, which `stored` holds, as
    /// the list of a value reading it, with `capacity`.
    fn storeCopiedPackedList(self: *Writer, node: checked.ConstNodeId, stored: StoredPart, items: const_store.ConstPackedList, capacity: u64) Allocator.Error!void {
        var copied = items;
        copied.bytes = try self.copyBlob(stored.store, items.bytes);
        self.store.fill(node, .{ .list = .{ .items = .{ .packed_bytes = copied }, .capacity = capacity } });
    }

    fn storeBytes(self: *Writer, bytes: []const u8) Allocator.Error!const_store.ConstStr {
        return try self.store.addBlobData(bytes);
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
            .pending, .layout_only => writerInvariant("product eligibility reached an unfinished const plan"),
            .str, .list, .box, .boxed_sequence, .tag_union, .fn_value, .erased_fn, .boxy_fn => return false,
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

    /// Store a list of `len` products whose elements lie in `memory` as the
    /// evaluation's memory holds them.
    fn storeProductList(self: *Writer, node: checked.ConstNodeId, plan: *const lir.PackedData.Plan, memory: []const u8, len: usize, capacity: u64) Allocator.Error!void {
        const byte_len = std.math.mul(usize, len, plan.packed_width) catch unreachable; // packed leaves are disjoint parts of an element, so this is at most memory_len
        const data = if (plan.isIdentity() and builtin.cpu.arch.endian() == .little)
            try self.store.addBlobData(memory)
        else blk: {
            plan.encode(try self.store.reserveBlobData(byte_len), memory, len, builtin.cpu.arch.endian());
            break :blk try self.store.commitBlobData(byte_len);
        };
        self.store.fill(node, .{ .list = .{ .items = .{ .packed_bytes = .{
            .bytes = data,
            .len = try checkedU32(len),
            .element = null,
            .product_width = plan.packed_width,
        } }, .capacity = capacity } });
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
            .layout_only,
            .zst,
            .str,
            .list,
            .box,
            .boxed_sequence,
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
        part: Part,
        capacity: u64,
    ) Error!void {
        if (list_layout.tag != .list) writerInvariant("packed scalar list had non-list layout");
        const elem_layout = list_layout.getIdx();
        const element = self.packedScalarForLayout(elem_layout) orelse
            writerInvariant("scalar const plan had a non-packable list element layout");
        const roc_list: *const RocList = switch (part) {
            .runtime => |value| @ptrCast(@alignCast(value.ptr)),
            .checked => |expr| {
                const items = self.checkedListItems(expr);
                self.store.fill(target_node, .{ .list = .{ .items = .{ .packed_bytes = .{
                    .bytes = try self.checkedPackedBlob(elem_layout, element, items),
                    .len = try checkedU32(items.len),
                    .element = element,
                } }, .capacity = capacity } });
                return;
            },
            // A constant of this list's type stored its items packed as
            // this same scalar.
            .stored => |stored| {
                const items = storedPackedItems(stored);
                if (builtin.mode == .debug) std.debug.assert(items.element != null and items.element.? == element);
                try self.storeCopiedPackedList(target_node, stored, items, capacity);
                return;
            },
        };

        const width: usize = element.byteWidth();
        if (builtin.mode == .debug) std.debug.assert(self.program.layouts.layoutSize(self.program.layouts.getLayout(elem_layout)) == width);
        const byte_len = std.math.mul(usize, roc_list.len(), width) catch
            writerInvariant("packed list byte length overflowed");
        if (roc_list.bytes == null and roc_list.len() != 0) {
            writerInvariant("non-empty packed list had null element pointer");
        }
        const data = if (memoryIsPackedScalars(element))
            try self.store.addBlobData(if (roc_list.bytes) |source| source[0..byte_len] else &.{})
        else blk: {
            const bytes = try self.store.reserveBlobData(byte_len);
            if (roc_list.bytes) |source| {
                for (0..roc_list.len()) |index| {
                    const scalar = self.storeScalar(elem_layout, .{ .ptr = source + index * width });
                    writePackedScalar(bytes[index * width ..][0..width], element, scalar);
                }
            }
            break :blk try self.store.commitBlobData(byte_len);
        };
        self.store.fill(target_node, .{ .list = .{ .items = .{ .packed_bytes = .{
            .bytes = data,
            .len = try checkedU32(roc_list.len()),
            .element = element,
        } }, .capacity = capacity } });
    }

    fn cloneCaptureType(self: *Writer, ty: const_store.ConstTypeId) Error!const_store.ConstTypeId {
        return self.store.type_store.cloneTypeFromTranslated(
            &self.program.const_types,
            &self.program.const_type_names,
            self.typeNames(),
            ty,
        );
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

/// Whether a list of `element` scalars in this program's memory already is
/// its packed encoding (`writePackedScalar`), so the list's bytes are stored
/// as they are. An integer, `Dec` or vector is packed as its little-endian
/// bytes, which a little-endian program's memory holds, and a big-endian
/// program's does not. A float is packed with every NaN as Roc's one NaN,
/// so each item is stored on its own.
fn memoryIsPackedScalars(element: const_store.ConstPackedScalar) bool {
    return switch (builtin.cpu.arch.endian()) {
        .little => switch (element) {
            .u8, .i8, .u16, .i16, .u32, .i32, .u64, .i64, .u128, .i128, .dec, .u8x16, .i8x16, .u16x8, .i16x8, .u32x4, .i32x4, .u64x2, .i64x2 => true,
            .f32, .f64 => false,
        },
        .big => false,
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

/// The bytes a value of a number type holding `scalar` occupies in memory,
/// as an evaluated program's memory holds it and `storeScalar` reads it.
fn scalarMemoryBytes(scalar: checked.ConstScalar) [16]u8 {
    var bytes: [16]u8 = @splat(0);
    switch (scalar) {
        inline .i8, .i16, .i32, .i64, .i128, .u8, .u16, .u32, .u64, .u128, .f32_bits, .f64_bits, .dec_bits => |value| {
            @memcpy(bytes[0..@sizeOf(@TypeOf(value))], std.mem.asBytes(&value));
        },
    }
    return bytes;
}

/// The evaluated value a part is; a part only an evaluated program produces
/// (a box, a sequence, a function) is never a checked expression's.
fn runtimePart(part: Part) Value {
    return switch (part) {
        .runtime => |value| value,
        .checked, .stored => writerInvariant("a value-form root holds a part only an evaluation produces"),
    };
}

/// A part of value-form expression `expr`.
fn checkedPart(expr: CIR.Expr.Idx) Part {
    return .{ .checked = expr };
}

/// The stored list `stored`.
fn storedList(stored: StoredPart) const_store.ConstList {
    const value = stored.value();
    if (value != .list) writerInvariant("a constant read at a list type is not a stored list");
    return value.list;
}

/// The items of the stored list `stored`.
fn storedListItems(stored: StoredPart) const_store.ConstListItems {
    return storedList(stored).items;
}

/// The packed items of stored list `stored`, which a list of a type whose
/// items pack stores.
fn storedPackedItems(stored: StoredPart) const_store.ConstPackedList {
    return switch (storedListItems(stored)) {
        .packed_bytes => |items| items,
        .nodes, .empty => writerInvariant("a stored list of packed items does not hold them packed"),
    };
}

/// The item nodes of stored list `stored`, which a non-empty list of a type
/// whose items do not pack stores.
fn storedItemNodes(stored: StoredPart) []const checked.ConstNodeId {
    return switch (storedListItems(stored)) {
        .nodes => |items| items,
        .packed_bytes, .empty => writerInvariant("a stored list of items that do not pack does not hold them as nodes"),
    };
}

/// The components of stored tuple or record `stored`, `count` of them, in
/// the order its plan stores them.
fn storedComponents(stored: StoredPart, count: usize) []const checked.ConstNodeId {
    const components = switch (stored.value()) {
        .tuple => |items| items,
        .record => |fields| fields,
        .pending, .zst, .scalar, .str, .list, .box, .crash, .checked_error, .tag, .nominal, .fn_value, .boxy_fn => writerInvariant("a constant read at a tuple or record type is not a stored tuple or record"),
    };
    if (components.len != count) writerInvariant("a stored product does not have its plan's components");
    return components;
}

/// The backing of stored nominal value `stored`.
fn storedBacking(stored: StoredPart) Part {
    const value = stored.value();
    if (value != .nominal) writerInvariant("a constant read at a named type is not a stored nominal value");
    return stored.child(value.nominal.backing);
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
    try testing.expectEqualStrings(first_bytes, artifact.const_store.strBytes(first_value.str.bytes));
    try testing.expectEqualStrings(second_bytes, artifact.const_store.strBytes(second_value.str.bytes));
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
    const stored = try writer.storeRoot(testConstRoot(list_plan, str_list_layout), .{ .ptr = @ptrCast(&roc_list) });

    const list_value = artifact.const_store.get(stored.const_node);
    try testing.expect(list_value == .list);
    try testing.expect(list_value.list.items == .nodes);
    const nodes = list_value.list.items.nodes;
    try testing.expectEqual(@as(usize, 2), nodes.len);
    const expected = [_][]const u8{ "a", text[2..] };
    for (expected, nodes) |expected_piece, node| {
        const piece = artifact.const_store.get(node);
        try testing.expect(piece == .str);
        try testing.expectEqualStrings(expected_piece, artifact.const_store.strBytes(piece.str.bytes));
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
    try testing.expect(list_value.list.items == .packed_bytes);
    const scalar_bytes = list_value.list.items.packed_bytes;
    try testing.expectEqual(@as(u32, 20 * 1024), scalar_bytes.len);
    try testing.expectEqual(const_store.ConstPackedScalar.u8, scalar_bytes.element);
    try testing.expectEqual(str_value.str.bytes, scalar_bytes.bytes);
    try testing.expectEqualSlices(u8, bytes, artifact.const_store.blobData(scalar_bytes.bytes));

    const u16_scalar_bytes = artifact.const_store.get(stored_u16_list.const_node).list.items.packed_bytes;
    try testing.expectEqual(@as(u32, 10 * 1024), u16_scalar_bytes.len);
    try testing.expectEqual(const_store.ConstPackedScalar.u16, u16_scalar_bytes.element);

    // An empty list keeps the capacity it was evaluated with.
    var roc_empty_list = RocList{
        .bytes = bytes.ptr,
        .length = 0,
        .capacity_or_alloc_ptr = RocList.encodeCapacity(bytes.len),
    };
    const stored_empty_list = try writer.storeRoot(testConstRoot(list_plan, u8_list_layout), .{ .ptr = @ptrCast(&roc_empty_list) });
    try testing.expect(artifact.const_store.get(stored_empty_list.const_node).list.items == .empty);
    try testing.expectEqual(@as(u64, bytes.len), artifact.const_store.get(stored_empty_list.const_node).list.capacity);
    try testing.expectEqual(str_value.str.bytes, u16_scalar_bytes.bytes);
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
