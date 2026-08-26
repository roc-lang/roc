//! C-ABI wrappers over the boxy runtime.
//!
//! Machine-code backends lower boxy LIR statements to calls into these
//! wrappers, sharing descriptor-guided semantics with the LIR interpreter by
//! construction. At this level descriptor and dictionary handles are ordinary
//! pointer-sized values (resolved `*const BoxyTypeDesc` / `*const BoxyDict`),
//! layouts and tag names are their `u32` ids, and results are written through
//! caller-provided out-pointers.
//!
//! One selected runtime backs each wrapper invocation. Ordinary embedders
//! install a process-global default; the hot-reload shim temporarily selects
//! the runtime owned by the executing code image.
//! once before calling entrypoints—either from live stores
//! (`initGlobal`) or from a mapped image's boxy sidecar
//! (`initGlobalFromSidecarView`)—and register a native callee per worker
//! proc for dictionary dispatch (`roc_boxy_register_proc`).
//!
//! Dictionary callee ABI: a registered `BoxyProcFn` receives the active
//! `RocOps`, the explicit in-process test invocation context (null outside
//! in-process test execution), then the fully adapted argument list as an array of value
//! pointers (explicit args first, then hidden descriptor pointers, then nested
//! dictionary pointers, each passed as a pointer to a pointer-sized slot;
//! zero-sized arguments pass null), writes its result bytes through `ret` in
//! the registered return layout, and stores the result's descriptor (or null)
//! through `ret_desc`.

const std = @import("std");
const builtin = @import("builtin");
const base = @import("base");
const layout_mod = @import("layout");
const lir = @import("lir");
const builtins = @import("builtins");
const lir_value = @import("value.zig");
const boxy_runtime = @import("boxy_runtime.zig");
const BoxyBuiltinFn = @import("backend").LirCodeGenMod.BoxyBuiltinFn;

const LIR = lir.LIR;
const LirStore = lir.LirStore;
const LirProgram = lir.Program;
const RocOps = builtins.host_abi.RocOps;
const Allocator = std.mem.Allocator;
const Value = lir_value.Value;
const BoxyRuntime = boxy_runtime.BoxyRuntime;
const BoxyTables = boxy_runtime.BoxyTables;
const Error = boxy_runtime.Error;
const BoxyTypeDesc = LirProgram.BoxyTypeDesc;

const BoxyDict = LirProgram.BoxyDict;
const RocList = builtins.list.RocList;

/// Native addresses of all Boxy C-ABI wrappers, indexed by `BoxyBuiltinFn`.
pub const BoxyNativeFnTable = @import("backend").LirCodeGenMod.BoxyNativeFnTable;

/// Build the explicit function table consumed by in-process machine code.
pub fn nativeFnTable() BoxyNativeFnTable {
    var table: BoxyNativeFnTable = undefined;
    inline for (@typeInfo(BoxyBuiltinFn).@"enum".fields) |field| {
        const boxy_fn: BoxyBuiltinFn = @enumFromInt(field.value);
        const name = comptime boxy_fn.symbolName();
        table[field.value] = @intFromPtr(&@field(@This(), name));
    }
    return table;
}

/// Native callee for one dictionary worker proc. `ops` threads the active
/// `RocOps`; `args` points at one pointer per explicit argument, each
/// addressing that argument's bytes in its own layout; `ret` receives the
/// worker's result; `ret_desc` receives the result descriptor (null when the
/// worker produces none).
pub const BoxyProcFn = *const fn (
    ops: *RocOps,
    test_context: ?*anyopaque,
    args: [*]const ?*const anyopaque,
    ret: ?*anyopaque,
    ret_desc: *?*const anyopaque,
) callconv(.c) void;

/// One explicit argument to `roc_boxy_call_dict`.
pub const RocBoxyCallArg = extern struct {
    value: ?[*]const u8,
    layout: u32,
    desc: ?*const BoxyTypeDesc,
};

const RegisteredProc = struct {
    callee: BoxyProcFn,
    ret_layout: layout_mod.Idx,
    borrowed_params: u64,
    ret_borrowed: bool,
    ret_lenders: u64,
};

const RegisteredErasedProc = struct {
    proc_id: u32,
    ret_layout: layout_mod.Idx,
    metadata_offset: u32,
    arg_layouts: LIR.BoxySpan,
    arg_desc_offsets: LIR.BoxySpan,
    capture_offset_base: u32,
};

const DescCopyCacheKey = struct {
    desc_id: u32,
    capture_ids: []const u32,
    capture_descs: []const ?*const BoxyTypeDesc,
};

const DescCopyCache = std.HashMapUnmanaged(DescCopyCacheKey, *const BoxyTypeDesc, struct {
    pub fn hash(_: @This(), key: DescCopyCacheKey) u64 {
        var hasher = std.hash.Wyhash.init(0);
        std.hash.autoHash(&hasher, key.desc_id);
        std.hash.autoHash(&hasher, key.capture_ids.len);
        for (key.capture_ids) |capture_id| std.hash.autoHash(&hasher, capture_id);
        std.hash.autoHash(&hasher, key.capture_descs.len);
        for (key.capture_descs) |desc| {
            const address: usize = if (desc) |ptr| @intFromPtr(ptr) else 0;
            std.hash.autoHash(&hasher, address);
        }
        return hasher.final();
    }

    pub fn eql(_: @This(), a: DescCopyCacheKey, b: DescCopyCacheKey) bool {
        if (a.desc_id != b.desc_id or
            a.capture_ids.len != b.capture_ids.len or
            a.capture_descs.len != b.capture_descs.len)
        {
            return false;
        }
        for (a.capture_ids, b.capture_ids) |a_id, b_id| {
            if (a_id != b_id) return false;
        }
        for (a.capture_descs, b.capture_descs) |a_desc, b_desc| {
            if (a_desc != b_desc) return false;
        }
        return true;
    }
}, 80);

/// The process-global boxy runtime state behind the C-ABI wrappers.
pub const GlobalBoxyRuntime = struct {
    gpa: Allocator,
    runtime: BoxyRuntime,
    /// Carries the string literal store when the global was initialized from
    /// a sidecar view; the runtime reads tag and field names through it.
    store_shell: LirStore,
    runtime_boxy_type_descs: std.ArrayList(*const BoxyTypeDesc) = .empty,
    runtime_boxy_desc_ids: std.AutoHashMapUnmanaged(usize, u32) = .empty,
    adapter_desc_specializations: std.AutoHashMapUnmanaged(boxy_runtime.AdapterDescMergeKey, *const BoxyTypeDesc) = .empty,
    desc_copy_cache: DescCopyCache = .empty,
    runtime_boxy_desc_refs: std.ArrayList(LirProgram.BoxyDescRef) = .empty,
    runtime_boxy_tag_variants: std.ArrayList(LirProgram.BoxyTagVariant) = .empty,
    runtime_boxy_tag_payload_descs: std.ArrayList(LirProgram.BoxyTagPayloadDesc) = .empty,
    runtime_boxy_payload_steps: std.ArrayList(LirProgram.BoxyPayloadStep) = .empty,
    /// Backs runtime-materialized descriptors; they live until deinit.
    desc_arena: std.heap.ArenaAllocator,
    /// Backs value temporaries; reset when the outermost wrapper call
    /// returns.
    value_scratch: std.heap.ArenaAllocator,
    call_depth: usize = 0,
    procs: std.AutoHashMapUnmanaged(u32, RegisteredProc) = .empty,
    /// Return layout of each Roc-created erased-callable proc, keyed by the
    /// worker's runtime code address. An erased call reads the actual return
    /// layout here to materialize the worker's result into the caller's
    /// expected layout. Host-provided callables are absent (the host does not
    /// register them); their result already uses the caller's exact layout.
    erased_procs: std.AutoHashMapUnmanaged(usize, RegisteredErasedProc) = .empty,
    /// Local-id to descriptor bindings for the descriptor template being
    /// materialized by the active `roc_boxy_desc_copy` call.
    capture_ids: []const u32 = &.{},
    capture_descs: []const ?*const BoxyTypeDesc = &.{},
};

var global: ?*GlobalBoxyRuntime = null;

const ActiveRuntimeSelection = if (builtin.os.tag == .linux and !builtin.link_libc) struct {
    const Entry = struct {
        tid: std.os.linux.pid_t,
        runtime: *GlobalBoxyRuntime,
        allocator: Allocator,
        next: ?*Entry,
    };

    var lock: std.atomic.Mutex = .unlocked;
    var entries: ?*Entry = null;

    fn lockEntries() void {
        while (!lock.tryLock()) std.atomic.spinLoopHint();
    }

    fn get() ?*GlobalBoxyRuntime {
        const tid = std.os.linux.gettid();
        lockEntries();
        defer lock.unlock();

        var current = entries;
        while (current) |entry| : (current = entry.next) {
            if (entry.tid == tid) return entry.runtime;
        }
        return null;
    }

    fn swap(runtime: ?*GlobalBoxyRuntime) ?*GlobalBoxyRuntime {
        const tid = std.os.linux.gettid();
        lockEntries();

        var link = &entries;
        while (link.*) |entry| : (link = &entry.next) {
            if (entry.tid != tid) continue;
            const previous = entry.runtime;
            if (runtime) |selected| {
                entry.runtime = selected;
                lock.unlock();
            } else {
                link.* = entry.next;
                lock.unlock();
                entry.allocator.destroy(entry);
            }
            return previous;
        }

        const selected = runtime orelse {
            lock.unlock();
            return null;
        };
        const entry = selected.gpa.create(Entry) catch {
            lock.unlock();
            @panic("boxy runtime could not record the active freestanding thread");
        };
        entry.* = .{
            .tid = tid,
            .runtime = selected,
            .allocator = selected.gpa,
            .next = entries,
        };
        entries = entry;
        lock.unlock();
        return null;
    }
} else struct {
    threadlocal var runtime: ?*GlobalBoxyRuntime = null;

    fn get() ?*GlobalBoxyRuntime {
        return runtime;
    }

    fn swap(selected: ?*GlobalBoxyRuntime) ?*GlobalBoxyRuntime {
        const previous = runtime;
        runtime = selected;
        return previous;
    }
};

fn currentRuntime() ?*GlobalBoxyRuntime {
    return ActiveRuntimeSelection.get() orelse global;
}

fn requireGlobal() *GlobalBoxyRuntime {
    return currentRuntime() orelse @panic("boxy ABI wrapper called before roc_boxy runtime initialization");
}

/// Select `runtime` for boxy ABI calls on the current thread and return the
/// previously selected runtime. The machine-code shim uses this to keep each
/// hot-reload generation paired with its own sidecar tables.
pub fn swapActiveRuntime(runtime: ?*GlobalBoxyRuntime) ?*GlobalBoxyRuntime {
    return ActiveRuntimeSelection.swap(runtime);
}

fn createRuntime(
    gpa: Allocator,
    store: *const LirStore,
    layout_store: *const layout_mod.Store,
    tables: BoxyTables,
    roc_ops: *RocOps,
) error{OutOfMemory}!*GlobalBoxyRuntime {
    const g = try gpa.create(GlobalBoxyRuntime);
    errdefer gpa.destroy(g);
    g.* = .{
        .gpa = gpa,
        .store_shell = LirStore.init(gpa),
        .desc_arena = std.heap.ArenaAllocator.init(gpa),
        .value_scratch = std.heap.ArenaAllocator.init(gpa),
        .runtime = .{
            .store = store,
            .layout_store = layout_store,
            .helper = lir_value.LayoutHelper.init(layout_store),
            .boxy_tables = tables,
            .runtime_boxy_type_descs = undefined,
            .runtime_boxy_desc_ids = undefined,
            .adapter_desc_specializations = undefined,
            .runtime_boxy_desc_refs = undefined,
            .runtime_boxy_tag_variants = undefined,
            .runtime_boxy_tag_payload_descs = undefined,
            .runtime_boxy_payload_steps = undefined,
            .roc_ops = roc_ops,
            .scratch = gpa,
            .descriptor_arena = undefined,
            .eval_arena = undefined,
        },
    };
    g.runtime.runtime_boxy_type_descs = &g.runtime_boxy_type_descs;
    g.runtime.runtime_boxy_desc_ids = &g.runtime_boxy_desc_ids;
    g.runtime.adapter_desc_specializations = &g.adapter_desc_specializations;
    g.runtime.runtime_boxy_desc_refs = &g.runtime_boxy_desc_refs;
    g.runtime.runtime_boxy_tag_variants = &g.runtime_boxy_tag_variants;
    g.runtime.runtime_boxy_tag_payload_descs = &g.runtime_boxy_tag_payload_descs;
    g.runtime.runtime_boxy_payload_steps = &g.runtime_boxy_payload_steps;
    g.runtime.descriptor_arena = g.desc_arena.allocator();
    g.runtime.eval_arena = g.desc_arena.allocator();
    return g;
}

/// Create a boxy runtime from live stores without installing a process-global
/// default. Callers select it for the current thread with `swapActiveRuntime`.
pub fn createRuntimeFromStores(
    gpa: Allocator,
    store: *const LirStore,
    layout_store: *const layout_mod.Store,
    tables: BoxyTables,
    roc_ops: *RocOps,
) error{OutOfMemory}!*GlobalBoxyRuntime {
    return createRuntime(gpa, store, layout_store, tables, roc_ops);
}

/// Initialize the process-global boxy runtime from live stores. `store`,
/// `layout_store`, the table slices, and `roc_ops` must outlive the global.
pub fn initGlobal(
    gpa: Allocator,
    store: *const LirStore,
    layout_store: *const layout_mod.Store,
    tables: BoxyTables,
    roc_ops: *RocOps,
) error{ OutOfMemory, AlreadyInitialized }!void {
    if (global != null) return error.AlreadyInitialized;
    global = try createRuntime(gpa, store, layout_store, tables, roc_ops);
}

/// Update the host services used by wrappers whose ABI does not take `RocOps`
/// as an explicit argument. In-process evaluators call this before each root
/// when several roots share one installed boxy runtime.
pub fn setGlobalRocOps(roc_ops: *RocOps) void {
    const g = currentRuntime() orelse return;
    g.runtime.roc_ops = roc_ops;
}

/// Initialize the process-global boxy runtime from a mapped boxy sidecar
/// view. The view (and the buffer behind it) and `roc_ops` must outlive the
/// global.
pub fn initGlobalFromSidecarView(
    gpa: Allocator,
    view: *const lir.LirImage.BoxySidecar.View,
    roc_ops: *RocOps,
) error{ OutOfMemory, AlreadyInitialized }!void {
    if (global != null) return error.AlreadyInitialized;
    global = try createRuntimeFromSidecarView(gpa, view, roc_ops);
}

/// Create a boxy runtime from one mapped sidecar without installing it as the
/// process-global default. The returned runtime borrows `view` and its backing
/// image until `deinitRuntime`.
pub fn createRuntimeFromSidecarView(
    gpa: Allocator,
    view: *const lir.LirImage.BoxySidecar.View,
    roc_ops: *RocOps,
) error{OutOfMemory}!*GlobalBoxyRuntime {
    const tables = BoxyTables{
        .type_descs = view.tables.type_descs,
        .dicts = view.tables.dicts,
        .adapters = view.tables.adapters,
        .desc_refs = view.tables.desc_refs,
        .dict_refs = view.tables.dict_refs,
        .tag_variants = view.tables.tag_variants,
        .tag_payload_descs = view.tables.tag_payload_descs,
        .field_names = view.tables.field_names,
        .adapt_steps = view.tables.adapt_steps,
        .payload_steps = view.tables.payload_steps,
        .method_slots = view.tables.method_slots,
        .method_arg_layouts = view.tables.method_arg_layouts,
        .method_hidden_desc_sources = view.tables.method_hidden_desc_sources,
        .erased_arg_layouts = view.tables.erased_arg_layouts,
        .erased_arg_desc_keys = view.tables.erased_arg_desc_keys,
        .erased_arg_desc_offsets = view.tables.erased_arg_desc_offsets,
        .erased_arg_desc_params = view.tables.erased_arg_desc_params,
    };
    const g = try createRuntime(gpa, undefined, &view.layouts, tables, roc_ops);
    g.store_shell.strings = view.strings;
    g.runtime.store = &g.store_shell;
    return g;
}

/// Tear down one boxy runtime. The embedder owns the stores and buffers it
/// points at.
pub fn deinitRuntime(g: *GlobalBoxyRuntime) void {
    g.desc_copy_cache.deinit(g.gpa);
    g.adapter_desc_specializations.deinit(g.gpa);
    g.runtime_boxy_desc_ids.deinit(g.gpa);
    g.runtime_boxy_payload_steps.deinit(g.gpa);
    g.runtime_boxy_tag_payload_descs.deinit(g.gpa);
    g.runtime_boxy_tag_variants.deinit(g.gpa);
    g.runtime_boxy_desc_refs.deinit(g.gpa);
    g.runtime_boxy_type_descs.deinit(g.gpa);
    g.procs.deinit(g.gpa);
    g.erased_procs.deinit(g.gpa);
    g.desc_arena.deinit();
    g.value_scratch.deinit();
    g.gpa.destroy(g);
}

/// Tear down the process-global boxy runtime. The embedder owns the stores
/// and buffers the global pointed at.
pub fn deinitGlobal() void {
    const g = global orelse return;
    global = null;
    deinitRuntime(g);
}

/// Engine services for wrapper-initiated boxy operations: descriptor and
/// dictionary handles resolve against the global tables (plus the active
/// desc-copy capture bindings), values allocate from the per-call scratch
/// arena, and RC plans come uncached from the layout store.
const AbiHooks = struct {
    g: *GlobalBoxyRuntime,
    test_context: ?*anyopaque,

    pub fn resolveDescRef(self: AbiHooks, desc_ref: LIR.BoxyDescRef) Error!*const BoxyTypeDesc {
        return switch (desc_ref) {
            .static => |desc_id| self.g.runtime.requireBoxyTypeDesc(desc_id),
            .runtime => |runtime_id| blk: {
                if (runtime_id >= self.g.runtime_boxy_type_descs.items.len) {
                    return error.RuntimeError;
                }
                break :blk self.g.runtime_boxy_type_descs.items[runtime_id];
            },
            .local => |local| blk: {
                for (self.g.capture_ids, self.g.capture_descs) |capture_id, capture_desc| {
                    if (capture_id == @intFromEnum(local)) {
                        break :blk capture_desc orelse abiCrashMissingDescriptorCapture(self.g, local, true);
                    }
                }
                abiCrashMissingDescriptorCapture(self.g, local, false);
            },
            .dict_method_arg, .dict_method_hidden => return error.RuntimeError,
        };
    }

    pub fn resolveDictRef(self: AbiHooks, dict_ref: LIR.BoxyDictRef) Error!*const BoxyDict {
        return switch (dict_ref) {
            .static => |dict_id| self.g.runtime.requireBoxyDict(dict_id),
            .local => error.RuntimeError,
        };
    }

    pub fn callInspectMethod(
        self: AbiHooks,
        method: LirProgram.BoxyMethodSlotId,
        value: Value,
        value_layout: layout_mod.Idx,
        desc: *const BoxyTypeDesc,
    ) Error!boxy_runtime.InspectCallResult {
        const scratch = self.g.value_scratch.allocator();
        const prepared = try self.g.runtime.prepareInspectCall(
            self,
            scratch,
            method,
            .{ .value = value, .layout = value_layout, .source_desc = desc },
        );
        const registered = self.g.procs.get(@intFromEnum(prepared.proc)) orelse return error.RuntimeError;
        if (prepared.arg_values.len == 0) return error.RuntimeError;
        const argument_is_borrowed = (prepared.borrowed_args & 1) != 0;
        const worker_borrows_argument = (registered.borrowed_params & 1) != 0;
        if (argument_is_borrowed and !worker_borrows_argument) {
            try self.g.runtime.performBoxyLayoutDrop(
                self,
                prepared.arg_values[0],
                prepared.arg_layouts[0],
                prepared.arg_descs[0],
                .incref,
                1,
                .atomic,
            );
        }

        const arg_ptrs = try scratch.alloc(?*const anyopaque, prepared.arg_values.len);
        for (prepared.arg_values, prepared.arg_layouts, 0..) |arg_value, arg_layout, i| {
            arg_ptrs[i] = if (self.g.runtime.helper.sizeOf(arg_layout) == 0) null else @ptrCast(arg_value.ptr);
        }
        const ret_value = try self.allocValue(registered.ret_layout);
        const ret_size = self.g.runtime.helper.sizeOf(registered.ret_layout);
        var ret_desc: ?*const anyopaque = null;
        registered.callee(
            self.g.runtime.roc_ops,
            self.test_context,
            arg_ptrs.ptr,
            if (ret_size == 0) null else @ptrCast(ret_value.ptr),
            &ret_desc,
        );
        if (!argument_is_borrowed and worker_borrows_argument) {
            try self.g.runtime.performBoxyLayoutDrop(
                self,
                prepared.arg_values[0],
                prepared.arg_layouts[0],
                prepared.arg_descs[0],
                .decref,
                1,
                .atomic,
            );
        }
        return .{
            .value = ret_value,
            .layout = registered.ret_layout,
            .desc = @ptrCast(@alignCast(ret_desc)),
            .borrowed = registered.ret_borrowed,
        };
    }

    pub fn allocValue(self: AbiHooks, layout_idx: layout_mod.Idx) Error!Value {
        const sa = self.g.runtime.helper.sizeAlignOf(layout_idx);
        if (sa.size == 0) return Value.zst;
        const scratch = self.g.value_scratch.allocator();
        const slice = switch (sa.alignment) {
            .@"1" => scratch.alignedAlloc(u8, .@"1", sa.size),
            .@"2" => scratch.alignedAlloc(u8, .@"2", sa.size),
            .@"4" => scratch.alignedAlloc(u8, .@"4", sa.size),
            .@"8" => scratch.alignedAlloc(u8, .@"8", sa.size),
            .@"16" => scratch.alignedAlloc(u8, .@"16", sa.size),
            _ => unreachable,
        } catch return error.OutOfMemory;
        @memset(slice, 0);
        return Value.fromSlice(slice);
    }

    pub fn allocRocDataWithRc(self: AbiHooks, data_bytes: usize, element_alignment: u32, elements_refcounted: bool) Error![*]u8 {
        return builtins.utils.allocateWithRefcount(data_bytes, element_alignment, elements_refcounted, self.g.runtime.roc_ops);
    }

    pub fn layoutContainsRc(self: AbiHooks, layout_idx: layout_mod.Idx) bool {
        const store = self.g.runtime.layout_store;
        return store.layoutContainsRefcounted(store.getLayout(layout_idx));
    }

    pub fn rcPlanFor(self: AbiHooks, helper: layout_mod.RcHelperKey) layout_mod.RcHelperPlan {
        return self.g.runtime.layout_store.rcHelperPlan(helper);
    }

    pub fn rcStructFieldPlan(self: AbiHooks, struct_plan: layout_mod.RcStructPlan, field_index: u32) ?layout_mod.RcFieldPlan {
        return self.g.runtime.layout_store.rcHelperStructFieldPlan(struct_plan, field_index);
    }

    pub fn rcTagVariantPlan(self: AbiHooks, tag_plan: layout_mod.RcTagUnionPlan, variant_index: u32) ?layout_mod.RcHelperKey {
        return self.g.runtime.layout_store.rcHelperTagUnionVariantPlan(tag_plan, variant_index);
    }

    pub fn traceProcId(_: AbiHooks) u32 {
        return std.math.maxInt(u32);
    }

    pub fn debugDumpProc(_: AbiHooks) void {}
};

fn hooks(g: *GlobalBoxyRuntime) AbiHooks {
    return .{ .g = g, .test_context = null };
}

fn hooksWithTestContext(g: *GlobalBoxyRuntime, test_context: ?*anyopaque) AbiHooks {
    return .{ .g = g, .test_context = test_context };
}

fn enter(g: *GlobalBoxyRuntime) void {
    g.call_depth += 1;
}

fn leave(g: *GlobalBoxyRuntime) void {
    g.call_depth -= 1;
    if (g.call_depth == 0) {
        _ = g.value_scratch.reset(.retain_capacity);
    }
}

fn abiCrash(g: *GlobalBoxyRuntime, comptime what: []const u8) noreturn {
    g.runtime.roc_ops.crash("boxy runtime " ++ what ++ " failed");
    unreachable;
}

/// Fixed-buffer message builder for the crash paths below.
///
/// These messages carry ids and keys that are worth having in a crash report,
/// but `std.fmt` instantiates a distinct formatter per argument tuple, and
/// `{any}` over a slice of structs instantiates one per field shape. In an
/// object whose only output is `RocOps.crash`, that links the entire `std.Io`
/// writer stack. Appending decimal digits directly keeps the diagnostics and
/// leaves the object free of the formatter. Writes past the buffer are dropped,
/// so a long id list truncates rather than failing the crash report.
const CrashMessage = struct {
    buf: [256]u8 = undefined,
    len: usize = 0,

    fn str(self: *CrashMessage, chunk: []const u8) void {
        const room = self.buf.len - self.len;
        const n = @min(room, chunk.len);
        @memcpy(self.buf[self.len..][0..n], chunk[0..n]);
        self.len += n;
    }

    fn uint(self: *CrashMessage, value: u64) void {
        var digits: [20]u8 = undefined;
        var i: usize = digits.len;
        var v = value;
        while (true) {
            i -= 1;
            digits[i] = '0' + @as(u8, @intCast(v % 10));
            v /= 10;
            if (v == 0) break;
        }
        self.str(digits[i..]);
    }

    fn keyList(self: *CrashMessage, keys: []const LIR.ErasedArgDescKey) void {
        self.str("[");
        for (keys, 0..) |key, index| {
            if (index != 0) self.str(", ");
            self.str("(");
            self.uint(key.arg_index);
            self.str(", ");
            self.uint(key.descriptor_index);
            self.str(")");
        }
        self.str("]");
    }

    fn uintList(self: *CrashMessage, values: []const u32) void {
        self.str("[");
        for (values, 0..) |value, index| {
            if (index != 0) self.str(", ");
            self.uint(value);
        }
        self.str("]");
    }

    fn text(self: *const CrashMessage) []const u8 {
        return self.buf[0..self.len];
    }
};

fn abiCrashMissingDescriptorCapture(
    g: *GlobalBoxyRuntime,
    local: LIR.LocalId,
    supplied_null: bool,
) noreturn {
    var message: CrashMessage = .{};
    message.str("boxy runtime descriptor capture local ");
    message.uint(@intFromEnum(local));
    message.str(if (supplied_null) " was null" else " was missing");
    message.str("; supplied capture ids=");
    message.uintList(g.capture_ids);
    g.runtime.roc_ops.crash(message.text());
    unreachable;
}

fn abiCrashNullErasedArgDescriptor(
    g: *GlobalBoxyRuntime,
    proc_id: u32,
    key: LIR.ErasedArgDescKey,
) noreturn {
    var message: CrashMessage = .{};
    message.str("boxy runtime erased call supplied a null descriptor to proc ");
    message.uint(proc_id);
    message.str(" for key (");
    message.uint(key.arg_index);
    message.str(", ");
    message.uint(key.descriptor_index);
    message.str(")");
    g.runtime.roc_ops.crash(message.text());
    unreachable;
}

fn abiCrashMissingErasedArgDescriptor(
    g: *GlobalBoxyRuntime,
    proc_id: u32,
    key: LIR.ErasedArgDescKey,
    supplied_keys: []const LIR.ErasedArgDescKey,
) noreturn {
    var message: CrashMessage = .{};
    message.str("boxy runtime erased proc ");
    message.uint(proc_id);
    message.str(" required descriptor key (");
    message.uint(key.arg_index);
    message.str(", ");
    message.uint(key.descriptor_index);
    message.str("); supplied keys=");
    message.keyList(supplied_keys);
    g.runtime.roc_ops.crash(message.text());
    unreachable;
}

fn abiCrashDuplicateErasedArgDescriptor(
    g: *GlobalBoxyRuntime,
    proc_id: u32,
    key: LIR.ErasedArgDescKey,
) noreturn {
    var message: CrashMessage = .{};
    message.str("boxy runtime erased call supplied duplicate descriptors to proc ");
    message.uint(proc_id);
    message.str(" for key (");
    message.uint(key.arg_index);
    message.str(", ");
    message.uint(key.descriptor_index);
    message.str(")");
    g.runtime.roc_ops.crash(message.text());
    unreachable;
}

fn layoutIdx(raw: u32) layout_mod.Idx {
    return @enumFromInt(raw);
}

fn valueAt(ptr: ?[*]const u8) Value {
    const p = ptr orelse return Value.zst;
    return .{ .ptr = @constCast(p) };
}

fn writeResult(g: *GlobalBoxyRuntime, out: ?[*]u8, result: Value, result_layout: layout_mod.Idx) void {
    const size = g.runtime.helper.sizeOf(result_layout);
    if (size == 0) return;
    const out_ptr = out orelse abiCrash(g, "result write without an out pointer");
    @memcpy(out_ptr[0..size], result.readBytes(size));
}

const BoxyListElementContext = struct {
    g: *GlobalBoxyRuntime,
    elem_layout: layout_mod.Idx,
    elem_desc: *const BoxyTypeDesc,
};

fn boxyListElementIncref(context: ?*anyopaque, element: ?[*]u8) callconv(.c) void {
    const ctx: *const BoxyListElementContext = @ptrCast(@alignCast(context orelse return));
    const value = if (element) |ptr| Value{ .ptr = ptr } else Value.zst;
    ctx.g.runtime.performBoxyLayoutDrop(
        hooks(ctx.g),
        value,
        ctx.elem_layout,
        ctx.elem_desc,
        .incref,
        1,
        .atomic,
    ) catch abiCrash(ctx.g, "list element incref");
}

fn boxyListElementDecref(context: ?*anyopaque, element: ?[*]u8) callconv(.c) void {
    const ctx: *const BoxyListElementContext = @ptrCast(@alignCast(context orelse return));
    const value = if (element) |ptr| Value{ .ptr = ptr } else Value.zst;
    ctx.g.runtime.performBoxyLayoutDrop(
        hooks(ctx.g),
        value,
        ctx.elem_layout,
        ctx.elem_desc,
        .decref,
        1,
        .atomic,
    ) catch abiCrash(ctx.g, "list element decref");
}

fn boxyListElementContext(
    g: *GlobalBoxyRuntime,
    list_desc: *const BoxyTypeDesc,
    elem_layout: u32,
) BoxyListElementContext {
    const resolved_elem_layout = layoutIdx(elem_layout);
    const elem_desc = (g.runtime.firstNestedBoxyDesc(hooks(g), list_desc) catch abiCrash(g, "list element descriptor")) orelse
        abiCrash(g, "missing list element descriptor");
    return .{
        .g = g,
        .elem_layout = resolved_elem_layout,
        .elem_desc = elem_desc,
    };
}

/// Register the native callee and return layout for one dictionary worker
/// proc. `roc_boxy_call_dict` dispatches slots that reference `proc_id` to
/// `callee`. An entrypoint registers every worker at startup; a program that
/// never installs the runtime (no descriptors or dictionaries) also never
/// dispatches a dictionary call, so registration is a no-op there.
pub fn roc_boxy_register_proc(
    proc_id: u32,
    callee: BoxyProcFn,
    ret_layout: u32,
    borrowed_params: u64,
    ret_borrowed: bool,
    ret_lenders: u64,
) callconv(.c) void {
    const g = currentRuntime() orelse return;
    g.procs.put(g.gpa, proc_id, .{
        .callee = callee,
        .ret_layout = layoutIdx(ret_layout),
        .borrowed_params = borrowed_params,
        .ret_borrowed = ret_borrowed,
        .ret_lenders = ret_lenders,
    }) catch abiCrash(g, "proc registration");
}

/// Record the return layout of one Roc-created erased-callable proc under its
/// runtime code address. `roc_boxy_call_erased` consults this to reconcile a
/// worker whose actual return layout differs from the call site's expected
/// layout. Registration happens as each erased callable value is built, so the
/// address is the relocated runtime address.
pub fn roc_boxy_register_erased_proc(
    fn_ptr: ?*const anyopaque,
    proc_id: u32,
    ret_layout: u32,
    metadata_offset: u32,
    arg_layouts_start: u32,
    arg_layouts_len: u32,
    arg_desc_offsets_start: u32,
    arg_desc_offsets_len: u32,
    capture_offset_base: u32,
) callconv(.c) void {
    const g = currentRuntime() orelse return;
    const ptr = fn_ptr orelse return;
    const offsets_end = @as(usize, arg_desc_offsets_start) + arg_desc_offsets_len;
    if (offsets_end > g.runtime.boxy_tables.erased_arg_desc_offsets.len) {
        abiCrash(g, "erased proc descriptor-offset registration");
    }
    const layouts_end = @as(usize, arg_layouts_start) + arg_layouts_len;
    if (layouts_end > g.runtime.boxy_tables.erased_arg_layouts.len) {
        abiCrash(g, "erased proc argument-layout registration");
    }
    g.erased_procs.put(g.gpa, @intFromPtr(ptr), .{
        .proc_id = proc_id,
        .ret_layout = layoutIdx(ret_layout),
        .metadata_offset = metadata_offset,
        .arg_layouts = .{
            .start = arg_layouts_start,
            .len = arg_layouts_len,
        },
        .arg_desc_offsets = .{
            .start = arg_desc_offsets_start,
            .len = arg_desc_offsets_len,
        },
        .capture_offset_base = capture_offset_base,
    }) catch abiCrash(g, "erased proc registration");
}

fn erasedInvocationCapture(
    g: *GlobalBoxyRuntime,
    registered: RegisteredErasedProc,
    capture: [*]u8,
    arg_descs: ?[*]const ?*const BoxyTypeDesc,
    arg_desc_keys_start: u32,
    arg_desc_keys_len: u32,
) [*]u8 {
    const keys_end = @as(usize, arg_desc_keys_start) + arg_desc_keys_len;
    if (keys_end > g.runtime.boxy_tables.erased_arg_desc_keys.len) {
        abiCrash(g, "erased call descriptor keys");
    }
    const keys = g.runtime.boxy_tables.erased_arg_desc_keys[arg_desc_keys_start..keys_end];
    const offsets_start: usize = registered.arg_desc_offsets.start;
    const offsets_end = offsets_start + registered.arg_desc_offsets.len;
    if (offsets_end > g.runtime.boxy_tables.erased_arg_desc_offsets.len) {
        abiCrash(g, "erased proc descriptor offsets");
    }
    const offsets = g.runtime.boxy_tables.erased_arg_desc_offsets[offsets_start..offsets_end];
    if (offsets.len == 0) return capture;
    const descs = arg_descs orelse abiCrash(g, "erased call descriptor operands");

    const capture_len: usize = registered.metadata_offset;
    const invocation = g.value_scratch.allocator().alignedAlloc(
        u8,
        std.mem.Alignment.fromByteUnits(builtins.erased_callable.capture_alignment),
        capture_len,
    ) catch abiCrash(g, "erased invocation capture allocation");
    @memcpy(invocation, capture[0..capture_len]);

    for (offsets) |offset| {
        var matching_desc_index: ?usize = null;
        for (keys, 0..) |key, desc_index| {
            if (!std.meta.eql(key, offset.key)) continue;
            if (matching_desc_index != null) {
                abiCrashDuplicateErasedArgDescriptor(g, registered.proc_id, offset.key);
            }
            matching_desc_index = desc_index;
        }
        const desc_index = matching_desc_index orelse
            abiCrashMissingErasedArgDescriptor(g, registered.proc_id, offset.key, keys);
        const destination = @as(usize, registered.capture_offset_base) + offset.offset;
        if (destination + @sizeOf(?*const BoxyTypeDesc) > invocation.len) {
            abiCrash(g, "erased invocation descriptor destination");
        }
        const desc = descs[desc_index] orelse
            abiCrashNullErasedArgDescriptor(g, registered.proc_id, offset.key);
        const payload_index = @intFromEnum(desc.payload_layout);
        if (payload_index >= g.runtime.layout_store.layoutCount()) {
            abiCrash(g, "erased invocation descriptor payload layout");
        }
        @memcpy(
            invocation[destination..][0..@sizeOf(?*const BoxyTypeDesc)],
            std.mem.asBytes(&@as(?*const BoxyTypeDesc, desc)),
        );
    }
    return invocation.ptr;
}

fn erasedArgLayouts(
    g: *GlobalBoxyRuntime,
    span: LIR.BoxySpan,
    comptime what: []const u8,
) []const layout_mod.Idx {
    const end = @as(usize, span.start) + span.len;
    if (end > g.runtime.boxy_tables.erased_arg_layouts.len) abiCrash(g, what);
    return g.runtime.boxy_tables.erased_arg_layouts[span.start..end];
}

fn erasedCallDescKeys(
    g: *GlobalBoxyRuntime,
    start: u32,
    len: u32,
) []const LIR.ErasedArgDescKey {
    const end = @as(usize, start) + len;
    if (end > g.runtime.boxy_tables.erased_arg_desc_keys.len) {
        abiCrash(g, "erased call descriptor keys");
    }
    return g.runtime.boxy_tables.erased_arg_desc_keys[start..end];
}

fn erasedRootArgDesc(
    g: *GlobalBoxyRuntime,
    arg_index: usize,
    arg_descs: ?[*]const ?*const BoxyTypeDesc,
    keys: []const LIR.ErasedArgDescKey,
) ?*const BoxyTypeDesc {
    if (keys.len == 0) return null;
    const descs = arg_descs orelse abiCrash(g, "erased call descriptor operands");
    var result: ?*const BoxyTypeDesc = null;
    for (keys, 0..) |key, index| {
        if (key.arg_index != arg_index or key.descriptor_index != 0) continue;
        if (result != null) abiCrash(g, "duplicate erased root argument descriptor");
        result = descs[index];
    }
    return result;
}

fn prepareErasedInvocationArgs(
    g: *GlobalBoxyRuntime,
    registered: RegisteredErasedProc,
    args: ?[*]const u8,
    call_layouts_span: LIR.BoxySpan,
    arg_descs: ?[*]const ?*const BoxyTypeDesc,
    arg_desc_keys_start: u32,
    arg_desc_keys_len: u32,
) ?[*]const u8 {
    const source_layouts = erasedArgLayouts(g, call_layouts_span, "erased call argument layouts");
    const target_layouts = erasedArgLayouts(g, registered.arg_layouts, "erased proc argument layouts");
    if (source_layouts.len != target_layouts.len) {
        abiCrash(g, "erased call argument-layout arity");
    }
    if (source_layouts.len == 0) return null;

    var layouts_match = true;
    for (source_layouts, target_layouts) |source_layout, target_layout| {
        layouts_match = layouts_match and source_layout == target_layout;
    }
    if (layouts_match) return args;

    const keys = erasedCallDescKeys(g, arg_desc_keys_start, arg_desc_keys_len);
    var target_size: u32 = 0;
    for (target_layouts) |target_layout| {
        const sa = g.runtime.helper.sizeAlignOf(target_layout);
        target_size = std.mem.alignForward(u32, target_size, @intCast(@max(sa.alignment.toByteUnits(), 1)));
        target_size += sa.size;
    }
    const packed_args = g.value_scratch.allocator().alignedAlloc(
        u8,
        .@"16",
        @max(target_size, 1),
    ) catch abiCrash(g, "erased argument buffer allocation");
    @memset(packed_args, 0);

    var source_offset: u32 = 0;
    var target_offset: u32 = 0;
    for (source_layouts, target_layouts, 0..) |source_layout, target_layout, arg_index| {
        const source_sa = g.runtime.helper.sizeAlignOf(source_layout);
        const target_sa = g.runtime.helper.sizeAlignOf(target_layout);
        source_offset = std.mem.alignForward(u32, source_offset, @intCast(@max(source_sa.alignment.toByteUnits(), 1)));
        target_offset = std.mem.alignForward(u32, target_offset, @intCast(@max(target_sa.alignment.toByteUnits(), 1)));

        const source_value = if (source_sa.size == 0)
            Value.zst
        else
            Value{ .ptr = @constCast((args orelse abiCrash(g, "erased call argument buffer")) + source_offset) };
        const target_value = if (source_layout == target_layout)
            source_value
        else
            g.runtime.materializeErasedCallArgument(
                hooks(g),
                source_value,
                source_layout,
                erasedRootArgDesc(g, arg_index, arg_descs, keys),
                target_layout,
            ) catch abiCrash(g, "erased call argument materialization");
        if (target_sa.size > 0) {
            @memcpy(packed_args[target_offset..][0..target_sa.size], target_value.readBytes(target_sa.size));
        }
        source_offset += source_sa.size;
        target_offset += target_sa.size;
    }
    return packed_args.ptr;
}

/// Invoke an erased callable and deliver its result in the caller's expected
/// layout. When the callable is a registered Roc worker whose actual return
/// layout differs from `expected_layout`, the worker writes into a scratch
/// buffer of its own layout and the result is materialized into the caller's
/// layout through the target descriptor. When the callable is unregistered or
/// its actual layout already equals the expected layout, the callable writes
/// the caller's buffer directly. `in_process` selects the ABI the callable was
/// compiled against and applies on every one of those paths.
pub fn roc_boxy_call_erased(
    ops: *RocOps,
    test_context: ?*anyopaque,
    in_process: bool,
    fn_ptr: ?*const anyopaque,
    ret: ?[*]u8,
    args: ?[*]const u8,
    capture: ?[*]u8,
    reuse: ?[*]u8,
    out_desc: *?*const BoxyTypeDesc,
    result_desc: ?*const BoxyTypeDesc,
    expected_layout: u32,
    arg_descs: ?[*]const ?*const BoxyTypeDesc,
    arg_desc_keys_start: u32,
    arg_desc_keys_len: u32,
    arg_layouts_start: u32,
    arg_layouts_len: u32,
) callconv(.c) void {
    const raw = fn_ptr orelse @panic("boxy erased call with null function pointer");
    const expected = layoutIdx(expected_layout);

    // Without an installed runtime there are no registered erased procs, so
    // every erased result already uses the caller's exact layout.
    const g = currentRuntime() orelse {
        var returned_desc: ?*const anyopaque = @ptrCast(result_desc);
        invokeErasedCallable(raw, in_process, test_context, ops, ret, args, capture, reuse, &returned_desc);
        out_desc.* = if (returned_desc) |desc| @ptrCast(@alignCast(desc)) else null;
        return;
    };

    const actual = g.erased_procs.get(@intFromPtr(raw));
    if (actual == null) {
        var returned_desc: ?*const anyopaque = @ptrCast(result_desc);
        invokeErasedCallable(raw, in_process, test_context, g.runtime.roc_ops, ret, args, capture, reuse, &returned_desc);
        out_desc.* = if (returned_desc) |desc| @ptrCast(@alignCast(desc)) else null;
        return;
    }

    const capture_ptr = capture orelse @panic("registered boxy erased callable had no capture pointer");
    const metadata = builtins.erased_callable.compilerMetadataPtr(capture_ptr, actual.?.metadata_offset);
    const metadata_desc: ?*const BoxyTypeDesc = if (metadata.result_desc) |ptr| @ptrCast(@alignCast(ptr)) else null;
    enter(g);
    defer leave(g);
    const invocation_capture = erasedInvocationCapture(
        g,
        actual.?,
        capture_ptr,
        arg_descs,
        arg_desc_keys_start,
        arg_desc_keys_len,
    );
    const invocation_args = prepareErasedInvocationArgs(
        g,
        actual.?,
        args,
        .{ .start = arg_layouts_start, .len = arg_layouts_len },
        arg_descs,
        arg_desc_keys_start,
        arg_desc_keys_len,
    );
    if (actual.?.ret_layout == expected and result_desc == null) {
        var returned_desc: ?*const anyopaque = @ptrCast(metadata_desc);
        invokeErasedCallable(raw, in_process, test_context, g.runtime.roc_ops, ret, invocation_args, invocation_capture, reuse, &returned_desc);
        out_desc.* = if (returned_desc) |desc| @ptrCast(@alignCast(desc)) else null;
        return;
    }

    const actual_layout = actual.?.ret_layout;
    const actual_size = g.runtime.helper.sizeOf(actual_layout);
    const worker_result = hooks(g).allocValue(actual_layout) catch abiCrash(g, "erased call result buffer");
    var returned_desc: ?*const anyopaque = @ptrCast(metadata_desc);
    invokeErasedCallable(raw, in_process, test_context, g.runtime.roc_ops, if (actual_size == 0) null else @ptrCast(worker_result.ptr), invocation_args, invocation_capture, reuse, &returned_desc);
    const actual_desc: ?*const BoxyTypeDesc = if (returned_desc) |desc| @ptrCast(@alignCast(desc)) else null;
    const materialized = g.runtime.materializeCallResult(
        hooks(g),
        worker_result,
        actual_layout,
        actual_desc,
        result_desc,
        expected,
    ) catch abiCrash(g, "erased call result materialization");
    writeResult(g, ret, materialized.value, expected);
    out_desc.* = materialized.desc;
}

const InProcessErasedCallableFn = *const fn (
    ops: *RocOps,
    test_context: ?*anyopaque,
    ret: ?[*]u8,
    args: ?[*]const u8,
    capture: ?[*]u8,
    reuse: ?[*]u8,
    out_desc: *?*const anyopaque,
) callconv(.c) void;

/// Invoke an erased callable through whichever ABI it was compiled against:
/// backends that emit in-process callables pass the test-invocation context as
/// a second parameter, everything else uses the plain public erased ABI.
fn invokeErasedCallable(
    raw: *const anyopaque,
    in_process: bool,
    test_context: ?*anyopaque,
    ops: *RocOps,
    ret: ?[*]u8,
    args: ?[*]const u8,
    capture: ?[*]u8,
    reuse: ?[*]u8,
    out_desc: *?*const anyopaque,
) void {
    if (in_process) {
        const callable: InProcessErasedCallableFn = @ptrCast(@alignCast(raw));
        callable(ops, test_context, ret, args, capture, reuse, out_desc);
    } else {
        const callable: builtins.erased_callable.ErasedCallableFn = @ptrCast(@alignCast(raw));
        callable(ops, ret, args, capture, reuse, out_desc);
    }
}

/// Box a payload into dynamic storage. Writes the boxed value through `out`
/// and the target local's descriptor through `out_desc`.
pub fn roc_boxy_box(
    out: ?[*]u8,
    out_desc: *?*const BoxyTypeDesc,
    payload: ?[*]const u8,
    payload_layout: u32,
    source_desc: ?*const BoxyTypeDesc,
    payload_desc: *const BoxyTypeDesc,
    payload_mode: u8,
    target_layout: u32,
) callconv(.c) void {
    const g = requireGlobal();
    enter(g);
    defer leave(g);
    const boxed = g.runtime.boxyBoxValue(
        hooks(g),
        valueAt(payload),
        layoutIdx(payload_layout),
        source_desc orelse payload_desc,
        payload_desc,
        @enumFromInt(payload_mode),
        layoutIdx(target_layout),
    ) catch abiCrash(g, "box");
    writeResult(g, out, boxed.value, layoutIdx(target_layout));
    out_desc.* = boxed.desc;
}

/// Read a dynamic box's payload back out. Writes the payload value through
/// `out` and the target local's descriptor through `out_desc`.
pub fn roc_boxy_unbox(
    out: ?[*]u8,
    out_desc: *?*const BoxyTypeDesc,
    source: ?[*]const u8,
    source_layout: u32,
    source_desc: *const BoxyTypeDesc,
    target_desc: ?*const BoxyTypeDesc,
    target_layout: u32,
    source_mode: u8,
) callconv(.c) void {
    const g = requireGlobal();
    enter(g);
    defer leave(g);
    const unboxed = g.runtime.boxyUnboxValue(
        hooks(g),
        valueAt(source),
        layoutIdx(source_layout),
        source_desc,
        target_desc,
        layoutIdx(target_layout),
        @enumFromInt(source_mode),
    ) catch abiCrash(g, "unbox");
    writeResult(g, out, unboxed.value, layoutIdx(target_layout));
    out_desc.* = unboxed.desc;
}

/// Execute one explicit representation adapter from the program side table.
pub fn roc_boxy_adapt(
    out: ?[*]u8,
    out_desc: *?*const BoxyTypeDesc,
    source: ?[*]const u8,
    source_desc: ?*const BoxyTypeDesc,
    target_desc: ?*const BoxyTypeDesc,
    adapter_id: u32,
    source_mode: u8,
) callconv(.c) void {
    const g = requireGlobal();
    enter(g);
    defer leave(g);
    const adapter: LIR.BoxyAdapterId = @enumFromInt(adapter_id);
    const planned = g.runtime.requireBoxyAdapter(adapter);
    if (source_desc) |desc| {
        const payload_index = @intFromEnum(desc.payload_layout);
        if (payload_index >= g.runtime.layout_store.layoutCount()) {
            abiCrash(g, "adapter source descriptor payload layout");
        }
    }
    if (target_desc) |desc| {
        const payload_index = @intFromEnum(desc.payload_layout);
        if (payload_index >= g.runtime.layout_store.layoutCount()) {
            abiCrash(g, "adapter target descriptor payload layout");
        }
    }
    const adapted = g.runtime.boxyAdaptValue(
        hooks(g),
        valueAt(source),
        source_desc,
        target_desc,
        adapter,
        @enumFromInt(source_mode),
    ) catch abiCrash(g, "adapt");
    writeResult(g, out, adapted.value, planned.target_layout);
    out_desc.* = adapted.desc;
}

/// Construct a tag value guided by the target descriptor, encoding through
/// the row extension when the tag is not local to the descriptor.
pub fn roc_boxy_tag(
    out: ?[*]u8,
    target_desc: *const BoxyTypeDesc,
    tag_name: u32,
    payload: ?[*]const u8,
    payload_layout: u32,
    payload_desc: ?*const BoxyTypeDesc,
    payload_mode: u8,
    target_layout: u32,
) callconv(.c) void {
    const g = requireGlobal();
    enter(g);
    defer leave(g);
    const constructed = g.runtime.constructBoxyTagValue(
        hooks(g),
        target_desc,
        @enumFromInt(tag_name),
        if (payload) |p| Value{ .ptr = @constCast(p) } else null,
        layoutIdx(payload_layout),
        payload_desc,
        @enumFromInt(payload_mode),
        layoutIdx(target_layout),
    ) catch abiCrash(g, "tag construction");
    writeResult(g, out, constructed, layoutIdx(target_layout));
}

/// Read one payload of a tag value by tag name. Writes the payload through
/// `out` and the payload's resolved descriptor (or null) through `out_desc`.
pub fn roc_boxy_tag_payload(
    out: ?[*]u8,
    out_desc: *?*const BoxyTypeDesc,
    source: ?[*]const u8,
    source_layout: u32,
    source_desc: *const BoxyTypeDesc,
    tag_name: u32,
    payload_index: u32,
    target_layout: u32,
    source_mode: u8,
) callconv(.c) void {
    const g = requireGlobal();
    enter(g);
    defer leave(g);
    const read = g.runtime.readBoxyTagPayloadByName(
        hooks(g),
        valueAt(source),
        layoutIdx(source_layout),
        source_desc,
        @enumFromInt(tag_name),
        payload_index,
        layoutIdx(target_layout),
        @enumFromInt(source_mode),
    ) catch abiCrash(g, "tag payload read");
    writeResult(g, out, read.value, layoutIdx(target_layout));
    out_desc.* = if (read.desc) |desc_ref|
        hooks(g).resolveDescRef(desc_ref) catch abiCrash(g, "tag payload descriptor resolution")
    else
        null;
}

/// Descriptor-guided structural equality.
pub fn roc_boxy_eq(
    lhs: ?[*]const u8,
    rhs: ?[*]const u8,
    value_layout: u32,
    desc: *const BoxyTypeDesc,
) callconv(.c) bool {
    const g = requireGlobal();
    enter(g);
    defer leave(g);
    return g.runtime.boxyValuesEqual(
        hooks(g),
        valueAt(lhs),
        valueAt(rhs),
        layoutIdx(value_layout),
        desc,
    ) catch abiCrash(g, "equality");
}

/// Render a descriptor-guided inspect string for a boxy value, writing the
/// resulting `RocStr` through `out`.
pub fn roc_boxy_inspect(
    out: ?[*]u8,
    test_context: ?*anyopaque,
    source: ?[*]const u8,
    source_layout: u32,
    desc: *const BoxyTypeDesc,
) callconv(.c) void {
    const g = requireGlobal();
    enter(g);
    defer leave(g);

    // Render into the per-call scratch arena so the accumulated bytes are
    // released when the outermost wrapper call returns.
    const scratch = g.value_scratch.allocator();
    const saved_arena = g.runtime.eval_arena;
    g.runtime.eval_arena = scratch;
    defer g.runtime.eval_arena = saved_arena;

    var bytes = std.ArrayList(u8).empty;
    g.runtime.appendBoxyInspect(
        hooksWithTestContext(g, test_context),
        &bytes,
        valueAt(source),
        layoutIdx(source_layout),
        desc,
    ) catch abiCrash(g, "inspect");

    const rendered = builtins.str.RocStr.fromSlice(bytes.items, g.runtime.roc_ops);
    const out_ptr = out orelse abiCrash(g, "inspect result write without an out pointer");
    const out_str: *align(1) builtins.str.RocStr = @ptrCast(out_ptr);
    out_str.* = rendered;
}

/// Descriptor-guided refcount operation (`op`: 0 = incref, 1 = decref,
/// 2 = free; `atomicity`: 0 = atomic, 1 = single-thread).
pub fn roc_boxy_drop(
    value: ?[*]u8,
    value_layout: u32,
    desc: ?*const BoxyTypeDesc,
    op: u8,
    count: u16,
    atomicity: u8,
) callconv(.c) void {
    const g = requireGlobal();
    enter(g);
    defer leave(g);
    const rc_op: layout_mod.RcOp = @enumFromInt(op);
    const rc_atomicity: builtins.utils.RcAtomicity = @enumFromInt(atomicity);
    const val: Value = if (value) |p| .{ .ptr = p } else Value.zst;
    const layout_idx = layoutIdx(value_layout);
    const layout_value = g.runtime.layout_store.getLayout(layout_idx);
    switch (rc_op) {
        .incref => if (layout_value.tag == .box or layout_value.tag == .box_of_zst) {
            g.runtime.performConcreteRc(hooks(g), .incref, layout_idx, val, count, rc_atomicity);
        } else g.runtime.performBoxyLayoutDrop(
            hooks(g),
            val,
            layout_idx,
            desc,
            .incref,
            count,
            rc_atomicity,
        ) catch abiCrash(g, "incref"),
        .decref, .free => g.runtime.performBoxyLayoutDrop(
            hooks(g),
            val,
            layout_idx,
            desc,
            rc_op,
            count,
            rc_atomicity,
        ) catch abiCrash(g, "drop"),
    }
}

/// Concatenate two lists using the element descriptor for copy and drop logic.
pub fn roc_boxy_list_concat(
    out: *RocList,
    a_bytes: ?[*]u8,
    a_len: usize,
    a_cap: usize,
    b_bytes: ?[*]u8,
    b_len: usize,
    b_cap: usize,
    alignment: u32,
    element_width: usize,
    elem_layout: u32,
    list_desc: *const BoxyTypeDesc,
    update_modes: u64,
    _: *RocOps,
) callconv(.c) void {
    const g = requireGlobal();
    enter(g);
    defer leave(g);
    var ctx = boxyListElementContext(g, list_desc, elem_layout);
    const update_mode_a: builtins.utils.UpdateMode = if (update_modes & 1 != 0) .InPlace else .Immutable;
    const update_mode_b: builtins.utils.UpdateMode = if (update_modes & 2 != 0) .InPlace else .Immutable;
    out.* = builtins.list.listConcat(
        .{ .bytes = a_bytes, .length = a_len, .capacity_or_alloc_ptr = a_cap },
        .{ .bytes = b_bytes, .length = b_len, .capacity_or_alloc_ptr = b_cap },
        alignment,
        element_width,
        true,
        @ptrCast(&ctx),
        &boxyListElementIncref,
        @ptrCast(&ctx),
        &boxyListElementDecref,
        update_mode_a,
        update_mode_b,
        g.runtime.roc_ops,
    );
}

/// Prepend one descriptor-governed element to a list.
pub fn roc_boxy_list_prepend(
    out: *RocList,
    list_bytes: ?[*]u8,
    list_len: usize,
    list_cap: usize,
    alignment: u32,
    element: ?[*]u8,
    element_width: usize,
    elem_layout: u32,
    list_desc: *const BoxyTypeDesc,
    update_mode: builtins.utils.UpdateMode,
    _: *RocOps,
) callconv(.c) void {
    const g = requireGlobal();
    enter(g);
    defer leave(g);
    var ctx = boxyListElementContext(g, list_desc, elem_layout);
    out.* = builtins.list.listPrepend(
        .{ .bytes = list_bytes, .length = list_len, .capacity_or_alloc_ptr = list_cap },
        alignment,
        element,
        element_width,
        true,
        @ptrCast(&ctx),
        &boxyListElementIncref,
        @ptrCast(&ctx),
        &boxyListElementDecref,
        update_mode,
        &builtins.list.copy_fallback,
        g.runtime.roc_ops,
    );
}

/// Produce a descriptor-governed sublist for the requested start and length.
pub fn roc_boxy_list_sublist(
    out: *RocList,
    list_bytes: ?[*]u8,
    list_len: usize,
    list_cap: usize,
    alignment: u32,
    element_width: usize,
    start: u64,
    len: u64,
    elem_layout: u32,
    list_desc: *const BoxyTypeDesc,
    update_mode: builtins.utils.UpdateMode,
    _: *RocOps,
) callconv(.c) void {
    const g = requireGlobal();
    enter(g);
    defer leave(g);
    var ctx = boxyListElementContext(g, list_desc, elem_layout);
    out.* = builtins.list.listSublist(
        .{ .bytes = list_bytes, .length = list_len, .capacity_or_alloc_ptr = list_cap },
        alignment,
        element_width,
        true,
        start,
        len,
        @ptrCast(&ctx),
        &boxyListElementDecref,
        update_mode,
        g.runtime.roc_ops,
    );
}

/// Remove one element at an index using descriptor-guided ownership operations.
pub fn roc_boxy_list_drop_at(
    out: *RocList,
    list_bytes: ?[*]u8,
    list_len: usize,
    list_cap: usize,
    alignment: u32,
    element_width: usize,
    index: u64,
    elem_layout: u32,
    list_desc: *const BoxyTypeDesc,
    update_mode: builtins.utils.UpdateMode,
    _: *RocOps,
) callconv(.c) void {
    const g = requireGlobal();
    enter(g);
    defer leave(g);
    var ctx = boxyListElementContext(g, list_desc, elem_layout);
    out.* = builtins.list.listDropAt(
        .{ .bytes = list_bytes, .length = list_len, .capacity_or_alloc_ptr = list_cap },
        alignment,
        element_width,
        true,
        index,
        @ptrCast(&ctx),
        &boxyListElementIncref,
        @ptrCast(&ctx),
        &boxyListElementDecref,
        update_mode,
        g.runtime.roc_ops,
    );
}

/// Replace one list element and return both the new list and displaced value.
pub fn roc_boxy_list_replace(
    out: *RocList,
    list_bytes: ?[*]u8,
    list_len: usize,
    list_cap: usize,
    alignment: u32,
    index: u64,
    element: ?[*]u8,
    element_width: usize,
    out_element: ?[*]u8,
    elem_layout: u32,
    list_desc: *const BoxyTypeDesc,
    update_mode: builtins.utils.UpdateMode,
    _: *RocOps,
) callconv(.c) void {
    const input = RocList{ .bytes = list_bytes, .length = list_len, .capacity_or_alloc_ptr = list_cap };
    if (update_mode == .InPlace) {
        out.* = builtins.list.listReplaceInPlace(input, index, element, element_width, out_element, &builtins.list.copy_fallback);
        return;
    }

    const g = requireGlobal();
    enter(g);
    defer leave(g);
    var ctx = boxyListElementContext(g, list_desc, elem_layout);
    out.* = builtins.list.listReplace(
        input,
        alignment,
        index,
        element,
        element_width,
        true,
        @ptrCast(&ctx),
        &boxyListElementIncref,
        @ptrCast(&ctx),
        &boxyListElementDecref,
        out_element,
        &builtins.list.copy_fallback,
        g.runtime.roc_ops,
    );
}

/// Set one list element using the descriptor to retain or release nested values.
pub fn roc_boxy_list_set(
    out: *RocList,
    list_bytes: ?[*]u8,
    list_len: usize,
    list_cap: usize,
    alignment: u32,
    index: u64,
    element: ?[*]u8,
    element_width: usize,
    elem_layout: u32,
    list_desc: *const BoxyTypeDesc,
    update_mode: builtins.utils.UpdateMode,
    _: *RocOps,
) callconv(.c) void {
    const g = requireGlobal();
    enter(g);
    defer leave(g);
    var ctx = boxyListElementContext(g, list_desc, elem_layout);
    out.* = builtins.list.listSet(
        .{ .bytes = list_bytes, .length = list_len, .capacity_or_alloc_ptr = list_cap },
        alignment,
        index,
        element,
        element_width,
        true,
        @ptrCast(&ctx),
        &boxyListElementIncref,
        @ptrCast(&ctx),
        &boxyListElementDecref,
        update_mode,
        &builtins.list.copy_fallback,
        g.runtime.roc_ops,
    );
}

/// Swap two list elements while preserving descriptor-governed ownership.
pub fn roc_boxy_list_swap(
    out: *RocList,
    list_bytes: ?[*]u8,
    list_len: usize,
    list_cap: usize,
    alignment: u32,
    element_width: usize,
    index_1: u64,
    index_2: u64,
    elem_layout: u32,
    list_desc: *const BoxyTypeDesc,
    update_mode: builtins.utils.UpdateMode,
    _: *RocOps,
) callconv(.c) void {
    const g = requireGlobal();
    enter(g);
    defer leave(g);
    var ctx = boxyListElementContext(g, list_desc, elem_layout);
    out.* = builtins.list.listSwap(
        .{ .bytes = list_bytes, .length = list_len, .capacity_or_alloc_ptr = list_cap },
        alignment,
        element_width,
        index_1,
        index_2,
        true,
        @ptrCast(&ctx),
        &boxyListElementIncref,
        @ptrCast(&ctx),
        &boxyListElementDecref,
        update_mode,
        &builtins.list.copy_fallback,
        g.runtime.roc_ops,
    );
}

/// Reverse a descriptor-governed list into the returned Roc list value.
pub fn roc_boxy_list_reverse(
    out: *RocList,
    list_bytes: ?[*]u8,
    list_len: usize,
    list_cap: usize,
    alignment: u32,
    element_width: usize,
    elem_layout: u32,
    list_desc: *const BoxyTypeDesc,
    update_mode: builtins.utils.UpdateMode,
    _: *RocOps,
) callconv(.c) void {
    const g = requireGlobal();
    enter(g);
    defer leave(g);
    var ctx = boxyListElementContext(g, list_desc, elem_layout);
    out.* = builtins.list.listReverse(
        .{ .bytes = list_bytes, .length = list_len, .capacity_or_alloc_ptr = list_cap },
        alignment,
        element_width,
        true,
        @ptrCast(&ctx),
        &boxyListElementIncref,
        @ptrCast(&ctx),
        &boxyListElementDecref,
        update_mode,
        &builtins.list.copy_fallback,
        g.runtime.roc_ops,
    );
}

/// Reserve list capacity while preserving descriptor-governed elements.
pub fn roc_boxy_list_reserve(
    out: *RocList,
    list_bytes: ?[*]u8,
    list_len: usize,
    list_cap: usize,
    alignment: u32,
    spare: u64,
    element_width: usize,
    elem_layout: u32,
    list_desc: *const BoxyTypeDesc,
    update_mode: builtins.utils.UpdateMode,
    _: *RocOps,
) callconv(.c) void {
    const g = requireGlobal();
    enter(g);
    defer leave(g);
    var ctx = boxyListElementContext(g, list_desc, elem_layout);
    out.* = builtins.list.listReserve(
        .{ .bytes = list_bytes, .length = list_len, .capacity_or_alloc_ptr = list_cap },
        alignment,
        spare,
        element_width,
        true,
        @ptrCast(&ctx),
        &boxyListElementIncref,
        @ptrCast(&ctx),
        &boxyListElementDecref,
        update_mode,
        g.runtime.roc_ops,
    );
}

/// Shrink a list allocation to its logical length without changing elements.
pub fn roc_boxy_list_release_excess_capacity(
    out: *RocList,
    list_bytes: ?[*]u8,
    list_len: usize,
    list_cap: usize,
    alignment: u32,
    element_width: usize,
    elem_layout: u32,
    list_desc: *const BoxyTypeDesc,
    update_mode: builtins.utils.UpdateMode,
    _: *RocOps,
) callconv(.c) void {
    const g = requireGlobal();
    enter(g);
    defer leave(g);
    var ctx = boxyListElementContext(g, list_desc, elem_layout);
    out.* = builtins.list.listReleaseExcessCapacity(
        .{ .bytes = list_bytes, .length = list_len, .capacity_or_alloc_ptr = list_cap },
        alignment,
        element_width,
        true,
        @ptrCast(&ctx),
        &boxyListElementIncref,
        @ptrCast(&ctx),
        &boxyListElementDecref,
        update_mode,
        g.runtime.roc_ops,
    );
}

/// Descriptor-guided tag test, following row extensions.
pub fn roc_boxy_tag_match(
    source: ?[*]const u8,
    source_layout: u32,
    source_desc: *const BoxyTypeDesc,
    tag_name: u32,
) callconv(.c) bool {
    const g = requireGlobal();
    enter(g);
    defer leave(g);
    return g.runtime.boxyTagMatches(
        hooks(g),
        valueAt(source),
        layoutIdx(source_layout),
        source_desc,
        @enumFromInt(tag_name),
    ) catch abiCrash(g, "tag match");
}

/// Materialize a static descriptor template into the runtime descriptor
/// tables. `capture_ids`/`capture_descs` bind the template's local descriptor
/// references. Descriptor projection uses the separate explicit projection
/// entry points below.
pub fn roc_boxy_desc_copy(
    desc_id: u32,
    capture_ids: ?[*]const u32,
    capture_descs: ?[*]const ?*const BoxyTypeDesc,
    capture_count: usize,
) callconv(.c) *const BoxyTypeDesc {
    const g = requireGlobal();
    enter(g);
    defer leave(g);
    const ids = if (capture_ids) |supplied| supplied[0..capture_count] else &.{};
    const descs = if (capture_descs) |supplied| supplied[0..capture_count] else &.{};
    const cache_key = DescCopyCacheKey{
        .desc_id = desc_id,
        .capture_ids = ids,
        .capture_descs = descs,
    };
    if (g.desc_copy_cache.get(cache_key)) |cached| return cached;

    g.capture_ids = ids;
    g.capture_descs = descs;
    defer {
        g.capture_ids = &.{};
        g.capture_descs = &.{};
    }
    const desc_ref = LIR.BoxyDescRef{ .static = @enumFromInt(desc_id) };
    const captures = LIR.LocalSpan{ .start = 0, .len = @intCast(capture_count) };
    const result = g.runtime.materializeBoxyDescRefValueWithCaptures(hooks(g), desc_ref, captures) catch abiCrash(g, "descriptor materialization");
    const cache_allocator = g.desc_arena.allocator();
    const owned_ids = cache_allocator.dupe(u32, ids) catch abiCrash(g, "descriptor materialization cache ids");
    const owned_descs = cache_allocator.dupe(?*const BoxyTypeDesc, descs) catch abiCrash(g, "descriptor materialization cache descriptors");
    g.desc_copy_cache.put(g.gpa, .{
        .desc_id = desc_id,
        .capture_ids = owned_ids,
        .capture_descs = owned_descs,
    }, result) catch abiCrash(g, "descriptor materialization cache");
    return result;
}

/// Resolve a static descriptor id to its descriptor pointer in the global
/// descriptor table.
pub fn roc_boxy_static_desc(desc_id: u32) callconv(.c) *const BoxyTypeDesc {
    const g = requireGlobal();
    return g.runtime.requireBoxyTypeDesc(@enumFromInt(desc_id));
}

/// Materialize a worker call's raw return value into the caller's declared
/// target layout, guided by the callee's returned descriptor (`actual_desc`,
/// null for erased returns) and the call site's result descriptor. Writes the
/// materialized bytes through `out` and the result's descriptor through
/// `out_desc`. Mirrors the result reconciliation the interpreter performs for
/// `assign_call`/`assign_call_erased`.
pub fn roc_boxy_materialize_call_result(
    out: ?[*]u8,
    out_desc: *?*const BoxyTypeDesc,
    value: ?[*]const u8,
    actual_layout: u32,
    actual_desc: ?*const BoxyTypeDesc,
    result_desc: ?*const BoxyTypeDesc,
    expected_layout: u32,
) callconv(.c) void {
    const g = requireGlobal();
    enter(g);
    defer leave(g);
    const materialized = g.runtime.materializeCallResult(
        hooks(g),
        valueAt(value),
        layoutIdx(actual_layout),
        actual_desc,
        result_desc,
        layoutIdx(expected_layout),
    ) catch abiCrash(g, "call result materialization");
    writeResult(g, out, materialized.value, layoutIdx(expected_layout));
    out_desc.* = materialized.desc;
}

/// Resolve a static dictionary id to its dictionary pointer in the global
/// dictionary table.
pub fn roc_boxy_static_dict(dict_id: u32) callconv(.c) *const BoxyDict {
    const g = requireGlobal();
    return g.runtime.requireBoxyDict(@enumFromInt(dict_id));
}

/// Resolve one explicit argument descriptor from a runtime dictionary method
/// adapter. Backends use this to materialize `BoxyDescRef.dict_method_arg`.
pub fn roc_boxy_dict_method_arg_desc(
    dict: *const BoxyDict,
    method_slot: u32,
    method: u32,
    arg_index: u32,
) callconv(.c) *const BoxyTypeDesc {
    const g = requireGlobal();
    enter(g);
    defer leave(g);
    return g.runtime.resolveDictMethodArgDesc(
        hooks(g),
        dict,
        method_slot,
        method,
        arg_index,
    ) catch abiCrash(g, "dictionary method argument descriptor resolution");
}

/// Resolve one hidden worker descriptor from a runtime dictionary method slot.
pub fn roc_boxy_dict_method_hidden_desc(
    dict: *const BoxyDict,
    method_slot: u32,
    method: u32,
    hidden_index: u32,
    shape: u32,
) callconv(.c) *const BoxyTypeDesc {
    const g = requireGlobal();
    enter(g);
    defer leave(g);
    return g.runtime.resolveDictMethodHiddenDesc(
        hooks(g),
        dict,
        method_slot,
        method,
        hidden_index,
        @enumFromInt(shape),
    ) catch abiCrash(g, "dictionary method hidden descriptor resolution");
}

/// Navigate to the nested descriptor at `nested_index` of an already-resolved
/// descriptor pointer. Used when a boxy descriptor reference names a nested
/// descriptor of a descriptor already materialized into a local.
pub fn roc_boxy_nested_desc(desc: *const BoxyTypeDesc, nested_index: u32) callconv(.c) *const BoxyTypeDesc {
    const g = requireGlobal();
    enter(g);
    defer leave(g);
    const nested = g.runtime.requireBoxyDescRefs(desc.nested_descs);
    if (nested_index >= nested.len) abiCrash(g, "nested descriptor navigation");
    const result = hooks(g).resolveDescRef(nested[nested_index]) catch abiCrash(g, "nested descriptor resolution");
    return result;
}

/// Resolve the allocation payload descriptor for a value in a committed Box
/// layout. The descriptor may use either the box-self or payload-direct
/// convention; the layout makes this projection explicit.
pub fn roc_boxy_box_payload_desc(desc: *const BoxyTypeDesc, box_layout: u32) callconv(.c) *const BoxyTypeDesc {
    const g = requireGlobal();
    enter(g);
    defer leave(g);
    return (g.runtime.boxyBoxAllocationPayloadDesc(hooks(g), @enumFromInt(box_layout), desc) catch
        abiCrash(g, "Box payload descriptor resolution")) orelse
        abiCrash(g, "Box payload descriptor missing");
}

/// Resolve one payload descriptor by exact tag identity and payload index.
pub fn roc_boxy_tag_payload_desc(
    desc: *const BoxyTypeDesc,
    tag_name: u32,
    payload_index: u32,
) callconv(.c) *const BoxyTypeDesc {
    const g = requireGlobal();
    enter(g);
    defer leave(g);
    const name: base.StringLiteral.Idx = @enumFromInt(tag_name);
    const variant = g.runtime.findLocalBoxyTagVariant(desc, name) orelse abiCrash(g, "tag-payload descriptor variant navigation");
    const payload_desc = g.runtime.findBoxyPayloadDesc(variant, payload_index) orelse abiCrash(g, "tag-payload descriptor navigation");
    return hooks(g).resolveDescRef(payload_desc) catch abiCrash(g, "tag-payload descriptor resolution");
}

/// Resolve the descriptor of a tag union's row extension.
pub fn roc_boxy_tag_ext_desc(desc: *const BoxyTypeDesc) callconv(.c) *const BoxyTypeDesc {
    const g = requireGlobal();
    return g.runtime.resolveBoxyTagExtDesc(hooks(g), desc) catch abiCrash(g, "tag-extension descriptor navigation");
}

/// Resolve the residual descriptor after subtracting a matched tag domain.
pub fn roc_boxy_tag_residual_desc(
    source_desc: *const BoxyTypeDesc,
    target_desc: *const BoxyTypeDesc,
) callconv(.c) *const BoxyTypeDesc {
    const g = requireGlobal();
    return g.runtime.materializeTagResidualBoxyDescValues(source_desc, target_desc) catch abiCrash(g, "residual tag descriptor materialization");
}

/// Encode a numeric literal per the descriptor's payload layout and box it
/// into dynamic storage.
pub fn roc_boxy_dynamic_num_literal(
    out: ?[*]u8,
    value: *align(1) const i128,
    desc: *const BoxyTypeDesc,
    target_layout: u32,
) callconv(.c) void {
    const g = requireGlobal();
    enter(g);
    defer leave(g);
    const literal = g.runtime.boxyDynamicNumLiteral(
        hooks(g),
        value.*,
        desc,
        layoutIdx(target_layout),
    ) catch abiCrash(g, "dynamic numeric literal");
    writeResult(g, out, literal, layoutIdx(target_layout));
}

/// Encode a numeric literal per `desc`'s payload layout. When an erased
/// binding has no concrete scalar payload, `default_layout` explicitly
/// selects the encoding. The selected descriptor is written through
/// `out_desc` alongside the dynamic value.
pub fn roc_boxy_dynamic_num_literal_ref(
    out: ?[*]u8,
    out_desc: *?*const BoxyTypeDesc,
    value: *align(1) const i128,
    desc: *const BoxyTypeDesc,
    default_layout: u32,
    target_layout: u32,
) callconv(.c) void {
    const g = requireGlobal();
    enter(g);
    defer leave(g);
    const effective = g.runtime.effectiveBoxyScalarLiteralDesc(
        desc,
        layoutIdx(default_layout),
    ) catch abiCrash(g, "dynamic numeric literal descriptor");
    const literal = g.runtime.boxyDynamicNumLiteral(
        hooks(g),
        value.*,
        effective,
        layoutIdx(target_layout),
    ) catch abiCrash(g, "dynamic numeric literal");
    writeResult(g, out, literal, layoutIdx(target_layout));
    out_desc.* = effective;
}

/// Encode a fractional literal using a descriptor-selected or explicit
/// default layout, and publish the selected descriptor through `out_desc`.
pub fn roc_boxy_dynamic_frac_literal_ref(
    out: ?[*]u8,
    out_desc: *?*const BoxyTypeDesc,
    dec_bits: *align(1) const i128,
    desc: *const BoxyTypeDesc,
    default_layout: u32,
    target_layout: u32,
) callconv(.c) void {
    const g = requireGlobal();
    enter(g);
    defer leave(g);
    const effective = g.runtime.effectiveBoxyScalarLiteralDesc(
        desc,
        layoutIdx(default_layout),
    ) catch abiCrash(g, "dynamic fractional literal descriptor");
    const literal = g.runtime.boxyDynamicFracLiteral(
        hooks(g),
        dec_bits.*,
        effective,
        layoutIdx(target_layout),
    ) catch abiCrash(g, "dynamic fractional literal");
    writeResult(g, out, literal, layoutIdx(target_layout));
    out_desc.* = effective;
}

/// Dispatch one dictionary method call: adapt the explicit arguments per the
/// slot's adapter, append hidden descriptors and nested dictionaries, and
/// either run descriptor-guided structural equality (writing a `u8` result)
/// or invoke the registered callee for the slot's worker proc. The result is
/// materialized into `out_layout` guided by the callee's returned descriptor
/// and `result_desc`; the result's descriptor is written through `out_desc`.
pub fn roc_boxy_call_dict(
    out: ?[*]u8,
    out_desc: *?*const BoxyTypeDesc,
    test_context: ?*anyopaque,
    dict: *const BoxyDict,
    method_slot: u32,
    method: u32,
    args: ?[*]const RocBoxyCallArg,
    args_len: usize,
    hidden_args: ?[*]const usize,
    hidden_len: usize,
    result_desc: ?*const BoxyTypeDesc,
    out_layout: u32,
) callconv(.c) void {
    const g = requireGlobal();
    enter(g);
    defer leave(g);
    const scratch = g.value_scratch.allocator();

    const raw_args: []const RocBoxyCallArg = if (args) |a| a[0..args_len] else &.{};
    const call_args = scratch.alloc(boxy_runtime.DictCallArg, raw_args.len) catch abiCrash(g, "dictionary call argument collection");
    for (raw_args, 0..) |raw, i| {
        call_args[i] = .{
            .value = valueAt(raw.value),
            .layout = layoutIdx(raw.layout),
            .source_desc = raw.desc,
        };
    }
    const raw_hidden: []const usize = if (hidden_args) |h| h[0..hidden_len] else &.{};
    const hidden_values = scratch.alloc(Value, raw_hidden.len) catch abiCrash(g, "dictionary call hidden argument collection");
    for (raw_hidden, 0..) |raw, i| {
        const slot = scratch.alloc(usize, 1) catch abiCrash(g, "dictionary call hidden argument collection");
        slot[0] = raw;
        hidden_values[i] = .{ .ptr = @ptrCast(slot.ptr) };
    }

    const prepared = g.runtime.prepareDictCall(
        hooks(g),
        scratch,
        dict,
        method_slot,
        method,
        call_args,
        hidden_values,
        .move,
    ) catch abiCrash(g, "dictionary call preparation");

    switch (prepared) {
        .structural_eq => |operand_desc| {
            const equal = g.runtime.boxyValuesEqual(
                hooks(g),
                call_args[0].value,
                call_args[1].value,
                call_args[0].layout,
                operand_desc,
            ) catch abiCrash(g, "dictionary structural equality");
            const out_ptr = out orelse abiCrash(g, "result write without an out pointer");
            out_ptr[0] = if (equal) 1 else 0;
            out_desc.* = null;
            for (call_args) |arg| {
                g.runtime.performBoxyLayoutDrop(
                    hooks(g),
                    arg.value,
                    arg.layout,
                    arg.source_desc,
                    .decref,
                    1,
                    .atomic,
                ) catch abiCrash(g, "structural dictionary argument release");
            }
        },
        .call => |call| {
            const registered = g.procs.get(@intFromEnum(call.proc)) orelse
                abiCrash(g, "dictionary dispatch to an unregistered proc");
            const arg_ptrs = scratch.alloc(?*const anyopaque, call.arg_values.len) catch abiCrash(g, "dictionary call argument collection");
            for (call.arg_values, call.arg_layouts, 0..) |arg_value, arg_layout, i| {
                arg_ptrs[i] = if (g.runtime.helper.sizeOf(arg_layout) == 0) null else @ptrCast(arg_value.ptr);
            }
            const ret_size = g.runtime.helper.sizeOf(registered.ret_layout);
            const ret_value = hooks(g).allocValue(registered.ret_layout) catch abiCrash(g, "dictionary call result buffer");
            var ret_desc: ?*const anyopaque = null;
            registered.callee(
                g.runtime.roc_ops,
                test_context,
                arg_ptrs.ptr,
                if (ret_size == 0) null else @ptrCast(ret_value.ptr),
                &ret_desc,
            );
            const resolved_ret_desc: ?*const BoxyTypeDesc = @ptrCast(@alignCast(ret_desc));
            if (registered.ret_borrowed) {
                g.runtime.performBoxyLayoutDrop(
                    hooks(g),
                    ret_value,
                    registered.ret_layout,
                    resolved_ret_desc,
                    .incref,
                    1,
                    .atomic,
                ) catch abiCrash(g, "borrowed dictionary result retain");
            }
            for (call.arg_values, call.arg_layouts, call.arg_descs, 0..) |arg_value, arg_layout, arg_desc, arg_index| {
                if (arg_index >= 64 or ((registered.borrowed_params >> @as(u6, @intCast(arg_index))) & 1) == 0) continue;
                g.runtime.performBoxyLayoutDrop(
                    hooks(g),
                    arg_value,
                    arg_layout,
                    arg_desc,
                    .decref,
                    1,
                    .atomic,
                ) catch abiCrash(g, "borrowed dictionary argument release");
            }
            const materialized = g.runtime.materializeCallResult(
                hooks(g),
                ret_value,
                registered.ret_layout,
                resolved_ret_desc,
                result_desc,
                layoutIdx(out_layout),
            ) catch abiCrash(g, "dictionary call result materialization");
            writeResult(g, out, materialized.value, layoutIdx(out_layout));
            out_desc.* = materialized.desc;
        },
    }
}
