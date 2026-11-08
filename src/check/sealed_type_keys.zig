//! Canonical type keys of a module's sealed source types, computed on first
//! demand.
//!
//! Each solver variable owns a write-once key slot. The first consumer that
//! needs a variable's key claims its slot and computes the key; consumers
//! that find it claimed wait; consumers that find it done read it with an
//! acquire load. Native hosts publish one fresh OS-zeroed mapping, without
//! touching unrequested slots. Compiler-WASM uses explicit heap pages because
//! linear memory has no independently releasable demand-zero mapping API.
//! Both keep slot addresses stable. Computations share one key writer, so a
//! component type's key is digested once per module however many roots contain
//! it. The writer is held only while a claimant computes, never while anyone waits
//! on a slot. Keys are never serialized: a module loaded from a cache entry
//! starts with every slot empty.
//!
//! Resolving a type from its key alone builds the module's key index on
//! first demand, through the module's index slot.

const std = @import("std");
const builtin = @import("builtin");
const base = @import("base");
const type_key_engine = @import("type_key_engine.zig");
const can = @import("can");
const collections = @import("collections");
const types = @import("types");
const canonical = @import("canonical_names.zig");
const canonical_type_keys = @import("canonical_type_keys.zig");
const sealed_type_view = @import("sealed_type_view.zig");

const Allocator = std.mem.Allocator;
const ModuleEnv = can.ModuleEnv;
const Var = types.Var;
const TypeKeyInfo = canonical_type_keys.TypeKeyInfo;

const KeySlot = collections.OnceSlot(TypeKeyInfo, .sealed_type_key);
// This is a host storage choice, fixed at compilation. An OS allocation
// failure never selects another backend.
const use_zeroed_mapping = if (builtin.cpu.arch.isWasm()) false else switch (builtin.os.tag) {
    .linux, .macos, .windows, .freebsd, .netbsd, .openbsd, .dragonfly => true,
    .freestanding,
    .other,
    .contiki,
    .fuchsia,
    .hermit,
    .managarm,
    .haiku,
    .hurd,
    .illumos,
    .plan9,
    .rtems,
    .serenity,
    .driverkit,
    .ios,
    .maccatalyst,
    .tvos,
    .visionos,
    .watchos,
    .uefi,
    .@"3ds",
    .wiiu,
    .@"switch",
    .gba,
    .psx,
    .ps3,
    .ps4,
    .ps5,
    .psp,
    .vita,
    .emscripten,
    .wasi,
    .amdhsa,
    .amdpal,
    .cuda,
    .mesa3d,
    .nvcl,
    .opencl,
    .opengl,
    .vulkan,
    .tios,
    .ashetos,
    => @compileError("sealed source key storage requires an explicit host backend"),
};
const KeyStorage = if (use_zeroed_mapping) ZeroedKeyMapping else HeapKeyPages;
const slots_per_page = 64;
const KeyPage = [slots_per_page]KeySlot;
const PageSlot = collections.OnceSlot(*KeyPage, .sealed_type_key_page);

comptime {
    // Zero bytes initialize only the atomic state. The result is never read
    // until OnceSlot.publish has assigned it in full and released the state.
    if (@backingInt(KeySlot.empty.state.raw) != 0)
        @compileError("OS-zeroed key slots require the empty state to encode as zero");
    if (@typeInfo(KeySlot).@"struct".field_names.len != 2 or !@hasField(KeySlot, "state") or !@hasField(KeySlot, "result"))
        @compileError("review zero initialization when OnceSlot gains fields");
    const AtomicState = @FieldType(KeySlot, "state");
    if (@typeInfo(AtomicState).@"struct".field_names.len != 1 or !@hasField(AtomicState, "raw") or @sizeOf(AtomicState) != @sizeOf(u32))
        @compileError("review the OS-zeroed atomic state representation");
}

const StorageMemory = struct {
    logical_bytes: usize = 0,
    reserved_bytes: usize = 0,
    allocated_bytes: usize = 0,
    directory_bytes: usize = 0,
};

/// A fresh anonymous/private mapping (Windows: reserve and commit). Only
/// PageAllocator.map promises the zero bytes required here; Allocator.alloc
/// may recycle bytes and poison them even when its underlying allocator maps.
const ZeroedKeyMapping = struct {
    slots: []KeySlot,
    reserved_bytes: usize,

    fn init(_: Allocator, count: usize) Allocator.Error!ZeroedKeyMapping {
        const bytes = std.math.mul(usize, count, @sizeOf(KeySlot)) catch return error.OutOfMemory;
        if (bytes == 0) return .{ .slots = &.{}, .reserved_bytes = 0 };
        const page_size = std.heap.pageSize();
        comptime std.debug.assert(@alignOf(KeySlot) <= std.heap.page_size_min);
        const padded = std.math.add(usize, bytes, page_size - 1) catch return error.OutOfMemory;
        const reserved_bytes = padded & ~(page_size - 1);
        const raw = std.heap.PageAllocator.map(reserved_bytes, .fromByteUnits(page_size)) orelse return error.OutOfMemory;
        const slots: [*]KeySlot = @ptrCast(@alignCast(raw));
        // No memset, Allocator wrapper, huge-page advice, or result read.
        return .{ .slots = slots[0..count], .reserved_bytes = reserved_bytes };
    }

    fn deinit(self: ZeroedKeyMapping, _: Allocator) void {
        if (self.reserved_bytes == 0) return;
        const raw: [*]align(std.heap.page_size_min) u8 = @ptrCast(@alignCast(self.slots.ptr));
        // Direct unmap neither poisons memory nor scans slot states.
        std.heap.PageAllocator.unmap(raw[0..self.reserved_bytes]);
    }

    fn slot(self: ZeroedKeyMapping, _: Allocator, _: std.Io, raw: usize) Allocator.Error!*KeySlot {
        return &self.slots[raw];
    }

    fn memory(self: ZeroedKeyMapping) StorageMemory {
        return .{ .logical_bytes = self.slots.len * @sizeOf(KeySlot), .reserved_bytes = self.reserved_bytes };
    }
};

/// Compiler-WASM owns pages through its heap allocator. Recycled linear
/// memory is not guaranteed zero, so each allocated page is initialized.
const HeapKeyPages = struct {
    pages: []PageSlot,
    count: usize,

    fn init(allocator: Allocator, count: usize) Allocator.Error!HeapKeyPages {
        const page_count = count / slots_per_page + @intFromBool(count % slots_per_page != 0);
        const pages = try allocator.alloc(PageSlot, page_count);
        @memset(pages, .empty);
        return .{ .pages = pages, .count = count };
    }

    fn deinit(self: HeapKeyPages, allocator: Allocator) void {
        for (self.pages) |*entry| if (entry.peek()) |page| allocator.destroy(page);
        allocator.free(self.pages);
    }

    fn allocatePage(allocator: Allocator) Allocator.Error!*KeyPage {
        const page = try allocator.create(KeyPage);
        @memset(page, .empty);
        return page;
    }

    fn slot(self: HeapKeyPages, allocator: Allocator, io: std.Io, raw: usize) Allocator.Error!*KeySlot {
        std.debug.assert(raw < self.count);
        const page = try self.pages[raw / slots_per_page].getOrCompute(io, Allocator.Error, allocator, allocatePage);
        return &page[raw % slots_per_page];
    }

    fn memory(self: HeapKeyPages) StorageMemory {
        var allocated_bytes: usize = 0;
        // Scan only the small WASM directory, never key pages or native maps.
        for (self.pages) |*entry| if (entry.peek() != null) {
            allocated_bytes += @sizeOf(KeyPage);
        };
        return .{ .logical_bytes = self.count * @sizeOf(KeySlot), .allocated_bytes = allocated_bytes, .directory_bytes = self.pages.len * @sizeOf(PageSlot) };
    }
};
const KeyIndex = std.AutoHashMapUnmanaged([32]u8, Var);

/// The key slots of one module's sealed source types.
pub const SealedTypeKeys = struct {
    allocator: Allocator,
    io: std.Io,
    env: *const ModuleEnv,
    table: collections.OnceSlot(KeyStorage, .sealed_type_key_table) = .empty,
    /// Diagnostics/test builds update this under writer_mutex after fromVar
    /// succeeds. A completed-key hit does no extra work.
    successful_keys: usize = 0,
    /// Deterministic storage-publication failure in tests, absent in production.
    test_fail_storage: base.TestSwitch(false) = .{},
    index: collections.OnceSlot(*KeyIndex, .sealed_type_key_index) = .empty,
    writer_mutex: std.Io.Mutex = .init,
    writer: ?canonical_type_keys.TypeWriter = null,
    /// Per variable: 0 while unknown, else 1 + whether a walk found the
    /// variable reaches an identity variable.
    identity: collections.zeroed_pages.SparseSlice(std.atomic.Value(u8)),

    /// Key slots for `env`'s sealed store. `allocator` must be safe to use
    /// from every thread that reads the module's types.
    pub fn create(allocator: Allocator, io: std.Io, env: *const ModuleEnv) Allocator.Error!*SealedTypeKeys {
        const keys = try allocator.create(SealedTypeKeys);
        errdefer allocator.destroy(keys);
        const identity = try collections.zeroed_pages.SparseSlice(std.atomic.Value(u8)).init(allocator, @intCast(env.types.len()));
        keys.* = .{ .allocator = allocator, .io = io, .env = env, .identity = identity };
        return keys;
    }

    pub fn destroy(self: *SealedTypeKeys) void {
        const allocator = self.allocator;
        if (self.table.peek()) |backing| backing.deinit(allocator);
        if (self.index.peek()) |index| {
            index.deinit(allocator);
            allocator.destroy(index);
        }
        if (self.writer) |*writer| writer.deinit();
        self.identity.deinit(allocator);
        allocator.destroy(self);
    }

    /// A diagnostic snapshot of this unique source-key owner. No allocations,
    /// slot claims, key computations or memo changes. Shared views print the
    /// same owner address: consumers deduplicate by (phase, owner).
    /// heap_requested_bytes excludes mapped key storage. Reserved virtual
    /// bytes are reported separately; neither counter measures resident RAM.
    pub fn traceMemory(self: *SealedTypeKeys, phase: []const u8) void {
        if (!base.dev_diagnostics.isSet(.trace_boxy_work)) return;
        self.writer_mutex.lockUncancelable(self.io);
        defer self.writer_mutex.unlock(self.io);
        const storage_memory = if (self.table.peek()) |backing| backing.memory() else StorageMemory{};
        const memory: canonical_type_keys.TypeWriter.KeyMemory = if (self.writer) |*writer| writer.retainedKeyMemory() else .{};
        const slots = if (self.table.peek() != null) self.identity.items.len else 0;
        const slot_bytes = storage_memory.allocated_bytes;
        const directory_bytes = storage_memory.directory_bytes;
        const identity_bytes = self.identity.items.len * @sizeOf(std.atomic.Value(u8));
        const index_bytes = if (self.index.peek()) |index| @sizeOf(KeyIndex) + type_key_engine.hashRetainedBytes([32]u8, Var, index.*) else @as(usize, 0);
        const persistent = memory.engine.persistent();
        const scratch = memory.engine.scratch + memory.adapter_scratch;
        const total = @sizeOf(SealedTypeKeys) + directory_bytes + slot_bytes + identity_bytes + index_bytes + persistent + scratch + memory.owned_ranks;
        std.debug.print("boxy-work: type-key-memory phase={s} owner={x} vars={d} slots={d} successful_keys={d} owner_bytes={d} slot_heap_requested_bytes={d} identity_bytes={d} index_bytes={d} writer_present={d} descriptions_bytes={d} class_index_bytes={d} summaries_bytes={d} identity_maps_bytes={d} persistent_bytes={d} engine_scratch_bytes={d} adapter_scratch_bytes={d} scratch_bytes={d} owned_rank_bytes={d} heap_requested_bytes={d} nodes={d} classes={d} summaries={d} map_nodes={d} directory_bytes={d} slot_logical_bytes={d} slot_reserved_virtual_bytes={d} storage_backend={s} module={s}\n", .{
            phase,                @intFromPtr(self),     self.env.types.len(),              slots,                      self.successful_keys,         @sizeOf(SealedTypeKeys),       slot_bytes,
            identity_bytes,       index_bytes,           @intFromBool(self.writer != null), memory.engine.descriptions, memory.engine.class_index,    memory.engine.summaries,       memory.engine.identity_maps,
            persistent,           memory.engine.scratch, memory.adapter_scratch,            scratch,                    memory.owned_ranks,           total,                         memory.nodes,
            memory.classes,       memory.summaries,      memory.map_nodes,                  directory_bytes,            storage_memory.logical_bytes, storage_memory.reserved_bytes, if (use_zeroed_mapping) "virtual" else "heap_pages",
            self.env.module_name,
        });
    }

    fn storage(self: *SealedTypeKeys) Allocator.Error!KeyStorage {
        return self.table.getOrCompute(self.io, Allocator.Error, self, allocateStorage);
    }

    fn allocateStorage(self: *SealedTypeKeys) Allocator.Error!KeyStorage {
        if (self.test_fail_storage.enabled()) return error.OutOfMemory;
        return KeyStorage.init(self.allocator, self.identity.items.len);
    }

    fn slot(self: *SealedTypeKeys, var_: Var) Allocator.Error!*KeySlot {
        const raw: usize = @backingInt(var_);
        std.debug.assert(raw < self.identity.items.len);
        const storage_ = try self.storage();
        return storage_.slot(self.allocator, self.io, raw);
    }

    /// The key of `var_`, a checked representative of the module's sealed
    /// store, and the facts its encoding found.
    pub fn info(self: *SealedTypeKeys, var_: Var) Allocator.Error!TypeKeyInfo {
        const key_slot = try self.slot(var_);
        return key_slot.getOrCompute(self.io, Allocator.Error, KeyRequest{ .keys = self, .var_ = var_ }, KeyRequest.compute);
    }

    const KeyRequest = struct {
        keys: *SealedTypeKeys,
        var_: Var,

        fn compute(request: KeyRequest) Allocator.Error!TypeKeyInfo {
            const keys = request.keys;
            keys.writer_mutex.lockUncancelable(keys.io);
            defer keys.writer_mutex.unlock(keys.io);
            if (keys.writer == null) {
                keys.writer = canonical_type_keys.TypeWriter.init(keys.allocator, &keys.env.types, keys.env);
                keys.writer.?.retainComposedKeys();
            }
            const result = try keys.writer.?.fromVar(request.var_);
            if (comptime base.dev_diagnostics.available or builtin.is_test) keys.successful_keys += 1;
            return result;
        }
    };

    /// Whether `var_` reaches an identity variable, when a walk recorded it.
    pub fn knownIdentity(self: *const SealedTypeKeys, var_: Var) ?bool {
        const index: usize = @backingInt(var_);
        std.debug.assert(index < self.identity.items.len);
        return switch (self.identity.items[index].load(.acquire)) {
            0 => null,
            1 => false,
            else => true,
        };
    }

    /// Record whether `var_` reaches an identity variable.
    pub fn recordIdentity(self: *SealedTypeKeys, var_: Var, value: bool) void {
        const index: usize = @backingInt(var_);
        std.debug.assert(index < self.identity.items.len);
        self.identity.items[index].store(if (value) 2 else 1, .release);
    }

    /// The lowest reached checked representative of `view`'s store whose key
    /// is `key`, if any.
    pub fn varForKey(self: *SealedTypeKeys, view: sealed_type_view.SealedTypeView, key: canonical.CanonicalTypeKey) Allocator.Error!?Var {
        const index = try self.index.getOrCompute(self.io, Allocator.Error, IndexRequest{ .keys = self, .view = view }, IndexRequest.build);
        return index.get(key.bytes);
    }

    const IndexRequest = struct {
        keys: *SealedTypeKeys,
        view: sealed_type_view.SealedTypeView,

        fn build(request: IndexRequest) Allocator.Error!*KeyIndex {
            const keys = request.keys;
            const allocator = keys.allocator;
            const index = try allocator.create(KeyIndex);
            index.* = .empty;
            errdefer {
                index.deinit(allocator);
                allocator.destroy(index);
            }
            for (request.view.var_facts, 0..) |facts, raw| {
                if (!facts.reached) continue;
                const var_: Var = @fromBackingInt(@intCast(raw));
                if (request.view.representative(var_) != var_) continue;
                const entry = try index.getOrPut(allocator, (try keys.info(var_)).key.bytes);
                if (!entry.found_existing) entry.value_ptr.* = var_;
            }
            return index;
        }
    };
};

/// Real solver-assigned representatives spanning full pages and a partial
/// final page. The redirect is reached but must not become an index entry.
const SlotTestStore = struct {
    const count = slots_per_page * 3 + 5;
    env: ModuleEnv,
    vars: [count]Var,
    redirect: Var,
    facts: [count + 1]sealed_type_view.SealedVarFacts,

    fn init(allocator: Allocator) Allocator.Error!SlotTestStore {
        var env = try ModuleEnv.init(allocator, "");
        errdefer env.deinit();
        var vars: [count]Var = undefined;
        for (&vars, 0..) |*var_, index| {
            const content: types.Content = if (index == count - 1)
                .err
            else if (index == slots_per_page * 2)
                .{ .flex = types.Flex.init() }
            else
                .{ .structure = .empty_record };
            var_.* = try env.types.freshFromContent(content);
        }
        const redirect = try env.types.freshRedirect(vars[slots_per_page]);
        try env.types.canonicalizeAndSeal(env.getIdentStoreConst());
        var facts: [count + 1]sealed_type_view.SealedVarFacts = @splat(.{});
        for ([_]Var{ vars[slots_per_page - 1], vars[slots_per_page], vars[count - 1], redirect }) |var_| {
            facts[@backingInt(var_)].reached = true;
        }
        return .{ .env = env, .vars = vars, .redirect = redirect, .facts = facts };
    }

    fn deinit(self: *SlotTestStore) void {
        self.env.deinit();
    }

    fn view(self: *const SlotTestStore) sealed_type_view.SealedTypeView {
        return .{ .env = &self.env, .names = &.{}, .origins = &.{}, .nominals = &.{}, .var_facts = &self.facts, .capabilities = &.{} };
    }

    fn freshInfo(self: *const SlotTestStore, var_: Var) Allocator.Error!TypeKeyInfo {
        return canonical_type_keys.fromVarInfo(std.testing.allocator, &self.env.types, &self.env, var_);
    }
};

test "source key storage preserves keys identity proofs and allocation-free hits" {
    const allocator = std.testing.allocator;
    var fixture = try SlotTestStore.init(allocator);
    defer fixture.deinit();
    var failing = std.testing.FailingAllocator.init(allocator, .{});
    const keys = try SealedTypeKeys.create(failing.allocator(), std.testing.io, &fixture.env);
    defer keys.destroy();
    try std.testing.expect(keys.table.peek() == null);
    keys.recordIdentity(fixture.vars[0], false);
    keys.recordIdentity(fixture.vars[slots_per_page * 2], true);
    try std.testing.expect(keys.table.peek() == null);

    const requested = [_]Var{ fixture.vars[0], fixture.vars[slots_per_page - 1], fixture.vars[slots_per_page], fixture.vars[SlotTestStore.count - 1] };
    for (requested) |var_| try std.testing.expectEqual(try fixture.freshInfo(var_), try keys.info(var_));
    const stored = keys.table.peek().?;
    if (comptime use_zeroed_mapping) {
        try std.testing.expectEqual(fixture.facts.len, stored.slots.len);
        try std.testing.expectEqual(@as(usize, 0), stored.memory().allocated_bytes);
        try std.testing.expect(stored.memory().reserved_bytes >= fixture.facts.len * @sizeOf(KeySlot));
    } else {
        try std.testing.expectEqual(@as(usize, 4), stored.pages.len);
        try std.testing.expect(stored.pages[2].peek() == null);
    }
    try std.testing.expectEqual(requested.len, keys.successful_keys);
    try std.testing.expectEqual(@as(?bool, false), keys.knownIdentity(fixture.vars[0]));
    try std.testing.expectEqual(@as(?bool, true), keys.knownIdentity(fixture.vars[slots_per_page * 2]));
    try std.testing.expectEqual(@as(?bool, null), keys.knownIdentity(fixture.vars[slots_per_page]));

    // Cached reads neither allocate nor move the directory, pages or keys.
    const first = try keys.slot(requested[0]);
    const last = try keys.slot(requested[requested.len - 1]);
    failing.fail_index = failing.alloc_index;
    failing.resize_fail_index = failing.resize_index;
    for (requested) |var_| try std.testing.expectEqual(try fixture.freshInfo(var_), try keys.info(var_));
    try std.testing.expectEqual(first, try keys.slot(requested[0]));
    try std.testing.expectEqual(last, try keys.slot(requested[requested.len - 1]));
    try std.testing.expectEqual(requested.len, keys.successful_keys);
}

test "source key storage publishes one table and wakes key waiters while other keys progress" {
    if (@import("builtin").single_threaded) return error.SkipZigTest;
    const allocator = std.testing.allocator;
    const io = std.testing.io;
    var fixture = try SlotTestStore.init(allocator);
    defer fixture.deinit();
    for ([_]bool{ true, false }) |hold_table| {
        const keys = try SealedTypeKeys.create(allocator, io, &fixture.env);
        defer keys.destroy();
        const held = if (hold_table) null else try keys.slot(fixture.vars[0]);
        // First race the storage publication; then hold one key while other
        // keys progress. Neither claim is held with the writer lock.
        if (hold_table) {
            try std.testing.expect(keys.table.acquire(io) == .claimed);
        } else {
            try std.testing.expect(held.?.acquire(io) == .claimed);
        }
        const Shared = struct {
            keys: *SealedTypeKeys,
            vars: [8]Var,
            results: [8]Allocator.Error!TypeKeyInfo = undefined,
            other_page_results: std.atomic.Value(u32) = .init(0),

            fn run(self: *@This(), index: usize) void {
                self.results[index] = self.keys.info(self.vars[index]);
                if (index >= 4) _ = self.other_page_results.fetchAdd(1, .release);
            }
        };
        var shared = Shared{ .keys = keys, .vars = .{
            fixture.vars[0],              fixture.vars[0],              fixture.vars[slots_per_page - 1],      fixture.vars[slots_per_page - 1],
            fixture.vars[slots_per_page], fixture.vars[slots_per_page], fixture.vars[SlotTestStore.count - 1], fixture.vars[SlotTestStore.count - 1],
        } };
        var threads: [8]std.Thread = undefined;
        var started: usize = 0;
        var key_claimed = true;
        defer {
            // Also release waiters and join started threads if spawning fails.
            if (key_claimed) {
                if (hold_table) keys.table.abandon(io) else held.?.abandon(io);
            }
            for (threads[0..started]) |thread| thread.join();
        }
        for (&threads, 0..) |*thread, index| {
            thread.* = try std.Thread.spawn(.{}, Shared.run, .{ &shared, index });
            started += 1;
        }
        if (hold_table) {
            while (keys.table.state.load(.acquire) != .contended) std.atomic.spinLoopHint();
        } else {
            while (held.?.state.load(.acquire) != .contended) std.atomic.spinLoopHint();
            while (shared.other_page_results.load(.acquire) != 4) std.atomic.spinLoopHint();
        }
        // Abandonment wakes the contenders to retry the storage or key claim.
        if (hold_table) keys.table.abandon(io) else held.?.abandon(io);
        key_claimed = false;
        for (threads[0..started]) |thread| thread.join();
        started = 0;
        for (shared.vars, shared.results) |var_, result| try std.testing.expectEqual(try fixture.freshInfo(var_), try result);
        try std.testing.expectEqual(@as(usize, 4), keys.successful_keys);
        if (held) |key_slot| try std.testing.expectEqual(key_slot, try keys.slot(fixture.vars[0]));
    }
}

test "source key storage retries mapping publication key and index failures" {
    const allocator = std.testing.allocator;
    var fixture = try SlotTestStore.init(allocator);
    defer fixture.deinit();
    var failing = std.testing.FailingAllocator.init(allocator, .{});
    const keys = try SealedTypeKeys.create(failing.allocator(), std.testing.io, &fixture.env);
    defer keys.destroy();
    const first = fixture.vars[0];
    const expected = try fixture.freshInfo(first);
    keys.recordIdentity(first, false);

    keys.test_fail_storage = .on;
    try std.testing.expectError(error.OutOfMemory, keys.info(first));
    try std.testing.expect(keys.table.peek() == null);
    try std.testing.expectEqual(@as(usize, 0), keys.successful_keys);
    keys.test_fail_storage = .off;
    const first_slot = try keys.slot(first);

    failing.fail_index = failing.alloc_index;
    failing.resize_fail_index = failing.resize_index;
    try std.testing.expectError(error.OutOfMemory, keys.info(first));
    try std.testing.expect(first_slot.peek() == null);
    failing.fail_index = std.math.maxInt(usize);
    failing.resize_fail_index = std.math.maxInt(usize);
    try std.testing.expectEqual(expected, try keys.info(first));

    try std.testing.expectEqual(@as(usize, 1), keys.successful_keys);
    try std.testing.expectEqual(expected, try keys.info(first));
    try std.testing.expectEqual(@as(usize, 1), keys.successful_keys);
    try std.testing.expectEqual(expected, try keys.info(fixture.vars[slots_per_page]));

    failing.fail_index = failing.alloc_index;
    try std.testing.expectError(error.OutOfMemory, keys.varForKey(fixture.view(), expected.key));
    try std.testing.expect(keys.index.peek() == null);
    failing.fail_index = std.math.maxInt(usize);
    try std.testing.expectEqual(@as(?Var, fixture.vars[slots_per_page - 1]), try keys.varForKey(fixture.view(), expected.key));
    try std.testing.expectEqual(@as(?bool, false), keys.knownIdentity(first));
    try std.testing.expectEqual(first_slot, try keys.slot(first));
}

fn storageAllocationFailureCase(allocator: Allocator, fixture: *const SlotTestStore, expected: TypeKeyInfo) (Allocator.Error || error{ TestUnexpectedResult, TestExpectedEqual })!void {
    const keys = try SealedTypeKeys.create(allocator, std.testing.io, &fixture.env);
    defer keys.destroy();
    try std.testing.expectEqual(expected, try keys.info(fixture.vars[0]));
    const view = fixture.view();
    // The first computed root is unreached. The index must choose the
    // lowest reached representative, even when a later page was keyed first.
    try std.testing.expectEqual(expected, try keys.info(fixture.vars[slots_per_page]));
    try std.testing.expectEqual(@as(?Var, fixture.vars[slots_per_page - 1]), try keys.varForKey(view, expected.key));
    const error_info = try keys.info(fixture.vars[SlotTestStore.count - 1]);
    try std.testing.expectEqual(@as(?Var, fixture.vars[SlotTestStore.count - 1]), try keys.varForKey(view, error_info.key));
    try std.testing.expectEqual(@as(?Var, null), try keys.varForKey(view, .{ .bytes = @splat(0xff) }));
    try std.testing.expectEqual(fixture.vars[slots_per_page], view.representative(fixture.redirect));
    try std.testing.expectEqual(@as(u32, 2), keys.index.peek().?.count());
    if (comptime use_zeroed_mapping) {
        try std.testing.expect(keys.table.peek().?.slots[@backingInt(fixture.redirect)].peek() == null);
    } else {
        const pages = keys.table.peek().?.pages;
        try std.testing.expect(pages[2].peek() == null);
        try std.testing.expect(pages[3].peek().?[@backingInt(fixture.redirect) % slots_per_page].peek() == null);
    }
}

test "source key storage release every allocation failure including reverse indexing" {
    var fixture = try SlotTestStore.init(std.testing.allocator);
    defer fixture.deinit();
    const expected = try fixture.freshInfo(fixture.vars[0]);
    try std.testing.checkAllAllocationFailures(std.testing.allocator, storageAllocationFailureCase, .{ &fixture, expected });
}

/// Deliberately nonzero raw heap allocations: this allocator supplies no
/// zero guarantee, including in optimized tests without debug poisoning.
const DirtyTestAllocator = struct {
    backing: Allocator,
    allocations: usize = 0,

    fn allocator(self: *DirtyTestAllocator) Allocator {
        return .{ .ptr = self, .vtable = &.{ .alloc = alloc, .resize = Allocator.noResize, .remap = Allocator.noRemap, .free = free } };
    }

    fn alloc(context: *anyopaque, len: usize, alignment: std.mem.Alignment, return_address: usize) ?[*]u8 {
        const self: *DirtyTestAllocator = @ptrCast(@alignCast(context));
        const bytes = self.backing.rawAlloc(len, alignment, return_address) orelse return null;
        @memset(bytes[0..len], 0xa5);
        self.allocations += 1;
        return bytes;
    }

    fn free(context: *anyopaque, bytes: []u8, alignment: std.mem.Alignment, return_address: usize) void {
        const self: *DirtyTestAllocator = @ptrCast(@alignCast(context));
        self.backing.rawFree(bytes, alignment, return_address);
    }
};

test "source key storage fresh native mappings bypass dirty and failing heap allocators" {
    if (comptime !use_zeroed_mapping) return error.SkipZigTest;
    var dirty = DirtyTestAllocator{ .backing = std.testing.allocator };
    const info: TypeKeyInfo = .{ .key = .{ .bytes = @splat(29) }, .contains_identity_variables = false, .composable = true };
    // Repeated mapping after writes tests fresh-zero admission independently
    // of whether the OS chooses the same virtual address again.
    for (0..3) |_| {
        const mapping = try ZeroedKeyMapping.init(dirty.allocator(), 7);
        defer mapping.deinit(dirty.allocator());
        try std.testing.expectEqual(@as(usize, 0), dirty.allocations);
        for ([_]usize{ 0, 6 }) |raw| {
            const key_slot = try mapping.slot(dirty.allocator(), std.testing.io, raw);
            try std.testing.expect(key_slot.peek() == null);
            try std.testing.expect(key_slot.acquire(std.testing.io) == .claimed);
            key_slot.publish(std.testing.io, info);
            try std.testing.expectEqual(info, key_slot.peek().?);
        }
        const memory = mapping.memory();
        try std.testing.expectEqual(@as(usize, 7 * @sizeOf(KeySlot)), memory.logical_bytes);
        try std.testing.expectEqual(@as(usize, 0), memory.allocated_bytes);
        try std.testing.expect(memory.reserved_bytes >= memory.logical_bytes);
    }
    var failing = std.testing.FailingAllocator.init(std.testing.allocator, .{ .fail_index = 0 });
    const mapping = try ZeroedKeyMapping.init(failing.allocator(), 1);
    defer mapping.deinit(failing.allocator());
    try std.testing.expectEqual(@as(usize, 0), failing.alloc_index);
    try std.testing.expect(mapping.slots[0].peek() == null);
    const empty = try ZeroedKeyMapping.init(failing.allocator(), 0);
    defer empty.deinit(failing.allocator());
    try std.testing.expectEqual(@as(usize, 0), empty.reserved_bytes);
    const too_many = std.math.maxInt(usize) / @sizeOf(KeySlot);
    try std.testing.expectError(error.OutOfMemory, ZeroedKeyMapping.init(failing.allocator(), too_many + 1));
    // Multiplication fits, but page rounding does not; no syscall is needed.
    try std.testing.expectError(error.OutOfMemory, ZeroedKeyMapping.init(failing.allocator(), too_many));
}

fn heapPagesAllocationCase(allocator: Allocator) (Allocator.Error || error{ TestExpectedEqual, TestUnexpectedResult })!void {
    var dirty = DirtyTestAllocator{ .backing = allocator };
    const heap = dirty.allocator();
    const pages = try HeapKeyPages.init(heap, slots_per_page * 3 + 1);
    defer pages.deinit(heap);
    const info: TypeKeyInfo = .{ .key = .{ .bytes = @splat(29) }, .contains_identity_variables = false, .composable = true };
    for ([_]usize{ 0, slots_per_page - 1, slots_per_page, slots_per_page * 3 }) |raw| {
        const key_slot = try pages.slot(heap, std.testing.io, raw);
        try std.testing.expect(key_slot.peek() == null);
        try std.testing.expect(key_slot.acquire(std.testing.io) == .claimed);
        key_slot.publish(std.testing.io, info);
        try std.testing.expectEqual(info, key_slot.peek().?);
        try std.testing.expectEqual(key_slot, try pages.slot(heap, std.testing.io, raw));
    }
    try std.testing.expect(pages.pages[2].peek() == null);
    const memory = pages.memory();
    try std.testing.expectEqual(@as(usize, 3 * @sizeOf(KeyPage)), memory.allocated_bytes);
    try std.testing.expectEqual(@as(usize, 4 * @sizeOf(PageSlot)), memory.directory_bytes);
    try std.testing.expectEqual(@as(usize, 0), memory.reserved_bytes);
}

test "source key storage WASM pages initialize dirty heap storage and release every allocation failure" {
    // Exercise the explicit WASM backend on native hosts too.
    try std.testing.checkAllAllocationFailures(std.testing.allocator, heapPagesAllocationCase, .{});
}

test "source key storage native zero state spans OS pages and mappings have independent lifetimes" {
    if (comptime !use_zeroed_mapping) return error.SkipZigTest;
    const allocator = std.testing.allocator;
    const page_size = std.heap.pageSize();
    const count = page_size * 3 / @sizeOf(KeySlot) + 5;
    const first = try ZeroedKeyMapping.init(allocator, count);
    var first_live = true;
    defer if (first_live) first.deinit(allocator);
    const second = try ZeroedKeyMapping.init(allocator, count);
    defer second.deinit(allocator);
    try std.testing.expect(first.slots.ptr != second.slots.ptr);
    const first_info: TypeKeyInfo = .{ .key = .{ .bytes = @splat(31) }, .contains_identity_variables = false, .composable = true };
    const second_info: TypeKeyInfo = .{ .key = .{ .bytes = @splat(47) }, .contains_identity_variables = true, .composable = false };
    const positions = [_]usize{ 0, (page_size - 1) / @sizeOf(KeySlot), page_size / @sizeOf(KeySlot) + 1, page_size * 2 / @sizeOf(KeySlot) + 1, count - 1 };
    for (positions) |raw| {
        const a = &first.slots[raw];
        const b = &second.slots[raw];
        try std.testing.expect(a.peek() == null);
        try std.testing.expect(b.peek() == null);
        try std.testing.expect(a.acquire(std.testing.io) == .claimed);
        a.publish(std.testing.io, first_info);
        try std.testing.expect(b.peek() == null);
        try std.testing.expect(b.acquire(std.testing.io) == .claimed);
        b.publish(std.testing.io, second_info);
        try std.testing.expectEqual(first_info, a.peek().?);
    }
    first.deinit(allocator);
    first_live = false;
    // Unmapping one owner cannot affect another live owner's key proofs.
    for (positions) |raw| try std.testing.expectEqual(second_info, second.slots[raw].peek().?);
    const fresh = try ZeroedKeyMapping.init(allocator, count);
    defer fresh.deinit(allocator);
    for (positions) |raw| try std.testing.expect(fresh.slots[raw].peek() == null);
}
