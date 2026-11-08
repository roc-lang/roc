//! Canonical type keys of a module's sealed source types, computed on first
//! demand.
//!
//! Each solver variable owns a write-once key slot. The first consumer that
//! needs a variable's key claims its slot and computes the key; consumers
//! that find it claimed wait; consumers that find it done read it with an
//! acquire load. A lazy directory publishes fixed pages of 64 slots on
//! demand; a directory entry and its page never move. Computations share
//! one key writer, so a component type's key is digested once per module
//! however many roots contain it; the
//! writer is held only while a claimant computes, never while anyone waits
//! on a slot. Keys are never serialized: a module loaded from a cache entry
//! starts with every slot empty.
//!
//! Resolving a type from its key alone builds the module's key index on
//! first demand, through the module's index slot.

const std = @import("std");
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
const slots_per_page = 64;
const KeyPage = [slots_per_page]KeySlot;
const PageSlot = collections.OnceSlot(*KeyPage, .sealed_type_key_page);
const KeyIndex = std.AutoHashMapUnmanaged([32]u8, Var);

/// The key slots of one module's sealed source types.
pub const SealedTypeKeys = struct {
    allocator: Allocator,
    io: std.Io,
    env: *const ModuleEnv,
    table: collections.OnceSlot([]PageSlot, .sealed_type_key_table) = .empty,
    index: collections.OnceSlot(*KeyIndex, .sealed_type_key_index) = .empty,
    writer_mutex: std.Io.Mutex = .init,
    writer: ?canonical_type_keys.TypeWriter = null,
    /// Per variable: 0 while unknown, else 1 + whether a walk found the
    /// variable reaches an identity variable.
    identity: []std.atomic.Value(u8),

    /// Key slots for `env`'s sealed store. `allocator` must be safe to use
    /// from every thread that reads the module's types.
    pub fn create(allocator: Allocator, io: std.Io, env: *const ModuleEnv) Allocator.Error!*SealedTypeKeys {
        const keys = try allocator.create(SealedTypeKeys);
        errdefer allocator.destroy(keys);
        const identity = try allocator.alloc(std.atomic.Value(u8), @intCast(env.types.len()));
        @memset(identity, .init(0));
        keys.* = .{ .allocator = allocator, .io = io, .env = env, .identity = identity };
        return keys;
    }

    pub fn destroy(self: *SealedTypeKeys) void {
        const allocator = self.allocator;
        if (self.table.peek()) |table| {
            for (table) |*page_slot| {
                if (page_slot.peek()) |page| allocator.destroy(page);
            }
            allocator.free(table);
        }
        if (self.index.peek()) |index| {
            index.deinit(allocator);
            allocator.destroy(index);
        }
        if (self.writer) |*writer| writer.deinit();
        allocator.free(self.identity);
        allocator.destroy(self);
    }

    /// A diagnostic snapshot of this unique source-key owner. No allocations,
    /// slot claims, key computations or memo changes. Shared views print the
    /// same owner address: consumers deduplicate by (phase, owner).
    pub fn traceMemory(self: *SealedTypeKeys, phase: []const u8) void {
        if (!base.dev_diagnostics.isSet(.trace_boxy_work)) return;
        self.writer_mutex.lockUncancelable(self.io);
        defer self.writer_mutex.unlock(self.io);
        const table = self.table.peek() orelse &.{};
        var pages: usize = 0;
        var done: usize = 0;
        for (table) |*page_slot| {
            if (page_slot.peek()) |page| {
                pages += 1;
                for (page) |*key_slot| done += @intFromBool(key_slot.peek() != null);
            }
        }
        const memory: canonical_type_keys.TypeWriter.KeyMemory = if (self.writer) |*writer| writer.retainedKeyMemory() else .{};
        const slots = pages * slots_per_page;
        const slot_bytes = pages * @sizeOf(KeyPage);
        const directory_bytes = table.len * @sizeOf(PageSlot);
        const identity_bytes = self.identity.len * @sizeOf(std.atomic.Value(u8));
        const index_bytes = if (self.index.peek()) |index| @sizeOf(KeyIndex) + type_key_engine.hashRetainedBytes([32]u8, Var, index.*) else @as(usize, 0);
        const persistent = memory.engine.persistent();
        const scratch = memory.engine.scratch + memory.adapter_scratch;
        const total = @sizeOf(SealedTypeKeys) + directory_bytes + slot_bytes + identity_bytes + index_bytes + persistent + scratch + memory.owned_ranks;
        std.debug.print("boxy-work: type-key-memory phase={s} owner={x} vars={d} slots={d} done={d} owner_bytes={d} slot_bytes={d} identity_bytes={d} index_bytes={d} writer_present={d} descriptions_bytes={d} class_index_bytes={d} summaries_bytes={d} identity_maps_bytes={d} persistent_bytes={d} engine_scratch_bytes={d} adapter_scratch_bytes={d} scratch_bytes={d} owned_rank_bytes={d} total_bytes={d} nodes={d} classes={d} summaries={d} map_nodes={d} directory_bytes={d} pages={d} module={s}\n", .{
            phase,          @intFromPtr(self),     self.env.types.len(),              slots,                      done,                      @sizeOf(SealedTypeKeys), slot_bytes,
            identity_bytes, index_bytes,           @intFromBool(self.writer != null), memory.engine.descriptions, memory.engine.class_index, memory.engine.summaries, memory.engine.identity_maps,
            persistent,     memory.engine.scratch, memory.adapter_scratch,            scratch,                    memory.owned_ranks,        total,                   memory.nodes,
            memory.classes, memory.summaries,      memory.map_nodes,                  directory_bytes,            pages,                     self.env.module_name,
        });
    }

    fn directory(self: *SealedTypeKeys) Allocator.Error![]PageSlot {
        return self.table.getOrCompute(self.io, Allocator.Error, self, allocateDirectory);
    }

    fn allocateDirectory(self: *SealedTypeKeys) Allocator.Error![]PageSlot {
        const len = self.identity.len;
        const count = len / slots_per_page + @intFromBool(len % slots_per_page != 0);
        const table = try self.allocator.alloc(PageSlot, count);
        @memset(table, .empty);
        return table;
    }

    fn allocatePage(self: *SealedTypeKeys) Allocator.Error!*KeyPage {
        const page = try self.allocator.create(KeyPage);
        @memset(page, .empty);
        return page;
    }

    fn slot(self: *SealedTypeKeys, var_: Var) Allocator.Error!*KeySlot {
        const raw: usize = @backingInt(var_);
        std.debug.assert(raw < self.identity.len);
        const table = try self.directory();
        // Publish storage before claiming a key or locking the writer.
        // Directory and page claims only allocate and initialize storage.
        const page = try table[raw / slots_per_page].getOrCompute(self.io, Allocator.Error, self, allocatePage);
        return &page[raw % slots_per_page];
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
            return try keys.writer.?.fromVar(request.var_);
        }
    };

    /// Whether `var_` reaches an identity variable, when a walk recorded it.
    pub fn knownIdentity(self: *const SealedTypeKeys, var_: Var) ?bool {
        const index: usize = @backingInt(var_);
        std.debug.assert(index < self.identity.len);
        return switch (self.identity[index].load(.acquire)) {
            0 => null,
            1 => false,
            else => true,
        };
    }

    /// Record whether `var_` reaches an identity variable.
    pub fn recordIdentity(self: *SealedTypeKeys, var_: Var, value: bool) void {
        const index: usize = @backingInt(var_);
        std.debug.assert(index < self.identity.len);
        self.identity[index].store(if (value) 2 else 1, .release);
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
const PageTestStore = struct {
    const count = slots_per_page * 3 + 5;
    env: ModuleEnv,
    vars: [count]Var,
    redirect: Var,
    facts: [count + 1]sealed_type_view.SealedVarFacts,

    fn init(allocator: Allocator) Allocator.Error!PageTestStore {
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

    fn deinit(self: *PageTestStore) void {
        self.env.deinit();
    }

    fn view(self: *const PageTestStore) sealed_type_view.SealedTypeView {
        return .{ .env = &self.env, .names = &.{}, .origins = &.{}, .nominals = &.{}, .var_facts = &self.facts, .capabilities = &.{} };
    }

    fn freshInfo(self: *const PageTestStore, var_: Var) Allocator.Error!TypeKeyInfo {
        return canonical_type_keys.fromVarInfo(std.testing.allocator, &self.env.types, &self.env, var_);
    }
};

test "paged source keys allocate only demanded pages and preserve keys and identity proofs" {
    const allocator = std.testing.allocator;
    var fixture = try PageTestStore.init(allocator);
    defer fixture.deinit();
    var failing = std.testing.FailingAllocator.init(allocator, .{});
    const keys = try SealedTypeKeys.create(failing.allocator(), std.testing.io, &fixture.env);
    defer keys.destroy();
    try std.testing.expect(keys.table.peek() == null);
    keys.recordIdentity(fixture.vars[0], false);
    keys.recordIdentity(fixture.vars[slots_per_page * 2], true);
    try std.testing.expect(keys.table.peek() == null);

    const requested = [_]Var{ fixture.vars[0], fixture.vars[slots_per_page - 1], fixture.vars[slots_per_page], fixture.vars[PageTestStore.count - 1] };
    for (requested) |var_| try std.testing.expectEqual(try fixture.freshInfo(var_), try keys.info(var_));
    const table = keys.table.peek().?;
    try std.testing.expectEqual(@as(usize, 4), table.len);
    try std.testing.expect(table[0].peek() != null);
    try std.testing.expect(table[1].peek() != null);
    try std.testing.expect(table[2].peek() == null);
    try std.testing.expect(table[3].peek() != null);
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
    try std.testing.expectEqual(table.ptr, keys.table.peek().?.ptr);
}

test "paged source keys publish one page to waiters while different pages progress" {
    if (@import("builtin").single_threaded) return error.SkipZigTest;
    const allocator = std.testing.allocator;
    const io = std.testing.io;
    var fixture = try PageTestStore.init(allocator);
    defer fixture.deinit();
    const keys = try SealedTypeKeys.create(allocator, io, &fixture.env);
    defer keys.destroy();
    const table = try keys.directory();
    // Force page contention without a writer lock. Other pages must be
    // computable before this claim is released.
    try std.testing.expect(table[0].acquire(io) == .claimed);
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
        fixture.vars[slots_per_page], fixture.vars[slots_per_page], fixture.vars[PageTestStore.count - 1], fixture.vars[PageTestStore.count - 1],
    } };
    var threads: [8]std.Thread = undefined;
    var started: usize = 0;
    var page_claimed = true;
    defer {
        // Also release waiters and join started threads if spawning fails.
        if (page_claimed) table[0].abandon(io);
        for (threads[0..started]) |thread| thread.join();
    }
    for (&threads, 0..) |*thread, index| {
        thread.* = try std.Thread.spawn(.{}, Shared.run, .{ &shared, index });
        started += 1;
    }
    while (table[0].state.load(.acquire) != .contended) std.atomic.spinLoopHint();
    while (shared.other_page_results.load(.acquire) != 4) std.atomic.spinLoopHint();
    // Abandonment wakes the contenders; exactly one publishes the page.
    table[0].abandon(io);
    page_claimed = false;
    for (threads[0..started]) |thread| thread.join();
    started = 0;
    for (shared.vars, shared.results) |var_, result| try std.testing.expectEqual(try fixture.freshInfo(var_), try result);
    try std.testing.expect(table[2].peek() == null);
    const page = table[0].peek().?;
    try std.testing.expectEqual(&page[0], try keys.slot(fixture.vars[0]));
    try std.testing.expectEqual(&page[slots_per_page - 1], try keys.slot(fixture.vars[slots_per_page - 1]));
}

test "paged source keys retry directory page key and index allocation failures" {
    const allocator = std.testing.allocator;
    var fixture = try PageTestStore.init(allocator);
    defer fixture.deinit();
    var failing = std.testing.FailingAllocator.init(allocator, .{});
    const keys = try SealedTypeKeys.create(failing.allocator(), std.testing.io, &fixture.env);
    defer keys.destroy();
    const first = fixture.vars[0];
    const expected = try fixture.freshInfo(first);
    keys.recordIdentity(first, false);

    failing.fail_index = failing.alloc_index;
    try std.testing.expectError(error.OutOfMemory, keys.info(first));
    try std.testing.expect(keys.table.peek() == null);
    failing.fail_index = std.math.maxInt(usize);
    const table = try keys.directory();

    failing.fail_index = failing.alloc_index;
    try std.testing.expectError(error.OutOfMemory, keys.info(first));
    try std.testing.expect(table[0].peek() == null);
    failing.fail_index = std.math.maxInt(usize);
    const first_slot = try keys.slot(first);

    failing.fail_index = failing.alloc_index;
    failing.resize_fail_index = failing.resize_index;
    try std.testing.expectError(error.OutOfMemory, keys.info(first));
    try std.testing.expect(first_slot.peek() == null);
    failing.fail_index = std.math.maxInt(usize);
    failing.resize_fail_index = std.math.maxInt(usize);
    try std.testing.expectEqual(expected, try keys.info(first));

    failing.fail_index = failing.alloc_index;
    try std.testing.expectError(error.OutOfMemory, keys.info(fixture.vars[slots_per_page]));
    try std.testing.expectEqual(expected, try keys.info(first));
    try std.testing.expect(table[1].peek() == null);
    failing.fail_index = std.math.maxInt(usize);
    try std.testing.expectEqual(expected, try keys.info(fixture.vars[slots_per_page]));

    failing.fail_index = failing.alloc_index;
    try std.testing.expectError(error.OutOfMemory, keys.varForKey(fixture.view(), expected.key));
    try std.testing.expect(keys.index.peek() == null);
    failing.fail_index = std.math.maxInt(usize);
    try std.testing.expectEqual(@as(?Var, fixture.vars[slots_per_page - 1]), try keys.varForKey(fixture.view(), expected.key));
    try std.testing.expectEqual(@as(?bool, false), keys.knownIdentity(first));
    try std.testing.expectEqual(first_slot, try keys.slot(first));
}

fn pageAllocationFailureCase(allocator: Allocator, fixture: *const PageTestStore, expected: TypeKeyInfo) (Allocator.Error || error{ TestUnexpectedResult, TestExpectedEqual })!void {
    const keys = try SealedTypeKeys.create(allocator, std.testing.io, &fixture.env);
    defer keys.destroy();
    try std.testing.expectEqual(expected, try keys.info(fixture.vars[0]));
    const view = fixture.view();
    // The first computed root is unreached. The index must choose the
    // lowest reached representative, even when a later page was keyed first.
    try std.testing.expectEqual(expected, try keys.info(fixture.vars[slots_per_page]));
    try std.testing.expectEqual(@as(?Var, fixture.vars[slots_per_page - 1]), try keys.varForKey(view, expected.key));
    const error_info = try keys.info(fixture.vars[PageTestStore.count - 1]);
    try std.testing.expectEqual(@as(?Var, fixture.vars[PageTestStore.count - 1]), try keys.varForKey(view, error_info.key));
    try std.testing.expectEqual(@as(?Var, null), try keys.varForKey(view, .{ .bytes = @splat(0xff) }));
    try std.testing.expectEqual(fixture.vars[slots_per_page], view.representative(fixture.redirect));
    try std.testing.expectEqual(@as(u32, 2), keys.index.peek().?.count());
    const table = keys.table.peek().?;
    try std.testing.expect(table[2].peek() == null);
    // The redirect shares the final allocated page but is never keyed.
    try std.testing.expect(table[3].peek().?[@backingInt(fixture.redirect) % slots_per_page].peek() == null);
}

test "paged source keys release every allocation failure including reverse indexing" {
    var fixture = try PageTestStore.init(std.testing.allocator);
    defer fixture.deinit();
    const expected = try fixture.freshInfo(fixture.vars[0]);
    try std.testing.checkAllAllocationFailures(std.testing.allocator, pageAllocationFailureCase, .{ &fixture, expected });
}
