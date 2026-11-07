//! Canonical type keys of a module's sealed source types, computed on first
//! demand.
//!
//! Each solver variable owns a write-once key slot. The first consumer that
//! needs a variable's key claims its slot and computes the key; consumers
//! that find it claimed wait; consumers that find it done read it with one
//! acquire load. Computations share one key writer, so a component type's
//! key is digested once per module however many roots contain it; the
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
const KeyIndex = std.AutoHashMapUnmanaged([32]u8, Var);

/// The key slots of one module's sealed source types.
pub const SealedTypeKeys = struct {
    allocator: Allocator,
    io: std.Io,
    env: *const ModuleEnv,
    table: collections.OnceSlot([]KeySlot, .sealed_type_key_table) = .empty,
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
        if (self.table.peek()) |table| allocator.free(table);
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
        var done: usize = 0;
        for (table) |*slot| done += @intFromBool(slot.peek() != null);
        const memory: canonical_type_keys.TypeWriter.KeyMemory = if (self.writer) |*writer| writer.retainedKeyMemory() else .{};
        const slot_bytes = table.len * @sizeOf(KeySlot);
        const identity_bytes = self.identity.len * @sizeOf(std.atomic.Value(u8));
        const index_bytes = if (self.index.peek()) |index| @sizeOf(KeyIndex) + type_key_engine.hashRetainedBytes([32]u8, Var, index.*) else @as(usize, 0);
        const persistent = memory.engine.persistent();
        const scratch = memory.engine.scratch + memory.adapter_scratch;
        const total = @sizeOf(SealedTypeKeys) + slot_bytes + identity_bytes + index_bytes + persistent + scratch + memory.owned_ranks;
        std.debug.print("boxy-work: type-key-memory phase={s} owner={x} vars={d} slots={d} done={d} owner_bytes={d} slot_bytes={d} identity_bytes={d} index_bytes={d} writer_present={d} descriptions_bytes={d} class_index_bytes={d} summaries_bytes={d} identity_maps_bytes={d} persistent_bytes={d} engine_scratch_bytes={d} adapter_scratch_bytes={d} scratch_bytes={d} owned_rank_bytes={d} total_bytes={d} nodes={d} classes={d} summaries={d} map_nodes={d} module={s}\n", .{
            phase,          @intFromPtr(self),     self.env.types.len(),              table.len,                  done,                      @sizeOf(SealedTypeKeys), slot_bytes,
            identity_bytes, index_bytes,           @intFromBool(self.writer != null), memory.engine.descriptions, memory.engine.class_index, memory.engine.summaries, memory.engine.identity_maps,
            persistent,     memory.engine.scratch, memory.adapter_scratch,            scratch,                    memory.owned_ranks,        total,                   memory.nodes,
            memory.classes, memory.summaries,      memory.map_nodes,                  self.env.module_name,
        });
    }

    fn slots(self: *SealedTypeKeys) Allocator.Error![]KeySlot {
        return self.table.getOrCompute(self.io, Allocator.Error, self, allocateSlots);
    }

    fn allocateSlots(self: *SealedTypeKeys) Allocator.Error![]KeySlot {
        const table = try self.allocator.alloc(KeySlot, @intCast(self.env.types.len()));
        @memset(table, .empty);
        return table;
    }

    /// The key of `var_`, a checked representative of the module's sealed
    /// store, and the facts its encoding found.
    pub fn info(self: *SealedTypeKeys, var_: Var) Allocator.Error!TypeKeyInfo {
        const table = try self.slots();
        return table[@backingInt(var_)].getOrCompute(self.io, Allocator.Error, KeyRequest{ .keys = self, .var_ = var_ }, KeyRequest.compute);
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
