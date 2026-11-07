//! Canonical type keys of a module's sealed source types, computed on first
//! demand.
//!
//! Each solver variable owns a write-once key slot. The first consumer that
//! needs a variable's key claims its slot and computes the key; consumers
//! that find it claimed wait; consumers that find it done read it with one
//! acquire load. A claimant computes with a key writer it borrows from the
//! module's writers, so claimants on different threads compute at once; each
//! writer keeps the keys of the component types it digested, so a component
//! is digested once per writer however many roots contain it. A writer is
//! held only while a claimant computes, never while anyone waits on a slot.
//! Keys are never serialized: a module loaded from a cache entry starts with
//! every slot empty.
//!
//! Resolving a type from its key alone builds the module's key index on
//! first demand, through the module's index slot.

const std = @import("std");
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
const CheckedTypeId = @import("checked_ids.zig").CheckedTypeId;

const KeySlot = collections.OnceSlot(TypeKeyInfo, .sealed_type_key);
const KeyIndex = std.AutoHashMapUnmanaged([32]u8, Var);

/// The key slots of one module's sealed source types.
pub const SealedTypeKeys = struct {
    allocator: Allocator,
    io: std.Io,
    env: *const ModuleEnv,
    table: collections.OnceSlot([]KeySlot, .sealed_type_key_table) = .empty,
    index: collections.OnceSlot(*KeyIndex, .sealed_type_key_index) = .empty,
    /// The writers no claimant has borrowed, with capacity for every writer
    /// the module has, and how many it has.
    idle_writers: std.ArrayListUnmanaged(*canonical_type_keys.TypeWriter) = .empty,
    writer_count: usize = 0,
    /// Guards `idle_writers` and `writer_count`; held around a borrow's or a
    /// return's list update only.
    writers_mutex: std.Io.Mutex = .init,
    /// Per variable: 0 while unknown, else 1 + whether a walk found the
    /// variable reaches an identity variable.
    identity: []std.atomic.Value(u8),
    /// Per variable: its canonical id once recorded, else
    /// `unknown_canonical`.
    canonical: []std.atomic.Value(u32),

    const unknown_canonical = std.math.maxInt(u32);

    /// Key slots for `env`'s sealed store. `allocator` must be safe to use
    /// from every thread that reads the module's types.
    pub fn create(allocator: Allocator, io: std.Io, env: *const ModuleEnv) Allocator.Error!*SealedTypeKeys {
        const keys = try allocator.create(SealedTypeKeys);
        errdefer allocator.destroy(keys);
        const identity = try allocator.alloc(std.atomic.Value(u8), @intCast(env.types.len()));
        errdefer allocator.free(identity);
        @memset(identity, .init(0));
        const canonical_ids = try allocator.alloc(std.atomic.Value(u32), @intCast(env.types.len()));
        @memset(canonical_ids, .init(unknown_canonical));
        keys.* = .{ .allocator = allocator, .io = io, .env = env, .identity = identity, .canonical = canonical_ids };
        return keys;
    }

    pub fn destroy(self: *SealedTypeKeys) void {
        const allocator = self.allocator;
        if (self.table.peek()) |table| allocator.free(table);
        if (self.index.peek()) |index| {
            index.deinit(allocator);
            allocator.destroy(index);
        }
        std.debug.assert(self.idle_writers.items.len == self.writer_count);
        for (self.idle_writers.items) |writer| {
            writer.deinit();
            allocator.destroy(writer);
        }
        self.idle_writers.deinit(allocator);
        allocator.free(self.identity);
        allocator.free(self.canonical);
        allocator.destroy(self);
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
            const writer = try keys.borrowWriter();
            defer keys.returnWriter(writer);
            return try writer.fromVar(request.var_);
        }
    };

    /// An idle writer, or a new one when every writer the module has is
    /// borrowed.
    fn borrowWriter(self: *SealedTypeKeys) Allocator.Error!*canonical_type_keys.TypeWriter {
        {
            self.writers_mutex.lockUncancelable(self.io);
            defer self.writers_mutex.unlock(self.io);
            if (self.idle_writers.pop()) |writer| return writer;
        }
        const writer = try self.allocator.create(canonical_type_keys.TypeWriter);
        errdefer self.allocator.destroy(writer);
        writer.* = canonical_type_keys.TypeWriter.init(self.allocator, &self.env.types, self.env);
        writer.retainComposedKeys();
        errdefer writer.deinit();
        self.writers_mutex.lockUncancelable(self.io);
        defer self.writers_mutex.unlock(self.io);
        try self.idle_writers.ensureTotalCapacity(self.allocator, self.writer_count + 1);
        self.writer_count += 1;
        return writer;
    }

    fn returnWriter(self: *SealedTypeKeys, writer: *canonical_type_keys.TypeWriter) void {
        self.writers_mutex.lockUncancelable(self.io);
        defer self.writers_mutex.unlock(self.io);
        self.idle_writers.appendAssumeCapacity(writer);
    }

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

    /// The canonical id of `var_` a previous request recorded.
    pub fn knownCanonical(self: *const SealedTypeKeys, var_: Var) ?CheckedTypeId {
        const index: usize = @backingInt(var_);
        std.debug.assert(index < self.canonical.len);
        const raw = self.canonical[index].load(.acquire);
        if (raw == unknown_canonical) return null;
        return @fromBackingInt(raw);
    }

    /// Record the canonical id of `var_`.
    pub fn recordCanonical(self: *SealedTypeKeys, var_: Var, id: CheckedTypeId) void {
        const index: usize = @backingInt(var_);
        std.debug.assert(index < self.canonical.len);
        self.canonical[index].store(@backingInt(id), .release);
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
