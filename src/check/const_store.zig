//! Checked compile-time constant store.

const std = @import("std");
const base = @import("base");

const checked_ids = @import("checked_ids.zig");
const names = @import("canonical_names.zig");

const Allocator = std.mem.Allocator;

pub const ConstNodeId = enum(u32) { _ };
pub const ConstFnId = enum(u32) { _ };

pub const ConstScalar = union(enum) {
    i8: i8,
    i16: i16,
    i32: i32,
    i64: i64,
    i128: i128,
    u8: u8,
    u16: u16,
    u32: u32,
    u64: u64,
    u128: u128,
    f32_bits: u32,
    f64_bits: u64,
    dec_bits: i128,
};

pub const ConstCapture = struct {
    binder: checked_ids.PatternBinderId,
    value: ConstNodeId,
};

pub const ConstFn = struct {
    fn_def: FnDef,
    source_fn_ty: checked_ids.CheckedTypeId,
    captures: []const ConstCapture = &.{},
};

pub const NamedType = struct {
    module: names.CheckedModuleDigest,
    ty: checked_ids.CheckedTypeId,
};

pub const FnDef = union(enum) {
    local_template: names.ProcTemplate,
    imported_template: names.ProcTemplate,
    nested: struct {
        owner: names.ProcTemplate,
        site: names.ProcSiteId,
    },
    local_hosted: names.ProcTemplate,
    imported_hosted: names.ProcTemplate,
    checked_generated: names.ProcTemplate,
};

pub const ConstValue = union(enum) {
    pending,
    zst,
    scalar: ConstScalar,
    str: []const u8,
    list: []const ConstNodeId,
    box: ConstNodeId,
    tuple: []const ConstNodeId,
    record: []const ConstNodeId,
    tag: struct {
        tag_name: names.TagNameId,
        payloads: []const ConstNodeId,
    },
    nominal: struct {
        named_type: NamedType,
        backing: ConstNodeId,
    },
    fn_value: ConstFnId,
};

pub const ConstStore = struct {
    const VisitState = enum { unseen, active, done };

    allocator: Allocator,
    values: std.ArrayList(ConstValue),
    fns: std.ArrayList(ConstFn),

    pub fn init(allocator: Allocator) ConstStore {
        return .{
            .allocator = allocator,
            .values = .empty,
            .fns = .empty,
        };
    }

    pub fn reserve(self: *ConstStore) Allocator.Error!ConstNodeId {
        const id: ConstNodeId = @enumFromInt(@as(u32, @intCast(self.values.items.len)));
        try self.values.append(self.allocator, .pending);
        return id;
    }

    pub fn fill(self: *ConstStore, id: ConstNodeId, value: ConstValue) void {
        const slot = &self.values.items[@intFromEnum(id)];
        switch (slot.*) {
            .pending => {},
            else => constStoreInvariant("const node filled more than once"),
        }
        slot.* = value;
    }

    pub fn append(self: *ConstStore, value: ConstValue) Allocator.Error!ConstNodeId {
        const id = try self.reserve();
        self.fill(id, value);
        return id;
    }

    pub fn appendFn(self: *ConstStore, fn_value: ConstFn) Allocator.Error!ConstFnId {
        const id: ConstFnId = @enumFromInt(@as(u32, @intCast(self.fns.items.len)));
        try self.fns.append(self.allocator, fn_value);
        return id;
    }

    pub fn get(self: *const ConstStore, id: ConstNodeId) ConstValue {
        return self.values.items[@intFromEnum(id)];
    }

    pub fn verifyPublished(self: *const ConstStore) void {
        if (@import("builtin").mode != .Debug) return;
        for (self.values.items) |value| {
            switch (value) {
                .pending => std.debug.panic("const store invariant violated: published store contains a pending node", .{}),
                else => {},
            }
        }
        const value_state = self.allocator.alloc(VisitState, self.values.items.len) catch |err| switch (err) {
            error.OutOfMemory => @panic("OOM"),
        };
        defer self.allocator.free(value_state);
        @memset(value_state, .unseen);

        const fn_state = self.allocator.alloc(VisitState, self.fns.items.len) catch |err| switch (err) {
            error.OutOfMemory => @panic("OOM"),
        };
        defer self.allocator.free(fn_state);
        @memset(fn_state, .unseen);

        for (self.values.items, 0..) |_, index| {
            self.verifyAcyclic(@enumFromInt(@as(u32, @intCast(index))), value_state, fn_state);
        }
        for (self.fns.items, 0..) |_, index| {
            self.verifyFnAcyclic(@enumFromInt(@as(u32, @intCast(index))), value_state, fn_state);
        }
    }

    pub fn deinit(self: *ConstStore) void {
        for (self.values.items) |*value| self.deinitValue(value);
        for (self.fns.items) |fn_value| self.allocator.free(fn_value.captures);
        self.fns.deinit(self.allocator);
        self.values.deinit(self.allocator);
        self.* = ConstStore.init(self.allocator);
    }

    fn deinitValue(self: *ConstStore, value: *ConstValue) void {
        switch (value.*) {
            .pending,
            .zst,
            .scalar,
            .box,
            .nominal,
            .fn_value,
            => {},
            .str => |bytes| self.allocator.free(bytes),
            .list => |items| self.allocator.free(items),
            .tuple => |items| self.allocator.free(items),
            .record => |items| self.allocator.free(items),
            .tag => |tag| self.allocator.free(tag.payloads),
        }
    }

    fn verifyAcyclic(
        self: *const ConstStore,
        id: ConstNodeId,
        value_state: []VisitState,
        fn_state: []VisitState,
    ) void {
        const index = @intFromEnum(id);
        if (index >= self.values.items.len) constStoreInvariant("published store contains an out-of-range value id");
        switch (value_state[index]) {
            .done => return,
            .active => constStoreInvariant("published store contains a cycle in const node edges"),
            .unseen => {},
        }

        value_state[index] = .active;
        switch (self.values.items[index]) {
            .pending => constStoreInvariant("published store contains a pending node"),
            .zst,
            .scalar,
            .str,
            => {},
            .fn_value => |fn_id| self.verifyFnAcyclic(fn_id, value_state, fn_state),
            .box => |child| self.verifyAcyclic(child, value_state, fn_state),
            .nominal => |nominal| self.verifyAcyclic(nominal.backing, value_state, fn_state),
            .list,
            .tuple,
            .record,
            => |children| {
                for (children) |child| self.verifyAcyclic(child, value_state, fn_state);
            },
            .tag => |tag| {
                for (tag.payloads) |payload| self.verifyAcyclic(payload, value_state, fn_state);
            },
        }
        value_state[index] = .done;
    }

    fn verifyFnAcyclic(
        self: *const ConstStore,
        id: ConstFnId,
        value_state: []VisitState,
        fn_state: []VisitState,
    ) void {
        const index = @intFromEnum(id);
        if (index >= self.fns.items.len) constStoreInvariant("published store contains an out-of-range function id");
        switch (fn_state[index]) {
            .done => return,
            .active => constStoreInvariant("published store contains a recursive function value"),
            .unseen => {},
        }

        fn_state[index] = .active;
        for (self.fns.items[index].captures) |capture| {
            self.verifyAcyclic(capture.value, value_state, fn_state);
        }
        fn_state[index] = .done;
    }
};

fn constStoreInvariant(comptime message: []const u8) noreturn {
    if (@import("builtin").mode == .Debug) {
        std.debug.panic("const store invariant violated: {s}", .{message});
    }
    unreachable;
}

test "const store declarations are referenced" {
    std.testing.refAllDecls(@This());
    _ = base;
}
