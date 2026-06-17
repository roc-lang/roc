//! Checked compile-time constant store.

const std = @import("std");

const checked_ids = @import("checked_ids.zig");
const names = @import("canonical_names.zig");
const serde = @import("artifact_serde.zig");

const Allocator = std.mem.Allocator;

/// Identifier for a node in the checked const store.
pub const ConstNodeId = enum(u32) { _ };
/// Identifier for a function value in the checked const store.
pub const ConstFnId = enum(u32) { _ };
/// Identifier for stored string backing bytes in the checked const store.
pub const ConstStrDataId = enum(u32) { _ };

/// Scalar value stored by compile-time evaluation.
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

/// Captured checked value inside a compile-time function value.
pub const ConstCapture = struct {
    binder: checked_ids.PatternBinderId,
    value: ConstNodeId,
};

/// Function value stored by compile-time evaluation.
pub const ConstFn = struct {
    fn_def: FnDef,
    source_fn_ty: checked_ids.CheckedTypeId,
    source_fn_key: names.TypeDigest,
    captures: []const ConstCapture = &.{},
};

/// Named type owner for a stored nominal constant.
pub const NamedType = struct {
    module: names.CheckedModuleDigest,
    ty: checked_ids.CheckedTypeId,
};

/// Checked function definition referenced by a stored function value.
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

/// Stored string value.
///
/// `data` identifies the backing bytes. `offset` and `len` describe the string
/// view into those bytes. This lets checked constants keep the sharing needed
/// for readonly static slices while still storing only checked Roc values.
pub const ConstStr = struct {
    data: ConstStrDataId,
    offset: u32,
    len: u32,
};

/// Compile-time constant stored in checked module data.
pub const ConstValue = union(enum) {
    pending,
    zst,
    scalar: ConstScalar,
    str: ConstStr,
    list: []const ConstNodeId,
    box: ConstNodeId,
    tuple: []const ConstNodeId,
    record: []const ConstNodeId,
    crash: ConstStr,
    tag: struct {
        tag_name: []const u8,
        payloads: []const ConstNodeId,
    },
    nominal: struct {
        named_type: NamedType,
        backing: ConstNodeId,
    },
    fn_value: ConstFnId,
};

/// Store of compile-time constants completed by checking finalization.
pub const ConstStore = struct {
    const VisitState = enum { unseen, active, done };

    allocator: Allocator,
    values: std.ArrayList(ConstValue),
    fns: std.ArrayList(ConstFn),
    str_data: std.ArrayList([]const u8),

    pub fn init(allocator: Allocator) ConstStore {
        return .{
            .allocator = allocator,
            .values = .empty,
            .fns = .empty,
            .str_data = .empty,
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

    pub fn addStrData(self: *ConstStore, bytes: []const u8) Allocator.Error!ConstStrDataId {
        const id: ConstStrDataId = @enumFromInt(@as(u32, @intCast(self.str_data.items.len)));
        const owned = try self.allocator.dupe(u8, bytes);
        errdefer self.allocator.free(owned);
        try self.str_data.append(self.allocator, owned);
        return id;
    }

    pub fn get(self: *const ConstStore, id: ConstNodeId) ConstValue {
        return self.values.items[@intFromEnum(id)];
    }

    /// Serialize the three backing lists. Nested owned slices (list/tuple/record
    /// payloads, fn captures, string bytes) are deep-copied on deserialize, so
    /// the restored store frees exactly what `deinit` expects.
    pub fn rocSerialize(self: *const ConstStore, w: *serde.Writer) Allocator.Error!void {
        try serde.serializeValue([]const ConstValue, w, &self.values.items);
        try serde.serializeValue([]const ConstFn, w, &self.fns.items);
        try serde.serializeValue([]const []const u8, w, &self.str_data.items);
    }

    /// Free hook for the artifact deserializer: the store owns its lists and
    /// their nested data via `self.allocator`, so `deinit` frees everything.
    pub fn rocFree(self: *ConstStore, _: Allocator) void {
        self.deinit();
    }

    pub fn rocDeserialize(r: *serde.Reader) serde.Reader.Error!ConstStore {
        var store = ConstStore.init(r.gpa);
        errdefer store.deinit();
        var values: []ConstValue = undefined;
        try serde.deserializeValue([]ConstValue, r, &values);
        store.values = .{ .items = values, .capacity = values.len };
        var fns: []ConstFn = undefined;
        try serde.deserializeValue([]ConstFn, r, &fns);
        store.fns = .{ .items = fns, .capacity = fns.len };
        var str_data: [][]const u8 = undefined;
        try serde.deserializeValue([][]const u8, r, &str_data);
        store.str_data = .{ .items = str_data, .capacity = str_data.len };
        return store;
    }

    pub fn strData(self: *const ConstStore, id: ConstStrDataId) []const u8 {
        const index = @intFromEnum(id);
        if (@import("builtin").mode == .Debug and index >= self.str_data.items.len) {
            constStoreInvariant("string backing id is out of range");
        }
        return self.str_data.items[index];
    }

    pub fn strBytes(self: *const ConstStore, str: ConstStr) []const u8 {
        const backing = self.strData(str.data);
        const offset: usize = str.offset;
        const len: usize = str.len;
        if (@import("builtin").mode == .Debug and (offset > backing.len or len > backing.len - offset)) {
            constStoreInvariant("string view is outside backing data");
        }
        return backing[offset..][0..len];
    }

    pub fn verifyComplete(self: *const ConstStore) Allocator.Error!void {
        if (@import("builtin").mode != .Debug) return;
        for (self.values.items) |value| {
            switch (value) {
                .pending => std.debug.panic("const store invariant violated: completed store contains a pending node", .{}),
                else => {},
            }
        }
        const value_state = try self.allocator.alloc(VisitState, self.values.items.len);
        defer self.allocator.free(value_state);
        @memset(value_state, .unseen);

        const fn_state = try self.allocator.alloc(VisitState, self.fns.items.len);
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
        for (self.str_data.items) |bytes| self.allocator.free(bytes);
        self.str_data.deinit(self.allocator);
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
            .str,
            .crash,
            => {},
            .list => |items| self.allocator.free(items),
            .tuple => |items| self.allocator.free(items),
            .record => |items| self.allocator.free(items),
            .tag => |tag| {
                self.allocator.free(tag.tag_name);
                self.allocator.free(tag.payloads);
            },
        }
    }

    fn verifyAcyclic(
        self: *const ConstStore,
        id: ConstNodeId,
        value_state: []VisitState,
        fn_state: []VisitState,
    ) void {
        const index = @intFromEnum(id);
        if (index >= self.values.items.len) constStoreInvariant("completed store contains an out-of-range value id");
        switch (value_state[index]) {
            .done => return,
            .active => constStoreInvariant("completed store contains a cycle in const node edges"),
            .unseen => {},
        }

        value_state[index] = .active;
        switch (self.values.items[index]) {
            .pending => constStoreInvariant("completed store contains a pending node"),
            .zst, .scalar => {},
            .str, .crash => |str| {
                _ = self.strBytes(str);
            },
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
        if (index >= self.fns.items.len) constStoreInvariant("completed store contains an out-of-range function id");
        switch (fn_state[index]) {
            .done => return,
            .active => constStoreInvariant("completed store contains a recursive function value"),
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
}
