//! Run-local concrete source type payload registry.
//!
//! `CanonicalTypeKey` is only an identity key. Any stage that constructs a
//! concrete mono/lambda/executable request must retain an explicit payload ref so
//! later lowering can clone or inspect the checked type graph without
//! deriving it from the key.

const std = @import("std");
const check = @import("check");

const Allocator = std.mem.Allocator;
const checked_artifact = check.CheckedArtifact;
const canonical = check.CanonicalNames;

/// Public `ConcreteSourceTypeRef` declaration.
pub const ConcreteSourceTypeRef = enum(u32) { _ };

/// Public `ConcreteSourceTypeSource` declaration.
pub const ConcreteSourceTypeSource = union(enum) {
    artifact: checked_artifact.ArtifactCheckedTypeRef,
    local: checked_artifact.CheckedTypeId,
};

/// Public `ConcreteSourceTypeRoot` declaration.
pub const ConcreteSourceTypeRoot = struct {
    key: canonical.CanonicalTypeKey,
    source: ConcreteSourceTypeSource,
};

const SourceKind = enum {
    artifact,
    local,
};

const SourceKey = struct {
    kind: SourceKind,
    artifact: [32]u8 = [_]u8{0} ** 32,
    ty: u32,
};

/// Public `Store` declaration.
pub const Store = struct {
    allocator: Allocator,
    roots: std.ArrayList(ConcreteSourceTypeRoot),
    by_key: std.StringHashMap(ConcreteSourceTypeRef),
    by_source: std.AutoHashMap(SourceKey, ConcreteSourceTypeRef),
    local_roots: std.ArrayList(checked_artifact.CheckedTypeRoot),
    local_payloads: std.ArrayList(checked_artifact.CheckedTypePayload),

    pub fn init(allocator: Allocator) Store {
        return .{
            .allocator = allocator,
            .roots = .empty,
            .by_key = std.StringHashMap(ConcreteSourceTypeRef).init(allocator),
            .by_source = std.AutoHashMap(SourceKey, ConcreteSourceTypeRef).init(allocator),
            .local_roots = .empty,
            .local_payloads = .empty,
        };
    }

    pub fn deinit(self: *Store) void {
        for (self.local_payloads.items) |*payload| deinitPayload(self.allocator, payload);
        self.local_payloads.deinit(self.allocator);
        self.local_roots.deinit(self.allocator);
        var keys = self.by_key.keyIterator();
        while (keys.next()) |stored_key| self.allocator.free(stored_key.*);
        self.by_key.deinit();
        self.by_source.deinit();
        self.roots.deinit(self.allocator);
        self.* = Store.init(self.allocator);
    }

    pub fn localView(self: *const Store) checked_artifact.CheckedTypeStoreView {
        return .{
            .roots = self.local_roots.items,
            .schemes = &.{},
            .payloads = self.local_payloads.items,
        };
    }

    pub fn registerArtifactRoot(
        self: *Store,
        artifact: checked_artifact.CheckedModuleArtifactKey,
        checked_types: checked_artifact.CheckedTypeStoreView,
        checked_root: checked_artifact.CheckedTypeId,
    ) Allocator.Error!ConcreteSourceTypeRef {
        const raw = @intFromEnum(checked_root);
        if (raw >= checked_types.roots.len) {
            invariantViolation("concrete source type store received a checked type id outside the artifact root table");
        }

        return try self.registerRoot(
            .{
                .key = checked_types.roots[raw].key,
                .source = .{ .artifact = .{
                    .artifact = artifact,
                    .ty = checked_root,
                } },
            },
            !try checkedTypeContainsIdentityVariables(self.allocator, checked_types.payloads, checked_root),
        );
    }

    pub fn registerLocalRoot(
        self: *Store,
        checked_root: checked_artifact.CheckedTypeId,
    ) Allocator.Error!ConcreteSourceTypeRef {
        const raw = @intFromEnum(checked_root);
        if (raw >= self.local_roots.items.len) {
            invariantViolation("concrete source type store received a local checked type id outside the local root table");
        }

        return try self.registerRoot(
            .{
                .key = self.local_roots.items[raw].key,
                .source = .{ .local = checked_root },
            },
            !try checkedTypeContainsIdentityVariables(self.allocator, self.local_payloads.items, checked_root),
        );
    }

    pub fn reserveLocalRoot(
        self: *Store,
        type_key: canonical.CanonicalTypeKey,
    ) Allocator.Error!checked_artifact.CheckedTypeId {
        const id: checked_artifact.CheckedTypeId = @enumFromInt(@as(u32, @intCast(self.local_roots.items.len)));
        try self.local_roots.append(self.allocator, .{
            .id = id,
            .key = type_key,
        });
        errdefer _ = self.local_roots.pop();
        try self.local_payloads.append(self.allocator, .pending);
        return id;
    }

    pub fn reservePendingLocalRoot(
        self: *Store,
    ) Allocator.Error!checked_artifact.CheckedTypeId {
        return try self.reserveLocalRoot(.{});
    }

    pub fn fillLocalRoot(
        self: *Store,
        checked_root: checked_artifact.CheckedTypeId,
        payload: checked_artifact.CheckedTypePayload,
    ) void {
        const raw = @intFromEnum(checked_root);
        if (raw >= self.local_payloads.items.len) {
            invariantViolation("concrete source type store fill referenced an unknown local root");
        }
        deinitPayload(self.allocator, &self.local_payloads.items[raw]);
        self.local_payloads.items[raw] = payload;
    }

    pub fn sealLocalRoot(
        self: *Store,
        checked_root: checked_artifact.CheckedTypeId,
        type_key: canonical.CanonicalTypeKey,
    ) Allocator.Error!ConcreteSourceTypeRef {
        const raw = @intFromEnum(checked_root);
        if (raw >= self.local_roots.items.len) {
            invariantViolation("concrete source type store seal referenced an unknown local root");
        }
        self.local_roots.items[raw].key = type_key;
        return try self.registerLocalRoot(checked_root);
    }

    pub fn root(self: *const Store, ref: ConcreteSourceTypeRef) ConcreteSourceTypeRoot {
        return self.roots.items[@intFromEnum(ref)];
    }

    pub fn key(self: *const Store, ref: ConcreteSourceTypeRef) canonical.CanonicalTypeKey {
        return self.root(ref).key;
    }

    pub fn refForKey(self: *const Store, type_key: canonical.CanonicalTypeKey) ?ConcreteSourceTypeRef {
        return self.by_key.get(type_key.bytes[0..]);
    }

    fn registerRoot(
        self: *Store,
        new_root: ConcreteSourceTypeRoot,
        dedupe_by_key: bool,
    ) Allocator.Error!ConcreteSourceTypeRef {
        const source_key = sourceKey(new_root.source);
        if (self.by_source.get(source_key)) |existing| return existing;

        if (dedupe_by_key) {
            if (self.by_key.get(new_root.key.bytes[0..])) |existing| {
                const existing_root = self.root(existing);
                if (!std.mem.eql(u8, existing_root.key.bytes[0..], new_root.key.bytes[0..])) {
                    invariantViolation("concrete source type store key map returned a non-equivalent payload");
                }
                return existing;
            }
        }

        const id: ConcreteSourceTypeRef = @enumFromInt(@as(u32, @intCast(self.roots.items.len)));
        const owned_key = if (dedupe_by_key) try self.allocator.dupe(u8, new_root.key.bytes[0..]) else null;
        errdefer if (owned_key) |key_bytes| self.allocator.free(key_bytes);

        try self.roots.append(self.allocator, new_root);
        errdefer _ = self.roots.pop();
        try self.by_source.put(source_key, id);
        errdefer _ = self.by_source.remove(source_key);
        if (owned_key) |key_bytes| try self.by_key.put(key_bytes, id);
        return id;
    }
};

fn sourceKey(source: ConcreteSourceTypeSource) SourceKey {
    return switch (source) {
        .artifact => |artifact| .{
            .kind = .artifact,
            .artifact = artifact.artifact.bytes,
            .ty = @intFromEnum(artifact.ty),
        },
        .local => |local| .{
            .kind = .local,
            .ty = @intFromEnum(local),
        },
    };
}

fn checkedTypeContainsIdentityVariables(
    allocator: Allocator,
    payloads: []const checked_artifact.CheckedTypePayload,
    root: checked_artifact.CheckedTypeId,
) Allocator.Error!bool {
    var active = std.AutoHashMap(checked_artifact.CheckedTypeId, void).init(allocator);
    defer active.deinit();
    return try checkedTypeContainsIdentityVariablesHelp(payloads, root, &active);
}

fn checkedTypeContainsIdentityVariablesHelp(
    payloads: []const checked_artifact.CheckedTypePayload,
    root: checked_artifact.CheckedTypeId,
    active: *std.AutoHashMap(checked_artifact.CheckedTypeId, void),
) Allocator.Error!bool {
    const raw = @intFromEnum(root);
    if (raw >= payloads.len) invariantViolation("concrete source type identity scan referenced missing payload");
    if (active.contains(root)) return false;
    try active.put(root, {});
    defer _ = active.remove(root);

    return switch (payloads[raw]) {
        .pending,
        .flex,
        .rigid,
        => true,
        .alias => |alias| blk: {
            if (try checkedTypeContainsIdentityVariablesHelp(payloads, alias.backing, active)) break :blk true;
            for (alias.args) |arg| {
                if (try checkedTypeContainsIdentityVariablesHelp(payloads, arg, active)) break :blk true;
            }
            break :blk false;
        },
        .record => |record| blk: {
            for (record.fields) |field| {
                if (try checkedTypeContainsIdentityVariablesHelp(payloads, field.ty, active)) break :blk true;
            }
            break :blk try checkedTypeContainsIdentityVariablesHelp(payloads, record.ext, active);
        },
        .record_unbound => |fields| blk: {
            for (fields) |field| {
                if (try checkedTypeContainsIdentityVariablesHelp(payloads, field.ty, active)) break :blk true;
            }
            break :blk false;
        },
        .tuple => |items| blk: {
            for (items) |item| {
                if (try checkedTypeContainsIdentityVariablesHelp(payloads, item, active)) break :blk true;
            }
            break :blk false;
        },
        .nominal => |nominal| blk: {
            if (try checkedTypeContainsIdentityVariablesHelp(payloads, nominal.backing, active)) break :blk true;
            for (nominal.args) |arg| {
                if (try checkedTypeContainsIdentityVariablesHelp(payloads, arg, active)) break :blk true;
            }
            break :blk false;
        },
        .function => |function| blk: {
            for (function.args) |arg| {
                if (try checkedTypeContainsIdentityVariablesHelp(payloads, arg, active)) break :blk true;
            }
            break :blk try checkedTypeContainsIdentityVariablesHelp(payloads, function.ret, active);
        },
        .empty_record,
        .empty_tag_union,
        => false,
        .tag_union => |tag_union| blk: {
            for (tag_union.tags) |tag| {
                for (tag.args) |arg| {
                    if (try checkedTypeContainsIdentityVariablesHelp(payloads, arg, active)) break :blk true;
                }
            }
            break :blk try checkedTypeContainsIdentityVariablesHelp(payloads, tag_union.ext, active);
        },
    };
}

/// Public `PayloadKeyBuilder` declaration.
pub const PayloadKeyBuilder = struct {
    allocator: Allocator,
    names: *const canonical.CanonicalNameStore,
    payloads: []const checked_artifact.CheckedTypePayload,
    hasher: std.crypto.hash.sha2.Sha256,
    active: std.AutoHashMap(checked_artifact.CheckedTypeId, u32),

    pub fn init(
        allocator: Allocator,
        names: *const canonical.CanonicalNameStore,
        payloads: []const checked_artifact.CheckedTypePayload,
    ) PayloadKeyBuilder {
        return .{
            .allocator = allocator,
            .names = names,
            .payloads = payloads,
            .hasher = std.crypto.hash.sha2.Sha256.init(.{}),
            .active = std.AutoHashMap(checked_artifact.CheckedTypeId, u32).init(allocator),
        };
    }

    pub fn deinit(self: *PayloadKeyBuilder) void {
        self.active.deinit();
    }

    pub fn keyForRoot(self: *PayloadKeyBuilder, root: checked_artifact.CheckedTypeId) Allocator.Error!canonical.CanonicalTypeKey {
        try self.writeType(root);
        return .{ .bytes = self.hasher.finalResult() };
    }

    fn writeType(self: *PayloadKeyBuilder, id: checked_artifact.CheckedTypeId) Allocator.Error!void {
        if (self.active.get(id)) |slot| {
            self.writeTag("cycle");
            self.writeU32(slot);
            return;
        }
        const raw = @intFromEnum(id);
        if (raw >= self.payloads.len) {
            invariantViolation("concrete source type key builder referenced missing payload");
        }

        const slot: u32 = @intCast(self.active.count());
        try self.active.put(id, slot);
        try self.writePayload(self.payloads[raw]);
        _ = self.active.remove(id);
    }

    fn writePayload(self: *PayloadKeyBuilder, payload: checked_artifact.CheckedTypePayload) Allocator.Error!void {
        switch (payload) {
            .pending => invariantViolation("concrete source type key requested for pending payload"),
            .flex => |flex| {
                self.writeTag("flex");
                self.writeBool(flex.name != null);
                if (flex.name) |name| self.writeBytes(name);
                try self.writeConstraints(flex.constraints);
            },
            .rigid => |rigid| {
                self.writeTag("rigid");
                if (rigid.name) |name| {
                    self.writeBytes(name);
                } else {
                    self.writeBytes("");
                }
                try self.writeConstraints(rigid.constraints);
            },
            .alias => |alias| {
                self.writeTag("alias");
                self.writeBytes(self.names.typeNameText(alias.name));
                self.writeBytes(self.names.moduleNameText(alias.origin_module));
                try self.writeType(alias.backing);
                self.writeU32(@intCast(alias.args.len));
                for (alias.args) |arg| try self.writeType(arg);
            },
            .record_unbound => |fields| {
                self.writeTag("record_unbound");
                try self.writeRecordFields(fields);
            },
            .record => |record| {
                self.writeTag("record");
                try self.writeRecordFields(record.fields);
                try self.writeType(record.ext);
            },
            .tuple => |tuple| {
                self.writeTag("tuple");
                self.writeU32(@intCast(tuple.len));
                for (tuple) |elem| try self.writeType(elem);
            },
            .nominal => |nominal| {
                self.writeTag("nominal");
                self.writeBytes(self.names.typeNameText(nominal.name));
                self.writeBytes(self.names.moduleNameText(nominal.origin_module));
                self.writeBool(nominal.is_opaque);
                try self.writeType(nominal.backing);
                self.writeU32(@intCast(nominal.args.len));
                for (nominal.args) |arg| try self.writeType(arg);
            },
            .function => |func| {
                switch (func.kind) {
                    .pure => self.writeTag("fn_pure"),
                    .effectful => self.writeTag("fn_effectful"),
                    .unbound => self.writeTag("fn_unbound"),
                }
                self.writeBool(func.needs_instantiation);
                self.writeU32(@intCast(func.args.len));
                for (func.args) |arg| try self.writeType(arg);
                try self.writeType(func.ret);
            },
            .empty_record => self.writeTag("empty_record"),
            .tag_union => |tag_union| {
                self.writeTag("tag_union");
                self.writeU32(@intCast(tag_union.tags.len));
                for (tag_union.tags) |tag| {
                    self.writeBytes(self.names.tagLabelText(tag.name));
                    self.writeU32(@intCast(tag.args.len));
                    for (tag.args) |arg| try self.writeType(arg);
                }
                try self.writeType(tag_union.ext);
            },
            .empty_tag_union => self.writeTag("empty_tag_union"),
        }
    }

    fn writeRecordFields(
        self: *PayloadKeyBuilder,
        fields: []const checked_artifact.CheckedRecordField,
    ) Allocator.Error!void {
        self.writeU32(@intCast(fields.len));
        for (fields) |field| {
            self.writeBytes(self.names.recordFieldLabelText(field.name));
            try self.writeType(field.ty);
        }
    }

    fn writeConstraints(
        self: *PayloadKeyBuilder,
        constraints: []const checked_artifact.CheckedStaticDispatchConstraint,
    ) Allocator.Error!void {
        self.writeU32(@intCast(constraints.len));
        for (constraints) |constraint| {
            self.writeBytes(self.names.methodNameText(constraint.fn_name));
            try self.writeType(constraint.fn_ty);
            self.writeTag(@tagName(constraint.origin));
            self.writeBool(constraint.binop_negated);
            self.writeBool(constraint.num_literal != null);
            if (constraint.num_literal) |num_literal| {
                self.hasher.update(&num_literal.bytes);
                self.writeBool(num_literal.is_u128);
                self.writeBool(num_literal.is_negative);
                self.writeBool(num_literal.is_fractional);
            }
        }
    }

    fn writeTag(self: *PayloadKeyBuilder, tag: []const u8) void {
        self.writeBytes(tag);
    }

    fn writeBytes(self: *PayloadKeyBuilder, bytes: []const u8) void {
        self.writeU32(@intCast(bytes.len));
        self.hasher.update(bytes);
    }

    fn writeBool(self: *PayloadKeyBuilder, value: bool) void {
        const byte: [1]u8 = if (value) .{1} else .{0};
        self.hasher.update(&byte);
    }

    fn writeU32(self: *PayloadKeyBuilder, value: u32) void {
        var bytes: [4]u8 = undefined;
        std.mem.writeInt(u32, &bytes, value, .little);
        self.hasher.update(&bytes);
    }
};

pub fn deinitPayload(allocator: Allocator, payload: *checked_artifact.CheckedTypePayload) void {
    switch (payload.*) {
        .pending,
        .empty_record,
        .empty_tag_union,
        => {},
        .flex => |flex| {
            if (flex.name) |name| allocator.free(name);
            deinitConstraints(allocator, flex.constraints);
        },
        .rigid => |rigid| {
            if (rigid.name) |name| allocator.free(name);
            deinitConstraints(allocator, rigid.constraints);
        },
        .alias => |alias| allocator.free(alias.args),
        .record => |record| allocator.free(record.fields),
        .record_unbound => |fields| allocator.free(fields),
        .tuple => |elems| allocator.free(elems),
        .nominal => |nominal| allocator.free(nominal.args),
        .function => |function| allocator.free(function.args),
        .tag_union => |tag_union| {
            for (tag_union.tags) |tag| allocator.free(tag.args);
            allocator.free(tag_union.tags);
        },
    }
    payload.* = .pending;
}

fn deinitConstraints(
    allocator: Allocator,
    constraints: []const checked_artifact.CheckedStaticDispatchConstraint,
) void {
    allocator.free(constraints);
}

fn invariantViolation(comptime message: []const u8) noreturn {
    if (@import("builtin").mode == .Debug) {
        std.debug.panic(message, .{});
    }
    unreachable;
}

test "concrete source type store declarations are referenced" {
    std.testing.refAllDecls(@This());
}
