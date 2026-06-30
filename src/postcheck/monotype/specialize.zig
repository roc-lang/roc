//! Monotype specialization worklist.

const std = @import("std");
const check = @import("check");
const Ast = @import("ast.zig");
const Type = @import("type.zig");

const names = check.CheckedNames;

/// Monotype function template paired with its requested function type.
pub const Spec = struct {
    fn_def: Ast.FnTemplate,
    ty: Type.TypeId,
};

/// Work queue for Monotype specialization.
pub const Queue = struct {
    entries: std.ArrayList(Spec),

    pub fn init() Queue {
        return .{ .entries = .empty };
    }

    pub fn deinit(self: *Queue, allocator: std.mem.Allocator) void {
        self.entries.deinit(allocator);
    }

    /// Add a specialization request if the exact request is not already queued.
    pub fn enqueue(self: *Queue, allocator: std.mem.Allocator, spec: Spec) std.mem.Allocator.Error!bool {
        for (self.entries.items) |existing| {
            if (specEql(existing, spec)) return false;
        }

        try self.entries.append(allocator, spec);
        return true;
    }
};

/// Result of reserving or reusing a specialization record.
pub const ReserveResult = struct {
    spec: ?Ast.SpecId,
    target: Ast.FnSlot,
    created: bool,
};

/// Existing specialization found without allocating a fresh local record.
pub const LookupResult = struct {
    spec: ?Ast.SpecId,
    target: Ast.FnSlot,
};

const LoadedSpecId = enum(u32) { _ };

const LoadedSpec = struct {
    record: Ast.SpecRecord,
    types: Type.DurableView,
    imported: Ast.ImportedFnId,
};

const SpecEntryId = union(enum(u8)) {
    local: Ast.SpecId,
    loaded: LoadedSpecId,
};

/// Direct specialization reservation table keyed by callable identity and type digest.
pub const SpecBuilder = struct {
    allocator: std.mem.Allocator,
    names: *const names.NameStore,
    types: *const Type.Store,
    records: *std.ArrayList(Ast.SpecRecord),
    loaded_records: std.ArrayList(LoadedSpec),
    lookup: std.AutoHashMap(SpecLookupDigest, std.ArrayList(SpecEntryId)),

    pub fn init(
        allocator: std.mem.Allocator,
        name_store: *const names.NameStore,
        type_store: *const Type.Store,
        records: *std.ArrayList(Ast.SpecRecord),
    ) SpecBuilder {
        return .{
            .allocator = allocator,
            .names = name_store,
            .types = type_store,
            .records = records,
            .loaded_records = .empty,
            .lookup = std.AutoHashMap(SpecLookupDigest, std.ArrayList(SpecEntryId)).init(allocator),
        };
    }

    pub fn deinit(self: *SpecBuilder) void {
        var lists = self.lookup.valueIterator();
        while (lists.next()) |list| list.deinit(self.allocator);
        self.lookup.deinit();
        self.loaded_records.deinit(self.allocator);
    }

    pub fn insertLoadedReady(
        self: *SpecBuilder,
        record: Ast.SpecRecord,
        types: Type.DurableView,
        imported: Ast.ImportedFnId,
    ) std.mem.Allocator.Error!LoadedSpecId {
        if (record.status != .ready) @import("../common.zig").invariant("loaded Monotype specialization record was not ready");

        const loaded_id: LoadedSpecId = @enumFromInt(@as(u32, @intCast(self.loaded_records.items.len)));
        try self.loaded_records.append(self.allocator, .{
            .record = record,
            .types = types,
            .imported = imported,
        });
        errdefer _ = self.loaded_records.pop();

        const lookup_digest = SpecLookupDigest.from(record.identity);
        const gop = try self.lookup.getOrPut(lookup_digest);
        if (!gop.found_existing) gop.value_ptr.* = .empty;
        try gop.value_ptr.append(self.allocator, .{ .loaded = loaded_id });
        return loaded_id;
    }

    pub fn reserve(
        self: *SpecBuilder,
        identity: Ast.SpecIdentity,
        fn_id: Ast.FnId,
    ) std.mem.Allocator.Error!ReserveResult {
        if (try self.find(identity)) |hit| {
            return .{
                .spec = hit.spec,
                .target = hit.target,
                .created = false,
            };
        }

        const lookup_digest = SpecLookupDigest.from(identity);
        const gop = try self.lookup.getOrPut(lookup_digest);
        if (!gop.found_existing) gop.value_ptr.* = .empty;

        const spec_id: Ast.SpecId = @enumFromInt(@as(u32, @intCast(self.records.items.len)));
        try self.records.append(self.allocator, .{
            .identity = identity,
            .fn_id = fn_id,
            .status = .reserved,
        });
        errdefer _ = self.records.pop();
        try gop.value_ptr.append(self.allocator, .{ .local = spec_id });
        return .{
            .spec = spec_id,
            .target = .{ .local = fn_id },
            .created = true,
        };
    }

    pub fn find(self: *SpecBuilder, identity: Ast.SpecIdentity) std.mem.Allocator.Error!?LookupResult {
        const entries = self.lookup.get(SpecLookupDigest.from(identity)) orelse return null;
        for (entries.items) |entry_id| {
            if (!try self.entryMatches(entry_id, identity)) continue;
            return .{
                .spec = switch (entry_id) {
                    .local => |spec_id| spec_id,
                    .loaded => null,
                },
                .target = self.entryTarget(entry_id),
            };
        }
        return null;
    }

    pub fn markLowering(self: *SpecBuilder, spec: Ast.SpecId) void {
        self.recordPtr(spec).status = .lowering;
    }

    pub fn markReady(self: *SpecBuilder, spec: Ast.SpecId, fn_id: Ast.FnId) void {
        const record = self.recordPtr(spec);
        record.fn_id = fn_id;
        record.status = .ready;
    }

    pub fn updateLocalIdentity(self: *SpecBuilder, spec: Ast.SpecId, identity: Ast.SpecIdentity) std.mem.Allocator.Error!void {
        const record = self.recordPtr(spec);
        record.identity = identity;

        const lookup_digest = SpecLookupDigest.from(identity);
        const gop = try self.lookup.getOrPut(lookup_digest);
        if (!gop.found_existing) gop.value_ptr.* = .empty;
        for (gop.value_ptr.items) |entry| {
            switch (entry) {
                .local => |existing| if (existing == spec) return,
                .loaded => {},
            }
        }
        try gop.value_ptr.append(self.allocator, .{ .local = spec });
    }

    fn recordPtr(self: *SpecBuilder, spec: Ast.SpecId) *Ast.SpecRecord {
        const index = @intFromEnum(spec);
        if (index >= self.records.items.len) @import("../common.zig").invariant("Monotype spec builder referenced a missing record");
        return &self.records.items[index];
    }

    fn entryMatches(self: *SpecBuilder, entry_id: SpecEntryId, identity: Ast.SpecIdentity) std.mem.Allocator.Error!bool {
        const record = self.entryRecord(entry_id);
        if (!std.meta.eql(record.identity.callable, identity.callable)) return false;
        if (!digestEql(record.identity.source_fn_ty_digest, identity.source_fn_ty_digest)) return false;
        if (!digestEql(record.identity.mono_fn_ty_digest, identity.mono_fn_ty_digest)) return false;
        return switch (entry_id) {
            .local => try self.types.typeEql(self.names, record.identity.mono_fn_ty, identity.mono_fn_ty),
            .loaded => |loaded_id| blk: {
                const loaded = self.loaded_records.items[@intFromEnum(loaded_id)];
                break :blk try Type.typeEqlAcrossStores(
                    self.allocator,
                    self.names,
                    self.types.view(),
                    identity.mono_fn_ty,
                    loaded.types,
                    record.identity.mono_fn_ty,
                );
            },
        };
    }

    fn entryRecord(self: *const SpecBuilder, entry_id: SpecEntryId) Ast.SpecRecord {
        return switch (entry_id) {
            .local => |spec_id| self.records.items[@intFromEnum(spec_id)],
            .loaded => |loaded_id| self.loaded_records.items[@intFromEnum(loaded_id)].record,
        };
    }

    fn entryTarget(self: *const SpecBuilder, entry_id: SpecEntryId) Ast.FnSlot {
        return switch (entry_id) {
            .local => |spec_id| .{ .local = self.records.items[@intFromEnum(spec_id)].fn_id },
            .loaded => |loaded_id| .{ .imported = self.loaded_records.items[@intFromEnum(loaded_id)].imported },
        };
    }
};

const SpecLookupDigest = struct {
    callable_digest: [32]u8,
    source_digest: [32]u8,
    mono_digest: [32]u8,

    fn from(identity: Ast.SpecIdentity) SpecLookupDigest {
        return .{
            .callable_digest = callableDigest(identity.callable),
            .source_digest = identity.source_fn_ty_digest.bytes,
            .mono_digest = identity.mono_fn_ty_digest.bytes,
        };
    }
};

fn specEql(left: Spec, right: Spec) bool {
    return Ast.fnTemplateIdentityEql(left.fn_def, right.fn_def) and left.ty == right.ty;
}

fn callableDigest(callable: Ast.CallableIdentity) [32]u8 {
    var hasher = std.crypto.hash.sha2.Sha256.init(.{});
    switch (callable) {
        .proc_template => |template| {
            writeBytes(&hasher, "proc_template");
            hasher.update(&template.module.bytes);
            writeU32(&hasher, template.proc_base);
            writeU32(&hasher, template.template);
        },
        .nested_site => |site| {
            writeBytes(&hasher, "nested_site");
            hasher.update(&site.module.bytes);
            writeU32(&hasher, site.owner_proc_base);
            writeU32(&hasher, site.owner_template);
            hasher.update(&site.owner_fn_digest.bytes);
            writeU32(&hasher, site.site);
        },
        .hosted => |hosted| {
            writeBytes(&hasher, "hosted");
            writeU32(&hasher, @intFromEnum(hosted));
        },
        .generated => |generated| {
            writeBytes(&hasher, "generated");
            writeU32(&hasher, @intFromEnum(generated));
        },
    }
    return hasher.finalResult();
}

fn digestEql(left: names.TypeDigest, right: names.TypeDigest) bool {
    return std.mem.eql(u8, left.bytes[0..], right.bytes[0..]);
}

fn writeBytes(hasher: *std.crypto.hash.sha2.Sha256, bytes: []const u8) void {
    writeU32(hasher, @intCast(bytes.len));
    hasher.update(bytes);
}

fn writeU32(hasher: *std.crypto.hash.sha2.Sha256, value: u32) void {
    const little = std.mem.nativeToLittle(u32, value);
    hasher.update(std.mem.asBytes(&little));
}

test "monotype specialize declarations are referenced" {
    std.testing.refAllDecls(@This());
}

test "monotype specialize queue keeps exact requests once" {
    var queue = Queue.init();
    defer queue.deinit(std.testing.allocator);

    const spec = testSpec(0, 0, 0);

    try std.testing.expect(try queue.enqueue(std.testing.allocator, spec));
    try std.testing.expect(!try queue.enqueue(std.testing.allocator, spec));
    try std.testing.expectEqual(@as(usize, 1), queue.entries.items.len);
}

test "monotype specialize queue keeps distinct requests" {
    var queue = Queue.init();
    defer queue.deinit(std.testing.allocator);

    try std.testing.expect(try queue.enqueue(std.testing.allocator, testSpec(0, 0, 0)));
    try std.testing.expect(try queue.enqueue(std.testing.allocator, testSpec(0, 0, 1)));
    try std.testing.expect(try queue.enqueue(std.testing.allocator, testSpec(0, 1, 0)));
    try std.testing.expectEqual(@as(usize, 3), queue.entries.items.len);
}

test "monotype specialize queue coalesces equivalent checked function type ids" {
    var queue = Queue.init();
    defer queue.deinit(std.testing.allocator);

    const first = testSpecWithSourceType(0, 0, 0, 1);
    const second = testSpecWithSourceType(0, 0, 0, 2);

    try std.testing.expect(try queue.enqueue(std.testing.allocator, first));
    try std.testing.expect(!try queue.enqueue(std.testing.allocator, second));
    try std.testing.expectEqual(@as(usize, 1), queue.entries.items.len);
}

test "monotype spec builder reuses exact specialization identities" {
    var name_store = names.NameStore.init(std.testing.allocator);
    defer name_store.deinit();

    var type_store = Type.Store.init(std.testing.allocator);
    defer type_store.deinit();

    const unit_ty = try type_store.add(.zst);
    const identity = testSpecIdentity(unit_ty, digestWithFirstByte(1), digestWithFirstByte(2));

    var records = std.ArrayList(Ast.SpecRecord).empty;
    defer records.deinit(std.testing.allocator);

    var builder = SpecBuilder.init(std.testing.allocator, &name_store, &type_store, &records);
    defer builder.deinit();

    const requested_fn: Ast.FnId = @enumFromInt(1);
    const duplicate_request_fn: Ast.FnId = @enumFromInt(2);
    try std.testing.expectEqual(@as(?LookupResult, null), try builder.find(identity));

    const first = try builder.reserve(identity, requested_fn);
    const second = try builder.reserve(identity, duplicate_request_fn);

    try std.testing.expect(first.created);
    try std.testing.expect(!second.created);
    try std.testing.expectEqual(first.spec, second.spec);
    try std.testing.expectEqual(Ast.FnSlot{ .local = requested_fn }, first.target);
    try std.testing.expectEqual(Ast.FnSlot{ .local = requested_fn }, second.target);
    try std.testing.expectEqual(@as(usize, 1), builder.records.items.len);
    const found = (try builder.find(identity)) orelse return error.TestUnexpectedResult;
    try std.testing.expectEqual(first.spec, found.spec);
    try std.testing.expectEqual(Ast.FnSlot{ .local = requested_fn }, found.target);

    const first_spec = first.spec orelse return error.TestUnexpectedResult;
    builder.markLowering(first_spec);
    try std.testing.expectEqual(Ast.SpecStatus.lowering, builder.records.items[@intFromEnum(first_spec)].status);
    builder.markReady(first_spec, @enumFromInt(3));
    try std.testing.expectEqual(Ast.SpecStatus.ready, builder.records.items[@intFromEnum(first_spec)].status);
    try std.testing.expectEqual(@as(Ast.FnId, @enumFromInt(3)), builder.records.items[@intFromEnum(first_spec)].fn_id);
}

test "monotype spec builder rekeys local records after sealed identity update" {
    var name_store = names.NameStore.init(std.testing.allocator);
    defer name_store.deinit();

    var type_store = Type.Store.init(std.testing.allocator);
    defer type_store.deinit();

    const request_ty = try type_store.add(.zst);
    const sealed_ty = try type_store.add(.{ .primitive = .str });
    const source_digest = digestWithFirstByte(1);
    const request_identity = testSpecIdentity(request_ty, source_digest, digestWithFirstByte(2));
    const sealed_identity = testSpecIdentity(sealed_ty, source_digest, digestWithFirstByte(3));

    var records = std.ArrayList(Ast.SpecRecord).empty;
    defer records.deinit(std.testing.allocator);

    var builder = SpecBuilder.init(std.testing.allocator, &name_store, &type_store, &records);
    defer builder.deinit();

    const reserved = try builder.reserve(request_identity, @enumFromInt(1));
    const spec = reserved.spec orelse return error.TestUnexpectedResult;

    try std.testing.expectEqual(@as(?LookupResult, null), try builder.find(sealed_identity));
    try builder.updateLocalIdentity(spec, sealed_identity);

    const sealed_found = (try builder.find(sealed_identity)) orelse return error.TestUnexpectedResult;
    try std.testing.expectEqual(@as(?Ast.SpecId, spec), sealed_found.spec);
    try std.testing.expectEqual(Ast.FnSlot{ .local = @as(Ast.FnId, @enumFromInt(1)) }, sealed_found.target);
    try std.testing.expectEqual(@as(?LookupResult, null), try builder.find(request_identity));

    const repeated = try builder.reserve(sealed_identity, @enumFromInt(2));
    try std.testing.expect(!repeated.created);
    try std.testing.expectEqual(@as(?Ast.SpecId, spec), repeated.spec);
    try std.testing.expectEqual(Ast.FnSlot{ .local = @as(Ast.FnId, @enumFromInt(1)) }, repeated.target);
    try std.testing.expectEqual(@as(usize, 1), records.items.len);
}

test "monotype spec builder keeps checked module boundary in callable identity" {
    var name_store = names.NameStore.init(std.testing.allocator);
    defer name_store.deinit();

    var type_store = Type.Store.init(std.testing.allocator);
    defer type_store.deinit();

    const unit_ty = try type_store.add(.zst);
    const source_digest = digestWithFirstByte(1);
    const mono_digest = digestWithFirstByte(2);

    var records = std.ArrayList(Ast.SpecRecord).empty;
    defer records.deinit(std.testing.allocator);

    var builder = SpecBuilder.init(std.testing.allocator, &name_store, &type_store, &records);
    defer builder.deinit();

    const first_module = testSpecIdentityWithModule(unit_ty, moduleDigestWithFirstByte(1), source_digest, mono_digest);
    const second_module = testSpecIdentityWithModule(unit_ty, moduleDigestWithFirstByte(2), source_digest, mono_digest);

    const first = try builder.reserve(first_module, @enumFromInt(1));
    const second = try builder.reserve(second_module, @enumFromInt(2));
    const repeated_first = try builder.reserve(first_module, @enumFromInt(3));

    try std.testing.expect(first.created);
    try std.testing.expect(second.created);
    try std.testing.expect(!repeated_first.created);
    try std.testing.expect(first.spec != second.spec);
    try std.testing.expectEqual(first.spec, repeated_first.spec);
    try std.testing.expectEqual(Ast.FnSlot{ .local = @enumFromInt(1) }, repeated_first.target);
    try std.testing.expectEqual(@as(usize, 2), builder.records.items.len);
}

test "monotype spec builder uses exact type equality after digest match" {
    var name_store = names.NameStore.init(std.testing.allocator);
    defer name_store.deinit();

    var type_store = Type.Store.init(std.testing.allocator);
    defer type_store.deinit();

    const module_name = try name_store.internModuleName("Test");
    const first_name = try name_store.internTypeName("First");
    const second_name = try name_store.internTypeName("Second");

    const first_ty = try type_store.add(.{ .named = .{
        .named_type = .{ .module = .{}, .ty = @enumFromInt(1) },
        .def = .{ .module_name = module_name, .type_name = first_name },
        .kind = .alias,
        .args = Type.Span.empty(),
        .backing = null,
    } });
    const second_ty = try type_store.add(.{ .named = .{
        .named_type = .{ .module = .{}, .ty = @enumFromInt(2) },
        .def = .{ .module_name = module_name, .type_name = second_name },
        .kind = .alias,
        .args = Type.Span.empty(),
        .backing = null,
    } });

    const forced_digest = digestWithFirstByte(9);
    var records = std.ArrayList(Ast.SpecRecord).empty;
    defer records.deinit(std.testing.allocator);

    var builder = SpecBuilder.init(std.testing.allocator, &name_store, &type_store, &records);
    defer builder.deinit();

    const first = try builder.reserve(testSpecIdentity(first_ty, digestWithFirstByte(1), forced_digest), @enumFromInt(1));
    const second = try builder.reserve(testSpecIdentity(second_ty, digestWithFirstByte(1), forced_digest), @enumFromInt(2));

    try std.testing.expect(first.created);
    try std.testing.expect(second.created);
    try std.testing.expect(first.spec != second.spec);
    try std.testing.expectEqual(@as(usize, 2), builder.records.items.len);
}

test "monotype spec builder reuses loaded records through exact cross-store type equality" {
    const allocator = std.testing.allocator;

    var name_store = names.NameStore.init(allocator);
    defer name_store.deinit();

    var current_types = Type.Store.init(allocator);
    defer current_types.deinit();
    var loaded_types = Type.Store.init(allocator);
    defer loaded_types.deinit();

    const current_unit = try current_types.add(.zst);
    _ = try loaded_types.add(.{ .primitive = .str });
    const loaded_unit = try loaded_types.add(.zst);

    const loaded_view = loaded_types.view();
    const loaded_digests = try allocator.alloc(names.TypeDigest, loaded_view.types.len);
    defer allocator.free(loaded_digests);
    for (loaded_digests, 0..) |*digest, index| {
        digest.* = loaded_types.typeDigest(&name_store, @enumFromInt(@as(u32, @intCast(index))));
    }
    const loaded_durable = Type.DurableView{
        .types = loaded_view.types,
        .type_digests = loaded_digests,
        .spans = loaded_view.spans,
        .fields = loaded_view.fields,
        .tags = loaded_view.tags,
        .declared_fields = loaded_view.declared_fields,
    };

    const source_digest = digestWithFirstByte(1);
    const mono_digest = digestWithFirstByte(2);
    const current_identity = testSpecIdentity(current_unit, source_digest, mono_digest);
    const loaded_identity = testSpecIdentity(loaded_unit, source_digest, mono_digest);

    var records = std.ArrayList(Ast.SpecRecord).empty;
    defer records.deinit(allocator);

    var builder = SpecBuilder.init(allocator, &name_store, &current_types, &records);
    defer builder.deinit();

    const imported: Ast.ImportedFnId = @enumFromInt(1);
    _ = try builder.insertLoadedReady(.{
        .identity = loaded_identity,
        .fn_id = @enumFromInt(9),
        .status = .ready,
    }, loaded_durable, imported);

    const found = (try builder.find(current_identity)) orelse return error.TestUnexpectedResult;
    try std.testing.expectEqual(@as(?Ast.SpecId, null), found.spec);
    try std.testing.expectEqual(Ast.FnSlot{ .imported = imported }, found.target);
    try std.testing.expectEqual(@as(usize, 0), builder.records.items.len);

    const hit = try builder.reserve(current_identity, @enumFromInt(1));
    try std.testing.expect(!hit.created);
    try std.testing.expectEqual(@as(?Ast.SpecId, null), hit.spec);
    try std.testing.expectEqual(Ast.FnSlot{ .imported = imported }, hit.target);
    try std.testing.expectEqual(@as(usize, 0), builder.records.items.len);
}

test "monotype spec builder rejects loaded records when exact cross-store type equality fails" {
    const allocator = std.testing.allocator;

    var name_store = names.NameStore.init(allocator);
    defer name_store.deinit();

    var current_types = Type.Store.init(allocator);
    defer current_types.deinit();
    var loaded_types = Type.Store.init(allocator);
    defer loaded_types.deinit();

    const current_unit = try current_types.add(.zst);
    const loaded_str = try loaded_types.add(.{ .primitive = .str });

    const loaded_view = loaded_types.view();
    const loaded_digests = try allocator.alloc(names.TypeDigest, loaded_view.types.len);
    defer allocator.free(loaded_digests);
    for (loaded_digests, 0..) |*digest, index| {
        digest.* = loaded_types.typeDigest(&name_store, @enumFromInt(@as(u32, @intCast(index))));
    }
    const loaded_durable = Type.DurableView{
        .types = loaded_view.types,
        .type_digests = loaded_digests,
        .spans = loaded_view.spans,
        .fields = loaded_view.fields,
        .tags = loaded_view.tags,
        .declared_fields = loaded_view.declared_fields,
    };

    const source_digest = digestWithFirstByte(1);
    const forced_mono_digest = digestWithFirstByte(2);
    const current_identity = testSpecIdentity(current_unit, source_digest, forced_mono_digest);
    const loaded_identity = testSpecIdentity(loaded_str, source_digest, forced_mono_digest);

    var records = std.ArrayList(Ast.SpecRecord).empty;
    defer records.deinit(allocator);

    var builder = SpecBuilder.init(allocator, &name_store, &current_types, &records);
    defer builder.deinit();

    _ = try builder.insertLoadedReady(.{
        .identity = loaded_identity,
        .fn_id = @enumFromInt(9),
        .status = .ready,
    }, loaded_durable, @enumFromInt(1));

    try std.testing.expectEqual(@as(?LookupResult, null), try builder.find(current_identity));

    const miss = try builder.reserve(current_identity, @enumFromInt(1));
    try std.testing.expect(miss.created);
    try std.testing.expect(miss.spec != null);
    try std.testing.expectEqual(Ast.FnSlot{ .local = @as(Ast.FnId, @enumFromInt(1)) }, miss.target);
    try std.testing.expectEqual(@as(usize, 1), builder.records.items.len);
}

fn testSpec(comptime proc_index: u32, comptime source_digest_byte: u8, comptime ty_index: u32) Spec {
    return testSpecWithSourceType(proc_index, source_digest_byte, ty_index, ty_index + 1);
}

fn testSpecWithSourceType(
    comptime proc_index: u32,
    comptime source_digest_byte: u8,
    comptime ty_index: u32,
    comptime source_ty_index: u32,
) Spec {
    return .{
        .fn_def = .{
            .fn_def = .{ .local_template = .{
                .proc_base = @enumFromInt(proc_index),
                .template = @enumFromInt(proc_index + 1),
            } },
            .source_fn_ty = @enumFromInt(source_ty_index),
            .source_fn_key = digestWithFirstByte(source_digest_byte),
            .mono_fn_ty = @enumFromInt(ty_index),
        },
        .ty = @enumFromInt(ty_index),
    };
}

fn digestWithFirstByte(comptime byte: u8) @import("check").CheckedNames.TypeDigest {
    var digest: @import("check").CheckedNames.TypeDigest = .{};
    digest.bytes[0] = byte;
    return digest;
}

fn moduleDigestWithFirstByte(comptime byte: u8) names.CheckedModuleDigest {
    var digest: names.CheckedModuleDigest = .{};
    digest.bytes[0] = byte;
    return digest;
}

fn testSpecIdentity(
    mono_fn_ty: Type.TypeId,
    source_digest: names.TypeDigest,
    mono_digest: names.TypeDigest,
) Ast.SpecIdentity {
    return testSpecIdentityWithModule(mono_fn_ty, .{}, source_digest, mono_digest);
}

fn testSpecIdentityWithModule(
    mono_fn_ty: Type.TypeId,
    module_digest: names.CheckedModuleDigest,
    source_digest: names.TypeDigest,
    mono_digest: names.TypeDigest,
) Ast.SpecIdentity {
    return .{
        .callable = .{ .proc_template = .{
            .module = module_digest,
            .proc_base = 0,
            .template = 1,
        } },
        .source_fn_ty_digest = source_digest,
        .mono_fn_ty_digest = mono_digest,
        .mono_fn_ty = mono_fn_ty,
    };
}
