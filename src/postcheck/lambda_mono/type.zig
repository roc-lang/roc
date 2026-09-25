//! Lambda Mono type store.
//!
//! Function types do not exist in this store. Finite function values are
//! generated callable tag unions, and erased function values use the erased
//! callable layout selected by direct LIR lowering.

const std = @import("std");
const DigestGraph = @import("../type_digest.zig").Graph;
const TypeDigestHasher = @import("base").TypeDigestHasher;
const check = @import("check");
const collections = @import("collections");

const Common = @import("../common.zig");
const MonoType = @import("../monotype/type.zig");

/// Checked boundary name module used by Lambda Mono types.
pub const names = check.CheckedNames;
const static_dispatch = check.StaticDispatchRegistry;
const GuardedList = collections.GuardedList;

fn StoreList(comptime T: type, comptime field_name: []const u8) type {
    return GuardedList.List(T, "lambda_mono.Type.Store." ++ field_name);
}

/// Guarded immutable span borrow for a named Lambda Mono type-store list.
pub fn StoreSpanBorrow(comptime T: type, comptime field_name: []const u8) type {
    return GuardedList.BorrowSpan(T, "lambda_mono.Type.Store." ++ field_name);
}

/// Identifier for a Lambda Mono type in this store.
pub const TypeId = enum(u32) { _ };
/// Identifier for a Lambda Mono function body.
pub const FnId = enum(u32) { _ };
/// Identifier for a callable variant in this store.
pub const FnVariantId = enum(u32) { _ };

/// Slice descriptor for type, field, tag, or callable-variant arrays.
pub const Span = extern struct {
    start: u32,
    len: u32,

    pub fn empty() Span {
        return .{ .start = 0, .len = 0 };
    }
};

/// Record field type entry. `default` is the Monotype `??` default identity
/// copied verbatim (both stores share the program name store): rows that
/// disagree about defaults are distinct monotypes, and this stage's digests
/// and const-store evidence must preserve that distinction.
pub const Field = struct {
    name: names.RecordFieldNameId,
    ty: TypeId,
    value_ty: ?TypeId = null,
    default: ?MonoType.FieldDefault,
};

/// Capture record field entry.
pub const CaptureField = struct {
    symbol: Common.Symbol,
    binder: ?check.CheckedModule.PatternBinderId,
    capture_id: ?check.CheckedModule.CaptureId = null,
    checked_capture_id: ?check.CheckedModule.CaptureId = null,
    /// Type the function body observes when it reads this capture.
    ty: TypeId,
    /// Type stored in the capture record field.
    storage_ty: TypeId,
};

/// Tag-union variant type entry.
pub const Tag = struct {
    name: names.TagNameId,
    checked_name: names.TagNameId,
    payloads: Span,
};

/// Callable variant entry.
pub const FnVariant = struct {
    id: FnVariantId,
    source: Common.Symbol,
    target: FnId,
    capture_ty: ?TypeId,
};

/// One entry of a nominal record's declared fields. Mirrors
/// `MonoType.DeclaredField`; consumed by layout and descriptor planning.
pub const DeclaredField = union(enum) {
    named: names.RecordFieldNameId,
    padding: TypeId,
};

/// Lambda Mono type content.
pub const Content = union(enum) {
    primitive: MonoType.Primitive,
    named: struct {
        named_type: MonoType.NamedType,
        def: MonoType.TypeDef,
        kind: MonoType.NamedKind,
        builtin_owner: ?static_dispatch.BuiltinOwner = null,
        args: Span,
        backing: ?struct {
            ty: TypeId,
            use: MonoType.BackingUse,
            authority: MonoType.BackingAuthority = .checked_public,
        } = null,
        /// Declared fields for a nominal/opaque record backing; empty otherwise.
        declared_order: Span = Span.empty(),
    },
    record: Span,
    capture_record: Span,
    tuple: Span,
    tag_union: Span,
    callable: Span,
    list: TypeId,
    box: TypeId,
    erased_fn: struct {
        source_fn_ty: names.TypeDigest,
        members: Span = .empty(),
    },
    erased_capture_ptr,
    zst,
};

/// Store for Lambda Mono types and their shared spans.
pub const Store = struct {
    allocator: std.mem.Allocator,
    digest_scratch: DigestScratch,
    types: StoreList(Content, "types"),
    spans: StoreList(TypeId, "spans"),
    fields: StoreList(Field, "fields"),
    capture_fields: StoreList(CaptureField, "capture_fields"),
    tags: StoreList(Tag, "tags"),
    fn_variants: StoreList(FnVariant, "fn_variants"),
    declared_fields: StoreList(DeclaredField, "declared_fields"),

    pub fn init(allocator: std.mem.Allocator) Store {
        return .{
            .allocator = allocator,
            .digest_scratch = DigestScratch.init(allocator),
            .types = .empty,
            .spans = .empty,
            .fields = .empty,
            .capture_fields = .empty,
            .tags = .empty,
            .fn_variants = .empty,
            .declared_fields = .empty,
        };
    }

    pub fn deinit(self: *Store) void {
        self.digest_scratch.deinit();
        self.declared_fields.deinit(self.allocator);
        self.fn_variants.deinit(self.allocator);
        self.tags.deinit(self.allocator);
        self.capture_fields.deinit(self.allocator);
        self.fields.deinit(self.allocator);
        self.spans.deinit(self.allocator);
        self.types.deinit(self.allocator);
    }

    pub fn add(self: *Store, content: Content) std.mem.Allocator.Error!TypeId {
        const id: TypeId = @enumFromInt(@as(u32, @intCast(self.types.len())));
        try self.types.append(self.allocator, content);
        return id;
    }

    pub fn set(self: *Store, id: TypeId, content: Content) void {
        self.types.set(@intFromEnum(id), content);
    }

    pub fn get(self: *const Store, id: TypeId) Content {
        return self.types.unsafeRawItemsForView()[@intFromEnum(id)];
    }

    pub fn typeCount(self: *const Store) usize {
        return self.types.len();
    }

    pub fn addSpan(self: *Store, values: []const TypeId) std.mem.Allocator.Error!Span {
        if (values.len == 0) return .empty();
        const start: u32 = @intCast(self.spans.len());
        try self.spans.appendSlice(self.allocator, values);
        return .{ .start = start, .len = @intCast(values.len) };
    }

    pub fn addFields(self: *Store, values: []const Field) std.mem.Allocator.Error!Span {
        if (values.len == 0) return .empty();
        const start: u32 = @intCast(self.fields.len());
        try self.fields.appendSlice(self.allocator, values);
        return .{ .start = start, .len = @intCast(values.len) };
    }

    pub fn addCaptureFields(self: *Store, values: []const CaptureField) std.mem.Allocator.Error!Span {
        if (values.len == 0) return .empty();
        const start: u32 = @intCast(self.capture_fields.len());
        try self.capture_fields.appendSlice(self.allocator, values);
        return .{ .start = start, .len = @intCast(values.len) };
    }

    pub fn addTags(self: *Store, values: []const Tag) std.mem.Allocator.Error!Span {
        if (values.len == 0) return .empty();
        const start: u32 = @intCast(self.tags.len());
        try self.tags.appendSlice(self.allocator, values);
        return .{ .start = start, .len = @intCast(values.len) };
    }

    pub fn addFnVariants(self: *Store, values: []const FnVariant) std.mem.Allocator.Error!Span {
        if (values.len == 0) return .empty();
        const start: u32 = @intCast(self.fn_variants.len());
        for (values, 0..) |variant, i| {
            var stored = variant;
            stored.id = @enumFromInt(@as(u32, @intCast(start + i)));
            try self.fn_variants.append(self.allocator, stored);
        }
        return .{ .start = start, .len = @intCast(values.len) };
    }

    pub fn span(self: *const Store, span_: Span) StoreSpanBorrow(TypeId, "spans") {
        return self.spans.borrowSpan(span_.start, span_.len);
    }

    pub fn typeAt(self: *const Store, span_: Span, index: usize) TypeId {
        return GuardedList.at(self.span(span_), index);
    }

    pub fn fieldSpan(self: *const Store, span_: Span) StoreSpanBorrow(Field, "fields") {
        return self.fields.borrowSpan(span_.start, span_.len);
    }

    pub fn fieldAt(self: *const Store, span_: Span, index: usize) Field {
        return GuardedList.at(self.fieldSpan(span_), index);
    }

    pub fn captureFieldSpan(self: *const Store, span_: Span) StoreSpanBorrow(CaptureField, "capture_fields") {
        return self.capture_fields.borrowSpan(span_.start, span_.len);
    }

    pub fn tagSpan(self: *const Store, span_: Span) StoreSpanBorrow(Tag, "tags") {
        return self.tags.borrowSpan(span_.start, span_.len);
    }

    pub fn tagAt(self: *const Store, span_: Span, index: usize) Tag {
        return GuardedList.at(self.tagSpan(span_), index);
    }

    pub fn addDeclaredFields(self: *Store, values: []const DeclaredField) std.mem.Allocator.Error!Span {
        if (values.len == 0) return .empty();
        const start: u32 = @intCast(self.declared_fields.len());
        try self.declared_fields.appendSlice(self.allocator, values);
        return .{ .start = start, .len = @intCast(values.len) };
    }

    pub fn declaredFieldSpan(self: *const Store, span_: Span) StoreSpanBorrow(DeclaredField, "declared_fields") {
        return self.declared_fields.borrowSpan(span_.start, span_.len);
    }

    pub fn declaredFieldAt(self: *const Store, span_: Span, index: usize) DeclaredField {
        return GuardedList.at(self.declaredFieldSpan(span_), index);
    }

    pub fn fnVariantSpan(self: *const Store, span_: Span) StoreSpanBorrow(FnVariant, "fn_variants") {
        return self.fn_variants.borrowSpan(span_.start, span_.len);
    }

    pub fn fnVariantAt(self: *const Store, span_: Span, index: usize) FnVariant {
        return GuardedList.at(self.fnVariantSpan(span_), index);
    }

    pub const View = struct {
        types: []const Content,
        spans: []const TypeId,
        fields: []const Field,
        capture_fields: []const CaptureField,
        tags: []const Tag,
        fn_variants: []const FnVariant,
        declared_fields: []const DeclaredField,
    };

    pub fn view(self: *const Store) View {
        return .{
            .types = self.types.unsafeRawItemsForView(),
            .spans = self.spans.unsafeRawItemsForView(),
            .fields = self.fields.unsafeRawItemsForView(),
            .capture_fields = self.capture_fields.unsafeRawItemsForView(),
            .tags = self.tags.unsafeRawItemsForView(),
            .fn_variants = self.fn_variants.unsafeRawItemsForView(),
            .declared_fields = self.declared_fields.unsafeRawItemsForView(),
        };
    }

    pub fn typeDigest(self: *Store, name_store: *const names.NameStore, ty: TypeId) std.mem.Allocator.Error!names.TypeDigest {
        return try self.digest_scratch.run(self, name_store, ty, null);
    }

    /// Digest of `ty` that is the same in every program giving a value this
    /// representation. `typeDigest` names a callable variant by this
    /// program's symbol and function numbering and a capture by its symbol;
    /// this digest names a variant by its source function's content digest
    /// and a capture by its position. It leaves out a variant's
    /// specialization target, which decides a caller's dispatch code but not
    /// the bytes of a value.
    pub fn contentDigest(self: *Store, name_store: *const names.NameStore, ty: TypeId, sources: CallableSources) std.mem.Allocator.Error!names.TypeDigest {
        return try self.digest_scratch.run(self, name_store, ty, sources);
    }

    /// Content digests of the source functions callable variants name.
    pub const CallableSources = struct {
        context: *const anyopaque,
        digest: *const fn (context: *const anyopaque, source: Common.Symbol) [TypeDigestHasher.digest_length]u8,
    };

    fn writeVariantDigest(hasher: *TypeDigestHasher, sources: ?CallableSources, variant: FnVariant) void {
        if (sources) |content| {
            hasher.update(&content.digest(content.context, variant.source));
        } else {
            writeU32(hasher, @intFromEnum(variant.source));
            writeU32(hasher, @intFromEnum(variant.target));
        }
    }

    fn encodeDigestNode(
        self: *const Store,
        name_store: *const names.NameStore,
        scratch: *DigestScratch,
        sources: ?CallableSources,
        ty: TypeId,
    ) std.mem.Allocator.Error!void {
        var scalar_hasher = TypeDigestHasher.init();
        const hasher = &scalar_hasher;
        switch (self.get(ty)) {
            .primitive => |primitive| {
                writeBytes(hasher, "primitive");
                writeBytes(hasher, @tagName(primitive));
            },
            .named => |named| {
                writeBytes(hasher, "named");
                hasher.update(&named.named_type.module.bytes);
                writeBytes(hasher, name_store.moduleIdentityBytes(named.def.module));
                writeOptionalU32(hasher, named.def.source_decl);
                writeBytes(hasher, name_store.typeNameText(named.def.type_name));
                writeOptionalDigest(hasher, named.def.generated);
                writeBytes(hasher, @tagName(named.def.iterator_representation));
                writeBytes(hasher, @tagName(named.def.iterator_kind));
                writeU32(hasher, named.def.iterator_depth);
                writeIteratorTopology(hasher, name_store, named.def.iterator_topology);
                writeBytes(hasher, @tagName(named.kind));
                if (named.builtin_owner) |owner| {
                    writeBytes(hasher, "builtin");
                    writeBytes(hasher, @tagName(owner));
                } else {
                    writeBytes(hasher, "not-builtin");
                }
                try self.writeTypeSpanDigest(hasher, scratch, named.args);
            },
            .record => |fields| {
                writeBytes(hasher, "record");
                const field_slice = self.fieldSpan(fields);
                writeU32(hasher, @intCast(field_slice.len));
                for (0..field_slice.len) |index| {
                    const field = GuardedList.at(field_slice, index);
                    writeBytes(hasher, name_store.recordFieldLabelText(field.name));
                    MonoType.writeFieldDefaultDigest(name_store, hasher, field.default);
                    if (field.value_ty) |value_ty| {
                        writeBytes(hasher, "field-optional-value");
                        try scratch.child(hasher, value_ty);
                    } else {
                        writeBytes(hasher, "field-inline-value");
                    }
                    try scratch.child(hasher, field.ty);
                }
            },
            .capture_record => |fields| {
                writeBytes(hasher, "capture_record");
                const field_slice = self.captureFieldSpan(fields);
                writeU32(hasher, @intCast(field_slice.len));
                for (0..field_slice.len) |index| {
                    const field = GuardedList.at(field_slice, index);
                    if (sources == null) writeU32(hasher, @intFromEnum(field.symbol));
                    try scratch.child(hasher, field.ty);
                    try scratch.child(hasher, field.storage_ty);
                }
            },
            .tuple => |items| {
                writeBytes(hasher, "tuple");
                try self.writeTypeSpanDigest(hasher, scratch, items);
            },
            .tag_union => |tags| {
                writeBytes(hasher, "tag_union");
                const tag_slice = self.tagSpan(tags);
                writeU32(hasher, @intCast(tag_slice.len));
                for (0..tag_slice.len) |index| {
                    const tag = GuardedList.at(tag_slice, index);
                    writeBytes(hasher, name_store.tagLabelText(tag.name));
                    try self.writeTypeSpanDigest(hasher, scratch, tag.payloads);
                }
            },
            .callable => |variants| {
                writeBytes(hasher, "callable");
                const variant_slice = self.fnVariantSpan(variants);
                writeU32(hasher, @intCast(variant_slice.len));
                for (0..variant_slice.len) |index| {
                    const variant = GuardedList.at(variant_slice, index);
                    writeVariantDigest(hasher, sources, variant);
                    if (variant.capture_ty) |capture_ty| {
                        writeBytes(hasher, "capture");
                        try scratch.child(hasher, capture_ty);
                    } else {
                        writeBytes(hasher, "no_capture");
                    }
                }
            },
            .list => |elem| {
                writeBytes(hasher, "list");
                try scratch.child(hasher, elem);
            },
            .box => |elem| {
                writeBytes(hasher, "box");
                try scratch.child(hasher, elem);
            },
            .erased_fn => |erased| {
                writeBytes(hasher, "erased_fn");
                hasher.update(&erased.source_fn_ty.bytes);
                const variant_slice = self.fnVariantSpan(erased.members);
                writeU32(hasher, @intCast(variant_slice.len));
                for (0..variant_slice.len) |index| {
                    const variant = GuardedList.at(variant_slice, index);
                    writeVariantDigest(hasher, sources, variant);
                    if (variant.capture_ty) |capture_ty| {
                        writeBytes(hasher, "capture");
                        try scratch.child(hasher, capture_ty);
                    } else {
                        writeBytes(hasher, "no_capture");
                    }
                }
            },
            .erased_capture_ptr => writeBytes(hasher, "erased_capture_ptr"),
            .zst => writeBytes(hasher, "zst"),
        }
        scratch.graph.setScalar(scratch.node_by_type.get(ty).?, hasher.finalResult());
    }

    fn writeTypeSpanDigest(
        self: *const Store,
        hasher: *TypeDigestHasher,
        scratch: *DigestScratch,
        span_: Span,
    ) std.mem.Allocator.Error!void {
        const values = self.span(span_);
        writeU32(hasher, @intCast(values.len));
        for (0..values.len) |index| {
            const child = GuardedList.at(values, index);
            try scratch.child(hasher, child);
        }
    }
};

/// Retained allocation capacity for one store's graph digest requests. No
/// resolved identity survives a request because Lambda Mono types are mutable.
const DigestScratch = struct {
    allocator: std.mem.Allocator,
    graph: DigestGraph,
    node_by_type: collections.DenseMap(TypeId, u32),
    pending: std.ArrayList(TypeId) = .empty,

    fn init(allocator: std.mem.Allocator) DigestScratch {
        return .{ .allocator = allocator, .graph = DigestGraph.init(allocator), .node_by_type = collections.DenseMap(TypeId, u32).init(allocator) };
    }

    fn deinit(self: *DigestScratch) void {
        self.graph.deinit();
        self.node_by_type.deinit();
        self.pending.deinit(self.allocator);
    }

    fn reset(self: *DigestScratch) void {
        self.graph.reset();
        self.node_by_type.clearRetainingCapacity();
        self.pending.clearRetainingCapacity();
    }

    fn discover(self: *DigestScratch, ty: TypeId) std.mem.Allocator.Error!u32 {
        const entry = try self.node_by_type.getOrPut(ty);
        if (entry.found_existing) return entry.value_ptr.*;
        const node = try self.graph.addNode();
        entry.value_ptr.* = node;
        try self.pending.append(self.allocator, ty);
        return node;
    }

    fn child(self: *DigestScratch, hasher: *TypeDigestHasher, ty: TypeId) std.mem.Allocator.Error!void {
        // Mark the position of every edge among the scalar data, while the
        // graph separately retains the exact ordered child identities.
        writeBytes(hasher, "child");
        try self.graph.putChild(try self.discover(ty));
    }

    fn run(self: *DigestScratch, store: *const Store, name_store: *const names.NameStore, ty: TypeId, sources: ?Store.CallableSources) std.mem.Allocator.Error!names.TypeDigest {
        defer self.reset();
        const root = try self.discover(ty);
        while (self.pending.pop()) |pending_ty| {
            const node = self.node_by_type.get(pending_ty).?;
            self.graph.beginNode(node);
            try store.encodeDigestNode(name_store, self, sources, pending_ty);
            self.graph.endNode(node);
        }
        return .{ .bytes = try self.graph.resolve(root) };
    }
};

/// Test source digests: a symbol's digest is its number modulo 10, so
/// symbols 3 and 13 name one source function.
fn testCallableSource(_: *const anyopaque, source: Common.Symbol) [TypeDigestHasher.digest_length]u8 {
    return @splat(@intCast(@intFromEnum(source) % 10));
}

test "Lambda Mono content digest ignores program numbering of callables and captures" {
    var name_store = names.NameStore.init(std.testing.allocator);
    defer name_store.deinit();
    var store = Store.init(std.testing.allocator);
    defer store.deinit();
    const sources = Store.CallableSources{ .context = &name_store, .digest = testCallableSource };
    const captured = try store.add(.{ .primitive = .bool });
    const Variant = struct { source: u32, target: u32, capture_symbol: u32 };
    const shapes = [_][2]Variant{
        .{ .{ .source = 3, .target = 1, .capture_symbol = 20 }, .{ .source = 4, .target = 2, .capture_symbol = 21 } },
        // The same representation under another program's numbering.
        .{ .{ .source = 13, .target = 8, .capture_symbol = 40 }, .{ .source = 14, .target = 9, .capture_symbol = 41 } },
        // Another source function in the first position.
        .{ .{ .source = 5, .target = 1, .capture_symbol = 20 }, .{ .source = 4, .target = 2, .capture_symbol = 21 } },
    };
    var content: [shapes.len]names.TypeDigest = undefined;
    var numbered: [shapes.len]names.TypeDigest = undefined;
    for (shapes, 0..) |shape, index| {
        var variants: [2]FnVariant = undefined;
        for (shape, &variants, 0..) |variant, *out, position| {
            const capture = try store.add(.{ .capture_record = try store.addCaptureFields(&.{.{
                .symbol = @enumFromInt(variant.capture_symbol),
                .binder = null,
                .ty = captured,
                .storage_ty = captured,
            }}) });
            out.* = .{
                .id = @enumFromInt(position),
                .source = @enumFromInt(variant.source),
                .target = @enumFromInt(variant.target),
                .capture_ty = capture,
            };
        }
        const callable = try store.add(.{ .callable = try store.addFnVariants(&variants) });
        content[index] = try store.contentDigest(&name_store, callable, sources);
        numbered[index] = try store.typeDigest(&name_store, callable);
    }
    try std.testing.expectEqualSlices(u8, &content[0].bytes, &content[1].bytes);
    try std.testing.expect(!std.mem.eql(u8, &numbered[0].bytes, &numbered[1].bytes));
    try std.testing.expect(!std.mem.eql(u8, &content[0].bytes, &content[2].bytes));
}

test "Lambda Mono digest terminates recursive erased captures independent of allocation order" {
    var name_store = names.NameStore.init(std.testing.allocator);
    defer name_store.deinit();
    const field_name = try name_store.internRecordFieldLabel("callback");
    var store = Store.init(std.testing.allocator);
    defer store.deinit();
    var roots: [2]TypeId = undefined;
    for (&roots, 0..) |*root, i| {
        const first = try store.add(.zst);
        const second = try store.add(.zst);
        const third = try store.add(.zst);
        const capture = if (i == 0) first else third;
        const record = second;
        const callable = if (i == 0) third else first;
        store.set(capture, .{ .capture_record = try store.addCaptureFields(&.{.{
            .symbol = @enumFromInt(7),
            .binder = null,
            .ty = record,
            .storage_ty = record,
        }}) });
        store.set(record, .{ .record = try store.addFields(&.{.{
            .name = field_name,
            .ty = callable,
            .default = null,
        }}) });
        store.set(callable, .{
            .erased_fn = .{
                .source_fn_ty = .{ .bytes = @splat(13) },
                .members = try store.addFnVariants(&.{.{
                    .id = undefined, // Assigned by addFnVariants.
                    .source = @enumFromInt(8),
                    .target = @enumFromInt(9),
                    .capture_ty = capture,
                }}),
            },
        });
        root.* = capture;
    }
    const expected = try store.typeDigest(&name_store, roots[0]);
    try std.testing.expectEqual(expected, try store.typeDigest(&name_store, roots[1]));
    // No traversal state or digest may survive a mutation of this store.
    store.set(roots[1], .{ .box = roots[1] });
    try std.testing.expect(!std.meta.eql(expected, try store.typeDigest(&name_store, roots[1])));
    try std.testing.expectEqual(expected, try store.typeDigest(&name_store, roots[0]));
}

test "Lambda Mono digest distinguishes recursive edges and callable targets" {
    var name_store = names.NameStore.init(std.testing.allocator);
    defer name_store.deinit();
    var store = Store.init(std.testing.allocator);
    defer store.deinit();
    const root = try store.add(.zst);
    const child = try store.add(.{ .box = root });
    store.set(root, .{ .tuple = try store.addSpan(&.{child}) });
    const outer_cycle = try store.typeDigest(&name_store, root);
    store.set(child, .{ .box = child });
    try std.testing.expect(!std.meta.eql(outer_cycle, try store.typeDigest(&name_store, root)));
    var digests: [2]names.TypeDigest = undefined;
    for (&digests, 0..) |*digest, i| {
        store.set(root, .{
            .callable = try store.addFnVariants(&.{.{
                .id = undefined, // Assigned by addFnVariants.
                .source = @enumFromInt(1),
                .target = @enumFromInt(i),
                .capture_ty = root,
            }}),
        });
        digest.* = try store.typeDigest(&name_store, root);
    }
    try std.testing.expect(!std.meta.eql(digests[0], digests[1]));
}

test "Lambda Mono acyclic digest ignores sharing" {
    var name_store = names.NameStore.init(std.testing.allocator);
    defer name_store.deinit();
    var store = Store.init(std.testing.allocator);
    defer store.deinit();
    const first = try store.add(.zst);
    const second = try store.add(.zst);
    const shared = try store.add(.{ .tuple = try store.addSpan(&.{ first, first }) });
    const copied = try store.add(.{ .tuple = try store.addSpan(&.{ first, second }) });
    const expected = try store.typeDigest(&name_store, shared);
    try std.testing.expectEqual(expected, try store.typeDigest(&name_store, shared));
    try std.testing.expectEqual(expected, try store.typeDigest(&name_store, copied));
}

test "Lambda Mono recursive digest propagates allocation failure" {
    const Run = struct {
        fn run(allocator: std.mem.Allocator) std.mem.Allocator.Error!void {
            var name_store = names.NameStore.init(allocator);
            defer name_store.deinit();
            var store = Store.init(allocator);
            defer store.deinit();
            const root = try store.add(.zst);
            store.set(root, .{ .box = root });
            _ = try store.typeDigest(&name_store, root);
        }
    };
    try std.testing.checkAllAllocationFailures(std.testing.allocator, Run.run, .{});
}

test "Lambda Mono digest reuses scratch capacity for shared graphs and cycles" {
    var allocations = std.testing.FailingAllocator.init(std.testing.allocator, .{});
    var name_store = names.NameStore.init(std.testing.allocator);
    defer name_store.deinit();
    var store = Store.init(allocations.allocator());
    defer store.deinit();
    const cyclic = try store.add(.zst);
    store.set(cyclic, .{ .box = cyclic });
    var root = cyclic;
    // A tree walk expands 2^4096 paths. Discovery must visit only these
    // 4097 nodes, and its depth must not consume the machine stack.
    for (0..4096) |_| root = try store.add(.{ .tuple = try store.addSpan(&.{ root, root }) });
    const expected = try store.typeDigest(&name_store, root);
    const before = allocations.allocations;
    allocations.fail_index = allocations.alloc_index;
    for (0..3) |_| {
        try std.testing.expectEqual(expected, try store.typeDigest(&name_store, root));
        _ = try store.typeDigest(&name_store, cyclic);
    }
    try std.testing.expectEqual(before, allocations.allocations);
}

test "Lambda Mono digest ignores recursive knot size and unrolling" {
    var name_store = names.NameStore.init(std.testing.allocator);
    defer name_store.deinit();
    var store = Store.init(std.testing.allocator);
    defer store.deinit();
    const single = try store.add(.zst);
    store.set(single, .{ .box = single });
    const expected = try store.typeDigest(&name_store, single);
    const first = try store.add(.zst);
    var last = first;
    for (0..350) |_| {
        const next = try store.add(.zst);
        store.set(last, .{ .box = next });
        last = next;
    }
    store.set(last, .{ .box = first });
    try std.testing.expectEqual(expected, try store.typeDigest(&name_store, first));
    const unrolled = try store.add(.{ .box = first });
    try std.testing.expectEqual(expected, try store.typeDigest(&name_store, unrolled));
}

test "Lambda Mono digest retries after every scratch allocation failure" {
    var name_store = names.NameStore.init(std.testing.allocator);
    defer name_store.deinit();
    var expected_store = Store.init(std.testing.allocator);
    defer expected_store.deinit();
    const expected_root = try expected_store.add(.zst);
    const expected_child = try expected_store.add(.{ .box = expected_root });
    expected_store.set(expected_root, .{ .tuple = try expected_store.addSpan(&.{ expected_child, expected_root }) });
    const expected = try expected_store.typeDigest(&name_store, expected_root);
    var budget: usize = 0;
    while (true) : (budget += 1) {
        var failing = std.testing.FailingAllocator.init(std.testing.allocator, .{});
        var store = Store.init(failing.allocator());
        defer store.deinit();
        const root = try store.add(.zst);
        const child = try store.add(.{ .box = root });
        store.set(root, .{ .tuple = try store.addSpan(&.{ child, root }) });
        failing.fail_index = failing.alloc_index + budget;
        const completed = if (store.typeDigest(&name_store, root)) |actual| blk: {
            try std.testing.expectEqual(expected, actual);
            break :blk true;
        } else |err| blk: {
            try std.testing.expectEqual(error.OutOfMemory, err);
            failing.fail_index = std.math.maxInt(usize);
            const retried = try store.typeDigest(&name_store, root);
            try std.testing.expectEqual(expected, retried);
            try std.testing.expectEqual(retried, try store.typeDigest(&name_store, root));
            break :blk false;
        };
        if (completed) break;
    }
}

fn writeBytes(hasher: *TypeDigestHasher, bytes: []const u8) void {
    writeU32(hasher, @intCast(bytes.len));
    hasher.update(bytes);
}

fn writeOptionalU32(hasher: *TypeDigestHasher, value: ?u32) void {
    if (value) |v| {
        hasher.update(&[_]u8{1});
        writeU32(hasher, v);
    } else {
        hasher.update(&[_]u8{0});
    }
}

fn writeOptionalDigest(hasher: *TypeDigestHasher, value: ?names.TypeDigest) void {
    if (value) |digest| {
        hasher.update(&[_]u8{1});
        hasher.update(&digest.bytes);
    } else {
        hasher.update(&[_]u8{0});
    }
}

fn writeIteratorTopology(
    hasher: *TypeDigestHasher,
    name_store: *const names.NameStore,
    topology: ?MonoType.IteratorTopology,
) void {
    const value = topology orelse {
        writeBytes(hasher, "no-iterator-topology");
        return;
    };
    writeBytes(hasher, "iterator-topology");
    writeBytes(hasher, name_store.recordFieldLabelText(value.len_field));
    writeBytes(hasher, name_store.recordFieldLabelText(value.step_field));
    writeBytes(hasher, name_store.tagLabelText(value.known_tag));
    writeBytes(hasher, name_store.tagLabelText(value.unknown_tag));
    writeBytes(hasher, name_store.tagLabelText(value.done_tag));
    writeBytes(hasher, name_store.tagLabelText(value.one_tag));
    writeBytes(hasher, name_store.tagLabelText(value.skip_tag));
    writeBytes(hasher, name_store.recordFieldLabelText(value.item_field));
    writeBytes(hasher, name_store.recordFieldLabelText(value.rest_field));
}

fn writeU32(hasher: *TypeDigestHasher, value: u32) void {
    const little = std.mem.nativeToLittle(u32, value);
    hasher.update(std.mem.asBytes(&little));
}

test "lambda mono type declarations are referenced" {
    std.testing.refAllDecls(@This());
}

test "lambda mono type content has callable shapes instead of function types" {
    try std.testing.expect(@hasField(Content, "callable"));
    try std.testing.expect(@hasField(Content, "erased_fn"));
    try std.testing.expect(!@hasField(Content, "func"));
}

test "lambda mono callable variants receive store-local ids" {
    var store = Store.init(std.testing.allocator);
    defer store.deinit();

    const capture_ty = try store.add(.zst);
    const variants = try store.addFnVariants(&.{
        .{ .id = @enumFromInt(99), .source = @enumFromInt(7), .target = @enumFromInt(70), .capture_ty = capture_ty },
        .{ .id = @enumFromInt(99), .source = @enumFromInt(8), .target = @enumFromInt(80), .capture_ty = null },
    });
    const callable = try store.add(.{ .callable = variants });

    const stored_variants = store.fnVariantSpan(store.get(callable).callable);
    try std.testing.expectEqual(@as(FnVariantId, @enumFromInt(variants.start)), GuardedList.at(stored_variants, 0).id);
    try std.testing.expectEqual(@as(FnVariantId, @enumFromInt(1)), GuardedList.at(stored_variants, 1).id);
    try std.testing.expectEqual(capture_ty, GuardedList.at(stored_variants, 0).capture_ty.?);
}

test "lambda mono empty spans use shared empty descriptor" {
    var store = Store.init(std.testing.allocator);
    defer store.deinit();

    const unit = try store.add(.zst);
    const nonempty_span = try store.addSpan(&.{unit});
    const nonempty_fields = try store.addFields(&.{.{ .name = @enumFromInt(1), .ty = unit, .default = null }});
    const nonempty_capture_fields = try store.addCaptureFields(&.{.{ .symbol = @enumFromInt(2), .binder = null, .ty = unit, .storage_ty = unit }});
    const nonempty_tags = try store.addTags(&.{.{ .name = @enumFromInt(3), .checked_name = @enumFromInt(3), .payloads = nonempty_span }});
    const nonempty_variants = try store.addFnVariants(&.{.{ .id = @enumFromInt(99), .source = @enumFromInt(4), .target = @enumFromInt(40), .capture_ty = unit }});
    try std.testing.expect(nonempty_span.len == 1);
    try std.testing.expect(nonempty_fields.len == 1);
    try std.testing.expect(nonempty_capture_fields.len == 1);
    try std.testing.expect(nonempty_tags.len == 1);
    try std.testing.expect(nonempty_variants.len == 1);

    try std.testing.expectEqual(Span.empty(), try store.addSpan(&.{}));
    try std.testing.expectEqual(Span.empty(), try store.addFields(&.{}));
    try std.testing.expectEqual(Span.empty(), try store.addCaptureFields(&.{}));
    try std.testing.expectEqual(Span.empty(), try store.addTags(&.{}));
    try std.testing.expectEqual(Span.empty(), try store.addFnVariants(&.{}));
}
