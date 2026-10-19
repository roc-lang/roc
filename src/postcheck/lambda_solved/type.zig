//! Lambda Solved type graph.

const std = @import("std");
const check = @import("check");

const Common = @import("../common.zig");
const MonoType = @import("../monotype/type.zig");

/// Checked boundary name module used by Lambda Solved types.
pub const names = check.CheckedNames;
const static_dispatch = check.StaticDispatchRegistry;

/// Identifier for a type variable in the Lambda Solved store.
pub const TypeVarId = enum(u32) {
    _,

    pub fn is_gt(self: TypeVarId, other: TypeVarId) bool {
        return @backingInt(self) > @backingInt(other);
    }
};

/// A `TypeVarId` or none, at the width of one id. `Store.add` refuses the
/// sentinel id, so `none` never names a variable.
pub const MaybeTypeVarId = enum(u32) {
    none = std.math.maxInt(u32),
    _,

    pub fn of(ty: ?TypeVarId) MaybeTypeVarId {
        const present = ty orelse return .none;
        return @fromBackingInt(@backingInt(present));
    }

    pub fn get(self: MaybeTypeVarId) ?TypeVarId {
        if (self == .none) return null;
        return @fromBackingInt(@backingInt(self));
    }
};

/// Slice descriptor for type, field, tag, capture, or member arrays.
pub const Span = extern struct {
    start: u32,
    len: u32,

    pub fn empty() Span {
        return .{ .start = 0, .len = 0 };
    }

    pub fn count(self: Span) usize {
        return @as(usize, self.len);
    }
};

/// Record field type entry. `default` is the Monotype `??` default identity
/// carried verbatim through lambda solving (shared program name store).
pub const Field = struct {
    name: names.RecordFieldNameId,
    ty: TypeVarId,
    value_ty: ?TypeVarId = null,
    default: ?MonoType.FieldDefault,
};

/// Tag-union variant type entry.
pub const Tag = struct {
    name: names.TagNameId,
    checked_name: names.TagNameId,
    payloads: Span,
};

/// Captured local recorded for a lambda-set member.
pub const Capture = struct {
    local: @import("../monotype_lifted/ast.zig").LocalId,
    symbol: Common.Symbol,
    binder: ?check.CheckedModule.PatternBinderId,
    capture_id: ?check.CheckedModule.CaptureId = null,
    checked_capture_id: ?check.CheckedModule.CaptureId = null,
    capture_kind: check.CheckedModule.ConstCaptureKind = .lexical,
    ty: TypeVarId,
};

/// Function member of a lambda set.
pub const FnMember = struct {
    lambda: Common.Symbol,
    captures: Span,
};

/// One entry of a nominal record's declared fields; consumed by layout and
/// descriptor planning (the backing row stays lexicographic).
pub const DeclaredField = union(enum) {
    named: names.RecordFieldNameId,
    padding: TypeVarId,
};

/// Lambda Solved type-variable content.
pub const Content = union(enum) {
    link: TypeVarId,
    unbound,
    forall,
    primitive: MonoType.Primitive,
    named: struct {
        named_type: MonoType.NamedType,
        def: MonoType.TypeDef,
        kind: MonoType.NamedKind,
        builtin_owner: ?static_dispatch.BuiltinOwner = null,
        args: Span,
        backing: ?struct {
            ty: TypeVarId,
            use: MonoType.BackingUse,
            authority: MonoType.BackingAuthority = .checked_public,
        } = null,
        /// Declared fields for a nominal/opaque record backing; empty otherwise.
        declared_order: Span = Span.empty(),
    },
    record: Span,
    tuple: Span,
    tag_union: Span,
    list: TypeVarId,
    box: TypeVarId,
    func: struct {
        args: Span,
        callable: TypeVarId,
        ret: TypeVarId,
    },
    lambda_set: Span,
    erased: struct {
        source_fn_ty: names.TypeDigest,
        members: Span = .empty(),
        /// The function type this callable was erased from. Its argument and
        /// result types are the erased ABI: every member entry is specialized
        /// at it, and every erased call converts to and from it. Null for an
        /// erased requirement Monotype imported as a bare digest.
        abi_fn: ?TypeVarId = null,
    },
    zst,
    /// Lazy leaf: this var's type is the referenced lifted Monotype, not yet
    /// materialized in this store. The solver expands a leaf one level the
    /// first time unification or a shape read touches it, and end-of-solve
    /// finalization replaces every surviving leaf with a link to a
    /// materialized clone, so program views never observe one. Each use still
    /// gets its own leaf var: unification rewrites var contents in place, so
    /// clones that can reach `unify` must own their vars.
    mono: struct {
        id: MonoType.TypeId,
        /// Clone context tying recursive back-references within one lazily
        /// materialized tree to their existing vars, exactly as an eager
        /// clone's memo map did. `no_leaf_context` until first expansion.
        ctx: u32 = no_leaf_context,
    },
};

/// Payload stored by `Content.named`.
pub const NamedContent = std.meta.fieldInfo(Content, .named).type;

/// Payload stored by `Content.erased`.
pub const ErasedContent = std.meta.fieldInfo(Content, .erased).type;

/// A type variable as the store keeps it. A named payload and an erased
/// payload are far larger than every other variant, so they live in side
/// lists and the node holds their index; every node is then a few words,
/// and a link walk reads only these.
pub const StoredNode = union(enum(u8)) {
    link: TypeVarId,
    unbound,
    forall,
    primitive: MonoType.Primitive,
    named: u32,
    record: Span,
    tuple: Span,
    tag_union: Span,
    list: TypeVarId,
    box: TypeVarId,
    func: @FieldType(Content, "func"),
    lambda_set: Span,
    erased: u32,
    zst,
    mono: @FieldType(Content, "mono"),
};

/// The content `node` stores, reading its payload from the side lists.
fn contentOf(node: StoredNode, named_payloads: []const NamedContent, erased_payloads: []const ErasedContent) Content {
    return switch (node) {
        .link => |target| .{ .link = target },
        .unbound => .unbound,
        .forall => .forall,
        .primitive => |primitive| .{ .primitive = primitive },
        .named => |index| .{ .named = named_payloads[index] },
        .record => |span_| .{ .record = span_ },
        .tuple => |span_| .{ .tuple = span_ },
        .tag_union => |span_| .{ .tag_union = span_ },
        .list => |elem| .{ .list = elem },
        .box => |elem| .{ .box = elem },
        .func => |func| .{ .func = func },
        .lambda_set => |span_| .{ .lambda_set = span_ },
        .erased => |index| .{ .erased = erased_payloads[index] },
        .zst => .zst,
        .mono => |leaf| .{ .mono = leaf },
    };
}

/// Sentinel for a lazy leaf that has not been reached by any expansion yet.
pub const no_leaf_context: u32 = std.math.maxInt(u32);

test "lambda solved named backing preserves generated-private authority" {
    const content = Content{ .named = .{
        .named_type = undefined,
        .def = undefined,
        .kind = .@"opaque",
        .args = Span.empty(),
        .backing = .{
            .ty = @fromBackingInt(@intCast(1)),
            .use = .runtime_layout_only,
            .authority = .generated_private,
        },
    } };
    try std.testing.expectEqual(
        MonoType.BackingAuthority.generated_private,
        content.named.backing.?.authority,
    );
}

/// Store for Lambda Solved type variables and their shared spans.
pub const Store = struct {
    allocator: std.mem.Allocator,
    vars: std.ArrayList(StoredNode),
    /// Payloads of the named nodes in `vars`, by `StoredNode.named` index.
    named_payloads: std.ArrayList(NamedContent),
    /// Payloads of the erased nodes in `vars`, by `StoredNode.erased` index.
    erased_payloads: std.ArrayList(ErasedContent),
    owned_named_backings: std.ArrayList(bool),
    spans: std.ArrayList(TypeVarId),
    fields: std.ArrayList(Field),
    tags: std.ArrayList(Tag),
    captures: std.ArrayList(Capture),
    fn_members: std.ArrayList(FnMember),
    declared_fields: std.ArrayList(DeclaredField),
    /// Advances on every write that can change a variable's meaning, so a
    /// result derived from the store can tell whether it is still current.
    mutation_epoch: u64 = 0,

    pub fn init(allocator: std.mem.Allocator) Store {
        return .{
            .allocator = allocator,
            .vars = .empty,
            .named_payloads = .empty,
            .erased_payloads = .empty,
            .owned_named_backings = .empty,
            .spans = .empty,
            .fields = .empty,
            .tags = .empty,
            .captures = .empty,
            .fn_members = .empty,
            .declared_fields = .empty,
        };
    }

    pub fn deinit(self: *Store) void {
        self.declared_fields.deinit(self.allocator);
        self.fn_members.deinit(self.allocator);
        self.captures.deinit(self.allocator);
        self.tags.deinit(self.allocator);
        self.fields.deinit(self.allocator);
        self.spans.deinit(self.allocator);
        self.owned_named_backings.deinit(self.allocator);
        self.erased_payloads.deinit(self.allocator);
        self.named_payloads.deinit(self.allocator);
        self.vars.deinit(self.allocator);
    }

    /// The node storing `content`, appending a named or erased payload to its
    /// side list. `previous` is the node being replaced, whose payload slot a
    /// payload of the same kind takes over.
    fn storeNode(self: *Store, content: Content, previous: ?StoredNode) std.mem.Allocator.Error!StoredNode {
        return switch (content) {
            .link => |target| .{ .link = target },
            .unbound => .unbound,
            .forall => .forall,
            .primitive => |primitive| .{ .primitive = primitive },
            .named => |named| blk: {
                if (previous) |node| if (node == .named) {
                    self.named_payloads.items[node.named] = named;
                    break :blk node;
                };
                const index = std.math.cast(u32, self.named_payloads.items.len) orelse return error.OutOfMemory;
                try self.named_payloads.append(self.allocator, named);
                break :blk .{ .named = index };
            },
            .record => |span_| .{ .record = span_ },
            .tuple => |span_| .{ .tuple = span_ },
            .tag_union => |span_| .{ .tag_union = span_ },
            .list => |elem| .{ .list = elem },
            .box => |elem| .{ .box = elem },
            .func => |func| .{ .func = func },
            .lambda_set => |span_| .{ .lambda_set = span_ },
            .erased => |erased| blk: {
                if (previous) |node| if (node == .erased) {
                    self.erased_payloads.items[node.erased] = erased;
                    break :blk node;
                };
                const index = std.math.cast(u32, self.erased_payloads.items.len) orelse return error.OutOfMemory;
                try self.erased_payloads.append(self.allocator, erased);
                break :blk .{ .erased = index };
            },
            .zst => .zst,
            .mono => |leaf| .{ .mono = leaf },
        };
    }

    pub fn add(self: *Store, content: Content) std.mem.Allocator.Error!TypeVarId {
        if (self.vars.items.len >= @backingInt(MaybeTypeVarId.none)) return error.OutOfMemory;
        const id: TypeVarId = @fromBackingInt(@as(u32, @intCast(self.vars.items.len)));
        try self.vars.ensureUnusedCapacity(self.allocator, 1);
        try self.owned_named_backings.ensureUnusedCapacity(self.allocator, 1);
        self.vars.appendAssumeCapacity(try self.storeNode(content, null));
        self.owned_named_backings.appendAssumeCapacity(false);
        return id;
    }

    pub fn set(self: *Store, id: TypeVarId, content: Content) std.mem.Allocator.Error!void {
        if (content == .link) return self.setLink(id, content.link);
        const node = try self.storeNode(content, self.vars.items[@backingInt(id)]);
        self.mutation_epoch +%= 1;
        self.vars.items[@backingInt(id)] = node;
    }

    /// Link `id` to `target`, carrying an owned named backing to its root.
    pub fn setLink(self: *Store, id: TypeVarId, target: TypeVarId) void {
        self.mutation_epoch +%= 1;
        if (self.owned_named_backings.items[@backingInt(id)]) {
            const target_root = self.root(target);
            self.owned_named_backings.items[@backingInt(target_root)] = true;
        }
        self.vars.items[@backingInt(id)] = .{ .link = target };
    }

    pub fn markNamedBacking(self: *Store, id: TypeVarId) void {
        const root_id = self.root(id);
        self.owned_named_backings.items[@backingInt(root_id)] = true;
    }

    pub fn isOwnedNamedBacking(self: *const Store, id: TypeVarId) bool {
        const root_id = self.root(id);
        return self.owned_named_backings.items[@backingInt(root_id)];
    }

    pub fn get(self: *const Store, id: TypeVarId) Content {
        return contentOf(self.vars.items[@backingInt(id)], self.named_payloads.items, self.erased_payloads.items);
    }

    pub fn root(self: *const Store, id: TypeVarId) TypeVarId {
        var current = id;
        while (true) {
            switch (self.vars.items[@backingInt(current)]) {
                .link => |target| current = target,
                else => return current,
            }
        }
    }

    pub fn rootContent(self: *const Store, id: TypeVarId) Content {
        return self.get(self.root(id));
    }

    pub fn rootCompressed(self: *Store, id: TypeVarId) TypeVarId {
        const root_id = self.root(id);
        // Path compression preserves every variable's meaning, so it is not a
        // mutation observers must see.
        const epoch = self.mutation_epoch;
        defer self.mutation_epoch = epoch;
        var current = id;
        while (current != root_id) {
            const next = switch (self.vars.items[@backingInt(current)]) {
                .link => |target| target,
                else => break,
            };
            self.setLink(current, root_id);
            current = next;
        }

        return root_id;
    }

    pub fn rootContentCompressed(self: *Store, id: TypeVarId) Content {
        return self.get(self.rootCompressed(id));
    }

    pub fn compressAllRoots(self: *Store) void {
        for (0..self.vars.items.len) |index| {
            _ = self.rootCompressed(@fromBackingInt(@intCast(@as(u32, @intCast(index)))));
        }
    }

    pub fn addSpan(self: *Store, values: []const TypeVarId) std.mem.Allocator.Error!Span {
        if (values.len == 0) return .empty();
        const start: u32 = @intCast(self.spans.items.len);
        try self.spans.appendSlice(self.allocator, values);
        return .{ .start = start, .len = @intCast(values.len) };
    }

    /// A span of `len` new vars at the end of the span pool, returned with
    /// its slots for the caller to fill before the store's next append.
    pub fn addSpanSlots(self: *Store, len: usize) std.mem.Allocator.Error!SpanSlots(TypeVarId) {
        return try appendSlots(TypeVarId, self.allocator, &self.spans, len);
    }

    /// A span of `len` new captures at the end of the capture pool, returned
    /// with its slots for the caller to fill before the store's next append.
    pub fn addCaptureSlots(self: *Store, len: usize) std.mem.Allocator.Error!SpanSlots(Capture) {
        return try appendSlots(Capture, self.allocator, &self.captures, len);
    }

    pub fn SpanSlots(comptime T: type) type {
        return struct { span: Span, slots: []T };
    }

    fn appendSlots(comptime T: type, allocator: std.mem.Allocator, list: *std.ArrayList(T), len: usize) std.mem.Allocator.Error!SpanSlots(T) {
        if (len == 0) return .{ .span = .empty(), .slots = &.{} };
        const start = std.math.cast(u32, list.items.len) orelse return error.OutOfMemory;
        const count = std.math.cast(u32, len) orelse return error.OutOfMemory;
        if (std.math.maxInt(u32) - start < count) return error.OutOfMemory;
        const slots = try list.addManyAsSlice(allocator, len);
        return .{ .span = .{ .start = start, .len = count }, .slots = slots };
    }

    pub fn addFields(self: *Store, values: []const Field) std.mem.Allocator.Error!Span {
        if (values.len == 0) return .empty();
        const start: u32 = @intCast(self.fields.items.len);
        try self.fields.appendSlice(self.allocator, values);
        return .{ .start = start, .len = @intCast(values.len) };
    }

    pub fn addTags(self: *Store, values: []const Tag) std.mem.Allocator.Error!Span {
        if (values.len == 0) return .empty();
        const start: u32 = @intCast(self.tags.items.len);
        try self.tags.appendSlice(self.allocator, values);
        return .{ .start = start, .len = @intCast(values.len) };
    }

    pub fn addCaptures(self: *Store, values: []const Capture) std.mem.Allocator.Error!Span {
        if (values.len == 0) return .empty();
        const start: u32 = @intCast(self.captures.items.len);
        try self.captures.appendSlice(self.allocator, values);
        return .{ .start = start, .len = @intCast(values.len) };
    }

    pub fn addMembers(self: *Store, values: []const FnMember) std.mem.Allocator.Error!Span {
        if (values.len == 0) return .empty();
        const start: u32 = @intCast(self.fn_members.items.len);
        try self.fn_members.appendSlice(self.allocator, values);
        return .{ .start = start, .len = @intCast(values.len) };
    }

    pub fn spanItem(self: *const Store, span_: Span, index: usize) TypeVarId {
        if (index >= span_.count()) Common.invariant("Lambda Solved type span index out of bounds");
        return self.spans.items[@as(usize, span_.start) + index];
    }

    pub fn addDeclaredFields(self: *Store, values: []const DeclaredField) std.mem.Allocator.Error!Span {
        if (values.len == 0) return .empty();
        const start: u32 = @intCast(self.declared_fields.items.len);
        try self.declared_fields.appendSlice(self.allocator, values);
        return .{ .start = start, .len = @intCast(values.len) };
    }

    pub fn fieldItem(self: *const Store, span_: Span, index: usize) Field {
        if (index >= span_.count()) Common.invariant("Lambda Solved field span index out of bounds");
        return self.fields.items[@as(usize, span_.start) + index];
    }

    pub fn declaredFieldItem(self: *const Store, span_: Span, index: usize) DeclaredField {
        if (index >= span_.count()) Common.invariant("Lambda Solved declared-field span index out of bounds");
        return self.declared_fields.items[@as(usize, span_.start) + index];
    }

    pub fn tagItem(self: *const Store, span_: Span, index: usize) Tag {
        if (index >= span_.count()) Common.invariant("Lambda Solved tag span index out of bounds");
        return self.tags.items[@as(usize, span_.start) + index];
    }

    pub fn captureItem(self: *const Store, span_: Span, index: usize) Capture {
        if (index >= span_.count()) Common.invariant("Lambda Solved capture span index out of bounds");
        return self.captures.items[@as(usize, span_.start) + index];
    }

    pub fn memberItem(self: *const Store, span_: Span, index: usize) FnMember {
        if (index >= span_.count()) Common.invariant("Lambda Solved member span index out of bounds");
        return self.fn_members.items[@as(usize, span_.start) + index];
    }

    pub const View = struct {
        vars: []const StoredNode,
        named_payloads: []const NamedContent,
        erased_payloads: []const ErasedContent,
        spans: []const TypeVarId,
        fields: []const Field,
        tags: []const Tag,
        captures: []const Capture,
        fn_members: []const FnMember,
        declared_fields: []const DeclaredField,

        pub fn get(self: View, id: TypeVarId) Content {
            return contentOf(self.vars[@backingInt(id)], self.named_payloads, self.erased_payloads);
        }

        pub fn root(self: View, id: TypeVarId) TypeVarId {
            var current = id;
            while (true) {
                switch (self.vars[@backingInt(current)]) {
                    .link => |target| current = target,
                    else => return current,
                }
            }
        }

        pub fn rootContent(self: View, id: TypeVarId) Content {
            return self.get(self.root(id));
        }

        pub fn span(self: View, span_: Span) []const TypeVarId {
            return self.spans[span_.start..][0..span_.len];
        }

        pub fn fieldSpan(self: View, span_: Span) []const Field {
            return self.fields[span_.start..][0..span_.len];
        }

        pub fn tagSpan(self: View, span_: Span) []const Tag {
            return self.tags[span_.start..][0..span_.len];
        }

        pub fn captureSpan(self: View, span_: Span) []const Capture {
            return self.captures[span_.start..][0..span_.len];
        }

        pub fn memberSpan(self: View, span_: Span) []const FnMember {
            return self.fn_members[span_.start..][0..span_.len];
        }

        pub fn declaredFieldSpan(self: View, span_: Span) []const DeclaredField {
            return self.declared_fields[span_.start..][0..span_.len];
        }
    };

    pub fn view(self: *const Store) View {
        return .{
            .vars = self.vars.items,
            .named_payloads = self.named_payloads.items,
            .erased_payloads = self.erased_payloads.items,
            .spans = self.spans.items,
            .fields = self.fields.items,
            .tags = self.tags.items,
            .captures = self.captures.items,
            .fn_members = self.fn_members.items,
            .declared_fields = self.declared_fields.items,
        };
    }
};

test "lambda solved type declarations are referenced" {
    std.testing.refAllDecls(@This());
}

test "lambda solved function types carry callable variables" {
    var store = Store.init(std.testing.allocator);
    defer store.deinit();

    const arg = try store.add(.unbound);
    const ret = try store.add(.unbound);
    const callable = try store.add(.unbound);
    const args = try store.addSpan(&.{arg});
    const fn_ty = try store.add(.{ .func = .{
        .args = args,
        .callable = callable,
        .ret = ret,
    } });

    const function = store.get(fn_ty).func;
    try std.testing.expectEqual(callable, function.callable);
    try std.testing.expectEqual(ret, function.ret);
    try std.testing.expectEqual(arg, store.spanItem(function.args, 0));
}

test "lambda solved type variable order uses the typed id helper" {
    try std.testing.expect(@as(TypeVarId, @fromBackingInt(@intCast(2))).is_gt(@fromBackingInt(@intCast(1))));
    try std.testing.expect(!@as(TypeVarId, @fromBackingInt(@intCast(1))).is_gt(@fromBackingInt(@intCast(2))));
}

test "lambda solved empty spans use shared empty descriptor" {
    var store = Store.init(std.testing.allocator);
    defer store.deinit();

    const unit = try store.add(.zst);
    const nonempty_span = try store.addSpan(&.{unit});
    const nonempty_fields = try store.addFields(&.{.{ .name = @fromBackingInt(@intCast(1)), .ty = unit, .default = null }});
    const nonempty_tags = try store.addTags(&.{.{ .name = @fromBackingInt(@intCast(2)), .checked_name = @fromBackingInt(@intCast(2)), .payloads = nonempty_span }});
    const nonempty_captures = try store.addCaptures(&.{.{ .local = @fromBackingInt(@intCast(3)), .symbol = @fromBackingInt(@intCast(4)), .binder = null, .ty = unit }});
    const nonempty_members = try store.addMembers(&.{.{ .lambda = @fromBackingInt(@intCast(5)), .captures = nonempty_captures }});
    try std.testing.expect(nonempty_span.len == 1);
    try std.testing.expect(nonempty_fields.len == 1);
    try std.testing.expect(nonempty_tags.len == 1);
    try std.testing.expect(nonempty_captures.len == 1);
    try std.testing.expect(nonempty_members.len == 1);

    try std.testing.expectEqual(Span.empty(), try store.addSpan(&.{}));
    try std.testing.expectEqual(Span.empty(), try store.addFields(&.{}));
    try std.testing.expectEqual(Span.empty(), try store.addTags(&.{}));
    try std.testing.expectEqual(Span.empty(), try store.addCaptures(&.{}));
    try std.testing.expectEqual(Span.empty(), try store.addMembers(&.{}));
}
