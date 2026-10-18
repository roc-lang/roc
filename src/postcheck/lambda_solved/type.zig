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

/// A variable's content as the store keeps it: `Content` with its two large
/// payloads, `named` and `erased`, held out of line by index. Solving leaves
/// nearly every variable a link, and the Solved program stays alive while
/// compile-time evaluation lowers each level from it, so a variable takes a
/// few words rather than the size of the largest payload.
const Slot = union(enum) {
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

const NamedPayload = @FieldType(Content, "named");
const ErasedPayload = @FieldType(Content, "erased");

fn slotContent(slots: []const Slot, named: []const NamedPayload, erased: []const ErasedPayload, id: TypeVarId) Content {
    return switch (slots[@backingInt(id)]) {
        .named => |index| .{ .named = named[index] },
        .erased => |index| .{ .erased = erased[index] },
        inline .link, .unbound, .forall, .primitive, .record, .tuple, .tag_union, .list, .box, .func, .lambda_set, .zst, .mono => |payload, tag| @unionInit(Content, @tagName(tag), payload),
    };
}

/// Store for Lambda Solved type variables and their shared spans.
pub const Store = struct {
    allocator: std.mem.Allocator,
    vars: std.ArrayList(Slot),
    /// Out-of-line payloads of `named` and `erased` variables. Rewriting a
    /// variable that already holds one reuses its entry.
    named_payloads: std.ArrayList(NamedPayload),
    erased_payloads: std.ArrayList(ErasedPayload),
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

    /// The number of variables.
    pub fn varCount(self: *const Store) usize {
        return self.vars.items.len;
    }

    pub fn add(self: *Store, content: Content) std.mem.Allocator.Error!TypeVarId {
        const id: TypeVarId = @fromBackingInt(@intCast(@as(u32, @intCast(self.vars.items.len))));
        try self.vars.ensureUnusedCapacity(self.allocator, 1);
        try self.owned_named_backings.ensureUnusedCapacity(self.allocator, 1);
        const slot = try self.slotFor(null, content);
        self.vars.appendAssumeCapacity(slot);
        self.owned_named_backings.appendAssumeCapacity(false);
        return id;
    }

    pub fn set(self: *Store, id: TypeVarId, content: Content) std.mem.Allocator.Error!void {
        const slot = try self.slotFor(self.vars.items[@backingInt(id)], content);
        self.mutation_epoch +%= 1;
        if (content == .link and self.owned_named_backings.items[@backingInt(id)]) {
            const target = self.root(content.link);
            self.owned_named_backings.items[@backingInt(target)] = true;
        }
        self.vars.items[@backingInt(id)] = slot;
    }

    /// The slot holding `content`, in place of `previous` when the variable
    /// already has one: a payload goes into the entry `previous` holds, if it
    /// holds one of the same kind.
    fn slotFor(self: *Store, previous: ?Slot, content: Content) std.mem.Allocator.Error!Slot {
        return switch (content) {
            .named => |payload| .{ .named = try storePayload(NamedPayload, self.allocator, &self.named_payloads, if (previous) |slot| if (slot == .named) slot.named else null else null, payload) },
            .erased => |payload| .{ .erased = try storePayload(ErasedPayload, self.allocator, &self.erased_payloads, if (previous) |slot| if (slot == .erased) slot.erased else null else null, payload) },
            inline .link, .unbound, .forall, .primitive, .record, .tuple, .tag_union, .list, .box, .func, .lambda_set, .zst, .mono => |payload, tag| @unionInit(Slot, @tagName(tag), payload),
        };
    }

    fn storePayload(comptime T: type, allocator: std.mem.Allocator, payloads: *std.ArrayList(T), reuse: ?u32, payload: T) std.mem.Allocator.Error!u32 {
        if (reuse) |index| {
            payloads.items[index] = payload;
            return index;
        }
        const index: u32 = @intCast(payloads.items.len);
        try payloads.append(allocator, payload);
        return index;
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
        return slotContent(self.vars.items, self.named_payloads.items, self.erased_payloads.items, id);
    }

    fn isLink(self: *const Store, id: TypeVarId) ?TypeVarId {
        return switch (self.vars.items[@backingInt(id)]) {
            .link => |target| target,
            .unbound, .forall, .primitive, .named, .record, .tuple, .tag_union, .list, .box, .func, .lambda_set, .erased, .zst, .mono => null,
        };
    }

    pub fn root(self: *const Store, id: TypeVarId) TypeVarId {
        var current = id;
        while (self.isLink(current)) |target| current = target;
        return current;
    }

    pub fn rootContent(self: *const Store, id: TypeVarId) Content {
        return self.get(self.root(id));
    }

    pub fn rootCompressed(self: *Store, id: TypeVarId) TypeVarId {
        const root_id = self.root(id);
        // Path compression preserves every variable's meaning, so it is not a
        // mutation observers must see. A link's slot holds no payload, so
        // rewriting one never allocates.
        var current = id;
        while (current != root_id) {
            const next = self.isLink(current) orelse break;
            if (self.owned_named_backings.items[@backingInt(current)]) {
                self.owned_named_backings.items[@backingInt(root_id)] = true;
            }
            self.vars.items[@backingInt(current)] = .{ .link = root_id };
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
        vars: []const Slot,
        named_payloads: []const NamedPayload,
        erased_payloads: []const ErasedPayload,
        spans: []const TypeVarId,
        fields: []const Field,
        tags: []const Tag,
        captures: []const Capture,
        fn_members: []const FnMember,
        declared_fields: []const DeclaredField,

        pub fn get(self: View, id: TypeVarId) Content {
            return slotContent(self.vars, self.named_payloads, self.erased_payloads, id);
        }

        /// The number of variables.
        pub fn varCount(self: View) usize {
            return self.vars.len;
        }

        pub fn root(self: View, id: TypeVarId) TypeVarId {
            var current = id;
            while (true) switch (self.vars[@backingInt(current)]) {
                .link => |target| current = target,
                .unbound, .forall, .primitive, .named, .record, .tuple, .tag_union, .list, .box, .func, .lambda_set, .erased, .zst, .mono => return current,
            };
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
