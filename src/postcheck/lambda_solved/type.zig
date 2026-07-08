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
        return @intFromEnum(self) > @intFromEnum(other);
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

/// Record field type entry.
pub const Field = struct {
    name: names.RecordFieldNameId,
    ty: TypeVarId,
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
    ty: TypeVarId,
};

/// Function member of a lambda set.
pub const FnMember = struct {
    lambda: Common.Symbol,
    captures: Span,
};

/// One entry of a nominal record's declared layout order; consumed only by
/// layout selection (the backing row stays lexicographic).
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
        } = null,
        /// Declared field order for a nominal/opaque record backing; empty
        /// otherwise.
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
    },
    zst,
};

/// Store for Lambda Solved type variables and their shared spans.
pub const Store = struct {
    allocator: std.mem.Allocator,
    vars: std.ArrayList(Content),
    spans: std.ArrayList(TypeVarId),
    fields: std.ArrayList(Field),
    tags: std.ArrayList(Tag),
    captures: std.ArrayList(Capture),
    fn_members: std.ArrayList(FnMember),
    declared_fields: std.ArrayList(DeclaredField),

    pub fn init(allocator: std.mem.Allocator) Store {
        return .{
            .allocator = allocator,
            .vars = .empty,
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
        self.vars.deinit(self.allocator);
    }

    pub fn add(self: *Store, content: Content) std.mem.Allocator.Error!TypeVarId {
        const id: TypeVarId = @enumFromInt(@as(u32, @intCast(self.vars.items.len)));
        try self.vars.append(self.allocator, content);
        return id;
    }

    pub fn set(self: *Store, id: TypeVarId, content: Content) void {
        self.vars.items[@intFromEnum(id)] = content;
    }

    pub fn get(self: *const Store, id: TypeVarId) Content {
        return self.vars.items[@intFromEnum(id)];
    }

    pub fn root(self: *const Store, id: TypeVarId) TypeVarId {
        var current = id;
        while (true) {
            switch (self.get(current)) {
                .link => |next| current = next,
                else => return current,
            }
        }
    }

    pub fn rootContent(self: *const Store, id: TypeVarId) Content {
        return self.get(self.root(id));
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

    pub fn span(self: *const Store, span_: Span) []const TypeVarId {
        return self.spans.items[span_.start..][0..span_.len];
    }

    pub fn spanItem(self: *const Store, span_: Span, index: usize) TypeVarId {
        if (index >= span_.count()) Common.invariant("Lambda Solved type span index out of bounds");
        return self.spans.items[@as(usize, span_.start) + index];
    }

    pub fn fieldSpan(self: *const Store, span_: Span) []const Field {
        return self.fields.items[span_.start..][0..span_.len];
    }

    pub fn addDeclaredFields(self: *Store, values: []const DeclaredField) std.mem.Allocator.Error!Span {
        if (values.len == 0) return .empty();
        const start: u32 = @intCast(self.declared_fields.items.len);
        try self.declared_fields.appendSlice(self.allocator, values);
        return .{ .start = start, .len = @intCast(values.len) };
    }

    pub fn declaredFieldSpan(self: *const Store, span_: Span) []const DeclaredField {
        return self.declared_fields.items[span_.start..][0..span_.len];
    }

    pub fn fieldItem(self: *const Store, span_: Span, index: usize) Field {
        if (index >= span_.count()) Common.invariant("Lambda Solved field span index out of bounds");
        return self.fields.items[@as(usize, span_.start) + index];
    }

    pub fn tagSpan(self: *const Store, span_: Span) []const Tag {
        return self.tags.items[span_.start..][0..span_.len];
    }

    pub fn tagItem(self: *const Store, span_: Span, index: usize) Tag {
        if (index >= span_.count()) Common.invariant("Lambda Solved tag span index out of bounds");
        return self.tags.items[@as(usize, span_.start) + index];
    }

    pub fn captureSpan(self: *const Store, span_: Span) []const Capture {
        return self.captures.items[span_.start..][0..span_.len];
    }

    pub fn captureItem(self: *const Store, span_: Span, index: usize) Capture {
        if (index >= span_.count()) Common.invariant("Lambda Solved capture span index out of bounds");
        return self.captures.items[@as(usize, span_.start) + index];
    }

    pub fn memberSpan(self: *const Store, span_: Span) []const FnMember {
        return self.fn_members.items[span_.start..][0..span_.len];
    }

    pub fn memberItem(self: *const Store, span_: Span, index: usize) FnMember {
        if (index >= span_.count()) Common.invariant("Lambda Solved member span index out of bounds");
        return self.fn_members.items[@as(usize, span_.start) + index];
    }

    pub const View = struct {
        vars: []const Content,
        spans: []const TypeVarId,
        fields: []const Field,
        tags: []const Tag,
        captures: []const Capture,
        fn_members: []const FnMember,
        declared_fields: []const DeclaredField,

        pub fn get(self: View, id: TypeVarId) Content {
            return self.vars[@intFromEnum(id)];
        }

        pub fn root(self: View, id: TypeVarId) TypeVarId {
            var current = id;
            while (true) {
                switch (self.get(current)) {
                    .link => |next| current = next,
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
    try std.testing.expectEqual(arg, store.span(function.args)[0]);
}

test "lambda solved type variable order uses the typed id helper" {
    try std.testing.expect(@as(TypeVarId, @enumFromInt(2)).is_gt(@enumFromInt(1)));
    try std.testing.expect(!@as(TypeVarId, @enumFromInt(1)).is_gt(@enumFromInt(2)));
}

test "lambda solved empty spans use shared empty descriptor" {
    var store = Store.init(std.testing.allocator);
    defer store.deinit();

    const unit = try store.add(.zst);
    const nonempty_span = try store.addSpan(&.{unit});
    const nonempty_fields = try store.addFields(&.{.{ .name = @enumFromInt(1), .ty = unit }});
    const nonempty_tags = try store.addTags(&.{.{ .name = @enumFromInt(2), .checked_name = @enumFromInt(2), .payloads = nonempty_span }});
    const nonempty_captures = try store.addCaptures(&.{.{ .local = @enumFromInt(3), .symbol = @enumFromInt(4), .binder = null, .ty = unit }});
    const nonempty_members = try store.addMembers(&.{.{ .lambda = @enumFromInt(5), .captures = nonempty_captures }});
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
