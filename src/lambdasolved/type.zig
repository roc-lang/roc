//! Lambda-set solving types.

const std = @import("std");
const base = @import("base");
const symbol_mod = @import("symbol");
const lifted_type = @import("monotype_lifted").Type;

pub const Symbol = symbol_mod.Symbol;
pub const Prim = lifted_type.Prim;
/// Public struct `Nominal`.
pub const Nominal = struct {
    module_idx: u32,
    ident: base.Ident.Idx,
    is_opaque: bool,
    args: Span(TypeVarId),
    backing: TypeVarId,
};

/// Public enum `TypeVarId`.
pub const TypeVarId = enum(u32) { _ };

/// Public function `Span`.
pub fn Span(comptime _: type) type {
    return extern struct {
        start: u32,
        len: u32,

        pub fn empty() @This() {
            return .{ .start = 0, .len = 0 };
        }

        pub fn isEmpty(self: @This()) bool {
            return self.len == 0;
        }
    };
}

/// Public struct `Capture`.
pub const Capture = struct {
    symbol: Symbol,
    ty: TypeVarId,
};

/// Public struct `Lambda`.
pub const Lambda = struct {
    symbol: Symbol,
    captures: Span(Capture),
};

/// Public struct `FnShape`.
pub const FnShape = struct {
    arg: TypeVarId,
    lset: TypeVarId,
    ret: TypeVarId,
};

/// Public union `LambdaRepr`.
pub const LambdaRepr = union(enum) {
    lset: []const Lambda,
    erased,
};

/// Public struct `LambdaMember`.
pub const LambdaMember = struct {
    lambda: Lambda,
    captures: []const Capture,
};

/// Public struct `Tag`.
pub const Tag = struct {
    name: base.Ident.Idx,
    args: Span(TypeVarId),
};

/// Public struct `Field`.
pub const Field = struct {
    name: base.Ident.Idx,
    ty: TypeVarId,
};

/// Public union `Content`.
pub const Content = union(enum) {
    func: struct {
        arg: TypeVarId,
        lset: TypeVarId,
        ret: TypeVarId,
    },
    list: TypeVarId,
    box: TypeVarId,
    tuple: Span(TypeVarId),
    tag_union: struct {
        tags: Span(Tag),
    },
    record: struct {
        fields: Span(Field),
    },
    primitive: Prim,
    lambda_set: Span(Lambda),
};

/// Public union `Node`.
pub const Node = union(enum) {
    link: TypeVarId,
    nominal: Nominal,
    unbd,
    for_a,
    content: Content,
};

/// Public struct `Store`.
pub const Store = struct {
    allocator: std.mem.Allocator,
    nodes: std.ArrayList(Node),
    type_var_ids: std.ArrayList(TypeVarId),
    tags: std.ArrayList(Tag),
    fields: std.ArrayList(Field),
    captures: std.ArrayList(Capture),
    lambdas: std.ArrayList(Lambda),

    pub fn init(allocator: std.mem.Allocator) Store {
        return .{
            .allocator = allocator,
            .nodes = .empty,
            .type_var_ids = .empty,
            .tags = .empty,
            .fields = .empty,
            .captures = .empty,
            .lambdas = .empty,
        };
    }

    pub fn deinit(self: *Store) void {
        self.nodes.deinit(self.allocator);
        self.type_var_ids.deinit(self.allocator);
        self.tags.deinit(self.allocator);
        self.fields.deinit(self.allocator);
        self.captures.deinit(self.allocator);
        self.lambdas.deinit(self.allocator);
    }

    pub fn fresh(self: *Store, node: Node) std.mem.Allocator.Error!TypeVarId {
        const idx: u32 = @intCast(self.nodes.items.len);
        try self.nodes.append(self.allocator, node);
        return @enumFromInt(idx);
    }

    pub fn freshUnbd(self: *Store) std.mem.Allocator.Error!TypeVarId {
        return try self.fresh(.unbd);
    }

    pub fn freshForA(self: *Store) std.mem.Allocator.Error!TypeVarId {
        return try self.fresh(.for_a);
    }

    pub fn freshContent(self: *Store, content: Content) std.mem.Allocator.Error!TypeVarId {
        return try self.fresh(.{ .content = content });
    }

    pub fn getNode(self: *const Store, id: TypeVarId) Node {
        return self.nodes.items[@intFromEnum(id)];
    }

    pub fn setNode(self: *Store, id: TypeVarId, node: Node) void {
        self.nodes.items[@intFromEnum(id)] = node;
    }

    pub fn unlink(self: *Store, id: TypeVarId) TypeVarId {
        const node = self.getNode(id);
        return switch (node) {
            .link => |next| blk: {
                const terminal = self.unlink(next);
                if (terminal != next) {
                    self.setNode(id, .{ .link = terminal });
                }
                break :blk terminal;
            },
            .nominal => |nominal| blk: {
                const terminal = self.unlink(nominal.backing);
                if (terminal != nominal.backing) {
                    var rewritten = nominal;
                    rewritten.backing = terminal;
                    self.setNode(id, .{ .nominal = rewritten });
                }
                break :blk id;
            },
            else => id,
        };
    }

    pub fn unlinkConst(self: *const Store, id: TypeVarId) TypeVarId {
        const node = self.getNode(id);
        return switch (node) {
            .link => |next| self.unlinkConst(next),
            .nominal => |nominal| self.unlinkConst(nominal.backing),
            else => id,
        };
    }

    pub fn unlinkPreservingNominal(self: *Store, id: TypeVarId) TypeVarId {
        const node = self.getNode(id);
        return switch (node) {
            .link => |next| blk: {
                const terminal = self.unlinkPreservingNominal(next);
                if (terminal != next) {
                    self.setNode(id, .{ .link = terminal });
                }
                break :blk terminal;
            },
            else => id,
        };
    }

    pub fn unlinkErasingNominal(self: *Store, id: TypeVarId) TypeVarId {
        const node = self.getNode(id);
        return switch (node) {
            .link => |next| blk: {
                const terminal = self.unlinkErasingNominal(next);
                if (terminal != next) {
                    self.setNode(id, .{ .link = terminal });
                }
                break :blk terminal;
            },
            .nominal => |nominal| blk: {
                const terminal = self.unlinkErasingNominal(nominal.backing);
                if (terminal != nominal.backing) {
                    var rewritten = nominal;
                    rewritten.backing = terminal;
                    self.setNode(id, .{ .nominal = rewritten });
                }
                break :blk terminal;
            },
            else => id,
        };
    }

    pub fn addTypeVarSpan(self: *Store, ids: []const TypeVarId) std.mem.Allocator.Error!Span(TypeVarId) {
        if (ids.len == 0) return Span(TypeVarId).empty();
        const start: u32 = @intCast(self.type_var_ids.items.len);
        try self.type_var_ids.appendSlice(self.allocator, ids);
        return .{ .start = start, .len = @intCast(ids.len) };
    }

    pub fn sliceTypeVarSpan(self: *const Store, span: Span(TypeVarId)) []const TypeVarId {
        if (span.len == 0) return &.{};
        return self.type_var_ids.items[span.start..][0..span.len];
    }

    pub fn addTags(self: *Store, values: []const Tag) std.mem.Allocator.Error!Span(Tag) {
        if (values.len == 0) return Span(Tag).empty();
        if (values.len == 1) {
            const start_single: u32 = @intCast(self.tags.items.len);
            try self.tags.append(self.allocator, values[0]);
            return .{ .start = start_single, .len = 1 };
        }

        const start: u32 = @intCast(self.tags.items.len);
        self.assertDistinctSortedTags(values);
        try self.tags.appendSlice(self.allocator, values);
        return .{ .start = start, .len = @intCast(values.len) };
    }

    pub fn sliceTags(self: *const Store, span: Span(Tag)) []const Tag {
        if (span.len == 0) return &.{};
        return self.tags.items[span.start..][0..span.len];
    }

    pub fn addFields(self: *Store, values: []const Field) std.mem.Allocator.Error!Span(Field) {
        if (values.len == 0) return Span(Field).empty();
        if (values.len > 1) {
            assertDistinctFields(values);
        }
        const start: u32 = @intCast(self.fields.items.len);
        try self.fields.appendSlice(self.allocator, values);
        return .{ .start = start, .len = @intCast(values.len) };
    }

    pub fn sliceFields(self: *const Store, span: Span(Field)) []const Field {
        if (span.len == 0) return &.{};
        return self.fields.items[span.start..][0..span.len];
    }

    pub fn addCaptures(self: *Store, values: []const Capture) std.mem.Allocator.Error!Span(Capture) {
        if (values.len == 0) return Span(Capture).empty();
        if (values.len == 1) {
            const start_single: u32 = @intCast(self.captures.items.len);
            try self.captures.append(self.allocator, values[0]);
            return .{ .start = start_single, .len = 1 };
        }

        const start: u32 = @intCast(self.captures.items.len);
        assertDistinctSortedCaptures(values);
        try self.captures.appendSlice(self.allocator, values);
        return .{ .start = start, .len = @intCast(values.len) };
    }

    pub fn sliceCaptures(self: *const Store, span: Span(Capture)) []const Capture {
        if (span.len == 0) return &.{};
        return self.captures.items[span.start..][0..span.len];
    }

    pub fn addLambdas(self: *Store, values: []const Lambda) std.mem.Allocator.Error!Span(Lambda) {
        if (values.len == 0) return Span(Lambda).empty();
        if (values.len == 1) {
            const start_single: u32 = @intCast(self.lambdas.items.len);
            try self.lambdas.append(self.allocator, values[0]);
            return .{ .start = start_single, .len = 1 };
        }

        const start: u32 = @intCast(self.lambdas.items.len);
        assertDistinctSortedLambdas(values);
        try self.lambdas.appendSlice(self.allocator, values);
        return .{ .start = start, .len = @intCast(values.len) };
    }

    pub fn sliceLambdas(self: *const Store, span: Span(Lambda)) []const Lambda {
        if (span.len == 0) return &.{};
        return self.lambdas.items[span.start..][0..span.len];
    }

    pub fn fnShape(self: *const Store, ty: TypeVarId) FnShape {
        const id = self.unlinkConst(ty);
        return switch (self.getNode(id)) {
            .content => |content| switch (content) {
                .func => |func| .{
                    .arg = func.arg,
                    .lset = func.lset,
                    .ret = func.ret,
                },
                else => debugPanic("lambdasolved.type fnShape expected function"),
            },
            else => debugPanic("lambdasolved.type fnShape expected function"),
        };
    }

    pub fn lambdaRepr(self: *const Store, fn_ty: TypeVarId) LambdaRepr {
        const fn_shape = self.fnShape(fn_ty);
        const lset = self.unlinkConst(fn_shape.lset);
        return switch (self.getNode(lset)) {
            .content => |content| switch (content) {
                .lambda_set => |span| .{ .lset = self.sliceLambdas(span) },
                .primitive => |prim| switch (prim) {
                    .erased => .erased,
                    else => debugPanic("lambdasolved.type lambdaRepr expected lambda set"),
                },
                else => debugPanic("lambdasolved.type lambdaRepr expected lambda set"),
            },
            .unbd, .for_a => .erased,
            else => debugPanic("lambdasolved.type lambdaRepr expected lambda set"),
        };
    }

    pub fn maybeLambdaRepr(self: *const Store, ty: TypeVarId) ?LambdaRepr {
        var id = ty;
        while (true) {
            switch (self.getNode(id)) {
                .link => |next| id = next,
                else => break,
            }
        }

        return switch (self.getNode(id)) {
            .nominal => |nominal| self.maybeLambdaRepr(nominal.backing),
            .content => |content| switch (content) {
                .func => self.lambdaRepr(id),
                .lambda_set => |span| .{ .lset = self.sliceLambdas(span) },
                .primitive => |prim| if (prim == .erased) .erased else null,
                else => null,
            },
            else => null,
        };
    }

    pub fn maybeLambdaMember(self: *const Store, fn_ty: TypeVarId, symbol: Symbol) ?LambdaMember {
        return switch (self.lambdaRepr(fn_ty)) {
            .erased => null,
            .lset => |lambdas| {
                for (lambdas) |lambda| {
                    if (lambda.symbol != symbol) continue;
                    return .{
                        .lambda = lambda,
                        .captures = self.sliceCaptures(lambda.captures),
                    };
                }
                return null;
            },
        };
    }

    pub fn requireLambdaMember(self: *const Store, fn_ty: TypeVarId, symbol: Symbol) LambdaMember {
        return self.maybeLambdaMember(fn_ty, symbol) orelse
            debugPanic("lambdasolved.type requireLambdaMember missing lambda in lambda set");
    }

    pub fn requireLambdaCaptures(self: *const Store, fn_ty: TypeVarId, symbol: Symbol) []const Capture {
        return self.requireLambdaMember(fn_ty, symbol).captures;
    }

    pub fn hasCapturelessLambda(self: *const Store, fn_ty: TypeVarId, symbol: Symbol) bool {
        const member = self.maybeLambdaMember(fn_ty, symbol) orelse return false;
        return member.captures.len == 0;
    }

    pub fn exactTargetForResolvedFn(self: *const Store, fn_ty: TypeVarId) ?Symbol {
        return switch (self.lambdaRepr(fn_ty)) {
            .erased => null,
            .lset => |lambdas| if (lambdas.len == 1) lambdas[0].symbol else null,
        };
    }

    pub fn structuralKeyOwned(self: *const Store, ty: TypeVarId) std.mem.Allocator.Error![]u8 {
        var serializer = StructuralKeySerializer{
            .allocator = self.allocator,
            .store = self,
            .seen = std.AutoHashMap(TypeVarId, u32).init(self.allocator),
            .out = .empty,
            .next_id = 0,
        };
        defer serializer.deinit();
        try serializer.writeType(ty);
        return try serializer.out.toOwnedSlice(self.allocator);
    }

    const TypePair = struct {
        left: TypeVarId,
        right: TypeVarId,
    };

    pub fn equalIds(self: *Store, left: TypeVarId, right: TypeVarId) bool {
        var visited = std.ArrayList(TypePair).empty;
        defer visited.deinit(self.allocator);
        return self.equalIdsVisited(left, right, &visited) catch false;
    }

    fn equalIdsVisited(
        self: *Store,
        left: TypeVarId,
        right: TypeVarId,
        visited: *std.ArrayList(TypePair),
    ) std.mem.Allocator.Error!bool {
        const left_id = self.unlink(left);
        const right_id = self.unlink(right);
        if (left_id == right_id) return true;

        for (visited.items) |pair| {
            if (pair.left == left_id and pair.right == right_id) return true;
        }
        try visited.append(self.allocator, .{ .left = left_id, .right = right_id });

        const left_node = self.getNode(left_id);
        const right_node = self.getNode(right_id);
        if (@as(std.meta.Tag(Node), left_node) != @as(std.meta.Tag(Node), right_node)) return false;

        return switch (left_node) {
            .link => unreachable,
            .nominal => |left_nominal| switch (right_node) {
                .nominal => |right_nominal| blk: {
                    if (left_nominal.module_idx != right_nominal.module_idx) break :blk false;
                    if (left_nominal.ident != right_nominal.ident) break :blk false;
                    if (left_nominal.is_opaque != right_nominal.is_opaque) break :blk false;

                    const left_args = self.sliceTypeVarSpan(left_nominal.args);
                    const right_args = self.sliceTypeVarSpan(right_nominal.args);
                    if (left_args.len != right_args.len) break :blk false;
                    for (left_args, right_args) |left_arg, right_arg| {
                        if (!try self.equalIdsVisited(left_arg, right_arg, visited)) break :blk false;
                    }

                    break :blk try self.equalIdsVisited(left_nominal.backing, right_nominal.backing, visited);
                },
                else => unreachable,
            },
            .unbd => true,
            .for_a => true,
            .content => |left_content| switch (right_node) {
                .content => |right_content| switch (right_content) {
                    .primitive => |right_prim| switch (left_content) {
                        .primitive => |left_prim| left_prim == right_prim,
                        else => false,
                    },
                    .func => |right_func| switch (left_content) {
                        .func => |left_func| blk: {
                            if (!try self.equalIdsVisited(left_func.arg, right_func.arg, visited)) break :blk false;
                            if (!try self.equalIdsVisited(left_func.lset, right_func.lset, visited)) break :blk false;
                            if (!try self.equalIdsVisited(left_func.ret, right_func.ret, visited)) break :blk false;
                            break :blk true;
                        },
                        else => false,
                    },
                    .list => |right_elem| switch (left_content) {
                        .list => |left_elem| try self.equalIdsVisited(left_elem, right_elem, visited),
                        else => false,
                    },
                    .box => |right_elem| switch (left_content) {
                        .box => |left_elem| try self.equalIdsVisited(left_elem, right_elem, visited),
                        else => false,
                    },
                    .tuple => |right_tuple| switch (left_content) {
                        .tuple => |left_tuple| blk: {
                            const left_elems = self.sliceTypeVarSpan(left_tuple);
                            const right_elems = self.sliceTypeVarSpan(right_tuple);
                            if (left_elems.len != right_elems.len) break :blk false;
                            for (left_elems, right_elems) |left_elem, right_elem| {
                                if (!try self.equalIdsVisited(left_elem, right_elem, visited)) break :blk false;
                            }
                            break :blk true;
                        },
                        else => false,
                    },
                    .tag_union => |right_union| switch (left_content) {
                        .tag_union => |left_union| blk: {
                            const left_tags = self.sliceTags(left_union.tags);
                            const right_tags = self.sliceTags(right_union.tags);
                            if (left_tags.len != right_tags.len) break :blk false;
                            for (left_tags, right_tags) |left_tag, right_tag| {
                                if (left_tag.name != right_tag.name) break :blk false;
                                const left_args = self.sliceTypeVarSpan(left_tag.args);
                                const right_args = self.sliceTypeVarSpan(right_tag.args);
                                if (left_args.len != right_args.len) break :blk false;
                                for (left_args, right_args) |left_arg, right_arg| {
                                    if (!try self.equalIdsVisited(left_arg, right_arg, visited)) break :blk false;
                                }
                            }
                            break :blk true;
                        },
                        else => false,
                    },
                    .record => |right_record| switch (left_content) {
                        .record => |left_record| blk: {
                            const left_fields = self.sliceFields(left_record.fields);
                            const right_fields = self.sliceFields(right_record.fields);
                            if (left_fields.len != right_fields.len) break :blk false;
                            for (left_fields, right_fields) |left_field, right_field| {
                                if (left_field.name != right_field.name) break :blk false;
                                if (!try self.equalIdsVisited(left_field.ty, right_field.ty, visited)) break :blk false;
                            }
                            break :blk true;
                        },
                        else => false,
                    },
                    .lambda_set => |right_lset| switch (left_content) {
                        .lambda_set => |left_lset| blk: {
                            const left_lambdas = self.sliceLambdas(left_lset);
                            const right_lambdas = self.sliceLambdas(right_lset);
                            if (left_lambdas.len != right_lambdas.len) break :blk false;
                            for (left_lambdas, right_lambdas) |left_lambda, right_lambda| {
                                if (left_lambda.symbol != right_lambda.symbol) break :blk false;
                                const left_caps = self.sliceCaptures(left_lambda.captures);
                                const right_caps = self.sliceCaptures(right_lambda.captures);
                                if (left_caps.len != right_caps.len) break :blk false;
                                for (left_caps, right_caps) |left_cap, right_cap| {
                                    if (left_cap.symbol != right_cap.symbol) break :blk false;
                                    if (!try self.equalIdsVisited(left_cap.ty, right_cap.ty, visited)) break :blk false;
                                }
                            }
                            break :blk true;
                        },
                        else => false,
                    },
                },
                else => unreachable,
            },
        };
    }

    fn assertDistinctSortedTags(self: *Store, tags: []const Tag) void {
        if (tags.len <= 1) return;

        var prev = tags[0];
        for (tags[1..]) |tag| {
            if (@as(u32, @bitCast(tag.name)) > @as(u32, @bitCast(prev.name))) {
                prev = tag;
                continue;
            }
            if (@as(u32, @bitCast(tag.name)) < @as(u32, @bitCast(prev.name))) {
                debugPanic("lambdasolved.type tag constructors were not pre-sorted");
            }

            const prev_args = self.sliceTypeVarSpan(prev.args);
            const tag_args = self.sliceTypeVarSpan(tag.args);
            if (prev_args.len != tag_args.len) {
                debugPanic("lambdasolved.type duplicate tag constructor had different arity");
            }
            for (prev_args, tag_args) |prev_arg, tag_arg| {
                if (!self.equalIds(prev_arg, tag_arg)) {
                    debugPanic("lambdasolved.type duplicate tag constructor had different payload types");
                }
            }
            debugPanic("lambdasolved.type duplicate tag constructor reached addTags");
        }
    }
};

const StructuralKeySerializer = struct {
    allocator: std.mem.Allocator,
    store: *const Store,
    seen: std.AutoHashMap(TypeVarId, u32),
    out: std.ArrayList(u8),
    next_id: u32,

    fn deinit(self: *StructuralKeySerializer) void {
        self.seen.deinit();
        self.out.deinit(self.allocator);
    }

    fn writeType(self: *StructuralKeySerializer, ty: TypeVarId) std.mem.Allocator.Error!void {
        const root = self.store.unlinkConst(ty);
        if (self.seen.get(root)) |existing| {
            try self.out.append(self.allocator, 'r');
            try self.writeU32(existing);
            return;
        }

        const id = self.next_id;
        self.next_id += 1;
        try self.seen.put(root, id);

        switch (self.store.getNode(root)) {
            .link => unreachable,
            .unbd => try self.out.append(self.allocator, 'u'),
            .for_a => try self.out.append(self.allocator, 'a'),
            .nominal => |nominal| {
                try self.out.append(self.allocator, 'n');
                try self.writeU32(nominal.module_idx);
                try self.writeU32(@as(u32, @bitCast(nominal.ident)));
                try self.writeBool(nominal.is_opaque);
                const args = self.store.sliceTypeVarSpan(nominal.args);
                try self.writeU32(@intCast(args.len));
                for (args) |arg| try self.writeType(arg);
                try self.writeType(nominal.backing);
            },
            .content => |content| switch (content) {
                .func => |func| {
                    try self.out.append(self.allocator, 'f');
                    try self.writeType(func.arg);
                    try self.writeType(func.lset);
                    try self.writeType(func.ret);
                },
                .list => |elem| {
                    try self.out.append(self.allocator, 'l');
                    try self.writeType(elem);
                },
                .box => |elem| {
                    try self.out.append(self.allocator, 'b');
                    try self.writeType(elem);
                },
                .tuple => |span| {
                    try self.out.append(self.allocator, 't');
                    const elems = self.store.sliceTypeVarSpan(span);
                    try self.writeU32(@intCast(elems.len));
                    for (elems) |elem| try self.writeType(elem);
                },
                .tag_union => |tag_union| {
                    try self.out.append(self.allocator, 'g');
                    const tags = self.store.sliceTags(tag_union.tags);
                    try self.writeU32(@intCast(tags.len));
                    for (tags) |tag| {
                        try self.writeU32(@as(u32, @bitCast(tag.name)));
                        const args = self.store.sliceTypeVarSpan(tag.args);
                        try self.writeU32(@intCast(args.len));
                        for (args) |arg| try self.writeType(arg);
                    }
                },
                .record => |record| {
                    try self.out.append(self.allocator, 'd');
                    const fields = self.store.sliceFields(record.fields);
                    try self.writeU32(@intCast(fields.len));
                    for (fields) |field| {
                        try self.writeU32(@as(u32, @bitCast(field.name)));
                        try self.writeType(field.ty);
                    }
                },
                .primitive => |prim| {
                    try self.out.append(self.allocator, 'p');
                    try self.writeU32(@intFromEnum(prim));
                },
                .lambda_set => |span| {
                    try self.out.append(self.allocator, 's');
                    const lambdas = self.store.sliceLambdas(span);
                    try self.writeU32(@intCast(lambdas.len));
                    for (lambdas) |lambda| {
                        try self.writeU32(lambda.symbol.raw());
                        const captures = self.store.sliceCaptures(lambda.captures);
                        try self.writeU32(@intCast(captures.len));
                        for (captures) |capture| {
                            try self.writeU32(capture.symbol.raw());
                            try self.writeType(capture.ty);
                        }
                    }
                },
            },
        }
    }

    fn writeU32(self: *StructuralKeySerializer, value: u32) std.mem.Allocator.Error!void {
        try self.out.appendSlice(self.allocator, std.mem.asBytes(&value));
    }

    fn writeBool(self: *StructuralKeySerializer, value: bool) std.mem.Allocator.Error!void {
        try self.out.append(self.allocator, if (value) 1 else 0);
    }
};

fn assertDistinctSortedCaptures(values: []const Capture) void {
    if (values.len <= 1) return;

    var prev = values[0];
    for (values[1..]) |capture| {
        if (capture.symbol.raw() > prev.symbol.raw()) {
            prev = capture;
            continue;
        }
        if (capture.symbol.raw() < prev.symbol.raw()) {
            debugPanic("lambdasolved.type captures were not pre-sorted");
        }
        debugPanic("lambdasolved.type duplicate capture symbol reached addCaptures");
    }
}

fn assertDistinctFields(values: []const Field) void {
    if (values.len <= 1) return;

    for (values, 0..) |field, i| {
        for (values[i + 1 ..]) |other| {
            if (field.name == other.name) {
                debugPanic("lambdasolved.type duplicate record field reached addFields");
            }
        }
    }
}

fn assertDistinctSortedLambdas(values: []const Lambda) void {
    if (values.len <= 1) return;

    var prev = values[0];
    for (values[1..]) |lambda| {
        if (lambda.symbol.raw() > prev.symbol.raw()) {
            prev = lambda;
            continue;
        }
        if (lambda.symbol.raw() < prev.symbol.raw()) {
            debugPanic("lambdasolved.type lambdas were not pre-sorted");
        }
        debugPanic("lambdasolved.type duplicate lambda symbol reached addLambdas");
    }
}

test "lambdasolved type tests" {
    std.testing.refAllDecls(@This());
}

fn debugPanic(comptime msg: []const u8) noreturn {
    @branchHint(.cold);
    std.debug.panic("{s}", .{msg});
}
