//! Canonical, serializable model of a package's public API, used by `roc bump`
//! to classify the semver magnitude of a change (patch/minor/major), Elm-style.
//!
//! The model is deliberately independent of any single compiler run: type
//! identity is carried by names (package url id + major version, module path,
//! type path), never by content hashes or compiler-internal keys, so two
//! separately-compiled package versions can be compared.
//!
//! Type variables are normalized to binding-position indices (assigned by
//! first occurrence in a fixed canonical traversal). Alpha-equivalent type
//! schemes therefore produce byte-identical canonical S-expressions, which is
//! exactly Elm's "consistent and injective renaming" equivalence: comparing
//! canonical strings is the whole equality check.

const std = @import("std");

const PackageApi = @This();

arena: std.heap.ArenaAllocator,
modules: std.ArrayListUnmanaged(ModuleApi),
types: std.ArrayListUnmanaged(ApiType),

/// Index into `types`.
pub const TypeId = enum(u32) { _ };

pub const ModuleApi = struct {
    /// The exposed module name from the package header, e.g. "Parser".
    name: []const u8,
    items: std.ArrayListUnmanaged(Item),
};

/// One public item: the exposed type itself, an associated method/constant,
/// or a nested associated type. `path` is the dotted path within the module,
/// e.g. "Parser", "Parser.map2", "Taxonomy.Domain.Kingdom.Phylum.Class.Order.Species.display".
pub const Item = struct {
    path: []const u8,
    kind: Kind,

    pub const Kind = union(enum) {
        /// A value, method, or constant; the payload is its type scheme root.
        value: TypeId,
        alias: Alias,
        nominal: Nominal,
    };

    pub const Alias = struct {
        arity: u32,
        target: TypeId,
    };

    pub const Nominal = struct {
        arity: u32,
        is_opaque: bool,
        /// The declared backing type. Null iff `is_opaque`: an opaque type's
        /// backing is invisible to consumers, so changing it is patch-level
        /// by construction.
        backing: ?TypeId,
    };
};

pub const ApiType = union(enum) {
    variable: Variable,
    function: Function,
    record: Record,
    tuple: []TypeId,
    tag_union: TagUnion,
    named: Named,
    empty_record,
    empty_tag_union,
};

/// A flex or rigid type variable. Multiple references to the same variable
/// within one item must share one `variable` node (one TypeId); variable
/// nodes must not be shared across items (normalization is per-item).
pub const Variable = struct {
    /// Binding-position index, assigned by `normalize`. Builders may leave 0.
    index: u32 = 0,
    /// Source name, used for human-readable rendering only; it never
    /// participates in canonical identity.
    display_name: ?[]const u8 = null,
    /// Static dispatch constraints (`where` clauses). Sorted by method name
    /// during normalization.
    constraints: []Constraint = &.{},
};

pub const Constraint = struct {
    method: []const u8,
    fn_ty: TypeId,
};

pub const Function = struct {
    effectful: bool,
    args: []TypeId,
    ret: TypeId,
};

pub const Record = struct {
    /// Sorted by field name during normalization.
    fields: []Field,
    /// Extension variable for open records; null means closed.
    ext: ?TypeId,
};

pub const Field = struct {
    name: []const u8,
    ty: TypeId,
};

pub const TagUnion = struct {
    /// Sorted by tag name during normalization.
    tags: []Tag,
    /// Extension variable for open unions; null means closed.
    ext: ?TypeId,
};

pub const Tag = struct {
    name: []const u8,
    args: []TypeId,
};

/// A reference to an alias or nominal type by stable identity. References are
/// never expanded: a change to the referenced declaration is reported at that
/// declaration's own item, not at every use site.
pub const Named = struct {
    origin: TypeOrigin,
    /// Qualified path: for `self`, "Module.Type[.Nested...]"; for builtins and
    /// external packages, the qualified type name as seen by consumers.
    path: []const u8,
    args: []TypeId,
};

pub const TypeOrigin = union(enum) {
    /// Declared in the package being diffed.
    self,
    /// Declared in the compiler builtins.
    builtin,
    /// Declared in a versioned URL dependency. The major version is part of
    /// identity: pointing the public API at a different major of the same
    /// dependency is a breaking change.
    external: External,

    pub const External = struct {
        url_id: []const u8,
        major: u32,
    };
};

pub fn init(gpa: std.mem.Allocator) PackageApi {
    return .{
        .arena = std.heap.ArenaAllocator.init(gpa),
        .modules = .empty,
        .types = .empty,
    };
}

pub fn deinit(self: *PackageApi) void {
    self.arena.deinit();
}

pub fn allocator(self: *PackageApi) std.mem.Allocator {
    return self.arena.allocator();
}

pub fn addType(self: *PackageApi, ty: ApiType) std.mem.Allocator.Error!TypeId {
    const id: TypeId = @enumFromInt(self.types.items.len);
    try self.types.append(self.arena.allocator(), ty);
    return id;
}

pub fn getType(self: *const PackageApi, id: TypeId) *ApiType {
    return &self.types.items[@intFromEnum(id)];
}

/// Returns the index of the added module.
pub fn addModule(self: *PackageApi, name: []const u8) std.mem.Allocator.Error!usize {
    try self.modules.append(self.arena.allocator(), .{ .name = name, .items = .empty });
    return self.modules.items.len - 1;
}

pub fn addItem(self: *PackageApi, module_index: usize, item: Item) std.mem.Allocator.Error!void {
    try self.modules.items[module_index].items.append(self.arena.allocator(), item);
}

/// Sort modules and items, sort record fields / tags / constraints, and
/// assign canonical binding-position indices to all type variables. Must be
/// called once, after building and before diffing or serializing.
pub fn normalize(self: *PackageApi) std.mem.Allocator.Error!void {
    std.mem.sort(ModuleApi, self.modules.items, {}, moduleLessThan);
    for (self.modules.items) |*module| {
        std.mem.sort(Item, module.items.items, {}, itemLessThan);
        for (module.items.items) |item| {
            try self.normalizeItem(item);
        }
    }
}

fn moduleLessThan(_: void, a: ModuleApi, b: ModuleApi) bool {
    return std.mem.order(u8, a.name, b.name) == .lt;
}

fn itemLessThan(_: void, a: Item, b: Item) bool {
    return std.mem.order(u8, a.path, b.path) == .lt;
}

fn fieldLessThan(_: void, a: Field, b: Field) bool {
    return std.mem.order(u8, a.name, b.name) == .lt;
}

fn tagLessThan(_: void, a: Tag, b: Tag) bool {
    return std.mem.order(u8, a.name, b.name) == .lt;
}

fn constraintLessThan(_: void, a: Constraint, b: Constraint) bool {
    return std.mem.order(u8, a.method, b.method) == .lt;
}

const NormalizeState = struct {
    next_index: u32,
    assigned: std.AutoHashMapUnmanaged(TypeId, void),
};

fn normalizeItem(self: *PackageApi, item: Item) std.mem.Allocator.Error!void {
    var state = NormalizeState{ .next_index = 0, .assigned = .{} };
    defer state.assigned.deinit(self.arena.child_allocator);

    switch (item.kind) {
        .value => |ty| try self.normalizeType(ty, &state),
        .alias => |alias| try self.normalizeType(alias.target, &state),
        .nominal => |nominal| if (nominal.backing) |backing| try self.normalizeType(backing, &state),
    }
}

fn normalizeType(self: *PackageApi, id: TypeId, state: *NormalizeState) std.mem.Allocator.Error!void {
    const ty = self.getType(id);
    switch (ty.*) {
        .variable => |*variable| {
            // A variable already assigned in this item has been fully visited
            // (constraints included); stopping here also breaks the cycles
            // that self-referential constraints introduce.
            const gop = try state.assigned.getOrPut(self.arena.child_allocator, id);
            if (gop.found_existing) return;
            variable.index = state.next_index;
            state.next_index += 1;
            std.mem.sort(Constraint, variable.constraints, {}, constraintLessThan);
            for (variable.constraints) |constraint| {
                try self.normalizeType(constraint.fn_ty, state);
            }
        },
        .function => |function| {
            for (function.args) |arg| try self.normalizeType(arg, state);
            try self.normalizeType(function.ret, state);
        },
        .record => |*record| {
            std.mem.sort(Field, record.fields, {}, fieldLessThan);
            for (record.fields) |field| try self.normalizeType(field.ty, state);
            if (record.ext) |ext| try self.normalizeType(ext, state);
        },
        .tuple => |elems| {
            for (elems) |elem| try self.normalizeType(elem, state);
        },
        .tag_union => |*tag_union| {
            std.mem.sort(Tag, tag_union.tags, {}, tagLessThan);
            for (tag_union.tags) |tag| {
                for (tag.args) |arg| try self.normalizeType(arg, state);
            }
            if (tag_union.ext) |ext| try self.normalizeType(ext, state);
        },
        .named => |named| {
            for (named.args) |arg| try self.normalizeType(arg, state);
        },
        .empty_record, .empty_tag_union => {},
    }
}

const WriteError = std.mem.Allocator.Error || std.Io.Writer.Error;

/// Write the canonical S-expression for one item. Requires `normalize` to
/// have run. Two items are API-equal iff their canonical S-expressions are
/// byte-identical.
pub fn writeItemSExpr(self: *const PackageApi, gpa: std.mem.Allocator, item: Item, writer: *std.Io.Writer) WriteError!void {
    var visited = std.AutoHashMapUnmanaged(TypeId, void).empty;
    defer visited.deinit(gpa);
    var state = SExprState{ .gpa = gpa, .visited = &visited };

    switch (item.kind) {
        .value => |ty| {
            try writer.writeAll("(value ");
            try self.writeTypeSExpr(ty, writer, &state);
            try writer.writeAll(")");
        },
        .alias => |alias| {
            try writer.print("(alias (arity {d}) ", .{alias.arity});
            try self.writeTypeSExpr(alias.target, writer, &state);
            try writer.writeAll(")");
        },
        .nominal => |nominal| {
            try writer.print("(nominal (arity {d})", .{nominal.arity});
            if (nominal.is_opaque) {
                std.debug.assert(nominal.backing == null);
                try writer.writeAll(" (opaque)");
            } else {
                try writer.writeAll(" (backing ");
                try self.writeTypeSExpr(nominal.backing.?, writer, &state);
                try writer.writeAll(")");
            }
            try writer.writeAll(")");
        },
    }
}

const SExprState = struct {
    gpa: std.mem.Allocator,
    visited: *std.AutoHashMapUnmanaged(TypeId, void),
};

fn writeTypeSExpr(self: *const PackageApi, id: TypeId, writer: *std.Io.Writer, state: *SExprState) WriteError!void {
    const ty = self.getType(id);
    switch (ty.*) {
        .variable => |variable| {
            // Constraints are written at the variable's first occurrence only;
            // later references are bare. This keeps the form canonical and
            // finite even when a constraint's signature mentions the variable
            // it constrains.
            const gop = try state.visited.getOrPut(state.gpa, id);
            if (gop.found_existing or variable.constraints.len == 0) {
                try writer.print("(var {d})", .{variable.index});
                return;
            }
            try writer.print("(var {d}", .{variable.index});
            for (variable.constraints) |constraint| {
                try writer.writeAll(" (where \"");
                try writeEscaped(writer, constraint.method);
                try writer.writeAll("\" ");
                try self.writeTypeSExpr(constraint.fn_ty, writer, state);
                try writer.writeAll(")");
            }
            try writer.writeAll(")");
        },
        .function => |function| {
            try writer.writeAll(if (function.effectful) "(fn!" else "(fn");
            for (function.args) |arg| {
                try writer.writeAll(" ");
                try self.writeTypeSExpr(arg, writer, state);
            }
            try writer.writeAll(" ");
            try self.writeTypeSExpr(function.ret, writer, state);
            try writer.writeAll(")");
        },
        .record => |record| {
            try writer.writeAll("(record");
            for (record.fields) |field| {
                try writer.writeAll(" (field \"");
                try writeEscaped(writer, field.name);
                try writer.writeAll("\" ");
                try self.writeTypeSExpr(field.ty, writer, state);
                try writer.writeAll(")");
            }
            if (record.ext) |ext| {
                try writer.writeAll(" (ext ");
                try self.writeTypeSExpr(ext, writer, state);
                try writer.writeAll(")");
            }
            try writer.writeAll(")");
        },
        .tuple => |elems| {
            try writer.writeAll("(tuple");
            for (elems) |elem| {
                try writer.writeAll(" ");
                try self.writeTypeSExpr(elem, writer, state);
            }
            try writer.writeAll(")");
        },
        .tag_union => |tag_union| {
            try writer.writeAll("(tag-union");
            for (tag_union.tags) |tag| {
                try writer.writeAll(" (tag \"");
                try writeEscaped(writer, tag.name);
                try writer.writeAll("\"");
                for (tag.args) |arg| {
                    try writer.writeAll(" ");
                    try self.writeTypeSExpr(arg, writer, state);
                }
                try writer.writeAll(")");
            }
            if (tag_union.ext) |ext| {
                try writer.writeAll(" (ext ");
                try self.writeTypeSExpr(ext, writer, state);
                try writer.writeAll(")");
            }
            try writer.writeAll(")");
        },
        .named => |named| {
            try writer.writeAll("(named ");
            switch (named.origin) {
                .self => try writer.writeAll("self"),
                .builtin => try writer.writeAll("builtin"),
                .external => |external| {
                    try writer.writeAll("(pkg \"");
                    try writeEscaped(writer, external.url_id);
                    try writer.print("\" {d})", .{external.major});
                },
            }
            try writer.writeAll(" \"");
            try writeEscaped(writer, named.path);
            try writer.writeAll("\"");
            for (named.args) |arg| {
                try writer.writeAll(" ");
                try self.writeTypeSExpr(arg, writer, state);
            }
            try writer.writeAll(")");
        },
        .empty_record => try writer.writeAll("(empty-record)"),
        .empty_tag_union => try writer.writeAll("(empty-tag-union)"),
    }
}

fn writeEscaped(writer: *std.Io.Writer, text: []const u8) std.Io.Writer.Error!void {
    for (text) |char| {
        switch (char) {
            '"' => try writer.writeAll("\\\""),
            '\\' => try writer.writeAll("\\\\"),
            else => try writer.writeByte(char),
        }
    }
}

/// Allocate the canonical S-expression string for one item; caller frees.
pub fn itemCanonicalString(self: *const PackageApi, gpa: std.mem.Allocator, item: Item) std.mem.Allocator.Error![]u8 {
    var out = std.Io.Writer.Allocating.init(gpa);
    defer out.deinit();
    self.writeItemSExpr(gpa, item, &out.writer) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        error.WriteFailed => return error.OutOfMemory, // Allocating writer only fails on OOM.
    };
    return try out.toOwnedSlice();
}

/// Write the full API as an S-expression, one line per item. Requires
/// `normalize` to have run. Used for golden tests and debugging.
pub fn writeSExpr(self: *const PackageApi, gpa: std.mem.Allocator, writer: *std.Io.Writer) WriteError!void {
    try writer.writeAll("(package-api");
    for (self.modules.items) |module| {
        try writer.writeAll("\n  (module \"");
        try writeEscaped(writer, module.name);
        try writer.writeAll("\"");
        for (module.items.items) |item| {
            try writer.writeAll("\n    (item \"");
            try writeEscaped(writer, item.path);
            try writer.writeAll("\" ");
            try self.writeItemSExpr(gpa, item, writer);
            try writer.writeAll(")");
        }
        try writer.writeAll(")");
    }
    try writer.writeAll(")\n");
}

/// Render a human-readable signature for one item, for diff output. Variables
/// render as their source name when available, else `a`, `b`, ... by index.
pub fn renderItemSignature(self: *const PackageApi, gpa: std.mem.Allocator, item: Item, writer: *std.Io.Writer) WriteError!void {
    var visited = std.AutoHashMapUnmanaged(TypeId, void).empty;
    defer visited.deinit(gpa);
    var state = SExprState{ .gpa = gpa, .visited = &visited };

    switch (item.kind) {
        .value => |ty| try self.renderType(ty, writer, &state),
        .alias => |alias| {
            try writer.writeAll("(alias for ");
            try self.renderType(alias.target, writer, &state);
            try writer.writeAll(")");
        },
        .nominal => |nominal| {
            if (nominal.is_opaque) {
                try writer.writeAll("(opaque type)");
            } else {
                try writer.writeAll(":= ");
                try self.renderType(nominal.backing.?, writer, &state);
            }
        },
    }

    // Collect `where` clauses from every constrained variable reachable from
    // this item, in first-occurrence order.
    var where_out = std.Io.Writer.Allocating.init(gpa);
    defer where_out.deinit();
    var iter = state.visited.keyIterator();
    var constrained = std.ArrayListUnmanaged(TypeId).empty;
    defer constrained.deinit(gpa);
    while (iter.next()) |id| {
        if (self.getType(id.*).variable.constraints.len > 0) {
            try constrained.append(gpa, id.*);
        }
    }
    std.mem.sort(TypeId, constrained.items, self, variableIndexLessThan);
    var first = true;
    for (constrained.items) |id| {
        const variable = self.getType(id).variable;
        for (variable.constraints) |constraint| {
            try where_out.writer.writeAll(if (first) " where " else ", ");
            first = false;
            try self.renderVariableName(variable, &where_out.writer);
            try where_out.writer.print(".{s} : ", .{constraint.method});
            var inner_visited = std.AutoHashMapUnmanaged(TypeId, void).empty;
            defer inner_visited.deinit(gpa);
            var inner_state = SExprState{ .gpa = gpa, .visited = &inner_visited };
            try self.renderType(constraint.fn_ty, &where_out.writer, &inner_state);
        }
    }
    try writer.writeAll(where_out.written());
}

fn variableIndexLessThan(self: *const PackageApi, a: TypeId, b: TypeId) bool {
    return self.getType(a).variable.index < self.getType(b).variable.index;
}

fn renderVariableName(self: *const PackageApi, variable: Variable, writer: *std.Io.Writer) WriteError!void {
    _ = self;
    if (variable.display_name) |name| {
        try writer.writeAll(name);
    } else if (variable.index < 26) {
        try writer.writeByte('a' + @as(u8, @intCast(variable.index)));
    } else {
        try writer.print("t{d}", .{variable.index});
    }
}

fn renderType(self: *const PackageApi, id: TypeId, writer: *std.Io.Writer, state: *SExprState) WriteError!void {
    const ty = self.getType(id);
    switch (ty.*) {
        .variable => |variable| {
            _ = try state.visited.getOrPut(state.gpa, id);
            try self.renderVariableName(variable, writer);
        },
        .function => |function| {
            for (function.args, 0..) |arg, i| {
                if (i > 0) try writer.writeAll(", ");
                try self.renderTypeParenthesized(arg, writer, state);
            }
            try writer.writeAll(if (function.effectful) " => " else " -> ");
            try self.renderTypeParenthesized(function.ret, writer, state);
        },
        .record => |record| {
            try writer.writeAll("{ ");
            for (record.fields, 0..) |field, i| {
                if (i > 0) try writer.writeAll(", ");
                try writer.print("{s} : ", .{field.name});
                try self.renderType(field.ty, writer, state);
            }
            if (record.ext) |ext| {
                try writer.writeAll(", ..");
                try self.renderType(ext, writer, state);
            }
            try writer.writeAll(" }");
        },
        .tuple => |elems| {
            try writer.writeAll("(");
            for (elems, 0..) |elem, i| {
                if (i > 0) try writer.writeAll(", ");
                try self.renderType(elem, writer, state);
            }
            try writer.writeAll(")");
        },
        .tag_union => |tag_union| {
            try writer.writeAll("[");
            for (tag_union.tags, 0..) |tag, i| {
                if (i > 0) try writer.writeAll(", ");
                try writer.writeAll(tag.name);
                if (tag.args.len > 0) {
                    try writer.writeAll("(");
                    for (tag.args, 0..) |arg, j| {
                        if (j > 0) try writer.writeAll(", ");
                        try self.renderType(arg, writer, state);
                    }
                    try writer.writeAll(")");
                }
            }
            try writer.writeAll("]");
            if (tag_union.ext) |ext| {
                try self.renderType(ext, writer, state);
            }
        },
        .named => |named| {
            switch (named.origin) {
                .self, .builtin => {},
                .external => |external| try writer.print("{s} (major {d}) : ", .{ external.url_id, external.major }),
            }
            try writer.writeAll(named.path);
            if (named.args.len > 0) {
                try writer.writeAll("(");
                for (named.args, 0..) |arg, i| {
                    if (i > 0) try writer.writeAll(", ");
                    try self.renderType(arg, writer, state);
                }
                try writer.writeAll(")");
            }
        },
        .empty_record => try writer.writeAll("{}"),
        .empty_tag_union => try writer.writeAll("[]"),
    }
}

fn renderTypeParenthesized(self: *const PackageApi, id: TypeId, writer: *std.Io.Writer, state: *SExprState) WriteError!void {
    const needs_parens = self.getType(id).* == .function;
    if (needs_parens) try writer.writeAll("(");
    try self.renderType(id, writer, state);
    if (needs_parens) try writer.writeAll(")");
}

test "alpha-equivalent schemes produce identical canonical S-expressions" {
    const gpa = std.testing.allocator;

    // f : a, (a -> b) -> b   vs   f : x, (x -> y) -> y
    var api_a = PackageApi.init(gpa);
    defer api_a.deinit();
    const a_var1 = try api_a.addType(.{ .variable = .{ .display_name = "a" } });
    const a_var2 = try api_a.addType(.{ .variable = .{ .display_name = "b" } });
    const a_inner_args = try api_a.allocator().dupe(TypeId, &.{a_var1});
    const a_inner = try api_a.addType(.{ .function = .{ .effectful = false, .args = a_inner_args, .ret = a_var2 } });
    const a_outer_args = try api_a.allocator().dupe(TypeId, &.{ a_var1, a_inner });
    const a_outer = try api_a.addType(.{ .function = .{ .effectful = false, .args = a_outer_args, .ret = a_var2 } });
    const module_a = try api_a.addModule("M");
    try api_a.addItem(module_a, .{ .path = "M.f", .kind = .{ .value = a_outer } });
    try api_a.normalize();

    var api_b = PackageApi.init(gpa);
    defer api_b.deinit();
    const b_var1 = try api_b.addType(.{ .variable = .{ .display_name = "x" } });
    const b_var2 = try api_b.addType(.{ .variable = .{ .display_name = "y" } });
    const b_inner_args = try api_b.allocator().dupe(TypeId, &.{b_var1});
    const b_inner = try api_b.addType(.{ .function = .{ .effectful = false, .args = b_inner_args, .ret = b_var2 } });
    const b_outer_args = try api_b.allocator().dupe(TypeId, &.{ b_var1, b_inner });
    const b_outer = try api_b.addType(.{ .function = .{ .effectful = false, .args = b_outer_args, .ret = b_var2 } });
    const module_b = try api_b.addModule("M");
    try api_b.addItem(module_b, .{ .path = "M.f", .kind = .{ .value = b_outer } });
    try api_b.normalize();

    const str_a = try api_a.itemCanonicalString(gpa, api_a.modules.items[0].items.items[0]);
    defer gpa.free(str_a);
    const str_b = try api_b.itemCanonicalString(gpa, api_b.modules.items[0].items.items[0]);
    defer gpa.free(str_b);

    try std.testing.expectEqualStrings(str_a, str_b);
}

test "collapsing two variables into one changes the canonical form" {
    const gpa = std.testing.allocator;

    // f : a -> b   vs   f : a -> a
    var api_a = PackageApi.init(gpa);
    defer api_a.deinit();
    const a_var1 = try api_a.addType(.{ .variable = .{} });
    const a_var2 = try api_a.addType(.{ .variable = .{} });
    const a_args = try api_a.allocator().dupe(TypeId, &.{a_var1});
    const a_fn = try api_a.addType(.{ .function = .{ .effectful = false, .args = a_args, .ret = a_var2 } });
    const module_a = try api_a.addModule("M");
    try api_a.addItem(module_a, .{ .path = "M.f", .kind = .{ .value = a_fn } });
    try api_a.normalize();

    var api_b = PackageApi.init(gpa);
    defer api_b.deinit();
    const b_var = try api_b.addType(.{ .variable = .{} });
    const b_args = try api_b.allocator().dupe(TypeId, &.{b_var});
    const b_fn = try api_b.addType(.{ .function = .{ .effectful = false, .args = b_args, .ret = b_var } });
    const module_b = try api_b.addModule("M");
    try api_b.addItem(module_b, .{ .path = "M.f", .kind = .{ .value = b_fn } });
    try api_b.normalize();

    const str_a = try api_a.itemCanonicalString(gpa, api_a.modules.items[0].items.items[0]);
    defer gpa.free(str_a);
    const str_b = try api_b.itemCanonicalString(gpa, api_b.modules.items[0].items.items[0]);
    defer gpa.free(str_b);

    try std.testing.expect(!std.mem.eql(u8, str_a, str_b));
}

test "record fields and tags are sorted during normalization" {
    const gpa = std.testing.allocator;

    var api_a = PackageApi.init(gpa);
    defer api_a.deinit();
    const a_str = try api_a.addType(.{ .named = .{ .origin = .builtin, .path = "Str", .args = &.{} } });
    const a_fields = try api_a.allocator().dupe(Field, &.{
        .{ .name = "zeta", .ty = a_str },
        .{ .name = "alpha", .ty = a_str },
    });
    const a_rec = try api_a.addType(.{ .record = .{ .fields = a_fields, .ext = null } });
    const module_a = try api_a.addModule("M");
    try api_a.addItem(module_a, .{ .path = "M.r", .kind = .{ .value = a_rec } });
    try api_a.normalize();

    var api_b = PackageApi.init(gpa);
    defer api_b.deinit();
    const b_str = try api_b.addType(.{ .named = .{ .origin = .builtin, .path = "Str", .args = &.{} } });
    const b_fields = try api_b.allocator().dupe(Field, &.{
        .{ .name = "alpha", .ty = b_str },
        .{ .name = "zeta", .ty = b_str },
    });
    const b_rec = try api_b.addType(.{ .record = .{ .fields = b_fields, .ext = null } });
    const module_b = try api_b.addModule("M");
    try api_b.addItem(module_b, .{ .path = "M.r", .kind = .{ .value = b_rec } });
    try api_b.normalize();

    const str_a = try api_a.itemCanonicalString(gpa, api_a.modules.items[0].items.items[0]);
    defer gpa.free(str_a);
    const str_b = try api_b.itemCanonicalString(gpa, api_b.modules.items[0].items.items[0]);
    defer gpa.free(str_b);

    try std.testing.expectEqualStrings(str_a, str_b);
}

test "self-referential constraint does not loop" {
    const gpa = std.testing.allocator;

    // a where a.eq : a, a -> Bool
    var api = PackageApi.init(gpa);
    defer api.deinit();
    const bool_ty = try api.addType(.{ .named = .{ .origin = .builtin, .path = "Bool", .args = &.{} } });
    const var_id = try api.addType(.{ .variable = .{} });
    const eq_args = try api.allocator().dupe(TypeId, &.{ var_id, var_id });
    const eq_fn = try api.addType(.{ .function = .{ .effectful = false, .args = eq_args, .ret = bool_ty } });
    const constraints = try api.allocator().dupe(Constraint, &.{.{ .method = "eq", .fn_ty = eq_fn }});
    api.getType(var_id).variable.constraints = constraints;
    const module = try api.addModule("M");
    try api.addItem(module, .{ .path = "M.v", .kind = .{ .value = var_id } });
    try api.normalize();

    const str = try api.itemCanonicalString(gpa, api.modules.items[0].items.items[0]);
    defer gpa.free(str);

    try std.testing.expectEqualStrings(
        "(value (var 0 (where \"eq\" (fn (var 0) (var 0) (named builtin \"Bool\")))))",
        str,
    );
}
