//! The types `.boxy` lowering reasons about (design.md "`.boxy` Runtime
//! Lowering").
//!
//! A checked type lives in one module's checked type store and names that
//! module's ids for labels, declarations, and type variables. Boxy lowering
//! needs one program-wide view of the same types: to intern layouts and
//! static descriptors for closed types, to compose descriptors for types
//! that mention a worker's type variables, and to compare the rows a join
//! makes one. This store converts checked types into interned, structural,
//! module-independent nodes. A node that mentions a type variable names it by
//! its position in the converting worker's hidden descriptor parameters, so
//! a node is closed exactly when no `variable` node is reachable from it.
//!
//! Nominal types stay nominal: a node names its declaration and its argument
//! nodes, and the backing node of an instantiated declaration is computed on
//! demand from the declaration's checked backing with its formals bound to
//! the arguments, so a recursive declaration never expands without end.

const std = @import("std");
const base = @import("base");
const check = @import("check");
const collections = @import("collections");

const Allocator = std.mem.Allocator;
const checked = check.CheckedModule;
const names = check.CheckedNames;

/// Dense index of a checked module in one lowering.
pub const ModuleIndex = enum(u32) { _ };

/// One checked module's tables, read by every lowering step.
pub const ModuleView = struct {
    index: ModuleIndex,
    key: checked.ModuleId,
    view: checked.ImportedModuleView,

    pub fn types(self: *const ModuleView) checked.CheckedTypeStoreView {
        return self.view.checked_types;
    }

    pub fn bodies(self: *const ModuleView) checked.CheckedBodyStoreView {
        return self.view.checked_bodies;
    }

    pub fn nameStore(self: *const ModuleView) *const names.CanonicalNameStore {
        return self.view.canonical_names;
    }
};

/// The checked modules of one lowering, by dense index and by key.
pub const Modules = struct {
    allocator: Allocator,
    views: std.ArrayList(ModuleView) = .empty,
    by_key: std.AutoHashMapUnmanaged([32]u8, ModuleIndex) = .empty,

    pub fn deinit(self: *Modules) void {
        self.views.deinit(self.allocator);
        self.by_key.deinit(self.allocator);
    }

    /// Add a module unless one with the same key is present.
    pub fn add(self: *Modules, view: checked.ImportedModuleView) Allocator.Error!ModuleIndex {
        const entry = try self.by_key.getOrPut(self.allocator, view.key.bytes);
        if (entry.found_existing) return entry.value_ptr.*;
        const index: ModuleIndex = @fromBackingInt(@intCast(self.views.items.len));
        entry.value_ptr.* = index;
        try self.views.append(self.allocator, .{ .index = index, .key = view.key.digest(), .view = view });
        return index;
    }

    pub fn get(self: *const Modules, index: ModuleIndex) *const ModuleView {
        return &self.views.items[@backingInt(index)];
    }

    pub fn byKey(self: *const Modules, key: checked.ModuleId) *const ModuleView {
        const index = self.by_key.get(key.bytes) orelse
            invariant("checked data named a module outside this lowering");
        return self.get(index);
    }
};

/// A label (record field or tag name), interned program-wide by text.
pub const NameId = enum(u32) { _ };

/// An interned type node.
pub const TypeId = enum(u32) { _ };

/// A nominal declaration: its module and its id in that module's checked
/// type store.
pub const DeclRef = struct {
    module: ModuleIndex,
    decl: checked.CheckedNominalDeclarationId,
};

pub const Primitive = checked.CheckedPrimitive;

/// How a record field is stored (design.md "Field Kinds").
pub const FieldKind = enum(u8) {
    /// An inline slot (`required` and `defaulted` fields).
    inline_slot,
    /// A `[#Missing, #Present(value)]` presence slot (`optional` fields and
    /// fields whose kind is a scheme's undetermined kind variable).
    presence_slot,
};

/// A field of a record type, by the record's type and the field's name.
pub const FieldDefaultKey = struct { record: TypeId, name: NameId };

pub const Field = struct {
    name: NameId,
    ty: TypeId,
    kind: FieldKind,
};

pub const Tag = struct {
    name: NameId,
    payloads: Span,
};

/// A range of `TypeId`s in `Store.type_ids`.
pub const Span = struct { start: u32, len: u32 };
/// A range of fields in `Store.fields`.
pub const FieldSpan = struct { start: u32, len: u32 };
/// A range of tags in `Store.tags`.
pub const TagSpan = struct { start: u32, len: u32 };

/// A named type: a nominal or opaque declaration instantiated at `args`.
pub const Nominal = struct {
    decl: DeclRef,
    args: Span,
    /// The type's name, for `Str.inspect` of an override-free opaque.
    name: NameId,
    is_opaque: bool,
    /// The builtin this declaration is, if any.
    builtin: ?checked.CheckedBuiltinNominal,
    /// Whether the declaration has a checked backing. An opaque type with
    /// no backing is represented only by its own layout rules.
    has_backing: bool,
};

pub const Node = union(enum) {
    primitive: Primitive,
    /// The empty record (and every other zero-field record).
    empty_record,
    /// A record whose fields are sorted by label text. `ext` is a variable
    /// node standing for the row's unnamed remainder, or null for a closed
    /// record.
    record: struct { fields: FieldSpan, ext: ?TypeId },
    tuple: Span,
    /// A tag union whose tags are sorted by label text. `ext` as for records.
    tag_union: struct { tags: TagSpan, ext: ?TypeId },
    list: TypeId,
    box: TypeId,
    nominal: Nominal,
    /// A function value, which every strategy stores as an erased callable
    /// under `.boxy`.
    function: struct { args: Span, ret: TypeId, effectful: bool },
    /// Type variable `index` of the converting worker's scope: its
    /// descriptor is that worker's hidden parameter (or capture) `index`.
    variable: u32,
};

/// A binding of checked type variables to nodes, used while converting:
/// a worker's scheme variables to `variable` nodes, or a declaration's
/// formals to an instantiation's arguments.
pub const Env = struct {
    module: ModuleIndex,
    vars: []const checked.CheckedTypeId,
    bound: []const TypeId,

    pub const empty_bindings: []const checked.CheckedTypeId = &.{};

    pub fn lookup(self: Env, module: ModuleIndex, id: checked.CheckedTypeId) ?TypeId {
        if (module != self.module) return null;
        for (self.vars, 0..) |variable, index| {
            if (variable == id) return self.bound[index];
        }
        return null;
    }
};

/// The program-wide type store.
pub const Store = struct {
    allocator: Allocator,
    nodes: std.ArrayList(Node) = .empty,
    /// Whether each node's representation is independent of every type
    /// variable: no `variable` node is reachable from it except through a
    /// function type, whose values are erased callables.
    closed_flags: std.ArrayList(bool) = .empty,
    type_ids: std.ArrayList(TypeId) = .empty,
    fields: std.ArrayList(Field) = .empty,
    tags: std.ArrayList(Tag) = .empty,
    interned: std.HashMapUnmanaged(InternKey, TypeId, InternContext, std.hash_map.default_max_load_percentage) = .empty,
    key_bytes: std.ArrayList(u8) = .empty,
    scratch_key: std.ArrayList(u8) = .empty,

    name_texts: std.ArrayList([]const u8) = .empty,
    name_ids: std.StringHashMapUnmanaged(NameId) = .empty,
    /// Each nominal declaration's name, its own whatever its text: a
    /// descriptor compares nominals by name, and two declarations can share
    /// a name's text.
    decl_name_ids: std.AutoHashMapUnmanaged(DeclRef, NameId) = .empty,
    name_arena: std.heap.ArenaAllocator,

    /// Converted closed checked types, by module and checked id.
    closed_memo: std.AutoHashMapUnmanaged(ClosedKey, TypeId) = .empty,
    /// The backing of each instantiated nominal node.
    backing_memo: std.AutoHashMapUnmanaged(TypeId, TypeId) = .empty,
    /// The `??` default identity of each defaulted field of a record type
    /// conversion produced (design.md "Defaulted Fields").
    field_defaults: std.AutoHashMapUnmanaged(FieldDefaultKey, checked.CheckedFieldDefault) = .empty,
    /// `Converter.representationClosed` of each type asked about.
    representation_closed: std.AutoHashMapUnmanaged(TypeId, bool) = .empty,
    /// Each module's nominal declarations by nominal key, filled once per
    /// module on first use.
    decl_by_key: std.AutoHashMapUnmanaged(DeclKey, checked.CheckedNominalDeclarationId) = .empty,
    decl_keys_indexed: std.AutoHashMapUnmanaged(ModuleIndex, void) = .empty,

    const DeclKey = struct { module: ModuleIndex, key: names.NominalTypeKey };

    const ClosedKey = struct { module: ModuleIndex, ty: checked.CheckedTypeId };

    pub const InternKey = struct { start: u32, len: u32 };

    const InternContext = struct {
        bytes: *const std.ArrayList(u8),

        pub fn hash(ctx: InternContext, key: InternKey) u64 {
            return std.hash.Wyhash.hash(0, ctx.bytes.items[key.start..][0..key.len]);
        }

        pub fn eql(ctx: InternContext, a: InternKey, b: InternKey) bool {
            return std.mem.eql(u8, ctx.bytes.items[a.start..][0..a.len], ctx.bytes.items[b.start..][0..b.len]);
        }
    };

    const InternAdapter = struct {
        bytes: *const std.ArrayList(u8),

        pub fn hash(_: InternAdapter, key: []const u8) u64 {
            return std.hash.Wyhash.hash(0, key);
        }

        pub fn eql(ctx: InternAdapter, key: []const u8, stored: InternKey) bool {
            return std.mem.eql(u8, key, ctx.bytes.items[stored.start..][0..stored.len]);
        }
    };

    pub fn init(allocator: Allocator) Store {
        return .{ .allocator = allocator, .name_arena = std.heap.ArenaAllocator.init(allocator) };
    }

    pub fn deinit(self: *Store) void {
        self.nodes.deinit(self.allocator);
        self.closed_flags.deinit(self.allocator);
        self.type_ids.deinit(self.allocator);
        self.fields.deinit(self.allocator);
        self.tags.deinit(self.allocator);
        self.interned.deinit(self.allocator);
        self.key_bytes.deinit(self.allocator);
        self.scratch_key.deinit(self.allocator);
        self.name_texts.deinit(self.allocator);
        self.name_ids.deinit(self.allocator);
        self.decl_name_ids.deinit(self.allocator);
        self.name_arena.deinit();
        self.closed_memo.deinit(self.allocator);
        self.backing_memo.deinit(self.allocator);
        self.representation_closed.deinit(self.allocator);
        self.field_defaults.deinit(self.allocator);
        self.decl_by_key.deinit(self.allocator);
        self.decl_keys_indexed.deinit(self.allocator);
    }

    /// The declaration with nominal key `key` in `view`'s type store.
    pub fn declarationByKey(self: *Store, view: *const ModuleView, key: names.NominalTypeKey) Allocator.Error!?checked.CheckedNominalDeclarationId {
        if (!self.decl_keys_indexed.contains(view.index)) {
            try self.decl_keys_indexed.put(self.allocator, view.index, {});
            for (view.types().nominal_declarations) |declaration| {
                try self.decl_by_key.put(self.allocator, .{ .module = view.index, .key = declaration.nominal }, declaration.id);
            }
        }
        return self.decl_by_key.get(.{ .module = view.index, .key = key });
    }

    // -- Names --

    pub fn internName(self: *Store, text: []const u8) Allocator.Error!NameId {
        if (self.name_ids.get(text)) |existing| return existing;
        const owned = try self.name_arena.allocator().dupe(u8, text);
        const id: NameId = @fromBackingInt(@intCast(self.name_texts.items.len));
        try self.name_texts.append(self.allocator, owned);
        try self.name_ids.put(self.allocator, owned, id);
        return id;
    }

    /// The name of nominal declaration `decl`, whose text is `text`.
    pub fn internDeclName(self: *Store, decl: DeclRef, text: []const u8) Allocator.Error!NameId {
        if (self.decl_name_ids.get(decl)) |existing| return existing;
        const owned = try self.name_arena.allocator().dupe(u8, text);
        const id: NameId = @fromBackingInt(@intCast(self.name_texts.items.len));
        try self.name_texts.append(self.allocator, owned);
        try self.decl_name_ids.put(self.allocator, decl, id);
        return id;
    }

    pub fn nameText(self: *const Store, id: NameId) []const u8 {
        return self.name_texts.items[@backingInt(id)];
    }

    pub fn nameCount(self: *const Store) usize {
        return self.name_texts.items.len;
    }

    pub fn nameLessThan(self: *const Store, a: NameId, b: NameId) bool {
        return base.Ident.textLessThan(self.nameText(a), self.nameText(b));
    }

    // -- Nodes --

    pub fn get(self: *const Store, id: TypeId) Node {
        return self.nodes.items[@backingInt(id)];
    }

    pub fn isClosed(self: *const Store, id: TypeId) bool {
        return self.closed_flags.items[@backingInt(id)];
    }

    pub fn span(self: *const Store, value: Span) []const TypeId {
        return self.type_ids.items[value.start..][0..value.len];
    }

    pub fn fieldSpan(self: *const Store, value: FieldSpan) []const Field {
        return self.fields.items[value.start..][0..value.len];
    }

    pub fn tagSpan(self: *const Store, value: TagSpan) []const Tag {
        return self.tags.items[value.start..][0..value.len];
    }

    pub fn nominalArgs(self: *const Store, nominal: Nominal) []const TypeId {
        return self.span(nominal.args);
    }

    fn keyU32(self: *Store, value: u32) Allocator.Error!void {
        try self.scratch_key.appendSlice(self.allocator, std.mem.asBytes(&value));
    }

    fn keyType(self: *Store, id: TypeId) Allocator.Error!void {
        try self.keyU32(@backingInt(id));
    }

    fn keySpan(self: *Store, ids: []const TypeId) Allocator.Error!void {
        try self.keyU32(@intCast(ids.len));
        for (ids) |id| try self.keyType(id);
    }

    /// Intern a node whose children are already interned. Span-bearing
    /// nodes pass their children as slices; the store copies them.
    pub fn intern(self: *Store, node: NodeInput) Allocator.Error!TypeId {
        self.scratch_key.clearRetainingCapacity();
        try self.scratch_key.append(self.allocator, @backingInt(std.meta.activeTag(node)));
        var closed = true;
        switch (node) {
            .primitive => |prim| try self.scratch_key.append(self.allocator, @backingInt(prim)),
            .empty_record => {},
            .record => |record| {
                try self.keyU32(@intCast(record.fields.len));
                for (record.fields) |field| {
                    try self.keyU32(@backingInt(field.name));
                    try self.keyType(field.ty);
                    try self.scratch_key.append(self.allocator, @backingInt(field.kind));
                    closed = closed and self.isClosed(field.ty);
                }
                try self.keyU32(if (record.ext) |ext| @backingInt(ext) else std.math.maxInt(u32));
                if (record.ext) |ext| closed = closed and self.isClosed(ext);
            },
            .tuple => |items| {
                try self.keySpan(items);
                for (items) |item| closed = closed and self.isClosed(item);
            },
            .tag_union => |tag_union| {
                try self.keyU32(@intCast(tag_union.tags.len));
                for (tag_union.tags) |tag| {
                    try self.keyU32(@backingInt(tag.name));
                    try self.keySpan(tag.payloads);
                    for (tag.payloads) |payload| closed = closed and self.isClosed(payload);
                }
                try self.keyU32(if (tag_union.ext) |ext| @backingInt(ext) else std.math.maxInt(u32));
                if (tag_union.ext) |ext| closed = closed and self.isClosed(ext);
            },
            .list => |elem| {
                try self.keyType(elem);
                closed = self.isClosed(elem);
            },
            .box => |payload| {
                try self.keyType(payload);
                closed = self.isClosed(payload);
            },
            .nominal => |nominal| {
                try self.keyU32(@backingInt(nominal.decl.module));
                try self.keyU32(@backingInt(nominal.decl.decl));
                try self.keySpan(nominal.args);
                for (nominal.args) |arg| closed = closed and self.isClosed(arg);
            },
            .function => |function| {
                try self.keySpan(function.args);
                try self.keyType(function.ret);
                try self.scratch_key.append(self.allocator, @intFromBool(function.effectful));
                // Every function value is an erased callable whatever its
                // argument and result types, so its representation is closed.
            },
            .variable => |index| {
                try self.keyU32(index);
                closed = false;
            },
        }
        const gop = try self.interned.getOrPutContextAdapted(
            self.allocator,
            @as([]const u8, self.scratch_key.items),
            InternAdapter{ .bytes = &self.key_bytes },
            InternContext{ .bytes = &self.key_bytes },
        );
        if (gop.found_existing) return gop.value_ptr.*;
        const key_start: u32 = @intCast(self.key_bytes.items.len);
        try self.key_bytes.appendSlice(self.allocator, self.scratch_key.items);
        gop.key_ptr.* = .{ .start = key_start, .len = @intCast(self.scratch_key.items.len) };
        const id: TypeId = @fromBackingInt(@intCast(self.nodes.items.len));
        gop.value_ptr.* = id;
        const stored: Node = switch (node) {
            .primitive => |prim| .{ .primitive = prim },
            .empty_record => .empty_record,
            .record => |record| .{ .record = .{ .fields = try self.addFields(record.fields), .ext = record.ext } },
            .tuple => |items| .{ .tuple = try self.addSpan(items) },
            .tag_union => |tag_union| .{ .tag_union = .{ .tags = try self.addTags(tag_union.tags), .ext = tag_union.ext } },
            .list => |elem| .{ .list = elem },
            .box => |payload| .{ .box = payload },
            .nominal => |nominal| .{ .nominal = .{
                .decl = nominal.decl,
                .args = try self.addSpan(nominal.args),
                .name = nominal.name,
                .is_opaque = nominal.is_opaque,
                .builtin = nominal.builtin,
                .has_backing = nominal.has_backing,
            } },
            .function => |function| .{ .function = .{ .args = try self.addSpan(function.args), .ret = function.ret, .effectful = function.effectful } },
            .variable => |index| .{ .variable = index },
        };
        try self.nodes.append(self.allocator, stored);
        try self.closed_flags.append(self.allocator, closed);
        return id;
    }

    fn addSpan(self: *Store, ids: []const TypeId) Allocator.Error!Span {
        const start: u32 = @intCast(self.type_ids.items.len);
        try self.type_ids.appendSlice(self.allocator, ids);
        return .{ .start = start, .len = @intCast(ids.len) };
    }

    fn addFields(self: *Store, fields: []const Field) Allocator.Error!FieldSpan {
        const start: u32 = @intCast(self.fields.items.len);
        try self.fields.appendSlice(self.allocator, fields);
        return .{ .start = start, .len = @intCast(fields.len) };
    }

    fn addTags(self: *Store, tags: []const TagInput) Allocator.Error!TagSpan {
        const start: u32 = @intCast(self.tags.items.len);
        for (tags) |tag| {
            const payloads = try self.addSpan(tag.payloads);
            try self.tags.append(self.allocator, .{ .name = tag.name, .payloads = payloads });
        }
        return .{ .start = start, .len = @intCast(tags.len) };
    }

    pub fn primitive(self: *Store, value: Primitive) Allocator.Error!TypeId {
        return self.intern(.{ .primitive = value });
    }

    pub fn variable(self: *Store, index: u32) Allocator.Error!TypeId {
        return self.intern(.{ .variable = index });
    }

    pub fn emptyRecord(self: *Store) Allocator.Error!TypeId {
        return self.intern(.empty_record);
    }

    /// The empty tag union: a type with no values.
    pub fn emptyTagUnion(self: *Store) Allocator.Error!TypeId {
        return self.intern(.{ .tag_union = .{ .tags = &.{}, .ext = null } });
    }
};

pub const TagInput = struct {
    name: NameId,
    payloads: []const TypeId,
};

/// A node to intern, with its children as slices.
pub const NodeInput = union(enum) {
    primitive: Primitive,
    empty_record,
    record: struct { fields: []const Field, ext: ?TypeId },
    tuple: []const TypeId,
    tag_union: struct { tags: []const TagInput, ext: ?TypeId },
    list: TypeId,
    box: TypeId,
    nominal: struct {
        decl: DeclRef,
        args: []const TypeId,
        name: NameId,
        is_opaque: bool,
        builtin: ?checked.CheckedBuiltinNominal,
        has_backing: bool,
    },
    function: struct { args: []const TypeId, ret: TypeId, effectful: bool },
    variable: u32,
};

pub fn invariant(comptime message: []const u8) noreturn {
    base.invariant("Boxy lowering invariant violated: " ++ message, .{});
}

pub fn invariantFmt(comptime fmt: []const u8, args: anytype) noreturn {
    base.invariant("Boxy lowering invariant violated: " ++ fmt, args);
}

/// Converts checked types to store nodes with an explicit work stack.
pub const Converter = struct {
    store: *Store,
    modules: *const Modules,
    /// For row-join seeding: the scheme variables of every scope of the
    /// module being seeded. A variable among them is a scheme variable in the
    /// body lowering converts it in, so it seeds as a variable, keeping the
    /// types it occurs in out of every ground join set.
    seed_scheme_vars: ?*const std.AutoHashMapUnmanaged(checked.CheckedTypeId, void) = null,
    /// Whether an undetermined field kind outside every scheme commits to
    /// `required` (a generated codec contract's snapshot, whose kinds no
    /// caller can pin), rather than keeping its presence-slot representation.
    seal_undetermined_kinds: bool = false,
    /// The program's solution of each platform requirement identity
    /// variable: the app type the platform's requirement takes it to be.
    platform_identities: ?*const std.AutoHashMapUnmanaged(IdentityKey, TypeId) = null,

    /// A checked type of a module.
    pub const IdentityKey = struct { module: ModuleIndex, ty: checked.CheckedTypeId };

    /// The variable a seeding conversion gives a scheme variable.
    pub const seed_variable_index: u32 = std.math.maxInt(u32);

    const Frame = struct {
        module: ModuleIndex,
        ty: checked.CheckedTypeId,
        /// The children converted so far, in `results`.
        results_start: usize,
        /// Children still to convert, in `pending`.
        pending_start: usize,
        pending_next: usize,
    };

    /// Convert `ty`, read in `module`, binding the variables `env` names.
    /// A variable `env` does not bind must seal to its row default or its
    /// numeric default; any other unbound variable is an invariant failure.
    pub fn convert(self: Converter, module: ModuleIndex, ty: checked.CheckedTypeId, env: Env) Allocator.Error!TypeId {
        const allocator = self.store.allocator;
        var frames: std.ArrayList(Frame) = .empty;
        defer frames.deinit(allocator);
        var results: std.ArrayList(TypeId) = .empty;
        defer results.deinit(allocator);
        var pending: std.ArrayList(Child) = .empty;
        defer pending.deinit(allocator);

        if (try self.leaf(module, ty, env)) |done| return done;
        try self.pushFrame(&frames, &pending, results.items.len, module, ty);
        while (frames.items.len > 0) {
            const top = &frames.items[frames.items.len - 1];
            if (top.pending_next < pending.items.len) {
                const child = pending.items[top.pending_next];
                top.pending_next += 1;
                if (try self.leaf(child.module, child.ty, env)) |done| {
                    try results.append(allocator, done);
                } else {
                    if (frames.items.len > 1_000_000) invariant("checked type conversion nested past every finite type");
                    try self.pushFrame(&frames, &pending, results.items.len, child.module, child.ty);
                }
                continue;
            }
            const frame = frames.pop().?;
            const children = results.items[frame.results_start..];
            const built = try self.finish(frame.module, frame.ty, children, env);
            results.shrinkRetainingCapacity(frame.results_start);
            pending.shrinkRetainingCapacity(frame.pending_start);
            // A type with no identity variables converts the same under
            // every environment; a seeding conversion, which keeps scheme
            // variables apart, is not a lowering conversion.
            if (!self.seal_undetermined_kinds and self.seed_scheme_vars == null and !rootHasIdentityVariables(self.modules.get(frame.module), frame.ty)) {
                try self.store.closed_memo.put(allocator, .{ .module = frame.module, .ty = frame.ty }, built);
            }
            if (frames.items.len == 0) return built;
            try results.append(allocator, built);
        }
        unreachable; // the loop returns when the outermost frame finishes
    }

    const Child = struct { module: ModuleIndex, ty: checked.CheckedTypeId };

    fn rootHasIdentityVariables(view: *const ModuleView, ty: checked.CheckedTypeId) bool {
        const roots = view.types().rootRows();
        const index: usize = @backingInt(ty);
        if (index >= roots.len) return true;
        return roots[index].contains_identity_variables;
    }

    /// The node for `ty` when it needs no children converted first.
    fn leaf(self: Converter, module: ModuleIndex, ty: checked.CheckedTypeId, env: Env) Allocator.Error!?TypeId {
        if (env.lookup(module, ty)) |bound| return bound;
        if (self.platform_identities) |identities| {
            if (identities.get(.{ .module = module, .ty = ty })) |solved| return solved;
        }
        const view = self.modules.get(module);
        if (!self.seal_undetermined_kinds and !rootHasIdentityVariables(view, ty)) {
            if (self.store.closed_memo.get(.{ .module = module, .ty = ty })) |memo| return memo;
        }
        return switch (view.types().payload(ty)) {
            .pending => invariant("a pending checked type reached Boxy lowering"),
            .err => if (self.seed_scheme_vars != null)
                // A rejected type is never lowered; it joins nothing.
                try self.store.variable(seed_variable_index)
            else
                invariant("an erroneous checked type reached Boxy lowering"),
            .flex, .rigid => |variable| if (self.seed_scheme_vars) |vars| (if (vars.contains(ty)) try self.store.variable(seed_variable_index) else try self.sealVariable(variable)) else try self.sealVariable(variable),
            .empty_record => try self.store.emptyRecord(),
            .empty_tag_union => try self.store.emptyTagUnion(),
            .nominal => |nominal| switch (nominal.representation) {
                .builtin => |builtin_nominal| switch (checked.builtinRuntimeEncoding(builtin_nominal)) {
                    .primitive => |prim| try self.store.primitive(prim),
                    else => null,
                },
                else => null,
            },
            .alias, .record, .tuple, .function, .tag_union => null,
        };
    }

    /// The type a scheme variable takes where nothing relates it to a use:
    /// what it seals to when no scope binds it.
    pub fn sealedScheme(self: Converter, module: ModuleIndex, variable: checked.CheckedTypeId) Allocator.Error!TypeId {
        return switch (self.modules.get(module).types().payload(variable)) {
            .flex, .rigid => |payload| try self.sealVariable(payload),
            else => try self.convert(module, variable, .{ .module = module, .vars = &.{}, .bound = &.{} }),
        };
    }

    /// Whether the checked type `root` holds the erroneous type anywhere:
    /// the type of a value checking rejected, which no lowering converts.
    pub fn hasError(self: Converter, module: ModuleIndex, root: checked.CheckedTypeId) Allocator.Error!bool {
        const allocator = self.store.allocator;
        const types = self.modules.get(module).types();
        var seen: std.AutoHashMapUnmanaged(checked.CheckedTypeId, void) = .empty;
        defer seen.deinit(allocator);
        var pending: std.ArrayList(checked.CheckedTypeId) = .empty;
        defer pending.deinit(allocator);
        try pending.append(allocator, root);
        while (pending.pop()) |ty| {
            if ((try seen.getOrPut(allocator, ty)).found_existing) continue;
            switch (types.payload(ty)) {
                .err => return true,
                .alias => |alias| try pending.append(allocator, alias.backing),
                .record => |record| {
                    for (record.fields) |field| try pending.append(allocator, field.ty);
                    try pending.append(allocator, record.ext);
                },
                .tuple => |items| try pending.appendSlice(allocator, items),
                .tag_union => |tag_union| {
                    for (tag_union.tags) |tag| try pending.appendSlice(allocator, tag.argsSlice(types));
                    try pending.append(allocator, tag_union.ext);
                },
                .function => |function| {
                    try pending.appendSlice(allocator, function.args);
                    try pending.append(allocator, function.ret);
                },
                .nominal => |nominal| try pending.appendSlice(allocator, nominal.args),
                .pending, .flex, .rigid, .empty_record, .empty_tag_union => {},
            }
        }
        return false;
    }

    /// A variable the converting scope does not bind: it seals to its row
    /// default or its numeric default (`CheckedTypePayload.variableSealsToRowDefault`).
    fn sealVariable(self: Converter, variable: checked.CheckedTypeVariable) Allocator.Error!TypeId {
        if (variable.numeric_default_phase) |phase| {
            const target = checked.literal_defaulting.defaultTargetForPhase(phase) orelse
                invariant("a checking-finalized numeric variable reached Boxy lowering unresolved");
            return switch (target) {
                .dec => try self.store.primitive(.dec),
                .str => try self.store.primitive(.str),
            };
        }
        if (variable.row_default) |row_default| return switch (row_default) {
            .empty_record => try self.store.emptyRecord(),
            .empty_tag_union => try self.store.emptyTagUnion(),
        };
        // An unconstrained variable no scope binds has no values reachable at
        // it; its type is the empty tag union, as every other strategy gives it.
        return try self.store.emptyTagUnion();
    }

    fn pushFrame(self: Converter, frames: *std.ArrayList(Frame), pending: *std.ArrayList(Child), results_start: usize, module: ModuleIndex, ty: checked.CheckedTypeId) Allocator.Error!void {
        const allocator = self.store.allocator;
        const pending_start = pending.items.len;
        const view = self.modules.get(module);
        const types = view.types();
        switch (types.payload(ty)) {
            .alias => |alias| try pending.append(allocator, .{ .module = module, .ty = alias.backing }),
            .record => |record| {
                var current_fields = record.fields;
                var current = record.ext;
                while (true) {
                    for (current_fields) |field| try pending.append(allocator, .{ .module = module, .ty = field.ty });
                    current_fields = &.{};
                    switch (types.payload(current)) {
                        .alias => |alias| current = alias.backing,
                        .record => |ext_record| {
                            current_fields = ext_record.fields;
                            current = ext_record.ext;
                        },
                        else => break,
                    }
                }
                // The row's remainder, when it is not the empty record.
                switch (types.payload(current)) {
                    .empty_record => {},
                    else => try pending.append(allocator, .{ .module = module, .ty = current }),
                }
            },
            .tuple => |items| for (items) |item| try pending.append(allocator, .{ .module = module, .ty = item }),
            .tag_union => |tag_union| {
                var current_tags = tag_union.tags;
                var current = tag_union.ext;
                while (true) {
                    for (current_tags) |tag| {
                        for (tag.argsSlice(types)) |arg| try pending.append(allocator, .{ .module = module, .ty = arg });
                    }
                    current_tags = &.{};
                    switch (types.payload(current)) {
                        .alias => |alias| current = alias.backing,
                        .tag_union => |ext_union| {
                            current_tags = ext_union.tags;
                            current = ext_union.ext;
                        },
                        else => break,
                    }
                }
                switch (types.payload(current)) {
                    .empty_tag_union => {},
                    else => try pending.append(allocator, .{ .module = module, .ty = current }),
                }
            },
            .function => |function| {
                for (function.args) |arg| try pending.append(allocator, .{ .module = module, .ty = arg });
                try pending.append(allocator, .{ .module = module, .ty = function.ret });
            },
            .nominal => |nominal| for (nominal.args) |arg| try pending.append(allocator, .{ .module = module, .ty = arg }),
            .pending, .err, .flex, .rigid, .empty_record, .empty_tag_union => unreachable, // `leaf` answers these
        }
        try frames.append(allocator, .{
            .module = module,
            .ty = ty,
            .results_start = results_start,
            .pending_start = pending_start,
            .pending_next = pending_start,
        });
    }

    fn finish(self: Converter, module: ModuleIndex, ty: checked.CheckedTypeId, children: []const TypeId, env: Env) Allocator.Error!TypeId {
        _ = env;
        const allocator = self.store.allocator;
        const view = self.modules.get(module);
        const types = view.types();
        const name_store = view.nameStore();
        switch (types.payload(ty)) {
            .alias => return children[0],
            .tuple => return try self.store.intern(.{ .tuple = children }),
            .function => |function| return try self.store.intern(.{ .function = .{
                .args = children[0 .. children.len - 1],
                .ret = children[children.len - 1],
                .effectful = checked.finalizedFunctionKind(function.kind) == .effectful,
            } }),
            .record => |record| {
                var fields: std.ArrayList(Field) = .empty;
                defer fields.deinit(allocator);
                var defaults: std.ArrayList(struct { name: NameId, default: checked.CheckedFieldDefault }) = .empty;
                defer defaults.deinit(allocator);
                var child_index: usize = 0;
                var current_fields = record.fields;
                var current = record.ext;
                while (true) {
                    for (current_fields) |field| {
                        const field_name = try self.store.internName(name_store.recordFieldLabelText(field.name));
                        try fields.append(allocator, .{
                            .name = field_name,
                            .ty = children[child_index],
                            .kind = self.fieldKind(field.kind),
                        });
                        if (field.kind.tag == .defaulted) try defaults.append(allocator, .{ .name = field_name, .default = field.kind.default });
                        child_index += 1;
                    }
                    current_fields = &.{};
                    switch (types.payload(current)) {
                        .alias => |alias| current = alias.backing,
                        .record => |ext_record| {
                            current_fields = ext_record.fields;
                            current = ext_record.ext;
                        },
                        else => break,
                    }
                }
                var ext: ?TypeId = switch (types.payload(current)) {
                    .empty_record => null,
                    else => children[child_index],
                };
                // A remainder that converted to a closed record joins this row.
                if (ext) |ext_ty| switch (self.store.get(ext_ty)) {
                    .empty_record => ext = null,
                    .record => |ext_record| if (ext_record.ext == null) {
                        try fields.appendSlice(allocator, self.store.fieldSpan(ext_record.fields));
                        ext = null;
                    },
                    .variable => {},
                    else => invariant("a record row's remainder converted to a non-record type"),
                };
                if (fields.items.len == 0 and ext == null) return try self.store.emptyRecord();
                const store = self.store;
                std.mem.sort(Field, fields.items, store, struct {
                    fn lessThan(s: *Store, a: Field, b: Field) bool {
                        return s.nameLessThan(a.name, b.name);
                    }
                }.lessThan);
                const record_ty = try self.store.intern(.{ .record = .{ .fields = fields.items, .ext = ext } });
                for (defaults.items) |entry| try self.store.field_defaults.put(allocator, .{ .record = record_ty, .name = entry.name }, entry.default);
                return record_ty;
            },
            .tag_union => |tag_union| {
                var tags: std.ArrayList(TagInput) = .empty;
                defer tags.deinit(allocator);
                var child_index: usize = 0;
                var current_tags = tag_union.tags;
                var current = tag_union.ext;
                while (true) {
                    for (current_tags) |tag| {
                        const count = tag.argsSlice(types).len;
                        try tags.append(allocator, .{
                            .name = try self.store.internName(name_store.tagLabelText(tag.name)),
                            .payloads = children[child_index..][0..count],
                        });
                        child_index += count;
                    }
                    current_tags = &.{};
                    switch (types.payload(current)) {
                        .alias => |alias| current = alias.backing,
                        .tag_union => |ext_union| {
                            current_tags = ext_union.tags;
                            current = ext_union.ext;
                        },
                        else => break,
                    }
                }
                var ext: ?TypeId = switch (types.payload(current)) {
                    .empty_tag_union => null,
                    else => children[child_index],
                };
                var ext_payload_copies: std.ArrayList(TypeId) = .empty;
                defer ext_payload_copies.deinit(allocator);
                if (ext) |ext_ty| switch (self.store.get(ext_ty)) {
                    .tag_union => |ext_union| {
                        if (ext_union.ext == null) {
                            for (self.store.tagSpan(ext_union.tags)) |tag| {
                                try tags.append(allocator, .{ .name = tag.name, .payloads = self.store.span(tag.payloads) });
                            }
                            ext = null;
                        }
                    },
                    .variable => {},
                    else => invariant("a tag row's remainder converted to a non-tag type"),
                };
                const store = self.store;
                std.mem.sort(TagInput, tags.items, store, struct {
                    fn lessThan(s: *Store, a: TagInput, b: TagInput) bool {
                        return s.nameLessThan(a.name, b.name);
                    }
                }.lessThan);
                return try self.store.intern(.{ .tag_union = .{ .tags = tags.items, .ext = ext } });
            },
            .nominal => |nominal| return try self.nominalNode(module, nominal, children),
            .pending, .err, .flex, .rigid, .empty_record, .empty_tag_union => unreachable, // `leaf` answers these
        }
    }

    /// The type of the nominal `ty` of `module` names (through aliases),
    /// applied to `args` rather than to its own arguments.
    pub fn nominalAt(self: Converter, module: ModuleIndex, ty: checked.CheckedTypeId, args: []const TypeId) Allocator.Error!TypeId {
        const types = self.modules.get(module).types();
        var current = ty;
        while (true) switch (types.payload(current)) {
            .alias => |alias| current = alias.backing,
            .nominal => |nominal| {
                if (nominal.representation == .builtin) switch (checked.builtinRuntimeEncoding(nominal.representation.builtin)) {
                    .primitive => |prim| return try self.store.primitive(prim),
                    else => {},
                };
                return try self.nominalNode(module, nominal, args);
            },
            else => invariant("a stored named type's occurrence is not a nominal type"),
        };
    }

    fn nominalNode(self: Converter, module: ModuleIndex, nominal: @FieldType(checked.CheckedTypePayload, "nominal"), children: []const TypeId) Allocator.Error!TypeId {
        const view = self.modules.get(module);
        const name_store = view.nameStore();
        {
            {
                switch (nominal.representation) {
                    .builtin => |builtin_nominal| switch (checked.builtinRuntimeEncoding(builtin_nominal)) {
                        .list => return try self.store.intern(.{ .list = children[0] }),
                        .box => return try self.store.intern(.{ .box = children[0] }),
                        .primitive => unreachable, // `leaf` answers primitive builtins
                        // Generated codecs' serialization evidence: a field
                        // set is the list of its field handles, and a
                        // tag-union spec is the callable that parses its
                        // union.
                        .fields => return try self.store.intern(.{ .list = try self.fieldHandle() }),
                        .field => return try self.fieldHandle(),
                        .parse_tag_union_spec => return try self.store.intern(.{ .function = .{ .args = &.{}, .ret = children[0], .effectful = false } }),
                        else => {},
                    },
                    else => {},
                }
                const decl = try self.declarationRef(view, nominal);
                const decl_ref = decl orelse DeclRef{ .module = module, .decl = @fromBackingInt(std.math.maxInt(u32)) };
                return try self.store.intern(.{ .nominal = .{
                    .decl = decl_ref,
                    .args = children,
                    .name = if (decl != null) try self.store.internDeclName(decl_ref, name_store.typeNameText(nominal.name)) else try self.store.internName(name_store.typeNameText(nominal.name)),
                    .is_opaque = nominal.is_opaque,
                    .builtin = nominal.builtin,
                    .has_backing = decl != null,
                } });
            }
        }
    }

    /// The representation of `Encoding.FieldName`: a record field's
    /// position in its record, its (renamed) name, and the name's length.
    pub fn fieldHandle(self: Converter) Allocator.Error!TypeId {
        const u64_ty = try self.store.primitive(.u64);
        var fields = [_]Field{
            .{ .name = try self.store.internName("index"), .ty = u64_ty, .kind = .inline_slot },
            .{ .name = try self.store.internName("name"), .ty = try self.store.primitive(.str), .kind = .inline_slot },
            .{ .name = try self.store.internName("name_len"), .ty = u64_ty, .kind = .inline_slot },
        };
        std.mem.sort(Field, &fields, self.store, struct {
            fn lessThan(s: *Store, a: Field, b: Field) bool {
                return s.nameLessThan(a.name, b.name);
            }
        }.lessThan);
        return try self.store.intern(.{ .record = .{ .fields = &fields, .ext = null } });
    }

    fn fieldKind(self: Converter, kind: checked.CheckedFieldKind) FieldKind {
        return switch (kind.tag) {
            .required, .defaulted => .inline_slot,
            .optional => .presence_slot,
            .undetermined => if (self.seal_undetermined_kinds) .inline_slot else .presence_slot,
            .err => invariant("an erroneous record field kind reached Boxy lowering"),
        };
    }

    /// The declaration a nominal payload names, or null for an opaque type
    /// with no backing.
    fn declarationRef(self: Converter, view: *const ModuleView, nominal: checked.CheckedNominalType) Allocator.Error!?DeclRef {
        return switch (nominal.representation) {
            .opaque_without_backing => null,
            .local_declaration => |decl| .{ .module = view.index, .decl = decl },
            .imported_declaration => |imported| .{ .module = self.modules.byKey(imported.artifact).index, .decl = imported.declaration },
            .builtin => |builtin_nominal| blk: {
                if (!checked.builtinNominalHasDeclarationBacking(builtin_nominal)) {
                    invariantFmt("builtin nominal {s} has no declaration backing yet reached nominal conversion", .{@tagName(builtin_nominal)});
                }
                const owner = self.modules.byKey(nominal.owner_module.*);
                const declaration = owner.types().builtinNominalDeclaration(builtin_nominal) orelse
                    invariantFmt("module has no declaration for builtin nominal {s}", .{@tagName(builtin_nominal)});
                break :blk .{ .module = owner.index, .decl = declaration.id };
            },
            .local_box_payload_capability => |cap| blk: {
                const capability = view.view.interface_capabilities.boxPayloadCapability(cap.capability);
                const decl = (try self.store.declarationByKey(view, capability.nominal)) orelse
                    invariant("a box payload capability names a nominal its module does not declare");
                break :blk .{ .module = view.index, .decl = decl };
            },
            .imported_box_payload_capability => |cap| blk: {
                const owner = self.modules.byKey(cap.artifact);
                const capability = owner.view.interface_capabilities.boxPayloadCapability(cap.capability);
                const decl = (try self.store.declarationByKey(owner, capability.nominal)) orelse
                    invariant("a box payload capability names a nominal its module does not declare");
                break :blk .{ .module = owner.index, .decl = decl };
            },
        };
    }

    /// The backing node of an instantiated nominal node: the declaration's
    /// checked backing with its formals bound to the node's arguments.
    /// Whether values of `ty` have one committed layout: no type variable is
    /// reachable through the type's representation. A nominal is represented
    /// by its backing, so a nominal argument its backing does not store (a
    /// phantom argument, or one only a function type mentions) leaves the
    /// nominal's representation closed.
    pub fn representationClosed(self: Converter, root: TypeId) Allocator.Error!bool {
        const store = self.store;
        if (store.isClosed(root)) return true;
        if (store.representation_closed.get(root)) |memo| return memo;
        var seen: std.AutoHashMapUnmanaged(TypeId, void) = .empty;
        defer seen.deinit(store.allocator);
        var pending: std.ArrayList(TypeId) = .empty;
        defer pending.deinit(store.allocator);
        try pending.append(store.allocator, root);
        const closed = walk: while (pending.pop()) |ty| {
            if (store.isClosed(ty)) continue;
            if (store.representation_closed.get(ty)) |memo| {
                if (memo) continue else break :walk false;
            }
            if ((try seen.getOrPut(store.allocator, ty)).found_existing) continue;
            switch (store.get(ty)) {
                .variable => break :walk false,
                .primitive, .empty_record, .function => {},
                .record => |record| {
                    for (store.fieldSpan(record.fields)) |field| try pending.append(store.allocator, field.ty);
                    if (record.ext) |ext| try pending.append(store.allocator, ext);
                },
                .tuple => |items| try pending.appendSlice(store.allocator, store.span(items)),
                .tag_union => |tag_union| {
                    for (store.tagSpan(tag_union.tags)) |tag| try pending.appendSlice(store.allocator, store.span(tag.payloads));
                    if (tag_union.ext) |ext| try pending.append(store.allocator, ext);
                },
                .list => |elem| try pending.append(store.allocator, elem),
                .box => |payload| try pending.append(store.allocator, payload),
                .nominal => |nominal| if (nominal.has_backing) {
                    try pending.append(store.allocator, try self.backing(ty));
                } else {
                    try pending.appendSlice(store.allocator, store.nominalArgs(nominal));
                },
            }
        } else true;
        try store.representation_closed.put(store.allocator, root, closed);
        return closed;
    }

    pub fn backing(self: Converter, nominal_ty: TypeId) Allocator.Error!TypeId {
        if (self.store.backing_memo.get(nominal_ty)) |memo| return memo;
        const nominal = switch (self.store.get(nominal_ty)) {
            .nominal => |value| value,
            else => invariant("asked for the backing of a non-nominal type"),
        };
        if (!nominal.has_backing) invariant("asked for the backing of an opaque type without one");
        const view = self.modules.get(nominal.decl.module);
        const declaration = view.types().nominalDeclarationById(nominal.decl.decl);
        const formals = declaration.formalArgs(view.types());
        const args = try self.store.allocator.dupe(TypeId, self.store.nominalArgs(nominal));
        defer self.store.allocator.free(args);
        if (formals.len != args.len) invariant("nominal instantiation arity differs from its declaration");
        const env: Env = .{ .module = nominal.decl.module, .vars = formals, .bound = args };
        // The declaration's formals are identity variables, so no part of
        // the backing that mentions them reaches the closed-type memo.
        const result = try self.convert(nominal.decl.module, declaration.backing, env);
        try self.store.backing_memo.put(self.store.allocator, nominal_ty, result);
        return result;
    }
};

test "interning shares structurally equal nodes" {
    var store = Store.init(std.testing.allocator);
    defer store.deinit();
    const a = try store.primitive(.u64);
    const b = try store.primitive(.u64);
    try std.testing.expectEqual(a, b);
    const list_a = try store.intern(.{ .list = a });
    const list_b = try store.intern(.{ .list = b });
    try std.testing.expectEqual(list_a, list_b);
    try std.testing.expect(store.isClosed(list_a));
    const v = try store.variable(0);
    const list_v = try store.intern(.{ .list = v });
    try std.testing.expect(!store.isClosed(list_v));
}
