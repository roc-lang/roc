//! The types `.boxy` lowering reasons about (design.md "`.boxy` Runtime
//! Lowering").
//!
//! A checked type lives in one module's checked type store and names that
//! module's ids for labels, declarations, and type variables. Boxy lowering
//! needs one program-wide view of the same types: to intern layouts and
//! static descriptors for closed types, to write the descriptor recipes of
//! types that mention a worker's type variables, and to compare the rows a
//! join makes one. This store converts checked types into interned,
//! structural, module-independent nodes. A node that mentions a type variable
//! names it by its position in the converting worker's hidden descriptors, so
//! a node is closed exactly when no `variable` node is reachable from it.
//!
//! Nominal types stay nominal: a node names its declaration and its argument
//! nodes, and the backing node of an instantiated declaration is computed on
//! demand from the declaration's checked backing with its formals bound to
//! the arguments, so a recursive declaration never expands without end.

const std = @import("std");
const collections = @import("collections");
const base = @import("base");
const check = @import("check");
const boxy_runtime = @import("boxy_runtime");

const Allocator = std.mem.Allocator;
const checked = check.CheckedModule;
const static_dispatch = check.StaticDispatchRegistry;
const names = check.CheckedNames;

/// Dense index of a checked module in one lowering.
pub const ModuleIndex = enum(u32) { _ };

/// One checked module's tables, read by every lowering step.
pub const ModuleView = struct {
    index: ModuleIndex,
    key: checked.ModuleId,
    view: checked.ImportedModuleView,
    /// What `Modules.linkName` returns.
    link_name: [32]u8,

    pub fn types(self: *const ModuleView) checked.CheckedTypeStoreView {
        return self.view.checked_types;
    }

    pub fn bodies(self: *const ModuleView) checked.CheckedBodies {
        return self.view.checked_bodies;
    }

    pub fn nameStore(self: *const ModuleView) *const names.CanonicalNameStore {
        return self.view.canonical_names;
    }
};

/// The name each module of one link gives its workers and nominal types
/// (design.md "Caching Under `.boxy`"): what lets a module's object survive an
/// edit to another module's bodies. A module is named by its module name and
/// checking interface, which a body edit leaves alone. Modules with different
/// content can share those, as two versions of a package differing only in
/// bodies do, and they are distinct modules: this holds them, and each is
/// named by its content identity.
pub const ModuleNames = struct {
    content_named: std.AutoHashMapUnmanaged([32]u8, void) = .empty,

    pub fn deinit(self: *ModuleNames, allocator: Allocator) void {
        self.content_named.deinit(allocator);
    }

    pub fn of(self: *const ModuleNames, view: checked.ImportedModuleView) [32]u8 {
        if (self.content_named.contains(view.key.bytes)) return view.module_identity.stable_hash;
        return stableName(view);
    }

    /// The names of `views`, the modules of one link.
    pub fn fromViews(allocator: Allocator, views: []const checked.ImportedModuleView) Allocator.Error!ModuleNames {
        var owners: std.AutoHashMapUnmanaged([32]u8, ?[32]u8) = .empty;
        defer owners.deinit(allocator);
        for (views) |view| {
            const entry = try owners.getOrPut(allocator, stableName(view));
            if (!entry.found_existing) {
                entry.value_ptr.* = view.module_identity.stable_hash;
            } else if (entry.value_ptr.*) |owner| {
                if (!std.mem.eql(u8, &owner, &view.module_identity.stable_hash)) entry.value_ptr.* = null;
            }
        }
        var result: ModuleNames = .{};
        errdefer result.deinit(allocator);
        for (views) |view| {
            if (owners.get(stableName(view)).? == null) try result.content_named.put(allocator, view.key.bytes, {});
        }
        return result;
    }

    /// The name a body edit leaves alone (`fromViews`).
    pub fn stableName(view: checked.ImportedModuleView) [32]u8 {
        var hasher = base.Sha256.init(.{});
        hasher.update("roc.boxy.module_name.v1");
        hasher.update(view.module_env.module_name);
        hasher.update(&[_]u8{0});
        hasher.update(@tagName(view.module_identity.kind));
        hasher.update(&[_]u8{0});
        hasher.update(&view.module_env.checking_dependency.digest);
        return hasher.finalResult();
    }
};

/// The checked modules of one lowering, by dense index and by key.
pub const Modules = struct {
    allocator: Allocator,
    views: std.ArrayList(ModuleView) = .empty,
    by_key: std.AutoHashMapUnmanaged([32]u8, ModuleIndex) = .empty,
    /// The names of a module unit's link. A whole program links nothing, and
    /// names each module by its content identity.
    link_names: ?*const ModuleNames = null,

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
        const link_name = if (self.link_names) |link_names| link_names.of(view) else view.module_identity.stable_hash;
        try self.views.append(self.allocator, .{ .index = index, .key = view.key.digest(), .view = view, .link_name = link_name });
        return index;
    }

    pub fn get(self: *const Modules, index: ModuleIndex) *const ModuleView {
        return &self.views.items[@backingInt(index)];
    }

    /// The name module `index`'s workers and nominal types are digested
    /// under, the same in every object of one link.
    pub fn linkName(self: *const Modules, index: ModuleIndex) [32]u8 {
        return self.get(index).link_name;
    }

    /// The module of this lowering with key `key`, or null when the
    /// lowering does not contain it.
    pub fn find(self: *const Modules, key: checked.ModuleId) ?*const ModuleView {
        const index = self.by_key.get(key.bytes) orelse return null;
        return self.get(index);
    }

    pub fn byKey(self: *const Modules, key: checked.ModuleId) *const ModuleView {
        const index = self.by_key.get(key.bytes) orelse
            invariantFmt("checked data named a module outside this lowering ({s})", .{&std.fmt.bytesToHex(key.bytes[0..8], .lower)});
        return self.get(index);
    }
};

/// A label (record field or tag name), interned program-wide by text.
pub const NameId = enum(u32) { _ };

/// The content id of the label or tag `text`: the runtime's definition
/// (`boxy_runtime.labelId`) computed with the compiler's SHA-256.
pub fn labelContentId(text: []const u8) boxy_runtime.NameId {
    var hasher = base.Sha256.init(.{});
    hasher.update(boxy_runtime.desc.label_name_domain);
    hasher.update(text);
    return boxy_runtime.desc.nameIdFromDigest(hasher.finalResult());
}

/// The content id of the `same_name_ordinal`th nominal declaration named
/// `text` of the module with link name `module_name`
/// (`boxy_runtime.nominalId`).
pub fn nominalContentId(module_name: [32]u8, same_name_ordinal: u32, text: []const u8) boxy_runtime.NameId {
    var hasher = base.Sha256.init(.{});
    hasher.update(boxy_runtime.desc.nominal_name_domain);
    hasher.update(&module_name);
    var index_bytes: [4]u8 = undefined;
    std.mem.writeInt(u32, &index_bytes, same_name_ordinal, .little);
    hasher.update(&index_bytes);
    hasher.update(text);
    return boxy_runtime.desc.nameIdFromDigest(hasher.finalResult());
}

test "name content ids agree with the runtime's definition" {
    try std.testing.expectEqual(boxy_runtime.labelId("field"), labelContentId("field"));
    try std.testing.expectEqual(boxy_runtime.labelId(""), labelContentId(""));
    try std.testing.expectEqual(boxy_runtime.nominalId(@splat(7), 3, "Point"), nominalContentId(@splat(7), 3, "Point"));
}

/// An interned type node.
pub const TypeId = enum(u32) { _ };

/// A nominal declaration: its module and its id in that module's checked
/// type store.
pub const NominalDeclKey = struct {
    module: ModuleIndex,
    decl: checked.CheckedNominalDeclarationId,
};

pub const Primitive = checked.CheckedPrimitive;

/// The presence slot variant of an omitted optional field. `#` starts a
/// comment in source, so no user tag can spell it.
pub const missing_label = "#Missing";
/// The presence slot variant of a supplied optional field.
pub const present_label = "#Present";
/// The labels of the two `Store.committedKind` types.
const required_kind_label = "#Required";
const optional_kind_label = "#Optional";

/// How a record field is stored (design.md "Field Kinds").
pub const FieldKind = enum(u8) {
    /// An inline slot (`required` and `defaulted` fields).
    inline_slot,
    /// A `[#Missing, #Present(value)]` presence slot (`optional` fields).
    presence_slot,
    /// A slot of the scheme variable `Field.slot_variable`, whose binding is
    /// the field's storage at an instantiation: the value type for a required
    /// instantiation, the presence slot for an optional one (fields whose
    /// kind is a scheme's undetermined kind variable).
    variable_slot,
};

/// A field of a record type, by the record's type and the field's name.
pub const FieldDefaultKey = struct { record: TypeId, name: NameId };

/// One field of a record type: its name, type, and storage kind.
pub const Field = struct {
    name: NameId,
    ty: TypeId,
    kind: FieldKind,
    /// The scheme variable of a `variable_slot` field.
    slot_variable: u32 = 0,
};

/// One tag of a tag union type: its name and payload types.
pub const Tag = struct {
    name: NameId,
    payloads: Span,
};

/// A stored list of `TypeId`s: its entry in `Store.type_ids`.
pub const Span = struct { start: u32, len: u32 };
/// A range of fields in `Store.fields`.
pub const FieldSpan = struct { start: u32, len: u32 };
/// A range of tags in `Store.tags`.
pub const TagSpan = struct { start: u32, len: u32 };
/// A stored memory order: its entry in `Store.declared_slots`.
pub const SlotSpan = struct { start: u32, len: u32 };

/// How a nominal record with unnamed fields lays itself out (design.md
/// "Nominal Record Field Order"). A record type that has this is the backing
/// of such a nominal, and is a distinct type from the record of the same
/// fields, which the structural rule lays out. Its `fields` stay sorted by
/// label, so reading a field by name is the same for both; only storage
/// differs.
pub const DeclaredOrder = struct {
    /// The types of the unnamed fields, in declared order.
    padding: Span,
    /// The record's slots in declared order, each by storage index: a named
    /// field's position in the record's `fields`, and an unnamed field's
    /// position in `padding` past those.
    slots: SlotSpan,
};

/// A `DeclaredOrder` to intern, with its children as slices.
pub const DeclaredOrderInput = struct {
    padding: []const TypeId,
    slots: []const u32,
};

/// A record type: its fields sorted by label text, a variable node standing
/// for the row's unnamed remainder (null for a closed record), and the
/// memory order of a padded nominal's backing.
pub const Record = struct {
    fields: FieldSpan,
    ext: ?TypeId,
    declared: ?DeclaredOrder,
};

/// A named type: a nominal or opaque declaration instantiated at `args`.
pub const Nominal = struct {
    decl: NominalDeclKey,
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

/// One interned Boxy type.
pub const Node = union(enum) {
    primitive: Primitive,
    /// The empty record (and every other zero-field record).
    empty_record,
    record: Record,
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
    /// descriptor is that worker's hidden descriptor `index`.
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

/// How many erased-callable slots a value of a type holds
/// (`Converter.erasedResultSlots`).
pub const ErasedSlots = enum(u32) { none = 0, single = 1, several = 2 };

/// The program-wide type store.
pub const Store = struct {
    allocator: Allocator,
    nodes: std.ArrayList(Node) = .empty,
    /// Whether each node's representation is independent of every type
    /// variable: no `variable` node is reachable from it except through a
    /// function type, whose values are erased callables.
    closed_flags: std.ArrayList(bool) = .empty,
    /// The payload of each presence-slot type (design.md "Field Kinds"),
    /// recorded as `intern` meets its reserved labels, which no source type
    /// can spell, however the type came to be interned.
    presence_payloads: collections.DenseMapUnmanaged(TypeId, TypeId) = .empty,
    /// Every stored child list, indexed by its span's `start`. Each list is
    /// its own allocation in `span_arena`, so a slice `span`, `fieldSpan`, or
    /// `tagSpan` returns stays valid as the store grows.
    type_ids: std.ArrayList([]const TypeId) = .empty,
    fields: std.ArrayList([]const Field) = .empty,
    tags: std.ArrayList([]const Tag) = .empty,
    declared_slots: std.ArrayList([]const u32) = .empty,
    span_arena: std.heap.ArenaAllocator,
    interned: std.HashMapUnmanaged(InternKey, TypeId, InternContext, std.hash_map.default_max_load_percentage) = .empty,
    key_bytes: std.ArrayList(u8) = .empty,
    scratch_key: std.ArrayList(u8) = .empty,

    name_texts: std.ArrayList([]const u8) = .empty,
    /// Each name's content id (design.md "Name Identity"), by `NameId`.
    name_content_ids: std.ArrayList(boxy_runtime.NameId) = .empty,
    name_ids: std.StringHashMapUnmanaged(NameId) = .empty,
    /// Each nominal declaration's name, its own whatever its text: a
    /// descriptor compares nominals by name, and two declarations can share
    /// a name's text.
    decl_name_ids: std.AutoHashMapUnmanaged(NominalDeclKey, NameId) = .empty,
    name_arena: std.heap.ArenaAllocator,

    /// Converted closed checked types, by module and checked id.
    closed_memo: std.AutoHashMapUnmanaged(ClosedKey, TypeId) = .empty,
    /// The backing of each instantiated nominal node.
    backing_memo: collections.DenseMapUnmanaged(TypeId, TypeId) = .empty,
    /// The variables of an enclosing scope each nominal declaration's
    /// backing mentions (`Converter.outerVariables`).
    decl_outer_vars: std.AutoHashMapUnmanaged(NominalDeclKey, []const checked.CheckedTypeId) = .empty,
    /// The `??` default identity of each defaulted field of a record type
    /// conversion produced (design.md "Defaulted Fields").
    field_defaults: std.AutoHashMapUnmanaged(FieldDefaultKey, checked.CheckedFieldDefault) = .empty,
    /// `Converter.representationClosed` of each type asked about.
    representation_closed: collections.DenseMapUnmanaged(TypeId, bool) = .empty,
    /// `Converter.erasedResultSlots` of each type asked about.
    erased_slots: collections.DenseMapUnmanaged(TypeId, ErasedSlots) = .empty,
    /// Each module's nominal declarations by nominal key, filled once per
    /// module on first use.
    decl_by_key: std.AutoHashMapUnmanaged(DeclKey, checked.CheckedNominalDeclarationId) = .empty,
    decl_keys_indexed: std.AutoHashMapUnmanaged(ModuleIndex, void) = .empty,
    /// Each module's nominal declarations by source statement
    /// (`declarationBySourceDecl`), indexed on first use.
    decl_by_source: std.AutoHashMapUnmanaged(struct { module: ModuleIndex, source_decl: u32 }, checked.CheckedNominalDeclarationId) = .empty,
    decl_sources_indexed: std.AutoHashMapUnmanaged(ModuleIndex, void) = .empty,

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
        return .{
            .allocator = allocator,
            .span_arena = std.heap.ArenaAllocator.init(allocator),
            .name_arena = std.heap.ArenaAllocator.init(allocator),
        };
    }

    pub fn deinit(self: *Store) void {
        self.presence_payloads.deinit(self.allocator);
        self.nodes.deinit(self.allocator);
        self.closed_flags.deinit(self.allocator);
        self.type_ids.deinit(self.allocator);
        self.fields.deinit(self.allocator);
        self.tags.deinit(self.allocator);
        self.declared_slots.deinit(self.allocator);
        self.span_arena.deinit();
        self.interned.deinit(self.allocator);
        self.key_bytes.deinit(self.allocator);
        self.scratch_key.deinit(self.allocator);
        self.name_texts.deinit(self.allocator);
        self.name_content_ids.deinit(self.allocator);
        self.name_ids.deinit(self.allocator);
        self.decl_name_ids.deinit(self.allocator);
        self.name_arena.deinit();
        self.closed_memo.deinit(self.allocator);
        self.backing_memo.deinit(self.allocator);
        {
            var outer = self.decl_outer_vars.valueIterator();
            while (outer.next()) |vars| self.allocator.free(vars.*);
            self.decl_outer_vars.deinit(self.allocator);
        }
        self.representation_closed.deinit(self.allocator);
        self.erased_slots.deinit(self.allocator);
        self.field_defaults.deinit(self.allocator);
        self.decl_by_key.deinit(self.allocator);
        self.decl_keys_indexed.deinit(self.allocator);
        self.decl_by_source.deinit(self.allocator);
        self.decl_sources_indexed.deinit(self.allocator);
    }

    /// The declaration with nominal key `key` in `view`'s type store.
    /// The nominal declaration of `view` declared at source statement
    /// `source_decl`, or null when `view` declares none there.
    pub fn declarationBySourceDecl(self: *Store, view: *const ModuleView, source_decl: u32) Allocator.Error!?checked.CheckedNominalDeclarationId {
        if (!self.decl_sources_indexed.contains(view.index)) {
            try self.decl_sources_indexed.put(self.allocator, view.index, {});
            for (view.types().nominal_declarations) |declaration| {
                try self.decl_by_source.put(self.allocator, .{ .module = view.index, .source_decl = declaration.nominal.source_decl }, declaration.id);
            }
        }
        return self.decl_by_source.get(.{ .module = view.index, .source_decl = source_decl });
    }

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
        errdefer _ = self.name_texts.pop();
        try self.name_content_ids.append(self.allocator, labelContentId(text));
        errdefer _ = self.name_content_ids.pop();
        try self.name_ids.put(self.allocator, owned, id);
        return id;
    }

    /// The name of nominal declaration `decl`, whose text is `text`:
    /// `module_name` is the declaring module's link name and
    /// `same_name_ordinal` the declaration's position among those it shares
    /// `text` with.
    pub fn internDeclName(self: *Store, decl: NominalDeclKey, module_name: [32]u8, same_name_ordinal: u32, text: []const u8) Allocator.Error!NameId {
        if (self.decl_name_ids.get(decl)) |existing| return existing;
        const owned = try self.name_arena.allocator().dupe(u8, text);
        const id: NameId = @fromBackingInt(@intCast(self.name_texts.items.len));
        try self.name_texts.append(self.allocator, owned);
        errdefer _ = self.name_texts.pop();
        try self.name_content_ids.append(self.allocator, nominalContentId(module_name, same_name_ordinal, text));
        errdefer _ = self.name_content_ids.pop();
        try self.decl_name_ids.put(self.allocator, decl, id);
        return id;
    }

    pub fn nameText(self: *const Store, id: NameId) []const u8 {
        return self.name_texts.items[@backingInt(id)];
    }

    /// The content id the runtime `Name` node of `id` carries.
    pub fn nameContentId(self: *const Store, id: NameId) boxy_runtime.NameId {
        return self.name_content_ids.items[@backingInt(id)];
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
        return self.type_ids.items[value.start];
    }

    pub fn fieldSpan(self: *const Store, value: FieldSpan) []const Field {
        return self.fields.items[value.start];
    }

    pub fn tagSpan(self: *const Store, value: TagSpan) []const Tag {
        return self.tags.items[value.start];
    }

    pub fn nominalArgs(self: *const Store, nominal: Nominal) []const TypeId {
        return self.span(nominal.args);
    }

    /// The types of `record`'s unnamed fields, in declared order.
    pub fn paddingTypes(self: *const Store, record: Record) []const TypeId {
        const declared = record.declared orelse return &.{};
        return self.span(declared.padding);
    }

    /// The slots of `declared`, in declared order, by storage index.
    pub fn declaredSlots(self: *const Store, declared: DeclaredOrder) []const u32 {
        return self.declared_slots.items[declared.slots.start];
    }

    /// The record row of `ty`: `ty` itself, unless it is the backing of a
    /// padded nominal, whose row is the record of the same fields.
    pub fn recordRow(self: *Store, ty: TypeId) Allocator.Error!TypeId {
        const record = switch (self.get(ty)) {
            .record => |record| record,
            .primitive, .empty_record, .tuple, .tag_union, .list, .box, .nominal, .function, .variable => return ty,
        };
        if (record.declared == null) return ty;
        if (record.fields.len == 0 and record.ext == null) return try self.emptyRecord();
        return try self.intern(.{ .record = .{ .fields = self.fieldSpan(record.fields), .ext = record.ext } });
    }

    /// Whether two record types of one value store their fields alike.
    pub fn sameRecordOrder(self: *const Store, a: Record, b: Record) bool {
        const a_declared = a.declared orelse return b.declared == null;
        const b_declared = b.declared orelse return false;
        return std.mem.eql(u32, self.declaredSlots(a_declared), self.declaredSlots(b_declared));
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
                    try self.keyU32(field.slot_variable);
                    closed = closed and self.isClosed(field.ty) and field.kind != .variable_slot;
                }
                try self.keyU32(if (record.ext) |ext| @backingInt(ext) else std.math.maxInt(u32));
                if (record.ext) |ext| closed = closed and self.isClosed(ext);
                if (record.declared) |declared| {
                    try self.keySpan(declared.padding);
                    try self.keyU32(@intCast(declared.slots.len));
                    for (declared.slots) |slot| try self.keyU32(slot);
                    for (declared.padding) |padding| closed = closed and self.isClosed(padding);
                }
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
            .record => |record| .{ .record = .{
                .fields = try self.addFields(record.fields),
                .ext = record.ext,
                .declared = if (record.declared) |declared| .{
                    .padding = try self.addSpan(declared.padding),
                    .slots = try self.addSlots(declared.slots),
                } else null,
            } },
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
        switch (node) {
            .tag_union => |tag_union| if (tag_union.ext == null and tag_union.tags.len == 2 and
                std.mem.eql(u8, self.nameText(tag_union.tags[0].name), missing_label) and
                std.mem.eql(u8, self.nameText(tag_union.tags[1].name), present_label))
            {
                try self.presence_payloads.put(self.allocator, id, tag_union.tags[1].payloads[0]);
            },
            .primitive, .empty_record, .record, .tuple, .list, .box, .nominal, .function, .variable => {},
        }
        return id;
    }

    /// The payload of `ty` when it is a presence slot.
    pub fn presenceSlotPayload(self: *const Store, ty: TypeId) ?TypeId {
        return self.presence_payloads.get(ty);
    }

    /// The type standing for a field kind a checked substitution committed
    /// to, until the field's value type turns it into the field's storage
    /// (`Program.resolveKindSlots`). A body lowered in a scope binding a kind
    /// to it stores the field by that kind (`instantiatedField`).
    pub fn committedKind(self: *Store, optional: bool) Allocator.Error!TypeId {
        const name = try self.internName(if (optional) optional_kind_label else required_kind_label);
        return try self.intern(.{ .tag_union = .{ .tags = &.{.{ .name = name, .payloads = &.{} }}, .ext = null } });
    }

    /// Which field kind `ty` stands for, when it is a `committedKind`.
    pub fn committedKindOf(self: *const Store, ty: TypeId) ?enum { required, optional } {
        const tag_union = switch (self.get(ty)) {
            .tag_union => |tag_union| tag_union,
            .primitive, .empty_record, .record, .tuple, .list, .box, .nominal, .function, .variable => return null,
        };
        if (tag_union.ext != null or tag_union.tags.len != 1) return null;
        const text = self.nameText(self.tagSpan(tag_union.tags)[0].name);
        if (std.mem.eql(u8, text, required_kind_label)) return .required;
        if (std.mem.eql(u8, text, optional_kind_label)) return .optional;
        return null;
    }

    fn addSpan(self: *Store, ids: []const TypeId) Allocator.Error!Span {
        const start: u32 = @intCast(self.type_ids.items.len);
        try self.type_ids.append(self.allocator, try self.span_arena.allocator().dupe(TypeId, ids));
        return .{ .start = start, .len = @intCast(ids.len) };
    }

    fn addFields(self: *Store, fields: []const Field) Allocator.Error!FieldSpan {
        const start: u32 = @intCast(self.fields.items.len);
        try self.fields.append(self.allocator, try self.span_arena.allocator().dupe(Field, fields));
        return .{ .start = start, .len = @intCast(fields.len) };
    }

    fn addSlots(self: *Store, slots: []const u32) Allocator.Error!SlotSpan {
        const owned = try self.span_arena.allocator().dupe(u32, slots);
        try self.declared_slots.append(self.allocator, owned);
        return .{ .start = @intCast(self.declared_slots.items.len - 1), .len = @intCast(slots.len) };
    }

    fn addTags(self: *Store, tags: []const TagInput) Allocator.Error!TagSpan {
        const stored = try self.span_arena.allocator().alloc(Tag, tags.len);
        for (stored, tags) |*tag, input| tag.* = .{ .name = input.name, .payloads = try self.addSpan(input.payloads) };
        const start: u32 = @intCast(self.tags.items.len);
        try self.tags.append(self.allocator, stored);
        return .{ .start = start, .len = @intCast(tags.len) };
    }

    /// The field `name` of value type `ty` whose kind variable a scheme binds
    /// to `slot`: the field's storage at that instantiation, or a committed
    /// kind.
    pub fn instantiatedField(self: *const Store, name: NameId, ty: TypeId, slot: TypeId) Field {
        if (self.presence_payloads.contains(slot)) return .{ .name = name, .ty = ty, .kind = .presence_slot };
        if (self.committedKindOf(slot)) |kind| return .{ .name = name, .ty = ty, .kind = switch (kind) {
            .required => .inline_slot,
            .optional => .presence_slot,
        } };
        switch (self.get(slot)) {
            .variable => |index| if (slot != ty) return .{ .name = name, .ty = ty, .kind = .variable_slot, .slot_variable = index },
            .primitive, .empty_record, .record, .tuple, .tag_union, .list, .box, .nominal, .function => {},
        }
        return .{ .name = name, .ty = ty, .kind = .inline_slot };
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

/// A tag to intern: its name and payload types.
pub const TagInput = struct {
    name: NameId,
    payloads: []const TypeId,
};

/// A node to intern, with its children as slices.
pub const NodeInput = union(enum) {
    primitive: Primitive,
    empty_record,
    record: struct { fields: []const Field, ext: ?TypeId, declared: ?DeclaredOrderInput = null },
    tuple: []const TypeId,
    tag_union: struct { tags: []const TagInput, ext: ?TypeId },
    list: TypeId,
    box: TypeId,
    nominal: struct {
        decl: NominalDeclKey,
        args: []const TypeId,
        name: NameId,
        is_opaque: bool,
        builtin: ?checked.CheckedBuiltinNominal,
        has_backing: bool,
    },
    function: struct { args: []const TypeId, ret: TypeId, effectful: bool },
    variable: u32,
};

/// Report a Boxy lowering invariant violation.
pub fn invariant(comptime message: []const u8) noreturn {
    base.invariant("Boxy lowering invariant violated: " ++ message, .{});
}

/// Report a formatted Boxy lowering invariant violation.
pub fn invariantFmt(comptime fmt: []const u8, args: anytype) noreturn {
    base.invariant("Boxy lowering invariant violated: " ++ fmt, args);
}

/// Converts checked types to store nodes with an explicit work stack.
pub const Converter = struct {
    store: *Store,
    modules: *const Modules,
    /// Whether an undetermined field kind outside every scheme commits to
    /// `required` (a generated codec contract's snapshot, whose kinds no
    /// caller can pin), rather than keeping its presence-slot representation.
    seal_undetermined_kinds: bool = false,
    /// The program's solution of each platform requirement identity
    /// variable: the app type the platform's requirement takes it to be.
    platform_identities: ?*const std.AutoHashMapUnmanaged(IdentityKey, TypeId) = null,

    /// A checked type of a module.
    pub const IdentityKey = struct { module: ModuleIndex, ty: checked.CheckedTypeId };

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
        // Whether each result mentions a nominal whose backing reads the
        // converting scope's variables, which makes it scope-dependent
        // though checking found no identity variable in it.
        var scoped: std.ArrayList(bool) = .empty;
        defer scoped.deinit(allocator);
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
                    try scoped.append(allocator, false);
                } else {
                    if (frames.items.len > 1_000_000) invariant("checked type conversion nested past every finite type");
                    try self.pushFrame(&frames, &pending, results.items.len, child.module, child.ty);
                }
                continue;
            }
            const frame = frames.pop().?;
            const children = results.items[frame.results_start..];
            var frame_scoped = (try self.outerVariablesOf(frame.module, frame.ty)).len != 0;
            for (scoped.items[frame.results_start..]) |child_scoped| frame_scoped = frame_scoped or child_scoped;
            const built = try self.finish(frame.module, frame.ty, children, env);
            results.shrinkRetainingCapacity(frame.results_start);
            scoped.shrinkRetainingCapacity(frame.results_start);
            pending.shrinkRetainingCapacity(frame.pending_start);
            // A type with no identity variables converts the same under
            // every environment.
            if (!frame_scoped and !self.seal_undetermined_kinds and !try rootHasIdentityVariables(self.modules.get(frame.module), frame.ty)) {
                try self.store.closed_memo.put(allocator, .{ .module = frame.module, .ty = frame.ty }, built);
            }
            if (frames.items.len == 0) return built;
            try results.append(allocator, built);
            try scoped.append(allocator, frame_scoped);
        }
        unreachable; // the loop returns when the outermost frame finishes
    }

    const Child = struct { module: ModuleIndex, ty: checked.CheckedTypeId };

    /// The app type the program solves platform requirement identity
    /// variable `ty` to, or null for any other type.
    pub fn platformIdentity(self: Converter, module: ModuleIndex, ty: checked.CheckedTypeId) ?TypeId {
        const identities = self.platform_identities orelse return null;
        return identities.get(.{ .module = module, .ty = ty });
    }

    pub fn rootHasIdentityVariables(view: *const ModuleView, ty: checked.CheckedTypeId) Allocator.Error!bool {
        return try view.types().rootHasIdentityVariables(ty);
    }

    /// The node for `ty` when it needs no children converted first.
    fn leaf(self: Converter, module: ModuleIndex, ty: checked.CheckedTypeId, env: Env) Allocator.Error!?TypeId {
        if (env.lookup(module, ty)) |bound| return bound;
        if (self.platformIdentity(module, ty)) |solved| return solved;
        const view = self.modules.get(module);
        if (!self.seal_undetermined_kinds and !try rootHasIdentityVariables(view, ty)) {
            if (self.store.closed_memo.get(.{ .module = module, .ty = ty })) |memo| return memo;
        }
        return switch (view.types().payload(ty)) {
            .pending => invariant("a pending checked type reached Boxy lowering"),
            .err => invariant("an erroneous checked type reached Boxy lowering"),
            .flex, .rigid => |variable| try self.sealVariable(variable),
            .empty_record => try self.store.emptyRecord(),
            .empty_tag_union => try self.store.emptyTagUnion(),
            .nominal => |nominal| switch (nominal.representation) {
                .builtin => |builtin_nominal| switch (checked.builtinRuntimeEncoding(builtin_nominal)) {
                    .primitive => |prim| try self.store.primitive(prim),
                    .bool_tag_union, .try_nominal, .list, .box, .dict, .set, .iterator, .parse_tag_union_spec, .fields, .field, .crypto_sha256_digest, .crypto_sha256_hasher, .crypto_blake3_digest, .crypto_blake3_hasher => null,
                },
                .local_declaration, .imported_declaration, .local_box_payload_capability, .imported_box_payload_capability, .opaque_without_backing => null,
            },
            .field_kind => |kind| try self.store.committedKind(kind == .optional),
            .alias, .record, .tuple, .function, .tag_union => null,
        };
    }

    /// Whether scheme variable `variable` of `module` is SEALED (design.md
    /// "Sealed Result Rows"): a flex tag-row tail that an annotation bounds,
    /// whose row default is the empty row, and whose constraints closing
    /// the row discharges. A worker binds such a variable to the empty row
    /// and takes no descriptor for it; omitting it from the worker's scheme
    /// variables is exactly that, since a flex no scope binds seals to its
    /// row default (`sealVariable`).
    pub fn sealedSchemeVar(self: Converter, module: ModuleIndex, variable: checked.CheckedTypeId, fn_root: checked.CheckedTypeId, evidence_params: []const static_dispatch.EvidenceParamRecord) Allocator.Error!bool {
        const types = self.modules.get(module).types();
        const payload = types.payload(variable);
        const bounded = switch (payload) {
            .flex => |flex| flex.bounded_row_ext and flex.row_default == .empty_tag_union and payload.variableSealsToRowDefault(),
            .rigid, .pending, .field_kind, .err, .alias, .record, .tuple, .nominal, .function, .empty_record, .tag_union, .empty_tag_union => false,
        };
        if (!bounded) return false;
        // Polarity also opens a row in a callback's parameter, which the
        // definition produces but which no instantiation of its result
        // converts: a variable at such a position, or anywhere else outside
        // the result's reach, keeps its descriptor.
        if (try self.occursOutsideResultReach(module, fn_root, variable)) return false;
        // A callable the worker receives from its caller carries the caller's
        // instantiation of every variable its interface mentions. A function
        // parameter shows that interface and is excluded above; a `where`
        // dictionary's interface is its evidence parameter's callable, and a
        // nominal over a callable payload hides its callable's interface in
        // its methods, so a parameter holding one may involve any variable.
        for (evidence_params) |param| {
            if (try self.mentionsVariable(module, param.callable_ty, variable)) return false;
        }
        return !try self.argumentsHoldCallablePayload(module, fn_root);
    }

    /// Whether any argument of the function type `fn_root` holds a nominal
    /// over a callable payload (`local_box_payload_capability`,
    /// `imported_box_payload_capability`): a function value whose
    /// interface the signature does not show.
    fn argumentsHoldCallablePayload(self: Converter, module: ModuleIndex, fn_root: checked.CheckedTypeId) Allocator.Error!bool {
        const types = self.modules.get(module).types();
        const allocator = self.store.allocator;
        var root = fn_root;
        const function = while (true) switch (types.payload(root)) {
            .alias => |alias| root = alias.backing,
            .function => |function| break function,
            .pending, .field_kind, .err, .flex, .rigid, .record, .tuple, .nominal, .empty_record, .tag_union, .empty_tag_union => return true,
        };
        var pending: std.ArrayList(checked.CheckedTypeId) = .empty;
        defer pending.deinit(allocator);
        var seen: collections.DenseMapUnmanaged(checked.CheckedTypeId, void) = .empty;
        defer seen.deinit(allocator);
        try pending.appendSlice(allocator, function.args);
        while (pending.pop()) |ty| {
            if ((try seen.getOrPut(allocator, ty)).found_existing) continue;
            switch (types.payload(ty)) {
                .alias => |alias| {
                    try pending.appendSlice(allocator, alias.args);
                    try pending.append(allocator, alias.backing);
                },
                .record => |record| {
                    for (0..record.fields.len()) |field_index| {
                        const field = record.fields.get(field_index);
                        try pending.append(allocator, field.ty);
                    }
                    try pending.append(allocator, record.ext);
                },
                .tuple => |items| try pending.appendSlice(allocator, items),
                .tag_union => |tag_union| {
                    for (0..tag_union.tags.len()) |tag_index| {
                        const tag = tag_union.tags.get(tag_index);
                        try pending.appendSlice(allocator, tag.args);
                    }
                    try pending.append(allocator, tag_union.ext);
                },
                .function => |inner| {
                    try pending.appendSlice(allocator, inner.args);
                    try pending.append(allocator, inner.ret);
                },
                .nominal => |nominal| switch (nominal.representation) {
                    .local_box_payload_capability, .imported_box_payload_capability => return true,
                    .builtin, .local_declaration, .imported_declaration, .opaque_without_backing => try pending.appendSlice(allocator, nominal.args),
                },
                .pending, .field_kind, .err, .flex, .rigid, .empty_record, .empty_tag_union => {},
            }
        }
        return false;
    }

    /// Whether `variable` occurs in the function type `fn_root` anywhere but
    /// along its result's coercion reach: an argument, or a position under
    /// the result that a row coercion does not reach (design.md "Row
    /// Coercion Primitive": the root row, tag payload rows, the error
    /// argument of a `Try`, and alias backings are reached; records, tuples,
    /// lists, boxes, functions, and other named types' arguments are not).
    fn occursOutsideResultReach(self: Converter, module: ModuleIndex, fn_root: checked.CheckedTypeId, variable: checked.CheckedTypeId) Allocator.Error!bool {
        const types = self.modules.get(module).types();
        const allocator = self.store.allocator;
        var root = fn_root;
        const function = while (true) switch (types.payload(root)) {
            .alias => |alias| root = alias.backing,
            .function => |function| break function,
            .pending, .field_kind, .err, .flex, .rigid, .record, .tuple, .nominal, .empty_record, .tag_union, .empty_tag_union => return true,
        };
        for (function.args) |arg| {
            if (try self.mentionsVariable(module, arg, variable)) return true;
        }
        // Reached positions under the result, in which only the variable's
        // own occurrence as a row extension is on the reach.
        var reached: std.ArrayList(checked.CheckedTypeId) = .empty;
        defer reached.deinit(allocator);
        var seen: collections.DenseMapUnmanaged(checked.CheckedTypeId, void) = .empty;
        defer seen.deinit(allocator);
        try reached.append(allocator, function.ret);
        while (reached.pop()) |ty| {
            if (ty == variable) continue;
            if ((try seen.getOrPut(allocator, ty)).found_existing) continue;
            switch (types.payload(ty)) {
                .alias => |alias| try reached.append(allocator, alias.backing),
                .tag_union => |tag_union| {
                    for (0..tag_union.tags.len()) |tag_index| {
                        const tag = tag_union.tags.get(tag_index);
                        try reached.appendSlice(allocator, tag.args);
                    }
                    try reached.append(allocator, tag_union.ext);
                },
                .nominal => |nominal| switch (nominal.representation) {
                    .builtin => |builtin_nominal| if (checked.builtinRuntimeEncoding(builtin_nominal) == .try_nominal and nominal.args.len == 2) {
                        if (try self.mentionsVariable(module, nominal.args[0], variable)) return true;
                        try reached.append(allocator, nominal.args[1]);
                    } else {
                        for (nominal.args) |arg| if (try self.mentionsVariable(module, arg, variable)) return true;
                    },
                    .local_declaration, .imported_declaration, .local_box_payload_capability, .imported_box_payload_capability, .opaque_without_backing => {
                        for (nominal.args) |arg| if (try self.mentionsVariable(module, arg, variable)) return true;
                    },
                },
                .flex, .rigid, .pending, .field_kind, .err, .empty_record, .empty_tag_union => {},
                .record, .tuple, .function => if (try self.mentionsVariable(module, ty, variable)) return true,
            }
        }
        return false;
    }

    /// Whether `variable` occurs anywhere in `root`.
    fn mentionsVariable(self: Converter, module: ModuleIndex, root: checked.CheckedTypeId, variable: checked.CheckedTypeId) Allocator.Error!bool {
        const types = self.modules.get(module).types();
        const allocator = self.store.allocator;
        var pending: std.ArrayList(checked.CheckedTypeId) = .empty;
        defer pending.deinit(allocator);
        var seen: collections.DenseMapUnmanaged(checked.CheckedTypeId, void) = .empty;
        defer seen.deinit(allocator);
        try pending.append(allocator, root);
        while (pending.pop()) |ty| {
            if (ty == variable) return true;
            if ((try seen.getOrPut(allocator, ty)).found_existing) continue;
            switch (types.payload(ty)) {
                .alias => |alias| {
                    try pending.appendSlice(allocator, alias.args);
                    try pending.append(allocator, alias.backing);
                },
                .record => |record| {
                    for (0..record.fields.len()) |field_index| {
                        const field = record.fields.get(field_index);
                        try pending.append(allocator, field.ty);
                    }
                    try pending.append(allocator, record.ext);
                },
                .tuple => |items| try pending.appendSlice(allocator, items),
                .tag_union => |tag_union| {
                    for (0..tag_union.tags.len()) |tag_index| {
                        const tag = tag_union.tags.get(tag_index);
                        try pending.appendSlice(allocator, tag.args);
                    }
                    try pending.append(allocator, tag_union.ext);
                },
                .function => |function| {
                    try pending.appendSlice(allocator, function.args);
                    try pending.append(allocator, function.ret);
                },
                .nominal => |nominal| try pending.appendSlice(allocator, nominal.args),
                .pending, .field_kind, .err, .flex, .rigid, .empty_record, .empty_tag_union => {},
            }
        }
        return false;
    }

    /// The type a scheme variable takes where nothing relates it to a use:
    /// what it seals to when no scope binds it.
    pub fn sealedScheme(self: Converter, module: ModuleIndex, variable: checked.CheckedTypeId) Allocator.Error!TypeId {
        return switch (self.modules.get(module).types().payload(variable)) {
            .flex, .rigid => |payload| try self.sealVariable(payload),
            .pending, .field_kind, .err, .alias, .record, .tuple, .nominal, .function, .empty_record, .tag_union, .empty_tag_union => try self.convert(module, variable, .{ .module = module, .vars = &.{}, .bound = &.{} }),
        };
    }

    /// Whether the checked type `root` holds the erroneous type anywhere:
    /// the type of a value checking rejected, which no lowering converts.
    pub fn hasError(self: Converter, module: ModuleIndex, root: checked.CheckedTypeId) Allocator.Error!bool {
        const allocator = self.store.allocator;
        const types = self.modules.get(module).types();
        var seen: collections.DenseMapUnmanaged(checked.CheckedTypeId, void) = .empty;
        defer seen.deinit(allocator);
        var pending: std.ArrayList(checked.CheckedTypeId) = .empty;
        defer pending.deinit(allocator);
        try pending.append(allocator, root);
        while (pending.pop()) |ty| {
            if ((try seen.getOrPut(allocator, ty)).found_existing) continue;
            switch (types.payload(ty)) {
                .field_kind, .err => return true,
                .alias => |alias| try pending.append(allocator, alias.backing),
                .record => |record| {
                    for (0..record.fields.len()) |field_index| {
                        const field = record.fields.get(field_index);
                        try pending.append(allocator, field.ty);
                    }
                    try pending.append(allocator, record.ext);
                },
                .tuple => |items| try pending.appendSlice(allocator, items),
                .tag_union => |tag_union| {
                    for (0..tag_union.tags.len()) |tag_index| {
                        const tag = tag_union.tags.get(tag_index);
                        try pending.appendSlice(allocator, tag.args);
                    }
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
                    for (0..current_fields.len()) |field_index| {
                        const field = current_fields.get(field_index);
                        try pending.append(allocator, .{ .module = module, .ty = field.ty });
                    }
                    current_fields = .empty;
                    switch (types.payload(current)) {
                        .alias => |alias| current = alias.backing,
                        .record => |ext_record| {
                            current_fields = ext_record.fields;
                            current = ext_record.ext;
                        },
                        .pending, .field_kind, .err, .flex, .rigid, .tuple, .nominal, .function, .empty_record, .tag_union, .empty_tag_union => break,
                    }
                }
                // The row's remainder, when it is not the empty record.
                switch (types.payload(current)) {
                    .empty_record => {},
                    .pending, .field_kind, .err, .flex, .rigid, .alias, .record, .tuple, .nominal, .function, .tag_union, .empty_tag_union => try pending.append(allocator, .{ .module = module, .ty = current }),
                }
            },
            .tuple => |items| for (items) |item| try pending.append(allocator, .{ .module = module, .ty = item }),
            .tag_union => |tag_union| {
                var current_tags = tag_union.tags;
                var current = tag_union.ext;
                while (true) {
                    for (0..current_tags.len()) |tag_index| {
                        const tag = current_tags.get(tag_index);
                        for (tag.args) |arg| try pending.append(allocator, .{ .module = module, .ty = arg });
                    }
                    current_tags = .empty;
                    switch (types.payload(current)) {
                        .alias => |alias| current = alias.backing,
                        .tag_union => |ext_union| {
                            current_tags = ext_union.tags;
                            current = ext_union.ext;
                        },
                        .pending, .field_kind, .err, .flex, .rigid, .record, .tuple, .nominal, .function, .empty_record, .empty_tag_union => break,
                    }
                }
                switch (types.payload(current)) {
                    .empty_tag_union => {},
                    .pending, .field_kind, .err, .flex, .rigid, .alias, .record, .tuple, .nominal, .function, .empty_record, .tag_union => try pending.append(allocator, .{ .module = module, .ty = current }),
                }
            },
            .function => |function| {
                for (function.args) |arg| try pending.append(allocator, .{ .module = module, .ty = arg });
                try pending.append(allocator, .{ .module = module, .ty = function.ret });
            },
            .nominal => |nominal| for (nominal.args) |arg| try pending.append(allocator, .{ .module = module, .ty = arg }),
            .pending, .field_kind, .err, .flex, .rigid, .empty_record, .empty_tag_union => unreachable, // `leaf` answers these
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
                    for (0..current_fields.len()) |field_index| {
                        const field = current_fields.get(field_index);
                        const field_name = try self.store.internName(name_store.recordFieldLabelText(field.name));
                        try fields.append(allocator, self.convertedField(module, field_name, children[child_index], field.kind, env));
                        if (field.kind.tag == .defaulted) try defaults.append(allocator, .{ .name = field_name, .default = field.kind.default });
                        child_index += 1;
                    }
                    current_fields = .empty;
                    switch (types.payload(current)) {
                        .alias => |alias| current = alias.backing,
                        .record => |ext_record| {
                            current_fields = ext_record.fields;
                            current = ext_record.ext;
                        },
                        .pending, .field_kind, .err, .flex, .rigid, .tuple, .nominal, .function, .empty_record, .tag_union, .empty_tag_union => break,
                    }
                }
                var ext: ?TypeId = switch (types.payload(current)) {
                    .empty_record => null,
                    .pending, .field_kind, .err, .flex, .rigid, .alias, .record, .tuple, .nominal, .function, .tag_union, .empty_tag_union => children[child_index],
                };
                // A remainder that converted to a closed record joins this row.
                if (ext) |ext_ty| switch (self.store.get(ext_ty)) {
                    .empty_record => ext = null,
                    .record => |ext_record| if (ext_record.ext == null) {
                        try fields.appendSlice(allocator, self.store.fieldSpan(ext_record.fields));
                        ext = null;
                    },
                    .variable => {},
                    .primitive, .tuple, .tag_union, .list, .box, .nominal, .function => invariant("a record row's remainder converted to a non-record type"),
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
                    for (0..current_tags.len()) |tag_index| {
                        const tag = current_tags.get(tag_index);
                        const count = tag.args.len;
                        try tags.append(allocator, .{
                            .name = try self.store.internName(name_store.tagLabelText(tag.name)),
                            .payloads = children[child_index..][0..count],
                        });
                        child_index += count;
                    }
                    current_tags = .empty;
                    switch (types.payload(current)) {
                        .alias => |alias| current = alias.backing,
                        .tag_union => |ext_union| {
                            current_tags = ext_union.tags;
                            current = ext_union.ext;
                        },
                        .pending, .field_kind, .err, .flex, .rigid, .record, .tuple, .nominal, .function, .empty_record, .empty_tag_union => break,
                    }
                }
                var ext: ?TypeId = switch (types.payload(current)) {
                    .empty_tag_union => null,
                    .pending, .field_kind, .err, .flex, .rigid, .alias, .record, .tuple, .nominal, .function, .empty_record, .tag_union => children[child_index],
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
                    .primitive, .empty_record, .record, .tuple, .list, .box, .nominal, .function => invariant("a tag row's remainder converted to a non-tag type"),
                };
                const store = self.store;
                std.mem.sort(TagInput, tags.items, store, struct {
                    fn lessThan(s: *Store, a: TagInput, b: TagInput) bool {
                        return s.nameLessThan(a.name, b.name);
                    }
                }.lessThan);
                return try self.store.intern(.{ .tag_union = .{ .tags = tags.items, .ext = ext } });
            },
            .nominal => |nominal| return try self.nominalNode(module, nominal, children, env),
            .pending, .field_kind, .err, .flex, .rigid, .empty_record, .empty_tag_union => unreachable, // `leaf` answers these
        }
    }

    /// The type of the nominal `ty` of `module` names (through aliases),
    /// applied to `args` rather than to its own arguments, in the scope
    /// `env`.
    pub fn nominalAt(self: Converter, module: ModuleIndex, ty: checked.CheckedTypeId, args: []const TypeId, env: Env) Allocator.Error!TypeId {
        const types = self.modules.get(module).types();
        var current = ty;
        while (true) switch (types.payload(current)) {
            .alias => |alias| current = alias.backing,
            .nominal => |nominal| {
                if (nominal.representation == .builtin) switch (checked.builtinRuntimeEncoding(nominal.representation.builtin)) {
                    .primitive => |prim| return try self.store.primitive(prim),
                    .bool_tag_union, .try_nominal, .list, .box, .dict, .set, .iterator, .parse_tag_union_spec, .fields, .field, .crypto_sha256_digest, .crypto_sha256_hasher, .crypto_blake3_digest, .crypto_blake3_hasher => {},
                };
                return try self.nominalNode(module, nominal, args, env);
            },
            .pending, .field_kind, .err, .flex, .rigid, .record, .tuple, .function, .empty_record, .tag_union, .empty_tag_union => invariant("a stored named type's occurrence is not a nominal type"),
        };
    }

    fn nominalNode(self: Converter, module: ModuleIndex, nominal: @FieldType(checked.CheckedTypePayload, "nominal"), children: []const TypeId, env: Env) Allocator.Error!TypeId {
        const allocator = self.store.allocator;
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
                        .bool_tag_union, .try_nominal, .dict, .set, .iterator, .crypto_sha256_digest, .crypto_sha256_hasher, .crypto_blake3_digest, .crypto_blake3_hasher => {},
                    },
                    .local_declaration, .imported_declaration, .local_box_payload_capability, .imported_box_payload_capability, .opaque_without_backing => {},
                }
                const decl = try self.declarationKey(view, nominal);
                const decl_key = decl orelse NominalDeclKey{ .module = module, .decl = @fromBackingInt(std.math.maxInt(u32)) };
                // A declaration in a function body whose backing mentions
                // the body's variables is instantiated at their types here
                // too: they follow its own arguments.
                const outer = if (decl) |ref| try self.outerVariables(ref) else &.{};
                const args = try allocator.alloc(TypeId, children.len + outer.len);
                defer allocator.free(args);
                @memcpy(args[0..children.len], children);
                for (outer, args[children.len..]) |variable, *arg| arg.* = (try self.leaf(decl_key.module, variable, env)).?;
                return try self.store.intern(.{ .nominal = .{
                    .decl = decl_key,
                    .args = args,
                    .name = if (decl != null) try self.declName(decl_key, name_store.typeNameText(nominal.name)) else try self.store.internName(name_store.typeNameText(nominal.name)),
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

    /// A record field of value type `ty`: an undetermined kind takes the
    /// binding its scheme gives it, and a kind no scope binds commits to
    /// `required`, as it does for every other strategy.
    fn convertedField(self: Converter, module: ModuleIndex, name: NameId, ty: TypeId, kind: checked.CheckedFieldKind, env: Env) Field {
        return switch (kind.tag) {
            .required, .defaulted => .{ .name = name, .ty = ty, .kind = .inline_slot },
            .optional => .{ .name = name, .ty = ty, .kind = .presence_slot },
            .undetermined => {
                if (!self.seal_undetermined_kinds) {
                    const variable = kind.undeterminedVariable() orelse invariant("an undetermined field kind has no variable");
                    if (env.lookup(module, variable)) |slot| return self.store.instantiatedField(name, ty, slot);
                }
                return .{ .name = name, .ty = ty, .kind = .inline_slot };
            },
            .err => invariant("an erroneous record field kind reached Boxy lowering"),
        };
    }

    /// The declaration a nominal payload names, or null for an opaque type
    /// with no backing.
    fn declarationKey(self: Converter, view: *const ModuleView, nominal: checked.CheckedNominalType) Allocator.Error!?NominalDeclKey {
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
        var seen: collections.DenseMapUnmanaged(TypeId, void) = .empty;
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
                    for (store.fieldSpan(record.fields)) |field| {
                        if (field.kind == .variable_slot) break :walk false;
                        try pending.append(store.allocator, field.ty);
                    }
                    if (record.ext) |ext| try pending.append(store.allocator, ext);
                    try pending.appendSlice(store.allocator, store.paddingTypes(record));
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

    /// How many erased-callable slots a value of `root` holds: none, one,
    /// or several. A function type is one slot; a record or tuple holds the
    /// slots of its fields; a tag union holds those of whichever variant it
    /// is, so its count is its largest variant's; a list of callables holds
    /// any number; a box holds its payload's, so `Box(function)` is one; a
    /// nominal holds its backing's. A type variable holds none: its
    /// descriptor is what says what it holds. A recursive occurrence holds
    /// none. An erased call whose result is one slot is an owned call
    /// (design.md "Destination-Passing Results and Allocation Reuse").
    pub fn erasedResultSlots(self: Converter, root: TypeId) Allocator.Error!ErasedSlots {
        const store = self.store;
        if (store.erased_slots.get(root)) |memo| return memo;
        const several: u32 = @backingInt(ErasedSlots.several);
        var in_progress: collections.DenseMapUnmanaged(TypeId, void) = .empty;
        defer in_progress.deinit(store.allocator);
        var frames: std.ArrayList(ErasedSlotFrame) = .empty;
        defer {
            for (frames.items) |frame| if (frame.owns_children) store.allocator.free(frame.children);
            frames.deinit(store.allocator);
        }
        const root_frame = try self.erasedSlotFrame(root, &in_progress) orelse {
            const slots = self.erasedSlotLeaf(root);
            try store.erased_slots.put(store.allocator, root, slots);
            return slots;
        };
        try frames.append(store.allocator, root_frame);
        var result: u32 = 0;
        while (frames.items.len != 0) {
            const top = &frames.items[frames.items.len - 1];
            if (top.kind == .max and top.next < top.variants.len) {
                const variant = top.variants[top.next];
                top.next += 1;
                try frames.append(store.allocator, .{ .ty = null, .kind = .sum, .children = store.span(variant.payloads) });
                continue;
            }
            if (top.kind != .max and top.next < top.children.len) {
                const child = top.children[top.next];
                top.next += 1;
                const contributed: u32 = if (store.erased_slots.get(child)) |memo|
                    @backingInt(memo)
                else if (in_progress.contains(child))
                    0
                else if (try self.erasedSlotFrame(child, &in_progress)) |frame| {
                    try frames.append(store.allocator, frame);
                    continue;
                } else @backingInt(self.erasedSlotLeaf(child));
                top.count = @min(several, top.count + contributed);
                continue;
            }
            const finished = frames.pop().?;
            if (finished.owns_children) store.allocator.free(finished.children);
            const count: u32 = switch (finished.kind) {
                .sum, .max => finished.count,
                .list => if (finished.count == 0) 0 else several,
            };
            if (finished.ty) |ty| {
                try store.erased_slots.put(store.allocator, ty, @fromBackingInt(@intCast(count)));
                _ = in_progress.remove(ty);
            }
            if (frames.items.len == 0) {
                result = count;
                break;
            }
            const parent = &frames.items[frames.items.len - 1];
            parent.count = switch (parent.kind) {
                .sum, .list => @min(several, parent.count + count),
                .max => @max(parent.count, count),
            };
        }
        return @fromBackingInt(@intCast(result));
    }

    /// One type being counted by `erasedResultSlots`: its children's
    /// counts join by sum, by maximum over a union's variants, or as a
    /// list's, which holds any number of its item's.
    const ErasedSlotFrame = struct {
        /// The type, or null for one tag variant's payloads.
        ty: ?TypeId,
        kind: enum { sum, max, list },
        children: []const TypeId = &.{},
        /// Whether `children` was gathered for this frame rather than being
        /// a span of the store's.
        owns_children: bool = false,
        next: usize = 0,
        /// The variants a `max` frame opens, one at a time.
        variants: []const Tag = &.{},
        count: u32 = 0,
    };

    /// The slots a type without children holds (`erasedResultSlots`).
    fn erasedSlotLeaf(self: Converter, ty: TypeId) ErasedSlots {
        return switch (self.store.get(ty)) {
            .function => .single,
            .variable, .primitive, .empty_record, .nominal => .none,
            .record, .tuple, .tag_union, .list, .box => unreachable, // erasedSlotFrame opens every type with children
        };
    }

    /// The frame counting `ty`'s children (`erasedResultSlots`), marking
    /// `ty` in progress, or null for a type without children.
    fn erasedSlotFrame(self: Converter, ty: TypeId, in_progress: *collections.DenseMapUnmanaged(TypeId, void)) Allocator.Error!?ErasedSlotFrame {
        const store = self.store;
        const frame: ErasedSlotFrame = switch (store.get(ty)) {
            .variable, .primitive, .empty_record, .function => return null,
            .record => |record| blk: {
                const fields = store.fieldSpan(record.fields);
                const padding = store.paddingTypes(record);
                const children = try store.allocator.alloc(TypeId, fields.len + padding.len);
                for (children[0..fields.len], fields) |*child, field| child.* = field.ty;
                @memcpy(children[fields.len..], padding);
                break :blk .{ .ty = ty, .kind = .sum, .children = children, .owns_children = true };
            },
            .tuple => |items| .{ .ty = ty, .kind = .sum, .children = store.span(items) },
            .tag_union => |tag_union| .{ .ty = ty, .kind = .max, .variants = store.tagSpan(tag_union.tags) },
            .list => |elem| .{ .ty = ty, .kind = .list, .children = try store.allocator.dupe(TypeId, &.{elem}), .owns_children = true },
            .box => |payload| .{ .ty = ty, .kind = .sum, .children = try store.allocator.dupe(TypeId, &.{payload}), .owns_children = true },
            .nominal => |nominal| if (nominal.has_backing)
                .{ .ty = ty, .kind = .sum, .children = try store.allocator.dupe(TypeId, &.{try self.backing(ty)}), .owns_children = true }
            else
                return null,
        };
        try in_progress.put(store.allocator, ty, {});
        return frame;
    }

    /// The outer variables of the nominal declaration `ty` names, when `ty`
    /// is a nominal with a declaration, else none.
    fn outerVariablesOf(self: Converter, module: ModuleIndex, ty: checked.CheckedTypeId) Allocator.Error![]const checked.CheckedTypeId {
        const view = self.modules.get(module);
        return switch (view.types().payload(ty)) {
            .nominal => |nominal| switch (nominal.representation) {
                .local_declaration => |decl| try self.outerVariables(.{ .module = module, .decl = decl }),
                .builtin, .imported_declaration, .local_box_payload_capability, .imported_box_payload_capability, .opaque_without_backing => &.{},
            },
            .pending, .field_kind, .err, .flex, .rigid, .alias, .record, .tuple, .function, .empty_record, .tag_union, .empty_tag_union => &.{},
        };
    }

    /// The name of nominal declaration `decl`, which is its own name
    /// whatever its text (design.md "Name Identity"): the runtime node's id
    /// digests the declaring module's link name and the declaration.
    fn declName(self: Converter, decl: NominalDeclKey, text: []const u8) Allocator.Error!NameId {
        const view = self.modules.get(decl.module);
        const declaration = view.types().nominalDeclarationById(decl.decl);
        // Declarations sharing a name are told apart by their order, which
        // the rest of the module's source leaves alone.
        var ordinal: u32 = 0;
        const name = view.types().nominalDeclarationName(declaration);
        for (view.types().nominal_declarations[0..@backingInt(decl.decl)]) |earlier| {
            if (view.types().nominalDeclarationName(earlier) == name) ordinal += 1;
        }
        return try self.store.internDeclName(decl, self.modules.linkName(decl.module), ordinal, text);
    }

    /// The variables of an enclosing scope that the backing of `decl` (a
    /// declaration in a function body) mentions, directly or through the
    /// backings of the declarations it names: the variables its backing
    /// reaches that are no reached declaration's formal. A top-level
    /// declaration has none.
    pub fn outerVariables(self: Converter, decl: NominalDeclKey) Allocator.Error![]const checked.CheckedTypeId {
        if (self.store.decl_outer_vars.get(decl)) |known| return known;
        const allocator = self.store.allocator;
        const view = self.modules.get(decl.module);
        const types = view.types();
        var seen: collections.DenseMapUnmanaged(checked.CheckedTypeId, void) = .empty;
        defer seen.deinit(allocator);
        var decls: collections.DenseMapUnmanaged(checked.CheckedNominalDeclarationId, void) = .empty;
        defer decls.deinit(allocator);
        var formals: collections.DenseMapUnmanaged(checked.CheckedTypeId, void) = .empty;
        defer formals.deinit(allocator);
        var variables: std.ArrayList(checked.CheckedTypeId) = .empty;
        defer variables.deinit(allocator);
        var pending: std.ArrayList(checked.CheckedTypeId) = .empty;
        defer pending.deinit(allocator);
        try decls.put(allocator, decl.decl, {});
        const root = types.nominalDeclarationById(decl.decl);
        for (root.formalArgs(types)) |formal| try formals.put(allocator, formal, {});
        try pending.append(allocator, root.backing);
        try pending.appendSlice(allocator, root.paddingFieldTypes(types));
        while (pending.pop()) |ty| {
            if ((try seen.getOrPut(allocator, ty)).found_existing) continue;
            switch (types.payload(ty)) {
                .flex, .rigid => try variables.append(allocator, ty),
                .alias => |alias| try pending.append(allocator, alias.backing),
                .record => |record| {
                    for (0..record.fields.len()) |field_index| {
                        const field = record.fields.get(field_index);
                        try pending.append(allocator, field.ty);
                    }
                    try pending.append(allocator, record.ext);
                },
                .tuple => |items| try pending.appendSlice(allocator, items),
                .tag_union => |tag_union| {
                    for (0..tag_union.tags.len()) |tag_index| {
                        const tag = tag_union.tags.get(tag_index);
                        try pending.appendSlice(allocator, tag.args);
                    }
                    try pending.append(allocator, tag_union.ext);
                },
                .function => |function| {
                    try pending.appendSlice(allocator, function.args);
                    try pending.append(allocator, function.ret);
                },
                .nominal => |nominal| {
                    try pending.appendSlice(allocator, nominal.args);
                    switch (nominal.representation) {
                        .local_declaration => |nested| if ((try decls.getOrPut(allocator, nested)).found_existing == false) {
                            const declaration = types.nominalDeclarationById(nested);
                            for (declaration.formalArgs(types)) |formal| try formals.put(allocator, formal, {});
                            try pending.append(allocator, declaration.backing);
                            try pending.appendSlice(allocator, declaration.paddingFieldTypes(types));
                        },
                        .builtin, .imported_declaration, .local_box_payload_capability, .imported_box_payload_capability, .opaque_without_backing => {},
                    }
                },
                .pending, .field_kind, .err, .empty_record, .empty_tag_union => {},
            }
        }
        var outer: std.ArrayList(checked.CheckedTypeId) = .empty;
        defer outer.deinit(allocator);
        for (variables.items) |variable| {
            if (!formals.contains(variable)) try outer.append(allocator, variable);
        }
        // Declaration order of the checked ids, so every instantiation
        // lists the same variables in the same positions.
        std.mem.sort(checked.CheckedTypeId, outer.items, {}, struct {
            fn lessThan(_: void, a: checked.CheckedTypeId, b: checked.CheckedTypeId) bool {
                return @backingInt(a) < @backingInt(b);
            }
        }.lessThan);
        const owned = try allocator.dupe(checked.CheckedTypeId, outer.items);
        try self.store.decl_outer_vars.put(allocator, decl, owned);
        return owned;
    }

    pub fn backing(self: Converter, nominal_ty: TypeId) Allocator.Error!TypeId {
        if (self.store.backing_memo.get(nominal_ty)) |memo| return memo;
        const nominal = switch (self.store.get(nominal_ty)) {
            .nominal => |value| value,
            .primitive, .empty_record, .record, .tuple, .tag_union, .list, .box, .function, .variable => invariant("asked for the backing of a non-nominal type"),
        };
        if (!nominal.has_backing) invariant("asked for the backing of an opaque type without one");
        const view = self.modules.get(nominal.decl.module);
        const declaration = view.types().nominalDeclarationById(nominal.decl.decl);
        const formals = try std.mem.concat(self.store.allocator, checked.CheckedTypeId, &.{ declaration.formalArgs(view.types()), try self.outerVariables(nominal.decl) });
        defer self.store.allocator.free(formals);
        const args = try self.store.allocator.dupe(TypeId, self.store.nominalArgs(nominal));
        defer self.store.allocator.free(args);
        if (formals.len != args.len) invariant("nominal instantiation arity differs from its declaration");
        const env: Env = .{ .module = nominal.decl.module, .vars = formals, .bound = args };
        // The declaration's formals are identity variables, so no part of
        // the backing that mentions them reaches the closed-type memo.
        const row = try self.convert(nominal.decl.module, declaration.backing, env);
        const result = try self.declaredBacking(nominal.decl.module, declaration, row, env);
        try self.store.backing_memo.put(self.store.allocator, nominal_ty, result);
        return result;
    }

    /// The backing of `declaration`, whose record row is `row`: `row` itself
    /// unless the declaration has unnamed fields, which make the backing a
    /// record type of its own (`DeclaredOrder`).
    fn declaredBacking(self: Converter, module: ModuleIndex, declaration: checked.CheckedNominalDeclaration, row: TypeId, env: Env) Allocator.Error!TypeId {
        const allocator = self.store.allocator;
        const view = self.modules.get(module);
        const declared_padding = declaration.paddingFieldTypes(view.types());
        if (declared_padding.len == 0) return row;
        const fields: []const Field, const ext: ?TypeId = switch (self.store.get(row)) {
            .record => |record| .{ self.store.fieldSpan(record.fields), record.ext },
            .empty_record => .{ &.{}, null },
            .primitive, .tuple, .tag_union, .list, .box, .nominal, .function, .variable => invariant("a nominal declaration with unnamed fields does not back onto a record"),
        };
        const padding = try allocator.alloc(TypeId, declared_padding.len);
        defer allocator.free(padding);
        for (declared_padding, padding) |checked_ty, *slot| slot.* = try self.convert(module, checked_ty, env);
        const declared_fields = declaration.declaredFields(view.types());
        const slots = try allocator.alloc(u32, declared_fields.len);
        defer allocator.free(slots);
        for (declared_fields, slots) |declared, *slot| slot.* = switch (declared) {
            .named => |label| blk: {
                const name = try self.store.internName(view.nameStore().recordFieldLabelText(label));
                for (fields, 0..) |field, index| {
                    if (field.name == name) break :blk @intCast(index);
                }
                invariant("a declared field is absent from its nominal's record row");
            },
            .padding => |ordinal| @intCast(fields.len + ordinal),
        };
        const backing_ty = try self.store.intern(.{ .record = .{
            .fields = fields,
            .ext = ext,
            .declared = .{ .padding = padding, .slots = slots },
        } });
        for (fields) |field| {
            const default = self.store.field_defaults.get(.{ .record = row, .name = field.name }) orelse continue;
            try self.store.field_defaults.put(allocator, .{ .record = backing_ty, .name = field.name }, default);
        }
        return backing_ty;
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

test "a padded backing is a type of its own, whose unnamed fields count toward it" {
    var store = Store.init(std.testing.allocator);
    defer store.deinit();
    const a = try store.internName("a");
    const z = try store.internName("z");
    const number = try store.primitive(.u32);
    const fields: []const Field = &.{ .{ .name = a, .ty = number, .kind = .inline_slot }, .{ .name = z, .ty = number, .kind = .inline_slot } };
    const plain = try store.intern(.{ .record = .{ .fields = fields, .ext = null } });
    const padded = try store.intern(.{ .record = .{ .fields = fields, .ext = null, .declared = .{ .padding = &.{number}, .slots = &.{ 1, 2, 0 } } } });
    const other_order = try store.intern(.{ .record = .{ .fields = fields, .ext = null, .declared = .{ .padding = &.{number}, .slots = &.{ 0, 2, 1 } } } });
    const other_padding = try store.intern(.{ .record = .{ .fields = fields, .ext = null, .declared = .{ .padding = &.{try store.primitive(.u64)}, .slots = &.{ 1, 2, 0 } } } });
    try std.testing.expect(plain != padded);
    try std.testing.expect(padded != other_order);
    try std.testing.expect(padded != other_padding);
    try std.testing.expectEqual(padded, try store.intern(.{ .record = .{ .fields = fields, .ext = null, .declared = .{ .padding = &.{number}, .slots = &.{ 1, 2, 0 } } } }));

    try std.testing.expect(!store.sameRecordOrder(store.get(plain).record, store.get(padded).record));
    try std.testing.expect(!store.sameRecordOrder(store.get(padded).record, store.get(plain).record));
    try std.testing.expect(!store.sameRecordOrder(store.get(padded).record, store.get(other_order).record));
    try std.testing.expect(store.sameRecordOrder(store.get(padded).record, store.get(other_padding).record));
    try std.testing.expectEqualSlices(TypeId, &.{number}, store.paddingTypes(store.get(padded).record));
    try std.testing.expectEqual(@as(usize, 0), store.paddingTypes(store.get(plain).record).len);

    try std.testing.expectEqual(plain, try store.recordRow(padded));
    try std.testing.expectEqual(plain, try store.recordRow(plain));
    try std.testing.expectEqual(number, try store.recordRow(number));
    const only_padding = try store.intern(.{ .record = .{ .fields = &.{}, .ext = null, .declared = .{ .padding = &.{number}, .slots = &.{0} } } });
    try std.testing.expectEqual(try store.emptyRecord(), try store.recordRow(only_padding));

    try std.testing.expect(store.isClosed(padded));
    const open = try store.intern(.{ .record = .{ .fields = fields, .ext = null, .declared = .{ .padding = &.{try store.variable(0)}, .slots = &.{ 1, 2, 0 } } } });
    try std.testing.expect(!store.isClosed(open));
}

/// Whether two types could unify without a structural conflict: the same
/// kind of type, the same primitive, records with the same fields, tuples
/// and functions of the same arity, the same nominal; a variable is
/// compatible with anything. A value constructed at a type compatible with
/// its destination is built at the destination (`Builder.exprType`).
pub fn compatible(allocator: Allocator, store: *const Store, a: TypeId, b: TypeId) Allocator.Error!bool {
    var pending: std.ArrayList([2]TypeId) = .empty;
    defer pending.deinit(allocator);
    var seen: std.AutoHashMapUnmanaged(struct { TypeId, TypeId }, void) = .empty;
    defer seen.deinit(allocator);
    try pending.append(allocator, .{ a, b });
    while (pending.pop()) |pair| {
        if (pair[0] == pair[1]) continue;
        if ((try seen.getOrPut(allocator, .{ pair[0], pair[1] })).found_existing) continue;
        const left = store.get(pair[0]);
        const right = store.get(pair[1]);
        if (left == .variable or right == .variable) continue;
        if (std.meta.activeTag(left) != std.meta.activeTag(right)) return false;
        switch (left) {
            .primitive => |primitive| if (primitive != right.primitive) return false,
            .empty_record => {},
            .list => |elem| try pending.append(allocator, .{ elem, right.list }),
            .box => |payload| try pending.append(allocator, .{ payload, right.box }),
            .tuple => |items| {
                const mine = store.span(items);
                const theirs = store.span(right.tuple);
                if (mine.len != theirs.len) return false;
                for (mine, theirs) |x, y| try pending.append(allocator, .{ x, y });
            },
            .record => |record| {
                const mine = store.fieldSpan(record.fields);
                const theirs = store.fieldSpan(right.record.fields);
                if (mine.len != theirs.len) return false;
                for (mine, theirs) |x, y| {
                    if (x.name != y.name) return false;
                    try pending.append(allocator, .{ x.ty, y.ty });
                }
            },
            .tag_union => |tag_union| {
                // Rows merge their tags; only shared labels at one arity
                // must agree.
                for (store.tagSpan(tag_union.tags)) |tag| {
                    for (store.tagSpan(right.tag_union.tags)) |other| {
                        if (tag.name != other.name or tag.payloads.len != other.payloads.len) continue;
                        for (store.span(tag.payloads), store.span(other.payloads)) |x, y| try pending.append(allocator, .{ x, y });
                    }
                }
            },
            .nominal => |nominal| {
                if (nominal.decl.module != right.nominal.decl.module or nominal.decl.decl != right.nominal.decl.decl) return false;
                for (store.nominalArgs(nominal), store.nominalArgs(right.nominal)) |x, y| try pending.append(allocator, .{ x, y });
            },
            .function => |function| {
                const mine = store.span(function.args);
                const theirs = store.span(right.function.args);
                if (mine.len != theirs.len) return false;
                for (mine, theirs) |x, y| try pending.append(allocator, .{ x, y });
                try pending.append(allocator, .{ function.ret, right.function.ret });
            },
            .variable => unreachable, // answered above
        }
    }
    return true;
}
