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
const Common = @import("../common.zig");
const boxy_runtime = @import("boxy_runtime");

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

/// Exact error reachability within one Boxy lowering's immutable checked
/// modules. The module index and checked ID together identify an immutable
/// node. Program.run installs all module views before lowering; its body
/// and template consumers only read those views. Conversion and stored-value
/// decoding intern into Boxy's separate Store, never the checked store, so
/// even a pending checked payload stays unchanged throughout this analysis.
/// The owner releases this analysis at the end of lowering, including when
/// the Program is retained for stored-function metadata.
pub const ErrorTypes = struct {
    const Key = struct { module: ModuleIndex, ty: checked.CheckedTypeId };
    const Walk = check.CheckedReachability.AnyReachable(Key, ErrorTypes);

    modules: *const Modules,
    walk: Walk,
    queries: usize = 0,

    /// Allocate a stable address for the SCC walk's context.
    pub fn create(allocator: Allocator, modules: *const Modules) Allocator.Error!*ErrorTypes {
        const self = try allocator.create(ErrorTypes);
        self.* = .{ .modules = modules, .walk = Walk.init(allocator, self) };
        return self;
    }

    pub fn destroy(self: *ErrorTypes) void {
        const allocator = self.walk.allocator;
        self.walk.deinit();
        allocator.destroy(self);
    }

    pub fn hasError(self: *ErrorTypes, module: ModuleIndex, root: checked.CheckedTypeId) Allocator.Error!bool {
        self.queries += 1;
        return try self.walk.visit(.{ .module = module, .ty = root });
    }

    pub const Stats = struct {
        queries: usize = 0,
        unique_expanded_nodes: usize = 0,
        // Exact allocated element capacity of the four traversal arrays;
        // the hash index is reported separately in slots, not guessed bytes.
        retained_array_capacity_bytes: usize = 0,
        index_capacity: usize = 0,
    };

    pub fn stats(self: *const ErrorTypes) Stats {
        return .{
            .queries = self.queries,
            .unique_expanded_nodes = self.walk.nodes.items.len,
            .retained_array_capacity_bytes = self.walk.nodes.capacity * @sizeOf(@TypeOf(self.walk.nodes.items[0])) +
                self.walk.frames.capacity * @sizeOf(@TypeOf(self.walk.frames.items[0])) +
                self.walk.children.capacity * @sizeOf(Key) + self.walk.component.capacity * @sizeOf(usize),
            .index_capacity = self.walk.index.capacity(),
        };
    }

    /// Preserve the old value-error predicate's edges exactly: alias backing
    /// only, nominal arguments only, and record field types rather than kinds.
    /// A field-kind root itself is rejected, as is an error root.
    pub fn visit(self: *ErrorTypes, children: anytype, key: Key) Allocator.Error!?bool {
        const payload = self.modules.get(key.module).types().payload(key.ty);
        switch (payload) {
            .field_kind, .err => return true,
            .alias => |alias| try children.add(.{ .module = key.module, .ty = alias.backing }),
            .record => |record| {
                for (0..record.fields.len()) |index| try children.add(.{ .module = key.module, .ty = record.fields.get(index).ty });
                try children.add(.{ .module = key.module, .ty = record.ext });
            },
            .tuple => |items| for (items) |ty| try children.add(.{ .module = key.module, .ty = ty }),
            .tag_union => |tag_union| {
                for (0..tag_union.tags.len()) |index| {
                    for (tag_union.tags.get(index).args) |ty| try children.add(.{ .module = key.module, .ty = ty });
                }
                try children.add(.{ .module = key.module, .ty = tag_union.ext });
            },
            .function => |function| {
                for (function.args) |ty| try children.add(.{ .module = key.module, .ty = ty });
                try children.add(.{ .module = key.module, .ty = function.ret });
            },
            .nominal => |nominal| for (nominal.args) |ty| try children.add(.{ .module = key.module, .ty = ty }),
            .pending, .flex, .rigid, .empty_record, .empty_tag_union => return false,
        }
        return null;
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
    /// Declaring modules by stable content identity, independent of local name stores.
    by_identity: std.AutoHashMapUnmanaged([32]u8, ModuleIndex) = .empty,
    /// Each module by its link name, the first module added under a name.
    by_link_name: std.AutoHashMapUnmanaged([32]u8, ModuleIndex) = .empty,
    /// The names of a module unit's link. A whole program links nothing, and
    /// names each module by its content identity.
    link_names: ?*const ModuleNames = null,
    /// The platform relation modules of the lowering's root, which is the
    /// first module: the app whose values fill the root's requirements.
    relations: std.ArrayList(ModuleIndex) = .empty,

    pub fn deinit(self: *Modules) void {
        self.relations.deinit(self.allocator);
        self.views.deinit(self.allocator);
        self.by_key.deinit(self.allocator);
        self.by_identity.deinit(self.allocator);
        self.by_link_name.deinit(self.allocator);
    }

    /// Add a module unless one with the same key is present.
    pub fn add(self: *Modules, view: checked.ImportedModuleView) Allocator.Error!ModuleIndex {
        const entry = try self.by_key.getOrPut(self.allocator, view.key.bytes);
        if (entry.found_existing) return entry.value_ptr.*;
        const index: ModuleIndex = @fromBackingInt(@intCast(self.views.items.len));
        entry.value_ptr.* = index;
        const link_name = if (self.link_names) |link_names| link_names.of(view) else view.module_identity.stable_hash;
        try self.views.append(self.allocator, .{ .index = index, .key = view.key.digest(), .view = view, .link_name = link_name });
        try self.by_identity.put(self.allocator, view.module_identity.stable_hash, index);
        const named = try self.by_link_name.getOrPut(self.allocator, link_name);
        if (!named.found_existing) named.value_ptr.* = index;
        return index;
    }

    pub fn get(self: *const Modules, index: ModuleIndex) *const ModuleView {
        return &self.views.items[@backingInt(index)];
    }

    /// Rebase a checked default from its name store into this program's modules.
    pub fn fieldDefault(self: *const Modules, module: ModuleIndex, default: checked.CheckedFieldDefault) Allocator.Error!FieldDefault {
        const origin = default.origin() orelse invariant("a field default has no declaring module");
        const identity = self.get(module).nameStore().moduleIdentityBytes(origin).*;
        const declaring = self.by_identity.get(identity) orelse invariant("a field default's declaring module is not in this lowering");
        const bodies = self.get(declaring).bodies();
        try bodies.demandIndex();
        const ordinal = std.sort.binarySearch(u32, bodies.defaultExprs(), default.expr_node, struct {
            fn order(target: u32, item: u32) std.math.Order {
                return std.math.order(target, item);
            }
        }.order) orelse invariant("a field default's expression is no default of its module");
        return .{ .module = declaring, .ordinal = @intCast(ordinal) };
    }

    /// The name module `index`'s workers and nominal types are digested
    /// under, the same in every object of one link.
    pub fn linkName(self: *const Modules, index: ModuleIndex) [32]u8 {
        return self.get(index).link_name;
    }

    /// The module of this lowering whose link name is `name`, or null when
    /// the lowering does not contain it.
    pub fn findLinkName(self: *const Modules, name: [32]u8) ?ModuleIndex {
        return self.by_link_name.get(name);
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

/// A `??` default's immutable identity, rebased into this program's modules:
/// its declaring module and its place among that module's defaults, which
/// an edit elsewhere in the module leaves alone (design.md "Caching Under
/// `.boxy`"). Defaults affect codec semantics, not the field's inline
/// representation.
pub const FieldDefault = struct { module: ModuleIndex, ordinal: u32 };

/// An optional `FieldDefault` at its own size.
pub const OptionalFieldDefault = collections.OptionalStruct(FieldDefault, "ordinal");

/// One field of a record type: its name, type, and storage kind.
pub const Field = struct {
    name: NameId,
    ty: TypeId,
    kind: FieldKind,
    /// The scheme variable of a `variable_slot` field.
    slot_variable: u32 = 0,
    default: OptionalFieldDefault = .none,
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
/// A stored memory order: its entry in `Store.declared_slots` and
/// `Store.declared_entries`.
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
    /// The record's slots in memory order, each by storage index: a named
    /// field's position in the record's `fields`, and an unnamed field's
    /// position in `padding` past those. The store derives them from the
    /// declaration's entries when it interns the record: the entries in
    /// declared order, then every field no entry names, which a row
    /// extension supplies, in label order. So every field has a slot, and
    /// resolving a row extension early or late gives one type.
    slots: SlotSpan,
};

/// One entry of a padded nominal's declaration, in declared order.
pub const DeclaredEntry = union(enum) {
    /// A named field, by label.
    named: NameId,
    /// An unnamed field, by its position in `padding`.
    padding: u32,
};

/// A record's `DeclaredOrder`, or its absence, at a `DeclaredOrder`'s own
/// size: the slots' `start` is stored plus one, so zero means the record has
/// no declared order.
pub const OptionalDeclaredOrder = struct {
    padding: Span = .{ .start = 0, .len = 0 },
    stored_slots_start: u32 = 0,
    slots_len: u32 = 0,

    pub const none: OptionalDeclaredOrder = .{};

    pub fn some(order: DeclaredOrder) OptionalDeclaredOrder {
        std.debug.assert(order.slots.start < std.math.maxInt(u32));
        return .{ .padding = order.padding, .stored_slots_start = order.slots.start + 1, .slots_len = order.slots.len };
    }

    pub fn get(self: OptionalDeclaredOrder) ?DeclaredOrder {
        if (self.stored_slots_start == 0) {
            std.debug.assert(self.padding.start == 0 and self.padding.len == 0 and self.slots_len == 0);
            return null;
        }
        return .{ .padding = self.padding, .slots = .{ .start = self.stored_slots_start - 1, .len = self.slots_len } };
    }
};

/// A `DeclaredOrder` to intern, with its children as slices.
pub const DeclaredOrderInput = struct {
    padding: []const TypeId,
    entries: []const DeclaredEntry,
};

/// A record type: its fields sorted by label text, a variable node standing
/// for the row's unnamed remainder (null for a closed record), and the
/// memory order of a padded nominal's backing.
pub const Record = struct {
    fields: FieldSpan,
    ext: collections.OptionalId(TypeId),
    declared: OptionalDeclaredOrder,
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

comptime {
    std.debug.assert(@sizeOf(Record) == 28);
    std.debug.assert(@sizeOf(Node) == 32);
    std.debug.assert(@sizeOf(Field) == 24);
}

/// A binding of checked type variables to nodes, used while converting:
/// a worker's scheme variables to `variable` nodes, or a declaration's
/// formals to an instantiation's arguments.
pub const Env = struct {
    module: ModuleIndex,
    vars: []const checked.CheckedTypeId,
    bound: []const TypeId,
    /// The position of each of `vars`, when the environment's owner built
    /// one (`VarPositions`): a worker's environment carries its scheme's, so
    /// its conversions' lookups do not scan the scheme.
    positions: ?*const VarPositions = null,
    /// Only literal-chain frames carry a checked substitution. Conversion
    /// substitutes a demanded root into a concrete environment before entering the
    /// ordinary converter; ordinary variable lookups remain concrete.
    lazy_substitution: ?*LazySubstitution = null,

    pub const empty_bindings: []const checked.CheckedTypeId = &.{};

    pub fn lookup(self: Env, module: ModuleIndex, id: checked.CheckedTypeId) ?TypeId {
        const index = self.lookupIndex(module, id) orelse return null;
        return self.bound[index];
    }

    /// The scheme position `id` is bound at, if any.
    pub fn lookupIndex(self: Env, module: ModuleIndex, id: checked.CheckedTypeId) ?usize {
        std.debug.assert(self.lazy_substitution == null);
        if (module != self.module) return null;
        if (self.positions) |positions| {
            std.debug.assert(positions.vars.ptr == self.vars.ptr and positions.vars.len == self.vars.len);
            return positions.map.get(id) orelse return null;
        }
        for (self.vars, 0..) |variable, index| {
            if (variable == id) return index;
        }
        return null;
    }
};

/// A literal frame's checked instantiation, resolved lazily: each actual is
/// converted only when a demanded type reads its variable. It is not a
/// runtime call instance, and its unused bindings can mention scopes the
/// literal root never entered.
pub const LazySubstitution = struct {
    module: ModuleIndex,
    vars: []const checked.CheckedTypeId,
    actuals: []const checked.CheckedTypeId,
    source_module: ModuleIndex,
    source: Env,
    lexical: Env,
    memo: std.AutoHashMapUnmanaged(Key, TypeId) = .empty,
    allocator: Allocator,

    const Key = struct { module: ModuleIndex, ty: checked.CheckedTypeId, seal_kinds: bool };
    const Query = struct { module: ModuleIndex, ty: checked.CheckedTypeId, env: Env };
    const Source = union(enum) { query: Query, concrete: TypeId };
    const Binding = struct { variable: checked.CheckedTypeId, source: Source };
    const Work = struct {
        query: Query,
        bindings: []const Binding,
        values: []TypeId,
        next: usize = 0,
    };

    /// The arena owns the frame, its memo, and the immutable substitution.
    pub fn init(arena: Allocator, module: ModuleIndex, vars: []const checked.CheckedTypeId, actuals: []const checked.CheckedTypeId, source_module: ModuleIndex, source: Env, lexical: Env) Allocator.Error!Env {
        if (vars.len != actuals.len) invariant("a literal frame's substitution differs from its scheme");
        if (vars.len == 0) return lexical;
        const frame = try arena.create(LazySubstitution);
        frame.* = .{ .module = module, .vars = vars, .actuals = actuals, .source_module = source_module, .source = source, .lexical = lexical, .allocator = arena };
        return .{ .module = module, .vars = &.{}, .bound = &.{}, .lazy_substitution = frame };
    }

    fn binding(converter: Converter, module: ModuleIndex, ty: checked.CheckedTypeId, original: Env) ?struct { source: Source, descend: bool = false } {
        var env = original;
        while (env.lazy_substitution) |frame| {
            if (module == frame.module) {
                if (std.mem.findScalar(checked.CheckedTypeId, frame.vars, ty)) |index| {
                    const actual = frame.actuals[index];
                    return .{ .source = .{ .query = .{ .module = frame.source_module, .ty = actual, .env = frame.source } } };
                }
            }
            env = frame.lexical;
        }
        if (env.lookupIndex(module, ty)) |index| return .{
            .source = .{ .concrete = env.bound[index] },
            .descend = converter.tailNodeAt(env, index) != null,
        };
        return null;
    }

    /// Discover only the bindings the ordinary conversion will read. Field
    /// kinds and whole-row bindings count too, not just flex/rigid leaves.
    fn dependencies(converter: Converter, query: Query, allocator: Allocator) Allocator.Error![]const Binding {
        var pending: std.ArrayList(Converter.Child) = .empty;
        defer pending.deinit(allocator);
        var seen: CheckedTypeMap(void) = .empty;
        defer seen.deinit(allocator);
        var bindings: std.ArrayList(Binding) = .empty;
        errdefer bindings.deinit(allocator);
        try pending.append(allocator, .{ .module = query.module, .ty = query.ty });
        const store = converter.modules.get(query.module).types();
        while (pending.pop()) |child| {
            const ty = child.ty;
            if ((try seen.getOrPut(allocator, ty)).found_existing) continue;
            if (binding(converter, query.module, ty, query.env)) |resolved| {
                try bindings.append(allocator, .{ .variable = ty, .source = resolved.source });
                if (!resolved.descend) continue;
            }
            switch (store.payload(ty)) {
                // Use precisely the converter's row flattening and alias
                // traversal, with the field-kind reads finish performs too.
                .alias, .record, .tag_union, .tuple, .function, .nominal => {
                    _ = try converter.appendChildren(&pending, query.module, store.payload(ty), true);
                },
                .pending, .err, .flex, .rigid, .field_kind, .empty_record, .empty_tag_union => {},
            }
        }
        return try bindings.toOwnedSlice(allocator);
    }

    fn known(converter: Converter, query: Query) Allocator.Error!?TypeId {
        const frame = query.env.lazy_substitution orelse return try converter.convert(query.module, query.ty, query.env);
        return frame.memo.get(.{ .module = query.module, .ty = query.ty, .seal_kinds = converter.seal_undetermined_kinds });
    }

    fn push(converter: Converter, stack: *std.ArrayList(Work), query: Query, allocator: Allocator) Allocator.Error!void {
        const bindings = try dependencies(converter, query, allocator);
        errdefer allocator.free(bindings);
        const values = try allocator.alloc(TypeId, bindings.len);
        errdefer allocator.free(values);
        try stack.append(allocator, .{ .query = query, .bindings = bindings, .values = values });
    }

    /// Substitute each demanded type once per frame, using an explicit dependency
    /// stack. No type conversion or dictionary lookup resolves unused bindings.
    fn convert(converter: Converter, module: ModuleIndex, ty: checked.CheckedTypeId, env: Env) Allocator.Error!TypeId {
        const allocator = converter.store.allocator;
        const root: Query = .{ .module = module, .ty = ty, .env = env };
        if (try known(converter, root)) |value| return value;
        var stack: std.ArrayList(Work) = .empty;
        defer {
            for (stack.items) |work| {
                allocator.free(work.bindings);
                allocator.free(work.values);
            }
            stack.deinit(allocator);
        }
        try push(converter, &stack, root, allocator);
        while (stack.items.len != 0) {
            const work = &stack.items[stack.items.len - 1];
            if (work.next < work.bindings.len) {
                const child = switch (work.bindings[work.next].source) {
                    .query => |query| query,
                    .concrete => |value| {
                        work.values[work.next] = value;
                        work.next += 1;
                        continue;
                    },
                };
                if (try known(converter, child)) |value| {
                    work.values[work.next] = value;
                    work.next += 1;
                } else try push(converter, &stack, child, allocator);
                continue;
            }
            const variables = try allocator.alloc(checked.CheckedTypeId, work.bindings.len);
            defer allocator.free(variables);
            for (work.bindings, variables) |entry, *variable| variable.* = entry.variable;
            const value = try converter.convert(work.query.module, work.query.ty, .{ .module = work.query.module, .vars = variables, .bound = work.values });
            const owner = work.query.env.lazy_substitution.?;
            try owner.memo.put(owner.allocator, .{ .module = work.query.module, .ty = work.query.ty, .seal_kinds = converter.seal_undetermined_kinds }, value);
            allocator.free(work.bindings);
            allocator.free(work.values);
            _ = stack.pop();
        }
        return (try known(converter, root)).?;
    }
};

/// A map keyed by checked type ids. Those live in two ranges, source
/// variables counting up from zero and synthesized types counting up from
/// the high bit (`check.CheckedTypeIds`), and a `DenseMap`'s chunk directory
/// spans every chunk between the lowest and highest key it holds, so one
/// map holding a key of each range would allocate, clear and free a
/// directory over half the 32-bit id space. This keeps one map per range.
pub fn CheckedTypeMap(comptime V: type) type {
    return struct {
        const Self = @This();
        const Range = collections.DenseMapUnmanaged(checked.CheckedTypeId, V);

        source: Range = .empty,
        synthesized: Range = .empty,

        pub const empty: Self = .{};

        pub fn get(self: *const Self, key: checked.CheckedTypeId) ?V {
            return if (check.CheckedTypeIds.isSynthesized(key)) self.synthesized.get(key) else self.source.get(key);
        }

        pub fn getOrPut(self: *Self, gpa: Allocator, key: checked.CheckedTypeId) Allocator.Error!Range.GetOrPutResult {
            const range = if (check.CheckedTypeIds.isSynthesized(key)) &self.synthesized else &self.source;
            return range.getOrPut(gpa, key);
        }

        pub fn count(self: *const Self) usize {
            return self.source.count() + self.synthesized.count();
        }

        /// Empty the map, keeping its storage, in time proportional to the
        /// entries it held.
        pub fn clearRetainingCapacity(self: *Self) void {
            self.source.clearRetainingCapacity();
            self.synthesized.clearRetainingCapacity();
        }

        pub fn deinit(self: *Self, gpa: Allocator) void {
            self.source.deinit(gpa);
            self.synthesized.deinit(gpa);
            self.* = undefined;
        }
    };
}

/// The position of each variable of a scope's variable list `vars`, for
/// `Env.lookup`: a variable listed twice (an enclosing scheme's variable that
/// a generalized local's scheme lists again) is at its first position, where
/// a scan finds it.
pub const VarPositions = struct {
    vars: []const checked.CheckedTypeId,
    map: CheckedTypeMap(u32) = .empty,

    pub fn init(allocator: Allocator, vars: []const checked.CheckedTypeId) Allocator.Error!VarPositions {
        var positions: VarPositions = .{ .vars = vars };
        errdefer positions.deinit(allocator);
        for (vars, 0..) |variable, index| {
            const entry = try positions.map.getOrPut(allocator, variable);
            if (!entry.found_existing) entry.value_ptr.* = @intCast(index);
        }
        return positions;
    }

    pub fn deinit(self: *VarPositions, allocator: Allocator) void {
        self.map.deinit(allocator);
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
    /// Unlike representation closure, includes function arguments and results.
    variable_free_flags: std.ArrayList(bool) = .empty,
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
    declared_entries: std.ArrayList([]const DeclaredEntry) = .empty,
    span_arena: std.heap.ArenaAllocator,
    interned: std.HashMapUnmanaged(InternKey, TypeId, InternContext, std.hash_map.default_max_load_percentage) = .empty,
    /// The checked types the current walk over a checked type has visited
    /// (`beginCheckedWalk`). A fresh dense map per walk would allocate a
    /// chunk for each id it meets and a directory spanning the ids between,
    /// and free them all again, though a walk visits only a few types spread
    /// over the module's whole id range; this one keeps its storage across
    /// walks, so a walk pays only for the entries it adds.
    checked_walk: CheckedTypeMap(void) = .empty,
    /// The node `intern` gave each primitive, the variable of each low index,
    /// the empty record and the empty tag union, once one was asked for:
    /// these leaves are asked for far more often than they are new, and
    /// interned nodes never change.
    primitive_ids: std.EnumArray(Primitive, ?TypeId) = .initFill(null),
    variable_ids: std.ArrayList(collections.OptionalId(TypeId)) = .empty,
    empty_record_id: ?TypeId = null,
    empty_tag_union_id: ?TypeId = null,
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

    /// Converted closed checked types, by module and checked id
    /// (`Converter.convert`).
    closed_memo: std.AutoHashMapUnmanaged(ClosedKey, ClosedMemo) = .empty,
    /// Converted checked types under a worker's own scheme environment (one
    /// carrying `Env.positions`), by module, checked id and environment.
    scheme_memo: std.AutoHashMapUnmanaged(SchemeKey, ClosedMemo) = .empty,
    /// The scheme position of each closed record or tag union that is a row
    /// node of the converting worker's signature: one whose row ends in a
    /// quantified uncommitted tail (design.md "Methods On Open Rows"). Such a
    /// type stores its values exactly as the plain record or tag union does,
    /// and is a distinct type only so a descriptor taken of it reads the
    /// worker's input for the whole value (`Program.rowNodesAsVariables`).
    /// Like a `variable` node's index, the position names a slot of the
    /// scope the type was converted in.
    tail_node_of: collections.DenseMapUnmanaged(TypeId, u32) = .empty,
    /// The backing of each instantiated nominal node.
    backing_memo: collections.DenseMapUnmanaged(TypeId, TypeId) = .empty,
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

    /// A memoized conversion, and whether the type reaches a record or tag
    /// union whose row ends in an uncommitted tail, which a scope can name as
    /// a closed row node (`Converter.pushFrame`). Such a type
    /// converts the same only in scopes that name no record or tag union.
    const ClosedMemo = struct { ty: TypeId, reaches_row_node: bool };

    /// A checked type under an environment that carries `Env.positions`.
    /// Its positions and bindings are never freed or changed while the
    /// store lives, so their addresses name the environment.
    pub const SchemeKey = struct { module: ModuleIndex, ty: checked.CheckedTypeId, env_module: ModuleIndex, positions: usize, bound: usize, bound_len: usize };

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
        self.variable_free_flags.deinit(self.allocator);
        self.type_ids.deinit(self.allocator);
        self.fields.deinit(self.allocator);
        self.tags.deinit(self.allocator);
        self.declared_slots.deinit(self.allocator);
        self.declared_entries.deinit(self.allocator);
        self.span_arena.deinit();
        self.interned.deinit(self.allocator);
        self.checked_walk.deinit(self.allocator);
        self.variable_ids.deinit(self.allocator);
        self.key_bytes.deinit(self.allocator);
        self.scratch_key.deinit(self.allocator);
        self.name_texts.deinit(self.allocator);
        self.name_content_ids.deinit(self.allocator);
        self.name_ids.deinit(self.allocator);
        self.decl_name_ids.deinit(self.allocator);
        self.name_arena.deinit();
        self.closed_memo.deinit(self.allocator);
        self.scheme_memo.deinit(self.allocator);
        self.tail_node_of.deinit(self.allocator);
        self.backing_memo.deinit(self.allocator);
        self.representation_closed.deinit(self.allocator);
        self.erased_slots.deinit(self.allocator);
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

    /// Direct payloads whose explicit Box is one erased allocation.
    pub fn isErasedBoxPayload(self: *const Store, id: TypeId) bool {
        return self.get(id) == .function;
    }

    pub fn isClosed(self: *const Store, id: TypeId) bool {
        return self.closed_flags.items[@backingInt(id)];
    }

    pub fn isVariableFree(self: *const Store, id: TypeId) bool {
        return self.variable_free_flags.items[@backingInt(id)];
    }

    /// Computed once from interned children; adapters need signature variables
    /// even though an erased callable itself always has a closed layout.
    fn inputVariableFree(self: *const Store, node: NodeInput) bool {
        switch (node) {
            .primitive, .empty_record => {},
            .variable => return false,
            .list, .box => |child| return self.isVariableFree(child),
            .record => |record| {
                for (record.fields) |field| {
                    if (field.kind == .variable_slot or !self.isVariableFree(field.ty)) return false;
                }
                if (record.ext) |ext| {
                    if (!self.isVariableFree(ext)) return false;
                }
                if (record.declared) |declared| {
                    for (declared.padding) |child| {
                        if (!self.isVariableFree(child)) return false;
                    }
                }
            },
            .tuple => |items| {
                for (items) |child| {
                    if (!self.isVariableFree(child)) return false;
                }
            },
            .tag_union => |tags| {
                for (tags.tags) |tag| {
                    for (tag.payloads) |child| {
                        if (!self.isVariableFree(child)) return false;
                    }
                }
                if (tags.ext) |ext| {
                    if (!self.isVariableFree(ext)) return false;
                }
            },
            .nominal => |nominal| {
                for (nominal.args) |child| {
                    if (!self.isVariableFree(child)) return false;
                }
            },
            .function => |function| {
                for (function.args) |child| {
                    if (!self.isVariableFree(child)) return false;
                }
                return self.isVariableFree(function.ret);
            },
        }
        return true;
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
        const declared = record.declared.get() orelse return &.{};
        return self.span(declared.padding);
    }

    /// The slots of `declared`, in memory order, by storage index.
    pub fn declaredSlots(self: *const Store, declared: DeclaredOrder) []const u32 {
        return self.declared_slots.items[declared.slots.start];
    }

    /// The declaration entries `declared`'s slots were derived from.
    pub fn declaredEntries(self: *const Store, declared: DeclaredOrder) []const DeclaredEntry {
        return self.declared_entries.items[declared.slots.start];
    }

    /// The record row of `ty`: `ty` itself, unless it is the backing of a
    /// padded nominal, whose row is the record of the same fields.
    pub fn recordRow(self: *Store, ty: TypeId) Allocator.Error!TypeId {
        const record = switch (self.get(ty)) {
            .record => |record| record,
            .primitive, .empty_record, .tuple, .tag_union, .list, .box, .nominal, .function, .variable => return ty,
        };
        if (record.declared.get() == null) return ty;
        if (record.fields.len == 0 and record.ext == .none) return try self.emptyRecord();
        return try self.intern(.{ .record = .{ .fields = self.fieldSpan(record.fields), .ext = record.ext.get() } });
    }

    /// Whether two record types of one value store their fields alike.
    pub fn sameRecordOrder(self: *const Store, a: Record, b: Record) bool {
        const a_declared = a.declared.get() orelse return b.declared.get() == null;
        const b_declared = b.declared.get() orelse return false;
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
                // Reading a field by name, merging a substituted remainder,
                // and deriving declared slots all rely on this order.
                if (std.debug.runtime_safety) {
                    for (record.fields[0..record.fields.len -| 1], record.fields[@min(1, record.fields.len)..]) |prev, next| {
                        std.debug.assert(self.nameLessThan(prev.name, next.name));
                    }
                }
                try self.keyU32(@intCast(record.fields.len));
                for (record.fields) |field| {
                    try self.keyU32(@backingInt(field.name));
                    try self.keyType(field.ty);
                    try self.scratch_key.append(self.allocator, @backingInt(field.kind));
                    try self.keyU32(field.slot_variable);
                    try self.scratch_key.append(self.allocator, @intFromBool(field.default.get() != null));
                    if (field.default.get()) |default| {
                        try self.keyU32(@backingInt(default.module));
                        try self.keyU32(default.ordinal);
                    }
                    closed = closed and self.isClosed(field.ty) and field.kind != .variable_slot;
                }
                try self.keyU32(if (record.ext) |ext| @backingInt(ext) else std.math.maxInt(u32));
                if (record.ext) |ext| closed = closed and self.isClosed(ext);
                if (record.declared) |declared| {
                    try self.keySpan(declared.padding);
                    try self.keyU32(@intCast(declared.entries.len));
                    for (declared.entries) |entry| {
                        try self.scratch_key.append(self.allocator, @backingInt(std.meta.activeTag(entry)));
                        try self.keyU32(switch (entry) {
                            .named => |name| @backingInt(name),
                            .padding => |ordinal| ordinal,
                        });
                    }
                    for (declared.padding) |padding| closed = closed and self.isClosed(padding);
                }
                try self.keyU32(if (record.tail_node) |position| position else std.math.maxInt(u32));
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
                try self.keyU32(if (tag_union.tail_node) |position| position else std.math.maxInt(u32));
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
                .ext = .of(record.ext),
                .declared = if (record.declared) |declared| .some(.{
                    .padding = try self.addSpan(declared.padding),
                    .slots = try self.addDeclaredOrder(record.fields, declared),
                }) else .none,
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
        switch (node) {
            .record => |record| if (record.tail_node) |position| try self.tail_node_of.put(self.allocator, id, position),
            .tag_union => |tag_union| if (tag_union.tail_node) |position| try self.tail_node_of.put(self.allocator, id, position),
            .primitive, .empty_record, .tuple, .list, .box, .nominal, .function, .variable => {},
        }
        try self.closed_flags.append(self.allocator, closed);
        try self.variable_free_flags.append(self.allocator, self.inputVariableFree(node));
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

    /// The scheme position `ty` is a row node at, when it is a closed record
    /// or tag union whose row ends in a quantified uncommitted tail of the
    /// scope it was converted in (`tail_node_of`).
    pub fn tailNode(self: *const Store, ty: TypeId) ?u32 {
        return self.tail_node_of.get(ty);
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

    /// Stores `declared`'s entries and the slots derived from them over
    /// `fields`, which are sorted by label (see `DeclaredOrder.slots`).
    fn addDeclaredOrder(self: *Store, fields: []const Field, declared: DeclaredOrderInput) Allocator.Error!SlotSpan {
        const arena = self.span_arena.allocator();
        const slots = try arena.alloc(u32, fields.len + declared.padding.len);
        const named = try self.allocator.alloc(bool, fields.len);
        defer self.allocator.free(named);
        @memset(named, false);
        var next_slot: usize = 0;
        for (declared.entries) |entry| {
            slots[next_slot] = switch (entry) {
                .named => |name| blk: {
                    const index = self.fieldIndex(fields, name) orelse invariant("a declared field is absent from its nominal's record row");
                    std.debug.assert(!named[index]);
                    named[index] = true;
                    break :blk index;
                },
                .padding => |ordinal| @intCast(fields.len + ordinal),
            };
            next_slot += 1;
        }
        for (named, 0..) |is_named, index| {
            if (is_named) continue;
            slots[next_slot] = @intCast(index);
            next_slot += 1;
        }
        std.debug.assert(next_slot == slots.len);
        try self.declared_slots.append(self.allocator, slots);
        try self.declared_entries.append(self.allocator, try arena.dupe(DeclaredEntry, declared.entries));
        return .{ .start = @intCast(self.declared_slots.items.len - 1), .len = @intCast(slots.len) };
    }

    /// The position of the field named `name` in `fields`, sorted by label.
    fn fieldIndex(self: *const Store, fields: []const Field, name: NameId) ?u32 {
        var low: usize = 0;
        var high: usize = fields.len;
        while (low < high) {
            const mid = low + (high - low) / 2;
            if (self.nameLessThan(fields[mid].name, name)) low = mid + 1 else high = mid;
        }
        if (low < fields.len and fields[low].name == name) return @intCast(low);
        return null;
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

    /// Start a walk over checked types, which records the types it visits
    /// with `visitChecked` and ends with `endCheckedWalk`. Walks do not nest.
    pub fn beginCheckedWalk(self: *Store) void {
        std.debug.assert(self.checked_walk.count() == 0);
    }

    /// Whether the current walk meets `ty` for the first time, recording it.
    pub fn visitChecked(self: *Store, ty: checked.CheckedTypeId) Allocator.Error!bool {
        return !(try self.checked_walk.getOrPut(self.allocator, ty)).found_existing;
    }

    pub fn endCheckedWalk(self: *Store) void {
        self.checked_walk.clearRetainingCapacity();
    }

    pub fn primitive(self: *Store, value: Primitive) Allocator.Error!TypeId {
        const slot = self.primitive_ids.getPtr(value);
        if (slot.*) |known| return known;
        const id = try self.intern(.{ .primitive = value });
        slot.* = id;
        return id;
    }

    pub fn variable(self: *Store, index: u32) Allocator.Error!TypeId {
        // Scope variables count up from zero; a relation's placeholder
        // variables sit far above them (`Program.relateThroughRequirements`)
        // and are interned without a slot here.
        if (index >= cached_variable_count) return try self.intern(.{ .variable = index });
        if (index < self.variable_ids.items.len) {
            if (self.variable_ids.items[index].get()) |known| return known;
        } else try self.variable_ids.appendNTimes(self.allocator, .none, index + 1 - self.variable_ids.items.len);
        const id = try self.intern(.{ .variable = index });
        self.variable_ids.items[index] = .some(id);
        return id;
    }

    /// How many of the lowest variable indices `variable` keeps a node for.
    const cached_variable_count = 1 << 12;

    pub fn emptyRecord(self: *Store) Allocator.Error!TypeId {
        if (self.empty_record_id) |known| return known;
        const id = try self.intern(.empty_record);
        self.empty_record_id = id;
        return id;
    }

    /// The empty tag union: a type with no values.
    pub fn emptyTagUnion(self: *Store) Allocator.Error!TypeId {
        if (self.empty_tag_union_id) |known| return known;
        const id = try self.intern(.{ .tag_union = .{ .tags = &.{}, .ext = null } });
        self.empty_tag_union_id = id;
        return id;
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
    record: struct { fields: []const Field, ext: ?TypeId, declared: ?DeclaredOrderInput = null, tail_node: ?u32 = null },
    tuple: []const TypeId,
    tag_union: struct { tags: []const TagInput, ext: ?TypeId, tail_node: ?u32 = null },
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
        /// The checked payload of `ty`, read once by `leaf`.
        payload: checked.CheckedTypePayload,
        /// `rootHasIdentityVariables` of `ty`, as `leaf` found it; null
        /// when the converter seals undetermined kinds and memoizes nothing.
        identity_variables: ?bool,
        /// The children converted so far, in `results`.
        results_start: usize,
        /// Children still to convert, in `pending`.
        pending_start: usize,
        pending_next: usize,
        /// Whether `ty` is a record or tag union whose row ends in an
        /// uncommitted tail (`ClosedMemo`), read where `pushFrame` finds the
        /// row's end.
        ends_in_uncommitted_tail: bool,
    };

    /// Convert `ty`, read in `module`, binding the variables `env` names.
    /// A variable `env` does not bind must seal to its row default or its
    /// numeric default; any other unbound variable is an invariant failure.
    pub fn convert(self: Converter, module: ModuleIndex, ty: checked.CheckedTypeId, env: Env) Allocator.Error!TypeId {
        if (env.lazy_substitution != null) return LazySubstitution.convert(self, module, ty, env);
        const allocator = self.store.allocator;
        var frames: std.ArrayList(Frame) = .empty;
        defer frames.deinit(allocator);
        var results: std.ArrayList(TypeId) = .empty;
        defer results.deinit(allocator);
        var pending: std.ArrayList(Child) = .empty;
        defer pending.deinit(allocator);

        // Whether each converted child reaches a record or tag union a scope
        // can name as a closed row node, parallel to `results`.
        var reaches: std.ArrayList(bool) = .empty;
        defer reaches.deinit(allocator);

        // A scope that names closed row nodes converts a type holding one to
        // the node's marked type or to the use's whole value, so a type
        // reaching one is shared only between scopes that name none.
        var shared: ?bool = null;
        var root_reaches = false;
        switch (try self.leaf(module, ty, env, &shared, &root_reaches)) {
            .done => |done| return done,
            .composite => |composite| try self.pushFrame(&frames, &pending, results.items.len, module, ty, composite),
        }
        while (frames.items.len > 0) {
            const top = &frames.items[frames.items.len - 1];
            if (top.pending_next < pending.items.len) {
                const child = pending.items[top.pending_next];
                top.pending_next += 1;
                var child_reaches = false;
                switch (try self.leaf(child.module, child.ty, env, &shared, &child_reaches)) {
                    .done => |done| {
                        try results.append(allocator, done);
                        try reaches.append(allocator, child_reaches);
                    },
                    .composite => |composite| {
                        if (frames.items.len > 1_000_000) invariant("checked type conversion nested past every finite type");
                        try self.pushFrame(&frames, &pending, results.items.len, child.module, child.ty, composite);
                    },
                }
                continue;
            }
            const frame = frames.pop().?;
            const children = results.items[frame.results_start..];
            const built = try self.finish(frame.module, frame.ty, frame.payload, children, env);
            const built_reaches = frame.ends_in_uncommitted_tail or std.mem.findScalar(bool, reaches.items[frame.results_start..], true) != null;
            results.shrinkRetainingCapacity(frame.results_start);
            reaches.shrinkRetainingCapacity(frame.results_start);
            pending.shrinkRetainingCapacity(frame.pending_start);
            // A type with no identity variables converts the same under
            // every environment that names no closed row node it reaches.
            const memo: Store.ClosedMemo = .{ .ty = built, .reaches_row_node = built_reaches };
            if (frame.identity_variables == false and (!built_reaches or try self.namesNoRow(env, &shared))) {
                try self.store.closed_memo.put(allocator, .{ .module = frame.module, .ty = frame.ty }, memo);
            } else if (schemeKey(frame.module, frame.ty, env)) |key| {
                if (!self.seal_undetermined_kinds) try self.store.scheme_memo.put(allocator, key, memo);
            }
            if (frames.items.len == 0) return built;
            try results.append(allocator, built);
            try reaches.append(allocator, built_reaches);
        }
        unreachable; // the loop returns when the outermost frame finishes
    }

    const Child = struct { module: ModuleIndex, ty: checked.CheckedTypeId };

    /// The `Store.scheme_memo` key of `ty` under `env`, when `env` is a
    /// worker's own scheme environment.
    fn schemeKey(module: ModuleIndex, ty: checked.CheckedTypeId, env: Env) ?Store.SchemeKey {
        const positions = env.positions orelse return null;
        return .{ .module = module, .ty = ty, .env_module = env.module, .positions = @intFromPtr(positions), .bound = @intFromPtr(env.bound.ptr), .bound_len = env.bound.len };
    }

    /// The app type the program solves platform requirement identity
    /// variable `ty` to, or null for any other type.
    pub fn platformIdentity(self: Converter, module: ModuleIndex, ty: checked.CheckedTypeId) ?TypeId {
        const identities = self.platform_identities orelse return null;
        return identities.get(.{ .module = module, .ty = ty });
    }

    pub fn rootHasIdentityVariables(view: *const ModuleView, ty: checked.CheckedTypeId) Allocator.Error!bool {
        return try view.types().rootHasIdentityVariables(ty);
    }

    /// Whether `env` names no record or tag union, so it converts every
    /// identity-free type as every other such environment does, decided on
    /// first need into `shared`. A scope's closed row nodes are the only
    /// records and tag unions an environment names (`tailNodeAt`), bound to
    /// a `variable` node in the scope itself and to the use's whole value in
    /// an instantiation.
    fn namesNoRow(self: Converter, env: Env, shared: *?bool) Allocator.Error!bool {
        if (shared.*) |known| return known;
        const names_none = for (env.vars) |variable| {
            switch (self.modules.get(env.module).types().payload(variable)) {
                .record, .tag_union => break false,
                .pending, .err, .flex, .rigid, .alias, .tuple, .nominal, .function, .empty_record, .empty_tag_union, .field_kind => {},
            }
        } else true;
        shared.* = names_none;
        return names_none;
    }

    /// The position of the closed row node `env` binds at `index`, if it
    /// binds one: a scheme position holding the checked record or tag union
    /// whose row ends in a quantified uncommitted tail
    /// (`Program.templateScope`), bound to a `variable` node, the one the
    /// converting scope names that position by. Such a record or tag union
    /// converts to its closed type marked with that position
    /// (`Store.tailNode`). An instantiation binds the position to the use's
    /// whole value, which the record or tag union converts to.
    fn tailNodeAt(self: Converter, env: Env, index: usize) ?u32 {
        const position = switch (self.store.get(env.bound[index])) {
            .variable => |position| position,
            .primitive, .empty_record, .record, .tuple, .tag_union, .list, .box, .nominal, .function => return null,
        };
        return switch (self.modules.get(env.module).types().payload(env.vars[index])) {
            .record, .tag_union => position,
            .pending, .err, .flex, .rigid, .alias, .tuple, .nominal, .function, .empty_record, .empty_tag_union, .field_kind => null,
        };
    }

    /// The position `ty` is a closed row node at in `env`'s scope, if it is
    /// one (`tailNodeAt`).
    fn tailNodeOf(self: Converter, module: ModuleIndex, ty: checked.CheckedTypeId, env: Env) ?u32 {
        const index = env.lookupIndex(module, ty) orelse return null;
        return self.tailNodeAt(env, index);
    }

    /// What `leaf` finds for a type: its node, or the checked payload of a
    /// type whose children convert first, with whether it reaches an
    /// identity variable (`Frame.identity_variables`).
    const Leaf = union(enum) {
        done: TypeId,
        composite: struct { payload: checked.CheckedTypePayload, identity_variables: ?bool },
    };

    /// The node for `ty` when it needs no children converted first.
    /// `reaches` says whether the node reaches a record or tag union a
    /// scope can name as a closed row node (`ClosedMemo`).
    fn leaf(self: Converter, module: ModuleIndex, ty: checked.CheckedTypeId, env: Env, shared: *?bool, reaches: *bool) Allocator.Error!Leaf {
        reaches.* = false;
        if (env.lookupIndex(module, ty)) |index| {
            // A closed row node of the scope converts as its own structure.
            if (self.tailNodeAt(env, index) == null) {
                // A closed row node an instantiation binds to the use's
                // whole value: the scope names the record or tag union.
                reaches.* = switch (self.modules.get(env.module).types().payload(env.vars[index])) {
                    .record, .tag_union => true,
                    .pending, .err, .flex, .rigid, .alias, .tuple, .nominal, .function, .empty_record, .empty_tag_union, .field_kind => false,
                };
                return .{ .done = env.bound[index] };
            }
        }
        if (self.platformIdentity(module, ty)) |solved| return .{ .done = solved };
        const view = self.modules.get(module);
        const identity_variables: ?bool = if (self.seal_undetermined_kinds) null else try rootHasIdentityVariables(view, ty);
        if (identity_variables == false) {
            if (self.store.closed_memo.get(.{ .module = module, .ty = ty })) |memo| {
                if (!memo.reaches_row_node or try self.namesNoRow(env, shared)) {
                    reaches.* = memo.reaches_row_node;
                    return .{ .done = memo.ty };
                }
            }
        }
        if (!self.seal_undetermined_kinds) {
            if (schemeKey(module, ty, env)) |key| {
                if (self.store.scheme_memo.get(key)) |memo| {
                    reaches.* = memo.reaches_row_node;
                    return .{ .done = memo.ty };
                }
            }
        }
        const payload = view.types().payload(ty);
        const node: ?TypeId = switch (payload) {
            .pending => invariant("a pending checked type reached Boxy lowering"),
            .err => invariant("an erroneous checked type reached Boxy lowering"),
            .flex, .rigid => |variable| try self.sealVariable(variable),
            .empty_record => try self.store.emptyRecord(),
            .empty_tag_union => try self.store.emptyTagUnion(),
            .nominal => |nominal| switch (nominal.representation) {
                .builtin => |builtin_nominal| switch (checked.builtinRuntimeEncoding(builtin_nominal)) {
                    .primitive => |prim| try self.store.primitive(prim),
                    .bool_tag_union, .try_nominal, .list, .box, .declared_backing, .parse_tag_union_spec, .fields, .field => null,
                },
                .local_declaration, .imported_declaration, .local_box_payload_capability, .imported_box_payload_capability, .opaque_without_backing => null,
            },
            .field_kind => |kind| try self.store.committedKind(kind == .optional),
            .alias, .record, .tuple, .function, .tag_union => null,
        };
        if (node) |done| return .{ .done = done };
        return .{ .composite = .{ .payload = payload, .identity_variables = identity_variables } };
    }

    /// The distinct flex and rigid variables the checked type `root` of
    /// `module` mentions, in the order a depth-first walk first reaches them.
    pub fn typeVariables(self: Converter, allocator: Allocator, module: ModuleIndex, root: checked.CheckedTypeId) Allocator.Error![]const checked.CheckedTypeId {
        const types = self.modules.get(module).types();
        const scratch = self.store.allocator;
        var found: std.ArrayList(checked.CheckedTypeId) = .empty;
        var pending: std.ArrayList(checked.CheckedTypeId) = .empty;
        defer pending.deinit(scratch);
        self.store.beginCheckedWalk();
        defer self.store.endCheckedWalk();
        try pending.append(scratch, root);
        while (pending.pop()) |ty| {
            if (!try self.store.visitChecked(ty)) continue;
            const start = pending.items.len;
            switch (types.payload(ty)) {
                .flex, .rigid => try found.append(allocator, ty),
                .alias => |alias| {
                    try pending.appendSlice(scratch, alias.args);
                    try pending.append(scratch, alias.backing);
                },
                .record => |record| {
                    for (0..record.fields.len()) |field_index| try pending.append(scratch, record.fields.get(field_index).ty);
                    try pending.append(scratch, record.ext);
                },
                .tuple => |items| try pending.appendSlice(scratch, items),
                .tag_union => |tag_union| {
                    for (0..tag_union.tags.len()) |tag_index| try pending.appendSlice(scratch, tag_union.tags.get(tag_index).args);
                    try pending.append(scratch, tag_union.ext);
                },
                .function => |function| {
                    try pending.appendSlice(scratch, function.args);
                    try pending.append(scratch, function.ret);
                },
                .nominal => |nominal| try pending.appendSlice(scratch, nominal.args),
                .pending, .field_kind, .err, .empty_record, .empty_tag_union => {},
            }
            // Children are visited in the order they were listed.
            std.mem.reverse(checked.CheckedTypeId, pending.items[start..]);
        }
        return found.items;
    }

    /// The type a scheme variable takes where nothing relates it to a use:
    /// what it seals to when no scope binds it.
    pub fn sealedScheme(self: Converter, module: ModuleIndex, variable: checked.CheckedTypeId) Allocator.Error!TypeId {
        return switch (self.modules.get(module).types().payload(variable)) {
            .flex, .rigid => |payload| try self.sealVariable(payload),
            .pending, .field_kind, .err, .alias, .record, .tuple, .nominal, .function, .empty_record, .tag_union, .empty_tag_union => try self.convert(module, variable, .{ .module = module, .vars = &.{}, .bound = &.{} }),
        };
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

    fn pushFrame(self: Converter, frames: *std.ArrayList(Frame), pending: *std.ArrayList(Child), results_start: usize, module: ModuleIndex, ty: checked.CheckedTypeId, composite: @FieldType(Leaf, "composite")) Allocator.Error!void {
        const allocator = self.store.allocator;
        const pending_start = pending.items.len;
        const ends_in_uncommitted_tail = try self.appendChildren(pending, module, composite.payload, false);
        try frames.append(allocator, .{
            .module = module,
            .ty = ty,
            .payload = composite.payload,
            .identity_variables = composite.identity_variables,
            .results_start = results_start,
            .pending_start = pending_start,
            .pending_next = pending_start,
            .ends_in_uncommitted_tail = ends_in_uncommitted_tail,
        });
    }

    /// Lazy substitution and conversion must visit the same flattened row children.
    /// Lazy substitution additionally binds the field kinds that finish reads.
    fn appendChildren(self: Converter, pending: *std.ArrayList(Child), module: ModuleIndex, payload: checked.CheckedTypePayload, comptime include_field_kinds: bool) Allocator.Error!bool {
        const allocator = self.store.allocator;
        const view = self.modules.get(module);
        const types = view.types();
        var ends_in_uncommitted_tail = false;
        switch (payload) {
            .alias => |alias| try pending.append(allocator, .{ .module = module, .ty = alias.backing }),
            .record => |record| {
                var current_fields = record.fields;
                var current = record.ext;
                while (true) {
                    for (0..current_fields.len()) |field_index| {
                        const field = current_fields.get(field_index);
                        try pending.append(allocator, .{ .module = module, .ty = field.ty });
                        if (include_field_kinds and !self.seal_undetermined_kinds)
                            if (field.kind.undeterminedVariable()) |variable| try pending.append(allocator, .{ .module = module, .ty = variable });
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
                    .flex => |flex| {
                        ends_in_uncommitted_tail = flex.row_tail == .uncommitted_record;
                        try pending.append(allocator, .{ .module = module, .ty = current });
                    },
                    .pending, .field_kind, .err, .rigid, .alias, .record, .tuple, .nominal, .function, .tag_union, .empty_tag_union => try pending.append(allocator, .{ .module = module, .ty = current }),
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
                    .empty_tag_union => ends_in_uncommitted_tail = types.uncommittedTagTail(current),
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
        return ends_in_uncommitted_tail;
    }

    fn finish(self: Converter, module: ModuleIndex, ty: checked.CheckedTypeId, payload: checked.CheckedTypePayload, children: []const TypeId, env: Env) Allocator.Error!TypeId {
        const allocator = self.store.allocator;
        const view = self.modules.get(module);
        const types = view.types();
        const name_store = view.nameStore();
        switch (payload) {
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
                var child_index: usize = 0;
                var current_fields = record.fields;
                var current = record.ext;
                while (true) {
                    for (0..current_fields.len()) |field_index| {
                        const field = current_fields.get(field_index);
                        const field_name = try self.store.internName(name_store.recordFieldLabelText(field.name));
                        try fields.append(allocator, try self.convertedField(module, field_name, children[child_index], field.kind, env));
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
                    .record => |ext_record| if (ext_record.ext == .none) {
                        try fields.appendSlice(allocator, self.store.fieldSpan(ext_record.fields));
                        ext = null;
                    },
                    .variable => {},
                    .primitive, .tuple, .tag_union, .list, .box, .nominal, .function => invariant("a record row's remainder converted to a non-record type"),
                };
                const tail_node = self.tailNodeOf(module, ty, env);
                if (fields.items.len == 0 and ext == null and tail_node == null) return try self.store.emptyRecord();
                const store = self.store;
                std.mem.sort(Field, fields.items, store, struct {
                    fn lessThan(s: *Store, a: Field, b: Field) bool {
                        return s.nameLessThan(a.name, b.name);
                    }
                }.lessThan);
                return try self.store.intern(.{ .record = .{ .fields = fields.items, .ext = ext, .tail_node = tail_node } });
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
                return try self.store.intern(.{ .tag_union = .{ .tags = tags.items, .ext = ext, .tail_node = self.tailNodeOf(module, ty, env) } });
            },
            .nominal => |nominal| return try self.nominalNode(module, nominal, children),
            .pending, .field_kind, .err, .flex, .rigid, .empty_record, .empty_tag_union => unreachable, // `leaf` answers these
        }
    }

    /// The type of the nominal `ty` of `module` names (through aliases),
    /// applied to `args` rather than to its own arguments: its declared
    /// arguments, then its context arguments.
    pub fn nominalAt(self: Converter, module: ModuleIndex, ty: checked.CheckedTypeId, args: []const TypeId) Allocator.Error!TypeId {
        const types = self.modules.get(module).types();
        var current = ty;
        while (true) switch (types.payload(current)) {
            .alias => |alias| current = alias.backing,
            .nominal => |nominal| {
                if (nominal.representation == .builtin) switch (checked.builtinRuntimeEncoding(nominal.representation.builtin)) {
                    .primitive => |prim| return try self.store.primitive(prim),
                    .bool_tag_union, .try_nominal, .list, .box, .declared_backing, .parse_tag_union_spec, .fields, .field => {},
                };
                return try self.nominalNode(module, nominal, args);
            },
            .pending, .field_kind, .err, .flex, .rigid, .record, .tuple, .function, .empty_record, .tag_union, .empty_tag_union => invariant("a stored named type's occurrence is not a nominal type"),
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
                        .parse_tag_union_spec => {
                            if (children.len != 4) invariant("a tag-union spec must carry shape, encoding, state, and error");
                            // The spec uses the erased-callable representation.
                            // Preserve its whole semantic contract here; codec
                            // lowering supplies the options-record call signature.
                            return try self.store.intern(.{ .function = .{ .args = children[1..], .ret = children[0], .effectful = false } });
                        },
                        .bool_tag_union, .try_nominal, .declared_backing => {},
                    },
                    .local_declaration, .imported_declaration, .local_box_payload_capability, .imported_box_payload_capability, .opaque_without_backing => {},
                }
                const decl = try self.declarationKey(view, nominal);
                const decl_key = decl orelse NominalDeclKey{ .module = module, .decl = @fromBackingInt(std.math.maxInt(u32)) };
                return try self.store.intern(.{ .nominal = .{
                    .decl = decl_key,
                    .args = children,
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
    fn convertedField(self: Converter, module: ModuleIndex, name: NameId, ty: TypeId, kind: checked.CheckedFieldKind, env: Env) Allocator.Error!Field {
        return switch (kind.tag) {
            .required => .{ .name = name, .ty = ty, .kind = .inline_slot },
            .defaulted => .{ .name = name, .ty = ty, .kind = .inline_slot, .default = .some(try self.modules.fieldDefault(module, kind.default)) },
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
                    if (record.ext.get()) |ext| try pending.append(store.allocator, ext);
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

    /// The name of nominal declaration `decl`, which is its own name
    /// whatever its text (design.md "Name Identity"): the runtime node's id
    /// digests the declaring module's link name and the declaration.
    fn declName(self: Converter, decl: NominalDeclKey, text: []const u8) Allocator.Error!NameId {
        if (self.store.decl_name_ids.get(decl)) |existing| return existing;
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

    pub fn backing(self: Converter, nominal_ty: TypeId) Allocator.Error!TypeId {
        if (self.store.backing_memo.get(nominal_ty)) |memo| return memo;
        const nominal = switch (self.store.get(nominal_ty)) {
            .nominal => |value| value,
            .primitive, .empty_record, .record, .tuple, .tag_union, .list, .box, .function, .variable => invariant("asked for the backing of a non-nominal type"),
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
            .record => |record| .{ self.store.fieldSpan(record.fields), record.ext.get() },
            .empty_record => .{ &.{}, null },
            .primitive, .tuple, .tag_union, .list, .box, .nominal, .function, .variable => invariant("a nominal declaration with unnamed fields does not back onto a record"),
        };
        const padding = try allocator.alloc(TypeId, declared_padding.len);
        defer allocator.free(padding);
        for (declared_padding, padding) |checked_ty, *slot| slot.* = try self.convert(module, checked_ty, env);
        const declared_fields = declaration.declaredFields(view.types());
        const entries = try allocator.alloc(DeclaredEntry, declared_fields.len);
        defer allocator.free(entries);
        for (declared_fields, entries) |declared, *entry| entry.* = switch (declared) {
            .named => |label| .{ .named = try self.store.internName(view.nameStore().recordFieldLabelText(label)) },
            .padding => |ordinal| .{ .padding = ordinal },
        };
        return try self.store.intern(.{ .record = .{
            .fields = fields,
            .ext = ext,
            .declared = .{ .padding = padding, .entries = entries },
        } });
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

test "record interning preserves immutable field default identities" {
    var store = Store.init(std.testing.allocator);
    defer store.deinit();
    var empty_modules: Common.EmptyCheckedModulesForTest = undefined;
    try empty_modules.init(std.testing.allocator);
    defer empty_modules.deinit(std.testing.allocator);
    var modules: Modules = .{ .allocator = std.testing.allocator };
    defer modules.deinit();
    var view = empty_modules.view();
    view.key = .{ .bytes = @splat(1) };
    view.module_identity.stable_hash = view.key.bytes;
    const first_module = try modules.add(view);
    view.key.bytes = @splat(2);
    view.module_identity.stable_hash = view.key.bytes;
    const second_module = try modules.add(view);
    const count = try store.internName("count");
    const u8_ty = try store.primitive(.u8);
    var fields = [_]Field{.{ .name = count, .ty = u8_ty, .kind = .inline_slot }};
    const required = try store.intern(.{ .record = .{ .fields = &fields, .ext = null } });
    const first_default: FieldDefault = .{ .module = first_module, .ordinal = 10 };
    fields[0].default = .some(first_default);
    const first = try store.intern(.{ .record = .{ .fields = &fields, .ext = null } });
    try std.testing.expectEqual(first, try store.intern(.{ .record = .{ .fields = &fields, .ext = null } }));
    fields[0].default = .some(.{ .module = first_module, .ordinal = 20 });
    const other_expr = try store.intern(.{ .record = .{ .fields = &fields, .ext = null } });
    fields[0].default = .some(.{ .module = second_module, .ordinal = 10 });
    const other_module = try store.intern(.{ .record = .{ .fields = &fields, .ext = null } });
    try std.testing.expect(required != first);
    try std.testing.expect(first != other_expr);
    try std.testing.expect(first != other_module);
    try std.testing.expect(other_expr != other_module);
    try std.testing.expectEqual(@as(?FieldDefault, null), store.fieldSpan(store.get(required).record.fields)[0].default.get());
    try std.testing.expectEqual(first_default, store.fieldSpan(store.get(first).record.fields)[0].default.get().?);
    fields[0].default = .none;
    try std.testing.expectEqual(required, try store.intern(.{ .record = .{ .fields = &fields, .ext = null } }));
    fields[0].default = .some(first_default);
    const padded = try store.intern(.{ .record = .{
        .fields = &fields,
        .ext = null,
        .declared = .{ .padding = &.{u8_ty}, .entries = &.{ .{ .named = count }, .{ .padding = 0 } } },
    } });
    try std.testing.expectEqual(first, try store.recordRow(padded));
}

test "a padded backing is a type of its own, whose unnamed fields count toward it" {
    var store = Store.init(std.testing.allocator);
    defer store.deinit();
    const a = try store.internName("a");
    const z = try store.internName("z");
    const number = try store.primitive(.u32);
    const fields: []const Field = &.{ .{ .name = a, .ty = number, .kind = .inline_slot }, .{ .name = z, .ty = number, .kind = .inline_slot } };
    const z_pad_a: []const DeclaredEntry = &.{ .{ .named = z }, .{ .padding = 0 }, .{ .named = a } };
    const plain = try store.intern(.{ .record = .{ .fields = fields, .ext = null } });
    const padded = try store.intern(.{ .record = .{ .fields = fields, .ext = null, .declared = .{ .padding = &.{number}, .entries = z_pad_a } } });
    const other_order = try store.intern(.{ .record = .{ .fields = fields, .ext = null, .declared = .{ .padding = &.{number}, .entries = &.{ .{ .named = a }, .{ .padding = 0 }, .{ .named = z } } } } });
    const other_padding = try store.intern(.{ .record = .{ .fields = fields, .ext = null, .declared = .{ .padding = &.{try store.primitive(.u64)}, .entries = z_pad_a } } });
    try std.testing.expect(plain != padded);
    try std.testing.expect(padded != other_order);
    try std.testing.expect(padded != other_padding);
    try std.testing.expectEqual(padded, try store.intern(.{ .record = .{ .fields = fields, .ext = null, .declared = .{ .padding = &.{number}, .entries = z_pad_a } } }));

    try std.testing.expect(!store.sameRecordOrder(store.get(plain).record, store.get(padded).record));
    try std.testing.expect(!store.sameRecordOrder(store.get(padded).record, store.get(plain).record));
    try std.testing.expect(!store.sameRecordOrder(store.get(padded).record, store.get(other_order).record));
    try std.testing.expect(store.sameRecordOrder(store.get(padded).record, store.get(other_padding).record));
    try std.testing.expectEqualSlices(TypeId, &.{number}, store.paddingTypes(store.get(padded).record));
    try std.testing.expectEqual(@as(usize, 0), store.paddingTypes(store.get(plain).record).len);

    try std.testing.expectEqual(plain, try store.recordRow(padded));
    try std.testing.expectEqual(plain, try store.recordRow(plain));
    try std.testing.expectEqual(number, try store.recordRow(number));
    const only_padding = try store.intern(.{ .record = .{ .fields = &.{}, .ext = null, .declared = .{ .padding = &.{number}, .entries = &.{.{ .padding = 0 }} } } });
    try std.testing.expectEqual(try store.emptyRecord(), try store.recordRow(only_padding));

    try std.testing.expect(store.isClosed(padded));
    const open = try store.intern(.{ .record = .{ .fields = fields, .ext = null, .declared = .{ .padding = &.{try store.variable(0)}, .entries = z_pad_a } } });
    try std.testing.expect(!store.isClosed(open));
}

test "a padded backing's fields that no entry names follow the entries in label order" {
    var store = Store.init(std.testing.allocator);
    defer store.deinit();
    const a = try store.internName("a");
    const b = try store.internName("b");
    const z = try store.internName("z");
    const number = try store.primitive(.u64);
    const byte = try store.primitive(.u8);
    const entries: []const DeclaredEntry = &.{ .{ .named = z }, .{ .padding = 0 } };

    // `Pad(r) := { z : U64, _ : U8, ..r }` at `r = { a : U64 }`.
    const one = try store.intern(.{ .record = .{
        .fields = &.{ .{ .name = a, .ty = number, .kind = .inline_slot }, .{ .name = z, .ty = number, .kind = .inline_slot } },
        .ext = null,
        .declared = .{ .padding = &.{byte}, .entries = entries },
    } });
    try std.testing.expectEqualSlices(u32, &.{ 1, 2, 0 }, store.declaredSlots(store.get(one).record.declared.get().?));
    try std.testing.expectEqualSlices(DeclaredEntry, entries, store.declaredEntries(store.get(one).record.declared.get().?));

    // At `r = { a : U64, b : U64 }`.
    const two = try store.intern(.{ .record = .{
        .fields = &.{ .{ .name = a, .ty = number, .kind = .inline_slot }, .{ .name = b, .ty = number, .kind = .inline_slot }, .{ .name = z, .ty = number, .kind = .inline_slot } },
        .ext = null,
        .declared = .{ .padding = &.{byte}, .entries = entries },
    } });
    try std.testing.expectEqualSlices(u32, &.{ 2, 3, 0, 1 }, store.declaredSlots(store.get(two).record.declared.get().?));

    // Generic in `r`, whose fields are not yet known.
    const open = try store.intern(.{ .record = .{
        .fields = &.{.{ .name = z, .ty = number, .kind = .inline_slot }},
        .ext = try store.variable(0),
        .declared = .{ .padding = &.{byte}, .entries = entries },
    } });
    try std.testing.expectEqualSlices(u32, &.{ 0, 1 }, store.declaredSlots(store.get(open).record.declared.get().?));
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

test "adapter signatures retain variables behind erased callable layouts" {
    var store = Store.init(std.testing.allocator);
    defer store.deinit();
    const number = try store.primitive(.u64);
    const variable = try store.variable(0);
    const generic = try store.intern(.{ .function = .{ .args = &.{variable}, .ret = variable, .effectful = false } });
    const concrete = try store.intern(.{ .function = .{ .args = &.{number}, .ret = number, .effectful = false } });
    const nested = try store.intern(.{ .function = .{ .args = &.{generic}, .ret = number, .effectful = false } });
    try std.testing.expect(store.isClosed(generic));
    try std.testing.expect(!store.isVariableFree(generic));
    try std.testing.expect(!store.isVariableFree(nested));
    try std.testing.expect(store.isVariableFree(concrete));
    try std.testing.expect(!store.isVariableFree(try store.intern(.{ .list = generic })));
}

// Independent pre-memo traversal, used only by the regression tests.
fn hasErrorUnmemoized(allocator: Allocator, modules: *const Modules, module: ModuleIndex, root: checked.CheckedTypeId) Allocator.Error!bool {
    const types = modules.get(module).types();
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

/// Register a distinct fixture module through the real module-ID producer:
/// `empty`'s module under its own identity, holding `store`'s types.
fn addErrorTestModule(empty: *const Common.EmptyCheckedModulesForTest, modules: *Modules, store: *checked.CheckedTypeStore) Allocator.Error!ModuleIndex {
    if (store.synthesized_keys == null) {
        store.attachSynthesizedKeys(try checked.SynthesizedTypeKeys.create(modules.allocator, std.testing.io, store.roots.items().len));
    }
    var identity: [32]u8 = @splat(0);
    std.mem.writeInt(u64, identity[0..8], @intCast(modules.views.items.len), .little);
    var view = empty.view();
    view.key = .{ .bytes = identity };
    view.module_identity.stable_hash = identity;
    view.checked_types = store.view();
    return try modules.add(view);
}

fn errorTestStore(allocator: Allocator) Allocator.Error!checked.CheckedTypeStore {
    var store = checked.CheckedTypeStore{};
    store.attachSynthesizedKeys(try checked.SynthesizedTypeKeys.create(allocator, std.testing.io, 0));
    return store;
}

test "Boxy literal substitution visits only demanded substitutions and preserves lexical scope" {
    const a = std.testing.allocator;
    var arena = std.heap.ArenaAllocator.init(a);
    defer arena.deinit();
    var checked_store = try errorTestStore(a);
    defer checked_store.deinit(a);
    const record = try checked_store.appendSyntheticPayloadRoot(a, .empty_record);
    const tags = try checked_store.appendSyntheticPayloadRoot(a, .empty_tag_union);
    const first = try checked_store.reserveSyntheticTypeRoot(a);
    try checked_store.declareIdentityInstance(a, first);
    try checked_store.fillSyntheticTypeRoot(a, first, .{ .flex = .{} });
    const second = try checked_store.reserveSyntheticTypeRoot(a);
    try checked_store.declareIdentityInstance(a, second);
    try checked_store.fillSyntheticTypeRoot(a, second, .{ .flex = .{} });
    // Converting this unused argument would violate the record-row invariant.
    const unrelated = try checked_store.appendSyntheticPayloadRoot(a, .{ .record = .{ .fields = &.{}, .ext = tags } });
    const pair = try checked_store.appendSyntheticPayloadRoot(a, .{ .tuple = try a.dupe(checked.CheckedTypeId, &.{ first, second }) });
    var empty_modules: Common.EmptyCheckedModulesForTest = undefined;
    try empty_modules.init(a);
    defer empty_modules.deinit(a);
    var modules = Modules{ .allocator = a };
    defer modules.deinit();
    const module = try addErrorTestModule(&empty_modules, &modules, &checked_store);
    var store = Store.init(a);
    defer store.deinit();
    const converter: Converter = .{ .store = &store, .modules = &modules };
    const empty: Env = .{ .module = module, .vars = &.{}, .bound = &.{} };
    const outer = try LazySubstitution.init(arena.allocator(), module, &.{ first, second }, &.{ record, unrelated }, module, empty, empty);
    try std.testing.expectEqual(try store.emptyRecord(), try converter.convert(module, first, outer));
    try std.testing.expectEqual(@as(usize, 1), outer.lazy_substitution.?.memo.count());
    // The local's own second binding shadows the unused outer argument.
    const inner = try LazySubstitution.init(arena.allocator(), module, &.{second}, &.{tags}, module, empty, outer);
    const result = try converter.convert(module, pair, inner);
    const items = store.span(store.get(result).tuple);
    try std.testing.expectEqualSlices(TypeId, &.{ try store.emptyRecord(), try store.emptyTagUnion() }, items);
    const count = inner.lazy_substitution.?.memo.count();
    try std.testing.expectEqual(result, try converter.convert(module, pair, inner));
    try std.testing.expectEqual(count, inner.lazy_substitution.?.memo.count());
}

test "Boxy literal substitution shares conversion row flattening and binds field kinds" {
    const a = std.testing.allocator;
    var arena = std.heap.ArenaAllocator.init(a);
    defer arena.deinit();
    var canonical_names = names.CanonicalNameStore.init(a);
    defer canonical_names.deinit();
    const label = try canonical_names.internRecordFieldLabel("item");
    var checked_store = try errorTestStore(a);
    defer checked_store.deinit(a);
    const empty_record = try checked_store.appendSyntheticPayloadRoot(a, .empty_record);
    const optional = try checked_store.appendSyntheticPayloadRoot(a, .{ .field_kind = .optional });
    const value = try checked_store.reserveSyntheticTypeRoot(a);
    try checked_store.declareIdentityInstance(a, value);
    try checked_store.fillSyntheticTypeRoot(a, value, .{ .flex = .{} });
    const kind = try checked_store.reserveSyntheticTypeRoot(a);
    try checked_store.declareIdentityInstance(a, kind);
    try checked_store.fillSyntheticTypeRoot(a, kind, .{ .flex = .{} });
    const row = try checked_store.appendSyntheticPayloadRoot(a, .{ .record = .{
        .fields = try a.dupe(checked.CheckedRecordField, &.{.{ .name = label, .ty = value, .kind = .undetermined(kind) }}),
        .ext = empty_record,
    } });
    const whole = try checked_store.appendSyntheticPayloadRoot(a, .{ .record = .{ .fields = &.{}, .ext = row } });
    var empty_modules: Common.EmptyCheckedModulesForTest = undefined;
    try empty_modules.init(a);
    defer empty_modules.deinit(a);
    var modules = Modules{ .allocator = a };
    defer modules.deinit();
    const module = try addErrorTestModule(&empty_modules, &modules, &checked_store);
    modules.views.items[@backingInt(module)].view.canonical_names = &canonical_names;
    var store = Store.init(a);
    defer store.deinit();
    const converter: Converter = .{ .store = &store, .modules = &modules };
    const empty: Env = .{ .module = module, .vars = &.{}, .bound = &.{} };
    for ([_]TypeId{ try store.emptyTagUnion(), try store.variable(7) }) |row_value| {
        const row_binding: Env = .{ .module = module, .vars = &.{row}, .bound = &.{row_value} };
        const substituted = try LazySubstitution.init(arena.allocator(), module, &.{ value, kind }, &.{ empty_record, optional }, module, empty, row_binding);
        const concrete: Env = .{
            .module = module,
            .vars = &.{ value, kind, row },
            .bound = &.{ try store.emptyRecord(), try store.committedKind(true), row_value },
        };
        // Extensions flatten; a root's row marker still visits its fields,
        // whereas a root bound to a whole value consumes that value directly.
        try std.testing.expectEqual(try converter.convert(module, whole, concrete), try converter.convert(module, whole, substituted));
        try std.testing.expectEqual(try converter.convert(module, row, concrete), try converter.convert(module, row, substituted));
    }
}

test "Boxy literal substitution composes deep chains without native recursion" {
    const a = std.testing.allocator;
    var arena = std.heap.ArenaAllocator.init(a);
    defer arena.deinit();
    var checked_store = try errorTestStore(a);
    defer checked_store.deinit(a);
    const record = try checked_store.appendSyntheticPayloadRoot(a, .empty_record);
    const variable = try checked_store.reserveSyntheticTypeRoot(a);
    try checked_store.declareIdentityInstance(a, variable);
    try checked_store.fillSyntheticTypeRoot(a, variable, .{ .flex = .{} });
    var empty_modules: Common.EmptyCheckedModulesForTest = undefined;
    try empty_modules.init(a);
    defer empty_modules.deinit(a);
    var modules = Modules{ .allocator = a };
    defer modules.deinit();
    const module = try addErrorTestModule(&empty_modules, &modules, &checked_store);
    var store = Store.init(a);
    defer store.deinit();
    const converter: Converter = .{ .store = &store, .modules = &modules };
    const empty: Env = .{ .module = module, .vars = &.{}, .bound = &.{} };
    var env = try LazySubstitution.init(arena.allocator(), module, &.{variable}, &.{record}, module, empty, empty);
    for (0..4096) |_| {
        env = try LazySubstitution.init(arena.allocator(), module, &.{variable}, &.{variable}, module, env, empty);
    }
    try std.testing.expectEqual(try store.emptyRecord(), try converter.convert(module, variable, env));
}

test "Boxy error reachability separates modules and preserves proofs across append-only publication" {
    const a = std.testing.allocator;
    var first = try errorTestStore(a);
    defer first.deinit(a);
    var second = try errorTestStore(a);
    defer second.deinit(a);
    const bad = try first.appendSyntheticPayloadRoot(a, .err);
    const good = try second.appendSyntheticPayloadRoot(a, .empty_record);
    try std.testing.expectEqual(bad, good); // IDs alone cannot identify a node.
    var empty_modules: Common.EmptyCheckedModulesForTest = undefined;
    try empty_modules.init(a);
    defer empty_modules.deinit(a);
    var modules = Modules{ .allocator = a };
    defer modules.deinit();
    const first_module = try addErrorTestModule(&empty_modules, &modules, &first);
    const second_module = try addErrorTestModule(&empty_modules, &modules, &second);
    const errors = try ErrorTypes.create(a, &modules);
    defer errors.destroy();
    try std.testing.expect(try errors.hasError(first_module, bad));
    try std.testing.expect(!try errors.hasError(second_module, good));
    // Force the synthetic pools to grow. Refresh the published slice view
    // after the producer finishes; existing IDs still denote the same nodes.
    for (0..128) |_| {
        const fresh = try second.reserveSyntheticTypeRoot(a);
        try second.fillSyntheticTypeRoot(a, fresh, .empty_record);
    }
    const new_bad = try second.appendSyntheticPayloadRoot(a, .err);
    modules.views.items[@backingInt(second_module)].view.checked_types = second.view();
    try std.testing.expect(!try errors.hasError(second_module, good));
    try std.testing.expect(try errors.hasError(second_module, new_bad));
    try std.testing.expectEqual(@as(usize, 4), errors.stats().queries);
    try std.testing.expectEqual(@as(usize, 3), errors.stats().unique_expanded_nodes);
}

test "Boxy error reachability matches old traversal for late-positive cycles and false diamonds" {
    const a = std.testing.allocator;
    var store = try errorTestStore(a);
    defer store.deinit(a);
    var ids: [8]checked.CheckedTypeId = undefined;
    for (&ids) |*id| id.* = try store.reserveSyntheticTypeRoot(a);
    const edges = [_][]const usize{ &.{ 1, 2 }, &.{0}, &.{}, &.{ 4, 5 }, &.{ 6, 6 }, &.{6}, &.{7}, &.{6} };
    for (ids, edges, 0..) |id, children, index| {
        if (index == 2) {
            try store.fillSyntheticTypeRoot(a, id, .err);
        } else {
            const items = try a.alloc(checked.CheckedTypeId, children.len);
            for (items, children) |*item, child| item.* = ids[child];
            try store.fillSyntheticTypeRoot(a, id, .{ .tuple = items });
        }
    }
    var empty_modules: Common.EmptyCheckedModulesForTest = undefined;
    try empty_modules.init(a);
    defer empty_modules.deinit(a);
    var modules = Modules{ .allocator = a };
    defer modules.deinit();
    const module = try addErrorTestModule(&empty_modules, &modules, &store);
    for (0..2) |order| {
        const errors = try ErrorTypes.create(a, &modules);
        defer errors.destroy();
        for (0..3) |_| for (0..ids.len) |position| {
            const index = if (order == 0) position else ids.len - 1 - position;
            const expected = try hasErrorUnmemoized(a, &modules, module, ids[index]);
            try std.testing.expectEqual(index < 3, expected);
            try std.testing.expectEqual(expected, try errors.hasError(module, ids[index]));
        };
        try std.testing.expectEqual(ids.len, errors.stats().unique_expanded_nodes);
        try std.testing.expectEqual(ids.len * 3, errors.stats().queries);
    }
}

test "Boxy error reachability preserves value-error edges and field-kind semantics" {
    const a = std.testing.allocator;
    var store = try errorTestStore(a);
    defer store.deinit(a);
    var canonical_names = names.CanonicalNameStore.init(a);
    defer canonical_names.deinit();
    store.key_names = &canonical_names;
    const type_name = try canonical_names.internTypeName("Value");
    const origin = try canonical_names.internModuleIdentity(&(@as([32]u8, @splat(1))));
    const label = try canonical_names.internRecordFieldLabel("value");
    const tag = try canonical_names.internTagLabel("Value");
    const good = try store.appendSyntheticPayloadRoot(a, .empty_record);
    const empty_tags = try store.appendSyntheticPayloadRoot(a, .empty_tag_union);
    const bad = try store.appendSyntheticPayloadRoot(a, .err);
    const kind = try store.appendSyntheticPayloadRoot(a, .{ .field_kind = .optional });
    // The old predicate treats a published pending node as false. This
    // fixture never fills it while the analysis is alive.
    const pending = try store.reserveSyntheticTypeRoot(a);
    const flex = try store.reserveSyntheticTypeRoot(a);
    try store.declareIdentityInstance(a, flex);
    try store.fillSyntheticTypeRoot(a, flex, .{ .flex = .{} });
    const rigid = try store.reserveSyntheticTypeRoot(a);
    try store.declareIdentityInstance(a, rigid);
    try store.fillSyntheticTypeRoot(a, rigid, .{ .rigid = .{} });
    const alias_good = try store.appendSyntheticPayloadRoot(a, .{ .alias = .{
        .name = type_name,
        .origin_module = origin,
        .owner_module = .{ .bytes = @splat(1) },
        .backing = good,
        .args = try a.dupe(checked.CheckedTypeId, &.{bad}),
    } });
    const alias_bad = try store.appendSyntheticPayloadRoot(a, .{ .alias = .{
        .name = type_name,
        .origin_module = origin,
        .owner_module = .{ .bytes = @splat(1) },
        .backing = bad,
    } });
    const nominal_good = try store.appendSyntheticPayloadRoot(a, .{ .nominal = .{
        .name = type_name,
        .origin_module = origin,
        .owner_module = .{ .bytes = @splat(1) },
        .source_decl = 0,
        .is_opaque = true,
        .representation = .opaque_without_backing,
        .args = try a.dupe(checked.CheckedTypeId, &.{good}),
        .padding_field_types = try a.dupe(checked.CheckedTypeId, &.{bad}),
    } });
    const nominal_bad = try store.appendSyntheticPayloadRoot(a, .{ .nominal = .{
        .name = type_name,
        .origin_module = origin,
        .owner_module = .{ .bytes = @splat(1) },
        .source_decl = 0,
        .is_opaque = true,
        .representation = .opaque_without_backing,
        .args = try a.dupe(checked.CheckedTypeId, &.{bad}),
    } });
    const kind_only = try store.appendSyntheticPayloadRoot(a, .{ .record = .{
        .fields = try a.dupe(checked.CheckedRecordField, &.{.{ .name = label, .ty = good, .kind = .err }}),
        .ext = good,
    } });
    const field_bad = try store.appendSyntheticPayloadRoot(a, .{ .record = .{
        .fields = try a.dupe(checked.CheckedRecordField, &.{.{ .name = label, .ty = kind }}),
        .ext = good,
    } });
    const ext_bad = try store.appendSyntheticPayloadRoot(a, .{ .record = .{ .fields = &.{}, .ext = bad } });
    const arg_bad = try store.appendSyntheticPayloadRoot(a, .{ .function = .{ .kind = .pure, .args = try a.dupe(checked.CheckedTypeId, &.{bad}), .ret = good } });
    const ret_bad = try store.appendSyntheticPayloadRoot(a, .{ .function = .{ .kind = .pure, .ret = bad } });
    const tags = try a.alloc(checked.CheckedTag, 1);
    tags[0] = .{ .name = tag, .args = try a.dupe(checked.CheckedTypeId, &.{bad}) };
    const tag_bad = try store.appendSyntheticPayloadRoot(a, .{ .tag_union = .{ .tags = tags, .ext = empty_tags } });
    const tag_ext_bad = try store.appendSyntheticPayloadRoot(a, .{ .tag_union = .{ .tags = &.{}, .ext = bad } });
    var empty_modules: Common.EmptyCheckedModulesForTest = undefined;
    try empty_modules.init(a);
    defer empty_modules.deinit(a);
    var modules = Modules{ .allocator = a };
    defer modules.deinit();
    const module = try addErrorTestModule(&empty_modules, &modules, &store);
    const errors = try ErrorTypes.create(a, &modules);
    defer errors.destroy();
    for ([_]checked.CheckedTypeId{ good, empty_tags, pending, flex, rigid, alias_good, nominal_good, kind_only }) |id| {
        try std.testing.expect(!try hasErrorUnmemoized(a, &modules, module, id));
        try std.testing.expect(!try errors.hasError(module, id));
    }
    for ([_]checked.CheckedTypeId{ bad, kind, alias_bad, nominal_bad, field_bad, ext_bad, arg_bad, ret_bad, tag_bad, tag_ext_bad }) |id| {
        try std.testing.expect(try hasErrorUnmemoized(a, &modules, module, id));
        try std.testing.expect(try errors.hasError(module, id));
    }
}

fn errorTypesAllocationCase(allocator: Allocator, modules: *const Modules, module: ModuleIndex, roots: []const checked.CheckedTypeId) (Allocator.Error || error{TestUnexpectedResult})!void {
    const errors = try ErrorTypes.create(allocator, modules);
    defer errors.destroy();
    for (roots) |root| try std.testing.expect(try errors.hasError(module, root));
}

test "Boxy error reachability propagates OOM and retries without stale proofs" {
    const a = std.testing.allocator;
    var store = try errorTestStore(a);
    defer store.deinit(a);
    const bad = try store.appendSyntheticPayloadRoot(a, .err);
    const first = try store.reserveSyntheticTypeRoot(a);
    const second = try store.reserveSyntheticTypeRoot(a);
    try store.fillSyntheticTypeRoot(a, first, .{ .tuple = try a.dupe(checked.CheckedTypeId, &.{ second, bad }) });
    try store.fillSyntheticTypeRoot(a, second, .{ .tuple = try a.dupe(checked.CheckedTypeId, &.{first}) });
    var empty_modules: Common.EmptyCheckedModulesForTest = undefined;
    try empty_modules.init(a);
    defer empty_modules.deinit(a);
    var modules = Modules{ .allocator = a };
    defer modules.deinit();
    const module = try addErrorTestModule(&empty_modules, &modules, &store);
    try std.testing.checkAllAllocationFailures(a, errorTypesAllocationCase, .{ &modules, module, @as([]const checked.CheckedTypeId, &.{ first, second, bad }) });
    // Fail during edge expansion after retaining a completed leaf proof.
    var failing = std.testing.FailingAllocator.init(a, .{});
    const errors = try ErrorTypes.create(failing.allocator(), &modules);
    defer errors.destroy();
    try std.testing.expect(try errors.hasError(module, bad));
    failing.fail_index = failing.alloc_index;
    failing.resize_fail_index = failing.resize_index;
    try std.testing.expectError(error.OutOfMemory, errors.hasError(module, first));
    try std.testing.expectEqual(@as(usize, 1), errors.stats().unique_expanded_nodes);
    try std.testing.expect(try errors.hasError(module, bad));
    failing.fail_index = std.math.maxInt(usize);
    failing.resize_fail_index = std.math.maxInt(usize);
    try std.testing.expect(try errors.hasError(module, first));
    try std.testing.expect(try errors.hasError(module, second));
    try std.testing.expectEqual(@as(usize, 3), errors.stats().unique_expanded_nodes);
}

test "Boxy error reachability work stays bounded by shared graph nodes" {
    const a = std.testing.allocator;
    for ([_]usize{ 16, 128, 1024 }) |count| {
        var store = try errorTestStore(a);
        defer store.deinit(a);
        const ids = try a.alloc(checked.CheckedTypeId, count);
        defer a.free(ids);
        for (ids) |*id| id.* = try store.reserveSyntheticTypeRoot(a);
        for (ids[0 .. count - 1], ids[1..]) |id, child| {
            try store.fillSyntheticTypeRoot(a, id, .{ .tuple = try a.dupe(checked.CheckedTypeId, &.{ child, child }) });
        }
        try store.fillSyntheticTypeRoot(a, ids[count - 1], .empty_record);
        var empty_modules: Common.EmptyCheckedModulesForTest = undefined;
        try empty_modules.init(a);
        defer empty_modules.deinit(a);
        var modules = Modules{ .allocator = a };
        defer modules.deinit();
        const module = try addErrorTestModule(&empty_modules, &modules, &store);
        const errors = try ErrorTypes.create(a, &modules);
        defer errors.destroy();
        try std.testing.expect(!try errors.hasError(module, ids[0]));
        const capacity = errors.stats().retained_array_capacity_bytes;
        for (0..3) |_| for (ids) |id| try std.testing.expect(!try errors.hasError(module, id));
        try std.testing.expectEqual(count, errors.stats().unique_expanded_nodes);
        try std.testing.expectEqual(count * 3 + 1, errors.stats().queries);
        try std.testing.expectEqual(capacity, errors.stats().retained_array_capacity_bytes);
    }
}
