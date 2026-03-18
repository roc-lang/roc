//! Monomorphic type system for MIR.
//!
//! Monotypes are fully concrete types with no type variables, no extensions,
//! no aliases, and no nominal/opaque/structural distinction. Records, tag unions,
//! and tuples are just records, tag unions, and tuples — the distinctions that
//! existed for static dispatch and module boundary enforcement are no longer needed.
//!
//! Each MIR expression has exactly one Monotype via a 1:1 Expr.Idx → Monotype.Idx mapping.

const std = @import("std");
const base = @import("base");
const can = @import("can");
const types = @import("types");

const Ident = base.Ident;
const Allocator = std.mem.Allocator;
const ModuleEnv = can.ModuleEnv;
const CommonIdents = can.ModuleEnv.CommonIdents;
const StaticDispatchConstraint = types.StaticDispatchConstraint;

/// Check if a constraint range contains a numeric constraint (from_numeral,
/// desugared_binop, or desugared_unaryop). These imply the type variable is
/// numeric and should default to Dec rather than unit.
fn hasNumeralConstraint(types_store: *const types.Store, constraints: StaticDispatchConstraint.SafeList.Range) bool {
    if (constraints.isEmpty()) return false;
    for (types_store.sliceStaticDispatchConstraints(constraints)) |constraint| {
        switch (constraint.origin) {
            .from_numeral, .desugared_binop, .desugared_unaryop => return true,
            .method_call, .where_clause => {},
        }
    }
    return false;
}

/// Index into the Store's monotypes array.
/// Since MIR has a 1:1 expr-to-type mapping, an Expr.Idx can be directly
/// reinterpreted as a Monotype.Idx.
pub const Idx = enum(u32) {
    _,

    pub const none: Idx = @enumFromInt(std.math.maxInt(u32));

    pub fn isNone(self: Idx) bool {
        return self == none;
    }
};

/// Span of Monotype.Idx values stored in the extra_data array.
pub const Span = extern struct {
    start: u32,
    len: u16,

    pub fn empty() Span {
        return .{ .start = 0, .len = 0 };
    }

    pub fn isEmpty(self: Span) bool {
        return self.len == 0;
    }
};

/// A monomorphic type — fully concrete, no type variables, no extensions.
pub const Monotype = union(enum) {
    /// Function type: args -> ret
    func: struct {
        args: Span,
        ret: Idx,
        effectful: bool,
    },

    /// Closed tag union (tags sorted by name)
    tag_union: struct {
        tags: TagSpan,
    },

    /// Closed record (fields sorted by name)
    record: struct {
        fields: FieldSpan,
    },

    /// Tuple
    tuple: struct {
        elems: Span,
    },

    /// List with element type
    list: struct {
        elem: Idx,
    },

    /// Primitive type
    prim: Prim,

    /// Box (heap-allocated wrapper)
    box: struct {
        inner: Idx,
    },

    /// Unit / empty record
    unit: void,

    /// Temporary placeholder used during recursive type construction.
    /// Must be overwritten before construction completes; surviving
    /// placeholders indicate a bug in fromFlatType/fromFuncType/fromNominalType.
    recursive_placeholder: void,
};

/// Primitive type kinds.
pub const Prim = enum {
    str,
    u8,
    i8,
    u16,
    i16,
    u32,
    i32,
    u64,
    i64,
    u128,
    i128,
    f32,
    f64,
    dec,
};

/// A tag in a tag union.
pub const Tag = struct {
    name: Ident.Idx,
    /// Span of Monotype.Idx for payload types
    payloads: Span,

    pub fn sortByNameAsc(ident_store: *const Ident.Store, a: Tag, b: Tag) bool {
        return orderByName(ident_store, a, b) == .lt;
    }

    fn orderByName(ident_store: *const Ident.Store, a: Tag, b: Tag) std.math.Order {
        const a_text = ident_store.getText(a.name);
        const b_text = ident_store.getText(b.name);
        return std.mem.order(u8, a_text, b_text);
    }
};

/// Span of Tags stored in the tags array.
pub const TagSpan = extern struct {
    start: u32,
    len: u16,

    pub fn empty() TagSpan {
        return .{ .start = 0, .len = 0 };
    }

    pub fn isEmpty(self: TagSpan) bool {
        return self.len == 0;
    }
};

/// A field in a record.
pub const Field = struct {
    name: Ident.Idx,
    type_idx: Idx,

    pub fn sortByNameAsc(ident_store: *const Ident.Store, a: Field, b: Field) bool {
        return orderByName(ident_store, a, b) == .lt;
    }

    fn orderByName(ident_store: *const Ident.Store, a: Field, b: Field) std.math.Order {
        const a_text = ident_store.getText(a.name);
        const b_text = ident_store.getText(b.name);
        return std.mem.order(u8, a_text, b_text);
    }
};

/// Span of Fields stored in the fields array.
pub const FieldSpan = extern struct {
    start: u32,
    len: u16,

    pub fn empty() FieldSpan {
        return .{ .start = 0, .len = 0 };
    }

    pub fn isEmpty(self: FieldSpan) bool {
        return self.len == 0;
    }
};

const NamedSpecialization = struct {
    name_text: []const u8,
    type_idx: Idx,
};

/// Flat storage for monomorphic types.
pub const Store = struct {
    monotypes: std.ArrayListUnmanaged(Monotype),
    /// Monotype.Idx values for spans (func args, tuple elems)
    extra_idx: std.ArrayListUnmanaged(u32),
    tags: std.ArrayListUnmanaged(Tag),
    fields: std.ArrayListUnmanaged(Field),

    /// Pre-interned index for the unit monotype.
    unit_idx: Idx,
    /// Pre-interned indices for each primitive monotype, indexed by `@intFromEnum(Prim)`.
    prim_idxs: [prim_count]Idx,
    /// Cached ordinary tag-union monotype for nominal Bool.
    bool_tag_union_idx: Idx,

    const prim_count = @typeInfo(Prim).@"enum".fields.len;

    pub const Scratches = struct {
        fields: base.Scratch(Field),
        tags: base.Scratch(Tag),
        idxs: base.Scratch(Idx),
        named_specializations: base.Scratch(NamedSpecialization),
        /// Ident store for sorting tag names alphabetically.
        /// Updated when switching modules during cross-module lowering.
        ident_store: ?*const Ident.Store = null,
        /// Module env owning the current `types_store` / `ident_store`.
        module_env: ?*const ModuleEnv = null,
        /// Shared module env slice used to resolve nominal definitions by origin module.
        all_module_envs: ?[]const *ModuleEnv = null,

        pub fn init(allocator: Allocator) Allocator.Error!Scratches {
            return .{
                .fields = try base.Scratch(Field).init(allocator),
                .tags = try base.Scratch(Tag).init(allocator),
                .idxs = try base.Scratch(Idx).init(allocator),
                .named_specializations = try base.Scratch(NamedSpecialization).init(allocator),
            };
        }

        pub fn deinit(self: *Scratches) void {
            self.fields.deinit();
            self.tags.deinit();
            self.idxs.deinit();
            self.named_specializations.deinit();
        }
    };

    /// Look up the pre-interned index for a primitive type.
    pub fn primIdx(self: *const Store, prim: Prim) Idx {
        return self.prim_idxs[@intFromEnum(prim)];
    }

    pub fn addBoolTagUnion(self: *Store, allocator: Allocator, common_idents: CommonIdents) !Idx {
        if (!self.bool_tag_union_idx.isNone()) return self.bool_tag_union_idx;

        const false_payloads = Span.empty();
        const true_payloads = Span.empty();
        const tags = try self.addTags(allocator, &.{
            .{ .name = common_idents.false_tag, .payloads = false_payloads },
            .{ .name = common_idents.true_tag, .payloads = true_payloads },
        });
        const idx = try self.addMonotype(allocator, .{ .tag_union = .{ .tags = tags } });
        self.bool_tag_union_idx = idx;
        return idx;
    }

    /// Pre-populate the store with the 16 fixed monotypes (unit + 15 primitives).
    pub fn init(allocator: Allocator) Allocator.Error!Store {
        var monotypes: std.ArrayListUnmanaged(Monotype) = .empty;
        try monotypes.ensureTotalCapacity(allocator, 1 + prim_count);

        // Unit slot
        const unit_idx: Idx = @enumFromInt(monotypes.items.len);
        monotypes.appendAssumeCapacity(.unit);

        // Primitive slots in enum order
        var prim_idxs: [prim_count]Idx = undefined;
        for (0..prim_count) |i| {
            prim_idxs[i] = @enumFromInt(monotypes.items.len);
            monotypes.appendAssumeCapacity(.{ .prim = @enumFromInt(i) });
        }

        return .{
            .monotypes = monotypes,
            .extra_idx = .empty,
            .tags = .empty,
            .fields = .empty,
            .unit_idx = unit_idx,
            .prim_idxs = prim_idxs,
            .bool_tag_union_idx = .none,
        };
    }

    pub fn deinit(self: *Store, allocator: Allocator) void {
        self.monotypes.deinit(allocator);
        self.extra_idx.deinit(allocator);
        self.tags.deinit(allocator);
        self.fields.deinit(allocator);
    }

    pub fn addMonotype(self: *Store, allocator: Allocator, mono: Monotype) !Idx {
        const idx: u32 = @intCast(self.monotypes.items.len);
        try self.monotypes.append(allocator, mono);
        return @enumFromInt(idx);
    }

    pub fn getMonotype(self: *const Store, idx: Idx) Monotype {
        return self.monotypes.items[@intFromEnum(idx)];
    }

    /// Add a span of Monotype.Idx values to extra_idx and return a Span.
    pub fn addIdxSpan(self: *Store, allocator: Allocator, ids: []const Idx) !Span {
        if (ids.len == 0) return Span.empty();
        const start: u32 = @intCast(self.extra_idx.items.len);
        for (ids) |id| {
            try self.extra_idx.append(allocator, @intFromEnum(id));
        }
        return .{ .start = start, .len = @intCast(ids.len) };
    }

    /// Get a slice of Monotype.Idx from a Span.
    pub fn getIdxSpan(self: *const Store, span: Span) []const Idx {
        if (span.len == 0) return &.{};
        const raw = self.extra_idx.items[span.start..][0..span.len];
        return @ptrCast(raw);
    }

    /// Add tags to the tags array and return a TagSpan.
    pub fn addTags(self: *Store, allocator: Allocator, tag_slice: []const Tag) !TagSpan {
        if (tag_slice.len == 0) return TagSpan.empty();
        const start: u32 = @intCast(self.tags.items.len);
        try self.tags.appendSlice(allocator, tag_slice);
        return .{ .start = start, .len = @intCast(tag_slice.len) };
    }

    /// Get a slice of Tags from a TagSpan.
    pub fn getTags(self: *const Store, span: TagSpan) []const Tag {
        if (span.len == 0) return &.{};
        return self.tags.items[span.start..][0..span.len];
    }

    /// Add fields to the fields array and return a FieldSpan.
    pub fn addFields(self: *Store, allocator: Allocator, field_slice: []const Field) !FieldSpan {
        if (field_slice.len == 0) return FieldSpan.empty();
        const start: u32 = @intCast(self.fields.items.len);
        try self.fields.appendSlice(allocator, field_slice);
        return .{ .start = start, .len = @intCast(field_slice.len) };
    }

    /// Get a slice of Fields from a FieldSpan.
    pub fn getFields(self: *const Store, span: FieldSpan) []const Field {
        if (span.len == 0) return &.{};
        return self.fields.items[span.start..][0..span.len];
    }

    /// Convert a CIR type variable to a Monotype, recursively resolving all
    /// type structure.
    ///
    /// `specializations` is a read-only map of type vars already bound to
    /// concrete monotypes by `bindTypeVarMonotypes` (polymorphic specialization).
    ///
    /// Cycle detection for recursive nominal types (the only legitimate source
    /// of cycles after type checking) is handled internally by `fromNominalType`
    /// using `nominal_cycle_breakers`.
    pub fn fromTypeVar(
        self: *Store,
        allocator: Allocator,
        types_store: *const types.Store,
        type_var: types.Var,
        common_idents: CommonIdents,
        specializations: *const std.AutoHashMap(types.Var, Idx),
        nominal_cycle_breakers: *std.AutoHashMap(types.Var, Idx),
        scratches: *Scratches,
    ) Allocator.Error!Idx {
        const resolved = types_store.resolveVar(type_var);

        // Check specialization bindings first (from bindTypeVarMonotypes).
        if (specializations.get(resolved.var_)) |cached| return cached;

        // Check nominal cycle breakers (for recursive nominal types like Tree := [Leaf, Node(Tree)]).
        if (nominal_cycle_breakers.get(resolved.var_)) |cached| return cached;

        return switch (resolved.desc.content) {
            .flex => |flex| {
                if (flex.name) |name| {
                    if (lookupNamedSpecialization(scratches, name)) |specialized| return specialized;
                }
                if (hasNumeralConstraint(types_store, flex.constraints))
                    return self.primIdx(.dec);
                return self.unit_idx;
            },
            .rigid => |rigid| {
                if (lookupNamedSpecialization(scratches, rigid.name)) |specialized| return specialized;
                if (hasNumeralConstraint(types_store, rigid.constraints))
                    return self.primIdx(.dec);
                return self.unit_idx;
            },
            .alias => |alias| {
                // Aliases are transparent — follow the backing var
                const backing_var = types_store.getAliasBackingVar(alias);
                return try self.fromTypeVar(allocator, types_store, backing_var, common_idents, specializations, nominal_cycle_breakers, scratches);
            },
            .structure => |flat_type| {
                return try self.fromFlatType(allocator, types_store, resolved.var_, flat_type, common_idents, specializations, nominal_cycle_breakers, scratches);
            },
            // Error types can appear nested inside function types (as argument
            // or return types) when --allow-errors is used. The guard in
            // lowerExpr only catches top-level error types, so we need to handle
            // them here too. Return unit as a safe placeholder so lowering can
            // continue and the runtime-error path is hit at runtime.
            .err => return self.unit_idx,
        };
    }

    fn fromFlatType(
        self: *Store,
        allocator: Allocator,
        types_store: *const types.Store,
        var_: types.Var,
        flat_type: types.FlatType,
        common_idents: CommonIdents,
        specializations: *const std.AutoHashMap(types.Var, Idx),
        nominal_cycle_breakers: *std.AutoHashMap(types.Var, Idx),
        scratches: *Scratches,
    ) Allocator.Error!Idx {
        return switch (flat_type) {
            .nominal_type => |nominal| {
                return try self.fromNominalType(allocator, types_store, var_, nominal, common_idents, specializations, nominal_cycle_breakers, scratches);
            },
            .empty_record => self.unit_idx,
            .empty_tag_union => try self.addMonotype(allocator, .{ .tag_union = .{ .tags = TagSpan.empty() } }),
            .record => |record| {
                const scratch_top = scratches.fields.top();
                defer scratches.fields.clearFrom(scratch_top);

                // Follow the record extension chain to collect ALL fields.
                // Roc's type system represents records as linked rows:
                // { a: A | ext } where ext -> { b: B | ext2 } where ext2 -> {}
                var current_row = record;
                rows: while (true) {
                    const fields_slice = types_store.getRecordFieldsSlice(current_row.fields);
                    const names = fields_slice.items(.name);
                    const vars = fields_slice.items(.var_);

                    for (names, vars) |name, field_var| {
                        var seen_name = false;
                        for (scratches.fields.sliceFromStart(scratch_top)) |existing| {
                            if (existing.name.eql(name)) {
                                seen_name = true;
                                break;
                            }
                        }
                        if (seen_name) continue;

                        const field_type = try self.fromTypeVar(allocator, types_store, field_var, common_idents, specializations, nominal_cycle_breakers, scratches);
                        try scratches.fields.append(.{ .name = name, .type_idx = field_type });
                    }

                    var ext_var = current_row.ext;
                    while (true) {
                        const ext_resolved = types_store.resolveVar(ext_var);
                        switch (ext_resolved.desc.content) {
                            .alias => |alias| {
                                ext_var = types_store.getAliasBackingVar(alias);
                                continue;
                            },
                            .structure => |ext_flat| switch (ext_flat) {
                                .record => |next_row| {
                                    current_row = next_row;
                                    continue :rows;
                                },
                                .record_unbound => |fields_range| {
                                    // Final open-row segment: append known fields.
                                    const ext_fields = types_store.getRecordFieldsSlice(fields_range);
                                    const ext_names = ext_fields.items(.name);
                                    const ext_vars = ext_fields.items(.var_);
                                    for (ext_names, ext_vars) |name, field_var| {
                                        var seen_name = false;
                                        for (scratches.fields.sliceFromStart(scratch_top)) |existing| {
                                            if (existing.name.eql(name)) {
                                                seen_name = true;
                                                break;
                                            }
                                        }
                                        if (seen_name) continue;

                                        const field_type = try self.fromTypeVar(allocator, types_store, field_var, common_idents, specializations, nominal_cycle_breakers, scratches);
                                        try scratches.fields.append(.{ .name = name, .type_idx = field_type });
                                    }
                                    break :rows;
                                },
                                .empty_record => break :rows,
                                else => {
                                    if (std.debug.runtime_safety) {
                                        std.debug.panic(
                                            "Monotype.fromTypeVar(record): unexpected row extension flat type '{s}'",
                                            .{@tagName(ext_flat)},
                                        );
                                    }
                                    unreachable;
                                },
                            },
                            .flex => {
                                if (findNamedRowExtensionMonotype(scratches, ext_var, types_store)) |specialized| {
                                    try self.appendSpecializedRecordFields(specialized, scratch_top, scratches);
                                }
                                break :rows; // Open record — treat as closed with collected fields
                            },
                            .rigid => {
                                if (findNamedRowExtensionMonotype(scratches, ext_var, types_store)) |specialized| {
                                    try self.appendSpecializedRecordFields(specialized, scratch_top, scratches);
                                }
                                break :rows; // Rigid record — treat as closed with collected fields
                            },
                            .err => {
                                if (std.debug.runtime_safety) {
                                    std.debug.panic(
                                        "Monotype.fromTypeVar(record): error row extension tail",
                                        .{},
                                    );
                                }
                                unreachable;
                            },
                        }
                    }
                }

                const collected_fields = scratches.fields.sliceFromStart(scratch_top);
                // Each row segment is sorted, but concatenation may not be.
                if (scratches.ident_store) |ident_store| {
                    std.mem.sort(Field, collected_fields, ident_store, Field.sortByNameAsc);
                }
                const field_span = try self.addFields(allocator, collected_fields);
                return try self.addMonotype(allocator, .{ .record = .{ .fields = field_span } });
            },
            .record_unbound => |fields_range| {
                // Extensible record — treat like a closed record with the known fields
                const fields_slice = types_store.getRecordFieldsSlice(fields_range);
                const names = fields_slice.items(.name);
                const vars = fields_slice.items(.var_);

                const scratch_top = scratches.fields.top();
                defer scratches.fields.clearFrom(scratch_top);

                for (names, vars) |name, field_var| {
                    const field_type = try self.fromTypeVar(allocator, types_store, field_var, common_idents, specializations, nominal_cycle_breakers, scratches);
                    try scratches.fields.append(.{ .name = name, .type_idx = field_type });
                }

                const field_span = try self.addFields(allocator, scratches.fields.sliceFromStart(scratch_top));
                return try self.addMonotype(allocator, .{ .record = .{ .fields = field_span } });
            },
            .tuple => |tuple| {
                const elem_vars = types_store.sliceVars(tuple.elems);
                const scratch_top = scratches.idxs.top();
                defer scratches.idxs.clearFrom(scratch_top);

                for (elem_vars) |elem_var| {
                    const elem_type = try self.fromTypeVar(allocator, types_store, elem_var, common_idents, specializations, nominal_cycle_breakers, scratches);
                    try scratches.idxs.append(elem_type);
                }

                const elem_span = try self.addIdxSpan(allocator, scratches.idxs.sliceFromStart(scratch_top));
                return try self.addMonotype(allocator, .{ .tuple = .{ .elems = elem_span } });
            },
            .tag_union => |tag_union_row| {
                const tags_top = scratches.tags.top();
                defer scratches.tags.clearFrom(tags_top);

                // Follow the tag union extension chain to collect ALL tags.
                // Roc's type system represents tag unions as linked rows:
                // [Ok a | ext] where ext -> [Err b | ext2] where ext2 -> empty_tag_union
                var current_row = tag_union_row;
                rows: while (true) {
                    const tags_slice = types_store.getTagsSlice(current_row.tags);
                    const tag_names = tags_slice.items(.name);
                    const tag_args = tags_slice.items(.args);

                    for (tag_names, tag_args) |name, args_range| {
                        const arg_vars = types_store.sliceVars(args_range);
                        const idxs_top = scratches.idxs.top();
                        defer scratches.idxs.clearFrom(idxs_top);

                        for (arg_vars) |arg_var| {
                            const payload_type = try self.fromTypeVar(allocator, types_store, arg_var, common_idents, specializations, nominal_cycle_breakers, scratches);
                            try scratches.idxs.append(payload_type);
                        }

                        const payloads_span = try self.addIdxSpan(allocator, scratches.idxs.sliceFromStart(idxs_top));
                        try scratches.tags.append(.{ .name = name, .payloads = payloads_span });
                    }

                    // Follow extension variable to find more tags
                    var ext_var = current_row.ext;
                    while (true) {
                        const ext_resolved = types_store.resolveVar(ext_var);
                        switch (ext_resolved.desc.content) {
                            .alias => |alias| {
                                ext_var = types_store.getAliasBackingVar(alias);
                                continue;
                            },
                            .structure => |ext_flat| switch (ext_flat) {
                                .tag_union => |next_row| {
                                    current_row = next_row;
                                    continue :rows;
                                },
                                .empty_tag_union => break :rows,
                                else => {
                                    if (std.debug.runtime_safety) {
                                        std.debug.panic(
                                            "Monotype.fromTypeVar(tag_union): unexpected row extension flat type '{s}'",
                                            .{@tagName(ext_flat)},
                                        );
                                    }
                                    unreachable;
                                },
                            },
                            .flex => {
                                if (findNamedRowExtensionMonotype(scratches, ext_var, types_store)) |specialized| {
                                    try self.appendSpecializedTagUnionTags(specialized, scratches);
                                }
                                break :rows; // Open tag union — treat as closed with collected tags
                            },
                            .rigid => {
                                if (findNamedRowExtensionMonotype(scratches, ext_var, types_store)) |specialized| {
                                    try self.appendSpecializedTagUnionTags(specialized, scratches);
                                }
                                break :rows; // Rigid tag union — treat as closed with collected tags
                            },
                            .err => {
                                if (std.debug.runtime_safety) {
                                    std.debug.panic(
                                        "Monotype.fromTypeVar(tag_union): error row extension tail",
                                        .{},
                                    );
                                }
                                unreachable;
                            },
                        }
                    }
                }

                const collected_tags = scratches.tags.sliceFromStart(tags_top);
                // Sort tags alphabetically to match discriminant assignment order.
                // Each ext-chain row is pre-sorted, but the concatenation may not be.
                if (scratches.ident_store) |ident_store| {
                    std.mem.sort(Tag, collected_tags, ident_store, Tag.sortByNameAsc);
                }
                const tag_span = try self.addTags(allocator, collected_tags);
                return try self.addMonotype(allocator, .{ .tag_union = .{ .tags = tag_span } });
            },
            .fn_pure => |func| try self.fromFuncType(allocator, types_store, func, false, common_idents, specializations, nominal_cycle_breakers, scratches),
            .fn_effectful => |func| try self.fromFuncType(allocator, types_store, func, true, common_idents, specializations, nominal_cycle_breakers, scratches),
            .fn_unbound => |func| try self.fromFuncType(allocator, types_store, func, false, common_idents, specializations, nominal_cycle_breakers, scratches),
        };
    }

    fn fromFuncType(
        self: *Store,
        allocator: Allocator,
        types_store: *const types.Store,
        func: types.Func,
        effectful: bool,
        common_idents: CommonIdents,
        specializations: *const std.AutoHashMap(types.Var, Idx),
        nominal_cycle_breakers: *std.AutoHashMap(types.Var, Idx),
        scratches: *Scratches,
    ) Allocator.Error!Idx {
        const arg_vars = types_store.sliceVars(func.args);
        const scratch_top = scratches.idxs.top();
        defer scratches.idxs.clearFrom(scratch_top);

        for (arg_vars) |arg_var| {
            const arg_type = try self.fromTypeVar(allocator, types_store, arg_var, common_idents, specializations, nominal_cycle_breakers, scratches);
            try scratches.idxs.append(arg_type);
        }

        const args_span = try self.addIdxSpan(allocator, scratches.idxs.sliceFromStart(scratch_top));
        const ret = try self.fromTypeVar(allocator, types_store, func.ret, common_idents, specializations, nominal_cycle_breakers, scratches);

        return try self.addMonotype(allocator, .{ .func = .{
            .args = args_span,
            .ret = ret,
            .effectful = effectful,
        } });
    }

    fn fromNominalType(
        self: *Store,
        allocator: Allocator,
        types_store: *const types.Store,
        nominal_var: types.Var,
        nominal: types.NominalType,
        common_idents: CommonIdents,
        specializations: *const std.AutoHashMap(types.Var, Idx),
        nominal_cycle_breakers: *std.AutoHashMap(types.Var, Idx),
        scratches: *Scratches,
    ) Allocator.Error!Idx {
        const ident = nominal.ident.ident_idx;
        const origin = nominal.origin_module;

        if (origin.eql(common_idents.builtin_module)) {
            // Bool/Str: unqualified idents from source declarations
            if (ident.eql(common_idents.str)) return self.primIdx(.str);
            if (ident.eql(common_idents.bool)) return try self.addBoolTagUnion(allocator, common_idents);
        }

        if (origin.eql(common_idents.builtin_module)) {

            // List: unqualified ident from mkListContent
            if (ident.eql(common_idents.list)) {
                const type_args = types_store.sliceNominalArgs(nominal);
                if (type_args.len > 0) {
                    const elem_type = try self.fromTypeVar(allocator, types_store, type_args[0], common_idents, specializations, nominal_cycle_breakers, scratches);
                    return try self.addMonotype(allocator, .{ .list = .{ .elem = elem_type } });
                }
                return self.unit_idx;
            }

            // Box: unqualified ident from mkBoxContent
            if (ident.eql(common_idents.box)) {
                const type_args = types_store.sliceNominalArgs(nominal);
                if (type_args.len > 0) {
                    const inner_type = try self.fromTypeVar(allocator, types_store, type_args[0], common_idents, specializations, nominal_cycle_breakers, scratches);
                    return try self.addMonotype(allocator, .{ .box = .{ .inner = inner_type } });
                }
                return self.unit_idx;
            }

            // Numeric types: qualified idents from mkNumberTypeContent (e.g. "Builtin.Num.I64")
            if (ident.eql(common_idents.i64_type)) return self.primIdx(.i64);
            if (ident.eql(common_idents.u8_type)) return self.primIdx(.u8);
            if (ident.eql(common_idents.i8_type)) return self.primIdx(.i8);
            if (ident.eql(common_idents.u16_type)) return self.primIdx(.u16);
            if (ident.eql(common_idents.i16_type)) return self.primIdx(.i16);
            if (ident.eql(common_idents.u32_type)) return self.primIdx(.u32);
            if (ident.eql(common_idents.i32_type)) return self.primIdx(.i32);
            if (ident.eql(common_idents.u64_type)) return self.primIdx(.u64);
            if (ident.eql(common_idents.u128_type)) return self.primIdx(.u128);
            if (ident.eql(common_idents.i128_type)) return self.primIdx(.i128);
            if (ident.eql(common_idents.f32_type)) return self.primIdx(.f32);
            if (ident.eql(common_idents.f64_type)) return self.primIdx(.f64);
            if (ident.eql(common_idents.dec_type)) return self.primIdx(.dec);
        }

        // For all other nominal types, strip the wrapper and follow the backing var.
        // In MIR there is no nominal/opaque/structural distinction.
        //
        // Recursive nominal types (e.g. Tree := [Leaf, Node(Tree)]) are the
        // only legitimate source of type cycles — the type checker has already
        // converted infinite/anonymous recursive types to errors. We use
        // `nominal_cycle_breakers` (separate from the specialization map) to
        // break these cycles: register a placeholder before recursing, then
        // overwrite it in-place with the real value.
        if (nominal_cycle_breakers.get(nominal_var)) |cached| return cached;
        if (findEquivalentNominalCycleBreaker(types_store, nominal, nominal_cycle_breakers)) |cached| {
            return cached;
        }

        const placeholder_idx = try self.addMonotype(allocator, .recursive_placeholder);
        try nominal_cycle_breakers.put(nominal_var, placeholder_idx);

        const named_specializations_top = try self.pushNominalArgSpecializations(
            allocator,
            types_store,
            nominal,
            common_idents,
            specializations,
            nominal_cycle_breakers,
            scratches,
        );
        defer scratches.named_specializations.clearFrom(named_specializations_top);

        const backing_var = types_store.getNominalBackingVar(nominal);
        const backing_idx = try self.fromTypeVar(allocator, types_store, backing_var, common_idents, specializations, nominal_cycle_breakers, scratches);

        // Copy the resolved backing type's value into our placeholder slot.
        // This value-copy is safe because every field inside a Monotype is an
        // index (Idx, Span, Ident.Idx) — never a pointer. The monotype store
        // is append-only, so all indices remain valid after the copy.
        self.monotypes.items[@intFromEnum(placeholder_idx)] = self.monotypes.items[@intFromEnum(backing_idx)];
        if (std.debug.runtime_safety) {
            std.debug.assert(self.monotypes.items[@intFromEnum(placeholder_idx)] != .recursive_placeholder);
        }
        return placeholder_idx;
    }

    fn lookupNamedSpecialization(scratches: *const Scratches, name: Ident.Idx) ?Idx {
        const ident_store = scratches.ident_store orelse return null;
        const name_text = ident_store.getText(name);
        const items = scratches.named_specializations.items.items;

        var i = items.len;
        while (i > 0) {
            i -= 1;
            if (std.mem.eql(u8, items[i].name_text, name_text)) {
                return items[i].type_idx;
            }
        }
        return null;
    }

    fn findNamedRowExtensionMonotype(scratches: *const Scratches, ext_var: types.Var, types_store: *const types.Store) ?Idx {
        const resolved = types_store.resolveVar(ext_var);
        return switch (resolved.desc.content) {
            .flex => |flex| if (flex.name) |name| lookupNamedSpecialization(scratches, name) else null,
            .rigid => |rigid| lookupNamedSpecialization(scratches, rigid.name),
            else => null,
        };
    }

    fn appendSpecializedRecordFields(
        self: *Store,
        specialized: Idx,
        scratch_top: u32,
        scratches: *Scratches,
    ) Allocator.Error!void {
        const mono = self.getMonotype(specialized);
        switch (mono) {
            .record => |record| {
                for (self.getFields(record.fields)) |field| {
                    var seen_name = false;
                    for (scratches.fields.sliceFromStart(scratch_top)) |existing| {
                        if (existing.name.eql(field.name)) {
                            seen_name = true;
                            break;
                        }
                    }
                    if (seen_name) continue;
                    try scratches.fields.append(field);
                }
            },
            .unit => {},
            else => {
                if (std.debug.runtime_safety) {
                    std.debug.panic(
                        "Monotype.fromTypeVar(record): nominal row specialization must be record or unit, found '{s}'",
                        .{@tagName(mono)},
                    );
                }
                unreachable;
            },
        }
    }

    fn appendSpecializedTagUnionTags(
        self: *Store,
        specialized: Idx,
        scratches: *Scratches,
    ) Allocator.Error!void {
        const mono = self.getMonotype(specialized);
        switch (mono) {
            .tag_union => |tag_union| {
                for (self.getTags(tag_union.tags)) |tag| {
                    try scratches.tags.append(tag);
                }
            },
            else => {
                if (std.debug.runtime_safety) {
                    std.debug.panic(
                        "Monotype.fromTypeVar(tag_union): nominal row specialization must be tag_union, found '{s}'",
                        .{@tagName(mono)},
                    );
                }
                unreachable;
            },
        }
    }

    fn pushNominalArgSpecializations(
        self: *Store,
        allocator: Allocator,
        types_store: *const types.Store,
        nominal: types.NominalType,
        common_idents: CommonIdents,
        specializations: *const std.AutoHashMap(types.Var, Idx),
        nominal_cycle_breakers: *std.AutoHashMap(types.Var, Idx),
        scratches: *Scratches,
    ) Allocator.Error!u32 {
        const top = scratches.named_specializations.top();

        const source_env = scratches.module_env orelse return top;
        const all_module_envs = scratches.all_module_envs orelse return top;
        const definition_env = findNominalDefinitionEnv(source_env, all_module_envs, nominal.origin_module) orelse return top;
        const type_name = source_env.getIdent(nominal.ident.ident_idx);
        const definition_nominal = findDefinitionNominal(definition_env, type_name) orelse return top;

        const formal_args = definition_env.types.sliceNominalArgs(definition_nominal);
        const actual_args = types_store.sliceNominalArgs(nominal);
        if (formal_args.len != actual_args.len) {
            if (std.debug.runtime_safety) {
                std.debug.panic(
                    "Monotype.fromNominalType: arg arity mismatch for nominal '{s}' (formal={d}, actual={d})",
                    .{ type_name, formal_args.len, actual_args.len },
                );
            }
            unreachable;
        }

        for (formal_args, actual_args) |formal_arg, actual_arg| {
            const formal_name_text = resolvedTypeVarNameText(&definition_env.types, definition_env, formal_arg) orelse continue;
            const actual_mono = try self.fromTypeVar(
                allocator,
                types_store,
                actual_arg,
                common_idents,
                specializations,
                nominal_cycle_breakers,
                scratches,
            );
            try scratches.named_specializations.append(.{
                .name_text = formal_name_text,
                .type_idx = actual_mono,
            });
        }

        return top;
    }

    fn resolvedTypeVarNameText(
        types_store: *const types.Store,
        module_env: *const ModuleEnv,
        var_: types.Var,
    ) ?[]const u8 {
        const resolved = types_store.resolveVar(var_);
        return switch (resolved.desc.content) {
            .rigid => |rigid| module_env.getIdent(rigid.name),
            .flex => |flex| if (flex.name) |name| module_env.getIdent(name) else null,
            else => null,
        };
    }

    fn findNominalDefinitionEnv(
        source_env: *const ModuleEnv,
        all_module_envs: []const *ModuleEnv,
        origin_module: Ident.Idx,
    ) ?*const ModuleEnv {
        const origin_name = source_env.getIdent(origin_module);
        for (all_module_envs) |candidate_env| {
            const candidate_name = candidate_env.getIdent(candidate_env.qualified_module_ident);
            if (std.mem.eql(u8, origin_name, candidate_name)) return candidate_env;
        }
        return null;
    }

    fn findDefinitionNominal(definition_env: *const ModuleEnv, type_name: []const u8) ?types.NominalType {
        for (definition_env.store.sliceStatements(definition_env.all_statements)) |stmt_idx| {
            const stmt = definition_env.store.getStatement(stmt_idx);
            switch (stmt) {
                .s_nominal_decl => |nominal_decl| {
                    const header = definition_env.store.getTypeHeader(nominal_decl.header);
                    if (!std.mem.eql(u8, definition_env.getIdent(header.relative_name), type_name)) continue;

                    const resolved = definition_env.types.resolveVar(ModuleEnv.varFrom(stmt_idx));
                    if (resolved.desc.content == .structure and resolved.desc.content.structure == .nominal_type) {
                        return resolved.desc.content.structure.nominal_type;
                    }
                },
                else => {},
            }
        }
        return null;
    }
};

fn findEquivalentNominalCycleBreaker(
    types_store: *const types.Store,
    nominal: types.NominalType,
    nominal_cycle_breakers: *const std.AutoHashMap(types.Var, Idx),
) ?Idx {
    var iter = nominal_cycle_breakers.iterator();
    while (iter.next()) |entry| {
        const resolved = types_store.resolveVar(entry.key_ptr.*);
        if (resolved.desc.content != .structure) continue;
        const flat = resolved.desc.content.structure;
        if (flat != .nominal_type) continue;
        const other_nominal = flat.nominal_type;

        if (!nominal.origin_module.eql(other_nominal.origin_module)) continue;
        if (!nominal.ident.ident_idx.eql(other_nominal.ident.ident_idx)) continue;

        const lhs_args = types_store.sliceNominalArgs(nominal);
        const rhs_args = types_store.sliceNominalArgs(other_nominal);
        if (lhs_args.len != rhs_args.len) continue;

        var args_match = true;
        for (lhs_args, rhs_args) |lhs_arg, rhs_arg| {
            const lhs_resolved = types_store.resolveVar(lhs_arg);
            const rhs_resolved = types_store.resolveVar(rhs_arg);
            if (lhs_resolved.var_ != rhs_resolved.var_) {
                args_match = false;
                break;
            }
        }

        if (args_match) return entry.value_ptr.*;
    }

    return null;
}
