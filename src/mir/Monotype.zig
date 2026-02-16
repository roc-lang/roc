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
const types = @import("types");
const can = @import("can");

const Ident = base.Ident;
const Allocator = std.mem.Allocator;
const CIR = can.CIR;

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
};

/// Primitive type kinds.
pub const Prim = enum {
    bool,
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

    const prim_count = @typeInfo(Prim).@"enum".fields.len;

    pub const Scratches = struct {
        fields: base.Scratch(Field),
        tags: base.Scratch(Tag),
        idxs: base.Scratch(Idx),

        pub fn init(allocator: Allocator) Allocator.Error!Scratches {
            return .{
                .fields = try base.Scratch(Field).init(allocator),
                .tags = try base.Scratch(Tag).init(allocator),
                .idxs = try base.Scratch(Idx).init(allocator),
            };
        }

        pub fn deinit(self: *Scratches) void {
            self.fields.deinit();
            self.tags.deinit();
            self.idxs.deinit();
        }
    };

    /// Look up the pre-interned index for a primitive type.
    pub fn primIdx(self: *const Store, prim: Prim) Idx {
        return self.prim_idxs[@intFromEnum(prim)];
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
    /// type structure. Uses `seen` for cycle detection on recursive types.
    pub fn fromTypeVar(
        self: *Store,
        allocator: Allocator,
        types_store: *const types.Store,
        type_var: types.Var,
        builtin_indices: CIR.BuiltinIndices,
        seen: *std.AutoHashMap(types.Var, Idx),
        scratches: *Scratches,
    ) Allocator.Error!Idx {
        const resolved = types_store.resolveVar(type_var);

        // Cycle detection: if we've already seen this var, return the cached idx
        if (seen.get(resolved.var_)) |cached| return cached;

        return switch (resolved.desc.content) {
            .flex, .rigid => {
                // Unresolved type variables must not appear in monomorphic code.
                // Reaching here means the type checker left something unresolved.
                unreachable;
            },
            .alias => |alias| {
                // Aliases are transparent — follow the backing var
                const backing_var = types_store.getAliasBackingVar(alias);
                return try self.fromTypeVar(allocator, types_store, backing_var, builtin_indices, seen, scratches);
            },
            .structure => |flat_type| {
                return try self.fromFlatType(allocator, types_store, resolved.var_, flat_type, builtin_indices, seen, scratches);
            },
            // Error types are caught in lowerExpr before resolveMonotype;
            // reaching here means a compiler bug in an earlier phase.
            .err => unreachable,
        };
    }

    fn fromFlatType(
        self: *Store,
        allocator: Allocator,
        types_store: *const types.Store,
        var_: types.Var,
        flat_type: types.FlatType,
        builtin_indices: CIR.BuiltinIndices,
        seen: *std.AutoHashMap(types.Var, Idx),
        scratches: *Scratches,
    ) Allocator.Error!Idx {
        return switch (flat_type) {
            .nominal_type => |nominal| {
                return try self.fromNominalType(allocator, types_store, var_, nominal, builtin_indices, seen, scratches);
            },
            .empty_record => self.unit_idx,
            .empty_tag_union => try self.addMonotype(allocator, .{ .tag_union = .{ .tags = TagSpan.empty() } }),
            .record => |record| {
                // Reserve a slot for cycle detection before recursing into fields
                const placeholder_idx = try self.addMonotype(allocator, .unit);
                try seen.put(var_, placeholder_idx);

                const fields_slice = types_store.getRecordFieldsSlice(record.fields);
                const names = fields_slice.items(.name);
                const vars = fields_slice.items(.var_);

                const scratch_top = scratches.fields.top();
                defer scratches.fields.clearFrom(scratch_top);

                for (names, vars) |name, field_var| {
                    const field_type = try self.fromTypeVar(allocator, types_store, field_var, builtin_indices, seen, scratches);
                    try scratches.fields.append(.{ .name = name, .type_idx = field_type });
                }

                const field_span = try self.addFields(allocator, scratches.fields.sliceFromStart(scratch_top));
                self.monotypes.items[@intFromEnum(placeholder_idx)] = .{ .record = .{ .fields = field_span } };
                return placeholder_idx;
            },
            .record_unbound => |fields_range| {
                // Extensible record — treat like a closed record with the known fields
                const fields_slice = types_store.getRecordFieldsSlice(fields_range);
                const names = fields_slice.items(.name);
                const vars = fields_slice.items(.var_);

                const placeholder_idx = try self.addMonotype(allocator, .unit);
                try seen.put(var_, placeholder_idx);

                const scratch_top = scratches.fields.top();
                defer scratches.fields.clearFrom(scratch_top);

                for (names, vars) |name, field_var| {
                    const field_type = try self.fromTypeVar(allocator, types_store, field_var, builtin_indices, seen, scratches);
                    try scratches.fields.append(.{ .name = name, .type_idx = field_type });
                }

                const field_span = try self.addFields(allocator, scratches.fields.sliceFromStart(scratch_top));
                self.monotypes.items[@intFromEnum(placeholder_idx)] = .{ .record = .{ .fields = field_span } };
                return placeholder_idx;
            },
            .tuple => |tuple| {
                const placeholder_idx = try self.addMonotype(allocator, .unit);
                try seen.put(var_, placeholder_idx);

                const elem_vars = types_store.sliceVars(tuple.elems);
                const scratch_top = scratches.idxs.top();
                defer scratches.idxs.clearFrom(scratch_top);

                for (elem_vars) |elem_var| {
                    const elem_type = try self.fromTypeVar(allocator, types_store, elem_var, builtin_indices, seen, scratches);
                    try scratches.idxs.append(elem_type);
                }

                const elem_span = try self.addIdxSpan(allocator, scratches.idxs.sliceFromStart(scratch_top));
                self.monotypes.items[@intFromEnum(placeholder_idx)] = .{ .tuple = .{ .elems = elem_span } };
                return placeholder_idx;
            },
            .tag_union => |tag_union| {
                const placeholder_idx = try self.addMonotype(allocator, .unit);
                try seen.put(var_, placeholder_idx);

                const tags_slice = types_store.getTagsSlice(tag_union.tags);
                const tag_names = tags_slice.items(.name);
                const tag_args = tags_slice.items(.args);

                const tags_top = scratches.tags.top();
                defer scratches.tags.clearFrom(tags_top);

                for (tag_names, tag_args) |name, args_range| {
                    const arg_vars = types_store.sliceVars(args_range);
                    const idxs_top = scratches.idxs.top();
                    defer scratches.idxs.clearFrom(idxs_top);

                    for (arg_vars) |arg_var| {
                        const payload_type = try self.fromTypeVar(allocator, types_store, arg_var, builtin_indices, seen, scratches);
                        try scratches.idxs.append(payload_type);
                    }

                    const payloads_span = try self.addIdxSpan(allocator, scratches.idxs.sliceFromStart(idxs_top));
                    try scratches.tags.append(.{ .name = name, .payloads = payloads_span });
                }

                const tag_span = try self.addTags(allocator, scratches.tags.sliceFromStart(tags_top));
                self.monotypes.items[@intFromEnum(placeholder_idx)] = .{ .tag_union = .{ .tags = tag_span } };
                return placeholder_idx;
            },
            .fn_pure => |func| try self.fromFuncType(allocator, types_store, var_, func, false, builtin_indices, seen, scratches),
            .fn_effectful => |func| try self.fromFuncType(allocator, types_store, var_, func, true, builtin_indices, seen, scratches),
            .fn_unbound => |func| try self.fromFuncType(allocator, types_store, var_, func, false, builtin_indices, seen, scratches),
        };
    }

    fn fromFuncType(
        self: *Store,
        allocator: Allocator,
        types_store: *const types.Store,
        var_: types.Var,
        func: types.Func,
        effectful: bool,
        builtin_indices: CIR.BuiltinIndices,
        seen: *std.AutoHashMap(types.Var, Idx),
        scratches: *Scratches,
    ) Allocator.Error!Idx {
        const placeholder_idx = try self.addMonotype(allocator, .unit);
        try seen.put(var_, placeholder_idx);

        const arg_vars = types_store.sliceVars(func.args);
        const scratch_top = scratches.idxs.top();
        defer scratches.idxs.clearFrom(scratch_top);

        for (arg_vars) |arg_var| {
            const arg_type = try self.fromTypeVar(allocator, types_store, arg_var, builtin_indices, seen, scratches);
            try scratches.idxs.append(arg_type);
        }

        const args_span = try self.addIdxSpan(allocator, scratches.idxs.sliceFromStart(scratch_top));
        const ret = try self.fromTypeVar(allocator, types_store, func.ret, builtin_indices, seen, scratches);

        self.monotypes.items[@intFromEnum(placeholder_idx)] = .{ .func = .{
            .args = args_span,
            .ret = ret,
            .effectful = effectful,
        } };
        return placeholder_idx;
    }

    fn fromNominalType(
        self: *Store,
        allocator: Allocator,
        types_store: *const types.Store,
        nominal_var: types.Var,
        nominal: types.NominalType,
        builtin_indices: CIR.BuiltinIndices,
        seen: *std.AutoHashMap(types.Var, Idx),
        scratches: *Scratches,
    ) Allocator.Error!Idx {
        const ident = nominal.ident.ident_idx;

        // Check if this is a builtin primitive type
        if (ident == builtin_indices.bool_ident) return self.primIdx(.bool);
        if (ident == builtin_indices.str_ident) return self.primIdx(.str);
        if (ident == builtin_indices.u8_ident) return self.primIdx(.u8);
        if (ident == builtin_indices.i8_ident) return self.primIdx(.i8);
        if (ident == builtin_indices.u16_ident) return self.primIdx(.u16);
        if (ident == builtin_indices.i16_ident) return self.primIdx(.i16);
        if (ident == builtin_indices.u32_ident) return self.primIdx(.u32);
        if (ident == builtin_indices.i32_ident) return self.primIdx(.i32);
        if (ident == builtin_indices.u64_ident) return self.primIdx(.u64);
        if (ident == builtin_indices.i64_ident) return self.primIdx(.i64);
        if (ident == builtin_indices.u128_ident) return self.primIdx(.u128);
        if (ident == builtin_indices.i128_ident) return self.primIdx(.i128);
        if (ident == builtin_indices.dec_ident) return self.primIdx(.dec);
        if (ident == builtin_indices.f32_ident) return self.primIdx(.f32);
        if (ident == builtin_indices.f64_ident) return self.primIdx(.f64);

        // Check if this is a builtin List type
        if (ident == builtin_indices.list_ident) {
            const type_args = types_store.sliceNominalArgs(nominal);
            if (type_args.len > 0) {
                const elem_type = try self.fromTypeVar(allocator, types_store, type_args[0], builtin_indices, seen, scratches);
                return try self.addMonotype(allocator, .{ .list = .{ .elem = elem_type } });
            }
            // List with no type arg — shouldn't happen in well-typed code
            return self.unit_idx;
        }

        // Check if this is a builtin Box type
        if (ident == builtin_indices.box_ident) {
            const type_args = types_store.sliceNominalArgs(nominal);
            if (type_args.len > 0) {
                const inner_type = try self.fromTypeVar(allocator, types_store, type_args[0], builtin_indices, seen, scratches);
                return try self.addMonotype(allocator, .{ .box = .{ .inner = inner_type } });
            }
            return self.unit_idx;
        }

        // For all other nominal types, strip the wrapper and follow the backing var.
        // In MIR there is no nominal/opaque/structural distinction.
        //
        // We must register a placeholder in `seen` before recursing so that
        // mutually-recursive nominal types (e.g. A → B → {field: A}) don't
        // loop forever. Cycle detection returns `placeholder_idx` to callers,
        // so we must overwrite it in-place with the real value — we can't just
        // return `backing_idx` because earlier callers already captured
        // `placeholder_idx`.
        const placeholder_idx = try self.addMonotype(allocator, .unit);
        try seen.put(nominal_var, placeholder_idx);

        const backing_var = types_store.getNominalBackingVar(nominal);
        const backing_idx = try self.fromTypeVar(allocator, types_store, backing_var, builtin_indices, seen, scratches);

        // Copy the resolved backing type's value into our placeholder slot.
        // This value-copy is safe (unlike the other handlers which build fresh
        // values from indices) because every field inside a Monotype is an index
        // (Idx, Span, Ident.Idx) — never a pointer. The monotype store is
        // append-only, so all indices remain valid after the copy.
        self.monotypes.items[@intFromEnum(placeholder_idx)] = self.monotypes.items[@intFromEnum(backing_idx)];
        return placeholder_idx;
    }
};
