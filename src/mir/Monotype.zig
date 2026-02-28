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

    pub fn sortByNameAsc(_: void, a: Tag, b: Tag) bool {
        return @as(u32, @bitCast(a.name)) < @as(u32, @bitCast(b.name));
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

    pub fn sortByNameAsc(_: void, a: Field, b: Field) bool {
        return @as(u32, @bitCast(a.name)) < @as(u32, @bitCast(b.name));
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

/// Flat storage for monomorphic types.
pub const Store = struct {
    pub const NominalHint = struct {
        module_idx: u32,
        ident: Ident.Idx,
    };

    monotypes: std.ArrayListUnmanaged(Monotype),
    /// Monotype.Idx values for spans (func args, tuple elems)
    extra_idx: std.ArrayListUnmanaged(u32),
    tags: std.ArrayListUnmanaged(Tag),
    fields: std.ArrayListUnmanaged(Field),
    nominal_hints: std.AutoHashMapUnmanaged(u32, NominalHint),
    /// Hash bucket -> canonical monotype roots in that bucket.
    canonical_by_hash: std.AutoHashMapUnmanaged(u64, std.ArrayListUnmanaged(Idx)),
    /// Memoized mapping from an existing monotype index to its canonical index.
    canonical_cache: std.AutoHashMapUnmanaged(u32, Idx),
    /// Placeholder indices that are still in progress and must not be interned yet.
    in_progress_placeholders: std.AutoHashMapUnmanaged(u32, void),

    /// Pre-interned index for the unit monotype.
    unit_idx: Idx,
    /// Pre-interned indices for each primitive monotype, indexed by `@intFromEnum(Prim)`.
    prim_idxs: [prim_count]Idx,

    const prim_count = @typeInfo(Prim).@"enum".fields.len;

    pub const Scratches = struct {
        fields: base.Scratch(Field),
        tags: base.Scratch(Tag),
        idxs: base.Scratch(Idx),
        /// Ident store for sorting tag names alphabetically.
        /// Updated when switching modules during cross-module lowering.
        ident_store: ?*const Ident.Store = null,
        /// Current module index for nominal hint recording.
        current_module_idx: u32 = 0,

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
            .nominal_hints = .{},
            .canonical_by_hash = .{},
            .canonical_cache = .{},
            .in_progress_placeholders = .{},
            .unit_idx = unit_idx,
            .prim_idxs = prim_idxs,
        };
    }

    pub fn deinit(self: *Store, allocator: Allocator) void {
        self.monotypes.deinit(allocator);
        self.extra_idx.deinit(allocator);
        self.tags.deinit(allocator);
        self.fields.deinit(allocator);
        self.nominal_hints.deinit(allocator);
        var bucket_it = self.canonical_by_hash.iterator();
        while (bucket_it.next()) |entry| {
            entry.value_ptr.deinit(allocator);
        }
        self.canonical_by_hash.deinit(allocator);
        self.canonical_cache.deinit(allocator);
        self.in_progress_placeholders.deinit(allocator);
    }

    pub fn addMonotype(self: *Store, allocator: Allocator, mono: Monotype) !Idx {
        if (mono == .unit) return self.unit_idx;
        if (mono == .prim) return self.primIdx(mono.prim);
        const raw_idx = try self.appendRawMonotype(allocator, mono);
        return try self.internExistingMonotype(allocator, raw_idx);
    }

    fn appendRawMonotype(self: *Store, allocator: Allocator, mono: Monotype) !Idx {
        const idx: u32 = @intCast(self.monotypes.items.len);
        try self.monotypes.append(allocator, mono);
        return @enumFromInt(idx);
    }

    fn addPlaceholder(self: *Store, allocator: Allocator) !Idx {
        const idx = try self.appendRawMonotype(allocator, .unit);
        try self.in_progress_placeholders.put(allocator, @intFromEnum(idx), {});
        return idx;
    }

    fn resolvePlaceholder(self: *Store, idx: Idx, mono: Monotype) void {
        self.monotypes.items[@intFromEnum(idx)] = mono;
        _ = self.in_progress_placeholders.remove(@intFromEnum(idx));
        _ = self.canonical_cache.remove(@intFromEnum(idx));
    }

    pub fn getMonotype(self: *const Store, idx: Idx) Monotype {
        return self.monotypes.items[@intFromEnum(idx)];
    }

    pub fn setNominalHint(self: *Store, allocator: Allocator, idx: Idx, hint: NominalHint) Allocator.Error!void {
        try self.nominal_hints.put(allocator, @intFromEnum(idx), hint);
    }

    pub fn getNominalHint(self: *const Store, idx: Idx) ?NominalHint {
        return self.nominal_hints.get(@intFromEnum(idx));
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

    const CanonicalToken = enum(u64) {
        rec_start = 1,
        rec_end = 2,
        rec_ref = 3,
        unit = 4,
        prim = 5,
        list = 6,
        box = 7,
        tuple = 8,
        func = 9,
        record = 10,
        tag_union = 11,
    };

    fn appendCanonicalTokensRec(
        self: *const Store,
        allocator: Allocator,
        idx: Idx,
        tokens: *std.ArrayList(u64),
        in_progress: *std.AutoHashMap(u32, u32),
        next_binder: *u32,
        has_unresolved_placeholder: *bool,
    ) Allocator.Error!void {
        const idx_u32 = @intFromEnum(idx);
        if (self.in_progress_placeholders.contains(idx_u32)) {
            has_unresolved_placeholder.* = true;
            return;
        }

        if (in_progress.get(idx_u32)) |binder| {
            try tokens.append(allocator, @intFromEnum(CanonicalToken.rec_ref));
            try tokens.append(allocator, binder);
            return;
        }

        const binder = next_binder.*;
        next_binder.* = binder + 1;
        try in_progress.put(idx_u32, binder);
        defer _ = in_progress.remove(idx_u32);

        try tokens.append(allocator, @intFromEnum(CanonicalToken.rec_start));
        try tokens.append(allocator, binder);

        switch (self.getMonotype(idx)) {
            .unit => try tokens.append(allocator, @intFromEnum(CanonicalToken.unit)),
            .prim => |prim| {
                try tokens.append(allocator, @intFromEnum(CanonicalToken.prim));
                try tokens.append(allocator, @intFromEnum(prim));
            },
            .list => |list| {
                try tokens.append(allocator, @intFromEnum(CanonicalToken.list));
                try self.appendCanonicalTokensRec(
                    allocator,
                    list.elem,
                    tokens,
                    in_progress,
                    next_binder,
                    has_unresolved_placeholder,
                );
            },
            .box => |boxed| {
                try tokens.append(allocator, @intFromEnum(CanonicalToken.box));
                try self.appendCanonicalTokensRec(
                    allocator,
                    boxed.inner,
                    tokens,
                    in_progress,
                    next_binder,
                    has_unresolved_placeholder,
                );
            },
            .tuple => |tuple| {
                try tokens.append(allocator, @intFromEnum(CanonicalToken.tuple));
                const elems = self.getIdxSpan(tuple.elems);
                try tokens.append(allocator, elems.len);
                for (elems) |elem| {
                    try self.appendCanonicalTokensRec(
                        allocator,
                        elem,
                        tokens,
                        in_progress,
                        next_binder,
                        has_unresolved_placeholder,
                    );
                }
            },
            .func => |func| {
                try tokens.append(allocator, @intFromEnum(CanonicalToken.func));
                try tokens.append(allocator, @intFromBool(func.effectful));
                const args = self.getIdxSpan(func.args);
                try tokens.append(allocator, args.len);
                for (args) |arg| {
                    try self.appendCanonicalTokensRec(
                        allocator,
                        arg,
                        tokens,
                        in_progress,
                        next_binder,
                        has_unresolved_placeholder,
                    );
                }
                try self.appendCanonicalTokensRec(
                    allocator,
                    func.ret,
                    tokens,
                    in_progress,
                    next_binder,
                    has_unresolved_placeholder,
                );
            },
            .record => |record| {
                try tokens.append(allocator, @intFromEnum(CanonicalToken.record));
                const fields = self.getFields(record.fields);
                try tokens.append(allocator, fields.len);
                for (fields) |field| {
                    try tokens.append(allocator, @as(u32, @bitCast(field.name)));
                    try self.appendCanonicalTokensRec(
                        allocator,
                        field.type_idx,
                        tokens,
                        in_progress,
                        next_binder,
                        has_unresolved_placeholder,
                    );
                }
            },
            .tag_union => |tu| {
                try tokens.append(allocator, @intFromEnum(CanonicalToken.tag_union));
                const tags = self.getTags(tu.tags);
                try tokens.append(allocator, tags.len);
                for (tags) |tag| {
                    try tokens.append(allocator, @as(u32, @bitCast(tag.name)));
                    const payloads = self.getIdxSpan(tag.payloads);
                    try tokens.append(allocator, payloads.len);
                    for (payloads) |payload| {
                        try self.appendCanonicalTokensRec(
                            allocator,
                            payload,
                            tokens,
                            in_progress,
                            next_binder,
                            has_unresolved_placeholder,
                        );
                    }
                }
            },
        }

        try tokens.append(allocator, @intFromEnum(CanonicalToken.rec_end));
    }

    fn buildCanonicalTokens(
        self: *const Store,
        allocator: Allocator,
        root: Idx,
        tokens: *std.ArrayList(u64),
    ) Allocator.Error!bool {
        var in_progress = std.AutoHashMap(u32, u32).init(allocator);
        defer in_progress.deinit();
        var next_binder: u32 = 0;
        var has_unresolved_placeholder = false;
        try self.appendCanonicalTokensRec(
            allocator,
            root,
            tokens,
            &in_progress,
            &next_binder,
            &has_unresolved_placeholder,
        );
        return has_unresolved_placeholder;
    }

    fn canonicalTokensEqual(
        self: *const Store,
        allocator: Allocator,
        lhs: Idx,
        rhs: Idx,
    ) Allocator.Error!bool {
        var lhs_tokens = std.ArrayList(u64).empty;
        defer lhs_tokens.deinit(allocator);
        if (try self.buildCanonicalTokens(allocator, lhs, &lhs_tokens)) return false;

        var rhs_tokens = std.ArrayList(u64).empty;
        defer rhs_tokens.deinit(allocator);
        if (try self.buildCanonicalTokens(allocator, rhs, &rhs_tokens)) return false;

        return std.mem.eql(u64, lhs_tokens.items, rhs_tokens.items);
    }

    fn internExistingMonotype(
        self: *Store,
        allocator: Allocator,
        idx: Idx,
    ) Allocator.Error!Idx {
        if (idx == self.unit_idx) return idx;

        if (self.getMonotype(idx) == .prim) {
            return self.primIdx(self.getMonotype(idx).prim);
        }

        const idx_u32 = @intFromEnum(idx);
        if (self.canonical_cache.get(idx_u32)) |cached| return cached;

        var tokens = std.ArrayList(u64).empty;
        defer tokens.deinit(allocator);
        const has_unresolved_placeholder = try self.buildCanonicalTokens(allocator, idx, &tokens);
        if (has_unresolved_placeholder) return idx;

        var hasher = std.hash.Wyhash.init(0);
        hasher.update(std.mem.sliceAsBytes(tokens.items));
        const hash = hasher.final();

        var bucket = try self.canonical_by_hash.getOrPut(allocator, hash);
        if (!bucket.found_existing) {
            bucket.value_ptr.* = .{};
        } else {
            for (bucket.value_ptr.items) |candidate| {
                if (try self.canonicalTokensEqual(allocator, idx, candidate)) {
                    try self.canonical_cache.put(allocator, idx_u32, candidate);
                    return candidate;
                }
            }
        }

        try bucket.value_ptr.append(allocator, idx);
        try self.canonical_cache.put(allocator, idx_u32, idx);
        return idx;
    }

    /// Convert a CIR type variable to a Monotype, recursively resolving all
    /// type structure. Uses `seen` for cycle detection on recursive types.
    pub fn fromTypeVar(
        self: *Store,
        allocator: Allocator,
        types_store: *const types.Store,
        type_var: types.Var,
        common_idents: CommonIdents,
        seen: *std.AutoHashMap(types.Var, Idx),
        scratches: *Scratches,
    ) Allocator.Error!Idx {
        const resolved = types_store.resolveVar(type_var);

        // Cycle detection: if we've already seen this var, return the cached idx.
        // If the cached graph has finished resolving, canonicalize before returning.
        if (seen.get(resolved.var_)) |cached| {
            const interned_cached = try self.internExistingMonotype(allocator, cached);
            if (interned_cached != cached) {
                try seen.put(resolved.var_, interned_cached);
            }
            return interned_cached;
        }

        const raw_idx = switch (resolved.desc.content) {
            .flex => |flex| if (hasNumeralConstraint(types_store, flex.constraints))
                self.primIdx(.dec)
            else
                self.unit_idx,
            .rigid => |rigid| if (hasNumeralConstraint(types_store, rigid.constraints))
                self.primIdx(.dec)
            else
                self.unit_idx,
            .alias => |alias| blk: {
                // Aliases are transparent — follow the backing var
                const backing_var = types_store.getAliasBackingVar(alias);
                break :blk try self.fromTypeVar(allocator, types_store, backing_var, common_idents, seen, scratches);
            },
            .structure => |flat_type| blk: {
                break :blk try self.fromFlatType(allocator, types_store, resolved.var_, flat_type, common_idents, seen, scratches);
            },
            // `.err` is a poison type from type-checking failures.
            // Lower it to unit so MIR lowering can continue and preserve
            // diagnostics/runtime error behavior instead of crashing.
            .err => self.unit_idx,
        };

        const interned = try self.internExistingMonotype(allocator, raw_idx);
        try seen.put(resolved.var_, interned);
        return interned;
    }

    fn fromFlatType(
        self: *Store,
        allocator: Allocator,
        types_store: *const types.Store,
        var_: types.Var,
        flat_type: types.FlatType,
        common_idents: CommonIdents,
        seen: *std.AutoHashMap(types.Var, Idx),
        scratches: *Scratches,
    ) Allocator.Error!Idx {
        return switch (flat_type) {
            .nominal_type => |nominal| {
                return try self.fromNominalType(allocator, types_store, var_, nominal, common_idents, seen, scratches);
            },
            .empty_record => self.unit_idx,
            .empty_tag_union => try self.addMonotype(allocator, .{ .tag_union = .{ .tags = TagSpan.empty() } }),
            .record => |record| {
                // Reserve a slot for cycle detection before recursing into fields
                const placeholder_idx = try self.addPlaceholder(allocator);
                try seen.put(var_, placeholder_idx);

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
                            if (@as(u32, @bitCast(existing.name)) == @as(u32, @bitCast(name))) {
                                seen_name = true;
                                break;
                            }
                        }
                        if (seen_name) continue;

                        const field_type = try self.fromTypeVar(allocator, types_store, field_var, common_idents, seen, scratches);
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
                                            if (@as(u32, @bitCast(existing.name)) == @as(u32, @bitCast(name))) {
                                                seen_name = true;
                                                break;
                                            }
                                        }
                                        if (seen_name) continue;

                                        const field_type = try self.fromTypeVar(allocator, types_store, field_var, common_idents, seen, scratches);
                                        try scratches.fields.append(.{ .name = name, .type_idx = field_type });
                                    }
                                    break :rows;
                                },
                                .empty_record => break :rows,
                                else => break :rows,
                            },
                            else => break :rows, // flex/rigid/err -> end of known chain
                        }
                    }
                }

                const collected_fields = scratches.fields.sliceFromStart(scratch_top);
                // Each row segment is sorted, but concatenation may not be.
                std.mem.sort(Field, collected_fields, {}, Field.sortByNameAsc);
                const field_span = try self.addFields(allocator, collected_fields);
                self.resolvePlaceholder(placeholder_idx, .{ .record = .{ .fields = field_span } });
                return placeholder_idx;
            },
            .record_unbound => |fields_range| {
                // Extensible record — treat like a closed record with the known fields
                const fields_slice = types_store.getRecordFieldsSlice(fields_range);
                const names = fields_slice.items(.name);
                const vars = fields_slice.items(.var_);

                const placeholder_idx = try self.addPlaceholder(allocator);
                try seen.put(var_, placeholder_idx);

                const scratch_top = scratches.fields.top();
                defer scratches.fields.clearFrom(scratch_top);

                for (names, vars) |name, field_var| {
                    const field_type = try self.fromTypeVar(allocator, types_store, field_var, common_idents, seen, scratches);
                    try scratches.fields.append(.{ .name = name, .type_idx = field_type });
                }

                const field_span = try self.addFields(allocator, scratches.fields.sliceFromStart(scratch_top));
                self.resolvePlaceholder(placeholder_idx, .{ .record = .{ .fields = field_span } });
                return placeholder_idx;
            },
            .tuple => |tuple| {
                const placeholder_idx = try self.addPlaceholder(allocator);
                try seen.put(var_, placeholder_idx);

                const elem_vars = types_store.sliceVars(tuple.elems);
                const scratch_top = scratches.idxs.top();
                defer scratches.idxs.clearFrom(scratch_top);

                for (elem_vars) |elem_var| {
                    const elem_type = try self.fromTypeVar(allocator, types_store, elem_var, common_idents, seen, scratches);
                    try scratches.idxs.append(elem_type);
                }

                const elem_span = try self.addIdxSpan(allocator, scratches.idxs.sliceFromStart(scratch_top));
                self.resolvePlaceholder(placeholder_idx, .{ .tuple = .{ .elems = elem_span } });
                return placeholder_idx;
            },
            .tag_union => |tag_union_row| {
                const placeholder_idx = try self.addPlaceholder(allocator);
                try seen.put(var_, placeholder_idx);

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
                            const payload_type = try self.fromTypeVar(allocator, types_store, arg_var, common_idents, seen, scratches);
                            try scratches.idxs.append(payload_type);
                        }

                        const payloads_span = try self.addIdxSpan(allocator, scratches.idxs.sliceFromStart(idxs_top));
                        try scratches.tags.append(.{ .name = name, .payloads = payloads_span });
                    }

                    // Follow extension variable to find more tags, transparently
                    // resolving aliases in the extension chain.
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
                                else => break :rows,
                            },
                            else => break :rows, // flex/rigid/err -> end of known chain
                        }
                    }
                }

                const collected_tags = scratches.tags.sliceFromStart(tags_top);
                // Sort tags alphabetically to match discriminant assignment order.
                // Each ext-chain row is pre-sorted, but the concatenation may not be.
                std.mem.sort(Tag, collected_tags, {}, Tag.sortByNameAsc);
                const tag_span = try self.addTags(allocator, collected_tags);
                self.resolvePlaceholder(placeholder_idx, .{ .tag_union = .{ .tags = tag_span } });
                return placeholder_idx;
            },
            .fn_pure => |func| try self.fromFuncType(allocator, types_store, var_, func, false, common_idents, seen, scratches),
            .fn_effectful => |func| try self.fromFuncType(allocator, types_store, var_, func, true, common_idents, seen, scratches),
            .fn_unbound => |func| try self.fromFuncType(allocator, types_store, var_, func, false, common_idents, seen, scratches),
        };
    }

    fn fromFuncType(
        self: *Store,
        allocator: Allocator,
        types_store: *const types.Store,
        var_: types.Var,
        func: types.Func,
        effectful: bool,
        common_idents: CommonIdents,
        seen: *std.AutoHashMap(types.Var, Idx),
        scratches: *Scratches,
    ) Allocator.Error!Idx {
        const placeholder_idx = try self.addPlaceholder(allocator);
        try seen.put(var_, placeholder_idx);

        const arg_vars = types_store.sliceVars(func.args);
        const scratch_top = scratches.idxs.top();
        defer scratches.idxs.clearFrom(scratch_top);

        for (arg_vars) |arg_var| {
            const arg_type = try self.fromTypeVar(allocator, types_store, arg_var, common_idents, seen, scratches);
            try scratches.idxs.append(arg_type);
        }

        const args_span = try self.addIdxSpan(allocator, scratches.idxs.sliceFromStart(scratch_top));
        const ret = try self.fromTypeVar(allocator, types_store, func.ret, common_idents, seen, scratches);

        self.resolvePlaceholder(placeholder_idx, .{ .func = .{
            .args = args_span,
            .ret = ret,
            .effectful = effectful,
        } });
        return placeholder_idx;
    }

    fn fromNominalType(
        self: *Store,
        allocator: Allocator,
        types_store: *const types.Store,
        nominal_var: types.Var,
        nominal: types.NominalType,
        common_idents: CommonIdents,
        seen: *std.AutoHashMap(types.Var, Idx),
        scratches: *Scratches,
    ) Allocator.Error!Idx {
        const ident = nominal.ident.ident_idx;
        const origin = nominal.origin_module;

        if (origin == common_idents.builtin_module) {
            // Bool/Str: unqualified idents from source declarations
            if (ident == common_idents.str) return self.primIdx(.str);
            if (ident == common_idents.bool) return self.primIdx(.bool);
        }

        if (origin == common_idents.builtin_module) {

            // List: unqualified ident from mkListContent
            if (ident == common_idents.list) {
                const type_args = types_store.sliceNominalArgs(nominal);
                if (type_args.len > 0) {
                    const elem_type = try self.fromTypeVar(allocator, types_store, type_args[0], common_idents, seen, scratches);
                    return try self.addMonotype(allocator, .{ .list = .{ .elem = elem_type } });
                }
                return self.unit_idx;
            }

            // Box: unqualified ident from mkBoxContent
            if (ident == common_idents.box) {
                const type_args = types_store.sliceNominalArgs(nominal);
                if (type_args.len > 0) {
                    const inner_type = try self.fromTypeVar(allocator, types_store, type_args[0], common_idents, seen, scratches);
                    return try self.addMonotype(allocator, .{ .box = .{ .inner = inner_type } });
                }
                return self.unit_idx;
            }

            // Numeric types: qualified idents from mkNumberTypeContent (e.g. "Builtin.Num.I64")
            if (ident == common_idents.i64_type) return self.primIdx(.i64);
            if (ident == common_idents.u8_type) return self.primIdx(.u8);
            if (ident == common_idents.i8_type) return self.primIdx(.i8);
            if (ident == common_idents.u16_type) return self.primIdx(.u16);
            if (ident == common_idents.i16_type) return self.primIdx(.i16);
            if (ident == common_idents.u32_type) return self.primIdx(.u32);
            if (ident == common_idents.i32_type) return self.primIdx(.i32);
            if (ident == common_idents.u64_type) return self.primIdx(.u64);
            if (ident == common_idents.u128_type) return self.primIdx(.u128);
            if (ident == common_idents.i128_type) return self.primIdx(.i128);
            if (ident == common_idents.f32_type) return self.primIdx(.f32);
            if (ident == common_idents.f64_type) return self.primIdx(.f64);
            if (ident == common_idents.dec_type) return self.primIdx(.dec);
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
        const placeholder_idx = try self.addPlaceholder(allocator);
        try seen.put(nominal_var, placeholder_idx);

        const backing_var = types_store.getNominalBackingVar(nominal);
        const backing_idx = try self.fromTypeVar(allocator, types_store, backing_var, common_idents, seen, scratches);

        // Copy the resolved backing type's value into our placeholder slot.
        // This value-copy is safe (unlike the other handlers which build fresh
        // values from indices) because every field inside a Monotype is an index
        // (Idx, Span, Ident.Idx) — never a pointer. The monotype store is
        // append-only, so all indices remain valid after the copy.
        self.resolvePlaceholder(placeholder_idx, self.monotypes.items[@intFromEnum(backing_idx)]);
        try self.setNominalHint(allocator, placeholder_idx, .{
            .module_idx = scratches.current_module_idx,
            .ident = nominal.ident.ident_idx,
        });
        return placeholder_idx;
    }
};
