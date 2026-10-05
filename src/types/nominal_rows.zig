//! Persistent ownership and transaction state for delayed nominal row openings.
//!
//! IDs and references belong to the containing type Store. Immutable schema
//! edges remain distinct from solver cells; publication never borrows an
//! instantiator, reader namespace, or allocator-owned substitution map.

const std = @import("std");
const base = @import("base");
const collections = @import("collections");
const types = @import("types.zig");

const Allocator = std.mem.Allocator;
const none = std.math.maxInt(u32);

/// One closed declaration row, shared by openings, not application state.
/// Its tag range belongs to Store.tags and its payload ranges to Store.vars.
pub const Schema = extern struct {
    declaration: types.NominalDecl.Idx,
    backing: types.Var,
    formals_start: u32,
    formals_count: u32,
    tags_start: u32,
    tags_count: u32,
    closed_tail: types.Var,

    pub const Idx = enum(u32) { _ };
    pub const List = collections.SafeList(@This());
};

/// A separately owned instantiation of a schema. All fields are fixed-width;
/// linked substitution/history entries contain Store-local IDs, not pointers.
pub const Opening = extern struct {
    schema: Schema.Idx,
    actuals_start: u32,
    actuals_count: u32,
    initial_rank: u32,
    creation_origin: base.ModuleIdentity.Idx,
    region_start: u32,
    region_end: u32,
    binding_head: u32 = none,
    name_head: u32 = none,
    rank_head: u32 = none,
    status: Status = .ready,

    pub const Idx = enum(u32) { _ };
    pub const List = collections.SafeList(@This());
    pub const Status = enum(u32) { ready, failed };

    pub fn creationRegion(self: @This()) base.Region {
        return base.Region.from_raw_offsets(self.region_start, self.region_end);
    }
};

/// The exact remaining occurrences of a closed schema row. Exclusions are
/// occurrence indices, not a set of names: duplicate-label equalities survive.
/// The residual is an ordinary solver reference owned by this Store.
pub const Fragment = extern struct {
    opening: Opening.Idx,
    exclusions_start: u32,
    exclusions_count: u32,
    residual: types.Var,

    pub const Idx = enum(u32) { _ };
    pub const List = collections.SafeList(@This());
};

pub const Binding = extern struct {
    template: types.Var,
    owned: types.Var,
    next: u32,

    pub const List = collections.SafeList(@This());
};

pub const NameBinding = extern struct {
    name: base.Ident.Idx,
    owned: types.Var,
    next: u32,

    pub const List = collections.SafeList(@This());
};

/// Generalization is a per-root event, not a reset of the opening's rank.
/// A generalized history entry and an escaped monomorphic entry consequently
/// remain distinguishable when a scheme is copied before payload demand.
pub const RankEntry = extern struct {
    template: types.Var,
    rank: u32,
    next: u32,

    pub const List = collections.SafeList(@This());
};

/// An immutable schema's exact translation at an import boundary. Foreign keys
/// are serialized source-Store/schema IDs qualified by module identity, not
/// borrowed reader IDs or graph edges into the source Store.
pub const SchemaImport = extern struct {
    source_module: base.ModuleIdentity.Idx,
    source_schema: Schema.Idx,
    local_schema: Schema.Idx,
    translations_start: u32,
    translations_count: u32,

    pub const List = collections.SafeList(@This());
};

pub const TemplateTranslation = extern struct {
    source: types.Var,
    local: types.Var,

    pub const List = collections.SafeList(@This());
};

/// A logical graph edge. Template IDs are immutable schema references scoped
/// to one opening, never mutable solver representatives or reader-only IDs.
pub const Reference = union(enum) {
    owned: types.Var,
    template: struct { opening: Opening.Idx, var_: types.Var },

    pub fn child(parent: Reference, var_: types.Var) Reference {
        return switch (parent) {
            .owned => .{ .owned = var_ },
            .template => |scope| .{ .template = .{ .opening = scope.opening, .var_ = var_ } },
        };
    }
};

pub const View = struct {
    reference: Reference,
    desc: types.Descriptor,
};

const OpeningUndo = struct { idx: Opening.Idx, old: Opening };

/// Append-only tables with journaled opening heads. Updating an existing map
/// entry appends a replacement and changes its owner's head; rollback restores
/// those heads as well as table lengths. Length-only rollback is insufficient.
pub const Tables = struct {
    schemas: Schema.List = .{},
    openings: Opening.List = .{},
    fragments: Fragment.List = .{},
    bindings: Binding.List = .{},
    names: NameBinding.List = .{},
    ranks: RankEntry.List = .{},
    exclusions: collections.SafeList(u32) = .{},
    schema_imports: SchemaImport.List = .{},
    template_translations: TemplateTranslation.List = .{},

    // Runtime transaction state is deliberately absent from Serialized.
    undo: std.ArrayList(OpeningUndo) = .empty,
    baseline_openings: ?u32 = null,

    pub const Savepoint = struct {
        schemas: usize,
        openings: usize,
        fragments: usize,
        bindings: usize,
        names: usize,
        ranks: usize,
        exclusions: usize,
        schema_imports: usize,
        template_translations: usize,
        undo: usize,
    };

    pub fn deinit(self: *Tables, gpa: Allocator) void {
        self.schemas.deinit(gpa);
        self.openings.deinit(gpa);
        self.fragments.deinit(gpa);
        self.bindings.deinit(gpa);
        self.names.deinit(gpa);
        self.ranks.deinit(gpa);
        self.exclusions.deinit(gpa);
        self.schema_imports.deinit(gpa);
        self.template_translations.deinit(gpa);
        self.undo.deinit(gpa);
    }

    pub fn clone(self: *const Tables, gpa: Allocator) Allocator.Error!Tables {
        var result: Tables = .{};
        errdefer result.deinit(gpa);
        result.schemas = try self.schemas.clone(gpa);
        result.openings = try self.openings.clone(gpa);
        result.fragments = try self.fragments.clone(gpa);
        result.bindings = try self.bindings.clone(gpa);
        result.names = try self.names.clone(gpa);
        result.ranks = try self.ranks.clone(gpa);
        result.exclusions = try self.exclusions.clone(gpa);
        result.schema_imports = try self.schema_imports.clone(gpa);
        result.template_translations = try self.template_translations.clone(gpa);
        return result;
    }

    pub fn appendSchema(self: *Tables, gpa: Allocator, schema: Schema) Allocator.Error!Schema.Idx {
        return @enumFromInt(@intFromEnum(try self.schemas.append(gpa, schema)));
    }

    pub fn appendOpening(self: *Tables, gpa: Allocator, opening: Opening) Allocator.Error!Opening.Idx {
        std.debug.assert(@intFromEnum(opening.schema) < self.schemas.len());
        return @enumFromInt(@intFromEnum(try self.openings.append(gpa, opening)));
    }

    pub fn getOpening(self: *const Tables, idx: Opening.Idx) Opening {
        return self.openings.get(@enumFromInt(@intFromEnum(idx))).*;
    }

    pub fn getSchema(self: *const Tables, idx: Schema.Idx) Schema {
        return self.schemas.get(@enumFromInt(@intFromEnum(idx))).*;
    }

    pub fn getFragment(self: *const Tables, idx: Fragment.Idx) Fragment {
        return self.fragments.get(@enumFromInt(@intFromEnum(idx))).*;
    }

    pub fn schemaImport(self: *const Tables, source_module: base.ModuleIdentity.Idx, source_schema: Schema.Idx) ?SchemaImport {
        for (self.schema_imports.items.items) |entry| {
            if (entry.source_module == source_module and entry.source_schema == source_schema) return entry;
        }
        return null;
    }

    pub fn translateTemplate(self: *const Tables, imported: SchemaImport, source: types.Var) types.Var {
        const entries = self.template_translations.items.items[imported.translations_start..][0..imported.translations_count];
        for (entries) |entry| if (entry.source == source) return entry.local;
        // The schema-copy producer supplies every referenced template root.
        // Reconstructing a correspondence from a destination graph is forbidden.
        @panic("nominal schema import is missing an explicit template translation");
    }

    /// Publish the schema-copy producer's complete correspondence, not a map
    /// inferred from an existing destination declaration. Immutable schema
    /// translation is shared independently of application opening ownership.
    pub fn publishSchemaImport(
        self: *Tables,
        gpa: Allocator,
        source_module: base.ModuleIdentity.Idx,
        source_schema: Schema.Idx,
        local_schema: Schema.Idx,
        translations: []const TemplateTranslation,
    ) Allocator.Error!SchemaImport {
        std.debug.assert(self.schemaImport(source_module, source_schema) == null);
        std.debug.assert(@intFromEnum(local_schema) < self.schemas.len());
        const start: u32 = @intCast(self.template_translations.len());
        errdefer self.template_translations.items.shrinkRetainingCapacity(start);
        for (translations) |entry| _ = try self.template_translations.append(gpa, entry);
        const imported = SchemaImport{
            .source_module = source_module,
            .source_schema = source_schema,
            .local_schema = local_schema,
            .translations_start = start,
            .translations_count = @intCast(translations.len),
        };
        _ = try self.schema_imports.append(gpa, imported);
        return imported;
    }

    pub fn binding(self: *const Tables, owner: Opening.Idx, template: types.Var) ?types.Var {
        var index = self.getOpening(owner).binding_head;
        while (index != none) {
            const entry = self.bindings.get(@enumFromInt(index)).*;
            if (entry.template == template) return entry.owned;
            index = entry.next;
        }
        return null;
    }

    pub fn nameBinding(self: *const Tables, owner: Opening.Idx, name: base.Ident.Idx) ?types.Var {
        var index = self.getOpening(owner).name_head;
        while (index != none) {
            const entry = self.names.get(@enumFromInt(index)).*;
            if (entry.name.eql(name)) return entry.owned;
            index = entry.next;
        }
        return null;
    }

    pub fn effectiveRank(self: *const Tables, owner: Opening.Idx, template: types.Var) types.Rank {
        const opening = self.getOpening(owner);
        var index = opening.rank_head;
        while (index != none) {
            const entry = self.ranks.get(@enumFromInt(index)).*;
            if (entry.template == template) return @enumFromInt(entry.rank);
            index = entry.next;
        }
        return @enumFromInt(opening.initial_rank);
    }

    fn journal(self: *Tables, gpa: Allocator, owner: Opening.Idx) Allocator.Error!void {
        if (self.baseline_openings) |baseline| {
            if (@intFromEnum(owner) < baseline) {
                try self.undo.append(gpa, .{ .idx = owner, .old = self.getOpening(owner) });
            }
        }
    }

    pub fn putBinding(self: *Tables, gpa: Allocator, owner: Opening.Idx, template: types.Var, owned: types.Var) Allocator.Error!void {
        if (self.binding(owner, template) == owned) return;
        try self.appendBindingEntry(gpa, owner, template, owned);
    }

    /// A producer insertion delta proves this key was absent from its exact
    /// seeded map. Do not rescan that map's persistent chain in release builds.
    /// Replacements still use putBinding and preserve its existing-key rule.
    pub fn appendNewBinding(self: *Tables, gpa: Allocator, owner: Opening.Idx, template: types.Var, owned: types.Var) Allocator.Error!void {
        std.debug.assert(self.binding(owner, template) == null);
        try self.appendBindingEntry(gpa, owner, template, owned);
    }

    fn appendBindingEntry(self: *Tables, gpa: Allocator, owner: Opening.Idx, template: types.Var, owned: types.Var) Allocator.Error!void {
        try self.journal(gpa, owner);
        const old = self.getOpening(owner);
        const idx = try self.bindings.append(gpa, .{ .template = template, .owned = owned, .next = old.binding_head });
        // No fallible work remains between publishing an entry and its head.
        self.openings.get(@enumFromInt(@intFromEnum(owner))).binding_head = @intFromEnum(idx);
    }

    pub fn putNameBinding(self: *Tables, gpa: Allocator, owner: Opening.Idx, name: base.Ident.Idx, owned: types.Var) Allocator.Error!void {
        if (self.nameBinding(owner, name) == owned) return;
        try self.journal(gpa, owner);
        const old = self.getOpening(owner);
        const idx = try self.names.append(gpa, .{ .name = name, .owned = owned, .next = old.name_head });
        self.openings.get(@enumFromInt(@intFromEnum(owner))).name_head = @intFromEnum(idx);
    }

    pub fn recordRank(self: *Tables, gpa: Allocator, owner: Opening.Idx, template: types.Var, rank: types.Rank) Allocator.Error!void {
        if (self.effectiveRank(owner, template) == rank) return;
        try self.journal(gpa, owner);
        const old = self.getOpening(owner);
        const idx = try self.ranks.append(gpa, .{ .template = template, .rank = @intFromEnum(rank), .next = old.rank_head });
        self.openings.get(@enumFromInt(@intFromEnum(owner))).rank_head = @intFromEnum(idx);
    }

    /// Journal invalidation before any demand work. A failure leaves this
    /// opening unusable until its paired Store savepoint is rolled back.
    pub const DemandGuard = struct { owner: Opening.Idx };

    pub fn beginDemand(self: *Tables, gpa: Allocator, owner: Opening.Idx) Allocator.Error!DemandGuard {
        if (self.getOpening(owner).status != .ready) return error.OutOfMemory;
        try self.journal(gpa, owner);
        self.openings.get(@enumFromInt(@intFromEnum(owner))).status = .failed;
        return .{ .owner = owner };
    }

    /// The guard already journaled this status change; completion cannot fail
    /// after publishing a complete substitution map.
    pub fn completeDemand(self: *Tables, guard: DemandGuard) void {
        const opening = self.openings.get(@enumFromInt(@intFromEnum(guard.owner)));
        std.debug.assert(opening.status == .failed);
        opening.status = .ready;
    }

    pub fn appendFragment(
        self: *Tables,
        gpa: Allocator,
        owner: Opening.Idx,
        excluded: []const u32,
        residual: types.Var,
    ) Allocator.Error!Fragment.Idx {
        const schema = self.getSchema(self.getOpening(owner).schema);
        const start: u32 = @intCast(self.exclusions.len());
        errdefer self.exclusions.items.shrinkRetainingCapacity(start);
        for (excluded) |occurrence| {
            std.debug.assert(occurrence < schema.tags_count);
            _ = try self.exclusions.append(gpa, occurrence);
        }
        return @enumFromInt(@intFromEnum(try self.fragments.append(gpa, .{
            .opening = owner,
            .exclusions_start = start,
            .exclusions_count = @intCast(excluded.len),
            .residual = residual,
        })));
    }

    pub fn excludes(self: *const Tables, fragment: Fragment.Idx, occurrence: u32) bool {
        const row = self.getFragment(fragment);
        const excluded = self.exclusions.items.items[row.exclusions_start..][0..row.exclusions_count];
        return std.mem.indexOfScalar(u32, excluded, occurrence) != null;
    }

    pub fn createSavepoint(self: *Tables) Savepoint {
        std.debug.assert(self.baseline_openings == null);
        self.baseline_openings = @intCast(self.openings.len());
        return .{
            .schemas = self.schemas.items.items.len,
            .openings = self.openings.items.items.len,
            .fragments = self.fragments.items.items.len,
            .bindings = self.bindings.items.items.len,
            .names = self.names.items.items.len,
            .ranks = self.ranks.items.items.len,
            .exclusions = self.exclusions.items.items.len,
            .schema_imports = self.schema_imports.items.items.len,
            .template_translations = self.template_translations.items.items.len,
            .undo = self.undo.items.len,
        };
    }

    pub fn commitSavepoint(self: *Tables, saved: Savepoint) void {
        std.debug.assert(self.baseline_openings != null);
        self.undo.shrinkRetainingCapacity(saved.undo);
        self.baseline_openings = null;
    }

    pub fn rollbackToSavepoint(self: *Tables, saved: Savepoint) void {
        std.debug.assert(self.baseline_openings != null);
        var index = self.undo.items.len;
        while (index > saved.undo) {
            index -= 1;
            const entry = self.undo.items[index];
            self.openings.get(@enumFromInt(@intFromEnum(entry.idx))).* = entry.old;
        }
        self.undo.shrinkRetainingCapacity(saved.undo);
        self.schemas.items.shrinkRetainingCapacity(saved.schemas);
        self.openings.items.shrinkRetainingCapacity(saved.openings);
        self.fragments.items.shrinkRetainingCapacity(saved.fragments);
        self.bindings.items.shrinkRetainingCapacity(saved.bindings);
        self.names.items.shrinkRetainingCapacity(saved.names);
        self.ranks.items.shrinkRetainingCapacity(saved.ranks);
        self.exclusions.items.shrinkRetainingCapacity(saved.exclusions);
        self.schema_imports.items.shrinkRetainingCapacity(saved.schema_imports);
        self.template_translations.items.shrinkRetainingCapacity(saved.template_translations);
        self.baseline_openings = null;
    }

    pub fn eql(self: *const Tables, other: *const Tables) bool {
        inline for (.{ "schemas", "openings", "fragments", "bindings", "names", "ranks", "exclusions", "schema_imports", "template_translations" }) |field| {
            const a = @field(self, field).items.items;
            const b = @field(other, field).items.items;
            if (a.len != b.len) return false;
            for (a, b) |lhs, rhs| if (!std.meta.eql(lhs, rhs)) return false;
        }
        return true;
    }

    /// Legacy relocated Store serialization uses the same complete tables.
    pub fn serialize(self: *const Tables, gpa: Allocator, writer: *collections.CompactWriter) Allocator.Error!Tables {
        std.debug.assert(self.baseline_openings == null);
        for (self.openings.items.items) |opening| std.debug.assert(opening.status == .ready);
        return .{
            .schemas = (try self.schemas.serialize(gpa, writer)).*,
            .openings = (try self.openings.serialize(gpa, writer)).*,
            .fragments = (try self.fragments.serialize(gpa, writer)).*,
            .bindings = (try self.bindings.serialize(gpa, writer)).*,
            .names = (try self.names.serialize(gpa, writer)).*,
            .ranks = (try self.ranks.serialize(gpa, writer)).*,
            .exclusions = (try self.exclusions.serialize(gpa, writer)).*,
            .schema_imports = (try self.schema_imports.serialize(gpa, writer)).*,
            .template_translations = (try self.template_translations.serialize(gpa, writer)).*,
        };
    }

    pub fn relocate(self: *Tables, offset: isize) void {
        self.schemas.relocate(offset);
        self.openings.relocate(offset);
        self.fragments.relocate(offset);
        self.bindings.relocate(offset);
        self.names.relocate(offset);
        self.ranks.relocate(offset);
        self.exclusions.relocate(offset);
        self.schema_imports.relocate(offset);
        self.template_translations.relocate(offset);
    }

    /// The typed persistent layout. Runtime undo trails are not frozen.
    pub const Serialized = extern struct {
        schemas: Schema.List.Serialized,
        openings: Opening.List.Serialized,
        fragments: Fragment.List.Serialized,
        bindings: Binding.List.Serialized,
        names: NameBinding.List.Serialized,
        ranks: RankEntry.List.Serialized,
        exclusions: collections.SafeList(u32).Serialized,
        schema_imports: SchemaImport.List.Serialized,
        template_translations: TemplateTranslation.List.Serialized,

        pub fn serialize(self: *Serialized, tables: *const Tables, gpa: Allocator, writer: *collections.CompactWriter) Allocator.Error!void {
            std.debug.assert(tables.baseline_openings == null);
            for (tables.openings.items.items) |opening| std.debug.assert(opening.status == .ready);
            try self.schemas.serialize(&tables.schemas, gpa, writer);
            try self.openings.serialize(&tables.openings, gpa, writer);
            try self.fragments.serialize(&tables.fragments, gpa, writer);
            try self.bindings.serialize(&tables.bindings, gpa, writer);
            try self.names.serialize(&tables.names, gpa, writer);
            try self.ranks.serialize(&tables.ranks, gpa, writer);
            try self.exclusions.serialize(&tables.exclusions, gpa, writer);
            try self.schema_imports.serialize(&tables.schema_imports, gpa, writer);
            try self.template_translations.serialize(&tables.template_translations, gpa, writer);
        }

        pub fn deserializeInto(self: *const Serialized, base_addr: usize) Tables {
            return .{
                .schemas = self.schemas.deserializeInto(base_addr),
                .openings = self.openings.deserializeInto(base_addr),
                .fragments = self.fragments.deserializeInto(base_addr),
                .bindings = self.bindings.deserializeInto(base_addr),
                .names = self.names.deserializeInto(base_addr),
                .ranks = self.ranks.deserializeInto(base_addr),
                .exclusions = self.exclusions.deserializeInto(base_addr),
                .schema_imports = self.schema_imports.deserializeInto(base_addr),
                .template_translations = self.template_translations.deserializeInto(base_addr),
            };
        }

        pub fn deserializeWithCopy(self: *const Serialized, base_addr: usize, gpa: Allocator) Allocator.Error!Tables {
            const view = self.deserializeInto(base_addr);
            return view.clone(gpa);
        }
    };
};
