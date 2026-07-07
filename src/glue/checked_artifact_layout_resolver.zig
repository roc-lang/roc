//! Resolves checked-artifact types into committed ordinary-data layouts.
//!
//! This is the glue/cache-path sibling of `type_layout_resolver.zig`: it consumes
//! serialized checked artifacts instead of live checker vars, then finalizes
//! through the shared layout graph/store.

const std = @import("std");
const check = @import("check");
const layout = @import("layout");

const CheckedArtifact = check.CheckedArtifact;
const CanonicalNameStore = check.CanonicalNames.CanonicalNameStore;
const Idx = layout.Idx;
const Store = layout.Store;
const Graph = layout.Graph;
const GraphField = layout.GraphField;
const GraphNodeId = layout.GraphNodeId;
const GraphRef = layout.GraphRef;

/// Lookup table from checked-module cache keys to the serialized checked artifacts
/// needed to resolve imported nominal declarations.
pub const ArtifactMap = std.AutoHashMap(CheckedArtifact.CheckedModuleArtifactKey, *const CheckedArtifact.CheckedModuleArtifact);

/// Failures that can occur while converting checked-artifact types to layouts.
pub const Error = std.mem.Allocator.Error || error{
    UnresolvedByValue,
};

const ParentContext = enum {
    ordinary,
    heap_indirect,
};

const TypeKey = struct {
    artifact_key: CheckedArtifact.CheckedModuleArtifactKey,
    checked_type: CheckedArtifact.CheckedTypeId,
};

const BuildState = struct {
    graph: Graph = .{},
    refs_by_type: std.AutoHashMap(TypeKey, GraphRef),

    fn init(allocator: std.mem.Allocator) BuildState {
        return .{ .refs_by_type = std.AutoHashMap(TypeKey, GraphRef).init(allocator) };
    }

    fn deinit(self: *BuildState, allocator: std.mem.Allocator) void {
        self.graph.deinit(allocator);
        self.refs_by_type.deinit();
    }
};

const NominalDeclarationLookup = struct {
    artifact: *const CheckedArtifact.CheckedModuleArtifact,
    declaration: CheckedArtifact.CheckedNominalDeclaration,
};

/// Resolves checked-artifact type IDs to interned layout IDs using committed
/// nominal declarations from the artifact set.
pub const Resolver = struct {
    store: *Store,
    allocator: std.mem.Allocator,
    artifacts_by_key: *const ArtifactMap,
    canonical_cache: std.AutoHashMap(TypeKey, Idx),

    pub fn init(store: *Store, artifacts_by_key: *const ArtifactMap) Resolver {
        return .{
            .store = store,
            .allocator = store.allocator,
            .artifacts_by_key = artifacts_by_key,
            .canonical_cache = std.AutoHashMap(TypeKey, Idx).init(store.allocator),
        };
    }

    pub fn deinit(self: *Resolver) void {
        self.canonical_cache.deinit();
    }

    pub fn resolve(
        self: *Resolver,
        artifact: *const CheckedArtifact.CheckedModuleArtifact,
        checked_type: CheckedArtifact.CheckedTypeId,
    ) Error!Idx {
        const key = TypeKey{ .artifact_key = artifact.key, .checked_type = checked_type };
        if (self.canonical_cache.get(key)) |cached| return cached;

        var build_state = BuildState.init(self.allocator);
        defer build_state.deinit(self.allocator);

        const root = try self.buildRefForType(artifact, checked_type, .ordinary, &build_state);
        const layout_idx = try self.store.internGraph(&build_state.graph, root);
        try self.canonical_cache.put(key, layout_idx);
        return layout_idx;
    }

    fn buildRefForType(
        self: *Resolver,
        artifact: *const CheckedArtifact.CheckedModuleArtifact,
        checked_type: CheckedArtifact.CheckedTypeId,
        parent_context: ParentContext,
        build_state: *BuildState,
    ) Error!GraphRef {
        const key = TypeKey{ .artifact_key = artifact.key, .checked_type = checked_type };
        if (build_state.refs_by_type.get(key)) |cached| return cached;
        if (self.canonical_cache.get(key)) |cached| return .{ .canonical = cached };

        return switch (checkedTypePayload(artifact, checked_type)) {
            .pending => unreachable,
            .flex, .rigid => if (parent_context == .heap_indirect)
                .{ .canonical = .opaque_ptr }
            else
                error.UnresolvedByValue,
            .alias => |alias| try self.buildRefForType(artifact, alias.backing, parent_context, build_state),
            .record => |record| try self.buildRecordRef(artifact, record.fields, record.ext, build_state),
            .record_unbound => |fields| try self.buildRecordRef(artifact, fields, null, build_state),
            .tuple => |items| try self.buildTupleRef(artifact, items, build_state),
            .nominal => |nominal| try self.buildNominalRef(artifact, checked_type, nominal, parent_context, build_state),
            .function => try self.buildNode(build_state, .erased_callable),
            .empty_record, .empty_tag_union => .{ .canonical = .zst },
            .tag_union => |tag_union| try self.buildTagUnionRef(artifact, tag_union.tags, tag_union.ext, build_state),
        };
    }

    fn buildNominalRef(
        self: *Resolver,
        artifact: *const CheckedArtifact.CheckedModuleArtifact,
        checked_type: CheckedArtifact.CheckedTypeId,
        nominal: CheckedArtifact.CheckedNominalType,
        parent_context: ParentContext,
        build_state: *BuildState,
    ) Error!GraphRef {
        if (nominal.builtin) |builtin| {
            if (try self.builtinLayoutRef(artifact, builtin, nominal, build_state)) |ref| return ref;
        }
        if (nominal.representation == .opaque_without_backing) {
            return if (parent_context == .heap_indirect) .{ .canonical = .opaque_ptr } else error.UnresolvedByValue;
        }

        const key = TypeKey{ .artifact_key = artifact.key, .checked_type = checked_type };
        if (build_state.refs_by_type.get(key)) |cached| return cached;
        if (self.canonical_cache.get(key)) |cached| return .{ .canonical = cached };

        const placeholder = try build_state.graph.reserveNode(self.allocator);
        const placeholder_ref = GraphRef{ .local = placeholder };
        try build_state.refs_by_type.put(key, placeholder_ref);

        if (try self.buildDeclaredNominalRecordRef(artifact, nominal, placeholder, build_state)) {
            return placeholder_ref;
        }

        const backing_ref = try self.buildRefForType(artifact, nominal.backing, parent_context, build_state);
        switch (backing_ref) {
            .canonical => {
                try build_state.refs_by_type.put(key, backing_ref);
                return backing_ref;
            },
            .local => |backing_node| {
                if (backing_node == placeholder) unreachable;
                build_state.graph.setNode(placeholder, build_state.graph.getNode(backing_node));
                return placeholder_ref;
            },
        }
    }

    fn builtinLayoutRef(
        self: *Resolver,
        artifact: *const CheckedArtifact.CheckedModuleArtifact,
        builtin: CheckedArtifact.CheckedBuiltinNominal,
        nominal: CheckedArtifact.CheckedNominalType,
        build_state: *BuildState,
    ) Error!?GraphRef {
        return switch (builtin) {
            .bool => .{ .canonical = .bool },
            .str => .{ .canonical = .str },
            .u8 => .{ .canonical = .u8 },
            .i8 => .{ .canonical = .i8 },
            .u16 => .{ .canonical = .u16 },
            .i16 => .{ .canonical = .i16 },
            .u32 => .{ .canonical = .u32 },
            .i32 => .{ .canonical = .i32 },
            .u64 => .{ .canonical = .u64 },
            .i64 => .{ .canonical = .i64 },
            .u128 => .{ .canonical = .u128 },
            .i128 => .{ .canonical = .i128 },
            .f32 => .{ .canonical = .f32 },
            .f64 => .{ .canonical = .f64 },
            .dec => .{ .canonical = .dec },
            .list => blk: {
                if (nominal.args.len == 0) break :blk .{ .canonical = .zst };
                const child = try self.buildRefForType(artifact, nominal.args[0], .heap_indirect, build_state);
                break :blk try self.buildNode(build_state, .{ .list = child });
            },
            .box => blk: {
                if (nominal.args.len == 0) break :blk .{ .canonical = .opaque_ptr };
                const child = try self.buildRefForType(artifact, nominal.args[0], .heap_indirect, build_state);
                break :blk try self.buildNode(build_state, .{ .box = child });
            },
            .parse_tag_union_spec,
            .fields,
            .field,
            => .{ .canonical = .zst },
            .dict,
            .set,
            .crypto_sha256_digest,
            .crypto_sha256_hasher,
            .crypto_blake3_digest,
            .crypto_blake3_hasher,
            => null,
        };
    }

    fn buildDeclaredNominalRecordRef(
        self: *Resolver,
        artifact: *const CheckedArtifact.CheckedModuleArtifact,
        nominal: CheckedArtifact.CheckedNominalType,
        placeholder: GraphNodeId,
        build_state: *BuildState,
    ) Error!bool {
        const lookup = self.nominalDeclarationFor(artifact, nominal) orelse return false;
        const declared_fields = lookup.declaration.declaredRecordFields(&lookup.artifact.checked_types);
        if (!hasPaddingField(declared_fields)) return false;

        var backing_fields = std.ArrayList(CheckedArtifact.CheckedRecordField).empty;
        defer backing_fields.deinit(self.allocator);
        try self.appendRecordRowFields(artifact, &backing_fields, nominal.backing);
        sortRecordFieldsByName(artifact, backing_fields.items);

        var graph_fields = std.ArrayList(GraphField).empty;
        defer graph_fields.deinit(self.allocator);
        try graph_fields.ensureTotalCapacity(self.allocator, declared_fields.len);

        var padding_index: u16 = @intCast(backing_fields.items.len);
        for (declared_fields) |field| {
            switch (field) {
                .named => |field_name_id| {
                    const field_name = lookup.artifact.canonical_names.recordFieldLabelText(field_name_id);
                    const match = backingFieldByName(artifact, backing_fields.items, field_name) orelse unreachable;
                    graph_fields.appendAssumeCapacity(.{
                        .index = match.index,
                        .child = try self.buildRefForType(artifact, match.field.ty, .ordinary, build_state),
                    });
                },
                .padding => |padding_ty| {
                    graph_fields.appendAssumeCapacity(.{
                        .index = padding_index,
                        .child = try self.buildRefForType(lookup.artifact, padding_ty, .ordinary, build_state),
                        .is_padding = true,
                    });
                    padding_index += 1;
                },
            }
        }

        const span = try build_state.graph.appendFields(self.allocator, graph_fields.items);
        build_state.graph.setNode(placeholder, .{ .struct_ = span });
        try build_state.graph.markNominalStruct(self.allocator, placeholder);
        return true;
    }

    fn buildRecordRef(
        self: *Resolver,
        artifact: *const CheckedArtifact.CheckedModuleArtifact,
        head: []const CheckedArtifact.CheckedRecordField,
        ext: ?CheckedArtifact.CheckedTypeId,
        build_state: *BuildState,
    ) Error!GraphRef {
        var fields = std.ArrayList(CheckedArtifact.CheckedRecordField).empty;
        defer fields.deinit(self.allocator);
        try fields.appendSlice(self.allocator, head);
        if (ext) |ext_id| try self.appendRecordRowFields(artifact, &fields, ext_id);
        if (fields.items.len == 0) return .{ .canonical = .zst };

        sortRecordFieldsByName(artifact, fields.items);

        var graph_fields = std.ArrayList(GraphField).empty;
        defer graph_fields.deinit(self.allocator);
        try graph_fields.ensureTotalCapacity(self.allocator, fields.items.len);
        for (fields.items, 0..) |field, index| {
            graph_fields.appendAssumeCapacity(.{
                .index = @intCast(index),
                .child = try self.buildRefForType(artifact, field.ty, .ordinary, build_state),
            });
        }
        return self.buildStructNode(build_state, graph_fields.items, false);
    }

    fn buildTupleRef(
        self: *Resolver,
        artifact: *const CheckedArtifact.CheckedModuleArtifact,
        items: []const CheckedArtifact.CheckedTypeId,
        build_state: *BuildState,
    ) Error!GraphRef {
        if (items.len == 0) return .{ .canonical = .zst };
        var fields = std.ArrayList(GraphField).empty;
        defer fields.deinit(self.allocator);
        try fields.ensureTotalCapacity(self.allocator, items.len);
        for (items, 0..) |item, index| {
            fields.appendAssumeCapacity(.{
                .index = @intCast(index),
                .child = try self.buildRefForType(artifact, item, .ordinary, build_state),
            });
        }
        return self.buildStructNode(build_state, fields.items, false);
    }

    fn buildTagUnionRef(
        self: *Resolver,
        artifact: *const CheckedArtifact.CheckedModuleArtifact,
        head: []const CheckedArtifact.CheckedTag,
        ext: CheckedArtifact.CheckedTypeId,
        build_state: *BuildState,
    ) Error!GraphRef {
        var tags = std.ArrayList(CheckedArtifact.CheckedTag).empty;
        defer tags.deinit(self.allocator);
        try tags.appendSlice(self.allocator, head);
        try self.appendTagRowTags(artifact, &tags, ext);
        if (tags.items.len == 0) return .{ .canonical = .zst };

        sortTagsByName(artifact, tags.items);

        var variants = std.ArrayList(GraphRef).empty;
        defer variants.deinit(self.allocator);
        try variants.ensureTotalCapacity(self.allocator, tags.items.len);
        for (tags.items) |tag| {
            variants.appendAssumeCapacity(try self.buildPayloadRef(artifact, tag.argsSlice(&artifact.checked_types), build_state));
        }

        const node_id = try build_state.graph.reserveNode(self.allocator);
        const span = try build_state.graph.appendRefs(self.allocator, variants.items);
        build_state.graph.setNode(node_id, .{ .tag_union = span });
        return .{ .local = node_id };
    }

    fn buildPayloadRef(
        self: *Resolver,
        artifact: *const CheckedArtifact.CheckedModuleArtifact,
        payload: []const CheckedArtifact.CheckedTypeId,
        build_state: *BuildState,
    ) Error!GraphRef {
        if (payload.len == 0) return .{ .canonical = .zst };
        return self.buildTupleRef(artifact, payload, build_state);
    }

    fn buildStructNode(
        self: *Resolver,
        build_state: *BuildState,
        fields: []const GraphField,
        nominal_struct: bool,
    ) Error!GraphRef {
        if (fields.len == 0) return .{ .canonical = .zst };
        const node_id = try build_state.graph.reserveNode(self.allocator);
        const span = try build_state.graph.appendFields(self.allocator, fields);
        build_state.graph.setNode(node_id, .{ .struct_ = span });
        if (nominal_struct) try build_state.graph.markNominalStruct(self.allocator, node_id);
        return .{ .local = node_id };
    }

    fn buildNode(
        self: *Resolver,
        build_state: *BuildState,
        node: layout.GraphNode,
    ) Error!GraphRef {
        const node_id = try build_state.graph.reserveNode(self.allocator);
        build_state.graph.setNode(node_id, node);
        return .{ .local = node_id };
    }

    fn appendRecordRowFields(
        self: *Resolver,
        artifact: *const CheckedArtifact.CheckedModuleArtifact,
        fields: *std.ArrayList(CheckedArtifact.CheckedRecordField),
        root: CheckedArtifact.CheckedTypeId,
    ) Error!void {
        var current: ?CheckedArtifact.CheckedTypeId = root;
        var seen = std.AutoHashMap(CheckedArtifact.CheckedTypeId, void).init(self.allocator);
        defer seen.deinit();

        while (current) |current_id| {
            if (seen.contains(current_id)) break;
            try seen.put(current_id, {});

            switch (checkedTypePayload(artifact, current_id)) {
                .alias => |alias| current = alias.backing,
                .empty_record => break,
                .record => |record| {
                    try fields.appendSlice(self.allocator, record.fields);
                    current = record.ext;
                },
                .record_unbound => |tail_fields| {
                    try fields.appendSlice(self.allocator, tail_fields);
                    break;
                },
                .flex, .rigid => |variable| {
                    if (variable.row_default == .empty_record) break;
                    return error.UnresolvedByValue;
                },
                else => return error.UnresolvedByValue,
            }
        }
    }

    fn appendTagRowTags(
        self: *Resolver,
        artifact: *const CheckedArtifact.CheckedModuleArtifact,
        tags: *std.ArrayList(CheckedArtifact.CheckedTag),
        root: CheckedArtifact.CheckedTypeId,
    ) Error!void {
        var current: ?CheckedArtifact.CheckedTypeId = root;
        var seen = std.AutoHashMap(CheckedArtifact.CheckedTypeId, void).init(self.allocator);
        defer seen.deinit();

        while (current) |current_id| {
            if (seen.contains(current_id)) break;
            try seen.put(current_id, {});

            switch (checkedTypePayload(artifact, current_id)) {
                .alias => |alias| current = alias.backing,
                .empty_tag_union => break,
                .tag_union => |tag_union| {
                    try tags.appendSlice(self.allocator, tag_union.tags);
                    current = tag_union.ext;
                },
                .flex, .rigid => |variable| {
                    if (variable.row_default == .empty_tag_union) break;
                    return error.UnresolvedByValue;
                },
                else => return error.UnresolvedByValue,
            }
        }
    }

    fn nominalDeclarationFor(
        self: *Resolver,
        artifact: *const CheckedArtifact.CheckedModuleArtifact,
        nominal: CheckedArtifact.CheckedNominalType,
    ) ?NominalDeclarationLookup {
        return switch (nominal.representation) {
            .local_declaration => |declaration_id| .{
                .artifact = artifact,
                .declaration = artifact.checked_types.nominalDeclarationById(declaration_id),
            },
            .imported_declaration => |imported| blk: {
                const owner = self.artifactByKey(CheckedArtifact.importedNominalDeclarationModuleId(imported)) orelse return null;
                break :blk .{
                    .artifact = owner,
                    .declaration = owner.checked_types.nominalDeclarationById(imported.declaration),
                };
            },
            .local_box_payload_capability => |capability_ref| blk: {
                const capability = artifact.interface_capabilities.boxPayloadCapability(capability_ref.capability);
                break :blk .{
                    .artifact = artifact,
                    .declaration = artifact.checked_types.nominalDeclaration(capability.nominal) orelse unreachable,
                };
            },
            .imported_box_payload_capability => |capability_ref| blk: {
                const owner = self.artifactByKey(CheckedArtifact.importedBoxPayloadCapabilityModuleId(capability_ref)) orelse return null;
                const capability = owner.interface_capabilities.boxPayloadCapability(capability_ref.capability);
                break :blk .{
                    .artifact = owner,
                    .declaration = owner.checked_types.nominalDeclaration(capability.nominal) orelse unreachable,
                };
            },
            .builtin,
            .opaque_without_backing,
            => null,
        };
    }

    fn artifactByKey(
        self: *Resolver,
        key: CheckedArtifact.CheckedModuleArtifactKey,
    ) ?*const CheckedArtifact.CheckedModuleArtifact {
        return self.artifacts_by_key.get(key);
    }

    fn checkedTypePayload(
        artifact: *const CheckedArtifact.CheckedModuleArtifact,
        checked_type: CheckedArtifact.CheckedTypeId,
    ) CheckedArtifact.CheckedTypePayload {
        return artifact.checked_types.payload(checked_type);
    }

    fn sortRecordFieldsByName(
        artifact: *const CheckedArtifact.CheckedModuleArtifact,
        fields: []CheckedArtifact.CheckedRecordField,
    ) void {
        std.mem.sort(CheckedArtifact.CheckedRecordField, fields, &artifact.canonical_names, recordFieldLessThan);
    }

    fn sortTagsByName(
        artifact: *const CheckedArtifact.CheckedModuleArtifact,
        tags: []CheckedArtifact.CheckedTag,
    ) void {
        std.mem.sort(CheckedArtifact.CheckedTag, tags, &artifact.canonical_names, tagLessThan);
    }

    fn backingFieldByName(
        artifact: *const CheckedArtifact.CheckedModuleArtifact,
        fields: []const CheckedArtifact.CheckedRecordField,
        name: []const u8,
    ) ?struct { index: u16, field: CheckedArtifact.CheckedRecordField } {
        for (fields, 0..) |field, index| {
            const field_name = artifact.canonical_names.recordFieldLabelText(field.name);
            if (std.mem.eql(u8, field_name, name)) {
                return .{ .index = @intCast(index), .field = field };
            }
        }
        return null;
    }
};

fn hasPaddingField(fields: []const CheckedArtifact.CheckedNominalRecordField) bool {
    for (fields) |field| {
        if (field == .padding) return true;
    }
    return false;
}

fn recordFieldLessThan(names: *const CanonicalNameStore, lhs: CheckedArtifact.CheckedRecordField, rhs: CheckedArtifact.CheckedRecordField) bool {
    return std.mem.order(u8, names.recordFieldLabelText(lhs.name), names.recordFieldLabelText(rhs.name)) == .lt;
}

fn tagLessThan(names: *const CanonicalNameStore, lhs: CheckedArtifact.CheckedTag, rhs: CheckedArtifact.CheckedTag) bool {
    return std.mem.order(u8, names.tagLabelText(lhs.name), names.tagLabelText(rhs.name)) == .lt;
}
