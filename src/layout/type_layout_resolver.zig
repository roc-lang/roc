//! Resolves type-checker vars into canonical ordinary-data layouts through the shared layout store.

const std = @import("std");
const base = @import("base");
const can = @import("can");
const types = @import("types");

const graph_mod = @import("graph.zig");
const layout_mod = @import("layout.zig");
const store_mod = @import("store.zig");
const work_mod = @import("work.zig");

const ModuleEnv = can.ModuleEnv;
const types_store = types.store;
const Ident = base.Ident;
const Var = types.Var;
const TypeScope = types.TypeScope;
const StaticDispatchConstraint = types.StaticDispatchConstraint;
const Idx = layout_mod.Idx;
const Store = store_mod.Store;
const LayoutGraph = graph_mod.Graph;
const GraphField = graph_mod.Field;
const GraphNode = graph_mod.Node;
const GraphRef = graph_mod.Ref;
const ModuleVarKey = work_mod.ModuleVarKey;

const ParentContext = enum {
    ordinary,
    heap_container,
};

const BuildState = struct {
    graph: LayoutGraph = .{},
    refs_by_var: std.AutoHashMap(ModuleVarKey, GraphRef),
    depends_on_unresolved_type_params: bool = false,

    fn init(allocator: std.mem.Allocator) BuildState {
        return .{
            .refs_by_var = std.AutoHashMap(ModuleVarKey, GraphRef).init(allocator),
        };
    }

    fn deinit(self: *BuildState, allocator: std.mem.Allocator) void {
        self.graph.deinit(allocator);
        self.refs_by_var.deinit();
    }
};

const ResolvedInput = struct {
    module_idx: u32,
    resolved: types_store.ResolvedVarDesc,
};

/// Resolves type vars into canonical layout ids through the shared graph interner/store.
///
/// Unlike the MIR monotype resolver, this one must preserve type-side concerns such as:
/// - `type_scope` substitutions
/// - caller-vs-target module ownership for polymorphic substitutions
/// - builtin nominal recognition (`Str`, `List`, `Box`, numeric builtins)
/// - recursive nominal cycle handling before MIR erases those distinctions
pub const Resolver = struct {
    store: *Store,
    allocator: std.mem.Allocator,
    all_module_envs: []const *const ModuleEnv,
    override_types_store: ?*const types_store.Store = null,
    builtin_str_ident: ?Ident.Idx,
    canonical_cache: std.AutoHashMap(ModuleVarKey, Idx),

    pub fn init(store: *Store) Resolver {
        return .{
            .store = store,
            .allocator = store.allocator,
            .all_module_envs = store.moduleEnvs(),
            .override_types_store = null,
            .builtin_str_ident = store.builtin_str_ident,
            .canonical_cache = std.AutoHashMap(ModuleVarKey, Idx).init(store.allocator),
        };
    }

    pub fn deinit(self: *Resolver) void {
        self.canonical_cache.deinit();
    }

    pub fn setOverrideTypesStore(self: *Resolver, override: *const types_store.Store) void {
        self.override_types_store = override;
        self.canonical_cache.clearRetainingCapacity();
    }

    pub fn resetModuleCache(self: *Resolver, new_module_envs: []const *const ModuleEnv) void {
        self.all_module_envs = new_module_envs;
        self.canonical_cache.clearRetainingCapacity();
    }

    pub fn resolve(
        self: *Resolver,
        module_idx: u32,
        unresolved_var: Var,
        type_scope: *const TypeScope,
        caller_module_idx: ?u32,
    ) std.mem.Allocator.Error!Idx {
        const initial_types = self.getTypesStore(module_idx);
        const initial_resolved = initial_types.resolveVar(unresolved_var);
        const initial_key = ModuleVarKey{ .module_idx = module_idx, .var_ = initial_resolved.var_ };
        if (self.canonical_cache.get(initial_key)) |cached| return cached;

        var build_state = BuildState.init(self.allocator);
        defer build_state.deinit(self.allocator);

        const root = try self.buildRefForVar(
            module_idx,
            unresolved_var,
            type_scope,
            caller_module_idx,
            .ordinary,
            &build_state,
        );
        const layout_idx = try self.store.internGraph(&build_state.graph, root);

        if (!build_state.depends_on_unresolved_type_params) {
            try self.canonical_cache.put(initial_key, layout_idx);
        }

        return layout_idx;
    }

    fn buildRefForVar(
        self: *Resolver,
        module_idx: u32,
        unresolved_var: Var,
        type_scope: *const TypeScope,
        caller_module_idx: ?u32,
        parent_context: ParentContext,
        build_state: *BuildState,
    ) std.mem.Allocator.Error!GraphRef {
        const input = self.resolveInput(module_idx, unresolved_var, type_scope, caller_module_idx);
        const current_module_idx = input.module_idx;
        const current = input.resolved;
        const cache_key = ModuleVarKey{ .module_idx = current_module_idx, .var_ = current.var_ };

        if (build_state.refs_by_var.get(cache_key)) |cached| return cached;
        if (self.canonical_cache.get(cache_key)) |cached| return .{ .canonical = cached };

        switch (current.desc.content) {
            .flex => |flex| return self.resolveUnboundFlex(current_module_idx, flex.constraints, parent_context, build_state),
            .rigid => |rigid| return self.resolveUnboundRigid(current_module_idx, rigid.constraints, parent_context, build_state),
            .alias => unreachable,
            .err => return .{ .canonical = .zst },
            .structure => |flat_type| {
                const resolved_ref = switch (flat_type) {
                    .empty_record, .empty_tag_union => GraphRef{ .canonical = .zst },
                    .record => |record_type| try self.buildRecordRef(
                        current_module_idx,
                        record_type,
                        type_scope,
                        caller_module_idx,
                        build_state,
                    ),
                    .record_unbound => |fields_range| try self.buildRecordUnboundRef(
                        current_module_idx,
                        fields_range,
                        type_scope,
                        caller_module_idx,
                        build_state,
                    ),
                    .tuple => |tuple_type| try self.buildTupleRef(
                        current_module_idx,
                        tuple_type,
                        type_scope,
                        caller_module_idx,
                        build_state,
                    ),
                    .tag_union => |tag_union_type| try self.buildTagUnionRef(
                        current_module_idx,
                        tag_union_type,
                        type_scope,
                        caller_module_idx,
                        build_state,
                    ),
                    .fn_pure, .fn_effectful, .fn_unbound => try self.buildClosureRef(build_state),
                    .nominal_type => |nominal_type| try self.buildNominalRef(
                        current_module_idx,
                        current.var_,
                        nominal_type,
                        type_scope,
                        caller_module_idx,
                        build_state,
                    ),
                };

                try build_state.refs_by_var.put(cache_key, resolved_ref);
                return resolved_ref;
            },
        }
    }

    fn buildClosureRef(
        self: *Resolver,
        build_state: *BuildState,
    ) std.mem.Allocator.Error!GraphRef {
        const empty_captures_idx = try self.store.getEmptyRecordLayout();
        return try self.buildNode(build_state, .{ .closure = .{ .canonical = empty_captures_idx } });
    }

    fn buildNominalRef(
        self: *Resolver,
        module_idx: u32,
        nominal_var: Var,
        nominal_type: types.NominalType,
        type_scope: *const TypeScope,
        caller_module_idx: ?u32,
        build_state: *BuildState,
    ) std.mem.Allocator.Error!GraphRef {
        if (self.isBuiltinStr(module_idx, nominal_type)) {
            return .{ .canonical = .str };
        }
        if (self.builtinNumericLayout(module_idx, nominal_type)) |layout_idx| {
            return .{ .canonical = layout_idx };
        }
        if (self.isBuiltinBox(module_idx, nominal_type)) {
            const type_args = self.getTypesStore(module_idx).sliceNominalArgs(nominal_type);
            std.debug.assert(type_args.len == 1);
            const child_ref = try self.buildRefForVar(
                module_idx,
                type_args[0],
                type_scope,
                caller_module_idx,
                .heap_container,
                build_state,
            );
            return try self.buildNode(build_state, .{ .box = child_ref });
        }
        if (self.isBuiltinList(module_idx, nominal_type)) {
            const type_args = self.getTypesStore(module_idx).sliceNominalArgs(nominal_type);
            std.debug.assert(type_args.len == 1);
            const child_ref = try self.buildRefForVar(
                module_idx,
                type_args[0],
                type_scope,
                caller_module_idx,
                .heap_container,
                build_state,
            );
            return try self.buildNode(build_state, .{ .list = child_ref });
        }

        const cache_key = ModuleVarKey{ .module_idx = module_idx, .var_ = nominal_var };
        if (build_state.refs_by_var.get(cache_key)) |cached| return cached;
        if (self.canonical_cache.get(cache_key)) |cached| return .{ .canonical = cached };
        if (self.findEquivalentNominalRef(module_idx, nominal_type, build_state)) |cached| return cached;
        if (self.findEquivalentNominalLayout(module_idx, nominal_type)) |cached| return .{ .canonical = cached };

        const placeholder_id = try build_state.graph.reserveNode(self.allocator);
        const placeholder_ref = GraphRef{ .local = placeholder_id };
        try build_state.refs_by_var.put(cache_key, placeholder_ref);

        const backing_var = self.getTypesStore(module_idx).getNominalBackingVar(nominal_type);
        const backing_ref = try self.buildRefForVar(
            module_idx,
            backing_var,
            type_scope,
            caller_module_idx,
            .ordinary,
            build_state,
        );

        switch (backing_ref) {
            .canonical => {
                try build_state.refs_by_var.put(cache_key, backing_ref);
                return backing_ref;
            },
            .local => |backing_node_id| {
                if (backing_node_id == placeholder_id) unreachable;
                build_state.graph.setNode(placeholder_id, build_state.graph.getNode(backing_node_id));
                return placeholder_ref;
            },
        }
    }

    fn buildRecordRef(
        self: *Resolver,
        module_idx: u32,
        record_type: types.Record,
        type_scope: *const TypeScope,
        caller_module_idx: ?u32,
        build_state: *BuildState,
    ) std.mem.Allocator.Error!GraphRef {
        var collected = std.ArrayList(types.RecordField).empty;
        defer collected.deinit(self.allocator);

        try self.collectRecordFields(module_idx, record_type, &collected);
        if (collected.items.len == 0) return .{ .canonical = .zst };

        const ident_store = self.envFor(module_idx).getIdentStoreConst();
        std.mem.sort(types.RecordField, collected.items, ident_store, types.RecordField.sortByNameAsc);

        var fields = std.ArrayList(GraphField).empty;
        defer fields.deinit(self.allocator);
        try fields.ensureTotalCapacity(self.allocator, collected.items.len);

        for (collected.items, 0..) |field, index| {
            fields.appendAssumeCapacity(.{
                .index = @intCast(index),
                .child = try self.buildRefForVar(
                    module_idx,
                    field.var_,
                    type_scope,
                    caller_module_idx,
                    .ordinary,
                    build_state,
                ),
            });
        }

        return try self.buildStructNode(build_state, fields.items);
    }

    fn buildRecordUnboundRef(
        self: *Resolver,
        module_idx: u32,
        fields_range: types.RecordField.SafeMultiList.Range,
        type_scope: *const TypeScope,
        caller_module_idx: ?u32,
        build_state: *BuildState,
    ) std.mem.Allocator.Error!GraphRef {
        var collected = std.ArrayList(types.RecordField).empty;
        defer collected.deinit(self.allocator);

        const fields_slice = self.getTypesStore(module_idx).getRecordFieldsSlice(fields_range);
        for (fields_slice.items(.name), fields_slice.items(.var_)) |name, var_| {
            try collected.append(self.allocator, .{ .name = name, .var_ = var_ });
        }

        if (collected.items.len == 0) return .{ .canonical = .zst };

        const ident_store = self.envFor(module_idx).getIdentStoreConst();
        std.mem.sort(types.RecordField, collected.items, ident_store, types.RecordField.sortByNameAsc);

        var fields = std.ArrayList(GraphField).empty;
        defer fields.deinit(self.allocator);
        try fields.ensureTotalCapacity(self.allocator, collected.items.len);

        for (collected.items, 0..) |field, index| {
            fields.appendAssumeCapacity(.{
                .index = @intCast(index),
                .child = try self.buildRefForVar(
                    module_idx,
                    field.var_,
                    type_scope,
                    caller_module_idx,
                    .ordinary,
                    build_state,
                ),
            });
        }

        return try self.buildStructNode(build_state, fields.items);
    }

    fn buildTupleRef(
        self: *Resolver,
        module_idx: u32,
        tuple_type: types.Tuple,
        type_scope: *const TypeScope,
        caller_module_idx: ?u32,
        build_state: *BuildState,
    ) std.mem.Allocator.Error!GraphRef {
        const elems = self.getTypesStore(module_idx).sliceVars(tuple_type.elems);
        if (elems.len == 0) return .{ .canonical = .zst };

        var fields = std.ArrayList(GraphField).empty;
        defer fields.deinit(self.allocator);
        try fields.ensureTotalCapacity(self.allocator, elems.len);

        for (elems, 0..) |elem_var, index| {
            fields.appendAssumeCapacity(.{
                .index = @intCast(index),
                .child = try self.buildRefForVar(
                    module_idx,
                    elem_var,
                    type_scope,
                    caller_module_idx,
                    .ordinary,
                    build_state,
                ),
            });
        }

        return try self.buildStructNode(build_state, fields.items);
    }

    fn buildTagUnionRef(
        self: *Resolver,
        module_idx: u32,
        tag_union_type: types.TagUnion,
        type_scope: *const TypeScope,
        caller_module_idx: ?u32,
        build_state: *BuildState,
    ) std.mem.Allocator.Error!GraphRef {
        var tags = std.ArrayList(types.Tag).empty;
        defer tags.deinit(self.allocator);

        try self.collectTags(module_idx, tag_union_type, &tags);
        if (tags.items.len == 0) return .{ .canonical = .zst };

        // Pure enum (all tags have zero args): represent as just the discriminant scalar.
        if (tags.items.len > 1) {
            const ts = self.getTypesStore(module_idx);
            var all_no_payload = true;
            for (tags.items) |tag| {
                if (ts.sliceVars(tag.args).len > 0) {
                    all_no_payload = false;
                    break;
                }
            }
            if (all_no_payload) {
                return .{ .canonical = if (tags.items.len <= 256)
                    layout_mod.Idx.u8
                else if (tags.items.len <= 65536)
                    layout_mod.Idx.u16
                else
                    layout_mod.Idx.u32 };
            }
        }

        const ident_store = self.envFor(module_idx).getIdentStoreConst();
        std.mem.sort(types.Tag, tags.items, ident_store, types.Tag.sortByNameAsc);

        var variants = std.ArrayList(GraphRef).empty;
        defer variants.deinit(self.allocator);
        try variants.ensureTotalCapacity(self.allocator, tags.items.len);

        for (tags.items) |tag| {
            variants.appendAssumeCapacity(try self.buildPayloadRef(
                module_idx,
                self.getTypesStore(module_idx).sliceVars(tag.args),
                type_scope,
                caller_module_idx,
                build_state,
            ));
        }

        return try self.buildTagUnionNode(build_state, variants.items);
    }

    fn buildPayloadRef(
        self: *Resolver,
        module_idx: u32,
        payload_vars: []const Var,
        type_scope: *const TypeScope,
        caller_module_idx: ?u32,
        build_state: *BuildState,
    ) std.mem.Allocator.Error!GraphRef {
        if (payload_vars.len == 0) return .{ .canonical = .zst };
        var fields = std.ArrayList(GraphField).empty;
        defer fields.deinit(self.allocator);
        try fields.ensureTotalCapacity(self.allocator, payload_vars.len);

        for (payload_vars, 0..) |payload_var, index| {
            fields.appendAssumeCapacity(.{
                .index = @intCast(index),
                .child = try self.buildRefForVar(
                    module_idx,
                    payload_var,
                    type_scope,
                    caller_module_idx,
                    .ordinary,
                    build_state,
                ),
            });
        }

        return try self.buildStructNode(build_state, fields.items);
    }

    fn buildStructNode(
        self: *Resolver,
        build_state: *BuildState,
        fields: []const GraphField,
    ) std.mem.Allocator.Error!GraphRef {
        if (fields.len == 0) return .{ .canonical = .zst };

        const node_id = try build_state.graph.reserveNode(self.allocator);
        const span = try build_state.graph.appendFields(self.allocator, fields);
        build_state.graph.setNode(node_id, .{ .struct_ = span });
        return .{ .local = node_id };
    }

    fn buildTagUnionNode(
        self: *Resolver,
        build_state: *BuildState,
        variants: []const GraphRef,
    ) std.mem.Allocator.Error!GraphRef {
        const node_id = try build_state.graph.reserveNode(self.allocator);
        const span = try build_state.graph.appendRefs(self.allocator, variants);
        build_state.graph.setNode(node_id, .{ .tag_union = span });
        return .{ .local = node_id };
    }

    fn buildNode(
        self: *Resolver,
        build_state: *BuildState,
        node: GraphNode,
    ) std.mem.Allocator.Error!GraphRef {
        const node_id = try build_state.graph.reserveNode(self.allocator);
        build_state.graph.setNode(node_id, node);
        return .{ .local = node_id };
    }

    fn collectRecordFields(
        self: *Resolver,
        module_idx: u32,
        record_type: types.Record,
        out: *std.ArrayList(types.RecordField),
    ) std.mem.Allocator.Error!void {
        const ts = self.getTypesStore(module_idx);

        var current_row = record_type;
        rows: while (true) {
            const fields_slice = ts.getRecordFieldsSlice(current_row.fields);
            const names = fields_slice.items(.name);
            const vars = fields_slice.items(.var_);

            for (names, vars) |name, field_var| {
                if (recordFieldSeen(out.items, name)) continue;
                try out.append(self.allocator, .{ .name = name, .var_ = field_var });
            }

            var ext_var = current_row.ext;
            while (true) {
                const ext_resolved = ts.resolveVar(ext_var);
                switch (ext_resolved.desc.content) {
                    .alias => |alias| {
                        ext_var = ts.getAliasBackingVar(alias);
                        continue;
                    },
                    .structure => |ext_flat| switch (ext_flat) {
                        .record => |next_row| {
                            current_row = next_row;
                            continue :rows;
                        },
                        .record_unbound => |fields_range| {
                            const ext_fields = ts.getRecordFieldsSlice(fields_range);
                            const ext_names = ext_fields.items(.name);
                            const ext_vars = ext_fields.items(.var_);
                            for (ext_names, ext_vars) |name, field_var| {
                                if (recordFieldSeen(out.items, name)) continue;
                                try out.append(self.allocator, .{ .name = name, .var_ = field_var });
                            }
                            break :rows;
                        },
                        .empty_record => break :rows,
                        else => unreachable,
                    },
                    .flex, .rigid => break :rows,
                    .err => unreachable,
                }
            }
        }
    }

    fn collectTags(
        self: *Resolver,
        module_idx: u32,
        tag_union_type: types.TagUnion,
        out: *std.ArrayList(types.Tag),
    ) std.mem.Allocator.Error!void {
        const ts = self.getTypesStore(module_idx);

        var current_row = tag_union_type;
        rows: while (true) {
            const tag_slice = ts.getTagsSlice(current_row.tags);
            const names = tag_slice.items(.name);
            const args = tag_slice.items(.args);

            for (names, args) |name, range| {
                try out.append(self.allocator, .{ .name = name, .args = range });
            }

            var ext_var = current_row.ext;
            while (true) {
                const ext_resolved = ts.resolveVar(ext_var);
                switch (ext_resolved.desc.content) {
                    .alias => |alias| {
                        ext_var = ts.getAliasBackingVar(alias);
                        continue;
                    },
                    .structure => |ext_flat| switch (ext_flat) {
                        .tag_union => |next_row| {
                            current_row = next_row;
                            continue :rows;
                        },
                        .empty_tag_union => break :rows,
                        else => unreachable,
                    },
                    .flex, .rigid => break :rows,
                    .err => unreachable,
                }
            }
        }
    }

    fn resolveUnboundFlex(
        self: *Resolver,
        module_idx: u32,
        constraints: StaticDispatchConstraint.SafeList.Range,
        parent_context: ParentContext,
        build_state: *BuildState,
    ) GraphRef {
        build_state.depends_on_unresolved_type_params = true;

        if (self.hasFromNumeralConstraint(module_idx, constraints)) {
            return .{ .canonical = Idx.default_num };
        }
        if (parent_context == .heap_container and !constraints.isEmpty()) {
            return .{ .canonical = Idx.default_num };
        }
        return .{ .canonical = .zst };
    }

    fn resolveUnboundRigid(
        self: *Resolver,
        module_idx: u32,
        constraints: StaticDispatchConstraint.SafeList.Range,
        parent_context: ParentContext,
        build_state: *BuildState,
    ) GraphRef {
        build_state.depends_on_unresolved_type_params = true;

        if (self.hasFromNumeralConstraint(module_idx, constraints)) {
            return .{ .canonical = Idx.default_num };
        }
        if (parent_context == .heap_container) {
            if (!constraints.isEmpty()) {
                return .{ .canonical = Idx.default_num };
            }
            return .{ .canonical = .zst };
        }
        if (constraints.isEmpty()) {
            return .{ .canonical = .zst };
        }

        unreachable;
    }

    fn resolveInput(
        self: *const Resolver,
        module_idx: u32,
        unresolved_var: Var,
        type_scope: *const TypeScope,
        caller_module_idx: ?u32,
    ) ResolvedInput {
        var current_module_idx = module_idx;
        var current = self.getTypesStore(current_module_idx).resolveVar(unresolved_var);

        while (true) {
            switch (current.desc.content) {
                .alias => |alias| {
                    current = self.getTypesStore(current_module_idx).resolveVar(
                        self.getTypesStore(current_module_idx).getAliasBackingVar(alias),
                    );
                    continue;
                },
                .flex, .rigid => {
                    if (caller_module_idx) |caller_mod| {
                        if (type_scope.lookup(current.var_)) |mapped_var| {
                            current_module_idx = caller_mod;
                            current = self.getTypesStore(current_module_idx).resolveVar(mapped_var);
                            continue;
                        }
                    }
                },
                else => {},
            }
            break;
        }

        return .{
            .module_idx = current_module_idx,
            .resolved = current,
        };
    }

    fn getTypesStore(self: *const Resolver, module_idx: u32) *const types_store.Store {
        if (self.override_types_store) |override| return override;
        return &self.all_module_envs[module_idx].types;
    }

    fn envFor(self: *const Resolver, module_idx: u32) *const ModuleEnv {
        return self.all_module_envs[module_idx];
    }

    fn hasFromNumeralConstraint(
        self: *const Resolver,
        module_idx: u32,
        constraints: StaticDispatchConstraint.SafeList.Range,
    ) bool {
        if (constraints.isEmpty()) return false;

        for (self.getTypesStore(module_idx).sliceStaticDispatchConstraints(constraints)) |constraint| {
            switch (constraint.origin) {
                .from_numeral, .desugared_binop, .desugared_unaryop => return true,
                .method_call, .where_clause => {},
            }
        }

        return false;
    }

    fn isBuiltinStr(self: *const Resolver, module_idx: u32, nominal_type: types.NominalType) bool {
        if (self.builtin_str_ident) |builtin_str| {
            if (nominal_type.ident.ident_idx.eql(builtin_str)) return true;
        }

        const env = self.envFor(module_idx);
        return nominal_type.origin_module.eql(env.idents.builtin_module) and
            nominal_type.ident.ident_idx.eql(env.idents.str);
    }

    fn isBuiltinList(self: *const Resolver, module_idx: u32, nominal_type: types.NominalType) bool {
        const env = self.envFor(module_idx);
        return nominal_type.origin_module.eql(env.idents.builtin_module) and
            nominal_type.ident.ident_idx.eql(env.idents.list);
    }

    fn isBuiltinBox(self: *const Resolver, module_idx: u32, nominal_type: types.NominalType) bool {
        const env = self.envFor(module_idx);
        return nominal_type.origin_module.eql(env.idents.builtin_module) and
            nominal_type.ident.ident_idx.eql(env.idents.box);
    }

    fn builtinNumericLayout(
        self: *const Resolver,
        module_idx: u32,
        nominal_type: types.NominalType,
    ) ?Idx {
        const env = self.envFor(module_idx);
        if (!nominal_type.origin_module.eql(env.idents.builtin_module)) return null;

        const ident_idx = nominal_type.ident.ident_idx;
        if (ident_idx.eql(env.idents.u8_type)) return .u8;
        if (ident_idx.eql(env.idents.i8_type)) return .i8;
        if (ident_idx.eql(env.idents.u16_type)) return .u16;
        if (ident_idx.eql(env.idents.i16_type)) return .i16;
        if (ident_idx.eql(env.idents.u32_type)) return .u32;
        if (ident_idx.eql(env.idents.i32_type)) return .i32;
        if (ident_idx.eql(env.idents.u64_type)) return .u64;
        if (ident_idx.eql(env.idents.i64_type)) return .i64;
        if (ident_idx.eql(env.idents.u128_type)) return .u128;
        if (ident_idx.eql(env.idents.i128_type)) return .i128;
        if (ident_idx.eql(env.idents.f32_type)) return .f32;
        if (ident_idx.eql(env.idents.f64_type)) return .f64;
        if (ident_idx.eql(env.idents.dec_type)) return .dec;
        return null;
    }

    fn findEquivalentNominalRef(
        self: *const Resolver,
        module_idx: u32,
        nominal_type: types.NominalType,
        build_state: *const BuildState,
    ) ?GraphRef {
        var iter = build_state.refs_by_var.iterator();
        while (iter.next()) |entry| {
            if (entry.key_ptr.module_idx != module_idx) continue;
            if (self.nominalVarMatches(module_idx, nominal_type, entry.key_ptr.var_)) {
                return entry.value_ptr.*;
            }
        }
        return null;
    }

    fn findEquivalentNominalLayout(
        self: *const Resolver,
        module_idx: u32,
        nominal_type: types.NominalType,
    ) ?Idx {
        var iter = self.canonical_cache.iterator();
        while (iter.next()) |entry| {
            if (entry.key_ptr.module_idx != module_idx) continue;
            if (self.nominalVarMatches(module_idx, nominal_type, entry.key_ptr.var_)) {
                return entry.value_ptr.*;
            }
        }
        return null;
    }

    fn nominalVarMatches(
        self: *const Resolver,
        module_idx: u32,
        nominal_type: types.NominalType,
        other_var: Var,
    ) bool {
        const ts = self.getTypesStore(module_idx);
        const resolved = ts.resolveVar(other_var);
        if (resolved.desc.content != .structure) return false;
        const other_flat = resolved.desc.content.structure;
        if (other_flat != .nominal_type) return false;
        const other_nominal = other_flat.nominal_type;

        if (!nominal_type.origin_module.eql(other_nominal.origin_module)) return false;
        if (!nominal_type.ident.ident_idx.eql(other_nominal.ident.ident_idx)) return false;

        const lhs_args = ts.sliceNominalArgs(nominal_type);
        const rhs_args = ts.sliceNominalArgs(other_nominal);
        if (lhs_args.len != rhs_args.len) return false;

        for (lhs_args, rhs_args) |lhs_arg, rhs_arg| {
            const lhs_resolved = ts.resolveVar(lhs_arg);
            const rhs_resolved = ts.resolveVar(rhs_arg);
            if (lhs_resolved.var_ != rhs_resolved.var_) return false;
        }

        return true;
    }
};

fn recordFieldSeen(fields: []const types.RecordField, name: Ident.Idx) bool {
    for (fields) |field| {
        if (field.name.eql(name)) return true;
    }
    return false;
}
