//! Checked modules to Monotype IR.

const std = @import("std");
const check = @import("check");
const can = @import("can");
const builtins = @import("builtins");
const base = @import("base");

const Common = @import("../common.zig");
const Ast = @import("ast.zig");
const Type = @import("type.zig");
const solve = @import("solve.zig");

const InstGraph = solve.InstGraph;
const NodeId = solve.NodeId;
const InstTag = solve.InstTag;
const InstField = solve.InstField;
const InstBacking = solve.InstBacking;
const tagLessThan = solve.tagLessThan;
const recordFieldLessThan = solve.recordFieldLessThan;
const assertNoDuplicateTags = solve.assertNoDuplicateTags;
const assertNoDuplicateRecordFields = solve.assertNoDuplicateRecordFields;

const Allocator = std.mem.Allocator;
const checked = check.CheckedModule;
const names = check.CheckedNames;
const static_dispatch = check.StaticDispatchRegistry;
const Ident = base.Ident;

/// Options used while lowering checked modules into Monotype IR.
pub const Options = struct {
    /// Preserve source-level procedure names for consumers that present runtime
    /// diagnostics from lowered code.
    proc_debug_names: bool = false,
};

/// Lower checked modules and explicit roots into Monotype IR.
pub fn run(
    allocator: Allocator,
    modules: Common.CheckedModules,
    roots: Common.RootRequests,
    options: Options,
) Common.LowerError!Ast.Program {
    if (roots.requests.len == 0 and roots.layout_requests.len == 0 and roots.static_data_requests.len == 0) {
        Common.invariant("Monotype lowering requires explicit roots, layout requests, or static data requests");
    }

    var program = Ast.Program.init(allocator);
    errdefer program.deinit();

    var builder = Builder.init(allocator, modules, &program, options);
    defer builder.deinit();
    try builder.initHostedCatalog();
    try builder.initMethodLookupIndex();

    for (roots.requests) |request| {
        try builder.lowerRoot(request);
    }
    for (roots.layout_requests) |checked_ty| {
        try builder.lowerLayoutRequest(checked_ty);
    }
    for (roots.static_data_requests) |request| {
        try builder.lowerStaticDataRequest(request);
    }

    program.next_symbol = builder.symbols.next;
    return program;
}

const ModuleView = struct {
    key: checked.ModuleId,
    module_env: *const can.ModuleEnv,
    module_identity: checked.ModuleIdentity,
    names: *const names.NameStore,
    types: checked.CheckedTypeStoreView,
    bodies: checked.CheckedBodyStoreView,
    exhaustiveness_sites: *const checked.CheckedExhaustivenessSiteTable,
    checked_const_bodies: *const checked.CheckedConstBodyTable,
    templates: *const checked.CheckedProcedureTemplateTable,
    compile_time_roots: *const checked.CompileTimeRootTable,
    entry_wrappers: *const checked.EntryWrapperTable,
    intrinsic_wrappers: *const checked.IntrinsicWrapperTable,
    hosted_procs: *const checked.HostedProcTable,
    static_dispatch_plans: *const static_dispatch.StaticDispatchPlanTable,
    method_registry: *const static_dispatch.MethodRegistry,
    resolved_refs: *const checked.ResolvedValueTable,
    nested_proc_sites: *const checked.NestedProcSiteTable,
    exported_procedure_bindings: checked.ExportedProcedureBindingView,
    exported_const_templates: checked.ExportedConstTemplateView,
    top_level_procedure_bindings: *const checked.TopLevelProcedureBindingTable,
    platform_required_bindings: *const checked.PlatformRequiredBindingTable,
    callable_eval_templates: checked.CallableEvalTemplateTableView,
    hoisted_constants: *const checked.HoistedConstTable,
    const_templates: *const checked.ConstTemplateTable,
    const_store: *const checked.ConstStore,
    interface_capabilities: *const checked.ModuleInterfaceCapabilities,
};

const ConstNode = struct {
    module: ModuleView,
    id: checked.ConstNodeId,
};

const MethodLookup = struct {
    view: ModuleView,
    target: static_dispatch.MethodTarget,
};

const MethodDispatch = struct {
    owner: static_dispatch.MethodOwner,
    method: names.MethodNameId,
};

const MethodLookupIndexEntry = struct {
    dispatch: MethodDispatch,
    view: ModuleView,
    target: static_dispatch.MethodTarget,

    fn lessThan(_: void, lhs: MethodLookupIndexEntry, rhs: MethodLookupIndexEntry) bool {
        return methodDispatchOrder(lhs.dispatch, rhs.dispatch) == .lt;
    }
};

fn assertMethodLookupIndexUnique(entries: []const MethodLookupIndexEntry) void {
    if (entries.len < 2) return;
    var index: usize = 1;
    while (index < entries.len) : (index += 1) {
        if (methodDispatchOrder(entries[index - 1].dispatch, entries[index].dispatch) != .eq) continue;
        Common.invariant("checked method registries contain duplicate dispatch targets");
    }
}

fn methodDispatchOrder(a: MethodDispatch, b: MethodDispatch) std.math.Order {
    const owner_order = methodOwnerOrder(a.owner, b.owner);
    if (owner_order != .eq) return owner_order;
    return orderEnum(names.MethodNameId, a.method, b.method);
}

fn methodOwnerOrder(a: static_dispatch.MethodOwner, b: static_dispatch.MethodOwner) std.math.Order {
    const a_tag = methodOwnerTagRank(a);
    const b_tag = methodOwnerTagRank(b);
    if (a_tag != b_tag) return orderU32(a_tag, b_tag);

    return switch (a) {
        .nominal => |a_nominal| switch (b) {
            .nominal => |b_nominal| blk: {
                const module_order = orderEnum(names.ModuleNameId, a_nominal.module_name, b_nominal.module_name);
                if (module_order != .eq) break :blk module_order;
                const type_order = orderEnum(names.TypeNameId, a_nominal.type_name, b_nominal.type_name);
                if (type_order != .eq) break :blk type_order;
                break :blk orderOptionalU32(a_nominal.source_decl, b_nominal.source_decl);
            },
            else => unreachable,
        },
        .source_decl => |a_decl| switch (b) {
            .source_decl => |b_decl| blk: {
                const module_order = orderEnum(names.ModuleNameId, a_decl.module_name, b_decl.module_name);
                if (module_order != .eq) break :blk module_order;
                break :blk orderU32(a_decl.statement, b_decl.statement);
            },
            else => unreachable,
        },
        .builtin => |a_builtin| switch (b) {
            .builtin => |b_builtin| orderEnum(static_dispatch.BuiltinOwner, a_builtin, b_builtin),
            else => unreachable,
        },
    };
}

fn methodOwnerTagRank(owner: static_dispatch.MethodOwner) u32 {
    return switch (owner) {
        .nominal => 0,
        .source_decl => 1,
        .builtin => 2,
    };
}

fn orderOptionalU32(a: ?u32, b: ?u32) std.math.Order {
    if (a) |a_value| {
        return if (b) |b_value| orderU32(a_value, b_value) else .gt;
    }
    return if (b == null) .eq else .lt;
}

fn orderEnum(comptime T: type, a: T, b: T) std.math.Order {
    return orderU32(@intFromEnum(a), @intFromEnum(b));
}

fn orderU32(a: u32, b: u32) std.math.Order {
    if (a < b) return .lt;
    if (a > b) return .gt;
    return .eq;
}

const FunctionShape = struct {
    args: Type.Span,
    ret: Type.TypeId,
};

const HostedCatalogEntry = struct {
    template: names.ProcTemplate,
    external_symbol_name: names.ExternalSymbolNameId,
    dispatch_index: u32,
    order: []const u8,
    def_idx: u32,
};

/// The platform header's hosted section, resolved to the same qualified keys
/// the checked modules' hosted tables sort by ("Module.func" with a trailing `!`
/// stripped). Section order defines hosted dispatch order, and each entry's
/// string is the hosted function's linker symbol.
const HostedSectionMap = struct {
    keys: []const []const u8,
    symbols: []const []const u8,

    fn deinit(self: HostedSectionMap, allocator: Allocator) void {
        for (self.keys) |key| allocator.free(key);
        allocator.free(self.keys);
        allocator.free(self.symbols);
    }
};

const BinderMap = std.AutoHashMap(checked.PatternBinderId, Ast.LocalId);

const LoweredTemplateStatus = enum {
    reserved,
    lowering,
    ready,
};

/// A procedure template specialization together with both the Monotype function
/// type requested by the call site that reserved it and the solved function type
/// produced after lowering the body. The request type remains a valid cache key
/// after the body solves previously-unresolved slots; recursive requests made
/// from another graph can still arrive with the same request shape.
const LoweredTemplate = struct {
    def: Ast.DefId,
    request_fn_ty: Type.TypeId,
    request_digest: names.TypeDigest,
    solved_fn_ty: Type.TypeId,
    solved_digest: names.TypeDigest,
    status: LoweredTemplateStatus,
};

const LoweredNestedStatus = enum {
    lowering,
    ready,
};

/// A nested function specialization together with the Monotype function type
/// its body is being lowered or was lowered at.
const LoweredNestedFn = struct {
    fn_id: Ast.FnId,
    fn_ty: Type.TypeId,
    status: LoweredNestedStatus,
};

const ReservedTemplate = struct {
    fn_id: Ast.FnId,
    needs_lowering: bool,
};

/// A nested function body together with the instantiation context that
/// snapshots the lexical binders at its site.
const NestedFnRequest = struct {
    ctx: *BodyContext,
    expr_id: checked.CheckedExprId,
    fn_template: Ast.FnTemplate,
};

const LambdaArgLet = struct {
    pat: Ast.PatId,
    value: Ast.ExprId,
};

const MergeBinder = struct {
    binder: checked.PatternBinderId,
    before: Ast.LocalId,
    ty: Type.TypeId,
};

const LoweredLambdaArgs = struct {
    args: Ast.Span(Ast.TypedLocal),
    body: Ast.ExprId,
};

const LoweredCall = struct {
    ret_ty: Type.TypeId,
    data: Ast.ExprData,
};

const ParseIntrinsic = enum {
    tag_union_parse,
    fields_rename_fields,
    fields_shortest_name,
    fields_longest_name,
    fields_iter,
    fields_for_size,
    field_name,
};

const FieldNameBound = enum {
    shortest,
    longest,
};

const FieldNamesIterMode = enum {
    all,
    for_size,
};

const ConstExprAddress = struct {
    store_module_bytes: [32]u8,
    type_module_bytes: [32]u8,
    node: u32,
    mono_ty: u32,
};

/// Key for a memoized structural-derivation helper def. `value_ty` is the type
/// being derived over; `result_ty` is the derivation's auxiliary type (the
/// produced Str for inspect, the Bool for equality, the Hasher for hashing).
const GeneratedHelperDefAddress = struct {
    value_ty: u32,
    result_ty: u32,
};

/// Tracks a memoized structural-derivation helper def (is_eq / inspect /
/// to_hash). `reserved` means the def id is allocated but its body has not yet
/// been filled in (used to break recursion); `ready` means the body is complete.
const GeneratedHelperDefEntry = union(enum) {
    reserved: Ast.DefId,
    ready: Ast.DefId,

    fn id(self: GeneratedHelperDefEntry) Ast.DefId {
        return switch (self) {
            .reserved => |def_id| def_id,
            .ready => |def_id| def_id,
        };
    }
};

const Builder = struct {
    allocator: Allocator,
    modules: Common.CheckedModules,
    root_view: checked.ImportedModuleView,
    program: *Ast.Program,
    proc_debug_names: bool,
    symbols: Common.SymbolGen = .{},
    type_cache: std.AutoHashMap(CheckedTypeAddress, Type.TypeId),
    /// Monotypes owned by the builder-global type cache. They are lowered
    /// without body evidence, so empty tag unions inside them are unresolved
    /// slots rather than solved uninhabited types.
    unsolved_monos: std.AutoHashMap(Type.TypeId, void),
    lowered_templates: std.AutoHashMap(TemplateFamily, std.ArrayList(LoweredTemplate)),
    lowered_nested_fns: std.AutoHashMap(NestedFnFamily, std.ArrayList(LoweredNestedFn)),
    nested_site_cache: std.AutoHashMap(NestedSiteAddress, names.ProcSiteId),
    const_expr_cache: std.AutoHashMap(ConstExprAddress, Ast.ExprId),
    inspect_defs: std.AutoHashMap(GeneratedHelperDefAddress, GeneratedHelperDefEntry),
    equality_defs: std.AutoHashMap(GeneratedHelperDefAddress, GeneratedHelperDefEntry),
    hash_defs: std.AutoHashMap(GeneratedHelperDefAddress, GeneratedHelperDefEntry),
    hosted_catalog: []HostedCatalogEntry = &.{},
    method_lookup_index: []MethodLookupIndexEntry = &.{},
    u64_ty: ?Type.TypeId = null,
    bool_ty: ?Type.TypeId = null,
    source_file_ids: std.AutoHashMap(u32, u32),
    constrain_depth: usize = 0,
    /// The specialization graph currently being lowered. Template body
    /// requests made anywhere inside that specialization defer to its end,
    /// when its types are final and specialization keys are stable.
    active_graph: ?*InstGraph = null,

    fn init(allocator: Allocator, modules: Common.CheckedModules, program: *Ast.Program, options: Options) Builder {
        return .{
            .allocator = allocator,
            .modules = modules,
            .root_view = checked.importedView(modules.root.module),
            .program = program,
            .proc_debug_names = options.proc_debug_names,
            .type_cache = std.AutoHashMap(CheckedTypeAddress, Type.TypeId).init(allocator),
            .unsolved_monos = std.AutoHashMap(Type.TypeId, void).init(allocator),
            .lowered_templates = std.AutoHashMap(TemplateFamily, std.ArrayList(LoweredTemplate)).init(allocator),
            .lowered_nested_fns = std.AutoHashMap(NestedFnFamily, std.ArrayList(LoweredNestedFn)).init(allocator),
            .nested_site_cache = std.AutoHashMap(NestedSiteAddress, names.ProcSiteId).init(allocator),
            .const_expr_cache = std.AutoHashMap(ConstExprAddress, Ast.ExprId).init(allocator),
            .inspect_defs = std.AutoHashMap(GeneratedHelperDefAddress, GeneratedHelperDefEntry).init(allocator),
            .equality_defs = std.AutoHashMap(GeneratedHelperDefAddress, GeneratedHelperDefEntry).init(allocator),
            .hash_defs = std.AutoHashMap(GeneratedHelperDefAddress, GeneratedHelperDefEntry).init(allocator),
            .source_file_ids = std.AutoHashMap(u32, u32).init(allocator),
        };
    }

    /// Source file table index for a module's view, registering the module on
    /// first use.
    fn fileIdFor(self: *Builder, view: ModuleView) Allocator.Error!u32 {
        const gop = try self.source_file_ids.getOrPut(view.module_identity.module_idx);
        if (!gop.found_existing) {
            gop.value_ptr.* = try self.program.addSourceFile(view.module_env.module_name);
        }
        return gop.value_ptr.*;
    }

    fn deinit(self: *Builder) void {
        self.source_file_ids.deinit();
        self.allocator.free(self.method_lookup_index);
        self.allocator.free(self.hosted_catalog);
        self.hash_defs.deinit();
        self.equality_defs.deinit();
        self.inspect_defs.deinit();
        self.const_expr_cache.deinit();
        self.nested_site_cache.deinit();
        var nested_lists = self.lowered_nested_fns.valueIterator();
        while (nested_lists.next()) |list| {
            list.deinit(self.allocator);
        }
        self.lowered_nested_fns.deinit();
        var template_lists = self.lowered_templates.valueIterator();
        while (template_lists.next()) |list| {
            list.deinit(self.allocator);
        }
        self.lowered_templates.deinit();
        self.unsolved_monos.deinit();
        self.type_cache.deinit();
    }

    fn initHostedCatalog(self: *Builder) Allocator.Error!void {
        var entries = std.ArrayList(HostedCatalogEntry).empty;
        errdefer entries.deinit(self.allocator);

        try self.appendHostedCatalogFromView(&entries, moduleView(self.root_view));
        for (self.modules.imports, 0..) |imported, index| {
            if (self.importModuleAlreadyScanned(imported.key, index)) continue;
            try self.appendHostedCatalogFromView(&entries, moduleView(imported));
        }
        for (self.modules.root.relation_modules, 0..) |relation, index| {
            if (self.relationModuleAlreadyScanned(relation.key, index)) continue;
            try self.appendHostedCatalogFromView(&entries, moduleView(relation));
        }

        const SortContext = struct {
            pub fn lessThan(_: void, a: HostedCatalogEntry, b: HostedCatalogEntry) bool {
                return switch (std.mem.order(u8, a.order, b.order)) {
                    .lt => true,
                    .gt => false,
                    .eq => a.def_idx < b.def_idx,
                };
            }
        };
        std.mem.sort(HostedCatalogEntry, entries.items, {}, SortContext.lessThan);

        for (entries.items, 0..) |*entry, index| {
            entry.dispatch_index = @intCast(index);
        }

        if (try self.buildHostedSectionMap()) |map| {
            defer map.deinit(self.allocator);

            // Platform checking already verified the section is total over the
            // platform's hosted functions and duplicate-free, so every catalog
            // entry has exactly one section position.
            if (entries.items.len != map.keys.len) {
                Common.invariant("platform hosted section disagrees with the hosted catalog size");
            }
            for (entries.items) |*entry| {
                const pos = blk: {
                    for (map.keys, 0..) |key, key_index| {
                        if (std.mem.eql(u8, key, entry.order)) break :blk key_index;
                    }
                    Common.invariant("hosted function is missing from the platform hosted section");
                };
                entry.dispatch_index = @intCast(pos);
                entry.external_symbol_name = try self.program.names.internExternalSymbolName(map.symbols[pos]);
            }

            const DispatchSort = struct {
                pub fn lessThan(_: void, a: HostedCatalogEntry, b: HostedCatalogEntry) bool {
                    return a.dispatch_index < b.dispatch_index;
                }
            };
            std.mem.sort(HostedCatalogEntry, entries.items, {}, DispatchSort.lessThan);
        }

        self.hosted_catalog = try entries.toOwnedSlice(self.allocator);
    }

    /// Find the platform module's hosted section among the checked modules and
    /// resolve it to qualified keys + linker symbols. Returns null when no
    /// module declares hosted entries (e.g. a platform with no hosted section,
    /// or compiler-internal evaluation).
    fn buildHostedSectionMap(self: *Builder) Allocator.Error!?HostedSectionMap {
        const platform_env = blk: {
            const root_env = moduleView(self.root_view).module_env;
            if (root_env.hosted_entries.items.items.len != 0) break :blk root_env;
            for (self.modules.imports) |imported| {
                const env = moduleView(imported).module_env;
                if (env.hosted_entries.items.items.len != 0) break :blk env;
            }
            for (self.modules.root.relation_modules) |relation| {
                const env = moduleView(relation).module_env;
                if (env.hosted_entries.items.items.len != 0) break :blk env;
            }
            return null;
        };

        const section = platform_env.hosted_entries.items.items;
        var keys = try self.allocator.alloc([]const u8, section.len);
        var key_count: usize = 0;
        errdefer {
            for (keys[0..key_count]) |key| self.allocator.free(key);
            self.allocator.free(keys);
        }
        const symbols = try self.allocator.alloc([]const u8, section.len);
        errdefer self.allocator.free(symbols);

        for (section, 0..) |entry, index| {
            var func_text = platform_env.getIdentText(entry.func_ident);
            if (func_text.len > 0 and func_text[func_text.len - 1] == '!') {
                func_text = func_text[0 .. func_text.len - 1];
            }
            keys[index] = if (entry.module_ident) |module_ident|
                try std.fmt.allocPrint(self.allocator, "{s}.{s}", .{ platform_env.getIdentText(module_ident), func_text })
            else
                try self.allocator.dupe(u8, func_text);
            key_count = index + 1;
            symbols[index] = platform_env.getString(entry.symbol);
        }

        return .{ .keys = keys, .symbols = symbols };
    }

    fn initMethodLookupIndex(self: *Builder) Allocator.Error!void {
        var entries = std.ArrayList(MethodLookupIndexEntry).empty;
        errdefer entries.deinit(self.allocator);

        try self.appendMethodLookupIndexFromView(&entries, moduleView(self.root_view));
        for (self.modules.imports, 0..) |imported, index| {
            if (self.importModuleAlreadyScanned(imported.key, index)) continue;
            try self.appendMethodLookupIndexFromView(&entries, moduleView(imported));
        }
        for (self.modules.root.relation_modules, 0..) |relation, index| {
            if (self.relationModuleAlreadyScanned(relation.key, index)) continue;
            try self.appendMethodLookupIndexFromView(&entries, moduleView(relation));
        }

        std.mem.sort(MethodLookupIndexEntry, entries.items, {}, MethodLookupIndexEntry.lessThan);
        assertMethodLookupIndexUnique(entries.items);

        self.method_lookup_index = try entries.toOwnedSlice(self.allocator);
    }

    fn appendMethodLookupIndexFromView(self: *Builder, entries: *std.ArrayList(MethodLookupIndexEntry), view: ModuleView) Allocator.Error!void {
        for (view.method_registry.entries) |entry| {
            try entries.append(self.allocator, .{
                .dispatch = .{
                    .owner = try self.methodOwnerInProgramNames(view, entry.key.owner),
                    .method = try self.methodName(view, entry.key.method),
                },
                .view = view,
                .target = entry.target,
            });
        }
    }

    fn appendHostedCatalogFromView(self: *Builder, entries: *std.ArrayList(HostedCatalogEntry), view: ModuleView) Allocator.Error!void {
        for (view.hosted_procs.procs) |proc| {
            try entries.append(self.allocator, .{
                .template = proc.template,
                .external_symbol_name = try self.program.names.internExternalSymbolName(view.names.externalSymbolNameText(proc.external_symbol_name)),
                .dispatch_index = 0,
                .order = proc.orderKey(view.hosted_procs),
                .def_idx = @intFromEnum(proc.def_idx),
            });
        }
    }

    fn hostedFn(self: *Builder, template: names.ProcTemplate) Ast.HostedFn {
        for (self.hosted_catalog) |entry| {
            if (!names.procedureTemplateRefEql(entry.template, template)) continue;
            return .{
                .template = template,
                .external_symbol_name = entry.external_symbol_name,
                .dispatch_index = entry.dispatch_index,
            };
        }
        Common.invariant("hosted procedure template was not output in the hosted catalog");
    }

    fn moduleName(self: *Builder, view: ModuleView, id: names.ModuleNameId) Allocator.Error!names.ModuleNameId {
        return self.program.names.internModuleName(view.names.moduleNameText(id));
    }

    fn typeName(self: *Builder, view: ModuleView, id: names.TypeNameId) Allocator.Error!names.TypeNameId {
        return self.program.names.internTypeName(view.names.typeNameText(id));
    }

    fn methodName(self: *Builder, view: ModuleView, id: names.MethodNameId) Allocator.Error!names.MethodNameId {
        return self.program.names.internMethodName(view.names.methodNameText(id));
    }

    fn methodOwnerInProgramNames(self: *Builder, view: ModuleView, owner: static_dispatch.MethodOwner) Allocator.Error!static_dispatch.MethodOwner {
        return switch (owner) {
            .builtin => |builtin| .{ .builtin = builtin },
            .source_decl => |decl| .{ .source_decl = .{
                .module_name = try self.moduleName(view, decl.module_name),
                .statement = decl.statement,
            } },
            .nominal => |nominal| .{ .nominal = .{
                .module_name = try self.moduleName(view, nominal.module_name),
                .type_name = try self.typeName(view, nominal.type_name),
                .source_decl = nominal.source_decl,
            } },
        };
    }

    fn recordFieldName(self: *Builder, view: ModuleView, id: names.RecordFieldNameId) Allocator.Error!names.RecordFieldNameId {
        return self.program.names.internRecordFieldLabel(view.names.recordFieldLabelText(id));
    }

    fn tagName(self: *Builder, view: ModuleView, id: names.TagNameId) Allocator.Error!names.TagNameId {
        return self.program.names.internTagLabel(view.names.tagLabelText(id));
    }

    fn typeDef(
        self: *Builder,
        view: ModuleView,
        module_name: names.ModuleNameId,
        type_name: names.TypeNameId,
        source_decl: ?u32,
    ) Allocator.Error!Type.TypeDef {
        return .{
            .module_name = try self.moduleName(view, module_name),
            .type_name = try self.typeName(view, type_name),
            .source_decl = source_decl,
        };
    }

    fn declaredModuleForAlias(self: *Builder, view: ModuleView, alias: checked.CheckedAliasType) names.CheckedModuleDigest {
        return self.moduleDigestForOrigin(view, alias.origin_module);
    }

    fn declaredModuleForNominal(self: *Builder, view: ModuleView, nominal: checked.CheckedNominalType) names.CheckedModuleDigest {
        return switch (nominal.representation) {
            .imported_declaration => |imported| moduleDigestFromId(checked.importedNominalDeclarationModuleId(imported)),
            .imported_box_payload_capability => |imported| moduleDigestFromId(checked.importedBoxPayloadCapabilityModuleId(imported)),
            .builtin,
            .local_declaration,
            .local_box_payload_capability,
            .opaque_without_backing,
            => self.moduleDigestForOrigin(view, nominal.origin_module),
        };
    }

    fn moduleDigestForOrigin(self: *Builder, view: ModuleView, origin_module: names.ModuleNameId) names.CheckedModuleDigest {
        const origin_name = view.names.moduleNameText(origin_module);
        if (moduleViewNameMatches(view, origin_name)) return moduleDigestFromId(view.key);

        const root = moduleView(self.root_view);
        if (moduleViewNameMatches(root, origin_name)) return moduleDigestFromId(root.key);

        for (self.modules.imports) |imported| {
            const imported_view = moduleView(imported);
            if (moduleViewNameMatches(imported_view, origin_name)) return moduleDigestFromId(imported_view.key);
        }

        for (self.modules.root.relation_modules) |relation| {
            const relation_view = moduleView(relation);
            if (moduleViewNameMatches(relation_view, origin_name)) return moduleDigestFromId(relation_view.key);
        }

        Common.invariant("checked named type origin module was not available to Monotype lowering");
    }

    fn lowerRoot(self: *Builder, request: checked.RootRequest) Allocator.Error!void {
        const def = if (request.procedure_template) |template|
            try self.lowerTemplate(template, moduleView(self.root_view), request.checked_type)
        else if (request.procedure_binding) |binding|
            try self.lowerProcedureBindingRoot(request, binding)
        else if (request.procedure_use) |procedure|
            try self.lowerProcedureUseRoot(request, procedure)
        else
            Common.invariant("root request reached Monotype without a checked procedure template or procedure source");
        try self.appendRuntimeSchemaRequestsForDef(def);
        try self.program.roots.append(self.allocator, .{ .def = def, .request = request });
    }

    fn lowerLayoutRequest(self: *Builder, checked_ty: checked.CheckedTypeId) Allocator.Error!void {
        const ty = try self.lowerType(moduleView(self.root_view), checked_ty);
        try self.program.layout_requests.append(self.allocator, .{
            .checked_type = checked_ty,
            .ty = ty,
        });
        try self.appendRuntimeSchemaRequestsForType(ty);
    }

    fn lowerStaticDataRequest(self: *Builder, request: Common.StaticDataRequest) Allocator.Error!void {
        const type_view = moduleView(self.root_view);
        const ret_ty = try self.lowerType(type_view, request.data.checked_type);
        const const_node = self.providedConstNode(request.data);
        const body = try self.restoreConstNodeAtType(const_node.module, type_view, const_node.id, ret_ty);
        const def: Ast.DefId = @enumFromInt(@as(u32, @intCast(self.program.defs.items.len)));
        try self.program.defs.append(self.allocator, .{
            .symbol = self.symbols.fresh(),
            .fn_def = null,
            .args = Ast.Span(Ast.TypedLocal).empty(),
            .body = .{ .roc = body },
            .ret = ret_ty,
        });
        try self.program.layout_requests.append(self.allocator, .{
            .checked_type = request.data.checked_type,
            .ty = ret_ty,
            .def = def,
        });
        try self.appendRuntimeSchemaRequestsForType(ret_ty);
    }

    fn appendRuntimeSchemaRequestsForDef(self: *Builder, def: Ast.DefId) Allocator.Error!void {
        const fn_ = self.program.defs.items[@intFromEnum(def)];
        for (self.program.typedLocalSpan(fn_.args)) |arg| {
            try self.appendRuntimeSchemaRequestsForType(arg.ty);
        }
        try self.appendRuntimeSchemaRequestsForType(fn_.ret);
    }

    fn appendRuntimeSchemaRequestsForType(self: *Builder, ty: Type.TypeId) Allocator.Error!void {
        var active = std.AutoHashMap(Type.TypeId, void).init(self.allocator);
        defer active.deinit();
        try self.appendRuntimeSchemaRequestsForTypeInner(ty, &active);
    }

    fn appendRuntimeSchemaRequestsForTypeInner(
        self: *Builder,
        ty: Type.TypeId,
        active: *std.AutoHashMap(Type.TypeId, void),
    ) Allocator.Error!void {
        if (active.contains(ty)) return;
        try active.put(ty, {});

        switch (self.program.types.get(ty)) {
            .named => |named| {
                for (self.program.types.span(named.args)) |arg| {
                    try self.appendRuntimeSchemaRequestsForTypeInner(arg, active);
                }
                const backing = named.backing orelse return;
                if (backing.use == .inspectable and self.runtimeSchemaBackedByRecordOrTagUnion(backing.ty)) {
                    try self.appendRuntimeSchemaRequest(.{ .def = named.def, .ty = ty });
                }
                try self.appendRuntimeSchemaRequestsForTypeInner(backing.ty, active);
            },
            .record => |fields| {
                for (self.program.types.fieldSpan(fields)) |field| {
                    try self.appendRuntimeSchemaRequestsForTypeInner(field.ty, active);
                }
            },
            .tuple => |items| {
                for (self.program.types.span(items)) |item| {
                    try self.appendRuntimeSchemaRequestsForTypeInner(item, active);
                }
            },
            .tag_union => |tags| {
                for (self.program.types.tagSpan(tags)) |tag| {
                    for (self.program.types.span(tag.payloads)) |payload| {
                        try self.appendRuntimeSchemaRequestsForTypeInner(payload, active);
                    }
                }
            },
            .list,
            .box,
            => |elem| try self.appendRuntimeSchemaRequestsForTypeInner(elem, active),
            .func => |func| {
                for (self.program.types.span(func.args)) |arg| {
                    try self.appendRuntimeSchemaRequestsForTypeInner(arg, active);
                }
                try self.appendRuntimeSchemaRequestsForTypeInner(func.ret, active);
            },
            .primitive,
            .erased,
            .zst,
            => {},
        }
    }

    fn runtimeSchemaBackedByRecordOrTagUnion(self: *Builder, ty: Type.TypeId) bool {
        return switch (self.program.types.get(ty)) {
            .record,
            .tag_union,
            => true,
            .named => |named| if (named.backing) |backing|
                backing.use == .inspectable and self.runtimeSchemaBackedByRecordOrTagUnion(backing.ty)
            else
                false,
            else => false,
        };
    }

    fn appendRuntimeSchemaRequest(self: *Builder, request: Ast.RuntimeSchemaRequest) Allocator.Error!void {
        for (self.program.runtime_schema_requests.items) |existing| {
            if (existing.def.module_name == request.def.module_name and existing.def.type_name == request.def.type_name) {
                return;
            }
        }
        try self.program.runtime_schema_requests.append(self.allocator, request);
    }

    fn lowerProcedureUseRoot(
        self: *Builder,
        request: checked.RootRequest,
        procedure: checked.ProcedureUseTemplate,
    ) Allocator.Error!Ast.DefId {
        const view = moduleView(self.root_view);
        const fn_ty = try self.lowerType(view, request.checked_type);
        const fn_data = self.functionShape(fn_ty, "procedure use root had a non-function checked type");
        const arg_tys = self.program.types.span(fn_data.args);
        const args = try self.allocator.alloc(Ast.TypedLocal, arg_tys.len);
        defer self.allocator.free(args);
        const arg_exprs = try self.allocator.alloc(Ast.ExprId, arg_tys.len);
        defer self.allocator.free(arg_exprs);

        for (arg_tys, 0..) |arg_ty, i| {
            const local = try self.program.addLocal(self.symbols.fresh(), arg_ty);
            args[i] = .{ .local = local, .ty = arg_ty };
            arg_exprs[i] = try self.program.addExpr(.{ .ty = arg_ty, .data = .{ .local = local } });
        }

        const callee = try self.fnDefForProcedureUseWithType(view, procedure, request.checked_type);
        const body = try self.program.addExpr(.{
            .ty = fn_data.ret,
            .data = .{ .call_proc = .{
                .callee = .{ .func = callee },
                .args = try self.program.addExprSpan(arg_exprs),
            } },
        });

        const def: Ast.DefId = @enumFromInt(@as(u32, @intCast(self.program.defs.items.len)));
        try self.program.defs.append(self.allocator, .{
            .symbol = self.symbols.fresh(),
            .fn_def = null,
            .args = try self.program.addTypedLocalSpan(args),
            .body = .{ .roc = body },
            .ret = fn_data.ret,
        });
        return def;
    }

    fn lowerProcedureBindingRoot(
        self: *Builder,
        request: checked.RootRequest,
        binding_id: checked.TopLevelProcedureBindingId,
    ) Allocator.Error!Ast.DefId {
        const view = moduleView(self.root_view);
        const fn_ty = try self.lowerType(view, request.checked_type);
        const fn_data = self.functionShape(fn_ty, "procedure binding root had a non-function checked type");
        const arg_tys = self.program.types.span(fn_data.args);
        const args = try self.allocator.alloc(Ast.TypedLocal, arg_tys.len);
        defer self.allocator.free(args);
        const arg_exprs = try self.allocator.alloc(Ast.ExprId, arg_tys.len);
        defer self.allocator.free(arg_exprs);

        for (arg_tys, 0..) |arg_ty, i| {
            const local = try self.program.addLocal(self.symbols.fresh(), arg_ty);
            args[i] = .{ .local = local, .ty = arg_ty };
            arg_exprs[i] = try self.program.addExpr(.{ .ty = arg_ty, .data = .{ .local = local } });
        }

        const binding = view.top_level_procedure_bindings.get(binding_id);
        const callee = try self.lowerProcedureBindingValue(view, binding, fn_ty);
        const body = try self.program.addExpr(.{
            .ty = fn_data.ret,
            .data = .{ .call_value = .{
                .callee = callee,
                .args = try self.program.addExprSpan(arg_exprs),
            } },
        });

        const def: Ast.DefId = @enumFromInt(@as(u32, @intCast(self.program.defs.items.len)));
        try self.program.defs.append(self.allocator, .{
            .symbol = self.symbols.fresh(),
            .fn_def = null,
            .args = try self.program.addTypedLocalSpan(args),
            .body = .{ .roc = body },
            .ret = fn_data.ret,
        });
        return def;
    }

    fn lowerProcedureBindingValue(
        self: *Builder,
        view: ModuleView,
        binding: checked.TopLevelProcedureBinding,
        mono_fn_ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        return switch (binding.body) {
            .direct_template => blk: {
                const source_fn_ty = schemeRoot(view, binding.source_scheme, "procedure binding source scheme was not output");
                const fn_template = self.fnDefForProcedureBindingBody(
                    view,
                    binding.body,
                    source_fn_ty,
                    view.types.rootKey(source_fn_ty),
                    mono_fn_ty,
                );
                const fn_id = try self.lowerFnTemplateDef(view, fn_template);
                break :blk try self.program.addExpr(.{
                    .ty = self.program.fnSource(fn_id).mono_fn_ty,
                    .data = .{ .fn_def = fn_id },
                });
            },
            .callable_eval_template => |template_id| try self.lowerCallableEvalBindingValue(view, template_id, mono_fn_ty),
        };
    }

    fn lowerCallableEvalBindingValue(
        self: *Builder,
        view: ModuleView,
        template_id: checked.CallableEvalTemplateId,
        mono_fn_ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        const raw = @intFromEnum(template_id);
        if (raw >= view.callable_eval_templates.templates.len) {
            Common.invariant("callable eval binding referenced a missing checked template");
        }
        const template = view.callable_eval_templates.templates[raw];
        const root = view.compile_time_roots.root(template.root);
        return switch (root.payload) {
            .fn_value => |fn_id| try self.restoreConstFnExpr(view, view, fn_id, mono_fn_ty),
            .pending => try self.lowerPendingCallableEvalBindingValue(view, template, root, mono_fn_ty),
            else => Common.invariant("callable eval binding root did not output a callable value"),
        };
    }

    fn lowerPendingCallableEvalBindingValue(
        self: *Builder,
        view: ModuleView,
        template: checked.CallableEvalTemplate,
        root: checked.CompileTimeRoot,
        mono_fn_ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        const wrapper = view.entry_wrappers.lookupByRoot(template.root) orelse
            Common.invariant("callable eval template root had no checked entry wrapper");

        const wrapper_args = try self.program.types.addSpan(&.{});
        const wrapper_fn_ty = try self.program.types.add(.{ .func = .{
            .args = wrapper_args,
            .ret = mono_fn_ty,
        } });
        const wrapper_template = self.fnDefForTemplate(
            view,
            wrapper.template,
            wrapper.checked_fn_root,
            view.types.rootKey(wrapper.checked_fn_root),
            wrapper_fn_ty,
        );

        const graph = try InstGraph.create(self.allocator, &self.program.types, &self.program.names, &self.unsolved_monos);
        defer graph.destroy();
        const saved_graph = self.active_graph;
        self.active_graph = graph;
        defer self.active_graph = saved_graph;
        var body_ctx = try BodyContext.init(self.allocator, self, view, wrapper.template, graph);
        defer body_ctx.deinit();
        const root_fn_key = Ast.fnTemplateDigest(wrapper_template, &self.program.types, &self.program.names);
        body_ctx.owner_context_fn_key = root_fn_key;
        body_ctx.current_fn_key = root_fn_key;
        try body_ctx.constrainTypeToMono(wrapper.checked_fn_root, wrapper_fn_ty);
        try body_ctx.constrainTypeToMono(template.checked_fn_root, mono_fn_ty);
        try body_ctx.constrainKnownType(root.checked_type, mono_fn_ty);

        const lowered = try body_ctx.lowerComptimeRootExprAtType(wrapper.body_expr, mono_fn_ty);
        try self.drainSpecRequests(graph);
        return lowered;
    }

    fn lowerTemplate(
        self: *Builder,
        template_ref: names.ProcTemplate,
        source_ty_view: ModuleView,
        source_fn_ty: checked.CheckedTypeId,
    ) Allocator.Error!Ast.DefId {
        const fn_ty = try self.lowerType(source_ty_view, source_fn_ty);
        return try self.lowerTemplateWithMono(template_ref, source_ty_view, source_fn_ty, source_ty_view.types.rootKey(source_fn_ty), fn_ty);
    }

    fn lowerTemplateWithMono(
        self: *Builder,
        template_ref: names.ProcTemplate,
        source_ty_view: ModuleView,
        source_fn_ty: checked.CheckedTypeId,
        source_fn_key: names.TypeDigest,
        fn_ty: Type.TypeId,
    ) Allocator.Error!Ast.DefId {
        return try self.lowerTemplateWithMonoFor(template_ref, source_ty_view, source_fn_ty, source_fn_key, fn_ty, null);
    }

    /// Specializations of one template family are deduplicated by structural
    /// function type. The request type is kept as a cache key even after the
    /// body solves that type to a more precise definition type, because
    /// deferred requests from other graphs can still carry the original request
    /// shape. Ready entries flow their solved type back into requesters so call
    /// sites and definitions stay in the same Monotype class.
    fn lowerTemplateWithMonoFor(
        self: *Builder,
        template_ref: names.ProcTemplate,
        source_ty_view: ModuleView,
        source_fn_ty: checked.CheckedTypeId,
        source_fn_key: names.TypeDigest,
        fn_ty: Type.TypeId,
        requester: ?*InstGraph,
    ) Allocator.Error!Ast.DefId {
        const family = TemplateFamily.from(template_ref, source_fn_key);
        const family_entry = try self.lowered_templates.getOrPut(family);
        if (!family_entry.found_existing) family_entry.value_ptr.* = .empty;
        const fn_ty_digest = self.program.types.typeDigest(&self.program.names, fn_ty);
        var reserved_def: ?Ast.DefId = null;
        var lower_fn_ty = fn_ty;
        for (family_entry.value_ptr.items) |*existing| {
            var matched_ty: ?Type.TypeId = null;
            if (existing.request_fn_ty == fn_ty) {
                matched_ty = existing.request_fn_ty;
            } else if (std.mem.eql(u8, existing.request_digest.bytes[0..], fn_ty_digest.bytes[0..])) {
                matched_ty = existing.request_fn_ty;
            }
            if (matched_ty == null and existing.status == .ready) {
                if (existing.solved_fn_ty == fn_ty) {
                    matched_ty = existing.solved_fn_ty;
                } else if (std.mem.eql(u8, existing.solved_digest.bytes[0..], fn_ty_digest.bytes[0..])) {
                    matched_ty = existing.solved_fn_ty;
                }
            }
            const match_ty = matched_ty orelse continue;
            if (requester) |graph| {
                if (match_ty != fn_ty) {
                    try graph.unify(try graph.importMono(fn_ty), try graph.importMono(match_ty));
                }
                if (existing.status == .ready and existing.solved_fn_ty != fn_ty) {
                    try graph.unify(try graph.importMono(fn_ty), try graph.importMono(existing.solved_fn_ty));
                }
                try graph.drainDirty();
            }
            switch (existing.status) {
                .ready,
                .lowering,
                => return existing.def,
                .reserved => {
                    reserved_def = existing.def;
                    lower_fn_ty = existing.request_fn_ty;
                    existing.status = .lowering;
                    break;
                },
            }
        }

        const view = self.moduleForDigest(names.procTemplateModuleDigest(template_ref));
        const template = view.templates.get(template_ref.template);
        const fn_template = self.fnDefForTemplate(view, template_ref, source_fn_ty, source_fn_key, lower_fn_ty);

        const Reservation = struct {
            def: Ast.DefId,
            fn_id: Ast.FnId,
            symbol: Common.Symbol,
        };
        const reservation: Reservation = if (reserved_def) |def_id| blk: {
            const def = &self.program.defs.items[@intFromEnum(def_id)];
            const fn_id = def.fn_id orelse
                Common.invariant("reserved Monotype procedure template definition had no function id");
            def.fn_def = fn_template;
            self.program.fns.items[@intFromEnum(fn_id)].source = fn_template;
            break :blk .{
                .def = def_id,
                .fn_id = fn_id,
                .symbol = def.symbol,
            };
        } else blk: {
            const symbol = self.symbols.fresh();
            try self.registerProcDebugNameForTemplate(symbol, view, template_ref);
            const fn_id = try self.program.addFn(fn_template);
            const def_id: Ast.DefId = @enumFromInt(@as(u32, @intCast(self.program.defs.items.len)));
            // The definition fills once its body lowers; a recursive request
            // that reuses this entry meanwhile reads the requested template,
            // which is the same specialization by construction.
            try self.program.defs.append(self.allocator, .{
                .symbol = symbol,
                .fn_def = fn_template,
                .fn_id = fn_id,
                .args = Ast.Span(Ast.TypedLocal).empty(),
                .body = .hosted,
                .ret = self.functionShape(lower_fn_ty, "procedure template root type was not a function").ret,
            });
            try family_entry.value_ptr.append(self.allocator, .{
                .def = def_id,
                .request_fn_ty = lower_fn_ty,
                .request_digest = self.program.types.typeDigest(&self.program.names, lower_fn_ty),
                .solved_fn_ty = lower_fn_ty,
                .solved_digest = self.program.types.typeDigest(&self.program.names, lower_fn_ty),
                .status = .lowering,
            });
            break :blk .{
                .def = def_id,
                .fn_id = fn_id,
                .symbol = symbol,
            };
        };

        switch (template.target) {
            .hosted => {
                const fn_data = self.functionShape(lower_fn_ty, "hosted procedure template root type was not a function");
                const args = try self.typedLocalsForArgs(self.program.types.span(fn_data.args));
                self.program.defs.items[@intFromEnum(reservation.def)] = .{
                    .symbol = reservation.symbol,
                    .fn_def = fn_template,
                    .fn_id = reservation.fn_id,
                    .args = args,
                    .body = .hosted,
                    .ret = fn_data.ret,
                };
                self.markTemplateReady(family, reservation.def, lower_fn_ty);
                return reservation.def;
            },
            .roc,
            .intrinsic,
            .entry,
            .comptime_only,
            => {},
        }

        const graph = try InstGraph.create(self.allocator, &self.program.types, &self.program.names, &self.unsolved_monos);
        defer graph.destroy();
        const saved_graph = self.active_graph;
        self.active_graph = graph;
        defer self.active_graph = saved_graph;
        var body_ctx = try BodyContext.init(self.allocator, self, view, template_ref, graph);
        const root_fn_key = Ast.fnTemplateDigest(fn_template, &self.program.types, &self.program.names);
        body_ctx.owner_context_fn_key = root_fn_key;
        body_ctx.current_fn_key = root_fn_key;
        defer body_ctx.deinit();
        const public_constraint_fn_ty = try body_ctx.publicOpaqueFunctionUnificationType(lower_fn_ty);
        if (moduleBytesEqual(source_ty_view.key.bytes, view.key.bytes)) {
            try body_ctx.constrainKnownType(source_fn_ty, public_constraint_fn_ty);
        }
        try body_ctx.constrainTypeToMono(template.checked_fn_root, public_constraint_fn_ty);

        // The requested function type becomes a view of this specialization's
        // root: every later stage compares call-site and definition types for
        // exact equality, so body evidence that refines the root (rows the
        // request narrowed, slots only the body pins) must reach the
        // requester's id in place. Builder-global types stay snapshots; they
        // serve many specializations.
        const root_node = try body_ctx.instNode(template.checked_fn_root);
        if (!self.unsolved_monos.contains(lower_fn_ty)) {
            try graph.addMonoView(root_node, lower_fn_ty);
        }
        const live_fn_ty = try graph.monoFor(root_node);
        const lowered = try body_ctx.lowerTemplateBody(template_ref, template, live_fn_ty);
        // The definition records the body's solved view of the root type.
        // Deferred call sites embed the requested type, which digests
        // identically because digests are alias-transparent and a solved
        // requester determines every interface slot; root-class call sites
        // adopt this recorded template directly.
        var def_template = fn_template;
        def_template.mono_fn_ty = live_fn_ty;
        self.program.fns.items[@intFromEnum(reservation.fn_id)].source = def_template;
        // The body may have refined slots the request left unresolved (empty
        // tag unions). Flow the solved view back into the requester's Monotype
        // so call sites embedding the requested id digest identically to this
        // definition.
        if (requester) |requester_graph| {
            try requester_graph.unify(
                try requester_graph.importMono(fn_ty),
                try requester_graph.importMono(live_fn_ty),
            );
            try requester_graph.drainDirty();
        }
        self.program.defs.items[@intFromEnum(reservation.def)] = .{
            .symbol = reservation.symbol,
            .fn_def = def_template,
            .fn_id = reservation.fn_id,
            .args = lowered.args,
            .body = .{ .roc = lowered.body },
            .ret = lowered.ret,
        };
        self.markTemplateReady(family, reservation.def, live_fn_ty);
        try self.drainSpecRequests(graph);
        return reservation.def;
    }

    fn reserveTemplateWithMonoFor(
        self: *Builder,
        template_ref: names.ProcTemplate,
        source_fn_ty: checked.CheckedTypeId,
        source_fn_key: names.TypeDigest,
        fn_ty: Type.TypeId,
        requester: *InstGraph,
    ) Allocator.Error!ReservedTemplate {
        const family = TemplateFamily.from(template_ref, source_fn_key);
        const family_entry = try self.lowered_templates.getOrPut(family);
        if (!family_entry.found_existing) family_entry.value_ptr.* = .empty;
        const fn_ty_digest = self.program.types.typeDigest(&self.program.names, fn_ty);
        for (family_entry.value_ptr.items) |existing| {
            var matched_ty: ?Type.TypeId = null;
            if (existing.request_fn_ty == fn_ty) {
                matched_ty = existing.request_fn_ty;
            } else if (std.mem.eql(u8, existing.request_digest.bytes[0..], fn_ty_digest.bytes[0..])) {
                matched_ty = existing.request_fn_ty;
            }
            if (matched_ty == null and existing.status == .ready) {
                if (existing.solved_fn_ty == fn_ty) {
                    matched_ty = existing.solved_fn_ty;
                } else if (std.mem.eql(u8, existing.solved_digest.bytes[0..], fn_ty_digest.bytes[0..])) {
                    matched_ty = existing.solved_fn_ty;
                }
            }
            const match_ty = matched_ty orelse continue;
            if (match_ty != fn_ty) {
                try requester.unify(try requester.importMono(fn_ty), try requester.importMono(match_ty));
            }
            if (existing.status == .ready and existing.solved_fn_ty != fn_ty) {
                try requester.unify(try requester.importMono(fn_ty), try requester.importMono(existing.solved_fn_ty));
            }
            try requester.drainDirty();
            const def = self.program.defs.items[@intFromEnum(existing.def)];
            return .{
                .fn_id = def.fn_id orelse
                    Common.invariant("reserved Monotype procedure template definition had no function id"),
                .needs_lowering = existing.status == .reserved,
            };
        }

        const view = self.moduleForDigest(names.procTemplateModuleDigest(template_ref));
        const fn_template = self.fnDefForTemplate(view, template_ref, source_fn_ty, source_fn_key, fn_ty);
        const fn_id = try self.program.addFn(fn_template);
        const def_id: Ast.DefId = @enumFromInt(@as(u32, @intCast(self.program.defs.items.len)));
        const symbol = self.symbols.fresh();
        try self.registerProcDebugNameForTemplate(symbol, view, template_ref);
        try self.program.defs.append(self.allocator, .{
            .symbol = symbol,
            .fn_def = fn_template,
            .fn_id = fn_id,
            .args = Ast.Span(Ast.TypedLocal).empty(),
            .body = .hosted,
            .ret = self.functionShape(fn_ty, "procedure template root type was not a function").ret,
        });
        try family_entry.value_ptr.append(self.allocator, .{
            .def = def_id,
            .request_fn_ty = fn_ty,
            .request_digest = fn_ty_digest,
            .solved_fn_ty = fn_ty,
            .solved_digest = fn_ty_digest,
            .status = .reserved,
        });
        return .{
            .fn_id = fn_id,
            .needs_lowering = true,
        };
    }

    fn markTemplateReady(self: *Builder, family: TemplateFamily, def: Ast.DefId, fn_ty: Type.TypeId) void {
        const entries = self.lowered_templates.getPtr(family) orelse
            Common.invariant("lowered procedure template family disappeared before completion");
        for (entries.items) |*entry| {
            if (entry.def != def) continue;
            entry.solved_fn_ty = fn_ty;
            entry.solved_digest = self.program.types.typeDigest(&self.program.names, fn_ty);
            entry.status = .ready;
            return;
        }
        Common.invariant("lowered procedure template definition disappeared before completion");
    }

    fn typedLocalsForArgs(self: *Builder, arg_tys: []const Type.TypeId) Allocator.Error!Ast.Span(Ast.TypedLocal) {
        const args = try self.allocator.alloc(Ast.TypedLocal, arg_tys.len);
        defer self.allocator.free(args);
        for (arg_tys, 0..) |arg_ty, index| {
            args[index] = .{
                .local = try self.program.addLocal(self.symbols.fresh(), arg_ty),
                .ty = arg_ty,
            };
        }
        return try self.program.addTypedLocalSpan(args);
    }

    fn fnDefForTemplate(
        self: *Builder,
        view: ModuleView,
        template: names.ProcTemplate,
        source_fn_ty: checked.CheckedTypeId,
        source_fn_key: names.TypeDigest,
        mono_fn_ty: Type.TypeId,
    ) Ast.FnTemplate {
        const fn_def: Ast.FnDef = switch (view.templates.get(template.template).target) {
            .hosted => if (moduleBytesEqual(view.key.bytes, names.procTemplateModuleDigest(template).bytes))
                .{ .local_hosted = self.hostedFn(template) }
            else
                .{ .imported_hosted = self.hostedFn(template) },
            .roc,
            .intrinsic,
            .entry,
            .comptime_only,
            => if (moduleBytesEqual(view.key.bytes, names.procTemplateModuleDigest(template).bytes))
                .{ .local_template = template }
            else
                .{ .imported_template = template },
        };

        return .{
            .fn_def = fn_def,
            .source_fn_ty = source_fn_ty,
            .source_fn_key = source_fn_key,
            .mono_fn_ty = mono_fn_ty,
        };
    }

    fn registerProcDebugNameForTemplate(
        self: *Builder,
        symbol: Common.Symbol,
        view: ModuleView,
        template: names.ProcTemplate,
    ) Allocator.Error!void {
        if (!self.proc_debug_names) return;
        const proc_base = view.names.procBase(template.proc_base);
        const export_name = proc_base.export_name orelse return;
        const name = try self.program.names.internExportName(view.names.exportNameText(export_name));
        try self.program.setProcDebugName(symbol, name);
    }

    fn fnDefForProcedureBindingBody(
        self: *Builder,
        view: ModuleView,
        body: checked.ProcedureBindingBody,
        source_fn_ty: checked.CheckedTypeId,
        source_fn_key: names.TypeDigest,
        mono_fn_ty: Type.TypeId,
    ) Ast.FnTemplate {
        return switch (body) {
            .direct_template => |direct| self.fnDefForCallableTemplate(view, direct.template, source_fn_ty, source_fn_key, mono_fn_ty),
            .callable_eval_template => Common.invariant("callable eval template must be restored through ConstStore before Monotype lowering"),
        };
    }

    fn fnDefForImportedBindingBody(
        self: *Builder,
        view: ModuleView,
        body: checked.ImportedProcedureBindingBody,
        source_fn_ty: checked.CheckedTypeId,
        source_fn_key: names.TypeDigest,
        mono_fn_ty: Type.TypeId,
    ) Ast.FnTemplate {
        return switch (body) {
            .direct_template => |direct| self.fnDefForCallableTemplate(view, direct.template, source_fn_ty, source_fn_key, mono_fn_ty),
            .callable_eval_template => Common.invariant("imported callable eval template must be restored through ConstStore before Monotype lowering"),
        };
    }

    fn fnDefForCallableTemplate(
        self: *Builder,
        view: ModuleView,
        template: names.CallableProcTemplate,
        source_fn_ty: checked.CheckedTypeId,
        source_fn_key: names.TypeDigest,
        mono_fn_ty: Type.TypeId,
    ) Ast.FnTemplate {
        return switch (template) {
            .checked => |checked_template| self.fnDefForTemplate(view, checked_template, source_fn_ty, source_fn_key, mono_fn_ty),
            .lifted,
            .synthetic,
            => Common.invariant("checked procedure binding referenced a post-check function template"),
        };
    }

    fn lowerType(self: *Builder, view: ModuleView, checked_ty: checked.CheckedTypeId) Allocator.Error!Type.TypeId {
        const address = checkedTypeAddress(view, checked_ty);
        if (self.type_cache.get(address)) |cached| return cached;

        const raw = @intFromEnum(checked_ty);
        if (raw >= view.types.payloadCount()) Common.invariant("checked type id outside checked type store");

        const reserved = try self.program.types.add(.zst);
        try self.type_cache.put(address, reserved);
        try self.unsolved_monos.put(reserved, {});
        const lowered = try self.lowerTypePayload(view, checked_ty, view.types.payload(checked_ty));
        self.program.types.types.items[@intFromEnum(reserved)] = lowered;
        return reserved;
    }

    fn lowerTypePayload(self: *Builder, view: ModuleView, checked_ty: checked.CheckedTypeId, payload: checked.CheckedTypePayload) Allocator.Error!Type.Content {
        return switch (payload) {
            .pending => Common.invariant("pending checked type reached Monotype lowering"),
            .flex => |variable| lowerCheckedTypeVariable(variable),
            .rigid => |variable| lowerCheckedTypeVariable(variable),
            .empty_record => .{ .record = .empty() },
            .empty_tag_union => .{ .tag_union = .empty() },
            .record_unbound => |fields| try self.lowerRecordFields(view, fields),
            .record => |record| try self.lowerRecordRow(view, record.fields, record.ext),
            .tuple => |items| blk: {
                const lowered = try self.lowerTypeSlice(view, items);
                defer self.allocator.free(lowered);
                break :blk .{ .tuple = try self.program.types.addSpan(lowered) };
            },
            .tag_union => |tag_union| try self.lowerTagUnionRow(view, tag_union.tags, tag_union.ext),
            .function => |fn_ty| blk: {
                const args = try self.lowerTypeSlice(view, fn_ty.args);
                defer self.allocator.free(args);
                break :blk .{ .func = .{
                    .args = try self.program.types.addSpan(args),
                    .ret = try self.lowerType(view, fn_ty.ret),
                } };
            },
            .alias => |alias| blk: {
                const args = try self.lowerTypeSlice(view, alias.args);
                defer self.allocator.free(args);
                break :blk .{ .named = .{
                    .named_type = .{ .module = self.declaredModuleForAlias(view, alias), .ty = checked_ty },
                    .def = try self.typeDef(view, alias.origin_module, alias.name, alias.source_decl),
                    .kind = .alias,
                    .args = try self.program.types.addSpan(args),
                    .backing = .{
                        .ty = try self.lowerType(view, alias.backing),
                        .use = .inspectable,
                    },
                } };
            },
            .nominal => |nominal| blk: {
                switch (nominal.representation) {
                    .builtin => |builtin| switch (checked.builtinRuntimeEncoding(builtin)) {
                        .primitive => |primitive| break :blk .{ .primitive = primitive },
                        .bool_tag_union => {},
                        .list => {
                            if (nominal.args.len != 1) Common.invariant("checked List nominal must have exactly one type argument");
                            break :blk .{ .list = try self.lowerType(view, nominal.args[0]) };
                        },
                        .box => {
                            if (nominal.args.len != 1) Common.invariant("checked Box nominal must have exactly one type argument");
                            break :blk .{ .box = try self.lowerType(view, nominal.args[0]) };
                        },
                        .parse_tag_union_spec,
                        .fields,
                        .field,
                        => {},
                    },
                    else => {},
                }

                const args = try self.lowerTypeSlice(view, nominal.args);
                defer self.allocator.free(args);
                const backing_use: Type.BackingUse = if (nominal.is_opaque) .runtime_layout_only else .inspectable;
                break :blk .{ .named = .{
                    .named_type = .{ .module = self.declaredModuleForNominal(view, nominal), .ty = checked_ty },
                    .def = try self.typeDef(view, nominal.origin_module, nominal.name, nominal.source_decl),
                    .kind = if (nominal.is_opaque) .@"opaque" else .nominal,
                    .builtin_owner = builtinOwner(nominal.builtin),
                    .args = try self.program.types.addSpan(args),
                    .backing = switch (nominal.representation) {
                        .opaque_without_backing => null,
                        else => .{
                            .ty = try self.lowerNominalBackingType(view, nominal, args),
                            .use = backing_use,
                        },
                    },
                    .declared_order = try self.declaredOrderForNominal(view, nominal),
                } };
            },
        };
    }

    fn lowerCheckedTypeVariable(variable: checked.CheckedTypeVariable) Type.Content {
        if (variable.numeric_default_phase) |phase| {
            return switch (phase) {
                .mono_specialization => .{ .primitive = .dec },
                .mono_specialization_str => .{ .primitive = .str },
                .checking_finalized => Common.invariant("checking-finalized numeric variable reached Monotype unresolved"),
            };
        }
        if (variable.row_default) |row_default| {
            return switch (row_default) {
                .empty_record => .{ .record = .empty() },
                .empty_tag_union => .{ .tag_union = .empty() },
            };
        }
        return .{ .tag_union = .empty() };
    }

    fn typeIsDec(self: *Builder, ty: Type.TypeId) bool {
        return switch (self.shapeContent(ty)) {
            .primitive => |primitive| primitive == .dec,
            else => false,
        };
    }

    fn namedBackingType(self: *Builder, ty: Type.TypeId) ?Type.TypeId {
        return switch (self.program.types.get(ty)) {
            .named => |named| if (named.backing) |backing| backing.ty else null,
            else => null,
        };
    }

    fn shapeContent(self: *Builder, ty: Type.TypeId) Type.Content {
        var current = ty;
        while (true) {
            switch (self.program.types.get(current)) {
                .named => |named| if (named.backing) |backing| {
                    current = backing.ty;
                    continue;
                } else {
                    return self.program.types.get(current);
                },
                else => |content| return content,
            }
        }
    }

    fn functionShape(self: *Builder, ty: Type.TypeId, comptime message: []const u8) FunctionShape {
        return switch (self.shapeContent(ty)) {
            .func => |func| .{ .args = func.args, .ret = func.ret },
            else => Common.invariant(message),
        };
    }

    fn tupleItemSpan(self: *Builder, ty: Type.TypeId) Type.Span {
        return switch (self.shapeContent(ty)) {
            .tuple => |items| items,
            else => Common.invariant("tuple pattern had a non-tuple checked type"),
        };
    }

    fn recordFieldsSpan(self: *Builder, ty: Type.TypeId) Type.Span {
        return switch (self.shapeContent(ty)) {
            .record => |fields| fields,
            else => Common.invariant("record pattern had a non-record checked type"),
        };
    }

    fn tagPayloadSpan(self: *Builder, ty: Type.TypeId, name: names.TagNameId) Type.Span {
        return switch (self.shapeContent(ty)) {
            .tag_union => |tags| {
                for (self.program.types.tagSpan(tags)) |tag| {
                    if (self.program.names.tagLabelTextEql(tag.name, name)) return tag.payloads;
                }
                Common.invariant("tag pattern was absent from checked tag-union type");
            },
            else => Common.invariant("tag pattern had a non-tag-union checked type"),
        };
    }

    fn lowerNominalBackingType(
        self: *Builder,
        view: ModuleView,
        nominal: checked.CheckedNominalType,
        mono_args: []const Type.TypeId,
    ) Allocator.Error!Type.TypeId {
        const graph = try InstGraph.create(self.allocator, &self.program.types, &self.program.names, &self.unsolved_monos);
        defer graph.destroy();
        const saved_graph = self.active_graph;
        self.active_graph = graph;
        defer self.active_graph = saved_graph;
        var ctx = try BodyContext.init(self.allocator, self, view, .{
            .proc_base = undefined, // type-only context; type lowering does not read the owner template
            .template = undefined, // type-only context; type lowering does not read the owner template
        }, graph);
        defer ctx.deinit();
        if (nominal.args.len != mono_args.len) {
            Common.invariant("checked nominal type argument arity differed from monotype named argument arity");
        }
        for (nominal.args, mono_args) |checked_arg, mono_arg| {
            try ctx.constrainTypeToMono(checked_arg, mono_arg);
        }
        if (ctx.nominalInstantiationSource(nominal)) |source| {
            if (source.declaration.formalArgs(ctx.view.types).len != mono_args.len) {
                Common.invariant("checked nominal declaration arity differed from nominal type use");
            }
            for (source.declaration.formalArgs(ctx.view.types), mono_args) |formal, mono_arg| {
                try ctx.constrainTypeToMono(ctx.checkedTypeInCurrentView(source.view, formal), mono_arg);
            }
        }
        const backing = try ctx.lowerType(ctx.nominalBackingRoot(nominal));
        return try self.structuralBackingForNominal(view, nominal, backing);
    }

    fn structuralBackingForNominal(
        self: *Builder,
        view: ModuleView,
        nominal: checked.CheckedNominalType,
        backing: Type.TypeId,
    ) Allocator.Error!Type.TypeId {
        const owner_def = try self.typeDef(view, nominal.origin_module, nominal.name, nominal.source_decl);
        var seen = std.AutoHashMap(Type.TypeId, void).init(self.allocator);
        defer seen.deinit();
        var current = backing;
        while (true) {
            if (seen.contains(current)) return current;
            try seen.put(current, {});
            switch (self.program.types.get(current)) {
                .named => |named| {
                    if (named.kind != .alias and !sameTypeDef(named.def, owner_def)) return current;
                    const next = named.backing orelse return current;
                    current = next.ty;
                },
                else => return current,
            }
        }
    }

    fn tupleItemTypes(self: *Builder, ty: Type.TypeId) []const Type.TypeId {
        return self.program.types.span(self.tupleItemSpan(ty));
    }

    fn recordFieldType(self: *Builder, ty: Type.TypeId, name: names.RecordFieldNameId) Type.TypeId {
        for (self.program.types.fieldSpan(self.recordFieldsSpan(ty))) |field| {
            if (self.program.names.recordFieldLabelTextEql(field.name, name)) return field.ty;
        }
        Common.invariant("record pattern field was absent from checked record type");
    }

    fn tagPayloadTypes(self: *Builder, ty: Type.TypeId, name: names.TagNameId) []const Type.TypeId {
        return self.program.types.span(self.tagPayloadSpan(ty, name));
    }

    fn lowerRecordFields(self: *Builder, view: ModuleView, fields: []const checked.CheckedRecordField) Allocator.Error!Type.Content {
        const lowered = try self.allocator.alloc(Type.Field, fields.len);
        defer self.allocator.free(lowered);
        for (fields, 0..) |field, i| {
            lowered[i] = .{
                .name = try self.recordFieldName(view, field.name),
                .ty = try self.lowerType(view, field.ty),
            };
        }
        return .{ .record = try self.program.types.addFields(lowered) };
    }

    fn lowerRecordRow(
        self: *Builder,
        view: ModuleView,
        head: []const checked.CheckedRecordField,
        ext: checked.CheckedTypeId,
    ) Allocator.Error!Type.Content {
        var fields = std.ArrayList(Type.Field).empty;
        defer fields.deinit(self.allocator);
        try self.appendRecordFields(view, &fields, head);

        var seen = std.AutoHashMap(checked.CheckedTypeId, void).init(self.allocator);
        defer seen.deinit();

        var current = ext;
        while (true) {
            if (seen.contains(current)) break;
            try seen.put(current, {});

            const payload = checkedPayload(view, current);
            switch (payload) {
                .alias => |alias| current = alias.backing,
                .empty_record => break,
                .flex, .rigid => |variable| {
                    if (variable.row_default == .empty_record) break;
                    Common.invariant("open non-record checked row reached Monotype record lowering");
                },
                .record_unbound => |tail_fields| {
                    try self.appendRecordFields(view, &fields, tail_fields);
                    break;
                },
                .record => |record| {
                    try self.appendRecordFields(view, &fields, record.fields);
                    current = record.ext;
                },
                else => Common.invariant("open or non-record checked row reached Monotype record lowering"),
            }
        }

        std.mem.sort(Type.Field, fields.items, &self.program.names, recordFieldLessThan);
        assertNoDuplicateRecordFields(&self.program.names, fields.items, "checked record row had duplicate fields at Monotype lowering");

        return .{ .record = try self.program.types.addFields(fields.items) };
    }

    fn appendRecordFields(
        self: *Builder,
        view: ModuleView,
        out: *std.ArrayList(Type.Field),
        fields: []const checked.CheckedRecordField,
    ) Allocator.Error!void {
        for (fields) |field| {
            try out.append(self.allocator, .{
                .name = try self.recordFieldName(view, field.name),
                .ty = try self.lowerType(view, field.ty),
            });
        }
    }

    fn lowerTagUnionRow(
        self: *Builder,
        view: ModuleView,
        head: []const checked.CheckedTag,
        ext: checked.CheckedTypeId,
    ) Allocator.Error!Type.Content {
        var tags = std.ArrayList(Type.Tag).empty;
        defer tags.deinit(self.allocator);
        try self.appendTags(view, &tags, head);

        var seen = std.AutoHashMap(checked.CheckedTypeId, void).init(self.allocator);
        defer seen.deinit();

        var current = ext;
        while (true) {
            if (seen.contains(current)) break;
            try seen.put(current, {});

            const payload = checkedPayload(view, current);
            switch (payload) {
                .alias => |alias| current = alias.backing,
                .empty_tag_union => break,
                .flex, .rigid => |variable| {
                    if (variable.row_default == .empty_tag_union) break;
                    Common.invariant("open non-tag checked row reached Monotype tag-union lowering");
                },
                .tag_union => |tag_union| {
                    try self.appendTags(view, &tags, tag_union.tags);
                    current = tag_union.ext;
                },
                else => Common.invariant("open or non-tag checked row reached Monotype tag-union lowering"),
            }
        }

        std.mem.sort(Type.Tag, tags.items, &self.program.names, tagLessThan);
        assertNoDuplicateTags(&self.program.names, tags.items, "checked tag row had duplicate tags at Monotype lowering");

        return .{ .tag_union = try self.program.types.addTags(tags.items) };
    }

    fn appendTags(
        self: *Builder,
        view: ModuleView,
        out: *std.ArrayList(Type.Tag),
        tags: []const checked.CheckedTag,
    ) Allocator.Error!void {
        for (tags) |tag| {
            const payloads = try self.lowerTypeSlice(view, tag.argsSlice(view.types));
            defer self.allocator.free(payloads);
            try out.append(self.allocator, .{
                .name = try self.tagName(view, tag.name),
                .checked_name = tag.name,
                .payloads = try self.program.types.addSpan(payloads),
            });
        }
    }

    fn lowerTypeSlice(self: *Builder, view: ModuleView, checked_tys: []const checked.CheckedTypeId) Allocator.Error![]Type.TypeId {
        const out = try self.allocator.alloc(Type.TypeId, checked_tys.len);
        errdefer self.allocator.free(out);
        for (checked_tys, 0..) |ty, i| {
            out[i] = try self.lowerType(view, ty);
        }
        return out;
    }

    fn moduleForDigest(self: *Builder, module_digest: names.CheckedModuleDigest) ModuleView {
        if (moduleBytesEqual(module_digest.bytes, self.root_view.key.bytes)) return moduleView(self.root_view);
        for (self.modules.imports) |imported| {
            if (moduleBytesEqual(module_digest.bytes, imported.key.bytes)) return moduleView(imported);
        }
        for (self.modules.root.relation_modules) |relation| {
            if (moduleBytesEqual(module_digest.bytes, relation.key.bytes)) return moduleView(relation);
        }
        Common.invariant("procedure template referenced a checked module that is not in the lowering input");
    }

    fn moduleForId(self: *Builder, module_id: checked.ModuleId) ModuleView {
        if (moduleBytesEqual(module_id.bytes, self.root_view.key.bytes)) return moduleView(self.root_view);
        for (self.modules.imports) |imported| {
            if (moduleBytesEqual(module_id.bytes, imported.key.bytes)) return moduleView(imported);
        }
        for (self.modules.root.relation_modules) |relation| {
            if (moduleBytesEqual(module_id.bytes, relation.key.bytes)) return moduleView(relation);
        }
        Common.invariant("procedure binding referenced a checked module that is not in the lowering input");
    }

    const NominalDeclLookup = struct {
        view: ModuleView,
        declaration: checked.CheckedNominalDeclaration,
        /// Unnamed padding field types (declared order), in `view`'s store. For a
        /// box-payload capability this is the *instance's* substituted padding, so
        /// type-parameterized padding reserves its instantiated size; for a direct
        /// declaration it is the declaration's own padding types.
        padding_field_tys: []const checked.CheckedTypeId,
    };

    /// Resolves a nominal's source declaration across every representation that
    /// has one, for reading declared field order. The box-payload representation
    /// is the common case for record nominals (assigned while the checked module
    /// data is built), so it must be handled, not just `*_declaration`.
    fn nominalDeclarationFor(self: *Builder, view: ModuleView, nominal: checked.CheckedNominalType) ?NominalDeclLookup {
        return switch (nominal.representation) {
            .local_declaration => |id| blk: {
                const decl = view.types.nominalDeclarationById(id);
                break :blk .{ .view = view, .declaration = decl, .padding_field_tys = decl.paddingFieldTypes(view.types) };
            },
            .imported_declaration => |imported| blk: {
                const sv = self.moduleForId(checked.importedNominalDeclarationModuleId(imported));
                const decl = sv.types.nominalDeclarationById(imported.declaration);
                break :blk .{ .view = sv, .declaration = decl, .padding_field_tys = decl.paddingFieldTypes(sv.types) };
            },
            .local_box_payload_capability => |cap| blk: {
                const capability = view.interface_capabilities.boxPayloadCapability(cap.capability);
                const decl = view.types.nominalDeclaration(capability.nominal) orelse break :blk null;
                break :blk .{ .view = view, .declaration = decl, .padding_field_tys = capability.paddingFieldTys(view.interface_capabilities) };
            },
            .imported_box_payload_capability => |cap| blk: {
                const sv = self.moduleForId(checked.importedBoxPayloadCapabilityModuleId(cap));
                const capability = sv.interface_capabilities.boxPayloadCapability(cap.capability);
                const decl = sv.types.nominalDeclaration(capability.nominal) orelse break :blk null;
                break :blk .{ .view = sv, .declaration = decl, .padding_field_tys = capability.paddingFieldTys(sv.interface_capabilities) };
            },
            .builtin, .opaque_without_backing => null,
        };
    }

    /// Builds the declared-field-order span for a nominal/opaque record backing
    /// from its source declaration (the declaration backing preserves source
    /// order, unlike the lexicographically-sorted lowered row). Returns the empty
    /// span for non-record backings or when no declaration is available. The
    /// span feeds layout selection only; the lowered row stays lexicographic.
    fn declaredOrderForNominal(self: *Builder, view: ModuleView, nominal: checked.CheckedNominalType) Allocator.Error!Type.Span {
        const lookup = self.nominalDeclarationFor(view, nominal) orelse return Type.Span.empty();
        const source_decl = lookup.declaration.nominal.source_decl orelse return Type.Span.empty();
        // Read declared field order from the declaration's record annotation,
        // which preserves source order (the checked row and every lowered row
        // sort lexicographically). `lookup.view` is the declaring module, so this
        // is correct across module boundaries.
        const module_env = lookup.view.module_env;
        const anno_idx = switch (module_env.store.getStatement(@enumFromInt(source_decl))) {
            .s_nominal_decl => |decl| decl.anno,
            else => return Type.Span.empty(),
        };
        // The backing record may be wrapped in parentheses; unwrap before reading
        // its fields (the backing type itself already handles parens).
        var record_anno = anno_idx;
        const record = while (true) {
            switch (module_env.store.getTypeAnno(record_anno)) {
                .parens => |parens| record_anno = parens.anno,
                .record => |record| break record,
                else => return Type.Span.empty(),
            }
        };
        const fields = module_env.store.sliceAnnoRecordFields(record.fields);
        if (fields.len == 0) return Type.Span.empty();
        // Unnamed fields are layout padding; their resolved checked types ride on
        // the declaration in declared order and are pulled sequentially as each
        // unnamed field is encountered while walking the declared annotation.
        // Padding types come from the lookup, which for an instantiated nominal
        // (box-payload capability) carries the *instance's* substituted padding
        // types — so a type-parameterized padding field (`_ : a`) reserves the
        // instantiated size, exactly like a named field of the same type.
        const padding_types = lookup.padding_field_tys;
        const padding_view = lookup.view;
        var padding_cursor: usize = 0;
        const entries = try self.allocator.alloc(Type.DeclaredField, fields.len);
        defer self.allocator.free(entries);
        for (fields, 0..) |field_idx, i| {
            const field = module_env.store.getAnnoRecordField(field_idx);
            if (field.is_unnamed) {
                if (padding_cursor >= padding_types.len) {
                    Common.invariant("nominal declaration had more unnamed fields than recorded padding types");
                }
                const checked_ty = padding_types[padding_cursor];
                padding_cursor += 1;
                entries[i] = .{ .padding = try self.lowerType(padding_view, checked_ty) };
            } else {
                entries[i] = .{ .named = try self.program.names.internRecordFieldLabel(module_env.getIdentText(field.name)) };
            }
        }
        return try self.program.types.addDeclaredFields(entries);
    }

    fn providedConstNode(self: *Builder, data: checked.ProvidedDataExport) ConstNode {
        const view = self.moduleForId(checked.constModuleId(data.const_ref));
        const template = view.const_templates.get(data.const_ref);
        return switch (template.state) {
            .stored_const => |stored| .{ .module = view, .id = stored.node },
            .reserved,
            .eval_template,
            => Common.invariant("static data request const was not stored before Monotype lowering"),
        };
    }

    fn lookupMethodTarget(
        self: *Builder,
        owner: static_dispatch.MethodOwner,
        method_view: ModuleView,
        method: names.MethodNameId,
    ) ?MethodLookup {
        return self.lookupMethodTargetByName(owner, method_view.names.methodNameText(method));
    }

    fn lookupMethodTargetByName(
        self: *Builder,
        owner: static_dispatch.MethodOwner,
        method_name: []const u8,
    ) ?MethodLookup {
        const method = self.program.names.lookupMethodName(method_name) orelse return null;
        return self.lookupMethodTargetInIndex(.{ .owner = owner, .method = method });
    }

    fn lookupMethodTargetInIndex(
        self: *Builder,
        dispatch: MethodDispatch,
    ) ?MethodLookup {
        var low: usize = 0;
        var high: usize = self.method_lookup_index.len;
        while (low < high) {
            const mid = low + (high - low) / 2;
            const entry = self.method_lookup_index[mid];
            switch (methodDispatchOrder(entry.dispatch, dispatch)) {
                .eq => return .{ .view = entry.view, .target = entry.target },
                .lt => low = mid + 1,
                .gt => high = mid,
            }
        }
        return null;
    }

    fn importModuleAlreadyScanned(self: *Builder, module_id: checked.ModuleId, import_index: usize) bool {
        if (moduleBytesEqual(module_id.bytes, self.root_view.key.bytes)) return true;
        for (self.modules.imports[0..import_index]) |imported| {
            if (moduleBytesEqual(module_id.bytes, imported.key.bytes)) return true;
        }
        return false;
    }

    fn relationModuleAlreadyScanned(self: *Builder, module_id: checked.ModuleId, relation_index: usize) bool {
        if (moduleBytesEqual(module_id.bytes, self.root_view.key.bytes)) return true;
        for (self.modules.imports) |imported| {
            if (moduleBytesEqual(module_id.bytes, imported.key.bytes)) return true;
        }
        for (self.modules.root.relation_modules[0..relation_index]) |relation| {
            if (moduleBytesEqual(module_id.bytes, relation.key.bytes)) return true;
        }
        return false;
    }

    fn fnDefForProcedureUseWithType(
        self: *Builder,
        source_ty_view: ModuleView,
        proc: checked.ProcedureUseTemplate,
        source_fn_ty: checked.CheckedTypeId,
    ) Allocator.Error!Ast.FnId {
        const mono_fn_ty = try self.lowerType(source_ty_view, source_fn_ty);
        const source_fn_key = proc.source_fn_ty_template;
        const fn_template = switch (proc.binding) {
            .top_level => |top_level| blk: {
                const view = self.moduleForId(checked.topLevelProcedureModuleId(top_level));
                const binding = view.top_level_procedure_bindings.get(top_level.binding);
                const binding_source = schemeRoot(view, binding.source_scheme, "top-level procedure binding source scheme was not output");
                break :blk self.fnDefForProcedureBindingBody(view, binding.body, binding_source, view.types.rootKey(binding_source), mono_fn_ty);
            },
            .imported => |imported| blk: {
                const view = self.moduleForId(checked.importedProcedureModuleId(imported));
                for (view.exported_procedure_bindings.bindings) |binding| {
                    if (binding.binding.def == imported.def and binding.binding.pattern == imported.pattern) {
                        const binding_source = schemeRoot(view, binding.source_scheme, "imported procedure binding source scheme was not output");
                        break :blk self.fnDefForImportedBindingBody(view, binding.body, binding_source, view.types.rootKey(binding_source), mono_fn_ty);
                    }
                }
                Common.invariant("imported procedure binding was not exported by its checked module");
            },
            .hosted => |hosted| blk: {
                break :blk self.fnDefForTemplate(self.moduleForId(moduleIdFromDigest(names.procTemplateModuleDigest(hosted.template))), hosted.template, source_fn_ty, source_fn_key, mono_fn_ty);
            },
            .platform_required => |required| blk: {
                const app_view = self.moduleForId(checked.requiredProcedureModuleId(required));
                const binding = app_view.top_level_procedure_bindings.get(required.procedure_binding);
                break :blk self.fnDefForProcedureBindingBody(app_view, binding.body, source_fn_ty, source_fn_key, mono_fn_ty);
            },
        };
        return try self.lowerFnTemplateDef(source_ty_view, fn_template);
    }

    /// Lower (or defer) a procedure template body and return the Monotype
    /// function id the call site should embed. Inside a specialization the
    /// request reserves an id and defers the body; outside one (root and
    /// wrapper paths, whose types come from the builder-global cache without
    /// body evidence), the body lowers now.
    fn lowerFnTemplateDef(self: *Builder, source_ty_view: ModuleView, fn_template: Ast.FnTemplate) Allocator.Error!Ast.FnId {
        const template_ref = switch (fn_template.fn_def) {
            .local_template,
            .imported_template,
            .checked_generated,
            => |template| template,
            .parser_runtime,
            .encode_to_runtime,
            => Common.invariant("generated serialization runtime function must be restored through ConstStore runtime restore"),
            .local_hosted,
            .imported_hosted,
            => |hosted| hosted.template,
            .nested => Common.invariant("nested function specialization must be lowered through nested function lowering"),
        };
        // Deferral exists so a requester's solved types key the request; a
        // request typed by an unsolved builder-global Monotype gains nothing
        // from waiting and its call site must embed the definition's solved
        // template instead.
        if (!self.unsolved_monos.contains(fn_template.mono_fn_ty)) {
            if (self.active_graph) |graph| {
                const reserved = try self.reserveTemplateWithMonoFor(
                    template_ref,
                    fn_template.source_fn_ty,
                    fn_template.source_fn_key,
                    fn_template.mono_fn_ty,
                    graph,
                );
                if (reserved.needs_lowering) {
                    try graph.deferred_templates.append(self.allocator, .{
                        .template_ref = template_ref,
                        .module = source_ty_view.key,
                        .source_fn_ty = fn_template.source_fn_ty,
                        .source_fn_key = fn_template.source_fn_key,
                        .fn_ty = fn_template.mono_fn_ty,
                    });
                }
                return reserved.fn_id;
            }
        }
        const def = try self.lowerTemplateWithMono(template_ref, source_ty_view, fn_template.source_fn_ty, fn_template.source_fn_key, fn_template.mono_fn_ty);
        return self.defFnId(def);
    }

    fn lowerRestoredConstFnTemplate(self: *Builder, type_view: ModuleView, fn_template: Ast.FnTemplate) Allocator.Error!Ast.FnId {
        switch (fn_template.fn_def) {
            .nested => {
                const fn_view = self.moduleForConstFnDef(fn_template.fn_def);
                const graph = try InstGraph.create(self.allocator, &self.program.types, &self.program.names, &self.unsolved_monos);
                defer graph.destroy();
                const saved_graph = self.active_graph;
                self.active_graph = graph;
                defer self.active_graph = saved_graph;
                var fn_ctx = try BodyContext.init(self.allocator, self, fn_view, ownerTemplateForConstFnDef(fn_template.fn_def), graph);
                defer fn_ctx.deinit();
                const fn_id = try self.lowerNestedFnFromContext(&fn_ctx, checkedLambdaExprIdForConstFn(fn_view, fn_template.fn_def), fn_template);
                try self.drainSpecRequests(graph);
                return fn_id;
            },
            else => return try self.lowerFnTemplateDef(type_view, fn_template),
        }
    }

    fn lowerFnTemplateDefFromContext(self: *Builder, source_ctx: *BodyContext, fn_template: Ast.FnTemplate) Allocator.Error!Ast.FnId {
        const template_ref = switch (fn_template.fn_def) {
            .local_template,
            .imported_template,
            .checked_generated,
            => |template| template,
            .parser_runtime,
            .encode_to_runtime,
            => Common.invariant("generated serialization runtime function must be restored through ConstStore runtime restore"),
            .local_hosted,
            .imported_hosted,
            => |hosted| hosted.template,
            .nested => Common.invariant("nested function specialization must be lowered through nested function lowering"),
        };
        const reserved = try self.reserveTemplateWithMonoFor(
            template_ref,
            fn_template.source_fn_ty,
            fn_template.source_fn_key,
            fn_template.mono_fn_ty,
            source_ctx.graph,
        );
        if (reserved.needs_lowering) {
            try source_ctx.graph.deferred_templates.append(self.allocator, .{
                .template_ref = template_ref,
                .module = source_ctx.view.key,
                .source_fn_ty = fn_template.source_fn_ty,
                .source_fn_key = fn_template.source_fn_key,
                .fn_ty = fn_template.mono_fn_ty,
            });
        }
        return reserved.fn_id;
    }

    fn defFnId(self: *Builder, def: Ast.DefId) Ast.FnId {
        return self.program.defs.items[@intFromEnum(def)].fn_id orelse
            Common.invariant("Monotype procedure template definition had no function id");
    }

    fn nestedFnForExpr(
        self: *Builder,
        view: ModuleView,
        owner: names.ProcTemplate,
        expr_id: checked.CheckedExprId,
        context_fn_key: names.TypeDigest,
    ) Allocator.Error!Ast.NestedFn {
        const address = NestedSiteAddress.from(view.key, owner, expr_id);
        const site = if (self.nested_site_cache.get(address)) |cached|
            cached
        else blk: {
            for (view.nested_proc_sites.sites) |candidate| {
                if (candidate.checked_expr == null or candidate.checked_expr.? != expr_id) continue;
                if (!names.procedureTemplateRefEql(candidate.owner_template, owner)) continue;
                try self.nested_site_cache.put(address, candidate.site);
                break :blk candidate.site;
            }
            Common.invariant("nested function expression reached Monotype without a checked nested function site");
        };
        return .{
            .owner = owner,
            .site = site,
            .context_fn_key = context_fn_key,
        };
    }

    fn fnTemplateForNestedExprWithMono(
        self: *Builder,
        view: ModuleView,
        owner: names.ProcTemplate,
        expr_id: checked.CheckedExprId,
        checked_fn_ty: checked.CheckedTypeId,
        source_fn_key: names.TypeDigest,
        mono_fn_ty: Type.TypeId,
        context_fn_key: names.TypeDigest,
    ) Allocator.Error!Ast.FnTemplate {
        return .{
            .fn_def = .{ .nested = try self.nestedFnForExpr(view, owner, expr_id, context_fn_key) },
            .source_fn_ty = checked_fn_ty,
            .source_fn_key = source_fn_key,
            .mono_fn_ty = mono_fn_ty,
        };
    }

    fn lowerNestedFnFromContext(
        self: *Builder,
        source_ctx: *BodyContext,
        expr_id: checked.CheckedExprId,
        fn_template: Ast.FnTemplate,
    ) Allocator.Error!Ast.FnId {
        const nested_ctx = try self.allocator.create(BodyContext);
        errdefer self.allocator.destroy(nested_ctx);
        nested_ctx.* = try source_ctx.nestedInstantiationContext(Ast.fnTemplateDigest(fn_template, &self.program.types, &self.program.names));
        // Nested functions share the requester's graph, and an inferred local
        // procedure's body pins signature variables (its own evidence) that
        // the requester's remaining body relies on, so the body lowers now.
        return try self.lowerNestedFnRequest(.{
            .ctx = nested_ctx,
            .expr_id = expr_id,
            .fn_template = fn_template,
        });
    }

    fn lowerNestedFnRequest(self: *Builder, request: NestedFnRequest) Allocator.Error!Ast.FnId {
        defer {
            request.ctx.deinit();
            self.allocator.destroy(request.ctx);
        }
        const fn_template = request.fn_template;
        const nested = switch (fn_template.fn_def) {
            .nested => |nested| nested,
            else => Common.invariant("local procedure specialization did not have a nested function identity"),
        };
        const family = NestedFnFamily.from(nested, fn_template.source_fn_key);
        const family_entry = try self.lowered_nested_fns.getOrPut(family);
        if (!family_entry.found_existing) family_entry.value_ptr.* = .empty;
        const fn_ty_digest = self.program.types.typeDigest(&self.program.names, fn_template.mono_fn_ty);
        for (family_entry.value_ptr.items) |existing| {
            if (existing.fn_ty != fn_template.mono_fn_ty) {
                const existing_digest = self.program.types.typeDigest(&self.program.names, existing.fn_ty);
                if (!std.mem.eql(u8, existing_digest.bytes[0..], fn_ty_digest.bytes[0..])) continue;
                try request.ctx.graph.unify(
                    try request.ctx.graph.importMono(fn_template.mono_fn_ty),
                    try request.ctx.graph.importMono(existing.fn_ty),
                );
                try request.ctx.graph.drainDirty();
            }
            switch (existing.status) {
                .ready,
                .lowering,
                => return existing.fn_id,
            }
        }

        const fn_id = try self.program.addFn(fn_template);
        try family_entry.value_ptr.append(self.allocator, .{
            .fn_id = fn_id,
            .fn_ty = fn_template.mono_fn_ty,
            .status = .lowering,
        });

        try request.ctx.constrainTypeToMono(fn_template.source_fn_ty, fn_template.mono_fn_ty);

        const root_node = try request.ctx.instNode(fn_template.source_fn_ty);
        if (!self.unsolved_monos.contains(fn_template.mono_fn_ty)) {
            try request.ctx.graph.addMonoView(root_node, fn_template.mono_fn_ty);
        }
        const live_fn_ty = try request.ctx.graph.monoFor(root_node);
        const lowered = try request.ctx.lowerNestedFunction(request.expr_id, live_fn_ty);
        var def_template = fn_template;
        def_template.mono_fn_ty = live_fn_ty;
        self.program.fns.items[@intFromEnum(fn_id)].source = def_template;
        try self.program.nested_defs.append(self.allocator, .{
            .symbol = self.symbols.fresh(),
            .fn_def = def_template,
            .fn_id = fn_id,
            .args = lowered.args,
            .body = lowered.body,
            .ret = lowered.ret,
        });
        self.markNestedFnReady(family, fn_id, live_fn_ty);
        return fn_id;
    }

    fn markNestedFnReady(self: *Builder, family: NestedFnFamily, fn_id: Ast.FnId, fn_ty: Type.TypeId) void {
        const entries = self.lowered_nested_fns.getPtr(family) orelse
            Common.invariant("lowered nested function family disappeared before completion");
        for (entries.items) |*entry| {
            if (entry.fn_id != fn_id) continue;
            entry.fn_ty = fn_ty;
            entry.status = .ready;
            return;
        }
        Common.invariant("lowered nested function disappeared before completion");
    }

    /// Process the specialization body requests this specialization enqueued
    /// while its body lowered. Its types are final now, so every request's
    /// specialization key is stable.
    fn drainSpecRequests(self: *Builder, graph: *InstGraph) Allocator.Error!void {
        while (graph.deferred_templates.pop()) |request| {
            _ = try self.lowerTemplateWithMonoFor(
                request.template_ref,
                self.moduleForId(request.module),
                request.source_fn_ty,
                request.source_fn_key,
                request.fn_ty,
                graph,
            );
        }
    }

    fn moduleForConstFnDef(self: *Builder, fn_def: anytype) ModuleView {
        return switch (fn_def) {
            .nested => |nested| self.moduleForDigest(names.procTemplateModuleDigest(nested.owner)),
            .parser_runtime => |runtime| self.moduleForDigest(names.procTemplateModuleDigest(runtime.owner)),
            .encode_to_runtime => |runtime| self.moduleForDigest(names.procTemplateModuleDigest(runtime.owner)),
            .local_template,
            .imported_template,
            .checked_generated,
            => |template| self.moduleForDigest(names.procTemplateModuleDigest(template)),
            .local_hosted,
            .imported_hosted,
            => |hosted| self.moduleForDigest(names.procTemplateModuleDigest(hostedTemplate(hosted))),
        };
    }

    fn constFnTemplateToMono(self: *Builder, fn_value: anytype, mono_fn_ty: Type.TypeId) Allocator.Error!Ast.FnTemplate {
        return .{
            .fn_def = try self.constFnDefToMono(fn_value.fn_def),
            .source_fn_ty = fn_value.source_fn_ty,
            .source_fn_key = fn_value.source_fn_key,
            .mono_fn_ty = mono_fn_ty,
        };
    }

    fn constFnDefToMono(self: *Builder, fn_def: anytype) Allocator.Error!Ast.FnDef {
        return switch (fn_def) {
            .local_template => |template| .{ .local_template = template },
            .imported_template => |template| .{ .imported_template = template },
            .nested => |nested| .{ .nested = .{ .owner = nested.owner, .site = nested.site, .context_fn_key = .{} } },
            .local_hosted => |template| .{ .local_hosted = self.hostedFn(template) },
            .imported_hosted => |template| .{ .imported_hosted = self.hostedFn(template) },
            .checked_generated => |template| .{ .checked_generated = template },
            .parser_runtime => |runtime| .{ .parser_runtime = .{
                .owner = runtime.owner,
                .expr = runtime.expr,
            } },
            .encode_to_runtime => |runtime| .{ .encode_to_runtime = .{
                .owner = runtime.owner,
                .expr = runtime.expr,
            } },
        };
    }

    fn restoreConstFnExpr(
        self: *Builder,
        store_view: ModuleView,
        type_view: ModuleView,
        fn_id: checked.ConstFnId,
        ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        const raw = @intFromEnum(fn_id);
        if (raw >= store_view.const_store.fns.items.len) Common.invariant("ConstStore function id is out of range");
        const fn_value = store_view.const_store.getFn(@enumFromInt(raw));
        if (fn_value.fn_def == .parser_runtime) {
            return try self.restoreConstParserRuntimeFnExpr(store_view, fn_value, ty);
        }
        if (fn_value.fn_def == .encode_to_runtime) {
            return try self.restoreConstEncodeToRuntimeFnExpr(store_view, fn_value, ty);
        }
        const template = try self.constFnTemplateToMono(fn_value, ty);
        if (fn_value.captures.len == 0) {
            const mono_fn_id = try self.lowerRestoredConstFnTemplate(type_view, template);
            return try self.program.addExpr(.{
                .ty = self.program.fnSource(mono_fn_id).mono_fn_ty,
                .data = .{ .fn_def = mono_fn_id },
            });
        }

        const fn_view = self.moduleForConstFnDef(fn_value.fn_def);
        const graph = try InstGraph.create(self.allocator, &self.program.types, &self.program.names, &self.unsolved_monos);
        defer graph.destroy();
        const saved_graph = self.active_graph;
        self.active_graph = graph;
        defer self.active_graph = saved_graph;
        var fn_ctx = try BodyContext.init(self.allocator, self, fn_view, ownerTemplateForConstFnDef(fn_value.fn_def), graph);
        defer fn_ctx.deinit();
        try fn_ctx.constrainTypeToMono(fn_value.source_fn_ty, ty);

        const lambda_expr_id = checkedLambdaExprIdForConstFn(fn_view, fn_value.fn_def);
        const lambda_expr = fn_view.bodies.expr(lambda_expr_id);

        const captures = try self.allocator.alloc(struct {
            binder: checked.PatternBinderId,
            previous: ?Ast.LocalId,
            value: Ast.ExprId,
            pat: Ast.PatId,
        }, fn_value.captures.len);
        var initialized: usize = 0;
        errdefer {
            while (initialized > 0) {
                initialized -= 1;
                if (captures[initialized].previous) |previous| {
                    fn_ctx.binders.put(captures[initialized].binder, previous) catch |err| switch (err) {
                        error.OutOfMemory => Common.invariant("restoring a previously inserted binder cannot reallocate"),
                    };
                } else {
                    _ = fn_ctx.binders.remove(captures[initialized].binder);
                }
            }
            self.allocator.free(captures);
        }
        defer self.allocator.free(captures);

        for (fn_value.captures, 0..) |capture, index| {
            const binder = constCaptureBinder(capture.id);
            const capture_ty = checkedBinderType(fn_view, binder);
            const lowered_ty = try fn_ctx.lowerType(capture_ty);
            const value_expr = try fn_ctx.restoreConstNodeAtType(store_view, fn_view, capture.value, lowered_ty);
            const local = try self.program.addLocalWithBinder(self.symbols.fresh(), lowered_ty, binder);
            try bindLocalName(self.program, fn_view, local, binder);
            const pat = try self.program.addPat(.{ .ty = lowered_ty, .data = .{ .bind = local } });
            const previous = fn_ctx.binders.get(binder);
            try fn_ctx.binders.put(binder, local);
            captures[index] = .{
                .binder = binder,
                .previous = previous,
                .value = value_expr,
                .pat = pat,
            };
            initialized += 1;
        }
        defer {
            var index = initialized;
            while (index > 0) {
                index -= 1;
                if (captures[index].previous) |previous| {
                    fn_ctx.binders.put(captures[index].binder, previous) catch |err| switch (err) {
                        error.OutOfMemory => Common.invariant("restoring a previously inserted binder cannot reallocate"),
                    };
                } else {
                    _ = fn_ctx.binders.remove(captures[index].binder);
                }
            }
        }

        var expr = try self.program.addExpr(.{
            .ty = ty,
            .data = switch (lambda_expr.data) {
                .lambda => try fn_ctx.lowerLambdaExpr(lambda_expr_id, template),
                else => Common.invariant("stored capturing function did not reference a checked lambda"),
            },
        });
        var index = captures.len;
        while (index > 0) {
            index -= 1;
            expr = try self.program.addExpr(.{
                .ty = ty,
                .data = .{ .let_ = .{
                    .bind = captures[index].pat,
                    .value = captures[index].value,
                    .rest = expr,
                } },
            });
        }
        try self.drainSpecRequests(graph);
        return expr;
    }

    fn restoreConstParserRuntimeFnExpr(
        self: *Builder,
        store_view: ModuleView,
        fn_value: check.ConstStore.ConstFn,
        ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        const runtime = switch (fn_value.fn_def) {
            .parser_runtime => |runtime| runtime,
            else => Common.invariant("non-parser function reached parser runtime restore"),
        };
        const fn_view = self.moduleForDigest(names.procTemplateModuleDigest(runtime.owner));
        const graph = try InstGraph.create(self.allocator, &self.program.types, &self.program.names, &self.unsolved_monos);
        defer graph.destroy();
        const saved_graph = self.active_graph;
        self.active_graph = graph;
        defer self.active_graph = saved_graph;

        var fn_ctx = try BodyContext.init(self.allocator, self, fn_view, runtime.owner, graph);
        defer fn_ctx.deinit();
        fn_ctx.current_fn_key = fn_value.source_fn_key;

        const expr = fn_view.bodies.expr(runtime.expr);
        const plan = dispatchPlanForRuntimeExpr(fn_view, runtime.expr);
        const plan_args = plan.argsSlice(fn_view.static_dispatch_plans);
        const callable_mono_ty = try fn_ctx.instantiateDispatchPlanCallTypeFromCaller(plan.callable_ty, &fn_ctx, expr.ty, plan_args, ty);
        const fn_data = self.functionShape(callable_mono_ty, "stored parser constructor had a non-function type");
        const arg_tys = try self.allocator.dupe(Type.TypeId, self.program.types.span(fn_data.args));
        defer self.allocator.free(arg_tys);
        if (arg_tys.len != 1) Common.invariant("stored parser constructor had an unexpected arity");
        if (!fn_ctx.sameType(fn_data.ret, ty)) Common.invariant("stored parser constructor result type differed from restored function type");

        const runtime_fn = self.functionShape(ty, "stored parser runtime value had a non-function type");
        const runtime_arg_tys = try self.allocator.dupe(Type.TypeId, self.program.types.span(runtime_fn.args));
        defer self.allocator.free(runtime_arg_tys);
        if (runtime_arg_tys.len != 1) Common.invariant("stored parser runtime function had an unexpected arity");

        const shape_ty = try fn_ctx.lowerType(plan.dispatcher_ty);
        const state_local = try self.program.addLocal(self.symbols.fresh(), runtime_arg_tys[0]);
        const state_expr = try self.localExpr(state_local, runtime_arg_tys[0]);

        var encoding_let: ?struct {
            local: Ast.LocalId,
            value: Ast.ExprId,
        } = null;
        const encoding_expr = if (constGeneratedCaptureNode(fn_value, parserEncodingCaptureId())) |node| blk: {
            const local = try self.program.addLocal(self.symbols.fresh(), arg_tys[0]);
            self.program.setLocalCaptureId(local, parserEncodingCaptureId());
            encoding_let = .{
                .local = local,
                .value = try self.restoreConstNodeAtType(store_view, fn_view, node, arg_tys[0]),
            };
            break :blk try self.localExpr(local, arg_tys[0]);
        } else try fn_ctx.lowerDispatchOperandAtType(plan_args[0], arg_tys[0]);

        const str_ty = try self.primitiveType(.str);
        var precomputed_plan = BodyContext.ParserPrecomputedPlan.init(self.allocator);
        defer precomputed_plan.deinit();
        try fn_ctx.buildParserRestoredPrecomputedPlan(&precomputed_plan, fn_value, store_view, fn_view, shape_ty, str_ty);

        const parsed = try fn_ctx.lowerParseShapeFromState(
            shape_ty,
            encoding_expr,
            arg_tys[0],
            state_expr,
            runtime_arg_tys[0],
            runtime_fn.ret,
            &precomputed_plan,
        );
        const runtime_fn_id = try self.program.addFn(.{
            .fn_def = .{ .parser_runtime = .{
                .owner = runtime.owner,
                .expr = runtime.expr,
            } },
            .source_fn_ty = fn_value.source_fn_ty,
            .source_fn_key = fn_value.source_fn_key,
            .mono_fn_ty = ty,
        });
        var parser_expr = try self.program.addExpr(.{ .ty = ty, .data = .{ .lambda = .{
            .fn_id = runtime_fn_id,
            .args = try self.program.addTypedLocalSpan(&.{
                .{ .local = state_local, .ty = runtime_arg_tys[0] },
            }),
            .body = parsed,
        } } });
        var capture_index = precomputed_plan.captures.items.len;
        while (capture_index > 0) {
            capture_index -= 1;
            const capture = precomputed_plan.captures.items[capture_index];
            parser_expr = try fn_ctx.wrapLet(capture.local, str_ty, capture.value, parser_expr, ty);
        }
        if (encoding_let) |let_| {
            parser_expr = try fn_ctx.wrapLet(let_.local, arg_tys[0], let_.value, parser_expr, ty);
        }

        try self.drainSpecRequests(graph);
        return parser_expr;
    }

    fn restoreConstEncodeToRuntimeFnExpr(
        self: *Builder,
        store_view: ModuleView,
        fn_value: check.ConstStore.ConstFn,
        ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        const runtime = switch (fn_value.fn_def) {
            .encode_to_runtime => |runtime| runtime,
            else => Common.invariant("non-encode_to function reached encode_to runtime restore"),
        };
        const fn_view = self.moduleForDigest(names.procTemplateModuleDigest(runtime.owner));
        const graph = try InstGraph.create(self.allocator, &self.program.types, &self.program.names, &self.unsolved_monos);
        defer graph.destroy();
        const saved_graph = self.active_graph;
        self.active_graph = graph;
        defer self.active_graph = saved_graph;

        var fn_ctx = try BodyContext.init(self.allocator, self, fn_view, runtime.owner, graph);
        defer fn_ctx.deinit();
        fn_ctx.current_fn_key = fn_value.source_fn_key;

        const expr = fn_view.bodies.expr(runtime.expr);
        const plan = dispatchPlanForRuntimeExpr(fn_view, runtime.expr);
        const plan_args = plan.argsSlice(fn_view.static_dispatch_plans);
        const callable_mono_ty = try fn_ctx.instantiateDispatchPlanCallTypeFromCaller(plan.callable_ty, &fn_ctx, expr.ty, plan_args, ty);
        const fn_data = self.functionShape(callable_mono_ty, "stored encode_to constructor had a non-function type");
        const arg_tys = try self.allocator.dupe(Type.TypeId, self.program.types.span(fn_data.args));
        defer self.allocator.free(arg_tys);
        if (arg_tys.len != 2) Common.invariant("stored encode_to constructor had an unexpected arity");
        if (!fn_ctx.sameType(fn_data.ret, ty)) Common.invariant("stored encode_to constructor result type differed from restored function type");

        const runtime_fn = self.functionShape(ty, "stored encode_to runtime value had a non-function type");
        const runtime_arg_tys = try self.allocator.dupe(Type.TypeId, self.program.types.span(runtime_fn.args));
        defer self.allocator.free(runtime_arg_tys);
        if (runtime_arg_tys.len != 1) Common.invariant("stored encode_to runtime function had an unexpected arity");

        const shape_ty = try fn_ctx.lowerType(plan.dispatcher_ty);
        const state_local = try self.program.addLocal(self.symbols.fresh(), runtime_arg_tys[0]);
        const state_expr = try self.localExpr(state_local, runtime_arg_tys[0]);

        var value_let: ?struct {
            local: Ast.LocalId,
            value: Ast.ExprId,
        } = null;
        const value_expr = if (constGeneratedCaptureNode(fn_value, encodeToValueCaptureId())) |node| blk: {
            const local = try self.program.addLocal(self.symbols.fresh(), arg_tys[0]);
            self.program.setLocalCaptureId(local, encodeToValueCaptureId());
            value_let = .{
                .local = local,
                .value = try self.restoreConstNodeAtType(store_view, fn_view, node, arg_tys[0]),
            };
            break :blk try self.localExpr(local, arg_tys[0]);
        } else try fn_ctx.lowerDispatchOperandAtType(plan_args[0], arg_tys[0]);

        var encoding_let: ?struct {
            local: Ast.LocalId,
            value: Ast.ExprId,
        } = null;
        const encoding_expr = if (constGeneratedCaptureNode(fn_value, encodeToEncodingCaptureId())) |node| blk: {
            const local = try self.program.addLocal(self.symbols.fresh(), arg_tys[1]);
            self.program.setLocalCaptureId(local, encodeToEncodingCaptureId());
            encoding_let = .{
                .local = local,
                .value = try self.restoreConstNodeAtType(store_view, fn_view, node, arg_tys[1]),
            };
            break :blk try self.localExpr(local, arg_tys[1]);
        } else try fn_ctx.lowerDispatchOperandAtType(plan_args[1], arg_tys[1]);

        const str_ty = try self.primitiveType(.str);
        var precomputed_plan = BodyContext.ParserPrecomputedPlan.init(self.allocator);
        defer precomputed_plan.deinit();
        precomputed_plan.next_capture_id = encodeToFirstFieldCaptureId();
        try fn_ctx.buildEncodeRestoredPrecomputedPlan(&precomputed_plan, fn_value, store_view, fn_view, shape_ty, str_ty);

        const encoded = try fn_ctx.lowerEncodeShapeToState(
            shape_ty,
            value_expr,
            encoding_expr,
            arg_tys[1],
            state_expr,
            runtime_arg_tys[0],
            runtime_fn.ret,
            &precomputed_plan,
        );
        const runtime_fn_id = try self.program.addFn(.{
            .fn_def = .{ .encode_to_runtime = .{
                .owner = runtime.owner,
                .expr = runtime.expr,
            } },
            .source_fn_ty = fn_value.source_fn_ty,
            .source_fn_key = fn_value.source_fn_key,
            .mono_fn_ty = ty,
        });
        var encoder_expr = try self.program.addExpr(.{ .ty = ty, .data = .{ .lambda = .{
            .fn_id = runtime_fn_id,
            .args = try self.program.addTypedLocalSpan(&.{
                .{ .local = state_local, .ty = runtime_arg_tys[0] },
            }),
            .body = encoded,
        } } });
        var capture_index = precomputed_plan.captures.items.len;
        while (capture_index > 0) {
            capture_index -= 1;
            const capture = precomputed_plan.captures.items[capture_index];
            encoder_expr = try fn_ctx.wrapLet(capture.local, str_ty, capture.value, encoder_expr, ty);
        }
        if (encoding_let) |let_| {
            encoder_expr = try fn_ctx.wrapLet(let_.local, arg_tys[1], let_.value, encoder_expr, ty);
        }
        if (value_let) |let_| {
            encoder_expr = try fn_ctx.wrapLet(let_.local, arg_tys[0], let_.value, encoder_expr, ty);
        }

        try self.drainSpecRequests(graph);
        return encoder_expr;
    }

    fn restoreConstNode(
        self: *Builder,
        store_view: ModuleView,
        type_view: ModuleView,
        node: checked.ConstNodeId,
        checked_ty: checked.CheckedTypeId,
    ) Allocator.Error!Ast.ExprId {
        return try self.restoreConstNodeAtType(store_view, type_view, node, try self.lowerType(type_view, checked_ty));
    }

    fn restoreConstNodeAtType(
        self: *Builder,
        store_view: ModuleView,
        type_view: ModuleView,
        node: checked.ConstNodeId,
        ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        const address = ConstExprAddress{
            .store_module_bytes = store_view.key.bytes,
            .type_module_bytes = type_view.key.bytes,
            .node = @intFromEnum(node),
            .mono_ty = @intFromEnum(ty),
        };
        if (self.const_expr_cache.get(address)) |existing| return existing;

        const value = store_view.const_store.get(node);
        switch (value) {
            .fn_value => |fn_id| {
                const expr = try self.restoreConstFnExpr(store_view, type_view, fn_id, ty);
                try self.const_expr_cache.put(address, expr);
                return expr;
            },
            else => {},
        }
        const data = try self.restoreConstData(store_view, type_view, value, ty);
        const expr = try self.program.addExpr(.{ .ty = ty, .data = data });
        try self.const_expr_cache.put(address, expr);
        return expr;
    }

    fn restoreConstData(
        self: *Builder,
        store_view: ModuleView,
        type_view: ModuleView,
        value: checked.ConstValue,
        ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprData {
        return switch (value) {
            .pending => Common.invariant("pending ConstStore node reached Monotype restore"),
            .zst => .unit,
            .scalar => |scalar| restoreScalar(scalar),
            .str => |str| .{ .str_lit = try self.program.addStringView(
                store_view.const_store.strData(str.data),
                str.offset,
                str.len,
            ) },
            .crash => |str| .{ .crash = try self.program.addStringView(
                store_view.const_store.strData(str.data),
                str.offset,
                str.len,
            ) },
            .list => |items| .{ .list = try self.restoreConstList(store_view, type_view, ty, items) },
            .box => |payload| blk: {
                const child = try self.restoreConstNodeAtType(store_view, type_view, payload, self.constBoxPayloadType(ty));
                break :blk .{ .low_level = .{
                    .op = .box_box,
                    .args = try self.program.addExprSpan(&.{child}),
                } };
            },
            .tuple => |items| .{ .tuple = try self.restoreConstTuple(store_view, type_view, ty, items) },
            .record => |items| .{ .record = try self.restoreConstRecord(store_view, type_view, ty, items) },
            .tag => |tag| .{ .tag = .{
                .name = try self.program.names.internTagLabel(tag.tag_name),
                .payloads = try self.restoreConstTagPayloads(store_view, type_view, ty, tag),
            } },
            .nominal => |nominal| .{ .nominal = try self.restoreConstNodeAtType(store_view, type_view, nominal.backing, self.namedBackingType(ty) orelse ty) },
            .fn_value => Common.invariant("ConstStore function value must be restored as an expression"),
        };
    }

    fn restoreConstList(
        self: *Builder,
        store_view: ModuleView,
        type_view: ModuleView,
        ty: Type.TypeId,
        items: []const checked.ConstNodeId,
    ) Allocator.Error!Ast.Span(Ast.ExprId) {
        const elem_ty = self.constListElemType(ty);
        const lowered = try self.allocator.alloc(Ast.ExprId, items.len);
        defer self.allocator.free(lowered);
        for (items, 0..) |item, index| {
            lowered[index] = try self.restoreConstNodeAtType(store_view, type_view, item, elem_ty);
        }
        return try self.program.addExprSpan(lowered);
    }

    fn restoreConstTuple(
        self: *Builder,
        store_view: ModuleView,
        type_view: ModuleView,
        ty: Type.TypeId,
        items: []const checked.ConstNodeId,
    ) Allocator.Error!Ast.Span(Ast.ExprId) {
        const item_span = self.tupleItemSpan(ty);
        const item_count: usize = @intCast(item_span.len);
        if (item_count != items.len) Common.invariant("ConstStore tuple length differs from checked type");
        const lowered = try self.allocator.alloc(Ast.ExprId, items.len);
        defer self.allocator.free(lowered);
        for (items, 0..) |item, index| {
            const item_ty = self.program.types.span(item_span)[index];
            lowered[index] = try self.restoreConstNodeAtType(store_view, type_view, item, item_ty);
        }
        return try self.program.addExprSpan(lowered);
    }

    fn restoreConstRecord(
        self: *Builder,
        store_view: ModuleView,
        type_view: ModuleView,
        ty: Type.TypeId,
        items: []const checked.ConstNodeId,
    ) Allocator.Error!Ast.Span(Ast.FieldExpr) {
        const field_span = self.recordFieldsSpan(ty);
        const field_count: usize = @intCast(field_span.len);
        if (field_count != items.len) Common.invariant("ConstStore record length differs from checked type");
        const lowered = try self.allocator.alloc(Ast.FieldExpr, items.len);
        defer self.allocator.free(lowered);
        for (items, 0..) |item, index| {
            const field = self.program.types.fieldSpan(field_span)[index];
            lowered[index] = .{
                .name = field.name,
                .value = try self.restoreConstNodeAtType(store_view, type_view, item, field.ty),
            };
        }
        return try self.program.addFieldExprSpan(lowered);
    }

    fn restoreConstTagPayloads(
        self: *Builder,
        store_view: ModuleView,
        type_view: ModuleView,
        ty: Type.TypeId,
        tag: anytype,
    ) Allocator.Error!Ast.Span(Ast.ExprId) {
        const mono_tag_name = try self.program.names.internTagLabel(tag.tag_name);
        const payload_span = self.tagPayloadSpan(ty, mono_tag_name);
        const payload_count: usize = @intCast(payload_span.len);
        if (payload_count != tag.payloads.len) Common.invariant("ConstStore tag payload count differs from checked type");
        const lowered = try self.allocator.alloc(Ast.ExprId, tag.payloads.len);
        defer self.allocator.free(lowered);
        for (tag.payloads, 0..) |payload, index| {
            const payload_ty = self.program.types.span(payload_span)[index];
            lowered[index] = try self.restoreConstNodeAtType(store_view, type_view, payload, payload_ty);
        }
        return try self.program.addExprSpan(lowered);
    }

    fn constListElemType(self: *Builder, ty: Type.TypeId) Type.TypeId {
        return switch (self.shapeContent(ty)) {
            .list => |elem| elem,
            else => Common.invariant("ConstStore list restored with a non-list monotype"),
        };
    }

    fn constBoxPayloadType(self: *Builder, ty: Type.TypeId) Type.TypeId {
        return switch (self.shapeContent(ty)) {
            .box => |payload| payload,
            else => Common.invariant("ConstStore box restored with a non-box monotype"),
        };
    }

    fn constRecordFields(self: *Builder, ty: Type.TypeId) []const Type.Field {
        return self.program.types.fieldSpan(self.recordFieldsSpan(ty));
    }

    fn inspectCall(self: *Builder, value: Ast.ExprId, value_ty: Type.TypeId, str_ty: Type.TypeId) Allocator.Error!Ast.ExprId {
        const def_id = try self.inspectDefForType(value_ty, str_ty);
        const fn_ty = try self.oneArgFnType(value_ty, str_ty);
        const callee = try self.program.addExpr(.{
            .ty = fn_ty,
            .data = .{ .def_ref = def_id },
        });
        const args = [_]Ast.ExprId{value};
        return try self.program.addExpr(.{
            .ty = str_ty,
            .data = .{ .call_value = .{
                .callee = callee,
                .args = try self.program.addExprSpan(&args),
            } },
        });
    }

    fn inspectDefForType(self: *Builder, value_ty: Type.TypeId, str_ty: Type.TypeId) Allocator.Error!Ast.DefId {
        const address = GeneratedHelperDefAddress{
            .value_ty = @intFromEnum(value_ty),
            .result_ty = @intFromEnum(str_ty),
        };
        if (self.inspect_defs.get(address)) |entry| return entry.id();

        const def_id: Ast.DefId = @enumFromInt(@as(u32, @intCast(self.program.defs.items.len)));
        try self.program.defs.append(self.allocator, undefined);
        try self.inspect_defs.put(address, .{ .reserved = def_id });

        const arg_local = try self.program.addLocal(self.symbols.fresh(), value_ty);
        const arg_expr = try self.localExpr(arg_local, value_ty);
        const body = try self.inspectBody(arg_expr, value_ty, value_ty, str_ty);

        const args = try self.program.addTypedLocalSpan(&.{.{ .local = arg_local, .ty = value_ty }});
        self.program.defs.items[@intFromEnum(def_id)] = .{
            .symbol = self.symbols.fresh(),
            .fn_def = null,
            .args = args,
            .body = .{ .roc = body },
            .ret = str_ty,
        };
        try self.inspect_defs.put(address, .{ .ready = def_id });
        return def_id;
    }

    fn inspectBody(
        self: *Builder,
        value: Ast.ExprId,
        value_ty: Type.TypeId,
        shape_ty: Type.TypeId,
        str_ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        return switch (self.program.types.get(shape_ty)) {
            .primitive => |primitive| try self.primitiveInspect(value, primitive, str_ty),
            .named => |named| blk: {
                if (named.builtin_owner) |owner| {
                    switch (owner) {
                        .list => break :blk try self.inspectList(value, self.singleTypeArg(named.args, "List"), str_ty),
                        .box => {},
                        else => {},
                    }
                }
                if (try self.toInspectCall(value, value_ty, str_ty)) |method_call| break :blk method_call;
                const backing = named.backing orelse Common.invariant("Str.inspect reached opaque named type without checked inspect authority");
                if (backing.use != .inspectable) {
                    break :blk try self.stringExpr("<opaque>", str_ty);
                }
                break :blk try self.inspectBody(value, value_ty, backing.ty, str_ty);
            },
            .record => |fields| try self.inspectRecord(value, self.program.types.fieldSpan(fields), str_ty),
            .tuple => |items| try self.inspectTuple(value, self.program.types.span(items), str_ty),
            .tag_union => |tags| try self.inspectTagUnion(value, value_ty, self.program.types.tagSpan(tags), str_ty),
            .list => |elem_ty| try self.inspectList(value, elem_ty, str_ty),
            .func, .erased => try self.stringExpr("<function>", str_ty),
            .zst => try self.stringExpr("{}", str_ty),
            .box => |elem_ty| blk: {
                const unboxed = try self.lowLevelExpr(.box_unbox, &.{value}, elem_ty);
                var out = try self.stringExpr("Box(", str_ty);
                out = try self.concatExpr(out, try self.inspectCall(unboxed, elem_ty, str_ty), str_ty);
                break :blk try self.concatExpr(out, try self.stringExpr(")", str_ty), str_ty);
            },
        };
    }

    fn toInspectCall(self: *Builder, value: Ast.ExprId, value_ty: Type.TypeId, str_ty: Type.TypeId) Allocator.Error!?Ast.ExprId {
        const owner = methodOwnerFromType(&self.program.types, value_ty) orelse return null;
        const lookup = self.lookupMethodTargetByName(owner, "to_inspect") orelse return null;
        const procedure = switch (lookup.target.kind) {
            .procedure => |procedure| procedure,
            .local_proc => return null,
        };
        const template = procedure.template;

        const graph = try InstGraph.create(self.allocator, &self.program.types, &self.program.names, &self.unsolved_monos);
        defer graph.destroy();
        const saved_graph = self.active_graph;
        self.active_graph = graph;
        defer self.active_graph = saved_graph;
        var target_ctx = try BodyContext.init(self.allocator, self, lookup.view, template, graph);
        defer target_ctx.deinit();
        const callable_mono_ty = try target_ctx.instantiateTargetCallTypeFromMonoArgs(lookup.target.callable_ty, &.{value_ty}, str_ty);
        const callee_def = try self.lowerTemplateWithMono(
            template,
            lookup.view,
            lookup.target.callable_ty,
            lookup.view.types.rootKey(lookup.target.callable_ty),
            callable_mono_ty,
        );

        const args = [_]Ast.ExprId{value};
        const call = try self.program.addExpr(.{ .ty = str_ty, .data = .{ .call_proc = .{
            .callee = .{ .func = self.defFnId(callee_def) },
            .args = try self.program.addExprSpan(&args),
        } } });
        try self.drainSpecRequests(graph);
        return call;
    }

    fn primitiveInspect(self: *Builder, value: Ast.ExprId, primitive: Type.Primitive, str_ty: Type.TypeId) Allocator.Error!Ast.ExprId {
        const args = [_]Ast.ExprId{value};
        return try self.lowLevelExpr(primitiveInspectLowLevelOp(primitive), &args, str_ty);
    }

    fn inspectTuple(self: *Builder, value: Ast.ExprId, items: []const Type.TypeId, str_ty: Type.TypeId) Allocator.Error!Ast.ExprId {
        if (items.len == 0) return try self.stringExpr("()", str_ty);
        const stable_items = try self.allocator.dupe(Type.TypeId, items);
        defer self.allocator.free(stable_items);

        var out = try self.stringExpr("(", str_ty);
        for (stable_items, 0..) |item_ty, i| {
            if (i != 0) out = try self.concatExpr(out, try self.stringExpr(", ", str_ty), str_ty);
            const item = try self.program.addExpr(.{
                .ty = item_ty,
                .data = .{ .tuple_access = .{ .tuple = value, .elem_index = @intCast(i) } },
            });
            out = try self.concatExpr(out, try self.inspectCall(item, item_ty, str_ty), str_ty);
        }
        return try self.concatExpr(out, try self.stringExpr(")", str_ty), str_ty);
    }

    fn inspectRecord(self: *Builder, value: Ast.ExprId, fields: []const Type.Field, str_ty: Type.TypeId) Allocator.Error!Ast.ExprId {
        if (fields.len == 0) return try self.stringExpr("{}", str_ty);
        const stable_fields = try self.allocator.dupe(Type.Field, fields);
        defer self.allocator.free(stable_fields);

        var out = try self.stringExpr("{ ", str_ty);
        for (stable_fields, 0..) |field, i| {
            if (i != 0) out = try self.concatExpr(out, try self.stringExpr(", ", str_ty), str_ty);
            out = try self.concatExpr(out, try self.stringExpr(self.program.names.recordFieldLabelText(field.name), str_ty), str_ty);
            out = try self.concatExpr(out, try self.stringExpr(": ", str_ty), str_ty);
            const field_value = try self.program.addExpr(.{
                .ty = field.ty,
                .data = .{ .field_access = .{ .receiver = value, .field = field.name } },
            });
            out = try self.concatExpr(out, try self.inspectCall(field_value, field.ty, str_ty), str_ty);
        }
        return try self.concatExpr(out, try self.stringExpr(" }", str_ty), str_ty);
    }

    fn inspectTagUnion(self: *Builder, value: Ast.ExprId, value_ty: Type.TypeId, tags: []const Type.Tag, str_ty: Type.TypeId) Allocator.Error!Ast.ExprId {
        if (tags.len == 0) {
            const msg = try self.program.addStringLiteral("uninhabited value reached Str.inspect");
            return try self.program.addExpr(.{
                .ty = str_ty,
                .data = .{ .crash = msg },
            });
        }
        const stable_tags = try self.allocator.dupe(Type.Tag, tags);
        defer self.allocator.free(stable_tags);

        const branches = try self.allocator.alloc(Ast.Branch, stable_tags.len);
        defer self.allocator.free(branches);

        for (stable_tags, 0..) |tag, i| {
            const payload_tys = try self.allocator.dupe(Type.TypeId, self.program.types.span(tag.payloads));
            defer self.allocator.free(payload_tys);
            const payload_pats = try self.allocator.alloc(Ast.PatId, payload_tys.len);
            defer self.allocator.free(payload_pats);
            const payload_exprs = try self.allocator.alloc(Ast.ExprId, payload_tys.len);
            defer self.allocator.free(payload_exprs);

            for (payload_tys, 0..) |payload_ty, payload_i| {
                const local = try self.program.addLocal(self.symbols.fresh(), payload_ty);
                payload_pats[payload_i] = try self.program.addPat(.{ .ty = payload_ty, .data = .{ .bind = local } });
                payload_exprs[payload_i] = try self.localExpr(local, payload_ty);
            }

            const pat = try self.program.addPat(.{
                .ty = value_ty,
                .data = .{ .tag = .{
                    .name = tag.name,
                    .payloads = try self.program.addPatSpan(payload_pats),
                } },
            });
            branches[i] = .{
                .pat = pat,
                .body = try self.inspectTagBody(tag.name, payload_exprs, payload_tys, str_ty),
            };
        }

        return try self.program.addExpr(.{
            .ty = str_ty,
            .data = .{ .match_ = .{
                .scrutinee = value,
                .branches = try self.program.addBranchSpan(branches),
            } },
        });
    }

    fn inspectTagBody(
        self: *Builder,
        name: names.TagNameId,
        payload_exprs: []const Ast.ExprId,
        payload_tys: []const Type.TypeId,
        str_ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        var out = try self.stringExpr(self.program.names.tagLabelText(name), str_ty);
        if (payload_exprs.len == 0) return out;
        out = try self.concatExpr(out, try self.stringExpr("(", str_ty), str_ty);
        for (payload_exprs, payload_tys, 0..) |payload_expr, payload_ty, i| {
            if (i != 0) out = try self.concatExpr(out, try self.stringExpr(", ", str_ty), str_ty);
            out = try self.concatExpr(out, try self.inspectCall(payload_expr, payload_ty, str_ty), str_ty);
        }
        return try self.concatExpr(out, try self.stringExpr(")", str_ty), str_ty);
    }

    fn inspectList(self: *Builder, value: Ast.ExprId, elem_ty: Type.TypeId, str_ty: Type.TypeId) Allocator.Error!Ast.ExprId {
        const u64_ty = try self.primitiveType(.u64);
        const bool_ty = try self.primitiveType(.bool);

        const len_local = try self.program.addLocal(self.symbols.fresh(), u64_ty);
        const len_pat = try self.bindPat(len_local, u64_ty);
        const len_value = try self.lowLevelExpr(.list_len, &.{value}, u64_ty);
        const len_expr = try self.localExpr(len_local, u64_ty);

        const index_local = try self.program.addLocal(self.symbols.fresh(), u64_ty);
        const out_local = try self.program.addLocal(self.symbols.fresh(), str_ty);
        const index_expr = try self.localExpr(index_local, u64_ty);
        const out_expr = try self.localExpr(out_local, str_ty);

        const done_cond = try self.lowLevelExpr(.num_is_eq, &.{ index_expr, len_expr }, bool_ty);
        const finish = try self.program.addExpr(.{
            .ty = str_ty,
            .data = .{ .break_ = try self.concatExpr(out_expr, try self.stringExpr("]", str_ty), str_ty) },
        });
        const step = try self.inspectListStep(value, elem_ty, str_ty, index_local, out_local);
        const body = try self.ifExpr(done_cond, finish, step, str_ty);

        const params = [_]Ast.TypedLocal{
            .{ .local = index_local, .ty = u64_ty },
            .{ .local = out_local, .ty = str_ty },
        };
        const initial_values = [_]Ast.ExprId{
            try self.intLiteralExpr(0, u64_ty),
            try self.stringExpr("[", str_ty),
        };
        const loop = try self.program.addExpr(.{
            .ty = str_ty,
            .data = .{ .loop_ = .{
                .params = try self.program.addTypedLocalSpan(&params),
                .initial_values = try self.program.addExprSpan(&initial_values),
                .body = body,
            } },
        });
        return try self.program.addExpr(.{
            .ty = str_ty,
            .data = .{ .let_ = .{
                .bind = len_pat,
                .value = len_value,
                .rest = loop,
            } },
        });
    }

    fn inspectListStep(
        self: *Builder,
        list_value: Ast.ExprId,
        elem_ty: Type.TypeId,
        str_ty: Type.TypeId,
        index_local: Ast.LocalId,
        out_local: Ast.LocalId,
    ) Allocator.Error!Ast.ExprId {
        const u64_ty = try self.primitiveType(.u64);
        const bool_ty = try self.primitiveType(.bool);
        const index_expr = try self.localExpr(index_local, u64_ty);
        const out_expr = try self.localExpr(out_local, str_ty);

        const first_cond = try self.lowLevelExpr(.num_is_eq, &.{ index_expr, try self.intLiteralExpr(0, u64_ty) }, bool_ty);
        const sep = try self.ifExpr(first_cond, try self.stringExpr("", str_ty), try self.stringExpr(", ", str_ty), str_ty);
        const elem = try self.lowLevelExpr(.list_get_unsafe, &.{ list_value, index_expr }, elem_ty);
        const elem_str = try self.inspectCall(elem, elem_ty, str_ty);
        const with_sep = try self.concatExpr(out_expr, sep, str_ty);
        const next_out = try self.concatExpr(with_sep, elem_str, str_ty);
        const next_index = try self.lowLevelExpr(.num_plus, &.{ index_expr, try self.intLiteralExpr(1, u64_ty) }, u64_ty);
        return try self.program.addExpr(.{
            .ty = str_ty,
            .data = .{ .continue_ = .{ .values = try self.program.addExprSpan(&.{ next_index, next_out }) } },
        });
    }

    fn primitiveType(self: *Builder, primitive: Type.Primitive) Allocator.Error!Type.TypeId {
        return switch (primitive) {
            .u64 => blk: {
                if (self.u64_ty) |ty| break :blk ty;
                const ty = try self.program.types.add(.{ .primitive = .u64 });
                self.u64_ty = ty;
                break :blk ty;
            },
            .bool => blk: {
                if (self.bool_ty) |ty| break :blk ty;
                const ty = try self.program.types.add(.{ .primitive = .bool });
                self.bool_ty = ty;
                break :blk ty;
            },
            else => try self.program.types.add(.{ .primitive = primitive }),
        };
    }

    fn oneArgFnType(self: *Builder, arg_ty: Type.TypeId, ret_ty: Type.TypeId) Allocator.Error!Type.TypeId {
        const args = [_]Type.TypeId{arg_ty};
        return try self.program.types.add(.{ .func = .{
            .args = try self.program.types.addSpan(&args),
            .ret = ret_ty,
        } });
    }

    fn twoArgFnType(self: *Builder, arg_ty: Type.TypeId, ret_ty: Type.TypeId) Allocator.Error!Type.TypeId {
        const args = [_]Type.TypeId{ arg_ty, arg_ty };
        return try self.program.types.add(.{ .func = .{
            .args = try self.program.types.addSpan(&args),
            .ret = ret_ty,
        } });
    }

    /// `(value, Hasher) -> Hasher`, the shape of a `to_hash` helper.
    fn hashFnType(self: *Builder, value_ty: Type.TypeId, hasher_ty: Type.TypeId) Allocator.Error!Type.TypeId {
        const args = [_]Type.TypeId{ value_ty, hasher_ty };
        return try self.program.types.add(.{ .func = .{
            .args = try self.program.types.addSpan(&args),
            .ret = hasher_ty,
        } });
    }

    fn singleTypeArg(self: *Builder, span: Type.Span, comptime owner: []const u8) Type.TypeId {
        const args = self.program.types.span(span);
        if (args.len != 1) Common.invariant(owner ++ " type reached Monotype inspect lowering without one type argument");
        return args[0];
    }

    fn localExpr(self: *Builder, local: Ast.LocalId, ty: Type.TypeId) Allocator.Error!Ast.ExprId {
        return try self.program.addExpr(.{ .ty = ty, .data = .{ .local = local } });
    }

    fn bindPat(self: *Builder, local: Ast.LocalId, ty: Type.TypeId) Allocator.Error!Ast.PatId {
        return try self.program.addPat(.{ .ty = ty, .data = .{ .bind = local } });
    }

    fn stringExpr(self: *Builder, text: []const u8, str_ty: Type.TypeId) Allocator.Error!Ast.ExprId {
        return try self.program.addExpr(.{
            .ty = str_ty,
            .data = .{ .str_lit = try self.program.addStringLiteral(text) },
        });
    }

    fn intLiteralExpr(self: *Builder, value: u64, ty: Type.TypeId) Allocator.Error!Ast.ExprId {
        return try self.program.addExpr(.{
            .ty = ty,
            .data = .{ .int_lit = unsignedIntLiteral(value) },
        });
    }

    fn lowLevelExpr(self: *Builder, op: can.CIR.Expr.LowLevel, args: []const Ast.ExprId, ret_ty: Type.TypeId) Allocator.Error!Ast.ExprId {
        return try self.program.addExpr(.{
            .ty = ret_ty,
            .data = .{ .low_level = .{
                .op = op,
                .args = try self.program.addExprSpan(args),
            } },
        });
    }

    fn concatExpr(self: *Builder, left: Ast.ExprId, right: Ast.ExprId, str_ty: Type.TypeId) Allocator.Error!Ast.ExprId {
        return try self.lowLevelExpr(.str_concat, &.{ left, right }, str_ty);
    }

    fn ifExpr(self: *Builder, cond: Ast.ExprId, then_expr: Ast.ExprId, else_expr: Ast.ExprId, ty: Type.TypeId) Allocator.Error!Ast.ExprId {
        const branches = [_]Ast.IfBranch{.{ .cond = cond, .body = then_expr }};
        return try self.program.addExpr(.{
            .ty = ty,
            .data = .{ .if_ = .{
                .branches = try self.program.addIfBranchSpan(&branches),
                .final_else = else_expr,
            } },
        });
    }
};

const LoweredTemplateBody = struct {
    args: Ast.Span(Ast.TypedLocal),
    body: Ast.ExprId,
    ret: Type.TypeId,
};

const BinderRestore = struct {
    binder: checked.PatternBinderId,
    previous: ?Ast.LocalId,
};

const CollectedListPattern = struct {
    local: Ast.LocalId,
    ty: Type.TypeId,
    patterns: []const checked.CheckedPatternId,
    rest: ?checked.CheckedListRestPattern,
};

const BodyContext = struct {
    allocator: Allocator,
    builder: *Builder,
    view: ModuleView,
    owner_template: names.ProcTemplate,
    owner_context_fn_key: names.TypeDigest,
    current_fn_key: names.TypeDigest,
    comptime_exhaustiveness_depth: u32,
    binders: BinderMap,
    local_proc_contexts: std.AutoHashMap(checked.PatternBinderId, names.TypeDigest),
    /// This specialization's type solver, shared by every instantiation
    /// context created while lowering the same specialization.
    graph: *InstGraph,
    /// Instantiation cache: checked types this context already cloned into the
    /// graph. Separate per context so re-instantiating a generic signature at
    /// another call site creates fresh nodes.
    node_map: std.AutoHashMap(CheckedTypeAddress, NodeId),
    /// Innermost-last stack of nominal-instance instantiation scopes; see
    /// instNominalBackingNode.
    decl_scopes: std.ArrayList(*std.AutoHashMap(CheckedTypeAddress, NodeId)) = .empty,
    string_literals: []?Ast.StringLiteralId,
    loop_contexts: std.ArrayList(LoopContext),
    /// Literal sub-patterns on non-builtin number types collected while
    /// lowering a match branch's pattern; each becomes an equality condition
    /// on the branch comparing the bound value against the literal's
    /// `from_numeral`-converted constant.
    pattern_literal_guards: std.ArrayList(PatternLiteralGuard),
    /// Structural equality types currently being expanded in this context.
    /// Seeing the same type again is a real recursive edge, which lowers to a
    /// reserved generated equality helper instead of recursively expanding AST.
    equality_expansion_stack: std.AutoHashMap(Type.TypeId, void),
    /// Structural hash types currently being expanded in this context.
    /// Seeing the same type again is a real recursive edge, which lowers to a
    /// reserved generated hash helper instead of recursively expanding AST.
    hash_expansion_stack: std.AutoHashMap(Type.TypeId, void),
    /// Source region to use while inlining a compile-time const eval template.
    /// The template body can be lowered from a lookup site, but diagnostics
    /// must point at the original const root.
    source_region_override: ?base.Region = null,

    const PatternLiteralGuard = struct {
        local: Ast.LocalId,
        conversion: checked.CheckedExprId,
        ty: Type.TypeId,
    };

    const ParserPrecomputedRecord = struct {
        renamed_field_locals: []Ast.LocalId,
        renamed_field_values: []Ast.ExprId,
        renamed_field_lengths: ?[]u32,
        renamed_field_texts: ?[][]const u8,
    };

    const ParserPrecomputedCapture = struct {
        local: Ast.LocalId,
        value: Ast.ExprId,
    };

    const ParserPrecomputedPlan = struct {
        allocator: Allocator,
        records: std.AutoHashMap(Type.TypeId, ParserPrecomputedRecord),
        captures: std.ArrayList(ParserPrecomputedCapture),
        next_capture_id: usize,

        fn init(allocator: Allocator) ParserPrecomputedPlan {
            return .{
                .allocator = allocator,
                .records = std.AutoHashMap(Type.TypeId, ParserPrecomputedRecord).init(allocator),
                .captures = .empty,
                .next_capture_id = 1,
            };
        }

        fn deinit(self: *ParserPrecomputedPlan) void {
            var records = self.records.valueIterator();
            while (records.next()) |record| {
                self.allocator.free(record.renamed_field_locals);
                self.allocator.free(record.renamed_field_values);
                if (record.renamed_field_lengths) |lengths| self.allocator.free(lengths);
                if (record.renamed_field_texts) |texts| self.allocator.free(texts);
            }
            self.records.deinit();
            self.captures.deinit(self.allocator);
        }
    };

    const MaterializedArg = struct {
        pattern: checked.CheckedPatternId,
        value: Ast.ExprId,
        ty: Type.TypeId,
    };

    const ParseRecordSlots = struct {
        presence_locals: []const Ast.LocalId,
        presence_tys: []const Type.TypeId,
        payload_locals: []const Ast.LocalId,
        payload_tys: []const Type.TypeId,

        fn fieldCount(self: ParseRecordSlots) usize {
            return self.payload_locals.len;
        }
    };

    const BindingContinuation = union(enum) {
        expr: Ast.ExprId,
        checked_expr: checked.CheckedExprId,
        materialized_args: struct {
            args: []const MaterializedArg,
            index: usize,
            body: checked.CheckedExprId,
        },
        iterator_body: struct {
            body: checked.CheckedExprId,
            result_ty: Type.TypeId,
            rest_expr: Ast.ExprId,
            carries: []const LoopCarry,
        },
    };

    const CurrentLocal = struct {
        binder: checked.PatternBinderId,
        local: Ast.LocalId,
    };

    fn init(
        allocator: Allocator,
        builder: *Builder,
        view: ModuleView,
        owner_template: names.ProcTemplate,
        graph: *InstGraph,
    ) Allocator.Error!BodyContext {
        const string_literals = try allocator.alloc(?Ast.StringLiteralId, view.bodies.stringLiteralCount());
        errdefer allocator.free(string_literals);
        @memset(string_literals, null);
        return .{
            .allocator = allocator,
            .builder = builder,
            .view = view,
            .owner_template = owner_template,
            .owner_context_fn_key = .{},
            .current_fn_key = .{},
            .comptime_exhaustiveness_depth = 0,
            .binders = BinderMap.init(allocator),
            .local_proc_contexts = std.AutoHashMap(checked.PatternBinderId, names.TypeDigest).init(allocator),
            .graph = graph,
            .node_map = std.AutoHashMap(CheckedTypeAddress, NodeId).init(allocator),
            .string_literals = string_literals,
            .loop_contexts = .empty,
            .pattern_literal_guards = .empty,
            .equality_expansion_stack = std.AutoHashMap(Type.TypeId, void).init(allocator),
            .hash_expansion_stack = std.AutoHashMap(Type.TypeId, void).init(allocator),
        };
    }

    fn deinit(self: *BodyContext) void {
        self.hash_expansion_stack.deinit();
        self.equality_expansion_stack.deinit();
        self.pattern_literal_guards.deinit(self.allocator);
        self.loop_contexts.deinit(self.allocator);
        self.allocator.free(self.string_literals);
        self.decl_scopes.deinit(self.allocator);
        self.node_map.deinit();
        self.local_proc_contexts.deinit();
        self.binders.deinit();
    }

    fn childContext(self: *BodyContext, current_fn_key: names.TypeDigest) Allocator.Error!BodyContext {
        return try self.childContextWithTypeCells(current_fn_key, true);
    }

    fn nestedInstantiationContext(self: *BodyContext, current_fn_key: names.TypeDigest) Allocator.Error!BodyContext {
        var child = try self.childContextWithTypeCells(current_fn_key, false);
        errdefer child.deinit();
        try child.constrainCopiedBinderTypes();
        return child;
    }

    fn childContextWithTypeCells(
        self: *BodyContext,
        current_fn_key: names.TypeDigest,
        copy_type_cells: bool,
    ) Allocator.Error!BodyContext {
        var child = try BodyContext.init(self.allocator, self.builder, self.view, self.owner_template, self.graph);
        errdefer child.deinit();
        child.owner_context_fn_key = self.owner_context_fn_key;
        child.current_fn_key = current_fn_key;
        child.comptime_exhaustiveness_depth = self.comptime_exhaustiveness_depth;

        var binder_iter = self.binders.iterator();
        while (binder_iter.next()) |entry| {
            try child.binders.put(entry.key_ptr.*, entry.value_ptr.*);
        }

        var proc_iter = self.local_proc_contexts.iterator();
        while (proc_iter.next()) |entry| {
            try child.local_proc_contexts.put(entry.key_ptr.*, entry.value_ptr.*);
        }

        if (copy_type_cells) {
            var node_iter = self.node_map.iterator();
            while (node_iter.next()) |entry| {
                try child.node_map.put(entry.key_ptr.*, entry.value_ptr.*);
            }
        }

        try child.loop_contexts.appendSlice(child.allocator, self.loop_contexts.items);

        return child;
    }

    fn constrainCopiedBinderTypes(self: *BodyContext) Allocator.Error!void {
        var binder_iter = self.binders.iterator();
        while (binder_iter.next()) |entry| {
            const local = entry.value_ptr.*;
            const local_ty = self.builder.program.locals.items[@intFromEnum(local)].ty;
            try self.constrainTypeToMono(checkedBinderType(self.view, entry.key_ptr.*), local_ty);
        }
    }

    /// Constrain a checked type to a Monotype: instantiate the checked type
    /// into the graph and unify with the (linked or imported) Monotype node.
    /// Pending view refills drain once the outermost constraint returns.
    fn constrainTypeToMono(
        self: *BodyContext,
        checked_ty: checked.CheckedTypeId,
        mono_ty: Type.TypeId,
    ) Allocator.Error!void {
        self.builder.constrain_depth += 1;
        defer self.builder.constrain_depth -= 1;
        try self.graph.unify(try self.instNode(checked_ty), try self.graph.importMono(mono_ty));
        if (self.builder.constrain_depth == 1) try self.graph.drainDirty();
    }

    /// Constrain two checked types from (possibly different) instantiation
    /// contexts of the same specialization to denote one type.
    fn constrainCheckedTypeRelations(
        self: *BodyContext,
        left_ty: checked.CheckedTypeId,
        right_ctx: *BodyContext,
        right_ty: checked.CheckedTypeId,
    ) Allocator.Error!void {
        if (self.graph != right_ctx.graph) {
            Common.invariant("checked type relation crossed specialization graphs");
        }
        self.builder.constrain_depth += 1;
        defer self.builder.constrain_depth -= 1;
        try self.graph.unify(try self.instNode(left_ty), try right_ctx.instNode(right_ty));
        if (self.builder.constrain_depth == 1) try self.graph.drainDirty();
    }

    fn monoAliasBacking(types: *const Type.Store, mono_ty: Type.TypeId) ?Type.TypeId {
        return switch (types.get(mono_ty)) {
            .named => |named| if (named.kind == .alias) blk: {
                const backing = named.backing orelse break :blk null;
                break :blk backing.ty;
            } else null,
            else => null,
        };
    }

    fn constrainKnownType(
        self: *BodyContext,
        checked_ty: checked.CheckedTypeId,
        mono_ty: Type.TypeId,
    ) Allocator.Error!void {
        try self.constrainTypeToMono(checked_ty, mono_ty);
    }

    fn typeAddress(self: *BodyContext, checked_ty: checked.CheckedTypeId) CheckedTypeAddress {
        return checkedTypeAddress(self.view, checked_ty);
    }

    fn lowerType(self: *BodyContext, checked_ty: checked.CheckedTypeId) Allocator.Error!Type.TypeId {
        return try self.graph.monoFor(try self.instNode(checked_ty));
    }

    /// Instantiate a checked type into this specialization's graph, caching by
    /// checked identity so every occurrence of the same checked root resolves
    /// to the same node within this instantiation context.
    fn instNode(self: *BodyContext, checked_ty: checked.CheckedTypeId) Allocator.Error!NodeId {
        // A checked empty tag union carries no identity worth sharing: it
        // records that nothing reaches a slot, and the slot yields to sibling
        // descriptions. One checked id serves many unrelated slots, so each
        // occurrence instantiates independently.
        switch (checkedPayload(self.view, checked_ty)) {
            .empty_tag_union => return try self.graph.newNode(.{ .unresolved = .{ .row_default = .empty_tag_union } }),
            else => {},
        }
        const address = self.typeAddress(checked_ty);
        if (self.scopedNode(address)) |existing| return existing;
        const placeholder = try self.graph.newNode(.{ .unresolved = .{} });
        try self.putScopedNode(address, placeholder);
        const built = try self.instNodeContent(checked_ty);
        try self.graph.unify(placeholder, built);
        return placeholder;
    }

    fn scopedNode(self: *BodyContext, address: CheckedTypeAddress) ?NodeId {
        var index = self.decl_scopes.items.len;
        while (index > 0) {
            index -= 1;
            if (self.decl_scopes.items[index].get(address)) |existing| return existing;
        }
        return self.node_map.get(address);
    }

    fn putScopedNode(self: *BodyContext, address: CheckedTypeAddress, node: NodeId) Allocator.Error!void {
        if (self.decl_scopes.items.len != 0) {
            try self.decl_scopes.items[self.decl_scopes.items.len - 1].put(address, node);
            return;
        }
        try self.node_map.put(address, node);
    }

    fn instNodeSlice(self: *BodyContext, checked_tys: []const checked.CheckedTypeId) Allocator.Error![]NodeId {
        const out = try self.graph.arena().alloc(NodeId, checked_tys.len);
        for (checked_tys, 0..) |checked_ty, index| {
            out[index] = try self.instNode(checked_ty);
        }
        return out;
    }

    fn instNodeContent(self: *BodyContext, checked_ty: checked.CheckedTypeId) Allocator.Error!NodeId {
        return switch (checkedPayload(self.view, checked_ty)) {
            .pending => Common.invariant("pending checked type reached Monotype instantiation"),
            .flex, .rigid => |variable| try self.graph.newNode(.{ .unresolved = .{
                .numeric_default_phase = variable.numeric_default_phase,
                .row_default = variable.row_default,
            } }),
            .empty_record => try self.graph.newNode(.empty_record),
            // A checked empty tag union records that no value reaches the
            // slot. Sibling descriptions of the same slot may still carry
            // tags (which are then unreachable), so the slot yields to them
            // and defaults to the empty union only when nothing else claims
            // it.
            .empty_tag_union => try self.graph.newNode(.{ .unresolved = .{ .row_default = .empty_tag_union } }),
            .alias => |alias| try self.graph.newNode(.{ .named = .{
                .named_type = .{ .module = self.builder.declaredModuleForAlias(self.view, alias), .ty = checked_ty },
                .def = try self.builder.typeDef(self.view, alias.origin_module, alias.name, alias.source_decl),
                .kind = .alias,
                .builtin_owner = null,
                .args = try self.instNodeSlice(alias.args),
                .backing = .{
                    .node = try self.instNode(alias.backing),
                    .use = .inspectable,
                },
            } }),
            .record_unbound => |fields| try self.graph.newNode(.{ .record = .{
                .fields = try self.instFields(fields),
                .ext = try self.graph.newNode(.empty_record),
            } }),
            .record => |record| try self.graph.newNode(.{ .record = .{
                .fields = try self.instFields(record.fields),
                .ext = try self.instNode(record.ext),
            } }),
            .tuple => |items| try self.graph.newNode(.{ .tuple = try self.instNodeSlice(items) }),
            .function => |function| try self.graph.newNode(.{ .func = .{
                .args = try self.instNodeSlice(function.args),
                .ret = try self.instNode(function.ret),
            } }),
            .tag_union => |tag_union| try self.graph.newNode(.{ .tag_union = .{
                .tags = try self.instTags(tag_union.tags),
                .ext = try self.instNode(tag_union.ext),
            } }),
            .nominal => |nominal| try self.instNominalNode(checked_ty, nominal),
        };
    }

    fn instFields(self: *BodyContext, fields: []const checked.CheckedRecordField) Allocator.Error![]InstField {
        const out = try self.graph.arena().alloc(InstField, fields.len);
        for (fields, 0..) |field, index| {
            out[index] = .{
                .name = try self.builder.recordFieldName(self.view, field.name),
                .ty = try self.instNode(field.ty),
            };
        }
        return out;
    }

    fn instTags(self: *BodyContext, tags: []const checked.CheckedTag) Allocator.Error![]InstTag {
        const out = try self.graph.arena().alloc(InstTag, tags.len);
        for (tags, 0..) |tag, index| {
            out[index] = .{
                .name = try self.builder.tagName(self.view, tag.name),
                .checked_name = tag.name,
                .payloads = try self.instNodeSlice(tag.argsSlice(self.view.types)),
            };
        }
        return out;
    }

    fn instNominalNode(
        self: *BodyContext,
        checked_ty: checked.CheckedTypeId,
        nominal: checked.CheckedNominalType,
    ) Allocator.Error!NodeId {
        switch (nominal.representation) {
            .builtin => |builtin| switch (checked.builtinRuntimeEncoding(builtin)) {
                .primitive => |primitive| return try self.graph.newNode(.{ .primitive = primitive }),
                .bool_tag_union => {},
                .list => {
                    if (nominal.args.len != 1) Common.invariant("checked List nominal must have exactly one type argument");
                    return try self.graph.newNode(.{ .list = try self.instNode(nominal.args[0]) });
                },
                .box => {
                    if (nominal.args.len != 1) Common.invariant("checked Box nominal must have exactly one type argument");
                    return try self.graph.newNode(.{ .box = try self.instNode(nominal.args[0]) });
                },
                .parse_tag_union_spec,
                .fields,
                .field,
                => {},
            },
            else => {},
        }

        const args = try self.instNodeSlice(nominal.args);
        const backing_node: ?NodeId = switch (nominal.representation) {
            .opaque_without_backing => null,
            else => try self.instNominalBackingNode(nominal, args),
        };
        const backing: ?InstBacking = if (backing_node) |node| .{
            .node = node,
            .use = if (nominal.is_opaque) .runtime_layout_only else .inspectable,
        } else null;
        return try self.graph.newNode(.{ .named = .{
            .named_type = .{ .module = self.builder.declaredModuleForNominal(self.view, nominal), .ty = checked_ty },
            .def = try self.builder.typeDef(self.view, nominal.origin_module, nominal.name, nominal.source_decl),
            .kind = if (nominal.is_opaque) .@"opaque" else .nominal,
            .builtin_owner = builtinOwner(nominal.builtin),
            .args = args,
            .backing = backing,
            .declared_order = try self.builder.declaredOrderForNominal(self.view, nominal),
        } });
    }

    /// Instantiate a nominal instance's backing. A local declaration's
    /// formals and backing share one set of checked roots across every
    /// instance of the nominal, so the backing instantiates inside a fresh
    /// scope seeded with this instance's argument nodes: two instances of the
    /// same nominal at different arguments stay independent, and the
    /// recursive uses inside the backing resolve through the scope chain.
    fn instNominalBackingNode(
        self: *BodyContext,
        nominal: checked.CheckedNominalType,
        args: []NodeId,
    ) Allocator.Error!NodeId {
        const source = self.nominalInstantiationSource(nominal) orelse
            return try self.instNode(self.nominalBackingRoot(nominal));
        if (source.declaration.formalArgs(self.view.types).len != args.len) {
            Common.invariant("checked nominal declaration arity differed from nominal type use");
        }
        var scope = std.AutoHashMap(CheckedTypeAddress, NodeId).init(self.allocator);
        defer scope.deinit();
        for (source.declaration.formalArgs(self.view.types), args) |formal, arg| {
            try scope.put(self.typeAddress(self.checkedTypeInCurrentView(source.view, formal)), arg);
        }
        try self.decl_scopes.append(self.allocator, &scope);
        defer _ = self.decl_scopes.pop();
        return try self.instNode(self.nominalBackingRoot(nominal));
    }

    const NominalInstantiationSource = struct {
        view: ModuleView,
        declaration: checked.CheckedNominalDeclaration,
    };

    fn nominalInstantiationSource(
        self: *BodyContext,
        nominal: checked.CheckedNominalType,
    ) ?NominalInstantiationSource {
        return switch (nominal.representation) {
            .local_declaration => |id| .{
                .view = self.view,
                .declaration = self.view.types.nominalDeclarationById(id),
            },
            .local_box_payload_capability,
            .imported_declaration,
            .imported_box_payload_capability,
            => null,
            .builtin,
            .opaque_without_backing,
            => null,
        };
    }

    fn nominalBackingRoot(
        self: *BodyContext,
        nominal: checked.CheckedNominalType,
    ) checked.CheckedTypeId {
        return switch (nominal.representation) {
            .local_declaration => |id| self.view.types.nominalDeclarationById(id).backing,
            .imported_declaration => nominal.backing,
            .local_box_payload_capability => |capability| self.view.interface_capabilities.boxPayloadCapability(capability.capability).backing_ty,
            .imported_box_payload_capability => |capability| blk: {
                const source_view = self.builder.moduleForId(checked.importedBoxPayloadCapabilityModuleId(capability));
                const backing = source_view.interface_capabilities.boxPayloadCapability(capability.capability).backing_ty;
                break :blk self.checkedTypeInCurrentView(source_view, backing);
            },
            .builtin,
            .opaque_without_backing,
            => nominal.backing,
        };
    }

    fn checkedTypeInCurrentView(
        self: *BodyContext,
        source_view: ModuleView,
        source_ty: checked.CheckedTypeId,
    ) checked.CheckedTypeId {
        if (moduleBytesEqual(source_view.key.bytes, self.view.key.bytes)) return source_ty;
        return self.view.types.rootForKey(source_view.types.rootKey(source_ty)) orelse
            Common.invariant("imported nominal declaration formal was not projected into the current checked type store");
    }

    fn lowerTemplateBody(
        self: *BodyContext,
        template_ref: names.ProcTemplate,
        template: checked.CheckedProcedureTemplate,
        fn_ty: Type.TypeId,
    ) Allocator.Error!LoweredTemplateBody {
        if (!names.procedureTemplateRefEql(self.owner_template, template_ref)) {
            Common.invariant("Monotype body context owner did not match lowered checked template");
        }
        const ret_ty = self.builder.functionShape(fn_ty, "checked procedure template root type was not a function").ret;

        switch (template.body) {
            .checked_body => |body_id| {
                const body = self.view.bodies.body(body_id);
                const root = self.view.bodies.expr(body.root_expr);
                return switch (root.data) {
                    .lambda => |lambda| try self.lowerLambdaTemplate(lambda, fn_ty),
                    .closure => |closure| blk: {
                        if (closure.captures.len != 0) {
                            Common.invariant("checked procedure template root closure had captures");
                        }
                        const lambda_expr = self.view.bodies.expr(closure.lambda);
                        break :blk switch (lambda_expr.data) {
                            .lambda => |lambda| try self.lowerLambdaTemplate(lambda, fn_ty),
                            else => Common.invariant("checked procedure template root closure did not point at a lambda"),
                        };
                    },
                    .hosted_lambda => Common.invariant("hosted lambda template must lower through hosted metadata, not source lambda body"),
                    else => .{
                        .args = .empty(),
                        .body = try self.lowerExprAtType(body.root_expr, ret_ty),
                        .ret = ret_ty,
                    },
                };
            },
            .entry_wrapper => |wrapper_id| {
                const wrapper = self.view.entry_wrappers.get(wrapper_id);
                const root = self.view.compile_time_roots.root(wrapper.root);
                const saved_source_region_override = self.source_region_override;
                defer self.source_region_override = saved_source_region_override;
                self.source_region_override = switch (root.kind) {
                    .hoisted_constant => self.view.bodies.expr(root.expr).source_region,
                    .constant,
                    .callable_binding,
                    .expect,
                    .numeral_conversion,
                    .quote_conversion,
                    => saved_source_region_override,
                };
                switch (root.kind) {
                    .numeral_conversion, .quote_conversion => return .{
                        .args = .empty(),
                        .body = try self.lowerNumeralRootBody(wrapper.body_expr, ret_ty),
                        .ret = ret_ty,
                    },
                    .constant,
                    .hoisted_constant,
                    .callable_binding,
                    .expect,
                    => {},
                }
                return .{
                    .args = .empty(),
                    .body = try self.lowerComptimeRootExprAtType(wrapper.body_expr, ret_ty),
                    .ret = ret_ty,
                };
            },
            .intrinsic_wrapper => |wrapper_id| {
                const wrapper = self.view.intrinsic_wrappers.get(wrapper_id);
                return switch (wrapper.intrinsic) {
                    .str_inspect => try self.lowerStrInspectIntrinsic(fn_ty, ret_ty),
                    .structural_eq => Common.invariant("structural equality intrinsic wrapper must lower through checked dispatch plans"),
                };
            },
        }
    }

    fn lowerStrInspectIntrinsic(self: *BodyContext, fn_ty: Type.TypeId, ret_ty: Type.TypeId) Allocator.Error!LoweredTemplateBody {
        const fn_data = self.builder.functionShape(fn_ty, "Str.inspect intrinsic had a non-function type");
        const arg_tys = try self.allocator.dupe(Type.TypeId, self.builder.program.types.span(fn_data.args));
        defer self.allocator.free(arg_tys);
        if (arg_tys.len != 1) Common.invariant("Str.inspect intrinsic requires exactly one argument");

        const arg_local = try self.builder.program.addLocal(self.builder.symbols.fresh(), arg_tys[0]);
        const typed_arg = Ast.TypedLocal{ .local = arg_local, .ty = arg_tys[0] };
        const local_expr = try self.builder.program.addExpr(.{ .ty = arg_tys[0], .data = .{ .local = arg_local } });
        const body = try self.builder.inspectCall(local_expr, arg_tys[0], ret_ty);
        return .{
            .args = try self.builder.program.addTypedLocalSpan(&.{typed_arg}),
            .body = body,
            .ret = ret_ty,
        };
    }

    fn lowerLambdaTemplate(self: *BodyContext, lambda: anytype, fn_ty: Type.TypeId) Allocator.Error!LoweredTemplateBody {
        const fn_data = self.builder.functionShape(fn_ty, "lambda template had a non-function type");
        const arg_tys = try self.allocator.dupe(Type.TypeId, self.builder.program.types.span(fn_data.args));
        defer self.allocator.free(arg_tys);
        if (arg_tys.len != lambda.args.len) Common.invariant("lambda template arity differs from concrete function type");

        const lowered = try self.lowerLambdaArgsAndBody(lambda.args, arg_tys, lambda.body, fn_data.ret);
        return .{
            .args = lowered.args,
            .body = lowered.body,
            .ret = fn_data.ret,
        };
    }

    fn lowerLambdaArgsAndBody(
        self: *BodyContext,
        checked_args: []const checked.CheckedPatternId,
        arg_tys: []const Type.TypeId,
        checked_body: checked.CheckedExprId,
        ret_ty: Type.TypeId,
    ) Allocator.Error!LoweredLambdaArgs {
        if (arg_tys.len != checked_args.len) Common.invariant("lambda arity differs from concrete function type");
        const saved_loc = self.builder.program.current_loc;
        defer self.builder.program.current_loc = saved_loc;
        const saved_region = self.builder.program.current_region;
        defer self.builder.program.current_region = saved_region;
        const body_expr = self.view.bodies.expr(checked_body);
        const body_region = self.sourceRegionForExpr(body_expr);
        self.builder.program.current_loc = try self.sourceLocFor(body_region);
        self.builder.program.current_region = body_region;

        const saved_comptime_depth = self.resetComptimeExhaustivenessDepth();
        defer self.restoreComptimeExhaustivenessDepth(saved_comptime_depth);

        const args = try self.allocator.alloc(Ast.TypedLocal, checked_args.len);
        defer self.allocator.free(args);
        var arg_lets = std.ArrayList(LambdaArgLet).empty;
        defer arg_lets.deinit(self.allocator);
        var materialized_args = std.ArrayList(MaterializedArg).empty;
        defer materialized_args.deinit(self.allocator);

        for (checked_args, arg_tys, 0..) |pattern_id, arg_ty, i| {
            if (self.patternNeedsExplicitBinding(pattern_id)) {
                const local = try self.builder.program.addLocal(self.builder.symbols.fresh(), arg_ty);
                args[i] = .{ .local = local, .ty = arg_ty };
                const value = try self.builder.localExpr(local, arg_ty);
                try materialized_args.append(self.allocator, .{
                    .pattern = pattern_id,
                    .value = value,
                    .ty = arg_ty,
                });
                continue;
            }

            const pat = try self.lowerPatternAtType(pattern_id, arg_ty);
            switch (self.builder.program.pats.items[@intFromEnum(pat)].data) {
                .bind => |local| args[i] = .{ .local = local, .ty = arg_ty },
                else => {
                    const local = try self.builder.program.addLocal(self.builder.symbols.fresh(), arg_ty);
                    args[i] = .{ .local = local, .ty = arg_ty };
                    const value = try self.builder.program.addExpr(.{ .ty = arg_ty, .data = .{ .local = local } });
                    try arg_lets.append(self.allocator, .{ .pat = pat, .value = value });
                },
            }
        }

        var body = try self.lowerBindingContinuation(.{ .materialized_args = .{
            .args = materialized_args.items,
            .index = 0,
            .body = checked_body,
        } }, ret_ty);
        const body_loc = self.builder.program.exprLoc(body);
        const body_region_after_lowering = self.builder.program.exprRegion(body);
        const saved_body_loc = self.builder.program.current_loc;
        defer self.builder.program.current_loc = saved_body_loc;
        const saved_body_region = self.builder.program.current_region;
        defer self.builder.program.current_region = saved_body_region;
        self.builder.program.current_loc = body_loc;
        self.builder.program.current_region = body_region_after_lowering;
        var remaining = arg_lets.items.len;
        while (remaining > 0) {
            remaining -= 1;
            const arg_let = arg_lets.items[remaining];
            body = try self.builder.program.addExpr(.{ .ty = ret_ty, .data = .{ .let_ = .{
                .bind = arg_let.pat,
                .value = arg_let.value,
                .rest = body,
            } } });
        }

        return .{
            .args = try self.builder.program.addTypedLocalSpan(args),
            .body = body,
        };
    }

    fn lowerNestedFunction(self: *BodyContext, expr_id: checked.CheckedExprId, fn_ty: Type.TypeId) Allocator.Error!LoweredTemplateBody {
        const expr = self.view.bodies.expr(expr_id);
        return switch (expr.data) {
            .lambda => |lambda| try self.lowerNestedLambdaTemplate(lambda, fn_ty),
            .closure => |closure| blk: {
                const lambda_expr = self.view.bodies.expr(closure.lambda);
                break :blk switch (lambda_expr.data) {
                    .lambda => |lambda| try self.lowerNestedLambdaTemplate(lambda, fn_ty),
                    else => Common.invariant("checked closure did not point at a lambda expression"),
                };
            },
            else => Common.invariant("local procedure site did not point at a lambda or closure"),
        };
    }

    fn lowerNestedLambdaTemplate(self: *BodyContext, lambda: anytype, fn_ty: Type.TypeId) Allocator.Error!LoweredTemplateBody {
        var saved = std.ArrayList(BinderRestore).empty;
        defer saved.deinit(self.allocator);
        for (lambda.args) |pattern_id| try self.savePatternBinders(pattern_id, &saved);
        defer self.restoreBinders(saved.items);
        return try self.lowerLambdaTemplate(lambda, fn_ty);
    }

    fn lowerExprType(self: *BodyContext, expr_id: checked.CheckedExprId) Allocator.Error!Type.TypeId {
        const expr = self.view.bodies.expr(expr_id);
        return switch (expr.data) {
            .call => |call| (try self.callResultMonoType(expr.ty, call, null)) orelse try self.lowerType(expr.ty),
            .dispatch_call => |plan| (try self.dispatchResultMonoType(expr.ty, plan, null)) orelse try self.lowerType(expr.ty),
            .interpolation => |interpolation| (try self.dispatchResultMonoType(expr.ty, interpolation.plan, null)) orelse try self.lowerType(expr.ty),
            .type_dispatch_call => |plan| (try self.dispatchResultMonoType(expr.ty, plan, null)) orelse try self.lowerType(expr.ty),
            .method_eq => |plan| (try self.dispatchResultMonoType(expr.ty, plan, null)) orelse try self.lowerType(expr.ty),
            .lookup_local => |lookup| try self.lookupExprMonoType(expr.ty, lookup.resolved),
            .lookup_external => |resolved| try self.lookupExprMonoType(expr.ty, resolved),
            .lookup_required => |resolved| try self.lookupExprMonoType(expr.ty, resolved),
            .lambda => |lambda| try self.lambdaFunctionType(lambda),
            .closure => |closure| try self.closureFunctionType(closure),
            .field_access => |field| try self.fieldAccessMonoType(field.receiver, field.field_name),
            else => try self.lowerType(expr.ty),
        };
    }

    /// Resolve a checked node's source region to a `SourceLoc` in this body's
    /// module.
    fn sourceLocFor(self: *BodyContext, region: base.Region) Allocator.Error!base.SourceLoc {
        if (region.isEmpty()) return base.SourceLoc.none;
        const line_starts = self.view.module_env.common.getLineStartsAll();
        if (line_starts.len == 0) return base.SourceLoc.none;
        const offset = region.start.offset;
        const line = base.RegionInfo.lineIdx(line_starts, offset);
        const column = base.RegionInfo.columnIdx(line_starts, line, offset) catch
            Common.invariant("checked node region resolved to an invalid line/column position");
        return .{
            .file = try self.builder.fileIdFor(self.view),
            .line = line + 1,
            .column = column + 1,
        };
    }

    fn sourceRegionForExpr(self: *const BodyContext, expr: checked.CheckedExpr) base.Region {
        return self.source_region_override orelse expr.source_region;
    }

    fn lowerExpr(self: *BodyContext, expr_id: checked.CheckedExprId) Allocator.Error!Ast.ExprId {
        const expr = self.view.bodies.expr(expr_id);
        const saved_loc = self.builder.program.current_loc;
        defer self.builder.program.current_loc = saved_loc;
        const saved_region = self.builder.program.current_region;
        defer self.builder.program.current_region = saved_region;
        const region = self.sourceRegionForExpr(expr);
        self.builder.program.current_loc = try self.sourceLocFor(region);
        self.builder.program.current_region = region;
        const expr_ty = try self.lowerExprType(expr_id);
        if (try self.restoredHoistedExprAtType(expr_id, expr_ty)) |restored| return restored;
        switch (expr.data) {
            .call => |call| return try self.lowerCallExpr(expr_id, expr.ty, call),
            .dispatch_call => |plan| return try self.lowerDispatchExpr(expr.ty, plan),
            .interpolation => |interpolation| return try self.lowerDispatchExpr(expr.ty, interpolation.plan),
            .type_dispatch_call => |plan| return try self.lowerDispatchExpr(expr.ty, plan),
            .method_eq => |plan| return try self.lowerDispatchExpr(expr.ty, plan),
            .structural_eq => |eq| return try self.lowerDirectStructuralEq(expr.ty, eq),
            .structural_hash => |h| return try self.lowerDirectStructuralHash(expr.ty, h),
            else => {},
        }
        return try self.lowerExprWithType(expr_id, expr_ty);
    }

    fn lowerComptimeRootExprAtType(
        self: *BodyContext,
        expr_id: checked.CheckedExprId,
        ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        self.comptime_exhaustiveness_depth += 1;
        defer self.comptime_exhaustiveness_depth -= 1;
        return try self.lowerExprAtType(expr_id, ty);
    }

    fn inComptimeExhaustivenessContext(self: *const BodyContext) bool {
        return self.comptime_exhaustiveness_depth != 0;
    }

    fn resetComptimeExhaustivenessDepth(self: *BodyContext) u32 {
        const saved = self.comptime_exhaustiveness_depth;
        self.comptime_exhaustiveness_depth = 0;
        return saved;
    }

    fn restoreComptimeExhaustivenessDepth(self: *BodyContext, saved: u32) void {
        self.comptime_exhaustiveness_depth = saved;
    }

    fn loweringOwnHoistedConstRoot(self: *BodyContext, entry: checked.HoistedConstEntry) bool {
        const wrapper = self.view.entry_wrappers.lookupByRoot(entry.root) orelse
            Common.invariant("hoisted const root had no checked entry wrapper");
        return names.procedureTemplateRefEql(wrapper.template, self.owner_template);
    }

    fn restoredHoistedConstAtType(
        self: *BodyContext,
        entry: checked.HoistedConstEntry,
        ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        try self.constrainTypeToMono(entry.checked_type, ty);
        const template = self.view.const_templates.get(entry.const_ref);
        return switch (template.state) {
            .stored_const => |stored| try self.restoreConstNodeAtType(self.view, self.view, stored.node, ty),
            .eval_template => |eval| try self.lowerConstEvalTemplateUse(self.view, eval, ty, self.hoistedConstSourceRegion(entry)),
            .reserved => Common.invariant("reserved hoisted const template reached Monotype"),
        };
    }

    fn hoistedConstSourceRegion(self: *const BodyContext, entry: checked.HoistedConstEntry) base.Region {
        if (entry.pattern) |pattern| {
            return self.view.bodies.pattern(pattern).source_region;
        }
        return self.view.bodies.expr(entry.expr).source_region;
    }

    fn restoredHoistedExprAtType(
        self: *BodyContext,
        expr_id: checked.CheckedExprId,
        ty: Type.TypeId,
    ) Allocator.Error!?Ast.ExprId {
        const entry = self.view.hoisted_constants.lookupByExpr(expr_id) orelse return null;
        if (self.loweringOwnHoistedConstRoot(entry)) return null;
        return try self.restoredHoistedConstAtType(entry, ty);
    }

    fn selectedHoistedConstEntry(
        self: *BodyContext,
        selected: checked.SelectedHoistedConstUse,
    ) checked.HoistedConstEntry {
        const const_ref = selected.const_use.const_ref;
        const module_id = checked.constModuleId(const_ref);
        if (!moduleBytesEqual(module_id.bytes, self.view.key.bytes)) {
            Common.invariant("selected hoisted local const ref referenced a different checked module");
        }
        const hoisted = switch (const_ref.owner) {
            .hoisted_expr => |owner| owner,
            .top_level_binding => Common.invariant("selected hoisted local const ref did not reference a hoisted const"),
        };
        if (hoisted.module_idx != self.view.module_identity.module_idx) {
            Common.invariant("selected hoisted local const ref had mismatched module index");
        }
        return self.view.hoisted_constants.lookupByExpr(hoisted.expr) orelse
            Common.invariant("selected hoisted local const ref had no hoisted const table entry");
    }

    fn selectedHoistedConstMonoType(
        self: *BodyContext,
        selected: checked.SelectedHoistedConstUse,
        checked_ty: checked.CheckedTypeId,
    ) Allocator.Error!?Type.TypeId {
        const entry = self.selectedHoistedConstEntry(selected);
        if (self.loweringOwnHoistedConstRoot(entry)) return null;
        const hoisted_ty = try self.lowerType(entry.checked_type);
        try self.constrainTypeToMono(checked_ty, hoisted_ty);
        return hoisted_ty;
    }

    fn restoreSelectedHoistedConstAtType(
        self: *BodyContext,
        selected: checked.SelectedHoistedConstUse,
        ty: Type.TypeId,
    ) Allocator.Error!?Ast.ExprId {
        const entry = self.selectedHoistedConstEntry(selected);
        if (self.loweringOwnHoistedConstRoot(entry)) return null;
        return try self.restoredHoistedConstAtType(entry, ty);
    }

    fn lowerExprWithType(
        self: *BodyContext,
        expr_id: checked.CheckedExprId,
        ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        const expr = self.view.bodies.expr(expr_id);
        const saved_loc = self.builder.program.current_loc;
        defer self.builder.program.current_loc = saved_loc;
        const saved_region = self.builder.program.current_region;
        defer self.builder.program.current_region = saved_region;
        const region = self.sourceRegionForExpr(expr);
        self.builder.program.current_loc = try self.sourceLocFor(region);
        self.builder.program.current_region = region;
        const data: Ast.ExprData = switch (expr.data) {
            .pending,
            .anno_only,
            .runtime_error,
            => Common.invariant("non-runtime checked expression reached Monotype lowering"),
            .num => |num| self.lowerIntLiteral(num.value, ty),
            .typed_int => |num| self.lowerIntLiteral(num.value, ty),
            .frac_f32 => |frac| self.lowerFracLiteral(.{ .f32 = frac.value }, ty),
            .frac_f64 => |frac| self.lowerFracLiteral(.{ .f64 = frac.value }, ty),
            .dec => |dec| self.lowerFracLiteral(.{ .dec = dec.value }, ty),
            .dec_small => Common.invariant("small decimal literal reached Monotype after numeric finalization"),
            .num_from_numeral => |plan| {
                if (try self.restoredNumeralConst(expr_id, ty)) |restored| return restored;
                return try self.lowerNumeralFold(expr.ty, plan, ty);
            },
            .typed_frac => Common.invariant("typed fractional integer literal reached Monotype after numeric finalization"),
            .typed_num_from_numeral => |plan| {
                if (try self.restoredNumeralConst(expr_id, ty)) |restored| return restored;
                return try self.lowerNumeralFold(expr.ty, plan, ty);
            },
            .str_from_quote => |quote| {
                if (try self.restoredNumeralConst(expr_id, ty)) |restored| return restored;
                return try self.lowerNumeralCall(expr.ty, quote.plan, ty);
            },
            .str_segment => |str| .{ .str_lit = try self.lowerStringLiteral(str) },
            .bytes_literal => |str| .{ .str_lit = try self.lowerStringLiteral(str) },
            .empty_list => .{ .list = .empty() },
            .empty_record => .{ .record = .empty() },
            .str => |segments| try self.lowerStr(segments),
            .lookup_local => |lookup| return try self.lowerLookupExprAtType(expr.ty, lookup.resolved, ty),
            .lookup_external => |resolved| return try self.lowerLookupExprAtType(expr.ty, resolved, ty),
            .lookup_required => |resolved| return try self.lowerLookupExprAtType(expr.ty, resolved, ty),
            .list => |items| .{ .list = try self.lowerListExpr(items, ty) },
            .tuple => |items| .{ .tuple = try self.lowerExprSpanAtTypes(items, self.builder.tupleItemTypes(ty)) },
            .record => |record| return try self.lowerRecordExpr(record, ty),
            .tag => |tag| blk: {
                const name = try self.builder.tagName(self.view, tag.name);
                break :blk .{ .tag = .{
                    .name = name,
                    .payloads = try self.lowerExprSpanAtTypes(tag.args, self.builder.tagPayloadTypes(ty, name)),
                } };
            },
            .zero_argument_tag => |tag| .{ .tag = .{ .name = try self.builder.tagName(self.view, tag.name), .payloads = .empty() } },
            .nominal => |nominal| .{ .nominal = try self.lowerExprAtType(nominal.backing_expr, self.builder.namedBackingType(ty) orelse ty) },
            .closure => |closure| try self.lowerClosure(expr_id, closure, ty),
            .lambda => blk: {
                break :blk try self.lowerLambdaExpr(
                    expr_id,
                    try self.builder.fnTemplateForNestedExprWithMono(self.view, self.owner_template, expr_id, expr.ty, self.view.types.rootKey(expr.ty), ty, self.current_fn_key),
                );
            },
            .call => Common.invariant("call expression reached ordinary expression lowering after call-site lowering"),
            .dispatch_call,
            .interpolation,
            .type_dispatch_call,
            .method_eq,
            => Common.invariant("dispatch expression reached ordinary expression lowering after call-site lowering"),
            .structural_eq => Common.invariant("structural equality reached ordinary expression lowering after explicit equality lowering"),
            .structural_hash => Common.invariant("structural hash reached ordinary expression lowering after explicit hash lowering"),
            .field_access => |field| field_access: {
                const receiver_ty = try self.lowerExprType(field.receiver);
                break :field_access .{ .field_access = .{
                    .receiver = try self.lowerExprAtType(field.receiver, receiver_ty),
                    .field = try self.builder.recordFieldName(self.view, field.field_name),
                } };
            },
            .tuple_access => |access| .{ .tuple_access = .{
                .tuple = try self.lowerExpr(access.tuple),
                .elem_index = access.elem_index,
            } },
            .match_ => |match| return try self.lowerMatchExpr(expr_id, match, ty),
            .if_ => |if_| return try self.lowerIfExpr(expr_id, if_, ty),
            .block => |block| try self.lowerBlock(block, ty),
            .binop,
            .unary_minus,
            .unary_not,
            => Common.invariant("desugared operator expression reached Monotype without checked dispatch or low-level form"),
            .ellipsis => .{ .crash = try self.builder.program.addStringLiteral("not implemented") },
            .crash => |msg| .{ .crash = try self.lowerStringLiteral(msg) },
            .dbg => |child| .{ .dbg = try self.lowerDbgMessage(child) },
            .expect_err => |expect_err| .{ .expect_err = .{
                .msg = try self.lowerExpectErrMessage(expect_err.expr, expect_err.snippet),
                .region = expr.source_region,
            } },
            .expect => |child| .{ .expect = try self.lowerExpr(child) },
            .break_ => try self.breakCurrentLoopExprData(),
            .return_ => |ret| .{ .return_ = try self.lowerExpr(ret.expr) },
            .for_ => |for_| try self.lowerIteratorFor(for_, ty, &.{}),
            .hosted_lambda => Common.invariant("hosted lambda expression reached ordinary Monotype expression lowering"),
            .run_low_level => |low_level| .{ .low_level = .{ .op = low_level.op, .args = try self.lowerExprSpan(low_level.args) } },
        };
        return try self.builder.program.addExpr(.{ .ty = ty, .data = data });
    }

    fn lowerDbgMessage(self: *BodyContext, child: checked.CheckedExprId) Allocator.Error!Ast.ExprId {
        const value = try self.lowerExpr(child);
        const value_ty = self.builder.program.exprs.items[@intFromEnum(value)].ty;
        const str_ty = try self.builder.primitiveType(.str);
        return try self.builder.inspectCall(value, value_ty, str_ty);
    }

    fn lowerExpectErrMessage(
        self: *BodyContext,
        child: checked.CheckedExprId,
        snippet: checked.CheckedStringLiteralId,
    ) Allocator.Error!Ast.ExprId {
        const value = try self.lowerExpr(child);
        const value_ty = self.builder.program.exprs.items[@intFromEnum(value)].ty;
        const str_ty = try self.builder.primitiveType(.str);
        const rendered = try self.builder.inspectCall(value, value_ty, str_ty);

        const snippet_index = @intFromEnum(snippet);
        if (snippet_index >= self.view.bodies.stringLiteralCount()) {
            Common.invariant("checked string literal id outside checked body string store");
        }
        const snippet_text = self.view.bodies.stringLiteral(@enumFromInt(snippet_index));
        const prefix_text = try std.fmt.allocPrint(
            self.builder.allocator,
            "The `?` operator in `{s}` evaluated an `Err` inside an `expect`. The value was: Err(",
            .{snippet_text},
        );
        defer self.builder.allocator.free(prefix_text);
        const prefix = try self.builder.stringExpr(prefix_text, str_ty);
        const with_value = try self.builder.concatExpr(prefix, rendered, str_ty);
        const suffix = try self.builder.stringExpr(")", str_ty);
        return try self.builder.concatExpr(with_value, suffix, str_ty);
    }

    fn lowerStr(self: *BodyContext, segments: []const checked.CheckedExprId) Allocator.Error!Ast.ExprData {
        const str_ty = try self.builder.primitiveType(.str);
        if (segments.len == 0) {
            return .{ .nominal = try self.builder.stringExpr("", str_ty) };
        }

        var out = try self.lowerExprAtType(segments[0], str_ty);
        for (segments[1..]) |segment| {
            const right = try self.lowerExprAtType(segment, str_ty);
            out = try self.builder.concatExpr(out, right, str_ty);
        }
        return .{ .nominal = out };
    }

    fn lowerStringLiteral(self: *BodyContext, id: checked.CheckedStringLiteralId) Allocator.Error!Ast.StringLiteralId {
        const index = @intFromEnum(id);
        if (index >= self.view.bodies.stringLiteralCount()) {
            Common.invariant("checked string literal id outside checked body string store");
        }
        if (self.string_literals[index]) |existing| return existing;
        const lowered = try self.builder.program.addStringLiteral(self.view.bodies.stringLiteral(@enumFromInt(index)));
        self.string_literals[index] = lowered;
        return lowered;
    }

    fn lowerCallExpr(self: *BodyContext, checked_expr_id: checked.CheckedExprId, checked_ret_ty: checked.CheckedTypeId, call: anytype) Allocator.Error!Ast.ExprId {
        if (try self.lowerParseIntrinsicCallExpr(checked_expr_id, checked_ret_ty, call, null)) |expr| return expr;
        const lowered = try self.lowerCall(checked_ret_ty, call);
        return try self.builder.program.addExpr(.{
            .ty = lowered.ret_ty,
            .data = lowered.data,
        });
    }

    fn checkedFunctionType(self: *BodyContext, checked_fn_ty: checked.CheckedTypeId) checked.CheckedFunctionType {
        return switch (resolvedPayload(self.view, checked_fn_ty).payload) {
            .function => |function| function,
            else => Common.invariant("checked call function type was not a function"),
        };
    }

    fn functionReturnType(self: *BodyContext, fn_ty: Type.TypeId) Type.TypeId {
        return self.builder.functionShape(fn_ty, "checked call function type was not a function").ret;
    }

    fn lowerParseIntrinsicCallExpr(
        self: *BodyContext,
        checked_expr_id: checked.CheckedExprId,
        checked_ret_ty: checked.CheckedTypeId,
        call: anytype,
        expected_ret_ty: ?Type.TypeId,
    ) Allocator.Error!?Ast.ExprId {
        const target = call.direct_target orelse return null;
        const intrinsic = self.parseIntrinsicForResolvedTarget(target) orelse return null;
        const source_fn_ty = self.directCallInstantiationSourceFnType(target, call.source_fn_ty_payload);

        const arg_tys = try self.allocator.alloc(Type.TypeId, call.args.len);
        defer self.allocator.free(arg_tys);
        for (call.args, 0..) |arg, index| {
            arg_tys[index] = try self.parseIntrinsicArgumentMonoType(arg);
        }
        const ret_ty = try self.parseIntrinsicReturnType(intrinsic, checked_ret_ty, call.args, arg_tys, expected_ret_ty);
        if (expected_ret_ty) |expected| {
            if (!self.sameType(expected, ret_ty)) Common.invariant("checked parse intrinsic lowered at a type different from its context type");
        }

        return switch (intrinsic) {
            .tag_union_parse => try self.lowerParseTagUnionDecode(call.args, arg_tys, ret_ty),
            .fields_rename_fields => try self.lowerFieldNamesRenameFieldNames(call.args, arg_tys, ret_ty),
            .fields_shortest_name => try self.lowerFieldNamesNameBound(call.args, arg_tys, ret_ty, .shortest),
            .fields_longest_name => try self.lowerFieldNamesNameBound(call.args, arg_tys, ret_ty, .longest),
            .fields_iter => try self.lowerFieldNamesIter(checked_expr_id, source_fn_ty, call.args, arg_tys, ret_ty, .all),
            .fields_for_size => try self.lowerFieldNamesIter(checked_expr_id, source_fn_ty, call.args, arg_tys, ret_ty, .for_size),
            .field_name => try self.lowerFieldName(call.args, arg_tys, ret_ty),
        };
    }

    fn parseIntrinsicReturnType(
        self: *BodyContext,
        intrinsic: ParseIntrinsic,
        checked_ret_ty: checked.CheckedTypeId,
        args: []const checked.CheckedExprId,
        arg_tys: []const Type.TypeId,
        expected_ret_ty: ?Type.TypeId,
    ) Allocator.Error!Type.TypeId {
        const derived = switch (intrinsic) {
            .tag_union_parse => blk: {
                if (args.len != 2 or arg_tys.len != 2) Common.invariant("ParseTagUnionSpec.parse reached Monotype with an unexpected arity");
                if (expected_ret_ty) |expected| break :blk expected;
                break :blk try self.lowerType(checked_ret_ty);
            },
            .fields_rename_fields => blk: {
                if (args.len != 2 or arg_tys.len != 2) Common.invariant("FieldNames.rename_fields reached Monotype with an unexpected arity");
                if (self.generatedFieldNamesBackingValueFieldNames(arg_tys[0]) != null) break :blk arg_tys[0];
                if (expected_ret_ty) |expected| break :blk expected;
                break :blk try self.lowerType(checked_ret_ty);
            },
            .fields_shortest_name, .fields_longest_name, .fields_iter, .field_name => blk: {
                if (args.len != 1 or arg_tys.len != 1) Common.invariant("field metadata intrinsic reached Monotype with an unexpected arity");
                if (expected_ret_ty) |expected| break :blk expected;
                break :blk try self.lowerType(checked_ret_ty);
            },
            .fields_for_size => blk: {
                if (args.len != 2 or arg_tys.len != 2) Common.invariant("FieldNames.for_size reached Monotype with an unexpected arity");
                if (expected_ret_ty) |expected| break :blk expected;
                break :blk try self.lowerType(checked_ret_ty);
            },
        };

        if (expected_ret_ty) |expected| {
            if (!self.sameType(expected, derived)) Common.invariant("checked parse intrinsic result differed from expected result type");
        }
        return derived;
    }

    fn parseIntrinsicArgumentMonoType(
        self: *BodyContext,
        checked_arg: checked.CheckedExprId,
    ) Allocator.Error!Type.TypeId {
        const expr = self.view.bodies.expr(checked_arg);
        switch (expr.data) {
            .lookup_local => |lookup| if (lookup.resolved) |ref_id| {
                if (self.currentLocalForResolvedValue(ref_id)) |local_id| {
                    return self.builder.program.locals.items[@intFromEnum(local_id)].ty;
                }
            },
            .lookup_external => |resolved| if (resolved) |ref_id| {
                if (self.currentLocalForResolvedValue(ref_id)) |local_id| {
                    return self.builder.program.locals.items[@intFromEnum(local_id)].ty;
                }
            },
            .lookup_required => |resolved| if (resolved) |ref_id| {
                if (self.currentLocalForResolvedValue(ref_id)) |local_id| {
                    return self.builder.program.locals.items[@intFromEnum(local_id)].ty;
                }
            },
            else => {},
        }
        return (try self.callArgumentMonoType(checked_arg, null)) orelse Common.invariant("checked parse intrinsic argument did not lower to a monotype");
    }

    fn lowerParseIntrinsicArgAtType(
        self: *BodyContext,
        checked_arg: checked.CheckedExprId,
        ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        const expr = self.view.bodies.expr(checked_arg);
        const maybe_ref: ?checked.ResolvedValueId = switch (expr.data) {
            .lookup_local => |lookup| lookup.resolved,
            .lookup_external => |ref_id| ref_id,
            .lookup_required => |ref_id| ref_id,
            else => null,
        };
        if (maybe_ref) |ref_id| {
            if (self.currentLocalForResolvedValue(ref_id)) |local_id| {
                const local_ty = self.builder.program.locals.items[@intFromEnum(local_id)].ty;
                if (!self.sameType(ty, local_ty)) {
                    Common.invariant("checked parse intrinsic local argument type differed from its concrete local type");
                }
                return try self.builder.program.addExpr(.{ .ty = local_ty, .data = .{ .local = local_id } });
            }

            const record = self.view.resolved_refs.records[@intFromEnum(ref_id)];
            switch (record.ref) {
                .local_proc => |local| return try self.builder.program.addExpr(.{
                    .ty = ty,
                    .data = .{ .fn_def = try self.fnTemplateForLocalProcWithMono(local, expr.ty, self.view.types.rootKey(expr.ty), ty) },
                }),
                .top_level_proc,
                .imported_proc,
                .hosted_proc,
                .promoted_top_level_proc,
                => |proc| return try self.lowerProcedureUseValueWithoutExtraConstraint(proc, ty),
                .platform_required_proc => |proc| return try self.lowerProcedureUseValueWithoutExtraConstraint(proc.procedure, ty),
                else => {},
            }
        }
        return try self.lowerExprAtType(checked_arg, ty);
    }

    fn lowerProcedureUseValueWithoutExtraConstraint(
        self: *BodyContext,
        proc: checked.ProcedureUseTemplate,
        mono_fn_ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        return switch (proc.binding) {
            .top_level => |top_level| blk: {
                const view = self.builder.moduleForId(checked.topLevelProcedureModuleId(top_level));
                const binding = view.top_level_procedure_bindings.get(top_level.binding);
                break :blk try self.builder.lowerProcedureBindingValue(view, binding, mono_fn_ty);
            },
            .imported => |imported| blk: {
                const view = self.builder.moduleForId(checked.importedProcedureModuleId(imported));
                for (view.exported_procedure_bindings.bindings) |binding| {
                    if (binding.binding.def == imported.def and binding.binding.pattern == imported.pattern) {
                        const binding_source = schemeRoot(view, binding.source_scheme, "imported procedure binding source scheme was not output");
                        const fn_template = self.builder.fnDefForImportedBindingBody(view, binding.body, binding_source, view.types.rootKey(binding_source), mono_fn_ty);
                        const fn_id = try self.builder.lowerFnTemplateDef(view, fn_template);
                        break :blk try self.builder.program.addExpr(.{
                            .ty = mono_fn_ty,
                            .data = .{ .fn_def = fn_id },
                        });
                    }
                }
                Common.invariant("imported procedure binding was not exported by its checked module");
            },
            .hosted => |hosted| blk: {
                const source_fn_ty = proc.source_fn_ty_payload orelse
                    Common.invariant("hosted procedure value reached parse intrinsic callback lowering without source type");
                const fn_template = self.builder.fnDefForTemplate(
                    self.builder.moduleForId(moduleIdFromDigest(names.procTemplateModuleDigest(hosted.template))),
                    hosted.template,
                    source_fn_ty,
                    proc.source_fn_ty_template,
                    mono_fn_ty,
                );
                const fn_id = try self.builder.lowerFnTemplateDefFromContext(self, fn_template);
                break :blk try self.builder.program.addExpr(.{ .ty = mono_fn_ty, .data = .{ .fn_def = fn_id } });
            },
            .platform_required => Common.invariant("platform required procedure reached parse intrinsic callback lowering"),
        };
    }

    fn parseIntrinsicForResolvedTarget(self: *BodyContext, target: checked.ResolvedValueId) ?ParseIntrinsic {
        const raw = @intFromEnum(target);
        if (raw >= self.view.resolved_refs.records.len) {
            Common.invariant("checked direct call target is outside resolved value table");
        }
        const proc = switch (self.view.resolved_refs.records[raw].ref) {
            .imported_proc => |proc| proc,
            else => return null,
        };
        const imported = switch (proc.binding) {
            .imported => |imported| imported,
            else => return null,
        };
        const view = self.builder.moduleForId(checked.importedProcedureModuleId(imported));
        return self.parseIntrinsicForBuiltinDef(view, imported.def);
    }

    fn parseIntrinsicForBuiltinDef(_: *BodyContext, view: ModuleView, def_idx: can.CIR.Def.Idx) ?ParseIntrinsic {
        if (view.module_env.module_role != .builtin) return null;

        const def = view.module_env.store.getDef(def_idx);
        const pattern = view.module_env.store.getPattern(def.pattern);
        const ident = switch (pattern) {
            .assign => |assign| assign.ident,
            else => return null,
        };
        const text = view.module_env.getIdentText(ident);
        if (Ident.textEql(text, "Builtin.ParseTagUnionSpec.parse")) return .tag_union_parse;
        if (Ident.textEql(text, "Builtin.Str.FieldName.FieldNames.rename_fields")) return .fields_rename_fields;
        if (Ident.textEql(text, "Builtin.Str.FieldName.FieldNames.shortest_name")) return .fields_shortest_name;
        if (Ident.textEql(text, "Builtin.Str.FieldName.FieldNames.longest_name")) return .fields_longest_name;
        if (Ident.textEql(text, "Builtin.Str.FieldName.FieldNames.iter")) return .fields_iter;
        if (Ident.textEql(text, "Builtin.Str.FieldName.FieldNames.for_size")) return .fields_for_size;
        if (Ident.textEql(text, "Builtin.Str.FieldName.name")) return .field_name;
        return null;
    }

    fn lowerFieldNamesRenameFieldNames(
        self: *BodyContext,
        args: []const checked.CheckedExprId,
        arg_tys: []const Type.TypeId,
        ret_ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        if (args.len != 2 or arg_tys.len != 2) Common.invariant("FieldNames.rename_fields reached Monotype with an unexpected arity");
        if (!self.sameType(arg_tys[0], ret_ty)) Common.invariant("FieldNames.rename_fields result type differed from its field set argument");

        const fields_value = try self.lowerParseIntrinsicArgAtType(args[0], arg_tys[0]);
        const rename_value = try self.lowerParseIntrinsicArgAtType(args[1], arg_tys[1]);
        if (self.generatedFieldNamesBackingValueFieldNames(arg_tys[0])) |backing_fields| {
            const fields_local = try self.builder.program.addLocal(self.builder.symbols.fresh(), ret_ty);
            const rename_local = try self.builder.program.addLocal(self.builder.symbols.fresh(), arg_tys[1]);
            var body = try self.lowerGeneratedFieldNamesRenameFieldNames(
                backing_fields,
                ret_ty,
                fields_local,
                rename_local,
                arg_tys[1],
            );
            body = try self.wrapLet(rename_local, arg_tys[1], rename_value, body, ret_ty);
            return try self.wrapLet(fields_local, ret_ty, fields_value, body, ret_ty);
        }

        const fields_local = try self.builder.program.addLocal(self.builder.symbols.fresh(), ret_ty);
        var body = try self.builder.localExpr(fields_local, ret_ty);
        body = try self.wrapWildcardLet(arg_tys[1], rename_value, body, ret_ty);
        return try self.wrapLet(fields_local, ret_ty, fields_value, body, ret_ty);
    }

    fn lowerGeneratedFieldNamesRenameFieldNames(
        self: *BodyContext,
        backing_fields: []const Type.Field,
        fields_ty: Type.TypeId,
        fields_local: Ast.LocalId,
        rename_local: Ast.LocalId,
        rename_fn_ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        const fields_backing_ty = self.builder.namedBackingType(fields_ty) orelse
            Common.invariant("generated FieldNames value expected a named backing type");
        const backing_local = try self.builder.program.addLocal(self.builder.symbols.fresh(), fields_backing_ty);
        const body = try self.lowerGeneratedFieldNamesRenameBackingRecord(
            backing_fields,
            fields_ty,
            fields_backing_ty,
            backing_local,
            rename_local,
            rename_fn_ty,
        );
        const field_pat = try self.builder.program.addPat(.{
            .ty = fields_ty,
            .data = .{ .nominal = try self.builder.bindPat(backing_local, fields_backing_ty) },
        });
        const branch = Ast.Branch{ .pat = field_pat, .body = body };
        return try self.builder.program.addExpr(.{ .ty = fields_ty, .data = .{ .match_ = .{
            .scrutinee = try self.builder.localExpr(fields_local, fields_ty),
            .branches = try self.builder.program.addBranchSpan(&[_]Ast.Branch{branch}),
        } } });
    }

    fn lowerGeneratedFieldNamesRenameBackingRecord(
        self: *BodyContext,
        backing_fields: []const Type.Field,
        fields_ty: Type.TypeId,
        fields_backing_ty: Type.TypeId,
        backing_local: Ast.LocalId,
        rename_local: Ast.LocalId,
        rename_fn_ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        const info = self.generatedFieldNamesBackingInfoFromBacking(fields_backing_ty) orelse
            Common.invariant("generated FieldNames value expected a generated backing type");
        if (backing_fields.len != info.item_fields.len) Common.invariant("generated FieldNames rename arity differed from item count");
        const str_ty = try self.builder.primitiveType(.str);
        const lowered_items = try self.allocator.alloc(Ast.FieldExpr, info.item_fields.len);
        defer self.allocator.free(lowered_items);
        const item_locals = try self.allocator.alloc(Ast.LocalId, info.item_fields.len);
        defer self.allocator.free(item_locals);
        const item_exprs = try self.allocator.alloc(Ast.ExprId, info.item_fields.len);
        defer self.allocator.free(item_exprs);
        const renamed_name_locals = try self.allocator.alloc(Ast.LocalId, info.item_fields.len);
        defer self.allocator.free(renamed_name_locals);
        const renamed_name_exprs = try self.allocator.alloc(Ast.ExprId, info.item_fields.len);
        defer self.allocator.free(renamed_name_exprs);

        const items_expr = try self.builder.program.addExpr(.{
            .ty = info.items_field.ty,
            .data = .{ .field_access = .{
                .receiver = try self.builder.localExpr(backing_local, fields_backing_ty),
                .field = info.items_field.name,
            } },
        });
        const items_local = try self.builder.program.addLocal(self.builder.symbols.fresh(), info.items_field.ty);

        for (info.item_fields, 0..) |field, index| {
            const field_ty = field.ty;
            item_exprs[index] = try self.builder.program.addExpr(.{
                .ty = field_ty,
                .data = .{ .field_access = .{
                    .receiver = try self.builder.localExpr(items_local, info.items_field.ty),
                    .field = field.name,
                } },
            });
            item_locals[index] = try self.builder.program.addLocal(self.builder.symbols.fresh(), field_ty);
            const current_name = try self.fieldNameFromLocal(item_locals[index], field_ty, str_ty);
            renamed_name_exprs[index] = try self.builder.program.addExpr(.{
                .ty = str_ty,
                .data = .{ .call_value = .{
                    .callee = try self.builder.localExpr(rename_local, rename_fn_ty),
                    .args = try self.builder.program.addExprSpan(&[_]Ast.ExprId{current_name}),
                } },
            });
            renamed_name_locals[index] = try self.builder.program.addLocal(self.builder.symbols.fresh(), str_ty);
            const renamed_handle = try self.lowerRecordFieldHandleWithName(
                field_ty,
                try self.builder.localExpr(renamed_name_locals[index], str_ty),
                try self.builder.lowLevelExpr(
                    .str_count_utf8_bytes,
                    &.{try self.builder.localExpr(renamed_name_locals[index], str_ty)},
                    try self.builder.primitiveType(.u64),
                ),
                index,
            );
            lowered_items[index] = .{
                .name = field.name,
                .value = renamed_handle,
            };
        }

        const items_record_expr = try self.builder.program.addExpr(.{
            .ty = info.items_field.ty,
            .data = .{ .record = try self.builder.program.addFieldExprSpan(lowered_items) },
        });
        const bound_exprs = try self.fieldNameBoundExprsFromLocals(renamed_name_locals, null, info.shortest_field.ty);
        const shortest_expr = bound_exprs[0];
        const longest_expr = bound_exprs[1];
        const backing_type_fields = self.builder.program.types.fieldSpan(self.builder.recordFieldsSpan(fields_backing_ty));
        const backing_values = try self.allocator.alloc(Ast.FieldExpr, backing_type_fields.len);
        defer self.allocator.free(backing_values);
        for (backing_type_fields, 0..) |field, index| {
            const label = self.builder.program.names.recordFieldLabelText(field.name);
            backing_values[index] = .{
                .name = field.name,
                .value = if (Ident.textEql(label, "items"))
                    items_record_expr
                else if (Ident.textEql(label, "shortest_name"))
                    shortest_expr
                else if (Ident.textEql(label, "longest_name"))
                    longest_expr
                else
                    Common.invariant("generated FieldNames backing contained an unexpected field"),
            };
        }
        const backing_expr = try self.builder.program.addExpr(.{
            .ty = fields_backing_ty,
            .data = .{ .record = try self.builder.program.addFieldExprSpan(backing_values) },
        });
        const renamed_fields = try self.builder.program.addExpr(.{
            .ty = fields_ty,
            .data = .{ .nominal = backing_expr },
        });
        var body = renamed_fields;
        var index = info.item_fields.len;
        while (index > 0) {
            index -= 1;
            body = try self.wrapLet(
                renamed_name_locals[index],
                str_ty,
                renamed_name_exprs[index],
                body,
                fields_ty,
            );
            body = try self.wrapLet(
                item_locals[index],
                info.item_fields[index].ty,
                item_exprs[index],
                body,
                fields_ty,
            );
        }
        return try self.wrapLet(items_local, info.items_field.ty, items_expr, body, fields_ty);
    }

    fn lowerFieldNamesNameBound(
        self: *BodyContext,
        args: []const checked.CheckedExprId,
        arg_tys: []const Type.TypeId,
        ret_ty: Type.TypeId,
        bound: FieldNameBound,
    ) Allocator.Error!Ast.ExprId {
        if (args.len != 1 or arg_tys.len != 1) Common.invariant("FieldNames name bound reached Monotype with an unexpected arity");
        if (!self.typeHasBuiltinOwner(ret_ty, .u64)) Common.invariant("FieldNames name bound result was not U64");

        const shape_ty = self.fieldsShapeType(arg_tys[0]);
        const fields = self.recordFieldsForShape(shape_ty);
        const fields_value = try self.lowerParseIntrinsicArgAtType(args[0], arg_tys[0]);
        if (self.generatedFieldNamesBackingInfo(arg_tys[0])) |info| {
            const fields_local = try self.builder.program.addLocal(self.builder.symbols.fresh(), arg_tys[0]);
            const body = try self.lowerGeneratedFieldNamesNameBound(
                info,
                arg_tys[0],
                fields_local,
                ret_ty,
                bound,
            );
            return try self.wrapLet(fields_local, arg_tys[0], fields_value, body, ret_ty);
        }

        var value: u64 = 0;
        if (fields.len > 0) {
            value = @intCast(self.builder.program.names.recordFieldLabelText(fields[0].name).len);
            for (fields[1..]) |field| {
                const len: u64 = @intCast(self.builder.program.names.recordFieldLabelText(field.name).len);
                value = switch (bound) {
                    .shortest => @min(value, len),
                    .longest => @max(value, len),
                };
            }
        }

        const bound_expr = try self.builder.intLiteralExpr(value, ret_ty);
        return try self.wrapWildcardLet(arg_tys[0], fields_value, bound_expr, ret_ty);
    }

    fn lowerGeneratedFieldNamesNameBound(
        self: *BodyContext,
        info: GeneratedFieldNamesBackingInfo,
        fields_ty: Type.TypeId,
        fields_local: Ast.LocalId,
        ret_ty: Type.TypeId,
        bound: FieldNameBound,
    ) Allocator.Error!Ast.ExprId {
        const fields_backing_ty = self.builder.namedBackingType(fields_ty) orelse
            Common.invariant("generated FieldNames value expected a named backing type");
        const backing_local = try self.builder.program.addLocal(self.builder.symbols.fresh(), fields_backing_ty);
        const bound_field = switch (bound) {
            .shortest => info.shortest_field,
            .longest => info.longest_field,
        };
        if (!self.sameType(bound_field.ty, ret_ty)) Common.invariant("generated FieldNames bound metadata type differed from result type");
        const bound_expr = try self.builder.program.addExpr(.{
            .ty = ret_ty,
            .data = .{ .field_access = .{
                .receiver = try self.builder.localExpr(backing_local, fields_backing_ty),
                .field = bound_field.name,
            } },
        });
        const field_pat = try self.builder.program.addPat(.{
            .ty = fields_ty,
            .data = .{ .nominal = try self.builder.bindPat(backing_local, fields_backing_ty) },
        });
        const branch = Ast.Branch{ .pat = field_pat, .body = bound_expr };
        return try self.builder.program.addExpr(.{ .ty = ret_ty, .data = .{ .match_ = .{
            .scrutinee = try self.builder.localExpr(fields_local, fields_ty),
            .branches = try self.builder.program.addBranchSpan(&[_]Ast.Branch{branch}),
        } } });
    }

    fn lowerFieldName(
        self: *BodyContext,
        args: []const checked.CheckedExprId,
        arg_tys: []const Type.TypeId,
        ret_ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        if (args.len != 1 or arg_tys.len != 1) Common.invariant("Field.name reached Monotype with an unexpected arity");
        if (!self.typeHasBuiltinOwner(ret_ty, .str)) Common.invariant("Field.name result was not Str");

        const field_ty = arg_tys[0];
        const backing_ty = self.builder.namedBackingType(field_ty) orelse
            Common.invariant("Field.name argument had no backing type");
        const name_field = self.recordFieldByText(backing_ty, "name");
        if (!self.sameType(name_field.ty, ret_ty)) Common.invariant("Field.name backing name field differed from Str");

        const field_value = try self.lowerParseIntrinsicArgAtType(args[0], field_ty);
        const field_local = try self.builder.program.addLocal(self.builder.symbols.fresh(), field_ty);
        const backing_local = try self.builder.program.addLocal(self.builder.symbols.fresh(), backing_ty);
        const name_expr = try self.builder.program.addExpr(.{
            .ty = ret_ty,
            .data = .{ .field_access = .{
                .receiver = try self.builder.localExpr(backing_local, backing_ty),
                .field = name_field.name,
            } },
        });
        const field_pat = try self.builder.program.addPat(.{
            .ty = field_ty,
            .data = .{ .nominal = try self.builder.bindPat(backing_local, backing_ty) },
        });
        const branch = Ast.Branch{ .pat = field_pat, .body = name_expr };
        const matched = try self.builder.program.addExpr(.{ .ty = ret_ty, .data = .{ .match_ = .{
            .scrutinee = try self.builder.localExpr(field_local, field_ty),
            .branches = try self.builder.program.addBranchSpan(&[_]Ast.Branch{branch}),
        } } });
        return try self.wrapLet(field_local, field_ty, field_value, matched, ret_ty);
    }

    fn generatedFieldNamesBackingType(
        self: *BodyContext,
        field_count: usize,
        field_handle_ty: Type.TypeId,
    ) Allocator.Error!Type.TypeId {
        const item_fields = try self.allocator.alloc(Type.Field, field_count);
        defer self.allocator.free(item_fields);

        for (item_fields, 0..) |*field, index| {
            const label = try std.fmt.allocPrint(self.allocator, "field_{d}", .{index});
            defer self.allocator.free(label);

            field.* = .{
                .name = try self.builder.program.names.internRecordFieldLabel(label),
                .ty = field_handle_ty,
            };
        }

        const items_ty = try self.builder.program.types.add(.{
            .record = try self.builder.program.types.addFields(item_fields),
        });
        const u64_ty = try self.builder.primitiveType(.u64);
        const fields = [_]Type.Field{
            .{
                .name = try self.builder.program.names.internRecordFieldLabel("items"),
                .ty = items_ty,
            },
            .{
                .name = try self.builder.program.names.internRecordFieldLabel("shortest_name"),
                .ty = u64_ty,
            },
            .{
                .name = try self.builder.program.names.internRecordFieldLabel("longest_name"),
                .ty = u64_ty,
            },
        };

        return try self.builder.program.types.add(.{
            .record = try self.builder.program.types.addFields(&fields),
        });
    }

    fn generatedFieldHandleBackingType(self: *BodyContext) Allocator.Error!Type.TypeId {
        const fields = [_]Type.Field{
            .{
                .name = try self.builder.program.names.internRecordFieldLabel("index"),
                .ty = try self.builder.primitiveType(.u64),
            },
            .{
                .name = try self.builder.program.names.internRecordFieldLabel("name"),
                .ty = try self.builder.primitiveType(.str),
            },
            .{
                .name = try self.builder.program.names.internRecordFieldLabel("name_len"),
                .ty = try self.builder.primitiveType(.u64),
            },
        };

        return try self.builder.program.types.add(.{
            .record = try self.builder.program.types.addFields(&fields),
        });
    }

    fn lowerFieldNamesValue(
        self: *BodyContext,
        fields_ty: Type.TypeId,
        fields_backing_ty: Type.TypeId,
        field_handle_ty: Type.TypeId,
        renamed_field_locals: []const Ast.LocalId,
        renamed_field_lengths: ?[]const u32,
    ) Allocator.Error!Ast.ExprId {
        const info = self.generatedFieldNamesBackingInfoFromBacking(fields_backing_ty) orelse
            Common.invariant("generated FieldNames backing did not have the expected shape");
        if (info.item_fields.len != renamed_field_locals.len) Common.invariant("generated FieldNames backing arity differed from renamed field count");
        if (renamed_field_lengths) |lengths| {
            if (lengths.len != renamed_field_locals.len) Common.invariant("generated FieldNames backing length arity differed from renamed field count");
        }

        const str_ty = try self.builder.primitiveType(.str);
        const item_values = try self.allocator.alloc(Ast.FieldExpr, info.item_fields.len);
        defer self.allocator.free(item_values);

        for (info.item_fields, 0..) |field, index| {
            const name_expr = try self.builder.localExpr(renamed_field_locals[index], str_ty);
            const name_len_expr = if (renamed_field_lengths) |lengths|
                try self.builder.intLiteralExpr(lengths[index], try self.builder.primitiveType(.u64))
            else
                try self.builder.lowLevelExpr(
                    .str_count_utf8_bytes,
                    &.{try self.builder.localExpr(renamed_field_locals[index], str_ty)},
                    try self.builder.primitiveType(.u64),
                );
            item_values[index] = .{
                .name = field.name,
                .value = try self.lowerRecordFieldHandleWithName(field_handle_ty, name_expr, name_len_expr, index),
            };
        }

        const items_expr = try self.builder.program.addExpr(.{
            .ty = info.items_field.ty,
            .data = .{ .record = try self.builder.program.addFieldExprSpan(item_values) },
        });
        const bound_exprs = try self.fieldNameBoundExprsFromLocals(
            renamed_field_locals,
            renamed_field_lengths,
            info.shortest_field.ty,
        );
        const shortest_expr = bound_exprs[0];
        const longest_expr = bound_exprs[1];
        const backing_type_fields = self.builder.program.types.fieldSpan(self.builder.recordFieldsSpan(fields_backing_ty));
        const backing_values = try self.allocator.alloc(Ast.FieldExpr, backing_type_fields.len);
        defer self.allocator.free(backing_values);
        for (backing_type_fields, 0..) |field, index| {
            const label = self.builder.program.names.recordFieldLabelText(field.name);
            backing_values[index] = .{
                .name = field.name,
                .value = if (Ident.textEql(label, "items"))
                    items_expr
                else if (Ident.textEql(label, "shortest_name"))
                    shortest_expr
                else if (Ident.textEql(label, "longest_name"))
                    longest_expr
                else
                    Common.invariant("generated FieldNames backing contained an unexpected field"),
            };
        }

        const backing_expr = try self.builder.program.addExpr(.{
            .ty = fields_backing_ty,
            .data = .{ .record = try self.builder.program.addFieldExprSpan(backing_values) },
        });
        return try self.builder.program.addExpr(.{
            .ty = fields_ty,
            .data = .{ .nominal = backing_expr },
        });
    }

    fn fieldNameBoundExprsFromLocals(
        self: *BodyContext,
        renamed_field_locals: []const Ast.LocalId,
        renamed_field_lengths: ?[]const u32,
        u64_ty: Type.TypeId,
    ) Allocator.Error!struct { Ast.ExprId, Ast.ExprId } {
        if (renamed_field_lengths) |lengths| {
            if (lengths.len != renamed_field_locals.len) Common.invariant("renamed field length arity differed from locals");
            var shortest: u64 = 0;
            var longest: u64 = 0;
            if (lengths.len > 0) {
                shortest = lengths[0];
                longest = lengths[0];
                for (lengths[1..]) |len| {
                    shortest = @min(shortest, len);
                    longest = @max(longest, len);
                }
            }
            return .{
                try self.builder.intLiteralExpr(shortest, u64_ty),
                try self.builder.intLiteralExpr(longest, u64_ty),
            };
        }

        if (renamed_field_locals.len == 0) {
            const zero = try self.builder.intLiteralExpr(0, u64_ty);
            return .{ zero, zero };
        }

        return .{
            try self.fieldNameBoundExprFromLocals(renamed_field_locals, u64_ty, .shortest),
            try self.fieldNameBoundExprFromLocals(renamed_field_locals, u64_ty, .longest),
        };
    }

    fn fieldNameBoundExprFromLocals(
        self: *BodyContext,
        renamed_field_locals: []const Ast.LocalId,
        u64_ty: Type.TypeId,
        bound: FieldNameBound,
    ) Allocator.Error!Ast.ExprId {
        if (renamed_field_locals.len == 0) return try self.builder.intLiteralExpr(0, u64_ty);

        const str_ty = try self.builder.primitiveType(.str);
        var body = try self.builder.lowLevelExpr(
            .str_count_utf8_bytes,
            &.{try self.builder.localExpr(renamed_field_locals[0], str_ty)},
            u64_ty,
        );
        for (renamed_field_locals[1..]) |field_local| {
            const current_local = try self.builder.program.addLocal(self.builder.symbols.fresh(), u64_ty);
            const candidate_local = try self.builder.program.addLocal(self.builder.symbols.fresh(), u64_ty);
            const len_expr = try self.builder.lowLevelExpr(
                .str_count_utf8_bytes,
                &.{try self.builder.localExpr(field_local, str_ty)},
                u64_ty,
            );
            const cond = try self.builder.lowLevelExpr(switch (bound) {
                .shortest => .num_is_lt,
                .longest => .num_is_gt,
            }, &.{
                try self.builder.localExpr(candidate_local, u64_ty),
                try self.builder.localExpr(current_local, u64_ty),
            }, try self.builder.primitiveType(.bool));
            const selected = try self.builder.ifExpr(
                cond,
                try self.builder.localExpr(candidate_local, u64_ty),
                try self.builder.localExpr(current_local, u64_ty),
                u64_ty,
            );
            body = try self.wrapLet(
                current_local,
                u64_ty,
                body,
                try self.wrapLet(candidate_local, u64_ty, len_expr, selected, u64_ty),
                u64_ty,
            );
        }
        return body;
    }

    fn fieldHandleIndexExpr(
        self: *BodyContext,
        field_local: Ast.LocalId,
        field_ty: Type.TypeId,
        ret_ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        if (!self.typeHasBuiltinOwner(ret_ty, .u64)) Common.invariant("Field index result was not U64");
        const backing_ty = self.builder.namedBackingType(field_ty) orelse
            Common.invariant("Field index argument had no backing type");
        const index_field = self.recordFieldByText(backing_ty, "index");
        if (!self.sameType(index_field.ty, ret_ty)) Common.invariant("Field backing index field differed from U64");

        const backing_local = try self.builder.program.addLocal(self.builder.symbols.fresh(), backing_ty);
        const index_expr = try self.builder.program.addExpr(.{
            .ty = ret_ty,
            .data = .{ .field_access = .{
                .receiver = try self.builder.localExpr(backing_local, backing_ty),
                .field = index_field.name,
            } },
        });
        const field_pat = try self.builder.program.addPat(.{
            .ty = field_ty,
            .data = .{ .nominal = try self.builder.bindPat(backing_local, backing_ty) },
        });
        const branch = Ast.Branch{ .pat = field_pat, .body = index_expr };
        return try self.builder.program.addExpr(.{ .ty = ret_ty, .data = .{ .match_ = .{
            .scrutinee = try self.builder.localExpr(field_local, field_ty),
            .branches = try self.builder.program.addBranchSpan(&[_]Ast.Branch{branch}),
        } } });
    }

    fn fieldNameLenFromLocal(
        self: *BodyContext,
        field_local: Ast.LocalId,
        field_ty: Type.TypeId,
        ret_ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        if (!self.typeHasBuiltinOwner(ret_ty, .u64)) Common.invariant("Field name length result was not U64");
        const backing_ty = self.builder.namedBackingType(field_ty) orelse
            Common.invariant("Field name length argument had no backing type");
        const name_len_field = self.recordFieldByText(backing_ty, "name_len");
        if (!self.sameType(name_len_field.ty, ret_ty)) Common.invariant("Field backing name_len field differed from U64");

        const backing_local = try self.builder.program.addLocal(self.builder.symbols.fresh(), backing_ty);
        const name_len_expr = try self.builder.program.addExpr(.{
            .ty = ret_ty,
            .data = .{ .field_access = .{
                .receiver = try self.builder.localExpr(backing_local, backing_ty),
                .field = name_len_field.name,
            } },
        });
        const field_pat = try self.builder.program.addPat(.{
            .ty = field_ty,
            .data = .{ .nominal = try self.builder.bindPat(backing_local, backing_ty) },
        });
        const branch = Ast.Branch{ .pat = field_pat, .body = name_len_expr };
        return try self.builder.program.addExpr(.{ .ty = ret_ty, .data = .{ .match_ = .{
            .scrutinee = try self.builder.localExpr(field_local, field_ty),
            .branches = try self.builder.program.addBranchSpan(&[_]Ast.Branch{branch}),
        } } });
    }

    fn lowerFieldNamesIter(
        self: *BodyContext,
        checked_expr_id: checked.CheckedExprId,
        checked_source_ty: checked.CheckedTypeId,
        args: []const checked.CheckedExprId,
        arg_tys: []const Type.TypeId,
        ret_ty: Type.TypeId,
        mode: FieldNamesIterMode,
    ) Allocator.Error!Ast.ExprId {
        const expected_arity: usize = switch (mode) {
            .all => 1,
            .for_size => 2,
        };
        if (args.len != expected_arity or arg_tys.len != expected_arity) Common.invariant("FieldNames iterator reached Monotype with an unexpected arity");

        const shape_ty = self.fieldsShapeType(arg_tys[0]);
        const fields = self.recordFieldsForShape(shape_ty);
        const backing_ty = self.builder.namedBackingType(ret_ty) orelse
            Common.invariant("FieldNames iterator result was not an Iter nominal type");
        const len_field = self.recordFieldByText(backing_ty, "len_if_known");
        const step_field = self.recordFieldByText(backing_ty, "step");
        const step_fn_ty = step_field.ty;
        const step_shape = self.builder.functionShape(step_fn_ty, "FieldNames iterator step field was not a function");
        if (self.builder.program.types.span(step_shape.args).len != 0) {
            Common.invariant("FieldNames iterator step function was not zero-argument");
        }
        const field_handle_ty = self.iterItemType(ret_ty);

        const fields_value = try self.lowerParseIntrinsicArgAtType(args[0], arg_tys[0]);
        const fields_local = try self.builder.program.addLocal(self.builder.symbols.fresh(), arg_tys[0]);
        const size_local: ?Ast.LocalId = if (mode == .for_size) blk: {
            if (!self.typeHasBuiltinOwner(arg_tys[1], .u64)) Common.invariant("FieldNames.for_size size argument was not U64");
            break :blk try self.builder.program.addLocal(self.builder.symbols.fresh(), arg_tys[1]);
        } else null;

        var iter_expr = if (self.generatedFieldNamesBackingFieldNames(arg_tys[0], field_handle_ty, fields.len)) |backing_fields|
            try self.lowerFieldNamesValueIter(
                backing_fields,
                arg_tys[0],
                fields_local,
                field_handle_ty,
                ret_ty,
                backing_ty,
                len_field.ty,
                step_fn_ty,
                step_shape.ret,
                size_local,
                arg_tys,
                checked_source_ty,
                checked_expr_id,
            )
        else if (mode == .all)
            try self.lowerFieldNamesStaticIterFromIndex(
                fields,
                0,
                field_handle_ty,
                ret_ty,
                backing_ty,
                len_field.ty,
                step_fn_ty,
                step_shape.ret,
                checked_source_ty,
                checked_expr_id,
            )
        else
            try self.lowerFieldNamesForSizeIter(
                fields,
                field_handle_ty,
                ret_ty,
                backing_ty,
                len_field.ty,
                step_fn_ty,
                step_shape.ret,
                size_local.?,
                arg_tys[1],
                checked_source_ty,
                checked_expr_id,
            );
        if (mode == .for_size) {
            const size_value = try self.lowerParseIntrinsicArgAtType(args[1], arg_tys[1]);
            iter_expr = try self.wrapLet(size_local.?, arg_tys[1], size_value, iter_expr, ret_ty);
        }
        return try self.wrapLet(fields_local, arg_tys[0], fields_value, iter_expr, ret_ty);
    }

    const GeneratedFieldNamesBackingInfo = struct {
        items_field: Type.Field,
        shortest_field: Type.Field,
        longest_field: Type.Field,
        item_fields: []const Type.Field,
    };

    fn generatedFieldNamesBackingInfo(
        self: *BodyContext,
        fields_ty: Type.TypeId,
    ) ?GeneratedFieldNamesBackingInfo {
        if (!self.typeHasBuiltinOwner(fields_ty, .fields)) return null;
        const backing_ty = self.builder.namedBackingType(fields_ty) orelse return null;
        return self.generatedFieldNamesBackingInfoFromBacking(backing_ty);
    }

    fn generatedFieldNamesBackingInfoFromBacking(
        self: *BodyContext,
        backing_ty: Type.TypeId,
    ) ?GeneratedFieldNamesBackingInfo {
        const items_field = self.recordFieldByTextOptional(backing_ty, "items") orelse return null;
        const shortest_field = self.recordFieldByTextOptional(backing_ty, "shortest_name") orelse return null;
        const longest_field = self.recordFieldByTextOptional(backing_ty, "longest_name") orelse return null;
        if (!self.typeHasBuiltinOwner(shortest_field.ty, .u64)) return null;
        if (!self.typeHasBuiltinOwner(longest_field.ty, .u64)) return null;
        const item_fields = switch (self.builder.shapeContent(items_field.ty)) {
            .record => |span| self.builder.program.types.fieldSpan(span),
            .zst => &.{},
            else => return null,
        };
        return .{
            .items_field = items_field,
            .shortest_field = shortest_field,
            .longest_field = longest_field,
            .item_fields = item_fields,
        };
    }

    fn generatedFieldNamesBackingFieldNames(
        self: *BodyContext,
        fields_ty: Type.TypeId,
        field_handle_ty: Type.TypeId,
        expected_count: usize,
    ) ?[]const Type.Field {
        const info = self.generatedFieldNamesBackingInfo(fields_ty) orelse return null;
        if (info.item_fields.len != expected_count) return null;
        for (info.item_fields) |field| {
            if (!self.sameType(field.ty, field_handle_ty)) return null;
        }
        return info.item_fields;
    }

    fn generatedFieldNamesBackingValueFieldNames(
        self: *BodyContext,
        fields_ty: Type.TypeId,
    ) ?[]const Type.Field {
        const info = self.generatedFieldNamesBackingInfo(fields_ty) orelse return null;
        for (info.item_fields) |field| {
            if (!self.typeHasBuiltinOwner(field.ty, .field)) return null;
        }
        return info.item_fields;
    }

    fn isGeneratedFieldNamesEvidenceType(self: *BodyContext, fields_ty: Type.TypeId) bool {
        return self.generatedFieldNamesBackingInfo(fields_ty) != null;
    }

    const GeneratedParseTagUnionSpecBackingInfo = struct {
        record_fields: []const Type.Field,
    };

    fn generatedParseTagUnionSpecBackingInfo(
        self: *BodyContext,
        spec_ty: Type.TypeId,
    ) ?GeneratedParseTagUnionSpecBackingInfo {
        if (!self.typeHasBuiltinOwner(spec_ty, .parse_tag_union_spec)) return null;
        const backing_ty = self.builder.namedBackingType(spec_ty) orelse return null;
        const record_fields = switch (self.builder.shapeContent(backing_ty)) {
            .record => |span| self.builder.program.types.fieldSpan(span),
            .zst => &.{},
            else => return null,
        };
        if (record_fields.len == 0) return null;
        return .{ .record_fields = record_fields };
    }

    fn isGeneratedParseTagUnionSpecEvidenceType(self: *BodyContext, spec_ty: Type.TypeId) bool {
        return self.generatedParseTagUnionSpecBackingInfo(spec_ty) != null;
    }

    fn isGeneratedOpaqueEvidenceType(self: *BodyContext, ty: Type.TypeId) bool {
        return self.isGeneratedFieldNamesEvidenceType(ty) or self.isGeneratedParseTagUnionSpecEvidenceType(ty);
    }

    fn publicOpaqueUnificationType(self: *BodyContext, ty: Type.TypeId) Allocator.Error!Type.TypeId {
        if (!self.isGeneratedOpaqueEvidenceType(ty)) return ty;
        return switch (self.builder.program.types.get(ty)) {
            .named => |named| try self.builder.program.types.add(.{ .named = .{
                .named_type = named.named_type,
                .def = named.def,
                .kind = named.kind,
                .builtin_owner = named.builtin_owner,
                .args = named.args,
                .backing = .{
                    .ty = try self.builder.program.types.add(.{ .record = .empty() }),
                    .use = if (named.backing) |backing| backing.use else .runtime_layout_only,
                },
                .declared_order = named.declared_order,
            } }),
            else => ty,
        };
    }

    fn publicOpaqueFunctionUnificationType(self: *BodyContext, fn_ty: Type.TypeId) Allocator.Error!Type.TypeId {
        const function = self.builder.functionShape(fn_ty, "public opaque unification requested for a non-function type");
        const args = self.builder.program.types.span(function.args);
        var changed = false;
        const public_args = try self.allocator.alloc(Type.TypeId, args.len);
        defer self.allocator.free(public_args);
        for (args, 0..) |arg, index| {
            public_args[index] = try self.publicOpaqueUnificationType(arg);
            if (public_args[index] != arg) changed = true;
        }
        if (!changed) return fn_ty;
        return try self.builder.program.types.add(.{ .func = .{
            .args = try self.builder.program.types.addSpan(public_args),
            .ret = function.ret,
        } });
    }

    fn lowerFieldNamesValueIter(
        self: *BodyContext,
        backing_fields: []const Type.Field,
        fields_ty: Type.TypeId,
        fields_local: Ast.LocalId,
        field_handle_ty: Type.TypeId,
        iter_ty: Type.TypeId,
        iter_backing_ty: Type.TypeId,
        len_ty: Type.TypeId,
        step_fn_ty: Type.TypeId,
        step_ret_ty: Type.TypeId,
        size_local: ?Ast.LocalId,
        arg_tys: []const Type.TypeId,
        checked_source_ty: checked.CheckedTypeId,
        source_expr_id: checked.CheckedExprId,
    ) Allocator.Error!Ast.ExprId {
        const fields_backing_ty = self.builder.namedBackingType(fields_ty) orelse
            Common.invariant("generated FieldNames value expected a named backing type");
        const info = self.generatedFieldNamesBackingInfoFromBacking(fields_backing_ty) orelse
            Common.invariant("generated FieldNames value expected a generated backing type");
        if (backing_fields.len != info.item_fields.len) Common.invariant("generated FieldNames iterator arity differed from item count");
        const backing_local = try self.builder.program.addLocal(self.builder.symbols.fresh(), fields_backing_ty);
        const items_expr = try self.builder.program.addExpr(.{
            .ty = info.items_field.ty,
            .data = .{ .field_access = .{
                .receiver = try self.builder.localExpr(backing_local, fields_backing_ty),
                .field = info.items_field.name,
            } },
        });
        const items_local = try self.builder.program.addLocal(self.builder.symbols.fresh(), info.items_field.ty);
        const body = try self.lowerFieldNamesValueIterFromIndex(
            info.item_fields,
            info.items_field.ty,
            items_local,
            0,
            field_handle_ty,
            iter_ty,
            iter_backing_ty,
            len_ty,
            step_fn_ty,
            step_ret_ty,
            size_local,
            arg_tys,
            checked_source_ty,
            source_expr_id,
        );
        const body_with_items = try self.wrapLet(items_local, info.items_field.ty, items_expr, body, iter_ty);
        const field_pat = try self.builder.program.addPat(.{
            .ty = fields_ty,
            .data = .{ .nominal = try self.builder.bindPat(backing_local, fields_backing_ty) },
        });
        const branch = Ast.Branch{ .pat = field_pat, .body = body_with_items };
        return try self.builder.program.addExpr(.{ .ty = iter_ty, .data = .{ .match_ = .{
            .scrutinee = try self.builder.localExpr(fields_local, fields_ty),
            .branches = try self.builder.program.addBranchSpan(&[_]Ast.Branch{branch}),
        } } });
    }

    fn lowerFieldNamesValueIterFromIndex(
        self: *BodyContext,
        backing_fields: []const Type.Field,
        items_ty: Type.TypeId,
        items_local: Ast.LocalId,
        index: usize,
        field_handle_ty: Type.TypeId,
        iter_ty: Type.TypeId,
        iter_backing_ty: Type.TypeId,
        len_ty: Type.TypeId,
        step_fn_ty: Type.TypeId,
        step_ret_ty: Type.TypeId,
        size_local: ?Ast.LocalId,
        arg_tys: []const Type.TypeId,
        checked_source_ty: checked.CheckedTypeId,
        source_expr_id: checked.CheckedExprId,
    ) Allocator.Error!Ast.ExprId {
        const len_expr = try self.lowerFieldNamesIterLen(backing_fields.len - index, len_ty, if (size_local == null) .all else .for_size);

        if (index == backing_fields.len) {
            const step_expr = try self.lowerFieldNamesDoneStep(checked_source_ty, source_expr_id, index, if (size_local == null) .all else .for_size, step_fn_ty, step_ret_ty);
            return try self.lowerInterpolationIterRecord(iter_ty, iter_backing_ty, len_expr, step_expr);
        }

        const rest_expr = try self.lowerFieldNamesValueIterFromIndex(
            backing_fields,
            items_ty,
            items_local,
            index + 1,
            field_handle_ty,
            iter_ty,
            iter_backing_ty,
            len_ty,
            step_fn_ty,
            step_ret_ty,
            size_local,
            arg_tys,
            checked_source_ty,
            source_expr_id,
        );
        const item_expr = try self.builder.program.addExpr(.{
            .ty = field_handle_ty,
            .data = .{ .field_access = .{
                .receiver = try self.builder.localExpr(items_local, items_ty),
                .field = backing_fields[index].name,
            } },
        });
        const step_expr = if (size_local) |local|
            try self.lowerFieldNamesSizeFilteredStep(
                checked_source_ty,
                source_expr_id,
                index,
                item_expr,
                rest_expr,
                step_fn_ty,
                step_ret_ty,
                local,
                arg_tys[1],
            )
        else
            try self.lowerFieldNamesOneStep(
                checked_source_ty,
                source_expr_id,
                index,
                item_expr,
                rest_expr,
                step_fn_ty,
                step_ret_ty,
            );
        return try self.lowerInterpolationIterRecord(iter_ty, iter_backing_ty, len_expr, step_expr);
    }

    fn lowerFieldNamesStaticIterFromIndex(
        self: *BodyContext,
        fields: []const Type.Field,
        index: usize,
        field_handle_ty: Type.TypeId,
        iter_ty: Type.TypeId,
        backing_ty: Type.TypeId,
        len_ty: Type.TypeId,
        step_fn_ty: Type.TypeId,
        step_ret_ty: Type.TypeId,
        checked_source_ty: checked.CheckedTypeId,
        source_expr_id: checked.CheckedExprId,
    ) Allocator.Error!Ast.ExprId {
        const len_expr = try self.lowerFieldNamesIterLen(fields.len - index, len_ty, .all);

        if (index == fields.len) {
            const step_expr = try self.lowerFieldNamesDoneStep(checked_source_ty, source_expr_id, index, .all, step_fn_ty, step_ret_ty);
            return try self.lowerInterpolationIterRecord(iter_ty, backing_ty, len_expr, step_expr);
        }

        const rest_expr = try self.lowerFieldNamesStaticIterFromIndex(
            fields,
            index + 1,
            field_handle_ty,
            iter_ty,
            backing_ty,
            len_ty,
            step_fn_ty,
            step_ret_ty,
            checked_source_ty,
            source_expr_id,
        );
        const item_expr = try self.lowerRecordFieldHandle(field_handle_ty, fields[index], index);
        const step_expr = try self.lowerFieldNamesOneStep(
            checked_source_ty,
            source_expr_id,
            index,
            item_expr,
            rest_expr,
            step_fn_ty,
            step_ret_ty,
        );
        return try self.lowerInterpolationIterRecord(iter_ty, backing_ty, len_expr, step_expr);
    }

    fn lowerFieldNamesForSizeIter(
        self: *BodyContext,
        fields: []const Type.Field,
        field_handle_ty: Type.TypeId,
        iter_ty: Type.TypeId,
        backing_ty: Type.TypeId,
        len_ty: Type.TypeId,
        step_fn_ty: Type.TypeId,
        step_ret_ty: Type.TypeId,
        size_local: Ast.LocalId,
        size_ty: Type.TypeId,
        checked_source_ty: checked.CheckedTypeId,
        source_expr_id: checked.CheckedExprId,
    ) Allocator.Error!Ast.ExprId {
        var body = try self.lowerFieldNamesStaticIterForLen(
            fields,
            field_handle_ty,
            iter_ty,
            backing_ty,
            len_ty,
            step_fn_ty,
            step_ret_ty,
            checked_source_ty,
            source_expr_id,
            null,
        );

        var seen = std.ArrayList(usize).empty;
        defer seen.deinit(self.allocator);
        for (fields) |field| {
            const len = self.builder.program.names.recordFieldLabelText(field.name).len;
            var already_seen = false;
            for (seen.items) |seen_len| {
                if (seen_len == len) {
                    already_seen = true;
                    break;
                }
            }
            if (already_seen) continue;
            try seen.append(self.allocator, len);

            const bucket = try self.lowerFieldNamesStaticIterForLen(
                fields,
                field_handle_ty,
                iter_ty,
                backing_ty,
                len_ty,
                step_fn_ty,
                step_ret_ty,
                checked_source_ty,
                source_expr_id,
                len,
            );
            const size_expr = try self.builder.localExpr(size_local, size_ty);
            const len_expr = try self.builder.intLiteralExpr(@intCast(len), size_ty);
            const cond = try self.builder.lowLevelExpr(.num_is_eq, &.{ size_expr, len_expr }, try self.builder.primitiveType(.bool));
            body = try self.builder.ifExpr(cond, bucket, body, iter_ty);
        }
        return body;
    }

    fn lowerFieldNamesStaticIterForLen(
        self: *BodyContext,
        fields: []const Type.Field,
        field_handle_ty: Type.TypeId,
        iter_ty: Type.TypeId,
        backing_ty: Type.TypeId,
        len_ty: Type.TypeId,
        step_fn_ty: Type.TypeId,
        step_ret_ty: Type.TypeId,
        checked_source_ty: checked.CheckedTypeId,
        source_expr_id: checked.CheckedExprId,
        target_len: ?usize,
    ) Allocator.Error!Ast.ExprId {
        return try self.lowerFieldNamesStaticFilteredIterFromIndex(
            fields,
            0,
            target_len,
            field_handle_ty,
            iter_ty,
            backing_ty,
            len_ty,
            step_fn_ty,
            step_ret_ty,
            checked_source_ty,
            source_expr_id,
        );
    }

    fn lowerFieldNamesStaticFilteredIterFromIndex(
        self: *BodyContext,
        fields: []const Type.Field,
        start_index: usize,
        target_len: ?usize,
        field_handle_ty: Type.TypeId,
        iter_ty: Type.TypeId,
        backing_ty: Type.TypeId,
        len_ty: Type.TypeId,
        step_fn_ty: Type.TypeId,
        step_ret_ty: Type.TypeId,
        checked_source_ty: checked.CheckedTypeId,
        source_expr_id: checked.CheckedExprId,
    ) Allocator.Error!Ast.ExprId {
        const next_index = self.nextFieldIndexForLen(fields, start_index, target_len);
        const remaining = self.countFieldNamesForLenFrom(fields, start_index, target_len);
        const len_expr = try self.lowerFieldNamesIterLen(remaining, len_ty, .all);

        if (next_index == null) {
            const step_expr = try self.lowerFieldNamesDoneStep(checked_source_ty, source_expr_id, start_index, .all, step_fn_ty, step_ret_ty);
            return try self.lowerInterpolationIterRecord(iter_ty, backing_ty, len_expr, step_expr);
        }

        const index = next_index.?;
        const rest_expr = try self.lowerFieldNamesStaticFilteredIterFromIndex(
            fields,
            index + 1,
            target_len,
            field_handle_ty,
            iter_ty,
            backing_ty,
            len_ty,
            step_fn_ty,
            step_ret_ty,
            checked_source_ty,
            source_expr_id,
        );
        const item_expr = try self.lowerRecordFieldHandle(field_handle_ty, fields[index], index);
        const step_expr = try self.lowerFieldNamesOneStep(
            checked_source_ty,
            source_expr_id,
            index,
            item_expr,
            rest_expr,
            step_fn_ty,
            step_ret_ty,
        );
        return try self.lowerInterpolationIterRecord(iter_ty, backing_ty, len_expr, step_expr);
    }

    fn nextFieldIndexForLen(
        self: *BodyContext,
        fields: []const Type.Field,
        start_index: usize,
        target_len: ?usize,
    ) ?usize {
        var index = start_index;
        while (index < fields.len) : (index += 1) {
            if (target_len) |len| {
                if (self.builder.program.names.recordFieldLabelText(fields[index].name).len != len) continue;
            }
            return index;
        }
        return null;
    }

    fn countFieldNamesForLenFrom(
        self: *BodyContext,
        fields: []const Type.Field,
        start_index: usize,
        target_len: ?usize,
    ) usize {
        var count: usize = 0;
        for (fields[start_index..]) |field| {
            if (target_len) |len| {
                if (self.builder.program.names.recordFieldLabelText(field.name).len != len) continue;
            }
            count += 1;
        }
        return count;
    }

    fn lowerFieldNamesIterLen(
        self: *BodyContext,
        remaining: usize,
        ty: Type.TypeId,
        mode: FieldNamesIterMode,
    ) Allocator.Error!Ast.ExprId {
        return switch (mode) {
            .all => try self.lowerInterpolationLenIfKnown(remaining, ty),
            .for_size => blk: {
                const unknown_tag = self.monoTagByText(ty, "Unknown");
                break :blk try self.builder.program.addExpr(.{ .ty = ty, .data = .{ .tag = .{
                    .name = unknown_tag.name,
                    .payloads = .empty(),
                } } });
            },
        };
    }

    fn lowerFieldNamesDoneStep(
        self: *BodyContext,
        checked_source_ty: checked.CheckedTypeId,
        source_expr_id: checked.CheckedExprId,
        index: usize,
        mode: FieldNamesIterMode,
        step_fn_ty: Type.TypeId,
        step_ret_ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        const done_tag = self.monoTagByText(step_ret_ty, "Done");
        const body = try self.builder.program.addExpr(.{ .ty = step_ret_ty, .data = .{ .tag = .{
            .name = done_tag.name,
            .payloads = .empty(),
        } } });
        return try self.lowerFieldNamesStepLambda(checked_source_ty, source_expr_id, index, mode, step_fn_ty, body);
    }

    fn lowerFieldNamesOneStep(
        self: *BodyContext,
        checked_source_ty: checked.CheckedTypeId,
        source_expr_id: checked.CheckedExprId,
        index: usize,
        item_expr: Ast.ExprId,
        rest_expr: Ast.ExprId,
        step_fn_ty: Type.TypeId,
        step_ret_ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        const one_tag = self.monoTagByText(step_ret_ty, "One");
        const one_payloads = self.builder.program.types.span(one_tag.payloads);
        if (one_payloads.len != 1) Common.invariant("Iter step One tag did not have one record payload");
        const one_payload = try self.lowerInterpolationOnePayload(one_payloads[0], item_expr, rest_expr);
        const one_body = try self.builder.program.addExpr(.{ .ty = step_ret_ty, .data = .{ .tag = .{
            .name = one_tag.name,
            .payloads = try self.builder.program.addExprSpan(&[_]Ast.ExprId{one_payload}),
        } } });

        return try self.lowerFieldNamesStepLambda(checked_source_ty, source_expr_id, index, .all, step_fn_ty, one_body);
    }

    fn lowerFieldNamesSizeFilteredStep(
        self: *BodyContext,
        checked_source_ty: checked.CheckedTypeId,
        source_expr_id: checked.CheckedExprId,
        index: usize,
        item_expr: Ast.ExprId,
        rest_expr: Ast.ExprId,
        step_fn_ty: Type.TypeId,
        step_ret_ty: Type.TypeId,
        size_local: Ast.LocalId,
        size_ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        const field_handle_ty = self.builder.program.exprs.items[@intFromEnum(item_expr)].ty;
        const item_local = try self.builder.program.addLocal(self.builder.symbols.fresh(), field_handle_ty);
        const item_local_expr = try self.builder.localExpr(item_local, field_handle_ty);

        const one_tag = self.monoTagByText(step_ret_ty, "One");
        const one_payloads = self.builder.program.types.span(one_tag.payloads);
        if (one_payloads.len != 1) Common.invariant("Iter step One tag did not have one record payload");
        const one_payload = try self.lowerInterpolationOnePayload(one_payloads[0], item_local_expr, rest_expr);
        const one_body = try self.builder.program.addExpr(.{ .ty = step_ret_ty, .data = .{ .tag = .{
            .name = one_tag.name,
            .payloads = try self.builder.program.addExprSpan(&[_]Ast.ExprId{one_payload}),
        } } });

        const skip_tag = self.monoTagByText(step_ret_ty, "Skip");
        const skip_payloads = self.builder.program.types.span(skip_tag.payloads);
        if (skip_payloads.len != 1) Common.invariant("Iter step Skip tag did not have one record payload");
        const skip_payload = try self.lowerInterpolationSkipPayload(skip_payloads[0], rest_expr);
        const skip_body = try self.builder.program.addExpr(.{ .ty = step_ret_ty, .data = .{ .tag = .{
            .name = skip_tag.name,
            .payloads = try self.builder.program.addExprSpan(&[_]Ast.ExprId{skip_payload}),
        } } });

        const len_expr = try self.fieldNameLenFromLocal(item_local, field_handle_ty, size_ty);
        const cond = try self.builder.lowLevelExpr(.num_is_eq, &.{
            len_expr,
            try self.builder.localExpr(size_local, size_ty),
        }, try self.builder.primitiveType(.bool));
        const body = try self.wrapLet(
            item_local,
            field_handle_ty,
            item_expr,
            try self.builder.ifExpr(cond, one_body, skip_body, step_ret_ty),
            step_ret_ty,
        );
        return try self.lowerFieldNamesStepLambda(checked_source_ty, source_expr_id, index, .for_size, step_fn_ty, body);
    }

    fn lowerInterpolationSkipPayload(
        self: *BodyContext,
        ty: Type.TypeId,
        rest_expr: Ast.ExprId,
    ) Allocator.Error!Ast.ExprId {
        const fields = self.builder.program.types.fieldSpan(self.builder.recordFieldsSpan(ty));
        const lowered = try self.allocator.alloc(Ast.FieldExpr, fields.len);
        defer self.allocator.free(lowered);

        for (fields, 0..) |field, i| {
            const label = self.builder.program.names.recordFieldLabelText(field.name);
            lowered[i] = .{
                .name = field.name,
                .value = if (Ident.textEql(label, "rest"))
                    rest_expr
                else
                    Common.invariant("Iter step Skip payload contained an unexpected field"),
            };
        }

        return try self.builder.program.addExpr(.{
            .ty = ty,
            .data = .{ .record = try self.builder.program.addFieldExprSpan(lowered) },
        });
    }

    fn fieldNameFromLocal(
        self: *BodyContext,
        field_local: Ast.LocalId,
        field_ty: Type.TypeId,
        ret_ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        const backing_ty = self.builder.namedBackingType(field_ty) orelse
            Common.invariant("Field.name argument had no backing type");
        const name_field = self.recordFieldByText(backing_ty, "name");
        if (!self.sameType(name_field.ty, ret_ty)) Common.invariant("Field.name backing name field differed from Str");

        const backing_local = try self.builder.program.addLocal(self.builder.symbols.fresh(), backing_ty);
        const name_expr = try self.builder.program.addExpr(.{
            .ty = ret_ty,
            .data = .{ .field_access = .{
                .receiver = try self.builder.localExpr(backing_local, backing_ty),
                .field = name_field.name,
            } },
        });
        const field_pat = try self.builder.program.addPat(.{
            .ty = field_ty,
            .data = .{ .nominal = try self.builder.bindPat(backing_local, backing_ty) },
        });
        const branch = Ast.Branch{ .pat = field_pat, .body = name_expr };
        return try self.builder.program.addExpr(.{ .ty = ret_ty, .data = .{ .match_ = .{
            .scrutinee = try self.builder.localExpr(field_local, field_ty),
            .branches = try self.builder.program.addBranchSpan(&[_]Ast.Branch{branch}),
        } } });
    }

    fn lowerFieldNamesStepLambda(
        self: *BodyContext,
        checked_source_ty: checked.CheckedTypeId,
        source_expr_id: checked.CheckedExprId,
        index: usize,
        mode: FieldNamesIterMode,
        step_fn_ty: Type.TypeId,
        body: Ast.ExprId,
    ) Allocator.Error!Ast.ExprId {
        const fn_id = try self.builder.program.addFn(.{
            .fn_def = .{ .checked_generated = self.owner_template },
            .source_fn_ty = checked_source_ty,
            .source_fn_key = generatedFieldNamesIterStepKey(self.current_fn_key, source_expr_id, index, mode),
            .mono_fn_ty = step_fn_ty,
        });
        return try self.builder.program.addExpr(.{ .ty = step_fn_ty, .data = .{ .lambda = .{
            .fn_id = fn_id,
            .args = try self.builder.program.addTypedLocalSpan(&.{}),
            .body = body,
        } } });
    }

    fn lowerRecordFieldHandle(
        self: *BodyContext,
        field_handle_ty: Type.TypeId,
        record_field: Type.Field,
        index: usize,
    ) Allocator.Error!Ast.ExprId {
        const str_ty = try self.builder.primitiveType(.str);
        const field_text = self.builder.program.names.recordFieldLabelText(record_field.name);
        return try self.lowerRecordFieldHandleWithName(
            field_handle_ty,
            try self.builder.stringExpr(field_text, str_ty),
            try self.builder.intLiteralExpr(@intCast(field_text.len), try self.builder.primitiveType(.u64)),
            index,
        );
    }

    fn lowerRecordFieldHandleWithName(
        self: *BodyContext,
        field_handle_ty: Type.TypeId,
        name_expr: Ast.ExprId,
        name_len_expr: Ast.ExprId,
        index: usize,
    ) Allocator.Error!Ast.ExprId {
        const backing_ty = self.builder.namedBackingType(field_handle_ty) orelse
            Common.invariant("generated Field handle expected a named backing type");
        const fields = self.builder.program.types.fieldSpan(self.builder.recordFieldsSpan(backing_ty));
        const lowered = try self.allocator.alloc(Ast.FieldExpr, fields.len);
        defer self.allocator.free(lowered);

        var saw_name = false;
        var saw_index = false;
        var saw_name_len = false;
        for (fields, 0..) |field, i| {
            const label = self.builder.program.names.recordFieldLabelText(field.name);
            lowered[i] = .{
                .name = field.name,
                .value = if (Ident.textEql(label, "name")) blk: {
                    saw_name = true;
                    if (!self.typeHasBuiltinOwner(field.ty, .str)) Common.invariant("Field backing name was not Str");
                    break :blk name_expr;
                } else if (Ident.textEql(label, "index")) blk: {
                    saw_index = true;
                    if (!self.typeHasBuiltinOwner(field.ty, .u64)) Common.invariant("Field backing index was not U64");
                    break :blk try self.builder.intLiteralExpr(@intCast(index), field.ty);
                } else if (Ident.textEql(label, "name_len")) blk: {
                    saw_name_len = true;
                    if (!self.typeHasBuiltinOwner(field.ty, .u64)) Common.invariant("Field backing name_len was not U64");
                    break :blk name_len_expr;
                } else {
                    Common.invariant("Field backing record contained an unexpected field");
                },
            };
        }
        if (!saw_name or !saw_index or !saw_name_len) Common.invariant("Field backing record did not contain name, index, and name_len");

        const backing_expr = try self.builder.program.addExpr(.{
            .ty = backing_ty,
            .data = .{ .record = try self.builder.program.addFieldExprSpan(lowered) },
        });
        return try self.builder.program.addExpr(.{
            .ty = field_handle_ty,
            .data = .{ .nominal = backing_expr },
        });
    }

    fn fieldsShapeType(self: *BodyContext, fields_ty: Type.TypeId) Type.TypeId {
        if (!self.typeHasBuiltinOwner(fields_ty, .fields)) Common.invariant("expected FieldNames(_shape) type");
        return switch (self.builder.program.types.get(fields_ty)) {
            .named => |named| blk: {
                const args = self.builder.program.types.span(named.args);
                if (args.len != 1) Common.invariant("FieldNames nominal did not have one type argument");
                break :blk args[0];
            },
            else => Common.invariant("FieldNames value was not a named type"),
        };
    }

    fn recordFieldsForShape(self: *BodyContext, shape_ty: Type.TypeId) []const Type.Field {
        return switch (self.builder.shapeContent(shape_ty)) {
            .record => |span| self.builder.program.types.fieldSpan(span),
            .zst => &.{},
            else => Common.invariant("FieldNames metadata requested for a non-record shape"),
        };
    }

    fn parserRecordFieldRank(
        self: *BodyContext,
        fields: []const Type.Field,
        field_index: usize,
    ) usize {
        if (field_index >= fields.len) Common.invariant("parser field capture requested for a missing record field");
        const target = self.builder.program.names.recordFieldLabelText(fields[field_index].name);
        var rank: usize = 0;
        for (fields, 0..) |field, index| {
            if (index == field_index) continue;
            const name = self.builder.program.names.recordFieldLabelText(field.name);
            if (std.mem.order(u8, name, target) == .lt) rank += 1;
        }
        return rank;
    }

    fn parserFieldCaptureIdForRecordField(
        self: *BodyContext,
        fields: []const Type.Field,
        field_index: usize,
        base_capture_id: usize,
    ) u32 {
        const capture_id = base_capture_id + self.parserRecordFieldRank(fields, field_index);
        if (capture_id > std.math.maxInt(u32)) Common.invariant("parser generated too many field captures");
        return @intCast(capture_id);
    }

    fn appendParserPrecomputedCaptures(
        self: *BodyContext,
        plan: *ParserPrecomputedPlan,
        fields: []const Type.Field,
        renamed_field_locals: []const Ast.LocalId,
        renamed_field_values: []const Ast.ExprId,
    ) Allocator.Error!void {
        if (fields.len != renamed_field_locals.len or fields.len != renamed_field_values.len) {
            Common.invariant("parser precomputed capture arity differed from record field count");
        }

        var rank: usize = 0;
        while (rank < fields.len) : (rank += 1) {
            for (fields, 0..) |_, index| {
                if (self.parserRecordFieldRank(fields, index) != rank) continue;
                try plan.captures.append(self.allocator, .{
                    .local = renamed_field_locals[index],
                    .value = renamed_field_values[index],
                });
                break;
            } else {
                Common.invariant("parser record field rank was missing from field set");
            }
        }
    }

    fn buildParserConstructionPrecomputedPlan(
        self: *BodyContext,
        plan: *ParserPrecomputedPlan,
        shape_ty: Type.TypeId,
        encoding_expr: Ast.ExprId,
        encoding_ty: Type.TypeId,
        str_ty: Type.TypeId,
    ) Allocator.Error!void {
        if (self.tryOptionalInfo(shape_ty)) |info| {
            return try self.buildParserConstructionPrecomputedPlan(plan, info.ok_payload_ty, encoding_expr, encoding_ty, str_ty);
        }
        if (self.customParserLookup(shape_ty) != null) return;
        if (self.typeHasBuiltinOwner(shape_ty, .str) or self.typeHasBuiltinOwner(shape_ty, .u64)) return;

        switch (self.builder.shapeContent(shape_ty)) {
            .record, .zst => try self.buildParserConstructionRecordPrecomputedPlan(plan, shape_ty, encoding_expr, encoding_ty, str_ty),
            .tag_union => |span| {
                for (self.builder.program.types.tagSpan(span)) |tag| {
                    for (self.builder.program.types.span(tag.payloads)) |payload_ty| {
                        try self.buildParserConstructionPrecomputedPlan(plan, payload_ty, encoding_expr, encoding_ty, str_ty);
                    }
                }
            },
            else => {},
        }
    }

    fn buildEncodeConstructionPrecomputedPlan(
        self: *BodyContext,
        plan: *ParserPrecomputedPlan,
        shape_ty: Type.TypeId,
        encoding_expr: Ast.ExprId,
        encoding_ty: Type.TypeId,
        str_ty: Type.TypeId,
    ) Allocator.Error!void {
        if (self.customEncodeToLookup(shape_ty) != null) return;
        if (self.typeHasBuiltinOwner(shape_ty, .str) or self.typeHasBuiltinOwner(shape_ty, .u64)) return;

        switch (self.builder.shapeContent(shape_ty)) {
            .record, .zst => {
                try self.buildEncodeConstructionRecordPrecomputedPlan(plan, shape_ty, encoding_expr, encoding_ty, str_ty);
                for (self.recordFieldsForShape(shape_ty)) |field| {
                    try self.buildEncodeConstructionPrecomputedPlan(plan, field.ty, encoding_expr, encoding_ty, str_ty);
                }
            },
            else => {},
        }
    }

    fn buildEncodeRestoredPrecomputedPlan(
        self: *BodyContext,
        plan: *ParserPrecomputedPlan,
        fn_value: check.ConstStore.ConstFn,
        store_view: ModuleView,
        fn_view: ModuleView,
        shape_ty: Type.TypeId,
        str_ty: Type.TypeId,
    ) Allocator.Error!void {
        if (self.customEncodeToLookup(shape_ty) != null) return;
        if (self.typeHasBuiltinOwner(shape_ty, .str) or self.typeHasBuiltinOwner(shape_ty, .u64)) return;

        switch (self.builder.shapeContent(shape_ty)) {
            .record, .zst => {
                try self.buildEncodeRestoredRecordPrecomputedPlan(plan, fn_value, store_view, fn_view, shape_ty, str_ty);
                for (self.recordFieldsForShape(shape_ty)) |field| {
                    try self.buildEncodeRestoredPrecomputedPlan(plan, fn_value, store_view, fn_view, field.ty, str_ty);
                }
            },
            else => {},
        }
    }

    fn buildEncodeConstructionRecordPrecomputedPlan(
        self: *BodyContext,
        plan: *ParserPrecomputedPlan,
        shape_ty: Type.TypeId,
        encoding_expr: Ast.ExprId,
        encoding_ty: Type.TypeId,
        str_ty: Type.TypeId,
    ) Allocator.Error!void {
        if (plan.records.contains(shape_ty)) return;

        const fields = self.recordFieldsForShape(shape_ty);
        const locals = try self.allocator.alloc(Ast.LocalId, fields.len);
        const values = try self.allocator.alloc(Ast.ExprId, fields.len);
        var inserted = false;
        errdefer if (!inserted) {
            self.allocator.free(locals);
            self.allocator.free(values);
        };

        const base_capture_id = plan.next_capture_id;
        plan.next_capture_id += fields.len;
        if (plan.next_capture_id > std.math.maxInt(u32)) Common.invariant("encode_to generated too many captures");

        for (fields, 0..) |field, index| {
            locals[index] = try self.builder.program.addLocal(self.builder.symbols.fresh(), str_ty);
            self.builder.program.setLocalCaptureId(
                locals[index],
                self.parserFieldCaptureIdForRecordField(fields, index, base_capture_id),
            );
            values[index] = try self.renamedRecordFieldNameExpr(encoding_expr, encoding_ty, field, str_ty);
        }

        try plan.records.put(shape_ty, .{
            .renamed_field_locals = locals,
            .renamed_field_values = values,
            .renamed_field_lengths = null,
            .renamed_field_texts = null,
        });
        inserted = true;

        try self.appendParserPrecomputedCaptures(plan, fields, locals, values);
    }

    fn buildEncodeRestoredRecordPrecomputedPlan(
        self: *BodyContext,
        plan: *ParserPrecomputedPlan,
        fn_value: check.ConstStore.ConstFn,
        store_view: ModuleView,
        fn_view: ModuleView,
        shape_ty: Type.TypeId,
        str_ty: Type.TypeId,
    ) Allocator.Error!void {
        if (plan.records.contains(shape_ty)) return;

        const fields = self.recordFieldsForShape(shape_ty);
        const locals = try self.allocator.alloc(Ast.LocalId, fields.len);
        const values = try self.allocator.alloc(Ast.ExprId, fields.len);
        var inserted = false;
        errdefer if (!inserted) {
            self.allocator.free(locals);
            self.allocator.free(values);
        };

        const base_capture_id = plan.next_capture_id;
        plan.next_capture_id += fields.len;
        if (plan.next_capture_id > std.math.maxInt(u32)) Common.invariant("encode_to generated too many captures");

        for (fields, 0..) |_, index| {
            const capture_id = self.parserFieldCaptureIdForRecordField(fields, index, base_capture_id);
            const node = constGeneratedCaptureNode(fn_value, capture_id) orelse
                Common.invariant("stored encode_to runtime function was missing a renamed field capture");
            locals[index] = try self.builder.program.addLocal(self.builder.symbols.fresh(), str_ty);
            self.builder.program.setLocalCaptureId(locals[index], capture_id);
            values[index] = try self.builder.restoreConstNodeAtType(store_view, fn_view, node, str_ty);
        }

        try plan.records.put(shape_ty, .{
            .renamed_field_locals = locals,
            .renamed_field_values = values,
            .renamed_field_lengths = null,
            .renamed_field_texts = null,
        });
        inserted = true;

        try self.appendParserPrecomputedCaptures(plan, fields, locals, values);
    }

    fn buildParserConstructionRecordPrecomputedPlan(
        self: *BodyContext,
        plan: *ParserPrecomputedPlan,
        shape_ty: Type.TypeId,
        encoding_expr: Ast.ExprId,
        encoding_ty: Type.TypeId,
        str_ty: Type.TypeId,
    ) Allocator.Error!void {
        if (plan.records.contains(shape_ty)) return;

        const fields = self.recordFieldsForShape(shape_ty);
        const locals = try self.allocator.alloc(Ast.LocalId, fields.len);
        const values = try self.allocator.alloc(Ast.ExprId, fields.len);
        var inserted = false;
        errdefer if (!inserted) {
            self.allocator.free(locals);
            self.allocator.free(values);
        };

        const base_capture_id = plan.next_capture_id;
        plan.next_capture_id += fields.len;
        if (plan.next_capture_id > std.math.maxInt(u32)) Common.invariant("parser generated too many captures");

        for (fields, 0..) |field, index| {
            locals[index] = try self.builder.program.addLocal(self.builder.symbols.fresh(), str_ty);
            self.builder.program.setLocalCaptureId(
                locals[index],
                self.parserFieldCaptureIdForRecordField(fields, index, base_capture_id),
            );
            values[index] = try self.renamedRecordFieldNameExpr(encoding_expr, encoding_ty, field, str_ty);
        }

        try plan.records.put(shape_ty, .{
            .renamed_field_locals = locals,
            .renamed_field_values = values,
            .renamed_field_lengths = null,
            .renamed_field_texts = null,
        });
        inserted = true;

        try self.appendParserPrecomputedCaptures(plan, fields, locals, values);

        for (fields) |field| {
            try self.buildParserConstructionPrecomputedPlan(plan, field.ty, encoding_expr, encoding_ty, str_ty);
        }
    }

    fn buildParserRestoredPrecomputedPlan(
        self: *BodyContext,
        plan: *ParserPrecomputedPlan,
        fn_value: check.ConstStore.ConstFn,
        store_view: ModuleView,
        fn_view: ModuleView,
        shape_ty: Type.TypeId,
        str_ty: Type.TypeId,
    ) Allocator.Error!void {
        if (self.tryOptionalInfo(shape_ty)) |info| {
            return try self.buildParserRestoredPrecomputedPlan(plan, fn_value, store_view, fn_view, info.ok_payload_ty, str_ty);
        }
        if (self.customParserLookup(shape_ty) != null) return;
        if (self.typeHasBuiltinOwner(shape_ty, .str) or self.typeHasBuiltinOwner(shape_ty, .u64)) return;

        switch (self.builder.shapeContent(shape_ty)) {
            .record, .zst => try self.buildParserRestoredRecordPrecomputedPlan(plan, fn_value, store_view, fn_view, shape_ty, str_ty),
            .tag_union => |span| {
                for (self.builder.program.types.tagSpan(span)) |tag| {
                    for (self.builder.program.types.span(tag.payloads)) |payload_ty| {
                        try self.buildParserRestoredPrecomputedPlan(plan, fn_value, store_view, fn_view, payload_ty, str_ty);
                    }
                }
            },
            else => {},
        }
    }

    fn parserPrecomputedRecordShapesForTagUnion(
        self: *BodyContext,
        plan: ?*const ParserPrecomputedPlan,
        union_ty: Type.TypeId,
    ) Allocator.Error![]Type.TypeId {
        var shapes = std.ArrayList(Type.TypeId).empty;
        errdefer shapes.deinit(self.allocator);
        var seen = std.AutoHashMap(Type.TypeId, void).init(self.allocator);
        defer seen.deinit();

        try self.appendParserPrecomputedRecordShapes(plan, &shapes, &seen, union_ty);
        return try shapes.toOwnedSlice(self.allocator);
    }

    fn appendParserPrecomputedRecordShapes(
        self: *BodyContext,
        plan: ?*const ParserPrecomputedPlan,
        shapes: *std.ArrayList(Type.TypeId),
        seen: *std.AutoHashMap(Type.TypeId, void),
        shape_ty: Type.TypeId,
    ) Allocator.Error!void {
        if (self.tryOptionalInfo(shape_ty)) |info| {
            return try self.appendParserPrecomputedRecordShapes(plan, shapes, seen, info.ok_payload_ty);
        }
        if (self.customParserLookup(shape_ty) != null) return;
        if (self.typeHasBuiltinOwner(shape_ty, .str) or self.typeHasBuiltinOwner(shape_ty, .u64)) return;

        if (seen.contains(shape_ty)) return;
        try seen.put(shape_ty, {});

        switch (self.builder.shapeContent(shape_ty)) {
            .record, .zst => {
                if (plan == null or plan.?.records.contains(shape_ty)) {
                    try shapes.append(self.allocator, shape_ty);
                }
                for (self.recordFieldsForShape(shape_ty)) |field| {
                    try self.appendParserPrecomputedRecordShapes(plan, shapes, seen, field.ty);
                }
            },
            .tag_union => |span| {
                for (self.builder.program.types.tagSpan(span)) |tag| {
                    for (self.builder.program.types.span(tag.payloads)) |payload_ty| {
                        try self.appendParserPrecomputedRecordShapes(plan, shapes, seen, payload_ty);
                    }
                }
            },
            else => {},
        }
    }

    fn generatedParseTagUnionSpecBackingRecordFieldName(
        self: *BodyContext,
        index: usize,
    ) Allocator.Error!names.RecordFieldNameId {
        const label = try std.fmt.allocPrint(self.allocator, "record_{d}", .{index});
        defer self.allocator.free(label);
        return try self.builder.program.names.internRecordFieldLabel(label);
    }

    fn generatedParseTagUnionSpecBackingFieldName(
        self: *BodyContext,
        index: usize,
    ) Allocator.Error!names.RecordFieldNameId {
        const label = try std.fmt.allocPrint(self.allocator, "field_{d}", .{index});
        defer self.allocator.free(label);
        return try self.builder.program.names.internRecordFieldLabel(label);
    }

    fn generatedParseTagUnionSpecBackingType(
        self: *BodyContext,
        record_shapes: []const Type.TypeId,
    ) Allocator.Error!Type.TypeId {
        const str_ty = try self.builder.primitiveType(.str);
        const outer_fields = try self.allocator.alloc(Type.Field, record_shapes.len);
        defer self.allocator.free(outer_fields);

        for (record_shapes, 0..) |record_shape, record_index| {
            const record_fields = self.recordFieldsForShape(record_shape);
            const inner_fields = try self.allocator.alloc(Type.Field, record_fields.len);
            defer self.allocator.free(inner_fields);

            for (inner_fields, 0..) |*field, field_index| {
                field.* = .{
                    .name = try self.generatedParseTagUnionSpecBackingFieldName(field_index),
                    .ty = str_ty,
                };
            }

            outer_fields[record_index] = .{
                .name = try self.generatedParseTagUnionSpecBackingRecordFieldName(record_index),
                .ty = try self.builder.program.types.add(.{
                    .record = try self.builder.program.types.addFields(inner_fields),
                }),
            };
        }

        return try self.builder.program.types.add(.{
            .record = try self.builder.program.types.addFields(outer_fields),
        });
    }

    fn lowerParseTagUnionSpecValue(
        self: *BodyContext,
        spec_ty: Type.TypeId,
        spec_backing_ty: Type.TypeId,
        record_shapes: []const Type.TypeId,
        plan: *const ParserPrecomputedPlan,
    ) Allocator.Error!Ast.ExprId {
        const str_ty = try self.builder.primitiveType(.str);
        const backing_fields = self.builder.program.types.fieldSpan(self.builder.recordFieldsSpan(spec_backing_ty));
        if (backing_fields.len != record_shapes.len) Common.invariant("generated tag-union spec backing arity differed from record shape count");

        const outer_values = try self.allocator.alloc(Ast.FieldExpr, record_shapes.len);
        defer self.allocator.free(outer_values);

        for (record_shapes, backing_fields, 0..) |record_shape, backing_field, record_index| {
            const precomputed = plan.records.get(record_shape) orelse
                Common.invariant("generated tag-union spec requested missing parser precomputed record");
            const inner_fields = self.builder.program.types.fieldSpan(self.builder.recordFieldsSpan(backing_field.ty));
            if (inner_fields.len != precomputed.renamed_field_locals.len) {
                Common.invariant("generated tag-union spec record arity differed from precomputed record");
            }

            const inner_values = try self.allocator.alloc(Ast.FieldExpr, inner_fields.len);
            defer self.allocator.free(inner_values);
            for (inner_fields, precomputed.renamed_field_locals, 0..) |inner_field, renamed_local, field_index| {
                inner_values[field_index] = .{
                    .name = inner_field.name,
                    .value = try self.builder.localExpr(renamed_local, str_ty),
                };
            }

            outer_values[record_index] = .{
                .name = backing_field.name,
                .value = try self.builder.program.addExpr(.{
                    .ty = backing_field.ty,
                    .data = .{ .record = try self.builder.program.addFieldExprSpan(inner_values) },
                }),
            };
        }

        const backing_expr = try self.builder.program.addExpr(.{
            .ty = spec_backing_ty,
            .data = .{ .record = try self.builder.program.addFieldExprSpan(outer_values) },
        });
        return try self.builder.program.addExpr(.{
            .ty = spec_ty,
            .data = .{ .nominal = backing_expr },
        });
    }

    fn buildParserPrecomputedPlanFromTagUnionSpec(
        self: *BodyContext,
        plan: *ParserPrecomputedPlan,
        spec_backing_local: Ast.LocalId,
        spec_backing_ty: Type.TypeId,
        union_ty: Type.TypeId,
    ) Allocator.Error!void {
        const record_shapes = try self.parserPrecomputedRecordShapesForTagUnion(null, union_ty);
        defer self.allocator.free(record_shapes);

        const backing_fields = self.builder.program.types.fieldSpan(self.builder.recordFieldsSpan(spec_backing_ty));
        if (backing_fields.len != record_shapes.len) {
            Common.invariant("generated tag-union spec backing arity differed from payload record shape count");
        }

        const str_ty = try self.builder.primitiveType(.str);
        for (record_shapes, backing_fields) |record_shape, backing_field| {
            if (plan.records.contains(record_shape)) continue;

            const record_fields = self.recordFieldsForShape(record_shape);
            const backing_record_fields = self.builder.program.types.fieldSpan(self.builder.recordFieldsSpan(backing_field.ty));
            if (backing_record_fields.len != record_fields.len) {
                Common.invariant("generated tag-union spec record arity differed from payload record arity");
            }

            const locals = try self.allocator.alloc(Ast.LocalId, record_fields.len);
            const values = try self.allocator.alloc(Ast.ExprId, record_fields.len);
            var inserted = false;
            errdefer if (!inserted) {
                self.allocator.free(locals);
                self.allocator.free(values);
            };

            const record_expr = try self.builder.program.addExpr(.{
                .ty = backing_field.ty,
                .data = .{ .field_access = .{
                    .receiver = try self.builder.localExpr(spec_backing_local, spec_backing_ty),
                    .field = backing_field.name,
                } },
            });

            for (backing_record_fields, 0..) |field, index| {
                if (!self.sameType(field.ty, str_ty)) {
                    Common.invariant("generated tag-union spec field name value was not Str");
                }
                locals[index] = try self.builder.program.addLocal(self.builder.symbols.fresh(), str_ty);
                values[index] = try self.builder.program.addExpr(.{
                    .ty = str_ty,
                    .data = .{ .field_access = .{
                        .receiver = record_expr,
                        .field = field.name,
                    } },
                });
            }

            try plan.records.put(record_shape, .{
                .renamed_field_locals = locals,
                .renamed_field_values = values,
                .renamed_field_lengths = null,
                .renamed_field_texts = null,
            });
            inserted = true;

            try self.appendParserPrecomputedCaptures(plan, record_fields, locals, values);
        }
    }

    fn buildParserRestoredRecordPrecomputedPlan(
        self: *BodyContext,
        plan: *ParserPrecomputedPlan,
        fn_value: check.ConstStore.ConstFn,
        store_view: ModuleView,
        fn_view: ModuleView,
        shape_ty: Type.TypeId,
        str_ty: Type.TypeId,
    ) Allocator.Error!void {
        if (plan.records.contains(shape_ty)) return;

        const fields = self.recordFieldsForShape(shape_ty);
        const locals = try self.allocator.alloc(Ast.LocalId, fields.len);
        const values = try self.allocator.alloc(Ast.ExprId, fields.len);
        const lengths = try self.allocator.alloc(u32, fields.len);
        const texts = try self.allocator.alloc([]const u8, fields.len);
        var inserted = false;
        errdefer if (!inserted) {
            self.allocator.free(locals);
            self.allocator.free(values);
            self.allocator.free(lengths);
            self.allocator.free(texts);
        };

        const base_capture_id = plan.next_capture_id;
        plan.next_capture_id += fields.len;
        if (plan.next_capture_id > std.math.maxInt(u32)) Common.invariant("parser generated too many captures");

        for (fields, 0..) |_, index| {
            const capture_id = self.parserFieldCaptureIdForRecordField(fields, index, base_capture_id);
            const node = constGeneratedCaptureNode(fn_value, capture_id) orelse
                Common.invariant("stored parser runtime function was missing a renamed field capture");
            locals[index] = try self.builder.program.addLocal(self.builder.symbols.fresh(), str_ty);
            self.builder.program.setLocalCaptureId(locals[index], capture_id);
            values[index] = try self.builder.restoreConstNodeAtType(store_view, fn_view, node, str_ty);
            lengths[index] = constStrNodeByteLen(store_view, node);
            texts[index] = constStrNodeBytes(store_view, node);
        }

        try plan.records.put(shape_ty, .{
            .renamed_field_locals = locals,
            .renamed_field_values = values,
            .renamed_field_lengths = lengths,
            .renamed_field_texts = texts,
        });
        inserted = true;

        try self.appendParserPrecomputedCaptures(plan, fields, locals, values);

        for (fields) |field| {
            try self.buildParserRestoredPrecomputedPlan(plan, fn_value, store_view, fn_view, field.ty, str_ty);
        }
    }

    fn lowerParseShapeFromState(
        self: *BodyContext,
        shape_ty: Type.TypeId,
        encoding_expr: Ast.ExprId,
        encoding_ty: Type.TypeId,
        state_expr: Ast.ExprId,
        state_ty: Type.TypeId,
        ret_ty: Type.TypeId,
        precomputed_plan: ?*const ParserPrecomputedPlan,
    ) Allocator.Error!Ast.ExprId {
        const selected = self.parseShapeSelection(shape_ty);
        if (Ident.textEql(selected.tag_text, "Record")) {
            const precomputed = if (precomputed_plan) |plan| plan.records.get(shape_ty) else null;
            return try self.lowerParseRecordFromState(
                shape_ty,
                encoding_expr,
                encoding_ty,
                state_expr,
                state_ty,
                ret_ty,
                precomputed_plan,
                if (precomputed) |record| record.renamed_field_locals else null,
                if (precomputed) |record| record.renamed_field_lengths else null,
                if (precomputed) |record| record.renamed_field_texts else null,
            );
        }

        const method_name: []const u8 =
            if (Ident.textEql(selected.tag_text, "Str"))
                "parse_str"
            else if (Ident.textEql(selected.tag_text, "U64"))
                "parse_u64"
            else if (Ident.textEql(selected.tag_text, "TagUnion"))
                "parse_tag_union"
            else
                Common.invariant("parser selected an unsupported shape");

        const parse_lookup = self.methodLookupForTypeName(encoding_ty, method_name);
        const state_arg_index: usize = if (Ident.textEql(selected.tag_text, "TagUnion")) 2 else 1;
        const parse_mono_ty = try self.methodTargetMonoTypeFromArgAtIndexAndRet(parse_lookup, state_arg_index, state_ty, ret_ty);
        const parse_fn = self.builder.functionShape(parse_mono_ty, "parser target method was not a function");
        const parse_arg_tys = self.builder.program.types.span(parse_fn.args);
        if (!Ident.textEql(selected.tag_text, "TagUnion")) {
            if (parse_arg_tys.len != 2) Common.invariant("parser target scalar method had an unexpected arity");
            if (!self.sameType(parse_arg_tys[0], encoding_ty)) Common.invariant("parser target encoding type differed from input encoding type");
            if (!self.sameType(parse_arg_tys[1], state_ty)) Common.invariant("parser target state type differed from input state type");

            const parse_args = [_]Ast.ExprId{ encoding_expr, state_expr };
            return try self.builder.program.addExpr(.{
                .ty = ret_ty,
                .data = .{ .call_proc = .{
                    .callee = .{ .func = try self.methodTargetCalleeWithMono(parse_lookup, parse_mono_ty) },
                    .args = try self.builder.program.addExprSpan(&parse_args),
                } },
            });
        }
        if (parse_arg_tys.len != 3) Common.invariant("parser target tag-union method had an unexpected arity");
        if (!self.sameType(parse_arg_tys[0], encoding_ty)) Common.invariant("parser target tag-union encoding type differed from input encoding type");
        if (!self.sameType(parse_arg_tys[2], state_ty)) Common.invariant("parser target tag-union state type differed from input state type");

        const record_shapes = if (precomputed_plan) |plan|
            try self.parserPrecomputedRecordShapesForTagUnion(plan, shape_ty)
        else
            &.{};
        defer if (precomputed_plan != null) self.allocator.free(record_shapes);

        const has_generated_spec = precomputed_plan != null and record_shapes.len != 0;
        const spec_ty = if (has_generated_spec) blk: {
            const spec_backing_ty = try self.generatedParseTagUnionSpecBackingType(record_shapes);
            break :blk try self.cloneNamedTypeWithArgs(parse_arg_tys[1], &.{shape_ty}, spec_backing_ty);
        } else parse_arg_tys[1];
        const callable_mono_ty = if (has_generated_spec)
            try self.methodTargetMonoTypeFromArgsPreservingArgs(parse_lookup, &.{ encoding_ty, spec_ty, state_ty }, ret_ty)
        else
            parse_mono_ty;
        const callable_fn = self.builder.functionShape(callable_mono_ty, "parser target tag-union method was not a function");
        const callable_arg_tys = self.builder.program.types.span(callable_fn.args);
        if (callable_arg_tys.len != 3) Common.invariant("parser target tag-union method had an unexpected arity");
        if (!self.sameType(callable_arg_tys[0], encoding_ty)) Common.invariant("parser target tag-union encoding type differed from generated input encoding type");
        if (!self.sameType(callable_arg_tys[1], spec_ty)) Common.invariant("parser target tag-union spec type differed from generated spec type");
        if (!self.sameType(callable_arg_tys[2], state_ty)) Common.invariant("parser target tag-union state type differed from generated input state type");

        const final_spec_expr = if (has_generated_spec) blk: {
            const plan = precomputed_plan orelse Common.invariant("generated tag-union spec requested without a precomputed plan");
            const spec_backing_ty = self.builder.namedBackingType(spec_ty) orelse
                Common.invariant("generated tag-union spec had no backing type");
            break :blk try self.lowerParseTagUnionSpecValue(spec_ty, spec_backing_ty, record_shapes, plan);
        } else try self.builder.program.addExpr(.{
            .ty = spec_ty,
            .data = .unit,
        });
        return try self.builder.program.addExpr(.{
            .ty = ret_ty,
            .data = .{ .call_proc = .{
                .callee = .{ .func = try self.methodTargetCalleeWithMono(parse_lookup, callable_mono_ty) },
                .args = try self.builder.program.addExprSpan(&[_]Ast.ExprId{ encoding_expr, final_spec_expr, state_expr }),
            } },
        });
    }

    fn lowerParseRecordFromState(
        self: *BodyContext,
        shape_ty: Type.TypeId,
        encoding_expr: Ast.ExprId,
        encoding_ty: Type.TypeId,
        state_expr: Ast.ExprId,
        state_ty: Type.TypeId,
        ret_ty: Type.TypeId,
        precomputed_plan: ?*const ParserPrecomputedPlan,
        precomputed_renamed_field_locals: ?[]const Ast.LocalId,
        precomputed_renamed_field_lengths: ?[]const u32,
        precomputed_renamed_field_texts: ?[][]const u8,
    ) Allocator.Error!Ast.ExprId {
        const ret_info = self.tryInfo(ret_ty);
        const parse_ok_ty = try self.parseResultOkType(shape_ty, state_ty);
        if (!self.sameType(ret_info.ok_ty, parse_ok_ty)) Common.invariant("record parser return Ok type differed from generated parse result");

        const record_fields = try self.allocator.dupe(Type.Field, switch (self.builder.shapeContent(shape_ty)) {
            .record => |span| self.builder.program.types.fieldSpan(span),
            .zst => &.{},
            else => Common.invariant("record parser requested for a non-record shape"),
        });
        defer self.allocator.free(record_fields);

        const u64_ty = try self.builder.primitiveType(.u64);
        const presence_word_count = recordPresenceWordCount(record_fields.len);
        const presence_locals = try self.allocator.alloc(Ast.LocalId, presence_word_count);
        defer self.allocator.free(presence_locals);
        const presence_tys = try self.allocator.alloc(Type.TypeId, presence_word_count);
        defer self.allocator.free(presence_tys);
        const initial_presence_values = try self.allocator.alloc(Ast.ExprId, presence_word_count);
        defer self.allocator.free(initial_presence_values);
        for (presence_locals, 0..) |*local, index| {
            local.* = try self.builder.program.addLocal(self.builder.symbols.fresh(), u64_ty);
            presence_tys[index] = u64_ty;
            initial_presence_values[index] = try self.builder.intLiteralExpr(0, u64_ty);
        }

        const payload_locals = try self.allocator.alloc(Ast.LocalId, record_fields.len);
        defer self.allocator.free(payload_locals);
        const payload_tys = try self.allocator.alloc(Type.TypeId, record_fields.len);
        defer self.allocator.free(payload_tys);
        const initial_payload_values = try self.allocator.alloc(Ast.ExprId, record_fields.len);
        defer self.allocator.free(initial_payload_values);
        for (record_fields, 0..) |field, index| {
            payload_tys[index] = field.ty;
            payload_locals[index] = try self.builder.program.addLocal(self.builder.symbols.fresh(), field.ty);
            initial_payload_values[index] = try self.uninitializedPayloadExpr(
                field.ty,
                presence_locals[recordPresenceWordIndex(index)],
                recordPresenceMask(index),
            );
        }
        const record_slots = ParseRecordSlots{
            .presence_locals = presence_locals,
            .presence_tys = presence_tys,
            .payload_locals = payload_locals,
            .payload_tys = payload_tys,
        };

        const parse_lookup = self.methodLookupForTypeName(encoding_ty, "parse_record_field");
        const generic_callable_ty = try self.methodTargetMonoTypeFromArgAtIndexIsolated(parse_lookup, 2, state_ty);
        const generic_parse_fn = self.builder.functionShape(generic_callable_ty, "parse_record_field target method was not a function");
        const generic_arg_tys = try self.allocator.dupe(Type.TypeId, self.builder.program.types.span(generic_parse_fn.args));
        defer self.allocator.free(generic_arg_tys);
        if (generic_arg_tys.len != 3) Common.invariant("parse_record_field target method had an unexpected arity");
        if (!self.sameType(generic_arg_tys[0], encoding_ty)) Common.invariant("parse_record_field encoding type differed from input encoding type");
        if (!self.sameType(generic_arg_tys[2], state_ty)) Common.invariant("parse_record_field state type differed from input state type");

        const generic_event_ty = self.tryInfo(generic_parse_fn.ret).ok_ty;
        const field_handle_template_ty = try self.recordEventFieldHandleTemplate(generic_event_ty);
        const field_handle_ty = try self.cloneNamedTypeWithArgs(
            field_handle_template_ty,
            &.{shape_ty},
            try self.generatedFieldHandleBackingType(),
        );
        const fields_backing_ty = try self.generatedFieldNamesBackingType(record_fields.len, field_handle_ty);
        const fields_ty = try self.cloneNamedTypeWithArgs(generic_arg_tys[1], &.{shape_ty}, fields_backing_ty);
        const event_ty = try self.parseRecordFieldEventType(state_ty, field_handle_ty);
        const step_try_ty = try self.tryTypeLike(ret_ty, event_ty, ret_info.err_ty);

        const callable_mono_ty = try self.methodTargetMonoTypeFromArgsPreservingArgs(parse_lookup, &.{ encoding_ty, fields_ty, state_ty }, step_try_ty);
        const parse_fn = self.builder.functionShape(callable_mono_ty, "parse_record_field target method was not a function");
        const parse_arg_tys = self.builder.program.types.span(parse_fn.args);
        if (parse_arg_tys.len != 3) Common.invariant("parse_record_field target method had an unexpected arity");
        if (!self.sameType(parse_arg_tys[0], encoding_ty)) Common.invariant("parse_record_field encoding type differed from input encoding type");
        if (!self.sameType(parse_arg_tys[1], fields_ty)) Common.invariant("parse_record_field fields type differed from generated field set type");
        if (!self.sameType(parse_arg_tys[2], state_ty)) Common.invariant("parse_record_field state type differed from input state type");

        const str_ty = try self.builder.primitiveType(.str);
        var owned_renamed_field_locals: ?[]Ast.LocalId = null;
        defer if (owned_renamed_field_locals) |locals| self.allocator.free(locals);
        var owned_renamed_field_values: ?[]Ast.ExprId = null;
        defer if (owned_renamed_field_values) |values| self.allocator.free(values);

        const renamed_field_locals = if (precomputed_renamed_field_locals) |locals| blk: {
            if (locals.len != record_fields.len) Common.invariant("precomputed renamed field arity differed from record field count");
            break :blk locals;
        } else blk: {
            const locals = try self.allocator.alloc(Ast.LocalId, record_fields.len);
            const values = try self.allocator.alloc(Ast.ExprId, record_fields.len);
            owned_renamed_field_locals = locals;
            owned_renamed_field_values = values;

            for (record_fields, 0..) |field, index| {
                locals[index] = try self.builder.program.addLocal(self.builder.symbols.fresh(), str_ty);
                values[index] = try self.renamedRecordFieldNameExpr(encoding_expr, encoding_ty, field, str_ty);
            }

            break :blk locals;
        };
        const renamed_field_lengths = if (precomputed_renamed_field_lengths) |lengths| blk: {
            if (lengths.len != record_fields.len) Common.invariant("precomputed renamed field length arity differed from record field count");
            break :blk lengths;
        } else null;
        const renamed_field_texts = if (precomputed_renamed_field_texts) |texts| blk: {
            if (texts.len != record_fields.len) Common.invariant("precomputed renamed field text arity differed from record field count");
            break :blk texts;
        } else null;

        const cursor_local = try self.builder.program.addLocal(self.builder.symbols.fresh(), state_ty);
        const fields_expr = try self.lowerFieldNamesValue(fields_ty, fields_backing_ty, field_handle_ty, renamed_field_locals, renamed_field_lengths);
        const fields_local = try self.builder.program.addLocal(self.builder.symbols.fresh(), fields_ty);
        const step_expr = try self.builder.program.addExpr(.{
            .ty = step_try_ty,
            .data = .{ .call_proc = .{
                .callee = .{ .func = try self.methodTargetCalleeWithMono(parse_lookup, callable_mono_ty) },
                .args = try self.builder.program.addExprSpan(&[_]Ast.ExprId{
                    encoding_expr,
                    try self.builder.localExpr(fields_local, fields_ty),
                    try self.builder.localExpr(cursor_local, state_ty),
                }),
            } },
        });

        const event_local = try self.builder.program.addLocal(self.builder.symbols.fresh(), event_ty);
        const event_body = try self.lowerParseRecordEvent(
            shape_ty,
            encoding_expr,
            encoding_ty,
            state_ty,
            ret_ty,
            record_slots,
            precomputed_plan,
            renamed_field_locals,
            renamed_field_lengths,
            renamed_field_texts,
            event_local,
            event_ty,
        );
        const loop_body = try self.sequenceTry(step_expr, step_try_ty, event_local, event_body, ret_ty);

        const params = try self.allocator.alloc(Ast.TypedLocal, 1 + record_slots.payload_locals.len + record_slots.presence_locals.len);
        defer self.allocator.free(params);
        params[0] = .{ .local = cursor_local, .ty = state_ty };
        // Payloads must be carried before presence words. LIR lowers continue
        // arguments into ordered `set_local` statements; ARC inserts cleanup of
        // the old payload immediately before the payload write, and that cleanup
        // must test the old presence bit, not the just-updated one.
        const payload_param_offset = 1;
        for (record_slots.payload_locals, record_slots.payload_tys, 0..) |local, ty, index| {
            params[payload_param_offset + index] = .{ .local = local, .ty = ty };
        }
        const presence_param_offset = payload_param_offset + record_slots.payload_locals.len;
        for (record_slots.presence_locals, record_slots.presence_tys, 0..) |local, ty, index| {
            params[presence_param_offset + index] = .{ .local = local, .ty = ty };
        }

        const initial_values = try self.allocator.alloc(Ast.ExprId, 1 + initial_payload_values.len + initial_presence_values.len);
        defer self.allocator.free(initial_values);
        initial_values[0] = state_expr;
        @memcpy(initial_values[1 .. 1 + initial_payload_values.len], initial_payload_values);
        @memcpy(initial_values[1 + initial_payload_values.len ..], initial_presence_values);
        var loop_expr = try self.builder.program.addExpr(.{ .ty = ret_ty, .data = .{ .loop_ = .{
            .params = try self.builder.program.addTypedLocalSpan(params),
            .initial_values = try self.builder.program.addExprSpan(initial_values),
            .body = loop_body,
        } } });
        loop_expr = try self.wrapLet(fields_local, fields_ty, fields_expr, loop_expr, ret_ty);
        if (owned_renamed_field_values) |renamed_field_values| {
            var index = renamed_field_locals.len;
            while (index > 0) {
                index -= 1;
                loop_expr = try self.wrapLet(renamed_field_locals[index], str_ty, renamed_field_values[index], loop_expr, ret_ty);
            }
        }
        return loop_expr;
    }

    fn lowerParseRecordEvent(
        self: *BodyContext,
        shape_ty: Type.TypeId,
        encoding_expr: Ast.ExprId,
        encoding_ty: Type.TypeId,
        state_ty: Type.TypeId,
        ret_ty: Type.TypeId,
        record_slots: ParseRecordSlots,
        precomputed_plan: ?*const ParserPrecomputedPlan,
        renamed_field_locals: []const Ast.LocalId,
        renamed_field_lengths: ?[]const u32,
        renamed_field_texts: ?[]const []const u8,
        event_local: Ast.LocalId,
        event_ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        const continue_tag = self.monoTagByText(event_ty, "Continue");
        const done_tag = self.monoTagByText(event_ty, "Done");
        const field_tag = self.monoTagByText(event_ty, "Field");
        const try_field_tag = self.monoTagByText(event_ty, "TryField");
        const try_field_caseless_tag = self.monoTagByText(event_ty, "TryFieldCaseless");
        const continue_payload_ty = self.singleTagPayloadType(continue_tag, "record parse Continue event");
        const done_payload_ty = self.singleTagPayloadType(done_tag, "record parse Done event");
        const field_payload_ty = self.singleTagPayloadType(field_tag, "record parse Field event");
        const try_field_payload_ty = self.singleTagPayloadType(try_field_tag, "record parse TryField event");
        const try_field_caseless_payload_ty = self.singleTagPayloadType(try_field_caseless_tag, "record parse TryFieldCaseless event");

        const continue_payload_local = try self.builder.program.addLocal(self.builder.symbols.fresh(), continue_payload_ty);
        const continue_payload_pat = try self.builder.bindPat(continue_payload_local, continue_payload_ty);
        const continue_pat = try self.builder.program.addPat(.{ .ty = event_ty, .data = .{ .tag = .{
            .name = continue_tag.name,
            .payloads = try self.builder.program.addPatSpan(&[_]Ast.PatId{continue_payload_pat}),
        } } });
        const continue_body = try self.lowerParseRecordContinueEvent(
            ret_ty,
            record_slots,
            continue_payload_local,
            continue_payload_ty,
        );

        const done_payload_local = try self.builder.program.addLocal(self.builder.symbols.fresh(), done_payload_ty);
        const done_payload_pat = try self.builder.bindPat(done_payload_local, done_payload_ty);
        const done_pat = try self.builder.program.addPat(.{ .ty = event_ty, .data = .{ .tag = .{
            .name = done_tag.name,
            .payloads = try self.builder.program.addPatSpan(&[_]Ast.PatId{done_payload_pat}),
        } } });
        const done_body = try self.lowerParseRecordDoneEvent(
            shape_ty,
            encoding_expr,
            encoding_ty,
            state_ty,
            ret_ty,
            record_slots,
            renamed_field_locals,
            done_payload_local,
            done_payload_ty,
        );

        const field_payload_local = try self.builder.program.addLocal(self.builder.symbols.fresh(), field_payload_ty);
        const field_payload_pat = try self.builder.bindPat(field_payload_local, field_payload_ty);
        const field_pat = try self.builder.program.addPat(.{ .ty = event_ty, .data = .{ .tag = .{
            .name = field_tag.name,
            .payloads = try self.builder.program.addPatSpan(&[_]Ast.PatId{field_payload_pat}),
        } } });
        const field_body = try self.lowerParseRecordDirectFieldEvent(
            shape_ty,
            encoding_expr,
            encoding_ty,
            state_ty,
            ret_ty,
            record_slots,
            precomputed_plan,
            field_payload_local,
            field_payload_ty,
        );

        const try_field_payload_local = try self.builder.program.addLocal(self.builder.symbols.fresh(), try_field_payload_ty);
        const try_field_payload_pat = try self.builder.bindPat(try_field_payload_local, try_field_payload_ty);
        const try_field_pat = try self.builder.program.addPat(.{ .ty = event_ty, .data = .{ .tag = .{
            .name = try_field_tag.name,
            .payloads = try self.builder.program.addPatSpan(&[_]Ast.PatId{try_field_payload_pat}),
        } } });
        const try_field_body = try self.lowerParseRecordNamedFieldEvent(
            shape_ty,
            encoding_expr,
            encoding_ty,
            state_ty,
            ret_ty,
            record_slots,
            precomputed_plan,
            renamed_field_locals,
            renamed_field_lengths,
            renamed_field_texts,
            try_field_payload_local,
            try_field_payload_ty,
            .exact,
        );

        const try_field_caseless_payload_local = try self.builder.program.addLocal(self.builder.symbols.fresh(), try_field_caseless_payload_ty);
        const try_field_caseless_payload_pat = try self.builder.bindPat(try_field_caseless_payload_local, try_field_caseless_payload_ty);
        const try_field_caseless_pat = try self.builder.program.addPat(.{ .ty = event_ty, .data = .{ .tag = .{
            .name = try_field_caseless_tag.name,
            .payloads = try self.builder.program.addPatSpan(&[_]Ast.PatId{try_field_caseless_payload_pat}),
        } } });
        const try_field_caseless_body = try self.lowerParseRecordNamedFieldEvent(
            shape_ty,
            encoding_expr,
            encoding_ty,
            state_ty,
            ret_ty,
            record_slots,
            precomputed_plan,
            renamed_field_locals,
            renamed_field_lengths,
            renamed_field_texts,
            try_field_caseless_payload_local,
            try_field_caseless_payload_ty,
            .caseless,
        );

        const branches = [_]Ast.Branch{
            .{ .pat = continue_pat, .body = continue_body },
            .{ .pat = done_pat, .body = done_body },
            .{ .pat = field_pat, .body = field_body },
            .{ .pat = try_field_pat, .body = try_field_body },
            .{ .pat = try_field_caseless_pat, .body = try_field_caseless_body },
        };
        return try self.builder.program.addExpr(.{ .ty = ret_ty, .data = .{ .match_ = .{
            .scrutinee = try self.builder.localExpr(event_local, event_ty),
            .branches = try self.builder.program.addBranchSpan(&branches),
        } } });
    }

    fn lowerParseRecordNamedDispatchBody(
        self: *BodyContext,
        key_local: Ast.LocalId,
        key_ty: Type.TypeId,
        renamed_field_locals: []const Ast.LocalId,
        renamed_field_lengths: ?[]const u32,
        renamed_field_texts: ?[]const []const u8,
        matched_bodies: []const Ast.ExprId,
        unknown_body: Ast.ExprId,
        mode: RecordFieldMatchMode,
        ret_ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        if (renamed_field_locals.len != matched_bodies.len) Common.invariant("record parser dispatch arity did not match renamed fields");
        if (renamed_field_lengths) |lengths| {
            if (lengths.len != renamed_field_locals.len) Common.invariant("record parser dispatch length arity did not match renamed fields");
            return try self.lowerParseRecordLengthBucketedDispatchBody(
                key_local,
                key_ty,
                renamed_field_locals,
                lengths,
                renamed_field_texts,
                matched_bodies,
                unknown_body,
                mode,
                ret_ty,
            );
        }
        var body = unknown_body;
        var index = matched_bodies.len;
        while (index > 0) {
            index -= 1;
            const renamed_field_expr = try self.builder.localExpr(renamed_field_locals[index], key_ty);
            const cond = try self.recordFieldNameMatch(
                key_local,
                key_ty,
                renamed_field_expr,
                if (renamed_field_texts) |texts| texts[index] else null,
                mode,
            );
            body = try self.builder.ifExpr(cond, matched_bodies[index], body, ret_ty);
        }
        return body;
    }

    fn lowerParseRecordLengthBucketedDispatchBody(
        self: *BodyContext,
        key_local: Ast.LocalId,
        key_ty: Type.TypeId,
        renamed_field_locals: []const Ast.LocalId,
        renamed_field_lengths: []const u32,
        renamed_field_texts: ?[]const []const u8,
        matched_bodies: []const Ast.ExprId,
        unknown_body: Ast.ExprId,
        mode: RecordFieldMatchMode,
        ret_ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        const u64_ty = try self.builder.primitiveType(.u64);
        const bool_ty = try self.builder.primitiveType(.bool);
        const key_len_local = try self.builder.program.addLocal(self.builder.symbols.fresh(), u64_ty);

        var body = unknown_body;
        var index = matched_bodies.len;
        while (index > 0) {
            index -= 1;
            const length = renamed_field_lengths[index];
            if (fieldLengthAlreadyHandled(renamed_field_lengths, index, length)) continue;
            const static_lane: ?StaticFieldLane = if ((mode == .exact or mode == .caseless) and length <= 24)
                if (renamed_field_texts) |texts| selectStaticFieldLane(renamed_field_lengths, texts, length) else null
            else
                null;

            var group_body = unknown_body;
            var field_index = matched_bodies.len;
            while (field_index > 0) {
                field_index -= 1;
                if (renamed_field_lengths[field_index] != length) continue;

                const renamed_field_expr = try self.builder.localExpr(renamed_field_locals[field_index], key_ty);
                if (static_lane) |lane| {
                    const texts = renamed_field_texts orelse Common.invariant("static field lane requested without field text");
                    const field_text = texts[field_index];
                    const probe = switch (mode) {
                        .direct => Common.invariant("direct field handles do not use string matching"),
                        .exact => try self.recordFieldNameStaticSmallWordMatch(key_local, key_ty, field_text, lane.offset, lane.active_len),
                        .caseless => try self.recordFieldNameStaticSmallWordCaselessMatch(key_local, key_ty, field_text, lane.offset, lane.active_len),
                    };
                    const matched_or_next = if (lane.offset == 0 and lane.active_len == length)
                        matched_bodies[field_index]
                    else blk: {
                        const full_cond = switch (mode) {
                            .direct => Common.invariant("direct field handles do not use string matching"),
                            .exact => try self.recordFieldNameStaticSmallExactMatch(key_local, key_ty, field_text),
                            .caseless => try self.recordFieldNameStaticSmallCaselessMatch(key_local, key_ty, field_text),
                        };
                        break :blk try self.builder.ifExpr(full_cond, matched_bodies[field_index], group_body, ret_ty);
                    };
                    group_body = try self.builder.ifExpr(probe, matched_or_next, group_body, ret_ty);
                } else {
                    const cond = try self.recordFieldNameMatch(
                        key_local,
                        key_ty,
                        renamed_field_expr,
                        if (renamed_field_texts) |texts| texts[field_index] else null,
                        mode,
                    );
                    group_body = try self.builder.ifExpr(cond, matched_bodies[field_index], group_body, ret_ty);
                }
            }

            const key_len_expr = try self.builder.localExpr(key_len_local, u64_ty);
            const length_expr = try self.builder.intLiteralExpr(length, u64_ty);
            const length_matches = try self.builder.lowLevelExpr(.num_is_eq, &.{ key_len_expr, length_expr }, bool_ty);
            body = try self.builder.ifExpr(length_matches, group_body, body, ret_ty);
        }

        const key_expr = try self.builder.localExpr(key_local, key_ty);
        const key_len_expr = try self.builder.lowLevelExpr(.str_count_utf8_bytes, &.{key_expr}, u64_ty);
        return try self.wrapLet(key_len_local, u64_ty, key_len_expr, body, ret_ty);
    }

    fn continueRecordLoopWithCurrentSlots(
        self: *BodyContext,
        ret_ty: Type.TypeId,
        cursor_expr: Ast.ExprId,
        record_slots: ParseRecordSlots,
    ) Allocator.Error!Ast.ExprId {
        const values = try self.allocator.alloc(Ast.ExprId, 1 + record_slots.payload_locals.len + record_slots.presence_locals.len);
        defer self.allocator.free(values);
        values[0] = cursor_expr;
        // Keep this order in sync with `lowerParseRecordFromState`: payload
        // writes must happen before the presence word marks them initialized.
        const payload_offset = 1;
        for (record_slots.payload_locals, record_slots.payload_tys, 0..) |local, ty, index| {
            values[payload_offset + index] = try self.builder.localExpr(local, ty);
        }
        const presence_offset = payload_offset + record_slots.payload_locals.len;
        for (record_slots.presence_locals, record_slots.presence_tys, 0..) |local, ty, index| {
            values[presence_offset + index] = try self.builder.localExpr(local, ty);
        }
        return try self.continueWith(ret_ty, values);
    }

    fn continueRecordLoopWithUpdatedField(
        self: *BodyContext,
        ret_ty: Type.TypeId,
        cursor_expr: Ast.ExprId,
        record_slots: ParseRecordSlots,
        field_value_local: Ast.LocalId,
        field_value_ty: Type.TypeId,
        replace_index: usize,
    ) Allocator.Error!Ast.ExprId {
        const values = try self.allocator.alloc(Ast.ExprId, 1 + record_slots.payload_locals.len + record_slots.presence_locals.len);
        defer self.allocator.free(values);
        values[0] = cursor_expr;
        const presence_word = recordPresenceWordIndex(replace_index);
        const presence_mask = recordPresenceMask(replace_index);
        // Keep this order in sync with `lowerParseRecordFromState`: ARC cleanup
        // of the old slot must see the old presence word before the new value is
        // marked present.
        const payload_offset = 1;
        for (record_slots.payload_locals, record_slots.payload_tys, 0..) |local, ty, index| {
            values[payload_offset + index] = if (index == replace_index) blk: {
                if (!self.sameType(ty, field_value_ty)) Common.invariant("record parser field payload type differed from slot payload type");
                break :blk try self.builder.localExpr(field_value_local, field_value_ty);
            } else try self.builder.localExpr(local, ty);
        }
        const presence_offset = payload_offset + record_slots.payload_locals.len;
        for (record_slots.presence_locals, record_slots.presence_tys, 0..) |local, ty, index| {
            values[presence_offset + index] = if (index == presence_word)
                try self.recordPresenceWithBit(local, ty, presence_mask)
            else
                try self.builder.localExpr(local, ty);
        }
        return try self.continueWith(ret_ty, values);
    }

    fn lowerParseRecordNamedFieldEvent(
        self: *BodyContext,
        shape_ty: Type.TypeId,
        encoding_expr: Ast.ExprId,
        encoding_ty: Type.TypeId,
        state_ty: Type.TypeId,
        ret_ty: Type.TypeId,
        record_slots: ParseRecordSlots,
        precomputed_plan: ?*const ParserPrecomputedPlan,
        renamed_field_locals: []const Ast.LocalId,
        renamed_field_lengths: ?[]const u32,
        renamed_field_texts: ?[]const []const u8,
        payload_local: Ast.LocalId,
        payload_ty: Type.TypeId,
        mode: RecordFieldMatchMode,
    ) Allocator.Error!Ast.ExprId {
        const str_ty = try self.builder.primitiveType(.str);
        const key_expr = try self.recordPayloadFieldAccess(payload_local, payload_ty, "name");
        if (!self.sameType(self.builder.program.exprs.items[@intFromEnum(key_expr)].ty, str_ty)) {
            Common.invariant("record named-field event key was not Str");
        }
        const rest_expr = try self.recordPayloadFieldAccess(payload_local, payload_ty, "rest");
        const key_local = try self.builder.program.addLocal(self.builder.symbols.fresh(), str_ty);
        const rest_local = try self.builder.program.addLocal(self.builder.symbols.fresh(), state_ty);
        const body = try self.lowerRecordNamedFieldDispatchBody(
            shape_ty,
            encoding_expr,
            encoding_ty,
            key_local,
            str_ty,
            rest_local,
            state_ty,
            ret_ty,
            record_slots,
            precomputed_plan,
            renamed_field_locals,
            renamed_field_lengths,
            renamed_field_texts,
            mode,
        );
        return try self.wrapLet(key_local, str_ty, key_expr, try self.wrapLet(rest_local, state_ty, rest_expr, body, ret_ty), ret_ty);
    }

    fn lowerParseRecordContinueEvent(
        self: *BodyContext,
        ret_ty: Type.TypeId,
        record_slots: ParseRecordSlots,
        continue_payload_local: Ast.LocalId,
        continue_payload_ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        const rest_expr = try self.recordPayloadFieldAccess(continue_payload_local, continue_payload_ty, "rest");
        return try self.continueRecordLoopWithCurrentSlots(ret_ty, rest_expr, record_slots);
    }

    fn lowerParseRecordDoneEvent(
        self: *BodyContext,
        shape_ty: Type.TypeId,
        encoding_expr: Ast.ExprId,
        encoding_ty: Type.TypeId,
        state_ty: Type.TypeId,
        ret_ty: Type.TypeId,
        record_slots: ParseRecordSlots,
        renamed_field_locals: []const Ast.LocalId,
        done_payload_local: Ast.LocalId,
        done_payload_ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        const ret_info = self.tryInfo(ret_ty);
        const rest_expr = try self.recordPayloadFieldAccess(done_payload_local, done_payload_ty, "rest");
        const record_try_ty = try self.tryTypeLike(ret_ty, shape_ty, ret_info.err_ty);
        const record_expr = try self.finishGeneratedRecordSlots(
            record_slots,
            shape_ty,
            record_try_ty,
            rest_expr,
            encoding_expr,
            encoding_ty,
            state_ty,
            renamed_field_locals,
        );
        const record_local = try self.builder.program.addLocal(self.builder.symbols.fresh(), shape_ty);
        const ok_body = try self.parseResultOk(
            ret_ty,
            try self.builder.localExpr(record_local, shape_ty),
            rest_expr,
            state_ty,
        );
        return try self.sequenceTry(record_expr, record_try_ty, record_local, ok_body, ret_ty);
    }

    fn lowerParseRecordDirectFieldEvent(
        self: *BodyContext,
        shape_ty: Type.TypeId,
        encoding_expr: Ast.ExprId,
        encoding_ty: Type.TypeId,
        state_ty: Type.TypeId,
        ret_ty: Type.TypeId,
        record_slots: ParseRecordSlots,
        precomputed_plan: ?*const ParserPrecomputedPlan,
        field_payload_local: Ast.LocalId,
        field_payload_ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        const rest_expr = try self.recordPayloadFieldAccess(field_payload_local, field_payload_ty, "rest");
        const field_expr = try self.recordPayloadFieldAccess(field_payload_local, field_payload_ty, "field");
        const field_ty = self.builder.program.exprs.items[@intFromEnum(field_expr)].ty;
        const field_local = try self.builder.program.addLocal(self.builder.symbols.fresh(), field_ty);
        const rest_local = try self.builder.program.addLocal(self.builder.symbols.fresh(), state_ty);
        const body = try self.lowerRecordDirectFieldDispatchBody(
            shape_ty,
            encoding_expr,
            encoding_ty,
            field_local,
            field_ty,
            rest_local,
            state_ty,
            ret_ty,
            record_slots,
            precomputed_plan,
        );
        return try self.wrapLet(field_local, field_ty, field_expr, try self.wrapLet(rest_local, state_ty, rest_expr, body, ret_ty), ret_ty);
    }

    fn lowerRecordDirectFieldDispatchBody(
        self: *BodyContext,
        shape_ty: Type.TypeId,
        encoding_expr: Ast.ExprId,
        encoding_ty: Type.TypeId,
        field_local: Ast.LocalId,
        field_ty: Type.TypeId,
        rest_local: Ast.LocalId,
        state_ty: Type.TypeId,
        ret_ty: Type.TypeId,
        record_slots: ParseRecordSlots,
        precomputed_plan: ?*const ParserPrecomputedPlan,
    ) Allocator.Error!Ast.ExprId {
        const fields = try self.allocator.dupe(Type.Field, switch (self.builder.shapeContent(shape_ty)) {
            .record => |span| self.builder.program.types.fieldSpan(span),
            .zst => &.{},
            else => Common.invariant("record direct field dispatch requested for a non-record shape"),
        });
        defer self.allocator.free(fields);
        if (fields.len != record_slots.fieldCount()) Common.invariant("record direct field dispatch state arity differed from field count");

        if (fields.len == 0) {
            return try self.lowerSkipRecordFieldNext(
                encoding_expr,
                encoding_ty,
                state_ty,
                ret_ty,
                rest_local,
                record_slots,
            );
        }

        const u64_ty = try self.builder.primitiveType(.u64);
        const index_local = try self.builder.program.addLocal(self.builder.symbols.fresh(), u64_ty);
        var body = try self.lowerParseMatchedRecordFieldNext(
            fields[fields.len - 1],
            fields.len - 1,
            encoding_expr,
            encoding_ty,
            state_ty,
            record_slots,
            precomputed_plan,
            rest_local,
            ret_ty,
        );
        var index = fields.len - 1;
        while (index > 0) {
            index -= 1;
            const matched = try self.lowerParseMatchedRecordFieldNext(
                fields[index],
                index,
                encoding_expr,
                encoding_ty,
                state_ty,
                record_slots,
                precomputed_plan,
                rest_local,
                ret_ty,
            );
            const index_expr = try self.builder.localExpr(index_local, u64_ty);
            const literal_expr = try self.builder.intLiteralExpr(@intCast(index), u64_ty);
            const cond = try self.builder.lowLevelExpr(.num_is_eq, &.{ index_expr, literal_expr }, try self.builder.primitiveType(.bool));
            body = try self.builder.ifExpr(cond, matched, body, ret_ty);
        }

        const handle_index = try self.fieldHandleIndexExpr(field_local, field_ty, u64_ty);
        return try self.wrapLet(index_local, u64_ty, handle_index, body, ret_ty);
    }

    fn lowerRecordNamedFieldDispatchBody(
        self: *BodyContext,
        shape_ty: Type.TypeId,
        encoding_expr: Ast.ExprId,
        encoding_ty: Type.TypeId,
        key_local: Ast.LocalId,
        key_ty: Type.TypeId,
        rest_local: Ast.LocalId,
        state_ty: Type.TypeId,
        ret_ty: Type.TypeId,
        record_slots: ParseRecordSlots,
        precomputed_plan: ?*const ParserPrecomputedPlan,
        renamed_field_locals: []const Ast.LocalId,
        renamed_field_lengths: ?[]const u32,
        renamed_field_texts: ?[]const []const u8,
        mode: RecordFieldMatchMode,
    ) Allocator.Error!Ast.ExprId {
        const fields = try self.allocator.dupe(Type.Field, switch (self.builder.shapeContent(shape_ty)) {
            .record => |span| self.builder.program.types.fieldSpan(span),
            .zst => &.{},
            else => Common.invariant("record named field dispatch requested for a non-record shape"),
        });
        defer self.allocator.free(fields);
        if (fields.len != record_slots.fieldCount()) Common.invariant("record named field dispatch state arity differed from field count");
        if (fields.len != renamed_field_locals.len) Common.invariant("record named field dispatch renamed field arity differed from field count");
        if (renamed_field_lengths) |lengths| {
            if (fields.len != lengths.len) Common.invariant("record named field dispatch renamed length arity differed from field count");
        }
        if (renamed_field_texts) |texts| {
            if (fields.len != texts.len) Common.invariant("record named field dispatch renamed text arity differed from field count");
        }

        const matched_bodies = try self.allocator.alloc(Ast.ExprId, fields.len);
        defer self.allocator.free(matched_bodies);
        for (fields, 0..) |field, index| {
            matched_bodies[index] = try self.lowerParseMatchedRecordFieldNext(
                field,
                index,
                encoding_expr,
                encoding_ty,
                state_ty,
                record_slots,
                precomputed_plan,
                rest_local,
                ret_ty,
            );
        }

        const unknown_body = try self.lowerSkipRecordFieldNext(
            encoding_expr,
            encoding_ty,
            state_ty,
            ret_ty,
            rest_local,
            record_slots,
        );

        return try self.lowerParseRecordNamedDispatchBody(
            key_local,
            key_ty,
            renamed_field_locals,
            renamed_field_lengths,
            renamed_field_texts,
            matched_bodies,
            unknown_body,
            mode,
            ret_ty,
        );
    }

    fn lowerSkipRecordFieldNext(
        self: *BodyContext,
        encoding_expr: Ast.ExprId,
        encoding_ty: Type.TypeId,
        state_ty: Type.TypeId,
        ret_ty: Type.TypeId,
        rest_local: Ast.LocalId,
        record_slots: ParseRecordSlots,
    ) Allocator.Error!Ast.ExprId {
        const ret_info = self.tryInfo(ret_ty);
        const skip_try_ty = try self.tryTypeLike(ret_ty, state_ty, ret_info.err_ty);
        const lookup = self.methodLookupForTypeName(encoding_ty, "skip_record_field");
        const callable_mono_ty = try self.methodTargetMonoTypeFromArgs(lookup, &.{ encoding_ty, state_ty }, skip_try_ty);
        const skip_fn = self.builder.functionShape(callable_mono_ty, "skip_record_field target method was not a function");
        const arg_tys = self.builder.program.types.span(skip_fn.args);
        if (arg_tys.len != 2) Common.invariant("skip_record_field target method had an unexpected arity");
        if (!self.sameType(arg_tys[0], encoding_ty)) Common.invariant("skip_record_field encoding type differed from record encoding type");
        if (!self.sameType(arg_tys[1], state_ty)) Common.invariant("skip_record_field state type differed from record state type");

        const skip_expr = try self.builder.program.addExpr(.{
            .ty = skip_try_ty,
            .data = .{ .call_proc = .{
                .callee = .{ .func = try self.methodTargetCalleeWithMono(lookup, callable_mono_ty) },
                .args = try self.builder.program.addExprSpan(&[_]Ast.ExprId{ encoding_expr, try self.builder.localExpr(rest_local, state_ty) }),
            } },
        });
        const skipped_local = try self.builder.program.addLocal(self.builder.symbols.fresh(), state_ty);
        const ok_body = try self.continueRecordLoopWithCurrentSlots(
            ret_ty,
            try self.builder.localExpr(skipped_local, state_ty),
            record_slots,
        );
        return try self.sequenceTry(skip_expr, skip_try_ty, skipped_local, ok_body, ret_ty);
    }

    fn lowerParseMatchedRecordFieldNext(
        self: *BodyContext,
        field: Type.Field,
        field_index: usize,
        encoding_expr: Ast.ExprId,
        encoding_ty: Type.TypeId,
        state_ty: Type.TypeId,
        record_slots: ParseRecordSlots,
        precomputed_plan: ?*const ParserPrecomputedPlan,
        rest_local: Ast.LocalId,
        ret_ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        const ret_info = self.tryInfo(ret_ty);
        const field_parse_ty = if (self.tryOptionalInfo(field.ty)) |info| info.ok_payload_ty else field.ty;
        const parse_ok_ty = try self.parseResultOkType(field_parse_ty, state_ty);
        const parse_ret_ty = try self.tryTypeLike(ret_ty, parse_ok_ty, ret_info.err_ty);
        const parse_expr = try self.lowerParseResultFromState(
            field_parse_ty,
            encoding_expr,
            encoding_ty,
            try self.builder.localExpr(rest_local, state_ty),
            state_ty,
            parse_ret_ty,
            precomputed_plan,
        );
        const value_name = try self.builder.program.names.internRecordFieldLabel("value");
        const rest_name = try self.builder.program.names.internRecordFieldLabel("rest");
        const parsed_value_local = try self.builder.program.addLocal(self.builder.symbols.fresh(), field_parse_ty);
        const field_value = if (self.tryOptionalInfo(field.ty) != null)
            try self.tryOk(field.ty, try self.builder.localExpr(parsed_value_local, field_parse_ty))
        else
            try self.builder.localExpr(parsed_value_local, field_parse_ty);
        const field_value_local = try self.builder.program.addLocal(self.builder.symbols.fresh(), field.ty);
        const parsed_rest_local = try self.builder.program.addLocal(self.builder.symbols.fresh(), state_ty);
        const ok_body = try self.continueRecordLoopWithUpdatedField(ret_ty, try self.builder.localExpr(parsed_rest_local, state_ty), record_slots, field_value_local, field.ty, field_index);
        const with_field_value = try self.wrapLet(field_value_local, field.ty, field_value, ok_body, ret_ty);
        return try self.sequenceTryRecord(parse_expr, parse_ret_ty, parsed_value_local, value_name, parsed_rest_local, rest_name, with_field_value, ret_ty);
    }

    fn lowerParseResultFromState(
        self: *BodyContext,
        shape_ty: Type.TypeId,
        encoding_expr: Ast.ExprId,
        encoding_ty: Type.TypeId,
        state_expr: Ast.ExprId,
        state_ty: Type.TypeId,
        ret_ty: Type.TypeId,
        precomputed_plan: ?*const ParserPrecomputedPlan,
    ) Allocator.Error!Ast.ExprId {
        return if (self.customParserLookup(shape_ty)) |lookup|
            try self.lowerCustomParserFromState(lookup, shape_ty, encoding_expr, encoding_ty, state_expr, state_ty, ret_ty)
        else
            try self.lowerParseShapeFromState(shape_ty, encoding_expr, encoding_ty, state_expr, state_ty, ret_ty, precomputed_plan);
    }

    fn lowerCustomParserFromState(
        self: *BodyContext,
        lookup: MethodLookup,
        shape_ty: Type.TypeId,
        encoding_expr: Ast.ExprId,
        encoding_ty: Type.TypeId,
        state_expr: Ast.ExprId,
        state_ty: Type.TypeId,
        ret_ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        const runtime_fn_ty = try self.builder.program.types.add(.{ .func = .{
            .args = try self.builder.program.types.addSpan(&.{state_ty}),
            .ret = ret_ty,
        } });
        const callable_mono_ty = try self.methodTargetMonoTypeFromArgs(lookup, &.{encoding_ty}, runtime_fn_ty);
        const parse_fn = self.builder.functionShape(callable_mono_ty, "custom parser target was not a function");
        const parse_arg_tys = self.builder.program.types.span(parse_fn.args);
        if (parse_arg_tys.len != 1) Common.invariant("custom parser target had an unexpected arity");
        if (!self.sameType(parse_arg_tys[0], encoding_ty)) Common.invariant("custom parser encoding type differed from input encoding type");
        if (!self.sameType(parse_fn.ret, runtime_fn_ty)) Common.invariant("custom parser runtime function type differed from expected type");

        const ret_info = self.tryInfo(ret_ty);
        const parse_ok_fields = switch (self.builder.shapeContent(ret_info.ok_ty)) {
            .record => |span| self.builder.program.types.fieldSpan(span),
            else => Common.invariant("custom parser result Ok type was not a parse result record"),
        };
        var found_value = false;
        for (parse_ok_fields) |field| {
            const field_text = self.builder.program.names.recordFieldLabelText(field.name);
            if (Ident.textEql(field_text, "value")) {
                found_value = true;
                if (!self.sameType(field.ty, shape_ty)) Common.invariant("custom parser value type differed from parsed shape");
            }
        }
        if (!found_value) Common.invariant("custom parser result was missing value field");

        const parser_expr = try self.builder.program.addExpr(.{
            .ty = runtime_fn_ty,
            .data = .{ .call_proc = .{
                .callee = .{ .func = try self.methodTargetCalleeWithMono(lookup, callable_mono_ty) },
                .args = try self.builder.program.addExprSpan(&[_]Ast.ExprId{encoding_expr}),
            } },
        });
        return try self.builder.program.addExpr(.{
            .ty = ret_ty,
            .data = .{ .call_value = .{
                .callee = parser_expr,
                .args = try self.builder.program.addExprSpan(&[_]Ast.ExprId{state_expr}),
            } },
        });
    }

    fn parseResultOkType(
        self: *BodyContext,
        value_ty: Type.TypeId,
        rest_ty: Type.TypeId,
    ) Allocator.Error!Type.TypeId {
        const rest_name = try self.builder.program.names.internRecordFieldLabel("rest");
        const value_name = try self.builder.program.names.internRecordFieldLabel("value");
        const fields = [_]Type.Field{
            .{ .name = rest_name, .ty = rest_ty },
            .{ .name = value_name, .ty = value_ty },
        };
        return try self.builder.program.types.add(.{ .record = try self.builder.program.types.addFields(&fields) });
    }

    fn parseResultOk(
        self: *BodyContext,
        try_ty: Type.TypeId,
        value_expr: Ast.ExprId,
        rest_expr: Ast.ExprId,
        rest_ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        const try_info = self.tryInfo(try_ty);
        const value_ty = self.builder.program.exprs.items[@intFromEnum(value_expr)].ty;
        const rest_name = try self.builder.program.names.internRecordFieldLabel("rest");
        const value_name = try self.builder.program.names.internRecordFieldLabel("value");
        const fields = [_]Ast.FieldExpr{
            .{ .name = rest_name, .value = rest_expr },
            .{ .name = value_name, .value = value_expr },
        };
        const record_expr = try self.builder.program.addExpr(.{
            .ty = try_info.ok_ty,
            .data = .{ .record = try self.builder.program.addFieldExprSpan(&fields) },
        });
        const parse_ok_ty = try self.parseResultOkType(value_ty, rest_ty);
        if (!self.sameType(try_info.ok_ty, parse_ok_ty)) Common.invariant("parse result Ok type differed from generated parse result record");
        return try self.tryOk(try_ty, record_expr);
    }

    fn uninitializedPayloadExpr(self: *BodyContext, ty: Type.TypeId, condition_local: Ast.LocalId, condition_mask: u64) Allocator.Error!Ast.ExprId {
        return try self.builder.program.addExpr(.{
            .ty = ty,
            .data = .{ .uninitialized_payload = .{ .condition = condition_local, .mask = condition_mask } },
        });
    }

    fn recordPresenceWithBit(
        self: *BodyContext,
        presence_local: Ast.LocalId,
        presence_ty: Type.TypeId,
        mask: u64,
    ) Allocator.Error!Ast.ExprId {
        const current = try self.builder.localExpr(presence_local, presence_ty);
        const bit = try self.builder.intLiteralExpr(mask, presence_ty);
        return try self.builder.lowLevelExpr(.num_bitwise_or, &.{ current, bit }, presence_ty);
    }

    fn parseRecordFieldEventType(
        self: *BodyContext,
        state_ty: Type.TypeId,
        field_handle_ty: Type.TypeId,
    ) Allocator.Error!Type.TypeId {
        const field_name_name = try self.builder.program.names.internRecordFieldLabel("field");
        const name_name = try self.builder.program.names.internRecordFieldLabel("name");
        const rest_name = try self.builder.program.names.internRecordFieldLabel("rest");
        const str_ty = try self.builder.primitiveType(.str);

        const field_fields = [_]Type.Field{
            .{ .name = field_name_name, .ty = field_handle_ty },
            .{ .name = rest_name, .ty = state_ty },
        };
        const field_payload_ty = try self.builder.program.types.add(.{ .record = try self.builder.program.types.addFields(&field_fields) });

        const try_field_fields = [_]Type.Field{
            .{ .name = name_name, .ty = str_ty },
            .{ .name = rest_name, .ty = state_ty },
        };
        const try_field_payload_ty = try self.builder.program.types.add(.{ .record = try self.builder.program.types.addFields(&try_field_fields) });

        const rest_fields = [_]Type.Field{
            .{ .name = rest_name, .ty = state_ty },
        };
        const rest_payload_ty = try self.builder.program.types.add(.{ .record = try self.builder.program.types.addFields(&rest_fields) });

        const continue_name = try self.builder.program.names.internTagLabel("Continue");
        const done_name = try self.builder.program.names.internTagLabel("Done");
        const field_name = try self.builder.program.names.internTagLabel("Field");
        const try_field_name = try self.builder.program.names.internTagLabel("TryField");
        const try_field_caseless_name = try self.builder.program.names.internTagLabel("TryFieldCaseless");
        const tags = [_]Type.Tag{
            .{
                .name = continue_name,
                .checked_name = continue_name,
                .payloads = try self.builder.program.types.addSpan(&[_]Type.TypeId{rest_payload_ty}),
            },
            .{
                .name = done_name,
                .checked_name = done_name,
                .payloads = try self.builder.program.types.addSpan(&[_]Type.TypeId{rest_payload_ty}),
            },
            .{
                .name = field_name,
                .checked_name = field_name,
                .payloads = try self.builder.program.types.addSpan(&[_]Type.TypeId{field_payload_ty}),
            },
            .{
                .name = try_field_name,
                .checked_name = try_field_name,
                .payloads = try self.builder.program.types.addSpan(&[_]Type.TypeId{try_field_payload_ty}),
            },
            .{
                .name = try_field_caseless_name,
                .checked_name = try_field_caseless_name,
                .payloads = try self.builder.program.types.addSpan(&[_]Type.TypeId{try_field_payload_ty}),
            },
        };
        return try self.builder.program.types.add(.{ .tag_union = try self.builder.program.types.addTags(&tags) });
    }

    fn recordEventFieldHandleTemplate(self: *BodyContext, event_ty: Type.TypeId) Allocator.Error!Type.TypeId {
        const field_tag = self.monoTagByText(event_ty, "Field");
        const payload_ty = self.singleTagPayloadType(field_tag, "record parse Field event");
        const field_name = try self.builder.program.names.internRecordFieldLabel("field");
        return self.builder.recordFieldType(payload_ty, field_name);
    }

    fn singleTagPayloadType(self: *BodyContext, tag: Type.Tag, comptime context: []const u8) Type.TypeId {
        const payloads = self.builder.program.types.span(tag.payloads);
        if (payloads.len != 1) Common.invariant(context ++ " had an unexpected payload count");
        return payloads[0];
    }

    fn recordPayloadFieldAccess(
        self: *BodyContext,
        payload_local: Ast.LocalId,
        payload_ty: Type.TypeId,
        comptime field_text: []const u8,
    ) Allocator.Error!Ast.ExprId {
        const field_name = try self.builder.program.names.internRecordFieldLabel(field_text);
        const field_ty = self.builder.recordFieldType(payload_ty, field_name);
        return try self.builder.program.addExpr(.{
            .ty = field_ty,
            .data = .{ .field_access = .{
                .receiver = try self.builder.localExpr(payload_local, payload_ty),
                .field = field_name,
            } },
        });
    }

    fn finishGeneratedRecordSlots(
        self: *BodyContext,
        record_slots: ParseRecordSlots,
        record_ty: Type.TypeId,
        ret_ty: Type.TypeId,
        rest_value: Ast.ExprId,
        encoding_expr: Ast.ExprId,
        encoding_ty: Type.TypeId,
        state_ty: Type.TypeId,
        renamed_field_locals: []const Ast.LocalId,
    ) Allocator.Error!Ast.ExprId {
        const ret_info = self.tryInfo(ret_ty);
        if (!self.sameType(ret_info.ok_ty, record_ty)) Common.invariant("record finish Try Ok type differed from record type");

        const record_fields = try self.allocator.dupe(Type.Field, switch (self.builder.shapeContent(record_ty)) {
            .record => |span| self.builder.program.types.fieldSpan(span),
            .zst => &.{},
            else => Common.invariant("record finish Ok type was not a record"),
        });
        defer self.allocator.free(record_fields);
        if (record_fields.len != record_slots.fieldCount()) Common.invariant("record parse state arity did not match finish record field count");
        if (record_fields.len != renamed_field_locals.len) Common.invariant("record parse renamed field arity did not match finish record field count");

        const rest_local = try self.builder.program.addLocal(self.builder.symbols.fresh(), state_ty);

        const out_fields = try self.allocator.alloc(Ast.FieldExpr, record_fields.len);
        defer self.allocator.free(out_fields);
        const field_tries = try self.allocator.alloc(?Ast.ExprId, record_fields.len);
        defer self.allocator.free(field_tries);
        @memset(field_tries, null);
        const field_try_tys = try self.allocator.alloc(?Type.TypeId, record_fields.len);
        defer self.allocator.free(field_try_tys);
        @memset(field_try_tys, null);
        const field_locals = try self.allocator.alloc(Ast.LocalId, record_fields.len);
        defer self.allocator.free(field_locals);
        for (record_fields, 0..) |field, index| {
            field_locals[index] = try self.builder.program.addLocal(self.builder.symbols.fresh(), field.ty);
            if (self.tryOptionalInfo(field.ty) == null) {
                const field_try_ty = try self.tryTypeLike(ret_ty, field.ty, ret_info.err_ty);
                field_try_tys[index] = field_try_ty;
                const presence_word = recordPresenceWordIndex(index);
                const is_present = try self.builder.localExpr(record_slots.presence_locals[presence_word], record_slots.presence_tys[presence_word]);
                field_tries[index] = try self.parseRecordFieldFromPresencePayload(
                    is_present,
                    recordPresenceMask(index),
                    record_slots.payload_locals[index],
                    record_slots.payload_tys[index],
                    field,
                    field_try_ty,
                    rest_local,
                    encoding_expr,
                    encoding_ty,
                    state_ty,
                    renamed_field_locals[index],
                );
            }
            out_fields[index] = .{
                .name = field.name,
                .value = try self.builder.localExpr(field_locals[index], field.ty),
            };
        }

        const record_expr = try self.builder.program.addExpr(.{
            .ty = ret_info.ok_ty,
            .data = .{ .record = try self.builder.program.addFieldExprSpan(out_fields) },
        });
        var body = try self.tryOk(ret_ty, record_expr);
        var field_index = record_fields.len;
        while (field_index > 0) {
            field_index -= 1;
            body = if (self.tryOptionalInfo(record_fields[field_index].ty) != null)
                try self.finishOptionalRecordFieldFromPresencePayload(
                    body,
                    ret_ty,
                    record_slots,
                    record_fields[field_index],
                    field_index,
                    field_locals[field_index],
                    rest_local,
                    encoding_expr,
                    encoding_ty,
                    state_ty,
                    renamed_field_locals[field_index],
                )
            else
                try self.sequenceTry(
                    field_tries[field_index] orelse Common.invariant("required record field was missing finish Try"),
                    field_try_tys[field_index] orelse Common.invariant("required record field was missing finish Try type"),
                    field_locals[field_index],
                    body,
                    ret_ty,
                );
        }
        return try self.wrapLet(rest_local, state_ty, rest_value, body, ret_ty);
    }

    const ParseShapeSelection = struct {
        tag_text: []const u8,
    };

    fn parseShapeSelection(self: *BodyContext, shape_ty: Type.TypeId) ParseShapeSelection {
        if (self.typeHasBuiltinOwner(shape_ty, .str)) return .{ .tag_text = "Str" };
        if (self.typeHasBuiltinOwner(shape_ty, .u64)) return .{ .tag_text = "U64" };
        return switch (self.builder.shapeContent(shape_ty)) {
            .record => .{ .tag_text = "Record" },
            .zst => .{ .tag_text = "Record" },
            .tag_union => .{ .tag_text = "TagUnion" },
            else => Common.invariant("parser shape was not supported"),
        };
    }

    fn lowerParseTagUnionDecode(
        self: *BodyContext,
        args: []const checked.CheckedExprId,
        arg_tys: []const Type.TypeId,
        ret_ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        if (args.len != 2 or arg_tys.len != 2) Common.invariant("ParseTagUnionSpec.parse reached Monotype with an unexpected arity");
        const ret_info = self.tryInfo(ret_ty);

        const value_name = try self.builder.program.names.internRecordFieldLabel("value");
        const value_ty = self.builder.recordFieldType(ret_info.ok_ty, value_name);
        const tag_span = switch (self.builder.shapeContent(value_ty)) {
            .tag_union => |span| span,
            else => Common.invariant("ParseTagUnionSpec.parse value type was not a tag union"),
        };
        const spec_ty = arg_tys[0];
        const spec_value = try self.lowerParseIntrinsicArgAtType(args[0], arg_tys[0]);
        const options_value = try self.lowerParseIntrinsicArgAtType(args[1], arg_tys[1]);
        const options_ty = arg_tys[1];
        const key_ty = self.builder.recordFieldType(options_ty, try self.builder.program.names.internRecordFieldLabel("tag"));
        const encoding_ty = self.builder.recordFieldType(options_ty, try self.builder.program.names.internRecordFieldLabel("encoding"));
        const state_ty = self.builder.recordFieldType(options_ty, try self.builder.program.names.internRecordFieldLabel("state"));
        const missing_ty = self.builder.recordFieldType(options_ty, try self.builder.program.names.internRecordFieldLabel("missing"));

        const options_local = try self.builder.program.addLocal(self.builder.symbols.fresh(), options_ty);
        const key_value = try self.recordPayloadFieldAccess(options_local, options_ty, "tag");
        const encoding_value = try self.recordPayloadFieldAccess(options_local, options_ty, "encoding");
        const slot_value = try self.recordPayloadFieldAccess(options_local, options_ty, "state");
        const missing_value = try self.recordPayloadFieldAccess(options_local, options_ty, "missing");

        const key_local = try self.builder.program.addLocal(self.builder.symbols.fresh(), key_ty);
        const encoding_local = try self.builder.program.addLocal(self.builder.symbols.fresh(), encoding_ty);
        const slot_local = try self.builder.program.addLocal(self.builder.symbols.fresh(), state_ty);
        const missing_local = try self.builder.program.addLocal(self.builder.symbols.fresh(), missing_ty);
        const tags = try self.allocator.dupe(Type.Tag, self.builder.program.types.tagSpan(tag_span));
        defer self.allocator.free(tags);

        var precomputed_plan = ParserPrecomputedPlan.init(self.allocator);
        defer precomputed_plan.deinit();
        const maybe_spec_backing_ty = if (self.generatedParseTagUnionSpecBackingInfo(spec_ty) != null)
            self.builder.namedBackingType(spec_ty) orelse Common.invariant("generated tag-union spec had no backing type")
        else
            null;
        const spec_local = if (maybe_spec_backing_ty != null)
            try self.builder.program.addLocal(self.builder.symbols.fresh(), spec_ty)
        else
            null;
        const spec_backing_local = if (maybe_spec_backing_ty) |spec_backing_ty| blk: {
            const local = try self.builder.program.addLocal(self.builder.symbols.fresh(), spec_backing_ty);
            try self.buildParserPrecomputedPlanFromTagUnionSpec(&precomputed_plan, local, spec_backing_ty, value_ty);
            break :blk local;
        } else null;

        var body = try self.unknownDecodedTagErr(ret_ty, missing_local, missing_ty);
        var index = tags.len;
        while (index > 0) {
            index -= 1;
            const tag_expr = try self.decodedTagUnionValue(
                tags[index],
                value_ty,
                ret_ty,
                encoding_local,
                encoding_ty,
                slot_local,
                state_ty,
                if (maybe_spec_backing_ty != null) &precomputed_plan else null,
            );
            const cond = try self.parseTagExactMatch(key_local, key_ty, tags[index]);
            body = try self.builder.ifExpr(cond, tag_expr, body, ret_ty);
        }

        body = try self.wrapLet(missing_local, missing_ty, missing_value, body, ret_ty);
        body = try self.wrapLet(slot_local, state_ty, slot_value, body, ret_ty);
        body = try self.wrapLet(encoding_local, encoding_ty, encoding_value, body, ret_ty);
        body = try self.wrapLet(key_local, key_ty, key_value, body, ret_ty);
        body = try self.wrapLet(options_local, options_ty, options_value, body, ret_ty);
        if (maybe_spec_backing_ty) |spec_backing_ty| {
            var capture_index = precomputed_plan.captures.items.len;
            while (capture_index > 0) {
                capture_index -= 1;
                const capture = precomputed_plan.captures.items[capture_index];
                body = try self.wrapLet(capture.local, try self.builder.primitiveType(.str), capture.value, body, ret_ty);
            }
            const backing_local = spec_backing_local orelse Common.invariant("generated tag-union spec backing local was missing");
            const spec_pat = try self.builder.program.addPat(.{
                .ty = spec_ty,
                .data = .{ .nominal = try self.builder.bindPat(backing_local, spec_backing_ty) },
            });
            const branch = Ast.Branch{ .pat = spec_pat, .body = body };
            const matched = try self.builder.program.addExpr(.{ .ty = ret_ty, .data = .{ .match_ = .{
                .scrutinee = try self.builder.localExpr(spec_local orelse Common.invariant("generated tag-union spec local was missing"), spec_ty),
                .branches = try self.builder.program.addBranchSpan(&[_]Ast.Branch{branch}),
            } } });
            return try self.wrapLet(spec_local orelse Common.invariant("generated tag-union spec local was missing"), spec_ty, spec_value, matched, ret_ty);
        }
        return try self.wrapWildcardLet(arg_tys[0], spec_value, body, ret_ty);
    }

    fn decodedTagUnionValue(
        self: *BodyContext,
        tag: Type.Tag,
        union_ty: Type.TypeId,
        ret_ty: Type.TypeId,
        encoding_local: Ast.LocalId,
        encoding_ty: Type.TypeId,
        slot_local: Ast.LocalId,
        slot_ty: Type.TypeId,
        precomputed_plan: ?*const ParserPrecomputedPlan,
    ) Allocator.Error!Ast.ExprId {
        const ret_info = self.tryInfo(ret_ty);
        const payload_tys = self.builder.program.types.span(tag.payloads);
        const payloads = try self.allocator.alloc(Ast.ExprId, payload_tys.len);
        defer self.allocator.free(payloads);
        const payload_parse_ok_tys = try self.allocator.alloc(Type.TypeId, payload_tys.len);
        defer self.allocator.free(payload_parse_ok_tys);
        const payload_parse_ret_tys = try self.allocator.alloc(Type.TypeId, payload_tys.len);
        defer self.allocator.free(payload_parse_ret_tys);
        const payload_parse_locals = try self.allocator.alloc(Ast.LocalId, payload_tys.len);
        defer self.allocator.free(payload_parse_locals);
        const value_name = try self.builder.program.names.internRecordFieldLabel("value");
        const rest_name = try self.builder.program.names.internRecordFieldLabel("rest");
        for (payload_tys, 0..) |payload_ty, index| {
            payload_parse_ok_tys[index] = try self.parseResultOkType(payload_ty, slot_ty);
            payload_parse_ret_tys[index] = try self.tryTypeLike(ret_ty, payload_parse_ok_tys[index], ret_info.err_ty);
            payload_parse_locals[index] = try self.builder.program.addLocal(self.builder.symbols.fresh(), payload_parse_ok_tys[index]);
            payloads[index] = try self.builder.program.addExpr(.{
                .ty = payload_ty,
                .data = .{ .field_access = .{
                    .receiver = try self.builder.localExpr(payload_parse_locals[index], payload_parse_ok_tys[index]),
                    .field = value_name,
                } },
            });
        }
        const tag_expr = try self.tagUnionValue(union_ty, tag, payloads);
        const final_rest_expr = if (payload_tys.len == 0)
            try self.builder.localExpr(slot_local, slot_ty)
        else
            try self.builder.program.addExpr(.{
                .ty = slot_ty,
                .data = .{ .field_access = .{
                    .receiver = try self.builder.localExpr(payload_parse_locals[payload_tys.len - 1], payload_parse_ok_tys[payload_tys.len - 1]),
                    .field = rest_name,
                } },
            });
        var body = try self.parseResultOk(ret_ty, tag_expr, final_rest_expr, slot_ty);
        var index = payload_tys.len;
        while (index > 0) {
            index -= 1;
            const slot_expr = if (index == 0)
                try self.builder.localExpr(slot_local, slot_ty)
            else
                try self.builder.program.addExpr(.{
                    .ty = slot_ty,
                    .data = .{ .field_access = .{
                        .receiver = try self.builder.localExpr(payload_parse_locals[index - 1], payload_parse_ok_tys[index - 1]),
                        .field = rest_name,
                    } },
                });
            const payload_parse = try self.lowerParseResultFromState(
                payload_tys[index],
                try self.builder.localExpr(encoding_local, encoding_ty),
                encoding_ty,
                slot_expr,
                slot_ty,
                payload_parse_ret_tys[index],
                precomputed_plan,
            );
            body = try self.sequenceTry(payload_parse, payload_parse_ret_tys[index], payload_parse_locals[index], body, ret_ty);
        }
        return body;
    }

    fn tagUnionValue(
        self: *BodyContext,
        ret_ty: Type.TypeId,
        tag: Type.Tag,
        payloads: []const Ast.ExprId,
    ) Allocator.Error!Ast.ExprId {
        const backing_ty = self.builder.namedBackingType(ret_ty);
        const tag_ty = backing_ty orelse ret_ty;
        const backing_expr = try self.builder.program.addExpr(.{
            .ty = tag_ty,
            .data = .{ .tag = .{
                .name = tag.name,
                .payloads = try self.builder.program.addExprSpan(payloads),
            } },
        });
        if (backing_ty == null) return backing_expr;
        return try self.builder.program.addExpr(.{
            .ty = ret_ty,
            .data = .{ .nominal = backing_expr },
        });
    }

    fn unknownDecodedTagErr(
        self: *BodyContext,
        ret_ty: Type.TypeId,
        missing_local: Ast.LocalId,
        missing_ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        return try self.tryErr(ret_ty, try self.builder.localExpr(missing_local, missing_ty));
    }

    fn parseTagExactMatch(
        self: *BodyContext,
        key_local: Ast.LocalId,
        key_ty: Type.TypeId,
        tag: Type.Tag,
    ) Allocator.Error!Ast.ExprId {
        if (!self.typeHasBuiltinOwner(key_ty, .str)) Common.invariant("ParseTagUnionSpec.parse key was not Str");
        const tag_text = self.builder.program.names.tagLabelText(tag.name);
        const tag_expr = try self.builder.stringExpr(tag_text, key_ty);
        const key_expr = try self.builder.localExpr(key_local, key_ty);
        return try self.builder.lowLevelExpr(.str_is_eq, &.{ key_expr, tag_expr }, try self.builder.primitiveType(.bool));
    }

    fn recordFieldNameExactMatch(
        self: *BodyContext,
        key_local: Ast.LocalId,
        key_ty: Type.TypeId,
        field_expr: Ast.ExprId,
    ) Allocator.Error!Ast.ExprId {
        const key_expr = try self.builder.localExpr(key_local, key_ty);
        return try self.builder.lowLevelExpr(.str_is_eq, &.{ key_expr, field_expr }, try self.builder.primitiveType(.bool));
    }

    fn recordFieldNameStaticSmallExactMatch(
        self: *BodyContext,
        key_local: Ast.LocalId,
        key_ty: Type.TypeId,
        field_text: []const u8,
    ) Allocator.Error!Ast.ExprId {
        if (field_text.len > 24) Common.invariant("static small field match requested for a long field name");

        var words = [3]u64{ 0, 0, 0 };
        for (field_text, 0..) |byte, index| {
            words[index / @sizeOf(u64)] |= @as(u64, byte) << @intCast((index % @sizeOf(u64)) * 8);
        }

        const u64_ty = try self.builder.primitiveType(.u64);
        const key_expr = try self.builder.localExpr(key_local, key_ty);
        const args = [_]Ast.ExprId{
            key_expr,
            try self.builder.intLiteralExpr(@intCast(field_text.len), u64_ty),
            try self.builder.intLiteralExpr(words[0], u64_ty),
            try self.builder.intLiteralExpr(words[1], u64_ty),
            try self.builder.intLiteralExpr(words[2], u64_ty),
        };
        return try self.builder.lowLevelExpr(.str_is_eq_static_small, &args, try self.builder.primitiveType(.bool));
    }

    fn recordFieldNameStaticSmallWordMatch(
        self: *BodyContext,
        key_local: Ast.LocalId,
        key_ty: Type.TypeId,
        field_text: []const u8,
        offset: u32,
        active_len: u32,
    ) Allocator.Error!Ast.ExprId {
        if (field_text.len > 24) Common.invariant("static small field lane requested for a long field name");
        const word = staticFieldLaneWord(field_text, offset, active_len);

        const u64_ty = try self.builder.primitiveType(.u64);
        const key_expr = try self.builder.localExpr(key_local, key_ty);
        const args = [_]Ast.ExprId{
            key_expr,
            try self.builder.intLiteralExpr(offset, u64_ty),
            try self.builder.intLiteralExpr(active_len, u64_ty),
            try self.builder.intLiteralExpr(word, u64_ty),
        };
        return try self.builder.lowLevelExpr(.str_static_small_word_eq, &args, try self.builder.primitiveType(.bool));
    }

    fn recordFieldNameStaticSmallCaselessMatch(
        self: *BodyContext,
        key_local: Ast.LocalId,
        key_ty: Type.TypeId,
        field_text: []const u8,
    ) Allocator.Error!Ast.ExprId {
        if (field_text.len > 24) Common.invariant("static small caseless field match requested for a long field name");

        const bool_ty = try self.builder.primitiveType(.bool);
        var body = try self.boolLiteral(true, bool_ty);
        var offset: u32 = @intCast(((field_text.len + 7) / 8) * 8);
        while (offset > 0) {
            offset -= 8;
            const remaining: u32 = @intCast(field_text.len - offset);
            const active_len: u32 = @min(remaining, 8);
            const cond = try self.recordFieldNameStaticSmallWordCaselessMatch(key_local, key_ty, field_text, offset, active_len);
            body = try self.builder.ifExpr(cond, body, try self.boolLiteral(false, bool_ty), bool_ty);
        }

        const u64_ty = try self.builder.primitiveType(.u64);
        const key_expr = try self.builder.localExpr(key_local, key_ty);
        const key_len_expr = try self.builder.lowLevelExpr(.str_count_utf8_bytes, &.{key_expr}, u64_ty);
        const length_expr = try self.builder.intLiteralExpr(@intCast(field_text.len), u64_ty);
        const length_matches = try self.builder.lowLevelExpr(.num_is_eq, &.{ key_len_expr, length_expr }, bool_ty);
        return try self.builder.ifExpr(length_matches, body, try self.boolLiteral(false, bool_ty), bool_ty);
    }

    fn recordFieldNameStaticSmallWordCaselessMatch(
        self: *BodyContext,
        key_local: Ast.LocalId,
        key_ty: Type.TypeId,
        field_text: []const u8,
        offset: u32,
        active_len: u32,
    ) Allocator.Error!Ast.ExprId {
        if (field_text.len > 24) Common.invariant("static small caseless field lane requested for a long field name");
        const word = staticFieldLaneWord(field_text, offset, active_len);

        const u64_ty = try self.builder.primitiveType(.u64);
        const key_expr = try self.builder.localExpr(key_local, key_ty);
        const args = [_]Ast.ExprId{
            key_expr,
            try self.builder.intLiteralExpr(offset, u64_ty),
            try self.builder.intLiteralExpr(active_len, u64_ty),
            try self.builder.intLiteralExpr(word, u64_ty),
        };
        return try self.builder.lowLevelExpr(.str_static_small_word_caseless_eq, &args, try self.builder.primitiveType(.bool));
    }

    fn renamedRecordFieldNameExpr(
        self: *BodyContext,
        encoding_expr: Ast.ExprId,
        encoding_ty: Type.TypeId,
        field: Type.Field,
        str_ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        const field_text = self.builder.program.names.recordFieldLabelText(field.name);
        const field_expr = try self.builder.stringExpr(field_text, str_ty);
        const lookup = self.methodLookupForTypeName(encoding_ty, "rename_field");
        const callable_mono_ty = try self.methodTargetMonoTypeFromArgs(lookup, &.{ encoding_ty, str_ty }, str_ty);
        const rename_fn = self.builder.functionShape(callable_mono_ty, "rename_field target method was not a function");
        const arg_tys = self.builder.program.types.span(rename_fn.args);
        if (arg_tys.len != 2) Common.invariant("rename_field target method had an unexpected arity");
        if (!self.sameType(arg_tys[0], encoding_ty)) Common.invariant("rename_field encoding argument differed from parser encoding type");
        if (!self.sameType(arg_tys[1], str_ty)) Common.invariant("rename_field name argument differed from Str");
        if (!self.sameType(rename_fn.ret, str_ty)) Common.invariant("rename_field return type differed from Str");
        return try self.builder.program.addExpr(.{
            .ty = str_ty,
            .data = .{ .call_proc = .{
                .callee = .{ .func = try self.methodTargetCalleeWithMono(lookup, callable_mono_ty) },
                .args = try self.builder.program.addExprSpan(&[_]Ast.ExprId{ encoding_expr, field_expr }),
            } },
        });
    }

    fn recordFieldNameMatch(
        self: *BodyContext,
        key_local: Ast.LocalId,
        key_ty: Type.TypeId,
        field_expr: Ast.ExprId,
        precomputed_field_text: ?[]const u8,
        mode: RecordFieldMatchMode,
    ) Allocator.Error!Ast.ExprId {
        return switch (mode) {
            .direct => Common.invariant("direct field handles do not use string matching"),
            .exact => if (precomputed_field_text) |field_text|
                if (field_text.len <= 24)
                    try self.recordFieldNameStaticSmallExactMatch(key_local, key_ty, field_text)
                else
                    try self.recordFieldNameExactMatch(key_local, key_ty, field_expr)
            else
                try self.recordFieldNameExactMatch(key_local, key_ty, field_expr),
            .caseless => blk: {
                if (precomputed_field_text) |field_text| {
                    if (field_text.len <= 24) break :blk try self.recordFieldNameStaticSmallCaselessMatch(key_local, key_ty, field_text);
                }
                const key_expr = try self.builder.localExpr(key_local, key_ty);
                break :blk try self.builder.lowLevelExpr(.str_caseless_ascii_equals, &.{ key_expr, field_expr }, try self.builder.primitiveType(.bool));
            },
        };
    }

    fn wrapLet(
        self: *BodyContext,
        local: Ast.LocalId,
        ty: Type.TypeId,
        value: Ast.ExprId,
        rest: Ast.ExprId,
        ret_ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        return try self.builder.program.addExpr(.{ .ty = ret_ty, .data = .{ .let_ = .{
            .bind = try self.builder.bindPat(local, ty),
            .value = value,
            .rest = rest,
        } } });
    }

    fn wrapWildcardLet(
        self: *BodyContext,
        ty: Type.TypeId,
        value: Ast.ExprId,
        rest: Ast.ExprId,
        ret_ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        return try self.builder.program.addExpr(.{ .ty = ret_ty, .data = .{ .let_ = .{
            .bind = try self.builder.program.addPat(.{ .ty = ty, .data = .wildcard }),
            .value = value,
            .rest = rest,
        } } });
    }

    fn lowerCall(self: *BodyContext, checked_ret_ty: checked.CheckedTypeId, call: anytype) Allocator.Error!LoweredCall {
        if (try self.lowerCallThatCannotReachCallee(checked_ret_ty, call)) |lowered| return lowered;

        if (call.direct_target) |target| {
            var call_ctx = try BodyContext.init(self.allocator, self.builder, self.view, self.owner_template, self.graph);
            defer call_ctx.deinit();
            call_ctx.owner_context_fn_key = self.owner_context_fn_key;
            call_ctx.current_fn_key = self.current_fn_key;

            const source_fn_ty = self.directCallInstantiationSourceFnType(target, call.source_fn_ty_payload);
            const mono_fn_ty = try call_ctx.instantiateCallTypeFromCaller(source_fn_ty, self, checked_ret_ty, call.args);
            const source_fn_key = call_ctx.view.types.rootKey(source_fn_ty);
            const callee = try self.fnTemplateForDirectCallWithMono(target, source_fn_ty, source_fn_key, mono_fn_ty);
            const fn_data = self.builder.functionShape(mono_fn_ty, "checked direct call target had a non-function type");
            try self.constrainTypeToMono(checked_ret_ty, fn_data.ret);
            return .{
                .ret_ty = fn_data.ret,
                .data = .{ .call_proc = .{
                    .callee = .{ .func = callee },
                    .args = try self.lowerExprSpanAtTypes(call.args, self.builder.program.types.span(fn_data.args)),
                } },
            };
        }

        const fn_ty = (try self.indirectCalleeMonoType(call.func)) orelse fn_ty: {
            var call_ctx = try BodyContext.init(self.allocator, self.builder, self.view, self.owner_template, self.graph);
            defer call_ctx.deinit();
            call_ctx.owner_context_fn_key = self.owner_context_fn_key;
            call_ctx.current_fn_key = self.current_fn_key;

            break :fn_ty try call_ctx.instantiateCallTypeFromCaller(call.source_fn_ty_payload, self, checked_ret_ty, call.args);
        };
        const fn_data = self.builder.functionShape(fn_ty, "checked call function type was not a function");
        return .{
            .ret_ty = fn_data.ret,
            .data = .{ .call_value = .{
                .callee = try self.lowerExprAtType(call.func, fn_ty),
                .args = try self.lowerExprSpanAtTypes(call.args, self.builder.program.types.span(fn_data.args)),
            } },
        };
    }

    fn indirectCalleeMonoType(self: *BodyContext, checked_func: checked.CheckedExprId) Allocator.Error!?Type.TypeId {
        const expr = self.view.bodies.expr(checked_func);
        return switch (expr.data) {
            .lookup_local,
            .lookup_external,
            .lookup_required,
            => self.localCalleeMonoType(checked_func),
            .field_access => try self.lowerExprType(checked_func),
            else => null,
        };
    }

    fn localCalleeMonoType(self: *BodyContext, checked_func: checked.CheckedExprId) ?Type.TypeId {
        const expr = self.view.bodies.expr(checked_func);
        const maybe_ref = switch (expr.data) {
            .lookup_local => |lookup| lookup.resolved,
            .lookup_external => |resolved| resolved,
            .lookup_required => |resolved| resolved,
            else => return null,
        };
        const ref_id = maybe_ref orelse Common.invariant("checked callee lookup reached Monotype without resolved value ref");
        const local_id = self.currentLocalForResolvedValue(ref_id) orelse return null;
        return self.builder.program.locals.items[@intFromEnum(local_id)].ty;
    }

    fn directCallInstantiationSourceFnType(
        self: *BodyContext,
        target: checked.ResolvedValueId,
        fallback: checked.CheckedTypeId,
    ) checked.CheckedTypeId {
        const raw = @intFromEnum(target);
        if (raw >= self.view.resolved_refs.records.len) {
            Common.invariant("checked direct call target is outside resolved value table");
        }
        return switch (self.view.resolved_refs.records[raw].ref) {
            .platform_required_proc => |required| required.procedure.source_fn_ty_payload orelse
                Common.invariant("platform-required procedure call missing relation-owned requested function type"),
            else => fallback,
        };
    }

    fn lowerCallThatCannotReachCallee(
        self: *BodyContext,
        checked_ret_ty: checked.CheckedTypeId,
        call: anytype,
    ) Allocator.Error!?LoweredCall {
        if (self.checkedExprDiverges(call.func)) {
            return try self.lowerDivergentCallOperand(checked_ret_ty, call.func);
        }
        for (call.args) |arg| {
            if (self.checkedExprDiverges(arg)) {
                return try self.lowerDivergentCallOperand(checked_ret_ty, arg);
            }
        }
        return null;
    }

    fn lowerDivergentCallOperand(
        self: *BodyContext,
        checked_ret_ty: checked.CheckedTypeId,
        operand: checked.CheckedExprId,
    ) Allocator.Error!LoweredCall {
        const ret_ty = try self.lowerType(checked_ret_ty);
        return .{
            .ret_ty = ret_ty,
            .data = try self.lowerDivergentExprDataAtType(operand, ret_ty),
        };
    }

    fn instantiateCallTypeFromCaller(
        self: *BodyContext,
        source_fn_ty: checked.CheckedTypeId,
        caller: *BodyContext,
        checked_ret_ty: checked.CheckedTypeId,
        checked_args: []const checked.CheckedExprId,
    ) Allocator.Error!Type.TypeId {
        return self.instantiateCallTypeFromCallerAtType(source_fn_ty, caller, checked_ret_ty, checked_args, null);
    }

    fn instantiateCallTypeFromCallerAtType(
        self: *BodyContext,
        source_fn_ty: checked.CheckedTypeId,
        caller: *BodyContext,
        checked_ret_ty: checked.CheckedTypeId,
        checked_args: []const checked.CheckedExprId,
        expected_ret_ty: ?Type.TypeId,
    ) Allocator.Error!Type.TypeId {
        const function = self.checkedFunctionType(source_fn_ty);
        if (function.args.len != checked_args.len) {
            Common.invariant("checked direct call arity differs from its function type");
        }
        const fn_node = try self.instNode(source_fn_ty);
        const generated_arg_overrides = try self.allocator.alloc(?Type.TypeId, function.args.len);
        defer self.allocator.free(generated_arg_overrides);
        @memset(generated_arg_overrides, null);
        var saw_generated_opaque_evidence = false;
        for (function.args, checked_args, 0..) |formal_ty, checked_arg, index| {
            const arg_ty = caller.view.bodies.expr(checked_arg).ty;
            const formal_node = try self.instNode(formal_ty);
            try self.graph.unify(formal_node, try caller.instNode(arg_ty));
            if (try caller.callArgumentMonoType(checked_arg, null)) |evidence_ty| {
                const evidence_node = try self.graph.importMono(evidence_ty);
                if (self.isGeneratedOpaqueEvidenceType(evidence_ty)) {
                    saw_generated_opaque_evidence = true;
                    generated_arg_overrides[index] = evidence_ty;
                    try self.graph.unify(try self.graph.importMono(try self.publicOpaqueUnificationType(evidence_ty)), formal_node);
                } else {
                    try self.graph.unify(formal_node, evidence_node);
                }
            }
        }
        try self.graph.unify(try self.instNode(function.ret), try caller.instNode(checked_ret_ty));
        if (expected_ret_ty) |expected| {
            try self.graph.unify(try self.instNode(function.ret), try self.graph.importMono(expected));
        }
        try self.graph.drainDirty();
        if (saw_generated_opaque_evidence) {
            const arg_tys = try self.allocator.alloc(Type.TypeId, function.args.len);
            defer self.allocator.free(arg_tys);
            for (function.args, generated_arg_overrides, 0..) |formal_ty, override, index| {
                arg_tys[index] = override orelse try self.graph.monoFor(try self.instNode(formal_ty));
            }
            return try self.builder.program.types.add(.{ .func = .{
                .args = try self.builder.program.types.addSpan(arg_tys),
                .ret = try self.graph.monoFor(try self.instNode(function.ret)),
            } });
        }
        return try self.graph.monoFor(fn_node);
    }

    fn instantiateDispatchPlanCallTypeFromCaller(
        self: *BodyContext,
        source_fn_ty: checked.CheckedTypeId,
        caller: *BodyContext,
        checked_ret_ty: checked.CheckedTypeId,
        operands: []const static_dispatch.StaticDispatchOperand,
        expected_ret_ty: ?Type.TypeId,
    ) Allocator.Error!Type.TypeId {
        const function = self.checkedFunctionType(source_fn_ty);
        if (function.args.len != operands.len) {
            Common.invariant("checked dispatch plan arity differs from its function type");
        }
        const fn_node = try self.instNode(source_fn_ty);
        for (function.args, operands) |formal_ty, operand| {
            try self.relateFormalToOperand(formal_ty, caller, operand);
        }
        try self.graph.unify(try self.instNode(function.ret), try caller.instNode(checked_ret_ty));
        if (expected_ret_ty) |expected| {
            try self.graph.unify(try self.instNode(function.ret), try self.graph.importMono(expected));
        }
        try self.graph.drainDirty();
        return try self.graph.monoFor(fn_node);
    }

    fn relateFormalToOperand(
        self: *BodyContext,
        formal_ty: checked.CheckedTypeId,
        caller: *BodyContext,
        operand: static_dispatch.StaticDispatchOperand,
    ) Allocator.Error!void {
        switch (operand) {
            .checked_expr => |checked_arg| {
                const arg_ty = caller.view.bodies.expr(checked_arg).ty;
                const formal_node = try self.instNode(formal_ty);
                try self.graph.unify(formal_node, try caller.instNode(arg_ty));
                if (try caller.callArgumentMonoType(checked_arg, null)) |evidence_ty| {
                    const evidence_node = try self.graph.importMono(evidence_ty);
                    if (self.isGeneratedOpaqueEvidenceType(evidence_ty)) {
                        try self.graph.unify(try self.graph.importMono(try self.publicOpaqueUnificationType(evidence_ty)), formal_node);
                    } else {
                        try self.graph.unify(formal_node, evidence_node);
                    }
                }
            },
            .generated_interpolation_iter,
            .generated_numeral,
            .generated_quote,
            => {},
        }
    }

    fn instantiateNumeralPlanCallType(
        self: *BodyContext,
        source_fn_ty: checked.CheckedTypeId,
        caller: *BodyContext,
        checked_ret_ty: checked.CheckedTypeId,
        target_ty: Type.TypeId,
        operands: []const static_dispatch.StaticDispatchOperand,
    ) Allocator.Error!Type.TypeId {
        const function = self.checkedFunctionType(source_fn_ty);
        if (function.args.len != operands.len) {
            Common.invariant("checked from_numeral plan arity differs from its function type");
        }
        const fn_node = try self.instNode(source_fn_ty);
        // The numeral expression's checked type is the converted value type;
        // the plan's checked structure relates it to the Try-shaped return.
        try self.graph.unify(try caller.instNode(checked_ret_ty), try self.graph.importMono(target_ty));
        try self.graph.unify(try self.instNode(checked_ret_ty), try self.graph.importMono(target_ty));
        for (function.args, operands) |formal_ty, operand| {
            try self.relateFormalToOperand(formal_ty, caller, operand);
        }
        try self.graph.drainDirty();
        return try self.graph.monoFor(fn_node);
    }

    fn instantiateTargetFromPlan(
        self: *BodyContext,
        source_fn_ty: checked.CheckedTypeId,
        plan_ctx: *BodyContext,
        plan_fn_ty: checked.CheckedTypeId,
        expected_ret_ty: ?Type.TypeId,
    ) Allocator.Error!Type.TypeId {
        const function = self.checkedFunctionType(source_fn_ty);
        const plan_function = plan_ctx.checkedFunctionType(plan_fn_ty);
        if (function.args.len != plan_function.args.len) {
            Common.invariant("checked dispatch target arity differed from its dispatch plan");
        }
        const fn_node = try self.instNode(source_fn_ty);
        try self.graph.unify(fn_node, try plan_ctx.instNode(plan_fn_ty));
        if (expected_ret_ty) |expected| {
            try self.graph.unify(try self.instNode(function.ret), try self.graph.importMono(expected));
        }
        try self.graph.drainDirty();
        return try self.graph.monoFor(fn_node);
    }

    fn instantiateTargetCallTypeFromMonoArgs(
        self: *BodyContext,
        source_fn_ty: checked.CheckedTypeId,
        arg_tys: []const Type.TypeId,
        ret_ty: Type.TypeId,
    ) Allocator.Error!Type.TypeId {
        const function = self.checkedFunctionType(source_fn_ty);
        if (function.args.len != arg_tys.len) {
            Common.invariant("checked synthetic dispatch target arity differs from its function type");
        }
        const fn_node = try self.instNode(source_fn_ty);
        for (function.args, arg_tys) |formal_ty, arg_ty| {
            try self.graph.unify(try self.instNode(formal_ty), try self.graph.importMono(arg_ty));
        }
        try self.graph.unify(try self.instNode(function.ret), try self.graph.importMono(ret_ty));
        try self.graph.drainDirty();
        return try self.graph.monoFor(fn_node);
    }

    fn instantiateTargetCallTypeFromMonoArgsPreservingArgs(
        self: *BodyContext,
        source_fn_ty: checked.CheckedTypeId,
        arg_tys: []const Type.TypeId,
        ret_ty: Type.TypeId,
    ) Allocator.Error!Type.TypeId {
        const function = self.checkedFunctionType(source_fn_ty);
        if (function.args.len != arg_tys.len) {
            Common.invariant("checked synthetic dispatch target arity differs from its function type");
        }
        for (function.args, arg_tys) |formal_ty, arg_ty| {
            try self.graph.unify(try self.graph.importMono(try self.publicOpaqueUnificationType(arg_ty)), try self.instNode(formal_ty));
        }
        try self.graph.unify(try self.instNode(function.ret), try self.graph.importMono(ret_ty));
        try self.graph.drainDirty();
        return try self.builder.program.types.add(.{ .func = .{
            .args = try self.builder.program.types.addSpan(arg_tys),
            .ret = ret_ty,
        } });
    }

    fn instantiateTargetCallTypeFromMonoArgAtIndexAndRet(
        self: *BodyContext,
        source_fn_ty: checked.CheckedTypeId,
        arg_index: usize,
        arg_ty: Type.TypeId,
        ret_ty: Type.TypeId,
    ) Allocator.Error!Type.TypeId {
        const function = self.checkedFunctionType(source_fn_ty);
        if (arg_index >= function.args.len) {
            Common.invariant("checked synthetic dispatch target argument index was outside its function type");
        }
        const fn_node = try self.instNode(source_fn_ty);
        try self.graph.unify(try self.instNode(function.args[arg_index]), try self.graph.importMono(arg_ty));
        try self.graph.unify(try self.instNode(function.ret), try self.graph.importMono(ret_ty));
        try self.graph.drainDirty();
        return try self.graph.monoFor(fn_node);
    }

    fn instantiateTargetCallTypeFromMonoArgAtIndex(
        self: *BodyContext,
        source_fn_ty: checked.CheckedTypeId,
        arg_index: usize,
        arg_ty: Type.TypeId,
    ) Allocator.Error!Type.TypeId {
        const function = self.checkedFunctionType(source_fn_ty);
        if (arg_index >= function.args.len) {
            Common.invariant("checked synthetic dispatch target argument index was outside its function type");
        }
        const fn_node = try self.instNode(source_fn_ty);
        try self.graph.unify(try self.instNode(function.args[arg_index]), try self.graph.importMono(arg_ty));
        try self.graph.drainDirty();
        return try self.graph.monoFor(fn_node);
    }

    fn callArgumentMonoType(
        self: *BodyContext,
        checked_arg: checked.CheckedExprId,
        expected_ty: ?Type.TypeId,
    ) Allocator.Error!?Type.TypeId {
        const expr = self.view.bodies.expr(checked_arg);
        switch (expr.data) {
            .call => |call| return try self.callResultMonoType(expr.ty, call, expected_ty),
            .dispatch_call => |plan| return try self.dispatchResultMonoType(expr.ty, plan, expected_ty),
            .interpolation => |interpolation| return try self.dispatchResultMonoType(expr.ty, interpolation.plan, expected_ty),
            .type_dispatch_call => |plan| return try self.dispatchResultMonoType(expr.ty, plan, expected_ty),
            .method_eq => |plan| return try self.dispatchResultMonoType(expr.ty, plan, expected_ty),
            .field_access => |field| return try self.fieldAccessMonoType(field.receiver, field.field_name),
            .lookup_local => |lookup| return try self.lookupExprMonoType(expr.ty, lookup.resolved),
            .lookup_external => |resolved| return try self.lookupExprMonoType(expr.ty, resolved),
            .lookup_required => |resolved| return try self.lookupExprMonoType(expr.ty, resolved),
            else => {},
        }
        if (expected_ty) |ty| {
            try self.constrainTypeToMono(expr.ty, ty);
            return ty;
        }
        return try self.lowerType(expr.ty);
    }

    fn lookupExprMonoType(
        self: *BodyContext,
        checked_ty: checked.CheckedTypeId,
        maybe_ref: ?checked.ResolvedValueId,
    ) Allocator.Error!Type.TypeId {
        const ref_id = maybe_ref orelse Common.invariant("checked lookup reached Monotype without resolved value ref");
        const record = self.view.resolved_refs.records[@intFromEnum(ref_id)];
        switch (record.ref) {
            .local_value,
            .pattern_binder,
            => {},
            .selected_hoisted_const => |selected| {
                if (try self.selectedHoistedConstMonoType(selected, checked_ty)) |ty| return ty;
            },
            .local_param,
            .local_mutable_version,
            .local_proc,
            .top_level_const,
            .imported_const,
            .top_level_proc,
            .imported_proc,
            .hosted_proc,
            .platform_required_declaration,
            .platform_required_const,
            .platform_required_proc,
            .promoted_top_level_proc,
            => {},
        }
        if (self.currentLocalForResolvedValue(ref_id)) |local_id| {
            const local_ty = self.builder.program.locals.items[@intFromEnum(local_id)].ty;
            try self.constrainTypeToMono(checked_ty, try self.publicOpaqueUnificationType(local_ty));
            return local_ty;
        }
        return switch (record.ref) {
            .top_level_const => |const_use| try self.constUseMonoType(const_use),
            .imported_const => |const_use| try self.constUseMonoType(const_use),
            .platform_required_const => |required| try self.constUseMonoType(required.const_use),
            .local_param,
            .local_value,
            .local_mutable_version,
            .pattern_binder,
            .local_proc,
            .selected_hoisted_const,
            .top_level_proc,
            .imported_proc,
            .hosted_proc,
            .platform_required_declaration,
            .platform_required_proc,
            .promoted_top_level_proc,
            => try self.lowerType(checked_ty),
        };
    }

    fn fieldAccessMonoType(
        self: *BodyContext,
        receiver: checked.CheckedExprId,
        field_name: names.RecordFieldNameId,
    ) Allocator.Error!Type.TypeId {
        const receiver_ty = try self.lowerExprType(receiver);
        return self.builder.recordFieldType(
            receiver_ty,
            try self.builder.recordFieldName(self.view, field_name),
        );
    }

    fn callResultMonoType(
        self: *BodyContext,
        checked_ret_ty: checked.CheckedTypeId,
        call: anytype,
        expected_ret_ty: ?Type.TypeId,
    ) Allocator.Error!?Type.TypeId {
        if (call.direct_target == null) {
            if (try self.indirectCalleeMonoType(call.func)) |fn_ty| {
                const ret_ty = self.functionReturnType(fn_ty);
                if (expected_ret_ty) |expected| {
                    if (!self.sameType(expected, ret_ty)) {
                        Common.invariant("checked indirect call result type differed from its expected Monotype type");
                    }
                    try self.constrainTypeToMono(checked_ret_ty, expected);
                    return expected;
                }
                try self.constrainTypeToMono(checked_ret_ty, ret_ty);
                return ret_ty;
            }

            var call_ctx = try BodyContext.init(self.allocator, self.builder, self.view, self.owner_template, self.graph);
            defer call_ctx.deinit();
            call_ctx.owner_context_fn_key = self.owner_context_fn_key;
            call_ctx.current_fn_key = self.current_fn_key;

            const mono_fn_ty = try call_ctx.instantiateCallTypeFromCallerAtType(call.source_fn_ty_payload, self, checked_ret_ty, call.args, expected_ret_ty);
            return call_ctx.functionReturnType(mono_fn_ty);
        }

        var call_ctx = try BodyContext.init(self.allocator, self.builder, self.view, self.owner_template, self.graph);
        defer call_ctx.deinit();
        call_ctx.owner_context_fn_key = self.owner_context_fn_key;
        call_ctx.current_fn_key = self.current_fn_key;

        const source_fn_ty = self.directCallInstantiationSourceFnType(call.direct_target.?, call.source_fn_ty_payload);
        const mono_fn_ty = try call_ctx.instantiateCallTypeFromCallerAtType(source_fn_ty, self, checked_ret_ty, call.args, expected_ret_ty);
        return call_ctx.functionReturnType(mono_fn_ty);
    }

    fn fnTemplateForDirectCallWithMono(
        self: *BodyContext,
        target: checked.ResolvedValueId,
        source_fn_ty: checked.CheckedTypeId,
        source_fn_key: names.TypeDigest,
        mono_fn_ty: Type.TypeId,
    ) Allocator.Error!Ast.FnId {
        const raw = @intFromEnum(target);
        if (raw >= self.view.resolved_refs.records.len) {
            Common.invariant("checked direct call target is outside resolved value table");
        }
        return switch (self.view.resolved_refs.records[raw].ref) {
            .local_proc => |local| try self.fnTemplateForLocalProcWithMono(local, source_fn_ty, source_fn_key, mono_fn_ty),
            .top_level_proc,
            .imported_proc,
            .hosted_proc,
            .promoted_top_level_proc,
            => |proc| try self.fnDefForProcedureUseWithMono(proc, source_fn_ty, source_fn_key, mono_fn_ty),
            .platform_required_proc => |proc| try self.fnDefForProcedureUseWithMono(proc.procedure, source_fn_ty, source_fn_key, mono_fn_ty),
            .local_param,
            .local_value,
            .local_mutable_version,
            .pattern_binder,
            .selected_hoisted_const,
            .top_level_const,
            .imported_const,
            .platform_required_declaration,
            .platform_required_const,
            => Common.invariant("checked direct call target was not a procedure"),
        };
    }

    fn fnTemplateForLocalProcWithMono(
        self: *BodyContext,
        local: checked.LocalProcedureBinding,
        source_fn_ty: checked.CheckedTypeId,
        source_fn_key: names.TypeDigest,
        mono_fn_ty: Type.TypeId,
    ) Allocator.Error!Ast.FnId {
        const context_fn_key = self.local_proc_contexts.get(local.binder) orelse
            Common.invariant("local procedure use reached Monotype before its declaration context");
        const fn_template = try self.builder.fnTemplateForNestedExprWithMono(
            self.view,
            self.owner_template,
            local.expr,
            source_fn_ty,
            source_fn_key,
            mono_fn_ty,
            context_fn_key,
        );
        return try self.builder.lowerNestedFnFromContext(self, local.expr, fn_template);
    }

    fn fnDefForProcedureUseWithMono(
        self: *BodyContext,
        proc: checked.ProcedureUseTemplate,
        source_fn_ty: checked.CheckedTypeId,
        source_fn_key: names.TypeDigest,
        mono_fn_ty: Type.TypeId,
    ) Allocator.Error!Ast.FnId {
        const fn_template = switch (proc.binding) {
            .top_level => |top_level| blk: {
                const view = self.builder.moduleForId(checked.topLevelProcedureModuleId(top_level));
                const binding = view.top_level_procedure_bindings.get(top_level.binding);
                const binding_source = schemeRoot(view, binding.source_scheme, "top-level procedure binding source scheme was not output");
                break :blk self.builder.fnDefForProcedureBindingBody(view, binding.body, binding_source, view.types.rootKey(binding_source), mono_fn_ty);
            },
            .imported => |imported| blk: {
                const view = self.builder.moduleForId(checked.importedProcedureModuleId(imported));
                for (view.exported_procedure_bindings.bindings) |binding| {
                    if (binding.binding.def == imported.def and binding.binding.pattern == imported.pattern) {
                        const binding_source = schemeRoot(view, binding.source_scheme, "imported procedure binding source scheme was not output");
                        break :blk self.builder.fnDefForImportedBindingBody(view, binding.body, binding_source, view.types.rootKey(binding_source), mono_fn_ty);
                    }
                }
                Common.invariant("imported procedure binding was not exported by its checked module");
            },
            .hosted => |hosted| blk: {
                break :blk self.builder.fnDefForTemplate(
                    self.builder.moduleForId(moduleIdFromDigest(names.procTemplateModuleDigest(hosted.template))),
                    hosted.template,
                    source_fn_ty,
                    source_fn_key,
                    mono_fn_ty,
                );
            },
            .platform_required => |required| blk: {
                const app_view = self.builder.moduleForId(checked.requiredProcedureModuleId(required));
                const binding = app_view.top_level_procedure_bindings.get(required.procedure_binding);
                break :blk self.builder.fnDefForProcedureBindingBody(app_view, binding.body, source_fn_ty, source_fn_key, mono_fn_ty);
            },
        };
        return try self.builder.lowerFnTemplateDefFromContext(self, fn_template);
    }

    fn currentLocalBindingForResolvedValue(self: *BodyContext, ref_id: checked.ResolvedValueId) ?CurrentLocal {
        const raw = @intFromEnum(ref_id);
        if (raw >= self.view.resolved_refs.records.len) {
            Common.invariant("checked lookup resolved value id was outside resolved value table");
        }
        const record = self.view.resolved_refs.records[raw];
        return switch (record.ref) {
            .local_param,
            .local_value,
            .local_mutable_version,
            .pattern_binder,
            => |local| .{
                .binder = local.binder,
                .local = self.binders.get(local.binder) orelse
                    Common.invariant("local lookup referenced an unbound pattern binder"),
            },
            .selected_hoisted_const => |selected| .{
                .binder = selected.local.binder,
                .local = self.binders.get(selected.local.binder) orelse
                    Common.invariant("selected hoisted local lookup referenced an unbound pattern binder"),
            },
            .local_proc,
            .top_level_const,
            .imported_const,
            .top_level_proc,
            .imported_proc,
            .hosted_proc,
            .platform_required_declaration,
            .platform_required_const,
            .platform_required_proc,
            .promoted_top_level_proc,
            => null,
        };
    }

    fn currentLocalForResolvedValue(self: *BodyContext, ref_id: checked.ResolvedValueId) ?Ast.LocalId {
        return if (self.currentLocalBindingForResolvedValue(ref_id)) |binding| binding.local else null;
    }

    fn lowerLookupExprAtType(
        self: *BodyContext,
        checked_ty: checked.CheckedTypeId,
        maybe_ref: ?checked.ResolvedValueId,
        ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        const ref_id = maybe_ref orelse Common.invariant("checked lookup reached Monotype without resolved value ref");
        const record = self.view.resolved_refs.records[@intFromEnum(ref_id)];
        switch (record.ref) {
            .local_value,
            .pattern_binder,
            => {},
            .selected_hoisted_const => |selected| {
                if (try self.restoreSelectedHoistedConstAtType(selected, ty)) |restored| return restored;
            },
            .local_param,
            .local_mutable_version,
            .local_proc,
            .top_level_const,
            .imported_const,
            .top_level_proc,
            .imported_proc,
            .hosted_proc,
            .platform_required_declaration,
            .platform_required_const,
            .platform_required_proc,
            .promoted_top_level_proc,
            => {},
        }
        if (self.currentLocalBindingForResolvedValue(ref_id)) |binding| {
            const local_id = binding.local;
            const local_ty = self.builder.program.locals.items[@intFromEnum(local_id)].ty;
            const binder_ty = checkedBinderType(self.view, binding.binder);
            // The local's Monotype identifies the binder's node in the
            // context that bound it; importing it unifies this context's
            // instantiation with that node before the use type is unified.
            try self.constrainTypeToMono(binder_ty, try self.publicOpaqueUnificationType(local_ty));
            try self.constrainTypeToMono(binder_ty, try self.publicOpaqueUnificationType(ty));
            try self.constrainTypeToMono(checked_ty, try self.publicOpaqueUnificationType(ty));
            const live_ty = try self.lowerType(binder_ty);
            const use_ty = if (self.sameType(ty, local_ty))
                local_ty
            else if (self.sameType(ty, live_ty))
                live_ty
            else
                Common.invariant("checked local lookup type differed from its expected Monotype use type");
            if (use_ty != local_ty) self.builder.program.setLocalType(local_id, use_ty);
            return try self.builder.program.addExpr(.{ .ty = use_ty, .data = .{ .local = local_id } });
        }

        switch (record.ref) {
            .top_level_const => |const_use| return try self.restoreConstUseAtType(const_use, ty),
            .imported_const => |const_use| return try self.restoreConstUseAtType(const_use, ty),
            .platform_required_const => |required| return try self.restoreConstUseAtType(required.const_use, ty),
            .local_param,
            .local_value,
            .local_mutable_version,
            .pattern_binder,
            .local_proc,
            .selected_hoisted_const,
            .top_level_proc,
            .imported_proc,
            .hosted_proc,
            .platform_required_declaration,
            .platform_required_proc,
            .promoted_top_level_proc,
            => {},
        }

        try self.constrainTypeToMono(checked_ty, ty);
        const data: Ast.ExprData = switch (record.ref) {
            .local_param,
            .local_value,
            .local_mutable_version,
            .pattern_binder,
            .selected_hoisted_const,
            => Common.invariant("local lookup reached Monotype without a current local binding"),
            .local_proc => |local| .{ .fn_def = try self.fnTemplateForLocalProcWithMono(local, checked_ty, self.view.types.rootKey(checked_ty), ty) },
            .top_level_proc,
            .imported_proc,
            .hosted_proc,
            .promoted_top_level_proc,
            => |proc| return try self.lowerProcedureUseValue(proc, ty),
            .platform_required_proc => |proc| return try self.lowerProcedureUseValue(proc.procedure, ty),
            .top_level_const,
            .imported_const,
            .platform_required_const,
            => unreachable,
            .platform_required_declaration => Common.invariant("platform required declaration reached Monotype without a binding"),
        };
        return try self.builder.program.addExpr(.{ .ty = ty, .data = data });
    }

    fn lowerProcedureUseValue(
        self: *BodyContext,
        proc: checked.ProcedureUseTemplate,
        mono_fn_ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        const source_fn_ty = proc.source_fn_ty_payload orelse
            Common.invariant("checked procedure value reached Monotype without a requested function type");
        try self.constrainTypeToMono(source_fn_ty, mono_fn_ty);

        return switch (proc.binding) {
            .top_level => |top_level| blk: {
                const view = self.builder.moduleForId(checked.topLevelProcedureModuleId(top_level));
                const binding = view.top_level_procedure_bindings.get(top_level.binding);
                break :blk try self.builder.lowerProcedureBindingValue(view, binding, mono_fn_ty);
            },
            .imported => |imported| blk: {
                const view = self.builder.moduleForId(checked.importedProcedureModuleId(imported));
                for (view.exported_procedure_bindings.bindings) |binding| {
                    if (binding.binding.def == imported.def and binding.binding.pattern == imported.pattern) {
                        break :blk try self.builder.lowerProcedureBindingValue(view, .{
                            .source_scheme = binding.source_scheme,
                            .body = switch (binding.body) {
                                .direct_template => |direct| .{ .direct_template = direct },
                                .callable_eval_template => |template_id| .{ .callable_eval_template = template_id },
                            },
                        }, mono_fn_ty);
                    }
                }
                Common.invariant("imported procedure binding was not exported by its checked module");
            },
            .hosted => |hosted| blk: {
                const fn_template = self.builder.fnDefForTemplate(
                    self.builder.moduleForId(moduleIdFromDigest(names.procTemplateModuleDigest(hosted.template))),
                    hosted.template,
                    source_fn_ty,
                    proc.source_fn_ty_template,
                    mono_fn_ty,
                );
                const fn_id = try self.builder.lowerFnTemplateDefFromContext(self, fn_template);
                break :blk try self.builder.program.addExpr(.{ .ty = mono_fn_ty, .data = .{ .fn_def = fn_id } });
            },
            .platform_required => |required| blk: {
                const view = self.builder.moduleForId(checked.requiredProcedureModuleId(required));
                const binding = view.top_level_procedure_bindings.get(required.procedure_binding);
                break :blk try self.builder.lowerProcedureBindingValue(view, binding, mono_fn_ty);
            },
        };
    }

    fn constUseMonoType(self: *BodyContext, const_use: checked.ConstUseTemplate) Allocator.Error!Type.TypeId {
        const requested_ty = const_use.requested_source_ty_payload orelse
            Common.invariant("checked const use reached Monotype without a requested checked type");
        return try self.lowerType(requested_ty);
    }

    fn restoreConstUseAtType(
        self: *BodyContext,
        const_use: checked.ConstUseTemplate,
        ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        const requested_ty = const_use.requested_source_ty_payload orelse
            Common.invariant("checked const use reached Monotype without a requested checked type");
        try self.constrainTypeToMono(requested_ty, ty);

        const store_view = self.builder.moduleForId(checked.constModuleId(const_use.const_ref));
        const template = store_view.const_templates.get(const_use.const_ref);
        return switch (template.state) {
            .stored_const => |stored| try self.restoreConstNodeAtType(store_view, self.view, stored.node, ty),
            .reserved => Common.invariant("reserved checked const template reached Monotype"),
            .eval_template => |eval| try self.lowerConstEvalTemplateUse(store_view, eval, ty, null),
        };
    }

    fn lowerConstEvalTemplateUse(
        self: *BodyContext,
        store_view: ModuleView,
        eval: checked.ConstEvalTemplate,
        ty: Type.TypeId,
        source_region_override: ?base.Region,
    ) Allocator.Error!Ast.ExprId {
        const body = store_view.checked_const_bodies.get(eval.body);
        const entry_template = store_view.templates.get(eval.entry_template.template);

        const wrapper_args = try self.builder.program.types.addSpan(&.{});
        const wrapper_fn_ty = try self.builder.program.types.add(.{ .func = .{
            .args = wrapper_args,
            .ret = ty,
        } });
        const wrapper_template = self.builder.fnDefForTemplate(
            store_view,
            eval.entry_template,
            entry_template.checked_fn_root,
            store_view.types.rootKey(entry_template.checked_fn_root),
            wrapper_fn_ty,
        );

        const graph = try InstGraph.create(self.allocator, &self.builder.program.types, &self.builder.program.names, &self.builder.unsolved_monos);
        defer graph.destroy();
        const saved_graph = self.builder.active_graph;
        self.builder.active_graph = graph;
        defer self.builder.active_graph = saved_graph;
        var body_ctx = try BodyContext.init(self.allocator, self.builder, store_view, eval.entry_template, graph);
        defer body_ctx.deinit();
        body_ctx.source_region_override = source_region_override;
        const root_fn_key = Ast.fnTemplateDigest(wrapper_template, &self.builder.program.types, &self.builder.program.names);
        body_ctx.owner_context_fn_key = root_fn_key;
        body_ctx.current_fn_key = root_fn_key;
        try body_ctx.constrainTypeToMono(entry_template.checked_fn_root, wrapper_fn_ty);
        try body_ctx.constrainTypeToMono(body.checked_type, ty);

        const result_node = try body_ctx.instNode(body.checked_type);
        if (!self.builder.unsolved_monos.contains(ty)) {
            try graph.addMonoView(result_node, ty);
        }
        const live_ty = try graph.monoFor(result_node);
        const lowered = try body_ctx.lowerComptimeRootExprAtType(body.body_expr, live_ty);
        try self.builder.drainSpecRequests(graph);
        return lowered;
    }

    fn restoreConstNode(
        self: *BodyContext,
        store_view: ModuleView,
        type_view: ModuleView,
        node: checked.ConstNodeId,
        checked_ty: checked.CheckedTypeId,
    ) Allocator.Error!Ast.ExprId {
        return try self.restoreConstNodeAtType(store_view, type_view, node, try self.builder.lowerType(type_view, checked_ty));
    }

    fn restoreConstNodeAtType(
        self: *BodyContext,
        store_view: ModuleView,
        type_view: ModuleView,
        node: checked.ConstNodeId,
        ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        const address = ConstExprAddress{
            .store_module_bytes = store_view.key.bytes,
            .type_module_bytes = type_view.key.bytes,
            .node = @intFromEnum(node),
            .mono_ty = @intFromEnum(ty),
        };
        if (self.builder.const_expr_cache.get(address)) |existing| return existing;

        const value = store_view.const_store.get(node);
        switch (value) {
            .fn_value => |fn_id| {
                const expr = try self.restoreConstFn(store_view, type_view, fn_id, ty);
                try self.builder.const_expr_cache.put(address, expr);
                return expr;
            },
            else => {},
        }
        const data = try self.restoreConstData(store_view, type_view, value, ty);
        const expr = try self.builder.program.addExpr(.{ .ty = ty, .data = data });
        try self.builder.const_expr_cache.put(address, expr);
        return expr;
    }

    fn restoreConstData(
        self: *BodyContext,
        store_view: ModuleView,
        type_view: ModuleView,
        value: checked.ConstValue,
        ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprData {
        return switch (value) {
            .pending => Common.invariant("pending ConstStore node reached Monotype restore"),
            .zst => .unit,
            .scalar => |scalar| restoreScalar(scalar),
            .str => |str| .{ .str_lit = try self.builder.program.addStringView(
                store_view.const_store.strData(str.data),
                str.offset,
                str.len,
            ) },
            .crash => |str| .{ .crash = try self.builder.program.addStringView(
                store_view.const_store.strData(str.data),
                str.offset,
                str.len,
            ) },
            .list => |items| .{ .list = try self.restoreConstList(store_view, type_view, ty, items) },
            .box => |payload| blk: {
                const child = try self.restoreConstNodeAtType(store_view, type_view, payload, self.constBoxPayloadType(ty));
                break :blk .{ .low_level = .{
                    .op = .box_box,
                    .args = try self.builder.program.addExprSpan(&.{child}),
                } };
            },
            .tuple => |items| .{ .tuple = try self.restoreConstTuple(store_view, type_view, ty, items) },
            .record => |items| .{ .record = try self.restoreConstRecord(store_view, type_view, ty, items) },
            .tag => |tag| .{ .tag = .{
                .name = try self.builder.program.names.internTagLabel(tag.tag_name),
                .payloads = try self.restoreConstTagPayloads(store_view, type_view, ty, tag),
            } },
            .nominal => |nominal| .{ .nominal = try self.restoreConstNodeAtType(store_view, type_view, nominal.backing, self.builder.namedBackingType(ty) orelse ty) },
            .fn_value => Common.invariant("ConstStore function value must be restored as an expression"),
        };
    }

    fn restoreConstList(
        self: *BodyContext,
        store_view: ModuleView,
        type_view: ModuleView,
        ty: Type.TypeId,
        items: []const checked.ConstNodeId,
    ) Allocator.Error!Ast.Span(Ast.ExprId) {
        const elem_ty = self.constListElemType(ty);
        const lowered = try self.allocator.alloc(Ast.ExprId, items.len);
        defer self.allocator.free(lowered);
        for (items, 0..) |item, index| {
            lowered[index] = try self.restoreConstNodeAtType(store_view, type_view, item, elem_ty);
        }
        return try self.builder.program.addExprSpan(lowered);
    }

    fn restoreConstTuple(
        self: *BodyContext,
        store_view: ModuleView,
        type_view: ModuleView,
        ty: Type.TypeId,
        items: []const checked.ConstNodeId,
    ) Allocator.Error!Ast.Span(Ast.ExprId) {
        const item_span = self.builder.tupleItemSpan(ty);
        const item_count: usize = @intCast(item_span.len);
        if (item_count != items.len) Common.invariant("ConstStore tuple length differs from checked type");
        const lowered = try self.allocator.alloc(Ast.ExprId, items.len);
        defer self.allocator.free(lowered);
        for (items, 0..) |item, index| {
            const item_ty = self.builder.program.types.span(item_span)[index];
            lowered[index] = try self.restoreConstNodeAtType(store_view, type_view, item, item_ty);
        }
        return try self.builder.program.addExprSpan(lowered);
    }

    fn restoreConstRecord(
        self: *BodyContext,
        store_view: ModuleView,
        type_view: ModuleView,
        ty: Type.TypeId,
        items: []const checked.ConstNodeId,
    ) Allocator.Error!Ast.Span(Ast.FieldExpr) {
        const field_span = self.builder.recordFieldsSpan(ty);
        const field_count: usize = @intCast(field_span.len);
        if (field_count != items.len) Common.invariant("ConstStore record length differs from checked type");
        const lowered = try self.allocator.alloc(Ast.FieldExpr, items.len);
        defer self.allocator.free(lowered);
        for (items, 0..) |item, index| {
            const field = self.builder.program.types.fieldSpan(field_span)[index];
            lowered[index] = .{
                .name = field.name,
                .value = try self.restoreConstNodeAtType(store_view, type_view, item, field.ty),
            };
        }
        return try self.builder.program.addFieldExprSpan(lowered);
    }

    fn restoreConstTagPayloads(
        self: *BodyContext,
        store_view: ModuleView,
        type_view: ModuleView,
        ty: Type.TypeId,
        tag: anytype,
    ) Allocator.Error!Ast.Span(Ast.ExprId) {
        const mono_tag_name = try self.builder.program.names.internTagLabel(tag.tag_name);
        const payload_span = self.builder.tagPayloadSpan(ty, mono_tag_name);
        const payload_count: usize = @intCast(payload_span.len);
        if (payload_count != tag.payloads.len) Common.invariant("ConstStore tag payload count differs from checked type");
        const lowered = try self.allocator.alloc(Ast.ExprId, tag.payloads.len);
        defer self.allocator.free(lowered);
        for (tag.payloads, 0..) |payload, index| {
            const payload_ty = self.builder.program.types.span(payload_span)[index];
            lowered[index] = try self.restoreConstNodeAtType(store_view, type_view, payload, payload_ty);
        }
        return try self.builder.program.addExprSpan(lowered);
    }

    fn constListElemType(self: *BodyContext, ty: Type.TypeId) Type.TypeId {
        return switch (self.builder.shapeContent(ty)) {
            .list => |elem| elem,
            else => Common.invariant("ConstStore list restored with a non-list monotype"),
        };
    }

    fn constBoxPayloadType(self: *BodyContext, ty: Type.TypeId) Type.TypeId {
        return switch (self.builder.shapeContent(ty)) {
            .box => |payload| payload,
            else => Common.invariant("ConstStore box restored with a non-box monotype"),
        };
    }

    fn constRecordFields(self: *BodyContext, ty: Type.TypeId) []const Type.Field {
        return self.builder.program.types.fieldSpan(self.builder.recordFieldsSpan(ty));
    }

    fn restoreConstFn(
        self: *BodyContext,
        store_view: ModuleView,
        type_view: ModuleView,
        fn_id: checked.ConstFnId,
        ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        return self.builder.restoreConstFnExpr(store_view, type_view, fn_id, ty);
    }

    fn lowerExprSpan(self: *BodyContext, checked_exprs: []const checked.CheckedExprId) Allocator.Error!Ast.Span(Ast.ExprId) {
        const lowered = try self.allocator.alloc(Ast.ExprId, checked_exprs.len);
        defer self.allocator.free(lowered);
        for (checked_exprs, 0..) |child, i| {
            lowered[i] = try self.lowerExpr(child);
        }
        return try self.builder.program.addExprSpan(lowered);
    }

    fn lowerListExpr(self: *BodyContext, checked_exprs: []const checked.CheckedExprId, ty: Type.TypeId) Allocator.Error!Ast.Span(Ast.ExprId) {
        const elem_ty = switch (self.builder.shapeContent(ty)) {
            .list => |elem| elem,
            else => Common.invariant("list expression had a non-list monotype"),
        };
        const lowered = try self.allocator.alloc(Ast.ExprId, checked_exprs.len);
        defer self.allocator.free(lowered);
        for (checked_exprs, 0..) |child, i| {
            lowered[i] = try self.lowerExprAtType(child, elem_ty);
        }
        return try self.builder.program.addExprSpan(lowered);
    }

    fn lowerExprSpanAtTypes(
        self: *BodyContext,
        checked_exprs: []const checked.CheckedExprId,
        tys: []const Type.TypeId,
    ) Allocator.Error!Ast.Span(Ast.ExprId) {
        if (checked_exprs.len != tys.len) Common.invariant("call argument arity differs from concrete function type");
        const stable_tys = try self.allocator.dupe(Type.TypeId, tys);
        defer self.allocator.free(stable_tys);
        const lowered = try self.allocator.alloc(Ast.ExprId, checked_exprs.len);
        defer self.allocator.free(lowered);
        for (checked_exprs, stable_tys, 0..) |child, ty, i| {
            lowered[i] = try self.lowerExprAtType(child, ty);
        }
        return try self.builder.program.addExprSpan(lowered);
    }

    const PreLoweredOperand = struct {
        index: usize,
        expr: Ast.ExprId,
    };

    fn lowerDispatchOperandsAtTypes(
        self: *BodyContext,
        operands: []const static_dispatch.StaticDispatchOperand,
        tys: []const Type.TypeId,
        pre_lowered: ?PreLoweredOperand,
    ) Allocator.Error!Ast.Span(Ast.ExprId) {
        if (operands.len != tys.len) Common.invariant("dispatch argument arity differs from concrete function type");
        const stable_tys = try self.allocator.dupe(Type.TypeId, tys);
        defer self.allocator.free(stable_tys);
        const lowered = try self.allocator.alloc(Ast.ExprId, operands.len);
        defer self.allocator.free(lowered);
        for (operands, stable_tys, 0..) |operand, ty, i| {
            if (pre_lowered) |pre| {
                if (pre.index == i) {
                    lowered[i] = pre.expr;
                    continue;
                }
            }
            lowered[i] = try self.lowerDispatchOperandAtType(operand, ty);
        }
        return try self.builder.program.addExprSpan(lowered);
    }

    fn lowerDispatchOperandAtType(
        self: *BodyContext,
        operand: static_dispatch.StaticDispatchOperand,
        ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        return switch (operand) {
            .checked_expr => |expr| try self.lowerExprAtType(expr, ty),
            .generated_interpolation_iter => |expr| try self.lowerGeneratedInterpolationIter(expr, ty),
            .generated_numeral => |literal| try self.lowerNumeralValue(literal, ty),
            .generated_quote => |literal| try self.lowerQuoteValue(literal, ty),
        };
    }

    fn lowerGeneratedInterpolationIter(
        self: *BodyContext,
        expr_id: checked.CheckedExprId,
        ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        const expr = self.view.bodies.expr(expr_id);
        const interpolation = switch (expr.data) {
            .interpolation => |interpolation| interpolation,
            else => Common.invariant("generated interpolation iterator operand pointed at non-interpolation expression"),
        };

        const backing_ty = self.builder.namedBackingType(ty) orelse
            Common.invariant("generated interpolation iterator expected Iter nominal type");
        const len_field = self.recordFieldByText(backing_ty, "len_if_known");
        const step_field = self.recordFieldByText(backing_ty, "step");
        const step_fn_ty = step_field.ty;
        const step_shape = self.builder.functionShape(step_fn_ty, "generated interpolation iterator step field was not a function");
        if (self.builder.program.types.span(step_shape.args).len != 0) {
            Common.invariant("generated interpolation iterator step function was not zero-argument");
        }

        return try self.lowerInterpolationIterFromIndex(
            interpolation,
            0,
            expr_id,
            ty,
            backing_ty,
            len_field.ty,
            step_fn_ty,
            step_shape.ret,
        );
    }

    fn lowerInterpolationIterFromIndex(
        self: *BodyContext,
        interpolation: checked.CheckedInterpolation,
        index: usize,
        source_expr_id: checked.CheckedExprId,
        iter_ty: Type.TypeId,
        backing_ty: Type.TypeId,
        len_ty: Type.TypeId,
        step_fn_ty: Type.TypeId,
        step_ret_ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        const remaining = interpolation.parts.len - index;
        const len_expr = try self.lowerInterpolationLenIfKnown(remaining, len_ty);

        if (index == interpolation.parts.len) {
            const step_expr = try self.lowerInterpolationDoneStep(interpolation.step_fn_ty, source_expr_id, index, step_fn_ty, step_ret_ty);
            return try self.lowerInterpolationIterRecord(iter_ty, backing_ty, len_expr, step_expr);
        }

        const rest_expr = try self.lowerInterpolationIterFromIndex(
            interpolation,
            index + 1,
            source_expr_id,
            iter_ty,
            backing_ty,
            len_ty,
            step_fn_ty,
            step_ret_ty,
        );
        const rest_local = try self.builder.program.addLocal(self.builder.symbols.fresh(), iter_ty);
        const rest_pat = try self.builder.bindPat(rest_local, iter_ty);
        const rest_ref = try self.builder.localExpr(rest_local, iter_ty);
        const step_expr = try self.lowerInterpolationOneStep(
            interpolation,
            index,
            source_expr_id,
            rest_ref,
            iter_ty,
            interpolation.step_fn_ty,
            step_fn_ty,
            step_ret_ty,
        );
        const iter_expr = try self.lowerInterpolationIterRecord(iter_ty, backing_ty, len_expr, step_expr);
        return try self.builder.program.addExpr(.{ .ty = iter_ty, .data = .{ .let_ = .{
            .bind = rest_pat,
            .value = rest_expr,
            .rest = iter_expr,
        } } });
    }

    fn lowerInterpolationIterRecord(
        self: *BodyContext,
        iter_ty: Type.TypeId,
        backing_ty: Type.TypeId,
        len_expr: Ast.ExprId,
        step_expr: Ast.ExprId,
    ) Allocator.Error!Ast.ExprId {
        const fields = self.builder.program.types.fieldSpan(self.builder.recordFieldsSpan(backing_ty));
        const lowered = try self.allocator.alloc(Ast.FieldExpr, fields.len);
        defer self.allocator.free(lowered);

        for (fields, 0..) |field, i| {
            const label = self.builder.program.names.recordFieldLabelText(field.name);
            lowered[i] = .{
                .name = field.name,
                .value = if (Ident.textEql(label, "len_if_known"))
                    len_expr
                else if (Ident.textEql(label, "step"))
                    step_expr
                else
                    Common.invariant("Iter backing record contained an unexpected field"),
            };
        }

        const record_expr = try self.builder.program.addExpr(.{
            .ty = backing_ty,
            .data = .{ .record = try self.builder.program.addFieldExprSpan(lowered) },
        });
        return try self.builder.program.addExpr(.{
            .ty = iter_ty,
            .data = .{ .nominal = record_expr },
        });
    }

    fn lowerInterpolationLenIfKnown(
        self: *BodyContext,
        remaining: usize,
        ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        const known_tag = self.monoTagByText(ty, "Known");
        const payloads = self.builder.program.types.span(known_tag.payloads);
        if (payloads.len != 1) Common.invariant("Iter.len_if_known Known tag did not have one payload");
        const count = try self.builder.intLiteralExpr(@intCast(remaining), payloads[0]);
        return try self.builder.program.addExpr(.{ .ty = ty, .data = .{ .tag = .{
            .name = known_tag.name,
            .payloads = try self.builder.program.addExprSpan(&[_]Ast.ExprId{count}),
        } } });
    }

    fn lowerInterpolationDoneStep(
        self: *BodyContext,
        source_fn_ty: checked.CheckedTypeId,
        source_expr_id: checked.CheckedExprId,
        index: usize,
        step_fn_ty: Type.TypeId,
        step_ret_ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        const done_tag = self.monoTagByText(step_ret_ty, "Done");
        const body = try self.builder.program.addExpr(.{ .ty = step_ret_ty, .data = .{ .tag = .{
            .name = done_tag.name,
            .payloads = .empty(),
        } } });
        return try self.lowerInterpolationStepLambda(source_fn_ty, source_expr_id, index, step_fn_ty, body);
    }

    fn lowerInterpolationOneStep(
        self: *BodyContext,
        interpolation: checked.CheckedInterpolation,
        index: usize,
        source_expr_id: checked.CheckedExprId,
        rest_expr: Ast.ExprId,
        rest_ty: Type.TypeId,
        source_fn_ty: checked.CheckedTypeId,
        step_fn_ty: Type.TypeId,
        step_ret_ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        const item_ty = self.iterItemType(rest_ty);
        const item_fields = self.builder.tupleItemTypes(item_ty);
        if (item_fields.len != 2) Common.invariant("generated interpolation iterator item was not a pair");

        const part = interpolation.parts[index];
        const value_expr = try self.lowerExprAtType(part.value, item_fields[0]);
        const segment_expr = try self.lowerExprAtType(part.following_segment, item_fields[1]);
        const item_expr = try self.builder.program.addExpr(.{ .ty = item_ty, .data = .{
            .tuple = try self.builder.program.addExprSpan(&[_]Ast.ExprId{ value_expr, segment_expr }),
        } });

        const one_tag = self.monoTagByText(step_ret_ty, "One");
        const payloads = self.builder.program.types.span(one_tag.payloads);
        if (payloads.len != 1) Common.invariant("Iter step One tag did not have one record payload");
        const payload_ty = payloads[0];
        const payload_expr = try self.lowerInterpolationOnePayload(payload_ty, item_expr, rest_expr);

        const body = try self.builder.program.addExpr(.{ .ty = step_ret_ty, .data = .{ .tag = .{
            .name = one_tag.name,
            .payloads = try self.builder.program.addExprSpan(&[_]Ast.ExprId{payload_expr}),
        } } });
        return try self.lowerInterpolationStepLambda(source_fn_ty, source_expr_id, index, step_fn_ty, body);
    }

    fn lowerInterpolationOnePayload(
        self: *BodyContext,
        ty: Type.TypeId,
        item_expr: Ast.ExprId,
        rest_expr: Ast.ExprId,
    ) Allocator.Error!Ast.ExprId {
        const fields = self.builder.program.types.fieldSpan(self.builder.recordFieldsSpan(ty));
        const lowered = try self.allocator.alloc(Ast.FieldExpr, fields.len);
        defer self.allocator.free(lowered);

        for (fields, 0..) |field, i| {
            const label = self.builder.program.names.recordFieldLabelText(field.name);
            lowered[i] = .{
                .name = field.name,
                .value = if (Ident.textEql(label, "item"))
                    item_expr
                else if (Ident.textEql(label, "rest"))
                    rest_expr
                else
                    Common.invariant("Iter step One payload contained an unexpected field"),
            };
        }

        return try self.builder.program.addExpr(.{
            .ty = ty,
            .data = .{ .record = try self.builder.program.addFieldExprSpan(lowered) },
        });
    }

    fn lowerInterpolationStepLambda(
        self: *BodyContext,
        source_fn_ty: checked.CheckedTypeId,
        source_expr_id: checked.CheckedExprId,
        index: usize,
        step_fn_ty: Type.TypeId,
        body: Ast.ExprId,
    ) Allocator.Error!Ast.ExprId {
        const fn_id = try self.builder.program.addFn(.{
            .fn_def = .{ .checked_generated = self.owner_template },
            .source_fn_ty = source_fn_ty,
            .source_fn_key = generatedInterpolationStepKey(self.current_fn_key, source_expr_id, index),
            .mono_fn_ty = step_fn_ty,
        });
        return try self.builder.program.addExpr(.{ .ty = step_fn_ty, .data = .{ .lambda = .{
            .fn_id = fn_id,
            .args = try self.builder.program.addTypedLocalSpan(&.{}),
            .body = body,
        } } });
    }

    fn recordFieldByText(self: *BodyContext, ty: Type.TypeId, text: []const u8) Type.Field {
        return self.recordFieldByTextOptional(ty, text) orelse
            Common.invariant("expected record field was absent from monotype type");
    }

    fn recordFieldByTextOptional(self: *BodyContext, ty: Type.TypeId, text: []const u8) ?Type.Field {
        const fields = switch (self.builder.shapeContent(ty)) {
            .record => |span| self.builder.program.types.fieldSpan(span),
            .zst => return null,
            else => return null,
        };
        for (fields) |field| {
            if (Ident.textEql(self.builder.program.names.recordFieldLabelText(field.name), text)) return field;
        }
        return null;
    }

    fn iterItemType(self: *BodyContext, ty: Type.TypeId) Type.TypeId {
        return switch (self.builder.program.types.get(ty)) {
            .named => |named| blk: {
                const args = self.builder.program.types.span(named.args);
                if (args.len != 1) Common.invariant("Iter nominal did not have one type argument");
                break :blk args[0];
            },
            else => Common.invariant("generated interpolation iterator expected named Iter type"),
        };
    }

    fn lowerExprAtType(
        self: *BodyContext,
        checked_expr: checked.CheckedExprId,
        ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        const expr = self.view.bodies.expr(checked_expr);
        const saved_loc = self.builder.program.current_loc;
        defer self.builder.program.current_loc = saved_loc;
        const saved_region = self.builder.program.current_region;
        defer self.builder.program.current_region = saved_region;
        const region = self.sourceRegionForExpr(expr);
        self.builder.program.current_loc = try self.sourceLocFor(region);
        self.builder.program.current_region = region;
        if (try self.restoredHoistedExprAtType(checked_expr, ty)) |restored| return restored;
        switch (expr.data) {
            .call => |call| {
                if (try self.lowerParseIntrinsicCallExpr(checked_expr, expr.ty, call, ty)) |lowered| return lowered;
                try self.constrainKnownType(expr.ty, ty);
                const lowered = try self.lowerCall(expr.ty, call);
                if (!self.sameType(ty, lowered.ret_ty)) {
                    Common.invariant("checked call expression lowered at a type different from its context type");
                }
                return try self.builder.program.addExpr(.{
                    .ty = ty,
                    .data = lowered.data,
                });
            },
            .dispatch_call => |plan| return try self.lowerDispatchExprAtType(expr.ty, plan, ty),
            .interpolation => |interpolation| return try self.lowerDispatchExprAtType(expr.ty, interpolation.plan, ty),
            .type_dispatch_call => |plan| return try self.lowerDispatchExprAtType(expr.ty, plan, ty),
            .method_eq => |plan| return try self.lowerDispatchExprAtType(expr.ty, plan, ty),
            .lookup_local => |lookup| return try self.lowerLookupExprAtType(expr.ty, lookup.resolved, ty),
            .lookup_external => |resolved| return try self.lowerLookupExprAtType(expr.ty, resolved, ty),
            .lookup_required => |resolved| return try self.lowerLookupExprAtType(expr.ty, resolved, ty),
            .structural_eq => |eq| {
                try self.constrainKnownType(expr.ty, ty);
                const lowered = try self.lowerDirectStructuralEqAtType(eq, ty);
                if (!self.sameType(ty, self.builder.program.exprs.items[@intFromEnum(lowered)].ty)) {
                    Common.invariant("checked structural equality lowered at a type different from its context type");
                }
                return lowered;
            },
            .structural_hash => |h| {
                try self.constrainKnownType(expr.ty, ty);
                const lowered = try self.lowerDirectStructuralHashAtType(h, ty);
                if (!self.sameType(ty, self.builder.program.exprs.items[@intFromEnum(lowered)].ty)) {
                    Common.invariant("checked structural hash lowered at a type different from its context type");
                }
                return lowered;
            },
            .lambda,
            .closure,
            => {
                const lowered = try self.lowerExprWithType(checked_expr, ty);
                if (!self.sameType(ty, self.builder.program.exprs.items[@intFromEnum(lowered)].ty)) {
                    Common.invariant("checked function expression lowered at a type different from its context type");
                }
                return lowered;
            },
            else => {},
        }
        try self.constrainKnownType(expr.ty, ty);
        const lowered = try self.lowerExprWithType(checked_expr, ty);
        if (!self.sameType(ty, self.builder.program.exprs.items[@intFromEnum(lowered)].ty)) {
            Common.invariant("checked expression lowered at a type different from its call operand type");
        }
        return lowered;
    }

    fn sameType(self: *BodyContext, expected: Type.TypeId, actual: Type.TypeId) bool {
        return self.sameTypeInner(expected, actual, 0);
    }

    fn sameTypeInner(self: *BodyContext, expected: Type.TypeId, actual: Type.TypeId, depth: u8) bool {
        if (expected == actual) return true;
        if (depth == 32) return false;
        if (monoAliasBacking(&self.builder.program.types, expected)) |backing| {
            if (self.sameTypeInner(backing, actual, depth + 1)) return true;
        }
        if (monoAliasBacking(&self.builder.program.types, actual)) |backing| {
            if (self.sameTypeInner(expected, backing, depth + 1)) return true;
        }
        const expected_digest = self.builder.program.types.typeDigest(&self.builder.program.names, expected);
        const actual_digest = self.builder.program.types.typeDigest(&self.builder.program.names, actual);
        if (std.mem.eql(u8, expected_digest.bytes[0..], actual_digest.bytes[0..])) return true;

        return self.sameTypeContent(expected, actual, depth + 1);
    }

    fn sameTypeContent(self: *BodyContext, expected: Type.TypeId, actual: Type.TypeId, depth: u8) bool {
        const expected_content = self.builder.program.types.get(expected);
        const actual_content = self.builder.program.types.get(actual);
        return switch (expected_content) {
            .primitive => |primitive| switch (actual_content) {
                .primitive => |actual_primitive| primitive == actual_primitive,
                else => false,
            },
            .named => |named| switch (actual_content) {
                .named => |actual_named| self.sameNamedType(named, actual_named, depth),
                else => false,
            },
            .record => |fields| switch (actual_content) {
                .record => |actual_fields| self.sameRecordFieldNames(fields, actual_fields, depth),
                else => false,
            },
            .tuple => |items| switch (actual_content) {
                .tuple => |actual_items| self.sameTypeSpans(items, actual_items, depth),
                else => false,
            },
            .tag_union => |tags| switch (actual_content) {
                .tag_union => |actual_tags| self.sameTags(tags, actual_tags, depth),
                else => false,
            },
            .list => |elem| switch (actual_content) {
                .list => |actual_elem| self.sameTypeInner(elem, actual_elem, depth),
                else => false,
            },
            .box => |elem| switch (actual_content) {
                .box => |actual_elem| self.sameTypeInner(elem, actual_elem, depth),
                else => false,
            },
            .func => |function| switch (actual_content) {
                .func => |actual_function| self.sameTypeSpans(function.args, actual_function.args, depth) and
                    self.sameTypeInner(function.ret, actual_function.ret, depth),
                else => false,
            },
            .erased => |erased| switch (actual_content) {
                .erased => |actual_erased| std.mem.eql(u8, erased.bytes[0..], actual_erased.bytes[0..]),
                else => false,
            },
            .zst => switch (actual_content) {
                .zst => true,
                else => false,
            },
        };
    }

    fn sameNamedType(self: *BodyContext, expected: anytype, actual: anytype, depth: u8) bool {
        if (!std.mem.eql(u8, expected.named_type.module.bytes[0..], actual.named_type.module.bytes[0..])) return false;
        if (expected.def.module_name != actual.def.module_name) return false;
        if (expected.def.source_decl != actual.def.source_decl) return false;
        if (expected.def.source_decl == null and expected.def.type_name != actual.def.type_name) return false;
        if (expected.kind != actual.kind) return false;
        if (expected.builtin_owner != actual.builtin_owner) return false;
        return self.sameTypeSpans(expected.args, actual.args, depth);
    }

    fn sameTypeSpans(self: *BodyContext, expected: Type.Span, actual: Type.Span, depth: u8) bool {
        const expected_items = self.builder.program.types.span(expected);
        const actual_items = self.builder.program.types.span(actual);
        if (expected_items.len != actual_items.len) return false;
        for (expected_items, actual_items) |expected_item, actual_item| {
            if (!self.sameTypeInner(expected_item, actual_item, depth)) return false;
        }
        return true;
    }

    fn sameRecordFieldNames(self: *BodyContext, expected: Type.Span, actual: Type.Span, depth: u8) bool {
        const expected_fields = self.builder.program.types.fieldSpan(expected);
        const actual_fields = self.builder.program.types.fieldSpan(actual);
        if (expected_fields.len != actual_fields.len) return false;
        for (expected_fields, actual_fields) |expected_field, actual_field| {
            if (expected_field.name != actual_field.name) return false;
            if (!self.sameTypeInner(expected_field.ty, actual_field.ty, depth)) return false;
        }
        return true;
    }

    fn sameTags(self: *BodyContext, expected: Type.Span, actual: Type.Span, depth: u8) bool {
        const expected_tags = self.builder.program.types.tagSpan(expected);
        const actual_tags = self.builder.program.types.tagSpan(actual);
        if (expected_tags.len != actual_tags.len) return false;
        for (expected_tags, actual_tags) |expected_tag, actual_tag| {
            if (expected_tag.name != actual_tag.name) return false;
            if (!self.sameTypeSpans(expected_tag.payloads, actual_tag.payloads, depth)) return false;
        }
        return true;
    }

    fn lowerRecordExpr(self: *BodyContext, record: anytype, ty: Type.TypeId) Allocator.Error!Ast.ExprId {
        const target_fields = switch (self.builder.shapeContent(ty)) {
            .record => |fields| fields,
            else => Common.invariant("record expression had a non-record monotype"),
        };
        const target_field_count: usize = @intCast(target_fields.len);
        const lowered = try self.allocator.alloc(Ast.FieldExpr, target_field_count);
        defer self.allocator.free(lowered);
        const base_record = if (record.ext) |ext| try self.lowerExpr(ext) else null;
        const base_ty = if (base_record) |base_expr| self.builder.program.exprs.items[@intFromEnum(base_expr)].ty else ty;
        const base_local = if (base_record) |_| try self.builder.program.addLocal(self.builder.symbols.fresh(), base_ty) else null;
        const base_expr = if (base_local) |local| try self.builder.localExpr(local, base_ty) else null;

        for (0..target_field_count) |i| {
            const field = self.builder.program.types.fieldSpan(target_fields)[i];
            const value = if (try self.recordUpdateFieldValue(record.fields, field.name)) |field_value|
                try self.lowerExprAtType(field_value, field.ty)
            else if (base_expr) |base_value|
                try self.builder.program.addExpr(.{
                    .ty = field.ty,
                    .data = .{ .field_access = .{
                        .receiver = base_value,
                        .field = field.name,
                    } },
                })
            else
                Common.invariant("closed record literal was missing a checked field value");
            lowered[i] = .{
                .name = field.name,
                .value = value,
            };
        }
        const record_expr = try self.builder.program.addExpr(.{ .ty = ty, .data = .{ .record = try self.builder.program.addFieldExprSpan(lowered) } });
        if (base_record) |base_value| {
            return try self.builder.program.addExpr(.{ .ty = ty, .data = .{ .let_ = .{
                .bind = try self.builder.bindPat(base_local orelse Common.invariant("record update lowered base without a base local"), base_ty),
                .value = base_value,
                .rest = record_expr,
            } } });
        }
        return record_expr;
    }

    fn recordUpdateFieldValue(
        self: *BodyContext,
        fields: []const checked.CheckedRecordExprField,
        name: names.RecordFieldNameId,
    ) Allocator.Error!?checked.CheckedExprId {
        for (fields) |field| {
            const lowered_name = try self.builder.recordFieldName(self.view, field.label);
            if (self.builder.program.names.recordFieldLabelTextEql(lowered_name, name)) return field.value;
        }
        return null;
    }

    fn lowerClosure(self: *BodyContext, expr_id: checked.CheckedExprId, closure: anytype, closure_ty: Type.TypeId) Allocator.Error!Ast.ExprData {
        const lambda = self.view.bodies.expr(closure.lambda);
        return switch (lambda.data) {
            .lambda => blk: {
                const nested = try self.builder.fnTemplateForNestedExprWithMono(self.view, self.owner_template, expr_id, lambda.ty, self.view.types.rootKey(lambda.ty), closure_ty, self.current_fn_key);
                break :blk try self.lowerLambdaExpr(expr_id, nested);
            },
            else => Common.invariant("checked closure did not point at a lambda expression"),
        };
    }

    fn closureFunctionType(self: *BodyContext, closure: anytype) Allocator.Error!Type.TypeId {
        const lambda = self.view.bodies.expr(closure.lambda);
        return switch (lambda.data) {
            .lambda => |lambda_data| try self.lambdaFunctionType(lambda_data),
            else => Common.invariant("checked closure did not point at a lambda expression"),
        };
    }

    fn lambdaFunctionType(self: *BodyContext, lambda: anytype) Allocator.Error!Type.TypeId {
        const args = try self.allocator.alloc(Type.TypeId, lambda.args.len);
        defer self.allocator.free(args);
        var saved = std.ArrayList(BinderRestore).empty;
        defer saved.deinit(self.allocator);

        for (lambda.args, 0..) |pattern_id, i| {
            args[i] = try self.lowerType(self.view.bodies.pattern(pattern_id).ty);
            try self.savePatternBinders(pattern_id, &saved);
            try self.preRegisterPatternBinders(pattern_id, args[i]);
        }
        defer self.restoreBinders(saved.items);

        return try self.builder.program.types.add(.{ .func = .{
            .args = try self.builder.program.types.addSpan(args),
            .ret = try self.lowerExprType(lambda.body),
        } });
    }

    fn lowerLambdaExpr(
        self: *BodyContext,
        expr_id: checked.CheckedExprId,
        nested: Ast.FnTemplate,
    ) Allocator.Error!Ast.ExprData {
        switch (nested.fn_def) {
            .nested => {},
            else => Common.invariant("expression-position lambda was not assigned a nested function identity"),
        }
        return .{ .fn_def = try self.builder.lowerNestedFnFromContext(self, expr_id, nested) };
    }

    fn lowerDispatchExpr(
        self: *BodyContext,
        checked_ret_ty: checked.CheckedTypeId,
        maybe_plan: ?static_dispatch.StaticDispatchPlanId,
    ) Allocator.Error!Ast.ExprId {
        return try self.lowerDispatchExprAtType(checked_ret_ty, maybe_plan, null);
    }

    fn lowerDispatchExprAtType(
        self: *BodyContext,
        checked_ret_ty: checked.CheckedTypeId,
        maybe_plan: ?static_dispatch.StaticDispatchPlanId,
        expected_ret_ty: ?Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        const plan_id = maybe_plan orelse Common.invariant("checked dispatch expression reached Monotype without a dispatch plan");
        const plan = self.view.static_dispatch_plans.plans[@intFromEnum(plan_id)];
        const plan_args = plan.argsSlice(self.view.static_dispatch_plans);

        var call_ctx = try BodyContext.init(self.allocator, self.builder, self.view, self.owner_template, self.graph);
        defer call_ctx.deinit();
        call_ctx.owner_context_fn_key = self.owner_context_fn_key;
        call_ctx.current_fn_key = self.current_fn_key;

        const callable_mono_ty = try call_ctx.instantiateDispatchPlanCallTypeFromCaller(plan.callable_ty, self, checked_ret_ty, plan_args, expected_ret_ty);
        const plan_fn_data = self.builder.functionShape(callable_mono_ty, "checked dispatch plan had a non-function type");
        const plan_arg_tys = try self.allocator.dupe(Type.TypeId, self.builder.program.types.span(plan_fn_data.args));
        defer self.allocator.free(plan_arg_tys);
        const plan_ret_ty = plan_fn_data.ret;

        const dispatcher_ty = try self.dispatcherMonoType(plan, plan_arg_tys);
        // A dispatcher slot fed only by another method dispatch stays
        // unresolved until that operand's target resolves. Lowering the
        // dispatcher operand first supplies the receiver's solved type; the
        // lowered expression is reused as the call argument.
        var pre_lowered: ?PreLoweredOperand = null;
        if (methodOwnerFromType(&self.builder.program.types, dispatcher_ty) == null) {
            switch (plan.dispatcher) {
                .arg => |index| {
                    pre_lowered = .{
                        .index = index,
                        .expr = try self.lowerDispatchOperandAtType(plan_args[index], plan_arg_tys[index]),
                    };
                },
                .type_only => {},
            }
        }
        const lookup = self.dispatchTarget(plan, dispatcher_ty);
        if (lookup == null) {
            return switch (plan.result_mode) {
                // `.equality` and `.hash` are both handled by lowerStructuralEquality,
                // which switches on result_mode internally.
                .equality, .hash => try self.lowerStructuralEquality(plan, callable_mono_ty, plan_ret_ty, self, pre_lowered),
                .parser_for => try self.lowerStructuralParser(plan, callable_mono_ty, plan_ret_ty, self, pre_lowered),
                .encode_to => try self.lowerStructuralEncodeTo(plan, callable_mono_ty, plan_ret_ty, self, pre_lowered),
                .value => Common.invariant("value dispatch plan had no resolved dispatch target"),
            };
        }
        const resolved = lookup.?;
        const target_mono_ty = try self.methodTargetMonoTypeFromPlan(resolved, &call_ctx, plan.callable_ty, expected_ret_ty);
        try call_ctx.constrainTypeToMono(plan.callable_ty, target_mono_ty);
        if (!self.sameType(callable_mono_ty, target_mono_ty)) {
            Common.invariant("checked dispatch target callable type differed from dispatch plan callable type");
        }
        const fn_data = self.builder.functionShape(target_mono_ty, "checked dispatch target had a non-function type");
        try self.constrainTypeToMono(checked_ret_ty, fn_data.ret);
        if (expected_ret_ty) |expected| {
            if (!self.sameType(expected, fn_data.ret)) Common.invariant("checked dispatch expression lowered at a type different from its call operand type");
        }
        const call_expr = try self.builder.program.addExpr(.{
            .ty = fn_data.ret,
            .data = try self.lowerResolvedDispatch(plan, resolved, target_mono_ty, self, pre_lowered),
        });
        return try self.applyDispatchResultMode(plan.result_mode, call_expr, fn_data.ret);
    }

    fn applyDispatchResultMode(
        self: *BodyContext,
        mode: static_dispatch.StaticDispatchResultMode,
        expr: Ast.ExprId,
        expr_ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        return switch (mode) {
            .value => expr,
            .parser_for => expr,
            .encode_to => expr,
            .equality => |eq| if (eq.negated) blk: {
                if (!self.typeHasBuiltinOwner(expr_ty, .bool)) Common.invariant("checked equality dispatch returned a non-Bool value");
                break :blk try self.builder.lowLevelExpr(.bool_not, &.{expr}, expr_ty);
            } else expr,
            // A resolved `to_hash` dispatch returns its Hasher result directly.
            .hash => expr,
        };
    }

    fn lowerNumeralCall(
        self: *BodyContext,
        checked_ret_ty: checked.CheckedTypeId,
        maybe_plan: ?static_dispatch.StaticDispatchPlanId,
        target_ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        const result = try self.lowerNumeralCallRaw(checked_ret_ty, maybe_plan, target_ty);
        return try self.unwrapNumeralResult(result.call, result.try_ty, target_ty);
    }

    const NumeralCall = struct {
        call: Ast.ExprId,
        try_ty: Type.TypeId,
    };

    fn lowerNumeralCallRaw(
        self: *BodyContext,
        checked_ret_ty: checked.CheckedTypeId,
        maybe_plan: ?static_dispatch.StaticDispatchPlanId,
        target_ty: Type.TypeId,
    ) Allocator.Error!NumeralCall {
        const plan_id = maybe_plan orelse Common.invariant("checked from_numeral expression reached Monotype without a dispatch plan");
        const plan = self.view.static_dispatch_plans.plans[@intFromEnum(plan_id)];
        if (plan.result_mode != .value) Common.invariant("checked from_numeral plan had a non-value result mode");
        const plan_args = plan.argsSlice(self.view.static_dispatch_plans);

        var call_ctx = try BodyContext.init(self.allocator, self.builder, self.view, self.owner_template, self.graph);
        defer call_ctx.deinit();
        call_ctx.owner_context_fn_key = self.owner_context_fn_key;
        call_ctx.current_fn_key = self.current_fn_key;

        const callable_mono_ty = try call_ctx.instantiateNumeralPlanCallType(plan.callable_ty, self, checked_ret_ty, target_ty, plan_args);
        const plan_fn_data = self.builder.functionShape(callable_mono_ty, "checked from_numeral plan had a non-function type");
        const plan_arg_tys = try self.allocator.dupe(Type.TypeId, self.builder.program.types.span(plan_fn_data.args));
        defer self.allocator.free(plan_arg_tys);
        const try_ty = plan_fn_data.ret;

        const dispatcher_ty = try self.dispatcherMonoType(plan, plan_arg_tys);
        const resolved = self.dispatchTarget(plan, dispatcher_ty) orelse
            Common.invariant("checked from_numeral dispatch unexpectedly resolved to structural equality");

        const target_mono_ty = try self.methodTargetMonoTypeFromPlan(resolved, &call_ctx, plan.callable_ty, try_ty);
        try call_ctx.constrainTypeToMono(plan.callable_ty, target_mono_ty);
        if (!self.sameType(callable_mono_ty, target_mono_ty)) {
            Common.invariant("checked from_numeral target callable type differed from dispatch plan callable type");
        }

        const fn_data = self.builder.functionShape(target_mono_ty, "checked from_numeral target had a non-function type");
        const call_expr = try self.builder.program.addExpr(.{
            .ty = fn_data.ret,
            .data = try self.lowerResolvedDispatch(plan, resolved, target_mono_ty, self, null),
        });
        return .{ .call = call_expr, .try_ty = fn_data.ret };
    }

    fn lowerNumeralRootBody(
        self: *BodyContext,
        expr_id: checked.CheckedExprId,
        try_ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        const expr = self.view.bodies.expr(expr_id);
        const plan = switch (expr.data) {
            .num_from_numeral, .typed_num_from_numeral => |plan| plan,
            .str_from_quote => |quote| quote.plan,
            else => Common.invariant("literal conversion root did not point at a conversion expression"),
        };
        const ok_tag = self.monoTagByText(try_ty, "Ok");
        const ok_payloads = self.builder.program.types.span(ok_tag.payloads);
        if (ok_payloads.len != 1) Common.invariant("numeral conversion root Try.Ok did not carry one payload");
        const result = try self.lowerNumeralCallRaw(expr.ty, plan, ok_payloads[0]);
        if (!self.sameType(result.try_ty, try_ty)) {
            Common.invariant("numeral conversion root type differed from the from_numeral result type");
        }
        return result.call;
    }

    fn restoredNumeralConst(
        self: *BodyContext,
        expr_id: checked.CheckedExprId,
        ty: Type.TypeId,
    ) Allocator.Error!?Ast.ExprId {
        const root = self.view.compile_time_roots.lookupNumeralRootByExpr(expr_id) orelse return null;
        return switch (root.payload) {
            .const_node => |node| try self.restoreConstNodeAtType(self.view, self.view, node, ty),
            .pending => null,
            else => Common.invariant("numeral conversion root stored a non-constant payload"),
        };
    }

    /// Fold a `from_numeral` conversion into a constant at its concrete target
    /// type. Compile-time finalization only registers a constant for literals
    /// whose type is already concrete at Check time; a literal inside a generic
    /// body (e.g. the `1` in `Iter.exclusive_range`'s `start.add_checked(1)`) is
    /// typed by the abstract `num` var and only gains a concrete type here, after
    /// monomorphization. Rather than emit a runtime `num_from_numeral` low-level
    /// op — which the compiled backends deliberately reject — we evaluate the
    /// conversion at the stage that owns monomorphic types, reusing the exact
    /// decimal-text + parse behavior the finalization/interpreter path uses so
    /// the constant is identical regardless of where the literal is monomorphized.
    fn lowerNumeralFold(
        self: *BodyContext,
        checked_ret_ty: checked.CheckedTypeId,
        maybe_plan: ?static_dispatch.StaticDispatchPlanId,
        target_ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        // Only the builtin numeric types implement `from_numeral` as the
        // `num_from_numeral` low-level op that compiled backends reject. A custom
        // numeric type carries a user-defined `from_numeral` body that lowers to
        // an ordinary call the backends handle, so it must keep going through the
        // real dispatch rather than being folded.
        const primitive = switch (self.builder.shapeContent(target_ty)) {
            .primitive => |p| p,
            else => return try self.lowerNumeralCall(checked_ret_ty, maybe_plan, target_ty),
        };

        const plan_id = maybe_plan orelse Common.invariant("checked from_numeral expression reached Monotype without a dispatch plan");
        const plan = self.view.static_dispatch_plans.plans[@intFromEnum(plan_id)];
        const plan_args = plan.argsSlice(self.view.static_dispatch_plans);
        if (plan_args.len != 1) Common.invariant("from_numeral plan did not carry exactly one operand");
        const literal = switch (plan_args[0]) {
            .generated_numeral => |lit| lit,
            else => Common.invariant("from_numeral plan operand was not a generated numeral"),
        };

        const text = try checked.numeralLiteralDecimalText(self.allocator, self.view.module_env, literal);
        defer self.allocator.free(text);

        const folded = foldNumeralPrimitive(primitive, text);

        // A value that does not fit its concrete target (e.g. a literal forced to
        // a too-narrow type after monomorphization) matches the existing generic
        // from_numeral behavior: a runtime crash on the conversion's Err branch.
        const data = folded orelse blk: {
            const msg = try self.builder.program.addStringLiteral("invalid numeric literal");
            break :blk Ast.ExprData{ .crash = msg };
        };
        return try self.builder.program.addExpr(.{ .ty = target_ty, .data = data });
    }

    /// Materialize a string literal as the `Str` argument of a `from_quote`
    /// dispatch call.
    fn lowerQuoteValue(
        self: *BodyContext,
        literal: checked.CheckedStringLiteralId,
        ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        return try self.builder.program.addExpr(.{
            .ty = ty,
            .data = .{ .str_lit = try self.lowerStringLiteral(literal) },
        });
    }

    fn lowerNumeralValue(
        self: *BodyContext,
        literal: can.ModuleEnv.NumeralLiteral,
        ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        const backing_ty = self.builder.namedBackingType(ty) orelse
            Common.invariant("checked from_numeral argument was not the Numeral nominal type");
        const literal_tag = self.monoTagByText(backing_ty, "Literal");
        const payloads = self.builder.program.types.span(literal_tag.payloads);
        if (payloads.len != 1) Common.invariant("Numeral Literal tag must have one record payload");

        const record_expr = try self.lowerNumeralRecord(literal, payloads[0]);
        const backing_expr = try self.builder.program.addExpr(.{
            .ty = backing_ty,
            .data = .{ .tag = .{
                .name = literal_tag.name,
                .payloads = try self.builder.program.addExprSpan(&[_]Ast.ExprId{record_expr}),
            } },
        });
        return try self.builder.program.addExpr(.{
            .ty = ty,
            .data = .{ .nominal = backing_expr },
        });
    }

    fn lowerNumeralRecord(
        self: *BodyContext,
        literal: can.ModuleEnv.NumeralLiteral,
        ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        const field_span = switch (self.builder.shapeContent(ty)) {
            .record => |span| span,
            else => Common.invariant("Numeral Literal payload was not a record"),
        };
        const field_count: usize = @intCast(field_span.len);
        const lowered = try self.allocator.alloc(Ast.FieldExpr, field_count);
        defer self.allocator.free(lowered);

        const before = self.view.module_env.numeralDigitsBefore(literal);
        const after = self.view.module_env.numeralDigitsAfter(literal);
        for (0..field_count) |i| {
            const field = self.builder.program.types.fieldSpan(field_span)[i];
            const label = self.builder.program.names.recordFieldLabelText(field.name);
            const value = if (Ident.textEql(label, "is_negative"))
                try self.boolLiteral(literal.isNegative(), field.ty)
            else if (Ident.textEql(label, "digits_before_pt"))
                try self.lowerU8List(before, field.ty)
            else if (Ident.textEql(label, "digits_after_pt"))
                try self.lowerU8List(after, field.ty)
            else if (Ident.textEql(label, "digits_after_pt_count"))
                try self.builder.intLiteralExpr(literal.after_decimal_digit_count, field.ty)
            else
                Common.invariant("Numeral record contained an unexpected field");
            lowered[i] = .{ .name = field.name, .value = value };
        }

        return try self.builder.program.addExpr(.{
            .ty = ty,
            .data = .{ .record = try self.builder.program.addFieldExprSpan(lowered) },
        });
    }

    fn lowerU8List(
        self: *BodyContext,
        values: []const u8,
        ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        const elem_ty = switch (self.builder.shapeContent(ty)) {
            .list => |elem| elem,
            else => Common.invariant("Numeral digit field was not a List(U8)"),
        };
        const items = try self.allocator.alloc(Ast.ExprId, values.len);
        defer self.allocator.free(items);
        for (values, 0..) |value, i| {
            items[i] = try self.builder.intLiteralExpr(value, elem_ty);
        }
        return try self.builder.program.addExpr(.{
            .ty = ty,
            .data = .{ .list = try self.builder.program.addExprSpan(items) },
        });
    }

    fn unwrapNumeralResult(
        self: *BodyContext,
        result: Ast.ExprId,
        try_ty: Type.TypeId,
        target_ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        const ok_tag = self.monoTagByText(try_ty, "Ok");
        const err_tag = self.monoTagByText(try_ty, "Err");
        const ok_payloads = self.builder.program.types.span(ok_tag.payloads);
        const err_payloads = self.builder.program.types.span(err_tag.payloads);
        if (ok_payloads.len != 1) Common.invariant("Try.Ok from from_numeral did not carry one payload");
        if (!self.sameType(ok_payloads[0], target_ty)) {
            Common.invariant("Try.Ok from from_numeral carried a type different from the literal target type");
        }

        const ok_local = try self.builder.program.addLocal(self.builder.symbols.fresh(), target_ty);
        const ok_payload_pat = try self.builder.bindPat(ok_local, target_ty);
        const ok_pat = try self.builder.program.addPat(.{ .ty = try_ty, .data = .{ .tag = .{
            .name = ok_tag.name,
            .payloads = try self.builder.program.addPatSpan(&[_]Ast.PatId{ok_payload_pat}),
        } } });
        const ok_body = try self.builder.localExpr(ok_local, target_ty);

        const err_payload_pats = try self.allocator.alloc(Ast.PatId, err_payloads.len);
        defer self.allocator.free(err_payload_pats);
        for (err_payloads, 0..) |payload_ty, i| {
            err_payload_pats[i] = try self.builder.program.addPat(.{ .ty = payload_ty, .data = .wildcard });
        }
        const err_pat = try self.builder.program.addPat(.{ .ty = try_ty, .data = .{ .tag = .{
            .name = err_tag.name,
            .payloads = try self.builder.program.addPatSpan(err_payload_pats),
        } } });
        const msg = try self.builder.program.addStringLiteral("invalid numeric literal");
        const err_body = try self.builder.program.addExpr(.{ .ty = target_ty, .data = .{ .crash = msg } });

        const branches = [_]Ast.Branch{
            .{ .pat = ok_pat, .body = ok_body },
            .{ .pat = err_pat, .body = err_body },
        };
        return try self.builder.program.addExpr(.{ .ty = target_ty, .data = .{ .match_ = .{
            .scrutinee = result,
            .branches = try self.builder.program.addBranchSpan(&branches),
        } } });
    }

    fn monoTagByText(self: *BodyContext, ty: Type.TypeId, text: []const u8) Type.Tag {
        return self.monoTagByTextOptional(ty, text) orelse Common.invariant("expected tag was absent from monotype tag union");
    }

    fn monoTagByTextOptional(self: *BodyContext, ty: Type.TypeId, text: []const u8) ?Type.Tag {
        return switch (self.builder.shapeContent(ty)) {
            .tag_union => |span| {
                for (self.builder.program.types.tagSpan(span)) |tag| {
                    if (Ident.textEql(self.builder.program.names.tagLabelText(tag.name), text)) return tag;
                }
                return null;
            },
            else => null,
        };
    }

    fn typeHasBuiltinOwner(self: *BodyContext, ty: Type.TypeId, owner: static_dispatch.BuiltinOwner) bool {
        return switch (methodOwnerFromType(&self.builder.program.types, ty) orelse return false) {
            .builtin => |actual| actual == owner,
            .source_decl => false,
            .nominal => false,
        };
    }

    fn dispatchResultMonoType(
        self: *BodyContext,
        checked_ret_ty: checked.CheckedTypeId,
        maybe_plan: ?static_dispatch.StaticDispatchPlanId,
        expected_ret_ty: ?Type.TypeId,
    ) Allocator.Error!?Type.TypeId {
        const plan_id = maybe_plan orelse Common.invariant("checked dispatch expression reached Monotype without a dispatch plan");
        const plan = self.view.static_dispatch_plans.plans[@intFromEnum(plan_id)];

        var call_ctx = try BodyContext.init(self.allocator, self.builder, self.view, self.owner_template, self.graph);
        defer call_ctx.deinit();
        call_ctx.owner_context_fn_key = self.owner_context_fn_key;
        call_ctx.current_fn_key = self.current_fn_key;

        const plan_args = plan.argsSlice(self.view.static_dispatch_plans);
        const callable_mono_ty = try call_ctx.instantiateDispatchPlanCallTypeFromCaller(plan.callable_ty, self, checked_ret_ty, plan_args, expected_ret_ty);
        const plan_fn_data = self.builder.functionShape(callable_mono_ty, "checked dispatch plan had a non-function type");
        const plan_arg_tys = try self.allocator.dupe(Type.TypeId, self.builder.program.types.span(plan_fn_data.args));
        defer self.allocator.free(plan_arg_tys);
        const plan_ret_ty = plan_fn_data.ret;
        const dispatcher_ty = try self.dispatcherMonoType(plan, plan_arg_tys);

        // The dispatcher may not be solved yet when this runs for argument
        // evidence; the target then resolves when the expression itself
        // lowers, and the plan's return carries the evidence for now.
        if (methodOwnerFromType(&self.builder.program.types, dispatcher_ty) == null) {
            try self.constrainTypeToMono(checked_ret_ty, plan_ret_ty);
            return plan_ret_ty;
        }
        const resolved = self.dispatchTarget(plan, dispatcher_ty) orelse {
            try self.constrainTypeToMono(checked_ret_ty, plan_ret_ty);
            return plan_ret_ty;
        };
        const target_mono_ty = try self.methodTargetMonoTypeFromPlan(resolved, &call_ctx, plan.callable_ty, null);
        try call_ctx.constrainTypeToMono(plan.callable_ty, target_mono_ty);
        if (!self.sameType(callable_mono_ty, target_mono_ty)) {
            Common.invariant("checked dispatch target callable type differed from dispatch plan callable type");
        }
        const ret_ty = self.functionReturnType(target_mono_ty);
        try self.constrainTypeToMono(checked_ret_ty, ret_ty);
        return ret_ty;
    }

    fn dispatcherMonoType(
        self: *BodyContext,
        plan: static_dispatch.StaticDispatchCallPlan,
        arg_tys: []const Type.TypeId,
    ) Allocator.Error!Type.TypeId {
        return switch (plan.dispatcher) {
            .arg => |index| blk: {
                if (index >= arg_tys.len) Common.invariant("dispatch plan dispatcher argument index was outside the argument span");
                break :blk arg_tys[index];
            },
            .type_only => try self.lowerType(plan.dispatcher_ty),
        };
    }

    fn dispatchTarget(
        self: *BodyContext,
        plan: static_dispatch.StaticDispatchCallPlan,
        dispatcher_ty: Type.TypeId,
    ) ?MethodLookup {
        switch (plan.resolution) {
            .resolved_target => |target| return self.methodLookupForResolvedTarget(target),
            .unresolved_checked_plan => {},
        }

        const owner = methodOwnerFromType(&self.builder.program.types, dispatcher_ty) orelse {
            if (planAllowsStructural(plan.result_mode)) return null;
            Common.invariant("dispatch plan had no method owner and no structural equality permission");
        };

        const lookup = self.builder.lookupMethodTarget(owner, self.view, plan.method) orelse {
            if (planAllowsStructural(plan.result_mode)) return null;
            Common.invariant("checked method registry is missing resolved dispatch target");
        };
        return lookup;
    }

    /// Whether a dispatch plan permits structural decomposition (equality or
    /// hashing) instead of resolving to a concrete method target.
    fn planAllowsStructural(mode: static_dispatch.StaticDispatchResultMode) bool {
        return switch (mode) {
            .equality => |eq| eq.structural_allowed,
            .hash => |hash| hash.structural_allowed,
            .parser_for => |parser_for| parser_for.structural_allowed,
            .encode_to => |encode_to| encode_to.structural_allowed,
            .value => false,
        };
    }

    fn methodLookupForResolvedTarget(
        self: *BodyContext,
        target: static_dispatch.MethodTarget,
    ) MethodLookup {
        return switch (target.kind) {
            .procedure => |procedure| .{
                .view = self.builder.moduleForDigest(names.procTemplateModuleDigest(procedure.template)),
                .target = target,
            },
            .local_proc => .{
                .view = self.view,
                .target = target,
            },
        };
    }

    fn methodTargetContext(
        self: *BodyContext,
        lookup: MethodLookup,
    ) Allocator.Error!BodyContext {
        const owner_template = switch (lookup.target.kind) {
            .procedure => |procedure| procedure.template,
            .local_proc => blk: {
                self.requireLocalMethodTargetInCurrentView(lookup);
                break :blk self.owner_template;
            },
        };
        return BodyContext.init(self.allocator, self.builder, lookup.view, owner_template, self.graph);
    }

    fn requireLocalMethodTargetInCurrentView(self: *BodyContext, lookup: MethodLookup) void {
        if (!moduleBytesEqual(lookup.view.key.bytes, self.view.key.bytes)) {
            Common.invariant("local method dispatch target belonged to a different checked module view");
        }
    }

    fn methodTargetMonoTypeFromPlan(
        self: *BodyContext,
        lookup: MethodLookup,
        plan_ctx: *BodyContext,
        plan_callable_ty: checked.CheckedTypeId,
        expected_ret_ty: ?Type.TypeId,
    ) Allocator.Error!Type.TypeId {
        var target_ctx = try self.methodTargetContext(lookup);
        defer target_ctx.deinit();
        return try target_ctx.instantiateTargetFromPlan(lookup.target.callable_ty, plan_ctx, plan_callable_ty, expected_ret_ty);
    }

    fn methodTargetMonoTypeFromArgs(
        self: *BodyContext,
        lookup: MethodLookup,
        arg_tys: []const Type.TypeId,
        ret_ty: Type.TypeId,
    ) Allocator.Error!Type.TypeId {
        var target_ctx = try self.methodTargetContext(lookup);
        defer target_ctx.deinit();
        return try target_ctx.instantiateTargetCallTypeFromMonoArgs(lookup.target.callable_ty, arg_tys, ret_ty);
    }

    fn methodTargetMonoTypeFromArgsPreservingArgs(
        self: *BodyContext,
        lookup: MethodLookup,
        arg_tys: []const Type.TypeId,
        ret_ty: Type.TypeId,
    ) Allocator.Error!Type.TypeId {
        var target_ctx = try self.methodTargetContext(lookup);
        defer target_ctx.deinit();
        return try target_ctx.instantiateTargetCallTypeFromMonoArgsPreservingArgs(lookup.target.callable_ty, arg_tys, ret_ty);
    }

    fn methodTargetMonoTypeFromArgAtIndexAndRet(
        self: *BodyContext,
        lookup: MethodLookup,
        arg_index: usize,
        arg_ty: Type.TypeId,
        ret_ty: Type.TypeId,
    ) Allocator.Error!Type.TypeId {
        var target_ctx = try self.methodTargetContext(lookup);
        defer target_ctx.deinit();
        return try target_ctx.instantiateTargetCallTypeFromMonoArgAtIndexAndRet(lookup.target.callable_ty, arg_index, arg_ty, ret_ty);
    }

    fn methodTargetMonoTypeFromArgAtIndexIsolated(
        self: *BodyContext,
        lookup: MethodLookup,
        arg_index: usize,
        arg_ty: Type.TypeId,
    ) Allocator.Error!Type.TypeId {
        var graph = try InstGraph.create(self.allocator, &self.builder.program.types, &self.builder.program.names, &self.builder.unsolved_monos);
        defer graph.destroy();

        const owner_template = switch (lookup.target.kind) {
            .procedure => |procedure| procedure.template,
            .local_proc => blk: {
                self.requireLocalMethodTargetInCurrentView(lookup);
                break :blk self.owner_template;
            },
        };
        var target_ctx = try BodyContext.init(self.allocator, self.builder, lookup.view, owner_template, graph);
        defer target_ctx.deinit();
        return try target_ctx.instantiateTargetCallTypeFromMonoArgAtIndex(lookup.target.callable_ty, arg_index, arg_ty);
    }

    fn methodLookupForTypeName(
        self: *BodyContext,
        owner_ty: Type.TypeId,
        method_name: []const u8,
    ) MethodLookup {
        const owner = methodOwnerFromType(&self.builder.program.types, owner_ty) orelse
            Common.invariant("parser format type did not have a method owner");
        return self.builder.lookupMethodTargetByName(owner, method_name) orelse
            Common.invariant("checked method registry is missing parser format method");
    }

    fn methodTargetCalleeWithMono(
        self: *BodyContext,
        lookup: MethodLookup,
        callable_mono_ty: Type.TypeId,
    ) Allocator.Error!Ast.FnId {
        const source_fn_ty = lookup.target.callable_ty;
        const source_fn_key = lookup.view.types.rootKey(source_fn_ty);
        return switch (lookup.target.kind) {
            .procedure => |procedure| blk: {
                const fn_template = self.builder.fnDefForTemplate(
                    lookup.view,
                    procedure.template,
                    source_fn_ty,
                    source_fn_key,
                    callable_mono_ty,
                );
                break :blk try self.builder.lowerFnTemplateDefFromContext(self, fn_template);
            },
            .local_proc => |local| blk: {
                self.requireLocalMethodTargetInCurrentView(lookup);
                break :blk try self.fnTemplateForLocalProcWithMono(
                    .{ .binder = local.binder, .expr = local.expr },
                    source_fn_ty,
                    source_fn_key,
                    callable_mono_ty,
                );
            },
        };
    }

    fn lowerResolvedDispatch(
        self: *BodyContext,
        plan: static_dispatch.StaticDispatchCallPlan,
        lookup: MethodLookup,
        callable_mono_ty: Type.TypeId,
        arg_ctx: *BodyContext,
        pre_lowered: ?PreLoweredOperand,
    ) Allocator.Error!Ast.ExprData {
        const fn_data = self.builder.functionShape(callable_mono_ty, "checked dispatch target had a non-function type");
        const args = try arg_ctx.lowerDispatchOperandsAtTypes(plan.argsSlice(self.view.static_dispatch_plans), self.builder.program.types.span(fn_data.args), pre_lowered);
        return .{ .call_proc = .{
            .callee = .{ .func = try self.methodTargetCalleeWithMono(lookup, callable_mono_ty) },
            .args = args,
        } };
    }

    fn lowerStructuralEquality(
        self: *BodyContext,
        plan: static_dispatch.StaticDispatchCallPlan,
        callable_mono_ty: Type.TypeId,
        ret_ty: Type.TypeId,
        arg_ctx: *BodyContext,
        pre_lowered: ?PreLoweredOperand,
    ) Allocator.Error!Ast.ExprId {
        return switch (plan.result_mode) {
            .equality => |eq| if (eq.structural_allowed) blk: {
                var operands = try self.lowerStructuralBinaryOperands("equality", plan, callable_mono_ty, arg_ctx, pre_lowered);
                defer operands.deinit(self.allocator);
                var result = try self.lowerEqualityExpr(operands.arg_tys[0], operands.first, operands.second, self.view.names.methodNameText(plan.method), ret_ty);
                if (eq.negated) {
                    result = try self.builder.lowLevelExpr(.bool_not, &.{result}, ret_ty);
                }
                break :blk result;
            } else Common.invariant("structural equality dispatch plan did not permit structural equality"),
            .hash => |hash| if (hash.structural_allowed) blk: {
                var operands = try self.lowerStructuralBinaryOperands("hash", plan, callable_mono_ty, arg_ctx, pre_lowered);
                defer operands.deinit(self.allocator);
                break :blk try self.lowerHashExpr(operands.arg_tys[0], operands.first, operands.second, ret_ty);
            } else Common.invariant("structural hash dispatch plan did not permit structural hashing"),
            .value => Common.invariant("value dispatch plan reached structural equality lowering"),
            .parser_for => Common.invariant("parser_for dispatch plan reached structural equality lowering"),
            .encode_to => Common.invariant("encode_to dispatch plan reached structural equality lowering"),
        };
    }

    /// Operands of a structural binary derivation (equality / hash) once both
    /// have been lowered. `arg_tys` is owned by the caller and freed via
    /// `deinit`; the terminal lowering reads `arg_tys[0]` for the derived type.
    const StructuralBinaryOperands = struct {
        first: Ast.ExprId,
        second: Ast.ExprId,
        arg_tys: []Type.TypeId,

        fn deinit(self: *StructuralBinaryOperands, allocator: Allocator) void {
            allocator.free(self.arg_tys);
        }
    };

    /// Lower the two operands of a structural equality/hash dispatch, honoring a
    /// `pre_lowered` operand at index 0 or 1. `noun` is the derivation name used
    /// only in invariant diagnostics.
    fn lowerStructuralBinaryOperands(
        self: *BodyContext,
        comptime noun: []const u8,
        plan: static_dispatch.StaticDispatchCallPlan,
        callable_mono_ty: Type.TypeId,
        arg_ctx: *BodyContext,
        pre_lowered: ?PreLoweredOperand,
    ) Allocator.Error!StructuralBinaryOperands {
        const plan_args = plan.argsSlice(self.view.static_dispatch_plans);
        if (plan_args.len != 2) Common.invariant("structural " ++ noun ++ " dispatch plan must have two operands");
        const fn_data = self.builder.functionShape(callable_mono_ty, "checked structural " ++ noun ++ " target had a non-function type");
        const arg_tys = try self.allocator.dupe(Type.TypeId, self.builder.program.types.span(fn_data.args));
        errdefer self.allocator.free(arg_tys);
        if (arg_tys.len != 2) Common.invariant("structural " ++ noun ++ " callable type must have two operands");
        const first = if (pre_lowered != null and pre_lowered.?.index == 0)
            pre_lowered.?.expr
        else
            try arg_ctx.lowerDispatchOperandAtType(plan_args[0], arg_tys[0]);
        const second = if (pre_lowered != null and pre_lowered.?.index == 1)
            pre_lowered.?.expr
        else
            try arg_ctx.lowerDispatchOperandAtType(plan_args[1], arg_tys[1]);
        return .{ .first = first, .second = second, .arg_tys = arg_tys };
    }

    fn lowerStructuralParser(
        self: *BodyContext,
        plan: static_dispatch.StaticDispatchCallPlan,
        callable_mono_ty: Type.TypeId,
        ret_ty: Type.TypeId,
        arg_ctx: *BodyContext,
        pre_lowered: ?PreLoweredOperand,
    ) Allocator.Error!Ast.ExprId {
        const parser = switch (plan.result_mode) {
            .parser_for => |parser_for| parser_for,
            else => Common.invariant("non-parser_for dispatch plan reached structural parse lowering"),
        };
        if (!parser.structural_allowed) Common.invariant("structural parser dispatch plan did not permit structural parser lowering");

        const fn_data = self.builder.functionShape(callable_mono_ty, "checked structural parser target had a non-function type");
        const arg_tys = try self.allocator.dupe(Type.TypeId, self.builder.program.types.span(fn_data.args));
        defer self.allocator.free(arg_tys);
        const plan_args = plan.argsSlice(self.view.static_dispatch_plans);
        if (arg_tys.len != 1 or plan_args.len != 1) Common.invariant("structural parser callable type must have one encoding argument");
        if (!self.sameType(fn_data.ret, ret_ty)) Common.invariant("structural parser return type differed from dispatch expression type");

        const runtime_fn = self.builder.functionShape(ret_ty, "checked structural parser return had a non-function type");
        const runtime_arg_tys = try self.allocator.dupe(Type.TypeId, self.builder.program.types.span(runtime_fn.args));
        defer self.allocator.free(runtime_arg_tys);
        if (runtime_arg_tys.len != 1) Common.invariant("structural parser runtime function must have one state argument");

        const shape_ty = try self.lowerType(plan.dispatcher_ty);
        if (!self.typeHasBuiltinOwner(shape_ty, .str)) {
            switch (self.builder.shapeContent(shape_ty)) {
                .record => |fields_span| blk: {
                    for (self.builder.program.types.fieldSpan(fields_span)) |field| {
                        if (!self.parseFieldTypeIsSupported(field.ty)) Common.invariant("structural parser record field type was not supported");
                    }
                    break :blk;
                },
                .tag_union => |tags_span| blk: {
                    const tags = self.builder.program.types.tagSpan(tags_span);
                    if (tags.len == 0) Common.invariant("structural parser empty tag union reached postcheck lowering");
                    for (tags) |tag| {
                        for (self.builder.program.types.span(tag.payloads)) |payload_ty| {
                            if (!self.parseFieldTypeIsSupported(payload_ty)) Common.invariant("structural parser tag-union payload type was not supported");
                        }
                    }
                    break :blk;
                },
                .zst => {},
                else => Common.invariant("structural parser dispatcher was not a supported structural type"),
            }
        }

        const encoding_value = if (pre_lowered != null and pre_lowered.?.index == 0)
            pre_lowered.?.expr
        else
            try arg_ctx.lowerDispatchOperandAtType(plan_args[0], arg_tys[0]);
        const encoding_local = try self.builder.program.addLocal(self.builder.symbols.fresh(), arg_tys[0]);
        self.builder.program.setLocalCaptureId(encoding_local, parserEncodingCaptureId());
        const state_local = try self.builder.program.addLocal(self.builder.symbols.fresh(), runtime_arg_tys[0]);

        const str_ty = try self.builder.primitiveType(.str);
        var precomputed_plan = ParserPrecomputedPlan.init(self.allocator);
        defer precomputed_plan.deinit();
        try self.buildParserConstructionPrecomputedPlan(
            &precomputed_plan,
            shape_ty,
            try self.builder.localExpr(encoding_local, arg_tys[0]),
            arg_tys[0],
            str_ty,
        );

        const parsed = try self.lowerParseShapeFromState(
            shape_ty,
            try self.builder.localExpr(encoding_local, arg_tys[0]),
            arg_tys[0],
            try self.builder.localExpr(state_local, runtime_arg_tys[0]),
            runtime_arg_tys[0],
            runtime_fn.ret,
            &precomputed_plan,
        );
        const fn_id = try self.builder.program.addFn(.{
            .fn_def = .{ .parser_runtime = .{
                .owner = self.owner_template,
                .expr = plan.expr,
            } },
            .source_fn_ty = plan.callable_ty,
            .source_fn_key = generatedParserRuntimeKey(self.current_fn_key, plan.expr),
            .mono_fn_ty = ret_ty,
        });
        var parser_expr = try self.builder.program.addExpr(.{ .ty = ret_ty, .data = .{ .lambda = .{
            .fn_id = fn_id,
            .args = try self.builder.program.addTypedLocalSpan(&.{
                .{ .local = state_local, .ty = runtime_arg_tys[0] },
            }),
            .body = parsed,
        } } });
        var capture_index = precomputed_plan.captures.items.len;
        while (capture_index > 0) {
            capture_index -= 1;
            const capture = precomputed_plan.captures.items[capture_index];
            parser_expr = try self.wrapLet(capture.local, str_ty, capture.value, parser_expr, ret_ty);
        }
        return try self.wrapLet(encoding_local, arg_tys[0], encoding_value, parser_expr, ret_ty);
    }

    fn lowerStructuralEncodeTo(
        self: *BodyContext,
        plan: static_dispatch.StaticDispatchCallPlan,
        callable_mono_ty: Type.TypeId,
        ret_ty: Type.TypeId,
        arg_ctx: *BodyContext,
        pre_lowered: ?PreLoweredOperand,
    ) Allocator.Error!Ast.ExprId {
        const encode_to = switch (plan.result_mode) {
            .encode_to => |encode_to| encode_to,
            else => Common.invariant("non-encode_to dispatch plan reached structural encode_to lowering"),
        };
        if (!encode_to.structural_allowed) Common.invariant("structural encode_to dispatch plan did not permit structural encode_to lowering");

        const fn_data = self.builder.functionShape(callable_mono_ty, "checked structural encode_to target had a non-function type");
        const arg_tys = try self.allocator.dupe(Type.TypeId, self.builder.program.types.span(fn_data.args));
        defer self.allocator.free(arg_tys);
        const plan_args = plan.argsSlice(self.view.static_dispatch_plans);
        if (arg_tys.len != 2 or plan_args.len != 2) Common.invariant("structural encode_to callable type must have value and encoding arguments");
        if (!self.sameType(fn_data.ret, ret_ty)) Common.invariant("structural encode_to return type differed from dispatch expression type");

        const runtime_fn = self.builder.functionShape(ret_ty, "checked structural encode_to return had a non-function type");
        const runtime_arg_tys = try self.allocator.dupe(Type.TypeId, self.builder.program.types.span(runtime_fn.args));
        defer self.allocator.free(runtime_arg_tys);
        if (runtime_arg_tys.len != 1) Common.invariant("structural encode_to runtime function must have one state argument");

        const shape_ty = try self.lowerType(plan.dispatcher_ty);
        if (!self.encodeFieldTypeIsSupported(shape_ty)) Common.invariant("structural encode_to dispatcher was not a supported structural type");

        const value_expr = if (pre_lowered != null and pre_lowered.?.index == 0)
            pre_lowered.?.expr
        else
            try arg_ctx.lowerDispatchOperandAtType(plan_args[0], arg_tys[0]);
        const encoding_value = if (pre_lowered != null and pre_lowered.?.index == 1)
            pre_lowered.?.expr
        else
            try arg_ctx.lowerDispatchOperandAtType(plan_args[1], arg_tys[1]);

        const value_local = try self.builder.program.addLocal(self.builder.symbols.fresh(), arg_tys[0]);
        const encoding_local = try self.builder.program.addLocal(self.builder.symbols.fresh(), arg_tys[1]);
        const state_local = try self.builder.program.addLocal(self.builder.symbols.fresh(), runtime_arg_tys[0]);
        self.builder.program.setLocalCaptureId(value_local, encodeToValueCaptureId());
        self.builder.program.setLocalCaptureId(encoding_local, encodeToEncodingCaptureId());

        const str_ty = try self.builder.primitiveType(.str);
        var precomputed_plan = ParserPrecomputedPlan.init(self.allocator);
        defer precomputed_plan.deinit();
        precomputed_plan.next_capture_id = encodeToFirstFieldCaptureId();
        try self.buildEncodeConstructionPrecomputedPlan(
            &precomputed_plan,
            shape_ty,
            try self.builder.localExpr(encoding_local, arg_tys[1]),
            arg_tys[1],
            str_ty,
        );

        const encoded = try self.lowerEncodeShapeToState(
            shape_ty,
            try self.builder.localExpr(value_local, arg_tys[0]),
            try self.builder.localExpr(encoding_local, arg_tys[1]),
            arg_tys[1],
            try self.builder.localExpr(state_local, runtime_arg_tys[0]),
            runtime_arg_tys[0],
            runtime_fn.ret,
            &precomputed_plan,
        );
        const fn_id = try self.builder.program.addFn(.{
            .fn_def = .{ .encode_to_runtime = .{
                .owner = self.owner_template,
                .expr = plan.expr,
            } },
            .source_fn_ty = plan.callable_ty,
            .source_fn_key = generatedEncodeToRuntimeKey(self.current_fn_key, plan.expr),
            .mono_fn_ty = ret_ty,
        });
        var encoder_expr = try self.builder.program.addExpr(.{ .ty = ret_ty, .data = .{ .lambda = .{
            .fn_id = fn_id,
            .args = try self.builder.program.addTypedLocalSpan(&.{
                .{ .local = state_local, .ty = runtime_arg_tys[0] },
            }),
            .body = encoded,
        } } });
        var capture_index = precomputed_plan.captures.items.len;
        while (capture_index > 0) {
            capture_index -= 1;
            const capture = precomputed_plan.captures.items[capture_index];
            encoder_expr = try self.wrapLet(capture.local, str_ty, capture.value, encoder_expr, ret_ty);
        }
        encoder_expr = try self.wrapLet(encoding_local, arg_tys[1], encoding_value, encoder_expr, ret_ty);
        return try self.wrapLet(value_local, arg_tys[0], value_expr, encoder_expr, ret_ty);
    }

    fn lowerEncodeShapeToState(
        self: *BodyContext,
        shape_ty: Type.TypeId,
        value_expr: Ast.ExprId,
        encoding_expr: Ast.ExprId,
        encoding_ty: Type.TypeId,
        state_expr: Ast.ExprId,
        state_ty: Type.TypeId,
        ret_ty: Type.TypeId,
        precomputed_plan: ?*const ParserPrecomputedPlan,
    ) Allocator.Error!Ast.ExprId {
        if (self.customEncodeToLookup(shape_ty)) |lookup| {
            return try self.lowerCustomEncodeToState(lookup, shape_ty, value_expr, encoding_expr, encoding_ty, state_expr, state_ty, ret_ty);
        }
        if (self.typeHasBuiltinOwner(shape_ty, .str)) {
            return try self.lowerEncodeFormatMethod("encode_str", &.{ value_expr, state_expr }, &.{ shape_ty, state_ty }, encoding_ty, ret_ty);
        }
        if (self.typeHasBuiltinOwner(shape_ty, .u64)) {
            return try self.lowerEncodeFormatMethod("encode_u64", &.{ value_expr, state_expr }, &.{ shape_ty, state_ty }, encoding_ty, ret_ty);
        }
        return switch (self.builder.shapeContent(shape_ty)) {
            .record, .zst => try self.lowerEncodeRecordToState(shape_ty, value_expr, encoding_expr, encoding_ty, state_expr, state_ty, ret_ty, precomputed_plan),
            else => Common.invariant("encode_to selected an unsupported shape"),
        };
    }

    fn lowerEncodeRecordToState(
        self: *BodyContext,
        shape_ty: Type.TypeId,
        value_expr: Ast.ExprId,
        encoding_expr: Ast.ExprId,
        encoding_ty: Type.TypeId,
        state_expr: Ast.ExprId,
        state_ty: Type.TypeId,
        ret_ty: Type.TypeId,
        precomputed_plan: ?*const ParserPrecomputedPlan,
    ) Allocator.Error!Ast.ExprId {
        const ret_info = self.tryInfo(ret_ty);
        if (!self.sameType(ret_info.ok_ty, state_ty)) Common.invariant("encode_to record return Ok type differed from state type");

        const record_fields = try self.allocator.dupe(Type.Field, switch (self.builder.shapeContent(shape_ty)) {
            .record => |span| self.builder.program.types.fieldSpan(span),
            .zst => &.{},
            else => Common.invariant("encode_to record requested for a non-record shape"),
        });
        defer self.allocator.free(record_fields);

        const str_ty = try self.builder.primitiveType(.str);
        var owned_renamed_field_locals: ?[]Ast.LocalId = null;
        defer if (owned_renamed_field_locals) |locals| self.allocator.free(locals);
        var owned_renamed_field_values: ?[]Ast.ExprId = null;
        defer if (owned_renamed_field_values) |values| self.allocator.free(values);

        const precomputed = if (precomputed_plan) |plan| plan.records.get(shape_ty) else null;
        const renamed_field_locals = if (precomputed) |record| blk: {
            if (record.renamed_field_locals.len != record_fields.len) Common.invariant("encode_to precomputed renamed field arity differed from record field count");
            break :blk record.renamed_field_locals;
        } else blk: {
            const locals = try self.allocator.alloc(Ast.LocalId, record_fields.len);
            const values = try self.allocator.alloc(Ast.ExprId, record_fields.len);
            owned_renamed_field_locals = locals;
            owned_renamed_field_values = values;
            for (record_fields, 0..) |field, index| {
                locals[index] = try self.builder.program.addLocal(self.builder.symbols.fresh(), str_ty);
                values[index] = try self.renamedRecordFieldNameExpr(encoding_expr, encoding_ty, field, str_ty);
            }
            break :blk locals;
        };

        const begin_try = try self.lowerEncodeFormatMethod("begin_record", &.{state_expr}, &.{state_ty}, encoding_ty, ret_ty);
        const begin_state_local = try self.builder.program.addLocal(self.builder.symbols.fresh(), state_ty);
        const fields_body = try self.lowerEncodeRecordFieldNamesFrom(
            shape_ty,
            value_expr,
            encoding_expr,
            encoding_ty,
            try self.builder.localExpr(begin_state_local, state_ty),
            state_ty,
            ret_ty,
            precomputed_plan,
            record_fields,
            renamed_field_locals,
            0,
        );
        var body = try self.sequenceTry(begin_try, ret_ty, begin_state_local, fields_body, ret_ty);
        if (owned_renamed_field_values) |renamed_field_values| {
            var index = renamed_field_locals.len;
            while (index > 0) {
                index -= 1;
                body = try self.wrapLet(renamed_field_locals[index], str_ty, renamed_field_values[index], body, ret_ty);
            }
        }
        return body;
    }

    fn lowerEncodeRecordFieldNamesFrom(
        self: *BodyContext,
        shape_ty: Type.TypeId,
        value_expr: Ast.ExprId,
        encoding_expr: Ast.ExprId,
        encoding_ty: Type.TypeId,
        state_expr: Ast.ExprId,
        state_ty: Type.TypeId,
        ret_ty: Type.TypeId,
        precomputed_plan: ?*const ParserPrecomputedPlan,
        record_fields: []const Type.Field,
        renamed_field_locals: []const Ast.LocalId,
        field_index: usize,
    ) Allocator.Error!Ast.ExprId {
        if (field_index >= record_fields.len) {
            return try self.lowerEncodeFormatMethod("end_record", &.{state_expr}, &.{state_ty}, encoding_ty, ret_ty);
        }

        const field = record_fields[field_index];
        const str_ty = try self.builder.primitiveType(.str);
        const renamed_field_expr = try self.builder.localExpr(renamed_field_locals[field_index], str_ty);
        const field_name_try = try self.lowerEncodeFormatMethod("encode_record_field", &.{ renamed_field_expr, state_expr }, &.{ str_ty, state_ty }, encoding_ty, ret_ty);
        const after_name_local = try self.builder.program.addLocal(self.builder.symbols.fresh(), state_ty);

        const field_value_expr = try self.builder.program.addExpr(.{
            .ty = field.ty,
            .data = .{ .field_access = .{
                .receiver = value_expr,
                .field = field.name,
            } },
        });
        const value_try = try self.lowerEncodeShapeToState(
            field.ty,
            field_value_expr,
            encoding_expr,
            encoding_ty,
            try self.builder.localExpr(after_name_local, state_ty),
            state_ty,
            ret_ty,
            precomputed_plan,
        );
        const after_value_local = try self.builder.program.addLocal(self.builder.symbols.fresh(), state_ty);
        const rest_body = try self.lowerEncodeRecordFieldNamesFrom(
            shape_ty,
            value_expr,
            encoding_expr,
            encoding_ty,
            try self.builder.localExpr(after_value_local, state_ty),
            state_ty,
            ret_ty,
            precomputed_plan,
            record_fields,
            renamed_field_locals,
            field_index + 1,
        );
        const value_body = try self.sequenceTry(value_try, ret_ty, after_value_local, rest_body, ret_ty);
        return try self.sequenceTry(field_name_try, ret_ty, after_name_local, value_body, ret_ty);
    }

    fn lowerCustomEncodeToState(
        self: *BodyContext,
        lookup: MethodLookup,
        shape_ty: Type.TypeId,
        value_expr: Ast.ExprId,
        encoding_expr: Ast.ExprId,
        encoding_ty: Type.TypeId,
        state_expr: Ast.ExprId,
        state_ty: Type.TypeId,
        ret_ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        const runtime_fn_ty = try self.builder.program.types.add(.{ .func = .{
            .args = try self.builder.program.types.addSpan(&.{state_ty}),
            .ret = ret_ty,
        } });
        const callable_mono_ty = try self.methodTargetMonoTypeFromArgs(lookup, &.{ shape_ty, encoding_ty }, runtime_fn_ty);
        const encode_fn = self.builder.functionShape(callable_mono_ty, "custom encode_to target was not a function");
        const encode_arg_tys = self.builder.program.types.span(encode_fn.args);
        if (encode_arg_tys.len != 2) Common.invariant("custom encode_to target had an unexpected arity");
        if (!self.sameType(encode_arg_tys[0], shape_ty)) Common.invariant("custom encode_to value type differed from encoded shape");
        if (!self.sameType(encode_arg_tys[1], encoding_ty)) Common.invariant("custom encode_to encoding type differed from input encoding type");
        if (!self.sameType(encode_fn.ret, runtime_fn_ty)) Common.invariant("custom encode_to runtime function type differed from expected type");

        const encoder_expr = try self.builder.program.addExpr(.{
            .ty = runtime_fn_ty,
            .data = .{ .call_proc = .{
                .callee = .{ .func = try self.methodTargetCalleeWithMono(lookup, callable_mono_ty) },
                .args = try self.builder.program.addExprSpan(&[_]Ast.ExprId{ value_expr, encoding_expr }),
            } },
        });
        return try self.builder.program.addExpr(.{
            .ty = ret_ty,
            .data = .{ .call_value = .{
                .callee = encoder_expr,
                .args = try self.builder.program.addExprSpan(&[_]Ast.ExprId{state_expr}),
            } },
        });
    }

    fn lowerEncodeFormatMethod(
        self: *BodyContext,
        method_name: []const u8,
        arg_exprs: []const Ast.ExprId,
        arg_tys: []const Type.TypeId,
        encoding_ty: Type.TypeId,
        ret_ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        const lookup = self.methodLookupForTypeName(encoding_ty, method_name);
        const callable_mono_ty = try self.methodTargetMonoTypeFromArgs(lookup, arg_tys, ret_ty);
        const encode_fn = self.builder.functionShape(callable_mono_ty, "encode_to target method was not a function");
        const actual_arg_tys = self.builder.program.types.span(encode_fn.args);
        if (actual_arg_tys.len != arg_tys.len) Common.invariant("encode_to target method had an unexpected arity");
        for (actual_arg_tys, arg_tys) |actual, expected| {
            if (!self.sameType(actual, expected)) Common.invariant("encode_to target method argument type differed from expected type");
        }
        if (!self.sameType(encode_fn.ret, ret_ty)) Common.invariant("encode_to target method return type differed from expected type");
        return try self.builder.program.addExpr(.{
            .ty = ret_ty,
            .data = .{ .call_proc = .{
                .callee = .{ .func = try self.methodTargetCalleeWithMono(lookup, callable_mono_ty) },
                .args = try self.builder.program.addExprSpan(arg_exprs),
            } },
        });
    }

    fn cloneNamedTypeWithArgs(
        self: *BodyContext,
        template_ty: Type.TypeId,
        args: []const Type.TypeId,
        backing_ty: Type.TypeId,
    ) Allocator.Error!Type.TypeId {
        const template = switch (self.builder.program.types.get(template_ty)) {
            .named => |named| named,
            else => Common.invariant("Parse compiler helper expected a named template type"),
        };
        return try self.builder.program.types.add(.{ .named = .{
            .named_type = template.named_type,
            .def = template.def,
            .kind = template.kind,
            .builtin_owner = template.builtin_owner,
            .args = try self.builder.program.types.addSpan(args),
            .backing = .{ .ty = backing_ty, .use = template.backing.?.use },
        } });
    }

    const TryOptionalInfo = struct {
        backing_ty: Type.TypeId,
        ok_payload_ty: Type.TypeId,
        err_ty: Type.TypeId,
    };

    const TryInfo = struct {
        backing_ty: Type.TypeId,
        ok_ty: Type.TypeId,
        err_ty: Type.TypeId,
        ok_tag: Type.Tag,
        err_tag: Type.Tag,
    };

    const RecordFieldMatchMode = enum {
        direct,
        exact,
        caseless,
    };

    const StaticFieldLane = struct {
        offset: u32,
        active_len: u32,
    };

    fn recordPresenceWordCount(field_count: usize) usize {
        return (field_count + 63) / 64;
    }

    fn recordPresenceWordIndex(field_index: usize) usize {
        return field_index / 64;
    }

    fn recordPresenceMask(field_index: usize) u64 {
        return @as(u64, 1) << @as(u6, @intCast(field_index % 64));
    }

    fn selectStaticFieldLane(lengths: []const u32, texts: []const []const u8, length: u32) StaticFieldLane {
        if (lengths.len != texts.len) Common.invariant("record parser dispatch text arity did not match field lengths");
        if (length == 0 or length > 24) Common.invariant("static field lane requested for an unsupported field length");

        var group_count: usize = 0;
        for (lengths, texts) |field_len, text| {
            if (field_len != length) continue;
            if (text.len != length) Common.invariant("record parser dispatch text length did not match precomputed length");
            group_count += 1;
        }
        if (group_count == 0) Common.invariant("static field lane requested for an empty length bucket");

        var best = StaticFieldLane{ .offset = 0, .active_len = @min(length, 8) };
        var best_distinct: usize = 0;
        const offsets = [_]u32{ 0, 8, 16 };
        for (offsets) |offset| {
            if (offset >= length) continue;
            const active_len: u32 = @min(length - offset, 8);
            var distinct: usize = 0;
            for (texts, lengths, 0..) |text, field_len, text_index| {
                if (field_len != length) continue;
                const word = staticFieldLaneWord(text, offset, active_len);
                var seen = false;
                var previous: usize = 0;
                while (previous < text_index) : (previous += 1) {
                    if (lengths[previous] != length) continue;
                    if (staticFieldLaneWord(texts[previous], offset, active_len) == word) {
                        seen = true;
                        break;
                    }
                }
                if (!seen) distinct += 1;
            }
            if (distinct > best_distinct) {
                best = .{ .offset = offset, .active_len = active_len };
                best_distinct = distinct;
                if (distinct == group_count) break;
            }
        }
        return best;
    }

    fn staticFieldLaneWord(text: []const u8, offset: u32, active_len: u32) u64 {
        if (active_len > 8) Common.invariant("static field lane exceeded one word");
        const offset_usize: usize = @intCast(offset);
        const active_len_usize: usize = @intCast(active_len);
        if (offset_usize > text.len or active_len_usize > text.len - offset_usize) Common.invariant("static field lane exceeded field text");
        var word: u64 = 0;
        var index: u32 = 0;
        while (index < active_len) : (index += 1) {
            word |= @as(u64, text[offset_usize + @as(usize, @intCast(index))]) << @intCast(index * 8);
        }
        return word;
    }

    fn fieldLengthAlreadyHandled(lengths: []const u32, index: usize, length: u32) bool {
        var i = index + 1;
        while (i < lengths.len) : (i += 1) {
            if (lengths[i] == length) return true;
        }
        return false;
    }

    fn tryInfo(self: *BodyContext, try_ty: Type.TypeId) TryInfo {
        const backing_ty = self.builder.namedBackingType(try_ty) orelse Common.invariant("expected a named Try type");
        const ok_tag = self.monoTagByText(backing_ty, "Ok");
        const err_tag = self.monoTagByText(backing_ty, "Err");
        const ok_payloads = self.builder.program.types.span(ok_tag.payloads);
        const err_payloads = self.builder.program.types.span(err_tag.payloads);
        if (ok_payloads.len != 1 or err_payloads.len != 1) Common.invariant("Try tags must each carry one payload");
        return .{
            .backing_ty = backing_ty,
            .ok_ty = ok_payloads[0],
            .err_ty = err_payloads[0],
            .ok_tag = ok_tag,
            .err_tag = err_tag,
        };
    }

    fn tryTypeLike(
        self: *BodyContext,
        template_try_ty: Type.TypeId,
        ok_ty: Type.TypeId,
        err_ty: Type.TypeId,
    ) Allocator.Error!Type.TypeId {
        const template_backing = self.builder.namedBackingType(template_try_ty) orelse Common.invariant("Try template type had no backing");
        const template_tags = switch (self.builder.shapeContent(template_backing)) {
            .tag_union => |span| self.builder.program.types.tagSpan(span),
            else => Common.invariant("Try template backing type was not a tag union"),
        };
        const tags = try self.allocator.alloc(Type.Tag, template_tags.len);
        defer self.allocator.free(tags);

        var found_ok = false;
        var found_err = false;
        for (template_tags, 0..) |tag, index| {
            const text = self.builder.program.names.tagLabelText(tag.name);
            const payload_ty = if (Ident.textEql(text, "Ok")) blk: {
                found_ok = true;
                break :blk ok_ty;
            } else if (Ident.textEql(text, "Err")) blk: {
                found_err = true;
                break :blk err_ty;
            } else {
                Common.invariant("Try backing type contained an unexpected tag");
            };
            tags[index] = .{
                .name = tag.name,
                .checked_name = tag.checked_name,
                .payloads = try self.builder.program.types.addSpan(&[_]Type.TypeId{payload_ty}),
            };
        }
        if (!found_ok or !found_err) Common.invariant("Try backing type did not contain Ok and Err tags");

        const backing_ty = try self.builder.program.types.add(.{ .tag_union = try self.builder.program.types.addTags(tags) });
        const args = [_]Type.TypeId{ ok_ty, err_ty };
        return try self.cloneNamedTypeWithArgs(template_try_ty, &args, backing_ty);
    }

    fn tryOk(self: *BodyContext, try_ty: Type.TypeId, value_expr: Ast.ExprId) Allocator.Error!Ast.ExprId {
        const info = self.tryInfo(try_ty);
        const value_ty = self.builder.program.exprs.items[@intFromEnum(value_expr)].ty;
        if (!self.sameType(value_ty, info.ok_ty)) Common.invariant("Try.Ok payload type differed from Try Ok type");
        const backing_expr = try self.builder.program.addExpr(.{
            .ty = info.backing_ty,
            .data = .{ .tag = .{
                .name = info.ok_tag.name,
                .payloads = try self.builder.program.addExprSpan(&[_]Ast.ExprId{value_expr}),
            } },
        });
        return try self.builder.program.addExpr(.{
            .ty = try_ty,
            .data = .{ .nominal = backing_expr },
        });
    }

    fn tryErr(self: *BodyContext, try_ty: Type.TypeId, err_expr: Ast.ExprId) Allocator.Error!Ast.ExprId {
        const info = self.tryInfo(try_ty);
        const err_ty = self.builder.program.exprs.items[@intFromEnum(err_expr)].ty;
        if (!self.sameType(err_ty, info.err_ty)) Common.invariant("Try.Err payload type differed from Try Err type");
        const backing_expr = try self.builder.program.addExpr(.{
            .ty = info.backing_ty,
            .data = .{ .tag = .{
                .name = info.err_tag.name,
                .payloads = try self.builder.program.addExprSpan(&[_]Ast.ExprId{err_expr}),
            } },
        });
        return try self.builder.program.addExpr(.{
            .ty = try_ty,
            .data = .{ .nominal = backing_expr },
        });
    }

    fn sequenceTry(
        self: *BodyContext,
        try_expr: Ast.ExprId,
        try_ty: Type.TypeId,
        ok_local: Ast.LocalId,
        ok_body: Ast.ExprId,
        out_try_ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        const info = self.tryInfo(try_ty);
        const out_info = self.tryInfo(out_try_ty);
        if (!self.sameType(info.err_ty, out_info.err_ty)) Common.invariant("sequenced Try error type differed from output Try error type");

        return try self.builder.program.addExpr(.{ .ty = out_try_ty, .data = .{ .try_sequence = .{
            .try_expr = try_expr,
            .ok_local = ok_local,
            .err_is_cold = true,
            .ok_body = ok_body,
        } } });
    }

    fn sequenceTryRecord(
        self: *BodyContext,
        try_expr: Ast.ExprId,
        try_ty: Type.TypeId,
        value_local: Ast.LocalId,
        value_field: names.RecordFieldNameId,
        rest_local: Ast.LocalId,
        rest_field: names.RecordFieldNameId,
        ok_body: Ast.ExprId,
        out_try_ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        const info = self.tryInfo(try_ty);
        const out_info = self.tryInfo(out_try_ty);
        if (!self.sameType(info.err_ty, out_info.err_ty)) Common.invariant("sequenced Try record error type differed from output Try error type");
        if (!self.sameType(self.recordFieldType(info.ok_ty, value_field), self.builder.program.locals.items[@intFromEnum(value_local)].ty)) {
            Common.invariant("sequenced Try record value local type differed from Ok record field");
        }
        if (!self.sameType(self.recordFieldType(info.ok_ty, rest_field), self.builder.program.locals.items[@intFromEnum(rest_local)].ty)) {
            Common.invariant("sequenced Try record rest local type differed from Ok record field");
        }

        return try self.builder.program.addExpr(.{ .ty = out_try_ty, .data = .{ .try_record_sequence = .{
            .try_expr = try_expr,
            .value_local = value_local,
            .value_field = value_field,
            .rest_local = rest_local,
            .rest_field = rest_field,
            .err_is_cold = true,
            .ok_body = ok_body,
        } } });
    }

    fn recordFieldType(self: *BodyContext, record_ty: Type.TypeId, field_name: names.RecordFieldNameId) Type.TypeId {
        return switch (self.builder.shapeContent(record_ty)) {
            .record => |fields_span| {
                for (self.builder.program.types.fieldSpan(fields_span)) |field| {
                    if (field.name == field_name) return field.ty;
                }
                Common.invariant("record field was absent from monotype record");
            },
            else => Common.invariant("record field operation expected a record type"),
        };
    }

    fn parseFieldTypeIsSupported(self: *BodyContext, ty: Type.TypeId) bool {
        if (self.typeHasBuiltinOwner(ty, .str)) return true;
        if (self.typeHasBuiltinOwner(ty, .u64)) return true;
        if (self.tryOptionalInfo(ty) != null) return true;
        if (self.customParserLookup(ty) != null) return true;
        return switch (self.builder.shapeContent(ty)) {
            .record => |fields_span| blk: {
                for (self.builder.program.types.fieldSpan(fields_span)) |field| {
                    if (!self.parseFieldTypeIsSupported(field.ty)) break :blk false;
                }
                break :blk true;
            },
            .tag_union => |tags_span| blk: {
                const tags = self.builder.program.types.tagSpan(tags_span);
                if (tags.len == 0) break :blk false;
                for (tags) |tag| {
                    for (self.builder.program.types.span(tag.payloads)) |payload_ty| {
                        if (!self.parseFieldTypeIsSupported(payload_ty)) break :blk false;
                    }
                }
                break :blk true;
            },
            .zst => true,
            else => false,
        };
    }

    fn encodeFieldTypeIsSupported(self: *BodyContext, ty: Type.TypeId) bool {
        if (self.typeHasBuiltinOwner(ty, .str)) return true;
        if (self.typeHasBuiltinOwner(ty, .u64)) return true;
        if (self.customEncodeToLookup(ty) != null) return true;
        return switch (self.builder.shapeContent(ty)) {
            .record => |fields_span| blk: {
                for (self.builder.program.types.fieldSpan(fields_span)) |field| {
                    if (!self.encodeFieldTypeIsSupported(field.ty)) break :blk false;
                }
                break :blk true;
            },
            .zst => true,
            else => false,
        };
    }

    fn customParserLookup(self: *BodyContext, ty: Type.TypeId) ?MethodLookup {
        const named = switch (self.builder.program.types.get(ty)) {
            .named => |named| named,
            else => return null,
        };
        if (named.builtin_owner != null) return null;
        switch (named.kind) {
            .nominal, .@"opaque" => {},
            .alias => return null,
        }
        const owner = methodOwnerFromType(&self.builder.program.types, ty) orelse
            Common.invariant("custom named parser type had no method owner");
        return self.builder.lookupMethodTargetByName(owner, "parser_for") orelse
            Common.invariant("checked method registry is missing custom parser_for target");
    }

    fn customEncodeToLookup(self: *BodyContext, ty: Type.TypeId) ?MethodLookup {
        const named = switch (self.builder.program.types.get(ty)) {
            .named => |named| named,
            else => return null,
        };
        if (named.builtin_owner != null) return null;
        switch (named.kind) {
            .nominal, .@"opaque" => {},
            .alias => return null,
        }
        const owner = methodOwnerFromType(&self.builder.program.types, ty) orelse
            Common.invariant("custom named encode_to type had no method owner");
        return self.builder.lookupMethodTargetByName(owner, "encode_to") orelse
            Common.invariant("checked method registry is missing custom encode_to target");
    }

    fn tryOptionalInfo(self: *BodyContext, ty: Type.TypeId) ?TryOptionalInfo {
        const backing_ty = self.builder.namedBackingType(ty) orelse return null;
        const ok_tag = self.monoTagByTextOptional(backing_ty, "Ok") orelse return null;
        const err_tag = self.monoTagByTextOptional(backing_ty, "Err") orelse return null;
        const ok_payloads = self.builder.program.types.span(ok_tag.payloads);
        const err_payloads = self.builder.program.types.span(err_tag.payloads);
        if (ok_payloads.len != 1 or err_payloads.len != 1) return null;
        return .{
            .backing_ty = backing_ty,
            .ok_payload_ty = ok_payloads[0],
            .err_ty = err_payloads[0],
        };
    }

    fn finishOptionalRecordFieldFromPresencePayload(
        self: *BodyContext,
        next_body: Ast.ExprId,
        ret_ty: Type.TypeId,
        record_slots: ParseRecordSlots,
        field: Type.Field,
        field_index: usize,
        field_local: Ast.LocalId,
        rest_local: Ast.LocalId,
        encoding_expr: Ast.ExprId,
        encoding_ty: Type.TypeId,
        state_ty: Type.TypeId,
        renamed_field_local: Ast.LocalId,
    ) Allocator.Error!Ast.ExprId {
        const field_ty = field.ty;
        const optional_info = self.tryOptionalInfo(field_ty) orelse
            Common.invariant("optional record finish requested for a non-optional field");
        const payload_local = record_slots.payload_locals[field_index];
        const payload_ty = record_slots.payload_tys[field_index];
        if (!self.sameType(payload_ty, field_ty)) {
            Common.invariant("generated optional record payload type differed from field type");
        }
        if (!self.sameType(self.builder.program.locals.items[@intFromEnum(field_local)].ty, field_ty)) {
            Common.invariant("record finish field local type differed from optional field type");
        }

        const renamed_field_ty = self.builder.program.locals.items[@intFromEnum(renamed_field_local)].ty;
        if (!self.typeHasBuiltinOwner(renamed_field_ty, .str)) Common.invariant("record parser renamed field local was not Str");
        const renamed_field_expr = try self.builder.localExpr(renamed_field_local, renamed_field_ty);
        const missing_error = try self.missingOptionalFieldError(encoding_expr, renamed_field_expr, rest_local, encoding_ty, state_ty, optional_info.err_ty);
        const missing_field = try self.tryErr(field_ty, missing_error);
        const present_field = try self.builder.localExpr(payload_local, payload_ty);
        const presence_word = recordPresenceWordIndex(field_index);
        const is_present_expr = try self.builder.localExpr(record_slots.presence_locals[presence_word], record_slots.presence_tys[presence_word]);
        const field_value = try self.builder.program.addExpr(.{ .ty = field_ty, .data = .{ .if_initialized_payload = .{
            .cond = is_present_expr,
            .cond_mask = recordPresenceMask(field_index),
            .payload = payload_local,
            .uninitialized_is_cold = false,
            .initialized = present_field,
            .uninitialized = missing_field,
        } } });
        return try self.wrapLet(field_local, field_ty, field_value, next_body, ret_ty);
    }

    fn parseRecordFieldFromPresencePayload(
        self: *BodyContext,
        is_present_expr: Ast.ExprId,
        is_present_mask: u64,
        payload_local: Ast.LocalId,
        payload_ty: Type.TypeId,
        field: Type.Field,
        field_try_ty: Type.TypeId,
        rest_local: Ast.LocalId,
        encoding_expr: Ast.ExprId,
        encoding_ty: Type.TypeId,
        state_ty: Type.TypeId,
        renamed_field_local: Ast.LocalId,
    ) Allocator.Error!Ast.ExprId {
        const field_ty = field.ty;
        const field_try_info = self.tryInfo(field_try_ty);
        if (!self.sameType(field_try_info.ok_ty, field_ty)) Common.invariant("structural parser field Try Ok type differed from field type");
        const renamed_field_ty = self.builder.program.locals.items[@intFromEnum(renamed_field_local)].ty;
        if (!self.typeHasBuiltinOwner(renamed_field_ty, .str)) Common.invariant("record parser renamed field local was not Str");
        const renamed_field_expr = try self.builder.localExpr(renamed_field_local, renamed_field_ty);

        if (!self.sameType(payload_ty, field_ty)) {
            Common.invariant("generated record parse payload type differed from parsed field type");
        }
        const present_body = try self.tryOk(field_try_ty, try self.builder.localExpr(payload_local, payload_ty));

        const absent_body = if (self.typeHasBuiltinOwner(field_ty, .str)) blk: {
            break :blk try self.tryErr(field_try_ty, try self.missingRecordFieldError(encoding_expr, renamed_field_expr, rest_local, encoding_ty, state_ty, field_try_info.err_ty));
        } else if (self.tryOptionalInfo(field_ty)) |info| blk: {
            const missing_error = try self.missingOptionalFieldError(encoding_expr, renamed_field_expr, rest_local, encoding_ty, state_ty, info.err_ty);
            const missing_field = try self.tryErr(field_ty, missing_error);
            break :blk try self.tryOk(field_try_ty, missing_field);
        } else try self.tryErr(field_try_ty, try self.missingRecordFieldError(encoding_expr, renamed_field_expr, rest_local, encoding_ty, state_ty, field_try_info.err_ty));

        return try self.builder.program.addExpr(.{ .ty = field_try_ty, .data = .{ .if_initialized_payload = .{
            .cond = is_present_expr,
            .cond_mask = is_present_mask,
            .payload = payload_local,
            .uninitialized_is_cold = true,
            .initialized = present_body,
            .uninitialized = absent_body,
        } } });
    }

    fn missingRecordFieldError(
        self: *BodyContext,
        encoding_expr: Ast.ExprId,
        field_name_expr: Ast.ExprId,
        rest_local: Ast.LocalId,
        encoding_ty: Type.TypeId,
        state_ty: Type.TypeId,
        err_ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        const str_ty = try self.builder.primitiveType(.str);
        const lookup = self.methodLookupForTypeName(encoding_ty, "missing_record_field");
        const callable_mono_ty = try self.methodTargetMonoTypeFromArgs(lookup, &.{ encoding_ty, str_ty, state_ty }, err_ty);
        const missing_fn = self.builder.functionShape(callable_mono_ty, "missing_record_field target method was not a function");
        const arg_tys = self.builder.program.types.span(missing_fn.args);
        if (arg_tys.len != 3) Common.invariant("missing_record_field target method had an unexpected arity");
        if (!self.sameType(arg_tys[0], encoding_ty)) Common.invariant("missing_record_field encoding type differed from record encoding type");
        if (!self.sameType(arg_tys[1], str_ty)) Common.invariant("missing_record_field name type differed from Str");
        if (!self.sameType(arg_tys[2], state_ty)) Common.invariant("missing_record_field state type differed from record rest state type");
        if (!self.sameType(missing_fn.ret, err_ty)) Common.invariant("missing_record_field return type differed from parse error type");

        const args = [_]Ast.ExprId{
            encoding_expr,
            field_name_expr,
            try self.builder.localExpr(rest_local, state_ty),
        };
        return try self.builder.program.addExpr(.{
            .ty = err_ty,
            .data = .{ .call_proc = .{
                .callee = .{ .func = try self.methodTargetCalleeWithMono(lookup, callable_mono_ty) },
                .args = try self.builder.program.addExprSpan(&args),
                .is_cold = true,
            } },
        });
    }

    fn missingOptionalFieldError(
        self: *BodyContext,
        encoding_expr: Ast.ExprId,
        field_name_expr: Ast.ExprId,
        rest_local: Ast.LocalId,
        encoding_ty: Type.TypeId,
        state_ty: Type.TypeId,
        err_ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        const str_ty = try self.builder.primitiveType(.str);
        const lookup = self.methodLookupForTypeName(encoding_ty, "missing_optional_field");
        const callable_mono_ty = try self.methodTargetMonoTypeFromArgs(lookup, &.{ encoding_ty, str_ty, state_ty }, err_ty);
        const missing_fn = self.builder.functionShape(callable_mono_ty, "missing_optional_field target method was not a function");
        const arg_tys = self.builder.program.types.span(missing_fn.args);
        if (arg_tys.len != 3) Common.invariant("missing_optional_field target method had an unexpected arity");
        if (!self.sameType(arg_tys[0], encoding_ty)) Common.invariant("missing_optional_field encoding type differed from record encoding type");
        if (!self.sameType(arg_tys[1], str_ty)) Common.invariant("missing_optional_field name type differed from Str");
        if (!self.sameType(arg_tys[2], state_ty)) Common.invariant("missing_optional_field state type differed from record rest state type");
        if (!self.sameType(missing_fn.ret, err_ty)) Common.invariant("missing_optional_field return type differed from optional field error type");

        const args = [_]Ast.ExprId{
            encoding_expr,
            field_name_expr,
            try self.builder.localExpr(rest_local, state_ty),
        };
        return try self.builder.program.addExpr(.{
            .ty = err_ty,
            .data = .{ .call_proc = .{
                .callee = .{ .func = try self.methodTargetCalleeWithMono(lookup, callable_mono_ty) },
                .args = try self.builder.program.addExprSpan(&args),
            } },
        });
    }

    fn lowerDirectStructuralEq(
        self: *BodyContext,
        checked_ret_ty: checked.CheckedTypeId,
        eq: anytype,
    ) Allocator.Error!Ast.ExprId {
        const ret_ty = try self.lowerType(checked_ret_ty);
        return try self.lowerDirectStructuralEqAtType(eq, ret_ty);
    }

    fn lowerDirectStructuralEqAtType(
        self: *BodyContext,
        eq: anytype,
        ret_ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        const operand_ty = try self.structuralEqualityOperandType(eq);
        const lhs = try self.lowerExprAtType(eq.lhs, operand_ty);
        const rhs = try self.lowerExprAtType(eq.rhs, operand_ty);
        var result = try self.lowerEqualityExpr(operand_ty, lhs, rhs, "is_eq", ret_ty);
        if (eq.negated) {
            result = try self.builder.lowLevelExpr(.bool_not, &.{result}, ret_ty);
        }
        return result;
    }

    fn structuralEqualityOperandType(self: *BodyContext, eq: anytype) Allocator.Error!Type.TypeId {
        const lhs_checked_ty = self.view.bodies.expr(eq.lhs).ty;
        const rhs_checked_ty = self.view.bodies.expr(eq.rhs).ty;
        const conflict_message = "checked structural equality operand type conflicted with an existing Monotype constraint";

        try self.constrainCheckedTypeRelations(lhs_checked_ty, self, rhs_checked_ty);

        if (try self.structuralEqualityExprResultType(eq.lhs, null)) |lhs_ty| {
            return try self.constrainStructuralEqualityOperandType(lhs_ty, eq.rhs, rhs_checked_ty, conflict_message);
        }
        if (try self.structuralEqualityExprResultType(eq.rhs, null)) |rhs_ty| {
            return try self.constrainStructuralEqualityOperandType(rhs_ty, eq.lhs, lhs_checked_ty, conflict_message);
        }

        return try self.lowerType(lhs_checked_ty);
    }

    /// Resolves the Monotype an equality operand evaluates to, when that operand is a
    /// result-producing expression (call, dispatch, lookup, field access). The shared
    /// equality operand type is taken from this result so an open tag literal on the other
    /// side cannot narrow it. Returns null for any other expression shape (e.g. a tag
    /// literal): the caller then falls through to the concrete-shape ladder in
    /// structuralEqualityOperandType, so this must not fall back to lowerType itself.
    fn structuralEqualityExprResultType(
        self: *BodyContext,
        expr_id: checked.CheckedExprId,
        expected_ty: ?Type.TypeId,
    ) Allocator.Error!?Type.TypeId {
        const expr = self.view.bodies.expr(expr_id);
        return switch (expr.data) {
            .call => |call| try self.callResultMonoType(expr.ty, call, expected_ty),
            .dispatch_call => |plan| try self.dispatchResultMonoType(expr.ty, plan, expected_ty),
            .interpolation => |interpolation| try self.dispatchResultMonoType(expr.ty, interpolation.plan, expected_ty),
            .type_dispatch_call => |plan| try self.dispatchResultMonoType(expr.ty, plan, expected_ty),
            .method_eq => |plan| try self.dispatchResultMonoType(expr.ty, plan, expected_ty),
            .lookup_local => |lookup| try self.lookupExprMonoType(expr.ty, lookup.resolved),
            .lookup_external => |resolved| try self.lookupExprMonoType(expr.ty, resolved),
            .lookup_required => |resolved| try self.lookupExprMonoType(expr.ty, resolved),
            .field_access => |field| blk: {
                if (expected_ty) |ty| {
                    try self.constrainTypeToMono(expr.ty, ty);
                    break :blk ty;
                }
                break :blk try self.fieldAccessMonoType(field.receiver, field.field_name);
            },
            else => null,
        };
    }

    fn constrainStructuralEqualityOperandType(
        self: *BodyContext,
        operand_ty: Type.TypeId,
        other_expr_id: checked.CheckedExprId,
        other_checked_ty: checked.CheckedTypeId,
        comptime conflict_message: []const u8,
    ) Allocator.Error!Type.TypeId {
        try self.constrainTypeToMono(other_checked_ty, operand_ty);
        if (try self.structuralEqualityExprResultType(other_expr_id, operand_ty)) |other_ty| {
            if (!self.sameType(operand_ty, other_ty)) Common.invariant(conflict_message);
        }
        return operand_ty;
    }

    /// Whether `shape` is a type the structural-derivation ladders (is_eq /
    /// to_hash) expand a layer at a time, guarding recursion through an
    /// expansion stack. Aggregates and transparent nominals expand; scalars,
    /// owned wrappers (List/Box), and opaque nominals bottom out instead.
    fn structurallyExpands(shape: Type.Content) bool {
        return switch (shape) {
            .record, .tuple, .tag_union => true,
            .named => |named| named.backing != null,
            else => false,
        };
    }

    fn lowerEqualityExpr(
        self: *BodyContext,
        ty: Type.TypeId,
        lhs: Ast.ExprId,
        rhs: Ast.ExprId,
        method_name: []const u8,
        bool_ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        return try self.lowerDerivation(EqDeriver, ty, .{ .lhs = lhs, .rhs = rhs }, .{
            .method_name = method_name,
            .result_ty = bool_ty,
        });
    }

    /// Hash `value` (of `value_ty`) into `hasher`, producing a new Hasher of
    /// `hasher_ty`. The threading is "innermost first": each component feeds the
    /// hasher accumulated so far.
    fn lowerHashExpr(
        self: *BodyContext,
        value_ty: Type.TypeId,
        value: Ast.ExprId,
        hasher: Ast.ExprId,
        hasher_ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        return try self.lowerDerivation(HashDeriver, value_ty, .{ .value = value, .hasher = hasher }, .{
            .method_name = "to_hash",
            .result_ty = hasher_ty,
        });
    }

    // Generic structural-derivation driver.
    //
    // is_eq and to_hash share one recursive ladder: walk a type one layer at a
    // time, decomposing aggregates (records/tuples/tag unions) and transparent
    // nominals, dispatching owned types (List) to their real method, and
    // emitting a leaf node (`structural_eq` / `structural_hash`) for scalars,
    // opaque nominals, and other inline-handled leaves. Recursion is broken by
    // an expansion stack plus a memoized generated helper def.
    //
    // The per-derivation specifics are supplied by a comptime `Deriver` type
    // (`EqDeriver` / `HashDeriver`). A Deriver provides:
    //   - `Operand`: the operands threaded into one derivation step (equality
    //     holds two `Ast.ExprId`; hashing holds the value plus the running
    //     hasher accumulator).
    //   - the expansion-stack and memoized-def-cache accessors plus the helper
    //     fn type, so recursion breaks identically for both.
    //   - `leaf`: emit the leaf node for inline-handled shapes.
    //   - `componentForField` / `componentForTuple`: build a component's
    //     operands from the aggregate operands and the running fold state.
    //   - `combineSeed` / `combine` plus `forward`: fold the per-component
    //     results (equality conjoins component bools with AND; hashing threads
    //     the accumulator left to right).
    //   - `ownedCall` / `named` / `tagUnion`: the shapes whose decomposition
    //     differs structurally between the two derivations.
    //
    // A `DerivationCtx` carries the runtime parameters shared by every step:
    // the derivation's result type (Bool / Hasher) and the method name.

    const DerivationCtx = struct {
        method_name: []const u8,
        result_ty: Type.TypeId,
    };

    fn lowerDerivation(
        self: *BodyContext,
        comptime D: type,
        ty: Type.TypeId,
        operand: D.Operand,
        ctx: DerivationCtx,
    ) Allocator.Error!Ast.ExprId {
        const shape = self.builder.program.types.get(ty);
        const expands_structurally = structurallyExpands(shape);
        var remove_active_expansion = false;
        const stack = D.expansionStack(self);
        defer if (remove_active_expansion) {
            _ = stack.remove(ty);
        };
        if (expands_structurally) {
            if (stack.contains(ty)) {
                return try self.derivationCall(D, ty, operand, ctx);
            }
            try stack.put(ty, {});
            remove_active_expansion = true;
        }

        return switch (shape) {
            .list => try D.ownedCall(self, ty, operand, ctx),
            .record => |fields| try self.derivationRecord(D, self.builder.program.types.fieldSpan(fields), operand, ctx),
            .tuple => |items| try self.derivationTuple(D, self.builder.program.types.span(items), operand, ctx),
            .tag_union => |tags| try D.tagUnion(self, ty, tags, operand, ctx),
            .named => |named| if (named.backing) |backing|
                try D.named(self, ty, backing.ty, operand, ctx)
            else
                try D.leaf(self, operand, ctx),
            .primitive,
            .zst,
            .func,
            .erased,
            .box,
            => try D.leaf(self, operand, ctx),
        };
    }

    /// Emit a call to the memoized recursive helper for `ty`, generating its def
    /// on first request.
    fn derivationCall(
        self: *BodyContext,
        comptime D: type,
        ty: Type.TypeId,
        operand: D.Operand,
        ctx: DerivationCtx,
    ) Allocator.Error!Ast.ExprId {
        const def_id = try self.derivationDefForType(D, ty, ctx);
        const fn_ty = try D.fnType(self, ty, ctx.result_ty);
        const callee = try self.builder.program.addExpr(.{
            .ty = fn_ty,
            .data = .{ .def_ref = def_id },
        });
        const args = D.callArgs(operand);
        return try self.builder.program.addExpr(.{
            .ty = ctx.result_ty,
            .data = .{ .call_value = .{
                .callee = callee,
                .args = try self.builder.program.addExprSpan(&args),
            } },
        });
    }

    /// Dispatch an owned (non-structural) type to its real derived method,
    /// shared by every derivation. The argument types and the per-derivation
    /// invariant wording come from the comptime `Deriver`; the operand-to-args
    /// mapping reuses `D.callArgs`.
    fn derivationOwnedCall(
        self: *BodyContext,
        comptime D: type,
        ty: Type.TypeId,
        operand: D.Operand,
        ctx: DerivationCtx,
    ) Allocator.Error!Ast.ExprId {
        const owner = methodOwnerFromType(&self.builder.program.types, ty) orelse
            Common.invariant(D.owned_missing_owner_msg);
        const lookup = self.builder.lookupMethodTargetByName(owner, ctx.method_name) orelse
            Common.invariant(D.owned_missing_target_msg);

        const arg_tys = D.ownedArgTypes(ty, ctx.result_ty);
        const callable_mono_ty = try self.methodTargetMonoTypeFromArgs(lookup, &arg_tys, ctx.result_ty);
        const args = D.callArgs(operand);
        return try self.builder.program.addExpr(.{ .ty = ctx.result_ty, .data = .{ .call_proc = .{
            .callee = .{ .func = try self.methodTargetCalleeWithMono(lookup, callable_mono_ty) },
            .args = try self.builder.program.addExprSpan(&args),
        } } });
    }

    fn derivationDefForType(
        self: *BodyContext,
        comptime D: type,
        value_ty: Type.TypeId,
        ctx: DerivationCtx,
    ) Allocator.Error!Ast.DefId {
        const cache = D.defCache(self);
        const address = D.defAddress(value_ty, ctx.result_ty);
        if (cache.get(address)) |entry| return entry.id();

        const def_id: Ast.DefId = @enumFromInt(@as(u32, @intCast(self.builder.program.defs.items.len)));
        try self.builder.program.defs.append(self.allocator, undefined);
        try cache.put(address, .{ .reserved = def_id });

        // A helper takes two args: the value (`value_ty`) and a second operand
        // whose type is derivation-specific (the other equality operand at
        // `value_ty`, or the running hasher at `result_ty`).
        const second_ty = D.helperSecondArgType(value_ty, ctx.result_ty);
        const self_local = try self.builder.program.addLocal(self.builder.symbols.fresh(), value_ty);
        const aux_local = try self.builder.program.addLocal(self.builder.symbols.fresh(), second_ty);
        const self_expr = try self.builder.localExpr(self_local, value_ty);
        const aux_expr = try self.builder.localExpr(aux_local, second_ty);
        const operand = D.helperOperand(self_expr, aux_expr);

        // The helper body is the structural expansion for this type. Temporarily
        // remove the caller's active mark so the body expands exactly one layer
        // and only recursive edges inside that layer call back to the reserved
        // helper.
        const stack = D.expansionStack(self);
        if (!stack.remove(value_ty)) {
            Common.invariant("recursive structural derivation helper requested outside an active expansion");
        }
        defer stack.putAssumeCapacity(value_ty, {});

        const body = try self.lowerDerivation(D, value_ty, operand, ctx);
        const args = try self.builder.program.addTypedLocalSpan(&.{
            .{ .local = self_local, .ty = value_ty },
            .{ .local = aux_local, .ty = second_ty },
        });
        self.builder.program.defs.items[@intFromEnum(def_id)] = .{
            .symbol = self.builder.symbols.fresh(),
            .fn_def = null,
            .args = args,
            .body = .{ .roc = body },
            .ret = ctx.result_ty,
        };
        try cache.put(address, .{ .ready = def_id });
        return def_id;
    }

    fn derivationRecord(
        self: *BodyContext,
        comptime D: type,
        fields: []const Type.Field,
        operand: D.Operand,
        ctx: DerivationCtx,
    ) Allocator.Error!Ast.ExprId {
        // Copy because recursive lowerDerivation may reallocate types.fields, invalidating the slice.
        const fields_copy = try self.allocator.dupe(Type.Field, fields);
        defer self.allocator.free(fields_copy);
        var state = try D.combineSeed(self, operand, ctx);
        var i: usize = 0;
        while (i < fields_copy.len) : (i += 1) {
            const field = fields_copy[if (D.forward) i else fields_copy.len - 1 - i];
            const component = try D.componentForField(self, operand, state, field);
            const result = try self.lowerDerivation(D, field.ty, component, ctx);
            state = try D.combine(self, state, result, ctx);
        }
        return state;
    }

    fn derivationTuple(
        self: *BodyContext,
        comptime D: type,
        items: []const Type.TypeId,
        operand: D.Operand,
        ctx: DerivationCtx,
    ) Allocator.Error!Ast.ExprId {
        // Copy because recursive lowerDerivation may reallocate types.spans, invalidating the slice.
        const items_copy = try self.allocator.dupe(Type.TypeId, items);
        defer self.allocator.free(items_copy);
        var state = try D.combineSeed(self, operand, ctx);
        var i: usize = 0;
        while (i < items_copy.len) : (i += 1) {
            const index = if (D.forward) i else items_copy.len - 1 - i;
            const item_ty = items_copy[index];
            const component = try D.componentForTuple(self, operand, state, item_ty, index);
            const result = try self.lowerDerivation(D, item_ty, component, ctx);
            state = try D.combine(self, state, result, ctx);
        }
        return state;
    }

    fn boolLiteral(self: *BodyContext, value: bool, bool_ty: Type.TypeId) Allocator.Error!Ast.ExprId {
        const u64_ty = try self.builder.primitiveType(.u64);
        const lhs = try self.builder.intLiteralExpr(0, u64_ty);
        const rhs = try self.builder.intLiteralExpr(if (value) 0 else 1, u64_ty);
        return try self.builder.lowLevelExpr(.num_is_eq, &.{ lhs, rhs }, bool_ty);
    }

    fn lowerDirectStructuralHash(
        self: *BodyContext,
        checked_ret_ty: checked.CheckedTypeId,
        h: anytype,
    ) Allocator.Error!Ast.ExprId {
        const ret_ty = try self.lowerType(checked_ret_ty);
        return try self.lowerDirectStructuralHashAtType(h, ret_ty);
    }

    fn lowerDirectStructuralHashAtType(
        self: *BodyContext,
        h: anytype,
        hasher_ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        const value_ty = try self.lowerExprType(h.value);
        const value = try self.lowerExprAtType(h.value, value_ty);
        const hasher = try self.lowerExprAtType(h.hasher, hasher_ty);
        return try self.lowerHashExpr(value_ty, value, hasher, hasher_ty);
    }

    /// Emit `Hasher.write_u64(hasher, value)` as a low-level operation.
    fn hasherWriteU64(
        self: *BodyContext,
        hasher: Ast.ExprId,
        value: u64,
        hasher_ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        const u64_ty = try self.builder.primitiveType(.u64);
        const value_expr = try self.builder.intLiteralExpr(value, u64_ty);
        return try self.builder.lowLevelExpr(.hasher_write_u64, &.{ hasher, value_expr }, hasher_ty);
    }

    const MatchOutput = union(enum) {
        value: Type.TypeId,
        state_result: struct {
            result_ty: Type.TypeId,
            state_ty: Type.TypeId,
            merge_binders: []const MergeBinder,
        },
        state_only: struct {
            state_ty: Type.TypeId,
            merge_binders: []const MergeBinder,
        },
    };

    fn matchOutputType(_: *BodyContext, output: MatchOutput) Type.TypeId {
        return switch (output) {
            .value => |ty| ty,
            .state_result => |state| state.state_ty,
            .state_only => |state| state.state_ty,
        };
    }

    fn lowerMatchBranchBody(
        self: *BodyContext,
        body: checked.CheckedExprId,
        output: MatchOutput,
    ) Allocator.Error!Ast.ExprId {
        return switch (output) {
            .value => |result_ty| try self.lowerBranchValueAtType(body, result_ty),
            .state_result => |state| try self.lowerBodyThenStateResult(
                body,
                state.result_ty,
                state.state_ty,
                state.merge_binders,
            ),
            .state_only => |state| try self.lowerBodyThenStateOnly(
                body,
                state.state_ty,
                state.merge_binders,
            ),
        };
    }

    fn lowerBranchValueAtType(
        self: *BodyContext,
        body: checked.CheckedExprId,
        result_ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        if (self.checkedExprDiverges(body)) {
            return try self.lowerDivergentExprAtType(body, result_ty);
        }
        return try self.lowerExprAtType(body, result_ty);
    }

    fn lowerMatchExpr(self: *BodyContext, expr_id: checked.CheckedExprId, match: anytype, result_ty: Type.TypeId) Allocator.Error!Ast.ExprId {
        const comptime_site = try self.matchComptimeSite(expr_id, match);
        const merge_binders = try self.stateMergeBinders(expr_id);
        defer self.allocator.free(merge_binders);
        if (merge_binders.len == 0) {
            return try self.lowerMatchExprWithOutput(match, .{ .value = result_ty }, comptime_site);
        }

        const state_ty = try self.stateResultType(merge_binders, result_ty);
        const state_expr = try self.lowerMatchExprWithOutput(match, .{ .state_result = .{
            .result_ty = result_ty,
            .state_ty = state_ty,
            .merge_binders = merge_binders,
        } }, comptime_site);

        const pattern_items = try self.allocator.alloc(Ast.PatId, merge_binders.len + 1);
        defer self.allocator.free(pattern_items);
        for (merge_binders, 0..) |merge, i| {
            const local = try self.builder.program.addLocalWithBinder(self.builder.symbols.fresh(), merge.ty, merge.binder);
            try bindLocalName(self.builder.program, self.view, local, merge.binder);
            try self.binders.put(merge.binder, local);
            pattern_items[i] = try self.builder.program.addPat(.{ .ty = merge.ty, .data = .{ .bind = local } });
        }

        const result_local = try self.builder.program.addLocal(self.builder.symbols.fresh(), result_ty);
        pattern_items[merge_binders.len] = try self.builder.program.addPat(.{ .ty = result_ty, .data = .{ .bind = result_local } });
        const bind_pat = try self.builder.program.addPat(.{ .ty = state_ty, .data = .{ .tuple = try self.builder.program.addPatSpan(pattern_items) } });
        const rest = try self.builder.localExpr(result_local, result_ty);

        return try self.builder.program.addExpr(.{ .ty = result_ty, .data = .{ .let_ = .{
            .bind = bind_pat,
            .value = state_expr,
            .rest = rest,
        } } });
    }

    fn lowerMatchExprWithOutput(
        self: *BodyContext,
        match: anytype,
        output: MatchOutput,
        comptime_site: ?Ast.ComptimeSiteId,
    ) Allocator.Error!Ast.ExprId {
        const output_ty = self.matchOutputType(output);
        return try self.builder.program.addExpr(.{
            .ty = output_ty,
            .data = try self.lowerMatch(match, output, comptime_site),
        });
    }

    fn patternNeedsExplicitBinding(self: *BodyContext, pattern_id: checked.CheckedPatternId) bool {
        const pattern = self.view.bodies.pattern(pattern_id);
        return switch (pattern.data) {
            .list => true,
            .record_destructure => |destructs| self.recordDestructsNeedExplicitRest(destructs),
            .assign,
            .as,
            .applied_tag,
            .nominal,
            .tuple,
            .num_literal,
            .small_dec_literal,
            .dec_literal,
            .frac_f32_literal,
            .frac_f64_literal,
            .str_literal,
            .str_interpolation,
            .underscore,
            .pending,
            .runtime_error,
            => false,
        };
    }

    fn patternCanMiss(self: *BodyContext, pattern_id: checked.CheckedPatternId) bool {
        const pattern = self.view.bodies.pattern(pattern_id);
        return switch (pattern.data) {
            .assign,
            .underscore,
            => false,
            .as => |as| self.patternCanMiss(as.pattern),
            .nominal => |nominal| self.patternCanMiss(nominal.backing_pattern),
            .tuple => |items| blk: {
                for (items) |child| {
                    if (self.patternCanMiss(child)) break :blk true;
                }
                break :blk false;
            },
            .record_destructure => |destructs| blk: {
                for (destructs) |destruct| {
                    const child = switch (destruct.kind) {
                        .required, .sub_pattern, .rest => |child_pattern| child_pattern,
                    };
                    if (self.patternCanMiss(child)) break :blk true;
                }
                break :blk false;
            },
            .applied_tag,
            .list,
            .num_literal,
            .small_dec_literal,
            .dec_literal,
            .frac_f32_literal,
            .frac_f64_literal,
            .str_literal,
            .str_interpolation,
            => true,
            .pending,
            .runtime_error,
            => false,
        };
    }

    fn recordDestructsNeedExplicitRest(self: *BodyContext, destructs: []const checked.CheckedRecordDestruct) bool {
        for (destructs) |destruct| {
            switch (destruct.kind) {
                .rest => |child| if (!self.patternIsIgnored(child)) return true,
                .required, .sub_pattern => {},
            }
        }
        return false;
    }

    fn listPatternCondition(
        self: *BodyContext,
        scrutinee: Ast.ExprId,
        list: anytype,
    ) Allocator.Error!Ast.ExprId {
        const u64_ty = try self.builder.primitiveType(.u64);
        const bool_ty = try self.builder.primitiveType(.bool);
        const len = try self.builder.lowLevelExpr(.list_len, &.{scrutinee}, u64_ty);
        const required = try self.builder.intLiteralExpr(list.patterns.len, u64_ty);
        const op: can.CIR.Expr.LowLevel = if (list.rest == null) .num_is_eq else .num_is_gte;
        return try self.builder.lowLevelExpr(op, &.{ len, required }, bool_ty);
    }

    fn applyListCheck(
        self: *BodyContext,
        scrutinee: Ast.ExprId,
        scrutinee_ty: Type.TypeId,
        list: anytype,
        body: Ast.ExprId,
        fallback: Ast.ExprId,
        output_ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        const elem_ty = self.constListElemType(scrutinee_ty);
        const u64_ty = try self.builder.primitiveType(.u64);
        const needs_len = if (list.rest) |rest| rest.index < list.patterns.len or rest.pattern != null else false;
        const len = if (needs_len) try self.builder.lowLevelExpr(.list_len, &.{scrutinee}, u64_ty) else null;

        const values = try self.allocator.alloc(Ast.ExprId, list.patterns.len);
        defer self.allocator.free(values);
        const patterns = try self.allocator.alloc(Ast.PatId, list.patterns.len);
        defer self.allocator.free(patterns);
        const sub_checks_per_elem = try self.allocator.alloc(std.ArrayList(CollectedListPattern), list.patterns.len);
        defer {
            for (sub_checks_per_elem) |*sub| sub.deinit(self.allocator);
            self.allocator.free(sub_checks_per_elem);
        }
        for (sub_checks_per_elem) |*sub| sub.* = std.ArrayList(CollectedListPattern).empty;
        const literal_guards_per_elem = try self.allocator.alloc([]PatternLiteralGuard, list.patterns.len);
        defer {
            for (literal_guards_per_elem) |guards| self.allocator.free(guards);
            self.allocator.free(literal_guards_per_elem);
        }
        for (literal_guards_per_elem) |*guards| guards.* = &.{};

        for (list.patterns, 0..) |pattern_id, index| {
            const item_index = try self.listPatternItemIndex(index, list.patterns.len, list.rest, len, u64_ty);
            values[index] = try self.builder.lowLevelExpr(.list_get_unsafe, &.{ scrutinee, item_index }, elem_ty);
            const guards_start = self.pattern_literal_guards.items.len;
            patterns[index] = try self.lowerPatternAtTypeCollectingLists(pattern_id, elem_ty, &sub_checks_per_elem[index]);
            literal_guards_per_elem[index] = try self.drainPatternLiteralGuards(guards_start);
        }

        var rest_pat: ?Ast.PatId = null;
        var rest_value: ?Ast.ExprId = null;
        var rest_sub_checks = std.ArrayList(CollectedListPattern).empty;
        defer rest_sub_checks.deinit(self.allocator);
        var rest_literal_guards: []PatternLiteralGuard = &.{};
        defer self.allocator.free(rest_literal_guards);
        if (list.rest) |rest| {
            if (rest.pattern) |rest_pattern| {
                const list_len = len orelse try self.builder.lowLevelExpr(.list_len, &.{scrutinee}, u64_ty);
                const fixed_count = try self.builder.intLiteralExpr(list.patterns.len, u64_ty);
                const rest_len = try self.builder.lowLevelExpr(.num_minus, &.{ list_len, fixed_count }, u64_ty);
                const rest_start = try self.builder.intLiteralExpr(rest.index, u64_ty);
                const range = try self.sublistRangeExpr(rest_start, rest_len, u64_ty);
                rest_value = try self.builder.lowLevelExpr(.list_sublist, &.{ scrutinee, range }, scrutinee_ty);
                const guards_start = self.pattern_literal_guards.items.len;
                rest_pat = try self.lowerPatternAtTypeCollectingLists(rest_pattern, scrutinee_ty, &rest_sub_checks);
                rest_literal_guards = try self.drainPatternLiteralGuards(guards_start);
            }
        }

        var success = body;

        var index = patterns.len;
        while (index > 0) {
            index -= 1;
            var elem_success = success;
            var sub_index = sub_checks_per_elem[index].items.len;
            while (sub_index > 0) {
                sub_index -= 1;
                const sub = sub_checks_per_elem[index].items[sub_index];
                const sub_scrut = try self.builder.localExpr(sub.local, sub.ty);
                elem_success = try self.applyListCheck(sub_scrut, sub.ty, sub, elem_success, fallback, output_ty);
            }
            elem_success = try self.applyPatternLiteralGuards(literal_guards_per_elem[index], elem_success, fallback, output_ty);
            success = try self.wrapPatternMatch(values[index], elem_ty, patterns[index], elem_success, fallback, output_ty);
        }

        if (rest_pat) |pat| {
            var rest_success = success;
            var sub_index = rest_sub_checks.items.len;
            while (sub_index > 0) {
                sub_index -= 1;
                const sub = rest_sub_checks.items[sub_index];
                const sub_scrut = try self.builder.localExpr(sub.local, sub.ty);
                rest_success = try self.applyListCheck(sub_scrut, sub.ty, sub, rest_success, fallback, output_ty);
            }
            rest_success = try self.applyPatternLiteralGuards(rest_literal_guards, rest_success, fallback, output_ty);
            success = try self.wrapPatternMatch(
                rest_value orelse Common.invariant("list rest pattern had no lowered rest value"),
                scrutinee_ty,
                pat,
                rest_success,
                fallback,
                output_ty,
            );
        }

        const cond = try self.listPatternCondition(scrutinee, list);
        return try self.builder.ifExpr(cond, success, fallback, output_ty);
    }

    fn preRegisterPatternBinders(
        self: *BodyContext,
        pattern_id: checked.CheckedPatternId,
        ty: Type.TypeId,
    ) Allocator.Error!void {
        const pattern = self.view.bodies.pattern(pattern_id);
        switch (pattern.data) {
            .assign => |binder| {
                if (self.binders.get(binder) == null) {
                    const local = try self.builder.program.addLocalWithBinder(self.builder.symbols.fresh(), ty, binder);
                    try bindLocalName(self.builder.program, self.view, local, binder);
                    try self.binders.put(binder, local);
                }
            },
            .as => |as| {
                if (self.binders.get(as.binder) == null) {
                    const local = try self.builder.program.addLocalWithBinder(self.builder.symbols.fresh(), ty, as.binder);
                    try bindLocalName(self.builder.program, self.view, local, as.binder);
                    try self.binders.put(as.binder, local);
                }
                try self.preRegisterPatternBinders(as.pattern, ty);
            },
            .applied_tag => |tag| {
                const name = try self.builder.tagName(self.view, tag.name);
                const payload_tys = self.builder.tagPayloadTypes(ty, name);
                if (tag.args.len != payload_tys.len) Common.invariant("pattern arity differs from concrete checked type");
                for (tag.args, payload_tys) |arg, arg_ty| {
                    try self.preRegisterPatternBinders(arg, arg_ty);
                }
            },
            .nominal => |nominal| {
                try self.preRegisterPatternBinders(nominal.backing_pattern, self.builder.namedBackingType(ty) orelse ty);
            },
            .record_destructure => |destructs| {
                for (destructs) |destruct| {
                    switch (destruct.kind) {
                        .required, .sub_pattern => |child| {
                            const name = try self.builder.recordFieldName(self.view, destruct.label);
                            const child_ty = self.builder.recordFieldType(ty, name);
                            try self.preRegisterPatternBinders(child, child_ty);
                        },
                        .rest => |rest_pattern| {
                            if (!self.patternIsIgnored(rest_pattern)) {
                                Common.invariant("record rest pattern must be lowered to explicit rest-record construction before Monotype output");
                            }
                        },
                    }
                }
            },
            .list => |list| {
                const elem_ty = self.constListElemType(ty);
                for (list.patterns) |child| {
                    try self.preRegisterPatternBinders(child, elem_ty);
                }
                if (list.rest) |rest| {
                    if (rest.pattern) |rest_pattern| {
                        try self.preRegisterPatternBinders(rest_pattern, ty);
                    }
                }
            },
            .tuple => |items| {
                const item_tys = self.builder.tupleItemTypes(ty);
                if (items.len != item_tys.len) Common.invariant("pattern arity differs from concrete checked type");
                for (items, item_tys) |item, item_ty| {
                    try self.preRegisterPatternBinders(item, item_ty);
                }
            },
            .str_interpolation => |str| {
                for (str.steps) |step| {
                    if (step.capture) |capture| {
                        try self.preRegisterPatternBinders(capture, ty);
                    }
                }
            },
            .pending,
            .num_literal,
            .small_dec_literal,
            .dec_literal,
            .frac_f32_literal,
            .frac_f64_literal,
            .str_literal,
            .underscore,
            .runtime_error,
            => {},
        }
    }

    fn lowerPatternAtTypeCollectingLists(
        self: *BodyContext,
        pattern_id: checked.CheckedPatternId,
        ty: Type.TypeId,
        checks_out: *std.ArrayList(CollectedListPattern),
    ) Allocator.Error!Ast.PatId {
        const pattern = self.view.bodies.pattern(pattern_id);
        try self.constrainTypeToMono(pattern.ty, ty);
        const data: Ast.PatData = switch (pattern.data) {
            .pending,
            .runtime_error,
            => Common.invariant("non-runtime checked pattern reached Monotype lowering"),
            .assign => |binder| blk: {
                const local = if (self.binders.get(binder)) |existing| existing else inner: {
                    const new_local = try self.builder.program.addLocalWithBinder(self.builder.symbols.fresh(), ty, binder);
                    try bindLocalName(self.builder.program, self.view, new_local, binder);
                    try self.binders.put(binder, new_local);
                    break :inner new_local;
                };
                break :blk .{ .bind = local };
            },
            .as => |as| blk: {
                const local = if (self.binders.get(as.binder)) |existing| existing else inner: {
                    const new_local = try self.builder.program.addLocalWithBinder(self.builder.symbols.fresh(), ty, as.binder);
                    try bindLocalName(self.builder.program, self.view, new_local, as.binder);
                    try self.binders.put(as.binder, new_local);
                    break :inner new_local;
                };
                break :blk .{ .as = .{
                    .pattern = try self.lowerPatternAtTypeCollectingLists(as.pattern, ty, checks_out),
                    .local = local,
                } };
            },
            .applied_tag => |tag| try self.lowerTagPatternCollectingLists(tag, ty, checks_out),
            .nominal => |nominal| .{ .nominal = try self.lowerPatternAtTypeCollectingLists(nominal.backing_pattern, self.builder.namedBackingType(ty) orelse ty, checks_out) },
            .record_destructure => |destructs| try self.lowerRecordPatternCollectingLists(destructs, ty, checks_out),
            .list => |list| blk: {
                const local = try self.builder.program.addLocal(self.builder.symbols.fresh(), ty);
                try checks_out.append(self.allocator, .{
                    .local = local,
                    .ty = ty,
                    .patterns = list.patterns,
                    .rest = list.rest,
                });
                break :blk .{ .bind = local };
            },
            .tuple => |items| .{ .tuple = try self.lowerPatternSpanAtTypesCollectingLists(items, self.builder.tupleItemTypes(ty), checks_out) },
            .num_literal => |num| if (num.conversion) |conversion|
                try self.bindLiteralGuardPattern(conversion, ty)
            else
                self.lowerNumPattern(num.value, ty),
            .small_dec_literal => |dec| if (dec.conversion) |conversion|
                try self.bindLiteralGuardPattern(conversion, ty)
            else
                Common.invariant("small decimal pattern reached Monotype after numeric finalization"),
            .dec_literal => |dec| if (dec.conversion) |conversion|
                try self.bindLiteralGuardPattern(conversion, ty)
            else
                .{ .dec_lit = dec.value },
            .frac_f32_literal => |value| .{ .frac_f32_lit = value },
            .frac_f64_literal => |value| .{ .frac_f64_lit = value },
            .str_literal => |str| if (str.conversion) |conversion|
                try self.bindLiteralGuardPattern(conversion, ty)
            else
                .{ .str_lit = try self.lowerStringLiteral(str.literal) },
            .str_interpolation => |str| try self.lowerStrPatternCollectingLists(str, ty, checks_out),
            .underscore => .wildcard,
        };
        return try self.builder.program.addPat(.{ .ty = ty, .data = data });
    }

    fn lowerStrPatternCollectingLists(
        self: *BodyContext,
        str: anytype,
        ty: Type.TypeId,
        checks_out: *std.ArrayList(CollectedListPattern),
    ) Allocator.Error!Ast.PatData {
        const steps = try self.allocator.alloc(Ast.StrPatternStep, str.steps.len);
        defer self.allocator.free(steps);

        for (str.steps, 0..) |step, i| {
            steps[i] = .{
                .capture = if (step.capture) |capture| try self.lowerPatternAtTypeCollectingLists(capture, ty, checks_out) else null,
                .delimiter = try self.lowerStringLiteral(step.delimiter),
            };
        }

        return .{ .str_pattern = .{
            .prefix = try self.lowerStringLiteral(str.prefix),
            .steps = try self.builder.program.addStrPatternStepSpan(steps),
            .end = switch (str.end) {
                .exact => .exact,
                .tail => .tail,
            },
        } };
    }

    fn lowerPatternSpanAtTypesCollectingLists(
        self: *BodyContext,
        checked_patterns: []const checked.CheckedPatternId,
        tys: []const Type.TypeId,
        checks_out: *std.ArrayList(CollectedListPattern),
    ) Allocator.Error!Ast.Span(Ast.PatId) {
        if (checked_patterns.len != tys.len) Common.invariant("pattern arity differs from concrete checked type");
        const lowered = try self.allocator.alloc(Ast.PatId, checked_patterns.len);
        defer self.allocator.free(lowered);
        for (checked_patterns, tys, 0..) |child, child_ty, i| {
            lowered[i] = try self.lowerPatternAtTypeCollectingLists(child, child_ty, checks_out);
        }
        return try self.builder.program.addPatSpan(lowered);
    }

    fn lowerTagPatternCollectingLists(
        self: *BodyContext,
        tag: anytype,
        ty: Type.TypeId,
        checks_out: *std.ArrayList(CollectedListPattern),
    ) Allocator.Error!Ast.PatData {
        const name = try self.builder.tagName(self.view, tag.name);
        return .{ .tag = .{
            .name = name,
            .payloads = try self.lowerPatternSpanAtTypesCollectingLists(tag.args, self.builder.tagPayloadTypes(ty, name), checks_out),
        } };
    }

    fn lowerRecordPatternCollectingLists(
        self: *BodyContext,
        destructs: []const checked.CheckedRecordDestruct,
        ty: Type.TypeId,
        checks_out: *std.ArrayList(CollectedListPattern),
    ) Allocator.Error!Ast.PatData {
        var lowered = std.ArrayList(Ast.RecordDestruct).empty;
        defer lowered.deinit(self.allocator);
        for (destructs) |destruct| {
            const child = switch (destruct.kind) {
                .required => |pattern| pattern,
                .sub_pattern => |pattern| pattern,
                .rest => |pattern| {
                    if (self.patternIsIgnored(pattern)) continue;
                    Common.invariant("record rest pattern must be lowered to explicit rest-record construction before Monotype output");
                },
            };
            const name = try self.builder.recordFieldName(self.view, destruct.label);
            const child_ty = switch (destruct.kind) {
                .required, .sub_pattern => self.builder.recordFieldType(ty, name),
                .rest => unreachable,
            };
            try lowered.append(self.allocator, .{
                .name = name,
                .pattern = try self.lowerPatternAtTypeCollectingLists(child, child_ty, checks_out),
            });
        }
        return .{ .record = try self.builder.program.addRecordDestructSpan(lowered.items) };
    }

    fn wrapPatternMatch(
        self: *BodyContext,
        value: Ast.ExprId,
        value_ty: Type.TypeId,
        pattern: Ast.PatId,
        success: Ast.ExprId,
        miss: Ast.ExprId,
        result_ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        const wildcard = try self.builder.program.addPat(.{ .ty = value_ty, .data = .wildcard });
        const branches = [_]Ast.Branch{
            .{ .pat = pattern, .body = success },
            .{ .pat = wildcard, .body = miss },
        };
        return try self.builder.program.addExpr(.{ .ty = result_ty, .data = .{ .match_ = .{
            .scrutinee = value,
            .branches = try self.builder.program.addBranchSpan(&branches),
        } } });
    }

    fn lowerBindingContinuation(
        self: *BodyContext,
        continuation: BindingContinuation,
        result_ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        return switch (continuation) {
            .expr => |expr| expr,
            .checked_expr => |expr| try self.lowerExprAtType(expr, result_ty),
            .materialized_args => |args| blk: {
                if (args.index >= args.args.len) break :blk try self.lowerExprAtType(args.body, result_ty);
                const arg = args.args[args.index];
                const miss = try self.runtimeCrashExpr(result_ty, "pattern match failed");
                break :blk try self.lowerMaterializedPatternThen(
                    arg.pattern,
                    arg.value,
                    arg.ty,
                    result_ty,
                    .{ .materialized_args = .{
                        .args = args.args,
                        .index = args.index + 1,
                        .body = args.body,
                    } },
                    miss,
                );
            },
            .iterator_body => |body| try self.lowerIteratorBodyThenContinue(
                body.body,
                body.result_ty,
                body.rest_expr,
                body.carries,
            ),
        };
    }

    fn lowerMaterializedPatternValueThen(
        self: *BodyContext,
        pattern_id: checked.CheckedPatternId,
        value: Ast.ExprId,
        value_ty: Type.TypeId,
        result_ty: Type.TypeId,
        continuation: BindingContinuation,
        miss: Ast.ExprId,
    ) Allocator.Error!Ast.ExprId {
        if (!self.patternNeedsExplicitBinding(pattern_id)) {
            const pat = try self.lowerPatternAtType(pattern_id, value_ty);
            const rest = try self.lowerBindingContinuation(continuation, result_ty);
            return try self.builder.program.addExpr(.{ .ty = result_ty, .data = .{ .let_ = .{
                .bind = pat,
                .value = value,
                .rest = rest,
            } } });
        }

        const local = try self.builder.program.addLocal(self.builder.symbols.fresh(), value_ty);
        const local_expr = try self.builder.localExpr(local, value_ty);
        const rest = try self.lowerMaterializedPatternThen(pattern_id, local_expr, value_ty, result_ty, continuation, miss);
        return try self.builder.program.addExpr(.{ .ty = result_ty, .data = .{ .let_ = .{
            .bind = try self.builder.bindPat(local, value_ty),
            .value = value,
            .rest = rest,
        } } });
    }

    fn lowerMaterializedPatternThen(
        self: *BodyContext,
        pattern_id: checked.CheckedPatternId,
        value: Ast.ExprId,
        value_ty: Type.TypeId,
        result_ty: Type.TypeId,
        continuation: BindingContinuation,
        miss: Ast.ExprId,
    ) Allocator.Error!Ast.ExprId {
        const pattern = self.view.bodies.pattern(pattern_id);
        return switch (pattern.data) {
            .list => |list| try self.lowerListPatternBindingThen(value, value_ty, list, result_ty, continuation, miss),
            .record_destructure => |destructs| if (self.recordDestructsNeedExplicitRest(destructs))
                try self.lowerRecordRestPatternBindingThen(value, value_ty, destructs, result_ty, continuation)
            else
                try self.lowerMaterializedPatternValueThen(pattern_id, value, value_ty, result_ty, continuation, miss),
            .assign,
            .as,
            .applied_tag,
            .nominal,
            .tuple,
            .num_literal,
            .small_dec_literal,
            .dec_literal,
            .frac_f32_literal,
            .frac_f64_literal,
            .str_literal,
            .str_interpolation,
            .underscore,
            .pending,
            .runtime_error,
            => try self.lowerMaterializedPatternValueThen(pattern_id, value, value_ty, result_ty, continuation, miss),
        };
    }

    fn lowerListPatternBindingThen(
        self: *BodyContext,
        value: Ast.ExprId,
        value_ty: Type.TypeId,
        list: anytype,
        result_ty: Type.TypeId,
        continuation: BindingContinuation,
        miss: Ast.ExprId,
    ) Allocator.Error!Ast.ExprId {
        const success = try self.lowerListPatternBindingSuccess(value, value_ty, list, result_ty, continuation, miss);
        const cond = try self.listPatternCondition(value, list);
        return try self.builder.ifExpr(cond, success, miss, result_ty);
    }

    fn listPatternItemIndex(
        self: *BodyContext,
        index: usize,
        pattern_count: usize,
        rest: ?checked.CheckedListRestPattern,
        len: ?Ast.ExprId,
        u64_ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        if (rest) |rest_info| {
            if (index >= rest_info.index) {
                const list_len = len orelse Common.invariant("list pattern trailing item index required list length");
                const trailing_count = try self.builder.intLiteralExpr(pattern_count - index, u64_ty);
                return try self.builder.lowLevelExpr(.num_minus, &.{ list_len, trailing_count }, u64_ty);
            }
        }
        return try self.builder.intLiteralExpr(index, u64_ty);
    }

    fn sublistRangeExpr(
        self: *BodyContext,
        start: Ast.ExprId,
        len: Ast.ExprId,
        u64_ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        const len_name = try self.builder.program.names.internRecordFieldLabel("len");
        const start_name = try self.builder.program.names.internRecordFieldLabel("start");
        const fields = [_]Type.Field{
            .{ .name = len_name, .ty = u64_ty },
            .{ .name = start_name, .ty = u64_ty },
        };
        const ty = try self.builder.program.types.add(.{ .record = try self.builder.program.types.addFields(&fields) });
        const exprs = [_]Ast.FieldExpr{
            .{ .name = len_name, .value = len },
            .{ .name = start_name, .value = start },
        };
        return try self.builder.program.addExpr(.{ .ty = ty, .data = .{ .record = try self.builder.program.addFieldExprSpan(&exprs) } });
    }

    fn lowerListPatternBindingSuccess(
        self: *BodyContext,
        value: Ast.ExprId,
        value_ty: Type.TypeId,
        list: anytype,
        result_ty: Type.TypeId,
        continuation: BindingContinuation,
        miss: Ast.ExprId,
    ) Allocator.Error!Ast.ExprId {
        const elem_ty = self.constListElemType(value_ty);
        const values = try self.allocator.alloc(Ast.ExprId, list.patterns.len);
        defer self.allocator.free(values);
        const patterns = try self.allocator.alloc(Ast.PatId, list.patterns.len);
        defer self.allocator.free(patterns);
        const u64_ty = try self.builder.primitiveType(.u64);
        const needs_len = if (list.rest) |rest| rest.index < list.patterns.len or rest.pattern != null else false;
        const len = if (needs_len) try self.builder.lowLevelExpr(.list_len, &.{value}, u64_ty) else null;

        for (list.patterns, 0..) |pattern_id, index| {
            const item_index = try self.listPatternItemIndex(index, list.patterns.len, list.rest, len, u64_ty);
            values[index] = try self.builder.lowLevelExpr(.list_get_unsafe, &.{ value, item_index }, elem_ty);
            patterns[index] = try self.lowerPatternAtType(pattern_id, elem_ty);
        }

        var rest_pat: ?Ast.PatId = null;
        var rest_value: ?Ast.ExprId = null;
        if (list.rest) |rest| {
            if (rest.pattern) |rest_pattern| {
                const list_len = len orelse try self.builder.lowLevelExpr(.list_len, &.{value}, u64_ty);
                const fixed_count = try self.builder.intLiteralExpr(list.patterns.len, u64_ty);
                const rest_len = try self.builder.lowLevelExpr(.num_minus, &.{ list_len, fixed_count }, u64_ty);
                const rest_start = try self.builder.intLiteralExpr(rest.index, u64_ty);
                const range = try self.sublistRangeExpr(rest_start, rest_len, u64_ty);
                rest_value = try self.builder.lowLevelExpr(.list_sublist, &.{ value, range }, value_ty);
                rest_pat = try self.lowerPatternAtType(rest_pattern, value_ty);
            }
        }

        var success = try self.lowerBindingContinuation(continuation, result_ty);
        var index = patterns.len;
        while (index > 0) {
            index -= 1;
            success = try self.wrapPatternMatch(values[index], elem_ty, patterns[index], success, miss, result_ty);
        }
        if (rest_pat) |pat| {
            success = try self.wrapPatternMatch(
                rest_value orelse Common.invariant("list rest pattern had no lowered rest value"),
                value_ty,
                pat,
                success,
                miss,
                result_ty,
            );
        }
        return success;
    }

    fn lowerRecordRestPatternBindingThen(
        self: *BodyContext,
        value: Ast.ExprId,
        value_ty: Type.TypeId,
        destructs: []const checked.CheckedRecordDestruct,
        result_ty: Type.TypeId,
        continuation: BindingContinuation,
    ) Allocator.Error!Ast.ExprId {
        var success = try self.lowerBindingContinuation(continuation, result_ty);
        var i = destructs.len;
        while (i > 0) {
            i -= 1;
            const destruct = destructs[i];
            switch (destruct.kind) {
                .required, .sub_pattern => |child| {
                    const name = try self.builder.recordFieldName(self.view, destruct.label);
                    const field_ty = self.builder.recordFieldType(value_ty, name);
                    const field_value = try self.builder.program.addExpr(.{
                        .ty = field_ty,
                        .data = .{ .field_access = .{
                            .receiver = value,
                            .field = name,
                        } },
                    });
                    const pat = try self.lowerPatternAtType(child, field_ty);
                    success = try self.builder.program.addExpr(.{ .ty = result_ty, .data = .{ .let_ = .{
                        .bind = pat,
                        .value = field_value,
                        .rest = success,
                    } } });
                },
                .rest => |child| {
                    if (self.patternIsIgnored(child)) continue;
                    const rest_ty = try self.lowerType(self.view.bodies.pattern(child).ty);
                    const rest_value = try self.lowerRecordRestValue(value, rest_ty);
                    const pat = try self.lowerPatternAtType(child, rest_ty);
                    success = try self.builder.program.addExpr(.{ .ty = result_ty, .data = .{ .let_ = .{
                        .bind = pat,
                        .value = rest_value,
                        .rest = success,
                    } } });
                },
            }
        }
        return success;
    }

    fn lowerRecordRestValue(
        self: *BodyContext,
        value: Ast.ExprId,
        rest_ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        const rest_fields = self.constRecordFields(rest_ty);
        const fields = try self.allocator.alloc(Ast.FieldExpr, rest_fields.len);
        defer self.allocator.free(fields);
        for (rest_fields, 0..) |field, i| {
            fields[i] = .{
                .name = field.name,
                .value = try self.builder.program.addExpr(.{
                    .ty = field.ty,
                    .data = .{ .field_access = .{
                        .receiver = value,
                        .field = field.name,
                    } },
                }),
            };
        }
        return try self.builder.program.addExpr(.{ .ty = rest_ty, .data = .{
            .record = try self.builder.program.addFieldExprSpan(fields),
        } });
    }

    fn runtimeCrashExpr(self: *BodyContext, ty: Type.TypeId, message: []const u8) Allocator.Error!Ast.ExprId {
        return try self.builder.program.addExpr(.{
            .ty = ty,
            .data = .{ .crash = try self.builder.program.addStringLiteral(message) },
        });
    }

    fn comptimeExhaustivenessFailedExpr(self: *BodyContext, ty: Type.TypeId, site: Ast.ComptimeSiteId) Allocator.Error!Ast.ExprId {
        return try self.builder.program.addExpr(.{
            .ty = ty,
            .data = .{ .comptime_exhaustiveness_failed = site },
        });
    }

    fn addComptimeSite(
        self: *BodyContext,
        kind: Ast.ComptimeSiteKind,
        region: base.Region,
        checked_site: ?checked.CheckedExhaustivenessSiteId,
        branch_regions: []const base.Region,
    ) Allocator.Error!Ast.ComptimeSiteId {
        return try self.builder.program.addComptimeSite(kind, region, checked_site, branch_regions);
    }

    fn wrapComptimeBranch(
        self: *BodyContext,
        site: ?Ast.ComptimeSiteId,
        branch_index: usize,
        body: Ast.ExprId,
    ) Allocator.Error!Ast.ExprId {
        const actual_site = site orelse return body;
        const ty = self.builder.program.exprs.items[@intFromEnum(body)].ty;
        return try self.builder.program.addExpr(.{ .ty = ty, .data = .{ .comptime_branch_taken = .{
            .site = actual_site,
            .branch_index = @intCast(branch_index),
            .body = body,
        } } });
    }

    fn matchComptimeSite(self: *BodyContext, expr_id: checked.CheckedExprId, match: anytype) Allocator.Error!?Ast.ComptimeSiteId {
        if (!self.inComptimeExhaustivenessContext()) return null;
        if (match.skip_exhaustiveness) return null;
        const branch_regions = try self.matchBranchRegions(match.branches);
        defer self.allocator.free(branch_regions);
        const expr = self.view.bodies.expr(expr_id);
        const site_kind: Ast.ComptimeSiteKind = switch (match.comptime_site_kind) {
            .match => .match,
            .destructure => .destructure,
        };
        const checked_site = switch (match.comptime_site_kind) {
            .match => self.view.exhaustiveness_sites.lookupByMatchExpr(expr_id),
            .destructure => blk: {
                if (match.branches.len != 1) break :blk null;
                const patterns = match.branches[0].patternsSlice(self.view.bodies);
                if (patterns.len != 1) break :blk null;
                break :blk self.view.exhaustiveness_sites.lookupByDestructurePattern(patterns[0].pattern);
            },
        };
        return try self.addComptimeSite(site_kind, expr.source_region, checked_site, branch_regions);
    }

    fn matchBranchRegions(self: *BodyContext, branches: []const checked.CheckedMatchBranch) Allocator.Error![]base.Region {
        const regions = try self.allocator.alloc(base.Region, branchCount(branches));
        var index: usize = 0;
        for (branches) |branch| {
            for (branch.patternsSlice(self.view.bodies)) |pattern| {
                regions[index] = self.view.bodies.pattern(pattern.pattern).source_region;
                index += 1;
            }
        }
        return regions;
    }

    fn ifComptimeSite(self: *BodyContext, expr_id: checked.CheckedExprId, if_: anytype) Allocator.Error!?Ast.ComptimeSiteId {
        if (!self.inComptimeExhaustivenessContext()) return null;
        if (!if_.warn_unused_branches) return null;
        const branch_regions = try self.ifBranchRegions(if_);
        defer self.allocator.free(branch_regions);
        const expr = self.view.bodies.expr(expr_id);
        return try self.addComptimeSite(.if_, expr.source_region, null, branch_regions);
    }

    fn ifBranchRegions(self: *BodyContext, if_: anytype) Allocator.Error![]base.Region {
        const regions = try self.allocator.alloc(base.Region, if_.branches.len + 1);
        for (if_.branches, 0..) |branch, index| {
            regions[index] = self.view.bodies.expr(branch.cond).source_region;
        }
        regions[if_.branches.len] = self.view.bodies.expr(if_.final_else).source_region;
        return regions;
    }

    fn lowerMatch(self: *BodyContext, match: anytype, output: MatchOutput, comptime_site: ?Ast.ComptimeSiteId) Allocator.Error!Ast.ExprData {
        const scrutinee_ty = try self.matchScrutineeType(match);
        const scrutinee = try self.lowerExprAtType(match.cond, scrutinee_ty);
        const branches = try self.allocator.alloc(Ast.Branch, branchCount(match.branches));
        defer self.allocator.free(branches);
        var index: usize = 0;
        for (match.branches) |branch| {
            for (branch.patternsSlice(self.view.bodies)) |pattern| {
                var branch_ctx = try self.childContext(self.current_fn_key);
                defer branch_ctx.deinit();
                var saved = std.ArrayList(BinderRestore).empty;
                defer saved.deinit(self.allocator);
                try branch_ctx.saveMatchPatternBinders(pattern, &saved);
                defer branch_ctx.restoreBinders(saved.items);

                const pat = try branch_ctx.lowerPatternAtType(pattern.pattern, scrutinee_ty);
                try branch_ctx.applyAlternativeBinderRemaps(pattern.binderRemapsSlice(self.view.bodies));
                const user_guard = if (branch.guard) |guard_expr| try branch_ctx.lowerExpr(guard_expr) else null;
                const guard = try branch_ctx.conjoinPatternLiteralGuards(user_guard);
                const body = try branch_ctx.wrapComptimeBranch(
                    comptime_site,
                    index,
                    try branch_ctx.lowerMatchBranchBody(branch.value, output),
                );
                branches[index] = .{
                    .pat = pat,
                    .guard = guard,
                    .body = body,
                };
                index += 1;
            }
        }
        return .{ .match_ = .{
            .scrutinee = scrutinee,
            .branches = try self.builder.program.addBranchSpan(branches),
            .comptime_site = comptime_site,
        } };
    }

    fn matchScrutineeType(self: *BodyContext, match: anytype) Allocator.Error!Type.TypeId {
        return try self.lowerExprType(match.cond);
    }

    fn saveMatchPatternBinders(
        self: *BodyContext,
        pattern: checked.CheckedMatchBranchPattern,
        saved: *std.ArrayList(BinderRestore),
    ) Allocator.Error!void {
        try self.savePatternBinders(pattern.pattern, saved);
        for (pattern.binderRemapsSlice(self.view.bodies)) |remap| {
            try self.saveBinder(remap.candidate_binder, saved);
            try self.saveBinder(remap.representative_binder, saved);
        }
    }

    fn savePatternBinders(
        self: *BodyContext,
        pattern_id: checked.CheckedPatternId,
        saved: *std.ArrayList(BinderRestore),
    ) Allocator.Error!void {
        const pattern = self.view.bodies.pattern(pattern_id);
        switch (pattern.data) {
            .assign => |binder| try self.saveBinder(binder, saved),
            .as => |as| {
                try self.savePatternBinders(as.pattern, saved);
                try self.saveBinder(as.binder, saved);
            },
            .applied_tag => |tag| for (tag.args) |arg| try self.savePatternBinders(arg, saved),
            .nominal => |nominal| try self.savePatternBinders(nominal.backing_pattern, saved),
            .record_destructure => |destructs| {
                for (destructs) |destruct| {
                    const child = switch (destruct.kind) {
                        .required => |child_pattern| child_pattern,
                        .sub_pattern => |child_pattern| child_pattern,
                        .rest => |child_pattern| child_pattern,
                    };
                    try self.savePatternBinders(child, saved);
                }
            },
            .list => |list| {
                for (list.patterns) |child| try self.savePatternBinders(child, saved);
                if (list.rest) |rest| if (rest.pattern) |rest_pattern| try self.savePatternBinders(rest_pattern, saved);
            },
            .tuple => |items| for (items) |child| try self.savePatternBinders(child, saved),
            .str_interpolation => |str| {
                for (str.steps) |step| {
                    if (step.capture) |capture| try self.savePatternBinders(capture, saved);
                }
            },
            .pending,
            .num_literal,
            .small_dec_literal,
            .dec_literal,
            .frac_f32_literal,
            .frac_f64_literal,
            .str_literal,
            .underscore,
            .runtime_error,
            => {},
        }
    }

    fn saveBinder(
        self: *BodyContext,
        binder: checked.PatternBinderId,
        saved: *std.ArrayList(BinderRestore),
    ) Allocator.Error!void {
        for (saved.items) |item| {
            if (item.binder == binder) return;
        }
        try saved.append(self.allocator, .{
            .binder = binder,
            .previous = self.binders.get(binder),
        });
    }

    fn applyAlternativeBinderRemaps(
        self: *BodyContext,
        remaps: []const checked.CheckedAlternativeBinderRemap,
    ) Allocator.Error!void {
        for (remaps) |remap| {
            const local = self.binders.get(remap.candidate_binder) orelse
                Common.invariant("match alternative binder remap referenced an unbound candidate binder");
            try self.binders.put(remap.representative_binder, local);
        }
    }

    fn restoreBinders(self: *BodyContext, saved: []const BinderRestore) void {
        var index = saved.len;
        while (index > 0) {
            index -= 1;
            if (saved[index].previous) |previous| {
                self.binders.put(saved[index].binder, previous) catch |err| switch (err) {
                    error.OutOfMemory => Common.invariant("restoring a previously inserted binder cannot reallocate"),
                };
            } else {
                _ = self.binders.remove(saved[index].binder);
            }
        }
    }

    fn lowerIfExpr(
        self: *BodyContext,
        expr_id: checked.CheckedExprId,
        if_: anytype,
        result_ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        const comptime_site = try self.ifComptimeSite(expr_id, if_);
        const merge_binders = try self.stateMergeBinders(expr_id);
        defer self.allocator.free(merge_binders);

        if (merge_binders.len == 0) {
            return try self.builder.program.addExpr(.{
                .ty = result_ty,
                .data = try self.lowerIf(if_, result_ty, result_ty, &.{}, comptime_site),
            });
        }

        const state_ty = try self.stateResultType(merge_binders, result_ty);
        const state_expr = try self.builder.program.addExpr(.{
            .ty = state_ty,
            .data = try self.lowerIf(if_, result_ty, state_ty, merge_binders, comptime_site),
        });

        const pattern_items = try self.allocator.alloc(Ast.PatId, merge_binders.len + 1);
        defer self.allocator.free(pattern_items);
        for (merge_binders, 0..) |merge, i| {
            const local = try self.builder.program.addLocalWithBinder(self.builder.symbols.fresh(), merge.ty, merge.binder);
            try bindLocalName(self.builder.program, self.view, local, merge.binder);
            try self.binders.put(merge.binder, local);
            pattern_items[i] = try self.builder.program.addPat(.{ .ty = merge.ty, .data = .{ .bind = local } });
        }

        const result_local = try self.builder.program.addLocal(self.builder.symbols.fresh(), result_ty);
        pattern_items[merge_binders.len] = try self.builder.program.addPat(.{ .ty = result_ty, .data = .{ .bind = result_local } });
        const bind_pat = try self.builder.program.addPat(.{ .ty = state_ty, .data = .{ .tuple = try self.builder.program.addPatSpan(pattern_items) } });
        const rest = try self.builder.localExpr(result_local, result_ty);

        return try self.builder.program.addExpr(.{ .ty = result_ty, .data = .{ .let_ = .{
            .bind = bind_pat,
            .value = state_expr,
            .rest = rest,
        } } });
    }

    fn stateMergeBinders(self: *BodyContext, expr_id: checked.CheckedExprId) Allocator.Error![]MergeBinder {
        var reassigned = std.ArrayList(checked.PatternBinderId).empty;
        defer reassigned.deinit(self.allocator);
        try self.collectReassignedBindersInExpr(expr_id, &reassigned);

        var merge_binders = std.ArrayList(MergeBinder).empty;
        errdefer merge_binders.deinit(self.allocator);
        for (reassigned.items) |binder| {
            const before = self.binders.get(binder) orelse continue;
            const ty = self.builder.program.locals.items[@intFromEnum(before)].ty;
            try merge_binders.append(self.allocator, .{
                .binder = binder,
                .before = before,
                .ty = ty,
            });
        }
        return try merge_binders.toOwnedSlice(self.allocator);
    }

    fn stateResultType(
        self: *BodyContext,
        merge_binders: []const MergeBinder,
        result_ty: Type.TypeId,
    ) Allocator.Error!Type.TypeId {
        const tys = try self.allocator.alloc(Type.TypeId, merge_binders.len + 1);
        defer self.allocator.free(tys);
        for (merge_binders, 0..) |merge, i| tys[i] = merge.ty;
        tys[merge_binders.len] = result_ty;
        return try self.builder.program.types.add(.{ .tuple = try self.builder.program.types.addSpan(tys) });
    }

    fn stateOnlyType(
        self: *BodyContext,
        merge_binders: []const MergeBinder,
    ) Allocator.Error!Type.TypeId {
        if (merge_binders.len == 0) return try self.unitType();
        if (merge_binders.len == 1) return merge_binders[0].ty;

        const tys = try self.allocator.alloc(Type.TypeId, merge_binders.len);
        defer self.allocator.free(tys);
        for (merge_binders, 0..) |merge, i| tys[i] = merge.ty;
        return try self.builder.program.types.add(.{ .tuple = try self.builder.program.types.addSpan(tys) });
    }

    fn lowerIf(
        self: *BodyContext,
        if_: anytype,
        result_ty: Type.TypeId,
        branch_ty: Type.TypeId,
        merge_binders: []const MergeBinder,
        comptime_site: ?Ast.ComptimeSiteId,
    ) Allocator.Error!Ast.ExprData {
        const branches = try self.allocator.alloc(Ast.IfBranch, if_.branches.len);
        defer self.allocator.free(branches);
        for (if_.branches, 0..) |branch, i| {
            const cond = try self.lowerExpr(branch.cond);
            var branch_ctx = try self.childContext(self.current_fn_key);
            defer branch_ctx.deinit();
            branches[i] = .{
                .cond = cond,
                .body = try branch_ctx.wrapComptimeBranch(
                    comptime_site,
                    i,
                    try branch_ctx.lowerIfBranchBody(branch.body, result_ty, branch_ty, merge_binders),
                ),
            };
        }
        var else_ctx = try self.childContext(self.current_fn_key);
        defer else_ctx.deinit();
        return .{ .if_ = .{
            .branches = try self.builder.program.addIfBranchSpan(branches),
            .final_else = try else_ctx.wrapComptimeBranch(
                comptime_site,
                if_.branches.len,
                try else_ctx.lowerIfBranchBody(if_.final_else, result_ty, branch_ty, merge_binders),
            ),
        } };
    }

    fn lowerIfBranchBody(
        self: *BodyContext,
        body: checked.CheckedExprId,
        result_ty: Type.TypeId,
        branch_ty: Type.TypeId,
        merge_binders: []const MergeBinder,
    ) Allocator.Error!Ast.ExprId {
        if (merge_binders.len == 0) return try self.lowerBranchValueAtType(body, result_ty);

        return try self.lowerBodyThenStateResult(body, result_ty, branch_ty, merge_binders);
    }

    fn lowerIfStateOnly(
        self: *BodyContext,
        if_: anytype,
        state_ty: Type.TypeId,
        merge_binders: []const MergeBinder,
        comptime_site: ?Ast.ComptimeSiteId,
    ) Allocator.Error!Ast.ExprData {
        const branches = try self.allocator.alloc(Ast.IfBranch, if_.branches.len);
        defer self.allocator.free(branches);
        for (if_.branches, 0..) |branch, i| {
            const cond = try self.lowerExpr(branch.cond);
            var branch_ctx = try self.childContext(self.current_fn_key);
            defer branch_ctx.deinit();
            branches[i] = .{
                .cond = cond,
                .body = try branch_ctx.wrapComptimeBranch(
                    comptime_site,
                    i,
                    try branch_ctx.lowerBodyThenStateOnly(branch.body, state_ty, merge_binders),
                ),
            };
        }
        var else_ctx = try self.childContext(self.current_fn_key);
        defer else_ctx.deinit();
        return .{ .if_ = .{
            .branches = try self.builder.program.addIfBranchSpan(branches),
            .final_else = try else_ctx.wrapComptimeBranch(
                comptime_site,
                if_.branches.len,
                try else_ctx.lowerBodyThenStateOnly(if_.final_else, state_ty, merge_binders),
            ),
        } };
    }

    fn lowerBodyThenStateResult(
        self: *BodyContext,
        body: checked.CheckedExprId,
        result_ty: Type.TypeId,
        state_ty: Type.TypeId,
        merge_binders: []const MergeBinder,
    ) Allocator.Error!Ast.ExprId {
        if (self.checkedExprDiverges(body)) return try self.lowerDivergentExprAtType(body, state_ty);

        const checked_body = self.view.bodies.expr(body);
        switch (checked_body.data) {
            .block => |block| {
                const statements = try self.lowerBlockStatements(block.statements);
                defer self.allocator.free(statements.items);
                const value = if (statements.diverges)
                    try self.unreachableAfterDivergentStatementExpr(result_ty)
                else if (self.checkedExprDiverges(block.final_expr))
                    try self.lowerDivergentExprAtType(block.final_expr, result_ty)
                else
                    try self.lowerExprAtType(block.final_expr, result_ty);
                return try self.builder.program.addExpr(.{ .ty = state_ty, .data = .{ .block = .{
                    .statements = try self.builder.program.addStmtSpan(statements.items[0..statements.len]),
                    .final_expr = try self.stateResultAfterValue(state_ty, merge_binders, result_ty, value),
                } } });
            },
            else => {
                const value = try self.lowerExprAtType(body, result_ty);
                return try self.stateResultAfterValue(state_ty, merge_binders, result_ty, value);
            },
        }
    }

    fn lowerBodyThenStateOnly(
        self: *BodyContext,
        body: checked.CheckedExprId,
        state_ty: Type.TypeId,
        merge_binders: []const MergeBinder,
    ) Allocator.Error!Ast.ExprId {
        if (self.checkedExprDiverges(body)) return try self.lowerDivergentExprAtType(body, state_ty);

        const checked_body = self.view.bodies.expr(body);
        switch (checked_body.data) {
            .block => |block| {
                var statements = try self.lowerBlockStatements(block.statements);
                defer self.allocator.free(statements.items);
                if (!statements.diverges) {
                    const final_stmt = try self.builder.program.addStmt(.{ .expr = if (self.checkedExprDiverges(block.final_expr))
                        try self.lowerDivergentExprAtType(block.final_expr, try self.lowerType(checked_body.ty))
                    else
                        try self.lowerExpr(block.final_expr) });
                    try statements.append(self.allocator, final_stmt);
                }
                return try self.builder.program.addExpr(.{ .ty = state_ty, .data = .{ .block = .{
                    .statements = try self.builder.program.addStmtSpan(statements.items[0..statements.len]),
                    .final_expr = try self.stateOnlyTupleExpr(state_ty, merge_binders),
                } } });
            },
            else => {
                const value = try self.lowerExpr(body);
                const stmt = try self.builder.program.addStmt(.{ .expr = value });
                return try self.builder.program.addExpr(.{ .ty = state_ty, .data = .{ .block = .{
                    .statements = try self.builder.program.addStmtSpan(&[_]Ast.StmtId{stmt}),
                    .final_expr = try self.stateOnlyTupleExpr(state_ty, merge_binders),
                } } });
            },
        }
    }

    fn stateResultTupleExpr(
        self: *BodyContext,
        state_ty: Type.TypeId,
        merge_binders: []const MergeBinder,
        result: Ast.ExprId,
    ) Allocator.Error!Ast.ExprId {
        const items = try self.allocator.alloc(Ast.ExprId, merge_binders.len + 1);
        defer self.allocator.free(items);
        for (merge_binders, 0..) |merge, i| {
            const current = self.binders.get(merge.binder) orelse merge.before;
            items[i] = try self.builder.localExpr(current, merge.ty);
        }
        items[merge_binders.len] = result;
        return try self.builder.program.addExpr(.{ .ty = state_ty, .data = .{ .tuple = try self.builder.program.addExprSpan(items) } });
    }

    fn stateResultAfterValue(
        self: *BodyContext,
        state_ty: Type.TypeId,
        merge_binders: []const MergeBinder,
        result_ty: Type.TypeId,
        value: Ast.ExprId,
    ) Allocator.Error!Ast.ExprId {
        const local = try self.builder.program.addLocal(self.builder.symbols.fresh(), result_ty);
        const local_expr = try self.builder.localExpr(local, result_ty);
        return try self.builder.program.addExpr(.{ .ty = state_ty, .data = .{ .let_ = .{
            .bind = try self.builder.bindPat(local, result_ty),
            .value = value,
            .rest = try self.stateResultTupleExpr(state_ty, merge_binders, local_expr),
        } } });
    }

    fn stateOnlyTupleExpr(
        self: *BodyContext,
        state_ty: Type.TypeId,
        merge_binders: []const MergeBinder,
    ) Allocator.Error!Ast.ExprId {
        if (merge_binders.len == 0) {
            return try self.builder.program.addExpr(.{ .ty = state_ty, .data = .unit });
        }
        if (merge_binders.len == 1) {
            const merge = merge_binders[0];
            const current = self.binders.get(merge.binder) orelse merge.before;
            return try self.builder.localExpr(current, merge.ty);
        }

        const items = try self.allocator.alloc(Ast.ExprId, merge_binders.len);
        defer self.allocator.free(items);
        for (merge_binders, 0..) |merge, i| {
            const current = self.binders.get(merge.binder) orelse merge.before;
            items[i] = try self.builder.localExpr(current, merge.ty);
        }
        return try self.builder.program.addExpr(.{ .ty = state_ty, .data = .{ .tuple = try self.builder.program.addExprSpan(items) } });
    }

    fn stateOnlyPattern(
        self: *BodyContext,
        state_ty: Type.TypeId,
        merge_binders: []const MergeBinder,
    ) Allocator.Error!Ast.PatId {
        if (merge_binders.len == 0) return try self.builder.program.addPat(.{ .ty = state_ty, .data = .wildcard });
        if (merge_binders.len == 1) {
            const merge = merge_binders[0];
            const local = try self.builder.program.addLocalWithBinder(self.builder.symbols.fresh(), merge.ty, merge.binder);
            try bindLocalName(self.builder.program, self.view, local, merge.binder);
            try self.binders.put(merge.binder, local);
            return try self.builder.program.addPat(.{ .ty = state_ty, .data = .{ .bind = local } });
        }

        const pattern_items = try self.allocator.alloc(Ast.PatId, merge_binders.len);
        defer self.allocator.free(pattern_items);
        for (merge_binders, 0..) |merge, i| {
            const local = try self.builder.program.addLocalWithBinder(self.builder.symbols.fresh(), merge.ty, merge.binder);
            try bindLocalName(self.builder.program, self.view, local, merge.binder);
            try self.binders.put(merge.binder, local);
            pattern_items[i] = try self.builder.program.addPat(.{ .ty = merge.ty, .data = .{ .bind = local } });
        }
        return try self.builder.program.addPat(.{ .ty = state_ty, .data = .{ .tuple = try self.builder.program.addPatSpan(pattern_items) } });
    }

    fn lowerBlock(self: *BodyContext, block: anytype, ty: Type.TypeId) Allocator.Error!Ast.ExprData {
        const stmts = try self.lowerBlockStatements(block.statements);
        defer self.allocator.free(stmts.items);
        return .{ .block = .{
            .statements = try self.builder.program.addStmtSpan(stmts.items[0..stmts.len]),
            .final_expr = if (stmts.diverges)
                try self.unreachableAfterDivergentStatementExpr(ty)
            else if (self.checkedExprDiverges(block.final_expr))
                try self.lowerDivergentExprAtType(block.final_expr, ty)
            else
                try self.lowerExprAtType(block.final_expr, ty),
        } };
    }

    const LoweredStatements = struct {
        items: []Ast.StmtId,
        len: usize,
        diverges: bool,

        fn append(self: *LoweredStatements, allocator: Allocator, stmt: Ast.StmtId) Allocator.Error!void {
            if (self.len == self.items.len) {
                const grown = try allocator.realloc(self.items, self.items.len + 1);
                self.items = grown;
            }
            self.items[self.len] = stmt;
            self.len += 1;
        }
    };

    fn lowerBlockStatements(self: *BodyContext, checked_statements: []const checked.CheckedStatementId) Allocator.Error!LoweredStatements {
        const items = try self.allocator.alloc(Ast.StmtId, checked_statements.len);
        var lowered = LoweredStatements{
            .items = items,
            .len = 0,
            .diverges = false,
        };
        for (checked_statements) |statement| {
            if (!self.checkedStatementHasRuntimeEffect(statement)) continue;
            if (!try self.appendExpandedPatternStatement(statement, &lowered)) {
                try lowered.append(self.allocator, try self.lowerStatement(statement));
            }
            if (self.checkedStatementDiverges(statement)) lowered.diverges = true;
        }
        return lowered;
    }

    fn appendExpandedPatternStatement(
        self: *BodyContext,
        statement_id: checked.CheckedStatementId,
        lowered: *LoweredStatements,
    ) Allocator.Error!bool {
        const statement = self.view.bodies.statement(statement_id);
        const saved_loc = self.builder.program.current_loc;
        defer self.builder.program.current_loc = saved_loc;
        const saved_region = self.builder.program.current_region;
        defer self.builder.program.current_region = saved_region;
        self.builder.program.current_loc = try self.sourceLocFor(statement.source_region);
        self.builder.program.current_region = statement.source_region;
        const pattern, const expr = switch (statement.data) {
            .decl => |decl| blk: {
                if (self.statementValueIsLocalProc(decl.expr)) return false;
                break :blk .{ decl.pattern, decl.expr };
            },
            .var_ => |decl| .{ decl.pattern, decl.expr },
            .reassign => |decl| .{ decl.pattern, decl.expr },
            else => return false,
        };

        const pattern_data = self.view.bodies.pattern(pattern).data;
        const destructs = switch (pattern_data) {
            .record_destructure => |destructs| destructs,
            else => return false,
        };
        if (!self.recordDestructsNeedExplicitRest(destructs)) return false;

        const value = try self.lowerExpr(expr);
        const value_ty = self.builder.program.exprs.items[@intFromEnum(value)].ty;
        const source_local = try self.builder.program.addLocal(self.builder.symbols.fresh(), value_ty);
        try lowered.append(self.allocator, try self.builder.program.addStmt(.{ .let_ = .{
            .pat = try self.builder.bindPat(source_local, value_ty),
            .value = value,
        } }));

        const source_expr = try self.builder.localExpr(source_local, value_ty);
        const comptime_site = if (self.inComptimeExhaustivenessContext() and self.patternCanMiss(pattern))
            try self.addComptimeSite(.destructure, statement.source_region, self.view.exhaustiveness_sites.lookupByDestructurePattern(pattern), &.{})
        else
            null;
        try self.appendRecordRestPatternStatements(source_expr, value_ty, destructs, lowered, comptime_site);
        return true;
    }

    fn appendRecordRestPatternStatements(
        self: *BodyContext,
        value: Ast.ExprId,
        value_ty: Type.TypeId,
        destructs: []const checked.CheckedRecordDestruct,
        lowered: *LoweredStatements,
        comptime_site: ?Ast.ComptimeSiteId,
    ) Allocator.Error!void {
        for (destructs) |destruct| {
            switch (destruct.kind) {
                .required, .sub_pattern => |child| {
                    const name = try self.builder.recordFieldName(self.view, destruct.label);
                    const field_ty = self.builder.recordFieldType(value_ty, name);
                    const field_value = try self.builder.program.addExpr(.{
                        .ty = field_ty,
                        .data = .{ .field_access = .{
                            .receiver = value,
                            .field = name,
                        } },
                    });
                    try lowered.append(self.allocator, try self.builder.program.addStmt(.{ .let_ = .{
                        .pat = try self.lowerPatternAtType(child, field_ty),
                        .value = field_value,
                        .comptime_site = if (self.patternCanMiss(child)) comptime_site else null,
                    } }));
                },
                .rest => |child| {
                    if (self.patternIsIgnored(child)) continue;
                    const rest_ty = try self.lowerType(self.view.bodies.pattern(child).ty);
                    const rest_value = try self.lowerRecordRestValue(value, rest_ty);
                    try lowered.append(self.allocator, try self.builder.program.addStmt(.{ .let_ = .{
                        .pat = try self.lowerPatternAtType(child, rest_ty),
                        .value = rest_value,
                        .comptime_site = if (self.patternCanMiss(child)) comptime_site else null,
                    } }));
                },
            }
        }
    }

    fn checkedStatementHasRuntimeEffect(self: *BodyContext, statement_id: checked.CheckedStatementId) bool {
        const raw = @intFromEnum(statement_id);
        if (raw >= self.view.bodies.statementCount()) {
            Common.invariant("checked runtime statement filter referenced a missing statement");
        }
        return switch (self.view.bodies.statement(@enumFromInt(raw)).data) {
            .decl => |decl| self.view.hoisted_constants.lookupByPattern(decl.pattern) == null,
            .var_,
            .var_uninitialized,
            .reassign,
            .crash,
            .dbg,
            .expr,
            .expect,
            .for_,
            .while_,
            .infinite_loop,
            .breakable_loop,
            .break_,
            .return_,
            => true,
            .import_,
            .alias_decl,
            .nominal_decl,
            .type_anno,
            .type_var_alias,
            => false,
            .pending,
            .runtime_error,
            => Common.invariant("invalid checked statement reached Monotype runtime statement filter"),
        };
    }

    fn lowerDivergentExprAtType(
        self: *BodyContext,
        checked_expr_id: checked.CheckedExprId,
        ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        const checked_expr = self.view.bodies.expr(checked_expr_id);
        switch (checked_expr.data) {
            .match_ => |match| return try self.lowerMatchExpr(checked_expr_id, match, ty),
            .if_ => |if_| return try self.lowerIfExpr(checked_expr_id, if_, ty),
            else => {},
        }

        return try self.builder.program.addExpr(.{
            .ty = ty,
            .data = try self.lowerDivergentExprDataAtType(checked_expr_id, ty),
        });
    }

    fn exprIdAsDivergentData(self: *BodyContext, expr: Ast.ExprId) Allocator.Error!Ast.ExprData {
        return .{ .block = .{
            .statements = try self.builder.program.addStmtSpan(&[_]Ast.StmtId{}),
            .final_expr = expr,
        } };
    }

    fn lowerDivergentExprDataAtType(
        self: *BodyContext,
        checked_expr_id: checked.CheckedExprId,
        ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprData {
        const checked_expr = self.view.bodies.expr(checked_expr_id);
        return switch (checked_expr.data) {
            .block => |block| try self.lowerBlock(block, ty),
            .match_ => |match| try self.exprIdAsDivergentData(try self.lowerMatchExpr(checked_expr_id, match, ty)),
            .if_ => |if_| try self.exprIdAsDivergentData(try self.lowerIfExpr(checked_expr_id, if_, ty)),
            .ellipsis => .{ .crash = try self.builder.program.addStringLiteral("not implemented") },
            .crash => |msg| .{ .crash = try self.lowerStringLiteral(msg) },
            .expect_err => |expect_err| .{ .expect_err = .{
                .msg = try self.lowerExpectErrMessage(expect_err.expr, expect_err.snippet),
                .region = checked_expr.source_region,
            } },
            .break_ => try self.breakCurrentLoopExprData(),
            .return_ => |ret| .{ .return_ = try self.lowerExpr(ret.expr) },
            else => Common.invariant("checked expression was marked divergent but has no divergent lowering path"),
        };
    }

    fn unreachableAfterDivergentStatementExpr(self: *BodyContext, ty: Type.TypeId) Allocator.Error!Ast.ExprId {
        return try self.builder.program.addExpr(.{
            .ty = ty,
            .data = .{ .crash = try self.builder.program.addStringLiteral("reached code after checked control transfer") },
        });
    }

    fn checkedExprDiverges(self: *BodyContext, expr_id: checked.CheckedExprId) bool {
        const raw = @intFromEnum(expr_id);
        if (raw >= self.view.bodies.exprCount()) {
            Common.invariant("checked divergence referenced a missing expression");
        }
        return self.view.bodies.exprDiverges(expr_id);
    }

    fn checkedStatementDiverges(self: *BodyContext, statement_id: checked.CheckedStatementId) bool {
        const raw = @intFromEnum(statement_id);
        if (raw >= self.view.bodies.statementCount()) {
            Common.invariant("checked divergence referenced a missing statement");
        }
        return self.view.bodies.statementDiverges(statement_id);
    }

    const LoopCarry = struct {
        binder: checked.PatternBinderId,
        initial_local: Ast.LocalId,
        param_local: Ast.LocalId,
        ty: Type.TypeId,
    };

    const LoopContext = struct {
        result_ty: Type.TypeId,
        carries: []const LoopCarry,
    };

    fn lowerIteratorFor(
        self: *BodyContext,
        for_: anytype,
        result_ty: Type.TypeId,
        carries: []const LoopCarry,
    ) Allocator.Error!Ast.ExprData {
        const plan_id = for_.plan orelse Common.invariant("checked iterator for reached Monotype without an iterator dispatch plan");
        const plan = self.view.static_dispatch_plans.iterator_for_plans[@intFromEnum(plan_id)];

        const step = try self.iteratorStepShape(plan.step_ty);
        const initial_iterator = try self.lowerIteratorDispatch(plan.iter, null, null);
        const iterator_ty = self.builder.program.exprs.items[@intFromEnum(initial_iterator)].ty;
        try self.constrainTypeToMono(plan.iterator_ty, iterator_ty);
        try self.constrainTypeToMono(step.one_rest.ty, iterator_ty);
        try self.constrainTypeToMono(step.skip_rest.ty, iterator_ty);
        const item_ty = try self.lowerType(step.one_item.ty);
        try self.constrainTypeToMono(plan.item_ty, item_ty);
        const step_expected_ty = try self.lowerType(plan.step_ty);
        const iterator_local = try self.builder.program.addLocal(self.builder.symbols.fresh(), iterator_ty);
        const iterator_param = Ast.TypedLocal{ .local = iterator_local, .ty = iterator_ty };

        var saved = std.ArrayList(BinderRestore).empty;
        defer saved.deinit(self.allocator);
        for (carries) |carry| {
            try self.saveBinder(carry.binder, &saved);
            try self.binders.put(carry.binder, carry.param_local);
        }
        defer self.restoreBinders(saved.items);

        try self.pushLoopContext(result_ty, carries);
        defer self.popLoopContext();

        const step_expr = try self.lowerIteratorDispatch(plan.next, iterator_param, step_expected_ty);
        try self.constrainTypeToMono(plan.step_ty, self.builder.program.exprs.items[@intFromEnum(step_expr)].ty);
        const done_body = if (carries.len == 0)
            try self.builder.program.addExpr(.{ .ty = result_ty, .data = .{ .break_ = null } })
        else
            try self.builder.program.addExpr(.{ .ty = result_ty, .data = .{ .break_ = try self.loopStateExpr(result_ty, carries) } });

        var branches: [3]Ast.Branch = undefined;
        branches[0] = .{
            .pat = try self.iteratorDonePattern(step),
            .body = done_body,
        };
        branches[1] = try self.iteratorOneBranch(for_, result_ty, step, iterator_ty, carries);
        branches[2] = try self.iteratorSkipBranch(result_ty, step, iterator_ty, carries);

        const match_expr = try self.builder.program.addExpr(.{
            .ty = result_ty,
            .data = .{ .match_ = .{
                .scrutinee = step_expr,
                .branches = try self.builder.program.addBranchSpan(&branches),
            } },
        });

        const params = try self.allocator.alloc(Ast.TypedLocal, 1 + carries.len);
        defer self.allocator.free(params);
        params[0] = iterator_param;
        for (carries, 0..) |carry, i| params[i + 1] = .{ .local = carry.param_local, .ty = carry.ty };

        const initial_values = try self.allocator.alloc(Ast.ExprId, 1 + carries.len);
        defer self.allocator.free(initial_values);
        initial_values[0] = initial_iterator;
        for (carries, 0..) |carry, i| {
            initial_values[i + 1] = try self.builder.localExpr(carry.initial_local, carry.ty);
        }

        return .{ .loop_ = .{
            .params = try self.builder.program.addTypedLocalSpan(params),
            .initial_values = try self.builder.program.addExprSpan(initial_values),
            .body = match_expr,
        } };
    }

    fn lowerWhile(
        self: *BodyContext,
        while_: anytype,
        result_ty: Type.TypeId,
        carries: []const LoopCarry,
    ) Allocator.Error!Ast.ExprData {
        var saved = std.ArrayList(BinderRestore).empty;
        defer saved.deinit(self.allocator);
        for (carries) |carry| {
            try self.saveBinder(carry.binder, &saved);
            try self.binders.put(carry.binder, carry.param_local);
        }
        defer self.restoreBinders(saved.items);

        try self.pushLoopContext(result_ty, carries);
        defer self.popLoopContext();

        const cond = try self.lowerExpr(while_.cond);
        const break_body = try self.breakCurrentLoopExpr();
        const continue_body = try self.lowerWhileBodyThenContinue(while_.body, result_ty, carries);
        const branches = [_]Ast.IfBranch{.{ .cond = cond, .body = continue_body }};
        const body = try self.builder.program.addExpr(.{ .ty = result_ty, .data = .{ .if_ = .{
            .branches = try self.builder.program.addIfBranchSpan(&branches),
            .final_else = break_body,
        } } });

        const params = try self.allocator.alloc(Ast.TypedLocal, carries.len);
        defer self.allocator.free(params);
        for (carries, 0..) |carry, i| params[i] = .{ .local = carry.param_local, .ty = carry.ty };

        const initial_values = try self.allocator.alloc(Ast.ExprId, carries.len);
        defer self.allocator.free(initial_values);
        for (carries, 0..) |carry, i| {
            initial_values[i] = try self.builder.localExpr(carry.initial_local, carry.ty);
        }

        return .{ .loop_ = .{
            .params = try self.builder.program.addTypedLocalSpan(params),
            .initial_values = try self.builder.program.addExprSpan(initial_values),
            .body = body,
        } };
    }

    fn lowerIteratorDispatch(
        self: *BodyContext,
        plan: static_dispatch.IteratorDispatchCall,
        loop_iterator: ?Ast.TypedLocal,
        expected_ret_ty: ?Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        const plan_args = plan.argsSlice(self.view.static_dispatch_plans);
        if (plan.dispatcher_arg_index >= plan_args.len) Common.invariant("iterator dispatch plan dispatcher argument index was outside the argument span");

        var call_ctx = try BodyContext.init(self.allocator, self.builder, self.view, self.owner_template, self.graph);
        defer call_ctx.deinit();
        call_ctx.owner_context_fn_key = self.owner_context_fn_key;
        call_ctx.current_fn_key = self.current_fn_key;

        const callable_mono_ty = try call_ctx.instantiateIteratorPlanCallTypeFromCaller(plan.callable_ty, self, plan_args, loop_iterator, expected_ret_ty);
        const plan_fn_data = self.builder.functionShape(callable_mono_ty, "checked iterator dispatch plan had a non-function type");
        const plan_arg_tys = try self.allocator.dupe(Type.TypeId, self.builder.program.types.span(plan_fn_data.args));
        defer self.allocator.free(plan_arg_tys);
        if (expected_ret_ty) |expected| {
            if (!self.sameType(plan_fn_data.ret, expected)) {
                Common.invariant("checked iterator dispatch plan return type differed from iterator-for expected type");
            }
        }

        const dispatcher_ty = plan_arg_tys[plan.dispatcher_arg_index];
        try call_ctx.constrainTypeToMono(plan.dispatcher_ty, dispatcher_ty);
        const actual_dispatcher_ty = (try self.iteratorOperandMonoType(plan_args[plan.dispatcher_arg_index], loop_iterator, dispatcher_ty)) orelse
            Common.invariant("iterator dispatch plan dispatcher operand did not match the checked dispatcher type");
        if (!self.sameType(dispatcher_ty, actual_dispatcher_ty)) {
            Common.invariant("iterator dispatch plan dispatcher operand differed from the checked dispatcher type");
        }
        const owner = methodOwnerFromType(&self.builder.program.types, dispatcher_ty) orelse
            Common.invariant("iterator dispatch plan had no method owner");
        const lookup = self.builder.lookupMethodTarget(owner, self.view, plan.method) orelse
            Common.invariant("checked iterator dispatch method registry is missing resolved target");

        const target_mono_ty = try self.methodTargetMonoTypeFromPlan(lookup, &call_ctx, plan.callable_ty, expected_ret_ty);
        try call_ctx.constrainTypeToMono(plan.callable_ty, target_mono_ty);
        if (!self.sameType(callable_mono_ty, target_mono_ty)) {
            Common.invariant("checked iterator dispatch target callable type differed from dispatch plan callable type");
        }

        const fn_data = self.builder.functionShape(target_mono_ty, "checked iterator dispatch target had a non-function type");
        if (expected_ret_ty) |expected| {
            if (!self.sameType(fn_data.ret, expected)) {
                Common.invariant("checked iterator dispatch target return type differed from iterator-for expected type");
            }
        }
        const args = try self.allocator.alloc(Ast.ExprId, plan_args.len);
        defer self.allocator.free(args);
        const arg_tys = self.builder.program.types.span(fn_data.args);
        for (plan_args, 0..) |operand, i| {
            args[i] = try self.lowerIteratorOperandAtType(operand, loop_iterator, arg_tys[i]);
        }

        return try self.builder.program.addExpr(.{
            .ty = fn_data.ret,
            .data = .{ .call_proc = .{
                .callee = .{ .func = try self.methodTargetCalleeWithMono(lookup, target_mono_ty) },
                .args = try self.builder.program.addExprSpan(args),
            } },
        });
    }

    fn instantiateIteratorPlanCallTypeFromCaller(
        self: *BodyContext,
        source_fn_ty: checked.CheckedTypeId,
        caller: *BodyContext,
        operands: []const static_dispatch.IteratorDispatchOperand,
        loop_iterator: ?Ast.TypedLocal,
        expected_ret_ty: ?Type.TypeId,
    ) Allocator.Error!Type.TypeId {
        const function = self.checkedFunctionType(source_fn_ty);
        if (function.args.len != operands.len) {
            Common.invariant("checked iterator dispatch target arity differs from its function type");
        }
        const fn_node = try self.instNode(source_fn_ty);
        for (function.args, operands) |formal_ty, operand| {
            const formal_node = try self.instNode(formal_ty);
            switch (operand) {
                .checked_expr => |checked_arg| {
                    const arg_ty = caller.view.bodies.expr(checked_arg).ty;
                    try self.graph.unify(formal_node, try caller.instNode(arg_ty));
                    if (try caller.callArgumentMonoType(checked_arg, null)) |evidence_ty| {
                        try self.graph.unify(formal_node, try self.graph.importMono(evidence_ty));
                    }
                },
                .loop_iterator_state => {
                    const iterator = loop_iterator orelse Common.invariant("iterator .next dispatch reached Monotype without a loop iterator local");
                    try self.graph.unify(formal_node, try self.graph.importMono(iterator.ty));
                },
            }
        }
        if (expected_ret_ty) |expected| {
            try self.graph.unify(try self.instNode(function.ret), try self.graph.importMono(expected));
        }
        try self.graph.drainDirty();
        return try self.graph.monoFor(fn_node);
    }

    fn iteratorOperandMonoType(
        self: *BodyContext,
        operand: static_dispatch.IteratorDispatchOperand,
        loop_iterator: ?Ast.TypedLocal,
        expected_ty: ?Type.TypeId,
    ) Allocator.Error!?Type.TypeId {
        return switch (operand) {
            .checked_expr => |expr| try self.callArgumentMonoType(expr, expected_ty),
            .loop_iterator_state => if (loop_iterator) |iterator| iterator.ty else Common.invariant("iterator .next dispatch reached Monotype without a loop iterator local"),
        };
    }

    fn lowerIteratorOperandAtType(
        self: *BodyContext,
        operand: static_dispatch.IteratorDispatchOperand,
        loop_iterator: ?Ast.TypedLocal,
        ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        return switch (operand) {
            .checked_expr => |expr| try self.lowerExprAtType(expr, ty),
            .loop_iterator_state => blk: {
                const iterator = loop_iterator orelse Common.invariant("iterator .next dispatch reached Monotype without a loop iterator local");
                if (!self.sameType(iterator.ty, ty)) Common.invariant("iterator .next operand type differed from instantiated callable argument type");
                break :blk try self.builder.localExpr(iterator.local, iterator.ty);
            },
        };
    }

    fn iteratorDonePattern(self: *BodyContext, step: IterStepShape) Allocator.Error!Ast.PatId {
        return try self.builder.program.addPat(.{ .ty = try self.lowerType(step.step_ty), .data = .{ .tag = .{
            .name = try self.builder.tagName(self.view, step.done_tag),
            .payloads = .empty(),
        } } });
    }

    fn iteratorOneBranch(
        self: *BodyContext,
        for_: anytype,
        result_ty: Type.TypeId,
        step: IterStepShape,
        iterator_ty: Type.TypeId,
        carries: []const LoopCarry,
    ) Allocator.Error!Ast.Branch {
        var saved = std.ArrayList(BinderRestore).empty;
        defer saved.deinit(self.allocator);
        try self.savePatternBinders(for_.pattern, &saved);
        for (carries) |carry| try self.saveBinder(carry.binder, &saved);
        defer self.restoreBinders(saved.items);

        const item_ty = try self.lowerType(step.one_item.ty);
        const rest_local = try self.builder.program.addLocal(self.builder.symbols.fresh(), iterator_ty);
        const item_local: ?Ast.LocalId = if (self.patternNeedsExplicitBinding(for_.pattern))
            try self.builder.program.addLocal(self.builder.symbols.fresh(), item_ty)
        else
            null;
        const record_pat = try self.iteratorOnePayloadPattern(for_.pattern, step, item_ty, iterator_ty, rest_local, item_local);
        const tag_pat = try self.builder.program.addPat(.{ .ty = try self.lowerType(step.step_ty), .data = .{ .tag = .{
            .name = try self.builder.tagName(self.view, step.one_tag),
            .payloads = try self.builder.program.addPatSpan(&[_]Ast.PatId{record_pat}),
        } } });

        const rest_expr = try self.builder.localExpr(rest_local, iterator_ty);
        const block = if (item_local) |local| blk: {
            const item_expr = try self.builder.localExpr(local, item_ty);
            const miss = try self.runtimeCrashExpr(result_ty, "pattern match failed");
            break :blk try self.lowerMaterializedPatternThen(
                for_.pattern,
                item_expr,
                item_ty,
                result_ty,
                .{ .iterator_body = .{
                    .body = for_.body,
                    .result_ty = result_ty,
                    .rest_expr = rest_expr,
                    .carries = carries,
                } },
                miss,
            );
        } else try self.lowerIteratorBodyThenContinue(for_.body, result_ty, rest_expr, carries);

        return .{ .pat = tag_pat, .body = block };
    }

    fn lowerIteratorBodyThenContinue(
        self: *BodyContext,
        body: checked.CheckedExprId,
        result_ty: Type.TypeId,
        rest_expr: Ast.ExprId,
        carries: []const LoopCarry,
    ) Allocator.Error!Ast.ExprId {
        const checked_body = self.view.bodies.expr(body);
        switch (checked_body.data) {
            .block => |block| {
                var statement_diverges = false;
                for (block.statements) |statement| {
                    if (self.checkedStatementDiverges(statement)) statement_diverges = true;
                }
                const extra: usize = if (statement_diverges) 0 else 1;
                const lowered_statements = try self.allocator.alloc(Ast.StmtId, block.statements.len + extra);
                defer self.allocator.free(lowered_statements);
                for (block.statements, 0..) |statement, i| {
                    lowered_statements[i] = try self.lowerStatement(statement);
                }
                if (!statement_diverges) {
                    lowered_statements[block.statements.len] = try self.builder.program.addStmt(.{ .expr = if (self.checkedExprDiverges(block.final_expr))
                        try self.lowerDivergentExprAtType(block.final_expr, result_ty)
                    else
                        try self.lowerExpr(block.final_expr) });
                }
                return try self.builder.program.addExpr(.{ .ty = result_ty, .data = .{ .block = .{
                    .statements = try self.builder.program.addStmtSpan(lowered_statements),
                    .final_expr = try self.continueWithState(result_ty, rest_expr, carries),
                } } });
            },
            else => {
                const body_expr = try self.lowerExpr(body);
                const body_stmt = try self.builder.program.addStmt(.{ .expr = body_expr });
                return try self.builder.program.addExpr(.{ .ty = result_ty, .data = .{ .block = .{
                    .statements = try self.builder.program.addStmtSpan(&[_]Ast.StmtId{body_stmt}),
                    .final_expr = try self.continueWithState(result_ty, rest_expr, carries),
                } } });
            },
        }
    }

    fn lowerWhileBodyThenContinue(
        self: *BodyContext,
        body: checked.CheckedExprId,
        result_ty: Type.TypeId,
        carries: []const LoopCarry,
    ) Allocator.Error!Ast.ExprId {
        const checked_body = self.view.bodies.expr(body);
        switch (checked_body.data) {
            .block => |block| {
                var statement_diverges = false;
                for (block.statements) |statement| {
                    if (self.checkedStatementDiverges(statement)) statement_diverges = true;
                }
                const extra: usize = if (statement_diverges) 0 else 1;
                const lowered_statements = try self.allocator.alloc(Ast.StmtId, block.statements.len + extra);
                defer self.allocator.free(lowered_statements);
                for (block.statements, 0..) |statement, i| {
                    lowered_statements[i] = try self.lowerStatement(statement);
                }
                if (!statement_diverges) {
                    lowered_statements[block.statements.len] = try self.builder.program.addStmt(.{ .expr = if (self.checkedExprDiverges(block.final_expr))
                        try self.lowerDivergentExprAtType(block.final_expr, result_ty)
                    else
                        try self.lowerExpr(block.final_expr) });
                }
                return try self.builder.program.addExpr(.{ .ty = result_ty, .data = .{ .block = .{
                    .statements = try self.builder.program.addStmtSpan(lowered_statements),
                    .final_expr = try self.continueWithCurrentState(result_ty, carries),
                } } });
            },
            else => {
                const body_expr = try self.lowerExpr(body);
                const body_stmt = try self.builder.program.addStmt(.{ .expr = body_expr });
                return try self.builder.program.addExpr(.{ .ty = result_ty, .data = .{ .block = .{
                    .statements = try self.builder.program.addStmtSpan(&[_]Ast.StmtId{body_stmt}),
                    .final_expr = try self.continueWithCurrentState(result_ty, carries),
                } } });
            },
        }
    }

    fn iteratorSkipBranch(
        self: *BodyContext,
        result_ty: Type.TypeId,
        step: IterStepShape,
        iterator_ty: Type.TypeId,
        carries: []const LoopCarry,
    ) Allocator.Error!Ast.Branch {
        const rest_local = try self.builder.program.addLocal(self.builder.symbols.fresh(), iterator_ty);
        const record_pat = try self.iteratorSkipPayloadPattern(step, iterator_ty, rest_local);
        const tag_pat = try self.builder.program.addPat(.{ .ty = try self.lowerType(step.step_ty), .data = .{ .tag = .{
            .name = try self.builder.tagName(self.view, step.skip_tag),
            .payloads = try self.builder.program.addPatSpan(&[_]Ast.PatId{record_pat}),
        } } });
        const rest_expr = try self.builder.localExpr(rest_local, iterator_ty);
        return .{
            .pat = tag_pat,
            .body = try self.continueWithState(result_ty, rest_expr, carries),
        };
    }

    fn iteratorOnePayloadPattern(
        self: *BodyContext,
        item_pattern: checked.CheckedPatternId,
        step: IterStepShape,
        item_ty: Type.TypeId,
        iterator_ty: Type.TypeId,
        rest_local: Ast.LocalId,
        item_local: ?Ast.LocalId,
    ) Allocator.Error!Ast.PatId {
        const item_pat = if (item_local) |local|
            try self.builder.bindPat(local, item_ty)
        else
            try self.lowerPatternAtType(item_pattern, item_ty);
        const item_field = try self.iteratorRecordDestruct(step.one_item.name, item_pat);
        const rest_field = try self.iteratorRecordDestruct(step.one_rest.name, try self.builder.bindPat(rest_local, iterator_ty));
        const fields = [_]Ast.RecordDestruct{ item_field, rest_field };
        return try self.builder.program.addPat(.{
            .ty = try self.lowerType(step.one_payload_ty),
            .data = .{ .record = try self.builder.program.addRecordDestructSpan(&fields) },
        });
    }

    fn iteratorSkipPayloadPattern(
        self: *BodyContext,
        step: IterStepShape,
        iterator_ty: Type.TypeId,
        rest_local: Ast.LocalId,
    ) Allocator.Error!Ast.PatId {
        const rest_field = try self.iteratorRecordDestruct(step.skip_rest.name, try self.builder.bindPat(rest_local, iterator_ty));
        const fields = [_]Ast.RecordDestruct{rest_field};
        return try self.builder.program.addPat(.{
            .ty = try self.lowerType(step.skip_payload_ty),
            .data = .{ .record = try self.builder.program.addRecordDestructSpan(&fields) },
        });
    }

    fn iteratorRecordDestruct(
        self: *BodyContext,
        field: names.RecordFieldNameId,
        pattern: Ast.PatId,
    ) Allocator.Error!Ast.RecordDestruct {
        return .{
            .name = try self.builder.recordFieldName(self.view, field),
            .pattern = pattern,
        };
    }

    fn pushLoopContext(self: *BodyContext, result_ty: Type.TypeId, carries: []const LoopCarry) Allocator.Error!void {
        try self.loop_contexts.append(self.allocator, .{
            .result_ty = result_ty,
            .carries = carries,
        });
    }

    fn popLoopContext(self: *BodyContext) void {
        _ = self.loop_contexts.pop();
    }

    fn currentLoopContext(self: *BodyContext) LoopContext {
        if (self.loop_contexts.items.len == 0) Common.invariant("break statement reached Monotype outside an explicit loop expression");
        return self.loop_contexts.items[self.loop_contexts.items.len - 1];
    }

    fn breakCurrentLoopExpr(self: *BodyContext) Allocator.Error!Ast.ExprId {
        const loop = self.currentLoopContext();
        const data = try self.breakCurrentLoopExprData();
        return try self.builder.program.addExpr(.{ .ty = loop.result_ty, .data = data });
    }

    fn breakCurrentLoopExprData(self: *BodyContext) Allocator.Error!Ast.ExprData {
        const loop = self.currentLoopContext();
        return if (loop.carries.len == 0)
            .{ .break_ = null }
        else
            .{ .break_ = try self.loopStateExpr(loop.result_ty, loop.carries) };
    }

    fn continueWith(self: *BodyContext, ty: Type.TypeId, values: []const Ast.ExprId) Allocator.Error!Ast.ExprId {
        return try self.builder.program.addExpr(.{ .ty = ty, .data = .{ .continue_ = .{
            .values = try self.builder.program.addExprSpan(values),
        } } });
    }

    fn continueWithCurrentState(
        self: *BodyContext,
        ty: Type.TypeId,
        carries: []const LoopCarry,
    ) Allocator.Error!Ast.ExprId {
        if (carries.len == 0) return try self.continueWith(ty, &.{});

        const values = try self.allocator.alloc(Ast.ExprId, carries.len);
        defer self.allocator.free(values);
        for (carries, 0..) |carry, i| {
            const current = self.binders.get(carry.binder) orelse Common.invariant("loop-carried mutable binder was absent");
            values[i] = try self.builder.localExpr(current, carry.ty);
        }
        return try self.continueWith(ty, values);
    }

    fn continueWithState(
        self: *BodyContext,
        ty: Type.TypeId,
        rest_expr: Ast.ExprId,
        carries: []const LoopCarry,
    ) Allocator.Error!Ast.ExprId {
        const values = try self.allocator.alloc(Ast.ExprId, 1 + carries.len);
        defer self.allocator.free(values);
        values[0] = rest_expr;
        for (carries, 0..) |carry, i| {
            const current = self.binders.get(carry.binder) orelse Common.invariant("loop-carried mutable binder was absent");
            values[i + 1] = try self.builder.localExpr(current, carry.ty);
        }
        return try self.continueWith(ty, values);
    }

    fn loopStateExpr(self: *BodyContext, ty: Type.TypeId, carries: []const LoopCarry) Allocator.Error!Ast.ExprId {
        if (carries.len == 0) Common.invariant("empty loop state requested");
        if (carries.len == 1) {
            const current = self.binders.get(carries[0].binder) orelse Common.invariant("loop-carried mutable binder was absent");
            return try self.builder.localExpr(current, carries[0].ty);
        }

        const items = try self.allocator.alloc(Ast.ExprId, carries.len);
        defer self.allocator.free(items);
        for (carries, 0..) |carry, i| {
            const current = self.binders.get(carry.binder) orelse Common.invariant("loop-carried mutable binder was absent");
            items[i] = try self.builder.localExpr(current, carry.ty);
        }
        return try self.builder.program.addExpr(.{ .ty = ty, .data = .{ .tuple = try self.builder.program.addExprSpan(items) } });
    }

    fn loopStateType(self: *BodyContext, unit_ty: Type.TypeId, carries: []const LoopCarry) Allocator.Error!Type.TypeId {
        if (carries.len == 0) return unit_ty;
        if (carries.len == 1) return carries[0].ty;

        const tys = try self.allocator.alloc(Type.TypeId, carries.len);
        defer self.allocator.free(tys);
        for (carries, 0..) |carry, i| tys[i] = carry.ty;
        return try self.builder.program.types.add(.{ .tuple = try self.builder.program.types.addSpan(tys) });
    }

    fn unitType(self: *BodyContext) Allocator.Error!Type.TypeId {
        return try self.builder.program.types.add(.{ .record = .empty() });
    }

    fn prepareLoopCarries(self: *BodyContext, binders: []const checked.PatternBinderId) Allocator.Error![]LoopCarry {
        var carries = std.ArrayList(LoopCarry).empty;
        errdefer carries.deinit(self.allocator);
        for (binders) |binder| {
            const initial = self.binders.get(binder) orelse continue;
            const ty = self.builder.program.locals.items[@intFromEnum(initial)].ty;
            try carries.append(self.allocator, .{
                .binder = binder,
                .initial_local = initial,
                .param_local = try self.builder.program.addLocal(self.builder.symbols.fresh(), ty),
                .ty = ty,
            });
        }
        return try carries.toOwnedSlice(self.allocator);
    }

    fn finalCarryPattern(self: *BodyContext, carries: []const LoopCarry, ty: Type.TypeId) Allocator.Error!Ast.PatId {
        if (carries.len == 0) Common.invariant("empty loop carry pattern requested");
        if (carries.len == 1) {
            const local = try self.builder.program.addLocalWithBinder(self.builder.symbols.fresh(), carries[0].ty, carries[0].binder);
            try bindLocalName(self.builder.program, self.view, local, carries[0].binder);
            try self.binders.put(carries[0].binder, local);
            return try self.builder.program.addPat(.{ .ty = ty, .data = .{ .bind = local } });
        }

        const items = try self.allocator.alloc(Ast.PatId, carries.len);
        defer self.allocator.free(items);
        for (carries, 0..) |carry, i| {
            const local = try self.builder.program.addLocalWithBinder(self.builder.symbols.fresh(), carry.ty, carry.binder);
            try bindLocalName(self.builder.program, self.view, local, carry.binder);
            try self.binders.put(carry.binder, local);
            items[i] = try self.builder.program.addPat(.{ .ty = carry.ty, .data = .{ .bind = local } });
        }
        return try self.builder.program.addPat(.{ .ty = ty, .data = .{ .tuple = try self.builder.program.addPatSpan(items) } });
    }

    fn collectReassignedBindersInExpr(
        self: *BodyContext,
        expr_id: checked.CheckedExprId,
        out: *std.ArrayList(checked.PatternBinderId),
    ) Allocator.Error!void {
        const expr = self.view.bodies.expr(expr_id);
        switch (expr.data) {
            .str => |segments| for (segments) |segment| try self.collectReassignedBindersInExpr(segment, out),
            .list => |items| for (items) |item| try self.collectReassignedBindersInExpr(item, out),
            .tuple => |items| for (items) |item| try self.collectReassignedBindersInExpr(item, out),
            .match_ => |match| {
                try self.collectReassignedBindersInExpr(match.cond, out);
                for (match.branches) |branch| {
                    if (branch.guard) |guard| try self.collectReassignedBindersInExpr(guard, out);
                    try self.collectReassignedBindersInExpr(branch.value, out);
                }
            },
            .if_ => |if_| {
                for (if_.branches) |branch| {
                    try self.collectReassignedBindersInExpr(branch.cond, out);
                    try self.collectReassignedBindersInExpr(branch.body, out);
                }
                try self.collectReassignedBindersInExpr(if_.final_else, out);
            },
            .call => |call| {
                try self.collectReassignedBindersInExpr(call.func, out);
                for (call.args) |arg| try self.collectReassignedBindersInExpr(arg, out);
            },
            .record => |record| {
                if (record.ext) |ext| try self.collectReassignedBindersInExpr(ext, out);
                for (record.fields) |field| try self.collectReassignedBindersInExpr(field.value, out);
            },
            .block => |block| {
                for (block.statements) |statement| try self.collectReassignedBindersInStatement(statement, out);
                try self.collectReassignedBindersInExpr(block.final_expr, out);
            },
            .tag => |tag| for (tag.args) |arg| try self.collectReassignedBindersInExpr(arg, out),
            .nominal => |nominal| try self.collectReassignedBindersInExpr(nominal.backing_expr, out),
            .binop => |binop| {
                try self.collectReassignedBindersInExpr(binop.lhs, out);
                try self.collectReassignedBindersInExpr(binop.rhs, out);
            },
            .unary_minus,
            .unary_not,
            .dbg,
            .expect,
            => |child| try self.collectReassignedBindersInExpr(child, out),
            .expect_err => |expect_err| try self.collectReassignedBindersInExpr(expect_err.expr, out),
            .field_access => |field| try self.collectReassignedBindersInExpr(field.receiver, out),
            .structural_eq => |eq| {
                try self.collectReassignedBindersInExpr(eq.lhs, out);
                try self.collectReassignedBindersInExpr(eq.rhs, out);
            },
            .structural_hash => |h| {
                try self.collectReassignedBindersInExpr(h.value, out);
                try self.collectReassignedBindersInExpr(h.hasher, out);
            },
            .tuple_access => |access| try self.collectReassignedBindersInExpr(access.tuple, out),
            .return_ => |ret| try self.collectReassignedBindersInExpr(ret.expr, out),
            .for_ => |for_| {
                try self.collectReassignedBindersInExpr(for_.expr, out);
                try self.collectReassignedBindersInExpr(for_.body, out);
            },
            .run_low_level => |low_level| for (low_level.args) |arg| try self.collectReassignedBindersInExpr(arg, out),
            .lambda,
            .closure,
            .hosted_lambda,
            .pending,
            .num,
            .frac_f32,
            .frac_f64,
            .dec,
            .dec_small,
            .num_from_numeral,
            .typed_int,
            .typed_frac,
            .typed_num_from_numeral,
            .str_from_quote,
            .str_segment,
            .bytes_literal,
            .lookup_local,
            .lookup_external,
            .lookup_required,
            .empty_list,
            .empty_record,
            .zero_argument_tag,
            .dispatch_call,
            .interpolation,
            .method_eq,
            .type_dispatch_call,
            .runtime_error,
            .crash,
            .ellipsis,
            .anno_only,
            .break_,
            => {},
        }
    }

    fn collectReassignedBindersInStatement(
        self: *BodyContext,
        statement_id: checked.CheckedStatementId,
        out: *std.ArrayList(checked.PatternBinderId),
    ) Allocator.Error!void {
        const statement = self.view.bodies.statement(statement_id);
        switch (statement.data) {
            .decl => |decl| try self.collectReassignedBindersInExpr(decl.expr, out),
            .var_ => |var_| try self.collectReassignedBindersInExpr(var_.expr, out),
            .var_uninitialized => {},
            .reassign => |reassign| {
                for (reassign.reassigned_binders) |binder| try self.appendUniqueBinder(out, binder);
                try self.collectReassignedBindersInExpr(reassign.expr, out);
            },
            .dbg,
            .expr,
            .expect,
            => |expr| try self.collectReassignedBindersInExpr(expr, out),
            .for_ => |for_| {
                try self.collectReassignedBindersInExpr(for_.expr, out);
                try self.collectReassignedBindersInExpr(for_.body, out);
            },
            .while_ => |while_| {
                try self.collectReassignedBindersInExpr(while_.cond, out);
                try self.collectReassignedBindersInExpr(while_.body, out);
            },
            .infinite_loop => |loop| {
                try self.collectReassignedBindersInExpr(loop.cond, out);
                try self.collectReassignedBindersInExpr(loop.body, out);
            },
            .breakable_loop => |loop| {
                try self.collectReassignedBindersInExpr(loop.cond, out);
                try self.collectReassignedBindersInExpr(loop.body, out);
            },
            .return_ => |ret| try self.collectReassignedBindersInExpr(ret.expr, out),
            .pending,
            .crash,
            .break_,
            .import_,
            .alias_decl,
            .nominal_decl,
            .type_anno,
            .type_var_alias,
            .runtime_error,
            => {},
        }
    }

    fn appendUniqueBinder(
        self: *BodyContext,
        out: *std.ArrayList(checked.PatternBinderId),
        binder: checked.PatternBinderId,
    ) Allocator.Error!void {
        for (out.items) |existing| {
            if (existing == binder) return;
        }
        try out.append(self.allocator, binder);
    }

    const IterStepShape = struct {
        step_ty: checked.CheckedTypeId,
        done_tag: names.TagNameId,
        one_tag: names.TagNameId,
        skip_tag: names.TagNameId,
        one_payload_ty: checked.CheckedTypeId,
        skip_payload_ty: checked.CheckedTypeId,
        one_item: checked.CheckedRecordField,
        one_rest: checked.CheckedRecordField,
        skip_rest: checked.CheckedRecordField,
    };

    fn iteratorStepShape(self: *BodyContext, step_ty: checked.CheckedTypeId) Allocator.Error!IterStepShape {
        const step_payload = resolvedPayload(self.view, step_ty).payload;
        const step_tag_union = switch (step_payload) {
            .tag_union => |tag_union| tag_union,
            else => Common.invariant("iterator .next plan did not return a tag union"),
        };

        var done_tag: ?names.TagNameId = null;
        var one_tag: ?names.TagNameId = null;
        var skip_tag: ?names.TagNameId = null;
        var one_payload_ty: ?checked.CheckedTypeId = null;
        var skip_payload_ty: ?checked.CheckedTypeId = null;

        var seen = std.AutoHashMap(checked.CheckedTypeId, void).init(self.allocator);
        defer seen.deinit();

        var tags = step_tag_union.tags;
        var current: ?checked.CheckedTypeId = step_tag_union.ext;
        while (true) {
            for (tags) |tag| {
                const tag_args = tag.argsSlice(self.view.types);
                const tag_text = self.view.names.tagLabelText(tag.name);
                if (Ident.textEql(tag_text, "Done")) {
                    if (tag_args.len != 0) Common.invariant("iterator Done step carried payloads");
                    if (done_tag != null) Common.invariant("iterator step type had duplicate Done tags");
                    done_tag = tag.name;
                } else if (Ident.textEql(tag_text, "One")) {
                    if (tag_args.len != 1) Common.invariant("iterator One step did not carry one payload");
                    if (one_tag != null) Common.invariant("iterator step type had duplicate One tags");
                    one_tag = tag.name;
                    one_payload_ty = tag_args[0];
                } else if (Ident.textEql(tag_text, "Skip")) {
                    if (tag_args.len != 1) Common.invariant("iterator Skip step did not carry one payload");
                    if (skip_tag != null) Common.invariant("iterator step type had duplicate Skip tags");
                    skip_tag = tag.name;
                    skip_payload_ty = tag_args[0];
                }
            }

            const ext = current orelse break;
            if (seen.contains(ext)) break;
            try seen.put(ext, {});

            switch (checkedPayload(self.view, ext)) {
                .alias => |alias| {
                    tags = &.{};
                    current = alias.backing;
                },
                .empty_tag_union => break,
                .flex, .rigid => |variable| {
                    if (variable.row_default == .empty_tag_union) break;
                    Common.invariant("open non-tag iterator step row reached Monotype lowering");
                },
                .tag_union => |tag_union| {
                    tags = tag_union.tags;
                    current = tag_union.ext;
                },
                else => Common.invariant("open or non-tag iterator step row reached Monotype lowering"),
            }
        }

        const one_payload = one_payload_ty orelse Common.invariant("iterator step type was missing One");
        const skip_payload = skip_payload_ty orelse Common.invariant("iterator step type was missing Skip");

        return .{
            .step_ty = step_ty,
            .done_tag = done_tag orelse Common.invariant("iterator step type was missing Done"),
            .one_tag = one_tag orelse Common.invariant("iterator step type was missing One"),
            .skip_tag = skip_tag orelse Common.invariant("iterator step type was missing Skip"),
            .one_payload_ty = one_payload,
            .skip_payload_ty = skip_payload,
            .one_item = checkedRecordFieldByName(self.view, one_payload, "item"),
            .one_rest = checkedRecordFieldByName(self.view, one_payload, "rest"),
            .skip_rest = checkedRecordFieldByName(self.view, skip_payload, "rest"),
        };
    }

    fn lowerStatement(self: *BodyContext, statement_id: checked.CheckedStatementId) Allocator.Error!Ast.StmtId {
        const statement = self.view.bodies.statement(statement_id);
        const saved_loc = self.builder.program.current_loc;
        defer self.builder.program.current_loc = saved_loc;
        const saved_region = self.builder.program.current_region;
        defer self.builder.program.current_region = saved_region;
        self.builder.program.current_loc = try self.sourceLocFor(statement.source_region);
        self.builder.program.current_region = statement.source_region;
        const stmt: Ast.Stmt = switch (statement.data) {
            .pending,
            .import_,
            .alias_decl,
            .nominal_decl,
            .type_anno,
            .type_var_alias,
            .runtime_error,
            => Common.invariant("non-runtime checked statement reached Monotype lowering"),
            .decl => |decl| blk: {
                if (self.statementValueIsLocalProc(decl.expr)) {
                    try self.registerLocalProc(decl.pattern);
                    const unit_ty = try self.unitType();
                    break :blk .{ .expr = try self.builder.program.addExpr(.{ .ty = unit_ty, .data = .unit }) };
                }
                break :blk try self.lowerPatternStatement(decl.pattern, decl.expr, statement.source_region);
            },
            .var_ => |decl| try self.lowerPatternStatement(decl.pattern, decl.expr, statement.source_region),
            .var_uninitialized => |decl| .{ .uninitialized = try self.lowerUninitializedPatternStatement(decl.pattern) },
            .reassign => |decl| try self.lowerPatternStatement(decl.pattern, decl.expr, statement.source_region),
            .crash => |msg| .{ .crash = try self.lowerStringLiteral(msg) },
            .dbg => |child| .{ .dbg = try self.lowerDbgMessage(child) },
            .expr => |child| try self.lowerExprStatement(child),
            .expect => |child| .{ .expect = try self.lowerExpr(child) },
            .for_ => |for_| blk: {
                var reassigned = std.ArrayList(checked.PatternBinderId).empty;
                defer reassigned.deinit(self.allocator);
                try self.collectReassignedBindersInExpr(for_.body, &reassigned);

                const carries = try self.prepareLoopCarries(reassigned.items);
                defer self.allocator.free(carries);

                const unit_ty = try self.unitType();
                const loop_ty = try self.loopStateType(unit_ty, carries);
                const expr = try self.builder.program.addExpr(.{ .ty = loop_ty, .data = try self.lowerIteratorFor(for_, loop_ty, carries) });
                if (carries.len == 0) break :blk .{ .expr = expr };

                break :blk .{ .let_ = .{
                    .pat = try self.finalCarryPattern(carries, loop_ty),
                    .value = expr,
                } };
            },
            .while_ => |while_| blk: {
                var reassigned = std.ArrayList(checked.PatternBinderId).empty;
                defer reassigned.deinit(self.allocator);
                try self.collectReassignedBindersInExpr(while_.cond, &reassigned);
                try self.collectReassignedBindersInExpr(while_.body, &reassigned);

                const carries = try self.prepareLoopCarries(reassigned.items);
                defer self.allocator.free(carries);

                const unit_ty = try self.unitType();
                const loop_ty = try self.loopStateType(unit_ty, carries);
                const expr = try self.builder.program.addExpr(.{ .ty = loop_ty, .data = try self.lowerWhile(while_, loop_ty, carries) });
                if (carries.len == 0) break :blk .{ .expr = expr };

                break :blk .{ .let_ = .{
                    .pat = try self.finalCarryPattern(carries, loop_ty),
                    .value = expr,
                } };
            },
            .infinite_loop => |loop| blk: {
                var reassigned = std.ArrayList(checked.PatternBinderId).empty;
                defer reassigned.deinit(self.allocator);
                try self.collectReassignedBindersInExpr(loop.cond, &reassigned);
                try self.collectReassignedBindersInExpr(loop.body, &reassigned);

                const carries = try self.prepareLoopCarries(reassigned.items);
                defer self.allocator.free(carries);

                const unit_ty = try self.unitType();
                const loop_ty = try self.loopStateType(unit_ty, carries);
                const expr = try self.builder.program.addExpr(.{ .ty = loop_ty, .data = try self.lowerWhile(loop, loop_ty, carries) });
                if (carries.len == 0) break :blk .{ .expr = expr };

                break :blk .{ .let_ = .{
                    .pat = try self.finalCarryPattern(carries, loop_ty),
                    .value = expr,
                } };
            },
            .breakable_loop => |loop| blk: {
                var reassigned = std.ArrayList(checked.PatternBinderId).empty;
                defer reassigned.deinit(self.allocator);
                try self.collectReassignedBindersInExpr(loop.cond, &reassigned);
                try self.collectReassignedBindersInExpr(loop.body, &reassigned);

                const carries = try self.prepareLoopCarries(reassigned.items);
                defer self.allocator.free(carries);

                const unit_ty = try self.unitType();
                const loop_ty = try self.loopStateType(unit_ty, carries);
                const expr = try self.builder.program.addExpr(.{ .ty = loop_ty, .data = try self.lowerWhile(loop, loop_ty, carries) });
                if (carries.len == 0) break :blk .{ .expr = expr };

                break :blk .{ .let_ = .{
                    .pat = try self.finalCarryPattern(carries, loop_ty),
                    .value = expr,
                } };
            },
            .break_ => .{ .expr = try self.breakCurrentLoopExpr() },
            .return_ => |ret| .{ .return_ = try self.lowerExpr(ret.expr) },
        };
        return try self.builder.program.addStmt(stmt);
    }

    fn lowerUninitializedPatternStatement(
        self: *BodyContext,
        pattern: checked.CheckedPatternId,
    ) Allocator.Error!Ast.PatId {
        const checked_pattern = self.view.bodies.pattern(pattern);
        return try self.lowerPatternAtType(pattern, try self.lowerType(checked_pattern.ty));
    }

    fn lowerPatternStatement(
        self: *BodyContext,
        pattern: checked.CheckedPatternId,
        expr: checked.CheckedExprId,
        source_region: base.Region,
    ) Allocator.Error!Ast.Stmt {
        const value = try self.lowerExpr(expr);
        const value_ty = self.builder.program.exprs.items[@intFromEnum(value)].ty;
        const comptime_site = if (self.inComptimeExhaustivenessContext() and self.patternCanMiss(pattern))
            try self.addComptimeSite(.destructure, source_region, self.view.exhaustiveness_sites.lookupByDestructurePattern(pattern), &.{})
        else
            null;
        if (!self.patternNeedsExplicitBinding(pattern)) {
            return .{ .let_ = .{
                .pat = try self.lowerPatternAtType(pattern, value_ty),
                .value = value,
                .comptime_site = comptime_site,
            } };
        }

        const unit_ty = try self.unitType();
        var unit = try self.builder.program.addExpr(.{ .ty = unit_ty, .data = .unit });
        unit = try self.wrapComptimeBranch(comptime_site, 0, unit);
        const miss = if (comptime_site) |site|
            try self.comptimeExhaustivenessFailedExpr(unit_ty, site)
        else
            try self.runtimeCrashExpr(unit_ty, "pattern match failed");
        return .{ .expr = try self.lowerMaterializedPatternValueThen(
            pattern,
            value,
            value_ty,
            unit_ty,
            .{ .expr = unit },
            miss,
        ) };
    }

    fn registerLocalProc(self: *BodyContext, pattern_id: checked.CheckedPatternId) Allocator.Error!void {
        const binder = self.localProcBinder(pattern_id);
        if (self.local_proc_contexts.get(binder)) |existing| {
            if (!std.mem.eql(u8, existing.bytes[0..], self.current_fn_key.bytes[0..])) {
                Common.invariant("local procedure binder had two declaration contexts");
            }
            return;
        }
        try self.local_proc_contexts.put(binder, self.current_fn_key);
    }

    fn localProcBinder(self: *BodyContext, pattern_id: checked.CheckedPatternId) checked.PatternBinderId {
        const pattern = self.view.bodies.pattern(pattern_id);
        return switch (pattern.data) {
            .assign => |binder| binder,
            else => Common.invariant("local procedure declaration pattern was not a binder"),
        };
    }

    fn lowerExprStatement(self: *BodyContext, expr_id: checked.CheckedExprId) Allocator.Error!Ast.Stmt {
        const merge_binders = try self.stateMergeBinders(expr_id);
        defer self.allocator.free(merge_binders);
        if (merge_binders.len == 0) return .{ .expr = try self.lowerExpr(expr_id) };

        const state_ty = try self.stateOnlyType(merge_binders);
        const checked_expr = self.view.bodies.expr(expr_id);
        const state_expr = switch (checked_expr.data) {
            .if_ => |if_| try self.builder.program.addExpr(.{
                .ty = state_ty,
                .data = try self.lowerIfStateOnly(if_, state_ty, merge_binders, try self.ifComptimeSite(expr_id, if_)),
            }),
            .match_ => |match| try self.lowerMatchExprWithOutput(match, .{ .state_only = .{
                .state_ty = state_ty,
                .merge_binders = merge_binders,
            } }, try self.matchComptimeSite(expr_id, match)),
            else => try self.lowerBodyThenStateOnly(expr_id, state_ty, merge_binders),
        };
        return .{ .let_ = .{
            .pat = try self.stateOnlyPattern(state_ty, merge_binders),
            .value = state_expr,
        } };
    }

    fn statementValueIsLocalProc(self: *BodyContext, expr_id: checked.CheckedExprId) bool {
        return switch (self.view.bodies.expr(expr_id).data) {
            .lambda,
            .closure,
            => true,
            else => false,
        };
    }

    fn lowerPatternAtType(self: *BodyContext, pattern_id: checked.CheckedPatternId, ty: Type.TypeId) Allocator.Error!Ast.PatId {
        const pattern = self.view.bodies.pattern(pattern_id);
        switch (pattern.data) {
            .assign => {},
            else => try self.constrainTypeToMono(pattern.ty, ty),
        }
        const data: Ast.PatData = switch (pattern.data) {
            .pending,
            .runtime_error,
            => Common.invariant("non-runtime checked pattern reached Monotype lowering"),
            .assign => |binder| blk: {
                const local = try self.builder.program.addLocalWithBinder(self.builder.symbols.fresh(), ty, binder);
                try bindLocalName(self.builder.program, self.view, local, binder);
                try self.binders.put(binder, local);
                break :blk .{ .bind = local };
            },
            .as => |as| blk: {
                const local = try self.builder.program.addLocalWithBinder(self.builder.symbols.fresh(), ty, as.binder);
                try bindLocalName(self.builder.program, self.view, local, as.binder);
                try self.binders.put(as.binder, local);
                break :blk .{ .as = .{
                    .pattern = try self.lowerPatternAtType(as.pattern, ty),
                    .local = local,
                } };
            },
            .applied_tag => |tag| try self.lowerTagPattern(tag, ty),
            .nominal => |nominal| .{ .nominal = try self.lowerPatternAtType(nominal.backing_pattern, self.builder.namedBackingType(ty) orelse ty) },
            .record_destructure => |destructs| try self.lowerRecordPattern(destructs, ty),
            .list => |list| try self.lowerListPattern(list, ty),
            .tuple => |items| .{ .tuple = try self.lowerTuplePattern(items, ty) },
            .num_literal => |num| if (num.conversion) |conversion|
                try self.bindLiteralGuardPattern(conversion, ty)
            else
                self.lowerNumPattern(num.value, ty),
            .small_dec_literal => |dec| if (dec.conversion) |conversion|
                try self.bindLiteralGuardPattern(conversion, ty)
            else
                Common.invariant("small decimal pattern reached Monotype after numeric finalization"),
            .dec_literal => |dec| if (dec.conversion) |conversion|
                try self.bindLiteralGuardPattern(conversion, ty)
            else
                .{ .dec_lit = dec.value },
            .frac_f32_literal => |value| .{ .frac_f32_lit = value },
            .frac_f64_literal => |value| .{ .frac_f64_lit = value },
            .str_literal => |str| if (str.conversion) |conversion|
                try self.bindLiteralGuardPattern(conversion, ty)
            else
                .{ .str_lit = try self.lowerStringLiteral(str.literal) },
            .str_interpolation => |str| try self.lowerStrPattern(str, ty),
            .underscore => .wildcard,
        };
        return try self.builder.program.addPat(.{ .ty = ty, .data = data });
    }

    fn lowerStrPattern(
        self: *BodyContext,
        str: anytype,
        ty: Type.TypeId,
    ) Allocator.Error!Ast.PatData {
        const steps = try self.allocator.alloc(Ast.StrPatternStep, str.steps.len);
        defer self.allocator.free(steps);

        for (str.steps, 0..) |step, i| {
            steps[i] = .{
                .capture = if (step.capture) |capture| try self.lowerPatternAtType(capture, ty) else null,
                .delimiter = try self.lowerStringLiteral(step.delimiter),
            };
        }

        return .{ .str_pattern = .{
            .prefix = try self.lowerStringLiteral(str.prefix),
            .steps = try self.builder.program.addStrPatternStepSpan(steps),
            .end = switch (str.end) {
                .exact => .exact,
                .tail => .tail,
            },
        } };
    }

    fn lowerPatternSpanAtTypes(
        self: *BodyContext,
        checked_patterns: []const checked.CheckedPatternId,
        tys: []const Type.TypeId,
    ) Allocator.Error!Ast.Span(Ast.PatId) {
        if (checked_patterns.len != tys.len) Common.invariant("pattern arity differs from concrete checked type");
        const lowered = try self.allocator.alloc(Ast.PatId, checked_patterns.len);
        defer self.allocator.free(lowered);
        for (checked_patterns, tys, 0..) |child, child_ty, i| {
            lowered[i] = try self.lowerPatternAtType(child, child_ty);
        }
        return try self.builder.program.addPatSpan(lowered);
    }

    fn lowerTuplePattern(self: *BodyContext, items: []const checked.CheckedPatternId, ty: Type.TypeId) Allocator.Error!Ast.Span(Ast.PatId) {
        return try self.lowerPatternSpanAtTypes(items, self.builder.tupleItemTypes(ty));
    }

    fn lowerListPattern(self: *BodyContext, list: anytype, ty: Type.TypeId) Allocator.Error!Ast.PatData {
        const elem_ty = self.constListElemType(ty);
        const lowered = try self.allocator.alloc(Ast.PatId, list.patterns.len);
        defer self.allocator.free(lowered);
        for (list.patterns, 0..) |child, i| {
            lowered[i] = try self.lowerPatternAtType(child, elem_ty);
        }
        const rest: ?Ast.ListRestPattern = if (list.rest) |r| .{
            .index = r.index,
            // A captured rest binds the remaining slice, which has the same
            // list type as the scrutinee.
            .pattern = if (r.pattern) |rest_pattern| try self.lowerPatternAtType(rest_pattern, ty) else null,
        } else null;
        return .{ .list = .{
            .patterns = try self.builder.program.addPatSpan(lowered),
            .rest = rest,
        } };
    }

    fn lowerTagPattern(self: *BodyContext, tag: anytype, ty: Type.TypeId) Allocator.Error!Ast.PatData {
        const name = try self.builder.tagName(self.view, tag.name);
        return .{ .tag = .{
            .name = name,
            .payloads = try self.lowerPatternSpanAtTypes(tag.args, self.builder.tagPayloadTypes(ty, name)),
        } };
    }

    fn lowerRecordPattern(self: *BodyContext, destructs: []const checked.CheckedRecordDestruct, ty: Type.TypeId) Allocator.Error!Ast.PatData {
        var lowered = std.ArrayList(Ast.RecordDestruct).empty;
        defer lowered.deinit(self.allocator);
        for (destructs) |destruct| {
            const child = switch (destruct.kind) {
                .required => |pattern| pattern,
                .sub_pattern => |pattern| pattern,
                .rest => |pattern| {
                    if (self.patternIsIgnored(pattern)) continue;
                    Common.invariant("record rest pattern must be lowered to explicit rest-record construction before Monotype output");
                },
            };
            const name = try self.builder.recordFieldName(self.view, destruct.label);
            const child_ty = switch (destruct.kind) {
                .required, .sub_pattern => self.builder.recordFieldType(ty, name),
                .rest => unreachable,
            };
            try lowered.append(self.allocator, .{
                .name = name,
                .pattern = try self.lowerPatternAtType(child, child_ty),
            });
        }
        return .{ .record = try self.builder.program.addRecordDestructSpan(lowered.items) };
    }

    fn patternIsIgnored(self: *BodyContext, pattern_id: checked.CheckedPatternId) bool {
        return switch (self.view.bodies.pattern(pattern_id).data) {
            .underscore => true,
            else => false,
        };
    }

    /// Lower a literal pattern on a non-builtin number type to a fresh bind,
    /// recording an equality condition for the enclosing match branch.
    fn bindLiteralGuardPattern(
        self: *BodyContext,
        conversion: checked.CheckedExprId,
        ty: Type.TypeId,
    ) Allocator.Error!Ast.PatData {
        const local = try self.builder.program.addLocal(self.builder.symbols.fresh(), ty);
        try self.pattern_literal_guards.append(self.allocator, .{
            .local = local,
            .conversion = conversion,
            .ty = ty,
        });
        return .{ .bind = local };
    }

    /// Compare a bound match value against a literal's converted constant,
    /// dispatching to the type's `is_eq` method when it has one and falling
    /// back to structural equality otherwise, mirroring `==`.
    fn lowerPatternLiteralEq(self: *BodyContext, entry: PatternLiteralGuard) Allocator.Error!Ast.ExprId {
        const scrutinee = try self.builder.localExpr(entry.local, entry.ty);
        const expected = try self.lowerExpr(entry.conversion);
        if (methodOwnerFromType(&self.builder.program.types, entry.ty)) |owner| {
            if (self.builder.lookupMethodTargetByName(owner, "is_eq")) |lookup| {
                var target_ctx = try self.methodTargetContext(lookup);
                defer target_ctx.deinit();
                const target_fn = target_ctx.checkedFunctionType(lookup.target.callable_ty);
                const bool_ty = try self.builder.lowerType(lookup.view, target_fn.ret);
                const arg_tys = [_]Type.TypeId{ entry.ty, entry.ty };
                const callable_mono_ty = try self.methodTargetMonoTypeFromArgs(lookup, &arg_tys, bool_ty);
                const callee = try self.methodTargetCalleeWithMono(lookup, callable_mono_ty);
                return try self.builder.program.addExpr(.{ .ty = bool_ty, .data = .{ .call_proc = .{
                    .callee = .{ .func = callee },
                    .args = try self.builder.program.addExprSpan(&.{ scrutinee, expected }),
                } } });
            }
        }
        return try self.lowerEqualityExpr(entry.ty, scrutinee, expected, "is_eq", try self.builder.primitiveType(.bool));
    }

    /// Take ownership of the literal-equality conditions collected since
    /// `start`, restoring the collection to that length.
    fn drainPatternLiteralGuards(self: *BodyContext, start: usize) Allocator.Error![]PatternLiteralGuard {
        const drained = try self.allocator.dupe(PatternLiteralGuard, self.pattern_literal_guards.items[start..]);
        self.pattern_literal_guards.shrinkRetainingCapacity(start);
        return drained;
    }

    /// Wrap a match-branch body so it only runs when every collected literal
    /// equality holds, jumping to the branch's miss target otherwise.
    fn applyPatternLiteralGuards(
        self: *BodyContext,
        guards: []const PatternLiteralGuard,
        body: Ast.ExprId,
        fallback: Ast.ExprId,
        output_ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        var result = body;
        var i = guards.len;
        while (i > 0) {
            i -= 1;
            const eq = try self.lowerPatternLiteralEq(guards[i]);
            result = try self.builder.ifExpr(eq, result, fallback, output_ty);
        }
        return result;
    }

    /// Conjoin the branch's collected literal-equality conditions with its
    /// (optional) user guard, literal conditions first.
    fn conjoinPatternLiteralGuards(self: *BodyContext, user_guard: ?Ast.ExprId) Allocator.Error!?Ast.ExprId {
        if (self.pattern_literal_guards.items.len == 0) return user_guard;
        const bool_ty = try self.builder.primitiveType(.bool);
        var cond = user_guard;
        var i = self.pattern_literal_guards.items.len;
        while (i > 0) {
            i -= 1;
            const eq = try self.lowerPatternLiteralEq(self.pattern_literal_guards.items[i]);
            cond = if (cond) |inner|
                try self.builder.ifExpr(eq, inner, try self.boolLiteral(false, bool_ty), bool_ty)
            else
                eq;
        }
        self.pattern_literal_guards.clearRetainingCapacity();
        return cond;
    }

    fn lowerNumPattern(self: *BodyContext, value: can.CIR.IntValue, ty: Type.TypeId) Ast.PatData {
        return if (self.builder.typeIsDec(ty))
            .{ .dec_lit = intValueToDec(value) }
        else
            .{ .int_lit = value };
    }

    fn lowerIntLiteral(self: *BodyContext, value: can.CIR.IntValue, ty: Type.TypeId) Ast.ExprData {
        return switch (self.builder.shapeContent(ty)) {
            .primitive => |primitive| switch (primitive) {
                .f32 => .{ .frac_f32_lit = @floatCast(intValueToF64(value)) },
                .f64 => .{ .frac_f64_lit = intValueToF64(value) },
                .dec => .{ .dec_lit = intValueToDec(value) },
                else => .{ .int_lit = value },
            },
            else => .{ .int_lit = value },
        };
    }

    /// Lower a fractional literal to the constant form its instantiated
    /// monotype demands. The checked stage finalizes a fractional literal's
    /// value to one numeric representation, but a generalized literal can be
    /// instantiated at a different fractional primitive than its finalized
    /// default (for example, a literal finalized to `Dec` that this
    /// specialization unifies to `F64`). The constant kind must follow the
    /// instantiated type so backends store bits the destination layout
    /// expects, mirroring `lowerIntLiteral`.
    fn lowerFracLiteral(self: *BodyContext, value: FracLiteralValue, ty: Type.TypeId) Ast.ExprData {
        return switch (self.builder.shapeContent(ty)) {
            .primitive => |primitive| switch (primitive) {
                .f32 => .{ .frac_f32_lit = value.asF32() },
                .f64 => .{ .frac_f64_lit = value.asF64() },
                .dec => .{ .dec_lit = value.asDec() },
                else => Common.invariant("fractional literal instantiated to a non-fractional Monotype primitive"),
            },
            else => Common.invariant("fractional literal instantiated to a non-primitive Monotype type"),
        };
    }
};

/// Structural `is_eq` specifics for the generic derivation driver
/// (`BodyContext.lowerDerivation`). Equality threads two operands, builds a
/// component access on each, and conjoins the per-component bools with AND so
/// the first inequality short-circuits to `false`.
const EqDeriver = struct {
    const Operand = struct {
        lhs: Ast.ExprId,
        rhs: Ast.ExprId,
    };

    /// Right-to-left so the AND fold nests the later components inside the
    /// earlier `if` bodies, matching source field/element order.
    const forward = false;

    fn expansionStack(self: *BodyContext) *std.AutoHashMap(Type.TypeId, void) {
        return &self.equality_expansion_stack;
    }

    fn defCache(self: *BodyContext) *std.AutoHashMap(GeneratedHelperDefAddress, GeneratedHelperDefEntry) {
        return &self.builder.equality_defs;
    }

    fn defAddress(value_ty: Type.TypeId, result_ty: Type.TypeId) GeneratedHelperDefAddress {
        return .{ .value_ty = @intFromEnum(value_ty), .result_ty = @intFromEnum(result_ty) };
    }

    fn fnType(self: *BodyContext, value_ty: Type.TypeId, result_ty: Type.TypeId) Allocator.Error!Type.TypeId {
        return try self.builder.twoArgFnType(value_ty, result_ty);
    }

    fn callArgs(operand: Operand) [2]Ast.ExprId {
        return .{ operand.lhs, operand.rhs };
    }

    fn helperSecondArgType(value_ty: Type.TypeId, _: Type.TypeId) Type.TypeId {
        return value_ty;
    }

    fn helperOperand(self_expr: Ast.ExprId, aux_expr: Ast.ExprId) Operand {
        return .{ .lhs = self_expr, .rhs = aux_expr };
    }

    fn leaf(self: *BodyContext, operand: Operand, ctx: BodyContext.DerivationCtx) Allocator.Error!Ast.ExprId {
        return try self.builder.program.addExpr(.{ .ty = ctx.result_ty, .data = .{ .structural_eq = .{
            .lhs = operand.lhs,
            .rhs = operand.rhs,
            .negated = false,
        } } });
    }

    fn combineSeed(self: *BodyContext, _: Operand, ctx: BodyContext.DerivationCtx) Allocator.Error!Ast.ExprId {
        return try self.boolLiteral(true, ctx.result_ty);
    }

    fn combine(self: *BodyContext, state: Ast.ExprId, component: Ast.ExprId, ctx: BodyContext.DerivationCtx) Allocator.Error!Ast.ExprId {
        return try self.builder.ifExpr(component, state, try self.boolLiteral(false, ctx.result_ty), ctx.result_ty);
    }

    fn componentForField(self: *BodyContext, operand: Operand, _: Ast.ExprId, field: Type.Field) Allocator.Error!Operand {
        const lhs_field = try self.builder.program.addExpr(.{ .ty = field.ty, .data = .{ .field_access = .{
            .receiver = operand.lhs,
            .field = field.name,
        } } });
        const rhs_field = try self.builder.program.addExpr(.{ .ty = field.ty, .data = .{ .field_access = .{
            .receiver = operand.rhs,
            .field = field.name,
        } } });
        return .{ .lhs = lhs_field, .rhs = rhs_field };
    }

    fn componentForTuple(self: *BodyContext, operand: Operand, _: Ast.ExprId, item_ty: Type.TypeId, index: usize) Allocator.Error!Operand {
        const lhs_item = try self.builder.program.addExpr(.{ .ty = item_ty, .data = .{ .tuple_access = .{
            .tuple = operand.lhs,
            .elem_index = @intCast(index),
        } } });
        const rhs_item = try self.builder.program.addExpr(.{ .ty = item_ty, .data = .{ .tuple_access = .{
            .tuple = operand.rhs,
            .elem_index = @intCast(index),
        } } });
        return .{ .lhs = lhs_item, .rhs = rhs_item };
    }

    const owned_missing_owner_msg = "owned equality call requested for a type without a method owner";
    const owned_missing_target_msg = "checked method registry is missing owned equality target";

    fn ownedArgTypes(ty: Type.TypeId, _: Type.TypeId) [2]Type.TypeId {
        return .{ ty, ty };
    }

    fn ownedCall(self: *BodyContext, ty: Type.TypeId, operand: Operand, ctx: BodyContext.DerivationCtx) Allocator.Error!Ast.ExprId {
        return try self.derivationOwnedCall(EqDeriver, ty, operand, ctx);
    }

    /// Decomposes structural equality on a nominal type by unwrapping both operands to
    /// their shared backing representation and comparing those. The nominal unwrap is a
    /// transparent alias at runtime, and recursing on the backing dispatches any owned
    /// types nested within it (e.g. a list inside the backing tag union) instead of
    /// leaving them for the LIR structural-equality lowering.
    fn named(self: *BodyContext, named_ty: Type.TypeId, backing_ty: Type.TypeId, operand: Operand, ctx: BodyContext.DerivationCtx) Allocator.Error!Ast.ExprId {
        const lhs_inner = try self.builder.program.addLocal(self.builder.symbols.fresh(), backing_ty);
        const rhs_inner = try self.builder.program.addLocal(self.builder.symbols.fresh(), backing_ty);

        const compare = try self.lowerDerivation(EqDeriver, backing_ty, .{
            .lhs = try self.builder.localExpr(lhs_inner, backing_ty),
            .rhs = try self.builder.localExpr(rhs_inner, backing_ty),
        }, ctx);

        const lhs_pat = try self.builder.program.addPat(.{ .ty = named_ty, .data = .{ .nominal = try self.builder.bindPat(lhs_inner, backing_ty) } });
        const rhs_pat = try self.builder.program.addPat(.{ .ty = named_ty, .data = .{ .nominal = try self.builder.bindPat(rhs_inner, backing_ty) } });

        const bind_rhs = try self.builder.program.addExpr(.{ .ty = ctx.result_ty, .data = .{ .let_ = .{
            .bind = rhs_pat,
            .value = operand.rhs,
            .rest = compare,
        } } });
        return try self.builder.program.addExpr(.{ .ty = ctx.result_ty, .data = .{ .let_ = .{
            .bind = lhs_pat,
            .value = operand.lhs,
            .rest = bind_rhs,
        } } });
    }

    /// Decomposes structural equality on a tag union into nested matches so that each
    /// payload is compared. This dispatches owned payload types (e.g. List) to their
    /// `is_eq` methods, matching how record and tuple fields are handled. Leaving tag
    /// unions as a `structural_eq` node would defer the payload comparison to the LIR
    /// structural-equality lowering, which can only compare inline-comparable types and
    /// panics on owned types such as lists.
    fn tagUnion(self: *BodyContext, ty: Type.TypeId, tags_span: Type.Span, operand: Operand, ctx: BodyContext.DerivationCtx) Allocator.Error!Ast.ExprId {
        // Bind both operands to locals so each is evaluated exactly once and the
        // right-hand side can be re-scrutinised inside every left-hand-side branch.
        const lhs_local = try self.builder.program.addLocal(self.builder.symbols.fresh(), ty);
        const rhs_local = try self.builder.program.addLocal(self.builder.symbols.fresh(), ty);

        // Copy the tag list because recursive lowerDerivation may reallocate type spans.
        const tags = try self.allocator.dupe(Type.Tag, self.builder.program.types.tagSpan(tags_span));
        defer self.allocator.free(tags);

        const branches = try self.allocator.alloc(Ast.Branch, tags.len);
        defer self.allocator.free(branches);
        for (tags, 0..) |tag, branch_index| {
            branches[branch_index] = try tagBranch(self, ty, rhs_local, tag, tags.len == 1, ctx);
        }

        const match_expr = try self.builder.program.addExpr(.{ .ty = ctx.result_ty, .data = .{ .match_ = .{
            .scrutinee = try self.builder.localExpr(lhs_local, ty),
            .branches = try self.builder.program.addBranchSpan(branches),
        } } });

        const bind_rhs = try self.builder.program.addExpr(.{ .ty = ctx.result_ty, .data = .{ .let_ = .{
            .bind = try self.builder.bindPat(rhs_local, ty),
            .value = operand.rhs,
            .rest = match_expr,
        } } });
        return try self.builder.program.addExpr(.{ .ty = ctx.result_ty, .data = .{ .let_ = .{
            .bind = try self.builder.bindPat(lhs_local, ty),
            .value = operand.lhs,
            .rest = bind_rhs,
        } } });
    }

    /// Builds one branch of the outer match in `tagUnion`: it matches the left operand
    /// against `tag`, binding its payloads, then matches the right operand against the
    /// same tag. When both sides carry `tag`, the payloads are compared pairwise; any
    /// other right-hand variant yields `false`.
    fn tagBranch(self: *BodyContext, ty: Type.TypeId, rhs_local: Ast.LocalId, tag: Type.Tag, single_variant: bool, ctx: BodyContext.DerivationCtx) Allocator.Error!Ast.Branch {
        // Copy payload types because recursive lowerDerivation may reallocate type spans.
        const payloads = try self.allocator.dupe(Type.TypeId, self.builder.program.types.span(tag.payloads));
        defer self.allocator.free(payloads);

        const lhs_pats = try self.allocator.alloc(Ast.PatId, payloads.len);
        defer self.allocator.free(lhs_pats);
        const rhs_pats = try self.allocator.alloc(Ast.PatId, payloads.len);
        defer self.allocator.free(rhs_pats);
        const lhs_exprs = try self.allocator.alloc(Ast.ExprId, payloads.len);
        defer self.allocator.free(lhs_exprs);
        const rhs_exprs = try self.allocator.alloc(Ast.ExprId, payloads.len);
        defer self.allocator.free(rhs_exprs);
        for (payloads, 0..) |payload_ty, i| {
            const lhs_payload_local = try self.builder.program.addLocal(self.builder.symbols.fresh(), payload_ty);
            const rhs_payload_local = try self.builder.program.addLocal(self.builder.symbols.fresh(), payload_ty);
            lhs_pats[i] = try self.builder.bindPat(lhs_payload_local, payload_ty);
            rhs_pats[i] = try self.builder.bindPat(rhs_payload_local, payload_ty);
            lhs_exprs[i] = try self.builder.localExpr(lhs_payload_local, payload_ty);
            rhs_exprs[i] = try self.builder.localExpr(rhs_payload_local, payload_ty);
        }

        // Conjunction of the pairwise payload comparisons, defaulting to true (a variant
        // with no payloads is equal once both discriminants match).
        var body = try self.boolLiteral(true, ctx.result_ty);
        var i = payloads.len;
        while (i > 0) {
            i -= 1;
            const payload_eq = try self.lowerDerivation(EqDeriver, payloads[i], .{ .lhs = lhs_exprs[i], .rhs = rhs_exprs[i] }, ctx);
            body = try self.builder.ifExpr(payload_eq, body, try self.boolLiteral(false, ctx.result_ty), ctx.result_ty);
        }

        const rhs_tag_pat = try self.builder.program.addPat(.{ .ty = ty, .data = .{ .tag = .{
            .name = tag.name,
            .payloads = try self.builder.program.addPatSpan(rhs_pats),
        } } });
        const rhs_scrutinee = try self.builder.localExpr(rhs_local, ty);
        const inner_match = if (single_variant) blk: {
            const inner_branches = [_]Ast.Branch{.{ .pat = rhs_tag_pat, .body = body }};
            break :blk try self.builder.program.addExpr(.{ .ty = ctx.result_ty, .data = .{ .match_ = .{
                .scrutinee = rhs_scrutinee,
                .branches = try self.builder.program.addBranchSpan(&inner_branches),
            } } });
        } else blk: {
            const wildcard = try self.builder.program.addPat(.{ .ty = ty, .data = .wildcard });
            const inner_branches = [_]Ast.Branch{
                .{ .pat = rhs_tag_pat, .body = body },
                .{ .pat = wildcard, .body = try self.boolLiteral(false, ctx.result_ty) },
            };
            break :blk try self.builder.program.addExpr(.{ .ty = ctx.result_ty, .data = .{ .match_ = .{
                .scrutinee = rhs_scrutinee,
                .branches = try self.builder.program.addBranchSpan(&inner_branches),
            } } });
        };

        const lhs_tag_pat = try self.builder.program.addPat(.{ .ty = ty, .data = .{ .tag = .{
            .name = tag.name,
            .payloads = try self.builder.program.addPatSpan(lhs_pats),
        } } });
        return .{ .pat = lhs_tag_pat, .body = inner_match };
    }
};

/// Structural `to_hash` specifics for the generic derivation driver. Hashing
/// threads a single value plus a running Hasher accumulator: each component
/// feeds the hasher accumulated so far, left to right.
const HashDeriver = struct {
    const Operand = struct {
        value: Ast.ExprId,
        hasher: Ast.ExprId,
    };

    /// Left-to-right: each component hashes into the accumulator threaded from
    /// the previous one.
    const forward = true;

    fn expansionStack(self: *BodyContext) *std.AutoHashMap(Type.TypeId, void) {
        return &self.hash_expansion_stack;
    }

    fn defCache(self: *BodyContext) *std.AutoHashMap(GeneratedHelperDefAddress, GeneratedHelperDefEntry) {
        return &self.builder.hash_defs;
    }

    fn defAddress(value_ty: Type.TypeId, result_ty: Type.TypeId) GeneratedHelperDefAddress {
        return .{ .value_ty = @intFromEnum(value_ty), .result_ty = @intFromEnum(result_ty) };
    }

    fn fnType(self: *BodyContext, value_ty: Type.TypeId, result_ty: Type.TypeId) Allocator.Error!Type.TypeId {
        return try self.builder.hashFnType(value_ty, result_ty);
    }

    fn callArgs(operand: Operand) [2]Ast.ExprId {
        return .{ operand.value, operand.hasher };
    }

    fn helperSecondArgType(_: Type.TypeId, result_ty: Type.TypeId) Type.TypeId {
        return result_ty;
    }

    fn helperOperand(self_expr: Ast.ExprId, aux_expr: Ast.ExprId) Operand {
        return .{ .value = self_expr, .hasher = aux_expr };
    }

    fn leaf(self: *BodyContext, operand: Operand, ctx: BodyContext.DerivationCtx) Allocator.Error!Ast.ExprId {
        return try self.builder.program.addExpr(.{ .ty = ctx.result_ty, .data = .{ .structural_hash = .{
            .value = operand.value,
            .hasher = operand.hasher,
        } } });
    }

    fn combineSeed(_: *BodyContext, operand: Operand, _: BodyContext.DerivationCtx) Allocator.Error!Ast.ExprId {
        return operand.hasher;
    }

    fn combine(_: *BodyContext, _: Ast.ExprId, component: Ast.ExprId, _: BodyContext.DerivationCtx) Allocator.Error!Ast.ExprId {
        return component;
    }

    fn componentForField(self: *BodyContext, operand: Operand, state: Ast.ExprId, field: Type.Field) Allocator.Error!Operand {
        const field_value = try self.builder.program.addExpr(.{ .ty = field.ty, .data = .{ .field_access = .{
            .receiver = operand.value,
            .field = field.name,
        } } });
        return .{ .value = field_value, .hasher = state };
    }

    fn componentForTuple(self: *BodyContext, operand: Operand, state: Ast.ExprId, item_ty: Type.TypeId, index: usize) Allocator.Error!Operand {
        const item_value = try self.builder.program.addExpr(.{ .ty = item_ty, .data = .{ .tuple_access = .{
            .tuple = operand.value,
            .elem_index = @intCast(index),
        } } });
        return .{ .value = item_value, .hasher = state };
    }

    const owned_missing_owner_msg = "owned hash call requested for a type without a method owner";
    const owned_missing_target_msg = "checked method registry is missing owned to_hash target";

    fn ownedArgTypes(ty: Type.TypeId, result_ty: Type.TypeId) [2]Type.TypeId {
        return .{ ty, result_ty };
    }

    fn ownedCall(self: *BodyContext, ty: Type.TypeId, operand: Operand, ctx: BodyContext.DerivationCtx) Allocator.Error!Ast.ExprId {
        return try self.derivationOwnedCall(HashDeriver, ty, operand, ctx);
    }

    fn named(self: *BodyContext, named_ty: Type.TypeId, backing_ty: Type.TypeId, operand: Operand, ctx: BodyContext.DerivationCtx) Allocator.Error!Ast.ExprId {
        const inner = try self.builder.program.addLocal(self.builder.symbols.fresh(), backing_ty);
        const hashed = try self.lowerDerivation(HashDeriver, backing_ty, .{
            .value = try self.builder.localExpr(inner, backing_ty),
            .hasher = operand.hasher,
        }, ctx);
        const pat = try self.builder.program.addPat(.{ .ty = named_ty, .data = .{ .nominal = try self.builder.bindPat(inner, backing_ty) } });
        return try self.builder.program.addExpr(.{ .ty = ctx.result_ty, .data = .{ .let_ = .{
            .bind = pat,
            .value = operand.value,
            .rest = hashed,
        } } });
    }

    /// Decompose a tag-union hash: write the discriminant index into the hasher,
    /// then match on the value and thread the active variant's payloads through.
    fn tagUnion(self: *BodyContext, value_ty: Type.TypeId, tags_span: Type.Span, operand: Operand, ctx: BodyContext.DerivationCtx) Allocator.Error!Ast.ExprId {
        // Bind the value to a local so it is matched exactly once.
        const value_local = try self.builder.program.addLocal(self.builder.symbols.fresh(), value_ty);

        // Copy the tag list because recursive lowerDerivation may reallocate type spans.
        const tags = try self.allocator.dupe(Type.Tag, self.builder.program.types.tagSpan(tags_span));
        defer self.allocator.free(tags);

        const branches = try self.allocator.alloc(Ast.Branch, tags.len);
        defer self.allocator.free(branches);
        for (tags, 0..) |tag, index| {
            branches[index] = try tagBranch(self, value_ty, tag, @intCast(index), operand.hasher, ctx);
        }

        const match_expr = try self.builder.program.addExpr(.{ .ty = ctx.result_ty, .data = .{ .match_ = .{
            .scrutinee = try self.builder.localExpr(value_local, value_ty),
            .branches = try self.builder.program.addBranchSpan(branches),
        } } });

        return try self.builder.program.addExpr(.{ .ty = ctx.result_ty, .data = .{ .let_ = .{
            .bind = try self.builder.bindPat(value_local, value_ty),
            .value = operand.value,
            .rest = match_expr,
        } } });
    }

    fn tagBranch(self: *BodyContext, value_ty: Type.TypeId, tag: Type.Tag, variant_index: u64, hasher: Ast.ExprId, ctx: BodyContext.DerivationCtx) Allocator.Error!Ast.Branch {
        // Copy payload types because recursive lowerDerivation may reallocate type spans.
        const payloads = try self.allocator.dupe(Type.TypeId, self.builder.program.types.span(tag.payloads));
        defer self.allocator.free(payloads);

        const pats = try self.allocator.alloc(Ast.PatId, payloads.len);
        defer self.allocator.free(pats);
        const payload_exprs = try self.allocator.alloc(Ast.ExprId, payloads.len);
        defer self.allocator.free(payload_exprs);
        for (payloads, 0..) |payload_ty, i| {
            const payload_local = try self.builder.program.addLocal(self.builder.symbols.fresh(), payload_ty);
            pats[i] = try self.builder.bindPat(payload_local, payload_ty);
            payload_exprs[i] = try self.builder.localExpr(payload_local, payload_ty);
        }

        // First write the discriminant index, then thread each payload's hash.
        var acc = try self.hasherWriteU64(hasher, variant_index, ctx.result_ty);
        for (payloads, 0..) |payload_ty, i| {
            acc = try self.lowerDerivation(HashDeriver, payload_ty, .{ .value = payload_exprs[i], .hasher = acc }, ctx);
        }

        const tag_pat = try self.builder.program.addPat(.{ .ty = value_ty, .data = .{ .tag = .{
            .name = tag.name,
            .payloads = try self.builder.program.addPatSpan(pats),
        } } });
        return .{ .pat = tag_pat, .body = acc };
    }
};

/// A fractional literal value in its checked-stage representation, used to
/// re-derive the constant kind the instantiated Monotype primitive requires.
const FracLiteralValue = union(enum) {
    f32: f32,
    f64: f64,
    dec: builtins.dec.RocDec,

    fn asF64(self: FracLiteralValue) f64 {
        return switch (self) {
            .f32 => |v| @floatCast(v),
            .f64 => |v| v,
            .dec => |v| v.toF64(),
        };
    }

    fn asF32(self: FracLiteralValue) f32 {
        return switch (self) {
            .f32 => |v| v,
            .f64 => |v| @floatCast(v),
            .dec => |v| @floatCast(v.toF64()),
        };
    }

    fn asDec(self: FracLiteralValue) builtins.dec.RocDec {
        return switch (self) {
            .f32 => |v| builtins.dec.RocDec.fromF64(@floatCast(v)) orelse
                Common.invariant("f32 fractional literal could not be represented as Dec at its instantiated type"),
            .f64 => |v| builtins.dec.RocDec.fromF64(v) orelse
                Common.invariant("f64 fractional literal could not be represented as Dec at its instantiated type"),
            .dec => |v| v,
        };
    }
};

/// Record a local's source-level name from its pattern binder. An `assign`
/// pattern's region is exactly the identifier token's span, so the name is
/// the source text at that region. Binders from other pattern forms (`as`)
/// stay unnamed.
fn bindLocalName(
    program: *Ast.Program,
    view: ModuleView,
    local: Ast.LocalId,
    binder: checked.PatternBinderId,
) Allocator.Error!void {
    const entry = view.bodies.patternBinder(binder);
    const pattern = view.bodies.pattern(entry.pattern);
    if (pattern.data != .assign) return;
    const source = view.module_env.common.source;
    const start = pattern.source_region.start.offset;
    const end = pattern.source_region.end.offset;
    if (start >= end or end > source.len) return;
    try program.setLocalName(local, source[start..end]);
}

fn moduleView(view: checked.ImportedModuleView) ModuleView {
    return .{
        .key = view.key,
        .module_env = view.module_env,
        .module_identity = view.module_identity,
        .names = checked.importedNames(view),
        .types = view.checked_types,
        .bodies = view.checked_bodies,
        .exhaustiveness_sites = view.exhaustiveness_sites,
        .checked_const_bodies = view.checked_const_bodies,
        .templates = view.checked_procedure_templates,
        .compile_time_roots = view.compile_time_roots,
        .entry_wrappers = view.entry_wrappers,
        .intrinsic_wrappers = view.intrinsic_wrappers,
        .hosted_procs = view.hosted_procs,
        .static_dispatch_plans = view.static_dispatch_plans,
        .method_registry = view.method_registry,
        .resolved_refs = view.resolved_value_refs,
        .nested_proc_sites = view.nested_proc_sites,
        .exported_procedure_bindings = view.exported_procedure_bindings,
        .exported_const_templates = view.exported_const_templates,
        .top_level_procedure_bindings = view.top_level_procedure_bindings,
        .platform_required_bindings = view.platform_required_bindings,
        .callable_eval_templates = view.callable_eval_templates,
        .hoisted_constants = view.hoisted_constants,
        .const_templates = view.const_templates,
        .const_store = view.const_store,
        .interface_capabilities = view.interface_capabilities,
    };
}

fn moduleViewNameMatches(view: ModuleView, module_name: []const u8) bool {
    return Ident.textEql(module_name, view.names.moduleNameText(view.module_identity.module_name)) or
        Ident.textEql(module_name, view.names.moduleNameText(view.module_identity.display_module_name)) or
        Ident.textEql(module_name, view.names.moduleNameText(view.module_identity.qualified_module_name));
}

fn restoreScalar(scalar: checked.ConstScalar) Ast.ExprData {
    return switch (scalar) {
        .i8 => |value| .{ .int_lit = signedIntLiteral(value) },
        .i16 => |value| .{ .int_lit = signedIntLiteral(value) },
        .i32 => |value| .{ .int_lit = signedIntLiteral(value) },
        .i64 => |value| .{ .int_lit = signedIntLiteral(value) },
        .i128 => |value| .{ .int_lit = signedIntLiteral(value) },
        .u8 => |value| .{ .int_lit = unsignedIntLiteral(value) },
        .u16 => |value| .{ .int_lit = unsignedIntLiteral(value) },
        .u32 => |value| .{ .int_lit = unsignedIntLiteral(value) },
        .u64 => |value| .{ .int_lit = unsignedIntLiteral(value) },
        .u128 => |value| .{ .int_lit = unsignedIntLiteral(value) },
        .f32_bits => |bits| .{ .frac_f32_lit = @bitCast(bits) },
        .f64_bits => |bits| .{ .frac_f64_lit = @bitCast(bits) },
        .dec_bits => |bits| .{ .dec_lit = .{ .num = bits } },
    };
}

/// Parse a numeral's decimal text into a constant literal of the
/// concrete numeric `primitive`, mirroring the interpreter's `parseNumeralPayload`
/// (`std.fmt.parseInt`/`parseFloat`/`RocDec.fromNonemptySlice`). Returns null
/// when the value does not fit the target representation (out of range, or a
/// fractional text parsed as an integer), so the caller can lower the Err branch.
fn foldNumeralPrimitive(primitive: Type.Primitive, text: []const u8) ?Ast.ExprData {
    return switch (primitive) {
        .u8 => foldUnsignedNumeral(u8, text),
        .u16 => foldUnsignedNumeral(u16, text),
        .u32 => foldUnsignedNumeral(u32, text),
        .u64 => foldUnsignedNumeral(u64, text),
        .u128 => foldUnsignedNumeral(u128, text),
        .i8 => foldSignedNumeral(i8, text),
        .i16 => foldSignedNumeral(i16, text),
        .i32 => foldSignedNumeral(i32, text),
        .i64 => foldSignedNumeral(i64, text),
        .i128 => foldSignedNumeral(i128, text),
        .f32 => if (std.fmt.parseFloat(f32, text)) |v| .{ .frac_f32_lit = v } else |_| null,
        .f64 => if (std.fmt.parseFloat(f64, text)) |v| .{ .frac_f64_lit = v } else |_| null,
        .dec => if (builtins.dec.RocDec.fromNonemptySlice(text)) |d| .{ .dec_lit = .{ .num = d.num } } else null,
        .bool, .str => Common.invariant("from_numeral target was a non-numeric primitive"),
    };
}

fn foldUnsignedNumeral(comptime T: type, text: []const u8) ?Ast.ExprData {
    const value = std.fmt.parseInt(T, text, 10) catch return null;
    return .{ .int_lit = unsignedIntLiteral(value) };
}

fn foldSignedNumeral(comptime T: type, text: []const u8) ?Ast.ExprData {
    const value = std.fmt.parseInt(T, text, 10) catch return null;
    return .{ .int_lit = signedIntLiteral(value) };
}

fn signedIntLiteral(value: anytype) can.CIR.IntValue {
    const widened: i128 = @intCast(value);
    return .{ .bytes = @bitCast(widened), .kind = .i128 };
}

fn unsignedIntLiteral(value: anytype) can.CIR.IntValue {
    const widened: u128 = @intCast(value);
    return .{ .bytes = @bitCast(widened), .kind = .u128 };
}

fn generatedInterpolationStepKey(
    current_fn_key: names.TypeDigest,
    source_expr_id: checked.CheckedExprId,
    index: usize,
) names.TypeDigest {
    var hasher = std.crypto.hash.sha2.Sha256.init(.{});
    hasher.update("roc.generated_interpolation_step");
    hasher.update(&current_fn_key.bytes);
    var source_expr_bytes = std.mem.nativeToLittle(u32, @intFromEnum(source_expr_id));
    hasher.update(std.mem.asBytes(&source_expr_bytes));
    var index_bytes = std.mem.nativeToLittle(u64, @intCast(index));
    hasher.update(std.mem.asBytes(&index_bytes));
    return .{ .bytes = hasher.finalResult() };
}

fn generatedParserRuntimeKey(
    current_fn_key: names.TypeDigest,
    source_expr_id: checked.CheckedExprId,
) names.TypeDigest {
    var hasher = std.crypto.hash.sha2.Sha256.init(.{});
    hasher.update("roc.generated_structural_parser_runtime");
    hasher.update(&current_fn_key.bytes);
    var source_expr_bytes = std.mem.nativeToLittle(u32, @intFromEnum(source_expr_id));
    hasher.update(std.mem.asBytes(&source_expr_bytes));
    return .{ .bytes = hasher.finalResult() };
}

fn generatedEncodeToRuntimeKey(
    current_fn_key: names.TypeDigest,
    source_expr_id: checked.CheckedExprId,
) names.TypeDigest {
    var hasher = std.crypto.hash.sha2.Sha256.init(.{});
    hasher.update("roc.generated_structural_encode_to_runtime");
    hasher.update(&current_fn_key.bytes);
    var source_expr_bytes = std.mem.nativeToLittle(u32, @intFromEnum(source_expr_id));
    hasher.update(std.mem.asBytes(&source_expr_bytes));
    return .{ .bytes = hasher.finalResult() };
}

fn parserEncodingCaptureId() u32 {
    return 0;
}

fn encodeToValueCaptureId() u32 {
    return 0;
}

fn encodeToEncodingCaptureId() u32 {
    return 1;
}

fn encodeToFirstFieldCaptureId() usize {
    return 2;
}

fn generatedFieldNamesIterStepKey(
    current_fn_key: names.TypeDigest,
    source_expr_id: checked.CheckedExprId,
    index: usize,
    mode: FieldNamesIterMode,
) names.TypeDigest {
    var hasher = std.crypto.hash.sha2.Sha256.init(.{});
    hasher.update("roc.generated_fields_iter_step");
    hasher.update(&current_fn_key.bytes);
    var source_expr_bytes = std.mem.nativeToLittle(u32, @intFromEnum(source_expr_id));
    hasher.update(std.mem.asBytes(&source_expr_bytes));
    var index_bytes = std.mem.nativeToLittle(u64, @intCast(index));
    hasher.update(std.mem.asBytes(&index_bytes));
    const mode_byte: u8 = switch (mode) {
        .all => 0,
        .for_size => 1,
    };
    hasher.update(&[_]u8{mode_byte});
    return .{ .bytes = hasher.finalResult() };
}

fn checkedPayload(view: ModuleView, checked_ty: checked.CheckedTypeId) checked.CheckedTypePayload {
    const raw = @intFromEnum(checked_ty);
    if (raw >= view.types.payloadCount()) Common.invariant("checked type id outside checked type store");
    return view.types.payload(checked_ty);
}

fn schemeRoot(view: ModuleView, source_scheme: anytype, comptime missing_message: []const u8) checked.CheckedTypeId {
    const scheme = view.types.schemeForKey(source_scheme) orelse Common.invariant(missing_message);
    return scheme.root;
}

fn checkedLambdaExprIdForConstFn(view: ModuleView, fn_def: anytype) checked.CheckedExprId {
    return switch (fn_def) {
        .nested => |nested| blk: {
            const site = checkedNestedSite(view, nested);
            const expr_id = site.checked_expr orelse Common.invariant("stored nested function had no checked expression site");
            const expr = view.bodies.expr(expr_id);
            break :blk switch (expr.data) {
                .lambda => expr_id,
                .closure => |closure| closure.lambda,
                else => Common.invariant("stored nested function site did not point at a lambda or closure"),
            };
        },
        else => Common.invariant("capturing stored function must reference a checked nested function"),
    };
}

fn ownerTemplateForConstFnDef(fn_def: anytype) names.ProcTemplate {
    return switch (fn_def) {
        .nested => |nested| nested.owner,
        else => Common.invariant("capturing stored function must have a nested owner template"),
    };
}

fn checkedNestedSite(view: ModuleView, nested: anytype) checked.NestedProcSite {
    for (view.nested_proc_sites.sites) |site| {
        if (site.site != nested.site) continue;
        if (!names.procedureTemplateRefEql(site.owner_template, nested.owner)) continue;
        return site;
    }
    Common.invariant("stored nested function referenced a missing checked nested site");
}

fn checkedBinderType(view: ModuleView, binder: checked.PatternBinderId) checked.CheckedTypeId {
    const raw = @intFromEnum(binder);
    if (raw >= view.bodies.patternBinderCount()) Common.invariant("stored function capture binder is outside checked body store");
    const pattern = view.bodies.patternBinder(@enumFromInt(raw)).pattern;
    const pattern_raw = @intFromEnum(pattern);
    if (pattern_raw >= view.bodies.patternCount()) Common.invariant("stored function capture pattern is outside checked body store");
    return view.bodies.pattern(@enumFromInt(pattern_raw)).ty;
}

fn constCaptureBinder(id: check.ConstStore.CaptureId) checked.PatternBinderId {
    return switch (id) {
        .binder => |binder| binder,
        .generated => Common.invariant("generated capture reached source lambda restore"),
    };
}

fn constGeneratedCaptureNode(fn_value: check.ConstStore.ConstFn, capture_id: u32) ?checked.ConstNodeId {
    for (fn_value.captures) |capture| {
        switch (capture.id) {
            .generated => |actual| if (actual == capture_id) return capture.value,
            .binder => {},
        }
    }
    return null;
}

fn constStrNodeByteLen(view: ModuleView, node: checked.ConstNodeId) u32 {
    return switch (view.const_store.get(node)) {
        .str => |str| str.len,
        else => Common.invariant("stored parser renamed field capture was not a Str constant"),
    };
}

fn constStrNodeBytes(view: ModuleView, node: checked.ConstNodeId) []const u8 {
    return switch (view.const_store.get(node)) {
        .str => |str| view.const_store.strBytes(str),
        else => Common.invariant("stored parser renamed field capture was not a Str constant"),
    };
}

fn dispatchPlanForRuntimeExpr(view: ModuleView, expr_id: checked.CheckedExprId) static_dispatch.StaticDispatchCallPlan {
    const expr = view.bodies.expr(expr_id);
    const plan_id = switch (expr.data) {
        .dispatch_call => |maybe| maybe orelse Common.invariant("stored serialization dispatch expression had no dispatch plan"),
        .type_dispatch_call => |maybe| maybe orelse Common.invariant("stored serialization type dispatch expression had no dispatch plan"),
        else => Common.invariant("stored serialization runtime function did not reference a dispatch expression"),
    };
    const plan_raw = @intFromEnum(plan_id);
    if (plan_raw >= view.static_dispatch_plans.plans.len) Common.invariant("stored serialization dispatch plan is outside plan table");
    return view.static_dispatch_plans.plans[plan_raw];
}

fn moduleIdFromDigest(ref: names.CheckedModuleDigest) checked.ModuleId {
    return .{ .bytes = ref.bytes };
}

fn moduleDigestFromId(key: checked.ModuleId) names.CheckedModuleDigest {
    return .{ .bytes = key.bytes };
}

fn hostedTemplate(hosted: anytype) names.ProcTemplate {
    const Hosted = @TypeOf(hosted);
    if (Hosted == names.ProcTemplate) return hosted;
    if (Hosted == Ast.HostedFn) return hosted.template;
    @compileError("unsupported hosted function payload");
}

fn methodOwnerFromType(types: *const Type.Store, ty: Type.TypeId) ?static_dispatch.MethodOwner {
    return switch (types.ownerHead(ty)) {
        .none => null,
        .builtin => |owner| .{ .builtin = owner },
        .named_type => |def| if (def.source_decl) |source_decl|
            .{ .source_decl = .{
                .module_name = def.module_name,
                .statement = source_decl,
            } }
        else
            .{ .nominal = .{
                .module_name = def.module_name,
                .type_name = def.type_name,
            } },
    };
}

fn builtinOwner(builtin: ?checked.CheckedBuiltinNominal) ?static_dispatch.BuiltinOwner {
    return switch (builtin orelse return null) {
        .bool => .bool,
        .str => .str,
        .u8 => .u8,
        .i8 => .i8,
        .u16 => .u16,
        .i16 => .i16,
        .u32 => .u32,
        .i32 => .i32,
        .u64 => .u64,
        .i64 => .i64,
        .u128 => .u128,
        .i128 => .i128,
        .f32 => .f32,
        .f64 => .f64,
        .dec => .dec,
        .list => .list,
        .box => .box,
        .parse_tag_union_spec => .parse_tag_union_spec,
        .fields => .fields,
        .field => .field,
    };
}

fn primitiveInspectLowLevelOp(primitive: Type.Primitive) can.CIR.Expr.LowLevel {
    return switch (primitive) {
        .str => .str_inspect,
        .u8 => .u8_to_str,
        .i8 => .i8_to_str,
        .u16 => .u16_to_str,
        .i16 => .i16_to_str,
        .u32 => .u32_to_str,
        .i32 => .i32_to_str,
        .u64 => .u64_to_str,
        .i64 => .i64_to_str,
        .u128 => .u128_to_str,
        .i128 => .i128_to_str,
        .f32 => .f32_to_str,
        .f64 => .f64_to_str,
        .dec => .dec_to_str,
        .bool => Common.invariant("Bool must lower as an ordinary tag union before Str.inspect"),
    };
}

const ResolvedPayload = struct {
    root: checked.CheckedTypeId,
    payload: checked.CheckedTypePayload,
};

fn resolvedPayload(view: ModuleView, ty: checked.CheckedTypeId) ResolvedPayload {
    var current = ty;
    var remaining = view.types.payloadCount();
    while (remaining > 0) : (remaining -= 1) {
        const payload = checkedPayload(view, current);
        switch (payload) {
            .alias => |alias| current = alias.backing,
            else => return .{ .root = current, .payload = payload },
        }
    }
    Common.invariant("checked type alias chain was cyclic");
}

fn checkedRecordFieldByName(
    view: ModuleView,
    checked_ty: checked.CheckedTypeId,
    field_name: []const u8,
) checked.CheckedRecordField {
    var current = checked_ty;
    var remaining = view.types.payloadCount();
    while (remaining > 0) : (remaining -= 1) {
        switch (checkedPayload(view, current)) {
            .alias => |alias| current = alias.backing,
            .nominal => |nominal| current = nominal.backing,
            .empty_record => break,
            .record_unbound => |fields| {
                for (fields) |field| {
                    if (Ident.textEql(view.names.recordFieldLabelText(field.name), field_name)) return field;
                }
                break;
            },
            .record => |record| {
                for (record.fields) |field| {
                    if (Ident.textEql(view.names.recordFieldLabelText(field.name), field_name)) return field;
                }
                current = record.ext;
            },
            else => Common.invariant("expected checked record field lookup reached a non-record checked type"),
        }
    }
    Common.invariant("expected checked record field was absent");
}

fn intValueToDec(value: can.CIR.IntValue) builtins.dec.RocDec {
    const whole = switch (value.kind) {
        .i128 => @as(i128, @bitCast(value.bytes)),
        .u128 => blk: {
            const unsigned = @as(u128, @bitCast(value.bytes));
            if (unsigned > @as(u128, @intCast(std.math.maxInt(i128)))) {
                Common.invariant("integer pattern solved as Dec exceeded Dec whole-number range");
            }
            break :blk @as(i128, @intCast(unsigned));
        },
    };
    return builtins.dec.RocDec.fromWholeInt(whole) orelse {
        Common.invariant("integer pattern solved as Dec exceeded Dec range");
    };
}

fn intValueToF64(value: can.CIR.IntValue) f64 {
    return switch (value.kind) {
        .i128 => builtins.compiler_rt_128.i128_to_f64(@as(i128, @bitCast(value.bytes))),
        .u128 => builtins.compiler_rt_128.u128_to_f64(@as(u128, @bitCast(value.bytes))),
    };
}

fn branchCount(branches: anytype) usize {
    var count: usize = 0;
    for (branches) |branch| count += branch.pt_len;
    return count;
}

fn moduleBytesEqual(a: [32]u8, b: [32]u8) bool {
    return std.mem.eql(u8, a[0..], b[0..]);
}

fn sameTypeDef(left: Type.TypeDef, right: Type.TypeDef) bool {
    return left.module_name == right.module_name and
        left.type_name == right.type_name and
        left.source_decl == right.source_decl;
}

const CheckedTypeAddress = struct {
    module_bytes: [32]u8,
    type_id: u32,
};

fn checkedTypeAddress(view: ModuleView, checked_ty: checked.CheckedTypeId) CheckedTypeAddress {
    return .{
        .module_bytes = view.key.bytes,
        .type_id = @intFromEnum(checked_ty),
    };
}

const TemplateFamily = struct {
    module_bytes: [32]u8,
    proc_base: u32,
    template: u32,
    source_fn_key_bytes: [32]u8,

    fn from(template: names.ProcTemplate, source_fn_key: names.TypeDigest) TemplateFamily {
        return .{
            .module_bytes = names.procTemplateModuleDigest(template).bytes,
            .proc_base = @intFromEnum(template.proc_base),
            .template = @intFromEnum(template.template),
            .source_fn_key_bytes = source_fn_key.bytes,
        };
    }
};

const NestedSiteAddress = struct {
    module_bytes: [32]u8,
    owner_proc_base: u32,
    owner_template: u32,
    expr: u32,

    fn from(
        module_key: checked.ModuleId,
        owner: names.ProcTemplate,
        expr_id: checked.CheckedExprId,
    ) NestedSiteAddress {
        return .{
            .module_bytes = module_key.bytes,
            .owner_proc_base = @intFromEnum(owner.proc_base),
            .owner_template = @intFromEnum(owner.template),
            .expr = @intFromEnum(expr_id),
        };
    }
};

const NestedFnFamily = struct {
    module_bytes: [32]u8,
    owner_proc_base: u32,
    owner_template: u32,
    site: u32,
    context_fn_key_bytes: [32]u8,
    source_fn_key_bytes: [32]u8,

    fn from(nested: Ast.NestedFn, source_fn_key: names.TypeDigest) NestedFnFamily {
        return .{
            .module_bytes = names.procTemplateModuleDigest(nested.owner).bytes,
            .owner_proc_base = @intFromEnum(nested.owner.proc_base),
            .owner_template = @intFromEnum(nested.owner.template),
            .site = @intFromEnum(nested.site),
            .context_fn_key_bytes = nested.context_fn_key.bytes,
            .source_fn_key_bytes = source_fn_key.bytes,
        };
    }
};

test "monotype lower declarations are referenced" {
    std.testing.refAllDecls(@This());
}

test "record parser presence words cover fields wider than one u64" {
    try std.testing.expectEqual(@as(usize, 0), BodyContext.recordPresenceWordCount(0));
    try std.testing.expectEqual(@as(usize, 1), BodyContext.recordPresenceWordCount(1));
    try std.testing.expectEqual(@as(usize, 1), BodyContext.recordPresenceWordCount(64));
    try std.testing.expectEqual(@as(usize, 2), BodyContext.recordPresenceWordCount(65));
    try std.testing.expectEqual(@as(usize, 2), BodyContext.recordPresenceWordCount(128));
    try std.testing.expectEqual(@as(usize, 3), BodyContext.recordPresenceWordCount(129));

    try std.testing.expectEqual(@as(usize, 0), BodyContext.recordPresenceWordIndex(0));
    try std.testing.expectEqual(@as(usize, 0), BodyContext.recordPresenceWordIndex(63));
    try std.testing.expectEqual(@as(usize, 1), BodyContext.recordPresenceWordIndex(64));
    try std.testing.expectEqual(@as(usize, 1), BodyContext.recordPresenceWordIndex(127));
    try std.testing.expectEqual(@as(usize, 2), BodyContext.recordPresenceWordIndex(128));

    try std.testing.expectEqual(@as(u64, 1), BodyContext.recordPresenceMask(0));
    try std.testing.expectEqual(@as(u64, 1) << 63, BodyContext.recordPresenceMask(63));
    try std.testing.expectEqual(@as(u64, 1), BodyContext.recordPresenceMask(64));
    try std.testing.expectEqual(@as(u64, 2), BodyContext.recordPresenceMask(65));
    try std.testing.expectEqual(@as(u64, 1) << 63, BodyContext.recordPresenceMask(127));
    try std.testing.expectEqual(@as(u64, 1), BodyContext.recordPresenceMask(128));
}
