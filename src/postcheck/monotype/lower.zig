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

/// A procedure template specialization together with the Monotype function
/// type its body is reserved or lowered at.
const LoweredTemplate = struct {
    def: Ast.DefId,
    fn_ty: Type.TypeId,
    status: LoweredTemplateStatus,
};

/// A lowered nested function specialization together with the Monotype
/// function type its body was lowered at.
const LoweredNestedFn = struct {
    nested_id: Ast.NestedDefId,
    fn_id: Ast.FnId,
    fn_ty: Type.TypeId,
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

const ConstExprAddress = struct {
    store_module_bytes: [32]u8,
    type_module_bytes: [32]u8,
    node: u32,
    mono_ty: u32,
};

const InspectDefAddress = struct {
    value_ty: u32,
    str_ty: u32,
};

const EqualityDefAddress = struct {
    value_ty: u32,
    bool_ty: u32,
};

const InspectDefEntry = union(enum) {
    reserved: Ast.DefId,
    ready: Ast.DefId,

    fn id(self: InspectDefEntry) Ast.DefId {
        return switch (self) {
            .reserved => |def_id| def_id,
            .ready => |def_id| def_id,
        };
    }
};

const EqualityDefEntry = union(enum) {
    reserved: Ast.DefId,
    ready: Ast.DefId,

    fn id(self: EqualityDefEntry) Ast.DefId {
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
    inspect_defs: std.AutoHashMap(InspectDefAddress, InspectDefEntry),
    equality_defs: std.AutoHashMap(EqualityDefAddress, EqualityDefEntry),
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
            .inspect_defs = std.AutoHashMap(InspectDefAddress, InspectDefEntry).init(allocator),
            .equality_defs = std.AutoHashMap(EqualityDefAddress, EqualityDefEntry).init(allocator),
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
                .order = proc.order_key,
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

    /// Specializations of one template family are deduplicated by the CURRENT
    /// structural digest of their function types: both sides of the comparison
    /// are live, so a specialization whose function type was refined after it
    /// was registered still deduplicates its own recursive requests. A hit from
    /// a requesting specialization unifies the requested function type with the
    /// cached one, so a requester that later diverges from a reused body is a
    /// unification conflict rather than a silent mismatch.
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
            if (existing.fn_ty != fn_ty) {
                const existing_digest = self.program.types.typeDigest(&self.program.names, existing.fn_ty);
                if (!std.mem.eql(u8, existing_digest.bytes[0..], fn_ty_digest.bytes[0..])) continue;
                if (requester) |graph| {
                    try graph.unify(try graph.importMono(fn_ty), try graph.importMono(existing.fn_ty));
                    try graph.drainDirty();
                }
            }
            switch (existing.status) {
                .ready,
                .lowering,
                => return existing.def,
                .reserved => {
                    reserved_def = existing.def;
                    lower_fn_ty = existing.fn_ty;
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
                .fn_ty = lower_fn_ty,
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
        if (moduleBytesEqual(source_ty_view.key.bytes, view.key.bytes)) {
            try body_ctx.constrainKnownType(source_fn_ty, lower_fn_ty);
        }
        try body_ctx.constrainTypeToMono(template.checked_fn_root, lower_fn_ty);

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
            if (existing.fn_ty != fn_ty) {
                const existing_digest = self.program.types.typeDigest(&self.program.names, existing.fn_ty);
                if (!std.mem.eql(u8, existing_digest.bytes[0..], fn_ty_digest.bytes[0..])) continue;
                try requester.unify(try requester.importMono(fn_ty), try requester.importMono(existing.fn_ty));
                try requester.drainDirty();
            }
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
            .fn_ty = fn_ty,
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
            entry.fn_ty = fn_ty;
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
        if (raw >= view.types.payloads.len) Common.invariant("checked type id outside checked type store");

        const reserved = try self.program.types.add(.zst);
        try self.type_cache.put(address, reserved);
        try self.unsolved_monos.put(reserved, {});
        const lowered = try self.lowerTypePayload(view, checked_ty, view.types.payloads[raw]);
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
            if (source.declaration.formal_args.len != mono_args.len) {
                Common.invariant("checked nominal declaration arity differed from nominal type use");
            }
            for (source.declaration.formal_args, mono_args) |formal, mono_arg| {
                try ctx.constrainTypeToMono(ctx.checkedTypeInCurrentView(source.view, formal), mono_arg);
            }
        }
        return try ctx.lowerType(ctx.nominalBackingRoot(nominal));
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
            const payloads = try self.lowerTypeSlice(view, tag.args);
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
                const binding_source = schemeRoot(app_view, binding.source_scheme, "platform required procedure binding source scheme was not output");
                break :blk self.fnDefForProcedureBindingBody(app_view, binding.body, binding_source, app_view.types.rootKey(binding_source), mono_fn_ty);
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
            return existing.fn_id;
        }

        const nested_id: Ast.NestedDefId = @enumFromInt(@as(u32, @intCast(self.program.nested_defs.items.len)));
        const fn_id = try self.program.addFn(fn_template);
        try self.program.nested_defs.append(self.allocator, undefined);
        try family_entry.value_ptr.append(self.allocator, .{
            .nested_id = nested_id,
            .fn_id = fn_id,
            .fn_ty = fn_template.mono_fn_ty,
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
        self.program.nested_defs.items[@intFromEnum(nested_id)] = .{
            .symbol = self.symbols.fresh(),
            .fn_def = def_template,
            .fn_id = fn_id,
            .args = lowered.args,
            .body = lowered.body,
            .ret = lowered.ret,
        };
        if (live_fn_ty != fn_template.mono_fn_ty) {
            const entries = self.lowered_nested_fns.getPtr(family) orelse
                Common.invariant("lowered nested function family disappeared before completion");
            for (entries.items) |*entry| {
                if (entry.nested_id != nested_id) continue;
                entry.fn_ty = live_fn_ty;
                break;
            }
        }
        return fn_id;
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
        const fn_value = store_view.const_store.fns.items[raw];
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

        const lambda_expr_id = checkedLambdaExprIdForConstFn(fn_view, fn_value.fn_def);
        const lambda_expr = fn_view.bodies.exprs[@intFromEnum(lambda_expr_id)];

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
            const capture_ty = checkedBinderType(fn_view, capture.binder);
            const lowered_ty = try self.lowerType(fn_view, capture_ty);
            const value_expr = try fn_ctx.restoreConstNode(store_view, fn_view, capture.value, capture_ty);
            const local = try self.program.addLocalWithBinder(self.symbols.fresh(), lowered_ty, capture.binder);
            try bindLocalName(self.program, fn_view, local, capture.binder);
            const pat = try self.program.addPat(.{ .ty = lowered_ty, .data = .{ .bind = local } });
            const previous = fn_ctx.binders.get(capture.binder);
            try fn_ctx.binders.put(capture.binder, local);
            captures[index] = .{
                .binder = capture.binder,
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
        const address = InspectDefAddress{
            .value_ty = @intFromEnum(value_ty),
            .str_ty = @intFromEnum(str_ty),
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

    const PatternLiteralGuard = struct {
        local: Ast.LocalId,
        conversion: checked.CheckedExprId,
        ty: Type.TypeId,
    };

    const MaterializedArg = struct {
        pattern: checked.CheckedPatternId,
        value: Ast.ExprId,
        ty: Type.TypeId,
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
        const string_literals = try allocator.alloc(?Ast.StringLiteralId, view.bodies.string_literals.len);
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
        };
    }

    fn deinit(self: *BodyContext) void {
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
                .payloads = try self.instNodeSlice(tag.args),
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
        if (source.declaration.formal_args.len != args.len) {
            Common.invariant("checked nominal declaration arity differed from nominal type use");
        }
        var scope = std.AutoHashMap(CheckedTypeAddress, NodeId).init(self.allocator);
        defer scope.deinit();
        for (source.declaration.formal_args, args) |formal, arg| {
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
                const body = self.view.bodies.bodies[@intFromEnum(body_id)];
                const root = self.view.bodies.exprs[@intFromEnum(body.root_expr)];
                return switch (root.data) {
                    .lambda => |lambda| try self.lowerLambdaTemplate(lambda, fn_ty),
                    .closure => |closure| blk: {
                        if (closure.captures.len != 0) {
                            Common.invariant("checked procedure template root closure had captures");
                        }
                        const lambda_expr = self.view.bodies.exprs[@intFromEnum(closure.lambda)];
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
                switch (root.kind) {
                    .numeral_conversion, .quote_conversion => return .{
                        .args = .empty(),
                        .body = try self.lowerNumeralRootBody(wrapper.body_expr, ret_ty),
                        .ret = ret_ty,
                    },
                    else => {},
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
        const body_expr = self.view.bodies.exprs[@intFromEnum(checked_body)];
        self.builder.program.current_loc = try self.sourceLocFor(body_expr.source_region);

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
        const saved_body_loc = self.builder.program.current_loc;
        defer self.builder.program.current_loc = saved_body_loc;
        self.builder.program.current_loc = body_loc;
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
        const expr = self.view.bodies.exprs[@intFromEnum(expr_id)];
        return switch (expr.data) {
            .lambda => |lambda| try self.lowerNestedLambdaTemplate(lambda, fn_ty),
            .closure => |closure| blk: {
                const lambda_expr = self.view.bodies.exprs[@intFromEnum(closure.lambda)];
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
        const expr = self.view.bodies.exprs[@intFromEnum(expr_id)];
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

    fn lowerExpr(self: *BodyContext, expr_id: checked.CheckedExprId) Allocator.Error!Ast.ExprId {
        const expr = self.view.bodies.exprs[@intFromEnum(expr_id)];
        const saved_loc = self.builder.program.current_loc;
        defer self.builder.program.current_loc = saved_loc;
        self.builder.program.current_loc = try self.sourceLocFor(expr.source_region);
        switch (expr.data) {
            .call => |call| return try self.lowerCallExpr(expr.ty, call),
            .dispatch_call => |plan| return try self.lowerDispatchExpr(expr.ty, plan),
            .interpolation => |interpolation| return try self.lowerDispatchExpr(expr.ty, interpolation.plan),
            .type_dispatch_call => |plan| return try self.lowerDispatchExpr(expr.ty, plan),
            .method_eq => |plan| return try self.lowerDispatchExpr(expr.ty, plan),
            .structural_eq => |eq| return try self.lowerDirectStructuralEq(expr.ty, eq),
            else => {},
        }
        const ty = try self.lowerExprType(expr_id);
        return try self.lowerExprWithType(expr_id, ty);
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

    fn resetComptimeExhaustivenessDepth(self: *BodyContext) u32 {
        const saved = self.comptime_exhaustiveness_depth;
        self.comptime_exhaustiveness_depth = 0;
        return saved;
    }

    fn restoreComptimeExhaustivenessDepth(self: *BodyContext, saved: u32) void {
        self.comptime_exhaustiveness_depth = saved;
    }

    fn inComptimeExhaustivenessContext(self: *const BodyContext) bool {
        return self.comptime_exhaustiveness_depth != 0;
    }

    fn lowerExprWithType(
        self: *BodyContext,
        expr_id: checked.CheckedExprId,
        ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        const expr = self.view.bodies.exprs[@intFromEnum(expr_id)];
        const saved_loc = self.builder.program.current_loc;
        defer self.builder.program.current_loc = saved_loc;
        self.builder.program.current_loc = try self.sourceLocFor(expr.source_region);
        const data: Ast.ExprData = switch (expr.data) {
            .pending,
            .anno_only,
            .runtime_error,
            => Common.invariant("non-runtime checked expression reached Monotype lowering"),
            .num => |num| self.lowerIntLiteral(num.value, ty),
            .typed_int => |num| self.lowerIntLiteral(num.value, ty),
            .frac_f32 => |frac| .{ .frac_f32_lit = frac.value },
            .frac_f64 => |frac| .{ .frac_f64_lit = frac.value },
            .dec => |dec| .{ .dec_lit = dec.value },
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
        if (snippet_index >= self.view.bodies.string_literals.len) {
            Common.invariant("checked string literal id outside checked body string store");
        }
        const snippet_text = self.view.bodies.string_literals[snippet_index];
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
        if (index >= self.view.bodies.string_literals.len) {
            Common.invariant("checked string literal id outside checked body string store");
        }
        if (self.string_literals[index]) |existing| return existing;
        const lowered = try self.builder.program.addStringLiteral(self.view.bodies.string_literals[index]);
        self.string_literals[index] = lowered;
        return lowered;
    }

    fn lowerCallExpr(self: *BodyContext, checked_ret_ty: checked.CheckedTypeId, call: anytype) Allocator.Error!Ast.ExprId {
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
        const expr = self.view.bodies.exprs[@intFromEnum(checked_func)];
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
        const expr = self.view.bodies.exprs[@intFromEnum(checked_func)];
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
        for (function.args, checked_args) |formal_ty, checked_arg| {
            const arg_ty = caller.view.bodies.exprs[@intFromEnum(checked_arg)].ty;
            const formal_node = try self.instNode(formal_ty);
            try self.graph.unify(formal_node, try caller.instNode(arg_ty));
            if (try caller.callArgumentMonoType(checked_arg, null)) |evidence_ty| {
                try self.graph.unify(formal_node, try self.graph.importMono(evidence_ty));
            }
        }
        try self.graph.unify(try self.instNode(function.ret), try caller.instNode(checked_ret_ty));
        if (expected_ret_ty) |expected| {
            try self.graph.unify(try self.instNode(function.ret), try self.graph.importMono(expected));
        }
        try self.graph.drainDirty();
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
                const arg_ty = caller.view.bodies.exprs[@intFromEnum(checked_arg)].ty;
                const formal_node = try self.instNode(formal_ty);
                try self.graph.unify(formal_node, try caller.instNode(arg_ty));
                if (try caller.callArgumentMonoType(checked_arg, null)) |evidence_ty| {
                    try self.graph.unify(formal_node, try self.graph.importMono(evidence_ty));
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

    fn callArgumentMonoType(
        self: *BodyContext,
        checked_arg: checked.CheckedExprId,
        expected_ty: ?Type.TypeId,
    ) Allocator.Error!?Type.TypeId {
        const expr = self.view.bodies.exprs[@intFromEnum(checked_arg)];
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
        if (self.currentLocalForResolvedValue(ref_id)) |local_id| {
            const local_ty = self.builder.program.locals.items[@intFromEnum(local_id)].ty;
            try self.constrainTypeToMono(checked_ty, local_ty);
            return local_ty;
        }
        const record = self.view.resolved_refs.records[@intFromEnum(ref_id)];
        return switch (record.ref) {
            .top_level_const => |const_use| try self.constUseMonoType(const_use),
            .imported_const => |const_use| try self.constUseMonoType(const_use),
            .platform_required_const => |required| try self.constUseMonoType(required.const_use),
            else => try self.lowerType(checked_ty),
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
                const binding_source = schemeRoot(app_view, binding.source_scheme, "platform required procedure binding source scheme was not output");
                break :blk self.builder.fnDefForProcedureBindingBody(app_view, binding.body, binding_source, app_view.types.rootKey(binding_source), mono_fn_ty);
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
            else => null,
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
        if (self.currentLocalBindingForResolvedValue(ref_id)) |binding| {
            const local_id = binding.local;
            const local_ty = self.builder.program.locals.items[@intFromEnum(local_id)].ty;
            const binder_ty = checkedBinderType(self.view, binding.binder);
            // The local's Monotype identifies the binder's node in the
            // context that bound it; importing it unifies this context's
            // instantiation with that node before the use type is unified.
            try self.constrainTypeToMono(binder_ty, local_ty);
            try self.constrainTypeToMono(binder_ty, ty);
            try self.constrainTypeToMono(checked_ty, ty);
            const live_ty = try self.lowerType(binder_ty);
            if (live_ty != local_ty) self.builder.program.setLocalType(local_id, live_ty);
            if (!self.sameType(ty, live_ty)) {
                Common.invariant("checked local lookup type differed from its expected Monotype use type");
            }
            return try self.builder.program.addExpr(.{ .ty = live_ty, .data = .{ .local = local_id } });
        }

        switch (record.ref) {
            .top_level_const => |const_use| return try self.restoreConstUseAtType(const_use, ty),
            .imported_const => |const_use| return try self.restoreConstUseAtType(const_use, ty),
            .platform_required_const => |required| return try self.restoreConstUseAtType(required.const_use, ty),
            else => {},
        }

        try self.constrainTypeToMono(checked_ty, ty);
        const data: Ast.ExprData = switch (record.ref) {
            .local_param,
            .local_value,
            .local_mutable_version,
            .pattern_binder,
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
            .eval_template => |eval| try self.lowerConstEvalTemplateUse(store_view, eval, ty),
        };
    }

    fn lowerConstEvalTemplateUse(
        self: *BodyContext,
        store_view: ModuleView,
        eval: checked.ConstEvalTemplate,
        ty: Type.TypeId,
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
        const expr = self.view.bodies.exprs[@intFromEnum(expr_id)];
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
        for (self.builder.program.types.fieldSpan(self.builder.recordFieldsSpan(ty))) |field| {
            if (Ident.textEql(self.builder.program.names.recordFieldLabelText(field.name), text)) return field;
        }
        Common.invariant("expected record field was absent from monotype type");
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
        const expr = self.view.bodies.exprs[@intFromEnum(checked_expr)];
        const saved_loc = self.builder.program.current_loc;
        defer self.builder.program.current_loc = saved_loc;
        self.builder.program.current_loc = try self.sourceLocFor(expr.source_region);
        switch (expr.data) {
            .call => |call| {
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
                .record => |actual_fields| self.sameRecordFields(fields, actual_fields, depth),
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

    fn sameRecordFields(self: *BodyContext, expected: Type.Span, actual: Type.Span, depth: u8) bool {
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
        const lambda = self.view.bodies.exprs[@intFromEnum(closure.lambda)];
        return switch (lambda.data) {
            .lambda => blk: {
                const nested = try self.builder.fnTemplateForNestedExprWithMono(self.view, self.owner_template, expr_id, lambda.ty, self.view.types.rootKey(lambda.ty), closure_ty, self.current_fn_key);
                break :blk try self.lowerLambdaExpr(expr_id, nested);
            },
            else => Common.invariant("checked closure did not point at a lambda expression"),
        };
    }

    fn closureFunctionType(self: *BodyContext, closure: anytype) Allocator.Error!Type.TypeId {
        const lambda = self.view.bodies.exprs[@intFromEnum(closure.lambda)];
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
            args[i] = try self.lowerType(self.view.bodies.patterns[@intFromEnum(pattern_id)].ty);
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

        var call_ctx = try BodyContext.init(self.allocator, self.builder, self.view, self.owner_template, self.graph);
        defer call_ctx.deinit();
        call_ctx.owner_context_fn_key = self.owner_context_fn_key;
        call_ctx.current_fn_key = self.current_fn_key;

        const callable_mono_ty = try call_ctx.instantiateDispatchPlanCallTypeFromCaller(plan.callable_ty, self, checked_ret_ty, plan.args, expected_ret_ty);
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
                        .expr = try self.lowerDispatchOperandAtType(plan.args[index], plan_arg_tys[index]),
                    };
                },
                .type_only => {},
            }
        }
        const lookup = self.dispatchTarget(plan, dispatcher_ty);
        if (lookup == null) {
            return try self.lowerStructuralEquality(plan, callable_mono_ty, plan_ret_ty, self, pre_lowered);
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
            .equality => |eq| if (eq.negated) blk: {
                if (!self.typeHasBuiltinOwner(expr_ty, .bool)) Common.invariant("checked equality dispatch returned a non-Bool value");
                break :blk try self.builder.lowLevelExpr(.bool_not, &.{expr}, expr_ty);
            } else expr,
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

        var call_ctx = try BodyContext.init(self.allocator, self.builder, self.view, self.owner_template, self.graph);
        defer call_ctx.deinit();
        call_ctx.owner_context_fn_key = self.owner_context_fn_key;
        call_ctx.current_fn_key = self.current_fn_key;

        const callable_mono_ty = try call_ctx.instantiateNumeralPlanCallType(plan.callable_ty, self, checked_ret_ty, target_ty, plan.args);
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
        const expr = self.view.bodies.exprs[@intFromEnum(expr_id)];
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
        if (plan.args.len != 1) Common.invariant("from_numeral plan did not carry exactly one operand");
        const literal = switch (plan.args[0]) {
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
        return switch (self.builder.shapeContent(ty)) {
            .tag_union => |span| {
                for (self.builder.program.types.tagSpan(span)) |tag| {
                    if (Ident.textEql(self.builder.program.names.tagLabelText(tag.name), text)) return tag;
                }
                Common.invariant("expected tag was absent from monotype tag union");
            },
            else => Common.invariant("expected a tag union monotype"),
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

        const callable_mono_ty = try call_ctx.instantiateDispatchPlanCallTypeFromCaller(plan.callable_ty, self, checked_ret_ty, plan.args, expected_ret_ty);
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
        const owner = methodOwnerFromType(&self.builder.program.types, dispatcher_ty) orelse {
            if (plan.result_mode == .equality and plan.result_mode.equality.structural_allowed) return null;
            Common.invariant("dispatch plan had no method owner and no structural equality permission");
        };

        const lookup = self.builder.lookupMethodTarget(owner, self.view, plan.method) orelse {
            if (plan.result_mode == .equality and plan.result_mode.equality.structural_allowed) return null;
            Common.invariant("checked method registry is missing resolved dispatch target");
        };
        return lookup;
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
        const args = try arg_ctx.lowerDispatchOperandsAtTypes(plan.args, self.builder.program.types.span(fn_data.args), pre_lowered);
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
                if (plan.args.len != 2) Common.invariant("structural equality dispatch plan must have two operands");
                const fn_data = self.builder.functionShape(callable_mono_ty, "checked structural equality target had a non-function type");
                const arg_tys = try self.allocator.dupe(Type.TypeId, self.builder.program.types.span(fn_data.args));
                defer self.allocator.free(arg_tys);
                if (arg_tys.len != 2) Common.invariant("structural equality callable type must have two operands");
                const lhs = if (pre_lowered != null and pre_lowered.?.index == 0)
                    pre_lowered.?.expr
                else
                    try arg_ctx.lowerDispatchOperandAtType(plan.args[0], arg_tys[0]);
                const rhs = if (pre_lowered != null and pre_lowered.?.index == 1)
                    pre_lowered.?.expr
                else
                    try arg_ctx.lowerDispatchOperandAtType(plan.args[1], arg_tys[1]);
                var result = try self.lowerEqualityExpr(arg_tys[0], lhs, rhs, self.view.names.methodNameText(plan.method), ret_ty);
                if (eq.negated) {
                    result = try self.builder.lowLevelExpr(.bool_not, &.{result}, ret_ty);
                }
                break :blk result;
            } else Common.invariant("structural equality dispatch plan did not permit structural equality"),
            .value => Common.invariant("value dispatch plan reached structural equality lowering"),
        };
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
        const lhs_checked_ty = self.view.bodies.exprs[@intFromEnum(eq.lhs)].ty;
        const rhs_checked_ty = self.view.bodies.exprs[@intFromEnum(eq.rhs)].ty;
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
        const expr = self.view.bodies.exprs[@intFromEnum(expr_id)];
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

    fn lowerEqualityExpr(
        self: *BodyContext,
        ty: Type.TypeId,
        lhs: Ast.ExprId,
        rhs: Ast.ExprId,
        method_name: []const u8,
        bool_ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        const shape = self.builder.program.types.get(ty);
        const expands_structurally = switch (shape) {
            .record, .tuple, .tag_union => true,
            .named => |named| named.backing != null,
            else => false,
        };
        var remove_active_expansion = false;
        defer if (remove_active_expansion) {
            _ = self.equality_expansion_stack.remove(ty);
        };
        if (expands_structurally) {
            if (self.equality_expansion_stack.contains(ty)) {
                return try self.equalityCall(ty, lhs, rhs, method_name, bool_ty);
            }
            try self.equality_expansion_stack.put(ty, {});
            remove_active_expansion = true;
        }

        return switch (shape) {
            .list => try self.lowerOwnedEqualityCall(ty, lhs, rhs, method_name, bool_ty),
            .record => |fields| try self.lowerRecordEqualityExpr(self.builder.program.types.fieldSpan(fields), lhs, rhs, method_name, bool_ty),
            .tuple => |items| try self.lowerTupleEqualityExpr(self.builder.program.types.span(items), lhs, rhs, method_name, bool_ty),
            .tag_union => |tags| try self.lowerTagUnionEqualityExpr(ty, tags, lhs, rhs, method_name, bool_ty),
            .named => |named| if (named.backing) |backing|
                try self.lowerNamedEqualityExpr(ty, backing.ty, lhs, rhs, method_name, bool_ty)
            else
                try self.builder.program.addExpr(.{ .ty = bool_ty, .data = .{ .structural_eq = .{
                    .lhs = lhs,
                    .rhs = rhs,
                    .negated = false,
                } } }),
            .primitive,
            .zst,
            .func,
            .erased,
            .box,
            => try self.builder.program.addExpr(.{ .ty = bool_ty, .data = .{ .structural_eq = .{
                .lhs = lhs,
                .rhs = rhs,
                .negated = false,
            } } }),
        };
    }

    fn equalityCall(
        self: *BodyContext,
        value_ty: Type.TypeId,
        lhs: Ast.ExprId,
        rhs: Ast.ExprId,
        method_name: []const u8,
        bool_ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        if (!std.mem.eql(u8, method_name, "is_eq")) {
            Common.invariant("recursive structural equality helper requested for a non-is_eq method");
        }

        const def_id = try self.equalityDefForType(value_ty, method_name, bool_ty);
        const fn_ty = try self.builder.twoArgFnType(value_ty, bool_ty);
        const callee = try self.builder.program.addExpr(.{
            .ty = fn_ty,
            .data = .{ .def_ref = def_id },
        });
        const args = [_]Ast.ExprId{ lhs, rhs };
        return try self.builder.program.addExpr(.{
            .ty = bool_ty,
            .data = .{ .call_value = .{
                .callee = callee,
                .args = try self.builder.program.addExprSpan(&args),
            } },
        });
    }

    fn equalityDefForType(
        self: *BodyContext,
        value_ty: Type.TypeId,
        method_name: []const u8,
        bool_ty: Type.TypeId,
    ) Allocator.Error!Ast.DefId {
        const address = EqualityDefAddress{
            .value_ty = @intFromEnum(value_ty),
            .bool_ty = @intFromEnum(bool_ty),
        };
        if (self.builder.equality_defs.get(address)) |entry| return entry.id();

        const def_id: Ast.DefId = @enumFromInt(@as(u32, @intCast(self.builder.program.defs.items.len)));
        try self.builder.program.defs.append(self.allocator, undefined);
        try self.builder.equality_defs.put(address, .{ .reserved = def_id });

        const lhs_local = try self.builder.program.addLocal(self.builder.symbols.fresh(), value_ty);
        const rhs_local = try self.builder.program.addLocal(self.builder.symbols.fresh(), value_ty);
        const lhs_expr = try self.builder.localExpr(lhs_local, value_ty);
        const rhs_expr = try self.builder.localExpr(rhs_local, value_ty);

        // The helper body is the structural expansion for this type. Temporarily
        // remove the caller's active mark so the body expands one layer and only
        // recursive edges inside that layer call back to the reserved helper.
        if (!self.equality_expansion_stack.remove(value_ty)) {
            Common.invariant("recursive structural equality helper requested outside an active equality expansion");
        }
        defer self.equality_expansion_stack.putAssumeCapacity(value_ty, {});

        const body = try self.lowerEqualityExpr(value_ty, lhs_expr, rhs_expr, method_name, bool_ty);
        const args = try self.builder.program.addTypedLocalSpan(&.{
            .{ .local = lhs_local, .ty = value_ty },
            .{ .local = rhs_local, .ty = value_ty },
        });
        self.builder.program.defs.items[@intFromEnum(def_id)] = .{
            .symbol = self.builder.symbols.fresh(),
            .fn_def = null,
            .args = args,
            .body = .{ .roc = body },
            .ret = bool_ty,
        };
        try self.builder.equality_defs.put(address, .{ .ready = def_id });
        return def_id;
    }

    /// Decomposes structural equality on a nominal type by unwrapping both operands to
    /// their shared backing representation and comparing those through `lowerEqualityExpr`.
    /// The nominal unwrap is a transparent alias at runtime, and recursing on the backing
    /// dispatches any owned types nested within it (e.g. a list inside the backing tag
    /// union) instead of leaving them for the LIR structural-equality lowering.
    fn lowerNamedEqualityExpr(
        self: *BodyContext,
        named_ty: Type.TypeId,
        backing_ty: Type.TypeId,
        lhs: Ast.ExprId,
        rhs: Ast.ExprId,
        method_name: []const u8,
        bool_ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        const lhs_inner = try self.builder.program.addLocal(self.builder.symbols.fresh(), backing_ty);
        const rhs_inner = try self.builder.program.addLocal(self.builder.symbols.fresh(), backing_ty);

        const compare = try self.lowerEqualityExpr(
            backing_ty,
            try self.builder.localExpr(lhs_inner, backing_ty),
            try self.builder.localExpr(rhs_inner, backing_ty),
            method_name,
            bool_ty,
        );

        const lhs_pat = try self.builder.program.addPat(.{ .ty = named_ty, .data = .{ .nominal = try self.builder.bindPat(lhs_inner, backing_ty) } });
        const rhs_pat = try self.builder.program.addPat(.{ .ty = named_ty, .data = .{ .nominal = try self.builder.bindPat(rhs_inner, backing_ty) } });

        const bind_rhs = try self.builder.program.addExpr(.{ .ty = bool_ty, .data = .{ .let_ = .{
            .bind = rhs_pat,
            .value = rhs,
            .rest = compare,
        } } });
        return try self.builder.program.addExpr(.{ .ty = bool_ty, .data = .{ .let_ = .{
            .bind = lhs_pat,
            .value = lhs,
            .rest = bind_rhs,
        } } });
    }

    /// Decomposes structural equality on a tag union into nested matches so that each
    /// payload is compared through `lowerEqualityExpr`. This dispatches owned payload
    /// types (e.g. List) to their `is_eq` methods, matching how record and tuple fields
    /// are handled. Leaving tag unions as a `structural_eq` node would defer the payload
    /// comparison to the LIR structural-equality lowering, which can only compare types
    /// that are inline-comparable and panics on owned types such as lists.
    fn lowerTagUnionEqualityExpr(
        self: *BodyContext,
        ty: Type.TypeId,
        tags_span: Type.Span,
        lhs: Ast.ExprId,
        rhs: Ast.ExprId,
        method_name: []const u8,
        bool_ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        // Bind both operands to locals so each is evaluated exactly once and the
        // right-hand side can be re-scrutinised inside every left-hand-side branch.
        const lhs_local = try self.builder.program.addLocal(self.builder.symbols.fresh(), ty);
        const rhs_local = try self.builder.program.addLocal(self.builder.symbols.fresh(), ty);

        // Copy the tag list because recursive lowerEqualityExpr may reallocate type spans.
        const tags = try self.allocator.dupe(Type.Tag, self.builder.program.types.tagSpan(tags_span));
        defer self.allocator.free(tags);

        const branches = try self.allocator.alloc(Ast.Branch, tags.len);
        defer self.allocator.free(branches);
        for (tags, 0..) |tag, branch_index| {
            branches[branch_index] = try self.lowerTagEqualityBranch(ty, rhs_local, tag, tags.len == 1, method_name, bool_ty);
        }

        const match_expr = try self.builder.program.addExpr(.{ .ty = bool_ty, .data = .{ .match_ = .{
            .scrutinee = try self.builder.localExpr(lhs_local, ty),
            .branches = try self.builder.program.addBranchSpan(branches),
        } } });

        const bind_rhs = try self.builder.program.addExpr(.{ .ty = bool_ty, .data = .{ .let_ = .{
            .bind = try self.builder.bindPat(rhs_local, ty),
            .value = rhs,
            .rest = match_expr,
        } } });
        return try self.builder.program.addExpr(.{ .ty = bool_ty, .data = .{ .let_ = .{
            .bind = try self.builder.bindPat(lhs_local, ty),
            .value = lhs,
            .rest = bind_rhs,
        } } });
    }

    /// Builds one branch of the outer match in `lowerTagUnionEqualityExpr`: it matches the
    /// left operand against `tag`, binding its payloads, then matches the right operand
    /// against the same tag. When both sides carry `tag`, the payloads are compared
    /// pairwise; any other right-hand variant yields `false`.
    fn lowerTagEqualityBranch(
        self: *BodyContext,
        ty: Type.TypeId,
        rhs_local: Ast.LocalId,
        tag: Type.Tag,
        single_variant: bool,
        method_name: []const u8,
        bool_ty: Type.TypeId,
    ) Allocator.Error!Ast.Branch {
        // Copy payload types because recursive lowerEqualityExpr may reallocate type spans.
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
        var body = try self.boolLiteral(true, bool_ty);
        var i = payloads.len;
        while (i > 0) {
            i -= 1;
            const payload_eq = try self.lowerEqualityExpr(payloads[i], lhs_exprs[i], rhs_exprs[i], method_name, bool_ty);
            body = try self.builder.ifExpr(payload_eq, body, try self.boolLiteral(false, bool_ty), bool_ty);
        }

        const rhs_tag_pat = try self.builder.program.addPat(.{ .ty = ty, .data = .{ .tag = .{
            .name = tag.name,
            .payloads = try self.builder.program.addPatSpan(rhs_pats),
        } } });
        const rhs_scrutinee = try self.builder.localExpr(rhs_local, ty);
        const inner_match = if (single_variant) blk: {
            const inner_branches = [_]Ast.Branch{.{ .pat = rhs_tag_pat, .body = body }};
            break :blk try self.builder.program.addExpr(.{ .ty = bool_ty, .data = .{ .match_ = .{
                .scrutinee = rhs_scrutinee,
                .branches = try self.builder.program.addBranchSpan(&inner_branches),
            } } });
        } else blk: {
            const wildcard = try self.builder.program.addPat(.{ .ty = ty, .data = .wildcard });
            const inner_branches = [_]Ast.Branch{
                .{ .pat = rhs_tag_pat, .body = body },
                .{ .pat = wildcard, .body = try self.boolLiteral(false, bool_ty) },
            };
            break :blk try self.builder.program.addExpr(.{ .ty = bool_ty, .data = .{ .match_ = .{
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

    fn lowerRecordEqualityExpr(
        self: *BodyContext,
        fields: []const Type.Field,
        lhs: Ast.ExprId,
        rhs: Ast.ExprId,
        method_name: []const u8,
        bool_ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        // Copy because recursive lowerEqualityExpr may reallocate types.fields, invalidating the slice.
        const fields_copy = try self.allocator.dupe(Type.Field, fields);
        defer self.allocator.free(fields_copy);
        var rest = try self.boolLiteral(true, bool_ty);
        var index = fields_copy.len;
        while (index > 0) {
            index -= 1;
            const field = fields_copy[index];
            const lhs_field = try self.builder.program.addExpr(.{ .ty = field.ty, .data = .{ .field_access = .{
                .receiver = lhs,
                .field = field.name,
            } } });
            const rhs_field = try self.builder.program.addExpr(.{ .ty = field.ty, .data = .{ .field_access = .{
                .receiver = rhs,
                .field = field.name,
            } } });
            const field_eq = try self.lowerEqualityExpr(field.ty, lhs_field, rhs_field, method_name, bool_ty);
            rest = try self.builder.ifExpr(field_eq, rest, try self.boolLiteral(false, bool_ty), bool_ty);
        }
        return rest;
    }

    fn lowerTupleEqualityExpr(
        self: *BodyContext,
        items: []const Type.TypeId,
        lhs: Ast.ExprId,
        rhs: Ast.ExprId,
        method_name: []const u8,
        bool_ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        // Copy because recursive lowerEqualityExpr may reallocate types.spans, invalidating the slice.
        const items_copy = try self.allocator.dupe(Type.TypeId, items);
        defer self.allocator.free(items_copy);
        var rest = try self.boolLiteral(true, bool_ty);
        var index = items_copy.len;
        while (index > 0) {
            index -= 1;
            const item_ty = items_copy[index];
            const lhs_item = try self.builder.program.addExpr(.{ .ty = item_ty, .data = .{ .tuple_access = .{
                .tuple = lhs,
                .elem_index = @intCast(index),
            } } });
            const rhs_item = try self.builder.program.addExpr(.{ .ty = item_ty, .data = .{ .tuple_access = .{
                .tuple = rhs,
                .elem_index = @intCast(index),
            } } });
            const item_eq = try self.lowerEqualityExpr(item_ty, lhs_item, rhs_item, method_name, bool_ty);
            rest = try self.builder.ifExpr(item_eq, rest, try self.boolLiteral(false, bool_ty), bool_ty);
        }
        return rest;
    }

    fn lowerOwnedEqualityCall(
        self: *BodyContext,
        ty: Type.TypeId,
        lhs: Ast.ExprId,
        rhs: Ast.ExprId,
        method_name: []const u8,
        bool_ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprId {
        const owner = methodOwnerFromType(&self.builder.program.types, ty) orelse
            Common.invariant("owned equality call requested for a type without a method owner");
        const lookup = self.builder.lookupMethodTargetByName(owner, method_name) orelse
            Common.invariant("checked method registry is missing owned equality target");

        const arg_tys = [_]Type.TypeId{ ty, ty };
        const callable_mono_ty = try self.methodTargetMonoTypeFromArgs(lookup, &arg_tys, bool_ty);
        const args = [_]Ast.ExprId{ lhs, rhs };
        return try self.builder.program.addExpr(.{ .ty = bool_ty, .data = .{ .call_proc = .{
            .callee = .{ .func = try self.methodTargetCalleeWithMono(lookup, callable_mono_ty) },
            .args = try self.builder.program.addExprSpan(&args),
        } } });
    }

    fn boolLiteral(self: *BodyContext, value: bool, bool_ty: Type.TypeId) Allocator.Error!Ast.ExprId {
        const u64_ty = try self.builder.primitiveType(.u64);
        const lhs = try self.builder.intLiteralExpr(0, u64_ty);
        const rhs = try self.builder.intLiteralExpr(if (value) 0 else 1, u64_ty);
        return try self.builder.lowLevelExpr(.num_is_eq, &.{ lhs, rhs }, bool_ty);
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
            .value => |result_ty| try self.lowerExprAtType(body, result_ty),
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
        if (!self.matchContainsListPattern(match)) {
            return try self.builder.program.addExpr(.{
                .ty = output_ty,
                .data = try self.lowerMatch(match, output, comptime_site),
            });
        }

        const scrutinee = try self.lowerExpr(match.cond);
        const scrutinee_ty = self.builder.program.exprs.items[@intFromEnum(scrutinee)].ty;
        const scrutinee_local = try self.builder.program.addLocal(self.builder.symbols.fresh(), scrutinee_ty);
        const scrutinee_expr = try self.builder.localExpr(scrutinee_local, scrutinee_ty);
        const rest = try self.lowerListPatternMatchAlternatives(scrutinee_expr, scrutinee_ty, match.branches, output, comptime_site);
        return try self.builder.program.addExpr(.{ .ty = output_ty, .data = .{ .let_ = .{
            .bind = try self.builder.bindPat(scrutinee_local, scrutinee_ty),
            .value = scrutinee,
            .rest = rest,
        } } });
    }

    fn matchContainsListPattern(self: *BodyContext, match: anytype) bool {
        for (match.branches) |branch| {
            for (branch.patterns) |pattern| {
                if (self.patternContainsList(pattern.pattern)) return true;
            }
        }
        return false;
    }

    fn patternContainsList(self: *BodyContext, pattern_id: checked.CheckedPatternId) bool {
        const pattern = self.view.bodies.patterns[@intFromEnum(pattern_id)];
        return switch (pattern.data) {
            .list => true,
            .as => |as| self.patternContainsList(as.pattern),
            .applied_tag => |tag| blk: {
                for (tag.args) |child| {
                    if (self.patternContainsList(child)) break :blk true;
                }
                break :blk false;
            },
            .nominal => |nominal| self.patternContainsList(nominal.backing_pattern),
            .record_destructure => |destructs| blk: {
                for (destructs) |destruct| {
                    const child = switch (destruct.kind) {
                        .required => |child_pattern| child_pattern,
                        .sub_pattern => |child_pattern| child_pattern,
                        .rest => |child_pattern| child_pattern,
                    };
                    if (self.patternContainsList(child)) break :blk true;
                }
                break :blk false;
            },
            .tuple => |items| blk: {
                for (items) |child| {
                    if (self.patternContainsList(child)) break :blk true;
                }
                break :blk false;
            },
            .assign,
            .pending,
            .num_literal,
            .small_dec_literal,
            .dec_literal,
            .frac_f32_literal,
            .frac_f64_literal,
            .str_literal,
            .underscore,
            .runtime_error,
            => false,
        };
    }

    fn patternNeedsExplicitBinding(self: *BodyContext, pattern_id: checked.CheckedPatternId) bool {
        const pattern = self.view.bodies.patterns[@intFromEnum(pattern_id)];
        return switch (pattern.data) {
            .list => true,
            .record_destructure => |destructs| self.recordDestructsNeedExplicitRest(destructs),
            else => false,
        };
    }

    fn patternCanMiss(self: *BodyContext, pattern_id: checked.CheckedPatternId) bool {
        const pattern = self.view.bodies.patterns[@intFromEnum(pattern_id)];
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

    fn lowerListPatternMatchAlternatives(
        self: *BodyContext,
        scrutinee: Ast.ExprId,
        scrutinee_ty: Type.TypeId,
        branches: []const checked.CheckedMatchBranch,
        output: MatchOutput,
        comptime_site: ?Ast.ComptimeSiteId,
    ) Allocator.Error!Ast.ExprId {
        const output_ty = self.matchOutputType(output);
        var fallback = if (comptime_site) |site|
            try self.comptimeExhaustivenessFailedExpr(output_ty, site)
        else blk: {
            const msg = try self.builder.program.addStringLiteral("non-exhaustive checked match reached Monotype");
            break :blk try self.builder.program.addExpr(.{ .ty = output_ty, .data = .{ .crash = msg } });
        };

        var branch_index = branches.len;
        var alternative_index = branchCount(branches);
        while (branch_index > 0) {
            branch_index -= 1;
            const branch = branches[branch_index];
            var pattern_index = branch.patterns.len;
            while (pattern_index > 0) {
                pattern_index -= 1;
                alternative_index -= 1;
                fallback = try self.lowerListPatternMatchAlternative(
                    scrutinee,
                    scrutinee_ty,
                    branch.patterns[pattern_index],
                    branch.guard,
                    branch.value,
                    fallback,
                    output,
                    comptime_site,
                    alternative_index,
                );
            }
        }

        return fallback;
    }

    fn lowerListPatternMatchAlternative(
        self: *BodyContext,
        scrutinee: Ast.ExprId,
        scrutinee_ty: Type.TypeId,
        pattern: checked.CheckedMatchBranchPattern,
        guard: ?checked.CheckedExprId,
        body: checked.CheckedExprId,
        fallback: Ast.ExprId,
        output: MatchOutput,
        comptime_site: ?Ast.ComptimeSiteId,
        alternative_index: usize,
    ) Allocator.Error!Ast.ExprId {
        const output_ty = self.matchOutputType(output);
        const checked_pattern = self.view.bodies.patterns[@intFromEnum(pattern.pattern)];
        switch (checked_pattern.data) {
            .list => |list| {
                var branch_ctx = try self.childContext(self.current_fn_key);
                defer branch_ctx.deinit();
                var saved = std.ArrayList(BinderRestore).empty;
                defer saved.deinit(self.allocator);
                try branch_ctx.saveMatchPatternBinders(pattern, &saved);
                defer branch_ctx.restoreBinders(saved.items);

                try branch_ctx.preRegisterPatternBinders(pattern.pattern, scrutinee_ty);
                try branch_ctx.applyAlternativeBinderRemaps(pattern.binder_remaps);

                var body_lowered = try branch_ctx.wrapComptimeBranch(
                    comptime_site,
                    alternative_index,
                    try branch_ctx.lowerMatchBranchBody(body, output),
                );
                if (guard) |guard_expr| {
                    const guard_cond = try branch_ctx.lowerExpr(guard_expr);
                    body_lowered = try branch_ctx.builder.ifExpr(guard_cond, body_lowered, fallback, output_ty);
                }

                return try branch_ctx.applyListCheck(scrutinee, scrutinee_ty, list, body_lowered, fallback, output_ty);
            },
            .underscore => {
                var branch_ctx = try self.childContext(self.current_fn_key);
                defer branch_ctx.deinit();
                var saved = std.ArrayList(BinderRestore).empty;
                defer saved.deinit(self.allocator);
                try branch_ctx.saveMatchPatternBinders(pattern, &saved);
                defer branch_ctx.restoreBinders(saved.items);
                try branch_ctx.applyAlternativeBinderRemaps(pattern.binder_remaps);
                const branch_body = try branch_ctx.wrapComptimeBranch(
                    comptime_site,
                    alternative_index,
                    try branch_ctx.lowerMatchBranchBody(body, output),
                );
                if (guard) |guard_expr| {
                    const guard_cond = try branch_ctx.lowerExpr(guard_expr);
                    return try branch_ctx.builder.ifExpr(guard_cond, branch_body, fallback, output_ty);
                }
                return branch_body;
            },
            else => {
                var branch_ctx = try self.childContext(self.current_fn_key);
                defer branch_ctx.deinit();
                var saved = std.ArrayList(BinderRestore).empty;
                defer saved.deinit(self.allocator);
                try branch_ctx.saveMatchPatternBinders(pattern, &saved);
                defer branch_ctx.restoreBinders(saved.items);

                try branch_ctx.preRegisterPatternBinders(pattern.pattern, scrutinee_ty);

                var checks = std.ArrayList(CollectedListPattern).empty;
                defer checks.deinit(branch_ctx.allocator);
                const literal_guards_start = branch_ctx.pattern_literal_guards.items.len;
                const pat = try branch_ctx.lowerPatternAtTypeCollectingLists(pattern.pattern, scrutinee_ty, &checks);
                const literal_guards = try branch_ctx.drainPatternLiteralGuards(literal_guards_start);
                defer branch_ctx.allocator.free(literal_guards);

                try branch_ctx.applyAlternativeBinderRemaps(pattern.binder_remaps);

                var body_lowered = try branch_ctx.wrapComptimeBranch(
                    comptime_site,
                    alternative_index,
                    try branch_ctx.lowerMatchBranchBody(body, output),
                );
                if (guard) |guard_expr| {
                    const guard_cond = try branch_ctx.lowerExpr(guard_expr);
                    body_lowered = try branch_ctx.builder.ifExpr(guard_cond, body_lowered, fallback, output_ty);
                }

                body_lowered = try branch_ctx.applyPatternLiteralGuards(literal_guards, body_lowered, fallback, output_ty);

                var i = checks.items.len;
                while (i > 0) {
                    i -= 1;
                    const entry = checks.items[i];
                    const scrut_expr = try branch_ctx.builder.localExpr(entry.local, entry.ty);
                    body_lowered = try branch_ctx.applyListCheck(scrut_expr, entry.ty, entry, body_lowered, fallback, output_ty);
                }

                const wildcard = try self.builder.program.addPat(.{ .ty = scrutinee_ty, .data = .wildcard });
                const match_branches = [_]Ast.Branch{
                    .{ .pat = pat, .body = body_lowered },
                    .{ .pat = wildcard, .body = fallback },
                };
                return try self.builder.program.addExpr(.{ .ty = output_ty, .data = .{ .match_ = .{
                    .scrutinee = scrutinee,
                    .branches = try self.builder.program.addBranchSpan(&match_branches),
                } } });
            },
        }
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
        const pattern = self.view.bodies.patterns[@intFromEnum(pattern_id)];
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
        const pattern = self.view.bodies.patterns[@intFromEnum(pattern_id)];
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
            .underscore => .wildcard,
        };
        return try self.builder.program.addPat(.{ .ty = ty, .data = data });
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
        const pattern = self.view.bodies.patterns[@intFromEnum(pattern_id)];
        return switch (pattern.data) {
            .list => |list| try self.lowerListPatternBindingThen(value, value_ty, list, result_ty, continuation, miss),
            .record_destructure => |destructs| if (self.recordDestructsNeedExplicitRest(destructs))
                try self.lowerRecordRestPatternBindingThen(value, value_ty, destructs, result_ty, continuation)
            else
                try self.lowerMaterializedPatternValueThen(pattern_id, value, value_ty, result_ty, continuation, miss),
            else => try self.lowerMaterializedPatternValueThen(pattern_id, value, value_ty, result_ty, continuation, miss),
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
                    const rest_ty = try self.lowerType(self.view.bodies.patterns[@intFromEnum(child)].ty);
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
        branch_regions: []const base.Region,
    ) Allocator.Error!Ast.ComptimeSiteId {
        return try self.builder.program.addComptimeSite(kind, region, branch_regions);
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
        const expr = self.view.bodies.exprs[@intFromEnum(expr_id)];
        return try self.addComptimeSite(.match, expr.source_region, branch_regions);
    }

    fn matchBranchRegions(self: *BodyContext, branches: []const checked.CheckedMatchBranch) Allocator.Error![]base.Region {
        const regions = try self.allocator.alloc(base.Region, branchCount(branches));
        var index: usize = 0;
        for (branches) |branch| {
            for (branch.patterns) |pattern| {
                regions[index] = self.view.bodies.patterns[@intFromEnum(pattern.pattern)].source_region;
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
        const expr = self.view.bodies.exprs[@intFromEnum(expr_id)];
        return try self.addComptimeSite(.if_, expr.source_region, branch_regions);
    }

    fn ifBranchRegions(self: *BodyContext, if_: anytype) Allocator.Error![]base.Region {
        const regions = try self.allocator.alloc(base.Region, if_.branches.len + 1);
        for (if_.branches, 0..) |branch, index| {
            regions[index] = self.view.bodies.exprs[@intFromEnum(branch.cond)].source_region;
        }
        regions[if_.branches.len] = self.view.bodies.exprs[@intFromEnum(if_.final_else)].source_region;
        return regions;
    }

    fn lowerMatch(self: *BodyContext, match: anytype, output: MatchOutput, comptime_site: ?Ast.ComptimeSiteId) Allocator.Error!Ast.ExprData {
        const scrutinee_ty = try self.matchScrutineeType(match);
        const scrutinee = try self.lowerExprAtType(match.cond, scrutinee_ty);
        const branches = try self.allocator.alloc(Ast.Branch, branchCount(match.branches));
        defer self.allocator.free(branches);
        var index: usize = 0;
        for (match.branches) |branch| {
            for (branch.patterns) |pattern| {
                var branch_ctx = try self.childContext(self.current_fn_key);
                defer branch_ctx.deinit();
                var saved = std.ArrayList(BinderRestore).empty;
                defer saved.deinit(self.allocator);
                try branch_ctx.saveMatchPatternBinders(pattern, &saved);
                defer branch_ctx.restoreBinders(saved.items);

                const pat = try branch_ctx.lowerPatternAtType(pattern.pattern, scrutinee_ty);
                try branch_ctx.applyAlternativeBinderRemaps(pattern.binder_remaps);
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
        for (pattern.binder_remaps) |remap| {
            try self.saveBinder(remap.candidate_binder, saved);
            try self.saveBinder(remap.representative_binder, saved);
        }
    }

    fn savePatternBinders(
        self: *BodyContext,
        pattern_id: checked.CheckedPatternId,
        saved: *std.ArrayList(BinderRestore),
    ) Allocator.Error!void {
        const pattern = self.view.bodies.patterns[@intFromEnum(pattern_id)];
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
        if (merge_binders.len == 0) return try self.lowerExprAtType(body, result_ty);

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

        const checked_body = self.view.bodies.exprs[@intFromEnum(body)];
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

        const checked_body = self.view.bodies.exprs[@intFromEnum(body)];
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
        const statement = self.view.bodies.statements[@intFromEnum(statement_id)];
        const saved_loc = self.builder.program.current_loc;
        defer self.builder.program.current_loc = saved_loc;
        self.builder.program.current_loc = try self.sourceLocFor(statement.source_region);
        const pattern, const expr = switch (statement.data) {
            .decl => |decl| blk: {
                if (self.statementValueIsLocalProc(decl.expr)) return false;
                break :blk .{ decl.pattern, decl.expr };
            },
            .var_ => |decl| .{ decl.pattern, decl.expr },
            .reassign => |decl| .{ decl.pattern, decl.expr },
            else => return false,
        };

        const pattern_data = self.view.bodies.patterns[@intFromEnum(pattern)].data;
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
            try self.addComptimeSite(.destructure, statement.source_region, &.{})
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
                    const rest_ty = try self.lowerType(self.view.bodies.patterns[@intFromEnum(child)].ty);
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
        if (raw >= self.view.bodies.statements.len) {
            Common.invariant("checked runtime statement filter referenced a missing statement");
        }
        return switch (self.view.bodies.statements[raw].data) {
            .decl,
            .var_,
            .reassign,
            .crash,
            .dbg,
            .expr,
            .expect,
            .for_,
            .while_,
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
        return try self.builder.program.addExpr(.{
            .ty = ty,
            .data = try self.lowerDivergentExprDataAtType(checked_expr_id, ty),
        });
    }

    fn lowerDivergentExprDataAtType(
        self: *BodyContext,
        checked_expr_id: checked.CheckedExprId,
        ty: Type.TypeId,
    ) Allocator.Error!Ast.ExprData {
        const checked_expr = self.view.bodies.exprs[@intFromEnum(checked_expr_id)];
        return switch (checked_expr.data) {
            .block => |block| try self.lowerBlock(block, ty),
            .ellipsis => .{ .crash = try self.builder.program.addStringLiteral("not implemented") },
            .crash => |msg| .{ .crash = try self.lowerStringLiteral(msg) },
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
        if (raw >= self.view.bodies.exprs.len) {
            Common.invariant("checked divergence referenced a missing expression");
        }
        return self.view.bodies.exprDiverges(expr_id);
    }

    fn checkedStatementDiverges(self: *BodyContext, statement_id: checked.CheckedStatementId) bool {
        const raw = @intFromEnum(statement_id);
        if (raw >= self.view.bodies.statements.len) {
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
        if (plan.dispatcher_arg_index >= plan.args.len) Common.invariant("iterator dispatch plan dispatcher argument index was outside the argument span");

        var call_ctx = try BodyContext.init(self.allocator, self.builder, self.view, self.owner_template, self.graph);
        defer call_ctx.deinit();
        call_ctx.owner_context_fn_key = self.owner_context_fn_key;
        call_ctx.current_fn_key = self.current_fn_key;

        const callable_mono_ty = try call_ctx.instantiateIteratorPlanCallTypeFromCaller(plan.callable_ty, self, plan.args, loop_iterator, expected_ret_ty);
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
        const actual_dispatcher_ty = (try self.iteratorOperandMonoType(plan.args[plan.dispatcher_arg_index], loop_iterator, dispatcher_ty)) orelse
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
        const args = try self.allocator.alloc(Ast.ExprId, plan.args.len);
        defer self.allocator.free(args);
        const arg_tys = self.builder.program.types.span(fn_data.args);
        for (plan.args, 0..) |operand, i| {
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
                    const arg_ty = caller.view.bodies.exprs[@intFromEnum(checked_arg)].ty;
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
        const checked_body = self.view.bodies.exprs[@intFromEnum(body)];
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
        const checked_body = self.view.bodies.exprs[@intFromEnum(body)];
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
        const data: Ast.ExprData = if (loop.carries.len == 0)
            .{ .break_ = null }
        else
            .{ .break_ = try self.loopStateExpr(loop.result_ty, loop.carries) };
        return try self.builder.program.addExpr(.{ .ty = loop.result_ty, .data = data });
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
        const expr = self.view.bodies.exprs[@intFromEnum(expr_id)];
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
            => {},
        }
    }

    fn collectReassignedBindersInStatement(
        self: *BodyContext,
        statement_id: checked.CheckedStatementId,
        out: *std.ArrayList(checked.PatternBinderId),
    ) Allocator.Error!void {
        const statement = self.view.bodies.statements[@intFromEnum(statement_id)];
        switch (statement.data) {
            .decl => |decl| try self.collectReassignedBindersInExpr(decl.expr, out),
            .var_ => |var_| try self.collectReassignedBindersInExpr(var_.expr, out),
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
                const tag_text = self.view.names.tagLabelText(tag.name);
                if (Ident.textEql(tag_text, "Done")) {
                    if (tag.args.len != 0) Common.invariant("iterator Done step carried payloads");
                    if (done_tag != null) Common.invariant("iterator step type had duplicate Done tags");
                    done_tag = tag.name;
                } else if (Ident.textEql(tag_text, "One")) {
                    if (tag.args.len != 1) Common.invariant("iterator One step did not carry one payload");
                    if (one_tag != null) Common.invariant("iterator step type had duplicate One tags");
                    one_tag = tag.name;
                    one_payload_ty = tag.args[0];
                } else if (Ident.textEql(tag_text, "Skip")) {
                    if (tag.args.len != 1) Common.invariant("iterator Skip step did not carry one payload");
                    if (skip_tag != null) Common.invariant("iterator step type had duplicate Skip tags");
                    skip_tag = tag.name;
                    skip_payload_ty = tag.args[0];
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
        const statement = self.view.bodies.statements[@intFromEnum(statement_id)];
        const saved_loc = self.builder.program.current_loc;
        defer self.builder.program.current_loc = saved_loc;
        self.builder.program.current_loc = try self.sourceLocFor(statement.source_region);
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
            .break_ => .{ .expr = try self.breakCurrentLoopExpr() },
            .return_ => |ret| .{ .return_ = try self.lowerExpr(ret.expr) },
        };
        return try self.builder.program.addStmt(stmt);
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
            try self.addComptimeSite(.destructure, source_region, &.{})
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
        const pattern = self.view.bodies.patterns[@intFromEnum(pattern_id)];
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
        const checked_expr = self.view.bodies.exprs[@intFromEnum(expr_id)];
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
        return switch (self.view.bodies.exprs[@intFromEnum(expr_id)].data) {
            .lambda,
            .closure,
            => true,
            else => false,
        };
    }

    fn lowerPatternAtType(self: *BodyContext, pattern_id: checked.CheckedPatternId, ty: Type.TypeId) Allocator.Error!Ast.PatId {
        const pattern = self.view.bodies.patterns[@intFromEnum(pattern_id)];
        try self.constrainTypeToMono(pattern.ty, ty);
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
            .list => Common.invariant("list pattern must be lowered to explicit list operations before Monotype output"),
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
            .underscore => .wildcard,
        };
        return try self.builder.program.addPat(.{ .ty = ty, .data = data });
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
        return switch (self.view.bodies.patterns[@intFromEnum(pattern_id)].data) {
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
    const entry = view.bodies.pattern_binders[@intFromEnum(binder)];
    const pattern = view.bodies.patterns[@intFromEnum(entry.pattern)];
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

fn checkedPayload(view: ModuleView, checked_ty: checked.CheckedTypeId) checked.CheckedTypePayload {
    const raw = @intFromEnum(checked_ty);
    if (raw >= view.types.payloads.len) Common.invariant("checked type id outside checked type store");
    return view.types.payloads[raw];
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
            const expr = view.bodies.exprs[@intFromEnum(expr_id)];
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
    if (raw >= view.bodies.pattern_binders.len) Common.invariant("stored function capture binder is outside checked body store");
    const pattern = view.bodies.pattern_binders[raw].pattern;
    const pattern_raw = @intFromEnum(pattern);
    if (pattern_raw >= view.bodies.patterns.len) Common.invariant("stored function capture pattern is outside checked body store");
    return view.bodies.patterns[pattern_raw].ty;
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
    var remaining = view.types.payloads.len;
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
    var remaining = view.types.payloads.len;
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
    for (branches) |branch| count += branch.patterns.len;
    return count;
}

fn moduleBytesEqual(a: [32]u8, b: [32]u8) bool {
    return std.mem.eql(u8, a[0..], b[0..]);
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
