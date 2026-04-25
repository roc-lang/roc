//! Checked module artifact boundary.
//!
//! Public post-check lowering is moving toward consuming immutable artifacts and
//! narrowed read-only views instead of loose checked modules plus side stores.

const std = @import("std");
const builtin = @import("builtin");
const base = @import("base");
const can = @import("can");
const types = @import("types");
const symbol = @import("symbol");
const TypedCIR = @import("typed_cir.zig");
const static_dispatch = @import("static_dispatch_registry.zig");

const Allocator = std.mem.Allocator;
const Ident = base.Ident;
const ModuleEnv = can.ModuleEnv;
const CIR = can.CIR;
const Var = types.Var;

pub const CheckedModuleArtifactKey = struct {
    bytes: [32]u8 = [_]u8{0} ** 32,
};

pub const ModuleIdentity = struct {
    module_idx: u32,
    module_name: []const u8,
    display_module_name: Ident.Idx,
    qualified_module_ident: Ident.Idx,
    kind: ModuleEnv.ModuleKind,
};

pub const ImportIdentity = struct {
    import_idx: CIR.Import.Idx,
    resolved_module_idx: ?u32,
};

pub const CheckingContextIdentity = struct {
    imports: []ImportIdentity = &.{},

    pub fn deinit(self: *CheckingContextIdentity, allocator: Allocator) void {
        allocator.free(self.imports);
        self.* = .{};
    }
};

pub const ExportTable = struct {
    defs: []CIR.Def.Idx = &.{},

    pub fn deinit(self: *ExportTable, allocator: Allocator) void {
        allocator.free(self.defs);
        self.* = .{};
    }
};

pub const ProvidesRequiresMetadata = struct {
    provides: []ModuleEnv.ProvidesEntry = &.{},
    requires: []ModuleEnv.RequiredType = &.{},

    pub fn deinit(self: *ProvidesRequiresMetadata, allocator: Allocator) void {
        allocator.free(self.provides);
        allocator.free(self.requires);
        self.* = .{};
    }
};

pub const RootRequestKind = enum {
    runtime_entrypoint,
    provided_export,
    platform_required_binding,
    hosted_export,
    test_expect,
    repl_expr,
    dev_expr,
    compile_time_constant,
};

pub const RootAbi = enum {
    roc,
    platform,
    hosted,
    test_expect,
    compile_time,
};

pub const RootExposure = enum {
    private,
    exported,
    platform_required,
    hosted,
};

pub const RootSource = union(enum) {
    def: CIR.Def.Idx,
    expr: CIR.Expr.Idx,
    statement: CIR.Statement.Idx,
    required_binding: u32,
};

pub const RootRequest = struct {
    order: u32,
    module_idx: u32,
    kind: RootRequestKind,
    source: RootSource,
    checked_type: Var,
    abi: RootAbi,
    exposure: RootExposure,
};

pub const RootRequestTable = struct {
    requests: []RootRequest = &.{},

    pub fn fromModule(allocator: Allocator, module: TypedCIR.Module) Allocator.Error!RootRequestTable {
        var requests = std.ArrayList(RootRequest).empty;
        errdefer requests.deinit(allocator);

        const module_env = module.moduleEnvConst();
        const exports = module_env.store.sliceDefs(module_env.exports);
        for (exports) |def_idx| {
            try appendRoot(&requests, allocator, .{
                .module_idx = module.moduleIndex(),
                .kind = .provided_export,
                .source = .{ .def = def_idx },
                .checked_type = module.defType(def_idx),
                .abi = .roc,
                .exposure = .exported,
            });
        }

        for (module_env.requires_types.items.items, 0..) |required_type, i| {
            try appendRoot(&requests, allocator, .{
                .module_idx = module.moduleIndex(),
                .kind = .platform_required_binding,
                .source = .{ .required_binding = @intCast(i) },
                .checked_type = ModuleEnv.varFrom(required_type.type_anno),
                .abi = .platform,
                .exposure = .platform_required,
            });
        }

        for (module_env.store.sliceStatements(module_env.all_statements)) |statement_idx| {
            const stmt = module_env.store.getStatement(statement_idx);
            if (stmt != .s_expect) continue;
            try appendRoot(&requests, allocator, .{
                .module_idx = module.moduleIndex(),
                .kind = .test_expect,
                .source = .{ .statement = statement_idx },
                .checked_type = ModuleEnv.varFrom(stmt.s_expect.body),
                .abi = .test_expect,
                .exposure = .private,
            });
        }

        for (module.allDefs()) |def_idx| {
            const def = module.def(def_idx);
            if (topLevelExprIsAlreadyProcedure(def.expr.data)) continue;
            try appendRoot(&requests, allocator, .{
                .module_idx = module.moduleIndex(),
                .kind = .compile_time_constant,
                .source = .{ .def = def_idx },
                .checked_type = module.defType(def_idx),
                .abi = .compile_time,
                .exposure = .private,
            });
        }

        return .{ .requests = try requests.toOwnedSlice(allocator) };
    }

    pub fn deinit(self: *RootRequestTable, allocator: Allocator) void {
        allocator.free(self.requests);
        self.* = .{};
    }
};

const RootRequestWithoutOrder = struct {
    module_idx: u32,
    kind: RootRequestKind,
    source: RootSource,
    checked_type: Var,
    abi: RootAbi,
    exposure: RootExposure,
};

fn appendRoot(
    requests: *std.ArrayList(RootRequest),
    allocator: Allocator,
    request: RootRequestWithoutOrder,
) Allocator.Error!void {
    try requests.append(allocator, .{
        .order = @intCast(requests.items.len),
        .module_idx = request.module_idx,
        .kind = request.kind,
        .source = request.source,
        .checked_type = request.checked_type,
        .abi = request.abi,
        .exposure = request.exposure,
    });
}

fn topLevelExprIsAlreadyProcedure(expr: CIR.Expr) bool {
    return switch (expr) {
        .e_lambda, .e_closure, .e_anno_only, .e_hosted_lambda => true,
        else => false,
    };
}

pub const HostedProc = struct {
    module_idx: u32,
    def_idx: CIR.Def.Idx,
    expr_idx: CIR.Expr.Idx,
    symbol_name: Ident.Idx,
    deterministic_index: u32,
};

pub const HostedProcTable = struct {
    procs: []HostedProc = &.{},

    pub fn fromModule(allocator: Allocator, module: TypedCIR.Module) Allocator.Error!HostedProcTable {
        var procs = std.ArrayList(HostedProc).empty;
        errdefer procs.deinit(allocator);

        for (module.allDefs()) |def_idx| {
            const def = module.def(def_idx);
            switch (def.expr.data) {
                .e_hosted_lambda => |hosted| try procs.append(allocator, .{
                    .module_idx = module.moduleIndex(),
                    .def_idx = def_idx,
                    .expr_idx = def.expr.idx,
                    .symbol_name = hosted.symbol_name,
                    .deterministic_index = @intCast(procs.items.len),
                }),
                else => {},
            }
        }

        return .{ .procs = try procs.toOwnedSlice(allocator) };
    }

    pub fn deinit(self: *HostedProcTable, allocator: Allocator) void {
        allocator.free(self.procs);
        self.* = .{};
    }
};

pub const PlatformRequiredBinding = struct {
    module_idx: u32,
    requires_idx: u32,
    ident: Ident.Idx,
    type_anno: CIR.TypeAnno.Idx,
    lookup_target: ?CIR.Def.Idx,
};

pub const PlatformRequiredBindingTable = struct {
    bindings: []PlatformRequiredBinding = &.{},

    pub fn fromModule(allocator: Allocator, module: TypedCIR.Module) Allocator.Error!PlatformRequiredBindingTable {
        const module_env = module.moduleEnvConst();
        const bindings = try allocator.alloc(PlatformRequiredBinding, module_env.requires_types.items.items.len);
        errdefer allocator.free(bindings);

        for (module_env.requires_types.items.items, 0..) |required_type, i| {
            bindings[i] = .{
                .module_idx = module.moduleIndex(),
                .requires_idx = @intCast(i),
                .ident = required_type.ident,
                .type_anno = required_type.type_anno,
                .lookup_target = module.requiredLookupTarget(@intCast(i)),
            };
        }

        return .{ .bindings = bindings };
    }

    pub fn deinit(self: *PlatformRequiredBindingTable, allocator: Allocator) void {
        allocator.free(self.bindings);
        self.* = .{};
    }
};

pub const ModuleInterfaceCapabilities = struct {
    exported_def_count: u32,
    method_count: u32,
    provides_count: u32,
    requires_count: u32,

    pub fn fromModule(module: TypedCIR.Module) ModuleInterfaceCapabilities {
        const module_env = module.moduleEnvConst();
        return .{
            .exported_def_count = @intCast(module_env.store.sliceDefs(module_env.exports).len),
            .method_count = @intCast(module.methodIdentEntries().len),
            .provides_count = @intCast(module_env.provides_entries.items.items.len),
            .requires_count = @intCast(module_env.requires_types.items.items.len),
        };
    }
};

pub const ComptimeSchemaId = enum(u32) { _ };
pub const ComptimeValueId = enum(u32) { _ };

pub const ConstRef = struct {
    artifact: CheckedModuleArtifactKey,
    module_idx: u32,
    pattern: CIR.Pattern.Idx,
    schema: ComptimeSchemaId,
    value: ComptimeValueId,
    source_type: Var,
};

pub const TopLevelValueKind = union(enum) {
    pending,
    serializable_constant: ConstRef,
    procedure_value: symbol.Symbol,
};

pub const TopLevelValueEntry = struct {
    module_idx: u32,
    pattern: CIR.Pattern.Idx,
    source_type: Var,
    value: TopLevelValueKind,
};

pub const TopLevelValueTable = struct {
    entries: []TopLevelValueEntry = &.{},

    pub fn fromModule(allocator: Allocator, module: TypedCIR.Module, proc_symbols: *symbol.Store) Allocator.Error!TopLevelValueTable {
        var entries = std.ArrayList(TopLevelValueEntry).empty;
        errdefer entries.deinit(allocator);

        for (module.allDefs()) |def_idx| {
            const def = module.def(def_idx);
            const value: TopLevelValueKind = if (topLevelExprIsAlreadyProcedure(def.expr.data))
                .{ .procedure_value = try proc_symbols.add(def.patternName() orelse Ident.Idx.NONE, .{
                    .top_level_def = .{
                        .module_idx = module.moduleIndex(),
                        .def_idx = @intFromEnum(def_idx),
                    },
                }) }
            else
                .pending;

            try entries.append(allocator, .{
                .module_idx = module.moduleIndex(),
                .pattern = def.pattern.idx,
                .source_type = module.defType(def_idx),
                .value = value,
            });
        }

        return .{ .entries = try entries.toOwnedSlice(allocator) };
    }

    pub fn deinit(self: *TopLevelValueTable, allocator: Allocator) void {
        allocator.free(self.entries);
        self.* = .{};
    }
};

pub const PromotedProcedure = struct {
    symbol: symbol.Symbol,
    source_binding: CIR.Pattern.Idx,
    source_type: Var,
};

pub const PromotedProcedureTable = struct {
    procedures: []PromotedProcedure = &.{},

    pub fn deinit(self: *PromotedProcedureTable, allocator: Allocator) void {
        allocator.free(self.procedures);
        self.* = .{};
    }
};

pub const CompileTimeValueStore = struct {
    schemas_len: u32 = 0,
    values_len: u32 = 0,

    pub fn deinit(_: *CompileTimeValueStore, _: Allocator) void {}
};

pub const CheckedModuleArtifact = struct {
    key: CheckedModuleArtifactKey,
    module_identity: ModuleIdentity,
    checking_context_identity: CheckingContextIdentity,
    module_env: *const ModuleEnv,
    exports: ExportTable,
    provides_requires: ProvidesRequiresMetadata,
    method_registry: static_dispatch.MethodRegistry,
    static_dispatch_plans: static_dispatch.StaticDispatchPlanTable,
    root_requests: RootRequestTable,
    hosted_procs: HostedProcTable,
    platform_required_bindings: PlatformRequiredBindingTable,
    interface_capabilities: ModuleInterfaceCapabilities,
    top_level_values: TopLevelValueTable,
    promoted_procedures: PromotedProcedureTable,
    compile_time_roots_present: bool = false,
    comptime_values: CompileTimeValueStore,
    proc_symbols: symbol.Store,

    pub fn deinit(self: *CheckedModuleArtifact, allocator: Allocator) void {
        self.proc_symbols.deinit();
        self.comptime_values.deinit(allocator);
        self.promoted_procedures.deinit(allocator);
        self.top_level_values.deinit(allocator);
        self.platform_required_bindings.deinit(allocator);
        self.hosted_procs.deinit(allocator);
        self.root_requests.deinit(allocator);
        self.static_dispatch_plans.deinit(allocator);
        self.method_registry.deinit(allocator);
        self.provides_requires.deinit(allocator);
        self.exports.deinit(allocator);
        self.checking_context_identity.deinit(allocator);
    }

    pub fn verifyPublished(self: *const CheckedModuleArtifact) void {
        if (builtin.mode != .Debug) return;

        std.debug.assert(self.module_identity.module_idx != std.math.maxInt(u32));
        std.debug.assert(self.module_env.module_name.ptr == self.module_identity.module_name.ptr);

        for (self.root_requests.requests, 0..) |request, i| {
            std.debug.assert(request.order == i);
            std.debug.assert(request.module_idx == self.module_identity.module_idx);
        }

        for (self.hosted_procs.procs, 0..) |proc, i| {
            std.debug.assert(proc.deterministic_index == i);
            std.debug.assert(proc.module_idx == self.module_identity.module_idx);
        }

        for (self.platform_required_bindings.bindings, 0..) |binding, i| {
            std.debug.assert(binding.requires_idx == i);
            std.debug.assert(binding.module_idx == self.module_identity.module_idx);
        }
    }
};

pub const ImportedModuleView = struct {
    key: CheckedModuleArtifactKey,
    module_identity: ModuleIdentity,
    exports: *const ExportTable,
    method_registry: *const static_dispatch.MethodRegistry,
    interface_capabilities: *const ModuleInterfaceCapabilities,
    comptime_values: *const CompileTimeValueStore,
};

pub const LoweringModuleView = struct {
    artifact: *const CheckedModuleArtifact,
    roots: *const RootRequestTable,
};

pub fn importedView(artifact: *const CheckedModuleArtifact) ImportedModuleView {
    return .{
        .key = artifact.key,
        .module_identity = artifact.module_identity,
        .exports = &artifact.exports,
        .method_registry = &artifact.method_registry,
        .interface_capabilities = &artifact.interface_capabilities,
        .comptime_values = &artifact.comptime_values,
    };
}

pub fn loweringView(artifact: *const CheckedModuleArtifact) LoweringModuleView {
    return .{
        .artifact = artifact,
        .roots = &artifact.root_requests,
    };
}

pub fn publishFromTypedModule(
    allocator: Allocator,
    modules: *const TypedCIR.Modules,
    module_idx: u32,
) Allocator.Error!CheckedModuleArtifact {
    const module = modules.module(module_idx);
    const module_env = module.moduleEnvConst();

    var proc_symbols = symbol.Store.init(allocator);
    errdefer proc_symbols.deinit();

    const exports = try allocator.dupe(CIR.Def.Idx, module_env.store.sliceDefs(module_env.exports));
    errdefer allocator.free(exports);

    const provides = try allocator.dupe(ModuleEnv.ProvidesEntry, module_env.provides_entries.items.items);
    errdefer allocator.free(provides);

    const requires = try allocator.dupe(ModuleEnv.RequiredType, module_env.requires_types.items.items);
    errdefer allocator.free(requires);

    var method_registry = try static_dispatch.MethodRegistry.fromTypedModules(allocator, modules);
    errdefer method_registry.deinit(allocator);

    var root_requests = try RootRequestTable.fromModule(allocator, module);
    errdefer root_requests.deinit(allocator);

    var hosted_procs = try HostedProcTable.fromModule(allocator, module);
    errdefer hosted_procs.deinit(allocator);

    var platform_required_bindings = try PlatformRequiredBindingTable.fromModule(allocator, module);
    errdefer platform_required_bindings.deinit(allocator);

    var top_level_values = try TopLevelValueTable.fromModule(allocator, module, &proc_symbols);
    errdefer top_level_values.deinit(allocator);

    var artifact = CheckedModuleArtifact{
        .key = .{},
        .module_identity = .{
            .module_idx = module_idx,
            .module_name = module_env.module_name,
            .display_module_name = module_env.display_module_name_idx,
            .qualified_module_ident = module_env.qualified_module_ident,
            .kind = module_env.module_kind,
        },
        .checking_context_identity = .{},
        .module_env = module_env,
        .exports = .{ .defs = exports },
        .provides_requires = .{
            .provides = provides,
            .requires = requires,
        },
        .method_registry = method_registry,
        .static_dispatch_plans = .{},
        .root_requests = root_requests,
        .hosted_procs = hosted_procs,
        .platform_required_bindings = platform_required_bindings,
        .interface_capabilities = ModuleInterfaceCapabilities.fromModule(module),
        .top_level_values = top_level_values,
        .promoted_procedures = .{},
        .comptime_values = .{},
        .proc_symbols = proc_symbols,
    };
    artifact.verifyPublished();
    return artifact;
}

test "artifact views are read-only projections" {
    const artifact = CheckedModuleArtifact{
        .key = .{},
        .module_identity = .{
            .module_idx = 0,
            .module_name = "Test",
            .display_module_name = Ident.Idx.NONE,
            .qualified_module_ident = Ident.Idx.NONE,
            .kind = .package,
        },
        .checking_context_identity = .{},
        .module_env = undefined,
        .exports = .{},
        .provides_requires = .{},
        .method_registry = .{},
        .static_dispatch_plans = .{},
        .root_requests = .{},
        .hosted_procs = .{},
        .platform_required_bindings = .{},
        .interface_capabilities = .{
            .exported_def_count = 0,
            .method_count = 0,
            .provides_count = 0,
            .requires_count = 0,
        },
        .top_level_values = .{},
        .promoted_procedures = .{},
        .comptime_values = .{},
        .proc_symbols = symbol.Store.init(std.testing.allocator),
    };

    const imported = importedView(&artifact);
    const lowering = loweringView(&artifact);
    try std.testing.expect(imported.exports == &artifact.exports);
    try std.testing.expect(lowering.roots == &artifact.root_requests);
}
