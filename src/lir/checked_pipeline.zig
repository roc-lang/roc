//! Public checked-module-to-LIR lowering API.
//!
//! This is the only public lowering entrance after checking. It consumes
//! complete checked modules, explicit root requests, and target configuration.
//! It returns LIR or resource failure.

const std = @import("std");
const builtin = @import("builtin");
const base = @import("base");
const check = @import("check");
const core = @import("lir_core");

const Arc = @import("arc.zig");
const Trmc = @import("trmc.zig");
const BoxReuse = @import("box_reuse.zig");
const ReturnSlot = @import("return_slot.zig");
const StrAppend = @import("str_append.zig");
const ScalarizeJoins = @import("scalarize_joins.zig");
const TagReachability = @import("tag_reachability.zig");
const ReachableProcs = @import("reachable_procs.zig");
const LIR = core.LIR;
const LirImage = @import("lir_image.zig");
const LirProgram = core.Program;
const postcheck = @import("postcheck");

const Allocator = std.mem.Allocator;
const checked = check.CheckedModule;

/// Resource failure while lowering checked modules to LIR.
pub const LowerResourceError = Allocator.Error;

/// Root checked module plus the checked imports visible to post-check lowering.
pub const CheckedModuleSet = struct {
    root: checked.LoweringModuleView,
    imports: []const checked.ImportedModuleView = &.{},
};

/// Root requests that determine which checked definitions become LIR roots.
pub const RootRequestSet = struct {
    requests: []const checked.RootRequest = &.{},
    layout_requests: []const checked.CheckedTypeId = &.{},
    /// Request layouts and materialization roots for host-visible provided data.
    include_provided_data_exports: bool = false,
    /// Restore eligible stored constants as internal readonly static values.
    include_internal_static_data: bool = false,
    test_plan_metadata: []const postcheck.Common.RootTestPlanMetadata = &.{},
};

/// Target settings and checked module state for the checked-to-LIR pipeline.
pub const TargetConfig = struct {
    target_usize: base.target.TargetUsize = base.target.TargetUsize.native,
    checked_module_state: CheckedModuleState = .complete,
    inline_mode: InlineMode = .none,
    inline_expects: InlineExpectMode = .run,
    /// Allow `List.map` to reuse a unique input list's allocation when the
    /// input and output element layouts are interchangeable. Optimized builds
    /// enable this; dev builds and compile-time evaluation leave it off so
    /// the in-place branch is dropped during lowering.
    list_in_place_map: bool = false,
    /// Preserve source-level procedure names in LIR for runtime diagnostics.
    proc_debug_names: bool = false,
    /// Control Monotype specialization cache reads and writes.
    monotype_cache: MonotypeCacheControl = .{},
    /// Build ConstStore materialization plans for requested layouts.
    /// Disable this only for consumers that read requested layout metadata and
    /// never materialize requested-layout values.
    layout_request_const_plans: bool = true,
    /// Delete LIR switch edges whose tag discriminants are unreachable. This
    /// is enabled for optimized builds and kept off for dev and compile-time
    /// evaluation.
    tag_reachability: bool = false,
    /// Debug-only: forwarded to `SolvedLirLower.Options.debug_materialized_out`
    /// so a differential harness can execute the Debug verifier's materialized
    /// Lambda Mono program. The slot receives a value only in Debug builds.
    debug_materialized_out: ?*?postcheck.LambdaMono.Ast.Program = null,
};

/// Whether the root checked module is complete or inside checking finalization.
pub const CheckedModuleState = enum {
    complete,
    checking_finalization,
};

pub const RuntimeRecordFieldSchema = postcheck.SolvedLirLower.RuntimeRecordFieldSchema;
pub const RuntimeRecordSchema = postcheck.SolvedLirLower.RuntimeRecordSchema;
pub const RuntimeTagSchema = postcheck.SolvedLirLower.RuntimeTagSchema;
pub const RuntimeTagUnionSchema = postcheck.SolvedLirLower.RuntimeTagUnionSchema;
pub const InlineMode = postcheck.SolvedInline.Mode;
pub const InlineExpectMode = postcheck.SolvedLirLower.InlineExpectMode;
pub const MonotypeCacheControl = postcheck.Monotype.Lower.SpecializationCacheControl;

/// Materialized Lambda Mono program type, re-exported for harnesses that
/// receive one through `TargetConfig.debug_materialized_out`.
pub const LambdaMonoProgram = postcheck.LambdaMono.Ast.Program;

/// Runtime record and tag-union schemas needed by dev tooling.
pub const RuntimeValueSchemaStore = struct {
    allocator: Allocator,
    records: std.ArrayList(RuntimeRecordSchema),
    tag_unions: std.ArrayList(RuntimeTagUnionSchema),

    pub fn init(allocator: Allocator) RuntimeValueSchemaStore {
        return .{
            .allocator = allocator,
            .records = .empty,
            .tag_unions = .empty,
        };
    }

    pub fn deinit(self: *RuntimeValueSchemaStore) void {
        for (self.records.items) |schema| {
            for (schema.fields) |field| self.allocator.free(field.name);
            self.allocator.free(schema.fields);
            self.allocator.free(schema.type_name);
        }
        for (self.tag_unions.items) |schema| {
            for (schema.tags) |tag| self.allocator.free(tag.name);
            self.allocator.free(schema.tags);
            self.allocator.free(schema.type_name);
        }
        self.tag_unions.deinit(self.allocator);
        self.records.deinit(self.allocator);
        self.* = RuntimeValueSchemaStore.init(self.allocator);
    }

    pub fn record(self: *const RuntimeValueSchemaStore, type_name: []const u8) RuntimeRecordSchema {
        for (self.records.items) |schema| {
            if (std.mem.eql(u8, schema.type_name, type_name)) return schema;
        }
        if (builtin.mode == .Debug) {
            std.debug.panic("runtime schema invariant violated: missing record schema for {s}", .{type_name});
        }
        unreachable;
    }

    pub fn tagUnion(self: *const RuntimeValueSchemaStore, type_name: []const u8) RuntimeTagUnionSchema {
        for (self.tag_unions.items) |schema| {
            if (std.mem.eql(u8, schema.type_name, type_name)) return schema;
        }
        if (builtin.mode == .Debug) {
            std.debug.panic("runtime schema invariant violated: missing tag union schema for {s}", .{type_name});
        }
        unreachable;
    }
};

/// Fully lowered LIR program plus root and runtime schema metadata.
pub const LoweredProgram = struct {
    lir_result: LirProgram.Result,
    main_proc: ?LIR.LirProcSpecId,
    target_usize: base.target.TargetUsize,
    runtime_value_schemas: RuntimeValueSchemaStore,

    pub fn deinit(self: *LoweredProgram) void {
        self.runtime_value_schemas.deinit();
        self.lir_result.deinit();
    }

    pub fn platformEntrypoints(
        self: *const LoweredProgram,
        allocator: Allocator,
    ) Allocator.Error![]LirImage.PlatformEntrypoint {
        const root_procs = self.lir_result.root_procs.items;
        const root_metadata = self.lir_result.root_metadata.items;
        if (root_procs.len != root_metadata.len) {
            checkedPipelineInvariant("root metadata count differs from root proc count");
        }

        var entrypoints = std.ArrayList(LirImage.PlatformEntrypoint).empty;
        errdefer entrypoints.deinit(allocator);

        for (root_procs, root_metadata) |root_proc, metadata| {
            if (metadata.abi != .platform and metadata.exposure != .platform_required) continue;
            try entrypoints.append(allocator, .{
                .ordinal = @intCast(entrypoints.items.len),
                .root_proc = root_proc,
            });
        }

        return try entrypoints.toOwnedSlice(allocator);
    }

    pub fn platformEntrypointNames(
        self: *const LoweredProgram,
        allocator: Allocator,
        root_module: *const checked.Module,
    ) Allocator.Error![]const []const u8 {
        const root_metadata = self.lir_result.root_metadata.items;

        var names = std.ArrayList([]const u8).empty;
        errdefer {
            for (names.items) |name| allocator.free(name);
            names.deinit(allocator);
        }

        for (root_metadata) |metadata| {
            if (metadata.abi != .platform and metadata.exposure != .platform_required) continue;
            const root = root_module.lookupRootRequestByOrder(metadata.order) orelse
                checkedPipelineInvariant("platform entrypoint root metadata has no checked root request");
            const name = root_module.entrypointNameForRoot(root) orelse
                checkedPipelineInvariant("platform entrypoint root metadata has no checked entrypoint name");
            try names.append(allocator, try allocator.dupe(u8, name));
        }

        return try names.toOwnedSlice(allocator);
    }
};

/// Lower checked modules and explicit roots directly into an ARC-ready LIR program.
pub fn lowerCheckedModulesToLir(
    allocator: Allocator,
    modules: CheckedModuleSet,
    roots: RootRequestSet,
    target: TargetConfig,
) LowerResourceError!LoweredProgram {
    try verifyCheckedBoundary(modules, target);

    const layout_requests = try collectLayoutRequests(allocator, modules.root.module, roots.layout_requests, roots.include_provided_data_exports);
    defer allocator.free(layout_requests);
    const static_data_requests = switch (target.checked_module_state) {
        .complete => if (roots.include_provided_data_exports)
            try collectStaticDataRequests(allocator, modules.root.module)
        else
            try allocator.alloc(postcheck.Common.StaticDataRequest, 0),
        .checking_finalization => try allocator.alloc(postcheck.Common.StaticDataRequest, 0),
    };
    defer allocator.free(static_data_requests);

    var mono = try postcheck.Monotype.Lower.run(
        allocator,
        checkedModules(modules),
        rootRequests(roots, layout_requests, static_data_requests),
        .{
            .proc_debug_names = target.proc_debug_names,
            .specialization_cache = target.monotype_cache,
            .static_data_literals = target.checked_module_state == .checking_finalization or roots.include_internal_static_data,
            .target_usize = target.target_usize,
            .inline_expects = switch (target.inline_expects) {
                .run => .run,
                .omit => .omit,
            },
        },
    );
    var mono_owned = true;
    errdefer if (mono_owned) mono.deinit();

    var lifted = try postcheck.MonotypeLifted.Lift.run(allocator, mono);
    mono_owned = false;
    mono = undefined;
    var lifted_owned = true;
    errdefer if (lifted_owned) lifted.deinit();

    if (target.inline_mode != .none) {
        try postcheck.MonotypeLifted.SpecConstr.run(allocator, &lifted);
    }
    try postcheck.MonotypeLifted.Lift.recomputeCaptures(allocator, &lifted);

    var solved = try postcheck.LambdaSolved.Solve.run(allocator, lifted);
    lifted_owned = false;
    lifted = undefined;
    var solved_owned = true;
    errdefer if (solved_owned) solved.deinit();

    var inline_plan = try postcheck.SolvedInline.analyze(allocator, target.inline_mode, &solved);
    defer inline_plan.deinit();

    var lowered = try postcheck.SolvedLirLower.run(allocator, target.target_usize, solved, .{
        .inline_plan = inline_plan.view(),
        .inline_expects = target.inline_expects,
        .list_in_place_map = target.list_in_place_map,
        .dict_seed_mode = switch (target.checked_module_state) {
            .complete => .runtime,
            .checking_finalization => .comptime_zero,
        },
        .proc_debug_names = target.proc_debug_names,
        .layout_request_const_plans = target.layout_request_const_plans,
        .test_plan_metadata = roots.test_plan_metadata,
        .debug_materialized_out = target.debug_materialized_out,
    });
    solved_owned = false;
    solved = undefined;
    errdefer lowered.deinit();

    // TRMC/TCE must rewrite recursive procs before ARC insertion: it deletes
    // calls and changes allocation sites, and ARC panics on pre-existing RC
    // statements (see src/lir/trmc.zig).
    try Trmc.run(&lowered.lir_result.store, &lowered.lir_result.layouts);
    try ScalarizeJoins.run(&lowered.lir_result.store, &lowered.lir_result.layouts);
    try BoxReuse.run(&lowered.lir_result.store, &lowered.lir_result.layouts);
    try ReturnSlot.run(&lowered.lir_result.store, &lowered.lir_result.layouts);
    try StrAppend.run(&lowered.lir_result.store);
    if (target.tag_reachability) {
        try TagReachability.run(&lowered.lir_result);
    }
    try ReachableProcs.run(&lowered.lir_result);

    try Arc.insert(&lowered.lir_result.store, &lowered.lir_result.layouts, .{
        .roots = lowered.lir_result.root_procs.items,
        .specialize = target.inline_mode != .none,
    });

    if (roots.requests.len != 0 and lowered.lir_result.root_procs.items.len == 0) {
        checkedPipelineInvariant("explicit root set produced no LIR roots");
    }

    const main_proc: ?LIR.LirProcSpecId = if (lowered.lir_result.root_procs.items.len == 0)
        null
    else
        lowered.lir_result.root_procs.items[0];
    const runtime_value_schemas = convertRuntimeSchemas(allocator, lowered.runtime_schemas);
    lowered.runtime_schemas = postcheck.SolvedLirLower.RuntimeSchemaStore.init(allocator);
    errdefer runtime_value_schemas.deinit();

    const lir_result = lowered.lir_result;
    lowered.lir_result = undefined;

    return .{
        .lir_result = lir_result,
        .main_proc = main_proc,
        .target_usize = target.target_usize,
        .runtime_value_schemas = runtime_value_schemas,
    };
}

fn verifyCheckedBoundary(modules: CheckedModuleSet, target: TargetConfig) Allocator.Error!void {
    if (builtin.mode != .Debug) return;
    switch (target.checked_module_state) {
        .complete => try modules.root.module.verifyComplete(),
        .checking_finalization => modules.root.module.verifyReadyForCompileTimeLowering(),
    }
}

fn checkedModules(modules: CheckedModuleSet) postcheck.Common.CheckedModules {
    return .{
        .root = modules.root,
        .imports = modules.imports,
    };
}

fn rootRequests(
    roots: RootRequestSet,
    layout_requests: []const checked.CheckedTypeId,
    static_data_requests: []const postcheck.Common.StaticDataRequest,
) postcheck.Common.RootRequests {
    return .{
        .requests = roots.requests,
        .layout_requests = layout_requests,
        .static_data_requests = static_data_requests,
        .test_plan_metadata = roots.test_plan_metadata,
    };
}

fn collectLayoutRequests(
    allocator: Allocator,
    _: *const checked.Module,
    explicit: []const checked.CheckedTypeId,
    _: bool,
) Allocator.Error![]checked.CheckedTypeId {
    var requests = std.ArrayList(checked.CheckedTypeId).empty;
    errdefer requests.deinit(allocator);

    try requests.appendSlice(allocator, explicit);
    return try requests.toOwnedSlice(allocator);
}

/// Select ABI roots for native object/archive/shared-library outputs.
pub fn selectPlatformExportRoots(
    allocator: Allocator,
    requests: []const checked.RootRequest,
) Allocator.Error![]checked.RootRequest {
    var selected = std.ArrayList(checked.RootRequest).empty;
    errdefer selected.deinit(allocator);

    for (requests) |request| {
        if (request.kind != .provided_export) continue;
        try selected.append(allocator, request);
    }

    return try selected.toOwnedSlice(allocator);
}

/// Select platform roots for LIR images consumed by host shims/interpreters.
pub fn selectPlatformEntrypointRoots(
    allocator: Allocator,
    requests: []const checked.RootRequest,
) Allocator.Error![]checked.RootRequest {
    var selected = std.ArrayList(checked.RootRequest).empty;
    errdefer selected.deinit(allocator);

    for (requests) |request| {
        switch (request.kind) {
            .provided_export,
            .platform_required_binding,
            => try selected.append(allocator, request),
            else => {},
        }
    }

    return try selected.toOwnedSlice(allocator);
}

fn collectStaticDataRequests(
    allocator: Allocator,
    root: *const checked.Module,
) Allocator.Error![]postcheck.Common.StaticDataRequest {
    var requests = std.ArrayList(postcheck.Common.StaticDataRequest).empty;
    errdefer requests.deinit(allocator);

    for (root.provided_exports.exports) |provided| {
        switch (provided) {
            .data => |data| {
                try requests.append(allocator, .{
                    .const_locator = data.const_ref,
                    .checked_type = data.checked_type,
                });
            },
            .procedure => {},
        }
    }

    return try requests.toOwnedSlice(allocator);
}

fn convertRuntimeSchemas(
    allocator: Allocator,
    input: postcheck.SolvedLirLower.RuntimeSchemaStore,
) RuntimeValueSchemaStore {
    return .{
        .allocator = allocator,
        .records = input.records,
        .tag_unions = input.tag_unions,
    };
}

fn checkedPipelineInvariant(comptime message: []const u8) noreturn {
    if (builtin.mode == .Debug) {
        std.debug.panic("checked pipeline invariant violated: {s}", .{message});
    }
    unreachable;
}

test "checked pipeline declarations are referenced" {
    std.testing.refAllDecls(@This());
}
