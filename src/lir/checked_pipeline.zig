//! Public checked-module-to-LIR lowering API.
//!
//! This is the only public lowering entrance after checking. It consumes
//! published checked modules, explicit root requests, and target configuration.
//! It returns LIR or resource failure.

const std = @import("std");
const builtin = @import("builtin");
const base = @import("base");
const check = @import("check");

const Arc = @import("arc.zig");
const LIR = @import("LIR.zig");
const LirImage = @import("lir_image.zig");
const LirProgram = @import("program.zig");
const postcheck = @import("../postcheck/mod.zig");

const Allocator = std.mem.Allocator;
const checked = check.CheckedModule;

pub const LowerResourceError = Allocator.Error;

pub const CheckedModuleSet = struct {
    root: checked.LoweringModuleView,
    imports: []const checked.ImportedModuleView = &.{},
};

pub const RootRequestSet = struct {
    requests: []const checked.RootRequest = &.{},
    compile_time_requests: []const checked.CompileTimeEvaluationRequest = &.{},
    purpose: RootPurpose = .runtime,
    compile_time_module_sink: ?*checked.Module = null,
};

pub const RootPurpose = enum {
    runtime,
    compile_time,
};

pub const TargetConfig = struct {
    target_usize: base.target.TargetUsize = base.target.TargetUsize.native,
    checked_state: CheckedState = .published,
};

pub const CheckedState = enum {
    published,
    checking_finalization,
};

pub const RuntimeRecordFieldSchema = postcheck.LirLower.RuntimeRecordFieldSchema;
pub const RuntimeRecordSchema = postcheck.LirLower.RuntimeRecordSchema;
pub const RuntimeTagSchema = postcheck.LirLower.RuntimeTagSchema;
pub const RuntimeTagUnionSchema = postcheck.LirLower.RuntimeTagUnionSchema;

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

pub const LoweredProgram = struct {
    lir_result: LirProgram.Result,
    main_proc: LIR.LirProcSpecId,
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

pub fn lowerCheckedModulesToLir(
    allocator: Allocator,
    modules: CheckedModuleSet,
    roots: RootRequestSet,
    target: TargetConfig,
) LowerResourceError!LoweredProgram {
    verifyCheckedBoundary(modules, target);

    var mono = try postcheck.Monotype.Lower.run(
        allocator,
        checkedModules(modules),
        rootRequests(roots),
    );
    var mono_owned = true;
    errdefer if (mono_owned) mono.deinit();

    var lifted = try postcheck.MonotypeLifted.Lift.run(allocator, mono);
    mono_owned = false;
    mono = undefined;
    var lifted_owned = true;
    errdefer if (lifted_owned) lifted.deinit();

    var solved = try postcheck.LambdaSolved.Solve.run(allocator, lifted);
    lifted_owned = false;
    lifted = undefined;
    var solved_owned = true;
    errdefer if (solved_owned) solved.deinit();

    var lambda_mono = try postcheck.LambdaMono.Lower.run(allocator, solved);
    solved_owned = false;
    solved = undefined;
    var lambda_mono_owned = true;
    errdefer if (lambda_mono_owned) lambda_mono.deinit();

    var lowered = try postcheck.LirLower.run(allocator, target.target_usize, lambda_mono);
    lambda_mono_owned = false;
    lambda_mono = undefined;
    errdefer lowered.deinit();

    try Arc.insert(&lowered.lir_result.store, &lowered.lir_result.layouts);

    if (lowered.lir_result.root_procs.items.len == 0) {
        checkedPipelineInvariant("explicit root set produced no LIR roots");
    }

    const main_proc = lowered.lir_result.root_procs.items[0];
    const runtime_value_schemas = convertRuntimeSchemas(allocator, lowered.runtime_schemas);
    lowered.runtime_schemas = postcheck.LirLower.RuntimeSchemaStore.init(allocator);
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

fn verifyCheckedBoundary(modules: CheckedModuleSet, target: TargetConfig) void {
    if (builtin.mode != .Debug) return;
    switch (target.checked_state) {
        .published => modules.root.module.verifyPublished(),
        .checking_finalization => modules.root.module.verifyReadyForCompileTimeLowering(),
    }
}

fn checkedModules(modules: CheckedModuleSet) postcheck.Common.CheckedModules {
    return .{
        .root = modules.root,
        .imports = modules.imports,
    };
}

fn rootRequests(roots: RootRequestSet) postcheck.Common.RootRequests {
    return .{
        .requests = roots.requests,
        .compile_time_requests = roots.compile_time_requests,
        .purpose = switch (roots.purpose) {
            .runtime => .runtime,
            .compile_time => .compile_time,
        },
        .compile_time_module_sink = roots.compile_time_module_sink,
    };
}

fn convertRuntimeSchemas(
    allocator: Allocator,
    input: postcheck.LirLower.RuntimeSchemaStore,
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
