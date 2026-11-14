//! What each optimization level means for lowering checked modules to LIR:
//! the single source of the lowering options every backend, compile-time
//! evaluation, and the build-time Builtin unit (src/build/builtin_unit.zig)
//! use, so a lowering option lands in all of them at once.

const base = @import("base");
const check = @import("check");
const lir = @import("lir");
const postcheck = @import("postcheck");
const backend = @import("backend");
const compile = @import("compile");
const RocTarget = @import("roc_target").RocTarget;

/// The optimization strategy for the compilation of a Roc program
pub const OptLevel = enum {
    dev,
    interpreter,
    speed,
    size,

    /// What choosing the level means, for the help of flags that take one.
    pub fn description(self: OptLevel) []const u8 {
        return switch (self) {
            .dev => "native dev backend, fast compilation",
            .interpreter => "interpreted, no code generation",
            .speed => "LLVM, optimized for execution speed",
            .size => "LLVM, optimized for binary size",
        };
    }

    /// Convert to the backend evaluation enum used by internal modules
    pub fn toBackend(self: OptLevel) @import("eval").EvalBackend {
        return switch (self) {
            .interpreter => .interpreter,
            .dev => .dev,
            .size, .speed => .llvm,
        };
    }
};

/// The post-check inliner's mode at `opt`.
pub fn postCheckInlineModeForOpt(opt: OptLevel) lir.CheckedPipeline.InlineMode {
    return switch (opt) {
        .size, .speed => .wrappers,
        .dev => lir.CheckedPipeline.dev_inline_mode,
        .interpreter => .none,
    };
}

/// Optimized builds let SpecConstr's value-aware clones chase known values
/// through any direct call; dev builds confine that chase to iterator
/// pipelines, which keeps post-check time and emitted program size bounded
/// while still collapsing `Iter` loops. The interpreter never reaches
/// SpecConstr (its inline mode is `.none`), so its value here is inert.
pub fn specConstrCloneInliningForOpt(opt: OptLevel) postcheck.MonotypeLifted.SpecConstr.CloneInlining {
    return switch (opt) {
        .size, .speed => .all_calls,
        .dev, .interpreter => lir.CheckedPipeline.dev_spec_constr_clone_inlining,
    };
}

/// Only optimized builds, which are what get deployed, keep `Dict`'s runtime
/// seed (`TargetConfig.zero_dict_seed`).
pub fn zeroDictSeedForOpt(opt: OptLevel) bool {
    return switch (opt) {
        .size, .speed => false,
        .dev, .interpreter => true,
    };
}

/// Give dev procedures stable code keys for hot reload (`TargetConfig.site_code_keys`).
pub fn siteCodeKeysForOpt(opt: OptLevel) bool {
    return switch (opt) {
        .dev => true,
        .size, .speed, .interpreter => false,
    };
}

/// Whether list operations reuse a unique input list's allocation at `opt`
/// (`TargetConfig.list_in_place_map`).
pub fn listInPlaceMapForOpt(opt: OptLevel) bool {
    return switch (opt) {
        .size, .speed => true,
        .dev, .interpreter => false,
    };
}

/// Only `--opt=speed` and `--opt=size` hand their LIR to the LLVM backend,
/// the one consumer of branch expectations
/// (`TargetConfig.reads_branch_expectations`).
pub fn readsBranchExpectationsForOpt(opt: OptLevel) bool {
    return switch (opt) {
        .size, .speed => true,
        .dev, .interpreter => false,
    };
}

/// Whether switch edges of unreachable tags are deleted at `opt`
/// (`TargetConfig.tag_reachability`).
pub fn tagReachabilityForOpt(opt: OptLevel) bool {
    return switch (opt) {
        .size, .speed => true,
        .dev, .interpreter => false,
    };
}

/// Whether the LIR rewrites that only speed up the produced program run.
/// Dev builds exist to compile fast; `--opt=speed` and `--opt=size` are where
/// the program's own speed is bought.
pub fn optimizeLirForOpt(opt: OptLevel) bool {
    return switch (opt) {
        .size, .speed => true,
        .dev, .interpreter => false,
    };
}

/// Whether checks proven safe by value-range analysis are elided at `opt`
/// (`TargetConfig.prove_ranges`).
pub fn proveRangesForOpt(opt: OptLevel) bool {
    return switch (opt) {
        .size, .speed => true,
        .dev, .interpreter => false,
    };
}

/// Whether inline expects are compiled in at `opt`.
pub fn inlineExpectModeForOpt(opt: OptLevel) lir.CheckedPipeline.InlineExpectMode {
    return switch (opt) {
        .size, .speed => .omit,
        .dev, .interpreter => .run,
    };
}

/// What a program lowered for its platform entrypoints is consumed as.
pub const PlatformEntrypointArtifact = union(enum) {
    /// Pointer-width-independent LIR consumed directly by the interpreter.
    lir_image,
    /// Target-specific machine code and readonly data consumed by the dev shim.
    dev_run_image: DevRunImage,
};

/// A dev shim image's target, and whether the shim runs it under hot reload,
/// which reserves a header at the start of every erased callable's capture.
pub const DevRunImage = struct {
    target: RocTarget,
    hot_reload: bool,
};

/// Which checked root definitions become LIR roots for a backend.
pub const CheckedLirRoots = union(enum) {
    /// Provided exports: LIR consumed by host
    /// shims and interpreters (run, embedded builds, hot reload, glue).
    platform_entrypoints: PlatformEntrypointArtifact,
    /// Provided exports, with static data
    /// exports materialized: LIR for linked outputs (native/LLVM builds).
    linked_output,
    /// Pre-selected expect/test roots with their plan metadata (roc test).
    test_plan: struct {
        requests: []const check.CheckedArtifact.RootRequest,
        metadata: []const postcheck.Common.RootTestPlanMetadata,
    },
};

/// The single "checked artifacts → LIR" adapter shared by every backend: run,
/// build-LLVM, build-native, embedded, and test lowering all go through here.
///
/// Owns platform-root selection and the optimization-derived lowering option
/// list so a new lowering option lands in every backend at once instead of in
/// whichever hand-copied option lists remembered it.
pub fn checkedRuntimeLoweringConfig(
    roots: CheckedLirRoots,
    opt: OptLevel,
    specialization_strategy: base.SpecializationStrategy,
    target: RocTarget,
    proc_debug_names: bool,
) compile.build.RuntimeLoweringConfig {
    return .{
        .include_provided_data_exports = roots == .linked_output,
        .include_internal_static_data = switch (roots) {
            .linked_output => true,
            .platform_entrypoints => |artifact| artifact == .dev_run_image,
            .test_plan => false,
        },
        .target = .{
            .target_usize = base.target.TargetUsize.fromPtrBitWidth(target.ptrBitWidth()),
            .erased_capture_prefix = devCapturePrefix(roots, opt, target),
            .specialization_strategy = specialization_strategy,
            .inline_mode = postCheckInlineModeForOpt(opt),
            .specialize_arc_modes = optimizeLirForOpt(opt),
            .spec_constr_clone_inlining = specConstrCloneInliningForOpt(opt),
            .consume_dead_boxes = switch (roots) {
                .linked_output => true,
                .platform_entrypoints => |artifact| artifact == .dev_run_image,
                .test_plan => opt == .dev,
            },
            .inline_expects = switch (roots) {
                .test_plan => .run,
                .platform_entrypoints, .linked_output => inlineExpectModeForOpt(opt),
            },
            .list_in_place_map = listInPlaceMapForOpt(opt),
            .zero_dict_seed = zeroDictSeedForOpt(opt),
            .site_code_keys = siteCodeKeysForOpt(opt),
            .owned_proc_signatures = opt == .dev,
            .tag_reachability = tagReachabilityForOpt(opt),
            .reads_branch_expectations = readsBranchExpectationsForOpt(opt),
            .prove_ranges = proveRangesForOpt(opt),
            .reuse_boxes = optimizeLirForOpt(opt),
            .thread_confined_rc = optimizeLirForOpt(opt),
            .prove_uniqueness = optimizeLirForOpt(opt),
            .immortal_string_literals = optimizeLirForOpt(opt),
            .proc_debug_names = proc_debug_names,
        },
    };
}

/// The bytes a program's erased callables reserve at the start of their
/// captures. Native dev code always supports a hot-reload session, so every
/// native dev build, like compile-time evaluation, reserves the hot-reload
/// prefix and compiles the same erased callables; other backends and the
/// interpreter reserve none.
pub fn devCapturePrefix(roots: CheckedLirRoots, opt: OptLevel, target: RocTarget) u32 {
    if (opt != .dev) return 0;
    switch (roots) {
        .platform_entrypoints => |artifact| switch (artifact) {
            .dev_run_image => {},
            .lir_image => return 0,
        },
        .linked_output, .test_plan => {},
    }
    return if (backend.dev.isa.isaOfOrNull(target) != null) @import("builtins").erased_callable.hot_reload_capture_prefix_size else 0;
}

/// The lowering of the module objects compile-time evaluation links against
/// on a compiler running on `target`: a dev build's units, so `roc build`
/// links the objects compile-time evaluation filed. The Builtin unit the
/// compiler's build embeds (src/build/builtin_unit.zig) is built under it.
pub fn compileTimeObjectLowering(target: RocTarget) compile.coordinator.BoxyObjectLowering {
    return .{
        .target = target,
        .config = checkedRuntimeLoweringConfig(.linked_output, .dev, .boxy, target, false).target,
        .opt = @tagName(OptLevel.dev),
        .inline_expects = inlineExpectModeForOpt(.dev) == .run,
    };
}
