//! Bootstrap compiler builder
//!
//! Builds a minimal roc compiler that uses hardcoded minimal builtins.
//! This bootstrap compiler is used at build time to compile the full builtin .roc files.

const std = @import("std");
const Build = std.Build;
const Step = Build.Step;
const OptimizeMode = std.builtin.OptimizeMode;
const ResolvedTarget = Build.ResolvedTarget;
const modules = @import("modules.zig");

/// Build the bootstrap compiler with minimal builtins
pub fn buildBootstrapCompiler(
    b: *Build,
    roc_modules: *modules.RocModules,
    target: ResolvedTarget,
    optimize: OptimizeMode,
) *Step.Compile {
    const bootstrap_exe = b.addExecutable(.{
        .name = "roc_bootstrap",
        .root_source_file = b.path("src/cli/main.zig"),
        .target = target,
        .optimize = optimize,
        .link_libc = true,
    });

    // Add build option to enable minimal builtins mode
    const bootstrap_options = b.addOptions();
    bootstrap_options.addOption(bool, "use_minimal_builtins", true);
    bootstrap_exe.root_module.addOptions("bootstrap_options", bootstrap_options);

    // Add config module (needed by app_stub.zig and builder.zig)
    const config = b.addOptions();
    config.addOption(bool, "llvm", false); // Bootstrap doesn't need LLVM
    bootstrap_exe.root_module.addOptions("config", config);

    // Add legal_details anonymous import
    bootstrap_exe.root_module.addAnonymousImport("legal_details", .{
        .root_source_file = b.path("legal_details"),
    });

    // Add all the standard roc modules
    roc_modules.addAll(bootstrap_exe);

    return bootstrap_exe;
}
