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
    tracy: ?[]const u8,
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

    // Add Tracy support if enabled (bootstrap doesn't link LLVM so pass false)
    addTracySupport(b, roc_modules.build_options, bootstrap_exe, target, false, tracy);

    return bootstrap_exe;
}

/// Add Tracy profiling support to a compile step
/// This is extracted from build.zig's add_tracy function
fn addTracySupport(
    b: *Build,
    module_build_options: *Build.Module,
    exe: *Step.Compile,
    target: ResolvedTarget,
    links_llvm: bool,
    tracy: ?[]const u8,
) void {
    exe.root_module.addImport("build_options", module_build_options);
    if (tracy) |tracy_path| {
        const client_cpp = b.pathJoin(
            &[_][]const u8{ tracy_path, "public", "TracyClient.cpp" },
        );

        // On mingw, we need to opt into windows 7+ to get some features required by tracy.
        const tracy_c_flags: []const []const u8 = if (target.result.os.tag == .windows and target.result.abi == .gnu)
            &[_][]const u8{ "-DTRACY_ENABLE=1", "-fno-sanitize=undefined", "-D_WIN32_WINNT=0x601" }
        else
            &[_][]const u8{ "-DTRACY_ENABLE=1", "-fno-sanitize=undefined" };

        exe.root_module.addIncludePath(.{ .cwd_relative = tracy_path });
        exe.root_module.addCSourceFile(.{ .file = .{ .cwd_relative = client_cpp }, .flags = tracy_c_flags });
        exe.root_module.addCSourceFile(.{ .file = .{ .cwd_relative = "src/build/tracy-shutdown.cpp" }, .flags = tracy_c_flags });
        if (!links_llvm) {
            exe.root_module.linkSystemLibrary("c++", .{ .use_pkg_config = .no });
        }
        exe.root_module.link_libc = true;

        if (target.result.os.tag == .windows) {
            exe.root_module.linkSystemLibrary("dbghelp", .{});
            exe.root_module.linkSystemLibrary("ws2_32", .{});
        }
    }
}
