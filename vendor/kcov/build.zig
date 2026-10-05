const std = @import("std");

const common_flags = [_][]const u8{
    "-g",
    "-Wall",
    "-D_GLIBCXX_USE_NANOSLEEP",
    "-DKCOV_LIBRARY_PREFIX=/tmp",
    "-DKCOV_HAS_LIBBFD=0",
    "-DKCOV_LIBFD_DISASM_STYLED=0",
    // Necessary for debug version to work: https://github.com/ziglang/zig/issues/18521
    "-fno-sanitize=undefined",
};

const c_flags = [_][]const u8{} ++ common_flags;

const cxx_flags = [_][]const u8{
    "-std=c++17",
} ++ common_flags;

const kcov_srcs_cpp = [_][]const u8{
    "src/capabilities.cc",
    "src/collector.cc",
    "src/configuration.cc",
    "src/engine-factory.cc",
    "src/engines/bash-engine.cc",
    "src/engines/system-mode-engine.cc",
    "src/engines/system-mode-file-format.cc",
    "src/engines/python-engine.cc",
    "src/filter.cc",
    "src/gcov.cc",
    "src/main.cc",
    "src/merge-file-parser.cc",
    "src/output-handler.cc",
    "src/parser-manager.cc",
    "src/reporter.cc",
    "src/source-file-cache.cc",
    "src/utils.cc",
    "src/writers/cobertura-writer.cc",
    "src/writers/codecov-writer.cc",
    "src/writers/json-writer.cc",
    "src/writers/html-writer.cc",
    "src/writers/sonarqube-xml-writer.cc",
    "src/writers/writer-base.cc",
    "src/writers/nocover.cc",
    "src/system-mode/file-data.cc",
};

const macho_srcs_cpp = [_][]const u8{
    "src/parsers/macho-parser.cc",
    "src/engines/mach-engine.cc",
};

const elf_srcs_cpp = [_][]const u8{
    "src/engines/ptrace.cc",
    "src/engines/ptrace_linux.cc",
    "src/engines/clang-coverage-engine.cc",
    "src/parsers/elf.cc",
    "src/parsers/elf-parser.cc",
    "src/parsers/dwarf.cc",
    "src/dummy-solib-handler.cc", // Use dummy handler to avoid needing embedded library
};

const elf_srcs_c = [_][]const u8{
    "src/solib-parser/phdr_data.c",
};

const disassembler_srcs_cpp = [_][]const u8{
    "src/parsers/dummy-disassembler.cc",
};

const coveralls_srcs_cpp = [_][]const u8{
    "src/writers/coveralls-writer.cc",
};

pub fn build(b: *std.Build) void {
    const target = b.standardTargetOptions(.{});
    const optimize = b.standardOptimizeOption(.{});

    const os_tag = target.result.os.tag;
    const is_macos = os_tag == .macos;

    // Build helper shared libraries
    const bash_execve_redirector_lib = buildBashExecveRedirectorLib(b, target, optimize);
    const bash_tracefd_cloexec_lib = buildBashTracefdCloexecLib(b, target, optimize);
    const kcov_system_lib = buildKcovSystemLib(b, target, optimize);

    // Use WriteFiles to create generated source files
    const wf = b.addWriteFiles();

    // Generate version.c directly
    _ = wf.add("version.c", "const char *kcov_version = \"42\";\n");

    // Generate html-data-files.cc using Python
    const gen_html = b.addSystemCommand(&.{"python3"});
    gen_html.addFileArg(b.path("src/bin-to-c-source.py"));
    gen_html.addFileArg(b.path("data/bcov.css"));
    gen_html.addArg("css_text");
    gen_html.addFileArg(b.path("data/amber.png"));
    gen_html.addArg("icon_amber");
    gen_html.addFileArg(b.path("data/glass.png"));
    gen_html.addArg("icon_glass");
    gen_html.addFileArg(b.path("data/source-file.html"));
    gen_html.addArg("source_file_text");
    gen_html.addFileArg(b.path("data/index.html"));
    gen_html.addArg("index_text");
    gen_html.addFileArg(b.path("data/js/handlebars.js"));
    gen_html.addArg("handlebars_text");
    gen_html.addFileArg(b.path("data/js/kcov.js"));
    gen_html.addArg("kcov_text");
    gen_html.addFileArg(b.path("data/js/jquery.min.js"));
    gen_html.addArg("jquery_text");
    gen_html.addFileArg(b.path("data/js/jquery.tablesorter.min.js"));
    gen_html.addArg("tablesorter_text");
    gen_html.addFileArg(b.path("data/js/jquery.tablesorter.widgets.min.js"));
    gen_html.addArg("tablesorter_widgets_text");
    gen_html.addFileArg(b.path("data/tablesorter-theme.css"));
    gen_html.addArg("tablesorter_theme_text");
    const html_data_file = wf.addCopyFile(gen_html.captureStdOut(.{}), "html-data-files.cc");

    // Generate bash-helper.cc
    const gen_bash = b.addSystemCommand(&.{"python3"});
    gen_bash.addFileArg(b.path("src/bin-to-c-source.py"));
    gen_bash.addFileArg(b.path("src/engines/bash-helper.sh"));
    gen_bash.addArg("bash_helper");
    gen_bash.addFileArg(b.path("src/engines/bash-helper-debug-trap.sh"));
    gen_bash.addArg("bash_helper_debug_trap");
    const bash_helper_file = wf.addCopyFile(gen_bash.captureStdOut(.{}), "bash-helper.cc");

    // Generate python-helper.cc
    const gen_python = b.addSystemCommand(&.{"python3"});
    gen_python.addFileArg(b.path("src/bin-to-c-source.py"));
    gen_python.addFileArg(b.path("src/engines/python-helper.py"));
    gen_python.addArg("python_helper");
    const python_helper_file = wf.addCopyFile(gen_python.captureStdOut(.{}), "python-helper.cc");

    // Generate bash-redirector-library.cc from compiled library
    const gen_bash_redir = b.addSystemCommand(&.{"python3"});
    gen_bash_redir.addFileArg(b.path("src/bin-to-c-source.py"));
    gen_bash_redir.addArtifactArg(bash_execve_redirector_lib);
    gen_bash_redir.addArg("bash_redirector_library");
    const bash_redir_file = wf.addCopyFile(gen_bash_redir.captureStdOut(.{}), "bash-redirector-library.cc");

    // Generate bash-cloexec-library.cc from compiled library
    const gen_bash_cloexec = b.addSystemCommand(&.{"python3"});
    gen_bash_cloexec.addFileArg(b.path("src/bin-to-c-source.py"));
    gen_bash_cloexec.addArtifactArg(bash_tracefd_cloexec_lib);
    gen_bash_cloexec.addArg("bash_cloexec_library");
    const bash_cloexec_file = wf.addCopyFile(gen_bash_cloexec.captureStdOut(.{}), "bash-cloexec-library.cc");

    // Generate kcov-system-library.cc from compiled library
    const gen_kcov_sys = b.addSystemCommand(&.{"python3"});
    gen_kcov_sys.addFileArg(b.path("src/bin-to-c-source.py"));
    gen_kcov_sys.addArtifactArg(kcov_system_lib);
    gen_kcov_sys.addArg("kcov_system_library");
    const kcov_sys_file = wf.addCopyFile(gen_kcov_sys.captureStdOut(.{}), "kcov-system-library.cc");

    // Build main executable
    const exe_module = b.createModule(.{
        .target = target,
        .optimize = optimize,
        .link_libc = true,
    });

    exe_module.addIncludePath(b.path("src/include"));
    exe_module.addIncludePath(b.path("src/solib-parser"));
    exe_module.addIncludePath(wf.getDirectory());

    exe_module.addCSourceFiles(.{ .files = &kcov_srcs_cpp, .flags = &cxx_flags });
    exe_module.addCSourceFiles(.{ .files = &disassembler_srcs_cpp, .flags = &cxx_flags });
    exe_module.addCSourceFiles(.{ .files = &coveralls_srcs_cpp, .flags = &cxx_flags });

    // Add platform-specific source files
    if (!is_macos) {
        // Linux: add ELF parser and ptrace engine
        exe_module.addCSourceFiles(.{ .files = &elf_srcs_cpp, .flags = &cxx_flags });
        exe_module.addCSourceFiles(.{ .files = &elf_srcs_c, .flags = &c_flags });
    }

    // Variable to hold the mig step for later dependency
    var gen_mach_step: ?*std.Build.Step = null;
    var mig_wf: ?*std.Build.Step.WriteFile = null;

    if (is_macos) {
        exe_module.addCSourceFiles(.{ .files = &macho_srcs_cpp, .flags = &cxx_flags });
        // macOS uses dummy solib handler (no ELF/LD_PRELOAD support)
        exe_module.addCSourceFile(.{
            .file = b.path("src/dummy-solib-handler.cc"),
            .flags = &cxx_flags,
        });

        // Generate mach files on macOS using a separate WriteFile step
        mig_wf = b.addWriteFiles();
        // Add a marker file to establish the directory
        _ = mig_wf.?.add(".mig_marker", "");

        const gen_mach = b.addSystemCommand(&.{"mig"});
        gen_mach.addFileArg(b.path("src/engines/osx/mach_exc.defs"));
        gen_mach.setCwd(mig_wf.?.getDirectory());
        gen_mach_step = &gen_mach.step;

        // Add the mig output directory as include path for headers
        exe_module.addIncludePath(mig_wf.?.getDirectory());

        exe_module.addCSourceFile(.{
            .file = mig_wf.?.getDirectory().path(b, "mach_excServer.c"),
            .flags = &c_flags,
        });
    }

    // Add generated source files
    exe_module.addCSourceFile(.{
        .file = wf.getDirectory().path(b, "version.c"),
        .flags = &c_flags,
    });
    exe_module.addCSourceFile(.{
        .file = html_data_file,
        .flags = &cxx_flags,
    });
    exe_module.addCSourceFile(.{
        .file = bash_helper_file,
        .flags = &cxx_flags,
    });
    exe_module.addCSourceFile(.{
        .file = python_helper_file,
        .flags = &cxx_flags,
    });
    exe_module.addCSourceFile(.{
        .file = bash_redir_file,
        .flags = &cxx_flags,
    });
    exe_module.addCSourceFile(.{
        .file = bash_cloexec_file,
        .flags = &cxx_flags,
    });
    exe_module.addCSourceFile(.{
        .file = kcov_sys_file,
        .flags = &cxx_flags,
    });

    exe_module.linkSystemLibrary("c++", .{});
    exe_module.linkSystemLibrary("curl", .{});
    exe_module.linkSystemLibrary("z", .{});

    if (is_macos) {
        // macOS uses libdwarf for DWARF parsing.
        // The library file is libdwarf.dylib, so the linker name must be "dwarf" (not "libdwarf").
        // Zig prepends "lib" to the name when searching, so "libdwarf" incorrectly searches
        // for "liblibdwarf.dylib". pkg-config (if installed) provides correct flags automatically.
        exe_module.linkSystemLibrary("dwarf", .{ .use_pkg_config = .yes });
        // Add Homebrew paths as fallback when pkg-config is not available.
        // Homebrew's dwarfutils 2.x installs headers in a versioned subdirectory (libdwarf-2/).
        const homebrew = b.graph.environ_map.get("HOMEBREW_PREFIX") orelse
            (if (target.result.cpu.arch == .aarch64) "/opt/homebrew" else "/usr/local");
        exe_module.addSystemIncludePath(.{ .cwd_relative = b.fmt("{s}/include", .{homebrew}) });
        exe_module.addLibraryPath(.{ .cwd_relative = b.fmt("{s}/lib", .{homebrew}) });
    } else {
        // Linux uses elfutils (libelf + libdw)
        exe_module.linkSystemLibrary("libelf", .{ .use_pkg_config = .yes });
        exe_module.linkSystemLibrary("libdw", .{ .use_pkg_config = .yes });
    }

    const exe = b.addExecutable(.{
        .name = "kcov",
        .root_module = exe_module,
    });

    // Add dependency on mig step if on macOS
    if (gen_mach_step) |mig_step| {
        exe.step.dependOn(mig_step);
    }

    // On macOS, kcov needs to be codesigned to use task_for_pid
    if (is_macos) {
        const codesign = b.addSystemCommand(&.{
            "codesign",
            "-s",
            "-",
            "--entitlements",
        });
        codesign.addFileArg(b.path("osx-entitlements.xml"));
        codesign.addArg("-f");
        codesign.addArtifactArg(exe);

        // Make the install step depend on codesign
        b.getInstallStep().dependOn(&codesign.step);
    }

    b.installArtifact(exe);

    // Run step
    const run_cmd = b.addRunArtifact(exe);
    run_cmd.step.dependOn(b.getInstallStep());
    run_cmd.addPassthruArgs();

    const run_step = b.step("run", "Run kcov");
    run_step.dependOn(&run_cmd.step);
}

fn buildBashExecveRedirectorLib(
    b: *std.Build,
    target: std.Build.ResolvedTarget,
    optimize: std.builtin.OptimizeMode,
) *std.Build.Step.Compile {
    const mod = b.createModule(.{
        .target = target,
        .optimize = optimize,
        .link_libc = true,
    });
    mod.addCSourceFiles(.{
        .files = &[_][]const u8{"src/engines/bash-execve-redirector.c"},
        .flags = &c_flags,
    });

    return b.addLibrary(.{
        .name = "bash_execve_redirector",
        .linkage = .dynamic,
        .root_module = mod,
    });
}

fn buildBashTracefdCloexecLib(
    b: *std.Build,
    target: std.Build.ResolvedTarget,
    optimize: std.builtin.OptimizeMode,
) *std.Build.Step.Compile {
    const mod = b.createModule(.{
        .target = target,
        .optimize = optimize,
        .link_libc = true,
    });
    mod.addCSourceFiles(.{
        .files = &[_][]const u8{"src/engines/bash-tracefd-cloexec.c"},
        .flags = &c_flags,
    });

    return b.addLibrary(.{
        .name = "bash_tracefd_cloexec",
        .linkage = .dynamic,
        .root_module = mod,
    });
}

fn buildKcovSystemLib(
    b: *std.Build,
    target: std.Build.ResolvedTarget,
    optimize: std.builtin.OptimizeMode,
) *std.Build.Step.Compile {
    const mod = b.createModule(.{
        .target = target,
        .optimize = optimize,
        .link_libc = true,
    });
    mod.addIncludePath(b.path("src/include"));
    mod.addCSourceFiles(.{
        .files = &[_][]const u8{
            "src/engines/system-mode-binary-lib.cc",
            "src/utils.cc",
            "src/system-mode/registration.cc",
        },
        .flags = &cxx_flags,
    });
    mod.linkSystemLibrary("c++", .{});
    mod.linkSystemLibrary("z", .{});
    mod.linkSystemLibrary("curl", .{});

    return b.addLibrary(.{
        .name = "kcov_system_lib",
        .linkage = .dynamic,
        .root_module = mod,
    });
}
