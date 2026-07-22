//! Roc command line interface for the new compiler. Entrypoint of the Roc binary.
//! Build with `zig build -Dfuzz -Dsystem-afl=false`.
//! Result is at `./zig-out/bin/roc`
//!
//! ## Module Data Modes
//!
//! The CLI supports two modes for passing compiled Roc programs to the interpreter:
//!
//! ### IPC Interpreter Mode (`roc --opt=interpreter path/to/app.roc`)
//! - Compiles Roc source through ARC-inserted LIR and publishes a viewable LIR image in shared memory
//! - Spawns interpreter host as child process that maps the shared memory
//! - Fast startup, same-architecture only
//! - See: `buildLirImageWithBuildEnv`
//!
//! ### Embedded Interpreter Mode (`roc build --opt=interpreter path/to/app.roc`)
//! - Compiles Roc source through the same checked-artifact to LIR path as IPC mode
//! - Embeds the viewable LIR image directly in the output binary
//! - The interpreter shim receives only the LIR image pointer and length
//!
//! For detailed documentation, see `src/interpreter_shim/README.md`.

const std = @import("std");
const builtin = @import("builtin");
/// Configure std library logging to suppress debug messages in production.
/// This prevents debug logs from polluting stderr which should only contain
/// actual program output (like Stderr.line! calls).
pub const std_options: std.Options = .{
    .log_level = .warn,
    // On Windows, Zig's default segfault handler installs a vectored exception
    // handler that runs before SetUnhandledExceptionFilter and short-circuits
    // our handler in src/base/stack_overflow.zig. Disable it on Windows so our
    // signal-safe handler runs and we get stable exit codes (134/136/139).
    .enable_segfault_handler = builtin.os.tag != .windows and std.debug.default_enable_segfault_handler,
};
var debug_threaded_io_instance: std.Io.Threaded = .init_single_threaded;
/// Override the default debug IO so that `std.Options.debug_io` uses a properly
/// initialized Threaded instance with a real allocator. Without this, the default
/// `global_single_threaded` has `.allocator = .failing` and process spawning fails.
pub const std_options_debug_threaded_io: *std.Io.Threaded = &debug_threaded_io_instance;

const build_options = @import("build_options");
const shim_symbols = @import("builtins").shim_symbols;
const base = @import("base");
const reporting = @import("reporting");
const parse = @import("parse");
const tracy = @import("tracy");
const ctx_mod = @import("ctx");
const compile = @import("compile");
const can = @import("can");
const check = @import("check");
const bundle = @import("bundle");
const unbundle = @import("unbundle");

comptime {
    if (builtin.is_test) {
        _ = @import("libc_finder.zig");
        _ = @import("test_shared_memory_system.zig");
    }
}
const ipc = @import("ipc");
const fmt = @import("fmt");
const eval = @import("eval");
const lir = @import("lir");
const postcheck = @import("postcheck");
const GuardedList = lir.LirStore.GuardedList;
const echo_platform = @import("echo_platform");
const lsp = @import("lsp");
const ansi_term = @import("ansi_term.zig");
const progress = @import("progress.zig");
const watch_mod = if (builtin.target.cpu.arch == .wasm32) struct {
    pub const WatchEvent = struct { path: []const u8 };
    pub const WatchCallbackWithContext = *const fn (context: ?*anyopaque, event: WatchEvent) void;
    pub const Watcher = struct {
        pub fn initAllFiles(
            _: std.mem.Allocator,
            _: std.Io,
            _: []const []const u8,
            _: ?*anyopaque,
            _: WatchCallbackWithContext,
        ) error{OutOfMemory}!*Watcher {
            return error.OutOfMemory;
        }

        pub fn start(_: *Watcher) error{UnsupportedWatchMode}!void {
            return error.UnsupportedWatchMode;
        }

        pub fn deinit(_: *Watcher) void {}
    };
} else @import("watch");

const cli_args = @import("cli_args.zig");
const install_store = @import("install.zig");
const host_symbols = @import("host_symbols.zig");
const roc_target = @import("target.zig");
const target_selection = @import("target_selection.zig");
pub const targets_validator = @import("targets_validator.zig");
const platform_validation = @import("platform_validation.zig");
const cli_context = @import("CliCtx.zig");
const cli_problem = @import("CliProblem.zig");
const ReplLine = @import("ReplLine.zig");
const ReplSession = @import("ReplSession.zig");

const CliCtx = cli_context.CliCtx;
const Io = cli_context.Io;
const Command = cli_context.Command;
const CliError = cli_context.CliError;
const renderProblem = cli_context.renderProblem;

comptime {
    if (builtin.is_test) {
        std.testing.refAllDecls(cli_args);
        std.testing.refAllDecls(progress);
        std.testing.refAllDecls(targets_validator);
        std.testing.refAllDecls(target_selection);
        std.testing.refAllDecls(platform_validation);
        std.testing.refAllDecls(cli_context);
        std.testing.refAllDecls(cli_problem);
        std.testing.refAllDecls(@import("builder.zig"));
        std.testing.refAllDecls(@import("host_symbols.zig"));
        std.testing.refAllDecls(@import("test/platform_config.zig"));
        std.testing.refAllDecls(@import("ReplLine.zig"));
        std.testing.refAllDecls(@import("ReplSession.zig"));
    }
}
const libc_finder = @import("libc_finder.zig");
const linker = @import("linker.zig");
const builder = @import("builder.zig");
const llvm_codegen = @import("llvm_codegen");

/// Check if LLVM is available
const llvm_available = builder.isLLVMAvailable();

const SharedMemoryAllocator = ipc.SharedMemoryAllocator;
const CoreCtx = ctx_mod.CoreCtx;
const CIR = can.CIR;
const ModuleEnv = can.ModuleEnv;
const BuildEnv = compile.BuildEnv;
const Mode = compile.package.Mode;
const TimingInfo = compile.package.TimingInfo;

/// Resolves the worker-thread count from an optional `--max-threads` value,
/// defaulting to the detected CPU count (or 1 when detection fails), and derives
/// the single- vs multi-threaded compilation mode from it. Returned as a
/// `{ thread_count, mode }` tuple for destructuring at the call site.
fn resolveThreadDefaults(max_threads: ?usize) struct { usize, Mode } {
    const thread_count: usize = max_threads orelse (std.Thread.getCpuCount() catch 1);
    return .{ thread_count, if (thread_count <= 1) .single_threaded else .multi_threaded };
}

/// Options for constructing the orchestration core (`BuildEnv`) for a CLI
/// command. Every entry path — check, build, run, test, docs, bundle — wires
/// the core through `initCliBuildEnv`, so thread defaults, the working
/// directory, cache attachment, and the publication mode are configured in
/// exactly one place.
const CliBuildEnvOptions = struct {
    max_threads: ?usize = null,
    /// Explicit `--no-cache`; the only way a pipeline opts out of the
    /// checked-module cache.
    no_cache: bool = false,
    verbose_cache: bool = false,
    resolution_config: compile.package_resolution.Config = .{},
    track_watch_inputs: bool = false,
    /// Identity-only synthetic marking for staged default-app roots (check
    /// sites). Sites that also need diagnostics remapped call
    /// `setSyntheticRootSourceMappingWithLineOffset` themselves, which
    /// implies these identities.
    synthetic_default_app: bool = false,
    source_dir_override: ?[]const u8 = null,
    post_check_publication_mode: compile.build.PostCheckPublicationMode = .executable_artifacts,
    /// Root path tested against the compiler-owned builtin sources; matches
    /// are compiled with the `.builtin` module role (check sites).
    builtin_role_path: ?[]const u8 = null,
    /// The bundle URL a URL/installed root came from; becomes the root's
    /// package identity in place of the extracted path.
    root_source_url: ?[]const u8 = null,
    /// The bundle URL an explicit `--main` came from; becomes the root
    /// identity if that main file ends up as the discovery root.
    main_source_url: ?[]const u8 = null,
};

const InitCliBuildEnvError = Allocator.Error ||
    compile.build.InitError ||
    std.Io.Dir.RealPathFileAllocError;

/// Construct the orchestration core for a CLI command. This is the single
/// place the CLI creates a `BuildEnv`: every pipeline caches unless the user
/// passed an explicit `--no-cache`, and no entry path configures the core ad
/// hoc.
fn initCliBuildEnv(ctx: *CliCtx, opts: CliBuildEnvOptions) InitCliBuildEnvError!BuildEnv {
    const thread_count, const mode = resolveThreadDefaults(opts.max_threads);

    // Arena-owned so the path outlives the returned BuildEnv, which borrows it.
    const cwd = try std.Io.Dir.cwd().realPathFileAlloc(ctx.io.std_io, ".", ctx.arena);

    var build_env = try BuildEnv.init(ctx.gpa, mode, thread_count, RocTarget.detectNative(), cwd, ctx.io.std_io);
    errdefer build_env.deinit();

    build_env.compiler_version = build_options.compiler_version;
    build_env.resolution_config = opts.resolution_config;
    build_env.setWatchInputTracking(opts.track_watch_inputs);
    build_env.setPostCheckPublicationMode(opts.post_check_publication_mode);
    if (opts.synthetic_default_app) {
        // Staged default-app roots and their synthesized platform live in a
        // per-invocation temp dir; identity must be the stable synthetic one
        // so cache keys and nominal identity match across runs and pipelines.
        build_env.setSyntheticRootPackageIdentity();
        build_env.setSyntheticRootPlatformPackageIdentity();
    }
    if (opts.source_dir_override) |source_dir| {
        build_env.setRootSourceDirOverride(source_dir);
    }
    if (opts.builtin_role_path) |path| {
        if (isCompilerOwnedBuiltinSourcePath(ctx.gpa, ctx.io.std_io, path)) {
            build_env.setRootModuleRole(.builtin);
        }
    }
    if (opts.root_source_url) |url| {
        // The URL was validated before any pipeline could receive it, so the
        // only reachable failure here is allocation.
        build_env.setRootUrl(url) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            error.InvalidUrl => unreachable,
        };
    }
    if (opts.main_source_url) |url| {
        build_env.setMainUrl(url) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            error.InvalidUrl => unreachable,
        };
    }
    if (!opts.no_cache) try build_env.enableDefaultCacheManager(opts.verbose_cache);

    return build_env;
}

const CacheManager = compile.CacheManager;
const CacheConfig = compile.CacheConfig;
const cache_config_mod = compile.config;
const backend = @import("backend");
const layout = @import("layout");
const docs = @import("docs");
const bump = @import("bump");
const RocTarget = @import("target.zig").RocTarget;

const CliMainError =
    cli_problem.ReportedError ||
    CliError ||
    Allocator.Error ||
    cli_args.ParseError ||
    libc_finder.FindLibcError ||
    linker.LinkError ||
    bundle.BundleError ||
    unbundle.UnbundleError ||
    unbundle.download.DownloadError ||
    fmt.FormatPathError ||
    fmt.FormatStdinError ||
    ReplLine.ReadLineError ||
    eval.BuiltinModules.InitError ||
    eval.test_helpers.TestHelperError ||
    ipc.CoordinationError ||
    ipc.platform.SharedMemoryError ||
    lir.LirImage.ImageError ||
    lir.CheckedPipeline.LowerResourceError ||
    backend.wasm.WasmModule.NoLinkObjectContractError ||
    backend.wasm.WasmModule.RelocatableEncodeError ||
    backend.wasm.WasmModule.RelocationError ||
    backend.wasm.WasmModule.EncodeError ||
    backend.wasm.WasmModule.SymbolLookupError ||
    backend.RunImage.WriteError ||
    backend.RunImage.ImageError ||
    backend.CompilationError ||
    compile.build.InitError ||
    compile.build.BuildError ||
    compile.build.CompileDiscoveredError ||
    compile.build.BuildWithMainError ||
    compile.package.TypeCheckModuleError ||
    compile.package_resolution.FetchError ||
    CoreCtx.ReadError ||
    CoreCtx.WriteError ||
    CoreCtx.FetchUrlError ||
    CoreCtx.MakePathError ||
    lsp.server.RunWithStdIoError ||
    glue.GlueError ||
    RocCheckError ||
    RocTestError ||
    WatchCommandError ||
    std.Thread.SpawnError ||
    std.process.SpawnError ||
    std.process.Child.WaitError ||
    std.process.RunError ||
    std.process.Args.Iterator.InitError ||
    std.process.Args.ToSliceError ||
    std.process.ExecutablePathError ||
    std.Io.Dir.AccessError ||
    std.Io.Dir.CopyFileError ||
    std.Io.Dir.CreateDirError ||
    std.Io.Dir.CreateDirPathError ||
    std.Io.Dir.DeleteFileError ||
    std.Io.Dir.Iterator.Error ||
    std.Io.Dir.OpenError ||
    std.Io.Dir.ReadFileAllocError ||
    std.Io.Dir.RealPathFileAllocError ||
    std.Io.Dir.StatFileError ||
    std.Io.Dir.WriteFileError ||
    std.Io.File.OpenError ||
    std.Io.File.ReadPositionalError ||
    std.Io.File.ReadStreamingError ||
    std.Io.File.Reader.Error ||
    std.Io.File.StatError ||
    std.Io.File.SyncError ||
    std.Io.File.Writer.Error ||
    std.Io.File.MultiReader.UnendingError ||
    std.Io.net.Ip4Address.ParseError ||
    std.Io.net.Ip6Address.ParseError ||
    std.Io.net.IpAddress.ListenError ||
    std.Io.net.Server.AcceptError ||
    std.Io.net.Stream.Reader.Error ||
    std.Io.net.Stream.Writer.Error ||
    std.Io.Reader.Error ||
    std.Io.Timeout.Error ||
    std.Io.Writer.Error ||
    error{
        ArchiveWriteFailed,
        BrokenDocLinks,
        BuiltinsExtractionFailed,
        CheckFailed,
        CompilationFailed,
        ComptimeExhaustiveness,
        Crash,
        DivisionByZero,
        DocsFailed,
        EmptyArchive,
        EntrypointNotFound,
        ExpectErr,
        ExpectedAppHeader,
        ExpectedPlatformString,
        ExpectedString,
        FailedToCreateUniqueTempDir,
        FdConfigFailed,
        FileNotFound,
        FormattingFailed,
        HandleInheritanceFailed,
        HashMismatch,
        Internal,
        InvalidArguments,
        InvalidDependency,
        InvalidFilename,
        InvalidLirImage,
        InvalidMagic,
        InvalidPackageName,
        InvalidPath,
        InvalidSharedMemory,
        InvalidTarget,
        LLVMCompilationFailed,
        LLVMNotAvailable,
        MissingBundleFiles,
        MissingFilesDirectory,
        MissingTargetFile,
        MissingTargetsSection,
        NativeCompilationFailed,
        NoCacheDir,
        NoPlatformSource,
        NotAnAppHeader,
        PathAlreadyExists,
        PlatformNotSupported,
        ProcessCreationFailed,
        ProcessExitCodeFailed,
        ProcessWaitFailed,
        ReadFailed,
        ResolutionFailed,
        RuntimeError,
        TempDirCreation,
        TestsFailed,
        TypeCheckingFailed,
        UnbundleFailed,
        Unexpected,
        UnresolvedBuiltinImport,
        UnsupportedCrossCompilation,
        UnsupportedHeader,
        UnsupportedTarget,
        UnsupportedWatchMode,
        WasmOutputWriteFailed,
        WriteFailed,
    };

/// Shim archive kind used by cached host executables.
const ShimLibraryKind = enum {
    lir,
    machine_code,
};

/// Embedded shim libraries for the native host target.
/// LIR mode interprets shared-memory or embedded LIR images. Machine-code mode
/// executes dev backend run images directly from shared memory.
const ShimLibraries = struct {
    const interpreter_native = if (builtin.is_test)
        &[_]u8{}
    else if (builtin.target.os.tag == .windows)
        @embedFile("roc_interpreter_shim.lib")
    else
        @embedFile("libroc_interpreter_shim.a");

    const machine_code_native = if (builtin.is_test)
        &[_]u8{}
    else if (builtin.target.os.tag == .windows)
        @embedFile("roc_machine_code_shim.lib")
    else
        @embedFile("libroc_machine_code_shim.a");

    pub fn forTarget(kind: ShimLibraryKind, _: RocTarget) []const u8 {
        return switch (kind) {
            .lir => interpreter_native,
            .machine_code => machine_code_native,
        };
    }
};

fn shimLibraryBytes(kind: ShimLibraryKind, target: ?RocTarget) []const u8 {
    return if (target) |t| ShimLibraries.forTarget(kind, t) else switch (kind) {
        .lir => ShimLibraries.interpreter_native,
        .machine_code => ShimLibraries.machine_code_native,
    };
}

fn shimLibraryDigest(kind: ShimLibraryKind, target: ?RocTarget) [32]u8 {
    var hasher = std.crypto.hash.sha2.Sha256.init(.{});
    updateHashBytes(&hasher, "roc-shim-library-cache-v1");
    updateHashBytes(&hasher, @tagName(kind));
    hasher.update(shimLibraryBytes(kind, target));
    var out: [32]u8 = undefined;
    hasher.final(&out);
    return out;
}

fn shimLibraryCacheFilename(ctx: *CliCtx, kind: ShimLibraryKind, target: RocTarget) Allocator.Error![]const u8 {
    const digest = shimLibraryDigest(kind, target);
    const digest_hex = std.fmt.bytesToHex(digest, .lower);
    const stem = switch (kind) {
        .lir => "roc_interpreter_shim",
        .machine_code => "roc_machine_code_shim",
    };
    return if (target.isWindows())
        std.fmt.allocPrint(ctx.arena, "{s}_{s}_{s}.lib", .{ stem, @tagName(target), digest_hex[0..] })
    else
        std.fmt.allocPrint(ctx.arena, "lib{s}_{s}_{s}.a", .{ stem, @tagName(target), digest_hex[0..] });
}

/// Embedded pre-compiled builtins object files for each target.
/// These contain the wrapper functions needed by the dev backend for string/list operations.
/// Used by `roc build --opt=dev` to link the app object with builtins.
/// Now using static libraries instead of object files to include compiler_rt
/// (needed for 128-bit integer operations used by Dec type).
const BuiltinsObjects = struct {
    /// Native builtins (for host platform builds)
    const native = if (builtin.is_test)
        &[_]u8{}
    else if (builtin.os.tag == .windows)
        @embedFile("roc_builtins.obj")
    else
        @embedFile("roc_builtins.o");

    /// Cross-compilation target builtins (Linux musl targets)
    const x64musl = if (builtin.is_test) &[_]u8{} else @embedFile("targets/x64musl/roc_builtins.o");
    const arm64musl = if (builtin.is_test) &[_]u8{} else @embedFile("targets/arm64musl/roc_builtins.o");

    /// Cross-compilation target builtins (Linux glibc targets)
    const x64glibc = if (builtin.is_test) &[_]u8{} else @embedFile("targets/x64glibc/roc_builtins.o");
    const arm64glibc = if (builtin.is_test) &[_]u8{} else @embedFile("targets/arm64glibc/roc_builtins.o");

    /// WebAssembly target builtins (wasm32-freestanding) - not used by dev backend
    const wasm32 = if (builtin.is_test) &[_]u8{} else @embedFile("targets/wasm32/roc_builtins.o");

    /// Cross-compilation target builtins (Windows targets)
    const x64win = if (builtin.is_test) &[_]u8{} else @embedFile("targets/x64win/roc_builtins.obj");
    const arm64win = if (builtin.is_test) &[_]u8{} else @embedFile("targets/arm64win/roc_builtins.obj");

    /// Cross-compilation target builtins (macOS targets)
    const x64mac = if (builtin.is_test) &[_]u8{} else @embedFile("targets/x64mac/roc_builtins.o");
    const arm64mac = if (builtin.is_test) &[_]u8{} else @embedFile("targets/arm64mac/roc_builtins.o");

    /// Extern-symbol-mode builtins: host operations are linker-resolved
    /// symbols (the symbol ABI) instead of RocOps vtable calls.
    const native_extern = if (builtin.is_test)
        &[_]u8{}
    else if (builtin.os.tag == .windows)
        @embedFile("roc_builtins_extern.obj")
    else
        @embedFile("roc_builtins_extern.o");

    const x64musl_extern = if (builtin.is_test) &[_]u8{} else @embedFile("targets/x64musl/roc_builtins_extern.o");
    const arm64musl_extern = if (builtin.is_test) &[_]u8{} else @embedFile("targets/arm64musl/roc_builtins_extern.o");
    const x64glibc_extern = if (builtin.is_test) &[_]u8{} else @embedFile("targets/x64glibc/roc_builtins_extern.o");
    const arm64glibc_extern = if (builtin.is_test) &[_]u8{} else @embedFile("targets/arm64glibc/roc_builtins_extern.o");
    const wasm32_extern = if (builtin.is_test) &[_]u8{} else @embedFile("targets/wasm32/roc_builtins_extern.o");
    const x64win_extern = if (builtin.is_test) &[_]u8{} else @embedFile("targets/x64win/roc_builtins_extern.obj");
    const arm64win_extern = if (builtin.is_test) &[_]u8{} else @embedFile("targets/arm64win/roc_builtins_extern.obj");
    const x64mac_extern = if (builtin.is_test) &[_]u8{} else @embedFile("targets/x64mac/roc_builtins_extern.o");
    const arm64mac_extern = if (builtin.is_test) &[_]u8{} else @embedFile("targets/arm64mac/roc_builtins_extern.o");

    /// Get the appropriate builtins object bytes for the given target
    pub fn forTarget(target: RocTarget) []const u8 {
        return switch (target) {
            .x64musl => x64musl,
            .arm64musl => arm64musl,
            .x64glibc => x64glibc,
            .arm64glibc => arm64glibc,
            .wasm32 => wasm32,
            .x64win => x64win,
            .arm64win => arm64win,
            .x64mac => x64mac,
            .arm64mac => arm64mac,
            // Fallback for other targets (will use native, may not work for cross-compilation)
            else => native,
        };
    }

    /// Get the extern-symbol-mode builtins object bytes for the given target
    pub fn forTargetExtern(target: RocTarget) []const u8 {
        return switch (target) {
            .x64musl => x64musl_extern,
            .arm64musl => arm64musl_extern,
            .x64glibc => x64glibc_extern,
            .arm64glibc => arm64glibc_extern,
            .wasm32 => wasm32_extern,
            .x64win => x64win_extern,
            .arm64win => arm64win_extern,
            .x64mac => x64mac_extern,
            .arm64mac => arm64mac_extern,
            // Fallback for other targets (will use native, may not work for cross-compilation)
            else => native_extern,
        };
    }

    /// Get the filename for builtins object on given target
    pub fn filename(target: RocTarget) []const u8 {
        return switch (target.toOsTag()) {
            .windows => "roc_builtins.obj",
            else => "roc_builtins.o",
        };
    }

    /// Get the filename for the extern-symbol-mode builtins object on given target
    pub fn filenameExtern(target: RocTarget) []const u8 {
        return switch (target.toOsTag()) {
            .windows => "roc_builtins_extern.obj",
            else => "roc_builtins_extern.o",
        };
    }
};

const DefaultPlatformRuntimeObjects = struct {
    const x64musl = if (builtin.is_test) &[_]u8{} else @embedFile("targets/x64musl/roc_default_platform.o");
    const arm64musl = if (builtin.is_test) &[_]u8{} else @embedFile("targets/arm64musl/roc_default_platform.o");
    const x64glibc = if (builtin.is_test) &[_]u8{} else @embedFile("targets/x64glibc/roc_default_platform.o");
    const arm64glibc = if (builtin.is_test) &[_]u8{} else @embedFile("targets/arm64glibc/roc_default_platform.o");
    const x64mac = if (builtin.is_test) &[_]u8{} else @embedFile("targets/x64mac/roc_default_platform.o");
    const arm64mac = if (builtin.is_test) &[_]u8{} else @embedFile("targets/arm64mac/roc_default_platform.o");
    const x64win = if (builtin.is_test) &[_]u8{} else @embedFile("targets/x64win/roc_default_platform.obj");
    const arm64win = if (builtin.is_test) &[_]u8{} else @embedFile("targets/arm64win/roc_default_platform.obj");

    pub fn forTarget(target: RocTarget) ?[]const u8 {
        return switch (target) {
            .x64musl => x64musl,
            .arm64musl => arm64musl,
            .x64glibc, .x64linux => x64glibc,
            .arm64glibc, .arm64linux => arm64glibc,
            .x64mac => x64mac,
            .arm64mac => arm64mac,
            .x64win => x64win,
            .arm64win => arm64win,
            else => null,
        };
    }

    pub fn filename(target: RocTarget) []const u8 {
        return if (target.isWindows()) "roc_default_platform.obj" else "roc_default_platform.o";
    }
};

// Workaround for Zig standard library compilation issue on macOS ARM64.
//
// The Problem:
// When importing std.c directly, Zig attempts to compile ALL C function declarations,
// including mremap which has this signature in std/c.zig:9562:
//   pub extern "c" fn mremap(addr: ?*align(page_size) const anyopaque, old_len: usize,
//                            new_len: usize, flags: MREMAP, ...) *anyopaque;
//
// The variadic arguments (...) at the end trigger this compiler error on macOS ARM64:
//   "parameter of type 'void' not allowed in function with calling convention 'aarch64_aapcs_darwin'"
//
// This is because:
// 1. mremap is a Linux-specific syscall that doesn't exist on macOS
// 2. The variadic declaration is incompatible with ARM64 macOS calling conventions
// 3. Even though we never call mremap, just importing std.c triggers its compilation
//
// Related issues:
// - https://github.com/ziglang/zig/issues/6321 - Discussion about mremap platform support
// - mremap is only available on Linux/FreeBSD, not macOS/Darwin
//
// Solution:
// Instead of importing all of std.c, we create a minimal wrapper that only exposes
// the specific types and functions we actually need. This avoids triggering the
// compilation of the broken mremap declaration.
//
// TODO: This workaround can be removed once the upstream Zig issue is fixed.
/// Minimal wrapper around std.c types and functions to avoid mremap compilation issues.
/// Contains only the C types and functions we actually need.
pub const c = struct {
    pub const mode_t = std.c.mode_t;
    pub const off_t = std.c.off_t;

    pub const close = std.c.close;
    pub const link = std.c.link;
    pub const ftruncate = std.c.ftruncate;
    pub const _errno = std.c._errno;
};

// Platform-specific shared memory implementation
const is_windows = builtin.target.os.tag == .windows;

var windows_console_configured = false;
var windows_console_previous_code_page: ?std.os.windows.UINT = null;

fn ensureWindowsConsoleSupportsAnsiAndUtf8() void {
    if (!is_windows) return;
    if (windows_console_configured) return;
    windows_console_configured = true;

    // Ensure the legacy console interprets escape sequences and UTF-8 output.
    // GetConsoleOutputCP / SetConsoleOutputCP were removed from std.os.windows.kernel32
    // in Zig 0.16; declare them locally.
    const console = struct {
        extern "kernel32" fn GetConsoleOutputCP() callconv(.winapi) u32;
        extern "kernel32" fn SetConsoleOutputCP(wCodePageID: u32) callconv(.winapi) std.os.windows.BOOL;
    };
    const current_code_page = console.GetConsoleOutputCP();
    if (current_code_page != 0 and current_code_page != 65001) {
        windows_console_previous_code_page = current_code_page;
        _ = console.SetConsoleOutputCP(65001);
    }
    // Note: ANSI escape support is enabled in Io.init()
}

fn restoreWindowsConsoleCodePage() void {
    if (!is_windows) return;
    const console = struct {
        extern "kernel32" fn SetConsoleOutputCP(wCodePageID: u32) callconv(.winapi) std.os.windows.BOOL;
    };
    if (windows_console_previous_code_page) |code_page| {
        windows_console_previous_code_page = null;
        _ = console.SetConsoleOutputCP(code_page);
    }
}

// POSIX shared memory functions
const posix = if (!is_windows) struct {
    extern "c" fn shm_open(name: [*:0]const u8, oflag: c_int, mode: std.c.mode_t) c_int;
    extern "c" fn shm_unlink(name: [*:0]const u8) c_int;
    extern "c" fn munmap(addr: *anyopaque, len: usize) c_int;
} else struct {};

// Windows shared memory functions
const windows = if (is_windows) struct {
    const HANDLE = *anyopaque;
    const DWORD = u32;
    const BOOL = c_int;
    const LPCWSTR = [*:0]const u16;
    const STARTUPINFOW = extern struct {
        cb: DWORD,
        lpReserved: ?LPCWSTR,
        lpDesktop: ?LPCWSTR,
        lpTitle: ?LPCWSTR,
        dwX: DWORD,
        dwY: DWORD,
        dwXSize: DWORD,
        dwYSize: DWORD,
        dwXCountChars: DWORD,
        dwYCountChars: DWORD,
        dwFillAttribute: DWORD,
        dwFlags: DWORD,
        wShowWindow: u16,
        cbReserved2: u16,
        lpReserved2: ?*u8,
        hStdInput: ?HANDLE,
        hStdOutput: ?HANDLE,
        hStdError: ?HANDLE,
    };
    const PROCESS_INFORMATION = extern struct {
        hProcess: HANDLE,
        hThread: HANDLE,
        dwProcessId: DWORD,
        dwThreadId: DWORD,
    };

    extern "kernel32" fn SetHandleInformation(hObject: HANDLE, dwMask: DWORD, dwFlags: DWORD) BOOL;
    extern "kernel32" fn CreateProcessW(
        lpApplicationName: ?LPCWSTR,
        lpCommandLine: ?[*:0]u16,
        lpProcessAttributes: ?*anyopaque,
        lpThreadAttributes: ?*anyopaque,
        bInheritHandles: BOOL,
        dwCreationFlags: DWORD,
        lpEnvironment: ?*anyopaque,
        lpCurrentDirectory: ?LPCWSTR,
        lpStartupInfo: *STARTUPINFOW,
        lpProcessInformation: *PROCESS_INFORMATION,
    ) BOOL;
    extern "kernel32" fn WaitForSingleObject(hHandle: HANDLE, dwMilliseconds: DWORD) DWORD;
    extern "kernel32" fn GetExitCodeProcess(hProcess: HANDLE, lpExitCode: *DWORD) BOOL;

    const HANDLE_FLAG_INHERIT = 0x00000001;
    const INFINITE = 0xFFFFFFFF;
} else struct {};

const Allocator = std.mem.Allocator;
const ColorPalette = reporting.ColorPalette;

const legalDetailsFileContent = @embedFile("legal_details");

/// Preferred size for shared memory allocator: 2TB on 64-bit, 256MB on 32-bit.
///
/// We need a large size because SharedMemoryAllocator is a bump allocator that
/// cannot free memory. During type checking, the types Store grows significantly
/// and every array growth allocates new memory without freeing old, causing
/// memory fragmentation. With a 25KB source file, type checking can use ~2GB
/// of shared memory due to this fragmentation.
///
/// On 64-bit Linux, we reserve 2TB of virtual address space. This is possible
/// without consuming physical memory because memfd_create with lazy page
/// allocation means untouched pages cost nothing.
///
/// On macOS, shm_open + ftruncate creates a Mach VM object with higher per-object
/// kernel overhead than Linux's memfd_create. Using 2TB causes kernel resource pressure
/// that accumulates across rapid sequential process invocations (e.g., running tests
/// in a loop), leading to SIGKILL from the jetsam memory pressure system.
/// We use 8GB on macOS which provides ample headroom while keeping kernel overhead low.
///
/// On Windows, SEC_RESERVE on CreateFileMapping reserves address space without
/// page file backing, but MapViewOfFile still appears to charge against the
/// system commit limit. Under parallel test load (`zig build run-test-zig` with several
/// workers each spawning `roc.exe`), four concurrent 2 TB reservations trip
/// ERROR_COMMITMENT_LIMIT on CI runners (7 GB RAM + limited page file). 8 GB
/// matches the macOS bound and leaves plenty of headroom for real programs.
///
/// On 32-bit targets, we use 256MB since larger sizes won't fit in the address space.
///
/// Test builds may provide an explicit size with `-Dshared-memory-size`.
/// This keeps production Linux at 2TB while allowing Valgrind CI to use a
/// smaller arena that Memcheck can map.
///
/// If the OS rejects the preferred reservation (e.g. aarch64 Linux kernels
/// built with CONFIG_ARM64_VA_BITS=39 cap user VA at ~256 GiB and refuse a
/// 2 TiB mmap with ENOMEM), `createSharedMemory` halves the request down to
/// `SHARED_MEMORY_MIN_SIZE` before giving up. See `createWithMinSize`.
const SHARED_MEMORY_SIZE: usize = if (build_options.has_shared_memory_size)
    configuredSharedMemorySize()
else if (@sizeOf(usize) < 8)
    256 * 1024 * 1024 // 256MB for 32-bit targets
else if (builtin.os.tag == .macos)
    8 * 1024 * 1024 * 1024 // 8GB for macOS (shm_open has higher kernel overhead)
else if (builtin.os.tag == .windows)
    8 * 1024 * 1024 * 1024 // 8GB for Windows (MapViewOfFile commit accounting)
else
    2 * 1024 * 1024 * 1024 * 1024; // 2TB for 64-bit Linux

fn configuredSharedMemorySize() usize {
    if (comptime build_options.shared_memory_size > std.math.maxInt(usize)) {
        @compileError("-Dshared-memory-size does not fit in usize for this target");
    }

    return @intCast(build_options.shared_memory_size);
}

/// Floor for the retry loop in `createSharedMemory`. Set to the
/// macOS/Windows reservation — documented as "ample headroom for real
/// programs" — so a smaller reservation still produces a usable arena. On
/// 32-bit targets the preferred size is already smaller than 8 GiB and an
/// 8 GiB literal doesn't fit in `usize`, so the floor is the preferred size
/// itself (single attempt, no retry); `-Dshared-memory-size` builds are
/// likewise handled by the allocator clamping `min_size` to the preferred.
const SHARED_MEMORY_MIN_SIZE: usize = if (@sizeOf(usize) < 8)
    SHARED_MEMORY_SIZE
else
    8 * 1024 * 1024 * 1024;

/// Create the shared-memory arena used for the parent-produced LIR runtime
/// image. Tries the preferred size first and halves down to
/// `SHARED_MEMORY_MIN_SIZE` if the OS rejects the reservation; see
/// `SharedMemoryAllocator.createWithMinSize` for details.
fn createSharedMemory(io: std.Io, page_size: usize) error{ CreateFileMappingFailed, FtruncateFailed, InvalidHandle, MapViewOfFileFailed, MemfdCreateFailed, MmapFailed, OpenFileMappingFailed, OutOfMemory, ShmOpenFailed, ShmUnlinkFailed, TempFileOpenFailed, TempFileUnlinkFailed, UnsupportedPlatform }!SharedMemoryAllocator {
    return SharedMemoryAllocator.createWithMinSize(io, SHARED_MEMORY_SIZE, SHARED_MEMORY_MIN_SIZE, page_size);
}

/// Create the shared-memory arena used for dev-shim machine code.
fn createExecutableSharedMemory(io: std.Io, page_size: usize) error{ CreateFileMappingFailed, FtruncateFailed, InvalidHandle, MapViewOfFileFailed, MemfdCreateFailed, MmapFailed, OpenFileMappingFailed, OutOfMemory, ShmOpenFailed, ShmUnlinkFailed, TempFileOpenFailed, TempFileUnlinkFailed, UnsupportedPlatform }!SharedMemoryAllocator {
    return SharedMemoryAllocator.createExecutableWithMinSize(io, SHARED_MEMORY_SIZE, SHARED_MEMORY_MIN_SIZE, page_size);
}

/// Cross-platform hardlink creation
fn createHardlink(ctx: *CliCtx, source: []const u8, dest: []const u8) (Allocator.Error || error{ InvalidUtf8, PathAlreadyExists, Unexpected })!void {
    if (comptime builtin.target.os.tag == .windows) {
        // On Windows, use CreateHardLinkW
        const source_w = try std.unicode.utf8ToUtf16LeAllocZ(ctx.arena, source);
        const dest_w = try std.unicode.utf8ToUtf16LeAllocZ(ctx.arena, dest);

        // Declare CreateHardLinkW since it's not in all versions of std
        const kernel32 = struct {
            extern "kernel32" fn CreateHardLinkW(
                lpFileName: [*:0]const u16,
                lpExistingFileName: [*:0]const u16,
                lpSecurityAttributes: ?*anyopaque,
            ) callconv(.winapi) std.os.windows.BOOL;
        };

        if (kernel32.CreateHardLinkW(dest_w, source_w, null) == .FALSE) {
            const err = std.os.windows.GetLastError();
            switch (err) {
                .ALREADY_EXISTS => return error.PathAlreadyExists,
                else => return error.Unexpected,
            }
        }
    } else {
        // On POSIX systems, use the link system call
        const source_c = try ctx.arena.dupeZ(u8, source);
        const dest_c = try ctx.arena.dupeZ(u8, dest);

        const result = c.link(source_c, dest_c);
        if (result != 0) {
            const errno = c._errno().*;
            switch (errno) {
                17 => return error.PathAlreadyExists, // EEXIST
                else => return error.Unexpected,
            }
        }
    }
}

/// Generate a cryptographically secure random ASCII string for directory names
fn generateRandomSuffix(ctx: *CliCtx) Allocator.Error![]u8 {
    // TODO: Consider switching to a library like https://github.com/abhinav/temp.zig
    // for more robust temporary file/directory handling
    const charset = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789";

    const suffix = try ctx.arena.alloc(u8, 32);

    // Fill with cryptographically secure random bytes
    ctx.io.std_io.random(suffix);

    // Convert to ASCII characters from our charset
    for (suffix) |*byte| {
        byte.* = charset[byte.* % charset.len];
    }

    return suffix;
}

/// Create a unique temporary directory under roc/{version}/{random}/.
/// Returns the path to the directory (allocated from arena, no need to free).
/// Uses system temp directory to avoid race conditions when cache is cleared.
pub fn createUniqueTempDir(ctx: *CliCtx) (Allocator.Error || std.Io.Dir.CreateDirPathError || std.Io.Dir.CreateDirError || error{FailedToCreateUniqueTempDir})![]const u8 {
    // Get the version-specific temp directory: {temp}/roc/{version}
    const version_temp_dir = try cache_config_mod.getVersionTempDir(ctx.coreCtx(), ctx.arena);

    // Ensure the roc/{version} directory exists
    // makePath automatically handles PathAlreadyExists internally
    try std.Io.Dir.cwd().createDirPath(ctx.io.std_io, version_temp_dir);

    // Try to create a unique subdirectory with random suffix
    var attempt: u8 = 0;
    while (attempt < 6) : (attempt += 1) {
        const random_suffix = try generateRandomSuffix(ctx);
        const dir_path = try std.fs.path.join(ctx.arena, &.{ version_temp_dir, random_suffix });

        // Try to create the directory
        std.Io.Dir.cwd().createDir(ctx.io.std_io, dir_path, .default_dir) catch |err| switch (err) {
            error.PathAlreadyExists => {
                // Directory already exists, try again with a new random suffix
                continue;
            },
            else => {
                return err;
            },
        };

        return dir_path;
    }

    // Failed after 6 attempts
    return error.FailedToCreateUniqueTempDir;
}

/// Write shared memory coordination file (.txt) next to the executable.
/// This is the file that the child process reads to find the shared memory fd.
const WriteFdCoordinationFileError = Allocator.Error || std.Io.File.OpenError || std.Io.File.Writer.Error || std.Io.File.SyncError || error{InvalidPath};

/// Write the shared memory coordination file used by POSIX fd inheritance.
pub fn writeFdCoordinationFile(ctx: *CliCtx, temp_exe_path: []const u8, shm_handle: SharedMemoryHandle) WriteFdCoordinationFileError!void {
    // The coordination file is at {temp_dir}.txt where temp_dir is the directory containing the exe
    const temp_dir = std.fs.path.dirname(temp_exe_path) orelse return error.InvalidPath;

    // Ensure we have no trailing slashes
    var dir_path = temp_dir;
    while (dir_path.len > 0 and (dir_path[dir_path.len - 1] == '/' or dir_path[dir_path.len - 1] == '\\')) {
        dir_path = dir_path[0 .. dir_path.len - 1];
    }

    const fd_file_path = try std.fmt.allocPrint(ctx.arena, "{s}.txt", .{dir_path});

    // Create the file (exclusive - fail if exists to detect collisions)
    const fd_file = std.Io.Dir.cwd().createFile(ctx.io.std_io, fd_file_path, .{ .exclusive = true }) catch |err| {
        // Error is handled by caller with ctx.fail()
        return err;
    };
    defer fd_file.close(ctx.io.std_io);

    // Write shared memory info to file. The handle is written as a plain
    // integer on both platforms -- on Windows it is a HANDLE (a pointer), and
    // `ipc.coordination.parseHandle` turns the integer back into one.
    const handle_int = if (is_windows) @intFromPtr(shm_handle.fd) else shm_handle.fd;
    const fd_str = try std.fmt.allocPrint(ctx.arena, "{}\n{}", .{ handle_int, shm_handle.size });
    try fd_file.writeStreamingAll(ctx.io.std_io, fd_str);
    try fd_file.sync(ctx.io.std_io);
}

/// Create the temporary directory structure for fd communication.
/// Returns the path to the executable in the temp directory (allocated from arena, no need to free).
/// Uses the standard roc/{version}/{random}/ structure in the system temp directory.
/// The exe_display_name is the name that will appear in `ps` output (e.g., "app.roc").
pub fn createTempDirStructure(ctx: *CliCtx, exe_path: []const u8, exe_display_name: []const u8, shm_handle: SharedMemoryHandle, _: ?[]const u8) Allocator.Error![]const u8 {
    // Get the version-specific temp directory: {temp}/roc/{version}
    const version_temp_dir = try cache_config_mod.getVersionTempDir(ctx.coreCtx(), ctx.arena);

    // Ensure the roc/{version} directory exists
    // makePath automatically handles PathAlreadyExists internally
    try std.Io.Dir.cwd().createDirPath(ctx.io.std_io, version_temp_dir);

    // Try to create a unique subdirectory with random suffix
    var attempt: u8 = 0;
    while (attempt < 6) : (attempt += 1) {
        const random_suffix = try generateRandomSuffix(ctx);
        const temp_dir_path = try std.fs.path.join(ctx.arena, &.{ version_temp_dir, random_suffix });

        // The coordination file path is the directory path with .txt appended
        const dir_name_with_txt = try std.fmt.allocPrint(ctx.arena, "{s}.txt", .{temp_dir_path});

        // Try to create the directory
        std.Io.Dir.cwd().createDir(ctx.io.std_io, temp_dir_path, .default_dir) catch |err| switch (err) {
            error.PathAlreadyExists => {
                // Directory already exists, try again with a new random suffix
                continue;
            },
            else => {
                return err;
            },
        };

        // Try to create the fd file
        const fd_file = std.Io.Dir.cwd().createFile(ctx.io.std_io, dir_name_with_txt, .{ .exclusive = true }) catch |err| switch (err) {
            error.PathAlreadyExists => {
                // File already exists, remove the directory and try again
                std.Io.Dir.cwd().deleteDir(ctx.io.std_io, temp_dir_path) catch {};
                continue;
            },
            else => {
                // Clean up directory on other errors
                std.Io.Dir.cwd().deleteDir(ctx.io.std_io, temp_dir_path) catch {};
                return err;
            },
        };
        // Note: We'll close this explicitly later, before spawning the child

        // Write shared memory info to file (POSIX only - Windows uses command line args)
        const fd_str = try std.fmt.allocPrint(ctx.arena, "{}\n{}", .{ shm_handle.fd, shm_handle.size });

        try fd_file.writeStreamingAll(ctx.io.std_io, fd_str);

        // IMPORTANT: Flush and close the file explicitly before spawning child process
        // On Windows, having the file open can prevent child process access
        try fd_file.sync(ctx.io.std_io); // Ensure data is written to disk
        fd_file.close(ctx.io.std_io);

        // Create hardlink to executable in temp directory with display name
        const temp_exe_path = try std.fs.path.join(ctx.arena, &.{ temp_dir_path, exe_display_name });

        // Try to create a hardlink first (more efficient than copying)
        createHardlink(ctx, exe_path, temp_exe_path) catch {
            // If hardlinking fails for any reason, fall back to copying
            // Common reasons: cross-device link, permissions, file already exists
            try std.Io.Dir.cwd().copyFile(exe_path, std.Io.Dir.cwd(), temp_exe_path, ctx.io.std_io, .{});
        };

        return temp_exe_path;
    }

    // Failed after 6 attempts
    return error.FailedToCreateUniqueTempDir;
}

var debug_allocator: std.heap.DebugAllocator(.{ .stack_trace_frames = build_options.debug_gpa_stack_trace_frames }) = .{
    .backing_allocator = std.heap.page_allocator,
};

fn renderValidationError(
    allocator: std.mem.Allocator,
    result: platform_validation.ValidationResult,
    stderr: anytype,
) void {
    const rendered = platform_validation.renderValidationError(allocator, result, stderr);
    if (rendered) {} else {}
}

fn renderDiagnostics(build_env: *BuildEnv, stderr: anytype) Allocator.Error!void {
    const diag = try build_env.renderDiagnostics(stderr);
    if (diag.errors > 0) {} else {}
}

/// The CLI entrypoint for the Roc compiler.
pub fn main(init: std.process.Init) Allocator.Error!void {
    // Initialize the debug IO with a real allocator so std.Options.debug_io
    // can spawn processes, create directories, etc.
    debug_threaded_io_instance = .init(init.gpa, .{
        .argv0 = .init(init.minimal.args),
        .environ = init.minimal.environ,
    });
    defer debug_threaded_io_instance.deinit();

    // Install stack overflow handler early, before any significant work.
    // This gives us a helpful error message instead of a generic segfault
    // if the compiler blows the stack (e.g., due to infinite recursion in type translation).
    const stack_overflow_installed = base.stack_overflow.installForCurrentThread();
    if (comptime builtin.mode == .Debug) {
        std.debug.assert(stack_overflow_installed);
    } else if (!stack_overflow_installed) {
        unreachable;
    }

    var gpa_tracy: tracy.TracyAllocator(null) = undefined;
    var gpa, const is_safe = gpa: {
        // Debug builds use the leak-checking debug allocator; -Ddebug-gpa forces it
        // in release builds too (e.g. to leak-check a ReleaseSafe binary). Everything
        // else uses the fast target allocator — see base.defaultGpa.
        const use_debug_allocator = builtin.os.tag != .freestanding and
            (builtin.mode == .Debug or build_options.debug_gpa);
        if (use_debug_allocator) {
            // Under Valgrind, use libc's malloc instead: Valgrind can't see the
            // debug allocator's sub-allocations (it carves them out of mmap'd
            // pages) but tracks every malloc/free. Builds with Valgrind support
            // carry the client requests, so this can auto-switch under Valgrind.
            if (builtin.link_libc and std.valgrind.runningOnValgrind() != 0) {
                break :gpa .{ std.heap.c_allocator, false };
            }
            break :gpa .{ debug_allocator.allocator(), true };
        }
        break :gpa .{ base.defaultGpa(), false };
    };
    defer restoreWindowsConsoleCodePage();
    defer if (is_safe) {
        std.debug.assert(build_options.debugGpaOk(debug_allocator.deinit()));
    };

    if (tracy.enable_allocation) {
        gpa_tracy = tracy.tracyAllocator(gpa);
        gpa = gpa_tracy.allocator();
    }

    var arena_impl = base.SingleThreadArena.init(gpa);
    defer arena_impl.deinit();
    const arena = arena_impl.allocator();

    var args_list: std.ArrayList([]const u8) = .empty;
    defer args_list.deinit(arena);
    var args_iter = try init.minimal.args.iterateAllocator(arena);
    while (args_iter.next()) |arg| {
        try args_list.append(arena, arg);
    }
    const args = args_list.items;

    mainArgs(gpa, arena, args, init.io) catch |err| {
        // Handle OutOfMemory specially - it may not have been printed
        switch (err) {
            error.OutOfMemory => {
                // Use std.debug.print to stderr since we don't have access to ctx.io here
                // TODO: if virtual address allocation fails at 4gb, fall back on doing `roc build` followed by manually running the executable
                std.debug.print("The Roc compiler ran out of memory trying to preallocate virtual address space for compiling and running this program. Try using `roc build` to build the executable separately, then run it manually.\n", .{});
            },
            else => {
                // All other errors: problems were already recorded/rendered by the
                // command handlers; exit cleanly below without a stack trace.
            },
        }
        // Exit cleanly without showing a stack trace to the user.
        if (tracy.enable) {
            tracy.waitForShutdown(init.io);
        }
        restoreWindowsConsoleCodePage();
        std.process.exit(1);
    };

    if (tracy.enable) {
        tracy.waitForShutdown(init.io);
    }
}

fn parsedArgsStartBackgroundCleanup(args: cli_args.CliArgs) bool {
    return switch (args) {
        .run, .build, .check, .test_cmd, .docs, .bump, .glue, .install, .experimental_lsp => true,
        .fmt, .bundle, .unbundle, .repl, .version, .help, .licenses, .problem => false,
    };
}

fn startBackgroundCacheCleanup(gpa: Allocator, arena: Allocator, std_io: std.Io) void {
    // Start background cache cleanup on a separate thread.
    // This is a fire-and-forget thread that:
    // - Cleans up stale temp directories (>5 min old)
    // - Cleans up old persistent cache files (>30 days old)
    // - Exits automatically when done
    //
    // We intentionally don't join the thread. If the main process exits before
    // cleanup completes, the OS will automatically terminate the cleanup thread.
    // This ensures cleanup never delays compilation or execution.
    //
    // Resolve the temp/cache locations here using the same resolver the cache
    // writer uses, so cleanup can never target a different directory than where
    // artifacts are written. The background thread itself is CoreCtx-free and
    // allocation-free; it only borrows these base paths (copied in by value).
    const cleanup_ctx = CoreCtx.default(gpa, arena, std_io);
    const temp_base: []const u8 = cache_config_mod.getTempDir(cleanup_ctx, arena) catch "";
    const cache_base: []const u8 = blk: {
        const cfg = cache_config_mod.CacheConfig{ .roc_ctx = cleanup_ctx };
        break :blk cfg.getEffectiveCacheDir(arena) catch "";
    };
    if (temp_base.len != 0 or cache_base.len != 0) {
        if (compile.CacheCleanup.startBackgroundCleanup(temp_base, cache_base, std_io)) |_| {
            // Thread started successfully, will run in background.
        } else |_| {
            // Non-fatal: cleanup failure shouldn't prevent compilation.
            std.log.debug("Failed to start background cleanup thread", .{});
        }
    }
}

fn mainArgs(gpa: Allocator, arena: Allocator, args: []const []const u8, std_io: std.Io) CliMainError!void {
    const trace = tracy.trace(@src());
    defer trace.end();

    ensureWindowsConsoleSupportsAnsiAndUtf8();

    // Create I/O interface - this is passed to all command handlers via ctx
    var io = Io.create(std_io);

    if (args.len >= 2 and std.mem.eql(u8, args[1], hot_reload_dev_command)) {
        var ctx = CliCtx.init(gpa, arena, &io, .run);
        ctx.initIo();
        defer ctx.deinit();
        return rocInternalHotReloadDev(&ctx, args[2..]);
    }

    const parsed_args = try cli_args.parse(arena, std_io, args[1..]);
    if (parsedArgsStartBackgroundCleanup(parsed_args)) {
        startBackgroundCacheCleanup(gpa, arena, std_io);
    }

    // Determine command for context
    const command: Command = switch (parsed_args) {
        .run => .run,
        .build => .build,
        .check => .check,
        .test_cmd => .test_cmd,
        .fmt => .fmt,
        .bundle => .bundle,
        .unbundle => .unbundle,
        .bump => .bump,
        .install => .install,
        else => .unknown,
    };

    // Create CLI context at the top level - this is passed to all command handlers
    var ctx = CliCtx.init(gpa, arena, &io, command);
    ctx.initIo(); // Must be called after ctx is at its final stack location
    defer ctx.deinit(); // deinit flushes I/O

    try switch (parsed_args) {
        .run => |run_args| {
            if (std.mem.eql(u8, run_args.path, "main.roc")) {
                std.Io.Dir.cwd().access(ctx.io.std_io, run_args.path, .{}) catch |err| switch (err) {
                    error.FileNotFound => {
                        const cwd_path = std.Io.Dir.cwd().realPathFileAlloc(ctx.io.std_io, ".", arena) catch |real_err| {
                            ctx.io.stderr().print(
                                "Error: No app file specified and default 'main.roc' was not found. Additionally, the current directory could not be resolved: {}\n",
                                .{real_err},
                            ) catch {};
                            return error.FileNotFound;
                        };
                        ctx.io.stderr().print(
                            "Error: No app file specified and default 'main.roc' was not found in {s}\n",
                            .{cwd_path},
                        ) catch {};
                        ctx.io.stderr().print(
                            "\nHint: pass an explicit path (e.g. `roc my-app.roc`) or create a 'main.roc' in that directory.\n",
                            .{},
                        ) catch {};
                        return error.FileNotFound;
                    },
                    else => {
                        ctx.io.stderr().print(
                            "Error: Unable to access default 'main.roc': {}\n",
                            .{err},
                        ) catch {};
                        return err;
                    },
                };
            }

            rocRun(&ctx, run_args, args[0]) catch |err| switch (err) {
                error.CliError => {
                    // Problems already recorded in context, render them below
                },
                else => return err,
            };
        },
        .check => |check_args| rocCheck(&ctx, check_args, args[0]),
        .build => |build_args| rocBuild(&ctx, build_args, args[0]) catch |err| switch (err) {
            error.CliError => {
                // Problems already recorded in context, render them below
            },
            else => return err,
        },
        .bundle => |bundle_args| rocBundle(&ctx, bundle_args),
        .unbundle => |unbundle_args| rocUnbundle(&ctx, unbundle_args),
        .fmt => |format_args| rocFormat(&ctx, format_args),
        .test_cmd => |test_args| try rocTest(&ctx, test_args, args[0]),
        .repl => |repl_args| rocRepl(&ctx, repl_args),
        .glue => |glue_args| rocGlue(&ctx, glue_args) catch |err| switch (err) {
            error.CliError => {
                // Problems already recorded in context, render them below
            },
            else => return err,
        },
        .version => ctx.io.stdout().print("Roc compiler version {s}\n", .{build_options.compiler_version}),
        .docs => |docs_args| rocDocs(&ctx, docs_args),
        .bump => |bump_args| rocBump(&ctx, bump_args) catch |err| switch (err) {
            error.CliError => {
                // Problems already recorded in context, render them below
            },
            else => return err,
        },
        .install => |install_args| rocInstall(&ctx, install_args, args[0]) catch |err| switch (err) {
            error.CliError => {
                // Problems already recorded in context, render them below
            },
            else => return err,
        },
        .experimental_lsp => |lsp_args| try lsp.runWithStdIo(gpa, std_io, .{
            .transport = lsp_args.debug_io,
            .build = lsp_args.debug_build,
            .syntax = lsp_args.debug_syntax,
            .server = lsp_args.debug_server,
        }),
        .help => |help_message| {
            try ctx.io.stdout().writeAll(help_message);
        },
        .licenses => {
            try ctx.io.stdout().writeAll(legalDetailsFileContent);
        },
        .problem => |problem| {
            try switch (problem) {
                .missing_flag_value => |details| ctx.io.stderr().print("Error: no value was supplied for {s}\n", .{details.flag}),
                .unexpected_argument => |details| ctx.io.stderr().print("Error: roc {s} received an unexpected argument: `{s}`\n", .{ details.cmd, details.arg }),
                .invalid_flag_value => |details| ctx.io.stderr().print("Error: `{s}` is not a valid value for {s}. The valid options are {s}\n", .{ details.value, details.flag, details.valid_options }),
                .shorthand_requires_run => |details| ctx.io.stderr().print(
                    "Error: `{s}` looks like an installed shorthand, and running one requires the `run` subcommand: `roc run {s}`\nIf you meant a local file named `{s}`, write it as `./{s}` instead.\n",
                    .{ details.name, details.name, details.name, details.name },
                ),
            };
            return error.InvalidArguments;
        },
    };

    // Render any problems accumulated during command execution
    if (ctx.hasProblems()) {
        try ctx.renderProblemsTo(ctx.io.stderr());
        if (ctx.hasErrors()) {
            return error.CliError;
        }
    }
}

fn buildShimEntrypoints(
    ctx: *CliCtx,
    store: *const lir.LirStore,
    platform_entrypoints: []const lir.LirImage.PlatformEntrypoint,
    entrypoint_names: []const []const u8,
) (Allocator.Error || CliError)![]llvm_codegen.MonoLlvmCodeGen.ShimEntrypoint {
    if (platform_entrypoints.len != entrypoint_names.len) {
        return ctx.fail(.{ .shim_generation_failed = .{ .err = error.InvalidLirImage } });
    }

    var shim_entrypoints = try ctx.arena.alloc(llvm_codegen.MonoLlvmCodeGen.ShimEntrypoint, platform_entrypoints.len);
    for (platform_entrypoints) |entrypoint| {
        const ordinal: usize = @intCast(entrypoint.ordinal);
        if (ordinal >= entrypoint_names.len) {
            return ctx.fail(.{ .shim_generation_failed = .{ .err = error.InvalidLirImage } });
        }

        const spec = store.getProcSpec(entrypoint.root_proc);
        const arg_locals = store.getLocalSpan(spec.args);
        const arg_layouts = try ctx.arena.alloc(layout.Idx, arg_locals.len);
        for (0..arg_locals.len) |i| {
            const local_id = GuardedList.at(arg_locals, i);
            arg_layouts[i] = store.getLocal(local_id).layout_idx;
        }
        shim_entrypoints[ordinal] = .{
            .symbol_name = entrypoint_names[ordinal],
            .entry_index = entrypoint.ordinal,
            .arg_layouts = arg_layouts,
            .ret_layout = spec.ret_layout,
        };
    }

    return shim_entrypoints;
}

/// Generate platform host shim object file using LLVM from already-lowered LIR data.
/// Returns the path to the generated object file (allocated from arena, no need to free), or null if LLVM unavailable.
/// If `embedded_lir_image` is present, embed the already-lowered LIR image
/// and call the interpreter shim entrypoint that views the image directly.
/// If debug is true, include debug information in the generated object file.
fn generatePlatformHostShimFromLirData(
    ctx: *CliCtx,
    cache_dir: []const u8,
    entrypoint_names: []const []const u8,
    checked_hosted_symbols: ?[]const []const u8,
    target: RocTarget,
    store: *const lir.LirStore,
    layouts: *const layout.Store,
    platform_entrypoints: []const lir.LirImage.PlatformEntrypoint,
    embedded_lir_image: ?[]const u8,
    image_cache_len: usize,
    default_run_start: bool,
    debug: bool,
) (Allocator.Error || error{ CliError, LLVMCompilationFailed })!?[]const u8 {
    // Check if LLVM is available (this is a compile-time check)
    if (!llvm_available) {
        std.log.debug("LLVM not available, skipping platform host shim generation", .{});
        return null;
    }

    // Create std.Target for the target RocTarget.
    const std_target = stdTargetForLlvmBuild(ctx, target) catch |err| {
        return ctx.fail(.{ .shim_generation_failed = .{ .err = err } });
    };
    const llvm_cpu = llvmCpuNameForTarget(std_target);
    const llvm_features = try llvmFeatureStringForTarget(ctx.arena, std_target);

    const shim_entrypoints = try buildShimEntrypoints(
        ctx,
        store,
        platform_entrypoints,
        entrypoint_names,
    );

    // Hosted dispatch table symbols, ordered by dispatch index. Multiple proc
    // specs may share a dispatch index (specializations of the same hosted
    // function); they all carry the same symbol.
    const hosted_symbols = checked_hosted_symbols orelse try hostedSymbolsFromLirDispatch(ctx.arena, store);

    var codegen = llvm_codegen.MonoLlvmCodeGen.initForLinkedObject(ctx.gpa, store, std_target);
    codegen.layout_store = layouts;
    defer codegen.deinit();

    var bitcode_result = codegen.generateInterpreterShimModule(
        "roc_platform_shim",
        shim_entrypoints,
        hosted_symbols,
        embedded_lir_image,
        default_run_start,
    ) catch |err| {
        return ctx.fail(.{ .shim_generation_failed = .{ .err = err } });
    };
    defer bitcode_result.deinit();

    // Name the scratch artifacts by the shim's deterministic inputs. The raw
    // image bytes contain uninitialized struct padding from serialization, so
    // hash the derived entrypoint ABI, the hosted table, and the image length
    // instead of the bytes themselves.
    var hash = std.hash.Crc32.init();
    const abi_digest = try entrypointAbiDigestFromLirData(ctx, store, layouts, platform_entrypoints, target);
    hash.update(&abi_digest);
    for (hosted_symbols) |symbol| {
        hash.update(symbol);
        hash.update(&[_]u8{0});
    }
    var image_len_bytes: [8]u8 = undefined;
    std.mem.writeInt(u64, &image_len_bytes, @intCast(image_cache_len), .little);
    hash.update(&image_len_bytes);
    hash.update(if (embedded_lir_image != null) "embed" else "dispatch");
    hash.update(if (default_run_start) "default-run-start" else "host-provided-start");
    for (entrypoint_names) |name| {
        hash.update(name);
        hash.update(&[_]u8{0});
    }
    hash.update(target.toTriple());
    hash.update(if (debug) "debug" else "nodebug");
    hash.update(llvm_cpu);
    hash.update(&[_]u8{0});
    hash.update(llvm_features);
    const content_hash = hash.final();

    const bitcode_filename = std.fmt.allocPrint(ctx.arena, "platform_shim_{x}.bc", .{content_hash}) catch |err| {
        return ctx.fail(.{ .shim_generation_failed = .{ .err = err } });
    };
    const object_filename = std.fmt.allocPrint(ctx.arena, "platform_shim_{x}.o", .{content_hash}) catch |err| {
        return ctx.fail(.{ .shim_generation_failed = .{ .err = err } });
    };

    const bitcode_path = std.fs.path.join(ctx.arena, &.{ cache_dir, bitcode_filename }) catch |err| {
        return ctx.fail(.{ .shim_generation_failed = .{ .err = err } });
    };

    const object_path = std.fs.path.join(ctx.arena, &.{ cache_dir, object_filename }) catch |err| {
        return ctx.fail(.{ .shim_generation_failed = .{ .err = err } });
    };

    // Write bitcode to file
    const bc_file = std.Io.Dir.cwd().createFile(ctx.io.std_io, bitcode_path, .{}) catch |err| {
        return ctx.fail(.{ .file_write_failed = .{ .path = bitcode_path, .err = err } });
    };
    defer bc_file.close(ctx.io.std_io);

    const bytes = std.mem.sliceAsBytes(bitcode_result.bitcode);
    bc_file.writeStreamingAll(ctx.io.std_io, bytes) catch |err| {
        return ctx.fail(.{ .file_write_failed = .{ .path = bitcode_path, .err = err } });
    };

    const compile_config = builder.CompileConfig{
        .input_path = bitcode_path,
        .output_path = object_path,
        .optimization = .speed,
        .target = target,
        .cpu = llvm_cpu,
        .features = llvm_features,
        .debug = debug, // Use the debug flag passed from caller
        .no_target_libcalls = noTargetLibcallsForLlvmBuild(target),
    };

    if (builder.compileBitcodeToObject(ctx.gpa, ctx.io.std_io, compile_config)) |success| {
        if (!success) {
            std.log.warn("LLVM compilation not ready, falling back to clang", .{});
            return error.LLVMCompilationFailed;
        }
    } else |err| {
        std.log.warn("Failed to compile with embedded LLVM: {}, falling back to clang", .{err});
        return error.LLVMCompilationFailed;
    }

    std.log.debug("Generated platform host shim: {s}", .{object_path});

    return object_path;
}

/// Generate platform host shim object file using LLVM.
/// Returns the path to the generated object file (allocated from arena, no need to free), or null if LLVM unavailable.
/// If `lir_image` is present, embed the already-lowered LIR image
/// and call the interpreter shim entrypoint that views the image directly.
/// If debug is true, include debug information in the generated object file.
fn generatePlatformHostShim(
    ctx: *CliCtx,
    cache_dir: []const u8,
    entrypoint_names: []const []const u8,
    checked_hosted_symbols: ?[]const []const u8,
    target: RocTarget,
    lir_image: []const u8,
    embed_image: bool,
    default_run_start: bool,
    debug: bool,
) (Allocator.Error || error{ CliError, LLVMCompilationFailed })!?[]const u8 {
    // Check if LLVM is available before viewing the image.
    if (!llvm_available) {
        std.log.debug("LLVM not available, skipping platform host shim generation", .{});
        return null;
    }

    // View the LIR image to derive the entrypoint ABI, the hosted dispatch
    // table, and the layout store the C-ABI lowering needs.
    if (lir_image.len < @sizeOf(SharedMemoryAllocator.Header) + @sizeOf(lir.LirImage.Header)) {
        return ctx.fail(.{ .shim_generation_failed = .{ .err = error.InvalidLirImage } });
    }
    const image_header: *const lir.LirImage.Header = @ptrCast(@alignCast(lir_image.ptr + @sizeOf(SharedMemoryAllocator.Header)));
    // The host shim's C-ABI lowering needs layout sizes for the target being
    // built, so resolve the width-independent image for that pointer width.
    const shim_target_usize = base.target.TargetUsize.fromPtrBitWidth(target.ptrBitWidth());
    const view = lir.LirImage.viewMappedImageWithAllocator(image_header, lir_image.ptr, lir_image.len, shim_target_usize, ctx.arena) catch |err| {
        return ctx.fail(.{ .shim_generation_failed = .{ .err = err } });
    };

    return generatePlatformHostShimFromLirData(
        ctx,
        cache_dir,
        entrypoint_names,
        checked_hosted_symbols,
        target,
        &view.store,
        &view.layouts,
        view.platform_entrypoints,
        if (embed_image) lir_image else null,
        lir_image.len,
        default_run_start,
        debug,
    );
}

fn ensureCompilerCacheDirExists(std_io: std.Io, path: []const u8) std.Io.Dir.CreateDirPathError!void {
    // This helper is only for compiler-owned internal cache directories.
    // User-facing output paths should still fail normally if the parent directory is missing.
    std.Io.Dir.cwd().createDirPath(std_io, path) catch |err| switch (err) {
        error.PathAlreadyExists => {},
        else => return err,
    };
}

fn updateHashU32(hasher: *std.crypto.hash.sha2.Sha256, value: u32) void {
    var buf: [4]u8 = undefined;
    std.mem.writeInt(u32, &buf, value, .little);
    hasher.update(&buf);
}

fn updateHashBool(hasher: *std.crypto.hash.sha2.Sha256, value: bool) void {
    hasher.update(if (value) "\x01" else "\x00");
}

fn updateHashBytes(hasher: *std.crypto.hash.sha2.Sha256, bytes: []const u8) void {
    var len_buf: [8]u8 = undefined;
    std.mem.writeInt(u64, &len_buf, @intCast(bytes.len), .little);
    hasher.update(&len_buf);
    hasher.update(bytes);
}

fn bytesDigest(bytes: []const u8) [32]u8 {
    var hasher = std.crypto.hash.sha2.Sha256.init(.{});
    hasher.update(bytes);
    return hasher.finalResult();
}

fn fileContentsDigest(ctx: *CliCtx, path: []const u8) CliError![32]u8 {
    const file = std.Io.Dir.cwd().openFile(ctx.io.std_io, path, .{}) catch |err| {
        return ctx.fail(.{ .file_read_failed = .{
            .path = path,
            .err = err,
        } });
    };
    defer file.close(ctx.io.std_io);

    var hasher = std.crypto.hash.sha2.Sha256.init(.{});
    var read_buf: [64 * 1024]u8 = undefined;
    while (true) {
        const bytes_read = file.readStreaming(ctx.io.std_io, &.{&read_buf}) catch |err| switch (err) {
            error.EndOfStream => break,
            else => {
                return ctx.fail(.{ .file_read_failed = .{
                    .path = path,
                    .err = err,
                } });
            },
        };
        if (bytes_read == 0) break;
        hasher.update(read_buf[0..bytes_read]);
    }
    return hasher.finalResult();
}

fn platformHostShimIdentity(target: RocTarget, entrypoint_names: []const []const u8, debug: bool) [32]u8 {
    var hasher = std.crypto.hash.sha2.Sha256.init(.{});
    updateHashBytes(&hasher, "roc-platform-host-shim-v1");
    updateHashBytes(&hasher, target.toTriple());
    updateHashBool(&hasher, debug);
    updateHashU32(&hasher, @intCast(entrypoint_names.len));
    for (entrypoint_names) |name| {
        updateHashBytes(&hasher, name);
    }
    return hasher.finalResult();
}

const HostedCacheEntry = struct {
    module_key: [32]u8,
    order_key: []const u8,
    external_symbol_name: []const u8,
    def_idx: u32,
    deterministic_index: u32,
    dispatch_index: u32 = 0,
};

const CheckedHostedTable = struct {
    entries: []const HostedCacheEntry,
    symbols: []const []const u8,
};

fn checkedModuleKeySeen(seen_keys: []const [32]u8, key: [32]u8) bool {
    for (seen_keys) |seen_key| {
        if (std.mem.eql(u8, &seen_key, &key)) return true;
    }
    return false;
}

fn appendHostedCacheEntriesFromView(
    allocator: Allocator,
    entries: *std.ArrayList(HostedCacheEntry),
    seen_keys: *std.ArrayList([32]u8),
    view: check.CheckedArtifact.ImportedModuleView,
) Allocator.Error!void {
    if (checkedModuleKeySeen(seen_keys.items, view.key.bytes)) return;
    try seen_keys.append(allocator, view.key.bytes);

    for (view.hosted_procs.procs) |proc| {
        try entries.append(allocator, .{
            .module_key = view.key.bytes,
            .order_key = proc.orderKey(view.hosted_procs),
            .external_symbol_name = view.canonical_names.externalSymbolNameText(proc.external_symbol_name),
            .def_idx = @intFromEnum(proc.def_idx),
            .deterministic_index = proc.deterministic_index,
        });
    }
}

const HostedSectionMap = struct {
    keys: []const []const u8,
    symbols: []const []const u8,
};

fn hostedSectionMapFromEnv(allocator: Allocator, env: *const ModuleEnv) Allocator.Error!HostedSectionMap {
    const section = env.hosted_entries.items.items;
    const keys = try allocator.alloc([]const u8, section.len);
    errdefer {
        for (keys) |key| allocator.free(key);
        allocator.free(keys);
    }
    const symbols = try allocator.alloc([]const u8, section.len);
    errdefer allocator.free(symbols);

    for (section, 0..) |entry, index| {
        var func_text = env.getIdentText(entry.func_ident);
        if (func_text.len > 0 and func_text[func_text.len - 1] == '!') {
            func_text = func_text[0 .. func_text.len - 1];
        }
        keys[index] = if (entry.module_ident) |module_ident|
            try std.fmt.allocPrint(allocator, "{s}.{s}", .{ env.getIdentText(module_ident), func_text })
        else
            try allocator.dupe(u8, func_text);
        symbols[index] = env.getString(entry.symbol);
    }

    return .{ .keys = keys, .symbols = symbols };
}

fn deinitHostedSectionMap(allocator: Allocator, map: HostedSectionMap) void {
    for (map.keys) |key| allocator.free(key);
    allocator.free(map.keys);
    allocator.free(map.symbols);
}

fn findHostedSectionEnv(
    root_artifact: *const check.CheckedArtifact.CheckedModuleArtifact,
    imported_artifacts: []const check.CheckedArtifact.ImportedModuleView,
    relation_artifacts: []const check.CheckedArtifact.ImportedModuleView,
) ?*const ModuleEnv {
    const root_env = root_artifact.moduleEnvConst();
    if (root_env.hosted_entries.items.items.len != 0) return root_env;
    for (imported_artifacts) |view| {
        if (view.module_env.hosted_entries.items.items.len != 0) return view.module_env;
    }
    for (relation_artifacts) |view| {
        if (view.module_env.hosted_entries.items.items.len != 0) return view.module_env;
    }
    return null;
}

fn applyHostedSectionMap(entries: []HostedCacheEntry, map: HostedSectionMap) void {
    if (entries.len != map.keys.len) {
        if (builtin.mode == .Debug) {
            std.debug.panic("default roc command invariant violated: hosted section size {d} differs from checked hosted catalog size {d}", .{ map.keys.len, entries.len });
        }
        unreachable;
    }

    for (entries) |*entry| {
        const dispatch_index = blk: {
            for (map.keys, 0..) |key, index| {
                if (std.mem.eql(u8, key, entry.order_key)) break :blk index;
            }
            if (builtin.mode == .Debug) {
                std.debug.panic("default roc command invariant violated: hosted function '{s}' is missing from the platform hosted section", .{entry.order_key});
            }
            unreachable;
        };
        entry.dispatch_index = @intCast(dispatch_index);
        entry.external_symbol_name = map.symbols[dispatch_index];
    }

    const DispatchSort = struct {
        pub fn lessThan(_: void, a: HostedCacheEntry, b: HostedCacheEntry) bool {
            return a.dispatch_index < b.dispatch_index;
        }
    };
    std.mem.sort(HostedCacheEntry, entries, {}, DispatchSort.lessThan);
}

fn checkedHostedTable(
    allocator: Allocator,
    root_artifact: *const check.CheckedArtifact.CheckedModuleArtifact,
    imported_artifacts: []const check.CheckedArtifact.ImportedModuleView,
    relation_artifacts: []const check.CheckedArtifact.ImportedModuleView,
) Allocator.Error!CheckedHostedTable {
    var hosted_entries = std.ArrayList(HostedCacheEntry).empty;
    defer hosted_entries.deinit(allocator);
    var seen_keys = std.ArrayList([32]u8).empty;
    defer seen_keys.deinit(allocator);

    try appendHostedCacheEntriesFromView(
        allocator,
        &hosted_entries,
        &seen_keys,
        check.CheckedArtifact.importedView(root_artifact),
    );
    for (imported_artifacts) |view| {
        try appendHostedCacheEntriesFromView(allocator, &hosted_entries, &seen_keys, view);
    }
    for (relation_artifacts) |view| {
        try appendHostedCacheEntriesFromView(allocator, &hosted_entries, &seen_keys, view);
    }

    const SortContext = struct {
        pub fn lessThan(_: void, a: HostedCacheEntry, b: HostedCacheEntry) bool {
            return switch (std.mem.order(u8, a.order_key, b.order_key)) {
                .lt => true,
                .gt => false,
                .eq => if (a.def_idx != b.def_idx)
                    a.def_idx < b.def_idx
                else
                    std.mem.order(u8, &a.module_key, &b.module_key) == .lt,
            };
        }
    };
    std.mem.sort(HostedCacheEntry, hosted_entries.items, {}, SortContext.lessThan);

    for (hosted_entries.items, 0..) |*entry, index| {
        entry.dispatch_index = @intCast(index);
    }

    if (findHostedSectionEnv(root_artifact, imported_artifacts, relation_artifacts)) |env| {
        const map = try hostedSectionMapFromEnv(allocator, env);
        defer deinitHostedSectionMap(allocator, map);
        applyHostedSectionMap(hosted_entries.items, map);
    }

    const entries = try hosted_entries.toOwnedSlice(allocator);
    errdefer allocator.free(entries);

    const symbols = try allocator.alloc([]const u8, entries.len);
    errdefer allocator.free(symbols);
    var stable_symbol_count: usize = 0;
    errdefer for (symbols[0..stable_symbol_count]) |symbol| allocator.free(symbol);
    for (entries, symbols) |*entry, *symbol| {
        const stable_symbol = try allocator.dupe(u8, entry.external_symbol_name);
        entry.external_symbol_name = stable_symbol;
        symbol.* = stable_symbol;
        stable_symbol_count += 1;
    }

    return .{
        .entries = entries,
        .symbols = symbols,
    };
}

const LayoutHashContext = struct {
    layouts: *const layout.Store,
    seen: std.AutoHashMap(layout.Idx, u32),
    next_seen: u32 = 0,

    fn init(allocator: Allocator, layouts: *const layout.Store) LayoutHashContext {
        return .{
            .layouts = layouts,
            .seen = std.AutoHashMap(layout.Idx, u32).init(allocator),
        };
    }

    fn deinit(self: *LayoutHashContext) void {
        self.seen.deinit();
    }

    fn hashIdx(
        self: *LayoutHashContext,
        hasher: *std.crypto.hash.sha2.Sha256,
        idx: layout.Idx,
    ) Allocator.Error!void {
        if (idx == layout.Idx.none) {
            updateHashBytes(hasher, "layout-none");
            return;
        }

        if (self.seen.get(idx)) |seen_index| {
            updateHashBytes(hasher, "layout-ref");
            updateHashU32(hasher, seen_index);
            return;
        }

        const seen_index = self.next_seen;
        self.next_seen += 1;
        try self.seen.put(idx, seen_index);

        const layout_val = self.layouts.getLayout(idx);
        const size_align = self.layouts.layoutSizeAlign(layout_val);
        updateHashBytes(hasher, "layout-node");
        updateHashU32(hasher, seen_index);
        updateHashU32(hasher, @intCast(@intFromEnum(layout_val.tag)));
        updateHashU32(hasher, @intCast(size_align.size));
        updateHashU32(hasher, @intCast(size_align.alignment.toByteUnits()));
        updateHashBool(hasher, self.layouts.layoutContainsRefcounted(layout_val));

        switch (layout_val.tag) {
            .scalar => {
                const scalar = layout_val.getScalar();
                updateHashU32(hasher, @intCast(@intFromEnum(scalar.tag)));
                switch (scalar.tag) {
                    .int => updateHashU32(hasher, @intCast(@intFromEnum(scalar.getInt()))),
                    .frac => updateHashU32(hasher, @intCast(@intFromEnum(scalar.getFrac()))),
                    .str, .opaque_ptr => {},
                }
            },
            .box, .list, .ptr => try self.hashIdx(hasher, layout_val.getIdx()),
            .box_of_zst, .list_of_zst, .erased_callable, .zst => {},
            .closure => try self.hashIdx(hasher, layout_val.getClosure().captures_layout_idx),
            .struct_ => {
                const info = self.layouts.getStructInfo(layout_val);
                updateHashU32(hasher, @intCast(info.alignment.toByteUnits()));
                updateHashU32(hasher, info.size());
                updateHashU32(hasher, @intCast(info.fields.len));
                for (0..info.fields.len) |i| {
                    const field = info.fields.get(i);
                    updateHashU32(hasher, @intCast(field.index));
                    updateHashBool(hasher, field.is_padding);
                    try self.hashIdx(hasher, field.layout);
                }
            },
            .tag_union => {
                const info = self.layouts.getTagUnionInfo(layout_val);
                updateHashU32(hasher, @intCast(info.alignment.toByteUnits()));
                updateHashU32(hasher, info.size());
                updateHashU32(hasher, @intCast(info.discriminant_offset));
                updateHashU32(hasher, @intCast(info.data.discriminant_size));
                updateHashU32(hasher, @intCast(info.variants.len));
                for (0..info.variants.len) |i| {
                    const variant = info.variants.get(i);
                    try self.hashIdx(hasher, variant.payload_layout);
                }
            },
        }
    }
};

fn updateLayoutFingerprint(
    allocator: Allocator,
    hasher: *std.crypto.hash.sha2.Sha256,
    layouts: *const layout.Store,
    layout_idx: layout.Idx,
) Allocator.Error!void {
    var ctx = LayoutHashContext.init(allocator, layouts);
    defer ctx.deinit();
    try ctx.hashIdx(hasher, layout_idx);
}

fn updatePlatformAppRelationIdentity(
    hasher: *std.crypto.hash.sha2.Sha256,
    root_artifact: *const check.CheckedArtifact.CheckedModuleArtifact,
) void {
    // Host-boundary fingerprint: hash the relation/binding SHAPE only, never
    // checked type keys. Type keys are deep content digests (they embed the
    // app module's content identity), so hashing them would change the host
    // interface fingerprint on every app source edit and break hot reload of
    // unchanged interfaces. Interface compatibility itself is enforced by the
    // rebuild's type check, and ABI stability by the host callable layout
    // identity hashed alongside this.
    updateHashBytes(hasher, "platform-app-relations-v2");

    const relations = root_artifact.platform_requirement_relations.relations;
    updateHashU32(hasher, @intCast(relations.len));
    for (relations) |relation| {
        updateHashU32(hasher, @intFromEnum(relation.declaration));
        updateHashU32(hasher, relation.requires_idx);
        updateHashU32(hasher, @intFromEnum(relation.value_kind));
    }

    const bindings = root_artifact.platform_required_bindings.bindings;
    updateHashU32(hasher, @intCast(bindings.len));
    for (bindings) |binding| {
        updateHashU32(hasher, @intFromEnum(binding.declaration));
        updateHashU32(hasher, binding.requires_idx);
        updateHashU32(hasher, @intFromEnum(binding.checked_relation));
        updateHashU32(hasher, @intFromEnum(std.meta.activeTag(binding.value_use)));
    }
}

fn updateHostCallableLayoutIdentity(
    allocator: Allocator,
    hasher: *std.crypto.hash.sha2.Sha256,
    store: *const lir.LirStore,
    layouts: *const layout.Store,
    platform_entrypoints: []const lir.LirImage.PlatformEntrypoint,
) Allocator.Error!void {
    updateHashBytes(hasher, "host-callable-layouts-v1");
    updateHashU32(hasher, @intCast(platform_entrypoints.len));
    for (platform_entrypoints) |entrypoint| {
        updateHashU32(hasher, entrypoint.ordinal);
        const proc = store.getProcSpec(entrypoint.root_proc);
        const arg_layouts = try argLayoutsForProc(allocator, store, entrypoint.root_proc);
        defer allocator.free(arg_layouts);
        updateHashU32(hasher, @intCast(arg_layouts.len));
        for (arg_layouts) |arg_layout| {
            try updateLayoutFingerprint(allocator, hasher, layouts, arg_layout);
        }
        try updateLayoutFingerprint(allocator, hasher, layouts, proc.ret_layout);
    }
}

fn checkedInterpreterHostIdentity(
    allocator: Allocator,
    root_artifact: *const check.CheckedArtifact.CheckedModuleArtifact,
    store: *const lir.LirStore,
    layouts: *const layout.Store,
    platform_entrypoints: []const lir.LirImage.PlatformEntrypoint,
    entrypoint_names: []const []const u8,
    target_usize: base.target.TargetUsize,
    hosted_table: CheckedHostedTable,
) Allocator.Error![32]u8 {
    var hasher = std.crypto.hash.sha2.Sha256.init(.{});
    updateHashBytes(&hasher, "roc-run-checked-host-interface-v2");
    updateHashU32(&hasher, @intFromEnum(target_usize));

    const declarations_hash = root_artifact.platform_required_declarations.identityHash(&root_artifact.canonical_names);
    hasher.update(&declarations_hash);
    updatePlatformAppRelationIdentity(&hasher, root_artifact);
    try updateHostCallableLayoutIdentity(allocator, &hasher, store, layouts, platform_entrypoints);

    updateHashU32(&hasher, @intCast(entrypoint_names.len));
    for (entrypoint_names) |name| {
        updateHashBytes(&hasher, name);
    }

    updateHashU32(&hasher, @intCast(hosted_table.entries.len));
    for (hosted_table.entries) |entry| {
        updateHashU32(&hasher, entry.dispatch_index);
        hasher.update(&entry.module_key);
        updateHashBytes(&hasher, entry.order_key);
        updateHashBytes(&hasher, entry.external_symbol_name);
        updateHashU32(&hasher, entry.def_idx);
        updateHashU32(&hasher, entry.deterministic_index);
    }

    return hasher.finalResult();
}

fn updateInterpreterExeFileLinkInput(
    hasher: *std.crypto.hash.sha2.Sha256,
    declared_path: []const u8,
    resolved_path: []const u8,
    content_digest: [32]u8,
) void {
    updateHashBytes(hasher, "file");
    updateHashBytes(hasher, declared_path);
    updateHashBytes(hasher, resolved_path);
    hasher.update(&content_digest);
}

fn updateInterpreterExeAppLinkInput(
    hasher: *std.crypto.hash.sha2.Sha256,
    shim_kind: ShimLibraryKind,
    target: RocTarget,
    entrypoint_names: []const []const u8,
    debug: bool,
) void {
    updateHashBytes(hasher, "app");
    const shim_digest = shimLibraryDigest(shim_kind, target);
    hasher.update(&shim_digest);
    updateHashBool(hasher, llvm_available);
    if (llvm_available) {
        const platform_shim_identity = platformHostShimIdentity(target, entrypoint_names, debug);
        hasher.update(&platform_shim_identity);
    }
}

/// Digest of the entrypoint C ABI and hosted dispatch table baked into the
/// generated interpreter shim. Part of the interpreter executable cache key:
/// the cached exe's marshalling code must match the program's entrypoint ABI.
fn entrypointAbiDigestFromLirData(
    ctx: *CliCtx,
    store: *const lir.LirStore,
    layouts: *const layout.Store,
    platform_entrypoints: []const lir.LirImage.PlatformEntrypoint,
    target: RocTarget,
) (Allocator.Error || CliError)![32]u8 {
    const abi_target: layout.abi.Target = switch (target.toCpuArch()) {
        .aarch64 => .aarch64,
        .x86_64 => if (target.toOsTag() == .windows) .x86_64_windows else .x86_64_sysv,
        .wasm32 => .wasm32,
        else => return ctx.fail(.{ .shim_generation_failed = .{ .err = error.UnsupportedTarget } }),
    };

    var hasher = std.crypto.hash.sha2.Sha256.init(.{});
    updateHashBytes(&hasher, "roc-entrypoint-abi-v1");

    const hashPlacement = struct {
        fn go(h: *std.crypto.hash.sha2.Sha256, placement: layout.abi.Placement) void {
            switch (placement) {
                .none => updateHashU32(h, 0),
                .indirect => updateHashU32(h, 1),
                .registers => |pieces| {
                    updateHashU32(h, 2);
                    updateHashU32(h, @intCast(pieces.len));
                    for (pieces) |piece| {
                        updateHashU32(h, @intFromEnum(piece.class));
                        updateHashU32(h, piece.offset);
                        updateHashU32(h, piece.size);
                    }
                },
            }
        }
    }.go;

    updateHashU32(&hasher, @intCast(platform_entrypoints.len));
    for (platform_entrypoints) |entrypoint| {
        const spec = store.getProcSpec(entrypoint.root_proc);
        const arg_locals = store.getLocalSpan(spec.args);
        const arg_layouts = try ctx.arena.alloc(layout.Idx, arg_locals.len);
        for (0..arg_locals.len) |i| {
            const local_id = GuardedList.at(arg_locals, i);
            arg_layouts[i] = store.getLocal(local_id).layout_idx;
        }
        const lowered = layout.abi.lower(ctx.arena, layouts, abi_target, arg_layouts, spec.ret_layout, false) catch return error.OutOfMemory;
        updateHashU32(&hasher, entrypoint.ordinal);
        hashPlacement(&hasher, lowered.ret);
        updateHashU32(&hasher, @intCast(lowered.args.len));
        for (lowered.args) |arg_placement| {
            hashPlacement(&hasher, arg_placement);
        }
    }

    for (store.getProcSpecs()) |spec| {
        const hosted = spec.hosted orelse continue;
        updateHashU32(&hasher, hosted.dispatch_index);
        updateHashBytes(&hasher, store.getString(hosted.symbol));
    }

    return hasher.finalResult();
}

fn interpreterExeLinkInputsIdentity(
    ctx: *CliCtx,
    shim_kind: ShimLibraryKind,
    link_spec: roc_target.TargetLinkSpec,
    platform_dir: []const u8,
    files_dir: []const u8,
    target: RocTarget,
    entrypoint_names: []const []const u8,
    debug: bool,
) (Allocator.Error || CliError)![32]u8 {
    var hasher = std.crypto.hash.sha2.Sha256.init(.{});
    updateHashBytes(&hasher, "roc-run-link-inputs-v3");
    updateHashBytes(&hasher, @tagName(target));

    const target_name = @tagName(target);
    for (link_spec.items) |item| {
        switch (item) {
            .file_path => |file_name| {
                const full_path = try std.fs.path.join(ctx.arena, &.{ platform_dir, files_dir, target_name, file_name });
                const content_digest = try fileContentsDigest(ctx, full_path);
                updateInterpreterExeFileLinkInput(&hasher, file_name, full_path, content_digest);
            },
            .app => updateInterpreterExeAppLinkInput(&hasher, shim_kind, target, entrypoint_names, debug),
            .win_gui => updateHashBytes(&hasher, "win_gui"),
        }
    }

    return hasher.finalResult();
}

fn defaultRunCheckedHostIdentity(
    target: RocTarget,
    entrypoint_names: []const []const u8,
    hosted_symbols: []const []const u8,
) [32]u8 {
    var hasher = std.crypto.hash.sha2.Sha256.init(.{});
    updateHashBytes(&hasher, "roc-run-default-checked-host-v1");
    updateHashBytes(&hasher, @tagName(target));
    updateHashBytes(&hasher, echo_platform.run_shim_platform_main_source);
    updateHashBytes(&hasher, echo_platform.echo_module_source);

    updateHashU32(&hasher, @intCast(entrypoint_names.len));
    for (entrypoint_names) |name| {
        updateHashBytes(&hasher, name);
    }

    updateHashU32(&hasher, @intCast(hosted_symbols.len));
    for (hosted_symbols) |symbol| {
        updateHashBytes(&hasher, symbol);
    }

    return hasher.finalResult();
}

fn defaultRunLinkInputsIdentity(
    ctx: *CliCtx,
    target: RocTarget,
    libc_info: ?libc_finder.LibcInfo,
) CliError!?[32]u8 {
    const runtime_bytes = DefaultPlatformRuntimeObjects.forTarget(target) orelse return null;

    var hasher = std.crypto.hash.sha2.Sha256.init(.{});
    updateHashBytes(&hasher, "roc-run-default-link-inputs-v2");
    updateHashBytes(&hasher, @tagName(target));
    hasher.update(&bytesDigest(runtime_bytes));

    if (libc_info) |info| {
        updateHashBytes(&hasher, "libc");
        updateHashBytes(&hasher, info.arch);
        updateHashBytes(&hasher, info.dynamic_linker);
        const dynamic_linker_digest = try fileContentsDigest(ctx, info.dynamic_linker);
        hasher.update(&dynamic_linker_digest);
        updateHashBytes(&hasher, info.lib_dir);
        updateHashBytes(&hasher, info.libc_path);
        const libc_digest = try fileContentsDigest(ctx, info.libc_path);
        hasher.update(&libc_digest);
    } else {
        updateHashBytes(&hasher, "no-libc");
    }

    return hasher.finalResult();
}

const ShimHostExeCacheInputs = struct {
    shim_kind: ShimLibraryKind,
    target: RocTarget,
    debug: bool,
    checked_host_identity: [32]u8,
    link_inputs_identity: [32]u8,
};

fn shimHostExeCacheDigest(inputs: ShimHostExeCacheInputs) [32]u8 {
    var hasher = std.crypto.hash.sha2.Sha256.init(.{});
    updateHashBytes(&hasher, "roc-run-shim-host-cache-v3");
    updateHashBytes(&hasher, build_options.compiler_version);
    updateHashBytes(&hasher, @tagName(inputs.shim_kind));
    updateHashBytes(&hasher, @tagName(inputs.target));
    updateHashBool(&hasher, inputs.debug);
    const shim_digest = shimLibraryDigest(inputs.shim_kind, inputs.target);
    hasher.update(&shim_digest);
    hasher.update(&inputs.checked_host_identity);
    hasher.update(&inputs.link_inputs_identity);
    return hasher.finalResult();
}

fn shimHostExeCacheName(
    ctx: *CliCtx,
    inputs: ShimHostExeCacheInputs,
) (Allocator.Error || error{CliError})![]const u8 {
    const digest = shimHostExeCacheDigest(inputs);
    const digest_hex = std.fmt.bytesToHex(digest, .lower);
    return std.fmt.allocPrint(ctx.arena, "roc_{s}", .{digest_hex[0..]}) catch |err| {
        return ctx.fail(.{ .cache_dir_unavailable = .{ .reason = @errorName(err) } });
    };
}

fn testDigest(byte: u8) [32]u8 {
    return [_]u8{byte} ** 32;
}

fn testCacheDigest(checked_host_identity: [32]u8, link_inputs_identity: [32]u8) [32]u8 {
    return shimHostExeCacheDigest(.{
        .shim_kind = .lir,
        .target = .x64linux,
        .debug = false,
        .checked_host_identity = checked_host_identity,
        .link_inputs_identity = link_inputs_identity,
    });
}

fn testLinkInputsIdentityForFiles(
    first_declared: []const u8,
    first_resolved: []const u8,
    first_contents: []const u8,
    second_declared: []const u8,
    second_resolved: []const u8,
    second_contents: []const u8,
) [32]u8 {
    var hasher = std.crypto.hash.sha2.Sha256.init(.{});
    updateHashBytes(&hasher, "roc-run-link-inputs-v1");
    updateHashBytes(&hasher, @tagName(RocTarget.x64linux));
    updateInterpreterExeFileLinkInput(&hasher, first_declared, first_resolved, bytesDigest(first_contents));
    updateInterpreterExeFileLinkInput(&hasher, second_declared, second_resolved, bytesDigest(second_contents));
    return hasher.finalResult();
}

test "interpreter executable cache digest changes for checked host identity" {
    const baseline = testCacheDigest(testDigest(1), testDigest(2));
    const changed = testCacheDigest(testDigest(3), testDigest(2));
    try std.testing.expect(!std.mem.eql(u8, &baseline, &changed));
}

test "interpreter executable cache digest changes for linked host file contents" {
    const link_a = testLinkInputsIdentityForFiles(
        "libhost.a",
        "/platform/targets/x64linux/libhost.a",
        "old-host",
        "libsupport.a",
        "/platform/targets/x64linux/libsupport.a",
        "support",
    );
    const link_b = testLinkInputsIdentityForFiles(
        "libhost.a",
        "/platform/targets/x64linux/libhost.a",
        "new-host",
        "libsupport.a",
        "/platform/targets/x64linux/libsupport.a",
        "support",
    );
    try std.testing.expect(!std.mem.eql(u8, &link_a, &link_b));
}

test "interpreter executable cache digest changes for declared link order" {
    const link_a = testLinkInputsIdentityForFiles(
        "first.a",
        "/platform/targets/x64linux/first.a",
        "first",
        "second.a",
        "/platform/targets/x64linux/second.a",
        "second",
    );
    const link_b = testLinkInputsIdentityForFiles(
        "second.a",
        "/platform/targets/x64linux/second.a",
        "second",
        "first.a",
        "/platform/targets/x64linux/first.a",
        "first",
    );
    try std.testing.expect(!std.mem.eql(u8, &link_a, &link_b));
}

test "interpreter executable cache digest changes for platform host shim entrypoints" {
    const entrypoints_a = [_][]const u8{"init!"};
    const entrypoints_b = [_][]const u8{ "init!", "render!" };
    const shim_a = platformHostShimIdentity(.x64linux, &entrypoints_a, false);
    const shim_b = platformHostShimIdentity(.x64linux, &entrypoints_b, false);
    try std.testing.expect(!std.mem.eql(u8, &shim_a, &shim_b));
}

fn rejectRunTargetNotExecutable(ctx: *CliCtx, target: RocTarget) error{ WriteFailed, UnsupportedTarget }!void {
    const native_target = RocTarget.detectNative();
    try ctx.io.stderr().print(
        "Error: unsupported target for the default roc command: {s} cannot be executed on this host ({s}).\n\nUse `roc build --target={s}` to produce an artifact for that target.\n",
        .{ @tagName(target), @tagName(native_target), @tagName(target) },
    );
    return error.UnsupportedTarget;
}

fn rocRun(ctx: *CliCtx, args: cli_args.RunArgs, arg0: []const u8) CliMainError!void {
    switch (install_store.classifySourceRef(args.path)) {
        .url => return rocRunUrl(ctx, args, arg0),
        // The parser rejects bare `roc <shorthand>`, so a shorthand here came
        // from the explicit `roc run` subcommand.
        .shorthand => return rocRunInstalled(ctx, args),
        .local_path => {},
    }

    if (args.watch and args.opt != .dev) {
        try ctx.io.stderr().print(
            "Error: `roc --watch` currently supports only the dev backend.\n",
            .{},
        );
        return error.UnsupportedWatchMode;
    }

    return switch (args.opt) {
        .interpreter, .dev => rocRunSharedMemoryShim(ctx, args, arg0),
        .size, .speed => rocRunBuildAndExec(ctx, args, arg0),
    };
}

/// Direct URL run: download into the ordinary disposable package cache,
/// then build and run once. URL roots are immutable managed sources, so they
/// are never implicitly watched.
fn rocRunUrl(ctx: *CliCtx, args: cli_args.RunArgs, arg0: []const u8) CliMainError!void {
    if (args.explicit_watch) {
        try ctx.io.stderr().print("Error: --watch is not supported for URL sources.\n", .{});
        return error.UnsupportedWatchMode;
    }

    const resolved = try resolveUrlBundle(ctx, args.path);
    var url_args = args;
    url_args.path = resolved.source_path;
    url_args.root_source_url = args.path;
    url_args.watch = false;
    return rocRunBuildAndExec(ctx, url_args, arg0);
}

/// Run the optimized executable that `roc install` built for this shorthand.
/// No compilation and no network access happen here: the prebuilt binary is
/// the entire artifact, so this works even with the package cache deleted.
fn rocRunInstalled(ctx: *CliCtx, args: cli_args.RunArgs) CliMainError!void {
    const name = args.path;
    if (args.explicit_watch) {
        try ctx.io.stderr().print("Error: --watch is not supported for installed shorthands.\n", .{});
        return error.UnsupportedWatchMode;
    }
    if (args.explicit_opt) {
        try ctx.io.stderr().print(
            "Error: --opt has no effect on `roc run {s}`; installed tools always run the optimized binary that was built at install time.\n",
            .{name},
        );
        return error.InvalidArguments;
    }
    if (args.target != null) {
        try ctx.io.stderr().print(
            "Error: --target has no effect on `roc run {s}`; installed tools are built for this machine at install time.\n",
            .{name},
        );
        return error.InvalidArguments;
    }

    const entry = try resolveInstalledEntry(ctx, name);
    if (entry.kind != .executable) {
        try ctx.io.stderr().print(
            "Error: `{s}` is installed as a glue spec, not an application. Use it with: roc glue {s} <output-dir> <platform>\n",
            .{ name, name },
        );
        return error.InvalidArguments;
    }
    const term = try runCompiledExecutable(ctx, entry.artifact_path, args.app_args);
    try finishCompiledRun(ctx, entry.artifact_path, term, 0);
}

const install_manifest_size_limit = 64 * 1024;

/// Errors that resolving a URL/shorthand source reference can produce, shared
/// by every subcommand that accepts one.
const SourceRefResolveError = CliError || Allocator.Error || error{ UnsupportedWatchMode, WriteFailed };

/// A source argument resolved to a compilable local path, together with the
/// bundle URL it came from when the source was a managed URL/installed root.
/// The URL — not the extracted path — is the root's package identity.
const ResolvedSourceArg = struct {
    path: []const u8,
    url: ?[]const u8,
};

/// Resolve a source argument that may be a bundle URL or an installed
/// shorthand into a local source path. Local paths pass through unchanged.
/// URL and installed roots are immutable managed sources, so `--watch` is
/// rejected for both.
fn resolveSourceArg(ctx: *CliCtx, path: []const u8, watch: bool) SourceRefResolveError!ResolvedSourceArg {
    switch (install_store.classifySourceRef(path)) {
        .local_path => return .{ .path = path, .url = null },
        .url => {
            if (watch) {
                try ctx.io.stderr().print("Error: --watch is not supported for URL sources.\n", .{});
                return error.UnsupportedWatchMode;
            }
            const resolved = try resolveUrlBundle(ctx, path);
            return .{ .path = resolved.source_path, .url = path };
        },
        .shorthand => {
            if (watch) {
                try ctx.io.stderr().print("Error: --watch is not supported for installed shorthands.\n", .{});
                return error.UnsupportedWatchMode;
            }
            const entry = try resolveInstalledEntry(ctx, path);
            return .{ .path = entry.paths.main_roc_path, .url = entry.url };
        },
    }
}

/// A validated install entry: its paths, the kind of artifact it carries,
/// and the bundle URL recorded in its manifest (the entry's compiler-level
/// identity).
const ResolvedInstalledEntry = struct {
    paths: install_store.EntryPaths,
    kind: install_store.InstallKind,
    artifact_path: []const u8,
    url: []const u8,
};

/// Resolve a shorthand to its published install entry, validating the
/// manifest and the presence of the built artifact. Missing or corrupt state
/// is an explicit error — never a fallback to a cache entry or a redownload.
fn resolveInstalledEntry(ctx: *CliCtx, name: []const u8) (CliError || Allocator.Error)!ResolvedInstalledEntry {
    const root = install_store.installRootDir(ctx.coreCtx(), ctx.arena) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        error.NoHomeDirectory => return ctx.fail(.{ .install_dir_unavailable = .{
            .reason = "No home directory could be determined",
        } }),
    };
    const version_dir = try install_store.versionDir(ctx.arena, root);
    const entry = try install_store.entryPaths(ctx.arena, version_dir, name);

    const manifest_bytes = std.Io.Dir.cwd().readFileAlloc(ctx.io.std_io, entry.manifest_path, ctx.arena, .limited(install_manifest_size_limit)) catch |err| switch (err) {
        error.FileNotFound => {
            var entry_dir = std.Io.Dir.cwd().openDir(ctx.io.std_io, entry.entry_dir, .{}) catch {
                return ctx.fail(.{ .unknown_shorthand = .{ .name = name } });
            };
            entry_dir.close(ctx.io.std_io);
            return ctx.fail(.{ .install_entry_corrupt = .{
                .name = name,
                .path = entry.entry_dir,
                .reason = "its install.json manifest is missing",
            } });
        },
        else => return ctx.fail(.{ .install_entry_corrupt = .{
            .name = name,
            .path = entry.entry_dir,
            .reason = "its install.json manifest could not be read",
        } }),
    };
    var parsed = (try install_store.parseManifest(ctx.gpa, manifest_bytes)) orelse {
        return ctx.fail(.{ .install_entry_corrupt = .{
            .name = name,
            .path = entry.entry_dir,
            .reason = "its install.json manifest is not valid",
        } });
    };
    defer parsed.deinit();

    // The recorded URL is the entry's identity, so it must still be a valid
    // bundle URL — a manifest that fails this check is corrupt, and nothing
    // downstream may be handed an unvalidated URL.
    _ = base.url.parseUrlPath(parsed.manifest().url) catch {
        return ctx.fail(.{ .install_entry_corrupt = .{
            .name = name,
            .path = entry.entry_dir,
            .reason = "its install.json manifest records an invalid URL",
        } });
    };
    const manifest_url = try ctx.arena.dupe(u8, parsed.manifest().url);
    // parseManifest already validated the kind string.
    const kind = install_store.manifestKind(parsed.manifest()).?;

    const artifact_path = entry.artifactPath(kind);
    std.Io.Dir.cwd().access(ctx.io.std_io, artifact_path, .{}) catch {
        return ctx.fail(.{ .install_entry_corrupt = .{
            .name = name,
            .path = entry.entry_dir,
            .reason = "its built artifact is missing",
        } });
    };

    return .{ .paths = entry, .kind = kind, .artifact_path = artifact_path, .url = manifest_url };
}

fn rocRunSharedMemoryShim(ctx: *CliCtx, args: cli_args.RunArgs, arg0: []const u8) CliMainError!void {
    const trace = tracy.trace(@src());
    defer trace.end();

    // Check if this is a default_app (headerless file with main!) before
    // linking the platform host shim.
    if (try readDefaultAppSource(ctx, args.path)) |source| {
        // Headerless default apps never hot reload; they just run once. The shared-memory
        // shim is the run mechanism where the default platform runtime exists (Linux native,
        // or any cross-target run); elsewhere we use the plain run-once path.
        if (useDefaultAppSharedMemoryShim(args)) {
            return rocRunDefaultAppSharedMemoryShim(ctx, args, source);
        }
        return rocRunDefaultApp(ctx, args, source);
    }

    if (args.opt == .dev and args.no_cache and !args.watch) {
        return rocRunBuildAndExec(ctx, args, arg0);
    }

    // Initialize cache - used to store our shim, and linked interpreter executables in cache
    const cache_config = CacheConfig{
        .enabled = !args.no_cache,
        .verbose = false,
        .roc_ctx = ctx.coreCtx(),
    };
    var cache_manager = CacheManager.init(ctx.gpa, cache_config, ctx.coreCtx());

    // Create cache directory for linked interpreter executables
    const exe_cache_dir = cache_manager.config.getExeCacheDir(ctx.arena) catch |err| {
        return ctx.fail(.{ .cache_dir_unavailable = .{ .reason = @errorName(err) } });
    };

    ensureCompilerCacheDirExists(ctx.io.std_io, exe_cache_dir) catch |err| switch (err) {
        error.PathAlreadyExists => {},
        else => {
            return ctx.fail(.{ .directory_create_failed = .{ .path = exe_cache_dir, .err = err } });
        },
    };

    // The final executable name seen in `ps` is the roc filename (e.g., "app.roc")
    const exe_display_name = std.fs.path.basename(args.path);

    // Display name for temp directory (what shows in ps)
    const exe_display_name_with_ext = if (builtin.target.os.tag == .windows)
        std.fmt.allocPrint(ctx.arena, "{s}.exe", .{exe_display_name}) catch |err| {
            return ctx.fail(.{ .cache_dir_unavailable = .{ .reason = @errorName(err) } });
        }
    else
        ctx.arena.dupe(u8, exe_display_name) catch |err| {
            return ctx.fail(.{ .cache_dir_unavailable = .{ .reason = @errorName(err) } });
        };

    // Create unique temp directory for this build (uses PID for uniqueness)
    const temp_dir_path = createUniqueTempDir(ctx) catch |err| {
        return ctx.fail(.{ .temp_dir_failed = .{ .err = err } });
    };

    // The executable is built directly in the temp dir with the display name
    const exe_path = std.fs.path.join(ctx.arena, &.{ temp_dir_path, exe_display_name_with_ext }) catch |err| {
        return ctx.fail(.{ .cache_dir_unavailable = .{ .reason = @errorName(err) } });
    };

    // Resolve platform paths from the app header before linking the host shim.
    const platform_paths = try resolvePlatformPaths(ctx, args.path);

    // Validate platform header and get link spec
    var link_spec: ?roc_target.TargetLinkSpec = null;
    var targets_config: ?roc_target.TargetsConfig = null;
    if (platform_paths.platform_source_path) |platform_source| {
        if (platform_validation.validatePlatformHeader(ctx.arena, ctx.io.std_io, platform_source)) |validation| {
            targets_config = validation.config;

            const selected = try selectRunPlatformTarget(ctx, validation.config, platform_source, args.target);
            link_spec = selected.link_spec;
        } else |err| {
            switch (err) {
                error.MissingTargetsSection => {
                    ctx.io.stderr().print("Error: Platform is missing a targets section.\n\n", .{}) catch {};
                    ctx.io.stderr().print("All platforms must have a 'targets:' section in their header\n", .{}) catch {};
                    ctx.io.stderr().print("that specifies which targets are supported and what files to link.\n", .{}) catch {};
                    return error.PlatformNotSupported;
                },
                else => {
                    std.log.debug("Could not validate platform header: {}", .{err});
                },
            }
        }
    }

    // All platforms must have a targets section with a link spec for a compatible target
    const validated_link_spec = link_spec orelse {
        ctx.io.stderr().print("Error: Platform does not support any target compatible with this system.\n\n", .{}) catch {};
        ctx.io.stderr().print("The platform's targets section must specify files to link for\n", .{}) catch {};
        ctx.io.stderr().print("the current system. Check the platform header for supported targets.\n", .{}) catch {};
        return error.PlatformNotSupported;
    };

    // Lower before linking so the host shim uses checked entrypoint metadata
    // rather than rediscovering roots from platform source syntax after checking.
    var reporter = makeReporter(ctx, "roc", args.timings);
    defer reporter.deinit();
    reporter.start();

    var lowered_result: ?LoweredCoordinatorResult = null;
    defer if (lowered_result) |*result| result.deinit();

    var shm_handle_opt: ?SharedMemoryHandle = null;
    defer if (shm_handle_opt) |handle| closeSharedMemoryHandle(handle);

    var entrypoint_names: []const []const u8 = &.{};
    var hosted_symbols: []const []const u8 = &.{};
    var checked_host_identity_opt: ?[32]u8 = null;
    var error_count: usize = 0;
    var warning_count: usize = 0;

    switch (args.opt) {
        .dev => {
            lowered_result = try lowerLirWithBuildEnv(
                ctx,
                ctx.gpa,
                .{ .dev_run_image = validated_link_spec.target },
                args.path,
                null,
                null,
                args.max_threads,
                args.opt,
                resolutionConfigFromLimits(args.resolve_limits),
                !args.no_cache,
                &reporter,
                true,
            );
            const result = if (lowered_result) |*value| value else unreachable;
            entrypoint_names = result.entrypoint_names;
            hosted_symbols = result.hosted_symbols;
            checked_host_identity_opt = result.checked_host_identity;
            error_count = result.counts.errors;
            warning_count = result.counts.warnings;
        },
        .interpreter => {
            const shm_result = try buildLirImageWithBuildEnv(
                ctx,
                args.path,
                null,
                null,
                args.max_threads,
                args.opt,
                resolutionConfigFromLimits(args.resolve_limits),
                !args.no_cache,
                &reporter,
                true,
            );
            shm_handle_opt = shm_result.handle;
            entrypoint_names = shm_result.entrypoint_names;
            hosted_symbols = shm_result.hosted_symbols;
            checked_host_identity_opt = shm_result.checked_host_identity;
            error_count = shm_result.error_count;
            warning_count = shm_result.warning_count;
        },
        .size, .speed => unreachable,
    }

    if (error_count > 0 and entrypoint_names.len == 0) {
        reporter.fail();
        if (args.allow_errors) return;
        return error.TypeCheckingFailed;
    }
    reporter.finish();

    if (entrypoint_names.len == 0) {
        if (builtin.mode == .Debug) {
            std.debug.panic("default roc command invariant violated: no platform entrypoints in checked LIR root metadata", .{});
        }
        unreachable;
    }

    const selected_target = validated_link_spec.target;
    const enable_debug = builtin.mode == .Debug;
    const shim_kind: ShimLibraryKind = switch (args.opt) {
        .dev => .machine_code,
        .interpreter => .lir,
        .size, .speed => unreachable,
    };

    const checked_host_identity = checked_host_identity_opt orelse {
        if (builtin.mode == .Debug) {
            std.debug.panic("default roc command invariant violated: missing checked host identity after successful LIR image build", .{});
        }
        unreachable;
    };

    const platform_dir = if (platform_paths.platform_source_path) |p|
        std.fs.path.dirname(p) orelse "."
    else
        ".";
    const files_dir = if (targets_config) |cfg| cfg.inputs_dir orelse "targets" else "targets";
    const target_name = @tagName(selected_target);

    const link_inputs_identity = try interpreterExeLinkInputsIdentity(
        ctx,
        shim_kind,
        validated_link_spec,
        platform_dir,
        files_dir,
        selected_target,
        entrypoint_names,
        enable_debug,
    );

    const exe_cache_name = try shimHostExeCacheName(ctx, .{
        .shim_kind = shim_kind,
        .target = selected_target,
        .debug = enable_debug,
        .checked_host_identity = checked_host_identity,
        .link_inputs_identity = link_inputs_identity,
    });
    const exe_cache_name_with_ext = if (builtin.target.os.tag == .windows)
        std.fmt.allocPrint(ctx.arena, "{s}.exe", .{exe_cache_name}) catch |err| {
            return ctx.fail(.{ .cache_dir_unavailable = .{ .reason = @errorName(err) } });
        }
    else
        ctx.arena.dupe(u8, exe_cache_name) catch |err| {
            return ctx.fail(.{ .cache_dir_unavailable = .{ .reason = @errorName(err) } });
        };

    const exe_cache_path = std.fs.path.join(ctx.arena, &.{ exe_cache_dir, exe_cache_name_with_ext }) catch |err| {
        return ctx.fail(.{ .cache_dir_unavailable = .{ .reason = @errorName(err) } });
    };

    // Check if the interpreter executable already exists in cache
    const cache_exists = if (args.no_cache) false else blk: {
        std.Io.Dir.cwd().access(ctx.io.std_io, exe_cache_path, .{}) catch {
            break :blk false;
        };
        break :blk true;
    };

    if (cache_exists) {
        // Cached executable exists - hardlink from cache to temp dir
        std.log.debug("Using cached executable: {s}", .{exe_cache_path});
        createHardlink(ctx, exe_cache_path, exe_path) catch |err| {
            // If hardlinking fails, fall back to copying
            std.log.debug("Hardlink from cache failed, copying: {}", .{err});
            std.Io.Dir.cwd().copyFile(exe_cache_path, std.Io.Dir.cwd(), exe_path, ctx.io.std_io, .{}) catch |copy_err| {
                return ctx.fail(.{ .file_write_failed = .{
                    .path = exe_path,
                    .err = copy_err,
                } });
            };
        };
    } else {

        // Extract shim library to temp dir to avoid race conditions
        const shim_filename = switch (shim_kind) {
            .lir => if (builtin.target.os.tag == .windows) "roc_interpreter_shim.lib" else "libroc_interpreter_shim.a",
            .machine_code => if (builtin.target.os.tag == .windows) "roc_machine_code_shim.lib" else "libroc_machine_code_shim.a",
        };
        const shim_path = std.fs.path.join(ctx.arena, &.{ temp_dir_path, shim_filename }) catch {
            return error.OutOfMemory;
        };

        // Always extract to temp dir (unique per process, no race condition)
        // Use the selected target's shim (which may differ from native if falling back to a compatible target)
        extractShimLibrary(ctx, shim_kind, shim_path, selected_target) catch |err| {
            return ctx.fail(.{ .shim_generation_failed = .{ .err = err } });
        };

        // Generate platform host shim using the published checked-artifact entrypoints
        // Use temp dir to avoid race conditions when multiple processes run in parallel
        // Auto-enable debug when roc is built in debug mode (no explicit --debug flag for the default `roc` command)
        const platform_shim_path = switch (args.opt) {
            .dev => blk: {
                const result = if (lowered_result) |*value| value else unreachable;
                const lowered = successfulLoweredProgram(result, "default roc command");
                const platform_entrypoints = try lowered.platformEntrypoints(ctx.gpa);
                defer ctx.gpa.free(platform_entrypoints);
                break :blk try generatePlatformHostShimFromLirData(
                    ctx,
                    temp_dir_path,
                    entrypoint_names,
                    hosted_symbols,
                    selected_target,
                    &lowered.lir_result.store,
                    &lowered.lir_result.layouts,
                    platform_entrypoints,
                    null,
                    0,
                    false,
                    enable_debug,
                );
            },
            .interpreter => blk: {
                const shm_handle = shm_handle_opt orelse {
                    if (builtin.mode == .Debug) {
                        std.debug.panic("interpreter run invariant violated: missing LIR shared-memory handle", .{});
                    }
                    unreachable;
                };
                const shm_image_bytes = @as([*]const u8, @ptrCast(shm_handle.ptr))[0..shm_handle.size];
                break :blk try generatePlatformHostShim(
                    ctx,
                    temp_dir_path,
                    entrypoint_names,
                    hosted_symbols,
                    selected_target,
                    shm_image_bytes,
                    false,
                    false,
                    enable_debug,
                );
            },
            .size, .speed => unreachable,
        };

        // Link the host.a with our shim to create the interpreter executable using our linker
        // Try LLD first, then clang if LLVM is not available.
        var extra_args = std.array_list.Managed([]const u8).initCapacity(ctx.arena, 32) catch {
            return error.OutOfMemory;
        };

        // Add system libraries for macOS
        if (builtin.target.os.tag == .macos) {
            extra_args.append("-lSystem") catch {
                return error.OutOfMemory;
            };
        }

        // Build object files list from the link spec items
        // Items are linked in the order specified in the targets section
        var object_files = std.array_list.Managed([]const u8).initCapacity(ctx.arena, 16) catch {
            return error.OutOfMemory;
        };

        std.log.debug("Platform dir: {s}, files_dir: {s}, target: {s}", .{ platform_dir, files_dir, target_name });

        // Process each link item in order
        var host_input_paths = std.ArrayList([]const u8).empty;

        for (validated_link_spec.items) |item| {
            switch (item) {
                .file_path => |file_name| {
                    // Resolve path: platform_dir / files_dir / target_name / file_name
                    const full_path = std.fs.path.join(ctx.arena, &.{
                        platform_dir, files_dir, target_name, file_name,
                    }) catch {
                        return error.OutOfMemory;
                    };
                    std.log.debug("Adding link item: {s}", .{full_path});
                    object_files.append(full_path) catch {
                        return error.OutOfMemory;
                    };
                    host_input_paths.append(ctx.arena, full_path) catch {
                        return error.OutOfMemory;
                    };
                },
                .app => {
                    // Add the compiled Roc application (shim)
                    std.log.debug("Adding app (shim): {s}", .{shim_path});
                    object_files.append(shim_path) catch {
                        return error.OutOfMemory;
                    };
                    // Also add platform shim if available
                    if (platform_shim_path) |path| {
                        object_files.append(path) catch {
                            return error.OutOfMemory;
                        };
                    }
                },
                .win_gui => {
                    // Windows GUI flag - handled separately in linker config
                    std.log.debug("win_gui flag detected", .{});
                },
            }
        }

        // Determine ABI from target (for musl detection)
        const target_abi: linker.TargetAbi = if (validated_link_spec.target.isStatic()) .musl else .gnu;
        std.log.debug("Target ABI: {?}", .{target_abi});

        // No pre/post files needed - everything comes from link spec in order
        const empty_files: []const []const u8 = &.{};

        // Build full path to platform files directory for sysroot lookup
        const platform_files_dir = std.fs.path.join(ctx.arena, &.{ platform_dir, files_dir }) catch {
            return error.OutOfMemory;
        };

        // The interpreter executable's shim references the same hosted and
        // runtime symbols compiled output would; the host inputs must define
        // them all.
        {
            const referenced_hosted_symbols = switch (args.opt) {
                .dev => blk: {
                    const result = if (lowered_result) |*value| value else unreachable;
                    const lowered = successfulLoweredProgram(result, "default roc command");
                    break :blk try hostedSymbolsFromLir(ctx.arena, &lowered.lir_result.store);
                },
                .interpreter => blk: {
                    const shm_handle = shm_handle_opt orelse {
                        if (builtin.mode == .Debug) {
                            std.debug.panic("interpreter run invariant violated: missing LIR shared-memory handle", .{});
                        }
                        unreachable;
                    };
                    const view = try viewLirImageFromHandle(shm_handle, base.target.TargetUsize.native, ctx.arena);
                    break :blk try hostedSymbolsFromLir(ctx.arena, &view.store);
                },
                .size, .speed => unreachable,
            };
            try verifyHostInputSymbols(
                ctx,
                host_input_paths.items,
                referenced_hosted_symbols,
                target_name,
                false,
            );
        }

        const link_config = linker.LinkConfig{
            .target_abi = target_abi,
            .output_path = exe_path,
            .object_files = object_files.items,
            .platform_files_pre = empty_files,
            .platform_files_post = empty_files,
            .extra_args = extra_args.items,
            .can_exit_early = false,
            .disable_output = false,
            .platform_files_dir = platform_files_dir,
            .scratch_dir = temp_dir_path,
        };

        linker.link(ctx, link_config) catch |err| {
            return ctx.fail(.{ .linker_failed = .{
                .err = err,
                .target = @tagName(validated_link_spec.target),
            } });
        };

        // After building, hardlink to cache for future runs
        // Force-hardlink (delete existing first) since hash collision means identical content
        std.log.debug("Caching executable to: {s}", .{exe_cache_path});
        std.Io.Dir.cwd().deleteFile(ctx.io.std_io, exe_cache_path) catch |err| switch (err) {
            error.FileNotFound => {}, // OK, doesn't exist
            else => std.log.debug("Could not delete existing cache file: {}", .{err}),
        };
        createHardlink(ctx, exe_path, exe_cache_path) catch |err| {
            // If hardlinking fails, fall back to copying
            std.log.debug("Hardlink to cache failed, copying: {}", .{err});
            std.Io.Dir.cwd().copyFile(exe_path, std.Io.Dir.cwd(), exe_cache_path, ctx.io.std_io, .{}) catch |copy_err| {
                // Non-fatal - just means future runs won't be cached
                std.log.debug("Failed to copy to cache: {}", .{copy_err});
            };
        };
    }

    if (args.opt == .dev) {
        const result = if (lowered_result) |*value| value else unreachable;
        const lowered = successfulLoweredProgram(result, "default roc command");
        const internal_static_data = successfulInternalStaticData(result, "default roc command");
        shm_handle_opt = try publishDevRunImage(ctx, selected_target, entrypoint_names, lowered, internal_static_data, args.watch);
    }

    const shm_handle = shm_handle_opt orelse {
        if (builtin.mode == .Debug) {
            std.debug.panic("default roc command invariant violated: missing shared-memory handle before launching shim", .{});
        }
        unreachable;
    };

    std.log.debug("Launching shim executable: {s}", .{exe_path});
    if (args.watch) {
        const result = if (lowered_result) |*value| value else {
            if (builtin.mode == .Debug) {
                std.debug.panic("hot reload invariant violated: missing lowered result for dev shim watch run", .{});
            }
            unreachable;
        };
        try runHotReloadDevShim(
            ctx,
            arg0,
            exe_path,
            shm_handle,
            args,
            selected_target,
            checked_host_identity,
            result.watch_inputs,
            null,
            warning_count,
        );
    } else if (comptime is_windows) {
        // Windows: Use handle inheritance approach
        std.log.debug("Using Windows handle inheritance approach", .{});
        try runWithWindowsHandleInheritance(ctx, exe_path, shm_handle, args.app_args);
    } else {
        // POSIX: Use existing file descriptor inheritance approach
        std.log.debug("Using POSIX file descriptor inheritance approach", .{});
        try runWithPosixFdInheritance(ctx, exe_path, shm_handle, args.app_args);
    }
    std.log.debug("Interpreter execution completed", .{});

    // Exit with code 2 if there were warnings (but no errors)
    exitOnWarnings(ctx, warning_count);
}

fn rocRunBuildAndExec(ctx: *CliCtx, args: cli_args.RunArgs, arg0: []const u8) CliMainError!void {
    const trace = tracy.trace(@src());
    defer trace.end();

    const temp_dir = createUniqueTempDir(ctx) catch |err| {
        return ctx.fail(.{ .temp_dir_failed = .{ .err = err } });
    };
    var cleanup_temp_dir = true;
    defer if (cleanup_temp_dir) {
        compile.CacheCleanup.deleteTempDir(ctx.io.std_io, temp_dir);
    };

    const output_filename = try compiledRunOutputFilename(ctx, args.path);
    const exe_path = try std.fs.path.join(ctx.arena, &.{ temp_dir, output_filename });

    var warning_count: usize = 0;
    try rocBuild(ctx, .{
        .path = args.path,
        .opt = args.opt,
        .target = args.target,
        .output = exe_path,
        .debug = false,
        .allow_errors = args.allow_errors,
        .verbose = false,
        .no_cache = args.no_cache,
        .max_threads = args.max_threads,
        .wasm_memory = null,
        .wasm_stack_size = null,
        .exit_on_warnings = false,
        .warning_count_out = &warning_count,
        .require_executable_output = true,
        .require_host_runnable_output = true,
        .suppress_build_status = true,
        .resolve_limits = args.resolve_limits,
        .synthetic_default_platform = false,
        .source_dir_override = null,
        .root_source_url = args.root_source_url,
    }, arg0);

    const term = try runCompiledExecutable(ctx, exe_path, args.app_args);

    compile.CacheCleanup.deleteTempDir(ctx.io.std_io, temp_dir);
    cleanup_temp_dir = false;

    try finishCompiledRun(ctx, exe_path, term, warning_count);
}

fn compiledRunOutputFilename(ctx: *CliCtx, roc_path: []const u8) Allocator.Error![]const u8 {
    const module_name = try base.module_path.getModuleNameAlloc(ctx.arena, roc_path);
    if (builtin.target.os.tag == .windows) {
        return try std.fmt.allocPrint(ctx.arena, "{s}.exe", .{module_name});
    }
    return module_name;
}

fn runCompiledExecutable(
    ctx: *CliCtx,
    exe_path: []const u8,
    app_args: []const []const u8,
) (CliError || error{OutOfMemory})!std.process.Child.Term {
    const argv = ctx.arena.alloc([]const u8, 1 + app_args.len) catch {
        return error.OutOfMemory;
    };
    argv[0] = exe_path;
    for (app_args, 0..) |arg, i| {
        argv[1 + i] = arg;
    }

    var child = std.process.spawn(ctx.io.std_io, .{
        .argv = argv,
        .cwd = .inherit,
        .stdout = .inherit,
        .stderr = .inherit,
    }) catch |err| {
        return ctx.fail(.{ .child_process_spawn_failed = .{
            .command = exe_path,
            .err = err,
        } });
    };

    return child.wait(ctx.io.std_io) catch |err| {
        return ctx.fail(.{ .child_process_wait_failed = .{
            .command = exe_path,
            .err = err,
        } });
    };
}

const NativeRunTermination = union(enum) {
    success,
    exit_code: u8,
    signal: std.posix.SIG,
    stopped: std.posix.SIG,
    unknown: u32,
};

fn classifyNativeRunTermination(term: std.process.Child.Term, warning_count: usize) NativeRunTermination {
    return switch (term) {
        .exited => |code| if (code != 0)
            .{ .exit_code = code }
        else if (warning_count > 0)
            .{ .exit_code = 2 }
        else
            .success,
        .signal => |signal| .{ .signal = signal },
        .stopped => |signal| .{ .stopped = signal },
        .unknown => |status| .{ .unknown = status },
    };
}

fn finishCompiledRun(
    ctx: *CliCtx,
    exe_path: []const u8,
    term: std.process.Child.Term,
    warning_count: usize,
) (CliError || error{WriteFailed})!void {
    switch (classifyNativeRunTermination(term, warning_count)) {
        .success => return,
        .exit_code => |code| {
            ctx.io.flush();
            std.process.exit(code);
        },
        .signal => |signal| {
            const sig_num = @intFromEnum(signal);
            const result = platform_validation.targets_validator.ValidationResult{
                .process_signaled = .{ .signal = sig_num },
            };
            renderValidationError(ctx.gpa, result, ctx.io.stderr());
            ctx.io.flush();
            std.process.exit(128 +| @as(u8, @truncate(sig_num)));
        },
        .stopped => |signal| {
            return ctx.fail(.{ .child_process_signaled = .{
                .command = exe_path,
                .signal = @intFromEnum(signal),
            } });
        },
        .unknown => |status| {
            return ctx.fail(.{ .child_process_failed = .{
                .command = exe_path,
                .exit_code = status,
            } });
        },
    }
}

/// Check if a file is a default_app (headerless file with a main! function).
/// On success, returns the file source (caller owns the allocation).
/// Returns null if the file is not a default_app.
fn readDefaultAppSource(ctx: *CliCtx, file_path: []const u8) std.mem.Allocator.Error!?[]const u8 {
    const max_source_size = 256 * 1024 * 1024; // 256 MB
    const source = std.Io.Dir.cwd().readFileAlloc(ctx.io.std_io, file_path, ctx.gpa, .limited(max_source_size)) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        // Any other read failure (e.g. file not found) means this isn't a
        // default app to handle here; fall through to the normal path.
        else => return null,
    };

    const module_name = base.module_path.getModuleNameAlloc(ctx.arena, file_path) catch |err| switch (err) {
        error.OutOfMemory => {
            ctx.gpa.free(source);
            return error.OutOfMemory;
        },
    };

    var env = ModuleEnv.init(ctx.gpa, source) catch |err| switch (err) {
        error.OutOfMemory => {
            ctx.gpa.free(source);
            return error.OutOfMemory;
        },
    };
    defer env.deinit();
    env.common.source = source;
    env.module_name = module_name;

    const ast = parse.file(ctx.gpa, &env.common) catch |err| switch (err) {
        error.OutOfMemory => {
            ctx.gpa.free(source);
            return error.OutOfMemory;
        },
    };
    defer ast.deinit();

    const file = ast.store.getFile();
    const header = ast.store.getHeader(file.header);

    // Only headerless files (type_module) can be default apps
    if (header != .type_module) {
        ctx.gpa.free(source);
        return null;
    }

    if (!ast.hasMainBangDecl()) {
        ctx.gpa.free(source);
        return null;
    }

    return source;
}

/// State for the CLI echo platform's virtual I/O context.
/// Intercepts reads for the synthetic app source and embedded platform files.
const default_app_run_header =
    "app [main!] { pf: platform \"./.roc_echo_platform/main.roc\" }\n\n" ++
    "import pf.Echo\n\n" ++
    "echo! = |msg| Echo.line!(msg)\n\n";

fn writeDefaultAppSyntheticRunSource(ctx: *CliCtx, app_path: []const u8, original_source: []const u8) CliMainError!void {
    const synthetic_source = try std.mem.concat(ctx.gpa, u8, &.{ default_app_run_header, original_source });
    defer ctx.gpa.free(synthetic_source);

    try std.Io.Dir.cwd().writeFile(ctx.io.std_io, .{ .sub_path = app_path, .data = synthetic_source });
}

/// Run a default_app (headerless file with main! and echo platform).
/// This compiles the app through checked artifacts and executes the resulting
/// LIR image with the echo platform host function.
fn rocRunDefaultApp(ctx: *CliCtx, args: cli_args.RunArgs, original_source: []const u8) CliMainError!void {
    defer ctx.gpa.free(original_source);

    // Write synthetic app + echo platform files into a unique temp directory.
    // The coordinator reads platform/module files from disk, so the synthetic
    // sources must exist on the filesystem (a `cliEchoReadFile` filesystem
    // override only helps build_env paths, not the coordinator spawned by
    // `buildLirRuntimeImageWithCoordinator`).
    const temp_dir = createUniqueTempDir(ctx) catch |err| {
        ctx.io.stderr().print("error: failed to create temp dir: {}\n", .{err}) catch {};
        return err;
    };
    defer std.Io.Dir.cwd().deleteTree(ctx.io.std_io, temp_dir) catch {};

    const platform_dir = std.fs.path.join(ctx.arena, &.{ temp_dir, ".roc_echo_platform" }) catch return error.OutOfMemory;
    std.Io.Dir.cwd().createDirPath(ctx.io.std_io, platform_dir) catch |err| {
        ctx.io.stderr().print("error: failed to create platform dir {s}: {}\n", .{ platform_dir, err }) catch {};
        return err;
    };

    const app_path = std.fs.path.join(ctx.arena, &.{ temp_dir, "main.roc" }) catch return error.OutOfMemory;
    const platform_main_path = std.fs.path.join(ctx.arena, &.{ platform_dir, "main.roc" }) catch return error.OutOfMemory;
    const echo_module_path = std.fs.path.join(ctx.arena, &.{ platform_dir, "Echo.roc" }) catch return error.OutOfMemory;

    writeDefaultAppSyntheticRunSource(ctx, app_path, original_source) catch |err| {
        ctx.io.stderr().print("error: failed to write {s}: {}\n", .{ app_path, err }) catch {};
        return err;
    };
    std.Io.Dir.cwd().writeFile(ctx.io.std_io, .{ .sub_path = platform_main_path, .data = echo_platform.platform_main_source }) catch |err| {
        ctx.io.stderr().print("error: failed to write {s}: {}\n", .{ platform_main_path, err }) catch {};
        return err;
    };
    std.Io.Dir.cwd().writeFile(ctx.io.std_io, .{ .sub_path = echo_module_path, .data = echo_platform.echo_module_source }) catch |err| {
        ctx.io.stderr().print("error: failed to write {s}: {}\n", .{ echo_module_path, err }) catch {};
        return err;
    };

    const original_source_dir = std.fs.path.dirname(args.path) orelse ".";
    var reporter = makeReporter(ctx, "roc", args.timings);
    defer reporter.deinit();
    reporter.start();
    const shm_result = try buildLirImageWithBuildEnv(
        ctx,
        app_path,
        original_source_dir,
        .{ .original_path = args.path, .original_source = original_source },
        args.max_threads,
        args.opt,
        resolutionConfigFromLimits(args.resolve_limits),
        !args.no_cache,
        &reporter,
        true,
    );
    defer closeSharedMemoryHandle(shm_result.handle);

    if (shm_result.error_count > 0 and shm_result.entrypoint_names.len == 0) {
        reporter.fail();
        if (args.allow_errors) return;
        return error.TypeCheckingFailed;
    }
    reporter.finish();

    const view = try viewLirImageFromHandle(shm_result.handle, base.target.TargetUsize.native, ctx.arena);

    var hosted_fn_array = [_]echo_platform.host_abi.HostedFn{echo_platform.host_abi.hostedFn(&echo_platform.echoHostedFn)};
    var echo_env = echo_platform.EchoEnv{ .std_io = ctx.io.std_io };
    var roc_ops = echo_platform.makeDefaultRocOps(&echo_env, &hosted_fn_array);
    echo_platform.g_roc_ops = &roc_ops;
    var cli_args_list = try echo_platform.buildCliArgs(args.app_args, &roc_ops);

    var result_buf: [16]u8 align(16) = undefined;
    try evaluateLirImageEntrypoint(
        ctx.gpa,
        &view,
        0,
        &roc_ops,
        @ptrCast(&result_buf),
        @ptrCast(&cli_args_list),
    );

    const exit_code = result_buf[0];
    if (exit_code != 0) std.process.exit(exit_code);
    if (echo_env.inline_expect_failed) std.process.exit(1);
    exitOnWarnings(ctx, shm_result.warning_count);
}

fn rocRunDefaultAppSharedMemoryShim(ctx: *CliCtx, args: cli_args.RunArgs, original_source: []const u8) CliMainError!void {
    defer ctx.gpa.free(original_source);

    const native_target = RocTarget.detectNative();
    const default_target = defaultRunShimTarget(native_target);
    const selected_target = if (args.target) |target_str| blk: {
        const requested = RocTarget.fromString(target_str) orelse {
            try ctx.io.stderr().print("Error: invalid target for roc: {s}\n", .{target_str});
            return error.InvalidTarget;
        };
        if (!devShimTargetCompatible(requested, native_target)) {
            try rejectRunTargetNotExecutable(ctx, requested);
            unreachable;
        }
        if (requested.isStatic()) {
            try ctx.io.stderr().print(
                "Error: shared-memory dev runs for headerless default apps require a dynamic Linux target; got {s}.\n",
                .{@tagName(requested)},
            );
            return error.UnsupportedTarget;
        }
        break :blk requested;
    } else default_target;

    if (selected_target.toOsTag() != .linux) {
        try ctx.io.stderr().print(
            "Error: shared-memory dev runs for headerless default apps are currently supported only on Linux targets.\n",
            .{},
        );
        return error.UnsupportedTarget;
    }

    if (DefaultPlatformRuntimeObjects.forTarget(selected_target) == null) {
        return rejectRunTargetNotExecutable(ctx, selected_target);
    }

    const cache_config = CacheConfig{
        .enabled = !args.no_cache,
        .verbose = false,
        .roc_ctx = ctx.coreCtx(),
    };
    var cache_manager = CacheManager.init(ctx.gpa, cache_config, ctx.coreCtx());
    const exe_cache_dir = cache_manager.config.getExeCacheDir(ctx.arena) catch |err| {
        return ctx.fail(.{ .cache_dir_unavailable = .{ .reason = @errorName(err) } });
    };
    ensureCompilerCacheDirExists(ctx.io.std_io, exe_cache_dir) catch |err| switch (err) {
        error.PathAlreadyExists => {},
        else => return ctx.fail(.{ .directory_create_failed = .{ .path = exe_cache_dir, .err = err } }),
    };

    const temp_dir = createUniqueTempDir(ctx) catch |err| {
        return ctx.fail(.{ .temp_dir_failed = .{ .err = err } });
    };
    var cleanup_temp_dir = true;
    defer if (cleanup_temp_dir) {
        compile.CacheCleanup.deleteTempDir(ctx.io.std_io, temp_dir);
    };

    const platform_dir = try std.fs.path.join(ctx.arena, &.{ temp_dir, ".roc_echo_platform" });
    try std.Io.Dir.cwd().createDirPath(ctx.io.std_io, platform_dir);

    const app_path = try std.fs.path.join(ctx.arena, &.{ temp_dir, "main.roc" });
    const platform_main_path = try std.fs.path.join(ctx.arena, &.{ platform_dir, "main.roc" });
    const echo_module_path = try std.fs.path.join(ctx.arena, &.{ platform_dir, "Echo.roc" });

    try writeDefaultAppSyntheticRunSource(ctx, app_path, original_source);
    try std.Io.Dir.cwd().writeFile(ctx.io.std_io, .{ .sub_path = platform_main_path, .data = echo_platform.run_shim_platform_main_source });
    try std.Io.Dir.cwd().writeFile(ctx.io.std_io, .{ .sub_path = echo_module_path, .data = echo_platform.echo_module_source });

    const original_source_dir = std.fs.path.dirname(args.path) orelse ".";
    var reporter = makeReporter(ctx, "roc", args.timings);
    defer reporter.deinit();
    reporter.start();
    var lowered_result = try lowerLirWithBuildEnv(
        ctx,
        ctx.gpa,
        .{ .dev_run_image = selected_target },
        app_path,
        original_source_dir,
        .{ .original_path = args.path, .original_source = original_source },
        args.max_threads,
        args.opt,
        resolutionConfigFromLimits(args.resolve_limits),
        !args.no_cache,
        &reporter,
        true,
    );
    defer lowered_result.deinit();

    if (lowered_result.counts.errors > 0 and lowered_result.lowered == null) {
        reporter.fail();
        if (args.allow_errors) return;
        return error.TypeCheckingFailed;
    }
    reporter.finish();

    const entrypoint_names = lowered_result.entrypoint_names;
    if (entrypoint_names.len == 0) {
        if (builtin.mode == .Debug) {
            std.debug.panic("default app run invariant violated: no platform entrypoints", .{});
        }
        unreachable;
    }

    const lowered = successfulLoweredProgram(&lowered_result, "default app run");
    const enable_debug = builtin.mode == .Debug;
    const exe_checked_host_identity = defaultRunCheckedHostIdentity(selected_target, entrypoint_names, lowered_result.hosted_symbols);
    const libc_info: ?libc_finder.LibcInfo = if (selected_target.isDynamic())
        libc_finder.findLibc(ctx) catch |err| {
            try ctx.io.stderr().print(
                "Error: could not find system libc for shared-memory default app run: {}\n",
                .{err},
            );
            return err;
        }
    else
        null;
    const link_inputs_identity = (try defaultRunLinkInputsIdentity(ctx, selected_target, libc_info)) orelse {
        return rejectRunTargetNotExecutable(ctx, selected_target);
    };

    const exe_cache_name = try shimHostExeCacheName(ctx, .{
        .shim_kind = .machine_code,
        .target = selected_target,
        .debug = enable_debug,
        .checked_host_identity = exe_checked_host_identity,
        .link_inputs_identity = link_inputs_identity,
    });
    const exe_cache_name_with_ext = if (builtin.target.os.tag == .windows)
        try std.fmt.allocPrint(ctx.arena, "{s}.exe", .{exe_cache_name})
    else
        try ctx.arena.dupe(u8, exe_cache_name);
    const exe_cache_path = try std.fs.path.join(ctx.arena, &.{ exe_cache_dir, exe_cache_name_with_ext });

    const exe_display_name = std.fs.path.basename(args.path);
    const exe_display_name_with_ext = if (builtin.target.os.tag == .windows)
        try std.fmt.allocPrint(ctx.arena, "{s}.exe", .{exe_display_name})
    else
        try ctx.arena.dupe(u8, exe_display_name);
    const exe_path = try std.fs.path.join(ctx.arena, &.{ temp_dir, exe_display_name_with_ext });

    const cache_exists = if (args.no_cache) false else blk: {
        std.Io.Dir.cwd().access(ctx.io.std_io, exe_cache_path, .{}) catch break :blk false;
        break :blk true;
    };

    if (cache_exists) {
        createHardlink(ctx, exe_cache_path, exe_path) catch {
            try std.Io.Dir.cwd().copyFile(exe_cache_path, std.Io.Dir.cwd(), exe_path, ctx.io.std_io, .{});
        };
    } else {
        const shim_filename = if (builtin.target.os.tag == .windows) "roc_machine_code_shim.lib" else "libroc_machine_code_shim.a";
        const shim_path = try std.fs.path.join(ctx.arena, &.{ temp_dir, shim_filename });
        extractShimLibrary(ctx, .machine_code, shim_path, selected_target) catch |err| {
            return ctx.fail(.{ .shim_generation_failed = .{ .err = err } });
        };

        const platform_entrypoints = try lowered.platformEntrypoints(ctx.gpa);
        defer ctx.gpa.free(platform_entrypoints);
        const platform_shim_path = (try generatePlatformHostShimFromLirData(
            ctx,
            temp_dir,
            entrypoint_names,
            lowered_result.hosted_symbols,
            selected_target,
            &lowered.lir_result.store,
            &lowered.lir_result.layouts,
            platform_entrypoints,
            null,
            0,
            true,
            enable_debug,
        )) orelse return ctx.fail(.{ .shim_generation_failed = .{ .err = error.LLVMCompilationFailed } });

        const runtime_path = (try writeDefaultPlatformRuntimeObject(ctx, temp_dir, selected_target)) orelse {
            return rejectRunTargetNotExecutable(ctx, selected_target);
        };

        const object_files = [_][]const u8{
            platform_shim_path,
            shim_path,
            runtime_path,
        };
        var extra_args = try std.array_list.Managed([]const u8).initCapacity(ctx.arena, 5);
        if (libc_info) |info| {
            try extra_args.append("-dynamic-linker");
            try extra_args.append(info.dynamic_linker);
            try extra_args.append("-L");
            try extra_args.append(info.lib_dir);
            try extra_args.append("-lc");
        }

        const link_config = linker.LinkConfig{
            .target_format = linker.TargetFormat.detectFromOs(selected_target.toOsTag()),
            .target_abi = if (selected_target.isStatic()) .musl else .gnu,
            .target_os = selected_target.toOsTag(),
            .target_arch = selected_target.toCpuArch(),
            .output_path = exe_path,
            .object_files = &object_files,
            .can_exit_early = false,
            .disable_output = false,
            .scratch_dir = temp_dir,
            .extra_args = extra_args.items,
        };

        linker.link(ctx, link_config) catch |err| {
            return ctx.fail(.{ .linker_failed = .{
                .err = err,
                .target = @tagName(selected_target),
            } });
        };

        std.Io.Dir.cwd().deleteFile(ctx.io.std_io, exe_cache_path) catch |err| switch (err) {
            error.FileNotFound => {},
            else => std.log.debug("Could not delete existing cache file: {}", .{err}),
        };
        createHardlink(ctx, exe_path, exe_cache_path) catch {
            std.Io.Dir.cwd().copyFile(exe_path, std.Io.Dir.cwd(), exe_cache_path, ctx.io.std_io, .{}) catch |copy_err| {
                std.log.debug("Failed to copy default run executable to cache: {}", .{copy_err});
            };
        };
    }

    // Headerless default apps never hot reload — they compile through throwaway synthetic
    // source files, so there is nothing stable to reload. They just run once.
    const internal_static_data = successfulInternalStaticData(&lowered_result, "default app run");
    const shm_handle = try publishDevRunImage(ctx, selected_target, entrypoint_names, lowered, internal_static_data, false);
    defer closeSharedMemoryHandle(shm_handle);

    if (comptime is_windows) {
        try runWithWindowsHandleInheritance(ctx, exe_path, shm_handle, args.app_args);
    } else {
        try runWithPosixFdInheritance(ctx, exe_path, shm_handle, args.app_args);
    }
    cleanup_temp_dir = false;

    exitOnWarnings(ctx, lowered_result.counts.warnings);
}

/// Append an argument to a command line buffer with proper Windows quoting.
/// Windows command line parsing rules:
/// - Arguments containing spaces, tabs, or quotes must be quoted
/// - Embedded quotes must be escaped with backslash: " -> \"
/// - Backslashes before quotes must be doubled: \" -> \\"
fn appendWindowsQuotedArg(cmd_builder: *std.array_list.Managed(u8), arg: []const u8) Allocator.Error!void {
    const needs_quoting = arg.len == 0 or std.mem.findAny(u8, arg, " \t\"") != null;

    if (!needs_quoting) {
        try cmd_builder.appendSlice(arg);
        return;
    }

    try cmd_builder.append('"');
    var backslash_count: usize = 0;
    for (arg) |char| {
        if (char == '\\') {
            backslash_count += 1;
        } else if (char == '"') {
            // Double all backslashes before quote, then escape the quote
            // N backslashes + " -> 2N backslashes + \"
            for (0..backslash_count * 2) |_| try cmd_builder.append('\\');
            backslash_count = 0;
            try cmd_builder.appendSlice("\\\"");
        } else {
            // Emit accumulated backslashes as-is (not before a quote)
            for (0..backslash_count) |_| try cmd_builder.append('\\');
            backslash_count = 0;
            try cmd_builder.append(char);
        }
    }
    // Double any trailing backslashes before closing quote
    for (0..backslash_count * 2) |_| try cmd_builder.append('\\');
    try cmd_builder.append('"');
}

/// Run child process using Windows handle inheritance (idiomatic Windows approach)
fn runWithWindowsHandleInheritance(ctx: *CliCtx, exe_path: []const u8, shm_handle: SharedMemoryHandle, app_args: []const []const u8) (CliError || error{OutOfMemory})!void {
    // Make the shared memory handle inheritable
    if (windows.SetHandleInformation(@ptrCast(shm_handle.fd), windows.HANDLE_FLAG_INHERIT, windows.HANDLE_FLAG_INHERIT) == 0) {
        return ctx.fail(.{ .shared_memory_failed = .{
            .operation = "set handle inheritable",
            .err = error.HandleInheritanceFailed,
        } });
    }

    // Convert paths to Windows wide strings
    const exe_path_w = std.unicode.utf8ToUtf16LeAllocZ(ctx.arena, exe_path) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        error.InvalidUtf8 => return ctx.fail(.{ .child_process_spawn_failed = .{
            .command = exe_path,
            .err = err,
        } }),
    };

    const cwd = std.Io.Dir.cwd().realPathFileAlloc(ctx.io.std_io, ".", ctx.arena) catch {
        return ctx.fail(.{ .directory_not_found = .{
            .path = ".",
        } });
    };
    const cwd_w = std.unicode.utf8ToUtf16LeAllocZ(ctx.arena, cwd) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        error.InvalidUtf8 => return ctx.fail(.{ .directory_not_found = .{
            .path = cwd,
        } }),
    };

    // Hand the shared-memory handle and size to the child out of band, the same
    // way the POSIX path does.
    //
    // These used to be passed as the first two command-line arguments. That put
    // them in the child's argv, where the platform host has no way to tell them
    // apart from the user's arguments and passes them straight through to the
    // Roc application: `roc --opt=interpreter app.roc` gave the app
    // `["...app.roc.exe", "448", "976608"]` instead of just the executable path.
    // No platform host can be expected to know about roc's IPC plumbing, and
    // none of them compensate for it, so keep argv clean instead.
    writeFdCoordinationFile(ctx, exe_path, shm_handle) catch |err| {
        const temp_dir = std.fs.path.dirname(exe_path) orelse exe_path;
        const fd_file_path = std.fmt.allocPrint(ctx.arena, "{s}.txt", .{temp_dir}) catch exe_path;
        return ctx.fail(.{ .file_write_failed = .{
            .path = fd_file_path,
            .err = err,
        } });
    };

    // Build command line string with proper quoting for Windows
    var cmd_builder = std.array_list.Managed(u8).initCapacity(ctx.gpa, 256) catch {
        return error.OutOfMemory;
    };
    defer cmd_builder.deinit();
    try cmd_builder.print("\"{s}\"", .{exe_path});
    for (app_args) |arg| {
        try cmd_builder.append(' ');
        try appendWindowsQuotedArg(&cmd_builder, arg);
    }
    try cmd_builder.append(0); // null terminator for sentinel

    const cmd_line = cmd_builder.items[0 .. cmd_builder.items.len - 1 :0];
    const cmd_line_w = std.unicode.utf8ToUtf16LeAllocZ(ctx.arena, cmd_line) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        error.InvalidUtf8 => return ctx.fail(.{ .child_process_spawn_failed = .{
            .command = exe_path,
            .err = err,
        } }),
    };

    // Set up process creation structures
    var startup_info = std.mem.zeroes(windows.STARTUPINFOW);
    startup_info.cb = @sizeOf(windows.STARTUPINFOW);

    var process_info = std.mem.zeroes(windows.PROCESS_INFORMATION);

    // Create the child process with handle inheritance

    // Create the child process with handle inheritance enabled
    const success = windows.CreateProcessW(
        exe_path_w.ptr, // Application name
        cmd_line_w.ptr, // Command line (mutable)
        null, // Process attributes
        null, // Thread attributes
        1, // bInheritHandles = TRUE
        0, // Creation flags
        null, // Environment
        cwd_w.ptr, // Current directory
        &startup_info, // Startup info
        &process_info, // Process info
    );

    if (success == 0) {
        const last_error = std.os.windows.GetLastError();
        std.log.err("CreateProcessW failed with Windows error code: {}", .{last_error});
        std.log.err("exe_path: {s}", .{exe_path});
        std.log.err("cmd_line: {s}", .{cmd_builder.items[0 .. cmd_builder.items.len - 1]});
        std.log.err("cwd: {s}", .{cwd});
        return ctx.fail(.{ .child_process_spawn_failed = .{
            .command = exe_path,
            .err = error.ProcessCreationFailed,
        } });
    }

    // Child process spawned successfully

    // Wait for the child process to complete
    std.log.debug("Waiting for child process to complete: {s}", .{exe_path});
    const wait_result = windows.WaitForSingleObject(process_info.hProcess, windows.INFINITE);
    if (wait_result != 0) { // WAIT_OBJECT_0 = 0
        // Clean up handles before returning
        _ = ipc.platform.windows.CloseHandle(process_info.hProcess);
        _ = ipc.platform.windows.CloseHandle(process_info.hThread);
        return ctx.fail(.{ .child_process_wait_failed = .{
            .command = exe_path,
            .err = error.ProcessWaitFailed,
        } });
    }

    // Get the exit code
    var exit_code: windows.DWORD = undefined;
    if (windows.GetExitCodeProcess(process_info.hProcess, &exit_code) == 0) {
        // Clean up handles before returning
        _ = ipc.platform.windows.CloseHandle(process_info.hProcess);
        _ = ipc.platform.windows.CloseHandle(process_info.hThread);
        return ctx.fail(.{ .child_process_wait_failed = .{
            .command = exe_path,
            .err = error.ProcessExitCodeFailed,
        } });
    }

    // Clean up process handles
    _ = ipc.platform.windows.CloseHandle(process_info.hProcess);
    _ = ipc.platform.windows.CloseHandle(process_info.hThread);

    // On Windows, clean up temp files after the child process exits.
    // (Unlike Unix, Windows locks files while they're being executed)
    if (std.fs.path.dirname(exe_path)) |temp_dir_path| {
        compile.CacheCleanup.deleteTempDir(ctx.io.std_io, temp_dir_path);
        std.log.debug("Cleaned up temp directory: {s}", .{temp_dir_path});
    }

    // Check exit code and propagate to parent
    if (exit_code != 0) {
        std.log.debug("Child process {s} exited with code: {}", .{ exe_path, exit_code });
        if (exit_code == 0xC0000005) { // STATUS_ACCESS_VIOLATION
            const result = platform_validation.targets_validator.ValidationResult{
                .process_crashed = .{ .exit_code = exit_code, .is_access_violation = true },
            };
            renderValidationError(ctx.gpa, result, ctx.io.stderr());
        } else if (exit_code >= 0xC0000000) { // NT status codes for exceptions
            const result = platform_validation.targets_validator.ValidationResult{
                .process_crashed = .{ .exit_code = exit_code, .is_access_violation = false },
            };
            renderValidationError(ctx.gpa, result, ctx.io.stderr());
        }
        // Propagate the exit code (truncated to u8 for compatibility)
        std.process.exit(@truncate(exit_code));
    }

    std.log.debug("Child process completed successfully", .{});
}

/// Run child process using POSIX file descriptor inheritance (existing approach for Unix)
/// The exe_path should already be in a unique temp directory created by createUniqueTempDir.
fn runWithPosixFdInheritance(ctx: *CliCtx, exe_path: []const u8, shm_handle: SharedMemoryHandle, app_args: []const []const u8) (CliError || error{OutOfMemory})!void {
    // Write the coordination file (.txt) next to the executable
    // The executable is already in a unique temp directory
    std.log.debug("Writing fd coordination file for: {s}", .{exe_path});
    writeFdCoordinationFile(ctx, exe_path, shm_handle) catch |err| {
        // Get the actual .txt file path for error reporting
        const temp_dir = std.fs.path.dirname(exe_path) orelse exe_path;
        const fd_file_path = std.fmt.allocPrint(ctx.arena, "{s}.txt", .{temp_dir}) catch exe_path;
        return ctx.fail(.{ .file_write_failed = .{
            .path = fd_file_path,
            .err = err,
        } });
    };
    std.log.debug("Coordination file written successfully", .{});

    // Configure fd inheritance - clear FD_CLOEXEC so child process inherits the fd
    const current_flags = std.c.fcntl(shm_handle.fd, std.c.F.GETFD);
    if (current_flags == -1) {
        return ctx.fail(.{ .shared_memory_failed = .{
            .operation = "get fd flags",
            .err = error.FdConfigFailed,
        } });
    }

    // Clear FD_CLOEXEC - the flag value is 1
    const new_flags = current_flags & ~@as(c_int, 1);
    if (std.c.fcntl(shm_handle.fd, std.c.F.SETFD, new_flags) == -1) {
        return ctx.fail(.{ .shared_memory_failed = .{
            .operation = "set fd flags",
            .err = error.FdConfigFailed,
        } });
    }

    // Debug-only verification that fd flags were actually cleared
    if (comptime builtin.mode == .Debug) {
        const verify_flags = std.c.fcntl(shm_handle.fd, std.c.F.GETFD);
        if (verify_flags == -1) {
            return ctx.fail(.{ .shared_memory_failed = .{
                .operation = "verify fd flags",
                .err = error.FdConfigFailed,
            } });
        }
        if ((verify_flags & 1) != 0) {
            return ctx.fail(.{ .shared_memory_failed = .{
                .operation = "clear FD_CLOEXEC",
                .err = error.FdConfigFailed,
            } });
        }
        std.log.debug("fd={} FD_CLOEXEC cleared successfully", .{shm_handle.fd});
    }

    // Build argv slice using arena allocator (memory lives until arena is freed)
    const argv = ctx.arena.alloc([]const u8, 1 + app_args.len) catch {
        return error.OutOfMemory;
    };
    argv[0] = exe_path;
    for (app_args, 0..) |arg, i| {
        argv[1 + i] = arg;
    }

    std.log.debug("Spawning child process: {s} with {} app args", .{ exe_path, app_args.len });
    std.log.debug("Child process inherits current working directory", .{});
    var child = std.process.spawn(ctx.io.std_io, .{
        .argv = argv,
        .cwd = .inherit,
        .stdout = .inherit,
        .stderr = .inherit,
    }) catch |err| {
        return ctx.fail(.{ .child_process_spawn_failed = .{
            .command = exe_path,
            .err = err,
        } });
    };
    std.log.debug("Child process spawned successfully (PID: {})", .{child.id});

    // Wait for child to complete
    const term = child.wait(ctx.io.std_io) catch |err| {
        return ctx.fail(.{ .child_process_wait_failed = .{
            .command = exe_path,
            .err = err,
        } });
    };

    // Clean up temp files after child has exited.
    // We wait until after child exits because the child needs to read the coordination
    // file to find the shared memory before it can run.
    // The background cleanup thread will also clean up old temp directories.
    if (std.fs.path.dirname(exe_path)) |temp_dir_path| {
        compile.CacheCleanup.deleteTempDir(ctx.io.std_io, temp_dir_path);
        std.log.debug("Cleaned up temp directory: {s}", .{temp_dir_path});
    }

    // Check the termination status
    switch (term) {
        .exited => |exit_code| {
            if (exit_code == 0) {
                std.log.debug("Child process completed successfully", .{});
            } else {
                // Propagate the exit code from the child process to our parent
                std.log.debug("Child process {s} exited with code: {}", .{ exe_path, exit_code });
                std.process.exit(exit_code);
            }
        },
        .signal => |signal| {
            const sig_num = @intFromEnum(signal);
            std.log.debug("Child process {s} killed by signal: {}", .{ exe_path, sig_num });
            const result = platform_validation.targets_validator.ValidationResult{
                .process_signaled = .{ .signal = sig_num },
            };
            renderValidationError(ctx.gpa, result, ctx.io.stderr());
            // Standard POSIX convention: exit with 128 + signal number
            std.process.exit(128 +| @as(u8, @truncate(sig_num)));
        },
        .stopped => |signal| {
            return ctx.fail(.{ .child_process_signaled = .{
                .command = exe_path,
                .signal = @intFromEnum(signal),
            } });
        },
        .unknown => |status| {
            return ctx.fail(.{ .child_process_failed = .{
                .command = exe_path,
                .exit_code = status,
            } });
        },
    }
}

const HotShimChild = struct {
    child: std.process.Child,
    thread: std.Thread,
    done: std.atomic.Value(bool) = std.atomic.Value(bool).init(false),
    term: ?std.process.Child.Term = null,
    wait_error: ?std.process.Child.WaitError = null,

    fn waitThread(self: *HotShimChild, io: std.Io) void {
        self.term = self.child.wait(io) catch |err| {
            self.wait_error = err;
            self.done.store(true, .seq_cst);
            return;
        };
        self.done.store(true, .seq_cst);
    }
};

fn makeSharedMemoryHandleInheritable(ctx: *CliCtx, shm_handle: SharedMemoryHandle) CliError!void {
    if (comptime is_windows) {
        if (windows.SetHandleInformation(@ptrCast(shm_handle.fd), windows.HANDLE_FLAG_INHERIT, windows.HANDLE_FLAG_INHERIT) == 0) {
            return ctx.fail(.{ .shared_memory_failed = .{
                .operation = "set handle inheritable",
                .err = error.HandleInheritanceFailed,
            } });
        }
    } else {
        const current_flags = std.c.fcntl(shm_handle.fd, std.c.F.GETFD);
        if (current_flags == -1) {
            return ctx.fail(.{ .shared_memory_failed = .{
                .operation = "get fd flags",
                .err = error.FdConfigFailed,
            } });
        }

        const new_flags = current_flags & ~@as(c_int, 1);
        if (std.c.fcntl(shm_handle.fd, std.c.F.SETFD, new_flags) == -1) {
            return ctx.fail(.{ .shared_memory_failed = .{
                .operation = "set fd flags",
                .err = error.FdConfigFailed,
            } });
        }
    }
}

fn spawnHotShimChild(
    ctx: *CliCtx,
    exe_path: []const u8,
    shm_handle: SharedMemoryHandle,
    app_args: []const []const u8,
) (CliError || Allocator.Error || std.Thread.SpawnError || std.process.SpawnError)!*HotShimChild {
    // Every platform hands the shared-memory handle and size to the child
    // through the coordination file. Windows used to append them to argv
    // instead, which leaked roc's IPC plumbing into the Roc application's
    // arguments -- see the note in `runWithWindowsHandleInheritance`.
    writeFdCoordinationFile(ctx, exe_path, shm_handle) catch |err| {
        const temp_dir = std.fs.path.dirname(exe_path) orelse exe_path;
        const fd_file_path = std.fmt.allocPrint(ctx.arena, "{s}.txt", .{temp_dir}) catch exe_path;
        return ctx.fail(.{ .file_write_failed = .{
            .path = fd_file_path,
            .err = err,
        } });
    };
    try makeSharedMemoryHandleInheritable(ctx, shm_handle);

    const argv = try ctx.arena.alloc([]const u8, 1 + app_args.len);
    argv[0] = exe_path;
    for (app_args, 0..) |arg, i| {
        argv[1 + i] = arg;
    }

    const child = std.process.spawn(ctx.io.std_io, .{
        .argv = argv,
        .cwd = .inherit,
        .stdout = .inherit,
        .stderr = .inherit,
    }) catch |err| {
        return ctx.fail(.{ .child_process_spawn_failed = .{
            .command = exe_path,
            .err = err,
        } });
    };

    const watched = try ctx.gpa.create(HotShimChild);
    watched.* = .{
        .child = child,
        .thread = undefined,
    };
    watched.thread = std.Thread.spawn(.{}, HotShimChild.waitThread, .{ watched, ctx.io.std_io }) catch |err| {
        watched.child.kill(ctx.io.std_io);
        ctx.gpa.destroy(watched);
        return err;
    };
    return watched;
}

fn terminateHotShimChild(child: *HotShimChild) void {
    if (child.child.id) |pid| {
        switch (builtin.os.tag) {
            .windows => {
                _ = std.os.windows.ntdll.NtTerminateProcess(pid, @enumFromInt(1));
            },
            .wasi => {},
            else => std.posix.kill(pid, .KILL) catch {},
        }
    }
}

fn destroyHotShimChild(ctx: *CliCtx, child: *HotShimChild) void {
    ctx.gpa.destroy(child);
}

const HotReloadRebuild = struct {
    child: *WatchChild,
    argv: WatchChildArgv,
    inputs_path: []const u8,
    generation: u64,
    allocation: HotReloadImageAllocation,
    published_reported: bool = false,

    fn cancelAndJoin(self: *HotReloadRebuild) void {
        terminateWatchChild(self.child);
        joinWatchChild(self.child);
    }

    fn deinit(self: *HotReloadRebuild, ctx: *CliCtx) void {
        destroyWatchChild(ctx, self.child);
        self.argv.deinit(ctx.gpa);
        std.Io.Dir.cwd().deleteFile(ctx.io.std_io, self.inputs_path) catch {};
        ctx.gpa.free(self.inputs_path);
    }
};

const HotReloadSourceRewrite = struct {
    source_path: []const u8,
    synthetic_app_path: []const u8,
    source_dir_override: []const u8,
};

const HotReloadFreeRegion = struct {
    start: usize,
    end: usize,

    fn len(self: HotReloadFreeRegion) usize {
        return self.end - self.start;
    }
};

const HotReloadDescriptorSlot = struct {
    offset: usize,
    preserve_refs: bool,
    fresh: bool,
};

const HotReloadImageAllocation = struct {
    generation: u64,
    descriptor_offset: usize,
    image_limit: usize,
    region_start: usize,
    region_end: usize,
    append_offset: usize,
    preserve_descriptor_refs: bool = false,
};

const HotReloadDescriptorTracker = struct {
    descriptors: std.ArrayList(usize) = .empty,
    free_descriptor_slots: std.ArrayList(HotReloadDescriptorSlot) = .empty,
    free_regions: std.ArrayList(HotReloadFreeRegion) = .empty,
    descriptor_floor: usize = 0,
    next_descriptor_offset: usize = 0,

    fn deinit(self: *HotReloadDescriptorTracker, gpa: Allocator) void {
        self.descriptors.deinit(gpa);
        self.free_descriptor_slots.deinit(gpa);
        self.free_regions.deinit(gpa);
    }
};

fn buildHotReloadChildArgv(
    ctx: *CliCtx,
    arg0: []const u8,
    args: cli_args.RunArgs,
    selected_target: RocTarget,
    shm_handle: SharedMemoryHandle,
    expected_host_identity: [32]u8,
    generation: u64,
    allocation: HotReloadImageAllocation,
    inputs_path: []const u8,
    source_rewrite: ?HotReloadSourceRewrite,
) Allocator.Error!WatchChildArgv {
    var argv = std.ArrayList([]const u8).empty;
    errdefer argv.deinit(ctx.gpa);
    var owned = std.ArrayList([]const u8).empty;
    errdefer {
        for (owned.items) |arg| ctx.gpa.free(arg);
        owned.deinit(ctx.gpa);
    }

    const expected_hex = std.fmt.bytesToHex(expected_host_identity, .lower);

    try argv.append(ctx.gpa, arg0);
    try argv.append(ctx.gpa, hot_reload_dev_command);
    try appendOwnedArg(ctx.gpa, &argv, &owned, "--path={s}", .{args.path});
    try appendOwnedArg(ctx.gpa, &argv, &owned, "--target={s}", .{@tagName(selected_target)});
    try appendOwnedArg(ctx.gpa, &argv, &owned, "--generation={}", .{generation});
    try appendOwnedArg(ctx.gpa, &argv, &owned, "--descriptor-offset={}", .{allocation.descriptor_offset});
    try appendOwnedArg(ctx.gpa, &argv, &owned, "--image-limit={}", .{allocation.image_limit});
    try appendOwnedArg(ctx.gpa, &argv, &owned, "--region-start={}", .{allocation.region_start});
    try appendOwnedArg(ctx.gpa, &argv, &owned, "--region-end={}", .{allocation.region_end});
    try appendOwnedArg(ctx.gpa, &argv, &owned, "--append-offset={}", .{allocation.append_offset});
    try appendOwnedArg(ctx.gpa, &argv, &owned, "--preserve-descriptor-refs={}", .{if (allocation.preserve_descriptor_refs) @as(u8, 1) else @as(u8, 0)});
    if (comptime is_windows) {
        try appendOwnedArg(ctx.gpa, &argv, &owned, "--shm-handle={}", .{@intFromPtr(shm_handle.fd)});
    } else {
        try appendOwnedArg(ctx.gpa, &argv, &owned, "--shm-handle={}", .{shm_handle.fd});
    }
    try appendOwnedArg(ctx.gpa, &argv, &owned, "--shm-size={}", .{shm_handle.mapped_size});
    try appendOwnedArg(ctx.gpa, &argv, &owned, "--expected-host={s}", .{expected_hex[0..]});
    try appendOwnedArg(ctx.gpa, &argv, &owned, "--watch-inputs-file={s}", .{inputs_path});
    if (source_rewrite) |rewrite| {
        try appendOwnedArg(ctx.gpa, &argv, &owned, "--synthetic-source={s}", .{rewrite.source_path});
        try appendOwnedArg(ctx.gpa, &argv, &owned, "--synthetic-output={s}", .{rewrite.synthetic_app_path});
        try appendOwnedArg(ctx.gpa, &argv, &owned, "--source-dir={s}", .{rewrite.source_dir_override});
    }
    if (args.max_threads) |jobs| try appendOwnedArg(ctx.gpa, &argv, &owned, "--jobs={}", .{jobs});
    if (args.no_cache) try appendOwnedArg(ctx.gpa, &argv, &owned, "--no-cache={}", .{@as(u8, 1)});
    try appendResolveLimitArgs(ctx.gpa, &argv, &owned, args.resolve_limits);

    return .{
        .argv = try argv.toOwnedSlice(ctx.gpa),
        .owned = try owned.toOwnedSlice(ctx.gpa),
    };
}

fn spawnHotReloadRebuild(
    ctx: *CliCtx,
    arg0: []const u8,
    args: cli_args.RunArgs,
    selected_target: RocTarget,
    shm_handle: SharedMemoryHandle,
    expected_host_identity: [32]u8,
    generation: u64,
    allocation: HotReloadImageAllocation,
    source_rewrite: ?HotReloadSourceRewrite,
) CliMainError!HotReloadRebuild {
    try makeSharedMemoryHandleInheritable(ctx, shm_handle);

    const inputs_path = try createWatchInputsPath(ctx);
    errdefer ctx.gpa.free(inputs_path);
    errdefer std.Io.Dir.cwd().deleteFile(ctx.io.std_io, inputs_path) catch {};

    var argv = try buildHotReloadChildArgv(ctx, arg0, args, selected_target, shm_handle, expected_host_identity, generation, allocation, inputs_path, source_rewrite);
    errdefer argv.deinit(ctx.gpa);

    const child = try spawnWatchChild(ctx, argv.argv);
    errdefer {
        terminateWatchChild(child);
        joinWatchChild(child);
        destroyWatchChild(ctx, child);
    }

    return .{
        .child = child,
        .argv = argv,
        .inputs_path = inputs_path,
        .generation = generation,
        .allocation = allocation,
    };
}

fn hotReloadRebuildSucceeded(child: *const WatchChild) bool {
    const term = child.term orelse return false;
    return switch (term) {
        .exited => |code| code == 0,
        .signal, .stopped, .unknown => false,
    };
}

fn finishHotReloadRebuild(
    ctx: *CliCtx,
    state: *WatchState,
    signal: *WatchEventSignal,
    rebuild: *HotReloadRebuild,
) CliMainError!bool {
    joinWatchChild(rebuild.child);
    if (rebuild.child.output_error) |err| return err;

    try replayWatchChildOutput(ctx, rebuild.child, true);

    const rebuild_succeeded = hotReloadRebuildSucceeded(rebuild.child);
    const new_inputs = if (rebuild_succeeded)
        try readWatchInputsFileAfterChild(ctx, rebuild.inputs_path, &.{})
    else
        readWatchInputsFile(ctx, rebuild.inputs_path, &.{}) catch |err| switch (err) {
            error.WatchInputsMissing,
            error.WatchInputsReadFailed,
            error.WatchInputsMalformed,
            => return false,
            else => |e| return e,
        };
    const changed_during_refresh = try refreshWatchState(ctx, state, signal, new_inputs);

    if (rebuild_succeeded) {
        try reportHotReloadRebuildPublished(ctx, rebuild);
    }

    return changed_during_refresh;
}

fn reportHotReloadRebuildPublished(ctx: *CliCtx, rebuild: *HotReloadRebuild) CliOutputWriteError!void {
    if (rebuild.published_reported) return;
    try ctx.io.stderr().print("--- roc watch: hot reload generation {} published ---\n", .{rebuild.generation});
    ctx.io.flush();
    rebuild.published_reported = true;
}

fn reportHotReloadAcknowledgement(
    ctx: *CliCtx,
    control: *const ipc.hot_reload.Control,
    last_reported_ack: *u64,
    active_rebuild: ?*HotReloadRebuild,
) CliOutputWriteError!?u64 {
    const ack = ipc.hot_reload.acknowledgement(control) orelse return null;
    const generation = ack.generation;
    if (generation == 0 or generation <= last_reported_ack.*) return null;

    if (active_rebuild) |rebuild| {
        if (generation == rebuild.generation and ack.status != .none) {
            try reportHotReloadRebuildPublished(ctx, rebuild);
        }
    }

    switch (ack.status) {
        .none => return null,
        .accepted => try ctx.io.stderr().print("--- roc watch: hot reload generation {} accepted by host ---\n", .{generation}),
        .rejected => try ctx.io.stderr().print("--- roc watch: hot reload generation {} rejected by host; previous code remains active ---\n", .{generation}),
    }
    ctx.io.flush();
    last_reported_ack.* = generation;
    return generation;
}

fn hotReloadRecycleUnpublishedAllocation(
    gpa: Allocator,
    control: *ipc.hot_reload.Control,
    tracker: *HotReloadDescriptorTracker,
    allocation: HotReloadImageAllocation,
) Allocator.Error!void {
    if (ipc.hot_reload.publishedImage(control)) |image| {
        if (image.generation == allocation.generation and image.descriptor_offset == allocation.descriptor_offset) {
            try hotReloadTrackDescriptor(gpa, tracker, image.descriptor_offset);
            return;
        }
    }
    try hotReloadReleaseDescriptorSlot(gpa, tracker, allocation.descriptor_offset, allocation.preserve_descriptor_refs);
}

fn hotReloadBasePtr(shm_handle: SharedMemoryHandle) [*]align(1) u8 {
    return @ptrCast(@alignCast(shm_handle.ptr));
}

fn hotReloadTrackDescriptor(
    gpa: Allocator,
    tracker: *HotReloadDescriptorTracker,
    descriptor_offset: usize,
) Allocator.Error!void {
    if (descriptor_offset == ipc.hot_reload.invalid_descriptor_offset) return;
    for (tracker.descriptors.items) |tracked_offset| {
        if (tracked_offset == descriptor_offset) return;
    }
    try tracker.descriptors.append(gpa, descriptor_offset);
}

fn hotReloadDescriptorSlotSize() usize {
    return std.mem.alignForward(usize, @sizeOf(ipc.hot_reload.ImageDescriptor), @alignOf(ipc.hot_reload.ImageDescriptor));
}

fn hotReloadInitialDescriptorOffset(shm_handle: SharedMemoryHandle) error{InvalidSharedMemory}!usize {
    const descriptor_size = hotReloadDescriptorSlotSize();
    if (shm_handle.mapped_size < @sizeOf(SharedMemoryAllocator.Header) + descriptor_size) return error.InvalidSharedMemory;
    const unaligned = shm_handle.mapped_size - descriptor_size;
    const offset = std.mem.alignBackward(usize, unaligned, @alignOf(ipc.hot_reload.ImageDescriptor));
    if (offset < @sizeOf(SharedMemoryAllocator.Header)) return error.InvalidSharedMemory;
    return offset;
}

fn hotReloadPreviousDescriptorOffset(offset: usize) ?usize {
    const descriptor_size = hotReloadDescriptorSlotSize();
    if (offset < @sizeOf(SharedMemoryAllocator.Header) + descriptor_size) return null;
    return offset - descriptor_size;
}

fn hotReloadInitDescriptorTracker(
    tracker: *HotReloadDescriptorTracker,
    initial_descriptor_offset: usize,
) error{InvalidSharedMemory}!void {
    if (initial_descriptor_offset == ipc.hot_reload.invalid_descriptor_offset) return error.InvalidSharedMemory;
    tracker.descriptor_floor = initial_descriptor_offset;
    tracker.next_descriptor_offset = hotReloadPreviousDescriptorOffset(initial_descriptor_offset) orelse return error.InvalidSharedMemory;
}

fn hotReloadReleaseDescriptorSlot(
    gpa: Allocator,
    tracker: *HotReloadDescriptorTracker,
    descriptor_offset: usize,
    preserve_refs: bool,
) Allocator.Error!void {
    if (descriptor_offset == ipc.hot_reload.invalid_descriptor_offset) return;
    for (tracker.free_descriptor_slots.items) |slot| {
        if (slot.offset == descriptor_offset) return;
    }
    try tracker.free_descriptor_slots.append(gpa, .{
        .offset = descriptor_offset,
        .preserve_refs = preserve_refs,
        .fresh = false,
    });
}

fn hotReloadChooseDescriptorSlot(
    gpa: Allocator,
    tracker: *HotReloadDescriptorTracker,
) (Allocator.Error || error{InvalidSharedMemory})!HotReloadDescriptorSlot {
    if (tracker.free_descriptor_slots.items.len > 0) {
        return tracker.free_descriptor_slots.pop().?;
    }

    // Keep the descriptor list capacity growing in parent-owned memory before
    // handing this slot to a worker. If the worker publishes successfully, the
    // next sweep tracks it through the control block; if it fails, the slot is
    // returned to free_descriptor_slots.
    try tracker.free_descriptor_slots.ensureUnusedCapacity(gpa, 1);

    const offset = tracker.next_descriptor_offset;
    if (offset == ipc.hot_reload.invalid_descriptor_offset) return error.InvalidSharedMemory;
    const next_offset = hotReloadPreviousDescriptorOffset(offset) orelse return error.InvalidSharedMemory;
    tracker.descriptor_floor = offset;
    tracker.next_descriptor_offset = next_offset;

    return .{
        .offset = offset,
        .preserve_refs = false,
        .fresh = true,
    };
}

fn hotReloadReturnDescriptorSlotAfterFailedChoice(
    gpa: Allocator,
    tracker: *HotReloadDescriptorTracker,
    slot: HotReloadDescriptorSlot,
) Allocator.Error!void {
    if (slot.fresh and tracker.descriptor_floor == slot.offset) {
        const descriptor_size = hotReloadDescriptorSlotSize();
        tracker.descriptor_floor = slot.offset + descriptor_size;
        tracker.next_descriptor_offset = slot.offset;
        return;
    }

    try hotReloadReleaseDescriptorSlot(gpa, tracker, slot.offset, slot.preserve_refs);
}

fn hotReloadDescriptor(
    shm_handle: SharedMemoryHandle,
    descriptor_offset: usize,
) error{InvalidSharedMemory}!*ipc.hot_reload.ImageDescriptor {
    return ipc.hot_reload.descriptorFromOffset(
        hotReloadBasePtr(shm_handle),
        shm_handle.mapped_size,
        descriptor_offset,
    ) orelse error.InvalidSharedMemory;
}

fn hotReloadDescriptorForWrite(
    shm: *SharedMemoryAllocator,
    descriptor_offset: usize,
) CliMainError!*ipc.hot_reload.ImageDescriptor {
    if (descriptor_offset == ipc.hot_reload.invalid_descriptor_offset) return error.InvalidSharedMemory;
    if (descriptor_offset > shm.total_size) return error.InvalidSharedMemory;
    if (shm.total_size - descriptor_offset < @sizeOf(ipc.hot_reload.ImageDescriptor)) return error.InvalidSharedMemory;
    if (descriptor_offset % @alignOf(ipc.hot_reload.ImageDescriptor) != 0) return error.InvalidSharedMemory;

    if (comptime is_windows) {
        const commit_result = ipc.platform.windows.VirtualAlloc(
            @ptrCast(shm.base_ptr + descriptor_offset),
            @sizeOf(ipc.hot_reload.ImageDescriptor),
            ipc.platform.windows.MEM_COMMIT,
            ipc.platform.windows.PAGE_READWRITE,
        );
        if (commit_result == null) return error.OutOfMemory;
    }

    return @ptrCast(@alignCast(shm.base_ptr + descriptor_offset));
}

fn hotReloadValidAllocation(
    shm_handle: SharedMemoryHandle,
    reusable_boundary: usize,
    snapshot: ipc.hot_reload.ImageDescriptorSnapshot,
) bool {
    if (snapshot.allocation_start < reusable_boundary) return false;
    if (snapshot.allocation_end <= snapshot.allocation_start) return false;
    if (snapshot.allocation_end > shm_handle.mapped_size) return false;
    if (snapshot.image_offset < snapshot.allocation_start) return false;
    if (snapshot.image_offset >= snapshot.allocation_end) return false;
    if (snapshot.image_size <= snapshot.image_offset) return false;
    if (snapshot.image_size > snapshot.allocation_end) return false;
    return true;
}

fn hotReloadFreeRegionLessThan(_: void, a: HotReloadFreeRegion, b: HotReloadFreeRegion) bool {
    return a.start < b.start or (a.start == b.start and a.end < b.end);
}

fn hotReloadNormalizeFreeRegions(tracker: *HotReloadDescriptorTracker) void {
    std.mem.sort(HotReloadFreeRegion, tracker.free_regions.items, {}, hotReloadFreeRegionLessThan);

    var write_index: usize = 0;
    for (tracker.free_regions.items) |region| {
        if (region.end <= region.start) continue;
        if (write_index > 0 and region.start <= tracker.free_regions.items[write_index - 1].end) {
            tracker.free_regions.items[write_index - 1].end = @max(tracker.free_regions.items[write_index - 1].end, region.end);
        } else {
            tracker.free_regions.items[write_index] = region;
            write_index += 1;
        }
    }
    tracker.free_regions.items.len = write_index;
}

fn hotReloadAddFreeRegion(
    gpa: Allocator,
    tracker: *HotReloadDescriptorTracker,
    shm_handle: SharedMemoryHandle,
    reusable_boundary: usize,
    start: usize,
    end: usize,
) CliMainError!void {
    if (start < reusable_boundary or end <= start or end > shm_handle.mapped_size) return error.InvalidSharedMemory;
    try tracker.free_regions.append(gpa, .{
        .start = start,
        .end = end,
    });
    hotReloadNormalizeFreeRegions(tracker);
}

fn hotReloadSubtractFreeRegion(
    gpa: Allocator,
    tracker: *HotReloadDescriptorTracker,
    start: usize,
    end: usize,
) Allocator.Error!void {
    if (end <= start) return;

    var i: usize = 0;
    while (i < tracker.free_regions.items.len) {
        const region = tracker.free_regions.items[i];
        if (end <= region.start or start >= region.end) {
            i += 1;
            continue;
        }

        const has_left = start > region.start;
        const has_right = end < region.end;

        if (has_left and has_right) {
            tracker.free_regions.items[i].end = start;
            try tracker.free_regions.append(gpa, .{
                .start = end,
                .end = region.end,
            });
            i += 1;
        } else if (has_left) {
            tracker.free_regions.items[i].end = start;
            i += 1;
        } else if (has_right) {
            tracker.free_regions.items[i].start = end;
            i += 1;
        } else {
            _ = tracker.free_regions.swapRemove(i);
        }
    }

    hotReloadNormalizeFreeRegions(tracker);
}

fn hotReloadUsedSizeIncludingFreeRegions(tracker: *const HotReloadDescriptorTracker, used_size: usize) usize {
    var high_water = used_size;
    for (tracker.free_regions.items) |region| {
        high_water = @max(high_water, region.end);
    }
    return high_water;
}

fn hotReloadSweepImageDescriptors(
    gpa: Allocator,
    control: *ipc.hot_reload.Control,
    shm_handle: SharedMemoryHandle,
    reusable_boundary: usize,
    tracker: *HotReloadDescriptorTracker,
) CliMainError!void {
    const current = ipc.hot_reload.publishedImage(control);
    if (current) |image| try hotReloadTrackDescriptor(gpa, tracker, image.descriptor_offset);

    var used_size = reusable_boundary;
    var i: usize = 0;
    while (i < tracker.descriptors.items.len) {
        const descriptor_offset = tracker.descriptors.items[i];
        const descriptor = try hotReloadDescriptor(shm_handle, descriptor_offset);
        const snapshot = ipc.hot_reload.descriptorSnapshot(descriptor);
        if (!hotReloadValidAllocation(shm_handle, reusable_boundary, snapshot)) {
            return error.InvalidSharedMemory;
        }

        const is_current = current != null and
            current.?.descriptor_offset == descriptor_offset and
            current.?.generation == snapshot.generation and
            current.?.image_offset == snapshot.image_offset and
            current.?.image_size == snapshot.image_size;

        if (is_current) {
            if (snapshot.state != .published) return error.InvalidSharedMemory;
            used_size = @max(used_size, snapshot.allocation_end);
            try hotReloadSubtractFreeRegion(gpa, tracker, snapshot.allocation_start, snapshot.allocation_end);
            i += 1;
            continue;
        }

        if (snapshot.refs == 0) {
            ipc.hot_reload.markDescriptorReclaimed(descriptor);
            try hotReloadAddFreeRegion(
                gpa,
                tracker,
                shm_handle,
                reusable_boundary,
                snapshot.allocation_start,
                snapshot.allocation_end,
            );
            try hotReloadReleaseDescriptorSlot(gpa, tracker, descriptor_offset, true);
            _ = tracker.descriptors.swapRemove(i);
        } else {
            ipc.hot_reload.markDescriptorRetired(descriptor);
            used_size = @max(used_size, snapshot.allocation_end);
            try hotReloadSubtractFreeRegion(gpa, tracker, snapshot.allocation_start, snapshot.allocation_end);
            i += 1;
        }
    }

    used_size = hotReloadUsedSizeIncludingFreeRegions(tracker, used_size);
    try SharedMemoryAllocator.rewindMappedHeader(
        hotReloadBasePtr(shm_handle),
        shm_handle.mapped_size,
        used_size,
    );
}

fn hotReloadChooseImageAllocation(
    gpa: Allocator,
    control: *ipc.hot_reload.Control,
    shm_handle: SharedMemoryHandle,
    reusable_boundary: usize,
    tracker: *HotReloadDescriptorTracker,
    generation: u64,
) CliMainError!HotReloadImageAllocation {
    try hotReloadSweepImageDescriptors(gpa, control, shm_handle, reusable_boundary, tracker);

    const append_offset = try SharedMemoryAllocator.mappedHeaderUsedSize(
        hotReloadBasePtr(shm_handle),
        shm_handle.mapped_size,
    );

    var best_region: ?HotReloadFreeRegion = null;
    for (tracker.free_regions.items) |region| {
        if (best_region == null or region.len() > best_region.?.len()) {
            best_region = region;
        }
    }

    const descriptor_slot = try hotReloadChooseDescriptorSlot(gpa, tracker);
    if (best_region == null and tracker.descriptor_floor <= append_offset) {
        try hotReloadReturnDescriptorSlotAfterFailedChoice(gpa, tracker, descriptor_slot);
        return error.OutOfMemory;
    }

    if (best_region) |region| {
        return .{
            .generation = generation,
            .descriptor_offset = descriptor_slot.offset,
            .image_limit = tracker.descriptor_floor,
            .region_start = region.start,
            .region_end = region.end,
            .append_offset = append_offset,
            .preserve_descriptor_refs = descriptor_slot.preserve_refs,
        };
    }

    return .{
        .generation = generation,
        .descriptor_offset = descriptor_slot.offset,
        .image_limit = tracker.descriptor_floor,
        .region_start = 0,
        .region_end = 0,
        .append_offset = append_offset,
        .preserve_descriptor_refs = descriptor_slot.preserve_refs,
    };
}

fn runHotReloadDevShim(
    ctx: *CliCtx,
    arg0: []const u8,
    exe_path: []const u8,
    shm_handle: SharedMemoryHandle,
    args: cli_args.RunArgs,
    selected_target: RocTarget,
    expected_host_identity: [32]u8,
    initial_watch_inputs: []const compile.watch_inputs.Input,
    source_rewrite: ?HotReloadSourceRewrite,
    warning_count: usize,
) CliMainError!void {
    var signal = WatchEventSignal{};
    var state = WatchState{};
    defer state.deinit(ctx);

    var initial_input_set = try collectHotReloadWatchInputSet(ctx, initial_watch_inputs, source_rewrite);
    var initial_input_set_needs_deinit = true;
    errdefer if (initial_input_set_needs_deinit) initial_input_set.deinit(ctx);

    const host_child = try spawnHotShimChild(ctx, exe_path, hotReloadHostChildHandle(shm_handle), args.app_args);
    var host_child_joined = false;
    errdefer {
        if (!host_child_joined) {
            terminateHotShimChild(host_child);
            host_child.thread.join();
        }
        destroyHotShimChild(ctx, host_child);
    }

    initial_input_set_needs_deinit = false;
    var pending_rebuild = try refreshWatchState(ctx, &state, &signal, initial_input_set);
    initial_input_set = .{ .inputs = &.{}, .snapshot = &.{} };

    var next_generation: u64 = 2;
    const hot_reload_control = ipc.hot_reload.controlFromBase(@ptrCast(@alignCast(shm_handle.ptr)));
    const initial_image = ipc.hot_reload.publishedImage(hot_reload_control) orelse return error.InvalidSharedMemory;
    const hot_reload_reclaim_boundary = @sizeOf(SharedMemoryAllocator.Header);
    var hot_reload_tracker = HotReloadDescriptorTracker{};
    defer hot_reload_tracker.deinit(ctx.gpa);
    try hotReloadInitDescriptorTracker(&hot_reload_tracker, initial_image.descriptor_offset);
    try hotReloadTrackDescriptor(ctx.gpa, &hot_reload_tracker, initial_image.descriptor_offset);
    var last_reported_ack = ipc.hot_reload.acknowledgedGeneration(hot_reload_control);
    var active_rebuild: ?HotReloadRebuild = null;
    defer {
        if (active_rebuild) |*rebuild| {
            rebuild.cancelAndJoin();
            rebuild.deinit(ctx);
        }
    }

    while (!host_child.done.load(.seq_cst)) {
        if (try reportHotReloadAcknowledgement(
            ctx,
            hot_reload_control,
            &last_reported_ack,
            if (active_rebuild) |*rebuild| rebuild else null,
        )) |_| {
            if (active_rebuild == null) {
                try hotReloadSweepImageDescriptors(
                    ctx.gpa,
                    hot_reload_control,
                    shm_handle,
                    hot_reload_reclaim_boundary,
                    &hot_reload_tracker,
                );
            }
        }

        if (active_rebuild) |*rebuild| {
            if (rebuild.child.done.load(.seq_cst)) {
                const changed_during_refresh = finishHotReloadRebuild(ctx, &state, &signal, rebuild) catch |err| {
                    try hotReloadRecycleUnpublishedAllocation(ctx.gpa, hot_reload_control, &hot_reload_tracker, rebuild.allocation);
                    rebuild.deinit(ctx);
                    active_rebuild = null;
                    return err;
                };
                pending_rebuild = changed_during_refresh or pending_rebuild;
                if (!hotReloadRebuildSucceeded(rebuild.child)) {
                    try hotReloadRecycleUnpublishedAllocation(ctx.gpa, hot_reload_control, &hot_reload_tracker, rebuild.allocation);
                }
                rebuild.deinit(ctx);
                active_rebuild = null;
                try hotReloadSweepImageDescriptors(
                    ctx.gpa,
                    hot_reload_control,
                    shm_handle,
                    hot_reload_reclaim_boundary,
                    &hot_reload_tracker,
                );
            }
        }

        if (try consumeDebouncedWatchChange(ctx, &signal, &state)) {
            if (active_rebuild) |*rebuild| {
                rebuild.cancelAndJoin();
                try hotReloadRecycleUnpublishedAllocation(ctx.gpa, hot_reload_control, &hot_reload_tracker, rebuild.allocation);
                rebuild.deinit(ctx);
                active_rebuild = null;
            }
            pending_rebuild = true;
        }

        if (pending_rebuild and active_rebuild == null) {
            const allocation = try hotReloadChooseImageAllocation(
                ctx.gpa,
                hot_reload_control,
                shm_handle,
                hot_reload_reclaim_boundary,
                &hot_reload_tracker,
                next_generation,
            );
            active_rebuild = spawnHotReloadRebuild(
                ctx,
                arg0,
                args,
                selected_target,
                shm_handle,
                expected_host_identity,
                next_generation,
                allocation,
                source_rewrite,
            ) catch |err| {
                try hotReloadRecycleUnpublishedAllocation(ctx.gpa, hot_reload_control, &hot_reload_tracker, allocation);
                return err;
            };
            next_generation += 1;
            pending_rebuild = false;
        }

        std.Io.sleep(ctx.io.std_io, std.Io.Duration.fromMilliseconds(watch_debounce_ms), .awake) catch {};
    }

    host_child.thread.join();
    host_child_joined = true;
    defer destroyHotShimChild(ctx, host_child);
    _ = try reportHotReloadAcknowledgement(
        ctx,
        hot_reload_control,
        &last_reported_ack,
        if (active_rebuild) |*rebuild| rebuild else null,
    );

    if (active_rebuild) |*rebuild| {
        rebuild.cancelAndJoin();
        try hotReloadRecycleUnpublishedAllocation(ctx.gpa, hot_reload_control, &hot_reload_tracker, rebuild.allocation);
        rebuild.deinit(ctx);
        active_rebuild = null;
    }

    if (host_child.wait_error) |err| {
        return ctx.fail(.{ .child_process_wait_failed = .{
            .command = exe_path,
            .err = err,
        } });
    }

    if (std.fs.path.dirname(exe_path)) |temp_dir_path| {
        compile.CacheCleanup.deleteTempDir(ctx.io.std_io, temp_dir_path);
        std.log.debug("Cleaned up temp directory: {s}", .{temp_dir_path});
    }

    const term = host_child.term orelse {
        return ctx.fail(.{ .child_process_wait_failed = .{
            .command = exe_path,
            .err = error.ProcessWaitFailed,
        } });
    };
    try finishCompiledRun(ctx, exe_path, term, warning_count);
}

/// Handle for cross-platform shared memory operations.
/// Contains the file descriptor/handle, memory pointer, and size.
pub const SharedMemoryHandle = struct {
    fd: if (is_windows) *anyopaque else c_int,
    ptr: *anyopaque,
    /// The used size of the shared memory (for coordination with child process).
    size: usize,
    /// The total mapped size of the shared memory region (for munmap cleanup).
    /// This may be much larger than `size` since the bump allocator reserves
    /// a large virtual address region upfront.
    mapped_size: usize,
};

fn hotReloadHostChildHandle(handle: SharedMemoryHandle) SharedMemoryHandle {
    var child_handle = handle;
    child_handle.size = handle.mapped_size;
    return child_handle;
}

test "hot reload host child maps the full shared-memory reservation" {
    const fd = if (comptime is_windows)
        @as(*anyopaque, @ptrFromInt(0x1234))
    else
        @as(c_int, 1234);
    const original = SharedMemoryHandle{
        .fd = fd,
        .ptr = @as(*anyopaque, @ptrFromInt(0x5678)),
        .size = 4096,
        .mapped_size = 8192,
    };

    const child = hotReloadHostChildHandle(original);
    try std.testing.expectEqual(original.fd, child.fd);
    try std.testing.expectEqual(original.ptr, child.ptr);
    try std.testing.expectEqual(@as(usize, 8192), child.size);
    try std.testing.expectEqual(@as(usize, 8192), child.mapped_size);
}

test "hot reload worker args require append offset" {
    const zero_host = "0000000000000000000000000000000000000000000000000000000000000000";

    const parsed = try parseHotReloadDevWorkerArgs(&.{
        "--path=app.roc",
        "--target=x64linux",
        "--generation=2",
        "--descriptor-offset=7680",
        "--image-limit=7680",
        "--region-start=1024",
        "--region-end=3072",
        "--append-offset=4096",
        "--preserve-descriptor-refs=1",
        "--shm-handle=3",
        "--shm-size=8192",
        "--expected-host=" ++ zero_host,
        "--watch-inputs-file=watch-inputs",
    });

    try std.testing.expectEqual(@as(u64, 2), parsed.generation);
    try std.testing.expectEqual(@as(usize, 7680), parsed.descriptor_offset);
    try std.testing.expectEqual(@as(usize, 7680), parsed.image_limit);
    try std.testing.expectEqual(@as(usize, 1024), parsed.region_start);
    try std.testing.expectEqual(@as(usize, 3072), parsed.region_end);
    try std.testing.expectEqual(@as(usize, 4096), parsed.append_offset);
    try std.testing.expectEqual(true, parsed.preserve_descriptor_refs);
    try std.testing.expectError(error.InvalidArguments, parseHotReloadDevWorkerArgs(&.{
        "--path=app.roc",
        "--target=x64linux",
        "--generation=2",
        "--descriptor-offset=7680",
        "--image-limit=7680",
        "--region-start=1024",
        "--region-end=3072",
        "--append-offset=4096",
        "--preserve-descriptor-refs=false",
        "--shm-handle=3",
        "--shm-size=8192",
        "--expected-host=" ++ zero_host,
        "--watch-inputs-file=watch-inputs",
    }));
    try std.testing.expectError(error.InvalidArguments, parseHotReloadDevWorkerArgs(&.{
        "--path=app.roc",
        "--target=x64linux",
        "--generation=2",
        "--descriptor-offset=7680",
        "--image-limit=7680",
        "--shm-handle=3",
        "--shm-size=8192",
        "--expected-host=" ++ zero_host,
        "--watch-inputs-file=watch-inputs",
    }));
}

fn testingSharedMemoryHandle(shm: *SharedMemoryAllocator) SharedMemoryHandle {
    return .{
        .fd = shm.handle,
        .ptr = shm.base_ptr,
        .size = shm.getUsedSize(),
        .mapped_size = shm.total_size,
    };
}

fn testingHotReloadDescriptor(shm: *SharedMemoryAllocator, offset: usize) *ipc.hot_reload.ImageDescriptor {
    // Route through the production helper so tests commit the descriptor's page on
    // Windows (SEC_RESERVE leaves it reserved-but-uncommitted) exactly as a real run does.
    return hotReloadDescriptorForWrite(shm, offset) catch unreachable;
}

fn testingPrepareHotReloadDescriptor(
    shm: *SharedMemoryAllocator,
    generation: u64,
    descriptor_offset: usize,
    image_offset: usize,
    image_bound: usize,
    allocation_start: usize,
    allocation_end: usize,
) *ipc.hot_reload.ImageDescriptor {
    const descriptor = testingHotReloadDescriptor(shm, descriptor_offset);
    ipc.hot_reload.prepareDescriptor(
        descriptor,
        generation,
        image_offset,
        image_bound,
        allocation_start,
        allocation_end,
        true,
    );
    return descriptor;
}

fn testingInitHotReloadDescriptorTracker(
    tracker: *HotReloadDescriptorTracker,
    initial_descriptor_offset: usize,
    lowest_descriptor_offset: usize,
) error{InvalidSharedMemory}!void {
    try hotReloadInitDescriptorTracker(tracker, initial_descriptor_offset);
    tracker.descriptor_floor = lowest_descriptor_offset;
    tracker.next_descriptor_offset = hotReloadPreviousDescriptorOffset(lowest_descriptor_offset) orelse return error.InvalidSharedMemory;
}

test "hot reload allocation coalesces adjacent reclaimed image regions" {
    const page_size = try SharedMemoryAllocator.getSystemPageSize();
    var shm = try SharedMemoryAllocator.create(std.testing.io, 64 * 1024, page_size);
    defer shm.deinit(std.testing.allocator);

    const handle = testingSharedMemoryHandle(&shm);
    const desc0_offset = try hotReloadInitialDescriptorOffset(handle);
    const desc1_offset = hotReloadPreviousDescriptorOffset(desc0_offset).?;
    const desc2_offset = hotReloadPreviousDescriptorOffset(desc1_offset).?;

    const control = ipc.hot_reload.controlFromBase(shm.base_ptr);
    const desc0 = testingPrepareHotReloadDescriptor(&shm, 1, desc0_offset, 512, 2048, 512, 2048);
    ipc.hot_reload.init(control, desc0_offset, desc0);
    try SharedMemoryAllocator.rewindMappedHeader(shm.base_ptr, shm.total_size, 2048);

    const desc1 = testingPrepareHotReloadDescriptor(&shm, 2, desc1_offset, 2048, 4096, 2048, 4096);
    ipc.hot_reload.publishDescriptor(control, 2, desc1_offset, desc1);
    try SharedMemoryAllocator.rewindMappedHeader(shm.base_ptr, shm.total_size, 4096);

    const desc2 = testingPrepareHotReloadDescriptor(&shm, 3, desc2_offset, 4096, 8192, 4096, 8192);
    ipc.hot_reload.publishDescriptor(control, 3, desc2_offset, desc2);
    try SharedMemoryAllocator.rewindMappedHeader(shm.base_ptr, shm.total_size, 8192);

    var tracker = HotReloadDescriptorTracker{};
    defer tracker.deinit(std.testing.allocator);
    try testingInitHotReloadDescriptorTracker(&tracker, desc0_offset, desc2_offset);
    try hotReloadTrackDescriptor(std.testing.allocator, &tracker, desc0_offset);
    try hotReloadTrackDescriptor(std.testing.allocator, &tracker, desc1_offset);
    try hotReloadTrackDescriptor(std.testing.allocator, &tracker, desc2_offset);

    const allocation = try hotReloadChooseImageAllocation(std.testing.allocator, control, handle, @sizeOf(SharedMemoryAllocator.Header), &tracker, 4);

    try std.testing.expectEqual(@as(u64, 4), allocation.generation);
    try std.testing.expect(allocation.descriptor_offset == desc0_offset or allocation.descriptor_offset == desc1_offset);
    try std.testing.expectEqual(desc2_offset, allocation.image_limit);
    try std.testing.expectEqual(@as(usize, 512), allocation.region_start);
    try std.testing.expectEqual(@as(usize, 4096), allocation.region_end);
    try std.testing.expectEqual(@as(usize, 8192), allocation.append_offset);
    try std.testing.expectEqual(true, allocation.preserve_descriptor_refs);
    try std.testing.expectEqual(ipc.hot_reload.DescriptorState.reclaimed, ipc.hot_reload.descriptorSnapshot(desc1).state);
    try std.testing.expectEqual(ipc.hot_reload.DescriptorState.published, ipc.hot_reload.descriptorSnapshot(desc2).state);
    try std.testing.expectEqual(@as(usize, 8192), try SharedMemoryAllocator.mappedHeaderUsedSize(shm.base_ptr, shm.total_size));
}

test "hot reload sweep keeps acknowledged current descriptor live" {
    const page_size = try SharedMemoryAllocator.getSystemPageSize();
    var shm = try SharedMemoryAllocator.create(std.testing.io, 64 * 1024, page_size);
    defer shm.deinit(std.testing.allocator);

    const handle = testingSharedMemoryHandle(&shm);
    const desc0_offset = try hotReloadInitialDescriptorOffset(handle);
    const desc1_offset = hotReloadPreviousDescriptorOffset(desc0_offset).?;

    const control = ipc.hot_reload.controlFromBase(shm.base_ptr);
    const desc0 = testingPrepareHotReloadDescriptor(&shm, 1, desc0_offset, 512, 2048, 512, 2048);
    ipc.hot_reload.init(control, desc0_offset, desc0);
    try SharedMemoryAllocator.rewindMappedHeader(shm.base_ptr, shm.total_size, 2048);

    const desc1 = testingPrepareHotReloadDescriptor(&shm, 2, desc1_offset, 2048, 4096, 2048, 4096);
    ipc.hot_reload.publishDescriptor(control, 2, desc1_offset, desc1);
    try SharedMemoryAllocator.rewindMappedHeader(shm.base_ptr, shm.total_size, 4096);
    ipc.hot_reload.acknowledge(control, 2, .accepted);

    var tracker = HotReloadDescriptorTracker{};
    defer tracker.deinit(std.testing.allocator);
    try testingInitHotReloadDescriptorTracker(&tracker, desc0_offset, desc1_offset);
    try hotReloadTrackDescriptor(std.testing.allocator, &tracker, desc0_offset);
    try hotReloadTrackDescriptor(std.testing.allocator, &tracker, desc1_offset);

    const allocation = try hotReloadChooseImageAllocation(std.testing.allocator, control, handle, @sizeOf(SharedMemoryAllocator.Header), &tracker, 3);

    try std.testing.expectEqual(desc0_offset, allocation.descriptor_offset);
    try std.testing.expectEqual(desc1_offset, allocation.image_limit);
    try std.testing.expectEqual(@as(usize, 512), allocation.region_start);
    try std.testing.expectEqual(@as(usize, 2048), allocation.region_end);
    try std.testing.expectEqual(@as(usize, 4096), allocation.append_offset);
    try std.testing.expectEqual(true, allocation.preserve_descriptor_refs);
    try std.testing.expectEqual(ipc.hot_reload.DescriptorState.published, ipc.hot_reload.descriptorSnapshot(desc1).state);
    try std.testing.expectEqual(@as(usize, 4096), try SharedMemoryAllocator.mappedHeaderUsedSize(shm.base_ptr, shm.total_size));
}

test "hot reload sweep keeps top reclaimed descriptor region addressable" {
    const page_size = try SharedMemoryAllocator.getSystemPageSize();
    var shm = try SharedMemoryAllocator.create(std.testing.io, 64 * 1024, page_size);
    defer shm.deinit(std.testing.allocator);

    const handle = testingSharedMemoryHandle(&shm);
    const desc0_offset = try hotReloadInitialDescriptorOffset(handle);
    const desc1_offset = hotReloadPreviousDescriptorOffset(desc0_offset).?;

    const control = ipc.hot_reload.controlFromBase(shm.base_ptr);
    const desc0 = testingPrepareHotReloadDescriptor(&shm, 3, desc0_offset, 512, 2048, 512, 2048);
    ipc.hot_reload.init(control, desc0_offset, desc0);
    try SharedMemoryAllocator.rewindMappedHeader(shm.base_ptr, shm.total_size, 2048);

    const desc1 = testingPrepareHotReloadDescriptor(&shm, 2, desc1_offset, 2048, 4096, 2048, 4096);
    try SharedMemoryAllocator.rewindMappedHeader(shm.base_ptr, shm.total_size, 4096);
    ipc.hot_reload.publishDescriptor(control, 3, desc0_offset, desc0);

    var tracker = HotReloadDescriptorTracker{};
    defer tracker.deinit(std.testing.allocator);
    try testingInitHotReloadDescriptorTracker(&tracker, desc0_offset, desc1_offset);
    try hotReloadTrackDescriptor(std.testing.allocator, &tracker, desc0_offset);
    try hotReloadTrackDescriptor(std.testing.allocator, &tracker, desc1_offset);

    const allocation = try hotReloadChooseImageAllocation(std.testing.allocator, control, handle, @sizeOf(SharedMemoryAllocator.Header), &tracker, 4);

    try std.testing.expectEqual(desc1_offset, allocation.descriptor_offset);
    try std.testing.expectEqual(desc1_offset, allocation.image_limit);
    try std.testing.expectEqual(@as(usize, 2048), allocation.region_start);
    try std.testing.expectEqual(@as(usize, 4096), allocation.region_end);
    try std.testing.expectEqual(@as(usize, 4096), allocation.append_offset);
    try std.testing.expectEqual(true, allocation.preserve_descriptor_refs);
    try std.testing.expectEqual(ipc.hot_reload.DescriptorState.reclaimed, ipc.hot_reload.descriptorSnapshot(desc1).state);
    try std.testing.expectEqual(ipc.hot_reload.DescriptorState.published, ipc.hot_reload.descriptorSnapshot(desc0).state);
    try std.testing.expectEqual(@as(usize, 4096), try SharedMemoryAllocator.mappedHeaderUsedSize(shm.base_ptr, shm.total_size));
}

test "hot reload allocation skips retired retained descriptors" {
    const page_size = try SharedMemoryAllocator.getSystemPageSize();
    var shm = try SharedMemoryAllocator.create(std.testing.io, 64 * 1024, page_size);
    defer shm.deinit(std.testing.allocator);

    const handle = testingSharedMemoryHandle(&shm);
    const desc0_offset = try hotReloadInitialDescriptorOffset(handle);
    const desc1_offset = hotReloadPreviousDescriptorOffset(desc0_offset).?;
    const desc2_offset = hotReloadPreviousDescriptorOffset(desc1_offset).?;

    const control = ipc.hot_reload.controlFromBase(shm.base_ptr);
    const desc0 = testingPrepareHotReloadDescriptor(&shm, 1, desc0_offset, 512, 2048, 512, 2048);
    ipc.hot_reload.init(control, desc0_offset, desc0);
    ipc.hot_reload.retainDescriptor(desc0);
    try SharedMemoryAllocator.rewindMappedHeader(shm.base_ptr, shm.total_size, 2048);

    const desc1 = testingPrepareHotReloadDescriptor(&shm, 2, desc1_offset, 2048, 4096, 2048, 4096);
    ipc.hot_reload.publishDescriptor(control, 2, desc1_offset, desc1);
    ipc.hot_reload.retainDescriptor(desc1);
    try SharedMemoryAllocator.rewindMappedHeader(shm.base_ptr, shm.total_size, 4096);

    const desc2 = testingPrepareHotReloadDescriptor(&shm, 3, desc2_offset, 4096, 8192, 4096, 8192);
    ipc.hot_reload.publishDescriptor(control, 3, desc2_offset, desc2);
    try SharedMemoryAllocator.rewindMappedHeader(shm.base_ptr, shm.total_size, 8192);

    var tracker = HotReloadDescriptorTracker{};
    defer tracker.deinit(std.testing.allocator);
    try testingInitHotReloadDescriptorTracker(&tracker, desc0_offset, desc2_offset);
    try hotReloadTrackDescriptor(std.testing.allocator, &tracker, desc0_offset);
    try hotReloadTrackDescriptor(std.testing.allocator, &tracker, desc1_offset);
    try hotReloadTrackDescriptor(std.testing.allocator, &tracker, desc2_offset);

    const allocation = try hotReloadChooseImageAllocation(std.testing.allocator, control, handle, @sizeOf(SharedMemoryAllocator.Header), &tracker, 4);

    try std.testing.expectEqual(hotReloadPreviousDescriptorOffset(desc2_offset).?, allocation.descriptor_offset);
    try std.testing.expectEqual(hotReloadPreviousDescriptorOffset(desc2_offset).?, allocation.image_limit);
    try std.testing.expectEqual(@as(usize, 0), allocation.region_start);
    try std.testing.expectEqual(@as(usize, 0), allocation.region_end);
    try std.testing.expectEqual(@as(usize, 8192), allocation.append_offset);
    try std.testing.expectEqual(false, allocation.preserve_descriptor_refs);
    try std.testing.expectEqual(ipc.hot_reload.DescriptorState.retired, ipc.hot_reload.descriptorSnapshot(desc1).state);
    try std.testing.expectEqual(@as(u32, 1), ipc.hot_reload.descriptorSnapshot(desc1).refs);

    ipc.hot_reload.releaseDescriptor(desc0);
    ipc.hot_reload.releaseDescriptor(desc1);
}

test "hot reload allocation rolls back fresh descriptor when append has no room" {
    const page_size = try SharedMemoryAllocator.getSystemPageSize();
    var shm = try SharedMemoryAllocator.create(std.testing.io, 64 * 1024, page_size);
    defer shm.deinit(std.testing.allocator);

    const handle = testingSharedMemoryHandle(&shm);
    const desc0_offset = try hotReloadInitialDescriptorOffset(handle);
    const desc1_offset = hotReloadPreviousDescriptorOffset(desc0_offset).?;

    const control = ipc.hot_reload.controlFromBase(shm.base_ptr);
    const desc0 = testingPrepareHotReloadDescriptor(&shm, 1, desc0_offset, 512, desc1_offset, 512, desc1_offset);
    ipc.hot_reload.init(control, desc0_offset, desc0);
    try SharedMemoryAllocator.rewindMappedHeader(shm.base_ptr, shm.total_size, desc1_offset);

    var tracker = HotReloadDescriptorTracker{};
    defer tracker.deinit(std.testing.allocator);
    try hotReloadInitDescriptorTracker(&tracker, desc0_offset);
    try hotReloadTrackDescriptor(std.testing.allocator, &tracker, desc0_offset);

    try std.testing.expectError(
        error.OutOfMemory,
        hotReloadChooseImageAllocation(std.testing.allocator, control, handle, @sizeOf(SharedMemoryAllocator.Header), &tracker, 2),
    );
    try std.testing.expectEqual(desc0_offset, tracker.descriptor_floor);
    try std.testing.expectEqual(desc1_offset, tracker.next_descriptor_offset);
    try std.testing.expectEqual(@as(usize, 0), tracker.free_descriptor_slots.items.len);
}

test "hot reload allocation can use reclaimed region when append has no room" {
    const page_size = try SharedMemoryAllocator.getSystemPageSize();
    var shm = try SharedMemoryAllocator.create(std.testing.io, 64 * 1024, page_size);
    defer shm.deinit(std.testing.allocator);

    const handle = testingSharedMemoryHandle(&shm);
    const desc0_offset = try hotReloadInitialDescriptorOffset(handle);
    const desc1_offset = hotReloadPreviousDescriptorOffset(desc0_offset).?;

    const control = ipc.hot_reload.controlFromBase(shm.base_ptr);
    const desc0 = testingPrepareHotReloadDescriptor(&shm, 1, desc0_offset, 4096, desc1_offset, 4096, desc1_offset);
    ipc.hot_reload.init(control, desc0_offset, desc0);
    try SharedMemoryAllocator.rewindMappedHeader(shm.base_ptr, shm.total_size, desc1_offset);

    var tracker = HotReloadDescriptorTracker{};
    defer tracker.deinit(std.testing.allocator);
    try hotReloadInitDescriptorTracker(&tracker, desc0_offset);
    try hotReloadTrackDescriptor(std.testing.allocator, &tracker, desc0_offset);
    try hotReloadAddFreeRegion(std.testing.allocator, &tracker, handle, @sizeOf(SharedMemoryAllocator.Header), 512, 4096);

    const allocation = try hotReloadChooseImageAllocation(std.testing.allocator, control, handle, @sizeOf(SharedMemoryAllocator.Header), &tracker, 2);

    try std.testing.expectEqual(desc1_offset, allocation.descriptor_offset);
    try std.testing.expectEqual(desc1_offset, allocation.image_limit);
    try std.testing.expectEqual(@as(usize, 512), allocation.region_start);
    try std.testing.expectEqual(@as(usize, 4096), allocation.region_end);
    try std.testing.expectEqual(desc1_offset, allocation.append_offset);
}

/// Result of setting up shared memory with type checking information.
/// Contains the shared memory handle for the compiled modules and
/// counts of errors and warnings encountered during compilation.
pub const SharedMemoryResult = struct {
    handle: SharedMemoryHandle,
    entrypoint_names: []const []const u8,
    hosted_symbols: []const []const u8,
    checked_host_identity: ?[32]u8,
    error_count: usize,
    warning_count: usize,
};

const CoordinatorReportCounts = struct {
    errors: usize,
    warnings: usize,
};

const LoweredCoordinatorResult = struct {
    lowered: ?lir.CheckedPipeline.LoweredProgram,
    internal_static_data: ?[]backend.StaticDataExport,
    entrypoint_names: []const []const u8,
    hosted_symbols: []const []const u8,
    checked_host_identity: ?[32]u8,
    watch_inputs: []const compile.watch_inputs.Input,
    watch_inputs_allocator: Allocator,
    counts: CoordinatorReportCounts,

    fn deinit(self: *LoweredCoordinatorResult) void {
        if (self.internal_static_data) |static_data| {
            compile.static_data_exports.deinitStaticData(self.watch_inputs_allocator, static_data);
        }
        if (self.lowered) |*lowered| {
            lowered.deinit();
        }
        self.deinitWatchInputs();
    }

    fn deinitWatchInputs(self: *LoweredCoordinatorResult) void {
        compile.watch_inputs.deinit(self.watch_inputs_allocator, self.watch_inputs);
        self.watch_inputs = &.{};
    }
};

fn successfulInternalStaticData(result: *const LoweredCoordinatorResult, label: []const u8) []const backend.StaticDataExport {
    return result.internal_static_data orelse {
        if (builtin.mode == .Debug) {
            std.debug.panic("{s} invariant violated: dev RunImage lowering produced no internal static data bundle", .{label});
        }
        unreachable;
    };
}

fn successfulLoweredProgram(result: *LoweredCoordinatorResult, label: []const u8) *lir.CheckedPipeline.LoweredProgram {
    return if (result.lowered) |*lowered| lowered else {
        if (builtin.mode == .Debug) {
            std.debug.panic("{s} invariant violated: successful coordinator lowering produced no lowered program", .{label});
        }
        unreachable;
    };
}

/// Render every report drained from the core and print the run-path summary
/// trailer. Draining moves the reports out of the core, so calling this again
/// renders nothing new — re-rendering a report is structurally impossible
/// rather than a call-site convention (the PR 9759 bug class). Version-bump
/// notes and synthetic default-app path remapping are applied by the core
/// during the drain.
fn renderDrainedBuildEnvReports(ctx: *CliCtx, build_env: *BuildEnv, display_path: []const u8) Allocator.Error!CoordinatorReportCounts {
    var counts = CoordinatorReportCounts{ .errors = 0, .warnings = 0 };

    const drained = try build_env.drainReports();
    defer build_env.freeDrainedReports(drained);

    for (drained) |mod| {
        for (mod.reports) |*report| {
            switch (report.severity) {
                .info => continue,
                .fatal, .runtime_error => counts.errors += 1,
                .warning => counts.warnings += 1,
            }
            if (!builtin.is_test) {
                reporting.renderReportToTerminal(report, ctx.io.stderr(), ColorPalette.ANSI, ctx.terminalReportConfig()) catch {};
            }
        }
    }

    if (counts.errors > 0 or counts.warnings > 0) {
        const stderr = ctx.io.stderr();
        stderr.writeAll("\n") catch {};
        stderr.print("Found {} error(s) and {} warning(s) for {s}.\n", .{
            counts.errors,
            counts.warnings,
            display_path,
        }) catch {};
    }

    ctx.io.flush();
    return counts;
}

fn sharedMemoryResult(
    shm: *SharedMemoryAllocator,
    counts: CoordinatorReportCounts,
    entrypoint_names: []const []const u8,
    hosted_symbols: []const []const u8,
    checked_host_identity: ?[32]u8,
) SharedMemoryResult {
    return .{
        .handle = .{
            .fd = shm.handle,
            .ptr = shm.base_ptr,
            .size = shm.getUsedSize(),
            .mapped_size = shm.total_size,
        },
        .entrypoint_names = entrypoint_names,
        .hosted_symbols = hosted_symbols,
        .checked_host_identity = checked_host_identity,
        .error_count = counts.errors,
        .warning_count = counts.warnings,
    };
}

fn closeSharedMemoryHandle(handle: SharedMemoryHandle) void {
    if (comptime is_windows) {
        _ = ipc.platform.windows.UnmapViewOfFile(handle.ptr);
        _ = ipc.platform.windows.CloseHandle(@ptrCast(handle.fd));
    } else {
        _ = posix.munmap(handle.ptr, handle.mapped_size);
        if (c.close(handle.fd) != 0) {}
    }
}

fn viewLirImageFromHandle(handle: SharedMemoryHandle, target_usize: base.target.TargetUsize, arena: std.mem.Allocator) lir.LirImage.ImageError!lir.LirImage.ProgramView {
    const base_ptr: [*]align(1) u8 = @ptrCast(@alignCast(handle.ptr));
    const header: *const lir.LirImage.Header = @ptrCast(@alignCast(base_ptr + @sizeOf(SharedMemoryAllocator.Header)));
    return lir.LirImage.viewMappedImageWithAllocator(header, base_ptr, handle.size, target_usize, arena);
}

fn devShimTargetCompatible(selected: RocTarget, native: RocTarget) bool {
    return selected.toCpuArch() == native.toCpuArch() and
        selected.toOsTag() == native.toOsTag() and
        selected.ptrBitWidth() == native.ptrBitWidth();
}

fn useDefaultAppSharedMemoryShim(args: cli_args.RunArgs) bool {
    if (args.opt != .dev) return false;
    if (args.target != null) return true;

    const native_target = RocTarget.detectNative();
    const default_target = defaultRunShimTarget(native_target);
    return devShimTargetCompatible(default_target, native_target) and
        default_target.toOsTag() == .linux and
        DefaultPlatformRuntimeObjects.forTarget(default_target) != null;
}

fn defaultRunShimTarget(native: RocTarget) RocTarget {
    return switch (native) {
        .x64musl, .x64glibc, .x64linux => .x64linux,
        .arm64musl, .arm64glibc, .arm64linux => .arm64linux,
        else => native,
    };
}

const DevRunImagePublication = struct {
    descriptor: ?*ipc.hot_reload.ImageDescriptor,
    descriptor_offset: usize,
};

const hot_reload_dev_command = "__roc_hot_reload_dev";

const HotReloadDevWorkerArgs = struct {
    path: []const u8,
    target: []const u8,
    generation: u64,
    descriptor_offset: usize,
    image_limit: usize,
    region_start: usize,
    region_end: usize,
    append_offset: usize,
    preserve_descriptor_refs: bool,
    shm_handle: []const u8,
    shm_size: usize,
    expected_host_identity: [32]u8,
    watch_inputs_file: []const u8,
    synthetic_source_path: ?[]const u8,
    synthetic_output_path: ?[]const u8,
    source_dir_override: ?[]const u8,
    max_threads: ?usize,
    no_cache: bool,
    resolve_limits: cli_args.ResolveLimitArgs,
};

fn parseHotReloadDigest(hex: []const u8) error{InvalidArguments}![32]u8 {
    if (hex.len != 64) return error.InvalidArguments;

    var digest: [32]u8 = undefined;
    for (&digest, 0..) |*byte, i| {
        const hi = hexNibble(hex[i * 2]) orelse return error.InvalidArguments;
        const lo = hexNibble(hex[i * 2 + 1]) orelse return error.InvalidArguments;
        byte.* = (hi << 4) | lo;
    }
    return digest;
}

fn hotReloadFlagValue(arg: []const u8, flag: []const u8) ?[]const u8 {
    if (!std.mem.startsWith(u8, arg, flag)) return null;
    if (arg.len <= flag.len or arg[flag.len] != '=') return null;
    return arg[flag.len + 1 ..];
}

fn parseHotReloadBool(value: []const u8) error{InvalidArguments}!bool {
    if (std.mem.eql(u8, value, "0")) return false;
    if (std.mem.eql(u8, value, "1")) return true;
    return error.InvalidArguments;
}

fn parseHotReloadDevWorkerArgs(args: []const []const u8) error{InvalidArguments}!HotReloadDevWorkerArgs {
    var path: ?[]const u8 = null;
    var target: ?[]const u8 = null;
    var generation: ?u64 = null;
    var descriptor_offset: ?usize = null;
    var image_limit: ?usize = null;
    var region_start: ?usize = null;
    var region_end: ?usize = null;
    var append_offset: ?usize = null;
    var preserve_descriptor_refs: ?bool = null;
    var shm_handle: ?[]const u8 = null;
    var shm_size: ?usize = null;
    var expected_host_identity: ?[32]u8 = null;
    var watch_inputs_file: ?[]const u8 = null;
    var synthetic_source_path: ?[]const u8 = null;
    var synthetic_output_path: ?[]const u8 = null;
    var source_dir_override: ?[]const u8 = null;
    var max_threads: ?usize = null;
    var no_cache: bool = false;
    var resolve_limits = cli_args.ResolveLimitArgs{};

    for (args) |arg| {
        if (hotReloadFlagValue(arg, "--path")) |value| {
            path = value;
        } else if (hotReloadFlagValue(arg, "--target")) |value| {
            target = value;
        } else if (hotReloadFlagValue(arg, "--generation")) |value| {
            generation = std.fmt.parseInt(u64, value, 10) catch return error.InvalidArguments;
        } else if (hotReloadFlagValue(arg, "--descriptor-offset")) |value| {
            descriptor_offset = std.fmt.parseInt(usize, value, 10) catch return error.InvalidArguments;
        } else if (hotReloadFlagValue(arg, "--image-limit")) |value| {
            image_limit = std.fmt.parseInt(usize, value, 10) catch return error.InvalidArguments;
        } else if (hotReloadFlagValue(arg, "--region-start")) |value| {
            region_start = std.fmt.parseInt(usize, value, 10) catch return error.InvalidArguments;
        } else if (hotReloadFlagValue(arg, "--region-end")) |value| {
            region_end = std.fmt.parseInt(usize, value, 10) catch return error.InvalidArguments;
        } else if (hotReloadFlagValue(arg, "--append-offset")) |value| {
            append_offset = std.fmt.parseInt(usize, value, 10) catch return error.InvalidArguments;
        } else if (hotReloadFlagValue(arg, "--preserve-descriptor-refs")) |value| {
            preserve_descriptor_refs = try parseHotReloadBool(value);
        } else if (hotReloadFlagValue(arg, "--shm-handle")) |value| {
            shm_handle = value;
        } else if (hotReloadFlagValue(arg, "--shm-size")) |value| {
            shm_size = std.fmt.parseInt(usize, value, 10) catch return error.InvalidArguments;
        } else if (hotReloadFlagValue(arg, "--expected-host")) |value| {
            expected_host_identity = try parseHotReloadDigest(value);
        } else if (hotReloadFlagValue(arg, "--watch-inputs-file")) |value| {
            watch_inputs_file = value;
        } else if (hotReloadFlagValue(arg, "--synthetic-source")) |value| {
            synthetic_source_path = value;
        } else if (hotReloadFlagValue(arg, "--synthetic-output")) |value| {
            synthetic_output_path = value;
        } else if (hotReloadFlagValue(arg, "--source-dir")) |value| {
            source_dir_override = value;
        } else if (hotReloadFlagValue(arg, "--jobs")) |value| {
            max_threads = std.fmt.parseInt(usize, value, 10) catch return error.InvalidArguments;
        } else if (hotReloadFlagValue(arg, "--no-cache")) |value| {
            no_cache = try parseHotReloadBool(value);
        } else if (hotReloadFlagValue(arg, "--max-package-mb")) |value| {
            resolve_limits.max_package_mb = std.fmt.parseInt(u32, value, 10) catch return error.InvalidArguments;
        } else if (hotReloadFlagValue(arg, "--max-transitive-mb")) |value| {
            resolve_limits.max_transitive_mb = std.fmt.parseInt(u32, value, 10) catch return error.InvalidArguments;
        } else {
            return error.InvalidArguments;
        }
    }

    return .{
        .path = path orelse return error.InvalidArguments,
        .target = target orelse return error.InvalidArguments,
        .generation = generation orelse return error.InvalidArguments,
        .descriptor_offset = descriptor_offset orelse return error.InvalidArguments,
        .image_limit = image_limit orelse return error.InvalidArguments,
        .region_start = region_start orelse return error.InvalidArguments,
        .region_end = region_end orelse return error.InvalidArguments,
        .append_offset = append_offset orelse return error.InvalidArguments,
        .preserve_descriptor_refs = preserve_descriptor_refs orelse return error.InvalidArguments,
        .shm_handle = shm_handle orelse return error.InvalidArguments,
        .shm_size = shm_size orelse return error.InvalidArguments,
        .expected_host_identity = expected_host_identity orelse return error.InvalidArguments,
        .watch_inputs_file = watch_inputs_file orelse return error.InvalidArguments,
        .synthetic_source_path = synthetic_source_path,
        .synthetic_output_path = synthetic_output_path,
        .source_dir_override = source_dir_override,
        .max_threads = max_threads,
        .no_cache = no_cache,
        .resolve_limits = resolve_limits,
    };
}

fn rocInternalHotReloadDev(ctx: *CliCtx, raw_args: []const []const u8) CliMainError!void {
    const args = parseHotReloadDevWorkerArgs(raw_args) catch |err| {
        try ctx.io.stderr().print("Error: invalid internal hot-reload compiler arguments: {}\n", .{err});
        return err;
    };

    const selected_target = RocTarget.fromString(args.target) orelse {
        try ctx.io.stderr().print("Error: invalid internal hot-reload target: {s}\n", .{args.target});
        return error.InvalidTarget;
    };

    const page_size = try SharedMemoryAllocator.getSystemPageSize();
    const handle = try ipc.coordination.parseHandle(args.shm_handle);
    var shm = try SharedMemoryAllocator.fromFdWithHeaderOffset(handle, args.shm_size, page_size);
    defer shm.deinit(ctx.gpa);

    const control = ipc.hot_reload.controlFromBase(shm.base_ptr);
    if (!ipc.hot_reload.initialized(control)) {
        try ctx.io.stderr().writeAll("Error: hot-reload shared memory control block is not initialized.\n");
        return error.InvalidSharedMemory;
    }

    var original_source_owned: ?[]const u8 = null;
    defer if (original_source_owned) |source| ctx.gpa.free(source);
    const source_rewrite: ?HotReloadSourceRewrite = if (args.synthetic_source_path) |source_path| blk: {
        const synthetic_output_path = args.synthetic_output_path orelse return error.InvalidArguments;
        const source_dir_override = args.source_dir_override orelse return error.InvalidArguments;
        const max_source_size = 256 * 1024 * 1024;
        original_source_owned = try std.Io.Dir.cwd().readFileAlloc(ctx.io.std_io, source_path, ctx.gpa, .limited(max_source_size));
        try writeDefaultAppSyntheticRunSource(ctx, synthetic_output_path, original_source_owned.?);
        break :blk .{
            .source_path = source_path,
            .synthetic_app_path = synthetic_output_path,
            .source_dir_override = source_dir_override,
        };
    } else blk: {
        if (args.synthetic_output_path != null or args.source_dir_override != null) return error.InvalidArguments;
        break :blk null;
    };

    var lowered_result = try lowerLirWithBuildEnv(
        ctx,
        ctx.gpa,
        .{ .dev_run_image = selected_target },
        args.path,
        if (source_rewrite) |rewrite| rewrite.source_dir_override else null,
        if (source_rewrite) |rewrite| .{
            .original_path = rewrite.source_path,
            .original_source = original_source_owned.?,
        } else null,
        args.max_threads,
        .dev,
        resolutionConfigFromLimits(args.resolve_limits),
        !args.no_cache,
        null,
        false,
    );
    defer lowered_result.deinit();

    try writeHotReloadWatchPathsFile(ctx, args.watch_inputs_file, lowered_result.watch_inputs, source_rewrite);

    if (lowered_result.counts.errors > 0) {
        return error.TypeCheckingFailed;
    }

    const checked_host_identity = lowered_result.checked_host_identity orelse {
        if (builtin.mode == .Debug) {
            std.debug.panic("hot reload invariant violated: missing checked host identity after successful rebuild", .{});
        }
        unreachable;
    };
    if (!std.mem.eql(u8, &checked_host_identity, &args.expected_host_identity)) {
        try ctx.io.stderr().writeAll("Error: hot reload changed the platform host interface; restart `roc --watch`.\n");
        return error.TypeCheckingFailed;
    }

    const lowered = successfulLoweredProgram(&lowered_result, "hot reload compiler");
    const publication = try writeDevRunImageToSharedMemory(
        ctx,
        &shm,
        selected_target,
        lowered_result.entrypoint_names,
        lowered,
        successfulInternalStaticData(&lowered_result, "hot reload compiler"),
        .{
            .generation = args.generation,
            .descriptor_offset = args.descriptor_offset,
            .image_limit = args.image_limit,
            .region_start = args.region_start,
            .region_end = args.region_end,
            .append_offset = args.append_offset,
            .preserve_descriptor_refs = args.preserve_descriptor_refs,
        },
    );
    shm.updateHeader();
    ipc.hot_reload.publishDescriptor(
        control,
        args.generation,
        publication.descriptor_offset,
        publication.descriptor orelse return error.InvalidSharedMemory,
    );
}

fn writeDevRunImageToSharedMemory(
    ctx: *CliCtx,
    shm: *SharedMemoryAllocator,
    selected_target: RocTarget,
    entrypoint_names: []const []const u8,
    lowered: *const lir.CheckedPipeline.LoweredProgram,
    internal_static_data: []const backend.StaticDataExport,
    hot_reload_allocation: ?HotReloadImageAllocation,
) CliMainError!DevRunImagePublication {
    if (comptime !backend.host_lir_codegen_available) {
        try ctx.io.stderr().print(
            "Error: The dev backend cannot run in memory on this host architecture.\n",
            .{},
        );
        return error.UnsupportedTarget;
    } else {
        const native_target = backend.HostLirCodeGen.roc_target;
        if (!devShimTargetCompatible(selected_target, native_target)) {
            try rejectRunTargetNotExecutable(ctx, selected_target);
            unreachable;
        }

        const store = &lowered.lir_result.store;
        const layouts = &lowered.lir_result.layouts;

        var static_strings = try backend.StaticStringData.build(ctx.gpa, store, native_target);
        defer static_strings.deinit();

        var readonly_data = std.ArrayList(backend.StaticDataExport).empty;
        defer readonly_data.deinit(ctx.gpa);
        try readonly_data.ensureTotalCapacity(ctx.gpa, internal_static_data.len + static_strings.exports.len);
        try readonly_data.appendSlice(ctx.gpa, internal_static_data);
        try readonly_data.appendSlice(ctx.gpa, static_strings.exports);

        var codegen = try backend.HostLirCodeGen.init(
            ctx.gpa,
            store,
            layouts,
            static_strings.entries,
            .preserve,
        );
        defer codegen.deinit();
        codegen.generation_mode = .shim_execution;
        codegen.enable_hot_reload = hot_reload_allocation != null;

        const proc_specs = store.getProcSpecs();
        try codegen.compileAllProcSpecs(proc_specs);

        const static_rc_helpers = try backend.collectRequiredRcHelpers(ctx.gpa, readonly_data.items);
        defer ctx.gpa.free(static_rc_helpers);
        try codegen.compileStaticDataRcHelpers(static_rc_helpers);

        var runtime_proc_count: usize = 0;
        for (proc_specs) |proc| {
            if (!proc.is_static_initializer) runtime_proc_count += 1;
        }

        const code_symbols = try ctx.gpa.alloc(
            backend.RunImage.CodeSymbolInput,
            runtime_proc_count + static_rc_helpers.len,
        );
        var initialized_code_symbols: usize = 0;
        defer {
            for (code_symbols[0..initialized_code_symbols]) |symbol| ctx.gpa.free(symbol.name);
            ctx.gpa.free(code_symbols);
        }
        for (proc_specs, 0..) |proc, i| {
            if (proc.is_static_initializer) continue;
            const proc_id: lir.LIR.LirProcSpecId = @enumFromInt(@as(u32, @intCast(i)));
            const compiled = codegen.compiledProcSymbol(proc_id) orelse {
                if (builtin.mode == .Debug) {
                    std.debug.panic("dev run invariant violated: LIR proc {d} was not compiled before image symbol publication", .{i});
                }
                unreachable;
            };
            code_symbols[initialized_code_symbols] = .{
                .name = try backend.procSymbolName(ctx.gpa, compiled.name),
                .code_offset = compiled.code_start,
            };
            initialized_code_symbols += 1;
        }
        for (static_rc_helpers) |helper_key| {
            const code_offset = codegen.compiledStaticDataRcHelperOffset(helper_key) orelse {
                if (builtin.mode == .Debug) {
                    std.debug.panic(
                        "dev run invariant violated: static RC helper {x} was not compiled before image symbol publication",
                        .{helper_key.encode()},
                    );
                }
                unreachable;
            };
            code_symbols[initialized_code_symbols] = .{
                .name = try backend.atomicRcHelperSymbolName(ctx.gpa, helper_key),
                .code_offset = code_offset,
            };
            initialized_code_symbols += 1;
        }

        const platform_entrypoints = try lowered.platformEntrypoints(ctx.gpa);
        defer ctx.gpa.free(platform_entrypoints);

        const entrypoints = try ctx.gpa.alloc(backend.RunImage.EntrypointInput, platform_entrypoints.len);
        defer ctx.gpa.free(entrypoints);

        for (platform_entrypoints, 0..) |platform_entrypoint, i| {
            const ordinal: usize = @intCast(platform_entrypoint.ordinal);
            if (ordinal >= entrypoint_names.len) {
                if (builtin.mode == .Debug) {
                    std.debug.panic("dev run invariant violated: platform entrypoint ordinal {d} exceeds name table length {d}", .{ ordinal, entrypoint_names.len });
                }
                unreachable;
            }

            const proc = store.getProcSpec(platform_entrypoint.root_proc);
            const arg_layouts = try argLayoutsForProc(ctx.gpa, store, platform_entrypoint.root_proc);
            defer ctx.gpa.free(arg_layouts);

            const exported = try codegen.generateEntrypointWrapper(
                entrypoint_names[ordinal],
                platform_entrypoint.root_proc,
                arg_layouts,
                proc.ret_layout,
            );
            entrypoints[i] = .{
                .ordinal = platform_entrypoint.ordinal,
                .code_offset = exported.offset,
            };
        }

        const generated_code = codegen.getGeneratedCode();
        const relocations = codegen.getRelocations();

        var empty_region_buffer: [0]u8 = .{};
        var fixed_region_buffer = std.heap.FixedBufferAllocator.init(&empty_region_buffer);
        var image_allocator = shm.allocator();
        var descriptor: ?*ipc.hot_reload.ImageDescriptor = null;
        var descriptor_offset: usize = ipc.hot_reload.invalid_descriptor_offset;
        var allocation_start: usize = 0;
        var allocation_end: usize = 0;
        var reset_descriptor_refs = true;
        const original_total_size = shm.total_size;
        defer shm.total_size = original_total_size;

        if (hot_reload_allocation) |allocation| {
            const reusable_capacity = if (allocation.region_end > allocation.region_start)
                allocation.region_end - allocation.region_start
            else
                0;
            const required_bound = try backend.RunImage.requiredCapacityFromOffset(
                shm.page_size,
                allocation.region_start,
                generated_code,
                entrypoints,
                code_symbols,
                relocations,
                readonly_data.items,
            );
            const required_capacity = required_bound - allocation.region_start;

            descriptor = try hotReloadDescriptorForWrite(shm, allocation.descriptor_offset);
            descriptor_offset = allocation.descriptor_offset;
            reset_descriptor_refs = !allocation.preserve_descriptor_refs;

            if (reusable_capacity >= required_capacity) {
                const region_bytes = shm.base_ptr[allocation.region_start..allocation.region_end];
                fixed_region_buffer = std.heap.FixedBufferAllocator.init(region_bytes);
                image_allocator = fixed_region_buffer.allocator();
                allocation_start = allocation.region_start;
            } else {
                if (allocation.image_limit <= allocation.append_offset) return error.OutOfMemory;
                try shm.resetToUsedSize(allocation.append_offset);
                shm.total_size = allocation.image_limit;
                allocation_start = allocation.append_offset;
            }
        }

        const header = try backend.RunImage.writeToSharedMemory(
            ctx.gpa,
            image_allocator,
            shm.base_ptr,
            shm.page_size,
            generated_code,
            entrypoints,
            code_symbols,
            relocations,
            readonly_data.items,
        );
        const image_offset = @intFromPtr(header) - @intFromPtr(shm.base_ptr);
        const image_bound: usize = @intCast(header.image_size);
        if (hot_reload_allocation) |allocation| {
            allocation_end = image_bound;
            ipc.hot_reload.prepareDescriptor(
                descriptor.?,
                allocation.generation,
                image_offset,
                image_bound,
                allocation_start,
                allocation_end,
                reset_descriptor_refs,
            );
            try shm.resetToUsedSize(@max(allocation.append_offset, allocation_end));
        }
        return .{
            .descriptor = descriptor,
            .descriptor_offset = descriptor_offset,
        };
    }
}

fn publishDevRunImage(
    ctx: *CliCtx,
    selected_target: RocTarget,
    entrypoint_names: []const []const u8,
    lowered: *const lir.CheckedPipeline.LoweredProgram,
    internal_static_data: []const backend.StaticDataExport,
    enable_hot_reload: bool,
) CliMainError!SharedMemoryHandle {
    const page_size = try SharedMemoryAllocator.getSystemPageSize();
    var shm = try createExecutableSharedMemory(ctx.io.std_io, page_size);
    errdefer shm.deinit(ctx.gpa);

    const hot_reload_allocation: ?HotReloadImageAllocation = if (enable_hot_reload) blk: {
        const descriptor_offset = try hotReloadInitialDescriptorOffset(.{
            .fd = shm.handle,
            .ptr = shm.base_ptr,
            .size = shm.getUsedSize(),
            .mapped_size = shm.total_size,
        });
        break :blk .{
            .generation = 1,
            .descriptor_offset = descriptor_offset,
            .image_limit = descriptor_offset,
            .region_start = 0,
            .region_end = 0,
            .append_offset = @sizeOf(SharedMemoryAllocator.Header),
        };
    } else null;
    const publication = try writeDevRunImageToSharedMemory(
        ctx,
        &shm,
        selected_target,
        entrypoint_names,
        lowered,
        internal_static_data,
        hot_reload_allocation,
    );
    if (enable_hot_reload) {
        ipc.hot_reload.init(
            ipc.hot_reload.controlFromBase(shm.base_ptr),
            publication.descriptor_offset,
            publication.descriptor orelse return error.InvalidSharedMemory,
        );
    }
    shm.updateHeader();
    return .{
        .fd = shm.handle,
        .ptr = shm.base_ptr,
        .size = shm.getUsedSize(),
        .mapped_size = shm.total_size,
    };
}

fn argLayoutsForProc(
    allocator: Allocator,
    store: *const lir.LirStore,
    proc_id: lir.LirProcSpecId,
) Allocator.Error![]layout.Idx {
    const proc = store.getProcSpec(proc_id);
    const arg_ids = store.getLocalSpan(proc.args);
    const arg_layouts = try allocator.alloc(layout.Idx, arg_ids.len);
    errdefer allocator.free(arg_layouts);

    for (0..arg_ids.len) |i| {
        const local_id = GuardedList.at(arg_ids, i);
        arg_layouts[i] = store.getLocal(local_id).layout_idx;
    }

    return arg_layouts;
}

fn reportCliInterpreterError(ops: *echo_platform.host_abi.RocOps, interpreter: *const eval.LirInterpreter, err: eval.LirInterpreter.Error) void {
    const message = switch (err) {
        error.OutOfMemory => "Roc interpreter ran out of memory",
        error.RuntimeError => interpreter.getRuntimeErrorMessage() orelse "Roc runtime error",
        error.DivisionByZero => interpreter.getRuntimeErrorMessage() orelse "Division by zero",
        error.ComptimeExhaustiveness => "compile-time exhaustiveness failure reached runtime code",
        error.Crash => return,
        // expect_err statements only occur in top-level expect test roots,
        // never in program entrypoints.
        error.ExpectErr => unreachable,
    };
    ops.crash(message);
}

fn evaluateLirImageEntrypoint(
    allocator: Allocator,
    view: *const lir.LirImage.ProgramView,
    ordinal: u32,
    ops: *echo_platform.host_abi.RocOps,
    ret_ptr: ?*anyopaque,
    arg_ptr: ?*anyopaque,
) Allocator.Error!void {
    var interpreter = try eval.LirInterpreter.init(allocator, &view.store, &view.layouts, ops, .preserve);
    defer interpreter.deinit();

    _ = interpreter.runEntrypoint(view, ordinal, arg_ptr, ret_ptr) catch |err| switch (err) {
        error.EntrypointNotFound => {
            if (builtin.mode == .Debug) {
                std.debug.panic("CLI LIR image invariant violated: missing platform entrypoint ordinal {d}", .{ordinal});
            }
            unreachable;
        },
        else => |e| {
            reportCliInterpreterError(ops, &interpreter, e);
            return;
        },
    };
}

/// Source mapping for a headerless default app staged into a temp dir: the
/// core remaps diagnostics from the synthetic file back to the user's file.
const SyntheticDefaultAppMapping = struct {
    original_path: []const u8,
    original_source: []const u8,
};

const PlatformEntrypointArtifact = union(enum) {
    /// Pointer-width-independent LIR consumed directly by the interpreter.
    lir_image,
    /// Target-specific machine code and readonly data consumed by the dev shim.
    dev_run_image: RocTarget,
};

fn lowerLirWithBuildEnv(
    ctx: *CliCtx,
    lir_allocator: Allocator,
    artifact: PlatformEntrypointArtifact,
    roc_file_path: []const u8,
    source_dir_override: ?[]const u8,
    synthetic_default_app: ?SyntheticDefaultAppMapping,
    max_threads: ?usize,
    opt: cli_args.OptLevel,
    resolution_config: compile.package_resolution.Config,
    enable_checked_cache: bool,
    reporter: ?*progress.Reporter,
    allow_user_errors: bool,
) CliMainError!LoweredCoordinatorResult {
    var build_env = try initCliBuildEnv(ctx, .{
        .max_threads = max_threads,
        .no_cache = !enable_checked_cache,
        .resolution_config = resolution_config,
        .track_watch_inputs = true,
        .source_dir_override = source_dir_override,
        .post_check_publication_mode = if (allow_user_errors)
            .executable_artifacts_allow_user_errors
        else
            .executable_artifacts,
    });
    defer build_env.deinit();
    if (synthetic_default_app) |mapping| {
        // Diagnostics for a staged default app must point at the user's real
        // file (and its real line numbers) rather than the staged copy; the
        // mapping also pins the stable synthetic package identities.
        build_env.setSyntheticRootSourceMappingWithLineOffset(
            mapping.original_path,
            mapping.original_source,
            default_app_run_header.len,
            countNewlines(default_app_run_header),
        );
    }

    const display_path = if (synthetic_default_app) |mapping| mapping.original_path else roc_file_path;

    if (reporter) |r| r.begin("Resolving Dependencies");
    build_env.discoverDependencies(roc_file_path) catch |err| {
        if (reporter) |r| r.fail();
        _ = try renderDrainedBuildEnvReports(ctx, &build_env, display_path);
        return switch (err) {
            error.OutOfMemory => error.OutOfMemory,
            error.FileNotFound => ctx.fail(.{ .file_not_found = .{
                .path = roc_file_path,
                .context = .source_file,
            } }),
            else => |e| e,
        };
    };

    // `roc run` executes apps. Reject other roots before compiling so the
    // executable-artifact machinery below never sees an app-less workspace.
    {
        const pkg_name = build_env.discovered_pkg_name orelse return error.CliError;
        const root_pkg = build_env.packages.get(pkg_name) orelse return error.CliError;
        if (root_pkg.kind != .app and root_pkg.kind != .default_app) {
            if (reporter) |r| r.fail();
            return ctx.fail(.{ .expected_app_header = .{
                .path = display_path,
                .found = @tagName(root_pkg.kind),
            } });
        }
    }
    if (reporter) |r| r.end();

    if (reporter) |r| r.begin("Type Checking");
    build_env.compileDiscovered() catch |err| {
        if (reporter) |r| r.fail();
        _ = try renderDrainedBuildEnvReports(ctx, &build_env, display_path);
        return switch (err) {
            error.OutOfMemory => error.OutOfMemory,
            else => |e| e,
        };
    };

    const counts = try renderDrainedBuildEnvReports(ctx, &build_env, display_path);
    const watch_inputs = try build_env.collectWatchInputStates();
    errdefer compile.watch_inputs.deinit(ctx.gpa, watch_inputs);

    if (!build_env.executable_artifacts_finalized) {
        if (reporter) |r| r.fail();
        return .{
            .lowered = null,
            .internal_static_data = null,
            .entrypoint_names = &.{},
            .hosted_symbols = &.{},
            .checked_host_identity = null,
            .watch_inputs = watch_inputs,
            .watch_inputs_allocator = ctx.gpa,
            .counts = counts,
        };
    }
    if (reporter) |r| r.endWithBreakdown(&frontEndBreakdown(build_env.getTimingInfo()));

    const root_artifact = build_env.executableRootCheckedArtifact();
    const imported_artifacts = try build_env.collectImportedArtifactViews(ctx.gpa, root_artifact);
    defer ctx.gpa.free(imported_artifacts);
    const relation_artifacts = try build_env.collectRelationArtifactViews(ctx.gpa, root_artifact);
    defer ctx.gpa.free(relation_artifacts);

    if (reporter) |r| r.begin("Specializing");
    var lowered = try lowerCheckedSourceToLir(
        lir_allocator,
        ctx.gpa,
        root_artifact,
        imported_artifacts,
        relation_artifacts,
        .{ .platform_entrypoints = artifact },
        opt,
        base.target.TargetUsize.native,
        false,
    );
    errdefer lowered.deinit();
    if (reporter) |r| r.end();

    const internal_static_data: ?[]backend.StaticDataExport = switch (artifact) {
        .lir_image => null,
        .dev_run_image => |target| try compile.static_data_exports.buildStaticData(
            ctx.gpa,
            .{
                .root = check.CheckedArtifact.loweringViewWithRelations(root_artifact, relation_artifacts),
                .imports = imported_artifacts,
            },
            &lowered,
            target,
            .{},
        ),
    };
    errdefer if (internal_static_data) |static_data| {
        compile.static_data_exports.deinitStaticData(ctx.gpa, static_data);
    };

    const entrypoint_names = try lowered.platformEntrypointNames(ctx.arena, root_artifact);
    const hosted_table = try checkedHostedTable(
        ctx.arena,
        root_artifact,
        imported_artifacts,
        relation_artifacts,
    );
    const platform_entrypoints = try lowered.platformEntrypoints(ctx.gpa);
    defer ctx.gpa.free(platform_entrypoints);
    const checked_host_identity = try checkedInterpreterHostIdentity(
        ctx.gpa,
        root_artifact,
        &lowered.lir_result.store,
        &lowered.lir_result.layouts,
        platform_entrypoints,
        entrypoint_names,
        lowered.target_usize,
        hosted_table,
    );

    return .{
        .lowered = lowered,
        .internal_static_data = internal_static_data,
        .entrypoint_names = entrypoint_names,
        .hosted_symbols = hosted_table.symbols,
        .checked_host_identity = checked_host_identity,
        .watch_inputs = watch_inputs,
        .watch_inputs_allocator = ctx.gpa,
        .counts = counts,
    };
}

/// Build shared memory containing a viewable ARC-inserted LIR image.
///
/// The parent process owns parse, canonicalization, checking, checked module
/// publication, post-check lowering, LIR lowering, and ARC insertion. The child
/// process maps only the LIR image and never sees `ModuleEnv`, CIR, checked
/// modules, or post-check IRs.
pub fn buildLirImageWithBuildEnv(
    ctx: *CliCtx,
    roc_file_path: []const u8,
    source_dir_override: ?[]const u8,
    synthetic_default_app: ?SyntheticDefaultAppMapping,
    max_threads: ?usize,
    opt: cli_args.OptLevel,
    resolution_config: compile.package_resolution.Config,
    enable_checked_cache: bool,
    reporter: ?*progress.Reporter,
    allow_user_errors: bool,
) CliMainError!SharedMemoryResult {
    // Create shared memory with SharedMemoryAllocator, trying progressively smaller
    // sizes if larger ones fail (e.g., due to valgrind or overcommit-disabled Linux)
    const page_size = try SharedMemoryAllocator.getSystemPageSize();
    var shm = try createSharedMemory(ctx.io.std_io, page_size);
    errdefer shm.deinit(ctx.gpa);

    const shm_allocator = shm.allocator();
    const image_header = try shm_allocator.create(lir.LirImage.Header);

    var lowered_result = try lowerLirWithBuildEnv(
        ctx,
        shm_allocator,
        .lir_image,
        roc_file_path,
        source_dir_override,
        synthetic_default_app,
        max_threads,
        opt,
        resolution_config,
        enable_checked_cache,
        reporter,
        allow_user_errors,
    );
    defer lowered_result.deinitWatchInputs();

    if (lowered_result.counts.errors > 0 and lowered_result.lowered == null) {
        shm.updateHeader();
        return sharedMemoryResult(&shm, lowered_result.counts, &.{}, &.{}, null);
    }

    const lowered = if (lowered_result.lowered) |*program| program else {
        if (builtin.mode == .Debug) {
            std.debug.panic("LIR image invariant violated: successful coordinator lowering produced no lowered program", .{});
        }
        unreachable;
    };
    const platform_entrypoints = try lowered.platformEntrypoints(shm_allocator);
    try lir.LirImage.fillHeaderInSharedMemory(
        image_header,
        shm.base_ptr,
        shm.getUsedSize(),
        &lowered.lir_result,
        platform_entrypoints,
    );

    shm.updateHeader();
    return sharedMemoryResult(
        &shm,
        lowered_result.counts,
        lowered_result.entrypoint_names,
        lowered_result.hosted_symbols,
        lowered_result.checked_host_identity,
    );
}

/// Platform resolution result containing the platform source path
pub const PlatformPaths = struct {
    platform_source_path: ?[]const u8, // Optional - may not exist for some platforms
};

/// Resolve platform specification from a Roc file to find both host library and platform source.
/// Returns PlatformPaths with arena-allocated paths (no need to free).
pub fn resolvePlatformPaths(ctx: *CliCtx, roc_file_path: []const u8) (CliError || Allocator.Error)!PlatformPaths {
    const header_info = try parseCliAppHeader(ctx, roc_file_path);
    const app_dir = std.fs.path.dirname(roc_file_path) orelse ".";
    return resolvePlatformRefToPaths(ctx, header_info.platform_ref, roc_file_path, app_dir);
}

fn parseCliAppHeader(ctx: *CliCtx, app_file_path: []const u8) (Allocator.Error || error{CliError})!compile.app_header.AppHeaderInfo {
    return compile.app_header.parseAppHeader(ctx.coreCtx(), ctx.gpa, ctx.arena, app_file_path) catch |err| switch (err) {
        error.OutOfMemory => error.OutOfMemory,
        error.NotAnAppHeader => ctx.fail(.{ .expected_app_header = .{
            .path = app_file_path,
            .found = "non-app",
        } }),
        error.FileNotFound => ctx.fail(.{ .file_not_found = .{
            .path = app_file_path,
            .context = .source_file,
        } }),
        error.AccessDenied => ctx.fail(.{ .file_read_failed = .{
            .path = app_file_path,
            .err = error.AccessDenied,
        } }),
        error.StreamTooLong => ctx.fail(.{ .file_read_failed = .{
            .path = app_file_path,
            .err = error.StreamTooLong,
        } }),
        error.IoError => ctx.fail(.{ .file_read_failed = .{
            .path = app_file_path,
            .err = error.ReadFailed,
        } }),
    };
}

fn resolvePlatformRefToPaths(
    ctx: *CliCtx,
    platform_ref: compile.app_header.PlatformRef,
    app_file_path: []const u8,
    base_dir: []const u8,
) (CliError || Allocator.Error)!PlatformPaths {
    return switch (platform_ref) {
        .none => ctx.fail(.{ .expected_platform_string = .{ .path = app_file_path } }),
        .path_or_url => |platform_spec| resolvePlatformSpecToPaths(ctx, platform_spec, base_dir),
        .compiler_owned => |platform| blk: {
            const materialized = compile.compiler_platforms.materialize(ctx.arena, ctx.coreCtx(), null, platform) catch |err| switch (err) {
                error.OutOfMemory => return error.OutOfMemory,
                error.NoHomeDirectory => return ctx.fail(.{ .cache_dir_unavailable = .{
                    .reason = "Could not determine cache directory",
                } }),
                error.AccessDenied => return ctx.fail(.{ .file_write_failed = .{
                    .path = compile.compiler_platforms.identity(platform),
                    .err = error.AccessDenied,
                } }),
                error.IoError => return ctx.fail(.{ .file_write_failed = .{
                    .path = compile.compiler_platforms.identity(platform),
                    .err = error.WriteFailed,
                } }),
            };
            break :blk .{ .platform_source_path = materialized.root_file };
        },
    };
}

/// Check if platform spec is an absolute path and reject it.
/// Uses CliCtx for error reporting.
fn validatePlatformSpec(ctx: *CliCtx, platform_spec: []const u8) CliError!void {
    if (std.fs.path.isAbsolute(platform_spec)) {
        return ctx.fail(.{ .absolute_platform_path = .{ .platform_spec = platform_spec } });
    }
}

/// Resolve a platform specification to a platform source path.
/// Uses CliCtx for error reporting.
fn resolvePlatformSpecToPaths(ctx: *CliCtx, platform_spec: []const u8, base_dir: []const u8) CliError!PlatformPaths {
    // Handle URL-based platforms
    if (std.mem.startsWith(u8, platform_spec, "http")) {
        return resolveUrlPlatform(ctx, platform_spec) catch |err| switch (err) {
            error.CliError => return error.CliError,
            error.OutOfMemory => return ctx.fail(.{ .cache_dir_unavailable = .{
                .reason = "Out of memory while resolving URL platform",
            } }),
        };
    }

    // Check for absolute paths and reject them
    try validatePlatformSpec(ctx, platform_spec);

    // Try to interpret as a file path (must be relative, resolve relative to base_dir)
    const resolved_path = std.fs.path.join(ctx.arena, &.{ base_dir, platform_spec }) catch {
        return ctx.fail(.{ .file_read_failed = .{
            .path = platform_spec,
            .err = error.OutOfMemory,
        } });
    };

    std.Io.Dir.cwd().access(ctx.io.std_io, resolved_path, .{}) catch {
        return ctx.fail(.{ .platform_not_found = .{
            .app_path = base_dir,
            .platform_path = resolved_path,
        } });
    };

    // Platform spec should point to a .roc file
    if (std.mem.endsWith(u8, resolved_path, ".roc")) {
        return PlatformPaths{
            .platform_source_path = ctx.arena.dupe(u8, resolved_path) catch {
                return ctx.fail(.{ .file_read_failed = .{
                    .path = resolved_path,
                    .err = error.OutOfMemory,
                } });
            },
        };
    } else {
        // Non-.roc file path - not supported
        return ctx.fail(.{ .platform_validation_failed = .{
            .message = "Platform path must end with .roc",
        } });
    }
}

/// Get the roc cache directory for downloaded packages, creating it if needed.
/// Standard cache locations by platform:
/// - Linux/macOS: ~/.cache/roc/packages/ (respects XDG_CACHE_HOME if set)
/// - Windows: %LOCALAPPDATA%\roc\packages\
fn getRocCacheDir(allocator: std.mem.Allocator) (Allocator.Error || error{NoCacheDir})![]const u8 {
    // Check XDG_CACHE_HOME first (Linux/macOS)
    if (try getEnvVar(allocator, "XDG_CACHE_HOME")) |xdg_cache| {
        defer allocator.free(xdg_cache);
        return std.fs.path.join(allocator, &.{ xdg_cache, "roc", "packages" });
    }

    // Fall back to %LOCALAPPDATA%\roc\packages (Windows)
    if (comptime builtin.os.tag == .windows) {
        if (try getEnvVar(allocator, "LOCALAPPDATA")) |local_app_data| {
            defer allocator.free(local_app_data);
            return std.fs.path.join(allocator, &.{ local_app_data, "roc", "packages" });
        }
    }

    // Fall back to ~/.cache/roc/packages (Unix)
    if (try getEnvVar(allocator, "HOME")) |home| {
        defer allocator.free(home);
        return std.fs.path.join(allocator, &.{ home, ".cache", "roc", "packages" });
    }

    return error.NoCacheDir;
}

/// Cross-platform helper to get environment variable.
/// Returns null if the variable is not set. Caller must free the returned slice.
fn getEnvVar(allocator: std.mem.Allocator, key: []const u8) std.mem.Allocator.Error!?[]const u8 {
    const key_z = try allocator.dupeZ(u8, key);
    defer allocator.free(key_z);
    const value = std.c.getenv(key_z) orelse return null;
    const len = std.mem.len(value);
    return try allocator.dupe(u8, value[0..len]);
}

/// Convert parsed CLI size-limit flags to a resolution config.
fn resolutionConfigFromLimits(limits: cli_args.ResolveLimitArgs) compile.package_resolution.Config {
    var config = compile.package_resolution.Config{};
    if (limits.max_package_mb) |mb| {
        config.max_package_expanded_bytes = if (mb == 0) null else @as(u64, mb) * 1024 * 1024;
    }
    if (limits.max_transitive_mb) |mb| {
        config.max_transitive_expanded_bytes = if (mb == 0) null else @as(u64, mb) * 1024 * 1024;
    }
    return config;
}

const ResolvedUrlBundle = struct {
    source_path: []const u8,
};

/// Resolve a URL bundle (platform or package) by downloading and caching it.
/// The URL must point to a .tar.zst bundle with a base58-encoded BLAKE3 hash filename.
/// Returns the path to `main.roc` inside the cache directory.
/// Validate a bundle URL (https/loopback-http gate plus hash and version
/// syntax) and report a specific diagnostic for each way it can be invalid.
fn validateBundleUrl(ctx: *CliCtx, url: []const u8) (CliError || error{OutOfMemory})!base.url.ParsedUrl {
    return unbundle.download.validateUrl(url) catch |err| switch (err) {
        error.InvalidVersion => ctx.fail(.{ .invalid_url = .{
            .url = url,
            .reason = "This URL uses the reserved version 0.0.0, which means \"no version\". The lowest publishable version is 0.0.1.",
        } }),
        error.AmbiguousVersion => ctx.fail(.{ .invalid_url = .{
            .url = url,
            .reason = "This URL contains more than one version number. A package URL must contain exactly one MAJOR.MINOR.PATCH version before its hash.",
        } }),
        else => ctx.fail(.{ .invalid_url = .{
            .url = url,
            .reason = "Invalid URL format or missing hash. URLs must end with a base58-encoded BLAKE3 hash filename (e.g., '<hash>.tar.zst').",
        } }),
    };
}

fn resolveUrlBundle(ctx: *CliCtx, url: []const u8) (CliError || error{OutOfMemory})!ResolvedUrlBundle {
    const download = unbundle.download;

    // 1. Validate URL and extract hash
    const parsed_url = try validateBundleUrl(ctx, url);
    const base58_hash = parsed_url.hash;

    // 2. Get cache directory
    const cache_dir_path = getRocCacheDir(ctx.arena) catch {
        return ctx.fail(.{ .cache_dir_unavailable = .{ .reason = "Could not determine cache directory" } });
    };
    const package_dir_path = try std.fs.path.join(ctx.arena, &.{ cache_dir_path, base58_hash });

    // 3. Check if already cached
    const already_cached = blk: {
        var d = std.Io.Dir.cwd().openDir(ctx.io.std_io, package_dir_path, .{}) catch |err| switch (err) {
            error.FileNotFound => break :blk false,
            else => return ctx.fail(.{ .directory_not_found = .{ .path = package_dir_path } }),
        };
        d.close(ctx.io.std_io);
        break :blk true;
    };

    if (!already_cached) {
        // Not cached - need to download
        std.log.info("Downloading bundle from {s}...", .{url});

        // Create cache directory structure
        ensureCompilerCacheDirExists(ctx.io.std_io, cache_dir_path) catch |make_err| {
            return ctx.fail(.{ .directory_create_failed = .{
                .path = cache_dir_path,
                .err = make_err,
            } });
        };

        // Create package directory
        std.Io.Dir.cwd().createDir(ctx.io.std_io, package_dir_path, .default_dir) catch |make_err| switch (make_err) {
            error.PathAlreadyExists => {}, // Race condition, another process created it
            else => {
                return ctx.fail(.{ .directory_create_failed = .{
                    .path = package_dir_path,
                    .err = make_err,
                } });
            },
        };

        // Download and extract (path-based, no Dir handle needed)
        var gpa_copy = ctx.gpa;
        _ = download.downloadAndExtract(&gpa_copy, ctx.io.std_io, url, package_dir_path, .{}) catch |download_err| {
            std.Io.Dir.cwd().deleteTree(ctx.io.std_io, package_dir_path) catch {};
            return ctx.fail(.{ .download_failed = .{
                .url = url,
                .err = download_err,
            } });
        };

        std.log.info("Bundle cached at {s}", .{package_dir_path});
    }

    // Platforms must have a main.roc entry point
    const platform_source_path = try std.fs.path.join(ctx.arena, &.{ package_dir_path, "main.roc" });
    std.Io.Dir.cwd().access(ctx.io.std_io, platform_source_path, .{}) catch {
        // The problem is rendered after this frame returns, so the slice of
        // searched paths must live on the arena, not this stack frame.
        const searched_paths = try ctx.arena.alloc([]const u8, 1);
        searched_paths[0] = platform_source_path;
        return ctx.fail(.{ .platform_source_not_found = .{
            .platform_path = package_dir_path,
            .searched_paths = searched_paths,
        } });
    };

    return .{
        .source_path = platform_source_path,
    };
}

/// Default output basename for `roc build <url>`: the last path segment of
/// the URL's package id (the part before the version and content hash), e.g.
/// `tokei` for `https://example.com/tokei/1.2.3/<hash>.tar.zst`. Null when
/// the URL has no usable segment, in which case the module name is used.
fn urlDefaultOutputBasename(url: []const u8) ?[]const u8 {
    const parsed = base.url.parseUrlPath(url) catch return null;
    var it = std.mem.splitBackwardsScalar(u8, parsed.url_id.prefix(url), '/');
    while (it.next()) |segment| {
        if (segment.len == 0) continue;
        // The host segment can carry a port; a colon is not a usable filename
        // on Windows, so fall back to the module name instead.
        if (std.mem.findScalar(u8, segment, ':') != null) return null;
        return segment;
    }
    return null;
}

test "urlDefaultOutputBasename derives the package segment" {
    try std.testing.expectEqualStrings("tokei", urlDefaultOutputBasename("https://example.com/tokei/1.2.3/AQmoxbAY7eQfXMbi9XUxBvBGZcxZCs1tdNeFriRRkwSc.tar.zst").?);
    try std.testing.expectEqualStrings("thing", urlDefaultOutputBasename("https://example.com/thing/AQmoxbAY7eQfXMbi9XUxBvBGZcxZCs1tdNeFriRRkwSc.tar.zst").?);
    // A bare host:port segment is not a usable filename on Windows.
    try std.testing.expect(urlDefaultOutputBasename("http://127.0.0.1:8642/AQmoxbAY7eQfXMbi9XUxBvBGZcxZCs1tdNeFriRRkwSc.tar.zst") == null);
    try std.testing.expect(urlDefaultOutputBasename("not a url") == null);
}

/// A staging directory older than this cannot belong to a live install and
/// is safe to reclaim.
const stale_staging_max_age_ns: i128 = std.time.ns_per_day;

/// Reclaim staging directories stranded by interrupted installs. The install
/// root is deliberately outside every cache cleanup, and each staging name
/// embeds a random suffix no later install reuses, so nothing else ever
/// deletes them. Best-effort: any failure just leaves the sweep to a future
/// install.
fn sweepStaleStagingDirs(std_io: std.Io, version_dir: []const u8) void {
    var dir = std.Io.Dir.cwd().openDir(std_io, version_dir, .{ .iterate = true }) catch return;
    defer dir.close(std_io);

    const now_ns: i128 = std.Io.Timestamp.now(std_io, .real).nanoseconds;
    var it = dir.iterate();
    while (true) {
        const entry = (it.next(std_io) catch break) orelse break;
        if (entry.kind != .directory) continue;
        if (!std.mem.startsWith(u8, entry.name, ".staging-")) continue;

        const info = dir.statFile(std_io, entry.name, .{}) catch continue;
        const mtime_ns: i128 = @intCast(info.mtime.nanoseconds);
        if (now_ns - mtime_ns <= stale_staging_max_age_ns) continue;

        dir.deleteTree(std_io, entry.name) catch {};
    }
}

/// Install a bundle URL under a shorthand: download and verify it into a
/// staging directory, build it with --opt=speed for the host target, then
/// publish the completed entry with a single atomic rename so a partial
/// installation is never visible. After this succeeds, `roc run <shorthand>`
/// needs neither the network nor any cache.
fn rocInstall(ctx: *CliCtx, args: cli_args.InstallArgs, arg0: []const u8) CliMainError!void {
    if (!install_store.isValidShorthand(args.shorthand)) {
        return ctx.fail(.{ .invalid_shorthand = .{ .name = args.shorthand } });
    }
    const parsed_url = try validateBundleUrl(ctx, args.url);

    const root = install_store.installRootDir(ctx.coreCtx(), ctx.arena) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        error.NoHomeDirectory => return ctx.fail(.{ .install_dir_unavailable = .{
            .reason = "No home directory could be determined",
        } }),
    };
    const version_dir = try install_store.versionDir(ctx.arena, root);
    const entry = try install_store.entryPaths(ctx.arena, version_dir, args.shorthand);

    // Same name + same URL is idempotent; same name + different URL fails
    // without touching the existing entry.
    const existing_bytes: ?[]u8 = std.Io.Dir.cwd().readFileAlloc(ctx.io.std_io, entry.manifest_path, ctx.arena, .limited(install_manifest_size_limit)) catch |err| switch (err) {
        error.FileNotFound => existing: {
            var entry_dir = std.Io.Dir.cwd().openDir(ctx.io.std_io, entry.entry_dir, .{}) catch break :existing null;
            entry_dir.close(ctx.io.std_io);
            return ctx.fail(.{ .install_entry_corrupt = .{
                .name = args.shorthand,
                .path = entry.entry_dir,
                .reason = "its install.json manifest is missing",
            } });
        },
        else => return ctx.fail(.{ .install_entry_corrupt = .{
            .name = args.shorthand,
            .path = entry.entry_dir,
            .reason = "its install.json manifest could not be read",
        } }),
    };
    if (existing_bytes) |bytes| {
        var parsed = (try install_store.parseManifest(ctx.gpa, bytes)) orelse {
            return ctx.fail(.{ .install_entry_corrupt = .{
                .name = args.shorthand,
                .path = entry.entry_dir,
                .reason = "its install.json manifest is not valid",
            } });
        };
        defer parsed.deinit();
        if (std.mem.eql(u8, parsed.manifest().url, args.url)) {
            // Same name + same URL: an intact entry makes this a no-op, and a
            // damaged one is safe to repair since the URL matches.
            // parseManifest already validated the kind string.
            const existing_kind = install_store.manifestKind(parsed.manifest()).?;
            const artifact_intact = intact: {
                std.Io.Dir.cwd().access(ctx.io.std_io, entry.artifactPath(existing_kind), .{}) catch break :intact false;
                break :intact true;
            };
            if (artifact_intact) {
                switch (existing_kind) {
                    .executable => try ctx.io.stdout().print("`{s}` is already installed. Run it with: roc run {s}\n", .{ args.shorthand, args.shorthand }),
                    .glue => try ctx.io.stdout().print("`{s}` is already installed. Use it with: roc glue {s} <output-dir> <platform>\n", .{ args.shorthand, args.shorthand }),
                }
                return;
            }
            std.Io.Dir.cwd().deleteTree(ctx.io.std_io, entry.entry_dir) catch {
                return ctx.fail(.{ .install_entry_corrupt = .{
                    .name = args.shorthand,
                    .path = entry.entry_dir,
                    .reason = "its built artifact is missing, and the damaged entry could not be removed for repair",
                } });
            };
        } else {
            const existing_url = try ctx.arena.dupe(u8, parsed.manifest().url);
            return ctx.fail(.{ .shorthand_conflict = .{
                .name = args.shorthand,
                .existing_url = existing_url,
                .new_url = args.url,
            } });
        }
    }

    std.Io.Dir.cwd().createDirPath(ctx.io.std_io, version_dir) catch |err| {
        return ctx.fail(.{ .directory_create_failed = .{ .path = version_dir, .err = err } });
    };

    sweepStaleStagingDirs(ctx.io.std_io, version_dir);

    // The `.` prefix keeps staging directories out of the shorthand namespace.
    var staging_suffix: [8]u8 = undefined;
    ctx.io.std_io.random(&staging_suffix);
    const staging_name = try std.fmt.allocPrint(ctx.arena, ".staging-{s}-{s}", .{ args.shorthand, std.fmt.bytesToHex(staging_suffix, .lower) });
    const staging_dir = try std.fs.path.join(ctx.arena, &.{ version_dir, staging_name });
    const staging = try install_store.entryPathsIn(ctx.arena, staging_dir, args.shorthand);
    defer std.Io.Dir.cwd().deleteTree(ctx.io.std_io, staging_dir) catch {};

    std.Io.Dir.cwd().createDirPath(ctx.io.std_io, staging.source_dir) catch |err| {
        return ctx.fail(.{ .directory_create_failed = .{ .path = staging.source_dir, .err = err } });
    };
    std.Io.Dir.cwd().createDirPath(ctx.io.std_io, staging.bin_dir) catch |err| {
        return ctx.fail(.{ .directory_create_failed = .{ .path = staging.bin_dir, .err = err } });
    };

    try ctx.io.stdout().print("Downloading {s} ...\n", .{args.url});
    ctx.io.flush();

    const limits_config = resolutionConfigFromLimits(args.resolve_limits);
    var gpa_copy = ctx.gpa;
    _ = unbundle.download.downloadAndExtract(&gpa_copy, ctx.io.std_io, args.url, staging.source_dir, .{
        .max_expanded_bytes = limits_config.max_package_expanded_bytes,
    }) catch |download_err| {
        return ctx.fail(.{ .download_failed = .{ .url = args.url, .err = download_err } });
    };

    // The downloader leaves the compressed bundle next to the extraction for
    // cache reuse; an install entry is not a cache, so drop it.
    const kept_tar_name = try std.fmt.allocPrint(ctx.arena, "{s}.tar.zst", .{parsed_url.hash});
    const kept_tar_path = try std.fs.path.join(ctx.arena, &.{ staging.source_dir, kept_tar_name });
    std.Io.Dir.cwd().deleteFile(ctx.io.std_io, kept_tar_path) catch {};

    std.Io.Dir.cwd().access(ctx.io.std_io, staging.main_roc_path, .{}) catch {
        return ctx.fail(.{ .install_bundle_missing_main = .{
            .url = args.url,
            .searched_path = staging.main_roc_path,
        } });
    };

    // An app on a compiler-owned plugin platform (a glue spec) builds to a
    // plugin dylib; every other bundle goes through the executable pipeline,
    // which owns the diagnostics for non-app headers. Header-parse failures
    // are classified as executable for the same reason: that pipeline
    // reports them properly.
    const install_kind: install_store.InstallKind = kind: {
        const header = compile.app_header.parseAppHeader(ctx.coreCtx(), ctx.gpa, ctx.arena, staging.main_roc_path) catch break :kind .executable;
        break :kind switch (header.platform_ref) {
            .compiler_owned => |plugin_platform| switch (plugin_platform) {
                .glue => .glue,
            },
            else => .executable,
        };
    };

    switch (install_kind) {
        .executable => {
            try ctx.io.stdout().print("Building {s} with --opt=speed ...\n", .{args.shorthand});
            ctx.io.flush();

            var warning_count: usize = 0;
            try rocBuild(ctx, .{
                .path = staging.main_roc_path,
                .opt = .speed,
                .target = null,
                .output = staging.exe_path,
                .debug = false,
                .allow_errors = false,
                .verbose = false,
                .no_cache = false,
                .max_threads = args.max_threads,
                .wasm_memory = null,
                .wasm_stack_size = null,
                .exit_on_warnings = false,
                .warning_count_out = &warning_count,
                .require_executable_output = true,
                .require_host_runnable_output = true,
                .suppress_build_status = true,
                .resolve_limits = args.resolve_limits,
                .synthetic_default_platform = false,
                .source_dir_override = null,
                .root_source_url = args.url,
            }, arg0);
        },
        .glue => {
            try ctx.io.stdout().print("Building {s} glue plugin with --opt=speed ...\n", .{args.shorthand});
            ctx.io.flush();

            try glue.buildGlueSpecDylibFile(ctx.gpa, ctx.io.stderr(), staging.main_roc_path, staging.glue_dylib_path, .speed, ctx.io.std_io);
        },
    }

    // The manifest is written last, so a staged entry is complete by the time
    // it can be published.
    const manifest_json = try install_store.manifestToJson(ctx.arena, .{
        .format_version = install_store.manifest_format_version,
        .kind = @tagName(install_kind),
        .url = args.url,
        .hash = parsed_url.hash,
        .compiler_version = build_options.compiler_version,
    });
    std.Io.Dir.cwd().writeFile(ctx.io.std_io, .{ .sub_path = staging.manifest_path, .data = manifest_json }) catch |err| {
        return ctx.fail(.{ .file_write_failed = .{ .path = staging.manifest_path, .err = err } });
    };

    std.Io.Dir.cwd().rename(staging_dir, std.Io.Dir.cwd(), entry.entry_dir, ctx.io.std_io) catch |rename_err| {
        // A concurrent install of the same shorthand may have published first;
        // a same-URL winner makes this install a success with redundant work.
        const winner_bytes = std.Io.Dir.cwd().readFileAlloc(ctx.io.std_io, entry.manifest_path, ctx.arena, .limited(install_manifest_size_limit)) catch {
            return ctx.fail(.{ .install_publish_failed = .{ .name = args.shorthand, .err = rename_err } });
        };
        var winner = (try install_store.parseManifest(ctx.gpa, winner_bytes)) orelse {
            return ctx.fail(.{ .install_publish_failed = .{ .name = args.shorthand, .err = rename_err } });
        };
        defer winner.deinit();
        if (!std.mem.eql(u8, winner.manifest().url, args.url)) {
            const existing_url = try ctx.arena.dupe(u8, winner.manifest().url);
            return ctx.fail(.{ .shorthand_conflict = .{
                .name = args.shorthand,
                .existing_url = existing_url,
                .new_url = args.url,
            } });
        }
    };

    switch (install_kind) {
        .executable => try ctx.io.stdout().print("Installed {s}. Run it with: roc run {s}\n", .{ args.shorthand, args.shorthand }),
        .glue => try ctx.io.stdout().print("Installed {s}. Use it with: roc glue {s} <output-dir> <platform>\n", .{ args.shorthand, args.shorthand }),
    }
}

/// Resolve a URL platform specification by downloading and caching the bundle.
/// The URL must point to a .tar.zst bundle with a base58-encoded BLAKE3 hash filename.
fn resolveUrlPlatform(ctx: *CliCtx, url: []const u8) (CliError || error{OutOfMemory})!PlatformPaths {
    const resolved = try resolveUrlBundle(ctx, url);
    return PlatformPaths{
        .platform_source_path = resolved.source_path,
    };
}

/// Extract the selected embedded shim library to the specified path for the given target.
fn extractShimLibrary(ctx: *CliCtx, kind: ShimLibraryKind, output_path: []const u8, target: ?RocTarget) (std.Io.File.OpenError || std.Io.File.Writer.Error)!void {
    if (builtin.is_test) {
        // In test mode, create an empty file to avoid embedding issues
        const shim_file = try std.Io.Dir.cwd().createFile(ctx.io.std_io, output_path, .{});
        defer shim_file.close(ctx.io.std_io);
        return;
    }

    // Write the embedded shim library to the output path
    const shim_file = try std.Io.Dir.cwd().createFile(ctx.io.std_io, output_path, .{});
    defer shim_file.close(ctx.io.std_io);

    try shim_file.writeStreamingAll(ctx.io.std_io, shimLibraryBytes(kind, target));
}

/// Format a bundle path validation reason into a user-friendly error message
fn formatBundlePathValidationReason(reason: bundle.PathValidationReason) []const u8 {
    return switch (reason) {
        .empty_path => "Path cannot be empty",
        .path_too_long => "Path exceeds maximum length of 255 characters",
        .windows_reserved_char => |char| switch (char) {
            0 => "Path contains NUL byte (\\0)",
            ':' => "Path contains colon (:) which is reserved on Windows",
            '*' => "Path contains asterisk (*) which is a wildcard on Windows",
            '?' => "Path contains question mark (?) which is a wildcard on Windows",
            '"' => "Path contains quote (\") which is reserved on Windows",
            '<' => "Path contains less-than (<) which is reserved on Windows",
            '>' => "Path contains greater-than (>) which is reserved on Windows",
            '|' => "Path contains pipe (|) which is reserved on Windows",
            '\\' => "Path contains backslash (\\). Use forward slashes (/) for all paths",
            else => "Path contains reserved character",
        },
        .absolute_path => "Absolute paths are not allowed",
        .path_traversal => "Path traversal (..) is not allowed",
        .current_directory_reference => "Current directory reference (.) is not allowed",
        .contained_backslash_on_unix => "Path contains a backslash, which is a directory separator on Windows.",
        .windows_reserved_name => "Path contains Windows reserved device name (CON, PRN, AUX, NUL, COM1-9, LPT1-9)",
        .component_ends_with_space => "Path components cannot end with space",
        .component_ends_with_period => "Path components cannot end with period",
    };
}

/// Format an unbundle path validation reason into a user-friendly error message
fn formatUnbundlePathValidationReason(reason: unbundle.PathValidationReason) []const u8 {
    return switch (reason) {
        .empty_path => "Path cannot be empty",
        .path_too_long => "Path exceeds maximum length of 255 characters",
        .windows_reserved_char => |char| switch (char) {
            0 => "Path contains NUL byte (\\0)",
            ':' => "Path contains colon (:) which is reserved on Windows",
            '*' => "Path contains asterisk (*) which is a wildcard on Windows",
            '?' => "Path contains question mark (?) which is a wildcard on Windows",
            '"' => "Path contains quote (\") which is reserved on Windows",
            '<' => "Path contains less-than (<) which is reserved on Windows",
            '>' => "Path contains greater-than (>) which is reserved on Windows",
            '|' => "Path contains pipe (|) which is reserved on Windows",
            '\\' => "Path contains backslash (\\). Use forward slashes (/) for all paths",
            else => "Path contains reserved character",
        },
        .absolute_path => "Absolute paths are not allowed",
        .path_traversal => "Path traversal (..) is not allowed",
        .current_directory_reference => "Current directory reference (.) is not allowed",
        .contained_backslash_on_unix => "Path contains a backslash, which is a directory separator on Windows.",
        .windows_reserved_name => "Path contains Windows reserved device name (CON, PRN, AUX, NUL, COM1-9, LPT1-9)",
        .component_ends_with_space => "Path components cannot end with space",
        .component_ends_with_period => "Path components cannot end with period",
    };
}

/// Use the Coordinator to discover every transitive module the entry point
/// imports (directly, via re-exports, or via a `package [...]` header) and
/// append any not already in `file_paths` so the bundle includes them
/// automatically. `uncompressed_size` is updated to reflect the newly added
/// files. Also validates platform target binaries if a platform is found.
fn discoverAndAddBundleModules(
    ctx: *CliCtx,
    first_roc_file: []const u8,
    file_paths: *std.ArrayList([]const u8),
    uncompressed_size: *u64,
    stderr: anytype,
) CliMainError!void {
    // Resolve the entry point to an absolute path
    const abs_entry = std.Io.Dir.cwd().realPathFileAlloc(ctx.io.std_io, first_roc_file, ctx.gpa) catch |err| {
        try stderr.print("Error: Could not resolve path '{s}': {}\n", .{ first_roc_file, err });
        return err;
    };
    defer ctx.gpa.free(abs_entry);

    // Create a BuildEnv to parse headers and discover modules via the
    // Coordinator. Bundling compiles the workspace to discover transitive
    // modules; it uses the checked-module cache like every other pipeline.
    var build_env = try initCliBuildEnv(ctx, .{ .max_threads = 1 });
    defer build_env.deinit();

    // Run the build — the Coordinator discovers all transitive module dependencies
    build_env.build(abs_entry) catch {
        // Drain and display any errors from the build
        const drained = build_env.drainReports() catch &[_]BuildEnv.DrainedModuleReports{};
        defer build_env.freeDrainedReportsPathsOnly(drained);

        for (drained) |mod| {
            for (mod.reports) |report| {
                switch (report.severity) {
                    .runtime_error, .fatal => {
                        try stderr.print("{s}: error in module\n", .{mod.abs_path});
                    },
                    .warning => {
                        try stderr.print("{s}: warning in module\n", .{mod.abs_path});
                    },
                    .info => {},
                }
            }
        }
        // Build errors are not fatal for bundling — continue to check what we can
    };

    // Detect platform from BuildEnv packages using the accessor
    const platform_root_file = build_env.getPlatformRootFile();

    // Build a set of absolute paths already in the bundle list for dedup.
    var bundled_set = std.StringHashMap(void).init(ctx.gpa);
    defer bundled_set.deinit();

    for (file_paths.items) |rel_path| {
        const abs_path = std.Io.Dir.cwd().realPathFileAlloc(ctx.io.std_io, rel_path, ctx.gpa) catch continue;
        defer ctx.gpa.free(abs_path);
        try bundled_set.put(try ctx.arena.dupe(u8, abs_path), {});
    }

    // Append any discovered module that is not already in the bundle list.
    // URL dependencies are skipped: their files live in the local package
    // cache, and consumers resolve them from the URLs in the header.
    // The Coordinator yields absolute paths; convert each to a path relative
    // to cwd so it round-trips through `cwd.openFile` and survives the
    // bundle's path-validation step (which rejects absolute paths).
    if (build_env.coordinator) |coord| {
        var coord_pkg_it = coord.packages.iterator();
        while (coord_pkg_it.next()) |pkg_entry| {
            for (pkg_entry.value_ptr.*.modules.items) |mod_state| {
                const abs_path = mod_state.path;
                if (!build_env.isBundleableModule(pkg_entry.key_ptr.*, abs_path)) continue;
                if (bundled_set.contains(abs_path)) continue;

                const rel_path = std.fs.path.relative(ctx.arena, build_env.cwd, null, build_env.cwd, abs_path) catch {
                    try stderr.print("Error: Discovered module path is outside the current directory and cannot be bundled: {s}\n", .{abs_path});
                    return error.MissingBundleFiles;
                };

                // Confirm the file is actually readable from cwd before adding it.
                const file = std.Io.Dir.cwd().openFile(ctx.io.std_io, rel_path, .{}) catch |err| {
                    try stderr.print("Error: Could not open discovered module '{s}': {}\n", .{ rel_path, err });
                    return err;
                };
                const stat = file.stat(ctx.io.std_io) catch |err| {
                    file.close(ctx.io.std_io);
                    return err;
                };
                file.close(ctx.io.std_io);

                try file_paths.append(ctx.arena, rel_path);
                try bundled_set.put(try ctx.arena.dupe(u8, abs_path), {});
                uncompressed_size.* += stat.size;
            }
        }
    }

    // If a platform was detected, validate target binaries exist
    // Use TargetsConfig from BuildEnv (already extracted during header parsing)
    if (platform_root_file) |pf| {
        if (build_env.getPlatformTargetsConfig()) |tc| {
            const pf_dir = std.fs.path.dirname(pf) orelse ".";
            if (platform_validation.validateAllTargetFilesExist(ctx.arena, ctx.io.std_io, tc, pf_dir)) |result| {
                _ = platform_validation.renderValidationError(ctx.gpa, result, stderr);
                return switch (result) {
                    .missing_target_file => error.MissingTargetFile,
                    .missing_files_directory => error.MissingFilesDirectory,
                    else => error.MissingTargetFile,
                };
            }
        }
    }
}

/// Find the longest directory path that is an ancestor of every input in `abs_paths`.
/// All inputs must be absolute paths (they may be paths to files or directories).
/// Returns the common parent directory with no trailing path separator (except for
/// a filesystem root such as "/"). Returns an empty slice if the inputs share no
/// directory ancestor (e.g. two Windows paths on different drives).
fn longestCommonParentDir(allocator: std.mem.Allocator, abs_paths: []const []const u8) Allocator.Error![]u8 {
    std.debug.assert(abs_paths.len > 0);

    const isSep = struct {
        fn f(byte: u8) bool {
            return byte == '/' or byte == '\\';
        }
    }.f;

    // Start with the dirname of the first path.
    var common = std.ArrayList(u8).empty;
    errdefer common.deinit(allocator);
    const first_dir = std.fs.path.dirname(abs_paths[0]) orelse abs_paths[0];
    try common.appendSlice(allocator, first_dir);

    for (abs_paths[1..]) |path| {
        const dir = std.fs.path.dirname(path) orelse path;

        // Find longest byte prefix shared between `common` and `dir`.
        var i: usize = 0;
        const max = @min(common.items.len, dir.len);
        while (i < max and common.items[i] == dir[i]) : (i += 1) {}

        // i is on a directory boundary if it is at the end of either path
        // OR the next byte of either path is a separator.
        const at_boundary = (i == common.items.len and (i == dir.len or isSep(dir[i]))) or
            (i == dir.len and (i == common.items.len or isSep(common.items[i])));

        if (!at_boundary) {
            // Back up to the last separator within [0..i). Drop everything after it.
            var j: usize = i;
            while (j > 0) {
                j -= 1;
                if (isSep(common.items[j])) {
                    // Keep the separator only when it's the root sep at index 0.
                    i = if (j == 0) 1 else j;
                    break;
                }
            } else {
                i = 0;
            }
        }

        common.items.len = @min(common.items.len, i);
    }

    // Strip trailing separators (preserve a single root separator).
    while (common.items.len > 1 and isSep(common.items[common.items.len - 1])) {
        common.items.len -= 1;
    }

    return common.toOwnedSlice(allocator);
}

/// Bundles a roc package and its dependencies into a compressed tar archive
pub fn rocBundle(ctx: *CliCtx, args: cli_args.BundleArgs) CliMainError!void {
    const stdout = ctx.io.stdout();
    const stderr = ctx.io.stderr();

    // Start timing
    const start_time = std.Io.Timestamp.now(ctx.io.std_io, .real).nanoseconds;

    // Get current working directory
    const cwd = std.Io.Dir.cwd();

    // Determine output directory
    var output_dir = if (args.output_dir) |dir|
        try cwd.openDir(ctx.io.std_io, dir, .{})
    else
        cwd;
    defer if (args.output_dir != null) output_dir.close(ctx.io.std_io);

    // Create a temporary directory for the output file
    var tmp_dir = try std.Io.Dir.cwd().createDirPathOpen(ctx.io.std_io, ".roc_bundle_tmp", .{});
    defer {
        tmp_dir.close(ctx.io.std_io);
        std.Io.Dir.cwd().deleteTree(ctx.io.std_io, ".roc_bundle_tmp") catch {};
    }

    // Collect all files to bundle
    var file_paths = std.ArrayList([]const u8).empty;
    defer file_paths.deinit(ctx.arena);

    var uncompressed_size: u64 = 0;

    // If no paths provided, default to "main.roc"
    const paths_to_use = if (args.paths.len == 0) &[_][]const u8{"main.roc"} else args.paths;

    // Remember the first path from CLI args (before sorting)
    const first_cli_path = paths_to_use[0];

    // Detect whether any input path is absolute. Absolute paths are not allowed
    // inside the archive (the unbundle side rejects them, and a relative path
    // is what the user actually wants extracted). If any input is absolute we
    // rebase all paths against their longest common parent directory and pass
    // that directory to the bundle library — so the archive itself only ever
    // contains relative paths.
    var any_absolute = false;
    for (paths_to_use) |path| {
        if (std.fs.path.isAbsolute(path)) {
            any_absolute = true;
            break;
        }
    }

    // Check that all files exist and collect their sizes
    for (paths_to_use) |path| {
        const file = cwd.openFile(ctx.io.std_io, path, .{}) catch |err| {
            try stderr.print("Error: Could not open file '{s}': {}\n", .{ path, err });
            return err;
        };
        defer file.close(ctx.io.std_io);

        const stat = try file.stat(ctx.io.std_io);
        uncompressed_size += stat.size;

        try file_paths.append(ctx.arena, path);
    }

    // Find the first .roc file to use as entry point for module discovery
    const first_roc_file: ?[]const u8 = for (paths_to_use) |path| {
        if (std.mem.endsWith(u8, path, ".roc")) break path;
    } else null;

    // Use the Coordinator to discover all transitive module dependencies
    // (explicit imports plus modules exposed by a `package [...]` header)
    // and append any not already in the file list.
    if (first_roc_file) |roc_file| {
        try discoverAndAddBundleModules(ctx, roc_file, &file_paths, &uncompressed_size, stderr);
    }

    // Sort and deduplicate paths
    std.mem.sort([]const u8, file_paths.items, {}, struct {
        fn lessThan(_: void, a: []const u8, b: []const u8) bool {
            return std.mem.order(u8, a, b) == .lt;
        }
    }.lessThan);

    // Remove duplicates by keeping only unique consecutive elements
    var unique_count: usize = 0;
    for (file_paths.items, 0..) |path, i| {
        if (i == 0 or !std.mem.eql(u8, path, file_paths.items[i - 1])) {
            file_paths.items[unique_count] = path;
            unique_count += 1;
        }
    }
    file_paths.items.len = unique_count;

    // If we have more than one file, ensure the first CLI arg stays first
    if (file_paths.items.len > 1) {
        // Find the first CLI path in the sorted array
        var found_index: ?usize = null;
        for (file_paths.items, 0..) |path, i| {
            if (std.mem.eql(u8, path, first_cli_path)) {
                found_index = i;
                break;
            }
        }

        // Swap the found item with the first position if needed
        if (found_index) |idx| {
            if (idx != 0) {
                const temp = file_paths.items[0];
                file_paths.items[0] = file_paths.items[idx];
                file_paths.items[idx] = temp;
            }
        }
    }

    // If any input was absolute, rebase all paths against their longest common
    // parent directory so the archive only contains relative paths. The opened
    // directory becomes the base_dir passed to the bundle library.
    //
    // Discovery (above) added transitively imported modules as cwd-relative
    // paths; `realpathAlloc` resolves both forms so they share the common
    // parent uniformly here.
    var rebased_base_dir: ?std.Io.Dir = null;
    defer if (rebased_base_dir) |d| d.close(ctx.io.std_io);
    var archive_paths: []const []const u8 = file_paths.items;
    if (any_absolute) {
        const resolved = try ctx.arena.alloc([]u8, file_paths.items.len);
        for (file_paths.items, 0..) |p, i| {
            resolved[i] = cwd.realPathFileAlloc(ctx.io.std_io, p, ctx.arena) catch |err| {
                try stderr.print("Error: Could not resolve path '{s}': {}\n", .{ p, err });
                return err;
            };
        }

        const common = try longestCommonParentDir(ctx.arena, resolved);
        if (common.len == 0) {
            try stderr.print("Error: Input file paths have no common parent directory.\n", .{});
            return error.InvalidPath;
        }

        const opened_dir = std.Io.Dir.openDirAbsolute(ctx.io.std_io, common, .{}) catch |err| {
            try stderr.print("Error: Could not open common parent directory '{s}': {}\n", .{ common, err });
            return err;
        };
        rebased_base_dir = opened_dir;

        // Build relative-to-common paths for the archive.
        const rel_paths = try ctx.arena.alloc([]const u8, resolved.len);
        for (resolved, 0..) |abs, i| {
            // abs must start with `common`; everything after is the relative path.
            // common has no trailing separator, so skip the leading separator byte
            // that follows it within abs.
            if (abs.len <= common.len or
                !std.mem.eql(u8, abs[0..common.len], common) or
                !(abs[common.len] == '/' or abs[common.len] == '\\'))
            {
                try stderr.print("Error: Path '{s}' is not under the common parent '{s}'.\n", .{ abs, common });
                return error.InvalidPath;
            }
            rel_paths[i] = abs[common.len + 1 ..];
        }
        archive_paths = rel_paths;
    }

    // Create temporary output file
    const temp_filename = "temp_bundle.tar.zst";
    const temp_file = try tmp_dir.createFile(ctx.io.std_io, temp_filename, .{
        // Allow querying metadata (stat) on the handle, necessary for windows
        .read = true,
        .truncate = true,
    });
    defer temp_file.close(ctx.io.std_io);

    // Create file path iterator
    const FilePathIterator = struct {
        paths: []const []const u8,
        index: usize = 0,

        pub fn next(self: *@This()) Allocator.Error!?[]const u8 {
            if (self.index >= self.paths.len) return null;
            const path = self.paths[self.index];
            self.index += 1;
            return path;
        }
    };

    var iter = FilePathIterator{ .paths = archive_paths };

    // Bundle the files
    var allocator_copy = ctx.arena;
    var error_ctx: bundle.ErrorContext = undefined;
    var temp_writer_buffer: [4096]u8 = undefined;
    var temp_writer = temp_file.writerStreaming(ctx.io.std_io, &temp_writer_buffer);
    const bundle_base_dir = rebased_base_dir orelse cwd;
    const final_filename = bundle.bundleFiles(
        &iter,
        @intCast(args.compression_level),
        &allocator_copy,
        ctx.io.std_io,
        &temp_writer.interface,
        bundle_base_dir,
        null, // path_prefix parameter - null means no stripping
        &error_ctx,
    ) catch |err| {
        switch (err) {
            error.InvalidPath => {
                try stderr.print("Error: Invalid file path - {s}\n", .{formatBundlePathValidationReason(error_ctx.reason)});
                try stderr.print("Path: {s}\n", .{error_ctx.path});
            },
            else => {},
        }
        return err;
    };
    // No need to free when using arena allocator

    try temp_writer.interface.flush();

    // Get the compressed file size
    const compressed_stat = try temp_file.stat(ctx.io.std_io);
    const compressed_size = compressed_stat.size;

    // Move the temp file to the final location
    try tmp_dir.rename(temp_filename, output_dir, final_filename, ctx.io.std_io);

    // Calculate elapsed time
    const end_time = std.Io.Timestamp.now(ctx.io.std_io, .real).nanoseconds;
    const elapsed_ns = @as(u64, @intCast(end_time - start_time));
    const elapsed_ms = elapsed_ns / 1_000_000;

    // Calculate relative path for display
    const display_path = if (args.output_dir == null)
        final_filename
    else
        try std.fs.path.join(ctx.arena, &.{ args.output_dir.?, final_filename });
    // No need to free when using arena allocator

    // Print results
    try stdout.print("Created: {s}\n", .{display_path});
    try stdout.print("Compressed size: {} bytes\n", .{compressed_size});
    try stdout.print("Uncompressed size: {} bytes\n", .{uncompressed_size});
    try stdout.print("Compression ratio: {d:.2}:1\n", .{@as(f64, @floatFromInt(uncompressed_size)) / @as(f64, @floatFromInt(compressed_size))});
    try stdout.print("Time: {} ms\n", .{elapsed_ms});
}

fn rocUnbundle(ctx: *CliCtx, args: cli_args.UnbundleArgs) CliMainError!void {
    const stdout = ctx.io.stdout();
    const stderr = ctx.io.stderr();
    const cwd = std.Io.Dir.cwd();

    var had_errors = false;

    for (args.paths) |archive_path| {
        // Extract directory name from archive filename
        const basename = std.fs.path.basename(archive_path);
        var dir_name: []const u8 = undefined;

        if (std.mem.endsWith(u8, basename, ".tar.zst")) {
            dir_name = basename[0 .. basename.len - 8];
        } else {
            try stderr.print("Error: {s} is not a .tar.zst file\n", .{archive_path});
            had_errors = true;
            continue;
        }

        // Check if directory already exists
        cwd.access(ctx.io.std_io, dir_name, .{}) catch |err| switch (err) {
            error.FileNotFound => {
                // Good, directory doesn't exist
            },
            else => return err,
        };

        if (cwd.openDir(ctx.io.std_io, dir_name, .{})) |_| {
            try stderr.print("Error: Directory {s} already exists\n", .{dir_name});
            had_errors = true;
            continue;
        } else |_| {
            // Directory doesn't exist, proceed
        }

        // Create the output directory
        var output_dir = try cwd.createDirPathOpen(ctx.io.std_io, dir_name, .{});
        defer output_dir.close(ctx.io.std_io);

        // Open the archive file
        const archive_file = cwd.openFile(ctx.io.std_io, archive_path, .{}) catch |err| {
            try stderr.print("Error opening {s}: {s}\n", .{ archive_path, @errorName(err) });
            had_errors = true;
            continue;
        };
        defer archive_file.close(ctx.io.std_io);

        // Unbundle the archive
        var error_ctx: unbundle.ErrorContext = undefined;
        var archive_reader_buffer: [4096]u8 = undefined;
        var archive_reader = archive_file.reader(ctx.io.std_io, &archive_reader_buffer);
        unbundle.unbundleFiles(
            ctx.gpa,
            &archive_reader.interface,
            output_dir,
            ctx.io.std_io,
            basename,
            &error_ctx,
        ) catch |err| {
            switch (err) {
                error.HashMismatch => {
                    try stderr.print("Error: Hash mismatch for {s} - file may be corrupted\n", .{archive_path});
                    had_errors = true;
                },
                error.InvalidFilename => {
                    try stderr.print("Error: Invalid filename format for {s}\n", .{archive_path});
                    had_errors = true;
                },
                error.InvalidPath => {
                    try stderr.print("Error: Invalid path in archive - {s}\n", .{formatUnbundlePathValidationReason(error_ctx.reason)});
                    try stderr.print("Path: {s}\n", .{error_ctx.path});
                    try stderr.print("Archive: {s}\n", .{archive_path});
                    had_errors = true;
                },
                else => {
                    try stderr.print("Error unbundling {s}: {s}\n", .{ archive_path, @errorName(err) });
                    had_errors = true;
                },
            }
            continue; // Skip success message on error
        };

        try stdout.print("Extracted: {s}\n", .{dir_name});
    }

    if (had_errors) {
        return error.UnbundleFailed;
    }
}

fn rocBuild(ctx: *CliCtx, args_in: cli_args.BuildArgs, arg0: []const u8) CliMainError!void {
    var args = args_in;
    const resolved_source = try resolveSourceArg(ctx, args_in.path, args_in.watch);
    args.path = resolved_source.path;
    if (args.root_source_url == null) {
        args.root_source_url = resolved_source.url;
    }
    if (args.synthetic_output_basename == null) {
        switch (install_store.classifySourceRef(args_in.path)) {
            .shorthand => args.synthetic_output_basename = args_in.path,
            .url => args.synthetic_output_basename = urlDefaultOutputBasename(args_in.path),
            .local_path => {},
        }
    }

    // `roc build --watch` rebuilds on every change. The watch loop reruns this same
    // command (minus --watch) per change; the child writes its discovered inputs to
    // the --watch-inputs-file so the next iteration watches the right files.
    if (args.watch) {
        return runWatchCommand(ctx, arg0, .{ .build = args });
    }

    // Headerless apps build through a synthetic default platform.
    if (try readDefaultAppSource(ctx, args.path)) |source| {
        return rocBuildDefaultApp(ctx, args, source);
    }

    // Select build path based on optimization level
    switch (args.opt) {
        .dev => try rocBuildNative(ctx, args),
        .interpreter => try rocBuildEmbedded(ctx, args),
        .size, .speed => try rocBuildLlvm(ctx, args),
    }
}

fn rocBuildDefaultApp(ctx: *CliCtx, args: cli_args.BuildArgs, original_source: []const u8) CliMainError!void {
    defer ctx.gpa.free(original_source);

    const temp_dir = createUniqueTempDir(ctx) catch |err| {
        return ctx.fail(.{ .temp_dir_failed = .{ .err = err } });
    };
    defer std.Io.Dir.cwd().deleteTree(ctx.io.std_io, temp_dir) catch {};

    const platform_dir = try std.fs.path.join(ctx.arena, &.{ temp_dir, ".roc_echo_platform" });
    try std.Io.Dir.cwd().createDirPath(ctx.io.std_io, platform_dir);

    const app_filename = std.fs.path.basename(args.path);
    const app_path = try std.fs.path.join(ctx.arena, &.{ temp_dir, app_filename });
    const platform_main_path = try std.fs.path.join(ctx.arena, &.{ platform_dir, "main.roc" });
    const echo_module_path = try std.fs.path.join(ctx.arena, &.{ platform_dir, "Echo.roc" });

    const header = default_app_run_header;
    const normalized_original_source = try base.source_utils.normalizeLineEndingsAlloc(ctx.gpa, original_source);
    defer if (normalized_original_source.allocated) ctx.gpa.free(normalized_original_source.data);

    const synthetic_source = try std.mem.concat(ctx.gpa, u8, &.{ header, normalized_original_source.data });
    defer ctx.gpa.free(synthetic_source);

    try std.Io.Dir.cwd().writeFile(ctx.io.std_io, .{ .sub_path = app_path, .data = synthetic_source });
    try std.Io.Dir.cwd().writeFile(ctx.io.std_io, .{ .sub_path = platform_main_path, .data = defaultBuildPlatformSource(args) });
    try std.Io.Dir.cwd().writeFile(ctx.io.std_io, .{ .sub_path = echo_module_path, .data = echo_platform.echo_module_source });

    var synthetic_args = args;
    synthetic_args.path = app_path;
    synthetic_args.synthetic_default_platform = true;
    synthetic_args.source_dir_override = std.fs.path.dirname(args.path) orelse ".";
    synthetic_args.synthetic_root_original_path = args.path;
    synthetic_args.synthetic_root_original_source = normalized_original_source.data;
    synthetic_args.synthetic_root_header_len = header.len;
    synthetic_args.synthetic_root_header_lines = countNewlines(header);
    if (synthetic_args.output == null) {
        synthetic_args.output = args.synthetic_output_basename orelse try base.module_path.getModuleNameAlloc(ctx.arena, args.path);
    }

    switch (synthetic_args.opt) {
        .dev => try rocBuildNative(ctx, synthetic_args),
        .interpreter => try rocBuildEmbedded(ctx, synthetic_args),
        .size, .speed => try rocBuildLlvm(ctx, synthetic_args),
    }
}

fn defaultBuildPlatformSource(args: cli_args.BuildArgs) []const u8 {
    if (args.target) |target_str| {
        if (RocTarget.fromString(target_str)) |target| {
            return switch (target) {
                .x64mac, .arm64mac, .x64win, .arm64win => echo_platform.build_c_platform_main_source,
                .wasm32 => echo_platform.build_wasm_archive_platform_main_source,
                else => echo_platform.build_platform_main_source,
            };
        }

        return echo_platform.build_platform_main_source;
    }

    return switch (RocTarget.detectNative().toOsTag()) {
        .macos, .windows => echo_platform.build_c_platform_main_source,
        else => echo_platform.build_platform_main_source,
    };
}

/// Build using the dev backend to generate native machine code.
/// This produces truly compiled executables without an interpreter.
fn nativeBuildEntrypoints(
    ctx: *CliCtx,
    root_artifact: *const check.CheckedArtifact.CheckedModuleArtifact,
    lowered: *const lir.CheckedPipeline.LoweredProgram,
) Allocator.Error![]backend.Entrypoint {
    const root_procs = lowered.lir_result.root_procs.items;
    const root_metadata = lowered.lir_result.root_metadata.items;
    if (root_procs.len != root_metadata.len) {
        if (builtin.mode == .Debug) {
            std.debug.panic(
                "native build invariant violated: root metadata mismatch roots={d} metadata={d}",
                .{ root_procs.len, root_metadata.len },
            );
        }
        unreachable;
    }

    var entrypoints = std.ArrayList(backend.Entrypoint).empty;
    errdefer entrypoints.deinit(ctx.gpa);

    for (root_procs, root_metadata) |root_proc, metadata| {
        if (metadata.abi != .platform or metadata.exposure != .exported) continue;
        const root = root_artifact.lookupRootRequestByOrder(metadata.order) orelse {
            if (builtin.mode == .Debug) {
                std.debug.panic("native build invariant violated: missing root request order {d}", .{metadata.order});
            }
            unreachable;
        };
        if (root.kind != .provided_export) continue;

        const proc_spec = lowered.lir_result.store.getProcSpec(root_proc);
        const arg_locals = lowered.lir_result.store.getLocalSpan(proc_spec.args);
        const arg_layouts = try ctx.arena.alloc(layout.Idx, arg_locals.len);
        for (0..arg_locals.len) |i| {
            const local_id = GuardedList.at(arg_locals, i);
            arg_layouts[i] = lowered.lir_result.store.getLocal(local_id).layout_idx;
        }

        try entrypoints.append(ctx.gpa, .{
            .symbol_name = try nativeEntrypointSymbolName(ctx, root_artifact, root),
            .proc = root_proc,
            .arg_layouts = arg_layouts,
            .ret_layout = proc_spec.ret_layout,
        });
    }

    return try entrypoints.toOwnedSlice(ctx.gpa);
}

fn nativeEntrypointSymbolName(
    ctx: *CliCtx,
    root_artifact: *const check.CheckedArtifact.CheckedModuleArtifact,
    root: check.CheckedArtifact.RootRequest,
) Allocator.Error![]const u8 {
    const entrypoint_name = root_artifact.providedEntrypointName(root) orelse {
        if (builtin.mode == .Debug) {
            std.debug.panic(
                "platform entrypoint invariant violated: exported platform root has no published FFI symbol",
                .{},
            );
        }
        unreachable;
    };
    return try ctx.arena.dupe(u8, entrypoint_name);
}

const PlatformLinkInputs = struct {
    target_name: []const u8,
    platform_files_dir: []const u8,
    platform_files_pre: []const []const u8,
    platform_files_post: []const []const u8,
    wasm: ?roc_target.WasmTargetConfig,
};

fn selectBuildPlatformTarget(
    ctx: *CliCtx,
    targets_config: roc_target.TargetsConfig,
    platform_source: ?[]const u8,
    target_arg: ?[]const u8,
) error{ InvalidTarget, UnsupportedTarget, WriteFailed }!target_selection.SelectedTarget {
    return switch (target_selection.selectBuildTarget(targets_config, target_arg)) {
        .selected => |selected| selected,
        .invalid_target => |target_str| {
            renderValidationError(ctx.gpa, .{ .invalid_target = .{ .target_str = target_str } }, ctx.io.stderr());
            return error.InvalidTarget;
        },
        .unsupported_target => |target| {
            const result = platform_validation.createUnsupportedTargetResult(
                platform_source orelse "<unknown>",
                target,
                targets_config,
            );
            renderValidationError(ctx.gpa, result, ctx.io.stderr());
            return error.UnsupportedTarget;
        },
        .requires_executable => unreachable,
        .no_default => {
            if (targets_config.targets.len == 0) {
                renderValidationError(ctx.gpa, .{
                    .empty_targets = .{ .platform_path = platform_source orelse "<unknown>" },
                }, ctx.io.stderr());
                return error.UnsupportedTarget;
            }
            const native_target = RocTarget.detectNative();
            try ctx.io.stderr().print(
                "Error: roc build requires --target or a platform target for wasm32 or the detected native host ({s}).\n",
                .{@tagName(native_target)},
            );
            return error.UnsupportedTarget;
        },
        .not_runnable_on_host => unreachable,
    };
}

fn selectRunPlatformTarget(
    ctx: *CliCtx,
    targets_config: roc_target.TargetsConfig,
    platform_source: ?[]const u8,
    target_arg: ?[]const u8,
) error{ InvalidTarget, UnsupportedTarget, WriteFailed }!target_selection.SelectedTarget {
    return switch (target_selection.selectRunTarget(targets_config, target_arg)) {
        .selected => |selected| selected,
        .invalid_target => |target_str| {
            const result = platform_validation.targets_validator.ValidationResult{
                .invalid_target = .{ .target_str = target_str },
            };
            renderValidationError(ctx.gpa, result, ctx.io.stderr());
            return error.InvalidTarget;
        },
        .unsupported_target => |target| {
            const result = platform_validation.createUnsupportedTargetResult(
                platform_source orelse "<unknown>",
                target,
                targets_config,
            );
            renderValidationError(ctx.gpa, result, ctx.io.stderr());
            return error.UnsupportedTarget;
        },
        .requires_executable => |selected| {
            try rejectRequiredExecutableOutput(ctx, selected);
            unreachable;
        },
        .no_default => {
            if (targets_config.targets.len == 0) {
                renderValidationError(ctx.gpa, .{
                    .empty_targets = .{ .platform_path = platform_source orelse "<unknown>" },
                }, ctx.io.stderr());
                return error.UnsupportedTarget;
            }
            const native_target = RocTarget.detectNative();
            const result = platform_validation.createUnsupportedTargetResult(
                platform_source orelse "<unknown>",
                native_target,
                targets_config,
            );
            renderValidationError(ctx.gpa, result, ctx.io.stderr());
            return error.UnsupportedTarget;
        },
        .not_runnable_on_host => |target| {
            try rejectRunTargetNotExecutable(ctx, target);
            unreachable;
        },
    };
}

/// Map a target's declared output kind to the linker's output kind.
/// Collect the deduplicated hosted linker symbols the lowered program
/// references (only hosted functions the app actually uses have LIR procs).
fn hostedSymbolsFromLir(arena: std.mem.Allocator, store: *const lir.LirStore) std.mem.Allocator.Error![]const []const u8 {
    var seen = std.StringHashMap(void).init(arena);
    var symbols = std.ArrayList([]const u8).empty;
    for (store.getProcSpecs()) |spec| {
        const hosted = spec.hosted orelse continue;
        const text = store.getString(hosted.symbol);
        const gop = try seen.getOrPut(text);
        // Dupe into the arena: a diagnostic naming these symbols renders
        // after the lowered program is gone.
        if (!gop.found_existing) try symbols.append(arena, try arena.dupe(u8, text));
    }
    return symbols.items;
}

fn hostedSymbolsFromLirDispatch(arena: std.mem.Allocator, store: *const lir.LirStore) std.mem.Allocator.Error![]const []const u8 {
    var hosted_count: usize = 0;
    for (store.getProcSpecs()) |spec| {
        const hosted = spec.hosted orelse continue;
        hosted_count = @max(hosted_count, @as(usize, hosted.dispatch_index) + 1);
    }

    const hosted_symbols = try arena.alloc([]const u8, hosted_count);
    for (hosted_symbols) |*symbol| symbol.* = "";
    for (store.getProcSpecs()) |spec| {
        const hosted = spec.hosted orelse continue;
        hosted_symbols[hosted.dispatch_index] = store.getString(hosted.symbol);
    }
    return hosted_symbols;
}

/// Pre-link check: the platform's host inputs must define every hosted symbol
/// the app references plus the fixed runtime set. A missing hosted symbol
/// would otherwise resolve weakly to null and crash at the call; a missing
/// runtime symbol would surface as a raw linker error. Skipped when any host
/// input is in a format the scanner does not understand, since the result
/// would not be authoritative; the linker has the final say there.
fn verifyHostInputSymbols(
    ctx: *CliCtx,
    host_input_paths: []const []const u8,
    hosted_symbols: []const []const u8,
    target_name: []const u8,
    synthetic_default_platform: bool,
) CliMainError!void {
    if (host_input_paths.len == 0 and synthetic_default_platform) {
        return;
    }

    var needed = std.ArrayList([]const u8).empty;
    try needed.appendSlice(ctx.arena, &host_symbols.runtime_symbols);
    try needed.appendSlice(ctx.arena, hosted_symbols);

    const result = try host_symbols.scanHostInputs(ctx.arena, ctx.io.std_io, host_input_paths, needed.items);
    if (result.all_inputs_scanned and result.missing.len > 0) {
        return ctx.fail(.{ .missing_host_symbols = .{
            .symbols = result.missing,
            .target = target_name,
        } });
    }
}

fn writeDefaultPlatformRuntimeObject(ctx: *CliCtx, artifact_dir: []const u8, target: RocTarget) CliMainError!?[]const u8 {
    const bytes = DefaultPlatformRuntimeObjects.forTarget(target) orelse return null;
    const runtime_path = try std.fs.path.join(ctx.arena, &.{ artifact_dir, DefaultPlatformRuntimeObjects.filename(target) });
    backend.writeFileWindowsAvSafe(ctx.io.std_io, runtime_path, bytes) catch |err| {
        std.log.err("Failed to write default platform runtime object {s}: {}", .{ runtime_path, err });
        return err;
    };
    return runtime_path;
}

/// The host inputs of a link, in link order.
fn hostInputPaths(ctx: *CliCtx, link_inputs: PlatformLinkInputs) std.mem.Allocator.Error![]const []const u8 {
    var paths = try std.array_list.Managed([]const u8).initCapacity(
        ctx.arena,
        link_inputs.platform_files_pre.len + link_inputs.platform_files_post.len,
    );
    paths.appendSliceAssumeCapacity(link_inputs.platform_files_pre);
    paths.appendSliceAssumeCapacity(link_inputs.platform_files_post);
    return paths.items;
}

/// Archive outputs never reach the linker; they go through writeArchiveOutput.
fn linkerOutputKind(output: roc_target.OutputKind) linker.OutputKind {
    return switch (output) {
        .shared => .shared_lib,
        .exe => .exe,
        .archive => unreachable,
    };
}

fn llvmBuildLinkAbi(target: RocTarget, synthetic_default_platform: bool) linker.TargetAbi {
    if (synthetic_default_platform and target.toOsTag() == .linux) {
        return .musl;
    }
    return linker.TargetAbi.fromRocTarget(target);
}

/// Write an Archive output: a static archive of the platform's pre inputs,
/// the compiled objects, and the post inputs, with input archives flattened.
fn writeArchiveOutput(
    ctx: *CliCtx,
    target: RocTarget,
    final_output_path: []const u8,
    link_inputs: PlatformLinkInputs,
    object_files: []const []const u8,
) CliMainError!void {
    var inputs = try std.array_list.Managed([]const u8).initCapacity(
        ctx.arena,
        link_inputs.platform_files_pre.len + object_files.len + link_inputs.platform_files_post.len,
    );
    inputs.appendSliceAssumeCapacity(link_inputs.platform_files_pre);
    inputs.appendSliceAssumeCapacity(object_files);
    inputs.appendSliceAssumeCapacity(link_inputs.platform_files_post);
    builder.writeStaticArchive(ctx.gpa, final_output_path, inputs.items, target) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        error.ArchiveWriteFailed, error.LLVMNotAvailable => return ctx.fail(.{ .linker_failed = .{
            .err = err,
            .target = link_inputs.target_name,
        } }),
    };
}

fn rejectRequiredExecutableOutput(ctx: *CliCtx, selected: target_selection.SelectedTarget) error{ UnsupportedTarget, WriteFailed }!void {
    const stderr = ctx.io.stderr();
    switch (selected.output) {
        .archive => {
            try stderr.print("Error: The selected target only produces static archives.\n\n", .{});
            try stderr.print("Archive platforms produce .a/.lib files that must be linked\n", .{});
            try stderr.print("by another build. Use 'roc build' instead to produce\n", .{});
            try stderr.print("the archive.\n", .{});
        },
        .shared => {
            if (selected.target == .wasm32) {
                try stderr.print("Error: This platform cannot be run directly.\n\n", .{});
                try stderr.print("This platform targets wasm32 and produces a .wasm module. Use 'roc build'\n", .{});
                try stderr.print("to produce the wasm artifact, then load it with the host application.\n", .{});
            } else {
                try stderr.print("Error: The selected target only produces shared libraries.\n\n", .{});
                try stderr.print("Shared library platforms produce .so/.dylib/.dll files that must be\n", .{});
                try stderr.print("loaded by a host application. Use 'roc build' instead to produce\n", .{});
                try stderr.print("the library artifact.\n", .{});
            }
        },
        .exe => unreachable,
    }
    return error.UnsupportedTarget;
}

fn collectPlatformLinkInputs(
    ctx: *CliCtx,
    platform_dir: []const u8,
    targets_config: roc_target.TargetsConfig,
    target: RocTarget,
    link_type: roc_target.OutputKind,
) (Allocator.Error || error{ CliError, MissingTargetFile })!PlatformLinkInputs {
    const target_name = @tagName(target);
    const link_spec = targets_config.getLinkSpec(target) orelse {
        return ctx.fail(.{ .linker_failed = .{
            .err = error.UnsupportedTarget,
            .target = target_name,
        } });
    };
    const files_dir = targets_config.inputs_dir orelse "targets";
    var platform_files_pre = try std.array_list.Managed([]const u8).initCapacity(ctx.arena, 8);
    var platform_files_post = try std.array_list.Managed([]const u8).initCapacity(ctx.arena, 8);
    var hit_app = false;

    for (link_spec.items) |item| {
        switch (item) {
            .file_path => |path| {
                const full_path = try std.fs.path.join(ctx.arena, &.{ platform_dir, files_dir, target_name, path });
                std.Io.Dir.cwd().access(ctx.io.std_io, full_path, .{}) catch {
                    renderValidationError(ctx.gpa, .{ .missing_target_file = .{
                        .target = target,
                        .output = link_type,
                        .file_path = path,
                        .expected_full_path = full_path,
                    } }, ctx.io.stderr());
                    return error.MissingTargetFile;
                };
                if (!hit_app) {
                    try platform_files_pre.append(full_path);
                } else {
                    try platform_files_post.append(full_path);
                }
            },
            .app => hit_app = true,
            .win_gui => {},
        }
    }

    return .{
        .target_name = target_name,
        .platform_files_dir = try std.fs.path.join(ctx.arena, &.{ platform_dir, files_dir }),
        .platform_files_pre = platform_files_pre.items,
        .platform_files_post = platform_files_post.items,
        .wasm = link_spec.wasm,
    };
}

fn appendOwnedWasmInput(ctx: *CliCtx, owned_inputs: *std.ArrayList([]u8), path: []const u8) CliMainError![]const u8 {
    const bytes = try std.Io.Dir.cwd().readFileAlloc(ctx.io.std_io, path, ctx.gpa, .unlimited);
    errdefer ctx.gpa.free(bytes);
    try owned_inputs.append(ctx.gpa, bytes);
    return bytes;
}

fn freeOwnedWasmInputs(ctx: *CliCtx, owned_inputs: *std.ArrayList([]u8)) void {
    for (owned_inputs.items) |bytes| {
        ctx.gpa.free(bytes);
    }
    owned_inputs.deinit(ctx.gpa);
}

fn preloadWasmObject(
    ctx: *CliCtx,
    path: []const u8,
    member_name: ?[]const u8,
    bytes: []const u8,
) backend.wasm.WasmModule.ParseError!backend.wasm.WasmModule {
    return backend.wasm.WasmModule.preload(ctx.gpa, bytes, true) catch |err| {
        if (member_name) |name| {
            std.log.err("Failed to preload wasm archive member {s}({s}): {}", .{ path, name, err });
        } else {
            std.log.err("Failed to preload wasm input {s}: {}", .{ path, err });
        }
        return err;
    };
}

fn configuredWasmStackBytes(args: cli_args.BuildArgs, wasm: ?roc_target.WasmTargetConfig) usize {
    if (args.wasm_stack_size) |bytes| return bytes;
    if (wasm) |config| {
        if (config.initial_stack_size) |bytes| return bytes;
    }
    return linker.DEFAULT_WASM_STACK_SIZE;
}

fn configuredWasmMinimumMemory(args: cli_args.BuildArgs, wasm: ?roc_target.WasmTargetConfig) usize {
    if (args.wasm_memory) |bytes| return bytes;
    if (wasm) |config| {
        if (config.minimum_memory) |bytes| return bytes;
    }
    return linker.DEFAULT_WASM_INITIAL_MEMORY;
}

/// Whether linked wasm output may assume linear memory starts zero-filled.
/// Fresh (non-imported) wasm memory is always zeroed; imported memory is
/// zeroed only when the platform's targets config declares
/// `import_memory: Zeroed`. Mirrors the dev backend's
/// `omit_zero_fill_data_segments` decision in `configuredWasmMemory`.
fn configuredWasmZeroFilledMemory(wasm: ?roc_target.WasmTargetConfig) bool {
    if (wasm) |config| {
        return !config.import_memory.importsMemory() or config.import_memory.importedMemoryIsZeroed();
    }
    return true;
}

/// Binaryen post-link optimization mode for linked wasm output, derived from
/// the build's opt level: LLVM opt levels get the matching Binaryen pass;
/// dev/interpreter builds skip Binaryen entirely.
fn wasmOptimizeMode(opt: cli_args.OptLevel) linker.WasmOptimizeMode {
    return switch (opt) {
        .size => .size,
        .speed => .speed,
        .dev, .interpreter => .none,
    };
}

fn configuredWasmMemory(
    args: cli_args.BuildArgs,
    wasm: ?roc_target.WasmTargetConfig,
) backend.wasm.WasmModule.FinalMemoryConfig {
    const stack_bytes = configuredWasmStackBytes(args, wasm);
    const import_memory = if (wasm) |config| config.import_memory.importsMemory() else false;
    return .{
        .stack_bytes = @intCast(stack_bytes),
        .import_memory = import_memory,
        .imported_memory_zeroed = if (wasm) |config| config.import_memory.importedMemoryIsZeroed() else false,
        .minimum_memory = configuredWasmMinimumMemory(args, wasm),
        .maximum_memory = if (wasm) |config| config.maximum_memory else null,
        .export_memory = !import_memory,
    };
}

fn configureWasmDataBase(module: *backend.wasm.WasmModule, wasm: ?roc_target.WasmTargetConfig) void {
    if (wasm) |config| {
        if (config.global_base) |global_base| {
            module.setDataBase(global_base);
        }
    }
}

fn exportConfiguredWasmEntrypoints(module: *backend.wasm.WasmModule) CliMainError!void {
    try module.exportGlobalSymbols();
}

fn addWasmObject(
    ctx: *CliCtx,
    module: *backend.wasm.WasmModule,
    path: []const u8,
    member_name: ?[]const u8,
    bytes: []const u8,
    loaded_module: *bool,
) (backend.wasm.WasmModule.ParseError || backend.wasm.WasmModule.MergeError)!void {
    var next_module = try preloadWasmObject(ctx, path, member_name, bytes);

    if (!loaded_module.*) {
        module.* = next_module;
        loaded_module.* = true;
        return;
    }

    defer next_module.deinit();

    var merge_result = try module.mergeModule(&next_module);
    merge_result.deinit();
}

fn addWasmInput(
    ctx: *CliCtx,
    module: *backend.wasm.WasmModule,
    owned_inputs: *std.ArrayList([]u8),
    path: []const u8,
    loaded_module: *bool,
) CliMainError!void {
    const bytes = try appendOwnedWasmInput(ctx, owned_inputs, path);

    if (backend.wasm.ObjectArchive.isWasmObject(bytes)) {
        try addWasmObject(ctx, module, path, null, bytes, loaded_module);
        return;
    }

    if (!backend.wasm.ObjectArchive.isArchive(bytes)) {
        std.log.err("Failed to preload wasm input {s}: {}", .{ path, error.InvalidMagic });
        return error.InvalidMagic;
    }

    var member_count: usize = 0;
    var iter = backend.wasm.ObjectArchive.Iterator.init(bytes) catch |err| {
        std.log.err("Failed to read wasm archive {s}: {}", .{ path, err });
        return err;
    };

    while (true) {
        const maybe_member = iter.next() catch |err| {
            std.log.err("Failed to read wasm archive {s}: {}", .{ path, err });
            return err;
        };
        const member = maybe_member orelse break;
        member_count += 1;
        try addWasmObject(ctx, module, path, member.name, member.bytes, loaded_module);
    }

    if (member_count == 0) {
        std.log.err("Wasm archive {s} does not contain object members", .{path});
        return error.EmptyArchive;
    }
}

fn appendUniqueWasmExportName(exports: *std.array_list.Managed([]const u8), name: []const u8) Allocator.Error!void {
    for (exports.items) |existing| {
        if (std.mem.eql(u8, existing, name)) return;
    }
    try exports.append(name);
}

fn appendWasmObjectExportNames(
    ctx: *CliCtx,
    exports: *std.array_list.Managed([]const u8),
    path: []const u8,
    member_name: ?[]const u8,
    bytes: []const u8,
) CliMainError!void {
    var module = try preloadWasmObject(ctx, path, member_name, bytes);
    defer module.deinit();

    try module.exportGlobalSymbols();
    for (module.exports.items) |exp| {
        if (exp.kind == .func) {
            try appendUniqueWasmExportName(exports, exp.name);
        }
    }
}

fn appendWasmInputExportNames(
    ctx: *CliCtx,
    exports: *std.array_list.Managed([]const u8),
    owned_inputs: *std.ArrayList([]u8),
    path: []const u8,
) CliMainError!void {
    const bytes = try appendOwnedWasmInput(ctx, owned_inputs, path);

    if (backend.wasm.ObjectArchive.isWasmObject(bytes)) {
        try appendWasmObjectExportNames(ctx, exports, path, null, bytes);
        return;
    }

    if (!backend.wasm.ObjectArchive.isArchive(bytes)) {
        std.log.err("Failed to preload wasm input {s}: {}", .{ path, error.InvalidMagic });
        return error.InvalidMagic;
    }

    var member_count: usize = 0;
    var iter = backend.wasm.ObjectArchive.Iterator.init(bytes) catch |err| {
        std.log.err("Failed to read wasm archive {s}: {}", .{ path, err });
        return err;
    };

    while (true) {
        const maybe_member = iter.next() catch |err| {
            std.log.err("Failed to read wasm archive {s}: {}", .{ path, err });
            return err;
        };
        const member = maybe_member orelse break;
        member_count += 1;
        try appendWasmObjectExportNames(ctx, exports, path, member.name, member.bytes);
    }

    if (member_count == 0) {
        std.log.err("Wasm archive {s} does not contain object members", .{path});
        return error.EmptyArchive;
    }
}

fn collectWasmPlatformExports(
    ctx: *CliCtx,
    link_inputs: PlatformLinkInputs,
    owned_inputs: *std.ArrayList([]u8),
) CliMainError![]const []const u8 {
    if (link_inputs.wasm) |wasm| {
        if (wasm.exports) |exports| return exports;
    }

    var exports = std.array_list.Managed([]const u8).init(ctx.arena);

    for (link_inputs.platform_files_pre) |path| {
        try appendWasmInputExportNames(ctx, &exports, owned_inputs, path);
    }
    for (link_inputs.platform_files_post) |path| {
        try appendWasmInputExportNames(ctx, &exports, owned_inputs, path);
    }

    return exports.items;
}

fn writeDevWasmObject(
    ctx: *CliCtx,
    build_cache_dir: []const u8,
    lowered: *const lir.CheckedPipeline.LoweredProgram,
    entrypoints: []const backend.Entrypoint,
    static_data_exports: []const backend.StaticDataExport,
) CliMainError![]const u8 {
    if (entrypoints.len == 0) {
        if (builtin.mode == .Debug) {
            std.debug.panic("wasm object invariant violated: no exported platform entrypoints", .{});
        }
        unreachable;
    }

    var wasm_module = backend.wasm.WasmModule.init(ctx.gpa);
    // Ownership of the module moves into the codegen at initWithModule below;
    // free it here only if we fail before that point.
    var wasm_module_owned_here = true;
    errdefer if (wasm_module_owned_here) wasm_module.deinit();
    wasm_module.addMemoryImport();
    const table_symbol = try wasm_module.addTableImportWithSymbol();
    const stack_pointer_symbol = try wasm_module.addStackPointerImportWithSymbol();

    var codegen = backend.wasm.WasmCodeGen.initWithModule(
        ctx.gpa,
        &lowered.lir_result.store,
        &lowered.lir_result.layouts,
        &wasm_module,
    );
    wasm_module_owned_here = false;
    defer codegen.deinit();
    codegen.configureStackPointerReloc(stack_pointer_symbol);
    codegen.configureTableReloc(table_symbol);
    codegen.configureRelocatableObject();

    // Register the symbol-ABI imports while the module has no defined
    // functions yet; a function import added later would shift every defined
    // function index.
    codegen.configureSymbolAbi();
    try codegen.registerHostedSymbolTargets(lowered.lir_result.store.getProcSpecs());

    const builtins_bytes = BuiltinsObjects.forTargetExtern(.wasm32);
    if (builtins_bytes.len > 0) {
        var builtins_module = backend.wasm.WasmModule.preload(ctx.gpa, builtins_bytes, true) catch |err| {
            std.log.err("Failed to preload wasm builtins: {}", .{err});
            return err;
        };
        defer builtins_module.deinit();

        var merge_result = try codegen.module.mergeModuleForObject(&builtins_module);
        merge_result.deinit();
    }

    const builtin_symbols = backend.wasm.BuiltinSignatures.populateForRelocs(&codegen.module) catch |err| {
        std.log.err("Failed to locate wasm builtin symbols after object merge: {}", .{err});
        return err;
    };
    codegen.configureBuiltinRelocs(builtin_symbols);

    const static_rc_helpers = try backend.collectRequiredRcHelpers(ctx.gpa, static_data_exports);
    defer ctx.gpa.free(static_rc_helpers);
    codegen.static_data_rc_helpers = static_rc_helpers;
    try codegen.registerIndirectCallTypes();
    try codegen.compileAllProcSpecs(lowered.lir_result.store.getProcSpecs());
    try codegen.compileStaticDataRcHelpers(static_rc_helpers);

    for (entrypoints) |entry| {
        _ = try codegen.generateEntrypointWrapper(
            entry.symbol_name,
            entry.proc,
            entry.arg_layouts,
            entry.ret_layout,
        );
    }

    try codegen.flushPendingBodies();
    for (entrypoints) |entry| {
        _ = try codegen.module.findDefinedFunctionSymbolExact(entry.symbol_name);
    }
    try mergeStaticDataWasmModule(ctx, &codegen.module, static_data_exports, .relocatable_object);
    try codegen.module.verifyNoLinkObjectContract();

    const wasm_bytes = try codegen.module.encodeRelocatable(ctx.gpa);
    defer ctx.gpa.free(wasm_bytes);

    const obj_path = try std.fs.path.join(ctx.arena, &.{ build_cache_dir, "roc_app_wasm32.o" });
    backend.writeFileWindowsAvSafe(ctx.io.std_io, obj_path, wasm_bytes) catch |err| {
        std.log.err("Failed to write wasm object output: {}", .{err});
        return error.WasmOutputWriteFailed;
    };

    return obj_path;
}

fn rocBuildWasmSurgical(
    ctx: *CliCtx,
    args: cli_args.BuildArgs,
    target: RocTarget,
    link_type: roc_target.OutputKind,
    final_output_path: []const u8,
    build_cache_dir: []const u8,
    platform_dir: []const u8,
    targets_config: roc_target.TargetsConfig,
    lowered: *const lir.CheckedPipeline.LoweredProgram,
    entrypoints: []const backend.Entrypoint,
    static_data_exports: []const backend.StaticDataExport,
) CliMainError!void {
    if (entrypoints.len == 0) {
        if (builtin.mode == .Debug) {
            std.debug.panic("wasm build invariant violated: no exported platform entrypoints", .{});
        }
        unreachable;
    }

    const link_inputs = try collectPlatformLinkInputs(ctx, platform_dir, targets_config, target, link_type);

    if (link_type == .archive) {
        // Archives package whatever inputs the platform declared (possibly
        // just the app); no platform wasm file is required.
        const obj_path = try writeDevWasmObject(ctx, build_cache_dir, lowered, entrypoints, static_data_exports);
        try writeArchiveOutput(ctx, .wasm32, final_output_path, link_inputs, &.{obj_path});
        return;
    }

    if (link_inputs.platform_files_pre.len + link_inputs.platform_files_post.len == 0) {
        try ctx.io.stderr().writeAll("Error: wasm32 builds require a relocatable wasm platform file or archive.\n");
        return error.UnsupportedTarget;
    }

    var owned_inputs: std.ArrayList([]u8) = .empty;
    defer freeOwnedWasmInputs(ctx, &owned_inputs);

    if (link_inputs.wasm != null) {
        const obj_path = try writeDevWasmObject(ctx, build_cache_dir, lowered, entrypoints, static_data_exports);
        const object_files = try ctx.arena.alloc([]const u8, 1);
        object_files[0] = obj_path;
        const wasm_exports = try collectWasmPlatformExports(ctx, link_inputs, &owned_inputs);

        const link_config = linker.LinkConfig{
            .target_format = .wasm,
            .target_abi = null,
            .target_os = .freestanding,
            .target_arch = .wasm32,
            .output_path = final_output_path,
            .object_files = object_files,
            .platform_files_pre = link_inputs.platform_files_pre,
            .platform_files_post = link_inputs.platform_files_post,
            .extra_args = &.{},
            .can_exit_early = false,
            .disable_output = false,
            .wasm_initial_memory = configuredWasmMinimumMemory(args, link_inputs.wasm),
            .wasm_maximum_memory = if (link_inputs.wasm) |wasm| wasm.maximum_memory else null,
            .wasm_stack_size = configuredWasmStackBytes(args, link_inputs.wasm),
            .wasm_import_memory = if (link_inputs.wasm) |wasm| wasm.import_memory.importsMemory() else false,
            .wasm_zero_filled_memory = configuredWasmZeroFilledMemory(link_inputs.wasm),
            .wasm_debug_info = args.debug,
            .wasm_optimize = wasmOptimizeMode(args.opt),
            .wasm_global_base = if (link_inputs.wasm) |wasm| wasm.global_base else null,
            .wasm_exports = wasm_exports,
            .platform_files_dir = link_inputs.platform_files_dir,
            .scratch_dir = build_cache_dir,
        };

        linker.link(ctx, link_config) catch |err| {
            return ctx.fail(.{ .linker_failed = .{
                .err = err,
                .target = link_inputs.target_name,
            } });
        };
        return;
    }

    var loaded_module = true;
    var wasm_module = backend.wasm.WasmModule.init(ctx.gpa);
    configureWasmDataBase(&wasm_module, link_inputs.wasm);
    errdefer if (loaded_module) wasm_module.deinit();

    for (link_inputs.platform_files_pre) |path| {
        try addWasmInput(ctx, &wasm_module, &owned_inputs, path, &loaded_module);
    }
    for (link_inputs.platform_files_post) |path| {
        try addWasmInput(ctx, &wasm_module, &owned_inputs, path, &loaded_module);
    }

    try exportConfiguredWasmEntrypoints(&wasm_module);
    wasm_module.removeMemoryAndTableImports();

    const builtins_bytes = BuiltinsObjects.forTargetExtern(.wasm32);
    if (builtins_bytes.len > 0) {
        var builtins_module = backend.wasm.WasmModule.preload(ctx.gpa, builtins_bytes, true) catch |err| {
            std.log.err("Failed to preload wasm builtins: {}", .{err});
            return err;
        };
        defer builtins_module.deinit();

        var merge_result = try wasm_module.mergeModule(&builtins_module);
        merge_result.deinit();
    }

    const builtin_symbols = backend.wasm.BuiltinSignatures.populateForRelocs(&wasm_module) catch |err| {
        std.log.err("Failed to locate wasm builtin symbols after merge: {}", .{err});
        return err;
    };

    var codegen = backend.wasm.WasmCodeGen.initWithModule(
        ctx.gpa,
        &lowered.lir_result.store,
        &lowered.lir_result.layouts,
        &wasm_module,
    );
    defer codegen.deinit();
    loaded_module = false;
    codegen.configureBuiltinRelocs(builtin_symbols);
    codegen.configureStaticDataAddressTracking();

    const static_rc_helpers = try backend.collectRequiredRcHelpers(ctx.gpa, static_data_exports);
    defer ctx.gpa.free(static_rc_helpers);
    codegen.static_data_rc_helpers = static_rc_helpers;
    try codegen.registerIndirectCallTypes();
    codegen.configureSymbolAbi();
    try codegen.registerHostedSymbolTargets(lowered.lir_result.store.getProcSpecs());
    try codegen.compileAllProcSpecs(lowered.lir_result.store.getProcSpecs());
    try codegen.compileStaticDataRcHelpers(static_rc_helpers);

    var host_to_app_map: std.ArrayList(backend.wasm.WasmModule.HostToAppEntry) = .empty;
    defer host_to_app_map.deinit(ctx.gpa);
    try host_to_app_map.ensureTotalCapacity(ctx.gpa, entrypoints.len);

    for (entrypoints) |entry| {
        const fn_index = try codegen.generateEntrypointWrapper(
            entry.symbol_name,
            entry.proc,
            entry.arg_layouts,
            entry.ret_layout,
        );
        host_to_app_map.appendAssumeCapacity(.{
            .name = entry.symbol_name,
            .fn_index = fn_index,
        });
    }

    try codegen.flushPendingBodies();
    try mergeStaticDataWasmModule(ctx, &codegen.module, static_data_exports, .final_link);
    try codegen.module.linkHostToAppCalls(host_to_app_map.items);

    const memory_config = configuredWasmMemory(args, link_inputs.wasm);
    try codegen.module.finalizeMemoryAndTableWithConfig(memory_config);
    try codegen.module.resolveRelocations();

    const called_fns = try ctx.gpa.alloc(bool, codegen.module.liveFunctionCount());
    defer ctx.gpa.free(called_fns);
    @memset(called_fns, false);
    try codegen.module.eliminateDeadCode(called_fns);

    try codegen.module.verifyNoBuiltinImports();
    try codegen.module.materializeFuncBodies();

    const wasm_bytes = try codegen.module.encode(ctx.gpa);
    defer ctx.gpa.free(wasm_bytes);
    backend.writeFileWindowsAvSafe(ctx.io.std_io, final_output_path, wasm_bytes) catch |err| {
        std.log.err("Failed to write wasm output: {}", .{err});
        return error.WasmOutputWriteFailed;
    };
}

const LlvmObjectPaths = struct {
    artifact_dir: []const u8,
    bitcode_path: []const u8,
    object_path: []const u8,
};

fn staticDataLinkRootSymbols(
    ctx: *CliCtx,
    static_data_exports: []const backend.StaticDataExport,
    root_default_platform_backtrace: bool,
) Allocator.Error![]const []const u8 {
    var symbols = try std.array_list.Managed([]const u8).initCapacity(
        ctx.arena,
        static_data_exports.len + @as(usize, if (root_default_platform_backtrace) 2 else 0),
    );
    for (static_data_exports) |data_export| {
        if (!data_export.is_global) continue;
        try symbols.append(data_export.symbol_name);
    }
    if (root_default_platform_backtrace) {
        try symbols.append(shim_symbols.roc_default_backtrace_table);
        try symbols.append(shim_symbols.roc_default_backtrace_count);
    }
    return symbols.items;
}

fn sharedLibraryAppExports(
    ctx: *CliCtx,
    entrypoints: []const backend.Entrypoint,
    static_data_exports: []const backend.StaticDataExport,
) Allocator.Error![]const []const u8 {
    var symbols = try std.array_list.Managed([]const u8).initCapacity(
        ctx.arena,
        entrypoints.len + static_data_exports.len,
    );

    for (entrypoints) |entrypoint| {
        try symbols.append(entrypoint.symbol_name);
    }
    for (static_data_exports) |data_export| {
        if (!data_export.is_exported) continue;
        try symbols.append(data_export.symbol_name);
    }

    return symbols.items;
}

fn appendUniqueSharedLibraryExport(
    seen: *std.StringHashMap(void),
    symbols: *std.array_list.Managed([]const u8),
    symbol: []const u8,
) Allocator.Error!void {
    if (symbol.len == 0) return;
    const gop = try seen.getOrPut(symbol);
    if (gop.found_existing) return;
    try symbols.append(symbol);
}

/// Symbols a shared-library link must export: app-provided symbols plus host
/// input exports, so the final library exposes its explicit public ABI on every
/// target. Empty for non-shared output.
fn sharedLibraryExports(
    ctx: *CliCtx,
    link_type: roc_target.OutputKind,
    link_inputs: PlatformLinkInputs,
    app_export_symbols: []const []const u8,
) Allocator.Error![]const []const u8 {
    if (link_type != .shared) return &.{};

    const host_export_symbols = try host_symbols.collectHostExports(ctx.arena, ctx.io.std_io, try hostInputPaths(ctx, link_inputs));
    var seen = std.StringHashMap(void).init(ctx.arena);
    var symbols = try std.array_list.Managed([]const u8).initCapacity(
        ctx.arena,
        app_export_symbols.len + host_export_symbols.len,
    );

    for (app_export_symbols) |symbol| {
        try appendUniqueSharedLibraryExport(&seen, &symbols, symbol);
    }
    for (host_export_symbols) |symbol| {
        try appendUniqueSharedLibraryExport(&seen, &symbols, symbol);
    }

    return symbols.items;
}

fn llvmOptimizationLevel(opt: cli_args.OptLevel) builder.OptimizationLevel {
    return switch (opt) {
        .size => .size,
        .speed => .speed,
        .dev, .interpreter => {
            if (builtin.mode == .Debug) {
                std.debug.panic("LLVM build invariant violated: non-LLVM opt level {s}", .{@tagName(opt)});
            }
            unreachable;
        },
    };
}

fn stdTargetAbiForLlvmBuild(target: RocTarget) std.Target.Abi {
    return switch (target) {
        .x64musl, .arm64musl, .arm32musl => .musl,
        .x64glibc, .x64linux, .arm64glibc, .arm64linux, .arm32linux => .gnu,
        .x64win, .arm64win => .msvc,
        else => .none,
    };
}

fn noTargetLibcallsForLlvmBuild(target: RocTarget) bool {
    return switch (target.toOsTag()) {
        .macos, .windows => false,
        else => true,
    };
}

fn stdTargetForLlvmBuild(ctx: *CliCtx, target: RocTarget) std.zig.system.DetectError!std.Target {
    var query = std.Target.Query{
        .cpu_arch = target.toCpuArch(),
        .os_tag = target.toOsTag(),
        .abi = stdTargetAbiForLlvmBuild(target),
    };
    if (target.toOsTag() == .macos) {
        query.os_version_min = roc_target.macos_deployment.query_os_version;
    }

    // Raise the LLVM codegen floor above Zig's most-conservative per-arch
    // baseline (2003-era x86-64 with only SSE2; ARMv8.0 for aarch64) so Roc
    // programs may use the last two decades of instructions. The floor is an
    // explicit, portable minimum applied to every Roc-program LLVM compile,
    // native and cross alike, so it is independent of the CPU that the compiler
    // binary itself targets.
    switch (target.toCpuArch()) {
        .x86_64 => {
            // x86-64-v3 (Intel Haswell 2013+ / any AMD Zen) enables AVX2, BMI2,
            // POPCNT, FMA, and more. The v-levels deliberately exclude the AES
            // and PCLMULQDQ crypto instructions, so add those two explicitly.
            query.cpu_model = .{ .explicit = &std.Target.x86.cpu.x86_64_v3 };
            query.cpu_features_add.addFeature(@intFromEnum(std.Target.x86.Feature.aes));
            query.cpu_features_add.addFeature(@intFromEnum(std.Target.x86.Feature.pclmul));
        },
        .aarch64, .aarch64_be => {
            // Zig's baseline for aarch64-macos is already apple_m1, and a native
            // macOS host detects its actual (>= M1) CPU, so macOS needs no
            // explicit model. Linux/Windows baseline to generic ARMv8.0, so pin
            // Cortex-A76 (ARMv8.2 + crypto extension), which covers
            // Neoverse-N1/Graviton2+, Ampere, Snapdragon, and Raspberry Pi 5.
            if (target.toOsTag() != .macos) {
                query.cpu_model = .{ .explicit = &std.Target.aarch64.cpu.cortex_a76 };
            }
        },
        // arm32 and wasm keep their existing baselines: wasm codegen is handled
        // by Roc's own wasm backend, not this LLVM baseline.
        else => {},
    }

    return std.zig.system.resolveTargetQuery(ctx.io.std_io, query);
}

fn llvmCpuNameForTarget(std_target: std.Target) []const u8 {
    return std_target.cpu.model.llvm_name orelse "";
}

fn llvmFeatureStringForTarget(allocator: Allocator, std_target: std.Target) Allocator.Error![]const u8 {
    const all_features = std_target.cpu.arch.allFeaturesList();
    var model_features = std_target.cpu.model.features;
    model_features.populateDependencies(all_features);

    var features = std.ArrayList(u8).empty;
    errdefer features.deinit(allocator);

    for (all_features) |feature| {
        const llvm_name = feature.llvm_name orelse continue;
        const enabled = std_target.cpu.features.isEnabled(feature.index);
        const model_enabled = model_features.isEnabled(feature.index);
        if (enabled == model_enabled) continue;

        if (features.items.len > 0) {
            try features.append(allocator, ',');
        }
        try features.append(allocator, if (enabled) '+' else '-');
        try features.appendSlice(allocator, llvm_name);
    }

    if (features.items.len == 0) return "";
    return features.toOwnedSlice(allocator);
}

fn compileLlvmAppObject(
    ctx: *CliCtx,
    args: cli_args.BuildArgs,
    target: RocTarget,
    link_type: roc_target.OutputKind,
    lowered: *const lir.CheckedPipeline.LoweredProgram,
    entrypoints: []const backend.Entrypoint,
    static_data_exports: []const backend.StaticDataExport,
    enable_default_platform_runtime: bool,
    enable_default_platform_hosted_calls: bool,
) CliMainError!LlvmObjectPaths {
    const std_target = try stdTargetForLlvmBuild(ctx, target);
    const llvm_cpu = llvmCpuNameForTarget(std_target);
    const llvm_features = try llvmFeatureStringForTarget(ctx.arena, std_target);

    var codegen = llvm_codegen.MonoLlvmCodeGen.initForLinkedObject(
        ctx.gpa,
        &lowered.lir_result.store,
        std_target,
    );
    codegen.layout_store = &lowered.lir_result.layouts;
    const emit_debug_info = args.debug;
    codegen.emit_debug_info = emit_debug_info;
    codegen.emit_local_debug_info = emit_debug_info;
    codegen.enable_default_platform_runtime = enable_default_platform_runtime;
    codegen.enable_default_platform_hosted_calls = enable_default_platform_hosted_calls;
    codegen.enable_default_platform_diagnostics = enable_default_platform_hosted_calls and emit_debug_info;
    codegen.debug_producer = "roc " ++ build_options.compiler_version;
    defer codegen.deinit();

    const static_rc_helpers = try backend.collectRequiredRcHelpers(ctx.gpa, static_data_exports);
    defer ctx.gpa.free(static_rc_helpers);
    codegen.static_data_rc_helpers = static_rc_helpers;

    const llvm_entrypoints = try ctx.arena.alloc(llvm_codegen.MonoLlvmCodeGen.Entrypoint, entrypoints.len);
    for (entrypoints, 0..) |entrypoint, i| {
        llvm_entrypoints[i] = .{
            .symbol_name = entrypoint.symbol_name,
            .proc = entrypoint.proc,
            .arg_layouts = entrypoint.arg_layouts,
            .ret_layout = entrypoint.ret_layout,
        };
    }

    var bitcode = try codegen.generateEntrypointModule("roc_app_llvm", llvm_entrypoints);
    defer bitcode.deinit();

    const target_name = @tagName(target);
    const opt_name = @tagName(args.opt);
    // Shared libraries need position-independent code; keep their objects
    // separate from exe objects in the artifact directory.
    const pic = link_type == .shared;
    const kind_suffix: []const u8 = if (pic) "_pic" else "";
    const debug_suffix: []const u8 = if (emit_debug_info) "_debug" else "";
    var tuning_hash = std.hash.Crc32.init();
    tuning_hash.update(llvm_cpu);
    tuning_hash.update(&[_]u8{0});
    tuning_hash.update(llvm_features);
    const tuning_hash_value = tuning_hash.final();
    const bitcode_filename = try std.fmt.allocPrint(ctx.arena, "roc_app_llvm_{s}_{s}_{x}{s}{s}.bc", .{ target_name, opt_name, tuning_hash_value, kind_suffix, debug_suffix });
    const object_filename = try std.fmt.allocPrint(ctx.arena, "roc_app_llvm_{s}_{s}_{x}{s}{s}.o", .{ target_name, opt_name, tuning_hash_value, kind_suffix, debug_suffix });
    const artifact_dir = try createUniqueTempDir(ctx);
    errdefer std.Io.Dir.cwd().deleteTree(ctx.io.std_io, artifact_dir) catch {};
    const bitcode_path = try std.fs.path.join(ctx.arena, &.{ artifact_dir, bitcode_filename });
    const object_path = try std.fs.path.join(ctx.arena, &.{ artifact_dir, object_filename });

    backend.writeFileWindowsAvSafe(ctx.io.std_io, bitcode_path, std.mem.sliceAsBytes(bitcode.bitcode)) catch |err| {
        std.log.err("Failed to write LLVM bitcode {s}: {}", .{ bitcode_path, err });
        return err;
    };

    const compile_config = builder.CompileConfig{
        .input_path = bitcode_path,
        .output_path = object_path,
        .optimization = llvmOptimizationLevel(args.opt),
        .target = target,
        .cpu = llvm_cpu,
        .features = llvm_features,
        .debug = args.debug,
        .link_builtins = true,
        .pic = pic,
        // Linked LLVM output uses the symbol ABI: builtins reach the host
        // through extern symbols, never through a RocOps parameter.
        .host_call_extern = true,
        .no_target_libcalls = noTargetLibcallsForLlvmBuild(target),
    };

    const success = try builder.compileBitcodeToObject(ctx.gpa, ctx.io.std_io, compile_config);
    if (!success) {
        std.log.err("LLVM object compilation failed for {s}", .{bitcode_path});
        return error.LLVMCompilationFailed;
    }

    return .{
        .artifact_dir = artifact_dir,
        .bitcode_path = bitcode_path,
        .object_path = object_path,
    };
}

fn validateWasmStaticFunctionRelocations(
    module: *const backend.wasm.WasmModule,
    static_data_exports: []const backend.StaticDataExport,
) backend.wasm.WasmModule.SymbolLookupError!void {
    for (static_data_exports) |data_export| {
        for (data_export.relocations) |relocation| {
            if (relocation.kind != .function_pointer) continue;
            _ = try module.findDefinedFunctionSymbolExact(relocation.target_symbol_name);
        }
    }
}

fn mergeStaticDataWasmModule(
    ctx: *CliCtx,
    module: *backend.wasm.WasmModule,
    static_data_exports: []const backend.StaticDataExport,
    mode: backend.wasm.WasmModule.MergeMode,
) CliMainError!void {
    if (static_data_exports.len == 0) return;

    try validateWasmStaticFunctionRelocations(module, static_data_exports);

    var static_module = try backend.wasm.WasmModule.staticDataModule(ctx.gpa, static_data_exports);
    defer static_module.deinit();

    var merge_result = switch (mode) {
        .final_link => try module.mergeModule(&static_module),
        .relocatable_object => try module.mergeModuleForObject(&static_module),
    };
    merge_result.deinit();
}

fn writeCombinedLlvmWasmObject(
    ctx: *CliCtx,
    artifact_dir: []const u8,
    app_object_path: []const u8,
    static_data_exports: []const backend.StaticDataExport,
    opt: cli_args.OptLevel,
    owned_inputs: *std.ArrayList([]u8),
) CliMainError![]const u8 {
    var wasm_module = backend.wasm.WasmModule.init(ctx.gpa);
    defer wasm_module.deinit();
    wasm_module.addMemoryImport();
    _ = try wasm_module.addTableImportWithSymbol();
    _ = try wasm_module.addStackPointerImportWithSymbol();

    const app_bytes = try appendOwnedWasmInput(ctx, owned_inputs, app_object_path);
    var app_module = try preloadWasmObject(ctx, app_object_path, null, app_bytes);
    defer app_module.deinit();
    var app_merge = try wasm_module.mergeModuleForObject(&app_module);
    app_merge.deinit();

    try mergeStaticDataWasmModule(ctx, &wasm_module, static_data_exports, .relocatable_object);
    try wasm_module.verifyNoLinkObjectContract();

    const wasm_bytes = try wasm_module.encodeRelocatable(ctx.gpa);
    defer ctx.gpa.free(wasm_bytes);

    const obj_filename = try std.fmt.allocPrint(ctx.arena, "roc_app_llvm_wasm32_{s}.o", .{@tagName(opt)});
    const obj_path = try std.fs.path.join(ctx.arena, &.{ artifact_dir, obj_filename });
    backend.writeFileWindowsAvSafe(ctx.io.std_io, obj_path, wasm_bytes) catch |err| {
        std.log.err("Failed to write wasm object output: {}", .{err});
        return error.WasmOutputWriteFailed;
    };

    return obj_path;
}

fn rocBuildWasmLlvm(
    ctx: *CliCtx,
    args: cli_args.BuildArgs,
    link_type: roc_target.OutputKind,
    final_output_path: []const u8,
    platform_dir: []const u8,
    targets_config: roc_target.TargetsConfig,
    lowered: *const lir.CheckedPipeline.LoweredProgram,
    entrypoints: []const backend.Entrypoint,
    static_data_exports: []const backend.StaticDataExport,
) CliMainError!void {
    if (entrypoints.len == 0) {
        if (builtin.mode == .Debug) {
            std.debug.panic("LLVM wasm build invariant violated: no exported platform entrypoints", .{});
        }
        unreachable;
    }

    const app_object = try compileLlvmAppObject(
        ctx,
        args,
        .wasm32,
        link_type,
        lowered,
        entrypoints,
        static_data_exports,
        false,
        false,
    );
    defer std.Io.Dir.cwd().deleteTree(ctx.io.std_io, app_object.artifact_dir) catch {};

    var owned_inputs: std.ArrayList([]u8) = .empty;
    defer freeOwnedWasmInputs(ctx, &owned_inputs);

    const link_inputs = try collectPlatformLinkInputs(ctx, platform_dir, targets_config, .wasm32, link_type);
    if (link_type == .archive) {
        // Archives package whatever inputs the platform declared (possibly
        // just the app); no platform wasm file is required.
        const combined_obj = try writeCombinedLlvmWasmObject(ctx, app_object.artifact_dir, app_object.object_path, static_data_exports, args.opt, &owned_inputs);
        try writeArchiveOutput(ctx, .wasm32, final_output_path, link_inputs, &.{combined_obj});
        return;
    }

    if (link_inputs.platform_files_pre.len + link_inputs.platform_files_post.len == 0) {
        try ctx.io.stderr().writeAll("Error: wasm32 LLVM builds require a relocatable wasm platform file or archive.\n");
        return error.UnsupportedTarget;
    }

    if (link_inputs.wasm != null) {
        const combined_obj = try writeCombinedLlvmWasmObject(ctx, app_object.artifact_dir, app_object.object_path, static_data_exports, args.opt, &owned_inputs);
        const object_files = try ctx.arena.alloc([]const u8, 1);
        object_files[0] = combined_obj;
        const wasm_exports = try collectWasmPlatformExports(ctx, link_inputs, &owned_inputs);

        const link_config = linker.LinkConfig{
            .target_format = .wasm,
            .target_abi = null,
            .target_os = .freestanding,
            .target_arch = .wasm32,
            .output_path = final_output_path,
            .object_files = object_files,
            .platform_files_pre = link_inputs.platform_files_pre,
            .platform_files_post = link_inputs.platform_files_post,
            .extra_args = &.{},
            .can_exit_early = false,
            .disable_output = false,
            .wasm_initial_memory = configuredWasmMinimumMemory(args, link_inputs.wasm),
            .wasm_maximum_memory = if (link_inputs.wasm) |wasm| wasm.maximum_memory else null,
            .wasm_stack_size = configuredWasmStackBytes(args, link_inputs.wasm),
            .wasm_import_memory = if (link_inputs.wasm) |wasm| wasm.import_memory.importsMemory() else false,
            .wasm_zero_filled_memory = configuredWasmZeroFilledMemory(link_inputs.wasm),
            .wasm_debug_info = args.debug,
            .wasm_optimize = wasmOptimizeMode(args.opt),
            .wasm_global_base = if (link_inputs.wasm) |wasm| wasm.global_base else null,
            .wasm_exports = wasm_exports,
            .platform_files_dir = link_inputs.platform_files_dir,
            .scratch_dir = app_object.artifact_dir,
        };

        linker.link(ctx, link_config) catch |err| {
            return ctx.fail(.{ .linker_failed = .{
                .err = err,
                .target = link_inputs.target_name,
            } });
        };
        return;
    }

    var loaded_module = true;
    var wasm_module = backend.wasm.WasmModule.init(ctx.gpa);
    configureWasmDataBase(&wasm_module, link_inputs.wasm);
    errdefer if (loaded_module) wasm_module.deinit();

    for (link_inputs.platform_files_pre) |path| {
        try addWasmInput(ctx, &wasm_module, &owned_inputs, path, &loaded_module);
    }
    for (link_inputs.platform_files_post) |path| {
        try addWasmInput(ctx, &wasm_module, &owned_inputs, path, &loaded_module);
    }

    try exportConfiguredWasmEntrypoints(&wasm_module);
    wasm_module.removeMemoryAndTableImports();

    const app_bytes = try appendOwnedWasmInput(ctx, &owned_inputs, app_object.object_path);
    var app_module = try preloadWasmObject(ctx, app_object.object_path, null, app_bytes);
    defer app_module.deinit();
    var app_merge = try wasm_module.mergeModule(&app_module);
    app_merge.deinit();

    try mergeStaticDataWasmModule(ctx, &wasm_module, static_data_exports, .final_link);

    var host_to_app_map: std.ArrayList(backend.wasm.WasmModule.HostToAppEntry) = .empty;
    defer host_to_app_map.deinit(ctx.gpa);
    try host_to_app_map.ensureTotalCapacity(ctx.gpa, entrypoints.len);

    for (entrypoints) |entry| {
        const fn_index = try wasm_module.findDefinedFunctionIndexExact(entry.symbol_name);
        host_to_app_map.appendAssumeCapacity(.{
            .name = entry.symbol_name,
            .fn_index = fn_index,
        });
    }

    try wasm_module.linkHostToAppCalls(host_to_app_map.items);

    const memory_config = configuredWasmMemory(args, link_inputs.wasm);
    try wasm_module.finalizeMemoryAndTableWithConfig(memory_config);
    try wasm_module.resolveRelocations();

    const called_fns = try ctx.gpa.alloc(bool, wasm_module.liveFunctionCount());
    defer ctx.gpa.free(called_fns);
    @memset(called_fns, false);
    try wasm_module.eliminateDeadCode(called_fns);

    try wasm_module.verifyNoBuiltinImports();
    try wasm_module.materializeFuncBodies();

    const wasm_bytes = try wasm_module.encode(ctx.gpa);
    defer ctx.gpa.free(wasm_bytes);
    backend.writeFileWindowsAvSafe(ctx.io.std_io, final_output_path, wasm_bytes) catch |err| {
        std.log.err("Failed to write wasm output: {}", .{err});
        return error.WasmOutputWriteFailed;
    };

    wasm_module.deinit();
    loaded_module = false;
}

fn rocBuildLlvm(ctx: *CliCtx, args: cli_args.BuildArgs) CliMainError!void {
    const timer_start_ns = std.Io.Timestamp.now(ctx.io.std_io, .real).nanoseconds;

    var reporter = makeReporter(ctx, "roc build", args.timings);
    defer reporter.deinit();
    reporter.start();

    const output_path = if (args.output) |output|
        try ctx.arena.dupe(u8, output)
    else if (args.synthetic_output_basename) |basename|
        try ctx.arena.dupe(u8, basename)
    else
        try base.module_path.getModuleNameAlloc(ctx.arena, args.path);

    var build_env = try initCliBuildEnv(ctx, .{
        .max_threads = args.max_threads,
        .no_cache = args.no_cache,
        .verbose_cache = args.verbose,
        .resolution_config = resolutionConfigFromLimits(args.resolve_limits),
        .track_watch_inputs = args.watch_inputs_file != null,
        .source_dir_override = args.source_dir_override,
        .root_source_url = args.root_source_url,
    });
    if (args.synthetic_root_original_path) |original_path| {
        if (args.synthetic_root_original_source) |original_source| {
            build_env.setSyntheticRootSourceMappingWithLineOffset(original_path, original_source, args.synthetic_root_header_len, args.synthetic_root_header_lines);
        }
    }
    defer build_env.deinit();
    // Registered after build_env.deinit() so it runs first (LIFO), while build_env is still
    // valid: records discovered source inputs for `roc build --watch` on every exit path.
    defer writeBuildWatchInputsOnExit(ctx, args, &build_env);

    reporter.begin("Resolving Dependencies");
    build_env.discoverDependencies(args.path) catch |err| {
        reporter.fail();
        try renderDiagnostics(&build_env, ctx.io.stderr());
        return err;
    };
    reporter.end();

    const targets_config = build_env.getPlatformTargetsConfig() orelse {
        try renderProblem(ctx.gpa, ctx.io.stderr(), .{
            .no_platform_found = .{ .app_path = args.path },
        });
        return error.NoPlatformSource;
    };
    const platform_source = build_env.getPlatformRootFile();
    const platform_dir = if (platform_source) |path| std.fs.path.dirname(path) orelse "." else ".";

    const selected = if (args.require_host_runnable_output)
        try selectRunPlatformTarget(ctx, targets_config, platform_source, args.target)
    else
        try selectBuildPlatformTarget(ctx, targets_config, platform_source, args.target);
    const target = selected.target;
    const link_type = selected.output;

    if (target.isDynamic() and builtin.target.os.tag != .linux) {
        renderValidationError(ctx.gpa, .{
            .unsupported_glibc_cross = .{
                .target = target,
                .host_os = @tagName(builtin.target.os.tag),
            },
        }, ctx.io.stderr());
        return error.UnsupportedCrossCompilation;
    }

    if (target != .wasm32 and target.ptrBitWidth() != 64) {
        try ctx.io.stderr().print(
            "Error: roc build --opt={s} requires a 64-bit native host target, but {s} has {d}-bit pointers.\n",
            .{ @tagName(args.opt), @tagName(target), target.ptrBitWidth() },
        );
        return error.UnsupportedTarget;
    }

    const target_arch = target.toCpuArch();
    const target_os = target.toOsTag();
    switch (target_arch) {
        .x86_64, .aarch64, .wasm32 => {},
        else => {
            try ctx.io.stderr().print(
                "Error: roc build --opt={s} does not support the '{s}' architecture.\n",
                .{ @tagName(args.opt), @tagName(target_arch) },
            );
            return error.UnsupportedTarget;
        },
    }

    if (args.require_executable_output and link_type != .exe) {
        return rejectRequiredExecutableOutput(ctx, selected);
    }

    const final_output_path = if (args.output != null)
        output_path
    else blk: {
        const ext = target_selection.defaultBuildOutputExtension(link_type, target);
        break :blk try std.fmt.allocPrint(ctx.arena, "{s}{s}", .{ output_path, ext });
    };

    build_env.setTarget(target);
    build_env.setValidateTargetFilesForSelectedTarget(true);
    reporter.begin("Type Checking");
    build_env.compileDiscovered() catch |err| {
        reporter.fail();
        try renderDiagnostics(&build_env, ctx.io.stderr());
        return err;
    };
    reporter.endWithBreakdown(&frontEndBreakdown(build_env.getTimingInfo()));

    const diag = try build_env.renderDiagnostics(ctx.io.stderr());
    var total_warning_count = diag.warnings;
    if (diag.errors > 0) {
        reporter.fail();
        if (args.allow_errors) return;
        return error.CompilationFailed;
    }
    total_warning_count += try optimizedDbgWarningsForBuild(ctx, &build_env, args.opt);
    const resolved_targets_config = build_env.getPlatformTargetsConfig() orelse {
        try renderProblem(ctx.gpa, ctx.io.stderr(), .{
            .no_platform_found = .{ .app_path = args.path },
        });
        return error.NoPlatformSource;
    };

    const root_artifact = build_env.executableRootCheckedArtifact();
    const imported_artifacts = try build_env.collectImportedArtifactViews(ctx.gpa, root_artifact);
    defer ctx.gpa.free(imported_artifacts);
    const relation_artifacts = try build_env.collectRelationArtifactViews(ctx.gpa, root_artifact);
    defer ctx.gpa.free(relation_artifacts);

    const target_usize = base.target.TargetUsize.fromPtrBitWidth(target.ptrBitWidth());

    reporter.begin("Specializing");
    var lowered = try lowerCheckedSourceToLir(
        ctx.gpa,
        ctx.gpa,
        root_artifact,
        imported_artifacts,
        relation_artifacts,
        .linked_output,
        args.opt,
        target_usize,
        args.synthetic_default_platform,
    );
    defer lowered.deinit();
    reporter.end();

    const entrypoints = try nativeBuildEntrypoints(ctx, root_artifact, &lowered);
    defer ctx.gpa.free(entrypoints);

    const static_data_exports = try compile.static_data_exports.buildStaticData(
        ctx.gpa,
        .{
            .root = check.CheckedArtifact.loweringViewWithRelations(root_artifact, relation_artifacts),
            .imports = imported_artifacts,
        },
        &lowered,
        target,
        .{ .include_provided_exports = true },
    );
    defer compile.static_data_exports.deinitStaticData(ctx.gpa, static_data_exports);

    if (entrypoints.len == 0 and static_data_exports.len == 0) {
        if (builtin.mode == .Debug) {
            std.debug.panic("LLVM build invariant violated: no exported platform entrypoints or data symbols", .{});
        }
        unreachable;
    }

    if (target == .wasm32) {
        reporter.begin("Code Generation");
        try rocBuildWasmLlvm(
            ctx,
            args,
            link_type,
            final_output_path,
            platform_dir,
            resolved_targets_config,
            &lowered,
            entrypoints,
            static_data_exports,
        );
        reporter.end();
    } else {
        reporter.begin("Code Generation");
        const hosted_symbols = try hostedSymbolsFromLir(ctx.arena, &lowered.lir_result.store);
        const enable_default_platform_runtime = args.synthetic_default_platform and DefaultPlatformRuntimeObjects.forTarget(target) != null;

        const app_object = try compileLlvmAppObject(
            ctx,
            args,
            target,
            link_type,
            &lowered,
            entrypoints,
            static_data_exports,
            enable_default_platform_runtime,
            args.synthetic_default_platform,
        );
        defer std.Io.Dir.cwd().deleteTree(ctx.io.std_io, app_object.artifact_dir) catch {};

        var static_data_obj_path: ?[]const u8 = null;
        if (static_data_exports.len > 0) {
            var object_compiler = backend.ObjectFileCompiler.init(ctx.gpa);
            const static_obj_filename = try std.fmt.allocPrint(ctx.arena, "roc_static_data_{s}.o", .{@tagName(target)});
            const static_obj_path = try std.fs.path.join(ctx.arena, &.{ app_object.artifact_dir, static_obj_filename });
            try object_compiler.compileStaticDataObjectAndWrite(
                static_data_exports,
                target,
                static_obj_path,
                ctx.coreCtx(),
            );
            static_data_obj_path = static_obj_path;
        }

        const link_inputs = try collectPlatformLinkInputs(ctx, platform_dir, resolved_targets_config, target, link_type);

        var object_files = try std.array_list.Managed([]const u8).initCapacity(ctx.arena, 4);
        try object_files.append(app_object.object_path);
        if (static_data_obj_path) |path| {
            try object_files.append(path);
        }
        if (enable_default_platform_runtime) {
            if (try writeDefaultPlatformRuntimeObject(ctx, app_object.artifact_dir, target)) |runtime_path| {
                try object_files.append(runtime_path);
            } else {
                return error.UnsupportedTarget;
            }
        }
        reporter.end();

        reporter.begin("Linking");
        if (link_type == .archive) {
            try writeArchiveOutput(ctx, target, final_output_path, link_inputs, object_files.items);
        } else {
            try verifyHostInputSymbols(
                ctx,
                try hostInputPaths(ctx, link_inputs),
                hosted_symbols,
                link_inputs.target_name,
                args.synthetic_default_platform,
            );

            const force_undefined_symbols = try staticDataLinkRootSymbols(
                ctx,
                static_data_exports,
                enable_default_platform_runtime and args.debug,
            );
            const app_export_symbols = try sharedLibraryAppExports(ctx, entrypoints, static_data_exports);
            const export_symbols = try sharedLibraryExports(ctx, link_type, link_inputs, app_export_symbols);

            const link_config = linker.LinkConfig{
                .target_format = linker.TargetFormat.detectFromOs(target_os),
                .target_abi = llvmBuildLinkAbi(target, args.synthetic_default_platform),
                .target_os = target_os,
                .target_arch = target_arch,
                .output_path = final_output_path,
                .output_kind = linkerOutputKind(link_type),
                // LLVM output uses the symbol ABI, so host archives resolve
                // by symbol reference and unused host code can be stripped.
                .lazy_platform_archives = true,
                .object_files = object_files.items,
                .platform_files_pre = link_inputs.platform_files_pre,
                .platform_files_post = link_inputs.platform_files_post,
                .extra_args = &.{},
                .force_undefined_symbols = force_undefined_symbols,
                .export_symbols = export_symbols,
                .can_exit_early = false,
                .disable_output = false,
                .platform_files_dir = link_inputs.platform_files_dir,
                .scratch_dir = app_object.artifact_dir,
                .macho_dwarf_object = if (target_os == .macos and link_type != .archive)
                    app_object.object_path
                else
                    null,
            };

            linker.link(ctx, link_config) catch |err| {
                reporter.fail();
                return ctx.fail(.{ .linker_failed = .{
                    .err = err,
                    .target = link_inputs.target_name,
                } });
            };
        }
        reporter.end();
    }

    const elapsed_ns = @as(u64, @intCast(std.Io.Timestamp.now(ctx.io.std_io, .real).nanoseconds - timer_start_ns));
    reporter.finish();
    const cache_stats = build_env.getBuildStats();
    const cache_percent = if (cache_stats.modules_total > 0)
        @as(u32, @intCast((cache_stats.cache_hits * 100) / cache_stats.modules_total))
    else
        0;

    if (!args.suppress_build_status) {
        try printBuildSuccess(ctx, final_output_path, total_warning_count, elapsed_ns, args.verbose, cache_stats, cache_percent);
    }

    if (args.warning_count_out) |warning_count_out| {
        warning_count_out.* = total_warning_count;
    }

    exitBuildOnWarningsIfRequested(ctx, args, &build_env, total_warning_count);
}

fn rocBuildNative(ctx: *CliCtx, args: cli_args.BuildArgs) CliMainError!void {
    const timer_start_ns = std.Io.Timestamp.now(ctx.io.std_io, .real).nanoseconds;

    var reporter = makeReporter(ctx, "roc build", args.timings);
    defer reporter.deinit();
    reporter.start();

    const output_path = if (args.output) |output|
        try ctx.arena.dupe(u8, output)
    else if (args.synthetic_output_basename) |basename|
        try ctx.arena.dupe(u8, basename)
    else
        try base.module_path.getModuleNameAlloc(ctx.arena, args.path);

    const cache_config = CacheConfig{
        .enabled = true,
        .verbose = false,
        .roc_ctx = ctx.coreCtx(),
    };
    var cache_manager = CacheManager.init(ctx.gpa, cache_config, ctx.coreCtx());
    const cache_dir = try cache_manager.config.getCacheEntriesDir(ctx.arena);
    const build_cache_dir = try std.fs.path.join(ctx.arena, &.{ cache_dir, "roc_build" });
    ensureCompilerCacheDirExists(ctx.io.std_io, build_cache_dir) catch |err| switch (err) {
        error.PathAlreadyExists => {},
        else => return err,
    };

    var build_env = try initCliBuildEnv(ctx, .{
        .max_threads = args.max_threads,
        .no_cache = args.no_cache,
        .verbose_cache = args.verbose,
        .resolution_config = resolutionConfigFromLimits(args.resolve_limits),
        .track_watch_inputs = args.watch_inputs_file != null,
        .source_dir_override = args.source_dir_override,
        .root_source_url = args.root_source_url,
    });
    if (args.synthetic_root_original_path) |original_path| {
        if (args.synthetic_root_original_source) |original_source| {
            build_env.setSyntheticRootSourceMappingWithLineOffset(original_path, original_source, args.synthetic_root_header_len, args.synthetic_root_header_lines);
        }
    }
    defer build_env.deinit();
    // Registered after build_env.deinit() so it runs first (LIFO), while build_env is still
    // valid: records discovered source inputs for `roc build --watch` on every exit path.
    defer writeBuildWatchInputsOnExit(ctx, args, &build_env);

    reporter.begin("Resolving Dependencies");
    build_env.discoverDependencies(args.path) catch |err| {
        reporter.fail();
        try renderDiagnostics(&build_env, ctx.io.stderr());
        return err;
    };
    reporter.end();

    const targets_config = build_env.getPlatformTargetsConfig() orelse {
        try renderProblem(ctx.gpa, ctx.io.stderr(), .{
            .no_platform_found = .{ .app_path = args.path },
        });
        return error.NoPlatformSource;
    };
    const platform_source = build_env.getPlatformRootFile();
    const platform_dir = if (platform_source) |path| std.fs.path.dirname(path) orelse "." else ".";

    const selected = if (args.require_host_runnable_output)
        try selectRunPlatformTarget(ctx, targets_config, platform_source, args.target)
    else
        try selectBuildPlatformTarget(ctx, targets_config, platform_source, args.target);
    const target = selected.target;
    const link_type = selected.output;

    if (args.require_executable_output and link_type != .exe) {
        return rejectRequiredExecutableOutput(ctx, selected);
    }

    const target_arch = target.toCpuArch();
    const target_os = target.toOsTag();
    if (target.isDynamic() and builtin.target.os.tag != .linux) {
        renderValidationError(ctx.gpa, .{
            .unsupported_glibc_cross = .{
                .target = target,
                .host_os = @tagName(builtin.target.os.tag),
            },
        }, ctx.io.stderr());
        return error.UnsupportedCrossCompilation;
    }

    switch (target_arch) {
        .x86_64, .aarch64, .wasm32 => {},
        else => {
            try ctx.io.stderr().print(
                "Error: The native object backend does not support the '{s}' architecture.\n",
                .{@tagName(target_arch)},
            );
            return error.UnsupportedTarget;
        },
    }

    const final_output_path = if (args.output != null)
        output_path
    else blk: {
        const ext = target_selection.defaultBuildOutputExtension(link_type, target);
        break :blk try std.fmt.allocPrint(ctx.arena, "{s}{s}", .{ output_path, ext });
    };

    build_env.setTarget(target);
    build_env.setValidateTargetFilesForSelectedTarget(true);
    reporter.begin("Type Checking");
    build_env.compileDiscovered() catch |err| {
        reporter.fail();
        try renderDiagnostics(&build_env, ctx.io.stderr());
        return err;
    };
    reporter.endWithBreakdown(&frontEndBreakdown(build_env.getTimingInfo()));

    const diag = try build_env.renderDiagnostics(ctx.io.stderr());
    var total_warning_count = diag.warnings;
    if (diag.errors > 0) {
        reporter.fail();
        if (args.allow_errors) return;
        return error.CompilationFailed;
    }
    total_warning_count += try optimizedDbgWarningsForBuild(ctx, &build_env, args.opt);
    const resolved_targets_config = build_env.getPlatformTargetsConfig() orelse {
        try renderProblem(ctx.gpa, ctx.io.stderr(), .{
            .no_platform_found = .{ .app_path = args.path },
        });
        return error.NoPlatformSource;
    };

    const root_artifact = build_env.executableRootCheckedArtifact();
    const imported_artifacts = try build_env.collectImportedArtifactViews(ctx.gpa, root_artifact);
    defer ctx.gpa.free(imported_artifacts);
    const relation_artifacts = try build_env.collectRelationArtifactViews(ctx.gpa, root_artifact);
    defer ctx.gpa.free(relation_artifacts);

    const target_usize = base.target.TargetUsize.fromPtrBitWidth(target.ptrBitWidth());

    reporter.begin("Specializing");
    var lowered = try lowerCheckedSourceToLir(
        ctx.gpa,
        ctx.gpa,
        root_artifact,
        imported_artifacts,
        relation_artifacts,
        .linked_output,
        args.opt,
        target_usize,
        args.synthetic_default_platform,
    );
    defer lowered.deinit();
    reporter.end();

    const entrypoints = try nativeBuildEntrypoints(ctx, root_artifact, &lowered);
    defer ctx.gpa.free(entrypoints);

    const static_data_exports = try compile.static_data_exports.buildStaticData(
        ctx.gpa,
        .{
            .root = check.CheckedArtifact.loweringViewWithRelations(root_artifact, relation_artifacts),
            .imports = imported_artifacts,
        },
        &lowered,
        target,
        .{ .include_provided_exports = true },
    );
    defer compile.static_data_exports.deinitStaticData(ctx.gpa, static_data_exports);

    if (target_arch == .wasm32) {
        reporter.begin("Code Generation");
        try rocBuildWasmSurgical(
            ctx,
            args,
            target,
            link_type,
            final_output_path,
            build_cache_dir,
            platform_dir,
            resolved_targets_config,
            &lowered,
            entrypoints,
            static_data_exports,
        );
        reporter.end();

        const elapsed_ns = @as(u64, @intCast(std.Io.Timestamp.now(ctx.io.std_io, .real).nanoseconds - timer_start_ns));
        reporter.finish();
        const cache_stats = build_env.getBuildStats();
        const cache_percent = if (cache_stats.modules_total > 0)
            @as(u32, @intCast((cache_stats.cache_hits * 100) / cache_stats.modules_total))
        else
            0;

        if (!args.suppress_build_status) {
            try printBuildSuccess(ctx, final_output_path, total_warning_count, elapsed_ns, args.verbose, cache_stats, cache_percent);
        }
        return;
    }

    reporter.begin("Code Generation");
    if (entrypoints.len == 0 and static_data_exports.len == 0) {
        if (builtin.mode == .Debug) {
            std.debug.panic("native build invariant violated: no exported platform entrypoints or data symbols", .{});
        }
        unreachable;
    }

    var object_compiler = backend.ObjectFileCompiler.init(ctx.gpa);
    object_compiler.enable_default_platform_runtime = args.synthetic_default_platform;

    const build_scratch_dir = createUniqueTempDir(ctx) catch |err| {
        return ctx.fail(.{ .temp_dir_failed = .{ .err = err } });
    };
    const cleanup_build_scratch_dir = true;
    defer if (cleanup_build_scratch_dir) {
        compile.CacheCleanup.deleteTempDir(ctx.io.std_io, build_scratch_dir);
    };

    const obj_filename = try std.fmt.allocPrint(ctx.arena, "roc_app_{s}.o", .{@tagName(target)});
    const obj_path = try std.fs.path.join(ctx.arena, &.{ build_scratch_dir, obj_filename });
    object_compiler.compileToObjectFileAndWrite(
        &lowered.lir_result.store,
        &lowered.lir_result.layouts,
        entrypoints,
        static_data_exports,
        lowered.lir_result.store.getProcSpecs(),
        target,
        obj_path,
        ctx.coreCtx(),
    ) catch |err| {
        reporter.fail();
        std.log.err("Native compilation failed: {}", .{err});
        return error.NativeCompilationFailed;
    };

    const link_inputs = try collectPlatformLinkInputs(ctx, platform_dir, resolved_targets_config, target, link_type);

    const builtins_path = try std.fs.path.join(ctx.arena, &.{ build_scratch_dir, BuiltinsObjects.filenameExtern(target) });
    backend.writeFileWindowsAvSafe(ctx.io.std_io, builtins_path, BuiltinsObjects.forTargetExtern(target)) catch {
        return error.BuiltinsExtractionFailed;
    };

    var object_files = try std.array_list.Managed([]const u8).initCapacity(ctx.arena, 4);
    try object_files.append(obj_path);
    try object_files.append(builtins_path);
    if (args.synthetic_default_platform) {
        if (try writeDefaultPlatformRuntimeObject(ctx, build_scratch_dir, target)) |runtime_path| {
            try object_files.append(runtime_path);
        } else {
            return error.UnsupportedTarget;
        }
    }
    reporter.end();

    reporter.begin("Linking");
    if (link_type == .archive) {
        try writeArchiveOutput(ctx, target, final_output_path, link_inputs, object_files.items);
    } else {
        try verifyHostInputSymbols(
            ctx,
            try hostInputPaths(ctx, link_inputs),
            try hostedSymbolsFromLir(ctx.arena, &lowered.lir_result.store),
            link_inputs.target_name,
            args.synthetic_default_platform,
        );

        const force_undefined_symbols = try staticDataLinkRootSymbols(
            ctx,
            static_data_exports,
            false,
        );
        const app_export_symbols = try sharedLibraryAppExports(ctx, entrypoints, static_data_exports);
        const export_symbols = try sharedLibraryExports(ctx, link_type, link_inputs, app_export_symbols);

        const link_config = linker.LinkConfig{
            .target_format = linker.TargetFormat.detectFromOs(target_os),
            .target_abi = linker.TargetAbi.fromRocTarget(target),
            .target_os = target_os,
            .target_arch = target_arch,
            .output_path = final_output_path,
            .output_kind = linkerOutputKind(link_type),
            // Dev output uses the symbol ABI, so host archives resolve by
            // symbol reference and unused host code can be stripped.
            .lazy_platform_archives = true,
            .object_files = object_files.items,
            .platform_files_pre = link_inputs.platform_files_pre,
            .platform_files_post = link_inputs.platform_files_post,
            .extra_args = &.{},
            .force_undefined_symbols = force_undefined_symbols,
            .export_symbols = export_symbols,
            .can_exit_early = false,
            .disable_output = false,
            .platform_files_dir = link_inputs.platform_files_dir,
            .scratch_dir = build_scratch_dir,
            .macho_dwarf_object = if (target_os == .macos and link_type != .archive)
                obj_path
            else
                null,
        };

        linker.link(ctx, link_config) catch |err| {
            reporter.fail();
            return ctx.fail(.{ .linker_failed = .{
                .err = err,
                .target = link_inputs.target_name,
            } });
        };
    }
    reporter.end();

    const elapsed_ns = @as(u64, @intCast(std.Io.Timestamp.now(ctx.io.std_io, .real).nanoseconds - timer_start_ns));
    reporter.finish();
    const cache_stats = build_env.getBuildStats();
    const cache_percent = if (cache_stats.modules_total > 0)
        @as(u32, @intCast((cache_stats.cache_hits * 100) / cache_stats.modules_total))
    else
        0;

    if (!args.suppress_build_status) {
        try printBuildSuccess(ctx, final_output_path, total_warning_count, elapsed_ns, args.verbose, cache_stats, cache_percent);
    }

    if (args.warning_count_out) |warning_count_out| {
        warning_count_out.* = total_warning_count;
    }

    exitBuildOnWarningsIfRequested(ctx, args, &build_env, total_warning_count);
}

/// Build a standalone binary with the interpreter and an embedded LIR image.
/// This is the primary build path that creates executables or libraries without requiring IPC.
fn rocBuildEmbedded(ctx: *CliCtx, args: cli_args.BuildArgs) CliMainError!void {
    const timer_start_ns = std.Io.Timestamp.now(ctx.io.std_io, .real).nanoseconds;

    var reporter = makeReporter(ctx, "roc build", args.timings);
    defer reporter.deinit();
    reporter.start();

    const output_path = if (args.output) |output|
        try ctx.arena.dupe(u8, output)
    else if (args.synthetic_output_basename) |basename|
        try ctx.arena.dupe(u8, basename)
    else
        try base.module_path.getModuleNameAlloc(ctx.arena, args.path);

    const cache_config = CacheConfig{
        .enabled = true,
        .verbose = false,
        .roc_ctx = ctx.coreCtx(),
    };
    var cache_manager = CacheManager.init(ctx.gpa, cache_config, ctx.coreCtx());
    const cache_dir = try cache_manager.config.getCacheEntriesDir(ctx.arena);
    const build_cache_dir = try std.fs.path.join(ctx.arena, &.{ cache_dir, "roc_build" });
    ensureCompilerCacheDirExists(ctx.io.std_io, build_cache_dir) catch |err| switch (err) {
        error.PathAlreadyExists => {},
        else => return err,
    };

    var build_env = try initCliBuildEnv(ctx, .{
        .max_threads = args.max_threads,
        .no_cache = args.no_cache,
        .verbose_cache = args.verbose,
        .resolution_config = resolutionConfigFromLimits(args.resolve_limits),
        .track_watch_inputs = args.watch_inputs_file != null,
        .source_dir_override = args.source_dir_override,
        .root_source_url = args.root_source_url,
    });
    if (args.synthetic_root_original_path) |original_path| {
        if (args.synthetic_root_original_source) |original_source| {
            build_env.setSyntheticRootSourceMappingWithLineOffset(original_path, original_source, args.synthetic_root_header_len, args.synthetic_root_header_lines);
        }
    }
    defer build_env.deinit();
    // Registered after build_env.deinit() so it runs first (LIFO), while build_env is still
    // valid: records discovered source inputs for `roc build --watch` on every exit path.
    defer writeBuildWatchInputsOnExit(ctx, args, &build_env);

    reporter.begin("Resolving Dependencies");
    build_env.discoverDependencies(args.path) catch |err| {
        reporter.fail();
        try renderDiagnostics(&build_env, ctx.io.stderr());
        return err;
    };
    reporter.end();

    const targets_config = build_env.getPlatformTargetsConfig() orelse {
        try renderProblem(ctx.gpa, ctx.io.stderr(), .{
            .no_platform_found = .{ .app_path = args.path },
        });
        return error.NoPlatformSource;
    };
    const platform_source = build_env.getPlatformRootFile();
    const platform_dir = if (platform_source) |path| std.fs.path.dirname(path) orelse "." else ".";

    const selected = if (args.require_host_runnable_output)
        try selectRunPlatformTarget(ctx, targets_config, platform_source, args.target)
    else
        try selectBuildPlatformTarget(ctx, targets_config, platform_source, args.target);
    const target = selected.target;
    const link_type = selected.output;

    const native_target = RocTarget.detectNative();
    if (target != native_target) {
        const stderr = ctx.io.stderr();
        try stderr.print("Error: The interpreter backend only supports building for the native target ({s}).\n\n", .{@tagName(native_target)});
        try stderr.print("To cross-compile for {s}, use the dev backend:\n\n", .{@tagName(target)});
        try stderr.print("    roc build --opt=dev --target={s} {s}\n\n", .{ @tagName(target), args.path });
        return error.UnsupportedCrossCompilation;
    }

    if (args.require_executable_output and link_type != .exe) {
        return rejectRequiredExecutableOutput(ctx, selected);
    }

    const target_arch = target.toCpuArch();
    const target_os = target.toOsTag();
    const final_output_path = if (args.output != null)
        output_path
    else blk: {
        const ext = target_selection.defaultBuildOutputExtension(link_type, target);
        break :blk try std.fmt.allocPrint(ctx.arena, "{s}{s}", .{ output_path, ext });
    };

    build_env.setTarget(target);
    build_env.setValidateTargetFilesForSelectedTarget(true);
    reporter.begin("Type Checking");
    build_env.compileDiscovered() catch |err| {
        reporter.fail();
        try renderDiagnostics(&build_env, ctx.io.stderr());
        return err;
    };
    reporter.endWithBreakdown(&frontEndBreakdown(build_env.getTimingInfo()));

    const diag = try build_env.renderDiagnostics(ctx.io.stderr());
    var total_warning_count = diag.warnings;
    if (diag.errors > 0) {
        reporter.fail();
        if (args.allow_errors) return;
        return error.CompilationFailed;
    }
    total_warning_count += try optimizedDbgWarningsForBuild(ctx, &build_env, args.opt);
    const resolved_targets_config = build_env.getPlatformTargetsConfig() orelse {
        try renderProblem(ctx.gpa, ctx.io.stderr(), .{
            .no_platform_found = .{ .app_path = args.path },
        });
        return error.NoPlatformSource;
    };

    const root_artifact = build_env.executableRootCheckedArtifact();
    const imported_artifacts = try build_env.collectImportedArtifactViews(ctx.gpa, root_artifact);
    defer ctx.gpa.free(imported_artifacts);
    const relation_artifacts = try build_env.collectRelationArtifactViews(ctx.gpa, root_artifact);
    defer ctx.gpa.free(relation_artifacts);

    const page_size = try SharedMemoryAllocator.getSystemPageSize();
    var shm = try createSharedMemory(ctx.io.std_io, page_size);
    defer shm.deinit(ctx.gpa);

    const shm_allocator = shm.allocator();
    const image_header = try shm_allocator.create(lir.LirImage.Header);

    reporter.begin("Specializing");
    const lowered = try lowerCheckedSourceToLir(
        shm_allocator,
        ctx.gpa,
        root_artifact,
        imported_artifacts,
        relation_artifacts,
        .{ .platform_entrypoints = .lir_image },
        args.opt,
        base.target.TargetUsize.native,
        false,
    );
    reporter.end();

    reporter.begin("Code Generation");
    const platform_entrypoints = try lowered.platformEntrypoints(shm_allocator);
    try lir.LirImage.fillHeaderInSharedMemory(
        image_header,
        shm.base_ptr,
        shm.getUsedSize(),
        &lowered.lir_result,
        platform_entrypoints,
    );
    shm.updateHeader();

    const lir_image = try ctx.arena.dupe(u8, shm.base_ptr[0..shm.getUsedSize()]);
    const entrypoint_names = try lowered.platformEntrypointNames(ctx.arena, root_artifact);
    if (entrypoint_names.len == 0) {
        if (builtin.mode == .Debug) {
            std.debug.panic("embedded build invariant violated: no platform entrypoints", .{});
        }
        unreachable;
    }

    const link_inputs = try collectPlatformLinkInputs(ctx, platform_dir, resolved_targets_config, target, link_type);

    const shim_filename = try shimLibraryCacheFilename(ctx, .lir, target);
    const shim_path = try std.fs.path.join(ctx.arena, &.{ build_cache_dir, shim_filename });
    std.Io.Dir.cwd().access(ctx.io.std_io, shim_path, .{}) catch {
        extractShimLibrary(ctx, .lir, shim_path, target) catch |err| {
            return ctx.fail(.{ .shim_generation_failed = .{ .err = err } });
        };
    };

    const enable_debug = args.debug or (builtin.mode == .Debug);
    const platform_shim_path = try generatePlatformHostShim(
        ctx,
        build_cache_dir,
        entrypoint_names,
        null,
        target,
        lir_image,
        true,
        false,
        enable_debug,
    );

    var object_files = try std.array_list.Managed([]const u8).initCapacity(ctx.arena, 4);
    try object_files.append(shim_path);
    if (platform_shim_path) |path| {
        try object_files.append(path);
    }
    reporter.end();

    reporter.begin("Linking");
    if (link_type == .archive) {
        try writeArchiveOutput(ctx, target, final_output_path, link_inputs, object_files.items);
    } else {
        var extra_args = try std.array_list.Managed([]const u8).initCapacity(ctx.arena, 8);
        if (target.isMacOS()) {
            try extra_args.append("-lSystem");
        }

        const link_config = linker.LinkConfig{
            .target_format = linker.TargetFormat.detectFromOs(target_os),
            .target_abi = linker.TargetAbi.fromRocTarget(target),
            .target_os = target_os,
            .target_arch = target_arch,
            .output_path = final_output_path,
            .output_kind = linkerOutputKind(link_type),
            .object_files = object_files.items,
            .platform_files_pre = link_inputs.platform_files_pre,
            .platform_files_post = link_inputs.platform_files_post,
            .extra_args = extra_args.items,
            .export_symbols = try sharedLibraryExports(ctx, link_type, link_inputs, entrypoint_names),
            .can_exit_early = false,
            .disable_output = false,
            .wasm_initial_memory = configuredWasmMinimumMemory(args, link_inputs.wasm),
            .wasm_maximum_memory = if (link_inputs.wasm) |wasm| wasm.maximum_memory else null,
            .wasm_stack_size = configuredWasmStackBytes(args, link_inputs.wasm),
            .wasm_import_memory = if (link_inputs.wasm) |wasm| wasm.import_memory.importsMemory() else false,
            .wasm_zero_filled_memory = configuredWasmZeroFilledMemory(link_inputs.wasm),
            .wasm_debug_info = args.debug,
            .wasm_optimize = wasmOptimizeMode(args.opt),
            .wasm_global_base = if (link_inputs.wasm) |wasm| wasm.global_base else null,
            .platform_files_dir = link_inputs.platform_files_dir,
            .scratch_dir = build_cache_dir,
        };

        linker.link(ctx, link_config) catch |err| {
            reporter.fail();
            return ctx.fail(.{ .linker_failed = .{
                .err = err,
                .target = link_inputs.target_name,
            } });
        };
    }
    reporter.end();

    const elapsed_ns_embed = @as(u64, @intCast(std.Io.Timestamp.now(ctx.io.std_io, .real).nanoseconds - timer_start_ns));
    reporter.finish();
    const cache_stats = build_env.getBuildStats();
    const cache_percent = if (cache_stats.modules_total > 0)
        @as(u32, @intCast((cache_stats.cache_hits * 100) / cache_stats.modules_total))
    else
        0;

    if (!args.suppress_build_status) {
        try printBuildSuccess(ctx, final_output_path, total_warning_count, elapsed_ns_embed, args.verbose, cache_stats, cache_percent);
    }

    if (args.warning_count_out) |warning_count_out| {
        warning_count_out.* = total_warning_count;
    }

    exitBuildOnWarningsIfRequested(ctx, args, &build_env, total_warning_count);
}

// Test cache blob format
// Binary format for caching test results.

const CliTestResult = enum { passed, failed, compiler_error };

const CliTestFailureDetailVisibility = enum(u8) {
    always = 0,
    verbose_only = 1,
};

const CliTestTranscriptEventKind = enum(u8) {
    dbg = 0,
    expect_failed = 1,
    crashed = 2,
    crash_diagnostic = 3,
};

const CliTestTranscriptStream = enum(u8) {
    stdout = 0,
    stderr = 1,
};

const CliTestTranscriptEvent = struct {
    stream: CliTestTranscriptStream,
    kind: CliTestTranscriptEventKind,
    payload: []const u8,
};

const CliTestResultItem = struct {
    result: CliTestResult,
    order: u32,
    region: base.Region,
    transcript: []const CliTestTranscriptEvent = &.{},
    failure_detail: ?[]const u8,
    failure_detail_visibility: CliTestFailureDetailVisibility = .always,
};

const CliModuleTestResult = struct {
    env: *const ModuleEnv,
    path: []const u8,
    results: []const CliTestResultItem,
    cached: bool,
};

const CliTestRunSummary = struct {
    passed: u32 = 0,
    failed: u32 = 0,
    compiler_errors: u32 = 0,
    modules_with_tests: u32 = 0,
    cached_modules: u32 = 0,
};

const CliTestPlanEntry = struct {
    module_index: u32,
    root_index: u32,
    root_order: u32,
    result_index: u32,
    region: base.Region,
    symbol_name: [:0]const u8,
};

const CliTestPlanModule = struct {
    module: BuildEnv.CompiledModuleInfo,
    artifact: *const check.CheckedArtifact.CheckedModuleArtifact,
    test_roots: []check.CheckedArtifact.RootRequest,
    first_entry_index: u32,
    entry_count: u32,
    cached_results: ?[]CliTestResultItem = null,
    cached_summary: CliTestRunSummary = .{},

    fn releaseCachedResults(self: *CliTestPlanModule) []CliTestResultItem {
        const results = self.cached_results orelse {
            if (builtin.mode == .Debug) {
                std.debug.panic("CLI test invariant violated: cached results were released from an uncached plan module", .{});
            }
            unreachable;
        };
        self.cached_results = null;
        return results;
    }
};

const CliTestPlan = struct {
    modules: []CliTestPlanModule,
    entries: []CliTestPlanEntry,

    fn deinit(self: *CliTestPlan, allocator: Allocator) void {
        for (self.modules) |*module| {
            allocator.free(module.test_roots);
            if (module.cached_results) |results| {
                deinitCliTestResultItems(allocator, results);
            }
        }
        allocator.free(self.modules);
        deinitCliTestPlanEntries(allocator, self.entries);
    }
};

const CliCachedModuleTestResults = struct {
    results: []CliTestResultItem,
    summary: CliTestRunSummary,
};

fn deinitCliTestResultItemPayload(allocator: Allocator, result: CliTestResultItem) void {
    deinitCliTestTranscriptEvents(allocator, result.transcript);
    if (result.failure_detail) |message| allocator.free(message);
}

fn deinitCliTestResultItemPayloads(allocator: Allocator, results: []const CliTestResultItem) void {
    for (results) |result| deinitCliTestResultItemPayload(allocator, result);
}

fn deinitCliTestResultItems(allocator: Allocator, results: []const CliTestResultItem) void {
    deinitCliTestResultItemPayloads(allocator, results);
    allocator.free(@constCast(results));
}

fn deinitCliTestTranscriptEventPayloads(allocator: Allocator, events: []const CliTestTranscriptEvent) void {
    for (events) |event| {
        allocator.free(event.payload);
    }
}

fn deinitCliTestTranscriptEvents(allocator: Allocator, events: []const CliTestTranscriptEvent) void {
    deinitCliTestTranscriptEventPayloads(allocator, events);
    if (events.len > 0) allocator.free(@constCast(events));
}

fn deinitCliTestPlanEntries(allocator: Allocator, entries: []const CliTestPlanEntry) void {
    for (entries) |entry| {
        allocator.free(entry.symbol_name);
    }
    allocator.free(@constCast(entries));
}

const cli_test_cache_magic = "ROC_TEST_RESULTS_V6";

fn appendU32(bytes: *std.ArrayList(u8), allocator: std.mem.Allocator, value: u32) Allocator.Error!void {
    var buf: [4]u8 = undefined;
    std.mem.writeInt(u32, &buf, value, .little);
    try bytes.appendSlice(allocator, &buf);
}

fn readU32(bytes: []const u8, offset: *usize) ?u32 {
    if (offset.* + 4 > bytes.len) return null;
    const value = std.mem.readInt(u32, bytes[offset.*..][0..4], .little);
    offset.* += 4;
    return value;
}

fn readU8(bytes: []const u8, offset: *usize) ?u8 {
    if (offset.* >= bytes.len) return null;
    const value = bytes[offset.*];
    offset.* += 1;
    return value;
}

fn cliTestTranscriptEventPayload(event: CliTestTranscriptEvent) []const u8 {
    return event.payload;
}

fn cliTestCacheKey(
    artifact: *const check.CheckedArtifact.CheckedModuleArtifact,
) [32]u8 {
    var hasher = std.crypto.hash.sha2.Sha256.init(.{});
    hasher.update(cli_test_cache_magic);
    hasher.update(build_options.compiler_version);
    hasher.update(&artifact.key.bytes);
    var out: [32]u8 = undefined;
    hasher.final(&out);
    return out;
}

fn summarizeTestResults(results: []const CliTestResultItem) CliTestRunSummary {
    var summary = CliTestRunSummary{ .modules_with_tests = 1 };
    for (results) |result| {
        switch (result.result) {
            .passed => summary.passed += 1,
            .failed => summary.failed += 1,
            .compiler_error => summary.compiler_errors += 1,
        }
    }
    return summary;
}

fn storeCliTestResultsInCache(
    ctx: *CliCtx,
    cache_manager: ?*CacheManager,
    artifact: *const check.CheckedArtifact.CheckedModuleArtifact,
    results: []const CliTestResultItem,
) (Allocator.Error || error{NoHomeDirectory})!void {
    const manager = cache_manager orelse return;
    for (results) |result| {
        if (result.result == .compiler_error) return;
    }

    var bytes = std.ArrayList(u8).empty;
    defer bytes.deinit(ctx.gpa);
    try bytes.appendSlice(ctx.gpa, cli_test_cache_magic);
    try appendU32(&bytes, ctx.gpa, @intCast(results.len));
    for (results) |result| {
        try appendU32(&bytes, ctx.gpa, result.order);
        try bytes.append(ctx.gpa, switch (result.result) {
            .passed => 0,
            .failed => 1,
            .compiler_error => 2,
        });
        try appendU32(&bytes, ctx.gpa, @intCast(result.transcript.len));
        for (result.transcript) |event| {
            try bytes.append(ctx.gpa, @intFromEnum(event.kind));
            try bytes.append(ctx.gpa, @intFromEnum(event.stream));
            const payload = cliTestTranscriptEventPayload(event);
            try appendU32(&bytes, ctx.gpa, @intCast(payload.len));
            try bytes.appendSlice(ctx.gpa, payload);
        }
        if (result.failure_detail) |message| {
            try bytes.append(ctx.gpa, 1);
            try bytes.append(ctx.gpa, @intFromEnum(result.failure_detail_visibility));
            try appendU32(&bytes, ctx.gpa, @intCast(message.len));
            try bytes.appendSlice(ctx.gpa, message);
        } else {
            try bytes.append(ctx.gpa, 0);
        }
    }

    const entries_dir = try manager.config.getTestCacheDir(ctx.gpa);
    defer ctx.gpa.free(entries_dir);
    manager.storeRawBytes(cliTestCacheKey(artifact), bytes.items, entries_dir);
}

fn loadCliTestTranscriptEvents(
    allocator: Allocator,
    data: []const u8,
    offset: *usize,
) Allocator.Error!?[]CliTestTranscriptEvent {
    const count = readU32(data, offset) orelse return null;
    if (count == 0) return &.{};

    var events = std.ArrayList(CliTestTranscriptEvent).empty;
    var events_owned = false;
    defer {
        if (!events_owned) {
            for (events.items) |event| allocator.free(event.payload);
            events.deinit(allocator);
        }
    }

    for (0..@as(usize, @intCast(count))) |_| {
        const kind_raw = readU8(data, offset) orelse return null;
        const kind: CliTestTranscriptEventKind = switch (kind_raw) {
            0 => .dbg,
            1 => .expect_failed,
            2 => .crashed,
            3 => .crash_diagnostic,
            else => return null,
        };
        const stream_raw = readU8(data, offset) orelse return null;
        const stream: CliTestTranscriptStream = switch (stream_raw) {
            0 => .stdout,
            1 => .stderr,
            else => return null,
        };
        const payload_len = readU32(data, offset) orelse return null;
        const payload_len_usize: usize = @intCast(payload_len);
        if (offset.* + payload_len_usize > data.len) return null;
        const payload = try allocator.dupe(u8, data[offset.*..][0..payload_len_usize]);
        offset.* += payload_len_usize;
        errdefer allocator.free(payload);
        try events.append(allocator, .{
            .stream = stream,
            .kind = kind,
            .payload = payload,
        });
    }

    const owned = try events.toOwnedSlice(allocator);
    events_owned = true;
    return owned;
}

fn loadCachedCliTestResults(
    ctx: *CliCtx,
    cache_manager: ?*CacheManager,
    artifact: *const check.CheckedArtifact.CheckedModuleArtifact,
    module: BuildEnv.CompiledModuleInfo,
    test_roots: []const check.CheckedArtifact.RootRequest,
) (Allocator.Error || error{NoHomeDirectory})!?CliCachedModuleTestResults {
    const manager = cache_manager orelse return null;

    const entries_dir = try manager.config.getTestCacheDir(ctx.gpa);
    defer ctx.gpa.free(entries_dir);
    const data = manager.loadRawBytes(cliTestCacheKey(artifact), entries_dir) orelse return null;
    defer ctx.gpa.free(data);

    var offset: usize = 0;
    if (data.len < cli_test_cache_magic.len) return null;
    if (!std.mem.eql(u8, data[0..cli_test_cache_magic.len], cli_test_cache_magic)) return null;
    offset += cli_test_cache_magic.len;

    const count = readU32(data, &offset) orelse return null;
    if (count != test_roots.len) return null;

    var results = std.ArrayList(CliTestResultItem).empty;
    var results_owned_by_module = false;
    defer {
        if (!results_owned_by_module) {
            deinitCliTestResultItemPayloads(ctx.gpa, results.items);
            results.deinit(ctx.gpa);
        }
    }

    for (0..@as(usize, @intCast(count))) |root_index| {
        const order = readU32(data, &offset) orelse return null;
        const root = test_roots[root_index];
        if (order != root.order) return null;

        const result_tag = readU8(data, &offset) orelse return null;
        const result: CliTestResult = switch (result_tag) {
            0 => .passed,
            1 => .failed,
            2 => return null,
            else => return null,
        };
        const transcript = (try loadCliTestTranscriptEvents(ctx.gpa, data, &offset)) orelse return null;
        var transcript_owned_by_result = false;
        defer {
            if (!transcript_owned_by_result) deinitCliTestTranscriptEvents(ctx.gpa, transcript);
        }

        const has_message = readU8(data, &offset) orelse return null;

        const region = testRootRegion(module.semantic.env, root);

        var visibility: CliTestFailureDetailVisibility = .always;
        const message = if (has_message == 0) null else blk: {
            if (offset >= data.len) return null;
            visibility = switch (data[offset]) {
                0 => .always,
                1 => .verbose_only,
                else => return null,
            };
            offset += 1;
            const message_len = readU32(data, &offset) orelse return null;
            const message_len_usize: usize = @intCast(message_len);
            if (offset + message_len_usize > data.len) return null;
            const message = try ctx.gpa.dupe(u8, data[offset..][0..message_len_usize]);
            offset += message_len_usize;
            break :blk message;
        };

        try results.append(ctx.gpa, .{
            .result = result,
            .order = order,
            .region = region,
            .transcript = transcript,
            .failure_detail = message,
            .failure_detail_visibility = visibility,
        });
        transcript_owned_by_result = true;
    }
    if (offset != data.len) return null;

    var summary = summarizeTestResults(results.items);
    summary.cached_modules = 1;
    const owned_results = try results.toOwnedSlice(ctx.gpa);
    errdefer {
        deinitCliTestResultItems(ctx.gpa, owned_results);
    }
    results_owned_by_module = true;
    return .{
        .results = owned_results,
        .summary = summary,
    };
}

fn collectTestRootRequests(
    allocator: std.mem.Allocator,
    artifact: *const check.CheckedArtifact.CheckedModuleArtifact,
) Allocator.Error![]check.CheckedArtifact.RootRequest {
    var roots = std.ArrayList(check.CheckedArtifact.RootRequest).empty;
    errdefer roots.deinit(allocator);

    for (artifact.root_requests.requests) |root| {
        if (root.kind != .test_expect) continue;
        try roots.append(allocator, root);
    }

    return try roots.toOwnedSlice(allocator);
}

fn buildCliTestPlan(
    ctx: *CliCtx,
    modules: []const BuildEnv.CompiledModuleInfo,
) Allocator.Error!CliTestPlan {
    var planned_modules = std.ArrayList(CliTestPlanModule).empty;
    var entries = std.ArrayList(CliTestPlanEntry).empty;
    errdefer {
        for (planned_modules.items) |*module| {
            ctx.gpa.free(module.test_roots);
            if (module.cached_results) |results| {
                deinitCliTestResultItems(ctx.gpa, results);
            }
        }
        planned_modules.deinit(ctx.gpa);
        for (entries.items) |entry| {
            ctx.gpa.free(entry.symbol_name);
        }
        entries.deinit(ctx.gpa);
    }

    for (modules, 0..) |module, module_index| {
        const artifact = module.semantic.checked_artifact orelse continue;
        const test_roots = try collectTestRootRequests(ctx.gpa, artifact);
        errdefer ctx.gpa.free(test_roots);
        if (test_roots.len == 0) {
            ctx.gpa.free(test_roots);
            continue;
        }

        const first_entry_index: u32 = @intCast(entries.items.len);
        for (test_roots, 0..) |root, root_index| {
            const result_index: u32 = @intCast(entries.items.len);
            const symbol_name = try std.fmt.allocPrintSentinel(ctx.gpa, "roc_test_expect_{d}", .{result_index}, 0);
            entries.append(ctx.gpa, .{
                .module_index = @intCast(module_index),
                .root_index = @intCast(root_index),
                .root_order = root.order,
                .result_index = result_index,
                .region = testRootRegion(module.semantic.env, root),
                .symbol_name = symbol_name,
            }) catch |err| {
                ctx.gpa.free(symbol_name);
                return err;
            };
        }

        try planned_modules.append(ctx.gpa, .{
            .module = module,
            .artifact = artifact,
            .test_roots = test_roots,
            .first_entry_index = first_entry_index,
            .entry_count = @intCast(test_roots.len),
        });
    }

    const owned_modules = try planned_modules.toOwnedSlice(ctx.gpa);
    errdefer {
        for (owned_modules) |*module| {
            ctx.gpa.free(module.test_roots);
            if (module.cached_results) |results| {
                deinitCliTestResultItems(ctx.gpa, results);
            }
        }
        ctx.gpa.free(owned_modules);
    }
    const owned_entries = try entries.toOwnedSlice(ctx.gpa);
    errdefer deinitCliTestPlanEntries(ctx.gpa, owned_entries);

    return .{
        .modules = owned_modules,
        .entries = owned_entries,
    };
}

fn testRootRegion(
    env: *const ModuleEnv,
    root: check.CheckedArtifact.RootRequest,
) base.Region {
    return switch (root.source) {
        .statement => |statement| env.store.getStatementRegion(statement),
        else => {
            if (builtin.mode == .Debug) {
                std.debug.panic("CLI test invariant violated: test root was not published from an expect statement", .{});
            }
            unreachable;
        },
    };
}

const CliTestFailureDetail = struct {
    message: []const u8,
    visibility: CliTestFailureDetailVisibility,
};

fn appendExprSpanForExpectBindings(
    env: *const ModuleEnv,
    allocator: Allocator,
    stack: *std.ArrayList(CIR.Expr.Idx),
    span: CIR.Expr.Span,
) Allocator.Error!void {
    for (env.store.sliceExpr(span)) |expr_idx| {
        try stack.append(allocator, expr_idx);
    }
}

fn collectExpectBindingPatterns(
    env: *const ModuleEnv,
    allocator: Allocator,
    root: check.CheckedArtifact.RootRequest,
) Allocator.Error![]CIR.Pattern.Idx {
    const statement_idx = switch (root.source) {
        .statement => |statement| statement,
        else => return allocator.alloc(CIR.Pattern.Idx, 0),
    };
    const statement = env.store.getStatement(statement_idx);
    if (statement != .s_expect) return allocator.alloc(CIR.Pattern.Idx, 0);

    var stack = std.ArrayList(CIR.Expr.Idx).empty;
    defer stack.deinit(allocator);
    try stack.append(allocator, statement.s_expect.body);

    var patterns = std.ArrayList(CIR.Pattern.Idx).empty;
    errdefer patterns.deinit(allocator);

    while (stack.pop()) |expr_idx| {
        switch (env.store.getExpr(expr_idx)) {
            .e_lookup_local => |lookup| {
                if (std.mem.findScalar(CIR.Pattern.Idx, patterns.items, lookup.pattern_idx) == null) {
                    try patterns.append(allocator, lookup.pattern_idx);
                }
            },
            .e_list => |list| try appendExprSpanForExpectBindings(env, allocator, &stack, list.elems),
            .e_tuple => |tuple| try appendExprSpanForExpectBindings(env, allocator, &stack, tuple.elems),
            .e_str => |str| try appendExprSpanForExpectBindings(env, allocator, &stack, str.span),
            .e_tag => |tag| try appendExprSpanForExpectBindings(env, allocator, &stack, tag.args),
            .e_call => |call| {
                try stack.append(allocator, call.func);
                try appendExprSpanForExpectBindings(env, allocator, &stack, call.args);
            },
            .e_record => |record| {
                if (record.ext) |ext| try stack.append(allocator, ext);
                for (env.store.sliceRecordFields(record.fields)) |field_idx| {
                    try stack.append(allocator, env.store.getRecordField(field_idx).value);
                }
            },
            .e_block => |block| {
                try stack.append(allocator, block.final_expr);
                for (0..block.stmts.span.len) |stmt_offset| {
                    const stmt_idx = env.store.statementAt(block.stmts, stmt_offset);
                    switch (env.store.getStatement(stmt_idx)) {
                        .s_decl => |decl| try stack.append(allocator, decl.expr),
                        .s_var => |decl| try stack.append(allocator, decl.expr),
                        .s_reassign => |assign| try stack.append(allocator, assign.expr),
                        .s_expr => |stmt| try stack.append(allocator, stmt.expr),
                        .s_expect => |stmt| try stack.append(allocator, stmt.body),
                        .s_dbg => |stmt| try stack.append(allocator, stmt.expr),
                        .s_return => |stmt| try stack.append(allocator, stmt.expr),
                        .s_for => |stmt| {
                            try stack.append(allocator, stmt.expr);
                            try stack.append(allocator, stmt.body);
                        },
                        .s_crash,
                        .s_var_uninitialized,
                        .s_import,
                        .s_alias_decl,
                        .s_nominal_decl,
                        .s_type_anno,
                        .s_type_var_alias,
                        .s_runtime_error,
                        .s_break,
                        .s_while,
                        .s_infinite_loop,
                        .s_breakable_loop,
                        => {},
                    }
                }
            },
            .e_if => |if_expr| {
                try stack.append(allocator, if_expr.final_else);
                for (env.store.sliceIfBranches(if_expr.branches)) |branch_idx| {
                    const branch = env.store.getIfBranch(branch_idx);
                    try stack.append(allocator, branch.cond);
                    try stack.append(allocator, branch.body);
                }
            },
            .e_match => |match_expr| {
                try stack.append(allocator, match_expr.cond);
                for (env.store.sliceMatchBranches(match_expr.branches)) |branch_idx| {
                    const branch = env.store.getMatchBranch(branch_idx);
                    try stack.append(allocator, branch.value);
                    if (branch.guard) |guard| try stack.append(allocator, guard);
                }
            },
            .e_lambda => |lambda| try stack.append(allocator, lambda.body),
            .e_closure => |closure| try stack.append(allocator, closure.lambda_idx),
            .e_nominal => |nominal| try stack.append(allocator, nominal.backing_expr),
            .e_nominal_external => |nominal| try stack.append(allocator, nominal.backing_expr),
            .e_binop => |binop| {
                try stack.append(allocator, binop.lhs);
                try stack.append(allocator, binop.rhs);
            },
            .e_unary_minus => |unary| try stack.append(allocator, unary.expr),
            .e_unary_not => |unary| try stack.append(allocator, unary.expr),
            .e_field_access => |field| try stack.append(allocator, field.receiver),
            .e_method_call => |call| {
                try stack.append(allocator, call.receiver);
                try appendExprSpanForExpectBindings(env, allocator, &stack, call.args);
            },
            .e_dispatch_call => |call| {
                try stack.append(allocator, call.receiver);
                try appendExprSpanForExpectBindings(env, allocator, &stack, call.args);
            },
            .e_interpolation => |interpolation| {
                try stack.append(allocator, interpolation.first);
                try appendExprSpanForExpectBindings(env, allocator, &stack, interpolation.parts);
            },
            .e_structural_eq => |eq| {
                try stack.append(allocator, eq.lhs);
                try stack.append(allocator, eq.rhs);
            },
            .e_structural_hash => |hash| {
                try stack.append(allocator, hash.value);
                try stack.append(allocator, hash.hasher);
            },
            .e_method_eq => |eq| {
                try stack.append(allocator, eq.lhs);
                try stack.append(allocator, eq.rhs);
            },
            .e_type_method_call => |call| try appendExprSpanForExpectBindings(env, allocator, &stack, call.args),
            .e_type_dispatch_call => |call| try appendExprSpanForExpectBindings(env, allocator, &stack, call.args),
            .e_tuple_access => |access| try stack.append(allocator, access.tuple),
            .e_dbg => |dbg| try stack.append(allocator, dbg.expr),
            .e_expect_err => |expect_err| try stack.append(allocator, expect_err.expr),
            .e_expect => |expect_expr| try stack.append(allocator, expect_expr.body),
            .e_return => |ret| try stack.append(allocator, ret.expr),
            .e_for => |for_expr| {
                try stack.append(allocator, for_expr.expr);
                try stack.append(allocator, for_expr.body);
            },
            .e_run_low_level => |run| try appendExprSpanForExpectBindings(env, allocator, &stack, run.args),
            .e_num,
            .e_frac_f32,
            .e_frac_f64,
            .e_dec,
            .e_dec_small,
            .e_num_from_numeral,
            .e_typed_int,
            .e_typed_frac,
            .e_typed_num_from_numeral,
            .e_str_segment,
            .e_bytes_literal,
            .e_lookup_external,
            .e_lookup_required,
            .e_empty_list,
            .e_empty_record,
            .e_zero_argument_tag,
            .e_runtime_error,
            .e_crash,
            .e_ellipsis,
            .e_anno_only,
            .e_derived_method,
            .e_break,
            .e_hosted_lambda,
            => {},
        }
    }

    return try patterns.toOwnedSlice(allocator);
}

fn sourceBindingForPattern(env: *const ModuleEnv, pattern_idx: CIR.Pattern.Idx) ?[]const u8 {
    const src = env.getSourceAll();
    for (env.store.sliceDefs(env.all_defs)) |def_idx| {
        const def = env.store.getDef(def_idx);
        if (def.pattern != pattern_idx) continue;

        switch (env.store.getExpr(def.expr)) {
            .e_lambda,
            .e_closure,
            .e_hosted_lambda,
            => return null,
            else => {},
        }

        const pattern_region = env.store.getPatternRegion(def.pattern);
        const expr_region = env.store.getExprRegion(def.expr);
        const start: usize = @intCast(pattern_region.start.offset);
        const end: usize = @intCast(expr_region.end.offset);
        if (start >= end or end > src.len) return null;
        return std.mem.trim(u8, src[start..end], " \t\r\n");
    }
    return null;
}

fn buildExpectFailureDetail(
    allocator: Allocator,
    env: *const ModuleEnv,
    root: check.CheckedArtifact.RootRequest,
) Allocator.Error!CliTestFailureDetail {
    const patterns = try collectExpectBindingPatterns(env, allocator, root);
    defer allocator.free(patterns);

    var message = std.ArrayList(u8).empty;
    errdefer message.deinit(allocator);

    var count: usize = 0;
    for (patterns) |pattern_idx| {
        const binding = sourceBindingForPattern(env, pattern_idx) orelse continue;
        if (count == 0) {
            try message.appendSlice(allocator, "Mentioned values:\n");
        }
        try message.appendSlice(allocator, binding);
        try message.append(allocator, '\n');
        count += 1;
    }

    if (count == 0) {
        message.deinit(allocator);
        return .{
            .message = try allocator.dupe(u8, "TEST FAILURE: expect failed"),
            .visibility = .verbose_only,
        };
    }

    return .{
        .message = try message.toOwnedSlice(allocator),
        .visibility = .always,
    };
}

const CliTestExecutionMode = enum {
    interpreter,
    dev,
    llvm_size,
    llvm_speed,

    fn displayName(self: CliTestExecutionMode) []const u8 {
        return switch (self) {
            .interpreter => "interpreter",
            .dev => "dev",
            .llvm_size => "LLVM size",
            .llvm_speed => "LLVM speed",
        };
    }
};

fn cliTestExecutionMode(opt: cli_args.OptLevel) CliTestExecutionMode {
    return switch (opt) {
        .interpreter => .interpreter,
        .dev => .dev,
        .size => .llvm_size,
        .speed => .llvm_speed,
    };
}

fn postCheckInlineModeForOpt(opt: cli_args.OptLevel) lir.CheckedPipeline.InlineMode {
    return switch (opt) {
        .size, .speed => .wrappers,
        .dev, .interpreter => .none,
    };
}

fn listInPlaceMapForOpt(opt: cli_args.OptLevel) bool {
    return switch (opt) {
        .size, .speed => true,
        .dev, .interpreter => false,
    };
}

fn tagReachabilityForOpt(opt: cli_args.OptLevel) bool {
    return switch (opt) {
        .size, .speed => true,
        .dev, .interpreter => false,
    };
}

fn optimizedDbgWarningsForBuild(
    ctx: *CliCtx,
    build_env: *BuildEnv,
    opt: cli_args.OptLevel,
) Allocator.Error!usize {
    return switch (opt) {
        .size, .speed => try build_env.renderOptimizedDbgWarnings(ctx.io.stderr(), @tagName(opt)),
        .dev, .interpreter => 0,
    };
}

fn inlineExpectModeForOpt(opt: cli_args.OptLevel) lir.CheckedPipeline.InlineExpectMode {
    return switch (opt) {
        .size, .speed => .omit,
        .dev, .interpreter => .run,
    };
}

/// Which checked root definitions become LIR roots for a backend.
const CheckedLirRoots = union(enum) {
    /// Provided exports plus platform-required bindings: LIR consumed by host
    /// shims and interpreters (run, embedded builds, hot reload, glue).
    platform_entrypoints: PlatformEntrypointArtifact,
    /// Provided exports plus platform-required bindings, with static data
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
fn lowerCheckedSourceToLir(
    lir_allocator: Allocator,
    gpa: Allocator,
    root_artifact: *const check.CheckedArtifact.CheckedModuleArtifact,
    imported_artifacts: []const check.CheckedArtifact.ImportedModuleView,
    relation_artifacts: []const check.CheckedArtifact.ImportedModuleView,
    roots: CheckedLirRoots,
    opt: cli_args.OptLevel,
    target_usize: base.target.TargetUsize,
    proc_debug_names: bool,
) Allocator.Error!lir.CheckedPipeline.LoweredProgram {
    const selected_roots: []const check.CheckedArtifact.RootRequest = switch (roots) {
        .platform_entrypoints => try lir.CheckedPipeline.selectPlatformEntrypointRoots(gpa, root_artifact.root_requests.runtime_requests),
        .linked_output => try lir.CheckedPipeline.selectPlatformEntrypointRoots(gpa, root_artifact.root_requests.runtime_requests),
        .test_plan => |plan| plan.requests,
    };
    defer switch (roots) {
        .platform_entrypoints, .linked_output => gpa.free(selected_roots),
        .test_plan => {},
    };

    return lir.CheckedPipeline.lowerCheckedModulesToLir(
        lir_allocator,
        .{
            .root = check.CheckedArtifact.loweringViewWithRelations(root_artifact, relation_artifacts),
            .imports = imported_artifacts,
        },
        .{
            .requests = selected_roots,
            // Host-visible data exports exist only in linked outputs.
            .include_provided_data_exports = switch (roots) {
                .linked_output => true,
                else => false,
            },
            // Internal readonly values are embedded by linked outputs and dev
            // RunImages. LirImage deliberately remains pointer-width independent.
            .include_internal_static_data = switch (roots) {
                .linked_output => true,
                .platform_entrypoints => |artifact| switch (artifact) {
                    .dev_run_image => true,
                    .lir_image => false,
                },
                .test_plan => false,
            },
            .test_plan_metadata = switch (roots) {
                .test_plan => |plan| plan.metadata,
                else => &.{},
            },
        },
        .{
            .target_usize = target_usize,
            .inline_mode = postCheckInlineModeForOpt(opt),
            // Test lowering executes inline expects at every opt level; other
            // backends omit them from optimized output.
            .inline_expects = switch (roots) {
                .test_plan => .run,
                else => inlineExpectModeForOpt(opt),
            },
            .list_in_place_map = listInPlaceMapForOpt(opt),
            .tag_reachability = tagReachabilityForOpt(opt),
            .proc_debug_names = proc_debug_names,
        },
    );
}

const CliTestRootRun = struct {
    root: check.CheckedArtifact.RootRequest,
    env: *const ModuleEnv,
    path: []const u8,
    result_index: u32,
    root_proc: lir.LirProcSpecId,
    region: base.Region,
    arg_layouts: []const layout.Idx,
    ret_layout: layout.Idx,
    symbol_name: [:0]const u8,
};

const CliLoweredTestModule = struct {
    planned_index: usize,
    lowered: lir.CheckedPipeline.LoweredProgram,
    root_runs: []CliTestRootRun,

    fn deinit(self: *CliLoweredTestModule, allocator: Allocator) void {
        deinitCliTestRootRuns(allocator, self.root_runs);
        self.lowered.deinit();
    }
};

fn deinitCliTestRootRuns(allocator: Allocator, runs: []CliTestRootRun) void {
    for (runs) |run| {
        allocator.free(run.arg_layouts);
    }
    allocator.free(runs);
}

fn collectCliTestRootRuns(
    ctx: *CliCtx,
    planned: *const CliTestPlanModule,
    plan_entries: []const CliTestPlanEntry,
    lowered: *const lir.CheckedPipeline.LoweredProgram,
) Allocator.Error![]CliTestRootRun {
    var runs = std.ArrayList(CliTestRootRun).empty;
    errdefer {
        for (runs.items) |run| {
            ctx.gpa.free(run.arg_layouts);
        }
        runs.deinit(ctx.gpa);
    }

    const root_procs = lowered.lir_result.root_procs.items;
    const root_metadata = lowered.lir_result.root_metadata.items;
    if (root_procs.len != root_metadata.len) {
        if (builtin.mode == .Debug) {
            std.debug.panic("CLI test invariant violated: root proc count differs from root metadata count", .{});
        }
        unreachable;
    }

    for (root_procs, root_metadata) |root_proc, metadata| {
        if (metadata.kind != .test_expect) continue;
        const test_plan = metadata.test_plan orelse {
            if (builtin.mode == .Debug) {
                std.debug.panic("CLI test invariant violated: lowered test root metadata is missing its explicit test-plan slot", .{});
            }
            unreachable;
        };
        if (test_plan.root_index >= planned.test_roots.len or test_plan.result_index >= plan_entries.len) {
            if (builtin.mode == .Debug) {
                std.debug.panic(
                    "CLI test invariant violated: lowered test-plan slot root/result ({d}/{d}) is outside module roots/results ({d}/{d})",
                    .{ test_plan.root_index, test_plan.result_index, planned.test_roots.len, plan_entries.len },
                );
            }
            unreachable;
        }
        const root_index: usize = @intCast(test_plan.root_index);
        const root = planned.test_roots[root_index];
        const plan_entry = plan_entries[@intCast(test_plan.result_index)];
        if (builtin.mode == .Debug and
            (metadata.order != root.order or
                plan_entry.result_index != test_plan.result_index or
                plan_entry.module_index != test_plan.module_index or
                plan_entry.root_index != test_plan.root_index or
                plan_entry.root_order != root.order))
        {
            std.debug.panic(
                "CLI test invariant violated: explicit plan metadata ({d}/{d}/{d}/{d}) differs from plan entry/root ({d}/{d}/{d}/{d})",
                .{
                    test_plan.result_index,
                    test_plan.module_index,
                    test_plan.root_index,
                    metadata.order,
                    plan_entry.result_index,
                    plan_entry.module_index,
                    plan_entry.root_index,
                    root.order,
                },
            );
        }
        const proc = lowered.lir_result.store.getProcSpec(root_proc);
        const arg_layouts = try argLayoutsForProc(ctx.gpa, &lowered.lir_result.store, root_proc);
        errdefer ctx.gpa.free(arg_layouts);
        try runs.append(ctx.gpa, .{
            .root = root,
            .env = planned.module.semantic.env,
            .path = planned.module.path,
            .result_index = plan_entry.result_index,
            .root_proc = root_proc,
            .region = testRootRegion(planned.module.semantic.env, root),
            .arg_layouts = arg_layouts,
            .ret_layout = proc.ret_layout,
            .symbol_name = plan_entry.symbol_name,
        });
    }

    if (runs.items.len != planned.test_roots.len) {
        if (builtin.mode == .Debug) {
            std.debug.panic(
                "CLI test invariant violated: lowered {d} test roots for {d} checked test roots",
                .{ runs.items.len, planned.test_roots.len },
            );
        }
        unreachable;
    }

    return try runs.toOwnedSlice(ctx.gpa);
}

fn interpreterTestFailureMessage(
    allocator: std.mem.Allocator,
    interpreter: *const eval.LirInterpreter,
    err: eval.LirInterpreter.Error,
) std.mem.Allocator.Error![]const u8 {
    const message = switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        error.RuntimeError => interpreter.getRuntimeErrorMessage() orelse "Roc runtime error",
        error.DivisionByZero => interpreter.getRuntimeErrorMessage() orelse "Division by zero",
        error.ComptimeExhaustiveness => "compile-time exhaustiveness failure reached runtime code",
        error.Crash => interpreter.getCrashMessage() orelse "Test crashed",
        error.ExpectErr => interpreter.getExpectErrMessage() orelse
            "The `?` operator evaluated an `Err` inside an `expect`",
    };
    return try allocator.dupe(u8, message);
}

fn appendFailedCliTestResult(
    ctx: *CliCtx,
    results: *std.ArrayList(CliTestResultItem),
    run: CliTestRootRun,
    result: CliTestResult,
    transcript: []CliTestTranscriptEvent,
    message: []const u8,
    visibility: CliTestFailureDetailVisibility,
) Allocator.Error!void {
    errdefer ctx.gpa.free(message);
    try results.append(ctx.gpa, .{
        .result = result,
        .order = run.root.order,
        .region = run.region,
        .transcript = transcript,
        .failure_detail = message,
        .failure_detail_visibility = visibility,
    });
}

fn copyCliTestTranscriptEventsFromEval(
    allocator: Allocator,
    events: []const eval.test_helpers.BoolRootEvent,
) Allocator.Error![]CliTestTranscriptEvent {
    if (events.len == 0) return &.{};

    const EventParts = struct {
        stream: CliTestTranscriptStream,
        kind: CliTestTranscriptEventKind,
        payload: []const u8,
    };
    const copied = try allocator.alloc(CliTestTranscriptEvent, events.len);
    var copied_len: usize = 0;
    errdefer {
        for (copied[0..copied_len]) |event| allocator.free(event.payload);
        allocator.free(copied);
    }

    for (events, 0..) |event, index| {
        const parts: EventParts = switch (event) {
            .dbg => |message| .{ .stream = .stderr, .kind = .dbg, .payload = message },
            .expect_failed => |message| .{ .stream = .stderr, .kind = .expect_failed, .payload = message },
            .crashed => |message| .{ .stream = .stderr, .kind = .crashed, .payload = message },
        };
        copied[index] = .{
            .stream = parts.stream,
            .kind = parts.kind,
            .payload = try allocator.dupe(u8, parts.payload),
        };
        copied_len += 1;
    }

    return copied;
}

fn copyCliTestTranscriptEvents(
    allocator: Allocator,
    events: []const CliTestTranscriptEvent,
) Allocator.Error![]CliTestTranscriptEvent {
    if (events.len == 0) return &.{};

    const copied = try allocator.alloc(CliTestTranscriptEvent, events.len);
    var copied_len: usize = 0;
    errdefer {
        for (copied[0..copied_len]) |event| allocator.free(event.payload);
        allocator.free(copied);
    }

    for (events, 0..) |event, index| {
        copied[index] = .{
            .stream = event.stream,
            .kind = event.kind,
            .payload = try allocator.dupe(u8, event.payload),
        };
        copied_len += 1;
    }

    return copied;
}

fn appendCliTestTranscriptEvent(
    allocator: Allocator,
    events: []const CliTestTranscriptEvent,
    stream: CliTestTranscriptStream,
    kind: CliTestTranscriptEventKind,
    payload: []const u8,
) Allocator.Error![]const CliTestTranscriptEvent {
    const extended = try allocator.alloc(CliTestTranscriptEvent, events.len + 1);
    errdefer allocator.free(extended);
    @memcpy(extended[0..events.len], events);

    const owned_payload = try allocator.dupe(u8, payload);
    errdefer allocator.free(owned_payload);
    extended[events.len] = .{
        .stream = stream,
        .kind = kind,
        .payload = owned_payload,
    };

    if (events.len > 0) allocator.free(@constCast(events));
    return extended;
}

fn copyCliTestResultItem(
    allocator: Allocator,
    result: CliTestResultItem,
) Allocator.Error!CliTestResultItem {
    const transcript = try copyCliTestTranscriptEvents(allocator, result.transcript);
    errdefer deinitCliTestTranscriptEvents(allocator, transcript);
    const failure_detail = if (result.failure_detail) |message|
        try allocator.dupe(u8, message)
    else
        null;
    errdefer if (failure_detail) |message| allocator.free(message);

    return .{
        .result = result.result,
        .order = result.order,
        .region = result.region,
        .transcript = transcript,
        .failure_detail = failure_detail,
        .failure_detail_visibility = result.failure_detail_visibility,
    };
}

const CliInterpreterTestHostEnv = struct {
    allocator: Allocator,
    echo_env: echo_platform.EchoEnv,
    events: std.ArrayList(CliTestTranscriptEvent) = .empty,

    fn init(allocator: Allocator, std_io: std.Io) CliInterpreterTestHostEnv {
        return .{
            .allocator = allocator,
            .echo_env = .{ .std_io = std_io },
        };
    }

    fn deinit(self: *CliInterpreterTestHostEnv) void {
        self.resetObservation();
        self.events.deinit(self.allocator);
    }

    fn resetObservation(self: *CliInterpreterTestHostEnv) void {
        deinitCliTestTranscriptEvents(self.allocator, self.events.items);
        self.events.clearRetainingCapacity();
        self.echo_env.inline_expect_failed = false;
    }

    fn takeTranscript(self: *CliInterpreterTestHostEnv) Allocator.Error![]CliTestTranscriptEvent {
        if (self.events.items.len == 0) return &.{};
        return try self.events.toOwnedSlice(self.allocator);
    }

    fn installCallbacks(_: *CliInterpreterTestHostEnv, roc_ops: *echo_platform.host_abi.RocOps) void {
        roc_ops.roc_dbg = &rocDbg;
        roc_ops.roc_expect_failed = &rocExpectFailed;
        roc_ops.roc_crashed = &rocCrashed;
    }

    fn fromOps(ops: *echo_platform.host_abi.RocOps) *CliInterpreterTestHostEnv {
        const echo_env: *echo_platform.EchoEnv = @ptrCast(@alignCast(ops.env));
        return @alignCast(@fieldParentPtr("echo_env", echo_env));
    }

    fn appendEvent(
        self: *CliInterpreterTestHostEnv,
        stream: CliTestTranscriptStream,
        kind: CliTestTranscriptEventKind,
        bytes: []const u8,
    ) void {
        const payload = self.allocator.dupe(u8, bytes) catch {
            std.debug.panic("CLI interpreter test host failed to allocate transcript payload", .{});
        };
        self.events.append(self.allocator, .{
            .stream = stream,
            .kind = kind,
            .payload = payload,
        }) catch {
            self.allocator.free(payload);
            std.debug.panic("CLI interpreter test host failed to append transcript event", .{});
        };
    }

    fn rocDbg(ops: *echo_platform.host_abi.RocOps, bytes: [*]const u8, len: usize) callconv(.c) void {
        fromOps(ops).appendEvent(.stderr, .dbg, bytes[0..len]);
    }

    fn rocExpectFailed(ops: *echo_platform.host_abi.RocOps, bytes: [*]const u8, len: usize) callconv(.c) void {
        const self = fromOps(ops);
        self.echo_env.inline_expect_failed = true;
        self.appendEvent(.stderr, .expect_failed, bytes[0..len]);
    }

    fn rocCrashed(ops: *echo_platform.host_abi.RocOps, bytes: [*]const u8, len: usize) callconv(.c) void {
        fromOps(ops).appendEvent(.stderr, .crashed, bytes[0..len]);
    }
};

fn runInterpreterTestRoots(
    ctx: *CliCtx,
    lowered: *lir.CheckedPipeline.LoweredProgram,
    root_runs: []const CliTestRootRun,
    results: *std.ArrayList(CliTestResultItem),
    summary: *CliTestRunSummary,
) Allocator.Error!void {
    var hosted_fn_array = [_]echo_platform.host_abi.HostedFn{echo_platform.host_abi.hostedFn(&echo_platform.echoHostedFn)};
    var host_env = CliInterpreterTestHostEnv.init(ctx.gpa, ctx.io.std_io);
    defer host_env.deinit();
    var roc_ops = echo_platform.makeDefaultRocOps(&host_env.echo_env, &hosted_fn_array);
    host_env.installCallbacks(&roc_ops);
    echo_platform.g_roc_ops = &roc_ops;
    var interpreter = try eval.LirInterpreter.init(
        ctx.gpa,
        &lowered.lir_result.store,
        &lowered.lir_result.layouts,
        &roc_ops,
        .preserve,
    );
    defer interpreter.deinit();

    for (root_runs) |run| {
        host_env.resetObservation();
        const eval_result = interpreter.eval(.{
            .proc_id = run.root_proc,
            .arg_layouts = run.arg_layouts,
            .ret_layout = run.ret_layout,
        }) catch |err| {
            var transcript: []const CliTestTranscriptEvent = try host_env.takeTranscript();
            errdefer deinitCliTestTranscriptEvents(ctx.gpa, transcript);
            summary.failed += 1;
            // When a `?` operator failed the expect, point the report's
            // source snippet at the `?` itself.
            const failure_region = switch (err) {
                error.ExpectErr => interpreter.getExpectErrRegion() orelse run.region,
                else => run.region,
            };
            const message = try interpreterTestFailureMessage(ctx.gpa, &interpreter, err);
            var message_owned = true;
            errdefer if (message_owned) ctx.gpa.free(message);
            const failure_detail: ?[]const u8 = switch (err) {
                error.Crash => blk: {
                    transcript = try appendCliTestTranscriptEvent(ctx.gpa, transcript, .stderr, .crash_diagnostic, message);
                    ctx.gpa.free(message);
                    message_owned = false;
                    break :blk null;
                },
                else => blk: {
                    message_owned = false;
                    break :blk message;
                },
            };
            try results.append(ctx.gpa, .{
                .result = .failed,
                .order = run.root.order,
                .region = failure_region,
                .transcript = transcript,
                .failure_detail = failure_detail,
                .failure_detail_visibility = .always,
            });
            continue;
        };
        const transcript = try host_env.takeTranscript();
        errdefer deinitCliTestTranscriptEvents(ctx.gpa, transcript);

        const passed = switch (eval_result) {
            .value => |value| blk: {
                const ok = value.read(u8) != 0;
                interpreter.dropValue(value, run.ret_layout);
                break :blk ok;
            },
        };

        if (passed) {
            summary.passed += 1;
            try results.append(ctx.gpa, .{ .result = .passed, .order = run.root.order, .region = run.region, .transcript = transcript, .failure_detail = null });
        } else {
            summary.failed += 1;
            const detail = try buildExpectFailureDetail(ctx.gpa, run.env, run.root);
            try appendFailedCliTestResult(
                ctx,
                results,
                run,
                .failed,
                transcript,
                detail.message,
                detail.visibility,
            );
        }
    }
}

fn appendCompilerErrorsForRuns(
    ctx: *CliCtx,
    mode: CliTestExecutionMode,
    err: CliMainError,
    root_runs: []const CliTestRootRun,
    results: *std.ArrayList(CliTestResultItem),
    summary: *CliTestRunSummary,
) Allocator.Error!void {
    for (root_runs) |run| {
        summary.compiler_errors += 1;
        try appendFailedCliTestResult(
            ctx,
            results,
            run,
            .compiler_error,
            &.{},
            try std.fmt.allocPrint(ctx.gpa, "{s} test backend failed: {s}", .{ mode.displayName(), @errorName(err) }),
            .always,
        );
    }
}

fn addCliTestResultToSummary(summary: *CliTestRunSummary, result: CliTestResult) void {
    switch (result) {
        .passed => summary.passed += 1,
        .failed => summary.failed += 1,
        .compiler_error => summary.compiler_errors += 1,
    }
}

fn cliTestResultItemFromEval(
    ctx: *CliCtx,
    run: CliTestRootRun,
    eval_result: eval.test_helpers.BoolRootEvalResult,
) Allocator.Error!CliTestResultItem {
    var transcript: []const CliTestTranscriptEvent = try copyCliTestTranscriptEventsFromEval(ctx.gpa, eval_result.events);
    errdefer deinitCliTestTranscriptEvents(ctx.gpa, transcript);

    switch (eval_result.outcome) {
        .passed => |passed| {
            if (passed) {
                return .{ .result = .passed, .order = run.root.order, .region = run.region, .transcript = transcript, .failure_detail = null };
            } else {
                const detail = try buildExpectFailureDetail(ctx.gpa, run.env, run.root);
                return .{
                    .result = .failed,
                    .order = run.root.order,
                    .region = run.region,
                    .transcript = transcript,
                    .failure_detail = detail.message,
                    .failure_detail_visibility = detail.visibility,
                };
            }
        },
        .crashed => |message| {
            const crash_transcript = try appendCliTestTranscriptEvent(ctx.gpa, transcript, .stderr, .crash_diagnostic, message);
            transcript = &.{};
            return .{
                .result = .failed,
                .order = run.root.order,
                .region = run.region,
                .transcript = crash_transcript,
                .failure_detail = null,
                .failure_detail_visibility = .always,
            };
        },
        .expect_err => |failure| {
            // Point the report's source snippet at the `?` expression
            // whose Err failed the expect.
            const message = try ctx.gpa.dupe(u8, failure.message);
            errdefer ctx.gpa.free(message);
            return .{
                .result = .failed,
                .order = run.root.order,
                .region = base.Region.from_raw_offsets(failure.region_start, failure.region_end),
                .transcript = transcript,
                .failure_detail = message,
                .failure_detail_visibility = .always,
            };
        },
    }
}

fn appendEvalResultForRun(
    ctx: *CliCtx,
    run: CliTestRootRun,
    eval_result: eval.test_helpers.BoolRootEvalResult,
    results: *std.ArrayList(CliTestResultItem),
    summary: *CliTestRunSummary,
) Allocator.Error!void {
    const result = try cliTestResultItemFromEval(ctx, run, eval_result);
    errdefer deinitCliTestResultItemPayload(ctx.gpa, result);
    addCliTestResultToSummary(summary, result.result);
    try results.append(ctx.gpa, result);
}

fn runCompiledTestRoots(
    ctx: *CliCtx,
    mode: CliTestExecutionMode,
    lowered: *const lir.CheckedPipeline.LoweredProgram,
    root_runs: []const CliTestRootRun,
    results: *std.ArrayList(CliTestResultItem),
    summary: *CliTestRunSummary,
) Allocator.Error!void {
    var bool_roots = try ctx.gpa.alloc(eval.test_helpers.BoolRoot, root_runs.len);
    defer ctx.gpa.free(bool_roots);

    for (root_runs, 0..) |run, i| {
        bool_roots[i] = .{
            .symbol_name = run.symbol_name,
            .proc = run.root_proc,
            .arg_layouts = run.arg_layouts,
            .ret_layout = run.ret_layout,
        };
    }

    const eval_results = switch (mode) {
        .dev => eval.test_helpers.devEvalBoolRoots(
            ctx.gpa,
            &lowered.lir_result.store,
            &lowered.lir_result.layouts,
            bool_roots,
        ),
        .llvm_size => eval.test_helpers.llvmEvalBoolRoots(
            ctx.gpa,
            &lowered.lir_result.store,
            &lowered.lir_result.layouts,
            bool_roots,
            .size,
        ),
        .llvm_speed => eval.test_helpers.llvmEvalBoolRoots(
            ctx.gpa,
            &lowered.lir_result.store,
            &lowered.lir_result.layouts,
            bool_roots,
            .speed,
        ),
        .interpreter => unreachable,
    } catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        else => {
            try appendCompilerErrorsForRuns(ctx, mode, err, root_runs, results, summary);
            return;
        },
    };
    defer eval.test_helpers.deinitBoolRootEvalResults(ctx.gpa, eval_results);

    for (root_runs, eval_results) |run, eval_result| {
        try appendEvalResultForRun(ctx, run, eval_result, results, summary);
    }
}

fn lowerPlannedTestModule(
    ctx: *CliCtx,
    build_env: *BuildEnv,
    planned_index: usize,
    planned: *const CliTestPlanModule,
    plan_entries: []const CliTestPlanEntry,
    opt: cli_args.OptLevel,
) Allocator.Error!CliLoweredTestModule {
    const imported_artifacts = try build_env.collectImportedArtifactViews(ctx.gpa, planned.artifact);
    defer ctx.gpa.free(imported_artifacts);
    const relation_artifacts = try build_env.collectRelationArtifactViews(ctx.gpa, planned.artifact);
    defer ctx.gpa.free(relation_artifacts);

    const root_plan_metadata = try ctx.gpa.alloc(postcheck.Common.RootTestPlanMetadata, planned.test_roots.len);
    defer ctx.gpa.free(root_plan_metadata);
    for (planned.test_roots, 0..) |root, root_index| {
        const entry_index: usize = @intCast(planned.first_entry_index + @as(u32, @intCast(root_index)));
        const plan_entry = plan_entries[entry_index];
        if (builtin.mode == .Debug and (plan_entry.root_index != root_index or plan_entry.root_order != root.order)) {
            std.debug.panic(
                "CLI test invariant violated: plan entry root index/order ({d}/{d}) differs from lowered root ({d}/{d})",
                .{ plan_entry.root_index, plan_entry.root_order, root_index, root.order },
            );
        }
        root_plan_metadata[root_index] = .{
            .root_order = root.order,
            .result_index = plan_entry.result_index,
            .module_index = plan_entry.module_index,
            .root_index = plan_entry.root_index,
        };
    }

    var lowered = try lowerCheckedSourceToLir(
        ctx.gpa,
        ctx.gpa,
        planned.artifact,
        imported_artifacts,
        relation_artifacts,
        .{ .test_plan = .{
            .requests = planned.test_roots,
            .metadata = root_plan_metadata,
        } },
        opt,
        base.target.TargetUsize.native,
        false,
    );
    errdefer lowered.deinit();

    const root_runs = try collectCliTestRootRuns(ctx, planned, plan_entries, &lowered);
    errdefer deinitCliTestRootRuns(ctx.gpa, root_runs);

    return .{
        .planned_index = planned_index,
        .lowered = lowered,
        .root_runs = root_runs,
    };
}

fn runCheckedArtifactTests(
    ctx: *CliCtx,
    build_env: *BuildEnv,
    planned: *const CliTestPlanModule,
    plan_entries: []const CliTestPlanEntry,
    opt: cli_args.OptLevel,
    cache_manager: ?*CacheManager,
    module_results: *std.ArrayList(CliModuleTestResult),
) (Allocator.Error || error{NoHomeDirectory})!CliTestRunSummary {
    const module = planned.module;
    const artifact = planned.artifact;
    var lowered_module = try lowerPlannedTestModule(ctx, build_env, 0, planned, plan_entries, opt);
    defer lowered_module.deinit(ctx.gpa);

    var results = std.ArrayList(CliTestResultItem).empty;
    errdefer {
        deinitCliTestResultItemPayloads(ctx.gpa, results.items);
        results.deinit(ctx.gpa);
    }

    var summary = CliTestRunSummary{};
    const mode = cliTestExecutionMode(opt);
    switch (mode) {
        .interpreter => try runInterpreterTestRoots(ctx, &lowered_module.lowered, lowered_module.root_runs, &results, &summary),
        .dev => try runCompiledTestRoots(ctx, mode, &lowered_module.lowered, lowered_module.root_runs, &results, &summary),
        .llvm_size, .llvm_speed => unreachable,
    }
    summary.modules_with_tests = 1;

    try storeCliTestResultsInCache(ctx, cache_manager, artifact, results.items);

    try module_results.append(ctx.gpa, .{
        .env = module.semantic.env,
        .path = module.path,
        .results = try ctx.gpa.dupe(CliTestResultItem, results.items),
        .cached = false,
    });
    results.deinit(ctx.gpa);

    return summary;
}

fn deinitFreshResultSlots(allocator: Allocator, slots: []?[]CliTestResultItem) void {
    for (slots) |maybe_results| {
        if (maybe_results) |results| {
            deinitCliTestResultItems(allocator, results);
        }
    }
    allocator.free(slots);
}

fn runLlvmLoweredTestModulesOnce(
    ctx: *CliCtx,
    mode: CliTestExecutionMode,
    lowered_modules: []const CliLoweredTestModule,
    fresh_results: []?[]CliTestResultItem,
    summaries: []CliTestRunSummary,
    max_workers: ?usize,
    live_output: ?*CliOptimizedLiveTestOutput,
) ReportRenderError!void {
    if (lowered_modules.len == 0) return;

    var bool_modules = std.ArrayList(eval.test_helpers.BoolRootModule).empty;
    defer {
        for (bool_modules.items) |module| {
            ctx.gpa.free(@constCast(module.roots));
        }
        bool_modules.deinit(ctx.gpa);
    }

    var live_runs = std.ArrayList(CliTestRootRun).empty;
    defer live_runs.deinit(ctx.gpa);

    for (lowered_modules) |*lowered_module| {
        const bool_roots = try ctx.gpa.alloc(eval.test_helpers.BoolRoot, lowered_module.root_runs.len);
        errdefer ctx.gpa.free(bool_roots);
        for (lowered_module.root_runs, 0..) |run, root_index| {
            bool_roots[root_index] = .{
                .symbol_name = run.symbol_name,
                .proc = run.root_proc,
                .arg_layouts = run.arg_layouts,
                .ret_layout = run.ret_layout,
            };
            if (live_output != null) {
                try live_runs.append(ctx.gpa, run);
            }
        }

        try bool_modules.append(ctx.gpa, .{
            .store = &lowered_module.lowered.lir_result.store,
            .layouts = &lowered_module.lowered.lir_result.layouts,
            .roots = bool_roots,
        });
    }

    var completion_callback: ?eval.test_helpers.BoolRootCompletionCallback = null;
    var event_callback: ?eval.test_helpers.BoolRootEventCallback = null;
    if (live_output) |live| {
        live.setRuns(live_runs.items);
        completion_callback = .{
            .context = live,
            .complete = &CliOptimizedLiveTestOutput.completionCallback,
        };
        event_callback = .{
            .context = live,
            .notify = &CliOptimizedLiveTestOutput.eventCallback,
        };
    }
    defer if (live_output) |live| live.clearRuns();

    const eval_results = eval.test_helpers.llvmEvalBoolRootModulesWithMaxWorkersAndCallbacks(
        ctx.gpa,
        bool_modules.items,
        switch (mode) {
            .llvm_size => .size,
            .llvm_speed => .speed,
            .interpreter, .dev => unreachable,
        },
        max_workers,
        completion_callback,
        event_callback,
    ) catch |err| switch (err) {
        error.OutOfMemory => return error.OutOfMemory,
        else => {
            for (lowered_modules) |*lowered_module| {
                var results = std.ArrayList(CliTestResultItem).empty;
                errdefer {
                    deinitCliTestResultItemPayloads(ctx.gpa, results.items);
                    results.deinit(ctx.gpa);
                }

                try appendCompilerErrorsForRuns(
                    ctx,
                    mode,
                    err,
                    lowered_module.root_runs,
                    &results,
                    &summaries[lowered_module.planned_index],
                );
                summaries[lowered_module.planned_index].modules_with_tests = 1;
                const owned_results = try results.toOwnedSlice(ctx.gpa);
                fresh_results[lowered_module.planned_index] = owned_results;
                if (live_output) |live| {
                    for (lowered_module.root_runs, owned_results) |run, result| {
                        live.publishCopiedEntry(@intCast(run.result_index), run.env, run.path, result);
                    }
                    try live.checkError();
                }
            }
            return;
        },
    };
    defer eval.test_helpers.deinitBoolRootEvalResults(ctx.gpa, eval_results);
    if (live_output) |live| try live.checkError();

    var eval_index: usize = 0;
    for (lowered_modules) |*lowered_module| {
        var results = std.ArrayList(CliTestResultItem).empty;
        errdefer {
            deinitCliTestResultItemPayloads(ctx.gpa, results.items);
            results.deinit(ctx.gpa);
        }

        for (lowered_module.root_runs) |run| {
            try appendEvalResultForRun(
                ctx,
                run,
                eval_results[eval_index],
                &results,
                &summaries[lowered_module.planned_index],
            );
            eval_index += 1;
        }
        summaries[lowered_module.planned_index].modules_with_tests = 1;
        fresh_results[lowered_module.planned_index] = try results.toOwnedSlice(ctx.gpa);
    }
}

fn appendPlannedModuleResult(
    ctx: *CliCtx,
    module_results: *std.ArrayList(CliModuleTestResult),
    planned: *const CliTestPlanModule,
    results: []CliTestResultItem,
    cached: bool,
) Allocator.Error!void {
    var results_owned_by_module = false;
    errdefer if (!results_owned_by_module) deinitCliTestResultItems(ctx.gpa, results);
    try module_results.append(ctx.gpa, .{
        .env = planned.module.semantic.env,
        .path = planned.module.path,
        .results = results,
        .cached = cached,
    });
    results_owned_by_module = true;
}

fn runOptimizedTestPlan(
    ctx: *CliCtx,
    build_env: *BuildEnv,
    test_plan: *CliTestPlan,
    opt: cli_args.OptLevel,
    max_workers: ?usize,
    cache_manager: ?*CacheManager,
    module_results: *std.ArrayList(CliModuleTestResult),
    total: *CliTestRunSummary,
    live_output: ?*CliOptimizedLiveTestOutput,
) (ReportRenderError || error{NoHomeDirectory})!void {
    const mode = cliTestExecutionMode(opt);
    switch (mode) {
        .llvm_size, .llvm_speed => {},
        .interpreter, .dev => unreachable,
    }

    const summaries = try ctx.gpa.alloc(CliTestRunSummary, test_plan.modules.len);
    defer ctx.gpa.free(summaries);
    for (summaries) |*summary| {
        summary.* = .{};
    }

    const fresh_results = try ctx.gpa.alloc(?[]CliTestResultItem, test_plan.modules.len);
    defer deinitFreshResultSlots(ctx.gpa, fresh_results);
    for (fresh_results) |*slot| {
        slot.* = null;
    }

    var lowered_modules = std.ArrayList(CliLoweredTestModule).empty;
    defer {
        for (lowered_modules.items) |*lowered_module| {
            lowered_module.deinit(ctx.gpa);
        }
        lowered_modules.deinit(ctx.gpa);
    }

    for (test_plan.modules, 0..) |*planned, planned_index| {
        if (planned.cached_results != null) {
            summaries[planned_index] = planned.cached_summary;
            if (live_output) |live| {
                for (planned.cached_results.?, 0..) |result, root_index| {
                    live.publishCopiedEntry(
                        @intCast(planned.first_entry_index + @as(u32, @intCast(root_index))),
                        planned.module.semantic.env,
                        planned.module.path,
                        result,
                    );
                }
                try live.checkError();
            }
            continue;
        }

        try lowered_modules.append(
            ctx.gpa,
            try lowerPlannedTestModule(ctx, build_env, planned_index, planned, test_plan.entries, opt),
        );
    }

    try runLlvmLoweredTestModulesOnce(ctx, mode, lowered_modules.items, fresh_results, summaries, max_workers, live_output);

    for (lowered_modules.items) |*lowered_module| {
        const planned = &test_plan.modules[lowered_module.planned_index];
        if (summaries[lowered_module.planned_index].compiler_errors == 0) {
            try storeCliTestResultsInCache(ctx, cache_manager, planned.artifact, fresh_results[lowered_module.planned_index].?);
        }
    }

    for (test_plan.modules, 0..) |*planned, planned_index| {
        const cached = planned.cached_results != null;
        const results = if (cached) planned.releaseCachedResults() else fresh: {
            const owned = fresh_results[planned_index].?;
            fresh_results[planned_index] = null;
            break :fresh owned;
        };
        try appendPlannedModuleResult(ctx, module_results, planned, results, cached);

        const summary = summaries[planned_index];
        total.passed += summary.passed;
        total.failed += summary.failed;
        total.compiler_errors += summary.compiler_errors;
        total.modules_with_tests += summary.modules_with_tests;
        total.cached_modules += summary.cached_modules;
    }
}

const watch_debounce_ms = 25;
const watch_file_hash_limit = 256 * 1024 * 1024;
const watch_inputs_file_limit = 64 * 1024 * 1024;
const watch_inputs_magic = "roc-watch-inputs-v1";
const watch_separator = "\n--- roc watch: change detected; rerunning ---\n\n";

const WatchCommand = union(enum) {
    check: cli_args.CheckArgs,
    test_cmd: cli_args.TestArgs,
    build: cli_args.BuildArgs,
};

const WatchPathError = Allocator.Error || std.process.CurrentPathError;
const WatchCollectPathsError = WatchPathError;
const WatchSnapshotError = Allocator.Error;
const WatchCollectInputSetError = WatchCollectPathsError || WatchSnapshotError;
const WatchWriteInputsError = WatchCollectInputSetError || std.Io.Dir.WriteFileError;
const WatchReadInputsError = WatchCollectPathsError || WatchSnapshotError || error{ WatchInputsMissing, WatchInputsReadFailed, WatchInputsMalformed };
const WatchDirectoryError = Allocator.Error;
const WatcherStartError = std.Thread.SpawnError || error{ AlreadyStarted, UnsupportedWatchMode, WatchBackendFailed };
const WatchRefreshError = WatchSnapshotError || WatchDirectoryError || WatcherStartError;
const WatchChangeError = WatchSnapshotError;
const WatchInputsPathError = Allocator.Error || std.Io.Dir.CreateDirPathError;
const WatchSpawnChildError = Allocator.Error || std.process.SpawnError || std.Thread.SpawnError;
const CliOutputWriteError = error{WriteFailed};
const WatchChildOutputError = std.Io.File.MultiReader.UnendingError || std.Io.Timeout.Error || std.process.Child.WaitError;
const WatchCommandError = error{UnsupportedWatchMode} || WatchInputsPathError || WatchSpawnChildError || WatchCollectPathsError || WatchRefreshError || WatchReadInputsError || WatchChangeError || WatchChildOutputError || CliOutputWriteError;
const ReportRenderError = Allocator.Error || CliOutputWriteError;
const CheckFileWithBuildEnvPreservedError = compile.build.InitError || compile.build.BuildError || compile.build.CompileDiscoveredError || compile.build.BuildWithMainError || Allocator.Error || std.Io.Dir.RealPathFileAllocError || error{ ExpectedAppHeader, InvalidPackageName };
const RocTestError = WatchCommandError || compile.build.InitError || compile.build.BuildError || compile.build.CompileDiscoveredError || compile.build.BuildWithMainError || WatchWriteInputsError || ReportRenderError || std.Io.Dir.RealPathFileAllocError || SourceRefResolveError || error{ CompilationFailed, TestsFailed, NoHomeDirectory };
const RocCheckError = WatchCommandError || CheckFileWithBuildEnvPreservedError || WatchWriteInputsError || ReportRenderError || CliError || std.Io.Dir.CreateDirPathError || std.Io.Dir.WriteFileError || SourceRefResolveError || error{CheckFailed};

const WatchEventSignal = struct {
    dirty: std.atomic.Value(bool) = std.atomic.Value(bool).init(false),
};

const WatchFileState = union(enum) {
    hash: [32]u8,
    missing,
    unreadable,

    fn eql(a: WatchFileState, b: WatchFileState) bool {
        return switch (a) {
            .hash => |a_hash| switch (b) {
                .hash => |b_hash| std.mem.eql(u8, &a_hash, &b_hash),
                else => false,
            },
            .missing => b == .missing,
            .unreadable => b == .unreadable,
        };
    }
};

fn watchFileStateFromCompiler(state: compile.watch_inputs.State) WatchFileState {
    return switch (state) {
        .hash => |hash| .{ .hash = hash },
        .missing => .missing,
        .unreadable => .unreadable,
    };
}

const WatchSnapshotEntry = struct {
    state: WatchFileState,
};

const WatchInputSet = struct {
    inputs: []const []const u8,
    snapshot: []WatchSnapshotEntry,

    fn deinit(self: *WatchInputSet, ctx: *CliCtx) void {
        freeOwnedPathSlice(ctx.gpa, self.inputs);
        self.inputs = &.{};
        ctx.gpa.free(self.snapshot);
        self.snapshot = &.{};
    }
};

const WatchState = struct {
    inputs: []const []const u8 = &.{},
    snapshot: []WatchSnapshotEntry = &.{},
    watcher: ?*watch_mod.Watcher = null,

    fn deinit(self: *WatchState, ctx: *CliCtx) void {
        if (self.watcher) |watcher| {
            watcher.deinit();
            self.watcher = null;
        }
        freeOwnedPathSlice(ctx.gpa, self.inputs);
        self.inputs = &.{};
        ctx.gpa.free(self.snapshot);
        self.snapshot = &.{};
    }
};

const WatchChild = struct {
    child: std.process.Child,
    id: std.process.Child.Id,
    thread: std.Thread,
    done: std.atomic.Value(bool) = std.atomic.Value(bool).init(false),
    term: ?std.process.Child.Term = null,
    stdout: []u8 = &.{},
    stderr: []u8 = &.{},
    output_error: ?WatchChildOutputError = null,

    fn waitThread(self: *WatchChild, io: std.Io, gpa: Allocator) void {
        self.captureOutputAndWait(io, gpa) catch |err| {
            self.output_error = err;
            self.child.kill(io);
        };
        self.done.store(true, .seq_cst);
    }

    fn captureOutputAndWait(self: *WatchChild, io: std.Io, gpa: Allocator) WatchChildOutputError!void {
        var multi_reader_buffer: std.Io.File.MultiReader.Buffer(2) = undefined;
        var multi_reader: std.Io.File.MultiReader = undefined;
        multi_reader.init(gpa, io, multi_reader_buffer.toStreams(), &.{ self.child.stdout.?, self.child.stderr.? });
        defer multi_reader.deinit();

        while (multi_reader.fill(64, .none)) |_| {} else |err| switch (err) {
            error.EndOfStream => {},
            else => |e| return e,
        }
        try multi_reader.checkAnyError();

        self.term = try self.child.wait(io);

        self.stdout = try multi_reader.toOwnedSlice(0);
        errdefer {
            gpa.free(self.stdout);
            self.stdout = &.{};
        }

        self.stderr = try multi_reader.toOwnedSlice(1);
    }

    fn deinit(self: *WatchChild, gpa: Allocator) void {
        gpa.free(self.stdout);
        gpa.free(self.stderr);
    }
};

const WatchChildArgv = struct {
    argv: []const []const u8,
    owned: []const []const u8,

    fn deinit(self: *WatchChildArgv, gpa: Allocator) void {
        for (self.owned) |arg| gpa.free(arg);
        gpa.free(self.owned);
        gpa.free(self.argv);
    }
};

fn watchCommandPath(command: WatchCommand) []const u8 {
    return switch (command) {
        .check => |args| args.path,
        .test_cmd => |args| args.path,
        .build => |args| args.path,
    };
}

fn watchCommandMain(command: WatchCommand) ?[]const u8 {
    return switch (command) {
        .check => |args| args.main,
        .test_cmd => |args| args.main,
        .build => null,
    };
}

fn watchCallback(context: ?*anyopaque, _: watch_mod.WatchEvent) void {
    const signal: *WatchEventSignal = @ptrCast(@alignCast(context.?));
    signal.dirty.store(true, .seq_cst);
}

fn freeOwnedPathSlice(gpa: Allocator, paths: []const []const u8) void {
    for (paths) |path| gpa.free(path);
    gpa.free(paths);
}

fn absolutePathFromCwd(ctx: *CliCtx, path: []const u8) WatchPathError![]const u8 {
    if (std.fs.path.isAbsolute(path)) {
        return std.fs.path.resolve(ctx.gpa, &.{path});
    }

    var cwd_buffer: [std.fs.max_path_bytes]u8 = @splat(0);
    const cwd_len = try std.process.currentPath(ctx.io.std_io, &cwd_buffer);
    const cwd = cwd_buffer[0..cwd_len];
    return std.fs.path.resolve(ctx.gpa, &.{ cwd, path });
}

fn appendOwnedWatchPath(
    ctx: *CliCtx,
    paths: *std.ArrayList([]const u8),
    seen: *std.StringHashMapUnmanaged(void),
    path: []const u8,
) WatchCollectPathsError!bool {
    const absolute = try absolutePathFromCwd(ctx, path);
    errdefer ctx.gpa.free(absolute);

    if (seen.contains(absolute)) {
        ctx.gpa.free(absolute);
        return false;
    }

    try paths.append(ctx.gpa, absolute);
    errdefer _ = paths.pop();
    try seen.put(ctx.gpa, absolute, {});

    return true;
}

fn collectWatchPaths(ctx: *CliCtx, build_env: ?*BuildEnv, extra_paths: []const []const u8) WatchCollectPathsError![]const []const u8 {
    var paths = std.ArrayList([]const u8).empty;
    errdefer {
        for (paths.items) |path| ctx.gpa.free(path);
        paths.deinit(ctx.gpa);
    }

    var seen: std.StringHashMapUnmanaged(void) = .{};
    defer seen.deinit(ctx.gpa);

    if (build_env) |env| {
        const inputs = try env.collectWatchInputs();
        defer env.freeWatchInputs(inputs);

        for (inputs) |path| {
            _ = try appendOwnedWatchPath(ctx, &paths, &seen, path);
        }
    }

    for (extra_paths) |path| {
        _ = try appendOwnedWatchPath(ctx, &paths, &seen, path);
    }

    return paths.toOwnedSlice(ctx.gpa);
}

fn captureWatchInputSet(ctx: *CliCtx, inputs: []const []const u8) WatchCollectInputSetError!WatchInputSet {
    errdefer freeOwnedPathSlice(ctx.gpa, inputs);

    const snapshot = try computeWatchSnapshot(ctx, inputs);
    errdefer ctx.gpa.free(snapshot);

    return .{
        .inputs = inputs,
        .snapshot = snapshot,
    };
}

fn appendCompilerWatchInput(
    ctx: *CliCtx,
    paths: *std.ArrayList([]const u8),
    snapshot: *std.ArrayList(WatchSnapshotEntry),
    seen: *std.StringHashMapUnmanaged(void),
    input: compile.watch_inputs.Input,
) WatchCollectInputSetError!void {
    if (try appendOwnedWatchPath(ctx, paths, seen, input.path)) {
        try snapshot.append(ctx.gpa, .{ .state = watchFileStateFromCompiler(input.state) });
    }
}

fn appendCurrentCollectedWatchInput(
    ctx: *CliCtx,
    paths: *std.ArrayList([]const u8),
    snapshot: *std.ArrayList(WatchSnapshotEntry),
    seen: *std.StringHashMapUnmanaged(void),
    path: []const u8,
) WatchCollectInputSetError!void {
    if (try appendOwnedWatchPath(ctx, paths, seen, path)) {
        const absolute_path = paths.items[paths.items.len - 1];
        try snapshot.append(ctx.gpa, .{ .state = try readWatchFileState(ctx, absolute_path) });
    }
}

fn collectWatchInputSetFromCompilerInputs(
    ctx: *CliCtx,
    compiler_inputs: []const compile.watch_inputs.Input,
    extra_paths: []const []const u8,
) WatchCollectInputSetError!WatchInputSet {
    var paths = std.ArrayList([]const u8).empty;
    errdefer {
        for (paths.items) |path| ctx.gpa.free(path);
        paths.deinit(ctx.gpa);
    }

    var snapshot = std.ArrayList(WatchSnapshotEntry).empty;
    errdefer snapshot.deinit(ctx.gpa);

    var seen: std.StringHashMapUnmanaged(void) = .{};
    defer seen.deinit(ctx.gpa);

    for (compiler_inputs) |input| {
        try appendCompilerWatchInput(ctx, &paths, &snapshot, &seen, input);
    }

    for (extra_paths) |path| {
        try appendCurrentCollectedWatchInput(ctx, &paths, &snapshot, &seen, path);
    }

    const owned_paths = try paths.toOwnedSlice(ctx.gpa);
    errdefer freeOwnedPathSlice(ctx.gpa, owned_paths);

    return .{
        .inputs = owned_paths,
        .snapshot = try snapshot.toOwnedSlice(ctx.gpa),
    };
}

fn collectWatchInputSet(ctx: *CliCtx, build_env: ?*BuildEnv, extra_paths: []const []const u8) WatchCollectInputSetError!WatchInputSet {
    if (build_env) |env| {
        const inputs = try env.collectWatchInputStates();
        defer env.freeWatchInputStates(inputs);
        return collectWatchInputSetFromCompilerInputs(ctx, inputs, extra_paths);
    }

    const paths = try collectWatchPaths(ctx, null, extra_paths);
    return captureWatchInputSet(ctx, paths);
}

fn watchPathSeparator(byte: u8) bool {
    return byte == '/' or byte == '\\';
}

fn watchPathIsInsideDirectory(path: []const u8, dir: []const u8) bool {
    if (dir.len == 0) return false;
    if (std.mem.eql(u8, path, dir)) return true;
    if (!std.mem.startsWith(u8, path, dir)) return false;
    if (watchPathSeparator(dir[dir.len - 1])) return true;
    return path.len > dir.len and watchPathSeparator(path[dir.len]);
}

fn collectSyntheticBuildWatchInputSet(
    ctx: *CliCtx,
    build_env: *BuildEnv,
    synthetic_root_path: []const u8,
) WatchCollectInputSetError!WatchInputSet {
    const inputs = try build_env.collectWatchInputStates();
    defer build_env.freeWatchInputStates(inputs);

    var paths = std.ArrayList([]const u8).empty;
    errdefer {
        for (paths.items) |path| ctx.gpa.free(path);
        paths.deinit(ctx.gpa);
    }

    var snapshot = std.ArrayList(WatchSnapshotEntry).empty;
    errdefer snapshot.deinit(ctx.gpa);

    var seen: std.StringHashMapUnmanaged(void) = .{};
    defer seen.deinit(ctx.gpa);

    const synthetic_root_dir = std.fs.path.dirname(synthetic_root_path) orelse ".";
    const synthetic_root_dir_abs = try absolutePathFromCwd(ctx, synthetic_root_dir);
    defer ctx.gpa.free(synthetic_root_dir_abs);

    for (inputs) |input| {
        const input_abs = try absolutePathFromCwd(ctx, input.path);
        defer ctx.gpa.free(input_abs);
        if (watchPathIsInsideDirectory(input_abs, synthetic_root_dir_abs)) continue;
        try appendCompilerWatchInput(ctx, &paths, &snapshot, &seen, input);
    }

    const owned_paths = try paths.toOwnedSlice(ctx.gpa);
    errdefer freeOwnedPathSlice(ctx.gpa, owned_paths);

    return .{
        .inputs = owned_paths,
        .snapshot = try snapshot.toOwnedSlice(ctx.gpa),
    };
}

fn collectHotReloadWatchInputSet(
    ctx: *CliCtx,
    inputs_in: []const compile.watch_inputs.Input,
    source_rewrite: ?HotReloadSourceRewrite,
) WatchCollectInputSetError!WatchInputSet {
    var paths = std.ArrayList([]const u8).empty;
    errdefer {
        for (paths.items) |path| ctx.gpa.free(path);
        paths.deinit(ctx.gpa);
    }

    var seen: std.StringHashMapUnmanaged(void) = .{};
    defer seen.deinit(ctx.gpa);

    var snapshot = std.ArrayList(WatchSnapshotEntry).empty;
    errdefer snapshot.deinit(ctx.gpa);

    const synthetic_abs = if (source_rewrite) |rewrite|
        try absolutePathFromCwd(ctx, rewrite.synthetic_app_path)
    else
        null;
    defer if (synthetic_abs) |path| ctx.gpa.free(path);

    for (inputs_in) |input| {
        if (synthetic_abs) |synthetic_path| {
            const path_abs = try absolutePathFromCwd(ctx, input.path);
            defer ctx.gpa.free(path_abs);
            if (std.mem.eql(u8, path_abs, synthetic_path)) {
                try appendCurrentCollectedWatchInput(ctx, &paths, &snapshot, &seen, source_rewrite.?.source_path);
                continue;
            }
        }
        try appendCompilerWatchInput(ctx, &paths, &snapshot, &seen, input);
    }

    if (source_rewrite) |rewrite| {
        try appendCurrentCollectedWatchInput(ctx, &paths, &snapshot, &seen, rewrite.source_path);
    }

    const owned_paths = try paths.toOwnedSlice(ctx.gpa);
    errdefer freeOwnedPathSlice(ctx.gpa, owned_paths);

    return .{
        .inputs = owned_paths,
        .snapshot = try snapshot.toOwnedSlice(ctx.gpa),
    };
}

fn appendSerializedWatchFileState(gpa: Allocator, bytes: *std.ArrayList(u8), state: WatchFileState) Allocator.Error!void {
    switch (state) {
        .hash => |hash| {
            const hex = std.fmt.bytesToHex(hash, .lower);
            try bytes.append(gpa, 'h');
            try bytes.appendSlice(gpa, hex[0..]);
        },
        .missing => try bytes.append(gpa, 'm'),
        .unreadable => try bytes.append(gpa, 'u'),
    }
}

fn hexNibble(byte: u8) ?u8 {
    return switch (byte) {
        '0'...'9' => byte - '0',
        'a'...'f' => byte - 'a' + 10,
        'A'...'F' => byte - 'A' + 10,
        else => null,
    };
}

fn parseSerializedWatchFileState(serialized: []const u8) WatchReadInputsError!WatchFileState {
    if (serialized.len == 0) return error.WatchInputsMalformed;

    return switch (serialized[0]) {
        'h' => blk: {
            if (serialized.len != 65) return error.WatchInputsMalformed;
            var hash: [32]u8 = undefined;
            for (&hash, 0..) |*byte, i| {
                const hi = hexNibble(serialized[1 + i * 2]) orelse return error.WatchInputsMalformed;
                const lo = hexNibble(serialized[2 + i * 2]) orelse return error.WatchInputsMalformed;
                byte.* = (hi << 4) | lo;
            }
            break :blk .{ .hash = hash };
        },
        'm' => if (serialized.len == 1) .missing else error.WatchInputsMalformed,
        'u' => if (serialized.len == 1) .unreadable else error.WatchInputsMalformed,
        else => error.WatchInputsMalformed,
    };
}

fn appendWatchInputWithState(
    ctx: *CliCtx,
    paths: *std.ArrayList([]const u8),
    snapshot: *std.ArrayList(WatchSnapshotEntry),
    seen: *std.StringHashMapUnmanaged(void),
    path: []const u8,
    state: WatchFileState,
) WatchReadInputsError!void {
    if (path.len == 0) return error.WatchInputsMalformed;

    if (try appendOwnedWatchPath(ctx, paths, seen, path)) {
        try snapshot.append(ctx.gpa, .{ .state = state });
    }
}

fn appendCurrentWatchInput(
    ctx: *CliCtx,
    paths: *std.ArrayList([]const u8),
    snapshot: *std.ArrayList(WatchSnapshotEntry),
    seen: *std.StringHashMapUnmanaged(void),
    path: []const u8,
) WatchReadInputsError!void {
    if (try appendOwnedWatchPath(ctx, paths, seen, path)) {
        const absolute_path = paths.items[paths.items.len - 1];
        try snapshot.append(ctx.gpa, .{ .state = try readWatchFileState(ctx, absolute_path) });
    }
}

fn writeWatchInputSetFile(ctx: *CliCtx, file_path: []const u8, input_set: *const WatchInputSet) WatchWriteInputsError!void {
    var bytes = std.ArrayList(u8).empty;
    defer bytes.deinit(ctx.gpa);

    try bytes.appendSlice(ctx.gpa, watch_inputs_magic);
    try bytes.append(ctx.gpa, 0);

    for (input_set.inputs, input_set.snapshot) |path, snapshot| {
        try bytes.appendSlice(ctx.gpa, path);
        try bytes.append(ctx.gpa, 0);
        try appendSerializedWatchFileState(ctx.gpa, &bytes, snapshot.state);
        try bytes.append(ctx.gpa, 0);
    }

    try std.Io.Dir.cwd().writeFile(ctx.io.std_io, .{ .sub_path = file_path, .data = bytes.items });
}

fn writeWatchInputsFile(ctx: *CliCtx, file_path: []const u8, build_env: ?*BuildEnv, extra_paths: []const []const u8) WatchWriteInputsError!void {
    var input_set = try collectWatchInputSet(ctx, build_env, extra_paths);
    defer input_set.deinit(ctx);
    try writeWatchInputSetFile(ctx, file_path, &input_set);
}

/// For `roc build --watch`, the build child records its discovered source inputs so the
/// parent watch loop knows which files to watch. The root path is always included for
/// ordinary builds. Synthetic-default-platform builds compile through temporary files; the
/// child filters those generated temp files out, and the parent supplies the real root path
/// separately while preserving real discovered inputs such as file imports.
fn writeBuildWatchInputs(ctx: *CliCtx, args: cli_args.BuildArgs, build_env: *BuildEnv) WatchWriteInputsError!void {
    const file_path = args.watch_inputs_file orelse return;
    if (args.synthetic_default_platform) {
        var input_set = try collectSyntheticBuildWatchInputSet(ctx, build_env, args.path);
        defer input_set.deinit(ctx);
        try writeWatchInputSetFile(ctx, file_path, &input_set);
        return;
    }
    try writeWatchInputsFile(ctx, file_path, build_env, &[_][]const u8{args.path});
}

fn reportBuildWatchInputsWriteError(ctx: *CliCtx, file_path: []const u8, err: WatchWriteInputsError) void {
    ctx.io.stderr().print("Error: failed to write watch input state to {s}: {}\n", .{ file_path, err }) catch {};
    ctx.io.flush();
}

fn writeBuildWatchInputsOnExit(ctx: *CliCtx, args: cli_args.BuildArgs, build_env: *BuildEnv) void {
    writeBuildWatchInputs(ctx, args, build_env) catch |err| {
        const file_path = args.watch_inputs_file orelse return;
        reportBuildWatchInputsWriteError(ctx, file_path, err);
    };
}

/// The one warning exit policy: a command that finished its primary job with
/// warnings (and no errors) exits 2. Every verb routes through here so no
/// back half invents its own variant (the PR 9759 shim divergence class).
fn exitOnWarnings(ctx: *CliCtx, warning_count: usize) void {
    if (warning_count == 0) return;
    ctx.io.flush();
    std.process.exit(2);
}

fn exitBuildOnWarningsIfRequested(ctx: *CliCtx, args: cli_args.BuildArgs, build_env: *BuildEnv, total_warning_count: usize) void {
    if (!args.exit_on_warnings) return;
    if (total_warning_count > 0) writeBuildWatchInputsOnExit(ctx, args, build_env);
    exitOnWarnings(ctx, total_warning_count);
}

fn writeHotReloadWatchPathsFile(
    ctx: *CliCtx,
    file_path: []const u8,
    inputs: []const compile.watch_inputs.Input,
    source_rewrite: ?HotReloadSourceRewrite,
) WatchWriteInputsError!void {
    var input_set = try collectHotReloadWatchInputSet(ctx, inputs, source_rewrite);
    defer input_set.deinit(ctx);
    try writeWatchInputSetFile(ctx, file_path, &input_set);
}

fn readWatchInputsFile(ctx: *CliCtx, file_path: []const u8, extra_paths: []const []const u8) WatchReadInputsError!WatchInputSet {
    const bytes = std.Io.Dir.cwd().readFileAlloc(ctx.io.std_io, file_path, ctx.gpa, .limited(watch_inputs_file_limit)) catch |err| switch (err) {
        error.FileNotFound => return error.WatchInputsMissing,
        error.OutOfMemory => return error.OutOfMemory,
        else => return error.WatchInputsReadFailed,
    };
    defer ctx.gpa.free(bytes);

    var paths = std.ArrayList([]const u8).empty;
    errdefer {
        for (paths.items) |path| ctx.gpa.free(path);
        paths.deinit(ctx.gpa);
    }

    var snapshot = std.ArrayList(WatchSnapshotEntry).empty;
    errdefer snapshot.deinit(ctx.gpa);

    var seen: std.StringHashMapUnmanaged(void) = .{};
    defer seen.deinit(ctx.gpa);

    if (!std.mem.startsWith(u8, bytes, watch_inputs_magic)) return error.WatchInputsMalformed;
    var offset: usize = watch_inputs_magic.len;
    if (offset >= bytes.len or bytes[offset] != 0) return error.WatchInputsMalformed;
    offset += 1;

    while (offset < bytes.len) {
        const path_end = std.mem.findScalarPos(u8, bytes, offset, 0) orelse return error.WatchInputsMalformed;
        const path = bytes[offset..path_end];
        offset = path_end + 1;

        const state_end = std.mem.findScalarPos(u8, bytes, offset, 0) orelse return error.WatchInputsMalformed;
        const serialized_state = bytes[offset..state_end];
        offset = state_end + 1;

        try appendWatchInputWithState(
            ctx,
            &paths,
            &snapshot,
            &seen,
            path,
            try parseSerializedWatchFileState(serialized_state),
        );
    }

    for (extra_paths) |path| {
        try appendCurrentWatchInput(ctx, &paths, &snapshot, &seen, path);
    }

    const owned_paths = try paths.toOwnedSlice(ctx.gpa);
    errdefer freeOwnedPathSlice(ctx.gpa, owned_paths);

    return .{
        .inputs = owned_paths,
        .snapshot = try snapshot.toOwnedSlice(ctx.gpa),
    };
}

fn readWatchInputsFileAfterChild(ctx: *CliCtx, file_path: []const u8, extra_paths: []const []const u8) WatchCommandError!WatchInputSet {
    return readWatchInputsFile(ctx, file_path, extra_paths) catch |err| {
        switch (err) {
            error.OutOfMemory => {},
            error.WatchInputsMissing => try ctx.io.stderr().print("Error: watch child did not write source input state to {s}.\n", .{file_path}),
            error.WatchInputsReadFailed => try ctx.io.stderr().print("Error: failed to read watch input state from {s}.\n", .{file_path}),
            error.WatchInputsMalformed => try ctx.io.stderr().print("Error: watch child wrote malformed source input state to {s}.\n", .{file_path}),
            error.Unexpected,
            error.Canceled,
            error.NameTooLong,
            error.CurrentDirUnlinked,
            => try ctx.io.stderr().print("Error: failed to resolve an explicit watch path while reading source input state from {s}: {}\n", .{ file_path, err }),
        }
        ctx.io.flush();
        return err;
    };
}

fn readWatchFileState(ctx: *CliCtx, path: []const u8) WatchSnapshotError!WatchFileState {
    const bytes = std.Io.Dir.cwd().readFileAlloc(ctx.io.std_io, path, ctx.gpa, .limited(watch_file_hash_limit)) catch |err| switch (err) {
        error.FileNotFound => return .missing,
        error.OutOfMemory => return error.OutOfMemory,
        else => return .unreadable,
    };
    defer ctx.gpa.free(bytes);

    var hasher = std.crypto.hash.sha2.Sha256.init(.{});
    hasher.update(bytes);
    return .{ .hash = hasher.finalResult() };
}

fn computeWatchSnapshot(ctx: *CliCtx, paths: []const []const u8) WatchSnapshotError![]WatchSnapshotEntry {
    const snapshot = try ctx.gpa.alloc(WatchSnapshotEntry, paths.len);
    errdefer ctx.gpa.free(snapshot);

    for (paths, 0..) |path, i| {
        snapshot[i] = .{ .state = try readWatchFileState(ctx, path) };
    }

    return snapshot;
}

fn watchSnapshotChanged(a: []const WatchSnapshotEntry, b: []const WatchSnapshotEntry) bool {
    if (a.len != b.len) return true;
    for (a, b) |old, new| {
        if (!old.state.eql(new.state)) return true;
    }
    return false;
}

fn existingDirectory(ctx: *CliCtx, path: []const u8) bool {
    var dir = std.Io.Dir.openDirAbsolute(ctx.io.std_io, path, .{}) catch return false;
    dir.close(ctx.io.std_io);
    return true;
}

fn nearestExistingAncestor(ctx: *CliCtx, path: []const u8) WatchDirectoryError![]const u8 {
    var candidate = try ctx.gpa.dupe(u8, std.fs.path.dirname(path) orelse path);
    errdefer ctx.gpa.free(candidate);

    while (!existingDirectory(ctx, candidate)) {
        const parent = std.fs.path.dirname(candidate) orelse break;
        if (parent.len == candidate.len) break;

        const parent_copy = try ctx.gpa.dupe(u8, parent);
        ctx.gpa.free(candidate);
        candidate = parent_copy;
    }

    return candidate;
}

fn collectWatchDirectories(ctx: *CliCtx, paths: []const []const u8) WatchDirectoryError![]const []const u8 {
    var dirs = std.ArrayList([]const u8).empty;
    errdefer {
        for (dirs.items) |dir| ctx.gpa.free(dir);
        dirs.deinit(ctx.gpa);
    }

    var seen: std.StringHashMapUnmanaged(void) = .{};
    defer seen.deinit(ctx.gpa);

    for (paths) |path| {
        const dir = try nearestExistingAncestor(ctx, path);
        errdefer ctx.gpa.free(dir);

        if (seen.contains(dir)) {
            ctx.gpa.free(dir);
            continue;
        }

        try seen.put(ctx.gpa, dir, {});
        try dirs.append(ctx.gpa, dir);
    }

    return dirs.toOwnedSlice(ctx.gpa);
}

fn refreshWatchState(
    ctx: *CliCtx,
    state: *WatchState,
    signal: *WatchEventSignal,
    new_input_set: WatchInputSet,
) WatchRefreshError!bool {
    var owned_input_set = new_input_set;
    errdefer owned_input_set.deinit(ctx);

    const watch_dirs = try collectWatchDirectories(ctx, owned_input_set.inputs);
    defer freeOwnedPathSlice(ctx.gpa, watch_dirs);

    var new_watcher: ?*watch_mod.Watcher = null;
    if (watch_dirs.len > 0) {
        new_watcher = try watch_mod.Watcher.initAllFiles(ctx.gpa, ctx.io.std_io, watch_dirs, signal, watchCallback);
        errdefer if (new_watcher) |watcher| watcher.deinit();
        new_watcher.?.start() catch |err| switch (err) {
            error.WatchBackendFailed => {
                ctx.io.stderr().writeAll("Error: failed to start filesystem watching for source inputs.\n") catch {};
                ctx.io.flush();
                return err;
            },
            else => return err,
        };
    }

    const current_snapshot = try computeWatchSnapshot(ctx, owned_input_set.inputs);
    errdefer ctx.gpa.free(current_snapshot);

    const changed_during_refresh = watchSnapshotChanged(owned_input_set.snapshot, current_snapshot);
    ctx.gpa.free(owned_input_set.snapshot);
    owned_input_set.snapshot = &.{};

    state.deinit(ctx);
    state.inputs = owned_input_set.inputs;
    owned_input_set.inputs = &.{};
    state.snapshot = current_snapshot;
    state.watcher = new_watcher;
    new_watcher = null;

    return changed_during_refresh;
}

fn watchStateHasByteChanges(ctx: *CliCtx, state: *WatchState) WatchChangeError!bool {
    if (state.inputs.len == 0) return true;
    const current = try computeWatchSnapshot(ctx, state.inputs);
    defer ctx.gpa.free(current);
    return watchSnapshotChanged(state.snapshot, current);
}

fn consumeDebouncedWatchChange(ctx: *CliCtx, signal: *WatchEventSignal, state: *WatchState) WatchChangeError!bool {
    if (!signal.dirty.swap(false, .seq_cst)) return false;
    std.Io.sleep(ctx.io.std_io, std.Io.Duration.fromMilliseconds(watch_debounce_ms), .awake) catch {};
    _ = signal.dirty.swap(false, .seq_cst);
    return try watchStateHasByteChanges(ctx, state);
}

fn waitForWatchChange(ctx: *CliCtx, signal: *WatchEventSignal, state: *WatchState) WatchChangeError!void {
    while (true) {
        if (try consumeDebouncedWatchChange(ctx, signal, state)) return;
        std.Io.sleep(ctx.io.std_io, std.Io.Duration.fromMilliseconds(watch_debounce_ms), .awake) catch {};
    }
}

fn createWatchInputsPath(ctx: *CliCtx) WatchInputsPathError![]const u8 {
    try std.Io.Dir.cwd().createDirPath(ctx.io.std_io, ".zig-cache");

    var random: [16]u8 = undefined;
    ctx.io.std_io.random(&random);
    const hex = std.fmt.bytesToHex(random, .lower);

    return std.fmt.allocPrint(ctx.gpa, ".zig-cache/roc-watch-{s}.inputs", .{hex[0..]});
}

fn watchExtraPaths(command: WatchCommand) [2]?[]const u8 {
    return .{ watchCommandPath(command), watchCommandMain(command) };
}

fn appendExtraWatchPaths(command: WatchCommand, buffer: *[2][]const u8) []const []const u8 {
    var len: usize = 0;
    const extras = watchExtraPaths(command);
    for (extras) |path| {
        if (path) |p| {
            buffer[len] = p;
            len += 1;
        }
    }
    return buffer[0..len];
}

fn appendOwnedArg(
    gpa: Allocator,
    argv: *std.ArrayList([]const u8),
    owned: *std.ArrayList([]const u8),
    comptime fmt_str: []const u8,
    args: anytype,
) Allocator.Error!void {
    const arg = try std.fmt.allocPrint(gpa, fmt_str, args);
    errdefer gpa.free(arg);
    try argv.append(gpa, arg);
    try owned.append(gpa, arg);
}

fn appendResolveLimitArgs(
    gpa: Allocator,
    argv: *std.ArrayList([]const u8),
    owned: *std.ArrayList([]const u8),
    limits: cli_args.ResolveLimitArgs,
) Allocator.Error!void {
    if (limits.max_package_mb) |limit| {
        try appendOwnedArg(gpa, argv, owned, "--max-package-mb={}", .{limit});
    }
    if (limits.max_transitive_mb) |limit| {
        try appendOwnedArg(gpa, argv, owned, "--max-transitive-mb={}", .{limit});
    }
}

fn buildWatchChildArgv(ctx: *CliCtx, arg0: []const u8, command: WatchCommand, inputs_path: []const u8) Allocator.Error!WatchChildArgv {
    var argv = std.ArrayList([]const u8).empty;
    errdefer argv.deinit(ctx.gpa);
    var owned = std.ArrayList([]const u8).empty;
    errdefer {
        for (owned.items) |arg| ctx.gpa.free(arg);
        owned.deinit(ctx.gpa);
    }

    try argv.append(ctx.gpa, arg0);

    switch (command) {
        .check => |args| {
            try argv.append(ctx.gpa, "check");
            if (args.main) |main_path| try appendOwnedArg(ctx.gpa, &argv, &owned, "--main={s}", .{main_path});
            if (args.time) try argv.append(ctx.gpa, "--time");
            if (args.timings) try argv.append(ctx.gpa, "--timings");
            if (args.no_cache) try argv.append(ctx.gpa, "--no-cache");
            if (args.verbose) try argv.append(ctx.gpa, "--verbose");
            if (args.max_threads) |jobs| try appendOwnedArg(ctx.gpa, &argv, &owned, "--jobs={}", .{jobs});
            try appendResolveLimitArgs(ctx.gpa, &argv, &owned, args.resolve_limits);
            try appendOwnedArg(ctx.gpa, &argv, &owned, "--watch-inputs-file={s}", .{inputs_path});
            try argv.append(ctx.gpa, args.path);
        },
        .test_cmd => |args| {
            try argv.append(ctx.gpa, "test");
            try appendOwnedArg(ctx.gpa, &argv, &owned, "--opt={s}", .{@tagName(args.opt)});
            if (args.main) |main_path| try appendOwnedArg(ctx.gpa, &argv, &owned, "--main={s}", .{main_path});
            if (args.no_cache) try argv.append(ctx.gpa, "--no-cache");
            if (args.verbose) try argv.append(ctx.gpa, "--verbose");
            if (args.max_threads) |jobs| try appendOwnedArg(ctx.gpa, &argv, &owned, "--jobs={}", .{jobs});
            try appendResolveLimitArgs(ctx.gpa, &argv, &owned, args.resolve_limits);
            try appendOwnedArg(ctx.gpa, &argv, &owned, "--watch-inputs-file={s}", .{inputs_path});
            try argv.append(ctx.gpa, args.path);
        },
        .build => |args| {
            try argv.append(ctx.gpa, "build");
            try appendOwnedArg(ctx.gpa, &argv, &owned, "--opt={s}", .{@tagName(args.opt)});
            if (args.target) |target| try appendOwnedArg(ctx.gpa, &argv, &owned, "--target={s}", .{target});
            if (args.output) |output| try appendOwnedArg(ctx.gpa, &argv, &owned, "--output={s}", .{output});
            if (args.debug) try argv.append(ctx.gpa, "--debug");
            if (args.allow_errors) try argv.append(ctx.gpa, "--allow-errors");
            if (args.verbose) try argv.append(ctx.gpa, "--verbose");
            if (args.timings) try argv.append(ctx.gpa, "--timings");
            if (args.no_cache) try argv.append(ctx.gpa, "--no-cache");
            if (args.max_threads) |jobs| try appendOwnedArg(ctx.gpa, &argv, &owned, "--jobs={}", .{jobs});
            if (args.wasm_memory) |bytes| try appendOwnedArg(ctx.gpa, &argv, &owned, "--wasm-memory={}", .{bytes});
            if (args.wasm_stack_size) |bytes| try appendOwnedArg(ctx.gpa, &argv, &owned, "--wasm-stack-size={}", .{bytes});
            try appendResolveLimitArgs(ctx.gpa, &argv, &owned, args.resolve_limits);
            try appendOwnedArg(ctx.gpa, &argv, &owned, "--watch-inputs-file={s}", .{inputs_path});
            try argv.append(ctx.gpa, args.path);
        },
    }

    return .{
        .argv = try argv.toOwnedSlice(ctx.gpa),
        .owned = try owned.toOwnedSlice(ctx.gpa),
    };
}

fn spawnWatchChild(ctx: *CliCtx, argv: []const []const u8) WatchSpawnChildError!*WatchChild {
    var child = try std.process.spawn(ctx.io.std_io, .{
        .argv = argv,
        .stdin = .inherit,
        .stdout = .pipe,
        .stderr = .pipe,
    });

    const child_id = child.id.?;
    const watched = ctx.gpa.create(WatchChild) catch |err| {
        child.kill(ctx.io.std_io);
        return err;
    };

    watched.* = .{
        .child = child,
        .id = child_id,
        .thread = undefined,
    };

    watched.thread = std.Thread.spawn(.{}, WatchChild.waitThread, .{ watched, ctx.io.std_io, ctx.gpa }) catch |err| {
        watched.child.kill(ctx.io.std_io);
        ctx.gpa.destroy(watched);
        return err;
    };

    return watched;
}

fn terminateWatchChild(child: *WatchChild) void {
    switch (builtin.os.tag) {
        .windows => {
            _ = std.os.windows.ntdll.NtTerminateProcess(child.id, @enumFromInt(1));
        },
        .wasi => {},
        else => {
            std.posix.kill(child.id, .KILL) catch {};
        },
    }
}

fn joinWatchChild(child: *WatchChild) void {
    child.thread.join();
}

fn destroyWatchChild(ctx: *CliCtx, child: *WatchChild) void {
    child.deinit(ctx.gpa);
    ctx.gpa.destroy(child);
}

fn replayWatchChildOutput(ctx: *CliCtx, child: *WatchChild, print_separator: bool) CliOutputWriteError!void {
    const stdout = ctx.io.stdout();
    const stderr = ctx.io.stderr();

    if (print_separator) try stderr.writeAll(watch_separator);
    if (child.stdout.len > 0) try stdout.writeAll(child.stdout);
    if (child.stderr.len > 0) try stderr.writeAll(child.stderr);
    ctx.io.flush();
}

fn runWatchCommand(ctx: *CliCtx, arg0: []const u8, command: WatchCommand) WatchCommandError!void {
    if (comptime builtin.target.cpu.arch == .wasm32) {
        ctx.io.stderr().writeAll("Error: --watch is not supported in WebAssembly builds of the Roc CLI.\n") catch {};
        return error.UnsupportedWatchMode;
    }

    var signal = WatchEventSignal{};
    var state = WatchState{};
    defer state.deinit(ctx);

    var extra_buf: [2][]const u8 = undefined;
    const extra_paths = appendExtraWatchPaths(command, &extra_buf);

    var first_run = true;
    while (true) {
        const inputs_path = try createWatchInputsPath(ctx);
        defer ctx.gpa.free(inputs_path);
        defer std.Io.Dir.cwd().deleteFile(ctx.io.std_io, inputs_path) catch {};

        var child_argv = try buildWatchChildArgv(ctx, arg0, command, inputs_path);
        defer child_argv.deinit(ctx.gpa);

        {
            const child = try spawnWatchChild(ctx, child_argv.argv);
            defer destroyWatchChild(ctx, child);

            var restart_now = false;
            if (state.inputs.len == 0) {
                const initial_inputs = try collectWatchInputSet(ctx, null, extra_paths);
                if (try refreshWatchState(ctx, &state, &signal, initial_inputs)) {
                    terminateWatchChild(child);
                    restart_now = true;
                }
            }

            if (!restart_now) {
                while (!child.done.load(.seq_cst)) {
                    if (try consumeDebouncedWatchChange(ctx, &signal, &state)) {
                        terminateWatchChild(child);
                        restart_now = true;
                        break;
                    }
                    std.Io.sleep(ctx.io.std_io, std.Io.Duration.fromMilliseconds(watch_debounce_ms), .awake) catch {};
                }
            }

            joinWatchChild(child);

            if (restart_now) {
                first_run = false;
                continue;
            }

            if (child.output_error) |err| return err;
            try replayWatchChildOutput(ctx, child, !first_run);
        }

        const new_inputs = try readWatchInputsFileAfterChild(ctx, inputs_path, extra_paths);
        const changed_during_refresh = try refreshWatchState(ctx, &state, &signal, new_inputs);

        first_run = false;
        if (changed_during_refresh) continue;
        try waitForWatchChange(ctx, &signal, &state);
    }
}

fn rocTest(ctx: *CliCtx, args_in: cli_args.TestArgs, arg0: []const u8) RocTestError!void {
    const trace = tracy.trace(@src());
    defer trace.end();

    var args = args_in;
    const resolved_source = try resolveSourceArg(ctx, args_in.path, args_in.watch);
    args.path = resolved_source.path;
    args.root_source_url = resolved_source.url;
    if (args_in.main) |main_path| {
        const resolved_main = try resolveSourceArg(ctx, main_path, args_in.watch);
        args.main = resolved_main.path;
        args.main_source_url = resolved_main.url;
    }

    if (args.watch) {
        return runWatchCommand(ctx, arg0, .{ .test_cmd = args });
    }

    // Start timing
    const start_time = std.Io.Timestamp.now(ctx.io.std_io, .real).nanoseconds;

    const stdout = ctx.io.stdout();
    const stderr = ctx.io.stderr();

    // --- Normal compilation path ---

    var build_env = try initCliBuildEnv(ctx, .{
        .max_threads = args.max_threads,
        .no_cache = args.no_cache,
        .verbose_cache = args.verbose,
        .resolution_config = resolutionConfigFromLimits(args.resolve_limits),
        .track_watch_inputs = args.watch_inputs_file != null,
        .root_source_url = args.root_source_url,
        .main_source_url = args.main_source_url,
    });
    defer build_env.deinit();

    var extra_buf: [2][]const u8 = undefined;
    const extra_paths = appendExtraWatchPaths(.{ .test_cmd = args }, &extra_buf);

    if (args.main) |main_path| {
        build_env.buildWithMain(args.path, main_path) catch |err| {
            _ = try build_env.renderDiagnostics(stderr);
            if (args.watch_inputs_file) |file_path| {
                try writeWatchInputsFile(ctx, file_path, &build_env, extra_paths);
            }
            return err;
        };
    } else {
        build_env.discoverDependencies(args.path) catch |err| {
            _ = try build_env.renderDiagnostics(stderr);
            if (args.watch_inputs_file) |file_path| {
                try writeWatchInputsFile(ctx, file_path, &build_env, extra_paths);
            }
            return err;
        };
        build_env.compileDiscovered() catch |err| {
            _ = try build_env.renderDiagnostics(stderr);
            if (args.watch_inputs_file) |file_path| {
                try writeWatchInputsFile(ctx, file_path, &build_env, extra_paths);
            }
            return err;
        };
    }

    const diag = try build_env.renderDiagnostics(stderr);
    if (args.watch_inputs_file) |file_path| {
        try writeWatchInputsFile(ctx, file_path, &build_env, extra_paths);
    }
    if (diag.errors > 0) return error.CompilationFailed;

    const modules = try build_env.getCompiledModules(ctx.gpa);
    defer ctx.gpa.free(modules);

    const report_config = try testReportingConfig(ctx);

    var module_results = std.ArrayList(CliModuleTestResult).empty;
    defer {
        for (module_results.items) |module_result| {
            deinitCliTestResultItems(ctx.gpa, module_result.results);
        }
        module_results.deinit(ctx.gpa);
    }

    var test_plan = try buildCliTestPlan(ctx, modules);
    defer test_plan.deinit(ctx.gpa);

    for (test_plan.modules) |*planned| {
        if (try loadCachedCliTestResults(
            ctx,
            build_env.cache_manager,
            planned.artifact,
            planned.module,
            planned.test_roots,
        )) |cached| {
            planned.cached_results = cached.results;
            planned.cached_summary = cached.summary;
        }
    }

    var total = CliTestRunSummary{};
    const test_mode = cliTestExecutionMode(args.opt);
    const use_live_optimized_output = switch (test_mode) {
        .llvm_size, .llvm_speed => true,
        .interpreter, .dev => false,
    };

    var live_coordinator: ?CliTestTranscriptCoordinator = null;
    defer if (live_coordinator) |*coordinator| coordinator.deinit();
    var live_output: ?CliOptimizedLiveTestOutput = null;
    defer if (live_output) |*output| output.deinit();

    if (use_live_optimized_output) {
        const spill_temp_dir = createUniqueTempDir(ctx) catch |err| switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            else => return error.WriteFailed,
        };
        live_coordinator = try CliTestTranscriptCoordinator.init(
            ctx.gpa,
            ctx.io.std_io,
            stdout,
            stderr,
            args.verbose,
            report_config,
            .{
                .entry_count = test_plan.entries.len,
                .spill_dir = spill_temp_dir,
                .delete_spill_dir_on_deinit = true,
            },
        );
        live_output = try CliOptimizedLiveTestOutput.init(ctx, &live_coordinator.?, test_plan.entries.len);
    }

    switch (test_mode) {
        .llvm_size, .llvm_speed => try runOptimizedTestPlan(
            ctx,
            &build_env,
            &test_plan,
            args.opt,
            args.max_threads,
            build_env.cache_manager,
            &module_results,
            &total,
            if (live_output) |*output| output else null,
        ),
        .interpreter, .dev => {
            for (test_plan.modules) |*planned| {
                const summary = if (planned.cached_results != null) cached: {
                    const results = planned.releaseCachedResults();
                    try appendPlannedModuleResult(ctx, &module_results, planned, results, true);
                    break :cached planned.cached_summary;
                } else try runCheckedArtifactTests(
                    ctx,
                    &build_env,
                    planned,
                    test_plan.entries,
                    args.opt,
                    build_env.cache_manager,
                    &module_results,
                );
                total.passed += summary.passed;
                total.failed += summary.failed;
                total.compiler_errors += summary.compiler_errors;
                total.modules_with_tests += summary.modules_with_tests;
                total.cached_modules += summary.cached_modules;
            }
        },
    }

    // Calculate elapsed time
    const end_time = std.Io.Timestamp.now(ctx.io.std_io, .real).nanoseconds;
    const elapsed_ns = @as(u64, @intCast(end_time - start_time));
    const elapsed_ms = @as(f64, @floatFromInt(elapsed_ns)) / 1_000_000.0;
    const cached_suffix = if (total.modules_with_tests > 0 and total.cached_modules == total.modules_with_tests)
        " (cached)"
    else
        "";

    // Render the per-module bodies once into in-memory buffers so we can
    // print them after the summary line.
    var stdout_body = std.Io.Writer.Allocating.init(ctx.gpa);
    defer stdout_body.deinit();
    var stderr_body = std.Io.Writer.Allocating.init(ctx.gpa);
    defer stderr_body.deinit();

    if (!use_live_optimized_output) {
        try renderTestResultBodies(
            ctx.gpa,
            &stdout_body.writer,
            &stderr_body.writer,
            module_results.items,
            args.verbose,
            report_config,
        );
    }

    // Report results
    if (total.failed == 0 and total.compiler_errors == 0) {
        try stdout.writeAll(stdout_body.written());
        try stderr.writeAll(stderr_body.written());
        try stdout.print("All ({}) tests passed in {d:.1} ms.{s}\n", .{ total.passed, elapsed_ms, cached_suffix });
        // Same warning exit policy as check/build/run: passing tests with
        // compile warnings exit 2.
        exitOnWarnings(ctx, diag.warnings);
        return;
    }

    const total_tests = total.passed + total.failed + total.compiler_errors;
    try stdout.writeAll(stdout_body.written());
    try stderr.writeAll(stderr_body.written());
    try stderr.print("Ran {} tests{s} in {d:.1}ms:\n    " ++ ansi_term.green ++ "{}" ++ ansi_term.reset ++ " passed\n    " ++ ansi_term.red ++ "{}" ++ ansi_term.reset ++ " failed\n    " ++ ansi_term.yellow ++ "{}" ++ ansi_term.reset ++ " compiler errors\n", .{ total_tests, cached_suffix, elapsed_ms, total.passed, total.failed, total.compiler_errors });

    return error.TestsFailed;
}

fn testReportingConfig(ctx: *CliCtx) Allocator.Error!reporting.ReportingConfig {
    var config = ctx.terminalReportConfig();
    config.is_tty = std.Io.File.stderr().isTty(ctx.io.std_io) catch false;
    // terminalReportConfig() forces color_preference=.always, which makes
    // shouldUseColors() ignore is_tty entirely. Fall back to .auto so color
    // follows the real TTY status: redirected output (pipes/files) stays
    // uncolored while an attached terminal still gets color. The env vars
    // below override this either way.
    config.color_preference = .auto;

    if (try envVarNonEmpty(ctx.gpa, "NO_COLOR")) {
        config.color_preference = .never;
    } else if (try envVarEquals(ctx.gpa, "ROC_HIGH_CONTRAST", "1")) {
        config.color_preference = .high_contrast;
    } else if (try envVarNonEmpty(ctx.gpa, "FORCE_COLOR")) {
        config.color_preference = .always;
    }

    return config;
}

fn renderCliTestTranscriptEvents(
    stdout_body: *std.Io.Writer,
    stderr_body: *std.Io.Writer,
    events: []const CliTestTranscriptEvent,
) ReportRenderError!void {
    for (events) |event| {
        try renderCliTestTranscriptEvent(stdout_body, stderr_body, event);
    }
}

fn renderCliTestTranscriptEvent(
    stdout_body: *std.Io.Writer,
    stderr_body: *std.Io.Writer,
    event: CliTestTranscriptEvent,
) ReportRenderError!void {
    const writer = switch (event.stream) {
        .stdout => stdout_body,
        .stderr => stderr_body,
    };
    switch (event.kind) {
        .dbg => try writer.print("[dbg] {s}\n", .{event.payload}),
        .expect_failed => try writer.print("Expect failed: {s}\n", .{event.payload}),
        .crashed => {},
        .crash_diagnostic => try writer.print("Roc application crashed with this message:\n\n\t{s}\n\n", .{event.payload}),
    }
}

fn cliTestTranscriptEventEncodedSize(event: CliTestTranscriptEvent) usize {
    return 1 + 1 + 4 + event.payload.len;
}

fn appendCliTestTranscriptEventRecord(
    bytes: *std.ArrayList(u8),
    allocator: Allocator,
    event: CliTestTranscriptEvent,
) Allocator.Error!void {
    try bytes.append(allocator, @intFromEnum(event.kind));
    try bytes.append(allocator, @intFromEnum(event.stream));
    try appendU32(bytes, allocator, @intCast(event.payload.len));
    try bytes.appendSlice(allocator, event.payload);
}

fn renderCliTestTranscriptEventRecords(
    stdout_body: *std.Io.Writer,
    stderr_body: *std.Io.Writer,
    bytes: []const u8,
) ReportRenderError!usize {
    var offset: usize = 0;
    var count: usize = 0;
    while (offset < bytes.len) {
        const kind_raw = readU8(bytes, &offset) orelse return error.WriteFailed;
        const kind: CliTestTranscriptEventKind = switch (kind_raw) {
            0 => .dbg,
            1 => .expect_failed,
            2 => .crashed,
            3 => .crash_diagnostic,
            else => return error.WriteFailed,
        };
        const stream_raw = readU8(bytes, &offset) orelse return error.WriteFailed;
        const stream: CliTestTranscriptStream = switch (stream_raw) {
            0 => .stdout,
            1 => .stderr,
            else => return error.WriteFailed,
        };
        const payload_len = readU32(bytes, &offset) orelse return error.WriteFailed;
        const payload_len_usize: usize = @intCast(payload_len);
        if (offset + payload_len_usize > bytes.len) return error.WriteFailed;
        const payload = bytes[offset..][0..payload_len_usize];
        offset += payload_len_usize;

        try renderCliTestTranscriptEvent(stdout_body, stderr_body, .{
            .stream = stream,
            .kind = kind,
            .payload = payload,
        });
        count += 1;
    }
    return count;
}

const CliTestRenderEntry = struct {
    env: *const ModuleEnv,
    path: []const u8,
    result: *const CliTestResultItem,
};

const cli_test_transcript_spill_threshold_bytes: usize = 1024 * 1024;

const CliTestTranscriptCoordinatorOptions = struct {
    entry_count: usize,
    spill_dir: ?[]const u8 = null,
    delete_spill_dir_on_deinit: bool = false,
    spill_threshold_bytes: usize = cli_test_transcript_spill_threshold_bytes,
};

const CliTestBufferedTranscript = struct {
    events: std.ArrayList(CliTestTranscriptEvent) = .empty,
    memory_bytes: usize = 0,
    spill_path: ?[]u8 = null,

    fn deinit(self: *CliTestBufferedTranscript, allocator: Allocator, io: std.Io) void {
        deinitCliTestTranscriptEventPayloads(allocator, self.events.items);
        self.events.deinit(allocator);
        if (self.spill_path) |path| {
            std.Io.Dir.cwd().deleteFile(io, path) catch {};
            allocator.free(path);
        }
    }

    fn shouldSpill(
        self: *const CliTestBufferedTranscript,
        event_size: usize,
        spill_dir: ?[]const u8,
        threshold: usize,
    ) bool {
        if (spill_dir == null) return false;
        if (self.spill_path != null) return true;
        if (event_size > threshold) return true;
        return self.memory_bytes > threshold - event_size;
    }

    fn appendInMemory(
        self: *CliTestBufferedTranscript,
        allocator: Allocator,
        event: CliTestTranscriptEvent,
        event_size: usize,
    ) Allocator.Error!void {
        const payload = try allocator.dupe(u8, event.payload);
        errdefer allocator.free(payload);
        try self.events.append(allocator, .{
            .stream = event.stream,
            .kind = event.kind,
            .payload = payload,
        });
        self.memory_bytes += event_size;
    }

    fn createSpillPath(
        allocator: Allocator,
        spill_dir: []const u8,
        result_index: usize,
    ) Allocator.Error![]u8 {
        const filename = try std.fmt.allocPrint(allocator, "test-transcript-{d}.bin", .{result_index});
        defer allocator.free(filename);
        return try std.fs.path.join(allocator, &.{ spill_dir, filename });
    }

    fn writeSpillBytes(io: std.Io, path: []const u8, bytes: []const u8) CliOutputWriteError!void {
        std.Io.Dir.cwd().writeFile(io, .{ .sub_path = path, .data = bytes }) catch return error.WriteFailed;
    }

    fn appendSpillRecord(
        allocator: Allocator,
        io: std.Io,
        path: []const u8,
        event: CliTestTranscriptEvent,
    ) ReportRenderError!void {
        var record = std.ArrayList(u8).empty;
        defer record.deinit(allocator);
        try appendCliTestTranscriptEventRecord(&record, allocator, event);

        var file = std.Io.Dir.cwd().openFile(io, path, .{ .mode = .read_write, .allow_directory = false }) catch return error.WriteFailed;
        defer file.close(io);
        const stat = file.stat(io) catch return error.WriteFailed;
        var writer = file.writer(io, &.{});
        writer.seekTo(stat.size) catch return error.WriteFailed;
        writer.interface.writeAll(record.items) catch return error.WriteFailed;
        writer.flush() catch return error.WriteFailed;
    }

    fn spillExistingEvents(
        self: *CliTestBufferedTranscript,
        allocator: Allocator,
        io: std.Io,
        spill_dir: []const u8,
        result_index: usize,
    ) ReportRenderError!void {
        if (self.spill_path != null) return;

        const path = try createSpillPath(allocator, spill_dir, result_index);
        errdefer allocator.free(path);

        var bytes = std.ArrayList(u8).empty;
        defer bytes.deinit(allocator);
        for (self.events.items) |event| {
            try appendCliTestTranscriptEventRecord(&bytes, allocator, event);
        }

        try writeSpillBytes(io, path, bytes.items);
        deinitCliTestTranscriptEventPayloads(allocator, self.events.items);
        self.events.clearRetainingCapacity();
        self.memory_bytes = 0;
        self.spill_path = path;
    }

    fn append(
        self: *CliTestBufferedTranscript,
        allocator: Allocator,
        io: std.Io,
        result_index: usize,
        event: CliTestTranscriptEvent,
        spill_dir: ?[]const u8,
        spill_threshold_bytes: usize,
    ) ReportRenderError!void {
        const event_size = cliTestTranscriptEventEncodedSize(event);
        if (!self.shouldSpill(event_size, spill_dir, spill_threshold_bytes)) {
            try self.appendInMemory(allocator, event, event_size);
            return;
        }

        const dir = spill_dir orelse {
            try self.appendInMemory(allocator, event, event_size);
            return;
        };
        try self.spillExistingEvents(allocator, io, dir, result_index);
        try appendSpillRecord(allocator, io, self.spill_path.?, event);
    }

    fn flush(
        self: *CliTestBufferedTranscript,
        allocator: Allocator,
        io: std.Io,
        stdout_body: *std.Io.Writer,
        stderr_body: *std.Io.Writer,
    ) ReportRenderError!usize {
        var rendered_count: usize = 0;

        if (self.spill_path) |path| {
            const bytes = std.Io.Dir.cwd().readFileAlloc(io, path, allocator, .unlimited) catch return error.WriteFailed;
            defer allocator.free(bytes);
            rendered_count += try renderCliTestTranscriptEventRecords(stdout_body, stderr_body, bytes);
            std.Io.Dir.cwd().deleteFile(io, path) catch {};
            allocator.free(path);
            self.spill_path = null;
        }

        if (self.events.items.len > 0) {
            try renderCliTestTranscriptEvents(stdout_body, stderr_body, self.events.items);
            rendered_count += self.events.items.len;
            deinitCliTestTranscriptEventPayloads(allocator, self.events.items);
            self.events.clearRetainingCapacity();
            self.memory_bytes = 0;
        }

        return rendered_count;
    }
};

const CliTestTranscriptCoordinator = struct {
    allocator: Allocator,
    io: std.Io,
    stdout_body: *std.Io.Writer,
    stderr_body: *std.Io.Writer,
    verbose: bool,
    report_config: reporting.ReportingConfig,
    next_to_print: usize = 0,
    entries: []?CliTestRenderEntry,
    event_buffers: []CliTestBufferedTranscript,
    rendered_event_counts: []usize,
    spill_dir: ?[]const u8,
    delete_spill_dir_on_deinit: bool,
    spill_threshold_bytes: usize,

    fn init(
        allocator: Allocator,
        io: std.Io,
        stdout_body: *std.Io.Writer,
        stderr_body: *std.Io.Writer,
        verbose: bool,
        report_config: reporting.ReportingConfig,
        options: CliTestTranscriptCoordinatorOptions,
    ) Allocator.Error!CliTestTranscriptCoordinator {
        const entry_count = options.entry_count;
        const entries = try allocator.alloc(?CliTestRenderEntry, entry_count);
        errdefer allocator.free(entries);
        for (entries) |*entry| {
            entry.* = null;
        }
        const event_buffers = try allocator.alloc(CliTestBufferedTranscript, entry_count);
        errdefer allocator.free(event_buffers);
        for (event_buffers) |*buffer| {
            buffer.* = .{};
        }
        const rendered_event_counts = try allocator.alloc(usize, entry_count);
        errdefer allocator.free(rendered_event_counts);
        @memset(rendered_event_counts, 0);
        return .{
            .allocator = allocator,
            .io = io,
            .stdout_body = stdout_body,
            .stderr_body = stderr_body,
            .verbose = verbose,
            .report_config = report_config,
            .entries = entries,
            .event_buffers = event_buffers,
            .rendered_event_counts = rendered_event_counts,
            .spill_dir = options.spill_dir,
            .delete_spill_dir_on_deinit = options.delete_spill_dir_on_deinit,
            .spill_threshold_bytes = options.spill_threshold_bytes,
        };
    }

    fn deinit(self: *CliTestTranscriptCoordinator) void {
        for (self.event_buffers) |*buffer| {
            buffer.deinit(self.allocator, self.io);
        }
        if (self.delete_spill_dir_on_deinit) {
            if (self.spill_dir) |dir| {
                std.Io.Dir.cwd().deleteTree(self.io, dir) catch {};
            }
        }
        self.allocator.free(self.rendered_event_counts);
        self.allocator.free(self.event_buffers);
        self.allocator.free(self.entries);
    }

    fn publishEvent(
        self: *CliTestTranscriptCoordinator,
        result_index: usize,
        event: CliTestTranscriptEvent,
    ) ReportRenderError!void {
        if (builtin.mode == .Debug and result_index >= self.entries.len) {
            std.debug.panic("CLI test transcript coordinator received out-of-range event index {d} for {d} entries", .{ result_index, self.entries.len });
        }
        if (result_index < self.next_to_print) return;
        if (result_index == self.next_to_print) {
            try renderCliTestTranscriptEvents(self.stdout_body, self.stderr_body, (&event)[0..1]);
            self.rendered_event_counts[result_index] += 1;
            return;
        }

        try self.event_buffers[result_index].append(
            self.allocator,
            self.io,
            result_index,
            event,
            self.spill_dir,
            self.spill_threshold_bytes,
        );
    }

    fn publishFinished(
        self: *CliTestTranscriptCoordinator,
        result_index: usize,
        entry: CliTestRenderEntry,
    ) ReportRenderError!void {
        if (builtin.mode == .Debug and result_index >= self.entries.len) {
            std.debug.panic("CLI test transcript coordinator received out-of-range result index {d} for {d} entries", .{ result_index, self.entries.len });
        }
        self.entries[result_index] = entry;
        if (result_index == self.next_to_print) {
            try self.flushReadyPrefix();
        }
    }

    fn flushBufferedEventsForNext(self: *CliTestTranscriptCoordinator) ReportRenderError!void {
        if (self.next_to_print >= self.event_buffers.len) return;
        const buffer = &self.event_buffers[self.next_to_print];
        self.rendered_event_counts[self.next_to_print] += try buffer.flush(
            self.allocator,
            self.io,
            self.stdout_body,
            self.stderr_body,
        );
    }

    fn flushReadyPrefix(self: *CliTestTranscriptCoordinator) ReportRenderError!void {
        while (self.next_to_print < self.entries.len) {
            try self.flushBufferedEventsForNext();
            const entry = self.entries[self.next_to_print] orelse break;
            try renderCliTestResultEntry(
                self.allocator,
                self.stdout_body,
                self.stderr_body,
                entry,
                self.verbose,
                self.report_config,
                self.rendered_event_counts[self.next_to_print],
            );
            self.entries[self.next_to_print] = null;
            self.next_to_print += 1;
        }
        try self.flushBufferedEventsForNext();
    }
};

const CliOptimizedLiveTestOutput = struct {
    ctx: *CliCtx,
    coordinator: *CliTestTranscriptCoordinator,
    mutex: std.atomic.Mutex = .unlocked,
    owned_results: []?CliTestResultItem,
    runs: []const CliTestRootRun = &.{},
    err: ?ReportRenderError = null,

    fn init(
        ctx: *CliCtx,
        coordinator: *CliTestTranscriptCoordinator,
        entry_count: usize,
    ) Allocator.Error!CliOptimizedLiveTestOutput {
        const owned_results = try ctx.gpa.alloc(?CliTestResultItem, entry_count);
        for (owned_results) |*slot| {
            slot.* = null;
        }
        return .{
            .ctx = ctx,
            .coordinator = coordinator,
            .owned_results = owned_results,
        };
    }

    fn lock(self: *CliOptimizedLiveTestOutput) void {
        while (!self.mutex.tryLock()) {
            std.atomic.spinLoopHint();
        }
    }

    fn unlock(self: *CliOptimizedLiveTestOutput) void {
        self.mutex.unlock();
    }

    fn deinit(self: *CliOptimizedLiveTestOutput) void {
        for (self.owned_results) |maybe_result| {
            if (maybe_result) |result| {
                deinitCliTestResultItemPayload(self.ctx.gpa, result);
            }
        }
        self.ctx.gpa.free(self.owned_results);
    }

    fn setRuns(self: *CliOptimizedLiveTestOutput, runs: []const CliTestRootRun) void {
        self.runs = runs;
    }

    fn clearRuns(self: *CliOptimizedLiveTestOutput) void {
        self.runs = &.{};
    }

    fn checkError(self: *CliOptimizedLiveTestOutput) ReportRenderError!void {
        if (self.err) |err| return err;
    }

    fn publishCopiedEntry(
        self: *CliOptimizedLiveTestOutput,
        result_index: usize,
        env: *const ModuleEnv,
        path: []const u8,
        result: CliTestResultItem,
    ) void {
        self.lock();
        defer self.unlock();

        if (self.err != null) return;
        if (builtin.mode == .Debug and result_index >= self.owned_results.len) {
            std.debug.panic("CLI optimized live output received out-of-range result index {d} for {d} entries", .{ result_index, self.owned_results.len });
        }
        if (builtin.mode == .Debug and self.owned_results[result_index] != null) {
            std.debug.panic("CLI optimized live output received duplicate result index {d}", .{result_index});
        }

        const copied = copyCliTestResultItem(self.ctx.gpa, result) catch |err| {
            self.err = err;
            return;
        };
        self.owned_results[result_index] = copied;
        self.coordinator.publishFinished(result_index, .{
            .env = env,
            .path = path,
            .result = &self.owned_results[result_index].?,
        }) catch |err| {
            self.err = err;
        };
    }

    fn publishEvalResult(
        self: *CliOptimizedLiveTestOutput,
        call_index: usize,
        eval_result: eval.test_helpers.BoolRootEvalResult,
    ) void {
        if (builtin.mode == .Debug and call_index >= self.runs.len) {
            std.debug.panic("CLI optimized live output received out-of-range call index {d} for {d} roots", .{ call_index, self.runs.len });
        }
        const run = self.runs[call_index];
        const result = cliTestResultItemFromEval(self.ctx, run, eval_result) catch |err| {
            self.lock();
            self.err = err;
            self.unlock();
            return;
        };

        self.lock();
        defer self.unlock();

        if (self.err != null) {
            deinitCliTestResultItemPayload(self.ctx.gpa, result);
            return;
        }
        const result_index: usize = @intCast(run.result_index);
        if (builtin.mode == .Debug and result_index >= self.owned_results.len) {
            std.debug.panic("CLI optimized live output received out-of-range result index {d} for {d} entries", .{ result_index, self.owned_results.len });
        }
        if (builtin.mode == .Debug and self.owned_results[result_index] != null) {
            std.debug.panic("CLI optimized live output received duplicate result index {d}", .{result_index});
        }

        self.owned_results[result_index] = result;
        self.coordinator.publishFinished(result_index, .{
            .env = run.env,
            .path = run.path,
            .result = &self.owned_results[result_index].?,
        }) catch |err| {
            self.err = err;
        };
    }

    fn publishEvent(
        self: *CliOptimizedLiveTestOutput,
        call_index: usize,
        event_view: eval.test_helpers.BoolRootEventView,
    ) void {
        if (builtin.mode == .Debug and call_index >= self.runs.len) {
            std.debug.panic("CLI optimized live output received out-of-range event call index {d} for {d} roots", .{ call_index, self.runs.len });
        }
        const run = self.runs[call_index];
        const event: CliTestTranscriptEvent = switch (event_view) {
            .dbg => |payload| .{ .stream = .stderr, .kind = .dbg, .payload = payload },
            .expect_failed => |payload| .{ .stream = .stderr, .kind = .expect_failed, .payload = payload },
            .crashed => |payload| .{ .stream = .stderr, .kind = .crashed, .payload = payload },
        };

        self.lock();
        defer self.unlock();

        if (self.err != null) return;
        self.coordinator.publishEvent(@intCast(run.result_index), event) catch |err| {
            self.err = err;
        };
    }

    fn completionCallback(
        context: *anyopaque,
        call_index: usize,
        eval_result: *const eval.test_helpers.BoolRootEvalResult,
    ) void {
        const self: *CliOptimizedLiveTestOutput = @ptrCast(@alignCast(context));
        self.publishEvalResult(call_index, eval_result.*);
    }

    fn eventCallback(
        context: *anyopaque,
        call_index: usize,
        event: eval.test_helpers.BoolRootEventView,
    ) void {
        const self: *CliOptimizedLiveTestOutput = @ptrCast(@alignCast(context));
        self.publishEvent(call_index, event);
    }
};

test "test transcript coordinator streams and buffers in plan order" {
    const allocator = std.testing.allocator;

    var stdout_body = std.Io.Writer.Allocating.init(allocator);
    defer stdout_body.deinit();
    var stderr_body = std.Io.Writer.Allocating.init(allocator);
    defer stderr_body.deinit();

    var coordinator = try CliTestTranscriptCoordinator.init(
        allocator,
        std.testing.io,
        &stdout_body.writer,
        &stderr_body.writer,
        false,
        reporting.ReportingConfig.initForTesting(),
        .{ .entry_count = 3 },
    );
    defer coordinator.deinit();

    const dummy_env: *const ModuleEnv = undefined;

    const event_zero = CliTestTranscriptEvent{ .stream = .stderr, .kind = .dbg, .payload = "zero" };
    const event_one_a = CliTestTranscriptEvent{ .stream = .stderr, .kind = .dbg, .payload = "one-a" };
    const event_one_b = CliTestTranscriptEvent{ .stream = .stderr, .kind = .dbg, .payload = "one-b" };
    const event_two = CliTestTranscriptEvent{ .stream = .stderr, .kind = .dbg, .payload = "two" };

    const transcript_zero = [_]CliTestTranscriptEvent{event_zero};
    const transcript_one = [_]CliTestTranscriptEvent{ event_one_a, event_one_b };
    const transcript_two = [_]CliTestTranscriptEvent{event_two};

    const result_zero = CliTestResultItem{ .result = .passed, .order = 0, .region = base.Region.zero(), .transcript = transcript_zero[0..], .failure_detail = null };
    const result_one = CliTestResultItem{ .result = .passed, .order = 1, .region = base.Region.zero(), .transcript = transcript_one[0..], .failure_detail = null };
    const result_two = CliTestResultItem{ .result = .passed, .order = 2, .region = base.Region.zero(), .transcript = transcript_two[0..], .failure_detail = null };

    try coordinator.publishEvent(2, event_two);
    try coordinator.publishEvent(1, event_one_a);
    try std.testing.expectEqualStrings("", stdout_body.written());
    try std.testing.expectEqualStrings("", stderr_body.written());

    try coordinator.publishEvent(0, event_zero);
    try std.testing.expectEqualStrings("[dbg] zero\n", stderr_body.written());

    try coordinator.publishFinished(0, .{ .env = dummy_env, .path = "test.roc", .result = &result_zero });
    try std.testing.expectEqualStrings("[dbg] zero\n[dbg] one-a\n", stderr_body.written());

    try coordinator.publishEvent(1, event_one_b);
    try std.testing.expectEqualStrings("[dbg] zero\n[dbg] one-a\n[dbg] one-b\n", stderr_body.written());

    try coordinator.publishFinished(1, .{ .env = dummy_env, .path = "test.roc", .result = &result_one });
    try std.testing.expectEqualStrings("[dbg] zero\n[dbg] one-a\n[dbg] one-b\n[dbg] two\n", stderr_body.written());

    try coordinator.publishFinished(2, .{ .env = dummy_env, .path = "test.roc", .result = &result_two });
    try std.testing.expectEqualStrings("[dbg] zero\n[dbg] one-a\n[dbg] one-b\n[dbg] two\n", stderr_body.written());
    try std.testing.expectEqualStrings("", stdout_body.written());
}

test "test transcript coordinator spills later buffered entries to isolated temp dir" {
    const allocator = std.testing.allocator;

    var tmp = std.testing.tmpDir(.{});
    defer tmp.cleanup();
    try tmp.dir.createDir(std.testing.io, "spill", .default_dir);
    const spill_dir = try tmp.dir.realPathFileAlloc(std.testing.io, "spill", allocator);
    defer allocator.free(spill_dir);

    var stdout_body = std.Io.Writer.Allocating.init(allocator);
    defer stdout_body.deinit();
    var stderr_body = std.Io.Writer.Allocating.init(allocator);
    defer stderr_body.deinit();

    const dummy_env: *const ModuleEnv = undefined;
    const event_zero = CliTestTranscriptEvent{ .stream = .stderr, .kind = .dbg, .payload = "first" };
    const event_one_spilled = CliTestTranscriptEvent{ .stream = .stderr, .kind = .dbg, .payload = "later-spilled" };
    const event_one_live = CliTestTranscriptEvent{ .stream = .stderr, .kind = .dbg, .payload = "later-live" };

    const transcript_zero = [_]CliTestTranscriptEvent{event_zero};
    const transcript_one = [_]CliTestTranscriptEvent{ event_one_spilled, event_one_live };
    const result_zero = CliTestResultItem{ .result = .passed, .order = 0, .region = base.Region.zero(), .transcript = transcript_zero[0..], .failure_detail = null };
    const result_one = CliTestResultItem{ .result = .passed, .order = 1, .region = base.Region.zero(), .transcript = transcript_one[0..], .failure_detail = null };

    {
        var coordinator = try CliTestTranscriptCoordinator.init(
            allocator,
            std.testing.io,
            &stdout_body.writer,
            &stderr_body.writer,
            false,
            reporting.ReportingConfig.initForTesting(),
            .{
                .entry_count = 2,
                .spill_dir = spill_dir,
                .delete_spill_dir_on_deinit = true,
                .spill_threshold_bytes = 1,
            },
        );
        defer coordinator.deinit();

        try coordinator.publishEvent(1, event_one_spilled);
        try std.testing.expect(coordinator.event_buffers[1].spill_path != null);
        try std.testing.expectEqualStrings("", stderr_body.written());

        try coordinator.publishEvent(0, event_zero);
        try std.testing.expectEqualStrings("[dbg] first\n", stderr_body.written());

        try coordinator.publishFinished(0, .{ .env = dummy_env, .path = "test.roc", .result = &result_zero });
        try std.testing.expectEqualStrings("[dbg] first\n[dbg] later-spilled\n", stderr_body.written());

        try coordinator.publishEvent(1, event_one_live);
        try coordinator.publishFinished(1, .{ .env = dummy_env, .path = "test.roc", .result = &result_one });
        try std.testing.expectEqualStrings("[dbg] first\n[dbg] later-spilled\n[dbg] later-live\n", stderr_body.written());
        try std.testing.expectEqualStrings("", stdout_body.written());
    }

    try std.testing.expectError(error.FileNotFound, tmp.dir.access(std.testing.io, "spill", .{}));
}

fn renderCliTestResultEntry(
    allocator: Allocator,
    stdout_body: *std.Io.Writer,
    stderr_body: *std.Io.Writer,
    entry: CliTestRenderEntry,
    verbose: bool,
    report_config: reporting.ReportingConfig,
    transcript_events_already_rendered: usize,
) ReportRenderError!void {
    if (builtin.mode == .Debug and transcript_events_already_rendered > entry.result.transcript.len) {
        std.debug.panic(
            "CLI test transcript coordinator rendered {d} events before finished result with {d} events",
            .{ transcript_events_already_rendered, entry.result.transcript.len },
        );
    }
    try renderCliTestTranscriptEvents(stdout_body, stderr_body, entry.result.transcript[transcript_events_already_rendered..]);
    switch (entry.result.result) {
        .passed => {
            if (!verbose) return;
            const region_info = entry.env.calcRegionInfo(entry.result.region);
            const green = if (report_config.shouldUseColors()) ansi_term.green else "";
            const reset = if (report_config.shouldUseColors()) ansi_term.reset else "";
            try stdout_body.print("{s}PASS{s}: {s}:{}\n", .{ green, reset, entry.path, region_info.start_line_idx + 1 });
        },
        .failed => {
            const region_info = entry.env.calcRegionInfo(entry.result.region);
            try printTestProblem(
                allocator,
                stderr_body,
                entry.path,
                entry.env,
                region_info,
                "Fail",
                .runtime_error,
                entry.result.failure_detail,
                entry.result.failure_detail_visibility,
                verbose,
                report_config,
            );
        },
        .compiler_error => {
            const region_info = entry.env.calcRegionInfo(entry.result.region);
            try printTestProblem(
                allocator,
                stderr_body,
                entry.path,
                entry.env,
                region_info,
                "Compiler Error",
                .warning,
                entry.result.failure_detail,
                entry.result.failure_detail_visibility,
                verbose,
                report_config,
            );
        },
    }
}

/// Walk the per-module test results and write the per-test body output to
/// the supplied writers. Verbose PASS lines go to `stdout_body`; problem
/// blocks (and verbose PASS lines for partially-failing runs) go to
/// `stderr_body`.
fn renderTestResultBodies(
    allocator: Allocator,
    stdout_body: *std.Io.Writer,
    stderr_body: *std.Io.Writer,
    module_results: []const CliModuleTestResult,
    verbose: bool,
    report_config: reporting.ReportingConfig,
) ReportRenderError!void {
    // Verbose PASS lines go to stdout in every case; problem blocks go to
    // stderr. This matches the pre-refactor layout.
    var entry_count: usize = 0;
    for (module_results) |module_result| {
        entry_count += module_result.results.len;
    }

    var coordinator = try CliTestTranscriptCoordinator.init(
        allocator,
        std.Options.debug_io,
        stdout_body,
        stderr_body,
        verbose,
        report_config,
        .{ .entry_count = entry_count },
    );
    defer coordinator.deinit();

    var result_index: usize = 0;
    for (module_results) |module_result| {
        for (module_result.results) |*result| {
            try coordinator.publishFinished(result_index, .{
                .env = module_result.env,
                .path = module_result.path,
                .result = result,
            });
            result_index += 1;
        }
    }
}

/// Prints a formatted test problem to stderr, including the source snippet,
/// an optional doc comment from the preceding line, and an optional error message.
fn printTestProblem(
    allocator: Allocator,
    stderr: *std.Io.Writer,
    path: []const u8,
    env: *const ModuleEnv,
    region_info: base.RegionInfo,
    label: []const u8,
    severity: reporting.Severity,
    failure_detail: ?[]const u8,
    failure_detail_visibility: CliTestFailureDetailVisibility,
    verbose: bool,
    report_config: reporting.ReportingConfig,
) ReportRenderError!void {
    const src = env.getSourceAll();

    const doc_comment: ?[]const u8 = blk: {
        const line_starts = env.getLineStarts();
        const curr_line_start_idx = region_info.start_line_idx;
        const curr_line_start = line_starts[curr_line_start_idx];
        const prev_line_start = if (curr_line_start_idx > 0) line_starts[curr_line_start_idx - 1] else break :blk null;
        const prev_line = std.mem.trimStart(u8, src[prev_line_start..curr_line_start], " ");
        if (std.mem.startsWith(u8, prev_line, "##")) {
            break :blk std.mem.trimEnd(u8, prev_line, " \r\n");
        }
        break :blk null;
    };

    // The headline is the test's doc comment, if any. House style requires a
    // headline to end in a period, so append one when it's missing.
    var headline: []const u8 = "";
    var headline_owned = false;
    if (doc_comment) |dc| {
        if (std.mem.endsWith(u8, dc, ".")) {
            headline = dc;
        } else {
            headline = try std.fmt.allocPrint(allocator, "{s}.", .{dc});
            headline_owned = true;
        }
    }
    defer if (headline_owned) allocator.free(headline);

    var report = try reporting.Report.init(allocator, label, headline, severity);
    defer report.deinit();

    try report.addSourceContext(region_info, path, src, env.getLineStarts());

    const should_print_detail = switch (failure_detail_visibility) {
        .always => true,
        .verbose_only => verbose,
    };
    if (should_print_detail) {
        if (failure_detail) |msg| {
            switch (severity) {
                .warning => try report.addWarningMessage(msg),
                else => try report.addErrorMessage(msg),
            }
        }
    }

    try stderr.writeAll("\n");
    try reporting.renderReportToTerminal(&report, stderr, reporting.ColorUtils.getPaletteForConfig(report_config), report_config);
}

const ReplMode = enum {
    interactive,
    batch,
};

// The colorful "rockin' roc repl" greeting and prompts. The colored variants
// embed ANSI cyan (`\x1b[1;36m`) reset (`\x1b[0m`) sequences; the plain variants
// are used when color is disabled (e.g. NO_COLOR or `--no-color`).
const REPL_WELCOME_COLOR = "\n  The rockin' \x1b[1;36mroc repl\n────────────────────────\x1b[0m\n\n";
const REPL_WELCOME_PLAIN = "\n  The rockin' roc repl\n────────────────────────\n\n";
const REPL_SHORT_INSTRUCTIONS = "Enter an expression, or :help, or :q to quit.\n\n";
const REPL_PROMPT_COLOR = "\x1b[1;36m»\x1b[0m ";
const REPL_PROMPT_PLAIN = "» ";
const REPL_CONT_PROMPT_COLOR = "\x1b[1;36m…\x1b[0m ";
const REPL_CONT_PROMPT_PLAIN = "… ";

fn rocRepl(ctx: *CliCtx, repl_args: cli_args.ReplArgs) CliMainError!void {
    const stdout = ctx.io.stdout();
    const backend_kind = repl_args.opt.toBackend();
    const stdin = std.Io.File.stdin();
    const stdin_is_tty = stdin.isTty(ctx.io.std_io) catch false;
    const stdout_is_tty = std.Io.File.stdout().isTty(ctx.io.std_io) catch false;
    const mode: ReplMode = if (stdin_is_tty and stdout_is_tty) .interactive else .batch;
    const report_config = try replReportingConfig(ctx, repl_args, mode);

    const no_color_env = try envVarNonEmpty(ctx.gpa, "NO_COLOR");
    const use_color = mode == .interactive and !repl_args.no_color and !no_color_env;

    // The line editor handles Ctrl-C itself (press twice in a row to quit).
    // Ignore SIGINT at the process level so a Ctrl-C while the terminal is
    // momentarily in cooked mode — during startup or while evaluating — can't
    // hard-kill the REPL instead of going through that flow.
    if (mode == .interactive and builtin.os.tag != .windows) {
        const ignore_sigint = std.posix.Sigaction{
            .handler = .{ .handler = std.posix.SIG.IGN },
            .mask = std.posix.sigemptyset(),
            .flags = 0,
        };
        std.posix.sigaction(std.posix.SIG.INT, &ignore_sigint, null);
    }

    var reader = ReplLine.init(ctx.gpa);
    defer reader.deinit();

    // Publishing the Builtin module here is the dominant startup cost (~1s). Do
    // it before printing the greeting so the greeting and the first prompt appear
    // together and the REPL is immediately interactive — otherwise the greeting
    // shows with no prompt until this finishes.
    var session = try ReplSession.init(ctx.gpa, ctx.coreCtx(), backend_kind);
    defer session.deinit();

    if (mode == .interactive) {
        try stdout.writeAll(if (use_color) REPL_WELCOME_COLOR else REPL_WELCOME_PLAIN);
        try stdout.writeAll(REPL_SHORT_INSTRUCTIONS);
        ctx.io.flush();
    }

    var pending = std.ArrayList(u8).empty;
    defer pending.deinit(ctx.gpa);

    var should_exit = false;
    var had_diagnostics = false;
    while (!should_exit) {
        const prompt: []const u8 = if (mode == .interactive)
            if (pending.items.len == 0)
                (if (use_color) REPL_PROMPT_COLOR else REPL_PROMPT_PLAIN)
            else
                (if (use_color) REPL_CONT_PROMPT_COLOR else REPL_CONT_PROMPT_PLAIN)
        else
            "";

        const read_result = try reader.readLine(ctx.gpa, ctx.io.std_io, prompt, stdin);
        switch (read_result) {
            .eof => {
                if (pending.items.len > 0) {
                    should_exit = try processReplInput(ctx, &session, pending.items, report_config, &had_diagnostics);
                    pending.clearRetainingCapacity();
                }
                break;
            },
            .line => |raw_line| {
                defer ctx.gpa.free(raw_line);

                if (pending.items.len == 0 and std.mem.trim(u8, raw_line, " \t\r\n").len == 0) {
                    continue;
                }

                if (pending.items.len == 0 and std.mem.findAny(u8, raw_line, "\n\r") != null) {
                    should_exit = try processReplInput(ctx, &session, raw_line, report_config, &had_diagnostics);
                    continue;
                }

                if (pending.items.len > 0) try pending.append(ctx.gpa, '\n');
                try pending.appendSlice(ctx.gpa, raw_line);

                switch (try session.inputStatus(pending.items)) {
                    .incomplete => {},
                    .complete, .invalid => {
                        should_exit = try processReplInput(ctx, &session, pending.items, report_config, &had_diagnostics);
                        pending.clearRetainingCapacity();
                    },
                }
            },
        }
        ctx.io.flush();
    }

    if (mode == .batch and had_diagnostics) {
        return error.CliError;
    }
}

fn processReplInput(
    ctx: *CliCtx,
    session: *ReplSession,
    input: []const u8,
    report_config: reporting.ReportingConfig,
    had_diagnostics: *bool,
) CliMainError!bool {
    const stdout = ctx.io.stdout();
    const stderr = ctx.io.stderr();

    const statements = try session.splitInputIntoStatements(input);
    defer session.freeStatementSlices(statements);

    for (statements) |statement| {
        const result = try session.stepWithConfig(statement, report_config);
        defer result.deinit(ctx.gpa);

        switch (result) {
            .output => |output| {
                if (output.len > 0) {
                    try stdout.print("{s}\n", .{output});
                }
            },
            .diagnostic => |diagnostic| {
                had_diagnostics.* = true;
                if (diagnostic.len > 0) {
                    try stderr.print("{s}\n", .{diagnostic});
                }
            },
            .none => {},
            .exit => return true,
        }
    }

    return false;
}

fn replReportingConfig(ctx: *CliCtx, repl_args: cli_args.ReplArgs, mode: ReplMode) Allocator.Error!reporting.ReportingConfig {
    const stderr_is_tty = std.Io.File.stderr().isTty(ctx.io.std_io) catch false;
    const no_color_env = try envVarNonEmpty(ctx.gpa, "NO_COLOR");
    const force_color = try envVarNonEmpty(ctx.gpa, "FORCE_COLOR");
    const high_contrast = try envVarEquals(ctx.gpa, "ROC_HIGH_CONTRAST", "1");
    const color_disabled = repl_args.no_color or no_color_env;
    const should_color = !color_disabled and (force_color or (mode == .interactive and stderr_is_tty));

    // Always render the box layout; when color is disabled, keep the
    // color_terminal target but force the no-color palette so the REPL shows the
    // same plain box as non-TTY output (rather than the old markdown layout).
    var config = if (should_color)
        if (high_contrast) reporting.ReportingConfig.initHighContrast() else ctx.terminalReportConfig()
    else blk: {
        var plain = ctx.terminalReportConfig();
        plain.color_preference = .never;
        break :blk plain;
    };

    config.is_tty = stderr_is_tty;
    return config;
}

fn envVarNonEmpty(allocator: Allocator, name: []const u8) Allocator.Error!bool {
    const value = try getEnvVar(allocator, name) orelse return false;
    defer allocator.free(value);
    return value.len > 0;
}

fn envVarEquals(allocator: Allocator, name: []const u8, expected: []const u8) Allocator.Error!bool {
    const value = try getEnvVar(allocator, name) orelse return false;
    defer allocator.free(value);
    return std.mem.eql(u8, value, expected);
}

const glue = @import("glue");

const RocGlueError = glue.GlueError || CliError || SourceRefResolveError || error{InvalidArguments};

fn rocGlue(ctx: *CliCtx, args: cli_args.GlueArgs) RocGlueError!void {
    // The glue spec accepts a local path, a bundle URL, or an installed
    // shorthand. An installed glue entry also carries the plugin dylib that
    // was built with --opt=speed at install time, so it is loaded directly
    // instead of compiling a dylib on the fly.
    var glue_spec = args.glue_spec;
    var installed_dylib_path: ?[]const u8 = null;
    switch (install_store.classifySourceRef(args.glue_spec)) {
        .local_path => {},
        .url => glue_spec = (try resolveUrlBundle(ctx, args.glue_spec)).source_path,
        .shorthand => {
            const entry = try resolveInstalledEntry(ctx, args.glue_spec);
            if (entry.kind != .glue) {
                try ctx.io.stderr().print(
                    "Error: `{s}` is installed as an application, not a glue spec. Run it with: roc run {s}\n",
                    .{ args.glue_spec, args.glue_spec },
                );
                return error.InvalidArguments;
            }
            glue_spec = entry.paths.main_roc_path;
            installed_dylib_path = entry.artifact_path;
        },
    }
    const platform_path = (try resolveSourceArg(ctx, args.platform_path, false)).path;

    return glue.rocGlue(ctx.gpa, ctx.io.stderr(), ctx.io.stdout(), .{
        .glue_spec = glue_spec,
        .output_dir = args.output_dir,
        .platform_path = platform_path,
        .no_cache = args.no_cache,
        .installed_dylib_path = installed_dylib_path,
        .opt = switch (args.opt) {
            .dev => .dev,
            .size => .size,
            .speed => .speed,
            .interpreter => unreachable,
        },
    }, ctx.coreCtx(), ctx.io.std_io) catch |err| {
        switch (err) {
            error.GlueDylibStampMismatch, error.GlueDylibUnavailable => if (installed_dylib_path != null) {
                try ctx.io.stderr().print(
                    "The installed glue plugin for `{s}` cannot be used by this compiler. Reinstall it with: roc install {s} <URL>\n",
                    .{ args.glue_spec, args.glue_spec },
                );
            },
            else => {},
        }
        return err;
    };
}

/// Reads, parses, formats, and overwrites all Roc files at the given paths.
/// Recurses into directories to search for Roc files.
fn rocFormat(ctx: *CliCtx, args: cli_args.FormatArgs) CliMainError!void {
    const trace = tracy.trace(@src());
    defer trace.end();

    const stdout = ctx.io.stdout();
    const stderr = ctx.io.stderr();
    if (args.stdin) {
        fmt.formatStdin(ctx.gpa, ctx.io.std_io, std.Io.File.stdin(), std.Io.File.stdout(), stderr) catch |err| return err;
        return;
    }

    const timer_start_ns = std.Io.Timestamp.now(ctx.io.std_io, .real).nanoseconds;
    var elapsed: u64 = undefined;
    var failure_count: usize = 0;
    var had_errors: bool = false;

    if (args.check) {
        var unformatted_files = std.ArrayList([]const u8).empty;
        defer unformatted_files.deinit(ctx.gpa);

        for (args.paths) |path| {
            var result = try fmt.formatPath(ctx.gpa, ctx.arena, std.Io.Dir.cwd(), path, true, ctx.io.std_io, stderr);
            defer result.deinit();
            if (result.unformatted_files) |files| {
                try unformatted_files.appendSlice(ctx.gpa, files.items);
            }
            failure_count += result.failure;
        }

        elapsed = @as(u64, @intCast(std.Io.Timestamp.now(ctx.io.std_io, .real).nanoseconds - timer_start_ns));
        if (unformatted_files.items.len > 0) {
            try stdout.print("The following file(s) failed `roc fmt --check`:", .{});
            for (unformatted_files.items) |file_name| {
                try stdout.print("    {s}\n", .{file_name});
            }
            try stdout.print("You can fix this with `roc fmt FILENAME.roc`.", .{});
            had_errors = true;
        } else {
            try stdout.print("All formatting valid.\n", .{});
        }
        if (failure_count > 0) {
            try stdout.print("Failed to check {} files.", .{failure_count});
            had_errors = true;
        }
    } else {
        var success_count: usize = 0;
        for (args.paths) |path| {
            const result = try fmt.formatPath(ctx.gpa, ctx.arena, std.Io.Dir.cwd(), path, false, ctx.io.std_io, stderr);
            success_count += result.success;
            failure_count += result.failure;
        }
        elapsed = @as(u64, @intCast(std.Io.Timestamp.now(ctx.io.std_io, .real).nanoseconds - timer_start_ns));
        try stdout.print("Successfully formatted {} files\n", .{success_count});
        if (failure_count > 0) {
            try stdout.print("Failed to format {} files.\n", .{failure_count});
            had_errors = true;
        }
    }

    try stdout.print("Took ", .{});
    try formatElapsedTime(stdout, elapsed);
    try stdout.print(".\n", .{});

    if (had_errors) {
        return error.FormattingFailed;
    }
}

/// Create a progress reporter for a CLI operation. The breakdown is drawn to
/// stderr; it animates only when stderr is a terminal, and is shown when the
/// operation is slow or `--timings` was requested.
fn makeReporter(ctx: *CliCtx, op_label: []const u8, timings_flag: bool) progress.Reporter {
    const is_tty = if (builtin.target.cpu.arch == .wasm32)
        false
    else
        std.Io.File.stderr().isTty(ctx.io.std_io) catch false;
    return progress.Reporter.init(.{
        .std_io = ctx.io.std_io,
        .writer = ctx.io.stderr(),
        .op_label = op_label,
        .timings_flag = timings_flag,
        .is_tty = is_tty,
    });
}

/// Split the front-end's accumulated timing into the user-facing phases shown
/// in the breakdown once type checking completes.
fn frontEndBreakdown(timing: anytype) [3]progress.SubTiming {
    return .{
        .{ .name = "Parsing", .ns = timing.tokenize_parse_ns },
        .{ .name = "Name Resolution", .ns = timing.canonicalize_ns + timing.canonicalize_diagnostics_ns },
        .{ .name = "Type Inference", .ns = timing.type_checking_ns + timing.check_diagnostics_ns },
    };
}

/// Print the friendly post-build summary line and (optionally) cache statistics.
fn printBuildSuccess(
    ctx: *CliCtx,
    final_output_path: []const u8,
    warning_count: usize,
    elapsed_ns: u64,
    verbose: bool,
    cache_stats: anytype,
    cache_percent: u32,
) std.Io.Writer.Error!void {
    const stdout = ctx.io.stdout();
    const is_tty = if (builtin.target.cpu.arch == .wasm32)
        false
    else
        std.Io.File.stdout().isTty(ctx.io.std_io) catch false;
    const green = if (is_tty) ansi_term.green else "";
    const yellow = if (is_tty) ansi_term.yellow else "";
    const reset = if (is_tty) ansi_term.reset else "";
    const warning_color = if (warning_count == 0) green else yellow;
    const warnings_word = if (warning_count == 1) "warning" else "warnings";

    try stdout.print("{s}0{s} errors and {s}{d}{s} {s} found in ", .{
        green, reset, warning_color, warning_count, reset, warnings_word,
    });
    try progress.writeDuration(stdout, elapsed_ns);
    try stdout.print(" while successfully building:\n\n    {s}\n", .{final_output_path});

    if (verbose) {
        try stdout.print("\n    Modules: {} total, {} cached, {} built\n", .{
            cache_stats.modules_total,
            cache_stats.cache_hits,
            cache_stats.modules_compiled,
        });
        try stdout.print("    Cache Hit: {}%\n", .{cache_percent});
    }
}

/// Helper function to format elapsed time, showing decimal milliseconds
fn formatElapsedTime(writer: anytype, elapsed_ns: u64) error{WriteFailed}!void {
    const elapsed_ms_float = @as(f64, @floatFromInt(elapsed_ns)) / @as(f64, @floatFromInt(std.time.ns_per_ms));
    try writer.print("{d:.1} ms", .{elapsed_ms_float});
}

/// Helper function to format elapsed time as rounded integer milliseconds (no decimals)
fn formatElapsedTimeMs(writer: anytype, elapsed_ns: u64) error{WriteFailed}!void {
    const elapsed_ms: u64 = (elapsed_ns + 500_000) / 1_000_000; // Round to nearest ms
    try writer.print("{}ms", .{elapsed_ms});
}

/// Compute cache hit percentage as an integer (0-100), rounded to nearest
fn cacheHitPercent(cache_hits: u32, cache_misses: u32) u32 {
    const total = cache_hits + cache_misses;
    if (total == 0) return 0;
    return @intCast((@as(u64, cache_hits) * 100 + total / 2) / total);
}

/// Compute average module time in nanoseconds
fn moduleTimeAvgNs(sum_ns: u64, count: u32) u64 {
    if (count == 0) return 0;
    return sum_ns / count;
}

/// Convert nanoseconds to rounded milliseconds
fn nsToMs(ns: u64) u32 {
    return @intCast((ns + 500_000) / 1_000_000);
}

fn handleProcessFileError(err: anytype, stderr: anytype, path: []const u8) @TypeOf(err)!void {
    stderr.print("Failed to check {s}: ", .{path}) catch {};
    switch (err) {
        // Custom BuildEnv errors - these need special messages
        error.ExpectedAppHeader => stderr.print("Expected app header but found different header type\n", .{}) catch {},
        error.ExpectedPlatformString => stderr.print("Expected platform string in header\n", .{}) catch {},
        error.PathOutsideWorkspace => stderr.print("Dependency path outside workspace not allowed\n", .{}) catch {},
        error.UnsupportedHeader => stderr.print("Unsupported header type\n", .{}) catch {},
        error.ExpectedString => stderr.print("Expected string in header\n", .{}) catch {},
        error.Internal => stderr.print("Internal compiler error\n", .{}) catch {},
        error.InvalidDependency => stderr.print("Invalid dependency relationship\n", .{}) catch {},
        error.InvalidPackageName => stderr.print("Invalid package name\n", .{}) catch {},

        // Catch-all for any other errors
        else => stderr.print("{s}\n", .{@errorName(err)}) catch {},
    }

    return err;
}

/// Result from checking a file using BuildEnv
const CheckResult = struct {
    reports: []DrainedReport,
    timing: CheckTimingInfo = if (builtin.target.cpu.arch == .wasm32) .{} else .{
        .tokenize_parse_ns = 0,
        .canonicalize_ns = 0,
        .canonicalize_diagnostics_ns = 0,
        .type_checking_ns = 0,
        .check_diagnostics_ns = 0,
    },
    error_count: u32 = 0,
    warning_count: u32 = 0,
    /// Build statistics
    modules_total: u32 = 0,
    cache_hits: u32 = 0,
    cache_misses: u32 = 0,
    modules_compiled: u32 = 0,
    /// Module compile time tracking (in nanoseconds)
    module_time_min_ns: u64 = 0,
    module_time_max_ns: u64 = 0,
    module_time_sum_ns: u64 = 0,

    /// Free allocated memory
    pub fn deinit(self: *CheckResult, gpa: Allocator) void {
        for (self.reports) |*report| {
            report.deinit(gpa);
        }
        gpa.free(self.reports);
    }
};

/// Drained report with module info and file path
const DrainedReport = struct {
    file_path: []const u8,
    reports: []reporting.Report,

    pub fn deinit(self: *DrainedReport, gpa: Allocator) void {
        gpa.free(self.file_path);
        for (self.reports) |*report| {
            report.deinit();
        }
        gpa.free(self.reports);
    }
};

fn countNewlines(bytes: []const u8) u32 {
    var count: u32 = 0;
    for (bytes) |byte| {
        if (byte == '\n') count += 1;
    }
    return count;
}

fn remapDefaultAppSourceRegion(
    allocator: Allocator,
    region: *reporting.SourceCodeDisplayRegion,
    original_path: []const u8,
    original_source: []const u8,
    original_line_starts: []const u32,
    synthetic_header_lines: u32,
) Allocator.Error!void {
    if (region.start_line <= synthetic_header_lines or region.end_line <= synthetic_header_lines) return;
    if (original_line_starts.len == 0) return;

    const original_start_line = region.start_line - synthetic_header_lines;
    const original_end_line = region.end_line - synthetic_header_lines;
    const region_info = base.RegionInfo{
        .start_line_idx = original_start_line - 1,
        .start_col_idx = region.start_column - 1,
        .end_line_idx = original_end_line - 1,
        .end_col_idx = region.end_column - 1,
    };

    const line_text = try allocator.dupe(u8, region_info.calculateLineText(original_source, original_line_starts));
    errdefer allocator.free(line_text);
    const filename = try allocator.dupe(u8, original_path);
    errdefer allocator.free(filename);

    allocator.free(region.line_text);
    if (region.filename) |old_filename| allocator.free(old_filename);

    region.line_text = line_text;
    region.filename = filename;
    region.start_line = original_start_line;
    region.end_line = original_end_line;
}

fn remapDefaultAppDocumentElement(
    allocator: Allocator,
    element: *reporting.DocumentElement,
    original_path: []const u8,
    original_source: []const u8,
    original_line_starts: []const u32,
    synthetic_header_lines: u32,
) Allocator.Error!void {
    switch (element.*) {
        .source_code_region => |*region| try remapDefaultAppSourceRegion(
            allocator,
            region,
            original_path,
            original_source,
            original_line_starts,
            synthetic_header_lines,
        ),
        .source_code_with_underlines => |*underlines| {
            const old_start_line = underlines.display_region.start_line;
            try remapDefaultAppSourceRegion(
                allocator,
                &underlines.display_region,
                original_path,
                original_source,
                original_line_starts,
                synthetic_header_lines,
            );
            if (old_start_line <= synthetic_header_lines) return;
            for (underlines.underline_regions) |*underline| {
                if (underline.start_line > synthetic_header_lines) {
                    underline.start_line -= synthetic_header_lines;
                }
                if (underline.end_line > synthetic_header_lines) {
                    underline.end_line -= synthetic_header_lines;
                }
            }
        },
        else => {},
    }
}

fn remapDefaultAppCheckReports(
    ctx: *CliCtx,
    check_result: *CheckResult,
    synthetic_app_path: []const u8,
    original_path: []const u8,
    original_source: []const u8,
    synthetic_header_lines: u32,
) Allocator.Error!void {
    var original_line_starts = try base.RegionInfo.findLineStarts(ctx.gpa, original_source);
    defer original_line_starts.deinit(ctx.gpa);

    for (check_result.reports) |*module| {
        if (!std.mem.eql(u8, module.file_path, synthetic_app_path)) continue;

        const remapped_file_path = try ctx.gpa.dupe(u8, original_path);
        errdefer ctx.gpa.free(remapped_file_path);
        ctx.gpa.free(module.file_path);
        module.file_path = remapped_file_path;

        for (module.reports) |*report| {
            for (report.document.elements.items) |*element| {
                try remapDefaultAppDocumentElement(
                    report.document.allocator,
                    element,
                    original_path,
                    original_source,
                    original_line_starts.items.items,
                    synthetic_header_lines,
                );
            }
        }
    }
}

/// Timing information for check phases
const CheckTimingInfo = if (builtin.target.cpu.arch == .wasm32) struct {} else TimingInfo;

/// Result from checking a file that preserves the BuildEnv for further processing (e.g., docs generation)
const CheckResultWithBuildEnv = struct {
    check_result: CheckResult,
    build_env: BuildEnv,

    /// Free allocated memory including the BuildEnv
    pub fn deinit(self: *CheckResultWithBuildEnv, gpa: Allocator) void {
        self.check_result.deinit(gpa);
        self.build_env.deinit();
    }
};

fn buildForCheckWithOptionalMain(build_env: *BuildEnv, filepath: []const u8, main_filepath: ?[]const u8) CheckFileWithBuildEnvPreservedError!void {
    try build_env.buildResolvingMain(filepath, main_filepath);
}

/// Returns true when `filepath` is the compiler-owned builtin module (`Builtin.roc`).
///
/// We deliberately do NOT compare against the absolute path of the builtin source
/// on the *build* machine: a distributed binary would then only recognize the
/// builtin when run from the exact checkout directory it was built in. Instead we
/// detect the builtin by its filename plus two markers that only
/// the compiler-owned builtin source contains: the `ProvidedByCompiler` tag and
/// the `Str ::` declaration. This heuristic is host-independent and reliable in
/// practice.
fn isCompilerOwnedBuiltinSourcePath(gpa: Allocator, io: std.Io, filepath: []const u8) bool {
    if (!std.mem.eql(u8, std.fs.path.basename(filepath), "Builtin.roc")) return false;

    const max_source_size = 256 * 1024 * 1024; // 256 MB
    const source = std.Io.Dir.cwd().readFileAlloc(io, filepath, gpa, .limited(max_source_size)) catch return false;
    defer gpa.free(source);

    return std.mem.find(u8, source, "ProvidedByCompiler") != null and
        std.mem.find(u8, source, "Str ::") != null;
}

/// Check a Roc file using BuildEnv and preserve the BuildEnv for further processing
fn checkFileWithBuildEnvPreserved(
    ctx: *CliCtx,
    filepath: []const u8,
    main_filepath: ?[]const u8,
    root_source_url: ?[]const u8,
    main_source_url: ?[]const u8,
    _: bool,
    cache_config: CacheConfig,
    max_threads: ?usize,
    resolution_config: compile.package_resolution.Config,
    source_dir_override: ?[]const u8,
    track_watch_inputs: bool,
    synthetic_default_app: bool,
) CheckFileWithBuildEnvPreservedError!CheckResultWithBuildEnv {
    const trace = tracy.trace(@src());
    defer trace.end();

    // Note: no defer build_env.deinit() here because the env is returned;
    // the caller owns its lifetime (and the cache manager it carries).
    var build_env = try initCliBuildEnv(ctx, .{
        .max_threads = max_threads,
        .no_cache = !cache_config.enabled,
        .verbose_cache = cache_config.verbose,
        .resolution_config = resolution_config,
        .track_watch_inputs = track_watch_inputs,
        .synthetic_default_app = synthetic_default_app,
        .source_dir_override = source_dir_override,
        .builtin_role_path = filepath,
        .root_source_url = root_source_url,
        .main_source_url = main_source_url,
    });

    buildForCheckWithOptionalMain(&build_env, filepath, main_filepath) catch |err| {
        switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            else => {},
        }

        const drained = build_env.drainReports() catch &[_]BuildEnv.DrainedModuleReports{};

        var error_count: u32 = 0;
        var warning_count: u32 = 0;
        for (drained) |mod| {
            for (mod.reports) |report| {
                switch (report.severity) {
                    .info => {},
                    .runtime_error, .fatal => error_count += 1,
                    .warning => warning_count += 1,
                }
            }
        }

        var reports = try ctx.gpa.alloc(DrainedReport, drained.len);
        for (drained, 0..) |mod, i| {
            reports[i] = .{
                .file_path = try ctx.gpa.dupe(u8, mod.abs_path),
                .reports = mod.reports,
            };
        }
        build_env.freeDrainedReportsPathsOnly(drained);

        const timing = if (builtin.target.cpu.arch == .wasm32)
            CheckTimingInfo{}
        else
            build_env.getTimingInfo();
        const cache_stats = build_env.getBuildStats();

        return CheckResultWithBuildEnv{
            .check_result = .{
                .reports = reports,
                .timing = timing,
                .error_count = error_count,
                .warning_count = warning_count,
                .modules_total = cache_stats.modules_total,
                .cache_hits = cache_stats.cache_hits,
                .cache_misses = cache_stats.cache_misses,
                .modules_compiled = cache_stats.modules_compiled,
                .module_time_min_ns = cache_stats.module_time_min_ns,
                .module_time_max_ns = cache_stats.module_time_max_ns,
                .module_time_sum_ns = cache_stats.module_time_sum_ns,
            },
            .build_env = build_env,
        };
    };

    // Force processing to ensure canonicalization happens
    var sched_iter = build_env.schedulers.iterator();
    if (sched_iter.next()) |sched_entry| {
        const package_env = sched_entry.value_ptr.*;
        if (package_env.modules.items.len > 0) {
            const module_name = package_env.modules.items[0].name;

            // Keep processing until the module is done
            var max_iterations: u32 = 20;
            while (max_iterations > 0) : (max_iterations -= 1) {
                const phase = package_env.modules.items[0].phase;
                if (phase == .Done) break;

                package_env.processModuleByName(module_name) catch |err| switch (err) {
                    error.OutOfMemory => return error.OutOfMemory,
                    else => break,
                };
            }
        }
    }

    // Drain all reports
    const drained = try build_env.drainReports();

    // Count errors and warnings
    var error_count: u32 = 0;
    var warning_count: u32 = 0;

    for (drained) |mod| {
        for (mod.reports) |report| {
            switch (report.severity) {
                .info => {},
                .runtime_error, .fatal => error_count += 1,
                .warning => warning_count += 1,
            }
        }
    }

    // Convert BuildEnv drained reports to our format
    var reports = try ctx.gpa.alloc(DrainedReport, drained.len);
    for (drained, 0..) |mod, i| {
        reports[i] = .{
            .file_path = try ctx.gpa.dupe(u8, mod.abs_path),
            .reports = mod.reports, // Transfer ownership
        };
    }

    // Free the original drained reports (abs_path strings and outer slice only)
    // Note: reports ownership was transferred above, abs_path was duped
    build_env.freeDrainedReportsPathsOnly(drained);

    // Get timing information from BuildEnv
    const timing = if (builtin.target.cpu.arch == .wasm32)
        CheckTimingInfo{}
    else
        build_env.getTimingInfo();
    const cache_stats = build_env.getBuildStats();

    const check_result = CheckResult{
        .reports = reports,
        .timing = timing,
        .error_count = error_count,
        .warning_count = warning_count,
        .modules_total = cache_stats.modules_total,
        .cache_hits = cache_stats.cache_hits,
        .cache_misses = cache_stats.cache_misses,
        .modules_compiled = cache_stats.modules_compiled,
        .module_time_min_ns = cache_stats.module_time_min_ns,
        .module_time_max_ns = cache_stats.module_time_max_ns,
        .module_time_sum_ns = cache_stats.module_time_sum_ns,
    };

    return CheckResultWithBuildEnv{
        .check_result = check_result,
        .build_env = build_env,
    };
}

/// Check a Roc file using the ordinary BuildEnv path.
fn checkFileWithBuildEnv(
    ctx: *CliCtx,
    filepath: []const u8,
    main_filepath: ?[]const u8,
    root_source_url: ?[]const u8,
    main_source_url: ?[]const u8,
    _: bool,
    cache_config: CacheConfig,
    max_threads: ?usize,
    resolution_config: compile.package_resolution.Config,
    source_dir_override: ?[]const u8,
    synthetic_default_app: bool,
) CheckFileWithBuildEnvPreservedError!CheckResult {
    const trace = tracy.trace(@src());
    defer trace.end();

    var build_env = try initCliBuildEnv(ctx, .{
        .max_threads = max_threads,
        .no_cache = !cache_config.enabled,
        .verbose_cache = cache_config.verbose,
        .resolution_config = resolution_config,
        .source_dir_override = source_dir_override,
        .synthetic_default_app = synthetic_default_app,
        .root_source_url = root_source_url,
        .main_source_url = main_source_url,
        // Checking is not complete until the platform/app relation output
        // completes, so `roc check` finalizes the relation-bearing platform
        // root once (which also resolves the platform target config constants
        // the check flow depends on).
        .post_check_publication_mode = .executable_artifacts,
        .builtin_role_path = filepath,
    });
    defer build_env.deinit();

    buildForCheckWithOptionalMain(&build_env, filepath, main_filepath) catch |err| {
        switch (err) {
            error.OutOfMemory => return error.OutOfMemory,
            else => {},
        }

        const drained = build_env.drainReports() catch &[_]BuildEnv.DrainedModuleReports{};
        defer build_env.freeDrainedReportsPathsOnly(drained);

        var error_count: u32 = 0;
        var warning_count: u32 = 0;
        for (drained) |mod| {
            for (mod.reports) |report| {
                switch (report.severity) {
                    .info => {},
                    .runtime_error, .fatal => error_count += 1,
                    .warning => warning_count += 1,
                }
            }
        }

        var reports = try ctx.gpa.alloc(DrainedReport, drained.len);
        for (drained, 0..) |mod, i| {
            reports[i] = .{
                .file_path = try ctx.gpa.dupe(u8, mod.abs_path),
                .reports = mod.reports,
            };
        }

        const cache_stats = build_env.getBuildStats();
        return CheckResult{
            .reports = reports,
            .error_count = error_count,
            .warning_count = warning_count,
            .modules_total = cache_stats.modules_total,
            .cache_hits = cache_stats.cache_hits,
            .cache_misses = cache_stats.cache_misses,
            .modules_compiled = cache_stats.modules_compiled,
            .module_time_min_ns = cache_stats.module_time_min_ns,
            .module_time_max_ns = cache_stats.module_time_max_ns,
            .module_time_sum_ns = cache_stats.module_time_sum_ns,
        };
    };

    const drained = try build_env.drainReports();
    defer build_env.freeDrainedReportsPathsOnly(drained);

    var error_count: u32 = 0;
    var warning_count: u32 = 0;
    for (drained) |mod| {
        for (mod.reports) |report| {
            switch (report.severity) {
                .info => {},
                .runtime_error, .fatal => error_count += 1,
                .warning => warning_count += 1,
            }
        }
    }

    var reports = try ctx.gpa.alloc(DrainedReport, drained.len);
    for (drained, 0..) |mod, i| {
        reports[i] = .{
            .file_path = try ctx.gpa.dupe(u8, mod.abs_path),
            .reports = mod.reports,
        };
    }

    const timing = if (builtin.target.cpu.arch == .wasm32)
        CheckTimingInfo{}
    else
        build_env.getTimingInfo();
    const cache_stats = build_env.getBuildStats();

    return CheckResult{
        .reports = reports,
        .timing = timing,
        .error_count = error_count,
        .warning_count = warning_count,
        .modules_total = cache_stats.modules_total,
        .cache_hits = cache_stats.cache_hits,
        .cache_misses = cache_stats.cache_misses,
        .modules_compiled = cache_stats.modules_compiled,
        .module_time_min_ns = cache_stats.module_time_min_ns,
        .module_time_max_ns = cache_stats.module_time_max_ns,
        .module_time_sum_ns = cache_stats.module_time_sum_ns,
    };
}

fn finishRocCheck(
    ctx: *CliCtx,
    args: cli_args.CheckArgs,
    stdout: *std.Io.Writer,
    stderr: *std.Io.Writer,
    timer_start_ns: i128,
    check_result: *CheckResult,
) RocCheckError!void {
    const elapsed = @as(u64, @intCast(std.Io.Timestamp.now(ctx.io.std_io, .real).nanoseconds - timer_start_ns));

    for (check_result.reports) |module| {
        for (module.reports) |*report| {
            try reporting.renderReportToTerminal(report, stderr, ColorPalette.ANSI, ctx.terminalReportConfig());
        }
    }

    ctx.io.flush();

    if (check_result.error_count > 0 or check_result.warning_count > 0) {
        stderr.writeAll("\n") catch {};
        stderr.print("Found {} error(s) and {} warning(s) in ", .{
            check_result.error_count,
            check_result.warning_count,
        }) catch {};
        formatElapsedTimeMs(stderr, elapsed) catch {};
        stderr.print(" for {s}.\n", .{args.path}) catch {};

        if (args.verbose) {
            printVerboseStats(stderr, check_result);
        }

        ctx.io.flush();

        if (check_result.error_count > 0) {
            return error.CheckFailed;
        } else {
            exitOnWarnings(ctx, check_result.warning_count);
        }
    } else {
        stdout.print("No errors found in ", .{}) catch {};
        formatElapsedTimeMs(stdout, elapsed) catch {};
        stdout.print(" for {s}\n", .{args.path}) catch {};

        if (args.verbose) {
            printVerboseStats(stdout, check_result);
        }

        ctx.io.flush();
    }

    if (args.time) {
        printTimingBreakdown(stdout, if (builtin.target.cpu.arch == .wasm32) null else check_result.timing);
    }
}

const DefaultAppCheckSourceFiles = struct {
    app_path: []const u8,
    header_lines: u32,
};

fn writeDefaultAppCheckSourceFiles(
    ctx: *CliCtx,
    temp_dir: []const u8,
    original_path: []const u8,
    original_source: []const u8,
) (Allocator.Error || std.Io.Dir.CreateDirPathError || std.Io.Dir.WriteFileError)!DefaultAppCheckSourceFiles {
    const platform_dir = try std.fs.path.join(ctx.arena, &.{ temp_dir, ".roc_echo_platform" });
    try std.Io.Dir.cwd().createDirPath(ctx.io.std_io, platform_dir);

    const app_filename = std.fs.path.basename(original_path);
    const app_path = try std.fs.path.join(ctx.arena, &.{ temp_dir, app_filename });
    const platform_main_path = try std.fs.path.join(ctx.arena, &.{ platform_dir, "main.roc" });
    const echo_module_path = try std.fs.path.join(ctx.arena, &.{ platform_dir, "Echo.roc" });

    const synthetic_source = try std.mem.concat(ctx.gpa, u8, &.{ default_app_run_header, original_source });
    defer ctx.gpa.free(synthetic_source);

    try std.Io.Dir.cwd().writeFile(ctx.io.std_io, .{ .sub_path = app_path, .data = synthetic_source });
    try std.Io.Dir.cwd().writeFile(ctx.io.std_io, .{ .sub_path = platform_main_path, .data = echo_platform.platform_main_source });
    try std.Io.Dir.cwd().writeFile(ctx.io.std_io, .{ .sub_path = echo_module_path, .data = echo_platform.echo_module_source });

    return .{
        .app_path = app_path,
        .header_lines = countNewlines(default_app_run_header),
    };
}

fn rocCheckDefaultApp(
    ctx: *CliCtx,
    args: cli_args.CheckArgs,
    original_source: []const u8,
    cache_config: CacheConfig,
) RocCheckError!CheckResult {
    defer ctx.gpa.free(original_source);

    const temp_dir = createUniqueTempDir(ctx) catch |err| {
        return ctx.fail(.{ .temp_dir_failed = .{ .err = err } });
    };
    defer std.Io.Dir.cwd().deleteTree(ctx.io.std_io, temp_dir) catch {};

    const files = try writeDefaultAppCheckSourceFiles(ctx, temp_dir, args.path, original_source);
    const original_source_dir = std.fs.path.dirname(args.path) orelse ".";

    var check_result = try checkFileWithBuildEnv(
        ctx,
        files.app_path,
        null,
        null,
        null,
        args.time,
        cache_config,
        args.max_threads,
        resolutionConfigFromLimits(args.resolve_limits),
        original_source_dir,
        true,
    );
    errdefer check_result.deinit(ctx.gpa);

    try remapDefaultAppCheckReports(
        ctx,
        &check_result,
        files.app_path,
        args.path,
        original_source,
        files.header_lines,
    );

    return check_result;
}

const DefaultAppCheckResultWithBuildEnv = struct {
    result_with_env: CheckResultWithBuildEnv,
    synthetic_app_path: []const u8,

    fn deinit(self: *@This(), gpa: Allocator) void {
        self.result_with_env.deinit(gpa);
    }
};

fn rocCheckDefaultAppPreserved(
    ctx: *CliCtx,
    args: cli_args.CheckArgs,
    original_source: []const u8,
    cache_config: CacheConfig,
    track_watch_inputs: bool,
) RocCheckError!DefaultAppCheckResultWithBuildEnv {
    defer ctx.gpa.free(original_source);

    const temp_dir = createUniqueTempDir(ctx) catch |err| {
        return ctx.fail(.{ .temp_dir_failed = .{ .err = err } });
    };
    defer std.Io.Dir.cwd().deleteTree(ctx.io.std_io, temp_dir) catch {};

    const files = try writeDefaultAppCheckSourceFiles(ctx, temp_dir, args.path, original_source);
    const original_source_dir = std.fs.path.dirname(args.path) orelse ".";

    var result_with_env = try checkFileWithBuildEnvPreserved(
        ctx,
        files.app_path,
        null,
        null,
        null,
        args.time,
        cache_config,
        args.max_threads,
        resolutionConfigFromLimits(args.resolve_limits),
        original_source_dir,
        track_watch_inputs,
        true,
    );
    errdefer result_with_env.deinit(ctx.gpa);

    try remapDefaultAppCheckReports(
        ctx,
        &result_with_env.check_result,
        files.app_path,
        args.path,
        original_source,
        files.header_lines,
    );

    return .{
        .result_with_env = result_with_env,
        .synthetic_app_path = files.app_path,
    };
}

fn writeDefaultAppCheckWatchInputs(
    ctx: *CliCtx,
    file_path: []const u8,
    result: *DefaultAppCheckResultWithBuildEnv,
) WatchWriteInputsError!void {
    var input_set = try collectSyntheticBuildWatchInputSet(ctx, &result.result_with_env.build_env, result.synthetic_app_path);
    defer input_set.deinit(ctx);
    try writeWatchInputSetFile(ctx, file_path, &input_set);
}

fn rocCheck(ctx: *CliCtx, args_in: cli_args.CheckArgs, arg0: []const u8) RocCheckError!void {
    const trace = tracy.trace(@src());
    defer trace.end();

    var args = args_in;
    const resolved_source = try resolveSourceArg(ctx, args_in.path, args_in.watch);
    args.path = resolved_source.path;
    args.root_source_url = resolved_source.url;
    if (args_in.main) |main_path| {
        const resolved_main = try resolveSourceArg(ctx, main_path, args_in.watch);
        args.main = resolved_main.path;
        args.main_source_url = resolved_main.url;
    }

    if (args.watch) {
        return runWatchCommand(ctx, arg0, .{ .check = args });
    }

    const stdout = ctx.io.stdout();
    const stderr = ctx.io.stderr();

    const timer_start_ns = std.Io.Timestamp.now(ctx.io.std_io, .real).nanoseconds;

    var reporter = makeReporter(ctx, "roc check", args.timings);
    defer reporter.deinit();
    reporter.start();

    // Set up cache configuration based on command line args
    const cache_config = CacheConfig{
        .enabled = !args.no_cache,
        .verbose = args.verbose,
        .roc_ctx = ctx.coreCtx(),
    };

    // Use BuildEnv to check the file
    reporter.begin("Type Checking");
    if (args.watch_inputs_file) |file_path| {
        var extra_buf: [2][]const u8 = undefined;
        const extra_paths = appendExtraWatchPaths(.{ .check = args }, &extra_buf);

        if (try readDefaultAppSource(ctx, args.path)) |source| {
            var default_result = rocCheckDefaultAppPreserved(
                ctx,
                args,
                source,
                cache_config,
                true,
            ) catch |err| {
                reporter.fail();
                try writeWatchInputsFile(ctx, file_path, null, extra_paths);
                return handleProcessFileError(err, stderr, args.path);
            };
            defer default_result.deinit(ctx.gpa);
            const check_result = &default_result.result_with_env.check_result;

            if (builtin.target.cpu.arch == .wasm32) {
                reporter.end();
            } else {
                reporter.endWithBreakdown(&frontEndBreakdown(check_result.timing));
            }
            reporter.finish();

            try writeDefaultAppCheckWatchInputs(ctx, file_path, &default_result);
            return finishRocCheck(ctx, args, stdout, stderr, timer_start_ns, check_result);
        }

        var result_with_env = checkFileWithBuildEnvPreserved(
            ctx,
            args.path,
            args.main,
            args.root_source_url,
            args.main_source_url,
            args.time,
            cache_config,
            args.max_threads,
            resolutionConfigFromLimits(args.resolve_limits),
            null,
            true,
            false,
        ) catch |err| {
            reporter.fail();
            try writeWatchInputsFile(ctx, file_path, null, extra_paths);
            return handleProcessFileError(err, stderr, args.path);
        };
        defer result_with_env.deinit(ctx.gpa);
        const check_result = &result_with_env.check_result;

        if (builtin.target.cpu.arch == .wasm32) {
            reporter.end();
        } else {
            reporter.endWithBreakdown(&frontEndBreakdown(check_result.timing));
        }
        reporter.finish();

        try writeWatchInputsFile(ctx, file_path, &result_with_env.build_env, extra_paths);
        return finishRocCheck(ctx, args, stdout, stderr, timer_start_ns, check_result);
    }

    var check_result = if (try readDefaultAppSource(ctx, args.path)) |source|
        rocCheckDefaultApp(ctx, args, source, cache_config) catch |err| {
            reporter.fail();
            return handleProcessFileError(err, stderr, args.path);
        }
    else
        checkFileWithBuildEnv(
            ctx,
            args.path,
            args.main,
            args.root_source_url,
            args.main_source_url,
            args.time,
            cache_config,
            args.max_threads,
            resolutionConfigFromLimits(args.resolve_limits),
            null,
            false,
        ) catch |err| {
            reporter.fail();
            return handleProcessFileError(err, stderr, args.path);
        };
    defer check_result.deinit(ctx.gpa);

    if (builtin.target.cpu.arch == .wasm32) {
        reporter.end();
    } else {
        reporter.endWithBreakdown(&frontEndBreakdown(check_result.timing));
    }
    reporter.finish();

    return finishRocCheck(ctx, args, stdout, stderr, timer_start_ns, &check_result);
}

fn printTimingBreakdown(writer: anytype, timing: ?CheckTimingInfo) void {
    if (timing) |t| {
        writer.print("\nTiming breakdown:", .{}) catch {};
        writer.print("  tokenize + parse:             ", .{}) catch {};
        formatElapsedTime(writer, t.tokenize_parse_ns) catch {};
        writer.print("  ({} ns)", .{t.tokenize_parse_ns}) catch {};
        writer.print("  canonicalize:                 ", .{}) catch {};
        formatElapsedTime(writer, t.canonicalize_ns) catch {};
        writer.print("  ({} ns)", .{t.canonicalize_ns}) catch {};
        writer.print("  can diagnostics:              ", .{}) catch {};
        formatElapsedTime(writer, t.canonicalize_diagnostics_ns) catch {};
        writer.print("  ({} ns)", .{t.canonicalize_diagnostics_ns}) catch {};
        writer.print("  type checking:                ", .{}) catch {};
        formatElapsedTime(writer, t.type_checking_ns) catch {};
        writer.print("  ({} ns)", .{t.type_checking_ns}) catch {};
        writer.print("  type checking diagnostics:    ", .{}) catch {};
        formatElapsedTime(writer, t.check_diagnostics_ns) catch {};
        writer.print("  ({} ns)", .{t.check_diagnostics_ns}) catch {};
    }
}

/// Print verbose build statistics when --verbose flag is passed
/// Format:
///     Modules: 6 total, 4 cached, 2 built
///     Cache Hit: 67%
///     Build: 8ms / 14ms / 25ms (min / avg / max)
fn printVerboseStats(writer: anytype, result: *const CheckResult) void {
    const total = result.modules_total;
    if (total == 0) return;

    const cache_percent = cacheHitPercent(result.cache_hits, result.cache_misses);

    // Print modules breakdown
    writer.print("\n    Modules: {} total, {} cached, {} built\n", .{
        total,
        result.cache_hits,
        result.modules_compiled,
    }) catch {};

    // Print cache hit percentage
    writer.print("    Cache Hit: {}%\n", .{cache_percent}) catch {};

    // Print build time breakdown (only if we have compiled modules)
    if (result.modules_compiled > 0) {
        const min_ms = nsToMs(result.module_time_min_ns);
        const avg_ms = nsToMs(moduleTimeAvgNs(result.module_time_sum_ns, result.modules_compiled));
        const max_ms = nsToMs(result.module_time_max_ns);
        writer.print("    Build: {}ms / {}ms / {}ms (min / avg / max)\n", .{
            min_ms,
            avg_ms,
            max_ms,
        }) catch {};
    }
}

/// Start an HTTP server to serve the generated documentation.
///
/// Single-threaded blocking accept loop on 127.0.0.1:8080. GET-only.
/// Files are streamed up to a 10 MB cap; anything larger returns 500.
fn serveDocumentation(ctx: *CliCtx, docs_dir: []const u8) CliMainError!void {
    const stdout = ctx.io.stdout();
    const io = ctx.io.std_io;

    var address = try std.Io.net.IpAddress.parse("127.0.0.1", 8080);
    var server = try address.listen(io, .{ .reuse_address = true });
    defer server.deinit(io);

    stdout.print("Visit http://localhost:8080 to view the docs at ./{s}/\n", .{docs_dir}) catch {};
    stdout.print("Press Ctrl+C to stop the server\n", .{}) catch {};
    ctx.io.flush();

    while (true) {
        const stream = server.accept(io) catch |err| {
            ctx.io.stderr().print("Error accepting connection: {}\n", .{err}) catch {};
            ctx.io.flush();
            continue;
        };
        handleConnection(ctx, stream, docs_dir) catch |err| {
            ctx.io.stderr().print("Error handling connection: {}\n", .{err}) catch {};
            ctx.io.flush();
        };
    }
}

/// Handle a single HTTP connection. Closes the stream before returning.
fn handleConnection(ctx: *CliCtx, stream: std.Io.net.Stream, docs_dir: []const u8) CliMainError!void {
    const io = ctx.io.std_io;
    defer stream.close(io);

    var read_buffer: [4096]u8 = undefined;
    var conn_reader = stream.reader(io, &read_buffer);

    // Read whatever the client has sent so far (we only care about the
    // request line; the body of a GET is empty).
    var request_buf: [4096]u8 = undefined;
    var slices = [_][]u8{request_buf[0..]};
    const bytes_read = std.Io.Reader.readVec(&conn_reader.interface, &slices) catch |err| switch (err) {
        error.EndOfStream => 0,
        error.ReadFailed => return conn_reader.err orelse error.Unexpected,
    };
    if (bytes_read == 0) return;

    const request = request_buf[0..bytes_read];

    // Parse the request line: "METHOD PATH HTTP/x.y\r\n..."
    var lines = std.mem.splitSequence(u8, request, "\r\n");
    const request_line = lines.next() orelse return;

    var parts = std.mem.splitSequence(u8, request_line, " ");
    const method = parts.next() orelse return;
    const path = parts.next() orelse return;

    if (!std.mem.eql(u8, method, "GET")) {
        try sendResponse(io, stream, "405 Method Not Allowed", "text/plain", "Method Not Allowed");
        return;
    }

    // Resolve the URL path to a filesystem path under docs_dir.
    const file_path = try resolveFilePath(ctx.gpa, docs_dir, path);
    defer ctx.gpa.free(file_path);

    // Read the file (10 MB cap per response).
    const file_content = std.Io.Dir.cwd().readFileAlloc(io, file_path, ctx.gpa, .limited(10 * 1024 * 1024)) catch |err| {
        switch (err) {
            error.FileNotFound => try sendResponse(io, stream, "404 Not Found", "text/plain", "File Not Found"),
            else => try sendResponse(io, stream, "500 Internal Server Error", "text/plain", "Internal Server Error"),
        }
        return;
    };
    defer ctx.gpa.free(file_content);

    const content_type = getContentType(file_path);
    try sendResponse(io, stream, "200 OK", content_type, file_content);
}

/// Resolve the URL path against `docs_dir`, expanding directory paths to
/// their `index.html`. Caller owns the returned slice.
fn resolveFilePath(gpa: Allocator, docs_dir: []const u8, url_path: []const u8) Allocator.Error![]const u8 {
    const clean_path = if (url_path.len > 0 and url_path[0] == '/')
        url_path[1..]
    else
        url_path;

    // Empty or trailing-slash paths serve the directory's index.html.
    if (clean_path.len == 0 or clean_path[clean_path.len - 1] == '/') {
        return try std.fmt.allocPrint(gpa, "{s}/{s}index.html", .{ docs_dir, clean_path });
    }

    // If the last path component has an extension, serve it directly;
    // otherwise treat the path as a directory and serve its index.html.
    const last_slash = std.mem.findScalarLast(u8, clean_path, '/') orelse 0;
    const last_component = clean_path[last_slash..];
    const has_extension = std.mem.findScalar(u8, last_component, '.') != null;

    if (has_extension) {
        return try std.fmt.allocPrint(gpa, "{s}/{s}", .{ docs_dir, clean_path });
    } else {
        return try std.fmt.allocPrint(gpa, "{s}/{s}/index.html", .{ docs_dir, clean_path });
    }
}

/// Map a file extension to its HTTP Content-Type.
fn getContentType(file_path: []const u8) []const u8 {
    if (std.mem.endsWith(u8, file_path, ".html")) {
        return "text/html; charset=utf-8";
    } else if (std.mem.endsWith(u8, file_path, ".css")) {
        return "text/css";
    } else if (std.mem.endsWith(u8, file_path, ".js")) {
        return "application/javascript";
    } else if (std.mem.endsWith(u8, file_path, ".json")) {
        return "application/json";
    } else if (std.mem.endsWith(u8, file_path, ".png")) {
        return "image/png";
    } else if (std.mem.endsWith(u8, file_path, ".jpg") or std.mem.endsWith(u8, file_path, ".jpeg")) {
        return "image/jpeg";
    } else if (std.mem.endsWith(u8, file_path, ".svg")) {
        return "image/svg+xml";
    } else {
        return "text/plain";
    }
}

/// Send a full HTTP/1.1 response (headers + body) over a stream.
fn sendResponse(
    io: std.Io,
    stream: std.Io.net.Stream,
    status: []const u8,
    content_type: []const u8,
    body: []const u8,
) (Allocator.Error || error{WriteFailed})!void {
    var write_buffer: [4096]u8 = undefined;
    var stream_writer = stream.writer(io, &write_buffer);
    const w = &stream_writer.interface;

    try w.print(
        "HTTP/1.1 {s}\r\n" ++
            "Content-Type: {s}\r\n" ++
            "Content-Length: {d}\r\n" ++
            "Connection: close\r\n" ++
            "\r\n",
        .{ status, content_type, body.len },
    );
    try w.writeAll(body);
    try w.flush();
}

fn rocBump(ctx: *CliCtx, args_in: cli_args.BumpArgs) CliMainError!void {
    var args = args_in;
    const resolved_source = try resolveSourceArg(ctx, args_in.path, false);
    args.path = resolved_source.path;
    args.root_source_url = resolved_source.url;

    const stdout = ctx.io.stdout();

    // Resolve the old package's version, from --old-version or the URL.
    var old_version: ?base.url.Version = null;
    if (args.old_version) |raw| {
        const version = base.url.parseVersionComponent(raw) orelse {
            return ctx.fail(.{ .bump_failed = .{
                .title = "Invalid Old Version",
                .message = try std.fmt.allocPrint(ctx.arena, "`{s}` is not a valid version. Versions are MAJOR.MINOR.PATCH, e.g. 1.2.3.", .{raw}),
            } });
        };
        if (!version.isPresent()) {
            return ctx.fail(.{ .bump_failed = .{
                .title = "Invalid Old Version",
                .message = "The version 0.0.0 is reserved to mean \"no version\". The lowest publishable version is 0.0.1.",
            } });
        }
        old_version = version;
    }

    // Parse --expect up front so a malformed version fails before compiling.
    var expect_version: ?base.url.Version = null;
    if (args.expect) |raw| {
        expect_version = base.url.parseVersionComponent(raw) orelse {
            return ctx.fail(.{ .bump_failed = .{
                .title = "Invalid Expected Version",
                .message = try std.fmt.allocPrint(ctx.arena, "`{s}` is not a valid version. Versions are MAJOR.MINOR.PATCH, e.g. 1.2.3.", .{raw}),
            } });
        };
    }

    // Resolve the old package source to a local main.roc path.
    const old_path: []const u8 = blk: {
        if (std.mem.find(u8, args.old, "://") != null) {
            if (old_version == null) {
                if (base.url.parseUrlPath(args.old)) |parsed| {
                    if (parsed.version.isPresent()) old_version = parsed.version;
                } else |_| {}
            }
            const resolved = try resolveUrlBundle(ctx, args.old);
            break :blk resolved.source_path;
        }
        if (std.mem.endsWith(u8, args.old, ".tar.zst")) {
            break :blk try extractBundleForBump(ctx, args.old);
        }
        if (std.mem.endsWith(u8, args.old, ".roc")) break :blk args.old;
        break :blk try std.fs.path.join(ctx.arena, &.{ args.old, "main.roc" });
    };

    const old_version_value = old_version orelse {
        return ctx.fail(.{ .bump_failed = .{
            .title = "Missing Old Version",
            .message = "The old package's version could not be determined. Pass it with --old-version <MAJOR.MINOR.PATCH>; it can only be inferred when --old is a URL with a version path segment.",
        } });
    };

    const cache_config = CacheConfig{
        .enabled = !args.no_cache,
        .verbose = args.verbose,
        .roc_ctx = ctx.coreCtx(),
    };

    var old_result = try bumpCheckSide(ctx, old_path, null, cache_config, args, "old");
    defer old_result.deinit(ctx.gpa);
    var new_result = try bumpCheckSide(ctx, args.path, args.root_source_url, cache_config, args, "new");
    defer new_result.deinit(ctx.gpa);

    var old_api = try bumpExtractApi(ctx, &old_result.build_env, "old");
    defer old_api.deinit();
    var new_api = try bumpExtractApi(ctx, &new_result.build_env, "new");
    defer new_api.deinit();

    var result = try bump.diff.diff(ctx.gpa, &old_api, &new_api);
    defer result.deinit();

    stdout.print("Comparing {s} (old, {f}) with {s} (new)...\n", .{ args.old, old_version_value, args.path }) catch {};

    if (result.changes.len == 0) {
        stdout.print("\nNo API changes detected.\n", .{}) catch {};
    } else {
        // Changes arrive grouped by module (the differ merge-walks sorted
        // module lists), so a simple current-module tracker renders sections.
        var current_module: []const u8 = "";
        for (result.changes) |change| {
            if (!std.mem.eql(u8, change.module, current_module)) {
                current_module = change.module;
                var module_magnitude = bump.diff.Magnitude.patch;
                for (result.changes) |other| {
                    if (std.mem.eql(u8, other.module, change.module)) {
                        module_magnitude = module_magnitude.combine(other.magnitude);
                    }
                }
                stdout.print("\n---- {s} - {s} ----\n\n", .{ change.module, module_magnitude.name() }) catch {};
            }
            switch (change.kind) {
                .module_added => stdout.print("    Added module\n", .{}) catch {},
                .module_removed => stdout.print("    Removed module\n", .{}) catch {},
                .item_added => stdout.print("    + {s} : {s}\n", .{ change.path, change.new_rendered orelse "" }) catch {},
                .item_removed => stdout.print("    - {s} : {s}\n", .{ change.path, change.old_rendered orelse "" }) catch {},
                .item_changed => {
                    stdout.print("    - {s} : {s}\n", .{ change.path, change.old_rendered orelse "" }) catch {};
                    stdout.print("    + {s} : {s}\n", .{ change.path, change.new_rendered orelse "" }) catch {};
                },
            }
        }
    }

    const next = bump.diff.nextVersion(old_version_value, result.magnitude);
    stdout.print("\nThis is a {s} change.\n", .{result.magnitude.name()}) catch {};
    stdout.print("\n{f} -> {f}\n", .{ old_version_value, next }) catch {};
    if (old_version_value.major == 0) {
        stdout.print("\n(Pre-1.0.0 versions are 0.X.Y: breaking changes bump X, everything else bumps Y.)\n", .{}) catch {};
    }

    // With --expect, fail unless the declared version bumps at least as far
    // as the API diff requires. Bumping further than required is allowed.
    if (expect_version) |expected| {
        const declared = bump.diff.declaredMagnitude(old_version_value, expected) orelse {
            return ctx.fail(.{ .bump_failed = .{
                .title = "Insufficient Version Bump",
                .message = try std.fmt.allocPrint(ctx.arena, "The expected version {f} does not move forward from {f}.", .{ expected, old_version_value }),
            } });
        };
        if (@intFromEnum(declared) < @intFromEnum(result.magnitude)) {
            return ctx.fail(.{ .bump_failed = .{
                .title = "Insufficient Version Bump",
                .message = try std.fmt.allocPrint(
                    ctx.arena,
                    "This is a {s} change, so the next version must be at least {f}, but --expect was {f}.",
                    .{ result.magnitude.name(), next, expected },
                ),
            } });
        }
        stdout.print("\n{f} satisfies the required {s} bump.\n", .{ expected, result.magnitude.name() }) catch {};
    }
}

/// Check one side of a bump comparison, keeping its BuildEnv alive so the
/// public API can be extracted from the checked artifacts afterwards.
fn bumpCheckSide(
    ctx: *CliCtx,
    path: []const u8,
    root_source_url: ?[]const u8,
    cache_config: CacheConfig,
    args: cli_args.BumpArgs,
    side: []const u8,
) CliMainError!CheckResultWithBuildEnv {
    const stderr = ctx.io.stderr();
    var result = checkFileWithBuildEnvPreserved(
        ctx,
        path,
        null,
        root_source_url,
        null,
        false,
        cache_config,
        null,
        resolutionConfigFromLimits(args.resolve_limits),
        null,
        false,
        false,
    ) catch |err| {
        try handleProcessFileError(err, stderr, path);
        return error.CliError;
    };
    errdefer result.deinit(ctx.gpa);

    for (result.check_result.reports) |module| {
        for (module.reports) |*report| {
            reporting.renderReportToTerminal(report, stderr, ColorPalette.ANSI, ctx.terminalReportConfig()) catch {};
        }
    }
    if (result.check_result.error_count > 0) {
        return ctx.fail(.{ .bump_failed = .{
            .title = "Package Does Not Compile",
            .message = try std.fmt.allocPrint(
                ctx.arena,
                "The {s} package ({s}) does not compile with this compiler. roc bump needs both packages to compile so their public APIs can be compared.",
                .{ side, path },
            ),
        } });
    }
    return result;
}

/// Extract a local .tar.zst bundle into the content-addressed package cache
/// (keyed by the hash in its filename) and return the path to its main.roc.
fn extractBundleForBump(ctx: *CliCtx, archive_path: []const u8) CliMainError![]const u8 {
    const basename = std.fs.path.basename(archive_path);
    const hash = basename[0 .. basename.len - ".tar.zst".len];

    const cache_dir_path = getRocCacheDir(ctx.arena) catch {
        return ctx.fail(.{ .cache_dir_unavailable = .{ .reason = "Could not determine cache directory" } });
    };
    const package_dir_path = try std.fs.path.join(ctx.arena, &.{ cache_dir_path, hash });
    const main_roc_path = try std.fs.path.join(ctx.arena, &.{ package_dir_path, "main.roc" });

    // The cache is content-addressed by the archive's hash, so an existing
    // extraction can be reused as-is.
    const already_cached = blk: {
        std.Io.Dir.cwd().access(ctx.io.std_io, main_roc_path, .{}) catch break :blk false;
        break :blk true;
    };
    if (already_cached) return main_roc_path;

    var output_dir = try std.Io.Dir.cwd().createDirPathOpen(ctx.io.std_io, package_dir_path, .{});
    defer output_dir.close(ctx.io.std_io);

    const archive_file = std.Io.Dir.cwd().openFile(ctx.io.std_io, archive_path, .{}) catch {
        return ctx.fail(.{ .file_not_found = .{ .path = archive_path } });
    };
    defer archive_file.close(ctx.io.std_io);

    var error_ctx: unbundle.ErrorContext = undefined;
    var archive_reader_buffer: [4096]u8 = undefined;
    var archive_reader = archive_file.reader(ctx.io.std_io, &archive_reader_buffer);
    unbundle.unbundleFiles(
        ctx.gpa,
        &archive_reader.interface,
        output_dir,
        ctx.io.std_io,
        basename,
        &error_ctx,
    ) catch |err| {
        return ctx.fail(.{ .bump_failed = .{
            .title = "Cannot Extract Old Bundle",
            .message = try std.fmt.allocPrint(ctx.arena, "Failed to extract {s}: {s}.", .{ archive_path, @errorName(err) }),
        } });
    };

    std.Io.Dir.cwd().access(ctx.io.std_io, main_roc_path, .{}) catch {
        return ctx.fail(.{ .bump_failed = .{
            .title = "Cannot Extract Old Bundle",
            .message = try std.fmt.allocPrint(ctx.arena, "The bundle {s} does not contain a main.roc at its root.", .{archive_path}),
        } });
    };
    return main_roc_path;
}

/// Extract the public API of the root package of a finished build.
fn bumpExtractApi(ctx: *CliCtx, build_env: *compile.BuildEnv, side: []const u8) CliMainError!bump.PackageApi {
    const root_name = build_env.discovered_pkg_name orelse return error.Internal;
    const root_pkg = build_env.packages.getPtr(root_name) orelse return error.Internal;

    switch (root_pkg.kind) {
        .package, .platform => {},
        else => return ctx.fail(.{ .bump_failed = .{
            .title = "Not A Package",
            .message = try std.fmt.allocPrint(
                ctx.arena,
                "roc bump compares package APIs, but the {s} module ({s}) has neither a package nor a platform header.",
                .{ side, root_pkg.root_file },
            ),
        } }),
    }

    // Map every compiled module's identity to the package that owns it, so type
    // origins in public signatures resolve to stable package identities.
    var origins = bump.extract.OriginMap{};
    defer origins.deinit(ctx.gpa);
    {
        const builtin_env = build_env.builtin_modules.builtin_module.env;
        const builtin_identity_hash = builtin_env.contentIdentityHash() orelse return error.Internal;
        const builtin_origin = bump.extract.OriginMap.Origin{
            .kind = .builtin,
            .module_name = builtin_env.module_name,
        };
        try origins.putIdentity(ctx.gpa, builtin_identity_hash, builtin_origin);
        try origins.put(ctx.gpa, builtin_env.module_name, builtin_origin);
        try origins.put(ctx.gpa, builtin_env.getIdentText(builtin_env.qualified_module_ident), builtin_origin);

        var sched_iter = build_env.schedulers.iterator();
        while (sched_iter.next()) |sched_entry| {
            const pkg_name = sched_entry.key_ptr.*;
            const origin_kind: bump.extract.OriginMap.Origin.Kind = origin_blk: {
                if (std.mem.eql(u8, pkg_name, root_name)) break :origin_blk .self;
                const pkg = build_env.packages.getPtr(pkg_name) orelse break :origin_blk .{ .unstable = pkg_name };
                if (pkg.url) |*url_source| {
                    const parsed = base.url.parseUrlPath(url_source.url) catch break :origin_blk .{ .unstable = url_source.url };
                    if (!parsed.version.isPresent()) break :origin_blk .{ .unstable = url_source.url };
                    const url_id = try std.fmt.allocPrint(ctx.arena, "{s}{s}", .{
                        parsed.urlIdPrefix(url_source.url),
                        parsed.urlIdSuffix(url_source.url),
                    });
                    break :origin_blk .{ .external = .{
                        .url_id = url_id,
                        .major = parsed.version.major,
                        .minor = parsed.version.minor,
                    } };
                }
                // Path dependencies have no stable published identity.
                break :origin_blk .{ .unstable = pkg.root_file };
            };
            const package_env = sched_entry.value_ptr.*;
            for (package_env.modules.items) |*module_state| {
                if (module_state.moduleEnv()) |mod_env| {
                    // Checked types record origins under the package-qualified
                    // module name; register the bare name too for roots whose
                    // modules are referenced unqualified.
                    const origin = bump.extract.OriginMap.Origin{
                        .kind = origin_kind,
                        .module_name = mod_env.module_name,
                    };
                    const identity_hash = mod_env.contentIdentityHash() orelse return error.Internal;
                    try origins.putIdentity(ctx.gpa, identity_hash, origin);
                    try origins.put(ctx.gpa, mod_env.module_name, origin);
                    try origins.put(ctx.gpa, mod_env.getIdentText(mod_env.qualified_module_ident), origin);
                }
            }
        }
    }

    const public_modules = try build_env.getPublicRootModules(ctx.gpa);
    defer ctx.gpa.free(public_modules);

    var inputs = std.ArrayListUnmanaged(bump.extract.ModuleInput).empty;
    defer inputs.deinit(ctx.gpa);

    for (public_modules) |module| {
        const artifact = module.semantic.checked_artifact orelse return error.Internal;
        try inputs.append(ctx.gpa, .{
            .exposed_name = module.name,
            .module_env = module.semantic.env,
            .artifact = artifact,
        });
    }

    if (inputs.items.len == 0) {
        return ctx.fail(.{ .bump_failed = .{
            .title = "No Exposed Modules",
            .message = try std.fmt.allocPrint(
                ctx.arena,
                "The {s} package does not expose any modules, so there is no public API to compare.",
                .{side},
            ),
        } });
    }

    var extract_failure: ?bump.extract.Failure = null;
    return bump.extract.extractPackageApi(ctx.gpa, inputs.items, &origins, &extract_failure) catch |err| switch (err) {
        error.OutOfMemory => error.OutOfMemory,
        error.ExtractFailed => {
            const info = extract_failure.?;
            defer info.deinit(ctx.gpa);
            const message = switch (info.kind) {
                .unpublished_public_type => try std.fmt.allocPrint(
                    ctx.arena,
                    "Missing checked type data for the public item `{s}` in module `{s}`: {s}.",
                    .{ info.item_path, info.module_name, info.detail },
                ),
                .unknown_origin_module => try std.fmt.allocPrint(
                    ctx.arena,
                    "A public signature of `{s}` in module `{s}` references a type from module `{s}`, which does not belong to any package in this build.",
                    .{ info.item_path, info.module_name, info.detail },
                ),
                .unstable_dependency_in_public_api => try std.fmt.allocPrint(
                    ctx.arena,
                    "The public API (item `{s}` in module `{s}`) exposes a type from an unstable dependency:\n\n    {s}\n\nA publishable package may only expose types from versioned URL dependencies, because consumers need a stable package identity to compare versions against. Either stop exposing this type, or publish the dependency and depend on its URL.",
                    .{ info.item_path, info.module_name, info.detail },
                ),
                .private_type_in_public_api => try std.fmt.allocPrint(
                    ctx.arena,
                    "The {s} package's public API references the type `{s}`, which is not itself part of the exposed API.",
                    .{ side, info.detail },
                ),
            };
            return ctx.fail(.{ .bump_failed = .{ .title = "Cannot Extract Public API", .message = message } });
        },
    };
}

fn rocDocs(ctx: *CliCtx, args_in: cli_args.DocsArgs) CliMainError!void {
    const trace = tracy.trace(@src());
    defer trace.end();

    var args = args_in;
    const resolved_source = try resolveSourceArg(ctx, args_in.path, false);
    args.path = resolved_source.path;
    args.root_source_url = resolved_source.url;
    if (args_in.main) |main_path| {
        const resolved_main = try resolveSourceArg(ctx, main_path, false);
        args.main = resolved_main.path;
        args.main_source_url = resolved_main.url;
    }

    const stdout = ctx.io.stdout();
    const stderr = ctx.io.stderr();

    const timer_start_ns = std.Io.Timestamp.now(ctx.io.std_io, .real).nanoseconds;

    // Set up cache configuration based on command line args
    const cache_config = CacheConfig{
        .enabled = !args.no_cache,
        .verbose = args.verbose,
        .roc_ctx = ctx.coreCtx(),
    };

    // Use BuildEnv to check the file, preserving the BuildEnv for docs generation
    var result_with_env = checkFileWithBuildEnvPreserved(
        ctx,
        args.path,
        args.main,
        args.root_source_url,
        args.main_source_url,
        args.time,
        cache_config,
        null, // max_threads: use default (single-threaded for now)
        resolutionConfigFromLimits(args.resolve_limits),
        null,
        false,
        false,
    ) catch |err| {
        return handleProcessFileError(err, stderr, args.path);
    };

    // Clean up when we're done - this includes the BuildEnv and all module envs
    defer result_with_env.deinit(ctx.gpa);

    const check_result = &result_with_env.check_result;
    const elapsed = @as(u64, @intCast(std.Io.Timestamp.now(ctx.io.std_io, .real).nanoseconds - timer_start_ns));

    // Render reports grouped by module
    for (check_result.reports) |module| {
        for (module.reports) |*report| {

            // Render the diagnostic report to stderr
            reporting.renderReportToTerminal(report, stderr, ColorPalette.ANSI, ctx.terminalReportConfig()) catch |render_err| {
                stderr.print("Error rendering diagnostic report: {}", .{render_err}) catch {};
                // Fallback to just printing the title
                stderr.print("  {s}", .{report.title}) catch {};
            };
        }
    }

    if (check_result.error_count > 0 or check_result.warning_count > 0) {
        stderr.writeAll("\n") catch {};
        stderr.print("Found {} error(s) and {} warning(s) in ", .{
            check_result.error_count,
            check_result.warning_count,
        }) catch {};
        formatElapsedTime(stderr, elapsed) catch {};
        stderr.print(" for {s}.", .{args.path}) catch {};

        if (check_result.error_count > 0) {
            return error.DocsFailed;
        }
    }

    // Print timing breakdown if requested
    if (args.time) {
        printTimingBreakdown(stdout, if (builtin.target.cpu.arch == .wasm32) null else check_result.timing);
    }

    // Generate documentation for all packages and modules
    try generateDocs(ctx, &result_with_env.build_env, args.path, args.output, args.with_lang_ref);

    stdout.print("\nGenerated docs for {s}\n", .{args.path}) catch {};

    // Start HTTP server if --serve flag is enabled
    if (args.serve) {
        try serveDocumentation(ctx, args.output);
    }
}

// Documentation generation uses the docs module's extraction pipeline.
// See src/docs/ for DocModel, extract, and render_type modules.

/// Generate documentation for the root and all its dependencies and imported modules.
///
/// Builds a PackageDocs by extracting documentation from all compiled modules,
/// then generates an HTML documentation site in the output directory.
fn generateDocs(
    ctx: *CliCtx,
    build_env: *compile.BuildEnv,
    module_path: []const u8,
    base_output_dir: []const u8,
    with_lang_ref: bool,
) CliMainError!void {
    const DocModel = docs.DocModel;
    const extract = docs.extract;

    // Collect ModuleDocs from the explicit documentation surface.
    var module_docs_list = std.ArrayList(DocModel.ModuleDocs).empty;
    defer {
        for (module_docs_list.items) |*mod| mod.deinit(ctx.gpa);
        module_docs_list.deinit(ctx.gpa);
    }

    const is_package = build_env.rootIsPackage();

    // Track why docs extraction produced nothing so a zero-module result can
    // explain itself.
    // instead of silently writing an empty docs site.
    var modules_seen: usize = 0;
    var extract_failed: usize = 0;

    const modules = build_env.getDocumentationModules(ctx.gpa) catch |err| {
        std.debug.print("Error: failed to collect documentable modules for '{s}': {}\n", .{ module_path, err });
        return error.DocsFailed;
    };
    defer ctx.gpa.free(modules);

    for (modules) |module_info| {
        // Docs show display names (root alias, or "app"/"module" for the
        // root itself), never internal identity keys (URLs, absolute paths).
        const sched_pkg_name = build_env.displayNameForPackage(module_info.package_name);
        modules_seen += 1;

        var mod_docs = extract.extractModuleDocs(ctx.gpa, module_info.semantic.env, sched_pkg_name, module_info.path) catch |err| {
            std.debug.print("Warning: failed to extract docs for module {s}: {}\n", .{ module_info.name, err });
            extract_failed += 1;
            continue;
        };
        module_docs_list.append(ctx.gpa, mod_docs) catch {
            mod_docs.deinit(ctx.gpa);
            continue;
        };
    }

    // If no documentable modules were collected, fail loudly instead of
    // writing an empty docs site, and explain why so the cause is actionable
    // rather than a silent empty index.html.
    if (module_docs_list.items.len == 0) {
        std.debug.print("Error: found no documentable modules in '{s}'.\n", .{module_path});
        if (modules_seen == 0) {
            // The file compiled (any check errors would have aborted earlier),
            // yet no module was scheduled to document. This happens when the
            // source's module name shadows a compiler builtin (e.g. a file
            // declaring `Builtin`) or otherwise isn't built as a standalone
            // module — there are no canonicalization or type errors to report.
            std.debug.print(
                "  The file compiled successfully but produced no standalone module to document.\n" ++
                    "  This typically means its module name shadows a compiler builtin, or it is a\n" ++
                    "  builtin/dependency file that is only documented as part of its owning package.\n",
                .{},
            );
        } else {
            // Modules existed but every one was filtered out; report the
            // breakdown so the user knows which filter dropped them.
            std.debug.print(
                "  Saw {d} documentable module candidate(s), {d} failed extraction.\n",
                .{ modules_seen, extract_failed },
            );
        }
        return error.DocsFailed;
    }

    // Module collection can still depend on package hash-map order for
    // non-platform roots, and docs output must be deterministic.
    std.mem.sort(DocModel.ModuleDocs, module_docs_list.items, {}, DocModel.moduleDocsLessThan);

    // Determine the package name for the docs header.
    // For packages, use the parent directory name (e.g., "my_parser" from "my_parser/main.roc")
    // since the entry file is just a package definition.
    // For apps/platforms, use the filename without extension (e.g., "app" from "app.roc").
    const pkg_name = if (is_package)
        try ctx.gpa.dupe(u8, std.fs.path.basename(std.fs.path.dirname(module_path) orelse "."))
    else blk: {
        const basename = std.fs.path.basename(module_path);
        break :blk if (std.mem.endsWith(u8, basename, ".roc"))
            try ctx.gpa.dupe(u8, basename[0 .. basename.len - 4])
        else
            try ctx.gpa.dupe(u8, basename);
    };

    const modules_slice = try module_docs_list.toOwnedSlice(ctx.gpa);

    var package_docs = DocModel.PackageDocs{
        .name = pkg_name,
        .modules = modules_slice,
    };
    defer package_docs.deinit(ctx.gpa);

    // Promote the builtin types (Str, Num, …) to top-level modules so the
    // internal `Builtin` container never surfaces in the generated docs.
    try package_docs.reshapeBuiltin(ctx.gpa);
    try package_docs.resolveDocRefs(ctx.gpa);

    // Remove existing output directory to ensure a clean build
    try std.Io.Dir.cwd().deleteTree(ctx.io.std_io, base_output_dir);

    // Create output directory
    std.Io.Dir.cwd().createDirPath(ctx.io.std_io, base_output_dir) catch |err| switch (err) {
        error.PathAlreadyExists => {},
        else => return err,
    };

    // Load the language reference articles when requested. They are read from
    // `docs/langref` (relative to the current working directory).
    const langref_dir_path = "docs/langref";
    var langref: ?docs.render_markdown.LangRef = if (with_lang_ref)
        docs.render_markdown.load(ctx.gpa, ctx.io.std_io, langref_dir_path) catch |err| {
            std.debug.print("Error: failed to load language reference from '{s}': {}\n", .{ langref_dir_path, err });
            return error.DocsFailed;
        }
    else
        null;
    defer if (langref) |*lr| lr.deinit();

    // Generate HTML documentation site
    // TODO: support --format md and --format json output formats
    const render_html = docs.render_html;
    var broken_links: std.ArrayListUnmanaged(render_html.BrokenLink) = .empty;
    defer {
        for (broken_links.items) |bl| {
            ctx.gpa.free(bl.label);
            ctx.gpa.free(bl.resolved_anchor);
        }
        broken_links.deinit(ctx.gpa);
    }
    render_html.renderPackageDocs(ctx.gpa, ctx.io.std_io, &package_docs, base_output_dir, &broken_links, if (langref) |*lr| lr else null) catch |err| {
        return err;
    };

    if (broken_links.items.len > 0) {
        std.debug.print("Error: {d} doc reference(s) point at non-existent anchors:\n", .{broken_links.items.len});
        for (broken_links.items) |bl| {
            const path = if (bl.source_path.len > 0) bl.source_path else bl.source_module;
            if (bl.source_line > 0) {
                std.debug.print("  {s}:{d}: [{s}] -> #{s}\n", .{ path, bl.source_line, bl.label, bl.resolved_anchor });
            } else {
                std.debug.print("  {s}: [{s}] -> #{s}\n", .{ path, bl.label, bl.resolved_anchor });
            }
        }
        return error.BrokenDocLinks;
    }
}

test {
    _ = @import("linker.zig");
}

test "readWatchInputsFile reports missing child watch input output" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();

    var io_state = Io.create(testing.io);
    var ctx = CliCtx.init(allocator, arena.allocator(), &io_state, .check);
    ctx.initIo();
    defer ctx.deinit();

    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();

    const tmp_root = try tmp.dir.realPathFileAlloc(testing.io, ".", allocator);
    defer allocator.free(tmp_root);

    const missing_path = try std.fs.path.join(allocator, &.{ tmp_root, "missing-watch-inputs" });
    defer allocator.free(missing_path);

    try testing.expectError(error.WatchInputsMissing, readWatchInputsFile(&ctx, missing_path, &.{}));
}

test "readWatchInputsFile parses path states and explicit extras" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();

    var io_state = Io.create(testing.io);
    var ctx = CliCtx.init(allocator, arena.allocator(), &io_state, .check);
    ctx.initIo();
    defer ctx.deinit();

    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();

    const tmp_root = try tmp.dir.realPathFileAlloc(testing.io, ".", allocator);
    defer allocator.free(tmp_root);

    const inputs_path = try std.fs.path.join(allocator, &.{ tmp_root, "watch-inputs" });
    defer allocator.free(inputs_path);

    const watched_path = try std.fs.path.join(allocator, &.{ tmp_root, "watched.roc" });
    defer allocator.free(watched_path);

    const main_path = try std.fs.path.join(allocator, &.{ tmp_root, "main.roc" });
    defer allocator.free(main_path);

    var bytes = std.ArrayList(u8).empty;
    defer bytes.deinit(allocator);
    try bytes.appendSlice(allocator, watch_inputs_magic);
    try bytes.append(allocator, 0);
    try bytes.appendSlice(allocator, watched_path);
    try bytes.append(allocator, 0);
    try appendSerializedWatchFileState(allocator, &bytes, .missing);
    try bytes.append(allocator, 0);

    try tmp.dir.writeFile(testing.io, .{ .sub_path = "watch-inputs", .data = bytes.items });

    var input_set = try readWatchInputsFile(&ctx, inputs_path, &.{ watched_path, main_path });
    defer input_set.deinit(&ctx);

    try testing.expectEqual(@as(usize, 2), input_set.inputs.len);
    try testing.expectEqual(@as(usize, 2), input_set.snapshot.len);
    try testing.expectEqualStrings(watched_path, input_set.inputs[0]);
    try testing.expectEqualStrings(main_path, input_set.inputs[1]);
    try testing.expect(input_set.snapshot[0].state.eql(.missing));
    try testing.expect(input_set.snapshot[1].state.eql(.missing));
}

test "watchPathIsInsideDirectory respects path boundaries" {
    const testing = std.testing;

    try testing.expect(watchPathIsInsideDirectory("/tmp/roc-watch/app.roc", "/tmp/roc-watch"));
    try testing.expect(watchPathIsInsideDirectory("/tmp/roc-watch", "/tmp/roc-watch"));
    try testing.expect(!watchPathIsInsideDirectory("/tmp/roc-watch-other/app.roc", "/tmp/roc-watch"));
    try testing.expect(!watchPathIsInsideDirectory("/tmp/roc-watch", "/tmp/roc"));
    try testing.expect(watchPathIsInsideDirectory("C:\\tmp\\roc-watch\\app.roc", "C:\\tmp\\roc-watch"));
    try testing.expect(!watchPathIsInsideDirectory("C:\\tmp\\roc-watch-other\\app.roc", "C:\\tmp\\roc-watch"));
}

test "refreshWatchState reports stale snapshot for newly discovered input" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();

    var io_state = Io.create(testing.io);
    var ctx = CliCtx.init(allocator, arena.allocator(), &io_state, .check);
    ctx.initIo();
    defer ctx.deinit();

    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();

    const tmp_root = try tmp.dir.realPathFileAlloc(testing.io, ".", allocator);
    defer allocator.free(tmp_root);

    const watched_path = try std.fs.path.join(allocator, &.{ tmp_root, "watched.txt" });
    defer allocator.free(watched_path);

    try tmp.dir.writeFile(testing.io, .{ .sub_path = "watched.txt", .data = "old" });

    const inputs = try allocator.alloc([]const u8, 1);
    errdefer allocator.free(inputs);
    inputs[0] = try allocator.dupe(u8, watched_path);
    errdefer allocator.free(inputs[0]);

    const snapshot = try allocator.alloc(WatchSnapshotEntry, 1);
    errdefer allocator.free(snapshot);
    snapshot[0] = .{ .state = try readWatchFileState(&ctx, watched_path) };

    try tmp.dir.writeFile(testing.io, .{ .sub_path = "watched.txt", .data = "new" });

    var signal = WatchEventSignal{};
    var state = WatchState{};
    defer state.deinit(&ctx);

    const changed = try refreshWatchState(&ctx, &state, &signal, .{
        .inputs = inputs,
        .snapshot = snapshot,
    });

    try testing.expect(changed);
}

test "refreshWatchState reports stale snapshot for existing watch set" {
    const testing = std.testing;
    const allocator = testing.allocator;

    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();

    var io_state = Io.create(testing.io);
    var ctx = CliCtx.init(allocator, arena.allocator(), &io_state, .check);
    ctx.initIo();
    defer ctx.deinit();

    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();

    const tmp_root = try tmp.dir.realPathFileAlloc(testing.io, ".", allocator);
    defer allocator.free(tmp_root);

    const watched_path = try std.fs.path.join(allocator, &.{ tmp_root, "watched.txt" });
    defer allocator.free(watched_path);

    try tmp.dir.writeFile(testing.io, .{ .sub_path = "watched.txt", .data = "old" });

    var signal = WatchEventSignal{};
    var state = WatchState{};
    defer state.deinit(&ctx);

    const initial_inputs = try collectWatchInputSet(&ctx, null, &.{watched_path});
    try testing.expect(!try refreshWatchState(&ctx, &state, &signal, initial_inputs));

    const refreshed_inputs = try allocator.alloc([]const u8, 1);
    errdefer allocator.free(refreshed_inputs);
    refreshed_inputs[0] = try allocator.dupe(u8, watched_path);
    errdefer allocator.free(refreshed_inputs[0]);

    const refreshed_snapshot = try allocator.alloc(WatchSnapshotEntry, 1);
    errdefer allocator.free(refreshed_snapshot);
    refreshed_snapshot[0] = state.snapshot[0];

    try tmp.dir.writeFile(testing.io, .{ .sub_path = "watched.txt", .data = "new" });

    const changed = try refreshWatchState(&ctx, &state, &signal, .{
        .inputs = refreshed_inputs,
        .snapshot = refreshed_snapshot,
    });

    try testing.expect(changed);
}

test "appendWindowsQuotedArg" {
    const testing = std.testing;

    // Helper to test the quoting function
    const testQuote = struct {
        fn run(input: []const u8, expected: []const u8) CliMainError!void {
            var cmd = std.array_list.Managed(u8).initCapacity(testing.allocator, 64) catch unreachable;
            defer cmd.deinit();
            try appendWindowsQuotedArg(&cmd, input);
            try testing.expectEqualStrings(expected, cmd.items);
        }
    }.run;

    // Simple arg without spaces - no quoting needed
    try testQuote("simple", "simple");

    // Arg with spaces - needs quoting
    try testQuote("hello world", "\"hello world\"");

    // Arg with tab - needs quoting
    try testQuote("hello\tworld", "\"hello\tworld\"");

    // Empty arg - needs quoting
    try testQuote("", "\"\"");

    // Arg with embedded quote - needs escaping
    try testQuote("say \"hello\"", "\"say \\\"hello\\\"\"");

    // Arg with backslash not before quote - unchanged
    try testQuote("path\\to\\file", "path\\to\\file");

    // Arg with backslash before quote - backslash doubled
    try testQuote("path\\\"quote", "\"path\\\\\\\"quote\"");

    // Arg with trailing backslash - doubled when quoted
    try testQuote("path with spaces\\", "\"path with spaces\\\\\"");

    // Arg with multiple trailing backslashes (needs space to trigger quoting)
    try testQuote("has spaces\\\\", "\"has spaces\\\\\\\\\"");
}

test "isCompilerOwnedBuiltinSourcePath detects builtin by filename and content markers" {
    const testing = std.testing;
    const allocator = testing.allocator;
    const io = std.Io.Threaded.global_single_threaded.io();

    var tmp = testing.tmpDir(.{});
    defer tmp.cleanup();

    const tmp_root = try std.fs.path.join(allocator, &.{ ".zig-cache", "tmp", tmp.sub_path[0..] });
    defer allocator.free(tmp_root);

    const markers = "Str :: [ProvidedByCompiler].{\n}\n";

    const expectClassified = struct {
        fn check(gpa: Allocator, t_io: std.Io, root: []const u8, dir: std.Io.Dir, name: []const u8, data: []const u8, expected: bool) CliMainError!void {
            try dir.writeFile(t_io, .{ .sub_path = name, .data = data });
            const path = try std.fs.path.join(gpa, &.{ root, name });
            defer gpa.free(path);
            try testing.expectEqual(expected, isCompilerOwnedBuiltinSourcePath(gpa, t_io, path));
        }
    }.check;

    // The real builtin: correct filename plus both content markers.
    try expectClassified(allocator, io, tmp_root, tmp.dir, "Builtin.roc", markers, true);
    // Correct filename but missing the markers (a user file that happens to be
    // named Builtin.roc) must not be classified as compiler-owned.
    try expectClassified(allocator, io, tmp_root, tmp.dir, "Builtin.roc", "foo = 1\n", false);
    // The markers in a file that isn't named Builtin.roc must not match.
    try expectClassified(allocator, io, tmp_root, tmp.dir, "NotBuiltin.roc", markers, false);

    // A non-existent path is not the builtin (read failure → false, not a crash).
    const missing = try std.fs.path.join(allocator, &.{ tmp_root, "Missing.roc" });
    defer allocator.free(missing);
    try testing.expect(!isCompilerOwnedBuiltinSourcePath(allocator, io, missing));
}

test "classifyNativeRunTermination preserves warning exit code" {
    const testing = std.testing;

    const result = classifyNativeRunTermination(.{ .exited = 0 }, 1);

    try testing.expect(result == .exit_code);
    try testing.expectEqual(@as(u8, 2), result.exit_code);
}

test "classifyNativeRunTermination preserves signal termination" {
    const testing = std.testing;

    const result = classifyNativeRunTermination(.{ .signal = @enumFromInt(11) }, 0);

    try testing.expect(result == .signal);
    try testing.expectEqual(@as(std.posix.SIG, @enumFromInt(11)), result.signal);
}

test "longestCommonParentDir" {
    const testing = std.testing;
    const allocator = testing.allocator;

    const cases = [_]struct {
        paths: []const []const u8,
        expected: []const u8,
    }{
        // Single file: parent directory of that file.
        .{ .paths = &.{"/tmp/pkg/main.roc"}, .expected = "/tmp/pkg" },
        // Two files sharing a parent.
        .{ .paths = &.{ "/tmp/pkg/main.roc", "/tmp/pkg/Mod.roc" }, .expected = "/tmp/pkg" },
        // Files in sibling subdirectories: common parent.
        .{ .paths = &.{ "/tmp/nested/a/main.roc", "/tmp/nested/b/Mod.roc" }, .expected = "/tmp/nested" },
        // Names share a byte prefix but no directory boundary — must back up.
        .{ .paths = &.{ "/tmp/abc/a.roc", "/tmp/abd/b.roc" }, .expected = "/tmp" },
        // Only root in common.
        .{ .paths = &.{ "/etc/foo.roc", "/var/bar.roc" }, .expected = "/" },
        // Three files with same parent.
        .{ .paths = &.{ "/a/b/c/x.roc", "/a/b/c/y.roc", "/a/b/c/z.roc" }, .expected = "/a/b/c" },
        // Three files where the third narrows the common parent.
        .{ .paths = &.{ "/a/b/c/x.roc", "/a/b/c/y.roc", "/a/b/d/z.roc" }, .expected = "/a/b" },
    };

    for (cases) |tc| {
        const got = try longestCommonParentDir(allocator, tc.paths);
        defer allocator.free(got);
        testing.expectEqualStrings(tc.expected, got) catch |err| {
            std.debug.print("Failed case: expected='{s}' got='{s}'\n", .{ tc.expected, got });
            return err;
        };
    }
}
