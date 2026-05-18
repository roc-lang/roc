//! Shared compile-and-run pipeline for the echo platform.
//!
//! Both the wasm export (`echo.zig`) and the native CLI (`echo_native.zig`)
//! drive this function. Keeping the pipeline in one place means a native
//! reproduction with a real stack trace exercises identical code to the
//! browser path.

const std = @import("std");
const base = @import("base");
const check = @import("check");
const compile = @import("compile");
const echo_platform = @import("echo_platform");
const eval = @import("eval");
const lir = @import("lir");
const roc_target = @import("roc_target");
const reporting = @import("reporting");

const Allocator = std.mem.Allocator;
const BuildEnv = compile.BuildEnv;
const Io = compile.Io;
const RocTarget = roc_target.RocTarget;
const HostedFn = echo_platform.host_abi.HostedFn;

/// Diagnostic-emission interface. The wasm host renders HTML to `js_stderr`;
/// the native host renders plain text to its real stderr.
pub const Diagnostics = struct {
    ctx: ?*anyopaque,
    vtable: *const VTable,

    pub const VTable = struct {
        /// Emit a free-form message (e.g. "echo: step 'X' failed: Y").
        write: *const fn (ctx: ?*anyopaque, msg: []const u8) void,
        /// Render & emit a single compiler Report.
        emitReport: *const fn (ctx: ?*anyopaque, gpa: Allocator, report: *reporting.Report) void,
    };

    pub fn write(self: Diagnostics, msg: []const u8) void {
        self.vtable.write(self.ctx, msg);
    }

    pub fn emitReport(self: Diagnostics, gpa: Allocator, report: *reporting.Report) void {
        self.vtable.emitReport(self.ctx, gpa, report);
    }

    /// Format a labelled step-failure message and emit it.
    pub fn step(self: Diagnostics, comptime label: []const u8, err: anyerror) void {
        var buf: [256]u8 = undefined;
        const msg = std.fmt.bufPrint(&buf, "echo: step '" ++ label ++ "' failed: {s}\n", .{@errorName(err)}) catch {
            self.write("echo: step '" ++ label ++ "' failed (and message formatting failed)\n");
            return;
        };
        self.write(msg);
    }
};

/// A user-supplied module file (e.g. `Greeting.roc`).
pub const ExtraFile = struct {
    name: []const u8,
    content: []const u8,
};

/// Caller-controlled paths for the synthetic app & embedded platform files.
/// Both the wasm and native flows generate a synthetic app whose `app [main!]`
/// header references these paths.
pub const Paths = struct {
    app_abs: []const u8 = "/app/main.roc",
    platform_main: []const u8 = "/app/.roc_echo_platform/main.roc",
    echo_module: []const u8 = "/app/.roc_echo_platform/Echo.roc",
    cwd: []const u8 = "/app",
    /// Prefix prepended to extra-file paths so `addFile("Greeting", ...)`
    /// resolves as `<extras_prefix>Greeting.roc`. Must end with `/`.
    extras_prefix: []const u8 = "/app/",
};

/// Inputs to `runEcho`. See field doc comments for the contract on each.
pub const RunOptions = struct {
    /// Single contiguous arena used for the entire pipeline (BuildEnv,
    /// lowering, runtime image, interpreter). Must own a flat virtual
    /// region — `std.heap.ArenaAllocator` will not work because the
    /// runtime-image step computes offsets via `ptr - base_ptr` arithmetic
    /// (see `src/compile/README.md` "Runtime arena").
    runtime_fba: *std.heap.FixedBufferAllocator,
    /// Fallback Io that handles everything not served by EchoCtx (i.e. paths
    /// not in `Paths` and not in `extras`).
    fallback_io: Io,
    /// The headerless user source (the body of the synthetic app).
    source: []const u8,
    /// Extra user-supplied modules, available as `import <name>`.
    extras: []const ExtraFile,
    /// Where to send diagnostics.
    diagnostics: Diagnostics,
    /// Filesystem paths used by the synthetic app + embedded platform.
    paths: Paths = .{},
    /// Target word width. `.u32` for wasm32 builds.
    target_usize: base.target.TargetUsize = .u32,
    /// Roc compilation target.
    roc_target: RocTarget = .wasm32,
};

/// Compile and execute a headerless Roc source through the echo platform.
///
/// Wraps the user `source` in a synthetic `app [main!]` header that imports
/// the embedded echo platform, then walks the canonical embedding sequence
/// (see `src/compile/README.md`): BuildEnv discovery → check → LIR lowering
/// → `RuntimeImage.fillHeaderInBuffer` → `viewMappedImage` →
/// `LirInterpreter.runEntrypoint`.
///
/// Returns the Roc program's exit code (or 1 if an inline `expect` failed
/// while the program otherwise returned 0). On compilation/runtime error,
/// emits a labelled diagnostic through `opts.diagnostics` and propagates
/// the error so the caller can decide on an exit code.
pub fn runEcho(opts: RunOptions) !u8 {
    const allocator = opts.runtime_fba.allocator();
    const diag = opts.diagnostics;

    const header_str =
        "app [main!] { pf: platform \"./.roc_echo_platform/main.roc\" }\n\n" ++
        "import pf.Echo\n\n";
    const footer_str =
        "\n\necho! = |msg| Echo.line!(msg)\n";

    const synthetic_source = std.mem.concat(allocator, u8, &.{ header_str, opts.source, footer_str }) catch |err| {
        diag.step("std.mem.concat (synthetic source)", err);
        return err;
    };

    var echo_ctx = EchoCtx{
        .paths = opts.paths,
        .synthetic_app_source = synthetic_source,
        .extras = opts.extras,
        .fallback = opts.fallback_io,
    };

    var build_env = BuildEnv.init(allocator, .single_threaded, 1, opts.roc_target, opts.paths.cwd) catch |err| {
        diag.step("BuildEnv.init", err);
        return err;
    };
    defer build_env.deinit();
    build_env.filesystem = echo_ctx.io();

    build_env.discoverDependencies(opts.paths.app_abs) catch |err| {
        emitDiagnostics(&build_env, diag, allocator);
        diag.step("discoverDependencies", err);
        return err;
    };
    build_env.compileDiscovered() catch |err| {
        emitDiagnostics(&build_env, diag, allocator);
        diag.step("compileDiscovered", err);
        return err;
    };

    emitDiagnostics(&build_env, diag, allocator);

    const root_artifact = build_env.executableRootCheckedArtifact();

    const import_views = build_env.collectImportedArtifactViews(allocator, root_artifact) catch |err| {
        diag.step("collectImportedArtifactViews", err);
        return err;
    };
    defer allocator.free(import_views);
    const relation_views = build_env.collectRelationArtifactViews(allocator, root_artifact) catch |err| {
        diag.step("collectRelationArtifactViews", err);
        return err;
    };
    defer allocator.free(relation_views);

    var lowered = lir.CheckedPipeline.lowerArtifactsToLir(
        allocator,
        .{
            .root = check.CheckedArtifact.loweringViewWithRelations(root_artifact, relation_views),
            .imports = import_views,
        },
        .{ .requests = root_artifact.root_requests.requests },
        .{ .target_usize = opts.target_usize },
    ) catch |err| {
        diag.step("lowerArtifactsToLir", err);
        return err;
    };
    defer lowered.deinit();

    // Canonical embedding sequence: build entrypoints + runtime image, then
    // view it and execute via runEntrypoint. The viewMappedImage step
    // re-constructs the layout Store with the correct target_usize, which
    // matters for cross-compile cases.
    const entrypoints = lowered.platformEntrypoints(allocator) catch |err| {
        diag.step("platformEntrypoints", err);
        return err;
    };

    const runtime_header = allocator.create(lir.RuntimeImage.Header) catch |err| {
        diag.step("create RuntimeImage.Header", err);
        return err;
    };
    lir.RuntimeImage.fillHeaderInBuffer(
        runtime_header,
        opts.runtime_fba.buffer.ptr,
        opts.runtime_fba.end_index,
        &lowered.lir_result,
        lowered.target_usize,
        entrypoints,
    ) catch |err| {
        diag.step("RuntimeImage.fillHeaderInBuffer", err);
        return err;
    };

    const view = lir.RuntimeImage.viewMappedImage(
        runtime_header,
        opts.runtime_fba.buffer.ptr,
        opts.runtime_fba.end_index,
    ) catch |err| {
        diag.step("RuntimeImage.viewMappedImage", err);
        return err;
    };

    return runEchoView(allocator, &view, diag) catch |err| {
        diag.step("runEchoView", err);
        return err;
    };
}

fn runEchoView(
    allocator: Allocator,
    view: *const lir.RuntimeImage.ProgramView,
    diag: Diagnostics,
) !u8 {
    // HostedFn array order matters: the interpreter calls
    // `roc_ops.hosted_fns.fns[dispatch_index]`. Dispatch indices are sorted
    // alphabetically by fully-qualified `Module.fn_name` (with trailing `!`
    // stripped). The echo platform has only `Echo.line`, so order is
    // trivially correct — but additions must respect alphabetical order or
    // the wrong function will be called silently. See README "Host functions".
    var hosted_fn_array = [_]HostedFn{echo_platform.host_abi.hostedFn(&echo_platform.echoHostedFn)};
    var default_roc_ops_env: echo_platform.DefaultRocOpsEnv = .{};
    var roc_ops = echo_platform.makeDefaultRocOps(&default_roc_ops_env, &hosted_fn_array);
    var cli_args_list = echo_platform.buildCliArgs(&.{}, &roc_ops);
    var result_buf: [16]u8 align(16) = undefined;

    var interpreter = eval.LirInterpreter.init(
        allocator,
        &view.store,
        &view.layouts,
        &roc_ops,
    ) catch |err| {
        diag.step("LirInterpreter.init", err);
        return err;
    };
    defer interpreter.deinit();

    _ = interpreter.runEntrypoint(view, 0, @ptrCast(&cli_args_list), @ptrCast(&result_buf)) catch |err| switch (err) {
        error.RuntimeError, error.DivisionByZero => {
            if (interpreter.getRuntimeErrorMessage()) |msg| diag.write(msg);
            diag.step("interpreter.runEntrypoint", err);
            return error.EvaluationFailed;
        },
        error.Crash => {
            diag.step("interpreter.runEntrypoint", err);
            return error.EvaluationFailed;
        },
        error.OutOfMemory => {
            diag.step("interpreter.runEntrypoint", err);
            return error.OutOfMemory;
        },
        error.EntrypointNotFound => {
            diag.step("interpreter.runEntrypoint", err);
            return err;
        },
    };

    if (default_roc_ops_env.inline_expect_failed) return 1;
    return result_buf[0];
}

fn emitDiagnostics(build_env: *BuildEnv, diag: Diagnostics, gpa: Allocator) void {
    const drained = build_env.drainReports() catch return;
    defer build_env.freeDrainedReports(drained);

    for (drained) |mod| {
        for (mod.reports) |*report| {
            diag.emitReport(gpa, report);
        }
    }
}

// --- Echo I/O context ---

/// Unified I/O context for the echo platform. Intercepts readFile/fileExists
/// for synthetic app source, embedded platform files, and extras; delegates
/// everything else to `fallback`.
pub const EchoCtx = struct {
    paths: Paths,
    synthetic_app_source: []const u8,
    extras: []const ExtraFile,
    fallback: Io,

    pub fn io(self: *EchoCtx) Io {
        return .{ .ctx = @ptrCast(self), .vtable = echo_vtable };
    }

    /// Return the content for a synthetic/embedded path, or null if not synthetic.
    pub fn getSyntheticContent(self: *const EchoCtx, path: []const u8) ?[]const u8 {
        if (std.mem.eql(u8, path, self.paths.app_abs)) return self.synthetic_app_source;
        if (std.mem.eql(u8, path, self.paths.platform_main)) return echo_platform.platform_main_source;
        if (std.mem.eql(u8, path, self.paths.echo_module)) return echo_platform.echo_module_source;
        for (self.extras) |ef| {
            if (matchesExtra(self.paths.extras_prefix, ef.name, path)) return ef.content;
        }
        return null;
    }

    /// Check if `path` is one of the synthetic/embedded paths.
    pub fn isSyntheticPath(self: *const EchoCtx, path: []const u8) bool {
        return self.getSyntheticContent(path) != null;
    }
};

fn matchesExtra(prefix: []const u8, name: []const u8, path: []const u8) bool {
    const suffix = ".roc";
    if (path.len != prefix.len + name.len + suffix.len) return false;
    if (!std.mem.startsWith(u8, path, prefix)) return false;
    if (!std.mem.eql(u8, path[prefix.len..][0..name.len], name)) return false;
    return std.mem.eql(u8, path[prefix.len + name.len ..][0..suffix.len], suffix);
}

fn echoGetCtx(ctx_ptr: ?*anyopaque) *EchoCtx {
    return @ptrCast(@alignCast(ctx_ptr.?));
}

fn echoReadFile(ctx_ptr: ?*anyopaque, path: []const u8, gpa: Allocator) Io.ReadError![]u8 {
    const self = echoGetCtx(ctx_ptr);
    if (self.getSyntheticContent(path)) |content|
        return gpa.dupe(u8, content) catch return error.OutOfMemory;
    return self.fallback.readFile(path, gpa);
}

fn echoFileExists(ctx_ptr: ?*anyopaque, path: []const u8) bool {
    const self = echoGetCtx(ctx_ptr);
    if (self.isSyntheticPath(path)) return true;
    return self.fallback.fileExists(path);
}

fn echoReadFileInto(ctx_ptr: ?*anyopaque, path: []const u8, buf: []u8) Io.ReadError!usize {
    return echoGetCtx(ctx_ptr).fallback.readFileInto(path, buf);
}
fn echoWriteFile(ctx_ptr: ?*anyopaque, path: []const u8, data: []const u8) Io.WriteError!void {
    return echoGetCtx(ctx_ptr).fallback.writeFile(path, data);
}
fn echoStat(ctx_ptr: ?*anyopaque, path: []const u8) Io.StatError!Io.FileInfo {
    return echoGetCtx(ctx_ptr).fallback.stat(path);
}
fn echoListDir(ctx_ptr: ?*anyopaque, path: []const u8, gpa: Allocator) Io.ListError![]Io.FileEntry {
    return echoGetCtx(ctx_ptr).fallback.listDir(path, gpa);
}
fn echoDirName(ctx_ptr: ?*anyopaque, path: []const u8) ?[]const u8 {
    return echoGetCtx(ctx_ptr).fallback.dirName(path);
}
fn echoBaseName(ctx_ptr: ?*anyopaque, path: []const u8) []const u8 {
    return echoGetCtx(ctx_ptr).fallback.baseName(path);
}
fn echoJoinPath(ctx_ptr: ?*anyopaque, parts: []const []const u8, gpa: Allocator) Allocator.Error![]const u8 {
    return echoGetCtx(ctx_ptr).fallback.joinPath(parts, gpa);
}
fn echoCanonicalize(ctx_ptr: ?*anyopaque, path: []const u8, gpa: Allocator) Io.CanonicalizeError![]const u8 {
    return echoGetCtx(ctx_ptr).fallback.canonicalize(path, gpa);
}
fn echoMakePath(ctx_ptr: ?*anyopaque, path: []const u8) Io.MakePathError!void {
    return echoGetCtx(ctx_ptr).fallback.makePath(path);
}
fn echoRename(ctx_ptr: ?*anyopaque, old: []const u8, new: []const u8) Io.RenameError!void {
    return echoGetCtx(ctx_ptr).fallback.rename(old, new);
}
fn echoGetEnvVar(ctx_ptr: ?*anyopaque, key: []const u8, gpa: Allocator) Io.GetEnvVarError![]u8 {
    return echoGetCtx(ctx_ptr).fallback.getEnvVar(key, gpa);
}
fn echoFetchUrl(ctx_ptr: ?*anyopaque, gpa: Allocator, url: []const u8, dest: []const u8) Io.FetchUrlError!void {
    return echoGetCtx(ctx_ptr).fallback.fetchUrl(gpa, url, dest);
}
fn echoWriteStdout(ctx_ptr: ?*anyopaque, data: []const u8) Io.StdioError!void {
    return echoGetCtx(ctx_ptr).fallback.writeStdout(data);
}
fn echoWriteStderr(ctx_ptr: ?*anyopaque, data: []const u8) Io.StdioError!void {
    return echoGetCtx(ctx_ptr).fallback.writeStderr(data);
}
fn echoReadStdin(ctx_ptr: ?*anyopaque, buf: []u8) Io.StdioError!usize {
    return echoGetCtx(ctx_ptr).fallback.readStdin(buf);
}
fn echoIsTty(ctx_ptr: ?*anyopaque) bool {
    return echoGetCtx(ctx_ptr).fallback.isTty();
}

const echo_vtable = Io.VTable{
    .readFile = &echoReadFile,
    .readFileInto = &echoReadFileInto,
    .writeFile = &echoWriteFile,
    .fileExists = &echoFileExists,
    .stat = &echoStat,
    .listDir = &echoListDir,
    .dirName = &echoDirName,
    .baseName = &echoBaseName,
    .joinPath = &echoJoinPath,
    .canonicalize = &echoCanonicalize,
    .makePath = &echoMakePath,
    .rename = &echoRename,
    .getEnvVar = &echoGetEnvVar,
    .fetchUrl = &echoFetchUrl,
    .writeStdout = &echoWriteStdout,
    .writeStderr = &echoWriteStderr,
    .readStdin = &echoReadStdin,
    .isTty = &echoIsTty,
};
