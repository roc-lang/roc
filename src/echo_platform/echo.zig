//! WASM entry point for the echo platform.
//!
//! Provides exported functions for JavaScript to:
//! 1. Initialize the WASM module
//! 2. Register module files (e.g. Greeting.roc)
//! 3. Compile and execute a headerless Roc app through the full compiler pipeline
//!
//! Uses the same multi-module BuildEnv compilation as the CLI,
//! with a unified EchoCtx (Io implementation) that intercepts reads for
//! the echo platform's virtual files (main.roc, Echo.roc) and user-provided
//! modules, while delegating everything else to WasmFilesystem.

const std = @import("std");
const builtin = @import("builtin");
const base = @import("base");
const check = @import("check");
const compile = @import("compile");
const echo_platform = @import("echo_platform");
const eval = @import("eval");
const layout = @import("layout");
const lir = @import("lir");
const roc_target = @import("roc_target");
const reporting = @import("reporting");

const WasmFilesystem = @import("WasmFilesystem.zig");

const BuildEnv = compile.BuildEnv;
const Io = compile.Io;
const RocTarget = roc_target.RocTarget;
const HostedFn = echo_platform.host_abi.HostedFn;
const ReportingConfig = reporting.ReportingConfig;

const Allocator = std.mem.Allocator;

/// Public API.
pub const std_options: std.Options = .{
    .log_level = .warn,
    .logFn = logFn,
};

fn logFn(comptime level: std.log.Level, comptime scope: @TypeOf(.enum_literal), comptime format: []const u8, args: anytype) void {
    if (comptime builtin.target.os.tag == .freestanding) {
        return;
    }
    std.log.defaultLog(level, scope, format, args);
}

// Fixed-size heap in WASM linear memory (64 MB).
var wasm_heap_memory: [64 * 1024 * 1024]u8 = undefined;
var fba: std.heap.FixedBufferAllocator = undefined;
var allocator: Allocator = undefined;

// --- JS imports ---

const js = struct {
    extern "env" fn js_stderr(ptr: [*]const u8, len: usize) void;
};

fn jsErr(msg: []const u8) void {
    js.js_stderr(msg.ptr, msg.len);
}

fn emitDiagnostics(build_env: *BuildEnv) void {
    const drained = build_env.drainReports() catch return;
    defer build_env.freeDrainedReports(drained);

    const config = ReportingConfig.initHtml();

    for (drained) |mod| {
        for (mod.reports) |*report| {
            var diag_writer: std.Io.Writer.Allocating = .init(allocator);
            reporting.renderReportToHtml(report, &diag_writer.writer, config) catch continue;
            const output = diag_writer.written();
            if (output.len > 0) {
                js.js_stderr(output.ptr, output.len);
            }
        }
    }
}

fn platformRootProc(lowered: *const lir.CheckedPipeline.LoweredProgram) lir.LirProcSpecId {
    const root_procs = lowered.lir_result.root_procs.items;
    const root_metadata = lowered.lir_result.root_metadata.items;
    if (root_procs.len != root_metadata.len) {
        if (builtin.mode == .Debug) {
            std.debug.panic(
                "echo platform invariant violated: root metadata mismatch roots={d} metadata={d}",
                .{ root_procs.len, root_metadata.len },
            );
        }
        unreachable;
    }
    for (root_procs, root_metadata) |root_proc, metadata| {
        if (metadata.abi == .platform or metadata.exposure == .platform_required) return root_proc;
    }
    if (builtin.mode == .Debug) {
        std.debug.panic("echo platform invariant violated: checked artifact lowering produced no platform root", .{});
    }
    unreachable;
}

fn argLayoutsForProc(
    alloc: Allocator,
    store: *const lir.LirStore,
    proc_id: lir.LirProcSpecId,
) ![]layout.Idx {
    const proc = store.getProcSpec(proc_id);
    const arg_ids = store.getLocalSpan(proc.args);
    const arg_layouts = try alloc.alloc(layout.Idx, arg_ids.len);
    errdefer alloc.free(arg_layouts);

    for (arg_ids, 0..) |local_id, i| {
        arg_layouts[i] = store.locals.items[@intFromEnum(local_id)].layout_idx;
    }

    return arg_layouts;
}

fn runEchoLir(lowered: *const lir.CheckedPipeline.LoweredProgram) !u8 {
    var hosted_fn_array = [_]HostedFn{echo_platform.host_abi.hostedFn(&echo_platform.echoHostedFn)};
    var default_roc_ops_env: echo_platform.DefaultRocOpsEnv = .{};
    var roc_ops = echo_platform.makeDefaultRocOps(&default_roc_ops_env, &hosted_fn_array);
    var cli_args_list = echo_platform.buildCliArgs(&.{}, &roc_ops);
    var result_buf: [16]u8 align(16) = undefined;

    const root_proc = platformRootProc(lowered);
    const arg_layouts = try argLayoutsForProc(allocator, &lowered.lir_result.store, root_proc);
    defer allocator.free(arg_layouts);

    var interpreter = try eval.LirInterpreter.init(
        allocator,
        &lowered.lir_result.store,
        &lowered.lir_result.layouts,
        &roc_ops,
    );
    defer interpreter.deinit();

    const proc = lowered.lir_result.store.getProcSpec(root_proc);
    _ = interpreter.eval(.{
        .proc_id = root_proc,
        .arg_layouts = arg_layouts,
        .ret_layout = proc.ret_layout,
        .arg_ptr = @ptrCast(&cli_args_list),
        .ret_ptr = @ptrCast(&result_buf),
    }) catch |err| switch (err) {
        error.RuntimeError, error.DivisionByZero => {
            if (interpreter.getRuntimeErrorMessage()) |msg| jsErr(msg);
            return error.EvaluationFailed;
        },
        error.Crash => return error.EvaluationFailed,
        error.OutOfMemory => return error.OutOfMemory,
    };

    if (default_roc_ops_env.inline_expect_failed) return 1;
    return result_buf[0];
}

// --- Extra module file storage (static, survives FBA reset) ---

const MAX_FILES = 16;
const MAX_NAME_LEN = 256;
const MAX_CONTENT_LEN = 64 * 1024;

const ExtraFile = struct {
    name_buf: [MAX_NAME_LEN]u8 = undefined,
    name_len: usize = 0,
    content_buf: [MAX_CONTENT_LEN]u8 = undefined,
    content_len: usize = 0,

    fn name(self: *const ExtraFile) []const u8 {
        return self.name_buf[0..self.name_len];
    }

    fn content(self: *const ExtraFile) []const u8 {
        return self.content_buf[0..self.content_len];
    }
};

var extra_files: [MAX_FILES]ExtraFile = [_]ExtraFile{.{}} ** MAX_FILES;
var extra_file_count: usize = 0;

// Static buffer for copying source before FBA reset (avoids 64KB stack allocation in WASM).
var source_copy_buf: [MAX_CONTENT_LEN]u8 = undefined;

// WASM filesystem context — static so it survives FBA reset.
var wasm_ctx: WasmFilesystem.WasmContext = .{};

// --- Echo I/O context ---

/// Unified I/O context for the echo platform.
/// Intercepts readFile/fileExists for synthetic app source, embedded platform
/// files, and user-provided module files. All other operations delegate to the
/// WasmFilesystem implementation.
const EchoCtx = struct {
    app_abs_path: []const u8,
    synthetic_app_source: []const u8,
    platform_main_path: []const u8,
    echo_module_path: []const u8,
    fallback: Io,

    fn io(self: *@This()) Io {
        return .{ .ctx = @ptrCast(self), .vtable = echo_vtable };
    }

    /// Return the content for a synthetic/embedded path, or null if not synthetic.
    fn getSyntheticContent(self: *const @This(), path: []const u8) ?[]const u8 {
        if (std.mem.eql(u8, path, self.app_abs_path)) return self.synthetic_app_source;
        if (std.mem.eql(u8, path, self.platform_main_path)) return echo_platform.platform_main_source;
        if (std.mem.eql(u8, path, self.echo_module_path)) return echo_platform.echo_module_source;
        for (extra_files[0..extra_file_count]) |*ef| {
            var expected: [5 + MAX_NAME_LEN + 4]u8 = undefined;
            const prefix = "/app/";
            const suffix = ".roc";
            const total = prefix.len + ef.name_len + suffix.len;
            if (total <= expected.len) {
                @memcpy(expected[0..prefix.len], prefix);
                @memcpy(expected[prefix.len..][0..ef.name_len], ef.name());
                @memcpy(expected[prefix.len + ef.name_len ..][0..suffix.len], suffix);
                if (std.mem.eql(u8, path, expected[0..total])) return ef.content();
            }
        }
        return null;
    }

    /// Check if `path` is one of the synthetic/embedded paths.
    fn isSyntheticPath(self: *const @This(), path: []const u8) bool {
        return self.getSyntheticContent(path) != null;
    }
};

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

// --- Exported WASM API ---

/// Initialize the WASM module. Must be called once before compileAndRun.
export fn init() void {
    fba = std.heap.FixedBufferAllocator.init(&wasm_heap_memory);
    allocator = fba.allocator();
    extra_file_count = 0;
}

/// Allocate a buffer for the host to write data into.
/// Returns a pointer to the buffer, or null on OOM.
export fn allocateBuffer(size: usize) ?[*]u8 {
    const buf = allocator.alloc(u8, size) catch return null;
    return buf.ptr;
}

/// Register an extra module file (e.g. name="Greeting", content="module [msg]\n...").
/// Files are resolved as `/app/<name>.roc` during compilation.
/// Must be called after init() and before compileAndRun().
/// Returns 0 on success, 1 if too many files, 2 if name too long, 3 if content too long.
export fn addFile(
    name_ptr: [*]const u8,
    name_len: usize,
    content_ptr: [*]const u8,
    content_len: usize,
) u8 {
    if (extra_file_count >= MAX_FILES) {
        jsErr("addFile: too many files (max 16)");
        return 1;
    }
    if (name_len > MAX_NAME_LEN) {
        jsErr("addFile: module name too long (max 256 bytes)");
        return 2;
    }
    if (content_len > MAX_CONTENT_LEN) {
        jsErr("addFile: module content too long (max 64KB)");
        return 3;
    }

    var ef = &extra_files[extra_file_count];
    @memcpy(ef.name_buf[0..name_len], name_ptr[0..name_len]);
    ef.name_len = name_len;
    @memcpy(ef.content_buf[0..content_len], content_ptr[0..content_len]);
    ef.content_len = content_len;
    extra_file_count += 1;
    return 0;
}

/// Compile and execute Roc source code through the echo platform pipeline.
///
/// The source should be a headerless Roc module containing a `main!` declaration.
/// Any modules registered via addFile() are available as imports.
/// Returns the exit code from the Roc program (0 on success), or 255 on error.
export fn compileAndRun(source_ptr: [*]const u8, source_len: usize) u8 {
    // Copy source to a static buffer before resetting the FBA,
    // since allocateBuffer returned a pointer into the same heap.
    const len = @min(source_len, source_copy_buf.len);
    @memcpy(source_copy_buf[0..len], source_ptr[0..len]);

    const result = compileAndRunInner(source_copy_buf[0..len]) catch 255;

    // Clear extra files for next run
    extra_file_count = 0;

    return result;
}

fn compileAndRunInner(source: []const u8) !u8 {
    fba.reset();
    allocator = fba.allocator();

    const app_abs_path = "/app/main.roc";
    const platform_main_path = "/app/.roc_echo_platform/main.roc";
    const echo_module_path = "/app/.roc_echo_platform/Echo.roc";

    const header =
        "app [main!] { pf: platform \"./.roc_echo_platform/main.roc\" }\n\n" ++
        "import pf.Echo\n\n";
    const footer =
        "\n\necho! = |msg| Echo.line!(msg)\n";
    const synthetic_source = try std.mem.concat(allocator, u8, &.{ header, source, footer });

    wasm_ctx.setFilename(allocator, app_abs_path);
    wasm_ctx.setSource(allocator, synthetic_source);

    var echo_ctx = EchoCtx{
        .app_abs_path = app_abs_path,
        .synthetic_app_source = synthetic_source,
        .platform_main_path = platform_main_path,
        .echo_module_path = echo_module_path,
        .fallback = WasmFilesystem.wasm(&wasm_ctx),
    };

    var build_env = try BuildEnv.init(allocator, .single_threaded, 1, RocTarget.wasm32, "/app");
    defer build_env.deinit();
    build_env.filesystem = echo_ctx.io();

    build_env.discoverDependencies(app_abs_path) catch |err| {
        emitDiagnostics(&build_env);
        return err;
    };
    build_env.compileDiscovered() catch |err| {
        emitDiagnostics(&build_env);
        return err;
    };

    emitDiagnostics(&build_env);

    const root_artifact = build_env.executableRootCheckedArtifact();

    const import_views = try build_env.collectImportedArtifactViews(allocator, root_artifact);
    defer allocator.free(import_views);
    const relation_views = try build_env.collectRelationArtifactViews(allocator, root_artifact);
    defer allocator.free(relation_views);

    var lowered = try lir.CheckedPipeline.lowerArtifactsToLir(
        allocator,
        .{
            .root = check.CheckedArtifact.loweringViewWithRelations(root_artifact, relation_views),
            .imports = import_views,
        },
        .{ .requests = root_artifact.root_requests.requests },
        .{
            .target_usize = base.target.TargetUsize.u32,
        },
    );
    defer lowered.deinit();

    return try runEchoLir(&lowered);
}
