//! Unified I/O abstraction for the Roc compiler.
//!
//! Provides a VTable-based abstraction over filesystem and stdio operations
//! so compiler-core code is decoupled from `std.fs`/`std.io`/`std.posix`.
//! Consumers (CLI, WASM playground, tests) inject a concrete implementation.
//!
//! Pre-built implementations:
//!   - `Io.default()` — delegates to the real OS (or stubs on wasm32)
//!   - `Io.testing()` — panics on every call (override fields for mocks)

const std = @import("std");
const builtin = @import("builtin");
const Allocator = std.mem.Allocator;

const Self = @This();

ctx: ?*anyopaque,
vtable: VTable,

/// Function pointer table for I/O operations.
/// Implementations provide concrete functions; `ctx` is passed through as
/// the first argument, allowing implementations to carry state.
pub const VTable = struct {
    // --- Filesystem operations ---
    readFile: *const fn (?*anyopaque, []const u8, Allocator) ReadError![]u8,
    readFileInto: *const fn (?*anyopaque, []const u8, []u8) ReadError!usize,
    writeFile: *const fn (?*anyopaque, []const u8, []const u8) WriteError!void,
    fileExists: *const fn (?*anyopaque, []const u8) bool,
    stat: *const fn (?*anyopaque, []const u8) StatError!FileInfo,
    listDir: *const fn (?*anyopaque, []const u8, Allocator) ListError![]FileEntry,
    dirName: *const fn (?*anyopaque, []const u8) ?[]const u8,
    baseName: *const fn (?*anyopaque, []const u8) []const u8,
    joinPath: *const fn (?*anyopaque, []const []const u8, Allocator) Allocator.Error![]const u8,
    canonicalize: *const fn (?*anyopaque, []const u8, Allocator) CanonicalizeError![]const u8,
    makePath: *const fn (?*anyopaque, []const u8) MakePathError!void,
    rename: *const fn (?*anyopaque, []const u8, []const u8) RenameError!void,
    getEnvVar: *const fn (?*anyopaque, []const u8, Allocator) GetEnvVarError![]u8,
    fetchUrl: *const fn (?*anyopaque, Allocator, []const u8, []const u8) FetchUrlError!void,
    // --- Stdio operations ---
    writeStdout: *const fn (?*anyopaque, []const u8) StdioError!void,
    writeStderr: *const fn (?*anyopaque, []const u8) StdioError!void,
    readStdin: *const fn (?*anyopaque, []u8) StdioError!usize,
    isTty: *const fn (?*anyopaque) bool,
};

// --- Filesystem wrapper methods ---

/// Read the entire contents of `path`. Caller owns returned slice.
pub fn readFile(self: Self, path: []const u8, allocator: Allocator) ReadError![]u8 {
    return self.vtable.readFile(self.ctx, path, allocator);
}

/// Read `path` into `buffer`. Returns bytes read.
pub fn readFileInto(self: Self, path: []const u8, buffer: []u8) ReadError!usize {
    return self.vtable.readFileInto(self.ctx, path, buffer);
}

/// Write `data` to `path`, creating or truncating the file.
pub fn writeFile(self: Self, path: []const u8, data: []const u8) WriteError!void {
    return self.vtable.writeFile(self.ctx, path, data);
}

/// Return `true` if a file (or directory) exists at `path`.
pub fn fileExists(self: Self, path: []const u8) bool {
    return self.vtable.fileExists(self.ctx, path);
}

/// Get metadata for `path`.
pub fn stat(self: Self, path: []const u8) StatError!FileInfo {
    return self.vtable.stat(self.ctx, path);
}

/// Backward-compat alias for `stat`.
pub fn getFileInfo(self: Self, path: []const u8) StatError!FileInfo {
    return self.vtable.stat(self.ctx, path);
}

/// List all entries under `path` recursively. Caller owns the returned slice
/// and every `.path` string in it (free with `allocator`).
pub fn listDir(self: Self, path: []const u8, allocator: Allocator) ListError![]FileEntry {
    return self.vtable.listDir(self.ctx, path, allocator);
}

/// Return the directory portion of a path (no allocation).
pub fn dirName(self: Self, path: []const u8) ?[]const u8 {
    return self.vtable.dirName(self.ctx, path);
}

/// Return the filename portion of a path (no allocation).
pub fn baseName(self: Self, path: []const u8) []const u8 {
    return self.vtable.baseName(self.ctx, path);
}

/// Join path segments. Caller owns the result.
pub fn joinPath(self: Self, parts: []const []const u8, allocator: Allocator) Allocator.Error![]const u8 {
    return self.vtable.joinPath(self.ctx, parts, allocator);
}

/// Resolve `path` to a canonical absolute path. Caller owns the result.
pub fn canonicalize(self: Self, path: []const u8, allocator: Allocator) CanonicalizeError![]const u8 {
    return self.vtable.canonicalize(self.ctx, path, allocator);
}

/// Create all directories in `path` recursively (like `mkdir -p`).
pub fn makePath(self: Self, path: []const u8) MakePathError!void {
    return self.vtable.makePath(self.ctx, path);
}

/// Atomically rename `old_path` to `new_path`.
pub fn rename(self: Self, old_path: []const u8, new_path: []const u8) RenameError!void {
    return self.vtable.rename(self.ctx, old_path, new_path);
}

/// Look up environment variable `key`. Caller owns the returned slice.
pub fn getEnvVar(self: Self, key: []const u8, allocator: Allocator) GetEnvVarError![]u8 {
    return self.vtable.getEnvVar(self.ctx, key, allocator);
}

/// Download `url` and extract into `dest_path` directory.
pub fn fetchUrl(self: Self, allocator: Allocator, url: []const u8, dest_path: []const u8) FetchUrlError!void {
    return self.vtable.fetchUrl(self.ctx, allocator, url, dest_path);
}

// --- Stdio wrapper methods ---

/// Write `data` to stdout.
pub fn writeStdout(self: Self, data: []const u8) StdioError!void {
    return self.vtable.writeStdout(self.ctx, data);
}

/// Write `data` to stderr.
pub fn writeStderr(self: Self, data: []const u8) StdioError!void {
    return self.vtable.writeStderr(self.ctx, data);
}

/// Read from stdin into `buf`. Returns bytes read.
pub fn readStdin(self: Self, buf: []u8) StdioError!usize {
    return self.vtable.readStdin(self.ctx, buf);
}

/// Return true if stdout is connected to a TTY.
pub fn isTty(self: Self) bool {
    return self.vtable.isTty(self.ctx);
}

// --- Error types ---
// All errors use plain error sets — no std.posix-specific types —
// so they compile on wasm32-freestanding.

/// Errors that can occur when reading a file.
pub const ReadError = error{
    FileNotFound,
    AccessDenied,
    OutOfMemory,
    StreamTooLong,
    IoError,
};

/// Errors that can occur when writing a file.
pub const WriteError = error{
    AccessDenied,
    OutOfMemory,
    IoError,
};

/// Errors that can occur when querying file metadata.
pub const StatError = error{
    FileNotFound,
    AccessDenied,
    IoError,
};

/// Backward-compat alias.
pub const GetFileInfoError = StatError;

/// Errors that can occur when listing directory contents.
pub const ListError = error{
    FileNotFound,
    AccessDenied,
    OutOfMemory,
    IoError,
};

/// Errors that can occur when creating directories.
pub const MakePathError = error{
    AccessDenied,
    OutOfMemory,
    IoError,
};

/// Errors that can occur when renaming a file.
pub const RenameError = error{
    FileNotFound,
    AccessDenied,
    IoError,
};

/// Errors that can occur when canonicalizing a path.
pub const CanonicalizeError = error{
    FileNotFound,
    AccessDenied,
    OutOfMemory,
    IoError,
};

/// Errors that can occur when looking up an environment variable.
pub const GetEnvVarError = error{
    EnvironmentVariableNotFound,
    OutOfMemory,
};

/// Errors that can occur when fetching a URL.
pub const FetchUrlError = error{
    Unsupported,
    DownloadFailed,
    OutOfMemory,
};

/// Errors that can occur with stdio operations.
pub const StdioError = error{
    IoError,
    BrokenPipe,
};

/// Distinguishes files from directories and other entry types.
pub const FileKind = enum {
    file,
    directory,
    other,
};

/// Metadata about a file or directory.
pub const FileInfo = struct {
    kind: FileKind,
    size: u64,
    /// Modification time in nanoseconds since Unix epoch, or null if unavailable.
    mtime_ns: ?i128,
};

/// An entry returned by `listDir`. Paths are absolute.
pub const FileEntry = struct {
    path: []const u8,
    kind: FileKind,
};

/// Maximum valid file size for readToEndAlloc calls.
pub const max_file_size = std.math.maxInt(u32);

/// Wraps an `Io` and intercepts `readFile` for a single path,
/// returning `content` instead of reading from disk.
///
/// All other vtable functions (writeFile, fileExists, stat, …) delegate to `base`.
/// This is safe when `base` is `Io.os()` or `Io.default()` because those vtable
/// functions ignore their `ctx` argument — so passing a `ReadFileOverride` pointer
/// as `ctx` causes no harm.
///
/// Usage:
/// ```zig
/// var override = Io.ReadFileOverride{ .path = path, .content = text };
/// const orig = env.filesystem;
/// env.filesystem = override.io();
/// env.build(path) catch {};
/// env.filesystem = orig;
/// ```
pub const ReadFileOverride = struct {
    path: []const u8,
    content: []const u8,
    /// Fallback I/O for paths other than `path`.
    /// Must be an implementation whose non-readFile vtable functions ignore `ctx`
    /// (e.g. Io.os() or Io.default()). This is true for all OS-backed instances.
    base: Self = os(),

    pub fn io(self: *@This()) Self {
        var v = self.base.vtable;
        v.readFile = &readFileOverrideFn;
        return .{ .ctx = @ptrCast(self), .vtable = v };
    }
};

fn readFileOverrideFn(ctx: ?*anyopaque, path: []const u8, allocator: Allocator) ReadError![]u8 {
    const self: *ReadFileOverride = @ptrCast(@alignCast(ctx.?));
    if (std.mem.eql(u8, path, self.path))
        return allocator.dupe(u8, self.content) catch return error.OutOfMemory;
    return self.base.vtable.readFile(self.base.ctx, path, allocator);
}

const is_freestanding = builtin.os.tag == .freestanding;

// --- Static vtable instances ---

const os_vtable: VTable = if (is_freestanding)
    freestanding_vtable
else
    .{
        .readFile = &OsImpl.osReadFile,
        .readFileInto = &OsImpl.osReadFileInto,
        .writeFile = &OsImpl.osWriteFile,
        .fileExists = &OsImpl.osFileExists,
        .stat = &OsImpl.osStat,
        .listDir = &OsImpl.osListDir,
        .dirName = &OsImpl.osDirName,
        .baseName = &OsImpl.osBaseName,
        .joinPath = &OsImpl.osJoinPath,
        .canonicalize = &OsImpl.osCanonicalize,
        .makePath = &OsImpl.osMakePath,
        .rename = &OsImpl.osRename,
        .getEnvVar = &OsImpl.osGetEnvVar,
        .fetchUrl = &OsImpl.osFetchUrl,
        .writeStdout = &OsImpl.osWriteStdout,
        .writeStderr = &OsImpl.osWriteStderr,
        .readStdin = &OsImpl.osReadStdin,
        .isTty = &OsImpl.osIsTty,
    };

const testing_vtable = VTable{
    .readFile = &testingReadFile,
    .readFileInto = &testingReadFileInto,
    .writeFile = &testingWriteFile,
    .fileExists = &testingFileExists,
    .stat = &testingStat,
    .listDir = &testingListDir,
    .dirName = &testingDirName,
    .baseName = &testingBaseName,
    .joinPath = &testingJoinPath,
    .canonicalize = &testingCanonicalize,
    .makePath = &testingMakePath,
    .rename = &testingRename,
    .getEnvVar = &testingGetEnvVar,
    .fetchUrl = &testingFetchUrl,
    .writeStdout = &testingWriteStdout,
    .writeStderr = &testingWriteStderr,
    .readStdin = &testingReadStdin,
    .isTty = &testingIsTty,
};

const freestanding_vtable = VTable{
    .readFile = &freestandingReadFile,
    .readFileInto = &freestandingReadFileInto,
    .writeFile = &freestandingWriteFile,
    .fileExists = &freestandingFileExists,
    .stat = &freestandingStat,
    .listDir = &freestandingListDir,
    .dirName = &freestandingDirName,
    .baseName = &freestandingBaseName,
    .joinPath = &freestandingJoinPath,
    .canonicalize = &freestandingCanonicalize,
    .makePath = &freestandingMakePath,
    .rename = &freestandingRename,
    .getEnvVar = &freestandingGetEnvVar,
    .fetchUrl = &freestandingFetchUrl,
    .writeStdout = &freestandingWriteStdout,
    .writeStderr = &freestandingWriteStderr,
    .readStdin = &freestandingReadStdin,
    .isTty = &freestandingIsTty,
};

/// Get the default implementation for the current target.
/// On wasm32-freestanding returns stubs; callers may override via `WasmFilesystem`.
pub fn default() Self {
    if (comptime is_freestanding) {
        return .{ .ctx = null, .vtable = freestanding_vtable };
    }
    return .{ .ctx = null, .vtable = os_vtable };
}

/// Get a real OS implementation (never returns freestanding stubs).
pub fn os() Self {
    return .{ .ctx = null, .vtable = os_vtable };
}

/// Get a test implementation where every call panics.
/// Override individual vtable fields in your test to provide mock behavior.
pub fn testing() Self {
    return .{ .ctx = null, .vtable = testing_vtable };
}

// --- OS implementations ---
const OsImpl = if (is_freestanding) struct {} else struct {
fn osReadFile(_: ?*anyopaque, path: []const u8, allocator: Allocator) ReadError![]u8 {
    const file = std.fs.cwd().openFile(path, .{}) catch |err| return switch (err) {
        error.FileNotFound => error.FileNotFound,
        error.AccessDenied => error.AccessDenied,
        else => error.IoError,
    };
    defer file.close();
    return file.readToEndAlloc(allocator, max_file_size) catch |err| return switch (err) {
        error.OutOfMemory => error.OutOfMemory,
        else => error.IoError,
    };
}

fn osReadFileInto(_: ?*anyopaque, path: []const u8, buffer: []u8) ReadError!usize {
    const file = std.fs.cwd().openFile(path, .{}) catch |err| return switch (err) {
        error.FileNotFound => error.FileNotFound,
        error.AccessDenied => error.AccessDenied,
        else => error.IoError,
    };
    defer file.close();
    return file.readAll(buffer) catch return error.IoError;
}

fn osWriteFile(_: ?*anyopaque, path: []const u8, data: []const u8) WriteError!void {
    std.fs.cwd().writeFile(.{ .sub_path = path, .data = data }) catch |err| return switch (err) {
        error.AccessDenied => error.AccessDenied,
        else => error.IoError,
    };
}

fn osFileExists(_: ?*anyopaque, path: []const u8) bool {
    std.fs.cwd().access(path, .{}) catch return false;
    return true;
}

fn osStat(_: ?*anyopaque, path: []const u8) StatError!FileInfo {
    const s = std.fs.cwd().statFile(path) catch |err| return switch (err) {
        error.FileNotFound => error.FileNotFound,
        error.AccessDenied => error.AccessDenied,
        else => error.IoError,
    };
    return FileInfo{
        .kind = switch (s.kind) {
            .file => .file,
            .directory => .directory,
            else => .other,
        },
        .size = s.size,
        .mtime_ns = s.mtime,
    };
}

fn osListDir(_: ?*anyopaque, path: []const u8, allocator: Allocator) ListError![]FileEntry {
    var dir = std.fs.cwd().openDir(path, .{ .iterate = true }) catch |err| return switch (err) {
        error.FileNotFound => error.FileNotFound,
        error.AccessDenied => error.AccessDenied,
        else => error.IoError,
    };
    defer dir.close();

    var walker = dir.walk(allocator) catch return error.IoError;
    defer walker.deinit();

    var entries: std.ArrayList(FileEntry) = .empty;
    errdefer {
        for (entries.items) |entry| allocator.free(entry.path);
        entries.deinit(allocator);
    }

    while (true) {
        const next = walker.next() catch return error.IoError;
        const entry = next orelse break;
        const kind: FileKind = switch (entry.kind) {
            .file => .file,
            .directory => .directory,
            else => .other,
        };
        const owned_path = std.fs.path.join(allocator, &.{ path, entry.path }) catch return error.OutOfMemory;
        entries.append(allocator, .{ .path = owned_path, .kind = kind }) catch {
            allocator.free(owned_path);
            return error.OutOfMemory;
        };
    }

    return entries.toOwnedSlice(allocator) catch return error.OutOfMemory;
}

fn osDirName(_: ?*anyopaque, path: []const u8) ?[]const u8 {
    return std.fs.path.dirname(path);
}

fn osBaseName(_: ?*anyopaque, path: []const u8) []const u8 {
    return std.fs.path.basename(path);
}

fn osJoinPath(_: ?*anyopaque, parts: []const []const u8, allocator: Allocator) Allocator.Error![]const u8 {
    return std.fs.path.join(allocator, parts);
}

fn osCanonicalize(_: ?*anyopaque, path: []const u8, allocator: Allocator) CanonicalizeError![]const u8 {
    return std.fs.realpathAlloc(allocator, path) catch |err| return switch (err) {
        error.FileNotFound => error.FileNotFound,
        error.AccessDenied => error.AccessDenied,
        error.OutOfMemory => error.OutOfMemory,
        else => error.IoError,
    };
}

fn osMakePath(_: ?*anyopaque, path: []const u8) MakePathError!void {
    std.fs.cwd().makePath(path) catch |err| return switch (err) {
        error.AccessDenied => error.AccessDenied,
        else => error.IoError,
    };
}

fn osRename(_: ?*anyopaque, old_path: []const u8, new_path: []const u8) RenameError!void {
    std.fs.cwd().rename(old_path, new_path) catch |err| return switch (err) {
        error.FileNotFound => error.FileNotFound,
        error.AccessDenied => error.AccessDenied,
        else => error.IoError,
    };
}

fn osGetEnvVar(_: ?*anyopaque, key: []const u8, allocator: Allocator) GetEnvVarError![]u8 {
    return std.process.getEnvVarOwned(allocator, key) catch |err| return switch (err) {
        error.OutOfMemory => error.OutOfMemory,
        else => error.EnvironmentVariableNotFound,
    };
}

/// fetchUrl is intentionally a stub in the default OS vtable.
/// Real HTTP download support is injected by BuildEnv.init() using nativeFetchUrl.
/// Callers constructing their own Io for download support should set vtable.fetchUrl
/// to a suitable implementation before use.
fn osFetchUrl(_: ?*anyopaque, _: Allocator, _: []const u8, _: []const u8) FetchUrlError!void {
    return error.Unsupported;
}

fn osWriteStdout(_: ?*anyopaque, data: []const u8) StdioError!void {
    std.fs.File.stdout().writeAll(data) catch |err| return switch (err) {
        error.BrokenPipe => error.BrokenPipe,
        else => error.IoError,
    };
}

fn osWriteStderr(_: ?*anyopaque, data: []const u8) StdioError!void {
    std.fs.File.stderr().writeAll(data) catch |err| return switch (err) {
        error.BrokenPipe => error.BrokenPipe,
        else => error.IoError,
    };
}

fn osReadStdin(_: ?*anyopaque, buf: []u8) StdioError!usize {
    return std.fs.File.stdin().read(buf) catch |err| return switch (err) {
        error.BrokenPipe => error.BrokenPipe,
        else => error.IoError,
    };
}

fn osIsTty(_: ?*anyopaque) bool {
    return std.fs.File.stdout().isTty();
}
};

// --- Testing implementations — panic on every call ---

fn testingReadFile(_: ?*anyopaque, _: []const u8, _: Allocator) ReadError![]u8 {
    @panic("readFile should not be called in this test");
}

fn testingReadFileInto(_: ?*anyopaque, _: []const u8, _: []u8) ReadError!usize {
    @panic("readFileInto should not be called in this test");
}

fn testingWriteFile(_: ?*anyopaque, _: []const u8, _: []const u8) WriteError!void {
    @panic("writeFile should not be called in this test");
}

fn testingFileExists(_: ?*anyopaque, _: []const u8) bool {
    @panic("fileExists should not be called in this test");
}

fn testingStat(_: ?*anyopaque, _: []const u8) StatError!FileInfo {
    @panic("stat should not be called in this test");
}

fn testingListDir(_: ?*anyopaque, _: []const u8, _: Allocator) ListError![]FileEntry {
    @panic("listDir should not be called in this test");
}

fn testingDirName(_: ?*anyopaque, path: []const u8) ?[]const u8 {
    return std.fs.path.dirname(path);
}

fn testingBaseName(_: ?*anyopaque, path: []const u8) []const u8 {
    return std.fs.path.basename(path);
}

fn testingJoinPath(_: ?*anyopaque, parts: []const []const u8, allocator: Allocator) Allocator.Error![]const u8 {
    return std.fs.path.join(allocator, parts);
}

fn testingCanonicalize(_: ?*anyopaque, _: []const u8, _: Allocator) CanonicalizeError![]const u8 {
    @panic("canonicalize should not be called in this test");
}

fn testingMakePath(_: ?*anyopaque, _: []const u8) MakePathError!void {
    @panic("makePath should not be called in this test");
}

fn testingRename(_: ?*anyopaque, _: []const u8, _: []const u8) RenameError!void {
    @panic("rename should not be called in this test");
}

fn testingGetEnvVar(_: ?*anyopaque, _: []const u8, _: Allocator) GetEnvVarError![]u8 {
    return error.EnvironmentVariableNotFound;
}

fn testingFetchUrl(_: ?*anyopaque, _: Allocator, _: []const u8, _: []const u8) FetchUrlError!void {
    return error.Unsupported;
}

fn testingWriteStdout(_: ?*anyopaque, _: []const u8) StdioError!void {
    @panic("writeStdout should not be called in this test");
}

fn testingWriteStderr(_: ?*anyopaque, _: []const u8) StdioError!void {
    @panic("writeStderr should not be called in this test");
}

fn testingReadStdin(_: ?*anyopaque, _: []u8) StdioError!usize {
    @panic("readStdin should not be called in this test");
}

fn testingIsTty(_: ?*anyopaque) bool {
    return false;
}

// --- Freestanding implementations —
// Used on wasm32-freestanding where there is no real filesystem or stdio.
// Callers must override with a proper implementation (e.g. WasmFilesystem).

fn freestandingReadFile(_: ?*anyopaque, _: []const u8, _: Allocator) ReadError![]u8 {
    return error.FileNotFound;
}

fn freestandingReadFileInto(_: ?*anyopaque, _: []const u8, _: []u8) ReadError!usize {
    return error.FileNotFound;
}

fn freestandingWriteFile(_: ?*anyopaque, _: []const u8, _: []const u8) WriteError!void {
    return error.AccessDenied;
}

fn freestandingFileExists(_: ?*anyopaque, _: []const u8) bool {
    return false;
}

fn freestandingStat(_: ?*anyopaque, _: []const u8) StatError!FileInfo {
    return error.FileNotFound;
}

fn freestandingListDir(_: ?*anyopaque, _: []const u8, _: Allocator) ListError![]FileEntry {
    return error.FileNotFound;
}

fn freestandingDirName(_: ?*anyopaque, path: []const u8) ?[]const u8 {
    if (std.mem.lastIndexOfScalar(u8, path, '/')) |last_slash| {
        if (last_slash == 0) return "/";
        return path[0..last_slash];
    }
    return null;
}

fn freestandingBaseName(_: ?*anyopaque, path: []const u8) []const u8 {
    if (std.mem.lastIndexOfScalar(u8, path, '/')) |last_slash| {
        return path[last_slash + 1 ..];
    }
    return path;
}

fn freestandingJoinPath(_: ?*anyopaque, parts: []const []const u8, allocator: Allocator) Allocator.Error![]const u8 {
    var total: usize = 0;
    for (parts, 0..) |part, i| {
        total += part.len;
        if (i < parts.len - 1) total += 1;
    }
    const buf = try allocator.alloc(u8, total);
    var pos: usize = 0;
    for (parts, 0..) |part, i| {
        @memcpy(buf[pos..][0..part.len], part);
        pos += part.len;
        if (i < parts.len - 1) {
            buf[pos] = '/';
            pos += 1;
        }
    }
    return buf;
}

fn freestandingCanonicalize(_: ?*anyopaque, path: []const u8, allocator: Allocator) CanonicalizeError![]const u8 {
    // Best-effort on freestanding: return a copy of the input unchanged.
    return allocator.dupe(u8, path) catch return error.OutOfMemory;
}

fn freestandingMakePath(_: ?*anyopaque, _: []const u8) MakePathError!void {
    return error.AccessDenied;
}

fn freestandingRename(_: ?*anyopaque, _: []const u8, _: []const u8) RenameError!void {
    return error.AccessDenied;
}

fn freestandingGetEnvVar(_: ?*anyopaque, _: []const u8, _: Allocator) GetEnvVarError![]u8 {
    return error.EnvironmentVariableNotFound;
}

fn freestandingFetchUrl(_: ?*anyopaque, _: Allocator, _: []const u8, _: []const u8) FetchUrlError!void {
    return error.Unsupported;
}

fn freestandingWriteStdout(_: ?*anyopaque, _: []const u8) StdioError!void {
    return error.IoError;
}

fn freestandingWriteStderr(_: ?*anyopaque, _: []const u8) StdioError!void {
    return error.IoError;
}

fn freestandingReadStdin(_: ?*anyopaque, _: []u8) StdioError!usize {
    return 0;
}

fn freestandingIsTty(_: ?*anyopaque) bool {
    return false;
}

// --- Tests ---

test "os() creates an Io that can call dirName and baseName" {
    const fs = os();
    try std.testing.expectEqualStrings("foo", fs.dirName("foo/bar").?);
    try std.testing.expectEqualStrings("bar", fs.baseName("foo/bar"));
}

test "default() returns an Io" {
    const fs = default();
    try std.testing.expect(fs.dirName("a/b") != null);
    try std.testing.expectEqualStrings("b", fs.baseName("a/b"));
}

test "testing() has safe pure methods" {
    const fs = testing();
    try std.testing.expectEqualStrings("b", fs.baseName("a/b"));
    try std.testing.expect(!fs.isTty());
}

test "freestanding stubs return expected errors" {
    const fs = Self{ .ctx = null, .vtable = freestanding_vtable };
    try std.testing.expectError(error.FileNotFound, fs.readFile("x", std.testing.allocator));
    try std.testing.expectError(error.AccessDenied, fs.writeFile("x", "y"));
    try std.testing.expect(!fs.fileExists("x"));
    try std.testing.expectError(error.FileNotFound, fs.stat("x"));
    try std.testing.expectError(error.FileNotFound, fs.listDir("x", std.testing.allocator));
    try std.testing.expectError(error.AccessDenied, fs.makePath("x"));
    try std.testing.expectError(error.AccessDenied, fs.rename("x", "y"));
    try std.testing.expectError(error.IoError, fs.writeStdout("hi"));
    try std.testing.expectError(error.IoError, fs.writeStderr("hi"));
    try std.testing.expect(!fs.isTty());
}

test "freestanding dirName and baseName" {
    const fs = Self{ .ctx = null, .vtable = freestanding_vtable };
    try std.testing.expectEqualStrings("/usr", fs.dirName("/usr/bin").?);
    try std.testing.expectEqualStrings("bin", fs.baseName("/usr/bin"));
    try std.testing.expectEqualStrings("/", fs.dirName("/bin").?);
    try std.testing.expect(fs.dirName("nodir") == null);
    try std.testing.expectEqualStrings("nodir", fs.baseName("nodir"));
}

test "freestanding joinPath" {
    const fs = Self{ .ctx = null, .vtable = freestanding_vtable };
    const joined = try fs.joinPath(&.{ "a", "b", "c" }, std.testing.allocator);
    defer std.testing.allocator.free(joined);
    try std.testing.expectEqualStrings("a/b/c", joined);
}

test "freestanding readStdin returns 0" {
    const fs = Self{ .ctx = null, .vtable = freestanding_vtable };
    var buf: [16]u8 = undefined;
    const n = try fs.readStdin(&buf);
    try std.testing.expectEqual(@as(usize, 0), n);
}

test "freestanding canonicalize returns copy of input" {
    const fs = Self{ .ctx = null, .vtable = freestanding_vtable };
    const result = try fs.canonicalize("/some/path", std.testing.allocator);
    defer std.testing.allocator.free(result);
    try std.testing.expectEqualStrings("/some/path", result);
}
