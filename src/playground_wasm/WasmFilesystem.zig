//! WASM-specific filesystem implementation for the playground.
//! This provides a minimal filesystem interface where source code
//! can be provided from JavaScript and most other operations return errors.

const std = @import("std");
const ctx_mod = @import("ctx");
const CoreCtx = ctx_mod.CoreCtx;

const Allocator = std.mem.Allocator;

// Helper function to handle OOM errors
fn handleOom() noreturn {
    @panic("Out of memory");
}

/// Context struct holding state for the WASM filesystem.
/// Replaces previous global mutable state.
pub const WasmContext = struct {
    source: ?[]const u8 = null,
    allocator: ?Allocator = null,
    filename: []const u8 = "main.roc",

    /// Set the source code that will be returned by readFile.
    pub fn setSource(self: *WasmContext, alloc: Allocator, source: []const u8) void {
        // Free previous source if it exists
        if (self.source) |prev_source| {
            if (self.allocator) |prev_allocator| {
                prev_allocator.free(prev_source);
            }
        }

        // Store new source
        self.source = alloc.dupe(u8, source) catch handleOom();
        self.allocator = alloc;
    }

    /// Set the filename for the source file.
    pub fn setFilename(self: *WasmContext, alloc: Allocator, filename: []const u8) void {
        // Free previous filename if it was allocated (not the default)
        if (self.allocator) |prev_allocator| {
            if (self.filename.ptr != "main.roc".ptr) {
                prev_allocator.free(self.filename);
            }
        }

        // Store new filename
        self.filename = alloc.dupe(u8, filename) catch handleOom();
        self.allocator = alloc;
    }
};

/// Get a WASM filesystem implementation backed by the given context.
pub fn wasm(wasm_ctx: *WasmContext, alloc: Allocator, std_io: std.Io) CoreCtx {
    return .{ .ctx = @ptrCast(wasm_ctx), .vtable = wasm_vtable, .std_io = std_io, .gpa = alloc, .arena = alloc };
}

const wasm_vtable = CoreCtx.VTable{
    .readFile = &readFileWasm,
    .readFileInto = &readFileIntoWasm,
    .writeFile = &writeFileWasm,
    .fileExists = &fileExistsWasm,
    .stat = &statWasm,
    .listDir = &listDirWasm,
    .dirName = &dirNameWasm,
    .baseName = &baseNameWasm,
    .joinPath = &joinPathWasm,
    .canonicalize = &canonicalizeWasm,
    .makePath = &makePathWasm,
    .rename = &renameWasm,
    .getEnvVar = &getEnvVarWasm,
    .fetchUrl = &fetchUrlWasm,
    .deleteFile = &deleteFileWasm,
    .deleteDir = &deleteDirWasm,
    .deleteTree = &deleteTreeWasm,
    .createDir = &createDirWasm,
    .copyFile = &copyFileWasm,
    .timestampNow = &timestampNowWasm,
    .writeStdout = &writeStdoutWasm,
    .writeStderr = &writeStderrWasm,
    .readStdin = &readStdinWasm,
    .isTty = &isTtyWasm,
};

/// Recover the WasmContext from an opaque pointer.
fn getCtx(ctx_ptr: ?*anyopaque) *WasmContext {
    return @ptrCast(@alignCast(ctx_ptr.?));
}

/// Check if the given path matches the current source filename.
fn matchesSourceFile(self: *WasmContext, path: []const u8) bool {
    // Check exact match
    if (std.mem.eql(u8, path, self.filename)) return true;
    // Check with leading slash
    if (path.len > 0 and path[0] == '/') {
        if (std.mem.eql(u8, path[1..], self.filename)) return true;
    }
    // Check if path ends with /filename
    if (std.mem.endsWith(u8, path, self.filename)) {
        const prefix_len = path.len - self.filename.len;
        if (prefix_len > 0 and path[prefix_len - 1] == '/') return true;
    }
    return false;
}

fn fileExistsWasm(ctx_ptr: ?*anyopaque, _: std.Io, path: []const u8) bool {
    const self = getCtx(ctx_ptr);
    return matchesSourceFile(self, path);
}

fn readFileWasm(ctx_ptr: ?*anyopaque, _: std.Io, path: []const u8, alloc: Allocator) CoreCtx.ReadError![]u8 {
    const self = getCtx(ctx_ptr);
    if (matchesSourceFile(self, path)) {
        if (self.source) |source| {
            return alloc.dupe(u8, source) catch handleOom();
        } else {
            return error.FileNotFound;
        }
    }
    return error.FileNotFound;
}

fn readFileIntoWasm(ctx_ptr: ?*anyopaque, _: std.Io, path: []const u8, buffer: []u8) CoreCtx.ReadError!usize {
    const self = getCtx(ctx_ptr);
    if (matchesSourceFile(self, path)) {
        if (self.source) |source| {
            if (buffer.len < source.len) {
                return error.StreamTooLong;
            }
            @memcpy(buffer[0..source.len], source);
            return source.len;
        } else {
            return error.FileNotFound;
        }
    }
    return error.FileNotFound;
}

fn writeFileWasm(_: ?*anyopaque, _: std.Io, _: []const u8, _: []const u8) CoreCtx.WriteError!void {
    return error.AccessDenied;
}

fn statWasm(ctx_ptr: ?*anyopaque, _: std.Io, path: []const u8) CoreCtx.StatError!CoreCtx.FileInfo {
    const self = getCtx(ctx_ptr);
    if (matchesSourceFile(self, path)) {
        if (self.source) |source| {
            return CoreCtx.FileInfo{
                .kind = .file,
                .size = source.len,
                .mtime_ns = 0,
            };
        } else {
            return error.FileNotFound;
        }
    }
    return error.FileNotFound;
}

fn listDirWasm(_: ?*anyopaque, _: std.Io, _: []const u8, _: Allocator) CoreCtx.ListError![]CoreCtx.FileEntry {
    return error.FileNotFound;
}

fn dirNameWasm(_: ?*anyopaque, _: std.Io, absolute_path: []const u8) ?[]const u8 {
    if (std.mem.findScalarLast(u8, absolute_path, '/')) |last_slash| {
        if (last_slash == 0) {
            return "/";
        }
        return absolute_path[0..last_slash];
    }
    return null;
}

fn baseNameWasm(_: ?*anyopaque, _: std.Io, absolute_path: []const u8) []const u8 {
    if (std.mem.findScalarLast(u8, absolute_path, '/')) |last_slash| {
        return absolute_path[last_slash + 1 ..];
    }
    return absolute_path;
}

fn joinPathWasm(_: ?*anyopaque, _: std.Io, parts: []const []const u8, allocator: Allocator) Allocator.Error![]const u8 {
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

fn canonicalizeWasm(_: ?*anyopaque, _: std.Io, root_relative_path: []const u8, alloc: Allocator) CoreCtx.CanonicalizeError![]const u8 {
    return alloc.dupe(u8, root_relative_path) catch handleOom();
}

fn makePathWasm(_: ?*anyopaque, _: std.Io, _: []const u8) CoreCtx.MakePathError!void {
    return error.AccessDenied;
}

fn renameWasm(_: ?*anyopaque, _: std.Io, _: []const u8, _: []const u8) CoreCtx.RenameError!void {
    return error.AccessDenied;
}

fn getEnvVarWasm(_: ?*anyopaque, _: std.Io, _: []const u8, _: Allocator) CoreCtx.GetEnvVarError![]u8 {
    return error.EnvironmentVariableMissing;
}

fn fetchUrlWasm(_: ?*anyopaque, _: std.Io, _: Allocator, _: []const u8, _: []const u8) CoreCtx.FetchUrlError!void {
    return error.Unsupported;
}

fn deleteFileWasm(_: ?*anyopaque, _: std.Io, _: []const u8) CoreCtx.DeleteError!void {
    return error.AccessDenied;
}

fn deleteDirWasm(_: ?*anyopaque, _: std.Io, _: []const u8) CoreCtx.DeleteError!void {
    return error.AccessDenied;
}

fn deleteTreeWasm(_: ?*anyopaque, _: std.Io, _: []const u8) CoreCtx.DeleteError!void {
    return error.AccessDenied;
}

fn createDirWasm(_: ?*anyopaque, _: std.Io, _: []const u8) CoreCtx.MakePathError!void {
    return error.AccessDenied;
}

fn copyFileWasm(_: ?*anyopaque, _: std.Io, _: []const u8, _: []const u8) CoreCtx.CopyError!void {
    return error.AccessDenied;
}

fn timestampNowWasm(_: ?*anyopaque, _: std.Io) i128 {
    return 0;
}

fn writeStdoutWasm(_: ?*anyopaque, _: std.Io, _: []const u8) CoreCtx.StdioError!void {
    // WASM: stdout silently dropped (JS host can intercept via import override if desired)
}

fn writeStderrWasm(_: ?*anyopaque, _: std.Io, _: []const u8) CoreCtx.StdioError!void {
    // WASM: stderr silently dropped
}

fn readStdinWasm(_: ?*anyopaque, _: std.Io, _: []u8) CoreCtx.StdioError!usize {
    return 0;
}

fn isTtyWasm(_: ?*anyopaque, _: std.Io) bool {
    return false;
}
