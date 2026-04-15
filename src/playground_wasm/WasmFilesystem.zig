//! WASM-specific filesystem implementation for the playground.
//! This provides a minimal filesystem interface where source code
//! can be provided from JavaScript and most other operations return errors.

const std = @import("std");
const io_mod = @import("io");
const RocIo = io_mod.RocIo;

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
pub fn wasm(wasm_ctx: *WasmContext) RocIo {
    return .{ .ctx = @ptrCast(wasm_ctx), .vtable = wasm_vtable };
}

const wasm_vtable = RocIo.VTable{
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

fn fileExistsWasm(ctx_ptr: ?*anyopaque, path: []const u8) bool {
    const self = getCtx(ctx_ptr);
    return matchesSourceFile(self, path);
}

fn readFileWasm(ctx_ptr: ?*anyopaque, path: []const u8, alloc: Allocator) RocIo.ReadError![]u8 {
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

fn readFileIntoWasm(ctx_ptr: ?*anyopaque, path: []const u8, buffer: []u8) RocIo.ReadError!usize {
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

fn writeFileWasm(_: ?*anyopaque, _: []const u8, _: []const u8) RocIo.WriteError!void {
    return error.AccessDenied;
}

fn statWasm(ctx_ptr: ?*anyopaque, path: []const u8) RocIo.StatError!RocIo.FileInfo {
    const self = getCtx(ctx_ptr);
    if (matchesSourceFile(self, path)) {
        if (self.source) |source| {
            return RocIo.FileInfo{
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

fn listDirWasm(_: ?*anyopaque, _: []const u8, _: Allocator) RocIo.ListError![]RocIo.FileEntry {
    return error.FileNotFound;
}

fn dirNameWasm(_: ?*anyopaque, absolute_path: []const u8) ?[]const u8 {
    if (std.mem.lastIndexOfScalar(u8, absolute_path, '/')) |last_slash| {
        if (last_slash == 0) {
            return "/";
        }
        return absolute_path[0..last_slash];
    }
    return null;
}

fn baseNameWasm(_: ?*anyopaque, absolute_path: []const u8) []const u8 {
    if (std.mem.lastIndexOfScalar(u8, absolute_path, '/')) |last_slash| {
        return absolute_path[last_slash + 1 ..];
    }
    return absolute_path;
}

fn joinPathWasm(_: ?*anyopaque, parts: []const []const u8, allocator: Allocator) Allocator.Error![]const u8 {
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

fn canonicalizeWasm(_: ?*anyopaque, root_relative_path: []const u8, alloc: Allocator) RocIo.CanonicalizeError![]const u8 {
    return alloc.dupe(u8, root_relative_path) catch handleOom();
}

fn makePathWasm(_: ?*anyopaque, _: []const u8) RocIo.MakePathError!void {
    return error.AccessDenied;
}

fn renameWasm(_: ?*anyopaque, _: []const u8, _: []const u8) RocIo.RenameError!void {
    return error.AccessDenied;
}

fn getEnvVarWasm(_: ?*anyopaque, _: []const u8, _: Allocator) RocIo.GetEnvVarError![]u8 {
    return error.EnvironmentVariableNotFound;
}

fn fetchUrlWasm(_: ?*anyopaque, _: Allocator, _: []const u8, _: []const u8) RocIo.FetchUrlError!void {
    return error.Unsupported;
}

fn writeStdoutWasm(_: ?*anyopaque, _: []const u8) RocIo.StdioError!void {
    // WASM: stdout silently dropped (JS host can intercept via import override if desired)
}

fn writeStderrWasm(_: ?*anyopaque, _: []const u8) RocIo.StdioError!void {
    // WASM: stderr silently dropped
}

fn readStdinWasm(_: ?*anyopaque, _: []u8) RocIo.StdioError!usize {
    return 0;
}

fn isTtyWasm(_: ?*anyopaque) bool {
    return false;
}
