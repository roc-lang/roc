//! WASM-specific filesystem implementation for the playground.
//! This provides a minimal filesystem interface where source code
//! can be provided from JavaScript and most other operations return errors.

const std = @import("std");
const fs_mod = @import("fs");
const Filesystem = fs_mod.Filesystem;

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
pub fn wasm(wasm_ctx: *WasmContext) Filesystem {
    return .{ .ctx = @ptrCast(wasm_ctx), .vtable = wasm_vtable };
}

const wasm_vtable = Filesystem.VTable{
    .fileExists = &fileExistsWasm,
    .readFile = &readFileWasm,
    .readFileInto = &readFileIntoWasm,
    .writeFile = &writeFileWasm,
    .openDir = &openDirWasm,
    .dirName = &dirNameWasm,
    .baseName = &baseNameWasm,
    .canonicalize = &canonicalizeWasm,
    .makePath = &makePathWasm,
    .rename = &renameWasm,
    .getFileInfo = &getFileInfoWasm,
    .getEnvVar = &getEnvVarWasm,
    .fetchUrl = &fetchUrlWasm,
};

/// Recover the WasmContext from an opaque pointer.
fn getCtx(ctx_ptr: ?*anyopaque) *WasmContext {
    return @ptrCast(@alignCast(ctx_ptr.?));
}

/// Check if the given path matches the current source filename
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

fn fileExistsWasm(ctx_ptr: ?*anyopaque, absolute_path: []const u8) Filesystem.OpenError!bool {
    const self = getCtx(ctx_ptr);
    return matchesSourceFile(self, absolute_path);
}

fn readFileWasm(ctx_ptr: ?*anyopaque, relative_path: []const u8, alloc: Allocator) Filesystem.ReadError![]const u8 {
    const self = getCtx(ctx_ptr);
    if (matchesSourceFile(self, relative_path)) {
        if (self.source) |source| {
            return alloc.dupe(u8, source) catch handleOom();
        } else {
            return error.FileNotFound;
        }
    }

    return error.FileNotFound;
}

fn readFileIntoWasm(ctx_ptr: ?*anyopaque, path: []const u8, buffer: []u8) Filesystem.ReadError!usize {
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

fn writeFileWasm(_: ?*anyopaque, _: []const u8, _: []const u8) Filesystem.WriteError!void {
    return error.AccessDenied;
}

fn openDirWasm(_: ?*anyopaque, _: []const u8) Filesystem.OpenError!Filesystem.Dir {
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

fn baseNameWasm(_: ?*anyopaque, absolute_path: []const u8) ?[]const u8 {
    if (std.mem.lastIndexOfScalar(u8, absolute_path, '/')) |last_slash| {
        return absolute_path[last_slash + 1 ..];
    }
    return absolute_path;
}

fn canonicalizeWasm(_: ?*anyopaque, root_relative_path: []const u8, alloc: Allocator) Filesystem.CanonicalizeError![]const u8 {
    return alloc.dupe(u8, root_relative_path) catch handleOom();
}

fn makePathWasm(_: ?*anyopaque, _: []const u8) Filesystem.MakePathError!void {
    return error.AccessDenied;
}

fn renameWasm(_: ?*anyopaque, _: []const u8, _: []const u8) Filesystem.RenameError!void {
    return error.AccessDenied;
}

fn getFileInfoWasm(ctx_ptr: ?*anyopaque, path: []const u8) Filesystem.GetFileInfoError!Filesystem.FileInfo {
    const self = getCtx(ctx_ptr);
    if (matchesSourceFile(self, path)) {
        if (self.source) |source| {
            return Filesystem.FileInfo{
                .mtime_ns = 0,
                .size = source.len,
            };
        } else {
            return error.FileNotFound;
        }
    }

    return error.FileNotFound;
}

fn getEnvVarWasm(_: ?*anyopaque, _: []const u8, _: Allocator) Filesystem.GetEnvVarError![]u8 {
    return error.EnvironmentVariableNotFound;
}

fn fetchUrlWasm(_: ?*anyopaque, _: Allocator, _: []const u8, _: std.fs.Dir) Filesystem.FetchUrlError!void {
    return error.Unsupported;
}
