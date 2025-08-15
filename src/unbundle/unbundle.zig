//! Unbundle compressed tar archives using Zig's standard library
//!
//! This module provides unbundling functionality that works on all platforms
//! including WebAssembly, by using Zig's std.compress.zstd instead of
//! the C zstd library.

const builtin = @import("builtin");
const std = @import("std");
const base58 = @import("base58");

// Constants
const TAR_EXTENSION = ".tar.zst";
const STREAM_BUFFER_SIZE: usize = 64 * 1024; // 64KB buffer for streaming operations

/// Size of the decompression window buffer for zstd.
/// 8MB (2^23 bytes) is the default and recommended size for zstd decompression.
/// This matches zstd's default maximum window size, allowing us to decompress
/// any standard zstd stream. Smaller buffers would fail on streams compressed
/// with larger window sizes.
const ZSTD_WINDOW_BUFFER_SIZE: usize = 1 << 23; // 8MB

/// Errors that can occur during the unbundle operation.
pub const UnbundleError = error{
    DecompressionFailed,
    InvalidTarHeader,
    UnexpectedEndOfStream,
    FileCreateFailed,
    DirectoryCreateFailed,
    FileWriteFailed,
    HashMismatch,
    InvalidFilename,
    FileTooLarge,
    InvalidPath,
    NoDataExtracted,
    ChecksumFailure,
    DictionaryIdFlagUnsupported,
    MalformedBlock,
    MalformedFrame,
} || std.mem.Allocator.Error;

/// Context for error reporting during unbundle operations
pub const ErrorContext = struct {
    path: []const u8,
    reason: PathValidationReason,
};

/// Specific reason why a path validation failed
pub const PathValidationReason = union(enum) {
    empty_path,
    path_too_long,
    windows_reserved_char: u8,
    absolute_path,
    path_traversal,
    current_directory_reference,
    windows_reserved_name,
    contained_backslash_on_unix,
    component_ends_with_space,
    component_ends_with_period,
};

/// Virtual table for extract operations
pub const ExtractWriter = struct {
    ptr: *anyopaque,
    vtable: *const VTable,

    pub const VTable = struct {
        createFile: *const fn (ptr: *anyopaque, path: []const u8) CreateFileError!std.io.AnyWriter,
        finishFile: *const fn (ptr: *anyopaque, writer: std.io.AnyWriter) FinishFileError!void,
        makeDir: *const fn (ptr: *anyopaque, path: []const u8) MakeDirError!void,
    };

    pub const CreateFileError = error{
        FileCreateFailed,
        OutOfMemory,
    };

    pub const FinishFileError = error{
        // No specific errors for finish
        };

    pub const MakeDirError = error{
        DirectoryCreateFailed,
    };

    pub fn createFile(self: ExtractWriter, path: []const u8) CreateFileError!std.io.AnyWriter {
        return self.vtable.createFile(self.ptr, path);
    }

    pub fn finishFile(self: ExtractWriter, writer: std.io.AnyWriter) FinishFileError!void {
        return self.vtable.finishFile(self.ptr, writer);
    }

    pub fn makeDir(self: ExtractWriter, path: []const u8) MakeDirError!void {
        return self.vtable.makeDir(self.ptr, path);
    }
};

/// Directory-based extract writer for filesystem extraction
pub const DirExtractWriter = struct {
    dir: std.fs.Dir,
    allocator: std.mem.Allocator,
    open_files: std.ArrayList(std.fs.File),

    pub fn init(dir: std.fs.Dir, allocator: std.mem.Allocator) DirExtractWriter {
        return .{
            .dir = dir,
            .allocator = allocator,
            .open_files = std.ArrayList(std.fs.File).init(allocator),
        };
    }

    pub fn deinit(self: *DirExtractWriter) void {
        // Close any remaining open files
        for (self.open_files.items) |*file| {
            file.close();
        }
        self.open_files.deinit();
    }

    pub fn extractWriter(self: *DirExtractWriter) ExtractWriter {
        return ExtractWriter{
            .ptr = self,
            .vtable = &vtable,
        };
    }

    const vtable = ExtractWriter.VTable{
        .createFile = createFile,
        .finishFile = finishFile,
        .makeDir = makeDir,
    };

    fn createFile(ptr: *anyopaque, path: []const u8) ExtractWriter.CreateFileError!std.io.AnyWriter {
        const self: *DirExtractWriter = @ptrCast(@alignCast(ptr));

        // Ensure parent directories exist
        if (std.fs.path.dirname(path)) |parent| {
            self.dir.makePath(parent) catch return error.FileCreateFailed;
        }

        const file = self.dir.createFile(path, .{}) catch return error.FileCreateFailed;
        self.open_files.append(file) catch {
            file.close();
            return error.OutOfMemory;
        };

        // Return a custom AnyWriter that references the file directly
        return .{
            .context = @ptrCast(&self.open_files.items[self.open_files.items.len - 1]),
            .writeFn = fileWrite,
        };
    }

    fn fileWrite(context: *const anyopaque, bytes: []const u8) anyerror!usize {
        const file: *std.fs.File = @ptrCast(@alignCast(@constCast(context)));
        return file.write(bytes);
    }

    fn finishFile(ptr: *anyopaque, _: std.io.AnyWriter) ExtractWriter.FinishFileError!void {
        const self: *DirExtractWriter = @ptrCast(@alignCast(ptr));
        // Close and remove the last file
        if (self.open_files.items.len > 0) {
            const last_idx = self.open_files.items.len - 1;
            self.open_files.items[last_idx].close();
            _ = self.open_files.orderedRemove(last_idx);
        }
    }

    fn makeDir(ptr: *anyopaque, path: []const u8) ExtractWriter.MakeDirError!void {
        const self: *DirExtractWriter = @ptrCast(@alignCast(ptr));
        self.dir.makePath(path) catch return error.DirectoryCreateFailed;
    }
};

/// Buffer-based extract writer for in-memory extraction
pub const BufferExtractWriter = struct {
    allocator: std.mem.Allocator,
    files: std.StringHashMap(std.ArrayList(u8)),
    directories: std.ArrayList([]u8),
    current_file: ?*std.ArrayList(u8) = null,

    pub fn init(allocator: std.mem.Allocator) BufferExtractWriter {
        return .{
            .allocator = allocator,
            .files = std.StringHashMap(std.ArrayList(u8)).init(allocator),
            .directories = std.ArrayList([]u8).init(allocator),
        };
    }

    pub fn deinit(self: *BufferExtractWriter) void {
        var iter = self.files.iterator();
        while (iter.next()) |entry| {
            self.allocator.free(entry.key_ptr.*);
            entry.value_ptr.deinit();
        }
        self.files.deinit();

        for (self.directories.items) |dir| {
            self.allocator.free(dir);
        }
        self.directories.deinit();
    }

    pub fn extractWriter(self: *BufferExtractWriter) ExtractWriter {
        return ExtractWriter{
            .ptr = self,
            .vtable = &vtable,
        };
    }

    const vtable = ExtractWriter.VTable{
        .createFile = createFile,
        .finishFile = finishFile,
        .makeDir = makeDir,
    };

    fn createFile(ptr: *anyopaque, path: []const u8) ExtractWriter.CreateFileError!std.io.AnyWriter {
        const self: *BufferExtractWriter = @ptrCast(@alignCast(ptr));

        const key = self.allocator.dupe(u8, path) catch return error.OutOfMemory;
        errdefer self.allocator.free(key);

        const result = self.files.getOrPut(key) catch return error.OutOfMemory;
        if (result.found_existing) {
            self.allocator.free(key);
            result.value_ptr.clearRetainingCapacity();
        } else {
            result.value_ptr.* = std.ArrayList(u8).init(self.allocator);
        }

        self.current_file = result.value_ptr;
        // Create a proper AnyWriter that directly references the ArrayList
        return .{
            .context = @ptrCast(result.value_ptr),
            .writeFn = arrayListWrite,
        };
    }

    fn arrayListWrite(context: *const anyopaque, bytes: []const u8) anyerror!usize {
        const list: *std.ArrayList(u8) = @ptrCast(@alignCast(@constCast(context)));
        try list.appendSlice(bytes);
        return bytes.len;
    }

    fn finishFile(ptr: *anyopaque, _: std.io.AnyWriter) ExtractWriter.FinishFileError!void {
        const self: *BufferExtractWriter = @ptrCast(@alignCast(ptr));
        self.current_file = null;
    }

    fn makeDir(ptr: *anyopaque, path: []const u8) ExtractWriter.MakeDirError!void {
        const self: *BufferExtractWriter = @ptrCast(@alignCast(ptr));
        const dir_copy = self.allocator.dupe(u8, path) catch return error.DirectoryCreateFailed;
        self.directories.append(dir_copy) catch {
            self.allocator.free(dir_copy);
            return error.DirectoryCreateFailed;
        };
    }
};

/// Result of path validation when an error is found
pub const PathValidationError = struct {
    path: []const u8,
    reason: PathValidationReason,
};

// Windows reserved device names (case-insensitive)
const WINDOWS_RESERVED_NAMES = [_][]const u8{
    "CON",  "PRN",  "AUX",  "NUL",
    "COM1", "COM2", "COM3", "COM4",
    "COM5", "COM6", "COM7", "COM8",
    "COM9", "LPT1", "LPT2", "LPT3",
    "LPT4", "LPT5", "LPT6", "LPT7",
    "LPT8", "LPT9",
};

/// Check if a path has security or compatibility issues for unbundling
pub fn pathHasUnbundleErr(path: []const u8) ?PathValidationError {
    // Check for empty path
    if (path.len == 0) {
        return PathValidationError{
            .path = path,
            .reason = .empty_path,
        };
    }

    // Check for path too long (255 is a common limit)
    if (path.len > 255) {
        return PathValidationError{
            .path = path,
            .reason = .path_too_long,
        };
    }

    // Check for absolute path
    if (path[0] == '/' or path[0] == '\\') {
        return PathValidationError{
            .path = path,
            .reason = .absolute_path,
        };
    }

    // Check for Windows absolute path (C:, D:, etc.)
    if (path.len >= 2 and path[1] == ':') {
        return PathValidationError{
            .path = path,
            .reason = .absolute_path,
        };
    }

    // Check for reserved characters and validate components
    var iter = std.mem.tokenizeScalar(u8, path, '/');
    while (iter.next()) |component| {
        // Check for path traversal (..)
        if (std.mem.eql(u8, component, "..")) {
            return PathValidationError{
                .path = path,
                .reason = .path_traversal,
            };
        }

        // Check for current directory reference (.)
        if (std.mem.eql(u8, component, ".")) {
            return PathValidationError{
                .path = path,
                .reason = .current_directory_reference,
            };
        }

        // Check for Windows reserved names
        const upper_component = std.ascii.allocUpperString(std.heap.page_allocator, component) catch component;
        defer if (upper_component.ptr != component.ptr) std.heap.page_allocator.free(upper_component);

        // Check base name (without extension) for reserved names
        const base_name = if (std.mem.indexOfScalar(u8, upper_component, '.')) |dot_pos|
            upper_component[0..dot_pos]
        else
            upper_component;

        for (WINDOWS_RESERVED_NAMES) |reserved| {
            if (std.mem.eql(u8, base_name, reserved)) {
                return PathValidationError{
                    .path = path,
                    .reason = .windows_reserved_name,
                };
            }
        }

        // Check for components ending with space or period
        if (component.len > 0) {
            if (component[component.len - 1] == ' ') {
                return PathValidationError{
                    .path = path,
                    .reason = .component_ends_with_space,
                };
            }
            if (component[component.len - 1] == '.') {
                return PathValidationError{
                    .path = path,
                    .reason = .component_ends_with_period,
                };
            }
        }
    }

    // Check for Windows reserved characters in the entire path
    for (path) |char| {
        switch (char) {
            0 => return PathValidationError{
                .path = path,
                .reason = .{ .windows_reserved_char = char },
            },
            '<', '>', ':', '"', '|', '?', '*' => return PathValidationError{
                .path = path,
                .reason = .{ .windows_reserved_char = char },
            },
            '\\' => {
                // Backslash is only allowed on Windows as a path separator
                if (builtin.os.tag != .windows) {
                    return PathValidationError{
                        .path = path,
                        .reason = .contained_backslash_on_unix,
                    };
                }
            },
            else => {},
        }
    }

    return null;
}

/// Generic hashing reader that works with any reader type
fn HashingReader(comptime ReaderType: type) type {
    return struct {
        child_reader: ReaderType,
        hasher: *std.crypto.hash.Blake3,

        const Self = @This();
        pub const Error = ReaderType.Error;
        pub const Reader = std.io.Reader(*Self, Error, read);

        pub fn read(self: *Self, buffer: []u8) Error!usize {
            const n = try self.child_reader.read(buffer);
            if (n > 0) {
                self.hasher.update(buffer[0..n]);
            }
            return n;
        }

        pub fn reader(self: *Self) Reader {
            return .{ .context = self };
        }
    };
}

/// Unbundle a compressed tar archive, streaming from input_reader to extract_writer.
///
/// This is the core streaming unbundle logic that can be used by both file-based
/// unbundling and network-based downloading.
/// If an InvalidPath error is returned, error_context will contain details about the invalid path.
pub fn unbundleStream(
    input_reader: anytype,
    extract_writer: ExtractWriter,
    expected_hash: *const [32]u8,
    error_context: ?*ErrorContext,
) UnbundleError!void {
    // Create a hashing reader to verify the hash while reading
    var hasher = std.crypto.hash.Blake3.init(.{});

    // We need to create the specific type for this reader
    const ReaderType = @TypeOf(input_reader);
    const HashingReaderType = HashingReader(ReaderType);

    var hashing_reader = HashingReaderType{
        .child_reader = input_reader,
        .hasher = &hasher,
    };

    // Create zstandard decompressor
    var window_buffer: [ZSTD_WINDOW_BUFFER_SIZE]u8 = undefined;
    var zstd_stream = std.compress.zstd.decompressor(hashing_reader.reader(), .{
        .window_buffer = &window_buffer,
    });
    const decompressed_reader = zstd_stream.reader();

    // Create tar reader with buffers for file and link names
    var file_name_buffer: [std.fs.max_path_bytes]u8 = undefined;
    var link_name_buffer: [std.fs.max_path_bytes]u8 = undefined;
    var tar_iterator = std.tar.iterator(decompressed_reader, .{
        .file_name_buffer = &file_name_buffer,
        .link_name_buffer = &link_name_buffer,
    });

    var data_extracted = false;

    // Process all tar entries
    while (true) {
        const maybe_entry = tar_iterator.next() catch |err| switch (err) {
            error.EndOfStream => break,
            else => return error.InvalidTarHeader,
        };

        const entry = maybe_entry orelse break;
        const file_path = entry.name;

        // Validate path for security
        if (pathHasUnbundleErr(file_path)) |validation_error| {
            if (error_context) |ctx| {
                ctx.path = validation_error.path;
                ctx.reason = validation_error.reason;
            }
            return error.InvalidPath;
        }

        switch (entry.kind) {
            .directory => {
                // Create directory
                try extract_writer.makeDir(file_path);
                data_extracted = true;
            },
            .file => {
                // Create file and stream content
                const file_writer = try extract_writer.createFile(file_path);
                defer extract_writer.finishFile(file_writer) catch {};

                // Stream file content in chunks
                var buffer: [STREAM_BUFFER_SIZE]u8 = undefined;
                var bytes_remaining = entry.size;
                while (bytes_remaining > 0) {
                    const to_read = @min(buffer.len, bytes_remaining);
                    const bytes_read = try entry.reader().readAll(buffer[0..to_read]);
                    if (bytes_read == 0) return error.UnexpectedEndOfStream;
                    file_writer.writeAll(buffer[0..bytes_read]) catch return error.FileWriteFailed;
                    bytes_remaining -= bytes_read;
                }

                data_extracted = true;
            },
            .sym_link => {
                // Validate symlink target for security
                const link_target = entry.link_name;

                // Check if it's a relative path (no leading /)
                if (link_target.len > 0 and link_target[0] == '/') {
                    if (error_context) |ctx| {
                        ctx.path = file_path;
                        ctx.reason = .absolute_path;
                    }
                    return error.InvalidPath;
                }

                // Check for path traversal components (.. or .)
                var iter = std.mem.tokenizeScalar(u8, link_target, '/');
                while (iter.next()) |component| {
                    if (std.mem.eql(u8, component, "..")) {
                        if (error_context) |ctx| {
                            ctx.path = file_path;
                            ctx.reason = .path_traversal;
                        }
                        return error.InvalidPath;
                    }
                    if (std.mem.eql(u8, component, ".")) {
                        if (error_context) |ctx| {
                            ctx.path = file_path;
                            ctx.reason = .current_directory_reference;
                        }
                        return error.InvalidPath;
                    }
                }

                // Symlink is safe (relative, no .. or . components)
                // For DirExtractWriter, create the symlink
                // For BufferExtractWriter, we'll just skip it
                // TODO: Add symlink support to ExtractWriter interface

                // For now, consume the bytes even though we don't use them
                var buffer: [STREAM_BUFFER_SIZE]u8 = undefined;
                var bytes_remaining = entry.size;
                while (bytes_remaining > 0) {
                    const to_read = @min(buffer.len, bytes_remaining);
                    const bytes_read = try entry.reader().readAll(buffer[0..to_read]);
                    bytes_remaining -= bytes_read;
                }

                data_extracted = true;
            },
        }
    }

    // Verify hash
    var actual_hash: [32]u8 = undefined;
    hasher.final(&actual_hash);

    if (!std.mem.eql(u8, &actual_hash, expected_hash)) {
        return error.HashMismatch;
    }

    // Ensure we extracted something
    if (!data_extracted) {
        return error.NoDataExtracted;
    }
}

/// Validate a base58-encoded hash string and decode it.
///
/// Returns the decoded hash if valid, or null if invalid.
pub fn validateBase58Hash(base58_str: []const u8) !?[32]u8 {
    // A 32-byte hash encoded in base58 should be at least 43 characters
    // (all zeros would be 32 '1' characters, any non-zero value needs more)
    // Maximum is 44 characters for a full 256-bit value
    if (base58_str.len < 32 or base58_str.len > 44) {
        return null;
    }

    var hash: [32]u8 = undefined;
    base58.decode(base58_str, &hash) catch {
        return null;
    };

    return hash;
}

/// Unbundle files from a compressed tar archive to a directory.
///
/// The filename parameter should be the base58-encoded blake3 hash + .tar.zst extension.
/// If an InvalidPath error is returned, error_context will contain details about the invalid path.
pub fn unbundle(
    allocator: std.mem.Allocator,
    input_reader: anytype,
    extract_dir: std.fs.Dir,
    filename: []const u8,
    error_context: ?*ErrorContext,
) UnbundleError!void {
    // Extract expected hash from filename
    if (!std.mem.endsWith(u8, filename, TAR_EXTENSION)) {
        return error.InvalidFilename;
    }
    const base58_hash = filename[0 .. filename.len - TAR_EXTENSION.len]; // Remove .tar.zst
    const expected_hash = (try validateBase58Hash(base58_hash)) orelse {
        return error.InvalidFilename;
    };

    var dir_writer = DirExtractWriter.init(extract_dir, allocator);
    defer dir_writer.deinit();
    return unbundleStream(input_reader, dir_writer.extractWriter(), &expected_hash, error_context);
}
