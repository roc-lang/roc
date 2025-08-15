//! Unbundle compressed tar archives using Zig's standard library
//!
//! This module provides unbundling functionality that works on all platforms
//! including WebAssembly, by using Zig's std.compress.zstandard instead of
//! the C zstd library.

const builtin = @import("builtin");
const std = @import("std");
const base58 = @import("base58");

// Constants
const TAR_EXTENSION = ".tar.zst";
const STREAM_BUFFER_SIZE: usize = 64 * 1024; // 64KB buffer for streaming operations

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

/// Error type for path validation failures
pub const PathValidationError = struct {
    path: []const u8,
    reason: PathValidationReason,
};

/// Writer interface for extracting files during unbundle
pub const ExtractWriter = struct {
    ptr: *anyopaque,
    vtable: *const VTable,

    pub const VTable = struct {
        createFile: *const fn (ptr: *anyopaque, path: []const u8) CreateFileError!std.io.AnyWriter,
        finishFile: *const fn (ptr: *anyopaque, writer: std.io.AnyWriter) FinishFileError!void,
        makeDir: *const fn (ptr: *anyopaque, path: []const u8) MakeDirError!void,
    };

    pub const CreateFileError = error{ FileCreateFailed, InvalidPath, OutOfMemory };
    pub const FinishFileError = error{FileWriteFailed};
    pub const MakeDirError = error{ DirectoryCreateFailed, InvalidPath, OutOfMemory };

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

/// Directory-based extract writer
pub const DirExtractWriter = struct {
    dir: std.fs.Dir,

    pub fn init(dir: std.fs.Dir) DirExtractWriter {
        return .{ .dir = dir };
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
        return file.writer().any();
    }

    fn finishFile(_: *anyopaque, writer: std.io.AnyWriter) ExtractWriter.FinishFileError!void {
        // For file writers, we need to close the file
        // In Zig 0.14, we need to properly cast the context
        const file_writer = writer.context;
        const file = @as(*std.fs.File, @ptrCast(@alignCast(@constCast(file_writer))));
        file.close();
    }

    fn makeDir(ptr: *anyopaque, path: []const u8) ExtractWriter.MakeDirError!void {
        const self: *DirExtractWriter = @ptrCast(@alignCast(ptr));
        self.dir.makePath(path) catch return error.DirectoryCreateFailed;
    }
};

/// Buffer-based extract writer for in-memory extraction
pub const BufferExtractWriter = struct {
    allocator: *std.mem.Allocator,
    files: std.StringHashMap(std.ArrayList(u8)),
    directories: std.ArrayList([]u8),
    current_file: ?*std.ArrayList(u8) = null,

    pub fn init(allocator: *std.mem.Allocator) BufferExtractWriter {
        return .{
            .allocator = allocator,
            .files = std.StringHashMap(std.ArrayList(u8)).init(allocator.*),
            .directories = std.ArrayList([]u8).init(allocator.*),
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
            result.value_ptr.* = std.ArrayList(u8).init(self.allocator.*);
        }

        self.current_file = result.value_ptr;
        return result.value_ptr.writer().any();
    }

    fn finishFile(ptr: *anyopaque, _: std.io.AnyWriter) ExtractWriter.FinishFileError!void {
        const self: *BufferExtractWriter = @ptrCast(@alignCast(ptr));
        self.current_file = null;
    }

    fn makeDir(ptr: *anyopaque, path: []const u8) ExtractWriter.MakeDirError!void {
        const self: *BufferExtractWriter = @ptrCast(@alignCast(ptr));
        const dir_path = self.allocator.dupe(u8, path) catch return error.OutOfMemory;
        self.directories.append(dir_path) catch {
            self.allocator.free(dir_path);
            return error.OutOfMemory;
        };
    }
};

/// Validate a base58-encoded hash string and return the decoded hash.
/// Returns null if the hash is invalid.
pub fn validateBase58Hash(base58_hash: []const u8) !?[32]u8 {
    if (base58_hash.len > base58.base58_hash_bytes) {
        return null;
    }

    var hash: [32]u8 = undefined;
    base58.decode(base58_hash, &hash) catch return null;
    return hash;
}

/// Check if a path has any unbundling errors (security and compatibility issues)
pub fn pathHasUnbundleErr(path: []const u8) ?PathValidationError {
    if (path.len == 0) {
        return PathValidationError{
            .path = path,
            .reason = .empty_path,
        };
    }

    // Check for absolute paths
    if (path[0] == '/' or (builtin.target.os.tag == .windows and path.len >= 2 and path[1] == ':')) {
        return PathValidationError{
            .path = path,
            .reason = .absolute_path,
        };
    }

    // Check for path traversal attempts
    if (std.mem.indexOf(u8, path, "..") != null) {
        return PathValidationError{
            .path = path,
            .reason = .path_traversal,
        };
    }

    // Check for current directory references
    if (std.mem.eql(u8, path, ".") or std.mem.indexOf(u8, path, "./") != null or std.mem.indexOf(u8, path, "/.") != null) {
        return PathValidationError{
            .path = path,
            .reason = .current_directory_reference,
        };
    }

    return null;
}

/// Unbundle files from a compressed tar archive stream.
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
    const HashingReader = struct {
        child_reader: @TypeOf(input_reader),
        hasher: *std.crypto.hash.Blake3,

        pub const Error = @TypeOf(input_reader).Error;
        pub const Reader = std.io.Reader(@This(), Error, read);

        pub fn read(self: @This(), buffer: []u8) Error!usize {
            const n = try self.child_reader.read(buffer);
            if (n > 0) {
                self.hasher.update(buffer[0..n]);
            }
            return n;
        }

        pub fn reader(self: @This()) Reader {
            return .{ .context = self };
        }
    };

    var hashing_reader = HashingReader{
        .child_reader = input_reader,
        .hasher = &hasher,
    };

    // Create zstandard decompressor
    var zstd_stream = std.compress.zstd.decompressor(hashing_reader.reader(), .{});
    const decompressed_reader = zstd_stream.reader();

    // Create tar reader
    var tar_iterator = std.tar.iterator(decompressed_reader, .{
        .max_file_size = std.math.maxInt(usize), // No limit on file size
    });

    var data_extracted = false;

    // Process all tar entries
    while (try tar_iterator.next()) |entry| {
        const file_path = entry.path;

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
                try extract_writer.makeDir(file_path);
                data_extracted = true;
            },
            .file => {
                const file_writer = try extract_writer.createFile(file_path);
                defer extract_writer.finishFile(file_writer) catch {};

                // Stream the file content
                const file_size = std.math.cast(usize, entry.size) orelse return error.FileTooLarge;
                var bytes_remaining = file_size;
                var buffer: [STREAM_BUFFER_SIZE]u8 = undefined;

                while (bytes_remaining > 0) {
                    const to_read = @min(buffer.len, bytes_remaining);
                    const bytes_read = try entry.reader().readAll(buffer[0..to_read]);
                    if (bytes_read == 0) return error.UnexpectedEndOfStream;
                    try file_writer.writeAll(buffer[0..bytes_read]);
                    bytes_remaining -= bytes_read;
                }

                data_extracted = true;
            },
            else => {
                // Skip other entry types (symlinks, etc.)
                try entry.skip();
            },
        }
    }

    if (!data_extracted) {
        return error.NoDataExtracted;
    }

    // Verify the hash
    var actual_hash: [32]u8 = undefined;
    hasher.final(&actual_hash);
    if (!std.mem.eql(u8, &actual_hash, expected_hash)) {
        return error.HashMismatch;
    }
}

/// Unbundle files from a compressed tar archive to a directory.
///
/// The filename parameter should be the base58-encoded blake3 hash + .tar.zst extension.
/// If an InvalidPath error is returned, error_context will contain details about the invalid path.
pub fn unbundle(
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

    var dir_writer = DirExtractWriter.init(extract_dir);
    return unbundleStream(input_reader, dir_writer.extractWriter(), &expected_hash, error_context);
}
